#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/backend/preprocessor.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/backend/allscale_runtime_entities.h"

namespace allscale {
namespace compiler {
namespace backend {

	namespace core = insieme::core;
	namespace be = insieme::backend;

	using std::string;
	using std::map;


	namespace {

		core::ProgramPtr wrapStmt(const core::StatementPtr& expr);

		core::ProgramPtr replaceMain(const core::ProgramPtr& prog, const be::Converter& converter);

	}

	core::NodePtr EntryPointWrapper::process(const be::Converter& converter, const core::NodePtr& code) {

		// if the given node is a stmt ..
		if (auto stmt = code.isa<core::StatementPtr>()) {
			// .. wrap the expression into a main function and convert the result
			return process(converter, wrapStmt(stmt));
		}

		// the rest only concerns programs
		auto program = code.isa<core::ProgramPtr>();
		if (!program) return code;

		// replace the main function
		return replaceMain(program, converter);
	}


	namespace {

		core::ProgramPtr wrapStmt(const core::StatementPtr& stmt) {
			core::IRBuilder builder(stmt->getNodeManager());

			// if it is a lambda, make it the entry point
			if (auto lambda = stmt.isa<core::LambdaExprPtr>()) {
				return builder.program(core::ExpressionList({lambda}));
			}

			// create a symbol
			map<string,core::NodePtr> symbols;
			symbols["code"] = stmt;

			// simply wrap the given expression into an otherwise empty program
			return builder.parseProgram(
					"int<4> main() {"
					"	code;"
					"	return 0;"
					"}",
					symbols
			);
		}

		core::ProgramPtr replaceMain(const core::ProgramPtr& prog, const be::Converter& converter) {
			core::NodeManager& mgr = prog.getNodeManager();
			core::IRBuilder builder(mgr);

			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// TODO: wrap startup / shutdown + task wrapper

			// get the main entry point
			assert_eq(1, prog->getEntryPoints().size());
			core::LambdaExprPtr main = prog->getEntryPoints().front().as<core::LambdaExprPtr>();

			// convert main to accept a tuple of parameters
			auto closureType = builder.tupleType(main->getFunctionType()->getParameterTypeList());
			auto paramType = core::lang::buildRefType(closureType,true,false,core::lang::ReferenceType::Kind::CppReference);
			auto resType = lang::TreetureType(main->getFunctionType()->getReturnType(),false).toIRType();

			auto newFunType = builder.functionType(paramType,resType);

			auto param = builder.variable(paramType);

			core::ExpressionList args;
			for(unsigned i=0; i<main->getFunctionType()->getParameterTypes().size(); ++i) {
				args.push_back(builder.refComponent(param,i));
			}

			// create body wrapping up computation and treeture conversion
			auto body = builder.compoundStmt(
					builder.returnStmt(
							builder.callExpr(
									ext.getTreetureDone(),
									builder.callExpr(main, args)
							)
					)
			);

			// replace main by proper wrapper
			core::LambdaExprPtr oldMain = main;
			main = builder.lambdaExpr(newFunType, { param }, body);

			// check that what has been build is properly composed
			assert_true(core::checks::check(main).empty())
				<< core::checks::check(main);

			// convert this main into a work item
			WorkItemVariant impl(main);
			WorkItemDescription desc("main", impl);


			// create a new main entry point
			core::ExpressionList entryPointArgs;
			entryPointArgs.push_back(desc.toIR(mgr));
			for(const auto& cur : oldMain->getParameterList()->getParameters()) entryPointArgs.push_back(cur);

			auto& asbm = mgr.getLangExtension<AllScaleBackendModule>();
			auto newMainBody = builder.compoundStmt(
					builder.returnStmt(
							builder.callExpr(
									asbm.getProcessMain(),
									entryPointArgs
							)
					)
			);

			// create final main
			main = core::transform::replaceNode(mgr, core::LambdaExprAddress(oldMain)->getBody(), newMainBody).as<core::LambdaExprPtr>();

			// check that what has been build is properly composed
			assert_true(core::checks::check(main).empty())
				<< core::checks::check(main);

			// wrap up into a program, and be done
			return builder.program(core::ExpressionList{ main });
		}

	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
