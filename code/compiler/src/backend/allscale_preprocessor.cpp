#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/name_manager.h"

#include "allscale/compiler/allscale_utils.h"
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

			// simply wrap the given expression into an otherwise empty program
			auto wrapper = builder.parseProgram(
					"int<4> main() {"
					"	return 0;"
					"}"
			);

			// insert the statement as the first statement of the body
			return core::transform::insert(
					stmt->getNodeManager(),
					core::ProgramAddress(wrapper)->getEntryPoints()[0].as<core::LambdaExprAddress>()->getBody(),
					stmt,
					0
			).as<core::ProgramPtr>();
		}

		core::ProgramPtr replaceMain(const core::ProgramPtr& prog, const be::Converter& converter) {
			core::NodeManager& mgr = prog.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& basic = mgr.getLangBasic();
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// check that what has been build is properly composed
			assert_correct_ir(prog)
				<< "Invalid input program for EntryPointWrapper\n";

			// get the main entry point
			assert_eq(1, prog->getEntryPoints().size());
			core::LambdaExprPtr main = prog->getEntryPoints().front().as<core::LambdaExprPtr>();

			// if the original main function didn't accept parameters, we create a new one which does, as the parameters need to be passed on to the runtime
			if(main->getParameterList().size() == 0) {
				auto firstParamType = builder.getLangBasic().getInt4();
				auto firstParamVar = builder.variable(builder.refType(firstParamType));
				auto secondParamType = core::lang::buildPtrType(core::lang::buildPtrType(builder.getLangBasic().getChar()));
				auto secondParamVar = builder.variable(builder.refType(secondParamType));
				auto newMainFunType = builder.functionType(toVector<core::TypePtr>(firstParamType, secondParamType), builder.getLangBasic().getInt4());
				auto newMain = builder.lambdaExpr(newMainFunType, toVector(firstParamVar, secondParamVar), main.getBody());
				core::transform::utils::migrateAnnotations(main, newMain);
				main = newMain;
			}

			// convert main to accept a tuple of parameters
			auto closureType = builder.tupleType(main->getFunctionType()->getParameterTypeList());
			auto paramType = core::lang::buildRefType(closureType,true,false,core::lang::ReferenceType::Kind::CppReference);
			auto resType = lang::TreetureType(main->getFunctionType()->getReturnType(),false).toIRType();

			auto newFunType = builder.functionType(paramType,resType);

			auto param = builder.variable(paramType);

			core::ExpressionList args;
			for(unsigned i=0; i<main->getFunctionType()->getParameterTypes().size(); ++i) {
				args.push_back(builder.deref(builder.refComponent(param,i)));
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
				<< dumpPretty(main) << "\n"
				<< insieme::core::printer::dumpErrors(core::checks::check(main));

			// create a no-split test
			auto no_split = builder.lambdaExpr(basic.getBool(),{ param }, builder.returnStmt(basic.getFalse()));

			// convert this main into a work item
			WorkItemVariant impl(main);
			WorkItemDescription desc("main", no_split, impl);


			// create a new main entry point
			core::ExpressionList entryPointArgs;
			entryPointArgs.push_back(desc.toIR(mgr));
			for(const auto& cur : oldMain->getParameterList()->getParameters()) {
				entryPointArgs.push_back(builder.deref(cur));
			}

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
				<< dumpColor(main) << "\n"
				<< core::checks::check(main);

			// wrap up into a program, and be done
			return builder.program(core::ExpressionList{ main });
		}

	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
