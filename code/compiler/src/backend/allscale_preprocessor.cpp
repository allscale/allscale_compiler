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


	insieme::core::NodePtr ClosureConstructorEnforcer::process(const insieme::backend::Converter&, const insieme::core::NodePtr& code) {

		core::NodeManager& mgr = code.getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

		// -- Step 8: extra step: make all (nested) structs in the closure default-constructible

		// search all structs
		core::NodeMap recordSubstitutes;
		core::visitDepthFirstOnce(code,[&](const core::CallExprPtr& call){

			// only interested in call to PrecFunCreate operator
			if (!core::analysis::isCallOf(call,ext.getPrecFunCreate())) return;

			// collect all structs in the closure type
			core::visitDepthFirstOnce(call->getArgument(0)->getType(),[&](const core::RecordPtr& record){

				// Goal: add a all-field forward constructor to support initializer expressions

				// if there are no fields, the default constructor is sufficient
				if (record->getFields().empty()) return;

				auto newRecord = record;

				// search default constructor
				core::LambdaExprAddress defaultConstructor;
				for(const core::ExpressionAddress& cur : core::RecordAddress(record)->getConstructors()) {
					if (auto lambda = cur.isa<core::LambdaExprAddress>()) {
						if (lambda->getParameterList().size() == 1) {
							defaultConstructor = lambda;
						}
					}
				}

				core::VariablePtr thisVar = defaultConstructor->getParameterList()[0];
				core::VariableList parameters;
				core::StatementList initStmts;

				// the this pointer is the first parameter
				parameters.push_back(thisVar);

				auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
				for(const core::FieldPtr& field : record->getFields()) {

					// get field type
					auto fieldType = field->getType();

					// get parameter
					auto parameter = builder.variable(core::transform::materialize(fieldType));

					// create the field access
					auto fieldAccess = builder.callExpr(
							refExt.getRefMemberAccess(),
							builder.deref(thisVar),
							builder.getIdentifierLiteral(field->getName()),
							builder.getTypeLiteral(field->getType())
					);

					// create the init expression
					auto arg = core::lang::isReference(fieldType) ? parameter : builder.deref(parameter);
					auto initExpr = builder.initExpr(fieldAccess,arg);

					// TODO: actually call constructor of copied elements if necessary
					//  - so fare everything is initialized through an init expression

					// add to lists
					parameters.push_back(parameter);
					initStmts.push_back(initExpr);

				}

				// build the new constructor
				auto returnType = defaultConstructor.getAddressedNode()->getFunctionType()->getReturnType();
				auto ctor = builder.lambdaExpr(returnType,parameters,builder.compoundStmt(initStmts),"_",core::FK_CONSTRUCTOR);

				// insert into resulting record
				core::ExpressionList constructors = newRecord->getConstructors()->getExpressions();
				constructors.push_back(ctor);
				newRecord = core::transform::replaceNode(mgr,
						core::RecordAddress(newRecord)->getConstructors(),
						builder.expressions(constructors)
				).as<core::RecordPtr>();

				// -- Done: register for replacement

				// add replacement to substitution
				recordSubstitutes[record] = newRecord;
			});

			// --- Parameter Type ---

			// also: if the recursive parameter type is a struct, make sure it has a default constructor
			auto funParamType = call->getArgument(1)->getType().as<core::FunctionTypePtr>()->getParameterType(1);

			// remove references
			if (core::lang::isReference(funParamType)) {
				funParamType = core::analysis::getReferencedType(funParamType);
			}

			// take the first parameter in the passed tuple
			funParamType = funParamType.as<core::TupleTypePtr>()->getElement(0);

			// if this is a tag type, it needs a default constructor
			if (auto tagType = funParamType.isa<core::TagTypePtr>()) {
				auto record = tagType->getRecord();

				// search default constructor
				core::ExpressionAddress defaultConstructor;
				for(const core::ExpressionAddress& cur : core::RecordAddress(record)->getConstructors()) {

					// check whether this is the default constructor
					if (cur->getType().as<core::FunctionTypePtr>()->getParameterTypes()->size() == 1) {

						// if it is a lambda, we are done
						if (cur.isa<core::LambdaExprPtr>()) return;

						// this is the one we need to replace
						defaultConstructor = cur;
					}
				}

				// make sure it has been found
				assert_true(defaultConstructor)
					<< "Unable to locate pre-existing default constructor!";

				// build new default constructor
				auto thisType = defaultConstructor.getAddressedNode()->getType().as<core::FunctionTypePtr>()->getParameterType(0);
				auto newDefaultCtor = builder.getDefaultConstructor(thisType,core::ParentsPtr(),record->getFields());

				// replace default constructor
				auto newRecord = core::transform::replaceNode(mgr,defaultConstructor,newDefaultCtor).as<core::RecordPtr>();

				// add to replacements
				recordSubstitutes[record] = newRecord;
			}

		},true);

		// apply replacements on substitutes
		for(auto& cur : recordSubstitutes) {
			cur.second = core::transform::replaceAllGen(mgr,cur.second,recordSubstitutes,core::transform::globalReplacement);
		}

		// apply replacement
		auto res = core::transform::replaceAllGen(mgr,code,recordSubstitutes,core::transform::globalReplacement);

		// check the result
		assert_correct_ir(res);

		// return result
		return res;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
