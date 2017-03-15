#include "allscale/compiler/backend/allscale_operator.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/backend/allscale_code_fragments.h"

namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;
	using namespace insieme::backend;

	OperatorConverterTable& addRuntimeSpecificOps(NodeManager& manager, OperatorConverterTable& table) {

		#include "insieme/backend/operator_converter_begin.inc"

		{
			const AllScaleBackendModule& ext = manager.getLangExtension<AllScaleBackendModule>();

			table[ext.getProcessMain()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// resolve the work item
				auto& info = WorkItemDescriptions::getInstance(CONVERTER).getDescriptionType(context, ARG(0));

				// add dependency to definition
				context.addDependency(info.definition);

				// extract defining type
				auto workItemDescType = info.description_type;

				// create the target
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::main_wrapper");
				trg = c_ast::instantiate(trg,workItemDescType);

				// create the arguments
				std::vector<c_ast::NodePtr> args;
				for(unsigned i=1; i<call->getArgumentList().size(); ++i) {
					args.push_back(CONVERT_EXPR(call->getArgument(i)));
				}

				// create the call
				return c_ast::call(trg,args);
			};


			table[ext.getSpawnWorkItem()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// resolve the work item
				auto& info = WorkItemDescriptions::getInstance(CONVERTER).getDescriptionType(context, ARG(1));

				// add dependency to definition
				context.addDependency(info.definition);

				// extract defining type
				auto workItemDescType = info.description_type;

				// create the target
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::spawn");
				trg = c_ast::instantiate(trg,workItemDescType);

				// create the arguments
				std::vector<c_ast::NodePtr> args;
				for(unsigned i=2; i<call->getArgumentList().size(); ++i) {
					args.push_back(CONVERT_EXPR(call->getArgument(i)));
				}

				// create the call
				return c_ast::call(trg,args);
			};

			table[ext.getRecSpawnWorkItem()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// resolve the name
				const auto& lit = call->getArgument(1).as<core::CallExprPtr>()->getArgument(0).as<core::LiteralPtr>();

				// resolve the work item
				auto& info = WorkItemDescriptions::getInstance(CONVERTER).getDescriptionType(context, lit->getStringValue());

				// add dependency to definition
				context.addDependency(info.definition);

				// extract defining type
				auto workItemDescType = info.description_type;

				// create the target
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::spawn");
				trg = c_ast::instantiate(trg,workItemDescType);

				// return just the function, arguments follow
				return trg;
			};

			table[ext.getPrecFunCreate()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// extract arg and result types
				lang::PrecFunType precFunType(call->getType());

				auto& paramTypeInfo = GET_TYPE_INFO(precFunType.getParamType());
				auto& returnTypeInfo = GET_TYPE_INFO(precFunType.getReturnType());
				context.addDependency(paramTypeInfo.definition);
				context.addDependency(returnTypeInfo.definition);

				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::make_prec_operation");
				trg = c_ast::instantiate(trg,paramTypeInfo.rValueType,returnTypeInfo.rValueType);

				// wrap binding into a lambda wrapper
				core::BindExprPtr bind = ARG(0).as<core::BindExprPtr>();
				c_ast::ExpressionPtr bindWrapper = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::make_insieme_lambda_wrapper");
				c_ast::ExpressionPtr closure = c_ast::call(bindWrapper, CONVERT_ARG(0));

				// just forward parameters -- C++ resolution uses member function
				return c_ast::call(trg,closure);

			};

			table[ext.getPrecFunToFun()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// just forward parameters -- C++ resolution uses member function
				return CONVERT_ARG(0);

			};

			table[ext.getPrecFunToDepFun()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// just forward parameters -- C++ resolution uses member function
				return CONVERT_ARG(0);

			};
		}


		{
			const lang::AllscaleModule& ext = manager.getLangExtension<lang::AllscaleModule>();

			table[ext.getTreetureDone()] = OP_CONVERTER {

				// add dependency to result type
				auto& resTypeInfo = GET_TYPE_INFO(call->getType());
				context.addDependency(resTypeInfo.definition);

				// create result value via constructor call
				return c_ast::call(
						resTypeInfo.rValueType,
						CONVERT_ARG(0)
				);
			};

			table[ext.getTreetureRun()] = OP_CONVERTER {

				// add dependency to result type
				auto& resTypeInfo = GET_TYPE_INFO(call->getType());
				context.addDependency(resTypeInfo.definition);

				// just forward treeture
				return CONVERT_ARG(0);
			};

			table[ext.getTreetureGet()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("get_result"), {});
			};

			table[ext.getTreetureCombine()] = OP_CONVERTER {
				core::IRBuilder builder(call->getNodeManager());

				// TODO: integrate support for dependencies!!

				// add dependency to argument types
				context.addDependency(GET_TYPE_INFO(call->getArgument(1)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(2)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(3)->getType()).definition);

				// add dependency to result type
				context.addDependency(GET_TYPE_INFO(call->getType()).definition);

				// create a wrapper for the operator part
				auto paramType0 = lang::TreetureType(ARG(1)).getValueType();
				auto paramType1 = lang::TreetureType(ARG(2)).getValueType();
				auto resType = lang::TreetureType(call->getType()).getValueType();

				auto funType = builder.functionType({ paramType0, paramType1 }, resType );
				auto param0 = builder.variable(builder.refType(paramType0));
				auto param1 = builder.variable(builder.refType(paramType1));

				auto body = builder.compoundStmt(
						builder.returnStmt(
								builder.callExpr(
										ARG(3),
										builder.deref(param0),
										builder.deref(param1)
								)
						)
				);

				auto lambda = builder.lambdaExpr(funType, { param0, param1 }, body);
				auto info = context.getConverter().getFunctionManager().getInfo(context, lambda);
				context.addDependency(info.prototype);
				context.addRequirement(info.definition);

				// create a call to treeture_combine
				return c_ast::call(
						C_NODE_MANAGER->create("allscale::runtime::treeture_combine"),
						CONVERT_ARG(1),
						CONVERT_ARG(2),
						c_ast::ref(info.function->name)
				);
			};

			table[ext.getTreetureFromRef()] = OP_CONVERTER {
				return CONVERT_ARG(0);
			};
		}


		// intersect tuple component access code
		{
			auto& basic = manager.getLangBasic();

			table[basic.getTupleMemberAccess()] = OP_CONVERTER {

				// signature of operation:
				//		('a, uint<8>, type<'b>) -> 'b

				// add a dependency to the accessed type definition before accessing the type
				const core::TypePtr tupleType = ARG(0)->getType();
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
				context.getDependencies().insert(info.definition);

				// create member access
				core::ExpressionPtr index = ARG(1);
				while(index->getNodeType() == core::NT_CastExpr) {
					index = static_pointer_cast<const core::CastExpr>(index)->getSubExpression();
				}
				assert_eq(index->getNodeType(), core::NT_Literal);
				auto field = C_NODE_MANAGER->create<c_ast::NamedType>(
						C_NODE_MANAGER->create(index.as<core::LiteralPtr>()->getStringValue())
				);

				// access the component of the tuple
				return c_ast::call(
						c_ast::instantiate(C_NODE_MANAGER->create("hpx::util::get"),field),
						CONVERT_ARG(0)
				);
			};

			auto& refExt = manager.getLangExtension<core::lang::ReferenceExtension>();

			table[refExt.getRefComponentAccess()] = OP_CONVERTER {

				// signature of operation:
				//		(ref<'a>, uint<8>, type<'b>) -> ref<'b>

				// add a dependency to the accessed type definition before accessing the type
				const core::TypePtr tupleType = core::analysis::getReferencedType(ARG(0)->getType());
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
				context.getDependencies().insert(info.definition);

				core::ExpressionPtr index = ARG(1);
				while(auto cast = index.isa<core::CastExprPtr>()) {
					index = cast->getSubExpression();
				}
				assert_eq(index->getNodeType(), core::NT_Literal);
				auto field = C_NODE_MANAGER->create<c_ast::NamedType>(
						C_NODE_MANAGER->create(index.as<core::LiteralPtr>()->getStringValue())
				);

				// access the component of the tuple
				auto access = c_ast::call(
						c_ast::instantiate(C_NODE_MANAGER->create("hpx::util::get"),field),
						c_ast::derefIfNotImplicit(CONVERT_ARG(0), ARG(0))
				);

				// check whether a ref operation is required
				if (core::lang::isPlainReference(call)) {
					return c_ast::ref(access);
				}

				// return result
				return access;
			};

		}



		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
