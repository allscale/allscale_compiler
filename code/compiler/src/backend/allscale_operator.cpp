#include "allscale/compiler/backend/allscale_operator.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"

#include "allscale/compiler/core/data_serialization.h"
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


			auto spawnHandler = OP_CONVERTER {

				bool isSpawnFirst = LANG_EXT(AllScaleBackendModule).isCallOfSpawnFirstWorkItem(call);

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// resolve the work item, either by looking it up if it is a reference or by getting it from it's creation description
				WorkItemDescriptionInfo info;
				if(LANG_EXT(AllScaleBackendModule).isCallOfCreateWorkItemDescriptionReference(ARG(1))) {
					const auto& lit = call->getArgument(1).as<CallExprPtr>()->getArgument(0).as<LiteralPtr>();
					info = WorkItemDescriptions::getInstance(CONVERTER).getDescriptionType(context, lit->getStringValue());
				} else {
					info = WorkItemDescriptions::getInstance(CONVERTER).getDescriptionType(context, ARG(1));
				}

				// add dependency to definition
				context.addDependency(info.definition);

				// extract defining type
				auto workItemDescType = info.description_type;

				// create the target
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>(isSpawnFirst ? "allscale::spawn_first_with_dependencies" : "allscale::spawn_with_dependencies");
				trg = c_ast::instantiate(trg,workItemDescType);

				// create the arguments
				std::vector<c_ast::NodePtr> args;
				// add the dependencies as the first argument
				args.push_back(CONVERT_ARG(0));
				// and append all the others
				for(unsigned i=2; i<call->getArgumentList().size(); ++i) {
					args.push_back(CONVERT_ARG(i));
				}

				// create the call
				return c_ast::call(trg,args);
			};

			table[ext.getSpawnWorkItem()] = spawnHandler;
			table[ext.getSpawnFirstWorkItem()] = spawnHandler;

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

				// just forward parameters -- C++ resolution uses member function
				return c_ast::call(trg,CONVERT_ARG(0),CONVERT_ARG(1));

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

			table[ext.getRefRefPlainToRefRefCpp()] = OP_CONVERTER {

				// simply add a de-ref
				return c_ast::deref(CONVERT_ARG(0));

			};

			table[ext.getCreateDataItem()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// convert the data item type
				auto& dataItemInfo = GET_TYPE_INFO(insieme::core::analysis::getRepresentedType(ARG(0)));
				context.addDependency(dataItemInfo.definition);

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::DataItemManager::create");
				trg = c_ast::instantiate(trg,dataItemInfo.rValueType);

				// call the create function
				auto res = c_ast::call(trg);

				// forward all other parameters
				for(std::size_t i=1; i<call->getNumArguments(); ++i) {
					res->arguments.push_back(CONVERT_EXPR(call->getArgument(i)));
				}

				// done
				return res;
			};

			table[ext.getGetDataItem()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::DataItemManager::get");

				// call the get function
				return c_ast::call(trg, CONVERT_ARG(0));
			};

			table[ext.getCreateDataItemRequirement()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::createDataItemRequirement");

				// just forward parameters
				return c_ast::call(trg,CONVERT_ARG(0),CONVERT_ARG(1), CONVERT_ARG(2));

			};

			table[ext.getDataItemRangeSpan()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/api/core/data.h");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::api::core::span");

				// just forward parameters
				return c_ast::call(trg,CONVERT_ARG(0),CONVERT_ARG(1));

			};

			table[ext.getDataItemRangeUnion()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/api/core/data.h");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::api::core::merge");

				// create call
				auto res = c_ast::call(trg);

				// add parameters
				for(const auto& cur : call->getArgumentList()) {
					res->arguments.push_back(CONVERT_EXPR(cur));
				}

				// done
				return res;
			};

			table[ext.getDataItemCheckReadAccess()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::check_read");

				// create call
				auto res = c_ast::call(trg);

				// add parameters
				for(const auto& cur : call->getArgumentList()) {
					res->arguments.push_back(c_ast::deref(CONVERT_EXPR(cur)));
				}

				// done
				return c_ast::ref(res);
			};

			table[ext.getDataItemCheckWriteAccess()] = OP_CONVERTER {

				// add dependencies
				ADD_HEADER("allscale/runtime.hpp");

				// get a literal of the targeted function
				c_ast::ExpressionPtr trg = C_NODE_MANAGER->create<c_ast::Literal>("allscale::runtime::check_write");

				// create call
				auto res = c_ast::call(trg);

				// add parameters
				for(const auto& cur : call->getArgumentList()) {
					res->arguments.push_back(c_ast::deref(CONVERT_EXPR(cur)));
				}

				// done
				return c_ast::ref(res);
			};
		}


		{
			const lang::AllscaleModule& ext = manager.getLangExtension<lang::AllscaleModule>();

			table[ext.getTreetureDone()] = OP_CONVERTER {

				// add dependency to result type
				auto resType = call->getType();
				auto& resTypeInfo = GET_TYPE_INFO(resType);
				context.addDependency(resTypeInfo.definition);

				auto valueType = lang::TreetureType(resType).getValueType();

				// special handling for treeture_done(make_unused_type())
				if(isUnusedType(valueType)) {
					return C_NODE_MANAGER->create<c_ast::Initializer>();
				}

				// special handling of unit treetures
				if (NODE_MANAGER.getLangBasic().isUnit(valueType)) {
					// a void can not be passed => use the comma operator
					return c_ast::comma(
							// trigger the computation
							CONVERT_ARG(0),
							// create resulting void-treeture
							c_ast::call(C_NODE_MANAGER->create<c_ast::Literal>("allscale::make_ready_treeture"))
					);
				}

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

			table[ext.getTreetureIsDone()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("get_future"), {}), C_NODE_MANAGER->create("is_ready"), {});
			};

			table[ext.getTreetureIsValid()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("valid"), {});
			};

			table[ext.getTreetureWait()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("wait"), {});
			};


			table[ext.getTreetureParallel()] = OP_CONVERTER {

				// add dependency to argument types
				context.addDependency(GET_TYPE_INFO(call->getArgument(0)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(1)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(2)->getType()).definition);

				// add dependency to result type
				context.addDependency(GET_TYPE_INFO(call->getType()).definition);

				// create target for implicit std::move call
				context.addInclude("utility");
				auto std_move = C_NODE_MANAGER->create("std::move");

				// create a call to treeture_combine
				return c_ast::call(
						C_NODE_MANAGER->create("allscale::runtime::treeture_parallel"),
						CONVERT_ARG(0),
						c_ast::call(std_move, CONVERT_ARG(1)),
						c_ast::call(std_move, CONVERT_ARG(2))
				);
			};

			table[ext.getTreetureSequential()] = OP_CONVERTER {

				// add dependency to argument types
				context.addDependency(GET_TYPE_INFO(call->getArgument(0)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(1)->getType()).definition);
				context.addDependency(GET_TYPE_INFO(call->getArgument(2)->getType()).definition);

				// add dependency to result type
				context.addDependency(GET_TYPE_INFO(call->getType()).definition);

				// create target for implicit std::move call
				context.addInclude("utility");
				auto std_move = C_NODE_MANAGER->create("std::move");

				// create a call to treeture_combine
				return c_ast::call(
						C_NODE_MANAGER->create("allscale::runtime::treeture_sequential"),
						CONVERT_ARG(0),
						c_ast::call(std_move, CONVERT_ARG(1)),
						c_ast::call(std_move, CONVERT_ARG(2))
				);
			};

			table[ext.getTreetureCombine()] = OP_CONVERTER {
				IRBuilder builder(call->getNodeManager());

				// add dependency to argument types
				context.addDependency(GET_TYPE_INFO(call->getArgument(0)->getType()).definition);
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

				// create target for implicit std::move call
				context.addInclude("utility");
				auto std_move = C_NODE_MANAGER->create("std::move");

				// create a call to treeture_combine
				return c_ast::call(
						C_NODE_MANAGER->create("allscale::runtime::treeture_combine"),
						CONVERT_ARG(0),
						c_ast::call(std_move, CONVERT_ARG(1)),
						c_ast::call(std_move, CONVERT_ARG(2)),
						c_ast::ref(info.function->name)
				);
			};

			table[ext.getTreetureFromRef()] = OP_CONVERTER {
				return CONVERT_ARG(0);
			};

			table[ext.getTreetureToRef()] = OP_CONVERTER {
				return CONVERT_ARG(0);
			};

			table[ext.getTreetureToTaskRef()] = OP_CONVERTER {
				return CONVERT_ARG(0);
			};

			table[ext.getTaskRefDone()] = OP_CONVERTER {
				// create a new treeture<void>
				return c_ast::call(C_NODE_MANAGER->create<c_ast::Literal>("allscale::make_ready_treeture"));
			};

			table[ext.getTaskRefLeft()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("get_left_child"), {});
			};

			table[ext.getTaskRefRight()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("get_right_child"), {});
			};

			table[ext.getTaskRefWait()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("wait"), {});
			};

			table[ext.getTaskRefValid()] = OP_CONVERTER {

				// add dependency to argument type
				auto& resTypeInfo = GET_TYPE_INFO(call->getArgument(0)->getType());
				context.addDependency(resTypeInfo.definition);

				// convert to member call
				return c_ast::memberCall(CONVERT_ARG(0), C_NODE_MANAGER->create("valid"), {});
			};


			table[ext.getDependencyAfter()] = OP_CONVERTER {

				// convert arguments and add dependency to argument types
				std::vector<c_ast::NodePtr> args;
				for(const auto& arg : call->getArgumentList()) {
					context.addDependency(GET_TYPE_INFO(arg->getType()).definition);
					args.push_back(CONVERT_EXPR(arg));
				}

				// add dependency to result type
				context.addDependency(GET_TYPE_INFO(call->getType()).definition);

				// create a call to treeture_combine
				return c_ast::call(
						C_NODE_MANAGER->create("allscale::runtime::after"),
						args
				);
			};
		}


		// intersect tuple component access code
		{
			auto& basic = manager.getLangBasic();

			table[basic.getTupleMemberAccess()] = OP_CONVERTER {

				// signature of operation:
				//		('a, uint<8>, type<'b>) -> 'b

				// add a dependency to the accessed type definition before accessing the type
				const TypePtr tupleType = ARG(0)->getType();
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
				context.getDependencies().insert(info.definition);

				// create member access
				ExpressionPtr index = ARG(1);
				while(index->getNodeType() == NT_CastExpr) {
					index = static_pointer_cast<const CastExpr>(index)->getSubExpression();
				}
				assert_eq(index->getNodeType(), NT_Literal);
				auto field = C_NODE_MANAGER->create<c_ast::NamedType>(
						C_NODE_MANAGER->create(index.as<LiteralPtr>()->getStringValue())
				);

				// access the component of the tuple
				return c_ast::call(
						c_ast::instantiate(C_NODE_MANAGER->create("hpx::util::get"),field),
						CONVERT_ARG(0)
				);
			};

			auto& refExt = manager.getLangExtension<insieme::core::lang::ReferenceExtension>();

			table[refExt.getRefComponentAccess()] = OP_CONVERTER {

				// signature of operation:
				//		(ref<'a>, uint<8>, type<'b>) -> ref<'b>

				// add a dependency to the accessed type definition before accessing the type
				const TypePtr tupleType = insieme::core::analysis::getReferencedType(ARG(0)->getType());
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
				context.getDependencies().insert(info.definition);

				ExpressionPtr index = ARG(1);
				while(auto cast = index.isa<CastExprPtr>()) {
					index = cast->getSubExpression();
				}
				assert_eq(index->getNodeType(), NT_Literal);
				auto field = C_NODE_MANAGER->create<c_ast::NamedType>(
						C_NODE_MANAGER->create(index.as<LiteralPtr>()->getStringValue())
				);

				// access the component of the tuple
				auto access = c_ast::call(
						c_ast::instantiate(C_NODE_MANAGER->create("hpx::util::get"),field),
						c_ast::derefIfNotImplicit(CONVERT_ARG(0), ARG(0))
				);

				// check whether a ref operation is required
				if (insieme::core::lang::isPlainReference(call)) {
					return c_ast::ref(access);
				}

				// return result
				return access;
			};

		}


		// add support for serialization operations
		{
			auto& ext = manager.getLangExtension<core::SerializationModule>();

			table[ext.getRead()] = OP_CONVERTER {

				// signature of operation:
				//		ArchiveReader::(type<'a>)->'a

				// add include file defining this operator
				context.addInclude("allscale/utils/serializer.h");

				// get the type info for type 'a
				auto& info = GET_TYPE_INFO(insieme::core::analysis::getRepresentedType(ARG(1)));

				// add a dependency to the type definition
				context.addDependency(info.definition);

				// create the function to be called
				auto fun = c_ast::instantiate(
						C_NODE_MANAGER->create("read"),
						info.lValueType
				);

				// create the resulting call
				return C_NODE_MANAGER->create<c_ast::MemberCall>(
						fun, CONVERT_ARG(0), std::vector<c_ast::NodePtr>()
				);
			};

			table[ext.getWrite()] = OP_CONVERTER {

				// signature of operation:
				//		ArchiveWriter::(type<'a>,ref<'a,t,f,cpp_ref>)->unit

				// add include file defining this operator
				context.addInclude("allscale/utils/serializer.h");

				// get the type info for type 'a
				auto& info = GET_TYPE_INFO(insieme::core::analysis::getRepresentedType(ARG(1)));

				// add a dependency to the type definition
				context.addDependency(info.definition);

				// create the function to be called
				auto fun = c_ast::instantiate(
						C_NODE_MANAGER->create("write"),
						info.lValueType
				);

				// create the resulting call
				return C_NODE_MANAGER->create<c_ast::MemberCall>(
						fun, CONVERT_ARG(0), std::vector<c_ast::NodePtr>{ CONVERT_ARG(2) }
				);
			};

		}

		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
