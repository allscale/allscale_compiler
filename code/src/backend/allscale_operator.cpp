#include "allscale/compiler/backend/allscale_operator.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/type_manager.h"

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
		}



		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale