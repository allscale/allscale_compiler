#include "allscale/compiler/core/data_item_conversion.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/core/data_item_annotation.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	NodePtr convertDataItemReferences(const NodePtr& code, const ProgressCallback&) {

		// To be transformed:
		//  - every ref<data_item,_,_,_> is transforemd to a art_data_item_ref<data_item>
		//  - when accessing a art_data_item_ref<data_item>, it is converted back to a ref<data_item,_,_,_> through a art_data_item_get
		//  - constructor calls of data items are replaced by art_data_item_create calls
		//  - todo: destruction of data items must be marked!

		auto& mgr = code->getNodeManager();
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

		return core::transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// replace reference types
			if (auto type = node.isa<TypePtr>()) {
				if (isDataItem(type)) {
					return backend::getDataItemReferenceType(type);
				}
				return type;
			}

			// check for constructor calls
			if (auto decl = node.isa<DeclarationStmtPtr>()) {

				// get the variable
				auto var = decl->getVariable();

				// only interested in references
				if (!insieme::core::lang::isReference(var)) {
					return decl;
				}

				// extract the potential data item reference type
				auto dataItemRefType = insieme::core::analysis::getReferencedType(var->getType());

				// if the declared value is a DataItemReference, we have to fix the constructor
				if (backend::isDataItemReference(dataItemRefType)) {

					// retrieve the initialization value
					auto init = decl->getDeclaration()->getInitialization();

					// check the init expression
					if (init.isa<VariablePtr>()) return decl;

					// this should be a call expression
					assert_true(init.isa<CallExprPtr>()) << "Non-call-expression found: " << dumpReadable(init) << " of type " << init->getNodeType();

					// only interested in constructor calls
					auto call = init.as<CallExprPtr>();
					if (!call->getFunctionExpr()->getType().as<FunctionTypePtr>()->isConstructor()) return decl;

					// => replace it by a data item creation call
					ExpressionList args = call->getArgumentList();
					args[0] = builder.getTypeLiteral(backend::getReferencedDataItemType(dataItemRefType));

					// build the substitution declaration statement
					auto res = builder.declarationStmt(
						builder.declaration(var->getType(),builder.callExpr(ext.getCreateDataItem(),args)),
						var
					);

					return res;
				}
			}

			// check for accesses
			if (auto call = node.isa<CallExprPtr>()) {
				// if the targeted function is a member function
				auto funType = call->getFunctionExpr()->getType().as<FunctionTypePtr>();
				if (funType->isMemberFunction()) {

					// only interested if the object type is a data item reference
					if (!backend::isDataItemReference(insieme::core::analysis::getObjectType(funType))) return call;

					// unwrap each argument that is a data item reference
					std::map<NodeAddress,NodePtr> replacements;
					for(const auto& arg : CallExprAddress(call)->getArgumentList()) {
						if (backend::isDataItemReference(insieme::core::analysis::getReferencedType(arg->getType()))) {
							// we need to unpack the data item reference
							replacements[arg] = builder.callExpr(ext.getGetDataItem(),core::lang::buildRefKindCast(arg,core::lang::ReferenceType::Kind::CppReference));
						}
					}

					// apply replacement
					return (replacements.empty()) ? call : core::transform::replaceAll(mgr,replacements);
				}
			}

			// everything else, we are not interested
			return node;

		}, core::transform::globalReplacement);
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
