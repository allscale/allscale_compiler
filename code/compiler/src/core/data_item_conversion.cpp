#include "allscale/compiler/core/data_item_conversion.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/core/data_item_annotation.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/allscale_utils.h"


#include "insieme/core/dump/json_dump.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	NodePtr convertDataItemReferences(const NodePtr& code, const ProgressCallback&) {

		// To be transformed:
		//  - every data_item is transformed to a art_data_item_ref<data_item>
		//  - when accessing a art_data_item_ref<data_item>, it is converted back to a data_item through a art_data_item_get
		//  - constructor calls of data items are replaced by art_data_item_create calls

		auto& mgr = code->getNodeManager();
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

		auto res = core::transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// replace reference types
			if (auto type = node.isa<TypePtr>()) {
				if (isDataItem(type)) {
					return backend::getDataItemReferenceType(type);
				}
				return type;
			}

			// check for accesses
			if (const auto& call = node.isa<CallExprPtr>()) {

				// handle constructor calls
				if(core::analysis::isConstructorCall(call)) {

					ExpressionPtr callee = call->getFunctionExpr();
					core::TypePtr objectType = core::analysis::getObjectType(callee->getType());
					// ... which create data item references
					if(backend::isDataItemReference(objectType)) {

						// skip copy or move constructor calls
						if( core::analysis::isCopyConstructor(callee) || core::analysis::isMoveConstructor(callee)) {
							// we simply return the copied/moved object
							return core::lang::removeSurroundingRefCasts(core::analysis::getArgument(call, 1));
						}

						// replace constructor call with call to getCreateDataItem
						DeclarationList args = call->getArgumentDeclarationList();
						auto arg = builder.getTypeLiteral(backend::getReferencedDataItemType(objectType));
						args[0] = builder.declaration(core::transform::materialize(arg->getType()), arg);

						return builder.callExpr(ext.getCreateDataItem(), args);
					}
				}

				// if the targeted function is a member function
				auto funType = call->getFunctionExpr()->getType().as<FunctionTypePtr>();
				if (funType->isMemberFunction()) {

					// only interested if the object type is a data item reference
					if (!backend::isDataItemReference(insieme::core::analysis::getObjectType(funType))) return call;

					// unwrap each argument that is a data item reference
					std::map<NodeAddress,NodePtr> replacements;
					for(const auto& oldArg : CallExprAddress(call)->getArgumentList()) {
						core::lang::ReferenceType::Kind oldRefKind = core::lang::ReferenceType(oldArg).getKind();
						// the argument might be a cast to the correct type here. if it is a cast, we can remove it, as we'll cast ourselves later on
						auto arg = core::lang::removeSurroundingRefCasts(oldArg);
						if (backend::isDataItemReference(insieme::core::analysis::getReferencedType(arg->getType()))) {
							// we need to unpack the data item reference
							core::ExpressionPtr newArg = builder.callExpr(ext.getGetDataItem(), core::lang::buildRefKindCast(arg, core::lang::ReferenceType::Kind::CppReference));
							// however, we need to cast to the original type, if that wasn't cpp_ref, which we produce now
							if(oldRefKind != core::lang::ReferenceType::Kind::CppReference) {
								newArg = core::lang::buildRefKindCast(newArg, oldRefKind);
							}
							replacements[oldArg] = newArg;
						}
					}

					// apply replacement
					return (replacements.empty()) ? call : core::transform::replaceAll(mgr,replacements);
				}
			}

			// everything else, we are not interested
			return node;

		}, core::transform::globalReplacement);

		// remove data item tag on references (the are produced above as a side-effect)
		visitDepthFirstOnce(res,[&](const TypePtr& type) {
			if (backend::isDataItemReference(type)) {
				removeDataItemMark(type);
			}
		},true,true);

		// done
		return res;
	}

	namespace {

		bool containsDataItemReference(const TypePtr& type) {
			bool res = false;
			// we only also visit types here, and actually we only visit types by pruning at expressions. this way we visit all fields in the whole tree below 'type'
			visitDepthFirstOncePrunable(type,[&](const NodePtr& node){
				// check whether it can be pruned here
				if (res || node.isa<ExpressionPtr>()) return core::Action::Prune;

				// check whether the current type is a data item reference
				if (auto type = node.isa<TypePtr>()) {
					if (backend::isDataItemReference(type)) {
						res = true;
						return core::Action::Prune;
					}
				}

				// continue
				return core::Action::Descent;
			}, true);
			return res;
		}

		int numNestedRefs(const TypePtr& type) {
			if (core::lang::isReference(type)) {
				return 1 + numNestedRefs(core::analysis::getReferencedType(type));
			}
			return 0;
		}

		core::TagTypePtr fixTagType(const core::TagTypePtr& tagType) {
			auto structType = tagType->getStruct();
			if(!structType) return tagType;

			core::IRBuilder builder(tagType->getNodeManager());
			core::ParentList parents = structType->getParents()->getElements();
			core::FieldList fields = structType->getFields()->getElements();
			auto thisType = builder.refType(tagType->getTag());

			// we only continue here, if the passed struct can actually have a copy ctor - i.e. all fields are copyable
			if(!::all(fields, [](const auto& field) { return utils::isCopyable(field->getType()); })) return tagType;

			core::ExpressionList ctors = structType->getConstructors()->getElements();
			core::MemberFunctionList mfuns = structType->getMemberFunctions()->getElements();

			// handle constructors
			auto copyCtor = core::analysis::buildDefaultCopyConstructor(thisType, parents, fields);
			for(auto& ctor : ctors) {
				// if this is a defaulted copy ctor, we replace it
				if(core::analysis::isCopyConstructor(ctor) && core::analysis::isaDefaultConstructor(ctor)) {
					ctor = copyCtor;

				// if this is a defaulted move ctor, we replace it
				} else if(core::analysis::isMoveConstructor(ctor) && core::analysis::isaDefaultConstructor(ctor)) {
					auto moveCtor = core::analysis::buildDefaultMoveConstructor(thisType, parents, fields);;
					ctor = moveCtor;
				}
			}
			// add a copy constructor if there isn't one until now
			if(!::any(ctors, [](const auto& ctor) { return core::analysis::isCopyConstructor(ctor); })) {
				ctors.push_back(copyCtor);
			}

			// delete defaulted assignment operators. These have been generated by the frontend in error and shouldn't actually be there at all
			mfuns.erase(std::remove_if(mfuns.begin(), mfuns.end(), [](const auto& mfun) {
				return core::analysis::isaDefaultAssignment(mfun);
			}), mfuns.end());

			// create a new struct and normalize it
			return builder.structType(structType->getName()->getValue(), parents, fields, ctors,
			                          structType->hasDestructor() ? structType->getDestructor() : core::ExpressionPtr(), structType->getDestructorVirtual()->getValue(),
			                          mfuns, structType->getPureVirtualMemberFunctions()->getElements(), structType->getStaticMemberFunctions()->getElements());
		}
	}


	insieme::core::NodePtr convertCapturedDataItemReferences(const insieme::core::NodePtr& node, const ProgressCallback&) {
		auto& mgr = node->getNodeManager();
		IRBuilder builder(mgr);
		const auto& basic = mgr.getLangBasic();
		const auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

		// a utility to identify fields of captur structs
		auto isCapturedFieldName = [](const StringValuePtr& name) {
			return name->getValue().substr(0,8) == "capture_";
		};

		// a utility to identify types of captured fields that need to be rewritten
		auto isTargetedFieldType = [](const TypePtr& type) {
			if (!core::lang::isReference(type)) return TypePtr();
			auto elementType = core::analysis::getReferencedType(type);
			return containsDataItemReference(elementType) ? elementType : TypePtr();
		};

		// convert captured data item references to values
		auto res = core::transform::transformBottomUp(node,[&](const NodePtr& node)->NodePtr {

			// replace field types
			if (auto field = node.isa<FieldPtr>()) {
				auto type = field->getType();

				// check that this is a captured value (not too save, sorry)
				if (!isCapturedFieldName(field->getName())) return field;

				// if this is a reference to something containing a data item reference
				if (auto resType = isTargetedFieldType(type)) {
					// we have to capture it by value instead
					return builder.field(field->getName(),resType);
				}
			}

			// fix struct default members
			if(const auto& tagType = node.isa<core::TagTypePtr>()) {
				// if this struct contains a data item reference directly or indirectly, we need to fix it's copy/move ctors and remove defaulted assignment operators
				if(containsDataItemReference(tagType)) {
					return fixTagType(tagType);
				}
			}

			// also replace field accesses
			if (auto call = node.isa<CallExprPtr>()) {

				// first we are interested in member accesses
				bool isAccess = core::analysis::isCallOf(call,basic.getCompositeMemberAccess());
				bool isRefAccess = !isAccess && core::analysis::isCallOf(call,refExt.getRefMemberAccess());
				if (isAccess || isRefAccess) {

					// extract arguments
					auto obj = call->getArgument(0);
					auto field = call->getArgument(1);
					auto type = call->getArgument(2);

					// check field name
					if (isCapturedFieldName(field.as<LiteralPtr>()->getValue())) {

						// check field type
						if (auto newType = isTargetedFieldType(core::analysis::getRepresentedType(type))) {
							return builder.callExpr(
									(isAccess) ? basic.getCompositeMemberAccess() : refExt.getRefMemberAccess(),
									obj, field, builder.getTypeLiteral(newType)
							);
						}
					}
				}

				// where might also be too much deref calls
				if (refExt.isCallOfRefDeref(call)) {

					// if this is the proper type => skip the deref
					auto arg = call->getArgument(0);
					if (numNestedRefs(call->getType()) == numNestedRefs(arg->getType())) {
						if (refExt.isCallOfRefMemberAccess(arg) && isTargetedFieldType(arg->getType())) {
							// perform a ref-cast instead
							return core::lang::buildRefCast(arg,call->getType());
						}
					}
				}

				return call;
			}

			// fix declarations which got broken because we changed the type if their initialization
			if(auto decl = node.isa<DeclarationPtr>()) {
				const auto& arg = decl->getInitialization();
				if(core::lang::isReference(arg)) {
					core::lang::ReferenceType::Kind declRefKind = core::lang::ReferenceType(decl->getType()).getKind();
					// only consider declarations, which are initialized with a member access of DI reference type
					if((core::analysis::isCallOf(arg, basic.getCompositeMemberAccess()) || core::analysis::isCallOf(arg, refExt.getRefMemberAccess()))
							&& containsDataItemReference(insieme::core::analysis::getReferencedType(arg))) {
						// cast the initialization to the type of the declaration
						auto newArg = core::lang::buildRefKindCast(arg, declRefKind);
						return builder.declaration(decl->getType(), newArg);
					}
				}
			}

			// fix InitExpr nodes
			if (auto initExpr = node.isa<InitExprPtr>()) {
				const auto& initExprType = initExpr->getType();
				const auto& memoryExpr = initExpr->getMemoryExpr();
				const auto& memoryExprType = memoryExpr->getType().as<core::GenericTypePtr>();
				// ... which initialize a struct with a field which's type we changed from cpp_ref to plain
				if(core::lang::isReference(memoryExprType)) {
					if(const auto& tagType = core::analysis::getReferencedType(memoryExprType).isa<core::TagTypePtr>()) {
						// if we have any field which captures a data item
						if(::any(tagType->getRecord()->getFields(), [&](const auto& field) { return isCapturedFieldName(field->getName()) && containsDataItemReference(field->getType()); })) {
							// we need to fix the initializations. Previously we had cpp_ref, now we need plain
							core::DeclarationList decls = initExpr->getInitDecls();
							for(auto& decl : decls) {
								const auto& declType = decl->getType();
								if(core::lang::isCppReference(declType) && containsDataItemReference(insieme::core::analysis::getReferencedType(declType))) {
									auto newInit = core::lang::removeSurroundingRefCasts(decl->getInitialization());
									decl = utils::buildPassByValueDeclaration(newInit);
								}
							}
							return builder.initExpr(memoryExprType, memoryExpr, decls);
						}
					}
				}
				// ... which we broke by changing calls in their memory location
				if(initExprType != memoryExprType) {
					return builder.initExpr(memoryExprType, memoryExpr, initExpr->getInitDecls());
				}
			}

			// default, do nothing
			return node;
		}, core::transform::globalReplacement);

		// check wether this step was correct
		assert_correct_ir(res);

		// done
		return res;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
