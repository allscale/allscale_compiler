#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"

#include "allscale/compiler/analysis/data_requirement.h"

namespace allscale {
namespace compiler {
namespace backend {

	/**
	 * A set of language extensions comprising types and symbols for
	 * entities offered by the runtime interface.
	 */
	class AllScaleBackendModule : public insieme::core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class insieme::core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		AllScaleBackendModule(insieme::core::NodeManager& manager) : insieme::core::lang::Extension(manager) {}


	public:

		// import reference extension
		IMPORT_MODULE(insieme::core::lang::ReferenceExtension);

		// --- work item description ---

		LANG_EXT_LITERAL(CreateWorkItemDescription, "art_wi_desc_create", "('a,type<('Args...)>,type<'Res>)->art_wi_desc<('Args...),'Res>");

		LANG_EXT_LITERAL(CreateWorkItemDescriptionReference, "art_wi_desc_ref", "(identifier,type<cpp_ref<('Args...),t,f>>,type<'Res>)->art_wi_desc<('Args...),'Res>");


		// --- a primitive to start a work item ---

		// the operator for spawning a work item
		LANG_EXT_LITERAL(SpawnWorkItem,      "art_spawn",       "(dependencies,art_wi_desc<('Args...),'Res>,'InitArgs...)->treeture<'Res,f>");
		LANG_EXT_LITERAL(SpawnFirstWorkItem, "art_spawn_first", "(dependencies,art_wi_desc<('Args...),'Res>,'InitArgs...)->treeture<'Res,f>");

		// the operator for wrapping the execution of the main function
		LANG_EXT_LITERAL(ProcessMain, "art_main", "(art_wi_desc<('Args...),'Res>,'Args...)->'Res");


		// --- a primitive to wrap up work item instances not yet spawned ---

		LANG_EXT_LITERAL(PrecFunCreate,    "art_make_precfun",        "(('Closure...),(cpp_ref<dependencies,t,f>,cpp_ref<('Arg,'Closure...),t,f>)->treeture<'Res,f>)->precfun<'Arg,'Res>");
		LANG_EXT_LITERAL(PrecFunToFun,     "art_precfun_to_fun",      "(precfun<'Arg,'Res>)->('Arg)->treeture<'Res,f>");
		LANG_EXT_LITERAL(PrecFunToDepFun,  "art_precfun_to_dep_fun",  "(precfun<'Arg,'Res>)->(dependencies,'Arg)->treeture<'Res,f>");


		// --- data item handling ---

		LANG_EXT_TYPE(DataItemRefGen,"art_data_item_ref<'a>");

		LANG_EXT_LITERAL(CreateDataItem,  "art_data_item_create",  "(type<'a>, 'args...) -> art_data_item_ref<'a>");
		LANG_EXT_LITERAL(GetDataItem,     "art_data_item_get",     "(ref<art_data_item_ref<'a>,'c,'v,cpp_ref>) -> ref<art_data_item_ref<'a>,'c,'v,cpp_ref>");


		// --- data item access modes ---

		LANG_EXT_TYPE(AccessMode, "art_access_mode");
		LANG_EXT_LITERAL(AccessModeReadOnly,  "allscale::runtime::AccessMode::ReadOnly",  "art_access_mode");
		LANG_EXT_LITERAL(AccessModeReadWrite, "allscale::runtime::AccessMode::ReadWrite", "art_access_mode");


		// --- data item requirements ---

		LANG_EXT_TYPE(DataItemRequirementGen, "art_data_item_requirement<'a>");
		LANG_EXT_LITERAL(CreateDataItemRequirement,  "art_data_item_requirement_create",  "(ref<art_data_item_ref<'a>,'c,'v,cpp_ref>,'b,art_access_mode) -> art_data_item_requirement<'a>");


		// --- data item ranges ---

		LANG_EXT_LITERAL(DataItemRangeUnion, "art_data_item_range_union", "('a,'b...) -> 'a");
		LANG_EXT_LITERAL(DataItemRangeSpan,  "art_data_item_range_span",  "('a,'a) -> 'a");


		// --- data item access checks ---

		LANG_EXT_LITERAL(DataItemCheckReadAccess,  "art_data_item_check_read",  "(ref<'a,'c,'v,'k>) -> ref<'a,'c,'v,'k>");
		LANG_EXT_LITERAL(DataItemCheckWriteAccess, "art_data_item_check_write", "(ref<'a, f,'v,'k>) -> ref<'a, f,'v,'k>");

		// --- misc ---

		// a primitive for converting captured references from the pointer within the closure struct to a reference
		LANG_EXT_LITERAL(RefRefPlainToRefRefCpp,  "art_ref_ref_plain_to_ref_ref_cpp",  "(ref<ref<'a,'c1,'v1,plain>,'c2,'v2,'k>)->ref<ref<'a,'c1,'v1,cpp_ref>,'c2,'v2,'k>");

		// treeture<unused_type> is returned in lieu of treeture<unit>
		LANG_EXT_TYPE(UnusedType, "art_unused_type");
		LANG_EXT_LITERAL(MakeUnusedType, "art_make_unused_type", "() -> art_unused_type");



	};

	// -- utility functions --

	/**
	 * Tests whether the given node is a data item reference type or an expression of that type.
	 */
	bool isDataItemReference(const insieme::core::NodePtr&);

	/**
	 * Creates a data item reference type wrapping up the given data item type.
	 */
	insieme::core::TypePtr getDataItemReferenceType(const insieme::core::TypePtr&);

	/**
	 * Extracts the type of data item referenced by the given data item reference.
	 */
	insieme::core::TypePtr getReferencedDataItemType(const insieme::core::NodePtr&);

	/**
	 * Tests whether the given node is a data item reference type or an expression of that type.
	 */
	bool isDataItemRequirement(const insieme::core::NodePtr&);

	/**
	 * Creates a data item requirement type for the given data item type.
	 */
	insieme::core::TypePtr getDataItemRequirementType(const insieme::core::TypePtr&);

	/**
	 * Extracts the type of data item referenced by the given data item type.
	 */
	insieme::core::TypePtr getRequiredDataItemType(const insieme::core::NodePtr&);

	/**
	 * Creates an expression constructing a data requirement for the given data item reference.
	 */
	insieme::core::ExpressionPtr createDataItemRequirement(const insieme::core::ExpressionPtr& dataItemRef, const insieme::core::ExpressionPtr& range, analysis::AccessMode mode);

	/**
	 * Tests whether the given node is an unused type or an expression of that type.
	 */
	bool isUnusedType(const insieme::core::NodePtr&);

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
