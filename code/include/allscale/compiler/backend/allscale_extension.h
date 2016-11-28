#pragma once

#include "insieme/core/lang/extension.h"

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


		// --- work item description ---

		LANG_EXT_TYPE(WorkItemDescriptionGen, "art_wi_desc<'Res,'Args...>");

		LANG_EXT_LITERAL(CreateWorkItemDescription, "art_wi_desc_create", "('a,type<('Args...)>,type<'Res>)->art_wi_desc<('Args...),'Res>");

		LANG_EXT_LITERAL(CreateWorkItemDescriptionReference, "art_wi_desc_ref", "(identifier,type<('Args...)>,type<'Res>)->art_wi_desc<('Args...),'Res>");


		// --- a primitive to start a work item ---

		// the operator for spawning a work item
		LANG_EXT_LITERAL(SpawnWorkItem, "art_spawn", "(art_wi_desc<('Args...),'Res>,'Args...)->treeture<'Res,f>");

		// the operator for spawning a recursively nested work item
		LANG_EXT_LITERAL(RecSpawnWorkItem, "art_rec_spawn", "(art_wi_desc<('Args...),'Res>)->('Args...)->treeture<'Res,f>");

		// the operator for wrapping the execution of the main function
		LANG_EXT_LITERAL(ProcessMain, "art_main", "(art_wi_desc<('Args...),'Res>,'Args...)->'Res");

	};

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
