
#pragma once

#include "insieme/core/lang/extension.h"

namespace ic = insieme::core;

namespace allscale {
namespace compiler {

	class AllscaleModule : public ic::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class ic::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		AllscaleModule(ic::NodeManager& manager) : ic::lang::Extension(manager) {}

	  public:

		/**
		 * A constructor for functions usable within a prec call.
		 * Each function has a cutoff check, at least one base case and at least one step case
		 */
		LANG_EXT_LITERAL(BuildRecfun, "build_recfun" , "(('a) -> bool, list<('a) => 'b>, list<('a, (recfun<'a,'b>, 'c...)) => treeture<'b>>) -> recfun<'a,'b>")

		LANG_EXT_LITERAL(Prec, "prec", "( (recfun<'a,'b>, 'c...) ) -> ('a) => treeture<'b>")
		LANG_EXT_LITERAL(TreetureDone, "treeture_done", "('a) -> treeture<'a>")

		LANG_EXT_LITERAL(TreetureGet, "treeture_get", "(treeture<'a>) -> 'a")
		LANG_EXT_LITERAL(TreetureLeft, "treeture_left", "")
		LANG_EXT_LITERAL(TreetureRight, "treeture_right", "")

		LANG_EXT_DERIVED(TreetureWait, "(t : treeture<'a>) -> unit { treeture_get(t); }")

		LANG_EXT_LITERAL(RecfunCall, "recfun_call", "(recfun<'a,'b>, 'a) -> treeture<'b>")

	};



}
}
