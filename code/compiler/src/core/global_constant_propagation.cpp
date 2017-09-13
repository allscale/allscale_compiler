#include "allscale/compiler/core/global_constant_propagation.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	namespace {

		LiteralPtr isGlobalVariable(const NodePtr& cur) {
			auto& mgr = cur.getNodeManager();
			auto& refExt = mgr.getLangExtension<lang::ReferenceExtension>();

			if (auto call = refExt.isCallOfRefKindCast(cur)) {
				return isGlobalVariable(call->getArgument(0));
			}

			// a global constant is a reference to a literal
			auto lit = cur.isa<LiteralPtr>();
			return (lit && lang::isReference(lit)) ? lit : LiteralPtr();
		}

	}


	NodePtr propagateGlobalConstants(const NodePtr& code) {

		// assume the given IR is correct
		assert_correct_ir(code);

		// get some utilities
		NodeManager& mgr = code->getNodeManager();
		auto& refExt = mgr.getLangExtension<lang::ReferenceExtension>();
		IRBuilder builder(mgr);

		struct Constant {
			ExpressionPtr value;
			ExpressionPtr initExpr;
		};

		// map literals to their initial values
		std::map<LiteralPtr,Constant> constants;

		auto markAsNotConstant = [&](const LiteralPtr& lit) {
			// a null pointer marks it as a not-constant literal
			constants[lit].value = ExpressionPtr();
		};

		auto addConstant = [&](const LiteralPtr& lit, const Constant& constant) {
			auto pos = constants.find(lit);
			if (pos == constants.end()) {
				constants[lit] = constant;
			} else {
				markAsNotConstant(lit);
			}
		};


		// collect all constants
		visitDepthFirstOnce(code, [&](const NodePtr& cur){

			// -- looking for init expressions --
			if (auto init = cur.isa<InitExprPtr>()) {

				// if this init expression is targeting a global value
				if (auto lit = isGlobalVariable(init->getMemoryExpr())) {

					// there must be one argument
					if (init->getInitExprs().size() == 1) {

						// this argument is not another global
						auto initValue = init->getInitExprs()->getElement(0);
						if (!isGlobalVariable(initValue)) {

							// we have a new constant initialization
							addConstant(lit, { initValue, init });

							// done
							return;
						}
					}
				}

				// if a global literal is among the init expressions => not a constant
				for(const auto& cur : init->getInitExprs()) {
					if (auto lit = isGlobalVariable(cur)) {
						markAsNotConstant(lit);
					}
				}

				// done
				return;
			}

			// for the rest we are only interested in call expressions
			auto call = cur.isa<CallExprPtr>();
			if (!call) return;

			// read operations on globals are fine
			if (refExt.isCallOfRefDeref(call)) return;

			// so are kind casts
			if (refExt.isCallOfRefKindCast(call)) return;

			// assignments on globals may only happen once (init)
			if (refExt.isCallOfRefAssign(call)) {
				if (auto lit = isGlobalVariable(call->getArgument(0))) {
					// this might be another constant
					addConstant(lit, { call->getArgument(1), call });
					return;
				}
			}

			// if a global literal is passed to any other function => fail
			for(const auto& decl : call->getArgumentDeclarations()) {
				// if the parameter is not a reference, we are fine
				if (!lang::isReference(decl->getType())) continue;

				// if a global is passed by reference => that is a problem
				if (auto lit = isGlobalVariable(cur)) {
					markAsNotConstant(lit);
				}
			}

		}, true, true);


		// build map of replacements
		core::NodeMap replacements;
		for(const auto& cur : constants) {
			// only include real constants
			if (!cur.second.value) continue;

			// replace initialization by a no-op
			replacements[cur.second.initExpr] = builder.getNoOp();

			// add replacements for constants
			replacements[builder.deref(cur.first)] = cur.second.value;
			replacements[lang::buildRefKindCast(cur.first, lang::ReferenceType::Kind::CppReference)] = cur.second.value;
			replacements[lang::buildRefKindCast(cur.first, lang::ReferenceType::Kind::CppRValueReference)] = cur.second.value;
		}

		// apply replacements
		auto res = transform::replaceAll(mgr, code, replacements, transform::globalReplacement);

		// make some final checks
		assert_correct_ir(res);

		// done
		return res;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
