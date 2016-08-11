#pragma once

#include <iostream>
#include <utility>

#include "parec/core.h"
#include "parec/async.h"

namespace parec {

	// TODO: add functional utils ...

	template<typename T>
	struct id {
		T operator()(const T& v) const { return v; }
	};


	template<typename O, typename I>
	struct const_value {
		O value;
		const_value(const O& v) : value(v) {}
		const O& operator()(const I&) const { return value; }
	};

	template<typename O, typename I>
	const_value<O,I> constValue(const O& v) { return const_value<O,I>(v); }



	template<typename T>
	class set {

		struct node;

		struct isLeaf {
			bool operator()(const node* node) const { return !node; }
		};


		// the node for the binary tree
		struct node {

			T value;
			node* l;
			node* r;

			node(const T& value) : value(value), l(nullptr), r(nullptr) {};
			node(T&& value) : value(value), l(nullptr), r(nullptr) {};
			node(const T& value, node* l, node* r) : value(value), l(l), r(r) { };

			node(const node& other) : value(other.value), l(other.l->copy()), r(other.r->copy()) {}
			node(node&& other) : value(other.value), l(other.l), r(other.r) {}

			~node() {
				delete l;
				delete r;
			}

			/**
			 * Creates a deep-copy of this node -- ownership is passed on to the caller.
			 */
			node* copy() const {
				return prec(
						[](const node* node)->bool { return !node; },
						[](const node*)->node* { return nullptr; },
						[](const node* n, const auto& f)->node* {
							auto l = f(n->l);
							auto r = f(n->r);
							return new node(n->value, l.get(), r.get());
						}
				)(this).get();
			}

			/**
			 * Computes the number of elements in this sub-tree.
			 */
			std::size_t size() const {
				// implemented using the generic parallel recursive operator
				return prec(
						[](const node* node)->bool { return !node; },
						[](const node*)->std::size_t { return 0; },
						[](const node* n, const auto& f)->std::size_t {
							auto a = f(n->l);
							auto b = f(n->r);
							return 1 + a.get() + b.get();
						}
				)(this).get();
			}

			/**
			 * Tests whether the given value is in this set.
			 */
			bool contains(const T& value) const {
				// compute recursively use common operator
				return prec(
						[&](const node* cur)->bool { return !cur || cur->value == value; },
						[](const node* cur)->bool { return cur; },
						[&](const node* cur, const auto& f)->bool {
							return (cur->value < value) ? f(cur->r).get() : f(cur->l).get();
						}
				)(this).get();
			}

			/**
			 * Tests whether all the nodes of the given tree are in this tree too.
			 */
			bool containsAll(const node* other) const {
				typedef std::pair<const node*,const node*> pair;

				return prec(
						[](pair p) { return !p.first || !p.second; },
						[](pair p) { return !p.second; },
						[](pair p, const auto& f)->bool {

							auto x = p.first;
							auto o = p.second;

							// if they are the same pointer, we are done
							if (x == o) return true;

							// if values are equivalent ...
							if (x->value == o->value) {
								return all(
									f(pair(x->l,o->l)),
									f(pair(x->r,o->r))
								);
							}

							if (x->value < o->value) {

								node tmp(o->value);

								return all(
									f(pair(x, o->l)),
									f(pair(x->r, o->r)),
									f(pair(x->r, &tmp))
								);

							}

							node tmp(o->value);
							return all(
								f(pair(x->l, o->l)),
								f(pair(x->l, &tmp)),
								f(pair(x, o->r))
							);

						}
				)(pair(this, other)).get();

			}

			/**
			 * Inserts a set of nodes into this set.
			 */
			node* insert(node* other) {
				typedef std::pair<node*,node*> pair;

				return prec(
						[](pair cur)->bool { return !cur.first || !cur.second; },
						[](pair cur)->node* { return (cur.first) ? cur.first : cur.second ; },
						[](pair cur, const auto& f)->node* {

							using value_type = decltype(f(pair()));

							auto x = cur.first;
							auto o = cur.second;

							value_type l,r;

							// if values are equivalent ...
							if (x->value == o->value) {

								// add values child nodes only
								l = f(pair(x->l, o->l));
								r = f(pair(x->r, o->r));

								// node o is not used in the result set
								o->l = nullptr;
								o->r = nullptr;
								delete o;

							// if it is to be inserted on the right side
							} else if (x->value < o->value) {

								// cut of left branch
								auto b = o->l;
								o->l = nullptr;

								// add fragments
								l = f(pair(x->l, b));
								r = f(pair(x->r, o));

							// if it is to be inserted on the left side
							} else {

								// cut of right branch
								auto b = o->r;
								o->r = nullptr;

								// add fragments
								l = f(pair(x->l, o));
								r = f(pair(x->r, b));

							}

							// wait for fragments
							x->l = l.get();
							x->r = r.get();
							return x;
						}
				)(pair(this, other)).get();
			}

			/**
			 * Support for a print function.
			 */
			std::ostream& printTo(std::ostream& out) const {
				if (l) {
					l->printTo(out);
					out << ",";
				}
				out << value;
				if (r) {
					out << ",";
					r->printTo(out);
				}
				return out;
			}

		};

		node* root;

	public:

		set() : root(nullptr) {}

		set(const T& value) : root(new node(value)) { }

		set(T&& value) : root(new node(value)) { }

		set(const set<T>& other) : root(other.root->copy()) {}

		set(set<T>&& other) : root(other.root) {}

		~set() {
			delete root;
		}

		bool empty() const {
			return !root;
		}

		std::size_t size() const {
			return root->size();
		}

		// --- observers ---

		bool contains(const T& value) const {
			return root->contains(value);
		}

		bool isSubsetOf(const set<T>& other) const {
			return other.root->containsAll(root);
		}

		bool operator==(const set<T>& other) const {
			return all(
				async([&]()->bool { return this->isSubsetOf(other); }),
				async([&]()->bool { return other.isSubsetOf(*this); })
			);
		}

		bool operator!=(const set<T>& other) const {
			return !(*this == other);
		}

		// --- mutators ---

		set<T>& insert(const set<T>& other) {
			root = root->insert(other.root->copy());
			return *this;
		}

		set<T>& insert(const T& value) {
			return insert(set<T>(value));
		}

		set<T>& insert(T&& value) {
			return insert(set<T>(value));
		}

		set<T>& insert() {
			return *this;
		}

		template<typename ... Ts>
		set<T>& insert(const T& first, const Ts& ... ts) {
			return insert(first).insert(ts...);
		}

		std::ostream& printTo(std::ostream& out) const {
			out << "{";
			if (root) root->printTo(out);
			out << "}";
			return out;
		}

	};


} // end namespace parec

namespace std {

	template<typename T>
	std::ostream& operator<<(std::ostream& out, const parec::set<T>& set) {
		return set.printTo(out);
	}

}
