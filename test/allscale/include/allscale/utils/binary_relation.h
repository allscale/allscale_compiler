#pragma once

#include <bitset>
#include <iterator>
#include <utility>


#include "allscale/utils/range.h"

namespace allscale {
namespace utils {

	namespace detail {

		template<typename ID, typename element_type = std::pair<ID,ID>>
		class const_iterator;

		template<typename ID, unsigned Level>
		class node;

		template<typename ID, unsigned Level>
		class node_base {
		public:

			using element_type = std::pair<ID,ID>;

			std::size_t getPosition(std::size_t a, std::size_t b) const {
				a = (a >> (Level*4)) & 0xF;
				b = (b >> (Level*4)) & 0xF;
				return (a << 4) | b;
			}

			std::size_t getPosition(const element_type& element) const {
				return getPosition(element.first, element.second);
			}

		};

		template<typename ID, unsigned Level>
		class node : public node_base<ID,Level> {

			template<typename X, typename Y>
			friend class const_iterator;

			using nested_node = node<ID,Level-1>;
			using element_type = std::pair<ID,ID>;


			std::array<nested_node*,256> next;

		public:

			node()  {
				for(auto& cur : next) cur = nullptr;
			}

			~node() {
				for(auto& cur : next) delete cur;
			}

			template<typename PrivFactory, typename SuccFactory>
			bool insert(const element_type& entry, const PrivFactory& priv_factory, const SuccFactory& succ_factory) {
				auto pos = this->getPosition(entry);

				auto local_priv_factory = [&]()->const node<ID,0>* {
					// init predecessor
					const node<ID,0>* priv = nullptr;
					// narrow predecessor in local list
					for(int i=pos-1; i>=0; --i) {
						if (next[i]) {
							priv = next[i]->getLast();
							break;
						}
					}
					if (!priv) {
						priv = priv_factory();
					}
					return priv;
				};

				auto local_succ_factory = [&]()->const node<ID,0>* {
					// init successor
					const node<ID,0>* succ = nullptr;
					// narrow successor in local list
					for(unsigned i = pos+1; i<next.size();++i) {
						if (next[i]) {
							succ = next[i]->getFirst();
							break;
						}
					}
					if (!succ) {
						succ = succ_factory();
					}
					return succ;
				};

				// create new child node if necessary
				if (next[pos] == nullptr) {

					auto newNode = new nested_node();
					next[pos] = newNode;

					// insert the new entry
					newNode->insert(entry);

					auto newLeaf = newNode->getFirst();

					// link the new node into the list of linked leaf nodes

					// get priv and succ
					auto priv = local_priv_factory();
					auto succ = local_succ_factory();

					newLeaf->priv = priv;
					newLeaf->succ = succ;

					if (priv) priv->succ = newLeaf;
					if (succ) succ->priv = newLeaf;

					// done
					return true;
				}

				// insert node into pre-existing child node
				return next[pos]->insert(
						entry,
						local_priv_factory,
						local_succ_factory
				);
			}

			bool insert(const element_type& entry) {
				return insert(
					entry,
					[]()->const node<ID,0>* { return nullptr; },
					[]()->const node<ID,0>* { return nullptr; }
				);
			}

			bool contains(const element_type& entry) const {
				auto pos = this->getPosition(entry);
				return (next[pos]) ? next[pos]->contains(entry) : false;
			}

			const node<ID,0>* getFirst() const {
				for(const auto& cur : next) {
					if (cur) return cur->getFirst();
				}
				return nullptr;
			}

			const node<ID,0>* getLast() const {
				for(auto it = next.rbegin(); it != next.rend(); ++it) {
					if (*it) return (*it)->getLast();
				}
				return nullptr;
			}
		};


		template<typename ID>
		class node<ID,0> : public node_base<ID,0> {
		public:

			using element_type = std::pair<ID,ID>;

		private:

			template<typename X, typename Y>
			friend class const_iterator;

			std::bitset<256> data;
			element_type base;

		public:

			mutable const node<ID,0>* priv = nullptr;
			mutable const node<ID,0>* succ = nullptr;

			template<typename PrivFactory, typename SuccFactory>
			bool insert(const element_type& entry, const PrivFactory& priv_factory, const SuccFactory& succ_factory) {

				// fix/update the base
				base.first = entry.first & (~0xF);
				base.second = entry.second & (~0xF);

				// insert data element
				auto pos = this->getPosition(entry);
				auto res = !data[pos];
				data.set(pos);
				return res;
			}

			bool insert(const element_type& entry) {
				return insert(
					entry,
					[]()->const node<ID,0>* { return nullptr; },
					[]()->const node<ID,0>* { return nullptr; }
				);
			}

			bool contains(const element_type& entry) const {
				return data[this->getPosition(entry)];
			}

			const node<ID,0>* getFirst() const {
				return this;
			}

			const node<ID,0>* getLast() const {
				return this;
			}

			const node<ID,0>* getNextLeaf() const {
				return succ;
			}
		};

		template<typename ID, typename element_type>
		class const_iterator : public std::iterator<std::forward_iterator_tag,element_type> {

			element_type cur;

			const node<ID,0>* leaf;
			unsigned pos;

		public:

			const_iterator(const node<ID,0>* node = nullptr, unsigned pos = 0)
				: leaf(node), pos(pos) {
				findNext();
			}

			const_iterator& operator++() {

				// walk at least one step
				++pos;

				// go to next entry
				findNext();

				// done
				return *this;
			}

			const element_type& operator*() const {
				return cur;
			}

			bool operator==(const const_iterator& other) const {
				return leaf == other.leaf && pos == other.pos;
			}

			bool operator!=(const const_iterator& other) const {
				return !(*this == other);
			}

			void findNext() {

				// terminate if there are no more leafs
				if (leaf == nullptr) return;

				// find true flag
				while(pos < leaf->data.size() && !leaf->data[pos]) ++pos;

				// check whether boundaries are found
				if (pos >= leaf->data.size()) {
					leaf = leaf->getNextLeaf();
					pos = 0;
					findNext();
					return;
				}

				// update current value
				cur = leaf->base;
				cur.first = cur.first | (pos >> 4);
				cur.second = cur.second | (pos & 0xF);

			}

		};

		template<typename ID, typename Iter, typename element_type = std::pair<ID,ID>>
		struct source_filter_iterator : public std::iterator<std::forward_iterator_tag,element_type> {

			ID src;
			Iter it;
			Iter end;

		public:

			source_filter_iterator(ID src, Iter start, Iter end)
				: src(src), it(start), end(end) {
				while (it != end && (*it).first != src) {
					++it;
				}
			}

			source_filter_iterator& operator++() {
				// search next fitting source
				do{
					++it;
				} while(it != end && (*it).first != src);
				return *this;
			}

			const element_type& operator*() const {
				return *it;
			}

			bool operator==(const source_filter_iterator& other) const {
				return it == other.it;
			}

			bool operator!=(const source_filter_iterator& other) const {
				return !(*this == other);
			}

		};

		template<typename ID, typename Iter, typename element_type = std::pair<ID,ID>>
		struct target_filter_iterator : public std::iterator<std::forward_iterator_tag,element_type> {

			ID trg;
			Iter it;
			Iter end;

		public:

			target_filter_iterator(ID trg, Iter start, Iter end)
				: trg(trg), it(start), end(end) {
				while (it != end && (*it).second != trg) {
					++it;
				}
			}

			target_filter_iterator& operator++() {
				// search next fitting target
				do{
					++it;
				} while(it != end && (*it).second != trg);
				return *this;
			}

			const element_type& operator*() const {
				return *it;
			}

			bool operator==(const target_filter_iterator& other) const {
				return it == other.it;
			}

			bool operator!=(const target_filter_iterator& other) const {
				return !(*this == other);
			}

		};

	} // end namespace detail


	template<typename ID>
	class BinaryRelation {

		using root_node_type = detail::node<ID,((sizeof(ID)*8)/4)-1>;

		root_node_type* root;

		std::size_t m_size = 0;

	public:

		using element_type = std::pair<ID,ID>;
		using const_iterator = detail::const_iterator<ID>;

		using source_filter_iterator = detail::source_filter_iterator<ID,const_iterator>;
		using target_filter_iterator = detail::target_filter_iterator<ID,const_iterator>;

		BinaryRelation() {
			root = new root_node_type();
		}

		BinaryRelation(const BinaryRelation&) = delete;

		BinaryRelation(BinaryRelation&& other) : root(other.root) {
			other.root = nullptr;
		}

		~BinaryRelation() {
			delete root;
		}

		BinaryRelation& operator=(const BinaryRelation&) = delete;

		BinaryRelation& operator=(BinaryRelation&& other) {
			std::swap(root, other.root);
			return *this;
		}

		bool empty() const {
			return size() == 0;
		}

		std::size_t size() const {
			return m_size;
		}

		bool insert(const element_type& element) {
			bool res = root->insert(element);
			if (res) ++m_size;
			return res;
		}

		bool contains(const element_type& element) const {
			return root->contains(element);
		}

		const_iterator begin() const {
			return const_iterator(root->getFirst());
		}

		const_iterator end() const {
			return const_iterator();
		}


		utils::range<source_filter_iterator> getSinks(const ID& src) const {
			auto low = begin();
			auto hig = end();
			return utils::range<source_filter_iterator> {
					source_filter_iterator(src,low,hig),
					source_filter_iterator(src,hig,hig)
			};
		}

		utils::range<target_filter_iterator> getSources(const ID& trg) const {
			auto low = begin();
			auto hig = end();
			return utils::range<target_filter_iterator> {
					target_filter_iterator(trg,low,hig),
					target_filter_iterator(trg,hig,hig)
			};
		}

	};

} // end namespace utils
} // end namespace allscale
