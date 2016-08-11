#pragma once

#include "allscale/utils/graph.h"

#include <type_traits>

namespace allscale {
namespace utils {

	/**
	 * Features:
	 * 	- multiple node types
	 * 	- multiple relation types
	 * 		- relation kinds:
	 * 				N-M relation	- e.g. neighbors
	 * 				1-M relation	- e.g. parent / child
	 * 				1-1 relation (?) - e.g. left face
	 */


	namespace mesh {
	namespace reference {
		template<unsigned Layers, typename Nodes, typename Edges, typename Hierarchies>
		class Builder;
	}
	}

	template<typename A, typename B>
	struct edge {
		using src_node_type = A;
		using trg_node_type = B;
	};

	template<typename A, typename B>
	struct one_to_one_edge : public edge<A,B> {};

	template<typename A, typename B>
	struct one_to_many_edge : public edge<A,B> {};

	template<typename A, typename B>
	struct many_to_many_edge : public edge<A,B> {};

	template<typename A, typename B>
	struct hierarchy {
		using parent_node_type = A;
		using child_node_type = B;
	};

	template<unsigned Level>
	struct level {
		enum { value = Level };
	};

	template<typename ... Nodes>
	struct nodes {
		enum { size = sizeof...(Nodes) };
	};

	template<typename ... Edges>
	struct edges {
		enum { size = sizeof...(Edges) };
	};

	template<typename ... Hierarchies>
	struct hierarchies {
		enum { size = sizeof...(Hierarchies) };
	};

	template<typename A, typename B>
	struct merge;

	template<template<typename ...> class container, typename ... As, typename ... Bs>
	struct merge<container<As...>,container<Bs...>> {
		using type = container<As...,Bs...>;
	};

	template<
		typename Nodes,
		typename Edges,
		typename Hierarchies = hierarchies<>,
		unsigned layers = 1,
		template<unsigned Layers, typename Ns, typename Es, typename Hs> class implementation = mesh::reference::Builder
	>
	struct MeshBuilder {

		using Implementation = implementation<layers,Nodes,Edges,Hierarchies>;

		Implementation impl;

	public:

		static const unsigned Layers = layers;
		using nodes = Nodes;
		using edges = Edges;
		using hierarchies = Hierarchies;

		// -- type definitions --

		template<typename Kind, unsigned Level>
		using node_ref_type = typename Implementation::template node_ref_type<Kind,Level>;

		template<typename Kind, unsigned Level>
		using node_list_type = typename Implementation::template node_list_type<Kind,Level>;

		using mesh_type = typename Implementation::mesh_type;

		// -- mesh modeling --

		template<typename Kind,unsigned Level = 0>
		node_ref_type<Kind,Level> create() {
			return impl.template create<Kind,Level>();
		}

		template<typename Kind,unsigned Level = 0>
		node_list_type<Kind,Level> create(unsigned num) {
			return impl.template create<Kind,Level>(num);
		}

		template<typename EdgeKind, typename NodeKindA, typename NodeKindB, unsigned Level>
		void link(const node_ref_type<NodeKindA,Level>& a, const node_ref_type<NodeKindB,Level>& b) {
			static_assert(std::is_same<NodeKindA,typename EdgeKind::src_node_type>::value, "Invalid source node type");
			static_assert(std::is_same<NodeKindB,typename EdgeKind::trg_node_type>::value, "Invalid target node type");
			return impl.template link<EdgeKind>(a, b);
		}

		template<typename HierarchyKind, typename NodeKindA, typename NodeKindB, unsigned LevelA, unsigned LevelB>
		void link(const node_ref_type<NodeKindA,LevelA>& parent, const node_ref_type<NodeKindB,LevelB>& child) {
			static_assert(LevelA == LevelB+1, "Can not connect nodes of unrelated layers in hierarchies");
			static_assert(std::is_same<NodeKindA,typename HierarchyKind::parent_node_type>::value, "Invalid source node type");
			static_assert(std::is_same<NodeKindB,typename HierarchyKind::child_node_type>::value, "Invalid target node type");
			return impl.template link<HierarchyKind>(parent, child);
		}

		// -- build mesh --

		mesh_type build() {
			return impl.build();
		}
	};

	// --- utils ---


	template<typename T>
	struct get_level;

	template<unsigned L>
	struct get_level<level<L>> {
		enum { value = L };
	};

	template<typename T>
	struct get_level<T&> : public get_level<T> {};
	template<typename T>
	struct get_level<const T> : public get_level<T> {};
	template<typename T>
	struct get_level<volatile T> : public get_level<T> {};

	template<typename T>
	using plain_type = std::remove_cv_t<std::remove_reference_t<T>>;


} // end namespace utils
} // end namespace allscale

#include "allscale/utils/mesh/reference_implementation.h"
