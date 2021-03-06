#pragma once

/**
 * Provides the utilities for modeling and handling work items.
 */

#include <string>
#include <vector>

#include "insieme/utils/assert.h"
#include "insieme/utils/comparable.h"

#include "insieme/core/ir.h"
#include "insieme/core/encoder/encoder.h"

#include "allscale/compiler/backend/allscale_extension.h"

namespace allscale {
namespace compiler {
namespace backend {

	/**
	 * The class representing data requirement functions of work item variants.
	 */
	class WorkItemVariantDataRequirement : public insieme::core::encoder::encodable, public insieme::utils::less_than_comparable<WorkItemVariantDataRequirement> {

		/**
		 * The lambda implementing the computation of this data requirements.
		 */
		insieme::core::LambdaExprPtr implementation;

	public:

		WorkItemVariantDataRequirement(const insieme::core::LambdaExprPtr& implementation = insieme::core::LambdaExprPtr())
			: implementation(implementation) {}

		const insieme::core::LambdaExprPtr& getImplementation() const {
			assert_true(valid());
			return implementation;
		}

		bool valid() const {
			return implementation;
		}

		bool operator==(const WorkItemVariantDataRequirement& other) const {
			if (this == &other) return true;
			if (!valid() && !other.valid()) return true;
			if (!valid() || !other.valid()) return false;
			assert_true(valid());
			assert_true(other.valid());
			return *implementation == *other.implementation;
		}

		bool operator<(const WorkItemVariantDataRequirement& other) const {
			// invalid requirements are the smallest element
			if (!valid()) return other.valid();
			if (!other.valid()) return false;
			return *implementation < *other.implementation;
		}

		// --- encoder interface ---

		static insieme::core::TypePtr getEncodedType(insieme::core::NodeManager&);

		static bool isEncoding(const insieme::core::ExpressionPtr&);

		insieme::core::ExpressionPtr toIR(insieme::core::NodeManager&) const;

		static WorkItemVariantDataRequirement fromIR(const insieme::core::ExpressionPtr&);

	};

	/**
	 * The class utilized for modeling a work item variant.
	 */
	class WorkItemVariant : public insieme::core::encoder::encodable, public insieme::utils::less_than_comparable<WorkItemVariant> {

		/**
		 * The lambda implementing this work item variant.
		 */
		insieme::core::LambdaExprPtr implementation;

		/**
		 * The data item requirement of this variant.
		 */
		WorkItemVariantDataRequirement dataRequirements;

		/**
		 * Closure element names for diagnoses.
		 */
		std::vector<std::string> closureElementNames;

	public:

		WorkItemVariant(const insieme::core::LambdaExprPtr& implementation, const WorkItemVariantDataRequirement& requirements = WorkItemVariantDataRequirement(),
		                const std::vector<std::string>& closureElementNames = {});

		insieme::core::LambdaExprPtr getImplementation() const {
			return implementation;
		}

		insieme::core::LambdaExprPtr setImplementation(const insieme::core::LambdaExprPtr& impl) {
			return implementation = impl;
		}

		insieme::core::TypePtr getResultType() const;

		insieme::core::TupleTypePtr getClosureType() const;

		const std::vector<std::string>& getClosureElementNames() const;

		const WorkItemVariantDataRequirement& getDataRequirements() const {
			return dataRequirements;
		}

		void setDataRequirements(const WorkItemVariantDataRequirement& requirements);

		bool operator==(const WorkItemVariant& other) const {
			return *implementation == *other.implementation && dataRequirements == other.dataRequirements;
		}

		bool operator<(const WorkItemVariant& other) const {
			return *implementation < *other.implementation || (*implementation == *other.implementation && dataRequirements < other.dataRequirements);
		}

		// --- encoder interface ---

		static insieme::core::TypePtr getEncodedType(insieme::core::NodeManager&);

		static bool isEncoding(const insieme::core::ExpressionPtr&);

		insieme::core::ExpressionPtr toIR(insieme::core::NodeManager&) const;

		static WorkItemVariant fromIR(const insieme::core::ExpressionPtr&);

	};


	/**
	 * The class utilized for modeling work item descriptions.
	 */
	class WorkItemDescription : public insieme::core::encoder::encodable, public insieme::utils::less_than_comparable<WorkItemDescription> {

		/**
		 * The name of this work item.
		 */
		std::string name;

		/**
		 * A function testing for the base case of a recursive operation.
		 */
		insieme::core::LambdaExprPtr splitableTest;

		/**
		 * A list of implementation variants.
		 */
		std::vector<WorkItemVariant> variants;

	public:

		WorkItemDescription(
				const std::string& name,
				const insieme::core::LambdaExprPtr& splitableTest,
				const WorkItemVariant& process,
				const WorkItemVariant& split,
				const std::vector<WorkItemVariant>& variants = std::vector<WorkItemVariant>()
			) : name(name), splitableTest(splitableTest), variants({process,split}) {
			assert_true(splitableTest);
			// The result types have to be the same. unused_type and unit are considered equivalent too
			assert_true(process.getResultType() == split.getResultType()
			            || (isUnusedType(process.getResultType()) && splitableTest.getNodeManager().getLangBasic().isUnit(split.getResultType())));
			assert_eq(process.getClosureType(),split.getClosureType());

			// insert variants (and check types)
			for(const auto& cur : variants) {
				addVariant(cur);
			}
		}

		WorkItemDescription(
				const std::string& name,
				const insieme::core::LambdaExprPtr& splitableTest,
				const std::vector<WorkItemVariant>& variants
			) : name(name), splitableTest(splitableTest), variants(variants) {
			assert_true(splitableTest);
			assert_le(2,variants.size());
			for(const auto& cur : variants) {
				// The result types have to be the same. unused_type and unit are considered equivalent too
				assert_true(getResultType() == cur.getResultType()
				            || (isUnusedType(cur.getResultType()) && splitableTest.getNodeManager().getLangBasic().isUnit(getResultType())));
				assert_eq(getClosureType(), cur.getClosureType());
			}
		}

		WorkItemDescription(
				const std::string& name,
				const insieme::core::LambdaExprPtr& splitableTest,
				const WorkItemVariant& implementation)
			: WorkItemDescription(name, splitableTest, implementation, implementation) {}

		const std::string& getName() const {
			return name;
		}

		insieme::core::TypePtr getResultType() const {
			return getSplitVariant().getResultType();
		}

		insieme::core::TupleTypePtr getClosureType() const {
			return getSplitVariant().getClosureType();
		}

		insieme::core::LambdaExprPtr getSplitableTest() const {
			return splitableTest;
		}

		const WorkItemVariant& getProcessVariant() const {
			return variants[0];
		}

		const WorkItemVariant& getSplitVariant() const {
			return variants[1];
		}

		std::vector<WorkItemVariant> getOptionalVariants() const {
			return std::vector<WorkItemVariant>(variants.begin()+2,variants.end());
		}

		const std::vector<WorkItemVariant>& getVariants() const {
			return variants;
		}

		std::vector<WorkItemVariant>& getVariants() {
			return variants;
		}

		void addVariant(const WorkItemVariant& variant) {
			assert_eq(getResultType(), variant.getResultType());
			variants.push_back(variant);
		}

		bool operator==(const WorkItemDescription& other) const;

		bool operator<(const WorkItemDescription& other) const;

		// --- encoder interface ---

		static insieme::core::TypePtr getEncodedType(insieme::core::NodeManager&);

		static bool isEncoding(const insieme::core::ExpressionPtr&);

		insieme::core::ExpressionPtr toIR(insieme::core::NodeManager&) const;

		static WorkItemDescription fromIR(const insieme::core::ExpressionPtr&);

	};


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
