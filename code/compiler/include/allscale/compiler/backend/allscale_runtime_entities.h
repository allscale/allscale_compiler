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

namespace allscale {
namespace compiler {
namespace backend {


	/**
	 * The class utilized for modeling a work item variant.
	 */
	class WorkItemVariant : public insieme::core::encoder::encodable, public insieme::utils::less_than_comparable<WorkItemVariant> {

		/**
		 * The lambda implementing this work item variant.
		 */
		insieme::core::LambdaExprPtr implementation;

	public:

		WorkItemVariant(const insieme::core::LambdaExprPtr& implementation);

		insieme::core::LambdaExprPtr getImplementation() const {
			return implementation;
		}

		insieme::core::TypePtr getResultType() const;

		insieme::core::TupleTypePtr getClosureType() const;


		bool operator==(const WorkItemVariant& other) const {
			return *implementation == *other.implementation;
		}

		bool operator<(const WorkItemVariant& other) const {
			return *implementation < *other.implementation;
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
		 * A list of implementation variants.
		 */
		std::vector<WorkItemVariant> variants;

	public:

		WorkItemDescription(
				const std::string& name,
				const WorkItemVariant& process,
				const WorkItemVariant& split,
				const std::vector<WorkItemVariant>& variants = std::vector<WorkItemVariant>()
			) : name(name), variants({process,split}) {
			assert_eq(process.getResultType(),split.getResultType());
			assert_eq(process.getClosureType(),split.getClosureType());

			// insert variants (and check types)
			for(const auto& cur : variants) {
				addVariant(cur);
			}
		}

		WorkItemDescription(
				const std::string& name,
				const std::vector<WorkItemVariant>& variants
			) : name(name), variants(variants) {
			assert_le(2,variants.size());
			for(const auto& cur : variants) {
				assert_eq(getResultType(), cur.getResultType());
				assert_eq(getClosureType(), cur.getClosureType());
			}
		}

		WorkItemDescription(const std::string& name, const WorkItemVariant& implementation)
			: WorkItemDescription(name, implementation, implementation) {}

		const std::string& getName() const {
			return name;
		}

		insieme::core::TypePtr getResultType() const {
			return getProcessVariant().getResultType();
		}

		insieme::core::TupleTypePtr getClosureType() const {
			return getProcessVariant().getClosureType();
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
