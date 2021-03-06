#include "allscale/compiler/backend/allscale_runtime_entities.h"

#include <map>
#include <string>
#include <vector>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/encoder/tuples.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/lang/reference.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/backend/allscale_extension.h"


namespace allscale {
namespace compiler {
namespace backend {

	namespace core = insieme::core;
	namespace ie = insieme::core::encoder;


	// -- WorkItemVariantDataRequirement --

	using work_item_variant_data_requirement_tuple = std::tuple<core::LambdaExprPtr>;

	insieme::core::TypePtr WorkItemVariantDataRequirement::getEncodedType(insieme::core::NodeManager& mgr) {
		return ie::getTypeFor<work_item_variant_data_requirement_tuple>(mgr);
	}

	bool WorkItemVariantDataRequirement::isEncoding(const insieme::core::ExpressionPtr& e) {
		return ie::isEncodingOf<work_item_variant_data_requirement_tuple>(e);
	}

	insieme::core::ExpressionPtr WorkItemVariantDataRequirement::toIR(insieme::core::NodeManager& mgr) const {
		return ie::toIR(mgr, work_item_variant_data_requirement_tuple { implementation });
	}

	WorkItemVariantDataRequirement WorkItemVariantDataRequirement::fromIR(const insieme::core::ExpressionPtr& e) {
		auto tuple = ie::toValue<work_item_variant_data_requirement_tuple>(e);
		return WorkItemVariantDataRequirement(std::get<0>(tuple));
	}


	// -- WorkItemVariant --


	WorkItemVariant::WorkItemVariant(const core::LambdaExprPtr& implementation, const WorkItemVariantDataRequirement& requirements,
		                             const std::vector<std::string>& closureElementNames)
		: implementation(implementation), dataRequirements(requirements), closureElementNames(closureElementNames) {

		// check that the implementation is actually present
		assert_true(implementation) << "Implementation must not be null!";

		// make sure there is only a single parameter
		assert_eq(1,implementation->getFunctionType()->getParameterTypes().size())
			<< "Implementation can only have a single parameter, which must be a tuple reference.";

		// make sure this parameter is a C++ reference
		assert_pred1(core::lang::isCppReference, implementation->getFunctionType()->getParameterType(0))
			<< "Implementation parameter must be a tuple reference.";

		// make sure the parameter is a const-reference
		assert_true(core::lang::ReferenceType(implementation->getFunctionType()->getParameterType(0)).isConst())
			<< "Implementation parameter must be a const C++ reference."
			<< "Actual type: " << *implementation->getFunctionType()->getParameterType(0);

		// make sure the parameter is a tuple
		assert_true(core::lang::ReferenceType(implementation->getFunctionType()->getParameterType(0)).getElementType().isa<core::TupleTypePtr>())
				<< "Implementation parameter must be a tuple reference.\n"
				<< "Actual type: " << *implementation->getFunctionType()->getParameterType(0);

		// make sure the result type is a treeture
		assert_pred1(lang::isTreeture,implementation->getFunctionType()->getReturnType());

		// make sure the data requirements accept the same type of closure
		if (dataRequirements.valid()) {
			assert_eq(implementation->getFunctionType()->getParameterType(0),requirements.getImplementation()->getFunctionType()->getParameterType(0));
		}

	}

	core::TypePtr WorkItemVariant::getResultType() const {
		return lang::TreetureType(implementation->getFunctionType()->getReturnType()).getValueType();
	}

	core::TupleTypePtr WorkItemVariant::getClosureType() const {
		return core::lang::ReferenceType(implementation->getFunctionType()->getParameterType(0)).getElementType().as<core::TupleTypePtr>();
	}

	const std::vector<std::string>& WorkItemVariant::getClosureElementNames() const {
		return closureElementNames;
	}

	void WorkItemVariant::setDataRequirements(const WorkItemVariantDataRequirement& requirements) {
		dataRequirements = requirements;
		if (dataRequirements.valid()) {
			assert_eq(implementation->getFunctionType()->getParameterType(0),dataRequirements.getImplementation()->getFunctionType()->getParameterType(0));
		}
	}

	using work_item_variant_tuple = std::tuple<core::LambdaExprPtr, WorkItemVariantDataRequirement, std::vector<std::string>>;

	core::TypePtr WorkItemVariant::getEncodedType(core::NodeManager& mgr) {
		return ie::getTypeFor<work_item_variant_tuple>(mgr);
	}

	bool WorkItemVariant::isEncoding(const core::ExpressionPtr& e) {
		return ie::isEncodingOf<work_item_variant_tuple>(e);
	}

	core::ExpressionPtr WorkItemVariant::toIR(core::NodeManager& mgr) const {
		// convert this work item into a tuple
		return ie::toIR(mgr, work_item_variant_tuple{ implementation, dataRequirements, closureElementNames });
	}

	WorkItemVariant WorkItemVariant::fromIR(const core::ExpressionPtr& e) {
		auto tuple = ie::toValue<work_item_variant_tuple>(e);
		return WorkItemVariant(std::get<0>(tuple), std::get<1>(tuple), std::get<2>(tuple));
	}


	// -- WorkItemDescription --


	bool WorkItemDescription::operator==(const WorkItemDescription& other) const {
		return name == other.name && *splitableTest == *other.splitableTest && variants == other.variants;
	}

	bool WorkItemDescription::operator<(const WorkItemDescription& other) const {

		// compare one field after the other
		if (name != other.name) return name < other.name;
		if (*splitableTest != *other.splitableTest) return *splitableTest < *other.splitableTest;
		return lexicographical_compare(variants, other.variants);
	}

	using work_item_description_tuple = std::tuple<
			std::string,
			core::LambdaExprPtr,
			std::vector<WorkItemVariant>
	>;

	core::TypePtr WorkItemDescription::getEncodedType(core::NodeManager& mgr) {
		assert_fail() << "Type is generic, thus can not be fixed to a single type.";
		return core::TypePtr();
	}

	bool WorkItemDescription::isEncoding(const core::ExpressionPtr& e) {
		core::NodeManager& mgr = e.getNodeManager();
		const auto& asbm = mgr.getLangExtension<AllScaleBackendModule>();

		// check that it is the proper type
		return core::analysis::isCallOf(e, asbm.getCreateWorkItemDescription())
				&& ie::isEncodingOf<work_item_description_tuple>(e.as<core::CallExprPtr>()->getArgument(0));
	}

	core::ExpressionPtr WorkItemDescription::toIR(core::NodeManager& mgr) const {

		// encode the information stored within this description into a tuple
		auto info = ie::toIR(mgr, work_item_description_tuple{
			name, splitableTest, variants
		});

		// wrap this information into a work item description operator
		const AllScaleBackendModule& asbm = mgr.getLangExtension<AllScaleBackendModule>();
		core::IRBuilder builder(mgr);

		return builder.callExpr(
				asbm.getCreateWorkItemDescription(),
				info,
				builder.getTypeLiteral(getClosureType()),
				builder.getTypeLiteral(getResultType())
		);
	}

	WorkItemDescription WorkItemDescription::fromIR(const core::ExpressionPtr& e) {
		// make sure it is a proper encoding
		assert_pred1(isEncoding, e);

		// unpack the work item description
		auto tuple = ie::toValue<work_item_description_tuple>(e.as<core::CallExprPtr>()->getArgument(0));
		return WorkItemDescription(
			std::get<0>(tuple),
			std::get<1>(tuple),
			std::get<2>(tuple)
		);
	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
