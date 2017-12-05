#include "allscale/compiler/backend/allscale_type_handler.h"

#include <sstream>

#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/core/data_serialization.h"
#include "allscale/compiler/backend/allscale_extension.h"

namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme;
	using namespace insieme::core;
	using namespace insieme::backend;

	namespace {

		const TypeInfo* handleTupleType(ConversionContext& context, const TupleTypePtr& tuple) {
			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();
			auto& fragmentManager = converter.getFragmentManager();
			auto& typeManager = converter.getTypeManager();

			// ignore pointer types (they are also tuples, but handled differently)
			if (insieme::core::lang::isPointer(tuple)) return nullptr;

			// also ignore enumeration types
			if (insieme::core::lang::isEnum(tuple)) return nullptr;

			// create tuple type name
			auto type = mgr->create<c_ast::NamedType>(mgr->create("hpx::util::tuple"));
			type->isGenericType = true;

			// create defining fragment (empty, just collecting dependencies)
			auto def = fragmentManager->create<c_ast::DummyFragment>();
			auto dec = fragmentManager->create<c_ast::DummyFragment>();

			// add type parameters and corresponding dependencies
			for(const auto& cur : tuple) {

				// resolve this type
				const TypeInfo& info = typeManager.getTypeInfo(context,cur);

				type->parameters.push_back(info.rValueType);
				def->addDependency(info.declaration);
				dec->addDependency(info.definition);
			}

			// aggregate results
			return type_info_utils::createInfo(type,def,dec);
		}

		const TypeInfo* handleTreetureType(ConversionContext& context, const lang::TreetureType& type) {
			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();
			auto& fragmentManager = converter.getFragmentManager();

			// only resolve the released type (and map all to this one)
			if (!type.isReleased()) {
				auto released = lang::TreetureType(type.getValueType(), true).toIRType();
				return &context.getConverter().getTypeManager().getTypeInfo(context, released);
			}

			// intercept unused_type
			if(isUnusedType(type.getValueType())) {
				// this type should be mapped to
				//		allscale::unused_type

				// convert the type
				auto namedType = mgr->create<c_ast::NamedType>(mgr->create("allscale::runtime::unused_type"));
				// create resulting code fragment
				return type_info_utils::createInfo(namedType);
			}

			// resolve
			auto elementTypeInfo = converter.getTypeManager().getTypeInfo(context, type.getValueType());

			// convert the type
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create("allscale::treeture"));
			namedType->parameters.push_back(elementTypeInfo.rValueType);

			// create the instantiation of the treeture type
			auto init = c_ast::call(
					mgr->create("ALLSCALE_REGISTER_TREETURE_TYPE"),
					elementTypeInfo.rValueType
			);

			// wrap up definition into code fragment
			backend::c_ast::CodeFragmentPtr def = fragmentManager->create<backend::c_ast::CCodeFragment>(mgr,init);
			def->addDependency(elementTypeInfo.definition);

			// however, do not register unit treeture types
			if (type.getValueType()->getNodeManager().getLangBasic().isUnit(type.getValueType())) {
				def = fragmentManager->create<backend::c_ast::DummyFragment>();
			}

			// create resulting code fragment
			return type_info_utils::createInfo(namedType, def);
		}

		const TypeInfo* handleTaskReferenceType(ConversionContext& context, const TypePtr& type) {
			assert_pred1(lang::isTaskReference,type);

			auto& converter = context.getConverter();
			auto& fragmentManager = converter.getFragmentManager();

			// map this to the unit treeture
			auto unitTreeture = lang::TreetureType(type.getNodeManager().getLangBasic().getUnit(),true).toIRType();

			// resolve the unit treeture
			const auto& unitTreetureInfo = context.getConverter().getTypeManager().getTypeInfo(context,unitTreeture);

			// create a dummy definition
			auto def = fragmentManager->create<backend::c_ast::DummyFragment>();
			def->addDependency(unitTreetureInfo.definition);

			// resolve the task reference as the unit treeture
			return type_info_utils::createInfo(unitTreetureInfo.lValueType, def);
		}

		const TypeInfo* handlePrecFunType(ConversionContext& context, const lang::PrecFunType& type) {
			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();

			// this type should be mapped to
			//		allscale::runtime::prec_operation<A,B,C>
			// but C is context dependent; fortunately, we can always use auto!
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create("auto"));
			return type_info_utils::createInfo(namedType);
		}

		const TypeInfo* handleDependenciesType(ConversionContext& context, const insieme::core::TypePtr&) {
			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();

			// convert the type
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create("allscale::runtime::dependencies"));

			// return new type information
			return type_info_utils::createInfo(namedType);
		}

		const TypeInfo* handleDataItemReferenceType(ConversionContext& context, const insieme::core::TypePtr& type) {
			assert_pred1(isDataItemReference, type);

			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();

			// this type should be mapped to
			//		allscale::runtime::DataItemReference<A>

			// resolve
			auto dataItemTypeInfo = converter.getTypeManager().getTypeInfo(context, getReferencedDataItemType(type));

			// convert the type
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create("allscale::runtime::DataItemReference"));
			namedType->parameters.push_back(dataItemTypeInfo.rValueType);

			auto& fragmentManager = converter.getFragmentManager();

			std::stringstream ss;
			static unsigned dataItemReferenceCounter = 0;
			std::string dataItemName = format("data_item_type_%d", ++dataItemReferenceCounter);
			ss << "using " << dataItemName << " = " << *dataItemTypeInfo.rValueType << ";\n"
					<< "REGISTER_DATAITEMSERVER_DECLARATION(" << dataItemName << ")\n"
					<< "REGISTER_DATAITEMSERVER(" << dataItemName << ")";
			auto macro = mgr->create<backend::c_ast::OpaqueCode>(ss.str());
			auto decl = fragmentManager->create<backend::c_ast::CCodeFragment>(mgr, macro);
			decl->addDependency(dataItemTypeInfo.declaration);

			// create resulting code fragment
			return type_info_utils::createInfo(namedType, decl);
		}

		const TypeInfo* handleDataItemRequirementType(ConversionContext& context, const insieme::core::TypePtr& type) {
			assert_pred1(isDataItemRequirement, type);

			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();

			// this type should be mapped to
			//		allscale::runtime::DataItemRequirement<A>

			// resolve
			auto dataItemTypeInfo = converter.getTypeManager().getTypeInfo(context, getRequiredDataItemType(type));

			// convert the type
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create("allscale::runtime::DataItemRequirement"));
			namedType->parameters.push_back(dataItemTypeInfo.rValueType);

			// create resulting code fragment
			return type_info_utils::createInfo(namedType, dataItemTypeInfo.declaration);
		}

		const TypeInfo* handleArchiveUtility(ConversionContext& context, const std::string& typeName) {

			auto& converter = context.getConverter();
			auto& mgr = converter.getCNodeManager();

			// this type should be mapped to
			//		allscale::utils::ArchiveReader

			// create a dummy fragment
			auto decl = context.getConverter().getFragmentManager()->create<insieme::backend::c_ast::IncludeFragment>("allscale/utils/serializer.h");

			// convert the type
			auto namedType = mgr->create<c_ast::NamedType>(mgr->create(typeName));

			// create resulting code fragment
			return type_info_utils::createInfo(namedType, decl);
		}

		const TypeInfo* handleArchiveReader(ConversionContext& context, const insieme::core::TypePtr& type) {
			return handleArchiveUtility(context,"allscale::utils::ArchiveReader");
		}

		const TypeInfo* handleArchiveWriter(ConversionContext& context, const insieme::core::TypePtr& type) {
			return handleArchiveUtility(context,"allscale::utils::ArchiveWriter");
		}

		const TypeInfo* handleType(ConversionContext& context, const TypePtr& type) {

			// intercept tuple types (use hpx types instead)
			if (auto tupleType = type.isa<TupleTypePtr>()) {
				return handleTupleType(context,tupleType);
			}

			// intercept the treeture type
			if (lang::isTreeture(type)) {
				return handleTreetureType(context,lang::TreetureType(type));
			}

			// intercept the task_ref type
			if (lang::isTaskReference(type)) {
				return handleTaskReferenceType(context,type);
			}

			// intercept the dependencies type
			if (lang::isDependencies(type)) {
				return handleDependenciesType(context,type);
			}

			// intercept the prec operator type
			if (lang::isPrecFun(type)) {
				return handlePrecFunType(context,lang::PrecFunType(type));
			}

			// intercept the data item reference type
			if (isDataItemReference(type)) {
				return handleDataItemReferenceType(context,type);
			}

			// intercept the data item requirement type
			if (isDataItemRequirement(type)) {
				return handleDataItemRequirementType(context,type);
			}

			// intercept archive reader
			auto& ext = type->getNodeManager().getLangExtension<core::SerializationModule>();
			if (ext.isArchiveReader(type)) {
				return handleArchiveReader(context,type);
			}
			if (ext.isArchiveWriter(type)) {
				return handleArchiveWriter(context,type);
			}

			// it is not a special runtime type => let somebody else try
			return nullptr;
		}
	}

	TypeHandler AllScaleTypeHandler = &handleType;

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
