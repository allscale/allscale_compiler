#include "allscale/compiler/backend/allscale_type_handler.h"

#include "insieme/backend/c_ast/c_ast_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"

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
			auto def = fragmentManager->create<backend::c_ast::CCodeFragment>(mgr,init);
			def->addDependency(elementTypeInfo.definition);

			// create resulting code fragment
			return type_info_utils::createInfo(namedType, def);
		}

		const TypeInfo* handleType(ConversionContext& context, const TypePtr& type) {

			// intercept tuple types (use hpx types instead)
			if (auto tupleType = type.isa<TupleTypePtr>()) {
				return handleTupleType(context,tupleType);
			}

			// intercept the treeture type
			if (lang::TreetureType::isTreetureType(type)) {
				return handleTreetureType(context,lang::TreetureType(type));
			}

			// it is not a special runtime type => let somebody else try
			return 0;
		}
	}

	TypeHandler AllScaleTypeHandler = &handleType;

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
