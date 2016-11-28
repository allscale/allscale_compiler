#include "allscale/compiler/backend/allscale_code_fragments.h"

#include <map>
#include <string>

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/core/transform/node_replacer.h"

namespace allscale {
namespace compiler {
namespace backend {

	namespace core = insieme::core;
	namespace backend = insieme::backend;


	namespace detail {

		class WorkItemDescriptionsImpl {

			backend::c_ast::SharedCodeFragmentManager fragmentManager;

			std::map<WorkItemDescription,WorkItemDescriptionInfo> index;

			std::set<std::string> usedNames;

		public:

			WorkItemDescriptionsImpl(const backend::Converter& converter)
				: fragmentManager(converter.getFragmentManager()) {}

			WorkItemDescriptionInfo& getDescriptionType(backend::ConversionContext& context, const WorkItemDescription& desc) {

				// check the index
				auto pos = index.find(desc);
				if (pos != index.end()) return pos->second;

				// if not yet resolved, do it now
				resolve(context, desc);
				return getDescriptionType(context, desc);
			}

			const WorkItemDescriptionInfo& lookupDescriptionType(const std::string& name) const {
				// search the corresponding work item
				for(const auto& cur : index) {
					if (cur.first.getName() == name) {
						return cur.second;
					}
				}
				// this should not happen
				assert_fail() << "Requesting unresolved Work-Item name: " << name << "\n";
				return index.begin()->second;
			}

		private:

			// a utility for returning results of generator functions within this class
			struct TypeInfo {
				backend::c_ast::TypePtr type;
				backend::c_ast::CodeFragmentPtr declaration;
				backend::c_ast::CodeFragmentPtr definition;
			};

			struct WorkItemDescInfo {
				backend::c_ast::TypePtr type;
				backend::c_ast::NamedTypePtr defining_type;
				backend::c_ast::CodeFragmentPtr definition;
			};

			void resolve(backend::ConversionContext& context, const WorkItemDescription& desc) {

				// check that it is not yet resolved
				assert_true(index.find(desc) == index.end());

				// 1) get a name
				auto name = getName(desc);

				// 2) create the struct containing the function returning the name
				auto nameFactory = createNameFactory(name);

				// 3) create the work item description code
				auto descInfo = createWorkItemDescription(context,desc,name,nameFactory);

				// 4) register info without variants before resolving variants for supporting recursive references
				index[desc] = WorkItemDescriptionInfo{
					descInfo.definition,
					descInfo.type
				};

				// 5) convert variants
				std::vector<TypeInfo> variants;
				int i = 0;
				variants.push_back(createVariantImplementation(context,desc.getProcessVariant(),name,i++));
				variants.push_back(createVariantImplementation(context,desc.getSplitVariant(),name,i++));
				for(const auto& cur : desc.getOptionalVariants()) {
					variants.push_back(createVariantImplementation(context,cur,name,i++));
				}

				// 6) add variants to the description info
				addWorkItemVariants(descInfo, variants);
			}

			std::string getName(const WorkItemDescription& desc) {

				auto res = "__wi_" + desc.getName();

				// register name and check whether it is already in use
				if (usedNames.insert(res).second) {
					return res; 	// if not, we have a new name
				}

				// otherwise, start finding a free name
				int i = 0;
				do {
					i++;
					res = format("%s_%d", desc.getName(),i);
				} while (!usedNames.insert(res).second);

				// done
				return res;
			}

			TypeInfo createNameFactory(const std::string& name) {

				// get the C-Node Manager
				auto& mgr = getCNodeManager();

				// build the struct
				auto strct = mgr->create<backend::c_ast::StructType>(
						mgr->create<backend::c_ast::Identifier>(name + "_name")
				);

				// add the static member function returning the name
				strct->others.push_back(
						mgr->create<backend::c_ast::OpaqueCode>("static const char* name() { return \"" + name + "\"; }")
				);

				// get the struct definition
				auto strctDef = mgr->create<backend::c_ast::TypeDefinition>(strct);

				// create the code fragment containing the struct
				auto fragment = fragmentManager->create<backend::c_ast::CCodeFragment>(getCNodeManager(), strctDef);

				return TypeInfo{ strct, fragment, fragment };

			}

			TypeInfo createVariantImplementation(backend::ConversionContext& context, const WorkItemVariant& variant, const std::string& name, int i) {

				// get the C-Node Manager
				auto& mgr = getCNodeManager();

				auto opaque = [&](const std::string& code) {
					return mgr->create<backend::c_ast::OpaqueCode>(code);
				};

				// get the struct name
				auto structName = mgr->create<backend::c_ast::Identifier>(format("%s_variant_%d",name,i));

				// build the struct
				auto strct = mgr->create<backend::c_ast::StructType>(structName);

				// add valid constant
				strct->others.push_back(
						opaque("static constexpr bool valid = true;")
				);

				// create a function forwarding the call to the implementation in IR (execute function)
				auto impl = variant.getImplementation();

				// resolve the implementation function
				auto& implInfo = context.getConverter().getFunctionManager().getInfo(context, impl);

				// build wrapper function
				auto wrapper = mgr->create<backend::c_ast::Function>();
				wrapper->returnType = implInfo.function->returnType;
				wrapper->name = mgr->create("execute");
				wrapper->parameter = implInfo.function->parameter;
				wrapper->body = backend::c_ast::ret(backend::c_ast::call(
						implInfo.function->name,
						wrapper->parameter[0]
				));

				// wrap up wrapper as a static member function
				auto memberFun = mgr->create<backend::c_ast::MemberFunction>(structName, wrapper);
				memberFun->isStatic = true;

				// create code fragment for definition of member function
				auto wrapperFragment = fragmentManager->create<backend::c_ast::CCodeFragment>(
						getCNodeManager(),
						memberFun
				);
				wrapperFragment->addDependency(implInfo.prototype);

				// add wrapper to resulting struct
				strct->members.push_back(mgr->create<backend::c_ast::MemberFunctionPrototype>(memberFun));


				// get the struct declaration and definition
				auto strctDec = mgr->create<backend::c_ast::TypeDeclaration>(strct);
				auto strctDef = mgr->create<backend::c_ast::TypeDefinition>(strct);

				// create the code fragment containing the declarations and definitions
				auto dec = fragmentManager->create<backend::c_ast::CCodeFragment>(getCNodeManager(), strctDec);
				auto def = fragmentManager->create<backend::c_ast::CCodeFragment>(getCNodeManager(), strctDef);

				// connect the declaration and definition
				dec->addRequirement(def);
				def->addDependency(dec);

				def->addDependency(implInfo.prototype);
				def->addRequirement(wrapperFragment);

				// return result
				return TypeInfo{ strct, dec, def };
			}

			WorkItemDescInfo createWorkItemDescription(backend::ConversionContext& context, const WorkItemDescription& desc, const std::string& name, const TypeInfo& nameFactory) {

				// get the C-Node Manager
				auto& mgr = getCNodeManager();

				auto typeName = mgr->create(format("%s_work",name));

				// get the type to be defined
				auto namedType = mgr->create<backend::c_ast::NamedType>(typeName);

				// get the defining type
				auto definition = mgr->create<backend::c_ast::NamedType>(
						mgr->create("allscale::work_item_description")
				);

				// get result type information
				auto resTypeInfo = context.getConverter().getTypeManager().getTypeInfo(context, desc.getResultType());

				// add list of template parameters
				definition->parameters.push_back(resTypeInfo.rValueType);
				definition->parameters.push_back(nameFactory.type);

				// create the defining type alias
				auto alias = backend::c_ast::alias(namedType,definition);

				// create the code fragment containing the alias definition
				auto fragment = fragmentManager->create<backend::c_ast::CCodeFragment>(getCNodeManager(), alias);

				// add dependencies to parameter types
				fragment->addDependency(nameFactory.declaration);
				fragment->addRequirement(nameFactory.definition);

				return WorkItemDescInfo{ namedType, definition, fragment };
			}

			void addWorkItemVariants(const WorkItemDescInfo& info, const std::vector<TypeInfo>& variants) {

				// add work item variants as parameters to the definition
				for(const auto& cur : variants) {
					info.defining_type->parameters.push_back(cur.type);
				}

				// add dependencies to parameter types
				for(const auto& cur : variants) {
					info.definition->addDependency(cur.declaration);
					info.definition->addRequirement(cur.definition);
				}

			}

			const backend::c_ast::SharedCNodeManager& getCNodeManager() {
				return fragmentManager->getNodeManager();
			}

		};

	}

	WorkItemDescriptions::WorkItemDescriptions(const backend::Converter& converter)
		: impl(std::make_unique<detail::WorkItemDescriptionsImpl>(converter)) {}

	const WorkItemDescriptionInfo& WorkItemDescriptions::getDescriptionType(backend::ConversionContext& context, const WorkItemDescription& desc) {
		return impl->getDescriptionType(context,desc);
	}

	const WorkItemDescriptionInfo& WorkItemDescriptions::getDescriptionType(backend::ConversionContext& context, const core::ExpressionPtr& desc) {
		return getDescriptionType(context,WorkItemDescription::fromIR(desc));
	}

	const WorkItemDescriptionInfo& WorkItemDescriptions::getDescriptionType(insieme::backend::ConversionContext& context, const std::string& name) {
		return impl->lookupDescriptionType(name);
	}

	std::ostream& WorkItemDescriptions::printTo(std::ostream& out) const {
		return out;	// nothing to write for this fragment, content is in dependencies
	}

	WorkItemDescriptions& WorkItemDescriptions::getInstance(const backend::Converter& converter) {
		const static std::string KEY = "WorkItemDescriptionsFragment";

		auto& fragmentMgr = *converter.getFragmentManager();

		// look up the active instance
		auto res = fragmentMgr.getFragment(KEY);
		if (res) return static_cast<WorkItemDescriptions&>(*res);

		// there is none, create a new one
		auto instance = fragmentMgr.create<WorkItemDescriptions>(converter);
		fragmentMgr.bindFragment(KEY,instance);

		// return reference to new instance
		return *instance;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
