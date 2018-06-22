
#include "allscale/compiler/core/data_serialization.h"

#include "insieme/utils/name_mangling.h"

#include "insieme/annotations/c/include.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/cpp_std.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/type_utils.h"

#include "allscale/compiler/allscale_utils.h"
#include "allscale/compiler/backend/allscale_extension.h"

// debugging:
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/analysis/ir_utils.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	const std::string SerializationModule::READER_NAME = insieme::utils::mangle("allscale::utils::ArchiveReader");
	const std::string SerializationModule::WRITER_NAME = insieme::utils::mangle("allscale::utils::ArchiveWriter");

	namespace {

		static const std::string FUN_NAME_LOAD  = insieme::utils::mangle("load");
		static const std::string FUN_NAME_STORE = insieme::utils::mangle("store");

		bool hasLoadFunction(const TagTypeBindingPtr& binding) {
			if (!binding) return false;

			// get some utilities
			auto& mgr = binding->getNodeManager();
			auto& ext = mgr.getLangExtension<SerializationModule>();

			// look for load function in static members
			for(const StaticMemberFunctionPtr& cur : binding->getRecord()->getStaticMemberFunctions()) {

				// check the name
				if (cur->getNameAsString() != FUN_NAME_LOAD) continue;

				// check the function type
				auto funType = cur->getImplementation()->getType().as<FunctionTypePtr>();

				// check function type
				auto params = funType->getParameterTypes();
				if (params->size() != 1) continue;

				// check parameter
				if (!core::lang::isReference(params[0])) continue;
				auto readerType = core::lang::ReferenceType(params[0]);
				if (readerType.isConst()) continue;
				if (readerType.isVolatile()) continue;
				if (!readerType.isCppReference()) continue;
				if (!ext.isArchiveReader(readerType.getElementType())) continue;

				// check return type
				if (*funType->getReturnType() != *binding->getTag()) continue;

				// this is a valid load function
				return true;
			}

			// no load function found
			return false;
		}

		bool hasLoadFunction(const TagTypePtr& tagType) {
			return hasLoadFunction(tagType->getDefinition()->getBindingOf(tagType->getTag()));
		}

		bool hasStoreFunction(const TagTypeBindingPtr& binding) {
			auto& mgr = binding->getNodeManager();
			auto& basic = mgr.getLangBasic();
			auto& ext = mgr.getLangExtension<SerializationModule>();

			// look through set of member functions
			for(const MemberFunctionPtr& cur : binding->getRecord()->getMemberFunctions()) {

				// check the name
				if (cur->getNameAsString() != FUN_NAME_STORE) continue;

				// check the function type
				auto funType = cur->getImplementation()->getType().as<FunctionTypePtr>();

				// check function type
				auto params = funType->getParameterTypes();
				if (params->size() != 2) continue;

				// check first parameter
				if (!core::lang::isReference(params[0])) continue;
				if (!core::lang::ReferenceType(params[0]).isConst()) continue;

				// check second parameter
				if (!core::lang::isReference(params[1])) continue;
				auto writerType = core::lang::ReferenceType(params[1]);
				if (writerType.isConst()) continue;
				if (writerType.isVolatile()) continue;
				if (!writerType.isCppReference()) continue;
				if (!ext.isArchiveWriter(writerType.getElementType())) continue;

				// check return type
				if (!basic.isUnit(funType->getReturnType())) continue;

				// this is a valid store function
				return true;
			}

			// nothing found
			return false;
		}

		bool hasStoreFunction(const TagTypePtr& tagType) {
			return hasStoreFunction(tagType->getDefinition()->getBindingOf(tagType->getTag()));
		}

		std::pair<StaticMemberFunctionPtr, LambdaExprPtr> tryBuildLoadFunction(const TagTypeBindingPtr& binding) {
			auto& mgr = binding->getNodeManager();
			auto& ext = mgr.getLangExtension<SerializationModule>();
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
			IRBuilder builder(mgr);

			// check that the given record is a struct
			auto record = binding->getRecord().isa<StructPtr>();
			if (!record) return {};

			// block out derived classes for now
			if (!record->getParents().empty()) {
				// TODO: add support for parents
				return {};
			}

			// create a reader instance
			auto reader = builder.variable(
				core::lang::ReferenceType::create(ext.getArchiveReader(),false,false,core::lang::ReferenceType::Kind::CppReference),
				0
			);

			// build up body
			std::vector<StatementPtr> stmts;
			std::vector<DeclarationPtr> valueDecls;
			for(const auto& field : record->getFields()) {

				// get the element type
				auto elementType = field->getType();

				// TODO: export this in a general utility or replace by general utility

				// test whether this operation requires a materialization
				bool needsMaterialization = elementType.isa<TagTypePtr>();

				// create a read call
				auto read = builder.callExpr(
					(needsMaterialization) ? builder.refType(elementType) : elementType,
					ext.getRead(),
					core::lang::toPlainReference(reader),
					builder.getTypeLiteral(elementType)
				);

				// create a variable declaration
				auto decl = builder.declarationStmt(
						builder.variable(builder.refType(elementType),stmts.size()),
						(needsMaterialization) ? core::lang::buildRefKindCast(read, core::lang::ReferenceType::Kind::CppRValueReference) : read
				);

				// add to body statements
				stmts.push_back(decl);

				// record new declaration for init
				auto call = builder.callExpr(refExt.getRefMoveReference(), core::lang::buildRefKindCast(decl->getVariable(), core::lang::ReferenceType::Kind::CppReference));
				valueDecls.push_back(utils::buildPassByValueDeclaration(call));
			}

			// the return (and this) type
			auto retType = binding->getTag();

			// a dummy parameter we will add to a custom generated ctor if we do create one, in order to disambiguate it from other ctors
			const auto dummyCtorArgumentType = builder.structType("DummyCtorParamType", {});

			// build constructor
			core::LambdaExprPtr ctor;
			// only if we do have fields
			if(!record->getFields()->empty()) {
				// and if we have any other non-defaulted constructor already
				if(::any(record->getConstructors()->getElements(), [](const auto& ctor){ return !core::analysis::isaDefaultConstructor(ctor); })) {
					bool failed = false;
					unsigned variableId = 0;
					auto defaultCtorType = core::analysis::buildDefaultDefaultConstructorType(builder.refType(retType));
					core::TypeList ctorParamTypes;
					core::VariableList ctorVars;
					core::StatementList body;

					// add this variable
					ctorParamTypes.push_back(builder.refType(retType));
					auto thisVar = builder.variable(builder.refType(builder.refType(retType)), variableId++);
					ctorVars.push_back(thisVar);

					// for each field
					for(const auto& field : record->getFields()) {
						const auto& fieldType = field->getType();
						// create the parameter variable
						auto paramVar = builder.variable(builder.refType(fieldType, false, false, core::lang::ReferenceType::Kind::CppRValueReference), variableId++);
						ctorParamTypes.push_back(paramVar->getType());
						ctorVars.push_back(paramVar);

						// and the initialization in the body
						auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisVar),
																								builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
						if(const auto& fieldTagType = fieldType.isa<core::TagTypePtr>()) {
							auto fieldTypeCtorInternal = core::analysis::getMoveConstructor(fieldTagType);
							if(!fieldTypeCtorInternal) {
								failed = true;
								break;
							}
							auto fieldTypeCtor = fieldTagType->peel(fieldTypeCtorInternal);
							body.push_back(builder.callExpr(fieldTypeCtor, fieldAccess, paramVar));
						} else {
							body.push_back(builder.initExpr(fieldAccess, builder.deref(paramVar)));
						}
					}

					// if we can create a new ctor
					if(!failed) {
						// we add a dummy parameter to disambiguate calls
						ctorVars.push_back(builder.variable(builder.refType(dummyCtorArgumentType), variableId++));
						ctorParamTypes.push_back(dummyCtorArgumentType);
						auto ctorType = builder.functionType(ctorParamTypes, defaultCtorType->getReturnType(), core::FunctionKind::FK_CONSTRUCTOR);
						ctor = builder.lambdaExpr(ctorType, ctorVars, builder.compoundStmt(body));
					}
				}
			}

			// add final return statement
			auto refRetType = builder.refType(retType);
			// if we generated a custom ctor, we create a call to it and also add a dummy argument
			if(ctor) {
				// for a ctor-call do not materialize moved parameters, pass them as an r-value reference
				for(auto& decl : valueDecls) {
					auto val = decl->getInitialization();
					decl = builder.declaration(val->getType(),val);
				}

				// create the dummy variable and add it to the body
				const auto dummyVar = builder.variable(builder.refType(dummyCtorArgumentType), stmts.size());
				stmts.push_back(builder.declarationStmt(dummyVar, builder.callExpr(core::analysis::getDefaultConstructor(dummyCtorArgumentType),
				                                                                   core::lang::buildRefDecl(builder.refType(dummyCtorArgumentType)))));
				// now create the return statement with the constructor call, also passing the dummy variable
				valueDecls.insert(valueDecls.begin(), builder.declaration(builder.refType(refRetType), core::lang::buildRefDecl(refRetType)));
				valueDecls.push_back(builder.declaration(builder.refType(dummyCtorArgumentType), builder.deref(dummyVar)));
				stmts.push_back(
					builder.returnStmt(
						builder.callExpr(refRetType, ctor, valueDecls),
						refRetType
					)
				);

				// otherwise we use an init expression, since this struct can be initialized that way
			} else {
				stmts.push_back(
					builder.returnStmt(
						builder.initExpr(core::lang::buildRefDecl(refRetType), valueDecls),
						refRetType
					)
				);
			}

			// create body
			auto body = builder.compoundStmt(stmts);
			// assemble load function
			auto impl = builder.lambdaExpr(retType,{reader},body,"load");

			// done
			return { builder.staticMemberFunction(FUN_NAME_LOAD,impl), ctor };
		}



		MemberFunctionPtr tryBuildStoreFunction(const TagTypeBindingPtr& binding) {
			auto& mgr = binding->getNodeManager();
			auto& base = mgr.getLangBasic();
			auto& ext = mgr.getLangExtension<SerializationModule>();
			auto& ref = mgr.getLangExtension<core::lang::ReferenceExtension>();
			IRBuilder builder(mgr);

			// check that the given record is a struct
			auto record = binding->getRecord().isa<StructPtr>();
			if (!record) return nullptr;

			// block out derived classes for now
			if (!record->getParents().empty()) {
				// TODO: add support for parents
				return MemberFunctionPtr();
			}

			// create the this pointer
			auto selfVar = builder.variable(
				core::lang::ReferenceType::create(
					core::lang::ReferenceType::create(
						binding->getTag(),
						true,false,core::lang::ReferenceType::Kind::Plain
					),
					false,false,core::lang::ReferenceType::Kind::Plain
				),
				0
			);
			auto self = builder.deref(selfVar);

			// create a writer instance
			auto writer = builder.variable(
				core::lang::ReferenceType::create(ext.getArchiveWriter(),false,false,core::lang::ReferenceType::Kind::CppReference),
				1
			);

			// build up body
			std::vector<StatementPtr> stmts;
			for(const auto& field : record->getFields()) {
				// read the current field
				auto access = core::lang::buildRefKindCast(
						builder.callExpr(
							ref.getRefMemberAccess(),
							self,
							builder.getIdentifierLiteral(field->getName()),
							builder.getTypeLiteral(field->getType())
						),
						core::lang::ReferenceType::Kind::CppReference
				);

				// create a call to the writer
				auto write = builder.callExpr(
						ext.getWrite(),
						core::lang::toPlainReference(writer),
						builder.getTypeLiteral(field->getType()),
						access
				);

				// add to body statements
				stmts.push_back(write);
			}

			// create body
			auto body = builder.compoundStmt(stmts);

			// assemble store function
			auto impl = builder.lambdaExpr(base.getUnit(),{selfVar,writer},body,"store",core::FK_MEMBER_FUNCTION);

			// done
			return builder.memberFunction(false,FUN_NAME_STORE,impl);
		}

		TypePtr getThisType(const TagTypeBindingPtr& binding) {
			return core::lang::ReferenceType::create(
					binding->getTag(),
				false,false,core::lang::ReferenceType::Kind::Plain
			);
		}

		LambdaExprPtr buildDefaultDefaultConstructor(const TagTypeBindingPtr& binding) {
			// create this type
			auto thisType = getThisType(binding);
			// create the new default constructor
			auto record = binding->getRecord().isa<StructPtr>();
			return core::analysis::buildDefaultDefaultConstructor(thisType,record->getParents(),record->getFields());
		}

		MemberFunctionPtr buildDefaultCopyAssign(const TagTypeBindingPtr& binding) {
			// create this type
			auto thisType = getThisType(binding);
			// create the new default copy assignment
			auto record = binding->getRecord().isa<StructPtr>();
			return core::analysis::buildDefaultCopyAssignOperator(thisType,record->getParents(),record->getFields());
		}

		MemberFunctionPtr buildDefaultMoveAssign(const TagTypeBindingPtr& binding) {
			// create this type
			auto thisType = getThisType(binding);
			// create the new default move assignment
			auto record = binding->getRecord().isa<StructPtr>();
			return core::analysis::buildDefaultMoveAssignOperator(thisType,record->getParents(),record->getFields());
		}

	}



	bool isSerializable(const TypePtr& type){
		// check for null
		if (!type) return false;

		NodeManager& mgr = type->getNodeManager();
		auto& basic = mgr.getLangBasic();

		// handle basic types
		if (type.isa<GenericTypePtr>()) {

			// test primitive types
			if (basic.isBool(type)) return true;

			if (basic.isChar(type)) return true;
			if (basic.isWChar16(type)) return true;
			if (basic.isWChar32(type)) return true;

			if (basic.isInt1(type)) return true;
			if (basic.isInt2(type)) return true;
			if (basic.isInt4(type)) return true;
			if (basic.isInt8(type)) return true;

			if (basic.isUInt1(type)) return true;
			if (basic.isUInt2(type)) return true;
			if (basic.isUInt4(type)) return true;
			if (basic.isUInt8(type)) return true;

			if (basic.isReal4(type)) return true;
			if (basic.isReal8(type)) return true;
			if (basic.isReal16(type)) return true;

			// test for fixed sized array types
			if (lang::isFixedSizedArray(type)) {
				return isSerializable(lang::getArrayElementType(type));
			}

			// test C++ std language containers
			if (lang::isStdString(type)) return true;

			// for pairs ...
			if (lang::isStdPair(type)) {
				// ... check nested types
				return isSerializable(lang::getStdPairFirstType(type)) && isSerializable(lang::getStdPairSecondType(type));
			}

			if (lang::isStdArray(type)) return isSerializable(lang::getStdArrayElementType(type));
			if (lang::isStdVector(type)) return isSerializable(lang::getStdVectorElementType(type));
			if (lang::isStdMap(type)) return isSerializable(lang::getStdMapKeyType(type)) && isSerializable(lang::getStdMapValueType(type));

			// -- AllScale Backend Types --

			if (backend::isDataItemReference(type)) return true;

			// all intercepted types used to be considered serializable, which was to presumptuous
		}

		// if it is a user defined type
		if (auto tagType = type.isa<TagTypePtr>()) {

			// only structs can be serializable
			if (!tagType->isStruct()) return false;

			// check for load and store function
			return hasLoadFunction(tagType) && hasStoreFunction(tagType);
		}

		// test enumeration types (which is a special tuple type)
		if (lang::isEnum(type)) return true;

		// everything else is not serializable
		return false;
	}

	TagTypeBindingPtr tryMakeSerializable(const TagTypeBindingPtr& binding) {
		TagTypeBindingPtr notSerializable;

		// check for null
		if (!binding) return notSerializable;

		// check that it is a struct
		auto record = binding->getRecord().as<StructPtr>();
		if (!record) return notSerializable;


		// Part I: check that the serialization is allowed

		// collect a set of changes to be performed
		NodeMap changes;

		// start with parent types
		for(const auto& cur : record->getParents()) {
			if (!isSerializable(cur->getType())) return notSerializable;
		}

		// check field types
		for(const auto& cur : record->getFields()) {
			if (!isSerializable(cur->getType())) return notSerializable;

			// field that do not have copy constructor can also not be serialized
			if (auto tagType = cur->getType().isa<TagTypePtr>()) {
				if (!core::analysis::hasMoveConstructor(tagType)) {
					return notSerializable;
				}
			}
		}

		// also check that there is no load / store function
		if (hasLoadFunction(binding) || hasStoreFunction(binding)) return notSerializable;


		// Part II: add load/store member functions

		// get load and store functions (if possible)
		auto res = binding;
		auto loadRes = tryBuildLoadFunction(res);
		auto load = loadRes.first;
		auto ctor = loadRes.second;
		auto store = tryBuildStoreFunction(res);

		// if one of those could not be created => fail conversion (the ctor is optional)
		if (!load || !store) return notSerializable;

		// get a dummy-tag-type for the next steps
		IRBuilder builder(binding.getNodeManager());
		TagTypeBindingMap bindings;
		bindings[binding->getTag()] = binding->getRecord();
		auto dummyTagType = builder.tagType(binding->getTag(),builder.tagTypeDefinition(bindings));

		// get a new default constructor if necessary
		LambdaExprPtr newDefaultCtor;
		if (!core::analysis::hasDefaultConstructor(dummyTagType)) {
			newDefaultCtor = buildDefaultDefaultConstructor(binding);
		}

		// get a new copy assignment operator if needed
		MemberFunctionPtr newCopyAssignment;
		if (!core::analysis::hasCopyAssignment(dummyTagType)) {
			newCopyAssignment = buildDefaultCopyAssign(binding);
		}

		// get a new move assignment operator if needed
		MemberFunctionPtr newMoveAssignment;
		if (!core::analysis::hasMoveAssignment(dummyTagType)) {
			newMoveAssignment = buildDefaultMoveAssign(binding);
		}

		// add those new function
		auto& mgr = binding->getNodeManager();
		std::map<NodeAddress,NodePtr> replacements;

		{
			auto staticMemberFuns = record->getStaticMemberFunctions()->getChildList();
			staticMemberFuns.push_back(load);
			replacements[TagTypeBindingAddress(res)->getRecord()->getStaticMemberFunctions()] = StaticMemberFunctions::get(mgr,staticMemberFuns);
		}

		if(ctor || newDefaultCtor) {
			auto ctors = record->getConstructors()->getChildList();
			if (ctor) ctors.push_back(ctor);
			if (newDefaultCtor) ctors.push_back(newDefaultCtor);
			replacements[TagTypeBindingAddress(res)->getRecord()->getConstructors()] = Expressions::get(mgr,ctors);
		}

		{
			auto memberFuns = record->getMemberFunctions()->getChildList();
			memberFuns.push_back(store);
			if (newCopyAssignment) memberFuns.push_back(newCopyAssignment);
			if (newMoveAssignment) memberFuns.push_back(newMoveAssignment);
			replacements[TagTypeBindingAddress(res)->getRecord()->getMemberFunctions()] = MemberFunctions::get(mgr,memberFuns);
		}

		// conduct replacement
		return core::transform::replaceAll(mgr,replacements).as<TagTypeBindingPtr>();
	}

	TypePtr tryMakeSerializable(const TypePtr& type) {
		TypePtr notSerializable;

		// check for null
		if (!type) return type;

		// skip if it is already serializable
		if (isSerializable(type)) return type;

		// if not serializable, we can only make structs serializable
		auto tagType = type.isa<TagTypePtr>();
		if (!tagType || !tagType->isStruct()) return notSerializable;

		// check that the input is OK
		//assert_correct_ir(type);

		// serialize all contained records (where possible)
		std::map<NodeAddress,NodePtr> replacements;
		for(const auto& binding : TagTypeAddress(tagType)->getDefinition()) {
			// attempt to serialize the current binding
			auto mod = tryMakeSerializable(binding.getAddressedNode());
			if (mod) replacements[binding] = mod;
		}

		// test whether there are changes to be made
		if (replacements.empty()) {
			return notSerializable;

		}

		// apply replacements
		auto res = core::transform::replaceAll(type->getNodeManager(),replacements).as<TagTypePtr>();

		// check that everything is fine
		//assert_correct_ir(res);

		// done
		return res;
	}


	insieme::core::NodePtr addAutoSerializationCode(const insieme::core::NodePtr& code, const ProgressCallback&) {

		// check input code
		assert_correct_ir(code);

		// just apply serialization attempt on all struct types
		return core::transform::transformBottomUp(code,[](const TypePtr& type){
			// try adding serialization code
			auto mod = tryMakeSerializable(type);
			return (mod) ? mod : type;
		}, core::transform::globalReplacement);

	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
