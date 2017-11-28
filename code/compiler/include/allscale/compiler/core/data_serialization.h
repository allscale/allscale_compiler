#pragma once

#include "allscale/compiler/core/allscale_core.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * A language extension covering a set of serialization primitives.
	 */
	class SerializationModule : public insieme::core::lang::Extension {

		// constants for the name of reader and writer type
		static const std::string READER_NAME;
		static const std::string WRITER_NAME;

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class insieme::core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		SerializationModule(insieme::core::NodeManager& manager) : insieme::core::lang::Extension(manager) {}

	  public:

		/**
		 * The type of an archive reader.
		 */
		LANG_EXT_TYPE(ArchiveReader,READER_NAME);

		/**
		 * The type of an archive writer.
		 */
		LANG_EXT_TYPE(ArchiveWriter,WRITER_NAME);

		/**
		 * The operator reading data from an archive.
		 */
		LANG_EXT_LITERAL(Read,"read",READER_NAME + "::(type<'a>)->'a");

		/**
		 * The operator writing data to an archive.
		 */
		LANG_EXT_LITERAL(Write,"write",WRITER_NAME + "::(type<'a>,ref<'a,t,f,cpp_ref>)->unit");

	};


	/**
	 * Determines whether the given type is serializable or not. Only serializable
	 * data can be transfered between address spaces (e.g. distributed memory).
	 */
	bool isSerializable(const insieme::core::TypePtr& type);

	/**
	 * Attempts to add serialization capabilities to the given type, unless it is already
	 * serializable. If if already serializable, the given type will be returned. If the type
	 * is a struct type and not yet serializable, corresponding member functions enabling
	 * serialization will be added. For all other types, the attempt will fail. In this case,
	 * a null-pointer will be returned.
	 *
	 * @param type the type to be converted into a serializable type
	 * @return a serializable version of the type or null if the attempt fails
	 */
	insieme::core::TypePtr tryMakeSerializable(const insieme::core::TypePtr& type);

	/**
	 * A transformation pass adding data serialization code to every struct that can be serialized
	 * and does not have a user-provided serialization code.
	 *
	 * @param code the code to be converted
	 * @return the version of the code where classes contain serialization code where applicable
	 */
	insieme::core::NodePtr addAutoSerializationCode(const insieme::core::NodePtr& code, const ProgressCallback& = detail::ignoreProgress);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
