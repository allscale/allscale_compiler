#pragma once

#include "insieme/backend/preprocessor.h"

namespace allscale {
namespace compiler {
namespace backend {

	/**
	 * A pre-processor wrapping the entry point of the given code into the AllScale
	 * runtime startup and shutdown envelop and converts the invocation of main into
	 * a task spawning operation.
	 */
	class EntryPointWrapper : public insieme::backend::PreProcessor {
	  public:

		virtual insieme::core::NodePtr process(const insieme::backend::Converter& converter, const insieme::core::NodePtr& code) override;

		virtual std::ostream& printTo(std::ostream& out) const override { return out << "EntryPointWrapper"; }
	};


	/**
	 * A pre-processor step converting prec operator calls into code utilizing the runtimes
	 * work and data item infrastructure.
	 */
	class PrecConverter : public insieme::backend::PreProcessor {
	  public:

		virtual insieme::core::NodePtr process(const insieme::backend::Converter& converter, const insieme::core::NodePtr& code) override;

		virtual std::ostream& printTo(std::ostream& out) const override { return out << "PrecConverter"; }
	};

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
