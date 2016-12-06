#pragma once

#include <fstream>
#include <boost/filesystem.hpp>

#include "insieme/frontend/frontend.h"

#include "allscale/compiler/frontend/allscale_frontend.h"


namespace allscale {
namespace compiler {
namespace frontend {


	namespace fs = boost::filesystem;

	/**
	 * An enumeration of supported source language types.
	 */
	enum SrcType {
		C, CPP
	};

	/**
	 * A class managing the life-cycle of temporary source files which will only be
	 * created for the sake of unit tests.
	 */
	class Source {

		/**
		 * The path to the temporary file.
		 */
		fs::path file;

	public:

		/**
		 * The constructor creates a temporary file containing the given example code. The
		 * file will exist as long as the object is alive.
		 */
		Source(const string& code, SrcType type = CPP) {

			// create a temporary file containing the code
			switch(type) {
				case C:   file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.c"); break;
				case CPP: file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.cpp"); break;
				default: assert(false && "Invalid type selected!");
			}

			// write source to file
			std::fstream srcFile(file.string(), std::fstream::out);
			srcFile << code << "\n";
			srcFile.close();
		}

		Source(const Source&) = delete;
		Source(Source&&) = delete;

		~Source() {
			// remove temporary file
			if (fs::exists(file)) {
				fs::remove(file);
			}
		}

		const fs::path& getPath() const {
			return file;
		}

		operator fs::path() const { return getPath(); }

	};

	insieme::core::ProgramPtr parseFile(insieme::core::NodeManager& mgr, const fs::path& file) {

		// set up the frontend conversion job
		insieme::frontend::ConversionJob job(file);

		// configure the frontend with allscale specific settings
		configureConversionJob(job);

		// run the conversion job
		return job.execute(mgr);
	}

	insieme::core::ProgramPtr parseCode(insieme::core::NodeManager& mgr, const std::string& code) {

		// create a temporary code file
		Source src(code);

		// run the conversion
		return parseFile(mgr,src);
	}


} // end namespace frontend
} // end namespace compiler
} // end namespace allscale

