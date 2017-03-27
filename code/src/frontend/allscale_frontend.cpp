
#include "allscale/compiler/frontend/allscale_frontend.h"

#include "insieme/utils/config.h"
#include "insieme/frontend/extensions/interceptor_extension.h"

#include "allscale/compiler/frontend/allscale_fe_extension.h"


namespace allscale {
namespace compiler {
namespace frontend {

	void configureConversionJob(insieme::frontend::ConversionJob& job) {
		auto apiUtilsIncludeDir = insieme::utils::getInsiemeSourceRootDir() + "../../api/code/utils/include/";
		job.addIncludeDirectory(apiUtilsIncludeDir);
		auto apiIncludeDir = insieme::utils::getInsiemeSourceRootDir() + "../../api/code/api/include/";
		job.addIncludeDirectory(apiIncludeDir);
		job.addInterceptedHeaderDir(apiIncludeDir + "allscale/api/core/");
		job.setStandard(insieme::frontend::ConversionSetup::Standard::Cxx14);
		job.registerDefaultExtensions();
		job.registerFrontendExtension<AllscaleExtension, insieme::frontend::extensions::InterceptorExtension>();
	}

}
}
}
