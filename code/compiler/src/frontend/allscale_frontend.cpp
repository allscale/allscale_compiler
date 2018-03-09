
#include "allscale/compiler/frontend/allscale_frontend.h"

#include "insieme/frontend/extensions/interceptor_extension.h"

#include "allscale/compiler/frontend/allscale_fe_extension.h"
#include "allscale/compiler/config.h"


namespace allscale {
namespace compiler {
namespace frontend {

	void configureConversionJob(insieme::frontend::ConversionJob& job) {
		job.addInterceptedHeaderDir(getAllscaleAPICoreIncludeDir() + getAllscaleAPIInterceptionIncludePath());
		job.addInterceptedHeaderDir(getAllscaleAPICoreIncludeDir() + getAllscaleAPIDataItemsInterceptionIncludePath());
		job.addInterceptedHeaderDir(getAllscaleAPIUtilsIncludeDir());
		job.addInterceptionWhitelistEntry("allscale/utils/vector.h");
		job.addInterceptionWhitelistEntry("allscale/api/user/data/detail/adaptive_grid_detail.h");
		job.setStandard(insieme::frontend::ConversionSetup::Standard::Cxx14);
		job.registerDefaultExtensions();
		job.registerFrontendExtension<AllscaleExtension, insieme::frontend::extensions::InterceptorExtension>();
	}

}
}
}
