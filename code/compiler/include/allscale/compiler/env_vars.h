#pragma once

///////////// ANALYSIS

// enable debugging of allscale data requirement analysis
#define ALLSCALE_SHARED_MEMORY_ONLY "ALLSCALE_SHARED_MEMORY_ONLY"

// show node addresses in diagnostics output
#define ALLSCALE_DIAG_NODE_ADDRESSES "ALLSCALE_DIAG_NODE_ADDRESSES"

// skip global constant propagation step
#define ALLSCALE_SKIP_GLOBAL_CONSTANT_PROPAGATION "ALLSCALE_SKIP_GLOBAL_CONSTANT_PROPAGATION"

// skip data requirement analysis
#define ALLSCALE_SKIP_ANALYSIS "ALLSCALE_SKIP_ANALYSIS"

// enable debugging of allscale data requirement analysis
#define ALLSCALE_DEBUG_ANALYSIS "ALLSCALE_DEBUG_ANALYSIS"

// if set dump solver time statistics to given file
#define ALLSCALE_DUMP_DETAILED_ANALYSIS_STATS "ALLSCALE_DUMP_DETAILED_ANALYSIS_STATS"
