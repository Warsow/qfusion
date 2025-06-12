#include <common/facilities/syspublic.h>
#include <common/helpers/wswbasicmath.h>
#include <common/syslocal.h>

#include <cassert>

struct SystemFeaturesHolder {
	unsigned processorFeatures { 0 };
	std::optional<std::pair<unsigned, unsigned>> numberOfProcessors;
	SystemFeaturesHolder() {
		processorFeatures  = testProcessorFeatures();
		numberOfProcessors = testNumberOfProcessors();

		// We have only set the most significant feature bit for code clarity.
		// Since this bit implies all least-significant bits presence, set these bits
		if( processorFeatures ) {
			assert( wsw::isPowerOf2( processorFeatures ) );
			processorFeatures |= ( processorFeatures - 1u );
		}
	}
};

[[nodiscard]]
static auto getSystemFeaturesHolder() -> const SystemFeaturesHolder * {
	// This design is thread-safe
	static SystemFeaturesHolder instance;
	return &instance;
}

unsigned Sys_GetProcessorFeatures() {
	return getSystemFeaturesHolder()->processorFeatures;
}

auto Sys_GetNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>> {
	return getSystemFeaturesHolder()->numberOfProcessors;
}
