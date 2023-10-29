#ifndef WSW_04250cc2_5bf4_40d6_8c1b_b5305692255f_H
#define WSW_04250cc2_5bf4_40d6_8c1b_b5305692255f_H

#include "../common/common.h"
#include "../common/wswstaticvector.h"
#include "../common/wswstringview.h"
#include "../common/wswstring.h"
#include "../common/wswvector.h"

#include <functional>

namespace wsw::ui {

class WordsMatcher {
	// Use a holder to circumvent initialization order issues
	wsw::StaticVector<std::boyer_moore_searcher<const char *>, 1> m_exactMatcherHolder;
	wsw::Vector<unsigned> m_distanceBuffer;
	wsw::String m_stringDataBuffer;
	wsw::String m_inputBuffer;
public:
	struct Match {
		unsigned editDistance;
		unsigned commonLength;
	};
private:
	[[nodiscard]]
	auto matchByDistance( const wsw::StringView &input, const wsw::StringView &word, unsigned maxDist )
		-> std::optional<Match>;

	[[nodiscard]]
	auto prepareInput( const wsw::StringView &rawInput ) -> wsw::StringView;

	[[nodiscard]]
	auto distance( wsw::StringView a, wsw::StringView b, unsigned maxDist ) -> std::optional<Match>;

public:
	explicit WordsMatcher( const wsw::StringView &word );

	[[nodiscard]]
	auto match( const wsw::StringView &input, unsigned maxDist ) -> std::optional<Match>;
};

}

#endif