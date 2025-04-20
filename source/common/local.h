#ifndef WSW_bac442f1_f64d_4f40_a306_2fe44f42499c_H
#define WSW_bac442f1_f64d_4f40_a306_2fe44f42499c_H

#include "outputmessages.h"

#include <optional>
#include <utility>

#define comDebug()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Debug ) ).getWriter()
#define comNotice()  wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Notice ) ).getWriter()
#define comWarning() wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Warning ) ).getWriter()
#define comError()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Error ) ).getWriter()

[[nodiscard]]
auto testProcessorFeatures() -> unsigned;
[[nodiscard]]
auto testNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>>;

#endif