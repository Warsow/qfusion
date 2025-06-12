#include "configvars.h"
//#include <common/local.h>
#include "cvar.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/exceptions.h>
#include <common/helpers/stringsplitter.h>
#include <common/helpers/q_math.h>
#include <common/helpers/tonum.h>
#include <common/facilities/messagestreams.h>

#include <atomic>
#include <string>

template <typename T>
[[nodiscard]]
static auto loadFromOpaqueStorage( volatile T *storage ) -> T {
	using TypeOfAtomic = std::atomic<std::remove_cvref_t<T>>;
	static_assert( sizeof( TypeOfAtomic ) == sizeof( T ) && alignof( TypeOfAtomic ) == alignof( T ) );
	assert( !( ( (uintptr_t)storage ) % alignof( TypeOfAtomic ) ) );
	auto *atomic = reinterpret_cast<TypeOfAtomic *>( (void *)storage );
	assert( atomic->is_lock_free() );
	return atomic->load( std::memory_order::relaxed );
}

template <typename T>
static void storeToOpaqueStorage( volatile T *storage, T value ) {
	using TypeOfAtomic = std::atomic<std::remove_cvref_t<T>>;
	static_assert( sizeof( TypeOfAtomic ) == sizeof( T ) && alignof( TypeOfAtomic ) == alignof( T ) );
	assert( !( ( (uintptr_t)storage ) % alignof( TypeOfAtomic ) ) );
	auto *atomic = reinterpret_cast<TypeOfAtomic *>( (void *)storage );
	assert( atomic->is_lock_free() );
	atomic->store( value, std::memory_order::relaxed );
}

cvar_t *Cvar_Set2( const char *var_name, const char *value, bool force );

using wsw::operator""_asView;

DeclaredConfigVar::DeclaredConfigVar( const wsw::StringView &name, int registrationFlags )
	: m_name( name ), m_registrationFlags( registrationFlags ) {
	assert( m_name.isZeroTerminated() && !m_name.empty() );

	// Note: These lists differ for the game module and for the primary executable
	m_next     = s_listHead;
	s_listHead = this;
}

DeclaredConfigVar::~DeclaredConfigVar() {
	if( m_underlying ) {
		// TODO: This must be an atomic write, or even lock
		m_underlying->controller = nullptr;
	}
}

auto DeclaredConfigVar::modificationId() const -> uint64_t {
	return loadFromOpaqueStorage( &m_underlying->modificationId );
}

void DeclaredConfigVar::failOnOp( const char *op ) const {
	wsw::PodVector<char> message;
	message.append( "Attempt to"_asView );
	message.append( wsw::StringView( op ) );
	message.append( "a value of an uninitailized var"_asView );
	message.append( m_name.data(), m_name.size() );
	wsw::failWithRuntimeError( message.data() );
}

void DeclaredConfigVar::registerAllVars( DeclaredConfigVar *head ) {
	wsw::PodVector<char> defaultValueBuffer;

	for( DeclaredConfigVar *var = head; var; var = var->m_next ) {
		if( var->m_underlying ) {
			wsw::failWithRuntimeError( "Attempt to register a var twice" );
		}
		
		defaultValueBuffer.clear();
		var->getDefaultValueText( &defaultValueBuffer );
		if( defaultValueBuffer.empty() || defaultValueBuffer.back() != '\0' ) {
			defaultValueBuffer.push_back( '\0' );
		}

		var->m_underlying = Cvar_Get( var->m_name.data(), defaultValueBuffer.data(), var->m_registrationFlags, var );
	}
}

void DeclaredConfigVar::unregisterAllVars( DeclaredConfigVar *head ) {
	for( DeclaredConfigVar *var = head; var; var = var->m_next ) {
		if( var->m_underlying ) {
			var->m_underlying->controller = nullptr;
			var->m_underlying = nullptr;
		}
	}
}

template <typename T>
[[nodiscard]]
static auto getMinAllowedValue( const std::variant<std::monostate, Inclusive<T>, Exclusive<T>> &minBound ) -> T {
	T minAllowedValue = std::numeric_limits<T>::lowest();
	if( const Inclusive<T> *inclusiveLowerBound = std::get_if<Inclusive<T>>( &minBound ) ) {
		minAllowedValue = inclusiveLowerBound->value;
	} else if( const Exclusive<T> *exclusiveLowerBound = std::get_if<Exclusive<T>>( &minBound ) ) {
		assert( exclusiveLowerBound->value < std::numeric_limits<T>::max() );
		if constexpr( std::is_floating_point_v<T> ) {
			minAllowedValue = std::nextafter( exclusiveLowerBound->value, +std::numeric_limits<T>::infinity() );
			assert( std::isfinite( minAllowedValue ) );
		} else {
			minAllowedValue = exclusiveLowerBound->value + 1;
		}
		assert( minAllowedValue > exclusiveLowerBound->value );
	}
	return minAllowedValue;
}

template <typename T>
[[nodiscard]]
static auto getMaxAllowedValue( const std::variant<std::monostate, Inclusive<T>, Exclusive<T>> &maxBound ) -> T {
	T maxAllowedValue = std::numeric_limits<T>::max();
	if( const Inclusive<T> *inclusiveLowerBound = std::get_if<Inclusive<T>>( &maxBound ) ) {
		maxAllowedValue = inclusiveLowerBound->value;
	} else if( const Exclusive<T> *exclusiveUpperBound = std::get_if<Exclusive<T>>( &maxBound ) ) {
		assert( exclusiveUpperBound->value > std::numeric_limits<T>::lowest() );
		if constexpr( std::is_floating_point_v<T> ) {
			maxAllowedValue = std::nextafter( exclusiveUpperBound->value, -std::numeric_limits<T>::infinity() );
			assert( std::isfinite( maxAllowedValue ) );
		} else {
			maxAllowedValue = exclusiveUpperBound->value - 1;
		}
		assert( maxAllowedValue < exclusiveUpperBound->value );
	}
	return maxAllowedValue;
}

template <typename VarType, typename GivenType>
[[nodiscard]]
static auto convertValueUsingBounds( GivenType givenValue,
									 const std::variant<std::monostate, Inclusive<VarType>, Exclusive<VarType>> &minBound,
									 const std::variant<std::monostate, Inclusive<VarType>, Exclusive<VarType>> &maxBound )
	-> VarType {
	assert( !std::is_floating_point_v<GivenType> || ( std::isfinite( givenValue ) && std::isfinite( (VarType)givenValue ) ) );
	if( const VarType minValue = getMinAllowedValue( minBound ); givenValue < (GivenType)minValue ) {
		return minValue;
	}
	if( const VarType maxValue = getMaxAllowedValue( maxBound ); givenValue > (GivenType)maxValue ) {
		return maxValue;
	}
	return (VarType)givenValue;
}

BoolConfigVar::BoolConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {}

void BoolConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	defaultValueBuffer->push_back( m_params.byDefault ? '1' : '0' );
}

// TODO: This is for backward compatibility, purge it once cvar caller code gets rewritten
[[nodiscard]]
static auto asZeroTerminatedView( wsw::PodVector<char> *chars ) -> wsw::StringView {
	const size_t size = chars->size();
	if( chars->empty() || chars->back() != '\0' ) {
		chars->push_back( '\0' );
	}
	return wsw::StringView( chars->data(), size, wsw::StringView::ZeroTerminated );
}

auto BoolConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	if( const auto maybeNum = wsw::toNum<int64_t>( newValue ) ) {
		if( maybeNum == 0 ) {
			storeToOpaqueStorage( &m_cachedValue, false );
		} else if( maybeNum == 1 ) {
			storeToOpaqueStorage( &m_cachedValue, true );
		} else {
			storeToOpaqueStorage( &m_cachedValue, true );
			assert( tmpBuffer->empty() );
			tmpBuffer->push_back( '1' );
			return asZeroTerminatedView( tmpBuffer );
		}
	} else {
		if( newValue.equalsIgnoreCase( "true"_asView ) ) {
			storeToOpaqueStorage( &m_cachedValue, true );
		} else if( newValue.equalsIgnoreCase( "false"_asView ) ) {
			storeToOpaqueStorage( &m_cachedValue, false );
		} else {
			storeToOpaqueStorage( &m_cachedValue, m_params.byDefault );
			assert( tmpBuffer->empty() );
			tmpBuffer->push_back( m_cachedValue ? '1' : '0' );
			return asZeroTerminatedView( tmpBuffer );
		}
	}
	return std::nullopt;
}

auto BoolConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	if( const auto maybeNum = wsw::toNum<int64_t>( newValue ) ) {
		if( maybeNum != 0 && maybeNum != 1 ) {
			assert( tmpBuffer->empty() );
			tmpBuffer->push_back( '1' );
			return asZeroTerminatedView( tmpBuffer );
		}
	} else {
		if( !newValue.equalsIgnoreCase( "true"_asView ) && !newValue.equalsIgnoreCase( "false"_asView ) ) {
			assert( tmpBuffer->empty() );
			tmpBuffer->push_back( m_params.byDefault ? '1' : '0' );
			return asZeroTerminatedView( tmpBuffer );
		}
	}
	return std::nullopt;
}

bool BoolConfigVar::get() const {
	if( m_underlying ) [[likely]] {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		wsw::failWithRuntimeError( "Attempt to get uninitialized config variable" );
	}
}

void BoolConfigVar::helperOfSet( bool value, bool force ) {
	if( m_underlying ) [[likely]] {
		storeToOpaqueStorage( &m_cachedValue, value );
		Cvar_Set2( m_name.data(), value ? "1" : "0", force );
	} else {
		failOnSet();
	}
}

IntConfigVar::IntConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {
	assert( getMinAllowedValue( m_params.min ) <= getMaxAllowedValue( m_params.max ) );
	assert( convertValueUsingBounds( m_params.byDefault, m_params.min, m_params.max ) == m_params.byDefault );
}

void IntConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	// Should not allocate in this case
	defaultValueBuffer->append( std::to_string( m_params.byDefault ) );
}

auto IntConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	// Note: the double parsing kind of sucks, but it reduces implementation complexity
	// and strings are still the primary serialization/exchange form of config vars data.
	if( const std::optional<wsw::StringView> &correctedValueView = correctValue( newValue, tmpBuffer ) ) {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<int>( *correctedValueView ).value() );
		return correctedValueView;
	} else {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<int>( newValue ).value() );
		return std::nullopt;
	}
}

auto IntConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	if( const std::optional<int64_t> maybeNum = wsw::toNum<int64_t>( newValue ) ) {
		const int correctedValue = convertValueUsingBounds( *maybeNum, m_params.min, m_params.max );
		if( (int64_t)correctedValue != *maybeNum ) {
			assert( tmpBuffer->empty() );
			tmpBuffer->append( std::to_string( correctedValue ) );
			return asZeroTerminatedView( tmpBuffer );
		}
	} else {
		assert( tmpBuffer->empty() );
		tmpBuffer->append( std::to_string( m_params.byDefault ) );
		return asZeroTerminatedView( tmpBuffer );
	}
	return std::nullopt;
}

auto IntConfigVar::get() const -> int {
	if( m_underlying ) {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void IntConfigVar::helperOfSet( int value, bool force ) {
	if( m_underlying ) {
		value = convertValueUsingBounds( value, m_params.min, m_params.max );
		storeToOpaqueStorage( &m_cachedValue, value );
		Cvar_Set2( m_name.data(), std::to_string( value ).data(), force );
	} else {
		failOnSet();
	}
}

UnsignedConfigVar::UnsignedConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {
	assert( getMinAllowedValue( m_params.min ) <= getMaxAllowedValue( m_params.max ) );
	assert( convertValueUsingBounds( m_params.byDefault, m_params.min, m_params.max ) == m_params.byDefault );
}

void UnsignedConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	// Should not allocate in this case
	defaultValueBuffer->append( std::to_string( m_params.byDefault ) );
}

auto UnsignedConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	// See IntConfigVar remarks
	if( const std::optional<wsw::StringView> &correctedValueView = correctValue( newValue, tmpBuffer ) ) {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<unsigned>( *correctedValueView ).value() );
		return correctedValueView;
	} else {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<unsigned>( newValue ).value() );
		return std::nullopt;
	}
}

auto UnsignedConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	if( const std::optional<uint64_t> maybeNum = wsw::toNum<uint64_t>( newValue ) ) {
		const unsigned correctedValue = convertValueUsingBounds( *maybeNum, m_params.min, m_params.max );
		if( correctedValue != *maybeNum ) {
			assert( tmpBuffer->empty() );
			tmpBuffer->append( std::to_string( correctedValue ) );
			return asZeroTerminatedView( tmpBuffer );
		}
	} else {
		assert( tmpBuffer->empty() );
		tmpBuffer->append( std::to_string( m_params.byDefault ) );
		return asZeroTerminatedView( tmpBuffer );
	}
	return std::nullopt;
}

auto UnsignedConfigVar::get() const -> unsigned {
	if( m_underlying ) {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void UnsignedConfigVar::helperOfSet( unsigned value, bool force ) {
	if( m_underlying ) {
		value = convertValueUsingBounds( value, m_params.min, m_params.max );
		storeToOpaqueStorage( &m_cachedValue, value );
		Cvar_Set2( m_name.data(), std::to_string( value ).data(), force );
	} else {
		failOnSet();
	}
}

FloatConfigVar::FloatConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {
	assert( getMinAllowedValue( m_params.min ) <= getMaxAllowedValue( m_params.max ) );
	assert( convertValueUsingBounds( m_params.byDefault, m_params.min, m_params.max ) == m_params.byDefault );
}

void FloatConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	defaultValueBuffer->append( std::to_string( m_params.byDefault ) );
}

auto FloatConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	// See IntConfigVar remarks
	if( const std::optional<wsw::StringView> &correctedValueView = correctValue( newValue, tmpBuffer ) ) {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<float>( *correctedValueView ).value() );
		return correctedValueView;
	} else {
		storeToOpaqueStorage( &m_cachedValue, wsw::toNum<float>( newValue ).value() );
		return std::nullopt;
	}
}

auto FloatConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	if( const std::optional<float> maybeNum = wsw::toNum<float>( newValue ); maybeNum && std::isfinite( *maybeNum ) ) {
		const float correctedValue = convertValueUsingBounds( *maybeNum, m_params.min, m_params.max );
		if( correctedValue != *maybeNum ) {
			assert( tmpBuffer->empty() );
			tmpBuffer->append( std::to_string( correctedValue ) );
			return asZeroTerminatedView( tmpBuffer );
		}
	} else {
		assert( tmpBuffer->empty() );
		tmpBuffer->append( std::to_string( m_params.byDefault ) );
		return asZeroTerminatedView( tmpBuffer );
	}
	return std::nullopt;
}

auto FloatConfigVar::get() const -> float {
	if( m_underlying ) {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void FloatConfigVar::helperOfSet( float value, bool force ) {
	if( m_underlying ) {
		value = convertValueUsingBounds( value, m_params.min, m_params.max );
		storeToOpaqueStorage( &m_cachedValue, value );
		Cvar_Set2( m_name.data(), std::to_string( value ).data(), force );
	} else {
		failOnSet();
	}
}

StringConfigVar::StringConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {}

void StringConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	defaultValueBuffer->append( m_params.byDefault.data(), m_params.byDefault.size() );
}

auto StringConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> * ) -> std::optional<wsw::StringView> {
	// TODO: What should we do for strings?
	return std::nullopt;
}

auto StringConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> * ) const -> std::optional<wsw::StringView> {
	// TODO: What should we do for strings?
	return std::nullopt;
}

auto StringConfigVar::get() const -> wsw::StringView {
	// Note: see the remark to the declaration
	if( m_underlying ) {
		return wsw::StringView( m_underlying->string );
	} else {
		failOnGet();
	}
}

void StringConfigVar::helperOfSet( const wsw::StringView &value, bool force ) {
	if( m_underlying ) {
		if( value.isZeroTerminated() ) {
			Cvar_Set2( m_name.data(), value.data(), force );
		} else {
			wsw::PodVector ztValue( m_name.data(), m_name.size() );
			ztValue.push_back( '\0' );
			Cvar_Set2( m_name.data(), ztValue.data(), force );
		}
	} else {
		failOnSet();
	}
}

template <typename String>
static void colorToString( int color, String *__restrict string ) {
	assert( !COLOR_A( color ) );
	string->append( std::to_string( COLOR_R( color ) ) );
	string->push_back( ' ' );
	string->append( std::to_string( COLOR_G( color ) ) );
	string->push_back( ' ' );
	string->append( std::to_string( COLOR_B( color  ) ) );
}

[[nodiscard]]
static auto parseColor( const wsw::StringView &string ) -> std::optional<int> {
	int parts[3];
	int numParts = 0;

	wsw::StringSplitter splitter( string );
	for(;; ) {
		if( const std::optional<wsw::StringView> maybeToken = splitter.getNext() ) {
			if( numParts == 3 ) {
				return std::nullopt;
			}
			if( const auto maybeNum = wsw::toNum<uint8_t>( *maybeToken ) ) {
				parts[numParts++] = *maybeNum;
			} else {
				return std::nullopt;
			}
		} else {
			if( numParts == 3 ) {
				break;
			} else {
				return std::nullopt;
			}
		}
	}

	assert( numParts == 3 );
	return COLOR_RGB( parts[0], parts[1], parts[2] );
}

ColorConfigVar::ColorConfigVar( const wsw::StringView &name, Params &&params )
	: DeclaredConfigVar( name, params.flags ), m_params( params ) {}

void ColorConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	colorToString( m_params.byDefault & kRgbMask, defaultValueBuffer );
}

auto ColorConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	if( const std::optional<int> maybeColor = parseColor( newValue ) ) {
		storeToOpaqueStorage( &m_cachedValue, *maybeColor );
		return std::nullopt;
	} else {
		assert( tmpBuffer->empty() );
		storeToOpaqueStorage( &m_cachedValue, m_params.byDefault & kRgbMask );
		colorToString( m_params.byDefault & kRgbMask, tmpBuffer );
		return asZeroTerminatedView( tmpBuffer );
	}
}

auto ColorConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	if( !parseColor( newValue ) ) {
		assert( tmpBuffer->empty() );
		colorToString( m_params.byDefault & kRgbMask, tmpBuffer );
		return asZeroTerminatedView( tmpBuffer );
	}
	return std::nullopt;
}

auto ColorConfigVar::get() const -> int {
	if( m_underlying ) {
		assert( !( m_cachedValue & ~kRgbMask ) );
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void ColorConfigVar::helperOfSet( int value, bool force ) {
	if( m_underlying ) {
		// We clamp out-of-range values silently
		value = value & kRgbMask;
		wsw::StaticString<16> buffer;
		storeToOpaqueStorage( &m_cachedValue, value );
		colorToString( value, &buffer );
		Cvar_Set2( m_name.data(), buffer.data(), force );
	} else {
		failOnSet();
	}
}

UntypedEnumValueConfigVar::UntypedEnumValueConfigVar( const wsw::StringView &name, int registrationFlags,
													  MatcherObj matcherObj, MatcherFn matcherFn,
													  wsw::PodVector<int> &&enumValues, int defaultValue )
	: DeclaredConfigVar( name, registrationFlags ), m_matcherObj( matcherObj ), m_matcherFn( matcherFn )
	, m_enumValues( std::forward<wsw::PodVector<int>>( enumValues ) ), m_defaultValue( defaultValue ) {
	assert( !m_enumValues.empty() );
	m_minEnumValue = std::numeric_limits<int>::max();
	m_maxEnumValue = std::numeric_limits<int>::min();
	for( const int value: m_enumValues ) {
		m_minEnumValue = std::min( value, m_minEnumValue );
		m_maxEnumValue = std::max( value, m_maxEnumValue );
	}
}

auto UntypedEnumValueConfigVar::helperOfGet() const -> int {
	if( m_underlying ) [[likely]] {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void UntypedEnumValueConfigVar::helperOfSet( int value, bool force ) {
	if( m_underlying ) [[likely]] {
		if( !isAValidValue( value ) ) {
			value = m_defaultValue;
		}
		storeToOpaqueStorage( &m_cachedValue, value );
		Cvar_Set2( m_name.data(), std::to_string( value ).data(), force );
	} else {
		failOnSet();
	}
}

void UntypedEnumValueConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	defaultValueBuffer->append( std::to_string( m_defaultValue ) );
}

auto UntypedEnumValueConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	std::optional<int> validatedValue;
	if( const auto maybeNum = wsw::toNum<int>( newValue ) ) {
		if( isAValidValue( *maybeNum ) ) {
			return std::nullopt;
		}
	} else if( m_matcherFn( m_matcherObj, newValue ) != std::nullopt ) {
		return std::nullopt;
	}
	getDefaultValueText( tmpBuffer );
	return asZeroTerminatedView( tmpBuffer );
}

auto UntypedEnumValueConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	std::optional<int> validatedValue;
	if( const auto maybeNum = wsw::toNum<int>( newValue ) ) {
		if( isAValidValue( *maybeNum ) ) {
			validatedValue = maybeNum;
		}
	} else if( const auto maybeMatchedValue = m_matcherFn( m_matcherObj, newValue ) ) {
		validatedValue = maybeMatchedValue;
	}
	if( validatedValue ) {
		storeToOpaqueStorage( &m_cachedValue, *validatedValue );
		return std::nullopt;
	} else {
		storeToOpaqueStorage( &m_cachedValue, m_defaultValue );
		getDefaultValueText( tmpBuffer );
		return asZeroTerminatedView( tmpBuffer );
	}
}

bool UntypedEnumValueConfigVar::isAValidValue( int value ) const {
	if( value >= m_minEnumValue && value <= m_maxEnumValue ) {
		return wsw::contains( m_enumValues.begin(), m_enumValues.end(), value );
	}
	return false;
}

UntypedEnumFlagsConfigVar::UntypedEnumFlagsConfigVar( const wsw::StringView &name, int registrationFlags,
													  MatcherObj matcherObj, MatcherFn matcherFn,
													  wsw::PodVector<unsigned> &&enumValues,
													  size_t typeSizeInBytes, unsigned defaultValue )
	: DeclaredConfigVar( name, registrationFlags ), m_matcherObj( matcherObj ), m_matcherFn( matcherFn )
	, m_enumValues( std::forward<wsw::PodVector<unsigned>>( enumValues ) ) {
	assert( !m_enumValues.empty() );
	m_allBitsInEnumValues = 0;
	m_hasZeroInValues     = false;
	for( const unsigned value: m_enumValues ) {
		m_allBitsInEnumValues |= value;
		if( !value ) {
			m_hasZeroInValues = true;
		}
	}
	m_defaultValue = defaultValue & m_allBitsInEnumValues;
	// We can't just use "~0u" as lesser than "~0u" all-bits-set default values may be specified for types of lesser size
	assert( typeSizeInBytes == 1 || typeSizeInBytes == 2 || typeSizeInBytes == 4 );
	m_allBitsSetValueForType = 255;
	for( size_t i = 1; i < typeSizeInBytes; ++i ) {
		m_allBitsSetValueForType = ( m_allBitsSetValueForType << 8 ) | 255;
	}
}

void UntypedEnumFlagsConfigVar::getDefaultValueText( wsw::PodVector<char> *defaultValueBuffer ) const {
	assert( defaultValueBuffer->empty() );
	// TODO: retrieve the default value string using the matcher?
	if( m_defaultValue != m_allBitsSetValueForType && m_defaultValue != m_allBitsInEnumValues ) {
		defaultValueBuffer->append( std::to_string( m_defaultValue ) );
	} else {
		// Try displaying it nicer
		defaultValueBuffer->append( "-1", 2 );
	}
}

// Note: We do not correct "-1", despite internally truncating it. Let users type "-1" without hassle.

auto UntypedEnumFlagsConfigVar::correctValue( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) const -> std::optional<wsw::StringView> {
	std::optional<unsigned> validatedValue;
	if( const auto maybeNum = wsw::toNum<unsigned>( newValue ) ) {
		if( isAnAcceptableValue( *maybeNum ) ) {
			return std::nullopt;
		}
	} else {
		// Try parsing '|' - separated string of tokens or values
		if( parseValueFromString( newValue ) != std::nullopt ) {
			return std::nullopt;
		}
	}
	getDefaultValueText( tmpBuffer );
	return asZeroTerminatedView( tmpBuffer );
}

auto UntypedEnumFlagsConfigVar::handleValueChanges( const wsw::StringView &newValue, wsw::PodVector<char> *tmpBuffer ) -> std::optional<wsw::StringView> {
	std::optional<unsigned> validatedValue;
	if( const auto maybeNum = wsw::toNum<unsigned>( newValue ) ) {
		if( isAnAcceptableValue( *maybeNum ) ) {
			// While accepting (enum-type)~0u, make sure that it does not contain extra bits
			validatedValue = *maybeNum & m_allBitsInEnumValues;
		}
	} else if( wsw::toNum<int>( newValue ) == std::optional<int>( -1 ) ) {
		validatedValue = m_allBitsInEnumValues;
	} else {
		// Try parsing '|' - separated string of tokens or values
		validatedValue = parseValueFromString( newValue );
	}
	if( validatedValue ) {
		storeToOpaqueStorage( &m_cachedValue, *validatedValue );
		return std::nullopt;
	} else {
		storeToOpaqueStorage( &m_cachedValue, m_defaultValue );
		getDefaultValueText( tmpBuffer );
		return asZeroTerminatedView( tmpBuffer );
	}
}

auto UntypedEnumFlagsConfigVar::parseValueFromString( const wsw::StringView &string ) const -> std::optional<unsigned> {
	if( string.empty() ) {
		if( m_hasZeroInValues ) {
			return 0;
		}
		return std::nullopt;
	}

	unsigned newValueBits = 0;
	wsw::StringSplitter splitter( string );
	while( const auto maybeNextToken = splitter.getNext( '|' ) ) {
		if( const auto maybeTokenNum = wsw::toNum<unsigned>( *maybeNextToken ) ) {
			// Match against individual values TODO: Check range for doing lookups fast?
			if( wsw::contains( m_enumValues.begin(), m_enumValues.end(), *maybeTokenNum ) ) {
				newValueBits |= *maybeTokenNum;
			} else {
				return std::nullopt;
			}
		} else {
			if( const auto maybeMatchedValue = m_matcherFn( m_matcherObj, *maybeNextToken ) ) {
				newValueBits |= *maybeTokenNum;
			} else {
				return std::nullopt;
			}
		}
	}
	return newValueBits;
}

bool UntypedEnumFlagsConfigVar::isAnAcceptableValue( unsigned value ) const {
	// If it matches some bits (or is zero and zeroes are allowed)
	if( ( m_allBitsInEnumValues & value ) || ( m_hasZeroInValues && !value ) ) {
		// If it does not add extra bits, with the exception of the all-bits-set value
		if( !( ~m_allBitsInEnumValues & value ) || ( value == m_allBitsSetValueForType ) ) {
			// Note: to be really correct, we should check whether it's in set of actual possible combinations of flags
			return true;
		}
	}
	return false;
}

auto UntypedEnumFlagsConfigVar::helperOfGet() const -> unsigned {
	if( m_underlying ) [[likely]] {
		return loadFromOpaqueStorage( &m_cachedValue );
	} else {
		failOnGet();
	}
}

void UntypedEnumFlagsConfigVar::helperOfSet( unsigned value, bool force ) {
	if( m_underlying ) [[likely]] {
		if( isAnAcceptableValue( value ) ) {
			storeToOpaqueStorage( &m_cachedValue, value & m_allBitsInEnumValues );
		} else {
			assert( !( m_defaultValue & ~m_allBitsInEnumValues ) );
			storeToOpaqueStorage( &m_cachedValue, m_defaultValue );
		}
		// TODO: Was it incorrect? m_cachedValue = value & m_allBitsInEnumValues;
		if( value != m_allBitsSetValueForType && value != m_allBitsInEnumValues ) {
			Cvar_Set2( m_name.data(), std::to_string( value ).data(), force );
		} else {
			Cvar_Set2( m_name.data(), "-1", force );
		}
	} else {
		failOnSet();
	}
}