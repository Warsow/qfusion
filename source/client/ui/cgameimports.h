#ifndef WSW_1468775f_9a19_4e35_9daf_d703cb9288ef_H
#define WSW_1468775f_9a19_4e35_9daf_d703cb9288ef_H

#include <common/types/stringview.h>
#include <span>

struct BasicObjectiveIndicatorState {
public:
	uint8_t color[3] { 0, 0, 0 };
	uint8_t anim { 0 };
	int8_t progress { 0 };
	uint8_t iconNum { 0 };
	uint8_t stringNum { 0 };
	bool enabled { false };

	[[nodiscard]]
	bool operator==( const BasicObjectiveIndicatorState &that ) const {
		return color[0] == that.color[0] && color[1] == that.color[1] && color[2] == that.color[2] &&
			anim == that.anim && progress == that.progress && iconNum == that.iconNum &&
			stringNum == that.stringNum && enabled == that.enabled;
	}
	[[nodiscard]]
	bool operator!=( const BasicObjectiveIndicatorState &that ) const {
		return !( ( *this ) == that );
	}
};

BasicObjectiveIndicatorState CG_HudIndicatorState( int );

int CG_RealClientTeam();
bool CG_HasTwoTeams();

int CG_ActiveWeapon( unsigned viewStateIndex );
bool CG_HasWeapon( unsigned viewStateIndex, int weapon );
int CG_Health( unsigned viewStateIndex );
int CG_Armor( unsigned viewStateIndex );
auto CG_WeaponAmmo( unsigned viewStateIndex, int weapon ) -> std::pair<int, int>;
bool CG_IsPovAlive( unsigned viewStateIndex );

int CG_TeamAlphaDisplayedColor();
int CG_TeamBetaDisplayedColor();
int CG_TeamToForcedTeam( int team );
bool CG_IsOperator();
bool CG_IsChallenger();
bool CG_CanBeReady();
bool CG_IsReady();
int CG_MyRealTeam();

unsigned CG_GetPrimaryViewStateIndex();
unsigned CG_GetOurClientViewStateIndex();
std::optional<unsigned> CG_GetPlayerNumForViewState( unsigned viewStateIndex );
bool CG_IsViewAttachedToPlayer();

// TODO: Lift it to the top level
struct Rect {
	int x, y, width, height;
	[[nodiscard]]
	bool operator==( const Rect &that ) const {
		return x == that.x && y == that.y && width == that.width && height == that.height;
	}
	[[nodiscard]]
	bool operator!=( const Rect &that ) const {
		return x != that.x || y != that.y || width != that.width || height != that.height;
	}
};

// The primary pov is not included
void CG_GetMultiviewConfiguration( std::span<const uint8_t> *pane1ViewStateNums,
								   std::span<const uint8_t> *pane2ViewStateNums,
								   std::span<const uint8_t> *tileViewStateNums,
								   std::span<const Rect> *tilePositions );

struct ViewState;

std::optional<wsw::StringView> CG_HudIndicatorIconPath( int );
std::optional<wsw::StringView> CG_HudIndicatorStatusString( int );
auto CG_GetMatchClockTime() -> std::pair<int, int>;
std::optional<unsigned> CG_ActivePovOfViewState( unsigned viewStateIndex );
bool CG_IsUsingChasePov( unsigned viewStateIndex );
wsw::StringView CG_PlayerName( unsigned playerNum );
wsw::StringView CG_PlayerClan( unsigned playerClan );

bool CG_UsesTiledView();
void CG_SwitchToPlayerNum( unsigned playerNum );

extern struct gs_state_t *cggs;

#endif