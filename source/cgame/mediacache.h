#ifndef WSW_7f020248_88b8_40f2_ae59_1e97c51e2f82_H
#define WSW_7f020248_88b8_40f2_ae59_1e97c51e2f82_H

#include "../common/q_shared.h"
#include "../common/q_comref.h"
#include "../common/q_math.h"
#include "../common/gs_public.h"

#include "../common/wswstringview.h"
#include "../common/wswpodvector.h"
#include "../common/stringspanstorage.h"
#include "../client/snd_public.h"

struct model_s;
struct shader_s;

class MediaCache {
	// Currently all entries are precached.
	// Separate types that check a handle status on every access should be introduced if lazy loading is needed.
public:
	template <typename T>
	class CachedHandle {
		friend class MediaCache;

		// TODO: We can compress links to offsets from parent
		CachedHandle<T> *m_next { nullptr };
		T *m_handle { nullptr };
	public:
		[[nodiscard]]
		operator T*() { return m_handle; }
		[[nodiscard]]
		operator const T*() const { return m_handle; }
		[[nodiscard]]
		auto getAddressOfHandle() -> T ** { return &m_handle; }
	};

	class CachedSound : public CachedHandle<const SoundSet> {
		friend class MediaCache;
		CachedSound( MediaCache *parent, const SoundSetProps &props );
		SoundSetProps m_props;
	};
	class CachedModel : public CachedHandle<model_s> {
		friend class MediaCache;
		CachedModel( MediaCache *parent, const wsw::StringView &name );
		const wsw::StringView m_name;
	};
	class CachedMaterial : public CachedHandle<shader_s> {
		friend class MediaCache;
		CachedMaterial( MediaCache *parent, const wsw::StringView &name );
		const wsw::StringView m_name;
	};

	void registerSounds();
	void registerModels();
	void registerMaterials();
private:
	// TODO: Should we just keep precached and non-precached linked handles in different lists?
	CachedSound *m_sounds { nullptr };
	CachedModel *m_models { nullptr };
	CachedMaterial *m_materials { nullptr };

	template <typename T>
	void link( T *item, T **head ) {
		item->m_next = *head;
		*head = item;
	}

	void registerSound( CachedSound *sound );
	void registerModel( CachedModel *model );
	void registerMaterial( CachedMaterial *material );
public:
	MediaCache();

	CachedSound sndChat { this, SoundSetProps { .name = SoundSetProps::Exact { S_CHAT } } };
	CachedSound sndWeaponHit[4] {
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_0" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_1" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_2" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_3" } } },
	};
	CachedSound sndWeaponHit2[4] {
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_plus_0" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_plus_1" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_plus_2" } } },
		{ this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/misc/hit_plus_3" } } },
	};

	CachedSound sndImpactMetal { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_metal*" } } };
	CachedSound sndImpactGlass { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_glass*" } } };
	CachedSound sndImpactWood { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_wood*" } } };
	CachedSound sndImpactSoft { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_soft*" } } };
	CachedSound sndImpactSolid { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_solid*" } } };
	CachedSound sndImpactWater { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/impact_water*" } } };

	CachedSound sndWeaponKill { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_KILL } } };
	CachedSound sndWeaponHitTeam { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_HIT_TEAM } } };
	CachedSound sndWeaponUp { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_SWITCH } } };
	CachedSound sndWeaponUpNoAmmo { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_NOAMMO } } };

	CachedSound sndWalljumpFailed { this, SoundSetProps { .name = SoundSetProps::Exact { "sounds/world/ft_walljump_failed" } } };

	CachedSound sndItemRespawn { this, SoundSetProps { .name = SoundSetProps::Exact { S_ITEM_RESPAWN } } };
	CachedSound sndPlayerRespawn { this, SoundSetProps { .name = SoundSetProps::Exact { S_PLAYER_RESPAWN } } };
	CachedSound sndTeleportIn { this, SoundSetProps { .name = SoundSetProps::Exact { S_TELEPORT_IN } } };
	CachedSound sndTeleportOut { this, SoundSetProps { .name = SoundSetProps::Exact { S_TELEPORT_OUT } } };
	CachedSound sndShellHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_SHELL_HIT } } };

	CachedSound sndGunbladeWeakShot { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/blade_strike*" } } };
	CachedSound sndBladeFleshHit { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/blade_hitflsh*" } } };
	CachedSound sndBladeWallHit { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/blade_hitwall*" } } };

	CachedSound sndGunbladeStrongShot { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_GUNBLADE_S_SHOT } } };

	CachedSound sndGunbladeStrongHit { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/bladegun_strong_hit*" } } };

	CachedSound sndGrenadeWeakBounce { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/gren_strong_bounce*" } } };
	CachedSound sndGrenadeStrongBounce { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/gren_strong_bounce*" } } };
	CachedSound sndGrenadeArm { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/gren_strong_arm*" } } };
	CachedSound sndGrenadeActivate { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/gren_strong_activate*" } } };

	CachedSound sndGrenadeWeakExplosion { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_GRENADE_W_HIT } } };
	CachedSound sndGrenadeStrongExplosion { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_GRENADE_S_HIT } } };

	CachedSound sndRocketLauncherWeakHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_ROCKET_W_HIT } } };
	CachedSound sndRocketLauncherStrongHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_ROCKET_S_HIT } } };

	CachedSound sndPlasmaWeakHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_PLASMAGUN_W_HIT } } };
	CachedSound sndPlasmaStrongHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_PLASMAGUN_S_HIT } } };

	// Lasergun sounds
	CachedSound sndLasergunWeakHum { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_W_HUM } } };
	CachedSound sndLasergunWeakQuadHum { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_W_QUAD_HUM } } };
	CachedSound sndLasergunWeakStop { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_W_STOP } } };
	CachedSound sndLasergunStrongHum { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_S_HUM } } };
	CachedSound sndLasergunStrongQuadHum { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_S_QUAD_HUM } } };
	CachedSound sndLasergunStrongStop { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_LASERGUN_S_STOP } } };
	CachedSound sndLasergunHit { this, SoundSetProps { .name = SoundSetProps::Pattern { "sounds/weapons/laser_hit*" } } };

	CachedSound sndElectroboltHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_ELECTROBOLT_HIT } } };

	CachedSound sndExplosionLfe { this, SoundSetProps { .name = SoundSetProps::Exact { S_EXPLOSION_LFE } } };
	CachedSound sndQuadFireSound { this, SoundSetProps { .name = SoundSetProps::Exact { S_QUAD_FIRE } } };

	CachedSound sndWaveWeakHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_SHOCKWAVE_W_HIT } } };
	CachedSound sndWaveStrongHit { this, SoundSetProps { .name = SoundSetProps::Exact { S_WEAPON_SHOCKWAVE_S_HIT } } };

	CachedModel modRocketExplosion { this, wsw::StringView( PATH_ROCKET_EXPLOSION_MODEL ) };
	CachedModel modPlasmaExplosion { this, wsw::StringView( PATH_PLASMA_EXPLOSION_MODEL ) };
	CachedModel modWaveExplosion { this, wsw::StringView( PATH_SHOCKWAVE_EXPLOSION_MODEL ) };

	CachedModel modDash { this, wsw::StringView( "models/effects/dash_burst.md3" ) };
	CachedModel modHeadStun { this, wsw::StringView( "models/effects/head_stun.md3" ) };

	CachedModel modBulletExplode { this, wsw::StringView( PATH_BULLET_EXPLOSION_MODEL ) };
	CachedModel modBladeWallHit { this, wsw::StringView( PATH_GUNBLADEBLAST_IMPACT_MODEL ) };
	CachedModel modBladeWallExplo { this, wsw::StringView( PATH_GUNBLADEBLAST_EXPLOSION_MODEL ) };
	CachedModel modElectroBoltWallHit { this, wsw::StringView( PATH_ELECTROBLAST_IMPACT_MODEL ) };
	CachedModel modInstagunWallHit { this, wsw::StringView( PATH_INSTABLAST_IMPACT_MODEL ) };
	CachedModel modLasergunWallExplo { this, wsw::StringView( PATH_LASERGUN_IMPACT_MODEL ) };

	CachedMaterial shaderLaserSpikeParticle { this, wsw::StringView( "gfx/effects/laser_spike_particle" ) };
	CachedMaterial shaderLaserImpactParticle { this, wsw::StringView( "gfx/effects/laser_impact_particle" ) };
	CachedMaterial shaderPlasmaImpactParticle { this, wsw::StringView( "gfx/effects/plasma_impact_particle" ) };
	CachedMaterial shaderBlastImpactParticle { this, wsw::StringView( "gfx/effects/blast_impact_particle" ) };

	CachedMaterial shaderWoodBurstParticle { this, wsw::StringView( "gfx/effects/wood_burst_particle" ) };
	CachedMaterial shaderWoodDustParticle { this, wsw::StringView( "gfx/effects/wood_dust_particle" ) };
	CachedMaterial shaderWoodDebrisParticle { this, wsw::StringView( "gfx/effects/wood_debris_particle" ) };

	CachedMaterial shaderRocketSmokeTrailParticle { this, wsw::StringView( "gfx/effects/rocket_smoke_trail_particle" ) };
	CachedMaterial shaderRocketFireTrailParticle { this, wsw::StringView( "gfx/effects/rocket_fire_trail_particle" ) };
	CachedMaterial shaderRocketPolyTrailCombined { this, wsw::StringView( "gfx/effects/rocket_poly_trail_combined" ) };
	CachedMaterial shaderRocketPolyTrailStandalone { this, wsw::StringView( "gfx/effects/rocket_poly_trail_standalone" ) };

	CachedMaterial shaderGrenadeSmokeTrailParticle { this, wsw::StringView( "gfx/effects/grenade_smoke_trail_particle" ) };
	CachedMaterial shaderGrenadeFireTrailParticle { this, wsw::StringView( "gfx/effects/grenade_fire_trail_particle" ) };
	CachedMaterial shaderGrenadePolyTrailCombined { this, wsw::StringView( "gfx/effects/grenade_poly_trail_particle" ) };
	CachedMaterial shaderGrenadePolyTrailStandalone { this, wsw::StringView( "gfx/effects/grenade_poly_trail_standalone" ) };

	CachedMaterial shaderBlastCloudTrailParticle { this, wsw::StringView( "gfx/effects/blast_cloud_trail_particle" ) };
	CachedMaterial shaderBlastFireTrailParticle { this, wsw::StringView( "gfx/effects/blast_fire_trail_particle" ) };
	CachedMaterial shaderBlastPolyTrailCombined { this, wsw::StringView( "gfx/effects/blast_poly_trail_combined" ) };
	CachedMaterial shaderBlastPolyTrailStandalone { this, wsw::StringView( "gfx/effects/blast_poly_trail_standalone" ) };

	CachedMaterial shaderElectroCloudTrailParticle { this, wsw::StringView( "gfx/effects/electro_cloud_trail_particle" ) };
	CachedMaterial shaderElectroIonsTrailParticle { this, wsw::StringView( "gfx/effects/electro_ions_trail_particle" ) };
	CachedMaterial shaderElectroPolyTrail { this, wsw::StringView( "gfx/effects/electro_poly_trail" ) };
	CachedMaterial shaderElectroTrace { this, wsw::StringView( "gfx/effects/electro_trace" ) };

	CachedMaterial shaderPlasmaLingeringTrailParticle { this, wsw::StringView( "gfx/effects/pg/pg_lingering_trail_particle" ) };

	CachedMaterial shaderPlasmaTrailParticle1 { this, wsw::StringView( "gfx/effects/pg/pg_trail_particle1" ) };
	CachedMaterial shaderPlasmaTrailParticle2 { this, wsw::StringView( "gfx/effects/pg/pg_trail_particle2" ) };
	CachedMaterial shaderPlasmaTrailParticle3 { this, wsw::StringView( "gfx/effects/pg/pg_trail_particle3" ) };
	CachedMaterial shaderPlasmaTrailParticle4 { this, wsw::StringView( "gfx/effects/pg/pg_trail_particle4" ) };
	CachedMaterial shaderPlasmaTrailParticle5 { this, wsw::StringView( "gfx/effects/pg/pg_trail_particle5" ) };

	CachedMaterial shaderPlasmaPolyTrail { this, wsw::StringView( "gfx/effects/plasma_poly_trail" ) };

	CachedMaterial shaderSmokeHullSoftParticle { this, wsw::StringView( "gfx/effects/smoke_hull_soft_particle" ) };
	CachedMaterial shaderFireHullParticle { this, wsw::StringView( "gfx/effects/fire_hull_particle" ) };
	CachedMaterial shaderBlastHullParticle { this, wsw::StringView( "gfx/effects/blast_hull_particle" ) };

	CachedMaterial shaderSmokeHullHardParticle { this, wsw::StringView( "gfx/effects/smoke_hull_hard_particle" ) };

	CachedMaterial shaderBulletImpactFlare { this, wsw::StringView( "gfx/effects/bullet_impact_flare" ) };

	CachedMaterial shaderDirtImpactBurst { this, wsw::StringView( "gfx/effects/dirt_impact_burst" ) };
	CachedMaterial shaderDirtImpactParticle { this, wsw::StringView( "gfx/effects/dirt_impact_particle" ) };
	CachedMaterial shaderDirtImpactCloudSoft { this, wsw::StringView( "gfx/effects/dirt_impact_cloud_soft" ) };
	CachedMaterial shaderDirtImpactCloudHard { this, wsw::StringView( "gfx/effects/dirt_impact_cloud_hard" ) };

	CachedMaterial shaderSandImpactBurst { this, wsw::StringView( "gfx/effects/sand_impact_burst" )};

	CachedMaterial shaderLavaImpactDrop { this, wsw::StringView( "gfx/effects/lava_impact_drop" ) };

	CachedMaterial shaderLiquidImpactCloud { this, wsw::StringView( "gfx/effects/liquid_impact_cloud" ) };

	CachedMaterial shaderSandImpactDustSoft { this, wsw::StringView( "gfx/effects/sand_impact_dust_soft" ) };
	CachedMaterial shaderSandImpactDustHard { this, wsw::StringView( "gfx/effects/sand_impact_dust_hard" ) };

	CachedMaterial shaderStoneDustSoft { this, wsw::StringView( "gfx/effects/stone_dust_soft" ) };
	CachedMaterial shaderStoneDustHard { this, wsw::StringView( "gfx/effects/stone_dust_hard" ) };

	CachedMaterial shaderParticleFlare { this, wsw::StringView( "gfx/effects/particle_flare" ) };

	CachedMaterial shaderExplosionSpikeParticle { this, wsw::StringView( "gfx/effects/explosion_spike_particle" ) };
	CachedMaterial shaderExplosionSpriteParticle { this, wsw::StringView( "gfx/effects/explosion_sprite_particle" ) };

	CachedMaterial shaderSmokePolytrail { this, wsw::StringView( "gfx/effects/smokePolytrail/smoketrail0001" ) };

	CachedMaterial shaderBulletTracer { this, wsw::StringView( "gfx/effects/bullet_tracer" ) };
	CachedMaterial shaderPelletTracer { this, wsw::StringView( "gfx/effects/pellet_tracer" ) };

	CachedMaterial shaderGenericImpactRosetteSpike { this, wsw::StringView( "gfx/effects/generic_impact_spike" ) };
	CachedMaterial shaderMetalImpactRosetteInnerSpike { this, wsw::StringView( "gfx/effects/metal_impact_inner_spike" ) };
	CachedMaterial shaderMetalImpactRosetteOuterSpike { this, wsw::StringView( "gfx/effects/metal_impact_outer_spike" ) };

	CachedMaterial shaderElectroImpactParticle { this, wsw::StringView( "gfx/effects/electro_impact_particle" ) };
	CachedMaterial shaderInstaImpactParticle { this, wsw::StringView( "gfx/effects/insta_impact_particle" ) };

	CachedMaterial shaderMetalRicochetParticle { this, wsw::StringView( "gfx/effects/metal_ricochet_particle" ) };
	CachedMaterial shaderMetalDebrisParticle { this, wsw::StringView( "gfx/effects/metal_debris_particle" ) };
	CachedMaterial shaderGlassDebrisParticle { this, wsw::StringView( "gfx/effects/glass_debris_particle" ) };

	CachedMaterial shaderBladeImpactParticle { this, wsw::StringView( "gfx/effects/blade_impact_particle" ) };

	CachedMaterial shaderImpactRing { this, wsw::StringView( "gfx/misc/impact_ring" ) };

	CachedMaterial shaderNet { this, wsw::StringView( "gfx/hud/net" ) };
	CachedMaterial shaderSelect { this, wsw::StringView( "gfx/hud/select" ) };
	CachedMaterial shaderChatBalloon { this, wsw::StringView( PATH_BALLONCHAT_ICON ) };
	CachedMaterial shaderDownArrow { this, wsw::StringView( "gfx/2d/arrow_down" ) };

	CachedMaterial shaderWaterBubble { this, wsw::StringView( "gfx/misc/waterBubble" ) };
	CachedMaterial shaderSmokePuff { this, wsw::StringView( "gfx/misc/smokepuff" ) };

	CachedMaterial shaderSmokePuff1 { this, wsw::StringView( "gfx/misc/smokepuff1" ) };
	CachedMaterial shaderSmokePuff2 { this, wsw::StringView( "gfx/misc/smokepuff2" ) };
	CachedMaterial shaderSmokePuff3 { this, wsw::StringView( "gfx/misc/smokepuff3" ) };

	CachedMaterial realisticSmoke { this, wsw::StringView( "gfx/effects/realisticSmoke/smokeUp/smokeUp0001" )};

	CachedMaterial shaderCartoonHit { this, wsw::StringView( "gfx/misc/cartoonhit" ) };
	CachedMaterial shaderCartoonHit2 { this, wsw::StringView( "gfx/misc/cartoonhit2" ) };
	CachedMaterial shaderCartoonHit3 { this, wsw::StringView( "gfx/misc/cartoonhit3" ) };
	CachedMaterial shaderTeamMateIndicator { this, wsw::StringView( "gfx/indicators/teammate_indicator" ) };
	CachedMaterial shaderTeamCarrierIndicator { this, wsw::StringView( "gfx/indicators/teamcarrier_indicator" ) };
	CachedMaterial shaderTeleportShellGfx { this, wsw::StringView( "gfx/misc/teleportshell" ) };

	CachedMaterial shaderElectroBeam { this, wsw::StringView( "gfx/misc/electro" ) };
	CachedMaterial shaderElectroBeamAlpha { this, wsw::StringView( "gfx/misc/electro_alpha" ) };
	CachedMaterial shaderElectroBeamBeta { this, wsw::StringView( "gfx/misc/electro_beta" ) };
	CachedMaterial shaderWaveCorona { this, wsw::StringView( "gfx/misc/shockwave_corona" ) };
	CachedMaterial shaderWaveSparks { this, wsw::StringView( "gfx/misc/shockwave_sparks" ) };
	CachedMaterial shaderInstaBeam { this, wsw::StringView( "gfx/misc/instagun" ) };
	CachedMaterial shaderLaserGunBeam { this, wsw::StringView( "gfx/misc/laserbeam" ) };

	CachedMaterial shaderLaser { this, wsw::StringView( "gfx/misc/laser" ) };

	CachedMaterial shaderSmokeHull { this, wsw::StringView( "gfx/misc/smokehull" ) };

	CachedMaterial shaderFlagFlare { this, wsw::StringView( PATH_FLAG_FLARE_SHADER ) };

	CachedMaterial shaderRaceGhostEffect { this, wsw::StringView( "gfx/raceghost" ) };
};

// TODO: Return a java-style iterator
[[nodiscard]]
auto getRegularCrosshairFiles() -> const wsw::StringSpanStorage<unsigned, unsigned> &;

// TODO: Return a java-style iterator
[[nodiscard]]
auto getStrongCrosshairFiles() -> const wsw::StringSpanStorage<unsigned, unsigned> &;

static constexpr wsw::StringView kRegularCrosshairsDirName { "gfx/hud/crosshairs/regular" };
static constexpr wsw::StringView kStrongCrosshairsDirName { "gfx/hud/crosshairs/strong" };

template <typename Container, typename Appendable = wsw::StringView>
static void makeCrosshairFilePath( Container *container, const wsw::StringView &prefix, const wsw::StringView &fileName ) {
	container->clear();
	container->append( Appendable( prefix.data(), prefix.size() ) );
	container->append( Appendable( "/", 1 ) );
	container->append( Appendable( fileName.data(), fileName.size() ) );
	container->append( Appendable( ".svg", 4 ) );
}

// TODO: Use something else for indication of using the miniview bin?

[[nodiscard]]
auto getRegularCrosshairMaterial( const wsw::StringView &name, bool isForMiniview, unsigned size )
	-> std::optional<std::tuple<shader_s *, unsigned, unsigned>>;
[[nodiscard]]
auto getStrongCrosshairMaterial( const wsw::StringView &name, bool isForMiniview, unsigned size )
	-> std::optional<std::tuple<shader_s *, unsigned, unsigned>>;

#endif
