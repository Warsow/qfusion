#ifndef WSW_0466bd6c_f805_44e2_8c02_6483b82a1ff7_H
#define WSW_0466bd6c_f805_44e2_8c02_6483b82a1ff7_H

// TODO: Lift it to the top level
#include <server/game/ai/vec3.h>

[[nodiscard]]
auto calcSimplexNoise2D( float x, float y ) -> float;
[[nodiscard]]
auto calcSimplexNoise3D( float x, float y, float z ) -> float;
[[nodiscard]]
auto calcSimplexNoiseCurl( float x, float y, float z ) -> Vec3;

[[nodiscard]]
auto calcVoronoiNoiseSquared( float x, float y, float z ) -> float;
[[nodiscard]]
auto calcVoronoiNoiseLinear( float x, float y, float z ) -> float;

#endif
