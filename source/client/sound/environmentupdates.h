/*
Copyright (C) 2017-2026 vvk2212, Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef WSW_58984c74_c57e_4979_a7ce_c56b7e3a8894_H
#define WSW_58984c74_c57e_4979_a7ce_c56b7e3a8894_H

#include <common/helpers/q_math.h>

struct Source;
struct PanningUpdateState;

void updateEnvOfListenerAndSources( Source *sourceListHead, int64_t millisNow, int entNum,
									const vec3_t origin, const vec3_t velocity, const mat3_t axes );

void calcReverbPan( const vec3_t listenerOrigin, const mat3_t listenerAxes,
					const PanningUpdateState *updateState, vec3_t earlyPan, vec3_t latePan );

void calcPropagationOrigin( const vec3_t listenerOrigin, const vec3_t realSourceOrigin,
							float *sourcePitchScale, vec3_t sourceOriginToUse );

void setupSourceEffectsAndEnvUpdates( Source *src );

void disableSourceEffectsAndEnvUpdates( Source *src );

#endif
