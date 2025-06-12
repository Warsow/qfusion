/*
Copyright (C) 1997-2001 Id Software, Inc.

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

#ifndef WSW_e72f266d_d834_4218_b703_a3bf9c96ede0_H
#define WSW_e72f266d_d834_4218_b703_a3bf9c96ede0_H

#include <common/facilities/q_comref.h>

#define MAX_MSGLEN  32768       // max length of a message, which may be fragmented into multiple packets

typedef struct msg_s {
	uint8_t *data;
	size_t maxsize;
	size_t cursize;
	size_t readcount;
	bool compressed;
} msg_t;

typedef struct msg_field_s {
	int offset;
	int bits;
	int count;
	wireType_t encoding;
} msg_field_t;

// msg.c
void MSG_Init( msg_t *buf, uint8_t *data, size_t length );
void MSG_Clear( msg_t *buf );
void *MSG_GetSpace( msg_t *buf, size_t length );
void MSG_WriteData( msg_t *msg, const void *data, size_t length );
void MSG_CopyData( msg_t *buf, const void *data, size_t length );
int MSG_SkipData( msg_t *sb, size_t length );

//============================================================================

struct usercmd_s;
struct entity_state_s;

void MSG_WriteInt8( msg_t *sb, int c );
void MSG_WriteUint8( msg_t *sb, int c );
void MSG_WriteInt16( msg_t *sb, int c );
void MSG_WriteUint16( msg_t *sb, unsigned c );
void MSG_WriteInt32( msg_t *sb, int c );
void MSG_WriteInt64( msg_t *sb, int64_t c );
void MSG_WriteUintBase128( msg_t *msg, uint64_t c );
void MSG_WriteIntBase128( msg_t *msg, int64_t c );
void MSG_WriteFloat( msg_t *sb, float f );
void MSG_WriteHalfFloat( msg_t *sb, float f );
void MSG_WriteString( msg_t *sb, const char *s );
#define MSG_WriteAngle16( sb, f ) ( MSG_WriteInt16( ( sb ), ANGLE2SHORT( ( f ) ) ) )
void MSG_WriteDeltaUsercmd( msg_t *sb, const struct usercmd_s *from, struct usercmd_s *cmd );
void MSG_WriteDeltaEntity( msg_t *msg, const struct entity_state_s *from, const struct entity_state_s *to, bool force );
void MSG_WriteDeltaPlayerState( msg_t *msg, const player_state_t *ops, const player_state_t *ps );
void MSG_WriteDeltaGameState( msg_t *msg, const game_state_t *from, const game_state_t *to );
void MSG_WriteDeltaScoreboardData( msg_t *msg, const ReplicatedScoreboardData *from, const ReplicatedScoreboardData *to );
void MSG_WriteDir( msg_t *sb, float *vector );
void MSG_WriteDeltaStruct( msg_t *msg, const void *from, const void *to, const msg_field_t *fields, size_t numFields );

void MSG_BeginReading( msg_t *sb );
int MSG_ReadInt8( msg_t *msg );
int MSG_ReadUint8( msg_t *msg );
int16_t MSG_ReadInt16( msg_t *sb );
uint16_t MSG_ReadUint16( msg_t *sb );
int MSG_ReadInt32( msg_t *sb );
int64_t MSG_ReadInt64( msg_t *sb );
uint64_t MSG_ReadUintBase128( msg_t *msg );
int64_t MSG_ReadIntBase128( msg_t *msg );
float MSG_ReadFloat( msg_t *sb );
float MSG_ReadHalfFloat( msg_t *sb );
char *MSG_ReadString( msg_t *sb );
char *MSG_ReadStringLine( msg_t *sb );
#define MSG_ReadAngle16( sb ) ( SHORT2ANGLE( MSG_ReadInt16( ( sb ) ) ) )
void MSG_ReadDeltaUsercmd( msg_t *sb, const struct usercmd_s *from, struct usercmd_s *cmd );
int MSG_ReadEntityNumber( msg_t *msg, bool *remove, unsigned *byteMask );
void MSG_ReadDeltaEntity( msg_t *msg, const entity_state_t *from, entity_state_t *to, int number, unsigned byteMask );
void MSG_ReadDeltaPlayerState( msg_t *msg, const player_state_t *ops, player_state_t *ps );
void MSG_ReadDeltaGameState( msg_t *msg, const game_state_t *from, game_state_t *to );
void MSG_ReadDeltaScoreboardData( msg_t *msg, const ReplicatedScoreboardData *from, ReplicatedScoreboardData *to );
void MSG_ReadDir( msg_t *sb, float *vector );
void MSG_ReadData( msg_t *sb, void *buffer, size_t length );
void MSG_ReadDeltaStruct( msg_t *msg, const void *from, void *to, size_t size, const msg_field_t *fields, size_t numFields );
int MSG_BytesLeft( const msg_t *msg );
//============================================================================

#endif