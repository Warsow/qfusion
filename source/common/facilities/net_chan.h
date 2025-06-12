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

#ifndef WSW_a35bb5dd_b5c9_4eed_80bf_c76216d8f97e_H
#define WSW_a35bb5dd_b5c9_4eed_80bf_c76216d8f97e_H

#include <common/facilities/msg.h>
#include <common/facilities/net.h>

typedef struct {
	const socket_t *socket;

	int dropped;                // between last packet and previous

	netadr_t remoteAddress;
	int game_port;              // game port value to write when transmitting

	// sequencing variables
	int incomingSequence;
	int incoming_acknowledged;
	int outgoingSequence;

	// incoming fragment assembly buffer
	int fragmentSequence;
	size_t fragmentLength;
	uint8_t fragmentBuffer[MAX_MSGLEN];

	// outgoing fragment buffer
	// we need to space out the sending of large fragmented messages
	bool unsentFragments;
	size_t unsentFragmentStart;
	size_t unsentLength;
	uint8_t unsentBuffer[MAX_MSGLEN];
	bool unsentIsCompressed;

	bool fatal_error;
} netchan_t;

void Netchan_Init( void );
void Netchan_Shutdown( void );
void Netchan_Setup( netchan_t *chan, const socket_t *socket, const netadr_t *address, int qport );
bool Netchan_Process( netchan_t *chan, msg_t *msg );
bool Netchan_Transmit( netchan_t *chan, msg_t *msg );
bool Netchan_PushAllFragments( netchan_t *chan );
bool Netchan_TransmitNextFragment( netchan_t *chan );
int Netchan_CompressMessage( msg_t *msg, uint8_t tmpBuffer[MAX_MSGLEN] );
int Netchan_DecompressMessage( msg_t *msg, uint8_t tmpBuffer[MAX_MSGLEN] );
void Netchan_OutOfBand( const socket_t *socket, const netadr_t *address, size_t length, const uint8_t *data );

#ifndef _MSC_VER
void Netchan_OutOfBandPrint( const socket_t *socket, const netadr_t *address, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
void Netchan_OutOfBandPrint( const socket_t *socket, const netadr_t *address, _Printf_format_string_ const char *format, ... );
#endif

int Netchan_GamePort( void );


#endif