/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef PHYSDEVICE_H
#define PHYSDEVICE_H

#ifndef _AUDIOMASTER_H_
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "audiomaster.h"
#endif

#include <server/include/audiodev.h>

#define	MAXPHYSDEVICES	3
#define	DEFAULT_AUDIO_DEVICE	"alphie"

/*
  The pExchange structure is used to keep track of packet exchanges:  what's
  outstanding, what needs to be retried, etc.
*/
typedef struct {
	int	retry;		/* retry count */
	int	len;		/* length of audio command's data */
	struct audio_command *packet;
} pExchange;

typedef pExchange	*pExchangePtr;

#define EXCHANGESIZE	100			/* size of exchange list */

/*
  LineServer physical device description.
*/
typedef struct {
	AS		*slave;
	StateType	state;
	struct timeval	wstime;			/* last workstation time */
	ATime		lstime;			/* last LineServer time */
	ATime		lastPotentialUpdate;	/* as it says... */
	ATime		endPlayTime;		/* start of backfill area */
	unsigned char	map_mmr1;		/* shadow copies of codec */
	unsigned char	map_mmr2;
	unsigned char	map_mmr3;
	int		ogain;			/* output gain */
	pExchangePtr	elist;			/* packet exchange list */
} lsPhysDevice;

#define UPDATE_INTERVAL	100			/* housekeeping interval */
#define UPDATE_SIZE	1300			/* size of updates */

#define MAX_SIZE	1300			/* max data size of packet */

#endif
