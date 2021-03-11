/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

/*
		dropoff.h -- Include file for using dropoff subroutines
*/
#ifndef	_ANDREWENV_
#include <andyenv.h>
#endif /* _ANDREWENV_ */

/* Error returns from dropoff routine */

#define D_OK		1
#define D_OK_WARN	2
#define D_LOCALQ	3
#define D_CANT_QUEUE	4
#define D_BAD_PARMS	5
#define D_TEMP_FAIL	6
#define D_BAD_MESGFILE	7
#define D_OSERR		8

/* Flags for dropoff */
#define DF_NOLOCALQ	    (1<<0)	/* Don't allow local disk queueing */
#define DF_NOBLIP	    (1<<1)	/* Don't alert local daemon */
#define DF_NOLOCALDELIVERY  (1<<2)	/* Just go to global queues */
#define DF_FORCE	    (1<<3)	/* Force daemon to deliver now */
#define DF_ANDY	    (1<<4)	/* Use experimental service. */

#define DF_NEW_FMT	    (1<<30)	/* This is a new-format packet. */

extern char Dropoff_ErrMsg[];

/* Dropoff types; test_dropoff() returns one of these. */
#define DT_UNK		-1		/* Not yet known how to deliver mail */
#define DT_AMS		0		/* Will use standard AMS delivery dropoff */
#define DT_CANNOT	1		/* Dropoff can't work (want to use AMS del but that's not supported on this workstation */
#define DT_NONAMS	2		/* Will use OldSendmailProgram dropoff */
#define DT_AMSWAIT	3		/* Will use standard AMS delivery, but will not presume the existence of a queuemail daemon to poke.  Mail will probably sit in ~/.Outgoing for a while. */

#ifdef _DROPOFF_SYS
/*	The following stuff is only for implementing parts of the
	dropoff system.  To get this stuff, define the symbol
	_DROPOFF_SYS

There are three historical formats for packets sent from the dropoff library routine to the dropoff-daemon process (currently part of queuemail).  The first two formats include a workstation UID and a single pair of packed authentication tokens, as handled by the tokens.c module.  The second format also includes 32 bits of flags.  The third format includes the flags word, a workstation UID, a count of pairs of tokens following, and a directory (e.g. $HOME/.Outgoing) in which enqueued requests can be found.  Each packed pair of token is followed by a null-terminated text string of which the first byte is a flag: 2 if this isn't a primary token and 3 if it is.  The rest of the text string is the name of the cell to which the pair of tokens applies.

Thus, the first packet format, in byte addresses, is
0-3/	uid	(32 bits, network byte order)
4-.../	pair of Venus tokens, packed by tokens.c
Its length is DROPOFF_PACKET_LENGTH.

The second packet format, in byte addresses, is
0-3/	flags	(32 bits, network byte order)
4-7/	uid	(32 bits, network byte order)
8-.../	pair of Venus tokens, packed by tokens.c
Its length is DROPOFF_PKT_LEN.

The third packet format, in byte addresses, is
0-3/	flags	(32 bits, network byte order, with DF_NEW_FMT on)
4-7/	uid	(32 bits, network byte order)
8-11/	numpairs	(32 bits, network byte order)
12-.../	null-terminated string naming the directory to process
then a sequence of:
	pair of Venus tokens, packed by tokens.c
	null-terminated string naming first whether this is the primary token, then the cell to which this pair of tokens applies
until the packet runs out.
Thus, the length of this packet cannot be determined ahead of time.

*/

#ifdef AMS_DELIVERY_ENV

/* Room for a packet */
#define DROPOFF_PACKET_LENGTH	(TOKENS_LEN + 4)
#define DROPOFF_PKT_LEN		(TOKENS_LEN + 8)

#endif /* AMS_DELIVERY_ENV */

#endif /* _DROPOFF_SYS */
