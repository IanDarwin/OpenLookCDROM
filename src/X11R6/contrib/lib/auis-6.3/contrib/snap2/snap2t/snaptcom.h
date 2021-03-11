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


/* Debugging masks used within SNAP DBGMSGs */

#define NCR 0x8000  /* No carriage return   */
#define NPR 0x4000  /* No prefix	    */
#define PCK 0x2000  /* Pack /Unpack	    */
#define ACK 0x1000  /* Ack handling	    */
#define RCV 0x0800  /* Receive		    */
#define SND 0x0400  /* Send		    */
#define PKQ 0x0200  /* Packet queue	    */
#define MSG 0x0100  /* Msg encrypt/checksum */
#define ACC 0x0080  /* Accept		    */
#define REP 0x0040  /* Reply		    */
#define SUT 0x0020  /* Setup / takedown     */
#define CTL 0x0010  /* Control blocks	    */
#define BEG 0x0008  /* Begin Conversation   */
#define SWR 0x0004  /* Send with reply	    */
#define SNR 0x0002  /* Send no reply	    */
#define END 0x0001  /* End conversation     */

#include <amsenv.h>

#define PRIVATE static
#ifdef DEBUG
PRIVATE char snapid[] = "[-SNAP-] ";
char *SNAP_Ptr();
#define DBGMSG(mask,prtargs)\
	if ((mask)& (~(NCR|NPR)) & SNAP_debugmask)\
	    {if ((mask) & NPR); else printf("%s", snapid);\
	    printf prtargs;if ((mask) & NCR); else printf("\n");\
	    fflush(stdout);}\
	else
#else /* DEBUG */
#define DBGMSG(mask,prtargs)
#endif /* DEBUG */
#define PTR(x) SNAP_Ptr(sizeof((x)),(char *)(x))

extern int errno;
#ifndef EXTERN
#define EXTERN extern
#endif /* EXTERN */
EXTERN int SNAP_debugmask;

/* 1st byte of SNAP's Standard Header has the following flags: */
#define ACCEPT_SEQNO	0x80
#define ENCRYPTED_MSG	0x40
#define MANDATORY_ACK	0x20

#define NETADDR 	    struct sockaddr_in
#define BITSPERBYTE	    8
#define MAXSHORT	    0xFFFF

#ifndef NO
#define NO		    0
#define YES		    1
#endif

#ifndef TRUE
#define TRUE		    1
#define FALSE		    0
#endif

#define SYSTEMERROR(errcode) ((errcode)<0 ? (errcode) : 0-(errcode))

#define SNAP_HDRLENGTH	    (4*sizeof(SNAP_integer))
#define FRAG_HDRLENGTH	    (2*sizeof(SNAP_integer))

#define MAXWINDOW		10
#define MAJOR_VERSION		2
#define MINOR_VERSION		5
#ifdef IBMPC
#define CLIENT_WINDOW		3
#else /* IBMPC */
#define CLIENT_WINDOW		3
#endif /* IBMPC */
#define SERVER_WINDOW		3
#define RECEIVEFRAG_TRIES	1
#define WAITFORACK_TRIES	1

#define SERVER_MAXFRAG		1400
#define FRAG_NOT_RECEIVED	0
#define FRAG_RECEIVED		1

#ifdef IBMPC
#define CLIENT_MAXFRAG		1024
#define FARCHAR char far
#else /* IBMPC */
#ifdef MAC
#define CLIENT_MAXFRAG		512
#define FARCHAR char
#else /* MAC */
#define CLIENT_MAXFRAG		1024
#define FARCHAR char
#endif /* MAC */
#endif /* IBMPC */

#ifdef SNAPSCRAM
PRIVATE scramble (str, len)
char *str;
int len;
    {
    int i;
    for (i=0;i<len;i++)
	str[i] = ((~str[i]) ^ (0x93+i)) & 0xFF;
    }
#endif /* SNAPSCRAM */

#if (CLIENT_WINDOW > MAXWINDOW)
    Error: Inconsistant window size specification;
#endif /* (CLIENT_WINDOW > MAXWINDOW) */
#if (SERVER_WINDOW > MAXWINDOW)
    Error: inconsistant window size specification;
#endif /* (SERVER_WINDOW > MAXWINDOW) */

#define GUARDIAN_MAXFRAG    500

