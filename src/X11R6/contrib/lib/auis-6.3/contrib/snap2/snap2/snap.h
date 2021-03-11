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


typedef struct {
    int maxtime;		/* maximum elapsed time for a SNAP request */
    int timeout;		/* timeout interval in seconds */
    int maxmsgsize;		/* largest message in bytes (server only) */

/* Note that the maxmsgsize field (which is otherwise unused on the client
 * side of a connection) is used to pass back error codes for guardian-detected
 * failures during SNAP_BeginConv.  The following synonym is provided for
 * this purpose.
 */
#define guardian_rc maxmsgsize

    int encryptlevel;		/* encryption level */
#define SNAP_NOENCRYPT	0
#define SNAP_ENCRYPT	1

    } SNAP_CPARMS;

#define SNAP_integer	    long

#define SNAP_SUCCESS	    0
#define SNAP_FAIL	    (-1)
#define SNAP_NOMORECONVS    (1002*SNAP_FAIL)
#define SNAP_NOMEMORY	    (1003*SNAP_FAIL)
#define SNAP_BADPARMS	    (1004*SNAP_FAIL)
#define SNAP_NOMORERETRIES  (1005*SNAP_FAIL)
#define SNAP_XMITFAIL	    (1006*SNAP_FAIL)
#define SNAP_INVALIDOP	    (1007*SNAP_FAIL)
#define SNAP_INVALIDCID     (1008*SNAP_FAIL)
#define SNAP_BUFFERLIMIT    (1009*SNAP_FAIL)
#define SNAP_SOCKETERROR    (1010*SNAP_FAIL)
#define SNAP_NOINIT	    (1011*SNAP_FAIL)
#define SNAP_NOAUTHENTICATE (1012*SNAP_FAIL)

/* For compatibility with existing programs the following error
 * is defined as a synonym for SNAP_NOAUTHENTICATE.  Whenever this
 * error code is returned from SNAP_BeginConv, a detailed guardian
 * error code is available in the guardian_rc field of the SNAP_CPARMS
 * structure.
 */
#define SNAP_GUARDIAN_ERROR (1012*SNAP_FAIL)

#define SNAP_NOREPLYDUE     (1013*SNAP_FAIL)
#define SNAP_SERVERREJECT   (1014*SNAP_FAIL)
#define SNAP_TIMEOUT	    (1015*SNAP_FAIL)
#define SNAP_INVALIDACK     (1016*SNAP_FAIL)
#define SNAP_WRONGVERSION   (1017*SNAP_FAIL)
#define SNAP_SELECTFAILED   (1018*SNAP_FAIL)
#define SNAP_RESEND	    (1019*SNAP_FAIL)
#define SNAP_SERVERDIED     (1020*SNAP_FAIL)

/* The following error is only returned by the IBM PC SNAP implementation */

#define SNAP_ABORTED	    (2000*SNAP_FAIL)

/* Op codes in SNAP packets */

#define SNAP_SENDWITHREPLY  1
#define SNAP_SENDNOREPLY    2
#define SNAP_BEGINCONV	    3
#define SNAP_ENDCONV	    4
#define SNAP_REPLY	    5
#define SNAP_AUTHENTICATE   6
#define SNAP_ACK	    7
#define SNAP_NOTIFY	    8

/* Notify type codes */

#define SNAP_SERVERGONE     1
#define SNAP_BROADCAST	    2

#define SNAP_KEYLEN	    48

char *SNAP_AppendIntToMsg ();
char *SNAP_ExtractIntFromMsg ();
char *SNAP_AppendStringToMsg ();
char *SNAP_ExtractStringFromMsg ();
char *SNAP_AppendBytesToMsg ();
char *SNAP_ExtractBytesFromMsg ();
