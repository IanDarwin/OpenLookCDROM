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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/loadserv/RCS/snaperr.c,v 2.7 1992/12/15 21:05:43 rr2b R6tape $";
#endif

/*
  snaperr.c - return a next message to go with a snap error code
*/
#include <snap.h>

#define oSNAP_FIRST_ERR (SNAP_NOMORECONVS)

char *osnap_errlist[] = {
    "Too many connections active, run login and close some",
    "Not enough memory",
    "Did you spell the hostname correctly? OR server authenticate problem",
    "Internal error:Residual error code -- should not happen",
    "Can't talk to guardian server, perhaps it is dead?",
    "Internal error:Invalid op code to SNAP call",
    "Internal error:Invalid conversation id in SNAP call",
    "Internal error:SNAP buffer limit exceeded",
    "Internal error:SNAP was unable to create a socket",
    "Internal error:SNAP not initialized",
    "Host does not support loadserv or authentication failed",
    "Internal error:Unsolicited SNAP reply",
    "Server refused our connection",
    "Server timed out",
    "Internal SNAP protocol error",
    "SNAP version mismatch, do you have the most recent version?",
    "Internal error:SNAP select statement failed or was interrupted",
    "Things are weird; trying again might work",
    "Loadserv is dead",
};

int osnap_nerr = (sizeof osnap_errlist / sizeof osnap_errlist[0]);

#define SNAP_BADPARMS	    (1004*SNAP_FAIL)

char *oSNAP_ernum_to_str(aerr)
int aerr;
{
    int aindex;
    if(aerr==SNAP_ABORTED)return("Aborted by user ctl-break");
    aindex = oSNAP_FIRST_ERR - aerr;
    if((aindex >= 0)&&(aindex < osnap_nerr))
	return(osnap_errlist[aindex]);
    return 0;
}

char *ogasp_errlist[] = {
    "Impossible error 0",
    "Unknown service",
    "For some unknown reason, creation of server failed",
    "No more clients can be accepted by server",
    "Authentication failed",
    "No more servers can be created",
    "Unused",
    "Pipe sys call failed",
    "Fork sys call failed",
    "Bind sys call failed",
    "Client name is too long",
    "Guardian is not accepting remote requests",
    "Server is not accepting unauthenticated connections",
    "Setuid sys call failed",
    "A temporary failure due to a file server down occurred",
    "The format of the authentication string was bad",
    "RPC2/R initialization failed",
    "RPC2 authentication returned an impossible value",
    "User is not permitted to use this machine by user.permits",
    "Can't find user in /etc/passwd",
    "Authentication mismatch on existing connection",
    "No guest acct on cell",
    "Type of authentication not compiled",
    "Cell auth requested & can't find cell",
};

int ogasp_nerr = (sizeof ogasp_errlist / sizeof ogasp_errlist[0]);

char *oGASP_ernum_to_str(aerr)
int aerr;
{
    int aindex;
    aindex = aerr;
    if((aindex >= 0)&&(aindex < ogasp_nerr))
	return(ogasp_errlist[aindex]);
    return 0;
}
