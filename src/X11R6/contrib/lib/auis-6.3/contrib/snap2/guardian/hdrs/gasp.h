
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

	Constants used in the SNAP Guardian-Server &
	Guardian-Client interfaces.


Created: 10/22/85

*/

/* Return values from SNAPBeginConv to client */

    /* Unused */
#define GASP_UNKNOWN_REQUEST	1
    /* Unknown service */
#define GASP_UNKNOWN_SERVICE	2
    /* For some unknown reason, creation of server failed */
#define GASP_CANT_MAKE_SERVER	3
    /* No more clients can be accepted by server */
#define GASP_TOO_MANY_CLIENTS	4
    /* Authentication failed */
#define GASP_NO_AUTHORIZATION	5
    /* No more servers can be created */
#define GASP_TOO_MANY_SERVERS	6
    /* Unused */
#define GASP_NO_ADDRESSES	7
    /* Pipe sys call failed */
#define GASP_NO_PIPE		8
    /* Fork sys call failed */
#define GASP_NO_FORK		9
    /* Bind sys call failed */
#define GASP_NO_PORT		10
    /* Client name is too long */
#define GASP_CLIENT_TOO_LONG	11
    /* Guardian is not accepting remote requests */
#define GASP_NO_REMOTE_REQUESTS 12
    /* Server is not accepting unauthenticated connections */
#define GASP_NO_UNAUTH_CONNS	13
    /* Setuid sys call failed */
#define GASP_CANT_SETUID	14
    /* A temporary failure due to a file server down occurred */
#define GASP_TEMP_FAIL		15
    /* The format of the authentication string was bad */
#define GASP_AUTH_FORMAT	16
    /* RPC2/R initialization failed */
#define GASP_RPC_INIT		17
    /* RPC2 authentication returned an impossible value */
#define GASP_RPC2_RETURN	18
    /* User is not permitted to use this machine by user.permits */
#define GASP_USER_PERMITS	19
    /* Can't find user in /etc/passwd */
#define GASP_CANT_FIND_USER	20
    /* Authentication mismatch on existing connection */
#define GASP_MISMATCH		21
    /* No guest acct on cell */
#define GASP_NO_GUEST_ACCT	22
    /* Type of authentication not compiled */
#define GASP_AUTH_NOT_COMPILED	23
    /* Cell auth requested & can't find cell */
#define GASP_NO_CELL            24

/* Return values from GASP_ServerInit */
#define GASP_BAD_ARGS		1
#define GASP_NO_KEY		2
#define GASP_NO_GUARDIAN	3
#define GASP_NO_HOSTNAME	4
#define GASP_NO_HOST_ENTRY	5

/* Request types */
#define COUNT_RQ		1

typedef struct {
    unsigned type:32;	  /* Request type */
    unsigned count:32;
} SERVER_PACKET;

#define SERVER_PACKET_SIZE	8

/* Password/authentication stuff */

/* GASP password types */

    /* Null-terminated password */
#define GASP_PWD_STRING 	1
    /* Venus tokens & uid */
#define GASP_PWD_VTOKENS	2
    /* Uid, null-terminated password & cell */
#define GASP_PWD_CELL		3
    /* Set of (uid, tokens, cell) */
#define GASP_PWD_MULTI_TOKENS	4
