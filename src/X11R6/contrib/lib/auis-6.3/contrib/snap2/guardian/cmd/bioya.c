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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/guardian/cmd/RCS/bioya.c,v 2.8 1993/05/04 01:49:01 susan Exp $";
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/time.h */
#include <netinet/in.h>
#include <gasp.h>
#include <snap.h>

extern int SNAP_debugmask;

main(argc, argv)
    int argc;
    char *argv[];
{
    char *client, *buffer;
    int clientfd;
    SNAP_CPARMS connparms;
    int auth;

    puts("[Starting BIOYA test server]\n");

    /* SNAP_debugmask = 24279; */

    connparms.maxtime = 40;
    connparms.timeout = 1;
    connparms.maxmsgsize = 10000;
    connparms.encryptlevel = SNAP_ENCRYPT;
    printf ("[BIOYA] Return code from GASP_ServerInit was %d\n",
	GASP_ServerInit(argc, argv, &connparms, &client, &clientfd, &auth));
    printf("[BIOYA] Client is %s, authenticated: %d\n", client, auth);

    for (;;) {
	int rfd, msgtype, cid, len;
	struct timeval timeout;

	rfd = 1 << clientfd;
	timeout.tv_sec = 60;
	timeout.tv_usec = 0;
	if (!SNAP_MsgPending()) {
	    printf("[BIOYA] Starting select...");
	    fflush(stdout);
	    if (select(32, &rfd, 0, 0, &timeout) < 0) {
		perror("SELECT failed");
		exit(3);
	    }
	    printf("rfd=0x%x\n", rfd);
	}
	else
	    printf ("[BIOYA] Msg pending for accept...");

	if (SNAP_MsgPending() || (rfd & (1 << clientfd))) {
	    len = SNAP_Accept(&buffer, &msgtype, &cid, 0);
	    printf ("[BIOYA] Accept: %d bytes, type %d, id %d\n",
		    len, msgtype, cid);
	    execute(buffer, len, msgtype, cid);
	}

	GASP_Count(SNAP_ConvCount());
    }
}

#define PAD(n)	(((n)-1 | 3) + 1)

static int getint(ptr)
    char **ptr;
{
    int val;

    val = ntohl(*(int *)*ptr);
    *ptr += 4;
    return val;
}

static char *putint(ptr, val)
    char *ptr;
    int val;
{
    * (int *) ptr = htonl(val);
    return ptr + 4;
}

extern char *SNAP_AppendStringToMsg(), *SNAP_ExtractStringFromMsg();

#define putstr	SNAP_AppendStringToMsg
#define getstr	SNAP_ExtractStringFromMsg

static execute(buffer, len, type, cid)
    char *buffer;
    int len;
    int type, cid;
{
    char reply[1024];
    char *ptr, *orig_buffer;
    int op, arg, rc;
    register int i;

    orig_buffer = buffer;
    printf("[BIOYA] Buffer: ");
    if (len < 200) {
	for (i=0; i<len; i++) printf("%02x", (unsigned char) buffer[i]);
	putchar('\n');
    }

    op = getint(&buffer);
    switch (op) {
	case 1: 	/* Return argument */
			arg = getint(&buffer);
			printf("[BIOYA] Return argument: %d\n", arg);
			if (type == SNAP_SENDWITHREPLY) {
			    putint(reply, arg);
			    SNAP_Reply(reply, sizeof(int), cid);
			}
			break;
	case 2: 	/* Square argument */
			arg = getint(&buffer);
			printf("[BIOYA] Square argument: %d\n", arg);
			if (type == SNAP_SENDWITHREPLY) {
			    putint(reply, arg*arg);
			    SNAP_Reply(reply, sizeof(int), cid);
			}
			break;
	case 3: 	/* Return my name */
			puts("[BIOYA] Return my name");
			if (type == SNAP_SENDWITHREPLY) {
			    ptr = putstr(reply, "BIOYA");
			    SNAP_Reply(reply, ptr-reply, cid);
			}
			break;
	case 4: 	/* Exit */
			printf("[BIOYA] GASP_ServerTerm returned %d\n", GASP_ServerTerm());
			exit(1);
			break;
	case 5: 	/* Receive null pointer to string */
			getstr(buffer, &ptr);
			printf("[BIOYA] Null ptr: 0x%x\n", ptr);
			if (type == SNAP_SENDWITHREPLY) {
			    ptr = putstr(reply, 0);
			    SNAP_Reply(reply, ptr-reply, cid);
			}
			break;
	case 6: 	/* Receive empty string */
			getstr(buffer, &ptr);
			printf("[BIOYA] Empty string: \"%s\"\n", ptr);
			if (type == SNAP_SENDWITHREPLY) {
			    ptr = putstr(reply, "");
			    SNAP_Reply(reply, ptr-reply, cid);
			}
			break;
	case 7: 	/* Echo */
			if (type != SNAP_SENDWITHREPLY) {
			    printf("[BIOYA] no reply requested for echo!\n");
			}
			printf("[BIOYA] echo - length %d\n", len);
			rc = SNAP_Reply(orig_buffer, len, cid);
			printf("[BIOYA] rc from reply: %d\n", rc);
			break;
	default:	printf("[BIOYA] Unknown opcode: %d\n", op);
    }
}
