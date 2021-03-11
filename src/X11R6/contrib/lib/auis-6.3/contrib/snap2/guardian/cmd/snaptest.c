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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/guardian/cmd/RCS/snaptest.c,v 2.7 1992/12/15 21:05:43 rr2b R6tape $";
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/types.h */
#include <netinet/in.h>

#include <snap.h>
#include <gasp.h>

extern int SNAP_debugmask;
int errorcnt;

extern char *SNAP_AppendStringToMsg();
#define putstr	SNAP_AppendStringToMsg

#define NIL 0

/*
	snaptest host #iterations [ user [ passwd [ cell [ debug ] ] ] ]
*/

main (argc,argv)
int argc;
char **argv;
{
    extern long time();
    SNAP_CPARMS bcparms;
    SNAP_CPARMS cparms;
#ifdef AFS_ENV
    int dummy, rc;
    char *tokens;
#endif /* AFS_ENV */
    int cid, iters, len, type;
    register char *hostname, *user, *pw, *cell;
    static char longmsg[1000];

    if (argc < 3 || argc > 7) {
	puts("[SNAPTEST] Bad args");
	exit(1);
    }
    hostname = argv[1];
    iters = atoi(argv[2]);
    user = (argc >= 4 ? argv[3] : "userx");
    pw	 = (argc >= 5 ? argv[4] : "");
    cell = (argc >= 6 ? argv[5] : NIL);

    SNAP_debugmask = ((argc >= 6) ? atoi(argv[5]) : 0);
    errorcnt = 0;

    cparms.maxtime = 180;
    cparms.timeout = 1;
    cparms.encryptlevel = SNAP_ENCRYPT;
    bcparms.maxtime = 180;
    bcparms.timeout = 5;
    bcparms.encryptlevel = SNAP_ENCRYPT;

    printf("[SNAPTEST] ClientInit => %d\n", SNAP_ClientInit());

    type = 0;

    /* Magic new hack for cell name */
    strcpy(longmsg, pw);
    len = strlen(pw)+1;
    if (cell != NIL) {
	strcpy(longmsg+len, cell);
	len += strlen(cell)+1;
	type = GASP_PWD_CELL;
    }

#ifdef AFS_ENV
    /* New hack for multi-tokens */
    if (strcmp(pw, "-m") == 0) {
	rc = GetAndPackAllTokens(&tokens, &len, &dummy, 0 /*debug*/);
	if (rc <= 0) {
	    printf("GetAndPackAllTokens failed: %d\n", rc);
		SNAP_SocketTerm();
	    exit(1);
	}
	bcopy(tokens, longmsg, len);
	type = GASP_PWD_MULTI_TOKENS;
    }
#endif /* AFS_ENV */

    cid = SNAP_BeginConv("snap.guardian",
			 hostname,
			 "BIOYA",
			 user, longmsg, len, type,
			 &bcparms);
    printf("[SNAPTEST] BegConv => %d (guardian: %d)\n", cid, bcparms.guardian_rc);
    if (cid != SNAP_SUCCESS) {
	SNAP_SocketTerm();
	exit(1);
    }
    SNAP_SetConvParms(cid, &cparms);

    /* Make calls */
    printf("[SNAPTEST] Doing %d iterations\n", iters);
    srandom((int) time(0));
    for (; iters>0; iters--) {
	int proc, time;
	extern long random();

     	time = (rand() % 15) + 1;
	printf("[SNAPTEST] Sleeping for %d seconds...", time);
	fflush(stdout);
	sleep(time);
	putchar('\n');

	proc = (random() % 9) + 1;

	switch (proc) {
	    case 1:	call1(cid, rand());
			break;
	    case 2:	call2(cid, rand()%10000);
			break;
	    case 3:	call3(cid);
			break;
	    case 4:	call7(cid);
			break;
	    case 5:	call5(cid);
			break;
	    case 6:	call6(cid);
			break;
	    case 7:	call7(cid);
			break;
	    case 8:	call7(cid);
			break;
	    case 9:	call7(cid);
			break;
	    default:	printf("[SNAPTEST] Impossible proc: %d\n", proc);
	}
    }

    puts("[SNAPTEST]: Sending termination request");
    call4(cid);
    printf("[SNAPTEST] Total errors: %d\n", errorcnt);
    printf("[SNAPTEST] EndConv => %d\n", SNAP_EndConv(cid,"",0,NULL));
    printf("[SNAPTEST] ClientTerm => %d\n", SNAP_ClientTerm());
    SNAP_SocketTerm();
}

#define PAD(n)	(((n)-1 | 3) + 1)

static char *putint(ptr, val)
    char *ptr;
    int val;
{
    * (long *) ptr = htonl((long) val);
    return ptr + 4;
}

static int getint(ptr)
    char **ptr;
{
    long val;

    val = ntohl(* (long *) *ptr);
    *ptr += 4;
    return ((int) val);
}

extern char *SNAP_ExtractStringFromMsg();
#define getstr	SNAP_ExtractStringFromMsg

static call1(cid, arg)
    int cid, arg;
{
    char send[8], reply[4], *ptr;
    int rc;

    printf("[SNAPTEST] Call 1, cid %d, arg %d...", cid, arg);
    fflush(stdout);
    ptr = putint(send, 1);
    ptr = putint(ptr, arg);
    rc = SNAP_SendWithReply(cid, send, 8, reply, sizeof reply, NULL);
    if (rc != 4) {
	printf("bad reply, length %d\n", rc);
	errorcnt++;
	return;
    }
    ptr = reply;
    rc = getint(&ptr);
    if (rc == arg)
	printf("%d, %s\n", rc, "GOOD");
    else {
	printf("%d, %s\n", rc, "BAD");
	errorcnt++;
	}
}

static call2(cid, arg)
    int cid, arg;
{
    char send[8], reply[4], *ptr;
    int rc;

    printf("[SNAPTEST] Call 2, cid %d, arg %d...", cid, arg);
    fflush(stdout);
    ptr = putint(send, 2);
    ptr = putint(ptr, arg);
    rc = SNAP_SendWithReply(cid, send, 8, reply, sizeof reply, NULL);
    if (rc != 4) {
	printf("bad reply, length %d\n", rc);
	errorcnt++;
	return;
    }
    ptr = reply;
    rc = getint(&ptr);
    if (rc == arg*arg)
	printf("%d, %s\n", rc, "GOOD");
    else {
	printf("%d, %s\n", rc, "BAD");
	errorcnt++;
	}
}

static call3(cid)
    int cid;
{
    char send[4], reply[100], *ptr, *name;
    int rc;

    printf("[SNAPTEST] Call 3, cid %d...", cid);
    fflush(stdout);
    ptr = putint(send, 3);
    rc = SNAP_SendWithReply(cid, send, 4, reply, sizeof reply, NULL);
    if (rc < 0) {
	printf("bad reply, length %d\n", rc);
	errorcnt++;
	return;
    }
    ptr = reply;
    getstr(ptr, &name);
    if (strcmp(name, "BIOYA") == 0)
	printf("\"%s\", %s\n", name, "GOOD");
    else {
	printf("\"%s\", %s\n", name, "BAD");
	errorcnt++;
	}
}

static call4(cid)
    int cid;
{
    char send[4];
    int rc;

    printf("[SNAPTEST] Call 4 on cid %d...", cid);
    fflush(stdout);
    putint(send, 4);
    rc = SNAP_SendNoReply(cid, send, 4, NULL);
    if (rc == SNAP_SUCCESS)
	printf("%d, %s\n", rc, "GOOD");
    else {
	printf("%d, %s\n", rc, "BAD");
	errorcnt++;
	}
}

static call5(cid)
    int cid;
{
    char send[100], reply[100], *ptr;
    int rc;

    printf("[SNAPTEST] Call 5, cid %d...", cid);
    fflush(stdout);
    ptr = putint(send, 5);
    ptr = putstr(ptr, 0);
    rc = SNAP_SendWithReply(cid, send, ptr-send, reply, sizeof reply, NULL);
    if (rc < 0) {
	printf("reply BAD, error: %d\n", rc);
	errorcnt++;
	return;
    }
    getstr(reply, &ptr);
    printf("reply: 0x%x GOOD\n", ptr);
}
static call6(cid)
    int cid;
{
    char send[100], reply[100], *ptr;
    int rc;

    printf("[SNAPTEST] Call 6, cid %d...", cid);
    fflush(stdout);
    ptr = putint(send, 6);
    ptr = putstr(ptr, "");
    rc = SNAP_SendWithReply(cid, send, ptr-send, reply, sizeof reply, NULL);
    if (rc < 0) {
	printf("reply BAD, error: %d\n", rc);
	errorcnt++;
	return;
    }
    getstr(reply, &ptr);
    printf("reply: \"%s\" GOOD\n", ptr);
}

long time ();

static call7(cid)
    int cid;
{
    static char send[10000], reply[10000];
    int rc;
    register int i;
    int msglen;

    msglen = (random() % 9996) + 3;

    fflush(stdout);
    (void) putint(send, 7);
    for (i=4; i<(msglen-2); i++)
	send[i] = random() % 256;
    printf("[SNAPTEST] %010ld Call 7, cid %d len %d ...\n",
	time(NULL), cid, msglen);
    send[msglen-2] = 0x55;
    send[msglen-1] = 0x66;
    send[msglen] = 0x11;
    send[msglen+1] = 0x22;
    reply[msglen-2] = 0;
    reply[msglen-1] = 0;
    reply[msglen] = 0x77;
    reply[msglen+1] = 0x88;

    rc = SNAP_SendWithReply(cid, send, msglen,
	reply, msglen, NULL);
    if (rc < 0) {
	printf("[SNAPTEST] %010ld reply error: %d\n", time(NULL), rc);
	errorcnt++;
	return;
    }
    else
	printf("[SNAPTEST] %010ld reply len: %d ... ", time(NULL), rc);

    for (i=0; i<msglen; i++)
	if (send[i] != reply[i]) {
	    printf ("BAD in pos %d, %02x vs %02x\n", i+1, send[i], reply[i]);
	    errorcnt++;
	    return;
	}

    if ((reply[msglen] == 0x77) &&
	((reply[msglen+1] & 0x0FF) == 0x88))
	printf ("GOOD\n");
    else {
	printf("BAD\n");
	errorcnt++;
	}
}
