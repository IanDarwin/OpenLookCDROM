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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/bldattr.c,v 2.12 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <ms.h>
#include <hdrparse.h>
#include <andrewos.h> /* sys/types.h */
#include <netinet/in.h>  /* for htonl, etc. */

#if !POSIX_ENV
extern char *index(), *rindex();
#define strchr(s,c) index(s,c)
#define strrchr(s,c) rindex(s,c)
#endif

BuildAttributesField(msg)
struct MS_Message *msg;
{
    debug(1, ("Building Attributes field ptrs\n"));
    if (msg->ParsedStuff->HeadBody[HP_ACKTO]) {
	debug(16, ("Return receipt\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_RRR);
    }
    if (msg->ParsedStuff->HeadBody[HP_ENCLOSURE]) {
	debug(16, ("Parcel Post\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_ENCLOSURE);
    }
    if (msg->ParsedStuff->HeadBody[HP_DIRECTORYCREATION]) {
	debug(16, ("Directory creation\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_NEWDIR);
    }
    if (msg->ParsedStuff->HeadBody[HP_OLDVOTE]
        || msg->ParsedStuff->HeadBody[HP_VOTEREQUEST]) {
	debug(16, ("Vote!!!\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_VOTE);
    }
    if (msg->ParsedStuff->HeadBody[HP_SCRIBEFORMAT]
	 || msg->ParsedStuff->HeadBody[HP_CONTENTTYPE]) {
	debug(16, ("Scribe Format\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_FORMATTED);
    }
    if (msg->ParsedStuff->HeadBody[HP_REDISTRIBUTION]) {
	debug(16, ("Redistribution\n"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_REDISTRIBUTION);
    }
    if (msg->AuthUid < 0) {
	debug(16, ("Unauthenticated"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_UNAUTH);
    }
    if (msg->AuthUid == 0) {
	debug(16, ("From remote sender"));
	AMS_SET_ATTRIBUTE(msg->Snapshot, AMS_ATT_FROMNET);
    }
    return(BuildHashVals(msg));
}


/* The following hashing algorithm, KRHash, is derived from Karp & Rabin,
    Harvard Center for Research in Computing Technology Tech Report TR-31-81. */
/* The prime number in use is KRHashPrime, defined in ms.h.  It happens to be
    the largest prime number that will fit in 31 bits, except for 2^31-1 itself. */

/* #define MyPrime 2147483629 */

unsigned long KRHash(s)
register char *s;
{
    register unsigned long sum = 0;
    register unsigned int Bit;

    while (*s) {
	for (Bit = 0x80; Bit != 0; Bit >>= 1) {
	    sum += sum;
	    if (sum >= KRHashPrime) sum -= KRHashPrime;
	    if ((*s) & Bit) ++sum;
	    if (sum >= KRHashPrime) sum -= KRHashPrime;
	}
	++s;
    }
    return(sum+1);
}

static int BuildHashVals(Msg)
struct MS_Message *Msg;
{
    char LineBuf[2000], *s, *t, *mid;
    unsigned long midhash = 0, rephash = 0;
    int len;

    GetRightMid(Msg, &mid);
    if (mid) {
	midhash = KRHash(mid);
	free(mid);
    } else {
	midhash = 0;
    }
    LineBuf[0] = '\0';
    if (Msg->ParsedStuff->HeadBody[HP_INREPLYTO]) {
	len = Msg->ParsedStuff->HeadBodyLen[HP_INREPLYTO];
	strncpy(LineBuf, Msg->ParsedStuff->HeadBody[HP_INREPLYTO], len);
	LineBuf[len] = '\0';
    } else if (Msg->ParsedStuff->HeadBody[HP_REFERENCES]) {
	len = Msg->ParsedStuff->HeadBodyLen[HP_REFERENCES];
	strncpy(LineBuf, Msg->ParsedStuff->HeadBody[HP_REFERENCES], len);
	LineBuf[len] = '\0';
    }
    if (LineBuf[0]) { /* either of above cases */
	s = strchr(LineBuf, '<');
	if (s) {
	    t = strchr(++s, '>');
	    if (t) *t++ = '\0';
	    rephash = KRHash(s);
	    if (t && strchr(t, '<')) {
		rephash |= 0x80000000;
		debug(2, ("This one has multiple reply fields! rephash %d\n", rephash));
	    }
	}
    }
    midhash = htonl(midhash);
    rephash = htonl(rephash);
    bcopy(&midhash, AMS_MIDHASH(Msg->Snapshot), sizeof(long));
    bcopy(&rephash, AMS_REPLYHASH(Msg->Snapshot), sizeof(long));
    debug(2, ("Put hash values in snapshot -- mid %d, reply %d\n", midhash, rephash));
    return(0);
}

#ifdef TESTINGHASHES
ReportHashes(s, label)
char *s, *label;
{
    char *t;
    int i;
    long refint;

    t = AMS_MIDHASH(s);
    printf("%s: Raw mid hash: ", label);
    for (i=0; i<sizeof(long); ++i) {
	printf("%d ", *t++);
    }
    t = AMS_REPLYHASH(s);
    printf("\nRaw rep hash: ");
    for (i=0; i<sizeof(long); ++i) {
	printf("%d ", *t++);
    }
    printf("\n");
    bcopy(AMS_MIDHASH(s), &refint, sizeof(unsigned long));
    printf("midhash Reads out as %d\n", refint);
    bcopy(AMS_REPLYHASH(s), &refint, sizeof(unsigned long));
    printf("rephash Reads out as %d\n", refint);
}

#endif /* TESTINGHASHES */
