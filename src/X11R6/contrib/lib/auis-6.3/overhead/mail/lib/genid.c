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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/genid.c,v 2.22 1993/06/23 17:55:59 gk5g Exp $";
#endif

/*

genid.c -- Generates world-unique identifiers, or some approximation that fits
  into a 13 (or 18) character buffer.

This routine uses a static buffer that is overwritten with each call.
  The IsFileName parameter, if non-zero, specifies that the id should, on
      systems lacking the BSD file system enhancements, be restricted to <= 13
      characters.  Otherwise, 18 characters will be used.

We tell whether or not we need the shrunken file names using AMS_UseShortFileNames

Includes old routines from
        base64.c -- Routines for packing long ints into ascii base 64 strings
	No guarantees are made about the operations being invertible for
	negative numbers.
*/

#include <mailconf.h>
#include <ctype.h>
#include <andrewos.h> /* sys/time.h */
#include <netinet/in.h>	/* Get the definition for ntohl. */

static char Basis[65] =
	"0123456789:=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/* In the Basis table, the ``_'' character used to be ``:'' (before 16 Jan 1989).  This isn't a legitimate character in local-parts of RFC822, so it had to be rewritten.  The message server rewrote ``:'' as ``-''; the delivery system rewrote it as ``_''.  All three versions should always map to the same offset (10) via the DigVals table. */
/* Aggh.  19 Jan 1989: revert ``_'' to ``:'' because not all readers changed and because ``_'' is out of lexicographic order. */
static unsigned char DigVals[96] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0,		/* 040 thru 057 */
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 0, 11, 0, 0,		/* 060 thru 077 */
	0, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,	/* 0100 thru 0117 */
	27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 0, 0, 0, 0, 10,	/* 0120 thru 0137 */
	0, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,	/* 0140 thru 0157 */
	53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 0, 0, 0, 0, 0	/* 0160 thru 0177 */
};

char *convlongto64(num, pad)
/* unsigned */ long num;
/* unsigned */ int pad;
{
    static char Answer[7];

    Answer[6] =	0;	/* Initialize null termination */
    Answer[5] = Basis[(num & 077)];
    Answer[4] = Basis[(num >> 6) & 077];
    Answer[3] = Basis[(num >> 12) & 077];
    Answer[2] = Basis[(num >> 18) & 077];
    Answer[1] = Basis[(num >> 24) & 077];
    Answer[0] = Basis[((num >> 30) & 003) | ((pad & 017) << 2)];

    return(Answer);
}

/* Note that the following routine throws away the first 4 of 36 bits */

unsigned long conv64tolong(xnum)
register char *xnum;
{
    register int digits;
    unsigned long Answer = 0;

    digits = strlen(xnum);
    if (digits > 6) digits = 6;
    switch(digits) {
	case 6: Answer |= DigVals[(*xnum)-040] << 30; ++xnum;
	case 5: Answer |= DigVals[(*xnum)-040] << 24; ++xnum;
	case 4: Answer |= DigVals[(*xnum)-040] << 18; ++xnum;
	case 3: Answer |= DigVals[(*xnum)-040] << 12; ++xnum;
	case 2: Answer |= DigVals[(*xnum)-040] << 6; ++xnum;
	case 1: Answer |= DigVals[(*xnum)-040];
    }
    return(Answer);
}

#define RANDOMARRAYSIZE 64
static char RandomSpace[RANDOMARRAYSIZE + 1];	/* For random number generation routines */
extern long random();
#define ChooseRandomBits(n) (((unsigned long) random()) >> (32-n))


char *ams_genid(IsFileName)
int IsFileName;
{
    static char IDBuf[20];
    static long mycounter = 0, MyPid = -1;
    static unsigned long MyHostAddr;
/*    struct timeval tp;
    struct timezone tz; */
    struct osi_Times tp;
    int quadmillisecs; /* for 8 bits of sub-second time resolution */

/*    gettimeofday(&tp, &tz); */
    osi_GetTimes(&tp);
    if (MyPid < 0) {
	CheckAMSConfiguration();
	MyPid = getpid();
	MyHostAddr = (unsigned long) getaddr();
	MyHostAddr = ntohl(MyHostAddr);	/* so it's in the same space for all machines */
	initstate(tp.Secs ^ MyHostAddr ^ MyPid, RandomSpace, RANDOMARRAYSIZE);
    }
    quadmillisecs = (tp.USecs <<8) / 1000000;
    strcpy(IDBuf, convlongto64(tp.Secs, quadmillisecs & 0xF));
    strcat(IDBuf, convlongto64(MyHostAddr, (quadmillisecs >> 4) & 0xF));
    if (IsFileName && AMS_UseShortFileNames) {
	IDBuf[12] = Basis[(++mycounter) & 0x3F];
	IDBuf[13] = '\0';
    } else {
	strcat(IDBuf, convlongto64(MyPid<<16 | (((++mycounter)<<8) & 0x0000FF00) |ChooseRandomBits(8), ChooseRandomBits(4)));
    }
    return(IDBuf);
}

/* Compile for testing with

cc -g -O -I. -I -I/usr/andrew/include -DTESTINGONLYTESTING genid.c /usr/andrew/lib/libmail.a /usr/andrew/lib/libutil.a  /usr/andrew/lib/afs/libsys.a -o genid.test 
*/
#ifdef TESTINGONLYTESTING
#include <stdio.h>
main(argc, argv) int argc; unsigned char **argv; {
    int i, j;
    unsigned long foo, bar, baz;
    unsigned char *cp, *c2;
    
    if (argc > 1) {
	for (i=1; i<argc; ++i) {
		cp=argv[i];
		if (*cp == '+') ++cp;
		foo = conv64tolong(cp);
		bar = conv64tolong(cp+6);
		baz = conv64tolong(cp+12);
		printf("``%s'': generated %s from [", cp, NiceTime(foo));
		c2 = (unsigned char *) &bar;
		for (j = 0; j < 4; ++j) printf("%s%d",
					(j==0 ? "" : "."),
					*c2++);
		j=strlen(cp);
		if (j <= 13) {
			printf("], ctr (mod 64) of %d.\n", baz);
		} else {
			printf("], pid %d, ctr (mod 256) of %d.\n",
				(baz >> 16) & 0xFFFF, (baz >> 8) & 0xFF);
		}
	}
    } else {
	for (i=0; i<100; ++i) {
		cp = ams_genid(1);
		printf("Generated short id %s\n", cp);
	    }
	for (i=0; i<100; ++i) {
		cp = ams_genid(0);
		foo = conv64tolong(cp+10);
		baz = conv64tolong(convlongto64(foo, 0));
		printf("Generated long id %s (%d, %d)\n", cp, foo, baz);
		if (baz != foo) printf("AND THEY'RE NOT THE SAME!\n");
	}
    }
}
#endif /* TESTINGONLYTESTING */

