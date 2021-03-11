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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/capture.c,v 1.13 1993/08/25 20:34:51 susan Exp $";
#endif

/*
  capture.c Capture addresses as they fly by.
*/

#include <andrewos.h> /* sys/time.h */
/*#include <sys/file.h>*/
#include <stdio.h>
#include <ctype.h>
#include <util.h>
#include <mailconf.h>

void Capture(Hdr)
char *Hdr;
{/* Capture anything you like from the message header Hdr. */
    static struct flds {char *Name; char Abbr;} Fields[] = {
	{"From:", 'f'},
	{"Reply-to:", 'r'},
	{"ReSent-From:", 's'},
	{"ReSent-Reply-to:", 't'},
    };
#define numFields (sizeof(Fields) / sizeof(Fields[0]))
    int FD, ix;
    auto char CaptFN[1000];
    auto char WrittenLine[3000];
    char *begOrg, *endOrg;
    char *begFld, *endFld;
    int sigLen, lineLen;

    if (Hdr == NULL || *Hdr == '\0') return;
    sprintf(CaptFN, "%s/%s", DuplicateDB, CaptSuffix);
    FD = open(CaptFN, O_RDWR, 0644);
    if (FD < 0) return;
    if (osi_ExclusiveLockNoBlock(FD) < 0) {close(FD); return;}
    if (lseek(FD, 0, 2) < 0) return;
    sprintf(WrittenLine, "\n|%d|", time(0));
    sigLen = strlen(WrittenLine);
    WrittenLine[sigLen+1] = '|';
    if (BracketField(Hdr, "organize:", &begOrg, &endOrg, NULL) == 0) begOrg = endOrg = NULL;
    if (begOrg != NULL) {
	--endOrg;
	while (isspace(*endOrg) && endOrg > begOrg) --endOrg;
	if (endOrg == begOrg) begOrg = endOrg = NULL;
	else ++endOrg;
    }
    for (ix = 0; ix < numFields; ++ix) {
	if (BracketField(Hdr, Fields[ix].Name, &begFld, &endFld, NULL) != 0) {
	    --endFld;
	    while (isspace(*endFld) && endFld > begFld) --endFld;
	    if (endFld != begFld) {
		++endFld;	/* |time|flag|from|org| */
		WrittenLine[sigLen] = Fields[ix].Abbr;
		lineLen = sigLen + 2;
		strncpy(&WrittenLine[lineLen], begFld, (endFld - begFld));
		lineLen += (endFld - begFld);
		WrittenLine[lineLen++] = '|';
		if (begOrg != NULL) {
		    strncpy(&WrittenLine[lineLen], begOrg, (endOrg - begOrg));
		    lineLen += (endOrg - begOrg);
		}
		WrittenLine[lineLen++] = '|';
		if (writeall(FD, WrittenLine, lineLen) != lineLen) {close(FD); return;}
	    }
	}
    }
    close(FD);
}

#ifdef TESTINGONLYTESTING
main()
{
    Capture("From: hi there\n");
    Capture("From: hello@server\nOrganization: Dog Meat Pies, Mars\n");
    Capture("Resent-Reply-To: hiowareyou\n");
    exit(0);
}
#endif /* TESTINGONLYTESTING */
