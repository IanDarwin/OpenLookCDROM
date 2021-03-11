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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/cui/RCS/unixmach.c,v 2.9 1992/12/15 21:22:34 rr2b R6tape $";
#endif


 

/*  machine dependant functions for the cui program */

#include <cui.h>
#include <hdrparse.h>
#include <errprntf.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <andrewos.h> /* sys/file.h sys/time.h */
#define CUI_SOURCE_CUIFNS_C
#include <cuimach.h>

/*
 main is machine dependant because on the macintosh we need
 to allocate the heap between heap and stack space.  also, if
 the string stripper preprocessor has pulled out all the double quote
 literals load them in before anyone touches them
*/
int main(argc,argv)
int argc;
char **argv;
{
 return(cui_prog_main(argc,argv));
}

extern int LinesSincePause,CharsOnLine;
/*
  unix GetLine doesn't work on the mac.  Enter on the mac types in the whole
  line, including the promt.  Pretty stupid eh?
*/
char   *GetLine () {
    static char InBuf[LINEMAX + 1];
    int     c;
    int     lim;

    debug(1,("GetLine\n"));
    LinesSincePause = 3;
    CharsOnLine = 1;
    InBuf[0] = '\0';
    for (lim = 0; lim < LINEMAX && (c = getcharwithpausecheck()) != EOF && c != '\n'; ++lim) {
	InBuf[lim] = (char) c;
    }
    InBuf[lim] = 0;
    if (c == EOF) {
	clearerr(stdin);
	return((char *) - 1);
    }
    return(InBuf);
}
extern int CUI_SnapIsRunning;
getcharwithpausecheck() {
#ifdef CUI_KEEPALIVE_WITHOUT_SNAP
    int nfs = 0;
    FILE *rf[2];
    struct timeval timeout;

    if (!CUI_SnapIsRunning) {
	timeout.tv_sec = 30;
	timeout.tv_usec = 0;
	while (!nfs) {
	    rf[0] = stdin;
	    nfs = fselect(1, rf, 0, 0, &timeout);
	    if (!nfs) {
		if (mserrcode = MS_FastUpdateState()) {
		    ReportError("Could not update message server state after timeout.", ERR_WARNING, TRUE);
		}
	    }
	}
    }
#endif /* CUI_KEEPALIVE_WITHOUT_SNAP */
    return(getchar());
}
