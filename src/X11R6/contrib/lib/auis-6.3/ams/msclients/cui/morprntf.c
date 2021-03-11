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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/cui/RCS/morprntf.c,v 2.10 1992/12/15 21:22:34 rr2b R6tape $";
#endif


 

#include <cui.h>
#include <hdrparse.h>
#include <errprntf.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <andrewos.h> /* sys/file.h sys/time.h */
#define CUI_SOURCE_CUIFNS_C
#include <cuimach.h>

extern int CUI_SnapIsRunning;
extern char *GetLine();

int LinesOnTerminal = 24, LinesSincePause = 3,
    CharsOnLine = 1, TerminalLineWidth = 80;

NoMore() {
    LinesSincePause = 3;
    CharsOnLine = 1;
}

SetTerminalParams(h, w)
int h, w;
{
    LinesOnTerminal = h;
    TerminalLineWidth = w;
}

/* VARARGS */
moreprintf(format, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
char *format;
int  a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,
     a13, a14, a15, a16, a17, a18, a19, a20;
{
    char    EnormousLine[10+MAXBODY],
	   *s,
	   *t;

    sprintf(EnormousLine, format, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
    for (s = t = EnormousLine; s && *t;) {
	s = strchr(t, '\n');
	if (s) {
	    if (SpitOutChars(t, s-t+1) == MORE_NO_MORE) return(MORE_NO_MORE);
	    t = s + 1;
	} else {
	    if (SpitOutChars(t, strlen(t)) == MORE_NO_MORE) return(MORE_NO_MORE);
	}
    }
    return(0);
}

SpitOutChars(t, len)
char *t;
int len;
{
    char *myline;
    int newlen;

    while (CharsOnLine+len > TerminalLineWidth) {
	newlen = TerminalLineWidth - CharsOnLine -1;
	fwrite(t, sizeof(char), newlen, stdout);
	fputc('\n', stdout);
	if (LinesOnTerminal && ++LinesSincePause > (LinesOnTerminal)) {
	    safefprintf(stdout, "-- More -- Press Enter to continue or 'q' to quit: ");
	    myline = GetLine();
	    if (myline == (char *) - 1) {
		clearerr(stdin);
	    } else {
		if (*myline == 'q' || *myline == 'Q' || *myline == 'n' || *myline == 'N') {
		    return(MORE_NO_MORE);
		} else if (*myline == 'c' || *myline == 'C') {
		    SetTerminalParams(0,80);
		}
	    }
	}
	CharsOnLine = 0;
	len -= newlen;
	t += newlen;
    }
    fwrite(t, sizeof(char), len, stdout);
    fflush(stdout);
    CharsOnLine += len;
    if (t[len-1] == '\n') {
	if (LinesOnTerminal && ++LinesSincePause > LinesOnTerminal) {
	    safefprintf(stdout, "-- More -- Press Enter to continue or 'q' to quit: ");
	    myline = GetLine();
	    if (myline == (char *) - 1) {
		clearerr(stdin);
	    } else if (*myline == 'q' || *myline == 'Q' || *myline == 'n' || *myline == 'N') {
		return(MORE_NO_MORE);
	    } else if (*myline == 'c' || *myline == 'C') {
		SetTerminalParams(0,80);
	    }
	}
	CharsOnLine = 0;
    }
    return(0);
}
