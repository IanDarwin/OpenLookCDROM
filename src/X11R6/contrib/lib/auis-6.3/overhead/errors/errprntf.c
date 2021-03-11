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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/errors/RCS/errprntf.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 

/* *************************************************************** 

 errprntf.c:  Routine for printing Andrew-standard errors.

		********************************************
		**** For documentation, see errprntf.h **** 
		********************************************

*/

#include <stdio.h>
#include "errprntf.h"

#define CONTROLMAX 1000  /* Longest printf control string */

errprintf(application, type, log, id, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)

int type;
char *application, *log, *id, *format, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, 
    *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
    char ControlString[CONTROLMAX], *typestr;
    int numfields;

    if (type < 0 || type > 9) type = 0;
    numfields = 1;
    if (application) numfields = 2;
    if (log) numfields = 3;
    if (id) numfields = 4;
    sprintf(ControlString, "<%%s%%s%%s%%s%%s%%s%%s>%s\n", format);
    if (type == ERR_CRITICAL) {
	typestr = "critical";
    } else if (type <= ERR_WARNING) {
	typestr = "warning";
    } else if (type <= ERR_MONITOR) {
	typestr = "monitor";
    } else {
	typestr = "debug";
    }
    return(safefprintf(stderr, ControlString, 
	typestr, 
	(numfields > 1) ? ":" : "",
	application ? application : "",
	(numfields > 2) ? ":" : "",
	log ? log : "",
	(numfields > 3) ? ":" : "",
	id ? id : "",
	 s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, 
	s12, s13, s14, s15, s16, s17, s18, s19, s20));
}

safefprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)
FILE *fp;
char *control, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
    fprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
    fflush(fp);
    if (ferror(fp)) {
	fp = freopen("/dev/console", "w", fp);
	if (fp == NULL) return(-1);
	fprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
	fflush(fp);
	if (ferror(fp)) return(-1);
    }
    return(0);
}
