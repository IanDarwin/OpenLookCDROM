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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/shr/RCS/brkdown.c,v 2.10 1992/12/15 21:21:37 rr2b R6tape $";
#endif


 

#include <andrewos.h>

extern char *Next822LPart();

static char * Useful_Next822LPart(s, e, thing, size)
char *s, *e, *thing;
int size;
{
    while (1) {
	s = Next822LPart(s, e, thing, size);
	if (!s || (s == e) || (*thing != ';')) return(s);
    }
}

BreakDownContentTypeField(HeadBuf, fmt, fmtsz, vers, verssz, resources, resourcessz)
char *HeadBuf, *fmt, *vers, *resources;
int fmtsz, verssz, resourcessz;
{
    char *s, *end, *ptr;
    int len;

    *fmt = *vers = *resources = '\0';
    s = HeadBuf;
    end = HeadBuf + strlen(HeadBuf);
    s = Useful_Next822LPart(s, end, fmt, fmtsz);
    if (s) s = Useful_Next822LPart(s, end, vers, verssz);
    if (s) {
	ptr = resources;
	s = Useful_Next822LPart(s, end, ptr, resourcessz);
	do {
	    len = strlen(ptr);
	    ptr += len;
	    resourcessz -= len;
	    s = Next822LPart(s, end, ptr, resourcessz);
	} while (s && (s != end) && resourcessz > 0);
    }
}

/* This routine chops up a resource list into separate resources, allocating an array of pointers to the separate resources.  The separate resources themselves are not allocated, but are obtained via destructive operations on the original list. */

char **BreakDownResourcesIntoArray(reslist)
char *reslist;
{
    int ct=0;
    char *s=reslist;
    char **rval;
    
    while (s=strchr(s, ',')) {
	++ct;
	++s;
    }
    rval = (char **) malloc((1+ct) * sizeof(char *));
    if (rval) {
	rval[0] = reslist;
	ct = 0;
	while (s=strchr(rval[ct++], ',')) {
	    *s++ = '\0';
	    rval[ct] = s;
	}
	rval[ct] = 0;
    }
    return(rval);
}
