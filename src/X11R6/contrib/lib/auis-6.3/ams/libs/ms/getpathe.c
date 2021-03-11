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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getpathe.c,v 2.7 1992/12/15 21:19:11 rr2b R6tape $";
#endif

#include <ms.h>
#include <mailconf.h>

extern char home[];
extern Boolean DidInit;

MS_GetSearchPathEntry(which, buf, lim)
int     which,
        lim;
char   *buf;
{
    int     i;

    if (!DidInit) {		/* First time-only -- initialization
				   section */
	if ((i = InitializeSearchPaths()) != 0)
	    return(i);
    }
    switch(which) {
	case AMS_MAILPATH:
	    /* Wants *mail* directory */
	    strncpy(buf, home, lim);
	    strncat(buf, MAILSEARCHPATHTEMPLATE, lim);
	    break;
	case AMS_OFFICIALPATH:
	    strncpy(buf, OFFICIALSEARCHPATHTEMPLATE, lim);
	    break;
	case AMS_LOCALPATH:
	    strncpy(buf, LOCALSEARCHPATHTEMPLATE, lim);
	    break;
	case AMS_EXTERNALPATH:
	    strncpy(buf, EXTERNALSEARCHPATHTEMPLATE, lim);
	    break;
	default:
	    if (which < 0 || which >= MS_NumDirsInSearchPath) {
		AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_GETSEARCHPATHENTRY);
	    }
	    if (strlen(SearchPathElements[which].Path) > lim) {
		AMS_RETURN_ERRCODE(ERANGE, EIN_PARAMCHECK, EVIA_GETSEARCHPATHENTRY);
	    }
	    strncpy(buf, SearchPathElements[which].Path, lim);
	    break;
    }
    return(0);
}
