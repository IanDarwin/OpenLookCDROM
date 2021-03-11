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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/hdlpref.c,v 2.7 1992/12/15 21:19:11 rr2b R6tape $";
#endif

#include <ms.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern char *getprofile();

MS_HandlePreference(prog, pref, InVal, OutVal, OutLim, opcode, resulti, defaulti)
char *prog, *pref, *InVal; /* Passed IN */
char *OutVal; /* Passed OUT */
int OutLim, opcode, defaulti; /* Passed IN */
int *resulti; /* Passed OUT */
{
    char *s, *key;

    key = malloc(2+strlen(prog) + strlen(pref));
    if (!key) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_HANDLEPREFERENCE);
    }
    sprintf(key, "%s.%s", prog, pref);
    switch(opcode) {
	case AMS_GETPROFILESTRING:
	    s = getprofile(key);
	    if (s) {
		strncpy(OutVal, s, OutLim);
	    } else {
		OutVal[0] = '\0';
	    }
	    break;
	case AMS_GETPROFILEINT:
	    *resulti = getprofileint(key, defaulti);
	    break;
	case AMS_GETPROFILESWITCH:
	    *resulti = getprofileswitch(key, defaulti);
	    break;
	case AMS_SETPROFILESTRING:
	    if (setprofilestring(prog, pref, InVal)) {
		free(key);
		AMS_RETURN_ERRCODE(errno, EIN_SETPROF, EVIA_HANDLEPREFERENCE);
	    }
	    break;
	default:
	    free(key);
	    AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_HANDLEPREFERENCE);
    }
    free(key);
    return(0);
}
