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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/gtvpwnam.c,v 2.9 1992/12/15 21:10:10 rr2b R6tape $";
#endif

/* ************************************************************ *\
	gtvpwnam.c
	getvpwnam(vuid) is like getpwnam(nam), but for Vice IDs.
\* ************************************************************ */
#include <andyenv.h>
#include <andrewos.h> /* syslog.h */
#include <stdio.h>
#include <errno.h>
#include <util.h>
#ifdef WHITEPAGES_ENV
#include <pwd.h>
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <svcconf.h>
#ifndef NULL
#define NULL 0
#endif

extern int errno;

#ifdef WHITEPAGES_ENV
struct passwd *getvpwnam(vnam)
char *vnam;
{/* Return a struct passwd for vuid, a Vice pw_nam */
    wp_ErrorCode Res;
    wp_PrimeKey KVal;
    struct passwd *RV;
    extern struct passwd *_pw_getvpwkey();

    CheckServiceConfiguration();
    if (AMS_UseWP) {
	if (_pw_OpenWP() == 0) {
		errno = ENXIO;
		return NULL;
	}
	Res = wp_GetUIDOnly(vnam, &KVal);
	if (Res != wperr_NoError) {_pw_CloseWP(); errno = ETIMEDOUT; return NULL;}
	RV = _pw_getvpwkey(KVal);
	free(KVal);
	_pw_CloseWP();
	return RV;
    } else {
	return(getpwnam(vnam));
    }
}
#endif /* WHITEPAGES_ENV */

