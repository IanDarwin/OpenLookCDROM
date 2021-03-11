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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/restilde.c,v 2.14 1993/05/05 19:49:43 susan Exp $";
#endif

#include <util.h>
#include <ms.h>

#define MAXUSERNAME 100		/* Biggest length of a user name */

extern char home[], *StripWhiteEnds(), *FindUserDir();
extern char MyMailDomain[];

/* If you make any changes to this routine, check out TildeResolve in
MessagesBE.c as well. */

/* This routine takes a path name and resolves leading ~ references, e.g.
	~/foo and ~nsb/bar, returning a freshly malloced version */

ResolveTildes(old, new, domain)
char   *old,
      **new, *domain;
{
    char   *t, user[2*MAXUSERNAME], *udir;
    struct CellAuth *ca;
    int Res;

    debug(1,("ResolveTildes %s\n", old));
    old = StripWhiteEnds(old);
    if (*old != '~') {
	*new = NewString(old);
	if (*new == NULL) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_DISAMB);
	}
    } else {
	if (*++old == '/') {
	    if (domain == NULL || *domain == '\0' || ULstrcmp(domain, MyMailDomain) == 0) {
		udir = home;
	    } else {
		ca = NULL;
		Res = FindCell(domain, &ca);
		if (ca == NULL) {
		    if (Res < 0) Res = ETIMEDOUT;
		    else Res = EMSUNAUTH;
		    AMS_RETURN_ERRCODE(Res, EIN_DISAMB, EVIA_DISAMB);
		}
		FillInCell(ca);
		if (ca->homeDir == NULL) AMS_RETURN_ERRCODE(EMSNOHOME, EIN_DISAMB, EVIA_DISAMB);
		udir = ca->homeDir;
	    }
	    *new = malloc(strlen(old) + strlen(udir) + 2);
	    if (*new == NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_DISAMB);
	    }
	    sprintf(*new, "%s/%s", udir, ++old);
	} else {
	    for (t = user; *old && *old != '/'; ++old, ++t) {
		*t = *old;
	    }
	    *t = '\0';
	    if (*old)
		++old;
	    if (domain == NULL || *domain == '\0') domain = MyMailDomain;
#ifdef WHITEPAGES_ENV
	    cpw_error = 0;
#endif /* WHITEPAGES_ENV */
	    udir = FindUserDir(user, domain);
	    if (udir == (char *) - 1) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_GETPWNAM, EVIA_DISAMB);
	    }
	    if (udir == NULL) {
#ifdef WHITEPAGES_ENV
		AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(cpw_error, EMSNOSUCHUSER, TRUE), EIN_GETPWNAM, EVIA_DISAMB);
#else /* WHITEPAGES_ENV */
		AMS_RETURN_ERRCODE(EMSNOSUCHUSER, EIN_GETPWNAM, EVIA_DISAMB);
#endif /* WHITEPAGES_ENV */
	    }
	    *new = malloc(strlen(old) + strlen(udir) + 2);
	    if (*new == NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_DISAMB);
	    }
	    sprintf(*new, "%s/%s", udir, old);
	}
    }
    return(0);

}

