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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/tfail.c,v 2.10 1992/12/15 21:10:49 rr2b R6tape $";
#endif

/*
	tfail.c
	tfail() -- Return a Boolean indicating whether the errno value represents a temporary failure.
*/


 

#include <andrewos.h>
#include <stdio.h>
#include <errno.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/errors.h>
#endif /* AFS_ENV */
#include <util.h>

int tfail(errorNumber)
int errorNumber;
{
/* Returns a Boolean indicating whether the errno value errorNumber is probably a temporary failure condition, i.e., one that might succeed if tried again later.  Returns 1 (true) on a temporary failure, 0 (false) on a permanent failure.
Admittedly, for most of the error conditions described, we can make only a guess about the temporary-ness of an error (EIO? EMFILE? EROFS? EMLINK?), but this is only a rough guess.
*/

    if (vdown(errorNumber)) return 1;
    switch (errorNumber) {
	case EINTR:
	case EIO:
	case EAGAIN:
	case ENOMEM:
	case ENODEV:
	case ENFILE:
	case ETXTBSY:
	case ENOSPC:
	case ENETDOWN:
	case ENETUNREACH:
	case ENETRESET:
	case ECONNABORTED:
	case ECONNRESET:
	case ENOBUFS:
	case ESHUTDOWN:
	case ETIMEDOUT:
	case ECONNREFUSED:
	case EHOSTDOWN:
	case EHOSTUNREACH:
#ifdef EDQUOT
	case EDQUOT:
#endif /* EDQUOT */
#ifdef AFS_ENV
	case VSALVAGE:
	case VOFFLINE:
	case VBUSY:
	case VMOVED:
#endif /* AFS_ENV */
	    return 1;		/* temporary failures */
	default:
	    return 0;		/* all others are permanent failures */
    }
}
