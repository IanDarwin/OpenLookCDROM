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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/uerror.c,v 2.10 1992/12/15 21:10:49 rr2b R6tape $";
#endif

/*
	uerror.c -- Return a static string describing an errno value.
*/


 

#include <andrewos.h>
#include <errno.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/errors.h>
#endif /* AFS_ENV */
#ifndef NULL
#define NULL (char *) 0
#endif

char *UnixError(errorNumber)
int	errorNumber;
{
/* Returns a pointer to a static buffer containing English text describing the same error condition that errorNumber describes (interpreted as a Unix error number).  The text has no newlines in it.  We contend that this is what ``perror'' should have been returning all along. */
    static char ErrBuff[40];
    extern int sys_nerr;
    extern char *sys_errlist[];
#ifdef AFS_ENV
    static char *vice_errlist[] = {
	/* 101: VSALVAGE */		"Volume needs salvage",
	/* 102: VNOVNODE */		"Bad vnode number quoted",
	/* 103: VNOVOL */		"Volume not attached, doesn't exist, not created, or not online",
	/* 104: VVOLEXISTS */		"Volume already exists",
	/* 105: VNOSERVICE */		"Volume is not in service",
	/* 106: VOFFLINE */		"Volume is off line",
	/* 107: VONLINE */		"Volume is already on line",
	/* 108: VDISKFULL */		NULL,	/* mapped to ENOSPC */
	/* 109: VOVERQUOTA */		NULL,	/* mapped to EDQUOT */
	/* 110: VBUSY */			"Volume temporarily unavailable; try again",
	/* 111: VMOVED */		"Volume has moved to another server"
    };
#define	Vice_Errlist_Size	(sizeof(vice_errlist) / sizeof(vice_errlist[0]))
#endif /* AFS_ENV */

    if (errorNumber < sys_nerr && errorNumber > 0) return sys_errlist[errorNumber];
#ifdef AFS_ENV
    if (errorNumber >= VICE_SPECIAL_ERRORS
	 && errorNumber < (VICE_SPECIAL_ERRORS + Vice_Errlist_Size))
	if (vice_errlist[errorNumber - VICE_SPECIAL_ERRORS] != NULL)
	    return vice_errlist[errorNumber - VICE_SPECIAL_ERRORS];
#endif /* AFS_ENV */
    sprintf(ErrBuff, "Error number %d", errorNumber);
    return ErrBuff;
}
