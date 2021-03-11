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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/wpstat.c,v 2.12 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	wpstat.c
	The wp_ErrorString function.
	Include file ``wp.h'' declares the procedures for clients.
\* ************************************************************ */

#include <andyenv.h>
#include <stdio.h>
#include <andrewos.h>		/* sys/file.h */
#include <ctype.h>
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */
#include <pwd.h>
#include <util.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include <bt.h>
#include <wp.h>
#include <btwp.h>
#endif /* WHITEPAGES_ENV   */
extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

/* gives static English text interpreting an error code value */
char *wp_ErrorString(codevalue)
wp_ErrorCode codevalue;
{
    static char *WPErrs[wperr_MAX + 1 + 1] = {
	"Not an error",
	"Out of memory",
	"No keys found",
	"Too many keys found",
	"No such key",
	"Key error",
	"No such field",
	"Indeterminate result",
	"Field index out of bounds",
	"No such token kind",
	"Token malformed",
	"Function not implemented",
	"Internal error",
	"Undifferentiated file system error",
	"Undifferentiated Grits error",
	"Undifferentiated B-tree error",
	"B-tree temporary inconsistency",
	"B-tree index includes missing record",
	"Non-cell compatibility routine called before wp_Initialize",
	"Argument is not of wp_cd type",
	"White Pages are for the wrong cell",
	"No primary authentication to use for default cell",
	NULL };
	static char WPErrBuff[120];

	if (codevalue <= wperr_MAX && codevalue >= 0) return WPErrs[codevalue];
	if (codevalue >= wperr_FileSystemErrorBegin &&
	    codevalue <= wperr_FileSystemErrorEnd) {
	    sprintf(WPErrBuff, "White pages: %s",
		    UnixError(codevalue - wperr_FileSystemErrorBegin));
	    return WPErrBuff;
	}
	if (codevalue >= wperr_BTreeBaseValue
	    && codevalue < (wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)) {
	    sprintf(WPErrBuff, "B-tree: %s",
		    bt_ErrorString(codevalue - wperr_BTreeBaseValue));
	    return WPErrBuff;
	}
	if (codevalue >= (wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)
	    && codevalue <= (wperr_BTreeBaseValue + bterr_FileSystemErrorEnd)) {
	    return bt_ErrorString(codevalue - wperr_BTreeBaseValue);
	}
	sprintf(WPErrBuff, "White Pages error code %d", codevalue);
	return WPErrBuff;
}
