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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/cmenu/RCS/cmerror.c,v 2.7 1992/12/15 21:00:33 rr2b R6tape $";
#endif


 

#include <cmerror.h>

/*
 * _cmErrorCode - Global cmenu error code.
 */
int _cmErrorCode = cmE_NO_ERROR;

/*
 * _cmErrorList - Global cmenu error code discription strings.
 */
char *
_cmErrorList[cmE_CODE_COUNT] = {
    "No error",				/* cmE_NO_ERROR */
    "Menu not initialized",		/* cmE_NOT_INIT */
    "Argument out of bounds",		/* cmE_ARG_BOUNDS */
    "Pane not found",			/* cmE_P_NOT_FOUND */
    "Selection not found",		/* cmE_S_NOT_FOUND */
    "Unable to create graphics context",/* cmE_CREATE_GC */
    "Unable to calloc memory",		/* cmE_CALLOC */
    "Unable to create XAssocTable",	/* cmE_CREATE_ASSOC */
    "Unable to make pixmap",		/* cmE_MAKE_PIXMAP */
    "Unable to create cursor",		/* cmE_CREATE_CURSOR */
    "Unable to open font",		/* cmE_OPEN_FONT */
    "Unable to create windows",		/* cmE_CREATE_WINDOW */
    "Unable to create InputOnly window",/* cmE_CREATE_INPUTONLY */
};

char *cmenuError()
{
    char message[128];		/* Error message buffer. */

    if ((_cmErrorCode < cmE_CODE_COUNT) && (_cmErrorCode >= 0)) {
	return(_cmErrorList[_cmErrorCode]);
    }
    sprintf(message, "Unknown _cmErrorCode: %d", _cmErrorCode);
    return(message);
}

