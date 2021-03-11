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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/errops.c,v 2.8 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include <errops.h>

/* These are front-end routines to errnode operations */

int             EliErr_ErrP(st)	/* Has an error occurred? */
EliState_t     *st;
{
    return (st->g_errflag);
}

int             EliErr_ErrCode(st)
EliState_t     *st;
{
    return (eliErr_GetCode(EliErrNode(st)));
}

int EliErr_UnixErr(st)
EliState_t *st;
{
    return (eliErr_GetUnixErr(EliErrNode(st)));
}

EliSexp_t      *EliErr_BadSexp(st)
EliState_t     *st;
{
    return (eliErr_GetNode(EliErrNode(st)));
}

int             EliErr_BadSexpP(st)
EliState_t     *st;
{
    return (eliErr_GetNode(EliErrNode(st)) != NULL);
}

char           *EliErr_ErrLoc(st)
EliState_t     *st;
{
    return (eliErr_GetLoc(EliErrNode(st)));
}

int EliErr_BacktraceP(st)
EliState_t *st;
{
    return (eliErr_GetBacktrace(EliErrNode(st)) != NULL);
}

EliCons_t *EliErr_Backtrace(st)
EliState_t *st;
{
    return (eliErr_GetBacktrace(EliErrNode(st)));
}
