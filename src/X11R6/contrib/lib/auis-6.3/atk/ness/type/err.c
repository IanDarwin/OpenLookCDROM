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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/type/RCS/err.c,v 1.6 1993/09/01 01:03:02 gk5g Exp $";
#endif





/*
 *    $Log: err.c,v $
 * Revision 1.6  1993/09/01  01:03:02  gk5g
 * check for SIGBUS before using
 *
 * Revision 1.5  1993/05/04  01:23:33  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  02:57:05  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1992/12/15  21:37:21  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.3  1992/12/14  20:48:33  rr2b
 * disclaimerization
 *
 * Revision 1.2  1991/09/12  16:27:38  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.1  1989/08/22  15:28:53  wjh
 * Initial revision
 *
 */

#include "signal.h"
#include "setjmp.h"
#include "err.h"

jmp_buf err_env;

err_HandleSignal()
{
    err_MarkEnd();
    longjmp(err_env, 1);
}


_err_LookAhead()
{
#ifdef SIGBUS
    signal(SIGBUS, err_HandleSignal);
#endif
    signal(SIGSEGV, err_HandleSignal);
}

void
err_MarkEnd()
{
#ifdef SIGBUS
    signal(SIGBUS, SIG_DFL);
#endif
    signal(SIGSEGV, SIG_DFL);
}
