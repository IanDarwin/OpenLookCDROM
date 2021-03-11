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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/testing/RCS/utest.c,v 2.9 1993/07/16 14:24:15 rr2b Exp $";
#endif

#include <stdio.h>
#include <signal.h>
#include <system.h>
#include <andrewos.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#ifdef sys_sun4_51
#include "/usr/ucbinclude/sysexits.h"
#endif /* sys_sun4_51 */
#include <mail.h>

/* Test the UnixError, UnixSignal, and UnixSysExits routines. */
main()
{
    int Val;
    extern int EX_Nerr;

    for (Val = 0; Val < 115; Val++) {
	printf("UnixError(%d) is ``%s''", Val, UnixError(Val));
	if (vdown(Val)) printf(" (Vice down)");
	else if (tfail(Val)) printf(" (temp fail)");
	printf("\n");
    }
    printf("\n");
    for (Val = 0; Val < NSIG+1; Val++)
	printf("UnixSignal(%d) is ``%s''\n", Val, UnixSignal(Val));
    printf("\n");
    printf("UnixSysExits(%d) is ``%s''\n", EX_OK, UnixSysExits(EX_OK));
    for (Val = EX__BASE; Val <= (EX__BASE + EX_Nerr); Val++)
	printf("UnixSysExits(%d) is ``%s''\n", Val, UnixSysExits(Val));
}
