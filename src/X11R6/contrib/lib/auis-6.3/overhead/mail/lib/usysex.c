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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/usysex.c,v 2.10 1993/07/16 14:24:15 rr2b Exp $";
#endif

/*
  usysex.c -- Return a static string describing a sysexits value.
  */

#include <errno.h>
#include <andrewos.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#ifdef sys_sun4_51
#include "/usr/ucbinclude/sysexits.h"
#endif

char *UnixSysExits(exitNumber)
int	exitNumber;
{
    /* Returns a pointer to a static buffer containing English text describing the condition that exitNumber describes (interpreted as a sysexits.h value).  The text has no newlines in it. */
    static char ExitBuff[40];
    extern int EX_Nerr;
    extern char *EX_Messages[];

    if (exitNumber == EX_OK) return "OK";
    if (exitNumber >= EX__BASE && exitNumber < (EX__BASE + EX_Nerr))
	return EX_Messages[exitNumber - EX__BASE];
    sprintf(ExitBuff, "Exit number %d", exitNumber);
    return ExitBuff;
}
