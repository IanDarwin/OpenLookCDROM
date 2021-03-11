/* Copyright 1992 Carnegie Mellon University, All rights reserved.
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

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/system.c,v 1.3 1992/12/14 20:21:35 rr2b R6tape $ */

#ifndef NORCSID
static char system_c_rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/system.c,v 1.3 1992/12/14 20:21:35 rr2b R6tape $";
#endif

#include <andrewos.h>

/* work around a bug in system where if the parent process catches sigchld system returns a bogus exit status. */
#ifdef hpux
#include <stdlib.h>
#include <errno.h>
extern int errno;
int os_system(cmd)
char *cmd;
{
    int result;
    errno=0;
    result=system(cmd);
    if(result==-1 && errno==EINTR) return 0;
    return result;
}
#else
int os_system(cmd)
char *cmd;
{
    return system(cmd);
}
#endif

