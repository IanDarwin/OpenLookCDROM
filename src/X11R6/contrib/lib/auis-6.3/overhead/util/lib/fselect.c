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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/fselect.c,v 2.12 1993/10/07 21:57:31 gk5g Exp $";
#endif

#include <andrewos.h>		/* sys/time.h */
#include <stdio.h>

static int NOFILES;

int fselect(nfds, rfiles, wfiles, xfiles, timeout)
int	nfds;
FILE	**rfiles, **wfiles, **xfiles;
struct timeval	*timeout;
{
    int rmask = 0, wmask = 0, xmask = 0, ret = 0;
    register int	i;

    if (NOFILES <= 0) {
	NOFILES = getdtablesize();
	if (NOFILES > 32)
	    NOFILES = 32;
    }
    for (i = nfds; --i >= 0;) {
	register int fd;
	if (rfiles && rfiles[i] != NULL && (fd = fileno(rfiles[i])) >= 0 && fd < NOFILES) 
	    if (FILE_HAS_IO(rfiles[i]) > 0)
		ret++;
	    else
		rmask |= 1<<fd;
	if (wfiles && wfiles[i] != NULL && (fd = fileno(wfiles[i])) >= 0 && fd < NOFILES)
	    wmask |= 1<<fd;
	if (xfiles && xfiles[i] != NULL && (fd = fileno(xfiles[i])) >= 0 && fd < NOFILES)
	    xmask |= 1<<fd;
    }
    if (ret==0) 
	ret = select(NOFILES, &rmask, &wmask, &xmask, timeout);
    else rmask = wmask = xmask = 0;
    for (i = nfds; --i >= 0;) {
	if (rfiles && rfiles[i] != NULL &&
	    FILE_HAS_IO(rfiles[i]) <= 0 &&
	    (rmask & (1<<fileno(rfiles[i]))) == 0)
	    rfiles[i] = NULL;
	if (wfiles && wfiles[i] != NULL && (wmask & (1<<fileno(wfiles[i]))) == 0)
	    wfiles[i] = NULL;
	if (xfiles && xfiles[i] != NULL && (xmask & (1<<fileno(xfiles[i]))) == 0)
	    xfiles[i] = NULL;
    }
    return (ret);
}
