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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/llist.c,v 1.7 1992/12/15 21:24:39 rr2b R6tape $";
#endif

#include "llist.h"
#include <andrewos.h>

#if !POSIX_ENV
extern void	free();
extern caddr_t	malloc();
#endif

#define	align(x)	((x) + ((x) % sizeof(long)))

/*
** recursively free a linked list
*/
void
l_free(lp)
register struct llist *lp;
{
	if (lp->l_next == NULL)
		return;
	l_free(lp->l_next);
	free(lp->l_item);
}

/*
** allocate a new element in a linked list, along with enough space
** at the end of the item for the next list element header.
*/
struct llist *
l_alloc(lp, s, len)
register struct llist	*lp;
caddr_t	*s;
register unsigned len;
{
	if (s == NULL || lp == NULL)
		return(NULL);

	lp->l_len = len;
	len = align(len);

	if ((lp->l_item = malloc(len + sizeof(struct llist))) == NULL)
		return(NULL);

	bcopy(s, lp->l_item, len);
	lp->l_next = (struct llist *)(&lp->l_item[len]);

	/*
	** set up next list entry
	*/
	lp = lp->l_next;
	lp->l_next = NULL;
	lp->l_item = NULL;
	lp->l_len = 0;
	return(lp);
}
