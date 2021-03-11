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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/updlist.c,v 2.9 1992/12/15 21:28:38 rr2b R6tape $";
#endif


 

#include <class.h>
#include <updlist.eh>
#include <view.ih>

static struct updateitem *freeList = NULL;

boolean updatelist__InitializeObject(classID, self)
    struct classheader *classID;
    struct updatelist *self;
{
    self->updates = NULL;
    return TRUE;
}

void updatelist__FinalizeObject(classID, self)
    struct classheader *classID;
    struct updatelist *self;
{

    struct updateitem *item;

    if (self->updates != NULL) {
        for (item = self->updates; item->next != NULL; item = item->next)
            ;
        item->next = freeList;
        freeList = self->updates;
    }
}

void updatelist__AddTo(self, view)
    struct updatelist *self;
    struct view *view;
{
  /* modified to 1) add the view to the end of the list, and 2) remove earlier occurrence of the view from the list */

    register struct updateitem *newui;
    register struct updateitem *item, *last;

    /* if view appears on list already, remove it */
    for (item = self->updates, last = NULL;  item != NULL;  last = item, item = item->next)
      {
	if (item->view == view)
	  {
	    if (last == NULL)
	      self->updates = item->next;
	    else
	      last->next = item->next;
	    if (item->next != NULL)
	      last = item->next;	/* save that ptr for a moment... */
	    item->view = NULL;
	    item->next = freeList;
	    freeList = item;

	    /* now run to the end of the list using saved pointer */
	    while (last != NULL && last->next != NULL)
	      last = last->next;
	    break;
	  }
      }

    if (freeList != NULL)  {
	newui = freeList;
	freeList = freeList->next;
    }
    else  {
	newui = (struct updateitem *) malloc(sizeof(struct updateitem));
    }
    
    newui->view = view;
    newui->next = NULL;
    if (last == NULL)
      self->updates = newui;
    else
      last->next = newui;
}

void updatelist__DeleteTree(self, tree)
    struct updatelist *self;
    struct view *tree;
{
    struct updateitem *item;
    struct updateitem *next;
    struct updateitem **previous;

    previous = &self->updates;
    for (item = self->updates; item != NULL; item = next) {
        next = item->next;
	if (view_IsAncestor(item->view, tree)) {
            *previous = item->next;
            item->next = freeList;
            freeList = item;
        }
        else
            previous = &item->next;
    }
}


void updatelist__Clear(self)
    struct updatelist *self;
{
    struct updateitem *item;
    struct updateitem *lastitem = NULL;
    struct updateitem *pendingUpdates;

    pendingUpdates = self->updates;
    self->updates = NULL;
    for (item = pendingUpdates; item != NULL ; item = item->next) {
        view_Update(item->view);
        lastitem = item;
    }
    if (lastitem != NULL) {
        lastitem->next = freeList;
        freeList = pendingUpdates;
    }
}
