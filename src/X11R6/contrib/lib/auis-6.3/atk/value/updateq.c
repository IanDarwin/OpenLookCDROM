/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/updateq.c,v 2.7 1992/12/15 21:47:14 rr2b R6tape $";
#endif


 

#include <class.h>
#include <updateq.eh>
#define New(TYPE) ( TYPE *)malloc( sizeof (TYPE) )

/**************** statics ****************/


/**************** class procedures ****************/
boolean updateq__InitializeObject(classID, self )
struct classheader *classID;
     struct updateq * self;
{
  self->view = NULL;
  self->queue = NULL;
  return TRUE;
}


void updateq__FinalizeObject( self )
     struct updateq * self;
{
  updateq_ClearUpdateQueue(self);
}


void updateq__SetView( self, view )
     struct updateq * self;
     struct view * view;
{
  self->view = view;
}


void updateq__EnqueueUpdateFunction( self, fp )
     struct updateq * self;
     procedure fp;
{
  struct fp_queue *  newEntry;
  struct fp_queue ** qpointer;

  newEntry = New( struct fp_queue );
  newEntry->fp = fp;
  newEntry->next = NULL;
  for (qpointer = &self->queue; *qpointer != NULL;
       qpointer = &((*qpointer)->next));

  *qpointer = newEntry;
}


void updateq__ExecuteUpdateQueue(self)
     struct updateq * self;
{
  struct fp_queue * qpointer;

  /* The implimentation of this allows the clearq and enque methods
    to be envoked by one of the functions that gets called here.  if you
    break this, you will loose in a serious way */

  while (self->queue != NULL)
    {
      qpointer = self->queue;
      self->queue = self->queue->next;
      (*(qpointer->fp))(self->view);
      free(qpointer);
    }

}


void updateq__ClearUpdateQueue(self)
     struct updateq * self;
{
  struct fp_queue * qpointer;
  
  while (self->queue != NULL)
    {
      qpointer = self->queue;
      self->queue = self->queue->next;
      free(qpointer);
    }
}

