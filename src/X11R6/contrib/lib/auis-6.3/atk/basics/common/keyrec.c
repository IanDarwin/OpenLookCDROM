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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/keyrec.c,v 2.6 1992/12/15 21:27:56 rr2b R6tape $";
#endif


 

#include <class.h>
#include <keyrec.eh>

static struct keyitem *freeList = NULL;

boolean keyrec__InitializeObject(classID, self)
    struct classheader *classID;
    struct keyrec *self;
{
    if (freeList != NULL)  {
	self->head = freeList;
	freeList = freeList->next;
    }
    else
	self->head = (struct keyitem *) malloc(sizeof(struct keyitem));
    
    self->head->next = self->head;
    self->tail = self->head;
    return TRUE;
}

void keyrec__FinalizeObject(classID, self)
    struct classheader *classID;
    struct keyrec *self;
{
    self->tail->next = freeList;
    freeList = self->head;
}

void keyrec__Clear(self)
    struct keyrec *self;
{
    if (self->head != self->tail)  {
	self->tail->next = freeList;
	freeList = self->head->next;
	self->head->next = self->head;
	self->tail = self->head;
    }
}
	
boolean keyrec__StartRecording(self)
    struct keyrec *self;
{
    if (self->recording) return FALSE;
    self->recording = TRUE;
    keyrec_Clear(self);
}

boolean keyrec__StopRecording(self)
    struct keyrec *self;
{
    if (! self->recording) return FALSE;
    self->recording = FALSE;
    return TRUE;
}

boolean keyrec__Recording(self)
    struct keyrec *self;
{
    return self->recording;
}

struct keyrec *keyrec__Copy(self)
    struct keyrec *self;
{
    register struct keyrec *newkr;
    register struct keyitem *ki;
    register struct keyitem *newki;
    
    newkr = keyrec_NewFromObject(self);
    for (ki = self->head->next; ki != self->head; ki = ki->next)  {
	if (freeList != NULL)  {
	    newki = freeList;
	    freeList = freeList->next;
	}
	else
	    newki = (struct keyitem *) malloc(sizeof(struct keyitem));
	*newki = *ki;
	self->tail->next = newki;
	newki->next = self->head;
	self->tail = newki;
    }
    
    return newkr;
}

void keyrec__RecordEvent(self, type, view, value1, value2, value3)
    struct keyrec *self;
    enum keyrec_EventType type;
    struct view *view;
    union keyrec_KeyValue value1;
    union keyrec_KeyValue value2;
    union keyrec_KeyValue value3;
{
    register struct keyitem *newki;

    if (freeList != NULL)  {
	newki = freeList;
	freeList = freeList->next;
    }
    else
	newki = (struct keyitem *) malloc(sizeof(struct keyitem));
	
    newki->type = type;
    newki->view = view;
    newki->parm[0] = value1;
    newki->parm[1] = value2;
    newki->parm[2] = value3;
    self->tail->next = newki;
    newki->next = self->head;
    self->tail = newki;
}

boolean keyrec__StartPlaying(self)
    struct keyrec *self;
{
    if (self->recording || self->playing) return FALSE;
    self->playing = TRUE;
    self->current = self->head->next;
}

boolean keyrec__NextKey(self, type, view, value1, value2, value3)
    struct keyrec *self;
    enum keyrec_EventType *type;
    struct view **view;
    union keyrec_KeyValue *value1;
    union keyrec_KeyValue *value2;
    union keyrec_KeyValue *value3;
{
    if (! self->playing) return FALSE;
    if (self->current == self->head)  {
	self->playing = FALSE;
	return FALSE;
    }
    *type = self->current->type;
    *view = self->current->view;
    *value1 = self->current->parm[0];
    *value2 = self->current->parm[1];
    *value3 = self->current->parm[2];
    self->current = self->current->next;
    return TRUE;
}

void keyrec__StopPlaying(self)
    struct keyrec *self;
{
    self->playing = FALSE;
}

