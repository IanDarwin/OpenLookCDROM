static char *contimer_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/contimer.c,v 1.2 1994/02/01 20:47:28 rr2b Exp $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <contimer.eh>
#include <im.ih>
#include <event.ih>
#include <observe.ih>

void HandleTimer();

boolean contimer__InitializeObject(c, self)
struct classinfo *c;
struct contimer *self;
{
    self->proc = NULL;
    /* First timer event in one second */
    self->queuedevent = im_EnqueueEvent(HandleTimer, self,
				event_MSECtoTU(1000));
    /* Subsequently every 15 unless SetInterval is called  */
    self->interval = 15000;
    return(TRUE);
}

void contimer__FinalizeObject(c, self)
struct classinfo *c;
struct contimer *self;
{
    if (self->queuedevent != NULL) {
	event_Cancel(self->queuedevent);
	self->queuedevent=NULL;
    }
}

void HandleTimer(self)
struct contimer *self;
{
    self->queuedevent=NULL;
    if (self->proc) (self->proc)(self);
    contimer_NotifyObservers(self, observable_OBJECTCHANGED);
    self->queuedevent = im_EnqueueEvent(HandleTimer, self,
	event_MSECtoTU(self->interval)); 
}

void contimer__SetInterval(self, ms)
struct contimer *self;
int ms;
{
    self->interval = ms;
}

void contimer__SetDataCollectionProc(self, proc)
struct contimer *self;
int (*proc)();
{
    self->proc = proc;
}
