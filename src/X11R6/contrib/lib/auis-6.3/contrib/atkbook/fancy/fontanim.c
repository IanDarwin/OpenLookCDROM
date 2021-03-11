static char *fontanim_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/fancy/RCS/fontanim.c,v 1.2 1994/02/01 20:43:56 rr2b Exp $";

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
#include <fontanim.eh>
#include <im.ih>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>
#include <event.ih>

void ToggleStyle();

static char *InsertionText = "\n\n\n\nHello, world!\n\nYou are viewing a demonstration of how the Andrew Toolkit can be used to produce interesting and complex multi-font formatted text.\n\nThis should prove useful in understanding how to make fancy text.\n(At least, that's the intent.)\n";

boolean fontanim__InitializeObject(c, self)
struct classheader *c;
struct fontanim *self;
{
    struct style *styletmp;
    struct stylesheet *ss = fontanim_GetStyleSheet(self);

    fontanim_InsertCharacters(self, 0,
		InsertionText, strlen(InsertionText));
    fontanim_ReadTemplate(self, "help", FALSE);
    styletmp = stylesheet_Find(ss, "center");
    fontanim_SetGlobalStyle(self, styletmp);
    self->italic = stylesheet_Find(ss, "italic");
    self->bold = stylesheet_Find(ss, "bold");
    self->currentstyle = self->italic;
    self->myenv = fontanim_AddStyle(self, 4, 5,
			self->currentstyle);
    environment_SetStyle(self->myenv, TRUE, TRUE);

    self->myevent = im_EnqueueEvent(ToggleStyle, self,
				     event_MSECtoTU(1000)); 
    return(TRUE);
}

boolean fontanim__FinalizeObject(c, self)
struct classheader *c;
struct fontanim *self;
{
    if(self->myevent) {
	event_Cancel(self->myevent);
	self->myevent=NULL;
    }
    return(TRUE);
}

void ToggleStyle(self)
struct fontanim *self;
{
    self->myevent=NULL;
    if (self->currentstyle == self->italic) {
	self->currentstyle = self->bold;
    } else {
	self->currentstyle = self->italic;
    }
    fontanim_SetEnvironmentStyle(self, self->myenv,
				       self->currentstyle);
    fontanim_NotifyObservers(self,
			     observable_OBJECTCHANGED);
    self->myevent = im_EnqueueEvent(ToggleStyle, self,
				     event_MSECtoTU(1000));
}

char *fontanim__ViewName(self)
struct fontanim *self;
{
    return("textview");
}
