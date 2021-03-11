#include <andrewos.h>	/* for index() */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nevent1.c,v 1.6 1993/12/31 04:58:26 rr2b Exp $";
#endif


#include <im.ih>
#include <view.ih>
#include <message.ih>
#include <scroll.ih>
#include <matte.ih>
#include <lpair.ih>
#include <arbiterv.ih>
#include <celv.ih>
#include <cel.ih>
#include <valuev.ih>

/* for FrameMark */
#include <text.ih>
#include <textv.ih>
#include <frame.ih>
#include <buffer.ih>
#include <viewref.ih>

#include <ness.ih>
#include <envt.h>

static struct classinfo *scrollClass = NULL;
static struct classinfo *matteClass = NULL;
static struct classinfo *frameClass = NULL;
static struct classinfo *bufferClass = NULL;
static struct classinfo *arbiterviewClass = NULL;

/* ProperPtr(ptr, type)
	looks around in neighborhood of ptr for a pointer of the specified type
	If ptr is celview, the neighborhood includes child and TrueChild
	If ptr is view, the neighborhood includes its data object.  If the ptr is a view and nothing is found in the neighborhood, the ancestors of the view are used.
*/
	struct basicobject *
ProperPtr(ptr, type)
	struct basicobject *ptr;
	struct classinfo *type;
{
	struct dataobject *dobj;
	struct view *view;
	long i;
	boolean issame;

	if (ptr == NULL) return ptr;
	if (class_IsType(ptr, type)) return ptr;

	if ( ! class_IsType(ptr, viewClass)) {
		/* if ptr is not a view, maybe it is a cel.  
				Try its dataObject */
		if (class_IsType(ptr, celClass)) {
			dobj = cel_GetObject((struct cel *)ptr);
			if (class_IsType(dobj, type))
				return (struct basicobject *)dobj;
		}
		/* maybe it's a buffer */
		if (class_IsType(ptr, CLASS(buffer))) {
			dobj = buffer_GetData((struct buffer *)ptr);
			if (class_IsType(dobj, type))
				return (struct basicobject *)dobj;
		}
		
		return NULL;
	}
	/* it is a view.  If data object is wanted, check this one's */
	if (class_IsType(type, dataobjectClass)) {
		dobj = ((struct view *)ptr)->dataobject;
		if (class_IsType(dobj, type))
			return (struct basicobject *)dobj;
	}


	/* it is a view, try various kinds of containers */

	if (class_IsType(ptr, CLASS(arbiterview))) {
		/* sometimes an arbiter isn't really in the tree */
		/* sometimes it has a child pointer pointing to a bogus place */
		return NULL;
	}

#define NCLASSES 9
	for (i = 1; i <= NCLASSES; i++) {
	    view = NULL;
	    switch (i) {
	    case 1:   if ( (issame = class_IsType(ptr, celviewClass)) ) 
			view = celview_GetApplication((struct celview *)ptr);
		break;

	    case 2: if (issame) 
			view = celview_GetTrueChild((struct celview *)ptr);
		break;

	    case 3:   if ( (issame = class_IsType(ptr, lpairClass)) ) 
			view = lpair_GetNth((struct lpair *)ptr, 0);
		break;

	    case 4: if (issame) 
			view = lpair_GetNth((struct lpair *)ptr, 1);
		break;

	    case 5:   if ( (issame = class_IsType(ptr, CLASS(scroll))) ) 
			view = scroll_GetChild((struct scroll *)ptr);
		break;

	    case 6: if (issame) 
		view = scroll_GetScrollee((struct scroll *)ptr);
		break;

	    case 7: if (class_IsType(ptr, CLASS(matte))) 
			view = ((struct matte *)ptr)->child;
		break;

	    case 8: if (class_IsType(ptr, imClass)) 
			view = ((struct im *)ptr)->topLevel;
		break;

	    case 9: if (class_IsType(ptr, CLASS(frame))) 
			view = frame_GetChildView((struct frame *)ptr);
		break;

	    } /* end of switch(i) */

	    /* test the value of view left by the latest case */
	    if (view != NULL && 
		(view=(struct view *) ProperPtr((struct basicobject *)view, type)) != NULL)
			return (struct basicobject *)view;
	} /* end of for(i) */
#undef NCLASSES

	/* as a last resort try the ancestors of the view we were passed. */
	view = ((struct view *)ptr)->parent;

	while(view && !class_IsType(view, type)) view=view->parent;
	
	return (struct basicobject *)view;
}

	struct text *
FrameMark(ness, m, title, pgmnm, enablecommands, pPos, pLen)
	struct ness *ness;
	struct nessmark *m;
	char *pgmnm, *title;
	boolean enablecommands;
	long *pPos, *pLen;
{
	struct text *text;
	struct textview *textview;
	struct buffer *buffer;
	struct frame *frame;
	struct im *window;
	char *oldpgmnm;
	char *lastslash;
	int pos, len;
	struct dataobject *dobj;

	text = (struct text *)nessmark_GetText(m);
	pos = nessmark_GetPos(m);
	len =  nessmark_GetLength(m);

	oldpgmnm = im_GetProgramName();
	if (pgmnm != NULL && *pgmnm != '\0')
		im_SetProgramName(pgmnm);

	lastslash = rindex(title, '/');
	if (lastslash == NULL)
		lastslash = title;
	else lastslash++;

	/* create buffer.  If text has exactly one object, use it instead of text */
	dobj = (struct dataobject *)text;
	if (pos == 0 && len == 1 && text_GetLength(text) == 1) {
		struct viewref *vr  = text_FindViewreference(text, 0, 1);
		if (vr != NULL)
			dobj = vr->dataObject;
	}
	buffer = buffer_Create(lastslash, NULL, NULL, dobj);

	if ((frame = frame_Create(buffer)) == NULL)
		return NULL;
	if ((window = im_Create(NULL)) == NULL) 
		return NULL;
	im_SetView(window, frame);
	frame_PostDefaultHandler(frame, "message",
			frame_WantHandler(frame, "message"));
	message_DisplayString(frame, 0, "");
	frame_SetBuffer(frame, buffer, TRUE);
	frame_SetCommandEnable(frame, enablecommands);

	if (dobj == (struct dataobject *)text) {
		textview = (struct textview *)frame_GetView(frame);
		textview_WantInputFocus(textview, textview);
		ness_SetArbiter(ness, arbiterview_FindArb((struct view *)textview));
	}
	else {
		struct view *v = frame_GetView(frame);
		view_WantInputFocus(v, v);
		ness_SetArbiter(ness, arbiterview_FindArb(v));
		textview = textview_New();
		textview_SetDataObject(textview, text);
	}
	ness_SetDefaultText(ness, textview);
	textview_SetDotPosition(textview, pos);
	textview_SetDotLength(textview, len);
	textview_FrameDot(textview, pos);

	neventPost (ness, FALSE);
	/* This AddObserver call doesn't appear to serve any purpose.
	 There is no ness_ObservedChanged, so I don't know what would
	 be observed here.  -rr2b Dec 30, 1993.
	 ness_AddObserver(ness, ness); */

	im_KeyboardProcessor();	/* DO IT - wait while user interacts*/

	*pPos = textview_GetDotPosition(textview);
	*pLen = textview_GetDotLength(textview);
	if (dobj != (struct dataobject *)text) {
		textview_Destroy(textview);
	}
	im_SetProgramName(oldpgmnm);
	return text;
}


