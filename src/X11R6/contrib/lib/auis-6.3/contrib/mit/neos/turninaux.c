/* $Author: rr2b $ */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/turninaux.c,v 1.4 1993/07/29 16:13:45 rr2b Exp $";
#endif


 
/*
 * turninaux.c
 *
 * This is the overflow from the turnin.c module.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>
#include <class.h>
#include <atom.ih>
#include <atomlist.ih>
#include <blank.ih>
#include <buffer.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <eosbutt.ih>
#include <eos.h>
#include <fontdesc.ih>
#include <frame.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <im.ih>
#include <lpair.ih>
#include <menulist.ih>
#include <onoffv.ih>
#include <pshbttn.ih>
#include <rect.h>
#include <strinput.ih>
#include <value.ih>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <andrewos.h>	/* andrewos.h includes sys/types.h */
#undef label

#include <label.ih>
#include <labelv.ih>

#define debug(x)  /* printf x ; fflush(stdin);  */
/* #include <stdio.h> */

#define AUXMODULE 1
#include <turnin.eh>	/* turnin.eh uses struct label */

static char *ofont     = "Andy";
static char *otoplabel = "Use current edit buffer";
static char *obotlabel = "Use named file";

extern void turnin_TurninGo();
extern void turnin_Hide();

extern struct menulist *turnin_menus;
extern struct keymap *turnin_kmap;

boolean turnin__InitializeObject(classID, self)
struct classheader *classID;
struct turnin *self;
{
    int style;
    struct blank *blank;
    struct lpair *left, *right;

    self->turninfromfile = FALSE;
    self->daddy = NULL;
    self->kstate       = keystate_Create(self, turnin_kmap);
    self->title	       = label_New();
    self->titleV       = labelview_New();
    self->course       = (char *) malloc(33);

    self->menus        = menulist_DuplicateML(turnin_menus, self);

    /* Create the title */
    labelview_SetDataObject(self->titleV, self->title);
    label_SetFlags(self->title, label_CENTERED);
    label_SetText(self->title, "Turn In Assignment");
    label_SetFont(self->title, "helvetica", fontdesc_Plain, 24);

    /* Create the two buttons: OK and CANCEL */
    style = environ_GetProfileInt("buttonstyle", 2);
    self->okb = pushbutton_New();
    self->okv = newbuttonview_New();
    self->cancelb = pushbutton_New();
    self->cancelv = newbuttonview_New();
    pushbutton_SetStyle(self->okb, style);
    pushbutton_SetStyle(self->cancelb, style);
    pushbutton_SetText(self->okb, "OK");
    pushbutton_SetText(self->cancelb, "CANCEL");
    newbuttonview_SetDataObject(self->okv, self->okb);
    newbuttonview_SetDataObject(self->cancelv, self->cancelb);
    newbuttonview_AddRecipient(self->okv, atom_Intern("buttonpushed"), (struct view *) self, turnin_TurninGo, 0L);
    newbuttonview_AddRecipient(self->cancelv, atom_Intern("buttonpushed"), (struct view *) self, turnin_Hide, 0L);

    /* left and right are used to force the buttons to be a fixed size, with blank padding any extra space */
    left = lpair_New();
    blank = blank_New();
    lpair_HFixed(left, blank, self->okv, 100, FALSE);
    right = lpair_New();
    blank = blank_New();
    lpair_HTFixed(right, self->cancelv, blank, 100, FALSE);
    self->buttons = lpair_New();
    lpair_HSplit(self->buttons, left, right, 50, FALSE);
    lpair_SetMovable(self->buttons, FALSE);

    /* Create the questions */
    self->name   = strinput_New();
    self->number = strinput_New();
    strinput_SetPrompt(self->name, "Filename of assignment: ");
    strinput_SetPrompt(self->number, "Assignment number: ");
    self->questions = lpair_New();
    lpair_VSplit(self->questions, self->name, self->number, 50, FALSE);
    lpair_SetMovable(self->questions, FALSE);

    /* Create the on/off switch */

    /* Because of the way the value objects work, all the real */
    /* work must be postponed until after the views exist */
    /* We finish the job in turnin__FullUpdate where we set */
    /* ResourcesPosted to true so that we do the work only once. */

    self->onoff = value_New();
    self->onoffv = onoffV_New();
    self->ResourcesPosted = FALSE;

    self->userarea = lpair_New();
    lpair_VTFixed(self->userarea, self->onoffv, self->questions, 80, FALSE);

    /* Create the view itself */
    self->topscreen = lpair_New();
    lpair_VSplit(self->topscreen, self->titleV, self->userarea, 85, FALSE);
    lpair_SetMovable(self->topscreen, FALSE);
    self->whole = lpair_New();
    lpair_VFixed(self->whole, self->topscreen, self->buttons, 50, FALSE);
    lpair_LinkTree(self->whole, self);

    debug(("turnin object created\n"));

    return TRUE;
}

void turnin__FinalizeObject(classID, self)
struct classheader *classID;
struct turnin *self;
{
    /* Does nothing for now */
}



void turnin__LinkTree(self, parent)
struct turnin *self;
struct view *parent;
{
    super_LinkTree(self, parent);

    if (self->whole)
	lpair_LinkTree(self->whole, self);
}


void turnin__SetTitle(self, title)
struct turnin *self;
char *title;
/* Set the text of the title bar. If course-in-title is TRUE, then add the name
  of the course into the text */
{
    char string[80];
    strcpy(string, title);
    if (environ_GetProfileSwitch("course-in-title", TRUE)) {
	strcat(string, ": ");
	strcat(string, self->course);
    }
    label_SetText(self->title, string);
}

void turnin__FullUpdate(self, type, left, top, width, height)
register struct turnin *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle childRect;

    if (self->IDoNotExist) self->IDoNotExist = FALSE;

    /* Now that the views exist, and view tree is established, post the */
    /* resources that customize our onoff switch, if they have not */
    /* already been posted. */

    if (!self->ResourcesPosted) {
	turnin_PostResource(self, atomlist_StringToAtomlist("bodyfont-size"), atom_Intern("long"), (long) 12);
	turnin_PostResource(self, atomlist_StringToAtomlist("bodyfont"), atom_Intern("string"), (long) ofont);
	turnin_PostResource(self, atomlist_StringToAtomlist("top label"),atom_Intern("string"), (long) otoplabel);
	turnin_PostResource(self, atomlist_StringToAtomlist("bottom label"), atom_Intern("string"), (long) obotlabel);
	value_SetValue(self->onoff, 1);
	/* Set the data object After the view exists */
	/* and after the value has been set! */
	onoffV_SetDataObject(self->onoffv, self->onoff);
	self->ResourcesPosted = TRUE;
    }

    turnin_GetLogicalBounds(self, &childRect);

    lpair_InsertView(self->whole, self, &childRect);
    lpair_FullUpdate(self->whole, type, left, top, width, height);
}

struct view *turnin__Hit(self, action, x, y, clicks)
struct turnin *self;
enum view_MouseAction action;
long x, y, clicks;
{
    struct rectangle r, labelrect;
    struct point *mouse;
    struct view *tmpv;

    mouse = point_CreatePoint(x, y);
    onoffV_GetEnclosedBounds(self->onoffv, &r);
    labelview_GetEnclosedBounds(self->titleV, &labelrect);
    r.top += labelrect.height;

    tmpv = lpair_Hit(self->whole, action, x, y, clicks);
    if (rectangle_IsPtInRect(mouse, &r)) {
	turnin_WantInputFocus(self, self->name);
	return (struct view *) self;
    }
    return tmpv;

}
