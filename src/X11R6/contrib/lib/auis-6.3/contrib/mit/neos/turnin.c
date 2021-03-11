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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/turnin.c,v 1.4 1994/04/22 23:53:28 rr2b Exp $";
#endif


 
/*
 * turnin.c
 *
 * This does the work for turnin within the EOS applications.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>

#include <class.h>
#include <bind.ih>
#include <buffer.ih>
#include <cursor.ih>
#include <environ.ih>
#include <eos.h>
#include <frame.ih>
#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <message.ih>
#include <point.h>
#include <strinput.ih>
#include <turnin.eh>
#include <value.ih>
#include <view.ih>
#include <sys/errno.h>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <eosfx.ih>	/* eosfx.ih includes sys/types.h */
#undef label

#include <eos.ih>   /* eos.ih uses struct label */

#define debug(x)  /* printf x ; fflush(stdin);  */
/* #include <stdio.h> */

extern boolean turnin__InitializeObject();
extern void turnin__FinalizeObject();
extern void turnin__SetTitle();
extern void turnin__FullUpdate();
extern struct view *turnin__Hit();

static struct cursor *clockcursor;
struct keymap *turnin_kmap;
struct menulist *turnin_menus;

void CancelOperation(self, fxp)
struct turnin *self;
FX **fxp;
{
    eosfx_Close(fxp);
    turnin_WantInputFocus(self, self->name);
    StopWaitCursor();
}

void turnin_Hide( self, triggerer, rock)
struct turnin *self;
struct observable *triggerer;
long rock;
{
    if (!self) return;
    self->IDoNotExist = TRUE;
    /* "If I close my eyes, then you can't see me!" */
    im_VanishWindow(turnin_GetIM(self));
}

void turnin_TurninGo(self, triggerer, rock)
struct turnin *self;
struct observable *triggerer;
long rock;
/* This routine sends the paper specified by the strinput fields
   The name and number must be specified, else the routine fails
   If any errors occur during the sending of the file, then the routine fails
   If the routine fails, an error message is reported and nothing changes
   If it succeeds, then it gives a MESSAGE to self->daddy->frame
     and to self saying that turnin succeeded.
    Then it acts like we hit the ok button and hides the turnin window.
 */
{
    char filename[128], papername[128], assignment[6], *errormsg;
    struct buffer *buf;
    Paper paper;

    strcpy(filename, strinput_GetInput(self->name, 120));
    if (!strcmp(filename, "")) {
	message_DisplayString(self, DIALOG, "You must specify a filename");
	turnin_WantInputFocus(self, self->name);
	CancelOperation(self, NULL);
	return;
    }

    strcpy(assignment, strinput_GetInput(self->number, 5));
    if (!strcmp(assignment, "")) {
	message_DisplayString(self, DIALOG, "You must specify an assignment number");
	turnin_WantInputFocus(self, self->number);
	CancelOperation(self, NULL);
	return;
    }

    message_DisplayString(self, MESSAGE, "Please wait: Turning in paper....");
    StartWaitCursor();
    im_ForceUpdate();

    eosfx_PaperClear(&paper);
    paper.assignment = atoi(assignment);
    paper.type = TURNEDIN;
    strcpy (papername, eosfx_PathTail(filename));
    paper.filename = papername;

    if (self->daddy == NULL) {
	message_DisplayString(self, DIALOG, "Program bug - I am an orphan!");
	CancelOperation(self, NULL);
	return;
    }

    if (value_GetValue(self->onoff)) {
	char *home;
	debug(("Turning in buffer\n"));
	self->turninfromfile = FALSE;
	/* Save temporarily in home dir of user */
	home = environ_Get("HOME");
	strcpy(filename, home);
	strcat(filename, "/");
	strcat(filename, papername);
	strcpy(filename, eosfx_LocalUnique(filename));
	buf = frame_GetBuffer(self->daddy->frame);
	buffer_WriteToFile(buf, filename, buffer_ReliableWrite);
    } else {
	debug(("Turning in file\n"));
	self->turninfromfile = TRUE;
    }

    errormsg = eosfx_SendFile(self->course, filename, &paper, FALSE);
    if (!self->turninfromfile) unlink (filename);
    if (errormsg != NULL) {
	message_DisplayString(self, DIALOG, errormsg);
	message_DisplayString(self, MESSAGE, "Turn in failed.");
	CancelOperation(self, NULL);
	return;
    }
    message_DisplayString(self->daddy->frame, MESSAGE, "Assignment has been turned in.");
    message_DisplayString(self, MESSAGE, "Assignment has been turned in.");
    StopWaitCursor();
    self->IDoNotExist = TRUE;
    im_VanishWindow(turnin_GetIM(self));
    return;
}

void MoveOn(self, rock)
struct turnin *self;
long rock;
/* This is the routine bound to the Return key - it moves the focus from
  self->name to self->number, or, if self->number already has the focus,
      then it calls turnin_TurninGo
      */
{
    if (strinput_HaveYouGotTheFocus(self->name))
	turnin_WantInputFocus(self, self->number);
    else if (strinput_HaveYouGotTheFocus(self->number))
	turnin_TurninGo(self, NULL, 0);
}

static struct bind_Description turnin_bindings[] =
{
    {"turnin-move-on", "\015", 0, NULL, 0, 0, MoveOn, NULL},
    {"turnin-move-on", "\012", 0, NULL, 0, 0, MoveOn, NULL},
    {"turnin-cancel", NULL, 0, "Cancel", 0, 0, turnin_Hide, NULL},
    NULL
};

boolean turnin__InitializeClass(classID)
struct classheader *classID;
{
    turnin_menus = menulist_New();
    turnin_kmap = keymap_New();
    bind_BindList(turnin_bindings, turnin_kmap, turnin_menus, &turnin_classinfo);
    clockcursor = cursor_Create(NULL);
    cursor_SetStandard(clockcursor, Cursor_Wait);
    return TRUE;
}

void turnin__ReceiveInputFocus(self)
struct turnin *self;
{
    debug(("ReceiveFocus(turnin)\n"));
    turnin_WantInputFocus(self, self->whole);
}

void
turnin__PostMenus(self, ml)
struct turnin *self;
struct menulist *ml;
{
    super_PostMenus(self, self->menus);
}

void turnin__PostKeyState(self, ks)
struct turnin *self;
struct keystate *ks;
/* Want to add our own keybindings into the chain that gets passed to us */
{
    if (!ks) return;

    self->kstate->next = NULL;
    keystate_AddBefore(self->kstate, ks); 
    super_PostKeyState(self, self->kstate);
}

void
turnin__GoForIt(self)
struct turnin *self;
{
    strinput_SetInput(self->name, buffer_GetName(frame_GetBuffer(self->daddy->frame)));
    strinput_ClearInput(self->number); 
    value_SetValue(self->onoff, 1);
    turnin_WantUpdate(self, self->onoffv);
    message_DisplayString(self, MESSAGE, "");
    turnin_WantInputFocus(self, self->name);
}
