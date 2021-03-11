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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosaux.c,v 1.3 1992/12/15 21:55:51 rr2b R6tape $";
#endif


 
/*
 * eosaux.c
 *
 * This is the overflow from the eos.c module.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>
#include <class.h>
#include <atom.ih>
#include <buffer.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <eosbutt.ih>
#include <eos.h>
#include <eframe.ih>
#include <fontdesc.ih>
#include <frame.ih>
#include <im.ih>
#include <keystate.ih>
#include <lpair.ih>
#include <menulist.ih>
#include <pshbttn.ih>
#include <rect.h>
#include <view.ih>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <andrewos.h>	/* andrewos.h includes sys/types.h */
#include <eosfx.ih> /* eosfx.ih includes sys/types.h */
#undef label


#include <label.ih>
#include <labelv.ih>

#define AUXMODULE 1
#include <eos.eh>   /* eos.eh uses struct label */

extern void eos_PickUp();
extern void eos_TurnIn();
extern void eos_Handouts();
extern void eos_Exchange();
extern void eos_StyleGuide();
extern void eos_Help();
extern void eos_Grade();
extern void eos_Return();

extern struct menulist *eosMenu;
extern struct keymap *eosKeys;

static char *studentbuttonNames[]=
  { "Pick Up", "Turn In", "Handouts", "Exchange", "Guide", "Help", NULL};
static void (*studentbuttonFuncs[])()=
  { eos_PickUp, eos_TurnIn, eos_Handouts, eos_Exchange, eos_StyleGuide, eos_Help, NULL };

static char *instructbuttonNames[]=
  { "Grade","Return", "Handouts", "Exchange", "Guide", "Help", NULL };
static void (*instructbuttonFuncs[])()=
  { eos_Grade, eos_Return, eos_Handouts, eos_Exchange, eos_StyleGuide, eos_Help, NULL };

boolean eos__InitializeObject(classID, self)
struct classheader *classID;
register struct eos *self;
/* The view is made up: eos[screen[head[buttons, title], frame[editregion]]] */
{
    int i;
    char **buttonNames;
    void (**buttonFuncs)();
    struct lplist *lplist;
    struct buttonList  *blist;
    char *t; /* temporary string */
    char filename[128];
    long version; 

    lplist         = NULL;
    blist          = NULL;
    self->handouts = NULL;
    self->grades   = NULL;
    self->pickups  = NULL;
    self->exchanges= NULL;
    self->turnins  = NULL;
    self->displaywindow = NULL;
    self->course   = (char *) malloc(33);
    self->course[0] = '\0'; /* No course defined. njw 9/14/90 */
    self->program  = (char *) malloc(33);
    strcpy(self->program, "eos"); /* This should get overridden by application */
    self->dialogpri = environ_GetProfileInt("DialogPriority", 0);

    /* The editing buffer */
    self->editregion = buffer_FindBufferByName("Scratch");
    strcpy(filename, eosfx_LocalUnique("Scratch"));
    if (!self->editregion) self->editregion = buffer_Create("Scratch", filename, NULL, NULL);
    buffer_SetScratch(self->editregion, FALSE);

    version = dataobject_GetModified(buffer_GetData(self->editregion));

    buffer_SetCkpClock(self->editregion, 0);
    buffer_SetCkpVersion(self->editregion, version);
    buffer_SetWriteVersion(self->editregion, version);

    self->frame = frame_New();
    frame_SetCommandEnable(self->frame, TRUE);
    frame_SetBuffer(self->frame, self->editregion, TRUE);

    /* The title bar */
    self->title = label_New();
    self->titleV = labelview_New();
    labelview_SetDataObject(self->titleV, self->title);
    label_SetFlags(self->title, label_CENTERED);
    label_SetFont(self->title, "helvetica", fontdesc_Plain, 42);

    t = environ_Get("EOSTYPE");
    if (t && strcmp(t, "grading") == 0) {
	label_SetText(self->title, "GRADE: Editor");
	self->gradingflag = TRUE;
        buttonNames = instructbuttonNames;
	buttonFuncs = instructbuttonFuncs;
	self->paperatom = atom_Intern ("eos_paper");
    } else {
	label_SetText(self->title, "EOS: Editor");
	self->gradingflag = FALSE;
        buttonNames = studentbuttonNames;
	buttonFuncs = studentbuttonFuncs;
	self->paperatom = NULL;
    }

    /* Make the buttons */
    for ( i=0 ; buttonNames[i] != NULL ; i++ ) {
        blist = eosbutton_MakeButton(blist, buttonNames[i], buttonFuncs[i], (struct view *) self);
        if (buttonNames[i+1] != NULL) lplist = eosbutton_MakeLpair(lplist);
    } /* for - initializing buttons */

/*
 * The below lines glue the buttons into their lpairs, using the
 * linked lists blist and lplist
 * The lpairs are split so as each button is the same size
 * Making all buttons the same size is done by the code in the for loop, making
 * the n'th button be 1/n fraction of the size of the panel. 
 * i.e. The lpair_HSplit splits it 1/(n+1) to button and n/(n+1) to the previous
 * lpair.
 * The entire panel is finally placed into self->buttons
 */
    lpair_HSplit(lplist->lp, blist->next->buttv, blist->buttv, 50, FALSE);

    blist  = blist->next->next;
    i = 2;
    while (blist != NULL) {
        lpair_HSplit(lplist->next->lp, blist->buttv, lplist->lp, (long int) 100*i/(i+1), FALSE);
        lplist = lplist->next;
        blist  = blist->next;
        i++;
    }

    self->buttons = lplist->lp;
    self->head = lpair_Create(self->buttons, self->titleV, -40);

    /* We want the buttons to remain the same size all the time.
      and also, the title bar */
    self->screen = lpair_New();
    lpair_VTFixed(self->screen, self->head, self->frame, 75, 0);

    lpair_SetMovable(self->head, 0);
    lpair_SetMovable(self->screen, 0);

    /* Menus */
    self->menuflags = MENUS_general;
    if (self->gradingflag) self->menuflags |= MENUS_instructor;
    self->menus = menulist_DuplicateML(eosMenu, self);

    self->keys = keystate_Create(self, eosKeys);

   /* Place the end result into the view-tree */
    lpair_LinkTree(self->screen, self);
   
    return TRUE;
}

void eos__FinalizeObject(classID, self)
struct classheader *classID;
register struct eos *self;
{
    if (!self) return;

    if (self->screen) lpair_Destroy(self->screen);
    if (self->menus) menulist_Destroy(self->menus);
    if (self->course) free(self->course);
    if (self->program) free(self->program);
    if (self->keys) keystate_Destroy(self->keys);
}

void eos__LinkTree(self, parent)
struct eos *self;
struct view *parent;
{
    super_LinkTree(self, parent);

    if (self->screen)
        lpair_LinkTree(self->screen, self);
}

void eos__FullUpdate(self, type, left, top, width, height)
register struct eos *self;
enum view_UpdateType type;
long left, top, width, height;
/* Replace the 'eos' view with the view wanted: self->screen */
{
    struct rectangle childRect;

    eos_GetLogicalBounds(self, &childRect);
    lpair_InsertView(self->screen, self, &childRect);
    lpair_FullUpdate(self->screen, type, left, top, width, height);

}

struct view *eos__Hit(self, action, x, y, clicks)
struct eos *self;
enum view_MouseAction action;
long x, y, clicks;
/*
  All we want to do is to pass the hit into the sub-views
 */
{
    return lpair_Hit(self->screen, action, x, y, clicks);
}

void eos__PostKeyState(self, ks)
struct eos *self;
struct keystate *ks;
/* Want to add our own keybindings into the chain that gets passed to us */
{
    if (!ks) return;

    self->keys->next = NULL;
    keystate_AddBefore(self->keys, ks); 
    super_PostKeyState(self, self->keys);
}

void eos__SetTitle(self, title)
struct eos *self;
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

void eos_NewWindow(self)
    struct eos *self;
{
    struct eframe *new;
    register struct buffer *buffer;
    struct im *window;
    new = eframe_New();

    window = im_Create(NULL);
    im_SetView(window, new);

    buffer = frame_GetBuffer(self->frame);
    frame_SetBuffer(new->frame, buffer, TRUE);
    eframe_WantInputFocus(new, new);
}

