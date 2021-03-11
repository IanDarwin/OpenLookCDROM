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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/pickup.c,v 1.3 1992/12/15 21:56:26 rr2b R6tape $";
#endif


 
/*
 * pickup.c
 *
 * This does the work for pickup within the EOS applications.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>
#include <class.h>
#include <atom.ih>
#include <bind.ih>
#include <blank.ih>
#include <cursor.ih>
#include <environ.ih>
#include <eos.h>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else
      BEFORE the first use of struct label. */
#define label gezornenplatz
#include <eosfx.ih>
/* eosfx.ih includes sys/types.h */
#undef label

#include <eos.ih>   /* eos.ih uses struct label */
#include <fontdesc.ih>
#include <im.ih>
#include <label.ih>
#include <labelv.ih>
#include <lpair.ih>
#include <menulist.ih>
#include <message.ih>
#include <newbttnv.ih>
#include <pshbttn.ih>
#include <scroll.ih>
#include <text.ih>
#include <textv.ih>
#include <pickup.eh>	/* pickup.eh uses struct label */
#include <view.ih>

static struct cursor *clockcursor;
static struct menulist *menus;
#define pickupReadyToGo 1 /* menu flag indicating it's okay to show the continue option */

void CancelOperation(self, fxp)
struct pickup *self;
FX **fxp;
{
    eosfx_Close(fxp);
    pickup_WantInputFocus(self, self->textv);
    StopWaitCursor();
}

static void Hide( self, triggerer, rock)
struct pickup *self;
struct observable *triggerer;
long rock;
{
    if (!self) return;
    self->IDoNotExist = TRUE;
    /* "If I close my eyes, then you can't see me!" */
    im_VanishWindow(pickup_GetIM(self));
}

void pickup__DoPickUp(self)
register struct pickup *self;
{
/*
  open course;
  read list of GRADED pickup;
  display total;
  if (total = 0) then exit;
  foreach paper in list, until list exhausted {
      receive file;
      change status of paper to PICKEDUP
    display message;
  }
  display finished message;
  exit;
*/
    int i;
    Paper criterion, thispaper, tmppaper;
    Paperlist node;
    FX *fxp;
    struct im *thisIM;
    char *errormsg;
    char string[128];
    char filename[128];
    char openerr[256];
    boolean AskUser, pickup;
    char c;

    thisIM = pickup_GetIM(self);
    text_SetReadOnly(self->textobj, FALSE);
    text_ClearCompletely(self->textobj);
    textput("Reading course...\n");
    pickup_WantUpdate(self, self->textv);
    im_ForceUpdate();

    errormsg = eosfx_OpenCourse(self->course, &fxp);
    /* (fxp == NULL && errormsg) or (fxp is valid for reads&writes) */
    /* or (fxp is valid for reads, invalid for writes && errormsg) */
    if (!fxp) {
	/* We have no chance of continuing */
	textput(errormsg);
        textput("Press CONTINUE to close window");
        newbuttonview_Enable(self->okv, TRUE);
	self->menuflags = pickupReadyToGo;
        CancelOperation(self, NULL);
        return;
    } 
    /* Note: At this point, we may have a valid FXP, but not be able
     * to write to the server beacuse of some other error.
     * We continue anyway, doing reads only, to find out if there is
     * anything to read. If there isn't then we just keep quiet about the
     * error.
     * If there are things to pick up, we then start complaining to the user
     */
    if (errormsg)
	strcpy(openerr, errormsg); /* Keep a copy for later */
    else
	openerr[0] = '\0';  /* set it to the null string */

/* Make sure the list is 'clean' before we get a new list */
    eosfx_ListDestroy(&self->list);
    eosfx_DestroyPositions(&self->Positions);
 
/* Set the criterion - totally clean apart from type.
 * i.e. We want all GRADED papers belonging to this person.
 */
    eosfx_PaperClear(&criterion);
    criterion.type = GRADED;
    criterion.author = fxp->owner;

    if (errormsg = eosfx_List(fxp, &criterion, &self->list)) {
	/* If we get an error here, and we got one when we opened the course,
	 * we display the openCourse error, as that is probably more significant
	 */
	if (strcmp (openerr, ""))
	    textput(openerr);
	else
	    textput(errormsg);
        textput("\nPress CONTINUE to close window");
        newbuttonview_Enable(self->okv, TRUE);
	self->menuflags = pickupReadyToGo;
        CancelOperation(self, &fxp);
        return;
    }

/* Count the papers and place them into a paperPositions list */
    i = 0;
    for (node = self->list->Paperlist_res_u.list; node !=NULL; node = node->next) {
        eosfx_AddPaperText(&self->Positions, node, 0, 0);
        i++;
    }

    if (!i) {
        textput("No assignments have been returned.\n");
        textput("Press CONTINUE to close window");
    } else {
        if (i != 1)
	    sprintf(string, "There are %d assignments ready to pickup\n", i);
        else 
	    sprintf(string, "There is one paper to pick up.\n");
        textput(string);

	if (strcmp(openerr, "")) {
	    /* We can continue to pickup, but cannot tell the server about
	     * the collection
	     */
	    textput("\nHowever, there is a problem with the EOS server:\n");
	    textput(openerr);
	    textput("If you continue, the system will not know that you have collected these assignments, and next time you pickup, you will get them again.\n");
	    textput("Do you want to continue? (press 'y' or 'n') [y] >");
	    pickup_WantUpdate(self, self->textv);
	    im_ForceUpdate();
	    c = im_GetCharacter(thisIM);
	    if (c == 'y' || c == 'Y' || c == '\r' || c == '\n') {
		textput("Yes.\n");
		textput("Continuing...\n\n");
	    } else {
		textput("No.\n");
		textput("Cancelled.\n");
		textput("Press CONTINUE to close window");
		newbuttonview_Enable(self->okv, TRUE);
		self->menuflags = pickupReadyToGo;
		CancelOperation(self, &fxp);
		return;
	    }
	}
	AskUser = environ_GetProfileSwitch("InteractivePickup", FALSE);
	pickup  = TRUE;
        textput("Collecting...\n\n");
        pickup_WantUpdate(self, self->textv);
        im_ForceUpdate();
        while (self->Positions != NULL) {
            node = self->Positions->paper;
            self->Positions = self->Positions->next;
            thispaper = node->p;
	    if (AskUser) {
		StopWaitCursor();
		sprintf(string, "%s (#%d) - Collect? (press 'y' or 'n') >", thispaper.filename, thispaper.assignment);
		textput(string);
		pickup_WantUpdate(self, self->textv);
		im_ForceUpdate();
		c = im_GetCharacter(thisIM);
		if (c == 'y' || c == 'Y' || c == '\r' || c == '\n') {
		    pickup = TRUE;
		    textput("Yes\n");
		    pickup_WantUpdate(self, self->textv);
		    im_ForceUpdate();
		} else {
		    pickup = FALSE;
		    textput("No\n");
		}
		StartWaitCursor();
	    }
	    if (pickup) {
		strcpy(filename, eosfx_LocalUnique(thispaper.filename));
		if (errormsg = eosfx_RetrieveFile(fxp, &(thispaper), filename)) {
		    textput(errormsg);
		    textput("Pick Up aborted. Press CONTINUE to close window");
		    newbuttonview_Enable(self->okv, TRUE);
		    self->menuflags = pickupReadyToGo;
		    CancelOperation(self, &fxp);
		    return;
		}
		eosfx_PaperCopy(&thispaper, &tmppaper);
		tmppaper.type = PICKEDUP;
		if (errormsg = eosfx_Move (fxp, &thispaper, &tmppaper)) {
		    textput(errormsg);
		    textput("Pick up aborted. Press the CONTINUE button to close this window.");
		    newbuttonview_Enable(self->okv, TRUE);
		    self->menuflags = pickupReadyToGo;
		    CancelOperation(self, &fxp);
		    return;
		}
		eos_SetBuffer(self->daddy, filename, 0);
		sprintf(string, "Picked up assignment #%d as '%s'.\n", node->p.assignment, filename);
		textput(string);
	    }
            pickup_WantUpdate(self, self->textv);
            im_ForceUpdate();
        }
        textput("\nCollection completed. Press CONTINUE to close window");
    } 

    newbuttonview_Enable(self->okv, TRUE);
    self->menuflags = pickupReadyToGo;
    CancelOperation(self, &fxp);
    return;
}

static struct bind_Description pickupBindings[] = {
    {"pickup-continue", NULL, 0, "Continue...", 0, pickupReadyToGo, Hide, NULL},
    NULL
};


boolean pickup__InitializeClass(classID)
struct classheader *classID;
{
    menus = menulist_New();
    bind_BindList(pickupBindings, NULL, menus, &pickup_classinfo);
    clockcursor = cursor_Create(NULL);
    cursor_SetStandard(clockcursor, Cursor_Wait);
    return TRUE;
}

void pickup__SetTitle(self, title)
struct pickup *self;
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


boolean pickup__InitializeObject(classID, self)
struct classheader *classID;
struct pickup *self;
{
    int style;
    struct blank *blank;
    struct lpair *b1, *b2;

    self->menuflags  = 0;
    self->Positions  = NULL;
    self->daddy      = NULL;
    self->list       = NULL;
    self->title	     = label_New();
    self->titleV     = labelview_New();
    self->course     = (char *) malloc(33);
    if (!self->title || !self->titleV) return FALSE;

    self->menus = menulist_DuplicateML(menus, self);

/* Create the title */
    labelview_SetDataObject(self->titleV, self->title);
    label_SetFlags(self->title, label_CENTERED);
    label_SetText(self->title, "Pick Up");
    label_SetFont(self->title, "helvetica", fontdesc_Plain, 24);

/* Create the text junk */
    self->textobj = text_New();
    self->textv   = textview_New();
    if (!self->textobj || !self->textv) return FALSE;
    textview_SetDataObject(self->textv, self->textobj);
    self->scroll  = scroll_Create(self->textv, scroll_LEFT);
    if (!self->scroll)
	return FALSE;

/* Create the button */
    style = environ_GetProfileInt("buttonstyle", 2);
    self->okb = pushbutton_New();
    self->okv = newbuttonview_New();
    if (!self->okb || !self->okv) return FALSE;
    pushbutton_SetStyle(self->okb, style);
    pushbutton_SetText(self->okb, "CONTINUE");
    newbuttonview_SetDataObject(self->okv, self->okb);
    newbuttonview_AddRecipient(self->okv, atom_Intern("buttonpushed"), (struct view *) self, Hide, 0L);
    newbuttonview_Enable(self->okv, FALSE);

    b1 = lpair_New();
    b2 = lpair_New();
    blank = blank_New();
    lpair_HFixed(b1, blank, self->okv, 100, FALSE);
    blank = blank_New();
    lpair_HSplit(b2, b1, blank, 40, FALSE);

/* Create the view itself */
    self->topscreen = lpair_New();
    self->whole     = lpair_New();
    if (!self->whole || !self->topscreen) return FALSE;
    lpair_VSplit(self->topscreen, self->titleV, self->scroll, 85, FALSE);
    lpair_SetMovable(self->topscreen, FALSE);
    lpair_VSplit(self->whole, self->topscreen, b2, 20, FALSE);
    lpair_LinkTree(self->whole, self);

    return TRUE;
}

void pickup__FinalizeObject(classID, self)
struct classheader *classID;
struct pickup *self;
{
    /* Does nothing for now */
}


void
pickup__PostMenus(self, ml)
struct pickup *self;
struct menulist *ml;
{
    menulist_SetMask(self->menus, self->menuflags);
    super_PostMenus(self, self->menus);
}

void pickup__LinkTree(self, parent)
struct pickup *self;
struct view *parent;
{
    super_LinkTree(self, parent);

    if (self->whole)
        lpair_LinkTree(self->whole, self);
}

void pickup__FullUpdate(self, type, left, top, width, height)
register struct pickup *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle childRect;

    if (self->IDoNotExist) self->IDoNotExist = FALSE;

    pickup_GetLogicalBounds(self, &childRect);

    lpair_InsertView(self->whole, self, &childRect);
    lpair_FullUpdate(self->whole, type, left, top, width, height);
}

struct view *pickup__Hit(self, action, x, y, clicks)
struct pickup *self;
enum view_MouseAction action;
long x, y, clicks;
{
    return lpair_Hit(self->whole, action, x, y, clicks);
}

void pickup__ReceiveInputFocus(self)
struct pickup *self;
{
    pickup_WantInputFocus(self, self->textv);
}
