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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/util/RCS/popts.c,v 1.17 1994/03/01 23:25:11 rr2b Exp $";
#endif


#include <stdio.h>
/* sys/types.h in AIX PS2 defines "struct label", causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else in the preprocessor. */
#define label gezornenplatz
#include <andrewos.h> /* strings.h */
#undef label
#include <util.h>
#include <ctype.h>
#include <class.h>
#include <rect.h>

#include <atom.ih>	/* For all the strings we pass to the value */
#include <atomlist.ih>	/* ditto */
#include <bind.ih>	/* Binding menus */
#include <bpair.ih>	/* lpairs without lines in between */
#include <buffer.ih>	/* For when we create the window */
#include <checkv.ih>	/* Checklist widget */
#include <dataobj.ih>	/* General observer type things */
#include <environ.ih>	/* For changing print options */
#include <graphic.ih>	/* Drawing boxes and stuff */
#include <fontdesc.ih>	/* for face codes */
#include <frame.ih>	/* For creating the new window in */
#include <im.ih>	/* For the new window */
#include <event.ih>	/* For im_SetInteractionEvent() to DestroyWindow */
#include <keymap.ih>	/* Shortcuts for the menus */
#include <keystate.ih>	/* Needed, since we are playing with keymaps */
#include <label.ih>	/* Titles, etc */
#include <labelv.ih>	/* ditto */
#include <lpair.ih>	/* General layout */
#include <menulist.ih>	/* Menus */
#include <message.ih>	/* General messages */
#include <strinput.ih>	/* For text input - printername */

#ifdef POPTS_USE_SUITE
#include <suite.ih>	/* For the buttons */
#else /* POPTS_USE_SUITE */
#include <pshbttn.ih>
#include <pshbttnv.ih>
#endif /* POPTS_USE_SUITE */

#include <text.ih>	/* General text */
#include <value.ih>	/* To help out with checklists */
#include <view.ih>	/* Our parent */

#include <popts.eh>

static boolean  debug = FALSE;
#define DEBUG(s) {if (debug) {printf s ; fflush(stdout);}}
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))

/* Keep the following in sync with the values
 defined in txttroff.c  (we should consolidate
 them into a .h file) */

#define ENDNOTESONLY FALSE
#define	CONTENTSBYDEFAULT FALSE
#define AUTOENUMERATEDEFAULT FALSE
#define DUPLEXBYDEFAULT FALSE

#define BASERESOLUTION 72

#define SCALEWIDTH(v, x) (x*view_GetHorizontalResolution((struct view *)(v))/BASERESOLUTION)
#define SCALEHEIGHT(v, y) (y*view_GetVerticalResolution((struct view *)(v))/BASERESOLUTION)
#define DIMENSION_X SCALEWIDTH(vp_IM, 350)
#define DIMENSION_Y SCALEHEIGHT(vp_IM, 99)
#define INSET_X 1
#define INSET_Y 1

static int CheckType = 0L;
static int NumberOfChecks;
static struct menulist *Menus;
static struct keymap *Keys;
static struct Option options[] = {
#ifdef LANDSCAPE
    { "Print in	Landscape mode", "doc-landscape",
    "Portrait", PROFILETYPESWITCH, "no", "yes", NULL, 0 },
#endif

    { "Print table of contents with documents", "doc-contents",
    "PrintContents", PROFILETYPESWITCH, "yes", "no", NULL, CONTENTSBYDEFAULT },

    { "Enumerate contents automatically", "doc-enumerate",
    "AutoEnumerate", PROFILETYPESWITCH, NULL, NULL, NULL, AUTOENUMERATEDEFAULT },

#ifdef POPTSINDEX
    { "Print index with documents", "doc-index", 
    "PrintIndex", PROFILETYPESWITCH, "yes", "no", NULL, 0 },

#endif /* POPTSINDEX */
    { "Print footnotes at end of documents", "doc-footnotes",
    "Endnotes", PROFILETYPEEXISTS, "yes", NULL, NULL, ENDNOTESONLY },

    { "Swap right and left headers on even pages", "doc-duplex", "Duplex", PROFILETYPESWITCH, "yes", "no", NULL, DUPLEXBYDEFAULT },

    { NULL, NULL, NULL, PROFILETYPENONE, 0, 0, NULL, 0 }
};

int doValueChange();
void CreateWindow();
void DestroyWindow();
struct view *Control();
void ApplyValues();
void ResetValues();
void MenuDone();
void MenuCancel();
void PrinterSet();
void printopts_Nop();

static struct bind_Description MenuOptions[] = {
    {"printopts-post-window", NULL, 0, "Set Print Options", 0, 0, CreateWindow, "Set options for printing of document", NULL},
    NULL
};
static struct bind_Description PrivateOptions[] = {
    {"printopts-nop", "\012", 0, NULL, 0, 0, printopts_Nop, NULL},
    {"printopts-nop", "\015", 0, NULL, 0, 0, printopts_Nop, NULL},
    {"printopts-apply", NULL, 0, "Apply Values", 0, 0, ApplyValues, NULL},
    {"printopts-done", NULL, 0, "Done", 0, 0, MenuDone, NULL},
    {"printopts-cancel", NULL, 0, "Cancel", 0, 0, MenuCancel, NULL},
    NULL
};

/* To communicate resources to valueviews, the strings should be formed
 * into atomlists. This is done once for the major strings, to save cycles
 */
static struct atomlist *AL_bodyfont;
static struct atomlist *AL_bodyfont_size;
static struct atomlist *AL_label;
static struct atomlist *AL_checktype;
static struct atomlist *AL_borderwidth;
static struct atom *A_long;
static struct atom *A_string;
#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("label") ,\
   AL_checktype = atomlist_StringToAtomlist("checktype") ,\
   AL_borderwidth = atomlist_StringToAtomlist("border-width"), \
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )


#ifdef POPTS_USE_SUITE
/* THis is not nice, unfortunately there are not many nice ways of doing this */
#define ApplyCode 1
#define DoneCode 2
#define CancelCode 3
static suite_Specification apply[] = {
    suite_ItemCaption("Apply"),
    suite_ItemDatum(ApplyCode),
    NULL
};
static suite_Specification done[] = {
    suite_ItemCaption("Done"),
    suite_ItemDatum(DoneCode),
    NULL
};
static suite_Specification cancel[] = {
    suite_ItemCaption("Cancel"),
    suite_ItemDatum(CancelCode),
    NULL
};

suite_Specification control_spec[] = {
#if 0
    suite_Item(apply),
#endif
    suite_Item(done),
    suite_Item(cancel),
    suite_ItemCaptionFontName( "AndySans10b" ),
    suite_GutterSize( 5 ),
    suite_BorderSize( 0 ),
    suite_ItemOrder( suite_ColumnMajor ),
    suite_Arrangement( suite_Matrix ),
    suite_SelectionMode( suite_Exclusive ),
    suite_HitHandler(Control),
    NULL
};

#else /* POPTS_USE_SUITE */
/* Structure is linked list of buttons */   
  
static char *printopts_bNames[] = 
{ "Done", "Cancel", NULL };

static void (*printopts_bFuncs[])()=
{ MenuDone, MenuCancel, NULL };

static struct buttonList *printopts_MakeButton(blist, text, function, object) 
struct buttonList *blist;
char *text;
void (*function)();
struct view *object;
/* Creates a new button and with given attributes
   returns [newbutton::blist]
 */
{
    struct buttonList *button = (struct buttonList *)     malloc(sizeof(struct buttonList));

    button->butt  = pushbutton_New();
    button->buttv = pushbuttonview_New();
    pushbutton_SetStyle(button->butt, 1);
    pushbutton_SetText(button->butt, text);
    pushbuttonview_SetDataObject(button->buttv, button->butt);
    pushbuttonview_AddRecipient(button->buttv,   atom_Intern("buttonpushed"), object, function, 0L);
      
    button->next = blist;
    return button;
}

static struct lplist *printopts_MakeLpair(lpl)
struct lplist *lpl;
/* Creates a new lpair and returns [newlpair::lplist]
 */
{
    struct lplist *lpair = (struct lplist *) malloc(sizeof(struct lplist));
    
    lpair->lp = lpair_New();
    lpair->next = lpl;
    return lpair;
}


#endif /* POPTS_USE_SUITE */

boolean
printopts__InitializeClass(classID)
struct classheader *classID;
{
    struct classinfo *viewClassinfo;
   
    for (NumberOfChecks = 0; options[NumberOfChecks].name != NULL; NumberOfChecks++);

    InternAtoms;

    /* We want one or two of our own local functions */
    Menus = menulist_New();
    Keys  = keymap_New();
    bind_BindList(PrivateOptions, Keys, Menus, &printopts_classinfo);

    /* We want our function bound to view, not to self */
    viewClassinfo = class_Load("view");
    if (viewClassinfo != NULL) {
        bind_BindList(MenuOptions, NULL, NULL, viewClassinfo);
        return TRUE;
    }
    return FALSE;
}

#define CheckHeight 18

boolean
printopts__InitializeObject(classID, self)
struct classheader *classID;
register struct printopts  *self;
{
    long i;
    struct view *dummy;
    struct view *oldchecklist;
#ifndef POPTS_USE_SUITE
    struct lplist     *lplist = NULL;
    struct buttonList *blist = NULL;
#endif /* POPTS_USE_SUITE */
    struct lpair *lp;
    char *t;

    ENTER(printopts__InitializeObject);

    self->embedded = TRUE;
    self->OnScreen = FALSE;
    self->ResourcesPosted = FALSE;
    self->keys  = keystate_Create(self, Keys);
    self->menus = menulist_DuplicateML(Menus, self);

    self->values = (int *)calloc (NumberOfChecks, sizeof(int));

    /* copyin the options from static storage */
    /* Don't free the strings, they're static */
    for (i = 0; i < NumberOfChecks; i++)
	self->values[i]	= options[i].value;

    CheckType = environ_GetProfileInt("CheckType", 1);

    self->printername = strinput_New();
    strinput_SetPrompt(self->printername, "Name of printer: ");
    if((t = environ_Get("LPDEST")) == NULL)
    	if((t = environ_Get("PRINTER")) == NULL)
	    t = environ_GetProfile("spooldir");
    if (t)
	strncpy(self->pnamevalue, t, 80);
    else
	strcpy(self->pnamevalue, "");
    strinput_SetInput(self->printername, self->pnamevalue);

#ifdef POPTS_USE_SUITE
    self->suite = suite_Create(control_spec, self);
#endif /* POPTS_USE_SUITE */

    dummy = view_New();

    /* Construct the checklist */
    self->checklist = (struct bpair *) dummy;
    for (i = 0; i < NumberOfChecks; i++) {
	self->check[i].obj  = value_New();
	self->check[i].view = checkv_New();
	/* The rest of the setting up of the options takes place in FullUpdate */
	oldchecklist = (struct view *)self->checklist;
	self->checklist = bpair_New(); /* it accumulates */
	bpair_VFixed(self->checklist, oldchecklist,
		     self->check[i].view, CheckHeight, 0);
    }
    lp = lpair_New();
    lpair_VFixed(lp, self->printername, self->checklist, NumberOfChecks*CheckHeight + 2, FALSE); /* Doesn't account for resolution of device.... */
    self->image = lpair_New();

#ifdef POPTS_USE_SUITE
    self->image = lpair_HSplit(self->image, lp, self->suite, 20, FALSE);
#else /* POPTS_USE_SUITE */

    /* Yucky code follows! Make the sets of buttons */
    self->blist = NULL;
    self->lplist = NULL;
    for ( i=0 ; printopts_bNames[i] != NULL ; i++ ) {
	self->blist = printopts_MakeButton(self->blist, printopts_bNames[i], printopts_bFuncs[i], (struct view *) self);
	if (printopts_bNames[i+1] != NULL) 
	    self->lplist = printopts_MakeLpair(self->lplist);
    } /* for - initializing buttons */

    /*
      * The below lines glue the buttons into their lpairs, using the
      * linked lists blist and lplist
      * The lpairs are split so as each button is the same size
      * Making all buttons the same size is done by the code in the while loop, making
	  * the n'th button be 1/n fraction of the size of the panel. 
  * i.e. The lpair_HSplit splits it 1/(n+1) to button and n/(n+1) to the previous lpair.
  * The entire panel is finally placed into self->buttons
  */

    blist = self->blist;
    lplist = self->lplist;

    lpair_VSplit(lplist->lp, blist->next->buttv, blist->buttv, 50, FALSE);
    blist  = blist->next->next;
    i = 2;
    while (blist != NULL) {
	lpair_VSplit(lplist->next->lp, blist->buttv, lplist->lp, (long int) 100*i/(i+1), FALSE);
	lplist = lplist->next;
	blist  = blist->next;
	i++;
    } 

    self->image = lpair_HSplit(self->image, lp, lplist->lp, 20, FALSE);

#endif /* POPTS_USE_SUITE */
    self->pname=lp;
LEAVE(printopts__InitializeObject);
    return TRUE;
}

void 
printopts__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
register struct printopts  *self;
{
    struct bpair *checklist, *nextlist;
    struct lpair *leftSide = (struct lpair*) lpair_GetNth(self->image,0);
    register int j;
#ifndef POPTS_USE_SUITE
    struct buttonList *blist, *next_blist;
    struct lplist *lplist, *next_lplist;
#endif /* POPTS_USE_SUITE */
    register struct checkv *v;
    register struct value *o;

    ENTER(printopts__FinalizeObject);
    if(self->keys) keystate_Destroy(self->keys);
    if(self->menus) menulist_Destroy(self->menus);
    if(self->values) free(self->values);

    lpair_UnlinkTree(self->image);
    lpair_Destroy(self->image);
    lpair_Destroy(leftSide); /* Contains strinput and skewed bpair of checkv's. */
/*
  lpair_Destroy(rightSide); /* Contains lplist->lp; We deal with that below.
*/

    strinput_Destroy(self->printername);

    checklist = self->checklist;
    for (j = NumberOfChecks - 1; j >= 0; j--) {
	o = self->check[j].obj;
	v = self->check[j].view;
	if(j > 0) {
	    nextlist = (struct bpair*) bpair_GetNth(checklist, 0);
	    bpair_Destroy(checklist);
	    checklist = nextlist;
	}
	else {
	    struct view *dummy = bpair_GetNth(checklist, 0);
	    bpair_Destroy(checklist);
	    view_Destroy(dummy);
	}
	checkv_Destroy(v);
/* Disable any callback that may be generated when the value object is destroyed. */
	value_RemoveCallBack(o, self, doValueChange);
	if (options[j].func)
	    value_RemoveCallBack(o, self, options[j].func);
	value_RemoveCallBackObserver(o, self);
	value_Destroy(o);
    }

#ifdef POPTS_USE_SUITE
    suite_Destroy(self->suite);
#else
    lplist = self->lplist;
    blist = self->blist;
    while(lplist) {
	next_lplist = lplist->next;
/* We have to set these because we're walking the skewed tree from the bottom. */
	lplist->lp->obj[0] = NULL;
	lplist->lp->obj[1] = NULL;
	lpair_Destroy(lplist->lp);
	free(lplist);
	lplist = next_lplist;
    }
    while(blist) {
	next_blist = blist->next;
	pushbuttonview_Destroy(blist->buttv);
	pushbutton_Destroy(blist->butt);
	free(blist);
	blist = next_blist;
    }
#endif /* POPTS_USE_SUITE */
    LEAVE(printopts__FinalizeObject);
}

struct frame *
printopts__GetApplicationLayer(self)
struct printopts *self;
{
    struct frame *frame;

    self->embedded = FALSE;
    frame = frame_New();
    frame_SetView(frame, self);
    frame_PostDefaultHandler(frame, "message", frame_WantHandler(frame, "message"));

    return frame;
}

void
printopts__PostMenus(self, ml)
struct printopts *self;
struct menulist *ml;
{
    super_PostMenus(self, self->menus);
}

void printopts__PostKeyState(self, ks)
struct printopts *self;
struct keystate *ks;
/* Want to add our own keybindings into the chain that gets passed to us */
{
    self->keys->next = NULL;
    keystate_AddBefore(self->keys, ks); 
    super_PostKeyState(self, self->keys);
}

void 
printopts__FullUpdate(self, type, left, top, width, height)
register struct printopts  *self;
register enum view_UpdateType  type;
register long  left, top, width, height;
{
    struct rectangle r;
    self->OnScreen = (type != view_Remove);
    printopts_GetLogicalBounds(self, &r);
    DEBUG(("FullUpdate type %d  redraw (%d,%d,%d,%d) within (%d,%d,%d,%d)\n", 
	    type, left, top, width, height, r.left, r.top, r.width, r.height));

    /* Now that we are updating, the views exist, so we can set up those views */
    if (!self->ResourcesPosted) {
	register int i;
	register struct checkv *v;
	register struct value *o;

	/* Checklist */
	ResetValues(self);
	for (i = 0; i < NumberOfChecks; i++) {
	    struct atomlist *cname = atomlist_StringToAtomlist( options[i].name);
	    v = self->check[i].view;
	    o = self->check[i].obj;
	    checkv_SetName(v, cname);
	    atomlist_Destroy(cname);
	    checkv_PostResource(v, AL_checktype, A_long, CheckType);
	    checkv_PostResource(v, AL_borderwidth, A_long, 0L);
	    checkv_PostResource(v, AL_label, A_string, options[i].label);
	    checkv_PostResource(v, AL_bodyfont, A_string, "andy");
	    checkv_PostResource(v, AL_bodyfont_size, A_long, 12);
	    value_AddCallBackObserver(o, self, doValueChange, i);
	    if (options[i].func)
		value_AddCallBackObserver(o, self, options[i].func, i);
	    value_SetValue(o, self->values[i]);
	    checkv_SetDataObject(v, o);
	}
    }
    self->ResourcesPosted = TRUE;

    if (type == view_FullRedraw && self->embedded && self->OnScreen) {
	printopts_SetTransferMode(self, graphic_COPY);
	printopts_MoveTo(self, 0, 0);
	printopts_DrawLineTo(self, r.width-INSET_X, 0);
	printopts_DrawLineTo(self, r.width-INSET_X, r.height-INSET_Y);
	printopts_DrawLineTo(self, 0, r.height-INSET_Y);
	printopts_DrawLineTo(self, 0, 0);

	r.top+=INSET_Y, r.left+=INSET_X, r.height-=(2*INSET_Y), r.width-=(2*INSET_X);
    }

    DEBUG(("	Drawable at 0x%lx\n", printopts_GetDrawable(self)));

    if (type != view_PartialRedraw 
	 && type != view_LastPartialRedraw) {
	lpair_LinkTree(self->image, self);
	lpair_InsertView(self->image, self, &r);
    }
    lpair_FullUpdate(self->image,  type, 0, 0, r.width, r.height);
    LEAVE(printopts__FullUpdate);
}


void 
printopts__Update( self )
register struct printopts *self;
{
    ENTER(printopts__Update);
    lpair_Update(self->image);
    LEAVE(printopts__Update);
}


struct view *
printopts__Hit(self, action, x, y, num_clicks)
register struct printopts  *self;
register enum view_MouseAction  action;
register long  x, y, num_clicks;
{
    lpair_Hit(self->image, action, x-INSET_X, y-INSET_Y, num_clicks);
    printopts_WantInputFocus(self, self->printername);
    return (struct view*) self;
}

enum view_DSattributes
printopts__DesiredSize( self, width, height, pass, 
			desiredWidth, desiredHeight ) 
register struct printopts *self;
long width;
long height;
enum view_DSpass pass;
long *desiredWidth;
long *desiredHeight;
{
    *desiredWidth = 550,  *desiredHeight = 322;
    DEBUG(("Desired Size %d x %d\n", *desiredWidth, *desiredHeight));
    return view_Fixed;
}

void
printopts__Print( self, file, processor, format, level )
register struct printopts 	 *self;
register FILE   file;
register char  	 *processor;
register char  	 *format;
register boolean  	level;
{
    /* never print anything */
}


int
doValueChange(self, observed, rock, rock2)
register struct printopts *self;
register struct value *observed;
register long rock;
register long rock2;
{
    self->values[rock] = value_GetValue(observed);
}

void
PrinterSet(self, rock)
register struct printopts *self;
register long rock;
{
    strcpy(self->pnamevalue, strinput_GetInput(self->printername, 80));
}

/* We use this to override newline self-insert. */
void
printopts_Nop(self, rock)
register struct printopts *self;
register long rock;
{
}

void
ResetValues(self)
register struct printopts *self;
{
    int i;
    char *t;

    for (i = 0; i < NumberOfChecks; i++) {
	if (options[i].func == NULL && options[i].env) {
	    if ((t = environ_Get(options[i].env)) == NULL) {
		switch (options[i].profiletype) {
		    case PROFILETYPENONE:
			break;
		    case PROFILETYPEEXISTS:
			if(environ_GetProfile (options[i].env) != NULL)
			    self->values[i] = 1;
			else
			    self->values[i] = 0;
			break;
		    case PROFILETYPESWITCH:
			self->values[i] = environ_GetProfileSwitch (options[i].env, options[i].value);
			break;
		}
	    } else {
		if (options[i].envOn && strcmp(t, options[i].envOn) == 0) 
		    self->values[i] = 1;
		else
		    self->values[i] = 0;
	    }
	}
    }
    t = environ_Get("LPDEST");
    if (t == NULL)
	t = environ_Get("PRINTER");
    if (t)
	strncpy(self->pnamevalue, t, 80);
    else
	strcpy(self->pnamevalue, "");
    strinput_SetInput(self->printername, self->pnamevalue);
}

#ifdef POPTS_USE_SUITE
struct view *
Control(self, suite, item, object, action, x, y, clicks)
register struct printopts *self;
register struct suite *suite;
register long object;
enum view_MouseAction action;
long x, y, clicks;
{
    if ((action == view_LeftUp) || (action == view_RightUp)) {
	if(item && (object == suite_ItemObject)) {
	    switch(suite_ItemAttribute(suite, item, suite_ItemData(0))) {
		case ApplyCode:
		    ApplyValues(self, 0);
		    break;
		case DoneCode:
		    MenuDone(self, 0);
		    break;
		case CancelCode:
		    MenuCancel(self, 0);
		    break;
	    }
	}
    }
    suite_Reset(suite, suite_Normalize);
    return NULL;
}
#endif /* POPTS_USE_SUITE */

void
MenuDone(self, rock)
register struct printopts *self;
register long rock;
{
    ApplyValues(self, 0);

/* Avoid destroying window while within pushbuttonv_PullTrigger(). */
    im_ForceUpdate();
    im_EnqueueEvent(DestroyWindow, self, 0);
}

void
MenuCancel(self, rock)
register struct printopts *self;
register long rock;
{
    int i;

    ResetValues(self);
    for (i = 0; i < NumberOfChecks; i++) {
	value_SetValue(self->check[i].obj, self->values[i]);
    }

/* Avoid destroying window while within pushbuttonv_PullTrigger(). */
    im_ForceUpdate();
    im_EnqueueEvent(DestroyWindow, self, 0);
}


void
ApplyValues(self, rock)
register struct printopts *self;
register long rock;
{
    int i;
    for (i = 0; i < NumberOfChecks; i++) {
	if (options[i].func == NULL && options[i].env) {
	    if (self->values[i]){
		if (options[i].envOn)
		    environ_Put(options[i].env, options[i].envOn);
		else
		    environ_Delete(options[i].env);
	    } else {
		if (options[i].envOff)
		    environ_Put(options[i].env, options[i].envOff);
		else
		    environ_Delete(options[i].env);
	    }
	}
    }
    strcpy(self->pnamevalue, strinput_GetInput(self->printername, 80));
    if (*self->pnamevalue) {
	environ_Put("LPDEST", self->pnamevalue);
	environ_Put("PRINTER", self->pnamevalue);
    }
}

void
CreateWindow(vp, rock)
struct view *vp;
long rock;
{
    long left, top, width, height;
    struct printopts *self;
    struct im *vp_IM, *im;

    message_DisplayString(vp, 0, "Creating Dialog Box.  Please Wait.");
    im_ForceUpdate();

    self = printopts_New();
    if (!self) {
	message_DisplayString(vp, 100, "Could not create the options.\nPerhaps we're out of memory?");
	return;
    }

    im_GetPreferedDimensions(&left, &top, &width, &height);
    
#define USEOVERRIDE

#ifdef USEOVERRIDE
    if ((vp_IM = view_GetIM(vp)) == NULL) {
	printf ("Attempt to create dialogue box for nonexistant view!\n Creating top level window.");
	im_SetPreferedDimensions(left, top, DIMENSION_X, DIMENSION_Y);
	im = im_Create(NULL);
    } else {
	im_SetPreferedDimensions(0, 0,DIMENSION_X, DIMENSION_Y);
	im = im_CreateOverride(vp_IM);
    }
#else /* USEOVERRIDE */
    im_SetPreferedDimensions(left, top, DIMENSION_X, DIMENSION_Y);
    im = im_Create(NULL);
#endif /* USEOVERRIDE */
    if (im) {
	im_SetTitle(im, "Print Options");
	im_SetView(im, self);
    } else {
	message_DisplayString(vp, 100, "Could not create a new window\nPerhaps there isn't enough memory?");
	printopts_Destroy(self);
	return;
    }

    im_SetPreferedDimensions(left, top, width, height);
    message_DisplayString(vp, 0, "Done.");

    printopts_WantInputFocus(self, self->printername);
}   

void
DestroyWindow( self )
register struct printopts *self;
{
    struct im *pim=printopts_GetIM(self);
    if(pim) im_SetView(pim, NULL);
    printopts_Destroy(self);
    if(pim) im_Destroy(pim);
}

void
printopts__LinkTree( self, parent )
register struct printopts *self;
register struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->image)
	lpair_LinkTree(self->image, self);
    if (parent && view_GetIM(parent)) {
	int i;
	struct bpair *bp=self->checklist;
	struct view *dummy;
	struct classinfo *bpairinfo=NULL;

	if(bpairinfo==NULL) bpairinfo=class_Load("bpair");

	while(bp && class_GetType(bp)==bpairinfo) {
	    bpair_VFixed(bp, bpair_GetNth(bp, 0), bpair_GetNth(bp, 1), SCALEHEIGHT(parent, CheckHeight), 0);
	    bp=(struct bpair *)bpair_GetNth(bp, 0);
	}
	lpair_VFixed(self->pname, self->printername, self->checklist, SCALEHEIGHT(parent, NumberOfChecks*CheckHeight) + 2, FALSE); 
    }
}
