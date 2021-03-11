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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/frame/RCS/frame.c,v 2.51 1993/11/24 00:37:19 gk5g Exp $";
#endif


 

/* frame.c
 * Provides the toplevel view on a buffer and message line.
  */
#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <im.ih>
#include <view.ih>
#include <dataobj.ih>
#include <buffer.ih>
#include <style.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <framev.ih>
#include <framecmd.ih>
#include <framemsg.ih>
#include <cursor.ih>
#include <graphic.ih>
#include <proctbl.ih>
#include <message.ih>
#include <path.ih>
#include <text.ih>
#include <dialogv.ih>
#include <frame.eh>

#ifndef MAX
#define MAX(A,B) ((A > B) ? A: B)
#endif /* MAX */

static struct frame *allFrames = NULL; /* Maintains a list of all frames for enumeration purposes. */
static struct keymap *frameKeymap = NULL, *frameDefaultKeymap=NULL;
static struct menulist *frameMenus = NULL, *frameDefaultMenus=NULL;

static void TidyUp();
static void ComputeSize();
static void DoUpdate();
static void SaveBits();
static void PrepareMenus();
static void PurifyString();
static boolean RestoreBits();
static void CannotRestoreBits();
static void retractCursors();
static boolean InRectangle();

#define DEFAULTHEIGHT 20

#define ACCEPT 1
#define CANCEL 2
#define COMPLETE 3
#define HELP 4
#define frame_SetUpperShade(SELF,P) frame_setShade(SELF,((P)?200:(self->mono ?25:45)))
#define frame_SetLowerShade(SELF,P) frame_setShade(SELF,((P)?(self->mono ?25:45):200))
#define FORESHADE 30
struct pendingupdates {
    struct view *v;
    struct pendingupdates *next;
};

#define frame_FONTSIZE 12
#define frame_FONTPAD 10 /* bogus */
#define OFFSET 5
#define BUTTONON 1
#define BUTTONOFF 3

#define frame_SEPARATION 2
#define frame_TWOSEPARATIONS (2*frame_SEPARATION) /* 4 */
#define frame_HALFPADDING (frame_TWOSEPARATIONS+1) /* 5 */
#define frame_TOTALPADDING ((2*frame_HALFPADDING)+2) /* 10 */
#define frame_DOUBLEPADDING (2*frame_TOTALPADDING) /* 20 */
#define MINSIDEBYSIDEWIDTH 400
static int frame_GlobalStackCount = 0; /* a hack to prevent ugly recursion */
static struct keymap *mykm;
static struct proctable_Entry *returnconsidered, *cancel, *confirmAnswer, *gotkey;
static boolean QuitWindowOnlyDefault = -1; /* uninitialized */
static boolean ResizableMessageLine=TRUE;

static double foreground_color[3],background_color[3];
#define BUTTONDEPTH 3
#define BUTTONPRESSDEPTH 2
#define TEXTPAD 2

static void frame_setShade(self, val)
struct frame *self;
int val;			/* 0 - 200*/
{
    double pct;
    if(val > 100){
/*	frame_SetFGColor(self,.95,.95,.95); */
	if (self->mono) pct = .75;
	else {
	   /* frame_SetFGColor(self,.95,.95,.95); */
 frame_SetForegroundColor(self,NULL,105*256,105*256,105*256);
	    return;
	}
    }
    else if(val == 100){
	frame_SetFGColor(self, 
			    foreground_color[0], 
			    foreground_color[1], 
			    foreground_color[2]);
	return ;
    }
    else if(val == 0){
	frame_SetFGColor(self, 
			    background_color[0], 
			    background_color[1], 
			    background_color[2]);
	return;
    }
			    
    else pct = (double)val/ 100.;
    frame_SetFGColor(self, 
			 foreground_color[0]* pct 
			 + background_color[0]*(1.0-pct), 
			 foreground_color[1]*pct 
			 + background_color[1]*(1.0-pct), 
			 foreground_color[2]*pct 
			 + background_color[2]*(1.0-pct));

} /* frame_setShade */

#define CFGCOLOR "white"
#define CBGCOLOR "blue"

#define BWFGCOLOR "black"
#define BWBGCOLOR "white"

static char *
GetProfileString (pref, defalt)
     char *pref;
     char *defalt;
{
  char *p = environ_GetProfile (pref);
  if (p == NULL)
    return (defalt);
  else
    return (p);
}

frame_CacheSettings(self)
struct frame *self;
{
    char *fgcolor, *bgcolor;
    unsigned char fg_rgb[3], bg_rgb[3];
    self->mono = (frame_DisplayClass(self ) & graphic_Monochrome);
    if ( self->mono || environ_GetProfileSwitch("frame.MonochromeDialogBoxes", FALSE)) {
	fgcolor = GetProfileString("frame.DialogBoxMonochromeForeground", BWFGCOLOR);
	bgcolor = GetProfileString("frame.DialogBoxMonochromeBackground", BWBGCOLOR);
    }
    else {  
	fgcolor = GetProfileString("frame.DialogBoxColorForeground", CFGCOLOR);
	bgcolor = GetProfileString("frame.DialogBoxColorBackground", CBGCOLOR);
    }
	
    if (frame_GetIM(self) != NULL) {
	/* these calls don't make sense if there is no IM,
	    (in fact they seg fault!) */

	frame_SetForegroundColor(self, fgcolor, fg_rgb[0]*256L, fg_rgb[1]*256L, fg_rgb[2]*256L);
	frame_SetBackgroundColor(self, bgcolor, bg_rgb[0]*256L, bg_rgb[1]*256L, bg_rgb[2]*256L);
	frame_GetFGColor(self, 
			 &(foreground_color[0]), 
			 &(foreground_color[1]), 
			 &(foreground_color[2]));
	frame_GetBGColor(self, 
			 &(background_color[0]), 
			 &(background_color[1]), 
			 &(background_color[2]));
    }
}
static void drawButton(self,rect,text,pushed,borderonly,blit)
struct frame * self;
struct rectangle *rect;
char *text;
boolean pushed,borderonly,blit;
{

    struct rectangle Rect2;
    int r2_bot, r_bot;
    int tx = 0, ty = 0;
    short t_op;
    long offset;
    struct rectangle r;
    r = *rect;
    r.left +=frame_SEPARATION; r.top +=frame_SEPARATION;
    r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
    r.height -= frame_TWOSEPARATIONS;
    rect = &r; /* BOGUS FIX THIS */
    offset = 0;
/*    frame_SetFont(self, self->activefont); */

    frame_SetTransferMode(self, graphic_SOURCE);
    t_op = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM;
    Rect2.top = rect->top + BUTTONDEPTH + offset;
    Rect2.left = rect->left + BUTTONDEPTH  + offset;
    Rect2.width = rect->width - 2*BUTTONDEPTH ;
    Rect2.height = rect->height - 2*BUTTONDEPTH ;
    r2_bot = (Rect2.top)+(Rect2.height);
    r_bot = (rect->top)+(rect->height);
    tx = Rect2.left + (Rect2.width/ 2);

    ty = Rect2.top + ( Rect2.height/ 2);

    frame_SetTransferMode(self, graphic_COPY);
    if(!borderonly && (!self->mono || !blit)){
	frame_setShade(self, ((self->mono) ? 0:((pushed)? 0: FORESHADE)));
	frame_FillRect(self, &Rect2, NULL); /* the middle box */
	if(text && *text){
	    long len = strlen(text);
	    frame_setShade(self, 100);
	    frame_SetTransferMode(self, graphic_BLACK); 
	    frame_MoveTo(self, tx, ty);
	    frame_DrawText(self, text, len,t_op);
	    frame_SetTransferMode(self, graphic_COPY);
	}
    }
    if(self->mono && (blit || ((!blit) && pushed))){
	frame_SetTransferMode(self, graphic_INVERT);
	frame_FillRect(self, rect, NULL);
	frame_SetTransferMode(self, graphic_COPY);
    }
    if(self->mono){
	frame_setShade(self, 100);
	frame_DrawRect(self,rect); 
/*	frame_DrawRect(self,&Rect2); */
    }
    else {
	/* Drawing the button is too slow on mono displays */
	frame_SetUpperShade(self,pushed) ;
	frame_FillRectSize(self, rect->left, rect->top, BUTTONDEPTH + offset, rect->height, NULL);	/* left bar */

	frame_SetLowerShade(self,pushed) ;
	frame_FillRectSize(self, rect->left + rect->width - BUTTONDEPTH + offset, rect->top, BUTTONDEPTH - offset, rect->height, NULL); /* right bar */
	frame_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, rect->left, r_bot, rect->width, NULL); /* lower trapz */

	frame_SetUpperShade(self,pushed) ;

	frame_FillTrapezoid(self, rect->left, rect->top, rect->width, Rect2.left, Rect2.top, Rect2.width, NULL); /* upper trapz */
	frame_SetTransferMode(self, graphic_COPY);
    }
}

static int CalculateLineHeight(self)
    struct frame *self;
{

    struct style *defaultStyle;
    struct fontdesc *defaultFont;
    struct FontSummary *fontSummary;
    char fontFamily[256];
    long refBasis, refOperand, fontSize;

    if ((defaultStyle = frameview_GetDefaultStyle(self->messageView)) == NULL)
        return DEFAULTHEIGHT;
    style_GetFontFamily(defaultStyle, fontFamily, sizeof (fontFamily));
    style_GetFontSize(defaultStyle, (enum style_FontSize *) &refBasis, &fontSize);
    refOperand = style_GetAddedFontFaces(defaultStyle);
    defaultFont = fontdesc_Create(fontFamily, refOperand, fontSize);
    if ((fontSummary = fontdesc_FontSummary(defaultFont, frame_GetDrawable(self))) == NULL)
        return DEFAULTHEIGHT;
    return fontSummary->maxHeight + 4; /* Two for top and bottom border. */
}

void frame__SetReturnFocus(self, v)
struct frame *self;
struct view *v;
{
    if(self->returnFocus) view_RemoveObserver(self->returnFocus, self);
    if(v) {
	view_AddObserver(v, self);
    }
    self->returnFocus=v;
}

void frame__ReturnFocus(self)
struct frame *self;
{
    struct view *focus;
    if(self->returnFocus) {
	view_WantInputFocus(self->returnFocus, self->returnFocus);
	view_RemoveObserver(self->returnFocus, self);
	self->returnFocus=NULL;
    } else {
	focus=frame_GetView(self);
	if(focus) view_WantInputFocus(focus, focus);
    }
}

boolean frame__InitializeObject(classID, self)
    struct classheader *classID;
    struct frame *self;
{
    extern struct keymap *frame_InitKeymap();

    self->deleteTarget=NULL;
    self->returnFocus=NULL;
    self->realBuffer=NULL;
    self->realView=NULL;
    self->revertToReal = FALSE;

    self->dv=NULL;
    self->UsingDialog=FALSE;
    self->UseBuiltinDialogs= environ_GetProfileSwitch("UseBuiltinDialogs", FALSE);
    self->buffer = NULL;
    self->targetView = NULL;
    self->childView = NULL;
    self->messageLine = framemessage_Create(self);
    self->messageView = self->messageLine->messageView;
    frame_SetNth(self, 1, self->messageView);
    self->dialogLine = framemessage_Create(self);
    self->dialogView = self->dialogLine->messageView;
    framemessage_SetCompanion(self->messageLine,(struct msghandler *)self->dialogLine);
    self->title = NULL;

    frameKeymap = framecmds_InitKeymap(&frameMenus, &frameDefaultMenus, &frameDefaultKeymap);
    self->keystate = keystate_Create(self, frameDefaultKeymap);
    self->menulist = menulist_DuplicateML(frameMenus, self);
    self->defaultmenulist = menulist_DuplicateML(frameDefaultMenus, self);
    menulist_SetMask(self->defaultmenulist, frame_DefaultMenus);
    menulist_SetMask(self->menulist,frame_DefaultMenus);
    self->helpBuffer[0] = '\0';
    self->commandEnable = FALSE;
    self->lineHeight = 0;
    self->next = allFrames;
    allFrames = self;
    if (QuitWindowOnlyDefault == -1) {
	/* Can't do this in InitializeClass because the app name may not be
	 * known there.  We don't want to do it every time we create a frame
	 * either.  So we get it now...but just once.
	 */
	QuitWindowOnlyDefault = environ_GetProfileSwitch("QuitWindowOnly", FALSE);
	if (QuitWindowOnlyDefault) {
	    if (environ_GetProfileSwitch("QuitBuffer", FALSE))
		QuitWindowOnlyDefault++;
	}
    }
    self->QuitWindowOnly = QuitWindowOnlyDefault ;

    self->AwaitingFocus = 0;
    self->DialogBuffer = NULL;
    self->DialogBufferView = NULL;
    self->DialogTargetView = NULL;
 
    /* Create menulist/keystate */
    self->octcursor = cursor_Create((struct view *) self);
    self->arrowcursor = cursor_Create((struct view *) self);
    cursor_SetStandard(self->octcursor, Cursor_Octagon);
    cursor_SetStandard(self->arrowcursor, Cursor_Arrow);
    self->mymenus = menulist_Create(self);
    self->myfontdesc = fontdesc_Create("andysans", fontdesc_Bold, frame_FONTSIZE);
    self->mykeystate = keystate_Create(self, mykm);
    self->HeightsOfAnswer = NULL;
    self->uplist = NULL;
    self->mesrec.height = 0;
    self->UpdateRequested = FALSE;
    TidyUp(self);
    self->object = NULL;
    self->dataModified = FALSE;
    return TRUE;
}

void frame__FinalizeObject(classID, self)
    struct classheader *classID;
    struct frame *self;
{

    struct frame *traverse, **previous;

    if(frame_GetIM(self)) im_SetDeleteWindowCallback(frame_GetIM(self), NULL, NULL);
    
    if(self->deleteTarget) {
	buffer_RemoveObserver(self->deleteTarget, self);
    }
    
    if(self->returnFocus) {
	view_RemoveObserver(self->returnFocus, self);
    }
    
#if 0
    if (self->buffer != NULL) {
        if (self->childView != NULL) {
	    frame_SetNth(self, 0, NULL); /* Remove the view from the lpair so we can deallocate it. */
            buffer_RemoveView(self->buffer, self->childView);
	}
        buffer_RemoveObserver(self->buffer, self);
    }
#else
    frame_SetBuffer(self, NULL, FALSE);
#endif
    frame_SetNth(self, 1, NULL);    /* Remove the view from the lpair */	
    framemessage_Destroy(self->messageLine);
    framemessage_Destroy(self->dialogLine);
    cursor_Destroy(self->octcursor);	
    cursor_Destroy(self->arrowcursor);	
    if(self->menulist) menulist_Destroy(self->menulist);
    if(self->defaultmenulist) menulist_Destroy(self->defaultmenulist);
    if (self->title != NULL)
        free(self->title);

    previous = &allFrames;
    for (traverse = allFrames; traverse != NULL && traverse != self; traverse = traverse->next)
        previous = &traverse->next;
    if (traverse != NULL) /* Bad error if this is false. */
        *previous = traverse->next;

    if(self->mymenus) {
	menulist_Destroy(self->mymenus);
	self->mymenus=NULL;
    }
}

static void SetTitle();

void frame__FullUpdate(self, type, left, top, width, height)
struct frame *self;
enum view_UpdateType type;
long left, top, width, height;
{
    if (self->lineHeight == 0) {
	long dw, dh;
	self->lineHeight = CalculateLineHeight(self);
	frameview_DesiredSize(self->messageView, frame_GetLogicalWidth(self), self->lineHeight, 0, &dw, &dh);
	frame_VFixed(self, self->childView, self->messageView, dh, ResizableMessageLine);

	((struct lpair *)self)->needsfull=3;
    }

    SetTitle(self);
    self->drawn = FALSE;
    super_FullUpdate(self, type, left, top, width, height);
    if (self->IsAsking && !self->UsingDialog) {
	ComputeSize(self);
	DoUpdate(self);
    }
}
static handleNewData(self)
struct frame *self;
{
    struct view *inputFocus, *targetView,*oldchild;
    oldchild = self->childView;
    view_UnlinkTree(oldchild);
    view_UnlinkTree(self->targetView);
    if (self->object)
        dataobject_RemoveObserver(self->object, self);
    self->object = buffer_GetData(self->buffer);
    dataobject_AddObserver(self->object, self);
    /* self->realView = we don't set realView here since this only gets done for buffers.*/
    self->childView = buffer_GetView(self->buffer, &inputFocus, &targetView, NULL);
    self->targetView = targetView;
    frame_VFixed(self, self->childView, self->messageView, frameview_GetLogicalHeight(self->messageView), ResizableMessageLine);
 /*   buffer_RemoveView(self->buffer,oldchild); */

}

#include <lpair.ih>
void frame__WantNewSize(self, req)
struct frame *self;
struct view *req;
{
    long dw=(-1), dh=(-1);
    
    if(req!=(struct view *)self->messageView) return;

    
    frameview_DesiredSize(self->messageView, frame_GetLogicalWidth(self), self->lineHeight, 0, &dw, &dh);

    frame_VFixed(self, self->childView, self->messageView, dh, ResizableMessageLine);

    ((struct lpair *)self)->needsfull=3;
    
    frame_WantUpdate(self, self);
}

void frame__Update(self)
    struct frame *self;
{
    struct pendingupdates *pu;

    self->UpdateRequested = FALSE;
    if(self->buffer != NULL && self->object != buffer_GetData(self->buffer)){
	handleNewData(self);
    }
    SetTitle(self);
    if (self->IsAsking && self->uplist) {
	RestoreBits(self);
/* 	CannotRestoreBits(self);    Will work but is too slow to be worth it */ 
    }
    while (self->uplist) {
	view_Update(self->uplist->v);
	pu = self->uplist->next;
	free(self->uplist);
	self->uplist = pu;
    }
    if (self->IsAsking && !self->UsingDialog) {
	DoUpdate(self);
    } 
    else super_Update(self);
}


void frame__ObservedChanged(self, changed, value)
struct frame *self;
struct observable *changed;
long value;
{    
    if((struct observable *)self->returnFocus==changed && value==observable_OBJECTDESTROYED) {
	self->returnFocus=NULL;
	return;
    }
    if(value==observable_OBJECTDESTROYED && changed==(struct observable *)self->deleteTarget) {
	self->deleteTarget=NULL;
	return;
    }
    
    if (value!=observable_OBJECTDESTROYED && self->buffer != NULL && changed == (struct observable *) self->buffer && self->object != buffer_GetData(self->buffer)) {
	if (self->object)
	    dataobject_RemoveObserver(self->object, self);
	self->object = buffer_GetData(self->buffer);
	dataobject_AddObserver(self->object, self);
    }
    else if (changed == (struct observable *) self->object) {
	if (value == observable_OBJECTDESTROYED) {
	    self->object = NULL;
	}
	else if (self->dataModified || buffer_GetScratch(self->buffer)) {
	    return;
	}
    }

    if (!self->UpdateRequested)
	frame_WantUpdate(self, self);
}

void frame__SetBuffer(self, buffer, setInputFocus)
    struct frame *self;
    struct buffer *buffer;
    boolean setInputFocus;
{

    struct view *inputFocus, *targetView;

    self->realView = NULL;
    
    if(self->childView != NULL  && ((buffer != self->buffer) || buffer==NULL)) {
	/* moved from inside the two ifs below... */
	frame_SetNth(self, 0, NULL); /* Remove the view from the lpair so we can deallocate it. */
    }
    if (buffer != self->buffer) {
        if (self->buffer != NULL) {
            if (self->childView != NULL) {
                buffer_RemoveView(self->buffer, self->childView); /* Deallocate the view... */
            }
            if (self->object)
                dataobject_RemoveObserver(self->object, self);
            if(self->buffer!=self->deleteTarget) buffer_RemoveObserver(self->buffer, self);
        }
        self->realBuffer = self->buffer = buffer;
#if 1
        if (buffer != NULL) {
#endif
            buffer_AddObserver(buffer, self);
            self->object = buffer_GetData(self->buffer);
            dataobject_AddObserver(self->object, self);
	    self->childView = buffer_GetView(buffer, &inputFocus, &targetView, NULL);
            self->targetView = targetView;
	    frame_VFixed(self, self->childView, self->messageView, frameview_GetLogicalHeight(self->messageView), ResizableMessageLine);
	    frame_SetReturnFocus(self, inputFocus==NULL ? targetView : inputFocus);
            if (setInputFocus)
		if (inputFocus == NULL) {
		    view_WantInputFocus(targetView, targetView);
		} else {
		    view_WantInputFocus(inputFocus, inputFocus);
		}
#if 1
        }
#endif
    }
}

void frame__SetView(self, view)
struct frame *self;
struct view *view;
{
    self->realBuffer=NULL;
    
    if(self->childView != NULL) {
	frame_SetNth(self, 0, NULL); /* Remove the view from the lpair so we can deallocate it. */;
    }
    
    if (self->buffer != NULL) {
	if (self->childView != NULL) {
	    buffer_RemoveView(self->buffer, self->childView); /* Deallocate the view... */
	}
	if(self->deleteTarget==self->buffer) buffer_RemoveObserver(self->buffer, self);
	if (self->object)
	    dataobject_RemoveObserver(self->object, self);
	self->object = NULL;
	self->buffer = NULL;
    }
    else if(view == NULL && self->childView != NULL){
	frame_SetNth(self, 0, NULL); /* Remove the view from the lpair so it can be
				      destroyed by whoever created it */
    }
    frame_SetReturnFocus(self, view);
    self->realView = self->childView = self->targetView = view;
    if (view != NULL) {
	frame_VFixed(self, self->childView, self->messageView, frameview_GetLogicalHeight(self->messageView), ResizableMessageLine);
	view_WantInputFocus(view, view);
    }
}

/* Create a frame suitable for use as a buffer window. */
struct frame *frame__Create(classID, buffer)
    struct classheader *classID;
    register struct buffer *buffer;
{

    register struct frame *tempFrame = frame_New();

    frame_SetBuffer(tempFrame, buffer, TRUE);
    frame_SetCommandEnable(tempFrame, TRUE);
    return tempFrame;
}

/* Iterates over all frames.
 */
struct frame *frame__Enumerate(classID, mapFunction, functionData)
    struct classheader *classID;
    boolean (*mapFunction)();
    long functionData;
{

    struct frame *traverse, *next;

    for (traverse = allFrames; traverse != NULL; traverse = next) {
        next = traverse->next; /* So mapFunction is allowed to delete the frame. */
        if ((*mapFunction)(traverse, functionData))
            return traverse;
    }
    return NULL;
}

struct basicobject *frame__WantHandler(self, handlerName)
struct frame *self;
char *handlerName;
{
    if (strcmp(handlerName, "message") == 0)
	return (struct basicobject *) self->messageLine;
    else if (self->header.view.parent == NULL) return NULL;
    else
	return view_WantHandler(self->header.view.parent, handlerName);
}

void frame__PostKeyState(self, keystate)
struct frame *self;
struct keystate *keystate;
{
    if(self->IsAsking){
	if (keystate == NULL || keystate->object != (struct basicobject *) self->dialogView) {
	    self->mykeystate->next = NULL;
	    keystate_Reset(self->mykeystate);
	    keystate_AddAfter(self->mykeystate, keystate);
	}
	if (keystate == NULL) {
	    view_PostKeyState(self->header.view.parent, self->mykeystate);
	}
	else{
	    if (keystate == self->mykeystate->next) {
		self->mykeystate->next = NULL;
	    }
	    view_PostKeyState(self->header.view.parent, keystate);
	}
    }
    else if (keystate == NULL || keystate->object != (struct basicobject *) self->messageView) {
            self->keystate->next = NULL;
	    keystate_Reset(self->keystate);
	    keystate_AddAfter(self->keystate, keystate);
	    if (keystate == NULL) {
		view_PostKeyState(self->header.view.parent, self->keystate);
	    } else {
		view_PostKeyState(self->header.view.parent, keystate);
	    }
    } else view_PostKeyState(self->header.view.parent, keystate);
}


void frame__PostMenus(self, menulist)
struct frame *self;
struct menulist *menulist;
{

    if(self->IsAsking ){
	if((menulist != NULL) && self->hasDialogMessage && menulist->object == (struct basicobject *) self->dialogView){
	    menulist_ClearChain(self->mymenus);
	    menulist_ChainBeforeML(self->mymenus, menulist, (long) menulist);
	    view_PostMenus(self->header.view.parent,self->mymenus);
	}
	else {
	    view_PostMenus(self->header.view.parent, self->mymenus);
	}
    }
    else if ((menulist == NULL) || (menulist->object != (struct basicobject *) self->messageView)) {
	menulist_ClearChain(self->defaultmenulist);
	menulist_ClearChain(self->menulist);
	menulist_ChainBeforeML(self->defaultmenulist, menulist, (long) menulist);
	if(self->commandEnable) menulist_ChainBeforeML(self->defaultmenulist, self->menulist, (long)self->menulist);
	view_PostMenus(self->header.view.parent, self->defaultmenulist);
    }
    else
	view_PostMenus(self->header.view.parent, menulist);
}

void frame__SetCommandEnable(self, enable)
    struct frame *self;
    boolean enable;
{

    if ((self->commandEnable = enable) == TRUE) {
	if(self->keystate) keystate_Destroy(self->keystate);
	self->keystate=keystate_Create(self, frameKeymap);
	menulist_SetMask(self->menulist,menulist_GetMask(self->menulist) |
			 frame_BufferMenus);
	menulist_SetMask(self->defaultmenulist,menulist_GetMask(self->defaultmenulist) |
			 frame_BufferMenus);
    } else {
	if(self->keystate) keystate_Destroy(self->keystate);
	self->keystate=keystate_Create(self, frameDefaultKeymap);
	menulist_SetMask(self->menulist,menulist_GetMask(self->menulist) &
			 ~frame_BufferMenus);
	menulist_SetMask(self->defaultmenulist,menulist_GetMask(self->defaultmenulist) &
			 ~frame_BufferMenus);
    }
}

struct buffer *frame__GetHelpBuffer(self)
    struct frame *self;
{

    int i;
    struct buffer *buffer;

    for (i = 1; i < 100 && ((buffer = buffer_FindBufferByName(self->helpBuffer)) == NULL); i++) {

        char bufferName[30];

        sprintf(bufferName, "Help-Buffer-%d", i);
        if (((buffer = buffer_FindBufferByName(bufferName)) == NULL) || !buffer_Visible(buffer)) {
            if (buffer == NULL)
                buffer = buffer_Create(bufferName, NULL, "text", NULL);
            strncpy(self->helpBuffer, bufferName, sizeof(self->helpBuffer));
        }
    }
    buffer_SetScratch(buffer, TRUE);
    return buffer;
}
struct buffer *frame__SetHelpBuffer(self, buf, setInputFocus)
struct frame *self;
struct buffer *buf;
boolean setInputFocus;
{   /* Returns the last buffer IF it will need to be restored */
    struct view *inputFocus, *targetView;
    int NeedUpdate = 0;
    if(!self->IsAsking) {
	struct buffer *LastBuffer = frame_GetBuffer(self);
	struct buffer *realBuffer = self->realBuffer;
	struct view *realView = self->realView;
	frame_SetBuffer(self, buf, setInputFocus);
	self->realBuffer = realBuffer;
	self->realView = realView;
	self->revertToReal=TRUE;
	return LastBuffer;
    }
    /* Put the buffer in the dialog box */
    if(self->DialogBuffer == NULL){
	self->drawn = FALSE;
	NeedUpdate++;
    }
    if (buf != self->DialogBuffer) {
	if (self->DialogBuffer != NULL) {
	    if (self->DialogBufferView != NULL)
		buffer_RemoveView(self->DialogBuffer, self->DialogBufferView);
	    buffer_RemoveObserver(self->DialogBuffer, self);
	}
	buffer_AddObserver(buf, self);
	self->DialogBuffer = buf;
	self->DialogBufferView = buffer_GetView(buf, &inputFocus, &targetView,"helptextview");
	if(self->dv) {
	    dialogv_InstallSidekick(self->dv, self->DialogBufferView);
	    im_RedrawWindow(dialogv_GetIM(self->dv));
	}
	self->DialogTargetView = targetView;
    }
    if(NeedUpdate){
	ComputeSize(self);
	frame_WantUpdate(self,self);
    }
    return NULL;
}

void frame__RemoveHelp(self)
struct frame *self;
{
    if(self->revertToReal) {
	self->revertToReal=FALSE;
	if(self->realView) frame_SetView(self, self->realView);
	if(self->realBuffer) frame_SetBuffer(self, self->realBuffer, TRUE);
    }
}

void frame__SetTitle(self, title)
    struct frame *self;
    char *title;
{
    if (self->title != NULL)
        free(self->title);

    if (title != NULL) {
        self->title = (char *) malloc(strlen(title) + 1);
        strcpy(self->title, title);
    }
    else
        self->title = NULL;

    if (! self->UpdateRequested)
        frame_WantUpdate(self, self);
}

/*
 * This code sets the title bar such that the end of a filename is visible.
 * Seldom is there so much code to accomplish so little.
 */
static void SetTitle(self)
    struct frame *self;
{
    static char *readonly_flag = NULL;

    if (!readonly_flag) {
	/* first time this function has been called; we must read in the readonly_flag, or set it to the default. */
	readonly_flag = GetProfileString("ReadOnlyTitle", "(readonly)");
    }

#define WMTITLELEN 70 /* Can you say "Magic hack?" */

    if (self->title == NULL) {
        if (frame_GetBuffer(self) == NULL)
            return;
        else {

            char titleBuffer[WMTITLELEN], *titleLine;
            int maxLen = sizeof(titleBuffer) - 2;

	    *titleBuffer = '\0';

	    if (self->buffer != NULL){
		if(buffer_GetScratch(self->buffer))
		    self->dataModified = FALSE;
		else if (self->dataModified = (buffer_GetWriteVersion(self->buffer) < dataobject_GetModified(buffer_GetData(self->buffer)))) {
		    --maxLen; /* Make room for '*' */
		}
		if (buffer_GetReadOnly(self->buffer)) {
		    maxLen -= (strlen(readonly_flag)+1);	/* Make room for readonly flag and the space before it */
		}
	    }
	    titleLine=buffer_GetFilename(frame_GetBuffer(self));
	    if (titleLine!= NULL && *titleLine != '\0'){
		path_TruncatePath(titleLine, titleBuffer, maxLen, TRUE);
	    }
	    else {
		titleLine=buffer_GetName(frame_GetBuffer(self));
		if (titleLine==NULL) {
		    im_SetTitle(frame_GetIM(self), "");
		    return;
		}
		else{
		    strcpy(titleBuffer, "Buffer: ");
		    maxLen-=sizeof("Buffer: ")-1;
		    strncat(titleBuffer,titleLine,maxLen);
		}
	    }

	    if (self->dataModified)
		strcat(titleBuffer, "*");
	    if (buffer_GetReadOnly(self->buffer)) {
		strcat(titleBuffer, " ");
		strcat(titleBuffer, readonly_flag);
	    }

	    im_SetTitle(frame_GetIM(self), titleBuffer);
	}
    }
    else
	im_SetTitle(frame_GetIM(self), self->title);
}

/* The following is code to support the dialog box */

static void ConsiderReturning(self, Choice)
struct frame *self;
int Choice;
{
    if (self->StackPos != frame_GlobalStackCount) {
	framemessage_DisplayString(self->messageLine, 0, "Please answer the other dialog box first.");
	return;
    }
    if (Choice <= 0 || Choice > self->NumAnswerChoices) {
	framemessage_DisplayString(self->messageLine, 0, "That is not an answer; please try again.");
	return;
    }
    if(self->hasDialogMessage){
	switch(Choice){
	    case ACCEPT:
		frameview_Return(self->dialogView,(long)'\n');
		break;
	    case CANCEL:
		framemessage_CancelQuestion(self->dialogLine);
		break;
	    case COMPLETE:
		frameview_Complete(self->dialogView,(long)' ');
		if(!self->UsingDialog) drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],FALSE,FALSE,self->drawn);
		self->DefaultWildestAnswer = 0;
		return;
	    case HELP:
		frameview_Help(self->dialogView,(long)'?');
		if(!self->UsingDialog) drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],FALSE,FALSE,self->drawn);
		self->DefaultWildestAnswer = 0;
		return;
	}
    }
    self->IsAsking = 0;
    self->WildestAnswer = Choice;
}

struct view *
frame__Hit(self, action, x, y, nclicks)
struct frame *self;
enum view_MouseAction action;
long x, y, nclicks;
{
    int i;
    struct rectangle r;
    struct view *v;
    if(self->hasDialogMessage){
	if( self->dialogView && InRectangle(&self->mesrec, x, y))
	    return frameview_Hit(self->dialogView, action, view_EnclosedXToLocalX((struct view *)(self->dialogView), x), view_EnclosedYToLocalY((struct view *)(self->dialogView), y), nclicks);
	else if (self->DialogBufferView && InRectangle(&self->bufferrec, x, y))
	    return view_Hit(self->DialogBufferView, action, view_EnclosedXToLocalX((struct view *)(self->DialogBufferView), x), view_EnclosedYToLocalY((struct view *)(self->DialogBufferView), y), nclicks);
    }
    if (self->UsingDialog || !self->IsAsking
	 || (!self->IsBlocking && !InRectangle(&self->AnswerBox, x, y))) {
	/* Normal, non-dialog hit. */
	v = super_Hit(self, action, x, y, nclicks);
	/* Support for DRAG/DROP protocol.
	 * ==============================
	 * If frame commands are enabled, and the action was a file drop do:
	 *    1) Visit file for LeftFileDrop, and
         *    2) Visit file in new window for RightFileDrop.
	 * Note that many files can be dropped simultaneously.  In this case
	 * the files are loaded into buffers and the last is displayed on
	 * a visit-file, and all are displayed on a visit-file-new-window.
	 * The host is ignored for now.
	 * An improvement would have a wait cursor appear while files are fetched.
	 */
	if (self->commandEnable && (action == view_LeftFileDrop || action == view_RightFileDrop)) {
	    char **files;
	    int i;
	    struct buffer *b;
	    struct frame *f;
	    struct view *v;
	    struct im *im = frame_GetIM(self);

	    if (im == NULL) return v;
	    files = im_GetDroppedFiles(im);
	    if (files) {
		if (files[0] != NULL)
		    free(files[0]); /* ignore host for now */
		for (i = 1; files[i] != NULL; i++) {
		    b = buffer_GetBufferOnFile(files[i], buffer_MustExist);
		    if (b != NULL && action == view_RightFileDrop) {
			f = frame_GetFrameInWindowForBuffer(b);
			im = frame_GetIM(f);
			if (im)
			    im_ExposeWindow(im);
			v = frame_GetView(f);
			if (v)
			    view_WantInputFocus(v, v);
		    }
		    free(files[i]);
		}
		if (b != NULL && action == view_LeftFileDrop)
		    (void)frame_SetBuffer(self, b, TRUE); /* Show last file. */
		free(files);
	    }
	}
	return v;
    }
    if (action == view_LeftDown || action == view_RightDown) {
	r = self->HeightsOfAnswer[self->DefaultWildestAnswer];
	r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
	if(!(InRectangle(&r, x, y))){
	    for (i=1; i<=self->NumAnswerChoices; ++i) {
		r = self->HeightsOfAnswer[i];
		r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
		if (InRectangle(&r, x, y)) {
		    if(self->DefaultWildestAnswer != 0)
			drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],FALSE,FALSE,self->drawn);
		    self->DefaultWildestAnswer = i;
		    drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],TRUE,FALSE,self->drawn);
		}
	    }
	}
	return((struct view *) self);
    }
    else if (action == view_LeftMovement || action == view_RightMovement) {
	if(self->DefaultWildestAnswer != 0){
	    r = self->HeightsOfAnswer[self->DefaultWildestAnswer];
	    r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
	    if(!(InRectangle(&r, x, y))){
		drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],FALSE,FALSE,self->drawn);
		self->PotentialChoice = self->DefaultWildestAnswer;
		self->DefaultWildestAnswer = 0;
	    }
	}
	else 	if(self->PotentialChoice != 0){
	    r = self->HeightsOfAnswer[self->PotentialChoice];
	    r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
	    if((InRectangle(&r, x, y))){
		drawButton(self,&self->HeightsOfAnswer[self->PotentialChoice], self->MultipleAnswers[self->PotentialChoice],TRUE,FALSE,self->drawn);
		self->DefaultWildestAnswer = self->PotentialChoice ;
		self->PotentialChoice = 0;
	    }
	}

	return((struct view *) self);
    }
    /* Choose the answer here */
    i = self->DefaultWildestAnswer ;
    if(i > 0){
	r = self->HeightsOfAnswer[i];
	r.width = self->buttonmaxwid + frame_TOTALPADDING - frame_TWOSEPARATIONS;
	if (InRectangle(&r, x, y)) {
	    /* found right answer */
	    ConsiderReturning(self, i);
	    return((struct view *) self);
	}
    }
    if(!self->hasDialogMessage && InRectangle(&self->AnswerBox, x, y)) 
	ConsiderReturning(self, self->DefaultWildestAnswer);
    return((struct view *) self);
}
static boolean
InRectangle(r, x, y)
struct rectangle *r;
long x, y;
{
    if ((x < r->left) || (x > (r->left + r->width)) || (y < r->top) || (y > (r->top + r->height))) {
	return(0);
    }
    return(1);
}
static drawshadow(self,r)
struct frame *self;
struct rectangle *r;
{
    frame_FillRectSize(self,r->left + OFFSET,r->top + r->height,r->width,OFFSET,frame_GrayPattern(self,8,16));
    frame_FillRectSize(self,r->left + r->width,r->top + OFFSET,OFFSET,r->height - OFFSET,frame_GrayPattern(self,8,16));
}
static void
DoUpdate(self)
struct frame *self;
{
    struct rectangle *r;
    int i;
    if(!self->drawn){
	struct graphic *pattern;
	frame_CacheSettings(self);
	pattern = ((self->mono) ? frame_WhitePattern(self):frame_BlackPattern(self));
	self->drawn = TRUE;
	SaveBits(self);
	if(!self->mono)frame_setShade(self,FORESHADE);
	frame_SetFont(self, self->myfontdesc);
	frame_SetTransferMode(self, graphic_COPY);
	if(self->DialogBuffer){
	    frame_FillRect(self, &self->bufferrec, pattern);
	    frame_FillRect(self, &self->AnswerBox, pattern);
	    /* Also clear area between boxes */
	    if(!self->mono)frame_SetFGColor(self,  1.,1.,1.);
	    if(self->AnswerBox.top == self->bufferrec.top) 	
		frame_FillRectSize(self,self->AnswerBox.left + self->AnswerBox.width ,  self->AnswerBox.top,10, self->AnswerBox.height + OFFSET , pattern );
	    else {
		frame_FillRectSize(self,self->AnswerBox.left,  self->AnswerBox.height +self->AnswerBox.top, self->AnswerBox.width  + OFFSET ,10, pattern);
	    }

	}
	else 	
	    frame_FillRect(self,&self->AnswerBox, pattern);
	frame_setShade(self,100);
	if(self->DialogBuffer)   { 
	    drawshadow(self,&self->bufferrec);
	    frame_DrawRect(self,&self->bufferrec);
	}
	drawshadow(self,&self->AnswerBox);
	frame_DrawRect(self,&self->AnswerBox);
	for (i=1; i<= self->NumAnswerChoices; ++i) {
	   drawButton(self,&self->HeightsOfAnswer[i],self->MultipleAnswers[i],(i == self->DefaultWildestAnswer),FALSE,FALSE);
	}
	frame_setShade(self,100);
	if(self->hasDialogMessage){
	    r = &self->mesrec;
	    frame_DrawRectSize(self, r->left - 1, r->top -1, r->width + 2, r->height + 2);
	}
	r = &self->HeightsOfAnswer[0];
	frame_MoveTo(self, r->left + frame_SEPARATION + 3, r->top + frame_SEPARATION + 3);
	frame_DrawString(self, self->MultipleAnswers[0], graphic_ATLEFT | graphic_ATTOP);
	frame_PostCursor(self, &self->AnswerBox, self->octcursor);
	if(self->hasDialogMessage){
	    frameview_InsertView(self->dialogView, self, &self->mesrec);
	    frameview_FullUpdate(self->dialogView, view_FullRedraw , 0, 0, 0, 0);
	    frame_PostCursor(self, &self->mesrec, self->arrowcursor);
	    if(self->DialogBuffer){
		struct rectangle nr;
		nr = self->bufferrec;
		nr.top++;nr.left++; nr.width += -2; nr.height += -2;
		view_InsertView(self->DialogBufferView, self, &nr);
		view_FullUpdate(self->DialogBufferView, view_FullRedraw , 0, 0, 0, 0);
	    }
	}
	if (frame_GetIM(self) != NULL) {
	    frame_SetBGColor(self,  1.,1.,1.);
	    frame_SetFGColor(self,0.,0.,0.);
	    frame_SetTransferMode(self, graphic_INVERT);
	}
    }
}

static void
SaveBits(self)
struct frame *self;
{
    /* A no-op for now */
}

static int ButtonInteract(self, AnswerList, DefaultWildestAnswer, WildestAnswer, flags)
struct frame *self;
char **AnswerList;
long DefaultWildestAnswer, *WildestAnswer;
int flags;
{
    int i;
    int answer;
    struct view *focus;
    struct im *im=frame_GetIM(self);
    if (!im || self->IsAsking || self->AwaitingFocus) return(-1);
    while(im_Interact(0)); /*Clear out any pending updates */
    for (i = 0; AnswerList[i] && *AnswerList[i]; ++i) {
 	;
    }
    self->NumAnswerChoices = --i;
    if (i <= 0) {
	return(-1);
    }
    self->MultipleAnswers = AnswerList;
    self->DefaultWildestAnswer = DefaultWildestAnswer;
    self->StackPos = ++frame_GlobalStackCount;
    ComputeSize(self);
    self->IsAsking = 1;
    focus = im_GetInputFocus(im);
    if (focus && focus != (struct view *)self) view_LoseInputFocus(focus);
    frame_SetReturnFocus(self, focus);
    self->dv=NULL;
    if(!self->UseBuiltinDialogs && ((im_SupportsOverride(im) && self->IsBlocking) || (im_SupportsTransient(im) && !self->IsBlocking))) {
	self->dv=dialogv_Create(AnswerList+1, NULL, environ_GetProfileInt("dialogstyle", 4 /* sbutton_MOTIF */));
	if(self->dv) {
	    self->UsingDialog=TRUE;
	    text_ReadTemplate(dialogv_GetTextData(self->dv), "dialog", FALSE);
	    text_InsertCharacters( dialogv_GetTextData(self->dv), 0, *AnswerList, strlen(*AnswerList));
	}
    }
    PrepareMenus(self);
    if(! self->UpdateRequested) frame_WantUpdate(self, self);
    
    if(self->dv && self->UsingDialog) {
	while(self->IsAsking) {
	    long pos=im_Centered;
	    switch(self->PositionalPreference) {
		case message_OnTop:
		    pos|=im_AtTop;
		    break;
		case message_OnBottom:
		    pos|=im_AtBottom;
		    break;
		case message_Center:
		    pos|=im_InMiddle;
		default:
		    break;
	    }
	    answer=dialogv_PostChoice(self->dv, frame_GetIM(self), frame_GetIM(self), &self->IsAsking, (self->DefaultWildestAnswer<1)?-1:self->DefaultWildestAnswer-1, self->IsBlocking, pos)+1;
	    if(answer>0) ConsiderReturning(self, answer);
	}
    } else {
	self->UsingDialog=FALSE;
	while (self->IsAsking) {
	    im_Interact(1);
	}
    }
    --frame_GlobalStackCount;
    retractCursors(self);
    if(!self->UsingDialog && !RestoreBits(self))
	CannotRestoreBits(self);
    if ((self->WildestAnswer <=0) || (self->WildestAnswer > self->NumAnswerChoices)) {
	*WildestAnswer = -1;
    } else {
	*WildestAnswer = self->WildestAnswer;
    }
    TidyUp(self);
    if (focus != (struct view *)self ) frame_ReturnFocus(self);
    return((*WildestAnswer == -1) ? -1 : 0);
}
static void
PrepareMenus(self)
struct frame *self;
{
    int i, plen;
    char Priority[10], QBuf[600], BigBuf[1200]; /* No one in his right mind will ever exceed these */

    /* if usemine, force out my keystate & menulist.  Otherwise restore the
    last non-mine version. */
    if (self->IsAsking) {
	menulist_ClearML(self->mymenus);
#ifdef PROBABLYHOPLESS /* wm bug on long menu items */
	strcpy(QBuf, self->MultipleAnswers[0]);
	PurifyString(QBuf);
	strcat(QBuf, ",");
#else /* PROBABLYHOPLESS  */
	strcpy(QBuf, "Dialog Box~1,");
#endif /* PROBABLYHOPLESS  */
	plen = strlen(QBuf);
	for (i=1; i<= self->NumAnswerChoices; ++i) {
	    strcpy(BigBuf, QBuf);
	    strcat(BigBuf, self->MultipleAnswers[i]);
	    PurifyString(&BigBuf[plen]);
	    if (strlen(BigBuf) > 80) { /* wm bug on long menu items */
		BigBuf[78] = '\0';
		strcat(BigBuf, "...");
	    }
	    sprintf(Priority, "~%d", i);
	    strcat(BigBuf, Priority);
	    menulist_AddToML(self->mymenus, BigBuf, returnconsidered, i,0);
	}
	menulist_AddToML(self->mymenus, "Quit", 0, 0,0);
	if(self->dv) {
	    struct menulist *dup=menulist_DuplicateML(self->mymenus, self);
	    struct keystate *dupk = keystate_Create(self, mykm);
	    dialogv_SetExtraMenus(self->dv, dup);
	    dialogv_SetExtraKeyState(self->dv, dupk);
	}
	frame_PostMenus(self, self->mymenus); 
	frame_PostKeyState(self, self->mykeystate);
    }
}

/* This next routine tries to make sure a string won't blow up in a menu */
static void
PurifyString(s) 
char *s;
{
    if (!s) return;
    while (*s) {
	switch(*s) {
	    case ',':
		*s++ = '.';
		break;
	    case ':':
	    case '~':
		*s++ = '-';
		break;
	    default:
		++s;
	}
    }
}
static boolean
RestoreBits(self)
struct frame *self;
{
    /* for now, a no-op */
    return FALSE;
}

/* this next routine should go away once SaveBits and RestoreBits work. */
static void
CannotRestoreBits(self)
struct frame *self;
{
    struct rectangle r;

    frame_GetLogicalBounds(self, &r);
    super_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
}

void frame__Advice(self, pp)
struct frame *self;
enum message_Preference pp;
{
    if(pp == message_NoBlock) self->IsBlocking = 0;
    else self->PositionalPreference = pp;
}

static void
ComputeSize(self)
struct frame *self;
{
    long i, xw, yw, curht, totht, totwid,realwid, curleft, maxheight, boxheight,maxwid = 0;

    maxheight = 0;
    if (!self->HeightsOfAnswer) {
	self->HeightsOfAnswer = (struct rectangle *) malloc((2+self->NumAnswerChoices) * sizeof(struct rectangle));
    }
    self->buttonmaxwid = 0;
    realwid = frame_GetLogicalWidth(self);
    curht = frame_HALFPADDING;
    if(self->DialogBuffer && realwid > MINSIDEBYSIDEWIDTH){
	totwid = realwid / 2;
    }
    else totwid = realwid;
    /* get max button sizes */
    for(i = 1; i <= self->NumAnswerChoices; i++) {

        struct FontSummary *fontSummary;

	fontdesc_StringSize(self->myfontdesc, frame_GetDrawable(self), self->MultipleAnswers[i], &xw, &yw);
	if ((i > 0) && (xw > self->buttonmaxwid))
            self->buttonmaxwid = xw;

/* This code should really be factored out of this loop, but I don't have
 * time to test that now.
 */
        if ((fontSummary = fontdesc_FontSummary(self->myfontdesc,
                                                frame_GetDrawable(self))) == NULL) {
            if (yw < (frame_FONTSIZE+frame_FONTPAD))
                yw = frame_FONTSIZE+frame_FONTPAD;
        }
        else
            yw = fontSummary->maxHeight + frame_FONTPAD;

	if(maxheight < yw) maxheight = yw;
	self->HeightsOfAnswer[i].height = yw;
	self->HeightsOfAnswer[i].width = xw;
    }
    curleft = (totwid - self->buttonmaxwid - frame_TOTALPADDING) / 2;

    /* calculate the header placement */
    fontdesc_StringSize(self->myfontdesc, frame_GetDrawable(self), self->MultipleAnswers[0], &xw, &yw);
    if (yw < (frame_FONTSIZE+frame_FONTPAD)) yw = frame_FONTSIZE+frame_FONTPAD; /* bogus fontdesc bug */
    self->HeightsOfAnswer[0].left = (totwid - xw - frame_TOTALPADDING) / 2;
    self->HeightsOfAnswer[0].width = xw+frame_TOTALPADDING;
    if (xw > maxwid) maxwid = xw;
    self->HeightsOfAnswer[0].top = curht;
    self->HeightsOfAnswer[0].height = yw;
    curht += frame_TWOSEPARATIONS + yw;

    if(self->hasDialogMessage){
	/* calculate the dialog box placement */
	if(self->mesrec.height == 0)
	    self->mesrec.height = self->lineHeight;
	xw = totwid - frame_TOTALPADDING - frame_TOTALPADDING - frame_TOTALPADDING - frame_TOTALPADDING;
	self->mesrec.left = (totwid - xw - frame_TOTALPADDING) / 2;
	self->mesrec.width = xw+frame_TOTALPADDING;
	self->mesrec.top = curht;
	curht += frame_TWOSEPARATIONS + self->mesrec.height;
    }

    /* calculate button placement */

    for (i=1; i<= self->NumAnswerChoices; ++i) {
	self->HeightsOfAnswer[i].left = curleft;
	self->HeightsOfAnswer[i].top = curht;
	curht += frame_TWOSEPARATIONS + maxheight;
    }

    if(self->hasDialogMessage && (self->buttonmaxwid + frame_TOTALPADDING) * 2 < totwid){
	/* double up buttons */
	curht = self->HeightsOfAnswer[1].top;
	for(i = 1; i< self->NumAnswerChoices; i += 2) {
	    self->HeightsOfAnswer[i].left += -10 -(self->buttonmaxwid / 2);
	    self->HeightsOfAnswer[i+1].left += 10 + (self->buttonmaxwid / 2);
	    self->HeightsOfAnswer[i + 1].top = curht;
	    self->HeightsOfAnswer[i].top = curht;
	    self->HeightsOfAnswer[i].height = maxheight;
	    self->HeightsOfAnswer[i + 1].height = maxheight;
	    curht += frame_TWOSEPARATIONS + maxheight;
	}
	if(i == self->NumAnswerChoices){
	    self->HeightsOfAnswer[i].top = curht;
	    curht += frame_TWOSEPARATIONS + self->HeightsOfAnswer[i].height;
	}
	maxwid = MAX((self->buttonmaxwid + frame_TOTALPADDING)* 2 ,maxwid);
    }
    else maxwid = MAX((self->buttonmaxwid + frame_TOTALPADDING) ,maxwid);
    if(self->hasDialogMessage)
	maxwid =  self->mesrec.width - frame_TOTALPADDING;

    /* Decide where in the view rect to put the box rect */
    self->AnswerBox.height = curht + frame_TOTALPADDING;
    totht = frame_GetLogicalHeight(self);
    boxheight = (self->DialogBuffer && realwid <= MINSIDEBYSIDEWIDTH)?   (self->AnswerBox.height + OFFSET)* 2 : self->AnswerBox.height ;
    if (boxheight > (totht + frame_TOTALPADDING)) {
	self->AnswerBox.top = 0;
    } else if (self->PositionalPreference == message_OnTop) {
	self->AnswerBox.top = frame_HALFPADDING;
    } else if (self->PositionalPreference  == message_OnBottom) {
	self->AnswerBox.top = totht - boxheight - frame_HALFPADDING;
    } else {
	self->AnswerBox.top = (totht - boxheight) / 2;
    }
    self->AnswerBox.width = maxwid + frame_DOUBLEPADDING + frame_HALFPADDING;
    self->AnswerBox.left = (totwid - maxwid - frame_DOUBLEPADDING - frame_HALFPADDING) / 2;
    /* Now go back and fix up all the offsets */
    curht = self->AnswerBox.top;
    if (curht) {
	for (i=0; i<=self->NumAnswerChoices; ++i) {
	    self->HeightsOfAnswer[i].top += curht;
	}
	self->mesrec.top += curht;
    }

    if(self->DialogBuffer){
	if(totwid < realwid){
	    self->bufferrec.top = self->AnswerBox.top ;	
	    self->bufferrec.left = self->AnswerBox.left + self->AnswerBox.width  + OFFSET + OFFSET;
	    self->bufferrec.width = realwid - self->AnswerBox.width - (OFFSET * 6);
	    self->bufferrec.height = self->AnswerBox.height ;
	}
	else {
	    self->bufferrec.top = self->AnswerBox.top + self->AnswerBox.height + OFFSET + OFFSET;	
	    self->bufferrec.left = self->AnswerBox.left;
	    self->bufferrec.width = self->AnswerBox.width ;
	    self->bufferrec.height = self->AnswerBox.height ;

	}
    }
}


static void GotKey(self, c)
struct frame *self;
char c;
{
    int curpt, startpt=self->DefaultWildestAnswer;
    char c1;

    if (startpt <= 0 || startpt > self->NumAnswerChoices) {
	startpt = 1;
    }
    curpt = startpt;
    c1 = isupper(c) ? tolower(c) : toupper(c);
    while (1) {
	if (++curpt > self->NumAnswerChoices) curpt = 1;
	if ((*self->MultipleAnswers[curpt] == c)
	|| (*self->MultipleAnswers[curpt] == c1)) {
	     if(self->DefaultWildestAnswer != 0 && self->UseBuiltinDialogs)    drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer],	    self->MultipleAnswers[self->DefaultWildestAnswer],FALSE,FALSE,self->drawn);
	    self->DefaultWildestAnswer = curpt;
	    if(self->UseBuiltinDialogs) drawButton(self,&self->HeightsOfAnswer[self->DefaultWildestAnswer], self->MultipleAnswers[self->DefaultWildestAnswer],TRUE,FALSE,self->drawn);
	    else if(self->dv) dialogv_ActivateButton(self->dv, curpt-1);
	    return;
	}
	if (curpt == startpt) {
	    framemessage_DisplayString(self->messageLine, 0, "That keystroke does not match any of the choices.");
	    return;
	}
    }
}

static void ConfirmDefaultAnswer(self)
struct frame *self;
{
    ConsiderReturning(self, self->DefaultWildestAnswer);
}

static void Cancel(self)
struct frame *self;
{
    /* ^G key binding */
    if (self->StackPos != frame_GlobalStackCount) {
	framemessage_DisplayString(self->messageLine, 0, "Please answer the other dialog box first.");
	return;
    }
#ifdef MUSTANSWER
    if (self->IsBlocking) {
	framemessage_DisplayString(self->messageLine, 0, "This is a question that MUST be answered.");
    } else {
	framemessage_DisplayString(self->messageLine, 0, "Question Cancelled.");
	self->IsAsking = 0;
	self->WildestAnswer = -1;
	if(self->dv) dialogv_CancelQuestion(self->dv);
    }	
#else /* MUSTANSWER */
    framemessage_DisplayString(self->messageLine, 0, "Question Cancelled.");
    self->IsAsking = 0;
    self->WildestAnswer = -1;
    if(self->dv) dialogv_CancelQuestion(self->dv);
    
#endif /* MUSTANSWER */
    if(self->hasDialogMessage){
	framemessage_CancelQuestion(self->dialogLine);
    }

}

static void retractCursors(self)
struct frame *self;
{
    if(cursor_IsPosted(self->octcursor))
	frame_RetractCursor(self, self->octcursor);
    if(cursor_IsPosted(self->arrowcursor))
	frame_RetractCursor(self, self->arrowcursor);
    frame_PostMenus(self,NULL);
}

static void
TidyUp(self)
struct frame *self;
{
    struct pendingupdates *pu;
    if(!self->UseBuiltinDialogs) {
	self->UsingDialog=FALSE;
	if(self->dv) {
	    struct menulist *dup=dialogv_GetExtraMenus(self->dv);
	    struct keystate *dupk=dialogv_GetExtraKeys(self->dv);
	    dialogv_Destroy(self->dv);
	    if(dup!=NULL) menulist_ClearChain(dup);
		menulist_Destroy(dup);
	    if(dupk!=NULL) keystate_Destroy(dupk);
	    self->dv=NULL;
	}
    }
    self->IsAsking =  self->NumAnswerChoices = 0;  
    self->IsBlocking = TRUE;
    self->drawn = FALSE;
    self->DefaultWildestAnswer = self->StackPos = -1;
    self->PositionalPreference = message_Center;
    self->MultipleAnswers = NULL;
    while (self->uplist) {
	pu = self->uplist->next;
	free(self->uplist);
	self->uplist = pu;
    }
    if (self->HeightsOfAnswer) {
	free(self->HeightsOfAnswer);
	self->HeightsOfAnswer = NULL;
    }
    if(self->hasDialogMessage && self->dialogView  && self->UseBuiltinDialogs) frameview_UnlinkTree(self->dialogView);
    if (self->DialogBuffer != NULL) {
	if (self->DialogBufferView != NULL)
	    buffer_RemoveView(self->DialogBuffer, self->DialogBufferView);
	buffer_RemoveObserver(self->DialogBuffer, self);
    }
    self->DialogBuffer = NULL;
    self->DialogBufferView = NULL;
    self->DialogTargetView = NULL;
    self->hasDialogMessage = 0;
    self->PotentialChoice = 0;
}
static isDialogChild(self, v)
struct frame *self;
struct view *v;
{
    while(v->parent != NULL && v->parent != (struct view *) self) 
	v = v->parent;
    return(v == self->DialogBufferView || v == (struct view *)self->dialogView);
}
void frame__WantUpdate(self, v)
struct frame *self;
struct view *v;
{
    if (self->IsAsking && !self->UsingDialog) {
	struct pendingupdates *pu;
	if(isDialogChild(self,v)) {
	    super_WantUpdate(self,v);
	    return;
	}
	if (v != (struct view *) self) {
	    pu = (struct pendingupdates *) malloc(sizeof (struct pendingupdates));
	    pu->next = self->uplist;
	    pu->v = v;
	    self->uplist = pu;
	}
	super_WantUpdate(self, self); 
	self->UpdateRequested = TRUE;
    } else {
	super_WantUpdate(self, v);
    }
}
/*
  * This function is called when the window manager requests
  * a delete window.
  */
static void delete_window_request(im, self)
struct im *im;
struct frame *self;
{
    framecmds_DeleteWindow(self);
}

void frame__LinkTree(self, parent)
struct frame *self;
struct view *parent;
{
    struct im *oldim=frame_GetIM(self);
    super_LinkTree(self, parent);
    if(oldim) {
	im_SetDeleteWindowCallback(oldim, NULL, NULL);
    }
    if(frame_GetIM(self) && im_GetDeleteWindowCallback(frame_GetIM(self)) == NULL) {
	im_SetDeleteWindowCallback(frame_GetIM(self), delete_window_request, (long)self);
    }
}


#define ERROR (-1)
int frame__DisplayString(self, priority, string)
struct frame *self;
int priority;
char *string;
{
    char *mychoices[3];
    long result,defaultChoice;
    defaultChoice = 1;
    mychoices[0] = string;
    mychoices[1] = "continue";
    mychoices[2] = NULL;
    if(!ButtonInteract(self,mychoices,defaultChoice,&result,priority > frame_MUSTANSWER)){
	return(0);
    }
    return ERROR;
}

static void ReturnInterface(rock, ind, brock)
struct frame *rock;
int ind;
long brock;
{
    ConsiderReturning(rock, ind+1);
}


static struct view *PrepareForStringInput(self,prompt,bufferSize,CompButtons)
struct frame *self;
char *prompt;
int bufferSize; /* Is actual sizeof buffer including NUL. */
boolean CompButtons;
{
    static char *ans[6];
    struct view *focus;
    self->AwaitingFocus = 1;
    focus = im_GetInputFocus(frame_GetIM(self));
    if (focus) frame_WantInputFocus(self,self); /* we take the focus ourself so that when the dialogview asks for it, no-one else has to redraw */
    while(im_Interact(0)); /* let the current focus get his update before the box is drawn */
    self->AwaitingFocus = 0;
    self->MultipleAnswers = ans;
    self->MultipleAnswers[0] = prompt;
    self->MultipleAnswers[ACCEPT] = "accept";
    self->MultipleAnswers[CANCEL] = "cancel";
    if(CompButtons) {
	self->MultipleAnswers[COMPLETE] = "complete";
	self->MultipleAnswers[HELP] = "help";
	self->MultipleAnswers[5] = NULL;
	self->NumAnswerChoices = 4 ;
    }
    else {
	self->MultipleAnswers[3] = NULL;
	self->NumAnswerChoices = 2 ;
    }
    self->IsAsking = 1;
    self->hasDialogMessage = TRUE;
    self->DefaultWildestAnswer = 0;
    self->WildestAnswer = -1;
    self->StackPos = ++frame_GlobalStackCount;
    ComputeSize(self);
    self->dv=NULL;
    if(!self->UseBuiltinDialogs) {
	self->dv=dialogv_Create(self->MultipleAnswers+1, NULL /*	Default	Font */, environ_GetProfileInt("dialogstyle", 4 /* sbutton_MOTIF */));
	if(self->dv) {
	    self->UsingDialog=TRUE;

	    if(CompButtons) dialogv_SetLayout(self->dv, 2, 2);
	    else dialogv_SetLayout(self->dv, 1, 2);

	    text_InsertCharacters(dialogv_GetTextData(self->dv), 0, *self->MultipleAnswers, strlen(*self->MultipleAnswers));
	    text_ReadTemplate(dialogv_GetTextData(self->dv), "dialog", FALSE);
	    dialogv_InstallRider(self->dv, (struct view *)self->dialogLine->messageView);
	}
    }
    PrepareMenus(self);
    if(self->dv) {
	long pos=im_Centered;
	switch(self->PositionalPreference) {
	    case message_OnTop:
		pos|=im_AtTop;
		break;
	    case message_OnBottom:
		pos|=im_AtBottom;
		break;
	    case message_Center:
		pos|=im_InMiddle;
	    default:
		break;
	}
	dialogv_PostInput(self->dv, frame_GetIM(self), frame_GetIM(self), ReturnInterface, self, self->IsBlocking, pos);
	dialogv_PostDefaultHandler(self->dv, "message", self->messageLine);
    }
    if(!self->UpdateRequested)frame_WantUpdate(self, self);
    while(im_Interact(0)); 
   	    
    return focus;
}
int frame__AskForString(self, priority, prompt, defaultString, buffer, bufferSize)
struct frame *self;
int priority;
char *prompt, *defaultString, *buffer;
int bufferSize; /* Is actual sizeof buffer including NUL. */
{
    int i;
    struct view *focus;
    char *answer;
    if (self->IsAsking || self->AwaitingFocus) return(-1);
    answer=im_GetAnswer();
    if(answer==NULL) {
	if(im_AnswerWasCancel()) return (-1);

	focus = PrepareForStringInput(self,prompt,bufferSize,FALSE);
	frame_SetReturnFocus(self, focus);
	i = framemessage_AskForString(self->dialogLine, 0 ,"", defaultString, buffer, bufferSize);
	while(im_Interact(0));
	--frame_GlobalStackCount;
	retractCursors(self);
	if(!self->UsingDialog && !RestoreBits(self))
	    CannotRestoreBits(self);
	TidyUp(self);
	if(i>=0) im_RecordAnswer(buffer);
	else im_RecordCancellation();
	frame_ReturnFocus(self);
    } else {
    }
    
    return i;
}
int frame__AskForPasswd(self, priority, prompt, defaultString, buffer, bufferSize)
struct frame *self;
int priority;
char *prompt, *defaultString, *buffer;
int bufferSize; /* Is actual sizeof buffer including NUL. */
{
    int i;
    struct view *focus;
    char *answer;
    if (self->IsAsking || self->AwaitingFocus) return ERROR;
    answer=im_GetAnswer();
    if(answer==NULL) {
	if(im_AnswerWasCancel()) return ERROR;

	focus = PrepareForStringInput(self,prompt,bufferSize,FALSE);
	frame_SetReturnFocus(self, focus);
	i = framemessage_AskForPasswd(self->dialogLine, 0 ,"", defaultString, buffer, bufferSize);
	while(im_Interact(0));
	--frame_GlobalStackCount;
	retractCursors(self);
	if(!self->UsingDialog && !RestoreBits(self))
	    CannotRestoreBits(self);
	TidyUp(self);
	if(i>=0) im_RecordAnswer(buffer);
	else im_RecordCancellation();
	frame_ReturnFocus(self);
    } else {
	strncpy(buffer, answer, bufferSize);
	buffer[bufferSize-1]='\0';
	return 0;
    }
    return i;
}


int frame__AskForStringCompleted(self, priority, prompt, defaultString, buffer, bufferSize, keystate, completionProc, helpProc, completionData, flags)
struct frame *self;
int priority;
char *prompt, *defaultString, *buffer;
int bufferSize; /* Is actual sizeof buffer including NUL. */
struct keystate *keystate;
enum message_CompletionCode (*completionProc)(/* char *string, long rock, char *buffer, int buffersize */);
int (*helpProc)(/* char *partialKeyword, long rock, int (*helpTextFunction)(), long helpTextRockchar */);
long completionData;
int flags;
{  
    int i=ERROR;
    struct view *focus;
    char *answer;
    if (self->IsAsking || self->AwaitingFocus) return(-1);
    answer=im_GetAnswer();
    if(answer==NULL) {
	if(im_AnswerWasCancel()) return ERROR;

	focus = PrepareForStringInput(self,prompt,bufferSize,TRUE);
	frame_SetReturnFocus(self, focus);
	i = framemessage_AskForStringCompleted(self->dialogLine, 0 ,"", defaultString, buffer, bufferSize, keystate, completionProc, helpProc, completionData, flags);

	while(im_Interact(0));

	--frame_GlobalStackCount;
	retractCursors(self);

	if(!self->UsingDialog && !RestoreBits(self))
	    CannotRestoreBits(self);

	TidyUp(self);

	if(i>=0) im_RecordAnswer(buffer);
	else im_RecordCancellation();
	

	frame_ReturnFocus(self);
    } else {
	strncpy(buffer, answer, bufferSize);
	buffer[bufferSize-1]='\0';
	return 0;
    }
    return i;
}

int frame__MultipleChoiceQuestion(self, priority, prompt, defaultChoice, result, choices, abbrevKeys)
struct frame *self;
int priority;
char *prompt;
long defaultChoice;
long *result;
char **choices;
char *abbrevKeys;
{
    char *mychoices[100];
    int i;
    char *answer=im_GetAnswer();
    if(answer==NULL) {
	if(im_AnswerWasCancel()) return ERROR;

	mychoices[0] = prompt;
	defaultChoice++;
	i = 0;
	do { 
	    mychoices[i+1] = choices[i];
	    if(choices[i] == '\0') break;
	} while (choices[i++]);
	if(!ButtonInteract(self,mychoices,defaultChoice,result,priority > frame_MUSTANSWER)){
	    (*result)--;
	    im_RecordAnswer(choices[*result]);
	    return(0);
	} else im_RecordCancellation();
    } else {
	i=0;
	while(*choices) {
	    if(strcmp(*choices, answer)==0) {
		*result=i;
		return 0;
	    }
	    i++;
	    choices++;
	}
    }
    return ERROR;
}

static void SingleLine(self, key)
struct frame *self;
long key;
{
    self->messageView->lines=1;
    frameview_WantNewSize(self->messageView, self->messageView);

}


boolean frame__InitializeClass(classID)
    struct classheader *classID;
{
    char c[2], *s;

    ResizableMessageLine = environ_GetProfileSwitch("ResizableMessageLine", TRUE);
    mykm = keymap_New();
    returnconsidered = proctable_DefineProc("frame-consider-coming",  (procedure) ConsiderReturning, &frame_classinfo, NULL, "consider selecting an answer");
    cancel = proctable_DefineProc("frame-cancel",  (procedure) Cancel, &frame_classinfo, NULL, "Try to cancel dialog box");
    confirmAnswer = proctable_DefineProc("frame-confirm",  (procedure) ConfirmDefaultAnswer, &frame_classinfo, NULL, "Select the default answer");
    gotkey = proctable_DefineProc("frame-got-key",  (procedure) GotKey, &frame_classinfo, NULL, "change the default answer");

    proctable_DefineProc("frame-single-line-message", (procedure)SingleLine, &frame_classinfo, NULL, "Reset the message line to one line.");
    
    c[1] = '\0';
    s = c;
    for (*s = 'a'; *s <= 'z'; ++*s) {
	keymap_BindToKey(mykm, s, gotkey, *s);
    }
    for (*s = 'A'; *s <= 'Z'; ++*s) {
	keymap_BindToKey(mykm, s, gotkey, *s);
    }
    for (*s = '0'; *s <= '9'; ++*s) {
	keymap_BindToKey(mykm, s, gotkey, *s);
    }
    keymap_BindToKey(mykm, "\007", cancel, 0);
    keymap_BindToKey(mykm, "\015", confirmAnswer, 0);
    return TRUE;
}

static boolean
FindBuffer(f,b)
struct frame *f;
struct buffer *b;
{
/*
  Little, dippy routine passed to frame_Enumerate to find the
  frame which contains the buffer we want.
*/

  return(frame_GetBuffer(f)==b);
}

struct frame *frame__FindFrameForBuffer(classhdr, b)
struct classheader *classhdr;
struct buffer *b;
{
/*
  This tries to find the frame of our buffer.  If there is no
  such frame, make one
*/

  struct frame *f;

  if ((f = frame_Enumerate(FindBuffer, (long) b)) == NULL) {
    /* No frame--need to map buffer to new window */

    if((f = frame_New()) == NULL) {
	fprintf(stderr,"frame: Could not allocate enough memory.\n");
	return NULL;
    }

    frame_SetCommandEnable(f, TRUE);
    frame_SetBuffer(f, b, TRUE);
  }
  return f;
}

struct frame *frame__GetFrameInWindowForBuffer(classhdr, b)
struct classheader *classhdr;
struct buffer *b;
{
    struct frame *f;
    struct im *im;

    f = frame_FindFrameForBuffer(b);

    if (f != NULL) {
	if (frame_GetIM(f) == NULL) {
	    if((im = im_Create(NULL)) == NULL) {
		fprintf(stderr,"frame: Could not create new window.\n");
		return NULL;
	    }
	    im_SetView(im, f);
	    frame_PostDefaultHandler(f, "message", frame_WantHandler(f, "message"));
	}
    }

    return f;
}
