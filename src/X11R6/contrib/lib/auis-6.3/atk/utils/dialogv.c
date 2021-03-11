/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/utils/RCS/dialogv.c,v 1.10 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#ifndef lint
#endif /* lint */

#include <class.h>
#include <andrewos.h>
#include <environ.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <text.ih>
#include <textv.ih>
#include <sbutton.ih>
#include <sbuttonv.ih>
#include <menulist.ih>
#include <keystate.ih>

#include <im.ih>

#include <dialog.ih>
#include <dialogv.eh>

#define PADDING 5

boolean dialogv__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

boolean dialogv__InitializeObject(classID, self)
struct classheader *classID;
struct dialogv *self;
{
    
    self->cont=NULL;

    self->lastchoice=(-1);
    
    self->didsizeonce=FALSE;
    
    self->client=NULL;
    
    self->extrakeys=NULL;

    self->extramenus=NULL;

    self->hr.func=NULL;
    
    self->text=textview_New();
    if(!self->text) return FALSE;

    self->buttons=sbuttonv_New();
    if(!self->buttons) return FALSE;

    self->rider=self->sidekick=NULL;

    
    self->twidth = self->theight = self->bwidth = self->bheight = self->rheight = self->rwidth=0;
    self->sheight=self->swidth=0;
    
    self->destroyall=FALSE;
    
    return TRUE;
}

void dialogv__FinalizeObject(classID, self)
struct classheader *classID;
struct dialogv *self;
{
    struct im *im=dialogv_GetIM(self);
    /*  remove all knowledge of an object from this dialogv immediately after the object has been destroyed or dissociated with this dialogv, otherwise it may get re-associated depending on the order in which things are done in this function. */

    dialogv_UnlinkTree(self);
    
    if(self->text) {
	textview_Destroy(self->text);
	self->text=NULL;
    }

    if(self->buttons) {
	sbuttonv_Destroy(self->buttons);
	self->buttons=NULL;
    }

    if(self->rider) {
	view_UnlinkTree(self->rider);
	self->rider=NULL;
    }
    if(self->sidekick) {
	view_UnlinkTree(self->sidekick);
	self->sidekick=NULL;
    }

    
    if(self->destroyall) {
	struct dialog *dobj=(struct dialog *)dialogv_GetDataObject(self);
	if(dobj) dialog_Destroy(dobj);
	
	if(im) {
	    im_SetView(im, NULL);
	    im_Destroy(im);
	}
    }
    
}

void dialogv__SetDataObject(self, obj)
struct dialogv *self;
struct dataobject *obj;
{
    struct dialog *dobj=(struct dialog *)obj;
    if(dobj) {

	if(dialog_GetText(dobj)) {
	    if(self->text) textview_SetDataObject(self->text, dialog_GetText(dobj));
	} else {
	    fprintf(stderr,"dialogv: Warning SetDataObject called on dialog with no text!\n");
	    fflush(stderr);
	}
	if(dialog_GetButtons(dobj)) {
	    if(self->buttons) sbuttonv_SetDataObject(self->buttons, dialog_GetButtons(dobj));
	} else {
	    fprintf(stderr,"dialogv: Warning SetDataObject called on dialog with no buttons!\n");
	    fflush(stderr);
	}
    }
    super_SetDataObject(self, obj);
}

void dialogv__FullUpdate(self, type, left, top, width, height)
struct dialogv *self;
enum view_UpdateType type;
long left, top, width, height;
{
    int tpos=0;
    double topcolor[3];
    struct rectangle int1, int2, int3;
    struct rectangle r;
    struct dialog *d=(struct dialog *)dialogv_GetDataObject(self);
    
    if(!dialogv_GetIM(self) || !self->text || !self->buttons || !d) return;

    dialogv_GetLogicalBounds(self, &r);
    if(self->sidekick) {
	int1.width=r.width/2-2;
	int1.height=r.height;
	int1.left=0;
	int1.top=0;
	int2.top=0;
	int2.left=r.width/2 + 2;
	int2.width=r.width/2 - 2;
	int2.height=r.height;
    } else {
	int1.width=r.width;
	int1.height=r.height;
	int1.left=0;
	int1.top=0;
    }
    if(!self->didsizeonce) {
	long dw, dh;
	dialogv_DesiredSize(self, r.width, r.height, view_NoSet, &dw, &dh);
    }

    switch(type) {
	case view_Remove:
	    textview_FullUpdate(self->text, type, 0, 0, 0, 0);
	    sbuttonv_FullUpdate(self->buttons, type, 0, 0, 0, 0);
	    if(self->rider) {
		view_FullUpdate(self->rider, type, 0, 0, 0, 0);
	    }
	    if(self->sidekick) {
		view_FullUpdate(self->sidekick, type, 0, 0, 0, 0);
	    }
	    break;
	case view_MoveNoRedraw:
	    sbuttonv_DrawBorder(self, int1.left, int1.top, int1.width, int1.height, dialog_GetPrefs(d), sbuttonv_BORDEROUT, FALSE, &int1);
	    if(self->sidekick) sbuttonv_DrawBorder(self, int2.left, int2.top, int2.width, int2.height, dialog_GetPrefs(d), sbuttonv_BORDEROUT, FALSE, &int2);
	    tpos=int1.top;
	    sbuttonv_DrawBorder(self, int1.left+PADDING, int1.top, int1.width-2*PADDING, self->theight, dialog_GetPrefs(d), sbuttonv_BORDERIN, FALSE, &int3);
	    textview_InsertViewSize(self->text, self, int3.left, int3.top, int3.width, int3.height);
	    tpos+=self->theight+PADDING;
	    if(self->rider) {
		sbuttonv_DrawBorder(self, int1.left+PADDING, tpos, int1.width-2*PADDING, self->rheight, dialog_GetPrefs(d), sbuttonv_BORDEROUT, FALSE,  &int3);
		view_InsertViewSize(self->rider, self, int3.left, int3.top, int3.width, int3.height);
		tpos+=self->rheight;
	    }
	    sbuttonv_InsertViewSize(self->buttons, self, (int1.width-self->bwidth)/2+int1.left - 1, tpos, self->bwidth, self->bheight);
	    if(self->sidekick) {
		view_InsertViewSize(self->sidekick, self, int2.left, int2.top, int2.width, int2.height);
	    }
	    break;
	case view_PartialRedraw:
	    break;
	case view_LastPartialRedraw:
	case view_FullRedraw:
	    sbuttonv_InteriorBGColor(self, dialog_GetPrefs(d), FALSE, topcolor);
	    sbuttonv_DrawBorder(self, int1.left, int1.top, int1.width, int1.height, dialog_GetPrefs(d), sbuttonv_BORDEROUT, TRUE,  &int1);
	    if(self->sidekick) sbuttonv_DrawBorder(self, int2.left, int2.top, int2.width, int2.height, dialog_GetPrefs(d), sbuttonv_BORDEROUT, TRUE,  &int2);
	    tpos=int1.top+PADDING;
	    sbuttonv_DrawBorder(self, int1.left+5, tpos, int1.width-2*PADDING, self->theight, dialog_GetPrefs(d), sbuttonv_BORDERIN, TRUE,  &int3);
	    textview_InsertViewSize(self->text, self, int3.left, int3.left, int3.width, int3.height);
	    tpos+=self->theight+PADDING;
	    if(self->rider) {
		sbuttonv_DrawBorder(self, int1.left + 5, tpos, int1.width - 2*PADDING, self->rheight, dialog_GetPrefs(d), sbuttonv_BORDEROUT, TRUE,  &int3);
		view_InsertViewSize(self->rider, self, int3.left, int3.top, int3.width, int3.height);
		tpos+=self->rheight+1;
	    }
	    sbuttonv_SetBGColor(self->buttons, topcolor[0], topcolor[1], topcolor[2]);
	    sbuttonv_InsertViewSize(self->buttons, self, int1.left + (int1.width-self->bwidth)/2 - 1, tpos, self->bwidth, self->bheight);
	    if(self->sidekick) {
		view_InsertViewSize(self->sidekick, self, int2.left, int2.top, int2.width, int2.height);
	    }

	    textview_SetBGColor(self->text, topcolor[0], topcolor[1], topcolor[2]);
	    textview_FullUpdate(self->text, type, 0, 0, int1.width, self->theight);
	    if(self->rider) {
		view_SetBGColor(self->rider, topcolor[0], topcolor[1], topcolor[2]);
		view_FullUpdate(self->rider, type, 0, 0, self->rwidth, self->rheight);
	    }
	    if(self->sidekick) {
		view_FullUpdate(self->sidekick, type, 0, 0, int2.width, int2.height);
	    }
	    sbuttonv_FullUpdate(self->buttons, type, 0, 0, self->bwidth, self->bheight);
    }
    
}

struct view *dialogv__Hit(self, action, x, y, numberOfClicks)
struct dialogv *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{

    if(self->rider) {
	long rx=view_EnclosedXToLocalX(self->rider, x);
	long ry=view_EnclosedYToLocalY(self->rider, y);
	if( rx>=0 && rx<view_GetLogicalWidth(self->rider) && ry>=0 && ry<view_GetLogicalHeight(self->rider)) {
	    return view_Hit(self->rider, action, view_EnclosedXToLocalX(self->rider, x),view_EnclosedYToLocalY(self->rider, y), numberOfClicks);
	}
    }
    if(self->sidekick) {
	long sx=view_EnclosedXToLocalX(self->sidekick, x);
	long sy=view_EnclosedYToLocalY(self->sidekick, y);
	
	if(sx>=0 && sx<view_GetLogicalWidth(self->sidekick) && sy>=0 && sy<view_GetLogicalHeight(self->sidekick)) {
	    return view_Hit(self->sidekick, action, sx, sy, numberOfClicks);
	}
    }
    if(self->buttons) return sbuttonv_Hit(self->buttons, action, sbuttonv_EnclosedXToLocalX(self->buttons, x), sbuttonv_EnclosedYToLocalY(self->buttons, y), numberOfClicks);
    return (struct view *)self;
}

#define PERCENTAGE(x, p) (((x)*(p))/100)

enum view_DSattributes dialogv__DesiredSize(self, width, height, pass, dWidth, dHeight)
struct dialogv *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dHeight;
{
    long iw, ih;
    long dummy;
    int  oldheight;
    struct dialog *d=(struct dialog *)dialogv_GetDataObject(self);
    int style;
    
    self->didsizeonce=TRUE;
    
    if(!self->buttons || !self->text || !d) {
	*dWidth = width;
	*dHeight = (height > 2048) ? 256 :height;
	return view_HeightFlexible | view_WidthFlexible;
    }

    style=dialog_GetStyle(d);
    
    if(width<0 || width>32767) width=((struct view *)self)->parent?view_GetLogicalWidth(((struct view *)self)->parent): 500;
    
    
    (void) sbuttonv_DesiredSize(self->buttons, 0, 0, view_NoSet, &self->bwidth, &self->bheight);

    
    if(self->rider || self->sidekick) width=PERCENTAGE(width, 90);
    else {
	long low=self->bwidth, high=PERCENTAGE(width, 90);
	/* Please, there MUST be a better way! */
	(void) textview_DesiredSize(self->text, high, height-self->bheight, view_WidthSet, &self->twidth, &oldheight);
	sbuttonv_SizeForBorder(self->text, sbuttonv_Enclosing, style, FALSE, self->twidth, oldheight, &self->twidth, &oldheight);
	while(low<high-20) {
	    sbuttonv_SizeForBorder(self->text, sbuttonv_Interior, style, FALSE, (low+high)/2-2*PADDING, height-self->bheight, &self->twidth, &self->theight);
	    (void) textview_DesiredSize(self->text, self->twidth, self->theight, view_WidthSet, &self->twidth, &self->theight);
	    sbuttonv_SizeForBorder(self->text, sbuttonv_Enclosing, style, FALSE, self->twidth, self->theight, &self->twidth, &self->theight);
	    if(self->theight>oldheight) low=(low+high)/2+1;
	    else high=(low+high)/2;
	}
	width=high+10; /* SORRY.... this is a magic fudge factor, please fix if you can see how. */
    }
    
    if(width < self->bwidth) width=self->twidth=self->bwidth;
    else self->twidth=width;

    if(self->sidekick) {
	self->twidth=width/2;
	self->bwidth=width/2;
	self->swidth=width/2;
    }
    
    sbuttonv_SizeForBorder(self->text, sbuttonv_Interior, style, FALSE, self->twidth, height-self->bheight, &self->twidth, &self->theight);
    (void) textview_DesiredSize(self->text, self->twidth, height-self->bheight, view_WidthSet, &dummy, &self->theight);
    sbuttonv_SizeForBorder(self->text, sbuttonv_Enclosing, style, FALSE, self->twidth, self->theight, &dummy, &self->theight);

    self->twidth-=2*PADDING;
    
    if(self->rider) {
	sbuttonv_SizeForBorder(self->rider, sbuttonv_Interior, style, TRUE,  self->twidth-2*PADDING, height-self->bheight-self->theight, &iw, &ih);
	(void) view_DesiredSize(self->rider, iw, ih, view_WidthSet, &dummy, &self->rheight);
	sbuttonv_SizeForBorder(self->rider, sbuttonv_Enclosing, style, TRUE, dummy, self->rheight,  &dummy, &self->rheight);
    }

    if(self->sidekick) {
	sbuttonv_SizeForBorder(self->sidekick, sbuttonv_Interior, style, FALSE, width/2, self->theight + self->bheight + self->rheight, &iw, &ih);
	(void) view_DesiredSize(self->sidekick, iw, ih, view_WidthSet, &dummy, &self->sheight);
	sbuttonv_SizeForBorder(self->sidekick, sbuttonv_Enclosing, style, FALSE, dummy, self->sheight, &dummy, &self->sheight);
	
    }
    sbuttonv_SizeForBorder(self, sbuttonv_Enclosing, style, FALSE, width, self->theight + self->bheight +  self->rheight +2*PADDING, dWidth, dHeight);
    return view_Fixed;
}

void dialogv__InstallRider(self, rider)
struct dialogv *self;
struct view *rider;
{
    long dumbw, dumbh;
    self->roffset=PADDING;
    self->rider=rider;
    if(rider) view_LinkTree(rider, self);
    if(self->client) (void) dialogv_DesiredSize(self, view_GetLogicalWidth(self->client), view_GetLogicalHeight(self->client), view_WidthSet, &dumbw, &dumbh);
    if(rider) view_WantInputFocus(rider, rider);
}

void dialogv__InstallSidekick(self, sidekick)
struct dialogv *self;
struct view *sidekick;
{
    long dumbw, dumbh;
    self->sidekick=sidekick;
    if(sidekick) view_LinkTree(sidekick, self);
    if(self->client) (void) dialogv_DesiredSize(self, view_GetLogicalWidth(self->client), view_GetLogicalHeight(self->client), view_WidthSet, &dumbw, &dumbh);
}
	
void dialogv__LinkTree(self, parent)
struct dialogv *self;
struct view *parent;
{
    struct im *im;
    struct dialog *d=(struct dialog *)dialogv_GetDataObject(self);
    
    super_LinkTree(self, parent);
    
    im=dialogv_GetIM(self);
    
    if(d && im) {
	dialogv_SetForegroundColor(self, dialog_GetForeground(d), 0, 0, 0);
	dialogv_SetBackgroundColor(self, dialog_GetBackground(d), 0, 0, 0);
	dialogv_SetForegroundColor(self, dialog_GetForeground(d), 0, 0, 0);
    }
    
    if(self->buttons) sbuttonv_LinkTree(self->buttons, self);
    
    if(self->text) {
	textview_LinkTree(self->text, self);
	if(d && im) {
	    textview_SetForegroundColor(self->text, dialog_GetForeground(d), 0, 0, 0);
	    textview_SetBackgroundColor(self->text, dialog_GetBackground(d), 0, 0, 0);
	    textview_SetForegroundColor(self->text, dialog_GetForeground(d), 0, 0, 0);
	}
    }
    if(self->rider) {
	view_LinkTree(self->rider, self);
	if(d && im) {
	    view_SetForegroundColor(self->rider, dialog_GetForeground(d), 0, 0, 0);
	    view_SetBackgroundColor(self->rider, dialog_GetForeground(d), 0, 0, 0);
	    view_SetForegroundColor(self->rider, dialog_GetForeground(d), 0, 0, 0);
	}
    }
    if(self->sidekick) {
	view_LinkTree(self->sidekick, self);
	if(d && im) {
	    view_SetForegroundColor(self->sidekick, dialog_GetForeground(d), 0, 0, 0);
	    view_SetBackgroundColor(self->sidekick, dialog_GetForeground(d), 0, 0, 0);
	    view_SetForegroundColor(self->sidekick, dialog_GetForeground(d), 0, 0, 0);
	}
    }
}

struct dialogv *dialogv__Create(classID, list, font, style)
struct classheader *classID;
char **list;
char *font;
int style;
{
    struct sbutton *s;
    struct dialog *d=dialog_New();
    struct dialogv *dv=dialogv_New();
    struct sbutton_list *sl=NULL;
    int count=0, i;
    struct sbutton_prefs *prefs;

    
    if(!d || !dv) {
	if(d) dialog_Destroy(d);
	if(dv) dialogv_Destroy(dv);
	if(prefs) free(prefs);
	return NULL;
    }
    
    prefs=sbutton_GetDefaultPrefs(dialog_GetButtons(d));
    
    dv->destroyall=TRUE;
    
    while(*list && list[count]) count++;

    if(!count) {
	dialog_Destroy(d);
	dialogv_Destroy(dv);
	return NULL;
    }

    sl=(struct sbutton_list *)malloc(sizeof(struct sbutton_list)*(count+1));

    if(!sl) {
	dialog_Destroy(d);
	dialogv_Destroy(dv);
	return NULL;
    }

    for(i=0;i<count;i++) {
	sl[i].label=list[count-i-1];
	sl[i].trigger=NULL;
	sl[i].rock=0;
	sl[i].lit=FALSE;
    }
    sl[count].label=NULL;
    
    s=sbutton_CreateFilledSButton(prefs, sl);
    free(sl);
    
    if(!s) {
	dialog_Destroy(d);
	dialogv_Destroy(dv);
	return NULL;
    }
    
    dialog_SetButtons(d, s);
        
    dialogv_SetDataObject(dv, d);

    if(dialogv_GetTextView(dv) && dialogv_GetTextData(dv)) text_Clear(dialogv_GetTextData(dv));

    
    return dv;
}


static void HitFunc(self, rock, button, brock)
struct sbutton *self;
struct dialogv *rock;
int button;
long brock;
{
    rock->lastchoice=sbutton_GetCount(self)-button-1;
    *(rock->cont)=FALSE;
}

void dialogv__PostMenus(self, ml)
struct dialogv *self;
struct menulist *ml;
{
    if(!ml) {
	super_PostMenus(self, self->extramenus);
	return;
    }
    if(self->extramenus && self->extramenus!=ml) {
	menulist_ClearChain(self->extramenus);
	menulist_ChainAfterML(self->extramenus, ml, ml);
	super_PostMenus(self, self->extramenus);
    } else super_PostMenus(self, ml);
}

void dialogv__ReceiveInputFocus(self)
struct dialogv *self;
{
    if(self->rider) view_ReceiveInputFocus(self->rider);
    else {
	if(self->extrakeys) {
	    self->extrakeys->next = NULL;
	    dialogv_PostKeyState(self, self->extrakeys);
	}
	if(self->extramenus) dialogv_PostMenus(self, self->extramenus);
    }
}

void dialogv__SetExtraMenus(self, ml)
struct dialogv *self;
struct menulist *ml;
{
    self->extramenus=ml;
}

void dialogv__SetExtraKeyState(self, ks)
struct dialogv *self;
struct keystate *ks;
{
    self->extrakeys=ks;
}

void dialogv__ActivateButton(self, ind)
struct dialogv *self;
int ind;
{
    struct sbutton *sb=dialogv_GetButtonsData(self);
    sbutton_ActivateButton(sb, sb->count-ind-1);
    self->lastchoice=ind;
}

void dialogv__DeActivateButton(self, ind)
struct dialogv *self;
int ind;
{
    struct sbutton *sb=dialogv_GetButtonsData(self);
    if(sb && dialogv_GetButtonsView(self)) {
	sbutton_DeActivateButton(sb, sb->count-ind-1);
	self->lastchoice=(-1);
    }
}

static void ConfigureFunc(self, rock, customrock, parent, x, y, w, h)
struct im *self;
long rock, customrock;
struct im *parent;
long *x, *y;
unsigned long *w, *h;
{
    (void) dialogv_DesiredSize((struct dialogv *)customrock, im_GetLogicalWidth(parent), im_GetLogicalHeight(parent), view_NoSet, w, h);
    im_NormalConfiguration(self, rock, customrock, parent, x, y, w, h);
}
    
int dialogv__PostChoice(dv, im, client, cflag, deflt, block, pos)
struct dialogv *dv;
struct im *im;
struct view *client;
boolean *cflag;
int deflt;
boolean block;
long pos;
{
    struct im *new;
    struct sbutton *sb=dialogv_GetButtonsData(dv);
    procedure oldconfigfunc;
    long oldconfigrock, oldcustomrock;

    if(pos==0) pos=im_InMiddle|im_Centered;
    dv->client=client;
    
      /* Horrible hack to get desired size for the dialog box! We link the dialogview into the view tree of the client  and then after we have created the window unlink it so that it can be linked into the new im.  This is all because in order to get an appropriate size it really needs to be able to get at info about the actual display and it's fonts */
    dialogv_LinkTree(dv, client);

    /* Set the default configuration function temporarily so that the configuration function here will get called to determine the desired size for the window */
    oldconfigfunc=im_DefaultConfigureFunction(ConfigureFunc);
    oldconfigrock=im_DefaultConfigureRock(pos);
    oldcustomrock=im_DefaultConfigureCustomRock((long)dv);

    if(block) new=im_CreateOverride(im);
    else new=im_CreateTransient(im);
    
    im_DefaultConfigureFunction(oldconfigfunc);
    im_DefaultConfigureRock(oldconfigrock);
    im_DefaultConfigureCustomRock(oldcustomrock);

    /* unlink the dialogview from the client view tree so that it can be re-linked into the new im's view tree, since the new im is on the same screen and the same display all the computations done while under the client view tree should still be valid */
    dialogv_UnlinkTree(dv);
    
    if(!new) return -1;

    im_SetBorderWidth(new, 0);

    im_SetView(new, dv);
    
    dialogv_WantInputFocus(dv, dv);
    sbutton_GetHitFunc(sb)=(procedure)HitFunc;
    sbutton_GetHitFuncRock(sb)=(long)dv;
    if(deflt>=0 && deflt<sb->count) dialogv_ActivateButton(dv, deflt);
    dv->lastchoice=(-1);
    dv->cont=cflag;
    while(*cflag) im_Interact(TRUE);

    if(dv->lastchoice>=0) return dv->lastchoice;
    else return -1;
}

static void Interface(b, rock, ind, brock)
struct sbutton *b;
struct dialogv_HitRock *rock;
int ind;
long brock;
{
    if(rock->func) rock->func(rock->rock, b->count-ind-1, brock);
}

int dialogv__PostInput(dv, im, client, choicefunc, choicerock, block, pos)
struct dialogv *dv;
struct im *im;
struct view *client;
procedure choicefunc;
long choicerock;
boolean block;
long pos;
{
    struct im *new;
    struct sbutton *sb=dialogv_GetButtonsData(dv);
    procedure oldconfigfunc;
    long oldconfigrock, oldcustomrock;
    
    if(pos==0) pos=im_InMiddle|im_Centered;
    
    dv->client=client;
    
    dv->hr.func=choicefunc;
    dv->hr.rock=choicerock;
    /* Horrible hack to get desired size for the dialog box! */
    dialogv_LinkTree(dv, client);

    oldconfigfunc=im_DefaultConfigureFunction(ConfigureFunc);
    oldconfigrock=im_DefaultConfigureRock(pos);
    oldcustomrock=im_DefaultConfigureCustomRock((long)dv);
    
    if(block) new=im_CreateOverride(im);
    else new=im_CreateTransient(im);
    
    im_DefaultConfigureFunction(oldconfigfunc);
    im_DefaultConfigureRock(oldconfigrock);
    im_DefaultConfigureCustomRock(oldcustomrock);

    dialogv_UnlinkTree(dv);
    if(!new) return -1;
    
    im_SetBorderWidth(new, 0);

    im_SetView(new, dv);

    sbutton_GetHitFunc(sb)=(procedure)Interface;
    sbutton_GetHitFuncRock(sb)=(long)&dv->hr;

    return 0;
}

void dialogv__Vanish(self)
struct dialogv *self;
{
    if(dialogv_GetIM(self)) im_VanishWindow(dialogv_GetIM(self));
}

void dialogv__UnVanish(self)
struct dialogv *self;
{
    if(dialogv_GetIM(self)) im_ExposeWindow(dialogv_GetIM(self));
}

void dialogv__SetLayout(self, rows, cols)
struct dialogv *self;
int rows, cols;
{
    if(!dialogv_GetButtonsData(self)) return;
    sbutton_SetLayout(dialogv_GetButtonsData(self), rows, cols, sbutton_GrowRows);
}

void dialogv__CancelQuestion(self)
struct dialogv *self;
{
    *(self->cont)=FALSE;
    self->lastchoice=(-1);
}
