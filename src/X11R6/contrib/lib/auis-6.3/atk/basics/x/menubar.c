/* ********************************************************************** *\
 *         Copyright IBM Corporation 1990,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/menubar.c,v 1.45 1994/04/14 02:22:44 rr2b Exp $";
#endif


#include <andrewos.h>
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <errprntf.h>
#include "menubar.h"
#include <scache.h>
#include <shadows.h>

#define MOREMENUPRIORITY 127
#define MAXPANEPRIORITY 100
#define MAXITEMPRIORITY 99

#define MENUEVENTS (LeaveWindowMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask)
	
#define MBLOCKSIZE 12

#define ITEMHSPACE(mb) ((mb)->mbi->prefs->itemhspace)
	
#define FONTHEIGHT(f) (f->max_bounds.ascent + f->max_bounds.descent)

#define PADDING(mb) ((mb)->mbi->prefs->padding)

#ifndef MAX
#define MAX(x, y) ((x<y)?y:x)
#endif

#define ITEMHEIGHT(mb,m) (MAX(FONTHEIGHT((mb)->mbi->prefs->itemfont),FONTHEIGHT((mb)->mbi->prefs->keysfont)) + 2*(SHADOWWIDTH(mb)))

#define KEYSASCENT(mb,m) ((mb)->mbi->prefs->keysfont->max_bounds.ascent)
	
#define ITEMASCENT(mb,m) (MAX((mb)->mbi->prefs->itemfont->max_bounds.ascent, (mb)->mbi->prefs->keysfont->max_bounds.ascent))

#define ITEMDESCENT(mb,m) ((mb)->mbi->prefs->itemfont->max_bounds.descent)
#define TITLEDESCENT(mb) ((mb)->mbi->prefs->titlefont->max_bounds.descent)

#define SHADOWWIDTH(mb) ((mb)->mbi->prefs->depth + PADDING(mb))
	
#define VSPACE(mb, m) ((mb)->mbi->prefs->vspacing)
	
#define HSPACE(mb, m) ((mb)->mbi->prefs->hspacing + 2*(SHADOWWIDTH(mb)))

#define GROUPSPACING(mb, m) ((mb)->mbi->prefs->groupspacing>=0?(mb)->mbi->prefs->groupspacing:(ITEMHEIGHT(mb, m) + VSPACE(mb, m))/3)
	
#define CARDHEIGHT(mb, m) (SHADOWWIDTH(mb)*2 + (m)->nitems*(VSPACE(mb, m) + ITEMHEIGHT(mb, m)) +(((m)->groupcount?(((m)->groupcount-1)*GROUPSPACING(mb, m)):((CountGroups(m)-1)*GROUPSPACING(mb, m)))))

#undef ABS
#define ABS(x) (((x)<0)?-(x):(x))

#define ISSUBMENU(it) ((it)->flags&SUBMENUFLAG)
#define ITEMACTIVE(it) (it && it->flags&ACTIVEFLAG && (!(it->flags&SUBMENUFLAG) ||((struct tmenu *)it->data)->nitems>0))


static long grayImage[] = {
0xAAAAAAAA,
0x55555555,
0xAAAAAAAA,
0x55555555,
0xAAAAAAAA,
0x55555555,
0xAAAAAAAA,
0x55555555
};

/* defaultgetdefault: inform the user of a problem if the client program (ie xim) doesn't provide a getdefault function. */
static char *defaultgetdefault(dpy, pname)
Display *dpy;
char *pname;
{
	fprintf(stderr,"Warning: no getdefault function defined for menubar!\n");
	return NULL;
}

static GetDefaultsFunction getdefault = (GetDefaultsFunction)defaultgetdefault;

/* getdefaultint: get a user specified integer under name pname, or return def if none is specified. */
static int getdefaultint(dpy, pname, def)
Display *dpy;
char *pname;
int def;
{
    char *v;
    v=getdefault(dpy,pname);
    if(!v) return def;
    else return atoi(v);
}

 /* getdefaultbool: get a user specified boolean under name pname, or return def if none is specified. */
static int getdefaultbool(dpy, pname, def)
Display  *dpy;
char  *pname;
int  def;
{
    char *v;
    v=getdefault(dpy,pname);
    if(!v) return def;
    switch(v[0]) {
	case '0':
	case 'N':
	case 'n':
	case 'f':
	case 'F':
	    return FALSE;
	case 'Y':
	case 'y':
	case '1':
	case 'T':
	case 't':
	    return TRUE;
	case 'o':
	case 'O':
	    return v[1]=='n' || v[1]=='N';
    }
    return FALSE;
}


/* mb_SetGetDefault: sets the function to be used to get preferences, the function will be called like: func(dpy, prefname) (where dpy is a Display *) */
GetDefaultsFunction mb_SetGetDefault(func)
GetDefaultsFunction func;
{
    GetDefaultsFunction x=getdefault;
    getdefault=func;
    return x;
}

static struct prefs_s *prefs=NULL;


/* CountGroups: count the number of item "groups" (ie sets of items with priority x[0-9] where x is the same for each member of the same group) */
static int CountGroups(lm)
struct tmenu *lm;
{
    unsigned int mask=0, thismask;
    struct titem *it=lm->items;
    lm->groupcount=0;
    while(it) {
	thismask=1<<(it->prio/10);
	if(!(mask&thismask)) {
	    lm->groupcount++;
	    mask|=thismask;
	}
	it=it->next;
    }
    lm->groupmask=mask;
    return lm->groupcount;
}

/* ClearMenu: free all the items in a menu, if a FreeItem function has been set their data will be freed if it is non-NULL.*/
static void ClearMenu(mb,m)
struct menubar *mb;
struct tmenu *m;
{
    struct titem *i=m->items;
    while(i) {
	struct titem *o=i->next;
	if(m!=mb->moremenu && !ISSUBMENU(i) && i->data && mb->mbi->FreeItem ) mb->mbi->FreeItem(i->data);
	scache_Free(i->name);
	if(i->keys) scache_Free(i->keys);
	free(i);
	i=o;
    }
    m->iwidth=m->mw + ((m->prio==MOREMENUPRIORITY)?mb->mbi->prefs->iconfont->max_bounds.lbearing + mb->mbi->prefs->iconfont->max_bounds.rbearing +10:0);
    m->nitems=0;
    m->groupcount=0;
    m->items=NULL;
}

/* DestroyMenu: destroy the menu m and the data for all the items it contains */
static void DestroyMenu(mb,m)
struct menubar *mb;
struct tmenu *m;
{
    ClearMenu(mb,m);
    if(m->lookup) free(m->lookup);
    
    scache_Free(m->title);
    free(m);
}

/* FindMenu: return a pointer to the menu card named by title or NULL if no such card exists. */
static struct tmenu *FindMenu(mb,title)
struct menubar *mb;
char *title;
{
    struct tmenu **t=mb->menus;
    int i;
    if(!title) title=mb->mainmenu;
    for(i=0;i<mb->nmenus && strcmp((*t)->title,title);i++,t++);
    if(i<mb->nmenus) return *t;
    else return NULL;
}

/* FindItem: return a pointer to the menu item named by name or NULL if no such item exists. */
static struct titem *FindItem(t,name)
struct tmenu *t;
char *name;
{
    struct titem *w=t->items;
    while(w && strcmp(w->name,name)) w=w->next;
    return w;
}

/* UpdateGeometry: used when the menubar contents need to be refit to the new size and/or location of the menubar's window. */
static void UpdateGeometry(mb)
struct menubar *mb;
{
    Window root;
    unsigned int bw,depth;
    XGetGeometry(mb->mbi->dpy,mb->mbi->client,&root,&(mb->mbi->x), &(mb->mbi->y), &(mb->mbi->w), &(mb->mbi->h),&bw,&depth);
    XTranslateCoordinates(mb->mbi->dpy, mb->mbi->client, RootWindow(mb->mbi->dpy, DefaultScreen(mb->mbi->dpy)), 0, 0, &(mb->mbi->x), &(mb->mbi->y), &root);
}

/* CreateMenu: internal function to create a menu card, with a given name and priority. */
static struct tmenu *CreateMenu(mb,title,prio)
struct menubar *mb;
char *title;
int prio;
{
    struct tmenu *t=(struct tmenu *)malloc(sizeof(struct tmenu));
    if(!t) return NULL;
    if(!title) title=mb->mainmenu;
    t->title=scache_Hold(title);
    t->titlelen=strlen(title);
    t->prio=prio<0 ? MAXPANEPRIORITY : prio;
    t->groupcount=0;
    t->nitems=0;
    t->items=NULL;
    t->mw=XTextWidth(mb->mbi->prefs->titlefont, title, t->titlelen);
    t->h=FONTHEIGHT(mb->mbi->prefs->titlefont);
    t->kwidth=0;
    t->iwidth=t->mw + ((t->prio==MOREMENUPRIORITY)?mb->mbi->prefs->iconfont->max_bounds.lbearing + mb->mbi->prefs->iconfont->max_bounds.rbearing +10:0);
    t->next=NULL;
    t->lookup=NULL;
    if(mb->nmenus==mb->mmenus) {
	struct tmenu **t2;
	mb->mmenus+=MBLOCKSIZE;
	t2=(struct tmenu **)malloc(mb->mmenus*sizeof(struct tmenu *));
	bcopy(mb->menus, t2, mb->nmenus*sizeof(struct tmenu *));
	free(mb->menus);
	mb->menus=t2;
    }
    mb->menus[mb->nmenus++]=t;
    mb->resort=True;
    return t;
}

/* AddItem: an internal routine which adds an item to a menu card given a pointer to the menu card, doesn't do much sanity checking of the input as that should be done elsewhere. */
static struct titem *AddItem(mb,t,item,prio,submenu,data)
struct menubar *mb;
struct tmenu *t;
char *item;
int prio;
int submenu;
char *data;
{
    struct titem *i;
    struct titem *w=t->items;
    
    int width=XTextWidth(mb->mbi->prefs->itemfont,item,strlen(item)) + ((t->prio==MOREMENUPRIORITY)?mb->mbi->prefs->iconfont->max_bounds.lbearing + mb->mbi->prefs->iconfont->max_bounds.rbearing +10:0);

    t->groupcount=0;
    if(i = FindItem(t,item)) {
	i->keys=NULL;
	if(!ISSUBMENU(i) && i->data && mb->mbi->FreeItem) mb->mbi->FreeItem(i->data);
	if(submenu) i->flags |= SUBMENUFLAG;
	else i->flags &= ~SUBMENUFLAG;
	if(submenu) {
	    i->data=(char *)FindMenu(mb, data);
	    if(!i->data) fprintf(stderr,"Warning: submenu %s not found\n",data?data:"(NULL)");
	} else i->data = data;
	i->prio = prio<1 ? i->prio : prio;
	return i;
    }
    
    i=(struct titem *)malloc(sizeof(struct titem));
    if(!i) return NULL;
    i->name=scache_Hold(item);
    i->keys=NULL;
    if(submenu) i->flags|=SUBMENUFLAG;
    else i->flags&=~SUBMENUFLAG;
    i->flags&=~ACTIVEFLAG;
    
    if(submenu) {
	i->data=(char *)FindMenu(mb,data);
	if(!i->data) fprintf(stderr,"Warning: submenu %s not found\n",data?data:"(NULL)");
    } else i->data=data;
    i->prio=prio<1 ? 1 : prio;
    
    if(w) {
	if(i->prio<w->prio) {
	    i->next=w;
	    t->items=i;
	} else {
	    struct titem *max=NULL;
	    while(w) {
		if(i->prio>=w->prio) max=w;
		w=w->next;
	    }
	    i->next=max->next;
	    max->next=i;
	}
    } else {
	t->items=i;
	i->next=NULL;
    }
    t->nitems++;
    
    if(width>t->iwidth) t->iwidth=width;
     
    return i;
}

void mb_SetKeys(mb, title, item, keys)
struct menubar *mb;
char *title;
char *item;
char *keys;
{
    struct tmenu *t=FindMenu(mb, title);
    struct titem *i=t?FindItem(t, item):NULL;
   
    int kwidth=0;

    if(!i) return;
    
    if(keys) {
	kwidth=XTextWidth(mb->mbi->prefs->keysfont, keys, strlen(keys));
	if(kwidth>t->kwidth) t->kwidth=kwidth;
	i->keys=scache_Hold(keys);
    } else i->keys=NULL;

}
    

/* mb_AddSelection: add a new menu choice.  title is the name of the menu card it should appear on, tprio is the priority of this menu card or -1 if it shouldn't be modified.  item is the name of the menu itemm iprio is the priority of the item or -1 if it's priority shouldn't be modified, submenu indicates whether or not this item is another menu which will cascade off the top-level menu card when activated, data is to be supplied to the menubar's function when this item is choosen or a pointer to the menu card if this is a submenu. */
void mb_AddSelection(mb,title,tprio,item,iprio,submenu,data)
struct menubar *mb;
char *title, *item;
int tprio,iprio;
int submenu;
char *data;
{
     struct tmenu *t=FindMenu(mb,title);

     if(tprio>MAXPANEPRIORITY) tprio=MAXPANEPRIORITY;
     if(iprio>MAXITEMPRIORITY) iprio=MAXITEMPRIORITY;
     
     /* don't let clients mess with the more menu... */
     if(t==mb->moremenu) return;
     
     if(!t) {
	 t=CreateMenu(mb,title,tprio);
	 if(!t) return;

     } else {
	 if(tprio>0 && tprio!=t->prio && t->prio!=MOREMENUPRIORITY) {
	     t->prio=tprio;
	     mb->resort=True;
	 }
     }
     (void) AddItem(mb,t,item,iprio,submenu,data);
}

/* mb_SetItemStatus: indicate whether or not a menu item is active. */
void mb_SetItemStatus(mb,title,item,status)
struct menubar *mb;
char *title,*item;
int status;
{
    struct tmenu *m;
    struct titem *t;

    m=FindMenu(mb,title);
    if(m==NULL || m==mb->moremenu) return;

    t=FindItem(m,item);

    if(t==NULL) return;
    
    if(status) t->flags|=ACTIVEFLAG;
    else t->flags&=~ACTIVEFLAG;
}


/* mb_DeleteSelection: remove item from the card named title, returning the data associated with the item. */
void mb_DeleteSelection(mb,title,item)
struct menubar *mb;
char *title,*item;
{
    struct tmenu *t=FindMenu(mb,title);
    register struct titem *n,*n2;
    
    if(!t || !t->items || t==mb->moremenu) return;
    t->groupcount = 0;
    if(!strcmp(t->items->name,item)) {
	n=t->items->next;
	if(!ISSUBMENU(t->items) && t->items->data && mb->mbi->FreeItem) mb->mbi->FreeItem(t->items->data);
	
	scache_Free(t->items->name);
	free(t->items);
	t->items=n;
	t->nitems--;
    } else {
	n=t->items;
	while(n->next && strcmp(n->next->name,item)) n=n->next;
	if(n->next) {
	    n2=n->next;
	    n->next=n2->next;
	    if(!ISSUBMENU(n2) && n2->data && mb->mbi->FreeItem) mb->mbi->FreeItem(n2->data);

	    scache_Free(n2->name);
	    free(n2);
	    t->nitems--;
	}
    }
}

/* mcomp: decide which menu comes first. if a<b return -X else return 0 or +Y */
static int mcomp(a,b)
struct tmenu **a,**b;
{
    return (*a)->prio - (*b)->prio;
}

#define SetSeg(seg, x, y, xb, yb) do { (seg).x1=(x);(seg).y1=(y);(seg).x2=(xb);(seg).y2=(yb); } while(False);

/* SelectRegion: draw a highlighting box with the given position and size, with the shadow depth given by the user's preferences. */
static void SelectRegion(mb,win,gcs,x,y,w,h)
struct menubar *mb;
Window win;
struct gcs *gcs;
int x,y,w,h;
{
    static int oldsize=0;
    static XSegment *segs=NULL;
    int i;
    XGCValues gcv;

    if(mb->mbi->prefs->depth<=0) return;
    
    if(segs==NULL) {
	segs=(XSegment *)malloc(sizeof(XSegment)*2*mb->mbi->prefs->depth);
	if(segs==NULL) return;
	oldsize=mb->mbi->prefs->depth;
    } else if(oldsize!=mb->mbi->prefs->depth) {
	segs=(XSegment *)realloc(segs,sizeof(XSegment)*2*mb->mbi->prefs->depth);
	if(segs==NULL) return;
    }
    
    
    for(i=0;i<mb->mbi->prefs->depth;i++) {
	SetSeg(segs[i*2], x, y + i, x + w - 1 - i, y + i);
	SetSeg(segs[i*2 + 1], x + i, y, x + i, y + h - 1 - i);
    }

    
     XSetForeground(mb->mbi->dpy, gcs->select, gcs->topshadowPixel);

     gcv.stipple=gcs->topshadowPixmap;

     gcv.fill_style=(gcs->topshadowPixmap==None) ? FillSolid:FillStippled;
     XChangeGC(mb->mbi->dpy, gcs->select, GCFillStyle | ((gcs->topshadowPixmap!=None)?GCStipple:0), &gcv);
     
     XDrawSegments(mb->mbi->dpy,win, gcs->select,segs, mb->mbi->prefs->depth*2);
     
     XSetForeground(mb->mbi->dpy, gcs->select, gcs->bottomshadowPixel);
     gcv.stipple=gcs->bottomshadowPixmap;
     gcv.fill_style=(gcs->bottomshadowPixmap==None) ? FillSolid:FillStippled;
     XChangeGC(mb->mbi->dpy, gcs->select,GCFillStyle | ((gcs->bottomshadowPixmap!=None)? GCStipple:0), &gcv);
    
    for(i=0; i<mb->mbi->prefs->depth; i++) {
	
	SetSeg(segs[i*2],x + w - 1 - i, y + h - 1, x + w - 1 - i, y + 1 + i);
	SetSeg(segs[i*2 + 1], x + w - 1, y + h - 1 - i, x + 1 + i, y + h - 1 - i);
    }
    XDrawSegments(mb->mbi->dpy, win, gcs->select, segs, mb->mbi->prefs->depth*2);
}

/* UnSelectRegion: erase the highlighting box of the given size and location. */
static void UnSelectRegion(mb, win, gcs, x, y, w, h)
struct menubar *mb;
Window win;
struct gcs *gcs;
int x, y, w, h;
{
    static int oldsize=0;
    static XSegment *segs=NULL;
    int i;
    
    if(mb->mbi->prefs->depth<=0) return;
    if(segs==NULL) {
	segs=(XSegment *)malloc(sizeof(XSegment)*4*mb->mbi->prefs->depth);
	if(segs==NULL) return;
	oldsize=mb->mbi->prefs->depth;
    } else if(oldsize!=mb->mbi->prefs->depth) {
	
	segs=(XSegment *)realloc(segs,sizeof(XSegment)*4*mb->mbi->prefs->depth);
	if(segs==NULL) return;
	oldsize=mb->mbi->prefs->depth;
    }

    for(i=0;i<mb->mbi->prefs->depth;i++) {
	SetSeg(segs[i*4],x + w - 1 - i,y + h-1,x + w-1-i,y + 1 + i);
	SetSeg(segs[i*4 + 1],x + w-1,y + h-1-i,x + 1 + i,y + h-1-i);
	SetSeg(segs[i*4 + 2],x,y + i,x + w-1-i,y + i);
	SetSeg(segs[i*4 + 3],x + i,y,x + i,y + h-1-i);
    }
    XDrawSegments(mb->mbi->dpy, win, gcs->selecterase, segs, mb->mbi->prefs->depth*4);
}

/* SetTitleSelection: highlight or de-highlight the title of menu card menu. */
static void SetTitleSelection(mb, menu, onoff)
struct menubar *mb;
struct tmenu *menu;
int onoff;
{
    if(menu) {
	if(onoff)
	    SelectRegion(mb, mb->mbi->client, &mb->mbi->titlegcs, menu->mx - SHADOWWIDTH(mb) , SHADOWWIDTH(mb), menu->mw +  2*SHADOWWIDTH(mb), mb->mbi->prefs->menubarheight - SHADOWWIDTH(mb)*2);
	else
	    UnSelectRegion(mb, mb->mbi->client, &mb->mbi->titlegcs, menu->mx - SHADOWWIDTH(mb) , SHADOWWIDTH(mb), menu->mw +  2*SHADOWWIDTH(mb), mb->mbi->prefs->menubarheight - SHADOWWIDTH(mb)*2);
    }
    
    SelectRegion(mb, mb->mbi->client, &mb->mbi->titlegcs, 0, 0,mb->mbi->w, mb->mbi->prefs->menubarheight);

}

/* mb_RefitMenubar: recompute which menus fit on the menubar and construct the more menu appropriately. */
void mb_RefitMenubar(mb)
struct menubar *mb;
{
    int i, x= 2*SHADOWWIDTH(mb);
    int fullflag=False;
    if(mb->resort) {
	mb->resort=False;
	qsort(mb->menus, mb->nmenus,  sizeof(struct tmenu *), mcomp);
    }

    UpdateGeometry(mb);
    
    mb->lastvm=(-1);

    ClearMenu(mb, mb->moremenu);

    for(i=0; i<mb->nmenus; i++) {
	int fitflag=False;

	mb->menus[i]->mx=x;

	if((mb->menus[i]->nitems || i==0) && (x + mb->menus[i]->mw + HSPACE(mb, mb->menus[i]) + mb->morewidth <= mb->mbi->w - SHADOWWIDTH(mb) - 2 && !fullflag  || mb->menus[i]->prio==MOREMENUPRIORITY)) {
	    fitflag=True;
	    if(mb->menus[i]!=mb->moremenu) mb->lastvm=i;
	    mb->menus[i]->next=NULL;
	    x+=mb->menus[i]->mw + HSPACE(mb, mb->menus[i]);
	}

	if(!fitflag && (mb->menus[i]->nitems || i==0)) {
	    struct titem *ti;
	    fullflag=True;
	    mb->menus[i]->next=mb->overflow;
	    mb->overflow=mb->menus[i];
	    ti=AddItem(mb, mb->moremenu, mb->menus[i]->title, mb->menus[i]->prio, True, (char *)mb->menus[i]->title);
	    ti->flags|=ACTIVEFLAG;
	}
    }
}

/* mb_RedrawMenubar: redraw the menubar, clear indicates to what extent it should be redrawn if it is mb_FullRedraw the entire area is erased and redrawn, if it is mb_Update only the title area is erased, if it is mb_Exposed it is assumed the contents of the menubar haven't changed but just need to be redrawn */
void mb_RedrawMenubar(mb, clear)
register struct menubar *mb;
int clear;
{
    int i, y= mb->mbi->prefs->menubarheight - 2*SHADOWWIDTH(mb) - TITLEDESCENT(mb);
    XGCValues gcv;
    int lastgcmode=(-1);

    if(!mb->mbi->everdrawn) {
	clear=mb_FullRedraw;
	mb->mbi->everdrawn=True;
    }

    switch(clear) {
	case mb_Exposed:
	case mb_FullRedraw:
	    XFillRectangle(mb->mbi->dpy, mb->mbi->client, mb->mbi->titlegcs.selecterase, 0, 0, mb->mbi->w, mb->mbi->prefs->menubarheight);
	    break;
	case mb_Update:
	    XFillRectangle(mb->mbi->dpy, mb->mbi->client, mb->mbi->titlegcs.selecterase, SHADOWWIDTH(mb), SHADOWWIDTH(mb), mb->mbi->w-2*SHADOWWIDTH(mb), mb->mbi->prefs->menubarheight-2*SHADOWWIDTH(mb));
	    break;
    }
    
    for(i=0; i<mb->nmenus; i++) {
	struct tmenu *t=mb->menus[i];
	if(i>mb->lastvm) {
	    i=mb->nmenus-1;
	    t=mb->moremenu;
	}
	if(t->nitems>0) {
	    if(lastgcmode!=1) {
		gcv.fill_style=FillSolid;
		gcv.foreground=mb->mbi->titlegcs.whitePixel;
		XChangeGC(mb->mbi->dpy, mb->mbi->titlegcs.draw, GCFillStyle|GCForeground, &gcv);
		lastgcmode=1;
	    }
	    XDrawString(mb->mbi->dpy, mb->mbi->client, mb->mbi->titlegcs.draw, t->mx, y, t->title, t->titlelen);
	    
	} else if(mb->moremenu==t || i==0) {
	    if(lastgcmode!=0) {
		gcv.stipple=mb->mbi->titlegcs.grayStipple;

		gcv.fill_style= (gcv.stipple!=None) ? FillStippled : FillSolid;
		gcv.foreground=mb->mbi->titlegcs.grayPixel;

		XChangeGC(mb->mbi->dpy, mb->mbi->titlegcs.draw, GCFillStyle | GCForeground | ((gcv.stipple!=None)?GCStipple:0), &gcv);

		lastgcmode=0;
	    }
	    XDrawString(mb->mbi->dpy, mb->mbi->client, mb->mbi->titlegcs.draw, t->mx, y, t->title, t->titlelen);
	}

    }
    
    if(lastgcmode!=1) {
	gcv.fill_style=FillSolid;
	gcv.foreground=mb->mbi->titlegcs.whitePixel;
	XChangeGC(mb->mbi->dpy, mb->mbi->titlegcs.draw, GCFillStyle|GCForeground, &gcv);
    }

    SetTitleSelection(mb, mb->lastmenu, True);
}

/* DrawMenuItems: draw the menu items on the menu card t in the cards window. */
static void DrawMenuItems(mb,t)
struct menubar *mb;
struct tmenu *t;
{
    int lastprio=(-1);
    int lastgcmode=99;
    struct titem *it=t->items;
    int x= 2*SHADOWWIDTH(mb), y = SHADOWWIDTH(mb);
    int count=0;
    
    if(t->lookup)
	t->lookup=(struct titem **)realloc(t->lookup,t->nitems*sizeof(struct titem *));
    else
	t->lookup=(struct titem **)malloc(t->nitems*sizeof(struct titem *));
    
    XMapRaised(mb->mbi->dpy, t->window);
    XClearWindow(mb->mbi->dpy, t->window);
    
    SelectRegion(mb, t->window, &t->gcs, 0, 0, t->ww, t->wh);
    while(it) {
	XGCValues gcv;

	if(lastprio>=0 && lastprio<it->prio/10) {
	    y+=GROUPSPACING(mb, t);
	}
	
	lastprio=it->prio/10;
	
	it->y=(short)y;
	if(ITEMACTIVE(it)) {
	    if(lastgcmode!=1) {
		gcv.fill_style=FillSolid;
		gcv.foreground=t->gcs.whitePixel;
		XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFillStyle | GCForeground, &gcv);
		lastgcmode=1;
	    }
	} else {
	    if(lastgcmode!=0) {
		gcv.stipple=t->gcs.grayStipple;

		gcv.fill_style= (gcv.stipple!=None) ? FillStippled : FillSolid;
		gcv.foreground=t->gcs.grayPixel;

		XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFillStyle | GCForeground | ((gcv.stipple!=None)?GCStipple:0), &gcv);

		lastgcmode=0;
	    }
	}
	
	XDrawString(mb->mbi->dpy, t->window, t->gcs.draw, x, y + ITEMASCENT(mb, t) + SHADOWWIDTH(mb), it->name, strlen(it->name));
	
	if(it->keys) {
	    int len=strlen(it->keys);
	    
	    gcv.font=mb->mbi->prefs->keysfont->fid;
	    if(ITEMACTIVE(it)) gcv.foreground=mb->mbi->prefs->keysPixel;
	    else gcv.foreground=mb->mbi->prefs->grayitemPixel;
	    XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFont|GCForeground, &gcv);
	    
	    XDrawString(mb->mbi->dpy, t->window, t->gcs.draw, t->ww - t->kwidth - 2*SHADOWWIDTH(mb), y + ITEMASCENT(mb, t) + SHADOWWIDTH(mb), it->keys, len);

	    gcv.font=mb->mbi->prefs->itemfont->fid;
	    gcv.fill_style=FillSolid;
	    gcv.foreground=t->gcs.whitePixel;
	    XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFont|GCFillStyle | GCForeground, &gcv);
	    lastgcmode=1;
	}

	if(ISSUBMENU(it)) {
	    char tmp='W' + ((t->vert>0)?0:1)*2 + ((t->horiz>0)?0:1);
	    gcv.font=mb->mbi->prefs->iconfont->fid;
	    XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFont, &gcv);
	    XDrawString(mb->mbi->dpy,t->window, t->gcs.draw, x + t->iwidth - mb->mbi->prefs->iconfont->max_bounds.rbearing - 4 , y + ITEMASCENT(mb, t) + SHADOWWIDTH(mb), &tmp,1);

	    gcv.font=mb->mbi->prefs->itemfont->fid;

	    XChangeGC(mb->mbi->dpy, t->gcs.draw, GCFont, &gcv);
	}

	y+=ITEMHEIGHT(mb, t) + VSPACE(mb, t);
	
	t->lookup[count++]=it;
	it=it->next;
    }
}

static int ComputeItemPosition(mb, lm, inum)
struct menubar *mb;
struct tmenu *lm;
int inum;
{
    int i,c;
    
    if(!lm->groupcount) CountGroups(lm);
    for(i=1,c=0;!(i&1<<(inum+1));i<<=1) if(lm->groupmask&i) c++;
    return (ITEMHEIGHT(mb,lm) + VSPACE(mb, lm))*inum + (c-1)*GROUPSPACING(mb, lm);
}


#define POS_UP(mb,m1,m2,ya, ipos) ((ya) + 2*SHADOWWIDTH(mb) + ipos  - VSPACE(mb, (m1))/2 - CARDHEIGHT(mb, m2))

#define POS_DOWN(mb, m1, m2, ya, ipos) ((ya) + 2*SHADOWWIDTH(mb) + ipos)

/* ComputeMenuPositioning: choose where to position the menu m relative to the menubar, the primary considerations are: displaying as much of the menu as possible, and not covering the menubar. */
static void ComputeMenuPositioning(mb,m,x,y,w,h)
struct menubar *mb;
struct tmenu *m;
int *x,*y,*w,*h;
{
    int displayHeight=DisplayHeight(mb->mbi->dpy, DefaultScreen(mb->mbi->dpy));
    int displayWidth=DisplayWidth(mb->mbi->dpy, DefaultScreen(mb->mbi->dpy));
    
    int c;
    int lossageup, lossagedown;
    int extrawidth;
    struct titem *it;
    
    *h= CARDHEIGHT(mb, m);

    extrawidth=lossageup=lossagedown=0;
    for(c=0,it=m->items; it; it=it->next,c++) {
	if(ISSUBMENU(it)) {
	    struct tmenu *t=(struct tmenu *)it->data;
	    int ipos=ComputeItemPosition(mb, m, c);
	    int toppos=POS_UP(mb, m, t, mb->mbi->y + SHADOWWIDTH(mb) - *h, ipos);
	    int botpos=POS_DOWN(mb, m, t, mb->mbi->y + mb->mbi->prefs->menubarheight - SHADOWWIDTH(mb), ipos) + CARDHEIGHT(mb, t);
	    if(toppos<0) lossageup-=toppos;
	    if(botpos>displayHeight) lossagedown+=botpos-displayHeight;
	    if(t->iwidth + t->kwidth + 4*SHADOWWIDTH(mb) + (t->kwidth?ITEMHSPACE(mb):0) > extrawidth) extrawidth=t->iwidth + t->kwidth +  4*SHADOWWIDTH(mb) + (t->kwidth?ITEMHSPACE(mb):0);
	}
    }
    
    *x=mb->mbi->x + m->mx - 2*SHADOWWIDTH(mb);
    if(*x<0) *x=0;

    *w=m->iwidth + m->kwidth + 4*SHADOWWIDTH(mb) + (m->kwidth?ITEMHSPACE(mb):0);

    if(*x + *w>displayWidth) {

	*x=displayWidth - *w;
	m->horiz=(-1);

    } else {

	if(*x + *w + extrawidth>displayWidth) m->horiz=(-1);
	else m->horiz=1;

    }
    
    if(lossageup == 0 && lossagedown == 0) {
	int downpos=mb->mbi->y + mb->mbi->prefs->menubarheight - SHADOWWIDTH(mb);
	int uppos=mb->mbi->y + SHADOWWIDTH(mb) - CARDHEIGHT(mb, m);
	
	if(downpos + CARDHEIGHT(mb, m) > displayHeight) lossagedown+=downpos + CARDHEIGHT(mb, m) - displayHeight;
	if(uppos<0) lossageup-=uppos;
    }

    if(lossageup<lossagedown) {
	m->y=(*y)=mb->mbi->y + SHADOWWIDTH(mb) - CARDHEIGHT(mb, m);
	m->vert=(-1);
    } else {
	 m->y=(*y)=mb->mbi->y + mb->mbi->prefs->menubarheight - SHADOWWIDTH(mb);
	 m->vert=1;
    }
}

/* LocateMenu: return the index of the menu pointed at by ex (and ey) */
static int LocateMenu(mb, ex, ey)
struct menubar *mb;
int ex, ey;
{
    int i;
    struct tmenu *t;
    
    for(i=0;i<mb->nmenus;i++) {
	t=mb->menus[i];
	if(i>mb->lastvm) {
	    i=mb->nmenus-1;
	    t=mb->moremenu;
	}
	if(ex>t->mx && ex<t->mx+t->mw && t->nitems)
	    return i;
    }
    return -1;
}

/* BringUpMenu: bring up the top level menu at index menu in the set of menus on the menubar. */
static void BringUpMenu(mb,menu)
struct menubar *mb;
int menu;
{
    int x,y;
    int w,h;

    
    if(menu<0 || menu>=mb->nmenus) return;
    
    if(mb->lastmenu==mb->menus[menu]) return;
    
    SetTitleSelection(mb, mb->lastmenu, False);
    
    if(mb->lastmenu && mb->lastmenu->next!=NULL) XUnmapWindow(mb->mbi->dpy, mb->lastmenu->next->window);

    mb->lastmenuindex=menu;
    mb->lastmenu=mb->menus[menu];
    mb->lastmenu->lastitem=(-1);
    mb->lastmenu->gcs=mb->mbi->cardgcs;
    mb->lastmenu->window=mb->mbi->menuw;
    mb->lastmenu->next=NULL;
    
    SetTitleSelection(mb, mb->lastmenu, True);
    
    ComputeMenuPositioning(mb, mb->lastmenu, &x, &y, &w, &h);
    
    mb->lastmenu->ww=w;
    mb->lastmenu->wh=h;

    XMoveResizeWindow(mb->mbi->dpy, mb->lastmenu->window, x, y, mb->lastmenu->ww, mb->lastmenu->wh);
    
    mb->lastmenu->x=x - mb->mbi->x;
    mb->lastmenu->y=y;
    
    DrawMenuItems(mb,mb->lastmenu);
}

/* eventp: choose the events which will be processed by the menubar event loop */
#ifdef __STDC__
static Bool eventp(Display *dpy, XEvent *event, char *args)
#else
static Bool eventp(dpy, event, args)
Display *dpy;
XEvent *event;
char *args;
#endif
{
    struct menubar *mb=(struct menubar *)args;
    struct tmenu *menu=mb->lastmenu;
    
    while(menu && (menu->window!=((XAnyEvent *)event)->window)) menu=menu->next;
    
    mb->lasteventin=menu;
    
    switch (event->type) {
	case Expose:
	    return True;
	    
	case ButtonPress:
	case ButtonRelease:
	    return True;
	    
	case MotionNotify:
	    return  (((XMotionEvent *) event)->window == mb->mbi->client || menu); /**/
	    
	case LeaveNotify:
	    return  (Bool)menu || ((XAnyEvent *)event)->window==mb->mbi->client;

	case KeyPress:
	    return True;

	default:
	    return False;
    }
}

/* BringUpSubMenu: display the submenu associated with item inum on menu card lm */
static void BringUpSubMenu(mb, lm, inum)
struct menubar *mb;
struct tmenu *lm;
int inum;
{
    struct titem *item=lm->lookup[inum];
    struct tmenu *submenu=(struct tmenu *)item->data;
   
    submenu->lastitem=(-1);
    submenu->gcs=mb->mbi->cmcgcs;
    submenu->window=mb->mbi->cascadew;
    submenu->next=NULL;
    submenu->w=submenu->iwidth + submenu->kwidth + (submenu->kwidth?ITEMHSPACE(mb):0) + 2*SHADOWWIDTH(mb);
    submenu->h=FONTHEIGHT(mb->mbi->prefs->itemfont);

    submenu->ww=submenu->w + 2*SHADOWWIDTH(mb);
    submenu->wh=CARDHEIGHT(mb,submenu);
    
   if(lm->vert==1) submenu->y=lm->y + 2*SHADOWWIDTH(mb) + lm->lookup[inum]->y;
   else submenu->y=lm->y + 2*SHADOWWIDTH(mb) + lm->lookup[inum]->y  - VSPACE(mb, lm)/2 - CARDHEIGHT(mb, submenu);

   if(submenu->y<0) submenu->y=0;
   if(submenu->y+submenu->wh > DisplayHeight(mb->mbi->dpy, DefaultScreen(mb->mbi->dpy))) {
       submenu->y+= DisplayHeight(mb->mbi->dpy, DefaultScreen(mb->mbi->dpy)) - submenu->y - submenu->wh;
   }
       
    if(lm->horiz==1) submenu->x=mb->mbi->x + lm->x + lm->ww - SHADOWWIDTH(mb);
    else submenu->x=mb->mbi->x + lm->x - submenu->w - SHADOWWIDTH(mb);

    
    XMoveResizeWindow(mb->mbi->dpy, submenu->window, submenu->x, submenu->y, submenu->ww, submenu->wh);
    
    if(!lm->next) XMapRaised(mb->mbi->dpy, submenu->window);
    SelectRegion(mb, lm->window, &lm->gcs, 0, 0, lm->ww, lm->wh);
    
    lm->next=submenu;
    
    DrawMenuItems(mb, submenu);
    return;
}

/* SetItemSelection: highlight or de-highlight an item. */
static void SetItemSelection(mb, lm, item, onoff)
struct menubar *mb;
struct tmenu *lm;
int item;
int onoff;
{
    if(onoff) { 
	SelectRegion(mb, lm->window, &lm->gcs, SHADOWWIDTH(mb), ((int)lm->lookup[item]->y) , 2*SHADOWWIDTH(mb) + lm->iwidth + lm->kwidth + (lm->kwidth?ITEMHSPACE(mb):0), ITEMHEIGHT(mb, lm) + VSPACE(mb, lm));
	
    } else {

	UnSelectRegion(mb, lm->window, &lm->gcs, SHADOWWIDTH(mb), ((int)lm->lookup[item]->y), 2*SHADOWWIDTH(mb) + lm->kwidth + (lm->kwidth?ITEMHSPACE(mb):0) + lm->iwidth, ITEMHEIGHT(mb, lm) + VSPACE(mb, lm));
    }
}

 /* if we're bringing up a new menu wait to see if we get more events first
     return values:
     0: timed out, go ahead with MotionNotify processing
     1: found another MotionNotify in the queue put it in the event passed in
     2:	data ready on the X display's file descriptor */
static int WaitTillReady(mb, event)
struct menubar *mb;
XEvent *event;
{
    long nfds;
#ifdef FD_SET
    fd_set fdmask;
#else
    long fdmask=0;
#endif
    if(XCheckMaskEvent(mb->mbi->dpy, MENUEVENTS, event))
	return 1;
#ifdef FD_SET
    FD_ZERO(&fdmask);
    FD_SET(ConnectionNumber(mb->mbi->dpy), &fdmask);
#else
    fdmask=1<<ConnectionNumber(mb->mbi->dpy);
#endif
    nfds=select(ConnectionNumber(mb->mbi->dpy)+1, &fdmask, 0, 0, &mb->mbi->prefs->activatetime);
    if(nfds>0) return 2;
    else return 0;
}

/* SelectItem: highlight the itemth item on the menu card lm, also brings up any submenu and highlight's it's default item (usually the first) rot is used to decide which direction to go if the specified item is not active, -1 is up 1 is down. */
static void SelectItem(mb, lm, item, rot)
struct menubar *mb;
struct tmenu *lm;
int item;
int rot;
{
    int i=item;
    if(!lm->lookup) return;
    do {
	if(ITEMACTIVE(lm->lookup[i])) break;
	i+=rot;
	if(i<0) i=lm->nitems-1;
	else if(i>=lm->nitems) i=0;
    } while (i!=item);
    if(ITEMACTIVE(lm->lookup[i])) {
	if(lm->lastitem!=i) {
	    if(lm->lastitem>=0) SetItemSelection(mb, lm, lm->lastitem, False);
	    SetItemSelection(mb, lm, i, True);
	    if(ISSUBMENU(lm->lookup[i])) {
		BringUpSubMenu(mb, lm, i);
		if(lm->next) SelectItem(mb, lm->next, 0, rot);
	    }
	    lm->lastitem=i;
	}
    }
}

/* MoveDown: Select the next item down from the item currently selected on
  menucard lm in menubar mb, if the new item is a submenu activate it and select it's default item. Return 1 if the search for the next item should start over from the first item else return 0 */
static int MoveDown(mb, lm)
struct menubar *mb;
struct tmenu *lm;
{
    int c;
    c=lm->lastitem;
    if(c>=0) {
	do {
	    c++;
	    if(c>=lm->nitems) return 1;
	} while(!ITEMACTIVE(lm->lookup[c]) && c!=lm->lastitem);
    } else c=0;
    if(c!=lm->lastitem) SelectItem(mb, lm, c, 1);
    return 0;
}

/* MoveUp: Select the next item up from the item currently selected on
  menucard lm in menubar mb, if the new item is a submenu activate it and select it's default item. Return 1 if the search for the next item should start over from the first item else return 0 */
static int MoveUp(mb, lm)
struct menubar *mb;
struct tmenu *lm;
{
    int c;
    c=lm->lastitem;
    if(c>=0) {
	do {
	    c--;
	    if(c<0) return 1;
	} while(!ITEMACTIVE(lm->lookup[c]) && c!=lm->lastitem);
    } else c=0;
    if(c!=lm->lastitem) SelectItem(mb, lm, c, -1);
    return 0;
}

static int GetItemAt(mb, lm, y)
struct menubar *mb;
struct tmenu *lm;
int y;
{
    int item=(y - SHADOWWIDTH(mb))/(ITEMHEIGHT(mb, lm) + VSPACE(mb, lm));
    
    if(!lm->lookup || item<0) return -1;

    if(item>=lm->nitems) item=lm->nitems-1;
    
    while(item>=0) {
	if(y>=((int)lm->lookup[item]->y) && y<=((int)lm->lookup[item]->y)+ITEMHEIGHT(mb, lm)+VSPACE(mb, lm)) return item;
	else item--;
    }
    return item;
}

/* HandleInMenu: handle any changes needed due to move movement */
static int HandleInMenu(mb, x, y, didwait)
struct menubar *mb;
int x,y;
int didwait;
{
    struct tmenu *lm=mb->lasteventin;
    int item=GetItemAt(mb, lm, y);

    if(lm->lastitem==item) return 0;
    
    if(lm->lastitem>=0 && !lm->next) {
	SetItemSelection(mb, lm, lm->lastitem, False);
	lm->lastitem=(-1);
    }

    if(item>=0) {
	if(lm->lastitem>=0 && lm->next) {
	    SetItemSelection(mb, lm, lm->lastitem, False);
	    lm->lastitem=(-1);
	}
	
	if(lm->lookup[item] && ISSUBMENU(lm->lookup[item]) && ((struct tmenu *)lm->lookup[item]->data)->nitems>0) {

	    if(!didwait) return 1;
	    BringUpSubMenu(mb, lm, item);
	    
	} else if(lm->next) {
	    XUnmapWindow(mb->mbi->dpy, lm->next->window);
	    lm->next=NULL;
	}
	
	if(ITEMACTIVE(lm->lookup[item])) SetItemSelection(mb, lm, item, True);

	lm->lastitem=item;
    }
    return 0;
}

/* This macro should be true iff y and h indicate an intersection with the area of the menubar titles themselves instead of just the border around them. */
#define TITLESINTERSECT(mb,y,h) (((y)>2*SHADOWWIDTH(mb) && (y)<(mb)->mbi->prefs->menubarheight-2*SHADOWWIDTH(mb)) || ((y)<SHADOWWIDTH(mb) && (y)+(h)>2*SHADOWWIDTH(mb) && (y)+(h)<(mb)->mbi->prefs->menubarheight-2*SHADOWWIDTH(mb)))
/* */
#define GRABPOINTER 1 /* */

static Bool HandleButtonOrMotion(mb, window, event, lx, ly, x, y)
struct menubar *mb;
Window window;
XEvent *event;
int lx, ly, x, y;
{
    Bool needevent=True;
    
    if(mb->lasteventin) {
	if(HandleInMenu(mb, lx, ly, False)) {
	    switch(WaitTillReady(mb, event)) {
		case 0:
		    HandleInMenu(mb, lx, ly, True);
		    break;
		case 1:
		    needevent=False;
		case 2:
		    break;
	    }
	}
    } else {
	if(window==mb->mbi->client)

	    switch(WaitTillReady(mb, event)) {
		case 0:
		    if(y<=mb->mbi->prefs->menubarheight && y>=0)
			BringUpMenu(mb, LocateMenu(mb, lx, ly));
		    break;
		case 1:
		    needevent=False;
		    break;
		case 2:
		    break;
	    }
    }
    return needevent;
}

static void DoMenuLoop(mb, track)
struct menubar *mb;
Bool track;
{
    struct tmenu *itm;
    Bool exitmenus=False;
    Bool trackingmotion=track;
    Bool needevent=True;
    XEvent event;
#ifdef GRABPOINTER
    XUngrabPointer(mb->mbi->dpy, CurrentTime);
    
    if (XGrabPointer(mb->mbi->dpy, mb->mbi->client, True, ButtonMotionMask | ButtonPressMask | ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None, CurrentTime) != GrabSuccess) return;
    
    if(XGrabKeyboard(mb->mbi->dpy, mb->mbi->client, True, GrabModeAsync, GrabModeAsync,CurrentTime) != GrabSuccess) {

	XUngrabPointer(mb->mbi->dpy,CurrentTime);
	return;
    }
#endif
    do {
	int x,y;
	XMotionEvent *me=(XMotionEvent *)&event;
	XExposeEvent *ee=(XExposeEvent *)&event;
	XButtonEvent *be=(XButtonEvent *)&event;
	XKeyEvent *ke=(XKeyEvent *)&event;
	if(needevent) XIfEvent(mb->mbi->dpy, &event, eventp, (char *)mb);
	else needevent=True;

	switch(event.type) {
		/* This is really blecherous:
		 plan is that WaitTillReady will allow a delay before actually
		 processing MotionNotifys when they would bring up a new menu
		 card and also will only process the LAST MotionNotify in the
		 input queue */
	    case ButtonPress:
		x=be->x_root-mb->mbi->x;
		y=be->y_root-mb->mbi->y;
		trackingmotion=True;
		if(be->window==mb->mbi->client)
		    if(be->y>=mb->mbi->h || be->y<0 || be->x<0 || be->x>=mb->mbi->w) {
			if(mb->lasteventin) mb->lasteventin->lastitem=(-1);
			exitmenus=True;
			continue;
		    } else {
			if(mb->lastmenu && mb->lastmenu->lastitem>=0) {
			    struct tmenu *itm=mb->lastmenu;
			    while(itm) {
				if(itm->lastitem>=0) {
				    SetItemSelection(mb, itm, itm->lastitem, False);
				    if(itm->next) XUnmapWindow(mb->mbi->dpy, itm->next->window);
				    itm->lastitem=(-1);
				}
				itm=itm->next;
			    }
			}
		    }
	    case MotionNotify:
		if(!trackingmotion) continue;
		/* this looks silly but we shouldn't count on the ButtonEvent and MotionEvent structures remaining compatible... */
		if(event.type==MotionNotify) {
		    x=me->x_root-mb->mbi->x;
		    y=me->y_root-mb->mbi->y;
		    needevent=HandleButtonOrMotion(mb, me->window, &event, me->x, me->y, x, y);
		} else {
		    needevent=HandleButtonOrMotion(mb, be->window, &event, be->x, be->y, x, y);
		}
		
		break;
	
	   case LeaveNotify:
		if(mb->lasteventin && !mb->lasteventin->next && mb->lasteventin->lastitem>=0 && trackingmotion) {
		    SetItemSelection(mb, mb->lasteventin, mb->lasteventin->lastitem, False);
		    mb->lasteventin->lastitem=(-1);
		}
		break;
		
	    case Expose:
		if(ee->window == mb->mbi->client) mb->mbi->HandleExpose(ee,mb->mbi->ExposeData);
		else {
		    itm=mb->lastmenu;
		    while(itm) {
			if(itm->window==ee->window) {
			    DrawMenuItems(mb, itm);
			    break;
			}
			itm=itm->next;
		    }
		}
		break;
		
	    case ButtonRelease:
		trackingmotion=False;
		if(be->window==mb->mbi->client && be->y<mb->mbi->prefs->menubarheight && be->y>=0 && be->x>=0 && be->x<mb->mbi->w && be->button==mb->mbi->prefs->holdbutton) {
		    mb->lasteventin=mb->lastmenu;
		    if(mb->lastmenu) SelectItem(mb, mb->lastmenu, 0, 1);
		} else {
		    if(be->window==mb->mbi->menuw && mb->lastmenu && mb->lastmenu->lastitem>=0 && mb->lastmenu->lookup && ISSUBMENU(mb->lastmenu->lookup[mb->lastmenu->lastitem]) && be->button==mb->mbi->prefs->holdbutton) {
			if(mb->lastmenu->next) SelectItem(mb, mb->lastmenu->next, 0, 1);
		    } else exitmenus=True;
		}
		break;
		
	    case KeyPress: {
		int c, j;
		struct tmenu *lm=mb->lastmenu;
		if(!lm) continue;
		while(lm->next) lm=lm->next;
		switch(XKeycodeToKeysym(mb->mbi->dpy, ke->keycode, 0)) {
		    case XK_Escape:
			exitmenus=True;
			mb->lasteventin=NULL;
			continue;
		    case XK_Up:
			if(MoveUp(mb, lm)) SelectItem(mb, lm, lm->nitems-1, -1);
			break;
		    case XK_Down:
			if(MoveDown(mb, lm)) SelectItem(mb, lm, 0, 1);
			break;
		    case XK_Right:
			/* if the current menu is the more menu step down, if the user goes off the bottom of the more menu go back to the first menu card
			    if the current menu is not the more menu activate the menucard to the right of the current card */
			if(mb->lastmenu!=mb->moremenu || MoveDown(mb, mb->lastmenu)) {
				c=mb->lastmenuindex;
				if(c<0) c=0;
				j=c;
				do {
				    c++;
				    if(c>=mb->nmenus) c=0;
				    if(c>mb->lastvm) c=mb->nmenus-1;
				} while(mb->menus[c]->nitems==0 && c!=j);
				
				BringUpMenu(mb, c);
				if(mb->lastmenu) SelectItem(mb, mb->lastmenu, 0, 1);
			    }
				
			break;
		    case XK_Left:
			/* if the current menu is the more menu step back up, if the user goes off the top go to the menu card just before the more menu
			    if the current menu is not the more menu activate the menucard to the left of the current card */
			if(mb->lastmenu==mb->moremenu) {
			    if(MoveUp(mb, mb->lastmenu)) {
				c=mb->lastvm;
				if(c<0) {
				    SelectItem(mb, mb->lastmenu, mb->lastmenu->nitems-1, -1);
				} else {
				    BringUpMenu(mb, c);
				    if(mb->lastmenu) SelectItem(mb, mb->lastmenu, 0, 1);
				}
			    }
			} else {
			    c=mb->lastmenuindex;
			    if(c<0) c=mb->nmenus-1;
			    j=c;
			    do {
				c--;
				if(c>mb->lastvm) c=mb->lastvm;
				if(c<0) c=mb->nmenus-1;
			    } while(mb->menus[c]->nitems==0 && c!=j);
			    BringUpMenu(mb, c);
			    if(mb->lastmenu) if(mb->lastmenu==mb->moremenu) SelectItem(mb, mb->lastmenu, mb->lastmenu->nitems-1, -1);
			    else SelectItem(mb, mb->lastmenu, 0, 1);
			}
			break;
		    case XK_Return:
			mb->lasteventin=lm;
			exitmenus=True;
			break;
		    default:
			break;
		}
		}
		break;

	    default:
		;
	}
    } while(!exitmenus);
    
    SetTitleSelection(mb, mb->lastmenu, False);
#ifdef GRABPOINTER
    XUngrabPointer(mb->mbi->dpy, CurrentTime);
    XUngrabKeyboard(mb->mbi->dpy, CurrentTime);
#endif 
    for(itm=mb->lastmenu; itm; itm=itm->next) XUnmapWindow(mb->mbi->dpy, itm->window);
    
    XFlush(mb->mbi->dpy);
    if(mb->lasteventin && mb->lasteventin->lastitem>=0 && mb->lasteventin->lookup && !ISSUBMENU(mb->lasteventin->lookup[mb->lasteventin->lastitem]) && ITEMACTIVE(mb->lasteventin->lookup[mb->lasteventin->lastitem]) && mb->MenuFunction) {
	
	mb->MenuFunction(mb, mb->lasteventin->lookup[mb->lasteventin->lastitem]->data, mb->data);
	
    }
    
    mb->lastmenu=NULL;
    mb->lastmenuindex=(-1);
}

/* mb_HandleConfigure: handle being moved or resized, since the menubar needs to know where it is on the root window */
void mb_HandleConfigure(mbi, mb, width, height)
struct mbinit *mbi;
struct menubar *mb;
long width, height;
{
    Window dumb;
    mbi->h=height;
    XTranslateCoordinates(mbi->dpy, mbi->client, RootWindow(mbi->dpy, DefaultScreen(mbi->dpy)), 0, 0, &(mbi->x), &(mbi->y), &dumb);
    if(width!=mbi->w) {
	mbi->w=width;
	if(mb)  mb_RefitMenubar(mb);
    }
}


/* mb_Activate: start processing menubar events */
void mb_Activate(mb, x, y)
struct menubar *mb;
long x,y;
{
    BringUpMenu(mb, LocateMenu(mb, x, y));
    DoMenuLoop(mb, True);
}

/* mb_KeyboardActivate: activate menubar, bringing up the first menucard with it's first item selected, and start processing events. */
void mb_KeyboardActivate(mb)
struct menubar *mb;
{
    if(mb->lastvm>=0) BringUpMenu(mb, 0);
    else BringUpMenu(mb, mb->nmenus-1); /* bring up the more menu */
    if(mb->lastmenu) SelectItem(mb, mb->lastmenu, 0, 1);
    DoMenuLoop(mb, False);
}

/* SetGCs: Actually create the gc's for a gcs.. */
static void SetGCs(mbi,w,gcs,gcv,gcmask)
struct mbinit *mbi;
Window w;
struct gcs *gcs;
XGCValues *gcv;
unsigned long gcmask;
{
    unsigned long t;
    gcv->function=GXcopy;
    gcs->draw=XCreateGC(mbi->dpy, w,  gcmask, gcv);

    if(gcs->draw==None) return;

    gcs->select=XCreateGC(mbi->dpy, w,  gcmask , gcv);

    if(gcs->select==None) {
	XFreeGC(mbi->dpy,gcs->draw);
	gcs->draw=None;
	return;
    }
    
    t=gcv->foreground;
    gcv->foreground=gcv->background;
    gcv->background=t;

   
    gcs->selecterase=XCreateGC(mbi->dpy, w,  gcmask , gcv);
    if(gcs->selecterase==None) {
	XFreeGC(mbi->dpy, gcs->draw);
	XFreeGC(mbi->dpy, gcs->select);
	gcs->draw=None;
	gcs->select=None;
    }
    t=gcv->foreground;
    gcv->foreground=gcv->background;
    gcv->background=t;
   
    return;
}

/* FreeGCs: free the gc's in a gcs */
static void FreeGCs(mbi,gcs)
struct mbinit *mbi;
struct gcs *gcs;
{
    if(gcs->draw) {
	XFreeGC(mbi->dpy, gcs->draw);
	gcs->draw=NULL;
    }
    if(gcs->select) {
	XFreeGC(mbi->dpy, gcs->select);
	gcs->select=NULL;
    }
    if(gcs->selecterase) {
	XFreeGC(mbi->dpy, gcs->selecterase);
	gcs->selecterase=NULL;
    }    
}

/* ReallyGetFont: try to get the font called fontname if it isn't
  available try to get the "Fixed" font. */
static XFontStruct *ReallyGetFont(dpy,fontname)
Display *dpy;
char *fontname;
{
    XFontStruct *f=XLoadQueryFont(dpy,fontname);
    if(f==NULL) {
	f=XLoadQueryFont(dpy,"Fixed");
	if(f==NULL) {
	    fprintf(stderr, "Couldn't even get font \"Fixed\" for the menubar!\n");
	}
    }
    return f;
}

/* ReallyGetColor: get the color named color, if color is NULL or cannot be parsed by parse color use the color pointed to as a default and return the pixel value of the resulting color.  If color is non-null and can be parsed it's rgb values are filled into the color pointed to by desired and it's pixel value is returned. */
static unsigned long ReallyGetColor(dpy,color,desired)
Display *dpy;
char *color;
XColor *desired;
{
    XColor maybedesired;
    long status;
    
    if(color==NULL || XParseColor(dpy ,DefaultColormap(dpy, DefaultScreen(dpy)), color, &maybedesired)<=0) {
	desired->flags = DoRed | DoGreen | DoBlue;
    } else {
	*desired=maybedesired;
    }
    
    status = XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), desired);
    
    if(!status) return BlackPixel(dpy, DefaultScreen(dpy));
    else return desired->pixel;
}

#define FRACTION(x,y) ((((long)x)*((long)y))/100)

/* ISWHITE: true iff the color b is equivalent to white, p is a struct prefs_s for appropriate display */
#define ISWHITE(p,b) ((b).red==(p)->whiteColor.red && (b).blue==(p)->whiteColor.blue && (b).green==(p)->whiteColor.green)

/* ISBLACK: true iff the color b is equivalent to black, p is a struct prefs_s for appropriate display */
#define ISBLACK(p,b) ((b).red==(p)->blackColor.red && (b).blue==(p)->blackColor.blue && (b).green==(p)->blackColor.green)

/* MAXCOLOR: the brightest any of the RGB components can be. */
#define MAXCOLOR 65535

/* SetBottomShadow: try to get a good color for the bottom shadow..., it will be placed in the color pointed to by bcolor. */
static void SetBottomShadow(p, bName, bcolor, background)
struct prefs_s *p;
char *bName;
XColor *bcolor,*background;
{
    if(ISWHITE(p, *background)) {
	 *bcolor=p->blackColor;
     } else if(ISBLACK(p, *background)) {
	 *bcolor=p->whiteColor;
     } else {
	 if(p->newshadows) {
	     shadows_ComputeColor(background->red, background->green, background->blue, &bcolor->red,  &bcolor->green, &bcolor->blue, shadows_BOTTOMSHADOW);
	 } else {
	     bcolor->red=FRACTION(background->red,55);
	     bcolor->blue=FRACTION(background->blue,55);
	     bcolor->green=FRACTION(background->green,55);
	 }
     }
    (void)ReallyGetColor(p->dpy, bName, bcolor);
}


/* SetGray: try to get a good gray color.  If grayName is non-null it will be used if possible.  Otherwise a color hopefully between the background and foreground in brightness and a hue similar to the foreground will be used.  On a black and white display a grayish pixmap is used with the default foreground and background colors */
static void SetGray(p, foreground,  background, grayName, graycolor, graypixmap)
struct prefs_s *p;
char *grayName;
XColor *foreground, *background, *graycolor;
Pixmap *graypixmap;
{
    if(p->ColorDisplay) {
	int r,g,b;
	r=FRACTION(foreground->red, 100+p->grayPercentage);
	g=FRACTION(foreground->green, 100+p->grayPercentage);
	b=FRACTION(foreground->blue, 100+p->grayPercentage);
	if(r>MAXCOLOR || g>MAXCOLOR || b>MAXCOLOR){
	    r=FRACTION(foreground->red, 100-p->grayPercentage);
	    g=FRACTION(foreground->green, 100-p->grayPercentage);
	    b=FRACTION(foreground->blue, 100-p->grayPercentage);
	}
	graycolor->red=r;
	graycolor->green=g;
	graycolor->blue=b;
	(void)ReallyGetColor(p->dpy,  grayName, graycolor);
	if(ISWHITE(p, *graycolor) || ISBLACK(p, *graycolor)) *graypixmap=p->grayPixmap;
	else *graypixmap=None;
    } else {
	*graycolor=(*foreground);
	*graypixmap=p->grayPixmap;
    }
}

static void SetTopShadow(p, foreground,  background, topcolorName, topcolor, toppixmap)
struct prefs_s *p;
char *topcolorName;
XColor *foreground, *background, *topcolor;
Pixmap *toppixmap;
{
    if(p->ColorDisplay) {
	if(!p->newshadows) {
	    int r,g,b,c;
	    c=((long)3*65535-background->red-background->blue-background->green)/3;
	    r=background->red+FRACTION(c, p->topshadowPercentage);
	    g=background->green+FRACTION(c, p->topshadowPercentage);
	    b=background->blue+FRACTION(c, p->topshadowPercentage);
	    if(r>MAXCOLOR) r=MAXCOLOR;
	    if(g>MAXCOLOR) g=MAXCOLOR;
	    if(b>MAXCOLOR) b=MAXCOLOR;
	    topcolor->red=r;
	    topcolor->green=g;
	    topcolor->blue=b;
	} else {
	    shadows_ComputeColor(background->red, background->green, background->blue,  &topcolor->red, &topcolor->green, &topcolor->blue,  shadows_TOPSHADOW);
	}
	(void)ReallyGetColor(p->dpy,  topcolorName, topcolor);
	if(ISWHITE(p, *topcolor) || ISBLACK(p, *topcolor)) {
	    *toppixmap=p->grayPixmap;
	} else *toppixmap=None;
	if(topcolor->pixel==background->pixel) *topcolor=(*foreground);
    } else {
	*topcolor=(*foreground);
	*toppixmap=p->grayPixmap;
    }
}

/* SetPixmap: get a pixmap from file name, or if name is None set the pixmap to None. */
static void SetPixmap(p, name, pixmap)
struct prefs_s *p;
char *name;
Pixmap *pixmap;
{
    unsigned int width_ret, height_ret;
    int x_hot_ret, y_hot_ret;
    if(name) {
	if(strcmp(name, "None")) {
	    if(XReadBitmapFile(p->dpy, DefaultRootWindow(p->dpy), name, &width_ret, &height_ret, pixmap, &x_hot_ret, &y_hot_ret)!=0) {
		fprintf(stderr, "menubar: failed to read top shadow pixmap\n");
	    }
	} else *pixmap=None;
    }
}

/* BuildGraphicContext: try to make reasonable choices for everything given what a user has specified or not specified. */
static void BuildGraphicContext(p, foregroundColor, backgroundColor, grayColor, graypixmapName, topcolorName, toppixmapName, botcolorName, botpixmapName, realForeground, realBackground, realGray, grayPixmap, topShadow, bottomShadow, topshadowPixmap, bottomshadowPixmap)
struct prefs_s *p;
char *foregroundColor, *backgroundColor;
char *grayColor;
char *graypixmapName, *toppixmapName, *botpixmapName;
char *topcolorName, *botcolorName;
XColor *realForeground, *realBackground;
XColor *realGray;
Pixmap *grayPixmap;
XColor *topShadow;
XColor *bottomShadow;
Pixmap *topshadowPixmap, *bottomshadowPixmap;
{
    (void) ReallyGetColor(p->dpy, foregroundColor, realForeground);

    (void) ReallyGetColor(p->dpy, backgroundColor, realBackground);
    
    if(!(backgroundColor && foregroundColor) && realBackground->red==realForeground->red && realBackground->blue==realForeground->blue && realBackground->green==realForeground->green) {
	if(backgroundColor) {
	    if(((long)realBackground->red + realBackground->blue +  realBackground->green) > 3*MAXCOLOR/2) *realForeground = p->blackColor;
	    else *realForeground = p->whiteColor;
	} else if(foregroundColor) {
	    if(((long)realBackground->red + realBackground->blue + realBackground->green) > 3*MAXCOLOR/2) *realBackground = p->blackColor;
	    else *realBackground = p->whiteColor;
	}
    }

    *grayPixmap= (p->ColorDisplay?None:p->grayPixmap);
    
    SetPixmap(p, graypixmapName, grayPixmap);
    
    SetGray(p, realForeground, realBackground, grayColor, realGray, grayPixmap);

    SetTopShadow(p, realForeground, realBackground, topcolorName, topShadow, topshadowPixmap);
    
    SetBottomShadow(p, botcolorName, bottomShadow, realBackground);

    SetPixmap(p, toppixmapName, topshadowPixmap);

    SetPixmap(p, botpixmapName, bottomshadowPixmap);

}

/* Configure: Get the user's preferences for this display and do other configuration which is display specific. */
static void Configure(dpy, p, fore, back)
Display *dpy;
struct prefs_s *p;
XColor *fore,*back;
{
    char *fontName;
    char *graypixmapName, *toppixmapName, *botpixmapName;
    char *foreName, *backName, *grayName;
    char *topcolorName, *botcolorName;
    XColor foreground, background, gray;
    Pixmap grayPixmap=None, topshadowPixmap, bottomshadowPixmap;
    XColor menutopshadow, menubottomshadow;
    XColor cardtopshadow, cardbottomshadow;
    XColor keysColor;
    XWindowAttributes xwa;
    unsigned long activatetime;
    Screen *s = DefaultScreenOfDisplay(dpy);

    p->dpy=dpy;
    p->refcount=1;
    
    (void) ReallyGetColor(dpy, "White", &p->whiteColor);
    (void) ReallyGetColor(dpy, "Black", &p->blackColor);

    p->newshadows =  getdefaultbool(dpy, "UseNewShadows", FALSE);
    p->newshadows = getdefaultbool(dpy, "MenubarUseNewShadows", p->newshadows);
    
    p->ColorDisplay=False;
    if(XGetWindowAttributes(dpy, DefaultRootWindow(dpy), &xwa)>0) {
	if(xwa.depth>1  && CellsOfScreen(s) >= getdefaultint(dpy, "ForceMonochromeThreshold", 16)) {

	    p->ColorDisplay=True;
	}
    }

    foreground=(*fore);
    if(p->newshadows && p->ColorDisplay && ISWHITE(p, *back)) ReallyGetColor(dpy, "gray80", &background);
    else if(p->newshadows && p->ColorDisplay && ISBLACK(p, *back)) ReallyGetColor(dpy, "gray40", &background);
    else background=(*back);
    gray=foreground;
    
      
    p->grayPixmap=XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, DefaultScreen(dpy)), (char *) grayImage, 32, 8, BlackPixel(dpy, DefaultScreen(dpy)), WhitePixel(dpy, DefaultScreen(dpy)), 1);
    
    if(p->grayPixmap==None) {
	fprintf(stderr, "menubar: couldn't get the gray pixmap!\n");
    }

    p->groupspacing=getdefaultint(dpy, "MenubarGroupSpacing", -1);
    
    p->padding=getdefaultint(dpy, "MenubarPadding", 2);
    
    p->grayPercentage=getdefaultint(dpy, "MenubarGrayPercentage", 40);
    p->topshadowPercentage=getdefaultint(dpy, "MenubarTopShadowPercentage", 50);
    
    p->holdbutton=getdefaultint(dpy, "MenubarHoldButton", Button1);
    p->hspacing=getdefaultint(dpy,"MenubarSpacing",5);
    p->vspacing=getdefaultint(dpy, "MenubarCardSpacing", 5);
    p->itemhspace=getdefaultint(dpy, "MenubarKeysSpacing", 5);
    
    p->depth=getdefaultint(dpy,"MenubarShadowDepth", 2);

    activatetime=getdefaultint(dpy,"MenubarCardDelay", 0);
    p->activatetime.tv_usec=activatetime*1000;
    p->activatetime.tv_sec=activatetime/1000;

    fontName = getdefault(dpy,"MenubarTitleFont");
    if(fontName==NULL) fontName="andy12b";

    p->titlefont = ReallyGetFont(dpy,  fontName);

    fontName = getdefault(dpy,"MenubarItemfont");
    if(fontName==NULL) fontName="andy12b";

    p->itemfont = ReallyGetFont(dpy, fontName);

    fontName = getdefault(dpy,"MenubarKeysfont");
    if(fontName==NULL) fontName="andy12bi";

    p->keysfont = ReallyGetFont(dpy, fontName);

    p->iconfont = ReallyGetFont(dpy, "amenu12");

    foreName=getdefault(dpy,"MenubarTitleColor");
    backName=getdefault(dpy,"MenubarBackgroundColor");
    grayName=getdefault(dpy,"MenubarGrayTitleColor");
    graypixmapName=getdefault(dpy,"MenubarGrayTitleStipple");
    toppixmapName=getdefault(dpy,"MenubarTopShadowStipple");
    topcolorName=getdefault(dpy,"MenubarTopShadowColor");
    botpixmapName=getdefault(dpy,"MenubarBottomShadowStipple");
    botcolorName=getdefault(dpy,"MenubarBottomShadowColor");
    topshadowPixmap=(p->ColorDisplay?None:p->grayPixmap);
    bottomshadowPixmap=None;
    BuildGraphicContext(p, foreName, backName, grayName, graypixmapName,
			 topcolorName, toppixmapName, botcolorName, botpixmapName,
			 &foreground, &background, &gray,
			 &grayPixmap, &menutopshadow, &menubottomshadow, 
			 &topshadowPixmap, &bottomshadowPixmap);

    p->menutitlesPixel=foreground.pixel;
    p->menubackgroundPixel=background.pixel;
    p->graytitlePixel=gray.pixel;
    p->graytitleStipple= grayPixmap;
    p->topshadowPixel= menutopshadow.pixel;
    p->bottomshadowPixel= menubottomshadow.pixel;
    p->topshadowPixmap = topshadowPixmap;
    p->bottomshadowPixmap= bottomshadowPixmap;
    
    foreName=getdefault(dpy,"MenubarItemColor");
    backName=getdefault(dpy,"MenubarCardBackgroundColor");
    grayName=getdefault(dpy,"MenubarGrayItemColor");
    graypixmapName=getdefault(dpy,"MenubarGrayItemStipple");
    toppixmapName=getdefault(dpy,"MenubarCardTopShadowStipple");
    topcolorName=getdefault(dpy,"MenubarCardTopShadowColor");
    botpixmapName=getdefault(dpy,"MenubarCardBottomShadowStipple");
    botcolorName=getdefault(dpy,"MenubarCardBottomShadowColor");
    BuildGraphicContext(p, foreName, backName, grayName, graypixmapName,
			 topcolorName, toppixmapName, botcolorName, botpixmapName,
			 &foreground, &background, &gray,
			 &grayPixmap, &cardtopshadow, &cardbottomshadow,
			 &topshadowPixmap, &bottomshadowPixmap);

    p->carditemsPixel=foreground.pixel;
    p->cardbackgroundPixel=background.pixel;
    p->grayitemPixel=gray.pixel;
    p->grayitemStipple= grayPixmap;
    p->cardtopshadowPixel= cardtopshadow.pixel;
    p->cardbottomshadowPixel= cardbottomshadow.pixel;
    p->cardtopshadowPixmap= topshadowPixmap;
    p->cardbottomshadowPixmap= bottomshadowPixmap;
    keysColor = foreground;
    foreName = getdefault(dpy, "MenubarKeysColor");
    p->keysPixel = ReallyGetColor(dpy, foreName, &keysColor);
    p->menubarheight=FONTHEIGHT(p->titlefont) + (p->depth+p->padding)*4;
  
}

/* mb_GetPrefsForDisplay: get a struct prefs_s for a display, if the display has been seen before the same struct prefs_s will be returned again, and a reference count will be incremented. */
struct prefs_s *mb_GetPrefsForDisplay(dpy, fore, back)
Display *dpy;
XColor *fore, *back;
{
    struct prefs_s *p=prefs;
    
    while(p!=NULL)
	if(p->dpy==dpy) {
	    p->refcount++;
	    return p;
	} else p=p->next;
    
    p=(struct prefs_s *)malloc(sizeof(struct prefs_s));
    if(!p) return NULL;
    
    
    Configure(dpy,p,fore,back);
    
    p->next=prefs;
    prefs=p;
    
    return p;
}


/* MakeGCs: make all the gcs's from the preferences for this mbinit. */
static void MakeGCs(mbi)
struct mbinit *mbi;
{
    XGCValues gcv;
    unsigned long gcmask=(GCFunction | GCLineWidth | GCForeground | GCBackground);


    gcv.foreground=mbi->prefs->menutitlesPixel;
    gcv.background=mbi->prefs->menubackgroundPixel;

    gcv.line_width=0;

    if(mbi->prefs->titlefont!=NULL) {
	gcmask|=GCFont;
	gcv.font=mbi->prefs->titlefont->fid;
    }

    SetGCs(mbi, mbi->client, &mbi->titlegcs, &gcv,gcmask);
    
    mbi->titlegcs.topshadowPixel=mbi->prefs->topshadowPixel;

    mbi->titlegcs.bottomshadowPixel= mbi->prefs->bottomshadowPixel;

    mbi->titlegcs.topshadowPixmap=mbi->prefs->topshadowPixmap;
    mbi->titlegcs.bottomshadowPixmap= mbi->prefs->bottomshadowPixmap;
    mbi->titlegcs.grayPixel=mbi->prefs->graytitlePixel;
    mbi->titlegcs.whitePixel=mbi->prefs->menutitlesPixel;
    mbi->titlegcs.grayStipple=mbi->prefs->graytitleStipple;
    
    if(mbi->prefs->itemfont==NULL) gcmask&=(~GCFont);
    else {
	gcmask|=GCFont;
	gcv.font=mbi->prefs->itemfont->fid;
    }
    
    gcv.foreground=mbi->prefs->carditemsPixel;
    gcv.background=mbi->prefs->cardbackgroundPixel;

    SetGCs(mbi, mbi->menuw, &mbi->cardgcs, &gcv,gcmask);

    mbi->cardgcs.topshadowPixel=mbi->prefs->cardtopshadowPixel;
    mbi->cardgcs.bottomshadowPixel=mbi->prefs->cardbottomshadowPixel;
    mbi->cardgcs.topshadowPixmap=mbi->prefs->cardtopshadowPixmap;
    mbi->cardgcs.bottomshadowPixmap=mbi->prefs->cardbottomshadowPixmap;
    mbi->cardgcs.grayPixel=mbi->prefs->grayitemPixel;
    mbi->cardgcs.whitePixel=mbi->prefs->carditemsPixel;
    mbi->cardgcs.grayStipple=mbi->prefs->grayitemStipple;
    
    
    SetGCs(mbi, mbi->cascadew, &mbi->cmcgcs, &gcv,gcmask);
    
    mbi->cmcgcs.topshadowPixel=mbi->prefs->cardtopshadowPixel;
    mbi->cmcgcs.bottomshadowPixel=mbi->prefs->cardbottomshadowPixel;
    mbi->cmcgcs.topshadowPixmap=mbi->prefs->cardtopshadowPixmap;
    mbi->cmcgcs.bottomshadowPixmap=mbi->prefs->cardbottomshadowPixmap;
    mbi->cmcgcs.grayPixel=mbi->prefs->grayitemPixel;
    mbi->cmcgcs.whitePixel=mbi->prefs->carditemsPixel;
    mbi->cmcgcs.grayStipple=mbi->prefs->grayitemStipple;
    

}


/* MakeWindows: create the menu card windows which will be used for the menubars associated with this mbinit */
static void MakeWindows(mbi)
struct mbinit *mbi;
{

    XSetWindowAttributes xswa;

    mbi->menuw = XCreateSimpleWindow(mbi->dpy,  RootWindow(mbi->dpy, DefaultScreen(mbi->dpy)), 0, 0, 1, 1, 0, BlackPixel(mbi->dpy, DefaultScreen(mbi->dpy)), mbi->prefs->cardbackgroundPixel);
    XSetTransientForHint(mbi->dpy,mbi->menuw,mbi->client);

    xswa.override_redirect=True;
    xswa.save_under=True;
    XChangeWindowAttributes(mbi->dpy,mbi->menuw,CWSaveUnder | CWOverrideRedirect,&xswa);

    XSelectInput(mbi->dpy, mbi->menuw, MENUEVENTS);

    mbi->cascadew = XCreateSimpleWindow(mbi->dpy, RootWindow(mbi->dpy, DefaultScreen(mbi->dpy)), 0, 0, 1, 1, 0, BlackPixel(mbi->dpy, DefaultScreen(mbi->dpy)),mbi->prefs->cardbackgroundPixel );
    XSetTransientForHint(mbi->dpy,mbi->cascadew,mbi->client);

    xswa.override_redirect=True;
    xswa.save_under=True;
    XChangeWindowAttributes(mbi->dpy,mbi->cascadew,CWSaveUnder | CWOverrideRedirect,&xswa);

    XSelectInput(mbi->dpy, mbi->cascadew, MENUEVENTS);
    
}

/* DestroyPrefsForDisplay: get rid of the preferences for a display if they are no longer needed. */
static void DestroyPrefsForDisplay(dpy)
Display *dpy;
{
    struct prefs_s *p=prefs,*last=NULL;
    while(p!=NULL)
	if(p->dpy==dpy) {
	    p->refcount--;
	    if(p->refcount>0) return;
	    XFreeFont(dpy,p->titlefont);
	    XFreeFont(dpy,p->itemfont);
	    
	    if(last) last->next=p->next;
	    else prefs=p->next;
	    free(p);
	    return;
	} else {
	    last=p;
	    p=p->next;
	}
}

/* mb_init:  create a struct mbinit for use by all menubars for a given window, it handles getting preferences, setting the default foreground and background colors and what to do when expose events arrive while the menubar is active. */
struct mbinit *mb_Init(dpy,fore,back, expose, exposedata, freeitem)
Display *dpy;
XColor *fore,*back;
int (*expose)();
void (*freeitem)();
long *exposedata;
{
    struct mbinit *result=(struct mbinit *)malloc(sizeof(struct mbinit));
    if(result==NULL) return NULL;

    result->dpy=dpy;
    result->prefs=mb_GetPrefsForDisplay(dpy,fore,back);
    result->HandleExpose=expose;
    result->ExposeData=exposedata;
    result->FreeItem=freeitem;
    result->everdrawn=False;
    
    return result;
}

/* mb_InitWindows: make the menu card windows, and initialize the gcs's */
void mb_InitWindows(mbi,client)
struct mbinit *mbi;
Window client;
{  
    mbi->client=client;

    MakeWindows(mbi);
    
    MakeGCs(mbi);
}

/* mb_Finalize: finish off everything associated with the given mbinit. */
void mb_Finalize(mbi)
struct mbinit *mbi;
{
    DestroyPrefsForDisplay(mbi->dpy);
    FreeGCs(mbi,&mbi->titlegcs);
    FreeGCs(mbi,&mbi->cardgcs);
    FreeGCs(mbi,&mbi->cmcgcs);
    XDestroyWindow(mbi->dpy,mbi->menuw);
    XDestroyWindow(mbi->dpy,mbi->cascadew);
    free((char *)mbi);
}

/* mb_Destroy: finish off a menubar (as opposed to a mbinit) */
void mb_Destroy(mb)
struct menubar *mb;
{
    int i;
    for(i=0;i<mb->nmenus;i++) DestroyMenu(mb,mb->menus[i]);
    free(mb->menus);
    free(mb);
}

/* mb_Create: Construct a new menubar with a given maintitle, moretitle and item function.  The function func will be called as func(mb, itemdata, mbdata) when a menu item is choosen. */
struct menubar *mb_Create(mbi, maintitle, moretitle, data, func)
struct mbinit *mbi;
char *maintitle, *moretitle;
char *data;
void (*func)();
{
    struct menubar *mb=(struct menubar *)malloc(sizeof(struct menubar));
    
    if(!mb) return NULL;

    mb->mainmenu=maintitle;
    mb->moretitle=moretitle;
    mb->data=data;
    mb->mbi=mbi;
    
    mb->menus=(struct tmenu **)malloc(MBLOCKSIZE*sizeof(struct tmenu *));
    if(!mb->menus) {
	free(mb);
	return NULL;
    }
    
    mb->resort=False;
    mb->nmenus=0;
    mb->lastmenuindex=(-1);
    mb->mmenus=MBLOCKSIZE;
    mb->lastvm=0;
    mb->lastmenu=NULL;
    mb->MenuFunction=func;

    
    mb->morewidth=XTextWidth(mb->mbi->prefs->titlefont, moretitle, strlen(moretitle));

    mb->moremenu=CreateMenu(mb, moretitle, MOREMENUPRIORITY);
    mb_RefitMenubar(mb);
    
    return mb;
}
