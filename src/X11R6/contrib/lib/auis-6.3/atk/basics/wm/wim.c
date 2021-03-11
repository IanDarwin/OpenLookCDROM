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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/wim.c,v 1.21 1992/12/15 21:26:22 rr2b R6tape $";
#endif


 

/* Put in error messages for handling of keystrokes.
Figure out how to handle handlers and information requests.
Figure out some way to handle levels of user.  Macros should probably not be an novice level facility. */

#include <andrewos.h> /* sys/time.h sys/file.h */
#include <ctype.h>
#include <andyenv.h>
#include <signal.h>

#include <point.h>
#include <rect.h>
#include <graphic.ih>
#include <view.ih>
#define INTERACTION_MANAGER
#include <im.ih>
#undef INTERACTION_MANAGER
#include <wim.eh>
#include <event.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <updlist.ih>
#include <cursor.ih>
#include <wcursor.ih>
#include <mrl.ih>
#include <physical.h>
#include <message.ih>
#include <init.ih>
#include <bind.ih>


#define HITPIXELS 1
#define INITIALCUTBUFFERSIZE 200

#include <wgraphic.ih>
#include <wfontd.ih>
#include <wmclient.h>
static struct wm_window **windowList;
static long numberOfWindowFiles = 0;
static long wmWindowCtr = 0;

struct cursor *waitCursor;

#define im2window(AnIMPtr) \
	(((struct wmgraphic * )((AnIMPtr)->header.view.drawable))->window)



static struct im_GlobalDataType * gData = NULL;

/* nextWMRegion is for generating unique wm region numbers. WM dependent. */

/* static */ long nextWMRegion = 0;

/* size of a long, converted to a string */
#define CVSIZE 8

static char *cv2string(value)
    long value;
{

    static char tbuffer[20];

    /* this puts it in network byte order */
    sprintf(tbuffer, "%08x", value);
    return tbuffer;
}

static long cv2long(string)
    char *string;
{

    register int count;
    register long value;
    register char c;

    value = 0;
    for(count = 0; count < 8; count++) {
        value <<= 4;		/* shift over one nibble */
        c = *string++;
        if (c >= '0' && c <= '9')
            value += c - '0';
        else if (c >= 'a' && c <= 'z')
            value += c - 'a' + 10;
        else if (c >= 'A' && c <= 'Z')
            value += c - 'A' + 10;
    }
    return value;
}

#ifdef LWP
/* called on iomgr lwp's stack at a safe time*/
static int WakeUpIM(dummy)
    char *dummy;
{
    if (imPid != NULL)
        im_IOMGRCancel(imPid);
}
#endif /* LWP */

FlagRedraw (op, win)
int op;
struct wm_window *win; {
   register struct im *im;

   for (im = imList; im != NULL && im2window(im) != win; im = im->next);

   if (im != NULL)  {
       im->doRedraw = TRUE;
#ifdef LWP
       im_IOMGRSoftSig(WakeUpIM, NULL);
#endif /* LWP */
       globalDoRedraw = 1;
       PollTime.tv_sec = 0;	/* make it a polling select */
       PollTime.tv_usec = 0;
   }
}

/* the following table specifies the events that occur for each combination of
a mouse action and a previous state.  

The first index to mouseTable corresponds to one of the six mouse actions that
can arrive from wm.  The second index is the prior-state, and is one of the values
msLeftDown, msRightDown, or msAllUp.  The third index allows up to two 
operations to be done for each event.

The contents of the table are events to send to the client.  The next state is
deduced from the operation:

	view_LeftDown		msLeftDown
	view_LeftMovement	msLeftDown
	view_LeftUp		msAllUp
	view_RightDown		msRightDown
	view_RightMovement	msRightDown
	view_RightUp		msAllUp

Cases that are not normally expected are marked with a "!".  These can be
summarized by saying that an unexpected down action is processed by
generating an up an the prior button, while an unexpected up transition is
converted to the one that was expected.

*/

#define	msAllUp		0
#define	msLeftDown	1
#define	msRightDown	2

long statetran[3] = {im_AllUp, im_LeftDown, im_RightDown};


enum view_MouseAction   mouseTable[6][3][2] = {

/* action */
	/* prior state */  /* two actions    */

/* 0 case MouseMask(LeftButton, DownTransition) */  {
	/* msAllUp */   {view_LeftDown, view_NoMouseEvent},
	/* msLeftDown ! */   {view_LeftUp, view_LeftDown},
	/* msRightDown ! */   {view_RightUp, view_LeftDown}},

/* 1 case MouseMask(RightButton, DownTransition) */  {
	/* msAllUp */   {view_RightDown, view_NoMouseEvent},
	/* msLeftDown ! */   {view_LeftUp, view_RightDown},
	/* msRightDown ! */   {view_RightUp, view_RightDown}},

/* 2 case MouseMask(LeftButton, DownMovement) */  {
	/* msAllUp ! */   {view_LeftDown, view_NoMouseEvent},
	/* msLeftDown */   {view_LeftMovement, view_NoMouseEvent},
	/* msRightDown ! */   {view_RightMovement, view_NoMouseEvent}},

/* 3 case MouseMask(RightButton, DownMovement) */  {
	/* msAllUp ! */   {view_RightDown, view_NoMouseEvent},
	/* msLeftDown ! */   {view_LeftMovement, view_NoMouseEvent},
	/* msRightDown */   {view_RightMovement, view_NoMouseEvent}},

/* 4 case MouseMask(LeftButton, UpTransition) */  {
	/* msAllUp ! */   {view_LeftDown, view_LeftUp},
	/* msLeftDown */   {view_LeftUp, view_NoMouseEvent},
	/* msRightDown ! */   {view_RightUp, view_NoMouseEvent}},

/* 5 case MouseMask(RightButton, UpTransition) */  {
	/* msAllUp ! */   {view_RightDown, view_RightUp},
	/* msLeftDown ! */   {view_LeftUp, view_NoMouseEvent},
	/* msRightDown */   {view_RightUp, view_NoMouseEvent}}
};

	static void
mousetransition (self, operation, x, y)
	struct wmim *self;
	int operation;
	long x, y;
{
	long stepnum;
	long newstate;
	enum view_MouseAction action;
	for (stepnum = 0; stepnum < 2; stepnum ++) {
		action = mouseTable[operation][self->buttonState][stepnum];
		if (action != view_NoMouseEvent) {
			switch (action) {
			  case view_LeftDown:	newstate = msLeftDown;  break;
			  case view_RightDown:	newstate = msRightDown;  break;
			  case view_LeftMovement:	newstate = msLeftDown;  break;
			  case view_RightMovement:	newstate = msRightDown;  break;
			  case view_LeftUp:		newstate = msAllUp;  break;
			  case view_RightUp:	newstate = msAllUp;  break;
			}
			enQuserMouse(self, action, x, y, statetran[newstate]);
		}
	}
	self->buttonState = newstate;
}


static WindowGetC (file, self)
FILE *file;
struct wmim *self; {
    register int c;

    if (self->header.im.pendingInteractionEvents)
        wmim_DispatchPendingInteractionEvents(self);

    if (CurrentUserWindow != im2window(self))
	wm_SelectWindow(im2window(self));
    c = getc(winin);
    if (c == EOF) {
#ifdef notdef
	EOF occurs when the window goes away.  Possibly because wm is gone.

	(a) childDied should not be called:  it is for a subprocess death and wm isn't
	(b) we better not destroy the im.  Things still point at it.

	childDied = TRUE;
	im_Destroy(self);
#else
	/* If this is the only window left we exit. */
	if (imList == (struct im *) self && imList->next == NULL)
	    im_KeyboardExit(); /* Back out of im_KeyboardProcessor. This may have to happen multiple times...*/

/* The comment above about things still pointing to the im is
 * semi-bogus. The view_UnlinkTree done in im__FinalizeObject
 * covers most things. Others can watch for the observable
 * destroyed message...
 */
	else {

	    struct view *child = ((struct im *)self)->topLevel;

	    wmim_Destroy(self);
	    view_Destroy(child);
	}

#endif /* notdef */
    }
    else if (c == 0201) {	/* Menu hit has occurred */

        struct proctable_Entry *procTableEntry;
        struct basicobject *object;
        int count;
        long rock;
        char tbuffer[CVSIZE + 1];

        /* after the menu hit opcode,  we get  
		4 bytes of network byte order proc and 4 bytes of inset */
        for(count=0; count < CVSIZE; count++)
            tbuffer[count] = getc(winin);
        procTableEntry = (struct proctable_Entry *) cv2long(tbuffer);

        for(count=0; count < CVSIZE; count++)
            tbuffer[count] = getc(winin);
        object = (struct basicobject *) cv2long(tbuffer);

        for(count=0; count < CVSIZE; count++)
            tbuffer[count] = getc(winin);
        rock = (long) cv2long(tbuffer);
	
        enQuserMenu(self, procTableEntry, object, rock);
    }
    else if (c != wm_MouseInputToken) 
	enQuserKey(self, c);
    else {
	/* Mouse event has occurred */

	int wmaction;
	long x;
	long y;

	/* discard consecutive enqueued mouse movements */
	while (TRUE) {
		wm_SawMouse(&wmaction, &x, &y);
		if (wmaction != MouseMask(LeftButton, DownMovement) &&
				wmaction != MouseMask(RightButton, DownMovement)) 
			break;
		if (file->_cnt <= 0) 
			break;
		if ((*file->_ptr&0377) != wm_MouseInputToken)
			break;
		c = getc(file);
	}

	switch (wmaction)  {
	  case MouseMask(LeftButton, DownTransition):
		mousetransition(self, 0, x, y);  break;
	  case MouseMask(RightButton, DownTransition):
		mousetransition(self, 1, x, y);  break;
	  case MouseMask(LeftButton, DownMovement):
		mousetransition(self, 2, x, y);  break;
	  case MouseMask(RightButton, DownMovement):
		mousetransition(self, 3, x, y);  break;
	  case MouseMask(LeftButton, UpTransition):
		mousetransition(self, 4, x, y);  break;
	  case MouseMask(RightButton, UpTransition):
		mousetransition(self, 5, x, y);  break;
	}
    }
}

	boolean
wmim__CreateWindow(self, host)
    struct wmim *self;
    char *host;
{
    struct wm_window *newWin;
    
    if (host == NULL && defaultServerHost != NULL)  {
	host = defaultServerHost;
    }
    if (host != NULL) {
	char *whost, *colon;
	/* copy host to be whost.  
			Delete ":0" if found.   Convert uppercase to lower.  */
	whost = malloc(strlen(host)+1);
	strcpy(whost, host);
	colon = index(whost, ':');
	if (colon != NULL) *colon = '\0';
	for (host = whost; *host; host++)
			if (isupper(*host)) *host = tolower(*host);
		/* XXX probably ought to canonicalize the hostname to the "official" name  */
	newWin = wm_NewWindow(whost);
	free(whost);
    }
    else 
	newWin = wm_NewWindow(NULL);

    if (newWin == NULL)  {
	printf("im: Could not create new window; this program will not run without the window manager.\n");
	return FALSE;
    }
    
    if (numberOfWindowFiles == 0)  {
        numberOfWindowFiles = 2;
        windowList = (struct wm_window **) malloc(2 * sizeof(struct wm_window *));
    }
    else if (wmWindowCtr == numberOfWindowFiles)  {
        numberOfWindowFiles *= 2;
        windowList = (struct wm_window **) realloc(windowList, numberOfWindowFiles * sizeof(struct wm_window *));
    }
    windowList[wmWindowCtr++] = newWin;
    
    if (initialProgramName != NULL)
	wm_SetProgramName(initialProgramName);
    wm_SetTitle("");

    if (setDimensions)
        wm_SetDimensions(preferedWidth, preferedWidth, preferedHeight, preferedHeight);

    wm_SetMouseInterest(MouseMask(LeftButton, DownTransition) |
    			MouseMask(LeftButton, UpTransition) |
			MouseMask(RightButton, DownTransition) |
			MouseMask(RightButton, UpTransition) |
			MouseMask(LeftButton, DownMovement) |
			MouseMask(RightButton, DownMovement));
    self->buttonState = im_AllUp;

/* Prevents wm from scrolling the window when text is drawn at the bottom. */
    wm_DisableNewlines();

    im_AddFileHandler(winin, WindowGetC, self, 3);
    im2window(self) = newWin;

    point_SetPt(&wmim_GetDrawable(self)->physicalOrigin,0,0);
    im_SetLastUsed(self);
    return TRUE;
}

void wmim__FlushAllWindows(classID)
    struct classheader *classID;
{
    int i;

    for (i = 0; i < wmWindowCtr; i++)
        fflush(windowList[i]);
}

/* Menu stuff... */

/* Recursively checks if this menu list or any of its chainees need to be installed. */

/* Recursively installs a menu list and all its chainees. */
static void InstallMenus(self, menulist)
    struct wmim *self;
    struct menulist *menulist;
{
    register struct itemlist *item;
    struct headerlist *header;

    if (menulist == NULL) /* Should never happen, but... */
        return;

    menulist->curIM = (struct im *) self;

    for (header = menulist->menuChainAfter; header != NULL; header = header->next)
        InstallMenus(self, header->menulist);

    for(item = menulist->menus; item != NULL; item = item->next)
	if(menulist_ItemIsEnabled(menulist,item)){
	    char buffer[1024]; /* Big because we already got burned once... */

	    if (item->proc != NULL) {
		strcpy(buffer, item->string);
		strcat(buffer, ":\201");
		strcat(buffer, cv2string((long) item->proc));
		strcat(buffer, cv2string((long) menulist->object));
		strcat(buffer, cv2string(item->functionData));
	    }
	    else
		strcpy(buffer, item->string);

	    wm_AddMenu(buffer);
	}
    
    for (header = menulist->menuChainBefore; header != NULL; header = header->next)
        InstallMenus(self, header->menulist);
}

#define CACHEDREGIONS 10

struct cacheregion {
    int id;
    struct mlcacheNode *who;
    struct cacheregion *next, **selfP;
};

struct cacheregion *lastCacheRegion(list)
struct cacheregion *list;
{
    while(list->next!=NULL)
	list=list->next;
    return list;
}

linkCacheRegion(region,listP)
struct cacheregion *region, **listP;
{
    if(region->selfP!=NULL)
	*region->selfP=region->next;
    if(region->next!=NULL)
	region->next->selfP=region->selfP;

    region->next= *listP;
    region->selfP=listP;

    if(*listP!=NULL)
	(*listP)->selfP= &region->next;
    *listP=region;
}

struct cacheregion *unlinkCacheRegion(region)
struct cacheregion *region;
{
    if(region->selfP!=NULL)
	*region->selfP=region->next;
    if(region->next!=NULL)
	region->next->selfP=region->selfP;

    region->selfP=NULL;
    region->next=NULL;

    return region;
}

freeCacheRegions(region)
struct cacheregion *region;
{
    while(region!=NULL){
	struct cacheregion *next=region->next;
	free((char *)region);
	region=next;
    }
}

struct mlcacheNode {
    struct menulist *ml;
    long mask;
    int version;
    struct cacheregion *region;
    struct mlcacheNode *next,*prev,*others;
};

static struct mlcacheNode *newCacheNode(prev)
struct mlcacheNode *prev;
{
    struct mlcacheNode *cache=
      (struct mlcacheNode *)malloc(sizeof(struct mlcacheNode));

    cache->next=NULL;
    cache->prev=prev;
    cache->others=NULL;
    cache->ml=NULL;
    cache->region=NULL;
    cache->mask=0;
    cache->version= -1;
    
    return cache;
}

static void freeMLCache(cache)
struct mlcacheNode *cache;
{
/* printf("fmlc: freeing a cache\n"); */
    if(cache->next!=NULL)
	freeMLCache(cache->next);
    if(cache->others!=NULL)
	freeMLCache(cache->others);
    free((char *)cache);
}

static void discardCachedML(cache,rootP)
struct mlcacheNode *cache,**rootP;
{
    struct mlcacheNode **backP;

    cache->ml=NULL;

    while(cache->prev!=NULL && /* not root */
	  cache->others==NULL && cache->prev->next==cache && /* only child */
	  cache->next==NULL && cache->ml==NULL){
	struct mlcacheNode *trash=cache;
	cache=cache->prev;
	cache->next=NULL;
/* printf("dcml: blowing away a ml...going up.\n"); */
	free((char *)trash);
    }

    if(cache->ml!=NULL || cache->next!=NULL)
	return;

    if(cache->prev==NULL)
	backP=rootP;
    else
	backP= &cache->prev->next;

    while(*backP!=cache)
	backP= &(*backP)->others;

    if(backP!=rootP || cache->others!=NULL){ /* don't NULL out root */
/* printf("dcml: blowing away an OR node%s.\n",(backP==rootP)?" (Replacing root)":""); */
	*backP=cache->others;
	free((char *)cache);
    }
/* else printf("dcml: Avoiding blowing away the root.\n"); */
}

struct mlcacheNode *findCachedML(self,ml,cache,newVersionP)
struct wmim *self;
struct menulist *ml;
struct mlcacheNode *cache;
boolean *newVersionP;
{
    struct headerlist *h;
    struct mlcacheNode *unused=NULL;

    for(h=ml->menuChainBefore;h!=NULL;h=h->next){
	cache=findCachedML(self,h->menulist,cache,newVersionP);
	if(cache->next==NULL)
	    cache->next=newCacheNode(cache);
	cache=cache->next;
    }

    while(cache->ml!=ml || cache->mask!=ml->selectMask){
	if(cache->ml==NULL)
	    unused=cache;
	if(cache->others!=NULL)
	    cache=cache->others;
	else{
	    *newVersionP=TRUE;
	    if(unused==NULL)
		cache=cache->others=newCacheNode(cache->prev);
	    else
		cache=unused;
	    cache->ml=ml;
	    cache->mask=ml->selectMask;
	    cache->version=ml->menuVersion;
	}
    }

    if(cache->version!=ml->menuVersion ||
       self!=(struct wmim *)ml->curIM)
	*newVersionP=TRUE;

    for(h=ml->menuChainAfter;h!=NULL;h=h->next){
	if(cache->next==NULL)
	    cache->next=newCacheNode(cache);
	cache=findCachedML(self,h->menulist,cache->next,newVersionP);
    }

    return cache;
}

static void updateMenus(self,ml)
struct wmim *self;
struct menulist *ml;
{
    boolean newVersion=FALSE;
    struct cacheregion *destroy=NULL;
    struct mlcacheNode *cache;
    struct rectangle *rect= &self->header.view.drawable->visualBounds;

    if(ml==NULL){
	/* just take away old menus */
	if(self->menuRegion>=0)
	    wm_DefineRegion(self->menuRegion,0,0,0,0);
	return;
    }

    cache=findCachedML(self,ml,self->mlcache,&newVersion);

    if(cache->region==NULL || newVersion){
	if(newVersion){
	    menulist_IncrementMLVersion();
	    if(cache->region!=NULL)
/* {printf("um: destroying menu region %d.\n",cache->region->id); */
		destroy=unlinkCacheRegion(cache->region);
/* } */
	}	    

	if(self->freeRegions!=NULL)
	    cache->region=self->freeRegions;
	else if(self->activeRegions!=NULL){
	    cache->region=lastCacheRegion(self->activeRegions);
	    if(cache->region->who!=NULL && cache->region->who!=cache)
		discardCachedML(cache->region->who,&self->mlcache);
	    unlinkCacheRegion(cache->region);
	    /* this can't be most recently used region if the number
	     * of regions used is >2
	     */
	    wm_DestroyRegion(cache->region->id);
/* printf("um: reclaiming menu region %d.\n",cache->region->id); */
	}else{
	    fprintf(stderr,"wmim(updateMenus): out of menu regions!\n");
	    return;
	}

	cache->region->who=cache; 

        wm_DefineRegion(cache->region->id,0,0,0,0);
        wm_SelectRegion(cache->region->id);
/* printf("um: defining menu region %d.\n",cache->region->id); */
        InstallMenus(self,ml);
    }
/*
else if(cache->region->id!=self->menuRegion)
printf("um: reusing menu region %d.\n",cache->region->id);
*/	
    linkCacheRegion(cache->region,&self->activeRegions);

    if(cache->region->id==self->menuRegion)
	return;

    /* put new region in the right place */
    wm_DefineRegion(cache->region->id,
		    rect->left,rect->top,rect->width,rect->height);

    /* Collapse old one. */
    if(destroy==NULL || self->menuRegion!=destroy->id)
	wm_DefineRegion(self->menuRegion,0,0,0,0);

    if(destroy!=NULL){
	wm_DestroyRegion(destroy->id);
	linkCacheRegion(destroy,&self->freeRegions);
    }

    self->menuRegion=cache->region->id;
}

void wmim__PostMenus(self, menulist)
    struct wmim *self;
    struct menulist *menulist;
{
    struct wm_window *window = im2window(self);
    struct im * imself = (struct im *) self;
    struct rectangle *rect = &self->header.view.drawable->visualBounds;

    if (window == NULL)
        return;
    menulist_ClearChain(imself->menus);
    menulist_ChainBeforeML(imself->menus, menulist, menulist);
    if (imself->init != NULL)
        menulist = init_ModifyMenulist(imself->init, imself->menus);
    else
        menulist = imself->menus;

   /* since we're not called from a fullupdate */
    wmgraphic_ClearCache(window);
    wm_SelectWindow(window);
    wm_SetClipRectangle(rect->left, rect->top, rect->width, rect->height);

    updateMenus(self,menulist);
    fflush(winout);
}

/* wim__WhichWS()
	returns a string for the current window system:  "wm"
*/
	unsigned char *
wmim__WhichWS(self)
	struct wmim *self;
{
	return (unsigned char *)"wm";
}

boolean wmim__InitializeObject(classID, self)
struct classheader *classID;
struct wmim *self;
{
    int i;

    self->cursorRegion = nextWMRegion++;
    self->menuRegion= -1;
    self->mlcache=newCacheNode(NULL);
    self->freeRegions=NULL;
    for(i=0;i<CACHEDREGIONS;i++){
	linkCacheRegion((struct cacheregion *)
			calloc(1, sizeof(struct cacheregion)),
			&self->freeRegions);
	self->freeRegions->id=nextWMRegion++;
    }
    self->activeRegions=NULL;
    self->buttonState = im_AllUp;
    return TRUE;
}

void wmim__FinalizeObject(classID, self)
    struct classheader *classID;
    struct wmim *self;
{
    struct wm_window *oldwin = im2window(self);
    long wctr, i;

    for (wctr = 0; wctr < wmWindowCtr; wctr++)
	if (windowList[wctr] == oldwin) {
		for (i = wctr; i < wmWindowCtr-1; i++)
			windowList[i] = windowList[i+1];
		wmWindowCtr--;
		break;
	}
    wm_SelectWindow(oldwin);
    wmim_ClearCursorList(self); 

    /* Not really necessary since WM will implicitly free the regions when the window is deleted. */
    wm_DestroyRegion(self->cursorRegion);
    wm_DestroyRegion(self->menuRegion);

    im_RemoveFileHandler(winin);
    freeMLCache(self->mlcache);
    freeCacheRegions(self->freeRegions);
    freeCacheRegions(self->activeRegions);
    wm_DeleteWindow();
    ((struct wmgraphic *) self->header.view.drawable)->window = NULL; /* So nobody else tries to use it. */
}


void wmim__HandleRedraw (im)
register struct im *im;
{
    long width;
    long height;
    struct wmim * wmself = (struct wmim *) im;

    im->doRedraw = FALSE;
    im->inRedraw = TRUE;

    /* 
      Get the size of the window
      */

    wm_SelectWindow(im2window(im));
    wm_GetDimensions (&width, &height);

    /* 
      If it is actually on the screen then display the top level view
      */

    if (width > 0 && height > 0)  {

        wmgraphic_ClearCache(CurrentUserWindow);
        wm_SelectWindow(CurrentUserWindow);
        wm_SetClipRectangle(0, 0, width, height);
        wm_ClearWindow();
        rectangle_SetRectSize(&im->header.view.drawable->localBounds, 0,0,width,height);


        im->header.view.drawable->visualBounds = im->header.view.drawable->localBounds;
        point_SetPt(&im->header.view.drawable->enclosedOrigin,0,0);

        wm_DefineRegion(wmself->cursorRegion, 0, 0, width, height); /* For the process and window cursor... */

        if (wmself->menuRegion != -1)
            wm_DefineRegion(wmself->menuRegion, 0, 0, width, height); /* WM dependant menu support. */

        if(im->cursorlist) im_ClearCursorList(im);
        if (im->topLevel != NULL)  {
            view_InsertView(im->topLevel, im, &im->header.view.drawable->localBounds);
            view_FullUpdate(im->topLevel, view_FullRedraw, 0, 0, width, height);

            wmim_FlushAllWindows();

        }

        im_UpdateCursors(im);
        im->inRedraw = FALSE;
    }
}


/* when HandleFiles is called with beCheap true, it shouldn't do a select, but only
    check to see if any data is already in this process (via stdio's previous read).
    This should cut down on the number of selects per read done by this thing.
    
    Function returns true if it invoked any handlers, false otherwise.
*/
	static boolean 
wmim__HandleFiles (ClassID, twait, beCheap)
	struct classhdr *ClassID;
	long twait; 
	boolean beCheap;
{
    int nfs;
    long rmask = 0;
    long wmask = 0;
    boolean ret = FALSE;
    FILE *f;

    if ( ! beCheap)  wmim_FlushAllWindows();

    nfs = NFILEHandlers;
    while (--nfs>=0)  {
	f = globalFILEHandlers[nfs].file;
	if (f != NULL)  {
	    if (f->_cnt > 0)  {
		ret = TRUE;
		(globalFILEHandlers[nfs].proc)(f, globalFILEHandlers[nfs].procdata);
	    }
	    else
		rmask |= 1 << fileno(f);
	}
    }
    if (ret)
	return TRUE;
    else if (beCheap)
	return FALSE;

    if (NCanOutHandlers) {
	nfs = NCanOutHandlers;
	while (--nfs>=0)  {
	    f = CanOutHandlers[nfs].file;
	    if (f != NULL)
		wmask |= 1 << fileno(f);
	}
    }

    /* do the select to wait */

    if (twait < event_ENDOFTIME) {
	if ((PollTime.tv_sec = event_TUtoSEC(twait)))
		twait -= event_SECtoTU(PollTime.tv_sec);
	PollTime.tv_usec = event_TUtoUSEC(twait);
    }
    else {
	/* hack from Nichols: don't use 0 here, we have to be able to zap it from signal proc */

	PollTime.tv_sec = 99999999;
	PollTime.tv_usec = 0;
    }

    if (anyDelivered)
        im_DeliverSignals();		/* avoid non-race case */

#ifdef LWP
    im_LWPCurrentProcess(&imPid);
    ret = im_IOMGRSelect(32, &rmask, &wmask, 0, &PollTime);
#else /* LWP */
    ret = select(32, &rmask, &wmask, 0, &PollTime);
#endif /* LWP */
    /* deliver the signals */
    if (anyDelivered) im_DeliverSignals();

    if (ret > 0)  {
	if (rmask)  {

	    nfs = NFILEHandlers;
	    while (--nfs>=0)  {
		f = globalFILEHandlers[nfs].file;
		if (f != NULL && (rmask & (1 << fileno(f))) != 0)
		    (globalFILEHandlers[nfs].proc)(f, globalFILEHandlers[nfs].procdata);
	    }
	}
	if (wmask)  {
	    nfs = NCanOutHandlers;
	    while (--nfs>=0)  {
		f = CanOutHandlers[nfs].file;
		if (f != NULL && (wmask & (1 << fileno(f))) != 0)
		    (CanOutHandlers[nfs].proc)(f, CanOutHandlers[nfs].procdata);
	    }
	}
	return TRUE;
    }
    else
	return FALSE;
}

void wmim__RedrawWindow(self)
struct wmim *self;
{
    wmim_HandleRedraw(self); /*sigh*/
}


boolean  wmim__InitializeClass(classID)
    struct classheader *classID;
{
    gData = im_GetGlobalData(); /* load up the pointer to shared, global state */

    waitCursor = cursor_Create(NULL);
    cursor_SetStandard(waitCursor, Cursor_Wait);

    return TRUE;
}


/*     The following supplies cursor support.   */
/*
 NOTE The ClearCursors() Macro does different things in WM and X

In WM it delete the subrectangles associated with a cursor, but they will be rebuilt in im_UpdateCursor if the item remains in the im's cursorlist. Routines that remove cursors have to call this macro on any cursor that may be affected by the removal of a cursor that had been placed over it.

Under X it unmaps the inputonly window associated the cursor, since X handles these windows properly, only cursors that are really being removed are cleared.

*/
#define wmim_SelectWMCursor(CURSOR,self) 		(((struct cursor *)(CURSOR))->fillFont == NULL) ? wm_SetStandardCursor(((struct cursor *)(CURSOR))->fillChar) :  wm_SetCursor(fontdesc_GetRealFontDesc(((struct cursor *)(CURSOR))->fillFont, wmim_GetDrawable(self)),((struct cursor *)(CURSOR))->fillChar)

void wmim__ClearCursors(self, C)
struct wmim * self;
struct wmcursor * C; {
    int start = 0;
    struct rectangle result;
    register struct wmcursor *cp;

    if (im_IsPlaying()) return;

    if (CurrentUserWindow !=  im2window(self)) {
	wm_SelectWindow(im2window(self));
    }
    /* Clear out the rectangles of any previously placed intersecting cursor */
    for(cp = (struct wmcursor *) ((struct im *)self)->cursorlist; cp != NULL;cp = (struct wmcursor *)(cp->header.cursor.next)){
	if(cp == C) start++;
	else if(start && cp->mrlist){
	    rectangle_IntersectRect(&result,&(cp->rec),&(C->rec));
	    if(result.width > 0 && result.height > 0 && !rectangle_IsEnclosedBy(&(cp->rec),&(C->rec)) && 
	       !rectangle_IsEnclosedBy(&(C->rec),&(cp->rec))){
		cp->mrlist = mrl_Reset(cp->mrlist,NULL);
	    }
	}
    }
    if(C->mrlist)
	C->mrlist = mrl_Reset(C->mrlist,NULL);
}

void wmim__PostCursor(self,rec,cursor)
struct im *self;
struct rectangle *rec;
struct cursor *cursor;
{
    static struct rectangle temprec;
    struct rectangle *vrec = &(((struct wmcursor *)cursor)->rec);

    if (im_IsPlaying()) return;

    if(cursor == NULL || cursor->view == NULL || cursor->fillChar == 0 ) return;
    if(cursor->posted != self){
	if(cursor->posted != NULL)/*  Cursor already posted to another im  */
	    return;
        cursor->next = self->cursorlist;
        self->cursorlist = cursor;
	im_ClearCursors(self, cursor);
    }
/*     clip to rectangles view and change to window coord.
 */    rectangle_IntersectRect(&temprec,rec,&(cursor->view->drawable->visualBounds));
    physical_LogicalToGlobalRect(view_GetDrawable(cursor->view),&temprec);
    if (rectangle_IsEqualRect(&temprec,vrec)){
	if(cursor->posted && ((struct wmcursor *)cursor)->mrlist && 
	   ((struct wmcursor *)cursor)->mrlist->wmregion > 0) 
	    /* unnecessary post */ return;
    }
    else {
	*vrec = temprec;
	if(cursor->posted){
	    im_ClearCursors(self, cursor);
	}
    }
    self->cursorPostsPending = TRUE;
    cursor->posted = self;
    if(ProcessCursor == NULL && self->WindowCursor == NULL && self->inRedraw == FALSE)
	im_UpdateCursors(self);
}

void wmim__ClearCursorList(self)
struct wmim *self;
{
    if (im_IsPlaying()) return;

    super_ClearCursorList(self);
    wmim_UpdateCursors(self);
}

void wmim__UpdateCursors(self)
struct wmim *self;
{
    if (im_IsPlaying()) return;

    if (CurrentUserWindow !=  im2window(self)) {
        wm_SelectWindow(im2window(self));
        }
    wmgraphic_ClearCache(CurrentUserWindow);
    wm_SetClipRectangle(0, 0, wmim_GetLogicalWidth(self), wmim_GetLogicalHeight(self));

/* Clear the cursors on the two menu regions. */
    wm_SelectRegion(self->cursorRegion);
    if (!updateGlobalCursors(self)){
        wm_SetStandardCursor(wm_ArrowCursor);
        ActivateCursors(self);
    }
    fflush(winout);
}

static int updateGlobalCursors(self)
struct im *self;
{

    if(ProcessCursor != NULL) {
        if(self->CursorsAreActive) DeactivateCursors(self);
	wmim_SelectWMCursor(ProcessCursor,self);
        return(1);
    }
    else if(self->WindowCursor != NULL) {
        if(self->CursorsAreActive) DeactivateCursors(self);
	wmim_SelectWMCursor(self->WindowCursor,self);
        return(1);
    }
    return(0);
}

static setCursorRegions(cursor)
struct wmcursor *cursor;
{
    register struct cursor *cp;
    struct rectangle *rec;

    if(!cursor) return;
    rec = &(cursor->rec);
    if(cursor->header.cursor.next){
	setCursorRegions((struct wmcursor *) cursor->header.cursor.next);
    }
    if(cursor->mrlist == NULL){
	cursor->mrlist = mrl_Create(-1,NULL,rec);
    }
    if(cursor->header.cursor.next){
	for(cp = cursor->header.cursor.next; cp ; cp = cp->next){
	     if(((struct wmcursor*)cp)->mrlist)
		 ((struct wmcursor *)cp)->mrlist = mrl_Disect(((struct wmcursor *)cp)->mrlist,rec);
	}
    }
}

static ActivateCursors(self)
struct im *self;
{	
    register struct wmcursor *cp;
    register struct mrl *mr;

    cp = (struct wmcursor *) self->cursorlist;
    if(self->cursorPostsPending) setCursorRegions(cp);
    for(; cp != NULL;cp = (struct wmcursor *)(cp->header.cursor.next)){
	for (mr = cp->mrlist; mr != NULL; mr= mr->next){
	    if(mr->wmregion < 0 ) {
		mr->wmregion = nextWMRegion++;	
		wm_DefineRegion(mr->wmregion,mr->left,mr->top,mr->right - mr->left,mr->bottom - mr->top);
		wm_SelectRegion(mr->wmregion);
		wmim_SelectWMCursor(cp,self);
	    }
	    else if(!self->CursorsAreActive){
		wm_DefineRegion(mr->wmregion,mr->left,mr->top,mr->right - mr->left,mr->bottom - mr->top);
		wm_SelectRegion(mr->wmregion);
		wmim_SelectWMCursor(cp,self);
	    }
	}
    }
    self->CursorsAreActive = 1;
    self->cursorPostsPending = FALSE;
}

static DeactivateCursors(self)
struct im *self;
{
    register struct wmcursor *cp;
    register struct mrl *mr;

    for(cp = (struct wmcursor *)self->cursorlist; cp != NULL;cp = (struct wmcursor *) (cp->header.cursor.next)){
       for (mr = cp->mrlist; mr != NULL; mr= mr->next){
	 if(mr->wmregion > -1 )
            wm_DefineRegion(mr->wmregion,0,0,0,0);
        }
    }
    self->cursorPostsPending = FALSE;
    self->CursorsAreActive = 0;
}


void wmim__SetTitle(self, title)
    struct im *self;
    char *title;
{
    super_SetTitle(self,title);
    if (CurrentUserWindow !=  im2window(self))
        wm_SelectWindow(im2window(self));
    wm_SetTitle((self->title) ? self->title : "");
}

/*
 * The wm protocol disallows NUL characters in the cutbuff.
 * ToCutBuffer and FromCutBuffer map as follows:
 *     '\0' ==> QUOTE, '0';
 *      QUOTE ==> QUOTE, QUOTE
 * Where QUOTE is currently defined as '\035'.
 * A weird character is used for QUOTE so that, as much as possible,
 * we don't interfere with common characters (such as backslash)
 */

#define QUOTE '\035'

FILE *wmim__FromCutBuffer(self)
    struct wmim *self;
{
    long count;
    FILE *pasteFile;
    register int c;
    register char *p, haveQ = 0;

    im_SetProcessCursor(waitCursor);
    if (CurrentUserWindow !=  im2window(self))  {
	fflush(winout);
        wm_SelectWindow(im2window(self));
    }

    wm_ReadFromCutBuffer(0);
    GR_WAITFOR (GR_HEREISCUTBUFFER, 1);
    count = 0;
    p = readCutBuffer.string;
    while ((c = getc(winin)) != EOF && c != 0)  {
        if (haveQ) {
            if (c == '0')
                c = '\0';
            haveQ = 0;
        } else if (c == QUOTE) {
            haveQ = 1;
            continue;
        }
	count += 1;
	if (count > readCutBuffer.size)  {
	    if (readCutBuffer.string == NULL)  {
		readCutBuffer.size = (count >= INITIALCUTBUFFERSIZE) ? count : INITIALCUTBUFFERSIZE;
		readCutBuffer.string = malloc(readCutBuffer.size);
		p = readCutBuffer.string;
	    }
	    else  {
		readCutBuffer.size *= 2;
		if (readCutBuffer.size < count)
		    readCutBuffer.size = count;
		readCutBuffer.string = realloc(readCutBuffer.string, readCutBuffer.size);
		p = &(readCutBuffer.string[count - 1]);
	    }
	}
	*p++ = c;
    }

    readCutBuffer.pos = 0;
    readCutBuffer.length = count;
    pasteFile = (FILE *) im_vfileopen("r", &readCutBuffer);
    im_SetProcessCursor(NULL);
    return pasteFile;
}


void wmim__CloseToCutBuffer(self, writeFile)
    struct im *self;
    FILE *writeFile;
{
    register int i;
    register unsigned char *s;

    im_SetProcessCursor(waitCursor);

    im_vfileclose(writeFile, &writeCutBuffer);

    if (CurrentUserWindow != im2window(self))  {
	fflush(winout);
        wm_SelectWindow(im2window(self));
    }

    wm_WriteToCutBuffer();

    i = writeCutBuffer.length;
    s = (unsigned char *) writeCutBuffer.string;

    while (i--) {
        register int c = *s++;
        if (c == QUOTE)
            putc(c, winout);
        else if (c == '\0') {
            putc(QUOTE, winout);
            c = '0';
        }
        putc(c, winout);
    }

    putc('\0', winout);
    fflush(winout);

    im_SetProcessCursor(NULL);
}

void wmim__RotateCutBuffers(self, count)
    struct im *self;
    long count;
{

    if (CurrentUserWindow !=  im2window(self))
        wm_SelectWindow(im2window(self));
    wm_RotateCutRing(1 - count);

}

void wmim__AppendToCutBuffer(self, writeFile)
    struct wmim *self;
    FILE *writeFile;
{

    wmim_CloseToCutBuffer(self, writeFile);
    if (CurrentUserWindow != im2window(self))
        wm_SelectWindow(im2window(self));
    wm_AppendCutBuffers();
}

void wmim__SetWMFocus(self)
    struct wmim *self;
{

    struct wm_window *tempWindow = CurrentUserWindow;

    if (CurrentUserWindow != im2window(self)) {
        fflush(winout);
        wm_SelectWindow(im2window(self));
    }
    wm_AcquireInputFocus();
    fflush(winout);
    if (tempWindow != NULL)
        wm_SelectWindow(tempWindow);
}

/* The signal goo in these next two functions is used to synchronize the redraw
 * signals. The idea is to make it hard for us to get two signals in a row
 * before we can process the first one. This is done to prevent a UNIX kernel
 * bug involving out-of-band data from hanging the process.
 */
void wmim__ExposeWindow(self)
    struct wmim *self;
{
    int mask;
    struct wm_window *tempWindow = CurrentUserWindow;
    int width;
    int height;

    if (CurrentUserWindow != im2window(self)) {
        fflush(winout);
        wm_SelectWindow(im2window(self));
    }
    wm_GetDimensions(&width, &height);
    if (width > 0 || height > 0) {
	return;
    }
#if !defined(mc68020) || defined(sun34)
    mask = sigblock(1<<(15));
#endif /* !defined(mc68020) || defined(sun34) */
    wm_ExposeMe();
    fflush(winout);
#if !defined(mc68020) || defined(sun34)
    sigpause(~(1<<(15)));
    sigsetmask(mask);
#endif /* !defined(mc68020) || defined(sun34) */
    if (tempWindow != NULL)
        wm_SelectWindow(tempWindow);
}

void wmim__HideWindow(self)
    struct wmim *self;
{
    int mask;
    struct wm_window *tempWindow = CurrentUserWindow;
    int width;
    int height;

    if (CurrentUserWindow != im2window(self)) {
        fflush(winout);
        wm_SelectWindow(im2window(self));
    }

    wm_GetDimensions(&width, &height);
    if (width <= 0 && height <= 0) {
	return;
    }

#if !defined(mc68020) || defined(sun34)
    mask = sigblock(1<<(15));
#endif /* !defined(mc68020) || defined(sun34) */

    wm_HideMe();
    fflush(winout);

#if !defined(mc68020) || defined(sun34)
    sigpause(~(1<<(15)));
    sigsetmask(mask);
#endif /* !defined(mc68020) || defined(sun34) */

    if (tempWindow != NULL)
        wm_SelectWindow(tempWindow);
}

void wmim__VanishWindow(self)
    struct wmim *self;
{
	wmim_HideWindow(self);	/* shrink it */
	wmim_HideWindow(self);	/* make it really go away */
}
