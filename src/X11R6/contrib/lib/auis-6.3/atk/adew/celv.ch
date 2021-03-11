/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

#define celview_UnInitialized 0
#define celview_MakeHorz 1
#define celview_MakeVert 2
#define celview_INSERTVIEW 3
#define celview_HasView 4
#define celview_Initialized 5
#define celview_IsSplit 6
#define celview_UpdateView 7
#define celview_FirstUpdate 8
#define celview_NoUpdate 9
#define celview_NewOverlay 10
#define celview_DoFull 11

class celview[celv]:view {
overrides:
    WantNewSize(struct view *requestor);
    PostMenus(struct menulist *menulist);
    PostKeyState(struct keystate *keystate);
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    ReceiveInputFocus();
    LoseInputFocus();
    SetDataObject(struct dataobject *ls);
    ObservedChanged (struct observable *changed, long value);
    GetManyParameters( struct resourceList * resources,
		      struct atomlist * name,
		      struct atomlist * class );		      
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    LinkTree(struct view *parent);
    InitChildren();
    CanView(char *TypeName) returns boolean;
    WantUpdate(struct view *requestor);
    PostCursor(struct rectangle *rec,struct cursor *cursor) ;
    WantHandler(char *handlerName) returns struct basicobject *;
    GetOrigin(long width, long height, long *originX, long *originY);
methods:
    SetResizing(long key);
    makeview(struct cel *ls) returns struct view *;
    PromptForInfo(struct arbiter *arb,boolean promptForViewName,boolean changeRefName) returns int;
    PostParameters();
    Copy();
    SetHitfunc(struct view * (*hitfunc)(),long hitrock);
    SetKeymap( struct keymap *km);
    SetMenulist( struct menulist *ml);
    Repost();
    PushOverlay(struct view *view,struct rectangle *rect,long flags);
    PopOverlay(struct view *view) returns struct view *;
macromethods:
    GetTrueChild() ((self)->truechild)
    GetApplication() ((self)->child)
    GetRemoteArb() ((self)->rarb)
    SetRemoteArb(Arb) ((self)->rarb = (Arb))
    GetCel() (struct cel *) celview_GetDataObject(self)
    GetAppFlag() ((self)->AddAppLayer)
    SetAppFlag(VAL)  ((self)->AddAppLayer = (VAL))
classprocedures:
    InitializeClass()returns boolean;
    FinalizeObject(struct cel *self);
    InitializeObject(struct cel *self) returns boolean;
    ResourceListToString(char *str,struct resourceList *rl) returns int;
    GetCelviewFromView(struct view *v) returns struct celview *;
data:
	int HasFocus;
        struct keymap *keymap;
	struct keystate *keystate;
	struct menulist *menulist;
	struct cursor *cursor;
	int mode;
	struct arbiterview *arb,*rarb;
	int level;
	struct text *script;
    struct view *child,*truechild,*safec,*safetc;
    int promptforparameters;
    struct text *pdoc;
    int desw,desh;
    struct cursor *widthcursor, *heightcursor;
    int Moving,resizing,WasMoving, WasResizing;
    struct menulist *menus;
    int drawing, OldMode,sizepending,NeedsPost,vismode;
    struct atom *viewatm,*dataatm,*refatm;
    int application,NeedsRemade,NeedsReinit;
    struct view * (*hitfunc)();
    long hitrock;
    struct rectangle enclosingRect,childRect;
    struct overlay *olist;
    boolean TopLevel;
    boolean AddAppLayer; /* Add an application layer, regardless of flag in cel */
};

