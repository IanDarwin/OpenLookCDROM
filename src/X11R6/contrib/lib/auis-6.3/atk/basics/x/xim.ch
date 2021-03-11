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

/* im.ch
 * Class header file for the interface manager.
 *
 *
 */

#define xim_PRORGAMMERVERSION 1

#define xim2valid(AnXIMPtr) (((struct xgraphic *) ((AnXIMPtr)->header.view.drawable))->valid)
#define xim2window(AnXIMPtr) (((struct xgraphic *) ((AnXIMPtr)->header.view.drawable))->localWindow)
#define xim2display(AnXIMPtr) (xgraphic_XDisplay((struct xgraphic *)((AnXIMPtr)->header.view.drawable)))
#define xim2gc(AnXIMPtr) (xgraphic_XGC((struct xgraphic *)((AnXIMPtr)->header.view.drawable)))
#define xim2screen(AnXIMPtr) (xgraphic_XScreen((struct xgraphic *)((AnXIMPtr)->header.view.drawable)))
#define xim2fillgc(AnXIMPtr) (xgraphic_XFillGC((struct xgraphic *)((AnXIMPtr)->header.view.drawable)))
#define xim2graphic(AnXIMPtr) ((struct xgraphic *) ((AnXIMPtr)->header.view.drawable))

#define xim2programname(AnXIMPtr) ((AnXIMPtr)->programname?(AnXIMPtr)->programname:"No program name set!")

struct pane {
	char * paneStr;
	long priority;
	struct selection *firstSelection;
	struct pane *next;
};

struct selection  {
	char * selectionStr;
	long priority;
	struct proctable_Entry *proc;
	struct basicobject *object;
	long data;
	struct selection *next;
};

#define MENUVIEWCACHESIZE 57
struct menuviews {
    struct view *view;
    int refs;
    struct menuviews *next;
};

class xim : im  {

    overrides:
        UnlinkNotification(struct view *tree);
	RequestSelectionOwnership(struct view *requestor) returns boolean;
	GiveUpSelectionOwnership(struct view *requestor);

        ObservedChanged(struct observable *changed, long value);
	WhichWS() returns unsigned char *;
	PostMenus(struct menulist *menulist);
	PostCursor(struct rectangle *rec,struct cursor *cursor) ;
	ClearCursors(struct xcursor *C);
	UpdateCursors();
	SetTitle(char *titleString);
	FromCutBuffer() returns FILE *;
	OnlyFromCutBuffer() returns FILE *;
	OnlyFromSelection() returns FILE *;
	CloseToCutBuffer(FILE *writeFile);
	RotateCutBuffers(long count);
	AppendToCutBuffer(FILE *writeFile);

	/* These are window manager interface calls */
	SetWMFocus();
	ExposeWindow();
	HideWindow();
	VanishWindow();
	HandleRedraw();
	CreateWindow(char * host) returns boolean;
	CreateTransientWindow(struct im *other) returns boolean;
	CreateOverrideWindow(struct im *other) returns boolean;
	RedrawWindow();
	Hit (enum view_MouseAction action, long x, long y, long clicks) returns struct view *;

	/* These are part of the transient and overide support. */
	SupportsTransient() returns boolean;
	SupportsOverride() returns boolean;
	/* offscreen window support */
	SupportsOffscreen() returns boolean;
	CreateOffscreenWindow(struct im *other, long width, long height) returns boolean;
	SetBorderWidth(long n);
	GetLoc(struct view *vself,struct rectangle *rect) returns struct rectangle *;

	/* These methods support drag/drop protocol. */
	GetDroppedFiles() returns char **; /* NULL terminated array of strings */
	DropFile(char *pathname, struct cursor *cursor);
	DropFiles(char **pathnames, struct cursor *cursor);

	ReceiveColormap(struct xcolormap *xcmap);
	LoseColormap(struct xcolormap *xcmap);
	InstallColormap(struct xcolormap *xcmap);

	ResizeWindow(int width, int height) returns boolean;
	MoveWindow(int x, int y) returns boolean;

classprocedures:
	InitializeClass() returns boolean;
	InitializeObject(struct xim *self) returns boolean;
	FinalizeObject(struct xim *self);
	HandleFiles(long WaitTime, boolean beCheap) returns boolean;
	FlushAllWindows();
	AddAndrewFontPath(Display * DisplayPtr);
	
data:
	struct mouseStatus *MouseFacts;
	struct cmenu  *cmenu;
	struct mlcacheNode *mlcache;
	struct cacheregion *freeRegions, *activeRegions;
	struct xcursor *StandardCursor;
	struct xcursor *globalCursor;
	Window globalCursorWindow;
	long Xfileno;
	long backgroundPixel;
	boolean EverMapped;	/* hack for first XMapWindow (for messages) */
	boolean CurrentlyMapped;
	struct xim *popup_active;	/* contains pointer to override redirect im if any */
	struct xim *popup_parent;   /* Null if this xim is NOT a popup */
	struct menubar  *menu;
	boolean menubaron,cmenuson;
	struct mbinit *mbi;
	char **PopupMenuList;
	int PopupMenuListCount;
	struct cardorder *CardOrder, *MenubarCardOrder, *PopupCardOrder;
	int MenubarRedrawType;
	struct menubar *startupmenu;
	struct menuviews *menuviewslist[MENUVIEWCACHESIZE];
	char *programname;
	boolean IsOffscreenWindow;
	long xloc, yloc; /* location of window on screen */
	char **dropfiles;
	int initversion;
	boolean mshowkeys, cshowkeys;
	boolean mshowinactive, cshowinactive;
	boolean updateloc;
	Atom *AtomCache;
};

