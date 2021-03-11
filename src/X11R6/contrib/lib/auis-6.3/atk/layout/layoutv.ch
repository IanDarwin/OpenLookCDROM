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

/* $ACIS$ */

 

/* layoutview.ch - layout view definition and interface */
#define layoutview_VERSION 1


/* internal interfaces */

#define getView(self) ((self) -> header.view)
#define getLayout(self) ((struct layout *)layoutview_GetDataObject(self))

/* list of views (of a dataobject) */

/*  List of child views for this layout.

Since the child views are created at a later time than the components they
display, and since as views they belong to layoutv rather than layout, they
need a separate data structure.  The viewlist provides this service, listing
all child views and the components to which they correspond.  Order in this
simple linked list is not significant.  There should be at most one viewlist
entry per component.  Use the findviewlist or findview function to find it
and create it if necessary.
*/

struct viewlist {
    struct viewlist *nextview;		/* next view on list */
    struct view	*child;			/* child view - viewing this data object */
    struct component *component;	/* component for the child */
};

#define forallsubviews(self, v) for (v = (self)->subviews; v != NULL; v = v->nextview)
#define vChild(v) ((v)->child)
#define vComponent(v) ((v)->component)

#define vLeft(self, c)  (cLeft(c) <= 0 ? 0 : cLeft(c))
#define vTop(self, c)  (cTop(c) <= 0 ? 0 : cTop(c))
#define vRight(self, c) (cWidth(c) <= 0 ? layoutview_GetVisualWidth(self) : cRight(c))
#define vBottom(self, c) (cHeight(c) <= 0 ? layoutview_GetVisualHeight(self) : cBottom(c))
#define vWidth(self, c) (vRight(self, c) - vLeft(self, c))
#define vHeight(self, c) (vBottom(self, c) - vTop(self, c))

/* operating mode cases */

enum hitmode_enum {
    HIT_EXEC,			    /* in user mode */
    HIT_AUTHORING,		    /* in authoring mode */
    HIT_INITIALIZING,		    /* in authoring mode, selection active */
    HIT_DRAGGING,		    /* in authoring mode, dragging a component */
    HIT_CREATING};		    /* in authoring mode, creating a component */

enum createmode_enum {
    CREATE_NULL,		    /* do not fill in new components */
    CREATE_FILLER,		    /* fill new components with filler object */
    CREATE_PASTE};		    /* past into new components */

/* Interface definition */

class layoutview[layoutv]: view {

overrides:
  FullUpdate(enum view_UpdateType how, long left, long top, long width, long height);
  Update();
  Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
  DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
  ReceiveInputFocus();
  LoseInputFocus();
  PostMenus(struct menulist *ml);
  WantNewSize(struct view *requestor);
  LinkTree(struct view *parent);
  ObservedChanged(struct observable *changed, long status);
  Print(FILE *f, char *processor, char *finalFormat, boolean toplevel);

methods:
  SetSelection(struct component *c);
  SetSelectionSize(long left, long top, long width, long height) returns boolean;
  DestroyComponent();
  ToggleDebug();
  SetRunMode();
  SetInitMode();
  SetAuthoringMode();
  Paste();
  SetChildByName(char *dataname);
  SetChild();
  Promote();
  Demote();
  MakeBackground();
  MakeVariable();
  MakeFixed();
  SetCreateMode(enum createmode_enum createmode);
  SetGranularity(int granularity);
  ReplaceComponent(struct component *c, char *dataname);
  FindSubview(struct component *c) returns struct view *;
  RemoveSubview(struct view *child) returns struct component *;

macromethods:
  Selection() (self->selection)
  Hitmode() (self->hitmode)
  Createmode() (self->createmode)
  ApplyGranularity(v) ((self)->granularity > 0 ? (((v) / (self)->granularity) * (self)->granularity) : (v))

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct layoutview *self) returns boolean;
  FinalizeObject(struct	layoutview *self);

data:
	boolean	hasInputFocus;		    /* layout has the input focus */
	boolean	updateRequested;	    /* an update has been requested */
	long lastUpdate;		    /* modification timestamp of last update */
	struct keystate	*keystate;	    /* current keystate for the layout */
	struct menulist	*menulist;	    /* dup of general layout menu list */
	struct cursor *authoringCursor;	    /* cursor for layout authoring mode */
	struct viewlist	*subviews;	    /* list of imbedded views */
	enum hitmode_enum hitmode;	    /* current operating mode */
	enum createmode_enum createmode;    /* way to fill in new components */
	boolean	dragleft, dragright, dragtop, dragbottom;   /* which edges are being dragged */
	long dragx, dragy;		    /* mouse position at beginning of drag */
	long rubberleft, rubbertop, rubberwidth, rubberheight;	/* rubberband box */
	struct component *selection;	    /* currently selected component */
	int granularity;		    /* resolution for object placement */
};

/* end of layoutview.ch */

