/* figv.ch - drawing object view */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figv.ch,v 1.13 1993/08/10 00:04:10 Zarf Exp $
*/

#include <rect.h>
 
#define figview_Grain (64)
#define figview_NormScale (8)
#define figview_FigUPerPix (figview_Grain/figview_NormScale)
/* fig-units per pixel at normal scale */

#define figview_SpotRad (2)
#define figview_AnchRad (4)
/* distance (in pixels) one must move mouse to register a drag */
#define figview_MouseHysteresis (4)

#define figview_SelectDelta (2)

#define figview_OpCopy (1)
#define figview_OpCopyInset (2)
#define figview_OpCut (3)
#define figview_OpPaste (4)
#define figview_OpPasteRotate (5)

struct figv_oref {
    struct figobj *o;
    struct rectangle vbox, vselbox;
    boolean ignorevbox;
    boolean selected;
    short drawnselected;
    boolean knownselected;
    boolean selectdamaged;
    struct view *insetv;
    struct dataobject *inseto;
    struct rectangle insetb;
    boolean insetbmoved;
    boolean wantupdate;
    long timestamp;
};

struct figv_highlight {
    struct rectangle r, old;
    boolean changed, oldon;
    boolean focgone;
};

struct figv_redraw_item {
    long oref;
    long clip;
};

class figview [figv] : view {

    classprocedures:

      InitializeClass() returns boolean; 
      InitializeObject(struct figview *self) returns boolean;
      FinalizeObject(struct figview *self);

    overrides:

      FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
      Update();
      DesiredSize(long width, long height, enum view_DSpass pass, long *desiredWidth, long *desiredHeight) returns enum view_DSattributes; 
      Hit(enum view_MouseAction action, long x, long y, long n)	returns struct view *;
      WantUpdate(struct view *requestor);
      ReceiveInputFocus();
      LoseInputFocus();
      ObservedChanged(struct figure *dobj, long status);
      Print(FILE *file, char *processor, char *finalFormat, boolean toplevel);
      GetApplicationLayer() returns struct view *;
      GetInterface(char *interface_name) returns struct scrollfns *;
      PostMenus(struct menulist *ml);
      PostKeyState(struct keystate *ks);
      SetDataObject(struct dataobject *dobj);
      LinkTree(struct view *parent);

    methods:
      DestroyToolset();
      FlushDataChanges();
      ChangeZoom(long dir);
      ClearSelection();
      Select(struct figobj *o);
      SelectByRef(long ref);
      ToggleSelect(struct figobj *o);
      ToggleSelectByRef(long ref);
      Unselect(struct figobj *o);
      UnselectByRef(long ref);
      IsSelected(long ref) returns boolean;
      EnumerateSelection(procedure func, long rock);
      GetOneSelected() returns long;
      SetFocusByRef(long ref);
      SetExpertMode(boolean val);
      SetBuildKeystate(struct keystate *ks);
      SetNumHighlights(int num);
      BlockUpdates(boolean val);
      CutNPaste(short operation, long rock);

macromethods:
      ToPixX(val)  ((((val))*(self)->scale*figview_GetHorizontalResolution(self))/(figview_Grain*72)-(self)->ppanx)
      ToPixY(val)  ((((val))*(self)->scale*figview_GetVerticalResolution(self))/(figview_Grain*72)-(self)->ppany)
      ToPixW(val)  (((val)*(self)->scale*figview_GetHorizontalResolution(self))/(figview_Grain*72))
      ToPixH(val)  (((val)*(self)->scale*figview_GetVerticalResolution(self))/(figview_Grain*72))
      ToFigX(val)  (((((val)+(self)->ppanx)*figview_Grain*72)/((self)->scale*figview_GetHorizontalResolution(self))))
      ToFigY(val)  (((((val)+(self)->ppany)*figview_Grain*72)/((self)->scale*figview_GetVerticalResolution(self))))
      ToFigW(val)   (((val)*figview_Grain*72)/((self)->scale*figview_GetHorizontalResolution(self)))
      ToFigH(val)   (((val)*figview_Grain*72)/((self)->scale*figview_GetVerticalResolution(self)))
      ToPrintPixX(val)  (((val)-(self)->originx)/figview_FigUPerPix)
      ToPrintPixY(val)  (((val)-(self)->originy)/figview_FigUPerPix)
      ToPrintPixW(val)  ((val)/figview_FigUPerPix)
      ToPrintPixH(val)  ((val)/figview_FigUPerPix)
      ToDefFigX(val)  (((((val))*figview_FigUPerPix*72)/(figview_GetHorizontalResolution(self)))+(self)->originx)
      ToDefFigY(val)  (((((val))*figview_FigUPerPix*72)/(figview_GetVerticalResolution(self)))+(self)->originy)
      ToDefFigW(val)   (((val)*figview_FigUPerPix*72)/(figview_GetHorizontalResolution(self)))
      ToDefFigH(val)   (((val)*figview_FigUPerPix*72)/(figview_GetVerticalResolution(self)))


      GetNumSelected()  ((self)->numselected)
      GetFocusRef()  ((self)->focusgroup)
      SetFocusChangeFlag(val)  ((self)->focuschange = (val))
      GetNumHighlights()  ((self)->numhighlights)
      SetHighlightSize(val, x, y, w, h)  ((self)->highlights[val].changed = TRUE, rectangle_SetRectSize(&((self)->highlights[val].r), (x), (y), (w), (h)))
      SetHighlight(val, rr)  ((self)->highlights[val].changed = TRUE, (self)->highlights[val].r = (*(rr)))
      GetHighlight(val)  (&((self)->highlights[val].r))
      SetPrintRect(rect)  ((self)->PrintRect = (rect))
      GetCurrentClipRegion()  ((self)->currentclipreg)
      GetClippedUpdateRect() (&(self)->ClippedUpdateRect)

    data:
      struct figtoolview *toolset;

      boolean expertmode;
      boolean focuschange;

      struct figv_oref *objs;
      long objs_size;

      long numselected;

      int numhighlights;
      struct figv_highlight *highlights;
      long focusgroup;
      long focussib;

      struct rectangle UpdateRect, MustEraseRect;

      long originx, originy;
      long panx, pany;
      long scale;

      long *tmplist;
      long tmp_size;
      long tmpnum;

      struct region **clipreglist;
      struct region *tmpregion, *currentclipreg;
      long clipreg_size;

      struct figv_redraw_item *redrawlist;
      long redraw_size;
      long redrawnum;

      long lastx, lasty, rockx, rocky;
      long lastpaste[2], lastpasteoffset;
      long lastupdater;

      struct menulist *Menus;
      struct keystate *Keystate;
      struct keystate *BuildKeystate;
      char *ForegroundColor, *BackgroundColor, *FigBackColor;

      boolean OnScreen;		/* if not view_Removed */
      boolean embedded;		/* TRUE if no call to GetApplicationLayer */
      boolean HasInputFocus;	/* T if received input focus */
      boolean InputFocusClick;
      boolean ignoreUp;		/* T iff have just asked for InputFocus */
      boolean NeedFullUpdate;
      boolean DoingFullUpdate;
      boolean UpdateCached;
      boolean UpdatesBlocked;
      boolean ShowFocusAttachments;
      boolean ShowPrintArea;

      char *PSFileName;
      struct rectangle *PrintRect;
      long lastwidth, lastheight;
      struct region *figureclip;   /* used only if embedded is TRUE */ 
      long ppanx, ppany;
      struct rectangle ClippedUpdateRect;
};

