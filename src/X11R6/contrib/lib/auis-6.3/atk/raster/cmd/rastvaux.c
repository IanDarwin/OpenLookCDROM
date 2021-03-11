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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/rastvaux.c,v 1.21 1993/06/21 17:29:38 gk5g Exp $";
#endif

#include <andrewos.h> /* strings.h */
#include <class.h>
#define AUXMODULE 1
#include <rasterv.eh>

#include <im.ih>
#include <frame.ih>
#include <buffer.ih>
#include <view.ih>
#include <fontdesc.ih>
#include <sys/param.h> /* Defines MAXPATHLEN among other things */
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <cursor.ih>
#include <proctbl.ih>
#include <message.ih>
#include <scroll.ih>
#include <environ.ih>
#include <rect.h>
#include <txttroff.ih>
#include <rastimg.ih>
#include <raster.ih>
#include <rastoolv.ih>
#include <heximage.ih>
#include <dispbox.h>

extern void PostMenus();
extern void RotateCommand();

void CenterViewSelection();
void ViewHideHighlight();
void CorrectHighlight();
void DrawPanHighlight();
void StartPanning();
void ContinuePanning();
void ClipScroll();
void UpdateZoomedSelection();
void FinishPanning();
void SetPixel();
void DrawLineTo();
void ZoomToVisualBounds();
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	
 *	Methods
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*  -----------  Added  4/6/89 -------------- */
/* 
  Note:  for a lot of these I call the menu command functions.  At the
      end of these functions there is a PostMenus function call.  Since this
           function does nothing, I'm assuming that's not a problem.
   Note:  for FitToSize, I have no clue what the "Original" stuff is all
       about - I just copied the code from ScaleCommand.
*/
void rasterview__FitToSize(self, logicalrect )
register struct rasterview  *self;
struct rectangle logicalrect; 
{  
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    long x, y, w, h;
    long NewX, NewY, NewW, NewH;

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;

    /* Until Scaling works while Zoomed... */
    if (NotFullSize(self)) return;

    rectangle_GetRectSize(&self->DesiredSelection, &x, &y, &w, &h);
    rectangle_GetRectSize(&logicalrect, &NewX, &NewY, &NewW, &NewH);

 
    DEBUG(("Original is%s NULL\n", ((self->Original == NULL) ? "" : " not")));
    DEBUG(("New Absolute: (%d,%d)\n", NewW, NewH));

    if (self->Original == NULL) {
	self->Original = rasterimage_Clone(pix);
	rasterimage_SetBitsPtr(pix, NULL);
	rasterimage_GetScaledSubraster(self->Original,
				       &self->DesiredSelection, NewW, NewH, pix);
    }
    else {
	struct rectangle sub, original;
	/* map Desired Selection within scaled version to selection within the Original */
	rectangle_SetRectSize(&original, 0, 0,
			      rasterimage_GetWidth(self->Original),
			      rasterimage_GetHeight(self->Original));
	if (FullySelected(self))
	    sub = original;
	else {
	    float wscale =
	      rasterimage_GetWidth(self->Original)/rasterimage_GetWidth(pix);
	    float hscale =
	      rasterimage_GetHeight(self->Original)/rasterimage_GetHeight(pix);
	    rectangle_SetRectSize(&sub,
				  rectangle_Left(&self->DesiredSelection)*wscale,
				  rectangle_Top(&self->DesiredSelection)*hscale,
				  rectangle_Width(&self->DesiredSelection)*wscale,
				  rectangle_Height(&self->DesiredSelection) * hscale);
	    rectangle_IntersectRect(&sub, &sub, &original); }
	DEBUG(("Original: (%d,%d,%d,%d)\n",
	       rectangle_Left(&original), rectangle_Top(&original),
	       rectangle_Width(&original), rectangle_Height(&original)));
	DEBUG(("Selection: (%d,%d,%d,%d)\n",
	       rectangle_Left(&sub), rectangle_Top(&sub),
	       rectangle_Width(&sub), rectangle_Height(&sub)));
	DEBUG(("%s: 0x%x\n", class_GetTypeName(self->Original), self->Original));

	rasterimage_GetScaledSubraster(self->Original, &sub, NewW, NewH, pix);
    }

    rectangle_SetRectSize(&self->DesiredSelection, 0, 0, NewW, NewH);
    rectangle_SetRectSize(&self->ViewSelection, 0, 0, NewW, NewH);

    if (NotFullSize(self))
	ZoomToVisualBounds(self, 0, 0);

    CenterViewSelection(self);

    rasterimage_NotifyObservers(pix, rasterview_SCALECHANGED);
}  
  

void rasterview__AutoCenter(self)
register struct rasterview  *self;
{   
    CenterCommand(self, 0);
}

void rasterview__ZoomRaster(self, zoomIn )
register struct rasterview  *self;
boolean zoomIn; 
{ 
    if (zoomIn)
	ZoomInCommand(self, 0);
    else
	ZoomOutCommand(self, 0);
}

void rasterview__SetToolMode(self)
register struct rasterview  *self;
{   
    ToolCommand(self, 0);
}
	
void rasterview__SetPan(self)
register struct rasterview  *self;
{   
    PanCommand(self, 0);
}
	
void rasterview__SetRegionSelect(self)
register struct rasterview  *self;
{   
    RegionSelectCommand(self, 0);
}
	
void rasterview__SetTouchUp(self)
register struct rasterview  *self;
{   
    TouchUpCommand(self, 0);
}
	
void rasterview__RotateRaster(self)
register struct rasterview  *self;
{   
    RotateCommand(self, 0);
}

void rasterview__SetDataObject(self, ras)
register struct rasterview  *self;
register struct raster *ras;
{
    register struct raster *oldras = (struct raster *)rasterview_GetDataObject(self);
    DEBUG(("rasterview__SetDataObject(0x%lx, 0x%lx) was 0x%lx\n", self, ras, oldras));
    if (oldras == ras) return;	/* this is needed to avoid
				 Destroy'ing oldras in RemoveObserver */
    super_SetDataObject(self, ras);
    if (ras != NULL) {
	self->ViewSelection = self->DesiredSelection = ras->subraster;
	DEBUG(("VS: (%d,%d,%d,%d)\n",
	       rectangle_Left(&self->ViewSelection),
	       rectangle_Top(&self->ViewSelection),
	       rectangle_Width(&self->ViewSelection),
	       rectangle_Height(&self->ViewSelection))); }
    else 
	rectangle_EmptyRect(&self->DesiredSelection);
    rasterview_WantNewSize(self, self);
    LEAVE(rasterview__SetDataObject);
}

void rasterview__ObservedChanged(self, obs, status)
register struct rasterview  *self;
struct observable *obs;
long status;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;

    DEBUG(("Enter rasterview__ObservedChanged(0x%lx, 0x%lx, %d)   ras: 0x%lx\n", self, obs, status, ras));

    if (obs == (struct observable *)self->toolset) {
	if (status==observable_OBJECTDESTROYED) {
	    fprintf(stderr, "rasterview: observed toolset destroyed!\n");
	}
	else {
	    int toolnum = rastoolview_GetToolnum(self->toolset);
	    switch (toolnum) {
		case RASTOOL_PAN:
		    if (!Pan(self))
			rasterview_SetPan(self);
		    break;
		case RASTOOL_SELECT:
		    if (!RegionSelect(self))
			rasterview_SetRegionSelect(self);
		    break;
		case RASTOOL_TOUCHUP:
		    if (!TouchUp(self))
			rasterview_SetTouchUp(self);
		    break;
		default:
		    /* tool mode; always call SetToolMode, because we might be changing from one tool to another */
		    rasterview_SetToolMode(self);
		    break;
	    }
	}
    }
    else {
	struct raster *dobj = (struct raster *)obs;
	/* dobj should be the same as ras here; if they're not, things will seriously screw up */

	if (ras == NULL) return;
	pix = raster_GetPix(ras);

	if (status == observable_OBJECTDESTROYED) return;

	switch (status) {
	    case 0:
		self->UpdateWanted = FALSE;
		self->needsFullUpdate = TRUE;
		rasterview_WantUpdate(self,self);
		break;
	    case raster_BITSCHANGED:
	    case pixelimage_DATACHANGED:
		DEBUG(("   BitsChanged\n"));
		if (self->Original != NULL) {
		    rasterimage_Destroy(self->Original);
		    self->Original = NULL; }

		if (rasterimage_GetResized(pix)) {
		    self->PixChanged = self->ViewSelection;
		}
		else {
		    struct rectangle *C = rasterimage_GetChanged(pix);
		    if (NotFullSize(self)) {
			/* we now hack around somebody's stupidity. We have to get self->PixChanged in normal (not zoomed) coordinates; there's no such thing. Thus, we invent it by untransforming PixChanged, union it with C, and retransform it. */
			struct rectangle OPC;
			if (rectangle_IsEmptyRect(&self->PixChanged))
			    rectangle_EmptyRect(&OPC);
			else {
			    rectangle_SetRectSize(&OPC, rectangle_Left(&self->PixChanged) / self->Scale + rectangle_Left (&self->DisplayBoxSelection), rectangle_Top(&self->PixChanged) / self->Scale + rectangle_Top (&self->DisplayBoxSelection), rectangle_Width(&self->PixChanged) / self->Scale, rectangle_Height(&self->PixChanged) / self->Scale);
			}
			rectangle_UnionRect(&OPC, &OPC, C);
			rectangle_SetRectSize(&self->PixChanged, (rectangle_Left(&OPC) - rectangle_Left(&self->DisplayBoxSelection)) *self->Scale, (rectangle_Top(&OPC) - rectangle_Top(&self->DisplayBoxSelection)) *self->Scale, rectangle_Width(&OPC)*self->Scale, rectangle_Height(&OPC)*self->Scale);
			ReflectChangesInExpansion(self, &OPC); 
		    }
		    else {
			rectangle_UnionRect(&self->PixChanged, &self->PixChanged, C); 
		    }
		}
		DEBUG(("PixChanged: (%d,%d,%d,%d)\n", 
		       self->PixChanged.left, self->PixChanged.top, 
		       self->PixChanged.width, self->PixChanged.height));
		if(rectangle_IsEmptyRect(&self->PixChanged))
		    self->PixChanged = self->ViewSelection;
		rasterview_WantUpdate(self, self);
		break;
	    case raster_BOUNDSCHANGED:
		DEBUG(("   BoundsChanged\n"));
		/* The following is in the wrong place. What is needed is to be able to determine if the bounds changed in such a manner that the Original's bounds could be changed to reflect the bounds change. Since the bounds can change such that white space is added to the left/top of the image, resizing the Original is not enough. I am not sure that one CAN determine what to do in the general case. For now... */
		if (self->Original != NULL) {
		    rasterimage_Destroy(self->Original);
		    self->Original = NULL; }
		/* Break is intensionally left out here. */
	    case rasterview_SCALECHANGED:
		DEBUG(("   ScaleChanged\n"));
		self->PixChanged = self->ViewSelection;	/* copy rectangle */
		/* make sure the selection is inside the subraster, but otherwise retain the selection */
		rectangle_IntersectRect(&self->DesiredSelection,
					&self->DesiredSelection,
					&self->ViewSelection);
		self->needsFullUpdate = TRUE;
		rasterview_WantNewSize(self, self);
		/* the new size will force FullUpdate,
		 so we don't call WantUpdate */
		/* XXX WantNewSize does nothing if is an ApplicationLayer */
		if ( ! self->embedded)
		    rasterview_WantUpdate(self, self);
		break;
	    default:
		self->UpdateWanted = FALSE;
		self->needsFullUpdate = TRUE;
		rasterview_WantUpdate(self,self);
		break;
	}
	if (dobj == ras)
	    raster_SetModified(ras);    /* this is a bogus place for this  XXX */

    }
    LEAVE(rasterview__ObservedChanged);
}

void rasterview__ReceiveInputFocus(self)
register struct rasterview  *self;
{
    ENTER(rasterview__ReceiveInputFocus);
    self->Keystate->next = NULL;
    rasterview_PostKeyState(self, self->Keystate);

    self->HasInputFocus = TRUE;
    menulist_SetMask(self->Menus, 0);	/* ensure that menus are posted */
    PostMenus(self);
    rasterview_WantUpdate(self, self);

    LEAVE(rasterview__ReceiveInputFocus);
}

void rasterview__LoseInputFocus(self)
register struct rasterview  *self;
{
    ENTER(rasterview__LoseInputFocus);
    self->HasInputFocus = FALSE;
    rasterview_WantUpdate(self, self);
    /* this is the wrong place? Moved to within the Hit
      if (TouchUp(self)) {
	  struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
	  struct rasterimage *pix;
	  if (ras != NULL && (pix = raster_GetPix(ras)) != NULL)
	      rasterimage_NotifyObservers(pix, raster_BITSCHANGED); } */
    LEAVE(rasterview__LoseInputFocus);
}

void rasterview__WantUpdate(self, requestor)
struct rasterview  *self;
struct view *requestor;
{
    ENTER(rasterview__WantUpdate);

    if (self->inset && self->inset==requestor) {
	if (!self->InsetUpdateWanted) {
	    self->InsetUpdateWanted = TRUE;
	    super_WantUpdate(self, self); 
	}
    }
    else {
	if (!self->UpdateWanted) {
	    self->UpdateWanted = TRUE;
	    super_WantUpdate(self, self); 
	}
    }
    LEAVE(rasterview__WantUpdate);
}

static RedrawRaster(self, type, left, top, width, height)
struct rasterview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct rectangle SRC;	/* image rectangle to redraw */
    struct rectangle DEST;	/* the screen rectangle to be redrawn */
    struct rectangle LB;	/* logical rectangle, inset by BORDER */
    struct rectangle VB;	/* visible rectangle inside border */

    DEBUG(("RedrawRaster(%d (%d,%d,%d,%d))\n", type, left, top, width, height));

    self->UpdateWanted = FALSE;
    if (ras == NULL) return;
    if (self->NeedsCentering) {
	self->NeedsCentering = FALSE;
	if (! self->embedded) {
	    CenterViewSelection(self);
	    PostMenus(self); 
	}
    }
    /* Just in case the window has been resized. */
    ClipScroll(self);
    pix = (NotFullSize(self)) ? self->Expansion : raster_GetPix(ras);
    if (pix == NULL) {
	/* XXX Kludge: If there is no rasterimage, we create one */
	raster_SetPix(ras, pix = rasterimage_Create(91, 91));
	rectangle_SetRectSize(&self->DesiredSelection, 0, 0, 91, 91);
	self->ViewSelection = self->DesiredSelection;
	self->Shrunken = FALSE;
	/* This is to include the execution of the code which would be executed in raster_SetPix which is special cased to NOT inform the upper layers that the raster has changed.  This is a kludge to fix the problem when raster is run with a new file one should be able to quit without the message 'Are you sure you want to Quit?' Posting Menus should be sufficient. */
	PostMenus(self); 
    }
    /* compute LB, the entire allocated rectangle of pixels, but inside by BORDER pixels on all edges. */
    rasterview_GetLogicalBounds(self, &LB);
    InsetRect(&LB, BORDER, BORDER);
    DEBUG(("LB: (%d,%d,%d,%d) Scroll: (%d,%d)\n",
	    rectangle_Left(&LB), rectangle_Top(&LB), rectangle_Width(&LB),
	    rectangle_Height(&LB), self->Xscroll, self->Yscroll));

    /* compute Xoff,Yoff, increments which map a point in the logical view rectangle to a point in the stored raster image. */

    self->Xoff = - rectangle_Left(&LB);
    self->Yoff = - rectangle_Top(&LB);
    if (FullSize(self)) {
	self->Xoff += rectangle_Left(&self->ViewSelection) + self->Xscroll;
	self->Yoff += rectangle_Top(&self->ViewSelection) + self->Yscroll; 
    }
    else {
	if (self->Xscroll < 0)
	    self->Xoff += self->Xscroll;
	if (self->Yscroll < 0)
	    self->Yoff += self->Yscroll;
    }

    if (type == view_FullRedraw) rectangle_EmptyRect(&self->CurSelection);
    else ViewHideHighlight(self);	/* (requires Xoff and Yoff) */

    /* compute VB, the displayed portion of the image.  It is the intersection of the visual rectangle with LB */
    rasterview_GetVisualBounds(self, &VB);
    rectangle_IntersectRect(&VB, &VB, &LB);

    DEBUG(("VB: (%d,%d,%d,%d) Offset: (%d,%d)\n",
	    rectangle_Left(&VB), rectangle_Top(&VB), rectangle_Width(&VB),
	    rectangle_Height(&VB), self->Xoff, self->Yoff));

    /* compute SRC, a rectangle showing which bits of the image need to be replotted.  First we map the parameters given above into the pixel image coordinates, offsetting by the subraster amount.  Then we take the union with the PixChanged rect. */
    if (type == view_FullRedraw)
	SRC = VB;
    else {
	/* set T to pixels changed in view coords */
	struct rectangle T;
	T = self->PixChanged;
	DEBUG(("Changed: (%d,%d,%d,%d)\n",
	       rectangle_Left(&T), rectangle_Top(&T),
	       rectangle_Width(&T), rectangle_Height(&T)));
	OffsetRect(&T, -self->Xoff, -self->Yoff);
	/* set SRC to rectangle arg to Update() */
	rectangle_SetRectSize(&SRC, left, top, width, height);
	/* merge with changes noted in the data (PixChanged) */
	rectangle_UnionRect(&SRC, &SRC, &T);
	/* restrict the SRC to viewable portion */
	rectangle_IntersectRect(&SRC, &SRC, &VB);
    }
    DEBUG(("preSRC: (%d,%d,%d,%d)\n",
	    rectangle_Left(&SRC), rectangle_Top(&SRC),
	    rectangle_Width(&SRC), rectangle_Height(&SRC)));
    /* map SRC into the raster coords */
    OffsetRect(&SRC, self->Xoff, self->Yoff);
    /* and finally, restrict to the portion selected for this view */
    if (FullSize(self))
	rectangle_IntersectRect(&SRC, &SRC, &self->ViewSelection);
    else {
	struct rectangle NS;
	rectangle_SetRectSize(&NS, 0, 0,
			      rectangle_Width(&self->ViewSelection)*self->Scale,
			      rectangle_Height(&self->ViewSelection)*self->Scale);
	rectangle_IntersectRect(&SRC, &SRC, &NS); 
    }

    DEBUG(("SRC: (%d,%d,%d,%d)\n",
	    rectangle_Left(&SRC), rectangle_Top(&SRC),
	    rectangle_Width(&SRC), rectangle_Height(&SRC)));

    /* compute DEST by mapping SRC into the logical rectangle coordinates 
      and then take the intersection with the visible rect. */
    DEST = SRC;
    OffsetRect(&DEST, -self->Xoff, -self->Yoff);
    rectangle_IntersectRect(&DEST, &DEST, &VB);

    /* SRC is the source in the image space.  DEST is the destination, in
      screen coordinates.  DEST also gives the visible width and height. */

    rectangle_GetRectSize(&DEST, &left, &top, &width, &height);

    DEBUG(("DEST: (%d,%d,%d,%d)\n", left, top, width, height));

    /* XXX need to deal with ras->options */

    rasterview_SetTransferMode(self, graphic_WHITE);
    /* There must have been some reason to special case pan mode when embedded but I (pasieka) can not remember why.  Maybe someone will point out a bug.
	if (self->embedded && (Pan(self) || type == view_FullRedraw)
	    ) {
	    rasterview_GetVisualBounds(self, &VB);
	    InsetRect(&VB, BORDER, BORDER);
	    rasterview_FillRect(self, &VB, self->WhitePattern);
	}
	else */
    if (type == view_FullRedraw) {
	rasterview_GetVisualBounds(self, &VB);
	if (self->embedded) InsetRect(&VB, BORDER, BORDER);
	rasterview_FillRect(self, &VB, self->WhitePattern);
    }
    else if (width > 0  &&  height > 0) {
	/* erase the DEST because only the black bits get painted */
	rasterview_FillRect(self, &DEST, self->WhitePattern);
    }

    /* XXX Is a TransferMode needed with WritePixImage ??? */
    rasterview_SetTransferMode(self, graphic_COPY);
    if (width > 0  &&  height > 0) {
	struct graphic *G = rasterview_GetDrawable(self);
	graphic_WritePixImage(G, left, top,
			      pix, rectangle_Left(&SRC), rectangle_Top(&SRC),
			      width, height);

	/*if Zoomed draw the display box in the right place */
	if (NotFullSize(self) && ! self->DisplayBoxHidden)
	    DisplayBoxWritePixImage(self, G);
    }

    if (RegionSelect(self) || (Tool(self) && rastoolview_WantSelectionHighlighted(self->toolset)))
	CorrectHighlight(self);
    if (Pan(self)) {
	DrawPanHighlight(self, graphic_BLACK); }

    if (self->embedded) {
	rasterview_GetVisualBounds(self, &VB);
	InsetRect(&VB, BORDER, BORDER);
	DrawHighlightScreenCoordinates(self, rasterview_GetDrawable(self), VB,
				       graphic_BLACK, graphic_WHITE);
    }

    rectangle_EmptyRect(&self->PixChanged);
    self->needsFullUpdate = FALSE;

    LEAVE(RedrawRaster);
}

void rasterview__FullUpdate(self, type, left, top, width, height)
register struct rasterview  *self;
register enum view_UpdateType  type;
register long  left, top, width, height;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rectangle VB;
    ENTER(rasterview__FullUpdate);

    rasterview_GetVisualBounds(self, &VB);
    if((type == view_Remove) || (type == view_MoveNoRedraw)) {
	if(type == view_Remove) {
	    self->OnScreen = FALSE;
	    rasterview_RetractCursor(self, self->Cursor[self->Mode]);
	}
	else if(type == view_MoveNoRedraw) {
	    rasterview_RetractCursor(self, self->Cursor[self->Mode]);
	    rasterview_PostCursor(self, &VB, self->Cursor[self->Mode]);
	}
	return;
    }
    if(IsEmptyRect(&VB)) {
	return;
    }

    /* fixes bug that the view selection is not updated when raster_Read is called. */
    if (rectangle_IsEmptyRect(&self->ViewSelection))
	self->ViewSelection = self->DesiredSelection = ras->subraster;

#if 0
    menulist_SetMask(self->Menus, 0);	/* ensure that menus are posted */
    PostMenus(self);
#endif
    self->OnScreen = TRUE;
    /* 
      XXX this code sets ReadOnly in the 
      rasterimage if the parent is 'messages'
	  */
    if (! self->CheckedParent) {
	struct rasterimage *pix = raster_GetPix(ras);
	register struct view *v;
	self->CheckedParent = TRUE;
	for (v = (struct view *)self; v != NULL; v = v->parent) {
	    register char *nm = class_GetTypeName(v);
	    DEBUG(("parent: %s\n", nm));
	    if (strcmp(nm, "messages") == 0) {
		self->InMessages = TRUE;
		if (pix != NULL)
		    rasterimage_SetRO(pix, ras->readOnly = TRUE);
		break; } }
	DEBUG(("Done Checking parents.\n")); }
    if (type == view_FullRedraw || type == view_LastPartialRedraw) {
	/* must recompute graphics info because image
	 may be on different display hardware */
	self->WhitePattern = rasterview_WhitePattern(self);
	self->BlackPattern = rasterview_BlackPattern(self);
	self->GreyPattern = rasterview_GrayPattern(self, 1, 2);

	if (self->Cursor[0] == NULL) {
	    struct fontdesc *fd;
	    long junk;
	    DEBUG(("Creating Cursors\n"));
	    self->Cursor[RegionSelectMode] = cursor_Create(self);
	    self->Cursor[TouchUpMode] = cursor_Create(self);
	    self->Cursor[PanMode] = cursor_Create(self);
	    self->Cursor[WaitMode] = cursor_Create(self);
	    self->Cursor[ToolMode] = cursor_Create(self);
	    fd = fontdesc_Create("icon", 0, 12);
	    cursor_SetStandard(self->Cursor[RegionSelectMode], Cursor_Gunsight);
	    if (fontdesc_StringSize(fd, rasterview_GetDrawable(self), "!", &junk, &junk)
		> 5)
		cursor_SetGlyph(self->Cursor[TouchUpMode], fd, '!');
	    else
		cursor_SetStandard(self->Cursor[TouchUpMode], Cursor_Arrow);
	    cursor_SetStandard(self->Cursor[PanMode], Cursor_CrossHairs); 
	    cursor_SetStandard(self->Cursor[ToolMode], Cursor_CrossHairs); 
	    cursor_SetStandard(self->Cursor[WaitMode], Cursor_Wait);
	}
	/* reset the cursor */
	DEBUG(("Finished Posting Cursors\n")); fflush(stdout);
	rasterview_PostCursor(self, &VB, self->Cursor[self->Mode]); }

    if (NotFullSize(self)) {
	DEBUG(("Call ZoomToVisualBounds\n"));
	ZoomToVisualBounds(self, self->Xscroll/self->Scale, self->Yscroll/self->Scale); }

    DEBUG(("Call RedrawRaster\n"));
    RedrawRaster(self, type, left, top, width, height);

    if (self->inset && FullSize(self)) {
	rasterview_EraseRectSize(self, self->InsetBox.left-self->Xoff, self->InsetBox.top-self->Yoff, self->InsetBox.width, self->InsetBox.height); 
	view_FullUpdate(self->inset, view_FullRedraw, 0, 0, -1, -1);
    }
    LEAVE(rasterview__FullUpdate);
}

void rasterview__Update(self)
register struct rasterview *self;
{
    ENTER(rasterview__Update);
    if (! self->OnScreen) return;

    PostMenus(self);

    if (self->UpdateWanted) {
	if (self->needsFullUpdate)
	    RedrawRaster(self, view_FullRedraw, 0, 0, -1, -1);
	else
	    RedrawRaster(self, view_LastPartialRedraw, 0, 0, -1, -1);
    }

    if (self->inset && FullSize(self)) {
	view_InsertViewSize(self->inset, self, self->InsetBox.left-self->Xoff, self->InsetBox.top-self->Yoff, self->InsetBox.width, self->InsetBox.height);
	view_FullUpdate(self->inset, view_FullRedraw, 0, 0, -1, -1);
	self->InsetUpdateWanted = FALSE;
    }
    LEAVE(rasterview__Update);
}

enum view_DSattributes rasterview__DesiredSize(self, width, height, pass,
						desiredWidth, desiredHeight) 
register struct rasterview *self;
long width;
long height;
enum view_DSpass pass;
long *desiredWidth;
long *desiredHeight;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    register struct rasterimage *pix;
    ENTER(rasterview__DesiredSize);
    pix = raster_GetPix(ras);
    if (pix == NULL) {
	/* XXX Kludge: If there is no rasterimage, we create one */
	raster_SetPix(ras, pix = rasterimage_Create(91, 91));
	rectangle_SetRectSize(&self->DesiredSelection, 0, 0, 91, 91);
	self->Shrunken = FALSE; }
    if (ras != NULL && IsNotEmptyRect(&ras->subraster)) {
	/* leave BORDER pixels all around the image */
	*desiredWidth = rectangle_Width(&ras->subraster) + (2*BORDER);
	*desiredHeight = rectangle_Height(&ras->subraster) + (2*BORDER); }
    else {
	*desiredWidth = 95;
	*desiredHeight = 95; }
    if (self->Shrunken && *desiredHeight > 20) 
	*desiredHeight = 20;

    DEBUG(("Leave DesiredSize: %d x %d\n", *desiredWidth, *desiredHeight));

    return view_Fixed;
}

static char *MouseEvent[] = {
    "No Mouse Event",
    "Left Down",
    "Left Up",
    "Left Movement",
    "Right Down",
    "Right Up",
    "Right Movement" };
enum { DragCorner, DragTop, DragBottom, DragLeft, DragRight };

struct view * rasterview__Hit(self, action, x, y, num_clicks)
struct rasterview  *self;
enum view_MouseAction  action;
long  x, y, num_clicks;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct point truepoint;

    DEBUG(("rasterview_Hit %s at (%d, %d)\n", MouseEvent[(long)action], x, y));
    if (ras == NULL || action == view_NoMouseEvent)
	return (struct view *)self;
    if (! self->OnScreen)
	return NULL;

    point_SetPt(&truepoint, x+self->Xoff, y+self->Yoff);

    /* Ignore all hits until next up transition. */
    if (self->IgnoreUntilNextUpTransition) {
	if (action == view_LeftUp || action == view_RightUp) {
	    self->IgnoreUntilNextUpTransition = FALSE;
	    return NULL; }
	else
	    DEBUG(("Ignoring Mouse Movements until up transition.\n")); }

    else if (self->Shrunken) {
	self->Shrunken = FALSE;
	self->needsFullUpdate = TRUE;
	rasterview_WantNewSize(self, self);
	/* the new size will force FullUpdate, so we don't call WantUpdate */
	/* XXX WantNewSize does nothing if is an ApplicationLayer */
	if (! self->embedded)
	    rasterview_WantUpdate(self, self);
	/* This covers the case when the raster is shrunken and does not have the input focus. */
	if (! self->HasInputFocus)
	    rasterview_WantInputFocus(self, self);
	self->IgnoreUntilNextUpTransition = TRUE; }

    /* this tests for clicking in the overlaid inset */
    else if (self->inset 
	      && (action==view_LeftDown || action==view_RightDown) 
	      && (FullSize(self))
	      && rectangle_IsPtInRect(&truepoint, &self->InsetBox)) {
	long sx = view_EnclosedXToLocalX(self->inset, x);
	long sy = view_EnclosedYToLocalY(self->inset, y);
	self->DesiredSelection = self->InsetBox;
	return view_Hit(self->inset, action, sx, sy, num_clicks); }

    /* This covers the case when the raster is not shrunken and one just wants the input focus. */
    else if (! self->HasInputFocus) {
	rasterview_WantInputFocus(self, self);
	self->IgnoreUntilNextUpTransition = TRUE; }

    else if (self->MovingDisplayBox) {
	if (action == view_LeftUp) {
	    FinishMovingDisplayBox(self, x, y);
	    return NULL; }
	else
	    DEBUG(("Still Moving Display Box\n")); }

    /* This tests for clicking left within the Display Box. */
    else if (! self->DisplayBoxHidden
	      && NotFullSize(self) && action == view_LeftDown
	      && x > rectangle_Left(&self->DisplayBox) - 2*BORDER
	      && x < rectangle_Right(&self->DisplayBox) + 2*BORDER
	      && y > rectangle_Top(&self->DisplayBox) - 2*BORDER
	      && y < rectangle_Bottom(&self->DisplayBox) + 2*BORDER) {
	/* Begin moving the Display Box. */
	MoveDisplayBoxCommand(self, -1); }

    /* If in Pan Mode then do functions of panning. */
    else if (Pan(self)) {
	if (action == view_LeftDown || action == view_RightDown)
	    StartPanning(self, x, y);
	else if (action == view_LeftUp || action == view_RightUp) {
	    FinishPanning(self, x, y);
	    return NULL; }
	else
	    ContinuePanning(self, x, y); }

    /* At this point the hit is either for TouchUp or Region Select, or for one of the toolsetv tools. */
    else {
	struct rectangle SR;

	/* map coordinates to full size raster space. */
	x = truepoint.x;
	y = truepoint.y;
	if (NotFullSize(self)) {
	    x = rectangle_Left(&self->DisplayBoxSelection) + x/self->Scale;
	    y = rectangle_Top(&self->DisplayBoxSelection) + y/self->Scale;
	    SR = self->DisplayBoxSelection; }
	else
	    SR = self->ViewSelection;

	/* note that toolset hits are *not* constrained to be within the raster */
	if (self->toolset) {
	    int toolnum = rastoolview_GetToolnum(self->toolset);
	    if (toolnum!=RASTOOL_PAN && toolnum!=RASTOOL_SELECT && toolnum!=RASTOOL_TOUCHUP) {
		void (*tproc)();
		if (self->ShowCoords) {
		    static char cb[20];
		    sprintf(cb, "(%d,%d)", x, y);
		    message_DisplayString(self, 0, cb);
		}
		tproc = rastoolview_GetToolProc(self->toolset);
		if (tproc)
		    (*tproc) (self->toolset, action, x, y, num_clicks);
		return (struct view *)self;
	    }
	}

	/* Note that coordinates are constrained to refer to actual pixels
	 (left+width and top+height are the addresses of pixels
	  just outside the raster) */
	if (x < rectangle_Left(&SR)) x = rectangle_Left(&SR);
	else if (x > rectangle_Left(&SR)+rectangle_Width(&SR)-1)
	    x = rectangle_Left(&SR)+rectangle_Width(&SR)-1;
	if (y < rectangle_Top(&SR)) y = rectangle_Top(&SR);
	else if (y > rectangle_Top(&SR)+rectangle_Height(&SR)-1)
	    y = rectangle_Top(&SR)+rectangle_Height(&SR)-1;

	if (TouchUp(self)) {
	    if (self->ShowCoords) {
		static char cb[20];
		sprintf(cb, "(%d,%d)", x, y);
		message_DisplayString(self, 0, cb);
	    }

	    if (action == view_LeftDown)
		SetPixel(self, x, y, TRUE);
	    else if (action == view_LeftMovement)
		DrawLineTo(self, x, y, TRUE);
	    else if (action == view_RightDown)
		SetPixel(self, x, y, FALSE);
	    else if (action == view_RightMovement)
		DrawLineTo(self, x, y, FALSE);
	    else {
		struct rasterimage *pix;
		if ((pix = raster_GetPix(ras)) != NULL) {
		    DEBUG(("Notify Observers of Touchup\n"));
		    rasterimage_NotifyObservers(pix, raster_BITSCHANGED); } }
	    self->TouchUpX = x;
	    self->TouchUpY = y; }
	else {
	    long l, t, w, h;	/* top, left, width, height */
	    long farx, fary;

	    /* while mouse is down, have raster cursor throughout the window */
	    if (action == view_RightDown || action == view_LeftDown) {
		im_SetWindowCursor(rasterview_GetIM(self), self->Cursor[self->Mode]);
	    }
	    else if (action == view_RightUp || action == view_LeftUp)
		im_SetWindowCursor(rasterview_GetIM(self), NULL);

	    if (action == view_LeftUp) {
		if (num_clicks == 2) {
		    /* Double Left Click selects entire raster. */
		    rectangle_GetRectSize(&SR, &l, &t, &w, &h);
		    x = l;
		    y = t;
		    self->Xdown = x + w - 1;
		    self->Ydown = y + h - 1; } }
	    else if (action == view_LeftDown) {
		/* Single Left Click sets the desired selection to the single point under the mouse. */
		self->Xdown = x;
		self->Ydown = y; }
	    else if (action == view_RightDown) {
		/* Drag either a corner or an edge while right down. */
		struct rectangle *R = &self->DesiredSelection;
		long l, t, w, h;
		long r, b;
		rectangle_GetRectSize(R, &l, &t, &w, &h);
		r = l+w;
		b = t+h;
		if ((x < l || x > r) && y > t && y < b) {
		    if (y <= t+h/2)
			self->DraggingEdge = (int)DragTop;
		    else
			self->DraggingEdge = (int)DragBottom; }
		else if ((y < t || y > b) && x > l && x < r) {
		    if (x <= l+w/2)
			self->DraggingEdge = (int)DragLeft;
		    else 
			self->DraggingEdge = (int)DragRight; }
		else 
		    self->DraggingEdge = (int)DragCorner;
		/* set (Xdown, Ydown) to the furthest corner of old selection */
		farx = (x < l  ||  (x < r  &&  x-l < r-x)) ? r-1 : l;
		fary = (y < t  ||  (y < b  &&  y-t < b-y)) ? b-1 : t;
		self->Xdown = farx;
		self->Ydown = fary; }

	    switch (self->DraggingEdge) {
		case DragCorner:
		    break;
		case DragTop:
		    y = rectangle_Top(&self->DesiredSelection);
		    break;
		case DragBottom:
		    y = rectangle_Bottom(&self->DesiredSelection) - 1;
		    break;
		case DragLeft:
		    x = rectangle_Left(&self->DesiredSelection);
		    break;
		case DragRight:
		    x = rectangle_Right(&self->DesiredSelection) - 1; }

	    /* set DesiredSelection to have corners (x,y) and (self->Xdown,self->Ydown) */
	    farx = self->Xdown;
	    fary = self->Ydown;
	    l = (x<farx) ? x : farx;
	    t = (y<fary) ? y : fary;
	    w = x - farx;   if (w<0) w = -w;   w++;
	    h = y - fary;   if (h<0) h = -h;   h++;
	    rectangle_SetRectSize(&self->DesiredSelection, l, t, w, h);

	    if (self->ShowCoords) {
		static char cb[32];
		sprintf(cb, "(%d by %d) at (%d,%d)", w, h, l, t);
		message_DisplayString(self, 10, cb);
	    }

	    /* if rightup or leftup then update the scroll bars. */
	    if (action == view_LeftUp  ||  action == view_RightUp) {
		self->DraggingEdge = 0;
		self->needsFullUpdate=FALSE;
		rasterview_WantUpdate(self, self); }
	    CorrectHighlight(self);
	} }

    if (action == view_LeftUp  ||  action == view_RightUp)
	PostMenus(self);

    /*  XXX when we have drawing operations,
      the first mouse hit without a self->dataobject->pix
      must create the pix */

    LEAVE(rasterview__Hit);
    return (struct view *)self;		/* where to send subsequent hits */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *
 *	Printing
 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#ifdef _IBMR2
#define NO_FLOATS_IN_PRINTF_ENV
#endif

static char *PSheader[] = {
#ifndef NO_FLOATS_IN_PRINTF_ENV
	"%s  /width %d def  /height %d def /xScale %0.4f def /yScale %0.4f def\n",
#endif
	"%s     width xScale mul height yScale mul scale\n",
	"%s     /picstr width 7 add 8 idiv string def\n",
	"%s        width height 1\n",
	"%s        [width    0    0    height neg    0    height]\n",
	"%s        {currentfile picstr readhexstring pop}\n",
	"%s     image\n",
	NULL };

void rasterview__Print(self, file, processor, format, toplevel)
register struct rasterview	*self;
register FILE   	*file;
register char	*processor;
register char	*format;
register boolean	toplevel;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    register struct rasterimage *pix;
    char **psx;
    long row;
    short buf[300];
    long left, top, width, height;
    long wpts, hpts;  /* image dimensions in points */
    float xdscale, ydscale;
    char *prefix;

    ENTER(rasterview__Print);

    if (ras == NULL || (pix=raster_GetPix(ras)) == NULL) return;

    rectangle_GetRectSize(&self->ViewSelection, &left, &top, &width, &height);

    /* compute size in points and scaling as float values 
      add half unit to numerator to achieve rounding */
    wpts = (width * ras->xScale + (raster_UNITSCALE/2)) / raster_UNITSCALE;
    hpts = (height * ras->yScale + (raster_UNITSCALE/2)) / raster_UNITSCALE;

    if (ras->xScale == raster_UNITSCALE/2 
	 &&  ras->yScale == raster_UNITSCALE/2) {
	/* restrict to 6" by 9"  (1 inch = 72 points) */
	if (wpts > 6 * 72) { 
	    ras->xScale = ras->yScale =
	      6 * 72 * raster_UNITSCALE / width;
	    hpts = (height * ras->yScale + (raster_UNITSCALE/2)) 
	      / raster_UNITSCALE; }
	if (hpts > 9 * 72) 
	    ras->xScale = ras->yScale = 9 * 72 * raster_UNITSCALE / height;
	wpts = (width * ras->xScale + (raster_UNITSCALE/2)) / raster_UNITSCALE;
	hpts = (height * ras->yScale + (raster_UNITSCALE/2)) / raster_UNITSCALE; }
    xdscale = ((float) ras->xScale) / raster_UNITSCALE;
    ydscale = ((float) ras->yScale) / raster_UNITSCALE;

    if (strcmp(processor, "troff") == 0) {
	/* output to troff */
	if (toplevel)
	    /* take care of initial troff stream */
	    texttroff_BeginDoc(file);
	/*  Put macro to interface to postscript */
	texttroff_BeginPS(file, wpts, hpts);
	prefix = "\\!  ";
    }
    else if (strcmp(format, "troff") == 0)
	prefix = "\\!  ";
    else prefix = "";

#ifdef NO_FLOATS_IN_PRINTF_ENV
    fprintf(file, prefix);
    fprintf(file, "  /width %d def  /height %d def /xScale ", width, height);
    fprintf(file, "%d.%.4d def /yScale %d.%.4d def\n", (long)xdscale, (long)((xdscale-(long)xdscale+.00005)*10000), (long)ydscale, (long)((ydscale-(long)ydscale+.00005)*10000));
#endif
    
    for (psx = PSheader; *psx; psx++)
#ifndef NO_FLOATS_IN_PRINTF_ENV
	fprintf(file, *psx, prefix, width, height, xdscale, ydscale);
#else
	fprintf(file, *psx, prefix);
#endif

    /* generate bits of image
      XXX punt rotation and reflection  */
    for (row = top;  row < top+height; row++) {
	rasterimage_GetRow(pix, left, row, width, buf); 
	fprintf(file, "%s  ", prefix);
	heximage_WriteRow(file, buf, width, ! (ras->options & raster_INVERT)); }

    if (strcmp(processor, "troff") == 0) {
	texttroff_EndPS(file, wpts, hpts);
	if (toplevel)
	    texttroff_EndDoc(file); }

    LEAVE(rasterview__Print);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *
 *	Scrolling
 *	
 *	The coordinate space defined for the scrollbars by getinfo uses the 
 *	self->dataobject->subraster.  Its upper left is 0,0.  The ends of the scroll bar
 *	correspond to the width and height of the subraster.
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	y_getinfo(), y_setframe(), x_getinfo(), x_setframe();
static long	y_whatisat(), x_whatisat();
static struct scrollfns	vertical_scroll_interface =
		{y_getinfo, y_setframe, NULL, y_whatisat};
static struct scrollfns	horizontal_scroll_interface =
		{x_getinfo, x_setframe, NULL, x_whatisat};

static void x_getinfo(self, total, seen, dot)
register struct rasterview  *self;
register struct range  *total, *seen, *dot;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) {
	total->beg = seen->beg = seen->end = dot->beg = dot->end = 0;
	total->end = 1;
	return; }

    dot->beg = rectangle_Left(&self->DesiredSelection);
    dot->end = rectangle_Right(&self->DesiredSelection);
    if (NotFullSize(self)) {
	total->beg = rectangle_Left(&self->ViewSelection)*self->Scale;
	total->end = rectangle_Right(&self->ViewSelection)*self->Scale;
	seen->beg = rectangle_Left(&self->DisplayBoxSelection)*self->Scale;
	seen->end = rectangle_Right(&self->DisplayBoxSelection)*self->Scale;
    	dot->beg *= self->Scale;
	dot->end *= self->Scale;
    }
    else {
	total->beg = rectangle_Left(&self->ViewSelection);
	total->end = rectangle_Right(&self->ViewSelection);
	seen->beg = rectangle_Left(&self->ViewSelection) + self->Xscroll;
	seen->end = seen->beg + rasterview_GetLogicalWidth(self) - (2*BORDER);
    }

    if (seen->beg < total->beg) seen->beg = total->beg;
    if (seen->end > total->end) seen->end = total->end;
    if (dot->end > total->end) dot->end = total->end;
    DEBUG(("X info => total (%d, %d) seen (%d, %d) dot (%d, %d) Scr: %d\n", 
	    total->beg, total->end, seen->beg, seen->end, dot->beg, dot->end,
	    self->Xscroll));
}

static long x_whatisat(self, coordinate, outof)
register struct rasterview  *self;
register long  coordinate, outof;
{
    register long  value;
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) return 0;
    if (NotFullSize(self))
	value = (coordinate - (coordinate % self->Scale)) +
	  rectangle_Left(&self->DisplayBoxSelection)*self->Scale +
	  ((self->Xscroll < 0) ? self->Xscroll : 0);
    else
	value = coordinate + rectangle_Left(&self->ViewSelection) + self->Xscroll;
    DEBUG(("X what (%d, %d) => %d\n", coordinate, outof, value));
    return value;
}

static void  x_setframe(self, position, coordinate, outof) 
register struct rasterview *self;
int  position;
long  coordinate, outof;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    long oldscroll = self->Xscroll;
    if (ras == NULL) return;

    DEBUG(("Enter x_setframe(%d,%d,%d)\n", position, coordinate, outof));

    if (coordinate != 0 && position == rectangle_Left(&self->ViewSelection))
	position = self->Xscroll + rectangle_Left(&self->ViewSelection);
    if (Cropped(self))
	position -= rectangle_Left(&self->ViewSelection);

    self->Xscroll = position - coordinate;
    ClipScroll(self);

    if (self->Xscroll != oldscroll) {
	if (NotFullSize(self)) {
	    rectangle_EmptyRect(&self->DisplayBoxSelection);
	    UpdateZoomedSelection(self,
				  self->Xscroll/self->Scale,
				  self->Yscroll/self->Scale); }
	self->needsFullUpdate = TRUE;
	rasterview_WantUpdate(self, self); }

    DEBUG(("X set (%d, %d, %d) => %d Old: %d\n", 
	    position, coordinate, outof, self->Xscroll, oldscroll));
    DEBUG(("Scroll: (%d,%d)\n", self->Xscroll, self->Yscroll));
}

static void y_getinfo(self, total, seen, dot)
register struct rasterview  *self;
register struct range  *total, *seen, *dot;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) {
	total->beg = seen->beg = seen->end = dot->beg = dot->end = 0;
	total->end = 1;
	return; }

    dot->beg = rectangle_Top(&self->DesiredSelection);
    dot->end = rectangle_Bottom(&self->DesiredSelection);
    if (NotFullSize(self)) {
	total->beg = rectangle_Top(&self->ViewSelection)*self->Scale;
	total->end = rectangle_Bottom(&self->ViewSelection)*self->Scale;
	seen->beg = rectangle_Top(&self->DisplayBoxSelection)*self->Scale;
	seen->end = rectangle_Bottom(&self->DisplayBoxSelection)*self->Scale;
    	dot->beg *= self->Scale;
	dot->end *= self->Scale;
    }
    else {
	total->beg = rectangle_Top(&self->ViewSelection);
	total->end = rectangle_Bottom(&self->ViewSelection);
	seen->beg = rectangle_Top(&self->ViewSelection) + self->Yscroll;
	seen->end = seen->beg + rasterview_GetLogicalHeight(self) - (2*BORDER);
    }

    if (seen->beg < total->beg) seen->beg = total->beg;
    if (seen->end > total->end) seen->end = total->end;
    if (dot->end > total->end) dot->end = total->end;
    DEBUG(("Y info => total (%d, %d) seen (%d, %d) dot (%d, %d) Scr: %d\n", 
	    total->beg, total->end, seen->beg, seen->end, dot->beg, dot->end,
	    self->Yscroll));
}

static long y_whatisat(self, coordinate, outof)
register struct rasterview *self;
register long  coordinate, outof;
{
    register long  value;
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) return 0;
    if (NotFullSize(self))
	value = (coordinate - (coordinate % self->Scale)) +
	  rectangle_Top(&self->DisplayBoxSelection)*self->Scale +
	  ((self->Yscroll < 0) ? self->Yscroll : 0);
    else
	value = coordinate + rectangle_Top(&self->ViewSelection) + self->Yscroll;
    DEBUG(("Y what (%d, %d) => %d\n", coordinate, outof, value));
    return value;
}

static void  y_setframe(self, position, coordinate, outof)
register struct rasterview *self;
int  position;
long  coordinate, outof;
{
    register struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    long oldscroll = self->Yscroll;
    if (ras == NULL) return;

    DEBUG(("Enter y_setframe(%d,%d,%d)\n", position, coordinate, outof));

    if (coordinate != 0 && position == rectangle_Top(&self->ViewSelection))
	position = self->Yscroll + rectangle_Top(&self->ViewSelection);
    if (Cropped(self))
	position -= rectangle_Top(&self->ViewSelection);

    self->Yscroll = position - coordinate;
    ClipScroll(self);

    if (self->Yscroll != oldscroll) {
	if (NotFullSize(self)) {
	    rectangle_EmptyRect(&self->DisplayBoxSelection);
	    UpdateZoomedSelection(self,
				  self->Xscroll/self->Scale,
				  self->Yscroll/self->Scale); }
	self->needsFullUpdate = TRUE;
	rasterview_WantUpdate(self, self); }

    DEBUG(("Y set (%d, %d, %d) => %d Old: %d\n",
	    position, coordinate, outof, self->Yscroll, oldscroll));
    DEBUG(("Scroll: (%d,%d)\n", self->Xscroll, self->Yscroll));
}

struct scrollfns * rasterview__GetInterface(self, interface_name)
register struct rasterview *self;
register char  *interface_name;
{
    register struct scrollfns *interface;
    DEBUG(("GetInterface(%s)\n", interface_name));
    if (strcmp(interface_name, "scroll,vertical") == 0)
	interface = &vertical_scroll_interface;
    else if (strcmp(interface_name, "scroll,horizontal") == 0)
	interface = &horizontal_scroll_interface;
    else
	interface = NULL;
    return interface;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *
 *	Supporting procedures
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

boolean FindFrameHelp(frame, im)
struct frame *frame;
struct im *im;
{
    return (frame_GetIM(frame) == im);
}

void CurrentDirectory(self, f) 
struct rasterview *self; 
char *f; 
{ 
    struct frame *frame = frame_Enumerate(FindFrameHelp, rasterview_GetIM(self)); 
    struct buffer *buffer = frame_GetBuffer(frame); 
    char *filename; 
    if (frame != NULL && buffer != NULL) { 
	filename = buffer_GetFilename(buffer); 
	if (filename && strlen(filename) != 0) { 
	    char *slash; 
	    strcpy(f, buffer_GetFilename(buffer)); 
	    DEBUG(("CurrrentFilename: '%s'\n", f)); 
	    slash = (char*) rindex(f, '/'); 
	    if (slash != NULL) 
		slash[1] = '\0'; } 
	else { 
	    im_GetDirectory(f); 
	    strcat(f, "/"); } } 
    else { 
	im_GetDirectory(f); 
	strcat(f, "/"); } 

    DEBUG(("Return value: '%s'\n", f)); 
} 

void CenterViewSelection(self)
struct rasterview *self;
{
    struct rectangle VB;
    rasterview_GetVisualBounds(self, &VB);
    DEBUG(("Centering VB: (%d,%d,%d,%d)\n",
	    rectangle_Left(&VB), rectangle_Top(&VB),
	    rectangle_Width(&VB), rectangle_Height(&VB)));
    self->Xscroll =
      (rectangle_Width(&self->ViewSelection)*self->Scale - rectangle_Width(&VB))/2;
    self->Yscroll =
      (rectangle_Height(&self->ViewSelection)*self->Scale - rectangle_Height(&VB))/2;
}

/* selection
	The selected subrectangle is highlighted with a two pixel border,
	the inner pixel is white and the outer is black.
	
	To make room for this, the requested space for the image
	leaves room for two pixels of white around the image on all sides.
*/

/* ViewHideHighlight(self)
	repaints the selection area with the correct bits.
	paints white if painting in the two pixel border
	sets CurSelection to empty,
		but leaves DesiredSelection alone

	Strategy: paint the two pixel borders as though they did not overlap 
	the two pixel white space surrounding the image.
	Then test for overlap and, if any, repaint the entire 
	surrounding whitespace.
*/

void ViewHideHighlight(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct graphic *G = rasterview_GetDrawable(self);
    long Xoff = self->Xoff, Yoff = self->Yoff;
    long clipw, cliph;

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;
    if (IsEmptyRect(&self->CurSelection)) return;
    if (NotFullSize(self)) pix = self->Expansion;

    clipw = rasterimage_GetWidth(pix);
    cliph = rasterimage_GetHeight(pix);

    if (self->DisplayBoxOverlapped)
	DisplayBoxHideOverlappingHighlight(self, G, pix);
    else {
	long l, t, w, h;			/* dimensions of selection */
	long leftThick, rightThick;		/* thickness of line to repair */
	long topThick, botThick;
	long vsl, vst, vsw, vsh;		/* dimensions of self->ViewSelection. */
	struct rectangle CS;

	CS = self->CurSelection;

	/* restore bits from image */

	if (NotFullSize(self)) {
	    struct rectangle R;
	    R = self->DisplayBoxSelection;
	    rectangle_IntersectRect(&R, &R, &CS);
	    if (IsEmptyRect(&R)) return;
	    l = (rectangle_Left(&R) - rectangle_Left(&self->DisplayBoxSelection)) * self->Scale;
	    t = (rectangle_Top(&R) - rectangle_Top(&self->DisplayBoxSelection)) * self->Scale;
	    w = rectangle_Width(&R)*self->Scale;
	    h = rectangle_Height(&R)*self->Scale;
	    vsl = vst = 0;
	    vsw = rectangle_Width(&self->DisplayBoxSelection)*self->Scale;
	    vsh = rectangle_Height(&self->DisplayBoxSelection)*self->Scale;
	    rectangle_SetRectSize(&CS, l, t, w, h);
	}
	else {
	    rectangle_GetRectSize(&CS, &l, &t, &w, &h);
	    rectangle_GetRectSize(&self->ViewSelection, &vsl, &vst, &vsw, &vsh); }

	/* we have to do white first, because writepiximage is bogus (because defined transfer modes are bogus). Draw one white line overlapping the black border */
	DrawHighlight(self, G, CS, graphic_WHITE, -1);

	DEBUG(("CurSel: (%d,%d,%d,%d)\n", l, t, w, h));
	DEBUG(("VSel: (%d,%d,%d,%d)\n", vsl, vst, vsw, vsh));

	rasterview_SetTransferMode(self, graphic_COPY);

	/* repair left edge */
	leftThick =  (l-vsl <= 1) ? l - vsl : 2;
	if (leftThick) 
	    /* repaint left edge with pixels (do not do top and bottom corners) */
	    ClipAndWritePixImage(clipw, cliph,
				 G, l-leftThick-Xoff, t-Yoff, pix,
				 l-leftThick, t, leftThick, h);

	/* repair right edge */
	rightThick = (vsl+vsw) - (l+w);
	if (rightThick > 2) rightThick = 2;
	if (rightThick) 
	    /* redraw pixels of right edge (excluding corners) */
	    ClipAndWritePixImage(clipw, cliph,
				 G, l+w-Xoff, t-Yoff, pix, l+w, t, rightThick, h);

	/* repair top edge */
	topThick =  (t-vst <= 1) ? t-vst : 2;
	if (topThick) 
	    /* redraw pixels of top edge (including corners) */
	    ClipAndWritePixImage(clipw, cliph,
				 G, l-leftThick-Xoff, t-topThick-Yoff, pix,
				 l-leftThick, t-topThick,
				 w+leftThick+rightThick, topThick);

	/* repair bottom edge */
	botThick = (vst+vsh) - (t+h);
	if (botThick > 2) botThick = 2;
	if (botThick) 
	    /* redraw pixels of bottom edge (including corners) */
	    ClipAndWritePixImage(clipw, cliph,
				 G, l-leftThick-Xoff, t+h-Yoff, pix,
				 l-leftThick, t+h,
				 w+leftThick+rightThick, botThick);
    }
    if (NotFullSize(self))
	DisplayBoxHideHighlight(self, G);

    rectangle_EmptyRect(&self->CurSelection);

}

/* CorrectHighlight(self) 
	checks the selection highlighting parameters, CurSelection and DesiredSelection.
	If they differ, hides the old highlight and draws the new

	Knows about input focus and uses a grey highlight if we haven't got it
*/
void CorrectHighlight(self) 
struct rasterview *self;
{
    struct graphic *G = rasterview_GetDrawable(self);
    struct rectangle *CS = &self->CurSelection;
    struct rectangle *DS = &self->DesiredSelection;

    if (rectangle_IsEqualRect(CS, DS)
	 && self->HighlightIsGrey == self->HasInputFocus)
	/* highlight is correct */
	return;

    if (IsNotEmptyRect(&self->CurSelection))
	ViewHideHighlight(self);	/* remove the old highlight */

    self->HighlightIsGrey = self->HasInputFocus;
    *CS = *DS;
    if (IsEmptyRect(CS)) {
	/* There is no current selection.  This happens when there is no raster.  To show the user where we are, draw a rect.  Use Visual Bounds. */
	struct rectangle VB;
	rasterview_GetVisualBounds(self, &VB);
	rasterview_SetTransferMode(self, graphic_BLACK);
	SetWidthRect(&VB, rectangle_Width(&VB)-1);
	SetHeightRect(&VB, rectangle_Height(&VB)-1);
	graphic_DrawRect(G, &VB);
	return; }

    if (! TouchUp(self)) {
	if (self->HasInputFocus) {
	    /* draw black and white lines */
	    if (NotFullSize(self)) {
		DrawHighlightBehindDisplayBox(self, G, 0);
		DisplayBoxDrawHighlight(self, G); }
	    else
		DrawHighlightBlackAndWhite(self, G, self->DesiredSelection);
	}
	else {
	    /* draw grey border */
	    if (NotFullSize(self)) {
		DrawHighlightBehindDisplayBox(self, G, 1);
		DisplayBoxDrawHighlightGray(self, G); }
	    else {
		long l, t, w, h;
		rectangle_GetRectSize(&self->DesiredSelection, &l, &t, &w, &h);
		l -= self->Xoff;  /* convert to screen coords */
		t -= self->Yoff;
		rasterview_SetTransferMode(self, graphic_COPY);
		rasterview_FillRectSize(self, l-2, t-2, w+4, 2, self->GreyPattern);
		rasterview_FillRectSize(self, l-2, t+h, w+4, 2, self->GreyPattern);
		rasterview_FillRectSize(self, l-2, t, 2, h, self->GreyPattern);
		rasterview_FillRectSize(self, l+w, t, 2, h, self->GreyPattern); }
	}
    }
}

#define CalculatePanHighlight(self, R) {					\
if (FullSize((self)))								\
    *(R) = (self)->ViewSelection;						\
else {										\
    *(R) = (self)->ViewSelection;						\
    rectangle_SetRectSize((R),							\
			   (rectangle_Left((R)) - rectangle_Left(&(self)->DisplayBoxSelection)) * (self)->Scale,					\
			   (rectangle_Top((R)) - rectangle_Top(&(self)->DisplayBoxSelection)) * (self)->Scale,					\
			   rectangle_Width((R)) * (self)->Scale,		\
			   rectangle_Height((R)) * (self)->Scale); } }

void DrawPanHighlight(self, g)
struct rasterview *self;
short g;
{
    struct graphic *G = rasterview_GetDrawable(self);

    DEBUG(("Drawing Pan Highlight\n"));
    if (g == graphic_BLACK) {
	if (self->DisplayBoxHidden) {
	    struct rectangle PH;
	    CalculatePanHighlight(self, &PH);
	    DrawHighlightBlackAndWhite(self, G, PH); }
	else {
	    struct rectangle savedDS;
	    savedDS = self->DesiredSelection;
	    self->DesiredSelection = self->ViewSelection;
	    DrawHighlightBehindDisplayBox(self, G, 0);
	    DisplayBoxDrawHighlight(self, G);
	    self->DesiredSelection = savedDS; } }
    else {
	struct rectangle R;
	CalculatePanHighlight(self, &R);
	DrawHighlightWhite(self, G, R);
	if (! self->DisplayBoxHidden) {
	    DisplayBoxWritePixImageFull(self, G,
					raster_GetPix((struct raster *) rasterview_GetDataObject(self))); }
    }
    if (self->embedded) {
	struct rectangle VB;
	rasterview_GetVisualBounds(self, &VB);
	InsetRect(&VB, BORDER, BORDER);
	DrawHighlightScreenCoordinates(self, rasterview_GetDrawable(self), VB,
				       graphic_BLACK, -1); }
}

/* Draw a Target across the entire Visual Bounds less a BORDER all around. Target is three pixels wide: a black line surrounded by two white lines.
*/
void DrawTarget(self, x, y)
struct rasterview *self;
long x, y;
{
    struct graphic *G = rasterview_GetDrawable(self);
    struct rectangle VB;
    struct rectangle PH;
    long w, h;

    rasterview_GetVisualBounds(self, &VB);
    InsetRect(&VB, BORDER, BORDER);
    w = rectangle_Width(&VB);
    h = rectangle_Height(&VB);

    rasterview_SetTransferMode(self, graphic_BLACK);
    rasterview_MoveTo(self, x, BORDER);
    rasterview_DrawLineTo(self, x, h);
    rasterview_MoveTo(self, BORDER, y);
    rasterview_DrawLineTo(self, w, y);

    CorrectHighlight(self);

    CalculatePanHighlight(self, &PH);
    OffsetRect(&PH, - self->Xoff - BORDER,- self->Yoff - BORDER)

    rectangle_IntersectRect(&VB, &VB, &PH);
    if (IsNotEmptyRect(&VB)) {
	long l, t, w, h, r, b;
	rectangle_GetRectSize(&VB, &l, &t, &w, &h);
	l++; t++;
	r = l + w;
	b = t + h;
	DEBUG(("White Target: (%d,%d,%d,%d)\n", l, t, w, h));
	rasterview_SetTransferMode(self, graphic_WHITE);
	rasterview_MoveTo(self, x+1, t);
	rasterview_DrawLineTo(self, x+1, y-1);
	rasterview_DrawLineTo(self, r, y-1);
	rasterview_MoveTo(self, r, y+1);
	rasterview_DrawLineTo(self, x+1, y+1);
	rasterview_DrawLineTo(self, x+1, b);
	rasterview_MoveTo(self, x-1, b);
	rasterview_DrawLineTo(self, x-1, y+1);
	rasterview_DrawLineTo(self, l, y+1);
	rasterview_MoveTo(self, l, y-1);
	rasterview_DrawLineTo(self, x-1, y-1);
	rasterview_DrawLineTo(self, x-1, t);

    }

    /* Just in case any of the above white lines or the preceeding Hide overlapped the Start. */
    rasterview_SetTransferMode(self, graphic_BLACK);
    rasterview_MoveTo(self, self->StartPanX, BORDER);
    rasterview_DrawLineTo(self, self->StartPanX, h);
    rasterview_MoveTo(self, BORDER, self->StartPanY);
    rasterview_DrawLineTo(self, w, self->StartPanY);
}

void HideTarget(self, x, y)
struct rasterview *self;
long x, y;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct graphic *G = rasterview_GetDrawable(self);
    long clipw, cliph;
    struct rectangle VB;
    struct rectangle VS;
    long vsl, vst, vsw, vsh, vsr, vsb;

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;
    if (NotFullSize(self)) pix = self->Expansion;

    clipw = rasterimage_GetWidth(pix);
    cliph = rasterimage_GetHeight(pix);

    rasterview_GetVisualBounds(self, &VB);
    InsetRect(&VB, BORDER, BORDER);
    rasterview_SetTransferMode(self, graphic_WHITE);
    rasterview_MoveTo(self, x, BORDER);
    rasterview_DrawLineTo(self, x, rectangle_Height(&VB));
    rasterview_MoveTo(self, BORDER, y);
    rasterview_DrawLineTo(self, rectangle_Width(&VB), y);
    DrawPanHighlight(self, graphic_BLACK);

    if (FullSize(self)) {
	VS = self->ViewSelection;
	OffsetRect(&VS,
		   -self->Xscroll + BORDER - rectangle_Left(&VS),
		   -self->Yscroll + BORDER - rectangle_Top(&VS)); }
    else {
	VS = self->DisplayBoxSelection;
	rectangle_SetRectSize(&VS,
			      BORDER + ((self->Xscroll < 0) ?
					-self->Xscroll : 0),
			      BORDER + ((self->Yscroll < 0) ?
					-self->Yscroll : 0),
			      rectangle_Width(&VS)*self->Scale,
			      rectangle_Height(&VS)*self->Scale); }
    rectangle_IntersectRect(&VS, &VS, &VB);
    rectangle_GetRectSize(&VS, &vsl, &vst, &vsw, &vsh);
    vsr = vsl + vsw;
    vsb = vst + vsh;
    DEBUG(("Hide Area: (%d,%d,%d,%d) Scl:(%d,%d) \n",
	    vsl, vst, vsw, vsh, self->Xscroll, self->Yscroll));

    rasterview_SetTransferMode(self, graphic_COPY);

    if (x+1 >= vsl && x-1 <= vsr) {
	/* Repair Vertical strip */
	long l = (x-1 < vsl) ? vsl : x-1;
	l = (l+3 > vsr) ? vsr-3 : l;
	DEBUG(("Ver:(%d,%d,%d,%d) Off:(%d,%d)\n",
	       l, vst, 3, vsh, self->Xoff, self->Yoff));
	ClipAndWritePixImage(clipw, cliph, G, l, vst, pix,
			     l+self->Xoff, vst+self->Yoff, 3, vsh);
    }
    if (y+1 >= vst && y-1 <= vsb) {
	/* Repair Horizontal strip */
	long t = (y-1 < vst) ? vst : y-1;
	t = (t+3 > vsb) ? vsb-3 : t;
	DEBUG(("Hor:(%d,%d,%d,%d) Off:(%d,%d)\n",
	       vsl, t, vsw, 3, self->Xoff, self->Yoff));
	ClipAndWritePixImage(clipw, cliph, G, vsl, t, pix,
			     vsl+self->Xoff, t+self->Yoff, vsw, 3);
    }
    DrawPanHighlight(self, graphic_BLACK);

    if (! self->DisplayBoxHidden && IsNotEmptyRect(&self->DisplayBox)
	 && ((x-1 <= rectangle_Left(&self->DisplayBox)-2*BORDER
	      && x+1 >= rectangle_Right(&self->DisplayBox)+2*BORDER)
	     || (x+1 >= rectangle_Left(&self->DisplayBox)-2*BORDER
		 && x-1 <= rectangle_Right(&self->DisplayBox)+2*BORDER)
	     || (y-1 <= rectangle_Top(&self->DisplayBox)
		 && y+1 >= rectangle_Bottom(&self->DisplayBox))
	     || (y+1 >= rectangle_Top(&self->DisplayBox)-2*BORDER
		 && y-1 <= rectangle_Bottom(&self->DisplayBox)+2*BORDER)))
	DisplayBoxWritePixImage(self, G);
}

void StartPanning(self, x, y)
struct rasterview *self;
long x, y;
{
    DEBUG(("Start Panning at: (%d,%d)\n", x, y));
    self->StartPanX = x;
    self->StartPanY = y;
    self->PanX = x;
    self->PanY = y;
    self->SavedDesiredSelection = self->DesiredSelection;
    self->DesiredSelection = self->ViewSelection;
    /* the following hide forces a calculation of the PanHighlight */
    HideTarget(self, x, y);
    DrawTarget(self, x, y);
}

void ContinuePanning(self, x, y)
struct rasterview *self;
long x, y;
{
    DEBUG(("Continue Panning at: (%d,%d)\n", x, y));
    if (x == self->PanX && y == self->PanY) return;

    if (self->ShowCoords) {
	static char cb[20];
	sprintf(cb, "(%d,%d)", x-self->StartPanX, y-self->StartPanY);
	message_DisplayString(self, 0, cb);
    }

    HideTarget(self, self->PanX, self->PanY);
    DrawTarget(self, self->StartPanX, self->StartPanY);
    DrawTarget(self, x, y);
    self->PanX = x;
    self->PanY = y;
    if (self->embedded) {
	struct rectangle VB;
	rasterview_GetVisualBounds(self, &VB);
	InsetRect(&VB, BORDER, BORDER);
	DrawHighlightScreenCoordinates(self, rasterview_GetDrawable(self), VB,
				       graphic_BLACK, -1); }
}

void ClipScroll(self)
struct rasterview *self;
{
    long minLeft, maxRight;
    long minTop, maxBottom;
    struct rectangle VB;

    rasterview_GetVisualBounds(self, &VB);
    if (NotFullSize(self)) {
	self->Xscroll -= (self->Xscroll % self->Scale);
	minLeft = - (rectangle_Width(&VB) - 40);
	maxRight = (rectangle_Width(&self->ViewSelection) - 20) * self->Scale;
	self->Yscroll -= (self->Yscroll % self->Scale);
	minTop = - (rectangle_Height(&VB) - 40);
	maxBottom = (rectangle_Height(&self->ViewSelection) - 20) * self->Scale;
    }
    else {
	minLeft = - (rectangle_Width(&VB) - 18);
	maxRight = rectangle_Width(&self->ViewSelection) - 18;
	minTop = - (rectangle_Height(&VB) - 18);
	maxBottom = rectangle_Height(&self->ViewSelection) - 18;
    }
    if (minLeft > 0) minLeft = 0;
    if (minTop > 0) minTop = 0;
    if (maxRight < 0) maxRight = 0;
    if (maxBottom < 0) maxBottom = 0;

    DEBUG(("ClipScroll:\n   Visual: (%d,%d,%d,%d)\n   VS: (%d,%d,%d,%d)\n   Min: (%d,%d) Max: (%d,%d)\n",
	    rectangle_Left(&VB), rectangle_Top(&VB),
	    rectangle_Width(&VB), rectangle_Height(&VB),
	    rectangle_Left(&self->ViewSelection), rectangle_Top(&self->ViewSelection),
	    rectangle_Width(&self->ViewSelection), rectangle_Height(&self->ViewSelection),
	    minLeft, minTop, maxRight, maxBottom));

    if (self->Xscroll < minLeft) self->Xscroll = minLeft;
    if (self->Xscroll > maxRight) self->Xscroll = maxRight;
    if (self->Yscroll < minTop) self->Yscroll = minTop;
    if (self->Yscroll > maxBottom) self->Yscroll = maxBottom;
    
    DEBUG(("   Scroll: (%d,%d)\n", self->Xscroll, self->Yscroll));

}


void UpdateZoomedSelection(/* self, x, y */);

void FinishPanning(self, x, y)
struct rasterview *self;
long x, y;
{
    DEBUG(("Finish Panning at: (%d,%d)\n", x, y));
    DEBUG(("Started at: (%d,%d)\n", self->StartPanX, self->StartPanY));
    if (x == self->StartPanX && y == self->StartPanY)
	HideTarget(self, x, y);
    else {
	if (FullSize(self)) {
	    self->Xscroll += self->StartPanX - x;
	    self->Yscroll += self->StartPanY - y;
	    ClipScroll(self); }
	else {
	    long dx, dy;
	    dx = self->StartPanX - (self->StartPanX % self->Scale);
	    dy = self->StartPanY - (self->StartPanY % self->Scale);
	    dx -= x;
	    dy -= y;
	    dx -= (dx % self->Scale);
	    dy -= (dy % self->Scale);
	    DEBUG(("Delta: (%d,%d)\n", dx, dy));
	    self->Xscroll += dx;
	    self->Yscroll += dy;
	    ClipScroll(self);
	    if (dx !=0 || dy != 0) {
		rectangle_EmptyRect(&self->DisplayBoxSelection);
		UpdateZoomedSelection(self,
				      self->Xscroll/self->Scale,
				      self->Yscroll/self->Scale); }
	}

	self->DesiredSelection = self->SavedDesiredSelection;

	self->needsFullUpdate = TRUE;
	rasterview_WantUpdate(self, self); }
}

/* quick-n-dirty one-pixel draw. Displays correctly in rasterview self, even if zoomed, but does *not* notify other observers. x and y must be legal values. Note that bit is a boolean value. Also note that this is slightly different from the exported method rasterview_SetPixel(). */
void SetPixel(self, x, y, bit)
struct rasterview *self;
long x, y;
boolean bit;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL)) return;
    DEBUG(("Set Pixel in 0x%x to %s at (%d,%d)\n", pix, ((bit) ? "Black" : "White"), x, y));
    if (NotFullSize(self))
	SetPixelBehindDisplayBox(self, self->Expansion, x, y, bit);
    else {
	/* The following line does not work in X windows currently.
	graphic_SetBitAtLoc(rasterview_GetDrawable(self),
			    x - self->Xoff, y - self->Yoff, bit); */
	struct rectangle sub;
	rectangle_SetRectSize(&sub, x - self->Xoff, y - self->Yoff, 1, 1);
	rasterview_SetTransferMode(self, graphic_COPY);
	rasterview_FillRect(self, &sub, ((bit) ? self->BlackPattern : self->WhitePattern));
	rasterimage_SetPixel(raster_GetPix(ras), x, y, ((bit) ? 1 : 0));
    }
}

/* draw a line from self->Touch{X,Y} to x1, y1. Uses SetPixel. */
/* the following algorithm is taken from "Fundamentals of Interactive Graphics" by J. D. Foley and A. Van Dam, page 435.*/
void DrawLineTo(self, x1, y1, bit)
struct rasterview  *self;
long x1, y1;
boolean bit;
/* draw the line in the unzoomed version and reflect the changes up. */
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);

    long x2 = self->TouchUpX, y2 = self->TouchUpY;

    /* start algorithm */
    long dx, dy;
    register long incr1, incr2, d, x, y;
    float slope;

    if (ras == NULL || raster_GetPix(ras) == NULL) return;

    dx = x2-x1;
    dy = y2-y1;
 
    if (dx == 0) {
       register long yend;
       if (y1 < y2) {
          y = y1; yend = y2; }
        else {
           y = y2; yend = y1; }
       for (; y<= yend; y++)
          SetPixel(self, x1, y, bit);
       return; }

    slope = (float)dy/dx;
    dx = (dx>=0) ? dx : -dx;
    dy = (dy>=0) ? dy : -dy;

    /* both x and y are either increasing or decreasing */
    if (slope > 0) {
	if (slope <= 1) {
	    register long xend;
	    /* Slope is between 0 and 1. */
	    if (x1 > x2) {
		x = x2;  y = y2;  xend = x1; }
	    else {
		x = x1;  y = y1;  xend = x2; }
	    SetPixel(self, x, y, bit);
	    d = 2*dy - dx;
	    incr1 = 2*dy;
	    incr2 = 2*(dy-dx);
	    while (x < xend) {
		x++;
		if (d < 0) d += incr1;
		else {
		    y++;
		    d += incr2; }
		SetPixel(self, x, y, bit); } }
	else {
	    register long yend;
	    /* Slope is greater than 1. */
	    if (y1 > y2) {
		x = x2;  y = y2;  yend = y1; }
	    else {
		x = x1;  y = y1;  yend = y2; }
	    SetPixel(self, x, y, bit);
	    d = 2*dx - dy;
	    incr1 = 2*dx;
	    incr2 = 2*(dx-dy);
	    while (y < yend) {
		y++;
		if (d < 0) d += incr1;
		else {
		    x++;
		    d += incr2; }
		SetPixel(self, x, y, bit); } }
    }
    else {
	if (slope >= -1) {
	    register long xend;
	    /* Slope is between 0 and -1. */
	    if (x1 < x2) {
		x = x2;  y = y2;  xend = x1; }
	    else {
		x = x1;  y = y1;  xend = x2; }
	    SetPixel(self, x, y, bit);
	    d = 2*dy - dx;
	    incr1 = 2*dy;
	    incr2 = 2*(dy-dx);
	    while (x > xend) {
		x--;
		if (d < 0) d += incr1;
		else {
		    y++;
		    d += incr2; }
		SetPixel(self, x, y, bit); } }
	else {
	    register long yend;
	    /* Slope is less than -1. */
	    if (y1 < y2) {
		x = x2;  y = y2;  yend = y1; }
	    else {
		x = x1;  y = y1;  yend = y2; }
	    SetPixel(self, x, y, bit);
	    d = 2*dx - dy;
	    incr1 = 2*dx;
	    incr2 = 2*(dx-dy);
	    while (y > yend) {
		y--;
		if (d < 0) d += incr1;
		else {
		    x++;
		    d += incr2; }
		SetPixel(self, x, y, bit); } }
    } }

/* The (x, y) argument here is the upper left bit of the current View Selection to be displayed on the screen. If the x value is negative then display (0, y) at a negative xscroll. If the y value is negative then display (x, 0) at a negative yscroll. */
void ZoomToVisualBounds(self, x, y)
struct rasterview *self;
long x, y;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix = raster_GetPix(ras);
    struct rectangle VB;
    struct rectangle OldDisplayBoxSelection;
    long UseableW, SmallW, ZoomW;
    long UseableH, SmallH, ZoomH;
    long diff;

    OldDisplayBoxSelection = self->DisplayBoxSelection;

    DEBUG(("Enter ZoomToVisualBounds: (%d, %d)\n", x, y));

    /* Update the scroll values given the new (x, y). */
    self->Xscroll = x*self->Scale;
    self->Yscroll = y*self->Scale;
    ClipScroll(self);
    /* Use clipped values for (x, y) */
    x = self->Xscroll/self->Scale;
    y = self->Yscroll/self->Scale;

    self->DBXscroll = (self->Xscroll < 0) ? self->Xscroll/self->Scale : 0;
    self->DBYscroll = (self->Yscroll < 0) ? self->Yscroll/self->Scale : 0;
    self->Xoff = ((self->Xscroll < 0) ? self->Xscroll : 0) - BORDER;
    self->Yoff = ((self->Yscroll < 0) ? self->Yscroll : 0) - BORDER;

    /* Useable Width is the number of pixels we can use for display of the pixelimg.  Small Width is the number of pixels in the original raster which we are going to Zoom.  The Small Width is the smaller of the Useable Width divided by the scale of expansion and the number of pixels between the left edge of the selected region and the right edge of the image.  The Height can be determined in a similar manner. Note that the Display Box is always shown in the lower left corner whether or not it overlaps the Zoomed version of the image.  If there is an overlap, blit into the pixelimage the portion of the Display Box (including the border) which overlaps so that the updating of the selected region rectangle on the screen will not mis-paint the screen with the wrong information. */

    rasterview_GetVisualBounds(self, &VB);

    UseableW = rectangle_Width(&VB) - 2*BORDER;
    UseableW = UseableW - (UseableW % self->Scale);
    SmallW = UseableW/self->Scale;
    SetWidthRect(&self->DisplayBox, SmallW);
    if (x < 0) {    
	SmallW += x;
	x = 0; }
    diff = rectangle_Right(&self->ViewSelection) - x;
    if (SmallW > diff && diff > 0) SmallW = diff;
    SetWidthRect(&self->DisplayBoxSelection, SmallW);

    UseableH = rectangle_Height(&VB) - 2*BORDER;
    UseableH = UseableH - (UseableH % self->Scale);
    SmallH = UseableH/self->Scale;
    SetHeightRect(&self->DisplayBox, SmallH);
    if (y < 0) {    
	SmallH += y;
	y = 0; }
    diff = rectangle_Bottom(&self->ViewSelection) - y;
    if (SmallH > diff && diff > 0) SmallH = diff;
    SetHeightRect(&self->DisplayBoxSelection, SmallH);

    SetLeftRect(&self->DisplayBoxSelection, x);
    SetTopRect(&self->DisplayBoxSelection, y);

    if (self->DisplayBoxHidden) {
	SetLeftRect(&self->DisplayBox, -3*rectangle_Width(&self->DisplayBox)); }
    else {
	/* Force the Display Box to be located within the Visual Bounds. */
	if (rectangle_Left(&self->DisplayBox) < rectangle_Left(&VB)
	    || rectangle_Right(&self->DisplayBox) > rectangle_Right(&VB))
	    SetLeftRect(&self->DisplayBox, 5*self->Scale);
	if (rectangle_Top(&self->DisplayBox) < rectangle_Top(&VB)
	    || rectangle_Bottom(&self->DisplayBox) > rectangle_Bottom(&VB))
	    SetTopRect(&self->DisplayBox,
		       rectangle_Bottom(&VB) - rectangle_Height(&self->DisplayBox) - 5*self->Scale); }

    DEBUG(("ZoomVB: (%d,%d,%d,%d)\n",
	    rectangle_Left(&VB),
	    rectangle_Top(&VB),
	    rectangle_Width(&VB),
	    rectangle_Height(&VB)));
    DEBUG(("ZoomDB: (%d,%d,%d,%d)\n",
	    rectangle_Left(&self->DisplayBox),
	    rectangle_Top(&self->DisplayBox),
	    rectangle_Width(&self->DisplayBox),
	    rectangle_Height(&self->DisplayBox)));
    DEBUG(("ZoomDBS: (%d,%d,%d,%d)\n",
	    rectangle_Left(&self->DisplayBoxSelection),
	    rectangle_Top(&self->DisplayBoxSelection),
	    rectangle_Width(&self->DisplayBoxSelection),
	    rectangle_Height(&self->DisplayBoxSelection)));

    if (! rectangle_IsEqualRect(&OldDisplayBoxSelection, &self->DisplayBoxSelection)) {

	ZoomW = SmallW*self->Scale;
	ZoomH = SmallH*self->Scale;

	rasterimage_Resize(self->Expansion, ZoomW, ZoomH);
	rasterimage_GetScaledSubraster(pix, &self->DisplayBoxSelection,
				       ZoomW, ZoomH, self->Expansion);

	DEBUG(("ZoomSize: %d x %d\n", ZoomW, ZoomH));

	DisplayBoxBlitOverlap(self, pix); 
    }

    LEAVE(ZoomToVisualBounds);

}

void UpdateZoomedSelection(self, x, y)
struct rasterview *self;
long x, y;
{
    /* XXX Can be made MUCH more efficient. */
    ZoomToVisualBounds(self, x, y);
}
