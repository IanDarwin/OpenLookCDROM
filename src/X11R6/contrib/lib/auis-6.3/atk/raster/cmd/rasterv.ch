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


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1987
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/*
rasterview.H

	Class definitions for raster view

*/
#define BORDER 2
#define TWOBORDER 4

#define  rasterview_PROGRAMMERVERSION    2
#define rasterview_SCALECHANGED 10

class rasterview[rasterv] : view {
overrides:

	SetDataObject(/* struct rasterview *self, */ struct dataobject *ras);
		/* establishes data object */
	FullUpdate(/* struct rasterview *self, */ enum view_UpdateType type, 
			long left, long top, long width, long height);
	Update(/* struct rasterview *self, */);
	Hit(/* struct rasterview *self, */ enum view_MouseAction action, 
			long x, long y, long n)	returns struct view *;
	DesiredSize(/* struct rasterview *self, */ long width, long height, 
			enum view_DSpass pass, long *desiredWidth, long *desiredHeight) 
			returns enum view_DSattributes;
	ReceiveInputFocus(/* struct rasterview *self */);
	LoseInputFocus(/* struct rasterview *self */);
	ObservedChanged(/* struct rasterview *self, */ struct raster *dobj, long status);
	Print(/* struct rasterview *self, */ FILE *file, 
			char *processor, char *finalFormat, boolean topLevel);
	GetApplicationLayer(/* struct rasterview *self */) returns struct view *;
	GetInterface(/* struct rasterview *self, */ char *interface_name) 
			returns struct scrollfns *;
	WantUpdate(/* struct rasterview *self, */ struct rasterview *requestor);
	PostMenus(/* struct rasterview *self, */ struct menulist *ml);
	LinkTree(/* struct rasterview *self, */ struct view *parent);

macromethods:
	GetScale() ((self)->Scale)
        SetCoordinateDisplay(val) ((self)->ShowCoords = (val))
        GetOverlaidInset() ((self)->inset)

methods:
/*  -----------  Added 4/6/89 -------------- */
	FitToSize(/* struct rasterview *self, */ struct rectangle logicalrect );
 	AutoCenter(/* struct rasterview *self, */);
	ZoomRaster(/* struct rasterview *self, */ boolean zoomIn );
	SetPan(/* struct rasterview *self, */);
	SetRegionSelect(/* struct rasterview *self, */);
	SetTouchUp(/* struct rasterview *self, */);
	RotateRaster(/* struct rasterview *self, */);
/*  -----------  Added  4/6/89 -------------- */

/*  -----------  Added 5/23/91 -------------- */
	SetScale(/* struct rasterview *self, */ long newscale);
	ResizeRaster(/* struct rasterview *self, */ long w, long h);
/*  -----------  Added 5/23/91 -------------- */

/* --- added 5/27/92 --- */
	SetToolMode(/* struct rasterview *self, */);
	GetMode() returns int;
	SetDesiredSelection(struct rectangle *R);
	CopySelection();
	DestroyToolset();
	SetPixel(struct raster *ras, long x, long y, int bit); 
	BrushSetPixel(long x, long y, unsigned char *pattern, unsigned char *brush); 
	PatternSetPixel(long x, long y, unsigned char *pattern); 
/* --- added 6/14/92 --- */
	ImprintInset(long function);
	RemoveInset();
	ResizeInset();
	OverlayInset(char *insetname);

classprocedures:

	InitializeClass(/* struct classhdr *ClassID*/ ) returns boolean; 
			/* Create default Keymap & Menus */
	InitializeObject(/* struct classhdr *ClassID;*/ struct rasterview *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct rasterview *self);

data:

	boolean UpdateWanted;		/* set TRUE by WantUpdate */
	boolean InsetUpdateWanted;
	boolean needsFullUpdate;		/* TRUE is Update should use FullRedraw */
	boolean OnScreen;			/* if not view_Removed */
	boolean embedded;		/* TRUE if no call to GetApplicationLayer */
	short DraggingEdge;	/* Non-Zero when dragging an edge. */

	struct rectangle PixChanged;	/* minimal rectangle enclosing changes */

	/* these might differ between instances on different displays */
	struct graphic  *BlackPattern;
	struct graphic  *WhitePattern;
	struct graphic  *GreyPattern;

	struct menulist  *Menus;
	struct keystate *Keystate;
	struct cursor *Cursor[5];

	/* Subtract the following values from the coordinates of a point in the raster space to
	get the corresponding point in the logical view space. */
	long Xoff, Yoff;

	/* The following are the coordinates of the (left, top) bit to show on the screen in
	non-Expanded mode.  They are relative to the left upper corner of ViewSelection.. */
	long Xscroll, Yscroll;

	/* The next three rectangles have coordinates in the space of the entire raster they
	should also be subrectangles of dataobject->subraster */

	/* The rectangle which is the currently displayable subraster (used in cropping). */
	struct rectangle ViewSelection;

	/* the rectangle currently surrounded on screen (is Empty if none) */
	struct rectangle CurSelection;

	/* the rectangle that should be highlighted on screen */
	struct rectangle DesiredSelection;

	boolean HighlightIsGrey;		/* TRUE iff grey instead of b/w */
	boolean HasInputFocus;		/* TRUE iff we have the input focus */
	boolean IgnoreUntilNextUpTransition;	/* Just asked for Input Focus */
	long Xdown, Ydown;		/* coordinates of where mouse went down */

	short Mode;

	long TouchUpX, TouchUpY;	/* X, Y of last bit changed in touchup mode */
	struct rectangle SavedDesiredSelection;
	long StartPanX, StartPanY;	/* X, Y of starting to Pan */
	long PanX, PanY;

	boolean NeedsCentering;	/* For initial Centering */

	long Scale;		/* Current Scale factor */
	struct rasterimage *Original;	/* Needed ?? */
	struct rasterimage *Expansion;
	struct rectangle DisplayBox;	/* The location of the Display Box within visual rect*/
	long DBXscroll, DBYscroll;
	boolean MovingDisplayBox;
	boolean DisplayBoxHidden;
	struct rectangle DisplayBoxSelection;	/* Within the full image */
	boolean DisplayBoxOverlapped;
	struct point DisplayBoxAvoidancePath1[6];
	struct point DisplayBoxAvoidancePath2[4];
	struct point DisplayBoxAvoidancePath3[6];
	struct point DisplayBoxAvoidancePath4[4];
	short DisplayBoxPathLength1, DisplayBoxPathLength2;

	boolean CheckedParent;		/* have we set InMessages ? */
	boolean InMessages;		/* are we in messages ? */

	boolean Shrunken;			/* TRUE if image is shrunken */

	boolean ShowCoords;
	
	struct view *inset;
	struct rectangle InsetBox;

	struct rastoolview *toolset;	/* pointer to toolbox, or NULL */
};

#define	DRAW_REVERSE_PIXEL	(2)

#define	RegionSelectMode	0
#define	TouchUpMode		1
#define	PanMode			2
#define WaitMode		3
#define	ToolMode		4

#define RegionSelect(self) ((self)->Mode == RegionSelectMode)
#define TouchUp(self) ((self)->Mode == TouchUpMode)
#define Pan(self) ((self)->Mode == PanMode)
#define Tool(self) ((self)->Mode == ToolMode)

#define FullSize(self) ((self)->Scale == 1)
#define NotFullSize(self) ((self)->Scale != 1)

/* Uncropped(self) 
	tests to see if the rasterview is an uncropped version of the whole
	that is, the ViewSelection selects the entire rastimg.
*/
#define Cropped(self)							\
 raster_GetWidth((struct raster *)rasterview_GetDataObject(self)) !=	\
     rectangle_Width(&self->ViewSelection)				\
 || raster_GetHeight((struct raster *)rasterview_GetDataObject(self)) !=	\
	rectangle_Height(&self->ViewSelection)

#define Uncropped(self)							\
 raster_GetWidth((struct raster *)rasterview_GetDataObject(self)) ==	\
     rectangle_Width(&self->ViewSelection)				\
 && raster_GetHeight((struct raster *)rasterview_GetDataObject(self)) ==	\
	rectangle_Height(&self->ViewSelection)

/* FullySelected(self) 
	tests to see if the DesiredSelection selects the entire current ViewSelection.
*/
#define FullySelected(self)						\
 rectangle_IsEqualRect(&self->ViewSelection, &self->DesiredSelection)

