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


 


#define view_VERSION 1

enum view_UpdateType  {
    view_FullRedraw,
    view_PartialRedraw,
    view_LastPartialRedraw,
    view_MoveNoRedraw,
    view_Remove
};

enum view_MouseAction  {
    view_NoMouseEvent,
    view_LeftDown,
    view_LeftUp,
    view_LeftMovement,
    view_RightDown,
    view_RightUp,
    view_RightMovement,
    /* File drop actions are only available for DRAGDROP_ENV */
    view_LeftFileDrop,
    view_MiddleFileDrop,
    view_RightFileDrop
};

enum view_DSpass {
    view_NoSet,
    view_WidthSet,
    view_HeightSet
};

enum view_DSattributes {
    view_Fixed= 0,
    view_WidthLarger = 1,
    view_WidthSmaller = 2,
    view_WidthFlexible = 3,
    view_HeightLarger = 4,
    view_HeightSmaller = 8,
    view_HeightFlexible= 12
};


#define view_NOMOVEMENT			    0
#define view_ATLEFT			    1
#define view_ATRIGHT			    2
#define view_BETWEENLEFTANDRIGHT	    4
#define view_ATTOP			  010
#define view_ATBOTTOM			  020
#define view_ATBASELINE			  040
#define view_BETWEENTOPANDBOTTOM	 0100
#define view_BETWEENTOPANDBASELINE	 0200

/* Error codes for the description procedure */
enum view_DescriberErrs {
    view_NoDescribeError,
    view_NoDescriptionAvailable,
    view_NoDescribeString,
    view_NoDescribeObject,
    view_WrongSubclassDescription
};

#include <graphic.ih>

class view : observable[observe] {
overrides:
    ObservedChanged (struct observable *changed, long value);
methods:
    SetName( struct atomlist * name );
    GetName() returns struct atomlist *;
    GetClass() returns struct atomlist *;
    GetParameter( struct atomlist * name, struct atom * type, long * datum )
      returns short;
    GetResource( struct atomlist * name, struct atomlist * class,
		struct atom * type, long * datum ) returns short;
    GetManyParameters( struct resourceList * resources,
		      struct atomlist * name,
		      struct atomlist * class );		      
    PostResource( struct atomlist * path, struct atom * type,
		 long datum );
    InsertViewRegion(struct view *parent, struct region *region);
    InsertView(struct view * parent, struct rectangle * enclosingRect);
    InsertViewSize(struct view * EnclosingView, long xOriginInParent, long yOriginInParent, long width, long height);
    SetDataObject(struct dataobject *dataobject);
    GetApplicationLayer() returns struct view *;
    DeleteApplicationLayer(struct view *applicationLayer);
    DescriptionObject(char * format, long rock) returns char *;
    Describe(char * format, FILE * file, long rock) returns enum view_DescriberErrs;
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    GetOrigin(long width, long height, long *originX, long *originY);
    ReceiveInputFocus();
    LoseInputFocus();
    ReceiveColormap(struct colormap *cmap);
    LoseColormap(struct colormap *cmap);
    WantUpdate(struct view *requestor);
    WantInputFocus(struct view *requestor);
    WantColormap(struct view *requestor, struct colormap *cmap);
    WantNewSize(struct view *requestor);
    WantHandler(char *handlerName) returns struct basicobject *;
    WantInformation(char *key) returns char *;
    PostKeyState(struct keystate *keystate);
    PostMenus(struct menulist *menulist);
    PostCursor(struct rectangle *rec,struct cursor *cursor) ;
    RetractCursor(struct cursor *cursor) ;
    RetractViewCursors(struct view *requestor);
    PostDefaultHandler(char *handlerName, struct basicobject *handler);
    GetInterface(char *type) returns char *;
/* View linking/unlinking functions. */
    LinkTree(struct view *parent);
    UnlinkTree();
    IsAncestor(struct view *possibleAncestor) returns boolean;
    UnlinkNotification(struct view *unlinkedTree);

    GetWindowManagerType() returns char *;
    GetDevice() returns long;
    InitChildren();
    CanView(char *TypeName) returns boolean;

/* Support for selections ala X */
    LoseSelectionOwnership();
    WriteSelection(FILE *out) returns long;

macromethods:
    MoveToPt(Pt) (graphic_MoveToPt((self)->drawable,(Pt)))
    MoveTo(NewX,NewY) (graphic_MoveTo((self)->drawable,(NewX),(NewY)))
    Move(DeltaX, DeltaY) (graphic_Move((self)->drawable,(DeltaX),(DeltaY)))

    GetLogicalBounds(Rect) (graphic_GetLogicalBounds((self)->drawable,(Rect)))
    GetLogicalBoundsSize(x, y, width, height) (graphic_GetLogicalBoundsSize((self)->drawable,(x),(y),(width),(height)))
    GetLogicalRight() (graphic_GetLogicalRight((self)->drawable))
    GetLogicalBottom() (graphic_GetLogicalBottom((self)->drawable))
    GetLogicalLeft() (graphic_GetLogicalLeft((self)->drawable))
    GetLogicalHeight() (graphic_GetLogicalHeight(((self)->drawable)))
    GetLogicalWidth() (graphic_GetLogicalWidth(((self)->drawable)))
    GetLogicalTop() (graphic_GetLogicalTop(((self)->drawable)))

    SetVisualRegion(region) (graphic_SetVisualRegion((self)->drawable, (region)))
    GetVisualRegion(region) (graphic_GetVisualRegion((self)->drawable, (region)))
    GetVisualBounds(Rect) (graphic_GetVisualBounds((self)->drawable,(Rect)))
    GetVisualBoundsSize(x, y, width, height) (graphic_GetVisualBoundsSize((self)->drawable,(x),(y),(width),(height)))
    GetVisualRight() (graphic_GetVisualRight((self)->drawable))
    GetVisualBottom() (graphic_GetVisualBottom((self)->drawable))
    GetVisualWidth() (graphic_GetVisualWidth(((self)->drawable)))
    GetVisualHeight() (graphic_GetVisualHeight(((self)->drawable)))
    GetVisualLeft() (graphic_GetVisualLeft(((self)->drawable)))
    GetVisualTop() (graphic_GetVisualTop(((self)->drawable)))


    GetEnclosedBounds(Rect) (graphic_GetEnclosedBounds((self)->drawable, (Rect)))
    GetEnclosedBoundsSize(x, y, width, height) (graphic_GetEnclosedBoundsSize((self)->drawable,(x),(y),(width),(height)))
    GetEnclosedRight() (graphic_GetEnclosedRight((self)->drawable))
    GetEnclosedBottom() (graphic_GetEnclosedBottom((self)->drawable))
    GetEnclosedLeft() (graphic_GetEnclosedLeft(((self)->drawable)))
    GetEnclosedTop() (graphic_GetEnclosedTop(((self)->drawable)))
    GetEnclosedWidth() (graphic_GetEnclosedWidth(((self)->drawable)))
    GetEnclosedHeight() (graphic_GetEnclosedHeight(((self)->drawable)))

    GetCurrentPt(Pt) (graphic_GetCurrentPt((self)->drawable,(Pt)))

    DrawLineToPt(LineEnd) (graphic_DrawLineToPt((self)->drawable,(LineEnd)))
    DrawLineTo(XEnd, YEnd) (graphic_DrawLineTo((self)->drawable,(XEnd), (YEnd)))
    DrawLine(DeltaX, DeltaY) (graphic_DrawLine((self)->drawable, (DeltaX), (DeltaY)))

    DrawString(Text, Operation) (graphic_DrawString((self)->drawable, (Text), (Operation)))
    DrawText(Text, TextLength, Operation) (graphic_DrawText((self)->drawable, (Text), (TextLength), (Operation)))
    SetSpaceShim(Amount) (graphic_SetSpaceShim((self)->drawable,(Amount)))
    GetSpaceShim() (graphic_GetSpaceShim((self)->drawable))
    SetFont(ChosenFont) (graphic_SetFont((self)->drawable, (ChosenFont)))
    GetFont() (graphic_GetFont((self)->drawable))

    DrawRect(Rect) (graphic_DrawRect((self)->drawable,(Rect)))
    DrawRectSize(x,y,width,height) (graphic_DrawRectSize((self)->drawable,(x),(y),(width),(height)))
    DrawTrapezoid(topX, topY, topWidth, bottomX, bottomY, bottomWidth) (graphic_DrawTrapezoid((self)->drawable,(topX),(topY),(topWidth),(bottomX),(bottomY),(bottomWidth)))
    DrawPolygon(PointArray, PointCount) (graphic_DrawPolygon((self)->drawable,(PointArray), (PointCount)))
    DrawPath(PointArray, PointCount) (graphic_DrawPath((self)->drawable,(PointArray), (PointCount)))
    DrawOval(Rect) (graphic_DrawOval((self)->drawable,(Rect)))
    DrawOvalSize(x,y,width,height) (graphic_DrawOvalSize((self)->drawable, (x), (y), (width), (height)))
    DrawArc(EnclRect, StartAngle, OffsetAngle) (graphic_DrawArc((self)->drawable,(EnclRect),(StartAngle),(OffsetAngle)))
    DrawArcSize(x,y,width,height, StartAngle, OffsetAngle) (graphic_DrawArcSize((self)->drawable, (x), (y), (width), (height), (StartAngle), (OffsetAngle)))
    DrawRRect(OuterBox, InnerBox) (graphic_DrawRRect((self)->drawable, (OuterBox), (InnerBox)))
    DrawRRectSize(x,y,width,height, cornerWidth,cornerHeight) (graphic_DrawRRectSize((self)->drawable, (x), (y), (width), (height), (cornerWidth), (cornerHeight)))
    DrawRgn(Rgn) (graphic_DrawRgn((self)->drawable,(Rgn)))

    FillRect(Rect, Tile) (graphic_FillRect((self)->drawable, (Rect),(Tile)))
    FillRectSize(x,y,width,height, Tile) (graphic_FillRectSize((self)->drawable, (x), (y), (width), (height), (Tile)))
    FillTrapezoid(topX, topY, topWidth, bottomX, bottomY, bottomWidth, Tile) (graphic_FillTrapezoid((self)->drawable, (topX), (topY), (topWidth), (bottomX), (bottomY), (bottomWidth), (Tile)))
    FillPolygon(PointArray, PointCount, Tile) (graphic_FillPolygon((self)->drawable, (PointArray), (PointCount), (Tile)))
    FillOval(Rect, Tile) (graphic_FillOval((self)->drawable, (Rect), (Tile)))
    FillOvalSize(x,y,width,height, Tile) (graphic_FillOvalSize((self)->drawable, (x), (y), (width), (height), (Tile)))
    FillArc(EnclRect, StartAngle, OffsetAngle, Tile) (graphic_FillArc((self)->drawable, (EnclRect), (StartAngle), (OffsetAngle),(Tile)))
    FillArcSize(x,y,width,height, StartAngle, OffsetAngle, Tile) (graphic_FillArcSize((self)->drawable, (x), (y), (width), (height), (StartAngle), (OffsetAngle), (Tile)))
    FillRRect(OuterBox, InnerBox, Tile) (graphic_FillRRect((self)->drawable, (OuterBox), (InnerBox), (Tile)))
    FillRRectSize(x, y, width, height, cornerWidth, cornerHeight, Tile) (graphic_FillRRectSize((self)->drawable, (x), (y), (width), (height), (cornerWidth), (cornerHeight), (Tile)))
    FillRgn(Rgn, Tile) (graphic_FillRgn((self)->drawable, (Rgn), (Tile)))

    EraseRect(Rect) (graphic_EraseRect((self)->drawable, (Rect)))
    EraseRectSize(x,y,width,height) (graphic_EraseRectSize((self)->drawable, (x), (y), (width), (height)))
    EraseVisualRect() (graphic_EraseVisualRect((self)->drawable))

    BitBlt(SrcRect, DestView, DstOrigin, ClipRectList) (graphic_BitBlt((self)->drawable,(SrcRect), (((struct view *)(DestView))->drawable), (DstOrigin), (ClipRectList)))
    BitBltSize(srcX, srcY, dstX, dstY, Width, Height, DstView, clipX, clipY, clipWidth, clipHeight) (graphic_BitBltSize((self)->drawable, (srcX), (srcY), (dstX), (dstY), (Width),(Height), (((struct view*)(DstView))->drawable), (clipX),(clipY), (clipWidth), (clipHeight)))
    SetBitAtLoc(XPos, YPos, NewValue) (graphic_SetBitAtLoc((self)->drawable, (XPos), (YPos), (NewValue)))


    MoveLogicalOrigin(DeltaX, DeltaY) (graphic_MoveLogicalOrigin((self)->drawable,(DeltaX), (DeltaY)))
    SetLogicalOrigin(NewX, NewY) (graphic_SetLogicalOrigin((self)->drawable,(NewX), (NewY)))
    SetLogicalOriginPt(Pt) (graphic_SetLogicalOriginPt((self)->drawable,(Pt)))


    SetClippingRegion(region) (graphic_SetClippingRegion((self)->drawable, (region)))
    GetClippingRegion(region) (graphic_GetClippingRegion((self)->drawable, (region)))
    SetClippingRect(ClipRect) (graphic_SetClippingRect((self)->drawable,(ClipRect)))
    SetClippingRectSize(x, y, width, height) (graphic_SetClippingRectSize((self)->drawable,(x),(y),(width),(height)))
    ClearClippingRect() (graphic_ClearClippingRect((self)->drawable))
    GetClippingRect(Rect) (graphic_GetClippingRect((self)->drawable,(Rect)))
 /*  This doesn't actually exist in graphic... -rr2b GetClippingRectSize(retx, rety, retwidth, retheight) (graphic_GetClippingRectSize((self)->drawable,(retx), (rety), (retwidth), (retheight)))
*/
    SetLineWidth(NewLineWidth) (graphic_SetLineWidth((self)->drawable, (NewLineWidth)))
    GetLineWidth() (graphic_GetLineWidth((self)->drawable))

    SetLineDash( dashPattern, dashOffset, dashType ) (graphic_SetLineDash((self)->drawable, (dashPattern), (dashOffset), (dashType)))
    GetLineDash( dashPattern, dashOffset, dashType ) (graphic_GetLineDash((self)->drawable, (dashPattern), (dashOffset), (dashType)))

    SetLineCap(NewLineCap) (graphic_SetLineCap((self)->drawable, (NewLineCap)))
    GetLineCap() (graphic_GetLineCap((self)->drawable))

    SetLineJoin(NewLineJoin) (graphic_SetLineJoin((self)->drawable, (NewLineJoin)))
    GetLineJoin() (graphic_GetLineJoin((self)->drawable))

    SetTransferMode(NewTransferMode) (graphic_SetTransferMode((self)->drawable, (NewTransferMode)))
    GetTransferMode() (graphic_GetTransferMode((self)->drawable))

    FlushGraphics() (graphic_FlushGraphics((self)->drawable))

    WhitePattern() (graphic_WhitePattern((self)->drawable))
    BlackPattern() (graphic_BlackPattern((self)->drawable))
    GrayPattern(IntensityNum, IntensityDenom) (graphic_GrayPattern((self)->drawable, (IntensityNum), (IntensityDenom)))
    SetPatternOrigin(xpos, ypos)  (graphic_SetPatternOrigin((self)->drawable, (xpos), (ypos)))
    GetPatternOrigin(xpos, ypos)  (graphic_GetPatternOrigin((self)->drawable, (xpos), (ypos)))

    SetForegroundColor(char *colorName, red, green, blue) (graphic_SetForegroundColor((self)->drawable, (colorName), (red), (green), (blue)))
    GetForegroundColor(name, retred, retgreen, retblue)     (graphic_GetForegroundColor((self)->drawable, (name), (retred), (retgreen), (retblue)))
    SetBackgroundColor(char *colorName, red, green, blue ) (graphic_SetBackgroundColor((self)->drawable, (colorName), (red),(green),(blue)))
    GetBackgroundColor(name, retred, retgreen, retblue) (graphic_GetBackgroundColor((self)->drawable, (name), (retred), (retgreen),(retblue)))

    SetFGColor(red, green, blue) (graphic_SetFGColor((self)->drawable,(red),(green),(blue)))
    GetFGColor(red, green, blue) (graphic_GetFGColor((self)->drawable,(red),(green),(blue)))
    SetBGColor(red, green, blue) (graphic_SetBGColor((self)->drawable,(red),(green),(blue)))
    GetBGColor(red, green, blue) (graphic_GetBGColor((self)->drawable,(red),(green),(blue)))

    GetHorizontalResolution() (graphic_GetHorizontalResolution((self)->drawable))
    GetVerticalResolution() (graphic_GetVerticalResolution((self)->drawable))

    GetIM() ((self)->imPtr)
    GetDrawable() ((self)->drawable)

    GetColormap() (graphic_GetColormap((self)->drawable))
    SetColormap(cmap) (graphic_SetColormap((self)->drawable, cmap))
    GetInheritedColormap() (graphic_GetInheritedColormap((self)->drawable))
    SetInheritedColormap(cmap) (graphic_SetInheritedColormap((self)->drawable, cmap))
    ClearInheritedColormap() (graphic_ClearInheritedColormap((self)->drawable))
    CurrentColormap() (graphic_CurrentColormap((self)->drawable))

    EnclosedXToLocalX(xvalue)   ((xvalue)-graphic_GetEnclosedLeft((self)->drawable)+graphic_GetLogicalLeft((self)->drawable))
    EnclosedYToLocalY(yvalue)    ((yvalue)-graphic_GetEnclosedTop((self)->drawable)+graphic_GetLogicalTop((self)->drawable))

    GetDataObject() ((self)->dataobject)
    DisplayClass() (graphic_DisplayClass((self)->drawable ))
    ClearColors() (graphic_ClearColors((self)->drawable))
classprocedures:

    InitializeClass() returns boolean;
    FinalizeObject(struct view *self);
    InitializeObject(struct view *self) returns boolean;

data:

    struct atomlist * name;
    struct atomlist * className;
    short  name_explicitly_set;
    struct im *imPtr;
    struct view *parent;
    struct dataobject *dataobject;
    struct graphic * drawable;
};
