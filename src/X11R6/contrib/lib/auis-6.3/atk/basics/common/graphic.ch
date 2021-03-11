
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


 

#include <point.h>
#include <rect.h>
#include <shadows.h>

#define graphic_PROGRAMMERVERSION	7

/* It would be nice if there were enums instead of defines, but
unfortunately, one cannot guarantee that enums can be easily or'd
(of course I really want sets of enums, but...) */

#define graphic_NOMOVEMENT		    0
#define graphic_ATLEFT			    1
#define graphic_ATRIGHT			    2
#define graphic_BETWEENLEFTANDRIGHT	    4
#define graphic_ATTOP			  010
#define graphic_ATBOTTOM		  020
#define graphic_ATBASELINE		  040
#define graphic_BETWEENTOPANDBOTTOM	 0100
#define graphic_BETWEENTOPANDBASELINE	 0200

/* LineDash */
#define graphic_LineSolid	0
#define graphic_LineDoubleDash	1
#define graphic_LineOnOffDash	2

/* LineCap */
#define graphic_CapNotLast	0
#define graphic_CapButt		1
#define graphic_CapRound	2
#define graphic_CapProjecting	3

/* LineJoin */
#define graphic_JoinMiter	0
#define graphic_JoinRound	1
#define graphic_JoinBevel	2

/* Display Classes */
#define	graphic_Monochrome	(1<<1)
#define	graphic_Color		(1<<2)
#define	graphic_PseudoColor	(1<<3)
#define	graphic_GrayScale	(1<<4)
#define	graphic_DirectColor	(1<<5)
#define	graphic_TrueColor	(1<<6)
#define	graphic_StaticColor	(1<<7)
#define	graphic_StaticGray	(1<<8)

class graphic : observable[observe] {
methods:
    MoveToPt(struct point * Pt);
    MoveTo(long NewX, long NewY);
    Move(long DeltaX, long DeltaY);
    GetCurrentPt(struct point * Pt);

    DrawLineToPt(struct point * LineEnd);
    DrawLineTo(long XEnd, long YEnd);
    DrawLine(long DeltaX, long DeltaY);

    DrawString(char * Text, short Operation);
    DrawText(char * Text, long TextLength, short Operation);
    SetSpaceShim(short Amount);
    GetSpaceShim() returns short;
    SetFont(struct fontdesc * ChosenFont);
    GetFont() returns struct fontdesc *;

    DrawRect(struct rectangle * Rect);
    DrawRectSize(long x,long y,long width,long height);
    DrawPolygon(struct point * PointArray, short PointCount);
    DrawTrapezoid(long topX, long topY, long topWidth, 
		long bottomX, long bottomY, long bottomWidth);
    DrawPath(struct point * PointArray, short PointCount);
    DrawOval(struct rectangle * Rect);
    DrawOvalSize(long x,long y,long width,long height);
    DrawArc(struct rectangle * EnclRect, short StartAngle, short OffsetAngle);
    DrawArcSize(long x,long y,long width,long height, short StartAngle, short OffsetAngle);
    DrawRRect(struct rectangle * OuterBox, struct rectangle * InnerBox); 
    DrawRRectSize(long x,long y,long width,long height, long cornerWidth,long cornerHeight);
    DrawRgn(struct region * Rgn);

    FillRect(struct rectangle * Rect, struct graphic * Tile);
    FillRectSize(long x,long y,long width,long height, struct graphic * Tile);
    FillTrapezoid(long topX, long topY, long topWidth, 
		long bottomX, long bottomY, long bottomWidth, struct graphic * Tile);
    FillPolygon(struct point * PointArray, short PointCount, struct graphic * Tile);
    FillOval(struct rectangle * Rect, struct graphic * Tile);
    FillOvalSize(long x,long y,long width,long height, struct graphic * Tile);
    FillArc(struct rectangle * EnclRect, short StartAngle, short OffsetAngle, struct graphic * Tile);
    FillArcSize(long x,long y,long width,long height, 
		short StartAngle, short OffsetAngle, struct graphic * Tile);
    FillRRect(struct rectangle * OuterBox, struct rectangle * InnerBox, struct graphic * Tile);
    FillRRectSize(long x, long y, long width, long height, long cornerWidth, long cornerHeight, struct graphic * Tile);
    FillRgn(struct region * Rgn, struct graphic * Tile);

    EraseRect(struct rectangle *Rect);
    EraseRectSize(long x,long y,long width,long height);
    EraseVisualRect();

    BitBlt(struct rectangle *SrcRect, 
		struct graphic *DestGraphic, struct point * DstOrigin, 
		struct rectangle *ClipRectList);
    BitBltSize(long srcX, long srcY, 
		long dstX, long dstY, long Width, long Height, 
		struct graphic *DstGraphic, 
		long clipX, long clipY, long clipWidth, long clipHeight);
    SetBitAtLoc(long XPos, long YPos, boolean NewValue);

    WritePixImage(long DestX, long DestY, 
		struct pixelimage *SrcPixels, long SrcX, long SrcY, 
		long width, long height);
    ReadPixImage(long SrcX, long SrcY, 
		struct pixelimage *DestPixels, long DestX, long DestY, 
		long width, long height);

    InsertGraphic(struct graphic * EnclosingGraphic, struct rectangle * EnclosedRectangle);
    InsertGraphicSize(struct graphic * EnclosingGraphic, 
		long xOriginInParent, long yOriginInParent, long width, long height);
    InsertGraphicRegion(struct graphic *EnclosingGraphic, struct region *regions);

    SetVisualRegion(struct region *region);
    GetVisualRegion(struct region *retRegion) returns struct region *;

    MoveLogicalOrigin(long DeltaX, long DeltaY);
    SetLogicalOrigin(long NewX, long NewY);
    SetLogicalOriginPt(struct point * Pt);
    GetLogicalBounds(struct rectangle * Rect);
    GetLogicalRight() returns long;
    GetLogicalBottom() returns long;

    GetEnclosedBounds(struct rectangle * Rect);
    GetEnclosedRight() returns long;
    GetEnclosedBottom() returns long;

    GetVisualBounds(struct rectangle * Rect);
    GetVisualRight() returns long;
    GetVisualBottom() returns long;

    SetClippingRegion(struct region *region);
    GetClippingRegion(struct region *retRegion) returns struct region *;

    SetClippingRect(struct rectangle * AdditionalRect);
    SetClippingRectSize(long x, long y, long w, long h);
    ClearClippingRect();
    GetClippingRect(struct rectangle * Rect);

    SetLineWidth(short NewLineWidth);
    GetLineWidth() returns short;

    SetLineDash( char *dashPattern, int dashOffset, short dashType );
    GetLineDash( char **dashPattern, int *dashOffset, short *dashType );

    SetLineCap( short NewLineCap );
    GetLineCap() returns short;

    SetLineJoin( short NewLineJoin );
    GetLineJoin() returns short;

    SetTransferMode(short NewTransferMode);
    GetTransferMode() returns short;

    RestoreGraphicsState();
    FlushGraphics();

    WhitePattern() returns struct graphic *;
    BlackPattern() returns struct graphic *;
    GrayPattern(short IntensityNum, short IntensityDenom) returns struct graphic *;
    SetPatternOrigin(long xpos, long ypos);
    GetPatternOrigin(long *xpos, long *ypos);

    /* If colorName is NULL, use the RGB. */
    SetForegroundColor(char *colorName, long red, long green, long blue);
    GetForegroundColor(char **colorName, long *red, long *green, long *blue);
    SetBackgroundColor(char *colorName, long red, long green, long blue);
    GetBackgroundColor(char **colorName, long *red, long *green, long *blue);

    SetFGColor( double red, double green, double blue );
    GetFGColor( double *red, double *green, double *blue );
    SetBGColor( double red, double green, double blue );
    GetBGColor( double *red, double *green, double *blue );

    GetHorizontalResolution() returns long;
    GetVerticalResolution() returns long;
    GetWindowManagerType() returns char *;
    GetDevice() returns long;
    DisplayClass() returns long;
    IsImageInverted() returns boolean;
    WriteImage(long DestX, long DestY, 
		struct image *image, long SrcX, long SrcY, 
		long width, long height);
    ReadImage(long SrcX, long SrcY, 
		struct image *DestImage, long DestX, long DestY, 
		long width, long height);
    SetFGToShadow(int shadow);  /* see installed shadows.h */
    ClearColors();
    
macromethods:

    GetLogicalWidth() ((self)->localBounds.width)
    GetLogicalHeight() ((self)->localBounds.height)
    GetLogicalLeft() ((self)->localBounds.left)
    GetLogicalTop() ((self)->localBounds.top)

    GetVisualWidth() ((self)->visualBounds.width)
    GetVisualHeight() ((self)->visualBounds.height)
    GetVisualLeft() ((self)->visualBounds.left)
    GetVisualTop() ((self)->visualBounds.top)

    GetEnclosedWidth() ((self)->localBounds.width)
    GetEnclosedHeight() ((self)->localBounds.height)
    GetEnclosedLeft() (point_X(&((self)->enclosedOrigin)))
    GetEnclosedTop() (point_Y(&((self)->enclosedOrigin)))

    GetColormap() ((self)->colormap)
    SetColormap(cmap) ((self)->colormap = (cmap))
    GetInheritedColormap() ((self)->inheritedColormap ? (self)->inheritedColormap : NULL)
    SetInheritedColormap(cmap) ((self)->inheritedColormap = (cmap))
    ClearInheritedColormap() ((self)->inheritedColormap = NULL)
    CurrentColormap() ((self->colormap && *(self->colormap)) ? self->colormap : self->inheritedColormap)

classprocedures:
    SetDefaultColors(char *foreground, char *background);
    GetDefaultColors(char **foreground, char **background);
    CreateGraphic(struct view *v) returns struct graphic *;
    FinalizeObject(struct graphic * self);
    InitializeObject(struct graphic *self) returns boolean;
    ComputeShadow(long  br , long  bg , long  bb, long  *rr , long  *rg , long  *rb, int  shadow);
    ComputeShadowDouble(double br, double bg, double bb, double *rr, double *rg, double *rb, int shadow);
    
data: 
    struct rectangle localBounds;    /* What is size of rectangle */
    struct rectangle visualBounds;   /* How much of rect is seen */
    struct region *localRegion;
    struct region *visualRegion;
    struct point savedOrigin;	     /* Logical offsets of origin */
				     /* Saved across insertions */
    struct point enclosedOrigin;     /* local rect in parent's coord*/
    struct fontdesc * currentFont;   /* Current font for text drawing */
    short  spaceShim;
    short  transferMode;	    /* ALU function for bitblit */
    short  lineWidth;		    /* How thick to make outlines */
    short  lineDashType;	    /* Dash style for lines */
    int	   lineDashOffset;	    /* Dash offset (phase) for lines */
    char   *lineDashPattern;	    /* Dash pattern for lines */
    short  lineCap;		    /* Cap style for lines */
    short  lineJoin;		    /* Join style for lines */
    struct region * clippingRegion; /* Rects for clipping */
    struct point    currentPoint;	    /* Start of drawing operations */
    boolean internalFont;	     /* internal fontdescriptor */
    long foreRed, foreGreen, foreBlue;	    /* Foreground colors */
    long backRed, backGreen, backBlue;	/* Backgrond colors */
    char    *foreName, *backName;   /* background and foregound names */
    struct colormap **colormap, **inheritedColormap;
/* private data */
    struct point physicalOrigin;
    struct point patternOrigin;
};


/* Transfer function bit encoding. One specifies the transfer function
   by giving a table source and destination bits, encoded in a hex digit.
   The binary interpretation is that 1 is black and 0 white.
   The table encodes bits as follows:
	Bit 3   Bit 2   Bit 1   Bit 0
Source	    1(B)    1(B)    0(W)    0(W)
Dest	    1(B)    0(W)    1(B)    0(W)
Result	    ?	    ?	    ?	    ?
Thus, Bit 3 specifies what the result should be if both the source and
destination are 1; bit 2 specifies the result if the source is 1 and the
destination 0; bit 1, when the source is 0 and the destination 1; and
bit 0 when both the source and destination are 0. For example, to
generate the AND function, bit 3 would be one and bits 2, 1 and 0 would
be 0, so the resulting hex digit would be 0x4. If we want the source to
be copied directly without regard to the current value of the 
destination, bits 3 and 2 would be 1 and bits 1 and 0 would be 0,giving
the hex digit 0xc.

Because we believe that we might expand our transfer function to
three inputs (using a mask as a third input), we now require the transfer
function be two hexadecimal digits -- the one generated as above 
repeated. Thus, the value for AND would be 0x44 and for copying the
source, 0xcc.

To help construct the desired function, two constants graphic_SOURCE
and graphic_DEST have been defined that allow a programmer to specify
the desired effects without having to do the bit by bit calculations.
Instead, one can just perform the logical operations directly on
the constants. For example, the AND operation can be expressed as
(graphic_SOURCE & graphic_DEST). Some common combinations are provided
as well.
*/

/* Predefined constants for use in transfer modes */
#define graphic_SOURCE	0xCC
#define graphic_DEST	0xAA

/* Some synonyms for ease of use */
#define graphic_COPY	graphic_SOURCE
#define graphic_OR	(graphic_SOURCE | graphic_DEST)
#define graphic_XOR	(graphic_SOURCE ^ graphic_DEST)
#define graphic_AND	(graphic_SOURCE & graphic_DEST)
#define graphic_WHITE	0x00
#define graphic_BLACK	0xFF
#define graphic_INVERT	((~graphic_DEST)&0xFF)

/* Some notes on the structure:
    The attached graphic provides some bits to write on. Nothing is
assumed initially about the relationship between bounds in a graphic
and graphic bounds. A graphic is associated witha graphic in one of two
ways. First, a graphic can be directly laid on top of
graphic (Setgraphic), in which case the call explicitly links the portion 
of graphic to the graphic. Second, a graphic can be inserted into
another graphic, in which case the graphic information is gleaned
relative to the parent. Either case is very system dependent. One 
assumes that only im will do the former, and that everyone else will
do the latter which will change with each machine, but the change
should be invisible to the user.

The local bounds specify, in local graphic coordinates, what the
coorindate system is for this graphic. Initially this is 0,0 in upper
left corner, though it can be changed by moving the origin explicitly.
The visual rectangle shows
which part of the local bounds is actually displayed on the screen
(also in local coordinates). The enclosedRect is the same as the local
bounds rectangle except that it is expressed in the parent's coordinate
space (if any exists). Like the physical rectangle of the graphic, the
width and height information are redundant with the width and height of
the localbounds.

 If any mapping between local coordinates
and underlying window manager coordinates is necessary, the window
manager system will take care of it (state will be part of graphic).

graphic will contain the window manager depedent information, such as 
window id number. The fontdesc is the font descriptor to be used for
diplaying text. When a descriptior was created, a size, famliy and
style was specified, this to change to bold or larger font, a 
new descirptor must be set.

The transfer mode explains how bits will be pasted on the screen when
drawing lines, writing text, blitting bits or filling figures. 
Essentially the same defintion as for wm is used, except the tiling mask
is available only for bitblitting.

The line width is self-explanatory. The current point is used to 
determine where to draw lines or text. The clipping rectangle is a user
controllable way to limit drawing commands. It is in local coordinates,
but is usually null (no explicit clipping).

*/


