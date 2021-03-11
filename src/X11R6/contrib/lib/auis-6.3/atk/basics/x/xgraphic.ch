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


 

#include <fontdesc.ih>
#include <point.h>
#include <rect.h>
#include <region.ih>

#define xgraphic_PROGRAMMERVERSION	1
#include <image.ih>

typedef struct {
  Display  *disp;       /* destination display */
  int       scrn;       /* destination screen */
  int       depth;      /* depth of drawable we want/have */
  Drawable  drawable;   /* drawable to send image to */
  Pixel     foreground; /* foreground and background pixels for mono images */
  Pixel     background;
  Colormap  cmap;       /* colormap used for image */
  GC        gc;         /* cached gc for sending image */
  XImage   *ximage;     /* ximage structure */
} XImageInfo;


class xgraphic : graphic {
    overrides:
    ClearColors();
    DrawLineTo(long XEnd, long YEnd);
    DrawLine(long DeltaX, long DeltaY);

    DrawString(char *Text, short Operation);
    DrawText(char *Text, long TextLength, short Operation);

    SetFont(struct fontdesc *ChosenFont);

    DrawRectSize(long x,long y,long width,long height);
    DrawPolygon(struct point *PointArray, short PointCount);
    DrawPath(struct point *PointArray, short PointCount);
    DrawOvalSize(long x,long y,long width,long height);
    DrawArcSize(long x,long y,long width,long height, short StartAngle, short OffsetAngle);
    DrawRRectSize(long x,long y,long width,long height, long cornerWidth,long cornerHeight);
    DrawTrapezoid(long topX, long topY, long topWidth, long bottomX, long bottomY, long bottomWidth);


    FillRectSize(long x,long y,long width,long height, struct graphic *Tile);
    FillPolygon(struct point * PointArray, short PointCount, struct graphic *Tile);
    FillOvalSize(long x,long y,long width,long height, struct graphic *Tile);
    FillArcSize(long x,long y,long width,long height, short StartAngle, short OffsetAndle, struct graphic *Tile);
    FillRRectSize(long x, long y, long width, long height, long cornerWidth, long cornerHeight, struct graphic *Tile);
    FillRgn(region * Rgn, struct graphic *Tile);
    FillTrapezoid(long topX, long topY, long topWidth, long bottomX, long bottomY, long bottomWidth, struct graphic *Tile);

    BitBlt(struct rectangle *SrcRect, 
		struct graphic *DestGraphic, struct point *DstOrigin, 
		struct rectangle *ClipRectList);
    SetBitAtLoc(long XPos, long YPos, boolean NewValue);
    WritePixImage( long DestX, long DestY, 
		struct pixelimage *SrcPixels, long SrcX, long SrcY, 
		long width, long height);

    ReadPixImage(long SrcX, long SrcY, 
		struct pixelimage *DestPixels, long DestX, long DestY, 
		long width, long height);

    InsertGraphicRegion(struct graphic *EnclosingGraphic, struct region *regions);
    InsertGraphic(struct graphic *EnclosingGraphic, struct rectangle *EnclosedRectangle);
    InsertGraphicSize(struct graphic *EnclosingGraphic, 
		long xOriginInParent, long yOriginInParent, long width, long height);

    SetVisualRegion(struct region *region);
    SetClippingRegion(struct region *region);
    SetClippingRect(struct rectangle * AdditionalRect);
    ClearClippingRect();

    SetLineWidth(short NewLineWidth);
    SetLineDash( char *dashPattern, int dashOffset, short dashType );
    SetLineCap( short newLineCap );
    SetLineJoin( short newLineJoin );

    SetTransferMode(short NewTransferMode);

    FlushGraphics();

    WhitePattern() returns struct graphic *;
    BlackPattern() returns struct graphic *;
    GrayPattern(short IntensityNum, IntensityDenom) returns struct graphic *;
    SetPatternOrigin(long xpos, long ypos);

    /* If colorName is NULL, use the RGB. */
    SetForegroundColor(char *colorName, long red, long green, long blue);
    SetBackgroundColor(char *colorName, long red, long green, long blue);
    SetFGColor(double red, double green, double blue );
    SetBGColor(double red, double green, double blue );

    GetHorizontalResolution() returns long;
    GetVerticalResolution() returns long;
    GetWindowManagerType() returns char *;
    GetDevice() returns long;
    DisplayClass() returns long;
    IsImageInverted() returns boolean;
    WriteImage(long DestX, long DestY, 
		struct image *image, long SrcX, long SrcY, 
		long width, long height);
    ObservedChanged(struct observable *changed, long value);

macromethods:
    Valid() ((self)->valid)
    XWindow() ((self)->localWindow)
    XDisplay() ((self)->displayUsed)
    XScreen() ((self)->screenUsed)
    XGC()  ((self)->localGraphicContext)
    XFillGC() ((self)->localFillGraphicContext)

classprocedures:
    SetUpdateRegion(Region Rgn,Display* whichDisplay, Drawable whichWindow);
    FinalizeWindow(Display *whichDisplay, Drawable whichWindow);
    FinalizeObject(struct xgraphic *self);
    InitializeObject(struct xgraphic *self) returns boolean;
    InitializeClass() returns boolean;
data: 
    struct xgraphic **gray_levels;	/* mapping of gray pixmaps for monochrome rendering of colors */
    struct xgraphic **gray_shades;   /* Current set of gray pixmaps */
    GC localGraphicContext;	/* Context for the pixmap */
    GC localFillGraphicContext;	/* Context for filling */
    long lastFillStyle;		/* Fill style used for fill gc */
    unsigned long lastFillPixel; /* Pixel used for fill gc */
    struct xgraphic *lastFillTile;  /* Tile used for fill gc */
    Drawable localWindow;		/* X pixmap id  */
    Display *displayUsed;		/* which display is used */
    int screenUsed;			/* which screen is used */
    long lastUpdateRegionIDUsed;    /* set the last update region sequence number that has been processed */
    unsigned long foregroundpixel; /* Pixel value of foreground color. */
    unsigned long backgroundpixel; /* Pixel value of background color. */
    long DisplayClass;
    boolean valid;		/* Set to true if we have a valid Xwindow, etc */
    struct xgraphic *lastStipple;
    unsigned long *index;
    int numColorsInIndex;
    XImageInfo *ximageinfo;
    boolean flipforstipple;
    boolean haveMapInfo;
};
