/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* Copyright 1992 Carnegie Mellon University All rights reserved.
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

#define wmgraphic_PROGRAMMERVERSION	1



#define graphic_ConvertX(to, oldx)  \
    ((oldx) - ((struct graphic *) (to))->enclosedOrigin.x)
#define graphic_ConvertY(to, oldy)  \
    ((oldy) - ((struct graphic *) (to))->enclosedOrigin.y)

#define graphic_OutputX(self, oldx)  \
    ((oldx) - ((struct graphic *)(self))->visualBounds.left + (self)->header.pixmap.physicalOrigin.x)
#define graphic_OutputY(self, oldy)  \
    ((oldy) - ((struct graphic *)(self))->visualBounds.top + (self)->header.pixmap.physicalOrigin.y)



class wmgraphic[wgraphic] : graphic {
overrides:
    MoveTo(long NewX, long NewY);
    Move(long DeltaX, long DeltaY);

    DrawLineTo(long XEnd, long YEnd);
    DrawLine(long DeltaX, long DeltaY);

    DrawString(char * Text, short Operation);
    DrawText(char * Text, long TextLength, short Operation);
    SetSpaceShim(short Amount);
    SetFont(struct fontdesc * ChosenFont);

    DrawRectSize(long x,long y,long width,long height);
    DrawPolygon(struct point * PointArray, short PointCount);
    DrawPath(struct point * PointArray, short PointCount);
    DrawOvalSize(long x,long y,long width,long height);
    DrawArcSize(long x,long y,long width,long height, short StartAngle, short OffsetAngle);
    DrawRRectSize(long x,long y,long width,long height, long cornerWidth,long cornerHeight);

    FillRect(struct rectangle * Rect, struct graphic * Tile);
    FillRectSize(long x,long y,long width,long height, struct graphic * Tile);
    FillPolygon(struct point * PointArray, short PointCount, struct graphic * Tile);
    FillOvalSize(long x,long y,long width,long height, struct graphic * Tile);
    FillArcSize(long x,long y,long width,long height, 
		short StartAngle, short OffsetAngle, struct graphic * Tile);
    FillRRectSize(long x, long y, long width, long height, 
		long cornerWidth, long cornerHeight, struct graphic * Tile);
    FillRgn(region * Rgn, struct graphic * Tile);
    FillTrapezoid(long topX, long topY, long topWidth, 
		long bottomX, long bottomY, long bottomWidth, struct graphic * Tile);

    BitBlt(struct rectangle * SrcRect, 
		struct graphic *DestGraphic, struct point * DstOrigin, 
		struct rectangle * ClipRectList);
    WritePixImage(/* struct wmgraphic *self, */ long DestX, long DestY, 
		struct pixelimage *SrcPixels, long SrcX, long SrcY, 
		long width, long height);
		/* transfers a pixelimage to the graphic */
    ReadPixImage(/* struct wmgraphic *self, */ long SrcX, long SrcY, 
		struct pixelimage *DestPixels, long DestX, long DestY, 
		long width, long height);
		/* transfers a piece of the graphic to a piece of the pixelimage */

    InsertGraphic(struct graphic * EnclosingGraphic, struct rectangle * EnclosedRectangle);

    SetClippingRect(struct rectangle * AdditionalRect);
    ClearClippingRect();

    SetTransferMode(short NewTransferMode);

    RestoreGraphicsState();
    FlushGraphics();

    WhitePattern() returns struct graphic *;
    BlackPattern() returns struct graphic *;
    GrayPattern(short IntensityNum, IntensityDenom) returns struct graphic *;

    SetForegroundColor(char *colorName, long red, long green, long blue);
    SetBackgroundColor(char *colorName, long red, long green, long blue);
    SetFGColor(double red, double green, double blue);
    SetBGColor(double red, double green, double blue);

    GetHorizontalResolution() returns long;
    GetVerticalResolution() returns long;
    GetWindowManagerType() returns char *;
    GetDevice() returns long;
    WriteImage(long DestX, long DestY, struct image *SrcImage, long SrcX, long SrcY, long width, long height); /* transfers an image to the graphic */
    ReadImage(long SrcX, long SrcY, struct image *DestImage, long DestX, long DestY, long width, long height);  /* transfers a piece of the graphic to a piece of the image */

classprocedures:
    ClearCache(struct wm_window * wPtr);
data:
    struct wm_window * window;
    /* Special kludge for performing fills with characters */
    boolean altPixMapUsed;
    struct fontdesc * fillFont;
    char fillChar;
    short forePixel;
    short backPixel;
};
