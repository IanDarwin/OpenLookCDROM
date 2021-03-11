/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* Copyright 1992 Carnegie Mellon University All rights reserved.
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
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/wgraphic.c,v 1.17 1993/05/04 01:06:58 susan Exp $";
#endif /* NORCSID*/

#include <class.h>
#include <andrewos.h>
#include <wmclient.h>
#include <math.h>

static struct fontdesc *  wmgraphic_shadeFont = NULL;
static struct wmgraphic * (wmgraphic_shades[17]) =
	{NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	 NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	 NULL};


#include <wgraphic.eh>
#include <physical.h>
#include <graphic.ih>
#include <fontdesc.ih>
#include <pixelimg.ih>
#include <region.ih>
#include <image.ih>

struct wm_saved_state_vector {
    struct wm_saved_state_vector * next_vector;
    struct wm_window * associatedWindow;
    struct wmgraphic * LastGraphicToUseWindow;
};


static struct wm_saved_state_vector * WindowList = NULL;
struct wmgraphic * wmgraphic_lastUsedGraphic = NULL;
struct wm_saved_state_vector * wmgraphic_currentWindowBlockPtr = NULL;

static void wmgraphic_LocalRestoreGraphicsState();


static void wmgraphic_SelectWindow(wPtr)
struct wm_window* wPtr; {
    struct wm_saved_state_vector * tempWP;

    /* First flush out anything left over in old window */
    if (winout) fflush(winout);

    /* Now select the correct window */
    wm_SelectWindow(wPtr);

    /* Now find and remember the last graphic to use this window */
    for(tempWP=WindowList;
        tempWP && tempWP->associatedWindow != wPtr;
	tempWP=tempWP->next_vector);

    /* did we find it? yes -> reuse it, no -> create a new one */
    if (!tempWP) {
	    /* Allocate it */
	    tempWP = (struct wm_saved_state_vector *) malloc(sizeof(struct wm_saved_state_vector));

            /* Link in new block */
            tempWP->next_vector = WindowList;
            WindowList = tempWP;

            /* Record the window for searching */
            tempWP->associatedWindow = wPtr;

            /* And the associated graphic last used in this window */
            tempWP->LastGraphicToUseWindow = NULL;
	    }

    /* Remember the last window that was selected */
    wmgraphic_currentWindowBlockPtr = tempWP;

    /* And remember the graphic that last used this window */
    wmgraphic_lastUsedGraphic = tempWP->LastGraphicToUseWindow;

}


static int skipvalidation = 0;

static void ValidateState(testgraphic)
struct wmgraphic * testgraphic; {

    if (skipvalidation) return;

    /* Using the correct window? */
    if (CurrentUserWindow != testgraphic->window)
	 wmgraphic_SelectWindow(testgraphic->window);

    /* Using correct graphic? */
    if (testgraphic != wmgraphic_lastUsedGraphic) {
        if (!wmgraphic_currentWindowBlockPtr)
	    wmgraphic_SelectWindow(testgraphic->window);
	wmgraphic_LocalRestoreGraphicsState(testgraphic,wmgraphic_lastUsedGraphic);
        wmgraphic_currentWindowBlockPtr->LastGraphicToUseWindow
	    = wmgraphic_lastUsedGraphic = testgraphic;
    }
}

void wmgraphic__ClearCache(classID,wPtr)
struct classheader *classID;
struct wm_window * wPtr;
{
    struct wm_saved_state_vector * tempWP;

    /* This procedure removes the current caching information
       associated with grahpics and this window so that some
       other module can fool directly with the window system
       and I'll restore it correctly on the next graphics
       call */

    /* First, no graphic was last used */
    wmgraphic_lastUsedGraphic = NULL;

    /* Also, the graphic associated with that window is also undefined,
        so find and reset the last graphic to use this window */
    for(tempWP=WindowList;
        tempWP && tempWP->associatedWindow != wPtr;
	tempWP=tempWP->next_vector);
    if (tempWP) tempWP->LastGraphicToUseWindow = NULL;

}

static int wm_map[4] = { f_white, f_invert, f_copy, f_black};

static void wmgraphic_LocalSetTransferFunction(self)
struct wmgraphic * self;
{
    /* Set the local transfer function based on the logical
	function */

/* Note: the semantics we are trying to emulate is that lines (frames) and
text are stenciles through which black paint is sprayed. Copying between
bitmaps is done simply through moving bits via the specified transfer function.
However, WM has a different notion of transfer mode which varies with its
operation. Therefore, we assume that two different transfer modes may be
necessary: for filling and for text/line drawing. For filling, we use
the same encoding as for the graphic transfer function, except that we
duplicate the requsitie number of times. For text and lines, we assume use the
table below:

graphic function	result
0000 white		f_white
0001 nor		f_white
0010 ~src^dst		f_white
0011 ~src		f_white
0100 src^~dst		f_invert
0101 ~dst (invert)	f_invert
0110 xor		f_invert
0111 nand		f_invert
1000 and		f_copy
1001 eqv		f_copy
1010 dst		f_copy
1011 ~src v dst		f_copy
1100 src (copy)		f_black
1101 src v ~dst		f_black
1110 src v dst		f_black
1111 black		f_black

Since source is encoded as 1100, and destination as 1010, and since we
are assuming that the source is black, our transfer function really only
involves the left two bits -- the two where source = 1 = black. You should
note that where the left two bits (of the results)
are 00 corresponds to where the result
would appear white, when 01 => inversion, 10 => nop (or copy) and 11 => black.
Hence for all but direct graphic operations, we should be able to just
do case on those two bits and set the mode accordingly. For filling
and blitting, we would have to resort to the more general case.
 */

    unsigned long mode=self->header.graphic.transferMode;

    if(mode==graphic_WHITE) {
	if(self->backPixel!=1) mode=graphic_BLACK;
    } else if(mode==graphic_BLACK) {
	if(self->forePixel!=0) mode=graphic_WHITE;
    }
    
    wm_SetFunction(wm_map[ (mode>>2) & 0x3]);
}


/* This function is specific to WM -- it sets the transfer mode correctly
for fill and bitblt operations. Normally the WM mode is left set for
text and drawing operations
 */
static void wmgraphic_SetWMFillMode(self)
struct wmgraphic * self; {
    switch(self->header.graphic.transferMode) {
	/* Special cases where the modes are the same for both
	    text/drawing and filling, so there is nothing to reset */
	case graphic_BLACK:
	case graphic_WHITE:
	case graphic_INVERT: break;
	case graphic_COPY: wm_SetFunction(f_copy);
			    break;
	/* The general case -- here just pass through the
	properly encoded function */
	default: wm_SetFunction(
	    (self->header.graphic.transferMode |(self->header.graphic.transferMode << 8) ) & 0xFFFF);
			    break;
    }
}

static void wmgraphic_SetWMTextAndDrawMode(self)
struct wmgraphic * self; {
    wmgraphic_LocalSetTransferFunction(self);
    return;

    switch(self->header.graphic.transferMode) {
	/* Special cases where the modes are the same for both
	    text/drawing and filling, so there is nothing to reset */
	case graphic_BLACK:
	case graphic_WHITE:
	case graphic_INVERT: break;
	/* Not the same, do the calculation for picking an appropriate
	WM transfer function */
	case graphic_COPY: 
	default: wmgraphic_LocalSetTransferFunction(self);
			    break;
    }
}

#ifdef NOTUSED
static void ExtendedDraw(FromPt, ToPt)
struct point * FromPt;
struct point * ToPt; {
    /* Special case horizintal lines */
    /* Special case vertical lines */
    /* Sort points from left to right */
    /* Do positive slope */
	/* top triangle */
	/* Middle parallelogram */
	/* bottom triangle */
    /* Do negative slope */
	/* top triangle */
	/* Middle parallelogram */
	/* bottom triangle */
    /* Do the move (point no longer defined by wm) */
}
#endif /* NOTUSED */

void wmgraphic__MoveTo(self, NewX, NewY)
struct wmgraphic * self;
long NewY;
long NewX;{
    ValidateState(self);

    point_SetPt(&self->header.graphic.currentPoint,NewX,NewY);
    wm_MoveTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));
}

void wmgraphic__Move(self, DeltaX, DeltaY)
struct wmgraphic * self;
long DeltaY; 
long DeltaX;{
    ValidateState(self);

    point_OffsetPoint(&self->header.graphic.currentPoint,DeltaX,DeltaY);
    wm_MoveTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));

}

void wmgraphic__DrawLineTo(self, XEnd, YEnd)
struct wmgraphic * self;
long YEnd; 
long XEnd;{
    ValidateState(self);

    point_SetPt(&self->header.graphic.currentPoint,XEnd,YEnd);
    wm_DrawTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));
}

void wmgraphic__DrawLine(self,DeltaX, DeltaY)
struct wmgraphic * self;
long DeltaY; 
long DeltaX; {
    ValidateState(self);

    point_OffsetPoint(&self->header.graphic.currentPoint,DeltaX,DeltaY);
    wm_DrawTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));
}

/* Descriptions of string formats for string and text */

#define wmgraphic_NULLTERMINATED 0
#define wmgraphic_LENGTHGIVEN 1

static void wmgraphic_DrawChars(self,Text,Operation,StringMode,TextLength)
struct wmgraphic * self;
char * Text;
short Operation; 
short StringMode;
long TextLength; {

  if (Operation /* != graphic_NOMOVEMENT */ )  {
    register struct font *f;
    long x = point_X(&self->header.graphic.currentPoint);
    long y = point_Y(&self->header.graphic.currentPoint);

    f = fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self);
    if (Operation&
	   (graphic_ATTOP|graphic_BETWEENTOPANDBOTTOM|graphic_ATBOTTOM)){
	x += f -> NWtoOrigin.x - f -> Wbase.x;
	y += f -> NWtoOrigin.y - f -> Wbase.y;
    }
    if (Operation&graphic_BETWEENTOPANDBASELINE) {
	x += (f -> NWtoOrigin.x - f -> Wbase.x)>>1;
	y += (f -> NWtoOrigin.y - f -> Wbase.y)>>1;
    }
    if (Operation&graphic_ATBOTTOM) {
	x -= f -> NtoS.x;
	y -= f -> NtoS.y;
    }
    if (Operation&graphic_BETWEENTOPANDBOTTOM) {
	x -= f -> NtoS.x>>1;
	y -= f -> NtoS.y>>1;
    }
    if (Operation&(graphic_ATRIGHT|graphic_BETWEENLEFTANDRIGHT)) {

	long LastXWidth, LastYWidth;
	if (StringMode == wmgraphic_NULLTERMINATED)
		fontdesc_StringSize(self->header.graphic.currentFont, self,
			    Text, &LastXWidth, &LastYWidth);
	else
		fontdesc_TextSize(self->header.graphic.currentFont, self,
			    Text, TextLength, &LastXWidth, &LastYWidth);
	if (Operation&graphic_ATRIGHT) {
	    x -= LastXWidth;
	    y -= LastYWidth;
	} else {
	    x -= LastXWidth>>1;
	    y -= LastYWidth>>1;
	}
    }

    wmgraphic_MoveTo(self,x,y);
  }
  if (StringMode==wmgraphic_NULLTERMINATED)
        while (*Text) putc (*Text++, winout);
  else
	while (TextLength--) putc(*Text++, winout);

}

void wmgraphic__DrawString(self, Text, Operation)
struct wmgraphic * self;
char * Text;
short Operation; {
    ValidateState(self);

    wmgraphic_DrawChars(self,Text,Operation,wmgraphic_NULLTERMINATED,0);
}


void wmgraphic__DrawText(self, Text, TextLength, Operation)
struct wmgraphic * self;
char * Text;
long TextLength;
short Operation; {
    ValidateState(self);

    wmgraphic_DrawChars(self,Text,Operation,wmgraphic_LENGTHGIVEN,
			    TextLength);
}


void wmgraphic__SetSpaceShim(self,Amount)
struct wmgraphic * self;
short Amount; {
    ValidateState(self);

    self->header.graphic.spaceShim = Amount;
    wm_SetSpaceShim(Amount);
}


void wmgraphic__SetFont(self, ChosenFont)
struct wmgraphic * self;
struct fontdesc * ChosenFont;{
    ValidateState(self);
    if (ChosenFont) {
	self->header.graphic.currentFont = ChosenFont;
        }
    /* Damn, nothing there, so switch back to default */
    else {
        self->header.graphic.currentFont = fontdesc_Create("andysans", fontdesc_Plain, 12);
        }

    /* Select the new font */
    wm_SelectFont(fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self));

}

void wmgraphic__DrawRectSize(self,x, y,width,height)
struct wmgraphic * self;
long x,y,width,height;
{
    long curX, curY, curWidth, curHeight;
    long lineWidth, halfWidth, oddLineWidth;
    int i;

    ValidateState(self);

    lineWidth =  wmgraphic_GetLineWidth(self);
    halfWidth = (lineWidth - 1) / 2;
    oddLineWidth = lineWidth & 1 ? lineWidth : lineWidth - 1;
    curX = physical_LogicalXToGlobalX(self, x - halfWidth);
    curY = physical_LogicalYToGlobalY(self, y - halfWidth);
    curWidth = width + (halfWidth * 2);
    curHeight = height + (halfWidth * 2);

    /* First draw all of the symetric lines around the "infinitely thin center path */
    for(i=0;i<oddLineWidth;i++) {
	/* Draw a single rectangle, clockwise from upper left corner */
	wm_MoveTo(curX,curY);
	wm_DrawTo(curX += curWidth, curY);
	wm_DrawTo(curX, curY += curHeight);
	wm_DrawTo(curX -= curWidth, curY);
	wm_DrawTo(curX, curY -= curHeight);
	curX++; curY++; curWidth -= 2; curHeight -= 2;
    }

    /* Now see if we must add extra line segments for an even lineWidth. The X convention (and hence the one we follow) is that extra lines go on the left and on the top of each line segment, so that's where the extra lines will go */
    if (oddLineWidth != lineWidth) {
	/* We have an even line width, so draw the extra edges */

	/* By design, the next three edges to be drawn will be the bottom edge of the upper segment, the left edge of the right segment and the top edge of the bottom segment. We skip the first drawing operation and to the next two to give the extra left and top */
	wm_MoveTo(curX += curWidth,curY); /* Move to upper right corner */
	wm_DrawTo(curX, curY += curHeight); /* draw down along left side of right edge */
	wm_DrawTo(curX -= curWidth, curY); /* draw backward along top of bottom edge */

	/* Now calculate and draw the left and top edge of the left and top segments */
	wm_MoveTo(curX = physical_LogicalXToGlobalX(self, x - halfWidth)-1, curY = physical_LogicalYToGlobalY(self, y - halfWidth) + height + (halfWidth * 2) );
	wm_DrawTo(curX, curY -= height + (halfWidth * 2) + 1);
	wm_DrawTo(curX += width + (halfWidth * 2) + 1, curY);
    }
}

void wmgraphic__DrawPolygon(self, PointArray, PointCount)
struct wmgraphic * self;
struct point * PointArray;
short PointCount; {
    short cntindex;
    ValidateState(self);

    /* Move to point 0 */
    wmgraphic_MoveTo(self,PointArray[0].x,PointArray[0].y);
    /* Draw all but closing line back to start */
    for(cntindex=1;cntindex<PointCount;cntindex++) {
	wm_DrawTo( physical_LogicalPtToGlobalX(self,&PointArray[cntindex]), physical_LogicalPtToGlobalY(self,&PointArray[cntindex]));
    }
    /* Draw last line segment */
    wm_DrawTo( physical_LogicalPtToGlobalX(self,&PointArray[0]), physical_LogicalPtToGlobalY(self,&PointArray[0]));
}

void wmgraphic__DrawPath(self, PointArray, PointCount)
struct wmgraphic * self;
struct point * PointArray;
short PointCount; {
    short cntindex;
    ValidateState(self);

    /* Move to point 0 */
    wmgraphic_MoveTo(self,PointArray[0].x,PointArray[0].y);
    /* Draw all but closing line back to start */
    for(cntindex=1;cntindex<PointCount;cntindex++) {
	wm_DrawTo( physical_LogicalPtToGlobalX(self,&PointArray[cntindex]), physical_LogicalPtToGlobalY(self,&PointArray[cntindex]));
    }

}

void wmgraphic__DrawOvalSize(self, x,y,width,height)
struct wmgraphic * self;
long x,y,width,height; {

    wmgraphic_DrawArcSize(self,x,y,width,height,0,360);

}

void wmgraphic__DrawArcSize(self,x,y,width,height, StartAngle, OffsetAngle)
struct wmgraphic * self;
long x,y,width,height;
short StartAngle;
short OffsetAngle;{
    long xstart, ystart, xend, yend;
    double halfWidth, halfHeight;
    long realX, realY;

    ValidateState(self);

    realX = physical_LogicalXToGlobalX(self,x);
    realY = physical_LogicalYToGlobalY(self,y);

    halfWidth = width/2.0;
    halfHeight = height/2.0;
    xstart = realX + ((long) ( halfWidth*(1.0+sin(2.0*3.14159*(StartAngle/360.0)) )));
    xend = realX + ((long) ( halfWidth*(1.0+sin(2.0*3.14159*((StartAngle+OffsetAngle)/360.0)) )));
    ystart = realY + ((long) ( halfHeight*(1.0-cos(2.0*3.14159*(StartAngle/360.0)) )));
    yend = realY + ((long) ( halfHeight*(1.0-cos(2.0*3.14159*((StartAngle+OffsetAngle)/360.0)) )));

/*    printf("From left %d, top %d, right %d, bot %d, start %d, off %d\n calculated xstart %d, ystart %d, xend %d, yend %d\n", left, top, right, bot, start, off, xstart, ystart, xend, yend); */

    wm_MoveTo(xstart,ystart);
    wm_ArcTo(xend,yend,(OffsetAngle>0)?0:1,(long) halfHeight, (long) halfWidth, realX+((long) halfWidth), realY + ((long) halfHeight));

}


void wmgraphic__DrawRRectSize(self,x,y,width,height,cornerWidth,cornerHeight)
struct wmgraphic * self;
long x,y,width,height;
long cornerHeight, cornerWidth; {
    ValidateState(self);
    super_DrawRRectSize(self,x,y,width,height,cornerWidth,cornerHeight);
}


void wmgraphic__FillRectSize(self,x,y,width,height,Tile)
struct wmgraphic * self;
long x,y,width,height;
struct graphic * Tile; {
    struct rectangle r;

    ValidateState(self);
    rectangle_SetRectSize(&r,x,y,width,height);
    wmgraphic_FillRect(self,&r,Tile);
}

static int wmgraphic_WMNop(self,Tile)
struct wmgraphic * self;
struct wmgraphic * Tile; {

    /* Just checking to see if any of the NOPs were specified */

    /* trivial case -- the function is nop */
    if (self->header.graphic.transferMode == graphic_DEST) return TRUE;

    /* painting with black which affects causes no dest changes */
    if (wmgraphic_shades[16] &&
	(Tile==wmgraphic_shades[16]) &&
	( (self->header.graphic.transferMode & 0xC) == 0x8 ) ) return TRUE;

    /* painting with white causes no dest changes? */
    if (wmgraphic_shades[0] &&
	( Tile==wmgraphic_shades[0]) &&
	( (self->header.graphic.transferMode & 0x3) == 0x2) ) return TRUE;

    return FALSE;
}

static void wmgraphic_SmashWM(self,Rect,WMMode)
struct wmgraphic * self;
struct rectangle * Rect;
int WMMode; {
    struct rectangle mappedRectangle;
    mappedRectangle = *Rect;
    physical_LogicalToGlobalRect(self,&mappedRectangle);
    wm_SetFunction(WMMode);
    /* do the smash */
    wm_RasterSmash(rectangle_Left(&mappedRectangle),
	           rectangle_Top(&mappedRectangle),
		   rectangle_Width(&mappedRectangle),
		   rectangle_Height(&mappedRectangle));
    wmgraphic_LocalSetTransferFunction(self);
}

void wmgraphic__FillRect(self, Rect, GTile)
struct wmgraphic * self;
struct rectangle * Rect;
struct graphic * GTile; 
{
    struct rectangle Temp;

    /* Do the coercion only once */
    struct wmgraphic * Tile = (struct wmgraphic *) GTile;

    if (Tile == NULL) {
        Tile = (self->forePixel == 0) ?
          (struct wmgraphic *) wmgraphic_BlackPattern(self) :
          (struct wmgraphic *) wmgraphic_WhitePattern(self);
    }

    ValidateState(self);

    /* clip the rectangle to avoid a bug in WM  - 3 July 1989 */
    wmgraphic_GetVisualBounds(self, &Temp);
    if (self->header.graphic.clippingRegion) {
	struct rectangle clipRect;

	region_GetBoundingBox(self->header.graphic.clippingRegion, &clipRect);

	rectangle_IntersectRect(&Temp, &Temp, &clipRect);
    }

    rectangle_IntersectRect(&Temp, Rect, &Temp);

	if (rectangle_Width(&Temp) <= 0 || rectangle_Height(&Temp) <= 0)
	    return;

	/* Check for special cases */

	/* First, paint with white where the result
	        turns white for a white source */
	if ( wmgraphic_shades[0] && (Tile==wmgraphic_shades[0]) && 
	     ( (self->header.graphic.transferMode & 0x3) == 0x0) ) {
	    wmgraphic_SmashWM(self,&Temp,f_white);
	    return;
	     }
        
	/* paint with black where the result
		turns black for a black source */
	if  ( wmgraphic_shades[16] && (Tile==wmgraphic_shades[16]) &&
	      ( (self->header.graphic.transferMode & 0xC) == 0xC) ) {
	    wmgraphic_SmashWM(self,&Temp,f_black);
	    return;
	      }

	/* reverse the bits if painting with black and black
	   casue a bit flip */
	if ( wmgraphic_shades[16] && (Tile==wmgraphic_shades[16]) &&
	     ( (self->header.graphic.transferMode & 0xC) == 0x4) ) {
	    wmgraphic_SmashWM(self,&Temp,f_invert);
	    return;
	     }

	/* reverse the bits if painting with a reversed mode */
	if (self->header.graphic.transferMode == graphic_INVERT) {
	    wmgraphic_SmashWM(self,&Temp,f_invert);
	    return;
	}

	/* reverse the bits if painting with white and white causes
	    bits to flip */
	if ( wmgraphic_shades[0] && (Tile == wmgraphic_shades[0]) &&
	     ( (self->header.graphic.transferMode & 0x3) == 0x1) ) {
	    wmgraphic_SmashWM(self,&Temp,f_invert);
	    return;
	     }

	/* Painting with black, where the result
	        turns white for a black source */
	if ( wmgraphic_shades[16] && (Tile==wmgraphic_shades[16]) && 
	     ( (self->header.graphic.transferMode & 0xC) == 0x0) ) {
	    wmgraphic_SmashWM(self,&Temp,f_white);
	    return;
	     }

	/* painting with anything where the result
	        is white regardless of source */
	if (self->header.graphic.transferMode == graphic_WHITE) {
	    wmgraphic_SmashWM(self,&Temp,f_white);
	    return;
	}

	/* painting with white, where the result
		turns black for a white source */
	if ( wmgraphic_shades[0] && (Tile==wmgraphic_shades[0]) &&
	     ( (self->header.graphic.transferMode & 0x3) == 0x3 ) ) {
	    wmgraphic_SmashWM(self,&Temp,f_black);
	    return;
	     }

	/* paint with anything where the result
		turns black regardless of source */
	if (self->header.graphic.transferMode == graphic_BLACK){
	    wmgraphic_SmashWM(self,&Temp,f_black);
	    return;
	}
       
	/* Now let's check for some expensive Nops */
	if (wmgraphic_WMNop(self,Tile)) return;
	/* Check to see if Tile is really special wm hack */
	if (!Tile->altPixMapUsed) printf("illegal (noncharacter) graphic passed to fillrect");

	wmgraphic_SetWMFillMode(self);

	/* Fill the trapezoid! */
	physical_LogicalToGlobalRect(self,&Temp);	 /* translate to screen coords */
	wm_FillTrapezoid(rectangle_Left(&Temp),	 /* x1 */
		rectangle_Top(&Temp),		 /* y1 */
		rectangle_Width(&Temp),		 /* w1 */
		rectangle_Left(&Temp),		 /* x2 */
		rectangle_Bottom(&Temp)-1,	 /* y2 */
		rectangle_Width(&Temp),		 /* w2 */
		fontdesc_GetRealFontDesc(Tile->fillFont, self), /* font */
		Tile->fillChar);			 /* char */

	wmgraphic_SetWMTextAndDrawMode(self);

}

void wmgraphic__FillPolygon(self,PointArray, PointCount, Tile)
struct wmgraphic * self;
struct point * PointArray;
short PointCount;
struct graphic * Tile;{

    ValidateState(self);
/* the following code looks bogus  XXX    -wjh */
    wmgraphic_SetWMFillMode(self);
    wmgraphic_SetWMTextAndDrawMode(self);
    super_FillPolygon(self,PointArray,PointCount,Tile);
}

void wmgraphic__FillOvalSize(self,x,y,width,height,Tile)
struct wmgraphic * self;
long x,y,width,height;
struct graphic * Tile;{
    ValidateState(self);
/* ************************
**** test dummy code ****
********************** */
    wmgraphic_FillRectSize(self,x,y,width,height,Tile);
/* the following code looks bogus  XXX    -wjh */
    wmgraphic_SetWMFillMode(self);
    wmgraphic_SetWMTextAndDrawMode(self);
}

void wmgraphic__FillArcSize(self,x,y,width,height,StartAngle, OffsetAngle,Tile)
struct wmgraphic * self;
long x,y,width,height;
short StartAngle;
short OffsetAngle;
struct graphic * Tile;{

    ValidateState(self);
    wmgraphic_SetWMFillMode(self);
    super_FillArcSize(self,x,y,width,height,StartAngle,OffsetAngle,Tile);
    wmgraphic_SetWMTextAndDrawMode(self);
}


void wmgraphic__FillRRectSize(self,x,y,width,height,cornerWidth,cornerHeight,Tile)
struct wmgraphic * self;
long x,y,width,height;
long cornerWidth, cornerHeight;
struct graphic * Tile;{
    ValidateState(self);

    /* Handle pathologic cases in system indepedent manner
	(luser desires to bite bullet in efficiency) */

    if ( (2*cornerHeight >= height) || (2*cornerWidth >= width)) {
	/* Bizarre -- corners are bigger than rectangle, so 
	   make an appropriate looking oval */
	 if ( (2*cornerHeight >= height) && (2*cornerWidth >= width))
	    wmgraphic_FillOvalSize(self,x,y,width,height,Tile);
         else if (2*cornerHeight >= height) {
	    /* Fill left semi-oval */
	    wmgraphic_FillArcSize(self,x,y,2*cornerWidth,height,0,-180,Tile);
	    /* Fill vertical rectangle */
	    wmgraphic_FillRectSize(self,x+cornerWidth,y,width-2*cornerWidth,height,Tile);
	    /* Fill right semi-oval */
	    wmgraphic_FillArcSize(self,x+width-2*cornerWidth,y,2*cornerWidth,height,0,180,Tile);
	 }
	 else { /* assuming (2*cornerWidth >= width) */
	    /* Fill top semi-oval */
	    wmgraphic_FillArcSize(self,x,y,width,2*cornerHeight,-90,180,Tile);
	    /* Fill horizontal rectangle */
	    wmgraphic_FillRectSize(self,x,y+cornerHeight,width,height-2*cornerHeight,Tile);
	    /* Fill bottom semi-oval */
	    wmgraphic_FillArcSize(self,x,y+height-2*cornerHeight,width,2*cornerHeight,90,180,Tile);
	 }
         return;
    }

/* ************************
**** test dummy code ****
********************** */
    wmgraphic_FillRectSize(self,x,y,width,height,Tile);
    wmgraphic_SetWMFillMode(self);
    /* Fill me in here */
    wmgraphic_SetWMTextAndDrawMode(self);

}


void wmgraphic__FillRgn(self,Rgn,Tile)
struct wmgraphic * self;
struct region * Rgn;
struct graphic * Tile;{
    ValidateState(self);

    wmgraphic_SetWMFillMode(self);
    /* Fill me in here */
    super_FillRgn(self,Rgn,Tile);
    wmgraphic_SetWMTextAndDrawMode(self);

}


void wmgraphic__FillTrapezoid(self,topX, topY, topWidth, bottomX, bottomY, bottomWidth, Tile)
struct wmgraphic * self, *Tile;
long topX, topY, topWidth, bottomX, bottomY, bottomWidth;
{
    if (Tile == NULL) {
        Tile = (self->forePixel == 0) ?
          (struct wmgraphic *) wmgraphic_BlackPattern(self) :
          (struct wmgraphic *) wmgraphic_WhitePattern(self);
    }

    ValidateState(self);

    wmgraphic_SetWMFillMode(self);
    /* Check to see if Tile is really special wm hack */
    if (!Tile->altPixMapUsed) printf("wmgraphic_FillTrapezoid: illegal (noncharacter) graphic passed to filltrapezoid");


    wm_FillTrapezoid(physical_LogicalXToGlobalX(self,topX),
	    physical_LogicalYToGlobalY(self,topY),
	    topWidth,
	    physical_LogicalXToGlobalX(self,bottomX),
	    physical_LogicalYToGlobalY(self,bottomY),
	    bottomWidth,
	    fontdesc_GetRealFontDesc(Tile->fillFont,self),
	    Tile->fillChar);
    wmgraphic_SetWMTextAndDrawMode(self);

}


void wmgraphic__BitBlt(self, SrcRect, DstGraphic, DstOrigin, ClipRect)
struct wmgraphic * self;
struct rectangle * SrcRect;
struct graphic *DstGraphic;
struct point * DstOrigin;
struct rectangle * ClipRect;
{
    ValidateState(self);

    wmgraphic_SetWMFillMode(self);

    if (self->window!=((struct wmgraphic *)DstGraphic)->window)
	    printf("Unsupported transfer between windows\n");
/* despite documentation, these modes appear to be here! */
/* 
    else if (self->header.graphic.transferMode != graphic_COPY)
	    printf("Unsupported transfer mode\n");
 */
    else {
    /* Do the raster op */
    wm_RasterOp(physical_LogicalXToGlobalX(self,rectangle_Left(SrcRect)),
	 physical_LogicalYToGlobalY(self,rectangle_Top(SrcRect)),
	 physical_LogicalXToGlobalX(DstGraphic,point_X(DstOrigin)),
	 physical_LogicalYToGlobalY(DstGraphic,point_Y(DstOrigin)),
	 rectangle_Width(SrcRect),
	 rectangle_Height(SrcRect));

    wmgraphic_SetWMTextAndDrawMode(self);
    }

}

#define f_CharacterContext	1024
#define	RASTER_REGION_ID    91	    /* some newly unique region id (fred's badge!) */

void
wmgraphic__WritePixImage(self, DestX, DestY, SrcPixels, SrcX, SrcY, width, height)
	struct wmgraphic *self;
	long DestX, DestY, SrcX, SrcY, width, height;
	struct pixelimage *SrcPixels;
{
	long row;			/* cycle through the rows */
	struct rectangle Wanted, Available, Rect;
	long W = pixelimage_GetRowWidth(SrcPixels);
	long ByteWidth;
	short buf[2000];

	/* clip the request to the SrcPixel area available */
	rectangle_SetRectSize(&Wanted, SrcX, SrcY, width, height);
	rectangle_SetRectSize(&Available, 0, 0, 
			SrcPixels->pixelsPerRow, SrcPixels->numRows);
	rectangle_IntersectRect(&Rect, &Wanted, &Available);
	rectangle_GetRectSize(&Rect, &SrcX, &SrcY, &width, &height);
	if (width == 0  ||  height == 0) return;

	/* XXX We should also clip to the output window to avoid sending useless bits.
		At the moment we rely on caller to do this. */

	ByteWidth = (width + 7) / 8;
	wm_BitsToRegion(RASTER_REGION_ID, width, height);
	if ((SrcX & 0x7) == 0) {
	    unsigned char *src = pixelimage_GetBitsPtr(SrcPixels)+(SrcY) * W + SrcX/8;
	    for (row = 0;  row < height;  row++) {
		fwrite(src, ByteWidth, 1, winout);
		src += W;
	    }
	}
	else 
	    for (row = 0;  row < height;  row++) {
		pixelimage_GetRow(SrcPixels, SrcX, (SrcY + row), width, &buf[0]);
		fwrite(&buf[0], ByteWidth, 1, winout);
	}
	wm_SetFunction(f_BlackOnWhite|f_CharacterContext);
   	wm_RestoreRegion(RASTER_REGION_ID, 
			 physical_LogicalXToGlobalX(self, DestX), 
			 physical_LogicalYToGlobalY(self, DestY));

}

void
wmgraphic__WriteImage(self, DestX, DestY, SrcImage, SrcX, SrcY, width, height)
	struct wmgraphic *self;
	long DestX, DestY, SrcX, SrcY, width, height;
	struct image *SrcImage;
{
	printf("wmgraphic: WriteImage method is missing\n");
}

void
wmgraphic__ReadPixImage(self, SrcX, SrcY, DestPixels, DestX, DestY, width, height)
	struct wmgraphic *self;
	struct pixelimage *DestPixels;
	long SrcX, SrcY, DestX, DestY, width, height;
{
	printf("wmgraphic: ReadPixImage method is missing\n");
}

void
wmgraphic__ReadImage(self, SrcX, SrcY, DestImage, DestX, DestY, width, height)
	struct wmgraphic *self;
	struct image *DestImage;
	long SrcX, SrcY, DestX, DestY, width, height;
{
	printf("wmgraphic: ReadPixImage method is missing\n");
}

void wmgraphic_LocalSetClippingRect(self)
struct wmgraphic * self;
{
    struct rectangle Temp;

    wmgraphic_GetVisualBounds(self,&Temp);

    if (self->header.graphic.clippingRegion) {
	struct rectangle clipRect;

	region_GetBoundingBox(self->header.graphic.clippingRegion, &clipRect);
	rectangle_IntersectRect(&Temp ,&Temp, &clipRect);
    }

    physical_LogicalToGlobalRect(self,&Temp);
    wm_SetClipRectangle(rectangle_Left(&Temp), rectangle_Top(&Temp),
			rectangle_Width(&Temp), rectangle_Height(&Temp));
}

void wmgraphic__SetClippingRect(self, AdditionalRect)
struct wmgraphic * self;
struct rectangle * AdditionalRect;{
     /* Machine independent stuff */
     super_SetClippingRect(self,AdditionalRect);


     /* Machine dependent! */
    ValidateState(self);
    wmgraphic_LocalSetClippingRect(self);
}

void wmgraphic__ClearClippingRect(self)
struct wmgraphic * self;
{
    /* Machine independent state manipulation */
    super_ClearClippingRect(self);

    /* Dependent wm twiddling */
    ValidateState(self);
    wmgraphic_LocalSetClippingRect(self);
}


void wmgraphic__SetTransferMode(self,NewTransferMode)
struct wmgraphic * self;
short NewTransferMode;{

    self->header.graphic.transferMode = 0xFF & NewTransferMode;
    ValidateState(self);
    wmgraphic_LocalSetTransferFunction(self);
}


void wmgraphic__InsertGraphic(self, EnclosingGraphic, EnclosedRectangle)
struct wmgraphic * self;
struct wmgraphic * EnclosingGraphic;
struct rectangle * EnclosedRectangle;
    {

    /* do window manager independent stuff */
    super_InsertGraphic(self,EnclosingGraphic,EnclosedRectangle);

    /* And machine depenedent stuff */

    /* Copy over the machine dependent window capability */
    self->window = 
	    ((struct wmgraphic * )EnclosingGraphic)->window;
}


static int skiprestore = 1;

void wmgraphic__RestoreGraphicsState(self)
struct wmgraphic * self;
{
/* for debuggging */
    if (skiprestore) return;

/* select the window */
    if (CurrentUserWindow != self->window)  {
	if (winout != NULL)
	    fflush(winout);
	wm_SelectWindow(self->window);
    }

/* Restore current font */
    wm_SelectFont(fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self));

/* Restore the transfer mode */
    wmgraphic_LocalSetTransferFunction(self);

/* Restore the clipping rectangle */
    wmgraphic_LocalSetClippingRect(self);

/* restore the current point on the screen -- note direct use of
	procedure below, because we */
    wm_MoveTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));

/* restore the space shim */
    wm_SetSpaceShim(self->header.graphic.spaceShim);

}

static void wmgraphic_LocalRestoreGraphicsState(self,oldgPtr)
struct wmgraphic * self, * oldgPtr;
{

/* Restore current font */
    wm_SelectFont(fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self));

/* Restore the transfer mode */
    wmgraphic_LocalSetTransferFunction(self);

/* Restore the clipping rectangle */
    wmgraphic_LocalSetClippingRect(self);

/* restore the current point on the screen -- note direct use of
	procedure below, because we */
    wm_MoveTo(physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	      physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));

/* restore the space shim */
    wm_SetSpaceShim(self->header.graphic.spaceShim);

}

void wmgraphic__FlushGraphics(self)
struct wmgraphic * self; {
    ValidateState(self);
    if (winout) fflush(winout);
}

struct graphic * wmgraphic__WhitePattern(self)
struct wmgraphic * self; {
    if (wmgraphic_shades[0]) return (struct graphic *) wmgraphic_shades[0];
    return wmgraphic_GrayPattern(self,0,16);

}

struct graphic * wmgraphic__BlackPattern(self)
struct wmgraphic * self;{
    if (wmgraphic_shades[16]) 
	return (struct graphic *) wmgraphic_shades[16];
    return wmgraphic_GrayPattern(self,16,16);

}

struct graphic * wmgraphic__GrayPattern(self, IntensityNum, IntensityDenom)
struct wmgraphic * self;
short IntensityNum, IntensityDenom; {
    if (IntensityDenom !=16) 
	IntensityNum = (IntensityNum * 16) / IntensityDenom;
    if (IntensityNum < 0 ) IntensityNum = 0;
    else if (IntensityNum > 16) IntensityNum = 16;

    if (wmgraphic_shades[IntensityNum]) 
	return (struct graphic *) wmgraphic_shades[IntensityNum];

    ValidateState(self);

    if (!wmgraphic_shadeFont)
        wmgraphic_shadeFont = fontdesc_Create("shape",fontdesc_Plain,10);
    return (struct graphic *) (wmgraphic_shades[IntensityNum] = (struct wmgraphic *)
	fontdesc_CvtCharToGraphic(wmgraphic_shadeFont, self, IntensityNum));


}
void wmgraphic__SetFGColor(self, red, green, blue)
struct wmgraphic * self;
double red, green, blue; {
    super_SetFGColor(self, red, green, blue);
    wmgraphic_SetForegroundColor(self, NULL, (long)(65535*red), (long)(65535*green), (long)(65535*blue));
}

void wmgraphic__SetBGColor(self, red, green, blue)
struct wmgraphic * self;
double red, green, blue; {
    super_SetBGColor(self, red, green, blue);
    wmgraphic_SetBackgroundColor(self, NULL, (long)(65535*red), (long)(65535*green), (long)(65535*blue));
}


void wmgraphic__SetForegroundColor(self, colorName, red, green, blue)
struct wmgraphic * self;
char *colorName;
long red, green, blue; {
    struct graphic *g=(struct graphic *)self;

    super_SetForegroundColor(self, colorName, red, green, blue);
    ValidateState(self);
    if (colorName != NULL) {
        if (strcmp(colorName, "white") == 0)
            self->forePixel = 1;
        else
            self->forePixel = 0;
    }
    else {
        if (red == 0 && blue == 0 && green == 0)
            self->forePixel = 0; /* black */
        else self->forePixel = 1; /* white */
    }
    if(self->forePixel==0) {
	g->foreRed=g->foreGreen=g->foreBlue=0;
    } else g->foreRed=g->foreGreen=g->foreBlue=65535;

    wmgraphic_LocalSetTransferFunction(self);
}

void wmgraphic__SetBackgroundColor(self, colorName, red, green, blue)
struct wmgraphic * self;
char *colorName;
long red, green, blue; {
    struct graphic *g=(struct graphic *)self;

    super_SetBackgroundColor(self, colorName, red, green, blue);
    ValidateState(self);

    if (colorName != NULL) {
        if (strcmp(colorName, "black") == 0)
            self->backPixel = 0;
        else
            self->backPixel = 1;
    }
    else {
        if (red == 0 && blue == 0 && green == 0)	
            self->backPixel = 0; /* black */
        else self->backPixel = 1; /* white */
    }
    if(self->backPixel==0) {
	g->backRed=g->backGreen=g->backBlue=0;
    } else g->backRed=g->backGreen=g->backBlue=65535;

    wmgraphic_LocalSetTransferFunction(self);
}

long wmgraphic__GetHorizontalResolution(self)
struct wmgraphic * self; {
    return 80L;
}


long wmgraphic__GetVerticalResolution(self)
struct wmgraphic * self; {
    return 80L;
}

char * wmgraphic__GetWindowManagerType(self)
struct wmgraphic * self;{
    return "AndrewWM";
}

long wmgraphic__GetDevice(self)
struct wmgraphic * self;{
    return (long) self->window;
}


/* -------------------------------------------------- */
/*   Predefined procedures */
/* -------------------------------------------------- */

boolean wmgraphic__InitializeObject(classID,self)
struct classheader *classID;
struct wmgraphic *self;
{
    /* Note: this is very machine dependent */
    self->window /* wm_windowid */ = NULL;
    self->altPixMapUsed = 0;
    if (self->header.graphic.foreRed == 0 && self->header.graphic.foreGreen == 0 && self->header.graphic.foreBlue == 0) self->forePixel = 0; /* black */
    else self->forePixel = 1; /* white */
    if (self->header.graphic.backRed == 0 && self->header.graphic.backGreen == 0 && self->header.graphic.backBlue == 0) self->backPixel = 0; /* black */
    else self->backPixel = 1; /* white */
    return TRUE;
}

