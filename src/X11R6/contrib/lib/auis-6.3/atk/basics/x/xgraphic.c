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

#ifdef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/xgraphic.c,v 1.83 1994/04/19 16:15:05 rr2b Exp $";
#endif


 /* xgraphic.c
  */

#include <andrewos.h>
#include <class.h>
#include <stdio.h>
#include <util.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include <physical.h>
#include <graphic.ih>
#include <pixelimg.ih>
#include <fontdesc.ih>
#include <environ.ih>
#include <view.ih>
#include <xfontd.ih>
#include <region.ih>
#include <image.ih>
#include <xcolor.ih>
#include <xcmap.ih>
#include <xgraphic.eh>

static int regionDebug = 0;
static int imageDebug = 0;
static int bltDebug = 0;
static int GotError;

#define	XFullBrightness	65535.0

struct xgraphic_GrayBlock {
    struct xgraphic_GrayBlock * nextBlock;
    Display * displayUsed;
    int screenUsed;
    struct xgraphic * graphic_shades[17];
};

/* This colorCube has to be flexible.  You should be able to set the characteristics of the individual components along each component-dimension (r,g,b).  These characterstics include both (1) the exact intensities of the individual color components and (2) the inter-component spacing.  For now,  I'll just go with some smart number of evely spaced components.  The fun part will be determing the correct dimensions for the given display hardware. */

struct color_cube {
    XColor *cube[6][6][6];
    boolean inited;
    int depth;
    int componentStep;
    int availableColors;
    int samplesPerComponent;
};
static struct color_cube *colorCube = NULL;

static boolean haveMapInfo = FALSE;
XStandardColormap map_info;

struct image_prefs {
    boolean alwaysUsePrivateCmap;
    boolean alwaysFitFixed;
    boolean alwaysDither;
} imagePrefs;

static struct xgraphic_GrayBlock * grayBlockHeader = NULL;

#ifdef OLDXBUG
/********** Special declarations used in an attempt to circumvent an Xlib bug with regions. These declarations and their use shold be removed since they depend on internal (and therefore changable) structure of Xlib **********/
typedef struct {
    short x1, x2, y1, y2;
} Box, BOX, BoxRec, *BoxPtr;

typedef struct _XRegion {
    short size;
    short numRects;
    BOX *rects;
    BOX extents;
} REGION;
#endif /* OLDXBUG */

/******* end of special declarations *********/

#ifdef XRELEASE2_ENV
static TempXSetRegion( dpy, gc, r )
    Display *dpy;
    GC gc;
    register Region r;
{
    register int i;
    register XRectangle *xr;
    xr = (XRectangle *) 
    	_XAllocScratch(dpy, (unsigned long)(r->numRects * sizeof (XRectangle)));
    for (i = 0; i < r->numRects; i++) {
        xr[i].x = r->rects[i].x1;
	xr[i].y = r->rects[i].y1;
	xr[i].width = r->rects[i].x2 - r->rects[i].x1;
	xr[i].height = r->rects[i].y2 - r->rects[i].y1;
      }
    XSetClipRectangles(dpy, gc, 0, 0, xr, r->numRects, Unsorted);
}
#endif /* XRELEASE2_ENV */

struct xgraphic * * xgraphic_FindGrayBlock(WhichDisplay, WhichScreen)
Display * WhichDisplay;
int WhichScreen; {
    struct xgraphic_GrayBlock * CurBlock;
    int i;

    for (CurBlock=grayBlockHeader;CurBlock;CurBlock=CurBlock->nextBlock){
        if ( (CurBlock->displayUsed == WhichDisplay) &&
	     (CurBlock->screenUsed == WhichScreen)) 
	    return CurBlock->graphic_shades;
    }
    /* Not there, so add one */
    CurBlock = (struct xgraphic_GrayBlock *) malloc(sizeof(struct xgraphic_GrayBlock));
    CurBlock->nextBlock=grayBlockHeader;
    CurBlock->displayUsed = WhichDisplay;
    CurBlock->screenUsed = WhichScreen;
    for (i=0;i<17;i++) CurBlock->graphic_shades[i] =  NULL;
    grayBlockHeader = CurBlock;
    return CurBlock->graphic_shades;
}

/* C hacks to keep everything looknig the same */
struct font {
    XFontStruct dummy;
};


#define VerifyUpdateClipping(selfParam) \
    if ((selfParam)->lastUpdateRegionIDUsed != curUpdateRegionID) InstallUpdateRegion((selfParam))

static long curUpdateRegionID = 0; /* which region is currently being used */

struct xgraphic_UpdateBlock {
    struct xgraphic_UpdateBlock * nextBlock;
    Display * displayUsed;
    Drawable windowUsed;
    Region updateRegionInUse;
    long RegionCounter;
};

static struct xgraphic_UpdateBlock * updateBlockHeader = NULL;

struct  xgraphic_UpdateBlock * xgraphic_FindUpdateBlock(WhichDisplay, WhichWindow)
Display * WhichDisplay;
Drawable WhichWindow;
{
    struct xgraphic_UpdateBlock * CurBlock;

    if (regionDebug) printf("FindUpdateBlock: looking for display %X, window %X\n", WhichDisplay, WhichWindow);

    for (CurBlock=updateBlockHeader;CurBlock;CurBlock=CurBlock->nextBlock){
        if ( (CurBlock->displayUsed == WhichDisplay) &&
	     (CurBlock->windowUsed == WhichWindow)) {
	    if (regionDebug) printf("FindUpdate: found old %X\n", CurBlock);
	    return CurBlock;
	}
    }
    /* Not there, so add one */
    CurBlock = (struct xgraphic_UpdateBlock *) malloc(sizeof(struct xgraphic_UpdateBlock));
    if (regionDebug) printf("FindUpdate: making new %X\n", CurBlock);
    CurBlock->nextBlock=updateBlockHeader;
    CurBlock->displayUsed = WhichDisplay;
    CurBlock->windowUsed = WhichWindow;
    CurBlock->updateRegionInUse = NULL;
    CurBlock->RegionCounter = -1;
    updateBlockHeader = CurBlock;
    return CurBlock;
}


/* forward declaration */

void xgraphic_LocalSetClippingRect();

static void InstallUpdateRegion(self)
struct xgraphic * self; {
    struct xgraphic_UpdateBlock * curBlock;

    if (regionDebug) printf("InstallUpdateRegion: new region, cur glob ID %d\n", curUpdateRegionID);
    /* find out whether a real change has happened or just a false alarm */
    curBlock = xgraphic_FindUpdateBlock(xgraphic_XDisplay(self), xgraphic_XWindow(self));
    if (curBlock->RegionCounter == self->lastUpdateRegionIDUsed) {
	/* False alarm, someone else bumped counter, nothing for this graphic (window/display) has changed, so update our counter to show that we really are current with the latest changes */
	if (regionDebug) printf("InstallUpdateRegion: no change, curBlockID %d, graphic ID %d\n", curBlock->RegionCounter, self->lastUpdateRegionIDUsed);
	self->lastUpdateRegionIDUsed = curUpdateRegionID;
    }
    else {
	/* New region, so let's go use it */
	xgraphic_LocalSetClippingRect(self,curBlock);
    }
}

void xgraphic__SetUpdateRegion(classID,Rgn,whichDisplay,whichWindow)
struct classheader *classID;
Region Rgn;
Display* whichDisplay;
Drawable whichWindow; {
    struct xgraphic_UpdateBlock * curBlock;

    if (regionDebug) printf("SetUpdateRegion: Rgn %X, whichDisplay %X, whichWindow %X\n", Rgn, whichDisplay, whichWindow);

    curUpdateRegionID++;
    /* update list of regions and their use */
    curBlock = xgraphic_FindUpdateBlock(whichDisplay,whichWindow);
    curBlock->updateRegionInUse = Rgn;
    curBlock->RegionCounter = curUpdateRegionID;
    if (regionDebug) printf("SetUpdateRegion: for block %X, setting counter %d, region %X\n", curBlock, curBlock->RegionCounter, curBlock->updateRegionInUse);
}

void xgraphic__FinalizeWindow(classID, WhichDisplay, WhichWindow)
struct classheader *classID;
Display *WhichDisplay;
Drawable WhichWindow;
{
    struct xgraphic_UpdateBlock *CurBlock, *NextBlock, *last=NULL;
    for (CurBlock=updateBlockHeader ; CurBlock ; CurBlock=NextBlock){
	NextBlock=CurBlock->nextBlock;
	if ( (CurBlock->displayUsed == WhichDisplay) &&
	     (CurBlock->windowUsed == WhichWindow) ) {
	    if(last) last->nextBlock=NextBlock;
	    else updateBlockHeader=NextBlock;
	    if(CurBlock->updateRegionInUse) XDestroyRegion(CurBlock->updateRegionInUse);
	    free(CurBlock);
	} else
	    last=CurBlock;
    }
}

void xgraphic__ClearColors(self)
struct xgraphic *self;
{
    struct xcolormap *cmap = *(struct xcolormap**) xgraphic_CurrentColormap(self);
    int i;
    for(i=xcolormap_Used(cmap)-1;i>=0;i--) {
	struct xcolor *c=(struct xcolor *)xcolormap_NthColor(cmap, i);
	if(c  && xcolor_Pixel(c)!=self->foregroundpixel && xcolor_Pixel(c)!=self->backgroundpixel && xgraphic_IsObserver(self, c)) {
	    if(xcolor_ReferenceCount(c)==1) xcolormap_DestroyColor(cmap, c);
	    else xcolor_Destroy(c);
	}
    }    
}

/* since X uses 0=black and ATK uses 0=white, */
boolean xgraphic__IsImageInverted(self)
struct xgraphic *self;
{
    return TRUE;
}

static void xgraphic_LocalSetTransferFunction(self, prevValue)
struct xgraphic * self;
int prevValue;
{
/* This discussion documents how the X transfer function was calculated.

Graphic works on the following bit encoding:
0=white, 1= black

Source	1   1	0   0	    b	b   w	w
Destin	1   0	1   0	    b	w   b	w
Result	?   ?	?   ?	    ?	?   ?	?

X uses 0=black, 1 = white

We construct the following table of all transfer modes of Graphic (16
possible functions), determine what the visual effects are in terms of
black and white, and then remap the b/w values into X's 0/1 scheme.
Finally, we determine which X function causes the visual effect
and consider it the mapped ALU function for X.

	G Code	Appear	X Code X Function   X Func Encoding

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0000	wwww	1111	GXset	    1111

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0001	wwwb	1110	GXnand	    1110

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0010	wwbw	1101	GXorInverted 1101

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0011	wwbb	1100	GXcopyInverted 1100

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0100	wbww	1011	GXorReverse 1011

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0101	wbwb	1010	GXinvert    1010

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0110	wbbw	1001	GXequiv	    1001

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	0111	wbbb	1000	GXnor	    1000

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1000	bwww	0111	GXor	    0111

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1001	bwwb	0110	GXxor	    0110

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1010	bwbw	0101	GXnoop	    0101

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1011	bwbb	0100	GXandInverted 0100

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1100	bbww	0011	GXcopy	0011

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1101	bbwb	0010	GXandReverse 0010

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1110	bbbw	0001	GXand	0001

Src	1100	bbww	0011
Dst	1010	bwbw	0101
Result	1111	bbbb	0000	GXclear	0000

Therefore, the X transfer mode is exactly bit reverse of the graphic
transfer mode.

 */

    int xMode = (~self->header.graphic.transferMode) & 0xF;

/* Setup the foreground and background colors right. One would like to do this
 * using the transfer modes, but that is not the way X11 works...
 */
    if (self->header.graphic.transferMode == graphic_WHITE)  {
        xMode = GXcopy;
        if (prevValue != graphic_WHITE)  {
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->foregroundpixel);
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->backgroundpixel);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->backgroundpixel);
            self->lastFillPixel = self->backgroundpixel;
        }
    }
#ifndef PLANEMASK_ENV
    else if (self->header.graphic.transferMode == graphic_XOR)  {
        if (prevValue != graphic_XOR)  {
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XGC(self),	0);
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), 0);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->foregroundpixel ^ self->backgroundpixel);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel ^ self->backgroundpixel);
	    self->lastFillPixel = self->foregroundpixel ^ self->backgroundpixel;
	}
	xMode = GXxor;
    }
#endif /* PLANEMASK_ENV */
    else {
        if (prevValue == graphic_WHITE || prevValue == graphic_XOR)  {
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->backgroundpixel);
            XSetBackground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->backgroundpixel);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->foregroundpixel);
            XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel);
            self->lastFillPixel = self->foregroundpixel;
        }
        if (self->header.graphic.transferMode == graphic_BLACK)
            xMode = GXcopy;
    }

    if (self->header.graphic.transferMode == graphic_INVERT)  {
	if (prevValue != graphic_INVERT)  {
	    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel ^ self->backgroundpixel);
	    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel ^ self->backgroundpixel);
	}
    }
    else  {
	if (prevValue == graphic_INVERT)  {
	    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XGC(self), AllPlanes);
	    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XFillGC(self), AllPlanes);
	}
    }

    if(xMode==GXcopy && self->flipforstipple) {
	XSetFunction(xgraphic_XDisplay(self), xgraphic_XGC(self), GXcopyInverted);
    } else {
	XSetFunction(xgraphic_XDisplay(self),
                  xgraphic_XGC(self),
		     xMode);
    }
    XSetFunction(xgraphic_XDisplay(self),
                  self->localFillGraphicContext,
		  xMode);

    if(self->header.graphic.transferMode == graphic_XOR) {
	struct xgraphic *xgraphicGrayShade();
	struct xgraphic *tile=xgraphicGrayShade(self, 16);
	XSetStipple(xgraphic_XDisplay(self),  xgraphic_XGC(self), tile->localWindow);
    } else if(self->lastStipple) XSetStipple(xgraphic_XDisplay(self), xgraphic_XGC(self), self->lastStipple->localWindow);
}



void xgraphic__DrawLineTo(self, XEnd, YEnd)
struct xgraphic * self;
long YEnd; 
long XEnd;{
    VerifyUpdateClipping(self);

    XDrawLine(xgraphic_XDisplay(self),
	  xgraphic_XWindow(self),
	  xgraphic_XGC(self),
	  physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	  physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint),
	  physical_LogicalXToGlobalX(self,XEnd),
	  physical_LogicalYToGlobalY(self,YEnd));
    point_SetPt(&self->header.graphic.currentPoint,XEnd,YEnd);
}

void xgraphic__DrawLine(self,DeltaX, DeltaY)
struct xgraphic * self;
long DeltaY; 
long DeltaX; {

    struct point OldPt;

    VerifyUpdateClipping(self);

    OldPt = self->header.graphic.currentPoint;
    point_OffsetPoint(&self->header.graphic.currentPoint,DeltaX,DeltaY);
    XDrawLine(xgraphic_XDisplay(self),
	  xgraphic_XWindow(self),
	  xgraphic_XGC(self),
	  physical_LogicalPtToGlobalX(self,&OldPt),
	  physical_LogicalPtToGlobalY(self,&OldPt),
	  physical_LogicalPtToGlobalX(self,&self->header.graphic.currentPoint),
	  physical_LogicalPtToGlobalY(self,&self->header.graphic.currentPoint));

}

	static void 
ReallySetFont(self)
	struct xgraphic *self;
{
	/* Select the font for later text drawing:
		Note: only localGC used for text, not fillGC */
	XFontStruct *f = (XFontStruct *)xfontdesc_GetRealFontDesc(
			(struct xfontdesc *)self->header.graphic.currentFont, self);
	if (f != NULL)  
		XSetFont(xgraphic_XDisplay(self),  xgraphic_XGC(self), f->fid);
}


void xgraphic__SetFont(self, ChosenFont)
struct xgraphic * self;
struct xfontdesc * ChosenFont;{

    if (ChosenFont) {
	if (self->header.graphic.currentFont == (struct fontdesc *) ChosenFont)  {
	    return;
	}
	else {
	    self->header.graphic.currentFont = (struct fontdesc *) ChosenFont;
	}
    }
    /* Damn, nothing there, so switch back to default */
    else {
        self->header.graphic.currentFont = fontdesc_Create("andysans", fontdesc_Plain, 12);
    }

    ReallySetFont(self);
}


/* Descriptions of string formats for string and text */

#define xgraphic_NULLTERMINATED 0
#define xgraphic_LENGTHGIVEN 1

static void xgraphic_DrawChars(self,Text,Operation,StringMode,TextLength)
struct xgraphic * self;
char * Text;
short Operation; 
short StringMode;
long TextLength; {

    register XCharStruct *maxChar;
    long x = point_X(&self->header.graphic.currentPoint);
    long y = point_Y(&self->header.graphic.currentPoint);

    VerifyUpdateClipping(self);

    /* Do we need to generate a count? */
    if (StringMode==xgraphic_NULLTERMINATED) TextLength = strlen(Text);

    if (Operation /* !=graphic_NOMOVEMENT */) {
        /* GetRealFontDesc is used to load the font cache in fontdesc */
        maxChar =
        &fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self)->dummy.max_bounds;

        if (Operation&
	   (graphic_ATTOP|graphic_BETWEENTOPANDBOTTOM|graphic_ATBOTTOM)){
	    y += maxChar->ascent;
        }
        if (Operation&graphic_BETWEENTOPANDBASELINE) {
            y += maxChar->ascent >> 1;
        }
        if (Operation&graphic_ATBOTTOM) {
	    y -= maxChar->ascent + maxChar->descent;
        }
        if (Operation&graphic_BETWEENTOPANDBOTTOM) {
	    y -= (maxChar->ascent + maxChar->descent)>>1;
        }
        if (Operation&(graphic_ATRIGHT|graphic_BETWEENLEFTANDRIGHT)) {
	    long LastXWidth;
	    LastXWidth = XTextWidth(
	        &(fontdesc_GetRealFontDesc(self->header.graphic.currentFont, self)->dummy),
	        Text,TextLength);
	    if (Operation&graphic_ATRIGHT) {
	        x -= LastXWidth;
	    } else {
	        x -= LastXWidth>>1;
	    }
        }

    }
    /* Put out the actual characters */
    
    if (self->header.graphic.spaceShim) {
	/* Have space shim, must break up text and dump it */

#define InitialWordGuess 10

	static XTextItem * tip = NULL;
	static int NumAlloc = InitialWordGuess;
        int WdCnt;
	char * WordStart;
	int i;
	int CharCount;

	if( tip == NULL )
	    tip = (XTextItem *)
			 malloc(NumAlloc * sizeof(XTextItem));
	tip[0].chars = Text;
        tip[0].delta = 0;
	tip[0].font = 0; /* use GC font */
	WordStart = Text;
	CharCount = 0;
	WdCnt = 0;
	for(i=0;i<TextLength;i++) {
	    CharCount++;
	    if (Text[i]==' ') {
		/* Finish current slot (include the space) */
		tip[WdCnt].nchars = CharCount;
		/* Advance the pointers */
		WdCnt++;
		WordStart += CharCount;
		CharCount = 0;
		/* Room for one more? */
	        if (WdCnt == NumAlloc) {
		    /* Used up everything, allocate some more space */
		    NumAlloc += InitialWordGuess;
		    tip = (XTextItem *)
			 realloc(tip, NumAlloc * sizeof(XTextItem));
		}
		/* Fill in next slot */
		tip[WdCnt].chars = WordStart;
		tip[WdCnt].delta = self->header.graphic.spaceShim;
		tip[WdCnt].font = 0; /* use GC font */
	    }
	}
	/* Check to see if a last word is present */
	if (CharCount) {
	    /* Yes, so fill in remaining values */
	    tip[WdCnt].nchars = CharCount;
	    WdCnt++;
	}
	/* Go write it */
	XDrawText(xgraphic_XDisplay(self),
	    xgraphic_XWindow(self),
	    xgraphic_XGC(self),
	    physical_LogicalXToGlobalX(self,x),
	    physical_LogicalYToGlobalY(self,y),tip,WdCnt);
    } /* end of space shim test */
    else {
	/* No shim, we can just go for it */
	XDrawString(xgraphic_XDisplay(self),
	    xgraphic_XWindow(self),
	    xgraphic_XGC(self),
	    physical_LogicalXToGlobalX(self,x),
	    physical_LogicalYToGlobalY(self,y),Text,TextLength);	
    }

}

void xgraphic__DrawString(self, Text, Operation)
struct xgraphic * self;
char * Text;
short Operation; {

    xgraphic_DrawChars(self,Text,Operation,xgraphic_NULLTERMINATED,0);
}


void xgraphic__DrawText(self, Text, TextLength, Operation)
struct xgraphic * self;
char * Text;
long TextLength;
short Operation; {

    xgraphic_DrawChars(self,Text,Operation,xgraphic_LENGTHGIVEN,
			    TextLength);
}

void xgraphic__DrawRectSize(self,x, y,width,height)
struct xgraphic * self;
long x,y,width,height;
{
    VerifyUpdateClipping(self);

    XDrawRectangle(xgraphic_XDisplay(self),
		xgraphic_XWindow(self),
		xgraphic_XGC(self),
		physical_LogicalXToGlobalX(self,x),
		physical_LogicalYToGlobalY(self,y),
		width,height);

}

void xgraphic__DrawPolygon(self, PointArray, PointCount)
struct xgraphic * self;
struct point * PointArray;
short PointCount; {
    static XPoint * PolygonPts = NULL;
    static int numXPoints = 0;
    int i;

    VerifyUpdateClipping(self);

    if( numXPoints < (PointCount + 1) ) {
	numXPoints = PointCount + 1;
	if( PolygonPts == NULL )
	    PolygonPts = (XPoint *) malloc(sizeof(XPoint) * numXPoints);
	else
	    PolygonPts = (XPoint *) realloc(PolygonPts, numXPoints * sizeof(XPoint));
    }
    for(i=0;i<PointCount;i++){
	PolygonPts[i].x = physical_LogicalXToGlobalX(self, PointArray[i].x);
	PolygonPts[i].y = physical_LogicalYToGlobalY(self, PointArray[i].y);
	}
    PolygonPts[PointCount].x = physical_LogicalXToGlobalX(self, PointArray[0].x);
    PolygonPts[PointCount].y = physical_LogicalYToGlobalY(self, PointArray[0].y);
    XDrawLines(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), PolygonPts, PointCount+1, CoordModeOrigin);
}

void xgraphic__DrawPath(self, PointArray, PointCount)
struct xgraphic * self;
struct point * PointArray;
short PointCount; {
    static XPoint * PolygonPts = NULL;
    static int numXPoints = 0;
    int i;

    VerifyUpdateClipping(self);

    if( numXPoints < PointCount ) {
	numXPoints = PointCount;
	if( PolygonPts == NULL )
	    PolygonPts = (XPoint *) malloc(sizeof(XPoint) * numXPoints);
	else
	    PolygonPts = (XPoint *) realloc(PolygonPts, numXPoints * sizeof(XPoint));
    }
    for(i=0;i<PointCount;i++){
	PolygonPts[i].x = physical_LogicalXToGlobalX(self, PointArray[i].x);
	PolygonPts[i].y = physical_LogicalYToGlobalY(self, PointArray[i].y);
	}
    XDrawLines(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), PolygonPts, PointCount, CoordModeOrigin);
}

void xgraphic__DrawOvalSize(self, x,y,width,height)
struct xgraphic * self;
long x,y,width,height; {

    VerifyUpdateClipping(self);

    XDrawArc(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), physical_LogicalXToGlobalX(self,x), physical_LogicalYToGlobalY(self,y), width, height, 0, 360*64);

}

void xgraphic__DrawArcSize(self,x,y,width,height, StartAngle, OffsetAngle)
struct xgraphic * self;
long x,y,width,height;
short StartAngle;
short OffsetAngle;{
    int StartXAngle, OffsetXAngle;

    VerifyUpdateClipping(self);

    StartXAngle = (90-StartAngle) <<6;
    OffsetXAngle = (-OffsetAngle) <<6;

    XDrawArc(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), physical_LogicalXToGlobalX(self,x), physical_LogicalYToGlobalY(self,y), width, height, StartXAngle, OffsetXAngle);
}

void xgraphic__DrawRRectSize(self,x,y,width,height,cornerWidth,cornerHeight)
struct xgraphic * self;
long x,y,width,height;
long cornerHeight, cornerWidth; {

    VerifyUpdateClipping(self);

    /* Handle pathologic cases in system indepedent manner
	(luser desires to bite bullet in efficiency) */

    if ( (2*cornerHeight >= height) || (2*cornerWidth >= width)) {
	/* Bizarre -- corners are bigger than rectangle, so 
	   make an appropriate looking oval */
	 if ( (2*cornerHeight >= height) && (2*cornerWidth >= width))
	    xgraphic_DrawOvalSize(self,x,y,width,height);
         else if (2*cornerHeight >= height) {
	    /* Draw left semi-oval */
	    xgraphic_DrawArcSize(self,x,y,2*cornerWidth,height,0,-180);
	    /* Draw Top line */
	    xgraphic_MoveTo(self,x+cornerWidth,y);
	    xgraphic_DrawLine(self,width-2*cornerWidth,0);
	    /* Draw right semi-oval */
	    xgraphic_DrawArcSize(self,x+width-2*cornerWidth,y,2*cornerWidth,height,0,180);
	    /* Draw bottom line */
	    xgraphic_MoveTo(self,x+cornerWidth,y+height);
	    xgraphic_DrawLine(self,width-2*cornerWidth,0);
	 }
	 else { /* assuming (2*cornerWidth >= width) */
	    /* Draw top semi-oval */
	    xgraphic_DrawArcSize(self,x,y,width,2*cornerHeight,-90,180);
	    /* Draw right line */
	    xgraphic_MoveTo(self,x+width,y+cornerHeight);
	    xgraphic_DrawLine(self,0,height-2*cornerHeight);
	    /* Draw bottom semi-oval */
	    xgraphic_DrawArcSize(self,x,y+height-2*cornerHeight,width,2*cornerHeight,90,180);
	    /* Draw left line */
	    xgraphic_MoveTo(self,x,y+cornerHeight);
	    xgraphic_DrawLine(self,0,height-2*cornerHeight);
	 }
	 return;
    }

{   XArc RRPath[8];
    long realX, realY, realEndX, realEndY;

    realX = physical_LogicalXToGlobalX(self,x);
    realY = physical_LogicalYToGlobalY(self,y);
    realEndX = realX + width;
    realEndY = realY + height;


    /* Upper left hand corner */
    RRPath[0].x = realX;
    RRPath[0].y = realY;
    RRPath[0].width = cornerWidth * 2;
    RRPath[0].height = cornerHeight * 2;
    RRPath[0].angle1 = -180 * 64; /* 9 o'clock */
    RRPath[0].angle2 = -90 * 64; /* 90 degreee clockwise turn */
    /* Top line */
    RRPath[1].x = realX + cornerWidth;
    RRPath[1].y = realY;
    RRPath[1].width = width - 2*cornerWidth;
    RRPath[1].height = 0;
    RRPath[1].angle1 = -180 * 64; /* 9 o'clock position */
    RRPath[1].angle2 = 180 * 64; /* move through straight line */
    /* upper right corner */
    RRPath[2].x = realEndX - 2*cornerWidth;
    RRPath[2].y = realY;
    RRPath[2].width = cornerWidth * 2;
    RRPath[2].height = cornerHeight * 2;
    RRPath[2].angle1 = 90 * 64; /* 12 o'clock position */
    RRPath[2].angle2 = -90 * 64; /* move through right turn */
    /* right side down */
    RRPath[3].x = realEndX;
    RRPath[3].y = realY + cornerHeight;
    RRPath[3].width = 0;
    RRPath[3].height = height - 2*cornerHeight;
    RRPath[3].angle1 = 90 * 64; /* 12 o'clock position */
    RRPath[3].angle2 = -180 * 64; /* line straight down */
    /* lower right corner */
    RRPath[4].x = realEndX - 2*cornerWidth;
    RRPath[4].y = realEndY - 2*cornerHeight;
    RRPath[4].width = cornerWidth * 2;
    RRPath[4].height = cornerHeight * 2;
    RRPath[4].angle1 = 0; /* 3 o'clock */
    RRPath[4].angle2 = -90 * 64; /* 90 degrees clockwise */
    /* Bottom line */
    RRPath[5].x = realX + cornerWidth;
    RRPath[5].y = realEndY;
    RRPath[5].width = width - 2*cornerWidth;
    RRPath[5].height = 0;
    RRPath[5].angle1 = 0; /* 3 o'clock */
    RRPath[5].angle2 = -180 * 64;  /* move 180 degrees */
    /* lower left corner */
    RRPath[6].x = realX;
    RRPath[6].y = realEndY - 2*cornerHeight;
    RRPath[6].width = cornerWidth * 2;
    RRPath[6].height = cornerHeight * 2;
    RRPath[6].angle1 = -90 * 64 ; /* 6 o'clock */
    RRPath[6].angle2 = -90 * 64; /* 90 degrees clockwise */
    /* left side */
    RRPath[7].x = realX;
    RRPath[7].y = realY + cornerHeight;
    RRPath[7].width = 0;
    RRPath[7].height = height - 2*cornerHeight;
    RRPath[7].angle1 = -90 * 64; /* 6 o'clock */
    RRPath[7].angle2 = -180 * 64; /* move 180 degrees */

    XDrawArcs(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), RRPath, 8);
}

}

void xgraphic__DrawTrapezoid(self,topX,topY,topWidth,bottomX,bottomY,bottomWidth)
struct xgraphic * self;
long topX, topY, topWidth, bottomX, bottomY, bottomWidth;
{
    XPoint PolygonPts[5];

    VerifyUpdateClipping(self);

    PolygonPts[0].x = PolygonPts[4].x = physical_LogicalXToGlobalX(self, topX);
    PolygonPts[0].y = PolygonPts[4].y = physical_LogicalYToGlobalY(self, topY);
    PolygonPts[1].x = physical_LogicalXToGlobalX(self, topX+topWidth);
    PolygonPts[1].y = physical_LogicalYToGlobalY(self, topY);
    PolygonPts[2].x = physical_LogicalXToGlobalX(self, bottomX+bottomWidth);
    PolygonPts[2].y = physical_LogicalYToGlobalY(self, bottomY);
    PolygonPts[3].x = physical_LogicalXToGlobalX(self, bottomX);
    PolygonPts[3].y = physical_LogicalYToGlobalY(self, bottomY);

    XDrawLines(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XGC(self), PolygonPts, 5, CoordModeOrigin);
}

static void xgraphic_SetupFillGC(self, Tile)
struct xgraphic * self;
struct xgraphic * Tile; {
    int grayIndex;

struct xgraphic * tile = Tile;
register long	fgPixel;

    /* See if transfer mode will take care of it, i.e., mode is source independent. If so, just make sure that a fillsolid mode is picked in the belief that the server won't be smart enough to realize that only the shape matters and not to waste time aligning any random tile that was left over */
    if ( (self->header.graphic.transferMode == graphic_BLACK) ||
	 (self->header.graphic.transferMode == graphic_WHITE) ||
         (self->header.graphic.transferMode == graphic_INVERT) ||
	 (self->header.graphic.transferMode == graphic_DEST) ) {
	 /* source independent, just optimize server */
         if (self->lastFillStyle != FillSolid) {
	    XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillSolid);
	    self->lastFillStyle = FillSolid;
	 }
	 return;
    }

    if ( tile == NULL &&
	(( xgraphic_DisplayClass( self ) & graphic_Color ) /* ||
	 ( xgraphic_DisplayClass( self ) & graphic_StaticGray ) */))
      tile = ( struct xgraphic * ) xgraphic_BlackPattern(self);
    else if ( tile == NULL )
      tile = (self->lastFillTile != NULL) ? self->lastFillTile : (struct xgraphic *) xgraphic_BlackPattern(self);

    if(self->header.graphic.transferMode == graphic_XOR ) {
	if(tile==(struct xgraphic *)xgraphic_BlackPattern(self)) tile = self->gray_shades[16];
    }
    
    /* Hm, depends on sources, but source may be white or black, so let's special case those as well */
    if (!(xgraphic_DisplayClass(self) & graphic_StaticGray)
	&& self->gray_levels[16] != NULL && self->gray_levels[16] == tile) {
	/* We're using black, make sure context is OK */
	fgPixel = (self->header.graphic.transferMode == graphic_XOR) ? self->foregroundpixel ^ self->backgroundpixel : self->foregroundpixel;

        if (self->lastFillStyle != FillSolid) {
	    XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillSolid);
	    self->lastFillStyle = FillSolid;
	}
        if (self->lastFillPixel != fgPixel) {
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self),  fgPixel);
	    self->lastFillPixel = fgPixel;
	}
	self->lastFillTile = self->gray_levels[16];
    }
    else if (!(xgraphic_DisplayClass(self) & graphic_StaticGray)
	     && self->gray_levels[0] != NULL && self->gray_levels[0] == tile) {
	/* We're using white, make sure content is OK */

       if (self->lastFillStyle != FillSolid) {
	    XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillSolid);
	    self->lastFillStyle = FillSolid;
	}
        if (self->lastFillPixel !=  self->backgroundpixel) {
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self),  self->backgroundpixel);
	    self->lastFillPixel = self->backgroundpixel;
	}
	self->lastFillTile = self->gray_levels[0];
    }
    /* Not black or white, but maybe predefind gray that is already down loaded, if so just set fill style and don't download pixmap again. Note: we are exceedingly tricky by using the assignment statement to pickup the gray shade that matches, and by picking what we think will be most common ones first. We assume, as per C book, that evaluatio stops with first true test.  We include 0 and 16 for the monochrome case. */
    else   {
	register int i;
	static int ind[] = { 0, 16, 8, 4, 12, 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15};

	if (tile != NULL)  {
	    for (i = 0; i < (sizeof(ind)/sizeof(int)) && self->gray_shades[ind[i]] != tile; i++)
		;
	}
	if (i < (sizeof(ind)/sizeof(int)))  {
	    grayIndex = ind[i];

	    /* Lucky us, reusing a preloaded gray shade, so just make sure that fill style is set correctly, and see if we have alredy downloaded tile */
	    fgPixel = (self->header.graphic.transferMode == graphic_XOR) ? self->foregroundpixel ^ self->backgroundpixel : self->foregroundpixel;
	    
	    if (self->lastFillPixel != fgPixel) {
		XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self),  fgPixel);
		self->lastFillPixel = fgPixel;
	    }
	    if (self->lastFillStyle != FillOpaqueStippled) {
		XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillOpaqueStippled );
		self->lastFillStyle = FillOpaqueStippled;
	    }
	    if (self->lastFillTile != self->gray_shades[grayIndex]) {
		/* oh well, let's download the gray pattern */
		XSetStipple(xgraphic_XDisplay(self),
			 self->localFillGraphicContext,
			 tile->localWindow);
		self->lastFillTile = self->gray_shades[grayIndex];
	    }
	}
	/* Unknown, or unused tile, so download it and use it */
	else {
	    fgPixel = (self->header.graphic.transferMode == graphic_XOR) ? self->foregroundpixel ^ self->backgroundpixel : self->foregroundpixel;

	    if (self->lastFillPixel != fgPixel) {
		XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self),  fgPixel);
		self->lastFillPixel = fgPixel;
	    }
	    /* Let's see if style is right */
	    if (self->lastFillStyle != FillOpaqueStippled) {
		XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillOpaqueStippled);
		self->lastFillStyle = FillOpaqueStippled;
	    }
	    /* And let's send down the tile */
	    XSetStipple(xgraphic_XDisplay(self),
		     self->localFillGraphicContext,
		     tile->localWindow);
	    self->lastFillTile = tile;

	}
    }
}

void xgraphic__FillRectSize(self,x,y,width,height,Tile)
struct xgraphic * self;
long x,y,width,height;
struct xgraphic * Tile; {

    if (width <= 0 || height <= 0)  return;

    VerifyUpdateClipping(self);

    /* First change the pattern to match the tile
	    (we should special case white and black for efficiency) */
    xgraphic_SetupFillGC(self, Tile);

    /* And fill */
    XFillRectangle(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), physical_LogicalXToGlobalX(self,x), physical_LogicalYToGlobalY(self,y), width, height);
}


void xgraphic__FillPolygon(self, PointArray, PointCount, Tile)
struct xgraphic *self;
struct point *PointArray;
short PointCount;
struct xgraphic *Tile;{

    static XPoint *PolygonPts = NULL;
    static int numXPoints = 0;
    int i;

    VerifyUpdateClipping(self);

    if( numXPoints < PointCount ) {
	numXPoints = PointCount;
	if( PolygonPts == NULL )
	    PolygonPts = (XPoint *) malloc(sizeof(XPoint) * numXPoints);
	else
	    PolygonPts = (XPoint *) realloc(PolygonPts, numXPoints * sizeof(XPoint));
    }
    for(i=0;i<PointCount;i++){
	PolygonPts[i].x = physical_LogicalXToGlobalX(self, PointArray[i].x);
	PolygonPts[i].y = physical_LogicalYToGlobalY(self, PointArray[i].y);
	}
	
    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);

    XFillPolygon(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), PolygonPts, PointCount, Complex, CoordModeOrigin);

}

void xgraphic__FillOvalSize(self,x,y,width,height,Tile)
struct xgraphic * self;
long x,y,width,height;
struct xgraphic * Tile;{

    VerifyUpdateClipping(self);

    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);

    XFillArc(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), physical_LogicalXToGlobalX(self,x), physical_LogicalYToGlobalY(self,y), width, height, 0, 360 * 64);

}

void xgraphic__FillArcSize(self,x,y,width,height,StartAngle, OffsetAngle,Tile)
struct xgraphic * self;
long x,y,width,height;
short StartAngle;
short OffsetAngle;
struct xgraphic * Tile;{
    int StartXAngle, OffsetXAngle;

    VerifyUpdateClipping(self);

    StartXAngle = ((-StartAngle)+90) <<6;
    OffsetXAngle = (-OffsetAngle) <<6;

    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);
/*    XSetTile(xgraphic_XDisplay(self),
		xgraphic_XFillGC(self),
		Tile->localWindow,0,0); */
    XFillArc(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), physical_LogicalXToGlobalX(self,x), physical_LogicalYToGlobalY(self,y), width, height, StartXAngle, OffsetXAngle);

}


void xgraphic__FillRRectSize(self,x,y,width,height,cornerWidth,cornerHeight,Tile)
struct xgraphic * self;
long x,y,width,height;
long cornerWidth, cornerHeight;
struct xgraphic * Tile;{
    /* Handle pathologic cases in system indepedent manner
	(luser desires to bite bullet in efficiency) */

    VerifyUpdateClipping(self);

    if ( (2*cornerHeight >= height) || (2*cornerWidth >= width)) {
	/* Bizarre -- corners are bigger than rectangle, so 
	   make an appropriate looking oval */
	 if ( (2*cornerHeight >= height) && (2*cornerWidth >= width))
	    xgraphic_FillOvalSize(self,x,y,width,height,Tile);
         else if (2*cornerHeight >= height) {
	    /* Fill left semi-oval */
	    xgraphic_FillArcSize(self,x,y,2*cornerWidth,height,0,-180,Tile);
	    /* Fill vertical rectangle */
	    xgraphic_FillRectSize(self,x+cornerWidth,y,width-2*cornerWidth,height,Tile);
	    /* Fill right semi-oval */
	    xgraphic_FillArcSize(self,x+width-2*cornerWidth,y,2*cornerWidth,height,0,180,Tile);
	 }
	 else { /* assuming (2*cornerWidth >= width) */
	    /* Fill top semi-oval */
	    xgraphic_FillArcSize(self,x,y,width,2*cornerHeight,-90,180,Tile);
	    /* Fill horizontal rectangle */
	    xgraphic_FillRectSize(self,x,y+cornerHeight,width,height-2*cornerHeight,Tile);
	    /* Fill bottom semi-oval */
	    xgraphic_FillArcSize(self,x,y+height-2*cornerHeight,width,2*cornerHeight,90,180,Tile);
	 }
         return;
    }

{   XArc FourCorners[4];
    XRectangle InteriorRects[3];
    int i;
    long realX, realY, realEndX, realEndY;

    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);
/*    XSetTile(xgraphic_XDisplay(self),
		xgraphic_XFillGC(self),
		Tile->localWindow,0,0); */

    /* Now, we have to fill in four arcs and three rectangles */

    realX = physical_LogicalXToGlobalX(self,x);
    realY = physical_LogicalYToGlobalY(self,y);
    realEndX = realX + width;
    realEndY = realY + height;
    for (i=0;i<4;i++) {
	FourCorners[i].width = 2 * cornerWidth;
	FourCorners[i].height = 2* cornerHeight;
	FourCorners[i].angle2 = -90 << 6; /* make a clockwise right angle */
	}
    FourCorners[0].x = realX;
    FourCorners[0].y = realY;
    FourCorners[0].angle1 = 180 * 64; /* 9 o'clock */
    FourCorners[1].x = realEndX-2*cornerWidth;
    FourCorners[1].y = realY;
    FourCorners[1].angle1 = 90 * 64; /* 12 o'clock */
    FourCorners[2].x = realEndX-2*cornerWidth;
    FourCorners[2].y = realEndY-2*cornerHeight;
    FourCorners[2].angle1 = 0; /* 3 o'clock */
    FourCorners[3].x = realX;
    FourCorners[3].y = realEndY-2*cornerHeight;
    FourCorners[3].angle1 = -90 * 64; /* 6 o'clock */
    XFillArcs(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), FourCorners, 4);

    InteriorRects[0].x = realX+cornerWidth;
    InteriorRects[0].y = realY;
    InteriorRects[0].width = width - 2*cornerWidth;
    InteriorRects[0].height = height;
    InteriorRects[1].x = realX;
    InteriorRects[1].y = realY+cornerHeight;
    InteriorRects[1].width = cornerWidth;
    InteriorRects[1].height = height - 2* cornerHeight;
    InteriorRects[2].x = realEndX - cornerWidth;
    InteriorRects[2].y = realY+cornerHeight;
    InteriorRects[2].width = cornerWidth;
    InteriorRects[2].height = height - 2 * cornerHeight;
    XFillRectangles(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), InteriorRects, 3);
}

}

void xgraphic__FillRgn(self,Rgn,Tile)
struct xgraphic * self;
struct region * Rgn;
struct xgraphic * Tile;{
    struct region * tmpRegion;
    struct region * visRegion;
    struct xgraphic_UpdateBlock * curBlock;

/*    VerifyUpdateClipping(self); */ /* unneeded here since we will explicitly pull out the update region for our graphics hack */

    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);

    /* Now we do something tricky: we set the clipping rectangles of the GC to match those of the region and then fill everything. The steps are:
    1. Copy the region
    2. Intersect with visual rectangle
    3. Intersect with clipping rectangle
    4. Offset to physical rectangle
    5. set to clipping gc
    6. fill everything
    7. toss extra region
    8. restore clipping rectangle
    */
    /* Step 1 - make a copy */
    tmpRegion = region_DuplicateRegion(Rgn);

    /* Step 2 - clip to visual */
    if (self->header.graphic.visualRegion == NULL) {
	visRegion = region_CreateRectRegion(&self->header.graphic.visualBounds);
	region_IntersectRegion(tmpRegion,visRegion,tmpRegion);
	region_Destroy(visRegion);
    }
    else {
	region_IntersectRegion(tmpRegion, self->header.graphic.visualRegion, tmpRegion);
    }

    /* Step 3 - clip to graphic clipping */
    if (self->header.graphic.clippingRegion != NULL) {
        region_IntersectRegion(tmpRegion, self->header.graphic.clippingRegion, tmpRegion);
    }

    /* Step 4 - offset by physical coordinates */
    region_OffsetRegion(tmpRegion, physical_LogicalXToGlobalX(self,0), physical_LogicalYToGlobalY(self,0));

    /* Step 4a Check for compounding update regions */
    curBlock = xgraphic_FindUpdateBlock(xgraphic_XDisplay(self), xgraphic_XWindow(self));
    if (curBlock->updateRegionInUse) XIntersectRegion(tmpRegion->regionData, curBlock->updateRegionInUse, tmpRegion->regionData);

    /* Step 5 - installing clipping rectangles */
#ifdef XRELEASE2_ENV
    TempXSetRegion(xgraphic_XDisplay(self), xgraphic_XFillGC(self), tmpRegion->regionData);
#else /* XRELEASE2_ENV */
    XSetRegion(xgraphic_XDisplay(self), xgraphic_XFillGC(self), tmpRegion->regionData);
#endif /* XRELEASE2_ENV */

    /* Step 6 - fill entire visual */
    XFillRectangle(xgraphic_XDisplay(self),
	xgraphic_XWindow(self),
	xgraphic_XFillGC(self),
        point_X(&self->header.graphic.physicalOrigin),
        point_Y(&self->header.graphic.physicalOrigin),
        xgraphic_GetVisualWidth(self),
        xgraphic_GetVisualHeight(self));

    /* reset the clipping rectangle */
    xgraphic_LocalSetClippingRect(self,curBlock);

    /* throw away temp region */
    region_Destroy(tmpRegion);
}

void xgraphic__FillTrapezoid(self, topX, topY, topWidth, bottomX, bottomY, bottomWidth, Tile)
struct xgraphic * self, *Tile;
long topX, topY, topWidth, bottomX, bottomY, bottomWidth; {
    XPoint PolygonPts[4];

    VerifyUpdateClipping(self);

    PolygonPts[0].x = physical_LogicalXToGlobalX(self, topX);
    PolygonPts[0].y = physical_LogicalYToGlobalY(self, topY);
    PolygonPts[1].x = physical_LogicalXToGlobalX(self, topX+topWidth);
    PolygonPts[1].y = physical_LogicalYToGlobalY(self, topY);
    PolygonPts[2].x = physical_LogicalXToGlobalX(self, bottomX+bottomWidth);
    PolygonPts[2].y = physical_LogicalYToGlobalY(self, bottomY);
    PolygonPts[3].x = physical_LogicalXToGlobalX(self, bottomX);
    PolygonPts[3].y = physical_LogicalYToGlobalY(self, bottomY);
	
    /* First change the pattern to match the tile (we should special case white and black for efficiency) */
       xgraphic_SetupFillGC(self, Tile);

    XFillPolygon(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XFillGC(self), PolygonPts, 4, Convex, CoordModeOrigin);
}


void xgraphic__BitBlt(self, SrcRect, DstGraphic, DstOrigin, ClipRect)
struct xgraphic * self;
struct rectangle * SrcRect;
struct graphic *DstGraphic;
struct point * DstOrigin;
struct rectangle * ClipRect;
{
    if (rectangle_Width(SrcRect) != 0 && rectangle_Height(SrcRect) != 0)  {

	VerifyUpdateClipping(self);
	VerifyUpdateClipping((struct xgraphic *)DstGraphic);
	if (bltDebug) {
	    printf("SrcWin: %ld DestWin: %ld\n",
		   xgraphic_XWindow(self),
		   xgraphic_XWindow((struct xgraphic *)DstGraphic));
	    printf("src (%d,%d)  %ud x %ud  dest (%d,%d)\n",
		  (int)physical_LogicalXToGlobalX(self,rectangle_Left(SrcRect)),
		  (int)physical_LogicalYToGlobalY(self,rectangle_Top(SrcRect)),
		  (unsigned int)rectangle_Width(SrcRect),
		  (unsigned int)rectangle_Height(SrcRect),
		  (int)physical_LogicalXToGlobalX(DstGraphic,point_X(DstOrigin)),
		  (int)physical_LogicalYToGlobalY(DstGraphic,point_Y(DstOrigin)));
	}
	if(self->flipforstipple) {
	    XSetFunction(xgraphic_XDisplay(self), xgraphic_XGC((struct xgraphic *)DstGraphic), GXcopy);
	}
	XCopyArea(xgraphic_XDisplay(self), xgraphic_XWindow(self), xgraphic_XWindow((struct xgraphic *)DstGraphic),
		  xgraphic_XGC((struct xgraphic *)DstGraphic),
		  (int)physical_LogicalXToGlobalX(self,rectangle_Left(SrcRect)),
		  (int)physical_LogicalYToGlobalY(self,rectangle_Top(SrcRect)),
		  (unsigned int)rectangle_Width(SrcRect),
		  (unsigned int)rectangle_Height(SrcRect),
		  (int)physical_LogicalXToGlobalX(DstGraphic,point_X(DstOrigin)),
		  (int)physical_LogicalYToGlobalY(DstGraphic,point_Y(DstOrigin)));
	if(self->flipforstipple) {
	    XSetFunction(xgraphic_XDisplay(self), xgraphic_XGC((struct xgraphic *)DstGraphic), GXcopyInverted);
	}
	xgraphic_FlushGraphics(self);
    }
}

void xgraphic__SetBitAtLoc(self,XPos,YPos,NewValue)
struct xgraphic * self;
long XPos, YPos;
boolean NewValue; {

    VerifyUpdateClipping(self);

/* slimy, but works. We reuse the fill gc, set the mode to copy, the fill style to solid and set the pixel as appropriate. The mode gets reset by the localtransfer calculation, and the fill mode gets reset whenever encessary by the next fill operation */

    XSetFunction(xgraphic_XDisplay(self),
	    xgraphic_XFillGC(self),
	    GXcopy);

    XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XFillGC(self), FillSolid);
    self->lastFillStyle = FillSolid;

    if (NewValue == 0) self->lastFillPixel = self->backgroundpixel;
    else if (NewValue == 1) self->lastFillPixel = self->foregroundpixel;
    else { /* error */
    }

    XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->lastFillPixel);
    
    xgraphic_LocalSetTransferFunction(self, graphic_COPY);
}

static XImage *PixImage = NULL;
static Display *PixDisplay = NULL;

static void
SetUpPixImage(self, pixelimage)
	register struct xgraphic *self;
	register struct pixelimage *pixelimage;
{
    VerifyUpdateClipping(self);

    if(!PixImage || xgraphic_XDisplay(self) != PixDisplay) {
	/* make a new PixImage */
	if (PixImage != NULL) {
	    XDestroyImage(PixImage);
	}
	PixDisplay = xgraphic_XDisplay(self);
	PixImage = XCreateImage(PixDisplay, DefaultVisualOfScreen( DefaultScreenOfDisplay(PixDisplay)), 1, XYBitmap, 0, NULL, 0, 0, 16, 0);
	/* XXX ? Should the following affect the routines in the PixImage ??? */
	PixImage->bitmap_bit_order = MSBFirst;
	PixImage->byte_order = MSBFirst;
    }
    /* fill in the PixImage */
    PixImage->width = pixelimage->pixelsPerRow;
    PixImage->height = pixelimage->numRows;
    PixImage->bytes_per_line = pixelimage->RowWidth;
    PixImage->data = (char *)pixelimage->bits;
}
	

void 
xgraphic__WritePixImage(self, DestX, DestY, SrcPixels, SrcX, SrcY, width, height)
	register struct xgraphic *self;
	long DestX, DestY, SrcX, SrcY, width, height;
	struct pixelimage *SrcPixels;
{
    if (width > 0 && height > 0) {
	VerifyUpdateClipping(self);

	SetUpPixImage(self, SrcPixels);
	XPutImage(PixDisplay, 
		  xgraphic_XWindow(self),
		  xgraphic_XGC(self),
		  PixImage,
		  SrcX, SrcY, 
		  physical_LogicalXToGlobalX(self, DestX),
		  physical_LogicalYToGlobalY(self, DestY),
		  width, height);
    }
}
	
void 
xgraphic__ReadPixImage(self, SrcX, SrcY, DestPixels, DestX, DestY, width, height)
register struct xgraphic *self;
struct pixelimage *DestPixels;
long SrcX, SrcY, DestX, DestY, width, height;
{
    VerifyUpdateClipping(self);

    SetUpPixImage(self, DestPixels);
    /* The following will not necessarily work; it can fail and generate an X error. The necessary conditions are that the window be mapped and that the subrect requested be within the window bounds, or that the drawable be an off-screen pixmap.  */
    XGetSubImage(PixDisplay, 
		  xgraphic_XWindow(self),
		  physical_LogicalXToGlobalX(self, SrcX), physical_LogicalYToGlobalY(self, SrcY), width, height, 
		  1,  /* mask for only one plane,  XXX I hope.*/
		  XYPixmap, PixImage,  DestX, DestY);
}

/* find the best pixmap depth supported by the server for a particular
 * visual and return that depth.
 *
 * this is complicated by R3's lack of XListPixmapFormats so we fake it
 * by looking at the structure ourselves.
 */

static unsigned int 
bitsPerPixelAtDepth(disp, scrn, depth)
     Display      *disp;
     int           scrn;
     unsigned int  depth;
{
#if 1 /* the way things are */
  unsigned int a;

  for (a= 0; a < disp->nformats; a++)
    if (disp->pixmap_format[a].depth == depth)
      return(disp->pixmap_format[a].bits_per_pixel);

#else /* the way things should be */
  XPixmapFormatValues *xf;
  unsigned int nxf, a;

  xf = XListPixmapFormats(disp, &nxf);
  for (a = 0; a < nxf; a++)
    if (xf[a].depth == closest_depth)
      return(disp->pixmap_format[a].bits_per_pixel);
#endif

  /* this should never happen; if it does, we're in trouble
   */

  fprintf(stderr, "bitsPerPixelAtDepth: Can't find pixmap depth info!\n");
  exit(1);
}

unsigned long
Closest(r, g, b)
unsigned short r, g, b;
{
    register int i, j, k;
    unsigned int diff = 0, bestDiff = ~0;
    unsigned long best = 0;
    XColor *c;

    if(colorCube) {
	for(i = 0; i < colorCube->samplesPerComponent; i++)
	    for(j = 0; j < colorCube->samplesPerComponent; j++)
		for(k = 0; k < colorCube->samplesPerComponent; k++) {
		    c = colorCube->cube[i][j][k];
		    diff = abs(r - c->red) + abs(g - c->green) + abs(b - c->blue);
		    if(diff < bestDiff) {
			best = c->pixel;
			bestDiff = diff;
		    }
		}
    }
    return(best);
}

/***********************************/
XImageInfo *
imageToXImage( self, image, private_cmap, fit )
    struct xgraphic *self;
    struct image *image;
    unsigned int  private_cmap;
    unsigned int  fit;
{ 
/***********************************/
  Display *disp;
  int scrn;
  Visual *visual; /* visual to use */
  Pixel *redvalue, *greenvalue, *bluevalue;
  unsigned int newmap, linelen, dpixlen, dbits;
  register int x, y, a, b;
  unsigned short red, green, blue;
  struct xcolor *xc;
  XColor xcolor;
  XImageInfo *ximageinfo;
  unsigned int ddepth; /* depth of the visual to use */
  int visual_class;
  struct xcolormap **cmap = (struct xcolormap**) xgraphic_CurrentColormap(self);

  disp = xgraphic_XDisplay(self);
  scrn = DefaultScreen(disp);

  visual = DefaultVisual(disp, scrn);
  ddepth = DefaultDepth(disp, scrn);

  xcolor.flags = DoRed | DoGreen | DoBlue;
  redvalue = greenvalue = bluevalue = NULL;
  if(self->index) {
      self->numColorsInIndex = 0;
      free(self->index);
  }
  ximageinfo = (XImageInfo*) malloc(sizeof(XImageInfo));
  ximageinfo->disp = disp;
  ximageinfo->scrn = scrn;
  ximageinfo->depth = 0;
  ximageinfo->drawable = xgraphic_XWindow(self);
  ximageinfo->foreground = ximageinfo->background = 0;
  ximageinfo->cmap = (*cmap)->XColorMap;
  ximageinfo->gc = NULL;
  ximageinfo->ximage = NULL;

  /* process image based on type of visual we're sending to
   */

  switch (image->type) {
      case ITRUE:
	  switch (visual->class) {
	      case TrueColor:
	      case DirectColor:
		  /* goody goody */
		  break;
	      default:
		  if (visual->bits_per_rgb > 1)
		      image = image_Reduce(image, image_depthToColors(visual->bits_per_rgb));
		  else
		      image = image_Dither(image);
	  }
	  break;
      case IGREYSCALE:
      case IRGB:
	  switch(visual->class) {
	      case TrueColor:
	      case DirectColor:
		  /* no problem, we handle this just fine */
		  break;
	      default:
		  if (visual->bits_per_rgb > 1 && visual->map_entries > 2)
		      image = image_Reduce(image, image_depthToColors(visual->bits_per_rgb));
		  else
		      image = image_Dither(image);
		  break;
	  }
      case IBITMAP:
	  /* no processing ever needs to be done for bitmaps */
	  break;
  }

  /* do color allocation */
  if(xcolormap_Size(*cmap) > 2) {
      switch (visual->class) {
	  case TrueColor:
	  case DirectColor:
	      { Pixel pixval;
	      unsigned int redcolors, greencolors, bluecolors;
	      unsigned int redstep, greenstep, bluestep;
	      unsigned int redbottom, greenbottom, bluebottom;
	      unsigned int redtop, greentop, bluetop;

	      redvalue = (Pixel *) malloc(sizeof(Pixel) * 256);
	      greenvalue = (Pixel *) malloc(sizeof(Pixel) * 256);
	      bluevalue = (Pixel *) malloc(sizeof(Pixel) * 256);

	      retry_direct: 
/* tag we hit if a DirectColor allocation fails on
 * default colormap */
/* calculate number of distinct colors in each band */

		redcolors = greencolors = bluecolors = 1;
	      for (pixval = 1; pixval; pixval <<= 1) {
		  if (pixval & visual->red_mask)
		      redcolors <<= 1;
		  if (pixval & visual->green_mask)
		      greencolors <<= 1;
		  if (pixval & visual->blue_mask)
		      bluecolors <<= 1;
	      }

	  /* sanity check
	   */

	      if( (redcolors > visual->map_entries) ||
		 (greencolors > visual->map_entries) ||
		 (bluecolors > visual->map_entries)) {
		  if(imageDebug)
		      fprintf(stderr, "Warning: inconsistency in color information (this may be ugly)\n");
	      }

	      redstep = 256 / redcolors;
	      greenstep = 256 / greencolors;
	      bluestep = 256 / bluecolors;
	      redbottom = greenbottom = bluebottom = 0;
	      for (a = 0; a < visual->map_entries; a++) {
		  if (redbottom < 256)
		      redtop = redbottom + redstep;
		  if (greenbottom < 256)
		      greentop = greenbottom + greenstep;
		  if (bluebottom < 256)
		      bluetop = bluebottom + bluestep;

		  red = (redtop - 1) << 8;
		  green = (greentop - 1) << 8;
		  blue = (bluetop - 1) << 8;
		  if(! (xc = xcolormap_AllocColor(*cmap, NULL, red, green, blue, TRUE)) ) {

		  /* if an allocation fails for a DirectColor default visual then  we should create a private colormap and try again. */

		      if( (visual->class == DirectColor) &&
			 (visual == DefaultVisual(disp, scrn)) ) {
			  ximageinfo->cmap = XCreateColormap(disp, RootWindow(disp, scrn), visual, AllocNone);
			  goto retry_direct;
		      }

		  /* something completely unexpected happened
		   */

		      if (imageDebug)
			  fprintf(stderr, "imageToXImage: XAllocColor failed on a TrueColor/Directcolor visual\n");
		      return(NULL);
		  }
		  else {
		      xgraphic_AddObserver(self, xc);
		      xcolor_AddObserver(xc, self);
		  }

	      /* fill in pixel values for each band at this intensity
		  */

		  while ((redbottom < 256) && (redbottom < redtop))
		      redvalue[redbottom++]= xcolor.pixel & visual->red_mask;
		  while ((greenbottom < 256) && (greenbottom < greentop))
		      greenvalue[greenbottom++]= xcolor.pixel & visual->green_mask;
		  while ((bluebottom < 256) && (bluebottom < bluetop))
		      bluevalue[bluebottom++]= xcolor.pixel & visual->blue_mask;
	      }
	      }
	      break;

	  default:
	      retry: /* this tag is used when retrying because we couldn't get a fit */
		self->index = (Pixel *) malloc(sizeof(Pixel) * image->rgb->used);

 /* either create a new colormap or fit the image into the one we
 * have.  to create a new one, we create a private cmap and allocate
 * the colors writable.  fitting the colors is harder, we have to:
 *  1. grab the server so no one can goof with the colormap.
 *  2. count the available colors using XAllocColorCells.
 *  3. free the colors we just allocated.
 *  4. reduce the depth of the image to fit.
 *  5. allocate the colors again shareable.
 *  6. ungrab the server and continue on our way.
 * someone should shoot the people who designed X color allocation.
 */

	      if (private_cmap) {

		  if (fit)
		      XGrabServer(disp);

		  for (a = 0; a < image->rgb->used; a++, self->numColorsInIndex++) 
		  /* count entries we've got */
		      if (! XAllocColorCells(disp, ximageinfo->cmap, False, NULL, 0, self->index + a, 1))
			  break;

		  if (a > 0) {
		      XFreeColors(disp, ximageinfo->cmap, self->index, a, 0);
		      bzero(self->index, self->numColorsInIndex * sizeof(Pixel));
		      self->numColorsInIndex = 0;
		  }
		  
		  if (a == 0) {
		      free(self->index);
		      self->index = NULL;
		      self->numColorsInIndex = 0;
		      return(NULL);
		  }
		  
		  if (a <= 2) {
		      image = image_Dither(image);
		      fit = 0;
		      free(self->index);
		      self->index = NULL;
		      self->numColorsInIndex = 0;
		      goto retry;
		  }

		  if (a < image->rgb->used)
		      image = image_Reduce(image, a);

#define XStandardPixel(map, cols, i, shft) ((map)->base_pixel + \
  (((*(cols->red + i)>>8) * (map)->red_max + (1<<(shft-1))) >> shft) * \
		  (map)->red_mult + \
  (((*(cols->green + i)>>8) * (map)->green_max + (1<<(shft-1))) >> shft) * \
		  (map)->green_mult + \
  (((*(cols->blue + i)>>8) * (map)->blue_max + (1<<(shft-1))) >> shft) * \
		  (map)->blue_mult)

		  if (fit) {
		      for(a = 0; a < image->rgb->used; a++, self->numColorsInIndex++)
			  *(self->index + a) = haveMapInfo ?
			    XStandardPixel(&map_info, image->rgb, a, 8) :
			    Closest(*(image->rgb->red + a),
				    *(image->rgb->green + a),
				    *(image->rgb->blue + a));
		      XUngrabServer(disp);
		  }
		  else {
		      for (b = 0; b < a; b++) {
			  red = *(image->rgb->red + b);
			  green = *(image->rgb->green + b);
			  blue = *(image->rgb->blue + b);
			  xc = xcolormap_AllocColor(*cmap, NULL, red, green, blue, TRUE);
			  if(xc) {
			      *(self->index + b) = xcolor_Pixel(xc);
			      xgraphic_AddObserver(self, xc);
			      xcolor_AddObserver(xc, self);
			  }
		      }
		  }
	      }
	      else if ((visual == DefaultVisual(disp, scrn)) ||
		       (visual->class == StaticGray) ||
		       (visual->class == StaticColor)) {

/* allocate colors shareable (if we can)
 */
		  for (a = 0; a < image->rgb->used; a++)
		      *(self->index + a) = haveMapInfo ?
			XStandardPixel(&map_info, image->rgb, a, 8) :
			Closest(*(image->rgb->red + a),
				*(image->rgb->green + a),
				*(image->rgb->blue + a));
	      }
	      break;
      }
  }
/* create an XImage and related colormap based on the image type
 * we have.
 */

  switch(image->type) {
      case IBITMAP:
	  { byte *data;
	  int destlinelen = (image->width / 8) + (image->width % 8 ? 1 : 0);

	  data = (byte*) malloc(destlinelen * image->height);
	  bcopy((byte*) image->data, data, destlinelen * image->height);
	  ximageinfo->ximage = XCreateImage(disp, visual, 1, XYBitmap, 0, data, image->width, image->height, 8, 0);
	  ximageinfo->depth = ddepth;
	  ximageinfo->foreground = self->foregroundpixel;
	  ximageinfo->background = self->backgroundpixel;
	  ximageinfo->ximage->bitmap_bit_order = MSBFirst;
	  ximageinfo->ximage->byte_order = MSBFirst;
	  break;
	  }

      case IGREYSCALE:
      case IRGB:
      case ITRUE:

	  /* modify image data to match visual and colormap
	   */

	  dbits = bitsPerPixelAtDepth(disp, scrn, ddepth);
	  ximageinfo->depth = ddepth;
	  dpixlen= (dbits + 7) / 8;

	  switch (visual->class) {
	      case DirectColor:
	      case TrueColor:
		  { byte *data, *destptr, *srcptr;
		  Pixel pixval, newpixval;

		  ximageinfo->ximage = XCreateImage(disp, visual, ddepth, ZPixmap, 0, NULL, image->width, image->height, 8, 0);
		  data = (byte*) malloc(image->width * image->height * dpixlen);
		  ximageinfo->ximage->data = (char *)data;
		  destptr = data;
		  srcptr = image->data;
		  switch (image->type) {
		      case ITRUE:
			  for (y = 0; y < image->height; y++)
			      for (x = 0; x < image->width; x++) {
				  pixval = memToVal(srcptr, image->pixlen);
				  newpixval = redvalue[TRUE_RED(pixval)] |
				    greenvalue[TRUE_GREEN(pixval)] | bluevalue[TRUE_BLUE(pixval)];
				  valToMem(newpixval, destptr, dpixlen);
				  srcptr += image->pixlen;
				  destptr += dpixlen;
			      }
			  break;
		      case IGREYSCALE:
		      case IRGB:
			  for (y = 0; y < image->height; y++)
			      for (x = 0; x < image->width; x++) {
				  pixval = memToVal(srcptr, image->pixlen);
				  pixval = redvalue[image->rgb->red[pixval] >> 8] |
				    greenvalue[image->rgb->green[pixval] >> 8] |
				    bluevalue[image->rgb->blue[pixval] >> 8];
				  valToMem(pixval, destptr, dpixlen);
				  srcptr += image->pixlen;
				  destptr += dpixlen;
			      }
			  break;
		      default: /* something's broken */
			  return;
		  }
		  ximageinfo->ximage->byte_order = MSBFirst;
		  break;
		  }
	      default:

/* only IRGB images make it this far.
 */

/* if our XImage doesn't have modulus 8 bits per pixel, it's unclear
 * how to pack bits so we instead use an XYPixmap image.  this is
 * slower.
 */

		  if (dbits % 8) {
		      byte *data, *destdata, *destptr, *srcptr, mask;
		      Pixel pixmask, pixval;

		      ximageinfo->ximage = XCreateImage(disp, visual, ddepth, XYPixmap, 0, NULL, image->width, image->height, 8, 0);
		      data = (byte *)malloc(image->width * image->height * dpixlen);
		      ximageinfo->ximage->data = (char *)data;
		      bzero(data, image->width * image->height * dpixlen);
		      ximageinfo->ximage->bitmap_bit_order = MSBFirst;
		      ximageinfo->ximage->byte_order = MSBFirst;
		      linelen = (image->width + 7) / 8;
		      for (a = 0; a < dbits; a++) {
			  pixmask = 1 << a;
			  destdata = data + ((dbits - a - 1) * image->height * linelen);
			  srcptr = image->data;
			  for (y = 0; y < image->height; y++) {
			      destptr = destdata + (y * linelen);
			      *destptr = 0;
			      mask = 0x80;
			      for (x = 0; x < image->width; x++) {
				  pixval= memToVal(srcptr, image->pixlen);
				  srcptr += image->pixlen;
				  if (self->index[pixval] & pixmask)
				      *destptr |= mask;
				  mask >>= 1;
				  if (mask == 0) {
				      mask = 0x80;
				      destptr++;
				  }
			      }
			  }
		      }
		  }
		  else {
		      byte *data, *srcptr, *destptr;

		      ximageinfo->ximage = XCreateImage(disp, visual, ddepth, ZPixmap, 0, NULL, image->width, image->height, 8, 0);
		      dpixlen = (ximageinfo->ximage->bits_per_pixel + 7) / 8;
		      data = (byte*) malloc(image->width * image->height * dpixlen);
		      ximageinfo->ximage->data = (char *)data;
		      ximageinfo->ximage->byte_order = MSBFirst;
		      srcptr = image->data;
		      destptr = data;
		      for (y = 0; y < image->height; y++)
			  for (x = 0; x < image->width; x++) {
			      valToMem(self->index[memToVal(srcptr, image->pixlen)], destptr, dpixlen);
			      srcptr += image->pixlen;
			      destptr += dpixlen;
			  }
		  }
		  break;
	  }
  }
  if (redvalue) {
    free((byte *)redvalue);
    free((byte *)greenvalue);
    free((byte *)bluevalue);
  }
  return(ximageinfo);
}

static void
InitializeColorCube( self )
    register struct xgraphic *self;
{
    register int i, ri, gi, bi;
    unsigned short red, redStart = 0;
    unsigned short green, greenStart = 0;
    unsigned short blue, blueStart = 0;
    int cubesize, samples, step, nc;
    static int cubics[] = {512, 343, 216, 125, 64, 27, 8, -1};
    boolean goodCube = FALSE;
    struct xcolormap **xcmap = (struct xcolormap**) xgraphic_CurrentColormap(self);
    Display *disp;
    struct xcolor *xc;
    int status;

    disp = xgraphic_XDisplay(self);
    status = XGetStandardColormap(disp, RootWindow(disp, DefaultScreen(disp)), &map_info, XA_RGB_DEFAULT_MAP);
    if( !status )
	status = XGetStandardColormap(disp, RootWindow(disp, DefaultScreen(disp)), &map_info, XA_RGB_BEST_MAP);
    if(!status) {
	colorCube = (struct color_cube*) calloc(1, sizeof(struct color_cube));
	colorCube->depth = DefaultDepth(disp, DefaultScreen(disp));
	colorCube->availableColors = image_depthToColors(colorCube->depth);
	nc = colorCube->availableColors = image_depthToColors(colorCube->depth);
	if((cubesize = environ_GetProfileInt("colorcubesize", -1)) != -1)
	    for(i = 0; cubics[i] != -1; i++)
		if(cubesize == cubics[i]) {
		    goodCube = TRUE;
		    break;
		}

	if(!goodCube) {
	    int half_nc = nc/2;
	    for(i = 0; cubics[i] != -1; i++)
		if(half_nc >= cubics[i]) {
		    cubesize = cubics[i];
		    break;
		}
	}

	switch(cubesize) {
	    case 8:	    
		samples = 2; 
		step = redStart = greenStart = blueStart = colorCube->componentStep = 255/(samples+1);
		break;
	    case 27:    
		samples = 3; 
		step = colorCube->componentStep = 255/(samples+1);
		redStart = greenStart = blueStart = step;
		break;
	    case 64:    
		samples = 4; 
		step = colorCube->componentStep = 255/(samples-1);
		redStart = greenStart = blueStart = 0;
		break;
	    case 125:
		samples = 5;
		step = colorCube->componentStep = 255/(samples-1);
		redStart = greenStart = blueStart = 0;
		break;
	    case 216:
		samples = 6;
		step = colorCube->componentStep = 255/(samples-1);
		redStart = greenStart = blueStart = 0;
		break;
	    default:
		break;
	}
	colorCube->samplesPerComponent = samples;
	for(ri = 0, red = redStart; ri < samples; ri++, red += step)
	    for(gi = 0, green = greenStart; gi < samples; gi++, green += step)
		for(bi = 0, blue = blueStart; bi < samples; bi++, blue += step)
		    if(xc = xcolormap_AllocColor(*xcmap, NULL,
						 red << 8, green << 8, blue << 8,
						 TRUE)) {
			colorCube->cube[ri][gi][bi] = &xc->color;
		    }
    }
    else {
	if(map_info.colormap) {
	    haveMapInfo = TRUE;
	    if(map_info.colormap != DefaultColormap(disp, DefaultScreen(disp))) {
		XSetWindowAttributes attrib;
		unsigned long attribmask;

		attrib.colormap = map_info.colormap;
		attribmask = CWColormap;
		XChangeWindowAttributes(disp, xgraphic_XWindow(self), attribmask, &attrib);
	    }
	}
    }
}

static void
SetUpXImage( self, image )
    register struct xgraphic *self;
    register struct image *image;
{ static int private_cmap = FALSE;
  static int fit = FALSE;
  Display *dpy = xgraphic_XDisplay(self);
  int scrn = DefaultScreen(dpy);
  struct xcolormap **cmap = (struct xcolormap**) xgraphic_CurrentColormap(self);

  private_cmap = (DefaultColormap(dpy,scrn) != (*cmap)->XColorMap);
  fit = imagePrefs.alwaysFitFixed;
  VerifyUpdateClipping(self);
  if(!(xgraphic_DisplayClass(self) & graphic_Monochrome) &&
     !colorCube && !haveMapInfo)
      InitializeColorCube(self);

  if (xgraphic_XDisplay(self) != PixDisplay) {
      if (self->ximageinfo && self->ximageinfo->ximage) {
	  XDestroyImage(self->ximageinfo->ximage);
	  free(self->ximageinfo);
	  self->ximageinfo = NULL;
      }
      PixDisplay = (Display *) xgraphic_XDisplay(self);
  }
  if(!image->inited || !self->ximageinfo) {
      if(self->ximageinfo = imageToXImage(self, image, private_cmap, fit)) {
	  image->inited = TRUE;
  }
      }
}

/* Given an XImage and a drawable, move a rectangle from the Ximage
 * to the drawable.
 */

void 
sendXImage( self, ximageinfo, src_x, src_y, dst_x, dst_y, w, h )
    struct xgraphic *self;
    XImageInfo  *ximageinfo;
    int          src_x, src_y, dst_x, dst_y;
    unsigned int w, h;
{
  ximageinfo->gc = xgraphic_XGC(self);
  ximageinfo->drawable = xgraphic_XWindow(self);
  XPutImage(ximageinfo->disp, ximageinfo->drawable, ximageinfo->gc,
	    ximageinfo->ximage, src_x, src_y, dst_x, dst_y, w, h);
}

void 
xgraphic__WriteImage(self, DestX, DestY, image, SrcX, SrcY, width, height)
    register struct xgraphic *self;
    long DestX, DestY, SrcX, SrcY, width, height;
    struct image *image;
{
    if ((width>0) && (height>0)) {
	VerifyUpdateClipping(self);

	SetUpXImage(self, image);
	if(image->inited && self->ximageinfo)
	    sendXImage(self, self->ximageinfo, SrcX, SrcY, physical_LogicalXToGlobalX(self, DestX), physical_LogicalYToGlobalY(self, DestY), width, height);
    }
}

void xgraphic_LocalSetClippingRect(self,updateBlk)
struct xgraphic * self; 
struct xgraphic_UpdateBlock * updateBlk; {
    struct rectangle Temp;
    XRectangle XRect[1];

    /* If the visualBounds rect is empty convert it to an empty rectangle X can grok. */
    if(self->header.graphic.visualBounds.width<0 || self->header.graphic.visualBounds.height<0) self->header.graphic.visualBounds.width=self->header.graphic.visualBounds.height=0;
    
/* The clipping rectangle for a graphic is the intersection of the visual bounds of the graphic, the updtae clipping rectangle, and any clipping rectangle provided by the client of the graphic */

    
    if (regionDebug) {
	printf("LocalSetClippingRect: entering with updateBlk %X\n",updateBlk);
    }

    /* First check to see if we have any kind of update region to contend with */

    if (!updateBlk) updateBlk = xgraphic_FindUpdateBlock(xgraphic_XDisplay(self), xgraphic_XWindow(self));

    /* Calculate the limiting rectangle based on visual bounds and any set rectangle (and turn them into physical coorindates) */

    if (self->header.graphic.visualRegion == NULL &&
	 self->header.graphic.clippingRegion == NULL &&
	 ! updateBlk->updateRegionInUse) {
	xgraphic_GetVisualBounds(self,&Temp);
	physical_LogicalToGlobalRect(self,&Temp);
	XRect[0].x = rectangle_Left(&Temp);
	XRect[0].y = rectangle_Top(&Temp);
	XRect[0].width = rectangle_Width(&Temp);
	XRect[0].height = rectangle_Height(&Temp);

	if (regionDebug) printf("localsetclip: no update rgn, ignoring it\n");
	XSetClipRectangles(xgraphic_XDisplay(self),
			   xgraphic_XGC(self),
			   /* Clip origin */0,0,XRect,/* Num Rects */1,
			   YXBanded);
	XSetClipRectangles(xgraphic_XDisplay(self),
			   xgraphic_XFillGC(self),
			   /* Clip origin */0,0,XRect,/* Num Rects */1,
			   YXBanded);
    }
    else {
	struct region *clipRegion;

	if (self->header.graphic.visualRegion == NULL) {
	    clipRegion = region_CreateRectRegion(&self->header.graphic.visualBounds);
	}
	else {
	    clipRegion = region_New();

	    region_CopyRegion(clipRegion, self->header.graphic.visualRegion);
	}

	/* If we have a clipping region, factor it in */

	if (self->header.graphic.clippingRegion != NULL)  {
	    region_IntersectRegion(clipRegion, self->header.graphic.clippingRegion, clipRegion);
	}

	/* map it to physical space (X coordinates) */

	region_OffsetRegion(clipRegion, physical_LogicalXToGlobalX(self, 0),
			    physical_LogicalYToGlobalY(self, 0));

	if (regionDebug) printf("LocalSetClip: finished with clip and visual: x %d, y %d, width %d, height %d\n", rectangle_Left(&Temp), rectangle_Top(&Temp), rectangle_Width(&Temp), rectangle_Bottom(&Temp));


	if (regionDebug) printf("LocalSetClip: Using block %X\n", updateBlk);

	if (regionDebug) printf("localsetclip: region counter in update block %d, region %X\n", updateBlk->RegionCounter, updateBlk->updateRegionInUse);

	if (updateBlk->updateRegionInUse) {
	    /* Intersect it with the update region */
	    XIntersectRegion(region_GetRegionData(clipRegion),
			     updateBlk->updateRegionInUse,
			     region_GetRegionData(clipRegion));
	    /* Set clipping rectangles */

	    if (regionDebug){
		XRectangle fakeRect;

		printf("localsetclip: setting clip rect to intersected form\n");
		printf("xim:about to test for region equality\n");
		fakeRect.x = fakeRect.y = 0;
		fakeRect.width = fakeRect.height = 32000;
		XClipBox(region_GetRegionData(clipRegion),&fakeRect);
		printf("xgraphic: clipbox of final clipping region is %d, %d, %d, %d\n", fakeRect.x, fakeRect.y, fakeRect.width, fakeRect.height);
	    }
	}

#ifdef XRELEASE2_ENV
	TempXSetRegion(xgraphic_XDisplay(self), xgraphic_XGC(self), region_GetRegionData(clipRegion));
	TempXSetRegion(xgraphic_XDisplay(self), xgraphic_XFillGC(self), region_GetRegionData(clipRegion));
#else /* XRELEASE2_ENV */
	XSetRegion(xgraphic_XDisplay(self), xgraphic_XGC(self), region_GetRegionData(clipRegion));
	XSetRegion(xgraphic_XDisplay(self), xgraphic_XFillGC(self), region_GetRegionData(clipRegion));
#endif /* XRELEASE2_ENV */
	/* and toss temporary region */
        region_Destroy(clipRegion);
    }

    self->lastUpdateRegionIDUsed = curUpdateRegionID;
}

void xgraphic__SetClippingRegion(self, region)
struct xgraphic *self;
struct region *region;
{
    /* Machine independent stuff */
    super_SetClippingRegion(self,region);
    /* Machine dependent actions */
    xgraphic_LocalSetClippingRect(self,NULL);
}

void xgraphic__SetClippingRect(self, AdditionalRect)
struct xgraphic * self;
struct rectangle * AdditionalRect;{
    /* Machine independent stuff */
    super_SetClippingRect(self,AdditionalRect);
    /* Machine dependent actions */
    xgraphic_LocalSetClippingRect(self,NULL);
}

void xgraphic__ClearClippingRect(self)
struct xgraphic * self;
{
    /* Machine independent part */
    super_ClearClippingRect(self);
    /* Machine dependent */
    xgraphic_LocalSetClippingRect(self,NULL);
}

void xgraphic__SetLineWidth(self,NewLineWidth)
struct xgraphic * self;
short NewLineWidth;
{   XGCValues tempGC;

    if ( xgraphic_GetLineWidth( self ) != NewLineWidth )
    {
      super_SetLineWidth( self, NewLineWidth );
      self->header.graphic.lineWidth = NewLineWidth;
      tempGC.line_width = NewLineWidth;

      /* special case 1 to use the underlying hardware (width 0); the performance hit is just too great for a real width of 1 */
      if (NewLineWidth == 1) tempGC.line_width = 0;

      XChangeGC(xgraphic_XDisplay(self),
		xgraphic_XGC(self),
		GCLineWidth,&tempGC);
    }
}

void xgraphic__SetLineDash( self, dashPattern, dashOffset, dashType )
struct xgraphic *self;
char		*dashPattern;
int		dashOffset;
short		dashType;
{
    XGCValues tempGC;
    register int	n = 0;
    register char	*p;
    register short	type = dashType;
    char		*oldPattern = NULL;
    int			oldOffset;
    short		oldType;

    if ( dashPattern == NULL ) type = graphic_LineSolid;
    xgraphic_GetLineDash( self, &oldPattern, &oldOffset, &oldType );
    if ( oldPattern && dashPattern && ( strcmp( oldPattern, dashPattern ) == 0 ) && dashOffset == oldOffset && type == oldType );
    else
    {
      super_SetLineDash( self, dashPattern, dashOffset, type );
      switch( type )
      {
	case graphic_LineOnOffDash: tempGC.line_style = LineOnOffDash; break;
	case graphic_LineDoubleDash: tempGC.line_style = LineDoubleDash; break;
	case graphic_LineSolid:
        default: tempGC.line_style = LineSolid; break;
      }
      XChangeGC( xgraphic_XDisplay(self), xgraphic_XGC(self), GCLineStyle, &tempGC );
      if ( dashPattern )
      {
        p = dashPattern;
        while ( *p++ != 0 ) n++;
        if ( n ) XSetDashes( xgraphic_XDisplay( self ), xgraphic_XGC( self ), dashOffset, dashPattern, n );
      }
    }
}

void xgraphic__SetLineCap( self, newLineCap )
struct xgraphic *self;
short		newLineCap;
{
    XGCValues tempGC;

    if ( xgraphic_GetLineCap( self ) != newLineCap )
    {
      super_SetLineCap( self, newLineCap );
      switch( newLineCap )
      {
	case graphic_CapNotLast: tempGC.cap_style = CapNotLast; break;
	case graphic_CapRound: tempGC.cap_style = CapRound; break;
	case graphic_CapProjecting: tempGC.cap_style = CapProjecting; break;
	case graphic_CapButt:
        default: tempGC.cap_style = CapButt; break;
      }
      XChangeGC( xgraphic_XDisplay(self), xgraphic_XGC(self), GCCapStyle, &tempGC );
    }
}

void xgraphic__SetLineJoin( self, newLineJoin )
struct xgraphic *self;
short		newLineJoin;
{
    XGCValues tempGC;

    if ( xgraphic_GetLineJoin( self ) != newLineJoin )
    {
      super_SetLineJoin( self, newLineJoin );
      switch( newLineJoin )
      {
          case graphic_JoinRound: tempGC.join_style = JoinRound; break;
          case graphic_JoinBevel: tempGC.join_style = JoinBevel; break;
	  case graphic_JoinMiter:
	  default: tempGC.join_style = JoinMiter; break;
      }
      XChangeGC( xgraphic_XDisplay(self), xgraphic_XGC(self), GCJoinStyle, &tempGC );
    }
}

void xgraphic__SetTransferMode(self,NewTransferMode)
struct xgraphic * self;
short NewTransferMode;{

    short prevValue = self->header.graphic.transferMode;
    self->header.graphic.transferMode = 0xFF & NewTransferMode;
    xgraphic_LocalSetTransferFunction(self, prevValue);
}

static void xgraphicClearGrayLevels(self)
     struct xgraphic *self;
{
  register int i;

  /* clear the gray_scale to gray_level mapping table maintained for monochrome displays */
  for (i = 0;  i <= 16;  i++)
    self->gray_levels[i] = NULL;
}

static void HandleInsertion(self, EnclosingGraphic)
struct xgraphic *self;
struct xgraphic *EnclosingGraphic;
{
    XGCValues tempGCValues;
    long tmpx, tmpy;

    /* First see if we are moving between windows or displays */
    /* We actually may be too aggressive here, but it is probably safe
       for now, e.g., movement within the same display/screen pair
       may not need a recreation of stuff, but there is no
       guarantee in X that one wil have the same window attributes,
       since an offscreen bitmap may have a differentdepth even if
       created on the same display/screen pair. */
    if (
        (self->localWindow != 
	    xgraphic_XWindow(EnclosingGraphic)) ||
	(self->displayUsed !=
	    xgraphic_XDisplay(EnclosingGraphic)) ||
	(self->screenUsed !=
	 xgraphic_XScreen(EnclosingGraphic) ) ) {

	/* Yep, we changed, everything you know is wrong! */
	if (xgraphic_XGC(self)) {
	    XFreeGC(xgraphic_XDisplay(self), xgraphic_XGC(self));
	    XFreeGC(xgraphic_XDisplay(self), xgraphic_XFillGC(self));
	    xgraphic_XGC(self) = NULL;
	    xgraphic_XFillGC(self) = NULL;
	}
	self->DisplayClass = 0;

	/* Copy over the machine dependent window capability */
	self->localWindow = xgraphic_XWindow(EnclosingGraphic);
	self->displayUsed = xgraphic_XDisplay(EnclosingGraphic);
	self->screenUsed =  xgraphic_XScreen(EnclosingGraphic);
    }
    self->valid = xgraphic_Valid(EnclosingGraphic);

    /* And pick up the appropriate gray shades for this display */
    xgraphicClearGrayLevels(self);
    self->gray_shades = xgraphic_FindGrayBlock(xgraphic_XDisplay(self),
	    xgraphic_XScreen(self));

    if (xgraphic_Valid(self))  {
	if (!xgraphic_XGC(self)) {

	    tempGCValues.line_width=self->header.graphic.lineWidth;
	    /* special case hardware (width 0) for line width of 1 */
	    if (tempGCValues.line_width == 1 ) tempGCValues.line_width = 0;
	    self->foregroundpixel = BlackPixel(xgraphic_XDisplay(self), xgraphic_XScreen(self));
	    tempGCValues.foreground = self->foregroundpixel;
	    self->backgroundpixel = WhitePixel(xgraphic_XDisplay(self), xgraphic_XScreen(self));
	    tempGCValues.background = self->backgroundpixel;
#ifdef PLANEMASK_ENV
	    tempGCValues.plane_mask = tempGCValues.foreground ^ tempGCValues.background;

	    xgraphic_XGC(self) = XCreateGC(xgraphic_XDisplay(self),
					   xgraphic_XWindow(self),  GCLineWidth | GCForeground | GCBackground | GCPlaneMask, &tempGCValues);
#else /* PLANEMASK_ENV */
	    xgraphic_XGC(self) = XCreateGC(xgraphic_XDisplay(self),
					   xgraphic_XWindow(self),  GCLineWidth | GCForeground | GCBackground, &tempGCValues);
#endif /* PLANEMASK_ENV */

	    tempGCValues.fill_style = FillSolid;
	    self->lastFillStyle = FillSolid;
	    self->lastFillPixel = self->foregroundpixel;
	    self->lastFillTile = NULL;

	    xgraphic_GetPatternOrigin(self, &tmpx, &tmpy);
	    tempGCValues.ts_x_origin = tmpx;
	    tempGCValues.ts_y_origin = tmpy;

#ifdef PLANEMASK_ENV
	    xgraphic_XFillGC(self) =
	      XCreateGC(xgraphic_XDisplay(self),
			xgraphic_XWindow(self),GCFillStyle | GCForeground | GCBackground | GCPlaneMask | GCTileStipXOrigin | GCTileStipYOrigin, &tempGCValues);
#else /* PLANEMASK_ENV */
	    xgraphic_XFillGC(self) =
	      XCreateGC(xgraphic_XDisplay(self),
			xgraphic_XWindow(self),GCFillStyle | GCForeground | GCBackground | GCTileStipXOrigin | GCTileStipYOrigin, &tempGCValues);

#endif /* PLANEMASK_ENV */

	    xgraphic_LocalSetTransferFunction(self, graphic_COPY);
	    ReallySetFont(self);

	}
	xgraphic_LocalSetClippingRect(self,NULL);
    }
}


void xgraphic__InsertGraphicSize(self, EnclosingGraphic, xOriginInParent,
	yOriginInParent, width, height)
struct xgraphic * self;
struct graphic * EnclosingGraphic;
long xOriginInParent, yOriginInParent, width, height; {
    struct rectangle r;

    rectangle_SetRectSize(&r,xOriginInParent, yOriginInParent,
	    width,height);
    xgraphic_InsertGraphic(self,EnclosingGraphic,&r);
}

void xgraphic__InsertGraphic(self, EnclosingGraphic, EnclosedRectangle)
struct xgraphic * self;
struct xgraphic * EnclosingGraphic;
struct rectangle * EnclosedRectangle;
{
    /* First do the machine independent stuff */

    super_InsertGraphic(self,EnclosingGraphic, EnclosedRectangle);

    HandleInsertion(self, EnclosingGraphic);
}

void xgraphic__InsertGraphicRegion(self, EnclosingGraphic, region)
struct xgraphic * self;
struct xgraphic * EnclosingGraphic;
struct region * region;
{
    super_InsertGraphicRegion(self,EnclosingGraphic, region);

    HandleInsertion(self, EnclosingGraphic);
}

void xgraphic__SetVisualRegion(self, region)
struct xgraphic *self;
struct region *region;
{
    super_SetVisualRegion(self, region);

    xgraphic_LocalSetClippingRect(self,NULL);
}
    
void xgraphic__FlushGraphics(self)
struct xgraphic *self; {
    XFlush(xgraphic_XDisplay(self));
}

void xgraphic__SetPatternOrigin(self, xpos, ypos)
struct xgraphic * self;
long xpos, ypos;
{
    super_SetPatternOrigin(self, xpos, ypos);
    XSetTSOrigin(xgraphic_XDisplay(self), xgraphic_XFillGC(self), xpos, ypos);
}

struct graphic * xgraphic__WhitePattern(self)
struct xgraphic *self;
{
    if (self->gray_levels[0] != NULL)
      return (struct graphic *) self->gray_levels[0];
    else
    return (struct graphic *) xgraphic_GrayPattern(self,0,16);
}

struct graphic * xgraphic__BlackPattern(self)
struct xgraphic *self;
{
    if (self->gray_levels[16] != NULL)
      return (struct graphic *) self->gray_levels[16];
    else
    return (struct graphic *) xgraphic_GrayPattern(self,16,16);
}

static struct fontdesc *  xgraphic_shadeFont = NULL;
#define GETXFONT(self, graphic)  \
	((XFontStruct *)xfontdesc_GetRealFontDesc(self, graphic))


static void CacheShades(self)
struct xgraphic *self;
{
    struct xgraphic * RetValue;
    long width, height;
    long x, y;
    XFontStruct *info;
    XCharStruct *ci;
    Pixmap newPixmap;
    GC gce=None, gcd=None;
    char str[1];
    XGCValues gcattr;
    Display *dpy=XOpenDisplay(DisplayString(xgraphic_XDisplay(self)));
    Window root;
    char buf[1024], *pb=buf;
    long SpecialChar;
    Atom atk_shades;
    
    if(dpy==NULL) return;
    
    root=RootWindow(dpy, DefaultScreen(dpy));
    atk_shades=XInternAtom(dpy, "ATK_SHADES", FALSE);

    XSetCloseDownMode(dpy, RetainPermanent);
    
     /* The following is bogus: it should be a in-core graphic
      and created the same way -- insert it into a "universal pixmap"
      such as the root of the window and use ordinary graphic operations
      to fill in the values instead of going through the X versions.
      This should allow 1) transportability to other systems and
2) consistent semantics with other xgraphic objects */
   
    if (!xgraphic_shadeFont)
	xgraphic_shadeFont = fontdesc_Create("xshape",fontdesc_Plain,10);

    info = XLoadQueryFont(dpy, "xshape10"); /* leaked but this only happens once, so I chose to live with it rather than deal with the fact that the return value is random on failure... -rr2b */
    if(info==NULL) {
	XCloseDisplay(dpy);
	return;
    }
    
    for(SpecialChar=0;SpecialChar<=16;SpecialChar++) {
	RetValue = xgraphic_New();
	if(RetValue == NULL) {
	    XCloseDisplay(dpy);
	    return;
	}
	
	ci = (info->per_char != NULL) ? &(info->per_char[SpecialChar - info->min_char_or_byte2]) : &(info->max_bounds);
	width = ci->width;
	height = info->max_bounds.ascent + info->max_bounds.descent;
	/* Older code - no longer seems to work properly
	 width = ci->rbearing - ci->lbearing;
	 height = ci->descent + ci->ascent;
	 */
	x = 0;
	/* Older code - no longer seems to work properly
	 x = -ci->lbearing;
	 */
	y = ci->ascent;
	/* Note: we could have an empty character, in which case, we simulate it with a 1 by 1 character. Too bad X doesn't allow 0 sized pixmaps */
	if (!width) {
	    fprintf(stderr,"xfontdesc_CvtCharToGraphic: 0 width character %d in %X\n", SpecialChar, self);
	    width++;
	}
	if (!height) {
	    fprintf(stderr,"xfontdesc_CvtCharToGraphic: 0 height character %d in %X\n", SpecialChar, self);
	    height++;
	}
	newPixmap = XCreatePixmap(dpy, root, width, height, 1);
	if(gce == None) {
	    gcattr.fill_style = FillSolid;
	    gcattr.foreground = 0;
	    gce = XCreateGC(dpy, newPixmap, GCFillStyle | GCForeground, &gcattr);
	    gcattr.foreground = 1;
	    gcattr.font = info->fid;
	    gcattr.function = GXcopy;
	    gcd = XCreateGC(dpy, newPixmap, GCFunction | GCFont | GCFillStyle | GCForeground, &gcattr);

	}
	XFillRectangle(dpy, newPixmap, gce, 0, 0, width, height);
	str[0] = SpecialChar;
	XDrawString(dpy, newPixmap, gcd, x, y, str, 1);
	RetValue->localWindow = newPixmap;
	RetValue->displayUsed = xgraphic_XDisplay(self);
	RetValue->screenUsed = xgraphic_XScreen(self);
	RetValue->localGraphicContext = NULL;
	RetValue->localFillGraphicContext = NULL;
	self->gray_shades[SpecialChar] = RetValue;
	sprintf(pb, "%d ", newPixmap);
	pb+=strlen(pb);
    }
    XChangeProperty(dpy, root, atk_shades, XA_STRING, 8, PropModeReplace, (unsigned char*)buf, strlen(buf));
    XFreeGC(dpy, gce);
    XFreeGC(dpy, gcd);
    XCloseDisplay(dpy);
}


static void GetShades(self)
struct xgraphic *self;
{
    Display *dpy=xgraphic_XDisplay(self);
    Window root=RootWindow(dpy, DefaultScreen(dpy));
    Atom atk_shades=XInternAtom(dpy, "ATK_SHADES", FALSE);
    Atom RetAtom; 
    int RetFormat;
    unsigned long RetNumItems;
    unsigned long RetBytesAfter;
    unsigned char *RetData = NULL;

    if(XGetWindowProperty(dpy,root, atk_shades, 0L, 1024L, False, XA_STRING, &RetAtom, &RetFormat, &RetNumItems, &RetBytesAfter,  &RetData)!=Success || RetAtom==None) {
	CacheShades(self);
    } else {
	int i;
	char *p=(char *)RetData;
	for(i=0;i<=16;i++) {
	    self->gray_shades[i]=NULL;
	}
	for(i=0;i<=16 && p && *p;i++) {
	    struct xgraphic *RetValue = xgraphic_New();
	    Pixmap pix = atol(p);
	    p=index(p, ' ');
	    RetValue->localWindow = pix;
	    RetValue->displayUsed = xgraphic_XDisplay(self);
	    RetValue->screenUsed = xgraphic_XScreen(self);
	    RetValue->localGraphicContext = NULL;
	    RetValue->localFillGraphicContext = NULL;
	    self->gray_shades[i] = RetValue;
	    if(p) p++;
	}
	if(i<17) {
	    fprintf(stderr, "xgraphic: warning: ATK_SHADES property too short!\n");
	}
	XFree(RetData);
    }
}
static boolean cacheshades=FALSE;

struct xgraphic * xgraphicGrayShade (self, index)
     struct xgraphic *self;
     long index;	/* between 0 and 16, inclusive */
{
  if (index < 0 || index > 16)
    return (NULL);

  if (self->gray_shades[index] == NULL)
    {
      if(!cacheshades) {
	  if (!xgraphic_shadeFont)
	      xgraphic_shadeFont = fontdesc_Create("xshape",fontdesc_Plain,10);

	  self->gray_shades[index] = (struct xgraphic *) fontdesc_CvtCharToGraphic(xgraphic_shadeFont, self, index);
      } else {
	  GetShades(self);
      }
  }
  return (self->gray_shades[index]);
}

struct graphic * xgraphic__GrayPattern(self, IntensityNum, IntensityDenom)
    struct xgraphic *self;
     short IntensityNum, IntensityDenom;
{

  short index, xgraphic_ApproximateColor();

    if (IntensityDenom !=16) 
	IntensityNum = (IntensityNum * 16) / IntensityDenom;
    if (IntensityNum < 0 ) IntensityNum = 0;
    else if (IntensityNum > 16) IntensityNum = 16;

  if (self->gray_levels[IntensityNum]) 
      return (struct graphic *) self->gray_levels[IntensityNum];

  if (xgraphic_DisplayClass(self) & graphic_StaticGray) {
      char *colorName = NULL;
      long red, blue, green;
      short FGindex, BGindex;

      xgraphic_GetForegroundColor (self, &colorName, &red, &green, &blue);
      FGindex = xgraphic_ApproximateColor (self, colorName, &red, &green, &blue);
      xgraphic_GetBackgroundColor (self, &colorName, &red, &green, &blue);
      BGindex = xgraphic_ApproximateColor (self, colorName, &red, &green, &blue);
      index = BGindex + ((FGindex - BGindex) * IntensityNum) / 16;
    }
  else {
      index = IntensityNum;
    }

  return ((struct graphic *) (self->gray_levels[IntensityNum] = xgraphicGrayShade(self, index)));
}

/* The next three routines need the "currently installed" colormap.  But, where is that coming from?  You'd think it would come from im but there is no hook to im in the graphic layer.  */
    
static void SetFGColor( self, colorName, red, green, blue )
    struct xgraphic * self;
    char *colorName;
    long red, green, blue;
{
    struct xcolor *xc=NULL;
    struct xcolormap **cmap = (struct xcolormap**) xgraphic_CurrentColormap(self);
    Pixel pixval=BlackPixel(xgraphic_XDisplay(self), xgraphic_XScreen(self));;
    
    xgraphicClearGrayLevels(self);

    if(!(xgraphic_DisplayClass(self) & graphic_StaticGray)) {
	xc = xcolormap_LookupColor(*cmap, colorName, red, green, blue, TRUE);
	if (!xc) {
	    if(!(xc = xcolormap_AllocColor(*cmap, colorName, red, green, blue, TRUE))) {
		fprintf(stderr, "Couldn't allocate color %s(%d,%d,%d)\n", colorName, red, green, blue);
	    }
	    else {
		xgraphic_AddObserver(self, xc);
		xcolor_AddObserver(xc, self);
		if(!xcolormap_IsObserver(*cmap, self)) {
		    xcolormap_AddObserver(*cmap, self);
		    xgraphic_AddObserver(self, *cmap);
		}
	    }
	}
	else { /* borrowing xcolor, if self isn't already observing the color, get on observer list and increment ref count */
	    if(xgraphic_IsObserver(self, xc) == FALSE) {
		xgraphic_AddObserver(self, xc);
		xcolor_AddObserver(xc, self);
		xcolor_Reference(xc);
	    }
	    if(!xcolormap_IsObserver(*cmap, self)) {
		xcolormap_AddObserver(*cmap, self);
		xgraphic_AddObserver(self, *cmap);
	    }
	}
    }
    
    if(xc) {
	pixval=xcolor_Pixel(xc);
	xcolor_GetRGB(xc, red, green, blue);
	super_SetForegroundColor(self, colorName, red, green, blue);
    } else {
	super_SetForegroundColor(self, NULL, 0, 0, 0);
    }
   
    self->lastFillPixel = self->foregroundpixel = pixval;

	if (self->header.graphic.transferMode == graphic_WHITE)  {
	    XSetBackground(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel);
	    XSetBackground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel);
	}
#ifndef PLANEMASK_ENV 
	else if (self->header.graphic.transferMode == graphic_XOR) {
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel ^ self->backgroundpixel);
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel ^ self->backgroundpixel);
	}
#endif /* PLANEMASK_ENV  */
	else  {
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel);
	    XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel);
	}
#ifdef PLANEMASK_ENV
	XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel ^ self->backgroundpixel);
	XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel ^ self->backgroundpixel);
#endif /* PLANEMASK_ENV */
}

    
static void SetBGColor(self, colorName, red, green, blue)
    struct xgraphic *self;
    char *colorName;
    long red, green, blue;
{
    struct xcolor *xc=NULL;
    struct xcolormap **cmap = (struct xcolormap**) xgraphic_CurrentColormap(self);
    Pixel pixval=WhitePixel(xgraphic_XDisplay(self), xgraphic_XScreen(self));
    
    xgraphicClearGrayLevels(self);

    if(!cmap || !*cmap) {
	fprintf(stderr, "Null cmap returned from CurrentColormap\n");
	fprintf(stderr, "Inherited: %x\n\n", xgraphic_GetInheritedColormap(self));
    }

    if (!(xgraphic_DisplayClass(self) & graphic_StaticGray)) {
	xc = xcolormap_LookupColor(*cmap, colorName, red, green, blue, TRUE);
	if(!xc) {
	    if(!(xc = xcolormap_AllocColor(*cmap, colorName, red, green, blue, TRUE))){
		fprintf(stderr, "Couldn't allocate color %s(%d,%d,%d)\n", colorName, red, green, blue);
	    }
	    else {
		xgraphic_AddObserver(self, xc);
		xcolor_AddObserver(xc, self);
		if(!xcolormap_IsObserver(*cmap, self)) {
		    xcolormap_AddObserver(*cmap, self);
		    xgraphic_AddObserver(self, *cmap);
		}
	    }
	}
	else { /* borrowing xcolor, if self isn't already observing the color, get on observer list and increment ref count */
	    if(xgraphic_IsObserver(self, xc) == FALSE) {
		xgraphic_AddObserver(self, xc);
		xcolor_AddObserver(xc, self);
		xcolor_Reference(xc);
	    }
	    if(!xcolormap_IsObserver(*cmap, self)) {
		xcolormap_AddObserver(*cmap, self);
		xgraphic_AddObserver(self, *cmap);
	    }
	}
    }
    
    if(xc) {
	pixval=xcolor_Pixel(xc);
	xcolor_GetRGB(xc, red, green, blue);
	super_SetBackgroundColor(self, colorName, red, green, blue);
    } else {
	super_SetBackgroundColor(self, NULL, 65535, 65535, 65535);
    }
    
    self->backgroundpixel = pixval;

    if (self->header.graphic.transferMode == graphic_WHITE)  {
	XSetForeground(xgraphic_XDisplay(self), xgraphic_XGC(self), self->backgroundpixel);
	XSetForeground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->backgroundpixel);
    }
#ifndef PLANEMASK_ENV 
    else if (self->header.graphic.transferMode == graphic_XOR) {
	/* At this point background should be 0.  And should remain so */
    }
#endif /* PLANEMASK_ENV  */
    else  {
	XSetBackground(xgraphic_XDisplay(self), xgraphic_XGC(self),	self->backgroundpixel);
	XSetBackground(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->backgroundpixel);
    }
#ifdef PLANEMASK_ENV
    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XGC(self), self->foregroundpixel ^ self->backgroundpixel);
    XSetPlaneMask(xgraphic_XDisplay(self), xgraphic_XFillGC(self), self->foregroundpixel ^ self->backgroundpixel);
    XSetWindowBackground(xgraphic_XDisplay(self), xgraphic_XWindow(self), self->backgroundpixel);
#endif /* PLANEMASK_ENV */
}

static short xgraphic_ApproximateColor( self, colorName, red, green, blue )
  struct xgraphic *self;
  char *colorName;
  long *red, *green, *blue;
{
    short		    index = 0;
  struct xcolor *xc;
  struct xcolormap **cmap = (struct xcolormap **) xgraphic_CurrentColormap(self);

  if(xc = xcolormap_LookupColor(*cmap, colorName, *red, *green, *blue, FALSE)) {
      *red = xc->color.red;
      *green = xc->color.green;
      *blue = xc->color.blue;
    }
  if (colorName && !xc) {
      if(xc = xcolormap_AllocColor(*cmap, colorName, *red, *green, *blue, FALSE)) {
	  *red = xc->color.red;
	  *green = xc->color.green;
	  *blue = xc->color.blue;
	}
	else
	    fprintf( stderr, "xgraphic: named color not in database (%s)\n", colorName );
    }

  if ( xgraphic_DisplayClass( self ) & graphic_StaticGray ) {
	if (( *red == *green ) && ( *green == *blue ))
	    index = 16 - ( short )( 16.0 * (( double )*red )/XFullBrightness + .5);
	else
	    index = 16 - (short) (16.0 * (((double) *red) * 0.3 + ((double) *green) * 0.59 + ((double) *blue) * 0.11) / XFullBrightness + .5);
    }
  else if ( xgraphic_DisplayClass( self ) & graphic_GrayScale ) {
	if (( *red == *green ) && ( *green == *blue ));
	else
	    *red = (long)(((double) *red) * 0.3 + ((double) *green) * 0.59 + ((double) *blue) * 0.11 + .5 );
    }
    return index;
}

static void SetStipple(self, index)
struct xgraphic *self;
long index;
{
    struct xgraphic	*tile;

    tile = xgraphicGrayShade (self, index);
    xgraphic_SetupFillGC( self, tile );
    if ((xgraphic_DisplayClass(self) & graphic_Monochrome) && (index == 0 || index == 16))
     {
	XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XGC(self), FillSolid );
	self->flipforstipple=(index==0);
	xgraphic_SetTransferMode(self, self->header.graphic.transferMode);
	self->lastStipple=NULL;
     }
    else {
	self->flipforstipple=FALSE;
	XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XGC(self), FillOpaqueStippled );
	self->lastStipple=tile;
	if(self->header.graphic.transferMode==graphic_XOR) XSetStipple(xgraphic_XDisplay(self), xgraphic_XGC(self), self->lastFillTile?self->lastFillTile->localWindow:None);
	else XSetStipple(xgraphic_XDisplay(self),  xgraphic_XGC(self), tile->localWindow);
    }
}

/* SetForegroundColor and SetBackgroundColor do not ever free colormap entries.
 * This is a resource leak, but correct. This has something to do with the fact
 * that somebody may still be using these colors when one of the ground colors
 * changes... -Z-
 */
void xgraphic__SetForegroundColor(self, colorName, red, green, blue )
    struct xgraphic *self;
    char *colorName;
    long red, green, blue;
{
    short		index;
    long		oldRed, oldGreen, oldBlue;

    xgraphicClearGrayLevels(self);

    if ( xgraphic_DisplayClass( self ) & graphic_Color ) {
      xgraphic_GetForegroundColor( self, NULL, &oldRed, &oldGreen, &oldBlue );
      if ( colorName || oldRed != red || oldGreen != green || oldBlue != blue )
	  SetFGColor( self, colorName, red, green, blue );
    }
    else {
	index = xgraphic_ApproximateColor( self, colorName, &red, &green, &blue );
	if ( xgraphic_DisplayClass( self ) & graphic_GrayScale )
	  SetFGColor( self, colorName, red, red, red );
	else if ( xgraphic_DisplayClass( self ) & graphic_StaticGray) {
		SetFGColor( self, colorName, red, green, blue );
	  SetStipple (self, index);
	    }
	super_SetForegroundColor( self, colorName, red, green, blue );
    }
}

void xgraphic__SetBackgroundColor(self, colorName, red, green, blue)
    struct xgraphic *self;
    char *colorName;
    long red, green, blue;
{
    long oldRed, oldGreen, oldBlue;

    xgraphicClearGrayLevels(self);

    if ( xgraphic_DisplayClass( self ) & graphic_Color ) {
      xgraphic_GetBackgroundColor( self, NULL, &oldRed, &oldGreen, &oldBlue );
      if ( colorName || oldRed != red || oldGreen != green || oldBlue != blue )
	  SetBGColor( self, colorName, red, green, blue );
    }
    else {
	long foreRed, foreGreen, foreBlue;
	char *foreName;
	long index = xgraphic_ApproximateColor( self, colorName, &red, &green, &blue );

	if ( xgraphic_DisplayClass( self ) & graphic_GrayScale)
	  SetBGColor( self, colorName, red, red, red );
	else
	    SetBGColor( self, colorName, red, green, blue );
	super_SetBackgroundColor( self, colorName, red, green, blue );
    }
}

void xgraphic__SetFGColor( self, red, green, blue )
struct xgraphic	*self;
double		red, green, blue;
{
long		Red, Green, Blue, oldRed, oldGreen, oldBlue;
short		index;

    xgraphicClearGrayLevels(self);

    Red = ( long )( red * XFullBrightness );
    Green = ( long )( green * XFullBrightness );
    Blue = ( long )( blue * XFullBrightness );
    if ( xgraphic_DisplayClass( self ) & graphic_Color ) {
      xgraphic_GetForegroundColor( self, NULL, &oldRed, &oldGreen, &oldBlue );
      if ( oldRed != Red || oldGreen != Green || oldBlue != Blue )
        SetFGColor( self, NULL,	Red, Green, Blue );
    }
    else {
	index = xgraphic_ApproximateColor( self, NULL, &Red, &Green, &Blue );
	if ( xgraphic_DisplayClass( self ) & graphic_GrayScale )
	  SetFGColor( self, NULL, Red, Red, Red );
	else if ( xgraphic_DisplayClass( self ) & graphic_StaticGray ) {
	    if (index != 0 || index != 16) {
		long backRed;

		xgraphic_GetBackgroundColor(self, NULL, &backRed, NULL, NULL);
		SetStipple(self, index);
	    }
	    else {
		SetFGColor(self, NULL, Red, Green, Blue);
		XSetFillStyle(xgraphic_XDisplay(self), xgraphic_XGC(self), FillSolid);
	    }
	}
	super_SetForegroundColor( self, NULL, Red, Green, Blue );
    }
}

void xgraphic__SetBGColor( self, red, green, blue )
struct xgraphic	*self;
double		red, green, blue;
{
long		Red, Green, Blue, oldRed, oldGreen, oldBlue;

    xgraphicClearGrayLevels(self);

    Red = ( long )( red * XFullBrightness );
    Green = ( long )( green * XFullBrightness );
    Blue = ( long )( blue * XFullBrightness );
    if ( xgraphic_DisplayClass( self ) & graphic_Color ) {
      xgraphic_GetBackgroundColor( self, NULL, &oldRed, &oldGreen, &oldBlue );
      if ( oldRed != Red || oldGreen != Green || oldBlue != Blue )
        SetBGColor( self, NULL,	Red, Green, Blue );
    }
    else {
	long foreRed, foreGreen, foreBlue;
	char *foreName = NULL;
	long index = xgraphic_ApproximateColor( self, NULL, &Red, &Green, &Blue );

	if ( xgraphic_DisplayClass( self ) & graphic_GrayScale )
	  SetBGColor( self, NULL, Red, Red, Red );
	else {
	    if (index < 8) {
		/* Set background to white */
		Red = Green = Blue = 65535;
	    }
	    else {
		/* Set background to black */
		Red = Green = Blue = 0;
	    }
	    SetBGColor( self, NULL, Red, Green, Blue );
	    xgraphic_GetForegroundColor(self, &foreName, &foreRed, &foreGreen, &foreBlue);
	    index = xgraphic_ApproximateColor(self, foreName, &foreRed, &foreGreen, &foreBlue);
	    if (index != 0 && index != 16) {
		SetStipple(self, index);
	    }
	}
	super_SetBackgroundColor( self, NULL, Red, Green, Blue );
    }
}

long xgraphic__GetHorizontalResolution(self)
struct xgraphic * self; {
    long res;
    if (!xgraphic_XDisplay(self))
	return (long)72;
    res=(long) ((25.4 * DisplayWidth(xgraphic_XDisplay(self), xgraphic_XScreen(self))) / (DisplayWidthMM(xgraphic_XDisplay(self), xgraphic_XScreen(self))));
    res=environ_GetProfileInt("XDPI", res);
    return environ_GetProfileInt("XHorizontalDPI", res);
}


long xgraphic__GetVerticalResolution(self)
struct xgraphic * self; {
    long res;
    if (!xgraphic_XDisplay(self))
	return (long)72;
    res=(long) ((25.4 * DisplayHeight(xgraphic_XDisplay(self), xgraphic_XScreen(self))) / (DisplayHeightMM(xgraphic_XDisplay(self), xgraphic_XScreen(self))));
    res=environ_GetProfileInt("XDPI", res);
    return environ_GetProfileInt("XVerticalDPI", res);
}

char * xgraphic__GetWindowManagerType(self)
struct xgraphic * self;{
    return "XV11R1";
}

struct xgraphic_DeviceBlock {
    struct xgraphic_DeviceBlock * nextDevice;
    Display * displayUsed;
    int screenUsed;
};

static struct xgraphic_DeviceBlock * deviceList = NULL;

long xgraphic__GetDevice(self)
struct xgraphic * self;{
    struct xgraphic_DeviceBlock * tmp, * curDevice;

    /* Look for the matching device block for this graphic and call it the device */
    if (!self->valid) return 0;
    for (curDevice = deviceList; curDevice; curDevice = curDevice->nextDevice)
	if (curDevice->displayUsed == self->displayUsed && curDevice->screenUsed == self->screenUsed) return (long) curDevice;
    /* Block not found, so must add one */
    tmp = (struct xgraphic_DeviceBlock *) malloc (sizeof(struct xgraphic_DeviceBlock));
    tmp->displayUsed = self->displayUsed;
    tmp->screenUsed = self->screenUsed;
    tmp->nextDevice = deviceList;
    deviceList = tmp;
    return (long) tmp;
}

/* -------------------------------------------------- */
/*   Predefined procedures */
/* -------------------------------------------------- */

boolean xgraphic__InitializeObject(classID,self)
struct classheader *classID;
struct xgraphic *self;
{
    self->flipforstipple=FALSE;
    self->lastStipple=NULL;
    /* Note: these have to be set by IM when it creates a window,
	since premature setting may cause inability to place into
	a parent on a different screen */
    self->localGraphicContext = NULL;
    self->localFillGraphicContext = NULL;
    self->lastFillStyle = FillSolid;
    self->lastFillPixel =  0; /* black */
    self->lastFillTile = NULL;
    self->localWindow = NULL;
    self->displayUsed = NULL;
#if 0
    self->colormap = NULL;
    self->inheritedColormap = NULL;
#endif
    self->screenUsed = 0;
    /* we can't really fill in any of the graphic stuff until we
    have a parent or root */
    self->gray_shades = xgraphic_FindGrayBlock(NULL,NULL);
    self->gray_levels = (struct xgraphic **) malloc(sizeof(struct graphic *) * 17);
    xgraphicClearGrayLevels(self);
    self->valid = FALSE;
    self->lastUpdateRegionIDUsed = 0;
    self->DisplayClass = 0;
    self->ximageinfo = NULL;
    self->index = NULL;
    self->numColorsInIndex = 0;
    imagePrefs.alwaysUsePrivateCmap = environ_GetProfileSwitch("alwaysuseprivatecmap", FALSE);
    imagePrefs.alwaysDither = environ_GetProfileSwitch("alwaysdither", FALSE);
    imagePrefs.alwaysFitFixed = environ_GetProfileSwitch("alwaysfitfixed", FALSE);
    return TRUE;
}

boolean xgraphic__InitializeClass(classID)
struct classheader *classID;
{
    cacheshades=environ_GetProfileSwitch("CacheShades", FALSE);
    return TRUE;
}

void xgraphic__FinalizeObject(classID, self)
struct classheader *classID;
struct xgraphic *self;
{
    struct xcolormap **xcmap = (struct xcolormap **) xgraphic_CurrentColormap(self);
    if (self->localGraphicContext != NULL) {
	XFreeGC(self->displayUsed, self->localGraphicContext);
    }
    if (self->localFillGraphicContext != NULL) {
	XFreeGC(self->displayUsed, self->localFillGraphicContext);
    }
    if (self->gray_levels) {
	free(self->gray_levels);
    }
}
	
static long RealDisplayClass( self )
struct xgraphic		    *self;
{
    XWindowAttributes atts;
    long		    class = 0;
    
    if (xgraphic_XDisplay(self) == NULL)
      return (class);
    if((!XGetWindowAttributes(xgraphic_XDisplay(self), xgraphic_XWindow(self), &atts)) || atts.visual==NULL) return graphic_Monochrome | graphic_StaticGray;

    class= graphic_Monochrome | graphic_StaticGray;
    switch (atts.visual->class) {
	  case PseudoColor: class = graphic_PseudoColor;
	      class |= graphic_Color;
	      break;
	  case GrayScale: class = graphic_GrayScale;
	      break;
	  case DirectColor: class = graphic_DirectColor;
	      class |= graphic_Color;
	      break;
	  case TrueColor: class = graphic_TrueColor;
	      class |= graphic_Color;
	      break;
	  case StaticColor: class = graphic_StaticColor;
	      class |= graphic_Color;
	      break;
	  case StaticGray:
	      class = graphic_StaticGray;
	      if ( DisplayPlanes( xgraphic_XDisplay( self ), xgraphic_XScreen( self )) == 1 ) class |= graphic_Monochrome;
	      else {
		  Screen *s = DefaultScreenOfDisplay(xgraphic_XDisplay(self));
		  if(CellsOfScreen(s) < environ_GetProfileInt("ForceMonochromeThreshold", 16)) class |= graphic_Monochrome ;
		  break;
	      }
    }
    return class;
}

long xgraphic__DisplayClass( self )
struct xgraphic		    *self;
{
    if(self->DisplayClass == 0) 
	self->DisplayClass = RealDisplayClass(self);
    return (self->DisplayClass);
}

void xgraphic__ObservedChanged(self, changed, value)
struct xgraphic *self;
struct observable *changed;
long value;
{
    super_ObservedChanged(self, changed, value);
    switch(value) {
	case observable_OBJECTCHANGED:
	    break;
	case observable_OBJECTDESTROYED:
	    /* an xcolor or xcolormap is being destroyed, make sure we won't Notify it of future changes. */
	    xgraphic_RemoveObserver(self, changed);
	    break;

    }
}
