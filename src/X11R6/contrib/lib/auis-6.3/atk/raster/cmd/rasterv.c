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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/rasterv.c,v 2.53 1993/07/07 19:40:27 rr2b Exp $";
#endif


 

/* Added X Window Dump read/write menu options  -paul@athena.mit.edu 3/7/90; Added X Bitmap read/write menu options - paul@athena.mit.edu 5/16/90 */

/* Some comments on changes checked in on 8/10/88
>> SetPixel:
>>    The pix local variable is modified (somehow) by the
>>    call to graphic_SetBitAtLoc. Line after this call
>>    changed to use the value of raster_GetPix rather then pix.
>> Changed all occurances of:
>>    pixelimage_DATACHANGED --> raster_BITSCHANGED
>>    rasterimage_DATACHANGED --> raster_BITSCHANGED
>> ScaleCommand:
>>    Keeps copy of original pixels when scaling. Thus if one scales
>>    and then back up, the code extrapolates from the original rather
>>    then the scaled down version --> eliminates massive loss of
>>    information.
>> Post Menus:
>>    Reorganized to avoid duplication of code and to include several
>>    items when InMessages.
>> rasterview_FinalizeObject:
>>    Added: If original is non-NULL then
>> 		Destroy original and set original equal to NULL
>> rasterview_SetDataObject:
>>    Added Debug statement.
>> rasterview_ObservedChanged:
>>    When BITSCHANGED or BOUNDSCHANGED
>> 	if original is non-NULL then
>> 		Destroy original and set original equal to NULL
>>    Added: rasterview_SCALECHANGED.
>> rasterview_FullUpdate:
>>    if the ViewSelection is Empty then
>> 	set both the ViewSelection and DesiredSelection to the subraster.
*/

/* rasterv.c	

	The view module for the raster dataobject

*/

#include <stdio.h>
#include <andrewos.h> /* strings.h */
#include <class.h>
#include <im.ih>
#include <graphic.ih>
#include <frame.ih>
#include <buffer.ih>

#include <view.ih>
#include <fontdesc.ih>
#include <sys/param.h> /* Defines MAXPATHLEN among other things */

#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <cursor.ih>
/* #include <bind.ih> */
#include <proctbl.ih>
#include <message.ih>
#include <complete.ih>
#include <scroll.ih>
#include <environ.ih>

#include <rect.h>
#include <txttroff.ih>

#include <rastimg.ih>
#include <raster.ih>
#include <rasterv.eh>
#include <rastoolv.ih>
#include <heximage.ih>
#include <paint.ih>
#include <oldrf.ih>
#include <xwdio.ih>
#include <xbm.ih>

#include <dispbox.h>

static void ToggleDebug();

extern boolean FindFrameHelp();
extern void CurrentDirectory();
extern void UpdateZoomedSelection();
extern void ViewHideHighlight();
extern void CorrectHighlight();
extern void ZoomToVisualBounds();
extern void DrawPanHighlight();
extern void CenterViewSelection();
extern void MakeToolsetProc(), KillToolsetProc();
extern void ToggleCoordProc();
extern void OverlayInsetProc(), RemoveInsetProc(), ResizeInsetProc(), ImprintInsetProc();

void PostMenus();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	User Interface 
 *	
 *	Routines called from keystrokes or menu

	XXX  when first created, there is no rasterimage hanging from the struct raster.
	Some menu operations will have to cause one to exist 

\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* masks for menu items */

#define	menuCopy	(1<<0)	/* Copy, usually, and even when read only */
#define	menuRead	(1<<1)	/* ReadRaster, usually, and also if no pix */
#define	menuOps 	(1<<2)	/* Replace, MirrorXX */
#define	menuCrop	(1<<3)	/* Crop, usual, when selection < subraster */
#define	menuUncrop	(1<<4)	/* Uncrop, only when cropped */
#define	menuRotate	(1<<5)	/* Rotate, only when whole rasterimage selected */
#define	menuNegative	(1<<6)	/* Negative */
#define	menuExtend	(1<<7)	/* ExtendToMat, when matte exceeds image */
#define	menuShrink	(1<<8)	/* ShrinkToButton, when RasterInitiallyShrunk */
#define menuMoveDB	(1<<9)	/* Move the Display Box */
#define menuHideDB	(1<<10)	/* Hide the Display Box */
#define menuTouchUp	(1<<11)	/* Touch Up */
#define menuSelect	(1<<12)	/* Selection Mode */
#define menuPan		(1<<13)
#define menuWrite	(1<<14)
#define menuZoomIn	(1<<15)
#define menuZoomOut	(1<<16)
#define menuScale	(1<<17)
#define menuCenter	(1<<18)
#define menuUpperLeft	(1<<19)
#define menuSelectAll	(1<<20)
#define	menuToolset	(1<<21)
#define	menuToolsetKill	(1<<22)
#define	menuInsetCreate	(1<<23)
#define	menuInsetThere	(1<<24)

/* masks for menus */

#define	menuNoPix	(menuRead | menuWrite)
#define	menuReadOnly	menuCopy
#define	menuReadWrite	(menuCopy | menuOps | menuNegative | menuRead | menuWrite)

static struct menulist *Menus;
static struct keymap *Keymap;

static void ToggleDebug()
{
    debug = ! debug;
    printf("Debugging is now %s\n", (debug) ? "On" : "Off");  fflush (stdout);
}

static void CropCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) return;

    /* Until Crop works when Zoomed. */
    if (NotFullSize(self)) return;

    self->ViewSelection = self->DesiredSelection;
    ras->subraster = self->ViewSelection;

    if (self->embedded) {
	self->Xscroll = self->Yscroll = 0;
	raster_NotifyObservers(ras, raster_BOUNDSCHANGED);
    }
    else {
	CenterViewSelection(self);
	self->needsFullUpdate = TRUE;
	rasterview_WantUpdate(self, self);
    }
}

static void UncropCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    if (ras == NULL) return;

    if (! self->embedded) {
	self->Xscroll += rectangle_Left(&self->ViewSelection);
	self->Yscroll += rectangle_Top(&self->ViewSelection); }

    rectangle_SetRectSize(&self->ViewSelection, 0, 0,
			   raster_GetWidth(ras), raster_GetHeight(ras));
    ras->subraster = self->ViewSelection;

    if (self->embedded) {
	self->Xscroll = self->Yscroll = 0;
	ras->subraster = self->ViewSelection;
	raster_NotifyObservers(ras, raster_BOUNDSCHANGED);
    }
    else {
	self->needsFullUpdate = TRUE;
	rasterview_WantUpdate(self, self);
    }
}

static void ShrinkCommand(self, rock)
struct rasterview *self;
long rock;
{
    self->Shrunken = TRUE;
    self->needsFullUpdate = TRUE;
    rasterview_WantNewSize(self, self);
    /* the new size will force FullUpdate, so we don't call WantUpdate */
    /* XXX WantNewSize does nothing if is an ApplicationLayer */
    if ( ! self->embedded)
	rasterview_WantUpdate(self, self);
}

void CenterCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct rectangle VB;
    rasterview_GetVisualBounds(self, &VB);

    if (RegionSelect(self)) {
	self->Xscroll = rectangle_Left(&self->DesiredSelection) - rectangle_Left(&self->ViewSelection) -
	  (rectangle_Width(&VB)/self->Scale - rectangle_Width(&self->DesiredSelection) - 1)/2;
	self->Yscroll = rectangle_Top(&self->DesiredSelection) - rectangle_Top(&self->ViewSelection) -
	  (rectangle_Height(&VB)/self->Scale - rectangle_Height(&self->DesiredSelection) - 1)/2;
	if (NotFullSize(self))
	    ZoomToVisualBounds(self, self->Xscroll, self->Yscroll); }
	else {
	    CenterViewSelection(self);
	    if (NotFullSize(self))
		ZoomToVisualBounds(self,
				   self->Xscroll/self->Scale,
				   self->Yscroll/self->Scale); }

    self->needsFullUpdate = TRUE;
    rasterview_WantUpdate(self, self);
}

void UpperLeftCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);

    if (! RegionSelect(self) || ras == NULL) return;

    if (FullSize(self)) {
	self->Xscroll = rectangle_Left(&self->DesiredSelection);
	self->Yscroll = rectangle_Top(&self->DesiredSelection); }
    else
	UpdateZoomedSelection(self,
			      rectangle_Left(&self->DesiredSelection),
			      rectangle_Top(&self->DesiredSelection));

    self->needsFullUpdate = TRUE;
    rasterview_WantUpdate(self, self);
}

void SelectAllCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);

    if (! RegionSelect(self) || ras == NULL) return;

    self->DesiredSelection = self->ViewSelection;
    CorrectHighlight(self);
    self->needsFullUpdate=FALSE;
    rasterview_WantUpdate(self, self);
}

void rasterview__SetScale(self, newscale)
struct rasterview *self;
long newscale;
{
    long x, y;
    struct rectangle VB;
    double ratio, adjustment;

    /* Until zoom works with cropping... */
    if (Cropped(self)) {
	return;
    }

    if ((newscale < 1) || (newscale == self->Scale)) {
	return;
    }

    rasterview_GetVisualBounds(self, &VB);

    if (self->Scale < newscale) {
	if (rectangle_Width(&VB) + TWOBORDER < newscale
	 || rectangle_Height(&VB) + TWOBORDER < newscale) {
	    message_DisplayString(self, 0, "Zoom too large to fit current screen");
	    return;
	}
    }

    if (FullSize(self)) {
	/* Create the pixelimage which will hold the expanded version of the dataobj. */
	self->Expansion = rasterimage_Create(0,0);
    }

    ratio = ((double) newscale) / ((double) self->Scale);
    self->Scale = newscale;

    self->DisplayBoxHidden = (self->Scale < 4);

    /* Force ZoomToVisualBounds to place the Display Box in the default location and to determine the zoomed selection. */
    SetLeftRect(&self->DisplayBox, -1);
    SetTopRect(&self->DisplayBox, -1);
    rectangle_EmptyRect(&self->DisplayBoxSelection);

    /* The scrolling of the zoomed in raster can be determined by considering the vector between the center of the screen and the upper left corner of the current View Selection. That vector is scaled (in this case) by a factor of two. The x component of the vector can be calculated as the sum of the current Xscroll and one half the difference of new and old visual widths. The new Xscroll can then be calculated as ratio times the x component of the vector plus one half the computed visual width difference. The new Yscroll can be calculated in a similar fashion. The (x, y) to pass to ZoomToVisualBounds is then the scroll divided by the current scale. */
    
    adjustment = (ratio - 1.0) / 2.0;

    x = ((long) ((ratio * self->Xscroll + adjustment * rectangle_Width(&VB))))
		/ self->Scale;
    y = ((long) ((ratio * self->Yscroll + adjustment * rectangle_Height(&VB))))
		/ self->Scale;

    if (FullSize(self)) {
	rasterimage_Destroy(self->Expansion);
	self->Expansion = NULL;
	self->DisplayBoxOverlapped = FALSE;
	rectangle_EmptyRect(&self->DisplayBox);
	self->Xscroll = x;
	self->Yscroll = y;
    }
    else {
	ZoomToVisualBounds(self, x, y);
    }

    PostMenus(self);
    self->needsFullUpdate = TRUE;
    rasterview_WantUpdate(self, self);

} /* rasterview__SetScale */

void ZoomInCommand(self, rock)
struct rasterview *self;
long rock;
{
    DEBUG(("Scroll: (%d,%d)\n", self->Xscroll, self->Yscroll));
    DEBUG(("Zoom In Scale: %d VS: (%d,%d,%d,%d)\n", self->Scale * 2,
	    rectangle_Left(&self->ViewSelection),
	    rectangle_Top(&self->ViewSelection),
	    rectangle_Width(&self->ViewSelection),
	    rectangle_Height(&self->ViewSelection)));

    rasterview_SetScale(self, self->Scale * 2);
}

void ReflectChangesInExpansion(self, R)
struct rasterview *self;
struct rectangle *R;
/* R is the rectangle within the full image which has been changed */
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix, *new;
    struct rectangle change;
    long x, y, w, h;

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL))  return;

    rectangle_IntersectRect(&change, R, &self->DisplayBoxSelection);
    if (IsEmptyRect(&change)) return;

    rectangle_GetRectSize(&change, &x, &y, &w, &h);
    w *= self->Scale;
    h *= self->Scale;

    new = rasterimage_Create(w, h);

    rasterimage_GetScaledSubraster(pix, &change, w, h, new);

    x = rectangle_Left(&change) - rectangle_Left(&self->DisplayBoxSelection);
    y = rectangle_Top(&change) - rectangle_Top(&self->DisplayBoxSelection);
    rectangle_SetRectSize(&change, 0, 0, w, h);

    rasterimage_BlitSubraster(self->Expansion, x*self->Scale, y*self->Scale, new, &change, pixelimage_COPY);
    rasterimage_Destroy(new);

    /* If the Display Box is Empty then do not blit anything into the image. */
    if (IsNotEmptyRect(&self->DisplayBox))
	DisplayBoxBlitOverlap(self, pix);
}

void HideDisplayBox(self)
struct rasterview *self;
{
    if (RegionSelect(self))
	ViewHideHighlight(self);
    DisplayBoxHide(self);
}

void MoveDisplayBoxCommand(self, rock)
struct rasterview *self;
long rock;
{
    if (rock == 1 && FullSize(self)) return;

    if (rock >= 0)
	message_DisplayString(self, 0,
			      "Click Left to selection new position of Display Box");

    DEBUG(("Moving Display Box\n"));
    HideDisplayBox(self);
    self->MovingDisplayBox = TRUE;
}

void FinishMovingDisplayBox(self, x, y)
struct rasterview *self;
long x, y;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct graphic *G = rasterview_GetDrawable(self);
    struct rectangle VB;

    ENTER(FinishMovingDisplayBox);

    message_DisplayString(self, 0, "");

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL))  return;
    if (! self->MovingDisplayBox) return;

    rasterview_GetVisualBounds(self, &VB);

    SetLeftRect(&self->DisplayBox, x);
    SetTopRect(&self->DisplayBox, y);
    /* Keep the Display Box within the Visual Rectangle. */
    if (rectangle_Left(&self->DisplayBox) - 4*BORDER <= 0) {
	SetLeftRect(&self->DisplayBox, 4*BORDER); }
    if (rectangle_Top(&self->DisplayBox) - 4*BORDER <= 0) {
	SetTopRect(&self->DisplayBox, 4*BORDER); }
    if (rectangle_Right(&self->DisplayBox) + 3*BORDER > rectangle_Width(&VB)) {
	long left = rectangle_Right(&VB) - 2*BORDER;
	left -= (left % self->Scale);
	SetLeftRect(&self->DisplayBox,
		    left - rectangle_Width(&self->DisplayBox) - 3*BORDER); }
    if (rectangle_Bottom(&self->DisplayBox) + 3*BORDER > rectangle_Height(&VB)) {
	long top = rectangle_Bottom(&VB) - 2*BORDER;
	top -= (top % self->Scale);
	SetTopRect(&self->DisplayBox,
		   top - rectangle_Height(&self->DisplayBox) - 3*BORDER); }

    DEBUG(("Moving Display Box to: (%d,%d,%d,%d)\n",
	    rectangle_Left(&self->DisplayBox),
	    rectangle_Top(&self->DisplayBox),
	    rectangle_Width(&self->DisplayBox),
	    rectangle_Height(&self->DisplayBox)));

    self->DisplayBoxHidden = FALSE;
    self->MovingDisplayBox = FALSE;

    DisplayBoxBlitOverlap(self, pix);
    DisplayBoxWritePixImageFull(self, G, pix);

    if (RegionSelect(self))
	CorrectHighlight(self);
    if (Pan(self)) {
	DrawPanHighlight(self, graphic_BLACK); }

    PostMenus(self);
}

void HideDisplayBoxCommand(self, rock)
struct rasterview *self;
long rock;
{
    HideDisplayBox(self);
    SetLeftRect(&self->DisplayBox, -3*rectangle_Width(&self->DisplayBox));
    if (RegionSelect(self))
	CorrectHighlight(self);
    self->DisplayBoxHidden = TRUE;
    PostMenus(self);
}

void ZoomOutCommand(self, rock)
struct rasterview *self;
long rock;
{
    DEBUG(("Zoom Out Scale: %d\n", self->Scale / 2));
    rasterview_SetScale(self, self->Scale / 2);
}

void NormalSizeCommand(self, rock)
struct rasterview *self;
long rock;
{
    if (NotFullSize(self)) {
	rasterview_SetScale(self, 1);
    }
}

void RegionSelectCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct rectangle r;
    rasterview_GetVisualBounds(self, &r);

    message_DisplayString(self, 0, "Region Select Mode");
    rasterview_RetractCursor(self, self->Cursor[self->Mode]);

    if (Pan(self))
	DrawPanHighlight(self, graphic_WHITE);

    self->Mode = RegionSelectMode;
    CorrectHighlight(self);

    rasterview_NotifyObservers(self, NULL); 

    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
    PostMenus(self);
}

void TouchUpCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rectangle r;
    rasterview_GetVisualBounds(self, &r);

    if (ras == NULL || raster_GetPix(ras) == NULL) return;

    message_DisplayString(self, 0, "Touch Up Mode");
    rasterview_RetractCursor(self, self->Cursor[self->Mode]);

    if (RegionSelect(self) || 
	 (Tool(self) && !rectangle_IsEmptyRect(&(self->CurSelection))))
	ViewHideHighlight(self);
    else if (Pan(self))
	DrawPanHighlight(self, graphic_WHITE);

    self->Mode = TouchUpMode;

    rasterview_NotifyObservers(self, NULL); 

    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
    PostMenus(self);
}

void PanCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rectangle r;
    rasterview_GetVisualBounds(self, &r);
    if (ras == NULL || raster_GetPix(ras) == NULL) return;

    message_DisplayString(self, 0, "Pan Mode");
    rasterview_RetractCursor(self, self->Cursor[self->Mode]);

    if (RegionSelect(self) || 
	 (Tool(self) && !rectangle_IsEmptyRect(&(self->CurSelection))))
	ViewHideHighlight(self);

    self->Mode = PanMode;
    DrawPanHighlight(self, graphic_BLACK);

    rasterview_NotifyObservers(self, NULL); 

    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
    PostMenus(self);
}

void ToolCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rectangle r;

    rasterview_GetVisualBounds(self, &r);
    if (ras == NULL || raster_GetPix(ras) == NULL) return;

    message_DisplayString(self, 0, "Special Tool Mode");
    rasterview_RetractCursor(self, self->Cursor[self->Mode]);

    if (RegionSelect(self) || 
	 (Tool(self) && !rectangle_IsEmptyRect(&(self->CurSelection)))) {
	ViewHideHighlight(self);
    }
    else if (Pan(self))
	DrawPanHighlight(self, graphic_WHITE);

    self->Mode = ToolMode;

    if (rastoolview_WantSelectionHighlighted(self->toolset))
	CorrectHighlight(self);

    rasterview_NotifyObservers(self, NULL); 

    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
    PostMenus(self);
}

void ModifyCommand(self, rock)
struct rasterview *self;
long rock;
{
    /* -1 = negative, 0 = white, 1 = black, 2 = Gray */
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct rectangle R;

    R = self->DesiredSelection;

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;

    if (rock == 0)
	rasterimage_PaintSubraster(pix, &R, WHITEBYTE);
    else if (rock == 1)
	rasterimage_PaintSubraster(pix, &R, BLACKBYTE);
    else if (rock == -1)
	rasterimage_InvertSubraster(pix, &R);
    else if (rock == 2) {
	char inbuf[50];
	long level;
	AskOrCancel(self, "Gray level (1..15)[8]: ", inbuf);
	/* If there is a value returned then if the value cannot be parsed then error. */
	if (*inbuf) {
	    if (sscanf(inbuf, "%d", &level) != 1)	
		DisplayAndReturn(self, "Value must be digits with no decimal point."); }
	else level = 8;
	rasterimage_GraySubraster(pix, &R, level); }
    else if (rock == 3)
	rasterimage_MirrorLRSubraster(pix, &R);
    else if (rock == 4)
	rasterimage_MirrorUDSubraster(pix, &R);
 
    if (NotFullSize(self))
	ReflectChangesInExpansion(self, &R);

    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
}

/* RotateCommand(self, rock)
	Rotates the entire raster image of self.

	Strategy is to clone the rasterimage, set the bit array pointer to null in 
	the original and resize it.  Then the Rotate operation is done by copying
	the bits from the clone (which has the original bit array) to the new
	bit array (which is attached to the original rasterimage).
	Finally, the clone and the old bit array are discarded.
*/
void RotateCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    struct rasterimage *temp;
    long w, h;
    struct rectangle R;

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL)) return;

    /* Until Rotation works while Zoomed... */
    if (NotFullSize(self)) return;

    w = rasterimage_GetWidth(pix);
    h = rasterimage_GetHeight(pix);

    temp = rasterimage_Clone(pix);
    rasterimage_SetBitsPtr(pix, NULL);
    rasterimage_Resize(pix, h, w);

    rectangle_SetRectSize(&R, 0, 0, w, h);
    pixelimage_GetRotatedSubraster((struct pixelimage *)temp, &R,
				    (struct pixelimage *)pix);
    rasterimage_Destroy(temp);
    rectangle_SetRectSize(&self->DesiredSelection, 0, 0, h, w);
    rectangle_SetRectSize(&self->ViewSelection, 0, 0, h, w);
    CenterViewSelection(self);
    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
}

void ReadRaster(self, ras, filename)
struct rasterview *self;
struct raster *ras;
char *filename;
{
    /* need to use FindFile XXX */
    long readresult = dataobject_OBJECTCREATIONFAILED;
    FILE *f;
    register c;

    f = fopen(filename, "r");
    if (f == NULL) {
	char err[MAXPATHLEN + 50];
	sprintf(err, "Could not read file %s", filename);
	message_DisplayString((struct view *)self, 50, err);
	return; }

    /* The raster_Read routine must assume the header is gone therefore we have to read it. For compatability, we do not read beyond an initial 0xF1, which raster_Read knows about */

    ungetc((c=getc(f)), f);
    if (c == 0xF1) 
	readresult = raster_Read(ras, f, 0);
    else {
	/* XXX If the file does not begin with a raster, we may as well scan until we find one */
	static char hdr[] = "\\begindata{raster,";
	char *hx = hdr;

	while ((c = getc(f)) != EOF && *hx)
	    if (c == *hx) hx++;
	    else hx = (c == *hdr) ? hdr+1 : hdr;
	if (*hx) {
	    char err[MAXPATHLEN + 50];
	    sprintf(err, "No raster found in file %s", filename);
	    message_DisplayString((struct view *)self, 50, err);
	    fclose(f);
	    return; }
	while ((c=getc(f)) != '\n' && c != EOF) 
	{}	/* skip to end of header */
	readresult = raster_Read(ras, f, 0); }
    fclose(f);
    /* XXX need to inform observers of scroll change. */

    if (readresult == dataobject_NOREADERROR) {
	self->needsFullUpdate = TRUE;
	self->Xscroll = self->Yscroll = 0;
	/* select the entire raster */
	self->ViewSelection = self->DesiredSelection = ras->subraster;
	if (! self->embedded)
	    CenterViewSelection(self);
    }
    else {
	char err[MAXPATHLEN + 50];
	sprintf(err, "Error %d while reading file %s", readresult, filename);
	message_DisplayString((struct view *)self, 50, err); }
}

static void ReadFileCommand(self, rock)
struct rasterview *self;
long rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    char filename[MAXPATHLEN];
    enum message_CompletionCode result;

    ENTER(ReadFileCommand);
    if (ras == NULL) return;

    CurrentDirectory(self, filename);

    result = (enum message_CompletionCode)
      completion_GetFilename(self, 
			      "Read Raster File: ", 	/* prompt */
			      filename,			/* initial string */
			      filename,			/* working area */
			      MAXPATHLEN,		/* size of working area */
			      FALSE,			/* want a file, not a directory */
			      TRUE);			/* must be existing file */

    if (result != message_Complete && result != message_CompleteValid) {
	message_DisplayString((struct view *)self, 0, "Cancelled.");
	return; }

    ReadRaster(self, ras, filename);
    if (! self->embedded) {
	struct buffer *buffer = NULL;

	buffer = frame_GetBuffer(frame_Enumerate(FindFrameHelp, rasterview_GetIM(self)));
	if(buffer) buffer_SetFilename(buffer, filename);
    }

    /* the Read will cause a NotifyObservers, which will call WantUpdate */
    LEAVE(ReadFileCommand);
}

void WriteFileCommand(self)
struct rasterview *self ;
{
    /* write using filename from read as default. */
    message_DisplayString((struct view *)self, 0, "Write Raster not yet implemented");
}

enum RasterIOType {
	InMacPaint, 
	OutMacPaint, 
	OutRasterFile, 
	OutPostscript,
	Inxwd,
	Outxwd,
	Inxbm,
#ifndef X11_ENV
	Outxbm
#else
	Outxbm,
	MakeWD,
	MakeAsnap
#endif
};

static char *prompts[] = {
	"Read MacPaint file: ",
	"Write MacPaint file: ",
	"Write RasterFile file: ",
	"Write Postscript file: ",
	"Read X Window Dump file: ",
	"Write X Window Dump file: ",
	"Read X Bitmap file: ",
#ifndef X11_ENV
	"Write X Bitmap file: "
#else
	"Write X Bitmap file: ",
	"Window Dump: select a window.",
	"Area Dump: sweep out an area."
#endif
};

static void RasterIOCommand(self, rock)
struct rasterview *self;
enum RasterIOType rock;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    FILE *f;
    boolean inputop;
    char filename[MAXPATHLEN];
    enum message_CompletionCode result;
    int savedMode;
    struct rectangle r;

    ENTER(RasterIOCommand);
    if (ras == NULL) return;

#ifndef X11_ENV
    inputop =  (rock == InMacPaint || rock == Inxwd || rock == Inxbm);
#else
    inputop =  (rock == InMacPaint || rock == Inxwd || rock == Inxbm || (rock == MakeWD) || (rock == MakeAsnap));
#endif
    /* XXX adjust if add more input types */

    CurrentDirectory(self, filename);

#ifdef X11_ENV
    if (rock != MakeWD && rock != MakeAsnap) {
#endif
	result = (enum message_CompletionCode)
	  completion_GetFilename(self, 
				 prompts[(int)rock], 	/* prompt */
				 filename,	 		/* initial string */
				 filename,			/* working area */
				 MAXPATHLEN,		/* size of working area */
				 FALSE,			/* want a file, not a directory */
				 inputop);			/* must be existing file */

	if (result != message_Complete && result != message_CompleteValid) {
	    message_DisplayString((struct view *)self, 0, 
				  "Cancelled.");
	    return;
	}
#ifdef X11_ENV
    }
#endif

    savedMode = self->Mode;
    rasterview_RetractCursor(self, self->Cursor[self->Mode]);
    self->Mode = WaitMode;
    rasterview_GetVisualBounds(self, &r);
    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);

#ifdef X11_ENV
    if (rock == MakeWD || rock == MakeAsnap) {
	/* Dump a window and set it up to read */
	char cmd[100];
	int res;
	strcpy(filename, "/tmp/wdXXXXXX");
	mktemp(filename);
	if (rock == MakeWD)
	    sprintf(cmd, "xwd %s -out %s", (rasterview_DisplayClass(self) & graphic_Monochrome) ? "-xy" : "", filename);
	else
	    sprintf(cmd, "asnap -atk -noshow -file %s", filename);
	res = os_system(cmd);
	if (res != 0) {
	    char err[500];
	    if (rock == MakeWD) sprintf(err, "Failed: could not find xwd.");
	    else sprintf(err, "Failed: could not find asnap.");
	    message_DisplayString((struct view *)self, 0, err);
	    rasterview_RetractCursor(self, self->Cursor[self->Mode]);
	    self->Mode = savedMode;
	    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
	    return;
 	}
    }
#endif
    
    /* need to use FindFile XXX */
    /* need to check if output file exists XXX */

    f = fopen(filename, (inputop) ? "r" : "w");
    if (f == NULL) {
	char err[MAXPATHLEN + 50];
	sprintf(err, "Could not %s file %s", (inputop) ? "read" : "write", filename);
	message_DisplayString((struct view *)self, 0, err);
	rasterview_RetractCursor(self, self->Cursor[self->Mode]);
	self->Mode = savedMode;
	rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);
	return;
    }

    switch (rock) {
	case InMacPaint:
	    if (paint_ReadImage(f, raster_GetPix(ras)) 
		!= dataobject_NOREADERROR) {
		char msg[MAXPATHLEN + 50];
		sprintf(msg, "File %s apparently not in MacPaint format", filename);
		message_DisplayString((struct view *)self, 0, msg); }
	    else {
		self->Xscroll = self->Yscroll = 0;
		self->needsFullUpdate = TRUE;
		self->ViewSelection = self->DesiredSelection = ras->subraster; 
		/* the Read will cause a NotifyObservers,
		 which will call WantUpdate */
	    }
	    break;
	case OutMacPaint:
	    paint_WriteImage(f, raster_GetPix(ras), &self->ViewSelection);
	    break;
	case OutRasterFile:
	    oldRF_WriteImage(f, raster_GetPix(ras), &self->ViewSelection);
	    break;
	case OutPostscript:
	    heximage_WritePostscript(f, raster_GetPix(ras), &self->ViewSelection, 
				     (ras->xScale + 0e0) / raster_UNITSCALE, 
				     (ras->yScale + 0e0) / raster_UNITSCALE);
	    break;
	case Inxwd:
#ifdef X11_ENV
	case MakeWD:
#endif
	    if (xwdio_ReadImage(f, raster_GetPix(ras)) 
		!= dataobject_NOREADERROR) {
		char msg[MAXPATHLEN + 50];
		sprintf(msg, "File %s apparently not in X Window Dump format", filename);
		message_DisplayString((struct view *)self, 0, msg); }
	    else {
		self->Xscroll = self->Yscroll = 0;
		self->needsFullUpdate = TRUE;
		self->ViewSelection = self->DesiredSelection = ras->subraster; 
		/* the Read will cause a NotifyObservers,
		 which will call WantUpdate */
	    }
	    break;
#ifdef X11_ENV
	case MakeAsnap:
	    ReadRaster(self, ras, filename);
	    break;
#endif
	case Outxwd:
	    xwdio_WriteImage(f, raster_GetPix(ras),
			      &self->ViewSelection);
	    break;
	case Inxbm:
	    if (xbm_ReadImage(f, raster_GetPix(ras)) 
		!= dataobject_NOREADERROR) {
		char msg[MAXPATHLEN + 50];
		sprintf(msg, "File %s apparently not in X Bitmap format", filename);
		message_DisplayString((struct view *)self, 0, msg); }
	    else {
		self->Xscroll = self->Yscroll = 0;
		self->needsFullUpdate = TRUE;
		self->ViewSelection = self->DesiredSelection = ras->subraster; 
		/* the Read will cause a NotifyObservers,
		 which will call WantUpdate */
	    }
	    break;
	case Outxbm:
	    xbm_WriteImage(f, raster_GetPix(ras),
			      &self->ViewSelection);
	    break;

    }
    fclose(f);
#ifndef X11_ENV
    if (! self->embedded && inputop) 
#else
    if (rock == MakeWD || rock == MakeAsnap) unlink(filename);
    
    if (rock != MakeWD && rock != MakeAsnap && ! self->embedded && inputop) 
#endif
        {
	struct buffer *buffer = NULL;

	buffer = frame_GetBuffer(frame_Enumerate(FindFrameHelp, rasterview_GetIM(self)));
	if(buffer) buffer_SetFilename(buffer, filename);
    }

    rasterview_RetractCursor(self, self->Cursor[self->Mode]);
    self->Mode = savedMode;
    rasterview_PostCursor(self, &r, self->Cursor[self->Mode]);

    if ( ! inputop) {
	char msg[MAXPATHLEN + 50];
	sprintf(msg, "Wrote file %s", filename);
	message_DisplayString((struct view *)self, 0, msg); }

    LEAVE(RasterIOCommand);
}

static void CopyCommand(self) 
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    FILE *copyFile;

    if (ras == NULL) return;
    copyFile = im_ToCutBuffer(rasterview_GetIM(self));
    raster_WriteSubRaster(ras, copyFile, 91, &self->DesiredSelection);
    /* the 91 is any non-zero object id */
    im_CloseToCutBuffer(rasterview_GetIM(self), copyFile);

    if (self->ShowCoords) {
	static char cb[40];
	sprintf(cb, "Copied area %d by %d at (%d,%d)", rectangle_Width(&self->DesiredSelection), rectangle_Height(&self->DesiredSelection), rectangle_Left(&self->DesiredSelection), rectangle_Top(&self->DesiredSelection));
	message_DisplayString(self, 10, cb);
    }
}

static void ReplaceCommand (self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    FILE *pasteFile;
    static char hdr[] = "\\begindata{raster,";
    char *hx = hdr;
    register c;

    pasteFile = im_FromCutBuffer(rasterview_GetIM(self));

    /* if the file does not begin with a raster, 
	we may as well scan until we find one XXX */

    while ((c = getc(pasteFile)) != EOF && *hx)
	if (c == *hx) hx++;
	else hx = (c == *hdr) ? hdr+1 : hdr;
    if (*hx) 
	message_DisplayString((struct view *)self, 0, "No raster found in cut buffer");
    else {
	while ((c=getc(pasteFile)) != '\n' && c != EOF) 
	{}	/* skip to end of header */
	if (FullySelected(self) && Uncropped(self)) {
	    raster_Read(ras, pasteFile, 1);
	    self->ViewSelection = self->DesiredSelection = ras->subraster;
	}
	else
	    raster_ReadSubRaster(ras, pasteFile, &self->DesiredSelection);
    }
    im_CloseFromCutBuffer(rasterview_GetIM(self), pasteFile);
}

boolean MatExtendPossible(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rectangle VB;

    rasterview_GetVisualBounds(self, &VB);
    InsetRect(&VB, BORDER, BORDER);
    DEBUG(("Ext-P: Scroll: (%d,%d) Raster: (%d,%d)\n",
	    self->Xscroll, self->Yscroll,
	    raster_GetWidth(ras), raster_GetHeight(ras)));
    DEBUG(("       VS: (%d,%d,%d,%d) VB: (%d,%d,%d,%d)\n",
	    rectangle_Left(&self->ViewSelection),
	    rectangle_Top(&self->ViewSelection),
	    rectangle_Width(&self->ViewSelection),
	    rectangle_Height(&self->ViewSelection),
	    rectangle_Left(&VB),
	    rectangle_Top(&VB),
	    rectangle_Width(&VB),
	    rectangle_Height(&VB)));
    return ((self->Xscroll + rectangle_Width(&VB))/self->Scale > raster_GetWidth(ras)
	     || (self->Yscroll + rectangle_Height(&VB))/self->Scale > raster_GetHeight(ras)
	     || self->Xscroll < 0
	     || self->Yscroll < 0);
}

void rasterview__ResizeRaster(self, width, height)
struct rasterview *self;
long width, height;
{
    struct raster *ras;
    struct rasterimage *pix;
    struct rasterimage *new;
    long xscr, yscr, w, h;

    ras = (struct raster *) rasterview_GetDataObject(self);
    w = raster_GetWidth(ras);
    h = raster_GetHeight(ras);

    pix = raster_GetPix(ras);
    raster_Resize(ras, width, height);

    if ((self->Xscroll < 0) || (self->Yscroll < 0)) {
	if (self->Xscroll < 0) {
	    xscr = - self->Xscroll / self->Scale;
	    self->Xscroll = 0;
	}
	else {
	    xscr = 0;
	}
	if (self->Yscroll < 0) {
	    yscr = - self->Yscroll / self->Scale;
	    self->Yscroll = 0;
	}
	else {
	    yscr = 0;
	}

	new = rasterimage_Create(w, h);

	rasterimage_BlitSubraster(new, 0, 0, pix, &self->ViewSelection, pixelimage_COPY);
	rasterimage_Clear(pix);
	rasterimage_BlitSubraster(pix, xscr, yscr, new, &self->ViewSelection, pixelimage_COPY);
	rasterimage_Destroy(new);
    }

    rectangle_SetRectSize(&self->ViewSelection, 0, 0, width, height);
    rectangle_SetRectSize(&self->DesiredSelection, 0, 0, width, height);

    if (NotFullSize(self)) {
	ZoomToVisualBounds(self,
			   self->Xscroll/self->Scale,
			   self->Yscroll/self->Scale);
    }

    rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
    rasterimage_NotifyObservers(pix, raster_BOUNDSCHANGED);
} /* rasterview__ResizeRaster */

/* ExtendToMatCommand(self)
	Extend the raster size to the size of the matte, if bigger 
*/
static void ExtendToMatCommand(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    long l, t, w, h, r, b;
    long vr, vb;
    long width, height;
    struct rectangle VB;	/* the visual bounds measure the matte size */

    if (! MatExtendPossible(self)) return;

    rasterview_GetVisualBounds(self, &VB);
    InsetRect(&VB, BORDER, BORDER);
    vr = rectangle_Width(&VB);
    vb = rectangle_Height(&VB);

    l = -self->Xscroll;
    t = -self->Yscroll;
    w = raster_GetWidth(ras)*self->Scale;
    h = raster_GetHeight(ras)*self->Scale;
    r = l + w;
    b = t + h;

    width = (w + ((vr > r) ? (vr - r) : 0) + ((l > 0) ? l : 0))/self->Scale;
    height = (h + ((vb > b) ? (vb - b) : 0) + ((t > 0) ? t : 0))/self->Scale;

    DEBUG(("Visual: %d x %d\nCurrent Sides: (%d,%d,%d,%d)\nNew Size: %d x %d\n",
	    vr, vb,
	    l/self->Scale, t/self->Scale, r/self->Scale, b/self->Scale,
	    width, height));

    rasterview_ResizeRaster(self, width, height);
}

/* SetPrintSizeCommand(self)
	set scaling factors used for printing
	prompts for print size and sets scaling accordingly
	Note that the subraster is either the entire raster or the cropped raster.
*/
static void SetPrintSizeCommand(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    long w = rectangle_Width(&self->ViewSelection);
    long h = rectangle_Height(&self->ViewSelection);

    double newdw = 0.0, newdh = 0.0;
    long newyScale = 0, newxScale = 0;
    char newxsize[75], newysize[75], request[150];

    /* ask for height */
    sprintf(request, "Print height %0.2f  width %0.2f in.   New height [scale to width] ", 
	     h / 72.0 * ras->yScale / raster_UNITSCALE,
	     w / 72.0 * ras->xScale / raster_UNITSCALE);
    AskOrCancel(self, request, newysize);
    if (*newysize) {
	/* height specified.  parse it and set width request */
	if (sscanf(newysize, "%lf", &newdh) != 1)		
	    DisplayAndReturn(self,
			     "Value must be digits with at most one decimal point.");
	newyScale = (newdh * 72.0 * raster_UNITSCALE + (h/2)) / h;
	/* Change x scaling factor to correspond to the new y scaling factor */
	newxScale = newyScale;

	sprintf(request, "Print height %0.2f in.   New width [%0.2f] ", 
		h / 72.0 * newyScale / raster_UNITSCALE,
		w / 72.0 * newxScale / raster_UNITSCALE); }
    else {
	/* no height specified; scale to width, default original width */
	sprintf(request, "Print height %0.2f  width %0.2f in.   Scale to width [original:  %0.2f] ", 
		h / 72.0 * ras->yScale / raster_UNITSCALE,
		w / 72.0 * ras->xScale / raster_UNITSCALE,
		w / 72.0 / 2.0); }

    /* request new width */
    AskOrCancel(self, request, newxsize);
    if (*newxsize && sscanf(newxsize, "%lf", &newdw) != 1)
	DisplayAndReturn(self, "Value must be digits with at most one decimal point.");
    /* now analyze results.   set unspecified new..Scale  */
    if (*newxsize) {
	newxScale = (newdw * 72.0 * raster_UNITSCALE + (w/2)) / w;
	if (*newysize == '\0') 
	    newyScale = newxScale; }
    else if (*newysize == '\0') {
	/* no change specified, revert to 1/2 screen size */
	newxScale = raster_UNITSCALE / 2;
	newyScale = raster_UNITSCALE / 2; }

    /* Store new scaling factors. */
    ras->xScale = newxScale;
    ras->yScale = newyScale;

    DEBUG(("chosen size inches: %0.4f x %0.4f   points:  %ld x %ld   (scaled 0x%lx  0x%lx)  \n", 
	    newdw, newdh,
	    (w * ras->xScale + (raster_UNITSCALE/2)) / raster_UNITSCALE,
	    (h * ras->yScale + (raster_UNITSCALE/2)) / raster_UNITSCALE, 
	    newxScale, newyScale));

    /* display the new size */
    sprintf(request, "Print size is now height %0.2f width %0.2f in. ", 
	     h / 72.0 * ras->yScale / raster_UNITSCALE,
	     w / 72.0 * ras->xScale / raster_UNITSCALE);
    message_DisplayString(self, 0, request);
}

/* This keeps the orignal pixel image around so that
  one can extrapolate from the original rather then the
  scaled version of the image. */
static void ScaleCommand(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    long x, y, w, h;
    long NewW, NewH;
    char c[10], inbuf[50], request[150];

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;

    /* Until Scaling works while Zoomed... */
    if (NotFullSize(self)) return;

    rectangle_GetRectSize(&self->DesiredSelection, &x, &y, &w, &h);

    /* ask for scaling factor as a number of pixels or relative
	to current size */
    sprintf(request, "Change scale via relative size or absolute pixel [relative]: ",
	     inbuf);
    AskOrCancel(self, request, inbuf);
    sscanf(inbuf, "%[ar]", &c[0]);
    if (strcmp(c,"a")==0) {
	/* Ask for absolute values */
	sprintf(request, "New pixel width [%d]: ", w);
	AskOrCancel(self, request, inbuf);
	/* If there is a value returned then if the value cannot be parsed then error else calculate new height and default the new height correspondingly. Else default the new height and width to 1. */
	if (*inbuf) {
	    if (sscanf(inbuf, "%d", &NewW) != 1) {		
		DisplayAndReturn(self, "Value must be digits with no decimal point."); }
	    else NewH = (h * NewW)/w; }
	else {
	    NewW = w;
	    NewH = h; }
	sprintf(request, "New pixel height [%d]: ", NewH);

	AskOrCancel(self, request, inbuf);
	/* if there is a value returned and it cannot be parsed then error. */
	if (*inbuf && sscanf(inbuf, "%d", &NewH) != 1)		
	    DisplayAndReturn(self, "Value must be digits with no decimal point.");
	if (! *inbuf) NewH = (h * NewW)/w;
    }
    else {
	float ScaleW, ScaleH;
	/* Ask for relative values. */
	AskOrCancel(self, "New relative width [1.0000]: ", inbuf);
	if (*inbuf) {
	    if (sscanf(inbuf, "%f", &ScaleW) != 1) {
		DisplayAndReturn(self,
				 "Value must be digits with at most one decimal point."); }
	    else ScaleH = ScaleW; }
	else {
	    ScaleW = 1.0;
	    ScaleH = 1.0; }
	sprintf(request, "New relative height [%0.4f]: ", ScaleH);

	AskOrCancel(self, request, inbuf);
	if (*inbuf && sscanf(inbuf, "%f", &ScaleH) != 1) {
	    DisplayAndReturn(self,
			     "Value must be digits with at most one decimal point."); }
	if (! *inbuf) ScaleH = ScaleW;
	NewW = w * ScaleW;
	NewH = h * ScaleH;
    }

    sprintf(request, "New pixel (width, height): (%d, %d)", NewW, NewH);
    message_DisplayString(self, 0, request);

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

static void ScaleReplaceCommand(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix;
    FILE *pasteFile;
    static char hdr[] = "\\begindata{raster,";
    char *hx = hdr;
    register c;

    if (ras == NULL || (pix = raster_GetPix(ras)) == NULL) return;

    /* Mostly taken from ReplaceCommand */
    pasteFile = im_FromCutBuffer(rasterview_GetIM(self));

    /* if the file does not begin with a raster, 
	we may as well scan until we find one XXX */

    while ((c = getc(pasteFile)) != EOF && *hx)
	if (c == *hx) hx++;
	else hx = (c == *hdr) ? hdr+1 : hdr;
    if (*hx) 
	message_DisplayString((struct view *)self, 0, "No raster found in cut buffer");
    else {
	register struct raster *RasFromPaste = raster_Create(0,0);
	struct rasterimage *NewPix;
	struct rectangle R;
	long x, y, w, h;

	while ((c=getc(pasteFile)) != '\n' && c != EOF) 
	{}	/* skip to end of header */

	rectangle_GetRectSize(&self->DesiredSelection, &x, &y, &w, &h);

	raster_Read(RasFromPaste, pasteFile, 1);
	NewPix = raster_GetPix(RasFromPaste);

	rasterimage_GetScaledSubraster(NewPix, &RasFromPaste->subraster,
				       w, h, NewPix);
	rectangle_SetRectSize(&R, 0, 0, w, h);
	rasterimage_BlitSubraster(pix, x, y, NewPix, &R, pixelimage_COPY);

	raster_Destroy(RasFromPaste);

	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
    }
    im_CloseFromCutBuffer(rasterview_GetIM(self), pasteFile);
}

void PostMenus(self)
struct rasterview *self;
{
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);
    struct rasterimage *pix = NULL;
    long menumask;

    if ( !self->HasInputFocus ) return;
    if (ras != NULL) pix = raster_GetPix(ras);

    /* choose 'menumask' value */
    if (pix == NULL)
	menumask = menuNoPix;
    else { 
	if (ras->readOnly) {
	    /* XXX probably should have a readonly bit in the raster data object */
	    menumask = menuReadOnly;
	    if (self->InMessages) {
		/* XXX in  messages allow Crop, Uncrop, and, if fully selected, Negative */
		if (RegionSelect(self))
		    menumask |= menuPan | menuCenter;
		else if (Pan(self))
		    menumask |= menuSelect;
		if (Uncropped(self) && FullySelected(self))
		    /* Selection and subraster are both full image, include Negative */
		    menumask |= menuNegative;
	    }
	    else {
		if (RegionSelect(self))
		    menumask |= menuPan | menuCenter;
		else if (Pan(self))
		    menumask |= menuSelect;
	    }
	}
	else {
	    menumask = menuReadWrite;

	    if (RegionSelect(self))
		menumask |= menuTouchUp | menuPan | menuCenter;
	    else if (TouchUp(self))
		menumask |= menuSelect | menuPan;
	    else if (Pan(self))
		menumask |= menuSelect | menuTouchUp;
	    else if (Tool(self))
		menumask |= menuSelect | menuTouchUp | menuPan;
	    if (FullSize(self))
		menumask |= menuScale;
	    if (Uncropped(self) && FullySelected(self)) {
		/* Selection and subraster are both full image, include Rotate */
		/* Until this work in Zoomed mode: */
		if (FullSize(self)) {
		    menumask |= menuRotate; }
		if (! self->Shrunken && MatExtendPossible(self))
		    menumask |= menuExtend;
	    }

	    if (!self->inset)
		menumask |= menuInsetCreate;
	    else
		menumask |= menuInsetThere;
	}

	if (RegionSelect(self)) {
	    if (rectangle_Left(&self->DesiredSelection) != self->Xscroll/self->Scale
		|| rectangle_Top(&self->DesiredSelection) != self->Yscroll/self->Scale
		|| rectangle_IsEmptyRect(&self->DesiredSelection))
		menumask |= menuUpperLeft;
	    if (! rectangle_IsEqualRect(&self->ViewSelection, &self->DesiredSelection))
		menumask |= menuSelectAll; }

	if (Cropped(self)) {
	    /* Until this work in Zoomed mode: */
	    if (FullSize(self)) {
		menumask |= menuUncrop; 
		if(! FullySelected(self))
		    menumask |= menuCrop;
	    }
	}
	else {
	    /* Until Zooming while Cropped works. */
	    menumask |= menuZoomIn;
	    if (! FullySelected(self)) {
		/* Until this work in Zoomed mode: */
		if (FullSize(self))
		    menumask |= menuCrop; 
	    }

	    if (NotFullSize(self)) {
		menumask |= menuZoomOut;
		menumask |= menuMoveDB;
		if (! self->DisplayBoxHidden)
		    menumask |= menuHideDB; 
	    }
	}

	if (self->toolset)
	    menumask |= menuToolsetKill;
	else
	    menumask |= menuToolset;

    } /* end else [there is a pix] */

    if ( ! self->Shrunken && RastersInitiallyShrunk && self->embedded) 
	menumask |= menuShrink;

    DEBUG(("MenuMask: 0x%lx\n", menumask));

    if (menulist_SetMask(self->Menus, menumask))
	rasterview_PostMenus(self, NULL);
}

void rasterview__PostMenus(self, ml)
struct rasterview *self;
struct menulist *ml;
{
    /* Enable the menus for this object. */
    menulist_ClearChain(self->Menus);
    if (ml) menulist_ChainBeforeML(self->Menus, ml, ml);
    super_PostMenus(self, self->Menus);
}


/*  -----------  Added 4/6/89 -------------- */
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	
 *	Override methods
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

boolean rasterview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    struct proctable_Entry *proc = NULL;
    DEBUG(("ENTER rasterview__InitializeClass\n"));
    Menus = menulist_New();
    Keymap = keymap_New();

    proc = proctable_DefineProc("rasterv-toolset-create", MakeToolsetProc, &rasterview_classinfo, NULL, "Creates a window containing editing tools.");
    menulist_AddToML(Menus, "Raster~30,Toolset~80", proc, NULL, menuToolset); 

    proc = proctable_DefineProc("rasterv-toolset-destroy", KillToolsetProc, &rasterview_classinfo, NULL, "Deletes toolset window.");
    menulist_AddToML(Menus, "Raster~30,Remove Toolset~81", proc, NULL, menuToolsetKill);

    proc = proctable_DefineProc("rasterv-toggle-coord-display", ToggleCoordProc, &rasterview_classinfo, NULL, "Toggles display of mouse coordinates.");
    menulist_AddToML(Menus, "Raster~30,Show/Hide Coords~57", proc, NULL, 0);

    proc = proctable_DefineProc("rasterv-overlay-inset", OverlayInsetProc, &rasterview_classinfo, NULL, "Overlay an inset on the raster view.");
    menulist_AddToML(Menus, "Raster~30,Overlay Inset~72", proc, NULL, menuInsetCreate);

    proc = proctable_DefineProc("rasterv-remove-inset", RemoveInsetProc, &rasterview_classinfo, NULL, "Remove the overlaid inset.");
    menulist_AddToML(Menus, "Raster~30,Remove Inset~76", proc, NULL, menuInsetThere);

    proc = proctable_DefineProc("rasterv-resize-inset", ResizeInsetProc, &rasterview_classinfo, NULL, "Resize the overlaid inset.");
    menulist_AddToML(Menus, "Raster~30,Resize Inset~77", proc, NULL, menuInsetThere);

    proc = proctable_DefineProc("rasterv-imprint-inset", ImprintInsetProc, &rasterview_classinfo, NULL, "Imprint the overlaid inset.");
    menulist_AddToML(Menus, "Raster~30,Paste Down Inset~75", proc, 99, menuInsetThere);

    proc = proctable_DefineProc("rasterv-copy-subraster", CopyCommand,
				 &rasterview_classinfo, NULL,
				 "Copy selected subraster to kill-buffer.");
    keymap_BindToKey(Keymap, "\033w", proc, 0);	/* ESC - w */
    menulist_AddToML(Menus, "Copy~3", proc, 0, menuCopy);

    proc = proctable_DefineProc("rasterv-replace-subraster", ReplaceCommand,
				 &rasterview_classinfo, NULL,
				 "Replace selected subraster from kill-buffer.");
    keymap_BindToKey(Keymap, "\033y", proc, 0);	/* ESC - y */
    menulist_AddToML(Menus, "Replace~13", proc, 0, menuOps);

    proc = proctable_DefineProc("rasterv-replace-subraster", ReplaceCommand,
				 &rasterview_classinfo, NULL,
				 "Replace selected subraster from kill-buffer.");
    keymap_BindToKey(Keymap, "\031", proc, 0);	/* ^Y */
    menulist_AddToML(Menus, "Replace~13", proc, 0, menuOps);

    proc = proctable_DefineProc("rasterv-scale-replace", ScaleReplaceCommand,
				 &rasterview_classinfo, NULL,
				 "Scale image in paste buffer to fit in selected Region");
    keymap_BindToKey(Keymap, "\033\031", proc, 0);	/* ESC ^Y */
    menulist_AddToML(Menus, "Replace Scaled~15", proc, 0, menuOps);

    proc = proctable_DefineProc("rasterv-negative", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Interchange black and white within selection");
    keymap_BindToKey(Keymap, "\033n", proc, -1);	/* ESC - n */
    menulist_AddToML(Menus, "Raster Ops~20,Negative~21", proc, -1, menuNegative);

    proc = proctable_DefineProc("rasterv-white", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Set white within selection");
    keymap_BindToKey(Keymap, "\033W", proc, 0);	/* ESC - W */
    menulist_AddToML(Menus, "Raster Ops~20,White~22", proc, 0, menuOps);

    proc = proctable_DefineProc("rasterv-black", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Set black within selection");
    keymap_BindToKey(Keymap, "\033B", proc, 1);	/* ESC - B */
    menulist_AddToML(Menus, "Raster Ops~20,Black~23", proc, 1, menuOps);

    proc = proctable_DefineProc("rasterv-gray", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Set gray within selection");
    keymap_BindToKey(Keymap, "\033G", proc, 2);	/* ESC - G */
    menulist_AddToML(Menus, "Raster Ops~20,Gray~24", proc, 2, menuOps);

    proc = proctable_DefineProc("rasterv-mirror-left-right", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Interchange left and right within selection");
    keymap_BindToKey(Keymap, "\033L", proc, 3);	/* ESC - L */
    menulist_AddToML(Menus, "Raster Ops~20,Mirror LR~25", proc, 3, menuOps);

    proc = proctable_DefineProc("rasterv-mirror-up-down", ModifyCommand,
				 &rasterview_classinfo, NULL,
				 "Interchange top and bottom within selection");
    keymap_BindToKey(Keymap, "\033U", proc, 4);	/* ESC - U */
    menulist_AddToML(Menus, "Raster Ops~20,Mirror UD~26", proc, 4, menuOps);

    proc = proctable_DefineProc("rasterv-rotate", RotateCommand,
				 &rasterview_classinfo, NULL,
				 "Rotate entire raster image one-quarter turn clockwise");
    keymap_BindToKey(Keymap, "\033R", proc, 0);	/* ESC - R */
    menulist_AddToML(Menus, "Raster Ops~20,Rotate~27", proc, 0, menuRotate);

    proc = proctable_DefineProc("rasterv-scale", ScaleCommand,
				 &rasterview_classinfo, NULL,
				 "Scale the image");
    keymap_BindToKey(Keymap, "\033S", proc, 0);	/* ESC - S */	
    menulist_AddToML(Menus, "Raster Ops~20,Scale~32", proc, 0, menuScale);

    proc = proctable_DefineProc("rasterv-shrink-image", ShrinkCommand,
				 &rasterview_classinfo, NULL,
				 "Shrink the image of the raster to a button");
    keymap_BindToKey(Keymap, "\033s", proc, 0);	/* ESC - s */
    menulist_AddToML(Menus, "Raster Ops~20,Shrink to Button~67", proc, 0, menuShrink);

    proc = proctable_DefineProc("rasterv-center-image", CenterCommand,
				 &rasterview_classinfo, NULL,
				 "Center the Selected Region");
    keymap_BindToKey(Keymap, "\033c", proc, 0);	/* ESC - c */
    menulist_AddToML(Menus, "Raster Ops~20,Center Region~72", proc, 0, menuCenter);

    proc = proctable_DefineProc("rasterv-upperleft-image", UpperLeftCommand,
				 &rasterview_classinfo, NULL,
				 "Upper Left the Selected Region");
    keymap_BindToKey(Keymap, "\033c", proc, 0);	/*  */
    menulist_AddToML(Menus, "Raster Ops~20,Upper Left Region~73", proc, 0, menuUpperLeft);

    proc = proctable_DefineProc("rasterv-zoom-in", ZoomInCommand,
				 &rasterview_classinfo, NULL,
				 "Zoom In using upper left of selected region as starting point.");
    keymap_BindToKey(Keymap, "\033Z", proc, 0);	/* ESC - Z */
    menulist_AddToML(Menus, "Raster~20,Zoom In~12", proc, 0, menuZoomIn);

    proc = proctable_DefineProc("rasterv-zoom-out", ZoomOutCommand,
				 &rasterview_classinfo, NULL,
				 "Zoom Out");
    keymap_BindToKey(Keymap, "\033z", proc, 0);	/* ESC - z */
    menulist_AddToML(Menus, "Raster~20,Zoom Out~13", proc, 0, menuZoomOut);

    proc = proctable_DefineProc("rasterv-zoom-out-to-normal-size", NormalSizeCommand,
				 &rasterview_classinfo, NULL,
				 "Normal Size");
    keymap_BindToKey(Keymap, "\033N", proc, 0);	/* ESC - N */
    menulist_AddToML(Menus, "Raster~20,Normal Size~15", proc, 0, menuZoomOut);

    proc = proctable_DefineProc("rasterv-select-entire", SelectAllCommand,
				 &rasterview_classinfo, NULL,
				 "Select entire raster");
    keymap_BindToKey(Keymap, "\033N", proc, 0);	/* */
    menulist_AddToML(Menus, "Raster~20,Select All~22", proc, 0, menuSelectAll);

    proc = proctable_DefineProc("rasterv-region-select", RegionSelectCommand,
				 &rasterview_classinfo, NULL,
				 "Region Select Mode");
    keymap_BindToKey(Keymap, "\033\030R", proc, 0);	/* ESC - ^X R */
    menulist_AddToML(Menus, "Raster~20,Region Select~22", proc, 0, menuSelect);

    proc = proctable_DefineProc("rasterv-touchup", TouchUpCommand,
				 &rasterview_classinfo, NULL,
				 "Touch Up Mode");
    keymap_BindToKey(Keymap, "\033\030T", proc, 0);	/* ESC - ^X T */
    menulist_AddToML(Menus, "Raster~20,Touch Up~23", proc, 0, menuTouchUp);

    proc = proctable_DefineProc("rasterv-pan", PanCommand,
				 &rasterview_classinfo, NULL,
				 "Pan Mode");
    keymap_BindToKey(Keymap, "\033\030P", proc, 0);	/* ESC - ^X P */
    menulist_AddToML(Menus, "Raster~20,Pan~24", proc, 0, menuPan);

    proc = proctable_DefineProc("rasterv-move-display-box", MoveDisplayBoxCommand,
				 &rasterview_classinfo, NULL,
				 "Move the location of the Display Box. ");
    keymap_BindToKey(Keymap, "\033M", proc, 1);	/* ESC - M */
    menulist_AddToML(Menus, "Raster~20,Move Display Box~32", proc, 0, menuMoveDB);

    proc = proctable_DefineProc("rasterv-hide-display-box", HideDisplayBoxCommand,
				 &rasterview_classinfo, NULL,
				 "Hide the Display Box. ");
    keymap_BindToKey(Keymap, "\033H", proc, 1);	/* ESC - H */
    menulist_AddToML(Menus, "Raster~20,Hide Display Box~34", proc, 0, menuHideDB);

    proc = proctable_DefineProc("rasterv-set-print-scaling", SetPrintSizeCommand,
				 &rasterview_classinfo, NULL,
				 "Set the scaling factors used when printing");
    keymap_BindToKey(Keymap, "\033P", proc, 0);	/* ESC - P */
    menulist_AddToML(Menus, "Raster~30,Set Print Size~53", proc, 0, menuOps);

    proc = proctable_DefineProc("rasterv-extend-to-mat", ExtendToMatCommand,
				 &rasterview_classinfo, NULL,
				 "Add pixels to image to reach the bordering matte");
    keymap_BindToKey(Keymap, "\033X", proc, 0);	/* ESC - X */
    menulist_AddToML(Menus, "Raster~30,Extend to Mat~55", proc, 0, menuExtend);

    proc = proctable_DefineProc("rasterv-crop", CropCommand,
				 &rasterview_classinfo, NULL,
				 "Crop image to the selection");
    keymap_BindToKey(Keymap, "\033C", proc, 0);	/* ESC - C */
    menulist_AddToML(Menus, "Raster~30,Crop~63", proc, 0, menuCrop);

    proc = proctable_DefineProc("rasterv-uncrop", UncropCommand,
				 &rasterview_classinfo, NULL,
				 "Revert image to entire raster");
    keymap_BindToKey(Keymap, "\033c", proc, 0);	/* ESC - c */
    menulist_AddToML(Menus, "Raster~30,Uncrop~65", proc, 0, menuUncrop);

    proc = proctable_DefineProc("rasterv-toggle-debug", ToggleDebug,
				 &rasterview_classinfo, NULL,
				 "Toggle the rasterview debug flag");
    keymap_BindToKey(Keymap, "\033D", proc, 0);	/* ESC - D */
    /* No Menu Item for Debuging. */

    proc = proctable_DefineProc("rasterv-read-file", ReadFileCommand,
				 &rasterview_classinfo, NULL,
				 "Read a raster file");
    keymap_BindToKey(Keymap, "\033\022r", proc, 0);	/* ESC - ^R - r */
    menulist_AddToML(Menus, "File,Read Raster~12", proc, 0, menuRead);

    proc = proctable_DefineProc("rasterv-read-xwdfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Read file in X Window Dump format");
    keymap_BindToKey(Keymap, "\033\022x", proc, 0);
    /* ESC - ^R - x*/
    menulist_AddToML(Menus, "File,Read X Window Dump~13", proc, (long)Inxwd, menuRead);

    proc = proctable_DefineProc("rasterv-read-xbmfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Read file in X Bitmap format");
    keymap_BindToKey(Keymap, "\033\022b", proc, 0);
    /* ESC - ^R - b*/
    menulist_AddToML(Menus, "File,Read X Bitmap~14", proc, (long)Inxbm, menuRead);

    proc = proctable_DefineProc("rasterv-read-file", ReadFileCommand,
				 &rasterview_classinfo, NULL,
				 "Read a raster file");
    keymap_BindToKey(Keymap, "\033\022r", proc, 0);	/* ESC - ^R - r */
    menulist_AddToML(Menus, "Raster I/O~42,Read Raster~12", proc, 0, menuRead);

    proc = proctable_DefineProc("rasterv-read-macpaint", RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Read a file allegedly in MacPaint format");
    keymap_BindToKey(Keymap, "\033\022Im", proc, (long)InMacPaint);	/* ESC - ^R - I - m */
    menulist_AddToML(Menus, "Raster I/O~42,Read MacPaint~22", proc, (long)InMacPaint, menuRead);

    proc = proctable_DefineProc("rasterv-write-macpaint", RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Write file in MacPaint format");
    keymap_BindToKey(Keymap, "\033\022IM", proc, (long)OutMacPaint);	/* ESC - ^R - I - M */
    menulist_AddToML(Menus, "Raster I/O~42,Write MacPaint~24", proc, (long)OutMacPaint, menuWrite);

    proc = proctable_DefineProc("rasterv-write-postscript", RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Write file in postscript format");
    keymap_BindToKey(Keymap, "\033\022IP", proc, (long)OutPostscript);	/* ESC - ^R - I - P */
    menulist_AddToML(Menus, "Raster I/O~42,Write Postscript~32", proc, (long)OutPostscript, menuWrite);

    proc = proctable_DefineProc("rasterv-read-rasterfile", ReadFileCommand,
				 &rasterview_classinfo, NULL,
				 "Read a file allegedly in the old ITC RasterFile format");
    keymap_BindToKey(Keymap, "\033\022r", proc, 0);	/* ESC - ^R - r *//* same as Read Raster above */
    menulist_AddToML(Menus, "Raster I/O~42,Read Old Raster~42", proc, 0, menuRead);

    /* paul's attempt to add XWD I/O : */

    proc = proctable_DefineProc("rasterv-read-xwdfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Read file in X Window Dump format");
    keymap_BindToKey(Keymap, "\033\022x", proc, 0);
    /* ESC - ^R - x*/
    menulist_AddToML(Menus, "Raster I/O~42,Read X Window Dump~52", proc, (long)Inxwd, menuRead);


    proc = proctable_DefineProc("rasterv-write-xwdfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Write file in X Window Dump format");
    keymap_BindToKey(Keymap, "\033\022X", proc, 0);
    /* ESC - ^R - X*/
    menulist_AddToML(Menus, "Raster I/O~42,Write X Window Dump~54", proc, (long)Outxwd, menuWrite);

/* end of paul's attempt to add XWD I/O : */

/* paul's attempt to add X Bitmap I/O : */

    proc = proctable_DefineProc("rasterv-read-xbmfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Read file in X Bitmap format");
    keymap_BindToKey(Keymap, "\033\022b", proc, 0);
    /* ESC - ^R - b*/
    menulist_AddToML(Menus, "Raster I/O~42,Read X Bitmap~62", proc, (long)Inxbm, menuRead);


    proc = proctable_DefineProc("rasterv-write-xbmfile",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Write file in X Bitmap format");
    keymap_BindToKey(Keymap, "\033\022B", proc, 0);
    /* ESC - ^R - B*/
    menulist_AddToML(Menus, "Raster I/O~42,Write X Bitmap~64", proc, (long)Outxbm, menuWrite);

/* end of paul's attempt to add X Bitmap I/O : */

#ifdef X11_ENV
    proc = proctable_DefineProc("rasterv-make-xwd",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Dump X window and read.");
    keymap_BindToKey(Keymap, "\033\022Iw", proc, 0);
    /* ESC - ^R - I - w*/
    menulist_AddToML(Menus, "Raster I/O~42,Make Window Dump~70", proc, (long)MakeWD, menuRead);


    proc = proctable_DefineProc("rasterv-make-asnap",
				 RasterIOCommand,
				 &rasterview_classinfo, NULL,
				 "Sweep area and read.");
    keymap_BindToKey(Keymap, "\033\022Ia", proc, 0);
    /* ESC - ^R - I - a*/
    menulist_AddToML(Menus, "Raster I/O~42,Make Area Dump~71", proc, (long)MakeAsnap, menuRead);
#endif
  /* add old names to proctable */

    proc = proctable_DefineProc("raster-copy-subraster", CopyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-replace-subraster", ReplaceCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-replace-subraster", ReplaceCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-scale-replace", ScaleReplaceCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-negative", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-white", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-black", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-gray", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-mirror-left-right", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-mirror-up-down", ModifyCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-rotate", RotateCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-scale", ScaleCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-shrink-image", ShrinkCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-center-image", CenterCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-upperleft-image", UpperLeftCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-zoom-in", ZoomInCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-zoom-out", ZoomOutCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-zoom-out-to-normal-size", NormalSizeCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-select-entire", SelectAllCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-region-select", RegionSelectCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-touchup", TouchUpCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-pan", PanCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-move-display-box", MoveDisplayBoxCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-hide-display-box", HideDisplayBoxCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-set-print-scaling", SetPrintSizeCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-extend-to-mat", ExtendToMatCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-crop", CropCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-uncrop", UncropCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-toggle-debug", ToggleDebug,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-file", ReadFileCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-xwdfile", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-file", ReadFileCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-macpaint", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-write-macpaint", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-write-postscript", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-rasterfile", ReadFileCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-xwdfile", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-write-xwdfile", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-read-xbmfile", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-write-xbmfile", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
#ifdef X11_ENV
    proc = proctable_DefineProc("raster-make-xwd", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
    proc = proctable_DefineProc("raster-make-asnap", RasterIOCommand,
		&rasterview_classinfo, NULL, "Obsolete");
#endif

    RastersInitiallyShrunk = environ_GetProfileSwitch("RastersInitiallyShrunk", FALSE);
    DEBUG(("LEAVE rasterview__InitializeClass\n"));
    return TRUE;
}


boolean rasterview__InitializeObject(ClassID, self)
struct classhdr *ClassID;
register struct rasterview  *self;
{
    ENTER(rasterview__InitializeObject);
    self->Menus = menulist_DuplicateML(Menus, self);
    self->Keystate = keystate_Create(self, Keymap);
    self->Cursor[0] = NULL;
    self->Cursor[1] = NULL;
    self->Cursor[2] = NULL;
    self->Cursor[3] = NULL;
    self->Cursor[4] = NULL;
    rectangle_EmptyRect(&self->PixChanged);
    self->needsFullUpdate = TRUE;
    self->OnScreen = FALSE;
    self->embedded = TRUE;
    self->DraggingEdge = 0;
    self->HasInputFocus = FALSE;
    self->IgnoreUntilNextUpTransition = FALSE;
    self->Mode = 0;
    rectangle_EmptyRect(&self->ViewSelection);
    self->Scale = 1;
    self->Original = NULL;
    self->Expansion = NULL;
    self->TouchUpX = self->TouchUpY = 0;
    self->HighlightIsGrey = FALSE;
    self->Xscroll = self->Yscroll = 0;
    self->InMessages = FALSE;
    self->CheckedParent = FALSE;
    rectangle_EmptyRect(&self->CurSelection);
    rectangle_EmptyRect(&self->DesiredSelection);
    self->Shrunken = RastersInitiallyShrunk;
    self->NeedsCentering = TRUE;

    self->ShowCoords = 0;

    self->inset = NULL;
    rectangle_EmptyRect(&self->InsetBox);

    rectangle_EmptyRect(&self->DisplayBox);
    rectangle_EmptyRect(&self->DisplayBoxSelection);
    self->DBXscroll = self->DBYscroll = 0;
    self->MovingDisplayBox = FALSE;
    self->DisplayBoxHidden = TRUE;
    self->DisplayBoxOverlapped = FALSE;

    self->GreyPattern = NULL;
    self->WhitePattern = NULL;
    self->BlackPattern = NULL;

    self->toolset = NULL;

    LEAVE(rasterview__InitializeObject);
    return TRUE;
}

void rasterview__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
register struct rasterview  *self;
{
    int ix;

    ENTER(rasterview__FinalizeObject);
    menulist_Destroy(self->Menus);
    if (self->Original != NULL) {
	rasterimage_Destroy(self->Original);
	self->Original = NULL; }
    keystate_Destroy(self->Keystate);

    for (ix=0; ix<5; ix++) {
	if (self->Cursor[ix] != NULL)
	    cursor_Destroy(self->Cursor[ix]);
    }

    if (self->toolset) 
	rasterview_DestroyToolset(self);

    if (self->inset) {
	view_UnlinkTree(self->inset);
	view_Destroy(self->inset);
    }

    LEAVE(rasterview__FinalizeObject);
}

struct view * rasterview__GetApplicationLayer(self)
register struct rasterview *self;
{
    register struct scroll *view;
    ENTER(rasterview__GetApplicationLayer);
    view = scroll_CreateScroller(self, scroll_LEFT | scroll_BOTTOM, environ_GetProfile("RasterScrollClass"));
    self->embedded = FALSE;
    self->Shrunken = FALSE;
    keystate_AddBefore(keystate_Create(self, Keymap), self->Keystate);
    rasterview_WantInputFocus(self, self);
    LEAVE(rasterview__GetApplicationLayer);
    return (struct view *) view;
}

/* ------ following material was added 5/28/92 ------ */

int rasterview__GetMode(self)
struct rasterview *self;
{
    return self->Mode;
}

void rasterview__CopySelection(self)
struct rasterview *self;
{
    CopyCommand(self);
}

