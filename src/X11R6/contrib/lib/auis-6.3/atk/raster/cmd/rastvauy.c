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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/rastvauy.c,v 1.12 1993/05/04 01:28:26 susan Exp $";
#endif

#include <andrewos.h> /* strings.h */
#include <class.h>
#define AUXMODULE 1
#include <rasterv.eh>

#include <im.ih>
#include <frame.ih>
#include <buffer.ih>
#include <view.ih>
#include <text.ih>
#include <dataobj.ih>
#include <sys/param.h> /* Defines MAXPATHLEN among other things */
#include <menulist.ih>
#include <cursor.ih>
#include <message.ih>
#include <rect.h>
#include <rastimg.ih>
#include <raster.ih>
#include <rastoolv.ih>
#include <heximage.ih>
#include <dispbox.h>

extern void PostMenus(), CorrectHighlight();
void OverlayInsetProc(), RemoveInsetProc(), ResizeInsetProc(), ImprintInsetProc();

/* clean happy one-pixel draw. Displays correctly in rasterview self, even if zoomed, but does *not* notify other observers. If (x, y) is out of bounds, nothing happens. bit may be TRUE, FALSE, or DRAW_REVERSE_PIXEL. Note that this does more than the internal function SetPixel(). */
void rasterview__SetPixel(self, ras, x, y, bit)
struct rasterview *self;
struct raster *ras;
long x, y;
int bit;
{
    struct rasterimage *pix;
    struct rectangle SR;

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL)) return;

    SR = self->ViewSelection;
    /* Note that coordinates are constrained to refer to actual pixels
      (left+width and top+height are the addresses of pixels
	just outside the raster) */
    if ((x < rectangle_Left(&SR)) 
	 || (x > rectangle_Left(&SR)+rectangle_Width(&SR)-1) 
	 || (y < rectangle_Top(&SR)) 
	 || (y > rectangle_Top(&SR)+rectangle_Height(&SR)-1)) {
	return;
    }

    if (bit==DRAW_REVERSE_PIXEL) {
	bit = !	rasterimage_GetPixel(pix, x, y); 
    }
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
	rasterimage_SetPixel(pix, x, y, ((bit) ? 1 : 0)); 
    }
}

/* the following method replicates code from rasterview__SetPixel(). This is just to improve the speed. Any changes to __SetPixel() should be added to  __PatternSetPixel() also. */

/* like rasterview_SetPixel(), but uses an 8x8 pattern instead of solid black or white. If pattern is NULL, invert the pixel. */
void rasterview__PatternSetPixel(self, x, y, pattern)
struct rasterview *self;
long x, y;
unsigned char *pattern;
{
    struct rasterimage *pix;
    struct rectangle SR;
    int bit;
    struct raster *ras = (struct raster *)rasterview_GetDataObject(self);

    if ((ras == NULL) || ((pix = raster_GetPix(ras)) == NULL)) return;

    SR = self->ViewSelection;
    /* Note that coordinates are constrained to refer to actual pixels
      (left+width and top+height are the addresses of pixels
	just outside the raster) */
    if ((x < rectangle_Left(&SR)) 
	 || (x > rectangle_Left(&SR)+rectangle_Width(&SR)-1) 
	 || (y < rectangle_Top(&SR)) 
	 || (y > rectangle_Top(&SR)+rectangle_Height(&SR)-1)) {
	return;
    }

    if (!pattern) {
	bit = !	rasterimage_GetPixel(pix, x, y); 
    }
    else {
	bit = (pattern[y&7] >> (x&7)) & 1;
    }

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
	rasterimage_SetPixel(pix, x, y, ((bit) ? 1 : 0)); 
    }

}

/* draws up to 64 pixels according to an 8x8 brush. If the brush is NULL, draw just one pixel. Uses rasterview_PatternSetPixel(). */
void rasterview__BrushSetPixel(self, x, y, pattern, brush)
struct rasterview *self;
long x, y;
unsigned char *pattern;
unsigned char *brush;
{
    if (!brush) {
	rasterview_PatternSetPixel(self, x, y, pattern);
    }
    else {
	int ix, jx;
	char ch;

	x -= 3;
	y -= 3;
	for (jx=0; jx<8; jx++) {
	    ch = brush[jx];
	    for (ix=0; ix<8; ix++) {
		if ((ch>>ix)&1) 
		    rasterview_PatternSetPixel(self, x+ix, y+jx, pattern);
	    }
	}
    }
}

void ToggleCoordProc(self, rock)
struct rasterview *self;
char *rock;
{
    self->ShowCoords = !(self->ShowCoords);
    
    if (self->ShowCoords)
	message_DisplayString(self, 20, "Mouse coordinates will be displayed.");
    else
	message_DisplayString(self, 20, "Mouse coordinates will not be displayed.");
}

/* set the selected region rectangle and redraw the highlight. R may be empty, and need not be within the raster boundaries (if it is, it will be trimmed or emptied).
  This does *not* redraw the scrollbars; to do that, use
      self->needsFullUpdate=FALSE;
      rasterview_WantUpdate(self, caller); 
*/
void rasterview__SetDesiredSelection(self, R)
struct rasterview *self;
struct rectangle *R;
{
    rectangle_IntersectRect(&self->DesiredSelection, R, &self->ViewSelection); 
    CorrectHighlight(self);
}

void MakeToolsetProc(self, rock)
struct rasterview *self;
char *rock;
{
    struct raster *r = (struct raster *)rasterview_GetDataObject(self);
    struct im *im;
    struct frame *fr;
    struct rastoolview *tv;
    boolean res;

    if (self->toolset)  {
	message_DisplayString(self, 40, "There is already a toolset window.");
	return;
    }

    if (!r) {
	message_DisplayString(self, 40, "This view has no data!");
	return;
    }

    im = im_Create(NULL);
    if (!im) {
	message_DisplayString(self, 40, "Unable to create new window.");
	return;
    }

    fr = frame_New();
    if (!fr) {
	message_DisplayString(self, 40, "Unable to create window frame.");
	return;
    }

    /*frame_SetCommandEnable(fr, TRUE);*/
    frame_PostDefaultHandler(fr, "message", frame_WantHandler(fr, "message"));

    tv = rastoolview_New();
    if (!tv) {
	message_DisplayString(self, 40, "Unable to create toolset.");
	return;
    }

    res = rastoolview_SetPrimaryView(tv, self);
    if (!res) {
	message_DisplayString(self, 40, "Unable to initialize toolset (this shouldn't happen).");
	return;
    }

    self->toolset = tv;
    frame_SetView(fr, tv);
    frame_SetQuitWindowFlag(fr, TRUE);
    im_SetView(im, fr);
    {   
	struct buffer *buf = buffer_FindBufferByData(r); 
	char *nm;
	if (buf) {
	    nm = buffer_GetFilename(buf);
	    if (nm)
		im_SetTitle(im, nm);
	}
    }
    rastoolview_WantInputFocus(tv, tv);
    PostMenus(self);
}

void KillToolsetProc(self, rock)
struct rasterview *self;
char *rock;
{
    struct im *toolim = NULL;
    struct frame *toolfr = NULL;

    if (!self->toolset) {
	message_DisplayString(self, 40, "There is no toolset window.");
	return;
    }

    toolim = rastoolview_GetIM(self->toolset);
    if (toolim)
	toolfr = (struct frame *)toolim->topLevel;

    rastoolview_RemoveObserver(self->toolset, self);
    rasterview_RemoveObserver(self, self->toolset);

    rastoolview_SetMoribund(self->toolset, 1); /* ensure that toolset doesn't try to kill itself when it sees it's being unlinked */

    if (toolim) {
	im_SetView(toolim, NULL);
    }

    if (toolfr) {
	frame_SetView(toolfr, NULL);
    }

    rastoolview_Destroy(self->toolset);
    self->toolset = NULL;

    if (toolim) {
	im_Destroy(toolim); 
    }
    if (toolfr) {
	frame_Destroy(toolfr);
    }

    rasterview_SetPan(self); /* does PostMenus() */
}

void rasterview__DestroyToolset(self)
struct rasterview *self;
{
    KillToolsetProc(self, 0);
}

boolean objecttest(self, name, desiredname)
struct rasterview *self;
char *name,*desiredname;
{
    if(class_Load(name) == NULL){
	char foo[640];
	sprintf(foo,"Can't load %s",name);
	message_DisplayString(self, 0, foo);
	return(FALSE);
    }
    if(!class_IsTypeByName(name,desiredname)){
	char foo[640];
	sprintf(foo,"%s is not a %s",name,desiredname);
	message_DisplayString(self, 0, foo);
	return(FALSE);
    }
    return(TRUE);
}

/* overlay an inset, using self->DesiredSelection for the box dimensions. insetname should be the name of a subclass of dataobject. If insetname is NULL, the user will be prompted for a class name. */
void rasterview__OverlayInset(self, insetname)
struct rasterview *self;
char *insetname;
{
    OverlayInsetProc(self, insetname);
}

void OverlayInsetProc(self, rock)
struct rasterview *self;
char *rock;
{
    char iname[100], buf[200];
    char *viewname;
    long pf;
    struct dataobject *dobj;
    struct raster *ras;

    if (!self) return;
    if (self->inset) return;
    ras = (struct raster *)rasterview_GetDataObject(self);
    if (!ras || ras->readOnly) return;

    if (!im_SupportsOffscreen(rasterview_GetIM(self))) {
	message_DisplayString(self, 10, "Overlaid insets will not work in this window manager.");
	return;
    }

    if (rectangle_IsEmptyRect(&self->DesiredSelection)) {
	message_DisplayString(self, 10, "You must select a region for the inset.");
	return;
    }

    if (rock == NULL || ((long)rock >= 0 && (long)rock < 256)) {
	pf = message_AskForString(self, 20, "Data object to insert here [text] : ", 0, iname, sizeof(iname));
        if (pf < 0){
            message_DisplayString(self, 0, "Cancelled.");
            return;
	}
	if (strlen(iname)==0) {
	    strcpy(iname, "text");
	}
    }
    else {
        strncpy(iname, rock, sizeof(iname));
    }

    if (objecttest(self, iname, "dataobject") == FALSE) return;
    dobj = (struct dataobject *)class_NewObject(iname);
    if (!dobj) {
	message_DisplayString(self, 10, "Unable to create object.");
	return;
    }

    if (class_IsTypeByName(iname, "text")) {
	message_DisplayString(self, 0, "Reading template...");
	text_ReadTemplate((struct text *)dobj, "default", FALSE);
    }

    viewname = dataobject_ViewName(dobj);

    self->inset = (struct view *)class_NewObject(viewname);
    if (!self->inset) {
	sprintf(buf, "Unable to create view %s.", viewname);
	message_DisplayString(self, 10, buf);
	return;
    }

    view_SetDataObject(self->inset, dobj);

    view_LinkTree(self->inset, self);

    self->InsetBox = self->DesiredSelection;
    view_InsertViewSize(self->inset, self, self->InsetBox.left-self->Xoff, self->InsetBox.top-self->Yoff, self->InsetBox.width, self->InsetBox.height);
    self->InsetUpdateWanted = TRUE;

    PostMenus(self);
    view_WantInputFocus(self->inset, self->inset);
    rasterview_NotifyObservers(self, NULL); 

    sprintf(buf, "Inserted %s inset at %d,%d (%d by %d)", iname, self->InsetBox.left, self->InsetBox.top, self->InsetBox.width, self->InsetBox.height);
    message_DisplayString(self, 10, buf);
}

/* remove the overlaid inset */
void rasterview__RemoveInset(self)
struct rasterview *self;
{
    RemoveInsetProc(self);
}

void RemoveInsetProc(self, rock)
struct rasterview *self;
char *rock;
{
    if (!self->inset) return;

    rasterview_WantInputFocus(self, self);
    view_UnlinkTree(self->inset);
    view_Destroy(self->inset);
    self->inset = NULL;

    self->DesiredSelection = self->InsetBox;

    if (FullSize(self)) {
	rectangle_UnionRect(&self->PixChanged, &self->PixChanged, &self->InsetBox);
    }

    PostMenus(self);
    rasterview_NotifyObservers(self, NULL); 
    rasterview_WantUpdate(self, self);
}

/* resize the overlaid inset to the dimensions of self->DesiredSelection */
void rasterview__ResizeInset(self)
struct rasterview *self;
{
    ResizeInsetProc(self);
}

void ResizeInsetProc(self, rock)
struct rasterview *self;
char *rock;
{
    char buf[200];

    if (!self->inset) return;

    if (rectangle_IsEmptyRect(&self->DesiredSelection)) {
	message_DisplayString(self, 10, "You must select a region for the inset.");
	return;
    }

    if (FullSize(self)) {
	rectangle_UnionRect(&self->PixChanged, &self->PixChanged, &self->InsetBox);
    }

    self->InsetBox = self->DesiredSelection;
    view_InsertViewSize(self->inset, self, self->InsetBox.left-self->Xoff, self->InsetBox.top-self->Yoff, self->InsetBox.width, self->InsetBox.height);
    self->InsetUpdateWanted = TRUE;

    sprintf(buf, "Placed inset at %d,%d (%d by %d)", self->InsetBox.left, self->InsetBox.top, self->InsetBox.width, self->InsetBox.height);
    message_DisplayString(self, 10, buf);

    rasterview_WantUpdate(self, self);
}

/* Imprint (paste down) the overlaid inset on the raster. */
void rasterview__ImprintInset(self, function)
struct rasterview *self;
long function;
{
    ImprintInsetProc(self, (function % 16));
}

/* the rock determines the transfer mode (copy, or, xor, etc.) It should be a number between 0 and 15, as defined in graphic.ch. If it is not, it defaults to the toolset's selected mode, or COPY if there is no toolset. */
void ImprintInsetProc(self, rock)
struct rasterview *self;
long rock;
{
    struct im *offim;
    struct graphic *offgr;
    struct raster *ras;
    struct rasterimage *pix, *pix2;
    struct rectangle R;

    DEBUG(("ImprintInsetProc: rock=%d", rock));
    if (rock<0 || rock>=16) {
	if (self->toolset)
	    rock = rastoolview_GetPasteMode(self->toolset);
	else
	    rock = pixelimage_COPY;
    }
    DEBUG(("ImprintInsetProc: new rock=%d", rock));

    if (!self->inset) return;

    ras = (struct raster *)rasterview_GetDataObject(self);
    if (!ras) return;
    pix = raster_GetPix(ras);
    if (!pix) return;
    pix2 = rasterimage_New();
    if (!pix2) return;

    self->DesiredSelection = self->InsetBox;
    rasterview_WantInputFocus(self, self);
    view_UnlinkTree(self->inset);

    offim = im_CreateOffscreen(rasterview_GetIM(self), self->InsetBox.width, self->InsetBox.height);

    if (!offim) {
	view_Destroy(self->inset);
	self->inset = NULL;

	if (FullSize(self)) {
	    rectangle_UnionRect(&self->PixChanged, &self->PixChanged, &self->InsetBox);
	}

	rasterview_WantUpdate(self, self);
	message_DisplayString(self, 10, "Unable to create transfer pixmap");
	return;
    }

    im_SetView(offim, self->inset);
    view_FullUpdate(self->inset, view_FullRedraw, 0, 0, -1 ,-1);
    offgr = im_GetDrawable(offim);

    rasterimage_Resize(pix2, self->InsetBox.width, self->InsetBox.height);
    graphic_ReadPixImage(offgr, 0, 0, pix2, 0, 0, self->InsetBox.width, self->InsetBox.height);

    rectangle_SetRectSize(&R, 0, 0, self->InsetBox.width, self->InsetBox.height);

    if (graphic_IsImageInverted(((struct view *)offim)->drawable))
	rasterimage_InvertSubraster(pix2, &R);

    rasterimage_BlitSubraster(pix, self->InsetBox.left, self->InsetBox.top, pix2, &R, rock);

    rasterimage_Destroy(pix2);
    im_SetView(offim, NULL);
    im_Destroy(offim);
    view_Destroy(self->inset);
    self->inset = NULL;
    
    /*rectangle_UnionRect(rasterimage_GetChanged(pix), rasterimage_GetChanged(pix), &self->InsetBox);*/

    PostMenus(self);
    rasterview_NotifyObservers(self, NULL); 
    rasterimage_NotifyObservers(pix, rasterimage_DATACHANGED);
    message_DisplayString(self, 10, "Inset imprinted.");
}

void rasterview__LinkTree( self, parent )
struct rasterview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if (parent && rasterview_GetIM(self)) {
	if (self->inset)
	    view_LinkTree(self->inset, self);
    }
}
