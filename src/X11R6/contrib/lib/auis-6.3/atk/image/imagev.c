/*
    imagev.c - implementation of a view on an image

    The following image methods are used in this module:

        New()
        Destroy()
        WriteOtherFormat()
        ReadOtherFormat()
        Reset()
        Duplicate()
        SetSaveFormatString()
        SaveFormatString()
        SetJPEGSaveQuality()
        GetJPEGSaveQuality()
        Dither()
        Normalize()
        Gray()
        Zoom()
        GammaCorrect()
        Halftone()
        WriteNative()
        

    The image represents the raw data.  The raw data is bound to the view
    by the SetDataObject() override.  Foriegn image types are
    implemented as subclasses of the base image class.  Image subclasses
    only need override the following image methods:

        Read()
        Write()
        WriteNative()
        Load()

    Color allocation is handled by the graphic class, from which
    this view is derived.  The following graphic interfaces are used:

        WriteImage()

    This class provides the following, limited, set of features:

      * screen redraw
        - panning refresh
        - scaling

      * user dialog via menu and keystroke callbacks for:
        - import/export of foreign image types
        - basics view manipulations; scale, pan, ShowTrue, others not yet
          enabled
        - save, read

    The following view methods are overridden here:

      FullUpdate()
      Update()
      WantInputFocus()
      ReceiveInputFocus()
      LoseInputFocus()
      Hit()
      DesiredSize()
      PostMenus()
      PostKeyState()
      ObservedChanged()
      Print()
      ReceiveColormap()
      LoseColormap()
      UnlinkNotification()
      LinkTree()
      UnlinkTree()
      GetApplicationLayer()
      GetInterface()
      SetDataObject()

	Copyright Carnegie Mellon University 1992 - All rights reserved
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/imagev.c,v 1.34 1994/02/24 22:38:17 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <rect.h>
#include <attribs.h>
#include <filetype.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <cursor.ih>
#include <bind.ih>
#include <environ.ih>
#include <complete.ih>
#include <message.ih>
#include <scroll.ih>
#include <view.ih>
#include <buffer.ih>
#include <image.ih>
#include <graphic.ih>
#include <frame.ih>
#include <im.ih>
#include <txttroff.ih>
#include <sbuttonv.ih>
#include <sbutton.ih>
#include <region.ih>
#include <faces.ih>
#include <pbm.ih>
#include <sunraster.ih>
#include <gif.ih>
#include <tif.ih>
#include <rle.ih>
#include <xwd.ih>
#include <xbitmap.ih>
#include <fbm.ih>
#include <pcx.ih>
#include <img.ih>
#include <mac.ih>
#include <cmuwm.ih>
#include <mcidas.ih>
#include <jpeg.ih>
#include <cmap.ih>
#include <cmapv.ih>
#include <imagev.eh>

#define CONTINUOUS_PAN	0
#define DISCREET_PAN	1
#define PURE_POSTSCRIPT (-1) /* When this is passed to writePS as the
  toplevel arg, we're just writing pure postscript. */

#define	imagev_DefaultMenus		(1<<6)
#define	imagev_ReadWriteMenus		(1<<7)
#define	imagev_ReadOnlyMenus		(1<<8)
#define	imagev_GoodImageMenus		(1<<9)
#define imagev_JPEGFormatMenus		(1<<10)
#define imagev_CanHavePrivateCmap	(1<<11)
#define imagev_HasPrivateCmap		(1<<12)
#define imagev_HasNotPrivateCmap	(1<<13)

static struct keymap *keymap;
static struct menulist *menulist;
static struct cursor *waitCursor;

#define image_STARTHEIGHT (150)
#define image_STARTWIDTH (150)
#define DEFAULT_BORDER_SIZE (5)

/* This macro accesses the displayed copy of the image if possible, otherwise it uses the original.
 It should only be used where the scaled image is supposed to be used.  */
#define IMAGE(self) \
((struct image *) (self->scaled ? self->scaled : self->orig))

/* This macro is used to clear the passed rect to the background */
#define ClearRect(self, RectPtr)			\
    imagev_SetTransferMode(self, graphic_WHITE);	\
    imagev_FillRect(self, RectPtr, imagev_BlackPattern(self))

/* Forward declarations for menu & keystroke callbacks */
static void Import_Cmd(), Export_Cmd(),
  Dither(), Halftone(), Reduce(), Gray(),
  Normalize(), Brighten(), GammaCorrect(),
  ScaleToFit(), SaveAs(), InfoCmd(), ShowTrue(),
  ShowFixed(), Write_Postscript(), ReadCmd(),
  ChangeZoomCmd(), PanToOriginCmd(), RectToPix(),
  SetSaveQuality(), SetSaveFormat();

/* Definitions for menu and keystroke bindings */
static struct bind_Description imagevBindings[] = {

  {"imagev-import-gif", NULL, gif_imageType, "Import~30, GIF~10",
  gif_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a gif image."},

  {"imagev-import-tif", NULL, tif_imageType, "Import~30, TIFF~12",
  tif_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a TIFF image."},

  {"imagev-import-jpeg", NULL, jpeg_imageType, "Import~30, JPEG~14",
  jpeg_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a JPEG image." },

  {"imagev-import-pbm", NULL, pbm_imageType, "Import~30, PBM~16",
  pbm_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a pbm image."},

  {"imagev-import-xwd", NULL, xwd_imageType, "Import~30, XWD~18",
  xwd_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a xwd image."},

  {"imagev-import-sunraster", NULL, sunraster_imageType,
  "Import~12, Sunraster~10",  sunraster_imageType, imagev_DefaultMenus,
  (void(*)()) Import_Cmd, "Import a sunRaster image."},

  {"imagev-import-xbitmap", NULL, xbitmap_imageType, "Import~30, Xbitmap~12",
  xbitmap_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a X Bitmap image."},

  {"imagev-import-xpixmap", NULL, xpixmap_imageType, "Import~30, Xpixmap~14",
  xpixmap_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a X Pixmap image."},

  {"imagev-import-mac", NULL, mac_imageType, "Import~30, Mac~16",
  mac_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a mac image."},

  {"imagev-import-cmuwm", NULL, cmuwm_imageType, "Import~30, ATK Raster~18",
  cmuwm_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a cmuwm image."},

  {"imagev-export-gif", NULL, gif_imageType, "Export~40, GIF~10",
  gif_imageType, imagev_DefaultMenus, (void(*)()) Export_Cmd,
  "Write out a gif image."},

  {"imagev-export-postscript", NULL, 0, "Export~40, Postscript~12",
  0, imagev_DefaultMenus, (void(*)()) Write_Postscript,
  "Write out a Postscript file."},

  {"imagev-export-jpeg", NULL, jpeg_imageType, "Export~40, JPEG~14",
  jpeg_imageType, imagev_DefaultMenus, (void(*)()) Export_Cmd,
  "Write out a JPEG image."},

  {"imagev-export-cmuwm", NULL, cmuwm_imageType, "Export~40, ATK Raster~16",
  cmuwm_imageType, imagev_DefaultMenus, (void(*)()) Export_Cmd,
  "Write out a cmuwm image."},

#ifdef THESEWORK
  {"imagev-import-faces", NULL, faces_imageType, "Import~30, FACES~18",
  faces_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a faces image."},

  {"imagev-import-rle", NULL, rle_imageType, "Import~30, RLE~19",
  rle_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a rle image."},

  {"imagev-import-g3", NULL, g3_imageType, "Import~30, G3~20",
  g3_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a g3 FAX image."},

  {"imagev-import-fbm", NULL, fbm_imageType, "Import~30, FBM~21",
  fbm_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a fbm image."},

  {"imagev-import-pcx", NULL, pcx_imageType, "Import~30, PCX~22",
  pcx_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a pcx image."},

  {"imagev-import-img", NULL, img_imageType, "Import~30, IMG~23",
  img_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a img image."},

  {"imagev-import-mcidas", NULL, mcidas_imageType, "Import~30, MCIDAS~24",
  mcidas_imageType, imagev_DefaultMenus, (void(*)()) Import_Cmd,
  "Import a mcidas image."},
#endif /* THESEWORK */

#ifdef THISWORKS
  {"imagev-gray", NULL, 0, "Image~20, gray~20",
  0, imagev_GoodImageMenus, (void(*)()) Gray,
  "GrayScale an image."},

  {"imagev-dither", NULL, 0, "Image~20, dither~21",
  0, imagev_GoodImageMenus, (void(*)()) Dither,
  "Dither an image."},

  {"imagev-scale-to-fit", NULL, 0, "Image~20, Scale to fit~10",
  0, imagev_GoodImageMenus, (void(*)()) ScaleToFit,
  "Scale an image to fit the available screen space."},

  {"imagev-halftone", NULL, 0, "Image~20, halftone~30",
  0, imagev_GoodImageMenus, (void(*)()) Halftone,
  "Halftone an image."},

  {"imagev-reduce", NULL, 0, "Image~20, reduce~31",
  0, imagev_GoodImageMenus, (void(*)()) Reduce,
  "Reduce an image."},

  {"imagev-normalize", NULL, 0, "Image~20, normalize~32",
  0, imagev_GoodImageMenus, (void(*)()) Normalize,
  "Normalize an image."},

  {"imagev-brighten", NULL, 0, "Image~20, brighten~33",
  0, imagev_GoodImageMenus, (void(*)()) Brighten,
  "Brighten an image."},

  {"imagev-gamma-correct", NULL, 0, "Image~20, gamma correct~34",
  0, imagev_GoodImageMenus, (void(*)()) GammaCorrect,
  "GammaCorrect an image."},

#endif

  {"imagev-image-read", NULL, 0, "Image~20, Read~10",
  0, imagev_DefaultMenus, (void(*)()) ReadCmd,
  "Read in a new image file."},

  {"imagev-image-write", NULL, 0, "Image~20, Write~11",
  0, imagev_DefaultMenus, (void(*)()) SaveAs,
  "Save image to file."},

  {"imagev-zoom-in", "\033Z", 1, "Image~20, Zoom In~20",
  1, imagev_GoodImageMenus, (void(*)()) ChangeZoomCmd,
  "Change scale of view."},

  {"imagev-zoom-out", "\033z", -1, "Image~20, Zoom Out~22",
  -1, imagev_GoodImageMenus, (void(*)()) ChangeZoomCmd,
  "Change scale of view."},

  {"imagev-zoom-normal", "\033n", 0, "Image~20, Normal Size~24",
  0, imagev_GoodImageMenus, (void(*)()) ChangeZoomCmd,
  "Change scale of view."},

  {"imagev-pan-to-origin", "\033o", 0, "Image~20, Pan to Origin~30",
  0, imagev_GoodImageMenus, (void(*)()) PanToOriginCmd,
  "Pan to (0,0)."},

  {"imagev-show-true", NULL, 0, "Image~20, Show true~40",
  0, imagev_GoodImageMenus | imagev_CanHavePrivateCmap |
  imagev_HasNotPrivateCmap, (void(*)()) ShowTrue,
  "Show image with private cmap."},

  {"imagev-show-fixed", NULL, 0, "Image~20, Show fixed~42",
  0, imagev_GoodImageMenus | imagev_CanHavePrivateCmap |
  imagev_HasPrivateCmap, (void(*)()) ShowFixed,
  "Show image with fixed cmap."},

  {"imagev-set-save-quality", NULL, 0, "Image~20, Set Save Quality~50",
  0, imagev_GoodImageMenus | imagev_JPEGFormatMenus, (void(*)()) SetSaveQuality,
  "Set the quality level for the JPEG compressor [5-95]."},

  {"imagev-set-save-format", NULL, 0, "Image~20, Set Save Format~60",
  0, imagev_DefaultMenus, (void(*)()) SetSaveFormat,
  "Set the save format to be used by image. Can be jpeg or gif."},

#if 0
  {"imagev-image-info", NULL, 0, "Image~20, Info~60",
  0, imagev_DefaultMenus, (void(*)()) InfoCmd,
  "Get information about image."},
#endif

  NULL
};

/* Set dimensions on rect r such that it's smaller by deltax x deltay */
#define INSETRECT(r, deltax, deltay)					\
  rectangle_SetRectSize((r), rectangle_Left((r)) + (deltax),		\
  rectangle_Top((r)) + (deltay), rectangle_Width((r)) - 2*(deltax),	\
  rectangle_Height((r)) - 2*(deltay));

/* Set and retract the wait cursor */
#define POSTWAITCURSOR(self) im_SetProcessCursor(waitCursor)
#define RETRACTWAITCURSOR(self) im_SetProcessCursor(NULL)

/* Post a cursor of the specified type */
static void
PostCursor( self, type )
  struct imagev *self;
  int type;
{ struct rectangle r;
  if (self->cursorPosted) {
      self->cursorPosted = FALSE;
      imagev_RetractCursor(self, self->cursor);
  }
  imagev_GetVisualBounds(self,&r);
  cursor_SetStandard(self->cursor, type);
  super_PostCursor(self, &r, self->cursor);
  self->cursorPosted = TRUE;
}

boolean
imagev__InitializeClass( classID )
    struct classheader *classID;
{
  struct classinfo *classInfo = NULL;
  keymap = keymap_New();
  menulist = menulist_New();
  classInfo = class_Load("imagev");
  bind_BindList(imagevBindings, keymap, menulist, classInfo);
  waitCursor = cursor_Create(NULL);
  cursor_SetStandard(waitCursor, Cursor_Wait);
  return(TRUE);
}

/* This is the internal sbutton label, used to specify the image "pane" */
static char panebutton[]="panebutton";

boolean
imagev__InitializeObject( classID, self )
    struct classheader *classID;
    struct imagev *self;
{
  self->orig = self->scaled = NULL;
  if(! (self->keystate = keystate_Create(self, keymap)) )
    return(FALSE);
  self->menulist = menulist_DuplicateML(menulist, self);
  self->cursor = cursor_Create(self);
  self->cursorPosted = FALSE;
  cursor_SetStandard(self->cursor, Cursor_Arrow);
  self->do_fullupdate = TRUE;
  self->do_renderupdate = FALSE;
  self->image_modified = FALSE;
  self->buttonprefs = sbutton_GetNewPrefs(panebutton);
  sbutton_InitPrefs(self->buttonprefs, panebutton);
  self->haveFocus = self->isLinked = FALSE;
  rectangle_EmptyRect(&self->shown);
  rectangle_EmptyRect(&self->imagerect);
  self->canvas = NULL;
  self->onScreen = FALSE;
  self->embedded = TRUE;
  self->scale = imagev_NormScale;
  self->panx = self->pany = 0;
  self->bordersize = environ_GetProfileInt("imagebordersize",
					   DEFAULT_BORDER_SIZE);
  self->panStyle = environ_GetProfileInt("imagepanstyle",
					 CONTINUOUS_PAN);
  self->lastPixRect = (struct rectangle *) calloc(1, sizeof(struct rectangle));
  rectangle_EmptyRect(self->lastPixRect);
  self->havePrivateCmap = FALSE;
  self->privateCmap = NULL;
  return(TRUE);
}


void
imagev__FinalizeObject( classID, self )
    struct classheader *classID;
    struct imagev *self;
{
  if(self->scaled) {
      image_Destroy(self->scaled);
      self->scaled = NULL;
  }

  if(self->cursor) {
      cursor_Destroy(self->cursor);
      self->cursor=NULL;
  }

  if(self->buttonprefs) {
      sbutton_FreePrefs(self->buttonprefs);
      self->buttonprefs=NULL;
  }
  
  if(self->privateCmap) {
      colormap_Destroy(self->privateCmap);
      self->privateCmap=NULL;
  }
  
  if(self->menulist) {
      menulist_Destroy(self->menulist);
      self->menulist=NULL;
  }
  if(self->keystate) {
      keystate_Destroy(self->keystate);
      self->keystate=NULL;
  }
  
  if(self->lastPixRect) {
      free(self->lastPixRect);
      self->lastPixRect=NULL;
  }
}

struct sbutton_info thebutton = {
    NULL,   /* the prefs struct will be filled later */
    "",	    /* the label is empty */
    0,	    /* the rock isn't needed */
    NULL,   /* ditto for the trigger atom */
    FALSE,  /* initially not lit, will be set appropriately */
};

/* Splat the image whose upperleft is specified by DestPt into SrcRect */
#define SendImage(self, DestPt, SrcRect)		\
graphic_WriteImage(imagev_GetDrawable(self),		\
		   point_X(DestPt), point_Y(DestPt),	\
		   IMAGE(self),				\
		   rectangle_Left(SrcRect),		\
		   rectangle_Top(SrcRect),		\
		   rectangle_Width(SrcRect),		\
		   rectangle_Height(SrcRect))


/* Draw the outside of sbutton such that it looks like a window pane */

void
DrawBorder( self, outer, inner )
    struct imagev *self;
    struct rectangle *outer, *inner;
{
    struct region *rgn1 = region_CreateEmptyRegion();
    struct region *rgn2 = region_CreateEmptyRegion();
    struct region *origclip = region_CreateEmptyRegion();
    struct region *clip;

    if(!rgn1 || !rgn2 || !origclip) {
	if(rgn1) region_Destroy(rgn1);
	if(rgn2) region_Destroy(rgn2);
	if(origclip) region_Destroy(origclip);
	return;
    }
    clip = imagev_GetClippingRegion(self, origclip);
    imagev_SetTransferMode(self, graphic_COPY);
    INSETRECT(inner, self->bordersize, self->bordersize);
    region_RectRegion(rgn1, outer);
    region_RectRegion(rgn2, inner);
    region_SubtractRegion(rgn1, rgn2, rgn2);
    if(clip)
	region_IntersectRegion(rgn2, origclip, rgn2);
    imagev_SetClippingRegion(self, rgn2);
    thebutton.prefs = self->buttonprefs;
    thebutton.lit = self->haveFocus;
    sbuttonv_SafeDrawButton(self, &thebutton, outer);
    if (clip)
	imagev_SetClippingRegion(self, clip);
    else
	imagev_ClearClippingRect(self);
    region_Destroy(rgn1);
    region_Destroy(rgn2);
    if(clip)
	region_Destroy(clip);
}

/* Get coordinates of image relative to pane */
void
GetScreenCoordinates( self, pixRect )
    struct imagev *self;
    struct rectangle *pixRect;
{
    struct image *image = IMAGE(self);
    struct rectangle imageRect;

    if(image_Data(image)) {
	rectangle_SetRectSize(&imageRect, 0, 0,
			      imagev_ToImageW(self, image_Width(image)),
			      imagev_ToImageH(self, image_Height(image)));
	RectToPix(self, pixRect, &imageRect);
    }
    else
	rectangle_EmptyRect(pixRect);
}

void
imagev__FullUpdate( self, type, left, top, width, height )
    struct imagev *self;
    enum view_UpdateType type;
    long left, top, width, height;
{
    self->isLinked = TRUE;
    if(type == view_FullRedraw || type == view_LastPartialRedraw) {
	struct region *clipRgn = region_CreateEmptyRegion();
	struct region *clipEstablished;

	self->onScreen = TRUE;
	clipEstablished = imagev_GetClippingRegion(self, clipRgn);
	if(!self->do_renderupdate) { /* In here we must redraw the "pane"
	 around the screen image */
	    struct rectangle r;

	    if(self->canvas)
		free(self->canvas);
	    imagev_GetLogicalBounds(self, &r);
	    self->canvas = rectangle_Duplicate(&r);
	    DrawBorder(self, &r, self->canvas);
	}
	if(image_Data(IMAGE(self))) {
	    struct region *rgn1 = region_CreateEmptyRegion();
	    struct rectangle Pix, Src;
	    struct point Dest;

	    /* Starting here and ending with the SendImage call, we're just setting
	    up the regions of the screen that are to be rendered.  We take into
	    account the initial clipping region and only include in the final
	    clipping region that portion of the image that is visible that needs
	    to be redraw. */
	    GetScreenCoordinates(self, &Pix);
	    region_RectRegion(rgn1, self->canvas);
	    if(clipEstablished)
		region_IntersectRegion(rgn1, clipRgn, rgn1);
	    imagev_SetClippingRegion(self, rgn1);
	    imagev_SetTransferMode(self, graphic_COPY);

	    point_SetX(&Dest, MAX(Pix.left, rectangle_Left(self->canvas)));
	    point_SetY(&Dest, MAX(Pix.top, rectangle_Top(self->canvas)));
	    rectangle_IntersectRect(&Src, &Pix, self->canvas);
	    rectangle_SetLeft(&Src, MAX(rectangle_Left(self->canvas) - Pix.left, 0));
	    rectangle_SetTop(&Src, MAX(rectangle_Top(self->canvas) - Pix.top, 0));

	    /* Throw image onto screen */
	    SendImage(self, &Dest, &Src);

	    /* Everything from here on down to the end of this block is dealing
	    with clearing that portion of the screen where the old image may have
	    been. */
	    rectangle_IntersectRect(&Pix, &Pix, self->canvas);
	    if(self->do_renderupdate) {
		if(!rectangle_IsEmptyRect(&Pix)) {
		    struct region *rgn2 = region_CreateEmptyRegion();
		    region_RectRegion(rgn2, &Pix);
		    region_XorRegion(rgn1, rgn2, rgn1);

		    if(!rectangle_IsEmptyRect(self->lastPixRect)) {
			struct region *lastPixRgn = region_CreateEmptyRegion();
			region_RectRegion(lastPixRgn, self->lastPixRect);
			region_IntersectRegion(rgn1, lastPixRgn, rgn1);
			region_Destroy(lastPixRgn);
		    }

		    imagev_SetClippingRegion(self, rgn1);
		    ClearRect(self, self->canvas);
		    region_Destroy(rgn2);
		}
		else
		    ClearRect(self, self->canvas);
	    }
	    else
		PostCursor(self, Cursor_Arrow);
	    *self->lastPixRect = Pix;
	    region_Destroy(rgn1);
	}
	if(clipEstablished)
	    imagev_SetClippingRegion(self, clipRgn);
	else
	    imagev_ClearClippingRect(self);
    }
    else if(type == view_MoveNoRedraw) { /* This means that a parent inset has
     bitblit'ed us somewhere and now we have to retract our old cursor and
     post a new one over our new screen space. */
	imagev_RetractCursor(self, self->cursor);
	PostCursor(self, Cursor_Arrow);
    }
    else if(type == view_Remove) { /* This means that we've been removed from the
     screen, probably via an UnlinkTree call, and now we must simply retract our
     cursors and initialize some tracker instance vars */
	if(self->canvas)
	    free(self->canvas);
	self->canvas = NULL;
	imagev_RetractCursor(self, self->cursor);
	self->onScreen = FALSE;
    }
    self->do_fullupdate = self->do_renderupdate = FALSE;
}

struct view *
imagev__Hit( self, action, x, y, numberOfClicks)
    struct imagev *self;
    enum view_MouseAction action;
    long x;
    long y;
    long numberOfClicks;
{
    long iw, ih, ix, iy, px, py;
    iw = self->canvas->width;
    ih = self->canvas->height;
    ix = imagev_ToImageX(self, x+imagev_ToPixW(self,1)/2);
    iy = imagev_ToImageY(self, y+imagev_ToPixH(self,1)/2);

    if (!self->haveFocus) { /* We don't have the input focus, so call for it.
	When we have the input focus we can post our menus and keystates */
	imagev_WantInputFocus(self, self);
    }

    switch(action) {
	case view_LeftDown: /* Do nothing really.  Getting input focus. */
	    break;
	case view_RightDown: /* This is a start-pan event */
	    PostCursor(self, Cursor_CrossHairs);
	    self->rockx = ix;
	    self->rocky = iy;
	    if(self->panStyle == DISCREET_PAN) {
		px = imagev_ToPixX(self, ix);
		py = imagev_ToPixY(self, iy);
		imagev_SetTransferMode(self, graphic_INVERT);
		imagev_MoveTo(self, 0, py);
		imagev_DrawLineTo(self, iw-1, py);
		imagev_MoveTo(self, px, 0);
		imagev_DrawLineTo(self, px, ih-1);
	    }
	    self->lastx = px;
	    self->lasty = py;
	    break;
	case view_LeftMovement: /* Do nothing. Selections and editing have yet
	 to be implemented. */
	    break;
	case view_RightMovement: /* This is a pan event */
	    if(self->panStyle == DISCREET_PAN) {
		px = imagev_ToPixX(self, ix);
		py = imagev_ToPixY(self, iy);
		if( px != self->lastx || py != self->lasty ) {
		    imagev_SetTransferMode(self, graphic_INVERT);
		    imagev_MoveTo(self, 0, self->lasty);
		    imagev_DrawLineTo(self, iw-1, self->lasty);
		    imagev_MoveTo(self, self->lastx, 0);
		    imagev_DrawLineTo(self, self->lastx, ih-1);
		    imagev_MoveTo(self, 0, py);
		    imagev_DrawLineTo(self, iw-1, py);
		    imagev_MoveTo(self, px, 0);
		    imagev_DrawLineTo(self, px, ih-1);
		    self->lastx = px;
		    self->lasty = py;
		}

		break;
	    }
	case view_RightUp: /* This is a end-pan event */
	    if(self->panStyle == DISCREET_PAN) {
		imagev_SetTransferMode(self, graphic_INVERT);
		imagev_MoveTo(self, 0, self->lasty);
		imagev_DrawLineTo(self, iw-1, self->lasty);
		imagev_MoveTo(self, self->lastx, 0);
		imagev_DrawLineTo(self, self->lastx, ih-1);
	    }
	    self->panx += (self->rockx - ix);
	    self->pany += (self->rocky - iy);
	    self->do_renderupdate = TRUE;
	    imagev_WantUpdate(self, self);
	    break;
	case view_LeftUp: /* Do nothing.  We probably have the input focus now. */
	    break;
    }
    return((struct view *) self); /* Return self so that the up-transitions are
     short-circuited to us */
}

void
imagev__ReceiveInputFocus( self )
    struct imagev *self;
{
    if(self->haveFocus == FALSE) { /* Ask parent for input focus */
	self->haveFocus = TRUE;
	super_ReceiveInputFocus(self);
	if(self->embedded) { /* We are an embedded inset. Toggle pane */
	    struct rectangle r, *childrect;
	    imagev_GetLogicalBounds(self, &r);
	    childrect = rectangle_Duplicate(&r);
	    DrawBorder(self, &r, childrect);
	    free(childrect);
	}
    }
}

void
imagev__LoseInputFocus( self )
    struct imagev *self;
{   
    if(self->haveFocus == TRUE) { /* Ask parent to take away input focus. */
	self->haveFocus = FALSE;
	super_LoseInputFocus(self);
	if(self->isLinked && self->onScreen && self->embedded) {
	    /* Toggle pane */
	    struct rectangle r, *childrect;
	    imagev_GetLogicalBounds(self, &r);
	    childrect = rectangle_Duplicate(&r);
	    DrawBorder(self, &r, childrect);
	    free(childrect);
	}
    }
}

enum view_DSattributes 
imagev__DesiredSize( self, width, height, pass, desiredWidth, desiredHeight )
     struct imagev *self;
     long width, height;
     enum view_DSpass pass;
     long *desiredWidth, *desiredHeight;
{
    /* This routine needs to be smarter.  Now we just DEMAND that we be given
     enough space to accomodate our image dimensions.  In future, we may
     want to provide the user preferences as to thresholds for when to scale
     down an image such that it can be viewed as a whole. */
    struct image *image = IMAGE(self);
    *desiredWidth = image_Width(image) > 0 ?
      (image_Width(image) + (2 * self->bordersize)):
      image_STARTWIDTH;
    *desiredHeight = image_Height(image) > 0 ?
      image_Height(image) + (2 * self->bordersize) :
      image_STARTHEIGHT ;
    return(view_Fixed);
}

void
imagev__PostMenus( self, menulist )
    struct imagev *self;
    struct menulist *menulist;
{
    int mask = imagev_DefaultMenus;

    menulist_ClearChain(self->menulist);
    if(image_Data(self->orig)) {
	mask |= imagev_GoodImageMenus;
	if(strcmp(image_SaveFormatString(self->orig), "jpeg") == 0)
	    mask |= imagev_JPEGFormatMenus;
	if(imagev_DisplayClass(self) &
	   (graphic_PseudoColor | graphic_DirectColor | graphic_GrayScale)) {
	    mask |= imagev_CanHavePrivateCmap;
	    if(self->havePrivateCmap) {
		mask |= imagev_HasPrivateCmap;
		mask &= ~imagev_HasNotPrivateCmap;
	    }
	    else {
		mask |= imagev_HasNotPrivateCmap;
		mask &= ~imagev_HasPrivateCmap;
	    }
	}
	else
	    mask &= ~imagev_CanHavePrivateCmap;
    }
    menulist_SetMask(self->menulist, mask);
    if(menulist) 
	menulist_ChainAfterML(self->menulist, menulist, NULL);
    super_PostMenus(self, self->menulist);
}

void
imagev__ObservedChanged( self, changed, value )
    struct imagev *self;
    struct image *changed;
    long value;
{
    if(changed == self->orig)
	switch(value) { /* A new image has been bounds to us.  Render it. */
	    case image_NEW:
		imagev_WantNewSize(self, self);
		self->scale = imagev_NormScale;
		self->panx = self->pany = 0;
		if(self->scaled==NULL) self->scaled=image_New();
		if(self->scaled) {
		    image_Reset(self->scaled);
		    image_Duplicate(self->orig, self->scaled);
		    self->scaled->inited=FALSE;
		}
		self->do_fullupdate = TRUE;
		self->do_renderupdate = TRUE;
		self->orig->inited=FALSE;
		imagev_WantUpdate(self, self);
	    case observable_OBJECTCHANGED:
		break;
	    case observable_OBJECTDESTROYED:
		break;
	}
}

void
imagev__Update( self )
    struct imagev *self;
{
    if(self->do_fullupdate) { /* Render from scratch the complete image */
	struct rectangle r;
	imagev_GetVisualBounds(self, &r);
	ClearRect(self, &r);
	imagev_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
    }
    else { /* pan event: do_renderupdate */
	imagev_FullUpdate(self, view_FullRedraw, 0, 0, 0, 0);
    }
}

/* This needs to be table-driven.  For now, blow it off. */
static char *
imageTypeName(type)
    enum image_fileType type;
{
    static char *objName = NULL;
    switch (type) {
	case faces_imageType:
	    objName = "faces";
	    break;
	case pbm_imageType:
	    objName = "pbm";
	    break;
	case sunraster_imageType:
	    objName = "sunraster";
	    break;
	case gif_imageType:
	    objName = "gif";
	    break;
	case tif_imageType:
	    objName = "tif";
	    break;
	case rle_imageType:
	    objName = "rle";
	    break;
	case xwd_imageType:
	    objName = "xwd";
	    break;
	case xbitmap_imageType:
	    objName = "xbitmap";
	    break;
	case fbm_imageType:
	    objName = "fbm";
	    break;
	case pcx_imageType:
	    objName = "pcx";
	    break;
	case img_imageType:
	    objName = "img";
	    break;
	case mac_imageType:
	    objName = "mac";
	    break;
	case cmuwm_imageType:
	    objName = "cmuwm";
	    break;
	case mcidas_imageType:
	    objName = "mcidas";
	    break;
	case jpeg_imageType:
	    objName = "jpeg";
	    break;
	default:
	    fprintf(stderr, "imagev: no such image type.\n");
	    return;
    }
    return(objName);
}

/* This routine imports the given file as a particular image type. We don't
bother with automatic recognition, we just have the user say what type of image
it is. This should ultimately go away. */
static struct image *
image_Import( filename, type )
    char *filename;
    enum image_fileType type;
{
    int ret = -1;
    struct image *image;
    char *objName = NULL;
    objName = imageTypeName(type);
    if(image = (struct image *) class_NewObject(objName)) {
	image_Load(image, filename, NULL);
    }
    return(image);
}

/* Write the given image to file with the specified output format (JPEG, GIF) */
static int
image_Export( image, filename, type )
    struct image *image;
    char *filename;
    enum image_fileType type;
{
    int ret = -1;
    struct image *newimage;
    char *objName = NULL;
    objName = imageTypeName(type);
    if(newimage = (struct image*) class_NewObject(objName)) {
	image_Duplicate(image, newimage);
	ret = image_WriteNative(newimage, NULL, filename);
	image_Destroy(newimage);
    }
    return(ret);
}


/* Callback bound to each item on the Import menu card.. Passed in type
specifies which item (image format) was chosen */
static void
Import_Cmd( self, type )
    struct imagev *self;
    enum image_fileType type;
{
    struct image *image = self->orig;
    char filename[MAXPATHLEN + 1];
    char message[MAXPATHLEN];
    int result = 0;
    struct image *newimage;

    im_GetDirectory(filename);
    strcat(filename, "/");
    if(completion_GetFilename(self, "Import image file: ", filename,
			      filename, sizeof(filename), FALSE, TRUE) == -1)
	return;
    im_SetProcessCursor(waitCursor);
    if(newimage = image_Import(filename, type)) { /* OK */
	boolean havecmap=self->havePrivateCmap;
	if(self->scaled) {
	    image_Destroy(self->scaled);
	    self->scaled = NULL;
	}
	if(havecmap) {
	    imagev_WantColormap(self, self , NULL);
	    imagev_WantUpdate(self, self);
	    if(self->privateCmap) {
		colormap_Destroy(self->privateCmap);
		self->privateCmap=NULL;
	    }
	}
	image_Reset(image);
	image_Duplicate(newimage, image);
	image_Destroy(newimage);
	image_Compress(image);
	if(havecmap) {
	    self->privateCmap = im_CreateColormap(imagev_GetIM(self));
	    imagev_WantColormap(self, self, &self->privateCmap);
	}
	image_NotifyObservers(image, image_NEW);
	sprintf(message, "Imported image file '%.*s'", MAXPATHLEN,
		filename);
	image_SetModified(image);
	imagev_PostMenus(self, NULL);
    }
    else { /* Error ! */
	sprintf(message, "Could not Import image file '%.*s'",
		MAXPATHLEN, filename);
    }
    im_SetProcessCursor(NULL);
    message_DisplayString(self, 0, message);
}

/* Callback bound to each item on the Export menu card.  Passed in type
specifies which item (image format) was chosen */
static void
Export_Cmd( self, type )
    struct imagev *self;
    enum image_fileType type;
{
    struct image *image = self->orig;
    char filename[MAXPATHLEN + 1];
    char message[MAXPATHLEN];
    int result = 0;
    im_GetDirectory(filename);
    strcat(filename, "/");
    if(completion_GetFilename(self, "Export image file: ", filename,
			      filename, sizeof(filename), FALSE, FALSE) == -1)
	return;
    im_SetProcessCursor(waitCursor);
    if((result = image_Export(image, filename, type)) != 0) /* Error */
	sprintf(message, "Could not export image file '%.*s'",
		MAXPATHLEN, filename);
    im_SetProcessCursor(NULL);
    message_DisplayString(self, 0, message);
}

/* Callback bound to "Save As" menu item. */
static void
SaveAs( self, rock )
    struct imagev *self;
    long rock;
{
    char filename[MAXPATHLEN + 1];
    char message[MAXPATHLEN];
    int result = 0;

    im_GetDirectory(filename);
    strcat(filename, "/");
    if(completion_GetFilename(self, "Write image to file: ", filename,
			      filename, sizeof(filename), FALSE, FALSE) == -1)
	return;
    im_SetProcessCursor(waitCursor);
    result = WriteToFile(self, filename);
    im_SetProcessCursor(NULL);
    if(result == 0)
	sprintf(message, "Wrote image to file '%.*s'",
		MAXPATHLEN, filename);
    else
	sprintf(message, "Could not write image to file '%.*s'",
		MAXPATHLEN, filename);
    message_DisplayString(self, 0, message);
}

/* Save current image data to named file */
static int 
WriteToFile( self, filename )
  struct imagev  *self;
  char *filename;
{ struct image *image = self->orig;
  char realName[MAXPATHLEN], tempFilename[MAXPATHLEN];
  char *originalFilename = NULL, *endString, *basename;
  int closeCode, errorCode, originalMode, fd, counter = 1;
  FILE *outFile;
  struct stat statBuf;

  errorCode = 0;
  filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
  filename = realName;
  if((access(filename, W_OK) < 0) && (errno == EACCES))
    return(-1);
  if(stat(filename, &statBuf) >= 0)
    originalMode = statBuf.st_mode & (~S_IFMT);
  else originalMode = 0666;
#ifndef USESHORTFILENAMES
  strcpy(tempFilename,filename);
  strcat(tempFilename,".NEW");
  endString = tempFilename + strlen(tempFilename);
  while(access(tempFilename,F_OK) >= 0) /* While the file exists. */
    sprintf(endString,".%d",counter++);
#else /* USESHORTFILENAMES */
  strcpy(tempFilename,filename);
  basename = rindex(tempFilename,'/');
  if(!basename) basename = tempFilename;
  else basename++;
  if(strlen(basename) > 8) basename[8] = '\0';
  strcat(tempFilename,".NEW");
  endString = tempFilename + strlen(tempFilename);
  while(access(tempFilename,F_OK) >= 0 && counter < 10)
    sprintf(endString,".%d", counter++);
  if(counter == 10) return(-1);
#endif /* USESHORTFILENAMES */
  originalFilename = filename;
  filename = tempFilename;
  if((fd = open(filename, O_WRONLY | O_TRUNC | O_CREAT,originalMode)) < 0
     || (outFile = fdopen(fd,"w")) == NULL)
    return(-1);
  image_Write(image, outFile, im_GetWriteID(), 0);
  fflush(outFile);
  if(ferror(outFile)) {
    fclose(outFile);
    errorCode = 0;
    closeCode = -1;
  }
  else {
#ifdef AFS_ENV
    if((closeCode = vclose(fileno(outFile))) < 0) /* stdio can trash errno. */
      errorCode = errno; /* Protect it from the fclose below. */
    else if(originalFilename != NULL)
      if((closeCode = rename(filename, originalFilename)) < 0)
        errorCode = errno;
#else /* AFS_ENV */
    if((closeCode = close(fileno(outFile))) < 0) /* stdio can trash errno. */
      errorCode = errno; /* Protect it from the fclose below. */
    else if(originalFilename != NULL)
      if((closeCode = rename(filename, originalFilename)) < 0)
        errorCode = errno;
#endif /* AFS_ENV */
    fclose(outFile); /* Free stdio resources. */
    if(closeCode >= 0) { /* Reset readonly mode. */
      struct attributes attributes;

      attributes.next = NULL;
      attributes.key = "readonly";
      if(access(filename,W_OK) == -1 && errno == EACCES)
        attributes.value.integer = TRUE;
      else attributes.value.integer = FALSE;
      image_SetAttributes(image, &attributes);
      self->image_modified = image_GetModified(image);
    }
  }
  sprintf(tempFilename,"%s.CKP",filename);
  if(access(tempFilename,F_OK) >= 0)
    unlink(tempFilename);
  errno = errorCode;
  return(closeCode);
}

/* Callback bound to proctable entry "imagev-dither."  Operates on original data
and possibly scaled version. */
static void
Dither( self )
    struct imagev *self;
{
    struct image *image = self->orig;

    im_SetProcessCursor(waitCursor);
    image_Dither(image);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_Dither(self->scaled);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-halftone."  Operates on original data
and possibly scaled version. */
static void
Halftone( self )
    struct imagev *self;
{
    struct image *image = self->orig;

    im_SetProcessCursor(waitCursor);
    image_Halftone(image);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_Halftone(self->scaled);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-reduce."  Operates on original data
and possibly scaled version. */
static void
Reduce( self )
    struct imagev *self;
{
    struct image *image = self->orig;
    char *prompt = "Reduce to how many colors?", answer[MAXPATHLEN];
    int reduction = 0;

    if(message_AskForString(self, 0, prompt, NULL, answer,
			    sizeof(answer)) == -1)
	return;
    reduction = atoi(answer);
    im_SetProcessCursor(waitCursor);
    image_Reduce(image, reduction);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled){
	image_Reduce(self->scaled, reduction);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-gray."  Operates on original data
and possibly scaled version. */
static void
Gray( self )
    struct imagev *self;
{
    struct image *image = self->orig;

    im_SetProcessCursor(waitCursor);
    image_Gray(image);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_Gray(self->scaled);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-normalize."  Operates on original data
and possibly scaled version. */
static void
Normalize( self )
    struct imagev *self;
{
    struct image *image = self->orig;

    im_SetProcessCursor(waitCursor);
    image_Normalize(image);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_Normalize(self->scaled);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-brighten."  Operates on original data
and possibly scaled version. */
static void
Brighten( self )
    struct imagev *self;
{
    struct image *image = self->orig;
    char *prompt = "Brighten percent?", answer[MAXPATHLEN];
    unsigned int brighten = 0;

    if(message_AskForString(self, 0, prompt, NULL, answer,
			    sizeof(answer)) == -1)
	return;
    brighten = (unsigned int) abs(atoi(answer));
    im_SetProcessCursor(waitCursor);
    image_Brighten(image, brighten);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_Brighten(self->scaled, brighten);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-gamma-correct."  Operates on
original data and possibly scaled version. */
static void
GammaCorrect( self )
    struct imagev *self;
{
    struct image *image = self->orig;
    char *prompt = "Gamma level?", answer[MAXPATHLEN];
    float gamma = 0;

    if(message_AskForString(self, 0, prompt, NULL, answer,
			    sizeof(answer)) == -1)
	return;
    gamma = atof(answer);
    im_SetProcessCursor(waitCursor);
    image_GammaCorrect(image, gamma);
    image_SetModified(image);
    image->inited = FALSE;
    if(self->scaled) {
	image_GammaCorrect(self->scaled, gamma);
	image_SetModified(self->scaled);
	self->scaled->inited = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Callback bound to proctable entry "imagev-scale-to-fit."  If there's
a scaled version, it's destroyed and a new scaled version is created. */
static void
ScaleToFit( self )
    struct imagev *self;
{
    struct image *image = self->orig, *zoomed;
    struct rectangle r;
    float imageWidth, imageHeight;
    float screenWidth, screenHeight;
    unsigned int w, h;

    imagev_GetLogicalBounds(self, &r);
    INSETRECT(&r, self->bordersize, self->bordersize);
    im_SetProcessCursor(waitCursor);
    imageWidth = (float) image_Width(image);
    imageHeight = (float) image_Height(image);
    screenWidth = (float) rectangle_Width(&r);
    screenHeight = (float) rectangle_Height(&r);
    w = (unsigned int)((screenWidth * 100)/imageWidth);
    h = (unsigned int)((screenHeight * 100)/imageHeight);
    self->orig = image_Zoom(image, w, h);
    image_Destroy(image);
    image_SetModified(self->orig);
    self->orig->inited = FALSE;
    if(self->scaled) {
	image_Destroy(self->scaled);
	self->scaled = FALSE;
    }
    im_SetProcessCursor(NULL);
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Print the image as Postscript to the passed file descriptor */
void
imagev__Print( self, f, process, final, toplevel )
    struct imagev *self;
    FILE *f;
    char *process;
    char *final;
    int toplevel;
{
    FILE *tmpFile;
    char tmpName[MAXPATHLEN];
    strcpy(tmpName, tmpnam(NULL));
    if(tmpFile = fopen(tmpName, "w")) {
	int wpts, hpts;
	char buf[BUFSIZ], buf1[BUFSIZ];
	char *prefix;
	writePS(self, tmpFile, &wpts, &hpts, toplevel);
	fclose(tmpFile);
	if(tmpFile = fopen(tmpName, "r")) {
	    if(strcmp(process, "troff") == 0) {
		if(toplevel)
		    texttroff_BeginDoc(f);
		texttroff_BeginPS(f, wpts, hpts);
		prefix = "\\!  ";
	    }
	    else if(strcmp(final, "troff") == 0)
		prefix = "\\!  ";
	    else prefix = "";
	    while(fgets(buf, sizeof(buf), tmpFile)) {
		sprintf(buf1, "%s%s", prefix, buf);
		fputs(buf1, f);
	    }
	    fclose(tmpFile); unlink(tmpName);
	    if (strcmp(process, "troff") == 0) {
		texttroff_EndPS(f, wpts, hpts);
		if (toplevel)
		    texttroff_EndDoc(f);
	    }
	}
    }
}

/* This routine not yet used.  In future will display colormap for current
image and allow for the editing of its entries. */
static void
InfoCmd( self )
    struct imagev *self;
{
    struct im *im = im_Create(NULL);
    struct frame *frame = frame_New();
    struct colormap **cmap = imagev_CurrentColormap(self);
    struct buffer *buffer = buffer_Create("Current Colormap",
					  NULL, NULL, *cmap);
    if(im && frame && buffer) {
	frame_SetCommandEnable(frame, TRUE);
	im_SetInheritedColormap(im, cmap);
	im_SetView(im, frame);
	frame_PostDefaultHandler(frame,"message", frame_WantHandler(frame,"message"));
	frame_SetBuffer(frame, buffer, TRUE);
    }
    else {
	if(im) im_Destroy(im);
	if(frame) frame_Destroy(frame);
	if(buffer) buffer_Destroy(buffer);
    }
}

static void
ShowTrue( self )
    struct imagev *self;
{
    self->privateCmap = im_CreateColormap(imagev_GetIM(self));
    imagev_WantColormap(self, self, &self->privateCmap);
    imagev_WantUpdate(self, self);
}

void
imagev__ReceiveColormap( self, cmap )
    struct imagev *self;
    struct colormap *cmap;
{
    struct image *image = IMAGE(self);

    colormap_Clear(cmap);
    colormap_SetSize(cmap, image_RGBSize(image));
    image->inited = FALSE;
    self->do_fullupdate = TRUE;
    self->havePrivateCmap = TRUE;
    imagev_WantUpdate(self, self);
    imagev_PostMenus(self, NULL);
}

static void
ShowFixed( self )
    struct imagev *self;
{

    imagev_WantColormap(self, self , NULL);
    imagev_WantUpdate(self, self);
    if(self->privateCmap) {
	colormap_Destroy(self->privateCmap);
	self->privateCmap=NULL;
    }
}

void
imagev__LoseColormap( self, cmap )
    struct imagev *self;
    struct colormap *cmap;
{
    struct image *image = IMAGE(self);
    struct graphic *g = imagev_GetDrawable(self);
    int size = colormap_Size(cmap), i;
    struct color *c;
#if 0
/* Need to remove the color from the imagev's graphic observable list */

    if(size > 0) {
	for(i = 0; i < size; i++) {
	    c = colormap_NthColor(cmap, i);
	    graphic_RemoveObserver(g, c);
	}
    }
#endif
    image->inited = FALSE;
    self->do_fullupdate = TRUE;
    self->havePrivateCmap = FALSE;
    imagev_WantUpdate(self, self);
    imagev_PostMenus(self, NULL);
}

/* Write out current image data as Postscript.  See ps.c for implementation
of writePS that was ripped off from xv */
static void
Write_Postscript( self )
    struct imagev *self;
{
    struct image *image = self->orig;
    char filename[MAXPATHLEN + 1];
    char message[MAXPATHLEN];
    int result = 0;
    FILE *f;
    
    im_GetDirectory(filename);
    strcat(filename, "/");
    if(completion_GetFilename(self, "Postscript file: ", filename,
			      filename, sizeof(filename), FALSE, FALSE) == -1)
	return;
    im_SetProcessCursor(waitCursor);
    if(f = fopen(filename, "w")) {
	writePS(self, f, NULL, NULL, PURE_POSTSCRIPT);
	fclose(f);
	im_SetProcessCursor(NULL);
	sprintf(message, "Wrote Postscript file %s.\n", filename);
    }
    else {
	im_SetProcessCursor(NULL);
	sprintf(message, "Couldn't open %s for writing.\n", filename);
    }
    message_DisplayString(self, 0, message);
}

/* Create a scrollbar or other scroll class for this view */
struct view *
imagev__GetApplicationLayer( self )
    struct imagev *self;
{
    struct scroll *view;

    view = scroll_CreateScroller(self, scroll_LEFT | scroll_BOTTOM,
				 environ_GetProfile("imagescrollclass"));

    self->embedded = FALSE;
    self->bordersize = 0;
    return (struct view *) view;
}

/* Scroll interface */

static void y_getinfo(), y_setframe(), x_getinfo(), x_setframe();
static long y_whatisat(), x_whatisat();

static struct scrollfns vertical_scroll_interface = {
    y_getinfo,
    y_setframe,
    NULL,
    y_whatisat
};

static struct scrollfns horizontal_scroll_interface = {
    x_getinfo,
    x_setframe,
    NULL,
    x_whatisat
};

struct scrollfns *
imagev__GetInterface( self, interface_name )
    struct imagev *self;
    char *interface_name;
{
    struct scrollfns *interface = NULL;
    
    if (strcmp(interface_name, "scroll,vertical") == 0)
	interface = &vertical_scroll_interface;
    else if (strcmp(interface_name, "scroll,horizontal") == 0)
	interface = &horizontal_scroll_interface;
    return(interface);
}

static void
x_getinfo(self, total, seen, dot)
    struct imagev *self;
    struct range *total, *seen, *dot;
{
    struct rectangle *imageRect = imagev_GetImageRect(self);
    struct rectangle visualRect;
    struct rectangle *selectRect = NULL;

    if (rectangle_IsEmptyRect(imageRect)) {
	total->beg = 0;
	total->end = 1;
	seen->beg = -1;
	seen->end = -1;
	dot->beg = -1;
	dot->end = -1;
    }
    else {
	total->beg = 0;
	total->end = imageRect->width;

	imagev_GetVisualBounds(self, &visualRect);
	seen->beg = imagev_ToImageX(self, visualRect.left) -
	  imageRect->left;
	seen->end = imagev_ToImageX(self, visualRect.left + visualRect.width) -
	  imageRect->left;

	if (seen->beg < total->beg)
	    seen->beg = total->beg;
	if (seen->end > total->end)
	    seen->end = total->end;

	if (selectRect && !rectangle_IsEmptyRect(selectRect)) {
	    dot->beg = selectRect->left - imageRect->left;
	    dot->end = selectRect->left + selectRect->width - imageRect->left;

	    if (dot->beg < total->beg)
		dot->beg = total->beg;
	    if (dot->end > total->end)
		dot->end = total->end;
	}
	else {
	    dot->beg = -1;
	    dot->end = -1;
	}
    }
}

static long
x_whatisat(self, coordinate, outof)
    struct imagev *self;
    long coordinate, outof;
{
    struct rectangle *imageRect = imagev_GetImageRect(self);

    return imagev_ToImageX(self, coordinate) - imageRect->left;
}

static void
x_setframe(self, position, coordinate, outof) 
    struct imagev *self;
    int position;
    long coordinate, outof;
{
    long diffpos;
    struct rectangle *imageRect = imagev_GetImageRect(self);

    if( !rectangle_IsEmptyRect(imageRect) ) {
	diffpos = (imagev_ToImageX(self, coordinate) - imageRect->left) -
	  position;

	if (diffpos) {
	    self->panx -= diffpos;
	    self->do_renderupdate = TRUE;
	    imagev_WantUpdate(self, self);
	}
    }
}

static void
y_getinfo(self, total, seen, dot)
    struct imagev *self;
    struct range *total, *seen, *dot;
{
    struct rectangle *imageRect = imagev_GetImageRect(self);
    struct rectangle visualRect;
    struct rectangle *selectRect = NULL;

    if (rectangle_IsEmptyRect(imageRect)) {
	total->beg = 0;
	total->end = 1;
	seen->beg = -1;
	seen->end = -1;
	dot->beg = -1;
	dot->end = -1;
    }
    else {
	total->beg = 0;
	total->end = imageRect->height;

	imagev_GetVisualBounds(self, &visualRect);
	seen->beg = imagev_ToImageY(self, visualRect.top) -
	  imageRect->top;

	seen->end = imagev_ToImageY(self, visualRect.top + visualRect.height) -
	  imageRect->top;

	if (seen->beg < total->beg)
	    seen->beg = total->beg;
	if (seen->end > total->end)
	    seen->end = total->end;

	if (selectRect && !rectangle_IsEmptyRect(selectRect)) {
	    dot->beg = selectRect->top - imageRect->top;
	    dot->end = selectRect->top + selectRect->height -
	      imageRect->top;

	    if (dot->beg < total->beg)
		dot->beg = total->beg;
	    if (dot->end > total->end)
		dot->end = total->end;
	}
	else {
	    dot->beg = -1;
	    dot->end = -1;
	}
    }
}

static long
y_whatisat(self, coordinate, outof)
    struct imagev *self;
    long coordinate, outof;
{
    struct rectangle *imageRect = imagev_GetImageRect(self);

    return imagev_ToImageY(self, coordinate) - imageRect->top;
}

static void
y_setframe(self, position, coordinate, outof) 
    struct imagev *self;
    int position;
    long coordinate, outof;
{
    long diffpos;
    struct rectangle *imageRect = imagev_GetImageRect(self);

    if( !rectangle_IsEmptyRect(imageRect) ) {
	diffpos = imagev_ToImageY(self, coordinate) - imageRect->top -
	  position;

	if (diffpos) {
	    self->pany -= diffpos;
	    self->do_renderupdate = TRUE;
	    imagev_WantUpdate(self, self);
	}
    }
}

/* End Scroll Interface */

/* We are being removed from the view tree */
void
imagev__UnlinkTree( self )
    struct imagev *self;
{
    self->onScreen = self->isLinked = FALSE;
    rectangle_EmptyRect(&self->imagerect);
    super_UnlinkTree(self);
}

/* We're about to be removed from the view tree */
void
imagev__UnlinkNotification( self, unlinkedTree )
    struct imagev *self;
    struct view *unlinkedTree;
{
    self->onScreen = self->isLinked = FALSE;
    super_UnlinkNotification(self, unlinkedTree);
}

/* We're being placed into the view tree.  Set imagerect to something
reasonable. */
void
imagev__LinkTree( self, parent )
    struct imagev *self;
    struct view *parent;
{
    super_LinkTree(self, parent);
    if(parent && imagev_GetIM(self)) {
	struct image *image = IMAGE(self);
	if(image_Data(image)) {
	    long ix, iy, iw, ih;
	    ix = imagev_ToDefImageX(self, 0);
	    iy = imagev_ToDefImageY(self, 0);
	    iw = imagev_ToDefImageW(self, image_Width(image));
	    ih = imagev_ToDefImageH(self, image_Height(image));
	    rectangle_SetRectSize(&self->imagerect, ix, iy, iw, ih);
	}
	else {
	    rectangle_EmptyRect(&self->imagerect);
	}
    }
    else {
	self->isLinked = self->onScreen = FALSE;
	rectangle_EmptyRect(&self->imagerect);
    }
}

/* Read a native ATK image from file and view it */
static void
ReadCmd( self )
    struct imagev *self;
{
    struct image *image = self->orig;
    char filename[MAXPATHLEN + 1];
    char message[MAXPATHLEN];
    int result = 0;
    FILE *in;

    im_GetDirectory(filename);
    strcat(filename, "/");
    if(completion_GetFilename(self, "Image file: ", filename,
			      filename, sizeof(filename), FALSE, TRUE) == -1)
	return;
    im_SetProcessCursor(waitCursor);
    if(in = fopen(filename, "r")) { /* OK */
       long status;
       struct image *new = image_New();
       if(new) {
	   if((status = image_Read(new, in, 0)) == dataobject_NOREADERROR) {
	       image_Reset(image);
	       image_Duplicate(new, image);
	       if(self->scaled) {
		   image_Destroy(self->scaled);
		   self->scaled = NULL;
	       }
	       sprintf(message, "Read image file '%.*s'", MAXPATHLEN,
		       filename);
	       image_NotifyObservers(image, image_NEW);
	       image_SetModified(image);
	       imagev_PostMenus(self, NULL);
	   }
	   else {
	       switch(status) {
		   case dataobject_NOTATKDATASTREAM:
		       sprintf(message, "Contents of '%.*s' not ATK image data",
			       MAXPATHLEN, filename);
		       break;
		   case dataobject_PREMATUREEOF:
		       sprintf(message, "Contents of '%.*s' truncated", MAXPATHLEN, filename);
		       break;
		   case dataobject_OBJECTCREATIONFAILED:
		       sprintf(message, "Failed to create a necessary object");
		       break;
		   case dataobject_BADFORMAT:
		       sprintf(message, "Contents of '%.*s' in bad format",
			       MAXPATHLEN, filename);
		       break;
		   default:
		       sprintf(message,
			       "Unknown error encountered while reading '%.*s'",
			       MAXPATHLEN, filename);
		       break;
	       }
	   }
	   image_Destroy(new);
       }
       else { /* Error ! */
	sprintf(message, "Couldn't allocate memory for new image.");
       }
    }
    else { /* Error ! */
     sprintf(message, "Could not open image file '%.*s'",
	     MAXPATHLEN, filename);
    }
    im_SetProcessCursor(NULL);
    message_DisplayString(self, 0, message);
}

static void InternalZoomCmd(self, rock)
struct imagev *self;
long rock;
{
    long newscale, fact;
    long midx, midy, offx, offy;
    struct rectangle logicalRect;
    imagev_GetLogicalBounds(self, &logicalRect);
    midx = imagev_ToImageX(self, logicalRect.left + logicalRect.width/2);
    midy = imagev_ToImageY(self, logicalRect.top + logicalRect.height/2);

    if(rock < 0) 
	newscale = self->scale / 2;
    else if(rock > 0)
	newscale = self->scale * 2;
    else newscale = imagev_NormScale;

    if(newscale < 1) newscale = 1;
    else if (newscale > 1024) newscale = 1024;

    if(newscale != self->scale) {
	if(self->scaled) {
	    image_Destroy(self->scaled);
	    self->scaled = NULL;
	}
	if(newscale != imagev_NormScale)  {
	    fact = (newscale * 100)/imagev_NormScale;
	    self->scaled = image_Zoom(self->orig, fact, fact);
	   /* super_SetDataObject(self, (struct dataobject *) self->scaled); */
	    self->scaled->inited = FALSE;
	}
	else {
	    /* super_SetDataObject(self, (struct dataobject *) self->orig); */
	    if(self->scaled==NULL) self->scaled=image_New();
	    if(self->scaled) {
		image_Reset(self->scaled);
		image_Duplicate(self->orig, self->scaled);
		self->scaled->inited=FALSE;
	    }
	    self->orig->inited = FALSE;
	}
	offx = imagev_ToImageX(self, logicalRect.left + logicalRect.width/2);
	offy = imagev_ToImageY(self, logicalRect.top + logicalRect.height/2);
	self->panx += (offx - midx);
	self->pany += (offy - midy);

	self->scale = newscale;
	self->do_renderupdate = TRUE;
    }
}

/* Bind us to passed in image */
void
imagev__SetDataObject( self, image )
struct imagev *self;
struct image *image;
{
    self->orig = image;
    super_SetDataObject(self, (struct dataobject *) image);
    self->originx = image_GetOriginX(image);
    self->originy = image_GetOriginY(image);
    self->panx = self->originx;
    self->pany = self->originy;
    self->scale=imagev_NormScale;
    rectangle_EmptyRect(&self->imagerect);
    if(self->scaled==NULL) self->scaled=image_New();
    if(self->scaled) {
	image_Reset(self->scaled);
	image_Duplicate(self->orig, self->scaled);
	self->scaled->inited=FALSE;
    }
    self->orig->inited = FALSE;
}
    
/* Callback bound to "imagev-zoom-{in,out,normal}" proctable entries.  The passed
 in rock determines which of {in, out, normal} is to be performed:

   -1: scale down by half
    0: go to normal scale
   +1: scale up by half
*/
static void
ChangeZoomCmd( self, rock )
    struct imagev *self;
    long rock;
{
    if (rock == 0 && self->scale == imagev_NormScale)
	return;
    im_SetProcessCursor(waitCursor);
    InternalZoomCmd(self, rock);
    imagev_WantUpdate(self, self);
    im_SetProcessCursor(NULL);
}

/* Callback bound to "imagev-pan-to-origin."  Upper-left of image
is set to upper-left of our screen region. */
static void
PanToOriginCmd( self, rock )
    struct imagev *self;
    long rock;
{
    if (self->panx == self->originx &&
	self->pany == self->originy)
	return;

    self->panx = self->originx;
    self->pany = self->originy;
    self->do_renderupdate = TRUE;
    imagev_WantUpdate(self, self);
}

/* Post our keystate up the view tree to parent */
void
imagev__PostKeyState(self, ks)
    struct imagev *self;
    struct keystate *ks;
{
    struct keystate *new;
    new = keystate_AddAfter(self->keystate, ks);
    super_PostKeyState(self, new);
}

/* convert a rectangle in image coords to pix coords. */
static void RectToPix(self, dest, src)
    struct imagev *self;
    struct rectangle *dest, *src;
{
    if (rectangle_IsEmptyRect(src)) {
	rectangle_EmptyRect(dest);
	return;
    }
    rectangle_SetRectSize(dest,
			      imagev_ToPixX(self, rectangle_Left(src)),
			      imagev_ToPixY(self, rectangle_Top(src)), 
			      imagev_ToPixW(self, rectangle_Width(src)), 
			      imagev_ToPixH(self, rectangle_Height(src)));
}

/* SaveFormat and JPEG SaveQuality only apply to self->orig. Save quality
is an integer between 5-95 and determines how much information JPEG
compressor will save. */
static void
SetSaveQuality( self, rock )
    struct imagev *self;
    long rock;
{   struct image *image = self->orig;
    char *prompt = "Save quality [5-95]: ", answer[MAXPATHLEN];
    int q;

    sprintf(answer, "%d", image_GetJPEGSaveQuality(image));
    if(message_AskForString(self, 0, prompt, answer,
			    answer, sizeof(answer)) == -1)
	return;
    q = atoi(answer);
    image_SetJPEGSaveQuality(image, q);
    sprintf(answer, "Save quality set to %d", q);
    message_DisplayString(self, 0, answer);
}

/* Callback bound to proctable entry "imagev-set-save-format."  Prompt for
either JPEG or GIF.  Additional saveformats may be added later.  ATK image
datastream format is a base64-enoded, mail-safe JPEG or GIF. */
static void
SetSaveFormat( self, rock )
    struct imagev *self;
    long rock;
{   struct image *image = self->orig;
    char *prompt = "New Image Save-Format: ", response[100];
    static char *choices[3] = {"GIF", "JPEG", NULL};
    int result = 0;

    if(message_MultipleChoiceQuestion(self, 100, prompt, 0,
				      &result, choices, NULL) == -1)
	return;
    image_SetSaveFormatString(image, choices[result]);    
    sprintf(response, "Save-format set to %s", choices[result]);
    message_DisplayString(self, 0, response);
    imagev_PostMenus(self, NULL);
}
