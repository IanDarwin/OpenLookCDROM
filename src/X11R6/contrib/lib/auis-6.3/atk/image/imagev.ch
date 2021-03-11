/* imagev.ch - class description for view on images */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

#include <rect.h>
 
enum image_fileType {
  faces_imageType,			/* Faces Project */
  pbm_imageType,			/* Portable Bit Map (PBM, PGM, PPM) */
  sunraster_imageType,			/* Sun Rasterfile */
  gif_imageType,			/* GIF Image */
  rle_imageType,			/* Utah RLE Image */
  xwd_imageType,			/* X Window Dump */
  xbitmap_imageType,			/* X Bitmap */
  xpixmap_imageType,			/* X Pixmap */
  g3_imageType,				/* G3 FAX Image */
  fbm_imageType,			/* FBM Image */
  pcx_imageType,			/* PC Paintbrush Image */
  img_imageType,			/* GEM Bit Image */
  mac_imageType,			/* MacPaint Image */
  cmuwm_imageType,			/* CMU WM Raster */
  jpeg_imageType,			/* JPEG */
  mcidas_imageType,			/* McIDAS areafile */
  tif_imageType				/* Tagged Image File Format Image */
};

#define imagev_Grain (64)
#define imagev_NormScale (8)
#define imagev_ImagesPerPix (imagev_Grain/imagev_NormScale)
/* image-units per pixel at normal scale */

class imagev : view {
classprocedures:
  InitializeClass() returns boolean;
  InitializeObject( struct imagev *self ) returns boolean;
  FinalizeObject( struct imagev *self ) returns void;

overrides:
  Hit( enum view_MouseAction action, long x, long y, long numberOfClicks ) returns struct view *;
  DesiredSize( long width, long height, enum view_DSpass pass, long *dWidth, long *dheight ) returns enum view_DSattributes;
  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  PostMenus( struct menulist *menulist ) returns void;
  PostKeyState( struct keystate *ks ) returns void;
  ObservedChanged( struct observable *changed, long value ) returns void;
  Update() returns void;
  ReceiveInputFocus();
  LoseInputFocus();
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );
  ReceiveColormap(struct colormap *cmap);
  LoseColormap(struct colormap *cmap);
  UnlinkNotification(struct view *unlinkedTree);
  LinkTree(struct view *parent);
  UnlinkTree();
  GetApplicationLayer() returns struct view *;
  GetInterface(char *interface_name) returns struct scrollfns *;
  SetDataObject( struct image *image);

macromethods:

  ToPixX(val)  (((((val)-(self)->panx)*(self)->scale*imagev_GetHorizontalResolution(self))/(imagev_Grain*72)) + self->bordersize)
  ToPixY(val)  (((((val)-(self)->pany)*(self)->scale*imagev_GetVerticalResolution(self))/(imagev_Grain*72)) + self->bordersize)
  ToPixW(val)  (((val)*(self)->scale*imagev_GetHorizontalResolution(self))/(imagev_Grain*72))
  ToPixH(val)  (((val)*(self)->scale*imagev_GetVerticalResolution(self))/(imagev_Grain*72))
  ToImageX(val)  ((((val)*imagev_Grain*72)/((self)->scale*imagev_GetHorizontalResolution(self)))+(self)->panx)
  ToImageY(val)  ((((val)*imagev_Grain*72)/((self)->scale*imagev_GetVerticalResolution(self)))+(self)->pany)
  ToImageW(val)   (((val)*imagev_Grain*72)/((self)->scale*imagev_GetHorizontalResolution(self)))
  ToImageH(val)   (((val)*imagev_Grain*72)/((self)->scale*imagev_GetVerticalResolution(self)))
  ToPrintPixX(val)  (((val)-(self)->originx)/imagev_ImagesPerPix)
  ToPrintPixY(val)  (((val)-(self)->originy)/imagev_ImagesPerPix)
  ToPrintPixW(val)  ((val)/imagev_ImagesPerPix)
  ToPrintPixH(val)  ((val)/imagev_ImagesPerPix)
  ToDefImageX(val)  ((((val)*imagev_ImagesPerPix*72)/(imagev_GetHorizontalResolution(self)))+(self)->originx)
  ToDefImageY(val)  ((((val)*imagev_ImagesPerPix*72)/(imagev_GetVerticalResolution(self)))+(self)->originy)
  ToDefImageW(val)   (((val)*imagev_ImagesPerPix*72)/(imagev_GetHorizontalResolution(self)))
  ToDefImageH(val)   (((val)*imagev_ImagesPerPix*72)/(imagev_GetVerticalResolution(self)))
  GetImageRect() (&(self)->imagerect)

data:
  struct image *orig, *scaled;
  struct cursor *cursor;    
  boolean cursorPosted;
  int cursorMode;
  struct menulist *menulist;
  struct keystate *keystate;
  boolean do_fullupdate, do_renderupdate;
  boolean image_modified;
  struct sbutton_prefs *buttonprefs;
  boolean haveFocus, isLinked, onScreen;
  struct rectangle shown, imagerect, *canvas;
  boolean embedded, havePrivateCmap;
  long scale;
  long panx, pany;
  long originx, originy;
  long rockx, rocky;
  long lastx, lasty;
  int bordersize;
  int panStyle;
  struct rectangle *lastPixRect;
  struct colormap *privateCmap;
};
