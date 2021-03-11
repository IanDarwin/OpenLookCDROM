/* -*-C-*-
********************************************************************************
*
* File:         w_pixmap.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_pixmap.c,v 2.8 1994/06/06 15:40:56 npm Exp $
* Description:  Interfaces to Motif's Pixmap/XImage routines.
* Author:       Niels Mayer
* Created:      Thu Sep 28 18:59:42 1989
* Modified:     Sun Jun  5 14:49:38 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_pixmap.c,v 2.8 1994/06/06 15:40:56 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "winterp.h"

extern LVAL true;		/* xlglob.c */
extern Display* display;	/* winterp.c */
extern Window	root_win;	/* winterp.c */
extern Screen*  screen;		/* winterp.c */
extern Colormap colormap;	/* winterp.c */

/* 

Destroying unused Pixmaps and XImage's:

Pixmaps are used by widgets to display graphical information. By keeping
track of the widget and the resource under which the pixmap is stored, we
can figure out when to free up pixmaps by using XmDestroyPixmap() to update
the reference count on pixmaps used in winterp and free the memory for
pixmaps that are no longer referenced. This is certainly useful and good to
do because I expect pixmaps to take up a reasonable amount of client and
server space, so we should be efficient about their use, especially because
motif has made it easy to do so. 

Furthermore, we don't want users manually calling XmDestroyPixmap() on a
pixmap that is still referenced -- this will cause a guaranteed coredump
due to an "X Protocol Error". 

Unfortunately, the XImage* behind the pixmap still seems to persist even if
all pixmaps based on a particular Ximage no longer exists. Since XImages
can be created inside XmGetPixmap(), or through a XmSttring-->Xm*Pixmap*
conversion it will be very hard to get a hold of the actual XImage pointer
when all we have access to are the name-strings of the previously installed
images or the filenames of new images installed. Either we'll have to
prevent creating cached XImages inside XmGetPixmap() and the
XmSttring-->Xm*Pixmap* converters, and do all this ourselves, or we can
just punt. I'm going to punt on this.

The alternative for handling XImages is to let the users that care about
space taken up by XImages to use
XmInstallImage(_XmGetImageFromFile(<image_file_name>)) at application
startup time, and then destroy the <XImage_obj> themselves when they're not
needed.  Destroying an XImage after creating the necessary pixmaps from it
shouldn't cause any protocol errors, so this is the thing to do.

Every time a Pixmap is accessed either through XmGetPixmap() or through
winterp's string-->pixmap converter (which ends up calling XmGetPixmap()),
the reference count on that pixmap is automatically increased. We want to
symetrically reduce the reference count (by calling XmDestroyPixmap()) for
each pixmap reference that is no longer used. When the reference count is
reduced to 0, the pixmap itself is freed. We do this by creating an atomic
type XLTYPE_Pixmap to store the X Pixmap pointer; every reference to this
pixmap will create a pixmap reference object which points to this
XLTYPE_Pixmap object. Code in this module will keep track of the pixmap
references made, and will remove any pixmap-ref-objs from v_savedobjs when
the pixmap is no longer referenced. If no more references to the
XLTYPE_Pixmap exist, then it will be garbage collected -- code inside the
garbage collector will call XmDestroyPixmap() on these unused pixmaps.

In each pixmap-reference object, we store
1) a pointer to an object of type LVAL_Pixmap which is returned by
   primitive xm_get_pixmap.
2) name of widget resource storing the pixmap (the winterp symbol for the res)
3) the widget-object of the pixmap.

We remove pixmap-ref-objs from v_savedobjs when
1) the widget which references the pixmap gets destroyed
2) the widget resource that references the pixmap gets set to a new value
3) the pixmap is no longer referenced within the lisp system. (see note below)

In case (1) we want to hash the pixmap into the widget's v_savedobjs
hashbucket. When the widget's destroy callback gets called, all the objects
in the hashbucket corresonding to that widget are destroyed (callbacks,
pixmaps, etc).

In case (2), since we use the scheme mentioned above in (1) to store
pixmap-objs, we can just search thru the hashbucket looking for the
pixmap-obj on the approprate widget and widget-resource symbol. If we
find a pixmap-obj, remove it from the list and replace it with the new one.

In case (3), when no pixmap-ref-objs pointing to a particular XLTYPE_Pixmap
exist, that XLTYPE_Pixmap will get garbage collected and it's pixmap will
be deallocated by calling XmDestroyPixmap(). Note that if XmGetPixmap is
called N times on a particular pixmap, we create N XLTYPE_Pixmap objects
corresponding to the N refcount on that Pixmap stored inside Motif. Each
time the XLTYPE_Pixmap gets gc'd, the refcount gets reduced inside Motif.

The only other way a pixmap can be referenced is via XmGetPixmap() or
:get_values on a pixmap resource, in which case it's XmDestroyPimap()
operation should only happen when that pixmap was no longer referenced
neither in lisp variables nor as pixmap resources. To do this, we create a
new XLTYPE_Pixmap for each reference.

*/


/* from xlisp/xldmem.h: typedef struct _WINTERP_GIF_Color_Info_Node *WINTERP_GIF_COLOR_INFO; */
typedef struct _WINTERP_GIF_Color_Info_Node {
  int num_allocd_colors;
  unsigned long* allocd_colors;
} WINTERP_GIF_Color_Info_Node;


/******************************************************************************
 *
 ******************************************************************************/
void Wpm_Set_Pixmap_Reference(lval_pixmap, o_widget, lval_resname)
     LVAL lval_pixmap;		/* XLTYPE_Pixmap */
     LVAL o_widget;		/* widget on which this resource got set */
     LVAL lval_resname;		/* SYMBOL */
{
  int i = Wso_Hash(o_widget);	/* note that we hash all pixmapobjs on the same widget to the same hashbucket */
  LVAL l_hbucket = getelement(v_savedobjs, i);
  LVAL obj, refobj, l_prev = NIL;

#ifdef WINTERP_DEBUG_2
  fprintf(stderr, "\nWpm_Set_Pixmap_Reference(PixmapID=%lu, WidgetID=%lu, ResnameID=%lu).\n", (unsigned long) get_pixmap(lval_pixmap), (unsigned long) o_widget, (unsigned long) lval_resname);
#endif /* WINTERP_DEBUG_2 */

  /* go thru hashbucket, stopping if hit end, or if hit match */
  while ((l_hbucket != NIL)
	 && !(((obj = car(l_hbucket)) != NIL)
	      && (ntype(obj) == XLTYPE_PIXMAP_REFOBJ)
	      && (get_pixref_widget(obj) == o_widget)
	      && (get_pixref_resname(obj) == lval_resname))) {
	 l_prev = l_hbucket;
	 l_hbucket = cdr(l_hbucket);
       }

  /* remove the previously referenced pixmapobj --> allow it to be gc'd */
  if (l_hbucket != NIL) {	/* if something matched */
#ifdef WINTERP_DEBUG_2
    fprintf(stderr, "	Removing PixmapRef w/ PixmapID=%lu, WidgetID=%lu, ResnameID=%lu.\n", (unsigned long) get_pixmap(get_pixref_pixmap(car(l_hbucket))), (unsigned long) get_pixref_widget(car(l_hbucket)), (unsigned long) get_pixref_resname(car(l_hbucket)));
#endif /* WINTERP_DEBUG_2 */
    if (l_prev == NIL)		/* first elt matched */
      setelement(v_savedobjs, i, cdr(l_hbucket)); /* remove first elt */
    else
      rplacd(l_prev, cdr(l_hbucket)); /* remove elt pointed to by l_hbucket */
  }
#ifdef WINTERP_DEBUG_2
  else
    fprintf(stderr, "	Didn't remove any previous PixmapRef's\n");
#endif /* WINTERP_DEBUG_2 */

  /* save a new reference pixmap-obj under o_widget and lval_resname. */
  xlstkcheck(3);
  xlprotect(lval_pixmap);
  xlsave(l_hbucket);
  xlsave(refobj);

  refobj = new_pixrefobj();	/* create a "reference object" */
  set_pixref_pixmap(refobj, lval_pixmap); /* point it to the pixmap it references */
  set_pixref_widget(refobj, o_widget); /* set the widget referencing the pixmap */
  set_pixref_resname(refobj, lval_resname); /* set the resource name on that widget */
  l_hbucket = cons(refobj, getelement(v_savedobjs, i));	/* add reference obj to hashbucket */
  setelement(v_savedobjs, i, l_hbucket); /* store it in in v_savedobjs so it won't get gc'd */
  xlpopn(3);
}


/******************************************************************************
 * This routine is called by the garbage collector on any unreferenced pixmap
 * nodes. Note that XmDestroyPixmap() gets called on Pixmaps created by
 * was created by calling primitive XM_GET_PIXMAP or via an indirect XtConvert()
 * call in Wres_Append_LispArglist_To_XtArglist() (which also calls XmGetPixmap())
 * This is done because we must ensure that we only call XmDestroyPixmap()
 * once per invocation of XmGetPixmap() lest a pixmap be deallocated while it
 * is still ref'd inside Motif.
 ******************************************************************************/
void Wpm_Decr_Refcount_Or_Free_Pixmap(p)
     LVAL p;			/* XLTYPE_Pixmap */
{
  WINTERP_GIF_COLOR_INFO color_info;

#ifdef WINTERP_DEBUG_2
  fprintf(stderr, "\nWpm_Decr_Refcount_Or_Free_Pixmap(PixmapID=%lu).\n", (unsigned long) get_pixmap(p));
#endif /* WINTERP_DEBUG_2 */

  /* check to see if Pixmap was allocated by Wpm_Prim_GIF_TO_PIXMAP() */
  if ((color_info = get_pixmap_color_info(p)) != (WINTERP_GIF_COLOR_INFO) NULL) { 
    XFreePixmap(display, get_pixmap(p)); /* if it is, free it w/ XFreePixmap(), since alloc'd w/ XCreatePixmap() */
#ifdef WINTERP_DEBUG_2
    fprintf(stdout, "Freeing %d colors\n", color_info->num_allocd_colors);
    fflush(stdout);
#endif /* WINTERP_DEBUG_2 */
    if (color_info->num_allocd_colors > 0)
      XFreeColors(display, colormap, color_info->allocd_colors, color_info->num_allocd_colors, 0L);
    XtFree((char*) color_info->allocd_colors);
    XtFree((char*) color_info);
  }
  else {
    if (!XmDestroyPixmap(screen, get_pixmap(p)))
      xlerror("Internal error in garbage collecting a pixmap -- XmDestroyPixmap() couldn't find pixmap in pixmap cache.",
	      p);
  }
}


/******************************************************************************
 * (XM_GET_PIXMAP <image-name> <foreground> <background>)
 *
 * <image-name> is a string representing a XImage that has been
 * cached via XmInstallImage(). If such an XImage isn't found, then
 * <image-name> is treated as the filename for a X10 or X11 bitmap file.
 * You need to give a full pathname to the bitmap file, or alternately,
 * you may set the environment variable XBMLANGPATH to the directories to
 * be searched for bitmap files. XBMLANGPATH defaults to
 * /usr/lib/X11/%L/bitmaps/%N/%B:/usr/lib/X11/%L/bitmaps/%B:/usr/lib/X11/bitmaps/%B:/usr/include/X11/bitmaps/%B
 *
 * In additions to images you have installed, Motif features 10 preinstalled
 * images: "background", "25_foreground", "50_foreground", * "75_foreground",
 * "vertical", "horizontal", "slant_right", "slant_left", "menu_cascade", "menu_checkmark".
 *
 * <forground> and <background> may be strings repreenting color names, or
 * values of type XLTYPE_Pixel. (generated by X_ALLOC_COLOR, or
 * via widget-method :get_values :XMN_FOREGROUND or :XMN_BACKGROUND.
 *
 * This function returns a value of type XLTYPE_Pixmap, which is suitable for
 * passing on to any Pixmap-valued widget resource so as to display an image
 * inside a widget.
 *
 * Note that there is no interface to XmDestroyPixmap()-- unreferenced pixmaps
 * are automatically destroyed by winterp during garbage collection.
 ******************************************************************************/
LVAL Wpm_Prim_XM_GET_PIXMAP()
{
  XColor        screenColor;
  LVAL          str_image_name, lval_foreground, lval_background;
  Pixel         foreground, background;
  Pixmap        pixmap;

  str_image_name = xlgastring();

  switch (ntype(lval_foreground = xlgetarg())) {
  case STRING:
    if (!XParseColor(display, colormap, getstring(lval_foreground), &screenColor))
      xlerror("XParseColor() couldn't parse <foreground> color specification.", lval_foreground);
    if (!XAllocColor(display, colormap, &screenColor))
      xlerror("XAllocColor() couldn't allocate specified <foreground> color.", lval_foreground);
    foreground = screenColor.pixel;
    break;
  case XLTYPE_Pixel:
    foreground = get_xpixel(lval_foreground);
    break;
  default:
    xlerror("Bad type for <foreground> argument. Expected either a STRING or a PIXEL value.", lval_foreground);
    break;
  }

  switch (ntype(lval_background = xlgetarg())) {
  case STRING:
    if (!XParseColor(display, colormap, getstring(lval_background), &screenColor))
      xlerror("XParseColor() couldn't parse <background> color specification.", lval_background);
    if (!XAllocColor(display, colormap, &screenColor))
      xlerror("XAllocColor() couldn't allocate specified <background> color.", lval_background);
    background = screenColor.pixel;
    break;
  case XLTYPE_Pixel:
    background = get_xpixel(lval_background);
    break;
  default:
    xlerror("Bad type for <background> argument. Expected either a STRING or a PIXEL value.", lval_background);
    break;
  }

  xllastarg();

  pixmap = XmGetPixmap(screen, getstring(str_image_name), foreground, background);
  if (pixmap == XmUNSPECIFIED_PIXMAP)
    xlerror("XmGetPixmap() couldn't create a pixmap from given specification.", str_image_name);

#ifdef WINTERP_DEBUG_2
  fprintf(stderr, "\nXmGetPixmap() returned PixmapID=%lu.\n", (unsigned long) pixmap);
#endif /* WINTERP_DEBUG_2 */

  return (cv_pixmap(pixmap));
}


/******************************************************************************
 * (xm_install_image <ximage> <image_name>)
 * where <ximage> is an XImage-type object as returned by XM_GET_XIMAGE_FROM_FILE
 * <image_name> is a string, the name under which the XImage is cached
 *
 * returns T if success,  NIL if a NULL <ximage>, or duplicate <image_name>
 * is given.
 ******************************************************************************/
LVAL Wpm_Prim_XM_INSTALL_IMAGE()
{
#ifndef WINTERP_MOTIF_11	/* In 1.0 only, this is missing from Xm.h */
  extern Boolean XmInstallImage(); /* lib/Xm/ImageCache.c */
#endif				/* WINTERP_MOTIF_11 */
  LVAL lval_image;
  XImage* image;
  char*   name;

  lval_image = xlga_ximage();
  image = get_ximage(lval_image);
  if (image == (XImage*) NULL)
    xlerror("XImage has been deallocated by XM_UNINSTALL_IMAGE...", lval_image);
  name = getstring(xlgastring());
  xllastarg();

  if (XmInstallImage(image, name)) /* XmInstallImage() makes a local copy of <name>, the <name> argument can thus be safely freed via garbage collection */
    return (true);
  else
    return (NIL);
}


/******************************************************************************
 * (xm_uninstall_image <ximage>)
 * where <ximage> is an XImage-type object as returned by XM_GET_XIMAGE_FROM_FILE
 *
 * returns T if success,  NIL if a NULL <ximage>, or <ximage> cannot be
 * found in the image cache.
 *
 * In either case, the XImage and it's associated image data
 * (ximage->data) is deallocated via XDestroyImage(). The <ximage> is
 * marked as being destroyed, such that other attempts to access the
 * node within WINTERP will result in an error being signalled. Note that
 * calling XDestroyImage() isn't done in C-Motif's XmUninstallImage() routine,
 * but it makes sense to do in WINTERP.
 ******************************************************************************/
LVAL Wpm_Prim_XM_UNINSTALL_IMAGE()
{
#ifndef WINTERP_MOTIF_11	/* In 1.0 only, this is missing from Xm.h */
  extern Boolean XmUninstallImage(); /* lib/Xm/ImageCache.c */
#endif /* WINTERP_MOTIF_11 */
  Boolean xm_uninstall_success_p;
  LVAL lval_image;
  XImage* image;
  char*   name;

  lval_image = xlga_ximage();
  image = get_ximage(lval_image);
  if (image == (XImage*) NULL)
    xlerror("XImage has been deallocated by XM_UNINSTALL_IMAGE...", lval_image);
  xllastarg();

  xm_uninstall_success_p = XmUninstallImage(image);

  XDestroyImage(image);		/* Destroy the XImage 'ximage' and the image data ximage->data created by, for example, XM_GET_XIMAGE_FROM_FILE) */

  set_ximage(lval_image, (XImage*) NULL);
  
  if (xm_uninstall_success_p)
    return (true);
  else
    return (NIL);
}


/******************************************************************************
 * (xm_get_ximage_from_file <filepath>)
 * where <filepath> is the full name of the file containing the XImage data.
 *
 * returns an XImage object if an XImage was successfully retrieved from the
 * given file; NIL if failure.
 ******************************************************************************/
LVAL Wpm_Prim_XM_GET_IMAGE_FROM_FILE()
{
  XImage* image;
#ifdef _NO_PROTO		/*<Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
  extern XImage* _XmGetImageFromFile(); /* from <Xm/XmP.h>, def'd in lib/Xm/ReadImage.c */
#else  /* !defined(_NO_PROTO) ==> ANSI... */
  extern XImage* _XmGetImageFromFile(char *filename); /* from <Xm/XmP.h>, def'd in lib/Xm/ReadImage.c */
#endif /* _NO_PROTO */

  char* filename = getstring(xlgastring());
  xllastarg();
  if (image = _XmGetImageFromFile(filename))
    return (cv_ximage(image));
  else
    return (NIL);
}


#ifdef WINTERP_XTANGO_WIDGET
#include "widgets/xgif.h"

/******************************************************************************
 * (GIF_TO_PIXMAP <gif-file-name-str> [:verbose|:noverbose])
 *	==> returns a PIXMAP node...
 ******************************************************************************/
LVAL Wpm_Prim_GIF_TO_PIXMAP()
{
  extern LVAL k_verbose;	/* from xlisp/xlglob.c, init'd in xlisp/xlinit.c */
  extern LVAL k_NOVERBOSE;	/* from utils.c */
  XImage*	 ximage;
  char*          err_string;
  int		 num_alloc_cols;
  unsigned long* alloc_cols;
  Bool		 quiet_p;
  LVAL		 lval_filename;
  char*          filename;

  /*
   * retrieve req'd <gif-file-name-str> arg
   */
  lval_filename = xlgastring();
  filename = getstring(lval_filename);

  /*
   * retrieve optional :verbose arg
   */
  if (moreargs()) {
    LVAL lval_arg = nextarg();
    if (k_verbose == lval_arg) {
      quiet_p = False;
    }
    else if (k_NOVERBOSE == lval_arg) {
      quiet_p = True;
    }
    else {
      xlbadtype(lval_arg);
    }
  }
  else {
    quiet_p = True;
  }
  xllastarg();

  if (!GIF_To_XImage(display, screen, colormap, filename, quiet_p,
		     &err_string, &ximage, &num_alloc_cols, &alloc_cols)) {
    /* *err_string was set by GIF_To_XImage() */
    xlerror(err_string, lval_filename);
  }
  else {
    WINTERP_GIF_COLOR_INFO color_info;
    Pixmap                 pixmap;

    /* ximage         -- set by GIF_To_XImage() */
    /* num_alloc_cols -- set by GIF_To_XImage() */
    /* alloc_cols     -- set by GIF_To_XImage() */

    pixmap = XCreatePixmap(display, root_win,
			   (unsigned int) ximage->width,
			   (unsigned int) ximage->height,
			   DefaultDepthOfScreen(screen));
    XPutImage(display, pixmap, DefaultGCOfScreen(screen), ximage,
	      0,		/* src_x */
	      0,		/* src_y */
	      0,		/* dest_x */
	      0,		/* dest_y */
	      (unsigned int) ximage->width,
	      (unsigned int) ximage->height);

    XDestroyImage(ximage);	/* Destroy the XImage 'ximage' and the image data ximage->data created by GIF_To_XImage() */

    /*
     * When garbage collected, this node gets freed by  Wpm_Decr_Refcount_Or_Free_Pixmap()
     * which will call
     * XFreePixmap(display, pixmap);
     * if (num_alloc_cols > 0)
     *    XFreeColors(display, colormap, alloc_cols, num_alloc_cols, 0L);
     * XtFree((char*) alloc_cols);
     */
    color_info = (WINTERP_GIF_COLOR_INFO) XtMalloc(sizeof(struct _WINTERP_GIF_Color_Info_Node));
    color_info->num_allocd_colors = num_alloc_cols;
    color_info->allocd_colors = alloc_cols;
    return (cv_pixmap_allocd_colors(pixmap, color_info));
  }
}
#endif /* WINTERP_XTANGO_WIDGET */
