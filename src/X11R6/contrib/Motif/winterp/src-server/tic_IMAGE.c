/* -*-C-*-
********************************************************************************
*
* File:         tic_IMAGE.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_IMAGE.c,v 2.1 1994/06/06 15:41:07 npm Exp $
* Description:  TANGO:IMAGE_CLASS (the TANGOIMAGEOBJ metaclass)
* Author:       Niels P. Mayer
* Created:      Sat May 15 19:59:55 1993
* Modified:     Sun Jun  5 14:26:35 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_IMAGE.c,v 2.1 1994/06/06 15:41:07 npm Exp $";

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
#include "w_funtab.h"
#include "tango.h"

LVAL o_TANGOIMAGE_CLASS;	/* note -- this is used by t_classes.c */
LVAL s_TANGO_IMAGE_TYPE_ID;	/* note -- this is used by t_classes.c */


/*****************************************************************************
 * (send TANGO:IMAGE_CLASS :new ...) --> signals an error.
 * TANGO:IMAGE_CLASS is not an instatiable class, but it's subclasses are...
 ****************************************************************************/
LVAL Tango_Image_Class_Method_ISNEW()
{
  LVAL self;
  self = xlgaobject();
  /* xllastarg(); -- don't signal an error if more args... */

  xlerror("Base class 'TANGO:IMAGE_CLASS' is not instantiable", self);
  return (NIL);
}


/*****************************************************************************
 * (send <tiobj> :SHOW)
 *		--> return <Widget_Class>
 *
 * overrides OBJECT method :SHOW (xlobj.c:obshow()). Since TANGOIMAGEOBJs
 * contain a special non-LVAL " _bogus_ivar_ " we don't want to print that,
 * since it isn't a LVAL and could result in segmentation violations.
 ****************************************************************************/
LVAL Tango_Image_Class_Method_SHOW()
{
  return (Wcls_Generic_Hybrid_Array_Method_SHOW(o_TANGOIMAGE_CLASS));
}


/******************************************************************************
 * (send <tiobj> :EXISTS_P)
 * 	==> returns T if the TANGOIMAGEOBJ exists (hasn't been destroyed)
 *	==> returns NIL if the TANGOIMAGEOBJ has been destroyed
 *	    or has never been initialized.
 *******************************************************************************/
LVAL Tango_Image_Class_Method_EXISTS_P()
{
  extern LVAL true;
  LVAL self = xlga_tangoimageobj();
  xllastarg();

  if (get_tangoimageobj_timageID(self))
    return (true);
  else
    return (NIL);
}


/******************************************************************************
 * (send <tiobj> :PRIN1)
 *
 * the default :PRIN1 method for TANGOIMAGEOBJs -- redefine this in WINTERP to
 * alter the format used to print TANGOIMAGEOBJs.
 *******************************************************************************/
LVAL Tango_Image_Class_Method_PRIN1()
{
  extern LVAL s_stdout;
  LVAL self,fptr;

  /* get self and the file pointer */
  self = xlga_tangoimageobj();
#ifdef BETTERIO
  fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
  fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
  xllastarg();

  /* print it */
  Tcls_Print_TANGOIMAGEOBJ(fptr,self);

  /* return the object */
  return (self);
}


/******************************************************************************
 * (send <tiobj> :IMAGE_COPY)
 *	==> returns a TANGOIMAGEOBJ.
 *
 * This method returns a new instance of TANGO_IMAGE_CLASS (or a subclass
 * thereof) which is an exact copy of the one given as a parameter <tiobj>
 * (at that instant in time).  The new image will not appear until a transition
 * is performed, or until a :REFRESH message is sent to the TANGO:WIDGET_CLASS
 * instance in which the image resides. This behavior is similar to other cases
 * of TANGO_IMAGE_CLASS instance creation (method :NEW).
 * ----------------------------------------------------------------------------
 * TANGO_IMAGE
 * TANGOimage_copy (image) 
 *    TANGO_IMAGE image;
 ******************************************************************************/
LVAL Tango_Image_Class_Method_IMAGE_COPY()
{
  TANGO_IMAGE image;
  WINTERP_TANGO_CONTEXT context;
  LVAL o_image, o_new_image;
  int imageobj_size;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  context = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

  image = TANGOimage_copy(image);

  /* create an instance (subclass of TANGO_IMAGE_CLASS) of same class as the original image */
  xlsave1(o_new_image);
  imageobj_size = getsize(o_image); /* retrieve the size of the ARRAY-type underlying the TANGOIMAGEOBJ*/
  o_new_image = newvector(imageobj_size);	/* create the object's underlying ARRAY-type */
  o_new_image->n_type = XLTYPE_TANGOIMAGEOBJ; /* VECTOR o_new_image is now really a TANGOIMAGEOBJ */
  MEMCPY(o_new_image->n_vdata, o_image->n_vdata, imageobj_size * sizeof(LVAL)); /* make an exact copy of the original */
  Tcls_Initialize_TANGOIMAGEOBJ(o_new_image, image, context); /* update tangoimageobj slots timageID, context; set backpointer from TANGO_IMAGE to tangoimageobj; and add a new entry in v_savedobjs[] */

  Xtango_Restore_Context();
  xlpop(/* o_new_image */);
  return (o_new_image);
}


/******************************************************************************
 * (send <tiobj> :IMAGE_LOC <image_part_kwd>)
 *	==> returns a COMPLEX number #C(<x> <y>), where <x> and <y> are FLONUMS
 *		representing the location of the image
 *
 *	<image_part_kwd> is a keyword argument, one of:
 *		:CTR	-- center
 *		:NW	-- north west
 *		:NE	-- north east
 *		:E	-- east
 *		:SE	-- south east
 *		:S	-- south
 *		:SW     -- south west
 *		:W      -- west
 * 
 * This routine returns a COMPLEX FLONUM #C(<x> <y>) that corresponds to the
 * location of the given <image_part_kwd> of the given <tiobj>.  Valid
 * <image_part_kwd> include :CTR (center), and the compass directions :NW, :N,
 * :NE, :E, :SE, :S, :SW, :W. For rectangles and circles, actual locations on
 * the image boundary (other than the center) are returned.  For lines,
 * ellipses, polylines, polygons, splines, and text, a bounding box location
 * is returned.  For composites, a bounding box of all the subimages is
 * returned.
 * ----------------------------------------------------------------------------
 * TANGO_LOC
 * TANGOimage_loc (image,part) 
 *    TANGO_IMAGE image;
 *    TANGO_PART_TYPE part;
 ******************************************************************************/
LVAL Tango_Non_Poly_Image_Class_Method_IMAGE_LOC()
{
  TANGO_IMAGE image;
  LVAL o_image;
  TANGO_PART_TYPE part;
  TANGO_LOC loc;
  double loc_x, loc_y;

  image	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  part	= Xtango_Get_TANGO_PART_TYPE_Arg();
  xllastarg();

  Xtango_Save_Set_Context(get_tangoimageobj_context(o_image));
  Xtango_Check_Initialized();

  loc = TANGOimage_loc(image, part);
  TANGOloc_inquire(loc, &loc_x, &loc_y);
  XtFree((char*) loc);

  Xtango_Restore_Context();

  return (newdcomplex(loc_x, loc_y));
}


/* stolen from wc_WIDGET.c:Wc_WIDGET_Init() */
/*****************************************************************************
 * Initialize the TangoImage interface. Note that this procedure does
 * equivelent kinds of setup as done by xloinit(). The following call 
 * sequence happens on startup:
 * [xlsymbols(), xloinit()] <-- initwks() <-- xlinit() <-- main()...
 * However, note that main() calls xlinit() only if xlirestore("xlisp.wks") 
 * fails. In that case, we get the calling sequence:
 * obsymbols() <-- xlsymbols() <-- xlirestore() <-- main(). We could patch
 * in the symbols initialized in this module by putting initialization 
 * routines in ossymbols(), but it's not clear that this would make sense:
 * restore will only be able to restore TANGOIMAGEOBJs' data, but will not
 * be able to recreate images themselves.
 *
 * Thus, we call this procedure after xlinit() in main() and this reinitializes
 * the TANGOIMAGEOBJs independent of restoring. Of course, any TANGOIMAGEOBJs
 * that were saved/restored may now not have a valid class, since the 
 * pointers may have changed ...
 ******************************************************************************/
void Tic_IMAGE_Init()
{
  s_TANGO_IMAGE_TYPE_ID = xlenter("TANGO_IMAGE_TYPE_ID");

  /*
   * create 'TANGO:IMAGE_CLASS' by doing the (almost) equivalent of 
   * (setq Tango_Image_Class (send Class :new 
   *                           '(tangoimage_id)
   *                             object))
   * Creating a new 'Class' instance would end up calling clnew() to create
   * a new instance of 'Class', and then call clisnew() on that 'Class'
   * instance to set the appropriate slots for 'TANGO:IMAGE_CLASS'. Thus we need to
   * create object 'TANGO:IMAGE_CLASS', and with that object, set
   *    self.MESSAGES = ( ... )
   *    self.IVARS = ( ... )
   *    self.CVARS = NIL
   *    self.CVALS = NIL
   *    self.SUPERCLASS = 'Object'
   *    self.IVARCNT = TANGOIMAGEOBJ_SIZE     ;; == length(self.IVARS)
   *    self.IVARTOTAL = TANGOIMAGEOBJ_SIZE   ;; == self.IVARCNT + length(superclass.IVARS)
   *
   * 'TANGO:IMAGE_CLASS' contains methods corresponding to common operations on
   * 'TANGO_IMAGE' in the Xtango.
   *
   * Unlike WIDGETOBJs, TANGOIMAGEOBJs cannot be init'd by an :ISNEW method
   * on the 'TANGO:IMAGE_CLASS' which looks up the C class-id and creates
   * the appropriate tangoimage; instead, each subclass of TANGO:IMAGE_CLASS
   * will have it's own :ISNEW method which will make the appropriate call to
   * TANGOimage_create().
   *
   * Note that 'TANGO:IMAGE_CLASS' is a core class. TANGOIMAGEOBJ instances
   * should be created from classes inheriting from 'TANGO:IMAGE_CLASS'.  The
   * TANGO:IMAGE_CLASS method :ISNEW will complain if you try to instantiate
   * 'TANGO:IMAGE_CLASS'...
   */
  
  /*--------------- create 'Class' instance 'TANGO:IMAGE_CLASS' -----------------*/
  /* Calling xlclass() creates the 'Class' instance called 'TANGO:IMAGE_CLASS' and 
     sets IVARCNT, IVARTOTAL, and SUPERCLASS slots
     slots CVARS, CVALS are set to NIL */
  o_TANGOIMAGE_CLASS = xlclass("TANGO:IMAGE_CLASS", TANGOIMAGEOBJ_SIZE); 

  /* set up the list of ivarnames in IVARS */
  xladdivar(o_TANGOIMAGE_CLASS, " _bogus_ivar2_ "); /* this "variable name" corresponds to the 'WINTERP_TANGO_CONTEXT' on a TANGOIMAGEOBJ -- users shouldn't access this var, since it isn't an LVAL (hack). */
  xladdivar(o_TANGOIMAGE_CLASS, " _bogus_ivar1_ "); /* this "variable name" corresponds to the 'TANGO_IMAGE' slot on a TANGOIMAGEOBJ -- users shouldn't access this var, since it isn't an LVAL (hack). */

  /* set up the list of methods in MESSAGES, and bind the associated FSUBR */

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT /* template for adding new methods */
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_Tango_Image_Class_Method_); /*  */
#endif

  xladdmsg(o_TANGOIMAGE_CLASS, ":SHOW",
	   FTAB_Tango_Image_Class_Method_SHOW);
  xladdmsg(o_TANGOIMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_Image_Class_Method_ISNEW);
  xladdmsg(o_TANGOIMAGE_CLASS, ":EXISTS_P",
	   FTAB_Tango_Image_Class_Method_EXISTS_P);
  xladdmsg(o_TANGOIMAGE_CLASS, ":PRIN1",
	   FTAB_Tango_Image_Class_Method_PRIN1);
  xladdmsg(o_TANGOIMAGE_CLASS, ":IMAGE_COPY",
	   FTAB_Tango_Image_Class_Method_IMAGE_COPY);

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGOIMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Non_Poly_Image_Class_Method_IMAGE_LOC);
   */

  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_MOVE",
	   FTAB_Tango_Image_Class_Method_TX_MOVE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_VISIBLE",
	   FTAB_Tango_Image_Class_Method_TX_VISIBLE);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_COLOR",
           FTAB_Tango_Image_Class_Method_TX_COLOR);
   */
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_RAISE",
	   FTAB_Tango_Image_Class_Method_TX_RAISE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_LOWER",
	   FTAB_Tango_Image_Class_Method_TX_LOWER);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_DELAY",
	   FTAB_Tango_Image_Class_Method_TX_DELAY);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_REFRESH",
	   FTAB_Tango_Image_Class_Method_TX_REFRESH);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_DELETE",
	   FTAB_Tango_Image_Class_Method_TX_DELETE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_ZOOM",
	   FTAB_Tango_Image_Class_Method_TX_ZOOM);

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass...
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);
   */

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS.
  xladdmsg(o_TANGOIMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);
   */

  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_SHOW",
	   FTAB_Tango_Image_Class_Method_TAP_SHOW);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_FILL",
	   FTAB_Tango_Image_Class_Method_TAP_FILL);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_COLOR",
	   FTAB_Tango_Image_Class_Method_TAP_COLOR);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_VIS_TOGGLE",
	   FTAB_Tango_Image_Class_Method_TAP_VIS_TOGGLE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_JUMP",
	   FTAB_Tango_Image_Class_Method_TAP_JUMP);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_MOVE",
	   FTAB_Tango_Image_Class_Method_TAP_MOVE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_TRAVERSE",
	   FTAB_Tango_Image_Class_Method_TAP_TRAVERSE);
  xladdmsg(o_TANGOIMAGE_CLASS, ":TAP_FLASH",
	   FTAB_Tango_Image_Class_Method_TAP_FLASH);

/*
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_);
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_);
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_);
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_);
  xladdmsg(o_TANGOIMAGE_CLASS, ":",
	   FTAB_);
*/
}
