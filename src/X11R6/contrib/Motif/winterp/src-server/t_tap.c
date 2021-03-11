/* -*-C-*-
********************************************************************************
*
* File:         t_trans.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/t_tap.c,v 2.1 1994/06/06 15:41:12 npm Exp $
* Description:  Interfaces to Xtango "TAP Routines", the Tango Animation
*		Package. These are methods on TANGO:IMAGE_CLASS plus some
*		primitives.
* Author:       Niels P. Mayer
* Created:      Sat May 15 22:22:03 1993
* Modified:     Sun Jun  5 14:21:43 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/t_tap.c,v 2.1 1994/06/06 15:41:12 npm Exp $";

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
#include "tango.h"

/******************************************************************************
 * (send <tiobj> :TAP_SHOW)
 *	==> returns NIL
 * 
 * This method simply displays the <tiobj> (i.e. uses a delay transition to
 * display the object).  Creating a TANGO:IMAGE_CLASS instance will not cause
 * the image to appear on the display until until a subsequent animation
 * occurs, therefore this routine is useful for making sure that an image is
 * properly displayed.
 * 
 * Note that if a large number of images are to be displayed, it is more
 * efficient to do a :REFRESH on the TANGO:WIDGET_CLASS instance after all the
 * images have been created.
 * ----------------------------------------------------------------------------
 * void
 * TAPshow (image)
 *    TANGO_IMAGE image;
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_SHOW()
{
  TANGO_IMAGE image;
  LVAL o_image;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  Xtango_Save_Set_Context(get_tangoimageobj_context(o_image));
  Xtango_Check_Initialized();

  TAPshow(image);

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 * (send <tiobj> :TAP_FILL [:PERFORM])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_FILL()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();

  trans = TAPfill(image);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_COLOR [:PERFORM] <tango-color>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * <tango-color> gives the color to which the image is changed when the
 * transition is performed. <tango-color> can either be a string color name
 * (e.g. "white" "black" "red" "orange" "yellow" "green" "blue" or "maroon".)
 *
 * Alternately, <tango-color> can be a FIXNUM [0-7] representing the
 * tango-color pixel value. FIXNUM values for the standard
 * Xtango colors are specified via the constants TANGO_COLOR_WHITE,
 * TANGO_COLOR_BLACK, TANGO_COLOR_RED, TANGO_COLOR_ORANGE, TANGO_COLOR_YELLOW,
 * TANGO_COLOR_GREEN, TANGO_COLOR_BLUE, or TANGO_COLOR_MAROON. 
 * 
 * Note that :TAP_COLOR, :TX_COLOR, and TANGO:PATH_COLOR only work with the
 * eight "basic" tango colors mentioned above. Using colors other than the
 * eight aforementioned colors, or using colors generated by  TANGO:LOAD_COLOR
 * will signal an error.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_COLOR()
{
  LVAL o_image, lval_color;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  TANGO_COLOR tango_color;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  lval_color	= xlgetarg();	/* get <tango-color> */
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();  

  tango_color = Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error(lval_color);

  trans = TAPcolor(image, tango_color);
  
  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_VIS_TOGGLE [:PERFORM])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_VIS_TOGGLE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TAPvis_toggle(image);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_JUMP [:PERFORM] <image_part_kwd> <location>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
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
 *	<location> is a COMPLEX FLONUM #C(<x> <y>) representing the x/y
 *		coordinates of a location.
 *
 * This method sets up a transition to move <tiobj> to the given <location>
 * using the specified <image_part_kwd> as a "handle."  The transition uses a
 * path of length one, so essentially, the image will appear to "jump" from
 * its current location to the new location.  The necessary transition is
 * returned.
 * ---------------------------------------------------------------------------
 * TANGO_TRANS
 * TAPjump(image, part, location)
 *    TANGO_IMAGE image;
 *    TANGO_PART_TYPE part;
 *    TANGO_LOC location;
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_JUMP()
{
  LVAL o_image, lval_loc;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  TANGO_PART_TYPE part;
  TANGO_LOC location;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  part		= Xtango_Get_TANGO_PART_TYPE_Arg();
  lval_loc	= Xtango_Get_FLONUM_COMPLEX_Arg();
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  location = TANGOloc_create((double) getflonum(getelement(lval_loc, 0)), 
			     (double) getflonum(getelement(lval_loc, 1)));

  trans = TAPjump(image, part, location);

  XtFree((char*) location);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_MOVE [:PERFORM] <image_part_kwd> <location>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
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
 *	<location> is a COMPLEX FLONUM #C(<x> <y>) representing the x/y
 *		coordinates of a location.
 * 
 * This method sets up a transition to move <tiobj> from the location of its
 * indicated <image_part_kwd> to the given <location>.  This method is
 * similar to method :TAP_JUMP; however, instead of using a path of length one to
 * create the transition, it uses TANGO:PATH_MOTION to create the transition,
 * which means that the path length is 20.  The required transition is
 * returned.
 * ---------------------------------------------------------------------------
 * TAPmove(image, part, location)
 *    TANGO_IMAGE image;
 *    TANGO_PART_TYPE part;
 *    TANGO_LOC location;
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_MOVE()
{
  LVAL o_image, lval_loc;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  TANGO_PART_TYPE part;
  TANGO_LOC location;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  part		= Xtango_Get_TANGO_PART_TYPE_Arg();
  lval_loc	= Xtango_Get_FLONUM_COMPLEX_Arg();
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  location = TANGOloc_create((double) getflonum(getelement(lval_loc, 0)), 
			     (double) getflonum(getelement(lval_loc, 1)));

  trans = TAPmove(image, part, location);

  XtFree((char*) location);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_TRAVERSE [:PERFORM] <image_part_kwd> <location> <path_motion_kwd>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
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
 *	<location> is a COMPLEX FLONUM #C(<x> <y>) representing the x/y
 *		coordinates of a location.
 *	<path_motion_kwd> is a keyword, either :STRAIGHT, :CLOCKWISE,
 *		or :COUNTERCLOCKWISE.
 *
 * This routine creates the transition necessary to move an object (using the
 * specified <image_part_kwd> of the object) from its current position to
 * <location>.  The type of path that the object follows is given by the
 * parameter <path_motion_kwd>; <path_motion_kwd> is simply a path type, i.e.
 * it is of the same form as the path types for the TANGO:PATH_MOTION routine.
 * ----------------------------------------------------------------------------
 * TANGO_TRANS
 * TAPtraverse(image, part, location, motion)
 *    TANGO_IMAGE image;
 *    TANGO_PART_TYPE part;
 *    TANGO_LOC location;
 *    TANGO_PATH_TYPE motion;
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_TRAVERSE()
{
  LVAL o_image, lval_loc;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  TANGO_PART_TYPE part;
  TANGO_LOC location;
  TANGO_PATH_TYPE motion;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  part		= Xtango_Get_TANGO_PART_TYPE_Arg();
  lval_loc	= Xtango_Get_FLONUM_COMPLEX_Arg();
  motion	= Xtango_Get_TANGO_PATH_TYPE_Arg();
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  location = TANGOloc_create((double) getflonum(getelement(lval_loc, 0)), 
			     (double) getflonum(getelement(lval_loc, 1)));

  trans = TAPtraverse(image, part, location, motion);

  XtFree((char*) location);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <tiobj> :TAP_FLASH [:PERFORM] <num_flash_fixnum>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TAP_FLASH()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt;
  int num_flash;
  Boolean perform_p;

  image		= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  num_flash	= Xtango_Get_Positive_FIXNUM_Arg();
  xllastarg();

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TAPflash(image, num_flash);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (TANGO:TAP_SWITCH [:PERFORM] <image-1> <image-2>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TAP_SWITCH()
{
  LVAL o_image1, o_image2;
  TANGO_IMAGE image1, image2;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt1, contxt2;
  Boolean perform_p;

  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  image1	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image1);
  image2	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image2);
  xllastarg();

  contxt1 = get_tangoimageobj_context(o_image1);
  contxt2 = get_tangoimageobj_context(o_image2);

  if (contxt1 != contxt2)
    xlfail("TANGO:TAP_SWITCH -- both images must be in same widget.");

  Xtango_Save_Set_Context(contxt1);
  Xtango_Check_Initialized();
  
  trans = TAPswitch(image1, image2);
  
  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt1));
}


/******************************************************************************
 * (TANGO:TAP_EXCHANGE [:PERFORM] <image-1> <image-2>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TAP_EXCHANGE()
{
  LVAL o_image1, o_image2;
  TANGO_IMAGE image1, image2;
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT contxt1, contxt2;
  Boolean perform_p;

  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  image1	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image1);
  image2	= Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image2);
  xllastarg();

  contxt1 = get_tangoimageobj_context(o_image1);
  contxt2 = get_tangoimageobj_context(o_image2);

  if (contxt1 != contxt2)
    xlfail("TANGO:TAP_EXCHANGE -- both images must be in same widget.");

  Xtango_Save_Set_Context(contxt1);
  Xtango_Check_Initialized();
  
  trans = TAPexchange(image1, image2);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt1));
}
