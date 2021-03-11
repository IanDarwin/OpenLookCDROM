/* -*-C-*-
********************************************************************************
*
* File:         t_path.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/t_path.c,v 2.1 1994/06/06 15:41:13 npm Exp $
* Description:  Interfaces to Xtango procs dealing with type TANGO_PATH
* Author:       Niels P. Mayer
* Created:      Sat May 15 22:24:53 1993
* Modified:     Sun Jun  5 14:21:07 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/t_path.c,v 2.1 1994/06/06 15:41:13 npm Exp $";

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
 * Call this from garbage collector xlisp/xldmem.c:sweep()
 ******************************************************************************/
void Xtango_Context_Remove_TANGO_PATH(lval)
     LVAL lval;			/* XLTYPE_TANGO_PATH */
{
  TANGO_PATH path;

  if ((path = get_tangopath(lval)) != (TANGO_PATH) NULL) {
    /* Note assumption that path_free() doesn't need
     * Xtango_Save_Set_Context...() and Xtango_Restore_Context()
     * since no tango globals are affected by path_free() */
    path_free(path);		/* single arg version of TANGOpath_free(...) */
    set_tangopath(lval, (TANGO_PATH) NULL);
  }
}


/******************************************************************************
 * 
 ******************************************************************************/
static void Xtango_Error_Freed_TANGO_PATH(lval_path)
     LVAL lval_path;
{
  xlerror("TANGO_PATH has been freed by TANGO:PATH_FREE.", lval_path);
}


/******************************************************************************
 * 
 ******************************************************************************/
static TANGO_PATH Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(lval_path_return)
     LVAL *lval_path_return;
{
  TANGO_PATH path;

  if ((path = get_tangopath(*lval_path_return = xlga_tangopath())) == (TANGO_PATH) NULL)
    Xtango_Error_Freed_TANGO_PATH(*lval_path_return);
  else
    return (path);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a tango_path argument.
 *
 * The routine behaves as follows:
 *
 * + if the arg is of type TANGO_PATH (e.g. created by TANGO:PATH_CREATE),
 *   returns that, and sets result_to_be_freed_p==FALSE.
 *
 * + If no args available, returns a null path of length one, setting
 *   result_to_be_freed_p==TRUE.
 *
 * + if the arg is a FIXNUM, creates a null path whose length is the FIXNUM
 *   value; sets result_to_be_freed_p==TRUE.
 *
 * + if the arg is a complex number, creates/returns a path of length one
 *    with the path's x-offset set to the real part, and the y-offset set to
 *   the imaginary part; sets result_to_be_freed_p==TRUE.
 *
 * + if the arg is a list or array of complex numbers, creates/returns
 *   path with x-offsets set to the real part, y-offset set to the imag.
 *   part; sets result_to_be_freed_p==TRUE.
 ******************************************************************************/
#define DOUBLE_ARRAY_SIZE_INCREMENT 20
TANGO_PATH Xtango_Get_TANGO_PATH_Args(result_to_be_freed_p)
     Boolean *result_to_be_freed_p;
{
  int size, i;
  double* x_array_of_double;
  double* y_array_of_double;
  LVAL arg, arg2, elt, elt2;
  TANGO_PATH path;
  int path_len;

  if (moreargs()) {
    arg = nextarg();
    switch (ntype(arg)) {

    case XLTYPE_TANGO_PATH:	/* just return this TANGO_PATH */

      *result_to_be_freed_p = FALSE;
      if ((path = get_tangopath(arg)) == (TANGO_PATH) NULL)
	Xtango_Error_Freed_TANGO_PATH(arg);
      break;

    case FIXNUM:		/* return path created by TANGOpath_null(i), where i is the fixnum argument */

      path_len = (int) getfixnum(arg);
      if (path_len <= 0)
	xlerror("Path length must be > 0.", arg);
      *result_to_be_freed_p = TRUE;
      path = TANGOpath_null(path_len);
      break;

    case VECTOR:		/* return path created from VECTOR of COMPLEX numbers, or 2 VECTORs of X and Y FLONUMs */

      size = getsize(arg);
      if ((size > 0) && floatp(getelement(arg, 0))) { /* IF 1st elt of VECTOR is a FLONUM, arg is VECTOR of X offsets */
	arg2 = xlgavector();	/* arg2 must be a VECTOR of Y offsets, else signal error */
	if (size != getsize(arg2))
	  xlerror("Lengths of x and y path offsets are different.", arg);
	x_array_of_double
	  = (double*) XtMalloc((unsigned) (size * sizeof(double)));
	y_array_of_double
	  = (double*) XtMalloc((unsigned) (size * sizeof(double)));

	for (i = 0; i < size; i++) {
	  elt = getelement(arg, i);
	  elt2 = getelement(arg2, i);
	  if (floatp(elt) && floatp(elt2)) {
	    x_array_of_double[i] = (double) getflonum(elt);
	    y_array_of_double[i] = (double) getflonum(elt2);
	  }
	  else {
	    XtFree((char*) x_array_of_double);
	    XtFree((char*) y_array_of_double);
	    xlerror("Bad type -- Expected ARRAY of FLONUM numbers -- found a non FLONUM element.",
		    (!floatp(elt)) ? elt : elt2);
	  }
	}
      }
      else {			/* <arg> is a VECTOR of COMPLEX, else signal error */
	x_array_of_double
	  = (double*) XtMalloc((unsigned) (size * sizeof(double)));
	y_array_of_double
	  = (double*) XtMalloc((unsigned) (size * sizeof(double)));

	for (i = 0; i < size; i++) {
	  elt = getelement(arg, i);
	  if (complexp(elt) && floatp(getelement(elt, 0)) && floatp(getelement(elt, 1))) {
	    x_array_of_double[i] = (double) getflonum(getelement(elt, 0));
	    y_array_of_double[i] = (double) getflonum(getelement(elt, 1));
	  }
	  else {
	    XtFree((char*) x_array_of_double);
	    XtFree((char*) y_array_of_double);
	    xlerror("Bad type -- Expected ARRAY of COMPLEX (pair of FLONUMs) numbers -- found a non COMPLEX/FLONUM element.", elt);
	  }
	}
      }
      *result_to_be_freed_p = TRUE;
      path = TANGOpath_create(i, x_array_of_double, y_array_of_double);
      XtFree((char*) x_array_of_double);
      XtFree((char*) y_array_of_double);
      break;

    case CONS:			/* return path created from LIST of COMPLEX numbers */

      if (floatp(car(arg))) {	/* IF 1st elt of LIST is a FLONUM, THEN arg is LIST of X offsets */
	arg2 = xlgacons();	/* arg2 must be a LIST of Y offsets, else signal error */
	size = 0;
	x_array_of_double = y_array_of_double = (double*) NULL;

	for (i = 0 ; (consp(arg) && consp(arg2)) ; arg = cdr(arg), arg2 = cdr(arg2), i++) {
	  if (i >= size) {	/* make sure it'll fit into allocated array_of_double */
	    size += DOUBLE_ARRAY_SIZE_INCREMENT;
	    x_array_of_double
	      = (double*) XtRealloc((char*) x_array_of_double, (unsigned) (size * sizeof(double)));
	    y_array_of_double
	      = (double*) XtRealloc((char*) y_array_of_double, (unsigned) (size * sizeof(double)));
	  }
	  elt = car(arg);
	  elt2 = car(arg2);
	  if (floatp(elt) && floatp(elt2)) {
	    x_array_of_double[i] = (double) getflonum(elt);
	    y_array_of_double[i] = (double) getflonum(elt2);
	  }
	  else {
	    XtFree((char*) x_array_of_double);
	    XtFree((char*) y_array_of_double);
	    xlerror("Bad type -- Expected LIST of FLONUM numbers -- found a non FLONUM element.",
		    (!floatp(elt)) ? elt : elt2);
	  }
	}
	if ((arg != NIL) || (arg2 != NIL)) { /* if loop terminated due to list pointer not being a CONS cell */
	  XtFree((char*) x_array_of_double);
	  XtFree((char*) y_array_of_double);
	  xlerror("Bad type -- Expected two equal length LISTs of FLONUM numbers.",
		  (arg == NIL) ? arg2 : arg);
	}
      }
      else {			/* <arg> is an LIST of COMPLEX, else signal error */
	size = 0;
	x_array_of_double = y_array_of_double = (double*) NULL;

	for (i = 0 ; (consp(arg)) ; arg = cdr(arg), i++) {
	  if (i >= size) {	/* make sure it'll fit into allocated array_of_double */
	    size += DOUBLE_ARRAY_SIZE_INCREMENT;
	    x_array_of_double
	      = (double*) XtRealloc((char*) x_array_of_double, (unsigned) (size * sizeof(double)));
	    y_array_of_double
	      = (double*) XtRealloc((char*) y_array_of_double, (unsigned) (size * sizeof(double)));
	  }
	  elt = car(arg);
	  if (complexp(elt) && floatp(getelement(elt, 0)) && floatp(getelement(elt, 1))) {
	    x_array_of_double[i] = (double) getflonum(getelement(elt, 0));
	    y_array_of_double[i] = (double) getflonum(getelement(elt, 1));
	  }
	  else {
	    XtFree((char*) x_array_of_double);
	    XtFree((char*) y_array_of_double);
	    xlerror("Bad type -- Expected LIST of COMPLEX (pair of FLONUMs) numbers -- found a non COMPLEX element.", elt);
	  }
	}
	if (arg != NIL) {	/* if loop terminated due to list pointer not being a CONS cell */
	  XtFree((char*) x_array_of_double);
	  XtFree((char*) y_array_of_double);
	  xlerror("Bad type -- Expected LIST of COMPLEX (pair of FLONUMs) numbers -- found a non COMPLEX element.", arg);
	}
      }
      *result_to_be_freed_p = TRUE;
      path = TANGOpath_create(i, x_array_of_double, y_array_of_double);
      XtFree((char*) x_array_of_double);
      XtFree((char*) y_array_of_double);
      break;

    case COMPLEX:		/* return path of length N created from subsequent complex args. */

      size = DOUBLE_ARRAY_SIZE_INCREMENT;
      x_array_of_double
	= (double*) XtMalloc((unsigned) (size * sizeof(double)));
      y_array_of_double
	= (double*) XtMalloc((unsigned) (size * sizeof(double)));

      if (floatp(getelement(arg, 0)) && floatp(getelement(arg, 1))) { /* convert [COMPLEX-0] */
	x_array_of_double[0] = (double) getflonum(getelement(arg, 0));
	y_array_of_double[0] = (double) getflonum(getelement(arg, 1));
      }
      else {
	XtFree((char*) x_array_of_double);
	XtFree((char*) y_array_of_double);
	Xtango_Error_Bad_FLONUM_COMPLEX_Arg(arg);
      }
      for (i = 1 ; (moreargs()) ; i++) { /* convert [COMPLEX-1] ... [COMPLEX-n] */
	arg = nextarg();
	if (complexp(arg) && floatp(getelement(arg, 0)) && floatp(getelement(arg, 1))) {
	  if (i >= size) {	/* make sure it'll fit into allocated array_of_double */
	    size += DOUBLE_ARRAY_SIZE_INCREMENT;
	    x_array_of_double
	      = (double*) XtRealloc((char*) x_array_of_double, (unsigned) (size * sizeof(double)));
	    y_array_of_double
	      = (double*) XtRealloc((char*) y_array_of_double, (unsigned) (size * sizeof(double)));
	  }
	  x_array_of_double[i] = (double) getflonum(getelement(arg, 0));
	  y_array_of_double[i] = (double) getflonum(getelement(arg, 1));
	}
	else {
	  XtFree((char*) x_array_of_double);
	  XtFree((char*) y_array_of_double);
	  Xtango_Error_Bad_FLONUM_COMPLEX_Arg(arg);
	}
      }
      *result_to_be_freed_p = TRUE;
      path = TANGOpath_create(i, x_array_of_double, y_array_of_double);
      XtFree((char*) x_array_of_double);
      XtFree((char*) y_array_of_double);
      break;

    default:

      xlerror("Bad type -- expected either [NO ARG], [COMPLEX], [FIXNUM], or [SEQUENCE OF COMPLEX].", arg);
      break;
    }
  }
  else {
    *result_to_be_freed_p = TRUE;
    path = TANGOpath_null(1);	/* if no args, then create a null path of length 1 */
  }
  return (path);
}


/******************************************************************************
 * (TANGO:PATH_CREATE [<path>...])
 *	==> returns a TANGO_PATH node representing a path (<x0,y0>...<xn,yn>)
 *
 * The [<path>...] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *			example: (TANGO:PATH_CREATE)
 *					==> returns null path of length 1.
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *			example: (TANGO:PATH_CREATE 2)
 *					==> returns null path of length 2.
 *	* else, if <path> is one or more COMPLEX (e.g. #C(<x> <y>) arguments
 *		then a path element is created for each COMPLEX argument,
 *		with the x and y offsets of each path element set to each
 *		complex argument's <x> and <y> parts.
 *			example: (TANGO:PATH_CREATE #C(0.1 0.1) #C(0.1 0.1)
 *						    #C(0.1 0.1) #C(0.1 0.1))
 *					==> returns a path of length 4 w/
 *					    x and y offsets set to 0.1 .
 *	* else, if <path> is a a pair of sequences (LIST or ARRAY) of FLONUMS,
 *		then a path of the same length as the sequences is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 *			example: (TANGO:PATH_CREATE #'(0.1 0.1 0.1 0.1)
 *						    #'(0.1 0.1 0.1 0.1))
 *					==> returns a path of length 4 w/
 *					    x and y offsets set to 0.1 .
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_create (n,xoffsets,yoffsets) 
 *	int n;
 *	double xoffsets[],yoffsets[];
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_CREATE()
{
  extern LVAL s_unbound;
  TANGO_PATH path;
  Boolean tangopath_needs_freeing_p;
 
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  /*
   * Xtango_Get_TANGO_PATH_Args() as used for :TX_* methods allows for a
   * TANGO_PATH arg to be accepted, in which case it will set
   * tangopath_needs_freeing_p to FALSE; all other cases where a tangopath
   * is created sets tangopath_needs_freeing_p to TRUE
   */
  if (tangopath_needs_freeing_p == FALSE)
    xlbadtype(s_unbound);	/* pass s_unbound so that underlying xlerror() won't print val, see xlfail() */

  return (cv_tangopath(path));
}


/******************************************************************************
 * (TANGO:PATH_LENGTH <path>)
 *	==> returns a FIXNUM representing length of <path>.
 *
 *	<path> is a TANGO_PATH node, created (for example) by
 *	function TANGO:PATH_CREATE.
 *
 * This routine returns the numbers of offsets in the given path.
 * ------------------------------------------------------------------------------
 * int
 * TANGOpath_length (path) 
 *	TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_LENGTH()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cvfixnum((FIXTYPE) TANGOpath_length(path)));
}


/******************************************************************************
 * (TANGO:PATH_DX <path>)
 *	==> returns a FLONUM representing the x distance traversed by path
 *
 *	<path> is a TANGO_PATH node, created (for example) by
 *	function TANGO:PATH_CREATE.
 *
 * This routine returns the <x> distance that the given <path> traverses from
 * start to finish.
 * ------------------------------------------------------------------------------
 * double
 * TANGOpath_dx (path) 
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_DX()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cvflonum((FLOTYPE) TANGOpath_dx(path)));
}


/******************************************************************************
 * (TANGO:PATH_DY <path>)
 *	==> returns a FLONUM representing the y distance traversed by path
 *
 *	<path> is a TANGO_PATH node, created (for example) by
 *	function TANGO:PATH_CREATE.
 *
 * This routine returns the <y> distance that the given <path> traverses from
 * start to finish.
 * ------------------------------------------------------------------------------
 * double
 * TANGOpath_dy (path) 
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_DY()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cvflonum((FLOTYPE) TANGOpath_dy(path)));
}


/******************************************************************************
 * (TANGO:PATH_TYPE <k_type>)
 *	==> returns a TANGO_PATH node.
 *
 *	<k_type> is a keyword argument, one of
 *		:STRAIGHT	  -- move right
 *		:CLOCKWISE	  -- move clockwise right in an upward arc
 *		:COUNTERCLOCKWISE -- move counterclockwise left in an upward arc
 *
 * Each of these paths when created will have 20 relative offset points and
 * will change in <x> by a total value of 0.2 animation coordinate system
 * units.
 * 
 * The paths created by this routine will probably not be utilized as is,
 * although there is no reason why they cannot be.  They are provided
 * primarily as starting paths for use in the subsequent path modification
 * routines.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_type (type) 
 *    TANGO_PATH_TYPE type;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_TYPE()
{
  TANGO_PATH_TYPE type;

  type = Xtango_Get_TANGO_PATH_TYPE_Arg();
  xllastarg();

  return (cv_tangopath(TANGOpath_type(type)));
}


/******************************************************************************
 * (TANGO:PATH_COLOR <color>)
 *	==> returns a TANGO_PATH node.
 *
 *	<color> is a FIXNUM ranging from 0-7 representing a Xtango pixel value.
 *	The following symbols have been predefined with Xtango pixel values:
 *		TANGO_COLOR_WHITE			0
 *		TANGO_COLOR_YELLOW			1
 *		TANGO_COLOR_GREEN			2
 *		TANGO_COLOR_BLUE			3
 *		TANGO_COLOR_ORANGE			4
 *		TANGO_COLOR_RED				5
 *		TANGO_COLOR_MAROON			6
 *		TANGO_COLOR_BLACK			7
 *  
 * This routine returns a one offset path that will change an image to the given
 * color when the path is used as an argument to tango-image method :TX_COLOR.
 * The direction of the offset in the path determines the color to
 * utilize.  For example, if there is an image 'red_door_image' that you wish to
 * be painted black, you could call
 *	(send red_door_image :TX_COLOR (TANGO:PATH_COLOR TANGO_COLOR_BLACK))
 * For the above usage, you could also avoid calling TANGO:PATH_COLOR entirely
 * by using method :TAP_COLOR, e.g.
 *	(send red_door_image :TAP_COLOR TANGO_COLOR_BLACK).
 * 
 * Note that colors created with TANGO:LOAD_COLOR cannot be used as parameters to
 * this function. The supported colors are TANGO_COLOR_WHITE thru
 * TANGO_COLOR_BLACK.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_color (color) 
 *    TANGO_COLOR color;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_COLOR()
{
  TANGO_COLOR tango_color;

  tango_color = (TANGO_COLOR) getfixnum(xlgafixnum()); /* change to enumerated type ??? */
  xllastarg();

  return (cv_tangopath(TANGOpath_color(tango_color)));
}


/******************************************************************************
 * (TANGO:PATH_ADD_TAIL <path> <num>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<num> is a positive FIXNUM, if <=0, the value 0 is used.
 * 
 * This routine elongates a path by extending the "tail" of the path.
 * The parameter <num> designates the number of null #C(0.0 0.0) offsets
 * that will be apppended to the given <path>, producing a new path
 * that is returned.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_add_tail (path,num) 
 *    TANGO_PATH path;
 *    int num;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_ADD_TAIL()
{
  LVAL lval_path;
  TANGO_PATH path;
  int num;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  num  = Xtango_Get_Positive_FIXNUM_Arg(); /* get <num> */
  xllastarg();

  return (cv_tangopath(TANGOpath_add_tail(path, num)));
}


/******************************************************************************
 * (TANGO:PATH_ADD_HEAD <path> <num>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<num> is a positive FIXNUM, if <=0, the value 0 is used.
 * 
 * This routine elongates a path by extending the "head" of the path.
 * The parameter <num> designates the number of null #C(0.0 0.0) offsets
 * that will be prepended to the given <path>, producing a new path
 * that is returned.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_add_head (path,num) 
 *    TANGO_PATH path;
 *    int num;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_ADD_HEAD()
{
  LVAL lval_path;
  TANGO_PATH path;
  int num;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  num  = Xtango_Get_Positive_FIXNUM_Arg(); /* get <num> */
  xllastarg();

  return (cv_tangopath(TANGOpath_add_head(path, num)));
}


/******************************************************************************
 * (TANGO:PATH_DELETE_TAIL <path> <num>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<num> is a positive FIXNUM, if <=0, the value 0 is used.
 * 
 * This routine provides a way of shortening a path.  The parameter <num>
 * designates the number of offsets that will be removed from tail of the
 * given <path> in order to produce a new path that is returned.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_delete_tail (path,num) 
 *    TANGO_PATH path;
 *    int num;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_DELETE_TAIL()
{
  LVAL lval_path;
  TANGO_PATH path;
  int num;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  num  = Xtango_Get_Positive_FIXNUM_Arg(); /* get <num> */
  xllastarg();

  return (cv_tangopath(TANGOpath_delete_tail(path, num)));
}


/******************************************************************************
 * (TANGO:PATH_DELETE_HEAD <path> <num>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<num> is a positive FIXNUM, if <=0, the value 0 is used.
 * 
 * This routine provides a way of shortening a path.  The parameter <num>
 * designates the number of offsets that will be removed from head of the
 * given <path> in order to produce a new path that is returned.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_delete_head (path,num) 
 *    TANGO_PATH path;
 *    int num;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_DELETE_HEAD()
{
  LVAL lval_path;
  TANGO_PATH path;
  int num;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  num  = Xtango_Get_Positive_FIXNUM_Arg(); /* get <num> */
  xllastarg();

  return (cv_tangopath(TANGOpath_delete_head(path, num)));
}


/******************************************************************************
 * (TANGO:PATH_REVERSE <path>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * This routine returns a path that is the inverse of the given <path>.
 * Being an inverse path means that the order of offsets is reversed, and
 * each offset points in the exact opposite direction.  This routine
 * provides a way for an image to retrace its tracks from a movement
 * transition.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_reverse (path) 
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_REVERSE()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cv_tangopath(TANGOpath_reverse(path)));
}


/******************************************************************************
 * (TANGO:PATH_ROTATE <path> <deg>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<deg> is a positive FIXNUM, ranging from 0 to 360 (degrees).
 * 
 * This routine returns a path that corresponds to the rotation of the given
 * <path> in a counter-clockwise motion.  The number of degrees to rotate is
 * provided by the FIXNUM integer parameter <deg> which should be between
 * 0 and 360.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_rotate (path,deg) 
 *    TANGO_PATH path;
 *    int deg;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_ROTATE()
{
  LVAL lval_path;
  TANGO_PATH path;
  int deg;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  deg  = Xtango_Get_Positive_FIXNUM_Arg(); /* get <deg> */
  xllastarg();

  return (cv_tangopath(TANGOpath_rotate(path, deg)));
}


/******************************************************************************
 * (TANGO:PATH_INTERPOLATE <path> <factor>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<factor> is a FLONUM
 * 
 * This routine returns a path in which the number of offsets is modified by a
 * factor given by the <factor> parameter.  If a path with length 10 is
 * interpolated with a factor of 2.0, the returned path will have 20 offsets.
 * If the factor is 0.3, the path will have 3 offsets.  Straight linear
 * interpolation is used.  Consequently, when interpolating a path with
 * choppy, wavy motion characteristics, individual extrema may be lost with
 * certain <factor> parameters.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_interpolate (path,factor) 
 *    TANGO_PATH path;
 *    double factor;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_INTERPOLATE()
{
  LVAL lval_path;
  TANGO_PATH path;
  double factor;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  factor = Xtango_Get_Positive_FLONUM_Arg(); /* get <factor> */
  xllastarg();

  return (cv_tangopath(TANGOpath_interpolate(path, factor)));
}


/******************************************************************************
 * (TANGO:PATH_SCALE <path> <scale>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<scale> is a COMPLEX number #C(<dx> <dy>) where <dx> <dy> are FLONUMs.
 * 
 * This routine returns a path in which each offset is scaled by the given
 * factors in X and Y.  That is, each offsets' X and Y values are mulitplied
 * by the respective <dx> and <dy> factors specified in the COMPLEX <scale>
 * argument. So, for example, to return a path that would be the reflection of
 * a path across an imaginary vertical line, <scale> == #C(-1.0 1.0).
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_scale (path,dx,dy) 
 *    TANGO_PATH path;
 *    double dx, dy;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_SCALE()
{
  LVAL lval_path;
  TANGO_PATH path;
  LVAL scale;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  scale = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <scale> */
  xllastarg();

  return (cv_tangopath(TANGOpath_scale(path,
				       (double) getflonum(getelement(scale, 0)),
				       (double) getflonum(getelement(scale, 1)))));
}


/******************************************************************************
 * (TANGO:PATH_EXTEND <path> <extension>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<extension> is a COMPLEX number #C(<dx> <dy>) where <dx> <dy> are FLONUMs.
 * 
 * This routine returns a path in which each offset is extended by the given
 * factors in X and Y.  That is, each offsets' X and Y values have the
 * respective <dx> and <dy> factors (specified in the COMPLEX <extension>)
 * added to them.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_extend (path,dx,dy) 
 *    TANGO_PATH path;
 *    double dx, dy;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_EXTEND()
{
  LVAL lval_path;
  TANGO_PATH path;
  LVAL extension;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  extension = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <extension> */
  xllastarg();

  return (cv_tangopath(TANGOpath_extend(path,
					(double) getflonum(getelement(extension, 0)),
					(double) getflonum(getelement(extension, 1)))));
}


/******************************************************************************
 * (TANGO:PATH_COPY <path>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * This routine returns a new path that has an exact duplicate list of offsets
 * as the given <path>.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_copy (path) 
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_COPY()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cv_tangopath(TANGOpath_copy(path)));
}


/******************************************************************************
 * (TANGO:PATH_NULL <num>)
 *	==> returns a TANGO_PATH node.
 *
 * TANGO:PATH_CREATE with no args or a FIXNUM arg is the same as TANGOpath_null(),
 * therefore, no separate primitive TANGO:PATH_NULL is needed...
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_null (num) 
 *    int num;
 ******************************************************************************/


/******************************************************************************
 * (TANGO:PATH_ITERATE <path> <factor>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *	<factor> is a FLONUM
 * 
 * This routine returns a path which is an iteration of a given <path>.  The
 * <factor> FLONUM parameter provides how many times the given path should be
 * repeated in the path to be created.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_iterate (path,factor) 
 *    TANGO_PATH path;
 *    double factor;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_ITERATE()
{
  LVAL lval_path;
  TANGO_PATH path;
  double factor;

  path   = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  factor = Xtango_Get_Positive_FLONUM_Arg(); /* get <factor> */
  xllastarg();

  return (cv_tangopath(TANGOpath_iterate(path, factor)));
}


#define TANGO_PATH_ARG_SEQUENCE_SIZE 50	/* see paths[50] decl in TANGOpath_concatenate() and TANGOpath_compose() */


/******************************************************************************
 * (TANGO:PATH_CONCATENATE <path-0> <path-1> <path-2> ...)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path-i> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * This routine creates a path which is the concatenation of some number of
 * other paths.  The paths will be concatenated in order from the <path-0>
 * position on to the last <path-n> position. The first offset of a path is
 * relative to the last offset of the previous path.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_concatenate (num,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) 
 *    int num;
 *    TANGO_PATH p1,p2,p3,p4,p5,p6,p7,p8,p9,p10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_CONCATENATE()
{
  TANGO_PATH paths[TANGO_PATH_ARG_SEQUENCE_SIZE];
  LVAL arg;
  int i;

  paths[0] = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&arg); /* get <path-0> arg */
  i = 0;

  while (moreargs()) {		/* retrieve <path-1> ... <path-19> */
    arg = nextarg();		/* retrieve an arg from argument stack */
    i++;
    if (tangopath_p(arg)) {	/* check to make sure it's a TANGO_PATH */
      if (i >= TANGO_PATH_ARG_SEQUENCE_SIZE)
	xlerror("Maximum number of TANGO_PATH arguments exceeded.", arg);
      if ((paths[i] = get_tangopath(arg)) == (TANGO_PATH) NULL)
	Xtango_Error_Freed_TANGO_PATH(arg);
    }
    else
      xlbadtype(arg);
  }

  switch (i) {			/* varargs suck! ... but gnu-emacs keyboard macros rule */
  case 0:			/* only gets exec'd if while() loop above never got called */
    return (cv_tangopath(TANGOpath_concatenate(1,
       paths[0])));
    break;
  case 1:
    return (cv_tangopath(TANGOpath_concatenate(2,
       paths[0], paths[1])));
    break;
  case 2:
    return (cv_tangopath(TANGOpath_concatenate(3,
       paths[0], paths[1], paths[2])));
    break;
  case 3:
    return (cv_tangopath(TANGOpath_concatenate(4,
       paths[0], paths[1], paths[2], paths[3])));
    break;
  case 4:
    return (cv_tangopath(TANGOpath_concatenate(5,
       paths[0], paths[1], paths[2], paths[3], paths[4])));
    break;
  case 5:
    return (cv_tangopath(TANGOpath_concatenate(6,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5])));
    break;
  case 6:
    return (cv_tangopath(TANGOpath_concatenate(7,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6])));
    break;
  case 7:
    return (cv_tangopath(TANGOpath_concatenate(8,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7])));
    break;
  case 8:
    return (cv_tangopath(TANGOpath_concatenate(9,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8])));
    break;
  case 9:
    return (cv_tangopath(TANGOpath_concatenate(10,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9])));
    break;
  case 10:
    return (cv_tangopath(TANGOpath_concatenate(11,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10])));
    break;
  case 11:
    return (cv_tangopath(TANGOpath_concatenate(12,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11])));
    break;
  case 12:
    return (cv_tangopath(TANGOpath_concatenate(13,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12])));
    break;
  case 13:
    return (cv_tangopath(TANGOpath_concatenate(14,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13])));
    break;
  case 14:
    return (cv_tangopath(TANGOpath_concatenate(15,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14])));
    break;
  case 15:
    return (cv_tangopath(TANGOpath_concatenate(16,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15])));
    break;
  case 16:
    return (cv_tangopath(TANGOpath_concatenate(17,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16])));
    break;
  case 17:
    return (cv_tangopath(TANGOpath_concatenate(18,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17])));
    break;
  case 18:
    return (cv_tangopath(TANGOpath_concatenate(19,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18])));
    break;
  case 19:
    return (cv_tangopath(TANGOpath_concatenate(20,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19])));
    break;
  case 20:
    return (cv_tangopath(TANGOpath_concatenate(21,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20])));
    break;
  case 21:
    return (cv_tangopath(TANGOpath_concatenate(22,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21])));
    break;
  case 22:
    return (cv_tangopath(TANGOpath_concatenate(23,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22])));
    break;
  case 23:
    return (cv_tangopath(TANGOpath_concatenate(24,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23])));
    break;
  case 24:
    return (cv_tangopath(TANGOpath_concatenate(25,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24])));
    break;
  case 25:
    return (cv_tangopath(TANGOpath_concatenate(26,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25])));
    break;
  case 26:
    return (cv_tangopath(TANGOpath_concatenate(27,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26])));
    break;
  case 27:
    return (cv_tangopath(TANGOpath_concatenate(28,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27])));
    break;
  case 28:
    return (cv_tangopath(TANGOpath_concatenate(29,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28])));
    break;
  case 29:
    return (cv_tangopath(TANGOpath_concatenate(30,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29])));
    break;
  case 30:
    return (cv_tangopath(TANGOpath_concatenate(31,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30])));
    break;
  case 31:
    return (cv_tangopath(TANGOpath_concatenate(32,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31])));
    break;
  case 32:
    return (cv_tangopath(TANGOpath_concatenate(33,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32])));
    break;
  case 33:
    return (cv_tangopath(TANGOpath_concatenate(34,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33])));
    break;
  case 34:
    return (cv_tangopath(TANGOpath_concatenate(35,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34])));
    break;
  case 35:
    return (cv_tangopath(TANGOpath_concatenate(36,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35])));
    break;
  case 36:
    return (cv_tangopath(TANGOpath_concatenate(37,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36])));
    break;
  case 37:
    return (cv_tangopath(TANGOpath_concatenate(38,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37])));
    break;
  case 38:
    return (cv_tangopath(TANGOpath_concatenate(39,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38])));
    break;
  case 39:
    return (cv_tangopath(TANGOpath_concatenate(40,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39])));
    break;
  case 40:
    return (cv_tangopath(TANGOpath_concatenate(41,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40])));
    break;
  case 41:
    return (cv_tangopath(TANGOpath_concatenate(42,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41])));
    break;
  case 42:
    return (cv_tangopath(TANGOpath_concatenate(43,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42])));
    break;
  case 43:
    return (cv_tangopath(TANGOpath_concatenate(44,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43])));
    break;
  case 44:
    return (cv_tangopath(TANGOpath_concatenate(45,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44])));
    break;
  case 45:
    return (cv_tangopath(TANGOpath_concatenate(46,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45])));
    break;
  case 46:
    return (cv_tangopath(TANGOpath_concatenate(47,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46])));
    break;
  case 47:
    return (cv_tangopath(TANGOpath_concatenate(48,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47])));
    break;
  case 48:
    return (cv_tangopath(TANGOpath_concatenate(49,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47], paths[48])));
    break;
  case 49:
    return (cv_tangopath(TANGOpath_concatenate(50,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47], paths[48], paths[49])));
    break;
  default:
    xlfail("");
    break;
  }
}


/******************************************************************************
 * (TANGO:PATH_COMPOSE <path-0> <path-1> <path-2> ...)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path-i> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * This routine returns a path which is the composition of the paths given in
 * the <path-0> to <path-n> parameters.  By composition, we mean a cumulative
 * combination on an offset by offset basis. If a path is thought of as a
 * vector, the composition of paths produces a new vector that has the same
 * length as the originals and is like the vector sum of the original path
 * vectors.  Only paths with an equal number of offsets can be composed.
 * Otherwise, an error is signalled.
 * 
 * Essentially, path composition takes all the first relative <x> <y> offsets
 * and adds them together to make a new cumulative first <x>,<y> offset. This
 * is done for each offset in the paths.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_compose (num,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) 
 *    int num;
 *    TANGO_PATH p1,p2,p3,p4,p5,p6,p7,p8,p9,p10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_COMPOSE()
{
  TANGO_PATH paths[TANGO_PATH_ARG_SEQUENCE_SIZE];
  LVAL arg;
  int count, i;

  paths[0] = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&arg); /* get <path-0> arg so we can find number of offsets in path */
  count = paths[0]->count;
  i = 0;

  while (moreargs()) {		/* retrieve <path-1> ... <path-19> */
    arg = nextarg();		/* retrieve an arg from argument stack */
    i++;
    if (tangopath_p(arg)) {	/* check to make sure it's a TANGO_PATH */
      if (i >= TANGO_PATH_ARG_SEQUENCE_SIZE)
	xlerror("Maximum number of TANGO_PATH arguments exceeded.", arg);
      if ((paths[i] = get_tangopath(arg)) == (TANGO_PATH) NULL)
	Xtango_Error_Freed_TANGO_PATH(arg);
      if (paths[i]->count != count)
	xlerror("All TANGO_PATH arguments must have same number of offsets.", arg);
    }
    else
      xlbadtype(arg);
  }

  switch (i) {			/* varargs suck! ... but gnu-emacs keyboard macros rule */
  case 0:			/* only gets exec'd if while() loop above never got called */
    return (cv_tangopath(TANGOpath_compose(1,
       paths[0])));
    break;
  case 1:
    return (cv_tangopath(TANGOpath_compose(2,
       paths[0], paths[1])));
    break;
  case 2:
    return (cv_tangopath(TANGOpath_compose(3,
       paths[0], paths[1], paths[2])));
    break;
  case 3:
    return (cv_tangopath(TANGOpath_compose(4,
       paths[0], paths[1], paths[2], paths[3])));
    break;
  case 4:
    return (cv_tangopath(TANGOpath_compose(5,
       paths[0], paths[1], paths[2], paths[3], paths[4])));
    break;
  case 5:
    return (cv_tangopath(TANGOpath_compose(6,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5])));
    break;
  case 6:
    return (cv_tangopath(TANGOpath_compose(7,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6])));
    break;
  case 7:
    return (cv_tangopath(TANGOpath_compose(8,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7])));
    break;
  case 8:
    return (cv_tangopath(TANGOpath_compose(9,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8])));
    break;
  case 9:
    return (cv_tangopath(TANGOpath_compose(10,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9])));
    break;
  case 10:
    return (cv_tangopath(TANGOpath_compose(11,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10])));
    break;
  case 11:
    return (cv_tangopath(TANGOpath_compose(12,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11])));
    break;
  case 12:
    return (cv_tangopath(TANGOpath_compose(13,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12])));
    break;
  case 13:
    return (cv_tangopath(TANGOpath_compose(14,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13])));
    break;
  case 14:
    return (cv_tangopath(TANGOpath_compose(15,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14])));
    break;
  case 15:
    return (cv_tangopath(TANGOpath_compose(16,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15])));
    break;
  case 16:
    return (cv_tangopath(TANGOpath_compose(17,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16])));
    break;
  case 17:
    return (cv_tangopath(TANGOpath_compose(18,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17])));
    break;
  case 18:
    return (cv_tangopath(TANGOpath_compose(19,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18])));
    break;
  case 19:
    return (cv_tangopath(TANGOpath_compose(20,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19])));
    break;
  case 20:
    return (cv_tangopath(TANGOpath_compose(21,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20])));
    break;
  case 21:
    return (cv_tangopath(TANGOpath_compose(22,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21])));
    break;
  case 22:
    return (cv_tangopath(TANGOpath_compose(23,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22])));
    break;
  case 23:
    return (cv_tangopath(TANGOpath_compose(24,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23])));
    break;
  case 24:
    return (cv_tangopath(TANGOpath_compose(25,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24])));
    break;
  case 25:
    return (cv_tangopath(TANGOpath_compose(26,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25])));
    break;
  case 26:
    return (cv_tangopath(TANGOpath_compose(27,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26])));
    break;
  case 27:
    return (cv_tangopath(TANGOpath_compose(28,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27])));
    break;
  case 28:
    return (cv_tangopath(TANGOpath_compose(29,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28])));
    break;
  case 29:
    return (cv_tangopath(TANGOpath_compose(30,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29])));
    break;
  case 30:
    return (cv_tangopath(TANGOpath_compose(31,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30])));
    break;
  case 31:
    return (cv_tangopath(TANGOpath_compose(32,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31])));
    break;
  case 32:
    return (cv_tangopath(TANGOpath_compose(33,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32])));
    break;
  case 33:
    return (cv_tangopath(TANGOpath_compose(34,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33])));
    break;
  case 34:
    return (cv_tangopath(TANGOpath_compose(35,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34])));
    break;
  case 35:
    return (cv_tangopath(TANGOpath_compose(36,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35])));
    break;
  case 36:
    return (cv_tangopath(TANGOpath_compose(37,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36])));
    break;
  case 37:
    return (cv_tangopath(TANGOpath_compose(38,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37])));
    break;
  case 38:
    return (cv_tangopath(TANGOpath_compose(39,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38])));
    break;
  case 39:
    return (cv_tangopath(TANGOpath_compose(40,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39])));
    break;
  case 40:
    return (cv_tangopath(TANGOpath_compose(41,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40])));
    break;
  case 41:
    return (cv_tangopath(TANGOpath_compose(42,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41])));
    break;
  case 42:
    return (cv_tangopath(TANGOpath_compose(43,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42])));
    break;
  case 43:
    return (cv_tangopath(TANGOpath_compose(44,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43])));
    break;
  case 44:
    return (cv_tangopath(TANGOpath_compose(45,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44])));
    break;
  case 45:
    return (cv_tangopath(TANGOpath_compose(46,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45])));
    break;
  case 46:
    return (cv_tangopath(TANGOpath_compose(47,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46])));
    break;
  case 47:
    return (cv_tangopath(TANGOpath_compose(48,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47])));
    break;
  case 48:
    return (cv_tangopath(TANGOpath_compose(49,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47], paths[48])));
    break;
  case 49:
    return (cv_tangopath(TANGOpath_compose(50,
       paths[0], paths[1], paths[2], paths[3], paths[4],
       paths[5], paths[6], paths[7], paths[8], paths[9],
       paths[10], paths[11], paths[12], paths[13], paths[14],
       paths[15], paths[16], paths[17], paths[18], paths[19],
       paths[20], paths[21], paths[22], paths[23], paths[24],
       paths[25], paths[26], paths[27], paths[28], paths[29],
       paths[30], paths[31], paths[32], paths[33], paths[34],
       paths[35], paths[36], paths[37], paths[38], paths[39],
       paths[40], paths[41], paths[42], paths[43], paths[44],
       paths[45], paths[46], paths[47], paths[48], paths[49])));
    break;
  default:
    xlfail("");
    break;
  }
}


/******************************************************************************
 * (TANGO:PATH_DISTANCE <from_loc> <to_loc> <distance>)
 *	==> returns a TANGO_PATH node.
 * 
 *	<from_loc> is a COMPLEX number #C(<from_x> <from_y>), 
 *		where <from_x>, <from_y> are FLONUMs.
 *	<to_loc> is a COMPLEX number #C(<to_x> <to_y>),
 *		 where <to_x>, <to_y> are FLONUMs.
 *	<distance> is a FLONUM.
 *
 * This routine returns a path that proceeds in a straight line from the given
 * <from_loc> to the given <to_loc> with the condition that the path will have
 * an offset every time the given <distance> has been covered.  If the given
 * distance is larger than the distance between the <from_loc> and <to_loc>,
 * the path is only given one offset.  This routine is useful to create paths
 * that will make images move at the same velocity.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_distance (fromloc,toloc,distance) 
 *    TANGO_LOC fromloc;
 *    TANGO_LOC toloc;
 *    double distance;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_DISTANCE()
{
  TANGO_PATH path;
  LVAL from_loc, to_loc;
  TANGO_LOC fromloc, toloc;
  double distance;

  from_loc = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <from_loc> */
  to_loc   = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <to_loc> */
  distance = Xtango_Get_Positive_FLONUM_Arg(); /* get <distance> */
  xllastarg();

  fromloc = TANGOloc_create((double) getflonum(getelement(from_loc, 0)), 
			    (double) getflonum(getelement(from_loc, 1)));
  toloc   = TANGOloc_create((double) getflonum(getelement(to_loc, 0)),
			    (double) getflonum(getelement(to_loc, 1)));

  path = TANGOpath_distance(fromloc, toloc, distance);

  XtFree((char*) fromloc);
  XtFree((char*) toloc);

  return (cv_tangopath(path));
}


/******************************************************************************
 * (TANGO:PATH_EXAMPLE <from_loc> <to_loc> <path>)
 *	==> returns a TANGO_PATH node.
 * 
 *	<from_loc> is a COMPLEX number #C(<from_x> <from_y>), 
 *		where <from_x>, <from_y> are FLONUMs.
 *	<to_loc> is a COMPLEX number #C(<to_x> <to_y>),
 *		 where <to_x>, <to_y> are FLONUMs.
 *	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 *
 * This routine returns a path which "looks like" the given example <path>, but
 * which would move from the logical <from_loc> to the logical <to_loc>.
 * The created path will have the same number of offsets as the given path.
 * The general flow of movement in the example path is followed as closely as
 * possible by maintaining the same ratios between control points in both
 * paths. Clearly, however, if the two paths move in opposite directions, they
 * will not look much alike.  Typically, this routine will be used when one
 * wants an image to end up in some specific position, with the image
 * following some rough, given trajectory path in order to get there.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_example (fromloc,toloc,path) 
 *    TANGO_LOC fromloc;
 *    TANGO_LOC toloc;
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_EXAMPLE()
{
  LVAL lval_path;
  TANGO_PATH path;
  LVAL from_loc, to_loc;
  TANGO_LOC fromloc, toloc;

  from_loc = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <from_loc> */
  to_loc   = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <to_loc> */
  path     = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  fromloc = TANGOloc_create((double) getflonum(getelement(from_loc, 0)), 
			    (double) getflonum(getelement(from_loc, 1)));
  toloc   = TANGOloc_create((double) getflonum(getelement(to_loc, 0)),
			    (double) getflonum(getelement(to_loc, 1)));

  path = TANGOpath_example(fromloc, toloc, path); 

  XtFree((char*) fromloc);
  XtFree((char*) toloc);

  return (cv_tangopath(path));
}


/******************************************************************************
 * (TANGO:PATH_MOTION <from_loc> <to_loc> <path_type>)
 *	==> returns a TANGO_PATH node.
 * 
 *	<from_loc> is a COMPLEX number #C(<from_x> <from_y>), 
 *		where <from_x>, <from_y> are FLONUMs.
 *	<to_loc> is a COMPLEX number #C(<to_x> <to_y>),
 *		 where <to_x>, <to_y> are FLONUMs.
 *	<path_type> is a keyword, either :STRAIGHT,
 *		:CLOCKWISE, or :COUNTERCLOCKWISE.
 * 
 * This routine returns a path with movement characteristics of the given
 * <path_type>, but that also begins at the location with logical X and Y
 * coordinates of <from_loc> and proceeds to the location with logical X and Y
 * coordinates of the <to_loc>.  Note that the provided from and to locations
 * need not have absolute screen coordinate meaning.  Because a path is made
 * up of a group of relative offsets, these locations are provided just as
 * necessary aids in the path creation.  For example, the same path will be
 * created using from == #C(0.2 0.2) and to == #C(0.4 0.4) as using
 * from == #C(0.7 0.7) and to == #C(0.9 0.9). Most often however, the provided
 * locations will be specific window coordinates chosen to move a certain
 * image to a specific location.
 * 
 * The created path will contain 20 offsets and will be modelled to fit the
 * given <path_type>, which may be one of the following keywords :STRAIGHT,
 * :CLOCKWISE, or :COUNTERCLOCKWISE.  The straight option creates a path that
 * is a direct line from {\em fromloc} to {\em toloc} with equally spaced
 * offsets.  The two clock motions will create a path that curves in the given
 * clock motion direction.
 * 
 * Essentially, this routine is just
 * (TANGO:PATH_EXAMPLE <from_loc> <to_loc> (TANGO:PATH_TYPE <path_type>)).
 * ------------------------------------------------------------------------------
 * TANGOpath_motion (fromloc,toloc,type) 
 *    TANGO_LOC fromloc;
 *    TANGO_LOC toloc;
 *    TANGO_PATH_TYPE type;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_MOTION()
{
  TANGO_PATH path;
  LVAL from_loc, to_loc;
  TANGO_LOC fromloc, toloc;
  TANGO_PATH_TYPE type;

  from_loc = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <from_loc> */
  to_loc   = Xtango_Get_FLONUM_COMPLEX_Arg(); /* get <to_loc> */
  type     = Xtango_Get_TANGO_PATH_TYPE_Arg(); /* get <path_type> */
  xllastarg();

  fromloc = TANGOloc_create((double) getflonum(getelement(from_loc, 0)), 
			    (double) getflonum(getelement(from_loc, 1)));
  toloc   = TANGOloc_create((double) getflonum(getelement(to_loc, 0)),
			    (double) getflonum(getelement(to_loc, 1)));

  path = TANGOpath_motion(fromloc, toloc, type);

  XtFree((char*) fromloc);
  XtFree((char*) toloc);

  return (cv_tangopath(path));
}


/******************************************************************************
 * (TANGO:PATH_SMOOTH <path>)
 *	==> returns a TANGO_PATH node.
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * 
 * This routine returns a path which is a "smoother" version of the given
 * <path>.  Essentially, each offsets' value is determined by averaging in the
 * neighboring offsets' values. Currently, we use 2 times the designated
 * offset plus one time the previous and subsequent offsets.
 * ------------------------------------------------------------------------------
 * TANGO_PATH
 * TANGOpath_smooth (path) 
 *    TANGO_PATH path;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_SMOOTH()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path); /* get <path> */
  xllastarg();

  return (cv_tangopath(TANGOpath_smooth(path)));
}


/******************************************************************************
 * (TANGO:PATH_FREE <path>)
 *	==> returns NIL
 * 
 * 	<path> is a TANGO_PATH as returned by other TANGO:PATH_* routines.
 * 
 * This routine frees the memory space occupied by the TANGO_PATH arg <path>.
 *
 * Note that WINTERP automatically frees any TANGO_PATH node which is no longer
 * referenced. If you happen to store a TANGO_PATH on a global variable, or
 * within a closure (on function, method, lambda, etc) it will not get garbage
 * collected, so you may want to use this function to explicitly free it.
 *
 * If very long paths are being created, but garbage collections are happening
 * infrequently, the process size will grow larger than it needs to be
 * because memory taken up by TANGO_PATH's will not be able to be reused until
 * a garbage collection occurs.
 *
 * Calling TANGO:PATH_FREE frees the Xtango storage used by <path>. Subsequent
 * references of the node passed to this function via <path> will signal an error
 * indicating that the path has been freed. This might happen, for example, when
 * you use the <path> as an argument to a :TX_* TANGO_IMAGE_CLASS method.
 * ------------------------------------------------------------------------------
 * TANGOpath_free (num,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) 
 *    int num;
 *    TANGO_PATH p1,p2,p3,p4,p5,p6,p7,p8,p9,p10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_PATH_FREE()
{
  LVAL lval_path;
  TANGO_PATH path;

  path = Xtango_Get_Path_Arg_Returning_Validated_TANGO_PATH(&lval_path);
  xllastarg();
  path_free(path);
  set_tangopath(lval_path, (TANGO_PATH) NULL);
  return (NIL);
}
