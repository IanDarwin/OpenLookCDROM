/* -*-C-*-
********************************************************************************
*
* File:         t_utils.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/t_utils.c,v 2.6 1994/06/06 15:41:11 npm Exp $
* Description:  Utilities relating to WINTERP's Xtango interface. See
*		also tango.h, which is the header file for this file.
* Author:       Niels P. Mayer
* Created:      Tue Mar 23 02:11:05 1993
* Modified:     Sun Jun  5 14:22:47 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/t_utils.c,v 2.6 1994/06/06 15:41:11 npm Exp $";

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

#include <Xm/Xm.h>
#include "winterp.h"
#include "tango.h"

char string_err_msg_bad_loc_coord[]
  = "Bad tango image location coordinate --\n       expected COMPLEX #C(<x-flonum> <y-flonum>)";

char string_err_msg_bad_radius_coord[]
  = "Bad tango circle/ellipse radius coordinate --\n       expected either FLONUM or COMPLEX #C(<x-radius-flonum> <y-radius-flonum>)";

char string_err_msg_bad_color[]
  = "Bad tango image color --\n       expected either FIXNUM (TANGO_COLOR) or STRING";

char string_err_msg_bad_fill[]
  = "Bad tango image fill value --\n       expected FLONUM [0.0-1.0]";

char string_err_msg_bad_image_class[]
  = "Bad tango image class --\n       expected a Class OBJECT, subclass of TANGO:IMAGE_CLASS";

char string_err_msg_bad_size_coord[]
  = "Bad tango image size coordinate --\n       expected a COMPLEX #C(<x-size-fixnum> <y-size-fixnum>)";

char string_err_msg_bad_line_width[]
  = "Bad tango line width --\n       expected a FLONUM [0.0-1.0]";

char string_err_msg_bad_line_style[]
  = "Bad tango line style --\n       expected a FLONUM [0.0-1.0]";

char string_err_msg_bad_line_arrow[]
  = "Bad tango line arrow kind --\n       expected one of [:NO_ARROW, :FORW_ARROW, :BACK_ARROW, :BOTH_ARROW]";

char string_err_msg_bad_poly_offset[]
  = "Bad tango polygon/polyline/spline offset --\n       expected a COMPLEX #C(<x-vert-offset-fixnum> <y-vert-offset-fixnum>)";

char string_err_msg_too_many_poly_offsets[]
  = "Too many tango polygon/polyline/spline vertices --\n       8 vertices maximum";

char string_err_msg_too_few_poly_offsets[]
  = "Too few tango polygon/polyline/spline vertices --\n       2 vertices minimum";

char string_err_msg_bad_text[]
  = "Bad tango image text --\n       expected a STRING";

char string_err_msg_bad_fontstr[]
  = "Bad tango text image font specification --\n       expected a STRING or NIL";

char string_err_msg_bad_bitmap_vector[]
  = "Bad tango bitmap movie image value --\n       Expected Xtango bitmap-vector (a 3D array of TANGO_COLOR)";

char string_err_msg_bad_bitmap[]
  = "Bad tango bitmap image value --\n       Expected Xtango bitmap (a 2D array of TANGO_COLOR)";

char string_err_msg_bad_bitmap_row[]
  = "Bad tango bitmap movie image value --\n       Expected Xtango bitmap row-vector (an array of TANGO_COLOR)";

char string_err_msg_bad_bitmap_array_elt[]
  = "Bad tango bitmap movie image value --\n       Expected Xtango bitmap element of type FIXNUM (a TANGO_COLOR)";

char string_err_msg_too_many_bitmaps[]
  = "Tango bitmap image vector must contain <= 20 bitmaps";

char string_err_msg_bitmaps_of_different_heights[]
  = "Tango bitmap image vector contains bitmaps of different heights";

char string_err_msg_bitmaps_of_different_widths[]
  = "Tango bitmap image vector contains bitmaps of different widths";

char string_err_msg_bad_gif_fname[]
  = "Bad tango GIF filepath --\n       Expected a STRING";

char string_err_msg_bad_gif_verbose_arg[]
  = "Bad tango GIF verbose load argument  --\n       Expected keyword :VERBOSE";


/******************************************************************************
 * For WINTERP, xtango/xtangolocal.h #defines COMPLAIN as Xtango_COMPLAIN().
 * COMPLAIN is normally #defined as printf(), but here we want to ensure that
 * error messages from Xtango go to *error-output* via errputstr().
 ******************************************************************************/
void Xtango_COMPLAIN
#ifndef _NO_PROTO /* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *fmt, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
  va_list args;
#ifndef _NO_PROTO /* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
  va_start(args, fmt);
#else  /* defined(_NO_PROTO) */
  char *fmt;
  va_start(args);
  fmt = va_arg(args, char *);
#endif /* !defined(_NO_PROTO) ==> ANSI */

  (void) vsprintf(temptext, fmt, args);
  errputstr(temptext);
  va_end(args);
}


/******************************************************************************
 * For WINTERP, xtango/xtangolocal.h #defines DEBUG as "... Xtango_DEBUG_TRACE_PRINT()."
 * DEBUG is normally #defined as "... printf()", but here we want to ensure that
 * error messages from Xtango go to *trace-output* via trcputstr().
 ******************************************************************************/
void Xtango_DEBUG_TRACE_PRINT
#ifndef _NO_PROTO /* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *fmt, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
  va_list args;
#ifndef _NO_PROTO /* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
  va_start(args, fmt);
#else  /* defined(_NO_PROTO) */
  char *fmt;
  va_start(args);
  fmt = va_arg(args, char *);
#endif /* !defined(_NO_PROTO) ==> ANSI */

  (void) vsprintf(temptext, fmt, args);
  trcputstr(temptext);
  va_end(args);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a TANGO_COLOR argument,
 * e.g. all TANGO:*_IMAGE_CLASS :NEW methods.
 *
 * NOTE THAT THIS ROUTINE EXPECTS THAT YOU'VE SET UP XTANGO'S CONTEXT VIA
 * A PRIOR CALL TO Xtango_Save_Set_Context(). TYPICALLY, THEREFORE, YOU'LL
 * NEED TO PULL ALL THE ARGS OFF THE ARG-STACK, CALL Xtango_Save_Set_Context(),
 * AND THEN FINALLY CALL THIS ROUTINE.
 *
 * If <lval_color> is a STRING, then TANGOload_color() is called to convert
 * the value to a TANGO_COLOR. If <lval_color> is a FIXNUM, then the
 * value is checked to ensure that it is in the valid range for an Xtango
 * color, and that value is returned as TANGO_COLOR.
 ******************************************************************************/
TANGO_COLOR Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error(lval_color)
     LVAL lval_color;		/* STRING or FIXNUM */
{
  TANGO_COLOR tango_color;

  switch (ntype(lval_color)) {
  case STRING:
    /*
     * note that for Xtango_Load_Color_Else_Error() is a special version of
     * TANGOload_color() which will signal an error when Xtango runs out of
     * colors, or when XAllocNamedColor() fails. For monochrome systems,
     * this will only issue a warning to *error-output*.
     */
    tango_color = Xtango_Load_Color_Else_Error(getstring(lval_color)); /* this must occur after Xtango_Save_Set_Context() */
    break;
  case FIXNUM:
    tango_color = (TANGO_COLOR) getfixnum(lval_color);
    if (Xtango_Check_Invalid_Color(tango_color)) {
      Xtango_Restore_Context();
      xlerror("Invalid TANGO_COLOR pixel value.", lval_color); 
    }
    break;
  default:
    Xtango_Restore_Context();
    xlbadtype(lval_color); 
    break;
  }

  return (tango_color);
}


/*******************************************************************************
 * This routine similar to Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error() 
 * except that this returns the default color (without signalling error) if 
 * the given LVAL (string/fixnum) couldn't be converted to a valid tango color.
 *******************************************************************************/
TANGO_COLOR Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color)
     LVAL lval_color;		/* STRING or FIXNUM */
{
  TANGO_COLOR tango_color;

  switch (ntype(lval_color)) {
  case STRING:
    tango_color = TANGOload_color(getstring(lval_color)); /* this must occur after Xtango_Save_Set_Context() */
    break;
  case FIXNUM:
    tango_color = (TANGO_COLOR) getfixnum(lval_color);
    if (Xtango_Check_Invalid_Color(tango_color))
      tango_color = TANGO_COLOR_BLACK; /* default value... */
    break;
  default:
    tango_color = TANGO_COLOR_BLACK; /* default value... */
    break;
  }

  return (tango_color);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a positive integer arg.
 ******************************************************************************/
int Xtango_Get_Positive_FIXNUM_Arg()
{
  int num;
  LVAL lval_num;

  if ((num = (int) getfixnum(lval_num = xlgafixnum())) <= 0)
    xlerror("Expected a positive integer.", lval_num);
  else
    return (num);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a positive FLONUM arg.
 ******************************************************************************/
double Xtango_Get_Positive_FLONUM_Arg()
{
  double value;
  LVAL   lval;

  value = (double) getflonum((lval = xlgaflonum()));
  if (value <= 0.0)
    xlerror("FLONUM argument value must be > 0.0.", lval);
  else
    return (value);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a COMPLEX arg consisting
 * of a pair of FLONUMs. The fact that the COMPLEX contains FLONUMs must be
 * verified before going off and calling getflonum(getelement(arg, i)).
 ******************************************************************************/
LVAL Xtango_Get_FLONUM_COMPLEX_Arg()
{
  LVAL arg = xlgetarg();	
  if (complexp(arg) && floatp(getelement(arg, 0)) && floatp(getelement(arg, 1)))
    return (arg);
  else 
    Xtango_Error_Bad_FLONUM_COMPLEX_Arg(arg);
}


/******************************************************************************
 * We wrap xlerror() here because we don't want multiple copies of same error
 * message in the binary.
 ******************************************************************************/
void Xtango_Error_Bad_FLONUM_COMPLEX_Arg(arg)
     LVAL arg;
{
  xlerror("Bad type -- Expected a COMPLEX (pair of FLONUM) number.", arg);
}


/******************************************************************************
 * A helper routine used by prims/methods which take optional :PERFORM kwd.
 ******************************************************************************/
static LVAL k_PERFORM;		/* init'd below by T_Utils_Init() */
Boolean Xtango_Get_Optional_PERFORM_Kwd_Arg()
{
  if (moreargs() && (*xlargv == k_PERFORM)) { /* get optional :PERFORM arg */
    nextarg();
    return (TRUE);
  }
  else
    return (FALSE);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a TANGO_PATH_TYPE keyword
 * argument.
 ******************************************************************************/
static LVAL k_STRAIGHT, k_CLOCKWISE, k_COUNTERCLOCKWISE; /* init'd in T_Utils_Init() */
TANGO_PATH_TYPE Xtango_Get_TANGO_PATH_TYPE_Arg()
{
  LVAL lval_type;

  lval_type = xlgetarg();

  if (lval_type == k_STRAIGHT)
    return (TANGO_PATH_TYPE_STRAIGHT);
  else if (lval_type == k_CLOCKWISE)
    return (TANGO_PATH_TYPE_CLOCKWISE);
  else if (lval_type == k_COUNTERCLOCKWISE)
    return (TANGO_PATH_TYPE_COUNTERCLOCKWISE);
  else
    xlerror("Invalid tango path type", lval_type);
}


/******************************************************************************
 * A helper routine used by prims/methods which take a TANGO_PART_TYPE keyword
 * argument.
 ******************************************************************************/
LVAL k_TANGO_PART_TYPE_C;	/* this keyword also happens to be used by Xtango_Get_Text_Image_Args() */
static LVAL k_TANGO_PART_TYPE_NW, k_TANGO_PART_TYPE_N, /* init'd in T_Utils_Init() */
  k_TANGO_PART_TYPE_NE, k_TANGO_PART_TYPE_E, k_TANGO_PART_TYPE_SE,
  k_TANGO_PART_TYPE_S, k_TANGO_PART_TYPE_SW, k_TANGO_PART_TYPE_W;
TANGO_PART_TYPE Xtango_Get_TANGO_PART_TYPE_Arg()
{
  LVAL lval_type;

  lval_type = xlgetarg();

  if (lval_type == k_TANGO_PART_TYPE_C)
    return (TANGO_PART_TYPE_C);
  else if (lval_type == k_TANGO_PART_TYPE_NW)
    return (TANGO_PART_TYPE_NW);
  else if (lval_type == k_TANGO_PART_TYPE_N)
    return (TANGO_PART_TYPE_N);
  else if (lval_type == k_TANGO_PART_TYPE_NE)
    return (TANGO_PART_TYPE_NE);
  else if (lval_type == k_TANGO_PART_TYPE_E)
    return (TANGO_PART_TYPE_E);
  else if (lval_type == k_TANGO_PART_TYPE_SE)
    return (TANGO_PART_TYPE_SE);
  else if (lval_type == k_TANGO_PART_TYPE_S)
    return (TANGO_PART_TYPE_S);
  else if (lval_type == k_TANGO_PART_TYPE_SW)
    return (TANGO_PART_TYPE_SW);
  else if (lval_type == k_TANGO_PART_TYPE_W)
    return (TANGO_PART_TYPE_W);
  else
    xlerror("Invalid tango part type", lval_type);
}


/******************************************************************************
 * random utility -- must call after Xtango_Save_Set_Context_From_WidgetID()
 * or Xtango_Save_Set_Context()
 ******************************************************************************/
void Xtango_Check_Initialized()
{
  if (!TANGO__data) {
    Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
    xlfail("Must initialize with TANGO:WIDGET_CLASS-:BEGIN_DRAWING before calling this operation...");
  }
}

static WINTERP_TANGO_CONTEXT tango_widget_alist = (WINTERP_TANGO_CONTEXT) NULL;

/*****************************************************************************
 *
 ****************************************************************************/
WINTERP_TANGO_CONTEXT Xtango_Find_Context_From_WidgetID(widgetID)
     Widget widgetID;
{
  register WINTERP_TANGO_CONTEXT cur;

  if (!tango_widget_alist)	/* ASSERT(tango_widget_alist!=NULL): something has gone awry if this callbackproc got called but tango_widget_alist is empty. */
    xlfatal("Internal error 0 in Xtango_Find_Context_From_WidgetID().");

  cur = tango_widget_alist;
  while (cur && (cur->widget_ID != widgetID)) {
    cur = cur->next;
  }
  if (cur) {			/* cur->adata->easel == widgetID */
    return (cur);
  }
  else				/* ASSERT(cur!=NULL): something has gone awry if we can't find WINTERP_TANGO_CONTEXT element assoc'd w/ widgetID. */
    xlfatal("Internal error 1 in Xtango_Find_Context_From_WidgetID().");
}


/*****************************************************************************
 *
 ****************************************************************************/
WINTERP_TANGO_CONTEXT Xtango_Create_WINTERP_TANGO_CONTEXT(o_widget, widget_id)
     LVAL   o_widget;		/* XLTYPE_WIDGETOBJ */
     Widget widget_id;
{
  WINTERP_TANGO_CONTEXT context;

  context = (WINTERP_TANGO_CONTEXT) XtMalloc(sizeof(TangoWidgetAssoc_Node));
  context->widget_OBJ	 = o_widget;
  context->widget_ID	 = widget_id;
  context->adata	 = (ANIMATION_PTR) NULL;
  context->trans_reflist = (WINTERP_TANGO_CONTEXT_REF) NULL;

  if (tango_widget_alist)
    context->next = tango_widget_alist;
  else
    context->next = (WINTERP_TANGO_CONTEXT) NULL; /* elt is the only one on the alist */

  tango_widget_alist = context; /* make elt the new head of the list */
  return (context);
}


/*****************************************************************************
 *
 ****************************************************************************/
void Xtango_Remove_WINTERP_TANGO_CONTEXT(elt_to_destroy)
     WINTERP_TANGO_CONTEXT elt_to_destroy;
{
  WINTERP_TANGO_CONTEXT prev, cur;

  if (!tango_widget_alist)	/* ASSERT(tango_widget_alist!=NULL): Xtango_Remove_WINTERP_TANGO_CONTEXT() getting called with empty tango_widget_alist indicates a programming error -- this proc is meant to bracket Xtango_Create_WINTERP_TANGO_CONTEXT(). */
    xlfatal("Internal error 0 in Xtango_Remove_WINTERP_TANGO_CONTEXT().");

  /* free-up and NULLify all TANGO_TRANS which refer to the 
     context currently being destroyed -- mark the TANGO_TRANS nodes
     with NULL s.t. using nodes associated with a destroyed tango-widget
     and context will generate an XLISP error... */
  Xtango_Free_All_TANGO_TRANS_Assocd_With_Context(elt_to_destroy);

  prev = (WINTERP_TANGO_CONTEXT) NULL;
  cur = tango_widget_alist;
  while (cur && (cur != elt_to_destroy)) {
    prev = cur;
    cur = cur->next;
  }
  if (!prev) {			/* tango_widget_alist == cur == elt_to_destroy */
    tango_widget_alist = tango_widget_alist->next; /* pop the first elt */
    XtFree((char*) cur);
  }
  else if (cur) {		/* cur == elt_to_destroy */
    prev->next = cur->next;	/* remove cur from alist */
    XtFree((char*) cur);
  }
  else				/* ASSERT(cur!=NULL): Xtango_Remove_WINTERP_TANGO_CONTEXT() getting called without finding elt_to_destroy in tango_widget_alist indicates a programming error -- this proc is meant to bracket Xtango_Create_WINTERP_TANGO_CONTEXT(). */
    xlfatal("Internal error 1 in Xtango_Remove_WINTERP_TANGO_CONTEXT().");
}


/* TODO: #ifdef static globals in xtango/ *.c and add them to save/resore code
 below... */

static ANIMATION_PTR	saved_TANGO__data	= (ANIMATION_PTR) NULL;

static Boolean need_to_restore_saved_tango_globals = FALSE;


/******************************************************************************
 *
 ******************************************************************************/
void Xtango_Save_Set_Context_From_WidgetID(widget_id)
     Widget widget_id;
{
  /*
   * if the global 'TANGO__data->easel' == the widget_id on which tango-ops
   * are to be done, then don't bother with any of this -- the context was
   * set up by a previous call ...
   */
  if (!(TANGO__data && (TANGO__data->easel == widget_id))) {
    Xtango_Save_Set_Context(Xtango_Find_Context_From_WidgetID(widget_id));
  }
}


/******************************************************************************
 *
 ******************************************************************************/
void Xtango_Save_Set_Context(context)
     WINTERP_TANGO_CONTEXT context;
{
  if (context->adata != TANGO__data) { /* don't bother w/ save+restore if given 'context' is the same as the current context */

    need_to_restore_saved_tango_globals = TRUE;

    /* save old globals */
    saved_TANGO__data = TANGO__data;

    /* set new globals */
    TANGO__data = context->adata;
  }
}


/******************************************************************************
 *
 ******************************************************************************/
void Xtango_Begin_Drawing_Context_Kludge()
{
  need_to_restore_saved_tango_globals = FALSE;
}


/******************************************************************************
 *
 ******************************************************************************/
void Xtango_Restore_Context()
{
  if (need_to_restore_saved_tango_globals) {
    need_to_restore_saved_tango_globals = FALSE;

    /* restore previously set globals */
    TANGO__data = saved_TANGO__data;
  }
}


/******************************************************************************
 *
 ******************************************************************************/
void Xtango_Copy_Context(context)
     WINTERP_TANGO_CONTEXT context;
{
  /* set 'context' to current tango globals */
  context->adata = TANGO__data;
}


/******************************************************************************
 *
 ******************************************************************************/
T_Utils_Init()
{
  /* for Xtango_Get_Optional_PERFORM_Kwd_Arg() */
  k_PERFORM  = xlenter(":PERFORM");

  /* for Xtango_Get_TANGO_PATH_TYPE_Arg() */
  k_STRAIGHT         = xlenter(":STRAIGHT");
  k_CLOCKWISE        = xlenter(":CLOCKWISE");
  k_COUNTERCLOCKWISE = xlenter(":COUNTERCLOCKWISE");

  /* for Xtango_Get_TANGO_PART_TYPE_Arg() */
  k_TANGO_PART_TYPE_C	= xlenter(":CTR");
  k_TANGO_PART_TYPE_NW	= xlenter(":NW");
  k_TANGO_PART_TYPE_N	= xlenter(":N");
  k_TANGO_PART_TYPE_NE	= xlenter(":NE");
  k_TANGO_PART_TYPE_E	= xlenter(":E");
  k_TANGO_PART_TYPE_SE	= xlenter(":SE");
  k_TANGO_PART_TYPE_S	= xlenter(":S");
  k_TANGO_PART_TYPE_SW	= xlenter(":SW");
  k_TANGO_PART_TYPE_W	= xlenter(":W");
}
