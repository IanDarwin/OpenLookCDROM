/* -*-C-*-
********************************************************************************
*
* File:         tango.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tango.h,v 2.5 1994/06/06 15:41:11 npm Exp $
* Description:  External defs for WINTERP<-->Xtango interface
* Author:       Niels P. Mayer
* Created:      Tue Mar 23 01:57:40 1993
* Modified:     Sun Jun  5 14:23:49 1994 (Niels Mayer) npm@indeed
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

#include "xtango/xtangolocal.h"	/* need to get in deeper than xtango.h (which is also included) */

extern ANIMATION_PTR	TANGO__data; /* xtangolocal.h and xtangoloc.c */

typedef struct _TANGO_CONTEXT_Ref_Node *WINTERP_TANGO_CONTEXT_REF;
typedef struct _TANGO_CONTEXT_Ref_Node {
  LVAL			    lval; /* an lval containing reference to WINTERP_TANGO_CONTEXT */
  WINTERP_TANGO_CONTEXT_REF next;
} TANGO_CONTEXT_Ref_Node;

/*
 * Note the following typedef declared in xlisp/xlisp.h is commented out to
 * prevent error "redeclaration of 'WINTERP_TANGO_CONTEXT'"

typedef struct _TangoWidgetAssoc_Node *WINTERP_TANGO_CONTEXT;

 * this is done to define opaque ptr WINTERP_TANGO_CONTEXT
 * as needed by xldmem.h/xldmem.c w/o including ../tango.h
 */

typedef struct _TangoWidgetAssoc_Node {
  /*
   * The WIDGETOBJ assoc'd w/ current context and "Widget TANGO__data->easel;"
   */
  LVAL		widget_OBJ;

  /*
   * The Widget ID assoc'd w/ current context and "LVAL widget_OBJ"...
   */ 
  Widget	widget_ID;

  /*
   * The following group of fields are used by the following procs
   * Xtango_Save_Set_Context_From_WidgetID(), Xtango_Save_Set_Context(),
   * Xtango_Restore_Context(), Xtango_Copy_Context().
   *
   * The set of globals assoc'd w/ the strucure elements below are the
   * "context" associated with a particular tango-widget-instance. Since we can
   * have multiple tango-widget-instances, we keep a copy of the
   * tango-widget-instance's "context" in the structure elements below.
   * Prior to doing a tango operation on a particular tango-widget-instance,
   * its associated tango-image-instance, or its tango-transition-instance,
   * we must set the globals from the "context" via routines
   * Xtango_Save_Set_Context_From_WidgetID(), or Xtango_Save_Set_Context(). After
   * the tango-operation, the "context" should be restored with Xtango_Restore_Context().
   *
   * A new "context" is created when a new tango-widget-instance is created.
   * THe context is created by Xtango_Create_WINTERP_TANGO_CONTEXT(),
   * called from Wc_Xtango_Initialize_WIDGETOBJ(), which is called from
   * Xtango_Widget_Class_Method_ISNEW(), the tango-widget instance initializer.
   *
   * The tango globals are initialized/created/set by
   * Xtango_Widget_Class_Method_BEGIN_DRAWING(), and before returning, the routine
   * calls Xtango_Copy_Context() to copy the globals into the stuctures below.
   * Other methods may also alter globals, and these also call Xtango_Copy_Context()
   * to copy the updated globals back to the "context".
   * 
   * When the tango-widget-instance is destroyed, the "context" and associated
   * memory is destroyed by Xtango_Widget_Destroy_Callback().
   */
  ANIMATION_PTR adata;		/* ANIMATION_PTR TANGO__data */

  /*
   * This is a list of type LVAL(XLTYPE_TANGO_TRANS) which refer to this
   * WINTERP_TANGO_CONTEXT ptr.  When proc Xtango_Remove_WINTERP_TANGO_CONTEXT()
   * destroys the WINTERP_TANGO_CONTEXT, all LVAL(XLTYPE_TANGO_TRANS)s that
   * reference this structure must be set to NULL so as to prevent subesquent
   * reference to freed memory by values that are still referenceable by XLISP
   * -- an xlerror() will get signalled instead, informing user that the attempted
   * use of an LVAL(XLTYPE_TANGO_TRANS) corresponding to a destroyed tango-widget. 
   *
   * When a TANGO_TRANS is created by xlisp/xldmem.c:cv_tangotrans(), proc
   * t_trans.c:Xtango_Context_Add_TANGO_TRANS() will add the TANGO_TRANS
   * to the context.
   *
   * When the garbage collector xlisp/xldmem.c:sweep() removes a
   * no-longer-referenceable TANGO_TRANS,
   * t_trans.c:Xtango_Context_Remove_TANGO_TRANS() gets called to remove
   * the TANGO_TRANS reference from the list.
   */
  WINTERP_TANGO_CONTEXT_REF trans_reflist;

  /*
   * pointer to next widget/tango-context association...
   */
  WINTERP_TANGO_CONTEXT next;

} TangoWidgetAssoc_Node;


#define TANGO_VERTEX_SEQ_MAX_SIZE 7 /* this limit implicit in xtango/xtangolocal.h
				       -- see: struct _TANGO_POLYLINE,
				       struct _TANGO_POLYGON, struct _TANGO_SPLINE */


/* from wc_Xtango.c */
extern LVAL k_VISIBLE, k_INVISIBLE; /* init'd in Wc_Xtango_Init(), used by all Tango_*_Image_Class_Method_ISNEW() */
extern LVAL k_SHOW;		/* init'd in Wc_Xtango_Init(), used by all Tango_*_Image_Class_Method_ISNEW() */

extern Widget
Xtango_Get_TANGO_WIDGETOBJ_Returning_Validated_WidgetID
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL *wobj_return
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/* from t_classes.c */
extern TANGO_IMAGE Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL *tiobj_return
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Tcls_Destroy_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_IMAGE image,
 TANGO_TRANS cur_trans
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Tcls_Free_TANGO_IMAGE
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_IMAGE image
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Tcls_Get_OBJECT_Arg_Returning_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Tcls_Initialize_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL                  o_tangoImage,
 TANGO_IMAGE           tangoImage_id,
 WINTERP_TANGO_CONTEXT context
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Tcls_TangoImageID_To_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_IMAGE tangoImage_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char*            class_name,
 TANGO_IMAGE_TYPE image_type_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Tcls_Print_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL fptr,
 LVAL o_tangoImage
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Tcls_Get_TANGOIMAGECLASS_Symbol_From_TANGOIMAGEOBJ
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL o_tangoImage
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_IMAGE_TYPE
Tcls_TANGOIMAGECLASSOBJ_To_TANGO_IMAGE_TYPE
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL o_class
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/* from t_utils.c */
extern char string_err_msg_bad_loc_coord[];
extern char string_err_msg_bad_radius_coord[];
extern char string_err_msg_bad_color[];
extern char string_err_msg_bad_fill[];
extern char string_err_msg_bad_image_class[];
extern char string_err_msg_bad_size_coord[];
extern char string_err_msg_bad_line_width[];
extern char string_err_msg_bad_line_style[];
extern char string_err_msg_bad_line_arrow[];
extern char string_err_msg_bad_poly_offset[];
extern char string_err_msg_too_many_poly_offsets[];
extern char string_err_msg_too_few_poly_offsets[];
extern char string_err_msg_bad_text[];
extern char string_err_msg_bad_fontstr[];
extern char string_err_msg_bad_bitmap_vector[];
extern char string_err_msg_bad_bitmap[];
extern char string_err_msg_bad_bitmap_row[];
extern char string_err_msg_bad_bitmap_array_elt[];
extern char string_err_msg_too_many_bitmaps[];
extern char string_err_msg_bitmaps_of_different_heights[];
extern char string_err_msg_bitmaps_of_different_widths[];
extern char string_err_msg_bad_gif_fname[];
extern char string_err_msg_bad_gif_verbose_arg[];

extern void
Xtango_COMPLAIN
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char *fmt,
 ...
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_DEBUG_TRACE_PRINT
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char *fmt,
 ...
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_COLOR
Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval_color
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_COLOR
Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval_color
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern int
Xtango_Get_Positive_FIXNUM_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern double
Xtango_Get_Positive_FLONUM_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Xtango_Get_FLONUM_COMPLEX_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Error_Bad_FLONUM_COMPLEX_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL arg
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Boolean
Xtango_Get_Optional_PERFORM_Kwd_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_PATH_TYPE
Xtango_Get_TANGO_PATH_TYPE_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_PART_TYPE
Xtango_Get_TANGO_PART_TYPE_Arg
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Check_Initialized
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern WINTERP_TANGO_CONTEXT
Xtango_Find_Context_From_WidgetID
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widgetID
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern WINTERP_TANGO_CONTEXT
Xtango_Create_WINTERP_TANGO_CONTEXT
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL   o_widget,
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Remove_WINTERP_TANGO_CONTEXT
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 WINTERP_TANGO_CONTEXT elt_to_destroy
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Save_Set_Context_From_WidgetID
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Save_Set_Context
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 WINTERP_TANGO_CONTEXT tango_context
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Begin_Drawing_Context_Kludge
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Restore_Context
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Copy_Context
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 WINTERP_TANGO_CONTEXT context
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/* from t_path.c */
extern void
Xtango_Context_Remove_TANGO_PATH
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern TANGO_PATH
Xtango_Get_TANGO_PATH_Args
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Boolean *result_to_be_freed_p
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/* from t_trans.c */
extern void
Xtango_Context_Add_TANGO_TRANS
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Context_Remove_TANGO_TRANS
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Xtango_Free_All_TANGO_TRANS_Assocd_With_Context
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 WINTERP_TANGO_CONTEXT context
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/* from tic_Bitmap.c */
extern LVAL
Xtango_Pixmap_To_Lisp_2D_Array
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Pixmap       src_pix,
 int          src_x,
 int          src_y,
 unsigned int src_width,
 unsigned int src_height
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


/*
 * the following fns are from Xtango, but were not extern'd by xtango/xtango.h
 * nor xtango/xtangolocal.h. We extern them here simply to document the
 * "extension" of Xtango's API... The fns are used in t_trans.c, t_path.c,
 * and tic_Bitmap.c.NOTE ASSUMPTION: path_free() and trans_free() don't need
 * WINTERP to call Xtango_Save_Set_Context...() and Xtango_Restore_Context()
 * since no tango globals are affected by them.
 */
extern void
path_free
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_PATH path
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );				/* xtango/xtangotrans.c: single arg version of TANGOpath_free(...) */

extern void
transition_free
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_TRANS trans
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );				/* xtango/xtangotrans.c: single arg version of TANGOtrans_free(...) */

extern TANGO_IMAGE
image_create
(
#ifndef _NO_PROTO /* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 TANGO_IMAGE_TYPE type,
 ...
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );				/* xtango/xtangoimage.c: used by tic_Composit.c:Xtango_Get_Args_Create_Image() */

extern void
animate_begin			/* xtango/xtangowindow.c: used by wc_Xtango.c:Xtango_Widget_Class_Method_BEGIN_DRAWING() */
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );
