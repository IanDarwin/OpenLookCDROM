/* -*-C-*-
********************************************************************************
*
* File:         winterp.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/winterp.h,v 2.5 1994/06/06 15:40:36 npm Exp $
* Description:  Miscellaneous macros, #defines, etc
* Author:       Niels Mayer
* Created:      Wed Aug 31 14:09:08 1988
* Modified:     Sun Jun  5 15:19:43 1994 (Niels Mayer) npm@indeed
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

#define WINTERP_VERSION_INT  2
#define WINTERP_REVISION_INT 00

/*----------------------------------------------------------------------------
 * Kludges to sweeten Xt's ugly argument processing for widget creation.
 *---------------------------------------------------------------------------*/
extern Arg _args[];		/* global in winterp.c */
extern int _num_args;		/* global in winterp.c */

#define ARGLIST_RESET() \
  _num_args = 0

#define ARGLIST_ADD(name, value) \
  XtSetArg(_args[_num_args], (name), (value)); _num_args++

#define ARGLIST_SET_LAST_ELT(name, value) \
  XtSetArg(_args[(_num_args - 1)], (name), (value))

#define ARGLIST() \
  _args, _num_args


/* Setup cpp symbols for conditional compilation of Motif 1.0 vs Motif 1.1 */

#if (XmVersion >= 1001)		/* XmVersion from <Xm/Xm.h> */
#define WINTERP_MOTIF_11
#endif /* (XmVersion >= 1001) */

#if (XmVersion >= 1002)		/* XmVersion from <Xm/Xm.h> */
#define WINTERP_MOTIF_12
#endif /* (XmVersion >= 1002) */

#ifndef WINTERP_MOTIF_11	/* Motif 1.0 backwards compatibility */
typedef unsigned char XtEnum;
typedef char* XtPointer;
#endif /* WINTERP_MOTIF_11 */

#ifdef WINTERP_MOTIF_12		/* R4/Motif 1.1 backwards compatibility */
#include <X11/Xos.h>
#endif /* WINTERP_MOTIF_12 */

/*
 * Motif 1.1.1 has some changes over version 1.1 that are important enough to
 * require #ifdefs in the code. Gratuitous, consistency-violating alterations
 * to the API were made in fixing the 1.1 bugs. Because Motif 1.1 and Motif 1.1.1
 * cannot be identified through any sort of documented means, I had to resort
 * to a hack, in the truest sense of the word, in order to work around this
 * charming detail. The hack to determine which version of Motif 1.1 you're
 * running: 1.1.1 defines the new symbol XmDYNAMIC_DEFAULT_TAB_GROUP, and
 * 1.1 doesn't... The presence of that symbol tells WINTERP that we're using
 * 1.1.1 and defines WINTERP_MOTIF_111. This is used in a few places in
 * w_resources.c, for example.
 */
#ifdef WINTERP_MOTIF_11
#ifdef XmDYNAMIC_DEFAULT_TAB_GROUP
#define WINTERP_MOTIF_111
#endif /* XmDYNAMIC_DEFAULT_TAB_GROUP */
#endif /* WINTERP_MOTIF_11 */

/*
 * Motif 1.1.3 defines two new symbols, XmCR_CREATE and XmCR_PROTOCOLS, use
 * these to tell us we're running 1.1.3...
 */
#ifdef WINTERP_MOTIF_11
#ifdef XmCR_CREATE
#define WINTERP_MOTIF_113
#endif /* XmCR_CREATE */
#endif /* WINTERP_MOTIF_11 */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

#include "user_prefs.h"
#include "xlisp/xlisp.h"

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern LVAL v_savedobjs;	/* xlisp/xlglob.c */
extern char temptext[];		/* winterp.c */
extern LVAL s_CALLBACK_WIDGET, s_CALLBACK_REASON, s_CALLBACK_XEVENT; /* w_callbacks.c */
extern LVAL s_CALLBACK_WINDOW, s_CALLBACK_VALUE, s_CALLBACK_LENGTH; /* w_callbacks.c */
extern LVAL s_CALLBACK_DOIT;	/* w_callbacks.c */
#ifdef WINTERP_MOTIF_11
extern LVAL s_CALLBACK_CLICK_COUNT; /* w_callbacks.c */
#endif /* WINTERP_MOTIF_11 */
extern LVAL k_managed, k_unmanaged, k_dialog, k_scrolled; /* wc_WIDGET.c */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern void 
Lexical_Bindings_For_XmAnyCallbackStruct
(				/* wc_WIDGET.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL	   bindings_list,
 LVAL	   lexical_env,
 XtPointer call_data,
 LVAL	   client_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void 
Lexical_Bindings_For_XmSelectionBoxCallbackStruct
(				/* wc_SelectioB.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL	   bindings_list,
 LVAL	   lexical_env,
 XtPointer call_data,
 LVAL	   client_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wcb_Meta_Callbackproc
(				/* w_callbacks.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL      lval_closure,
 LVAL      client_data,
 XtPointer call_data,
 void	   (*bind_call_data_values_proc)
 (LVAL bindings_list, LVAL lexical_env, XtPointer call_data, LVAL client_data),
 void	   (*set_call_data_values_proc)
 (LVAL lexical_env, XtPointer call_data),
 void	   (*trace_enter_proc)
 (LVAL tracing, LVAL client_data)
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wcb_Callback_Trace_Proc
(				/* w_callbacks.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL tracing,
 LVAL client_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcb_Get_Callback_Reason_Symbol
(				/* w_callbacks.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 int cb_reason
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wcb_Callback_Missing_Call_Data_Error
(				/* w_callbacks.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL client_data,
 LVAL lval_symbol
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcb_Meta_Method_Add_Callback
(				/* w_callbacks.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 XtCallbackProc	callback_proc,
#if NeedWidePrototypes		/* <X11/Intrinsic.h> defines this -- note ANSI C's worse-is-better design-philosophy at work here. */
 int		one_callback_per_name_p
#else  /* !defined(NeedWidePrototypes)  */
 Boolean	one_callback_per_name_p
#endif /* NeedWidePrototypes */
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


extern LVAL
Wcls_Create_Subclass_Of_WIDGET_CLASS
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char*		class_name,
 WidgetClass	widgetclass_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Widget
Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL *wobj_return
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcls_WidgetClassID_To_WIDGETCLASSOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 WidgetClass widget_class
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcls_WidgetID_To_WIDGETOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcls_WidgetID_To_Generic_WIDGETOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wshl_WidgetID_To_WIDGETOBJ
(				/* wc_SHELL.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcls_Get_OBJECT_Arg_Returning_WIDGETOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wcls_Initialize_WIDGETOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL	o_widget,
 Widget	widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern WidgetClass
Wcls_WIDGETCLASSOBJ_To_WidgetClassID
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL o_class
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wcls_Generic_Hybrid_Array_Method_SHOW
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL base_class
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wcls_Print_WIDGETOBJ
(				/* w_classes.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL fptr,
 LVAL o_widget
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


extern ArgList
Wres_Get_LispArglist
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL     o_widget,
 Widget   widgetID,
 ArgList  prepend_args,
 Cardinal prepend_nargs,
 Cardinal *nargs
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wres_Free_C_Arglist_Data
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern XtPointer
Wres_GetValues_Token_Value
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern XtPointer
Wres_GetValues_Alternate_Token_Value
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Boolean
Wres_GetValues_Failed
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 XtPointer res_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Boolean
Wres_GetValues_Alternate_Failed
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 XtPointer res_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern char*
Wres_Get_Name
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL res
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wres_Get_Symbol
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL res
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Boolean
Wres_Is_Callback_P
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL res
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern ArgList
Wres_Get_GetValues_ArgList
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Cardinal *numargs
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wres_GetValues_ArgList_To_Lisp
(				/* w_resources.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget	  widget_id,
 ArgList  arglist,
 Cardinal numargs
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


extern int
Get_Int_Argument
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 long min,
 long max
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Dimension
Get_Dimension_Argument
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Position
Get_Position_Argument
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Winterp_Print_Newline
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Winterp_Print_Prompt
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
#if NeedWidePrototypes		/* <X11/Intrinsic.h> defines this -- note ANSI C's worse-is-better design-philosophy at work here. */
 int		newline_p
#else  /* !defined(NeedWidePrototypes)  */
 Boolean	newline_p
#endif /* NeedWidePrototypes */
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern char*
Wut_Sexp_To_String
(				/* utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL sexp,
 int* length
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern int
Read_From_Stream_Eval_And_Print
(				/* winterp.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL sexp_stream
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Winterp_Application_Shell_WMDelete_Callback
(				/* winterp.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget    shell,
 XtPointer closure,
 XtPointer call_data
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );


extern int
Wso_Hash
(				/* w_savedobjs.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL object
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wut_Wait_Till_Expose
(				/* w_utils.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern Window
Wxt_Validated_WidgetID_to_Window
(				/* w_libXt.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id,
 LVAL   o_widget
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wxt_Error_If_Gadget
(				/* w_libXt.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget widget_id,
 LVAL   o_widget
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wpm_Set_Pixmap_Reference
(				/* w_pixmap.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval_pixmap,
 LVAL o_widget,
 LVAL lval_resname
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wtx_Winterp_Lisp_Action_Proc
(				/* w_txlations.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 Widget    widget,
 XEvent*   event,
 String*   params,
 Cardinal* num_params
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wfu_Funtab_Sanity_Check
(				/* w_funtab.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 void
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wicb_Read_Sexp_Proc
(				/* w_inputCB.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL	client_data,		/* IN ... client_data==NULL means we funcall *process_sexp_proc() and *protect_sexp_proc() args below */
 char*	rdbuf,			/* IN/OUT */
 int	len,			/* IN */
 int*	paren_count,		/* IN/OUT */
 int*	comment_count,		/* IN/OUT */
 int*	read_state,		/* IN/OUT (enum type WINTERP_READER_STATE) */
 LVAL*	sexp_stream,		/* IN/OUT */
 LVAL*	last_elt,		/* IN/OUT */
 void	(*protect_sexp_proc)(LVAL sexp_stream), /* function pointer -- only used when client_data==NULL */
 void	(*process_sexp_proc)(LVAL sexp_stream) /* function pointer -- only used when client_data==NULL */
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );
