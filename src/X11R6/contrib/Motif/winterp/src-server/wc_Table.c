/* -*-C-*-
********************************************************************************
*
* File:         wc_Table.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_Table.c,v 2.7 1994/06/06 15:40:40 npm Exp $
* Description:  TABLE_WIDGET_CLASS
*		This is a contributed widget, see directory widgets/Table.{c,h}
* Author:       Niels Mayer
* Created:      Sat Oct 28 03:35:05 1989
* Modified:     Sun Jun  5 15:08:13 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_Table.c,v 2.7 1994/06/06 15:40:40 npm Exp $";

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
#include "widgets/Table.h"
#include "winterp.h"
#include "w_funtab.h"

LVAL k_TBL_LEFT, k_TBL_RIGHT,
  k_TBL_TOP, k_TBL_BOTTOM,
  k_TBL_SM_WIDTH, k_TBL_SM_HEIGHT,
  k_TBL_LK_WIDTH, k_TBL_LK_HEIGHT,
  k_TBL_DEF_OPT;

/******************************************************************************/
static XtTblMask Get_Table_Option_Args()
{
  LVAL arg;
  XtTblMask opt = 0;

  while (moreargs() && opt != TBL_DEF_OPT) {
    arg = nextarg();
    if (arg == k_TBL_LEFT)
      opt |= TBL_LEFT;
    else if (arg == k_TBL_RIGHT)
      opt |= TBL_RIGHT;
    else if (arg == k_TBL_TOP)
      opt |= TBL_TOP;
    else if (arg == k_TBL_BOTTOM)
      opt |= TBL_BOTTOM;
    else if (arg == k_TBL_SM_WIDTH)
      opt |= TBL_SM_WIDTH;
    else if (arg == k_TBL_SM_HEIGHT)
      opt |= TBL_SM_HEIGHT;
    else if (arg == k_TBL_LK_WIDTH)
      opt |= TBL_LK_WIDTH;
    else if (arg == k_TBL_LK_HEIGHT)
      opt |= TBL_LK_HEIGHT;
    else if (arg == k_TBL_DEF_OPT)
      opt = TBL_DEF_OPT;	/* overrides all other options... */
    else
      xlerror("Invalid Table Option -- must be one of [:TBL_DEF_OPT :TBL_LEFT :TBL_RIGHT :TBL_TOP :TBL_BOTTOM :TBL_SM_WIDTH :TBL_SM_HEIGHT :TBL_LK_WIDTH :TBL_LK_HEIGHT].", arg);
  }

  return (opt);
}


/******************************************************************************
 *  caddr_t XtTblParseLayout(layout)
 *  String layout;			-- String layout specification --
 *
 * We don't bother with this one -- it gets implicitly called in the
 * String to Resource converter whenever we try to setvalues "XmNlayout"
 ******************************************************************************/


/******************************************************************************
 * (XT_TBL_POSITION <widget> <col> <row>)
 * 		--> <widget>
 *
 * This routine positions a widget that has been created
 * under a widget of class tableWidgetClass.  The widget
 * will be placed at column `col' and row `row'.  If
 * the widget has never been placed before,  it will
 * span only one space in each direction and its
 * options will be the defaults for the table widget.
 *
 *  void XtTblPosition(w, col, row)
 *  Widget w;			-- Widget to position --
 *  Position col, row;		-- Position in array  --
 ******************************************************************************/
LVAL Prim_XT_TBL_POSITION()
{
  LVAL		lval_widget;
  Widget 	widget_id;
  Position      col, row;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);
  /* get <col> */
  col = Get_Position_Argument();
  /* get <row> */
  row = Get_Position_Argument();
  xllastarg();

  XtTblPosition(widget_id, col, row);

  return (lval_widget);
}


/******************************************************************************
 * (XT_TBL_RESIZE <widget> <h_span> <v_span>)
 * 		--> <widget>
 *
 * This routine changes the span of widget `w' to (`h_span', `v_span').
 * If the widget has never been placed before,  it will be located
 * at (0,0) and its options will be the defaults for its
 * parent table widget.
 *
 * void XtTblResize(w, h_span, v_span)
 * Widget w;			-- Widget to resize            --
 * Dimension h_span, v_span;	-- New widget span             --
 ******************************************************************************/
LVAL Prim_XT_TBL_RESIZE()
{
  LVAL		lval_widget;
  Widget 	widget_id;
  Dimension	h_span, v_span;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);
  /* get <h_span> */
  h_span = Get_Dimension_Argument();
  /* get <v_span> */
  v_span = Get_Dimension_Argument();
  xllastarg();

  XtTblResize(widget_id, h_span, v_span);

  return (lval_widget);
}


/******************************************************************************
 * (XT_TBL_OPTIONS <widget> <opt>)
 * 		--> <widget>
 *
 * This routine changes the options of widget `w' to `opt'.  If
 * the widget has never been placed before,  it will be located
 * and (0,0) with a span of (1,1) and its options will be the
 * default options for its parent table widget.  The option
 * mask is as follows:
 *   TBL_LEFT		Horizontally left justified.
 *   TBL_RIGHT		Horizontally right justified.
 *   TBL_TOP		Vertically top justified.
 *   TBL_BOTTOM 	Vertically bottom justified.
 *   TBL_SM_WIDTH	Force the width to be as small as possible.
 *   TBL_SM_HEIGHT	Force the height to be as small as possible.
 *   TBL_LK_WIDTH	Don't try to expand the widget horizontally.
 *   TBL_LK_HEIGHT	Don't try to expand the widget vertically.
 * If `options' is equal to TBL_DEF_OPT,  it is filled with 
 * the default value for the table widget.
 *
 *  void XtTblOptions(w, opt)
 *  Widget w;			-- Widget to change --
 *  XtTblMask opt;		-- New option mask  --
 ******************************************************************************/
LVAL Prim_XT_TBL_OPTIONS()
{
  LVAL		lval_widget;
  Widget 	widget_id;
  XtTblMask	opt;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);
  /* get [<options>...] */
  opt = Get_Table_Option_Args();
  xllastarg();

  XtTblOptions(widget_id, opt);

  return (lval_widget);
}


/******************************************************************************
 *  (XT_TBL_CONFIG <widget> <col> <row> <h_span> <v_span> <opt>)
 *       --> <widget>
 *
 * This routine positions a widget that has been created
 * under a widget of class tableWidgetClass.  The widget
 * will be placed at column `col' and row `row'.  The
 * widget will span the distances given by `h_span' and `v_span'.
 * The options argument is as follows:
 *   TBL_LEFT		Horizontally left justified.
 *   TBL_RIGHT		Horizontally right justified.
 *   TBL_TOP		Vertically top justified.
 *   TBL_BOTTOM 	Vertically bottom justified.
 *   TBL_SM_WIDTH	Force the width to be as small as possible.
 *   TBL_SM_HEIGHT	Force the height to be as small as possible.
 *   TBL_LK_WIDTH	Don't try to expand the widget horizontally.
 *   TBL_LK_HEIGHT	Don't try to expand the widget vertically.
 * If `options' is equal to TBL_DEF_OPT,  it is filled with 
 * the default value for the table widget.
 *
 *  void XtTblConfig(w, col, row, h_span, v_span, opt)
 *  Widget w;			-- Widget to position          --
 *  Position col, row;		-- Position in array           --
 *  Dimension h_span, v_span;	-- Horizonal and vertical span --
 *  XtTblMask opt;		-- Widget placement options    --
 * ******************************************************************************/
LVAL Prim_XT_TBL_CONFIG()
{
  LVAL		lval_widget;
  Widget 	widget_id;
  Position      col, row;
  Dimension	h_span, v_span;
  XtTblMask	opt;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);
  /* get <col> */
  col = Get_Position_Argument();
  /* get <row> */
  row = Get_Position_Argument();
  /* get <h_span> */
  h_span = Get_Dimension_Argument();
  /* get <v_span> */
  v_span = Get_Dimension_Argument();
  /* get [<options>...] */
  opt = Get_Table_Option_Args();
  xllastarg();

  XtTblConfig(widget_id, col, row, h_span, v_span, opt);

  return (lval_widget);
}


/******************************************************************************
 *  NOTE: new resources to add to w_resources.c:
 * { XmNdefaultOptions, XmCOptions, XmROptions, sizeof(XtTblMask),
 *   XtOffset(TableWidget, table.def_options), XtRImmediate, (caddr_t) 0 }
 * { XmNlayout, XmCLayout, XmRLayout, sizeof(caddr_t),
 *   XtOffset(TableWidget, table.init_layout), XtRImmediate, (caddr_t) &def }
 ******************************************************************************/
Wc_Table_Init()
{
  LVAL o_TABLE_WIDGET_CLASS;

  /*
   * force various string-->resource converters for table widget to be
   * initialized. Otherwise we won't be able to use special table widget
   * resources for the first table widget created....
   */
  TblClassInitialize();

  k_TBL_LEFT		= xlenter(":TBL_LEFT");
  k_TBL_RIGHT		= xlenter(":TBL_RIGHT");
  k_TBL_TOP		= xlenter(":TBL_TOP");
  k_TBL_BOTTOM		= xlenter(":TBL_BOTTOM");
  k_TBL_SM_WIDTH	= xlenter(":TBL_SM_WIDTH");
  k_TBL_SM_HEIGHT	= xlenter(":TBL_SM_HEIGHT");
  k_TBL_LK_WIDTH	= xlenter(":TBL_LK_WIDTH");
  k_TBL_LK_HEIGHT	= xlenter(":TBL_LK_HEIGHT");
  k_TBL_DEF_OPT		= xlenter(":TBL_DEF_OPT");

  o_TABLE_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("TABLE_WIDGET_CLASS",
					 tableWidgetClass);
}
