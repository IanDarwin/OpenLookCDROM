/* -*-C-*-
********************************************************************************
*
* File:         wc_CascadeB.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_CascadeB.c,v 2.6 1994/06/06 15:40:50 npm Exp $
* Description:  XM_CASCADE_BUTTON_WIDGET_CLASS/CASCADE_BUTTON_GADGET_CLASS
* Author:       Niels Mayer
* Created:      Fri Oct 27 21:54:13 1989
* Modified:     Sun Jun  5 14:55:59 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_CascadeB.c,v 2.6 1994/06/06 15:40:50 npm Exp $";

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
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * (send <cascadebuttonwidget> :highlight <highlight_p>)
 *    ==> returns <cascadebuttonwidget>
 *
 * This method draws the shadow higlight around the cascadebutton if
 * <highlight_p> is true, and unhighlights if <highlight_p> is NIL.
 * 
 * void XmCascadeButtonHighlight (cb, highlight)
 * Widget cb;
 * Boolean highlight;
 ******************************************************************************/
LVAL Xm_Cascade_Button_Widget_Class_Method_HIGHLIGHT()
{
  LVAL self, highlight_p;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  highlight_p = xlgetarg();
  xllastarg();
  
  XmCascadeButtonHighlight(widget_id, (highlight_p != NIL) ? TRUE : FALSE);
  
  return (self);
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <cascadebuttongadget> :highlight <highlight_p>)
 *    ==> returns <cascadebuttonwidget>
 *
 * This method draws the shadow higlight around the cascadebutton if
 * <highlight_p> is true, and unhighlights if <highlight_p> is NIL.
 * 
 * void XmCascadeButtonGadgetHighlight (Widget cb, Boolean highlight)
 ******************************************************************************/
LVAL Xm_Cascade_Button_Gadget_Class_Method_HIGHLIGHT()
{
  LVAL self, highlight_p;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  highlight_p = xlgetarg();
  xllastarg();
  
  XmCascadeButtonGadgetHighlight(widget_id, (highlight_p != NIL) ? TRUE : FALSE);

  return (self);
}
#endif /* WINTERP_MOTIF_11 */


/******************************************************************************
 *
 ******************************************************************************/
Wc_CascadeB_Init()
{
  LVAL o_XM_CASCADE_BUTTON_WIDGET_CLASS;
  LVAL o_XM_CASCADE_BUTTON_GADGET_CLASS;

  o_XM_CASCADE_BUTTON_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_CASCADE_BUTTON_WIDGET_CLASS",
					 xmCascadeButtonWidgetClass);

  xladdmsg(o_XM_CASCADE_BUTTON_WIDGET_CLASS, ":HIGHLIGHT",
	   FTAB_Xm_Cascade_Button_Widget_Class_Method_HIGHLIGHT);

  o_XM_CASCADE_BUTTON_GADGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_CASCADE_BUTTON_GADGET_CLASS",
					 xmCascadeButtonGadgetClass);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_CASCADE_BUTTON_GADGET_CLASS, ":HIGHLIGHT",
	   FTAB_Xm_Cascade_Button_Gadget_Class_Method_HIGHLIGHT);
#else				/* MOTIF 1.0 */
  xladdmsg(o_XM_CASCADE_BUTTON_GADGET_CLASS, ":HIGHLIGHT",
	   FTAB_Xm_Cascade_Button_Widget_Class_Method_HIGHLIGHT);
#endif				/* WINTERP_MOTIF_11 */

#ifdef WINTERP_MOTIF_11
  /*
   * share this method w/ XmRowColumn... see 
   * wc_RowColumn.c:Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET()
   * for details
   */
  xladdmsg(o_XM_CASCADE_BUTTON_GADGET_CLASS, ":GET_SUB_MENU_WIDGET",
	   FTAB_Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET);
#endif				/* WINTERP_MOTIF_11 */
}
