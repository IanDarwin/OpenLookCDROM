/* -*-C-*-
********************************************************************************
*
* File:         wc_ScrolledW.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_ScrolledW.c,v 2.6 1994/06/06 15:40:42 npm Exp $
* Description:  XM_SCROLLED_WINDOW_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 04:38:40 1989
* Modified:     Sun Jun  5 15:06:24 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_ScrolledW.c,v 2.6 1994/06/06 15:40:42 npm Exp $";

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
#include <Xm/ScrolledW.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * (send <ScrolledWin_Widget> :SET_AREAS <hscroll> <vscroll> <wregion>)
 * ==> returns <ScrolledWin_Widget>....
 * This method allows you to add or change the work region controlled by
 * a scrolled window, and set the scrollbars for scrolled windows in which
 * resource :XMN_SCROLLING_POLICY is set to :APPLICATION_DEFINED.
 * <wregion> <hscroll> and  <vscroll> are all WIDGETOBJS. Use NIL
 * if you don't want to set a particular parameter.
 *
 * void XmScrolledWindowSetAreas(sw, hscroll, vscroll, wregion)
 *     XmScrolledWindowWidget sw;
 *     Widget hscroll;
 *     Widget vscroll;
 *     Widget wregion;
 ******************************************************************************/
LVAL Xm_Scrolled_Window_Widget_Class_Method_SET_AREAS()
{
  LVAL self, widgetobj;
  Widget widget_id, hscroll_id, vscroll_id, wregion_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self); /* get <self> */

  if (moreargs() && (*xlargv == NIL)) {	/* get <hscroll> */
    nextarg();
    hscroll_id = NULL;
  }
  else
    hscroll_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) {	/* get <vscroll> */
    nextarg();
    vscroll_id = NULL;
  }
  else
    vscroll_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) {	/* get <wregion> */
    nextarg();
    wregion_id = NULL;
  }
  else
    wregion_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  xllastarg();

  XmScrolledWindowSetAreas(widget_id, hscroll_id, vscroll_id, wregion_id);
  
  return (self);
}

#ifdef WINTERP_MOTIF_12
/******************************************************************************
 * (send <ScrolledWin_Widget> :SCROLL_VISIBLE <wid> <hor-margin> <ver-margin>)
 * ==> returns <ScrolledWin_Widget>....
 *
 * Method :SCROLL_VISIBLE makes WIDGETOBJ <wid>, an obscured or partially
 * obscured widget or gadget descendant of a ScrolledWindow work area visible.
 * The function repositions the work area and sets the specified margins
 * (FIXNUMs <hor-margin> and <ver-margin>) between the widget and the nearest
 * viewport boundary. The widget's location relative to the viewport determines
 * whether one or both of the margins must be adjusted. This function requires
 * that the XmNscrollingPolicy of the ScrolledWindow widget be set to XmAUTOMATIC.
 *
 * void XmScrollVisible(
 *			Widget      	scrw,
 *			Widget          wid,
 *			Dimension       hor_margin, 
 *			Dimension       ver_margin);
 ******************************************************************************/
LVAL Xm_Scrolled_Window_Widget_Class_Method_SCROLL_VISIBLE()
{
  LVAL self, obscured;
  Widget widget_id, obscured_id;
  Dimension hor_margin, ver_margin;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self); /* get <self> */
  obscured_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&obscured); /* get <wid> */
  hor_margin = Get_Dimension_Argument();
  ver_margin = Get_Dimension_Argument();
  xllastarg();

  XmScrollVisible(widget_id, obscured_id, hor_margin, ver_margin);
  
  return (self);
}
#endif /* WINTERP_MOTIF_12 */


/******************************************************************************
 *
 ******************************************************************************/
Wc_ScrolledW_Init()
{
  LVAL o_XM_SCROLLED_WINDOW_WIDGET_CLASS;

  o_XM_SCROLLED_WINDOW_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_SCROLLED_WINDOW_WIDGET_CLASS",
					 xmScrolledWindowWidgetClass);

  xladdmsg(o_XM_SCROLLED_WINDOW_WIDGET_CLASS, ":SET_AREAS",
	   FTAB_Xm_Scrolled_Window_Widget_Class_Method_SET_AREAS);

#ifdef WINTERP_MOTIF_12
  xladdmsg(o_XM_SCROLLED_WINDOW_WIDGET_CLASS, ":SCROLL_VISIBLE",
	   FTAB_Xm_Scrolled_Window_Widget_Class_Method_SCROLL_VISIBLE);
#endif /* WINTERP_MOTIF_12 */
}
