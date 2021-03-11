/* -*-C-*-
********************************************************************************
*
* File:         wc_MainW.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/wc_MainW.c,v 2.6 1994/06/06 15:40:46 npm Exp $
* Description:  XM_MAIN_WINDOW_WIDGET_CLASS
* Author:       Niels Mayer
* Created:      Sat Oct 28 03:35:05 1989
* Modified:     Sun Jun  5 15:02:00 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/wc_MainW.c,v 2.6 1994/06/06 15:40:46 npm Exp $";

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
#include <Xm/MainW.h>
#include "winterp.h"
#include "w_funtab.h"


/******************************************************************************
 * (send <mainwinwidget> :SET_AREAS <menu> <command> <hscroll> <vscroll>
 *                                  <workregion>)
 *      ==> returns <mainwinwidget>.
 * This method is used to attach children to be managed by an instance of
 * XM_MAIN_WINDOW_WIDGET_CLASS. <menu> <command> <hscroll> <vscroll>
 * and <workregion> are all WIDGETOBJs. If you don't want to set
 * the particular region, pass NIL as the argument for the particular widget.
 * 
 * void XmMainWindowSetAreas(mw, menu, command, hscroll, vscroll, wregion)
 *     XmMainWindowWidget mw;
 *     Widget menu;
 *     Widget command;
 *     Widget hscroll;
 *     Widget vscroll;
 *     Widget wregion;
 ******************************************************************************/
LVAL Xm_Main_Window_Widget_Class_Method_SET_AREAS()
{
  LVAL self, widgetobj;
  Widget widget_id, menu, command, hscroll, vscroll, wregion;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self); /*  get <self> */

  if (moreargs() && (*xlargv == NIL)) { /* get <menu> */
    menu = NULL;
    nextarg();
  }
  else
    menu = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) { /* get <command> */
    command = NULL;
    nextarg();
  }
  else
    command = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) { /* get <hscroll> */
    hscroll = NULL;
    nextarg();
  }
  else
    hscroll = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) { /* get <vscroll> */
    vscroll = NULL;
    nextarg();
  }
  else
    vscroll = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  if (moreargs() && (*xlargv == NIL)) { /* get <workregion> */
    wregion = NULL;
    nextarg();
  }
  else
    wregion = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&widgetobj);

  xllastarg();

  XmMainWindowSetAreas(widget_id, menu, command, hscroll, vscroll, wregion);

  return (self);
}


/******************************************************************************
 * (send <mainwinwidget> :SEP1)
 *       ==> returns the WIDGETOBJ corresponding to the first separator widget.
 *
 * Widget XmMainWindowSep1(w)
 *  Widget w;
 ******************************************************************************/
LVAL Xm_Main_Window_Widget_Class_Method_SEP1()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();
  
  return (Wcls_WidgetID_To_WIDGETOBJ(XmMainWindowSep1(widget_id)));
}


/******************************************************************************
 * (send <mainwinwidget> :SEP2)
 *       ==> returns the WIDGETOBJ corresponding to the second separator widget.
 *
 * Widget XmMainWindowSep2(w)
 *  Widget w;
 ******************************************************************************/
LVAL Xm_Main_Window_Widget_Class_Method_SEP2()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();
  
  return (Wcls_WidgetID_To_WIDGETOBJ(XmMainWindowSep2(widget_id)));
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <mainwinwidget> :SEP3)
 *       ==> returns the WIDGETOBJ corresponding to the third separator widget.
 *
 * Widget XmMainWindowSep3(w)
 *  Widget w;
 ******************************************************************************/
LVAL Xm_Main_Window_Widget_Class_Method_SEP3()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();
  
  return (Wcls_WidgetID_To_WIDGETOBJ(XmMainWindowSep3(widget_id)));
}
#endif				/* WINTERP_MOTIF_11 */

/******************************************************************************
 *
 ******************************************************************************/
Wc_MainW_Init()
{
  LVAL o_XM_MAIN_WINDOW_WIDGET_CLASS;

  o_XM_MAIN_WINDOW_WIDGET_CLASS =
    Wcls_Create_Subclass_Of_WIDGET_CLASS("XM_MAIN_WINDOW_WIDGET_CLASS",
					 xmMainWindowWidgetClass);

  xladdmsg(o_XM_MAIN_WINDOW_WIDGET_CLASS, ":SET_AREAS",
	   FTAB_Xm_Main_Window_Widget_Class_Method_SET_AREAS);

  xladdmsg(o_XM_MAIN_WINDOW_WIDGET_CLASS, ":SEP1",
	   FTAB_Xm_Main_Window_Widget_Class_Method_SEP1);

  xladdmsg(o_XM_MAIN_WINDOW_WIDGET_CLASS, ":SEP2",
	   FTAB_Xm_Main_Window_Widget_Class_Method_SEP2);

#ifdef WINTERP_MOTIF_11
  xladdmsg(o_XM_MAIN_WINDOW_WIDGET_CLASS, ":SEP3",
	   FTAB_Xm_Main_Window_Widget_Class_Method_SEP3);
#endif				/* WINTERP_MOTIF_11 */

}
