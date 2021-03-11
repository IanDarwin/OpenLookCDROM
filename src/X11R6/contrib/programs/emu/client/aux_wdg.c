#if !defined(lint) && !defined(__clipper__)
     static char *rcsid = "aux_wdg.c,v 1.5 1994/06/03 10:58:37 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 *
 * Author: Jordan K. Hubbard
 * Date: August 31th, 1990.
 * Description: This handles all the scrolling schrott for the emu
 *		client.
 *
 * Revision History:
 *
 * aux_wdg.c,v
 * Revision 1.5  1994/06/03  10:58:37  me
 * The scrollbar thumb wasn't updated properly when it wasn't mapped.
 * Added a facility to track the thumb size and position while the
 * bar isn't mapped.
 *
 * Revision 1.4  1994/06/02  08:52:58  me
 * Fixed a (hopefully) last bug in the Scrollbar placing
 *
 * Revision 1.3  1994/06/02  07:47:10  me
 * Put the toolkit dependend code into separate files,
 * Cleaned up the geometry layout and management routine
 * Added support for dynamically addable and removable decorations
 *
 * Revision 1.2  1994/05/26  18:30:14  me
 * new copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 *
 * Revision 1.7  92/02/26  11:34:12  me
 * Steve Crooks clix port and general code cleanup
 */

#include "client.h"
#include <ctype.h>

/*
 * Convenience functions.
 */
Export Dimension
HeightOf(Widget w)
{
     Dimension height;
     Arg args[2];
     int i;

     i = 0;
     XtSetArg(args[i], XtNheight, &height);	i++;
     XtGetValues(w, args, i);
     return height;
}

Export Dimension
WidthOf(Widget w)
{
     Dimension width;
     Arg args[2];
     int i;

     i = 0;
     XtSetArg(args[i], XtNwidth, &width);	i++;
     XtGetValues(w, args, i);
     return width;
}

Export Dimension
BorderOf(Widget w)
{
     Dimension bw;
     Arg args[2];
     int i;

     i = 0;
     XtSetArg(args[i], XtNborderWidth, &bw);	i++;
     XtGetValues(w, args, i);
     return bw;
}

/*
 * Handle the positioning of children in the term widget. The term
 * widget just acts like a container and passes control to us whenever
 * something needs to be done regarding the layout of its children.
 *
 * "self" is always the term widget ID. "w" is the child causing
 * the layout request or NULL if the layout request is an externally
 * requested resize of the term widget. If w is the term widget itself,
 * (w == self) then this is the initial Realize request.
 * 
 * The "width" and "height" members are the width and height request
 * members, but TAKE HEED. These "height" and "width" values aren't always
 * relative to the same widget! If "w" is NULL or equal to "self", then the
 * width and height are for the term widget. If w is something else (hopefully
 * the canvas), then they're relative to that widget.
 *
 * IMPORTANT NOTE: Though this routine is in the client, it's LOGICALLY
 * part of the widget since it is doing the widget's internal geometry
 * management. For that reason, we MUST use things like XtResizeWidget()
 * to change height and width (even though the Xt Intrinsics guide says
 * clients should _not_ do this) since using XtSetValues() will put us into
 * an infinite loop! This layout cruft should be considered an "extension"
 * to the term widget, not a part of the true client code.
 *
 */

/* Offset of the canvas to allow for a little space around the edges */
Dimension c_offset = 1;

Export XtGeometryResult
do_layout(TermWidget self, Widget child, Dimension width, Dimension height)
{
     Dimension bCanvas = BorderOf(Canvas);
     Dimension yCanvas = bCanvas + c_offset, xCanvas = bCanvas + c_offset;
     Dimension m_h = 0;
     Dimension s_w = 0;
     Dimension wCanvas, hCanvas;
     Dimension m_bdr = 0, s_bdr = 0;
     Dimension m_all = 0, s_all = 0;
     Dimension b_width, b_height;

     if (Mmapped) {
	  m_h = HeightOf(MBar);
	  m_bdr = BorderOf(MBar);
	  m_all = m_h + 2 * m_bdr + m_offset;
     }

     if (Smapped) {
	  s_w = WidthOf(SBar);
	  s_bdr = BorderOf(SBar);
	  s_all = s_w + 2 * s_bdr + s_offset;
     }

     b_width = (Smapped ? s_all : 0) + 2 * bCanvas + 2 * c_offset;
     b_height = (Mmapped ? m_all : 0) + 2 * bCanvas + 2 * c_offset;

     if (child) {
	  int i;
	  Arg args[10];
	  Dimension o_width = width;
	  Dimension o_height = height;

	  o_width += b_width;
	  o_height += b_height;
	  wCanvas = width;
	  hCanvas = height;

	  if (Mmapped) {
	       /* Move menu bar to hide top and left border */
	       XtMoveWidget(MBar, m_offset, m_offset);
	       yCanvas = m_all + bCanvas + c_offset;
	  }
	  /* Move scrollbar down and over */
	  if (Smapped)  {
	       XtMoveWidget(SBar, s_offset, m_all + s_offset);
	       xCanvas = s_all + bCanvas + c_offset;
	  }
	  XtMoveWidget(Canvas, xCanvas, yCanvas);

	  /* Change our own size */
	  i = 0;
	  XtSetArg(args[i], XtNwidth, o_width);		i++;
	  XtSetArg(args[i], XtNheight, o_height);	i++;
	  XtSetValues((Widget)self, args, i);
     }
     else {
	  /*
	   * Adjust the height and width properly for the canvas
	   * size this was a resize request, not a geometry request.
	   * Also, since this is now the term size, we need to adjust the
	   * scrollbar height so that it's shorter if the menu bar is present.
	   */
	  hCanvas = height - m_all - 2 * c_offset;
	  wCanvas = width - s_all - 2 * c_offset;
     }

     /* Give the canvas the size it wants, modulo frobs */
     XtResizeWidget(Canvas, wCanvas, hCanvas, 0);

     if (Smapped)
	  XtResizeWidget(SBar, s_w,
			 hCanvas + 2 * bCanvas + 2 * c_offset
			 - 2 * s_offset - 2 * s_bdr,
			 1);

     if (Mmapped)
	  XtResizeWidget(MBar,
			 wCanvas + 2 * bCanvas + 2 * c_offset
			 - 2 * m_offset - 2 * m_bdr +
			 (SBar ? s_all : 0),
			 m_h, 1);

     /*
      * Tell our parent about the base hints
      */
     XtVaSetValues(XtParent((Widget)self),
		   XtNbaseWidth,	b_width,
		   XtNbaseHeight,	b_height,
		   NULL);

     /*
      * This is naive, yes, but we're not smart enough to handle the
      * failure case anyway, so why sweat it?
      */
     return XtGeometryYes;
}
     
/*
 * Manage or unmanage the given decorations. Right now we only
 * accept "SBar" and "MBar"
 */
Export void
flip_decorations(TermWidget term, String str)
{
     if (strcmp(str, "MBar") == 0) {
	  /*
	   * The Menubar
	   */
	  if (Mmapped) {
	       /*
		* Unmanage the MBar
		*/
	       Mmapped = False;
	       XtUnmanageChild(MBar);
	  } else {
	       /*
		* Manage it
		*/
	       Mmapped = True;
	       XtManageChild(MBar);
	  }
     }
     else if (strcmp(str, "SBar") == 0) {
	  /*
	   * The Scrollbar
	   */
	  if (Smapped) {
	       /*
		* Unmanage the SBar
		*/
	       Smapped = False;
	       XtUnmanageChild(SBar);
	  } else {
	       /*
		* Manage it
		*/
	       Smapped = True;
	       XtManageChild(SBar);

	       /*
		* Update the thumb
		*/
	       ScrollbarAdjust(NULL, 0., 0.);
	  }
     }

     /*
      * call the layout routine by hand 
      */
     do_layout(term, (Widget)Canvas, WidthOf(Canvas), HeightOf(Canvas));
}

/*
 * Dispatch a request for menu item 'w'. Right now, we don't use 'w'
 * for anything, but might in the future if we have to deal with multiple
 * term widgets with multiple menus, for example.
 */
/*ARGSUSED*/
Export void
DoMenuDispatch(Widget w, String str)
{
     Import int atoi();

     if (!str)
	  fatal("MenuDispatch: NULL string passed");

     while (isspace(*str))
	  ++str;
     if (isdigit(*str))
	  XpTermDoRop(Term, atoi(str));
     else if (str[0] == 'G' && str[1] == 'E' && str[2] == 'O'
	      && str[3] == ' ') {
	  flip_decorations(Term, str + 4);
     }
     else
	  XpTermDoEmulation(Term, str);
}
