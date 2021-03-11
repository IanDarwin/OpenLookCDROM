#ifndef _CLIENT_H
#define _CLIENT_H

/* client.h,v 1.3 1994/06/02 08:52:24 me Exp */

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
 * Common includes, types, routines, etc.
 *
 * Author: Jordan K. Hubbard
 * Date: October 18th, 1990.
 * Description: Some general stuff for the client.
 *
 * Revision History:
 *
 * client.h,v
 * Revision 1.3  1994/06/02  08:52:24  me
 * Added the necessary declarations for the client changes
 *
 * Revision 1.2  1994/05/26  18:30:35  me
 * new copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 *
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include "Term.h"
#include "TermCanvas.h"
#include "xt_ops.h"

#ifdef MOTIF
#include <Xm/ScrollBar.h>
#else /* ATHENA */
#include <X11/Xaw/Scrollbar.h>
#endif /* MOTIF */

/*
 * We redefine these here just to keep a consistent coding style between
 * our widget code and client code. We can't just #include common.h from
 * the widgets, since that would break spec (and we really don't want to
 * enforce our coding standard on the world by putting them in Term.h).
 */
#define Export
#define Import extern
#define Local static
#ifdef __GNUC__
#define Inline inline
#else
#define Inline
#endif

Import Dimension HeightOf(Widget), WidthOf(Widget);
Import TermWidget Term;
Import Widget SBar, MBar, Canvas;
Import Boolean Smapped, Mmapped;
Import Widget ScrollbarCreate(Widget);
Import void XpEmuInitializeMenus(Widget);
Import Widget XpEmuCreateMbar(Widget);
Import XtGeometryResult do_layout(TermWidget, Widget, Dimension, Dimension);
Import void ScrollbarAdjust(Widget, double, double);
Import Dimension m_offset, s_offset;

#endif /* _CLIENT_H */
