/*
 * $Id: xv_ct.h,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
#ifndef CT_XV_H
#define CT_XV_H

/*
 * ct_xv.h - common vars used in converting Pixwin stuff to Server_images
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 
/* key values for XV_KEY_DATA */
#define PANEL_MENU_KEY	200
#define MENU_KEY	201
#define PANEL_KEY	202
#define PANEL_ITEM_KEY	203

/* variables used for Xlib drawing */
extern Display	*mydisplay;
extern Drawable drawable, drawable2;
extern int screen;
extern GC gc, gcc, gccs;
extern XGCValues gc_val;
extern XFontStruct *xfont;
extern unsigned long foregr, backgr;
extern Window m_root;
extern Pixmap iconPixmap, icon, na_icon, rev_icon;
extern int m_x, m_y;
extern unsigned int m_width, m_height;
extern unsigned int m_border, m_depth;
extern unsigned int g_width, g_height;

#endif
