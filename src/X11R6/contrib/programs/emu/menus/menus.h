/* menus.h,v 1.2 1994/05/26 21:01:41 me Exp */

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
 * Menu header file for the emu client.
 *
 * Author: Thomas Bagli
 * Date: June 12th, 1990.
 * Description:
 *
 * Revision History:
 *
 * menus.h,v
 * Revision 1.2  1994/05/26  21:01:41  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:43  me
 * Initial import into CVS
 *
 * Revision 1.10  91/09/25  11:47:05  tom
 * Fixed bogus "String class" in XtGetSubresources calls.  Surprisingly
 * this was only noticed when compiled with R5.
 * 
 * Revision 1.9  91/09/24  16:22:53  tom
 * menu_item_mark becomes a resource
 * 
 */


#include "common.h"

#ifdef MOTIF

#if defined(hpux)
#include <X11/Xlib.h>
#include <stddef.h>
#endif

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CascadeB.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>

#else /* ATHENA */
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#endif /* MOTIF */


#ifdef MOTIF_ONE_ZERO
/* For Motif 1.0 Xt, from MIT Xt/Intrinsic.h */
typedef char* XtPointer;
#endif /* MOTIF_ONE_ZERO */


/* Action Procedures */

Export void XpEmuPopupMenu(Widget, XEvent *, String *, Cardinal *);

static XtActionsRec actionsList[] = {
    { "XpEmuPopupMenu",	XpEmuPopupMenu },
};


#ifdef TEST
Local void DoMenuDispatch(Widget, String);
#else /* !TEST */
Import void DoMenuDispatch(Widget, String);
#endif /* TEST */


/* Private Routines */
Local Widget DoCreateMenu(Widget, String, Cardinal, String);
Local char *get_token(char *);

/* menu items Callback Function */
Local void DoInteract(Widget, XtPointer, XtPointer);


typedef struct _MenuResources {
    Cardinal number_items;
    String item_lines;
} MenuResourcesRec, *MenuResources;

typedef struct _MenubarResources {
    int number_columns, number_menus;
} MenubarResourcesRec, *MenubarResources;

#ifdef MOTIF
#else /* ATHENA */
/* menu item checkmark pixmap */
typedef struct _MenuItemResources {
    Pixmap menu_item_mark;
} MenuItemResourcesRec, *MenuItemResources;
#endif /* MOTIF */

typedef struct _ItemResources {
    String action;
} ItemResourcesRec, *ItemResources;

#define XpNnumberItems "numberItems"
#define XpCNumberItems "NumberItems"

#define XpNitemLines "itemLines"
#define XpCItemLines "ItemLines"

#define XpNnumberColumns "numberColumns"
#define XpCNumberColumns "NumberColumns"

#define XpNnumberMenus "numberMenus"
#define XpCNumberMenus "NumberMenus"

#ifdef MOTIF
#else /* ATHENA */
#define XpNmenuItemMark "menuItemMark"
#define XpCMenuItemMark "menuItemMark"
#endif /* MOTIF */

#define XpNaction "action"
#define XpCAction "Action"


#ifdef MOTIF
#define BUTTON_WIDGET_CLASS	xmCascadeButtonWidgetClass
#define CALLBACK_REC		XmNactivateCallback
#define ITEM_WIDGET_CLASS	xmPushButtonGadgetClass
#define LINE_WIDGET_CLASS	xmSeparatorGadgetClass
#define MENUBAR_CLASS_STR	"XmRowColumn"
#define MENU_CLASS_STR		"XmRowColumn"
#define MENUENTRY_CLASS_STR	"XmPushButtonGadget"

#else /* ATHENA */
#define BUTTON_WIDGET_CLASS	menuButtonWidgetClass
#define CALLBACK_REC		XtNcallback
#define ITEM_WIDGET_CLASS	smeBSBObjectClass
#define LINE_WIDGET_CLASS	smeLineObjectClass
#define MENUBAR_CLASS_STR	"Form"
#define MENU_CLASS_STR		"SimpleMenu"
#define MENUENTRY_CLASS_STR	"SmeBSB"
#endif /* MOTIF */
