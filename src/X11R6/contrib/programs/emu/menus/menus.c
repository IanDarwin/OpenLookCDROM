#if !defined(lint) && !defined(__clipper__)
     static char *rcsid = "menus.c,v 1.3 1994/06/02 20:09:42 me Exp";
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
 * Menu code for the emu client.
 *
 * Author: Thomas Bagli
 * Date: June 28th, 1990
 * Description: This file contains  code implementing menus for
 *		the emu client. Either Athena or Motif menus may
 *		be chosen.
 *
 * Revision History:
 *
 * menus.c,v
 * Revision 1.3  1994/06/02  20:09:42  me
 * Broke up the menu creation function to allow for seperate
 * creation of the menu bar
 *
 * Revision 1.2  1994/05/26  21:01:38  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:43  me
 * Initial import into CVS
 *
 * Revision 1.26  92/10/16  16:58:18  me
 * More fixes by Steve Crooks
 * 
 * Revision 1.24  91/09/25  11:45:18  tom
 * Fixed bogus "String class" in XtGetSubresources calls.  Surprisingly
 * this was only noticed when compiled with R5.
 * 
 * Revision 1.23  91/09/24  16:23:57  tom
 * menu_item_mark becomes a resource
 * 
 * Revision 1.22  91/03/05  16:43:05  jkh
 * Memory tracing added.
 * 
 * Revision 1.21  90/12/20  17:11:18  jkh
 * Fixed bogus NULL check for string in get_token().
 * 
 * Revision 1.19  90/10/22  09:37:58  tom
 * Fixing up menus for Motif 1.1
 *
 */

#include "menus.h"

/*
 *    XpEmuInitializeMenus :
 *    public menu initialization function.
 */

Export void
XpEmuInitializeMenus(Widget parent)
{
#ifdef MOTIF
#else /* ATHENA */
     XtRegisterGrabAction (XpEmuPopupMenu, True,
			   (ButtonPressMask | ButtonReleaseMask),
			   GrabModeAsync, GrabModeAsync);
#endif /* MOTIF */
     XtAddActions (actionsList, XtNumber (actionsList));
}


Export Widget
XpEmuCreateMbar(Widget parent)
{
    /* menubar initialization. */
    Arg args[8];
    Cardinal i, nargs, number_rows;
#ifdef MOTIF
#else
    WidgetList children;
#endif 
     
    char menu_button_name[16];
                /* NOTE: Compiler complains about 'potential uninitialized */
                /* reference to "menuButton"'.  There's got to be a better */
                /* way to fix it.                                          */
    Widget menuBar, menuButton = NULL;

    static XtResource menubarResourcesList[] = {
	{ XpNnumberColumns, XpCNumberColumns, XtRInt, sizeof (int),
	    XtOffset (struct _MenubarResources *, number_columns),
	    XtRString, (XtPointer)"0",
	 },
	 { XpNnumberMenus, XpCNumberMenus, XtRInt, sizeof (int),
	    XtOffset (struct _MenubarResources *, number_menus),
	    XtRString, (XtPointer)"1",
	 },
    };
    static MenubarResourcesRec menubarResources;
     
    nargs = 0;
    XtSetArg (args[nargs], XtNborderWidth, 1);		nargs++;
#ifdef MOTIF
    XtSetArg (args[nargs], XmNpacking, XmPACK_COLUMN);	nargs++;
    menuBar = XmCreateMenuBar (parent, "menuBar", args, nargs);
    XtManageChild (menuBar);

#else /* ATHENA */
    menuBar = XtCreateWidget ("menuBar", formWidgetClass,
	parent, args, nargs);
#endif /* MOTIF */

#ifdef DEBUG
    debug ("XpEmuCreateMBar: w=0x%x, XtName=%s",
	 menuBar, XtName (menuBar));
#endif

    XtGetSubresources (menuBar, &menubarResources,
	XtName (menuBar), MENUBAR_CLASS_STR, menubarResourcesList,
	XtNumber (menubarResourcesList), (ArgList)NULL, (Cardinal)0);

    if (menubarResources.number_menus < 1)
	menubarResources.number_menus = 1;
    if (menubarResources.number_columns < 1)
	menubarResources.number_columns = menubarResources.number_menus;

    number_rows =
	(menubarResources.number_menus + 1) / menubarResources.number_columns;

#ifdef MOTIF
    /*
     *    Motif MenuBar is a horizontally-oriented RowColumn widget,
     *    so XmNnumColumns indicates how many rows are to be built.
     */
    if (menubarResources.number_columns < menubarResources.number_menus) {
	nargs = 0;
	XtSetArg (args[nargs], XmNnumColumns, number_rows); nargs++;
	XtSetValues (menuBar, args, nargs);
    }
#endif /* MOTIF */

    for (i = 1; i < menubarResources.number_menus + 1; i++) {
	sprintf (menu_button_name, "menuButton%d", i);
#ifdef MOTIF
	menuButton = XtCreateManagedWidget (menu_button_name,
	    BUTTON_WIDGET_CLASS, menuBar, (ArgList)NULL, (Cardinal)0);

#else /* ATHENA */
	nargs = 0;
	XtSetArg (args[nargs], XtNchildren, &children); nargs++;
	XtGetValues (menuBar, args, nargs);

	nargs = 0;
	if (i > menubarResources.number_columns) {
	    XtSetArg (args[nargs], XtNfromVert,
		children[i - menubarResources.number_columns - 1]);
	    nargs++;
	}
	if ((i - 1) % menubarResources.number_columns) {
	    XtSetArg (args[nargs], XtNfromHoriz, children[i - 2]);
	    nargs++;
	}
	menuButton = XtCreateManagedWidget (menu_button_name,
	    BUTTON_WIDGET_CLASS, menuBar, args, nargs);
#endif /* MOTIF */

#ifdef DEBUG
	debug ("XpEmuInitializeMenus: w=0x%x, XtName=%s",
	    menuButton, XtName (menuButton));
#endif
    }

    return menuBar;
}


/*
 *    XpEmuPopupMenu :
 *    Action Routine expects the first parameter to be a menu name.
 *    Additional parameters, if any, are interpreted as menu action
 *    ROP numbers, which are defined in the resource file.
 */

Export void
XpEmuPopupMenu(Widget w,
	       XEvent *event,	/* unused */
	       String *params,  /* menu name and ROPs */
	       Cardinal *param_count)      /* > 0 */
{
    Local XtResource menuResourcesList[] = {
	{ XpNnumberItems, XpCNumberItems, XtRInt, sizeof (int),
	    XtOffset (struct _MenuResources *, number_items),
	    XtRString, (XtPointer)NULL,
	},
	{ XpNitemLines, XpCItemLines, XtRString, sizeof (String),
	    XtOffset (struct _MenuResources *, item_lines),
	    XtRString, (XtPointer)NULL,
	},
    };
    Local MenuResourcesRec menuResources;
    char menu_path[64];
    Cardinal pc;
    Widget menu_id;

    if (*param_count == 0) {
	warn ("XpEmuPopupMenu name not properly specified.\n");
	return;
    }
     
#ifdef DEBUG
    debug ("XpEmuPopupMenu: w=0x%x, XtName=%s, menu_name=%s",
	 w, XtName (w), params[0]);
#endif
#if (1 == 0) /* NOTE: nested comments not allowed in ANSI */
    while (!XtIsComposite (reference))
	reference = XtParent (reference);

#ifdef MOTIF
    reference = XtParent (XtParent (XtParent (XtParent (w)));
#else /* ATHENA */
    reference = XtParent (XtParent (XtParent (w)));
#endif /* MOTIF */
#endif
    
    sprintf (menu_path, "*%s", params[0]);
/*
#ifdef DEBUG
    debug ("XpEmuPopupMenu: reference=0x%x, XtName=%s, menu_path=%s",
	 reference, XtName (reference), menu_path);
#endif

    if ((menu_id = XtNameToWidget (reference, menu_path)) == NULL) {
*/
    if ((menu_id = XtNameToWidget (w, menu_path)) == NULL) {
	XtGetSubresources (w, &menuResources, params[0], MENU_CLASS_STR,
	    menuResourcesList, XtNumber (menuResourcesList),
	    (ArgList)NULL, (Cardinal)0);
	menu_id = DoCreateMenu (w, params[0], menuResources.number_items,
	    menuResources.item_lines);
    }

    /* Execute any specified ROPs. */
    for (pc = 1; pc < *param_count; pc++)
	DoMenuDispatch (menu_id, params[pc]);

#ifdef MOTIF
    XmMenuPosition (menu_id, (XButtonPressedEvent *)event);
    XtManageChild (menu_id);
#else /* ATHENA */
    XtCallActionProc (w, "XawPositionSimpleMenu", event, params, 1);
    XtCallActionProc (w, "MenuPopup", event, params, 1);
#endif /* MOTIF */
}


/*
 *    XpEmuMenuAction :
 *    Routine for IOP calls.  Sets / resets menu item sensitivity
 *    and menu item toggle marks.
 */

Export void
XpEmuMenuAction(Widget w, String name, int action)
{
    Arg args[1];
    Cardinal nargs;
    Widget menu_item_id;

#ifdef MOTIF
#else /* ATHENA */
    static XtResource menuItemResourcesList[] = {
	{ XpNmenuItemMark, XpCMenuItemMark, XtRBitmap, sizeof (Pixmap),
	    XtOffset (struct _MenuItemResources *, menu_item_mark),
	    XtRString, (XtPointer)NULL,
	},
    };
    static MenuItemResourcesRec menuItemResources;
#endif /* MOTIF */

#ifdef DEBUG
    debug ("XpEmuMenuAction: w=0x%x, XtName=%s, name=%s, action=%d",
	 w, XtName (w), name, action);
#endif
    
    if ((menu_item_id = XtNameToWidget (w, name)) == NULL) {
	warn ("Unknown menu and/or item name : %s in %s.", name, XtName (w));
	return;
    }

#ifdef MOTIF
#else /* ATHENA */
    XtGetSubresources (menu_item_id, &menuItemResources,
	XtName (menu_item_id), MENUENTRY_CLASS_STR, menuItemResourcesList,
	XtNumber (menuItemResourcesList), (ArgList)NULL, (Cardinal)0);
#endif /* MOTIF */

    nargs = 0;
    switch (action) {
    case MENU_ITEM_ACTIVATE:
	XtSetArg (args[nargs], XtNsensitive, True); nargs++;
	break;

    case MENU_ITEM_DEACTIVATE:
	XtSetArg (args[nargs], XtNsensitive, False); nargs++;
	break;

    case MENU_ITEM_MARK:
#ifdef MOTIF
#else /* ATHENA */
	XtSetArg (args[nargs], XtNleftBitmap,
	    menuItemResources.menu_item_mark);
	nargs++;
#endif /* MOTIF */
	break;

    case MENU_ITEM_UNMARK:
#ifdef MOTIF
#else /* ATHENA */
	XtSetArg (args[nargs], XtNleftBitmap, None); nargs++;
#endif /* MOTIF */
	break;

    default:
	warn ("Unknown menu action %d for menu item %s.", action, name);
	return;
    }
    XtSetValues (menu_item_id, args, nargs);
}


#ifdef TEST
/*
 *    DoMenuDispatch
 *    emu client program function; it's a dummy for test purposes.
 */

Export void
DoMenuDispatch(Widget w, String str)
{
#ifdef DEBUG
    debug ("DoMenuDispatch: w=0x%x, XtName=%s, action=%s",
	  w, XtName (w), str);
#endif
}
#endif /* TEST */


/*
 *    DoCreateMenu
 *    DoInteract :
 *    Private Menu Routines
 */

Local Widget
DoCreateMenu(Widget w, String menu_name, Cardinal number_items,
	     String item_lines)
{
    Cardinal i;
    Widget menu, dummy, item_widget;
     
    static XtCallbackRec cb[2] = {
	{ (XtCallbackProc)DoInteract, (XtPointer)NULL },
	{ (XtCallbackProc)NULL, (XtPointer)NULL }
    };
    static Arg args[] = { { CALLBACK_REC, (XtArgVal)cb } };
 
    String lines = XtNewString (item_lines);
    char *line = get_token (lines);

#ifdef DEBUG
    debug ("DoCreateMenu: w=0x%x, XtName=%s, menu_name=%s\n\
	number_items=%d\n        item_lines=%s",
	w, XtName (w), menu_name, number_items, item_lines);
#endif
     
#ifdef MOTIF
    /*
     * Arrgl, This seems *not* to be documented, that you can't create
     * a PopupMenu directly byt have to use a dummy parent popup shell
     */
    dummy = XtCreatePopupShell("dummy", transientShellWidgetClass,
			      (Widget) w, NULL, 0);
    menu = XmCreatePopupMenu (dummy, menu_name, (ArgList)NULL, (Cardinal)0);

    item_widget = XtCreateManagedWidget (menu_name,
	xmLabelGadgetClass, menu, (ArgList)NULL, (Cardinal)0);
    item_widget = XtCreateManagedWidget ("titleSeparator",
	xmSeparatorGadgetClass, menu, (ArgList)NULL, (Cardinal)0);

#else /* ATHENA */
    menu = XtCreatePopupShell (menu_name, simpleMenuWidgetClass,
	w, (ArgList)NULL, (Cardinal)0);
#endif /* MOTIF */
     
    for (i = 1; i <= number_items; i++) {
	char item_name[10];

	sprintf (item_name, "item%d", i);
	item_widget = XtCreateManagedWidget (item_name, 
	    ITEM_WIDGET_CLASS, menu, args, (Cardinal)1);

#ifdef DEBUG
	debug ("DoCreateMenu: item_widget=0x%x, item_name=%s",
	    item_widget, item_name);
#endif

	if (line != NULL && ((strcmp (item_name, line)) == 0)) {
	    item_widget = XtCreateManagedWidget ("line", 
		LINE_WIDGET_CLASS, menu, (ArgList)NULL,
		(Cardinal)0);
	    line = get_token ((char *)NULL);
	}
    }
    XtFree (lines);
    return menu;
}

/*ARGSUSED*/
Local void
DoInteract(Widget w, XtPointer closure, XtPointer data)
{
    Arg args[2];
    Cardinal nargs;
    String action = NULL;

    static XtResource itemResourcesList[] = {
	{ XpNaction, XpCAction, XtRString, sizeof (String),
	    XtOffset (struct _ItemResources *, action),
	    XtRString, (XtPointer)NULL,
	},
    };
    static ItemResourcesRec itemResources;

    XtGetSubresources (XtParent (w), &itemResources, XtName (w),
	MENUENTRY_CLASS_STR, itemResourcesList, XtNumber (itemResourcesList),
	(ArgList)NULL, (Cardinal)0);

    /*  Use the menu_item label if no "action" resource exists. */
    if ((action = itemResources.action) == NULL) {
	nargs = 0;
	XtSetArg (args[nargs], XtNlabel, &action);	nargs++;
	XtGetValues (w, args, nargs);
    }
    DoMenuDispatch (w, action);
}

/*    get_token:
 *    Private routine that works like strtok.  The first call to this
 *    function should have the string to be dismantled as its parameter.
 *    All subsequent calls should have "NULL" as the parameter.  In both
 *    cases it returns either a null character terminated token or "NULL"
 *    if no token is found.
 */
Local char *
get_token(char *string)
{
    register char *str, *end;
    static char *savestr;

    if (string == NULL)
	string = savestr;

    if (string == NULL)
	return (NULL);

    /* Find start of "item". */
    str = index (string, 'i');
    if (str == NULL || *str == '\0')
	return (NULL);

    /* Find end of "item[0-9]". */
    end = str + 5;
    /* Check for trailing "[0-9]"'s. */
    for (; isdigit (*end); end++)
	;
    *end = '\0';

    savestr = end + 1;
    if (*savestr == '\0' || savestr == NULL)
	savestr = NULL;

    return (str);
}
