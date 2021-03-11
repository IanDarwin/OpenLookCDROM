/* $Id: ConvCtrlP.h,v 1.16 1994/06/01 10:04:26 ishisone Rel $ */
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _ConversionControlP_h
#define _ConversionControlP_h

#include <X11/ShellP.h>
#include "ConvCtrl.h"
#include "ICtypes.h"

#define XtREventSelectMethod "EventSelectMethod"

/*
 *	generic conversion control widget data structure
 */

typedef struct {
    void	(*Startup)();
    void	(*Finish)();
    void	(*ChangeAttributes)();
    void	(*ChangeFocus)();
    void	(*TextChange)();
    void	(*ModeChange)();
    void	(*SelectionControl)();
    void	(*AuxControl)();
} ConversionControlClassPart;

typedef struct _ConversionControlClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ShellClassPart	shell_class;
    WMShellClassPart	wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    ConversionControlClassPart	conversionControl_class;
} ConversionControlClassRec;

extern ConversionControlClassRec conversionControlClassRec;

typedef struct {
    /* resources */
    Widget	inputobj;
    WidgetClass inputobjclass;
    WidgetClass	displayobjclass;
    WidgetClass	selectionwidgetclass;
    Window	clientwindow;			/* READ ONLY RESOURCE */
    Window	focuswindow;			/* READ ONLY RESOURCE */
    Cursor	cursor;
    EventSelectMethod	eventselectmethod;
    Atom	textencoding;
    XtCallbackList	textcallback;
    XtCallbackList	newtextcallback;
    XtCallbackList	endcallback;
    XtCallbackList	unusedeventcallback;
    Boolean	sendbackKeyPress;	/* whether unused keypress events are
					 * sent back to the focus window or not
					 */
    /* private state */
    Boolean	active;		/* Am I active (i.e. doing conversion) now? */
    Boolean	notext;		/* true iff there is no convertion text */
    Window	oldclientwindow;	/* the last client window */
    Position	client_rootx;	/* client window position */
    Position	client_rooty;
    XWindowAttributes client_attr;	/* client window attributes */
    XWindowAttributes focus_attr;	/* focus window attributes */
    Window	probewindow;	/* window for event interception */
    Boolean	createinputobj;
    Boolean	eventused;
    Boolean	endnotify;	/* endNotify callback is called */
} ConversionControlPart;

typedef struct _ConversionControlRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;	
    ConversionControlPart	ccontrol;
} ConversionControlRec;

#define XtInheritStartup		(void(*)())_XtInherit
#define XtInheritFinish			(void(*)())_XtInherit
#define XtInheritChangeAttributes	(void(*)())_XtInherit
#define XtInheritChangeFocus		(void(*)())_XtInherit
#define XtInheritTextChange		(void(*)())_XtInherit
#define XtInheritModeChange		(void(*)())_XtInherit
#define XtInheritSelectionControl	(void(*)())_XtInherit
#define XtInheritAuxControl		(void(*)())_XtInherit

/*
 * ConversionControlClass methods:
 *
 * void (*Startup)(Widget w, unsigned long mask, ConversionAttributes *attrs)
 *	called from CControlStartCoversion() at conversion startup.
 *	all the attributes specified in attrs and client window ID are
 *	guaranteed to be valid (ie you don't have to check their validity).
 *
 * void (*Finish)(Widget w)
 *	called from CControlEndConversion() at conversion finish.
 *	also called when the client window is destroyed, so take care
 *	of XErrors, namely BadWindow.
 *
 * void (*ChangeAttributes)(Widget w, unsigned long mask,
 *				ConversionAttributes *attrs)
 *	called from CControlChangeAttributes when conversion attributes
 *	are changed. like Startup, all the attributes are guaranteed to
 *	be valid.
 *
 * void (*ChangeFocus)(Widget w, int set)
 *	called from CControlChangeFocus at focus change.  argument 'set'
 *	indicates the new focus state.  if it is non-zero, focus is set,
 *	if it is zero, focus is unset.
 *
 * void (*TextChange)(Widget w)
 *	called when converting text has changed.
 *
 * void (*ModeChange)(Widget w)
 *	called when input mode has changed.
 *
 * void (*SelectionControl)(Widget w, ICSelectionControlArg *arg)
 *	called to control candidate slection.
 *	see InputConv.h for type ICSelectionControlArg.
 *
 * void (*AuxControl)(Widget w, ICAuxControlArg *arg)
 *	called to control auxiliary area.
 *	see InputConv.h for type ICAuxControlArg.
 */

#endif
