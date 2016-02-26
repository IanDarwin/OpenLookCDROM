#ident	"@(#)atom.c	26.24	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

Atom	AtomAtomPair;
Atom	AtomChangeState;
Atom	AtomClass;
Atom	AtomClientWindow;
Atom	AtomColorMapWindows;
Atom	AtomDecorAdd;
Atom	AtomDecorClose;
Atom	AtomDecorDel;
Atom	AtomDecorFooter;
Atom	AtomDecorHeader;
Atom	AtomDecorPin;
Atom	AtomDecorResize;
Atom	AtomDelete;
Atom	AtomDeleteWindow;
Atom	AtomDfltBtn;
Atom	AtomLeftFooter;
Atom	AtomLength;
Atom	AtomListLength;
Atom	AtomMenuFull;
Atom	AtomMenuLimited;
Atom	AtomMultiple;
Atom	AtomName;
Atom	AtomNone;
Atom	AtomOlwmNoFocusWin;
Atom	AtomPinIn;
Atom	AtomPinOut;
Atom	AtomProtocols;
Atom	AtomPushpinState;
Atom	AtomRightFooter;
Atom	AtomSaveYourself;
Atom	AtomSunViewEnv;
Atom	AtomTakeFocus;
Atom	AtomTargets;
Atom	AtomTimestamp;
Atom	AtomUser;
Atom	AtomWMClass;
Atom	AtomWMName;
Atom	AtomWMIconName;
Atom	AtomWMHints;
Atom	AtomWMNormalHints;
Atom	AtomWMSizeHints;
Atom	AtomWMTransientFor;
Atom	AtomWMState;
Atom	AtomWTBase;
Atom	AtomWTCmd;
Atom	AtomWTHelp;
Atom	AtomWTNotice;
Atom	AtomWTOther;
Atom	AtomWinAttr;
Atom	AtomWindowBusy;
Atom	AtomSunLedMap;
Atom	AtomSunWMProtocols;
Atom	AtomSunWindowState;
Atom	AtomSunDragDropDSDM;
Atom	AtomSunDragDropInterest;
Atom	AtomSunDragDropSiteRects;
Atom	AtomSunOLWinAttr5;
Atom	AtomDecorIconName;
Atom	AtomSunReReadMenuFile;
#ifdef OW_I18N_L4
Atom	AtomCompoundText;
Atom	AtomDecorIMStatus;
Atom	AtomLeftIMStatus;
Atom	AtomRightIMStatus;
#endif

/***************************************************************************
* Global functions
***************************************************************************/

/*
 * InitAtoms -- initialize the atoms needed to communicate with Open
 *	Look clients
 */
void
InitAtoms(dpy)
Display	*dpy;
{
	/* ICCCM specific flags */
	AtomColorMapWindows = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", False);
	AtomWMState = XInternAtom(dpy, "WM_STATE", False);
	AtomChangeState = XInternAtom(dpy, "WM_CHANGE_STATE" , False);
	AtomProtocols = XInternAtom(dpy, "WM_PROTOCOLS" , False);
	AtomTakeFocus = XInternAtom(dpy, "WM_TAKE_FOCUS" , False);
	AtomSaveYourself = XInternAtom(dpy, "WM_SAVE_YOURSELF" , False);
	AtomDeleteWindow = XInternAtom(dpy, "WM_DELETE_WINDOW" , False);

	/* Predefined atoms - referenced from ClientDistributeProperties */
	AtomWMName = XA_WM_NAME;
	AtomWMIconName = XA_WM_ICON_NAME;
	AtomWMClass = XA_WM_CLASS;
	AtomWMHints = XA_WM_HINTS;
	AtomWMNormalHints = XA_WM_NORMAL_HINTS;
	AtomWMSizeHints = XA_WM_SIZE_HINTS;
	AtomWMTransientFor = XA_WM_TRANSIENT_FOR;

	/* OpenLook specific flags */
	AtomWinAttr = XInternAtom(dpy, "_OL_WIN_ATTR" , False);
	AtomPushpinState = XInternAtom(dpy, "_OL_PIN_STATE" , False);
	AtomWindowBusy = XInternAtom(dpy, "_OL_WIN_BUSY" , False);
	AtomLeftFooter = XInternAtom(dpy, "_OL_WINMSG_ERROR" , False);
	AtomRightFooter = XInternAtom(dpy, "_OL_WINMSG_STATE" , False);
	AtomPinOut = XInternAtom(dpy, "_OL_PIN_OUT" , False);
	AtomDecorResize = XInternAtom(dpy, "_OL_DECOR_RESIZE" , False);
	AtomWTBase = XInternAtom(dpy, "_OL_WT_BASE" , False);
	AtomDecorFooter = XInternAtom(dpy, "_OL_DECOR_FOOTER" , False);
	AtomDecorAdd = XInternAtom(dpy, "_OL_DECOR_ADD" , False);
	AtomDecorDel = XInternAtom(dpy, "_OL_DECOR_DEL" , False);
	AtomDecorPin = XInternAtom(dpy, "_OL_DECOR_PIN" , False);
	AtomWTCmd = XInternAtom(dpy, "_OL_WT_CMD" , False);
	AtomPinIn = XInternAtom(dpy, "_OL_PIN_IN" , False);
	AtomNone = XInternAtom(dpy, "_OL_NONE" , False);
	AtomWTNotice = XInternAtom(dpy, "_OL_WT_NOTICE" , False);
	AtomMenuFull = XInternAtom(dpy, "_OL_MENU_FULL" , False);
	AtomDecorHeader = XInternAtom(dpy, "_OL_DECOR_HEADER" , False);
	AtomWTHelp = XInternAtom(dpy, "_OL_WT_HELP" , False);
	AtomMenuLimited = XInternAtom(dpy, "_OL_MENU_LIMITED" , False);
	AtomDecorClose = XInternAtom(dpy, "_OL_DECOR_CLOSE" , False);
	AtomWTOther = XInternAtom(dpy, "_OL_WT_OTHER" , False);
	AtomOlwmNoFocusWin = XInternAtom(dpy,"_SUN_OLWM_NOFOCUS_WINDOW",False);
	AtomDfltBtn = XInternAtom(dpy, "_OL_DFLT_BTN", False);
	AtomDecorIconName = XInternAtom(dpy, "_OL_DECOR_ICON_NAME", False);
#ifdef OW_I18N_L4
	AtomDecorIMStatus = XInternAtom(dpy, "_OL_DECOR_IMSTATUS", False);
	AtomLeftIMStatus = XInternAtom(dpy, "_OL_WINMSG_IMSTATUS", False);
	AtomRightIMStatus = XInternAtom(dpy, "_OL_WINMSG_IMPREEDIT", False);
#endif

	/* ICCCM selection atoms */
	AtomAtomPair = XInternAtom(dpy,"ATOM_PAIR",False);
	AtomClientWindow = XInternAtom(dpy, "CLIENT_WINDOW",False);
	AtomClass = XInternAtom(dpy,"CLASS",False);
	AtomDelete = XInternAtom(dpy,"DELETE",False);
	AtomMultiple = XInternAtom(dpy,"MULTIPLE",False);
	AtomLength = XInternAtom(dpy,"LENGTH",False);
	AtomListLength = XInternAtom(dpy,"LIST_LENGTH",False);
	AtomName = XInternAtom(dpy,"NAME",False);
	AtomTargets = XInternAtom(dpy,"TARGETS",False);
	AtomTimestamp = XInternAtom(dpy,"TIMESTAMP",False);
	AtomUser = XInternAtom(dpy,"USER",False);
#ifdef OW_I18N_L4
	AtomCompoundText = XInternAtom(dpy, "COMPOUND_TEXT" , False);
#endif

	/* SunView environment */
	AtomSunViewEnv = XInternAtom(dpy,"_SUN_SUNVIEW_ENV",False);

	/* Sun window manager atoms */
	AtomSunLedMap = XInternAtom(dpy,"_SUN_LED_MAP",False);
	AtomSunWMProtocols = XInternAtom(dpy,"_SUN_WM_PROTOCOLS",False);
	AtomSunWindowState = XInternAtom(dpy,"_SUN_WINDOW_STATE",False);
	AtomSunOLWinAttr5 = XInternAtom(dpy,"_SUN_OL_WIN_ATTR_5",False);
	AtomSunReReadMenuFile = 
			XInternAtom(dpy,"_SUN_WM_REREAD_MENU_FILE",False);

	/* Sun drag-and-drop atoms */

	AtomSunDragDropInterest =
	    XInternAtom(dpy, "_SUN_DRAGDROP_INTEREST", False);
	AtomSunDragDropDSDM =
	    XInternAtom(dpy, "_SUN_DRAGDROP_DSDM", False);
	AtomSunDragDropSiteRects =
	    XInternAtom(dpy, "_SUN_DRAGDROP_SITE_RECTS", False);
}
