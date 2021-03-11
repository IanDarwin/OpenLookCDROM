/*
** 1994    Arthur David Olson
**
** The X Consortium, and any party obtaining a copy of these files from
** the X Consortium, directly or indirectly, is granted, free of charge, a
** full and unrestricted irrevocable, world-wide, paid up, royalty-free,
** nonexclusive right and license to deal in this software and
** documentation files (the "Software"), including without limitation the
** rights to use, copy, modify, merge, publish, distribute, sublicense,
** and/or sell copies of the Software, and to permit persons who receive
** copies from any such party to do so.  This license includes without
** limitation a license to do the foregoing actions under any patents of
** the party supplying this software to the X Consortium.
*/

#include "stdio.h"
#include "ctype.h"
#include "errno.h"
#include "setjmp.h"
#include "X11/Xos.h"
#include "X11/Xlib.h"
#include "X11/Xproto.h"
#include "X11/cursorfont.h"
#include "X11/Xmu/WinUtil.h"

#ifndef __STDC__
#define const
#endif /* !defined __STDC__ */

static const char	elsieid[] = "@(#)dox.c	9.1";

/*
** typedefs
*/

typedef struct {
	const char * const	name;
	const unsigned long	value;
} nv_T;

typedef enum {
	EXACT, LESSCOMMONHEAD, LESSCOMMONTAIL, WITHIN, ABBREVIATING
} matchtype_T;

/*
** function declarations
*/

static void		aloha();
static XArc *		arcs();
static int		catcher();
static int		chrhi();
static int		chrlo();
static Bool		ciequal();
static int		doerror();
static Bool		dump();
static void *		erealloc();
static char **		getfields();
static char *		getline();
static void		handle();
static int		iae();
static int		iaearg();
static Atom		iat();
static Bool		ibo();
static unsigned		ibu();
static unsigned int	ici();
static Colormap		ico();
static int		icparg();
static int		ics();
static int		icsarg();
static int		icssarg();
static Cursor		icu();
static Drawable		idr();
static int		ieqarg();
static Font		ifo();
static int		ifssarg();
static int		ifu();
static GC		igc();
static int		ignore();
static unsigned long	igcc();
static int		igm();
static int		iin();
static int		ijs();
static unsigned		ikbm();
static KeyCode		ikc();
static KeySym		iks();
static KeySym		iksfunc();
static long		ilofunc();
static int		ils();
static int		ipb();
static Pixmap		ipi();
static unsigned long	ipx();
static int		isacarg();
static int		isamarg();
static int		iscdmarg();
static long		isem();
static int		isfrarg();
static int		isfsarg();
static int		ishead();
static int		isifarg();
static int		isn();
static int		isncarg();
static int		issmarg();
static char *		ist();
static Time		iti();
static unsigned long	iuem();
static unsigned		iui();
static unsigned long	iul();
static Window		iwi();
static XID		ixi();
int			main();
static Bool		mull();
static void		myCloseDisplay();
static void		myDrawArcs();
static void		myDrawLines();
static void		myDrawPoints();
static void		myDrawRectangles();
static void		myDrawSegments();
static void		myFillArcs();
static void		myFillPolygon();
static void		myFillRectangles();
static void		myMaskEvent();
static void		myNextEvent();
static void		myPeekEvent();
static void		myWindowEvent();
static Window		namedwindow();
static char *		nonnull();
static void		o0xfunc();
static void		oat();
static void		obits();
static void		obo();
static void		oco();
static void		odr();
static void		oev();
static void		ogc();
static void		oin();
static void		okc();
static void		okeepco();
static void		okeepcu();
static void		okeepfo();
static void		okeeppi();
static void		okeepwi();
static void		okeyvec();
static void		oks();
static void		olo();
static void		oname();
static void		ost();
static void		ostat();
static void		otag();
static void		oti();
static void		oul();
static void		owi();
static void		oxi();
static void		oxifunc();
static Window		pickwindow();
static XPoint *		points();
static XRectangle *	rectangles();
static XSegment *	segments();
static void		spnl();
static void		strhi();
static void		strlo();
static unsigned long	sum();
static int		try();
static void		usage();
static unsigned long	value();
static int		valuefunc();
static void		wild2exit();
static XID		xifunc();

/*
** name/value tables
*/

static const nv_T	alloweventsarg[] = {		/* X.h */
{ "AllowEvents argument" },
{ "AsyncPointer",			AsyncPointer },
{ "SyncPointer",			SyncPointer },
{ "AsyncKeyboard",			AsyncKeyboard },
{ "SyncKeyboard",			SyncKeyboard },
{ "ReplayPointer",			ReplayPointer },
{ "ReplayKeyboard",			ReplayKeyboard },
{ "AsyncBoth",				AsyncBoth },
{ "SyncBoth",				SyncBoth },
{ NULL },
};

static const nv_T	changepropertyarg[] = {		/* X.h */
{ "ChangeProperty argument",		8 },
{ "PropModeReplace",			PropModeReplace },
{ "PropModePrepend",			PropModePrepend },
{ "PropModeAppend",			PropModeAppend },
{ NULL }
};

static const nv_T	changesavesetarg[] = {		/* X.h */
{ "ChangeSaveSet argument",		7 },
{ "SetModeInsert",			SetModeInsert },
{ "SetModeDelete",			SetModeDelete },
{ NULL }
};

static const nv_T	circulatesubwindowsarg[] = {	/* X.h */
{ "CirculateSubwindows argument" },
{ "RaiseLowest",			RaiseLowest },
{ "LowerHighest",			LowerHighest },
{ NULL }
};

static const nv_T	eventsqueuedarg[] = {		/* Xlib.h */
{ "EventsQueued argument",		6 },
{ "QueuedAlready",			QueuedAlready },
{ "QueuedAfterFlush",			QueuedAfterFlush },
{ "QueuedAfterReading",			QueuedAfterReading },
{ NULL }
};

static const nv_T	forcescreensaverarg[] = {	/* X.h */
{ "ForceScreenSaver argument",		11 },
{ "ScreenSaverActive",			ScreenSaverActive },
{ "ScreenSaverReset",			ScreenSaverReset },
{ NULL }
};

static const nv_T	setaccesscontrolarg[] = {	/* X.h */
{ "SetAccessControl argument",		(unsigned) -6 },
{ "EnableAccess",			EnableAccess },
{ "DisableAccess",			DisableAccess },
{ NULL }
};

static const nv_T	setarcmodearg[] = {		/* X.h */
{ "SetArcMode argument",		3 },
{ "ArcChord",				ArcChord },
{ "ArcPieSlice",			ArcPieSlice },
{ NULL }
};

static const nv_T	setclosedownmodearg[] = {	/* X.h */
{ "SetCloseDownMode argument" },
{ "DestroyAll",				DestroyAll },
{ "RetainPermanent",			RetainPermanent },
{ "RetainTemporary",			RetainTemporary },
{ NULL }
};

static const nv_T	setfillrulearg[] = {		/* X.h */
{ "SetFillRule argument",		(unsigned) -4 },
{ "EvenOddRule",			EvenOddRule },
{ "WindingRule",			WindingRule },
{ NULL }
};

static const nv_T	setfillstylearg[] = {		/* X.h */
{ "SetFillStyle argument",		4 },
{ "FillSolid",				FillSolid },
{ "FillTiled",				FillTiled },
{ "FillStippled",			FillStippled },
{ "FillOpaqueStippled",			FillOpaqueStippled },
{ NULL }
};

static const nv_T	setinputfocusarg[] = {		/* X.h */
{ "SetInputFocus argument",		8 },
{ "RevertToParent",			RevertToParent },
{ "RevertToPointerRoot",		RevertToPointerRoot },
{ "RevertToNone",			RevertToNone },
{ NULL }
};

static const nv_T	setsubwindowmodearg[] = {	/* X.h */
{ "SetSubwindowMode argument" },
{ "ClipByChildren",			ClipByChildren },
{ "IncludeInferiors",			IncludeInferiors },
{ NULL }
};

static const nv_T	storenamedcolorarg[] = {	/* X.h */
{ "StoreNamedColor argument",		2 },
{ "DoRed",				DoRed },
{ "DoGreen",				DoGreen },
{ "DoBlue",				DoBlue },
{ NULL }
};

static const nv_T	allowexposures[] = {		/* X.h */
{ "exposures name",			(unsigned) -9 },
{ "DontAllowExposures",			DontAllowExposures },
{ "AllowExposures",			AllowExposures },
{ "DontDefaultExposures",		DefaultExposures },
{ NULL }
};

static const nv_T	buttons[] = {			/* X.h */
{ "button name" },
{ "AnyButton",				AnyButton },
{ "Button1",				Button1 },
{ "Button2",				Button2 },
{ "Button3",				Button3 },
{ "Button4",				Button4 },
{ "Button5",				Button5 },
{ NULL }
};

static const nv_T	capstyles[] = {			/* X.h */
{ "cap-style name",			3 },
{ "CapNotLast",				CapNotLast },
{ "CapButt",				CapButt },
{ "CapRound",				CapRound },
{ "CapProjecting",			CapProjecting },
{ NULL }
};

static const nv_T	colormapchanges[] = {		/* X.h */
{ "colormap-change name",		8 },
{ "ColormapInstalled",			ColormapInstalled },
{ "ColormapUninstalled",		ColormapUninstalled },
{ NULL }
};

static const nv_T	configuremasks[] = {		/* X.h */
{ "configure-window mask",		2 },
{ "CWX",				CWX },
{ "CWY",				CWY },
{ "CWWidth",				CWWidth },
{ "CWHeight",				CWHeight },
{ "CWBorderWidth",			CWBorderWidth },
{ "CWSibling",				CWSibling },
{ "CWStackMode",			CWStackMode },
{ NULL }
};

static const nv_T	eventmasks[] = {		/* X.h */
{ "event-mask name",			(unsigned) -4 },
{ "NoEventMask",			NoEventMask },
{ "KeyPressMask",			KeyPressMask },
{ "KeyReleaseMask",			KeyReleaseMask },
{ "ButtonPressMask",			ButtonPressMask },
{ "ButtonReleaseMask",			ButtonReleaseMask },
{ "EnterWindowMask",			EnterWindowMask },
{ "LeaveWindowMask",			LeaveWindowMask },
{ "PointerMotionMask",			PointerMotionMask },
{ "PointerMotionHintMask",		PointerMotionHintMask },
{ "Button1MotionMask",			Button1MotionMask },
{ "Button2MotionMask",			Button2MotionMask },
{ "Button3MotionMask",			Button3MotionMask },
{ "Button4MotionMask",			Button4MotionMask },
{ "Button5MotionMask",			Button5MotionMask },
{ "ButtonMotionMask",			ButtonMotionMask },
{ "KeymapStateMask",			KeymapStateMask },
{ "ExposureMask",			ExposureMask },
{ "VisibilityChangeMask",		VisibilityChangeMask },
{ "StructureNotifyMask",		StructureNotifyMask },
{ "ResizeRedirectMask",			ResizeRedirectMask },
{ "SubstructureNotifyMask",		SubstructureNotifyMask },
{ "SubstructureRedirectMask",		SubstructureRedirectMask },
{ "FocusChangeMask",			FocusChangeMask },
{ "PropertyChangeMask",			PropertyChangeMask },
{ "ColormapChangeMask",			ColormapChangeMask },
{ "OwnerGrabButtonMask",		OwnerGrabButtonMask },
{ NULL }
};

static const nv_T	events[] = {			/* X.h */
{ "event name" },
{ "Error",				0 },
{ "Reply",				1 },
{ "KeyPress",				KeyPress },
{ "KeyRelease",				KeyRelease },
{ "ButtonPress",			ButtonPress },
{ "ButtonRelease",			ButtonRelease },
{ "MotionNotify",			MotionNotify },
{ "EnterNotify",			EnterNotify },
{ "LeaveNotify",			LeaveNotify },
{ "FocusIn",				FocusIn },
{ "FocusOut",				FocusOut },
{ "KeymapNotify",			KeymapNotify },
{ "Expose",				Expose },
{ "GraphicsExpose",			GraphicsExpose },
{ "NoExpose",				NoExpose },
{ "VisibilityNotify",			VisibilityNotify },
{ "CreateNotify",			CreateNotify },
{ "DestroyNotify",			DestroyNotify },
{ "UnmapNotify",			UnmapNotify },
{ "MapNotify",				MapNotify },
{ "MapRequest",				MapRequest },
{ "ReparentNotify",			ReparentNotify },
{ "ConfigureNotify",			ConfigureNotify },
{ "ConfigureRequest",			ConfigureRequest },
{ "GravityNotify",			GravityNotify },
{ "ResizeRequest",			ResizeRequest },
{ "CirculateNotify",			CirculateNotify },
{ "CirculateRequest",			CirculateRequest },
{ "PropertyNotify",			PropertyNotify },
{ "SelectionClear",			SelectionClear },
{ "SelectionRequest",			SelectionRequest },
{ "SelectionNotify",			SelectionNotify },
{ "ColormapNotify",			ColormapNotify },
{ "ClientMessage",			ClientMessage },
{ "MappingNotify",			MappingNotify },
{ NULL }
};

static const nv_T	functions[] = {			/* X.h */
{ "function name",			2 },
{ "GXclear",				GXclear },
{ "GXand",				GXand },
{ "GXandReverse",			GXandReverse },
{ "GXcopy",				GXcopy },
{ "GXandInverted",			GXandInverted },
{ "GXnoop",				GXnoop },
{ "GXxor",				GXxor },
{ "GXor",				GXor },
{ "GXnor",				GXnor },
{ "GXequiv",				GXequiv },
{ "GXinvert",				GXinvert },
{ "GXorReverse",			GXorReverse },
{ "GXcopyInverted",			GXcopyInverted },
{ "GXorInverted",			GXorInverted },
{ "GXnand",				GXnand },
{ "GXset",				GXset },
{ NULL }
};

static const nv_T	gccomponents[] = {		/* X.h */
{ "GC-component name",			2 },
{ "GCFunction",				GCFunction },
{ "GCPlaneMask",			GCPlaneMask },
{ "GCForeground",			GCForeground },
{ "GCBackground",			GCBackground },
{ "GCLineWidth",			GCLineWidth },
{ "GCLineStyle",			GCLineStyle },
{ "GCCapStyle",				GCCapStyle },
{ "GCJoinStyle",			GCJoinStyle },
{ "GCFillStyle",			GCFillStyle },
{ "GCFillRule",				GCFillRule },
{ "GCTile",				GCTile },
{ "GCStipple",				GCStipple },
{ "GCTileStipXOrigin",			GCTileStipXOrigin },
{ "GCTileStipYOrigin",			GCTileStipYOrigin },
{ "GCFont",				GCFont },
{ "GCSubwindowMode",			GCSubwindowMode },
{ "GCGraphicsExposures",		GCGraphicsExposures },
{ "GCClipXOrigin",			GCClipXOrigin },
{ "GCClipYOrigin",			GCClipYOrigin },
{ "GCClipMask",				GCClipMask },
{ "GCDashOffset",			GCDashOffset },
{ "GCDashList",				GCDashList },
{ "GCArcMode",				GCArcMode },
{ NULL }
};

static const nv_T	grabmodes[] = {			/* X.h */
{ "grab-mode name",			8 },
{ "GrabModeSync",			GrabModeSync },
{ "GrabModeAsync",			GrabModeAsync },
{ NULL }
};

static const nv_T	graphicsexposes[] = {		/* Xproto.h */
{ "expose name",			6 },
{ "X_CopyArea",				X_CopyArea },
{ "X_CopyPlane",			X_CopyPlane },
{ NULL }
};

static const nv_T	joinstyles[] = {		/* X.h */
{ "join-style name",			4 },
{ "JoinMiter",				JoinMiter },
{ "JoinRound",				JoinRound },
{ "JoinBevel",				JoinBevel },
{ NULL }
};

static const nv_T	keybutmasks[] = {		/* X.h */
{ "modifier name",			(unsigned) -4 },
{ "AnyModifier",			AnyModifier },
{ "ShiftMask",				ShiftMask },
{ "LockMask",				LockMask },
{ "ControlMask",			ControlMask },
{ "Mod1Mask",				Mod1Mask },
{ "Mod2Mask",				Mod2Mask },
{ "Mod3Mask",				Mod3Mask },
{ "Mod4Mask",				Mod4Mask },
{ "Mod5Mask",				Mod5Mask },
{ "Button1Mask",			Button1Mask },
{ "Button2Mask",			Button2Mask },
{ "Button3Mask",			Button3Mask },
{ "Button4Mask",			Button4Mask },
{ "Button5Mask",			Button5Mask },
{ NULL }
};

static const nv_T	linestyles[] = {		/* X.h */
{ "line-style name",			4 },
{ "LineSolid",				LineSolid },
{ "LineOnOffDash",			LineOnOffDash },
{ "LineDoubleDash",			LineDoubleDash },
{ NULL }
};

static const nv_T	mappings[] = {			/* X.h */
{ "mapping name",			7 },
{ "MappingModifier",			MappingModifier },
{ "MappingPointer",			MappingPointer },
{ "MappingKeyboard",			MappingKeyboard },
{ NULL }
};

static const nv_T	notifies[] = {			/* X.h */
{ "notify-mode name",			6 },
{ "NotifyNormal",			NotifyNormal },
{ "NotifyGrab",				NotifyGrab },
{ "NotifyUngrab",			NotifyUngrab },
{ "NotifyWhileGrabbed",			NotifyWhileGrabbed },
{ NULL }
};

static const nv_T	notifydetails[] = {		/* X.h */
{ "notify-detail name",			6 },
{ "NotifyAncestor",			NotifyAncestor },
{ "NotifyVirtual",			NotifyVirtual },
{ "NotifyInferior",			NotifyInferior },
{ "NotifyNonlinear",			NotifyNonlinear },
{ "NotifyNonlinearVirtual",		NotifyNonlinearVirtual },
{ "NotifyPointer",			NotifyPointer },
{ "NotifyPointerRoot",			NotifyPointerRoot },
{ "NotifyDetailNone",			NotifyDetailNone },
{ NULL }
};

static const nv_T	places[] = {			/* X.h */
{ "place name",				7 },
{ "PlaceOnTop",				PlaceOnTop },
{ "PlaceOnBottom",			PlaceOnTop },
{ NULL }
};

static const nv_T	preferblankings[] = {		/* X.h */
{ "prefer-blanking name",		(unsigned) -8 },
{ "DontPreferBlanking",			DontPreferBlanking },
{ "PreferBlanking",			PreferBlanking },
{ "DontDefaultBlanking",		DefaultBlanking },
{ NULL }
};

static const nv_T	propertychanges[] = {		/* X.h */
{ "property-change name",		8 },
{ "PropertyNewValue",			PropertyNewValue },
{ "PropertyDelete",			PropertyDelete },
{ NULL }
};

static const nv_T	visibilities[] = {		/* X.h */
{ "visibilty name",			10 },
{ "VisibilityUnobscured",		VisibilityUnobscured },
{ "VisibilityPartiallyObscured",	VisibilityPartiallyObscured },
{ "VisibilityFullyObscured",		VisibilityFullyObscured },
{ NULL }
};

static const nv_T	requestcodes[] = {		/* Xproto.h */
{ "request code",			2 },
{ "X_CreateWindow",			X_CreateWindow },
{ "X_ChangeWindowAttributes",		X_ChangeWindowAttributes },
{ "X_GetWindowAttributes",		X_GetWindowAttributes },
{ "X_DestroyWindow",			X_DestroyWindow },
{ "X_DestroySubwindows",		X_DestroySubwindows },
{ "X_ChangeSaveSet",			X_ChangeSaveSet },
{ "X_ReparentWindow",			X_ReparentWindow },
{ "X_MapWindow",			X_MapWindow },
{ "X_MapSubwindows",			X_MapSubwindows },
{ "X_UnmapWindow",			X_UnmapWindow },
{ "X_UnmapSubwindows",			X_UnmapSubwindows },
{ "X_ConfigureWindow",			X_ConfigureWindow },
{ "X_CirculateWindow",			X_CirculateWindow },
{ "X_GetGeometry",			X_GetGeometry },
{ "X_QueryTree",			X_QueryTree },
{ "X_InternAtom",			X_InternAtom },
{ "X_GetAtomName",			X_GetAtomName },
{ "X_ChangeProperty",			X_ChangeProperty },
{ "X_DeleteProperty",			X_DeleteProperty },
{ "X_GetProperty",			X_GetProperty },
{ "X_ListProperties",			X_ListProperties },
{ "X_SetSelectionOwner",		X_SetSelectionOwner },
{ "X_GetSelectionOwner",		X_GetSelectionOwner },
{ "X_ConvertSelection",			X_ConvertSelection },
{ "X_SendEvent",			X_SendEvent },
{ "X_GrabPointer",			X_GrabPointer },
{ "X_UngrabPointer",			X_UngrabPointer },
{ "X_GrabButton",			X_GrabButton },
{ "X_UngrabButton",			X_UngrabButton },
{ "X_ChangeActivePointerGrab",		X_ChangeActivePointerGrab },
{ "X_GrabKeyboard",			X_GrabKeyboard },
{ "X_UngrabKeyboard",			X_UngrabKeyboard },
{ "X_GrabKey",				X_GrabKey },
{ "X_UngrabKey",			X_UngrabKey },
{ "X_AllowEvents",			X_AllowEvents },
{ "X_GrabServer",			X_GrabServer },
{ "X_UngrabServer",			X_UngrabServer },
{ "X_QueryPointer",			X_QueryPointer },
{ "X_GetMotionEvents",			X_GetMotionEvents },
{ "X_TranslateCoords",			X_TranslateCoords },
{ "X_WarpPointer",			X_WarpPointer },
{ "X_SetInputFocus",			X_SetInputFocus },
{ "X_GetInputFocus",			X_GetInputFocus },
{ "X_QueryKeymap",			X_QueryKeymap },
{ "X_OpenFont",				X_OpenFont },
{ "X_CloseFont",			X_CloseFont },
{ "X_QueryFont",			X_QueryFont },
{ "X_QueryTextExtents",			X_QueryTextExtents },
{ "X_ListFonts",			X_ListFonts },
{ "X_ListFontsWithInfo",		X_ListFontsWithInfo },
{ "X_SetFontPath",			X_SetFontPath },
{ "X_GetFontPath",			X_GetFontPath },
{ "X_CreatePixmap",			X_CreatePixmap },
{ "X_FreePixmap",			X_FreePixmap },
{ "X_CreateGC",				X_CreateGC },
{ "X_ChangeGC",				X_ChangeGC },
{ "X_CopyGC",				X_CopyGC },
{ "X_SetDashes",			X_SetDashes },
{ "X_SetClipRectangles",		X_SetClipRectangles },
{ "X_FreeGC",				X_FreeGC },
{ "X_ClearArea",			X_ClearArea },
{ "X_CopyArea",				X_CopyArea },
{ "X_CopyPlane",			X_CopyPlane },
{ "X_PolyPoint",			X_PolyPoint },
{ "X_PolyLine",				X_PolyLine },
{ "X_PolySegment",			X_PolySegment },
{ "X_PolyRectangle",			X_PolyRectangle },
{ "X_PolyArc",				X_PolyArc },
{ "X_FillPoly",				X_FillPoly },
{ "X_PolyFillRectangle",		X_PolyFillRectangle },
{ "X_PolyFillArc",			X_PolyFillArc },
{ "X_PutImage",				X_PutImage },
{ "X_GetImage",				X_GetImage },
{ "X_PolyText8",			X_PolyText8 },
{ "X_PolyText16",			X_PolyText16 },
{ "X_ImageText8",			X_ImageText8 },
{ "X_ImageText16",			X_ImageText16 },
{ "X_CreateColormap",			X_CreateColormap },
{ "X_FreeColormap",			X_FreeColormap },
{ "X_CopyColormapAndFree",		X_CopyColormapAndFree },
{ "X_InstallColormap",			X_InstallColormap },
{ "X_UninstallColormap",		X_UninstallColormap },
{ "X_ListInstalledColormaps",		X_ListInstalledColormaps },
{ "X_AllocColor",			X_AllocColor },
{ "X_AllocNamedColor",			X_AllocNamedColor },
{ "X_AllocColorCells",			X_AllocColorCells },
{ "X_AllocColorPlanes",			X_AllocColorPlanes },
{ "X_FreeColors",			X_FreeColors },
{ "X_StoreColors",			X_StoreColors },
{ "X_StoreNamedColor",			X_StoreNamedColor },
{ "X_QueryColors",			X_QueryColors },
{ "X_LookupColor",			X_LookupColor },
{ "X_CreateCursor",			X_CreateCursor },
{ "X_CreateGlyphCursor",		X_CreateGlyphCursor },
{ "X_FreeCursor",			X_FreeCursor },
{ "X_RecolorCursor",			X_RecolorCursor },
{ "X_QueryBestSize",			X_QueryBestSize },
{ "X_QueryExtension",			X_QueryExtension },
{ "X_ListExtensions",			X_ListExtensions },
{ "X_ChangeKeyboardMapping",		X_ChangeKeyboardMapping },
{ "X_GetKeyboardMapping",		X_GetKeyboardMapping },
{ "X_ChangeKeyboardControl",		X_ChangeKeyboardControl },
{ "X_GetKeyboardControl",		X_GetKeyboardControl },
{ "X_Bell",				X_Bell },
{ "X_ChangePointerControl",		X_ChangePointerControl },
{ "X_GetPointerControl",		X_GetPointerControl },
{ "X_SetScreenSaver",			X_SetScreenSaver },
{ "X_GetScreenSaver",			X_GetScreenSaver },
{ "X_ChangeHosts",			X_ChangeHosts },
{ "X_ListHosts",			X_ListHosts },
{ "X_SetAccessControl",			X_SetAccessControl },
{ "X_SetCloseDownMode",			X_SetCloseDownMode },
{ "X_KillClient",			X_KillClient },
{ "X_RotateProperties",			X_RotateProperties },
{ "X_ForceScreenSaver",			X_ForceScreenSaver },
{ "X_SetPointerMapping",		X_SetPointerMapping },
{ "X_GetPointerMapping",		X_GetPointerMapping },
{ "X_SetModifierMapping",		X_SetModifierMapping },
{ "X_GetModifierMapping",		X_GetModifierMapping },
{ "X_NoOperation",			X_NoOperation },
{ NULL }
};

static const nv_T	cursors[] = {			/* cursorfont.h */
{ "cursor name",			3 },
{ "XC_X_cursor",			XC_X_cursor },
{ "XC_arrow",				XC_arrow },
{ "XC_based_arrow_down",		XC_based_arrow_down },
{ "XC_based_arrow_up",			XC_based_arrow_up },
{ "XC_boat",				XC_boat },
{ "XC_bogosity",			XC_bogosity },
{ "XC_bottom_left_corner",		XC_bottom_left_corner },
{ "XC_bottom_right_corner",		XC_bottom_right_corner },
{ "XC_bottom_side",			XC_bottom_side },
{ "XC_bottom_tee",			XC_bottom_tee },
{ "XC_box_spiral",			XC_box_spiral },
{ "XC_center_ptr",			XC_center_ptr },
{ "XC_circle",				XC_circle },
{ "XC_clock",				XC_clock },
{ "XC_coffee_mug",			XC_coffee_mug },
{ "XC_cross",				XC_cross },
{ "XC_cross_reverse",			XC_cross_reverse },
{ "XC_crosshair",			XC_crosshair },
{ "XC_diamond_cross",			XC_diamond_cross },
{ "XC_dot",				XC_dot },
{ "XC_dotbox",				XC_dotbox },
{ "XC_double_arrow",			XC_double_arrow },
{ "XC_draft_large",			XC_draft_large },
{ "XC_draft_small",			XC_draft_small },
{ "XC_draped_box",			XC_draped_box },
{ "XC_exchange",			XC_exchange },
{ "XC_fleur",				XC_fleur },
{ "XC_gobbler",				XC_gobbler },
{ "XC_gumby",				XC_gumby },
{ "XC_hand1",				XC_hand1 },
{ "XC_hand2",				XC_hand2 },
{ "XC_heart",				XC_heart },
{ "XC_icon",				XC_icon },
{ "XC_iron_cross",			XC_iron_cross },
{ "XC_left_ptr",			XC_left_ptr },
{ "XC_left_side",			XC_left_side },
{ "XC_left_tee",			XC_left_tee },
{ "XC_leftbutton",			XC_leftbutton },
{ "XC_ll_angle",			XC_ll_angle },
{ "XC_lr_angle",			XC_lr_angle },
{ "XC_man",				XC_man },
{ "XC_middlebutton",			XC_middlebutton },
{ "XC_mouse",				XC_mouse },
{ "XC_pencil",				XC_pencil },
{ "XC_pirate",				XC_pirate },
{ "XC_plus",				XC_plus },
{ "XC_question_arrow",			XC_question_arrow },
{ "XC_right_ptr",			XC_right_ptr },
{ "XC_right_side",			XC_right_side },
{ "XC_right_tee",			XC_right_tee },
{ "XC_rightbutton",			XC_rightbutton },
{ "XC_rtl_logo",			XC_rtl_logo },
{ "XC_sailboat",			XC_sailboat },
{ "XC_sb_down_arrow",			XC_sb_down_arrow },
{ "XC_sb_h_double_arrow",		XC_sb_h_double_arrow },
{ "XC_sb_left_arrow",			XC_sb_left_arrow },
{ "XC_sb_right_arrow",			XC_sb_right_arrow },
{ "XC_sb_up_arrow",			XC_sb_up_arrow },
{ "XC_sb_v_double_arrow",		XC_sb_v_double_arrow },
{ "XC_shuttle",				XC_shuttle },
{ "XC_sizing",				XC_sizing },
{ "XC_spider",				XC_spider },
{ "XC_spraycan",			XC_spraycan },
{ "XC_star",				XC_star },
{ "XC_target",				XC_target },
{ "XC_tcross",				XC_tcross },
{ "XC_top_left_arrow",			XC_top_left_arrow },
{ "XC_top_left_corner",			XC_top_left_corner },
{ "XC_top_right_corner",		XC_top_right_corner },
{ "XC_top_side",			XC_top_side },
{ "XC_top_tee",				XC_top_tee },
{ "XC_trek",				XC_trek },
{ "XC_ul_angle",			XC_ul_angle },
{ "XC_umbrella",			XC_umbrella },
{ "XC_ur_angle",			XC_ur_angle },
{ "XC_watch",				XC_watch },
{ "XC_xterm",				XC_xterm },
{ NULL }
};

static const nv_T	shapes[] = {			/* X.h */
{ "shape" },
{ "Complex",				Complex },
{ "Convex",				Convex },
{ "Nonconvex",				Nonconvex },
{ NULL }
};

static const nv_T	coordmodes[] = {		/* X.h */
{ "coordinate-mode value",		9 },
{ "CoordModeOrigin",			CoordModeOrigin },
{ "CoordModePrevious",			CoordModePrevious },
{ NULL }
};

/*
** Other global variables.
*/

static Display *	display;
static int		default_screen_number;
static Window		default_root_window;
static GC		default_gc;
static Colormap		default_colormap;
static unsigned long	black_pixel;
static unsigned long	white_pixel;

static const char *	progname;

static const char *	argfuncname;
static char *		testfuncname;

static Bool		intflag;
static Bool		frameflag;
static Bool		verboseflag;

static Bool		single;
static Bool		interactive;

static jmp_buf		env;

/*
** Error handling.
*/

static void
aloha()
{
	/*
	** Is this the end, or is it only. . .
	*/
	if (interactive)
		longjmp(env, 0);
	if (display != NULL)
		XCloseDisplay(display);
	exit(1);
}

static void
wild2exit(part1, part2)
register const char * const	part1;
register const char * const	part2;
{
	(void) fprintf(stderr, "\n%s: wild", progname);
	if (part1 != NULL)
		(void) fprintf(stderr, " %s", part1);
	if (part2 != NULL)
		(void) fprintf(stderr, " %s", part2);
	(void) fprintf(stderr, "\n");
	aloha();
}

#define wildexit(message)	wild2exit((message), (char *) NULL)
#define wildrexit(message)	wild2exit("result from", (message))

/*
** Memory management.
*/

static void *
erealloc(pointer, size)
register char * const	pointer;
register int		size;
{
	register char * result;

	if (size == 0)
		size = 1;
	if (pointer == NULL)
		result = (char *) malloc((unsigned int) size);
	else	result = (char *) realloc((char *) pointer,
			(unsigned int) size);
	if (result == NULL) {
		interactive = False;
		wildrexit("getting memory");
	}
	return result;
}

#define emalloc(size)	erealloc((char *) NULL, (size))

/*
** Character and string manipulation.
*/

static char *
nonnull(cp)
register char * cp;
{
	static char	null;

	return (cp == NULL) ? &null : cp;
}

static int
chrhi(c)
register const int	c;
{
	return (isascii(c) && islower(c)) ? toupper(c) : c;
}

static int
chrlo(c)
register const int	c;
{
	return (isascii(c) && isupper(c)) ? tolower(c) : c;
}

static void
strhi(cp)
register char * cp;
{
	for ( ; *cp != '\0'; ++cp)
		*cp = chrhi(*cp);
}

static void
strlo(cp)
register char * cp;
{
	for ( ; *cp != '\0'; ++cp)
		*cp = chrlo(*cp);
}

static Bool
ciequal(ap, bp)		/* case-insensitive equality */
register const char *	ap;
register const char *	bp;
{
	ap = nonnull(ap);
	bp = nonnull(bp);
	while (chrlo(*ap) == chrlo(*bp++))
		if (*ap++ == '\0')
			return True;
	return False;
}

#define csequal(ap, bp) ((Bool) (strcmp(nonnull(ap), nonnull(bp)) == 0))

/*
** Output side primitives.
*/

static void
spnl(donl)
register const Bool	donl;
{
	static Bool	did_output;

	if (donl) {
		if (did_output) {
			(void) printf("\n");
			did_output = False;
		}
	} else {
		if (did_output)
			(void) printf(" ");
		else	did_output = True;
	}
}

#define separate()	spnl(False)

/*
** name/value functions
*/

static void
oname(val, table)
register unsigned long	val;
register const nv_T *	table;
{
	register int	i;

	for (i = 1; table[i].name != NULL; ++i)
		if (table[i].value == val) {
			ost(table[i].name);
			return;
		}
	oul(val);
}

static void
obits(val, table)
register unsigned long		val;
register const nv_T * const	table;
{
	register int	i;
	register Bool	didbit;

	separate();
	didbit = False;
	for (i = 1; table[i].name != NULL; ++i)
		if ((val & table[i].value) == table[i].value) {
			val &= ~table[i].value;
			if (didbit)
				(void) printf("+");
			else	didbit = True;
			(void) printf("%s", table[i].name);
		}
	if (!didbit || val != 0) {
		if (didbit)
			(void) printf("+");
		o0xfunc((unsigned long) val);
	}
}

static int
ishead(abbr, name)
register const char *	abbr;
register const char *	name;
{
	while (*abbr != '\0')
		if (chrlo(*abbr++) != chrlo(*name++))
			return 0;
	return 1;
}

static int
valuefunc(abbr, name, mt, arg)
register const char *		abbr;
register const char *		name;
register const matchtype_T	mt;
register const int		arg;
{
	register const char *	cp;
	register int		c;

	switch (mt) {
		case EXACT:
			return ciequal(abbr, name);
		case LESSCOMMONHEAD:
			return ciequal(abbr, name + arg);
		case LESSCOMMONTAIL:
			while (*abbr != '\0')
				if (chrlo(*abbr++) != chrlo(*name++))
					return 0;
			return strlen(name) == -arg;
		case WITHIN:
			for (cp = name; *cp != '\0'; ++cp)
				if (ishead(abbr, cp))
					return 1;
			return 0;
		case ABBREVIATING:
			while ((c = chrlo(*abbr++)) != '\0')
				do {
					if (*name == '\0')
						return 0;
				} while (chrlo(*name++) != c);
			return 1;
		default:	/* "cannot happen" */
			for ( ; ; )
				wildexit("call of valuefunc");
	}
}

static unsigned long
value(name, table)
register const char * const	name;
register const nv_T * const	table;
{
	register const nv_T *	tp;
	register const nv_T *	retp;
	register int		mt;

	for (mt = EXACT; mt <= ABBREVIATING; ++mt) {
		switch (mt) {
			case LESSCOMMONHEAD:
				if ((int) table[0].value <= 0)
					continue;
				break;
			case LESSCOMMONTAIL:
				if ((int) table[0].value >= 0)
					continue;
				break;
		}
		retp = NULL;
		for (tp = &table[1]; tp->name != NULL; ++tp)
			if (valuefunc(name, tp->name, mt,
				(int) table[0].value))
					if (mt == EXACT ||
						mt == LESSCOMMONHEAD ||
						mt == LESSCOMMONTAIL)
							return tp->value;
					else if (retp == NULL)
						retp = tp;
					else	break;
		if (retp == NULL)
			continue;
		if (tp->name == NULL)
			return retp->value;
		/*
		** Ambiguous.
		*/
		(void) fprintf(stderr,
			"%s: wild value for %s (%s)--matches both %s and %s\n",
			progname, table->name, name, retp->name, tp->name);
		for ( ; ; )
			aloha();
	}
	/*
	** No match.
	*/
	(void) fprintf(stderr, "%s: wild value for %s (%s)\n",
		progname, table->name, name);
	(void) fprintf(stderr, "%s: usable values are:", progname);
	for (tp = &table[1]; tp->name != NULL; ++tp)
		(void) fprintf(stderr, " %s", tp->name);
	(void) fprintf(stderr, "\n");
	for ( ; ; )
		aloha();
}

static unsigned long
sum(string, table)
register const char *		string;
register const nv_T * const	table;
{
	register unsigned long	result;
	register char *		cp;

	result = 0;
	while ((cp = strchr(string, '+')) != NULL) {
		*cp = '\0';
		result |= value(string, table);
		*cp = '+';
		string = ++cp;
	}
	return result | value(string, table);
}

/*
** Balance of output side.
*/

static void
o0xfunc(x)
register const unsigned long	x;
{
	if (intflag || x <= 9)
		(void) printf("%lu", x);
	else	(void) printf("0x%lx", (long) x);
}

static void
ost(string)
register const char *	string;
{
	register int	c;

	separate();
	string = nonnull(string);
	while ((c = *string++) != '\0')
		if (c == '\\')
			(void) printf("\\\\");
		else if (c == '\b')
			(void) printf("\\b");
		else if (c == '\f')
			(void) printf("\\f");
		else if (c == '\r')
			(void) printf("\\r");
		else if (c == '\v')
			(void) printf("\\v");
		else if (isascii(c) && (isprint(c) || strchr(" \t\n", c) != 0))
			(void) printf("%c", c);
		else (void) printf("\\%03o", (unsigned char) c);
}

#define FUNC(name, type, format)	static void \
					name(x) \
					register const type x; \
					{ \
						separate(); \
						(void) printf(format, x); \
					}

FUNC(oin, int, "%d")
FUNC(olo, long, "%ld")
FUNC(oul, unsigned long, "%lu")

#undef FUNC

static void
oat(x)
register const Atom	x;
{
	ost(XGetAtomName(display, x));
}

static void
obo(x)
register const Bool	x;
{
	ost(x ? "True" : "False");
}

static void
oti(x)
register const Time	x;
{
	if (x == CurrentTime)
		ost("CurrentTime");
	else	oul((unsigned long) x);
}

static void
oxi(x)
register const XID	x;
{
	separate();
	o0xfunc((unsigned long) x);
}

static void
odr(x)
register const Drawable x;
{
	oxi((XID) x);
}

static void
okc(x)
register const KeyCode	x;
{
	oul((unsigned long) x);
}

static void
oxifunc(xid, keep)
register const XID	xid;
register const Bool	keep;
{
	if (keep && single)
		(void) XSetCloseDownMode(display, RetainPermanent);
	oxi(xid);
}

#define FUNC(name, type, flag)	static void \
					name(x) \
					register type x; { \
						oxifunc((XID) x, flag); \
					}

FUNC(oco, Colormap, False)
FUNC(okeepco, Colormap, True)
FUNC(okeepcu, Cursor, True)
FUNC(okeepfo, Font, True)
FUNC(ogc, GC, False)
FUNC(okeeppi, Pixmap, True)
FUNC(owi, Window, False)
FUNC(okeepwi, Window, True)

#undef FUNC

static void
oks(x)
register const KeySym	x;
{
	ost(XKeysymToString(x));
}

static void
ostat(status)
register const Status	status;
{
	oin((int) status);
}

#define BYTES_PER_KEYMAP_VECTOR 32
#define BITS_PER_KEYMAP_BYTE	8

static void
okeyvec(vector)
const char	vector[BYTES_PER_KEYMAP_VECTOR];
{
	register int	bytenum;
	register int	bitnum;
	register int	did_output;

	separate();
	did_output = False;
	for (bytenum = 0; bytenum < BYTES_PER_KEYMAP_VECTOR; ++bytenum)
		for (bitnum = 0; bitnum < BITS_PER_KEYMAP_BYTE; ++bitnum)
			if ((vector[bytenum] & (1 << bitnum)) != 0) {
				if (did_output)
					(void) printf("+");
				else	did_output = True;
				(void) printf("%d",
					bytenum * BITS_PER_KEYMAP_BYTE +
					bitnum);
			}
	if (!did_output)
		(void) printf("None");	/* for lack of anything better */
}

static void
otag(tag)
register const char * const	tag;
{
	if (verboseflag)
		ost(tag);
}

#define otat(tag, val)		(otag(tag), oat(val))
#define otbits(tag, val, table) (otag(tag), obits((val), (table)))
#define otbo(tag, val)		(otag(tag), obo(val))
#define otco(tag, val)		(otag(tag), oco(val))
#define otdr(tag, val)		(otag(tag), odr(val))
#define otin(tag, val)		(otag(tag), oin(val))
#define otkc(tag, val)		(otag(tag), okc(val))
#define otkeyvec(tag, val)	(otag(tag), okeyvec(val))
#define otlo(tag, val)		(otag(tag), olo(val))
#define otname(tag, val, table) (otag(tag), oname((val), (table)))
#define otst(tag, val)		(otag(tag), ost(val))
#define otti(tag, val)		(otag(tag), oti(val))
#define trul(tag, val)		(otag(tag), oul(val))
#define otxi(tag, val)		(otag(tag), oxi(val))
#define otwi(tag, val)		(otag(tag), owi(val))

static void
oev(e)
const XEvent	e;
{
	oname((unsigned long) e.type, events);
	if (e.type == X_Error) {
		char	buf[512];

		otxi("resourceid", e.xerror.resourceid);
		trul("serial", e.xerror.serial);
		(void) XGetErrorText(display, e.xerror.error_code,
			buf, sizeof buf);
		otst("error_code", buf);
		otname("request_code",
			(unsigned long) e.xerror.request_code, requestcodes);
		otin("minor_code", (int) e.xerror.minor_code);
		return;
	}
	trul("serial", e.xany.serial);
	otbo("send_event", e.xany.send_event);
	/*
	trvp("display", (void *) e.xany.display);
	*/
	switch (e.type) {
		case KeyPress:
		case KeyRelease:
			otwi("window", e.xkey.window);
			otwi("root", e.xkey.root);
			otwi("subwindow", e.xkey.subwindow);
			otti("time", e.xkey.time);
			otin("x", e.xkey.x);
			otin("y", e.xkey.y);
			otin("x_root", e.xkey.x_root);
			otin("y_root", e.xkey.y_root);
			otbits("state", (unsigned long) e.xkey.state,
				keybutmasks);
			otkc("keycode", e.xkey.keycode);
			otbo("same_screen", e.xkey.same_screen);
			break;
		case ButtonPress:
		case ButtonRelease:
			otwi("window", e.xbutton.window);
			otwi("root", e.xbutton.root);
			otwi("subwindow", e.xbutton.subwindow);
			otti("time", e.xbutton.time);
			otin("x", e.xbutton.x);
			otin("y", e.xbutton.y);
			otin("x_root", e.xbutton.x_root);
			otin("y_root", e.xbutton.y_root);
			otbits("state", (unsigned long) e.xbutton.state,
				keybutmasks);
			otname("button", (unsigned long) e.xbutton.button,
				buttons);
			otbo("same_screen", e.xbutton.same_screen);
			break;
		case MotionNotify:
			otwi("window", e.xmotion.window);
			otwi("root", e.xmotion.root);
			otwi("subwindow", e.xmotion.subwindow);
			otti("time", e.xmotion.time);
			otin("x", e.xmotion.x);
			otin("y", e.xmotion.y);
			otin("x_root", e.xmotion.x_root);
			otin("y_root", e.xmotion.y_root);
			otbits("state", (unsigned long) e.xmotion.state,
				keybutmasks);
			otbo("is_hint", (Bool) e.xmotion.is_hint);
			otbo("same_screen", e.xmotion.same_screen);
			break;
		case EnterNotify:
		case LeaveNotify:
			otwi("window", e.xcrossing.window);
			otwi("root", e.xcrossing.root);
			otwi("subwindow", e.xcrossing.subwindow);
			otti("time", e.xcrossing.time);
			otin("x", e.xcrossing.x);
			otin("y", e.xcrossing.y);
			otin("x_root", e.xcrossing.x_root);
			otin("y_root", e.xcrossing.y_root);
			otname("mode", (unsigned long) e.xcrossing.mode,
				notifies);
			otname("detail", (unsigned long) e.xcrossing.detail,
				notifydetails);
			otbo("same_screen", e.xcrossing.same_screen);
			otbo("focus", e.xcrossing.focus);
			otbits("state", (unsigned long) e.xcrossing.state,
				keybutmasks);
			break;
		case FocusIn:
		case FocusOut:
			otwi("window", e.xfocus.window);
			otname("mode", (unsigned long) e.xfocus.mode,
				notifies);
			otname("detail", (unsigned long) e.xfocus.detail,
				notifydetails);
			break;
		case KeymapNotify:
			otwi("window", e.xkeymap.window);
			otkeyvec("key_vector", e.xkeymap.key_vector);
			break;
		case Expose:
			otwi("window", e.xexpose.window);
			otin("x", e.xexpose.x);
			otin("y", e.xexpose.y);
			otin("width", e.xexpose.width);
			otin("height", e.xexpose.height);
			otin("count", e.xexpose.count);
			break;
		case GraphicsExpose:
			otdr("drawable", e.xgraphicsexpose.drawable);
			otin("x", e.xgraphicsexpose.x);
			otin("y", e.xgraphicsexpose.y);
			otin("width", e.xgraphicsexpose.width);
			otin("height", e.xgraphicsexpose.height);
			otin("count", e.xgraphicsexpose.count);
			otname("major_code",
				(unsigned long) e.xgraphicsexpose.major_code,
				graphicsexposes);
			otin("minor_code", e.xgraphicsexpose.minor_code);
			break;
		case NoExpose:
			otdr("drawable", e.xnoexpose.drawable);
			otname("major_code",
				(unsigned long) e.xnoexpose.major_code,
				graphicsexposes);
			otin("minor_code", e.xnoexpose.minor_code);
			break;
		case VisibilityNotify:
			otwi("window", e.xvisibility.window);
			otname("state", (unsigned long) e.xvisibility.state,
				visibilities);
			break;
		case CreateNotify:
			otwi("parent", e.xcreatewindow.parent);
			otwi("window", e.xcreatewindow.window);
			otin("x", e.xcreatewindow.x);
			otin("y", e.xcreatewindow.y);
			otin("width", e.xcreatewindow.width);
			otin("height", e.xcreatewindow.height);
			otin("border_width", e.xcreatewindow.border_width);
			otbo("override_redirect",
				e.xcreatewindow.override_redirect);
			break;
		case DestroyNotify:
			otwi("event", e.xdestroywindow.event);
			otwi("window", e.xdestroywindow.window);
			break;
		case UnmapNotify:
			otwi("event", e.xunmap.event);
			otwi("window", e.xunmap.window);
			otbo("from_configure", e.xunmap.from_configure);
			break;
		case MapNotify:
			otwi("event", e.xmap.event);
			otwi("window", e.xmap.window);
			otbo("override_redirect", e.xmap.override_redirect);
			break;
		case MapRequest:
			otwi("parent", e.xmaprequest.parent);
			otwi("window", e.xmaprequest.window);
			break;
		case ReparentNotify:
			otwi("event", e.xreparent.event);
			otwi("window", e.xreparent.window);
			otwi("parent", e.xreparent.parent);
			otin("x", e.xreparent.x);
			otin("y", e.xreparent.y);
			otbo("override_redirect",
				e.xreparent.override_redirect);
			break;
		case ConfigureNotify:
			otwi("event", e.xconfigure.event);
			otwi("window", e.xconfigure.window);
			otin("x", e.xconfigure.x);
			otin("y", e.xconfigure.y);
			otin("width", e.xconfigure.width);
			otin("height", e.xconfigure.height);
			otin("border_width", e.xconfigure.border_width);
			otwi("above", e.xconfigure.above);
			otbo("override_redirect",
				e.xconfigure.override_redirect);
			break;
		case ConfigureRequest:
			otwi("parent", e.xconfigurerequest.parent);
			otwi("window", e.xconfigurerequest.window);
			otin("x", e.xconfigurerequest.x);
			otin("y", e.xconfigurerequest.y);
			otin("width", e.xconfigurerequest.width);
			otin("height", e.xconfigurerequest.height);
			otin("border_width", e.xconfigurerequest.border_width);
			otwi("above", e.xconfigurerequest.above);
			otin("detail", e.xconfigurerequest.detail);
			otbits("value_mask", e.xconfigurerequest.value_mask,
				configuremasks);
			break;
		case GravityNotify:
			otwi("event", e.xgravity.event);
			otwi("window", e.xgravity.window);
			otin("x", e.xgravity.x);
			otin("y", e.xgravity.y);
			break;
		case ResizeRequest:
			otwi("window", e.xresizerequest.window);
			otin("width", e.xresizerequest.width);
			otin("height", e.xresizerequest.height);
			break;
		case CirculateNotify:
			otwi("event", e.xcirculate.event);
			otwi("window", e.xcirculate.window);
			otin("place", e.xcirculate.place);
			break;
		case CirculateRequest:
			otwi("parent", e.xcirculaterequest.parent);
			otwi("window", e.xcirculaterequest.window);
			otname("place",
				(unsigned long) e.xcirculaterequest.place,
				places);
			break;
		case PropertyNotify:
			otwi("window", e.xproperty.window);
			otat("atom", e.xproperty.atom);
			otti("time", e.xproperty.time);
			otname("state", (unsigned long) e.xproperty.state,
				propertychanges);
			break;
		case SelectionClear:
			otwi("window", e.xselectionclear.window);
			otat("selection", e.xselectionclear.selection);
			otti("time", e.xselectionclear.time);
			break;
		case SelectionRequest:
			otwi("owner", e.xselectionrequest.owner);
			otwi("requestor", e.xselectionrequest.requestor);
			otat("selection", e.xselectionrequest.selection);
			otat("target", e.xselectionrequest.target);
			otat("property", e.xselectionrequest.property);
			otti("time", e.xselectionrequest.time);
			break;
		case SelectionNotify:
			otwi("requestor", e.xselection.requestor);
			otat("selection", e.xselection.selection);
			otat("target", e.xselection.target);
			otat("property", e.xselection.property);
			otti("time", e.xselection.time);
			break;
		case ColormapNotify:
			otwi("window", e.xcolormap.window);
			otco("colormap", e.xcolormap.colormap);
			otbo("new", e.xcolormap.new);
			otname("state", (unsigned long) e.xcolormap.state,
				colormapchanges);
			break;
		case ClientMessage:
			otwi("window", e.xclient.window);
			otat("message_type", e.xclient.message_type);
			otin("format", e.xclient.format);
			(void) printf(" ");
			switch (e.xclient.format) {
				register int		i;
				register int		bytesper;
				register int		n;
				register unsigned long	val;

				default:
					(void) printf("?");
					break;
				case 8:
				case 16:
				case 32:
					bytesper = e.xclient.format / 8;
					n = 20 / bytesper;
					for (i = 0; i < n; ++i) {
						if (i != 0)
							(void) printf(",");
						val = (unsigned long)
							((bytesper == 1) ?
							e.xclient.data.b[i] :
							((bytesper == 2) ?
							e.xclient.data.s[i] :
							e.xclient.data.l[i]));
						(void) printf("%lu", val);
					}
					break;
			}
			break;
		case MappingNotify:
			otwi("window", e.xmapping.window);
			otname("request", (unsigned long) e.xmapping.request,
				mappings);
			otkc("first_keycode",
				(KeyCode) e.xmapping.first_keycode);
			otin("count", e.xmapping.count);
			break;
	}
}

/*
** Input side--non-XID, non-mode stuff.
*/

static char *
ist(cp)
register char * const	cp;
{
	return cp;
}

#define FUNC(name, type, fmt1, fmt2, mess) \
		static type \
		name(cp) \
		register const char * const	cp; \
		{ \
			type	result; \
			char	dummy; \
			if (sscanf(cp, fmt1, &result) != 1 || \
				sscanf(cp, fmt2, &result, &dummy) != 1) \
					wild2exit(mess, cp); \
			return result; \
		}

FUNC(iin, int, "%i", "%i%c", "non-int argument")
FUNC(iui, unsigned int, "%u", "%u%c", "non-unsigned-int argument")
FUNC(iul, unsigned long, "%lu", "%lu%c", "non-unsigned-long argument")

#undef FUNC

static long
ilofunc(mess, cp)
register const char * const	mess;
register const char * const	cp;
{
	char *	ptr;
	long	result;

	result = strtol(cp, &ptr, 0);
	if (ptr == cp || *ptr != '\0')
		wild2exit(mess, cp);
	return result;
}

static Bool
ibo(cp)
register const char * const	cp;
{
	register long	result;

	if (ciequal(cp, "True") || ciequal(cp, "yes") || ciequal(cp, "on"))
		return True;
	if (ciequal(cp, "False") || ciequal(cp, "no") || ciequal(cp, "off"))
		return False;
	result = ilofunc("non-bool argument", cp);
	if (result != True && result != False)
		wild2exit("non-bool argument", cp);
	return result;
}

static unsigned long
ipx(cp)
register const char * const	cp;
{
	register long	result;

	if (ciequal(cp, "black") || ciequal(cp, "blackpixel"))
		return black_pixel;
	if (ciequal(cp, "white") || ciequal(cp, "whitepixel"))
		return white_pixel;
	result = ilofunc("non-pixel argument", cp);
	if (result != (unsigned long) result)
		wild2exit("non-pixel argument", cp);
	return result;
}

static KeyCode
ikc(cp)
register const char * const	cp;
{
	register long	result;

	result = ilofunc("non-keycode argument", cp);
	if ((KeyCode) result != result)
		wild2exit("non-keycode argument", cp);
	return result;
}

static int
isn(cp)
register const char * const	cp;
{
	if (ciequal(cp, "default") || ciequal(cp, "defaultscreen"))
		return default_screen_number;
	return iui(cp);
}

static KeySym
iksfunc(cp, oldks)
register const char * const	cp;
register KeySym			oldks;
{
	register KeySym newks;

	newks = XStringToKeysym(cp);
	if (newks == NoSymbol)
		return oldks;
	if (oldks != NoSymbol && oldks != newks)
		wild2exit("ambiguous keysym argument", cp);
	return newks;
}

static KeySym
iks(cp)
register const char * const	cp;
{
	register KeySym result;
	register char * buf;
	register char * dp;
	register char * tailp;

	/*
	** Trivial way.
	*/
	if (cp == NULL || *cp == '\0' || ciequal(cp, "NoSymbol"))
		return NoSymbol;
	/*
	** Easy way.
	*/
	result = XStringToKeysym(cp);
	if (result != NoSymbol)
		return result;
	/*
	** Hard way.
	*/
	buf = emalloc(strlen(cp) + 1);
	(void) strcpy(buf, cp);
	tailp = strchr(buf, '_');
	if (tailp != NULL)
		if (*++tailp == '\0')
			tailp = NULL;
	/*
	** l..l (as in "dollar")
	*/
	strlo(buf);
	result = XStringToKeysym(buf);
	/*
	** Ul..l (as in "Home")
	*/
	strlo(buf);
	*buf = chrhi(*buf);
	result = iksfunc(buf, result);
	/*
	** Ul..lUl..l (as in "BackSpace")
	*/
	strlo(buf);
	*buf = chrhi(*buf);
	for (dp = buf + 1; *dp != '\0'; ++dp) {
		*dp = chrhi(*dp);
		result = iksfunc(buf, result);
		*dp = chrlo(*dp);
	}
	/*
	** l..l_U..U (as in "kana_YA")
	*/
	if (tailp != NULL) {
		strlo(buf);
		strhi(tailp);
		result = iksfunc(buf, result);
	}
	/*
	** U..U_Ul..l (as in "KP_Divide")
	*/
	if (tailp != NULL) {
		strhi(buf);
		strlo(tailp + 1);
		result = iksfunc(buf, result);
	}
	/*
	** Ul..l_U..Ul..l (as in "Shift_Lock" and "Greek_IOTAdiaeresis")
	*/
	if (tailp != NULL) {
		strlo(buf);
		*buf = chrhi(*buf);
		for (dp = tailp; *dp != '\0'; ++dp) {
			*dp = chrhi(*dp);
			result = iksfunc(buf, result);
		}
	}
	if (result == NoSymbol)
		wild2exit("non-keysym argument", cp);
	return result;
}

/*ARGSUSED*/
static int
catcher(unused_display, unused_keep)
register const Display * const		unused_display;
register const XErrorEvent * const	unused_keep;
{
    return 0;
}

static Atom
iat(cp)
register const char * const	cp;
{
	register char * acp;
	register Atom	result;
	register Atom	an;
	register int (* oldhandler)();

	if ((result = XInternAtom(display, cp, True)) != 0)
		return result;
	oldhandler = XSetErrorHandler(catcher);
	for (an = 1; (acp = XGetAtomName(display, an)) != NULL; ++an)
		if (ciequal(cp, acp))
			if (result == 0)
				result = an;
			else	wild2exit("multiple-match atom argument", cp);
	(void) XSetErrorHandler(oldhandler);
	if (result == 0)
		wild2exit("non-atom argument", cp);
	return result;
}

static Time
iti(cp)
register const char * const	cp;
{
	register long	result;

	if (ciequal(cp, "current") || ciequal(cp, "currenttime"))
		return CurrentTime;
	result = ilofunc("non-time argument", cp);
	if (result != (Time) result)
		wild2exit("non-time argument", cp);
	return result;
}

static unsigned int
ici(cp) /* cursor index */
register const char * const	cp;
{
	register long	result;

	if (strchr("0123456789", *nonnull(cp)) == NULL)
		return value(cp, cursors);
	result = ilofunc("non-cursor-index argument", cp);
	if (result != (unsigned int) result)
		wild2exit("non-cursor-index argument", cp);
	return result;
}

/*
** Input side--XID stuff.
*/

static Window
pickwindow()
{
	register Cursor cursor;
	register Window picked_win;
	register int	ndown;
	XEvent		pickevent;

	ndown = 0;
	picked_win = None;
	cursor = XCreateFontCursor(display, XC_crosshair);
	if (XGrabPointer(display, default_root_window, False,
		ButtonPressMask | ButtonReleaseMask, GrabModeSync,
		GrabModeAsync, None, cursor, CurrentTime) != GrabSuccess)
			wildexit("result grabbing pointer");
	while (picked_win == None || ndown != 0) {
		XAllowEvents(display, SyncPointer, CurrentTime);
		XWindowEvent(display, default_root_window,
			ButtonPressMask | ButtonReleaseMask, &pickevent);
		switch (pickevent.type) {
			case ButtonPress:
				picked_win =
					(pickevent.xbutton.subwindow == None) ?
					default_root_window :
					pickevent.xbutton.subwindow;
				++ndown;
				break;
			case ButtonRelease:
				if (ndown > 0)
					--ndown;
				break;
		}
	}
	XUngrabPointer(display, CurrentTime);
	XFlush(display);
	return frameflag ? picked_win : XmuClientWindow(display, picked_win);
}

static Window
namedwindow(name, topw, liberal)
register const char * const	name;
register const Window		topw;
register const Bool		liberal;
{
	Window *	children;
	Window		dummy;
	unsigned int	nchildren;
	int		i;
	Window		w;
	Window		neww;
	char *		window_name;

	w = 0;
	if (XFetchName(display, topw, &window_name) &&
		(liberal ? ciequal(window_name, name) :
		csequal(window_name, name)))
			w = topw;
	if (!XQueryTree(display, topw, &dummy, &dummy, &children, &nchildren))
		return w;
	for (i = 0; i < nchildren; ++i) {
		neww = namedwindow(name, children[i], liberal);
		if (neww == 0)
			continue;
		if (w == 0)
			w = neww;
		else	wild2exit("multiple windows matching name", name);
	}
	if (children)
		XFree((char *) children);
	return w;
}

/*ARGSUSED*/
static int
ignore(disp, eep)
Display *	disp;
XErrorEvent *	eep;
{
	return 0;
}

#define DOWINDOW	(1 << 1)
#define DOGC		(1 << 2)
#define DOCOLORMAP	(1 << 3)
#define DOCURSOR	(1 << 4)
#define DOFONT		(1 << 5)
#define DOPIXMAP	(1 << 6)
#define DODRAWABLE	(DOWINDOW | DOPIXMAP)
#define DOALL		(DOGC | DOCOLORMAP | DOCURSOR | DOFONT | DODRAWABLE)

static XID
xifunc(cp, flags, mess)
register const char * const	cp;
register const int		flags;
register const char * const	mess;
{
	register XID	result;
	register int	matches;
	long		l;

	result = 0;
	matches = 0;
	if (ciequal(cp, "none")) {
		result = (XID) None;
		++matches;
	}
	if ((flags & DOWINDOW) != 0 && ciequal(cp, "+")) {
		result = (XID) pickwindow();
		++matches;
	}
	if ((flags & DOWINDOW) != 0 &&
		(ciequal(cp, "root") ||
		ciequal(cp, "rootwindow") ||
		ciequal(cp, "defaultroot") ||
		ciequal(cp, "defaultrootwindow"))) {
			result = (XID) default_root_window;
			++matches;
	}
	if ((flags & DOGC) != 0 &&
		(ciequal(cp, "default") || ciequal(cp, "defaultgc"))) {
			result = (XID) default_gc;
			++matches;
	}
	if ((flags & DOCOLORMAP) != 0 &&
		(ciequal(cp, "default") || ciequal(cp, "defaultcolormap"))) {
			result = (XID) default_colormap;
			++matches;
	}
	{
		char *	ptr;

		l = strtol(cp, &ptr, 0);
		if (ptr != cp && *ptr == '\0') {
			result = l;
			++matches;
		}
	}
	if ((flags & DOWINDOW) != 0 && matches == 0) {
		register Bool	liberal;
		register Window newresult;
		register int (*	oldhandler)();

		/*
		** We get to catch errors to avoid problems if a window
		** disappears between a QueryTree and a FetchName.
		*/
		oldhandler = XSetErrorHandler(ignore);
		for (liberal = False; liberal <= True; ++liberal) {
			newresult = (XID) namedwindow(cp, default_root_window,
				liberal);
			if (newresult != 0) {
				result = newresult;
				++matches;
				break;
			}
		}
		(void) XSetErrorHandler(oldhandler);
	}
	if (matches > 1)
		wild2exit("ambiguous XID identifier", cp);
	if (flags == DOGC && result != (XID) default_gc)
		wildexit("GC argument");
	return result;
}

#define FUNC(name, type, flags, mess)	static type \
					name(cp) \
					register char * cp; \
					{ \
						return (type) xifunc(cp, \
							flags, mess); \
					}

FUNC(ico, Colormap, DOCOLORMAP, "non-colormap argument")
FUNC(icu, Cursor, DOCURSOR, "non-cursor argument")
FUNC(idr, Drawable, DODRAWABLE, "non-drawable argument")
FUNC(ifo, Font, DOFONT, "non-font argument")
FUNC(igc, GC, DOGC, "non-gc argument")
FUNC(ipi, Pixmap, DOPIXMAP, "non-pixmap argument")
FUNC(iwi, Window, DOWINDOW, "non-window argument")
FUNC(ixi, XID, DOALL, "non-xid arugment")

#undef FUNC

/*
** Input side--mode stuff.
*/

#define FUNC(name, type, func, arg)	static type \
					name(cp) \
					register const char * const	cp; \
					{ \
						return func(cp, arg); \
					}

FUNC(iaearg, int, value, alloweventsarg)
FUNC(icparg, int, value, changepropertyarg)
FUNC(icsarg, int, value, circulatesubwindowsarg)
FUNC(icssarg, int, value, changesavesetarg)
FUNC(ieqarg, int, value, eventsqueuedarg)
FUNC(ifssarg, int, value, forcescreensaverarg)
FUNC(isacarg, int, value, setaccesscontrolarg)
FUNC(isamarg, int, value, setarcmodearg)
FUNC(iscdmarg, int, value, setclosedownmodearg)
FUNC(isfrarg, int, value, setfillrulearg)
FUNC(isfsarg, int, value, setfillstylearg)
FUNC(isifarg, int, value, setinputfocusarg)
FUNC(issmarg, int, value, setsubwindowmodearg)
FUNC(isncarg, int, value, storenamedcolorarg)

FUNC(iae, int, value, allowexposures)
FUNC(ibu, unsigned int, value, buttons)
FUNC(ics, int, value, capstyles)
FUNC(ifu, int, value, functions)
FUNC(igcc, unsigned long, sum, gccomponents)
FUNC(igm, int, value, grabmodes)
FUNC(ikbm, unsigned int, sum, keybutmasks)
FUNC(ijs, int, value, joinstyles)
FUNC(ils, int, value, linestyles)
FUNC(ipb, int, value, preferblankings)
/*
** Why X11 has both signed and unsigned event_masks is beyond this writer.
*/
FUNC(isem, long, sum, eventmasks)
FUNC(iuem, unsigned long, sum, eventmasks)

#undef FUNC

/*
** Main course.
*/

static const char * const *	v;
static int			n;

/*ARGSUSED*/
static Bool
dump(doname, descargs, unused_donum)
register const char * const	doname;
register const char * const	descargs;
register const int		unused_donum;
{
	(void) fprintf(stderr, " %s", doname);
	if (descargs != NULL && *descargs != '\0')
		(void) fprintf(stderr, " %s", descargs);
	(void) fprintf(stderr, "\n");
	return False;
}

static Bool
mull(doname, descargs, donum)
register const char * const	doname;
register const char * const	descargs;
register const int		donum;
{
	if (display == NULL)
		wildexit("function call with closed display");
	if (!csequal(doname, testfuncname))
		return False;
	if ((donum < 0) ? (n >= -donum) : (n == donum))
		return True;
	(void) fprintf(stderr, "%s: usage is", progname);
	if (single)
		(void) fprintf(stderr, " %s", progname);
	(void) dump(doname, descargs, donum);
	for ( ; ; )
		aloha();
}

static XArc *
arcs(strings, nstrings)
register char ** const	strings;
register const int	nstrings;
{
	register int	i;
	register int	narcs;	/* from the DEA? */
	static XArc *	result;

	if (nstrings < 0 || (nstrings % 6) != 0)
		wildexit("number of x/y/w/h/angle1/angle2 values");
	narcs = nstrings / 6;
	result = (XArc *) erealloc(result, narcs * sizeof *result);
	for (i = 0; i < narcs; ++i) {
		result[i].x = iin(strings[i * 6]);
		result[i].y = iin(strings[i * 6 + 1]);
		result[i].width = iui(strings[i * 6 + 2]);
		result[i].height = iui(strings[i * 6 + 3]);
		result[i].angle1 = iin(strings[i * 6 + 4]);
		result[i].angle2 = iin(strings[i * 6 + 5]);
	}
	return result;
}

static XPoint *
points(strings, nstrings)
register char ** const	strings;
register const int	nstrings;
{
	register int	i;
	register int	npoints;
	static XPoint * result;

	if (nstrings < 0 || (nstrings % 2) != 0)
		wildexit("number of x/y pairs");
	npoints = nstrings / 2;
	result = (XPoint *) erealloc(result, npoints * sizeof *result);
	for (i = 0; i < npoints; ++i) {
		result[i].x = iin(strings[i * 2]);
		result[i].y = iin(strings[i * 2 + 1]);
	}
	return result;
}

static XRectangle *
rectangles(strings, nstrings)
register char ** const	strings;
register const int	nstrings;
{
	register int		i;
	register int		nrects;
	static XRectangle *	result;

	if (nstrings < 0 || (nstrings % 4) != 0)
		wildexit("number of x/y/w/h values");
	nrects = nstrings / 4;
	result = (XRectangle *) erealloc(result, nrects * sizeof *result);
	for (i = 0; i < nrects; ++i) {
		result[i].x = iin(strings[i * 4]);
		result[i].y = iin(strings[i * 4 + 1]);
		result[i].width = iui(strings[i * 4 + 2]);
		result[i].height = iui(strings[i * 4 + 3]);
	}
	return result;
}

static XSegment *
segments(strings, nstrings)
register char ** const	strings;
register const int	nstrings;
{
	register int		i;
	register int		nsegs;
	static XSegment *	result;

	if (nstrings < 0 || (nstrings % 4) != 0)
		wildexit("number of x1/y1/x2/y2 values");
	nsegs = nstrings / 4;
	result = (XSegment *) erealloc(result, nsegs * sizeof *result);
	for (i = 0; i < nsegs; ++i) {
		result[i].x1 = iin(strings[i * 4]);
		result[i].y1 = iin(strings[i * 4 + 1]);
		result[i].x2 = iin(strings[i * 4 + 2]);
		result[i].y2 = iin(strings[i * 4 + 3]);
	}
	return result;
}

static void
myCloseDisplay(dpy)
register Display * const	dpy;
{
	XSync(dpy, False);
	XCloseDisplay(dpy);
	display = NULL;
}

static void
myDrawArcs(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XDrawArcs(dpy, drawable, gc, arcs(&v[2], n - 2), (n - 2) / 6);
}

static void
myDrawLines(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XDrawLines(dpy, drawable, gc, points(&v[2], n - 3), (n - 3) / 2,
		(int) value(v[n - 1], coordmodes));
}

static void
myDrawRectangles(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XDrawRectangles(dpy, drawable, gc,
		rectangles(&v[2], n - 2), (n - 2) / 4);
}

static void
myDrawSegments(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XDrawSegments(dpy, drawable, gc, segments(&v[2], n - 2), (n - 2) / 4);
}

static void
myDrawPoints(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XDrawPoints(dpy, drawable, gc, points(&v[2], n - 3), (n - 3) / 2,
		(int) value(v[n - 1], coordmodes));
}

static void
myFillArcs(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XFillArcs(dpy, drawable, gc, arcs(&v[2], n - 2), (n - 2) / 6);
}

static void
myFillPolygon(dpy, drawable, gc)	/* points.. shape mode */
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XFillPolygon(dpy, drawable, gc, points(&v[2], n - 4), (n - 4) / 2,
		(int) value(v[n - 2], shapes),
		(int) value(v[n - 1], coordmodes));
}

static void
myFillRectangles(dpy, drawable, gc)
register Display * const	dpy;
register const Drawable		drawable;
register const GC		gc;
{
	XFillRectangles(dpy, drawable, gc,
		rectangles(&v[2], n - 2), (n - 2) / 4);
}

static void
myMaskEvent(dpy, sem)
register Display * const	dpy;
register long			sem;
{
	static XEvent e;

	XMaskEvent(dpy, sem, &e);
	oev(e);
}

static void
myNextEvent(dpy)
register Display * const	dpy;
{
	static XEvent e;

	XNextEvent(dpy, &e);
	oev(e);
}

static void
myPeekEvent(dpy)
register Display * const	dpy;
{
	XEvent e;

	XPeekEvent(dpy, &e);
	oev(e);
}

static void
myWindowEvent(dpy, win, em)
register Display * const	dpy;
register Window			win;
register long			em;
{
	XEvent	e;

	XWindowEvent(dpy, win, em, &e);
	oev(e);
}

static int
try(testp)
register Bool (* const	testp)();
{
	register const char *	cp;

	cp = nonnull(argfuncname);
	if (testfuncname != NULL)
		free(testfuncname);
	testfuncname = emalloc(strlen(cp) + 1);
	(void) strcpy(testfuncname, cp + (chrlo(*cp) == 'x'));
	strlo(testfuncname);
#define L1(f0) \
		, f0(v[0])
#define L2(f0, f1) \
		L1(f0), f1(v[1])
#define L3(f0, f1, f2) \
		L2(f0, f1), f2(v[2])
#define L4(f0, f1, f2, f3) \
		L3(f0, f1, f2), f3(v[3])
#define L5(f0, f1, f2, f3, f4) \
		L4(f0, f1, f2, f3), f4(v[4])
#define L6(f0, f1, f2, f3, f4, f5) \
		L5(f0, f1, f2, f3, f4), f5(v[5])
#define L7(f0, f1, f2, f3, f4, f5, f6) \
		L6(f0, f1, f2, f3, f4, f5), f6(v[6])
#define L8(f0, f1, f2, f3, f4, f5, f6, f7) \
		L7(f0, f1, f2, f3, f4, f5, f6), f7(v[7])
#define L9(f0, f1, f2, f3, f4, f5, f6, f7, f8) \
		L8(f0, f1, f2, f3, f4, f5, f6, f7), f8(v[8])
#define L10(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9) \
		L9(f0, f1, f2, f3, f4, f5, f6, f7, f8), f9(v[9])
#define PRE(name, func, dargs, r, num)	if ((*testp)(name, dargs, num)) { \
						r(func(display
#define POST						)); \
						return True; \
					}
#define DO(name, func, r) \
		PRE(name, func, (char *) NULL, r, 0) \
		POST
#define DO1(name, func, dargs, r, f0) \
		PRE(name, func, dargs, r, 1) \
		L1(f0) POST
#define DO2(name, func, dargs, r, f0, f1) \
		PRE(name, func, dargs, r, 2) \
		L2(f0, f1) POST
#define DO3(name, func, dargs, r, f0, f1, f2) \
		PRE(name, func, dargs, r, 3) \
		L3(f0, f1, f2) POST
#define DO4(name, func, dargs, r, f0, f1, f2, f3) \
		PRE(name, func, dargs, r, 4) \
		L4(f0, f1, f2, f3) POST
#define DO5(name, func, dargs, r, f0, f1, f2, f3, f4) \
		PRE(name, func, dargs, r, 5) \
		L5(f0, f1, f2, f3, f4) POST
#define DO6(name, func, dargs, r, f0, f1, f2, f3, f4, f5) \
		PRE(name, func, dargs, r, 6) \
		L6(f0, f1, f2, f3, f4, f5) POST
#define DO7(name, func, dargs, r, f0, f1, f2, f3, f4, f5, f6) \
		PRE(name, func, dargs, r, 7) \
		L7(f0, f1, f2, f3, f4, f5, f6) POST
#define DO8(name, func, dargs, r, f0, f1, f2, f3, f4, f5, f6, f7) \
		PRE(name, func, dargs, r, 8) \
		L8(f0, f1, f2, f3, f4, f5, f6, f7) POST
#define DO9(name, func, dargs, r, f0, f1, f2, f3, f4, f5, f6, f7, f8) \
		PRE(name, func, dargs, r, 9) \
		L9(f0, f1, f2, f3, f4, f5, f6, f7, f8) POST
#define DO10(name, func, dargs, r, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9) \
		PRE(name, func, dargs, r, 10) \
		L10(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9) POST
#define DO_2(name, func, dargs, r, f0, f1) \
		PRE(name, func, dargs, r, -2) \
		L2(f0, f1) POST
	switch (*testfuncname) {
default:
case 'a':
	DO("activatescreensaver", XActivateScreenSaver, (void));
	DO1("addtosaveset", XAddToSaveSet, "window", (void), iwi);
	DO2("allowevents", XAllowEvents,
		"event_mode time", (void), iaearg, iti);
	DO("autorepeatoff", XAutoRepeatOff, (void));
	DO("autorepeaton", XAutoRepeatOn, (void));
case 'b':
	DO1("bell", XBell, "percent", (void), iin);
	DO("bitmapbitorder", XBitmapBitOrder, oin);
	DO("bitmappad", XBitmapPad, oin);
	DO("bitmapunit", XBitmapUnit, oin);
	DO1("blackpixel", XBlackPixel, "screen_number", oul, isn);
case 'c':
	DO3("changeactivepointergrab", XChangeActivePointerGrab,
		"event_mask cursor time", (void), iuem, icu, iti);
	DO5("changepointercontrol", XChangePointerControl,
"do_accel do_threshold accel_numerator accel_denominator threshold",
		(void), ibo, ibo, iin, iin, iin);
	DO7("changeproperty", XChangeProperty,
		"window property type format mode data nelements",
		(void), iwi, iat, iat, iin, icparg,
		(unsigned char *) ist, iin);
	DO2("changesaveset", XChangeSaveSet,
		"window change_mode", (void), iwi, icssarg);
	DO2("circulatesubwindows", XCirculateSubwindows,
		"window direction", (void), iwi, icsarg);
	DO1("circulatesubwindowsdown", XCirculateSubwindowsDown,
		"window", (void), iwi);
	DO1("circulatesubwindowsup", XCirculateSubwindowsUp,
		"window", (void), iwi);
	DO6("cleararea", XClearArea,
		"window x y width height exposures",
		(void), iwi, iin, iin, iui, iui, ibo);
	DO1("clearwindow", XClearWindow, "window", (void), iwi);
	DO("closedisplay", myCloseDisplay, (void));
	DO("connectionnumber", XConnectionNumber, oin);
	DO5("convertselection", XConvertSelection,
		"selection target property requestor time",
		(void), iat, iat, iat, iwi, iti);
	DO9("copyarea", XCopyArea,
		"src dest gc src_x src_y width height dest_x dest_y",
		(void), idr, idr, igc, iin, iin, iui, iui, iin, iin);
	DO1("copycolormapandfree", XCopyColormapAndFree,
		"colormap", okeepco, ico);
	DO3("copygc", XCopyGC, "src valuemask dest", (void), igc, igcc, igc);
	DO10("copyplane", XCopyPlane,
		"src dest gc src_x src_y width height dest_x dest_y plane",
		(void), idr, idr, igc, iin, iin, iui, iui, iin, iin, iul);
	DO4("createbitmapfromdata", XCreateBitmapFromData,
		"drawable data width height", okeeppi, idr, ist, iui, iui);
	DO1("createfontcursor", XCreateFontCursor, "shape", okeepcu, ici);
	DO4("createpixmap", XCreatePixmap,
		"drawable width height depth", okeeppi, idr, iui, iui, iui);
	DO7("createpixmapfrombitmapdata", XCreatePixmapFromBitmapData,
		"drawable data width height fg bg depth",
		okeeppi, idr, ist, iui, iui, ipx, ipx, iui);
	DO8("createsimplewindow", XCreateSimpleWindow,
		"parent x y width height border_width border background",
		okeepwi, iwi, iin, iin, iui, iui, iui, ipx, ipx);
case 'd':
	DO1("defaultcolormap", XDefaultColormap, "screen_number", oco, isn);
	DO1("defaultdepth", XDefaultDepth, "screen_number", oin, isn);
	DO1("defaultgc", XDefaultGC, "screen_number", ogc, isn);
	DO("defaultrootwindow", XDefaultRootWindow, owi);
	DO("defaultscreen", XDefaultScreen, oin);
	DO2("definecursor", XDefineCursor, "window cursor", (void), iwi, icu);
	DO2("deleteproperty", XDeleteProperty,
		"window property", (void), iwi, iat);
	DO1("destroysubwindows", XDestroySubwindows, "window", (void), iwi);
	DO1("destroywindow", XDestroyWindow, "window", (void), iwi);
	DO("disableaccesscontrol", XDisableAccessControl, (void));
	DO1("displaycells", XDisplayCells, "screen_number", oin, isn);
	DO1("displayheight", XDisplayHeight, "screen_number", oin, isn);
	DO1("displayheightmm", XDisplayHeightMM, "screen_number", oin, isn);
	DO("displaymotionbuffersize", XDisplayMotionBufferSize, oul);
	DO1("displayplanes", XDisplayPlanes, "screen_number", oin, isn);
	DO("displaystring", XDisplayString, ost);
	DO1("displaywidth", XDisplayWidth, "screen_number", oin, isn);
	DO1("displaywidthmm", XDisplayWidthMM, "screen_number", oin, isn);
	DO8("drawarc", XDrawArc,
		"drawable gc x y width height angle1 angle2",
		(void), idr, igc, iin, iin, iui, iui, iin, iin);
	DO_2("drawarcs", myDrawArcs,
		"drawable gc [x y width height angle1 angle2 ...]",
		(void), idr, igc);
	DO6("drawimagestring", XDrawImageString,
		"drawable gc x y string length",
		(void), idr, igc, iin, iin, ist, iin);
	DO6("drawline", XDrawLine,
		"drawable gc x1 x2 y1 y2",
		(void), idr, igc, iin, iin, iin, iin);
	DO_2("drawlines", myDrawLines,
		"drawable gc [ x y ... ] mode", (void), idr, igc);
	DO4("drawpoint", XDrawPoint,
		"drawable gc x y", (void), idr, igc, iin, iin);
	DO_2("drawpoints", myDrawPoints,
		"drawable gc [ x y ... ] mode", (void), idr, igc);
	DO6("drawrectangle", XDrawRectangle,
		"drawable gc x y width height",
		(void), idr, igc, iin, iin, iui, iui);
	DO_2("drawrectangles", myDrawRectangles,
		"drawable gc [x y width height ...]", (void), idr, igc);
	DO_2("drawsegments", myDrawSegments,
		"drawable gc [x1 y1 x2 y2 ...]", (void), idr, igc);
	DO6("drawstring", XDrawString,
		"drawable gc x y string length",
		(void), idr, igc, iin, iin, ist, iin);
case 'e':
	DO("enableaccesscontrol", XEnableAccessControl, (void));
	DO1("eventsqueued", XEventsQueued, "mode", oin, ieqarg);
#if XlibSpecificationRelease - 6 >= 0
	DO("extendedmaxrequestsize", XExtendedMaxRequestSize, olo);
#endif /* XlibSpecificationRelease - 6 >= 0 */
case 'f':
	DO8("fillarc", XFillArc,
		"drawable gc x y width height angle1 angle2",
		(void), idr, igc, iin, iin, iui, iui, iin, iin);
	DO_2("fillarcs", myFillArcs,
		"drawable gc [x y width height angle1 angle2 ...]",
		(void), idr, igc);
	DO_2("fillpolygon", myFillPolygon,
		"drawable gc [ x y ... ] shape mode", (void), idr, igc);
	DO6("fillrectangle", XFillRectangle,
		"drawable gc x y width height",
		(void), idr, igc, iin, iin, iui, iui);
	DO_2("fillrectangles", myFillRectangles,
		"drawable gc [ x y width height ...]", (void), idr, igc);
	DO("flush", XFlush, (void));
	/*
	** The next is conditionalized for the benefit of XV11R4 users
	** (thanks to Jay Schmidgall <shmdgljd+@rchland.ibm.com>).
	*/
#if XlibSpecificationRelease - 5 >= 0
	DO1("flushgc", XFlushGC, "gc", (void), igc);
#endif /* XlibSpecificationRelease - 5 >= 0 */
	DO1("forcescreensaver", XForceScreenSaver, "mode", (void), ifssarg);
	DO1("freecolormap", XFreeColormap, "colormap", (void), ico);
	DO1("freecursor", XFreeCursor, "cursor", (void), icu);
	DO1("freegc", XFreeGC, "gc", (void), igc);
	DO1("freepixmap", XFreePixmap, "pixmap", (void), ipi);
case 'g':
	/*
	** No need to allow symbolic names as input here--
	** if you knew the symbolic name, you would not need
	** to call the function!
	*/
	DO1("getatomname", XGetAtomName, "atom", ost, (Atom) iui);
	DO2("getdefault", XGetDefault, "program option", ost, ist, ist);
	DO1("getselectionowner", XGetSelectionOwner, "selection", owi, iat);
	{
		static char	gbdesc[] = "\
button modifiers grab_window owner_events event_mask \
pointer_mode keyboard_mode confine_to cursor";

	DO9("grabbutton", XGrabButton,
		gbdesc, (void), ibu, ikbm, iwi, ibo, iuem, igm, igm, iwi, icu);
	}
	DO6("grabkey", XGrabKey,
"keycode modifiers grab_window owner_events pointer_mode keyboard_mode",
		(void), iin, ikbm, iwi, ibo, igm, igm);
	DO5("grabkeyboard", XGrabKeyboard,
		"grab_window owner_events pointer_mode keyboard_mode time",
		oin, iwi, ibo, igm, igm, iti);
	{
		static char	gpdesc[] = "\
grab_window owner_events event_mask pointer_mode \
keyboard_mode confine_to cursor time";

	DO8("grabpointer", XGrabPointer,
		gpdesc, oin, iwi, ibo, iuem, igm, igm, iwi, icu, iti);
	}
	DO("grabserver", XGrabServer, (void));
case 'i':
	DO2("iconifywindow", XIconifyWindow,
		"window screen_number", ostat, iwi, isn);
	DO("imagebyteorder", XImageByteOrder, oin);
	DO1("installcolormap", XInstallColormap, "colormap", (void), ico);
	DO2("internatom", (int) XInternAtom,
		"atom_name only_if_exists", oin, ist, ibo);
case 'k':
	/*
	** Don't ask.
	*/
#ifdef NeedWidePrototypes
	DO2("keycodetokeysym", XKeycodeToKeysym,
		"keycode index", oks, (unsigned int) ikc, iin);
#endif /* defined NeedWidePrototypes */
#ifndef NeedWidePrototypes
	DO2("keycodetokeysym", XKeycodeToKeysym,
		"keycode index", oks, ikc, iin);
#endif /* !defined NeedWidePrototypes */
	DO1("keysymtokeycode", XKeysymToKeycode, "keysym", okc, iks);
	DO1("killclient", XKillClient, "resource", (void), ixi);
case 'l':
	DO("lastknownrequestprocessed", XLastKnownRequestProcessed, oul);
	DO1("loadfont", XLoadFont, "name", okeepfo, ist);
	DO("lockdisplay", XLockDisplay, (void));
	DO1("lowerwindow", XLowerWindow, "window", (void), iwi);
case 'm':
	DO1("mapraised", XMapRaised, "window", (void), iwi);
	DO1("mapsubwindows", XMapSubwindows, "window", (void), iwi);
	DO1("mapwindow", XMapWindow, "window", (void), iwi);
	DO1("maskevent", myMaskEvent, "event_mask", (void), isem);
	DO("maxrequestsize", XMaxRequestSize, olo);
	DO5("moveresizewindow", XMoveResizeWindow,
		"window x y width height", (void), iwi, iin, iin, iui, iui);
	DO3("movewindow", XMoveWindow, "window x y", (void), iwi, iin, iin);
case 'n':
	DO("nextevent", myNextEvent, (void));
	DO("nextrequest", XNextRequest, oul);
	DO("noop", XNoOp, (void));
case 'p':
	DO("peekevent", myPeekEvent, (void));
	DO("pending", XPending, oin);
#if XlibSpecificationRelease - 6 >= 0
	DO1("processinternalconnection", XProcessInternalConnection, "fd",
		(void), iin);
#endif /* XlibSpecificationRelease - 6 >= 0 */
	DO("protocolrevision", XProtocolRevision, oin);
	DO("protocolversion", XProtocolVersion, oin);
case 'q':
	DO("qlength", XQLength, oin);
case 'r':
	DO1("raisewindow", XRaiseWindow, "window", (void), iwi);
	DO1("removefromsaveset", XRemoveFromSaveSet, "window", (void), iwi);
	DO4("reparentwindow", XReparentWindow,
		"window parent x y", (void), iwi, iwi, iin, iin);
	DO("resetscreensaver", XResetScreenSaver, (void));
	DO3("resizewindow", XResizeWindow,
		"window width height", (void), iwi, iui, iui);
	DO("resourcemanagerstring", XResourceManagerString, ost);
	DO1("rootwindow", XRootWindow, "screen_number", owi, isn);
	DO1("rotatebuffers", XRotateBuffers, "rotate", (void), iin);
case 's':
	DO("screencount", XScreenCount, oin);
	DO2("selectinput", XSelectInput,
		"window event_mask", (void), iwi, isem);
	DO("servervendor", XServerVendor, ost);
	DO1("setaccesscontrol", XSetAccessControl, "mode", (void), isacarg);
	DO2("setarcmode", XSetArcMode, "gc arc_mode", (void), igc, isamarg);
	DO2("setbackground", XSetBackground,
		"gc background", (void), igc, ipx);
	DO2("setclipmask", XSetClipMask, "gc pixmap", (void), igc, ipi);
	DO3("setcliporigin", XSetClipOrigin,
		"gc clip_x_origin clip_y_origin", (void), igc, iin, iin);
	DO1("setclosedownmode", XSetCloseDownMode,
		"close_mode", (void), iscdmarg);
	DO4("setdashes", XSetDashes,
		"gc dash_offset dash_list n", (void), igc, iin, ist, iin);
	DO2("setfillrule", XSetFillRule, "gc fill_rule", (void), igc, isfrarg);
	DO2("setfillstyle", XSetFillStyle,
		"gc fill_style", (void), igc, isfsarg);
	DO2("setfont", XSetFont, "gc font", (void), igc, ifo);
	DO2("setforeground", XSetForeground,
		"gc foreground", (void), igc, ipx);
	DO2("setfunction", XSetFunction, "gc function", (void), igc, ifu);
	DO2("setgraphicsexposures", XSetGraphicsExposures,
		"gc graphics_exposures", (void), igc, ibo);
	DO2("seticonname", XSetIconName, "window icon_name", (void), iwi, ist);
	DO3("setinputfocus", XSetInputFocus, "focus revert_to time",
		(void), iwi, isifarg, iti);
	DO5("setlineattributes", XSetLineAttributes,
		"gc line_width line_style cap_style join_style",
		(void), igc, iui, ils, ics, ijs);
	DO2("setplanemask", XSetPlaneMask, "gc plane_mask", (void), igc, iul);
	DO4("setscreensaver", XSetScreenSaver,
		"timeout interval prefer_blanking allow_exposures",
		(void), iin, iin, ipb, iae);
	DO3("setselectionowner", XSetSelectionOwner,
		"selection owner time", (void), iat, iwi, iti);
	DO5("setstate", XSetState,
		"gc foreground background function plane_mask",
		(void), igc, ipx, ipx, ifu, iul);
	DO2("setstipple", XSetStipple, "gc stipple", (void), igc, ipi);
	DO2("setsubwindowmode", XSetSubwindowMode,
		"gc subwindow_mode", (void), igc, issmarg);
	DO2("settile", XSetTile, "gc tile", (void), igc, ipi);
	DO2("settransientforhint", XSetTransientForHint,
		"window prop_window", (void), iwi, iwi);
	DO3("settsorigin", XSetTSOrigin,
		"gc ts_x_origin ts_y_origin", (void), igc, iin, iin);
	DO2("setwindowbackground", XSetWindowBackground,
		"window background_pixel", (void), iwi, ipx);
	DO2("setwindowbackgroundpixmap", XSetWindowBackgroundPixmap,
		"window background_pixmap", (void), iwi, ipi);
	DO2("setwindowborder", XSetWindowBorder,
		"window border_pixel", (void), iwi, ipx);
	DO2("setwindowborderpixmap", XSetWindowBorderPixmap,
		"window border_pixmap", (void), iwi, ipi);
	DO2("setwindowborderwidth", XSetWindowBorderWidth,
		"window width", (void), iwi, iui);
	DO2("setwindowcolormap", XSetWindowColormap,
		"window colormap", (void), iwi, ico);
	DO3("storebuffer", XStoreBuffer,
		"bytes nbytes buffer", (void), ist, iin, iin);
	DO2("storename", XStoreName, "window window_name", (void), iwi, ist);
	DO4("storenamedcolor", XStoreNamedColor,
		"colormap color pixel flags", (void), ico, ist, ipx, isncarg);
	DO1("sync", XSync, "discard", (void), ibo);
case 'u':
	DO1("undefinecursor", XUndefineCursor, "window", (void), iwi);
	DO3("ungrabbutton", XUngrabButton,
		"button modifiers grab_window", (void), ibu, ikbm, iwi);
	DO3("ungrabkey", XUngrabKey,
		"keycode modifiers grab_window", (void), iin, ikbm, iwi);
	DO1("ungrabkeyboard", XUngrabKeyboard, "time", (void), iti);
	DO1("ungrabpointer", XUngrabPointer, "time", (void), iti);
	DO("ungrabserver", XUngrabServer, (void));
	DO1("uninstallcolormap", XUninstallColormap, "colormap", (void), ico);
	DO1("unloadfont", XUnloadFont, "font", (void), ifo);
	DO("unlockdisplay", XUnlockDisplay, (void));
	DO1("unmapsubwindows", XUnmapSubwindows, "window", (void), iwi);
	DO1("unmapwindow", XUnmapWindow, "window", (void), iwi);
case 'v':
	DO("vendorrelease", XVendorRelease, oin);
case 'w':
	DO8("warppointer", XWarpPointer,
		"src_w dest_w src_x src_y src_width src_height dest_x dest_y",
		(void), iwi, iwi, iin, iin, iui, iui, iin, iin);
	DO1("whitepixel", XWhitePixel, "screen_number", oul, isn);
	DO2("windowevent", myWindowEvent,
		"window event_mask", (void), iwi, isem);
	DO2("withdrawwindow", XWithdrawWindow,
		"window screen_number", ostat, iwi, isn);
	DO6("writebitmapfile", XWriteBitmapFile,
		"filename bitmap width height x_hot y_hot",
		(void), ist, ipi, iui, iui, iin, iin);
	}
	return False;
}

static void
usage(listfuncs)
register const Bool	listfuncs;
{
	(void) fprintf(stderr, "%s: usage is %s ", progname, progname);
	(void) fprintf(stderr, "[-int] ");
	(void) fprintf(stderr, "[-frame] ");
	(void) fprintf(stderr, "[-verbose] ");
	(void) fprintf(stderr, "[-display displayname] ");
	(void) fprintf(stderr, "[function [arg...]] ");
	if (strchr(elsieid, '\t') != NULL)
		(void) fprintf(stderr, "# %s", strchr(elsieid, '\t') + 1);
	(void) fprintf(stderr, "\n");
	if (listfuncs) {
		(void) fprintf(stderr, " functions:\n");
		(void) try(dump);
	}
	exit(1);
}

static char **
getfields(cp)
register char * cp;
{
	register char *		dp;
	register int		nsubs;
	register int		len;
	static char **		array;
	static char *		copyp;

	cp = nonnull(cp);
	len = strlen(cp);
	copyp = erealloc(copyp, len + 1);
	array = (char **) erealloc((char *) array, (len + 1) * sizeof *array);
	(void) strcpy(copyp, cp);
	nsubs = 0;
	for ( ; ; ) {
		while (isascii(*cp) && isspace(*cp))
			++cp;
		if (*cp == '\0' || *cp == '#')
			break;
		array[nsubs++] = dp = cp;
		do {
			if ((*dp = *cp++) != '"')
				++dp;
			else while ((*dp = *cp++) != '"')
				if (*dp != '\0')
					++dp;
				else	wild2exit("misquoted line", copyp);
		} while (*cp != '\0' && *cp != '#' &&
			(!isascii(*cp) || !isspace(*cp)));
		if (isascii(*cp) && isspace(*cp))
			++cp;
		*dp = '\0';
	}
	array[nsubs] = NULL;
	return array;
}

static char *
getline()
{
	register int	used;
	register int	c;
	static char *	buf;
	static int	avail;

	used = 0;
	for ( ; ; ) {
		c = getchar();
		if (c == EOF || c == '\0' || !isascii(c))
			return NULL;
		if (used >= avail)
			buf = erealloc(buf, ++avail);
		if (c == '\n') {
			if (used > 0 && buf[used - 1] == '\r')
				--used;
			buf[used] = '\0';
			break;
		}
		buf[used++] = c;
	}
	return buf;
}

static void
handle(ac, av)
register const int			ac;
register const char * const * const	av;
{
	argfuncname = av[0];
	v = &av[1];
	n = ac - 1;
	if (try(mull))
		spnl(True);
	else	wild2exit("function name", av[0]);
}

static int
doerror(disp, eep)
Display *	disp;
XErrorEvent *	eep;
{
	char	buffer[512];

	(void) XGetErrorText(disp, eep->error_code, buffer, sizeof buffer);
	(void) fprintf(stderr, "\n%s: %s\n", progname, buffer);
	return 0;
}

int
main(argc, argv)
int	argc;
char *	argv[];
{
	register int	argn;
	register char * dispname;

	progname = argv[0];
	dispname = NULL;
	argn = 1;
	while (argn < argc)
		if (csequal(argv[argn], "--")) {
			++argn;
			break;
		} else if (csequal(argv[argn], "-display")) {
			++argn;
			if (argn >= argc)
				wildexit(
					"use of -display without displayname");
			else	dispname = argv[argn++];
		} else if (csequal(argv[argn], "-int")) {
			++argn;
			intflag = True;
		} else if (csequal(argv[argn], "-frame")) {
			++argn;
			frameflag = True;
		} else if (csequal(argv[argn], "-verbose")) {
			++argn;
			verboseflag = True;
		} else if (csequal(argv[argn], "="))
			usage(True);
		else if (argv[argn][0] == '-')
			usage(False);
		else	break;
	display = XOpenDisplay(dispname);
	if (display == 0) {
		(void) fprintf(stderr, "%s: XOpenDisplay errno %d\n",
			progname, errno);
		wildrexit("opening display");
	}
	default_screen_number = XDefaultScreen(display);
	default_root_window = XDefaultRootWindow(display);
	default_gc = XDefaultGC(display, default_screen_number);
	default_colormap = XDefaultColormap(display, default_screen_number);
	black_pixel = XBlackPixel(display, default_screen_number);
	white_pixel = XWhitePixel(display, default_screen_number);
	single = (argc != argn);
	if (single)
		handle(argc - argn, &argv[argn]);
	else {
		interactive = isatty(fileno(stdin));
		if (interactive) {
			(void) XSetErrorHandler(doerror);
			(void) setjmp(env);
		}
		for ( ; ; ) {
			register char *		buf;
			register char **	fields;
			register int		nfields;

			buf = getline();
			if (buf == NULL)
				break;
			fields = getfields(buf);
			for (nfields = 0; fields[nfields] != NULL; ++nfields)
				continue;
			if (nfields == 0)
				continue;
			handle(nfields, fields);
			if (ferror(stdout) || fflush(stdout))
				break;
			if (interactive && display != NULL)
				XSync(display, False);
		}
	}
	interactive = False;
	if (display != NULL)
		XSync(display, False);
	if (ferror(stdin))
		wildrexit("reading");
	if (ferror(stdout) || fflush(stdout) ||
		ferror(stderr) || fflush(stderr))
			wildrexit("writing");
	return 0;
}
