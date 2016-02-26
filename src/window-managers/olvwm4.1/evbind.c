/*
 *      (c) Copyright 1991 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)evbind.c	1.6 olvwm version 09 Feb 1994"
#endif

/*
 * Based on
#ident	"@(#)evbind.c	1.35	93/06/28 SMI"
 *
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef __STDC__
#include <X11/Intrinsic.h>
#endif
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "events.h"
#include "list.h"
#include "mem.h"
#include "kbdfuncs.h"
#include "resources.h"
#include "error.h"


/* ===== externs ========================================================== */

extern List *ScreenInfoList;

/* ===== private data ===================================================== */

static XrmQuark kbdCmdInstanceQ;
static XrmQuark kbdCmdClassQ;
static XrmQuark modInstanceQ;
static XrmQuark modClassQ;
static XrmQuark menuAccelInstanceQ;
static XrmQuark menuAccelClassQ;

/* ===== Modifier Binding ================================================= */


unsigned int ModMaskMap[MOD_MASK_COUNT];

typedef struct {
    char *rsrc_name;
    char *dflt_binding;
    int mapindex;
} ModDescriptor;

ModDescriptor ModDescriptorTable[] = {

     /*	    rsrc_name		default		    mapindex */
    {	    "Constrain",	"Control",	    MOD_CONSTRAIN	},
    {	    "WMGrab",		"Alt",		    MOD_WMGRAB		},
    {	    "Reduce",		"Meta",		    MOD_REDUCE		},
    {	    "Invert",		"Shift",	    MOD_INVERT		},
    {	    "SetDefault",	"Control",	    MOD_SETDEFAULT	},
    {	    "Ignore", "Lock,Num_Lock,mod5,Mode_switch", MOD_IGNORE	}

};
#define NMODBINDINGS (sizeof(ModDescriptorTable)/sizeof(ModDescriptor))


/*
 * establishModBindings
 *
 * Read through the modifier descriptor table and fill in the modifier mask 
 * map with modifier masks found in the server's modifier mask map.
 */
static void
establishModBindings(dpy, newDB)
    Display *dpy;
    XrmDatabase newDB;
{
    XrmQuark classlist[4], instlist[4];
    char *s;
    XrmRepresentation rep;
    XrmValue value;
    ModDescriptor *d;
    unsigned int polyStringToModifier();

    classlist[0] = OpenWinQ;
    classlist[1] = modClassQ;
    classlist[3] = NULLQUARK;

    instlist[0] = TopInstanceQ;
    instlist[1] = modInstanceQ;
    instlist[3] = NULLQUARK;

    for (d = ModDescriptorTable; d < ModDescriptorTable + NMODBINDINGS; ++d) {

	classlist[2] = instlist[2] = XrmStringToQuark(d->rsrc_name);

	if (XrmQGetResource(newDB, instlist, classlist, &rep, &value))
	    s = (char *) value.addr;
	else {
	    /*
	     * Use olwm binding -- see resources.c
	     */
	    classlist[0] = OlwmQ;
	    if (XrmQGetResource(newDB, instlist, classlist, &rep, &value))
		s = (char *) value.addr;
	    else s = d->dflt_binding;
    	    classlist[0] = OpenWinQ;
	}

	ModMaskMap[d->mapindex] = polyStringToModifier(dpy, s);
    }
}


/* ===== Mouse Binding ==================================================== */


/*
 * Table of mouse bindings.
 *
 * REMIND
 * 
 * At a future time, this table will be changeable via resources.  For now,
 * it's specified as a compile-time constant.
 */

typedef struct {
    int state;
    int button;
    SemanticAction action;
} MouseBinding;

#define BAD_STATE	0xffffffff

MouseBinding *MouseBindingTable;

MouseBinding ThreeButtons[] = {
  /*  state,		button,	 action		 */
    { 0,		Button1, ACTION_SELECT },
    { 0,		Button2, ACTION_ADJUST },
    { 0,		Button3, ACTION_MENU   },
    { ButtonToMask(1),	Button2, ACTION_MENU },
    { ButtonToMask(2),	Button1, ACTION_MENU },
    { ShiftMask,	Button1, ACTION_ADJUST },
    { ControlMask,	Button3, ACTION_MENU }, 
    { ControlMask|ButtonToMask(1),	Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(2),	Button1, ACTION_MENU },
    { BAD_STATE,        0,              ACTION_NONE             },
};

MouseBinding OneButtonOnly[] = {
  /*  state,            button,  action          */
    { 0,                Button1, ACTION_SELECT },
    { ShiftMask,        Button1, ACTION_ADJUST },
    { ControlMask,      Button1, ACTION_MENU },
    { ControlMask|ButtonToMask(1),      Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(2),      Button1, ACTION_MENU },
    { BAD_STATE,        0,              ACTION_NONE             },
};

MouseBinding TwoButtonsOlSpec[] = {     /* according to the OL spec */
  /*  state,            button,  action          */
    { 0,                Button1, ACTION_SELECT },
    { 0,                Button2, ACTION_MENU   },
    { ShiftMask,        Button1, ACTION_ADJUST },
    { ControlMask,      Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(1),      Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(2),      Button1, ACTION_MENU },
    { BAD_STATE,        0,              ACTION_NONE             },
};

MouseBinding TwoButtonsWithChording[] = {       /* The Xerox-inspired way */
  /*  state,            button,  action          */
    { 0,                Button1, ACTION_SELECT },
    { 0,                Button2, ACTION_ADJUST },
    { ButtonToMask(1),  Button2, ACTION_MENU },
    { ButtonToMask(2),  Button1, ACTION_MENU },
    { ShiftMask,        Button1, ACTION_ADJUST },
    { ControlMask,      Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(1),      Button2, ACTION_MENU },
    { ControlMask|ButtonToMask(2),      Button1, ACTION_MENU },
    { BAD_STATE,        0,              ACTION_NONE             },
};


/*
 * searchMouseBindings
 * 
 * Search the mouse binding table and return information about what was found.
 * Return values of MATCH_NONE, MATCH_INCOMPLETE, and MATCH_AMBIG mean that no
 * action is available for the given event.  MATCH_PREFIX and MATCH_EXACT
 * indicate that an action is available; this action is returned in the area
 * pointed to by the action parameter.
 */
static MouseMatchState
searchMouseBindings(pe, action)
    XButtonEvent *pe;
    SemanticAction *action;
{
    int i;
    int nexact = 0;	/* number of exact matches */
    int nprefix = 0;	/* number of prefix matches */
    int lastexact = 0;	/* index of last exact match */
    unsigned int modmask = (pe->state | ButtonToMask(pe->button));
    static int first = 1;

    if (first) {
	extern int numbuttons;
	switch (numbuttons) {           /* based on # buttons on pointer */
	    default:
	    case 3:
		MouseBindingTable = ThreeButtons;
		break;
	    case 2:
		if (GRV.MouseChordMenu) /* is chording enabled? */
		    MouseBindingTable = TwoButtonsWithChording;
		else
		    MouseBindingTable = TwoButtonsOlSpec;
		break;
	    case 1:
		MouseBindingTable = OneButtonOnly;
		break;
	}
	first = 0;
    }

    for (i=0; MouseBindingTable[i].state != BAD_STATE; ++i) {
	if (pe->state == MouseBindingTable[i].state &&
	  pe->button == MouseBindingTable[i].button) {
	    lastexact = i;
	    ++nexact;
	} else if (modmask == MouseBindingTable[i].state) {
	    ++nprefix;
	}
    }

    if (nexact == 0 && nprefix == 0)
	return MATCH_NONE;
    if (nexact == 0 && nprefix > 0)
	return MATCH_INCOMPLETE;
    if (nexact > 1)
	return MATCH_AMBIG;

    /* at this point, we know there is exactly one exact match */
    *action = MouseBindingTable[lastexact].action;
    if (nprefix == 0)
	return MATCH_EXACT;
    else
	return MATCH_PREFIX;
}


/*
 * checkChording
 *
 * Scan the input queue for button events that will disambiguate a single
 * action from a chorded action.  If there aren't any events in the queue,
 * wait for them until a certain timeout period has elapsed.  Return value
 * indicates whether a ButtonPress was seen further ahead in the event stream,
 * which indicates that this event is part of a chorded sequence.  The timeout 
 * parameter is updated with the amount of time remaining.
 */
static Bool
checkChording(dpy, timeout, pr)
    Display *dpy;
    struct timeval timeout;
    XButtonEvent *pr;
{
    XEvent e;
    int n;
    
    while (1) {
	/*
	 * Check for data on the connection.  Scan it for disambiguating 
	 * events.  Note that MotionNotify events within the move threshold 
	 * are discarded.
	 */
	n = XEventsQueued(dpy, QueuedAfterReading);
	if (n > 0 && XCheckMaskEvent(dpy,
	ButtonPressMask|ButtonReleaseMask|ButtonMotionMask, &e)) {
	    switch (e.type) {
	    case ButtonPress:
		XPutBackEvent(dpy, &e);
		return True;
	    case ButtonRelease:
		XPutBackEvent(dpy, &e);
		return False;
	    case MotionNotify:
		if (ABS(pr->x_root - e.xmotion.x_root) > GRV.MoveThreshold ||
		    ABS(pr->y_root - e.xmotion.y_root) > GRV.MoveThreshold) {
		    XPutBackEvent(dpy, &e);
		    return False;
		}
		break;
	    }
	}

	if (!AwaitEvents(dpy, &timeout))
	    return False;
    }
    /* NOTREACHED */
}


/*
 * ResolveMouseBinding
 *
 * Given a mouse button press event, determines whether this event completes
 * an event sequence that binds to an action.  If the button press is a prefix
 * of a chording sequence, and this press falls within the chording time of
 * the initial button press, checkChording is called to disambiguate the event
 * stream.  Returns a proper action if the action is complete, otherwise
 * returns ACTION_NONE.  All callers should ensure that no action is taken
 * when this routine returns ACTION_NONE.
 */
SemanticAction
ResolveMouseBinding(dpy, pevent, ignoremask)
    Display *dpy;
    XEvent *pevent;
    unsigned long ignoremask;
{
    MouseMatchState m;
    struct timeval timeout;
    SemanticAction a;
    static Time firstpresstime;
    XEvent e;

    /* copy *pevent to e, masking off ignored bits from the state */
    e = *pevent;
    e.xbutton.state &= ~(ignoremask |
			 ModMaskMap[MOD_IGNORE] |
			 ModMaskMap[MOD_WMGRAB]);

    /* Chording is in msec.  Convert to sec/usec for timeval. */
    timeout.tv_usec = GRV.MouseChordTimeout * 1000;
    if (timeout.tv_usec >= 1000000) {
	timeout.tv_sec = timeout.tv_usec / 1000000;
	timeout.tv_usec %= 1000000;
    } else {
	timeout.tv_sec = 0;
    }

    if (FirstButtonDown(&e)) {
	firstpresstime = e.xbutton.time;
    } else {
	if (e.xbutton.time - firstpresstime > GRV.MouseChordTimeout)
	    return ACTION_NONE;
    }

    m = searchMouseBindings((XButtonEvent *) &e, &a);
    if ((m == MATCH_PREFIX && !checkChording(dpy, timeout, (XButtonEvent *) &e))
	|| m == MATCH_EXACT) {
	return a;
    } else {
	return ACTION_NONE;
    }
}


/* ===== Keyboard Binding ================================================= */

/*
 * Table of default keyboard descriptors.  This table contains information 
 * necessary to initialize keyboard bindings and customize them based on 
 * resources.
 */

#define NULLFUNC ((void (*)())0)

extern void HandleHelpKey();

static void keySuspend();
static void keyResume();
static void keyQuoteNext();

static unsigned long mouselessMaskTable[] = { KD_SUNVIEW, KD_BASIC, KD_FULL };

KeyDescriptor KeyDescriptorTable[] = {

/*
	rsrc_name		    dflt_binding		function
	action			    flags
 */

{
	"Stop",			    "L1,Escape",		NULLFUNC,
	ACTION_STOP,		    KD_ALWAYS
}, {
	"DefaultAction",	"Return,Return+Meta,KP_Enter",	NULLFUNC,
	ACTION_EXEC_DEFAULT,	    KD_ALWAYS
}, {
	"Select",		    "space",			NULLFUNC,
	ACTION_SELECT,		    KD_ALWAYS
}, {
	"Adjust",		    "Insert+Alt",		NULLFUNC,
	ACTION_ADJUST,		    KD_ALWAYS
}, {
	"Menu",			    "space+Alt",		NULLFUNC,
	ACTION_MENU,		    KD_ALWAYS
}, {
	"InputFocusHelp",	    "question,question+Ctrl",	NULLFUNC,
	ACTION_FOCUS_HELP,	    KD_ALWAYS
}, {
	"Up",			    "Up",			NULLFUNC,
	ACTION_UP,		    KD_ALWAYS,
}, {
	"Down",			    "Down",			NULLFUNC,
	ACTION_DOWN,		    KD_ALWAYS,
}, {
	"Left",			    "Left",			NULLFUNC,
	ACTION_LEFT,		    KD_ALWAYS
}, {
	"Right",		    "Right",			NULLFUNC,
	ACTION_RIGHT,		    KD_ALWAYS
}, {
	"JumpUp",		    "Up+Ctrl",			NULLFUNC,
	ACTION_JUMP_UP,		    KD_ALWAYS
}, {
	"JumpDown",		    "Down+Ctrl",		NULLFUNC,
	ACTION_JUMP_DOWN,	    KD_ALWAYS
}, {
	"JumpLeft",		    "Left+Ctrl",		NULLFUNC,
	ACTION_JUMP_LEFT,	    KD_ALWAYS
}, {
	"JumpRight",		    "Right+Ctrl",		NULLFUNC,
	ACTION_JUMP_RIGHT,	    KD_ALWAYS
}, {
	"RowStart",		    "Home,R7",			NULLFUNC,
	ACTION_ROW_START,	    KD_ALWAYS
}, {
	"RowEnd",		    "End,R13",			NULLFUNC,
	ACTION_ROW_END,		    KD_ALWAYS
}, {
	"DataStart",		    "Home+Ctrl",		NULLFUNC,
	ACTION_DATA_START,	    KD_ALWAYS
}, {
	"DataEnd",		    "End+Ctrl",			NULLFUNC,
	ACTION_DATA_END,	    KD_ALWAYS
}, {
	"FirstControl",		    "bracketleft+Ctrl",		NULLFUNC,
	ACTION_FIRST_CONTROL,	    KD_ALWAYS
}, {
	"LastControl",		    "bracketright+Ctrl",	NULLFUNC,
	ACTION_LAST_CONTROL,	    KD_ALWAYS
}, {
	"NextElement",		    "Tab,Tab+Ctrl",		NULLFUNC,
	ACTION_NEXT_ELEMENT,	    KD_ALWAYS
}, {
	"PreviousElement",	    "Tab+Shift,Tab+Shift+Ctrl",	NULLFUNC,
	ACTION_PREVIOUS_ELEMENT,    KD_ALWAYS
}, {
	"Open",			    "L7+Alt",			NULLFUNC,
	ACTION_OPEN,		    KD_ALWAYS
}, {
	"Help",			    "Help",			HandleHelpKey,
	ACTION_HELP,		    KD_BASIC_FULL
}, {
	"LockColormap",		    "L2+Ctrl",		    KeyLockColormap,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"UnlockColormap",	    "L4+Ctrl",		    KeyUnlockColormap,
	ACTION_NONE,		    KD_BASIC_FULL
}, {
	"Front",		    "L5+Alt",			KeyFrontFocus,
	ACTION_FRONT,		    KD_BASIC_FULL
}, {
	"FocusToPointer",	    "j+Shift+Alt",	    KeyFocusToPointer,
	ACTION_NONE,		    KD_FULL
}, {
	"NextApp",		    "n+Alt",			KeyNextApp,
	ACTION_NONE,		    KD_FULL
}, {
	"PreviousApp",		    "N+Alt",			KeyPrevApp,
	ACTION_NONE,		    KD_FULL
}, {
	"ToggleInput",		    "t+Alt",			KeyToggleInput,
	ACTION_NONE,		    KD_FULL
}, {
	"NextWindow",		    "w+Alt",			KeyNextWindow,
	ACTION_NONE,		    KD_FULL
}, {
	"PreviousWindow",	    "W+Alt",			KeyPrevWindow,
	ACTION_NONE,		    KD_FULL
}, {
	"TogglePin",		    "Insert+Meta",		KeyTogglePin,
	ACTION_TOGGLE_PIN,	    KD_FULL
}, {
	"SuspendMouseless",	    "z+Alt",			keySuspend,
	ACTION_NONE,		    KD_FULL
}, {
	"ResumeMouseless",	    "Z+Alt",			keyResume,
	ACTION_NONE,		    KD_IMMUNE | KD_FULL
}, {
	"QuoteNextKey",		    "q+Alt",			keyQuoteNext,
	ACTION_NONE,		    KD_FULL
}, {
	"Refresh",		    "",				KeyRefresh,
	ACTION_REFRESH,		    KD_MENU_ACCEL
}, {
	"Back",			    "",				KeyBackFocus,
	ACTION_BACK,		    KD_MENU_ACCEL
}, {
	"OpenClose",		    "w+Meta",		    	KeyOpenCloseFocus,
	ACTION_OPEN_CLOSE,	    KD_MENU_ACCEL
}, {
	"FullRestore",		    "",				KeyFullRestore,
	ACTION_FULL_RESTORE,	    KD_MENU_ACCEL
}, {
	"Quit",			    "q+Meta",			KeyQuit,
	ACTION_QUIT,		    KD_MENU_ACCEL
}, {
	"Owner",		    "",				KeyOwner,
	ACTION_OWNER,		    KD_MENU_ACCEL
}, {
	"WorkspaceMenu",	    "M+Alt",		    KeyWorkspaceMenu,
	ACTION_NONE,		    KD_FULL
}, {
	"WindowMenu",		    "m+Alt",			KeyWindowMenu,
	ACTION_NONE,		    KD_FULL
}, {
	"Move",			    "",				KeyMove,
	ACTION_MOVE,		    KD_MENU_ACCEL
}, {
	"Resize",		    "",				KeyResize,
	ACTION_RESIZE,		    KD_MENU_ACCEL
}, {
	"Properties",		    "",				KeyProperties,
	ACTION_PROPS,	    	    KD_MENU_ACCEL
}, {
	"OpenClosePointer",	    "L7",		  KeyOpenClosePointer,
	ACTION_NONE,		    KD_ALWAYS
}, {
	"RaiseLower",		    "L5",		 KeyRaiseLowerPointer,
	ACTION_NONE,		    KD_ALWAYS
}, {
	"MakeInvisiblePointer",     "",                KeyMakeInvisiblePointer,
	ACTION_NONE,                KD_FULL
}, {
	"MakeInvisibleFocus",       "",                 KeyMakeInvisibleFocus,
	ACTION_NONE,                KD_FULL
}, {
	"MakeVisibleAll",           "",                 KeyMakeVisibleAll,
	ACTION_NONE,                KD_FULL
}, {
	"ToggleFullSizeZoomX",	    "F3+Alt+Shift",    KeyToggleFullSizeZoomX,
	ACTION_NONE,		    KD_FULL
}, {
	"ToggleDragWindow",	    "F6+Alt+Shift",    KeyToggleDragWindow,
	ACTION_NONE,		    KD_FULL
}, {
	"ToggleMoveGroups",	    "g+Alt",           KeyToggleMoveGroups,
	ACTION_NONE,		    KD_FULL
}, {
	"ToggleSticky",	            "t+Meta",             KeyToggleSticky,
	ACTION_STICK,		    KD_FULL
}, {
	"SaveWorkspace",	    "F10+Meta+Alt+Shift", KeySaveWorkspace,
	ACTION_NONE,		    KD_FULL
},

/*
 * Keymappings for the virtual desktop.  The keypad keys are mapped
 * in a clockwise fashion around the arrow keys.  These keys with a meta
 * modifier are grabbed and are always active, otherwise they fall through
 * to the no focus window which moves the vdm
 *
 */
   {    "VirtualUp",		    "Up+Meta",		KeyMoveVDM,
	ACTION_UP_V,		    KD_VIRTUAL				},
   {    "HalfUp",		    "Up+Shift",		NULLFUNC,
	ACTION_HALF_UP,	    	    KD_VIRTUAL				},
   {    "VirtualHalfUp",	    "Up+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_UP,		    KD_VIRTUAL				},
   {    "VirtualJumpUp",	    "Up+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_UP,		    KD_VIRTUAL				},

   {    "VirtualDown",		    "Down+Meta",	KeyMoveVDM,
	ACTION_DOWN_V,		    KD_VIRTUAL				},
   {    "HalfDown",		    "Down+Shift",	NULLFUNC,
	ACTION_HALF_DOWN,    	    KD_VIRTUAL				},
   {    "VirtualHalfDown",	    "Down+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_DOWN,	    KD_VIRTUAL				},
   {    "VirtualJumpDown",	    "Down+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_DOWN,	    KD_VIRTUAL				},

   {    "VirtualLeft",		    "Left+Meta",	KeyMoveVDM,
	ACTION_LEFT_V,		    KD_VIRTUAL				},
   {    "HalfLeft",		    "Left+Shift",	NULLFUNC,
	ACTION_HALF_LEFT,	    KD_VIRTUAL				},
   {    "VirtualHalfLeft",	    "Left+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_LEFT,	    KD_VIRTUAL				},
   {    "VirtualJumpLeft",	    "Left+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_LEFT,	    KD_VIRTUAL				},

   {    "VirtualRight",		    "Right+Meta",	KeyMoveVDM,
	ACTION_RIGHT_V,		    KD_VIRTUAL				},
   {    "HalfRight",		    "Right+Shift",	NULLFUNC,
	ACTION_HALF_RIGHT,	    KD_VIRTUAL				},
   {    "VirtualHalfRight",	    "Right+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_RIGHT,	    KD_VIRTUAL				},
   {    "VirtualJumpRight",	    "Right+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_RIGHT,	    KD_VIRTUAL				},

   {    "UpLeft",		    "R7",		NULLFUNC,
	ACTION_UPLEFT,	    	    KD_VIRTUAL				},
   {    "VirtualUpLeft",	    "R7+Meta",		KeyMoveVDM,
	ACTION_UPLEFT_V,	    KD_VIRTUAL				},
   {    "JumpUpLeft",		    "R7+Ctrl",		NULLFUNC,
	ACTION_JUMP_UPLEFT,	    KD_VIRTUAL				},
   {	"VirtualJumpUpLeft",	    "R7+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_UPLEFT,	    KD_VIRTUAL				},
   {    "HalfUpLeft",		    "R7+Shift",		NULLFUNC,
	ACTION_HALF_UPLEFT,	    KD_VIRTUAL				},
   {	"VirtualHalfUpLeft",	    "R7+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_UPLEFT,	    KD_VIRTUAL				},

   {    "VirtualUpRight",	    "R9+Meta",		KeyMoveVDM,
	ACTION_UPRIGHT_V,	    KD_VIRTUAL				},
   {    "UpRight",		    "R9",		NULLFUNC,
	ACTION_UPRIGHT,		    KD_VIRTUAL				},
   {    "JumpUpRight",		    "R9+Ctrl",		NULLFUNC,
	ACTION_JUMP_UPRIGHT,	    KD_VIRTUAL				},
   {    "VirtualJumpUpRight",	    "R9+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_UPRIGHT,	    KD_VIRTUAL				},
   {    "HalfUpRight",		    "R9+Shift",		NULLFUNC,
	ACTION_HALF_UPRIGHT,	    KD_VIRTUAL				},
   {    "VirtualHalfUpRight",	    "R9+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_UPRIGHT,	    KD_VIRTUAL				},

   {	"DownLeft",		    "R13",		NULLFUNC,
	ACTION_DOWNLEFT,	    KD_VIRTUAL				},
   {    "VirtualDownLeft",	    "R13+Meta",		KeyMoveVDM,
	ACTION_DOWNLEFT_V,	    KD_VIRTUAL				},
   {    "JumpDownLeft",		    "R13+Ctrl",		NULLFUNC,
	ACTION_JUMP_DOWNLEFT,	    KD_VIRTUAL				},
   {    "VirtualJumpDownLeft",	    "R13+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_DOWNLEFT,	    KD_VIRTUAL				},
   {    "HalfDownLeft",		    "R13+Shift",	NULLFUNC,
	ACTION_HALF_DOWNLEFT,	    KD_VIRTUAL				},
   {    "VirtualHalfDownLeft",	    "R13+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_DOWNLEFT,	    KD_VIRTUAL				},

   {    "VirtualDownRight",	    "R15+Meta",		KeyMoveVDM,
	ACTION_DOWNRIGHT_V,	    KD_VIRTUAL				},
   {    "DownRight",		    "R15",		NULLFUNC,
	ACTION_DOWNRIGHT,	    KD_VIRTUAL				},
   {    "JumpDownRight",	    "R15+Ctrl",		NULLFUNC,
	ACTION_JUMP_DOWNRIGHT,	    KD_VIRTUAL				},
   {    "VirtualJumpDownRight",	    "R15+Ctrl+Meta",	KeyMoveVDM,
	ACTION_JUMP_DOWNRIGHT,	    KD_VIRTUAL				},
   {    "HalfDownRight",	    "R15+Shift",	NULLFUNC,
	ACTION_HALF_DOWNRIGHT,	    KD_VIRTUAL				},
   {    "VirtualHalfDownRight",	    "R15+Shift+Meta",	KeyMoveVDM,
	ACTION_HALF_DOWNRIGHT,	    KD_VIRTUAL				},

   {    "VirtualHome",	    	    "R11+Meta",		KeyMoveVDM,
	ACTION_HOME_V,	    	    KD_VIRTUAL				},
   {    "GoHome",		    "R11",		NULLFUNC,
	ACTION_HOME,		    KD_VIRTUAL				},

   {    "VirtualScreen1",	    "F1+Meta",		KeyMoveVDM,
	ACTION_GOTO_1,		    KD_VIRTUAL				},
   {    "Screen1",	    	    "F1",		NULLFUNC,
	ACTION_GOTO_1,		    KD_VIRTUAL				},
   {    "VirtualScreen2",	    "F2+Meta",		KeyMoveVDM,
	ACTION_GOTO_2,		    KD_VIRTUAL				},
   {    "Screen2",	    	    "F2",		NULLFUNC,
	ACTION_GOTO_2,		    KD_VIRTUAL				},
   {    "VirtualScreen3",	    "F3+Meta",		KeyMoveVDM,
	ACTION_GOTO_3,		    KD_VIRTUAL				},
   {    "Screen3",	    	    "F3",		NULLFUNC,
	ACTION_GOTO_3,		    KD_VIRTUAL				},
   {    "VirtualScreen4",	    "F4+Meta",		KeyMoveVDM,
	ACTION_GOTO_4,		    KD_VIRTUAL				},
   {    "Screen4",	    	    "F4",		NULLFUNC,
	ACTION_GOTO_4,		    KD_VIRTUAL				},
   {    "VirtualScreen5",	    "F5+Meta",		KeyMoveVDM,
	ACTION_GOTO_5,		    KD_VIRTUAL				},
   {    "Screen5",	    	    "F5",		NULLFUNC,
	ACTION_GOTO_5,		    KD_VIRTUAL				},
   {    "VirtualScreen6",	    "F6+Meta",		KeyMoveVDM,
	ACTION_GOTO_6,		    KD_VIRTUAL				},
   {    "Screen6",	    	    "F6",		NULLFUNC,
	ACTION_GOTO_6,		    KD_VIRTUAL				},
   {    "VirtualScreen7",	    "F7+Meta",		KeyMoveVDM,
	ACTION_GOTO_7,		    KD_VIRTUAL				},
   {    "Screen7",	    	    "F7",		NULLFUNC,
	ACTION_GOTO_7,		    KD_VIRTUAL				},
   {    "VirtualScreen8",	    "F8+Meta",		KeyMoveVDM,
	ACTION_GOTO_8,		    KD_VIRTUAL				},
   {    "Screen8",	    	    "F8",		NULLFUNC,
	ACTION_GOTO_8,		    KD_VIRTUAL				},
   {    "VirtualScreen9",	    "F9+Meta",		KeyMoveVDM,
	ACTION_GOTO_9,		    KD_VIRTUAL				},
   {    "Screen9",	    	    "F9",		NULLFUNC,
	ACTION_GOTO_9,		    KD_VIRTUAL				},
   {    "VirtualScreen10",	    "F10+Meta",		KeyMoveVDM,
	ACTION_GOTO_10,		    KD_VIRTUAL				},
   {    "Screen10",	    	    "F10",		NULLFUNC,
	ACTION_GOTO_10,		    KD_VIRTUAL				},
};

#define NUMKEYDESCRIPTORS (sizeof(KeyDescriptorTable)/sizeof(KeyDescriptor))

typedef struct {
    KeySym sym;
    unsigned int mod;
} modsym;


#define KEYBINDING_TABLE_SIZE 60
#define KEYBINDING_TABLE_INCR 20

static KeyBinding *KeyBindingTable = NULL;
static KeyBinding *activeKey = NULL;
static int bindingTableCount = 0;
static int bindingTableSize = 0;
static Bool quotenext = False;

Bool mouselessSuspended = False;

/*
 * Suspension and resumption of Mouseless functions.
 */
static void
/* ARGSUSED */
keySuspend(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    mouselessSuspended = True;
}


static void
keyResume(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    if (mouselessSuspended)
        mouselessSuspended = False;
    else
	KeyBeep(dpy, ke);
}

static void
/* ARGSUSED */
keyQuoteNext(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    /*
     * Turn on quotenext on the release.  If we set it on the press,
     * the subsequent release would turn it off!
     */
    if (ke->type != KeyRelease)
	return;
    quotenext = True;
}


/*
 * Add a binding to the key binding table.
 */
void
AddKeyBinding(kc, mod, desc)
    KeyCode kc;
    unsigned int mod;
    KeyDescriptor *desc;
{
    KeyBinding *b;

    if (bindingTableCount == bindingTableSize) {
	bindingTableSize += KEYBINDING_TABLE_INCR;
	KeyBindingTable = MemRealloc(KeyBindingTable,
				     bindingTableSize*sizeof(KeyBinding));
    }

    b = &KeyBindingTable[bindingTableCount];
    b->keycode = kc;
    b->modstate = mod;
    b->desc = desc;
    ++bindingTableCount;
}


/*
 * Keysym aliasing.  Provides aliases for modifier keysyms.  Allows an alias 
 * to represent a mask or to be a synonym for up to two keysyms.  The keysyms 
 * are only looked at if the mask value is zero.
 */

typedef struct {
    char *alias;
    unsigned int mask;
    KeySym sym1, sym2;
} KeysymAlias;

static KeysymAlias KeysymAliasTable[] = {
   /* alias	    mask	    sym1	    sym2 */
    { "Any",	    AnyModifier,    0,		    0 },
    { "Shift",	    ShiftMask,	    0,		    0 },
    { "Lock",	    LockMask,	    0,		    0 },
    { "Control",    ControlMask,    0,		    0 },
    { "Ctrl",	    ControlMask,    0,		    0 },
    { "Ctl",	    ControlMask,    0,		    0 },
    { "Meta",	    0,		    XK_Meta_L,	    XK_Meta_R },
    { "Alt",	    0,		    XK_Alt_L,	    XK_Alt_R },
    { "Super",	    0,		    XK_Super_L,	    XK_Super_R },
    { "Hyper",	    0,		    XK_Hyper_L,	    XK_Hyper_R }
};
#define NUMALIASES (sizeof(KeysymAliasTable)/sizeof(KeysymAlias))


/*
 * Takes a word and presumes that it names a keysym.  Looks up this keysym in
 * the modifier mapping table and returns the corresponding modifier mask.  If
 * the string doesn't name a valid keysym, returns 0.  If the keysym is not a
 * modifier, returns 0.  If the word is "Any", returns AnyModifier.  Several
 * aliases are supported for well-known modifiers, e.g. "Meta" for "Meta_L" or
 * "Meta_R".  REMIND: If a keysym is on several keys, and only some of the
 * keys are modifiers, this function may fail to find the modifier mask.
 */
unsigned int
stringToModifier(dpy, word)
    Display *dpy;
    char *word;
{
    KeySym ks;
    KeyCode kc;
    KeysymAlias *ksa;
    int modnum;

    ks = XStringToKeysym(word);

    if (ks != NoSymbol) {
	kc = XKeysymToKeycode(dpy, ks);

	if (kc == 0)
	    return 0;
	else
	    return FindModifierMask(kc);
    }

    /*
     * It's not a valid keysym name, so try a bunch of aliases.  First,
     * Allow "mod1" ... "mod5" as synonyms for Mod1Mask ... Mod5Mask.
     */

    if (1 == sscanf(word, "mod%d", &modnum) &&
	modnum >= 1 && modnum <= 5)
    {
	return (1 << (Mod1MapIndex + modnum - 1));
    }

    /* look through the alias table for masks or keysyms */

    kc = 0;
    for (ksa = KeysymAliasTable; ksa < KeysymAliasTable + NUMALIASES;
	 ++ksa)
    {
	if (0 == strcmp(word, ksa->alias)) {
	    if (ksa->mask != 0)
		return ksa->mask;

	    kc = XKeysymToKeycode(dpy, ksa->sym1);

	    if (kc == 0)
		kc = XKeysymToKeycode(dpy, ksa->sym2);

	    if (kc == 0)
		return 0;
	    else
		return FindModifierMask(kc);
	}
    }

    return 0;
}


/*
 * Parses a comma-separated string into words, gets the modifier mask for 
 * each, ORs them together and returns the result.
 */
unsigned int
polyStringToModifier(dpy, str)
    Display *dpy;
    char *str;
{
    char buf[200];
    unsigned int result = 0;
    char *word;

    /* make a copy first, because strtok riddles the string with nulls. */
    strcpy(buf, str);
    word = strtok(buf, ",");
    while (word != NULL) {
	result |= stringToModifier(dpy, word);
	word = strtok(NULL, ",");
    }
    return result;
}


#define MAX_MAPPINGS 6

/*
 * Parses a key specification of the form
 *	    keymod[,keymod[,...]]
 * where keymod is
 *	    keysym[+modifier[+...]]
 */
static int
parseKeySpec(dpy, specifier, syms)
    Display *dpy;
    char *specifier;
    modsym *syms;
{
    char spec[200];
    char *mapping[MAX_MAPPINGS];
    char *keysym_string, *mod_string;
    KeySym keysym;
    int k;
    int modmask, newmod;
    int nbound = 0;
    char buffer[200];

    /* make a copy first, because strtok riddles the string with nulls. */
    strcpy(spec, specifier);

    /* break apart comma-separated mappings */
    mapping[0] = strtok(spec, ",");
    for (k = 1; k < MAX_MAPPINGS; ++k) {
	mapping[k] = strtok(NULL, ",");
	if (mapping[k] == NULL)
	    break;
    }

    /* for each mapping, break into keysym and modifier components */
    for (k = 0; k < MAX_MAPPINGS && mapping[k]; ++k) {
	keysym_string = strtok(mapping[k], "+");
	if (!keysym_string) {
	    (void) sprintf(buffer, GetString("bad key mapping `%s'\n"),
			  mapping[k]);
	    ErrorWarning(buffer);
	    continue;
	}
	keysym = XStringToKeysym(keysym_string);
	if (keysym == NoSymbol) {
	    (void) sprintf(buffer, GetString("can't find keysym for `%s'\n"),
			   keysym_string);
	    ErrorWarning(buffer);
	    continue;
	}

	/*
	 * If the keysym is upper case alphabetic, add a shift mask.  If it's 
	 * lower case, convert it to upper case.
	 */
	modmask = 0;
	if (XK_A <= keysym && keysym <= XK_Z)
	    modmask = ShiftMask;
	if (XK_a <= keysym && keysym <= XK_z)
	    keysym -= (XK_a - XK_A);

	while (1) {
	    mod_string = strtok(NULL, "+");
	    if (!mod_string)
		break;
	    newmod = stringToModifier(dpy, mod_string);
	    if (newmod == 0) {
		/*
		 * We couldn't find a modifier; ignore this binding.  We can't 
		 * use continue, because we want to continue an outer loop.
		 */
		goto nobinding;
	    }
	    modmask |= newmod;
	}
	syms[nbound].sym = keysym;
	syms[nbound].mod = modmask;
	++nbound;
    nobinding: ;
    }  /* for each mapping */
    return nbound;
}


/*
 * Run through the table of key descriptors and establish key bindings for
 * each descriptor.  First, the resource database is probed for a customized
 * key binding specification.  If one isn't found, the default key binding
 * specification is used.  Then, this specification is parsed into an array of
 * keysym/modifier pairs.  For each pair, the keyboard mapping table is
 * searched for the keysym and an entry is made into the binding table for
 * each instance of the keysym in the mapping table.  Thus, if a keysym
 * appears on more than one keystation, a key binding entry is created for
 * each.
 *
 * The Mouseless and Menu Accelerator modes are checked before the binding is
 * added.  For menu accelerator functions, if general menu accelerators are on
 * \and/ window menu accelerators are on, the binding is added.  For other
 * functions, the appropriate Mouseless mode is checked.  To effect a binding
 * that is always on, use KD_ALWAYS in the flags field of the key descriptor.
 *
 * Note: this is a change from earlier behavior, where a binding resource
 * would always be honored even if the Mouseless mode indicated that it would
 * be turned off.  Now, a binding is ignored if the Mouseless or Menu
 * Accelerator mode indicates that it should be.
 *
 */
static void
establishKeyBindings(dpy, rdb)
    Display *dpy;
    XrmDatabase rdb;
{
    KeyDescriptor *d;
    modsym syms[MAX_MAPPINGS];
    int nsyms;
    int i, j;
    int keytblsize = (MaxKeyCode-MinKeyCode+1) * KeySymsPerKeyCode;
    XrmQuark classlist[4], namelist[4];
    XrmQuark rep;
    XrmValue value;
    char *keyspec;

    classlist[3] = NULLQUARK;

    namelist[0] = TopInstanceQ;
    namelist[3] = NULLQUARK;

    for (d=KeyDescriptorTable; d < KeyDescriptorTable+NUMKEYDESCRIPTORS;
	    ++d) {

        classlist[0] = OpenWinQ;
	if (d->flags & KD_MENU_ACCEL) {
	    if (! GRV.WindowMenuAccelerators || ! GRV.MenuAccelerators)
		continue;
	    classlist[1] = menuAccelClassQ;
	    namelist[1] = menuAccelInstanceQ;
	} else {
	    if (! (d->flags & mouselessMaskTable[GRV.Mouseless]))
		continue;
	    classlist[1] = kbdCmdClassQ;
	    namelist[1] = kbdCmdInstanceQ;
	}

	classlist[2] = namelist[2] = XrmStringToQuark(d->rsrc_name);
	if (XrmQGetResource(rdb, namelist, classlist, &rep, &value)) {
	    keyspec = (char *) value.addr;
	} else {
	    /*
	     * Use olwm binding; see resources.c
	     */
	    classlist[0] = OlwmQ;
	    if (XrmQGetResource(rdb, namelist, classlist, &rep, &value))
		keyspec = (char *) value.addr;
	    else keyspec = d->dflt_binding;
	}
	nsyms = parseKeySpec(dpy, keyspec, syms);
	for (i=0; i<nsyms; ++i) {
	    for (j=0; j<keytblsize; ++j) {
		if (KbdMap[j] == syms[i].sym) {
		    AddKeyBinding(j/KeySymsPerKeyCode+MinKeyCode, syms[i].mod, d);
		    j += KeySymsPerKeyCode - (j % KeySymsPerKeyCode) - 1;
		}
	    }
	}
    }
}
	

/*
 * Issue or release passive grabs for the necessary keys on this particular
 * root window.  Run through the binding table and un/grab the key-modifier
 * combination itself, and also combined with the Lock and NumLock (if any)
 * modifier.  There thus may be up to four actual grabs per key binding.
 *
 * If virtual_only is true, we only grab the virtual root keys; this is
 * e.g. when .olvwmrc changes the keys we're interested in.
 */
static void
grabRootKeys(dpy, root, grab, virtual_only)
    Display *dpy;
    Window root;
    Bool grab;		/* True = grab, False = release */
    Bool virtual_only;
{
    KeyBinding *k;
    unsigned int NumLockMask =
	FindModifierMask(XKeysymToKeycode(dpy, XK_Num_Lock));

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->desc->function != NULLFUNC &&
	    (!virtual_only || k->desc->action == ACTION_VIRTUAL)) {
	    if (grab) {
		if (k->desc->action != ACTION_VIRTUAL || GRV.GrabVirtualKeys) {
		    XGrabKey(dpy, k->keycode, k->modstate,
			 root, False, GrabModeAsync, GrabModeSync);
		    XGrabKey(dpy, k->keycode, k->modstate | LockMask,
			 root, False, GrabModeAsync, GrabModeSync);
		}
	    } else {
		XUngrabKey(dpy, k->keycode, k->modstate, root);
		XUngrabKey(dpy, k->keycode, k->modstate|LockMask, root);
	    }
	    if (NumLockMask != 0) {
		if (grab) {
		    if (k->desc->action != ACTION_VIRTUAL || GRV.GrabVirtualKeys) {
		        XGrabKey(dpy, k->keycode, k->modstate | NumLockMask,
			     root, False, GrabModeAsync, GrabModeSync);
		        XGrabKey(dpy, k->keycode,
			     k->modstate | NumLockMask | LockMask,
			     root, False, GrabModeAsync, GrabModeSync);
		    }
		} else {
		    XUngrabKey(dpy, k->keycode, k->modstate | NumLockMask,
			       root);
		    XUngrabKey(dpy, k->keycode,
			       k->modstate | NumLockMask | LockMask, root);
		}
	    }
	}
    }
}


/*
 * Issue or release passive button grabs on this root window.  Like
 * grabRootKeys, has to deal with Lock and NumLock by issuing up to four
 * separate grabs.  Note: these are synchronous grabs.  This relies on the
 * root event handler to issue an AllowEvents or GrabPointer request.
 */
static void
grabRootButtons(dpy, root, grab)
    Display *dpy;
    Window root;
    Bool grab;		/* True = grab, False = release */
{
    unsigned int NumLockMask =
	FindModifierMask(XKeysymToKeycode(dpy, XK_Num_Lock));
    unsigned int eventmask =
	ButtonPressMask | ButtonMotionMask | ButtonReleaseMask;

    if (ModMaskMap[MOD_WMGRAB] != 0) {
	if (grab) {
	    XGrabButton(dpy, AnyButton,
			ModMaskMap[MOD_WMGRAB],
			root, False, eventmask,
			GrabModeSync, GrabModeSync, None, None);
	    XGrabButton(dpy, AnyButton,
			ModMaskMap[MOD_WMGRAB] | LockMask,
			root, False, eventmask,
			GrabModeSync, GrabModeSync, None, None);
	} else {
	    XUngrabButton(dpy, AnyButton,
			  ModMaskMap[MOD_WMGRAB],
			  root);
	    XUngrabButton(dpy, AnyButton,
			  ModMaskMap[MOD_WMGRAB] | LockMask,
			  root);
	}

	if (NumLockMask != 0) {
	    if (grab) {
		XGrabButton(dpy, AnyButton,
			    ModMaskMap[MOD_WMGRAB] | NumLockMask,
			    root, False, eventmask,
			    GrabModeSync, GrabModeSync, None, None);
		XGrabButton(dpy, AnyButton,
			    ModMaskMap[MOD_WMGRAB] | LockMask | NumLockMask,
			    root, False, eventmask,
			    GrabModeSync, GrabModeSync, None, None);
	    } else {
		XUngrabButton(dpy, AnyButton,
			      ModMaskMap[MOD_WMGRAB] | NumLockMask,
			      root);
		XUngrabButton(dpy, AnyButton,
			      ModMaskMap[MOD_WMGRAB] | LockMask | NumLockMask,
			      root);
	    }
	}
    }
}


/* ===== public functions ================================================= */

/*
 * Given a semantic action, looks it up in the keyboard binding table.
 * Returns a pointer to the first key binding record that matches the semantic
 * action.  The binding record is owned by the event binding system and must
 * not be altered by the caller, nor should references to it be maintained by
 * the caller, as it may be reclaimed if key bindings change.  Returns NULL if
 * no binding can be found.  As a special case, if the semantic action is
 * ACTION_NONE, NULL is returned.
 */

KeyBinding *
LookupKeyBinding(action)
    SemanticAction      action;
{
    KeyBinding          *k;
 
    if (action == ACTION_NONE)
	return NULL;

    for (k = KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->desc->action == action)
	    return k;
    }
    return NULL;
}

/*
 * Given a keyboard event, looks it up in the keyboard binding table.  If a 
 * binding is found, returns the semantic action associated with that key.  If 
 * no binding is found, returns ACTION_NONE.
 */

SemanticAction
/* ARGSUSED */
FindKeyboardAction(dpy, event)
    Display *dpy;
    XEvent *event;
{
    KeyBinding *k;
    SemanticAction a = ACTION_NONE;
    unsigned long ignore = ModMaskMap[MOD_IGNORE] | AnyButtonMask;

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->keycode == event->xkey.keycode
		&& k->modstate == (event->xkey.state & ~ignore)) {
	    a = k->desc->action;
	    break;
	}
    }
    return a;
}

/*
 * Keyboard actions added by olvwmrc are at the end of the list, but they
 * should take precedence over previous ones.  So this function does a
 * similar search as above but starts at the end.
 */
SemanticAction
/* ARGSUSED */
FindNewKeyboardAction(dpy, event)
    Display *dpy;
    XEvent *event;
{
    KeyBinding *k;
    SemanticAction a = ACTION_NONE;
    unsigned long ignore = ModMaskMap[MOD_IGNORE] | AnyButtonMask;

    for (k=KeyBindingTable+bindingTableCount-1; k >= KeyBindingTable; --k) {
	if (k->keycode == event->xkey.keycode
		&& k->modstate == (event->xkey.state & ~ignore)) {
	    a = k->desc->action;
	    break;
	}
    }
    return a;
}


/*
 * Given a keyboard event, looks it up in the keyboard binding table.
 * If a binding is found, executes the function bound to that key.  Returns 
 * True if a function was found and called, otherwise False.
 */
Bool
ExecuteKeyboardFunction(dpy, event)
    Display *dpy;
    XEvent *event;
{
    KeyBinding *k;
    void (*f)() = NULLFUNC;
    unsigned long ignore = ModMaskMap[MOD_IGNORE] | AnyButtonMask;

    for (k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
	if (k->keycode == event->xkey.keycode
		&& k->modstate == (event->xkey.state & ~ignore)
		&& k->desc->function != NULLFUNC) {
	    f = k->desc->function;
	    break;
	}
    }

    /* If the user pressed the STOP key, clear active key. */

    if (f == NULLFUNC) {
	if (FindKeyboardAction(dpy, event) == ACTION_STOP)
	    activeKey = NULL;
	XAllowEvents(dpy, AsyncKeyboard, event->xkey.time);
	return False;
    }

    /* invariant: k points to a valid key binding */

#ifdef notdef
    /*
     * On the first keypress, stash the active key binding; ignore subsequent
     * keypresses.  Ignore all key releases except the one corresponding to
     * the active binding.
     */
    if (event->type == KeyPress) {
	if (activeKey == NULL)
	    activeKey = k;
	else
	    return False;
    } else {			    /* KeyRelease */
	if (k == activeKey)
	    activeKey = NULL;
	else
	    return False;	    /* ignore it */
    }
#endif

    if (mouselessSuspended && !(k->desc->flags & KD_IMMUNE)) {
	XAllowEvents(dpy, ReplayKeyboard, event->xkey.time);
	return True;
    }

    if (quotenext) {
	XAllowEvents(dpy, ReplayKeyboard, event->xkey.time);
	quotenext = False;
	return True;
    }

    /*
     * Simply replay the event if this client has disallowed menu
     * accelerators.
     */
    if (k->desc->flags & KD_MENU_ACCEL && CurrentClient != NULL
        && ! CurrentClient->menuAccelerators) {
	XAllowEvents(dpy, ReplayKeyboard, event->xkey.time);
	return True;
    }

    if (event->type == KeyPress)
	XAllowEvents(dpy, AsyncKeyboard, event->xkey.time);

    (*f)(dpy, event);
    return True;
}


/* ===== Initialization =================================================== */

/*
 * Deal with key grabs on all root windows.  If grab = True, grab the keys; if 
 * grab = False, release the keys.  Note: the screens and the keyboard binding 
 * information must be initialized prior to calling this function.
 */

void
GrabVKeys(dpy, grab, virtual_only)
    Display *dpy;
    Bool grab;
    Bool virtual_only;
{
    List *l = ScreenInfoList;
    ScreenInfo *scr;
    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l))
	grabRootKeys(dpy, scr->rootid, grab, virtual_only);
}

void
GrabKeys(dpy, grab)
    Display *dpy;
    Bool grab;
{
    GrabVKeys(dpy, grab, False);
}

/*
 * Remove all key grabs, zero out the binding table, and rebuild it from the 
 * resource database.  Then, re-establish key grabs.
 */

void
RefreshKeyGrabsFile(dpy, rdb, file)
    Display	*dpy;
    XrmDatabase  rdb;
    char	*file;

{
    GrabKeys(dpy, False);
    bindingTableCount = 0;
    establishKeyBindings(dpy, rdb);
    ReInitOlvwmRC(dpy, file);
    GrabKeys(dpy, True);
}

void
RefreshKeyGrabs(dpy, rdb)
    Display *dpy;
    XrmDatabase rdb;
{
    RefreshKeyGrabsFile(dpy, rdb, (char *) NULL);
}

/*
 * Deal with button grabs on all root windows.  If grab = True, grab the 
 * buttons; if grab = False, release them.
 */
void
GrabButtons(dpy, grab)
    Display *dpy;
    Bool grab;
{
    List *l = ScreenInfoList;
    ScreenInfo *scr;

    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l))
	grabRootButtons(dpy, scr->rootid, grab);
}


/*
 * Remove all button grabs, regenerate the modifier mask table, and
 * re-establish the button grabs.
 */
void
RefreshButtonGrabs(dpy)
    Display *dpy;
{
    GrabButtons(dpy, False);
    establishModBindings(dpy, OlwmDB);
    GrabButtons(dpy, True);
}


/*
 * Update all bindings from a new resource database.  Called whenever the 
 * resource database changes.  
 */
Bool
UpdateBindings(dpy, newDB, regrabKeys)
    Display *dpy;
    XrmDatabase	newDB;
    Bool regrabKeys;
{
    KeyDescriptor *d;
    XrmQuark classlist[4], instlist[4];
    XrmQuark rep;
    XrmValue newvalue, oldvalue;
    int	size = sizeof(mouselessMaskTable) / sizeof(mouselessMaskTable[0]);
    int	i;
    Bool newexists, oldexists;

    if (GRV.GrabVirtualKeys)
        for (i = 0; i < size; i++)
	    mouselessMaskTable[i] |= KD_VIRTUAL;
     else for (i = 0; i < size; i++)
	    mouselessMaskTable[i] &= ~KD_VIRTUAL;
    GrabButtons(dpy, False);
    establishModBindings(dpy, newDB);
    GrabButtons(dpy, True);

    /*
     * Run through the KeyDescriptorTable and probe resources to see if any of
     * the binding resources has changed.
     */

    classlist[3] = NULLQUARK;

    instlist[0] = TopInstanceQ;
    instlist[3] = NULLQUARK;

    for (d=KeyDescriptorTable; d < KeyDescriptorTable+NUMKEYDESCRIPTORS; ++d) {

        classlist[0] = OpenWinQ;
	if (d->flags & KD_MENU_ACCEL) {
	    classlist[1] = menuAccelClassQ;
	    instlist[1] = menuAccelInstanceQ;
	} else {
	    classlist[1] = kbdCmdClassQ;
	    instlist[1] = kbdCmdInstanceQ;
	}

	classlist[2] = instlist[2] = XrmStringToQuark(d->rsrc_name);

	newexists = 
	    XrmQGetResource(newDB, instlist, classlist, &rep, &newvalue);
	oldexists = 
	    XrmQGetResource(OlwmDB, instlist, classlist, &rep, &oldvalue);

	if (!newexists) {
	    classlist[0] = OlwmQ;
	    newexists = 
	        XrmQGetResource(newDB, instlist, classlist, &rep, &newvalue);
	}
	if (!oldexists) {
	    classlist[0] = OlwmQ;
	    oldexists = 
	        XrmQGetResource(OlwmDB, instlist, classlist, &rep, &oldvalue);
	}
	
	/* skip resources that never existed */
	if (!newexists && !oldexists)
	    continue;
	
	/* skip resources that haven't changed */
	if (newexists && oldexists &&
	    0 == strcmp((char *) newvalue.addr, (char *) oldvalue.addr))
	        /* old and new values the same; ignore */
	        continue;

	regrabKeys = True;
    }

    if (regrabKeys)
	RefreshKeyGrabs(dpy, newDB);

    return regrabKeys;
}


/*
 * Initialize the event handling system, but don't do any key grabbing.  This 
 * function is called exactly *once* at startup.
 */
void
InitBindings(dpy)
    Display *dpy;
{
int	size = sizeof(mouselessMaskTable) / sizeof(mouselessMaskTable[0]);
int	i;

    kbdCmdInstanceQ = XrmStringToQuark("keyboardCommand");
    kbdCmdClassQ    = XrmStringToQuark("KeyboardCommand");

    modInstanceQ    = XrmStringToQuark("modifier");
    modClassQ	    = XrmStringToQuark("Modifier");

    menuAccelInstanceQ  = XrmStringToQuark("menuAccelerator");
    menuAccelClassQ     = XrmStringToQuark("MenuAccelerator");

    if (GRV.GrabVirtualKeys)
        for (i = 0; i < size; i++)
	    mouselessMaskTable[i] |= KD_VIRTUAL;
    KeyBindingTable = MemCalloc(KEYBINDING_TABLE_SIZE,sizeof(KeyBinding));
    bindingTableSize = KEYBINDING_TABLE_SIZE;
    establishKeyBindings(dpy, OlwmDB);
    establishModBindings(dpy, OlwmDB);
}

/*
 * Disable/Enable program keys in given range
 */
void
SetProgKeys(dpy, start_sym, end_sym, modstate, off)
    Display *dpy;
    KeySym start_sym;
    KeySym end_sym;
    unsigned long modstate;
    Boolean off;
{
    KeyBinding *k;
    KeySym keysym;
    extern CheckForKeyProg();

    for(k=KeyBindingTable; k < KeyBindingTable+bindingTableCount; ++k) {
      if (k->desc->action == ACTION_VIRTUAL) {
          keysym = XKeycodeToKeysym(dpy, k->keycode, 0);
          if ((modstate == 0L || (k->modstate & modstate))
                  && (start_sym == NoSymbol || keysym >= start_sym)
                  && (end_sym == NoSymbol || keysym <= end_sym)) {
              k->desc->function =
                  off ? NULLFUNC : (void (*)()) CheckForKeyProg;
          }
      }
    }
}
