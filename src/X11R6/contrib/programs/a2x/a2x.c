/* $XConsortium: a2x.c,v 1.130 94/04/17 20:45:38 rws Exp $ */
/*

Copyright (c) 1992  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

/*

Syntax of magic values in the input stream:

^T^A<kdelay> <delta> <mdelay>^T
			autorepeat every <kdelay> seconds for keys
			else change non-zero motion dx, dy to <delta>
			and autorepeat every <mdelay> seconds for motion
			<kdelay> and <mdelay> are floating point
			zero for any value means don't change it
^T^B<button>^T		toggle button <button> state (press or release)
^T^B0^T			release all pressed buttons
^T^C			set Control key for next device event
^T^D<dx> <dy>^T		move mouse by (<dx>, <dy>) pixels
^T^E			exit the program
^T^F<options>^T
	r		start recording macro
	a		abort recording of macro
	s<digit>	save recording as macro <digit>
	e<digit>	execute macro <digit>
	d<digit>	delete macro <digit>
^T^J<options>[ <mult>]^T
			jump to next closest top-level window
	Z		no-op letter to soak up uppercase from prev word 
	C		closest top-level window
	D		top-level window going down
	L		top-level window going left
	R		top-level window going right
	U		top-level window going up
	O		skip overlapping top-level windows
	c		closest widget
	d		widget going down
	l		widget going left
	r		widget going right
	u		widget going up
	k		require windows that select for key events
			(with b, means "key or button")
	b		require windows that select for button events
			(with k, means "key or button")			
	n<name>		require window with given name (WM_NAME)
			this must be the last option
	p<prefix>	require window with given name prefix (WM_NAME)
			this must be the last option
	N[<name>.<class>] require window with given name and/or class
			(WM_CLASS or _MIT_OBJ_CLASS)
			this must be the last option
			either <name> or <class> can be empty
	P[<name>.<class>] require window with given name and/or class prefix
			(WM_CLASS or _MIT_OBJ_CLASS)
			this must be the last option
			either <name> or <class> can be empty
	[ <mult>]	off-axis distance multiplier is <mult> (float)
^T^L<options>^T
	s<digit>	save current pointer position as location <digit>
			relative to window origin
	S<digit>	save current pointer position as location <digit>
			relative to closest window edges
	w<digit>	warp pointer to location <digit>
^T^M			set Meta key for next device event
^T^P			print debugging info
^T^Q			quit moving (mouse or key)
^T^RD<display>^T	switch to a new display
			:0 added if <display> contains no colon
^T^S			set Shift key for next device event
^T^T			^T
^T^U			re-read undo file
^T^V<string>^T		echo <string> to stdout
^T^W<dest> <x> <y>^T	warp to position (<x>,<y>) on destination <dest>
	if <x> is negative, it is replaced with width_of(<dest>) + <x>
	if <y> is negative, it is replaced with height_of(<dest>) + <x>
	values for <dest>:
	S or -1		current screen
	W or -2		top-level window containing pointer
	w or -3		innermost window containing pointer
	<screen>	screen number <screen>
^T^Y<options>[ <delay>]^T
			set/await trigger
	M		set MapNotify trigger
	U		set UnmapNotify trigger
	S<selection>	set trigger for selection named <selection>
	W		wait for trigger
	n<name>		require window with given name (WM_NAME)
			this must be the last option
	p<prefix>	require window with given name prefix (WM_NAME)
			this must be the last option
	N[<name>.<class>] require window with given name and/or class
			(WM_CLASS or _MIT_OBJ_CLASS)
			this must be the last option
			either <name> or <class> can be empty
	P[<name>.<class>] require window with given name and/or class prefix
			(WM_CLASS or _MIT_OBJ_CLASS)
			this must be the last option
			either <name> or <class> can be empty
	[ <delay>]	wait no more than <delay> seconds
			<delay> is floating-point
^T^Z<delay>^T		add <delay>-second delay to next event
			<delay> is floating-point
^T<hexstring>^T		press and release key with numeric keysym <hexstring>
			F<char> and F<char><char> are names, not numbers
^T<keysym>^T		press and release key with keysym named <keysym>

Note: if key is attached to a modifier, pressing it is temporary, will be
released automatically at next button or non-modifier key.

*/

#include <stdio.h>
#include <X11/Xos.h>
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *malloc(), *getenv();
#endif
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#ifdef XTRAP
#define NEED_EVENTS
#define NEED_REPLIES
#endif
#include <X11/Xproto.h>
#include <X11/extensions/shape.h>
#ifdef XTEST
#include <X11/extensions/XTest.h>
#endif
#ifdef XTRAP
#include <X11/extensions/xtraplib.h>
#include <X11/extensions/xtraplibp.h>
#endif
#ifdef XTESTEXT1
#include <X11/Xmd.h>
#include <X11/extensions/xtestext1.h>
#endif
#include <X11/keysym.h>
#include <X11/Xmu/WinUtil.h>
#if XlibSpecificationRelease >= 5
#include <X11/Xfuncs.h>
#endif
#include <ctype.h>
#ifndef MSDOS
#ifndef X_NOT_POSIX
#include <termios.h>
#else
#include <sgtty.h>
#endif
#else
#include <sys/time.h>
#include <sys/socket.h>
#endif
#define _POSIX_SOURCE
#include <signal.h>

#define standard_control_char '\024' /* control T */
#define standard_control_end '\224'

typedef enum
{MatchNone, MatchName, MatchNamePrefix, MatchClass, MatchClassPrefix}
MatchType;

typedef struct {
    MatchType match;
    int namelen;
    int classlen;
    char name[100];
    char class[100];
} MatchRec;

typedef struct {
    char dir;
    double mult;
    Screen *screen;
    int rootx, rooty;
    Mask input;
    Window best;
    int bestx, besty;
    double best_dist;
    Bool recurse;
    MatchRec match;
    Region overlap;
} JumpRec;

typedef struct {
    int x1;
    int y1;
    int x2;
    int y2;
} Box;

typedef struct {
    Window root;
    int type;
    unsigned long serial;
    struct timeval time;
    MatchRec match;
    Atom selection;
    int count;
    Window *windows;
} TriggerRec;

typedef struct _undo {
    struct _undo *next;
    int bscount;
    char *seq;
    int seq_len;
    char *undo;
    int undo_len;
} UndoRec;

typedef struct _macro {
    char *macro;
    int len;
} MacroRec;

typedef struct _location {
    Window window;
    int x, y;
    Bool from_right, from_bottom;
} LocationRec;

#ifdef MSDOS
#define Meta (1<<13)
typedef struct _keybinding {
    unsigned short modifiers;
    KeySym keysym;
    char c;
} KeyBindingRec;

KeyBindingRec pc_keys[] =
#include "pckeys.h"
#endif

#ifndef OLDDD
#define OLDDD False
#endif
Bool oldDD = OLDDD;
char *progname;
Display *dpy;
Atom MIT_OBJ_CLASS;
int Xfd;
int maxfd;
fd_set fdmask;
unsigned char button_map[256];
Bool button_state[256];
unsigned short modifiers[256];
KeyCode keycodes[256];
unsigned short modmask[256];
unsigned short curmods = 0;
unsigned short tempmods = 0;
Bool meta_is_alt = False;
KeyCode shift, control, mod1, mod2, mod3, mod4, mod5, meta;
char control_char = standard_control_char;
char control_end = standard_control_end;
Bool bs_is_del = True;
Bool bs_is_backspace = False;
char pc_bs = '\b';
KeySym last_sym = 0;
KeyCode last_keycode_for_sym = 0;
#ifndef MSDOS
#ifndef X_NOT_POSIX
struct termios oldterm;
#else
struct sgttyb oldterm;
#endif
#endif
Bool istty = False;
struct timeval timeout;
struct timeval *moving_timeout = NULL;
int moving_x = 0;
int moving_y = 0;
int last_keycode = 0;
unsigned short last_mods = 0;
unsigned long time_delay;
int (*olderror)();
int (*oldioerror)();
char history[8192];
int history_end = 0;
int macro_start = -1;
char *undofile = NULL;
#define UNDO_SIZE 256
UndoRec *undos[UNDO_SIZE];
int curbscount = 0;
UndoRec *curbsmatch = NULL;
Bool in_control_seq = False;
Bool skip_next_control_char = False;
TriggerRec trigger;
Window my_window = None;
JumpRec jump;
MacroRec macros[10];
LocationRec locations[10];
Bool need_bell = False;
Bool noecho = True;
Bool fakeecho = False;
Bool doclear = False;
char *hotwinname = "a2x";
Window hotwin = None;
char *hotkeyname = NULL;
char *hotwingeom = NULL;
Bool hotwinfocus = True;
KeyCode hotkey = 0;
#ifdef XTRAP
XETC *tc;
#endif
Bool do_mouse_fixup = False;
#ifdef XTESTEXT1
Bool fixup_mouse = False;
#endif
void (*generate_key)();
void (*generate_button)();
void (*generate_motion)();
void (*generate_warp)();
void (*flush_generate)();
void (*clean_up)();

void process();
KeyCode parse_keysym();

#if defined(XTRAP) || defined(XTESTEXT1)
void
delay_time() /* we have to approximate the delay */
{
    struct timeval delay;

    XFlush(dpy);
    delay.tv_sec = time_delay / 1000;
    delay.tv_usec = (time_delay % 1000) * 1000;
    select(0, NULL, NULL, NULL, &delay);
}
#endif

#ifdef XTEST
void
xtest_generate_key(key, press)
    int key;
    Bool press;
{
    XTestFakeKeyEvent(dpy, key, press, time_delay);
    time_delay = 0;
}
#endif

#ifdef XTRAP
void
xtrap_generate_key(key, press)
    int key;
    Bool press;
{
    delay_time();
    XESimulateXEventRequest(tc, press ? KeyPress : KeyRelease, key, 0, 0, 0);
    time_delay = 0;
}
#endif

#ifdef XTESTEXT1
void xtestext1_correct_mouse()
{
    Window root, w;
    int rx, ry, wx, wy;
    unsigned int m;

    XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &w,
		  &rx, &ry, &wx, &wy, &m);
    XTestMovePointer(dpy, 2, 0, &rx, &ry, 1);
    fixup_mouse = False;
}

void
xtestext1_generate_key(key, press)
    int key;
    Bool press;
{
    if (fixup_mouse)
	xtestext1_correct_mouse();
    XTestPressKey(dpy, 1, time_delay, key, press ? XTestPRESS : XTestRELEASE);
    time_delay = 0;
}
#endif

#ifdef XTEST
void
xtest_generate_button(button, press)
    int button;
    Bool press;
{
    XTestFakeButtonEvent(dpy, button, press, time_delay);
    time_delay = 0;
}
#endif

#ifdef XTRAP
void
xtrap_generate_button(button, press)
    int button;
    Bool press;
{
    delay_time();
    XESimulateXEventRequest(tc, press ? ButtonPress : ButtonRelease,
			    button, 0, 0, 0);
    time_delay = 0;
}
#endif

#ifdef XTESTEXT1
void
xtestext1_generate_button(button, press)
    int button;
    Bool press;
{
    if (fixup_mouse)
	xtestext1_correct_mouse();
    XTestPressButton(dpy, 2, time_delay, button,
		     press ? XTestPRESS : XTestRELEASE);
    time_delay = 0;
}
#endif

#ifdef XTEST
void
xtest_generate_motion(dx, dy)
    int dx, dy;
{
    XTestFakeRelativeMotionEvent(dpy, dx, dy, time_delay);
    time_delay = 0;
}
#endif

#if defined(XTRAP) || defined(XTESTEXT1)
void
warp_generate_motion(dx, dy)
    int dx, dy;
{
    delay_time();
    XWarpPointer(dpy, None, None, 0, 0, 0, 0, dx, dy);
    time_delay = 0;
}
#endif

#ifdef XTEST
void
xtest_generate_warp(screen, x, y)
    int screen, x, y;
{
    XTestFakeMotionEvent(dpy, screen, x, y, time_delay);
    time_delay = 0;
}
#endif

#ifdef XTRAP
void
xtrap_generate_warp(screen, x, y)
    int screen, x, y;
{
    delay_time();
    XESimulateXEventRequest(tc, MotionNotify, 0, x, y, 0);
    time_delay = 0;
}
#endif

#ifdef XTESTEXT1
void
xtestext1_generate_warp(screen, x, y)
    int screen, x, y;
{
    delay_time();
    XWarpPointer(dpy, None, DefaultRootWindow(dpy), 0, 0, 0, 0, x, y);
    time_delay = 0;
}
#endif

#ifdef XTESTEXT1
void
xtestext1_flush_generate()
{
    XTestFlush(dpy);
    fixup_mouse = do_mouse_fixup;
}
#endif

#ifdef XTRAP
void
xtrap_clean_up()
{
    if (tc)
	XEFreeTC(tc);      
    tc = NULL;
}
#endif

#ifndef MSDOS
#ifndef X_NOT_STDC_ENV
#define Strtod strtod
#define Strtol strtol
#else
double
Strtod(nptr, endptr)
    char *nptr;
    char **endptr;
{
    double val = 0.0;

    while (isspace(*nptr))
	nptr++;
    sscanf("%lf", &val);
    while (*nptr && !isspace(*nptr))
	nptr++;
    *endptr = nptr;
    return val;
}

long
Strtol(nptr, endptr, base)
    char *nptr;
    char **endptr;
    int base;
{
    long val = 0;

    while (isspace(*nptr))
	nptr++;
    sscanf(nptr, base == 10 ? "%ld" : "%lx", &val);
    while (*nptr && !isspace(*nptr))
	nptr++;
    *endptr = nptr;
    return val;
}
#endif
#endif

void
usage()
{
    printf("usage: %s [-display host:dpy] option ...\n", progname);
    printf("    -a\t\t\tuse Alt instead of Meta\n");
    printf("    -b\t\t\tdon't translate Backspace into Delete\n");
    printf("    -B\t\t\tPC switches Backspace and Delete\n");
    printf("    -bs\t\t\ttranslate Backspace into BackSpace\n");
    printf("    -c\t\t\tclears the screen at startup\n");
    printf("    -C <char>\t\tmake the control sequence char control-<char>\n");
    printf("    -e\t\t\techoes all characters typed (for debugging)\n");
    printf("    -E\t\t\tforce fake echo to help DOS telnet\n");
    printf("    -f\t\t\tDOS window should not receive synthetic focus\n");
    printf("    -g <geometry>\tgeometry to make DOS window\n");
    printf("    -h <keysym>\t\thotkey for DESQview/X\n");
    printf("    -p\t\t\tquery pointer position before every pointer motion\n");
    printf("    -u <undofile>\tuse the named undo file\n");
    printf("    -U\t\t\tturn off undo interpretation\n");
    printf("    -v <version>\tversion of DragonDictate being used\n");
    printf("    -w <name>\t\tname of DESQview/X DOS window\n");
    exit(1);
}

void
reset()
{
#ifndef MSDOS
    if (istty) {
#ifndef X_NOT_POSIX
	tcsetattr(0, TCSANOW, &oldterm);
#else
	ioctl(0, TIOCSETP, &oldterm);
#endif
    }
#endif
}

void
quit(val)
    int val;
{
    if (val < 0) {
	perror("a2x");
	val = 1;
    }
    reset();
    exit(val);
}

/*ARGSUSED*/
void
catch(sig)
int	sig;
{
    fprintf(stderr, "%: interrupt received, exiting\n", progname);
    quit(1);
}

int
error(Dpy, err)
    Display *Dpy;
    XErrorEvent *err;
{
    if (err->error_code == BadWindow || err->error_code == BadDrawable) {
	if (err->request_code == X_WarpPointer)
	    need_bell = True;
	return 0;
    }
    reset();
    return (*olderror)(Dpy, err);
}

int
ioerror(Dpy)
    Display *Dpy;
{
    reset();
    return (*oldioerror)(Dpy);
}

int
ddread(buf, len)
    char *buf;
    int len;
{
    int n;

#ifndef MSDOS
    n = read(0, buf, len);
    if (fakeecho)
	write(1, "\177", 1);
#else
    n = getch();
    if (n > 255) {
	if (n < 256 + sizeof(pc_keys) / sizeof(KeyBindingRec))
	    n = pc_keys[n - 256].c;
	else
	    n = ' ';
    }
    if (!noecho) {
	if (iscntrl(n))
	    printf("^%c", n + 'A' - 1);
	else
	    putchar(n);
    }
    *buf = n;
    n = 1;
#endif
    return n;
}

void
map_sym(i, j)
    int i, j;
{
    KeySym sym;

    sym = XKeycodeToKeysym(dpy, i, j);
    if (sym > 128)
	switch (sym) {
	case XK_Return:
	    sym = '\r';
	    break;
	case XK_Tab:
	    sym = '\t';
	    break;
	case XK_Escape:
	    sym = '\033';
	    break;
	case XK_Delete:
	    sym = 127;
	    break;
	}
    if (sym > 0 && sym < 128 && (!j || !keycodes[sym])) {
	keycodes[sym] = i;
	modifiers[sym] = j ? ShiftMask : 0;
    }
}

void
reset_mapping()
{
    int minkey, maxkey;
    int i, j, x, y, c;
    KeySym sym;
    XModifierKeymap *mmap;
    Window root, w, *children;
    char *name;
    unsigned int nchild, width, height, bwidth, depth;
    unsigned short modsmask;
    long mask;
    XSizeHints hints;
    unsigned char bmap[256];
    
    bzero((char *)button_map, sizeof(button_map));
    j = XGetPointerMapping(dpy, bmap, sizeof(bmap));
    for (i = 0; i < j; i++)
	button_map[bmap[i]] = i + 1;
    XDisplayKeycodes(dpy, &minkey, &maxkey);
    bzero((char *)modifiers, sizeof(modifiers));
    bzero((char *)keycodes, sizeof(keycodes));
    bzero((char *)modmask, sizeof(modmask));
    for (i = minkey; i <= maxkey; i++) {
	map_sym(i, 0, 0);
	map_sym(i, 1, ShiftMask);
    }
    for (c = 0; c < 32; c++) {
	if (!keycodes[c]) {
	    i = c + '@';
	    if (i >= 'A' && i <= 'Z')
		i += 'a' - 'A';
	    keycodes[c] = keycodes[i];
	    modifiers[c] = modifiers[i] | ControlMask;
	}
    }
    mmap = XGetModifierMapping(dpy);
    j = 0;
    shift = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[shift = mmap->modifiermap[j]] = ShiftMask;
    j += mmap->max_keypermod; /* lock */
    control = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[control = mmap->modifiermap[j]] = ControlMask;
    mod1 = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[mod1 = mmap->modifiermap[j]] = Mod1Mask;
    mod2 = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[mod2 = mmap->modifiermap[j]] = Mod2Mask;
    mod3 = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[mod3 = mmap->modifiermap[j]] = Mod3Mask;
    mod4 = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[mod4 = mmap->modifiermap[j]] = Mod4Mask;
    mod5 = 0;
    for (i = 0; i < mmap->max_keypermod; i++, j++)
	if (mmap->modifiermap[j])
	    modmask[mod5 = mmap->modifiermap[j]] = Mod5Mask;
    XFreeModifiermap(mmap);
    i = XKeysymToKeycode(dpy, meta_is_alt ? XK_Alt_L : XK_Meta_L);
    if (!i)
	i = XKeysymToKeycode(dpy, meta_is_alt ? XK_Alt_R : XK_Meta_R);
    modsmask = modmask[i];
    switch (modsmask) {
    case Mod1Mask:
	meta = mod1;
	break;
    case Mod2Mask:
	meta = mod2;
	break;
    case Mod3Mask:
	meta = mod3;
	break;
    case Mod4Mask:
	meta = mod4;
	break;
    case Mod5Mask:
	meta = mod5;
	break;
    default:
	meta = 0;
	break;
    }
#ifndef MSDOS
    if (meta) {
	for (c = 0; c < 128; c++) {
	    keycodes[c + 128] = keycodes[c];
	    modifiers[c + 128] = modifiers[c] | modsmask;
	}
    }
#else
    for (i = 0, c = 128; i < sizeof(pc_keys) / sizeof(KeyBindingRec); i++) {
	pc_keys[i].c = ' ';
	if (!pc_keys[i].keysym ||
	    ((pc_keys[i].modifiers & Meta) && !meta))
	    continue;
	if (pc_keys[i].keysym == XK_Delete && !pc_keys[i].modifiers) {
	    pc_keys[i].c = '\177';
	    continue;
	} else if (pc_keys[i].keysym == XK_at &&
		   pc_keys[i].modifiers == ControlMask) {
	    pc_keys[i].c = '\0';
	    continue;
	}
	j = XKeysymToKeycode(dpy, pc_keys[i].keysym);
	if (!j)
	    continue;
	if (c > 255) {
	    fprintf(stderr, "%s: key overflow at scan code 0x%x\n",
		    progname, c);
	    break;
	}
	modifiers[c] = pc_keys[i].modifiers;
	if (XKeycodeToKeysym(dpy, j, 0) != pc_keys[i].keysym &&
	    XKeycodeToKeysym(dpy, j, 1) == pc_keys[i].keysym) {
	    if (modifiers[c] & ShiftMask)
		continue;
	    modifiers[c] |= ShiftMask;
	}
	pc_keys[i].c = c;
	keycodes[c] = j;
	if (modifiers[c] & Meta)
	    modifiers[c] = (modifiers[c] & ~Meta) | modsmask;
	c++;
    }
#endif
    if (bs_is_del) {
	keycodes['\b'] = keycodes['\177'];
	modifiers['\b'] = modifiers['\177'];
    } else if (pc_bs == '\177') {
	c = keycodes['\b'];
	keycodes['\b'] = keycodes['\177'];
	keycodes['\177'] = c;
	modsmask = modifiers['\b'];
	modifiers['\b'] = modifiers['\177'];
	modifiers['\177'] = modsmask;
    }
    if (bs_is_backspace)
	keycodes[pc_bs] = parse_keysym("BackSpace", 9, &modifiers[pc_bs]);
    last_sym = 0;
    if (hotkeyname) {
	hotkey = parse_keysym(hotkeyname, strlen(hotkeyname), &modsmask);
	hotwin = None;
	if (hotkey) {
	    XQueryTree(dpy, DefaultRootWindow(dpy), &w, &w,
		       &children, &nchild);
	    for (i = 0; !hotwin && i < nchild; i++) {
		w = XmuClientWindow(dpy, children[i]);
		if (!XFetchName(dpy, w, &name))
		    continue;
		if (!strcmp(name, hotwinname))
		    hotwin = w;
		XFree(name);
	    }
	    XFree((char *)children);
	}
	if (hotwin) {
	    XGrabKey(dpy, hotkey, modsmask, DefaultRootWindow(dpy), False,
		     GrabModeAsync, GrabModeAsync);
	    if (hotwinfocus) {
		XEvent ev;
		ev.xfocus.type = FocusIn;
		ev.xfocus.serial = 0;
		ev.xfocus.window = hotwin;
		ev.xfocus.mode = NotifyNormal;
		ev.xfocus.detail = NotifyAncestor;
		XSendEvent(dpy, hotwin, False, FocusChangeMask, &ev);
	    }
	    if (hotwingeom &&
		XGetGeometry(dpy, w, &root, &x, &y, &width, &height,
			     &bwidth, &depth) &&
		XGetWMNormalHints(dpy, w, &hints, &mask)) {
		XWMGeometry(dpy, DefaultScreen(dpy), hotwingeom,
			    (char *)NULL, bwidth, &hints, &x, &y,
			    (int *)&width, (int *)&height, &j);
		XMoveResizeWindow(dpy, w, x, y, width, height);
	    }
	    hotwingeom = NULL;
	}
    }
    last_sym = 0;
}

void
reflect_modifiers(mods)
    unsigned int mods;
{
    unsigned int downmods = mods & ~curmods;
    unsigned int upmods = curmods & ~mods;

    if (upmods) {
	if (upmods & ShiftMask)
	    (*generate_key)(shift, False);
	if (upmods & ControlMask)
	    (*generate_key)(control, False);
	if (upmods & Mod1Mask)
	    (*generate_key)(mod1, False);
	if (upmods & Mod2Mask)
	    (*generate_key)(mod2, False);
	if (upmods & Mod3Mask)
	    (*generate_key)(mod3, False);
	if (upmods & Mod4Mask)
	    (*generate_key)(mod4, False);
	if (upmods & Mod5Mask)
	    (*generate_key)(mod5, False);
    }
    if (downmods) {
	if (downmods & ShiftMask)
	    (*generate_key)(shift, True);
	if (downmods & ControlMask)
	    (*generate_key)(control, True);
	if (downmods & Mod1Mask)
	    (*generate_key)(mod1, True);
	if (downmods & Mod2Mask)
	    (*generate_key)(mod2, True);
	if (downmods & Mod3Mask)
	    (*generate_key)(mod3, True);
	if (downmods & Mod4Mask)
	    (*generate_key)(mod4, True);
	if (downmods & Mod5Mask)
	    (*generate_key)(mod5, True);
    }
    curmods = mods;
}

void
do_key(key, mods)
    int key;
    unsigned short mods;
{

    if (!key)
	return;
    if (modmask[key]) {
	tempmods |= modmask[key];
	return;
    }
    reflect_modifiers(tempmods | mods);
    (*generate_key)(key, True);
    (*generate_key)(key, False);
    moving_timeout = NULL;
    last_keycode = key;
    last_mods = tempmods | mods;
    tempmods = 0;
}

void
do_char(c)
    unsigned char c;
{
    do_key(keycodes[c], modifiers[c]);
}

KeyCode
parse_keysym(buf, len, modsmask)
    char *buf;
    int len;
    unsigned short *modsmask;
{
    KeySym sym;
    char *endptr;

    if ((*buf == 'F' && len <= 3) ||
	!(sym = Strtol(buf, &endptr, 16)) || *endptr)
	sym = XStringToKeysym(buf);
    if (!sym)
	return 0;
    if (sym != last_sym) {
	last_sym = sym;
	last_keycode_for_sym = XKeysymToKeycode(dpy, sym);
    }
    if (XKeycodeToKeysym(dpy, last_keycode_for_sym, 0) != sym &&
	XKeycodeToKeysym(dpy, last_keycode_for_sym, 1) == sym)
	*modsmask = ShiftMask;
    else
	*modsmask = None;
    return last_keycode_for_sym;
}

void
do_keysym(buf, len)
    char *buf;
    int len;
{
    KeyCode key;
    unsigned short modsmask;

    key = parse_keysym(buf, len, &modsmask);
    if (key)
	do_key(key, modsmask);
    else
	XBell(dpy, 0);
}

void
do_button(button)
    int button;
{
    if (!button) {
	for (button = 1; button < 256; button++) {
	    if (button_state[button])
		do_button(button);
	}
	return;
    }
    if (button < 1 || button > 255)
	return;
    button = button_map[button];
    if (button) {
	reflect_modifiers(tempmods);
	button_state[button] = !button_state[button];
	(*generate_button)(button, button_state[button]);
	tempmods = 0;
    }
}

void
do_autorepeat(buf)
    char *buf;
{
    double delay, mdelay;
    int delta;
    char *endptr;

    delay = Strtod(buf, &endptr);
    if (*endptr) {
	delta = Strtol(endptr + 1, &endptr, 10);
	if (*endptr) {
	    mdelay = atof(endptr + 1);
	    if (!last_keycode) {
		delay = mdelay;
		if (delta) {
		    if (moving_x < 0)
			moving_x = -delta;
		    else if (moving_x > 0)
			moving_x = delta;
			if (moving_y < 0)
			moving_y = -delta;
		    else if (moving_y > 0)
			moving_y = delta;
		}
	    }
	    if (delay) {
		timeout.tv_sec = delay;
		timeout.tv_usec = (delay - timeout.tv_sec) * 1000000;
		moving_timeout = &timeout;
	    }
	}
    }
}

void
do_motion(buf)
    char *buf;
{
    int dx, dy;
    char *endptr;

    dx = Strtol(buf, &endptr, 10);
    if (*endptr) {
	dy = atoi(endptr + 1);
	if (!moving_timeout)
	    (*generate_motion)(dx, dy);
	else if (last_keycode)
	    moving_timeout = NULL;
	moving_x = dx;
	moving_y = dy;
	last_keycode = 0;
    }
}

void
do_warp(buf)
    char *buf;
{
    int screen;
    int x, y;
    char *endptr = NULL, *endptr2;
    Window root, w, cw;
    int rx, ry, wx, wy;
    unsigned int width, height, bwidth, depth, m;
    Bool negx, negy;

    switch (*buf) {
    case 'S':
	screen = -1;
	break;
    case 'W':
	screen = -2;
	break;
    case 'w':
	screen = -3;
	break;
    default:
	screen = Strtol(buf, &endptr, 10);
    }
    if (!endptr) {
	if (buf[1] != ' ')
	    return;
	endptr = buf + 1;
    }
    if (*endptr) {
	x = Strtol(endptr + 1, &endptr2, 10);
	if (*endptr2) {
	    y = atoi(endptr2 + 1);
	    negx = x < 0 || (x == 0 && endptr[1] == '-');
	    negy = y < 0 || (y == 0 && endptr2[1] == '-');
	    if (screen < 0 && (screen < -1 || negx || negy)) {
		if (screen < 0)
		    root = DefaultRootWindow(dpy);
		else
		    root = RootWindow(dpy, screen);
		XQueryPointer(dpy, root, &root, &w, &rx, &ry, &wx, &wy, &m);
		switch (screen) {
		case -2:
		    if (w)
			w = XmuClientWindow(dpy, w);
		    else
			w = root;
		    break;
		case -3:
		    cw = w;
		    while (cw) {
			w = cw;
			XQueryPointer(dpy, w, &root, &cw,
				      &rx, &ry, &wx, &wy, &m);
		    }
		}
		if (screen < -1 && w != root) {
		    if (negx || negy) {
			XGetGeometry(dpy, w, &root, &wx, &wy, &width, &height,
				     &bwidth, &depth);
			if (negx)
			    x += (int)width;
			if (negy)
			    y += (int)height;
		    }
		    XTranslateCoordinates(dpy, w, root, x, y, &x, &y, &w);
		    negx = False;
		    negy = False;
		}
		for (screen = 0; RootWindow(dpy, screen) != root; screen++)
		    ;
	    }
	    if (negx)
		x += DisplayWidth(dpy, screen);
	    if (negy)
		y += DisplayHeight(dpy, screen);
	    (*generate_warp)(screen, x, y);
	}
    }
}

void
get_region(reg, w, wa, getshape)
    Region reg;
    Window w;
    XWindowAttributes *wa;
    Bool getshape;
{
    XRectangle rect;
    XRectangle *rects;
    int n;
    int order;

    if (getshape &&
	(rects = XShapeGetRectangles(dpy, w, ShapeBounding, &n, &order))) {
	while (--n >= 0) {
	    rects[n].x += wa->x + wa->border_width;
	    rects[n].y += wa->y + wa->border_width;
	    XUnionRectWithRegion(&rects[n], reg, reg);
	}
	XFree((char *)rects);
    } else {
	rect.x = wa->x;
	rect.y = wa->y;
	rect.width = wa->width + (2 * wa->border_width);
	rect.height = wa->height + (2 * wa->border_width);
	XUnionRectWithRegion(&rect, reg, reg);
    }
}

Region
compute_univ(puniv, iuniv, w, wa, level)
    Region puniv;
    Region iuniv;
    Window w;
    XWindowAttributes *wa;
    int level;
{
    Region univ;

    univ = XCreateRegion();
    XIntersectRegion(iuniv, univ, iuniv);
    get_region(iuniv, w, wa, !level);
    if (!level && jump.overlap) {
	XIntersectRegion(iuniv, jump.overlap, univ);
	if (!XEmptyRegion(univ)) {
	    XDestroyRegion(univ);
	    return NULL;
	}
    }
    XIntersectRegion(puniv, iuniv, univ);
    if (XEmptyRegion(univ)) {
	XDestroyRegion(univ);
	return NULL;
    }
    XSubtractRegion(iuniv, univ, iuniv);
    XSubtractRegion(puniv, univ, puniv);
    return univ;
}

void
compute_box(univ, box)
    Region univ;
    Box *box;
{
    XRectangle rect;

    XClipBox(univ, &rect);
    box->x1 = rect.x;
    box->y1 = rect.y;
    box->x2 = rect.x + (int)rect.width - 1;
    box->y2 = rect.y + (int)rect.height - 1;
}

Bool
box_left(univ, iuniv)
    Region univ;
    Region iuniv;
{
    XRectangle rect;

    XUnionRegion(iuniv, univ, iuniv);
    XClipBox(iuniv, &rect);
    return (XRectInRegion(iuniv, rect.x, rect.y, rect.width, rect.height) ==
	    RectangleIn);
}

void
find_closest_point(univ, px, py)
    Region univ;
    int *px;
    int *py;
{
    int x, y;
    int max, i;
    Box box;

    x = *px;
    y = *py;
    if (XPointInRegion(univ, x, y) || XEmptyRegion(univ))
	return;
    compute_box(univ, &box);
    if (box.x2 < x)
	max = x - box.x2;
    else if (box.x1 > x)
	max = box.x1 - x;
    else
	max = 0;
    if (box.y2 < y)
	i = y - box.y2;
    else if (box.y1 > y)
	i = box.y1 - y;
    else
	i = 0;
    if (i > max)
	max = i;
    for (; 1; max++) {
	for (i = 0; i <= max; i++) {
	    if (XPointInRegion(univ, x - max, y - i)) {
		*px -= max;
		*py -= i;
		return;
	    }
	    if (XPointInRegion(univ, x - max, y + i)) {
		*px -= max;
		*py += i;
		return;
	    }
	    if (XPointInRegion(univ, x + max, y - i)) {
		*px += max;
		*py -= i;
		return;
	    }
	    if (XPointInRegion(univ, x + max, y + i)) {
		*px += max;
		*py += i;
		return;
	    }
	    if (XPointInRegion(univ, x - i, y - max)) {
		*px -= i;
		*py -= max;
		return;
	    }
	    if (XPointInRegion(univ, x - i, y + max)) {
		*px -= i;
		*py += max;
		return;
	    }
	    if (XPointInRegion(univ, x + i, y - max)) {
		*px += i;
		*py -= max;
		return;
	    }
	    if (XPointInRegion(univ, x + i, y + max)) {
		*px += i;
		*py += max;
		return;
	    }
	}
    }
}


void
compute_point(univ, wa)
    Region univ;
    XWindowAttributes *wa;
{
    jump.bestx = wa->x + wa->width / 2 + wa->border_width;
    jump.besty = wa->y + wa->height / 2 + wa->border_width;
    find_closest_point(univ, &jump.bestx, &jump.besty);
}

Region
destroy_region(univ)
    Region univ;
{
    if (univ)
	XDestroyRegion(univ);
    return NULL;
}

Bool
match_class(w, rec, prop)
    Window w;
    MatchRec *rec;
    Atom prop;
{
    Atom type;
    int format;
    int i;
    unsigned long len;
    unsigned long left;
    char *data;
    Bool ok;

    ok = False;
    if (XGetWindowProperty(dpy, w, prop, 0L, 10000L, False,
			   XA_STRING, &type, &format, &len, &left,
			   (unsigned char **)&data) == Success) {
	if (format == 8) {
	    i = strlen(data) + 1;
	    if (i > len)
		i = len;
	    if (rec->match == MatchClass) {
		if ((!rec->name[0] || !strcmp(rec->name, data)) &&
		    (!rec->class[0] || !strcmp(rec->class, data + i)))
		    ok = True;
	    } else {
		if ((!rec->name[0] ||
		     !strncmp(rec->name, data, rec->namelen)) &&
		    (!rec->class[0] ||
		     !strncmp(rec->class, data + i, rec->classlen)))
		    ok = True;
	    }
	}
	XFree(data);
    }
    return ok;
}

Bool
matches(w, rec, getcw)
    Window w;
    MatchRec *rec;
    Bool getcw;
{
    Bool ok;
    char *name;

    switch (rec->match) {
    case MatchName:
    case MatchNamePrefix:
	if (getcw)
	    w = XmuClientWindow(dpy, w);
	if (!XFetchName(dpy, w, &name))
	    return False;
	if (rec->match == MatchName)
	    ok = !strcmp(rec->name, name);
	else
	    ok = !strncmp(rec->name, name, rec->namelen);
	XFree(name);
	break;
    case MatchClass:
    case MatchClassPrefix:
	if (getcw)
	    w = XmuClientWindow(dpy, w);
	ok = ((getcw && match_class(w, rec, XA_WM_CLASS)) ||
	      match_class(w, rec, MIT_OBJ_CLASS) ||
	      (!getcw && match_class(w, rec, XA_WM_CLASS)));
	break;
    default:
	ok = True;
	break;
    }
    return ok;
}

double
compute_best_right(univ, cx, cy)
    Region univ;
    int cx;
    int cy;
{
    int maxy, maxx, x, y, Y, Y2, i;

    cx -= jump.rootx;
    cy -= jump.rooty;
    if (cy < 0)
	cy = -cy;
    maxy = HeightOfScreen(jump.screen) - jump.rooty;
    if (jump.rooty > maxy)
	maxy = jump.rooty;
    i = sqrt(jump.best_dist / jump.mult);
    if (i < maxy)
	maxy = i;
    maxx = WidthOfScreen(jump.screen) - jump.rootx;
    i = sqrt(jump.best_dist);
    if (i < maxx)
	maxx = i;
    if (XRectInRegion(univ, jump.rootx, jump.rooty - maxy,
		      maxx + 1, maxy + maxy + 1) == RectangleOut)
	return -1;
    for (Y = cy; Y <= maxy; Y++) {
	Y2 = Y * Y;
	for (y = cy; y <= Y; y++) {
	    x = sqrt(jump.mult * (Y2 - y*y));
	    if (x < cx)
		break;
	    if (XRectInRegion(univ, jump.rootx, jump.rooty - y,
			      x + 1, y + y + 1) == RectangleOut)
		continue;
	    for (i = 0; i <= x; i++) {
		if (!XPointInRegion(univ, jump.rootx + i, jump.rooty - y) &&
		    !XPointInRegion(univ, jump.rootx + i, jump.rooty + y))
		    continue;
		return i * i + jump.mult * y * y;
	    }
	}
    }
    return -1;
}

double
compute_best_left(univ, cx, cy)
    Region univ;
    int cx;
    int cy;
{
    int maxy, maxx, x, y, Y, Y2, i;

    cx = jump.rootx - cx;
    cy -= jump.rooty;
    if (cy < 0)
	cy = -cy;
    maxy = HeightOfScreen(jump.screen) - jump.rooty;
    if (jump.rooty > maxy)
	maxy = jump.rooty;
    i = sqrt(jump.best_dist / jump.mult);
    if (i < maxy)
	maxy = i;
    maxx = jump.rootx;
    i = sqrt(jump.best_dist);
    if (i < maxx)
	maxx = i;
    if (XRectInRegion(univ, jump.rootx - maxx, jump.rooty - maxy,
		      maxx + 1, maxy + maxy + 1) == RectangleOut)
	return -1;
    for (Y = cy; Y <= maxy; Y++) {
	Y2 = Y * Y;
	for (y = cy; y <= Y; y++) {
	    x = sqrt(jump.mult * (Y2 - y*y));
	    if (x < cx)
		break;
	    if (XRectInRegion(univ, jump.rootx - x, jump.rooty - y,
			      x + 1, y + y + 1) == RectangleOut)
		continue;
	    for (i = 0; i <= x; i++) {
		if (!XPointInRegion(univ, jump.rootx - i, jump.rooty - y) &&
		    !XPointInRegion(univ, jump.rootx - i, jump.rooty + y))
		    continue;
		return i * i + jump.mult * y * y;
	    }
	}
    }
    return -1;
}

double
compute_best_up(univ, cx, cy)
    Region univ;
    int cx;
    int cy;
{
    int maxy, maxx, x, y, X, X2, i;

    cx -= jump.rootx;
    if (cx < 0)
	cx = -cx;
    cy = jump.rooty - cy;
    maxx = WidthOfScreen(jump.screen) - jump.rootx;
    if (jump.rootx > maxx)
	maxx = jump.rootx;
    i = sqrt(jump.best_dist / jump.mult);
    if (i < maxx)
	maxx = i;
    maxy = jump.rooty;
    i = sqrt(jump.best_dist);
    if (i < maxy)
	maxy = i;
    if (XRectInRegion(univ, jump.rootx - maxx, jump.rooty - maxy,
		      maxx + maxx + 1, maxy + 1) == RectangleOut)
	return -1;
    for (X = cx; X <= maxx; X++) {
	X2 = X * X;
	for (x = cx; x <= X; x++) {
	    y = sqrt(jump.mult * (X2 - x*x));
	    if (y < cy)
		break;
	    if (XRectInRegion(univ, jump.rootx - x, jump.rooty - y,
			      x + x + 1, y + 1) == RectangleOut)
		continue;
	    for (i = 0; i <= y; i++) {
		if (!XPointInRegion(univ, jump.rootx - x, jump.rooty - i) &&
		    !XPointInRegion(univ, jump.rootx + x, jump.rooty - i))
		    continue;
		return jump.mult * x * x + i * i;
	    }
	}
    }
    return -1;
}

double
compute_best_down(univ, cx, cy)
    Region univ;
    int cx;
    int cy;
{
    int maxy, maxx, x, y, X, X2, i;

    cx -= jump.rootx;
    if (cx < 0)
	cx = -cx;
    cy -= jump.rooty;
    maxx = WidthOfScreen(jump.screen) - jump.rootx;
    if (jump.rootx > maxx)
	maxx = jump.rootx;
    i = sqrt(jump.best_dist / jump.mult);
    if (i < maxx)
	maxx = i;
    maxy = HeightOfScreen(jump.screen) - jump.rooty;
    i = sqrt(jump.best_dist);
    if (i < maxy)
	maxy = i;
    if (XRectInRegion(univ, jump.rootx - maxx, jump.rooty,
		      maxx + maxx + 1, maxy + 1) == RectangleOut)
	return -1;
    for (X = cx; X <= maxx; X++) {
	X2 = X * X;
	for (x = cx; x <= X; x++) {
	    y = sqrt(jump.mult * (X2 - x*x));
	    if (y < cy)
		break;
	    if (XRectInRegion(univ, jump.rootx - x, jump.rooty,
			      x + x + 1, y + 1) == RectangleOut)
		continue;
	    for (i = 0; i <= y; i++) {
		if (!XPointInRegion(univ, jump.rootx - x, jump.rooty + i) &&
		    !XPointInRegion(univ, jump.rootx + x, jump.rooty + i))
		    continue;
		return jump.mult * x * x + i * i;
	    }
	}
    }
    return -1;
}

double
compute_best_close(univ, cx, cy)
    Region univ;
    int cx;
    int cy;
{
    find_closest_point(univ, &cx, &cy);
    return ((cx - jump.rootx) * (cx - jump.rootx) +
	    (cy - jump.rooty) * (cy - jump.rooty));
}

double
compute_distance(univ)
    Region univ;
{
    Box box;
    int x, y;

    if (XPointInRegion(univ, jump.rootx, jump.rooty))
	return -1;
    compute_box(univ, &box);
    switch (jump.dir) {
    case 'R':
    case 'L':
	if (jump.dir == 'R' && box.x1 >= jump.rootx)
	    x = box.x1;
	else if (jump.dir == 'L' && box.x2 <= jump.rootx)
	    x = box.x2;
	else
	    x = jump.rootx;
	if (box.y2 < jump.rooty)
	    y = box.y2;
	else if (box.y1 > jump.rooty)
	    y = box.y1;
	else
	    y = jump.rooty;
	if (((x - jump.rootx) * (x - jump.rootx) +
	     jump.mult * (y - jump.rooty) * (y - jump.rooty)) >=
	    jump.best_dist)
	    return -1;
	if (jump.dir == 'R')
	    return compute_best_right(univ, x, y);
	return compute_best_left(univ, x, y);
    case 'U':
    case 'D':
	if (box.x2 < jump.rootx)
	    x = box.x2;
	else if (box.x1 > jump.rootx)
	    x = box.x1;
	else
	    x = jump.rootx;
	if (jump.dir == 'U' && box.y2 <= jump.rooty)
	    y = box.y2;
	else if (jump.dir == 'D' && box.y1 > jump.rooty)
	    y = box.y1;
	else
	    y = jump.rooty;
	if ((jump.mult * (x - jump.rootx) * (x - jump.rootx) +
	     (y - jump.rooty) * (y - jump.rooty)) >= jump.best_dist)
	    return -1;
	if (jump.dir == 'U')
	    return compute_best_up(univ, x, y);
	return compute_best_down(univ, x, y);
    default:
	if (box.x2 < jump.rootx)
	    x = box.x2;
	else if (box.x1 > jump.rootx)
	    x = box.x1;
	else
	    x = jump.rootx;
	if (box.y2 < jump.rooty)
	    y = box.y2;
	else if (box.y1 > jump.rooty)
	    y = box.y1;
	else
	    y = jump.rooty;
	if (((x - jump.rootx) * (x - jump.rootx) +
	     (y - jump.rooty) * (y - jump.rooty)) >= jump.best_dist)
	    return -1;
	return compute_best_close(univ, x, y);
    }
}

Bool
find_closest(parent, pwa, puniv, level)
    Window parent;
    XWindowAttributes *pwa;
    Region puniv;
    int level;
{
    Window *children;
    unsigned int nchild;
    Window child;
    XWindowAttributes wa;
    int i;
    Bool found;
    double dist;
    Box box;
    Region iuniv, univ;

    XQueryTree(dpy, parent, &wa.root, &wa.root, &children, &nchild);
    if (!nchild)
	return False;
    found = False;
    iuniv = XCreateRegion();
    univ = NULL;
    for (i = nchild; --i >= 0; univ = destroy_region(univ)) {
	child = children[i];
	if (!XGetWindowAttributes(dpy, child, &wa))
	    continue;
	if (wa.map_state != IsViewable)
	    continue;
	wa.x += pwa->x;
	wa.y += pwa->y;
	univ = compute_univ(puniv, iuniv, child, &wa, level);
	if (!univ)
	    continue;
	compute_box(univ, &box);
	switch (jump.dir) {
	case 'U':
	    if (box.y1 >= jump.rooty)
		continue;
	    break;
	case 'D':
	    if (box.y2 <= jump.rooty)
		continue;
	    break;
	case 'R':
	    if (box.x2 <= jump.rootx)
		continue;
	    break;
	case 'L':
	    if (box.x1 >= jump.rootx)
		continue;
	    break;
	}
	if (jump.recurse &&
	    find_closest(child, &wa, univ, level + 1))
	    found = True;
	if (jump.input && !(wa.all_event_masks & jump.input))
	    continue;
	if (XEmptyRegion(univ))
	    continue;
	if (jump.recurse && !box_left(univ, iuniv))
	    continue;
	if (!matches(child, &jump.match, !level))
	    continue;
	dist = compute_distance(univ);
	if (dist < 0 || dist >= jump.best_dist)
	    continue;
	jump.best = children[i];
	jump.best_dist = dist;
	compute_point(univ, &wa);
	found = True;
    }
    if (children)
	XFree((char *)children);
    return found;
}

char *
parse_class(buf, rec)
    char *buf;
    MatchRec *rec;
{
    char *cptr;

    cptr = rindex(buf, '.');
    if (cptr) {
	bcopy(buf, rec->name, cptr - buf);
	rec->name[cptr - buf + 1] = '\0';
	strcpy(rec->class, cptr + 1);
    } else {
	strcpy(rec->name, buf);
	rec->class[0] = '\0';
    }
    rec->namelen = strlen(rec->name);
    rec->classlen = strlen(rec->class);
    buf += strlen(buf) - 1;
    return buf;
}

char *
parse_name(buf, rec)
    char *buf;
    MatchRec *rec;
{
    strcpy(rec->name, buf);
    rec->namelen = strlen(rec->name);
    buf += strlen(buf) - 1;
    return buf;
}

void
do_jump(buf)
    char *buf;
{
    Window root, child;
    XWindowAttributes wa;
    int screen;
    Region univ;
    XRectangle rect;
    char *endptr;
    Bool overlap;
    unsigned int mask;

    jump.dir = 0;
    jump.mult = 10.0;
    jump.recurse = False;
    jump.input = 0;
    jump.best_dist = 4e9;
    jump.match.match = MatchNone;
    jump.overlap = NULL;
    overlap = False;
    for (; *buf; buf++) {
	switch (*buf) {
	case 'C':
	case 'D':
	case 'U':
	case 'L':
	case 'R':
	    jump.dir = *buf;
	    jump.recurse = False;
	    break;
	case 'c':
	case 'd':
	case 'u':
	case 'l':
	case 'r':
	    jump.dir = *buf - 'a' + 'A';
	    jump.recurse = True;
	    break;
	case 'k':
	    jump.input |= KeyPressMask|KeyReleaseMask;
	    break;
	case 'b':
	    jump.input |= ButtonPressMask|ButtonReleaseMask;
	    break;
	case 'N':
	case 'P':
	    if (*buf == 'N')
		jump.match.match = MatchClass;
	    else
		jump.match.match = MatchClassPrefix;
	    buf = parse_class(buf + 1, &jump.match);
	    break;
	case 'n':
	case 'p':
	    if (*buf == 'n')
		jump.match.match = MatchName;
	    else
		jump.match.match = MatchNamePrefix;
	    buf = parse_name(buf + 1, &jump.match);
	    break;
	case 'O':
	    overlap = True;
	    break;
	case ' ':
	    jump.mult = Strtod(buf+1, &endptr);
	    if (*endptr)
		return;
	    buf = endptr - 1;
	    break;
	case 'Z':
	    break;
	default:
	    return;
	}
    }
    root = DefaultRootWindow(dpy);
    while (1) {
	for (screen = 0; RootWindow(dpy, screen) != root; screen++)
	    ;
	if (XQueryPointer(dpy, root, &root, &child,
			  &jump.rootx, &jump.rooty,
			  &jump.bestx, &jump.besty, &mask))
	    break;
    }
    jump.screen = ScreenOfDisplay(dpy, screen);
    if (child && (jump.recurse || overlap))
	XGetWindowAttributes(dpy, child, &wa);
    if (jump.recurse && child) {
	root = child;
    } else {
	if (overlap && child) {
	    jump.overlap = XCreateRegion();
	    get_region(jump.overlap, child, &wa, True);
	}
	wa.x = 0;
	wa.y = 0;
	wa.width = WidthOfScreen(jump.screen);
	wa.height = HeightOfScreen(jump.screen);
    }
    univ = XCreateRegion();
    rect.x = wa.x;
    rect.y = wa.y;
    rect.width = wa.width;
    rect.height = wa.height;
    XUnionRectWithRegion(&rect, univ, univ);
    if (find_closest(root, &wa, univ, 0))
	(*generate_warp)(screen, jump.bestx, jump.besty);
    else
	XBell(dpy, 0);
    if (jump.overlap)
	XDestroyRegion(jump.overlap);
    XDestroyRegion(univ);
}

void
unset_trigger()
{
    switch (trigger.type) {
    case MapNotify:
    case UnmapNotify:
	XSelectInput(dpy, trigger.root, 0L);
	break;
    }
    if (trigger.windows) {
	XFree((char *)trigger.windows);
	trigger.windows = NULL;
	trigger.count = 0;
    }
    trigger.type = 0;
}

void
set_unmap_trigger()
{
    Window w, child;
    Window *children;
    unsigned int nchild;
    int i;
    int j;

    XSelectInput(dpy, trigger.root, SubstructureNotifyMask);
    XQueryTree(dpy, trigger.root, &w, &child, &children, &nchild);
    if (!nchild) {
	unset_trigger();
	return;
    }
    for (i = nchild, j = 0; --i >= 0; ) {
	w = children[i];
	if (matches(w, &trigger.match, True))
	    children[j++] = w;
    }
    trigger.windows = children;
    trigger.count = j;
}

void
set_trigger()
{
    switch (trigger.type) {
    case SelectionClear:
	trigger.selection = XInternAtom(dpy, trigger.match.name, False);
	if (!my_window)
	    my_window = XCreateSimpleWindow(dpy, trigger.root, 0, 0, 1, 1,
					    0, 0, 0);
	XSetSelectionOwner(dpy, trigger.selection, my_window, CurrentTime);
	break;
    case UnmapNotify:
	set_unmap_trigger();
	break;
    case MapNotify:
	XSelectInput(dpy, trigger.root, SubstructureNotifyMask);
	break;
    }
}

void
do_hotkey(ev)
    XEvent *ev;
{
    ev->xkey.window = hotwin;
    XSendEvent(dpy, hotwin, False, KeyPressMask, ev);
}

void
process_events()
{
    int i;
    int j;
    XEvent ev, sev;

    for (i = XEventsQueued(dpy, QueuedAfterReading); --i >= 0; ) {
	XNextEvent(dpy, &ev);
	switch (ev.type) {
	case KeyPress:
	    if (ev.xkey.keycode == hotkey)
		do_hotkey(&ev);
	    break;
	case MappingNotify:
	    XRefreshKeyboardMapping(&ev.xmapping);
	    reset_mapping();
	    break;
	case MapNotify:
	    if (trigger.type == MapNotify &&
		ev.xmap.serial >= trigger.serial &&
		ev.xmap.event == trigger.root &&
		matches(ev.xmap.window, &trigger.match, True))
		unset_trigger();
	    break;
	case UnmapNotify:
	    if (trigger.type == UnmapNotify &&
		ev.xunmap.serial >= trigger.serial &&
		ev.xunmap.event == trigger.root) {
		for (j = 0; j < trigger.count; j++) {
		    if (trigger.windows[j] == ev.xunmap.window) {
			unset_trigger();
			break;
		    }
		}
	    }
	    break;
	case SelectionClear:
	    if (trigger.type == SelectionClear &&
		ev.xselectionclear.serial >= trigger.serial &&
		ev.xselectionclear.selection == trigger.selection)
		unset_trigger();
	    break;
	case SelectionRequest:
	    sev.type = SelectionNotify;
	    sev.xselection.requestor = ev.xselectionrequest.requestor;
	    sev.xselection.selection = ev.xselectionrequest.selection;
	    sev.xselection.target = ev.xselectionrequest.target;
	    sev.xselection.property = None;
	    sev.xselection.time = ev.xselectionrequest.time;
	    XSendEvent(dpy, sev.xselection.requestor, False, None, &sev);
	    break;
	}
    }
}

Bool
flush_and_read()
{
    int pending;

    if (flush_generate)
	(*flush_generate)();
    if (pending = XPending(dpy))
	process_events();
    if (need_bell) {
	need_bell = False;
	XBell(dpy, 0);
	XFlush(dpy);
    }
    return pending != 0;
}

void
do_trigger(buf)
    char *buf;
{
    char *endptr;
    double delay;
    MatchType match;
    Bool wait;
    int type;
    Window child;
    int x, y;

    type = 0;
    match = MatchNone;
    wait = False;
    trigger.time.tv_sec = 10;
    trigger.time.tv_usec = 0;
    for (; *buf; buf++) {
	switch (*buf) {
	case 'M':
	    type = MapNotify;
	    break;
	case 'U':
	    type = UnmapNotify;
	    break;
	case 'W':
	    wait = True;
	    break;
	case 'S':
	    type = SelectionClear;
	    match = MatchName;
	    buf = parse_name(buf + 1, &trigger.match);
	    break;
	case 'N':
	case 'P':
	    if (*buf == 'n')
		match = MatchClass;
	    else
		match = MatchClassPrefix;
	    buf = parse_class(buf + 1, &trigger.match);
	    break;
	case 'n':
	case 'p':
	    if (*buf == 'n')
		match = MatchName;
	    else
		match = MatchNamePrefix;
	    buf = parse_name(buf + 1, &trigger.match);
	    break;
	case ' ':
	    delay = Strtod(buf+1, &endptr);
	    if (*endptr)
		return;
	    trigger.time.tv_sec = delay;
	    trigger.time.tv_usec = (delay - trigger.time.tv_sec) * 1000000;
	    buf = endptr - 1;
	    break;
	default:
	    return;
	}
    }
    if (type) {
	if (trigger.type)
	    unset_trigger();
	trigger.type = type;
	trigger.match.match = match;
	XQueryPointer(dpy, DefaultRootWindow(dpy), &trigger.root, &child,
		      &x, &y, &x, &y, (unsigned int *)&x);
	trigger.serial = NextRequest(dpy);
	set_trigger();
    }
    if (!wait || !trigger.type)
	return;
    while (trigger.type) {
	if (flush_and_read())
	    continue;
	FD_CLR(0, &fdmask);
	FD_SET(Xfd, &fdmask);
	type = select(maxfd, &fdmask, NULL, NULL, &trigger.time);
	if (type < 0)
	    quit(type);
	if (!type)
	    unset_trigger();
	else
	    process_events();
    }
}

void
trim_history()
{
    if (history_end < (sizeof(history)/2))
	history_end = 0;
    else {
	bcopy(history + (sizeof(history)/2), history, history_end);
	history_end -= sizeof(history)/2;
	if (macro_start >= 0) {
	    macro_start -= sizeof(history)/2;
	    if (macro_start < 0)
		fprintf(stderr, "%s: macro definition overflowed\n", progname);
	}
    }
    bzero(history + history_end, sizeof(history) - history_end);
}

void
save_control(buf, i, j)
    char *buf;
    int i, j;
{
    if (buf[j] == control_char)
	buf[j] = control_end;
    j = j - i + 1;
    if (history_end + j > sizeof(history))
	trim_history();
    bcopy(buf + i, history + history_end, j);
    history_end += j;
}

Bool
has_bs(c)
    char c;
{
    return (((c != control_end) && (!iscntrl(c) || (c == '\r'))) ||
	    (!oldDD && ((c == '\024') || (c == '\025') ||
			(c == '\224') || (c == '\225'))));
}

void
undo_stroke()
{
    char c;
    int i;
    KeyCode key;
    unsigned short modsmask;

    if (!history_end) {
	in_control_seq = False;
	for (; curbscount; curbscount--)
	    do_char(pc_bs);
	return;
    }
    c = history[history_end-1];
    if (!in_control_seq &&
	((c == control_end) ||
	 ((history_end > 1) &&
	  (history[history_end-2] == control_char)))) {
	in_control_seq = True;
    }
    if (!in_control_seq) {
	if (c == control_end)
	    in_control_seq = True;
	else if ((history_end > 1) &&
		 (history[history_end-2] == control_char)) {
	    in_control_seq = True;
	    switch (c) {
	    case '\003': /* control c */
		if (control)
		    tempmods &= ~modmask[control];
		break;
	    case '\015': /* control m */
		if (meta)
		    tempmods &= ~modmask[meta];
		break;
	    case '\023': /* control s */
		if (shift)
		    tempmods &= ~modmask[shift];
		break;
	    }
	}
    }
    if (has_bs(c)) {
	curbscount--;
	if (!in_control_seq) {
	    if ((history_end < 3) ||
		(history[history_end-2] != '\015' &&
		 history[history_end-2] != '\003') ||
		(history[history_end-3] != control_char))
		do_char(pc_bs);
	}
    }
    history_end--;
    if (in_control_seq) {
	for (i = history_end - 1; i >= 0; i--) {
	    c = history[i];
	    if (c == control_char) {
		if (tempmods && !iscntrl(history[i+1])) {
		    history[history_end] = '\0';
		    key = parse_keysym(history+i+1, history_end - i - 1,
				       &modsmask);
		    if (key)
			tempmods &= ~modmask[key];
		    history[history_end] = control_end;
		}
		history_end = i;
		in_control_seq = False;
		break;
	    } else if (has_bs(c))
		break;
	}
    }
}

void
undo_curbsmatch()
{
    if (curbsmatch) {
	history_end -= curbsmatch->seq_len;
	curbscount -= curbsmatch->bscount;
	process(curbsmatch->undo, curbsmatch->undo_len, 0);
	curbsmatch = NULL;
    }
}

void
do_backspace()
{
    UndoRec *u;
    Bool partial = False;

    curbscount++;
    while (curbscount) {
	if (!in_control_seq && history_end) {
	    for (u = undos[((unsigned char *)history)[history_end-1]];
		 u;
		 u = u->next) {
		if (history_end >= u->seq_len &&
		    !bcmp(history+history_end-u->seq_len, u->seq,
			  u->seq_len)) {
		    if (curbscount < u->bscount)
			partial = True;
		    else if (!curbsmatch || curbsmatch->seq_len < u->seq_len)
			curbsmatch = u;
		}
	    }
	    if (partial)
		return;
	    if (curbsmatch) {
		undo_curbsmatch();
		return;
	    }
	}
	undo_stroke();
    }
}

char
controlify(c)
    char c;
{
    if (c == '?')
	c = '\177';
    else {
	if (c >= 'a' && c <= 'z')
	    c -= 'a' - 'A';
	c -= '@';
    }
    return c;
}

char *
parse_string(buf, ip, lenp, term)
    char *buf;
    int *ip;
    int *lenp;
    char term;
{
    int i, j;
    char c;
    char *seq;

    j = 0;
    for (i = *ip; (c = buf[i]) && (c != term); i++) {
	if (c == '^')
	    buf[j++] = controlify(buf[++i]);
	else if (c == '\\')
	    buf[j++] = buf[++i];
	else
	    buf[j++] = c;
    }
    if (c != term)
	return NULL;
    *ip = i + 1;
    *lenp = j;
    seq = malloc(j + 1);
    bcopy(buf, seq, j);
    seq[j] = '\0';
    return seq;
}

int
bscount(s, len)
    char *s;
    int len;
{
    int n = 0;
    char c;

    while (--len >= 0) {
	c = *s++;
	if (has_bs(c))
	    n++;
    }
    return n;
}

void
mark_controls(s, len)
    char *s;
    int len;
{
    Bool in_seq = False;

    for ( ; --len >= 0; s++) {
	if (*s != control_char)
	    continue;
	if (in_seq)
	    *s = control_end;
	else
	    switch (s[1]) {
	    case '\003': /* control c */
	    case '\005': /* control e */
	    case '\015': /* control m */
	    case '\020': /* control p */
	    case '\021': /* control q */
	    case '\023': /* control s */
	    case '\025': /* control u */
		break;
	    default:
		in_seq = True;
	    }
    }
}

void
free_undo(up)
    UndoRec *up;
{
    free(up->seq);
    free(up->undo);
    free((char *)up);
}

void
get_undofile()
{
    FILE *fp;
    char buf[1024];
    int i;
    UndoRec *up, **upp;
    int line;

    fp = fopen(undofile, "r");
    if (!fp)
	return;
    for (i = 0; i < UNDO_SIZE; i++) {
	while (up = undos[i]) {
	    undos[i] = up->next;
	    free_undo(up);
	}
    }
    line = 1;
    for (; fgets(buf, sizeof(buf), fp); line++) {
	if (buf[0] == '\n' || buf[0] == '!')
	    continue;
	up = (UndoRec *)malloc(sizeof(UndoRec));
	up->seq = NULL;
	up->undo = NULL;
	i = 0;
	if (!(up->seq = parse_string(buf, &i, &up->seq_len, ':')) ||
	    !(up->undo = parse_string(buf, &i, &up->undo_len, '\n'))) {
	    fprintf(stderr, "%s: bad sequence in file %s, line %d\n",
		    progname, undofile, line);
	    free_undo(up);
	    continue;
	}
	mark_controls(up->seq, up->seq_len);
	up->bscount = bscount(up->seq, up->seq_len);
	if (!up->bscount) {
	    free_undo(up);
	    fprintf(stderr, "%s: bad sequence, no backspaces are generated, in file %s, line %d\n", progname, undofile, line);
	    continue;
	}
	i = ((unsigned char *)up->seq)[up->seq_len - 1];
	for (upp = &undos[i]; *upp; upp = &(*upp)->next)
	    ;
	*upp = up;
	up->next = NULL;
    }
    fclose(fp);
}

void
debug_state()
{
    int i, max;

    fprintf(stderr, "%s history: ", progname);
    max = sizeof(history) - 1;
    while (max >= history_end && !history[max])
	max--;
    if (max > history_end + 20)
	max = history_end + 20;
    for (i = history_end - 70; i <= max; i++) {
	if (i < 0)
	    continue;
	if (i >= sizeof(history))
	    break;
	if (i == history_end)
	    fprintf(stderr, "\ndeleted: ");
	if (history[i] == '\177')
	    fprintf(stderr, "^?");
	else if (history[i] == control_end)
	    fprintf(stderr, "^%c", control_end - '\200' + '@');
	else if (iscntrl(history[i]))
	    fprintf(stderr, "^%c", history[i] + '@');
	else
	    fprintf(stderr, "%c", history[i]);
    }
    fprintf(stderr, "\n");
    in_control_seq = False;
}

Bool
init_display(dname)
    char *dname;
{
    Display *ndpy;
#if defined(XTEST) || defined(XTESTEXT1)
    int eventb, errorb;
#endif
#ifdef XTEST
    int vmajor, vminor;
#endif
#ifdef XTRAP
    XETC *ntc;
    ReqFlags requests;
#endif
#ifdef XTESTEXT1
    int majop;
#endif

    ndpy = XOpenDisplay(dname);
    if (!ndpy) {
	fprintf(stderr, "%s: unable to open display '%s'\n",
		progname, XDisplayName(dname));
	return False;
    }
#ifdef XTEST
    if (XTestQueryExtension(ndpy, &eventb, &errorb, &vmajor, &vminor)) {
#ifdef X_XTestGrabControl
	if (vmajor > 2 || (vmajor == 2 && vminor >= 2))
	    XTestGrabControl(ndpy, True);
#endif
	if (clean_up)
	    (*clean_up)();
	generate_key = xtest_generate_key;
	generate_button = xtest_generate_button;
	generate_motion = xtest_generate_motion;
	generate_warp = xtest_generate_warp;
	flush_generate = NULL;
	clean_up = NULL;
    } else
#endif
    {
#ifdef XTRAP
    if ((ntc = XECreateTC(ndpy, 0L, NULL))) {
	if (clean_up)
	    (*clean_up)();
	(void)XEStartTrapRequest(ntc);
	bzero((char *)requests, sizeof(requests));
	BitTrue(requests, X_GrabServer);
	BitTrue(requests, X_UngrabServer);
	XETrapSetRequests(ntc, True, requests);
	(void)XETrapSetGrabServer(ntc, True);
	tc = ntc;
	generate_key = xtrap_generate_key;
	generate_button = xtrap_generate_button;
	generate_motion = warp_generate_motion;
	generate_warp = xtrap_generate_warp;
	flush_generate = NULL;
	clean_up = xtrap_clean_up;
    } else
#endif
    {
#ifdef XTESTEXT1
    if (XQueryExtension(ndpy, XTestEXTENSION_NAME, &majop, &eventb, &errorb)) {
	if (clean_up)
	    (*clean_up)();
	generate_key = xtestext1_generate_key;
	generate_button = xtestext1_generate_button;
	generate_motion = warp_generate_motion;
	generate_warp = xtestext1_generate_warp;
	flush_generate = xtestext1_flush_generate;
	clean_up = NULL;
    } else
#endif
    {
    fprintf(stderr,
	    "%s: display '%s' does not support necessary protocol extension\n",
	    progname, DisplayString(ndpy));
    return False;
    }
    }
    }
    if (dpy)
	XCloseDisplay(dpy);
    dpy = ndpy;
    reset_mapping();
    MIT_OBJ_CLASS = XInternAtom(dpy, "_MIT_OBJ_CLASS", False);
    my_window = None;
    Xfd = ConnectionNumber(dpy);
    maxfd = Xfd + 1;
    return True;
}

void
do_display(buf)
    char *buf;
{
    char name[1024];

    if (*buf++ != 'D')
	return;
    if (!index(buf, ':')) {
	strcpy(name, buf);
	strcat(name, ":0");
	buf = name;
    }
    (void)init_display(buf);
}

void
do_macro(buf)
    char *buf;
{
    int n, i;
    char *macro;

    switch (*buf) {
    case 'a':
	macro_start = -1;
	break;
    case 'd':
	if (isdigit(buf[1]) && !buf[2]) {
	    n = buf[1] - '0';
	    if (macros[n].macro)
		free(macros[n].macro);
	    macros[n].len = 0;
	    macros[n].macro = NULL;
	}
	break;
    case 'e':
	if (isdigit(buf[1]) && !buf[2]) {
	    n = buf[1] - '0';
	    if (macros[n].len)
		process(macros[n].macro, macros[n].len, 0);
	    else
		XBell(dpy, 0);
	}
	break;
    case 'r':
	if (!buf[1])
	    macro_start = history_end + 4;
	break;
    case 's':
	if (isdigit(buf[1]) && !buf[2] && macro_start >= 0) {
	    n = buf[1] - '0';
	    i = history_end - macro_start;
	    macro = malloc(i);
	    if (macros[n].macro)
		free(macros[n].macro);
	    macros[n].len = i;
	    macros[n].macro = macro;
	    bcopy(history + macro_start, macro, i);
	    for (; --i >= 0; macro++)
		if (*macro == control_end)
		    *macro = control_char;
	}
	macro_start = -1;
	break;
    }
}

void
do_location(buf)
    char *buf;
{
    int n;
    Window root, w;
    unsigned int mask;
    int screen, x, y, wx, wy;
    unsigned int width, height, bwidth, depth;

    switch (*buf) {
    case 's':
    case 'S':
	if (isdigit(buf[1]) && !buf[2]) {
	    n = buf[1] - '0';
	    root = DefaultRootWindow(dpy);
	    while (1) {
		for (screen = 0; RootWindow(dpy, screen) != root; screen++)
		    ;
		if (XQueryPointer(dpy, root, &root, &locations[n].window,
				  &locations[n].x, &locations[n].y,
				  &locations[n].x, &locations[n].y, &mask))
		    break;
	    }
	    if (!locations[n].window)
		locations[n].window = root;
	    else {
		w = XmuClientWindow(dpy, locations[n].window);
		if (w)
		    locations[n].window = w;
		XTranslateCoordinates(dpy, root, locations[n].window,
				      locations[n].x, locations[n].y,
				      &locations[n].x, &locations[n].y, &w);
	    }
	    locations[n].from_right = False;
	    locations[n].from_bottom = False;
	    if (*buf == 'S' && locations[n].window != root) {
		XGetGeometry(dpy, locations[n].window, &root, &x, &y,
			     &width, &height, &bwidth, &depth);
		if (locations[n].x > (int)width/2) {
		    locations[n].x -= (int)width;
		    locations[n].from_right = True;
		}
		if (locations[n].y > (int)height/2) {
		    locations[n].y -= (int)height;
		    locations[n].from_bottom = True;
		}
	    }
	}
	break;
    case 'w':
	if (isdigit(buf[1]) && !buf[2]) {
	    n = buf[1] - '0';
	    if (!locations[n].window)
		XBell(dpy, 0);
	    else {
		x = locations[n].x;
		y = locations[n].y;
		if (locations[n].from_right || locations[n].from_bottom) {
		    XGetGeometry(dpy, locations[n].window, &root, &wx, &wy,
				 &width, &height, &bwidth, &depth);
		    if (locations[n].from_right)
			x += (int)width;
		    if (locations[n].from_bottom)
			y += (int)height;
		}
		XWarpPointer(dpy, None, locations[n].window, 0, 0, 0, 0, x, y);
	    }
	}
	break;
    }
}

void
process(buf, n, len)
    char *buf;
    int n;
    int len;
{
    int i, j;

    for (i = 0; i < n; i++) {
	if (len) {
	    if (buf[i] == pc_bs) {
		do_backspace();
		continue;
	    } else if (curbscount) {
		undo_curbsmatch();
		while (curbscount)
		    undo_stroke();
	    } else if (in_control_seq)
		skip_next_control_char = True;
	}
	if (buf[i] != control_char) {
	    if (len) {
		if (history_end == sizeof(history))
		    trim_history();
		history[history_end++] = buf[i];
	    }
	    do_char(((unsigned char *)buf)[i]);
	    continue;
	}
	if (skip_next_control_char) {
	    skip_next_control_char = False;
	    in_control_seq = False;
	    save_control(buf, i, i);
	    continue;
	}
	i++;
	for (j = i; 1; j++) {
	    if (j == n) {
		if (!len)
		    return;
		if (n == len)
		    break;
		n = ddread(buf+j, len-j);
		if (n < 0)
		    quit(n);
		n += j;
	    }
	    if (buf[j] != control_char) {
		if (j != i) {
		    if (iscntrl(buf[j]) && buf[i] != '\026') {
			if (buf[j] != pc_bs) { /* abort */
			    i = j;
			    break;
			}
			bcopy(buf + j + 1, buf + j - 1, n - j - 1);
			n -= 2;
			j -= 2;
			if (j == i) /* abort */
			    break;
		    }
		    continue;
		}
		switch (buf[i]) {
		case '\003': /* control c */
		    do_key(control, 0);
		    break;
		case '\005': /* control e */
		    quit(0);
		    break;
		case '\010': /* control h */
		    if (buf[i] != pc_bs)
			continue;
		    j = i - 1;
		    break;
		case '\015': /* control m */
		    do_key(meta, 0);
		    break;
		case '\020': /* control p */
		    debug_state();
		    break;
		case '\021': /* control q */
		    moving_timeout = NULL;
		    break;
		case '\023': /* control s */
		    do_key(shift, 0);
		    break;
		case '\025': /* control u */
		    get_undofile();
		    break;
		case '\177': /* delete */
		    if (buf[i] != pc_bs)
			continue;
		    j = i - 1;
		    break;
		default:
		    continue;
		}
		if (len && (i == j))
		    save_control(buf, i - 1, j);
		break;
	    }
	    buf[j] = '\0';
	    switch (buf[i]) {
	    case '\0': /* control t */
		do_char(control_char);
		break;
	    case '\001': /* control a */
		do_autorepeat(buf + i + 1);
		break;
	    case '\002': /* control b */
		do_button(atoi(buf + i + 1));
		break;
	    case '\004': /* control d */
		do_motion(buf + i + 1);
		break;
	    case '\006': /* control f */
		do_macro(buf + i + 1);
		break;
	    case '\012': /* control j */
	    	do_jump(buf + i + 1);
		break;
	    case '\014': /* control l */
	    	do_location(buf + i + 1);
		break;
	    case '\022': /* control r */
		do_display(buf + i + 1);
		break;
	    case '\026': /* control v */
		if (i == j)
		    write(1, "\024", 1);
		else
		    write(1, buf + i + 1, j - i);
		break;
	    case '\027': /* control w */
		do_warp(buf + i + 1);
		break;
	    case '\031': /* control y */
		do_trigger(buf + i + 1);
		break;
	    case '\032': /* control z */
		time_delay = atof(buf + i + 1) * 1000;
		break;
	    default:
		do_keysym(buf + i, j - i);
	    }
	    buf[j] = control_char;
	    if (len)
		save_control(buf, i - 1, j);
	    i = j;
	    break;
	}
    }
}

main(argc, argv)
    int argc;
    char **argv;
{
    int n, i;
#ifndef MSDOS
#ifndef X_NOT_POSIX
    struct termios term;
#else
    struct sgttyb term;
#endif
#endif
    char *dname = NULL;
    char *s;
    char buf[1024];
    char fbuf[1024];
#ifdef MSDOS
    static struct timeval notime = {0, 0};
#endif

    progname = *argv;
    if (s = rindex(progname, '/'))
	progname = s + 1;
    FD_ZERO(&fdmask);
    for (argc--, argv++; argc > 0; argc--, argv++) {
	if (!strcmp(*argv, "-a")) {
	    meta_is_alt = True;
	} else if (!strcmp(*argv, "-b")) {
	    bs_is_del = False;
	    bs_is_backspace = False;
	} else if (!strcmp(*argv, "-bs")) {
	    bs_is_del = False;
	    bs_is_backspace = True;
	} else if (!strcmp(*argv, "-B")) {
	    pc_bs = '\177';
	} else if (!strcmp(*argv, "-c")) {
	    doclear = True;
	} else if (!strcmp(*argv, "-C")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    control_char = controlify(**argv);
	    control_end = control_char + '\200';
	} else if (!strcmp(*argv, "-d") || !strcmp(*argv, "-display")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    dname = *argv;
	} else if (!strcmp(*argv, "-e")) {
	    noecho = False;
	} else if (!strcmp(*argv, "-E")) {
	    fakeecho = True;
	} else if (!strcmp(*argv, "-f")) {
	    hotwinfocus = False;
	} else if (!strcmp(*argv, "-g")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    hotwingeom = *argv;
	} else if (!strcmp(*argv, "-h")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    hotkeyname = *argv;
	} else if (!strcmp(*argv, "-p")) {
	    do_mouse_fixup = True;
	} else if (!strcmp(*argv, "-u")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    undofile = *argv;
	} else if (!strcmp(*argv, "-U")) {
	    pc_bs = '\377';
	} else if (!strcmp(*argv, "-w")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    hotwinname = *argv;
	    break;
	} else if (!strcmp(*argv, "-v")) {
	    argc--; argv++;
	    if (!argc)
		usage();
	    oldDD = !strcmp(*argv, "1.01") || !strcmp(*argv, "1");
	    break;
	} else {
	    usage();
	}
    }
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_IGN);
#endif
#ifndef MSDOS
#ifndef X_NOT_POSIX
    if (tcgetattr(0, &term) >= 0) {
	istty = True;
	oldterm = term;
	term.c_lflag &= ~(ICANON|ISIG);
	term.c_iflag &= ~(IXOFF|IXON|ICRNL);
	if (noecho)
	    term.c_lflag &= ~ECHO;
#ifdef _POSIX_VDISABLE
	for (i = 0; i < NCCS; i++)
	    term.c_cc[i] = _POSIX_VDISABLE;
#else
	bzero((char *)term.c_cc, sizeof(term.c_cc));
#endif
	term.c_cc[VMIN] = 1;
	term.c_cc[VTIME] = 0;
	tcsetattr(0, TCSANOW, &term);
    }
#else
    if (ioctl(0, TIOCGETP, &term) >= 0) {
	istty = True;
	oldterm = term;
	term.sg_flags |= RAW;
	term.sg_flags &= ~(CBREAK|TANDEM|CRMOD|LCASE);
	if (noecho)
	    term.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETP, &term);
    }
#endif
    if (istty) {
	signal(SIGINT, catch);
	signal(SIGTERM, catch);
    }
#else
    if(isatty(0))
	istty = True;
#endif
    if (istty) {
	oldioerror = XSetIOErrorHandler(ioerror);
	olderror = XSetErrorHandler(error);
    }
    if (istty && doclear)
	write(1, "\033[H\033[2J", 7);
    if (!dname && !*(XDisplayName(dname)))
	dname = ":0";
    if (!init_display(dname))
	quit(1);
    if (!undofile) {
	if (s = getenv("HOME")) {
	    strcpy(fbuf, s);
#ifndef MSDOS
	    strcat(fbuf, "/");
#else
	    strcat(fbuf, "\\");
#endif
	} else
	    fbuf[0] = '\0';
#ifndef MSDOS
	strcat(fbuf, ".a2x");
#else
	strcat(fbuf, "a2x.un");
#endif
	undofile = fbuf;
    }
    get_undofile();
    while (1) {
	if (flush_and_read())
	    continue;
#ifndef MSDOS
	FD_SET(Xfd, &fdmask);
	FD_SET(0, &fdmask);
	i = select(maxfd, &fdmask, NULL, NULL, moving_timeout);
#else
	while (1) {
	    if (kbhit()) {
		i = 1;
		FD_SET(0, &fdmask);
		FD_CLR(Xfd, &fdmask);
		break;
	    } else {
		FD_SET(Xfd, &fdmask);
		FD_CLR(0, &fdmask);
		if (moving_timeout) {
		    i = select(maxfd, &fdmask, NULL, NULL, moving_timeout);
		    if (i || !kbhit())
			break;
		} else {
		    if (i = select(maxfd, &fdmask, NULL, NULL, &notime))
			break;
		    apiPause();
		}
	    }
	}
#endif
	if (i < 0)
	    quit(i);
	if (!i) {
	    if (moving_timeout) {
		if (last_keycode)
		    do_key(last_keycode, last_mods);
		else
		    (*generate_motion)(moving_x, moving_y);
		moving_timeout = &timeout;
		XFlush(dpy);
	    }
	    continue;
	}
	if (FD_ISSET(Xfd, &fdmask))
	    process_events();
	if (!FD_ISSET(0, &fdmask))
	    continue;
	n = ddread(buf, sizeof(buf));
	if (n <= 0)
	    quit(n);
	process(buf, n, sizeof(buf));
	reflect_modifiers(0);
    }
}
