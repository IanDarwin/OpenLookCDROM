/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/cmenu/RCS/cmcreate.c,v 2.35 1993/12/17 17:56:55 rr2b Exp $";
#endif

#include <andrewos.h>
#include <X11/Xlib.h>
#include <cmintern.h>
#include <crsrimg.h>
#include <wormimg.h>
#include <shadows.h>

char *getprofile();
int getprofileswitch();

#define FONTHEIGHT(f) (f->max_bounds.ascent + f->max_bounds.descent)

static struct cmenudata *menuDataList = NULL;

static Window GetMenuWindow(display, parent, dp)
Display *display;
Window parent;
struct cmenudata *dp;
{

    XSetWindowAttributes xswa;
    Pixmap cursorPixmap;
    Pixmap cursorMaskPixmap;
    XColor foreColor;
    XColor backColor;
    Window rootWindow = RootWindow(display, DefaultScreen(display));

    /*
      * Make the menu's cursor.
    */
    cursorPixmap = XCreateBitmapFromData(display, rootWindow, (char *) cursor_bits,
					  (int)cursor_width, (int)cursor_height);

    cursorMaskPixmap = XCreateBitmapFromData(
		display, rootWindow, (char *) cursormask_bits, (int) cursormask_width, (int) cursormask_height);
    /*
     *	Set up the Color Stuctures for the cursor. 
     */
    foreColor.pixel = BlackPixel(display, DefaultScreen(display));
    XQueryColor(display, DefaultColormap(display, DefaultScreen(display)), &foreColor);
    backColor.pixel = WhitePixel(display, DefaultScreen(display));
    XQueryColor(display, DefaultColormap(display, DefaultScreen(display)), &backColor);

    xswa.cursor = XCreatePixmapCursor(display, cursorPixmap, cursorMaskPixmap,
				       &foreColor, &backColor,
				       cursor_x_hot, cursor_y_hot);        

    XFreePixmap(display, cursorPixmap);
    XFreePixmap(display, cursorMaskPixmap);

    if (xswa.cursor == None) {
	_cmErrorCode = cmE_CREATE_CURSOR;
	return (Window) 0;
    }

    xswa.background_pixmap = None;
    xswa.override_redirect = TRUE;
    xswa.save_under = TRUE;
    xswa.event_mask = ButtonPressMask | ButtonReleaseMask |
      ExposureMask | PointerMotionHintMask;

    return XCreateWindow(display, parent, 0, 0, 1, 1, 0, 0, InputOutput,
			  CopyFromParent, CWBackPixmap | CWOverrideRedirect |
			  ((dp->useSaveUnder) ? CWSaveUnder : 0) | CWEventMask | CWCursor, &xswa);
}

static int GetGCs(dp, window, def_env, newshadows, colorDisplay)
struct cmenudata *dp;
Window window;
char *def_env;
int newshadows, colorDisplay;
{
    Display *display = dp->dpy;    
    GC tempgc;
    XGCValues gcv;
    Pixmap grayMap = None;
    Pixmap wormIcon = None;
    XWindowAttributes windowAttributes;
    int screen = DefaultScreen(display);
    Screen *s = DefaultScreenOfDisplay(display);
    static long grayImage[] = {0x55555555,
    0xAAAAAAAA,
    0x55555555,
    0xAAAAAAAA,
    0x55555555,
    0xAAAAAAAA,
    0x55555555,
    0xAAAAAAAA
    };
    unsigned long tileOrStipple = 0;
    unsigned long color;
    XColor grayColor, foreColor, backColor, topshadowColor, bottomshadowColor, exact, keysColor;
    
    foreColor.pixel=BlackPixel(display, DefaultScreen(display));
    backColor.pixel=WhitePixel(display, DefaultScreen(display));
        
    if (XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
		     dp->foregroundColor,&foreColor) == 0 ||
	 XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&foreColor) == 0) {
	foreColor.pixel = BlackPixel(display, DefaultScreen(display));
    }
    
    if (XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
		     dp->keysColor,&keysColor) == 0 ||
	 XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&keysColor) == 0) {
	keysColor.pixel = BlackPixel(display, DefaultScreen(display));
    }
    dp->keysPixel=keysColor.pixel;
    
    if (XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
		     dp->backgroundColor,&backColor) == 0 ||
	 XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&backColor) == 0) {

	if(dp->motifMenus && newshadows && colorDisplay) {
	    if (XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
			    "gray80",&backColor) == 0 ||
		XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&backColor) == 0) {
		backColor.pixel = WhitePixel(display, DefaultScreen(display));
	    }
	}
    }
    
    if (dp->motifMenus) {
	if (dp->topshadowColor==NULL || XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
			dp->topshadowColor,&topshadowColor) == 0 ||
	    XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&topshadowColor) == 0) {
	    if(newshadows) {
		topshadowColor.red = topshadowColor.green = topshadowColor.blue=65535;
		shadows_ComputeColor(backColor.red, backColor.green, backColor.blue, &topshadowColor.red, &topshadowColor.green, &topshadowColor.blue, shadows_TOPSHADOW);
		topshadowColor.flags=DoRed|DoGreen|DoBlue;
		if(XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&topshadowColor) == 0) topshadowColor.pixel = WhitePixel(display, DefaultScreen(display));
	    } else topshadowColor.pixel = WhitePixel(display, DefaultScreen(display));
	}
	if (XParseColor(display,DefaultColormap(display, DefaultScreen(display)),
			dp->bottomshadowColor,&bottomshadowColor) == 0 ||
	    XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&bottomshadowColor) == 0) {
	    if(newshadows) {
		bottomshadowColor.red = bottomshadowColor.green = bottomshadowColor.blue=0;
		shadows_ComputeColor(backColor.red, backColor.green, backColor.blue, &bottomshadowColor.red, &bottomshadowColor.green, &bottomshadowColor.blue, shadows_BOTTOMSHADOW);
		bottomshadowColor.flags=DoRed|DoGreen|DoBlue;
		if(XAllocColor(display,DefaultColormap(display, DefaultScreen(display)),&bottomshadowColor) == 0) bottomshadowColor.pixel = BlackPixel(display, DefaultScreen(display));
	    } else bottomshadowColor.pixel = BlackPixel(display, DefaultScreen(display));
	}
    }
    
    gcv.foreground=foreColor.pixel;
    
    if (colorDisplay) {

	grayColor.red = grayColor.green = grayColor.blue = 43256; /* 2/3 intensity gray. */
	grayColor.flags = DoRed | DoGreen | DoBlue;

	if (XAllocColor(display, DefaultColormap(display, DefaultScreen(display)), &grayColor) != 0) {
	    color = gcv.foreground = grayColor.pixel;
	    gcv.fill_style = FillSolid;
	    tileOrStipple = 0;
	} else gcv.fill_style = FillOpaqueStippled;
    }else gcv.fill_style = FillOpaqueStippled;

    if (gcv.fill_style != FillSolid) { 

	if ((grayMap = XCreateBitmapFromData(display, RootWindow(display, screen), (char *) grayImage, 32, 8)) == None)
	    return -1;

	gcv.foreground = foreColor.pixel;
	/*
	if (gcv.fill_style == FillTiled) {
	    gcv.tile = grayMap;
	    tileOrStipple = GCTile;
	}
	else */{
	    gcv.stipple = grayMap;
	    tileOrStipple = GCStipple;
	}
    }

    dp->grayforegroundPixel = gcv.foreground;
    gcv.background = backColor.pixel;
    gcv.font = dp->selectionFont->fid;
    tempgc = XCreateGC(display, window, GCFillStyle | GCForeground |
			GCBackground | GCFont | tileOrStipple, &gcv);

    if (grayMap != None)
	XFreePixmap(display, grayMap);

    if (tempgc != None)
	dp->grayGC = tempgc;
    else
	goto error;

    gcv.foreground = backColor.pixel;
    gcv.background = foreColor.pixel;
    if ((dp->whiteGC = XCreateGC(display, window, GCForeground | GCBackground, &gcv)) == None)
	goto error;

    gcv.foreground = foreColor.pixel;
    gcv.background = backColor.pixel;
    gcv.font = dp->titleFont->fid;
    if ((dp->titleBlackGC = XCreateGC(display, window, GCForeground | GCBackground | GCFont, &gcv)) == None)
	goto error;
    gcv.foreground = foreColor.pixel;
    gcv.background = backColor.pixel;
    gcv.font = dp->selectionFont->fid;
    dp->foregroundPixel = foreColor.pixel;
    if ((dp->blackGC = XCreateGC(display, window, GCForeground | GCBackground | GCFont, &gcv)) == None)
	goto error;

    gcv.subwindow_mode = IncludeInferiors;
    gcv.graphics_exposures = FALSE;
    gcv.function = GXcopy;
    if ((dp->saveUnderGC = XCreateGC(display, window, GCSubwindowMode | GCGraphicsExposures | GCFunction, &gcv)) == None)
	goto error;


    if (!colorDisplay) {
	gcv.plane_mask = BlackPixel(display, screen) ^ WhitePixel(display, screen);
	gcv.function = GXinvert;
	if ((dp->invertGC = XCreateGC(display, window, GCFunction | GCPlaneMask, &gcv)) == None)
	    goto error;
	if (dp->motifMenus) {
	    dp->topshadowGC = dp->grayGC;
	    dp->bottomshadowGC = dp->blackGC;
	}
	dp->highlightUsingGray = FALSE;
    }
    else {
	gcv.plane_mask = foreColor.pixel ^ backColor.pixel;
	gcv.function = GXinvert;
	if ((dp->invertGC = XCreateGC(display, window, GCFunction | GCPlaneMask, &gcv)) == None)
	    goto error;
	if (dp->motifMenus) {
	    gcv.foreground = topshadowColor.pixel;
	    gcv.background = foreColor.pixel;
	    if ((dp->topshadowGC = XCreateGC(display, window, GCForeground | GCBackground, &gcv)) == None)
		goto error;
	    gcv.foreground = bottomshadowColor.pixel;
	    gcv.background = foreColor.pixel;
	    if ((dp->bottomshadowGC = XCreateGC(display, window, GCForeground | GCBackground, &gcv)) == None)
		goto error;
	}
	dp->highlightUsingGray = FALSE;
    }

    if ((wormIcon = XCreatePixmapFromBitmapData(display, RootWindow(display, screen), (char *) worm_bits, worm_width, worm_height, colorDisplay ? foreColor.pixel : BlackPixel(display, screen), colorDisplay ? backColor.pixel : WhitePixel(display, screen), DefaultDepth(display, screen))) == None)
	goto error;
    else {
	dp->wormWidth = worm_width;
	dp->wormHeight = worm_height;
	dp->wormIcon = wormIcon;
    }

    return 0;

error:

      if (colorDisplay) {
	  unsigned long pixels[5];

	  pixels[0] = color; 
	  pixels[1] = foreColor.pixel;
	  pixels[2] = backColor.pixel;
	  XFreeColors(display, DefaultColormap(display, DefaultScreen(display)), pixels, 3, 0);
      }
    if (dp->grayGC != None)
	XFreeGC(display, dp->grayGC);
    if (dp->whiteGC != None)
	XFreeGC(display, dp->whiteGC);
    if (dp->titleBlackGC != None)
	XFreeGC(display, dp->titleBlackGC);
    if (dp->blackGC != None)
	XFreeGC(display, dp->blackGC);
    if (dp->saveUnderGC != None)
	XFreeGC(display,dp->saveUnderGC);
    if (dp->invertGC != None)
	XFreeGC(display,dp->invertGC);
    if (colorDisplay && dp->motifMenus) {
        if (dp->topshadowGC != None)
	    XFreeGC(display,dp->topshadowGC);
        if (dp->bottomshadowGC != None)
	    XFreeGC(display,dp->bottomshadowGC);
    }

    if (dp->wormIcon != None) /* Never happens, but here for safety when someone adds another item... */
	XFreePixmap(display,dp->wormIcon);

    return -1;
}

static int GetDisplayInfo(dpy, menu, def_env)
Display *dpy;
struct cmenu *menu;
char *def_env;
{

    struct cmenudata *dp;
    Window rootWindow;
    char *def_val;
    char *fontName;
    XWindowAttributes windowAttributes;
    Screen *s = DefaultScreenOfDisplay(dpy);
    int colorDisplay = FALSE;
    int newshadows=getprofileswitch("UseNewShadows", FALSE);
    
    newshadows=getprofileswitch("PopupUseNewShadows", newshadows);
    
    rootWindow = RootWindow(dpy, DefaultScreen(dpy));
    if (XGetWindowAttributes(dpy, rootWindow, &windowAttributes) > 0)
	if (windowAttributes.depth > 1 && CellsOfScreen(s) >= getprofileint("ForceMonochromeThreshold", 16)) {
	    colorDisplay = TRUE;
	}

    /*
      * Set up the default environment name.
	  */
    if (def_env == NULL || *def_env == '\0')
	def_env = "cmenu";

    /*
      Get menu data structure
      */

    for (dp = menuDataList; dp != NULL  && (dp->dpy != dpy
      || (strcmp(dp->def_env, def_env) != 0));
    dp = dp->next)
	;

    if (dp != NULL)  {
	menu->gMenuData = dp;
	return TRUE;
    }

    dp = (struct cmenudata *) malloc(sizeof(struct cmenudata));

    dp->dpy = dpy;
    dp->def_env = (char *) malloc(strlen(def_env) + 1);
    strcpy(dp->def_env, def_env);

    /*
      * Get the RootWindow and default pixel colors.
	  */

    if (getprofileswitch("MotifPopupMenus", FALSE))
	dp->motifMenus = TRUE;
    else if ((def_val = XGetDefault(dpy, def_env, "MotifPopupMenus")) != NULL) {
	dp->motifMenus = (*def_val == 'y');
    }
    else
	dp->motifMenus = FALSE;

    
    if ((def_val = getprofile("PopupKeysSpacing")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuKeysSpacing")) != NULL) {
	dp->keyspace=atoi(def_val);
    }
    else {
	dp->keyspace=5;
    }
    
    if ((def_val = getprofile("PopupForegroundColor")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuForeground")) != NULL) {
	dp->foregroundColor = (char*)malloc(strlen(def_val)+1);
	strcpy(dp->foregroundColor,def_val);
    }
    else {
	dp->foregroundColor = (char*)malloc(strlen("black")+1);
	strcpy(dp->foregroundColor,"black");
    }

    if ((def_val = getprofile("PopupKeysColor")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuKeys")) != NULL) {
	dp->keysColor = (char*)malloc(strlen(def_val)+1);
	strcpy(dp->keysColor,def_val);
    }
    else {
	dp->keysColor = (char*)malloc(strlen(dp->foregroundColor)+1);
	strcpy(dp->keysColor, dp->foregroundColor);
    }
    
    if ((def_val = getprofile("PopupBackgroundColor")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuBackground")) != NULL) {
	dp->backgroundColor = (char*)malloc(strlen(def_val)+1);
	strcpy(dp->backgroundColor,def_val);
    }
    else {
	dp->backgroundColor = (char*)malloc(sizeof("gray80"));
	strcpy(dp->backgroundColor,(dp->motifMenus && newshadows && colorDisplay)?"gray80":"white");
    }

    if ((def_val = getprofile("PopupTopShadowColor")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuTopShadowColor")) != NULL) {
	dp->topshadowColor = (char*)malloc(strlen(def_val)+1);
	strcpy(dp->topshadowColor,def_val);
    }
    else {
	dp->topshadowColor = NULL;
    }

    if ((def_val = getprofile("PopupBottomShadowColor")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "MenuBottomShadowColor")) != NULL) {
	dp->bottomshadowColor = (char*)malloc(strlen(def_val)+1);
	strcpy(dp->bottomshadowColor,def_val);
    }
    else {
	dp->bottomshadowColor = NULL;
    }

    if (getprofileswitch("PopupUseSaveUnder", FALSE))
	dp->useSaveUnder = TRUE;
    else if ((def_val = XGetDefault(dpy, def_env, "UseSaveUnder")) != NULL) {
	dp->useSaveUnder = (*def_val != 'n');
    }
    else
	dp->useSaveUnder = TRUE;

    if ((def_val = getprofile("PopupClickInterval")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "ClickInterval")) != NULL)
	dp->clickInterval = atoi(def_val);
    else
	dp->clickInterval = cmenu_DefaultClickTime;

    if ((dp->menuWindow = GetMenuWindow(dpy, rootWindow, dp)) == None)  {
	_cmErrorCode = cmE_CREATE_INPUTONLY;
	goto error;
    }

    /*
      Now have to deal with font information
      */
    if ((fontName = getprofile("PopupTitleFont")) == NULL &&
	(fontName = XGetDefault(dpy, def_env, "TitleFont")) == NULL)
	fontName = cmenu_DefaultTitleFont;

    if ((dp->titleFont = XLoadQueryFont(dpy, fontName)) == NULL && (dp->titleFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	_cmErrorCode = cmE_OPEN_FONT;
	goto error;
    }

    dp->titleFontHeight = FONTHEIGHT(dp->titleFont);
    dp->titleFontAscent = dp->titleFont->max_bounds.ascent;
    dp->titleFontDescent = dp->titleFont->max_bounds.descent;

    if ((dp->yTitleOffset = dp->titleFontHeight / 4) < 2)
	dp->yTitleOffset = 2;

    dp->yShift = dp->titleFontHeight + dp->yTitleOffset + 2;
    dp->xShift = 16;
    if (dp->motifMenus) {
	dp->titleFontHeight+=2;
	dp->yShift+=3;	/* double line under the title needs another pixel */
    }

    if ((def_val = getprofile("PopupPaneSpread")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "PaneSpread")) != NULL) {
	extern double atof();
	double spread = atof(def_val);
	dp->xShift *= spread;
	if (dp->xShift < 4) dp->xShift = 4;
    }

    dp->xTitleOffset = dp->titleFont->max_bounds.lbearing + dp->xShift / 3;

    if ((fontName = getprofile("PopupItemFont")) == NULL &&
	(fontName = XGetDefault(dpy, def_env, "SelectionFont")) == NULL)
	fontName = cmenu_DefaultSelectionFont;

    if ((dp->selectionFont = XLoadQueryFont(dpy, fontName)) == NULL && (dp->selectionFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	_cmErrorCode = cmE_OPEN_FONT;
	goto error;
    }

    dp->selectionFontHeight = FONTHEIGHT(dp->selectionFont) + 2; /* 2 pixels for selection box overlap. */
    dp->selectionFontAscent = dp->selectionFont->max_bounds.ascent + 2;
    dp->xSelectionOffset = dp->xShift + dp->selectionFont->max_bounds.lbearing;

    dp->bottomMargin = dp->selectionFontHeight / 2;

    dp->overlapWidth = dp->xShift / 2;

    if ((fontName = getprofile("PopupKeysFont")) == NULL &&
	(fontName = XGetDefault(dpy, def_env, "KeysFont")) == NULL)
	fontName = cmenu_DefaultKeysFont;

    if ((dp->keysFont = XLoadQueryFont(dpy, fontName)) == NULL && (dp->keysFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	_cmErrorCode = cmE_OPEN_FONT;
	goto error;
    }

    if(FONTHEIGHT(dp->keysFont)+2 > dp->selectionFontHeight) dp->selectionFontHeight = FONTHEIGHT(dp->keysFont)+2;
     /* 2 pixels for selection box overlap. */

    if(dp->keysFont->max_bounds.ascent + 2>dp->selectionFontAscent) dp->selectionFontAscent = dp->keysFont->max_bounds.ascent + 2;

    /* if(dp->xShift + dp->keysFont->max_bounds.lbearing > dp->xSelectionOffset) dp->xSelectionOffset = dp->xShift + dp->keysFont->max_bounds.lbearing; */

    if(dp->selectionFontHeight / 2 > dp->bottomMargin) dp->bottomMargin = dp->selectionFontHeight / 2;

    if ((def_val = getprofile("PopupOverlapPct")) != NULL ||
	(def_val = XGetDefault(dpy, def_env, "OverlapPct")) != NULL) {
	dp->overlapPct = atoi(def_val);
        if (dp->overlapPct < 0)  {
            dp->overlapPct = 0;
        }
        else if (dp->overlapPct > 100) {
            dp->overlapPct = 100;
        }
    }
    else {
        dp->overlapPct = 0;
    }
    

    /*
      Creates GC's and Windows
      */
    if (GetGCs(dp, rootWindow, def_env, newshadows, colorDisplay) < 0)  {
	_cmErrorCode = cmE_CREATE_GC;
	goto error;
    }

    dp->next = menuDataList;
    menuDataList = dp;

    menu->gMenuData = dp;
    return 0;

error:
      return -1;
}

struct cmenu *cmenu_Create(dpy, parent, def_env, freeFunction)
Display *dpy;		/* Display structure pointer. */
Window parent;		/* Window ID of the menu's parent window. */
register char *def_env;	/* X Defaults program environment name. */
void (*freeFunction)();
{

    struct cmenu *menu;

    /*
      * Calloc the cmenu structure, the initial pane and the bounding box.
      */
    ;
    if ((menu = (struct cmenu *) malloc(sizeof(struct cmenu))) == NULL) {
	_cmErrorCode = cmE_CALLOC;
	goto error;
    }

    if (GetDisplayInfo(dpy, menu, def_env) < 0)  {
	goto error;
    }

    /*
      * Construct the cmenu object.
      */

    menu->panes = NULL;
    menu->numberOfPanes = 0;
    menu->wormPane = -1;
    menu->wormSelection = -1;
    menu->freeFunction = freeFunction; /* Used for free data associated with selections. */
    /*
      * Return the completed cmenu.
      */
    _cmErrorCode = cmE_NO_ERROR;
    return(menu);

error:
    if (menu != NULL)
	  free(menu);

    return NULL;
}

