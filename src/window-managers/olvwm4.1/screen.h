/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)screen.h	1.4 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)screen.h	26.24	93/06/28 SMI"
 *
 */

#ifndef _OLWM_SCREEN_H
#define _OLWM_SCREEN_H

#ifdef OW_I18N_L4
#include "i18n.h"
#endif
#include <olgx/olgx.h>

#include "win.h"

/*
 *	Index's into GC array of ScreenInfo struct
 */
typedef enum {	ROOT_GC,
		FOREGROUND_GC,
		BORDER_GC,
		WINDOW_GC,
		WORKSPACE_GC,
		BUSY_GC,
		ICON_NORMAL_GC,
		ICON_MASK_GC,
		ICON_BORDER_GC,
		INPUTFOCUS_GC,
		VDM_GC,
		VDM_INPUT_GC,
		NUM_GCS 
} ScreenGCIndex;

/*
 *	Index's into Graphics_info array of ScreenInfo struct
 */
typedef enum { NORMAL_GINFO, 
	       BUTTON_GINFO, 
	       TEXT_GINFO,
	       REVPIN_GINFO,
	       INPUTFOCUS_GINFO,
	       NUM_GINFOS 
} ScreenGinfoIndex;

/*
 *	Index's into Pixmap array of ScreenInfo struct
 */
typedef enum {
    BUSY_STIPPLE,
    ICON_BITMAP,
    ICON_MASK,
    PROTO_DRAWABLE,
    GRAY50_BITMAP,
    NUM_PIXMAPS
} ScreenPixmapIndex;

/*
 * 	ColorMapFocus	- client/window which has colormap focus
 */
typedef struct _colormapfocus {
	struct _client		*client;
	struct _wingeneric	*window;
	Bool			locked;
} ColorMapFocus;

/*
 *	ColorInfo	- window/workspace/etc colors
 */
typedef struct _colorinfo {
	unsigned long		flags;
	Bool			reverseVideo;
	unsigned long		black, white;
	unsigned long		fgColor, bgColor;
	unsigned long		bg0Color,bg1Color,bg2Color,bg3Color;
	unsigned long		borderColor;
	WorkspaceStyle		workspaceType;
	unsigned long		workspaceColor;
	unsigned long		workspaceRootPixel;
	unsigned long           workspaceBitmapFg;
	unsigned long           workspaceBitmapBg;
	unsigned long		virtualFgColor, virtualBgColor;
	unsigned long		virtualGridColor, virtualFontColor;
	unsigned long		virtualPixmapColor;
	unsigned long		virtualInputColor;
	unsigned long		inputBg0Color,inputBg1Color,
				inputBg2Color,inputBg3Color;
	unsigned long		vIconColor;
} ColorInfo;

#define CIWorkspaceColorAlloced		(1L<<0)
#define CIWorkspaceBitmapColorsAlloced	(1L<<1)
#define CIWindowColorAlloced		(1L<<2)
#define CIForegroundColorAlloced	(1L<<3)
#define CIBackgroundColorAlloced	(1L<<4)
#define CIBorderColorAlloced		(1L<<5)
#define CIVirtualForegroundColorAlloced	(1L<<6)
#define CIVirtualBackgroundColorAlloced	(1L<<7)
#define CIVirtualFontColorAlloced	(1L<<8)
#define CIVirtualGridColorAlloced	(1L<<9)
#define CIInputFocusColorAlloced	(1L<<10)
#define CIVirtualPixmapColorAlloced	(1L<<11)

typedef struct _pixinfo {
    int		width, height;
    Pixmap	pixmap;
    unsigned long fg, bg;
    int		ncolors;
    XColor	*colors;
} PixInfo;

/*
 *	ScreenInfo	- Per screen info
 */
typedef struct _screeninfo {
	Display			*dpy;
	int			screen;
	Window			rootid;
	struct _winroot		*rootwin;
	int			depth;
	Visual			*visual;
	Colormap		colormap;
	Bool			iscolor;
	Bool			use3D;
	ColorInfo		colorInfo;
	GC			gc[NUM_GCS];
	Graphics_info		*gi[NUM_GINFOS];
	Pixmap			pixmap[NUM_PIXMAPS];
	PixInfo			pixInfo;	/* GIF/Pixmap bg info */
	struct _menuCache	*menuCache;
	ColorMapFocus		cmapfocus;
	int			framepos;
	struct _iconGrid	*iconGrid;
	char			**environment;
	int			instanceQ;	/* quark for this screen's
						   instance name */
	int			dfltIconWidth, dfltIconHeight;
#ifdef ALLPLANES
	Bool                    useAllPlanes;
#endif
	Window                  *winCache;      /* array of window IDs */
	int                     winCacheSize;   /* size of the array */
	int                     winCacheCount;  /* first free element */

	struct _virtualdesktop	*vdm;
	struct _menu		*menuTable[NUM_MENUS];
	struct {
	    struct _button	**frameFullButtons;
	    int			frameFullButtonsCount;
	    struct _button	**frameDismissButtons;
	    int			frameDismissButtonsCount;
	    struct _button	**frameLimitedButtons;
	    int			frameLimitedButtonsCount;
	} 			menuButtons;
} ScreenInfo;

/*
 *      Global functions from screen.c
 */

#if defined(__STDC__)

extern  void            InitScreens(Display *dpy);
extern  void            DestroyScreens(Display *dpy);
extern  ScreenInfo      *GetFirstScrInfo(void);
extern  ScreenInfo      *GetScrInfoOfScreen(int screen);
extern  ScreenInfo      *GetScrInfoOfRoot(Window root);
extern  void            SetWorkspaceBackground(Display *dpy);
extern  void            SetWindowColor(Display *dpy);
extern  void            SetForegroundColor(Display *dpy);
extern  void            SetBackgroundColor(Display *dpy);
extern  void            SetBorderColor(Display *dpy);
extern  void            SetTitleFont(Display *dpy);
extern  void            SetTextFont(Display *dpy);
extern  void            SetButtonFont(Display *dpy);
extern  void            SetIconFont(Display *dpy);
extern  void            SetGlyphFont(Display *dpy);
extern  void            SetIconLocation(Display *dpy);
extern  Window          ScreenCreateWindow(ScreenInfo*, Window,
					   int, int, int, int, unsigned long,
					   XSetWindowAttributes*);
extern  void            ScreenDestroyWindow(ScreenInfo*, Window);
extern  void            ScreenUpdateWinCacheSize(Display *dpy);

#else

extern  void            InitScreens();
extern  void            DestroyScreens();
extern  ScreenInfo      *GetFirstScrInfo();
extern  ScreenInfo      *GetScrInfoOfScreen();
extern  ScreenInfo      *GetScrInfoOfRoot();
extern  void            SetWorkspaceBackground();
extern  void            SetWindowColor();
extern  void            SetForegroundColor();
extern  void            SetBackgroundColor();
extern  void            SetBorderColor();
extern  void            SetTitleFont();
extern  void            SetTextFont();
extern  void            SetButtonFont();
extern  void            SetIconFont();
extern  void            SetGlyphFont();
extern  void            SetIconLocation();
extern  Window          ScreenCreateWindow();
extern  void            ScreenDestroyWindow();
extern  void            ScreenUpdateWinCacheSize();
#endif /* STDC */
 
#endif  /* _OLWM_SCREEN_H */

