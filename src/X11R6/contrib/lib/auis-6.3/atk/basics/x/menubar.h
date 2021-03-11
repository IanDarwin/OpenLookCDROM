/* **********************************************************************o *\
 *         Copyright IBM Corporation 1990,1991 - All Rights Reserved      *
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


#define mb_FullRedraw 0
#define mb_Update 1
#define mb_Exposed 2

struct prefs_s {
    Display *dpy;
    int newshadows;
    int ColorDisplay;
    int refcount;
    unsigned long topshadowPixel, cardtopshadowPixel;
    unsigned long bottomshadowPixel, cardbottomshadowPixel;
    unsigned long menutitlesPixel;
    unsigned long menubackgroundPixel;
    unsigned long carditemsPixel;
    unsigned long keysPixel;
    unsigned long cardbackgroundPixel;
    unsigned long grayitemPixel;
    unsigned long graytitlePixel;
    Pixmap topshadowPixmap, cardtopshadowPixmap;
    Pixmap bottomshadowPixmap, cardbottomshadowPixmap;
    Pixmap grayPixmap;
    Pixmap grayitemStipple;
    Pixmap graytitleStipple;
    XColor whiteColor, blackColor;
    XFontStruct *titlefont;
    XFontStruct *itemfont;
    XFontStruct *iconfont;
    XFontStruct *keysfont;
    int depth;
    int menubarheight;
    int vspacing, hspacing;
    int padding;
    int groupspacing;
    int itemhspace;
    struct timeval activatetime;
    unsigned int holdbutton;
    int grayPercentage, topshadowPercentage;
    struct prefs_s *next;
};

struct gcs {
    GC draw;
    GC select,selecterase;
    unsigned long topshadowPixel, bottomshadowPixel;
    Pixmap topshadowPixmap, bottomshadowPixmap;
    unsigned long grayPixel, whitePixel;
    Pixmap grayStipple;
};

struct mbinit {
    Display *dpy;	    /* display the menu is on */
    int	color;		    /* non-zero if this is a color display */
    Window client;	    /* window the menubar is for */
    Window menuw;	    /* window for the menu card */
    Window cascadew;	    /* window for first level of cascading */
    struct gcs titlegcs;    /* gcs used to draw the menubar */
    struct gcs cardgcs;	    /* gcs for the menu card */	    
    struct gcs cmcgcs;	    /* gcs for the first level of cascading */
    struct prefs_s *prefs;	    /* user configurable things */
    int x,y;
    unsigned int w,h;
    int	everdrawn;	    /* whether this menubar has ever been drawn */
    int	(*HandleExpose)();  /* function to be called with X expose events for the client window when
	a menu is up */
    long *ExposeData;	    /* Data to be passed to the HandleExpose function for the client */
    void (*FreeItem)();	    /* free a menu items data */
};

    
    

struct titem {
    char *name;		/* name of this item */
    struct titem *next;	/* next item */
    char *data;		/* data to be reported when this item is choosen */
    char *keys;		/* the keybinding for this item */
    short y;
#define SUBMENUFLAG (1<<1)
#define ACTIVEFLAG (1<<0)
    char flags;		/* bit 0 is 1 if item is active
			   bit 1 is 1 if item is a submenu */
    char prio;		/* priority of this item */
};

struct tmenu {
    int x,y,w,h,mx,mw,ww,wh;
    char *title;    /* title if this is a toplevel menu */
    struct titem *items;    /* linked list of the items */
    struct titem **lookup;
    struct tmenu *next;	/* next menu in the overflow list*/
    Window window;
    struct gcs gcs;
    short   iwidth;	    /* maximum width of any item name */
    short   kwidth;	    /* maximum width of any key binding */
    short   nitems;	    /* number of items in this menu */
    short   lastitem;
    short   titlelen;   /* length of the title */
    short   vert,horiz;
    unsigned short groupmask; /* groups which have items have their bit set... groups numbered 0-9 */
    char   groupcount; /* number of item groups used in this menu */
    char    prio;	    /* priority if this is a toplevel menu */
};

struct menubar {
    struct mbinit *mbi;
    char *mainmenu,*moretitle;
    int	lastvm;  /* index of last menu title visible on the menubar (aside from more menu) */
    struct tmenu *overflow;	/* linked list of menus which don't fit */
    struct tmenu **menus;   /* array of all the toplevel menus */
    struct tmenu *lastmenu;	/* the last toplevel menu put up */
    struct tmenu *lasteventin;
    struct tmenu *moremenu;
    int	nmenus;		    /* number of top level menus */
    int	mmenus;		    /* number of pointers allocated for menus */
    int	lastmenuindex;	    /* index into menus of the top menu currently displayed */
    int resort;	    /* whether the menubar needs re-sorting */
    void (*MenuFunction)(); /* function to be called for a menu selection */
    char *MenuFunctionData; /* data for the MenuFunction */
    char *data;		    /* more data */
    int morewidth;
    Bool refit;
};

#ifdef __STDC__
typedef char *(*GetDefaultsFunction)(Display *, char *);
#else
typedef char *(*GetDefaultsFunction)();
#endif

#ifndef __STDC__
extern struct mbinit *mb_Init();
extern void mb_InitWindows();
extern struct menubar *mb_Create();
extern void mb_AddSelection();
extern void mb_DeleteSelection();
extern void mb_HandleConfigure();
extern void mb_Activate();
extern void mb_RedrawMenubar();
extern struct prefs_s *mb_GetPrefsForDisplay();
extern void mb_SetItemStatus();
extern void mb_HandleConfigure();
extern void mb_KeyboardActivate();
#else
extern struct mbinit *mb_Init();
extern void mb_InitWindows();
extern struct menubar *mb_Create();
extern void mb_AddSelection();
extern void mb_DeleteSelection();
extern void mb_HandleConfigure();
extern void mb_Activate();
extern void mb_RedrawMenubar();
extern struct prefs_s *mb_GetPrefsForDisplay();
extern void mb_SetItemStatus();
extern void mb_HandleConfigure();
extern void mb_KeyboardActivate();
#endif

