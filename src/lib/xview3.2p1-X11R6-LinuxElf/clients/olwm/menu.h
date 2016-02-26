/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_MENU_H
#define _OLWM_MENU_H

#pragma ident "@(#)menu.h	26.35	93/06/28 SMI"

#include "events.h"


/* constants */


#define NOBUTTON	-1	/* no button is active */
#define PINBUTTON	-2	/* the pin is active */


/* types */


typedef int (*FuncPtr) ();	/* pointer to function returning int */

/*
 * Button: describes a single button in a menu.
 */
typedef struct _button {
    Text       *label[2];	/* displayed text, alternate text */
    char       *helpstring[2];	/* help for that button, alternate help */
    int		which;		/* which label to display */
    Bool        has_submenu;	/* tells whether this button has a submenu */
    Bool	enabled;	/* grayed out if not enabled */
    Bool	visible;	/* takes up no space if not visible */
    FuncPtr	callback;	/* function to call when item hit */
				/*   NULL if button has pullright */

    union {			/* additional data for the callback */
	struct _menu *submenu;	/* pointer to the submenu, if has_submenu */
	char   *command;	/* command, if callback is AppMenuFunc */
	void   *other;		/* unspecified */
    } action;

    SemanticAction semantic;	/* semantic action for window menu items */
} Button;

/*
 * Menu: describes a single popup menu.  Contains title, pin, list of buttons.
 */
typedef struct _menu {
    Text       *title;		/* the menu's title, NULL if no title */
    Button    **buttons;	/* array of buttons owned by this menu */
    int         buttonCount;	/* size of buttons array */
    int         buttonDefault;	/* index of dflt button into buttons array */
    Bool        hasPushPin;
    Bool	menudirty;	/* needs size recalced */
    char       *helpstring;
    Bool	wantAccelerators;   /* whether the object on which this menu */
				    /* appears wants the accelerators */
} Menu;

#define basewin_openitem	0
#define basewin_zoomitem	(basewin_openitem + 1)
#define basewin_moveitem	(basewin_zoomitem + 1)
#define basewin_resizeitem	(basewin_moveitem + 1)
#define basewin_propsitem	(basewin_resizeitem + 1)
#define basewin_backitem	(basewin_propsitem + 1)
#define basewin_refreshitem	(basewin_backitem + 1)
#define basewin_quititem	(basewin_refreshitem + 1)
#define basewin_ctbuttons	basewin_quititem

#define popup_dismissitem	0
#define popup_moveitem		(popup_dismissitem + 1)
#define popup_resizeitem	(popup_moveitem + 1)
#define popup_backitem		(popup_resizeitem + 1)
#define popup_refreshitem	(popup_backitem + 1)
#define popup_quititem		(popup_refreshitem + 1)
#define popup_ctbuttons		popup_quititem

/*
 * Global array of menus
 */
extern Menu *MenuTable[NUM_MENUS];
extern Bool flDoSetDefault;	/*is the ctrl key down (only used in winpinmenu.c)*/

/*
 * constants used by ShowStandardMenuSync()
 */
#define SYNC_DONE		0
#define SYNC_CHANGECLICK	(SYNC_DONE + 1)

/*
 * The ButtonInfo and MenuInfo structures are created in parallel with the
 * Button and Menu structures.  The info structures have geometry information, 
 * which depends on the fonts currently in use.
 */

typedef struct _buttonInfo {
    Button     *button;
    int         buttonX, buttonY;
    int		buttonHeight;
    Bool	flDirty;	/*damaged*/
    struct _menuInfo *subMenu;
    Bool	has_accel;	/* whether this button shows an accelerator */
    Bool	has_meta_mark;	/* whether accelerator has a diamond mark */
    Text       *accel_modifier;	/* accelerator modifier text */
    Text       *accel_key;	/* the actual accelerator key name */
} ButtonInfo;

typedef struct _menuInfo {
    int			depth;
    Menu	       *menu;
    ButtonInfo	       *buttons;
    int			notitleOffset, buttonOffset;
    int			menuX, menuY, menuWidth, menuHeight;
    int			titleX, titleY, titleHeight, titleWidth;
    int			pushPinX, pushPinY;
    int			maxbuttonWidth;
    int			labelPos, accModPos, buttonMarkPos, accKeyPos;
    struct _wingeneric *menuWin;
    struct _menuInfo   *origmenuInfo;
    struct _menuInfo   *pinnedBrother;
    Bool		childActive;
    Bool		pinIn;
    int			litButton;
    Bool		ignoreNextExpose;
    SemanticAction	action;
    int			ringedButton;
    Bool		hasAccelerators;
} MenuInfo;


/*
 * MenuCache: a per-screen cache of menu information.
 */
typedef struct _menuCache {
    MenuInfo	      **menuInfoList;	/* dynamic */
    int			nextSlot;
    int			maxSlots;	/* how big is menuInfoList */ 
    struct _winmenu   **menuWinList;
    int			maxDepth;
} MenuCache;


typedef enum _menuTrackMode {
    MODE_DRAG,				/* press-drag-release */
    MODE_CLICK				/* click-move-click */
} MenuTrackMode;


typedef enum _menuLocation {
    ML_BUTTON,				/* on a button */
    ML_PIN,				/* on the pin */
    ML_MENU,				/* elsewhere on the menu */
    ML_OFFMENU,				/* outside the menu entirely */
    ML_BUTTONDISABLED			/* on a disabled button */
} MenuLocation;

#define BUTTON_INDEX_OK(mi,idx) ((idx)>=0 && (idx)<(mi)->menu->buttonCount)

#ifdef notdef
/*
 * Default button list
 */
typedef struct _defaults {
    char        Name[80];
    int         DefaultButton;
    MenuInfo   *mInfo;
    struct _defaults *next;
}           Defaults, *DefaultsP;
#endif


/*
 *****************************************************************************
 * External functions
 *****************************************************************************
 */

void SetMenuDefault();
Menu *NewNamedMenu();
Bool AppendMenuItem();
Menu *CreateMenu();
Menu *GetEnabledMenu();
MenuInfo *MenuInfoCreate();
void ShowStandardMenu();
void ShowStandardMenuSync();
void SetClickCallback();

extern void InitMenus();
extern MenuCache *InitScreenMenus( /* Display *dpy, ScreenInfo *scrInfo */ );
extern void MenuCreate( /* dpy, menu */ );
extern void MenuShow( /* dpy, WinGeneric, menu, event */ );
extern void SetButton( /* dpy, menu, bindex, Bool */ );
extern void ExecButtonAction( /* dpy, winInfo, menu, btn, Bool */ );
extern void DrawMenu( /* dpy, menu */ );
extern int  PointInRect( /* x, y, rx, ry, rw, rh */ );


/*
 * generically useful region code that happens to live in menu.c
 */

void InitRegions();
void EmptyRegion();
void RectRegion();
void AppendExposeDamage();
void MakeExposeDamage();


/*
 *	WinMenu Functions (from winmenu.c)
 */
extern struct _winmenu *
MakeMenu( /* Display *dpy, 
	     WinRoot *winInfo */ 
	 );
extern void
MapMenuWindow(/* Display *dpy, 
		 WinMenu *winInfo, 
		 MenuInfo *menuInfo */ 
	      );
extern void
UnmapMenuWindow(/* Display *dpy, 
		   WinMenu *winInfo, 
		   MenuInfo *menuInfo */ 
		);


int MenuEventExpose();
int MenuEventDrawMenu();



SemanticAction MenuMouseAction(/*Display *dpy,
				 XEvent *pevent,
				 long mask*/
			       );

/*
 *	WinPinMenu Functions (from winpinmenu.c)
 */
extern struct _winpinmenu *
MakePinMenu(/* Display *dpy, 
	       WinRoot *winInfo, 
	       MenuInfo *menuInfo */ 
	    );


/*
 * macros for setting menu items
 */

#define DirtyMenu(pmenu)	(pmenu)->menudirty = True
#define _menuset(p,i,q,x)	do { \
				   Button *pb = p->buttons[i];	\
				   if (pb->q != (x)) {		\
				      DirtyMenu(p);		\
				      pb->q = (x);		\
				      }				\
				} while(0)
#define ToggleVisible(p,i,x)	_menuset(p,i,visible,(x))
#define ToggleItem(p,i,x)	_menuset(p,i,which,(((x) == 0)? False : True))
#define ToggleEnabled(p,i,x)	_menuset(p,i,enabled,(x))
#define SetMenuTitle(m,t)	do {  \
				   if (m->title != t) {		\
				      m->title = t;		\
				      DirtyMenu(m);		\
				   }				\
			 	} while (0)

#endif /* _OLWM_MENU_H */

