#ident	"@(#)usermenu.c	26.62	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

/*
 * This file contains all of the functions for manipulating the user menu
 *
 * Global Functions:
 * InitUserMenu 	-- load the user menu and initialise
 * ReInitUserMenu 	-- reload the user menu and re-initialise
 * RootMenuShow		-- call MenuShow on the root menu
 *
 */

/*
 * Syntax of the user menu file should be identical to that used by
 *	buildmenu (SunView style RootMenu files).
 *
 *	NOTICE that SunView compatibility has resulted in old-style
 *	olwm menus no longer being supported.
 *
 *	There are two new reserved keywords:
 *
 *		DEFAULT tags a default button
 *		TITLE tags a title string for a menu (for titlebar)
 *
 *	One syntax in sunview menus is not supported:
 *		<icon_file> can not be used as a menu item
 *
 *	Here are the common reserved keywords:
 *		MENU and END are used to delimit a submenu
 *		PIN (appearing after END) indicates the menu is pinnable
 *		EXIT (built-in - olwm service)
 *		REFRESH (built-in - olwm service)
 *		POSTSCRIPT will invoke psh on the named command
 *
 * 	The file is line-oriented, however commands to be executed can
 *	extend to the next line if the newline is escaped (\).
 *
 *	Each line consists of up to three fields:  a label (a string
 *	corresponding to either the menu label or menu option label),
 *	up to two tags (keywords), and a command to be executed
 *	(or a file from which to read a submenu).  Two tags are allowed
 *	if one of them is "DEFAULT" or "END".
 *
 *	The tag is used to indicate the start and end of menu definitions,
 *	pinnability, built-in functions, and default options.
 *	The label indicates the text which appears on the user's menu,
 *	and the command describes what should be done when each item
 *	is selected.
 *
 *	Labels must be enclosed in double quotes if they contain
 *	whitespace.  Commands may be enclosed in double quotes (but
 *	do not have to be).
 *
 *	Comments can be embedded in a file by starting a line with a
 *	pound sign (#).  Comments may not be preserved as the file is
 *	used.
 *
 *	There are several functions which aren't invoked as programs;
 *	rather, they are built in to window manager.  These built-in
 *	services are each denoted by a single keyword.  The keywords are
 *	listed in the svctokenlookup[] array initialization.
 *
 *	example (will always have label: "Workspace Menu"):
 *
 *	"Workspace Menu"	TITLE
 *	Programs	MENU
 *		"Helpful Programs"	TITLE
 *		"Command Tool"	cmdtool
 *		"Blue Xterm"	DEFAULT xterm -fg white \
 *				-bg blue
 *	Programs	END	PIN
 *	Utilities	MENU
 *		"Refresh Screen" DEFAULT REFRESH
 *		"Clipboard"	 CLIPBOARD
 *	Utilities	END
 */

#ifdef SYSV
#include <sys/types.h>
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>	/* for stat(2) */
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include <assert.h>
#include <string.h>
#include <pwd.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "globals.h"
#include "list.h"
#include "mem.h"
#include "win.h"
#include "menu.h"

static char *menuFileName	= "openwin-menu";
static char *workspaceHelpStub	= "workspace";
static int  menuRecursionCount;

extern char *getenv();

#define TOKLEN		300

#define DEFAULT_WORKSPACE_TITLE		"Workspace"
#define DEFAULT_WORKSPACE_HELPSTR	"workspace:DefaultMenu"


/* parseMenu return values */
#define MENU_RECURSION -2
#define MENU_FATAL     -1
#define MENU_NOTFOUND	0
#define MENU_OK		1
#define MENU_PINNABLE	2

typedef enum {
    UsrToken, MenuToken, EndToken, DefaultToken, PinToken,
    TitleToken, ServiceToken, PshToken
}           TokenType;

/* locally useful macros */

#define	APPEND_STRING(buf, str)	( strncat( buf, str, \
					( sizeof(buf) - strlen(buf) - 1 ) ) )
#define COUNT(x)	(sizeof(x) / sizeof(x[0]))

/*
 *****************************************************************************
 *	externals
 *****************************************************************************
 */

extern int  RefreshFunc();
extern int  ClipboardFunc();
extern int  PrintScreenFunc();
extern int  ExitFunc();
extern int  ExitNoConfirmFunc();
extern int  PropertiesFunc();
extern int  SaveWorkspaceFunc();
extern int  FlipDragFunc();
extern int  AppMenuFunc();
extern int  PshFunc();
extern int  NopFunc();
extern int  WindowCtlFunc();
extern int  RestartOLWM();
extern int  FlipFocusFunc();
extern int  ReReadUserMenuFunc();
extern int  OpenCloseSelnFunc();
extern int  FullRestoreSizeSelnFunc();
extern int  BackSelnFunc();
extern int  QuitSelnFunc();
extern int  StartDSDMFunc();
extern int  StopDSDMFunc();


/*
 *****************************************************************************
 *	local forward declarations
 *****************************************************************************
 */

static int  menuFromFile();
static int  parseMenu();
static void fillMenuStruct();
static TokenType lookupToken();
static Menu *buildFromSpec();
static void initMenu();
static void initButton();
static void freeButtonData();
static void freeMenuData();
static void freeUserMenu();
static Bool menuFileModified();
static void addToMenuInfo();
static void freeFileInfoList();
static int firstEnabledItem();

/*
 *****************************************************************************
 *	local data
 *****************************************************************************
 */

typedef struct {
    char       *filename;	/* menu file path */
    dev_t       device;	/* device that the inode/file reside on */
    ino_t       inode;	/* inode of menu file */
    time_t      mtime;	/* modification time */
}           FileInfo;

typedef struct {
    char       *topfilename;	/* top-level menu file name */
    List       *fileinfoList;	/* list of FileInfo's for each menu file */
}           MenuFileInfo;

MenuFileInfo menuFileInfo;

typedef struct _buttondata {
    struct _buttondata *next;
    char       *name;
    Bool        isDefault;
    Bool        isLast;
    FuncPtr     func;
    char       *exec;	/* string to be executed, like "xterm" */
    void       *submenu;
}           buttondata;


typedef struct {
    char       *title;
    char       *menulabel;
    int         idefault;	/* index of default button */
    int         nbuttons;
    Bool        pinnable;
    buttondata *bfirst;
}           menudata;


static menudata *makeRootMenu();

/*
 *****************************************************************************
 * 	global routines
 *****************************************************************************
 */


void SetWindowMenuLabels();

void
WindowMenuCreate(dpy)
    Display    *dpy;
{
    SetWindowMenuLabels();
    SetWindowMenuTitle();
    CreateScreenWindowMenuInfo(dpy);
}

void
WindowMenuDestroy(dpy)
    Display    *dpy;
{
    DestroyScreenWindowMenuInfo(dpy);
}


/*
 * getUserMenu
 *
 * Returns a pointer to a menudata structure describing the menu that was 
 * read.  If no menu was read successfully, returns NULL.
 */
static menudata *
getUserMenu()
{
    menudata   *userroot;
    char        temp[MAXPATHLEN];
    char       *path;
    char       *homeEnv;
    char        homePath[MAXPATHLEN];
    char       *openwinhomeEnv;
    char        openwinhomePath[MAXPATHLEN];

    /* try reading $OLWMMENU */
    path = getenv("OLWMMENU");
    if (path != NULL && (userroot = makeRootMenu(path)) != NULL)
	return userroot;

    /* try searching for "openwin-menu" */
    if ((userroot = makeRootMenu(menuFileName)) != NULL)
	return userroot;

    return NULL;
}


/*
 * createButton: allocate a new button structure, filled in based on the
 * parameters, and return it.
 */
static Button *
createButton(label0, label1, helpstr0, helpstr1, which, has_submenu,
	     enabled, visible, callback, action)
    Text   *label0;
    Text   *label1;
    char   *helpstr0;
    char   *helpstr1;
    int	    which;
    Bool    has_submenu;
    Bool    enabled;
    Bool    visible;
    FuncPtr callback;
    void   *action;
{
    Button *b =		(Button *) MemNew(Button);

    b->label[0] =	label0;
    b->label[1] =	label1;
    b->helpstring[0] =	helpstr0;
    b->helpstring[1] =	helpstr1;
    b->which =		which;
    b->has_submenu =	has_submenu;
    b->enabled =	enabled;
    b->visible =	visible;
    b->callback =	callback;
    b->action.other =	action;

    return b;
}


/*
 * createSimpleButton: create a simple button, with no alternate label,
 * with no submenu, always enabled and visible.
 */
static Button *
createSimpleButton(label, helpstr, callback, action)
    Text       *label;
    char       *helpstr;
    FuncPtr	callback;
    void       *action;
{
    return createButton(label, NULL, helpstr, NULL, 0,
			False, True, True, callback, action);
}


/*
 * createSeparatorButton: create and return a menu separator "button".
 */
static Button *
createSeparatorButton()
{
    return createButton(NULL, NULL, NULL, NULL, 0,
			False, False, True, NULL, NULL);
}


/*
 * getBuiltinMenu
 *
 * Create the built-in default menu.  This menu is used only if no menu file 
 * can be found.  It is intended to have a very minimal set of buttons.
 *
 * N.B.  All of the constant strings in this function must be copied into 
 * dynamically allocated storage, e.g. with GetNewText() or MemNewString().  
 * This is because these string pointers are put directly into the menu 
 * structure, which is assumed to be entirely on the heap.
 *
 * N.B.  If you add a new button to the built-in menu, you *must* change the
 * NBUTTONS macro to reflect the new number of buttons.  We would prefer to
 * use a static, initialized array of strings (letting the compiler determine
 * the size), but the string extraction routines require that the argument to
 * gettext() be a string constant, not the address of a string.  This forces
 * us to code the string constants in-line, thus requiring us to use an
 * atrocity like NBUTTONS.
 */

#define NBUTTONS 9	    /* number of buttons in the built-in menu */

static Menu *
getBuiltinMenu()
{
    Button **buttons;
    int b;

    buttons = (Button **) MemAlloc(NBUTTONS * sizeof(Button *));

    b = 0;

    buttons[b++] = createSimpleButton(GetNewText("Command Tool..."), NULL,
				      AppMenuFunc,
				      MemNewString("cmdtool"));

    buttons[b++] = createSimpleButton(GetNewText("xterm..."), NULL,
				      AppMenuFunc,
				      MemNewString("xterm"));

    buttons[b++] = createSeparatorButton();

    buttons[b++] = createSimpleButton(GetNewText("Refresh"), NULL,
				      RefreshFunc, NULL);

    buttons[b++] = createSimpleButton(GetNewText("Restart WM"), NULL,
				      RestartOLWM, NULL);

    buttons[b++] = createSimpleButton(GetNewText("Reread Menu File"), NULL,
				      ReReadUserMenuFunc, NULL);

    buttons[b++] = createSeparatorButton();

    buttons[b++] = createSimpleButton(GetNewText("Exit WM"), NULL,
				      ExitOLWM, NULL);

    buttons[b++] = createSimpleButton(GetNewText("Exit..."), NULL,
				      ExitFunc, NULL);

    assert(b == NBUTTONS);

    return CreateMenu(GetNewText(DEFAULT_WORKSPACE_TITLE),
	buttons, NBUTTONS, True, MemNewString(DEFAULT_WORKSPACE_HELPSTR));
}


/*
 * InitUserMenu
 *
 * Attempt to load a menu from the filesystem.  If no menu file can be found, 
 * create a built-in menu and use it instead.
 */
void
InitUserMenu(dpy)
    Display    *dpy;
{
    menudata *userroot;
    static Bool internalMenuTestMode = False;

    menuFileInfo.fileinfoList = NULL;
    menuFileInfo.topfilename = NULL;

    if (internalMenuTestMode)
	userroot = NULL;
    else
	userroot = getUserMenu();

    if (userroot == NULL) {
	MenuTable[MENU_ROOT] = getBuiltinMenu();
    } else {
	/* we read a menu from a file; now build it */
	MenuTable[MENU_ROOT] = buildFromSpec(dpy, userroot, userroot->title);
    }
}


/*
 * ReInitUserMenu
 *
 * Rereads the user menu if it has changed.  If the menu file has been
 * modified since last looked at or if forceReRead is True, attempt to create
 * a new menu from the file.  If the new file is ok and a menu is created then
 * use it, otherwise use the built-in workspace menu.
 *
 * REMIND - 1065563 - this shouldn't throw away the existing menu if the 
 * rereading attempt failed.
 */
void
ReInitUserMenu(dpy, forceReRead)
    Display    *dpy;
    Bool        forceReRead;
{
    menudata   *userroot;
    
    if (forceReRead || menuFileModified()) {
	freeFileInfoList(&menuFileInfo.fileinfoList);

	DestroyPinnedMenuClients();
	
	DestroyScreenUserMenuInfo(dpy);
	
	if (menuFileInfo.topfilename != NULL)
	    freeUserMenu(MenuTable[MENU_ROOT]);
	else if (MenuTable[MENU_ROOT] != NULL) {
	    /* default menu, most of it is just static data */
	    MemFree(MenuTable[MENU_ROOT]->buttons);
	    MemFree(MenuTable[MENU_ROOT]);
	}
	MenuTable[MENU_ROOT] = NULL;

	if (! forceReRead && menuFileInfo.topfilename != NULL) {
	    userroot = makeRootMenu(menuFileInfo.topfilename);
	} else {
	    MemFree(menuFileInfo.topfilename);
	    menuFileInfo.topfilename = NULL;
	    userroot = getUserMenu();
	}

	if (userroot == NULL)
	    MenuTable[MENU_ROOT] = getBuiltinMenu();
	else
	    MenuTable[MENU_ROOT] = buildFromSpec(dpy, userroot, userroot->title);

	CreateScreenUserMenuInfo(dpy);
    }
}


/*
 * RootMenuShow	- makes sure the user root menu is up to date and
 * then calls MenuShow on the root menu to bring it up on the display.
 */
void
RootMenuShow(dpy, winInfo, pEvent)
    Display    *dpy;
    WinGeneric *winInfo;
    XEvent     *pEvent;
{
    ReInitUserMenu(dpy, False);
    MenuShowSync(dpy, winInfo, MenuTable[MENU_ROOT], pEvent, NULL, NULL,
		 False, False);
}


/*
 * ReInitAllMenus
 *
 * Re-initialize all menus.
 */
void
ReInitAllMenus(dpy)
    Display	*dpy;
{
    WindowMenuDestroy(dpy);
    WindowMenuCreate(dpy);
    ReInitUserMenu(dpy, True);
}


/*
 *****************************************************************************
 * Local routines
 *****************************************************************************
 */

/*
 * expandPath
 *
 * Expands any environment variables in a path.  Returns a dynamically
 * allocated string with the expanded path.
 * 
 * Actually, this will also expand things of the nature:
 * $(OPENWINHOME)/include:${MUBMEL}/include:$FOOBLES/include
 */

static char *
expandPath(pin, messages)
    char *pin;
    Bool messages;
{
    char pathname[MAXPATHLEN];
    int haveslash;
    char envbuff[MAXPATHLEN];
    char *pend;
    char *penv;
    char *pstart;
    char *p;
    int len;
    struct passwd *ppw;
    char c;
    char *orig;

    if (pin == NULL)
	return NULL;

    orig = pin;

    *pathname = NULL;

    len = strlen(pin);

    if (pin[len] == '/')
	--len;

    haveslash = False;

    for (p = pathname;  len > 0;) {
	switch (*pin) {
	  case '~': /*overrides everything back to last :*/
	    ++pin;
	    --len;
	    if (len <= 1 || *pin == '/') {
		penv = getenv("HOME");
	    } else {
		int tmp;
		pend = strchr(pin, '/');
		if (pend == NULL) {
		    pend = strchr(pin, ':');
		    if (pend == NULL)
			pend = pin + strlen(pin);
		}
		tmp = pend - pin;
		memcpy(envbuff, pin, tmp);
		envbuff[tmp] = NULL;
		pin = pend;
		len -= tmp;
		ppw = getpwnam(envbuff);
		if (ppw == NULL && messages)
		    fprintf(stderr, 
		      GetString("olwm: couldn't find user \"%s\" in \"%s\"\n"),
		      envbuff, orig);
		penv = (char *) (ppw? ppw->pw_dir : NULL);
	    }
	    if (penv) {
		for (pstart = p;  pstart > pathname;   pstart--)
		    if (*pstart == ':') {
			++pstart;
			break;
		    }
		strcpy(pstart, penv);
		p = pstart + strlen(penv);
	    }
	    haveslash = False;
	    continue;
	  case '$':
	    if ((c = *(pin+1)) == '(' || c == '{') {
		int tmp;

		pin += 2;
		pend = strpbrk(pin, "})");
		if (pend == NULL) {
		    if (messages)
			fprintf(stderr, 
				GetString("olwm: no match for '%c' in pathname \"%s\"\n"), 
				c, orig);
		    return NULL;
		} else {
		    if (*pend == '}' && c != '{') {
			if (messages)
			    fprintf(stderr, 
				    GetString("olwm: found a '}' before a ')' in \"%s\"\n"),
				    orig);
			return NULL;
		    } else if (*pend == ')' && c != '(') {
			if (messages)
			    fprintf(stderr, 
				    GetString("olwm: found a ')' before a '}' in \"%s\"\n"),
				    orig);
			return NULL;
		    }
		}
		tmp = pend - pin;
		memcpy(envbuff, pin, tmp);
		envbuff[tmp] = NULL;
		len -= (2 + tmp + 1);
		pin = pend+1;
	    } else {
		--len;
		pend = strchr(++pin, '/');
		if (pend) {
		    int tmp = pend - pin;
		    memcpy(envbuff, pin, tmp);
		    envbuff[tmp] = NULL;
		    len -= tmp;
		    pin = pend;
		} else {
		    memcpy(envbuff, pin, len);
		    envbuff[len] = NULL;
		    len = 0;
		}
	    }
	    penv = getenv(envbuff);
	    if (penv) {
		int tmp = strlen(penv);
		if (haveslash && *penv == '/') {
		    /*if he put /usr//home, turn it into /home*/
		    /*/mumble:/usr//home -> /mumble:/home*/
		    for (; p > pathname;  p--)
			if (*p == ':') {
			    ++p;
			    break;
			}
		}
		memcpy(p, penv, tmp);
		p += tmp;
		haveslash = len > 0 && *(p-1) == '/';
	    }
	    if (len <= 0)
		break;
	    /*FALL THROUGH*/
	  default:
	    if (*pin != '/')
		haveslash = False;
	    else {
		if (!haveslash)
		    haveslash = True;
		else {
		    ++pin;
		    --len;
		    continue;
		}
	    }
	    *p++ = *pin++;
	    --len;
	}
    }
    *p = NULL;
    if (haveslash)
	pathname[strlen(pathname)-1] = NULL;

    return MemNewString(pathname);
}

/*
 * Menu Search Path
 */
#define	NUM_SEARCH_PATH		7
static	char	**menuSearchPath;

/*
 * makeMenuSearchPath
 */
static char **
makeMenuSearchPath()
{
	char	buf[MAXPATHLEN];
	char	*home;
	char	*owHome;
	int	i = 0;

	if ((home = getenv("HOME")) == NULL)
		home = ".";

	if ((owHome = getenv("OPENWINHOME")) == NULL)
		owHome = "/usr/openwin";

	menuSearchPath = (char **)MemAlloc(NUM_SEARCH_PATH*sizeof(char *));

#ifdef OW_I18N_L3
	/* $HOME/.<menufile>.<locale> */
	sprintf(buf, "%s/.%%1$s.%%2$s", home);
	menuSearchPath[i++] = MemNewString(buf);
#endif

	/* $HOME/.<menufile> */
	sprintf(buf, "%s/.%%s", home);
	menuSearchPath[i++] = MemNewString(buf);

#ifdef OW_I18N_L3
	/* $OPENWINHOME/share/locale/<locale>/olwm/<menufile> */
	sprintf(buf, "%s/share/locale/%%2$s/olwm/%%1$s", owHome);
	menuSearchPath[i++] = MemNewString(buf);

	/* $OPENWINHOME/lib/<menufile>.<locale> */
	sprintf(buf, "%s/lib/%%1$s.%%2$s", owHome);
	menuSearchPath[i++] = MemNewString(buf);
#endif

	/* $OPENWINHOME/lib/<menufile> */
	sprintf(buf, "%s/lib/%%s", owHome);
	menuSearchPath[i++] = MemNewString(buf);

	/* /usr/openwin/lib/<menufile> */
	menuSearchPath[i++] = MemNewString("/usr/openwin/lib/%s");

	menuSearchPath[i] = (char *)NULL;

	return menuSearchPath;
}

/*
 * menuFromFileSearch
 */
static int
#if defined(__STDC__)
menuFromFileSearch(
	char		*file,
	menudata	*menu,
	Bool		messages)
#else
menuFromFileSearch(file, menu, messages)
	char		*file;
	menudata	*menu;
	Bool		messages;
#endif /* __STDC__ */
{
	char		**pFmt;
	char		fullPath[MAXPATHLEN];
	char		*locale = GRV.lc_dlang.locale;
	int		rval;

	if (file[0] == '/')
		return menuFromFile(file, menu, messages);

	if (menuSearchPath == NULL)
		makeMenuSearchPath();

	for (pFmt = menuSearchPath; *pFmt; pFmt++) {

		(void)sprintf(fullPath, *pFmt, file, locale);
#ifdef DEBUG
fprintf(stderr,"menuFromFileSearch: trying '%s'\n",fullPath);
#endif

		if (access(fullPath, R_OK) == 0) {

			rval = menuFromFile(fullPath, menu, messages);

			if (rval >= MENU_OK)
				return rval;
		}
	}
	return MENU_NOTFOUND;
}


/*
 * menuFromFile - read a menu description from a file
 *
 *	Return values: same as parseMenu, with the addition of
 *		MENU_NOTFOUND = couldn't read submenu file
 */
static int
menuFromFile(file, menu, messages)
    char       *file;
    menudata   *menu;
    Bool        messages;
{
    char       *new;
    FILE       *stream;
    int         lineno = 1;	/* Needed for recursion */
    int         rval;

    if (++menuRecursionCount > 25) {
	fprintf(stderr,GetString("olwm: maximum menu filedepth exceeded\n"));
	menuRecursionCount = 0;
	return MENU_RECURSION;
    }

    /* expand any environment vars in path */
    if ((new = expandPath(file, messages)) != NULL)
	file = new;

    if (file[0] != '/')
	return menuFromFileSearch(file, menu, messages);

    if ((stream = fopen(file, "r")) == NULL) {
	if (messages)
	    fprintf(stderr, GetString("olwm: can't open menu file %s\n"), file);

	rval = MENU_NOTFOUND;
	goto end;
    }
    rval = parseMenu(file, stream, menu, &lineno);
    fclose(stream);

    if (rval >= MENU_OK) {
	addToMenuInfo(file);
	fillMenuStruct(menu);
    }

end:
    if (new)
	MemFree(new);

    return (rval);
}

/*
 * makeRootMenu - attempts to create a root menu from a given file.
 *	Returns NULL on failure, and assumes that the caller will free
 *	returned menudata.  
 */
static menudata *
makeRootMenu(file)
    char       *file;
{
    menudata  *userroot;

    initMenu(&userroot);
    userroot->pinnable = True;

    menuRecursionCount = 0;

    if (menuFromFileSearch(file, userroot, False) >= MENU_OK) {
	if (!menuFileInfo.topfilename)
	    menuFileInfo.topfilename = MemNewString(file);
	return userroot;
    } else {
	freeMenuData(userroot);
	freeFileInfoList(&menuFileInfo.fileinfoList);
	return (menudata *) NULL;
    }
}


/*
 * parseMenu -- read the user menu from the given stream and
 *	parse the stream into the menu structures defined locally.
 *	These structures (which are local to this module) are later
 *	used to build real menu structures.
 *
 *	Note that fillMenuStruct() needs to be called after parseMenu()
 *	is called (to finish filling out the menudata structure).
 *	If parseMenu() returns < 0, then freeMenuData() needs to be
 *	called instead, to free up unused memory.
 *
 *	Return values:
 *		MENU_OK		= an unpinnable menu was read successfully
 *		MENU_PINNABLE	= a pinnable menu was read successfully
 *		MENU_FATAL	= a fatal error was encountered
 *
 *	This is based heavily on buildmenu's getmenu() parsing routine.
 *
 */
static int
parseMenu(filename, stream, parent, lineno)
    char       *filename;
    FILE       *stream;
    menudata   *parent;
    int        *lineno;
{
    menudata   *currentMenu, *saveMenu;
    buttondata *currentButton;
    char        line[TOKLEN];
    char        label[TOKLEN];
    char        prog[TOKLEN];
    char        args[TOKLEN];
    static char localBuf[1024];
    char       *nqformat = "%[^ \t\n]%*[ \t]%[^ \t\n]%*[ \t]%[^\n]\n";
    char       *qformat =  "\"%[^\"]\"%*[ \t]%[^ \t\n]%*[ \t]%[^\n]\n";
    char       *format;
    register char *p;
    int         continuation;
    Bool        done;

    currentMenu = parent;
    initButton((buttondata **) & (currentMenu->bfirst));
    currentButton = currentMenu->bfirst;
    continuation = 0;

    for (; fgets(line, sizeof(line), stream); (*lineno)++) {
	if (line[0] == '#')
	    continue;

	for (p = line; isspace(*p); p++)
	    /* EMPTY */
	    ;

	if (*p == '\0')
	    continue;

	/*
	 * if we're already on a continuation line (the previous line ended in
	 * '\') then just copy the input through to the output until we get a
	 * line that doesn't end in '\' (nuke the vi backslash).
	 */
	if (continuation) {
	    /* fgets includes the newline in the string read */
	    while (line[strlen(line) - 2] == '\\') {
		/* get rid of backslash */
		line[strlen(line) - 2] = '\0';
		APPEND_STRING(localBuf, " ");
		APPEND_STRING(localBuf, p);
		if (!fgets(line, sizeof(line), stream))
		    break;
		(*lineno)++;
		for (p = line; isspace(*p); p++)
		    /* EMPTY */
		    ;
	    }
	    /* last line of continuation - replace \n with \0 */
	    line[strlen(line) - 1] = '\0';
	    APPEND_STRING(localBuf, " ");
	    APPEND_STRING(localBuf, p);
	    /* save it permanently in the buttondata structure */
	    currentButton->exec = MemNewString(localBuf);
	    localBuf[0] = '\0';
	    continuation = 0;
	    initButton((buttondata **) & (currentButton->next));
	    currentButton = currentButton->next;
	    continue;
	}
	/*
	 * if the line ends in '\' remember that continuation has started.
	 */
	if (line[strlen(line) - 2] == '\\') {
	    continuation = 1;
	    line[strlen(line) - 2] = '\0';
	}
	args[0] = '\0';
	format = (*p == '"') ? qformat : nqformat;

	if (sscanf(p, format, label, prog, args) < 2) {
	    /* seperator keyword appears alone on a line */
	    if (strcmp(label, "SEPARATOR") == 0) {
		currentButton->name = NULL;
		currentButton->isDefault = False;
		currentButton->func = NULL;
		currentButton->exec = NULL;
		currentButton->submenu = NULL;
		initButton((buttondata **) & (currentButton->next));
		currentButton = currentButton->next;
		continue;
	    }
	    /*otherwise...*/
	    fprintf(stderr,
		    GetString("olwm: syntax error in menu file %s, line %d\n"),
		    filename, *lineno);
	    return (MENU_FATAL);
	}
	
	if (strcmp(prog, "END") == 0) {
	    /* currently allocated button is last for this menu */
	    currentButton->isLast = True;
	    if (currentMenu->menulabel != NULL &&
		    strcmp(label, currentMenu->menulabel) != 0) {
		fprintf(stderr,
		   GetString("olwm: menu label mismatch in file %s, line %d\n"),
			filename, *lineno);
		return (MENU_FATAL);
	    }
	    /* compare PIN as # chars; args may have extra space */
	    if (strncmp(args, "PIN", 3) == 0)
		return (MENU_PINNABLE);
	    else
		return (MENU_OK);
	}
	if (strcmp(prog, "TITLE") == 0) {
	    currentMenu->title = MemNewString(label);

	    if (strncmp(args, "PIN", 3) == 0)
		currentMenu->pinnable = True;

	    /*
	     * we don't need to set up the next button, since the TITLE line
	     * didn't use up a button
	     */
	    continue;
	}
	currentButton->name = MemNewString(label);

	if (strcmp(prog, "DEFAULT") == 0) {
	    char       *t;
	    char       *u;

	    currentButton->isDefault = True;

	    /*
	     * Pull the first token from args into prog.
	     */
	    t = strtok(args, " \t");
	    if (t == NULL) {
		fprintf(stderr,
			GetString("olwm: error in menu file %s, line %d\n"),
			filename, *lineno);
		/* STRING_EXTRACTION - Since DEFAULT is keyword, do not
		 * translate.
		 */
		fputs(GetString("missing item after DEFAULT keyword.\n"), 
			stderr);
		return (MENU_FATAL);
	    }
	    strcpy(prog, t);
	    t = strtok(NULL, "");	/* get remainder of args */
	    if (t == NULL)
		args[0] = '\0';
	    else {
		u = args;
		/* can't use strcpy because they overlap */
		while (*u++ = *t++)
		    /* EMPTY */
		    ;
	    }
	}
	if (strcmp(prog, "INCLUDE") == 0) {
	    int         rval;

	    initMenu((menudata **) & (currentButton->submenu));
	    saveMenu = currentMenu;
	    currentMenu = (menudata *) currentButton->submenu;
	    currentMenu->menulabel = MemNewString(label);

	    if (args != NULL) {
		rval = menuFromFile(args, currentMenu, False);
		switch (rval) {
	        case MENU_PINNABLE:
	    	    currentMenu->pinnable = True;
		    /* FALL THRU */
	        case MENU_OK:
	            currentMenu = saveMenu;
		    break;
		default:	/* bad menu file */
		    initMenu((menudata **) & (currentButton->submenu));
		    break;
		}
	    }
	}
	if (strcmp(prog, "MENU") == 0) {
	    int         rval;

	    initMenu((menudata **) & (currentButton->submenu));
	    saveMenu = currentMenu;
	    currentMenu = (menudata *) currentButton->submenu;
	    currentMenu->menulabel = MemNewString(label);

	    if (args[0] == '\0') {
		/*
		 * we haven't incremented lineno for this read loop yet, so we
		 * need to do it now. when END is read, parseMenu returns
		 * without incrementing lineno, so the count will be ok when
		 * this loop increments it before reading the next line of the
		 * file.
		 */
		(*lineno)++;
		if ((rval = parseMenu(filename, stream,
				      currentMenu, lineno)) < 0) {
		    return (MENU_FATAL);
		} else
		    fillMenuStruct(currentMenu);
	    } else {
		rval = menuFromFile(args, currentMenu, True);
		if (rval <= MENU_NOTFOUND) {
		    return (MENU_FATAL);
		}
	    }
	    if (rval == MENU_PINNABLE)
		currentMenu->pinnable = True;

	    currentMenu = saveMenu;
	    /* if submenu not found, reuse button */
	    if (rval != MENU_NOTFOUND) {
		initButton((buttondata **) & (currentButton->next));
		currentButton = currentButton->next;
	    }
	    continue;
	}
	done = False;
	while (!done) {
	    switch (lookupToken(prog, &(currentButton->func))) {
	    case UsrToken:
		/*
		 * if UsrToken, that means that "prog" was just the first word
		 * of the command to be executed,
		 */
		strcpy(localBuf, prog);
		APPEND_STRING(localBuf, " ");
		APPEND_STRING(localBuf, args);
		/*
		 * copy current contents of localBuf back into args array so
		 * that PshToken code can be used
		 */
		strcpy(args, localBuf);
		localBuf[0] = '\0';
		/* fall through */
	    case PshToken:
		if (continuation)
		    strcpy(localBuf, args);
		else
		    currentButton->exec = MemNewString(args);
		done = True;
		break;
	    case PinToken:
		fprintf(stderr,
		     GetString("olwm: format error in menu file %s, line %d\n"),
		     filename, *lineno);
		/* STRING_EXTRACTION - Do not translate END or PIN since they
		 * 	are keywords in the menu file format
		 */
		fputs(
		 GetString("menu title and END required before PIN keyword.\n"),
		 stderr);
		return (MENU_FATAL);
		/* NOTREACHED */
		break;
	    default:
		/* some other valid token found and returned */
		done = True;
		break;
	    }
	}

	if (!continuation) {
	    initButton((buttondata **) & (currentButton->next));
	    currentButton = currentButton->next;
	}
    }
    /* never used the last button created */
    currentButton->isLast = True;

    return (MENU_OK);
}


/*
 * fillMenuStruct - Once the menu structures have been filled out using
 * 	information in the menu description file (via parseMenu()), the
 * 	nbuttons and idefault elements need to be set.
 */
static void
fillMenuStruct(mptr)
    menudata   *mptr;
{
    buttondata *bptr;
    int         buttonIndex = 0;

    bptr = mptr->bfirst;
    if (bptr->isLast == True) {
	MemFree(bptr);
	bptr = mptr->bfirst = NULL;
    }
    for (; bptr != NULL && bptr->isLast == False; bptr = bptr->next) {
	if (bptr->isDefault == True)
	    mptr->idefault = buttonIndex;

	if ((bptr->next)->isLast == True) {
	    MemFree(bptr->next);
	    bptr->next = NULL;
	}
	buttonIndex++;
    }
    /* buttonIndex is one past end, but started at 0, so = number buttons */
    mptr->nbuttons = buttonIndex;
}


/*
 * Array of allowed menu keywords (tokens).
 */

struct _svctoken {
    char       *token;
    FuncPtr     func;
    TokenType   toktype;
} svctokenlookup[] = {

    {	"REFRESH",	    RefreshFunc,	    ServiceToken	},
    {	"CLIPBOARD",	    ClipboardFunc,	    ServiceToken	},
    {	"PRINT_SCREEN",	    PrintScreenFunc,	    ServiceToken	},
    {	"EXIT",		    ExitFunc,		    ServiceToken	},
    {	"EXIT_NO_CONFIRM",  ExitNoConfirmFunc,	    ServiceToken	},
    {	"WMEXIT",	    ExitOLWM,		    ServiceToken	},
    {	"PROPERTIES",	    PropertiesFunc,	    ServiceToken	},
    {	"NOP",		    NopFunc,		    ServiceToken	},
    {	"DEFAULT",	    NULL,		    DefaultToken	},
    {	"MENU",		    NULL,		    MenuToken		},
    {	"END",		    NULL,		    EndToken		},
    {	"PIN",		    NULL,		    PinToken		},
    {	"TITLE",	    NULL,		    TitleToken		},
    {	"FLIPDRAG",	    FlipDragFunc,	    ServiceToken	},
    {	"SAVE_WORKSPACE",   SaveWorkspaceFunc,	    ServiceToken	},
    {	"POSTSCRIPT",	    PshFunc,		    PshToken		},
    {	"RESTART",	    RestartOLWM,	    ServiceToken	},
    {	"FLIPFOCUS",	    FlipFocusFunc,	    ServiceToken	},
    {	"REREAD_MENU_FILE", ReReadUserMenuFunc,	    ServiceToken	},
    {	"OPEN_CLOSE_SELN",  OpenCloseSelnFunc,	    ServiceToken	},
    {	"FULL_RESTORE_SIZE_SELN",   FullRestoreSizeSelnFunc, ServiceToken },
    {	"BACK_SELN",	    BackSelnFunc,	    ServiceToken	},
    {	"QUIT_SELN",	    QuitSelnFunc,	    ServiceToken	},
    {	"START_DSDM",	    StartDSDMFunc,	    ServiceToken	},
    {	"STOP_DSDM",	    StopDSDMFunc,	    ServiceToken	}

};

#define NSERVICES COUNT(svctokenlookup)

/*
 * lookupToken
 *
 * Look up a token in the list of tokens given a supposed keyword or service
 * name.  If the name doesn't match any existing token, return the
 * user-defined token.
 */
static      TokenType
lookupToken(nm, ppf)
    char       *nm;
    FuncPtr    *ppf;
{
    int         ii;

    for (ii = 0; ii < NSERVICES; ii++) {
	if (strcmp(nm, svctokenlookup[ii].token) == 0) {
	    if (ppf != (FuncPtr *) 0)
		*ppf = svctokenlookup[ii].func;
	    return svctokenlookup[ii].toktype;
	}
    }
    if (ppf != (FuncPtr *) 0)
	*ppf = AppMenuFunc;
    return UsrToken;
}


/*
 * buildFromSpec
 *
 * Build the real menu structures, and create the associated menus, from the
 * specifications parsed from the menu layout.  Free up the specifications as
 * we go along.
 */
static Menu *
buildFromSpec(dpy, pmenu, deftitle)
    Display    *dpy;
    menudata   *pmenu;
    char       *deftitle;
{
    Menu       *m;
    Button     *b;
    int         ii;
    buttondata *bdata, *bsave;
    Bool flpin;
    char *tit;
    char *menuHelp;
    char helpbuff[255];

    if (pmenu->pinnable) {
	flpin = True;
	if (pmenu->title == NULL) {
	    if (deftitle == NULL) {
		tit = MemNewString(GetString(DEFAULT_WORKSPACE_TITLE));
	    } else {
		tit = MemNewString(deftitle);
	    }
	} else {
	    tit = MemNewString(pmenu->title);
	}
    } else {
	flpin = False;
	/*
	 * Non-pinnable menus only get titles if they ask for them.
	 * m->title must be NULL if pmenu->title is NULL.
	 */
	if (pmenu->title == NULL)
	    tit = NULL;
	else
	    tit = MemNewString(pmenu->title);
    }

     menuHelp = NULL;

     if (tit != NULL) {
 	sprintf(helpbuff, "%s:%s", workspaceHelpStub, tit);
 	menuHelp = MemNewString(helpbuff);
     }

     if (menuHelp == NULL && deftitle != NULL) {
 	sprintf(helpbuff, "%s:%s", workspaceHelpStub, deftitle);
 	menuHelp = MemNewString(helpbuff);
     }

     if (menuHelp != NULL)
 	ReplaceChars(menuHelp, " \t", '_');

#ifdef OW_I18N_L4
     if (tit == NULL) {
	m = NewNamedMenu(NULL, flpin, menuHelp);
     } else {
	wchar_t *wtit = mbstowcsdup(tit);
	MemFree(tit);
	m = NewNamedMenu(wtit, flpin, menuHelp);
     }
#else
     m = NewNamedMenu(tit, flpin, menuHelp);
#endif

    /*
     * If no default has been specified, set the first button in the menu to be
     * the default button. REMIND: The OL spec wants the pin, if one exists, to
     * be the default in such a cse. Fix this.
     */

    for (ii = 0, bdata = pmenu->bfirst; ii < pmenu->nbuttons; ii++) {
	b = (Button *) MemNew(Button);

	/*right now, usermenus cannot have alternate items*/
#ifdef OW_I18N_L4
	b->label[0] = mbstowcsdup(bdata->name);
#else
	b->label[0] = bdata->name;
#endif
	b->label[1] = NULL;
	b->which = 0;

	b->has_submenu = (bdata->submenu != NULL);
	b->enabled = (bdata->name != NULL);
	b->visible = True;
	b->callback = bdata->func;

	if (! b->has_submenu) {
	    /* multi-purpose */
	    b->action.command = bdata->exec;
	} else {
	    b->action.submenu = buildFromSpec(dpy,
		(menudata *) bdata->submenu, bdata->name);
	    if (b->action.submenu->buttonCount < 1)
		b->enabled = False;
	}
	bsave = bdata;
	bdata = bdata->next;
	MemFree(bsave);
	AppendMenuItem(m, b);
    }
		     
    if (pmenu->idefault == NOBUTTON)
	SetMenuDefault(m, firstEnabledItem(m));
    else
	SetMenuDefault(m, pmenu->idefault);

    MemFree(pmenu->menulabel);
    MemFree(pmenu);

    return (m);
}


/*
 * initMenu - create and return an initialized menudata structure.
 */
static void
initMenu(newmenu)
    menudata  **newmenu;
{
    *newmenu = MemNew(menudata);
    (*newmenu)->title = NULL;
    (*newmenu)->menulabel = NULL;
    (*newmenu)->idefault = NOBUTTON;
    (*newmenu)->nbuttons = 0;
    (*newmenu)->pinnable = False;
    (*newmenu)->bfirst = (buttondata *) 0;
}


/*
 * initButton - create and return an initialized buttondata structure.
 */
static void
initButton(newButton)
    buttondata **newButton;
{
    *newButton = MemNew(buttondata);
    (*newButton)->next = NULL;
    (*newButton)->name = NULL;
    (*newButton)->isDefault = False;
    (*newButton)->isLast = False;
    (*newButton)->func = (FuncPtr) 0;
    (*newButton)->exec = NULL;
    (*newButton)->submenu = NULL;
}


/*
 * freeMenuData - free a menudata structure and any memory referenced by it.
 */
static void
freeMenuData(unusedMenu)
    menudata   *unusedMenu;
{
    buttondata *unusedButton;

    /* isLast probably isn't set, since this menu had an error */
    if ((unusedButton = unusedMenu->bfirst) != (buttondata *) 0)
	freeButtonData(unusedButton);

    MemFree(unusedMenu->title);
    MemFree(unusedMenu->menulabel);
    MemFree(unusedMenu);
}


/*
 * freeButtonData - free a buttondata structure and any memory referenced by
 * it.
 */
static void
freeButtonData(unusedButton)
    buttondata *unusedButton;
{
    if (unusedButton->next != NULL)
	freeButtonData(unusedButton->next);

    MemFree(unusedButton->name);
    MemFree(unusedButton->exec);
    if (unusedButton->submenu != NULL)
	freeMenuData(unusedButton->submenu);
    MemFree(unusedButton);
}


/*
 * freeUserMenu - recursively frees a Menu structure and all of its buttons.
 */
static void
freeUserMenu(menu)
    Menu       *menu;
{
    int         i;

    if (menu == NULL)
	return;

    for (i = 0; i < menu->buttonCount; i++) {

	/* free the submenu or the command string */

	if (menu->buttons[i]->has_submenu)
	    freeUserMenu(menu->buttons[i]->action.submenu);
	else
	    MemFree(menu->buttons[i]->action.command);

	/*
	 * REMIND: user menus cannot yet have alternates,
	 * nor specify help on a per item basis.
	 */
	MemFree(menu->buttons[i]->label[0]);
	MemFree(menu->buttons[i]);
    }
    MemFree(menu->buttons);
    MemFree(menu->title);
    MemFree(menu->helpstring);
    MemFree(menu);
}


/*
 * menuFileModified
 *
 * Check to see if any of the menu files have been changed.  Modifed is
 * defined as any change in either the inode or modification time of the file.
 * A change in the device/inode indicates a change in a symbolic link while a
 * change in the modification time indicates that the file has be edited.
 * Only true if the AutoReReadMenuFile resource is also true.
 */
static      Bool
menuFileModified()
{
    FileInfo   *fi;
    List       *lp;
    struct stat statbuf;

    if (!GRV.AutoReReadMenuFile)
	return False;

    lp = menuFileInfo.fileinfoList;
    for (fi = ListEnum(&lp); fi != NULL; fi = ListEnum(&lp)) {
	if (stat(fi->filename, &statbuf) < 0) {
	    return False;
	}
	if (statbuf.st_mtime != fi->mtime ||
		statbuf.st_dev != fi->device ||
		statbuf.st_ino != fi->inode) {
	    return True;
	}
    }
    return False;
}


/*
 * addToMenuInfo - Adds file and its stat info onto the list of FileInfo 
 * in menuFileInfo.  
 */
static void
addToMenuInfo(file)
    char       *file;
{
    FileInfo   *fi;
    struct stat statbuf;

    if (stat(file, &statbuf) < 0) {
	return;
    }
    fi = MemNew(FileInfo);
    fi->filename = MemNewString(file);
    fi->device = statbuf.st_dev;
    fi->inode = statbuf.st_ino;
    fi->mtime = statbuf.st_mtime;

    menuFileInfo.fileinfoList = ListCons(fi, menuFileInfo.fileinfoList);
}


/*
 * freeFileInfoList - frees all the FileInfo structs in a list and frees the
 * list itself.
 */
static void
freeFileInfoList(plist)
    List       **plist;
{
    FileInfo   *fi;
    List       *lp;
    List       *list = *plist;

    if (!list)
	return;

    lp = list;
    for (fi = ListEnum(&lp); fi != NULL; fi = ListEnum(&lp)) {
	MemFree(fi->filename);
	MemFree(fi);
    }

    ListDestroy(list);

    *plist = NULL;
}


/*
 *****************************************************************************
 */

/*
 *	Table of menus
 */
Menu       *MenuTable[NUM_MENUS];

/*
 *	Frame/icon menu action procs
 */
extern int  WindowOpenCloseAction(), WindowFullRestoreSizeAction();
extern int  WindowMoveAction(), WindowResizeAction();
extern int  WindowPropsAction(), WindowBackAction(), WindowRefreshAction();
extern int  WindowQuitAction(), WindowDismissThisAction();
extern int  WindowDismissAllAction(), WindowFlashOwnerAction();

/*
 *	Title and help strings
 */
static Text *windowTitle;
static char *windowMenuHelpString = "window:WindowMenu";

/*
 *	Buttons used to build the frame and icon menus
 *	REMIND: right now, toggles always use the same actions! 
 */
#ifdef __STDC__

static Button
openButton = {
    { NULL, NULL },
    { "window:Open", "window:Close" },
    0,
    False, 
    True,
    True,
    WindowOpenCloseAction,
    { NULL },
    ACTION_OPEN_CLOSE,
};

static Button
fullSizeButton = {
    { NULL, NULL },
    { "window:FullSize", "window:RestoreSize" },
    0,
    False,
    True,
    True,
    WindowFullRestoreSizeAction,
    { NULL },
    ACTION_FULL_RESTORE,
};

static Button
moveButton = {
    { NULL, NULL },
    { "window:Move", NULL },
    0,
    False, 
    True,
    True,
    WindowMoveAction, 
    { NULL },
    ACTION_MOVE,
};

static Button
resizeButton = {
    { NULL, NULL },
    { "window:Resize", NULL },
    0,
    False, 
    True,
    True,
    WindowResizeAction, 
    { NULL }, 
    ACTION_RESIZE,
};

static Button
propertiesButton = {
    { NULL, NULL },
    { "window:Properties", NULL },
    0,
    False, 
    False,
    True,
    WindowPropsAction, 
    { NULL }, 
    ACTION_PROPS,
};

static Button
backButton = {
    { NULL, NULL },
    { "window:Back", NULL },
    0,
    False, 
    True,
    True,
    WindowBackAction, 
    { NULL }, 
    ACTION_BACK,
};

static Button
refreshButton = {
    { NULL, NULL },
    { "window:Refresh", NULL },
    0,
    False, 
    True,
    True,
    WindowRefreshAction, 
    { NULL }, 
    ACTION_REFRESH,
};

static Button
quitButton = {
    { NULL, NULL },
    { "window:Quit", NULL },
    0, 
    False, 
    True,
    True,
    WindowQuitAction, 
    { NULL }, 
    ACTION_QUIT,
};

static Button
dismissButton = {
    { NULL, NULL },
    { "window:Dismiss", "window:Dismiss" }, /* REMIND "window:Cancel" ? */
    0,
    False,
    True,
    True,
    NULL,
    { NULL },
    ACTION_NONE,
};

static Button 
dismissThisButton = {
    { NULL, NULL },
    { "window:DismissThis", NULL },
    0,
    False, 
    True,
    True,
    WindowDismissThisAction, 
    { NULL }, 
    ACTION_OPEN_CLOSE,
};

static Button
dismissAllButton = {
    { NULL, NULL },
    { "window:DismissAll", NULL },
    0,
    False, 
    True,
    True,
    WindowDismissAllAction, 
    { NULL }, 
    ACTION_NONE,
};

static Button
ownerButton = {
    { NULL, NULL },
    { "window:Owner", NULL },
    0,
    False, 
    True,
    True,
    WindowFlashOwnerAction,
    { NULL }, 
    ACTION_OWNER,
};

#else
static Button openButton;
static Button fullSizeButton;
static Button moveButton;
static Button resizeButton;
static Button propertiesButton;
static Button backButton;
static Button refreshButton;
static Button quitButton;
static Button dismissButton;
static Button dismissThisButton;
static Button dismissAllButton;
static Button ownerButton;
#endif /* __STDC__ */

/*
 *	Actual frame/icon menus using shared buttons
 */

static Button *windowMenuFullButtons[] = {
    &openButton,
    &fullSizeButton,
    &moveButton,
    &resizeButton,
    &propertiesButton,
    &backButton,
    &refreshButton,
    &quitButton
};

static Button *windowMenuDismissButtons[] = {
    &dismissThisButton,
    &dismissAllButton,
};

static Button *windowMenuLimitedButtons[] = {
    &dismissButton,
    &moveButton,
    &resizeButton,
    &backButton,
    &refreshButton,
    &ownerButton,
};


/*****************************************************************************/


void
SetWindowMenuLabels()
{
	if (windowTitle)
		FreeText(windowTitle);
	windowTitle = GetText("Window");

	if (openButton.label[0])
		FreeText(openButton.label[0]);
	openButton.label[0] = GetText("Open");

	if (openButton.label[1])
		FreeText(openButton.label[1]);
	openButton.label[1] = GetText("Close");

	if (fullSizeButton.label[0])
		FreeText(fullSizeButton.label[0]);
	fullSizeButton.label[0] = GetText("Full Size");

	if (fullSizeButton.label[1])
		FreeText(fullSizeButton.label[1]);
	fullSizeButton.label[1] = GetText("Restore Size");

	if (moveButton.label[0])
		FreeText(moveButton.label[0]);
	moveButton.label[0] = GetText("Move");

	if (resizeButton.label[0])
		FreeText(resizeButton.label[0]);
	resizeButton.label[0] = GetText("Resize");

	if (propertiesButton.label[0])
		FreeText(propertiesButton.label[0]);
	propertiesButton.label[0] = GetText("Properties");

	if (backButton.label[0])
		FreeText(backButton.label[0]);
	backButton.label[0] = GetText("Back");

	if (refreshButton.label[0])
		FreeText(refreshButton.label[0]);
	refreshButton.label[0] = GetText("Refresh");

	if (quitButton.label[0])
		FreeText(quitButton.label[0]);
	quitButton.label[0] = GetText("Quit");

	if (dismissButton.label[0])
		FreeText(dismissButton.label[0]);
	dismissButton.label[0] = GetText("Dismiss");

	if (dismissButton.label[1])
		FreeText(dismissButton.label[1]);
	dismissButton.label[1] = GetText("Cancel");

	if (dismissAllButton.label[0])
		FreeText(dismissAllButton.label[0]);
	dismissAllButton.label[0] = GetText("All Pop-ups");

	if (dismissThisButton.label[0])
		FreeText(dismissThisButton.label[0]);
	dismissThisButton.label[0] = GetText("This Window");

	if (ownerButton.label[0])
		FreeText(ownerButton.label[0]);
	ownerButton.label[0] = GetText("Owner?");
}


SetWindowMenuTitle()
{
    MenuTable[MENU_FULL]->title = windowTitle;
    MenuTable[MENU_LIMITED]->title = windowTitle;
    MenuTable[MENU_LIMITED]->buttons[0]->action.submenu->title = windowTitle;
}

#ifndef __STDC__
/*
 * InitButton	-- initalizes button struct
 * This was added specifically for the MIT release, due to the fact
 * that initialization of union structures is not allowed by /bin/cc
 * on SunOS4.x.
 */
void
InitButton(button, help1, help2, which, has_submenu, 
		enabled, visible, callback, semantic)
    Button    *button;
    char	*help1;
    char	*help2;
    int		which;
    Bool	has_submenu;
    Bool	enabled;
    Bool	visible;
    FuncPtr	callback;
    SemanticAction	semantic;
{
    button->label[0] = NULL;
    button->label[1] = NULL;

    button->helpstring[0] = help1 ? strdup(help1) : (char *)NULL;
    button->helpstring[1] = help2 ? strdup(help2) : (char *)NULL;

    button->which = which;

    button->has_submenu = has_submenu;

    button->enabled = enabled;

    button->visible = visible;

    button->callback = callback;

    button->action.other = (void *)NULL;

    button->semantic = semantic;
}

/*
 * InitAllButtons-- initalizes all relevant button structs
 * This was added specifically for the MIT release, due to the fact
 * that initialization of union structures is not allowed by /bin/cc
 * on SunOS4.x.
 */
void
InitAllButtons()
{
    InitButton(&openButton, 
		"window:Open", "window:Close", 
		0, 
		False, 
		True, 
		True, 
		WindowOpenCloseAction, 
		ACTION_OPEN_CLOSE);

    InitButton(&fullSizeButton, 
		"window:FullSize", "window:RestoreSize", 
		0, 
		False, 
		True, 
		True, 
		WindowFullRestoreSizeAction, 
		ACTION_FULL_RESTORE);

    InitButton(&moveButton, 
		"window:Move", NULL, 
		0, 
		False, 
		True, 
		True, 
		WindowMoveAction, 
		ACTION_MOVE);

    InitButton(&resizeButton, 
		"window:Resize", NULL, 
		0, 
		False, 
		True, 
		True, 
		WindowResizeAction, 
		ACTION_RESIZE);

    InitButton(&propertiesButton, 
		"window:Properties", NULL, 
		0, 
		False, 
		False, 
		True, 
		WindowPropsAction, 
		ACTION_PROPS);

    InitButton(&backButton, 
		"window:Back", NULL, 
		0, 
		False, 
		True, 
		True, 
		WindowBackAction, 
		ACTION_BACK);

    InitButton(&refreshButton, 
		"window:Refresh", NULL, 
		0, 
		False, 
		True, 
		True, 
		WindowRefreshAction, 
		ACTION_REFRESH);

    InitButton(&quitButton, 
		"window:Quit", NULL, 
		0, 
		False, 
		True, 
		True, 
		WindowQuitAction, 
		ACTION_QUIT);

    InitButton(&dismissButton, 
		"window:Dismiss", "window:Dismiss", 
		0, 
		False, 
		True, 
		True, 
		NULL, 
		ACTION_NONE);

    InitButton(&dismissThisButton, 
		"window:DismissThis", NULL,
		0, 
		False, 
		True, 
		True, 
		WindowDismissThisAction, 
		ACTION_OPEN_CLOSE);

    InitButton(&dismissAllButton, 
		"window:DismissAll", NULL,
		0, 
		False, 
		True, 
		True, 
		WindowDismissAllAction, 
		ACTION_NONE);

    InitButton(&ownerButton, 
		"window:Owner", NULL,
		0, 
		False, 
		True, 
		True, 
		WindowFlashOwnerAction, 
		ACTION_OWNER);
}
#endif /* __STDC__ */

/*
 * InitMenus	-- Creates the built-in screen-independent menus
 */
void
InitMenus(dpy)
    Display    *dpy;
{
#ifndef __STDC__
    static int	init_all_buttons_done = False;
    /*
     * Initialize all buttons
     * This was added specifically for the MIT release to fix 
     * a problem where union initializing was not allowed in
     * SunOS4.x using /bin/cc
     */
    if (!init_all_buttons_done)  {
        InitAllButtons();
	init_all_buttons_done = True;
    }
#endif /* __STDC__ */

    SetWindowMenuLabels();

    MenuTable[MENU_FULL] = CreateMenu(windowTitle, windowMenuFullButtons, 
		COUNT(windowMenuFullButtons), False, windowMenuHelpString);
    MenuTable[MENU_LIMITED] = CreateMenu(windowTitle, windowMenuLimitedButtons, 
		COUNT(windowMenuLimitedButtons), False, windowMenuHelpString);

    SetMenuHier(MenuTable[MENU_LIMITED], popup_dismissitem,
		CreateMenu(windowTitle, windowMenuDismissButtons, 
		COUNT(windowMenuDismissButtons), False, windowMenuHelpString));

    /* this sets ROOT_MENU */
    InitUserMenu(dpy);
}



/*
 * CreateWindowMenuInfo
 *
 *	Assumes that Destroy called before Create.
 *	Assumes that the window menus will take up the first 6 slots
 */
int
CreateWindowMenuInfo(dpy, scrInfo)
    Display    *dpy;
    ScreenInfo *scrInfo;
{
    int         origNextSlot = scrInfo->menuCache->nextSlot;

    int j, x;

    scrInfo->menuCache->nextSlot = 0;

    (void) MenuInfoCreate(scrInfo->menuCache, scrInfo->rootwin, MenuTable[MENU_FULL], 1);
    (void) MenuInfoCreate(scrInfo->menuCache, scrInfo->rootwin, MenuTable[MENU_LIMITED], 1);

    scrInfo->menuCache->nextSlot = origNextSlot;
}


/*****************************************************************************/


/*
 * firstEnabledItem -- returns the index of the first button of a menu that is
 * both enabled and visible.
 */
static int
firstEnabledItem(menu)
    Menu *menu;
{
    int i;

    for (i = 0;  i < menu->buttonCount;  i++)
	if (menu->buttons[i]->enabled && menu->buttons[i]->visible)
	    return i;

    if (menu->hasPushPin)
	return PINBUTTON;

    return NOBUTTON;	/*can't do anything else...*/
}


Menu *
GetEnabledMenu(cli, flfull, flnotitle)
    Client *cli;
    Bool flfull;
    Bool flnotitle;
{
    WMDecorations *decor = cli->wmDecors;
    Menu *menu;
    static Bool lastmouseless = True;
    Bool flmouseless = ! (GRV.Mouseless == KbdSunView || mouselessSuspended);
    Bool flicon = cli->wmState != NormalState;
    Bool flresizable = decor->flags & WMDecorationResizeable;
    Bool flcancel = decor->cancel;

    if (flmouseless != lastmouseless) {
	moveButton.visible = flmouseless;
	resizeButton.visible = flmouseless;
	DirtyMenu(MenuTable[MENU_FULL]);
	DirtyMenu(MenuTable[MENU_LIMITED]);
	lastmouseless = flmouseless;
    }

    switch (decor->menu_type) {
      case MENU_FULL:
	menu = MenuTable[MENU_FULL];
	ToggleEnabled(menu, basewin_resizeitem, flresizable && ! flicon);
	ToggleEnabled(menu, basewin_zoomitem, flresizable);
	ToggleItem(menu, basewin_openitem, ! flicon);
	ToggleItem(menu, basewin_zoomitem, flfull);
	break;
      case MENU_LIMITED:
	menu = MenuTable[MENU_LIMITED];
	ToggleEnabled(menu, popup_resizeitem, flresizable && ! flicon);
	ToggleItem(menu, popup_dismissitem, flcancel);
	break;
      default:
	menu = NULL;
	break;
    }
    if (menu) {
	if (cli->menuAccelerators != menu->wantAccelerators) {
	    menu->wantAccelerators = cli->menuAccelerators;
	    DirtyMenu(menu);
	}

	if (menu->buttons[decor->def_item]->visible)
	    menu->buttonDefault = decor->def_item;
	else
	    menu->buttonDefault = firstEnabledItem(menu);

	if (flnotitle)
	    SetMenuTitle(menu, NULL);
	else
	    SetMenuTitle(menu, windowTitle);
    }
    return menu;
}

struct _setdefinfo {
    WinGenericFrame *win;
    Menu *menu;
    void (*proc)();
    void *data;
#ifdef DEBUG
    Bool flinuse;
#endif
};

static void
setFrameDefault(sdi)
    struct _setdefinfo *sdi;
{
    sdi->win->core.client->wmDecors->def_item = sdi->menu->buttonDefault;
    if (sdi->proc) {
	(*sdi->proc)(SYNC_DONE, 0, sdi->data);
	SetClickCallback(NULL, NULL);
    }
#ifdef DEBUG
    sdi->flinuse = False;
#endif
}

static void
doClickCallback(clickmode, sdi)
    MenuTrackMode clickmode;
    struct _setdefinfo *sdi;
{
    (*sdi->proc)(SYNC_CHANGECLICK, clickmode, sdi->data);
}


/*
 * ShowStandardMenuSync
 * 
 * Assemble a menu and show it from one of the base types.
 * If the menu came by hitting MENU on a button, pass flbutton.
 */
void
ShowStandardMenuSync(win, eve, flbutton, proc, data)
    WinGenericFrame *win;
    XEvent *eve;
    Bool flbutton;
    void (*proc)();
    void *data;
{
    static struct _setdefinfo sdi;

#ifdef DEBUG
    if (sdi.flinuse)
	fprintf(stderr, "showstandardmenusync: stranding defitem!\n");
    sdi.flinuse = True;
#endif
    sdi.menu = GetEnabledMenu(win->core.client, win->fcore.fullsize, flbutton);
    sdi.win = win;
    sdi.proc = proc;
    sdi.data = data;

    if (proc != NULL) {
	SetClickCallback(doClickCallback, &sdi);
    }
    MenuShowSync(win->core.client->dpy, win, sdi.menu, eve, setFrameDefault, &sdi, 
		 (eve->type == KeyPress) || (eve->type == KeyRelease),
		 flbutton);
}


/*
 * ShowStandardMenu
 *
 * Convenience function for showing a standard menu when no sync function is 
 * required.
 */
void
ShowStandardMenu(win, eve, flbutton)
    WinGenericFrame *win;
    XEvent *eve;
    Bool flbutton;
{
    ShowStandardMenuSync(win, eve, flbutton, NULL, NULL);
}
