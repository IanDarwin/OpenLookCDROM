
# line 7 "olvwmrc.y"
#ifdef IDENT
#ident  "@(#)olvwmrc.y	1.6 olvwm version 07 Jan 1994"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "i18n.h"

#include "olwm.h"
#include "ollocale.h"
#include "list.h"
#include "mem.h"
#include "win.h"
#include "menu.h"
#include "kbdfuncs.h"
#include "globals.h"
#include "virtual.h"

#define OLVWM_USE_SELECTION	"OLVWM_USE_SELECTION"

extern CheckForKeyProg();
char	*LookupToken();
char	*FindOlvwmRC();
void	resetKeys();
char	*strexpand();

typedef struct progscreen {
    char        *target;
    int         screen;
} ProgScreen;

typedef struct assignment {
    char	*name;
    char	*value;
} Variable;
static Variable *varMatch();

typedef enum { Warp, Open, Raise, Execute, Goto, Close, Quit,
		RaiseLower, Lower, Geometry, Rebind, Stick, SetSize,
		Focus, IfElse } Action;

typedef struct progkeynode {
    Action	action;
    char	*parameter;
} ProgKeyNode;

typedef struct progkey {
    int         modmask;
    KeyCode     keycode;
    List	*todo;
} ProgKey;

typedef struct ifelsestruct {
    char	*identifier;
    List	*doIf, *doElse;
} IfElseStruct;

typedef struct winmenuactions {
    char	*key;
    List	*actions;
} WinMenuActions;

List    *ProgScreenList = NULL;
List	*VariableList = NULL;
List    *ProgKeyList = NULL;
List	*WinMenuActionsList = NULL;

static Display	*dpy;

# line 89 "olvwmrc.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
 {
    int		ival;
    void	*sval;
} YYSTYPE;
# define WARP 257
# define OPEN 258
# define RAISE 259
# define CLOSE 260
# define QUIT 261
# define EXECUTE 262
# define GOTO 263
# define MODIFIER 264
# define WORD 265
# define COLON 266
# define OPENBRACE 267
# define CLOSEBRACE 268
# define COMMA 269
# define SCREEN 270
# define INT 271
# define STARTSQUOTE 272
# define STARTDQUOTE 273
# define ENDSQUOTE 274
# define ENDDQUOTE 275
# define WINMENU 276
# define PLUS 277
# define RAISELOWER 278
# define LOWER 279
# define GEOMETRY 280
# define REBIND 281
# define STICK 282
# define SETSIZE 283
# define FOCUS 284
# define EQUALS 285
# define IFELSE 286

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 527 "olvwmrc.y"

/* Programs */
#define YYDEBUG 1
#include "parse.c"

extern List	*ActiveClientList;
extern Client	*CurrentClient;

yyerror(s)
    char	*s;

{
char	msg[256];

    sprintf(msg, gettext("Syntax error near %s in .olvwmrc -- entry ignored\n"),
		yytext);
    ErrorWarning(msg);
}

static int
getname(s, name)
char **s;
char   name[];
{
   int  i = 0;
   char look = '\0';

   if (**s == '{') {
      (*s)++;
      look = '}';
   }

   while (**s && (isalnum(**s) || **s == '_')) {
      name[i++] = **s;
      (*s)++;
   }
   name[i] = '\0';

   if (**s != look) {
       (*s)--;
   }
   if (look && (**s != look)) {
       char msg[256];

       sprintf(msg,
	       gettext("Unterminated variable reference for '%s'.\n"), name);
       ErrorWarning(msg);
       return(False);
   }

   return(True);
}

static char *
strexpand(s)
char *s;
{
   int   length = 0;
   char *c;
   char *t;
   char *string;
   char  name[256];
   char  msg[256];

   /*
    *  Calculate length of fully expanded string.
    */
   for (t = s; *t; t++) {
       if (*t == '%') {
	   t++;

	   if (*t == '%')
	       length++;
	   else {
	       Variable *v;

	       if (getname(&t, name)) {
	           v = ListApply(VariableList, varMatch, name);
	           if (v == NULL) {
		       sprintf(msg,
			   gettext("Reference to undefined variable '%s' ignored.\n"),
		           name);
		       ErrorWarning(msg);
	           }
		   else {
		       length += strlen(v->value);
		   }
	       }
	   }
       }
       else if (*t == '$') {
	   t++;

	   if (*t == '$') {
	       length++;
	   }
	   else {
	       char *v;

	       if (getname(&t, name)) {
	           v = getenv(name);
	           if (v != NULL) {
		       length += strlen(v);
		   }
	       }
	   }
       }
       else {
 	   length++;
       }
   }

   /*
    *  Allocate and construct fully expanded string.
    */
   c = string = MemAlloc(length + 1);

   for (t = s; *t; t++) {
       if (*t == '%') {
	   t++;

	   if (*t == '%') {
 	       *c = *t;
	       c++;
	   }
	   else {
	       Variable *v;

	       if (getname(&t, name)) {
	           v = ListApply(VariableList, varMatch, name);
	           if (v == NULL) {
		       sprintf(msg,
			   gettext("Reference to undefined variable '%s' ignored.\n"),
		           name);
		       ErrorWarning(msg);
	           }
		   else {
		       strcpy(c, v->value);
		       c += strlen(v->value);
		   }
	       }
	   }
       }
       else if (*t == '$') {
	   t++;

	   if (*t == '$') {
 	       *c = *t;
	       c++;
	   }
	   else {
	       char *v;

	       if (getname(&t, name)) {
	           v = getenv(name);
	           if (v != NULL) {
		       strcpy(c, v);
		       c += strlen(v);
		   }
	       }
	   }
       }
       else {
 	   *c = *t;
	   c++;
       }
   }
   *c = '\0';
   return(string);
}

static Variable *
varMatch(var, name)
Variable *var;
char     *name;
{
    if (strcmp(var->name, name) == 0)
      return(var);
    return((Variable *) NULL);
}

static ProgKey	*
matchProgKey(p, ev)
    ProgKey	*p;
    XEvent	*ev;

{
    if (p->keycode == ev->xkey.keycode &&
	(p->modmask == AnyModifier || p->modmask == ev->xkey.state))
	return p;
    return NULL;
}

static ProgScreen *
matchProgString(p, s)
    ProgScreen	*p;
    char	*s;
{
char	*t, *t1;

    if (!s)
	return NULL;
    t1= strdup(p->target);
    t = LookupToken(t1, ",");
    while (t) {
        if (!strncmp(t, s, strlen(t))) {
	    free(t1);
	    return p;
	}
	t = LookupToken(NULL, ",");
    }
    free(t1);
    return NULL;
}

static int findClient_rootid;

static Client	*
findClient(c, s)
    Client	*c;
    char	*s;
{
    if (findClient_rootid && findClient_rootid != c->scrInfo->rootid)
	return NULL;
    if (c->framewin && c->framewin->fcore.name)
        if (!strncmp(c->framewin->fcore.name, s, strlen(s)))
	    return c;
    if (c->wmClass)
        if (!strcmp(c->wmClass, s))
	    return c;
    if (c->wmInstance)
        if (!strcmp(c->wmInstance, s))
	    return c;
    return NULL;
}

static int applyIsKey = False;
static int         rebind = False;
static char       *rebindFile = NULL;
static WinGeneric *CurrentScreenClient = NULL;
static Client     *FocusClient = NULL;

static void clientWarp();
static void clientSaveFocus();
static void clientRestoreFocus();

static int
applyAction(p, cli)
    ProgKeyNode	*p;
    Client	*cli;

{
Client	*c;
char	*s, *t;
char	**env;
List	*l;
struct stat statbuf;

    switch(p->action) {
	case Warp:
	    /*
	     * We only allow one lookup per warp, but we can't use p.parameter
	     * directly, since it may contain special characters which
	     * LookupToken will remove
	     */
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    if (!strcmp(t, OLVWM_USE_SELECTION)) {
		if (cli && cli->framewin)
		    clientWarp(cli);
	    }
	    else {
		    extern List *ScreenInfoList;
		    List *l = ScreenInfoList;
		    ScreenInfo *scr;

		    /* allow one warp per screen */
		    for (scr = ListEnum(&l); scr != NULL; scr = ListEnum(&l)) {
			findClient_rootid = scr->rootid;
			c = (Client *) ListApply(ActiveClientList, findClient, t);
			if (c)
			    clientWarp(c);
		    }
		    findClient_rootid = 0;
		}
	    free(s);
	    break;

	case Open:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin)
			StateNormal(cli, TimeFresh());
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t) && c->wmState != NormalState)
		    	    StateNormal(c, TimeFresh());
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Close:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin)
			StateIconic(cli, TimeFresh());
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t) && c->wmState != IconicState)
		    	    StateIconic(c, TimeFresh());
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Quit:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin)
		        ClientKill(cli, True);
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    ClientKill(c, True);
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Raise:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin) {
		        if (cli->wmState == IconicState)
			    RaiseWindow(cli->iconwin);
		        else if (cli->wmState == NormalState)
			    RaiseWindow(cli->framewin);
		    }
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    if (c->wmState == IconicState)
			        RaiseWindow(c->iconwin);
			    else if (c->wmState == NormalState)
				RaiseWindow(c->framewin);
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case RaiseLower:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin)
		        ClientToggleStacking(cli);
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    ClientToggleStacking(c);
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Lower:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (!strcmp(t, OLVWM_USE_SELECTION)) {
		    if (cli && cli->framewin) {
		        if (cli->wmState == IconicState)
			    LowerWindow(cli->iconwin);
		        else if (cli->wmState == NormalState)
			    LowerWindow(cli->framewin);
		    }
		}
		else {
	            l = ActiveClientList;
		    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
			if (findClient(c, t))
			    if (c->wmState == IconicState)
			        LowerWindow(c->iconwin);
			    else if (c->wmState == NormalState)
				LowerWindow(c->framewin);
		}
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;

	case Execute:
	    s = strdup(p->parameter);
	    t = LookupToken(s, ",");
	    while (t) {
		if (cli && cli->scrInfo)
	            ExecCommand(cli->scrInfo->environment, t);
		else ExecCommand(NULL, t);
		t = LookupToken(NULL, ",");
	    }
	    free(s);
	    break;
	
	case Goto:
	    if (cli && cli->scrInfo)
	        VDMGoto(dpy, cli, atoi(p->parameter));
	    break;

	case Geometry:
	    /*
	     *  If there is a client & it is resizable.
	     */
	    if (cli && cli->wmDecors &&
			(cli->wmDecors->flags & WMDecorationResizeable)) {
		int       r;
		int       x, y;
		unsigned int w, h;
		ScreenInfo	*scrInfo = cli->scrInfo;

		clientSaveFocus(cli);
		r = XParseGeometry(p->parameter, &x, &y, &w, &h);

		/*
		 *  Adjust dimensions by border size.
		 */
		if (r & WidthValue) {
		    w += 2 * cli->framewin->fcore.panewin->core.x;
		}
		else w = cli->framewin->core.width;

		if (r & HeightValue) {
		    h += cli->framewin->fcore.panewin->core.y +
				cli->framewin->fcore.panewin->core.x;
		}
		else h = cli->framewin->core.height;

		if (r & XNegative)
		    x = DisplayWidth(cli->dpy, scrInfo->screen) +
				x - w + cli->framewin->fcore.panewin->core.x;
		else if (!(r & XValue))
		    x = cli->framewin->core.x;

		if (r & YNegative)
		    y = DisplayHeight(cli->dpy, scrInfo->screen) +
				y - h + cli->framewin->fcore.panewin->core.x;
		else if (!(r & YValue))
		    y = cli->framewin->core.y;

		GFrameSetConfig(cli->framewin, x, y, w, h);

		clientRestoreFocus();
	    }
	    break;

	case Rebind:
	    rebindFile = p->parameter;
	    rebind = True;

	    /*
	     *  Don't rebind if a bindings file was specified & doesn't exist.
	     */
	    if ((p->parameter != NULL) && (stat(p->parameter, &statbuf) != 0)) {
	        rebind = False;
	    }
	    break;

	case Stick:
	    if (!p->parameter) {
		if (cli && cli->framewin) {
		    MakeSticky(cli, ! cli->sticky);
		}
	    }
	    else {
		if (!strcmp(p->parameter, "on")) {
		    if (cli && cli->framewin) {
		        MakeSticky(cli, True);
		    }
		}
		else if (!strcmp(p->parameter, "off")) {
		    if (cli && cli->framewin) {
		        MakeSticky(cli, False);
		    }
		}
		else if (!strcmp(p->parameter, "toggle")) {
		    if (cli && cli->framewin) {
		        MakeSticky(cli, ! cli->sticky);
		    }
		}
		else {
		    s = strdup(p->parameter);
		    t = LookupToken(s, ",");
		    while (t) {
		        l = ActiveClientList;
		        if (!strcmp(t, OLVWM_USE_SELECTION)) {
		            if (cli && cli->framewin) {
			        MakeSticky(cli, ! cli->sticky);
			    }
		        }
		        else for (c = ListEnum(&l); c != NULL;
					c = ListEnum(&l))
				if (findClient(c, t))
				    MakeSticky(c, ! c->sticky);
		        t = LookupToken(NULL, ",");
		    }
		    free(s);
		}
	    }
	    break;

	case SetSize:
	    if (! p->parameter) {
		if (cli && cli->framewin) {
		    ClientFullRestoreSizeToggle(cli, TimeFresh());
		}
	    }
	    else {
		if (!strcmp(p->parameter, "full")) {
		    if (cli && cli->framewin && ! cli->framewin->fcore.fullsize)
		        ClientFullRestoreSizeToggle(cli, TimeFresh());
		}
		else if (!strcmp(p->parameter, "save") ||
		         !strcmp(p->parameter, "store")) {
		    if (cli && cli->framewin &&
				(!strcmp(p->parameter, "store") ||
			         !cli->framewin->fcore.fullsize)) {
	                WinPaneFrame  *frameInfo = cli->framewin;

			frameInfo->restoreWidth = frameInfo->core.width -
		               (2 * frameInfo->fcore.panewin->core.x);
			frameInfo->restoreHeight = frameInfo->core.height -
		               (frameInfo->fcore.panewin->core.y +
				cli->framewin->fcore.panewin->core.x);
			frameInfo->restoreY = frameInfo->core.y;
			frameInfo->restoreX = frameInfo->core.x;

			/*
			 *  Make sure Restore/Full size menu item is correct
			 *  for SetSize: store operation.
			 */
		        if (!strcmp(p->parameter, "store"))
			{
			    frameInfo->restoreSet = False;
			    cli->framewin->fcore.fullsize = False;
			}
			else
			{
			    frameInfo->restoreSet = True;
			    cli->framewin->fcore.fullsize = True;
			}

			cli->framewin->fcore.fullsize = True;
		    }
		}
		else if (!strcmp(p->parameter, "restore")) {
		    if (cli && cli->framewin && cli->framewin->fcore.fullsize)
		        ClientFullRestoreSizeToggle(cli, TimeFresh());
		}
		else if (cli && cli->framewin && !strcmp(p->parameter, "toggle")) {
		    ClientFullRestoreSizeToggle(cli, TimeFresh());
		}
		else {
		    s = strdup(p->parameter);
		    t = LookupToken(s, ",");
		    while (t) {
			l = ActiveClientList;
			if (!strcmp(t, OLVWM_USE_SELECTION)) {
			    if (cli && cli->framewin) {
			        ClientFullRestoreSizeToggle(cli, TimeFresh());
			    }
			}
			else for (c = ListEnum(&l); c != NULL; c = ListEnum(&l))
				if (findClient(c, t))
				    ClientFullRestoreSizeToggle(c, TimeFresh());
			t = LookupToken(NULL, ",");
		    }
		    free(s);
		}
	    }
	    break;

	case Focus:
	    if (!strcmp(p->parameter, "save")) {
		clientSaveFocus(cli);
	    }
	    else if (!strcmp(p->parameter, "restore")) {
		clientRestoreFocus();
	    }
	    break;
	
	case IfElse: {
	    IfElseStruct	*ie;

	    ie = (IfElseStruct *) p->parameter;
	    l = ActiveClientList;
	    for (c = ListEnum(&l); c != NULL; c = ListEnum(&l)) {
		if (findClient(c, ie->identifier)) {
		    ListApply(ie->doIf, applyAction, c);
		    return;
		}
	    }
	    ListApply(ie->doElse, applyAction, cli);
	}

    }
    return False;
}

static void
clientWarp(c)
    Client	*c;

{
int	rootX, rootY, winX, winY;
unsigned int keys;
Window  root, child;

    VDMMoveTo(dpy, c, c->framewin->core.x, c->framewin->core.y);
    /*
     * If we got here via a key, move the mouse to the window and
     * give it focus
     */
    if (applyIsKey) {
	XWarpPointer(dpy, None, c->framewin->core.self, 0, 0, 0, 0,
			 c->framewin->core.width / 2,
			 c->framewin->core.height / 2);
	
    }
    if (!GRV.FocusFollowsMouse)
	ClientSetFocus(c, True, CurrentTime);
}

static void
clientSaveFocus(cli)
    Client	*cli;
{
    if (cli) {
	FocusClient = cli;
    }
}

static void
clientRestoreFocus()
{
    if (FocusClient) {
	/*
	 *  Set focus to FocusClient, notify client of focus,
	 *  set current client to FocusClient, reset FocusClient.
	 */
	ClientSetFocus(FocusClient, True, CurrentTime);
	ClientSetCurrent(FocusClient);

	FocusClient = NULL;
    }
}

char *
FindOlvwmRC(buf)
    struct stat	*buf;
{
char	*path;
static char	s[256];
extern char	*getenv();
struct stat	tmp;

    if (buf == NULL)
	buf = &tmp;

    /* try reading OLVWMRC */
    path = getenv("OLVWMRC");
    if (path && stat(path, buf) == 0)
	return path;

    /* try reading current directory */
    sprintf(s, ".olvwmrc");
    if (stat(s, buf) == 0)
	return s;

    if ((path = getenv("HOME")) != NULL) {
        sprintf(s, "%s/.olvwmrc", getenv("HOME"));
	if (stat(s, buf) == 0)
	    return s;
    }
    return NULL;
}

static struct stat olvwmBuf;
static struct stat olvwmOldBuf;

InitOlvwmRC(ldpy, path)
    Display	*ldpy;
    char	*path;

{

    if (path == NULL)
	path = FindOlvwmRC(&olvwmBuf);
    if (path == NULL || (yyin = fopen(path, "r")) == NULL)
	return;
    olvwmOldBuf = olvwmBuf;
    dpy = ldpy;
    yyparse();
#ifdef DEBUG
    DumpProgKeyList();
    DumpScreenList();
    DumpWinMenuList();
#endif
    fclose(yyin);
}

/*
 * Check to see if olvwmrc has been changed since we last read it
 */
CheckOlvwmRC(dpy)
    Display *dpy;
{
char *p = FindOlvwmRC(&olvwmBuf);
extern XrmDatabase OlwmDB;

    if (!p)
	return;
    if (olvwmBuf.st_mtime != olvwmOldBuf.st_mtime
          || olvwmBuf.st_size != olvwmOldBuf.st_size
          || olvwmBuf.st_dev != olvwmOldBuf.st_dev
          || olvwmBuf.st_ino != olvwmOldBuf.st_ino) {
      RefreshKeyGrabs(dpy, OlwmDB);
      olvwmOldBuf = olvwmBuf;
    }
}

DestroyVariable(p)
    Variable	*p;
{
    MemFree(p->name);
    MemFree(p->value);
    return False;
}

DestroyProgScreen(p)
    ProgScreen	*p;
{
    MemFree(p->target);
    return False;
}

DestroyProgKeyNode(p)
    ProgKeyNode	*p;
{
    if (p->action == IfElse) {
	IfElseStruct *ie;

	ie = (IfElseStruct *) p->parameter;
	MemFree(ie->identifier);
	ListApply(ie->doIf, DestroyProgKeyNode, 0);
	ListApply(ie->doElse, DestroyProgKeyNode, 0);
    }
    else MemFree(p->parameter);
    return False;
}

DestroyProgKey(p)
    ProgKey	*p;
{
    ListApply(p->todo, DestroyProgKeyNode, 0);
    ListDestroy(p->todo);
    return False;
}

DestroyWinMenuActions(p)
    WinMenuActions	*p;
{
    ListApply(p->actions, DestroyProgKeyNode, 0);
    ListDestroy(p->actions);
    return False;
}

ReInitOlvwmRC(ldpy, path)
    Display	*ldpy;
    char	*path;

{
    ListApply(ProgScreenList, DestroyProgScreen, 0);
    ListDestroy(ProgScreenList);
    ListApply(ProgKeyList, DestroyProgKey, 0);
    ListDestroy(ProgKeyList);
    ListApply(WinMenuActionsList, DestroyWinMenuActions, 0);
    ListDestroy(WinMenuActionsList);
    ListApply(VariableList, DestroyVariable, 0);
    ListDestroy(VariableList);
    ProgScreenList = NULL;
    ProgKeyList = NULL;
    WinMenuActionsList = NULL;
    VariableList = NULL;
    InitOlvwmRC(ldpy, path);
}

SearchProgString(dpy, scrInfo, name, inst, wm_class,
		 frame_x, frame_y, icon_x, icon_y)
    Display	*dpy;
    ScreenInfo	*scrInfo;
    char	*name, *inst, *wm_class;
    int		*frame_x, *frame_y, *icon_x, *icon_y;
{
ProgScreen	*p = NULL;
int		dw = DisplayWidth(dpy, scrInfo->screen);
int		dh = DisplayHeight(dpy, scrInfo->screen);

    if (name)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, name);
    if (!p && inst)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, inst);
    if (!p && wm_class)
       p = (ProgScreen *) ListApply(ProgScreenList, matchProgString, wm_class);
    if (p) {
	*frame_x = (*frame_x % dw) + dw * (p->screen % scrInfo->vdm->columns) +
				scrInfo->vdm->offsetX;
	*icon_x = (*icon_x % dw) + dw * (p->screen % scrInfo->vdm->columns) +
				scrInfo->vdm->offsetX;
	*frame_y = (*frame_y % dh) + dh * (p->screen / scrInfo->vdm->columns) +
				scrInfo->vdm->offsetY;
	*icon_y = (*icon_y % dh) + dh * (p->screen / scrInfo->vdm->columns) +
				scrInfo->vdm->offsetY;
    }
}

/* ARGSUSED */
CheckForKeyProg(dpy, ev)
    Display	*dpy;
    XEvent	*ev;
{
ProgKey	*p;
WinGeneric	*win;
extern XrmDatabase OlwmDB;

    p = (ProgKey *) ListApply(ProgKeyList, matchProgKey, ev);
    if (!p)
	return False;
    if (ev->xkey.type != KeyPress)
	return False;
    applyIsKey = True;
    ListApply(p->todo, applyAction, CurrentClient);

    if (rebind) {
	RefreshKeyGrabsFile(dpy, OlwmDB, rebindFile);
	rebind = False;
	rebindFile = NULL;
    }
    return True;
}

DumpProgKeyNode(n)
    ProgKeyNode	*n;
{
    printf(gettext("Action %d parameter %s\n"), n->action, n->parameter);
    return False;
}

DumpProgKey(p)
    ProgKey	*p;
{
    printf(gettext("Actions for key %d mask %x\n"), p->keycode, p->modmask);
    ListApply(p->todo, DumpProgKeyNode, 0);
    return False;
}

DumpProgKeyList()
{
    ListApply(ProgKeyList, DumpProgKey, 0);
}

DumpProgScreen(p)
    ProgScreen	*p;
{
    printf(gettext("Screen %d:  %s\n"), p->screen, p->target);
    return False;
}

DumpWinMenu(p)
    WinMenuActions	*p;
{
    printf(gettext("Menu key %s\n"), p->key);
    ListApply(p->actions, DumpProgKeyNode, 0);
    return False;
}

DumpScreenList()
{
    ListApply(ProgScreenList, DumpProgScreen, 0);
}

DumpWinMenuList()
{
    ListApply(WinMenuActionsList, DumpWinMenu, 0);
}

char *
LookupToken(src, delim)
    char	*src;
    char	*delim;

{
static char	last[128], *next, *final;
char	*s;
int	idx = 0;

    if (src) {
	next = src;
	final = src + strlen(src);
    }
    if (next > final)
	return NULL;
    s = next;
    while (*s && !strchr(delim, *s)) {
	if (*s == '\\')
	    s++;
	else if (*s == '\"') {
	    last[idx++] = *s++;
	    while (*s && *s != '\"')
		last[idx++] = *s++;
	}
	else if (*s == '\'') {
	    last[idx++] = *s++;
	    while (*++s && *s != '\'')
		last[idx++] = *s++;
	}
	last[idx++] = *s++;
    }
    *s = '\0';
    last[idx] = '\0';
    next = s + 1;
    return last;
}

/* ARGSUSED */
MenuOfWindowsAction(dpy,winInfo,menuInfo,idx)
Display 	*dpy;
WinGeneric      *winInfo;
MenuInfo    	*menuInfo;
int     	idx;
{
Client	*cli;
List	*l;

    cli = (Client *) menuInfo->menu->buttons[idx]->action.submenu;
    l = (List *) ListApply(WinMenuActionsList,
			matchProgString, cli->framewin->fcore.name);
    if (!l)
        l = (List *) ListApply(WinMenuActionsList, matchProgString, cli->wmInstance);
    if (!l)
        l = (List *) ListApply(WinMenuActionsList, matchProgString, cli->wmClass);
    if (l) {
	applyIsKey = False;
	ListApply(l, applyAction, cli);
    }
    else {
	/* Warp */
	VDMMoveTo(dpy, cli, cli->framewin->core.x, cli->framewin->core.y);
	/* Open */
	if (cli && cli->wmState != NormalState)
	    StateNormal(cli, TimeFresh());
	/* Raise */
	RaiseWindow(cli->framewin);
    }
}

static void *
addButton(cli, menu)
    Client	*cli;
    Menu	*menu;

{
Button	*b;
int	len;

#define MENU_LENGTH	(32)

    if (!cli->framewin)
	return NULL;
    if (GRV.VirtualMenuSort == SortYounger || 
	GRV.VirtualMenuSort == SortAlpha)
        if (findClient_rootid != cli->screen)
	    return NULL;
    if (!menu->buttonCount++)
	menu->buttons = (Button **) MemAlloc(sizeof(Button *));
    else menu->buttons = (Button **)
		MemRealloc(menu->buttons, menu->buttonCount * sizeof(Button *));
    b = (Button *) MemAlloc(sizeof(Button));
    menu->buttons[menu->buttonCount - 1] = b;

    len = strlen(cli->framewin->fcore.name);
    if (len > MENU_LENGTH)
	len = MENU_LENGTH;

    b->label[0].kind = StringLabel;
    b->label[1].kind = NoType;
    b->label[0].string = MemAlloc(len + 4);
    b->label[0].string[0] = '\0';
    if (cli->wmState == IconicState)
        strcat(b->label[0].string, "\244");
    else strcat(b->label[0].string, "  ");
    strcat(b->label[0].string, " ");
    strncat(b->label[0].string, cli->framewin->fcore.name, MENU_LENGTH);
    b->label[1].string = NULL;
    b->helpstring[0] = b->helpstring[1] = NULL;
    b->which = 0;
    b->has_submenu = False;
    b->enabled = True;
    b->visible = True;
    b->callback = MenuOfWindowsAction;
    b->action.submenu = (Menu *) cli;
    b->generate_func = NULL;
    return NULL;
}

static int
cmpButton(a1, a2)
#ifdef SVR4
    const void	*a1, *a2;
#else
    void	*a1, *a2;
#endif

{
Button	**b1 = (Button **) a1;
Button	**b2 = (Button **) a2;
int	type;
char	buf1[256], buf2[256];

    type = ((*b1)->label[0].string[0] == '\244') |
	   (((*b2)->label[0].string[0] == '\244') << 1);

    switch(type) {
	default:
	case 0:
    	    return strcmp(gettext((*b1)->label[0].string),
		  	  gettext((*b2)->label[0].string));
	case 1:
	    return 1;
	case 2:
	    return -1;
	case 3:
	    strnlower(buf1, gettext((*b1)->label[0].string + 1),
		      strlen(gettext((*b1)->label[0].string + 1)));
	    strnlower(buf2, gettext((*b2)->label[0].string + 1),
		      strlen(gettext((*b2)->label[0].string + 1)));
    	    return strcmp(buf1, buf2);
    }
}

/* ARGSUSED */
GenWinMenuFunc(dpy, menuInfo, bindex, cache, winInfo, depth)
    Display	*dpy;
    MenuInfo	*menuInfo;
    int		bindex;
    MenuCache	*cache;
    WinGeneric	*winInfo;
    int		depth;
{
Menu	*menu;
int	columns, slot;
MenuCache	*menuCache;
extern MenuInfo	*MenuInfoCreate();

    menuCache = winInfo->core.client->scrInfo->menuCache;
    for (slot = 0; slot < menuCache->nextSlot; slot++)
	if (menuInfo->buttons[bindex].subMenu ==
			menuCache->menuInfoList[slot])
	    break;
    if (slot == menuCache->nextSlot)
	slot = MENU_NEWSLOT;

    columns = menuInfo->buttons[bindex].subMenu->menu->prefColSize;
    MenuInfoDestroy(menuInfo->buttons[bindex].subMenu);
    menu = (Menu *) MemAlloc(sizeof(Menu));
    menu->buttons = NULL;
    menu->buttonCount = 0;
    menu->buttonDefault = NOBUTTON;
    menu->hasPushPin = False;
    menu->menudirty = True;
    menu->helpstring = "olvwm:WinMenu";
    menu->btnPerCol = 0;
    menu->maxLabWidth = 0;
    menu->prefColSize = columns;

    findClient_rootid = winInfo->core.client->screen;
    ListApply(ActiveClientList, addButton, menu);
    if (GRV.VirtualMenuSort == SortAlpha ||
	GRV.VirtualMenuSort == SortAlphaAll)
        qsort(menu->buttons, menu->buttonCount, sizeof(Button *), cmpButton);

    menuInfo->buttons[bindex].subMenu =
			MenuInfoCreate(cache, winInfo, menu, depth, slot);
}
yytabelem yyexca[] ={
-1, 0,
	0, 1,
	265, 1,
	270, 1,
	276, 1,
	-2, 0,
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 54
# define YYLAST 189
yytabelem yyact[]={

    46,    47,    48,    51,    52,    49,    50,    20,    28,    10,
    18,   104,    22,    79,    12,    25,    86,    24,    25,    14,
     9,    53,    54,    55,    56,    57,    58,    59,   102,    60,
    46,    47,    48,    51,    52,    49,    50,    18,    98,    18,
    62,   101,    80,    29,    19,    15,    78,    77,    76,    75,
    74,    53,    54,    55,    56,    57,    58,    59,    73,    60,
    46,    47,    48,    51,    52,    49,    50,    72,    71,    64,
    70,    99,    69,    68,    67,    66,    65,     2,    23,    16,
     6,    53,    54,    55,    56,    57,    58,    59,     5,    60,
    46,    47,    48,    51,    52,    49,    50,     4,     3,    45,
    44,    30,    43,    42,    41,    40,    11,    26,    17,     8,
    13,    53,    54,    55,    56,    57,    58,    59,    37,    60,
    36,    34,    35,    39,    38,    33,    32,    31,     7,    27,
    21,     1,     0,     0,    61,    63,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    82,    83,    84,     0,
    87,    88,    89,    90,     0,     0,     0,     0,     0,    97,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    81,     0,     0,   100,    85,     0,
     0,   103,     0,    91,    92,    93,    94,    95,    96 };
yytabelem yypact[]={

  -179,  -256,  -249,-10000000,-10000000,-10000000,-10000000,  -222,  -226,  -223,
  -278,-10000000,  -259,-10000000,-10000000,-10000000,  -251,-10000000,-10000000,-10000000,
  -226,  -269,  -224,  -167,-10000000,  -226,  -228,-10000000,  -195,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -190,  -191,  -192,  -193,
  -194,  -196,  -198,  -199,  -208,  -216,  -217,  -218,  -219,  -220,
  -253,-10000000,-10000000,  -225,-10000000,  -226,  -226,  -226,  -226,  -255,
  -226,  -226,  -226,  -226,  -226,  -226,  -226,  -226,  -226,  -226,
-10000000,-10000000,  -254,  -254,  -254,-10000000,-10000000,  -254,  -254,  -254,
  -254,-10000000,-10000000,-10000000,-10000000,-10000000,  -229,  -197,-10000000,-10000000,
  -227,  -239,-10000000,  -257,-10000000 };
yytabelem yypgo[]={

     0,   131,    79,   130,    78,   128,   127,   126,   125,   124,
   123,   122,   121,   120,   118,   109,   108,   107,   106,   105,
   104,   103,   102,   100,    99,    98,    97,    88,    80 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     1,    28,    25,    26,
    27,    17,    17,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     6,
    13,    14,     7,     9,    10,     8,    12,    11,    11,    19,
    20,    20,    21,    22,    23,    24,     5,    18,     3,     3,
     2,     2,    15,    16 };
yytabelem yyr2[]={

     0,     0,     4,     4,     4,     4,     4,     7,     9,     7,
     9,     1,    11,     1,     5,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     5,     7,     7,     7,     7,    19,     5,     3,     1,     7,
     3,     7,     7,     3 };
yytabelem yychk[]={

-10000000,    -1,   256,   -25,   -26,   -27,   -28,    -5,   -15,   276,
   265,   -18,   270,   -16,   268,   267,    -2,   -16,   265,   267,
   285,    -3,   271,    -4,   268,   269,   -17,   -16,   277,   267,
   268,    -6,    -7,    -8,   -12,   -11,   -13,   -14,    -9,   -10,
   -19,   -20,   -21,   -22,   -23,   -24,   257,   258,   259,   262,
   263,   260,   261,   278,   279,   280,   281,   282,   283,   284,
   286,   -16,   268,   -16,   264,   266,   266,   266,   266,   266,
   266,   266,   266,   266,   266,   266,   266,   266,   266,   266,
   267,   -16,    -2,    -2,    -2,   -16,   271,    -2,    -2,    -2,
    -2,   -16,   -16,   -16,   -16,   -16,   -16,    -4,   267,   268,
    -4,   268,   267,    -4,   268 };
yytabelem yydef[]={

    -2,    -2,     0,     2,     3,     4,     5,     0,     0,     0,
    53,    48,     0,    47,     6,    13,     0,    50,    53,    11,
     0,    46,     0,     0,     9,     0,     0,     7,     0,    52,
     8,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    51,    10,     0,    49,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    40,     0,     0,     0,     0,
    13,    29,    32,    35,    36,    37,    38,    30,    31,    33,
    34,    39,    41,    42,    43,    44,     0,     0,    13,    12,
     0,     0,    13,     0,    45 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"WARP",	257,
	"OPEN",	258,
	"RAISE",	259,
	"CLOSE",	260,
	"QUIT",	261,
	"EXECUTE",	262,
	"GOTO",	263,
	"MODIFIER",	264,
	"WORD",	265,
	"COLON",	266,
	"OPENBRACE",	267,
	"CLOSEBRACE",	268,
	"COMMA",	269,
	"SCREEN",	270,
	"INT",	271,
	"STARTSQUOTE",	272,
	"STARTDQUOTE",	273,
	"ENDSQUOTE",	274,
	"ENDDQUOTE",	275,
	"WINMENU",	276,
	"PLUS",	277,
	"RAISELOWER",	278,
	"LOWER",	279,
	"GEOMETRY",	280,
	"REBIND",	281,
	"STICK",	282,
	"SETSIZE",	283,
	"FOCUS",	284,
	"EQUALS",	285,
	"IFELSE",	286,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"file : /* empty */",
	"file : file KeyProg",
	"file : file ScreenProg",
	"file : file WinMenuProg",
	"file : file Assignment",
	"file : error CLOSEBRACE",
	"Assignment : WORD EQUALS String",
	"KeyProg : KeySpec OPENBRACE Actions CLOSEBRACE",
	"ScreenProg : ScreenStart List CLOSEBRACE",
	"WinMenuProg : WINMENU OPENBRACE WinMenuActions CLOSEBRACE",
	"WinMenuActions : /* empty */",
	"WinMenuActions : WinMenuActions String OPENBRACE Actions CLOSEBRACE",
	"Actions : /* empty */",
	"Actions : Actions WarpAction",
	"Actions : Actions OpenAction",
	"Actions : Actions RaiseAction",
	"Actions : Actions ExecuteAction",
	"Actions : Actions GotoAction",
	"Actions : Actions CloseAction",
	"Actions : Actions QuitAction",
	"Actions : Actions RaiseLowerAction",
	"Actions : Actions LowerAction",
	"Actions : Actions GeometryAction",
	"Actions : Actions RebindAction",
	"Actions : Actions StickAction",
	"Actions : Actions SetSizeAction",
	"Actions : Actions FocusAction",
	"Actions : Actions IfElseAction",
	"WarpAction : WARP COLON String",
	"CloseAction : CLOSE COLON List",
	"QuitAction : QUIT COLON List",
	"OpenAction : OPEN COLON List",
	"RaiseLowerAction : RAISELOWER COLON List",
	"LowerAction : LOWER COLON List",
	"RaiseAction : RAISE COLON List",
	"ExecuteAction : EXECUTE COLON List",
	"GotoAction : GOTO COLON String",
	"GotoAction : GOTO COLON INT",
	"GeometryAction : GEOMETRY COLON String",
	"RebindAction : REBIND COLON",
	"RebindAction : REBIND COLON String",
	"StickAction : STICK COLON String",
	"SetSizeAction : SETSIZE COLON String",
	"FocusAction : FOCUS COLON String",
	"IfElseAction : IFELSE COLON String OPENBRACE Actions CLOSEBRACE OPENBRACE Actions CLOSEBRACE",
	"KeySpec : Key Modifier",
	"Key : String",
	"Modifier : /* empty */",
	"Modifier : Modifier PLUS MODIFIER",
	"List : String",
	"List : List COMMA String",
	"ScreenStart : SCREEN INT OPENBRACE",
	"String : WORD",
};
#endif /* YYDEBUG */
/*
 * Copyright (c) 1993 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)yaccpar	6.12	93/06/07 SMI"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
			skip_init:
				yynerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 7:
# line 160 "olvwmrc.y"
{
		    Variable *v;

	            v = ListApply(VariableList, varMatch, yypvt[-2].sval);

		    if (v != NULL) {
		        MemFree(v->value);
		        v->value = strdup(yypvt[-0].sval);
		    }
		    else {
		        v = (Variable *) MemAlloc(sizeof(*v));
		        v->name = strdup(yypvt[-2].sval);
		        v->value = strdup(yypvt[-0].sval);
		        VariableList = ListCons(v, VariableList);
		    }
		} break;
case 8:
# line 178 "olvwmrc.y"
{
		    ProgKey	*p;
		    int		ret;
		    KeyDescriptor	*d;

		    p = yypvt[-3].sval;
		    p->todo = yypvt[-1].sval;
		    d = (KeyDescriptor *) MemAlloc(sizeof(*d));
		    d->rsrc_name = NULL;
		    d->dflt_binding = NULL;
		    d->function = (void (*)()) CheckForKeyProg;
		    d->action = ACTION_VIRTUAL;
		    d->flags = KD_IMMUNE;
		    AddKeyBinding(p->keycode, p->modmask, d);
		    ProgKeyList = ListCons(yypvt[-3].sval, ProgKeyList);
		} break;
case 9:
# line 196 "olvwmrc.y"
{
		    ProgScreen	*p;

		    p = (ProgScreen *) MemAlloc(sizeof(ProgScreen));
		    p->screen = yypvt[-2].ival - 1;		/* count internally from 0 */
		    p->target = yypvt[-1].sval;
		    ProgScreenList = ListCons(p, ProgScreenList);
		} break;
case 10:
# line 206 "olvwmrc.y"
{
		    static int DoneWinMenuActions = False;

		    if (DoneWinMenuActions) {
			ErrorWarning(gettext("Duplicate WINMENU entry in .olvwmrc"));
			YYERROR;
		    }
		    WinMenuActionsList = yypvt[-1].sval;
		} break;
case 11:
# line 217 "olvwmrc.y"
{ yyval.sval = NULL; } break;
case 12:
# line 219 "olvwmrc.y"
{ 
		    WinMenuActions	*p;

		    p = (WinMenuActions *) MemAlloc(sizeof(WinMenuActions));
		    p->key = strdup(yypvt[-3].sval);
		    p->actions = yypvt[-1].sval;
		    yyval.sval = ListCons(p, yypvt[-4].sval);
		} break;
case 13:
# line 229 "olvwmrc.y"
{ yyval.sval = NULL; } break;
case 14:
# line 231 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 15:
# line 233 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 16:
# line 235 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 17:
# line 237 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 18:
# line 239 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 19:
# line 241 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 20:
# line 243 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 21:
# line 245 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 22:
# line 247 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 23:
# line 249 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 24:
# line 251 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 25:
# line 253 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 26:
# line 255 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 27:
# line 257 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 28:
# line 259 "olvwmrc.y"
{ yyval.sval = ListCons(yypvt[-0].sval, yypvt[-1].sval); } break;
case 29:
# line 262 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Warp;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 30:
# line 272 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Close;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 31:
# line 282 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Quit;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 32:
# line 292 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Open;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 33:
# line 302 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = RaiseLower;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 34:
# line 312 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Lower;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 35:
# line 322 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Raise;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 36:
# line 332 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Execute;
			    p->parameter = yypvt[-0].sval;
			    yyval.sval = p;
			} break;
case 37:
# line 342 "olvwmrc.y"
{
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Goto;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 38:
# line 351 "olvwmrc.y"
{
			    ProgKeyNode	*p;
			    char	s[80];

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Goto;
			    sprintf(s, "%d", yypvt[-0].ival);
			    p->parameter = strdup(s);
			    yyval.sval = p;
			} break;
case 39:
# line 364 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Geometry;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 40:
# line 374 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Rebind;
			    p->parameter = NULL;
			    yyval.sval = p;
			} break;
case 41:
# line 383 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Rebind;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 42:
# line 393 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Stick;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 43:
# line 403 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = SetSize;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 44:
# line 413 "olvwmrc.y"
{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Focus;
			    p->parameter = strdup(yypvt[-0].sval);
			    yyval.sval = p;
			} break;
case 45:
# line 424 "olvwmrc.y"
{
			    ProgKeyNode *p;
			    IfElseStruct *s;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    s = (IfElseStruct *) MemAlloc(sizeof(IfElseStruct));

			    p->action = IfElse;
			    p->parameter = (char *) s;
			    s->identifier = strdup(yypvt[-6].sval);
			    s->doIf = yypvt[-4].sval;
			    s->doElse = yypvt[-1].sval;

			    yyval.sval = p;
			} break;
case 46:
# line 441 "olvwmrc.y"
{ 
			    KeySym	ks;
			    KeyCode	kc;
			    ProgKey	*p;
			    char	msg[80];

			    ks = XStringToKeysym(yypvt[-1].sval);
			    if (ks == NoSymbol) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    yypvt[-1].sval);
				ErrorWarning(msg);
				YYERROR;
			    }
			    kc = XKeysymToKeycode(dpy, ks);
			    if (kc == 0) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    yypvt[-1].sval);
				ErrorWarning(msg);
				YYERROR;
			    }
			    p = (ProgKey *) MemAlloc(sizeof(ProgKey));
			    p->keycode = kc;
			    p->modmask = yypvt[-0].ival;

			    yyval.sval = p;
			    free(yypvt[-1].sval);
			} break;
case 47:
# line 472 "olvwmrc.y"
{ yyval.sval = strdup(yypvt[-0].sval); } break;
case 48:
# line 475 "olvwmrc.y"
{ yyval.ival = 0; } break;
case 49:
# line 477 "olvwmrc.y"
{
			    KeyCode	kc;
			    char msg[80];

			    if (yypvt[-0].ival == -1)
			        yyval.ival = AnyModifier;
			    else {
				kc = XKeysymToKeycode(dpy, yypvt[-0].ival);
				if (kc == 0) {
				    sprintf(msg,
				    gettext("Unknown modifer %d\n in .olvwmrc"),
				    yypvt[-1].ival);
				    ErrorWarning(msg);
				    YYERROR;
				}
			    }
			    yyval.ival |= FindModifierMask(kc);
			} break;
case 50:
# line 497 "olvwmrc.y"
{ yyval.sval = yypvt[-0].sval; } break;
case 51:
# line 499 "olvwmrc.y"
{
			    char	*s;

			    s = MemAlloc(strlen(yypvt[-2].sval) + strlen(yypvt[-0].sval) + 2);
			    sprintf(s, "%s,%s", yypvt[-2].sval, yypvt[-0].sval);
			    free(yypvt[-2].sval);
			    free(yypvt[-0].sval);
			    yyval.sval = s;
			} break;
case 52:
# line 510 "olvwmrc.y"
{
		    yyval.ival = yypvt[-1].ival;
		} break;
case 53:
# line 515 "olvwmrc.y"
{
		char	*t;

		t = yypvt[-0].sval;
		if (*t == '\"' || *t == '\'') {
		    /* word in quotes; get rid of them */
		    t++;
		    t[strlen(t) - 1] = '\0';
		}
		yyval.sval = strexpand(t);
		free(yypvt[-0].sval);
	    } break;
	}
	goto yystack;		/* reset registers in driver code */
}

