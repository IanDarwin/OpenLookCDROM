/*
 *      (c) Copyright 1991 Scott Oaks
 *      See LEGAL_NOTICE file for terms of the license.
 */ 

%{
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
%}


%start file

%union {
    int		ival;
    void	*sval;
}

%token	<ival> WARP
%token	<ival> OPEN
%token	<ival> RAISE
%token	<ival> CLOSE
%token	<ival> QUIT
%token	<ival> EXECUTE
%token	<ival> GOTO
%token	<ival> MODIFIER
%token	<sval> WORD
%token	<ival> COLON
%token	<ival> OPENBRACE
%token	<ival> CLOSEBRACE
%token	<ival> COMMA
%token	<ival> SCREEN
%token	<ival> INT
%token	<sval> STARTSQUOTE
%token	<sval> STARTDQUOTE
%token	<sval> ENDSQUOTE
%token	<sval> ENDDQUOTE
%token	<ival> WINMENU
%token  <ival> PLUS
%token	<ival> RAISELOWER
%token	<ival> LOWER
%token	<ival> GEOMETRY
%token	<ival> REBIND
%token	<ival> STICK
%token	<ival> SETSIZE
%token	<ival> FOCUS
%token	<ival> EQUALS
%token	<ival> IFELSE

%type	<sval> List
%type	<ival> Modifier
%type	<sval> Actions
%type	<sval> KeySpec
%type	<sval> WarpAction
%type	<sval> OpenAction
%type	<sval> RaiseAction
%type	<sval> RaiseLowerAction
%type	<sval> LowerAction
%type	<sval> GotoAction
%type	<sval> ExecuteAction
%type	<sval> CloseAction
%type	<sval> QuitAction
%type	<ival> ScreenStart
%type	<sval> String
%type	<sval> WinMenuActions
%type	<sval> Key
%type	<sval> GeometryAction
%type	<sval> RebindAction
%type	<sval> StickAction
%type	<sval> SetSizeAction
%type	<sval> FocusAction
%type	<sval> IfElseAction

%%
/* Rules */

file	:	/* empty */
	|	file KeyProg
	|	file ScreenProg
	|	file WinMenuProg
	|	file Assignment
	|	error CLOSEBRACE

Assignment	:	WORD EQUALS String
		{
		    Variable *v;

	            v = ListApply(VariableList, varMatch, $1);

		    if (v != NULL) {
		        MemFree(v->value);
		        v->value = strdup($3);
		    }
		    else {
		        v = (Variable *) MemAlloc(sizeof(*v));
		        v->name = strdup($1);
		        v->value = strdup($3);
		        VariableList = ListCons(v, VariableList);
		    }
		}

KeyProg	:	KeySpec OPENBRACE Actions CLOSEBRACE
		{
		    ProgKey	*p;
		    int		ret;
		    KeyDescriptor	*d;

		    p = $1;
		    p->todo = $3;
		    d = (KeyDescriptor *) MemAlloc(sizeof(*d));
		    d->rsrc_name = NULL;
		    d->dflt_binding = NULL;
		    d->function = (void (*)()) CheckForKeyProg;
		    d->action = ACTION_VIRTUAL;
		    d->flags = KD_IMMUNE;
		    AddKeyBinding(p->keycode, p->modmask, d);
		    ProgKeyList = ListCons($1, ProgKeyList);
		}

ScreenProg :	ScreenStart List CLOSEBRACE
		{
		    ProgScreen	*p;

		    p = (ProgScreen *) MemAlloc(sizeof(ProgScreen));
		    p->screen = $1 - 1;		/* count internally from 0 */
		    p->target = $2;
		    ProgScreenList = ListCons(p, ProgScreenList);
		}

WinMenuProg :	WINMENU OPENBRACE WinMenuActions CLOSEBRACE
		{
		    static int DoneWinMenuActions = False;

		    if (DoneWinMenuActions) {
			ErrorWarning(gettext("Duplicate WINMENU entry in .olvwmrc"));
			YYERROR;
		    }
		    WinMenuActionsList = $3;
		}

WinMenuActions : /* empty */
		{ $$ = NULL; }
	|	WinMenuActions String OPENBRACE Actions CLOSEBRACE
		{ 
		    WinMenuActions	*p;

		    p = (WinMenuActions *) MemAlloc(sizeof(WinMenuActions));
		    p->key = strdup($2);
		    p->actions = $4;
		    $$ = ListCons(p, $1);
		}

Actions :	/* empty */
		{ $$ = NULL; }
	|	Actions WarpAction
		{ $$ = ListCons($2, $1); }
	|	Actions OpenAction
		{ $$ = ListCons($2, $1); }
	|	Actions RaiseAction
		{ $$ = ListCons($2, $1); }
	|	Actions ExecuteAction
		{ $$ = ListCons($2, $1); }
	|	Actions GotoAction
		{ $$ = ListCons($2, $1); }
	|	Actions CloseAction
		{ $$ = ListCons($2, $1); }
	|	Actions QuitAction
		{ $$ = ListCons($2, $1); }
	|	Actions RaiseLowerAction
		{ $$ = ListCons($2, $1); }
	|	Actions LowerAction
		{ $$ = ListCons($2, $1); }
	|	Actions GeometryAction
		{ $$ = ListCons($2, $1); }
	|	Actions RebindAction
		{ $$ = ListCons($2, $1); }
	|	Actions StickAction
		{ $$ = ListCons($2, $1); }
	|	Actions SetSizeAction
		{ $$ = ListCons($2, $1); }
	|	Actions FocusAction
		{ $$ = ListCons($2, $1); }
	|	Actions IfElseAction
		{ $$ = ListCons($2, $1); }

WarpAction :	WARP COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Warp;
			    p->parameter = strdup($3);
			    $$ = p;
			}
	
CloseAction:	CLOSE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Close;
			    p->parameter = $3;
			    $$ = p;
			}
	
QuitAction:	QUIT COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Quit;
			    p->parameter = $3;
			    $$ = p;
			}

OpenAction:	OPEN COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Open;
			    p->parameter = $3;
			    $$ = p;
			}
			
RaiseLowerAction:	RAISELOWER COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = RaiseLower;
			    p->parameter = $3;
			    $$ = p;
			}
			
LowerAction:	LOWER COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Lower;
			    p->parameter = $3;
			    $$ = p;
			}
			
RaiseAction:	RAISE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Raise;
			    p->parameter = $3;
			    $$ = p;
			}
			
ExecuteAction:	EXECUTE COLON List
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Execute;
			    p->parameter = $3;
			    $$ = p;
			}

GotoAction:	GOTO COLON String
			{
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Goto;
			    p->parameter = strdup($3);
			    $$ = p;
			}
	|	GOTO COLON INT
			{
			    ProgKeyNode	*p;
			    char	s[80];

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Goto;
			    sprintf(s, "%d", $3);
			    p->parameter = strdup(s);
			    $$ = p;
			}


GeometryAction :	GEOMETRY COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Geometry;
			    p->parameter = strdup($3);
			    $$ = p;
			}

RebindAction :	REBIND COLON
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Rebind;
			    p->parameter = NULL;
			    $$ = p;
			}
	|	REBIND COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Rebind;
			    p->parameter = strdup($3);
			    $$ = p;
			}

StickAction :	STICK COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Stick;
			    p->parameter = strdup($3);
			    $$ = p;
			}

SetSizeAction : SETSIZE COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = SetSize;
			    p->parameter = strdup($3);
			    $$ = p;
			}

FocusAction : FOCUS COLON String
			{ 
			    ProgKeyNode	*p;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    p->action = Focus;
			    p->parameter = strdup($3);
			    $$ = p;
			}

IfElseAction: IFELSE COLON String OPENBRACE Actions CLOSEBRACE
				      OPENBRACE Actions CLOSEBRACE
			{
			    ProgKeyNode *p;
			    IfElseStruct *s;

			    p = (ProgKeyNode *) MemAlloc(sizeof(ProgKeyNode));
			    s = (IfElseStruct *) MemAlloc(sizeof(IfElseStruct));

			    p->action = IfElse;
			    p->parameter = (char *) s;
			    s->identifier = strdup($3);
			    s->doIf = $5;
			    s->doElse = $8;

			    $$ = p;
			}

KeySpec : 	Key Modifier
			{ 
			    KeySym	ks;
			    KeyCode	kc;
			    ProgKey	*p;
			    char	msg[80];

			    ks = XStringToKeysym($1);
			    if (ks == NoSymbol) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    $1);
				ErrorWarning(msg);
				YYERROR;
			    }
			    kc = XKeysymToKeycode(dpy, ks);
			    if (kc == 0) {
				sprintf(msg,
				    gettext("Unknown keysymbol %s in .olvwmrc"),
				    $1);
				ErrorWarning(msg);
				YYERROR;
			    }
			    p = (ProgKey *) MemAlloc(sizeof(ProgKey));
			    p->keycode = kc;
			    p->modmask = $2;

			    $$ = p;
			    free($1);
			}

Key	:	String
		{ $$ = strdup($1); }

Modifier :	/* empty */
			{ $$ = 0; }
	|	Modifier PLUS MODIFIER
			{
			    KeyCode	kc;
			    char msg[80];

			    if ($3 == -1)
			        $$ = AnyModifier;
			    else {
				kc = XKeysymToKeycode(dpy, $3);
				if (kc == 0) {
				    sprintf(msg,
				    gettext("Unknown modifer %d\n in .olvwmrc"),
				    $2);
				    ErrorWarning(msg);
				    YYERROR;
				}
			    }
			    $$ |= FindModifierMask(kc);
			}

List	:	String
			{ $$ = $1; }
	|	List COMMA String
			{
			    char	*s;

			    s = MemAlloc(strlen($1) + strlen($3) + 2);
			    sprintf(s, "%s,%s", $1, $3);
			    free($1);
			    free($3);
			    $$ = s;
			}

ScreenStart :	SCREEN INT OPENBRACE
		{
		    $$ = $2;
		}

String :    WORD
	    {
		char	*t;

		t = $1;
		if (*t == '\"' || *t == '\'') {
		    /* word in quotes; get rid of them */
		    t++;
		    t[strlen(t) - 1] = '\0';
		}
		$$ = strexpand(t);
		free($1);
	    }
%%
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
