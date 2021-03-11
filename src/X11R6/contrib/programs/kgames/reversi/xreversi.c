#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <Xkw/Layout.h>
#include "Reversi.h"
#include <stdio.h>
#include "reversi.h"

static Widget	topLevel, layout, reversi, error;
static Widget	quit, hint, player, undoButton, restart;
static Widget	playerLabel, playWhite, playBlack, playBoth, playNeither;

static Widget	levelLabel, levelValue;
static Widget	turn;

static void DoSetLevel();

/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XtParseCommand is let loose. */

typedef struct _Xreversi {
	int		animateTimeout;
	int		animateRepeat;
} Xreversi;

Xreversi    app_resources;

#define offset(field)	XtOffset(Xreversi*, field)

static XtResource resources[] = {
    {"animateTimeout", "AnimateTimeout", XtRInt, sizeof (int),
	offset (animateTimeout), XtRImmediate, (char *) 150},
    {"animateRepeat", "AnimateRepeat", XtRInt, sizeof (int),
	offset (animateRepeat), XtRImmediate, (char *) 4},
};

static XrmOptionDescRec options[] = {
{"-animate",	"*animateTimeout",	XrmoptionSepArg,	NULL}
};

Widget
MakeCommandButton(box, name, function)
Widget box;
char *name;
XtCallbackProc function;
{
  Widget w = XtCreateManagedWidget(name, commandWidgetClass, box, NULL, ZERO);
  if (function != NULL)
    XtAddCallback(w, XtNcallback, function, (caddr_t) NULL);
  return w;
}

Widget
MakeRadioButton (parent, name, callback, value, group)
    Widget	    parent;
    char	    *name;
    XtCallbackProc  callback;
    int		    value;
    Widget	    group;
{
    Arg		args[2];
    Widget	button;
    Cardinal	n;

    n = 0;
    XtSetArg (args[n], XtNradioData, (caddr_t) value); n++;
    if (group) {
	XtSetArg (args[n], XtNradioGroup, (caddr_t) group); n++;
    }
    button = XtCreateManagedWidget (name, toggleWidgetClass,
				       parent, args, n);
    if (callback != NULL)
	XtAddCallback (button, XtNcallback, callback, (caddr_t) NULL);
    return button;
}

Widget 
MakeStringBox(parent, name, string)
Widget parent;
String name, string;
{
    Arg args[5];
    Cardinal numargs = 0;
    Widget StringW;
    Widget Source;
    
    XtSetArg(args[numargs], XtNeditType, XawtextEdit); numargs++;
    XtSetArg(args[numargs], XtNstring, string); numargs++;
    
    StringW = XtCreateManagedWidget(name, asciiTextWidgetClass, 
			      parent, args, numargs);
    
    Source = XawTextGetSource (StringW);
    XtAddCallback (Source, XtNcallback, DoSetLevel, (caddr_t) NULL);
    return(StringW);  
}

extern int  com, defcom;
extern int  level;

int UIdone, UIret;

static void
DoQuit ()
{
    dispError ("");
    UIdone = 1;
    UIret = 0;
}

DoHint ()
{
    dispError ("");
    doHint ();
}

DoUndo ()
{
    dispError ("");
    undo ();
}

DoRestart ()
{
    dispError ("");
    UIdone = 1;
    UIret = 1;
}

#define PLAY_WHITE	1
#define PLAY_BLACK	2
#define PLAY_BOTH	3
#define PLAY_NEITHER	4

DoPlay ()
{
    int	    current;

    current = (int) XawToggleGetCurrent (playWhite);
    switch (current)
    {
    case PLAY_WHITE:
	defcom = WHITE;
	com = WHITE;
	break;
    case PLAY_BLACK:
    	defcom = BLACK;
    	com = BLACK;
	break;
    case PLAY_BOTH:
	com = 0;
	break;
    case PLAY_NEITHER:
	com = 2;
	break;
    }
}

SetPlay ()
{
    int	current, should_be;

    current = (int) XawToggleGetCurrent (playWhite);
    switch (com)
    {
    case WHITE:
	should_be = PLAY_WHITE;
	break;
    case BLACK:
	should_be = PLAY_BLACK;
	break;
    case 0:
	should_be = PLAY_BOTH;
	break;
    case 2:
	should_be = PLAY_NEITHER;
	break;
    }
    if (current != should_be)
    {
	XawToggleSetCurrent (playWhite, (caddr_t) should_be);
    }
}

static void
DoMove (w, closure, call_data)
    Widget	w;
    XtPointer	closure;
    XtPointer	call_data;
{
    ReversiMovePtr  move = (ReversiMovePtr) call_data;
    dispError ("");
    domove (move->x + 1, move->y + 1);
}

static char levelString[100];

static Bool levelChanged;

static void
DoSetLevel ()
{
    levelChanged = TRUE;
}

SetLevel ()
{
    Widget  source;
    Arg	    args[1];
    char    *value;
    int	    newlevel;

    levelChanged = FALSE;
    source = XawTextGetSource (levelValue);
    XtSetArg (args[0], XtNstring, &value);
    XtGetValues (source, args, 1);
    if (sscanf (value, "%d", &newlevel) == 1)
	level = newlevel;
}

XtActionsRec xreversi_actions[] = {
    "SetLevel",	    DoSetLevel,
};

void dispInit(argc, argv)
    unsigned int argc;
    char **argv;
{
    Arg	args[1];
    int	current;

    topLevel = XtInitialize( NULL, "Xreversi",
			     options, XtNumber(options),
			     &argc, argv );

    XtGetApplicationResources(topLevel, &app_resources, resources,
			      XtNumber(resources), NULL, 0);

    XtAppAddActions (XtWidgetToApplicationContext (topLevel),
		     xreversi_actions, XtNumber (xreversi_actions));

    layout = XtCreateManagedWidget ( "layout", layoutWidgetClass, topLevel, NULL, ZERO);
    reversi = XtCreateManagedWidget( "reversi", reversiWidgetClass, layout, NULL, ZERO );
    XtAddCallback (reversi, XtNstoneCallback, DoMove, (caddr_t) NULL);
    error = XtCreateManagedWidget ( "error", labelWidgetClass, layout, NULL, ZERO );
    quit = MakeCommandButton (layout, "quit", DoQuit);
    hint = MakeCommandButton (layout, "hint", DoHint);
    undoButton = MakeCommandButton (layout, "undo", DoUndo);
    restart = MakeCommandButton (layout, "restart", DoRestart);
    playerLabel = XtCreateManagedWidget ("playerLabel", labelWidgetClass, layout, NULL, ZERO);
    playWhite = MakeRadioButton (layout, "playWhite", DoPlay, PLAY_WHITE, (Widget) NULL);
    playBlack = MakeRadioButton (layout, "playBlack", DoPlay, PLAY_BLACK, playWhite);
    playBoth = MakeRadioButton (layout, "playBoth", DoPlay, PLAY_BOTH, playWhite);
    playNeither = MakeRadioButton (layout, "playNeither", DoPlay, PLAY_NEITHER, playWhite);
    SetPlay ();
    levelLabel = XtCreateManagedWidget ( "levelLabel", labelWidgetClass, layout, NULL, ZERO);
    sprintf (levelString, "%d", level);
    levelValue = MakeStringBox (layout, "levelValue", levelString);
    XtSetKeyboardFocus (layout, levelValue);
    turn = XtCreateManagedWidget ("turn", labelWidgetClass, layout, NULL, ZERO);
    XtRealizeWidget(topLevel);
}

playGame ()
{
    XEvent  event;
    UIdone = 0;

    while (!UIdone)
    {
	checkInput ();
	SetPlay ();
	waitForServer(True);
    }
    return UIret;
}

display (board)
boardT	board;
{
    int	    i, j;
    ReversiStone    stone;

    for (i = 1; i <= SIZE; i++)
	for (j = 1; j <= SIZE; j++)
	{
	    switch (board[i][j])
	    {
	    case BLACK:
		stone = StoneBlack;
		break;
	    case WHITE:
		stone = StoneWhite;
		break;
	    default:
		stone = StoneNone;
		break;
	    }
	    XawReversiSetSpot (reversi, i-1, j-1, stone);
	}
    XFlush (XtDisplay (topLevel));
}

dispError (s)
char	*s;
{
    Arg	args[1];

    XtSetArg (args[0], XtNlabel, s);
    XtSetValues (error, args, 1);
    waitForServer(False);
}

dispGrid ()
{
}

dispEnd ()
{
}

waitForServer(yield)
    Boolean yield;
{
    XSync (XtDisplay (topLevel), False);
    while (yield || XtPending ())
    {
	XEvent	event;
	XtNextEvent (&event);
	XtDispatchEvent (&event);
	if (levelChanged)
	    SetLevel ();
	yield = False;
    }
}

dispTurn (player)
{
    static char	turnString[100];
    static int	oldPlayer = 100;
    Arg	args[1];

    SetPlay ();
    if (player != oldPlayer)
    {
	if (player == EMPTY)
	    sprintf (turnString, "Game over");
	else
	    sprintf (turnString, "%s's turn", 
		     player == WHITE ? "white" : "black");
    	XtSetArg (args[0], XtNlabel, turnString);
    	XtSetValues (turn, args, 1);
    	oldPlayer = player;
    }
    waitForServer (False);
}

dispMove (x, y, player)
{
    ReversiStone	A, B;

    if (player == WHITE)
	A = StoneWhite;
    else
	A = StoneBlack;
    B = StoneNone;
    XawReversiAnimateSpot (reversi, x - 1, y - 1, A, B, 
			   (unsigned long) app_resources.animateTimeout,
			   app_resources.animateRepeat);
    dispError ("");
}

dispHint (x, y, player)
{
    dispMove (x, y, player);
}
