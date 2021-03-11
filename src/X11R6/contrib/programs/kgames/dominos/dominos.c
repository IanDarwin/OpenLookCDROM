/*
 * $NCD$
 *
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, Network Computing Devices
 */

# include	<X11/Intrinsic.h>
# include	<X11/StringDefs.h>
# include	<X11/Shell.h>
# include	<X11/Xos.h>
# include	<X11/Xaw/Command.h>
# include	<X11/Xaw/Box.h>
# include	<X11/Xaw/Dialog.h>
# include	<X11/Xaw/Label.h>
# include	<X11/Xaw/MenuButton.h>
# include	<X11/Xaw/SimpleMenu.h>
# include	<X11/Xaw/SmeBSB.h>
# include	<X11/Xaw/AsciiText.h>
# include	<X11/Xaw/Cardinals.h>
# include	<X11/Xaw/Porthole.h>
# include	<X11/Xaw/Panner.h>
# include	<X11/Xaw/Scrollbar.h>
# include	"Dominos.h"
# include	<Xkw/Layout.h>
# include	<X11/Xutil.h>
# include	"dominos.h"
# include	<stdio.h>

Widget	    toplevel;
Widget	    frame;
Widget	    panner;
Widget	    porthole;
Widget	    board_w;
Widget	    player_porthole;
Widget	    player_scrollbar;
Widget	    player_w;
Widget	    message;
Widget	    menuBar;
Widget	    fileMenuButton;
Widget	    fileMenu;
Widget	    newGame;
Widget	    undo;
Widget	    hint;
Widget	    computerCount;
Widget	    draw;
Widget	    score_w[MAX_PLAYERS];

int	    total_score[MAX_PLAYERS];
int	    last_score[MAX_PLAYERS];

char	    *player_names[MAX_PLAYERS] = { "You", "I" };
int	    game_over;

typedef struct _dominosResources {
    int		animationSpeed;
    String	saveFile;
} DominosResources, *DominosResourcesPtr;

DominosResources dominosResources;

Count (domino)
    DominoPtr	domino;
{
    int	c = 0;

    for (; domino; domino = domino->peer[LinkPeer])
	c++;
    return c;
}

#define DISPLAY_BOARD	    1
#define DISPLAY_PLAYER	    2
#define DISPLAY_COMPUTER	    4

DisplayDominos (changes)
    int	changes;
{
    int	count;
    
    if (changes & DISPLAY_PLAYER)
	DominosSetDominos (player_w, &player[0]);
    if (changes & DISPLAY_COMPUTER)
    {
	count = Count (player[1]);
	Message (computerCount, "I have %d domino%s.",
		 count, count != 1 ? "s" : "");
    }
    if (changes & DISPLAY_BOARD)
	DominosSetDominos (board_w, &board);
}

MakeMove (player, source, target, dir, orientation)
    DominoPtr	*player;
    DominoPtr	source, target;
    Direction	dir, orientation;
{
    if (game_over)
	return;
    PlayerMove (player, source, target, dir, orientation);
    if (!*player)
	GameOver ();
}

int
MakeDraw (player)
    DominoPtr	*player;
{
    if (game_over)
	return FALSE;
    if (!PlayerDraw (player, TRUE))
    {
	GameOver ();
	return FALSE;
    }
    return TRUE;
}

PlayEdgeFunc (source, target, dir, orientation, data)
    DominoPtr	source;
    DominoPtr	target;
    Direction	dir, orientation;
    pointer	data;
{
    PlayPtr play = (PlayPtr) data;

    if (target == play->target &&
	source == play->source &&
	(play->dist == 0 || play->dir == dir))
    {
	MakeMove (play->player, source, target, dir, orientation);
	return FALSE;
    }
    return TRUE;
}

Play (player, domino, target, dir, dist)
    DominoPtr	*player;
    DominoPtr	domino;
    DominoPtr	target;
    Direction	dir;
    int		dist;
{
    PlayRec play;
    
    if (!board)
    {
	PlayerFirstMove (player, domino);
	DisplayDominos (DISPLAY_PLAYER|DISPLAY_BOARD);
	if (!game_over)
	    ComputerMove ();
    }
    else
    {
	play.player = player;
	play.source = domino;
	play.target = target;
	play.dist = dist;
	play.dir = dir;
	if (!FindPlays (board, domino, PlayEdgeFunc,
			(pointer) &play))
	{
	    DisplayDominos (DISPLAY_PLAYER|DISPLAY_BOARD);
	    if (!game_over)
		ComputerMove ();
	}
    }
}

ComputerMove ()
{
    PlayRec play;
    
    if (game_over)
	return;
    MessageStart ();
    for (;;)
    {
	if (FindPlay (&player[1], &play))
	{
	    MessageAppend ("I play %dx%d.", play.source->pips[0],
			   play.source->pips[1]);
	    MessageEnd (message);
	    MakeMove (&player[1], play.source, play.target, 
		      play.dir, play.orientation);
	    DisplayDominos (DISPLAY_COMPUTER|DISPLAY_BOARD);
	    break;
	}
	MessageAppend ("I draw. ");
	if (!PlayerDraw (&player[1], TRUE))
	{
	    MessageEnd (message);
	    DisplayDominos (DISPLAY_COMPUTER);
	    GameOver ();
	    break;
	}
    }
}

DisplayScores ()
{
    int	    i;
    
    for (i = 0; i < NumPlayers; i++)
    {
	Message (score_w[i], "%s have %d point%s.", 
		 player_names[i], total_score[i], 
		 total_score[i] == 1 ? "" : "s");
    }
}

Score ()
{
    int		scores[MAX_PLAYERS];
    int		i;
    int		best, best_i;
    DominoPtr	domino;
    int		score;

    best = 1000;
    best_i = -1;
    for (i = 0; i < NumPlayers; i++)
    {
	scores[i] = 0;
	for (domino = player[i]; domino; domino = domino->peer[LinkPeer])
	    scores[i] += domino->pips[0] + domino->pips[1];
	if (scores[i] < best)
	{
	    best = scores[i];
	    best_i = i;
	}
    }
    score = 0;
    for (i = 0; i < NumPlayers; i++)
    {
	if (i != best_i)
	    score += scores[i];
	last_score[i] = 0;
    }
    score -= scores[best_i];
    Message (message, "%s win, score %d",
	     player_names[best_i], score);
    total_score[best_i] += score;
    last_score[best_i] = score;
    DisplayScores ();
}

Draw ()
{
    MakeDraw (&player[0]);
    DisplayDominos (DISPLAY_PLAYER);
    MakeLastPlayerVisible ();
}

Hint ()
{
    PlayRec play;
    
    if (game_over)
    {
	Message (message, "Game over.");
    }
    else if (FindPlay (&player[0], &play))
    {
	Message (message, "Play %dx%d on %dx%d.",
		 play.source->pips[0], play.source->pips[1],
		 play.target->pips[0], play.target->pips[1]);
    }
    else
    {
	Message (message, "Draw.");
    }
}
	
GameOver ()
{
    game_over = TRUE;
    Score ();
}

UndoGameOver ()
{
    int	    i;
    
    if (!game_over)
	return;
    game_over = FALSE;
    for (i = 0; i < NumPlayers; i++)
	total_score[i] -= last_score[i];
    DisplayScores ();
}

NewGame ()
{
    if (!game_over)
    {
	if (!YesOrNo (toplevel, "Abandon game in progress?"))
	    return;
    }
    do {
	ResetGame ();
	DisplayDominos (DISPLAY_COMPUTER|DISPLAY_PLAYER|DISPLAY_BOARD);
    } while (!MakeFirstMove ());
    game_over = FALSE;
}

Save ()
{
    FILE    *f;
    int	    i;

    f = fopen (dominosResources.saveFile, "w");
    if (!f)
	return;
    WriteDominos (f, pile);
    for (i = 0; i < NumPlayers; i++)
    {
	WriteDominos (f, player[i]);
    }
    WriteDominos (f, board);
    WriteScores (f, total_score, NumPlayers);
    WriteInt (f, game_over);
    fclose (f);
}
    
int
Restore ()
{
    FILE    *f;
    int	    i;
    DominoPtr	new_pile;
    DominoPtr	new_player[MAX_PLAYERS];
    DominoPtr	new_board;
    int		new_score[MAX_PLAYERS];
    int		new_game_over;

    f = fopen (dominosResources.saveFile, "r");
    if (!f)
	return FALSE;
    new_pile = ReadDominos (f);
    if (DominoErrno)
    {
	fclose (f);
	return FALSE;
    }
    for (i = 0; i < NumPlayers; i++)
    {
	new_player[i] = ReadDominos (f);
	if (DominoErrno)
	{
	    fclose (f);
	    return FALSE;
	}
    }
    new_board = ReadDominos (f);
    if (DominoErrno)
    {
	fclose (f);
	return FALSE;
    }
    ReadScores (f, new_score, NumPlayers);
    if (DominoErrno)
    {
	fclose (f);
	return FALSE;
    }
    ReadInt (f, &new_game_over);
    if (DominoErrno)
	new_game_over = FALSE;
    fclose (f);
    DisposeGame ();
    pile = new_pile;
    for (i = 0; i < NumPlayers; i++)
    {
	player[i] = new_player[i];
	total_score[i] = new_score[i];
    }
    board = new_board;
    game_over = new_game_over;
    DisplayScores ();
    if (game_over)
	NewGame ();
    else
	DisplayDominos (DISPLAY_COMPUTER|DISPLAY_PLAYER|DISPLAY_BOARD);
    return TRUE;
}

GetSize (widget, w, h)
    Widget	widget;
    Dimension	*w, *h;
{
    Arg		args[2];
    
    XtSetArg (args[0], XtNwidth, w);
    XtSetArg (args[1], XtNheight, h);
    XtGetValues (widget, args, 2);
}

Center (original, new)
    Widget  original, new;
{
    Arg		args[2];
    Dimension	center_width, center_height;
    Dimension	prompt_width, prompt_height;
    Position	source_x, source_y, dest_x, dest_y;
    /*
     * place the widget in the center of the "parent"
     */
    GetSize (new, &prompt_width, &prompt_height);
    GetSize (original, &center_width, &center_height);
    source_x = (int)(center_width - prompt_width) / 2;
    source_y = (int)(center_height - prompt_height) / 3;
    XtTranslateCoords (original, source_x, source_y, &dest_x, &dest_y);
    XtSetArg (args[0], XtNx, dest_x);
    XtSetArg (args[1], XtNy, dest_y);
    XtSetValues (new, args, 2);
}
    
static int  yn_done, yn_answer;

/*ARGSUSED*/
static void YesFunc (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    yn_answer = 1;
    yn_done = 1;
}

/*ARGSUSED*/
static void NoFunc (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    yn_answer = 0;
    yn_done = 1;
}

YesOrNo (original, prompt)
    Widget  original;
    char    *prompt;
{
    Arg	    args[3];
    XEvent  event;
    Widget  shell, dialog, label;
    
    XtSetArg (args[0], XtNmappedWhenManaged, FALSE);
    shell = XtCreateApplicationShell ("yesOrNo", transientShellWidgetClass,
				      args, ONE);
    dialog = XtCreateManagedWidget ("yesOrNoDialog", layoutWidgetClass,
				shell, NULL, ZERO);
    label = XtCreateManagedWidget ("yesOrNoLabel", labelWidgetClass,
				dialog, NULL, ZERO);
    XawDialogAddButton (dialog, "yesOrNoOk", YesFunc, NULL);
    XawDialogAddButton (dialog, "yesOrNoNo", NoFunc, NULL);

    XtSetArg (args[0], XtNlabel, prompt);
    XtSetValues (label, args, 1);
    XtRealizeWidget (shell);
    if (XtIsRealized (original))
    {
	Center (original, shell);
	XtSetKeyboardFocus (original, dialog);
    }
    else
    {
	Dimension   prompt_width, prompt_height;
	Position    x, y;
	
	GetSize (shell, &prompt_width, &prompt_height);
	x = (XtScreen (shell)->width - (int) prompt_width) / 2;
	y = (XtScreen (shell)->height - (int) prompt_height) / 3;
	XtSetArg (args[0], XtNx, x);
	XtSetArg (args[1], XtNy, y);
	XtSetValues (shell, args, 2);
    }
    XtMapWidget (shell);
    yn_done = 0;
    while (!yn_done) {
	XtNextEvent (&event);
	XtDispatchEvent (&event);
    }
    XtSetKeyboardFocus (original, (Widget) None);
    XtDestroyWidget (shell);
    return yn_answer;
}

FileError (s)
    char    *s;
{
    char    label[1024];

    sprintf (label, "%s: %s", dominosResources.saveFile, s);
    YesOrNo (toplevel, label);
}

Quit ()
{
    if (YesOrNo (toplevel, "Save game?"))
	Save ();
    exit (0);
}

Undo ()
{
    DominoPtr	*p;

    if (!undoList)
    {
	Message (message, "Nothing to undo.");
	return;
    }
    if (game_over)
	UndoGameOver ();
    Message (message, "Undo.");
    do {
	p = undoList->player;
	PlayerUndo ();
    } while (undoList && p != &player[0]);
    if (!undoList)
	MakeFirstMove ();
    DisplayDominos (DISPLAY_COMPUTER|DISPLAY_PLAYER|DISPLAY_BOARD);
}

int
MakeFirstMove ()
{
    DominoPtr	max_domino;
    DominoPtr	domino;
    DominoPtr	*max_player;
    int		i;

    max_domino = 0;
    max_player = 0;
    for (i = 0; i < NumPlayers; i++)
    {
	for (domino = player[i]; domino; domino = domino->peer[LinkPeer])
	{
	    if (IsDouble (domino))
	    {
		if (!max_domino || domino->pips[0] > max_domino->pips[0])
		{
		    max_domino = domino;
		    max_player = &player[i];
		}
	    }
	}
    }
    if (max_domino)
    {
	PlayerFirstMove (max_player, max_domino);
	i = DISPLAY_BOARD;
	if (max_player != &player[1])
	    i |= DISPLAY_PLAYER;
	else
	    i |= DISPLAY_COMPUTER;
	DisplayDominos (i);
	if (max_player != &player[1])
	    ComputerMove ();
	DisposeUndoList ();
	return TRUE;
    }
    return FALSE;
}

/*	Function Name: PannerCallback
 *	Description: called when the panner has moved.
 *	Arguments: panner - the panner widget.
 *                 closure - *** NOT USED ***.
 *                 report_ptr - the panner record.
 *	Returns: none.
 */

/* ARGSUSED */
void 
PannerCallback(w, closure, report_ptr)
Widget w;
XtPointer closure, report_ptr;
{
    Arg args[2];
    XawPannerReport *report = (XawPannerReport *) report_ptr;
    Widget child = (Widget) closure;

    XtSetArg (args[0], XtNx, -report->slider_x);
    XtSetArg (args[1], XtNy, -report->slider_y);

    XtSetValues(child, args, TWO);
}

/*	Function Name: PortholeCallback
 *	Description: called when the porthole or its child has
 *                   changed 
 *	Arguments: porthole - the porthole widget.
 *                 panner_ptr - the panner widget.
 *                 report_ptr - the porthole record.
 *	Returns: none.
 */

/* ARGSUSED */
void 
PortholeCallback(w, panner_ptr, report_ptr)
Widget w;
XtPointer panner_ptr, report_ptr;
{
    Arg args[10];
    Cardinal n = 0;
    XawPannerReport *report = (XawPannerReport *) report_ptr;
    Widget panner = (Widget) panner_ptr;

    XtSetArg (args[n], XtNsliderX, report->slider_x); n++;
    XtSetArg (args[n], XtNsliderY, report->slider_y); n++;
    if (report->changed != (XawPRSliderX | XawPRSliderY)) {
	XtSetArg (args[n], XtNsliderWidth, report->slider_width); n++;
	XtSetArg (args[n], XtNsliderHeight, report->slider_height); n++;
	XtSetArg (args[n], XtNcanvasWidth, report->canvas_width); n++;
	XtSetArg (args[n], XtNcanvasHeight, report->canvas_height); n++;
    }
    XtSetValues (panner, args, n);
}

/* ARGSUSED */
void
PlayerScrollbarCallback (w, closure, data)
    Widget  w;
    XtPointer	closure, data;
{
    Widget	player_w = (Widget) closure;
    float	pos = *((float *) data);
    Arg		args[10];
    Cardinal	n;
    Dimension	player_width;
    Position	player_x;

    n = 0;
    XtSetArg (args[n], XtNwidth, &player_width); n++;
    XtGetValues (player_w, args, n);
    player_x = -((float) player_width) * pos;
    n = 0;
    XtSetArg (args[n], XtNx, player_x); n++;
    XtSetValues (player_w, args, n);
}

/*ARGSUSED*/
void
PlayerPortholeCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    float  top, shown;
    XawPannerReport *report = (XawPannerReport *) data;
    Widget scrollbar = (Widget) closure;

    top = ((float) report->slider_x) / ((float) report->canvas_width);
    shown = ((float) report->slider_width) / ((float) report->canvas_width);
    XawScrollbarSetThumb (scrollbar, top, shown);
}

MakeLastPlayerVisible ()
{
    Arg		args[10];
    Cardinal	n;
    Dimension	player_width, porthole_width;
    Position	x;

    n = 0;
    XtSetArg (args[n], XtNwidth, &player_width); n++;
    XtGetValues (player_w, args, n);
    n = 0;
    XtSetArg (args[n], XtNwidth, &porthole_width); n++;
    XtGetValues (player_porthole, args, n);
    x = porthole_width - player_width;
    if (x < 0)
    {
	n = 0;
	XtSetArg (args[n], XtNx, x); n++;
	XtSetValues (player_w, args, n);
    }
}

/*ARGSUSED*/
static void
NewGameCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    NewGame ();
}

/*ARGSUSED*/
static void
QuitCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Quit ();
}

/*ARGSUSED*/
static void
UndoCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Undo ();
}

/*ARGSUSED*/
static void
DrawCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Draw ();
}

/*ARGSUSED*/
static void
HintCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Hint ();
}

/*ARGSUSED*/
static void
RestoreCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Restore ();
}

/*ARGSUSED*/
static void
SaveCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Save ();
}

static int	    selected_player;
static DominoPtr    selected_domino;

/*ARGSUSED*/
static void
BoardCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    DominosInputPtr input = (DominosInputPtr) data;

    if (strcmp (*input->params, "dest") != 0)
    {
	selected_domino = 0;
	selected_player = 0;
	return;
    }
    if (!selected_domino)
	return;
    Play (&player[selected_player], selected_domino, 
	  input->domino, input->direction, input->distance);
}

/*ARGSUSED*/
static void
PlayerCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    DominosInputPtr input = (DominosInputPtr) data;

    selected_domino = 0;
    selected_player = 0;
    if (strcmp (*input->params, "source") != 0)
	return;
    if (input->domino && input->distance == 0)
    {
	selected_player = (int) closure;
	selected_domino = input->domino;
    }
}

    
/*ARGSUSED*/
static void UndoAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Undo ();
}
    
/*ARGSUSED*/
static void NewGameAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    NewGame ();
}
    
/*ARGSUSED*/
static void QuitAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Quit ();
}

/*ARGSUSED*/
static void HintAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Hint ();
}

/*ARGSUSED*/
static void RestoreAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Restore ();
}

/*ARGSUSED*/
static void SaveAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Save ();
}

/*ARGSUSED*/
static void DrawAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Draw ();
}

/*ARGSUSED*/
static void YesAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    yn_answer = 1;
    yn_done = 1;
}

/*ARGSUSED*/
static void NoAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    yn_answer = 0;
    yn_done = 0;
}

XtActionsRec	actions[] = {
    "dominosUndo",	UndoAction,
    "dominosNewGame",	NewGameAction,
    "dominosQuit",	QuitAction,
    "dominosHint",	HintAction,
    "dominosRestore",	RestoreAction,
    "dominosSave",	SaveAction,
    "dominosDraw",	DrawAction,
    "dominosYes",	YesAction,
    "dominosNo",	NoAction,
};

struct menuEntry {
    char    *name;
    void    (*function)();
};

struct menuEntry fileMenuEntries[] = {
    "restore", RestoreCallback,
    "save", SaveCallback,
    "quit", QuitCallback,
};
    
Widget
CreateMenu (parent, name, entries, count)
    Widget  parent;
    char    *name;
    struct menuEntry	*entries;
    int	    count;
{
    Widget  menu;
    Widget  entry;
    int	    i;

    menu = XtCreatePopupShell (name, simpleMenuWidgetClass,
			       parent, NULL, ZERO);
    for (i = 0; i < count; i++) {
	entry = XtCreateManagedWidget (entries[i].name,
				       smeBSBObjectClass, menu, NULL, ZERO);
	XtAddCallback (entry, XtNcallback, entries[i].function, NULL);
    }
    return menu;
}

#define offset(field) XtOffsetOf(DominosResources, field)

XtResource resources[] = {
    { "animationSpeed", "AnimationSpeed", XtRInt, sizeof (int),
     offset(animationSpeed), XtRImmediate, (XtPointer) -1},
    { "saveFile", "SaveFile", XtRString, sizeof (String),
     offset(saveFile), XtRString, NULL},
};

XrmOptionDescRec options[] = {
    "-squareCards",	"*Cards.roundCards",	XrmoptionNoArg, "False",
};

makeDefaultSaveFile ()
{
    if (!dominosResources.saveFile || !*dominosResources.saveFile)
    {
	char	path[1024], *malloc ();

	sprintf (path, "%s/%s", getenv ("HOME"), ".dominos");
	dominosResources.saveFile = malloc (strlen (path) + 1);
	strcpy (dominosResources.saveFile, path);
    }
}

main (argc, argv)
    int	    argc;
    char    **argv;
{
    Atom	wm_delete_window;
    int		i;
    
    toplevel = XtInitialize (argv[0], "Dominos", options, XtNumber(options),
			     &argc, argv);
    
    XtGetApplicationResources (toplevel, (XtPointer)&dominosResources, resources,
			       XtNumber (resources), NULL, 0);
    
    makeDefaultSaveFile ();

    AnimateSetSpeed (dominosResources.animationSpeed);
    
    XtAddActions (actions, XtNumber(actions));

    XtOverrideTranslations 
	(toplevel, 
	 XtParseTranslationTable ("<Message>WM_PROTOCOLS: dominosQuit()"));
    frame = XtCreateManagedWidget ("frame", layoutWidgetClass, toplevel, NULL, 0);
    menuBar = XtCreateManagedWidget ("menuBar", layoutWidgetClass, frame, NULL, 0);
    fileMenuButton = XtCreateManagedWidget ("fileMenuButton",
					    menuButtonWidgetClass,
					    menuBar, NULL, ZERO);
    fileMenu = CreateMenu (fileMenuButton, "fileMenu", 
			   fileMenuEntries, XtNumber (fileMenuEntries));
    newGame = XtCreateManagedWidget ("newGame", commandWidgetClass,
				     menuBar, NULL, ZERO);
    XtAddCallback(newGame, XtNcallback, NewGameCallback, NULL);
    undo = XtCreateManagedWidget ("undo", commandWidgetClass,
				  menuBar, NULL, ZERO);
    XtAddCallback(undo, XtNcallback, UndoCallback, NULL);
    hint = XtCreateManagedWidget ("hint", commandWidgetClass,
				  menuBar, NULL, ZERO);
    XtAddCallback(hint, XtNcallback, HintCallback, NULL);

    draw = XtCreateManagedWidget ("draw", commandWidgetClass,
				  menuBar, NULL, ZERO);
    XtAddCallback(draw, XtNcallback, DrawCallback, NULL);

    for (i = 0; i < NumPlayers; i++)
    {
	char	foo[32];

	sprintf (foo, "score%d", i);
	score_w[i] = XtCreateManagedWidget(foo, labelWidgetClass,
					   menuBar, NULL, ZERO);
    }
    porthole = XtCreateManagedWidget("porthole", portholeWidgetClass,
				     frame, NULL, ZERO);
    
    panner = XtCreateManagedWidget("panner", pannerWidgetClass,
				   frame, NULL, ZERO);
    
    board_w = XtCreateManagedWidget ("board", dominosWidgetClass, porthole, NULL, 0);

    XtAddCallback (board_w, XtNinputCallback, BoardCallback, NULL);
    
    XtAddCallback(porthole, XtNreportCallback, PortholeCallback,
		  (XtPointer) panner);

    XtAddCallback(panner, XtNreportCallback, PannerCallback,
		  (XtPointer) board_w);
    
    player_porthole = XtCreateManagedWidget("player_porthole", portholeWidgetClass,
					  frame, NULL, ZERO);

    player_scrollbar = XtCreateManagedWidget("player_scrollbar", scrollbarWidgetClass,
					     frame, NULL, ZERO);

    player_w = XtCreateManagedWidget ("player", dominosWidgetClass, 
				      player_porthole, NULL, 0);
    
    XtAddCallback (player_porthole, XtNreportCallback, PlayerPortholeCallback,
		   (XtPointer) player_scrollbar);

    XtAddCallback (player_scrollbar, XtNjumpProc, PlayerScrollbarCallback,
		   (XtPointer) player_w);
    
    XtAddCallback(player_w, XtNinputCallback, PlayerCallback, NULL);

    message = XtCreateManagedWidget ("message", labelWidgetClass, frame, NULL, 0);
    
    computerCount = XtCreateManagedWidget ("computerCount", labelWidgetClass, frame, NULL, ZERO);
    
    srandom (getpid () ^ time ((long *) 0));
    
    Message (message, "Keith's Dominos, Version 1.0");
    if (!Restore ())
    {
	NewGame ();
	DisplayScores ();
    }
    
    XtRealizeWidget (toplevel);
    wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				   False);
    (void) XSetWMProtocols (XtDisplay(toplevel), XtWindow(toplevel),
                            &wm_delete_window, 1);

    XtMainLoop ();
    return 0;
}
