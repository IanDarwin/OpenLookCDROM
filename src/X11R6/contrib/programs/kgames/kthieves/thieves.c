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
# include	<Xkw/Cards.h>
# include	<Xkw/Layout.h>
# include	<X11/Xutil.h>
# include	<Xkw/CardsUtil.h>

Widget	    toplevel;
Widget	    frame;
Widget	    deck;
Widget	    draw;
Widget	    piles;
Widget	    stacks;
Widget	    message;
Widget	    deckCount;
Widget	    menuBar;
Widget	    fileMenuButton;
Widget	    fileMenu;
Widget	    newGame;
Widget	    undo;
Widget	    hint;
Widget	    score;

#define MAX_STACKS  104
#define DEF_NUM_STACKS  10
#define NUM_PILES   8
#define NUM_CARDS   104
#define DEF_FIRST_ROWS  4

CardStackRec	deckStack;
CardStackRec	stackStacks[MAX_STACKS];
CardStackRec	pileStacks[NUM_PILES];
CardStackRec	drawStack;

CardRec		rawcards[NUM_CARDS];

CardStackPtr	fromStack;
CardPtr		fromCard;
int		dealNumber;

typedef struct _thievesResources {
    int		animationSpeed;
    Boolean    	squishCards;
    int		numColumns;
    int		numRows
} ThievesResources, *ThievesResourcesPtr;

ThievesResources thievesResources;

#define numStacks   thievesResources.numColumns
#define firstRows   thievesResources.numRows

InitStacks ()
{
    int	    col;
    int	    row;
    CardDisplay	    display;
    
    for (col = 0; col < numStacks; col++)
    {
	display = CardDisplayAll;
	if (thievesResources.squishCards)
	    display = CardDisplaySome;
	CardInitStack (&stackStacks[col],
		       stacks, CardsNone, False, col, display);
    }
    for (col = 0; col < NUM_PILES; col++)
    {
	row = col >> 1;
	CardInitStack (&pileStacks[col],
		       piles, CardsEmpty, False, col & 1, CardDisplayTop);
	pileStacks[col].basePosition = row;
    }
    CardInitStack (&deckStack,
		   deck, CardsEmpty, False, 0, CardDisplayBottom);
    CardInitStack (&drawStack,
		   draw, CardsNone, True, 0, CardDisplayAll);
}

GenerateCards ()
{
    int		    i;
    CardPtr	    card;

    card = rawcards;
    for (i = 0; i < 2; i++) 
    {
	CardGenerateStandardDeck (card);
	card += 52;
    }
    rawcards[51].next = &rawcards[52];
    rawcards[52].prev = &rawcards[51];
    deckStack.first = &rawcards[0];
    deckStack.last = &rawcards[NUM_CARDS-1];
}

FirstDeal ()
{
    int	    row, col;
    
    for (row = 0; row < firstRows; row++) 
    {
	for (col = 0; col < numStacks; col++) 
	{
	    CardMove (&deckStack, deckStack.last, &stackStacks[col], False);
	    CardTurn (stackStacks[col].last, CardFaceUp, False);
	}
    }
    dealNumber = (52 * 2) - (firstRows * numStacks);
    Message (deckCount, "%d", dealNumber);
}

CheckStackTop (stack)
    CardStackPtr    stack;
{
    if (stack->last && stack->last->face == CardFaceDown)
	CardTurn (stack->last, CardFaceUp, True);
}

/*
 * Score:
 
 each card played on the right counts as its face value
 ace ==> 1
 duece ==> 2

10 ==> 10
jack ==> 11
queen ==> 12
king ==> 13

Just total up the points and that is the score.
a winning game has 728 points.

 */

int
ComputeScore ()
{
    int		    score = 0;
    int		    col;
    CardStackPtr    pile;
    CardPtr	    card;
    int		    numPiles = 0;

    for (col = 0; col < NUM_PILES; col++)
    {
	pile = &pileStacks[col];
	for (card = pile->first; card; card = card->next)
	    score += CardsRankToInt (card->card.rank) + 1;
    }
    
    return (score);
}
    
DisplayStacks ()
{
    int		    col;
    CardPtr	    card, c;
    CardStackPtr    stack;
    
    CardDisplayStack (&deckStack);
    CardDisplayStack (&drawStack);
    
    for (col = 0; col < NUM_PILES; col++)
	CardDisplayStack (&pileStacks[col]);
    
    for (col = 0; col < numStacks; col++)
    {
	stack = &stackStacks[col];
	if (thievesResources.squishCards)
	{
	    for (card = stack->last; card;)
	    {
		card->shouldBeUp = True;
		if (card->face == CardFaceDown)
		{
		    while (card->prev && card->prev->face == CardFaceDown)
		    {
			card->shouldBeUp = False;
			card = card->prev;
		    }
		}
		else
		{
		    c = CardInReverseSuitOrder (card);
		    if (c != card)
		    {
			while ((card = card->prev) != c)
			    card->shouldBeUp = False;
			card->shouldBeUp = True;
		    }
		}
		card = card->prev;
	    }
	}
	CardDisplayStack (stack);
    }
    CardsUpdateDisplay (deck);
    CardsUpdateDisplay (draw);
    CardsUpdateDisplay (piles);
    CardsUpdateDisplay (stacks);
}

/* User interface functions */

void
ResetDealNumber (closure)
    char    *closure;
{
    dealNumber = (int) closure;
    Message (deckCount, "%d", dealNumber);
}

void
Deal ()
{
    CardPtr   c;

    if (!deckStack.last) 
    {
	Message (message, "No more cards in the deck.");
	return;
    }
    CardMove (&deckStack, deckStack.last, &drawStack, True);
    CardTurn (drawStack.last, CardFaceUp, True);
    CardRecordHistoryCallback (ResetDealNumber, (char *) dealNumber);
    Message (deckCount, "%d", --dealNumber);
}

void
NewGame ()
{
    CardsRemoveAllCards (deck);
    CardsRemoveAllCards (piles);
    CardsRemoveAllCards (stacks);
    CardsRemoveAllCards (draw);
    fromStack = 0;
    fromCard = 0;
    InitStacks ();
    GenerateCards ();
    CardShuffle (&deckStack);
    FirstDeal ();
    CardInitHistory ();
    DisplayStacks ();
}

void
Undo ()
{
    if (!CardUndo ())
	Message (message, "Nothing to undo.");
    DisplayStacks ();
}

void
Score ()
{
    Message (message, "Current position scores %d out of 728.", 
	     ComputeScore ());
}

void
Quit ()
{
    exit (0);
}

CardStackPtr
FindFinishPlay (from_card)
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    CardPtr	    card;
    
    for (i = 0; i < NUM_PILES; i++)
    {
	to_stack = &pileStacks[i];
	if (from_card->card.rank == CardsAce && to_stack->last == NULL ||
	    to_stack->last && CardIsInSuitOrder (to_stack->last, from_card))
	{
	    return to_stack;
	}
    }
    return NULL;
}

CardStackPtr
FindStackPlay (from_card)
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    CardPtr	    card;
    
    for (i = 0; i < numStacks; i++)
    {
	to_stack = &stackStacks[i];
	card = to_stack->last;
	if (card && CardIsInSuitOrder (from_card, card))
	    return to_stack;
    }
    return NULL;
}

CardStackPtr
FindEmptyPlay (from_card)
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    
    for (i = 0; i < numStacks; i++)
    {
	to_stack = &stackStacks[i];
	if (to_stack->last == NULL)
	    return to_stack;
    }
    return NULL;
}

void
Play (from_stack, from_card, to_stack)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
{
    int		    i;
    CardPtr	    card;

    if (to_stack != from_stack)
    {
	if (to_stack->last != NULL && !
	    CardIsInOrder (from_card, to_stack->last))
	{
	    Message (message, "Can't move the %P to the %P.",
		     &from_card->card, &to_stack->last->card);
	    return;
	}
    } else {
	if (!(to_stack = FindFinishPlay (from_card)) &&
	    !(to_stack = FindStackPlay (from_card)) &&
	    !(to_stack = FindEmptyPlay (from_card)))
	{
	    Message (message, "Nowhere to move the %P.", &from_card->card);
	    return;
	}
    }
    CardMove (from_stack, from_card, to_stack, True);
}

static Boolean
AlreadyEmpty (a,b)
    CardPtr a, b;
{
    return !b;
}

FindAMove ()
{
    int		    col;
    CardStackPtr    from_stack, to_stack;
    CardPtr	    from_card;
    Boolean	    goodenough[MAX_STACKS];

    to_stack = NULL;
    for (col = 0; col < numStacks; col++)
	goodenough[col] = False;
    
#define FindOneInStack(from_stack,func) \
    if (from_card = (from_stack)->last) \
	to_stack = func(from_card);

#define FindOneCheck(already, func) \
    for (col = 0; !to_stack && col < numStacks; col++) {\
	if (goodenough[col]) continue; \
	from_stack = &stackStacks[col]; \
	if (!from_stack->last) continue; \
	from_card = from_stack->last; \
	if (!from_card->prev || !already (from_card, from_card->prev)) \
	    to_stack = func(from_card); \
	else \
	    goodenough[col] = True; \
    } \
    if (!to_stack) { \
	FindOneInStack (&drawStack,func); \
    }
    
#define FindOne(func) \
    for (col = 0; !to_stack && col < numStacks; col++) {\
	from_stack = &stackStacks[col]; \
	FindOneInStack(from_stack,func); \
    } \
    if (!to_stack) { \
	FindOneInStack (&drawStack,func); \
    }

    FindOneCheck (CardIsInSuitOrder, FindStackPlay);
    if (to_stack) {
	Message (message, "Move the %P to the %P.", &from_card->card,
		 &to_stack->last->card);
	return;
    }
    FindOne (FindFinishPlay);
    if (to_stack) {
	Message (message, "Move the %P to the finish.", &from_card->card);
	return;
    }
    FindOneCheck (AlreadyEmpty, FindEmptyPlay);
    if (to_stack) {
	Message (message, "Move the %P to column %d", &from_card->card,
		 to_stack - stackStacks + 1);
	return;
    }
    if (deckStack.last) {
	Message (message, "Deal the next card.");
    } else {
	Message (message, "It's all over.");
    }
}

Restore ()
{
    Message (message, "Restore not implemented");
}

Save ()
{
    Message (message, "Save not implemented");
}

Expand (stack)
    CardStackPtr    stack;
{
    CardPtr card, t;

    if (card = stack->first) {
	MessageStart ();
	MessageAppend ("Column contains:");
	while (card) {
	    if (card->face == CardFaceUp)
	    {
		MessageAppend (" %p", &card->card);
		t = CardInSuitOrder (card);
		if (t != card && t != card->next)
		{
		    card = t;
		    MessageAppend ("-%p", &card->card);
		}
	    }
	    card = card->next;
	}
	MessageAppend (".");
	MessageEnd (message);
    }
    else
	Message (message, "Column is empty");
}

/* Callbacks to user interface functions */

static void
DeckCallback (w, closure, data)
    Widget	w;
    XtPointer	closure;
    XtPointer	data;
{
    CardsInputPtr    input = (CardsInputPtr) data;
    CardStackPtr    stack;
    CardPtr	    card;
    
    Message (message, "");
    Deal ();
    CardNextHistory ();
    DisplayStacks ();
}

static void
StackCallback (w, closure, data)
    Widget	w;
    XtPointer	closure;
    XtPointer	data;
{
    CardsInputPtr    input = (CardsInputPtr) data;
    CardStackPtr    stack;
    CardPtr	    card;
    String	    type;
    int		    i;

    Message (message, "");
    if (w == stacks)
	stack = &stackStacks[input->col];
    else if (w == draw)
	stack = &drawStack;
    card = stack->last;
    if (*input->num_params) {
	type = *input->params;
	if (!strcmp (type, "source"))
	{
	    fromStack = stack;
	    if (fromStack->last)
		fromCard = fromStack->last;
	    else
		Message (message, "Selected stack is empty.");
	}
	else if (!strcmp (type, "dest"))
	{
	    if (fromCard)
	    {
		Play (fromStack, fromCard, stack);
		fromCard = NULL;
		CardNextHistory ();
		DisplayStacks ();
	    }
	}
	else if (!strcmp (type, "expand"))
	{
	    Expand (stack);
	}
    }
}

static void
NewGameCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    NewGame ();
}

static void
QuitCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Quit ();
}

static void
ScoreCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Score ();
}

static void
UndoCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Undo ();
}

static void
FindAMoveCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    FindAMove ();
}

static void
RestoreCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Restore ();
}

static void
SaveCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    Save ();
}

/* actions to user interface functions */

static void UndoAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Undo ();
}
    
static void NewGameAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    NewGame ();
}
    
static void ScoreAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Score ();
}
    
static void QuitAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Quit ();
}

static void FindAMoveAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    FindAMove ();
}

static void RestoreAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Restore ();
}

static void SaveAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    Save ();
}

XtActionsRec	actions[] = {
    "thievesUndo",	UndoAction,
    "thievesNewGame",	NewGameAction,
    "thievesScore",	ScoreAction,
    "thievesQuit",	QuitAction,
    "thievesFindAMove",	FindAMoveAction,
    "thievesRestore",	RestoreAction,
    "thievesSave",	SaveAction,
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

#define offset(field) XtOffsetOf(ThievesResources, field)

XtResource resources[] = {
    { "animationSpeed", "AnimationSpeed", XtRInt, sizeof (int),
     offset(animationSpeed), XtRImmediate, (XtPointer) -1},
    { "squishCards", "SquishCards", XtRBoolean, sizeof (Boolean),
     offset(squishCards), XtRImmediate, (XtPointer) FALSE},
    { "numColumns", "NumColumns", XtRInt, sizeof (int),
     offset(numColumns), XtRImmediate, (XtPointer) DEF_NUM_STACKS},
    { "numRows", "NumRows", XtRInt, sizeof (int),
     offset(numRows), XtRImmediate, (XtPointer) DEF_FIRST_ROWS},
};

XrmOptionDescRec options[] = {
    "-squish",		"*squishCards",		XrmoptionNoArg,	"True",
    "-squareCards",	"*Cards.roundCards",	XrmoptionNoArg, "False",
    "-smallCards",	"*Cards.smallCards",	XrmoptionNoArg, "True",
    "-noanimate",	"*animationSpeed",	XrmoptionNoArg, "0",
    "-animationSpeed",	"*animationSpeed",	XrmoptionSepArg, NULL,
    "-cols",		".numColumns",		XrmoptionSepArg, NULL,
    "-rows",		".numRows",		XrmoptionSepArg, NULL,
};

main (argc, argv)
    int	    argc;
    char    **argv;
{
    Arg	args[20];
    int	nargs;
    Atom wm_delete_window;

    toplevel = XtInitialize (argv[0], "KThieves", options, XtNumber(options),
			     &argc, argv);
    
    XtGetApplicationResources (toplevel, (XtPointer)&thievesResources, resources,
			       XtNumber (resources), NULL, 0);
    
    AnimateSetSpeed (thievesResources.animationSpeed);
    
    XtAddActions (actions, XtNumber(actions));

    XtOverrideTranslations 
	(toplevel, 
	 XtParseTranslationTable ("<Message>WM_PROTOCOLS: thievesQuit()"));
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
    XtAddCallback(hint, XtNcallback, FindAMoveCallback, NULL);
    score = XtCreateManagedWidget ("score", commandWidgetClass,
				   menuBar, NULL, ZERO);
    XtAddCallback(score, XtNcallback, ScoreCallback, NULL);
    deck = XtCreateManagedWidget ("deck", cardsWidgetClass, frame, NULL, 0);
    deckCount = XtCreateManagedWidget ("deckCount", labelWidgetClass, frame, NULL, 0);
    XtAddCallback (deck, XtNinputCallback, DeckCallback, NULL);
    draw = XtCreateManagedWidget ("draw", cardsWidgetClass, frame, NULL, 0);
    XtAddCallback (draw, XtNinputCallback, StackCallback, NULL);
    piles = XtCreateManagedWidget ("piles", cardsWidgetClass, frame, NULL, 0);
    nargs = 0;
    XtSetArg (args[nargs], XtNnumCols, numStacks); nargs++;
    stacks = XtCreateManagedWidget ("stacks", cardsWidgetClass, frame, args, nargs);
    XtAddCallback (stacks, XtNinputCallback, StackCallback, NULL);
    message = XtCreateManagedWidget ("message", labelWidgetClass, frame, NULL, 0);
    srandom (getpid () ^ time ((long *) 0));
    NewGame ();
    XtRealizeWidget (toplevel);
    wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				   False);
    (void) XSetWMProtocols (XtDisplay(toplevel), XtWindow(toplevel),
                            &wm_delete_window, 1);

    XtMainLoop ();
}
