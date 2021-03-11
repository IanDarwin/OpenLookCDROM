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
Widget	    piles;
Widget	    stacks;
Widget	    suits;
Widget	    message;
Widget	    menuBar;
Widget	    fileMenuButton;
Widget	    fileMenu;
Widget	    newGame;
Widget	    undo;
Widget	    hint;
Widget	    score;
Widget	    pileAll;

#define NUM_STACKS  10
#define NUM_PILES   4
#define NUM_SUITS   4
#define NUM_CARDS   52

CardStackRec	stackStacks[NUM_STACKS];
CardStackRec	pileStacks[NUM_PILES];
CardStackRec	suitStacks[NUM_SUITS];

CardRec		rawcards[NUM_CARDS];

CardStackPtr	fromStack;
CardPtr		fromCard;

typedef struct _ktowersResources {
    int		animationSpeed;
} KtowersResources, *KtowersResourcesPtr;

KtowersResources ktowersResources;

InitStacks ()
{
    int		    col;
    CardDisplay	    display;
    
    for (col = 0; col < NUM_STACKS; col++)
    {
	CardInitStack (&stackStacks[col],
		       stacks, CardsNone, False, col, CardDisplayAll);
    }
    for (col = 0; col < NUM_PILES; col++)
    {
	CardInitStack (&pileStacks[col],
		       piles, CardsEmpty, False, col, CardDisplayTop);
    }
    for (col = 0; col < NUM_SUITS; col++)
    {
	CardInitStack (&suitStacks[col],
		       suits, CardsEmpty, False, col, CardDisplayTop);
    }
}

GenerateCards ()
{
    CardGenerateStandardDeck (rawcards);
    suitStacks[0].first = &rawcards[0];
    suitStacks[0].last = &rawcards[NUM_CARDS-1];
}

#define FIRST_ROWS  5

FirstDeal ()
{
    int	    row, col;
    
    for (row = 0; row < FIRST_ROWS; row++) 
    {
	for (col = 0; col < NUM_STACKS; col++) 
	{
	    CardMove (&suitStacks[0], suitStacks[0].last, &stackStacks[col], False);
	    CardTurn (stackStacks[col].last, CardFaceUp, False);
	}
    }
    col = 1;
    while (suitStacks[0].last)
    {
	CardMove (&suitStacks[0], suitStacks[0].last, &pileStacks[col], False);
	CardTurn (pileStacks[col].last, CardFaceUp, False);
	col++;
    }
}

int
ComputeScore ()
{
    int	    col;
    int	    score;
    CardPtr card;

    score = 0;
    for (col = 0; col < NUM_SUITS; col++)
    {
	for (card = suitStacks[col].first; card; card = card->next)
	{
	    if (card->card.rank < CardsJack)
		score += (int) card->card.rank;
	    else
		score += 10;
	}
    }
    return score;
}
    
DisplayStacks ()
{
    int		    col;
    
    for (col = 0; col < NUM_SUITS; col++)
	CardDisplayStack (&suitStacks[col]);
    
    for (col = 0; col < NUM_PILES; col++)
	CardDisplayStack (&pileStacks[col]);
    
    for (col = 0; col < NUM_STACKS; col++)
	CardDisplayStack (&stackStacks[col]);
    CardsUpdateDisplay (suits);
    CardsUpdateDisplay (piles);
    CardsUpdateDisplay (stacks);
}

/* User interface functions */

void
NewGame ()
{
    CardsRemoveAllCards (suits);
    CardsRemoveAllCards (piles);
    CardsRemoveAllCards (stacks);
    fromStack = 0;
    fromCard = 0;
    InitStacks ();
    GenerateCards ();
    CardShuffle (&suitStacks[0]);
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
    Message (message, "Current position scores %d out of 340.", 
	     ComputeScore ());
}

void
Quit ()
{
    exit (0);
}

Boolean
IsLegalSuitPlay (from_stack, from_card, to_stack)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
{
    if (from_card->card.rank == CardsAce)
    {
	if (to_stack->last == NULL)
	    return True;
    } 
    else 
    {
	if (to_stack->last != NULL &&
	    CardIsInSuitOrder (to_stack->last, from_card))
	    return True;
    }
    return False;
}

CardStackPtr
FindSuitPlay (from_stack, from_card)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    
    for (i = 0; i < NUM_SUITS; i++)
    {
	to_stack = &suitStacks[i];
	if (IsLegalSuitPlay (from_stack, from_card, to_stack))
	    return to_stack;
    }
    return NULL;
}

Boolean
IsLegalPilePlay (from_stack, from_card, to_stack)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
{
    if (to_stack->last == NULL)
	return True;
    return False;
}

CardStackPtr
FindPilePlay (from_stack, from_card)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    
    for (i = 0; i < NUM_PILES; i++)
    {
	to_stack = &pileStacks[i];
	if (IsLegalPilePlay (from_stack, from_card, to_stack))
	    return to_stack;
    }
    return NULL;
}

Boolean
IsLegalRegularPlay (from_stack, from_card, to_stack)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
{
    CardPtr to_card;

    to_card = to_stack->last;
    if (to_card && CardIsInSuitOrder (from_card, to_card))
	return True;
    return False;
}

CardStackPtr
FindRegularPlay (from_stack, from_card)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
{
    int		    i;
    CardStackPtr    to_stack;
    
    for (i = 0; i < NUM_STACKS; i++)
    {
	to_stack = &stackStacks[i];
	if (IsLegalRegularPlay (from_stack, from_card, to_stack))
	    return to_stack;
    }
    return NULL;
}

Boolean
IsLegalEmptyPlay (from_stack, from_card, to_stack)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
{
    if (from_card->card.rank == CardsKing && to_stack->last == NULL)
	return True;
    return False;
}

CardStackPtr
FindEmptyPlay (from_stack, from_card)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
{
    int		    i;
    int		    col = from_stack - stackStacks;
    CardStackPtr    to_stack;
    
    for (i = 0; i < NUM_STACKS; i++)
    {
	to_stack = &stackStacks[i];
	if (IsLegalEmptyPlay (from_stack, from_card, to_stack))
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

    if (!from_card)
    {
	from_card = from_stack->last;
	if (!from_card)
	{
	    Message (message, "No cards there.");
	    return;
	}
    }
    if (to_stack != from_stack)
    {
	if (to_stack->widget == stacks)
	{
	    if (!IsLegalRegularPlay (from_stack, from_card, to_stack) &&
		!IsLegalEmptyPlay (from_stack, from_card, to_stack))
	    {
		if (!to_stack->last)
		    Message (message, "Can't move the %P to an empty pile.",
			     &from_card->card);
		else
		    Message (message, "Can't move the %P to the %P.",
			     &from_card->card, &to_stack->last->card);
		return;
	    }
	}
	else if (to_stack->widget == piles)
	{
	    if (!IsLegalPilePlay (from_stack, from_card, to_stack))
	    {
		if (!to_stack->last)
		    Message (message, "Can't move the %P to an empty stack.",
			     &from_card->card);
		else
		    Message (message, "Can't move the %P to the %P.",
			     &from_card->card, &to_stack->last->card);
		return;
	    }
	} 
	else if (to_stack->widget == suits)
	{
	    if (!IsLegalSuitPlay (from_stack, from_card, to_stack))
	    {
		if (!to_stack->last)
		    Message (message, "Can't move the %P to an empty suit.",
			     &from_card->card);
		else
		    Message (message, "Can't move the %P to the %P.",
			     &from_card->card, &to_stack->last->card);
		return;
	    }
	}
    } else {
	if (!(to_stack = FindSuitPlay (from_stack, from_card)) &&
	    !(to_stack = FindRegularPlay (from_stack, from_card)) &&
	    !(to_stack = FindEmptyPlay (from_stack, from_card)) &&
	    !(to_stack = FindPilePlay (from_stack, from_card)))
	{
	    Message (message, "Not a valid move.");
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

static Boolean
AlreadyInOrder (a, b)
    CardPtr a, b;
{
    if (a && b && CardInSuitOrder (a,b))
	return True;
    return False;
}

FindAMove ()
{
    int		    col;
    CardStackPtr    from_stack, to_stack;
    CardPtr	    from_card;

    to_stack = NULL;
    
#define FindOne(func) \
    for (col = 0; !to_stack && col < NUM_STACKS; col++) {\
	from_stack = &stackStacks[col]; \
	if (!from_stack->last) continue; \
	from_card = from_stack->last; \
	to_stack = func(from_stack, from_card); \
    } \
    for (col = 0; !to_stack && col < NUM_PILES; col++) {\
	from_stack = &pileStacks[col]; \
	if (!from_stack->last) continue; \
	from_card = from_stack->last; \
	to_stack = func(from_stack, from_card); \
    }

    FindOne (FindSuitPlay);
    if (to_stack) {
	if (to_stack->last)
	{
	    Message (message, "Move the %P to the %P.", &from_card->card,
			 &to_stack->last->card);
        }
        else
        {
	    Message (message, "Move the %P", &from_card->card);
	}
        return;
    }
    FindOne (FindRegularPlay);
    if (to_stack) {
	Message (message, "Move the %P to the %P.", &from_card->card,
		 &to_stack->last->card);
	return;
    }
    FindOne (FindEmptyPlay);
    if (to_stack) {
	Message (message, "Move the %P to column %d", &from_card->card,
		 to_stack - stackStacks + 1);
	return;
    }
    FindOne (FindPilePlay);
    if (to_stack) {
	Message (message, "Move the %P to a pile.", &from_card->card);
	return;
    }
    Message (message, "It's all over.");
}

Restore ()
{
    Message (message, "Restore not implemented");
}

Save ()
{
    Message (message, "Save not implemented");
}

PileAll ()
{
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
    int		    col;
    Boolean	    done = False;

    Message (message, "");
    do {
	to_stack = 0;
	FindOne (FindSuitPlay);
	if (to_stack)
	{
	    Play (from_stack, from_card, to_stack);
	    done = True;
	    CardNextHistory ();
	    DisplayStacks ();
	}
    } while (to_stack);
    if (!done)
	Message (message, "No cards to pile.");
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
    else if (w == suits)
	stack = &suitStacks[input->col];
    else if (w == piles)
	stack = &pileStacks[input->col];
    else
	return;
    for (card = stack->last; card; card = card->prev)
	if (card->isUp && card->row == input->row)
	    break;
    if (*input->num_params) {
	type = *input->params;
	if (!strcmp (type, "source"))
	{
	    fromStack = stack;
	    fromCard = 0;
	    if (!fromStack->last)
		Message (message, "Selected stack is empty.");
	}
	else if (!strcmp (type, "dest"))
	{
	    if (fromStack)
	    {
		Play (fromStack, fromCard, stack);
		fromStack = NULL;
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

static void
PileAllCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    PileAll ();
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

static void PileAllAction (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    PileAll ();
}

XtActionsRec	actions[] = {
    "ktowersUndo",	UndoAction,
    "ktowersNewGame",	NewGameAction,
    "ktowersScore",	ScoreAction,
    "ktowersQuit",	QuitAction,
    "ktowersFindAMove",	FindAMoveAction,
    "ktowersRestore",	RestoreAction,
    "ktowersSave",	SaveAction,
    "ktowersPileAll",	PileAllAction,
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

#define offset(field) XtOffsetOf(KtowersResources, field)

XtResource resources[] = {
    { "animationSpeed", "AnimationSpeed", XtRInt, sizeof (int),
     offset(animationSpeed), XtRImmediate, (XtPointer) -1},
};

XrmOptionDescRec options[] = {
    "-smallCards",	"*Cards.smallCards",	XrmoptionNoArg, "True",
    "-squareCards",	"*Cards.roundCards",	XrmoptionNoArg, "False",
    "-noanimate",	"*animationSpeed",	XrmoptionNoArg, "0",
    "-animationSpeed",	"*animationSpeed",	XrmoptionSepArg, NULL
};

main (argc, argv)
    int	    argc;
    char    **argv;
{
    Arg	args[20];
    Atom wm_delete_window;

    toplevel = XtInitialize (argv[0], "KTowers", options, XtNumber(options),
			     &argc, argv);
    
    XtGetApplicationResources (toplevel, (XtPointer)&ktowersResources, resources,
			       XtNumber (resources), NULL, 0);
    
    AnimateSetSpeed (ktowersResources.animationSpeed);
    
    XtAddActions (actions, XtNumber(actions));

    XtOverrideTranslations 
	(toplevel, 
	 XtParseTranslationTable ("<Message>WM_PROTOCOLS: ktowersQuit()"));
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
    pileAll = XtCreateManagedWidget ("pileAll", commandWidgetClass,
				   menuBar, NULL, ZERO);
    XtAddCallback(pileAll, XtNcallback, PileAllCallback, NULL);
    suits = XtCreateManagedWidget ("suits", cardsWidgetClass, frame, NULL, 0);
    XtAddCallback (suits, XtNinputCallback, StackCallback, NULL);
    piles = XtCreateManagedWidget ("piles", cardsWidgetClass, frame, NULL, 0);
    XtAddCallback (piles, XtNinputCallback, StackCallback, NULL);
    stacks = XtCreateManagedWidget ("stacks", cardsWidgetClass, frame, NULL, 0);
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
