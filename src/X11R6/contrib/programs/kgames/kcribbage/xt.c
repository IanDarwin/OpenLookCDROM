# include	<stdio.h>
# include	"deck.h"
# include	"cribbage.h"
# include	<X11/Intrinsic.h>
# include	<X11/StringDefs.h>
# include	<X11/Shell.h>
# include	<X11/Xos.h>
# include	<X11/Xaw/Paned.h>
# include	<X11/Xaw/Form.h>
# include	<X11/Xaw/Box.h>
# include	<X11/Xaw/Command.h>
# include	<X11/Xaw/Dialog.h>
# include	<X11/Xaw/Label.h>
# include	<X11/Xaw/MenuButton.h>
# include	<X11/Xaw/SimpleMenu.h>
# include	<X11/Xaw/SmeBSB.h>
# include	<X11/Xaw/AsciiText.h>
/* # include	<X11/Xaw/Cardinals.h> */
# include	<Xkw/Hand.h>
# include	<Xkw/Cards.h>
# include	<Xkw/Thermo.h>
# include	<Xkw/Layout.h>
# include	<Xkw/Pad.h>
# include	"CribBoard.h"
# include	<X11/Xutil.h>

static Widget	    toplevel;
static Widget	    menuBar;
static Widget	    fileMenuButton;
static Widget	    fileMenu;
static Widget	    layout;
static Widget	    computer;
static Widget	    message;
static Widget	    player;
static Widget	    table;
static Widget	    playcrib;
static Widget	    playScore;
static Widget	    compScore;
static Widget	    compcrib;
static Widget	    tableScore;

typedef struct _cribbageResources {
    int		animationSpeed;
    Boolean    	explain;
    Boolean	quiet;
    Boolean	random;
} CribbageResources, *CribbageResourcesPtr;

CribbageResources cribbageResources;

#define NUM_CARDS   10

typedef struct _cribbageCard {
    CardsCardRec    card;
    XtPointer	    private;
} CribbageCardRec, *CribbageCardPtr;

static CribbageCardRec	computerCards[NUM_CARDS];
static CribbageCardRec	playerCards[NUM_CARDS];
static CribbageCardRec	tableCards[NUM_CARDS];
static CribbageCardRec	playcribCards[NUM_CARDS];
static CribbageCardRec	compcribCards[NUM_CARDS];

#define SCORE_WIDTH	41
#define SCORE_HEIGHT	9

#define CHAR_BUF    1024
static char textbuf[CHAR_BUF];
static int  text_in, text_out;

static void key_action (w, e, p, n)
    Widget  w;
    XEvent  *e;
    String  p;
    Cardinal*n;
{
    int	    len;
    KeySym  keysym;
    XComposeStatus  status;

    len = XLookupString (e, textbuf + text_in, CHAR_BUF - text_in, &keysym, &status);
    text_in += len;
}

XtActionsRec	actions[] = {
    "cribbageKey", key_action,
};

static void
QuitCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    quit ();
}

static int	selectedCard;
static Boolean	cardSelected;

static void
PlayerCallback (w, closure, data)
    Widget	w;
    XtPointer	closure, data;
{
    CardsInputPtr   input = (CardsInputPtr) data;
    
    selectedCard = input->row;
    cardSelected = True;
}

struct menuEntry {
    char    *name;
    void    (*function)();
};

struct menuEntry fileMenuEntries[] = {
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
			       parent, NULL, 0);
    for (i = 0; i < count; i++) {
	entry = XtCreateManagedWidget (entries[i].name,
				       smeBSBObjectClass, menu, NULL, 0);
	XtAddCallback (entry, XtNcallback, entries[i].function, NULL);
    }
    return menu;
}

#define offset(field) XtOffsetOf(CribbageResources, field)

XtResource resources[] = {
    { "animationSpeed", "AnimationSpeed", XtRInt, sizeof (int),
     offset(animationSpeed), XtRImmediate, (XtPointer) 500},
    { "explain", "Explain", XtRBoolean, sizeof (Boolean),
     offset(explain), XtRImmediate, (XtPointer) False},
    { "quiet", "Quiet", XtRBoolean, sizeof (Boolean),
     offset(quiet), XtRImmediate, (XtPointer) False},
    { "random", "Random", XtRBoolean, sizeof (Boolean),
     offset(random), XtRImmediate, (XtPointer) True},
};

XrmOptionDescRec options[] = {
    "-smallCards",	"*Cards.smallCards",	XrmoptionNoArg, "True",
    "-squareCards",	"*Cards.roundCards",	XrmoptionNoArg, "False",
    "-noanimate",	".animationSpeed",	XrmoptionNoArg, "0",
    "-animationSpeed",	".animationSpeed",	XrmoptionSepArg, NULL,
    "-explain",		".explain",		XrmoptionNoArg,	 "True",
    "-quiet",		".quiet",		XrmoptionNoArg,	 "True",
    "-random",		".random",		XrmoptionNoArg,	 "True",
};

UIInit (argc, argv)
    int	    argc;
    char    **argv;
{
    Arg		    args[10];
    Cardinal	    n;

    toplevel = XtInitialize (argv[0], "Cribbage", options, XtNumber(options), &argc, argv);
    
    XtGetApplicationResources (toplevel, (XtPointer)&cribbageResources, resources,
			       XtNumber (resources), NULL, 0);
    
    explain = cribbageResources.explain;
    quiet = cribbageResources.quiet;
    rflag = cribbageResources.random;

    XtAddActions (actions, XtNumber(actions));
    layout = XtCreateManagedWidget ("layout", layoutWidgetClass, toplevel, NULL, 0);
    menuBar = XtCreateManagedWidget ("menuBar", layoutWidgetClass, layout, NULL, 0);
    fileMenuButton = XtCreateManagedWidget ("fileMenuButton",
					    menuButtonWidgetClass,
					    menuBar, NULL, 0);
    fileMenu = CreateMenu (fileMenuButton, "fileMenu", 
			   fileMenuEntries, XtNumber (fileMenuEntries));
    player = XtCreateManagedWidget ("player", cardsWidgetClass, layout, NULL, 0);
    XtAddCallback (player, XtNinputCallback, PlayerCallback, NULL);
    computer = XtCreateManagedWidget ("computer", cardsWidgetClass, layout, NULL, 0);
    table = XtCreateManagedWidget ("table", cardsWidgetClass, layout, NULL, 0);
    tableScore = XtCreateManagedWidget ("tableScore", labelWidgetClass, layout, NULL, 0);
    playScore = XtCreateManagedWidget ("playscore", cribBoardWidgetClass, layout, NULL, 0);
    compScore = XtCreateManagedWidget ("compscore", cribBoardWidgetClass, layout, NULL, 0);
    playcrib = XtCreateManagedWidget ("playcrib", cardsWidgetClass, layout, NULL, 0);
    compcrib = XtCreateManagedWidget ("compcrib", cardsWidgetClass, layout, NULL, 0);
    message = XtCreateManagedWidget ("message", padWidgetClass, layout, NULL, 0);
    XtRealizeWidget (toplevel);
}

UISuspend ()
{
}

UIResume ()
{
}

UIFinish ()
{
}

static int compPegs[2];
static int playPegs[2];

resetPegs (w, pegs)
    Widget  w;
    int	    *pegs;
{
    int	    i;

    for (i = 0; i < 2; i++)
    {
	pegs[i] = CribBoardUnset;
	XkwCribBoardSetPeg (w, i, CribBoardUnset);
    }
}

UIInitBoard ()
{
    resetPegs (compScore, compPegs);
    resetPegs (playScore, playPegs);
}

UIGameScore (who, num)
{
#ifdef NOTDEF
    char    buf[100];
    sprintf (buf, "Games: %3d", num);
    XkwPadText (scoreWidget, 1 + 6 * who, 28, buf, strlen (buf));
#endif
}

UIRefresh ()
{
    XkwPadUpdate (message);
    HandUpdateDisplay (computer);
    HandUpdateDisplay (player);
    HandUpdateDisplay (table);
    HandUpdateDisplay (playcrib);
    HandUpdateDisplay (compcrib);
}

UIWait ()
{
    XEvent  event;
    
    UIMessage ("--More--", FALSE);
    UIRefresh ();
    for (;;)
    {
	XtNextEvent (&event);
	switch (event.type) {
	case KeyRelease:
        case ButtonRelease:
	    continue;
	case KeyPress:
	case ButtonPress:
	    return;
	default:
	    XtDispatchEvent (&event);
	}
    }
}

static Widget
widget (who)
    int	who;
{
    switch (who) {
    case PLAYER:    return player;
    case COMPUTER:  return computer;
    case TABLE:	    return table;
    }
}    

static CribbageCardPtr
Cards (who)
    int	who;
{
    switch (who) {
    case PLAYER:    return playerCards;
    case COMPUTER:  return computerCards;
    case TABLE:	    return tableCards;
    }
}    

UIEraseHand (who)
    int	who;
{
    Widget  w = widget (who);

    UIClearHand (who);
    HandUpdateDisplay (w);
}

UIClearHand (who)
{
    CribbageCardPtr    cards = Cards(who);
    int		    i;

    if (who == TABLE)
	Message (tableScore, "");
    for (i = 0; i < NUM_CARDS; i++)
    {
	cards[i].card.suit = CardsNone;
	cards[i].private = 0;
    }
    HandRemoveAllCards (widget (who));
}

static CardsSuit CardsSuitMap[] = {
    CardsSpade, CardsHeart, CardsDiamond, CardsClub 
};

static CardsRank CardsRankMap[] = {
    CardsAce, Cards2, Cards3, Cards4, Cards5, Cards6, Cards7,
    Cards8, Cards9, Cards10, CardsJack, CardsQueen, CardsKing, CardsEmpty
};

static
updateCards (w, h, n, cards, blank)
    Widget	    w;
    CARD	    h[];
    int		    n;
    CribbageCardPtr cards;
    BOOLEAN	    blank;
{
    int		    i;
    CardsSuit	    suit;
    CardsRank	    rank;

    for (i = 0; i < n; i++) 
    {
	if (h[i].rank == EMPTY)
	{
	    if (cards[i].private)
	    {
		CardsRemoveCard (w, cards[i].private);
		cards[i].private = 0;
		cards[i].card.suit = CardsNone;
	    }
	}
	else
	{
	    if (blank)
	    {
		suit = CardsBack;
		rank = CardsAce;
	    }
	    else
	    {
		suit = CardsSuitMap[h[i].suit];
		rank = CardsRankMap[h[i].rank];
	    }
	    if (cards[i].card.suit != suit || cards[i].card.rank != rank)
	    {
		cards[i].card.suit = suit;
		cards[i].card.rank = rank;
		if (cards[i].private)
		    CardsReplaceCard (w, cards[i].private, &cards[i].card);
		else
		    cards[i].private = CardsAddCard (w, &cards[i].card, i, i);
	    }
	}
    }
    for (; i < NUM_CARDS; i++) {
	if (cards[i].private)
	{
	    CardsRemoveCard (w, cards[i].private);
	    cards[i].private = 0;
	    cards[i].card.suit = CardsNone;
	}
    }
}

UIPrintHand (h, n, who, blank)
    CARD    h[];
    int	    n;
    int	    who;
    BOOLEAN blank;
{
    updateCards (widget (who), h, n, Cards(who), blank);
}

UIPrintCrib (who, card, blank)
    int	    who;
    CARD    *card;
    BOOLEAN blank;
{
    Widget	    w, ow;
    CARD	    h;
    CribbageCardPtr cards, ocards;
    
    if (who == COMPUTER)
    {
	w = compcrib;
	cards = compcribCards;
	ow = playcrib;
	ocards = playcribCards;
    }
    else
    {
	w = playcrib;
	cards = playcribCards;
	ow = compcrib;
	ocards = compcribCards;
    }
    
    updateCards (w, card, 1, cards, blank);
    updateCards (ow, NULL, 0, ocards, blank);
}

UITableScore (score, n)
    int	score;
{
    Message (tableScore, "Score: %d", score);
}

UIPrintPeg (score, on, who)
    int	    score;
    BOOLEAN on;
    int	    who;
{
    Widget	w;
    int		*pegs;
    int		i;

    if (who == COMPUTER)
    {
	w = compScore;
	pegs = compPegs;
    }
    else
    {
	w = playScore;
	pegs = playPegs;
    }
    
    if (score <= 0)
	score = CribBoardUnset;

    if (!on)
    {
	if (score == pegs[1])
	    i = 1;
	else
	    i = 0;
	score = CribBoardUnset;
    }
    else
    {
	if (pegs[0] == CribBoardUnset)
	    i = 0;
	else
	    i = 1;
    }
    pegs[i] = score;
    XkwCribBoardSetPeg (w, i, score - 1);
}

static int  msgLine, msgCol;
static int  curLine, curCol;

ShowCursor ()
{
    char    attr[1];
    
    curLine = msgLine;
    curCol = msgCol;
    attr[0] = XkwPadInverse;
    XkwPadAttributes (message, curLine, curCol, attr, 1);
}

HideCursor ()
{
    char    attr[1];
    
    attr[0] = XkwPadNormal;
    if (curLine >= 0)
	XkwPadAttributes (message, curLine, curCol, attr, 1);
    curLine = -1;
}

UIReadChar ()
{
    int	    c;
    XEvent  event;
    
    ShowCursor ();
    UIRefresh ();
    while (text_in == text_out) {
	XtNextEvent (&event);
	XtDispatchEvent (&event);
    }
    c = textbuf[text_out++];
    if (text_out == text_in)
	text_out = text_in = 0;
    if (c == '\r')
	c = '\n';
    HideCursor ();
    return c;
}


UIEchoChar (c)
    char    c;
{
    XkwPadText (message, msgLine, msgCol, &c, 1);
    msgCol++;
}

UIReadLine (buf, len)
    char    *buf;
{
    int	    ox, oy;
    char    *sp;
    int	    c;
    char    str[2];
    
    /*
     * loop reading in the string, and put it in a temporary buffer
     */
    ox = msgCol;
    oy = msgLine;
    for (sp = buf; (c = UIReadChar()) != '\n'; ) {
	if (c == -1)
	    continue;
	else if (c == '\b') {	/* process erase character */
	    if (sp > buf) {
		register int i;

		sp--;
		--msgCol;
		XkwPadText (message, msgLine, msgCol, " ", 1);
	    }
	    continue;
	}
	else if (c == '\025') {	/* process kill character */
	    sp = buf;
	    msgCol = ox;
	    msgLine = oy;
	    continue;
	}
	else if (sp == buf && c == ' ')
	    continue;
	if (sp >= &buf[len-1] || !(isprint(c) || c == ' '))
	    XBell (XtDisplay (toplevel), 0);
	else {
	    if (islower(c))
		c = toupper(c);
	    *sp++ = c;
	    
	    str[0] = c;
	    str[1] = '\0';
	    UIMessage (str, FALSE);
	}
	UIRefresh ();
    }
    *sp = '\0';
}

static CheckScroll ()
{
    Arg	arg[1];
    Dimension	rows;

    XtSetArg (arg[0], XtNnumRows, &rows);
    XtGetValues (message, arg, 1);
    while (msgLine >= rows)
    {
	XkwPadScroll (message, 0, rows, -1);
	msgLine--;
	curLine--;
    }
}

UIMessage (str, newline)
    char    *str;
    int	    newline;
{
    int	    len = strlen (str);
    if (newline)
    {
	msgLine++;
	CheckScroll ();
	msgCol = 0;
    }
    XkwPadText (message, msgLine, msgCol, str, len);
    msgCol += len;
}

UIGetMessageSize ()
{
    Arg	arg;
    Dimension	cols;

    XtSetArg (arg, XtNnumCols, &cols);
    XtGetValues (message, &arg, 1);
    return cols;
}

UIClearMsg ()
{
    XkwPadClear (message);
    msgLine = 0;
    msgCol = 0;
}

UIGetPlayerCard (hand, n, prompt)
    CARD    hand[];
    int	    n;
    char    *prompt;
{
    XEvent  event;
    for (;;) {
	msg (prompt);
	UIRefresh ();
	cardSelected = False;
	while (!cardSelected) 
	{
	    XtNextEvent (&event);
	    XtDispatchEvent (&event);
	}
	if (0 <= selectedCard && selectedCard < n)
	{
	    msgcard (hand[selectedCard], FALSE);
	    endmsg (FALSE);
	    return selectedCard;
	}
	msg ("Sorry, I missed that");
    }
}
