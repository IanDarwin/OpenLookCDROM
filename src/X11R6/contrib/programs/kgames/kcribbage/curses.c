# include	<stdio.h>
# include	<signal.h>
# include	<curses.h>
# include	"deck.h"
# include	"cribbage.h"
# include	"cribcur.h"

WINDOW	*Compwin;			/* computer's hand window */
WINDOW	*Msgwin;			/* messages for the player */
WINDOW	*Playwin;			/* player's hand window */
WINDOW	*Tablewin;			/* table window */

# ifndef	erasechar
#	define	erasechar()	_tty.sg_erase
#	define	killchar()	_tty.sg_kill
# endif		attron

#define CNTL(c)	((c) & 0x37)

static void
bye ()
{
    signal (SIGINT, SIG_IGN);
    quit (1);
}

UIInit (argc, argv)
    int	    argc;
    char    **argv;
{
    initscr();
    signal(SIGINT, bye);
    crmode();
    noecho();
    Playwin = subwin(stdscr, PLAY_Y, PLAY_X, 0, 0);
    Tablewin = subwin(stdscr, TABLE_Y, TABLE_X, 0, PLAY_X);
    Compwin = subwin(stdscr, COMP_Y, COMP_X, 0, TABLE_X + PLAY_X);
    Msgwin = subwin(stdscr, MSG_Y, MSG_X, Y_MSG_START, SCORE_X + 1);
    leaveok(Playwin, TRUE);
    leaveok(Tablewin, TRUE);
    leaveok(Compwin, TRUE);
    clearok(stdscr, FALSE);
    while (--argc > 0) {
	if ((*++argv)[0] != '-') {
	    UIFinish ();
	    fprintf(stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n");
	    exit(1);
	}
	bust = FALSE;
	for (s = argv[0] + 1; *s != NULL; s++) {
	    switch (*s) {
		case 'e':
		    explain = TRUE;
		    break;
		case 'q':
		    quiet = TRUE;
		    break;
		case 'r':
		    rflag = TRUE;
		    break;
		default:
		    UIFinish ();
		    fprintf(stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n");
		    exit(2);
		    break;
	    }
	    if (bust)
		break;
	}
    }
    if (!quiet) {
	msg("Do you need instructions for cribbage? ");
	if (getuchar() == 'Y') {
	    UISuspend ();
	    system(INSTRCMD);
	    UIResume ();
	    msg("For the rules of this program, do \"man cribbage\"");
	}
    }
}

UISuspend ()
{
    endwin();
    fflush(stdout);
}

UIResume ()
{
    crmode();
    noecho();
    clear();
    refresh();
}

UIFinish ()
{
    mvcur(0, COLS - 1, LINES - 1, 0);
    fflush(stdout);
    endwin();
    putchar('\n');
}

UIClearMsg ()
{
    wclrtobot(Msgwin);
}

UIInitBoard ()
{
    mvaddstr(SCORE_Y + 0, SCORE_X, "+---------------------------------------+");
    mvaddstr(SCORE_Y + 1, SCORE_X, "|  Score:   0     YOU                   |");
    mvaddstr(SCORE_Y + 2, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 3, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 4, SCORE_X, "|                                       |");
    mvaddstr(SCORE_Y + 5, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 6, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 7, SCORE_X, "|  Score:   0      ME                   |");
    mvaddstr(SCORE_Y + 8, SCORE_X, "+---------------------------------------+");
}

UIGameScore(who, num)
{
    mvprintw(SCORE_Y + 1 + 6 * who, SCORE_X + 28, "Games: %3d", num);
}

UIRefresh ()
{
    refresh ();
}

static WINDOW *
win(who)
    int	who;
{
    switch (who) {
    case PLAYER:    return Playwin;
    case COMPUTER:  return Compwin;
    case TABLE:	    return Tablewin;
    }
}    

UIEraseHand (who)
    int	who;
{
    WINDOW  *w = win(who);

    werase (w);
    wrefresh (w);
}

UIClearHand (who)
{
    werase (win(who));
}

/*
 * prcard:
 *	Print out a card on the window at the specified location
 */

extern char *rankchar[RANKS];
extern char *suitchar[SUITS];

static
prcard(win, y, x, c, blank)
WINDOW		*win;
int		y, x;
CARD		c;
BOOLEAN		blank;
{
	if (c.rank == EMPTY)
	    return;
	mvwaddstr(win, y + 0, x, "+-----+");
	mvwaddstr(win, y + 1, x, "|     |");
	mvwaddstr(win, y + 2, x, "|     |");
	mvwaddstr(win, y + 3, x, "|     |");
	mvwaddstr(win, y + 4, x, "+-----+");
	if (!blank) {
		mvwaddch(win, y + 1, x + 1, rankchar[c.rank][0]);
		waddch(win, suitchar[c.suit][0]);
		mvwaddch(win, y + 3, x + 4, rankchar[c.rank][0]);
		waddch(win, suitchar[c.suit][0]);
	}
}

/*
 * printcard:
 *	Print out a card.
 */
static
printcard(win, cardno, c, blank)
WINDOW		*win;
int		cardno;
CARD		c;
BOOLEAN		blank;
{
	prcard(win, cardno * 2, cardno, c, blank);
}

/*
 * prhand:
 *	Print a hand of n cards
 */
static
prhand(h, n, win, blank)
CARD		h[];
int		n;
WINDOW		*win;
BOOLEAN		blank;
{
	register int	i;

	werase(win);
	for (i = 0; i < n; i++)
	    printcard(win, i, *h++, blank);
	wrefresh(win);
}

UIPrintHand (h, n, who, blank)
    CARD	h[];
    int		n;
    int		who;
    BOOLEAN	blank;
{
    prhand (h, n, win(who), blank);
}

UIPrintCrib (who, card, blank)
    int	    who;
    CARD    *card;
    BOOLEAN blank;
{
    register int	y, cardx;

    if (who == COMPUTER)
	cardx = CRIB_X;
    else
	cardx = 0;

    mvaddstr(CRIB_Y, cardx + 1, "CRIB");
    prcard(stdscr, CRIB_Y + 1, cardx, *card, blank);

    if (who == COMPUTER)
	cardx = 0;
    else
	cardx = CRIB_X;

    for (y = CRIB_Y; y <= CRIB_Y + 5; y++)
	mvaddstr(y, cardx, "       ");
    refresh ();
}

UITableScore (score, n)
    int	    score;
{
    mvwprintw(Tablewin, (n + 2) * 2, n + 1, "%2d", score);
    wrefresh(Tablewin);
}

/*
 * prpeg:
 *	Put out the peg character on the score board and put the
 *	score up on the board.
 */
static
prpeg(score, peg, myturn)
register int	score;
char		peg;
BOOLEAN		myturn;
{
	register int	y, x;

	if (!myturn)
		y = SCORE_Y + 2;
	else
		y = SCORE_Y + 5;

	if (score <= 0 || score >= glimit) {
		if (peg == '.')
			peg = ' ';
		if (score == 0)
			x = SCORE_X + 2;
		else {
			x = SCORE_X + 2;
			y++;
		}
	}
	else {
		x = (score - 1) % 30;
		if (score > 90 || (score > 30 && score <= 60)) {
			y++;
			x = 29 - x;
		}
		x += x / 5;
		x += SCORE_X + 3;
	}
	mvaddch(y, x, peg);
	mvprintw(SCORE_Y + (myturn ? 7 : 1), SCORE_X + 10, "%3d", score);
}

UIPrintPeg (score, on, who)
    int	    score;
    BOOLEAN on;
    int	    who;
{
    prpeg(score, on ? PEG : '.', who == COMPUTER);
}

UIReadChar ()
{
    register int	cnt, y, x;
    auto char		c;

over:
    cnt = 0;
    while (read(0, &c, 1) <= 0)
	if (cnt++ > 100)	/* if we are getting infinite EOFs */
	    bye();		/* quit the game */
    if (c == CNTL('L')) {
	wrefresh(curscr);
	goto over;
    }
    if (c == '\r')
	return '\n';
    else
	return c;
}

UIEchoChar (c)
    int	    c;
{
    waddch (Msgwin, c);
}

static int	Lineno = 0;
static int	Mpos = 0;

UIReadLine (buf, len)
    char    *buf;
    int	    len;
{
    register char	*sp;
    register int	c, oy, ox;
    register WINDOW	*oscr;

    oscr = stdscr;
    stdscr = Msgwin;
    getyx(stdscr, oy, ox);
    refresh();
    /*
     * loop reading in the string, and put it in a temporary buffer
     */
    for (sp = buf; (c = UIReadChar()) != '\n'; clrtoeol(), refresh()) {
	if (c == -1)
	    continue;
	else if (c == erasechar()) {	/* process erase character */
	    if (sp > buf) {
		register int i;

		sp--;
		for (i = strlen(unctrl(*sp)); i; i--)
		    addch('\b');
	    }
	    continue;
	}
	else if (c == killchar()) {	/* process kill character */
	    sp = buf;
	    move(oy, ox);
	    continue;
	}
	else if (sp == buf && c == ' ')
	    continue;
	if (sp >= &buf[len-1] || !(isprint(c) || c == ' '))
	    putchar(CNTL('G'));
	else {
	    if (islower(c))
		c = toupper(c);
	    *sp++ = c;
	    addstr(unctrl(c));
	    Mpos++;
	}
    }
    *sp = '\0';
    stdscr = oscr;
}


UIMessage (str, newline)
    char    *str;
    int	    newline;
{
    register int	len;
    register char	*mp, *omp;
    static int		lastline = 0;

    len = strlen(str);
    if (!newline)
    {
	if (Mpos + len < MSG_X)
	    wmove(Msgwin, Lineno > 0 ? Lineno - 1 : MSG_Y - 1, Mpos);
	else {
	    mvwaddch(Msgwin, Lineno, 0, ' ');
	    wclrtoeol(Msgwin);
	    if (++Lineno >= MSG_Y)
		Lineno = 0;
	}
	waddstr(Msgwin, str);
	wrefresh(Msgwin);
    }
    else
    {
	/*
	 * All messages should start with uppercase
	 */
	mvaddch(lastline + Y_MSG_START, SCORE_X, ' ');
	if (islower(str[0]) && str[1] != ')')
	    str[0] = toupper(str[0]);
	mp = str;
	if (len / MSG_X + Lineno >= MSG_Y) {
	    while (Lineno < MSG_Y) {
		wmove(Msgwin, Lineno++, 0);
		wclrtoeol(Msgwin);
	    }
	    Lineno = 0;
	}
	mvaddch(Lineno + Y_MSG_START, SCORE_X, '*');
	lastline = Lineno;
	do {
	    mvwaddstr(Msgwin, Lineno, 0, mp);
	    if ((len = strlen(mp)) > MSG_X) {
		omp = mp;
		for (mp = &mp[MSG_X-1]; *mp != ' '; mp--)
		    continue;
		while (*mp == ' ')
		    mp--;
		mp++;
		wmove(Msgwin, Lineno, mp - omp);
		wclrtoeol(Msgwin);
	    }
	    if (++Lineno >= MSG_Y)
		Lineno = 0;
	} while (len > MSG_X);
	wclrtoeol(Msgwin);
	Mpos = len;
	wrefresh(Msgwin);
    }
}


UIGetMessageSize ()
{
    return MSG_X;
}

/*
 * incard:
 *	Inputs a card in any format.  It reads a line ending with a CR
 *	and then parses it.
 */
static
incard(crd)
CARD		*crd;
{
	char		*getline();
	register int	i;
	int		rnk, sut;
	char		*line, *p, *p1;
	BOOLEAN		retval;

	retval = FALSE;
	rnk = sut = EMPTY;
	if (!(line = getline()))
		goto gotit;
	p = p1 = line;
	while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
	*p1++ = NULL;
	if(  *p == NULL  )  goto  gotit;
			/* IMPORTANT: no real card has 2 char first name */
	if(  strlen(p) == 2  )  {               /* check for short form */
	    rnk = EMPTY;
	    for( i = 0; i < RANKS; i++ )  {
		if(  *p == *rankchar[i]  )  {
		    rnk = i;
		    break;
		}
	    }
	    if(  rnk == EMPTY  )  goto  gotit;     /* it's nothing... */
	    ++p;                                /* advance to next char */
	    sut = EMPTY;
	    for( i = 0; i < SUITS; i++ )  {
		if(  *p == *suitchar[i]  )  {
		    sut = i;
		    break;
		}
	    }
	    if(  sut != EMPTY  )  retval = TRUE;
	    goto  gotit;
	}
	rnk = EMPTY;
	for( i = 0; i < RANKS; i++ )  {
	    if(  !strcmp( p, rankname[i] )  ||  !strcmp( p, rankchar[i] )  )  {
		rnk = i;
		break;
	    }
	}
	if(  rnk == EMPTY  )  goto  gotit;
	p = p1;
	while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
	*p1++ = NULL;
	if(  *p == NULL  )  goto  gotit;
	if(  !strcmp( "OF", p )  )  {
	    p = p1;
	    while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
	    *p1++ = NULL;
	    if(  *p == NULL  )  goto  gotit;
	}
	sut = EMPTY;
	for( i = 0; i < SUITS; i++ )  {
	    if(  !strcmp( p, suitname[i] )  ||  !strcmp( p, suitchar[i] )  )  {
		sut = i;
		break;
	    }
	}
	if(  sut != EMPTY  )  retval = TRUE;
gotit:
	(*crd).rank = rnk;
	(*crd).suit = sut;
	return( retval );
}
/*
 * UIGetPlayerCard:
 *	reads a card, supposedly in hand, accepting unambigous brief
 *	input, returns the index of the card found...
 */
UIGetPlayerCard (hand, n, prompt)
CARD		hand[];
int		n;
char		*prompt;
{
	register int           i, j;
	CARD                    crd;

	if (n < 1) {
	    printf("\nINFROM: %d = n < 1!!\n", n);
	    exit(74);
	}
	for (;;) {
	    msg(prompt);
	    if (incard(&crd)) {			/* if card is full card */
		if (!isone(crd, hand, n))
		    msg("That's not in your hand");
		else {
		    for (i = 0; i < n; i++)
			if (hand[i].rank == crd.rank &&
			    hand[i].suit == crd.suit)
				break;
		    if (i >= n) {
			printf("\nINFROM: isone or something messed up\n");
			exit(77);
		    }
		    return i;
		}
	    }
	    else				/* if not full card... */
		if (crd.rank != EMPTY) {
		    for (i = 0; i < n; i++)
			if (hand[i].rank == crd.rank)
				break;
		    if (i >= n)
			msg("No such rank in your hand");
		    else {
			for (j = i + 1; j < n; j++)
			    if (hand[j].rank == crd.rank)
				break;
			if (j < n)
			    msg("Ambiguous rank");
			else
			    return i;
		    }
		}
		else
		    msg("Sorry, I missed that");
	}
	/* NOTREACHED */
}

/*
 * wait_for
 *	Sit around until the guy types the right key
 */
static void
wait_for(ch)
register char	ch;
{
    register char	c;

    if (ch == '\n')
	while ((c = UIReadChar ()) != '\n' && c != '\r')
	    continue;
    else
	while (UIReadChar () != ch)
	    continue;
}

/*
 * do_wait:
 *	Wait for the user to type ' ' before doing anything else
 */
UIWait ()
{
    UIMessage ("--More--", FALSE);
    wait_for(' ');
}


