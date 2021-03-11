/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include "richlex.h"
#include "richset.h"
#include <config.h>

extern char *getenv();
#ifdef AMIGA
extern char *strchr();
#endif

#ifdef __MSDOS__
unsigned _stklen=16384;	/* Increase stack size under MS-DOS */
#endif

int iso2022_fputc ();

/*
 * ########################################################################
 * The function "InitGlobals" must be updated whenever a new variable is
 * added here or a default value is changed.  The same must be done for
 * the next section of variables also.  This is needed because this module
 * could be linked into another program as a library and called more than
 * once in a single program execution.
 * ########################################################################
 */
static int linepos = 0, inspace = 0, leftmargin = 0, rightmargin, biggertext=0;
static int workingleft = 0, workingright, inexcerpt = 0, insignature = 0;
static int standout=0, underline=0, bold=0;
static int termcolumns=80, termrows=23;
int controlputc();

/* A common problem, in justifying text, is figuring out how to format a 
   line when part of it wants to be left-justified, part right-justified, 
   and part centered, or some combination thereof.  There is no perfect 
   solution to this problem, so this program takes an incredibly cheesy 
   but simple way out:  voting.  For each character on the line, a point
   is added to the "center" score if it is supposed to be centered, and 
   so on.  If it is inside TWO "center" environments, two points are added.
   This may be the world's first implementation of justification by voting...
*/

static int centerenv=0, leftjustenv=0, rightjustenv=0;
static int centerct=0, leftjustct=0, rightjustct=0;
static int UsePager = 0;
static int linesused = 0;
static int JustSawCmd=0;
static int JustCorrect = 0;	/* Non-zero to just correct and not format */
static int OverStrike = 0;
static char MoveRight[10];
static char standoutbuf[50], standendbuf[50], StartUnderline[50], StopUnderline[50];
static char KS[50], KE[50], BoldOn[50], BoldOff[50];
static char charsetname[50];
static int FakeTerminal;

#ifndef AMIGA
#ifndef __MSDOS__
#define TPUTS_OK
#endif
#endif
#ifndef TPUTS_OK
extern tputs();
#endif

static outputc(), realoutputc(), MakeWorkingMargins(), Pause(), fputsmovingright(), ResetTerminalCodes(), FinalizeTerminal(), outputstr(), FPUTS();

#define	OUTC(c)		(outputc((RCHAR)(c)))

static void
InitGlobals()
{
    linepos = 0;
    inspace = 0;
    leftmargin = 0;
    biggertext = 0;
    workingleft = 0;
    inexcerpt = 0;
    insignature = 0;
    termcolumns = 80;
    termrows = 23;
    centerenv = 0;
    leftjustenv = 0;
    rightjustenv = 0;
    centerct = 0;
    leftjustct = 0;
    rightjustct = 0;
    UsePager = 0;
    linesused = 0;
    JustSawCmd = 0;
    JustCorrect = 0;
    standout = 0;
    underline = 0;
    bold = 0;
}

static void
cleanup(signum)
int signum;
{
    FinalizeTerminal();
#if defined(AMIGA) || defined(__MSDOS__)
    exit(signum);
#else
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
#endif
}

static InitSignals() {
    signal(SIGINT, cleanup);
#if !defined(AMIGA)
#if !defined(__MSDOS__)
    signal(SIGILL, cleanup);
    signal(SIGTRAP, cleanup);
    signal(SIGIOT, cleanup);
    signal(SIGFPE, cleanup);
#ifndef linux
    signal(SIGEMT, cleanup);
    signal(SIGBUS, cleanup);
#endif
    signal(SIGSEGV, cleanup);
    signal(SIGTERM, cleanup);
#ifdef SIGXCPU
    signal(SIGXCPU, cleanup);
#endif
#endif
#endif
}

static nomemabort() {
    fprintf(stderr, "richtext: Out of memory\n");
    FinalizeTerminal();
#ifdef AMIGA
    exit(20);
#else
    exit(-1);
#endif
}

#ifndef	RICHTEXT_LIBRARY

/*
 * Only include the main function if this module is not being used as a
 * library call.
 */

main(argc, argv)
int argc;
char **argv;
{
  exit(richtext_main(argc, argv));
}

#endif

richtext_main(argc, argv)
int argc;
char **argv;
{
    RCHAR c;
    int i, atstart, negated,
    ForceTermcap=0, ForcePlain=0, NotTtyEnv = 0;
    char token[MAX_TOKEN_SIZE], *tok, *nottty;
    char tbuf[1024], *term, *dum;
    FILE *InputFP = stdin, *tmpfp;
#ifdef __MSDOS__
    int ansi = 0;
#endif

    InitSignals();
    InitGlobals();
    richtextreset();
    strcpy(charsetname,"us-ascii");
    dum = (char *) getenv("MM_USEPAGER");
    if (dum && atoi(dum)) ++UsePager;
    for (i=1; i< argc; ++i) {
        if (!strcmp(argv[i], "-p")) {
            ++UsePager;
        } else if (!strcmp(argv[i],"-c")) {
            /* Only perform correction: don't format */
            JustCorrect = 1;
        } else if (!strcmp(argv[i], "-f")) {
            /* Force termcap usage */
            ForceTermcap = 1;
        } else if (!strcmp(argv[i], "-t")) {
            /* Force plain text */
            ForcePlain = 1;
        } else if (!strcmp(argv[i],"-n")) {
            /* Disable the richtext correction routines */
            CorrectionEnabled = 0;
        } else if (!strcmp(argv[i],"-m")) {
            /* Enable the multi-byte '<' hack */
            RichtextLessThanFlag = 1;
	} else if (!strncmp(argv[i], "-s", 2)) {
	    /* Specify the character set to use (and convert to lower case) */
	    char *name;
	    if (argv[i][2])
		strcpy(charsetname, argv[i] + 2);
	    else if (i < (argc - 1))
		strcpy(charsetname, argv[++i]);
	    name = charsetname;
	    while (*name) {
		if (isupper(*name))
		    *name = tolower(*name);
		++name;
	    }
	} else if (!strcmp(argv[i], "-o")) {
	    /* Use Overstriking */
	    OverStrike = 1;
	    ForcePlain = 1;
#ifdef __MSDOS__
        } else if (!strcmp(argv[i], "-a")) {
            ansi = !ansi;
#endif
        } else {
            /* open for input */
            tmpfp = fopen(argv[i], "r");
            if (tmpfp)
	    	InputFP = tmpfp;
	    else {
	    	perror (argv[i]);
		exit (1);
	    }
        }
    }
#ifdef AMIGA
    /* THIS IS THE AMIGA TERMINAL INITIALIZATION CODE */
    nottty = (char *) getenv("MM_NOTTTY");
    if (nottty) NotTtyEnv = atoi(nottty);
    if (!isatty(0) || !isatty(1) || NotTtyEnv) {
        UsePager = 0;   /* Disable pager if I/O has been redirected or we're under a window-oriented mail reader */
    }
    KS[0] = 0;
    KE[0] = 0;
    if ((!ForcePlain && isatty(1)) || ForceTermcap) {
        FakeTerminal = 0;
        strcpy(standoutbuf, "\x9b\x37m");   /* Enter standout (highlighted) mode */
        strcpy(standendbuf, "\x9b\x30m");   /* Exit standout mode */
        strcpy(BoldOn,      "\x9b\x31m");   /* Enter bold mode */
        strcpy(BoldOff,     "\x9b\x30m");   /* Exit bold mode */
        strcpy(StartUnderline,"\x9b\x34m"); /* Enter underline mode */
        strcpy(StopUnderline,"\x9b\x30m");  /* Exit underline mode */
        strcpy(MoveRight, " ");
    } else if (OverStrike) {
	FakeTerminal = 1;
	BoldOn[0] = '\0';
	BoldOff[0] = '\0';
	standoutbuf[0] = '\0';
	standendbuf[0] = '\0';
	StartUnderline[0] = '\0';
	StopUnderline[0] = '\0';
	strcpy(MoveRight, " ");
    } else {
        FakeTerminal = 1;
        strcpy(BoldOn, "*");
        strcpy(BoldOff, "*");
        strcpy(standoutbuf, "_");
        strcpy(standendbuf, "_");
        strcpy(StartUnderline, "_");
        strcpy(StopUnderline, "_");
        strcpy(MoveRight, " ");
    }
#else
#ifdef __MSDOS__
    /* THIS IS THE DOS TERMINAL INITIALIZATION CODE */
    FakeTerminal = 1;
    termcolumns = 80;
    termrows = 23;
    strcpy(MoveRight, " ");
    if (!ansi && !OverStrike) {
        KS[0] = NULL;
        KE[0] = NULL;
        strcpy(BoldOn, "*");
        strcpy(BoldOff, "*");
        strcpy(standoutbuf, "_");
        strcpy(standendbuf, "_");
        strcpy(StartUnderline, "_");
        strcpy(StopUnderline, "_");
    } else if (OverStrike) {
        KS[0] = NULL;
        KE[0] = NULL;
	BoldOn[0] = '\0';
	BoldOff[0] = '\0';
	standoutbuf[0] = '\0';
	standendbuf[0] = '\0';
	StartUnderline[0] = '\0';
	StopUnderline[0] = '\0';
    } else {
        strcpy(KS, "\x01B[37m");  /* White text (reset) */
        strcpy(KE, "\x01B[37m");  /* White text (reset) */
        strcpy(BoldOn, "\x01B[1m");   /* Bold text */
        strcpy(BoldOff, "\x01B[0m");   /* End bold text (normal) */
        strcpy(StartUnderline, "\x01B[31m");  /* Underline (red text) */
        strcpy(StopUnderline, "\x01B[37m");   /* Underline end (white) */
        /* The following should probably be something else, for italic */
        strcpy(standoutbuf, "\x01B[31m"); 
        strcpy(standendbuf, "\x01B[37m"); 
    }
#else
    /* THIS IS THE UNIX TERMINAL INITIALIZATION CODE */
    nottty = (char *) getenv("MM_NOTTTY");
    if (nottty) NotTtyEnv = atoi(nottty);
    if (UsePager && (!isatty(0) || !isatty(1) || NotTtyEnv)) {
        UsePager = 0;   /* Disable pager if I/O has been redirected or we're under a window-oriented mail reader */
    }
    if ((!ForcePlain && (isatty(1) || isatty(0))) || ForceTermcap) {
        term = (char *) getenv("TERM");
    } else {
        term = NULL;
    }
    if (term && tgetent(tbuf, term) != 1) {
        term = NULL;
    }
    if (term) {
        dum = KS;
        if (tgetstr("ks", &dum)) *dum = '\0'; else KS[0] = '\0';
        dum = KE;
        if (tgetstr("ke", &dum)) *dum = '\0'; else KE[0] = '\0';
        dum = standoutbuf;
        if (tgetstr("so", &dum)) *dum = '\0'; else standoutbuf[0] = '\0';
        dum = standendbuf;
        if (tgetstr("se", &dum)) *dum = '\0'; else standendbuf[0] = '\0';
        dum = BoldOn;
        if (tgetstr("md", &dum)) *dum = '\0'; else strcpy(BoldOn, standoutbuf);
        dum = BoldOff;
        if (tgetstr("me", &dum)) *dum = '\0'; else strcpy(BoldOff, standendbuf);
        dum = StartUnderline;
        if (tgetstr("us", &dum)) *dum = '\0'; else StartUnderline[0] = '\0';
        dum = StopUnderline;
        if (tgetstr("ue", &dum)) *dum = '\0'; else StopUnderline[0] = '\0';
        dum = MoveRight;
        if (tgetstr("nd", &dum)) *dum = '\0'; else {
            MoveRight[0] = ' ';
            MoveRight[1] = '\0';
        }
        /* Some TERMCAP entries have REALLY screwed up "nd" fields, sigh... */
        if (!strcmp(MoveRight, "\014")) strcpy(MoveRight, " ");
        termcolumns = tgetnum("co");
        if (termcolumns <= 0) termcolumns = 80;
        termrows = tgetnum("li");
        if (termrows <= 0) termrows = 23;
	else termrows--;
        FakeTerminal=0;
    } else if (OverStrike) {
        KS[0] = '\0';
        KE[0] = '\0';
	FakeTerminal = 1;
	BoldOn[0] = '\0';
	BoldOff[0] = '\0';
	standoutbuf[0] = '\0';
	standendbuf[0] = '\0';
	StartUnderline[0] = '\0';
	StopUnderline[0] = '\0';
	strcpy(MoveRight, " ");
        termcolumns = 80;
        termrows = 23;
    } else {
        KS[0] = '\0';
        KE[0] = '\0';
        FakeTerminal = 1;
        strcpy(BoldOn, "*");
        strcpy(BoldOff, "*");
        strcpy(standoutbuf, "_");
        strcpy(standendbuf, "_");
        strcpy(StartUnderline, "_");
        strcpy(StopUnderline, "_");
        strcpy(MoveRight, " ");
        termcolumns = 80;
        termrows = 23;
    }
#endif
#endif
    /* Check for the LINES & COLUMNS hack */
    dum = getenv("LINES");
    if (dum && ((i=atoi(dum)) > 0)) termrows = i - 1;
    dum = getenv("COLUMNS");
    if (dum && ((i=atoi(dum)) > 0)) termcolumns = i;
    charsetnameinit(charsetname);
    RichtextPutc = iso2022_fputc;
    if (JustCorrect) {
        richtextcorrect(InputFP,stdout);
        return(0);
    }
    FPUTS(KS, stdout);
    rightmargin = workingright = termcolumns - 1;
    while((c = richtextlex(InputFP,token)) != (RCHAR)EOF) {
        if (c == RICHTEXT_COMMAND || c == RICHTEXT_NEG_COMMAND) { 
            negated = (c == RICHTEXT_NEG_COMMAND);
            tok = token;

	    /* Try to process the command with the character set processors */
	    if (charsetcommand (tok,negated))
	        continue;

	    /* Do the default action for the command */
            switch(tok[0]) {
                case 'b':
                    if (!strcmp(tok, "bold")) {
                        if (negated) {
                            --bold;
                            if (bold <= 0) {
				if (FakeTerminal) outputstr(BoldOff);
				else controloutput(BoldOff, 0);
			    }
                        } else {
                            ++bold;
                        }
                        ResetTerminalCodes(FakeTerminal, standout, underline,
                                            bold, standoutbuf, standendbuf, 0,
                                            StartUnderline, StopUnderline, 0, BoldOn,
                                            BoldOff, 1);
                    } else if (!strcmp(tok, "bigger")) {
                        if (negated) --biggertext; else ++biggertext;
                    }
                    break;
                case 'c':
                    if (!strcmp(tok, "center")) {
                        if (negated) --centerenv; else ++centerenv;
                    } else if (!strcmp(tok, "comment")) {
                        int commct=1;
			int tempc;
                        while (commct > 0) {
                            while ((tempc = getc(InputFP)) != '<' 
                                    && tempc != EOF) ;
                            if (tempc == EOF) break;
                            for (i=0; (tempc = getc(InputFP)) != '>' 
                              && tempc != EOF; ++i) {
                                token[i] = isupper(tempc) ? 
                                  tolower(tempc) : tempc;
                            }
                            if (tempc== EOF) break;
                            token[i] = 0;
                            if (!strcmp(token, "/comment")) --commct;
                            if (!strcmp(token, "comment")) ++commct;
                        }
                    }
                    break;
                case 'e':
                    if (!strcmp(tok, "excerpt")) {
                        atstart = !(linepos > workingleft);
                        if (negated) {
                            leftmargin -= 4;
                            rightmargin += 4;
                            --inexcerpt;
                        } else {
                            leftmargin += 4;
                            rightmargin -= 4;
                            ++inexcerpt;
                        }
                        MakeWorkingMargins();
                        if (!atstart) OUTC('\n');
                    }
                    break;
                case 'f':
                    if (!strcmp(tok, "flushleft")) {
                        if (negated) --leftjustenv; else ++leftjustenv;
                    } else if (!strcmp(tok, "flushright")) {
                        if (negated) --rightjustenv; else ++rightjustenv;
                    }
                    break;
                case 'i':
                    if (!strcmp(tok, "italic")) {
                        if (negated) {
                            --standout;
                            if (standout <= 0) {
				if (FakeTerminal) outputstr(standendbuf);
				else controloutput(standendbuf, 0);
			    }
                        } else {
                            ++standout;
                        }
                        ResetTerminalCodes(FakeTerminal, standout, underline, bold,
                                            standoutbuf, standendbuf, 1, StartUnderline,
                                            StopUnderline, 0, BoldOn, BoldOff, 0);
                    } else if (!strcmp(tok, "indent")) {
                        if (negated) {
                            leftmargin -= 4;
                        } else {
                            leftmargin += 4;
                        }
                    } else if (!strcmp(tok, "indentright")) {
                        if (negated) {
                            rightmargin += 4;
                        } else {
                            rightmargin -= 4;
                        }
                    }
                    MakeWorkingMargins();
                    break;
                case 'l':
                    if (!strcmp(tok, "lt")) {
                        OUTC('<');
                    }
                    break;
                case 'n':
                    if (!strcmp(tok, "nl")) {
                        OUTC('\n');
                    } else if (!strcmp(tok, "np")) {
                        OUTC('\n');
                        OUTC('\014');
                    }
                    break;
                case 'u':
                    if (!strcmp(tok, "underline")) {
                        if (negated) {
                            --underline;
                            if (underline <= 0) {
				if (FakeTerminal) outputstr(StopUnderline);
				else controloutput(StopUnderline,0);
			    }
                        } else {
                            ++underline;
                        }
                        ResetTerminalCodes(FakeTerminal, standout, underline, bold,
                                            standoutbuf, standendbuf, 0, StartUnderline,
                                            StopUnderline, 1, BoldOn, BoldOff, 0);
                    }
                    break;
                case 'o':
                    if (!strcmp(tok, "outdent")) {
                        if (negated) {
                            leftmargin += 4;
                        } else {
                            leftmargin -= 4;
                        }
                    } else if (!strcmp(tok, "outdentright")) {
                        if (negated) {
                            rightmargin -= 4;
                        } else {
                            rightmargin += 4;
                        }
                    }
                    MakeWorkingMargins();
                    break;                    
                case 'p':
                    if (!strcmp(tok, "paragraph")) {
                        if (negated) OUTC('\n');
                        OUTC('\n');
                    }
                    break;
                case 's':
                    if (!strcmp(tok, "signature")) {
                        atstart = !(linepos > workingleft);
                        if (negated) {
                            leftmargin -= 4;
                            rightmargin += 4;
                            --insignature;
                        } else {
                            leftmargin += 4;
                            rightmargin -= 4;
                            ++insignature;
                        }
                        MakeWorkingMargins();
                        if (!atstart) OUTC('\n');
                    }
                    break;
                default:
                    /* Ignore all other tokens */
                    break;
            }
            JustSawCmd = 1;
        } else if (c == '\n') {
            if (linepos > 0 && !inspace) {
                OUTC(' ');
            }
            JustSawCmd = 0;
        } else {
            OUTC(c);
            JustSawCmd = 0;
        }
    }
    if (term) { /* cleanup a bit for insurance */
        controloutput(standendbuf, 0);
        controloutput(StopUnderline, 0);
    }
    OUTC('\n'); /* for good measure */
    FPUTS(KE, stdout);
    fflush(stdout);
    if (UsePager) {
        Pause();
    }
    return(0);
}

static struct charsetmember OutputBuf[1000] = {0,0};
static int PendingOutput = 0, PendingControls = 0;

controlputc(c)
int c;
{
    charmemberctrl (&OutputBuf[PendingOutput],(RCHAR)c);
    ++PendingOutput;
    ++PendingControls;
}

static immediate_controlputc(c)
int c;
{
    (*RichtextPutc) (c, stdout);
}

controloutput(s, immediate)
char *s;
int immediate;
{
    tputs(s, 1, immediate ? immediate_controlputc : controlputc);
}

static folding_point (buf, pos)
struct	charsetmember *buf;
int	pos;
{
    int i;
    for (i = pos; i > 0; --i) {
    	if (!charisctrl (buf[i]) && charmemberfold (buf[i]))
	    return (i);
    }
    return (0);
}

static calc_column (buf, pos)
struct	charsetmember *buf;
int	pos;
{
    struct charsetmember *s;
    int col = 0;
    for (s = buf; (s -> ch); ++s) {
    	if (!charisctrl (*s))
    	    col += charmemberwidth (*s);
	else
	    ++col;
    }
    return (col);
}

static FlushOut() {
    int i, j, x;
    static struct charsetmember NewOutputBuf[1000];
    struct charsetmember *s;
    int NewPendingControls=0, NewPendingOutput = 0;

    OutputBuf[PendingOutput].ch = 0;
    OutputBuf[PendingOutput].charset = NULL;
    if (linepos >= workingright) {
        for (i=0, j=0; j<workingright && i < PendingOutput; ++i) {
            if (!charisctrl (OutputBuf[i])) ++j;
        }
	x = folding_point (OutputBuf,i);
	if ((workingleft + 4) < x)
	    i = x;
	else
            while (i > workingleft + 4) {
                if (!charisctrl (OutputBuf[i])
                     && isspace((unsigned char) OutputBuf[i].ch)) {
                    break;
                }
                --i;
            }
        for (j=0; i<PendingOutput; ++j, ++i) {
            NewOutputBuf[j] = OutputBuf[i];
            ++NewPendingOutput;
            if (charisctrl (NewOutputBuf[j])) ++NewPendingControls;
        }
        PendingOutput -= NewPendingOutput;
        PendingControls -= NewPendingControls;
        charmember (&OutputBuf[PendingOutput],(RCHAR)'\n');
	PendingOutput++;
        OutputBuf[PendingOutput].ch = 0;
        OutputBuf[PendingOutput].charset = 0;
#ifdef MISTAKE
        ResetTerminalCodes(FakeTerminal, standout, underline, bold,
                            standoutbuf, standendbuf, 0, StartUnderline,
                            StopUnderline, 0, BoldOn, BoldOff, 0);
#endif
    }
    if ((rightjustct > leftjustct)
         && (rightjustct > centerct)
         && (rightjustct > 0)) {
        /* right justified */
        i = termcolumns - calc_column (OutputBuf,PendingOutput) - 1
		+ PendingControls;
        for (j=0; j<i; ++j)  {
            controloutput(MoveRight, 1);
        }
        fputsmovingright(OutputBuf, stdout);
        ++linesused;
    } else if ((leftjustct > centerct)
                && (leftjustct > 0)) {
        /* left justified */
        for (s=OutputBuf; s -> ch && isspace((unsigned char) (s -> ch)); ++s)
	    {;}
        fputsmovingright(s, stdout);
        ++linesused;
    } else if (centerct > 0) {
        /* centered */
        i = (termcolumns - calc_column (OutputBuf,PendingOutput) - 1
		+ PendingControls) / 2;
        for (j=0; j<i; ++j) {
            controloutput(MoveRight, 1);
        }
        fputsmovingright(OutputBuf, stdout);
        ++linesused;
    } else {
        /* Leave indentation (margins) as-is */
        fputsmovingright(OutputBuf, stdout);
        ++linesused;
    }
    if (linesused >= termrows && UsePager) Pause();
    rightjustct = leftjustct = centerct = 0; /* not quite right for wrapping, sigh... */
    PendingOutput = PendingControls = linepos = 0;
    inspace = 1;
    j = (isspace((unsigned char) NewOutputBuf[0].ch)) ? 1 : 0;
    for ( ; j<NewPendingOutput; ++j) {
        if (charisctrl (NewOutputBuf[j])) {
	    OutputBuf[PendingOutput++] = NewOutputBuf[j];
            ++PendingControls;
        } else {
            realoutputc(NewOutputBuf[j], 1);
        }
    }
    ResetTerminalCodes(FakeTerminal, standout, underline, bold,
                        standoutbuf, standendbuf, 0, StartUnderline,
                        StopUnderline, 0, BoldOn, BoldOff, 0);
}

static outputc(c)
RCHAR c;
{
    struct charsetmember member;
    charmember (&member,c);
    realoutputc(member, 0);
}

static realoutputc(c, alreadyformatted)
struct charsetmember c;
int alreadyformatted;
{
    int i, newinspace;

    if (c.ch == '\n') {
    	charmember (&OutputBuf[PendingOutput],(RCHAR)'\n');
	PendingOutput++;
        FlushOut();
    } else if (c.ch == '\t') {
        int tabpos = (linepos + 8) / 8;
        if (tabpos >= workingright) {
    	    charmember (&OutputBuf[PendingOutput],(RCHAR)'\n');
	    PendingOutput++;
            FlushOut();
        } else {
            int spaces = (8*tabpos) - linepos;
            while (spaces-->0) {
    	        charmember (&OutputBuf[PendingOutput],(RCHAR)' ');
	        PendingOutput++;
                ++linepos;
            }
        }
    } else {
        newinspace = isspace((unsigned char) c.ch);
        if (!inspace || !newinspace || !JustSawCmd) {
            if (linepos == 0) {
                int i = workingleft;
                if (inexcerpt) {
    	            charmember (&OutputBuf[PendingOutput],(RCHAR)'>');
	            PendingOutput++;
                    --i;
                }
                if (insignature) {
    	            charmember (&OutputBuf[PendingOutput],(RCHAR)'+');
	            PendingOutput++;
                    --i;
                }
                while (i-->0) {
    	            charmember (&OutputBuf[PendingOutput],(RCHAR)' ');
	            PendingOutput++;
                }
                linepos = workingleft;
            }
            if (!alreadyformatted && biggertext && !(inspace && newinspace)) {
    	        charmember (&OutputBuf[PendingOutput],(RCHAR)'_');
	        PendingOutput++;
                ++linepos;
            }
            inspace = newinspace;
            linepos += charmemberwidth (c);
	    OutputBuf[PendingOutput++] = c;
	    if (OverStrike && !inspace) {
		if (bold) {
		    controlputc('\b');
		    controlputc((int) c.ch);
		}
		if (standout||underline) {
		    controlputc('\b');
		    controlputc('_');
		}
	    }
            leftjustct += leftjustenv;
            rightjustct += rightjustenv;
            centerct += centerenv;
            if (c.ch == '\014') inspace = 1;
            if (linepos >= workingright) FlushOut();
        }
    }
}

static MakeWorkingMargins() {
    int oldworkingleft=workingleft, i;

    workingleft = leftmargin;
    workingright = rightmargin;
    if (workingleft < 0) workingleft = 0;
    if (workingright < 0) workingright = 0;
    if (workingright > (termcolumns - 1)) workingright = (termcolumns - 1);
    if (workingleft > (workingright - 8)) workingleft = workingright -8;
    if (linepos == oldworkingleft && linepos != 0) {
        for (i=workingleft-oldworkingleft; i > 0; --i) OUTC(' ');
    }
}

static Pause()
{
    int	c;

    (void) fputs("Press RETURN to continue (or 'q' to quit):", stdout);
    fflush(stdout);
    c = getc(stdin);
    if (c == 'q' || c == 'Q') exit(0);
    linesused = 0;
}

/* Leading spaces should be output as MoveRight, to avoid 
   having margins that are underlined or reverse video */

static fputsmovingright(s, fp)
struct charsetmember *s;
FILE *fp;
{
    int inmargin=1;
    if (!s) return;
    while (s -> ch) {
        if (inmargin && (s -> ch) == ' ') {
            controloutput(MoveRight, 1);
        } else {
            if (inmargin) inmargin = 0;
	    if (charisctrl (*s))
	        (*RichtextPutc) ((int)(s -> ch), fp);
	    else
	    	charmemberrender (*s, fp);
        }
        ++s;
    }
}

static ResetTerminalCodes(FakeTerminal, standout, underline, bold, standoutbuf, standendbuf,
                    modifiedstandout, StartUnderline, StopUnderline, modifiedunderline,
                    BoldOn, BoldOff, modifiedbold)
char *standoutbuf, *standendbuf, *StartUnderline, *StopUnderline,
    *BoldOn, *BoldOff;
{
    if (OverStrike)
	return;

    /* We always turn back on the appropriate terminal modes, because
      on some terminals one thing turns off all of them */
    if (standout >= 1) {
        if (FakeTerminal) {
            if (modifiedstandout && standout == 1) outputstr(standoutbuf);
        } else controloutput(standoutbuf, 0);
    }
    if (bold >= 1) {
        if (FakeTerminal) {
            if (modifiedbold && bold == 1) outputstr(BoldOn);
        } else controloutput(BoldOn, 0);
    }
    if (underline >= 1) {
        if (FakeTerminal) {
            if (modifiedunderline && underline == 1) outputstr(StartUnderline);
        } else controloutput(StartUnderline, 0);
    }
}

static FinalizeTerminal() {
    tputs(standendbuf, 1, immediate_controlputc);
    tputs(BoldOff, 1, immediate_controlputc);
    tputs(StopUnderline, 1, immediate_controlputc);
    FPUTS(KE, stdout);
}

static outputstr(s)
char *s;
{
    while (*s) OUTC(*s++);
}

#ifndef TPUTS_OK
tputs(s, n, func)
char *s;
int n;
int (*func)();
{
    if (s) {
        while (*s) {
            func((int)*s);
            ++s;
        }
    }
    return (0);
}
#endif

static FPUTS(s,fp)
unsigned char *s;
FILE *fp;
{
    while(*s) (*RichtextPutc)((int)(*s++),fp);
}
