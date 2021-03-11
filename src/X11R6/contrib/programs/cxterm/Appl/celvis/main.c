/* main.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains the main() function of vi */

#include "config.h"
#include <signal.h>
#include <setjmp.h>
#include "vi.h"

extern		trapint(); /* defined below */
extern char	*getenv();
jmp_buf		jmpenv;

/*---------------------------------------------------------------------*/
void HZ_abort(i)
	int i ;
{
	/* change the terminal mode back the way it was */
	suspend_curses();
	exit(i);
}

void main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*cmd = (char *)0;
	char	*tag = (char *)0;
	char	*str;
#if	MSDOS || TOS
	char firstarg[256];
#else
	char *firstarg;
#endif

#ifndef NO_ARGV0
	/* set mode to MODE_VI or MODE_EX depending on program name */
#if	MSDOS
	switch (argv[0][strlen(argv[0]) - 1 - 4])	/* kjh@usc.edu */
#else /*MSDOS*/
	switch (argv[0][strlen(argv[0]) - 1])
#endif/*MSDOS*/
	{
#if	MSDOS
	  case 'X':			/* "ex" uppercase only, kjh@usc.edu */
#endif/*MSDOS*/
	  case 'x':			/* "ex" */
		mode = MODE_EX;
		break;

#if	MSDOS
	  case 'W':			/* uppercase only, kjh@usc.edu */
#endif/*MSDOS*/
	  case 'w':			/* "view" */
		mode = MODE_VI;
		*o_readonly = TRUE;
		break;
#ifndef NO_EXTENSIONS
#if	MSDOS
	  case 'T':			/* uppercase only, kjh@usc.edu */
#endif/*MSDOS*/
	  case 't':			/* "edit" or "input" */
		mode = MODE_VI;
		*o_inputmode = TRUE;
		break;
#endif
	  default:			/* "vi" or "elvis" */
		mode = MODE_VI;
	}
#endif

	/* start curses */
	initscr();
	cbreak();
	noecho();
	scrollok(stdscr, TRUE);

#ifndef DEBUG
#ifdef	SIGQUIT
	/* normally, we ignore SIGQUIT.  SIGINT is trapped later */
	signal(SIGQUIT, SIG_IGN);
#endif
#ifdef	SIGILL
	signal(SIGILL, HZ_abort);
#endif
#ifdef	SIGSEGV
	signal(SIGSEGV, HZ_abort);
#endif
#ifdef	SIGTERM
	signal(SIGTERM, HZ_abort);
#endif
#endif

	/* initialize the options */
	initopts();

	/* map the arrow keys.  The KU,KD,KL,and KR variables correspond to
	 * the :ku=: (etc.) termcap capabilities.  The variables are defined
	 * as part of the curses package.
	 */
	if (has_KU) mapkey(has_KU, "k",    WHEN_VICMD|WHEN_INMV, "<Up>");
	if (has_KD) mapkey(has_KD, "j",    WHEN_VICMD|WHEN_INMV, "<Down>");
	if (has_KL) mapkey(has_KL, "h",    WHEN_VICMD|WHEN_INMV, "<Left>");
	if (has_KR) mapkey(has_KR, "l",    WHEN_VICMD|WHEN_INMV, "<Right>");
	if (has_HM) mapkey(has_HM, "^",    WHEN_VICMD|WHEN_INMV, "<Home>");
	if (has_EN) mapkey(has_EN, "$",    WHEN_VICMD|WHEN_INMV, "<End>");
	if (has_PU) mapkey(has_PU, "\002", WHEN_VICMD|WHEN_INMV, "<PgUp>");
	if (has_PD) mapkey(has_PD, "\006", WHEN_VICMD|WHEN_INMV, "<PgDn>");
#if	MSDOS
	if (*o_pcbios)
	{
		mapkey("#R", "i", WHEN_VICMD|WHEN_INMV,	"<Insrt>");
		mapkey("#S", "x", WHEN_VICMD|WHEN_INMV,	"<Del>");
		mapkey("#s", "B", WHEN_VICMD|WHEN_INMV,	"^<left>");
		mapkey("#t", "W", WHEN_VICMD|WHEN_INMV,	"^<right>");
	}
#else
	if (ERASEKEY != '\177')
	{
		mapkey("\177", "x", WHEN_VICMD|WHEN_INMV, "<Del>");
	}
#endif

	/* process any flags */
	for (i = 1; i < argc && *argv[i] == '-'; i++)
	{
		switch (argv[i][1])
		{
		  case 'R':	/* readonly */
			*o_readonly = TRUE;
			break;

		  case 'r':	/* recover */
			msg("Use the `virec` command to recover lost files\n");
			refresh();
			endwin();
			exit(0);
			break;

		  case 't':	/* tag */
			if (argv[i][2])
			{
				tag = argv[i] + 2;
			}
			else
			{
				i++;
				tag = argv[i];
			}
			break;

		  case 'v':	/* vi mode */
			mode = MODE_VI;
			break;

		  case 'e':	/* ex mode */
			mode = MODE_EX;
			break;
#ifndef NO_EXTENSIONS
		  case 'i':	/* input mode */
			*o_inputmode = TRUE;
			break;
#endif
		  default:
			msg("Ignoring unknown flag \"%s\"", argv[i]);
		}
	}

	/* if we were given an initial ex command, save it... */
	if (i < argc && *argv[i] == '+')
	{
		if (argv[i][1])
		{
			cmd = argv[i++] + 1;
		}
		else
		{
			cmd = "$"; /* "vi + file" means start at EOF */
			i++;
		}
	}

	/* the remaining args are file names. */
	nargs = argc - i;
	if (nargs > 0)
	{
#if ! ( MSDOS || TOS )
		firstarg = argv[i];
#endif
		strcpy(args, argv[i]);
		while (++i < argc && strlen(args) + 1 + strlen(argv[i]) < sizeof args)
		{
			strcat(args, " ");
			strcat(args, argv[i]);
		}
	}
#if ! ( MSDOS || TOS )
	else
	{
		firstarg = "";
	}
#endif
	argno = 0;

#if MSDOS || TOS
	if (nargs > 0)
	{
		/* do we really need to do wildcard? */
		if (strchr(args, '*') || strchr(args, '?'))
			strcpy(args, wildcard(args));
		nargs = 1;
		for (i = 0; args[i]; i++)
		{
			if (args[i] == ' ')
			{
				nargs++;
			}
		}
		for (i = 0; args[i] && args[i] != ' '; i++)
		{
			firstarg[i] = args[i];
		}
		firstarg[i] = '\0';
	}
	else
	{
		firstarg[0] = '\0';
	}
#endif

	/* perform the .exrc files and EXINIT environment variable */
#ifdef SYSEXRC
	doexrc(SYSEXRC);
#endif
#ifdef HMEXRC
	str = getenv("HOME");
	if (str)
	{
		sprintf(tmpblk.c, "%s%c%s", str, SLASH, HMEXRC);
		doexrc(tmpblk.c);
	}
#endif
	doexrc(EXRC);
#ifdef EXINIT
	str = getenv(EXINIT);
	if (str)
	{
		doexcmd(str);
	}
#endif

	/* search for a tag now, if desired */
	blkinit();
	if (tag)
	{
		cmd_tag(MARK_FIRST, MARK_FIRST, CMD_TAG, 0, tag);
	}

	/* if no tag, or tag failed, then start with first arg */
	if (tmpfd < 0 && tmpstart(firstarg) == 0 && *origname)
	{
		ChangeText
		{
		}
		clrflag(file, MODIFIED);
	}
	
	/* now we do the immediate ex command that we noticed before */
	if (cmd)
	{
		doexcmd(cmd);
	}

	/* repeatedly call ex() or vi() (depending on the mode) until the
	 * mode is set to MODE_QUIT
	 */
	while (mode != MODE_QUIT)
	{
		if (setjmp(jmpenv))
		{
			/* Maybe we just aborted a change? */
			abortdo();
		}
#if	TURBOC
		signal(SIGINT, (void(*)()) trapint);
#else
		signal(SIGINT, trapint);
#endif

		switch (mode)
		{
		  case MODE_VI:
			vi();
			break;

		  case MODE_EX:
			ex();
			break;
#ifdef DEBUG
		  default:
			msg("mode = %d?", mode);
			mode = MODE_QUIT;
#endif
		}
	}

	/* free up the cut buffers */
	cutend();

	/* end curses */
#ifndef	NO_CURSORSHAPE	
	if (has_CQ)
		do_CQ();
#endif
	refresh();
	endwin();

	exit(0);
	/*NOTREACHED*/
}


/*ARGSUSED*/
trapint(signo)
	int	signo;
{
	resume_curses(FALSE);
	longjmp(jmpenv, 1);
}

