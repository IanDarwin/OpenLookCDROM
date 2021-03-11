/* tio.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains terminal I/O functions */

#include "config.h"
#include <signal.h>
#include "vi.h"


/* This function reads in a line from the terminal. */
int vgets(prompt, buf, bsize)
	char	prompt;	/* the prompt character, or '\0' for none */
	char	*buf;	/* buffer into which the string is read */
	int	bsize;	/* size of the buffer */
{
	int	len;	/* how much we've read so far */
	int	ch;	/* a character from the user */
	int	quoted;	/* is the next char quoted? */
	int	tab;	/* column position of cursor */
	char	widths[132];	/* widths of characters */

	/* show the prompt */
	move(LINES - 1, 0);
	tab = 0;
	if (prompt)
	{
		addch(prompt);
		tab = 1;
	}
	clrtoeol();
	refresh();

	/* read in the line */
	quoted = len = 0;
	for (;;)
	{
		ch = getkey(quoted ? 0 : WHEN_EX);

		/* some special conversions */
		if (ch == ctrl('D') && len == 0)
			ch = ctrl('[');

#ifdef	SIGTSTP
		if ((mode == MODE_EX) && (!quoted) && (ch == ctrl('Z'))) {
			extern int suspend_elvis();

			suspend_elvis();
			return -1;
		}
#endif	/*SIGTSTP*/

		/* inhibit detection of special chars (except ^J) after a ^V */
		if (quoted && ch != '\n')
		{
			ch |= 256;
		}

		/* process the character */
		switch(ch)
		{
		  case ctrl('V'):
			qaddch('^');
			qaddch('\b');
			quoted = TRUE;
			break;

		  case ctrl('['):
			return -1;

		  case '\n':
		  case '\r':
			clrtoeol();
			goto BreakBreak;

		  case '\b':
			if (len > 0)
			{
				len--;
				if (IsIdxHZ(len, buf) < 0)
				{   /* squeeze out the 2nd HZ byte */
					len--;
					addstr("\b \b");
					tab--;
				}
				addstr("\b\b\b\b\b\b\b\b" + 8 - widths[len]);
				addstr("        " + 8 - widths[len]);
				addstr("\b\b\b\b\b\b\b\b" + 8 - widths[len]);
				if (mode == MODE_EX)
				{
					clrtoeol();
				}
				tab -= widths[len];
			}
			else
			{
				return -1;
			}
			break;

		  default:
			/* strip off quotation bit */
			if (ch & 256)
			{
				ch &= ~256;
				quoted = FALSE;
				qaddch(' ');
				qaddch('\b');
			}
			/* add & echo the char */
			if (len < bsize - 1)
			{
				if (ch == '\t')
				{
					widths[len] = *o_tabstop - (tab % *o_tabstop);
					addstr("        " + 8 - widths[len]);
					tab += widths[len];
				}
				else if (ch > 0 && ch < ' ') /* > 0 by GB */
				{
					addch('^');
					addch(ch + '@');
					widths[len] = 2;
					tab += 2;
				}
				else if (ch == '\177')
				{
					addch('^');
					addch('?');
					widths[len] = 2;
					tab += 2;
				}
				else if (IsHiBitOn(ch))
				{
					if (len == bsize - 2) {
						beep ();
					}

					addch(ch);
					widths[len] = 1;
					buf[len++] = ch;

					ch = getkey(quoted ? 0 : WHEN_EX);
					addch(ch);
					widths[len] = 1;
					/* "buf[len++] = ch" at end of the loop */

					tab += 2;
				}
				else
				{
					addch(ch);
					widths[len] = 1;
					tab++;
				}
				buf[len++] = ch;
			}
			else
			{
				beep();
			}
		}
	}
BreakBreak:
	refresh();
	buf[len] = '\0';
	return len;
}


/* ring the terminal's bell */
beep()
{
	if (*o_vbell)
	{
		do_VB();
		refresh();
	}
	else if (*o_errorbells)
	{
		ttywrite("\007", 1);
	}
}

static manymsgs; /* This variable keeps msgs from overwriting each other */

/* Write a message in an appropriate way.  This should really be a varargs
 * function, but there is no such thing as vwprintw.  Hack!!!  Also uses a
 * little sleaze in the way it saves messages for repetition later.
 *
 * msg((char *)0)	- repeats the previous message
 * msg("")		- clears the message line
 * msg("%s %d", ...)	- does a printf onto the message line
 */
/*VARARGS1*/
msg(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	char	*fmt;
	long	arg1, arg2, arg3, arg4, arg5, arg6, arg7;
{
	static char	pmsg[80];	/* previous message */
	char		*start;		/* start of current message */

	if (mode != MODE_VI)
	{
		sprintf(pmsg, fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		qaddstr(pmsg);
		addch('\n');
		exrefresh();
	}
	else
	{
		/* redrawing previous message? */
		if (!fmt)
		{
			move(LINES - 1, 0);
			standout();
			qaddch(' ');
			addstr(pmsg);
			qaddch(' ');
			standend();
			clrtoeol();
			return;
		}

		/* just blanking out message line? */
		if (!*fmt)
		{
			if (!*pmsg) return;
			*pmsg = '\0';
			move(LINES - 1, 0);
			clrtoeol();
			return;
		}

		/* wait for keypress between consecutive msgs */
		if (manymsgs)
		{
			qaddstr("[More...]");
			wqrefresh(stdscr);
			getkey(0);
		}

		/* real message */
		move(LINES - 1, 0);
		standout();
		qaddch(' ');
		sprintf(pmsg, fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		qaddstr(pmsg);
		qaddch(' ');
		standend();
		clrtoeol();
		refresh();
	}
	manymsgs = TRUE;
}


/* This function calls refresh() if the option exrefresh is set */
exrefresh()
{
	char	*scan;

	/* If this ex command wrote ANYTHING set exwrote so vi's  :  command
	 * can tell that it must wait for a user keystroke before redrawing.
	 */
	for (scan=kbuf; scan<stdscr; scan++)
		if (*scan == '\n')
			exwrote = TRUE;

#if	MICROSOFT			/* avoid compiler bug */
	scan = stdscr;
#define	stdscr	scan
#endif
	/* now we do the refresh thing */
	if (*o_exrefresh)
	{
		refresh();
	}
	else
	{
		wqrefresh(stdscr);
	}
#if	MICROSOFT
#undef	stdscr
	stdscr = scan;
#endif	
	manymsgs = FALSE;
}


/* This variable holds a single ungotten key, or 0 for no key */
static int ungotten;
ungetkey(key)
	int	key;
{
	ungotten = key;
}

/* This array describes mapped key sequences */
static struct _keymap
{
	char	*name;		/* name of the key, or NULL */
	char	rawin[LONGKEY];	/* the unmapped version of input */
	char	cooked[80];	/* the mapped version of input */
	int	len;		/* length of the unmapped version */
	int	when;		/* when is this key mapped? */
}
	mapped[MAXMAPS];

#if !MSDOS && !TOS
static int dummy(){} /* for timeout */
#endif

/* This function reads in a keystroke for VI mode.  It automatically handles
 * key mapping.
 */
int getkey(when)
	int		when;		/* which bits must be ON? */
{
	static char	keybuf[100];	/* array of already-read keys */
	static int	nkeys;		/* total number of keys in keybuf */
	static int	next;		/* index of next key to return */
	static char	*cooked;	/* rawin, or pointer to converted key */ 
	static int	oldwhen;	/* "when" from last time */
	static int	oldleft;
	static long	oldtop;
	static long	oldnlines;
	static char	*cshape;	/* current cursor shape */
	register char	*kptr;		/* &keybuf[next] */
	register struct _keymap *km;	/* used to count through keymap */
	register int	i, j, k;

	/* if this key is needed for delay between multiple error messages,
	 * then reset the manymsgs flag and abort any mapped key sequence.
	 */
	if (manymsgs)
	{
		manymsgs = FALSE;
		cooked = (char *)0;
		ungotten = 0;
	}

	/* if we have an ungotten key, use it */
	if (ungotten != 0)
	{
		k = ungotten;
		ungotten = 0;
		return k;
	}

	/* if we're doing a mapped key, get the next char */
	if (cooked && *cooked)
	{
		return *cooked++;
	}

	/* if keybuf is empty, fill it */
	if (next == nkeys)
	{
#ifndef NO_CURSORSHAPE
		/* make sure the cursor is the right shape */
		if (has_CQ)
		{
			cooked = cshape;
			switch (when)
			{
			  case WHEN_EX:		cooked = CX;	break;
			  case WHEN_VICMD:	cooked = CV;	break;
			  case WHEN_VIINP:	cooked = CI;	break;
			  case WHEN_VIREP:	cooked = CR;	break;
			}
			if (cooked != cshape)
			{
				cshape = cooked;
				switch (when)
				{
				  case WHEN_EX:		do_CX();	break;
				  case WHEN_VICMD:	do_CV();	break;
				  case WHEN_VIINP:	do_CI();	break;
				  case WHEN_VIREP:	do_CR();	break;
				}
			}
			cooked = (char *)0;
		}
#endif

#ifndef NO_SHOWMODE
		/* if "showmode" then say which mode we're in */
		if (*o_showmode
		 && mode == MODE_VI
		 && (when != oldwhen || topline != oldtop || leftcol != oldleft || nlines != oldnlines))
		{
			oldwhen = when;
			oldtop = topline;
			oldleft = leftcol;
			oldnlines = nlines;

			if (when & WHEN_VICMD)
			{
				redraw(cursor, FALSE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr("Command");
				standend();
				redraw(cursor, FALSE);
			}
			else if (when & WHEN_VIINP)
			{
				redraw(cursor, TRUE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr(" Input ");
				standend();
				redraw(cursor, TRUE);
			}
			else if (when & WHEN_VIREP)
			{
				redraw(cursor, TRUE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr("Replace");
				standend();
				redraw(cursor, TRUE);
			}
		}
		else
#endif

		/* redraw if getting a VI command */
		if (when & WHEN_VICMD)
		{
			redraw(cursor, FALSE);
		}

		/* read the rawin keystrokes */
		refresh();
		while ((nkeys = ttyread(keybuf, sizeof keybuf)) <= 0)
		{
			/* terminal was probably resized */
			*o_lines = LINES;
			*o_columns = COLS;
			if (when & (WHEN_VICMD|WHEN_VIINP|WHEN_VIREP))
			{
				redraw(MARK_UNSET, FALSE);
				redraw(cursor, (when & WHEN_VICMD) == 0);
				refresh();
			}
		}
		next = 0;
	}

	/* see how many mapped keys this might be */
	kptr = &keybuf[next];
#ifdef	ORIGINAL
	for (i = j = 0, k = -1, km = mapped; i < MAXMAPS; i++, km++)
#else /*ORIGINAL*/
	for (i = j = 0, k = -1, km = mapped; ( when & WHEN_VICMD ) && i < MAXMAPS; i++, km++)
					     /* are we in CMD-mode? */
#endif/*ORIGINAL*/
	{
		if ((km->when & when) && km->len > 0 && *km->rawin == *kptr)
		{
			if (km->len > nkeys - next)
			{
				if (!strncmp(km->rawin, kptr, nkeys - next))
				{
					j++;
				}
			}
			else
			{
				if (!strncmp(km->rawin, kptr, km->len))
				{
					j++;
					k = i;
				}
			}
		}
	}

	/* if more than one, try to read some more */
	while (j > 1)
	{
#if ANY_UNIX
		signal(SIGALRM, dummy);
#endif
		alarm((unsigned)*o_keytime);
		i = nkeys;
		if ((k = ttyread(keybuf + nkeys, sizeof keybuf - nkeys)) >= 0)
		{
			nkeys += k;
		}
		alarm(0);

		/* if we couldn't read any more, pretend 0 mapped keys */
		if (i == nkeys)
		{
			j = 0;
		}
		else /* else we got some more - try again */
		{
			for (i = j = 0, k = -1, km = mapped; i < MAXMAPS; i++, km++)
			{
				if ((km->when & when) && km->len > 0 && *km->rawin == *kptr)
				{
					if (km->len > nkeys - next)
					{
						if (!strncmp(km->rawin, kptr, nkeys - next))
						{
							j++;
						}
					}
					else
					{
						if (!strncmp(km->rawin, kptr, km->len))
						{
							j++;
							k = i;
						}
					}
				}
			}
		}
	}

	/* if unambiguously mapped key, use it! */
	if (j == 1 && k >= 0)
	{
		next += mapped[k].len;
		cooked = mapped[k].cooked;
#ifndef NO_EXTENSIONS
		if ((when & (WHEN_VIINP|WHEN_VIREP))
		 && (mapped[k].when & WHEN_INMV))
		{
			return 0; /* special case, means "a movement char follows" */
		}
		else
#endif
		{
			return *cooked++;
		}
	}
	else
	/* assume key is unmapped, but still translate weird erase key to '\b' */
	if (keybuf[next] == ERASEKEY && when != 0)
	{
		next++;
		return '\b';
	}
	else
	{
		return keybuf[next++];
	}
}


/* This function maps or unmaps a key */
mapkey(rawin, cooked, when, name)
	char	*rawin;	/* the input key sequence, before mapping */
	char	*cooked;/* after mapping */
	short	when;	/* bitmap of when mapping should happen */
	char	*name;	/* name of the key, if any */
{
	int	i, j;

	/* if the mapped version starts with the word "visual" then set WHEN_INMV */
	if (!strncmp(cooked, "visual ", 7))
	{
		when |= WHEN_INMV;
		cooked += 7;
	}
	/* if WHEN_INMV is set, then WHEN_VIINP and WHEN_VIREP must be set */
	if (when & WHEN_INMV)
	{
		when |= (WHEN_VIINP | WHEN_VIREP);
	}

	/* see if the key sequence was mapped before */
	j = strlen(rawin);
	for (i = 0; i < MAXMAPS; i++)
	{
		if (mapped[i].len == j
		 && !strncmp(mapped[i].rawin, rawin, j)
		 && (mapped[i].when & when))
		{
			break;
		}
	}

	/* if not already mapped, then try to find a new slot to use */
	if (i == MAXMAPS)
	{
		for (i = 0; i < MAXMAPS && mapped[i].len > 0; i++)
		{
		}
	}

	/* no room for the new key? */
	if (i == MAXMAPS)
	{
		msg("No room left in the key map table");
		return;
	}

	/* map the key */
	if (cooked && *cooked)
	{
		/* Map the key */
		mapped[i].len = j;
		strncpy(mapped[i].rawin, rawin, j);
		strcpy(mapped[i].cooked, cooked);
		mapped[i].when = when;
		mapped[i].name = name;
	}
	else /* unmap the key */
	{
		mapped[i].len = 0;
	}
}

/* Dump keys of a given type - WHEN_VICMD dumps the ":map" keys, and
 * WHEN_VIINP|WHEN_VIREP dumps the ":map!" keys
 */
dumpkey(when)
{
	int	i, len, mlen;
	char	*scan;
	char	*mraw;

	for (i = 0; i < MAXMAPS; i++)
	{
		/* skip unused entries, or entries that don't match "when" */
		if (mapped[i].len <= 0 || !(mapped[i].when & when))
		{
			continue;
		}

		/* dump the key label, if any */
		len = 8;
		if (mapped[i].name)
		{
			qaddstr(mapped[i].name);
			len -= strlen(mapped[i].name);
		}
		do
		{
			qaddch(' ');
		} while (len-- > 0);

		/* dump the raw version */
		len = 0;
		mlen = mapped[i].len;
		mraw = mapped[i].rawin;
		for (scan = mraw; scan < mraw + mlen; scan++)
		{
			if (UCHAR(*scan) < ' ' || *scan == '\177')
			{
				qaddch('^');
				qaddch(*scan ^ '@');
				len += 2;
			}
			else
			{
				qaddch(*scan);
				len++;
			}
		}
		do
		{
			qaddch(' ');
		} while (++len < 8);

		/* dump the mapped version */
		if ((mapped[i].when & WHEN_INMV) && (when & (WHEN_VIINP|WHEN_VIREP)))
		{
			qaddstr("visual ");
		}
		for (scan = mapped[i].cooked; *scan; scan++)
		{
			if (UCHAR(*scan) < ' ' || *scan == '\177')
			{
				qaddch('^');
				qaddch(*scan ^ '@');
			}
			else
			{
				qaddch(*scan);
			}
		}

		addch('\n');
		exrefresh();
	}
}



/* This function saves the current configuration of mapped keys to a file */
savekeys(fd)
	int	fd;	/* file descriptor to save them to */
{
	int	i;
	char	buf[80];

	/* now write a map command for each key other than the arrows */
	for (i = 0; i < MAXMAPS; i++)
	{
		/* ignore keys that came from termcap */
		if (mapped[i].name)
		{
			continue;
		}

		/* If this isn't used, ignore it */
		if (mapped[i].len <= 0)
		{
			continue;
		}

		/* write the map command */
		if (mapped[i].when & WHEN_INMV)
		{
			sprintf(buf, "map%s %.*s visual %s\n",
				(mapped[i].when & WHEN_VICMD) ? "" : "!",
				mapped[i].len, mapped[i].rawin,
				mapped[i].cooked);
			twrite(fd, buf, strlen(buf));
		}
		else
		{
			if (mapped[i].when & WHEN_VICMD)
			{
				sprintf(buf, "map %.*s %s\n",
					mapped[i].len, mapped[i].rawin,
					mapped[i].cooked);
				twrite(fd, buf, strlen(buf));
			}
			if (mapped[i].when & (WHEN_VIINP | WHEN_VIREP))
			{
				sprintf(buf, "map! %.*s %s\n",
					mapped[i].len, mapped[i].rawin,
					mapped[i].cooked);
				twrite(fd, buf, strlen(buf));
			}
		}
	}
}
