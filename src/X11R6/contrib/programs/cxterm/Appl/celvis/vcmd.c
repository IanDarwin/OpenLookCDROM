/* vcmd.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains the functions that handle VI commands */


#include "config.h"
#include "vi.h"
#if	MSDOS
#include <process.h>
#include <string.h>
#endif
#if	TOS
#include <osbind.h>
#include <string.h>
#endif

int	FindNCharToMark( m )
	MARK	m ;
{
	int	idx;
	long	nByte ;
	int	n ;

	idx = markidx( m );
	pfetch(markline(m));

	/* Find actual num of char from beginning to mark m */
	for ( n = 0, nByte = 0 ; nByte < idx ; n++ ) {
		if ( IsHiBitOn( ptext[nByte] ) ) {
			nByte += 2 ;
		} else {
			nByte ++ ;
		}
	}
	return n ;
}

int	IsIdxHZ( idx, text )
	int	idx;
	char*	text;
{
	int	i ;
	int	s = 0;	/* 0: ASCII; 1: HZ-1st-byte; -1: HZ-2nd-byte */

	if (idx > strlen(text))
		return 0 ;

	for (i = 0 ; i <= idx; i++ ) {
		if (s == 1)	/* previous byte was HZ-1st-byte */
			s = -1;
		else if ( IsHiBitOn( text[i] ) )
			s = 1;
		else
			s = 0;
	}
	return s ;
}

int	FindNBytesOfP2CharFromMarkP1( m, cnt )
	MARK	m ;
	int	cnt ;
{
	int	idx;
	unsigned char *bp ;
	long	nBytes ;
	int	n ;

	idx = markidx( m );
	pfetch(markline(m));
	bp = (unsigned char *) ptext + idx ;

	/* Find actual no. of bytes to be deleted;
	 * HZ-char is 2 bytes; else 1 byte.
	 */
	nBytes = 0 ;
	for ( n = cnt ; --n >= 0 ; ) {
		if ( IsHiBitOn( *( bp + nBytes ) ) ) {
			nBytes += 2 ;
		} else {
			nBytes ++ ;
		}
		if (nBytes > plen - idx)
			break;
	}
	return nBytes ;
}

int	FindNBytesOfP2CharBackFromMarkP1( m, cnt )
	MARK	m ;
	int	cnt ;
{
	int	idx;
	unsigned char *bp ;
	long	nBytes ;
	int	n ;

	idx = markidx( m );

	/* find actual # of chars from line beginning to mark m */
	n = FindNCharToMark (m) - cnt;
	if ( n < 0 )
		n = 0;

	/* find actual byte # of n chars from line beginning */
	nBytes = FindNBytesOfP2CharFromMarkP1( m - idx, n );

	return (idx - nBytes);
}

/* This function puts the editor in EX mode */
MARK v_quit()
{
	mode = MODE_EX;
	return cursor;
}

/* This function causes the screen to be redrawn */
MARK v_redraw()
{
	redraw(MARK_UNSET, FALSE);
	return cursor;
}

/* This function executes a single EX command, and waits for a user keystroke
 * before returning to the VI screen.  If that keystroke is another ':', then
 * another EX command is read and executed.
 */
/*ARGSUSED*/
MARK v_1ex(m, text)
	MARK	m;	/* the current line */
	char	*text;	/* the first command to execute */
{
	/* scroll up, so we don't overwrite the command */
	if (mode == MODE_COLON)
	{
		addch('\n');
		refresh();
	}

	/* run the command.  be careful about modes & output */
	exwrote = (mode == MODE_COLON);
	doexcmd(text);
	exrefresh();

	/* if mode is no longer MODE_VI, then we should quit right away! */
	if (mode != MODE_VI && mode != MODE_COLON)
		return cursor;

	/* The command did some output.  Wait for a keystoke. */
	if (exwrote)
	{
		mode = MODE_VI;	
		msg("[Hit any key to continue]");
		if (getkey(0) == ':')
		{	mode = MODE_COLON;
			addch('\n');
		}
		else
			redraw(MARK_UNSET, FALSE);
	}

	return cursor;
}

/* This function undoes the last change */
/*ARGSUSED*/
MARK v_undo(m)
	MARK	m;	/* (ignored) */
{
	undo();
	redraw(MARK_UNSET, FALSE);
	return cursor;
}

/* This function deletes the character(s) that the cursor is on */
MARK v_xchar(m, cnt)
	MARK	m;	/* where to start deletions */
	long	cnt;	/* number of chars to delete */
{
	DEFAULT(1);

	pfetch(markline(m));
	cnt = FindNBytesOfP2CharFromMarkP1( m, cnt );

	if (markidx(m + cnt) > plen)
	{
		cnt = plen - markidx(m);
	}

	if (cnt == 0L)
	{
		return MARK_UNSET;
	}

	ChangeText
	{
		cut(m, m + cnt);
		delete(m, m + cnt);
	}
	return m;
}

/* This function deletes character to the left of the cursor */
MARK v_Xchar(m, cnt)
	MARK	m;	/* where deletions end */
	long	cnt;	/* number of chars to delete */
{
	DEFAULT(1);

	/* if we're at the first char of the line, error! */
	if (markidx(m) == 0)
	{
		return MARK_UNSET;
	}

	cnt = FindNBytesOfP2CharBackFromMarkP1( m, cnt );
	/* make sure we don't try to delete more chars than there are */
	if (cnt > markidx(m))
	{
		cnt = markidx(m);
	}

	/* delete 'em */
	ChangeText
	{
		cut(m - cnt, m);
		delete(m - cnt, m);
	}

	return m - cnt;
}

/* This function defines a mark */
/*ARGSUSED*/
MARK v_mark(m, count, key)
	MARK	m;	/* where the mark will be */
	long	count;	/* (ignored) */
	int	key;	/* the ASCII label of the mark */
{
	if (key < 'a' || key > 'z')
	{
		msg("Marks must be from a to z");
	}
	else
	{
		mark[key - 'a'] = m;
	}
	return m;
}

/* This function toggles upper & lower case letters */
MARK v_ulcase(m)
	MARK	m;	/* where to make the change */
{
	char	new[2];
#if	MSDOS || TOS
	char	*pos;
	char	*lower="\207\201\202\204\206\221\224\244";
	char 	*upper="\200\232\220\216\217\222\231\245";
#endif

	/* extract the char that's there now */
	pfetch(markline(m));
	new[0] = ptext[markidx(m)];
	new[1] = '\0';

	/* change it if necessary */
	if (new[0] >= 'a' && new[0] <= 'z' || new[0] >= 'A' && new[0] <= 'Z')
	{
		new[0] ^= ('A' ^ 'a');
		ChangeText
		{
			change(m, m + 1, new);
		}
	}
#if	MSDOS || TOS
	if ((pos=strchr(lower, new[0]))!=0)
		new[0]=upper[(int)(pos-lower)];
	else if ((pos=strchr(upper, new[0]))!=0)
		new[0]=lower[(int)(pos-upper)];
	else
		goto nochange;		/* Urghh - GB */
	ChangeText
	{
		change(m, m + 1, new);
	}
nochange:
#endif
	if (new[0] && ptext[markidx(m) + 1])
	{
		m++;
	}
	return m;
}


MARK v_replace(m, cnt, key)
	MARK	m;	/* first char to be replaced */
	long	cnt;	/* number of chars to replace */
	int	key;	/* what to replace them with */
{
	register char	*text;
	register int	i;
	static int	samekey;
	int key2;	/* 2nd byte if it is a HZ char */
	int rlen;

	DEFAULT(1);

	rlen = FindNBytesOfP2CharFromMarkP1( m, cnt );

	/* map ^M to '\n' */
	if (key == '\r')
	{
		key = '\n';
	}
	else if (key == ctrl('V'))
	{
		if (doingdot)
			key = samekey;
		else
			key = samekey = getkey(0);
		if (key == 0)
			return MARK_UNSET;
	}
	else if (IsHiBitOn(key))
	{
		if (doingdot)
			key2 = samekey;
		else
			key2 = samekey = getkey(0);
		cnt *= 2;
	}
	else if (!doingdot && key == ctrl('['))
	{
		samekey = 0;
		return MARK_UNSET;
	}

	/* make sure the resulting line isn't too long */
	if (cnt > BLKSIZE - 2 - markidx(m))
	{
		cnt = BLKSIZE - 2 - markidx(m);
	}

	/* build a string of the desired character with the desired length */
	for (text = tmpblk.c, i = cnt; i > 0; i--)
	{
		*text++ = key;
		if (IsHiBitOn(key)) {
			*text++ = key2;
			i--;
		}
	}
	*text = '\0';

	/* make sure cnt doesn't extend past EOL */
	pfetch(markline(m));
	if (markidx(m) + rlen > plen)
	{
		rlen = plen - markidx(m);
	}

	/* do the replacement */
	ChangeText
	{
		change(m, m + rlen, tmpblk.c);
	}

	if (*tmpblk.c == '\n')
	{
		return (m & ~(BLKSIZE - 1)) + cnt * BLKSIZE;
	}
	else
	{
		if (IsHiBitOn(key))
			return m + cnt - 2;
		else
			return m + cnt - 1;
	}
}

MARK v_overtype(m)
	MARK		m;	/* where to start overtyping */
{
	MARK		end;	/* end of a substitution */
	static long	width;	/* width of a single-line replace */

	/* the "doingdot" version of replace is really a substitution */
	if (doingdot)
	{
		/* was the last one really repeatable? */
		if (width < 0)
		{
			msg("Can't repeat a multi-line overtype command");
			return MARK_UNSET;
		}

		/* replacing nothing by nothing?  Don't bother */
		if (width == 0)
		{
			return m;
		}

		/* markidx(m) + width is 1st byte of a HZ? */
		pfetch(markline(m));
		if (IsIdxHZ (markidx(m) + width, ptext) > 0)
		{
			/* markidx(m) + width is at 1st byte HZ */
			/* replace some plus one chars by repeated text */
			return v_subst(m, width+1);
		}

		/* replace some chars by repeated text */
		return v_subst(m, width);
	}

	/* Normally, we input starting here, in replace mode */
	ChangeText
	{
		end = input(m, m, WHEN_VIREP);
	}

	/* if we ended on the same line we started on, then this
	 * overtype is repeatable via the dot key.
	 */
	if (markline(end) == markline(m) && end >= m - 1L)
	{
		width = end - m + 1L;
		if (IsHiBitOnMark(end))
			width++;
	}
	else if (markline(end) == markline(m) && IsHiBitOnMark(end) && end == m - 2L)
	{
		width = 0;
	}
	else /* it isn't repeatable */
	{
		width = -1L;
	}

	return end;
}


/* This function selects which cut buffer to use */
/*ARGSUSED*/
MARK v_selcut(m, cnt, key)
	MARK	m;
	long	cnt;
	int	key;
{
	cutname(key);
	return m;
}

/* This function pastes text from a cut buffer */
/*ARGSUSED*/
MARK v_paste(m, cnt, cmd)
	MARK	m;	/* where to paste the text */
	long	cnt;	/* (ignored) */
	int	cmd;	/* either 'p' or 'P' */
{
	ChangeText
	{
		m = paste(m, cmd == 'p', FALSE);
	}
	return m;
}

/* This function yanks text into a cut buffer */
MARK v_yank(m, n)
	MARK	m, n;	/* range of text to yank */
{
	cut(m, n);
	return m;
}

/* This function deletes a range of text */
MARK v_delete(m, n)
	MARK	m, n;	/* range of text to delete */
{
	long	nBytes ;

	/* illegal to try and delete nothing */
	if (n <= m)
	{
		return MARK_UNSET;
	}

	/* Do it */
	ChangeText
	{
		cut(m, n);
		delete(m, n);
	}
	return m;
}


/* This starts input mode without deleting anything */
MARK v_insert(m, cnt, key)
	MARK	m;	/* where to start (sort of) */
	long	cnt;	/* repeat how many times? */
	int	key;	/* what command is this for? {a,A,i,I,o,O} */
{
	int	wasdot;
	long	reps;

	DEFAULT(1);

	ChangeText
	{
		/* tweak the insertion point, based on command key */
		switch (key)
		{
		  case 'i':
			break;

		  case 'a':
			pfetch(markline(m));
			if (plen > 0)
			{
				if ( IsHiBitOnMark(m) ) {
					m += 2 ;
				} else {
					m ++ ;
				}
			}
			break;

		  case 'I':
			m = m_front(m, 1L);
			break;

		  case 'A':
			pfetch(markline(m));
			m = (m & ~(BLKSIZE - 1)) + plen;
			break;

		  case 'O':
			m &= ~(BLKSIZE - 1);
			add(m, "\n");
			break;

		  case 'o':
			m = (m & ~(BLKSIZE - 1)) + BLKSIZE;
			add(m, "\n");
			break;
		}

		/* insert the same text once or more */
		for (reps = cnt, wasdot = doingdot; reps > 0; reps--, doingdot = TRUE)
		{
			m = input(m, m, WHEN_VIINP);
		}

		/* compensate for inaccurate redraw clues from input() */
		if ((key == 'O' || key == 'o') && wasdot)
		{
			redraw(MARK_UNSET);
		}

		doingdot = FALSE;
	}

	return m;
}

/* This starts input mode with some text deleted */
MARK v_change(m, n)
	MARK	m, n;	/* the range of text to change */
{
	int	lnmode;	/* is this a line-mode change? */

	/* swap them if they're in reverse order */
	if (m > n)
	{
		MARK	tmp;
		tmp = m;
		m = n;
		n = tmp;
	}

	/* for line mode, retain the last newline char */
	lnmode = (markidx(m) == 0 && markidx(n) == 0 && m != n);
	if (lnmode)
	{
		n -= BLKSIZE;
		pfetch(markline(n));
		n = (n & ~(BLKSIZE - 1)) + plen;
	}

	ChangeText
	{
		cut(m, n);
		m = input(m, n, WHEN_VIINP);
	}

	/* compensate for inaccurate redraw clues from paste() */
	if (doingdot)
	{
		preredraw = markline(n);
		if (lnmode)
		{
			preredraw++;
			postredraw++;
		}
	}

	return m;
}

/* This function replaces a given number of characters with input */
MARK v_subst(m, cnt)
	MARK	m;	/* where substitutions start */
	long	cnt;	/* number of chars to replace */
{
	DEFAULT(1);

	/* make sure we don't try replacing past EOL */
	pfetch(markline(m));
	cnt = FindNBytesOfP2CharFromMarkP1( m, cnt );
	if (markidx(m) + cnt > plen)
	{
		cnt = plen - markidx(m);
	}

	/* Go for it! */
	ChangeText
	{
		cut(m, m + cnt);
		m = input(m, m + cnt, WHEN_VIINP);
	}
	return m;
}

/* This calls the ex "join" command to join some lines together */
MARK v_join(m, cnt)
	MARK	m;	/* the first line to be joined */
	long	cnt;	/* number of other lines to join */
{
	MARK	joint;	/* where the lines were joined */

	DEFAULT(1);

	/* figure out where the joint will be */
	pfetch(markline(m));
	joint = (m & ~(BLKSIZE - 1)) + plen;

	/* join the lines */
	cmd_join(m, m + MARK_AT_LINE(cnt), CMD_JOIN, 0, "");
	mustredraw = TRUE;

	/* the cursor should be left at the joint */
	return joint;
}

/* This calls the ex shifter command to shift some lines */
static MARK shift_help(m, n, excmd)
	MARK	m, n;	/* range of lines to shift */
	CMD	excmd;	/* which way do we shift? */
{
	/* make sure our endpoints aren't in reverse order */
	if (m > n)
	{
		MARK tmp;

		tmp = n;
		n = m;
		m = tmp;
	}

	/* linemode? adjust for inclusive endmarks in ex */
	if (markidx(m) == 0 && markidx(n) == 0)
	{
		n -= BLKSIZE;
	}

	cmd_shift(m, n, excmd, 0, "");
	return m;
}

/* This calls the ex "<" command to shift some lines left */
MARK v_lshift(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	return shift_help(m, n, CMD_SHIFTL);
}

/* This calls the ex ">" command to shift some lines right */
MARK v_rshift(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	return shift_help(m, n, CMD_SHIFTR);
}

/* This runs some lines through a filter program */
MARK v_filter(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	char	cmdln[100];	/* a shell command line */

	/* linemode? adjust for inclusive endmarks in ex */
	if (markidx(m) == 0 && markidx(n) == 0)
	{
		n -= BLKSIZE;
	}

	if (vgets('!', cmdln, sizeof(cmdln)) > 0)
	{
		filter(m, n, cmdln);
	}

	redraw(MARK_UNSET, FALSE);
	return m;
}


/* This function runs the ex "file" command to show the file's status */
MARK v_status()
{
	cmd_file(cursor, cursor, CMD_FILE, 0, "");
	return cursor;
}

/* This function switches to the previous file, if possible */
MARK v_switch()
{
	if (!*prevorig)
		msg("No previous file");
	else
	{	strcpy(tmpblk.c, prevorig);
		cmd_edit(cursor, cursor, CMD_EDIT, 0, tmpblk.c);
	}
	return cursor;
}

/* This function does a tag search on a keyword */
/*ARGSUSED*/
MARK v_tag(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	/* remember the initial change level */
	cnt = changes;

	/* move the cursor to the start of the tag name, where m is */
	cursor = m;

	/* perform the tag search */
	cmd_tag(cursor, cursor, CMD_TAG, 0, keyword);

	return cursor;
}

#ifndef NO_EXTENSIONS
/* This function looks up a keyword by calling the helpprog program */
/*ARGSUSED*/
MARK v_keyword(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	int	status;
#if	TOS
	char cmdline[130];
#endif

	move(LINES - 1, 0);
	addstr("---------------------------------------------------------\n");
	clrtoeol();
	refresh();
	suspend_curses();

#if	ANY_UNIX

	switch (fork())
	{
	  case -1:						/* error */
		break;

	  case 0:						/* child */
		execl(o_keywordprg, o_keywordprg, keyword, (char *)0);
		HZ_abort(2); /* if we get here, the exec failed */

	  default:						/* parent */
		wait(&status);
		if (status > 0)
		{
			write(2, "<<< failed >>>\n", 15);
		}
	}

#endif
#if	MSDOS
	if ((status=spawnlp(P_WAIT, o_keywordprg, o_keywordprg, keyword,
						(char *)0))==-1)
		write(2, "<<< failed >>>\n", 15);
#endif
#if	TOS
	strcpy(cmdline+1, keyword);
	cmdline[0]=strlen(keyword);
	if ((status=Pexec(0, o_keywordprg, cmdline, "\0"))<0)
		write(2, "<<< failed >>>\n", 15);
#endif

	resume_curses(FALSE); /* "resume, but not quietly" */
	redraw(MARK_UNSET, FALSE);
	return m;
}



MARK v_increment(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	static	sign;
	char	newval[12];
	long	atol();

	DEFAULT(1);

	/* get one more keystroke, unless doingdot */
	if (!doingdot)
	{
		sign = getkey(0);
	}

	/* adjust the number, based on that second keystroke */
	switch (sign)
	{
	  case '+':
	  case '#':
		cnt = atol(keyword) + cnt;
		break;

	  case '-':
		cnt = atol(keyword) - cnt;
		break;

	  case '=':
		break;

	  default:
		return MARK_UNSET;
	}
	sprintf(newval, "%ld", cnt);

	ChangeText
	{
		change(m, m + strlen(keyword), newval);
	}

	return m;
}
#endif


/* This function acts like the EX command "xit" */
/*ARGSUSED*/
MARK v_xit(m, cnt, key)
	MARK	m;	/* ignored */
	long	cnt;	/* ignored */
	int	key;	/* must be a second 'Z' */
{
	/* if second char wasn't 'Z', fail */
	if (key != 'Z')
	{
		return MARK_UNSET;
	}

	/* move the physical cursor to the end of the screen */
	move(LINES - 1, 0);
	clrtoeol();
	refresh();

	/* do the xit command */
	cmd_xit(m, m, CMD_XIT, FALSE, "");

	/* if we're really going to quit, then scroll the screen up 1 line */
	if (mode == MODE_QUIT)
	{
		addch('\n');
		refresh();
	}

	/* regardless of whether we succeeded or failed, return the cursor */
	return m;
}


/* This function undoes changes to a single line, if possible */
MARK v_undoline(m)
	MARK	m;	/* where we hope to undo the change */
{
	if (markline(m) != U_line)
	{
		return MARK_UNSET;
	}

	ChangeText
	{
		changeline(U_line, U_text);
	}
	return m & ~(BLKSIZE - 1);
}
