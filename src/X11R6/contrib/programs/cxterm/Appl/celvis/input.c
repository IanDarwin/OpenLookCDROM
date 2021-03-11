/* input.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains the input() function, which implements vi's INPUT mode.
 * It also contains the code that supports digraphs.
 */

#include <ctype.h>
#include "config.h"
#include "vi.h"


#ifndef NO_DIGRAPH
static struct
{
	char	key1;
	char	key2;
	char	dig;
} digs[MAXDIGS];

static char digraph(key1, key2)
	char	key1;	/* the first character (punctuation) */
	char	key2;	/* the overtyped character (probably a letter) */
{
	int	i;

	/* can only be a digraph if key1 is punctuation */
	if (!isascii(key1) || !ispunct(key1))
	{
		return key2;
	}

	/* scan through the digraph chart */
	for (i = 0;
	     i < MAXDIGS && (!digs[i].dig || digs[i].key1 != key1 || digs[i].key2 != key2);
	     i++)
	{
	}

	/* if this combination isn't in there, just use the new key */
	if (i >= MAXDIGS)
	{
		return key2;
	}

	/* else use the digraph key */
	return digs[i].dig;
}

do_digraph(bang, args)
	int	bang;
	char	args[];
{
	int	i;
	int	dig;

	/* if no args, then display the existing digraphs */
	if (*args < ' ')
	{
		for (i = 0; i < MAXDIGS; i++)
		{
			if (digs[i].dig)
			{
				msg("digraph %c%c %c", digs[i].key1, digs[i].key2, digs[i].dig);
			}
		}
		return;
	}

	/* first character of a digraph must be punctuation */
	if (!isascii(args[0]) || !ispunct(args[0]))
	{
		msg("The first character of a digraph must be punctuation");
		return;
	}
	if (!args[1])
	{
		msg("Digraphs must be composed of two characters");
		return;
	}

	/* locate the new digraph character */
	for (i = 2; args[i] == ' ' || args[i] == '\t'; i++)
	{
	}
	dig = args[i];
	if (!bang && dig)
	{
		dig |= 0x80;
	}

	/* search for the digraph */
	for (i = 0; i < MAXDIGS && (digs[i].key1 != args[0] || digs[i].key2 != args[1]); i++)
	{
	}
	if (i >= MAXDIGS)
	{
		for (i = 0; i < MAXDIGS && digs[i].dig; i++)
		{
		}
		if (i >= MAXDIGS)
		{
			msg("Out of space in the digraph table");
			return;
		}
	}

	/* assign it the new digraph value */
	digs[i].key1 = args[0];
	digs[i].key2 = args[1];
	digs[i].dig = dig;
}

savedigs(fd)
	int		fd;
{
	int		i;
	static char	buf[] = "digraph! XX Y\n";

	for (i = 0; i < MAXDIGS; i++)
	{
		if (digs[i].dig)
		{
			buf[9] = digs[i].key1;
			buf[10] = digs[i].key2;
			buf[12] = digs[i].dig;
			write(fd, buf, 14);
		}
	}
}
#endif


/* This function allows the user to replace an existing (possibly zero-length)
 * chunk of text with typed-in text.  It returns the MARK of the last character
 * that the user typed in.
 */
MARK input(from, to, when)
	MARK	from;	/* where to start inserting text */
	MARK	to;	/* extent of text to delete */
	int	when;	/* either WHEN_VIINP or WHEN_VIREP */
{
	unsigned char	key[3];	/* key char or HZ followed by '\0' */
	unsigned char	keyTemp[2];	/* HZ 1st byte followed by '\0'*/
					/* for v_subst() with HZ input */
	int		nByteInput ;
	char	*build;	/* used in building a newline+indent string */
	char	*scan;	/* used while looking at the indent chars of a line */
	MARK	m;	/* some place in the text */
#ifndef NO_EXTENSIONS
	int	quit = FALSE;	/* boolean: are we exiting after this? */
#endif

#ifdef DEBUG
	/* if "from" and "to" are reversed, complain */
	if (from > to)
	{
		msg("ERROR: input(%ld:%d, %ld:%d)",
			markline(from), markidx(from),
			markline(to), markidx(to));
		return MARK_UNSET;
	}
#endif

	key[2] = 0; /* may assign key[1] = 0 or 2nd byte of HZ later */
	keyTemp[1] = 0 ;	/* pre-assign */

	/* if we're replacing text with new text, save the old stuff */
	/* (Alas, there is no easy way to save text for replace mode) */
	if (from != to)
	{
		cut(from, to);
	}

	ChangeText
	{
		/* if doing a dot command, then reuse the previous text */
		if (doingdot)
		{
			/* delete the text that's there now */
			if (from != to)
			{
				delete(from, to);
			}

			/* insert the previous text */
			cutname('.');
			cursor = paste(from, FALSE, TRUE) + 1L;
			/* explicitly set preredraw and postredraw */
			redrawafter = markline(from);
			preredraw = markline(to);
			postredraw = markline(cursor);
		}
		else /* interactive version */
		{
			int addspace = 0;

			/* if doing a change within the line... */
			if (from != to && markline(from) == markline(to))
			{
				/* mark the end of the text with a "$" */
				pfetch(markline(to-1));
				if (IsIdxHZ(markidx(to-1), ptext) < 0)
					change (to - 2, to, " $");
				else
					change(to - 1, to, "$");
			}
			else
			{
				/* delete the old text right off */
				if (from != to)
				{
					delete(from, to);
				}
				to = from;
			}

			/* handle autoindent of the first line, maybe */
			cursor = from;
			if (*o_autoindent && markline(cursor) > 1L && markidx(cursor) == 0)
			{
				/* Only autoindent blank lines. */
				pfetch(markline(cursor));
				if (plen == 0)
				{
					/* Okay, we really want to autoindent */
					pfetch(markline(cursor) - 1L);
					for (scan = ptext, build = tmpblk.c;
					     *scan == ' ' || *scan == '\t';
					     )
					{
						*build++ = *scan++;
					}
					if (build > tmpblk.c)
					{
						*build = '\0';
						add(cursor, tmpblk.c);
						cursor += (build - tmpblk.c);
					}
				}
			}

			/* repeatedly add characters from the user */
			nByteInput = 0 ;

			for (;;)
			{
				/* Get a character */
				redraw(cursor, TRUE);
				key[0] = getkey(when);

				if ( IsHiBitOn( key[0] ) ) {
					key[1] = getkey(when);
					nByteInput = 2 ;
				} else {
					key[1] = 0 ;
					nByteInput = 1 ;
				}
				/* if whitespace & wrapmargin is set & we're
				/* past the warpmargin, then change the
				/* whitespace character into a newline
				 */
				if ((*key == ' ' || *key == '\t')
				 && *o_wrapmargin != 0)
				{
					pfetch(markline(cursor));
					if (idx2col(cursor, ptext, TRUE) > COLS - (*o_wrapmargin & 0xff))
					{
						*key = '\n';
					}
				}

				/* process it */
				switch (*key)
				{
#ifndef NO_EXTENSIONS
				  case 0: /* special movement mapped keys */
					*key = getkey(0);
					switch (*key)
					{
					  case 'h':	m = m_left(cursor, 0L);		break;
					  case 'j':	m = m_down(cursor, 0L);		break;
					  case 'k':	m = m_up(cursor, 0L);		break;
					  case 'l':	m = cursor + 1;
							if (IsHiBitOnMark(cursor))
								m++ ;
							break;
					  case 'b':	m = m_bword(cursor, 0L);	break;
					  case 'w':	m = m_fword(cursor, 0L);	break;
					  case '^':	m = m_front(cursor, 0L);	break;
					  case '$':	m = m_rear(cursor, 0L);		break;
					  case ctrl('B'):
					  case ctrl('F'):
							m = m_scroll(cursor, 0L, *key); break;
					  case 'x':	m = v_xchar(cursor, 0L);	break;
					  case 'i':	m = to = from = cursor;		break;
					  default:	m = MARK_UNSET;			break;
					}
					/* adjust the moved cursor */
					m = adjmove(cursor, m, (*key == 'j' || *key == 'k' ? 0x20 : 0));
					if (*key == '$' || (*key == 'l' && m <= cursor))
					{
						m++;
					}
					/* if the cursor is reasonable, use it */
					if (m == MARK_UNSET)
					{
						beep();
					}
					else
					{
						if (to > cursor)
						{
							delete(cursor, to);
							redraw(cursor, TRUE);
						}
						from = to = cursor = m;
					}
					break;

				  case ctrl('Z'):
					if (getkey(0) == ctrl('Z'))
					{
						quit = TRUE;
						goto BreakBreak;
					}
					break;
#endif
				  case ctrl('['):
					goto BreakBreak;

				  case ctrl('U'):
					if (markline(cursor) == markline(from))
					{
						cursor = from;
					}
					else
					{
						cursor &= ~(BLKSIZE - 1);
					}
					break;

				  case ctrl('D'):
				  case ctrl('T'):
					mark[27] = cursor;
					cmd_shift(cursor, cursor, *key == ctrl('D') ? CMD_SHIFTL : CMD_SHIFTR, TRUE, "");
					if (mark[27])
					{
						cursor = mark[27];
					}
					else
					{
						cursor = m_front(cursor, 0L);
					}
					break;

				  case '\b':
					if (cursor <= from)
					{
						beep();
					}
					else if (markidx(cursor) == 0)
					{
						cursor -= BLKSIZE;
						pfetch(markline(cursor));
						cursor += plen;
					}
					else
					{
						cursor = m_left(cursor, 1L) ;
					}
					break;

				  case ctrl('W'):
					m = m_bword(cursor, 1L);
					if (markline(m) == markline(cursor) && m >= from)
					{
						cursor = m;
						if (from > cursor)
						{
							from = cursor;
						}
					}
					else
					{
						beep();
					}
					break;

				  case '\n':
				  case '\r':
					build = tmpblk.c;
					*build++ = '\n';
					if (*o_autoindent)
					{
						pfetch(markline(cursor));
						for (scan = ptext; *scan == ' ' || *scan == '\t'; )
						{
							*build++ = *scan++;
						}
					}
					*build = 0;
					if (cursor >= to && when != WHEN_VIREP)
					{
						add(cursor, tmpblk.c);
					}
					else
					{
						change(cursor, to, tmpblk.c);
					}
					redraw(cursor, TRUE);
					to = cursor = (cursor & ~(BLKSIZE - 1))
							+ BLKSIZE
							+ (int)(build - tmpblk.c) - 1;
					break;

				  case ctrl('A'):
				  case ctrl('P'):
					if (cursor < to)
					{
						delete(cursor, to);
					}
					if (*key == ctrl('A'))
					{
						cutname('.');
					}
					to = cursor = paste(cursor, FALSE, TRUE) + 1L;
					break;

				  case ctrl('V'):
					if (cursor >= to && when != WHEN_VIREP)
					{
						add(cursor, "^");
					}
					else
					{
						change(cursor, to, "^");
						to = cursor + 1;
					}
					redraw(cursor, TRUE);
					*key = getkey(0);
					if (*key == '\n')
					{
						/* '\n' too hard to handle */
						*key = '\r';
					}
					change(cursor, cursor + 1, key);
					cursor++;
					if (cursor > to)
					{
						to = cursor;
					}
					break;

				  case ctrl('L'):
				  case ctrl('R'):
					redraw(MARK_UNSET, FALSE);
					break;

				  default:
					if (cursor >= to && when != WHEN_VIREP)
					{
						add(cursor, key);
						cursor += nByteInput ; /* += 1 or 2 */
						to = cursor;
					}
					else
					{

					/* cursor < to || when == WHEN_VIREP */
					/* called from v_subst() ... */
					/* or after some \b... */

						pfetch(markline(cursor));
						if (markidx(cursor) == plen)
						{
							add(cursor, key);
							addspace = 0;
						}
						else
						{
#ifndef NO_DIGRAPH
							*key = digraph(ptext[markidx(cursor)], *key);
#endif
							if ( cursor + nByteInput < to || when == WHEN_VIREP ) {
								if ((nByteInput == 1)
								    && IsHiBitOnMark(cursor))
								{ /* an ASC over 1st half of a HZ */
									change(cursor+1, cursor+2, " ");
									addspace = 1;
								} else if ((nByteInput == 2)
								    && !IsHiBitOnMark(cursor)
								    && IsHiBitOnMark(cursor+1))
								{ /* an HZ over an ASC + half HZ */
									change(cursor+2, cursor+3, " ");
									addspace = 1;
								} else 
									addspace = 0;
								change(cursor, cursor + nByteInput, key);
							} else if ( cursor + nByteInput <= to ) {
								change(cursor, cursor + nByteInput, key);
								addspace = 0;
							} else { /* nByteInput crosses to */
								keyTemp[0] = key[0] ;
								change(cursor, cursor + 1, keyTemp);
								if ( nByteInput == 2 ) {
									/* add the 2nd byte */
									add( cursor + 1, &key[1] );
									to++ ;
								}
								addspace = 0;
							}
						}
						cursor += nByteInput ; /* += 1 or 2 */
					}
				} /* end switch(*key) */
			} /* end for(;;) */
BreakBreak:;

			/* delete any excess characters */
			if (cursor < to)
			{
				delete(cursor, to);
			}
			else if ((when == WHEN_VIREP) && addspace)
				delete(cursor, cursor+1);

		} /* end if doingdot else */

	} /* end ChangeText */

	/* put the new text into a cut buffer for possible reuse */
	if (!doingdot)
	{
		blksync();
		cutname('.');
		cut(from, cursor);
	}

	/* move to last char that we inputted, unless it was newline */
	if (markidx(cursor) != 0)
	{
		cursor--;
		if ( IsIdxHZ(markidx(cursor), ptext) < 0 )
			cursor--;
	}
	redraw(cursor, FALSE);

#ifndef NO_EXTENSIONS
	if (quit)
	{
		refresh();
		cursor = v_xit(cursor, 0L, 'Z');
	}
#endif

	rptlines = 0L;
	return cursor;
}
