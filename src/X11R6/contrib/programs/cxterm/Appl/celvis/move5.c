/* m_5.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains the word-oriented movement functions */

#include <ctype.h>
#include "config.h"
#include "vi.h"

#ifndef isascii
# define isascii(c)	!((c) & ~0x7f)
#endif
# define At1stHZbyte(text)	(IsIdxHZ((int)((text)-ptext),ptext) > 0)
# define At2ndHZbyte(text)	(IsIdxHZ((int)((text)-ptext),ptext) < 0)
# define AtASCII(text)		(IsIdxHZ((int)((text)-ptext),ptext) == 0)


MARK	m_fword(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;
	register int	i;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		i = *text++;
		/* if we hit the end of the line, continue with next line */
		if (IsHiBitOn(i))
		{
			/* include contiguous HZ */
			while (i && IsHiBitOn(i))
			{
				text++;
				i = *text++;
			}
		}
		else if (isalnum(i) || i == '_')
		{
			/* include an alphanumeric word */
			while (i && isascii(i) && (isalnum(i) || i == '_'))
			{
				i = *text++;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (i && isascii(i) && !isalnum(i) && !isspace(i))
			{
				i = *text++;
			}
		}

		/* include trailing whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}
		text--;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}


MARK	m_bword(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		text--;

		/* include preceding whitespace */
		while (text < ptext || isascii(*text) && isspace(*text))
		{
			/* did we hit the end of this line? */
			if (text < ptext)
			{
				/* move to preceding line, if there is one */
				l--;
				if (l <= 0)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext + plen - 1;
			}
			else
			{
				text--;
			}
		}

		if (At2ndHZbyte (text))
		{
			/* include contiguous HZ */
			while (text >= ptext && At2ndHZbyte (text))
			{
				text -= 2;
			}
		}
		else if (isalnum(*text) || *text == '_')
		{
			/* include an alphanumeric word */
			while (text >= ptext && AtASCII(text) && (isalnum(*text) || *text == '_'))
			{
				text--;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (text >= ptext && AtASCII(text) && !isalnum(*text) && !isspace(*text))
			{
				text--;
			}
		}
		text++;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}

MARK	m_eword(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;
	register int	i;
	int	idx;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	idx = markidx(m) ;
	text = ptext + idx ;
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		if ( IsHiBitOn( *( text ) ) ) {
			text ++ ;
		}
		text++;
		i = *text++;

		/* include preceding whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}

		if (IsHiBitOn(i))
		{
			/* include contiguous HZ */
			while (i && IsHiBitOn(i))
			{
				text++;
				i = *text++;
			}
		}
		else if (isalnum(i) || i == '_')
		{
			/* include an alphanumeric word */
			while (i && isascii(i) && (isalnum(i) || i == '_'))
			{
				i = *text++;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (i && isascii(i) && !isalnum(i) && !isspace(i))
			{
				i = *text++;
			}
		}
		text -= 2;
		if (At2ndHZbyte (text))
			text --;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}

MARK	m_fWord(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;
	register int	i;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		i = *text++;
		/* if we hit the end of the line, continue with next line */
		if (IsHiBitOn (i))
		{
			/* include contiguous HZ */
			while (i && IsHiBitOn(i))
			{
				text++;
				i = *text++;
			}
		}
		else
		{
			/* include contiguous non-space characters */
			while (i && isascii(i) && !isspace(i))
			{
				i = *text++;
			}
		}

		/* include trailing whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}
		text--;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}


MARK	m_bWord(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		text--;

		/* include trailing whitespace */
		while (text < ptext || isascii(*text) && isspace(*text))
		{
			/* did we hit the end of this line? */
			if (text < ptext)
			{
				/* move to next line, if there is one */
				l--;
				if (l <= 0)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext + plen - 1;
			}
			else
			{
				text--;
			}
		}

		if (At2ndHZbyte (text))
		{
			/* include contiguous HZ */
			while (text >= ptext && At2ndHZbyte(text))
			{
				text -= 2;
			}
		}
		else
		{
			/* include contiguous non-whitespace */
			while (text >= ptext && AtASCII(text) && !isspace(*text))
			{
				text--;
			}
		}
		text++;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}

MARK	m_eWord(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	register long	l;
	register char	*text;
	register int	i;
	int	idx;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	idx = markidx(m) ;
	text = ptext + idx ;
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		text++;

		if ( IsHiBitOn( *( text ) ) ) {
			text ++ ;
		}
		i = *text++;

		/* include preceding whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}

		if (IsHiBitOn (i))
		{
			/* include contiguous HZ */
			while (i && IsHiBitOn(i))
			{
				text++;
				i = *text++;
			}
		}
		else 
		{
			/* include contiguous non-whitespace */
			while (i && isascii(i) && !isspace(i))
			{
				i = *text++;
			}
		}
		text -= 2;
		if (At2ndHZbyte (text))
			text --;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}
