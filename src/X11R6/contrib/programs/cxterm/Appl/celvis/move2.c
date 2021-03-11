/* m_2.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This function contains the mvoement functions that perform RE searching */

#include "config.h"
#include "vi.h"
#include "regexp.h"

static regexp	*re;	/* compiled version of the pattern to search for */
static		prevsf;	/* boolean: previous search direction was forward? */

MARK	m_nsrch(m)
	MARK	m;	/* where to start searching */
{
	if (prevsf)
	{
		m = m_fsrch(m, "");
		prevsf = TRUE;
	}
	else
	{
		m = m_bsrch(m, "");
		prevsf = FALSE;
	}
	return m;
}

MARK	m_Nsrch(m)
	MARK	m;	/* where to start searching */
{
	if (prevsf)
	{
		m = m_bsrch(m, "");
		prevsf = TRUE;
	}
	else
	{
		m = m_fsrch(m, "");
		prevsf = FALSE;
	}
	return m;
}

MARK	m_fsrch(m, ptrn)
	MARK	m;	/* where to start searching */
	char	*ptrn;	/* pattern to search for */
{
	long	l;	/* line# of line to be searched */
	char	*line;	/* text of line to be searched */
	int	wrapped;/* boolean: has our search wrapped yet? */
	int	pos;	/* where we are in the line */

	/* remember: "previous search was forward" */
	prevsf = TRUE;

	if (ptrn && *ptrn)
	{
		/* free the previous pattern */
		if (re) free(re);

		/* compile the pattern */
		re = regcomp(ptrn);
		if (!re)
		{
			return MARK_UNSET;
		}
	}
	else if (!re)
	{
		msg("No previous expression");
		return MARK_UNSET;
	}

	/* search forward for the pattern */
	pos = markidx(m) + 1;
	pfetch(markline(m));
	if (pos >= plen)
	{
		pos = 0;
		m = (m | (BLKSIZE - 1)) + 1;
	}
	wrapped = FALSE;
	for (l = markline(m); l != markline(m) + 1 || !wrapped; l++)
	{
		/* wrap search */
		if (l > nlines)
		{
			/* if we wrapped once already, then the search failed */
			if (wrapped)
			{
				break;
			}

			/* else maybe we should wrap now? */
			if (*o_wrapscan)
			{
				l = 0;
				wrapped = TRUE;
				continue;
			}
			else
			{
				break;
			}
		}

		/* get this line */
		line = fetchline(l);

		/* check this line */
		while (regexec(re, &line[pos], (pos == 0)))
		{
			/* match! */
			pos = (int)(re->startp[0] - line);
			if (IsIdxHZ (pos, line) < 0)
			{	/* bogus match, at 2nd-byte HZ! */
				pos ++;
				continue;	/* from new pos at this line */
			}

			/* real match! */
			if (wrapped && *o_warn)
				msg("(wrapped)");
			return MARK_AT_LINE(l) + pos;
		}
		pos = 0;
	}

	/* not found */
	msg(*o_wrapscan ? "Not found" : "Hit bottom without finding RE");
	return MARK_UNSET;
}

MARK	m_bsrch(m, ptrn)
	MARK	m;	/* where to start searching */
	char	*ptrn;	/* pattern to search for */
{
	long	l;	/* line# of line to be searched */
	char	*line;	/* text of line to be searched */
	int	wrapped;/* boolean: has our search wrapped yet? */
	int	pos;	/* last acceptable idx for a match on this line */
	int	last;	/* remembered idx of the last acceptable match on this line */
	int	try;	/* an idx at which we strat searching for another match */

	/* remember: "previous search was not forward" */
	prevsf = FALSE;

	if (ptrn && *ptrn)
	{
		/* free the previous pattern, if any */
		if (re) free(re);

		/* compile the pattern */
		re = regcomp(ptrn);
		if (!re)
		{
			return MARK_UNSET;
		}
	}
	else if (!re)
	{
		msg("No previous expression");
		return MARK_UNSET;
	}

	/* search backward for the pattern */
	pos = markidx(m);
	wrapped = FALSE;
	for (l = markline(m); l != markline(m) - 1 || !wrapped; l--)
	{
		/* wrap search */
		if (l < 1)
		{
			if (*o_wrapscan)
			{
				l = nlines + 1;
				wrapped = TRUE;
				continue;
			}
			else
			{
				break;
			}
		}

		/* get this line */
		line = fetchline(l);

		/* check this line */
		while (regexec(re, line, 1) && (int)(re->startp[0] - line) < pos)
		{
			/* match!  now find the last acceptable one in this line */
			do
			{
				last = (int)(re->startp[0] - line);
				try = (int)(re->endp[0] - line);
			} while (try > 0
				 && regexec(re, &line[try], FALSE)
				 && (int)(re->startp[0] - line) < pos);

			if (IsIdxHZ (last, line) < 0)
			{	/* bogus match, at 2nd-byte HZ! */
				pos = last;
				continue;	/* from new pos at this line */
			}

			if (wrapped && *o_warn)
				msg("(wrapped)");
			return MARK_AT_LINE(l) + last;
		}
		pos = BLKSIZE;
	}

	/* not found */
	msg(*o_wrapscan ? "Not found" : "Hit top without finding RE");
	return MARK_UNSET;
}

