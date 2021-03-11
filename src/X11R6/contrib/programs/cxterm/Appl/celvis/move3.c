/* m_3.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains movement functions that perform character searches */

#include "config.h"
#include "vi.h"

#ifndef NO_CHARSEARCH
static MARK	(*prevfwdfn)();	/* function to search in same direction */
static MARK	(*prevrevfn)();	/* function to search in opposite direction */
static char	prev_key;	/* sought cvhar from previous [fFtT] */
static char	prev_key2;	/* second byte of sought char if it is HANZI */
int		samekey = 0;	/* boolean: is command ';' or ','? */

MARK	m__ch(m, cnt, cmd)
	MARK	m;	/* current position */
	long	cnt;
	char	cmd;	/* command: either ',' or ';' */
{
	MARK	(*tmp)();

	if (!prevfwdfn)
	{
		msg("No previous f, F, t, or T command");
		return MARK_UNSET;
	}

	samekey = 1;

	if (cmd == ',')
	{
		m =  (*prevrevfn)(m, cnt, prev_key);

		/* Oops! we didn't want to change the prev*fn vars! */
		tmp = prevfwdfn;
		prevfwdfn = prevrevfn;
		prevrevfn = tmp;

		return m;
	}
	else
	{
		return (*prevfwdfn)(m, cnt, prev_key);
	}
}

/* move forward within this line to next occurrence of key */
MARK	m_fch(m, cnt, key)
	MARK	m;	/* where to search from */
	long	cnt;
	char	key;	/* what to search for */
{
	register char	*text;
	int	key2;	/* 2nd byte if HANZI */

	DEFAULT(1);

	prevfwdfn = m_fch;
	prevrevfn = m_Fch;
	prev_key = key;
	if (samekey)
		key2 = prev_key2;
	else if (IsHiBitOn(key))
		prev_key2 = key2 = getkey(0);
	samekey = 0;

	pfetch(markline(m));
	text = ptext + markidx(m);
	while (cnt-- > 0)
	{
		do
		{
			m++;
			text++;
		} while (*text && (*text != key ||
				   (IsHiBitOn(key) && *(text+1) != key2 ) ||
				   IsIdxHZ(markidx(m), ptext) < 0) );
	}
	if (!*text)
	{
		return MARK_UNSET;
	}
	return m;
}

/* move backward within this line to previous occurrence of key */
MARK	m_Fch(m, cnt, key)
	MARK	m;	/* where to search from */
	long	cnt;
	char	key;	/* what to search for */
{
	register char	*text;
	int	key2;	/* 2nd byte if HANZI */

	DEFAULT(1);

	prevfwdfn = m_Fch;
	prevrevfn = m_fch;
	prev_key = key;
	if (samekey)
		key2 = prev_key2;
	else if (IsHiBitOn(key))
		prev_key2 = key2 = getkey(0);
	samekey = 0;

	pfetch(markline(m));
	text = ptext + markidx(m);
	while (cnt-- > 0)
	{
		do
		{
			m--;
			text--;
		} while (text >= ptext && (*text != key ||
				   (IsHiBitOn(key) && *(text+1) != key2 ) ||
				   IsIdxHZ(markidx(m), ptext) < 0) );
	}
	if (text < ptext)
	{
		return MARK_UNSET;
	}
	return m;
}

/* move forward within this line almost to next occurrence of key */
MARK	m_tch(m, cnt, key)
	MARK	m;	/* where to search from */
	long	cnt;
	char	key;	/* what to search for */
{
	/* skip the adjacent char */
	pfetch(markline(m));
	if (plen <= markidx(m))
	{
		return MARK_UNSET;
	}
	if (IsHiBitOnMark(m))
		m += 2;
	else
		m++;

	m = m_fch(m, cnt, key);
	if (m == MARK_UNSET)
	{
		return MARK_UNSET;
	}

	prevfwdfn = m_tch;
	prevrevfn = m_Tch;

	if (IsIdxHZ( markidx(m - 1), ptext ) < 0)
		return m - 2;
	else
		return m - 1;
}

/* move backward within this line almost to previous occurrence of key */
MARK	m_Tch(m, cnt, key)
	MARK	m;	/* where to search from */
	long	cnt;
	char	key;	/* what to search for */
{
	/* skip the adjacent char */
	if (markidx(m) == 0)
	{
		return MARK_UNSET;
	}
	m--;

	m = m_Fch(m, cnt, key);
	if (m == MARK_UNSET)
	{
		return MARK_UNSET;
	}

	prevfwdfn = m_Tch;
	prevrevfn = m_tch;

	if (IsIdxHZ( markidx(m + 1), ptext ) < 0)
		return m + 2;
	else
		return m + 1;
}
#endif
