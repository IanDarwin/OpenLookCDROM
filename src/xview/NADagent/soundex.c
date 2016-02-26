/*
**	SOUNDEX CODING
**
**	Rules:
**	1.	Retain the first letter; ignore non-alphabetic characters.
**	2.	Replace second and subsequent characters by a group code.
**		Group	Letters
**		1		BFPV
**		2		CGJKSXZ
**		3		DT
**		4		L
**		5		MN
**		6		R
**	3.	Do not repeat digits
**	4.	Truncate or ser-pad to 4-character result.
**
**	Originally formatted with tabstops set at 4 spaces -- you were warned!
**
**	Code by: Jonathan Leffler (john@sphinx.co.uk)
**	This code is shareware -- I wrote it; you can have it for free
**	if you supply it to anyone else who wants it for free.
**
**	BUGS: Assumes ASCII
*/

#include <ctype.h>
static char	lookup[] = {
	'0',	/* A */
	'1',	/* B */
	'2',	/* C */
	'3',	/* D */
	'0',	/* E */
	'1',	/* F */
	'2',	/* G */
	'0',	/* H */
	'0',	/* I */
	'2',	/* J */
	'2',	/* K */
	'4',	/* L */
	'5',	/* M */
	'5',	/* N */
	'0',	/* O */
	'1',	/* P */
	'0',	/* Q */
	'6',	/* R */
	'2',	/* S */
	'3',	/* T */
	'0',	/* U */
	'1',	/* V */
	'0',	/* W */
	'2',	/* X */
	'0',	/* Y */
	'2',	/* Z */
};

/*
**	Soundex for arbitrary number of characters of information
*/
char	*nsoundex(str, n)
char	*str;	/* In: String to be converted */
int		 n;		/* In: Number of characters in result string */
{
	static	char	buff[10];
	register char	*s;
	register char	*t;
	char	c;
	char	l;

	if (n <= 0)
		n = 4;	/* Default */
	if (n > sizeof(buff) - 1)
		n = sizeof(buff) - 1;
	t = &buff[0];

	for (s = str; ((c = *s) != '\0') && t < &buff[n]; s++)
	{
		if (!isascii(c))
			continue;
		if (!isalpha(c))
			continue;
		c = toupper(c);
		if (t == &buff[0])
		{
			l = *t++ = c;
			continue;
		}
		c = lookup[c-'A'];
		if (c != '0' && c != l)
			l = *t++ = c;
	}
	while (t < &buff[n])
		*t++ = '0';
	*t = '\0';
	return(&buff[0]);
}

/* Normal external interface */
char	*soundex(str)
char	*str;
{
	return(nsoundex(str, 4));
}

/*
**	Alternative interface:
**	void	soundex(given, gets)
**	char	*given;
**	char	*gets;
**	{
**		strcpy(gets, nsoundex(given, 4));
**	}
*/


#ifdef TEST
#include <stdio.h>
main()
{
	char	buff[30];

	while (fgets(buff, sizeof(buff), stdin) != (char *)0)
		printf("Given: %s Soundex produces %s\n", buff, soundex(buff));
}
#endif
