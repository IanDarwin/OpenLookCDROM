/*
 *	This routine rolls ndie nside-sided dice.
 *
 * @(#)roll.c	1.1 (Berkeley) 4/1/82
 *
 */

roll(ndie, nsides)
int	ndie, nsides;
{
	register long	tot;
	register unsigned	n, r;

	tot = 0;
	n = ndie;
	while (n--) {
		r = random();
		if (r < 0)
			r = -r;
		tot += r % nsides + 1;
	}
	return tot;
}
