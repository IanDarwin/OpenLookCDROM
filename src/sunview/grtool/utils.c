/*
	utils.c - misc utilities

	$Header: utils.c,v 1.1 89/07/19 22:41:04 pturner Locked $
*/

fswap(x, y)
    double *x, *y;
{
    double tmp;

    tmp = (*x);
    *x = (*y);
    *y = tmp;
}

iswap(x, y)
    int *x, *y;
{
    int tmp;

    tmp = (*x);
    *x = (*y);
    *y = tmp;
}

lowtoupper(s)
    char *s;
{
    int i;

    for (i = 0; i < strlen(s); i++) {
	if (s[i] >= 'a' && s[i] <= 'z')
	    s[i] = s[i] - 32;
    }
}

delchar(s, c)
    char *s, c;
{
    int i, j, len;

    j = 0;
    len = strlen(s);
    for (i = 0; i < len; i++) {
	if (s[i] != c) {
	    s[j++] = s[i];
	}
    }
    s[j] = 0;
}

convertchar(s, cf, ct)
    char *s, cf, ct;
{
    while (*s++)
	if (*s == cf)
	    *s = ct;
}

int ilog2(n)
    int n;
{
    int idiv2, imod2, i;

    i = 0;
    idiv2 = n;
    if (idiv2 <= 3)
	return (-1);
    while (idiv2 > 0) {
	imod2 = idiv2 % 2;
	idiv2 = idiv2 / 2;
	if ((imod2 == 1) && (idiv2 > 1))
	    return (-1);
	i++;
    }
    return (--i);
}
