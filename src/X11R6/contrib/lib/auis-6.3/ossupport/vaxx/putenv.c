/*
 * putenv.c
 *
 * Interface to BSD_4.3 setenv routine that acts like
 * POSUX putenv.
 * Based on AMIGA Putenv from metamail.
 */

#include <stdio.h>
char *index();

int
putenv(def)
char *def;
{
    char *cp;
    char nameBuf[1024];

    if ((cp = index(def, '=')) == NULL || def == cp) {
        return (1);
    }

    strncpy(nameBuf, def, cp - def);
    nameBuf[(cp - def)] = 0;
    cp++;               /* Now points to value part of environment string. */

    if(setenv(nameBuf, cp, 1) == 0)
	return (0);
    else
	return (1);
}
