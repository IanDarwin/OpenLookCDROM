/*
 *
 * input error checking, fexists()
 *
 * 	$Header: io.c,v 1.3 89/08/26 10:03:43 pturner Locked $
 *
 */

#include <stdio.h>
#include "defines.h"

static char readbuf[80];

int ibounds(x, lower, upper, name)
    int x, lower, upper;
    char *name;
{
    int test;

    test = ((x >= lower) && (x <= upper));
    if (!test) {
	sprintf(readbuf, " in %s : parameter must be in (%d , %d)", name, lower, upper);
	errwin(readbuf);
    }
    return (test);
}

int fbounds(x, lower, upper, name)
    double x, lower, upper;
    char *name;
{
    int test;

    test = ((x >= lower) && (x <= upper));
    if (!test) {
	sprintf(readbuf, " in %s : parameter must be in (%lf , %lf)", name, lower, upper);
	errwin(readbuf);
    }
    return (test);
}

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

int fexists(to)
    char *to;
{
    int fold;
    char tbuf[128];
    struct stat stto;

    fold = open(to, 0);
    if (stat(to, &stto) >= 0) {
	sprintf(tbuf, "Overwrite %s?", to);
	if (!yesno(tbuf,"","YES","NO")) {
	    close(fold);
	    return (1);
	}
	close(fold);
	return (0);
    }
    close(fold);
    return (0);
}
