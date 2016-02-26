#ifndef lint
static char *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/alloc.c,v 1.1 93/01/06 03:27:09 gounares Exp Locker: gounares $";
#endif

/*
 * $Log:	alloc.c,v $
 * Revision 1.1  93/01/06  03:27:09  gounares
 * Initial revision
 * 
 */

/*
 * alloc.c
 * 
 * Safe allocation functions.  In the best of worlds, these functions will
 * nicely handle out-of-memory conditions, but for now, they will just exit,
 * since if you are out of memory, you have a lot more problems that just
 * this tool messing up.
 * 
 * written by: Alex Gounares
 * 
 * 9/18/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the 
<!@ 1 COPYING 1 #This is an example of Object Linking
#when 'Run' this text will be passed to the Shell
cat COPYING 
@!>
GNU Public License Version 
<!@ 1 COPYING 1 #This is an example of Object Linking
#when 'Run' this text will be passed to the Shell
cat COPYING 
@!>
2
 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"

/*
 * acalloc -- Alex's safe calloc :)
 */
char           *
acalloc(num, size)
    int             num,
                    size;
{
	char           *mem;

	if ((mem = calloc(num, size)) == NULL) {
		fprintf(stderr,
			"Out of memory.  You're hosed :)\n");
		exit(1);
	}
	return mem;
}

/*
 * amalloc -- Alex's ultra-safe, ready-to-cut, easy-to-use malloc, as used in
 * hospitals
 */
char           *
amalloc(size)
    int             size;
{
	char           *mem;

	if ((mem = malloc(size)) == NULL) {
		fprintf(stderr,
			"Out of memory.  You're very hosed :)\nWe outta here\n");
		exit(1);
	}
	return mem;
}
