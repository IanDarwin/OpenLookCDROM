#ifndef lint
static char *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/misc.c,v 1.1 93/01/06 03:27:37 gounares Exp Locker: gounares $";
#endif

/*
 * $Log:	misc.c,v $
 * Revision 1.1  93/01/06  03:27:37  gounares
 * Initial revision
 * 
 */

/*
 * misc.c
 * 
 * just useful stuff that didn't seem to belong anywhere else
 * 
 * written by Alex Gounares 9/22/92
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "misc.h"
#include "alloc.h"

/*
 * slit_name -- given a full pathname, split it into the file name and the
 * directory name
 */
void
split_name(szFull, szDir, szFile)
    char           *szFull,
                   *szDir,
                   *szFile;
{
	register char  *temp;

	if ((temp = rindex(szFull, '/')) == NULL) {
		strcpy(szFile, szFull);
		strcpy(szDir, "./");
	} else {
		strcpy(szFile, temp + 1);
		*(temp) = '\0';
		strcpy(szDir, szFull);
		*(temp) = '/';
	}
}

/*
 * szJoin -- join a directory name and a file name into one.  This function
 * allocates memory
 */
char           *
szJoin(szDir, szFile)
{
	int             size;
	char           *szBuf;

	size = strlen(szDir) + strlen(szFile) + 2;

	szBuf = (char *) amalloc(size);
	sprintf(szBuf, "%s/%s", szDir, szFile);
	return szBuf;
}
