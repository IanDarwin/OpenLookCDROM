/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/hp700/RCS/getconsole.c,v 1.1 1993/06/24 17:10:06 gk5g Exp $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/hp700/RCS/getconsole.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/hp700/RCS/getconsole.c,v 1.1 1993/06/24 17:10:06 gk5g Exp $";
#endif /* lint */

/* 
 ***************************************************************
 * Routines for monitoring messages on console service
 * and (on some machines) /dev/console.
 * Some routines swiped from ttyscript.
 ****************************************************************
 */

#include <stdio.h>
#include <errno.h>
#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h */
#include <sys/ioctl.h>
#include <termios.h>

extern char *sys_errlist[];

main()
{
    int SubChannel, tempfd;
    char ptyname[64];
    int ON = 1;
    char buf[1024];
    FILE *SubChannelFile;
    
    if (! GetPtyandName(&SubChannel, &tempfd, ptyname, sizeof(ptyname)))  {
        printf("getconsole: Incomplete error monitoring: Can't open pty %s %s\n", ptyname, sys_errlist[errno]);
	exit(1);
    }
    if (ioctl (tempfd, TIOCCONS, (char *) &ON) < 0) {
	printf("getconsole: Incomplete error monitoring: ioctl (TIOCCONS) failed (%s)\n", sys_errlist[errno]);
	exit(1);
    }
    SubChannelFile = fdopen(SubChannel, "r");

    while(fgets(buf, sizeof(buf), SubChannelFile)) {
	fputs(buf, stdout);
	fflush(stdout);
    }
    if (feof(SubChannelFile)) {
	printf("getconsole: EOF reading console output\n");
    }
    else {
	printf("getconsole: error reading console output: %s\n",
	       sys_errlist[errno]);
    }
    exit(1);
}

    
