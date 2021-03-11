/* -*-C-*-
*******************************************************************************
*
* File:         pty_svr4.c
* RCS:          $Header: /users/npm/src/winterp/src-server/expect/RCS/pty_svr4.c,v 2.4 1994/06/06 15:43:06 npm Exp $
* Description:  pty_svr4.c - routines to allocate ptys - SYSVR4
* Author:       Don Libes, NIST
* Created:      6/22/91
* Modified:     Sun Jun  5 03:31:29 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* libexpect, by Don Libes, National Institute of Standards and Technology
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of National Institute of Standards and
* Technology, Don Libes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. National Institute of Standards and Technology,
* Don Libes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, DON LIBES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE NATIONAL
* INSTITUTE OF STANDARDS AND TECHNOLOGY, DON LIBES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/expect/RCS/pty_svr4.c,v 2.4 1994/06/06 15:43:06 npm Exp $";

/* pty_svr4.c - routines to allocate ptys - SYSVR4

Written by: Don Libes, NIST, 6/22/91

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

Modified by Ed Klein for SYSVR4
Modified by Niels Mayer for Solaris 2.3 (5/3/94)
*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/fcntl.h>
#include <sys/termio.h>
#include "translate.h"
#include <sys/sysmacros.h>
#include <sys/stat.h>
#include <stdio.h>

#ifdef sun			/* NPM: added to work w/ Solaris 2.3 -- define 'I_PUSH' */
#include <sys/stropts.h>
#endif /* defined(sun) */

static int	realuid;
static int	realgid;
void debuglog();

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static char line[32];
static   struct stat sb;
static void
pty_stty(s,name)
char *s;		/* args to stty */
char *name;		/* name of pty */
{
#define MAX_ARGLIST 10240
	char buf[MAX_ARGLIST];	/* overkill is easier */

	sprintf(buf,"stty %s < %s > %s",s,name,name);
	system(buf);
}

struct	termio exp_tty_original;

#define GET_TTYTYPE	0
#define SET_TTYTYPE	1
void ttytype(request,fd,s)
int request;
int fd;
char *s;	/* stty args, used only if request == SET_TTYTYPE */
{
	static int is_a_tty;

	if (request == GET_TTYTYPE) {
		if (-1 == ioctl(fd, TCGETA, (char *)&exp_tty_original)) {
			is_a_tty = FALSE;
		} else is_a_tty = TRUE;
	} else {	/* type == SET_TTYTYPE */
		if (is_a_tty) {
			(void) ioctl(fd, TCSETA, (char *)&exp_tty_original);
		} else {
			/* if running in the background, we have no access */
			/* to a a tty to copy parameters from, so use ones */
			/* supplied by original Makefile */
			debuglog("getptyslave: (default) stty %s\n",DFLT_STTY);
			pty_stty(DFLT_STTY,line);
		}
		if (s) {
			/* give user a chance to override any terminal parms */
			debuglog("getptyslave: (user-requested) stty %s\n",s);
			pty_stty(s,line);
		}
	}
}

void
init_pty()
{
	ttytype(GET_TTYTYPE,0,(char *)0);
}

/* returns fd of master end of pseudotty */
int
getptymaster()
{
	int ptynum;
	int fd;
	char *m;

	if ((fd = open("/dev/ptmx", O_RDWR)) == -1) return(-1);
	if ((m = ptsname(fd)) == NULL || unlockpt(fd) || grantpt(fd)) {
		close(fd);
		return(-1);
	}
	strncpy(line, m, sizeof line);
	(void) ioctl(fd, TIOCFLUSH, (char *) 0);

	return(fd);
}

int
getptyslave(stty_args)
char *stty_args;
{
	int slave;
	
	extern char *sys_errlist[];
#if 0 /* NPM: COMMENTOUT */
	int errno;	/* some but not all systems require this */
#else /* NPM: FIX ... */
	extern int errno;	/* some but not all systems require this */
#endif /* NPM */

	if (0 > (slave = open(line, O_RDWR))) {
#if 0 /* NPM: COMMENTOUT */
		errorlog(stderr,"cannot open slave at %s!\n",line);
#else /* NPM: FIX */
		fprintf(stderr,"cannot open slave at %s!\n",line);
#endif /* NPM */
		return(-1);
	}

	if (ioctl(slave, I_PUSH, "ptem")) {
#if 0 /* NPM: COMMENTOUT */
		errorlog("iotcl ptem: %s\n",sys_errlist[errno]);
#else /* NPM: FIX */
		fprintf(stderr,"error in 'libexpect' -- ioctl ptem: %s\n",sys_errlist[errno]);
#endif /* NPM */
	}
	if (ioctl(slave, I_PUSH, "ldterm")) {
#if 0 /* NPM: COMMENTOUT */
		errorlog("iotcl ldterm: %s\n",sys_errlist[errno]);
#else /* NPM: FIX */
		fprintf(stderr,"error in 'libexpect' -- ioctl ldterm: %s\n",sys_errlist[errno]);
#endif /* NPM */
	}
	if (ioctl(slave, I_PUSH, "ttcompat")) {
#if 0 /* NPM: COMMENTOUT */
		errorlog("iotcl ttcompat: %s\n",sys_errlist[errno]);
#else /* NPM: FIX */
		fprintf(stderr,"error in 'libexpect' -- ioctl ttcompat: %s\n",sys_errlist[errno]);
#endif /* NPM */
	}

	/* sanity check - if slave not 0, skip rest of this and return */
	/* to what will later be detected as an error in caller */
	if (0 != slave) return(slave);

	fcntl(0,F_DUPFD,1);	/* duplicate 0 onto 1 to prepare for stty */
	ttytype(SET_TTYTYPE,slave,stty_args);
	return(slave);
}

