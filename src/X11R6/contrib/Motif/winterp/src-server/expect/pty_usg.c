/* -*-C-*-
*******************************************************************************
*
* File:         pty_usg.c
* RCS:          $Header: /users/npm/src/winterp/src-server/expect/RCS/pty_usg.c,v 2.4 1994/06/06 15:43:06 npm Exp $
* Description:  pty_usg.c - routines to allocate ptys - usg version
* Author:       Don Libes, NIST
* Created:      2/6/90
* Modified:     Sun Jun  5 03:45:39 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/expect/RCS/pty_usg.c,v 2.4 1994/06/06 15:43:06 npm Exp $";

/* pty_usg.c - routines to allocate ptys - usg version

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#ifdef AUX2
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif
#include "term.h"
#include "translate.h"

void debuglog();

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif
#ifdef UTS
#include <sys/vty.h>
static char	line[MAXPTYNAMELEN];
static char	sline[MAXPTYNAMELEN];
#else
static char	line[] = "/dev/ptyXX";
#endif
static char	*tty_type;		/* ptr to char [pt] denoting
					   whether it is a pty or tty */
static char	*tty_bank;		/* ptr to char [p-z] denoting
					   which bank it is */
static char	*tty_num;		/* ptr to char [0-f] denoting
					   which number it is */

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
static void
ttytype(request,fd,s)
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
	tty_type = &line[strlen("/dev/")];
	tty_bank = &line[strlen("/dev/pty")];
	tty_num  = &line[strlen("/dev/ptyp")];
	ttytype(GET_TTYTYPE,0,(char *)0);
}

#ifndef R_OK
/* 3b2 doesn't define these according to jthomas@nmsu.edu. */
#define R_OK 04
#define W_OK 02
#endif

/* returns fd of master end of pseudotty */
int
getptymaster()
{
	char *hex;
	struct stat stat_buf;
	int master;
#ifdef UTS
	master = getpty(line, sline, O_RDWR);
#else
	for (*tty_bank = 'p';; (*tty_bank)++) {
		*tty_num = '0';
		if (stat(line, &stat_buf) < 0) break;
		for (hex = "0123456789abcdef";*hex;hex++) {
			*tty_num = *hex;
			*tty_type = 'p';
			if (0 <= (master = open(line, O_RDWR))) {
#endif
				/* verify slave side is usable */
				*tty_type = 't';
				if (access(line, R_OK|W_OK) != 0) {
					(void) close(master);
#ifdef UTS				
					return(-1);
#else
					continue;
#endif
				}
				return(master);
#ifndef UTS
			}
		}
	}
#endif
	return(-1);
}

int
getptyslave(stty_args)
char *stty_args;
{
	int slave;
#ifdef UTS
	if (0 > (slave = open(sline, O_RDWR))) return(-1);
#else
	if (0 > (slave = open(line, O_RDWR))) return(-1);
#endif

	/* sanity check - if slave not 0, skip rest of this and return */
	/* to what will later be detected as an error in caller */
	if (0 != slave) return(slave);

	fcntl(0,F_DUPFD,1);	/* duplicate 0 onto 1 to prepare for stty */
	ttytype(SET_TTYTYPE,slave,stty_args);
	return(slave);
}
