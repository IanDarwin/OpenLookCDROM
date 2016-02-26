/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Modifications to the original Sun Microsystems, Inc. source code
 * made by the Grasshopper Group are in the Public Domain.
 *
 * Extensions to this file by Eric Messick of the Grasshopper Group.
 *
 * Grasshopper Group
 * 212 Clayton St
 * San Francisco, CA 94117
 *
 */

#ifndef lint
static  char sccsid[] = "@(#)sys5.c 9.6 88/01/19 SMI";
static	char RCSid[] =
	"@(#)$Header: /it/grass/gterm/RCS/sys5.c,v 2.3 1991/04/23 06:52:32 hugh Grass2 $";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#ifdef	REF
#include	<ref/config.h>
#endif

/*
 * System V Support Routines.
 */
#ifdef HAVE_SYSV_TTYS
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/termio.h>
#include <signal.h>
#include <stdio.h>

static	struct termio old_termio, slave_termio, default_termio = {
    BRKINT|IGNPAR|ISTRIP|ICRNL|IXON|IXANY,
    OPOST|ONLCR,
    B9600|CS8|CREAD|HUPCL|CLOCAL,
    ISIG|ICANON|ECHO|ECHOE|ECHOK,
    0,
    03,CQUIT,010,025,CEOF,CNUL,0,0
};
static	int notty = 0;
extern	int CharsPerLine;
extern	int LinesPerScreen;

GetTTYDefaults()
{ 
    int fd;

    /*
     * Try to copy the termio settings for our controlling terminal for
     * use on the slave pty, but watch out for the case where we were invoked
     * as a background process by a shell that may have subsequently put the
     * control terminal into raw mode (e.g. ksh will do this).
     */
    slave_termio = default_termio;	/* assume the worst */
    if ((fd = open("/dev/tty", O_RDWR|O_NDELAY)) >= 0) {
	if (ioctl(fd, TCGETA, &old_termio) == 0) {
	    if (old_termio.c_line == default_termio.c_line) {
		if (old_termio.c_lflag & ICANON)
		    slave_termio = old_termio;
		else {
		    /* if not doing canonical input, we can probably still
		     * use the VINTR, VQUIT, VERASE, and VKILL parameters
		     */
		    slave_termio.c_cc[VINTR] = old_termio.c_cc[VINTR];
		    slave_termio.c_cc[VQUIT] = old_termio.c_cc[VQUIT];
		    slave_termio.c_cc[VERASE] = old_termio.c_cc[VERASE];
		    slave_termio.c_cc[VKILL] = old_termio.c_cc[VKILL];
		}
	    }
	}
	close(fd);
    }
    else
	notty = 1;
}

SetTTYState(fd)
    int fd;
{

    (void) ioctl(fd, TCSETA, &slave_termio);
    {
	char num[16];
	sprintf(num, "%d", LinesPerScreen);
	set_environment_var("LINES",num);
	sprintf(num, "%d", CharsPerLine);
	set_environment_var("COLUMNS", num);
    }
}

DisAssociateTTY()
{

}

AssociateTTY()
{

    if (notty)
	setpgrp();
}

SetupControllingTTY(line)
    char line[];
{

    (void) setpgrp();
    (void) close(open(line, O_RDWR));
}

#include <utmp.h>
#ifndef UTMP_FILE
#define UTMP_FILE "/etc/utmp"
#endif
#include <pwd.h>

static	int tslot = -1;

addut(uid, line)
    char line[];
{
#ifdef UTMP
    struct utmp utmp;
    struct passwd *pw, *getpwuid();
    int i;

#ifdef HAVE_PUTUTLINE
	extern struct utmp *getutline();

	memset(&utmp, 0, sizeof (utmp));
	memcpy(utmp.ut_line, &line[5], sizeof (utmp.ut_line));
	utmp.ut_id[0] = 'P' ;
	utmp.ut_id[1] = 'S' ;
	utmp.ut_id[2] = utmp.ut_line[3] ;
	utmp.ut_id[3] = utmp.ut_line[4] ;
	utmp.ut_type = USER_PROCESS;
	setutent();
	getutline(&utmp);
	pw = getpwuid(uid);
	if (pw)
		memcpy(utmp.ut_name, pw->pw_name, sizeof (utmp.ut_name));
	utmp.ut_pid = getpid() ;
	(void) time(&utmp.ut_time);
	pututline(&utmp);
	endutent();
#else
    /*
     * Record entry in /etc/utmp if possible.
     */
    tslot = ttyslot();
#if defined(hpux)
    /*
     * Must create utmp entry before ttyslot will work.
     */
    if (tslot < 0) {
	int fd = open(UTMP_FILE, O_RDWR|O_APPEND);

	if (fd > -1) {
	    memset(&utmp, 0, sizeof (utmp));
	    memcpy(utmp.ut_line, &line[5], sizeof (utmp.ut_line));
	    utmp.ut_type = USER_PROCESS;
	    write(fd, &utmp, sizeof (utmp));
	    close(fd);
	    tslot = ttyslot();		/* try again */
	} 
    }
#endif /* hpux */
    pw = getpwuid(uid);
    if (tslot > 0 && pw && (i = open(UTMP_FILE, O_RDWR)) >= 0) {
	char *cp, *p, *getenv(), *index();

	memset(&utmp, 0, sizeof (utmp));
	memcpy(utmp.ut_line, &line[5], sizeof (utmp.ut_line));
	memcpy(utmp.ut_name, pw->pw_name, sizeof (utmp.ut_name));
	memcpy(utmp.ut_id, "NeWS", sizeof (utmp.ut_id));
	utmp.ut_type = USER_PROCESS;
	(void) time(&utmp.ut_time);
	(void) lseek(i, tslot * sizeof (utmp), 0);
	(void) write(i, &utmp, sizeof (utmp));
	(void) close(i);
    }
#endif
#endif /* UTMP */
}

rmut()
{
#ifdef UTMP
#ifdef HAVE_PUTUTLINE
	extern char ptsname[];
	extern struct utmp *getutline();
	struct utmp utmp;
	struct passwd *pw, *getpwuid();

	memset(&utmp, 0, sizeof (utmp));
	memcpy(utmp.ut_line, &ptsname[5], sizeof (utmp.ut_line));
	utmp.ut_id[0] = 'P' ;
	utmp.ut_id[1] = 'S' ;
	utmp.ut_id[2] = utmp.ut_line[3] ;
	utmp.ut_id[3] = utmp.ut_line[4] ;
	utmp.ut_type = USER_PROCESS;
	setutent();
	if (getutline(&utmp)) {
		utmp.ut_type = DEAD_PROCESS;
		pw = getpwuid(getuid());
		if (pw)
			memcpy(utmp.ut_name, pw->pw_name, sizeof(utmp.ut_name));
		utmp.ut_pid = 0 ;
		(void) time(&utmp.ut_time);
		pututline(&utmp);
		}
	endutent();
#else
    int fd;

    if (tslot >= 0 && (fd = open(UTMP_FILE, O_WRONLY)) >= 0) {
	(void) lseek(fd, tslot * sizeof (utmp), 0);
	(void) read(fd, &utmp, sizeof (utmp));
	utmp.ut_type = DEAD_PROCESS;
	(void) lseek(fd, tslot * sizeof (utmp), 0);
	(void) write(fd, &utmp, sizeof (utmp));
	(void) close(fd);
    }
#endif
#endif
}

killpg(pgrp, sig)
{

    kill(-pgrp, sig);
}
#endif /* HAVE_SYSV_TTYS */
