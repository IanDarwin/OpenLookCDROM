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

#ifdef REF
#include <ref/config.h>
#endif

#ifndef lint
static  char sccsid[] = "@(#)bsd.c 9.5 88/01/19 SMI";
static	char RCSid[] = "@(#)$Header: /it/grass/gterm/RCS/bsd.c,v 2.5 1991/04/23 06:53:03 hugh Grass2 $";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#ifdef HAVE_BSD_TTYS
/*
 * BSD Support Routines.
 */
#include <sys/types.h>
#include <sgtty.h>
#include <sys/file.h>
#include <signal.h>
#include <stdio.h>

extern void perror();
#ifndef bzero
extern void bzero();
#endif
extern char *strncpy();
extern long time();
extern long lseek();

/* Tty mode statics */
static	struct tchars tc;
static	struct ltchars ltc;
static	struct sgttyb sg;
static	int lm;			/* localmodes */
static	int ld;			/* ldisc */

/* Default settings for tty mode stuff */
static	struct tchars tc_df =
    { CINTR, CQUIT, CSTART, CSTOP, CEOF, CBRK };
static	struct ltchars ltc_df =
    { CSUSP, CDSUSP, CRPRNT, CFLUSH, CWERASE, CLNEXT };
static	struct sgttyb sg_df =
    { B9600, B9600, CERASE, CKILL, ANYP|ECHO|CRMOD };
static	int lm_df = LCRTBS|LCRTERA|LCRTKIL|LCTLECH;
static	int ld_df = NTTYDISC;

extern	int console;
extern	int CharsPerLine;
extern	int LinesPerScreen;

static void get_tty_util();

GetTTYDefaults()
{
    int fd;
    extern int BackGround;

    /* If we are in the background then dont even bother opeing /dev/tty */
    if (BackGround) {
	    if(we_getptyparms(&ld, &lm, &sg, &tc, &ltc) == -1) {
		    get_tty_util(-1);
	    }
    } else {
	    /* Get settings of controlling terminal */
	    fd = open("/dev/tty", O_RDWR);
	    get_tty_util(fd);
	    if (fd >= 0)
	      close(fd);
	    /*
	     * Modify setting with data passed thru the environment.
	     * Read environment variable WINDOW_TTYPARMS
	     */
	    if(we_getptyparms(&ld, &lm, &sg, &tc, &ltc) == -1) {
		/*
		 * Try to get the tty parameters from stderr (2).
		 * Using stdin (0) fails when being started in the background
		 *   because csh redirects stdin from the tty to /dev/null.
		 */
		    fd = 2;
		    if (!isatty(fd)) {
			    fd = open("/dev/console", 2);
		    }
		    get_tty_util(fd);
		    if (fd != 2)
		      (void) close(fd);
	    }
    }
}

static void
get_tty_util(fd)
    int fd;
{
    if (fd > 0) { /* Most often fd == 2 */
	if(ioctl(fd, TIOCGETP, &sg) == 0 || (sg.sg_flags&ECHO) == 0)
	    sg = sg_df;
	(void) ioctl(fd, TIOCGETC, &tc);
	if (tc.t_quitc == 0)
	    tc = tc_df;
	(void) ioctl(fd, TIOCGETD, &ld);
	if (ld == 0)
	    ld = ld_df;
	(void) ioctl(fd, TIOCLGET, &lm);
	if (lm == 0)
	    lm = lm_df;
	(void) ioctl(fd, TIOCGLTC, &ltc);
	if (ltc.t_suspc == 0)
	    ltc = ltc_df;
#ifndef sun	/* with this bash & csh to quit right after execvp on SunOS 4.1*/
	(void) ioctl(fd, TIOCNOTTY, 0);
#endif
    } else {
	/* Last resort...use some default values */
	tc = tc_df;
	ld = ld_df;
	lm = lm_df;
	ltc = ltc_df;
	sg = sg_df;
    }
}

SetTTYState(fd)
    int fd;
{
    (void) ioctl(fd, TIOCHPCL, 0);
    (void) ioctl(fd, TIOCSETD, &ld);
    if ((sg.sg_flags & ECHO) == 0)
	sg = sg_df;
    (void) ioctl(fd, TIOCSETP, &sg);
    (void) ioctl(fd, TIOCSETC, &tc);
    (void) ioctl(fd, TIOCSLTC, &ltc);
    (void) ioctl(fd, TIOCLSET, &lm);
#ifdef TIOCSWINSZ
    { struct winsize ws;
      ws.ws_row = LinesPerScreen; ws.ws_col = CharsPerLine;
      (void) ioctl(fd, TIOCSWINSZ, &ws);
    }
#endif
#ifdef TIOCSSIZE
    { struct ttysize ts;
      ts.ts_lines = LinesPerScreen; ts.ts_cols = CharsPerLine;
      (void) ioctl(fd, TIOCSSIZE, &ts);
    }
#endif
    if (console) {
#ifdef TIOCCONS
	if (ioctl(fd, TIOCCONS, 0) < 0)
	    perror("tcap(TIOCCONS)");
#endif
    }
}

DisAssociateTTY()
{
    int fd;

    if (( fd = open("/dev/tty", O_RDWR)) >= 0) {
	ioctl(fd, TIOCNOTTY, 0);
	close(fd);
    }
}

AssociateTTY()
{

}

/*ARGSUSED*/
SetupControllingTTY(line)
   char line[];
{
    int fd, pid;

    if ((fd = open("/dev/tty", O_RDWR)) < 0) {
	(void) setpgrp(0, 0);
	(void) close(open(line, O_RDWR));
    } else {
	close(fd);
    }
    (void) setpgrp(0, pid = getpid());
    (void) ioctl(0, TIOCSPGRP, &pid);
}

#include <utmp.h>
#include <pwd.h>

#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

static	char utmpf[] = "/etc/utmp";
static	char wtmpf[] = "/usr/adm/wtmp";
static	int tslot = -1;
static	struct utmp utmp;

addut(uid, line)
    int uid;
    char line[];
{
#ifdef UTMP
    struct passwd *pw;
    int i;

    /*
     * Record entry in /etc/utmp if possible.
     */
    tslot = ttyslot();
    pw = getpwuid(uid);
    if (tslot > 0 && pw && (i = open(utmpf, O_RDWR)) >= 0) {
	char *cp, *p, *getenv(), *index();

	bzero((char *)&utmp, sizeof (utmp));
	SCPYN(utmp.ut_line, &line[5]);
	SCPYN(utmp.ut_name, pw->pw_name);
	if ((cp = getenv("NEWSSERVER")) == NULL)
	    cp = "";
	if ((p = index(cp, ';')) && p[1] != '\0')
	    cp = p+1;
	SCPYN(utmp.ut_host, cp);
	(void) time(&utmp.ut_time);
	(void) lseek(i, (long)(tslot * sizeof (utmp)), L_SET);
	(void) write(i, (char *)&utmp, sizeof (utmp));
	(void) close(i);
	if ((i = open(wtmpf, O_WRONLY|O_APPEND)) >= 0) {
		write(i, (char *)&utmp, sizeof(utmp));
		close(i);
	}
    }
#endif
}

rmut()
{
#ifdef UTMP
    int fd;
    struct utmp wtmp;

    if (tslot >= 0 && (fd = open(utmpf, O_WRONLY)) >= 0) {
	wtmp = utmp;
	bzero((char *)&utmp, sizeof (utmp));
	(void) lseek(fd, (long)(tslot * sizeof (utmp)), L_SET);
	(void) write(fd, (char *)&utmp, sizeof (utmp));
	(void) close(fd);
	fd = open(wtmpf, O_WRONLY|O_APPEND);
	if (fd >= 0) {
	    SCPYN(wtmp.ut_name, "");
	    SCPYN(wtmp.ut_host, "");
	    time(&wtmp.ut_time);
	    write(fd, (char *)&wtmp, sizeof(wtmp));
	    close(fd);
	}
    }
#endif
}
#endif /* !HAVE_BSD_TTYS */

#ifdef sun
#define	WE_TTYPARMS	"WINDOW_TTYPARMS"

/*
 * Get tty settings from environment.
 */
int
we_getptyparms(ldisc, localmodes, mode, tchars, ltchars)
	int *ldisc, *localmodes;
	struct sgttyb *mode;
	struct tchars *tchars;
	struct ltchars *ltchars;
{
	char *str, *getenv();
	short temps[16];	/* Needed for sscanf as there is no %hhd */

	if((str = getenv(WE_TTYPARMS)) == NULL) {
		return (-1);
	} else {
		if (sscanf(str,
  "%ld,%ld,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd,%hd",
		    ldisc, localmodes, &temps[0], &temps[1], &temps[2],
		    &temps[3], &mode->sg_flags, &temps[4], &temps[5], &temps[6],
		    &temps[7], &temps[8], &temps[9], &temps[10], &temps[11],
		    &temps[12], &temps[13], &temps[14], &temps[15])
		     != 19)
			return (-1);
		    mode->sg_ispeed = temps[0];
		    mode->sg_ospeed = temps[1];
		    mode->sg_erase = temps[2];
		    mode->sg_kill = temps[3];
		    tchars->t_intrc = temps[4];
		    tchars->t_quitc = temps[5];
		    tchars->t_startc = temps[6];
		    tchars->t_stopc = temps[7];
		    tchars->t_eofc = temps[8];
		    tchars->t_brkc = temps[9];
		    ltchars->t_suspc = temps[10];
		    ltchars->t_dsuspc = temps[11];
		    ltchars->t_rprntc = temps[12];
		    ltchars->t_flushc = temps[13];
		    ltchars->t_werasc = temps[14];
		    ltchars->t_lnextc = temps[15];
		/*
		 * Always clear
		 */
		(void)unsetenv(WE_TTYPARMS);
		return (0);
	}
}
#endif	/* of !HAVE_BST_TTYS */

