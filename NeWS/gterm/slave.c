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
static char sccsid[] = "@(#)slave.c 9.6 88/01/19 Copyright 1985 Sun Micro";
static	char RCSid[] = "@(#)$Header: /it/grass/gterm/RCS/slave.c,v 2.7 1991/04/23 06:52:01 hugh Grass2 $";
#endif


/*
 * Copyright (c) 1985 by Sun Microsystems, Inc. 
 */

/*-
	slave.c

	slave.c, Tue Apr  1 09:17:08 1986

		David Rosenthal,
		Sun Microsystems
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifdef REF
#include <ref/config.h>
#endif
#include <signal.h>

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

/*#define DEBUG*/
#ifdef DEBUG
#define err0(A) fprintf(fdd,A);fflush(fdd)
#define err1(A,B) fprintf(fdd,A,B);fflush(fdd)
#define err2(A,B,C) fprintf(fdd,A,B,C);fflush(fdd)
#define err3(A,B,C,D) fprintf(fdd,A,B,C,D);fflush(fdd)
#else
#define err0(A)
#define err1(A,B)
#define err2(A,B,C)
#define err3(A,B,C,D)
#endif

extern void exit();
extern unsigned sleep();
extern void perror();

#ifndef HAVE_VFORK
#define	vfork()	fork()
#endif

#ifdef SYSVREF
/* Micom-Interlan software installs some non-streams based pty's on 'p' */
#ifdef INTERLANTCP
#define PTYNAME "qrst"
#else
#define PTYNAME "pqrst"
#endif
#else
#define PTYNAME "pqrst"
#endif

int	Mfd, /* the /dev/ptyX, this is what gterm reads from */
        Sfd; /* the /dev/ttyX, this is what the spawned procces sees */
/* XXX - there should be a library routine to get a pty */
char	ptcname[] = "/dev/ptyXX";
char	ptsname[] = "/dev/ttyXX";
int	pgrp;
extern	int errno;

FILE       *
spawn_slave(name, args)
    char *name, **args;
{
    FILE *Master;	/* What gterm reads from to display on screen */
    int i, pid, uid, gid, retval;
    extern int BackGround;
    unsigned delay = 2 ;
#define	MAXDELAY	120
    char *gp, *tp;

#ifdef DEBUG
	FILE *fdd;

	fdd = fopen("/dev/console", "r+");
#endif /* DEBUG */

    GetTTYDefaults();
    /* Search through the ptys looking for an open one */
    for (gp = PTYNAME; *gp; gp++)
	for (tp = "0123456789abcdef"; *tp; tp++) {
	    ptcname[sizeof ("/dev/pty")-1] = *gp;
	    ptcname[sizeof ("/dev/ptyX")-1] = *tp;
	    if ((Mfd = open(ptcname, 2)) < 0)
		continue;
	    ptsname[sizeof ("/dev/tty")-1] = *gp;
	    ptsname[sizeof ("/dev/ttyX")-1] = *tp;
	    if ((Sfd = open(ptsname, 2)) >= 0)
		goto done;
	    close(Mfd);
    }
done:
    if (Mfd < 0 || Sfd < 0)	return (NULL);
    uid = getuid(), gid = getgid();
    (void) chown(ptsname, uid, gid);		/* set tty ownership */
    (void) chmod(ptsname, 0622);		/* set tty protection */
    /*
     * Establish tty state.
     */
    SetTTYState(Sfd);
    signal(SIGWINCH, SIG_IGN);
    /*
     * If we're to operate in the background, fork
     * to return control to the shell and disassociate
     * ourselves from the tty.  The caller closes
     * file descriptors so that, for example, rsh
     * will not be kept around.
     */
    if (BackGround) {
	err0("\n\nPreFork\n");
        retval = fork();
	if (retval > 0) {
		err1("gterm: Parent exiting, child=%d\n", retval);
		exit(0);
        } else if (retval == -1) { 
		fprintf(stderr, "gterm: backgrouding fork failed.\n\t");
		perror("system error");
	}
	/* Else fork==0 and we are the child process! */
	(void) setpgrp(0, getpid());
#ifdef HAVE_JCSETPGRP
	if (jcsetpgrp(getpid()) == -1) {
		perror("gterm: failed setpgrp()");
	}
#endif
	DisAssociateTTY();
    } else
	AssociateTTY();
    err0("TTYStuff\n");
    Master = fdopen(Mfd, "r+");
    err2("Master = 0x%x, errno = %d\n", Master, errno);
    while ((pid = vfork()) < 0 && errno == EAGAIN) {
	sleep(delay);
	if ((delay <<= 1) > MAXDELAY) {
	    close (Mfd); close (Sfd);
	    err0("Return NULL\n");
	    err3("delay %d, MAXDELAY %d, errno %d\n", delay, MAXDELAY, errno);
	    return (NULL);
	}
    }

    err1("Post Forks, pid == %d\n", pid);
    if (pid == 0) {
	/*
	 * Setup controlling tty. Sfd == stdin, stdout & stderr
	 */
	for (i = 0; i < 3; i++)
	    if (dup2(Sfd, i) == -1) {
	      err1("bad dup2 on %d\n\t", i);
	      perror("gterm: Standard I/O dup2() failure");
            }
	if (Sfd > 2) (void)close(Sfd);
	DisAssociateTTY();	/* Fixes -fg not working under SunOS 4.x */
	addut(uid, ptsname);
	for (i = getdtablesize(); i > 2; i--)
	    (void) close(i);
	SetupControllingTTY(ptsname);
	(void) setuid(uid), (void) setgid(gid);
        /* restore various signals from SIG_IGN to their defaults */
        signal (SIGINT, SIG_DFL);
        signal (SIGQUIT, SIG_DFL);
        signal (SIGTERM, SIG_DFL);
        signal(SIGWINCH, SIG_DFL);
	execvp(name, args);
	fprintf(stderr, "gterm: spawning failure for:\n\t");
	perror(name);
	sleep(20);	/* Let error message reach user */
	_exit(errno);
    }
    pgrp = pid;
    close(Sfd);
    return (Master);
}

CleanupPty()
{

    rmut();
    (void) chown(ptsname, 0, 0);		/* revoke ownership */
    (void) chmod(ptsname, 0666);		/* revoke protection */
}
