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
static	char RCSid[] = "@(#)$Header: slave.c,v 2.1 88/10/04 04:22:54 eric Release $";
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

extern void exit();
extern unsigned sleep();
extern void perror();

#ifndef HAVE_VFORK
#define	vfork()	fork()
#endif

int	Mfd, Sfd;
/* XXX - there should be a library routine to get a pty */
char	ptcname[] = "/dev/ptyXX";
char	ptsname[] = "/dev/ttyXX";
int	pgrp;
extern	int errno;
extern	int BackGround;

FILE       *
spawn_slave(name, args)
    char *name, **args;
{
    FILE *Master;
    int i, pid, uid, gid;
    unsigned delay = 2 ;
#define	MAXDELAY	120
    char *gp, *tp;

    GetTTYDefaults();
#ifdef SYSVREF
/* Micom-Interlan software installs some non-streams based pty's on 'p' */
#ifdef INTERLANTCP
    for (gp = "qrst"; *gp; gp++)
#else
    for (gp = "pqrst"; *gp; gp++)
#endif
#else
    for (gp = "pqrst"; *gp; gp++)
#endif
	for (tp = "0123456789abcdef"; *tp; tp++) {
	    ptcname[sizeof ("/dev/pty")-1] = *gp;
	    ptcname[sizeof ("/dev/ptyX")-1] = *tp;
	    if ((Mfd = open(ptcname, 2)) < 0)
		continue;
	    ptsname[sizeof ("/dev/pty")-1] = *gp;
	    ptsname[sizeof ("/dev/ptyX")-1] = *tp;
	    if ((Sfd = open(ptsname, 2)) >= 0)
		goto done;
	    close(Mfd);
    }
done:
    if (Mfd < 0 || Sfd < 0)
	return (NULL);
    uid = getuid(), gid = getgid();
    (void) chown(ptsname, uid, gid);		/* set tty ownership */
    (void) chmod(ptsname, 0622);		/* set tty protection */
    /*
     * Establish tty state.
     */
    SetTTYState(Sfd);
    /*
     * If we're to operate in the background, fork
     * to return control to the shell and disassociate
     * ourselves from the tty.  The caller closes
     * file descriptors so that, for example, rsh
     * will not be kept around.
     */
    if (BackGround) {
	if (fork())
	    exit(0);
	(void) setpgrp(0, getpid());
	DisAssociateTTY();
    } else
	AssociateTTY();
    Master = fdopen(Mfd, "r+");
    while ((pid = vfork()) < 0 && errno == EAGAIN) {
	sleep(delay);
	if ((delay <<= 1) > MAXDELAY) {
	    close (Mfd); close (Sfd);
	    return (NULL);
	}
    }
    if (pid == 0) {
	/*
	 * Setup controlling tty.
	 */
	for (i = 0; i < 3; i++)
	    (void) dup2(Sfd, i);
	addut(uid, ptsname);
	for (i = getdtablesize(); i > 2; i--)
	    (void) close(i);
	SetupControllingTTY(ptsname);
	(void) setuid(uid), (void) setgid(gid);
	execvp(name, args);
	perror(name);
	exit(errno);
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
