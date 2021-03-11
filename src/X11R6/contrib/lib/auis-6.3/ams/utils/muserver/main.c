/* main.c -- main program for muserver processes
 *
 *	(C) Copyright 1991 by Carnegie Mellon University
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in 
 * supporting documentation, and that the name of CMU not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  
 * 
 * CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * Author: Chris Newman
 * Start Date: 9/18/91
 */

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/muserver/RCS/main.c,v 1.6 1993/10/26 19:50:09 gk5g Exp $";
#endif

#include <andrewos.h>
#include <sys/ioctl.h>

extern void update();
extern void server();

int pid;

/* diown the parent tty
 */
void disown_tty()
{
#ifdef TIOCNOTTY
    int fd;

    if ((fd = open("/dev/tty", O_WRONLY)) >= 0) {
	(void) ioctl(fd, TIOCNOTTY, 0);
	(void) close(fd);
    }
#endif    
}

/* start both update and server processes with pipe
 * from update to server
 */
main(argc, argv)
    int argc;
    char **argv;
{
    int fds[2];

    disown_tty();
    if (pipe(fds) < 0) {
	perror("pipe");
	exit(1);
    }
    if ((pid = fork()) < 0) {
	perror("fork");
	exit(1);
    }
    if (pid == 0) {
	(void) close(fds[1]);
	(void) dup2(fds[0], 0);
	(void) close(fds[0]);
	server();
    } else {
	(void) close(fds[0]);
	(void) dup2(fds[1], 1);
	(void) close(fds[1]);
	update();
    }
    /*NOTREACHED*/
}
