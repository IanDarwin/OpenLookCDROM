

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/rdemo/check/RCS/check.c,v 1.4 1992/12/15 22:00:08 rr2b R6tape $";
#endif
#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "../config.h"

extern int errno, atoi();
extern char *sys_errlist[], *rindex();

main(argc, argv)
int argc;
char **argv;
{
    char *hostname, dirname[1 + MAXPATHLEN], filename[1 + MAXPATHLEN], *pidstr;
    int semcount = 0, pid;
    DIR *semdir;
    struct direct *direntry;
    struct stat statbuf;
    long thetime = time(0);

    if (argc < 2)
	exit(1);
    hostname = argv[1];
    sprintf(dirname, "%s/%s", SEMAPHOREDIR, hostname);
    if (!(semdir = opendir(dirname)))
	exit(1);
    while (direntry = readdir(semdir)) {
	if (strcmp(direntry->d_name, ".") && strcmp(direntry->d_name, "..")) {
	    sprintf(filename, "%s/%s", dirname, direntry->d_name);
	    if (stat(filename, &statbuf)) {
		/* error occurred in stat, rely on kill(pid, 0) */
		if (pidstr = rindex(filename, ',')) {
		    pid = atoi(pidstr + 1);
		    if (kill(pid, 0) == 0) {
			++semcount;
		    }
		    else {
			/* It's invalid */
			(void) unlink(filename);
		    }
		}
		else {
		    /* Couldn't tell either way, assume sem is valid */
		    ++semcount;
		}
	    }
	    else {
		if ((thetime - (long) statbuf.st_ctime) >=
		    (60 * DEMOMINUTES)) {
		    (void) unlink(filename);
		    /* If the unlink didn't succeed, don't sweat it */
		}
		else {
		    if (pidstr = rindex(filename, ',')) {
			pid = atoi(pidstr + 1);
			if (kill(pid, 0) == 0) {
			    ++semcount;
			}
			else {
			    (void) unlink(filename);
			}
		    }
		    else {
			/* Here, it really *is* an OK semaphore */
			++semcount;
		    }
		}
	    }
	}
    }
    exit((semcount >= MAXUSERS) ? 1 : 0);
}
