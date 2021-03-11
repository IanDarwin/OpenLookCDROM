/* update.c -- the update process
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

#include <andrewos.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/param.h>
#include "mufbuf.h"
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#ifndef MAX
#define MAX(a,b) (a > b ? a : b)
#endif

static char logfile[] = "/tmp/mupdate.log";
static char oldfile[] = "/tmp/mupdate.old";

#define MS_MASTERUPDATE ".MS.Master/Update"

/* from asetup.c: */
extern char *treeroots[];
extern void andrewsetup();

#define SLEEPTIME (5 * 60)

struct muflist {
    int t;
    mufbuf mbuf;
} master[3];

/* print an error message with time to the logfile
 */
static void update_msg(msg, num)
    char *msg;
    int num;
{
    struct timeval tv;

    gettimeofday(&tv, 0);
    fprintf(stderr, "%s (%d) %s", msg, num, ctime(&tv.tv_sec));
}
static void update_fatal(msg, num)
    char *msg;
    int num;
{
    update_msg(msg, num);
    exit(1);
}

/* update a mufbuf and send updates to stdout
 */
void update()
{
    int i, fd, changed;
    struct stat stbuf;
    mufbuf mbuf;
    FILE *in;
    char *dst;
    char buf[MAX(MAXMUFSTR,MAXPATHLEN)];
    
    /* init logfile */
    (void) rename(logfile, oldfile);
    if ((fd = open(logfile, O_CREAT|O_WRONLY|O_TRUNC, 0644)) < 0) {
	perror(logfile);
	exit(1);
    }
    (void) dup2(fd, 2);
    (void) close(fd);

    /* init mufbufs */
    for (i = 0; treeroots[i]; ++i) {
	if (mufbuf_init(&master[i].mbuf) < 0) {
	    update_fatal("mufbuf_init", 0);
	}
    }
    if (mufbuf_init(&mbuf) < 0) update_fatal("mufbuf_init", 0);

    /* read setup file */
    andrewsetup();

    /* mainloop */
    update_msg("started", 0);
    for (;;) {
	/* scan master update files */
	changed = 0;
	for (i = 0; treeroots[i]; ++i) {
	    (void) strcpy(buf, treeroots[i]);
	    (void) strcat(buf, MS_MASTERUPDATE);
	    if (stat(buf, &stbuf) >= 0
		&& master[i].t != stbuf.st_mtime
		&& (in = fopen(buf, "r")) != (FILE *) NULL) {
		mufbuf_clear(&master[i].mbuf);
		while (fgets(buf, sizeof (buf), in) != (char *) NULL) {
		    dst = mufbuf_grow(&master[i].mbuf);
		    if (dst == (char *) NULL) {
			update_fatal("mufbuf_grow", 0);
		    }
		    if (mufstr_encode(dst, buf)) {
			changed = 1;
			if (mufbuf_add(&master[i].mbuf, dst) < 0) {
			    update_fatal("mufbuf_add", 0);
			}
		    }
		}
		(void) fclose(in);
		master[i].t = stbuf.st_mtime;
	    }
	}

	/* update client */
	if (changed) {
	    mufbuf_clear(&mbuf);
	    for (i = 0; treeroots[i]; ++i) {
		if (mufbuf_copy(&mbuf, &master[i].mbuf, 1) < 0) {
		    update_fatal("mufbuf_copy", master[i].mbuf.entries);
		}
	    }
	    mufbuf_qsort(&mbuf);
	    putchar((mbuf.sused >> 24) & 0xff);
	    putchar((mbuf.sused >> 16) & 0xff);
	    putchar((mbuf.sused >> 8) & 0xff);
	    putchar(mbuf.sused & 0xff);
	    for (i = 0; i < mbuf.entries; ++i) {
		puts(mbuf.table[i]);
	    }
	    if (fflush(stdout) == EOF) {
		update_fatal("pipe broken", 0);
	    }
	}
	sleep(SLEEPTIME);
    }
}
