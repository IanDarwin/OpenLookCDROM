/* asetup.c -- andrew setup
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
 * Start Date: 9/19/91
 */

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <util.h>
#include <sys/param.h>
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#define MAXHOSTS 16

static char hosts[MAXHOSTNAMELEN * MAXHOSTS];
static char official[MAXPATHLEN] = "/afs/andrew.cmu.edu/usr0/bb/off/.MESSAGES";
static char local[MAXPATHLEN] = "/afs/andrew.cmu.edu/usr0/bb/.MESSAGES";
static char external[MAXPATHLEN] = "/afs/andrew.cmu.edu/usr0/netbb/.MESSAGES";
static char *rootnames[] = {
    "OfficialBboardRoot",
    "LocalBboardRoot",
    "ExternalBboardRoot",
    (char *) NULL
};
char *treeroots[] = {
    official,
    local,
    external,
    (char *) NULL
};
int mu_numhosts = 0;
char *mu_hosts[MAXHOSTS];

/* read /etc/AndrewSetup to find treeroots
 */
void andrewsetup()
{
    static int setup = 0;
    int i;
    char *str;

    /* exit if already setup */
    if (setup) return;
    setup = 1;

    for (i = 0; rootnames[i] !=  (char *) NULL; ++i) {
	str = GetConfiguration(rootnames[i]);
	if (str)
	    strcpy(treeroots[i], str);
    }

    for (i = 0; treeroots[i] != (char *) NULL; ++i) {
	(void) strcat(treeroots[i], "/");
    }
}
