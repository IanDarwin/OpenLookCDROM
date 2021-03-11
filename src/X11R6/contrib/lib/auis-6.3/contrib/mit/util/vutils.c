/* $Author: rr2b $ */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/util/RCS/vutils.c,v 1.5 1992/12/15 21:50:50 rr2b R6tape $";
#endif


 

/*************************************************
 * View utility package
 * Contains stuff useful to all views.
 *************************************************/

/*************************************************
 * Copyright (C) 1990 by the Massachusetts Institute of Technology
 * Permission to use, copy, modify, distribute, and sell this
 * software and its documentation for any purpose is hereby
 * granted without fee, provided that the above copyright notice
 * appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation,
 * and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  M.I.T. makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *************************************************/


#include <andrewos.h>

#include <ctype.h>

#include <class.h>

#include <bind.ih>
#include <view.ih>
#include <im.ih>
#include <message.ih>
#include <environ.ih>
#include <envrment.ih>

#include <vutils.eh>

#define DIALOG 100
#define MESSAGE 0

/* Routines appear in this file in "bottom-up" order. */
/* This is so I don't have to deal with declaring forward */
/* references. */

/* Added friendly read-only behavior from txtvcmds.c */

#define H_D_STRING "Help Could not start. It was "

static void helpDeath (pid, self, status)
int pid;
struct view *self;
union wait *status;
{
    char buf[128],*em,*statustostr();

    strcpy(buf, H_D_STRING);
    em=statustostr(status, buf+strlen(H_D_STRING), 128 - strlen(H_D_STRING));
    if (em == NULL) {
	/* successful completion */
	message_DisplayString(self, MESSAGE, "If you don't see a help window now, something is wrong.");
    } else {
	message_DisplayString(self, MESSAGE, buf);
    }
}

static void forkhelpproc (self, key)
struct view *self;
long key;
{
    char *helpname = environ_AndrewDir("/bin/help");
    int pid, fd;

    switch (pid = osi_vfork()) {
	case 0:
	    for (fd = getdtablesize(); fd > 2; --fd) close(fd);
	    execl(helpname, helpname, im_GetProgramName(), 0);
	    printf ("Exec of %s failed.\n", helpname);
	    fflush (stdout);
	    _exit(-1);
	case -1:
	    message_DisplayString(self, DIALOG, "Could not start the help!");
	    break;
	default:
	    message_DisplayString(self, MESSAGE, "A Help window should appear shortly.");
	    im_AddZombieHandler(pid, helpDeath, (long)self);
	    break;
    }
    return;
}

boolean vutils__InitializeClass(classID)
struct classheader *classID;
{
    static struct bind_Description compat_fns[] = {
	{"vutils-fork-help", NULL, 0, NULL, 0, 0, forkhelpproc, "Call Andrew help in a new process.", "vutils"},

        {NULL},
    };
    struct classinfo *viewClassinfo;

    viewClassinfo = class_Load("view");
    if (viewClassinfo != NULL) {
        bind_BindList(compat_fns, NULL, NULL, viewClassinfo);
        return TRUE;
    }
    else
        return FALSE;
}
