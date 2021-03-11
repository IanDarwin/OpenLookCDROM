/* Copyright 1993 Carnegie Mellon University All rights reserved.
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/rdemo/rdemosh/RCS/rdemosh.c,v 1.6 1993/12/07 16:41:58 rr2b Exp $";
#endif
#include <stdio.h>
#include <andrewos.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <sys/file.h>

#include <X11/Xlib.h>
#include <xgraphic.ih>
#include <im.ih>
#include <xim.ih>
#include <frame.ih>
#include <cursor.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <pshbttn.ih>
#include <pshbttnv.ih>
#include <message.ih>

#include <rdemosh.eh>

#include "../config.h"

#ifndef ALARMTIME
#define ALARMTIME DEMOMINUTES*60
#endif

#define MAJORVERSION (3)
#define MINORVERSION (0)

static char *MyHostname = NULL;
static char *MyUniqueId = NULL;
static FILE *AuditFile = (FILE *) 0;

extern int errno;
extern char *sys_errlist[];

enum lpair_side {
    lpair_TopLeft, lpair_BottomRight
};

enum lpair_dir {
    lpair_SideBySide, lpair_AboveAndBelow
};

enum lpair_type {
    lpair_Percent, lpair_Fixed
};

static void DoTour(), DoHelp(), DoEz(), DoConsole();
static void DoMessages(), DoExit();
static void DoComments();

static struct buttoninfo {
    char *label, *text;
    void (*fn)();
    int pid;
} Buttons[] = {
    {"tour", "A tour of the Andrew Toolkit", DoTour, 0},
    {"ez", "The all-purpose ATK editor", DoEz, 0},
    {"comment", "We invite your comments on this service", DoComments, 0},
    {"help", "The ATK on-line documentation program", DoHelp, 0},
    {"console", "The ATK machine performance monitor", DoConsole, 0},
    {"messages", "The ATK mail, bboard, and news reader", DoMessages, 0},
    {"Exit", "Leave this demo", DoExit, 0},
    {NULL, NULL, (void (*) ()) 0, 0}
};

static struct lpair *lpair_AddSplit(lp, v, side, splitdir, splittype,
				    splitval, movable)
struct lpair *lp;
struct view *v;
enum lpair_side side;
enum lpair_dir splitdir;
enum lpair_type splittype;
long splitval;
boolean movable;
{
    struct lpair *new = lpair_New();

    if (!new)
	return ((struct lpair *) 0);
    switch (splittype) {
      case lpair_Percent:
	switch (splitdir) {
	  case lpair_SideBySide:
	    if (side == lpair_TopLeft)
		lpair_HSplit(lp, v, new, 100 - splitval, movable);
	    else
		lpair_HSplit(lp, new, v, splitval, movable);
	    break;
	  case lpair_AboveAndBelow:
	    if (side == lpair_TopLeft)
		lpair_VSplit(lp, v, new, 100 - splitval, movable);
	    else
		lpair_VSplit(lp, new, v, splitval, movable);
	    break;
	}
	break;
      case lpair_Fixed:
	switch (splitdir) {
	  case lpair_SideBySide:
	    if (side == lpair_TopLeft)
		lpair_HTFixed(lp, v, new, splitval, movable);
	    else
		lpair_HFixed(lp, new, v, splitval, movable);
	    break;
	  case lpair_AboveAndBelow:
	    if (side == lpair_TopLeft)
		lpair_VTFixed(lp, v, new, splitval, movable);
	    else
		lpair_VFixed(lp, v, new, splitval, movable);
	    break;
	}
	break;
    }
    return (new);
}

boolean rdemosh__InitializeClass(c)
struct classheader *c;
{
    return (TRUE);
}

static void ZombieHandler(pid, rock, status)
int pid;
long rock;
int *status;
{
    Buttons[rock].pid = 0;
    if (AuditFile) {
	fprintf(AuditFile, "%ld\tEXIT-%s\n", time(0), Buttons[rock].label);
	fflush(AuditFile);
    }
}

static void Do(name, frame, index, args, newpag)
char *name;
struct frame *frame;
long index;
char **args;
boolean newpag;
{
    int forkval;
    char msgbuf[100];

    if (Buttons[index].pid > 0) {
	sprintf(msgbuf, "%s is already running", name);
	message_DisplayString(frame, 100, msgbuf);
	return;
    }
    if (AuditFile) {
	fprintf(AuditFile, "%ld\t%s\n", time(0), name);
	fflush(AuditFile);
    }
    if ((forkval = osi_vfork()) == 0) {
	if (newpag)
	    setpag();
	execvp(*args, args);
    }
    else {
	if (forkval > 0) {
	    sprintf(msgbuf, "Starting %s.  Window should appear soon.",
		    name);
	    message_DisplayString(frame, 0, msgbuf);
	    Buttons[index].pid = forkval;
	    im_AddZombieHandler(forkval, ZombieHandler, index);
	}
	else {
	    sprintf(msgbuf, "Error starting %s (%s)",
		    name, sys_errlist[errno]);
	    message_DisplayString(frame, 100, msgbuf);
	}
	frame_WantInputFocus(frame, frame);
    }
}

static void DoHelp(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "helpapp", "-d", "-new", NULL};

    Do("Help", self->Frame, rock, args, TRUE);
}

static void DoTour(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "ezapp", "-d", TOURFILE, NULL};

    Do("Tour", self->Frame, rock, args, TRUE);
}

static void DoEz(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "ezapp", "-d", EZFILE, NULL};

    Do("Ez", self->Frame, rock, args, TRUE);
}

static void DoComments(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "comment", "-d", COMMENTFILE, NULL};

    Do("Comment", self->Frame, rock, args, TRUE);
}

static void DoConsole(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "consoleapp", "-d", NULL};

    Do("Console", self->Frame, rock, args, TRUE);
}

static void DoMessages(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"runapp", "-l", "typetext", "-l", "typescript",
			       "messagesapp", "-d", NULL};

    Do("Messages", self->Frame, rock, args, FALSE);
}

static void DoExit(self, b, rock)
struct rdemosh *self;
struct pushbutton *b;
long rock;
{
    im_KeyboardExit();
}

boolean rdemosh__InitializeObject(c, self)
struct classheader *c;
struct rdemosh *self;
{
    int len = ((sizeof (Buttons)) / (sizeof (struct buttoninfo))) - 1;

    if (!(self->buttons = (struct pushbutton **)
	  malloc(len * (sizeof (struct pushbutton *)))))
	return (FALSE);
    if (!(self->buttontext = (struct text **)
	  malloc(len * (sizeof (struct text *)))))
	return (FALSE);
    self->numbuttons = len;
    rdemosh_SetMajorVersion(self, MAJORVERSION);
    rdemosh_SetMinorVersion(self, MINORVERSION);
    rdemosh_SetName(self, "Remote Andrew Demo");
    self->timeup = FALSE;
    return (TRUE);
}

void rdemosh__FinalizeObject(c, self)
struct classheader *c;
struct rdemosh *self;
{
}

static void GetEmailAddress(self, time)
struct rdemosh *self;
long time;
{
    self->email[0] = '\0';
    message_AskForString(self->Frame, 100, "Please enter your e-mail address",
			 NULL, self->email,
			 (sizeof (self->email) - 1));
    if (AuditFile) {
	fprintf(AuditFile, "email\t%s\n", self->email);
	fflush(AuditFile);
    }
}

static void TimeUp(signal, self)
int signal;
struct rdemosh *self;
{
    self->timeup = TRUE;
    im_KeyboardExit();
}

/* Futile attempt at maintaining consistency in the face of adversity */
static void YaGotMe(signal, self)
int signal;
struct rdemosh *self;
{
    struct buttoninfo *ptr;

    for (ptr = Buttons; ptr->label; ++ptr) {
	if (ptr->pid) {
	    kill(ptr->pid, SIGTERM);
	    ptr->pid = 0;
	}
    }
    if (AuditFile) {
	fprintf(AuditFile, "%ld\tSIGNAL-%d\n", time(0), signal);
	fflush(AuditFile);
	fsync(fileno(AuditFile));
	fclose(AuditFile);
	sync();
	AuditFile = (FILE *) 0;
    }
}

int rdemosh__Run(self)
struct rdemosh *self;
{
    int result = super_Run(self);
    char buf[100 + MAXPATHLEN];
    struct buttoninfo *ptr;

    if (result != 0) {
	fprintf(stderr, "Could not run application!\n");
    }
    for (ptr = Buttons; ptr->label; ++ptr) {
	if (ptr->pid) {
	    kill(ptr->pid, SIGTERM);
	    ptr->pid = 0;
	}
    }
    buf[0] = '\0';
    if (self->timeup)
	strcat(buf, "Time limit has expired.  ");
    strcat(buf, "Thank you for using the Remote Andrew Demo.");
    message_DisplayString(self->Frame, 100, buf);
    im_VanishWindow(frame_GetIM(self->Frame));
    close(0);
    close(1);
    close(2);

    if (AuditFile) {
	fprintf(AuditFile, "%ld\t%s\n", time(0),
		self->timeup ? "TIMEOUT" : "EXIT");
	fflush(AuditFile);
	fsync(fileno(AuditFile));
	fclose(AuditFile);
	sync();
	AuditFile = (FILE *) 0;
    }
    return (result);
}

boolean rdemosh__Start(self)
struct rdemosh *self;
{
    struct lpair *tmp, *top, *buttonlp;
    struct pushbuttonview *pbv;
    struct textview *tv, *tvtmp;
    struct im *Im;
    struct view *applayer;
    int i;
    FILE *fp;
    char auditfilename[1 + MAXPATHLEN];

    if (!super_Start(self)) {
	fprintf(stderr, "Could not start application!\n");
	return (FALSE);
    }
    self->Frame = frame_New();
    self->Text = text_New();
    if (!(fp = fopen(WELCOMEFILE, "r"))) {
	fprintf(stderr, "Could not read welcome file (%s)!\n",
		sys_errlist[errno]);
	return (FALSE);
    }
    text_AlwaysInsertFile(self->Text, fp, WelcomeFile, 0);
    fclose(fp);
    text_SetReadOnly(self->Text, TRUE);
    Im = im_Create(NULL);
    tv = textview_New();
    textview_SetDataObject(tv, self->Text);
    applayer = textview_GetApplicationLayer(tv);
    top = tmp = lpair_New();
    tmp = lpair_AddSplit(tmp, applayer, lpair_TopLeft, lpair_AboveAndBelow,
			 lpair_Percent, 50, FALSE);
    for (i = 0; i < self->numbuttons; ++i) {
	self->buttons[i] = pushbutton_New();
	self->buttontext[i] = text_New();
	pbv = pushbuttonview_New();
	tvtmp = textview_New();
	pushbuttonview_SetDataObject(pbv, self->buttons[i]);
	textview_SetDataObject(tvtmp, self->buttontext[i]);
	pushbutton_SetText(self->buttons[i], Buttons[i].label);
	pushbutton_SetStyle(self->buttons[i], pushbutton_MOTIF);
	text_AlwaysInsertCharacters(self->buttontext[i], 0,
				    Buttons[i].text,
				    strlen(Buttons[i].text));
	text_SetReadOnly(self->buttontext[i], TRUE);
	pushbuttonview_AddRecipient(pbv, atom_Intern("buttonpushed"),
				    self, Buttons[i].fn, (long) i);
	buttonlp = lpair_New();
	lpair_HSplit(buttonlp, pbv, tvtmp, 70, FALSE);
	if (i < (self->numbuttons - 1)) {
	    if (i < (self->numbuttons - 2)) {
		tmp = lpair_AddSplit(tmp, (struct view *) buttonlp,
				     lpair_TopLeft,
				     lpair_AboveAndBelow, lpair_Percent,
				     100 / (self->numbuttons - i), FALSE);
	    }
	    else {
		lpair_VSplit(tmp, buttonlp, NULL, 50, FALSE);
	    }
	}
	else {
	    lpair_SetNth(tmp, 1, buttonlp);
	}
    }
    frame_SetView(self->Frame, top);
    im_SetView(Im, self->Frame);
    frame_WantInputFocus(self->Frame, self->Frame);

    sprintf(auditfilename, "%s/%s", AUDITDIR, MyUniqueId);
    if (AuditFile = fopen(auditfilename, "w")) {
	struct xim *xim = (struct xim *) Im;
	Display *dpy = xim2display(xim);
	int screen = xim2screen(xim);

	fprintf(AuditFile, "%s\t%s\t%ld\n", MyHostname,
		getenv("DISPLAY"), time(0));
	fprintf(AuditFile,
		"vendor\t%s\nversion\t%d.%d\nrelease\t%d\nplanes\t%d\nscreens\t%d\n",
		ServerVendor(dpy), ProtocolVersion(dpy), ProtocolRevision(dpy),
		VendorRelease(dpy), DisplayPlanes(dpy, screen),
		ScreenCount(dpy));
	fflush(AuditFile);
    }
    im_SignalHandler(SIGXCPU, TimeUp, (char *) self);
    im_SignalHandler(SIGALRM, TimeUp, (char *) self);
    im_SignalHandler(SIGTERM, YaGotMe, (char *) self);
    im_SignalHandler(SIGPIPE, YaGotMe, (char *) self);
    alarm(ALARMTIME);
    im_EnqueueEvent(GetEmailAddress, (char *) self, 0L);
    return (TRUE);
}

boolean rdemosh__ParseArgs(self, argc, argv)
struct rdemosh *self;
int argc;
char **argv;
{
    if (super_ParseArgs(self, argc, argv)) {
	if (*argv == NULL) {
	    fprintf(stderr, "No arguments given!\n");
	    return (FALSE);
	}
	++argv;			/* Skip argv[0] */
	if ((MyHostname = *(argv++)) == NULL) {
	    fprintf(stderr, "No hostname given!\n");
	    return (FALSE);
	}
	if ((MyUniqueId = *argv) == NULL) {
	    fprintf(stderr, "No ID given!\n");
	    return (FALSE);
	}
	return (TRUE);
    }
    fprintf(stderr, "Application could not parse arguments!\n");
    return (FALSE);
}
