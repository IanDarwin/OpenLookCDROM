

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/launchapp/RCS/launchapp.c,v 1.8 1992/12/15 21:36:09 rr2b R6tape $";
#endif
#include <stdio.h>
#include <andrewos.h>
#include <sys/param.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/file.h>

#include <im.ih>
#include <frame.ih>
#include <environ.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <pshbttn.ih>
#include <pshbttnv.ih>
#include <message.ih>
#include <fontdesc.ih>
#include <style.ih>

#ifdef USE_CLOCK
#include <clock.ih>
#include <clockv.ih>
#endif /* USE_CLOCK */

#include <launchapp.eh>

#define MAJORVERSION (1)
#define MINORVERSION (0)

#define TOURFILENAME "/doc/AtkTour/Tour"

#define WELCOMESTR ("Welcome to the Andrew Toolkit Application Launcher")
#define SPIELTEXT ("We hope you enjoy this opportunity to try the Andrew Toolkit.  Please mail any comments or questions to ")
#define MAILADDR ("info-andrew-request+@andrew.cmu.edu")


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
static void DoTypescript(), DoBush();

static struct buttoninfo {
    char *label, *text;
    void (*fn)();
    int pid;
} Buttons[] = {
    {"tour", "Tour of the Andrew Toolkit", DoTour, 0},
    {"ez", "Multimedia editor", DoEz, 0},
    {"help", "On-line documentation reader", DoHelp, 0},
    {"messages", "Mail, bboard, and news reader", DoMessages, 0},
    {"typescript", "Unix shell interface", DoTypescript, 0},
    {"console", "Machine performance monitor", DoConsole, 0},
    {"bush", "Filesystem browser", DoBush, 0},
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

static void ZombieHandler(pid, rock, status)
int pid;
long rock;
int *status;
{
    Buttons[rock].pid = 0;
}

static void Do(name, frame, index, args)
char *name;
struct frame *frame;
long index;
char **args;
{
    int forkval;
    char msgbuf[100];

    if (Buttons[index].pid > 0) {
	sprintf(msgbuf, "%s is already running", name);
	message_DisplayString(frame, 100, msgbuf);
	return;
    }
    if ((forkval = osi_vfork()) == 0) {
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


static char *Bin(str)
char *str;
{
    static char buf[1024];
    char *bin=environ_AndrewDir("/bin/");
    if(strlen(bin)>sizeof(buf)-strlen(str)-1) return str;
    strcpy(buf, bin);
    strcat(buf, str);
    return buf;
}

static void DoHelp(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"help", "-d", "-new", NULL};

    args[0]=Bin("help");
    
    Do("Help", self->frame, rock, args);
}

static void DoTour(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    char *args[4];
    args[0] = Bin("ez");
    args[1] = self->TourFile;
    args[2] = "-d";
    args[3] = NULL;
    Do("Tour", self->frame, rock, args);
}

static void DoEz(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    char *args[4];
    args[0] = Bin("ez");
    args[1] = environ_AndrewDir("/help/ez.help");
    args[2] = "-d";
    args[3] = NULL;
    Do("Ez", self->frame, rock, args);
}

static void DoConsole(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"console", "-d", NULL};

    args[0]=Bin("console");
    
    Do("Console", self->frame, rock, args);
}

static void DoMessages(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"messages", "-d", NULL};

    args[0]=Bin("messages");
    Do("Messages", self->frame, rock, args);
}

static void DoTypescript(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"typescript", "-d", NULL};

    args[0]=Bin("typescript");
    Do("Typescript", self->frame, rock, args);
}

static void DoBush(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    static char *args[] = {"bush", "-d", NULL};

    args[0]=Bin("bush");
    Do("Bush", self->frame, rock, args);
}

static void DoExit(self, b, rock)
struct launchapp *self;
struct pushbutton *b;
long rock;
{
    im_KeyboardExit();
}

boolean launchapp__InitializeObject(c, self)
struct classheader *c;
struct launchapp *self;
{
    char *tourFile = (char *) environ_AndrewDir(TOURFILENAME);

    self->numbuttons = ((sizeof (Buttons)) / (sizeof (struct buttoninfo))) - 1;
    if (!(self->buttons = (struct pushbutton **)
	  malloc(self->numbuttons * (sizeof (struct pushbutton *)))))
	return (FALSE);
    if (!(self->buttontext = (struct text **)
	  malloc(self->numbuttons * (sizeof (struct text *)))))
	return (FALSE);
    if(tourFile) {
	if((self->TourFile = (char *) malloc(strlen(tourFile) + 1)) != NULL)
	    strcpy(self->TourFile, tourFile);
	else {
	    printf("Can't allocate enough memory; exitting.");
	    return(FALSE);
	}
    }
    else {
	printf("Can't find tour file; exiting.");
	return(FALSE);
    }

    launchapp_SetMajorVersion(self, MAJORVERSION);
    launchapp_SetMinorVersion(self, MINORVERSION);
    launchapp_SetName(self, "Andrew Toolkit Application Launcher");

    return(TRUE);
}

void launchapp__FinalizeObject(c, self)
struct classheader *c;
struct launchapp *self;
{
    if(self->buttons) free(self->numbuttons);
    if(self->buttontext) {
	int i;
	for (i = 0; i < self->numbuttons; i++) {
	    text_Destroy(self->buttontext[i]);
	    pushbutton_Destroy(self->buttons[i]);
	}
	free(self->buttontext);
	free(self->buttons);
    }
    if(self->TourFile) free(self->TourFile);
}

/* Futile attempt at maintaining consistency in the face of adversity */
static void YaGotMe(signal, self)
int signal;
struct launchapp *self;
{
    struct buttoninfo *ptr;

    for (ptr = Buttons; ptr->label; ++ptr) {
	if (ptr->pid) {
	    kill(ptr->pid, SIGTERM);
	    ptr->pid = 0;
	}
    }
}

int launchapp__Run(self)
struct launchapp *self;
{
    int result;
    struct buttoninfo *ptr;

    if ((result = super_Run(self)) != 0) {
	fprintf(stderr, "Could not run application!\n");
    }
    for (ptr = Buttons; ptr->label; ++ptr) {
	if (ptr->pid) {
	    kill(ptr->pid, SIGTERM);
	    ptr->pid = 0;
	}
    }
    return (result);
}

boolean launchapp__Start(self)
struct launchapp *self;
{
    struct lpair *tmp, *top2, *top, *buttonlp;
    struct pushbuttonview *pbv;
    struct textview *tv, *tvtmp;
    struct im *Im;
    struct view *applayer;
    struct text *t;
    struct style *st;
#ifdef USE_CLOCK
    struct clock *cl;
    struct clockview *clv;
    struct clock_options *clopts;
#endif /* USE_CLOCK */
    int i;

    environ_Put("ANDREWDIR", environ_AndrewDir(""));
    if (!super_Start(self)) {
	fprintf(stderr, "Could not start application!\n");
	return (FALSE);
    }

    self->frame = frame_New();
    Im = im_Create(NULL);

    t = text_New();
    text_AlwaysInsertCharacters(t, (long) 0, WELCOMESTR,
				(sizeof (WELCOMESTR) - 1));
    text_SetReadOnly(t, TRUE);
    st = style_New();
    style_SetFontSize(st, style_ConstantFontSize, (long) 12);
    style_SetJustification(st, style_Centered);
    style_AddNewFontFace(st, fontdesc_Bold);
    style_AddNewFontFace(st, fontdesc_Italic);
    text_SetGlobalStyle(t, st);

    tv = textview_New();
    textview_SetDataObject(tv, t);

    top2 = tmp = lpair_New();
    tmp = lpair_AddSplit(tmp, tv, lpair_TopLeft, lpair_AboveAndBelow,
			 lpair_Percent, 20, FALSE);
    
    t = text_New();
    text_AlwaysInsertCharacters(t, (long) 0, SPIELTEXT,
				(sizeof (SPIELTEXT) - 1));
    text_AlwaysInsertCharacters(t, (sizeof (SPIELTEXT) - 1),
				MAILADDR, (sizeof (MAILADDR) - 1));
    st = style_New();
    style_AddNewFontFace(st, fontdesc_Bold);
    text_AddStyle(t, (sizeof (SPIELTEXT) - 1), (sizeof (MAILADDR) - 1), st);
    st = style_New();
    style_SetJustification(st, style_LeftJustified);
    text_SetGlobalStyle(t, st);
    text_SetReadOnly(t, TRUE);

    tv = textview_New();
    textview_SetDataObject(tv, t);
    applayer = textview_GetApplicationLayer(tv);
#ifdef USE_CLOCK
    cl = clock_New();
    clopts = clock_GetOptions(cl);
    clopts->border_shape = circle;
    clopts->border_width = 1;
    clopts->major_ticks = 4;
    clopts->minor_ticks = 12;
    clopts->seconds_length = 90;
    clock_SetOptions(cl, &clopts);

    clv = clockview_New();
    clockview_SetDataObject(clv, cl);
    lpair_HSplit(tmp, applayer, clv, 20, FALSE);
#else
    lpair_HSplit(tmp, applayer, NULL, 0, FALSE);
#endif /* USE_CLOCK */

    top = tmp = lpair_New();
    tmp = lpair_AddSplit(tmp, top2, lpair_TopLeft, lpair_AboveAndBelow,
			 lpair_Percent, 40, FALSE);
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
    frame_SetView(self->frame, top);
    im_SetView(Im, self->frame);
    frame_WantInputFocus(self->frame, self->frame);

    im_SignalHandler(SIGTERM, YaGotMe, (char *) self);
    im_SignalHandler(SIGPIPE, YaGotMe, (char *) self);
    return (TRUE);
}
