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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eos.c,v 1.3 1992/12/15 21:55:51 rr2b R6tape $";
#endif


 
/*
 * eos.c
 *
 * Basic view for EOS application.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>

#include <stdio.h>

#include <class.h>

#include <atom.ih>
#include <bind.ih>
#include <buffer.ih>
#include <complete.ih>
#include <cursor.ih>
#include <eos.h>
#include <environ.ih>
#include <dataobj.ih>
#include <fontdesc.ih>
#include <frame.ih>
#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <message.ih>
#include <style.ih>
#include <text.ih>
#include <textv.ih>

#define EOSUSETRANSIENTS

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <andrewos.h>	/* andrewos.h includes sys/types.h */
#include <eosfx.ih> /* eosfx.ih includes sys/types.h */
#undef label

#include <eos.eh>   /* eos.eh uses struct label */
#include <pickup.ih>	/* pickup.ih uses struct label */
#include <papers.ih>	/* papers.ih uses struct label */
#include <proctbl.ih>
#include <turnin.ih>	/* turnin.ih uses struct label */
#include <view.ih>

extern boolean eos__InitializeObject();
extern void eos__FinalizeObject();
extern void eos__LinkTree();
extern void eos__FullUpdate();
extern struct view *eos__Hit();
extern void eos__PostKeyState();
extern void eos__SetTitle();
extern void eos_NewWindow();

/* Global Variables */
static stringlist_res *Courses;
static struct cursor *clockcursor;
struct menulist *eosMenu;
struct keymap *eosKeys;

static struct {
    struct buffer *data;
    struct im *im;
    struct frame *fr;
} StyleGuideWindow;

struct helpRock {
    procedure HelpWork;
    struct text *text;
    long rock;
    char *partial;
};

int
ReadCourseList(self)
register struct eos *self;
{
    /* Read the list of available courses */
    /* Returns non-zero if failure */
    FX *fxp;
    char *errm;

    if (errm = eosfx_OpenCourse(self->course, &fxp))
	if (!fxp) {
	    /* We cannot do anything with this course! */
	    message_DisplayString(self->frame, 100, errm);
	    return -1;
	}
    /* Else - we have an fxp with which we can read (at least) */
    if (errm = eosfx_Directory(fxp, &Courses)) {
	message_DisplayString(self->frame, 100, errm);
	eosfx_Close(&fxp);
	return -1;
    }
    eosfx_Close(&fxp);
    return 0;
}

enum message_CompletionCode
CourseComplete(partial, self, buffer, bufferSize)
char *partial;
struct eos *self;
char *buffer;
int bufferSize;
{
    struct result result;
    stringlist node;
    char textBuffer[100];

    if (Courses == NULL)
	if (ReadCourseList(self))
	    return message_Invalid;

    textBuffer[0] = '\0';
    result.partialLen = strlen(partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer)-1;
    result.partial = partial;
    
    for (node = Courses->stringlist_res_u.list; node != NULL; node = node->next)
    {
	completion_CompletionWork(node->s, &result); 
    }
    strncpy(buffer, result.best, bufferSize);
    if (result.bestLen == bufferSize) /* Ensure it's a real string */
	buffer[result.bestLen] = '\0';
    return result.code;
}

static void
CourseHelp(partial, self, HelpWork, rock)
char *partial;
struct eos *self;
procedure HelpWork;
long rock;
{
    stringlist node;
    int l = strlen(partial);
 
    if (Courses == NULL)
	if (ReadCourseList(self))
	    return;

   (*HelpWork)(rock, message_HelpGenericItem, "List of possible courses:\n\n");
	
    for (node = Courses->stringlist_res_u.list; node != NULL; node = node->next)
    {
	if (partial)
	    if (strncmp(partial, node->s, l) != 0)
		continue;
	(*HelpWork)(rock, message_HelpListItem, node->s, NULL);
    }
}

static void SetCourse(self, rock)
struct eos *self;
char *rock;
/* Requests user to type the name of a course.
   if rock != NULL, it points to string containing desired coursename
 */
{
    char newco[33];
    char string[64];
    char oldco[33];
    char *errormsg;
    boolean success = FALSE;
    FX *fxp;

    strcpy(oldco, self->course); /* Keep copy of old course */

    if (rock)
	strcpy(self->course, rock);
    else {
	if (message_Asking(self->frame))
	    return;
	if (message_AskForStringCompleted(self->frame,self->dialogpri, "Name of course to use: ", self->course, newco, 32, NULL, CourseComplete, CourseHelp, self, message_MustMatch) == -1) {
	    message_DisplayString(self->frame, 0, "Cancelled.");
	    return;
	}
	strcpy(self->course, newco);
    }

    im_SetProcessCursor(clockcursor);
    message_DisplayString(self->frame, 0, "Checking course...");
    im_ForceUpdate();
    if (errormsg = eosfx_OpenCourse(self->course, &fxp)) {
	message_DisplayString(self->frame, 100, errormsg);
	if (fxp) {
	    /* Well, we can at least read from this course */
	    message_DisplayString(self->frame, 0, "Warning: errors were found when opening the course");
	    success = TRUE;
	} else {
	    /* This course is broken */
	    message_DisplayString(self->frame, 0, "Course not changed because of errors");
	    strcpy(self->course, oldco); /* Back to whatever it was before */
	}
    } else {
	message_DisplayString(self->frame, 0, "OK.");
	success = TRUE;
    }
    eosfx_Close(&fxp);
    im_SetProcessCursor(NULL);

    if (success) {
	/* Update the title (in case the title names the course) */
	if (self->gradingflag)
	    eos_SetTitle(self, "GRADE: Editor");
	else
	    eos_SetTitle(self, "EOS: Editor");
    }
    eos_WantInputFocus(self, self);
}

void
eos__SetCourse(self, course)
register struct eos *self;
char *course;
{
    /* Since we cannot keybind this, we have to have two procedures... */
    /* (You cannot put class_routines into bind_Description) */
    SetCourse(self, course);
}

static boolean CheckCourse(self)
struct eos *self;
/* If `course` is set to be "" i.e. no course is specified,
   then SetCourse is called to request the course in a Dialog.
   Post:  Returns TRUE if, on exit, the course has any value other
          than "". Else returns FALSE.
 */
{
    if (!strcmp(self->course, "")) {
        message_DisplayString(self->frame, MESSAGE, "You must specify a course for this function.");
        SetCourse(self, NULL);
        if (!strcmp(self->course, "")) {
            message_DisplayString(self->frame, MESSAGE, "Operation cancelled because no course was given");
            return FALSE;
        }
        message_DisplayString(self->frame, MESSAGE, "");
    }

    return TRUE;
}

struct papers *StartWindow(self, string, title, wdfactor, htfactor, wantframe)
register struct eos *self;
char *string;
char *title;
int wdfactor, htfactor;
boolean wantframe;
/* Creates a new 'paper' object on the screen.
   string = title box of window.
   the window is text + scroll + title [+ buttons] + frame
   If wantframe is true then a frame is wrapped around the window
 */
{
    long left, top, width, height;
    struct im *im;
    struct frame *fr;
    struct papers *window;

    /* We want the new window to be half the height of the current
     * so change the preferred dimensions temporarily
     */
    im_GetPreferedDimensions(&left, &top, &width, &height);
    im_SetPreferedDimensions(left, top, width/wdfactor, height/htfactor);
    im = im_Create(NULL);
    im_SetPreferedDimensions(left, top, width, height);
    if (wantframe) {
        fr = frame_New();
        if (!fr) {
            im_Destroy(im);
            message_DisplayString(self->frame, DIALOG, "Could not create a new window!");
            return NULL;
        }
    }

    /* Create the window */
    window = papers_New();
    papers_SetCourse(window, self->course);
    papers_SetTitle(window, string);
    papers_SetParent(window, self);
    if (self->gradingflag) papers_SetGrading(window);

    /* Place window into display */
    if (wantframe) {
        frame_SetView(fr, window);
        im_SetView(im, fr);
    } else
        im_SetView(im, window);

    im_SetTitle(im, title);
    return window;
}

struct turnin *StartTurninWindow(self)
register struct eos *self;
{
    long left, top, width, height;
    struct im *im;
    struct frame *fr;
    struct turnin *window;
    struct im *other;
    int	enlarge;

    /* We want the new window to be half the height of the current
      * so change the preferred dimensions temporarily
      */
    im_GetPreferedDimensions(&left, &top, &width, &height);

    /* enlarge if menubar on, otherwise labels are invisible */	
    if (environ_GetProfileSwitch("Menubar", FALSE)) enlarge = 20;
    else enlarge = 0;

#ifdef EOSUSETRANSIENTS
    if ((other = eos_GetIM(self)) == NULL) {
	printf ("Attempt to create dialogue box for nonexistant view!\n Creating top level window.");
	im_SetPreferedDimensions(left, top, width, width/2 + enlarge);
	im = im_Create(NULL);
    } else {
	im_SetPreferedDimensions(0, 0, width, width/2 + enlarge);
	im = im_CreateTransient(other);
    }
#else /*EOSUSETRANSIENTS */
    im_SetPreferedDimensions(left, top, width, width/2 + enlarge);
    im = im_Create(NULL);
#endif /* EOSUSETRANSIENTS */
    im_SetPreferedDimensions(left, top, width, height);
    fr = frame_New();
    if (!fr) {
	im_Destroy(im);
	message_DisplayString(self->frame, DIALOG, "Could not create a new window!");
	return NULL;
    }

    /* Create the window */
    window = turnin_New();
    turnin_SetCourse(window, self->course);
    turnin_SetTitle(window, "Turn In Assignment");
    turnin_SetParent(window, self);

    /* Place window into display */
    frame_SetView(fr, window);
    frame_SetCommandEnable(fr, FALSE);
    im_SetView(im, fr);

    im_SetTitle(im, "turn in");
    return window;
}

struct pickup *StartPickupWindow(self)
register struct eos *self;
{
    long left, top, width, height;
    struct im *im;
    struct frame *fr;
    struct pickup *window;
    struct im *other;

    /* We want the new window to be half the height of the current
      * so change the preferred dimensions temporarily
      */
    im_GetPreferedDimensions(&left, &top, &width, &height);
#ifdef EOSUSETRANSIENTS
    if ((other = eos_GetIM(self)) == NULL) {
	printf ("Attempt to create dialogue box for nonexistant view!\n Creating top level window.");
	im_SetPreferedDimensions(left, top, width, width/2);
	im = im_Create(NULL);
    } else {
	im_SetPreferedDimensions(0, 0, width, width/2);
	im = im_CreateTransient(other);
    }
#else /*EOSUSETRANSIENTS */
    im_SetPreferedDimensions(left, top, width, width/2);
    im = im_Create(NULL);
#endif /* EOSUSETRANSIENTS */
    im_SetPreferedDimensions(left, top, width, height);
    fr = frame_New();
    if (!fr) {
	im_Destroy(im);
	message_DisplayString(self->frame, DIALOG, "Could not create a new window!");
	return NULL;
    }

    /* Create the window */
    window = pickup_New();
    pickup_SetCourse(window, self->course);
    pickup_SetTitle(window, "Pick Up");
    pickup_SetParent(window, self);

    /* Place window into display */
    frame_SetView(fr, window);
    frame_SetCommandEnable(fr, FALSE);
    im_SetView(im, fr);

    im_SetTitle(im, "pick up");
    return window;
}

struct windowlist *LookUpWindow(wlist, course)
struct windowlist *wlist;
char *course;
/* Searches for course in wlist. If found, returns the window
   associated with course, else returns NULL
 */
{
    if (wlist == NULL) return NULL;

    while (wlist != NULL && strcmp(wlist->course, course))
        wlist = wlist->next;
    return (wlist ? wlist : NULL);
}

void AddWindow(wlist, window, course)
struct windowlist **wlist;
union windows *window;
char *course;
/* Pre:  window is a window onto an application which has not yet been
             used for `course`
  Post: returns <window,course>::wlist
 */
{
    struct windowlist *node = (struct windowlist *) malloc(sizeof(struct windowlist));

    node->course = (char *) malloc(strlen(course) + 1);
    strcpy(node->course, course);
    node->window = window;
    node->next   = *wlist;
    *wlist = node;
}

void eos_PickUp(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
/*
  Triggered from a button. It creates a new pickup object (if neccessary) and then returns
 */
{
    struct windowlist *win;
    struct pickup *window;
    union windows *listentry = (union windows *) malloc(sizeof(union windows));

    StopWaitCursor();
    if (!CheckCourse(self)) return;
    StartWaitCursor();

    win = LookUpWindow(self->pickups, self->course);
    if (win == NULL) {
        /* This is the only window in this course - so create it and place into list */
        if ((window = StartPickupWindow(self)) == NULL) return;
	listentry->pickupwin = window;
	AddWindow(&self->pickups, listentry, self->course);
        pickup_DoPickUp(window);
    } else
        if (win->window->pickupwin->IDoNotExist) {
            /* There was a pickup at one point but it was 'cancelled'
             */
	    im_ExposeWindow(pickup_GetIM(win->window->pickupwin));
            pickup_DoPickUp(win->window->pickupwin);
        } else {
            message_DisplayString(self->frame, MESSAGE, "Re-trying pickup.");
            im_ExposeWindow(pickup_GetIM(win->window->pickupwin));
            pickup_DoPickUp(win->window->pickupwin);
        }

    StopWaitCursor();
    eos_WantInputFocus(self, self);
}

void eos_TurnIn(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
{
    struct windowlist *win;
    struct turnin *window;
    union windows *listentry = (union windows *) malloc(sizeof(union windows));

    StopWaitCursor();
    if (!CheckCourse(self)) return;
    StartWaitCursor();

    win = LookUpWindow(self->turnins, self->course);
    if (win== NULL) {
        /* This is the only window in this course - so create it and place into list */
        if ((window = StartTurninWindow(self)) == NULL) return;
	listentry->turninwin = window;
        AddWindow(&self->turnins, listentry, self->course);
	turnin_GoForIt(window);
    } else
        if (win->window->turninwin->IDoNotExist) {
            /* There was a window at one point but it was 'cancelled'
             */
	    im_ExposeWindow(turnin_GetIM(win->window->turninwin));
	    turnin_GoForIt(win->window->turninwin);
        } else {
            message_DisplayString(self->frame, MESSAGE, "You already have a Turn In window for that course");
            im_ExposeWindow(turnin_GetIM(win->window->turninwin));
        }

    StopWaitCursor();
    eos_WantInputFocus(self, self);
}

void eos_Handouts(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
{
    struct windowlist *win;
    struct papers *hw;
    union windows *listentry = (union windows *) malloc(sizeof(union windows));

    StopWaitCursor();
    if (!CheckCourse(self)) return;
    StartWaitCursor();

    win = LookUpWindow(self->handouts, self->course);
    if (win== NULL) {
        /* This is the only window in this course - so create it and place into list */
        /* This should really be multi-threaded, so that handouts do not
         * interfere with the eos window, but alas...
         */
	if ((hw = StartWindow(self, "EOS: Handouts", "handouts", 1, 2, TRUE)) == NULL) return;
        if (self->gradingflag)
            papers_SetDisplay(hw, papers_SIDESUBDISP, papersHandouts);
        else
            papers_SetDisplay(hw, papers_SIDE, papersHandouts);
	listentry->paperswin = hw;
        AddWindow(&self->handouts, listentry, self->course);
	if (environ_GetProfileSwitch("PromptForDefaultHandouts", FALSE)) papers_SetDefault(hw);
        papers_ListHandouts(hw);
    } else
        if (win->window->paperswin->IDoNotExist) {
            /* There was a handouts at one point but it was 'cancelled'
	      * So, we need to expose the window and redo it's listing
             */
	    im_ExposeWindow(papers_GetIM(win->window->paperswin));
            papers_ListHandouts(win->window->paperswin);
        } else {
            message_DisplayString(self->frame, MESSAGE, "Updating pre-existing Handouts window.");
            im_ExposeWindow(papers_GetIM(win->window->paperswin));
            papers_ListHandouts(win->window->paperswin);
        }

    StopWaitCursor();
    eos_WantInputFocus(self, self);
}

void eos_Exchange(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
{
    struct windowlist *win;
    struct papers *window;
    union windows *listentry = (union windows *) malloc(sizeof(union windows));

    StopWaitCursor();
    if (!CheckCourse(self)) return;
    StartWaitCursor();

    win = LookUpWindow(self->exchanges, self->course);
    if (win == NULL) {
        /* This is the only window in this course - so create it and place into list */
	if ((window = StartWindow(self, "EOS: Exchange", "exchange", 1, 2, TRUE)) == NULL) return;
	listentry->paperswin = window;
        AddWindow(&self->exchanges, listentry,  self->course);
        if (self->gradingflag)
            papers_SetDisplay(window, papers_SIDESUBDISP, papersExchange);
        else
	    papers_SetDisplay(window, papers_SIDESUBMIT,    papersExchange);
	if (environ_GetProfileSwitch("PromptForDefaultExchange", FALSE)) papers_SetDefault(window);
        papers_ListExchange(window);
    } else
	if (win->window->paperswin->IDoNotExist) {
            /* There was an exchanges at one point but it was 'cancelled'
             */
	    im_ExposeWindow(papers_GetIM(win->window->paperswin));
            papers_ListExchange(win->window->paperswin);
        } else {
            message_DisplayString(self->frame, MESSAGE, "Updating pre-existing Exchange window.");
            im_ExposeWindow(papers_GetIM(win->window->paperswin));
            papers_ListExchange(win->window->paperswin);
        }

    StopWaitCursor();
    eos_WantInputFocus(self, self);
}

/* The next function is from cmu's hyplink/linkv.c
 * it is used in enumerating the frames to find a buffer
 */
static boolean FindBuffer(f,b)
struct frame *f;
struct buffer *b;
{
/*
  Little, dippy routine passed to frame_Enumerate to find the
  frame which contains the buffer we want.
*/

  return(frame_GetBuffer(f)==b);
}

static void OpenGuide(self)
struct eos *self;
/* 
  creates a new window (im[frame[buffer]]) on the file containing the style guide 
 */
{
    char string[128];

    if ((StyleGuideWindow.data = buffer_GetBufferOnFile(STYLEGUIDE, buffer_MustExist)) == NULL) {
	message_DisplayString(self->frame, DIALOG, "Could not find the Style Guide");
        strcpy(string, "The Style Guide is supposed to be at ");
        strcat(string, STYLEGUIDE);
        message_DisplayString(self->frame, MESSAGE, string);
        return;
    }

    StyleGuideWindow.fr = frame_New();
    frame_SetCommandEnable(StyleGuideWindow.fr, TRUE);
    frame_SetBuffer(StyleGuideWindow.fr, StyleGuideWindow.data, TRUE);
    StyleGuideWindow.im = im_Create(NULL);
    im_SetView(StyleGuideWindow.im, StyleGuideWindow.fr);
    eos_WantInputFocus(self, frame_GetView(StyleGuideWindow.fr));
    im_ForceUpdate();
}

void eos_StyleGuide(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
/* 
  Button triggered procedure which checks to see if there already is a style guide;
  If there is, then it merely makes sure it is visible, else it calls OpenGuide
 */
{
    struct frame *f;

    if (StyleGuideWindow.im == NULL || StyleGuideWindow.data == NULL) {
        /* First time style guide */
        OpenGuide(self);
    } else {
        if (!buffer_Visible(StyleGuideWindow.data))
            /* Yep - we got a buffer, but no window */
            OpenGuide(self);
        else {
            /* We have buffer AND window - so expose it */
            f = frame_Enumerate(FindBuffer, StyleGuideWindow.data);
            im_ExposeWindow(frame_GetIM(f));
        }
    }
    return;
}

void eos_Help(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
/* 
  Button triggered procedure. Fork()s an andrew help process, using the
   program name as the parameter to help
 */
{
    char *helpname = environ_AndrewDir("/bin/help");
    int fd;

    switch (osi_vfork()) {
	case 0:
	    for (fd = getdtablesize(); fd > 2; --fd) close(fd);
	    execl(helpname, helpname, im_GetProgramName(), 0);
	    printf ("Exec of %s failed.\n", helpname);
	    fflush (stdout);
	    _exit(-1);
	case -1:
	    message_DisplayString(self->frame, DIALOG, "Could not start the help!");
	    break;
	default:
	    message_DisplayString(self->frame, MESSAGE, "A Help window will appear shortly.");
	    break;
    }
    eos_WantInputFocus(self, self);
    return;
}

/*-------------------------------------------
 * The instructor's interface rountines 
 *-------------------------------------------
 */

void eos_Grade(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
/* Same as handouts, pickup, turnin and exchange */
{
    struct windowlist *win;
    struct papers *gw;
    union windows *listentry = (union windows *) malloc(sizeof(union windows));
    enum papers_Toggles type;

    StopWaitCursor();
    if (!CheckCourse(self)) return;
    StartWaitCursor();

    type = papers_NEW;
    if (environ_GetProfileSwitch("StartWithOldPapers", FALSE)) type = papers_OLD;

    win = LookUpWindow(self->grades, self->course);
    if (win == NULL) {
        /* This is the only window in this c             ourse - so create it and place into list */
	if ((gw = StartWindow(self, "GRADE: Papers to Grade", "grading", 1, 1, TRUE)) == NULL) return;
	listentry->paperswin = gw;
        AddWindow(&self->grades, listentry, self->course);
        papers_SetDisplay(gw, papers_ALTSIDE, papersGrading);
	if (environ_GetProfileSwitch("PromptForDefaultGrade", FALSE)) papers_SetDefault(gw);
	papers_GradingListType(gw, type);
    } else
	if (win->window->paperswin->IDoNotExist) {
            /* There was a window at one point but it was 'cancelled'
             */
	    im_ExposeWindow(papers_GetIM(win->window->paperswin));
	    papers_GradingListType(win->window->paperswin, type);
        } else {
            message_DisplayString(self->frame, MESSAGE, "If you really want an update,hide first, or use the menu command.");
            im_ExposeWindow(papers_GetIM(win->window->paperswin));
        }

    StopWaitCursor();
    eos_WantInputFocus(self, self);
    return;
}


void eos_Return(self, triggerer, rock)
struct eos *self;
struct observable *triggerer;
long rock;
{
    Paper paper;
    Paper *ppaper;
    long retrock;
    char answer[128], uname[33], strnum[6], *errormsg;   
    struct buffer *buf;
    struct dataobject *buf_data;
    int can_delete = 0;
    int version;
    FX *fxp;

    eosfx_PaperClear(&paper);

    buf = frame_GetBuffer (self->frame);
    buf_data = buffer_GetData (buf);

    if (!dataobject_Get (buf_data, self->paperatom, &(self->paperatom), &retrock)) {
	if (message_AskForString(self->frame, MESSAGE, "Please give the username of the recipient of this paper:", "", uname, 32) == -1) {
	    message_DisplayString(self->frame, MESSAGE, "Cancelled.");
	    eos_WantInputFocus(self, self);
	    return;
	}
	paper.author = uname;
	if (message_AskForString(self->frame, MESSAGE, "What is the assignment number?", "", strnum, 5) == -1) {
	    message_DisplayString(self->frame, MESSAGE, "Cancelled.");
	    eos_WantInputFocus(self, self);
	    return;
	}
	paper.assignment = atoi(strnum);
	paper.filename = eos_NameBuffer(self);
    } else {
	ppaper = (Paper *)retrock;
	eosfx_PaperCopy (ppaper, &paper);
	can_delete = 1;
    }

    paper.type = GRADED;
    message_DisplayString(self->frame, MESSAGE, "Please wait: Sending paper...");
    StartWaitCursor();
    im_ForceUpdate();
    strcpy(answer, (char *) mktemp(".eosXXXXXX"));
    buffer_WriteToFile(buf, answer, buffer_ReliableWrite | buffer_MakeBackup);
    if (errormsg = eosfx_SendFile(self->course, answer, &paper, TRUE)) {
	message_DisplayString(self->frame, MESSAGE, "Cancelled");
	message_DisplayString(self->frame, DIALOG, errormsg);
	eos_WantInputFocus(self, self);
	StopWaitCursor();
	im_ForceUpdate();
	return;
    } else message_DisplayString(self, MESSAGE, "Done.");

    /* We want to inform the buffer that it no longer needs saving */
    version = dataobject_GetModified(buf_data);
    buffer_SetCkpClock(buf, 0);
    buffer_SetCkpVersion(buf, version);
    buffer_SetWriteVersion(buf, version);
    unlink(buffer_GetCkpFilename(buf));

    /* Should we blow away original? */
    if (can_delete && environ_GetProfileSwitch ("OverwriteOriginalPaper", TRUE)) {
	/* Recover paper identity for delete. */
	if (errormsg = eosfx_OpenCourse(self->course, &fxp))
	    message_DisplayString(self->frame, DIALOG, errormsg);
	else {
	    if (errormsg = eosfx_Delete(fxp, ppaper))
		message_DisplayString(self->frame, DIALOG, errormsg);
	    else eosfx_Close(&fxp);
	}
	/* clear the old paper away (dataobj_UnPut would be nice) */
	namespace_Unbind (buf_data->properties, self->paperatom);
	eosfx_PaperFreeContents (ppaper);
	if (errormsg) message_DisplayString(self->frame, MESSAGE, "Paper has been returned.");
	else message_DisplayString(self->frame, MESSAGE, "Paper has been returned; original deleted.");
    } else message_DisplayString(self->frame, MESSAGE, "Paper has been returned.");
    eos_WantInputFocus(self, self);
    StopWaitCursor();
    im_ForceUpdate();
    return;
}

static char delwindowWarning[] =
  "Deleting this window kills the program.";
static char *delwindowChoices[] = {
	"Continue Running",
	"Quit Application",
	NULL};

#define delwindow_CANCEL 0
#define delwindow_QUIT   1

void eos_DeleteWindow(self)
    struct eos *self;
{
    long answer;
    if (message_MultipleChoiceQuestion(self->frame, 0,
					delwindowWarning, delwindow_CANCEL,
					&answer, delwindowChoices, NULL)
	 == -1)
	return;
    switch(answer){
	case delwindow_CANCEL:
	    return;

	case delwindow_QUIT :
	    frame_Exit(self->frame);
    }
}


void eos_SetDefault(self)
    struct eos *self;
{
    struct textview *tv;
    long dsize, usize, fontStyle;
    enum style_FontSize basis;
    char *font, fontname[100];
    struct buffer *buffer;

    /* tickle the font size if it's different from the default */
    tv = (struct textview *) frame_GetView(self->frame);
    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
	struct style *ds = textview_GetDefaultStyle(tv);

	if ((font = environ_GetProfile("bodyfont")) == NULL || ! fontdesc_ExplodeFontName(font, fontname, sizeof(fontname), &fontStyle, &dsize)) {
	    dsize = 12;
	}

	usize = environ_GetProfileInt("DisplayFontsize", 20);
	style_GetFontSize(ds, &basis, &usize);
	if (basis == style_ConstantFontSize && dsize != usize)
	    style_SetFontSize(ds, style_ConstantFontSize, dsize);
	eos_WantUpdate(self, self);
    }
}

void eos_SetDisplay(self)
    struct eos *self;
{
    struct textview *tv;
    long dsize, usize;
    enum style_FontSize basis;
    struct buffer *buffer;

    /* tickle the font size if it's different from the default */
    tv = (struct textview *) frame_GetView(self->frame);
    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
	struct style *ds = textview_GetDefaultStyle(tv);
	usize = environ_GetProfileInt("DisplayFontsize", 20);
	style_GetFontSize(ds, &basis, &dsize);
	if (basis == style_ConstantFontSize && dsize != usize)
	    style_SetFontSize(ds, style_ConstantFontSize, usize);
	eos_WantUpdate(self, self);
    }
}

void eos__SetFontDisplay(self)
    struct eos *self;
{
    eos_SetDisplay(self);
}

void eos__SetFontDefault(self)
    struct eos *self;
{
    eos_SetDefault(self);
}

static struct bind_Description eosBindings[]={
/*  { proc-name,     keybinding,rock, Menu name,        Menu rock, menuflag,
      function, documentation [, module-name]}
 */
    {"eos-change-course",    NULL, 0, "Change Course",         0, MENUS_general,   SetCourse,      "Set New Course Number"},
    {"eos-new-window",    "\0302", 0, NULL,         0, MENUS_general,   eos_NewWindow,      "Creates a new window"},
    {"eos-set-display-font",    NULL, 0, "View in BIG Font",         0, MENUS_general,   eos_SetDisplay,      "View buffer in Display Font"},
    {"eos-set-default-font",    NULL, 0, "View in Default Font",         0, MENUS_general,   eos_SetDefault,      "View buffer in Default Font"},
    {"eos-delete-window", "\030\004", 0, "Delete Window~89", 0, MENUS_delwindow, eos_DeleteWindow, "Same as to Quit"},
    NULL
};

boolean eos__InitializeClass(classID)
struct classheader *classID;
{
    eosMenu = menulist_New();
    eosKeys = keymap_New();
    bind_BindList(eosBindings, eosKeys, eosMenu, &eos_classinfo);

    clockcursor = cursor_Create(NULL);
    cursor_SetStandard(clockcursor, Cursor_Wait);
    StyleGuideWindow.im = NULL;
    return TRUE;
}

void eos__Update(self)
register struct eos *self;
{
/*    eos_EraseVisualRect(self); */
    eos_FullUpdate(self, view_FullRedraw, eos_GetLogicalTop(self), eos_GetLogicalLeft(self), eos_GetLogicalWidth(self), eos_GetLogicalHeight(self));
}

void eos__PostMenus(self, menulist)
struct eos *self;
struct menulist *menulist;
{
    /* Received the children's menus. Want to add our own options,
           then pass menulist up to superclass */
    menulist_ClearChain(self->menus);
    menulist_SetMask(self->menus, self->menuflags);
    if (menulist) menulist_ChainAfterML(self->menus, menulist, menulist);

    super_PostMenus(self,self->menus); 
}


void eos__ReceiveInputFocus(self)
struct eos *self;
/* 
  We want to ensure that when we receive the focus, the focus is passed on
  into the editing window
 */

{
    eos_WantInputFocus(self, frame_GetView(self->frame));
}


void eos__SetBuffer(self, filename, flags)
struct eos *self;
char *filename;
long flags;
{
    self->editregion = buffer_GetBufferOnFile(filename, flags);
    frame_SetBuffer(self->frame, self->editregion, TRUE);
}

char *eos__NameBuffer(self)
struct eos *self;
{
    return buffer_GetName(frame_GetBuffer(self->frame));
}

