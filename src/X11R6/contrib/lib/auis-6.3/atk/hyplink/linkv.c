/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/hyplink/RCS/linkv.c,v 1.20 1992/12/15 21:36:09 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <sys/param.h>	/* for MAXPATHLEN */
#include <linkv.eh>
#include <link.ih>
#include <atom.ih>
#include <buffer.ih>
#include <complete.ih>
#include <fontdesc.ih>
#include <frame.ih>
#include <im.ih>
#include <menulist.ih>
#include <message.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <pshbttnv.ih>
#include <textv.ih>
#include <text.ih>
#include <view.ih>

#ifdef ATTBL_ENV
#include <bind.ih>
#include <environ.ih>
#include "shellesc.ih"
#endif

/* Defined constants and macros */
#define DBG(x) fprintf(stderr, "\nDebug: %s.", x);fflush(stderr);

#define PROMPTFONT "andy12b"
#define FONT "andy"
#define FONTSIZE 12
#define BUTTONDEPTH 4
#define BUTTONPRESSDEPTH 2
#define TEXTPAD 2

/* External Declarations */

/* Forward Declarations */
static void LinkProc(),  WarpLink(), TargetProc(), AutolinkProc(), InsertProc();
static boolean FindBuffer();
static char *FileName();
static struct view *FindViewofBuffer();

/* Global Variables */
static struct menulist *linkview_menulist = NULL;
static struct linkview *autolink_source = NULL;
static struct classinfo *proctype = NULL;
static struct classinfo *textviewtype = NULL;


#ifdef ATTBL_ENV
static struct bind_Description frameBindings[] = {
    {"linkview-set-target", "", 0, "Inset~33,Autolink target here~12", 0, 0, TargetProc, "Execute this proc from the frame of the the buffer for the target file of a link.  To be called after linkview-autolink."},
    NULL
};

static struct bind_Description textviewBindings[] = {
    {"linkview-insert-link", "", 0, "Inset~33,Insert Link~13", 0, 0, InsertProc, "Insert a link object in the current document."},
    NULL
};
#endif

boolean
linkview__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data, particularly, set up the proc table entries 
  and the menu list (which is cloned for each instance of this class).
*/

  struct proctable_Entry *proc = NULL;

  proctype = class_Load("frame");
  textviewtype = class_Load("textview");
  linkview_menulist = menulist_New();

#ifndef ATTBL_ENV
  proc = proctable_DefineProc("linkview-set-target", TargetProc, proctype, NULL, "Execute this proc from the frame of the the buffer for the target file of a link.  To be called after linkview-autolink.");

  proc = proctable_DefineProc("linkview-insert-link", InsertProc, textviewtype, NULL, "Insert a link object in the current document.");
#endif

  proc = proctable_DefineProc("linkview-autolink", AutolinkProc, &linkview_classinfo, NULL, "Starts the autolink process.  Waits for linkview-set-target to be invoked, which tells this link what file to link to.");
  menulist_AddToML(linkview_menulist, "Link~1,Autolink~1", proc, NULL, 0);

  proc = proctable_DefineProc("linkview-set-link", LinkProc, &linkview_classinfo, NULL, "Prompts for user to set target filename of the link button.");
  menulist_AddToML(linkview_menulist, "Link~1,Set Link~11", proc, NULL, 0);

#ifdef ATTBL_ENV
  attachBindings(frameBindings, TRUE, environ_GetProfileSwitch("LinkMenus", TRUE), "frame");
  attachBindings(textviewBindings, TRUE, environ_GetProfileSwitch("LinkMenus", TRUE), "textview");
#endif

  return(TRUE);
}


boolean
linkview__InitializeObject(c, self)
struct classheader *c;
struct linkview *self;
{
/*
  Set up the data for each instance of the object (i.e.: clone the menu
  list, add the trigger recipient).
*/

  self->ml = menulist_DuplicateML(linkview_menulist, self);
  return(linkview_AddRecipient(self, atom_Intern("buttonpushed"), self, WarpLink, 0L));
}


void
linkview__FinalizeObject(c, self)
struct classheader *c;
struct linkview *self;
{
  return;
}



void
linkview__PostMenus(self, ml)
struct linkview *self;
struct menulist *ml;
{
/*
  Enable the menus for this object.
*/

  menulist_ClearChain(self->ml);
  if (ml) menulist_ChainAfterML(self->ml, ml, ml);
  super_PostMenus(self, self->ml);
}


	static void
LinkProc(self, param)
	struct linkview *self;
	char *param;
{
/*
  This is the routine which asks the user for the target of the link.
  Uses the (amazingly quick) completion package.
*/

	char buf[MAXPATHLEN], *p;
	struct link *l = (struct link *)linkview_GetDataObject(self);

	if (param != NULL) 
	    strcpy(buf, param);
	else {
	    if (completion_GetFilename(self, "New target file for link:  ",
				       link_GetRawLink(l),
				       buf, sizeof(buf), 0, 0) < 0)
		return;
	    if (index(buf, '$')) {
		char tmpbuf[1 + MAXPATHLEN];

		(void) im_GetDirectory(tmpbuf);
		strcat(tmpbuf, "/$");
		if (strncmp(buf, tmpbuf, strlen(tmpbuf)) == 0) {

		    /* Okay, here's a really gross hack.
		       This hack remedies the following problem:
		         completion_GetFilename will prepend the current
			 directory to the resulting string when the user's
			 input looks like a relative pathname.  Unfortunately,
			 strings of the form "$FOO/whatever" look like
			 relative pathnames, and so get changed to
			 "/xyz/abc/$FOO/whatever", which is wrong.  On the
			 other hand, if the user's input is
			 "jkl/$FOO/pqr" then it is correct to prepend the
			 working directory.
		       The solution is to look at the result of
		       completion_GetFilename and check whether it's of
		       the form "workingdirectory/$anything".  If it is,
		       we strip the "workingdirectory/" off it.
		       At this point in the code, we know it's of that form.
		     */

		    strcpy(tmpbuf, buf + strlen(tmpbuf) - 1);
		    strcpy(buf, tmpbuf);
		}
	    }
	}

	link_SetLink(l, buf);
	if (link_GetText(l) == NULL) {
	    link_SetText(l, FileName(buf));
	    message_DisplayString(self, 10, "Changed link target (and label).");
	} else {
	    message_DisplayString(self, 10, "Changed link target.");
	}
}



static boolean
FindBuffer(f,b)
struct frame *f;
struct buffer *b;
{
/*
  Little, dippy routine passed to frame_Enumerate to find the
  frame which contains the buffer we want.
*/

  return(frame_GetBuffer(f)==b);
}


static struct view *
FindViewofBuffer(b)
struct buffer *b;
{
/*
  I don't know why *I* have to do this, it should be a buffer method.
  Anyways, this tries to find the frame of our buffer.  If there is no
  such frame, make one, make a new IM for it (new window), and put the
  buffer in the frame in the IM.  *phew*
*/

  struct frame *f;
  struct im *im;

  if ((f = frame_Enumerate(FindBuffer, b))==NULL) {
    /* No frame--need to map buffer to new window */

    if((f = frame_New()) == NULL) {
	fprintf(stderr,"hyplink: Could not allocate enough memory.\n");
	return((struct view*) NULL);
    }
    if((im = im_Create(NULL)) == NULL) {
	fprintf(stderr,"hyplink: Could not create new window.\n");
	if(f) frame_Destroy(f);
	return((struct view*) NULL);
    }
    im_SetView(im, f);

    frame_SetCommandEnable(f, TRUE);
    frame_PostDefaultHandler(f, "message", frame_WantHandler(f, "message"));
    frame_SetBuffer(f, b, TRUE);
  }
  return(frame_GetView(f));
}


static void
WarpLink(self, triggerer, rock)
struct linkview *self;
struct observable *triggerer;
long rock;
{
/*
  Do the actual "warp".  The semantics I want are:
  If the file isn't in a buffer, put it in one.
  If the buffer isn't in a window, put it in one.
  Warp the cursor & set the input focus to the target window (file).
*/

  char temp[MAXPATHLEN];
  struct buffer *buffer;
  struct view *view;
  struct im *im;
  char *filename;
  struct link *b = (struct link *)linkview_GetDataObject(self);

  if (!(filename=link_GetResolvedLink(b))) {
      message_DisplayString(self, 10, "No link");
      return;
  }

#ifdef ATTBL_ENV
  /* extend so that hyplink can execute a command */
  if (filename[0] == '!') {
      /* execute command and capture output in buffer */
      shellesc_RunAsync(&filename[1], "Link Endpoint", &buffer, NULL, NULL, 0, 0, FM_CREATE);
  }
  else {
      /* read file into buffer */
#endif
  if ((buffer = buffer_FindBufferByFile(filename)) == NULL) {
    /* don't have the file in memory, get it */
    if ((buffer = buffer_GetBufferOnFile(filename, 0)) == NULL) {
      sprintf(temp, "Couldn't get file %s.", filename);
      message_DisplayString(self, 50, temp);
      return;
    }
  }
#ifdef ATTBL_ENV
  }
#endif
  /* have the file in buffer buffer */

  /* get a view on the buffer */
  view = FindViewofBuffer(buffer);

  if ((view == NULL) || ((im = view_GetIM(view)) == NULL)) {
    message_DisplayString(self, 50, "Couldn't find window.");
    return;
  }

  view_WantInputFocus(view,view);

  /* If view is a textview, set dot pos-len */
  if (class_IsTypeByName(class_GetTypeName(view), "textview")) {
      struct textview *tv = (struct textview *)view;
      struct text *t = (struct text *)textview_GetDataObject(tv);
      long pos = link_GetPos(b);
      long len = link_GetLen(b);
      long docLen = text_GetLength(t);
      if (pos > docLen) pos = docLen;
      if (pos + len > docLen) len = docLen - pos;
      textview_SetDotPosition(tv, pos);
      textview_SetDotLength(tv, len);
      textview_FrameDot(tv, pos);
      textview_WantInputFocus(tv, tv);
  }

  /* pop to top window */
  im_ExposeWindow(im);
  /* warp cursor there */
  im_SetWMFocus(im);
}


static char *
FileName(path)
char *path;
{
/*
  Returns the filename portion of path (i.e.: strips leading
  directories and any extensions--unless the filename begins
  with a '.', in which case it strips the extensions after the 
  first '.' [so: ".cshrc" and ".cshrc.BAK" maps to ".cshrc"]);
  Warning:  destructively modifies the string path!
*/
  char *r, *s;

  if ((s = rindex(path, '/'))==NULL) {
    s = path;
  } else {
    ++s;
  }
  if ((r = index(s+1, '.'))!=NULL) *r = '\0';

  return(s);
}


static void
InsertProc(tv,l)
struct textview *tv;
long l;
{
    long pos;
    struct text *t = (struct text *) textview_GetDataObject(tv);
    pos = textview_GetDotPosition(tv) + textview_GetDotLength(tv);
    tv->currentViewreference = text_InsertObject(t, pos,"link", NULL);
    text_NotifyObservers(t,0);
}


static void
TargetProc(v, param)
struct view *v;
long param;
{
/*
  First, checks to see if there is a link object waiting for an
  autolink.  Then, if checks to make sure this frame has a buffer,
  and that the buffer has a filename.
*/
  struct buffer *fb;
  char *fn, name[255];
  struct link *l;

  if (!autolink_source) {
    message_DisplayString(v, 50, "Please set autolink source first.");
    return;
  }

  if (!class_IsTypeByName(class_GetTypeName(v),"frame")) {
    message_DisplayString(v, 10, "Autolink works on frames only");
    return;
  }
  if ((fb = (struct buffer *)frame_GetBuffer(((struct frame *)v)))==NULL) {
    message_DisplayString(v, 10, "Autolink works with buffers only");
    return;
  }

  if ((fn = buffer_GetFilename((struct buffer *)fb))==NULL) {
    message_DisplayString(v, 50, "Can only autolink to buffers on files.");
  } else {
    struct view *vw;
    strcpy(name, fn);
    l = (struct link *)linkview_GetDataObject(autolink_source);
    link_SetLink(l, name);
    /* if textview, also set pos and len */
    vw = frame_GetView(((struct frame *)v));

    if (vw != NULL & class_IsTypeByName(class_GetTypeName(vw), "textview")) {
	struct textview *tv = (struct textview *)vw;
	link_SetPos(l, textview_GetDotPosition(tv));
	link_SetLen(l, textview_GetDotLength(tv));
    }
    /* Now set the label if needed */
    if (link_GetText(l) == NULL) {
	link_SetText(l,FileName(name));
	message_DisplayString(autolink_source, 10, "Changed link target (and label).");
    } else {
      message_DisplayString(autolink_source, 10, "Changed link target.");
    } /* if (!link_LabelSetP(...)) */
  } /* if (!(fn = ...)) */
  autolink_source = NULL;

  return;
}


static void
AutolinkProc(self, param)
struct linkview *self;
long param;
{
/*
  Start the autolink process.  Check to make sure we're not trouncing
  on another autolink first though....
*/
  static char *conflict[] = {
    "Use new autolink source",
    "Use old autolink source",
    "Cancel autolink",
    NULL
  };
  int answer;

  if (autolink_source) {
    if (message_MultipleChoiceQuestion(self,99,"Already autolinking!", 2, &answer, conflict, NULL)>= 0) {
      switch(answer) {
      case 0:
	message_DisplayString(self, 10, "Please indicate autolink target buffer.");
	message_DisplayString(autolink_source, 10, "Cancelled.  Using new source.");
	autolink_source = self;
	return;
      case 1:
	message_DisplayString(self, 10, "Cancelled.  Using old source");
	message_DisplayString(autolink_source, 10, "Please indicate autolink target buffer with the linkview-set-target proc.");
	return;
      case 2:
	message_DisplayString(self, 10, "Autolink cancelled.");
	message_DisplayString(autolink_source, 10, "Autolink cancelled.");
	autolink_source = NULL;
	return;
      }
    } else {
	message_DisplayString(self, 10, "Autolink cancelled.");
	message_DisplayString(autolink_source, 10, "Autolink cancelled.");
	autolink_source = NULL;
	return;
    }
  }
  message_DisplayString(self, 10, "Please indicate autolink target buffer.");
  autolink_source = self;

  return;
}

static void linkview__Link(self)
struct linkview *self; {
    LinkProc(self, 0);
}

static void linkview__AutoLink(self)
struct linkview *self; {
    AutolinkProc(self, 0);
}

static void linkview__LinkFile(self, dest)
struct linkview *self;
char *dest; {
    LinkProc(self, dest);
}
