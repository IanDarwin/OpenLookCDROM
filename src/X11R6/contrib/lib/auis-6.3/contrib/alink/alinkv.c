/* ********************************************************************** *\
 * Copyright (c) AT&T Bell Laboratories	1990  -	All Rights Reserved
 ****************************************************************************
*        Copyright AT&T Bell Laboratories - All Rights Reserved        *
*                                                                          *
* Permission to use, copy, modify, and distribute this software and its    *
* documentation for any purpose and without fee is hereby granted,         *
* provided that the above copyright notice appear in all copies and        *
* that both that copyright notice and this permission notice appear in     *
* supporting documentation, and that the name of IBM not be used in        *
* advertising or publicity pertaining to distribution of the software      *
* without specific, written prior permission.                              *
*                                                                          *
* IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL IBM *
* BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY      *
* DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER  *
* IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING   *
* OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.    *
****************************************************************************
*
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/alink/RCS/alinkv.c,v 1.5 1994/04/26 22:35:33 rr2b Exp $";
#endif


/*
 *  Audio link.
 *  
 *  Looks like a button, but when you press, it plays audio message.
 *  For sparcstation running Sun OS 4.1 or later.
 *
 *  Charles Hayden cch@mtgzfs3.att.com
 */

#include <alinkv.eh>
#include <alink.ih>
#include <andrewos.h>
#include <atom.ih>
#include <bind.ih>
#include <buffer.ih>
#include <complete.ih>
#include <dict.ih>
#include <environ.ih>
#include <im.ih>
#include <matte.ih>
#include <menulist.ih>
#include <message.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <pshbttnv.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>
#include <viewref.ih>

#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <sun/audioio.h>


#define AUDIO_DEV "/dev/audio"
#define AUDIO_CTL "/dev/audioctl"

/* Defined constants and macros */
#define DBG(x) fprintf(stderr, "\nDebug: %s.", x);fflush(stderr);

/* Forward Declarations */
static void LabelProc(), PlayAudio(), RecordAudio(),
            SetRecordGain(), SetPlayGain(), SetEarPhones();
static void InsertAlink(), InsertRecord();

/* Global Variables */
static struct menulist *alinkview_menulist = NULL;

static struct bind_Description alinkBindings[] = {
    {"alinkview-insert-alink", "", 0, "Inset~33,Insert Audio~20", 0, 0, InsertAlink, "Insert audio link."},
    {"alinkview-insert-record", "", 0, "Inset~33,Record Audio~21", 0, 0, InsertRecord, "Insert audio link and record."},
    NULL
};


boolean alinkview__InitializeClass(c)
struct classheader *c; {
/* 
  Initialize all the class data, particularly, set up the proc table entries 
  and the menu list (which is cloned for each instance of this class).
*/

  struct proctable_Entry *proc = NULL;
  struct classinfo *textviewtype = class_Load("textview");

  alinkview_menulist = menulist_New();

  proc = proctable_DefineProc("alinkview-play-audio", PlayAudio, &alinkview_classinfo, NULL, "Plays the audio segment.");
  menulist_AddToML(alinkview_menulist, "Audio~1,Play Audio~1", proc, NULL, 0);

  proc = proctable_DefineProc("alinkview-record-audio", RecordAudio, &alinkview_classinfo, NULL, "Records the audio segment.");
  menulist_AddToML(alinkview_menulist, "Audio~1,Change recording~11",
proc, NULL, 0);

  proc = proctable_DefineProc("alinkview-set-label-text", LabelProc, &alinkview_classinfo, NULL, "Prompts for user to set the text string of the link button.");
  menulist_AddToML(alinkview_menulist, "Audio~1,Change Label~12", proc,
NULL, 0);

#ifdef ATTBL_ENV
  attachBindings(alinkBindings, TRUE, environ_GetProfileSwitch("AlinkMenus", TRUE), "textview");
#else
  proc = proctable_DefineProc("alinkview-insert-alink", InsertAlink, textviewtype, NULL, "Insert audio link.");

  proc = proctable_DefineProc("alinkview-insert-record", InsertRecord, textviewtype, NULL, "Insert audio link and record.");

#endif

  return(TRUE);
}

boolean alinkview__InitializeObject(c, self)
struct classheader *c;
struct alinkview *self; {
/*
  Set up the data for each instance of the object (i.e.: clone the menu
  list, add the trigger recipient).
*/

  self->ml = menulist_DuplicateML(alinkview_menulist, self);
  return(alinkview_AddRecipient(self, atom_Intern("buttonpushed"), self,
PlayAudio, 0L));
}

void alinkview__FinalizeObject(c, self)
struct classheader *c;
struct alinkview *self; {
  return;
}


void alinkview__PostMenus(self, ml)
struct alinkview *self;
struct menulist *ml; {
/*
  Enable the menus for this object.
*/

  menulist_ClearChain(self->ml);
  if (ml) menulist_ChainAfterML(self->ml, ml, ml);
  super_PostMenus(self, self->ml);
}

static void InsertAlink(tv,l)
struct textview *tv;
long l; {
    long pos;
    struct text *t = (struct text *) textview_GetDataObject(tv);
    pos = textview_GetDotPosition(tv) + textview_GetDotLength(tv);
    tv->currentViewreference = text_InsertObject(t, pos,"alink", NULL);
    text_NotifyObservers(t,0);
}

/* This was copied from textview.c */
static void CreateMatte(tv, vr)
struct textview *tv;
struct viewref *vr; {
    struct view *v =
	(struct view *) dictionary_LookUp(tv, (char *) vr);

    if (v == NULL && class_IsTypeByName(vr->viewType, "view")) {
	struct matte *m = matte_Create(vr, (struct view *) tv);
	if (m != NULL) {
	    m->drawing = TRUE;
	    m->resizing = FALSE;
	    v = (struct view *)m;
	    viewref_AddObserver(vr, tv);
	    dictionary_Insert(tv, (char *) vr, (char *) v);
	}
    }

    if (v != NULL) {
	if (v->parent != (struct view *) tv)
	    view_LinkTree(v, (struct view *) tv);
	view_InitChildren(v);
    }
}

static void InsertRecord(tv,l)
struct textview *tv;
long l; {
    long pos;
    struct matte *m;
    struct alinkview *av;
    struct text *t = (struct text *) textview_GetDataObject(tv);
    pos = textview_GetDotPosition(tv) + textview_GetDotLength(tv);
    tv->currentViewreference = text_InsertObject(t, pos,"alink", NULL);

    m = (struct matte *)dictionary_LookUp(tv, tv->currentViewreference);
    if (m == NULL) {
	CreateMatte(tv, tv->currentViewreference);
	m = (struct matte *) dictionary_LookUp(tv, tv->currentViewreference);
    }
    av = (struct alinkview *)m->child;
    alinkview_RecordAudio(av);

    text_NotifyObservers(t,0);
}

static void LabelProc(self, param)
struct alinkview *self;
long param; {
/*
  This is the routine which asks the user for a new text label.
*/

  char buf[255];
  struct alink *l = (struct alink *)alinkview_GetDataObject(self);
  char *oldtext;

  oldtext = alink_LabelSetP(l) ? alink_GetText(l) : NULL;
  if (message_AskForString(self,50,"Enter new text for button: ",
			   oldtext, buf, sizeof(buf)) >= 0) {
    alink_SetText(l, buf);
    alink_NotifyObservers(l, observable_OBJECTCHANGED);
    alinkview_WantNewSize(self,self);
    alinkview_WantUpdate(self, self);
    message_DisplayString(self, 10, "Changed link text.");
  }
}

static void PlayAudio(self, triggerer, rock)
struct alinkview *self;
struct observable *triggerer;
long rock; {

    /*
     * Play a message here
     */

    struct alink *b = (struct alink *)alinkview_GetDataObject(self);
    char buf[256];
    char *p;
    long len, left;
    int audio_out;
    int interrupted = FALSE;
    struct im *im = alinkview_GetIM(self);

    if (!alink_GetAudio(b)) {
	message_DisplayString(self, 10, "No message.");
	return;
    }
    if ((audio_out = OpenForPlay(self, AUDIO_DEV)) < 0)
	return;

    message_DisplayString(self, 10, "Playing... Type Control-G to interrupt.");
    while(im_Interact(0));

    p = alink_GetAudio(b);
    left = len = alink_GetAudioLength(b);
    while (left > 0) {
	int nread = (left > 256 ? 256 : left);
	int wrtn = 0;

	if (im_CheckForInterrupt()) {
	    interrupted = TRUE;
	    break;
	}
	bcopy(p, buf, nread);
	p += nread;
	left -= nread;
	while (wrtn == 0) {
	    wrtn = write(audio_out, buf, nread);
	    if (wrtn == 0) usleep(1000);
	}
	if (wrtn != nread) break;
    }

    close(audio_out);

    if (interrupted)
	message_DisplayString(self, 10, "Playback interrupted.");
    else
	message_DisplayString(self, 10, "Playback complete.");
}

static void RecordAudio(self, param)
struct alinkview *self;
long param; {

    /*
     * Record a message here.
     */
    struct alink *b = (struct alink *)alinkview_GetDataObject(self);
    int audio_in;
    int rtn;
    int nread;
    char buf[256];
    long len = 0;
    char *abuf;
    long alen = 0;
    long alloc = 8192;
    struct im *im = alinkview_GetIM(self);

    if(environ_GetProfileSwitch("PromptForRecord", TRUE) == TRUE) {
	if (message_AskForString(self, 10, 
				 "Type RETURN to record.", 
				 NULL, buf, 256) < 0) 
	    return;
    }

    message_DisplayString(self, 10, "Type Control-G to end recording.");
    while(im_Interact(0));

    if ((audio_in = OpenForRecord(self, AUDIO_DEV)) < 0)
	return;

    abuf = malloc(alloc);
    alen = alloc;

    for ( ; ; ) {
	nread = read(audio_in, buf, 256);
	if (nread == 0) break;
	if (len + nread > alen) {
	    alen += alloc;
	    abuf = realloc(abuf, alen);
	}
	bcopy(buf, abuf + len, nread);
	len += nread;
	if (im_CheckForInterrupt()) break;
    }

    close(audio_in);

    abuf = realloc(abuf, len);

    alink_SetAudio(b, len, abuf);
    if (!alink_LabelSetP(b)) {
	char line[100];
	char *oldtext = alink_LabelSetP(b) ? alink_GetText(b) : NULL;
	message_AskForString(self, 10, "Label: ", oldtext, line, 100);
	alink_SetText(b, line);
	alinkview_WantNewSize(self, self);
	alinkview_WantUpdate(self, self);
    }
    else
	message_DisplayString(self, 10, "Recording complete.");
    alink_NotifyObservers(b, observable_OBJECTCHANGED);
    return;
}

void alinkview__RecordAudio(self)
struct alinkview *self; {
    RecordAudio(self, 0);
}

static int OpenForPlay(self, device) 
struct alinkview *self;
char *device; {
    int audio_fd;
    long gain;
    boolean earphones;
    extern int errno;

    audio_fd = open(AUDIO_DEV, O_WRONLY | O_NDELAY);
    if (audio_fd < 0) {
	if (errno == EBUSY)
	    message_DisplayString(self, 10, "The audio device is busy.");
	else
	    message_DisplayString(self, 10, 
				  "The audio device can't be opened.");
	return audio_fd;
    }
    if (gain = environ_GetProfileInt("PlayLevel", 50))
	SetPlayGain(gain);
    if (earphones = environ_GetProfileSwitch("Earphones", FALSE))
	SetEarPhones(earphones);

    return audio_fd;

}

static void SetPlayGain(gain)
long gain; {
    int audio_ctl_fd;
    audio_info_t audio_info;

    if ((audio_ctl_fd = open(AUDIO_CTL, O_RDWR)) < 0)
	return;
    AUDIO_INITINFO(&audio_info);

    /* gain must in the range 0 - 99 */
    if (gain > 99)
	gain = 99;
    else if (gain < 0)
	gain = 0;

    /* scale gain from 0 to 99 to 0 to AUDIO_MAX_GAIN */
    gain = (int) (((float)gain / 99.0) 
			* ((float) AUDIO_MAX_GAIN));
    audio_info.play.gain = gain;
    ioctl(audio_ctl_fd, AUDIO_SETINFO, &audio_info);

    close(audio_ctl_fd);
}

static void SetEarPhones(earphones)
boolean earphones; {

    int audio_ctl_fd;
    audio_info_t audio_info;

    if ((audio_ctl_fd = open(AUDIO_CTL, O_RDWR)) < 0)
	return;
    AUDIO_INITINFO(&audio_info);
    if (earphones) 
	audio_info.play.port = AUDIO_HEADPHONE;
    else
	audio_info.play.port = AUDIO_SPEAKER;
    ioctl(audio_ctl_fd, AUDIO_SETINFO, &audio_info);
}


static int OpenForRecord(self, device) 
struct alinkview *self;
char *device; {
    int audio_fd;
    long gain;
    extern int errno;

    audio_fd = open(AUDIO_DEV, O_RDONLY | O_NDELAY);
    if (audio_fd < 0) {
	if (errno == EBUSY)
	    message_DisplayString(self, 10, "The audio device is busy.");
	else
	    message_DisplayString(self, 10, 
				  "The audio device can't be opened.");
	return audio_fd;
    }

    if (gain = environ_GetProfileInt("RecordLevel", 50))
	SetRecordGain(gain);

    return audio_fd;

}


static void SetRecordGain(gain)
long gain; {
    int audio_ctl_fd;
    audio_info_t audio_info;

    if ((audio_ctl_fd = open(AUDIO_CTL, O_RDWR)) < 0)
	return;
    AUDIO_INITINFO(&audio_info);

    /* gain must be in the range 0 - 99 */
    if (gain > 99)
	gain = 99;
    else if (gain < 0)
	gain = 0;

    /* scale gain from 0 to 99 to 0 to AUDIO_MAX_GAIN */
    gain = (int) (((float)gain / 99.0) 
			  * ((float) AUDIO_MAX_GAIN));
    audio_info.record.gain = gain;
    ioctl(audio_ctl_fd, AUDIO_SETINFO, &audio_info);

    close(audio_ctl_fd);
}

