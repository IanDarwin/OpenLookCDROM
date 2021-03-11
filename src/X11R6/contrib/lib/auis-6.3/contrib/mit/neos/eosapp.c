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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosapp.c,v 1.5 1993/07/29 16:05:45 rr2b Exp $";
#endif


 
/*
 * eosapp.c
 *
 * EOS application start-up.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>
#include <class.h>
#include <buffer.ih>
#include <environ.ih>
#include <eosapp.eh>
#include <eos.ih>
#include <dataobj.ih>
#include <view.ih>
#include <frame.ih>
#include <im.ih>
#include <eos.h>
#include <text.ih>
#include <cursor.ih>
#include <message.ih>
#include <rect.h>
#include <event.ih>
#include <proctbl.ih>
#include <eosvers.h>

static char course[33];
/* the following two declarations used to be static, but Bill Cattey says that breaks something on the Sun. I don't understand, but it's his problem. -ap1i */
long eos_CkpInterval; /* How often to run Checkpoint routine. */
long eos_CkpLatency; /* The minimum amount of time to wait to checkpoint a buffer. */
#define EOS_DEF_CKPINTERVAL 30 /* Default for CkpInterval. */
#define EOS_DEF_CKPLATENCY 4 /* Default fo CkpLatency. */

static struct cursor *eos_waitCursor;

/* The checkpointing algorithm used here is copied from eza.c
 * (Copyright IBM Corporation 1988,1989 - All Rights Reserved *
 *        For full copyright information see:'andrew/config/COPYRITE').
 * It based on the following notions:
 *
 * 1) At Most, one buffer will be checkpointed every T seconds.
 * 2) At most, a given buffer will be checkpointed once in an (a * T) second
 *    interval.
 * 3) Only buffers which have been modified since the last time they were
 *    checkpointed (or written) are checkpointed.
 * 4) All buffers that meet criterion 3 will be checkpointed before any buffer is
 *    checkpointed twice.
 * 5) Scratch buffers are not checkpointed.
 *
 * The algorithm is implemented using two fields in the buffer structure. The
 * first is the ckpVersion field, which holds the dataobject version number as of
 * the last checkpoint (or file write). The second is the ckpClock field which is
 * reset on each checkpoint (or file write), and is incremented every T seconds.
 * To find which buffer to checkpoint, take the one with the highest ckpClock
 * greater than a. If there are none greater than a, don't checkpoint any.
 *
 * A third field in the buffer structure is used to allow the user to specify
 * the checkpointing behavior on a per buffer basis. This field is called the
 * latency and is basically the number of clock intervals above (or below if
 * negative) the system should wait before checkpointing this buffer. The code
 * is somewhat hairier than it might be since the buffer package initializes
 * both the ckpClock and eos_CkpLatency  fields to 0 so we bias it by the value of
 * eos_CkpLatency. This is the price paid for generality in the buffer package.
 *
 * Currently T (eos_CkpInterval) is 30 seconds and a (eos_CkpLatency) is 4 (i.e. 2
 * minutes checkpoint latency). However, a can be changed on a per
 * buffer basis.
 */

struct bestbuffer {
    struct buffer *buffer;
    long bufferclock;
};

boolean eos_FindCkpBuffer(buffer, best)
    struct buffer *buffer;
    struct bestbuffer *best;
{

    if (!buffer_GetScratch(buffer) && dataobject_GetModified(buffer_GetData(buffer)) > buffer_GetCkpVersion(buffer)) {
        if (buffer_GetCkpClock(buffer) > best->bufferclock) {
            best->buffer = buffer;
            best->bufferclock = buffer_GetCkpClock(buffer);
        }
        buffer_SetCkpClock(buffer, buffer_GetCkpClock(buffer) + 1);
    }
    return FALSE;
}

#define view_Visible(view) (!rectangle_IsEmptyRect(&(((struct graphic *) view)->visualBounds)))

boolean eos_CkpMessage(applicationView, targetView, inputFocusView, message)
    struct view *applicationView, *targetView, *inputFocusView;
    char *message;
{
    if (inputFocusView == NULL) /* Handles case where input focus is not set... */
        inputFocusView = targetView;

    if (view_Visible(inputFocusView)) {
        message_DisplayString(inputFocusView, 0, message);
        return TRUE;
    }
    return FALSE;
}

void eos_Checkpoint(dummyData)
    long dummyData;
{
    struct bestbuffer result;

    result.buffer = NULL;
    result.bufferclock = eos_CkpLatency - 1; /* (number + 1) * EOS_CKPINTERVAL seconds is how often a given buffer can be checkpointed. */

    buffer_Enumerate(eos_FindCkpBuffer, (long) &result);
    if (result.buffer != NULL) {

        int closeCode;

        im_SetProcessCursor(eos_waitCursor);
        if (buffer_Visible(result.buffer))
            buffer_EnumerateViews(result.buffer, eos_CkpMessage, (long) "Checkpointing...");
        im_ForceUpdate();

        if ((closeCode = buffer_WriteToFile(result.buffer, buffer_GetCkpFilename(result.buffer), 0)) >= 0) {
            buffer_SetCkpVersion(result.buffer, dataobject_GetModified(buffer_GetData(result.buffer)));
            buffer_SetCkpClock(result.buffer, buffer_GetCkpLatency(result.buffer));
        }

        if (buffer_Visible(result.buffer))
            buffer_EnumerateViews(result.buffer, eos_CkpMessage, (long)(closeCode ? "Checkpoint Failed." : "Checkpointed."));
        im_SetProcessCursor(NULL);
    }
    im_EnqueueEvent((procedure) eos_Checkpoint, 0, event_SECtoTU(eos_CkpInterval));
}

void eos_SetBufferCkpLatency(frame, key)
    struct frame *frame;
    long key;
{

    struct buffer *buffer;
    char answer[20];

    if ((buffer = frame_GetBuffer(frame)) == NULL)
        return;

/* This stuff is a little hairy. Basically, it is converting a latency number to
 * seconds and back again. A latency number is shifted from 0, or specifically,
 * the code waits (latency + eos_CkpLatency) * eos_CkpInterval seconds before
 * considering checkpointing this buffer. 
 */
    sprintf(answer, "%d", (eos_CkpLatency - buffer_GetCkpLatency(buffer)) * eos_CkpInterval);
    if (message_AskForString(frame, 0, "Minimum checkpoint time in seconds: ", answer, answer, sizeof(answer)) != -1) {

        long latencyIntervals = atoi(answer) / eos_CkpInterval;

        buffer_SetCkpLatency(buffer, eos_CkpLatency - latencyIntervals);
        buffer_SetCkpClock(buffer, eos_CkpLatency - latencyIntervals); /* Force new checkpoint time to take effect now. */
    }
}

static void eos_StartupError(error, string)
    struct text *error;
    char *string;
{
    text_InsertCharacters(error, text_GetLength(error), string, strlen(string));
}

static void eos_addFile(self,name)
struct eosapp *self;
char *name;
{
    /* Its a file right? */
    struct eosapp_fileList *fileEntry=
      (struct eosapp_fileList *) malloc(sizeof(struct eosapp_fileList));

    fileEntry->filename=name;
    fileEntry->ObjectName=self->defaultObject;
    fileEntry->next=NULL;
    *self->fileLink=fileEntry;
    self->fileLink=(&(fileEntry->next));
}

static void eos_makeErrorBuf(self)
struct eosapp *self;
{
    self->errorBuffer = buffer_Create("Startup-Errors", NULL, "text", NULL);
    buffer_SetScratch(self->errorBuffer, TRUE);
}

void eosapp__ReadInitFile(self)
struct eosapp *self;
{
    eos_makeErrorBuf(self);

    eosapp_SetErrorProc(self,eos_StartupError);
    eosapp_SetErrorRock(self,(pointer)buffer_GetData(self->errorBuffer));

    super_ReadInitFile(self);
}


boolean eosapp__Start(self)
struct eosapp *self;
{
    struct im *im;
    struct eos *eos;
    struct eosapp_fileList *fileEntry, *next;
    struct text *errtext;

    if (!super_Start(self))
        return FALSE;

    if (strcmp (eosapp_GetName(self), "grade") == 0)
	environ_Put("EOSTYPE", "grading");
    else
	environ_Put("EOSTYPE", "student");

    eos = eos_New();
    if (eos == NULL)
        return FALSE;

    if(self->errorBuffer==NULL)
	eos_makeErrorBuf(self);

    errtext=(struct text *)buffer_GetData(self->errorBuffer);

    buffer_SetDefaultObject(self->defaultObject);

    if(self->files==NULL)  {
	char *defFile;

	if ((defFile = environ_GetProfile("DefaultStartUpFile")) != NULL && *defFile != '\0')  {
	    eos_addFile(self, defFile);
	}
    }

    for (fileEntry = self->files; fileEntry != NULL; fileEntry = next) {
        if(fileEntry->ObjectName != NULL){
            if(class_IsTypeByName(fileEntry->ObjectName,"dataobject"))
                buffer_SetDefaultObject(fileEntry->ObjectName);
            else {
                char errorMessage[200];
                sprintf(errorMessage,"%s is not a known dataobject\n",fileEntry->ObjectName);
                eos_StartupError(errtext,errorMessage);
                eos->editregion = NULL;
                next = fileEntry->next;
                free(fileEntry);
                continue;
            }
        }
        eos_SetBuffer(eos, fileEntry->filename, 0);

        if(fileEntry->ObjectName != NULL)
            buffer_SetDefaultObject(self->defaultObject);

        if (eos->editregion != NULL) {
            long version = dataobject_GetModified(buffer_GetData(eos->editregion));

            buffer_SetCkpClock(eos->editregion, 0);
            buffer_SetCkpVersion(eos->editregion, version);
            buffer_SetWriteVersion(eos->editregion, version);
	}
        else {
            char errorMessage[200];
            sprintf(errorMessage, "File %s does not exist and could not be created.", fileEntry->filename);
            eos_StartupError(errtext,errorMessage);
        }
        next = fileEntry->next;
        free(fileEntry);
    }

    if (text_GetLength(errtext) != 0) {
        text_InsertCharacters(errtext, 0, "Errors encountered during startup:\n", sizeof("Errors encountered during startup:\n") - 1);
    }
    else {
        buffer_Destroy(self->errorBuffer);
        self->errorBuffer = NULL;
    }

    im = im_Create(NULL);
    if (im == NULL) {
        eos_Destroy(eos);
        return FALSE;
    }

    eos_SetProgram(eos, eosapp_GetName(self));

    if (eos->gradingflag)
	eos_SetTitle(eos, "GRADE: Editor");
    else
	eos_SetTitle(eos, "EOS Editor");

    frame_PostDefaultHandler(eos->frame, "message", frame_WantHandler(eos->frame, "message"));
    if (self->errorBuffer)
        frame_SetBuffer(eos->frame, self->errorBuffer, TRUE);

    im_SetView(im, eos);
    eos_WantInputFocus(eos, eos);
    eos_waitCursor = cursor_Create(NULL);
    cursor_SetStandard(eos_waitCursor, Cursor_Wait);
    if (*course) {
	eos_SetCourse(eos, course);
    } else
	message_DisplayString(eos->frame, 0, "Warning: no course defined");
    return TRUE;
}

boolean eosapp__InitializeObject(classID, self)
struct classheader *classID;
struct eosapp *self;
{
    eosapp_SetMajorVersion(self, EOS_MAJORVERSION);
    eosapp_SetMinorVersion(self, EOS_MINORVERSION);
    self->files = NULL;
    self->fileLink = &self->files;
    self->errorBuffer = NULL;
    self->defaultObject = NULL;
    return TRUE;
}


boolean eosapp__ParseArgs(self, argc, argv)
struct eosapp *self;
int argc;
char **argv;
{
    boolean waiting_for_course  = FALSE;
    char *t; /* temporary string */

    t = environ_Get("COURSE");
    if (t)
	strncpy(course, t, 32);

    if (!super_ParseArgs(self, argc, argv))
      return FALSE;

    while (*++argv != NULL) {
        if (**argv == '-')
            switch ((*argv)[1]) {
            case 'c':
                waiting_for_course = TRUE;
                break;
            default:
                fprintf(stderr, "Unrecognised switch: -%c\n", (*argv)[1]);
                fprintf(stderr, "Usage: %s [-c course]\n", eosapp_GetName(self));
                return FALSE;
            }
        else {
            if (waiting_for_course) {
                if (strlen(*argv) >32) {
                    strncpy(course, *argv, 32);
                    course[32] = '\0';
                } else
                    strncpy(course, *argv, 32);
                waiting_for_course = FALSE;
            } else
                eos_addFile(self, *argv); 
        }
    }

    return TRUE;
}
                
int eosapp__Run(self)
struct eosapp *self;
{
 
    if(!eosapp_Fork(self))
	return -1;

/* A CheckPointInterval of 0 means don't checkpoint. */
    if ((eos_CkpInterval = environ_GetProfileInt("CheckpointInterval", EOS_DEF_CKPINTERVAL)) != 0) {
        eos_CkpLatency = environ_GetProfileInt("CheckpointMinimum", EOS_DEF_CKPLATENCY * eos_CkpInterval) / eos_CkpInterval;
        im_EnqueueEvent((procedure) eos_Checkpoint, 0, event_SECtoTU(eos_CkpInterval));
    }

    im_KeyboardProcessor();

    return 0;
}

boolean eosapp__InitializeClass()
{

    proctable_DefineProc("eosapp-set-buffer-checkpoint-latency", eos_SetBufferCkpLatency, class_Load("frame"), NULL, "Set the number of checkpoint intervals to wait before checkpointing the current buffer.");

    return TRUE;
}
