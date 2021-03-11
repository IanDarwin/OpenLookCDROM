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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/frame/RCS/framemsg.c,v 1.27 1994/02/07 19:33:37 rr2b Exp $";
#endif


 

/* framemsg.c
 * Provides functions for frame's message line.
  */

#include <andrewos.h>
#include <class.h>
#include <im.ih>
#include <keystate.ih>
#include <view.ih>
#include <environ.ih>
#include <event.ih>
#include <msghndlr.ih>
#include <frame.ih>
#include <text.ih>
#include <textv.ih>
#include <framev.ih>
#include <style.ih>
#include <owatch.ih>

#include <environ.ih>
#include <sys/param.h>
#include <sys/stat.h>
#include <messitem.ih>
#include <framemsg.eh>

static boolean isMessFileLoaded = 0;

#define DEFAULTMESSAGETIMEOUT 15 /* Default timeout in seconds. */
static int  DIALOGPRIORITY ;
#define DEFAULTDIALOGPRIORITY 50
#define USEDIALOG(priority) (priority > DIALOGPRIORITY)
static long messageTimeout;

static char copyrightnotice[]=AUIS_SHORT_COPYRIGHT;

boolean framemessage__InitializeObject(classID, self)
    struct classheader *classID;
    struct framemessage *self;
    {
    self->erasureEvent=NULL;
    self->frame = NULL;
    self->messageText = text_New();
    text_SetObjectInsertionFlag(self->messageText,FALSE);
    text_InsertCharacters(self->messageText, 0, copyrightnotice, strlen(copyrightnotice));
    self->messageView = frameview_Create(self);
    frameview_SetDataObject(self->messageView, self->messageText);
    self->oldInputFocus = NULL;
    self->asking = FALSE;
    self->punt = FALSE;
    self->completionProc = NULL;
    self->helpProc = NULL;
    self->flags = 0;
    self->textBuffer = NULL;
    self->maxTextSize = 0;
    self->keystate = NULL;
    self->messageLen = 0;
    DIALOGPRIORITY = environ_GetProfileInt( "DialogPriority", DEFAULTDIALOGPRIORITY);
    if(DIALOGPRIORITY > 99 ) DIALOGPRIORITY = 99;
    if(DIALOGPRIORITY < 1 ) DIALOGPRIORITY = 1;
    self->companion = NULL;
    return TRUE;
}

void framemessage__FinalizeObject(classID, self)
    struct classheader *classID;
    struct framemessage *self;
{

    if (self->messageView != NULL)
        frameview_Destroy(self->messageView);
    if (self->messageText)
        text_Destroy(self->messageText);

}

struct framemessage *framemessage__Create(classID, frame)
    struct classheader *classID;
    struct frame *frame;
{

    struct framemessage *self;

    self = framemessage_New();
    self->frame = frame;
    return self;
}

static void EraseDisplayedMessage(self)
    struct framemessage *self;
{
    self->erasureEvent=NULL;
    if (self->messageLen > 0) {
        text_AlwaysDeleteCharacters(self->messageText, 0, self->messageLen);
	self->messageLen = 0;
	frameview_SetWantedLines(self->messageView, 1);
        frameview_WantUpdate(self->messageView, self->messageView);
    }
}

/* Enqueue erasure event. */
static void QueueErasure(self, length)
struct framemessage *self;
long length; /* Length of text to erase. */
{
   if(!frameview_GetIM(self->messageView)) return;
   if(self->erasureEvent) im_CancelInteractionEvent( frameview_GetIM(self->messageView), self->erasureEvent);
    self->messageLen = length;
    if(length>0)
	self->erasureEvent = im_SetInteractionEvent(frameview_GetIM(self->messageView),
			       (procedure) EraseDisplayedMessage,
			       (long) self,
						    event_SECtoTU(messageTimeout));
    else self->erasureEvent=NULL;
}

int framemessage__DisplayString(self, priority, string)
struct framemessage *self;
int priority;
char *string;
{

      int len;

      char *str = messitem_Replace(string);
      len = strlen(str);

    if (USEDIALOG(priority) ) {
	return frame_DisplayString(self->frame, priority, str);
    }

    if (framemessage_Asking(self)) {
	if(len>0){
	    if (self->messageLen != 0)
		text_AlwaysDeleteCharacters(self->messageText,
					    2,self->messageLen-5);
	    else
		text_AlwaysInsertCharacters(self->messageText,
					    0, "[  ] ", 5);
	    text_AlwaysInsertCharacters(self->messageText, 2, str, len);
	    len += 5;
	    if(text_GetFence(self->messageText) == 0)
		text_SetFence(self->messageText,len);
	}else
	    text_AlwaysDeleteCharacters(self->messageText,
					0,self->messageLen);
    } else {
	frameview_SetWantedLines(self->messageView, 1);
        text_Clear(self->messageText);
	if(len>0){
	    text_AlwaysInsertCharacters(self->messageText, 0, str, len);
	    /* Omitted setdotpos because 1) it should be at 0, 2) if it doesn't have the input focus, the dot will not be displayed. */
	    frameview_SetTopPosition(self->messageView, 0);
	}
    }

    QueueErasure(self, len);
    frameview_WantUpdate(self->messageView, self->messageView);
}

#define ERROR -1 /* For lack of a better value. */

/* Really does everything common to all "AskFor..." calls.
 * Should either be renamed, or broken into two routines. 
 */
static void BuildPrompt(self, prompt, defaultString)
    struct framemessage *self;
    char *prompt;
    char *defaultString;
{

    int len;

    text_Clear(self->messageText);
    self->messageLen = 0;
    if(frameview_GetIM(self->messageView) && self->erasureEvent) {
	im_CancelInteractionEvent(frameview_GetIM(self->messageView), self->erasureEvent);
	self->erasureEvent=NULL;
    }
    text_AlwaysInsertCharacters(self->messageText, 0, prompt, len = strlen(prompt));
    text_SetFence(self->messageText, len);
    if (!(self->flags & message_NoInitialString) && (defaultString != NULL))
        text_AlwaysInsertCharacters(self->messageText, text_GetLength(self->messageText), defaultString, strlen(defaultString));
    frameview_SetDotPosition(self->messageView, text_GetLength(self->messageText));
    frameview_SetDotLength(self->messageView, 0);
}

void CancelNoExit();

static void Process(self, buffer, bufferSize)
struct framemessage *self;
char *buffer;
int bufferSize;
{
    char *qanswer=NULL;
    qanswer=im_GetAnswer();
    if(qanswer==NULL) {
	if(!im_AnswerWasCancel()) im_KeyboardProcessor();
	else CancelNoExit(self);
	if (!self->punt) {
	    framemessage_GetCurrentString(self, buffer, bufferSize);
	    im_RecordAnswer(buffer);
	} else im_RecordCancellation();
    } else {
	strncpy(buffer, qanswer, bufferSize-1);
	buffer[bufferSize-1]='\0';
    }
}

int framemessage__AskForString(self, priority, prompt, defaultString, buffer, bufferSize)
    struct framemessage *self;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize; /* Is actual sizeof buffer including NUL. */
{
    char *qanswer = NULL;
    char *str = messitem_Replace(prompt);
    struct owatch_data *checkit=NULL;

    if (framemessage_Asking(self))
        return ERROR;

    self->flags = 0;
    self->hasDefault = (defaultString != NULL);
    
    if ( ((struct textview *) self->messageView)->editor == VI &&
	 ((struct textview *) self->messageView)->viMode == COMMAND )
	frameview_ToggleVIMode(self->messageView);

    if (USEDIALOG(priority) ) {
	return frame_AskForString(self->frame, priority, str, defaultString, buffer, bufferSize);
    }
    BuildPrompt(self, str, defaultString);

    self->oldInputFocus = im_GetInputFocus(frameview_GetIM(self->messageView));
    checkit=owatch_Create(self->oldInputFocus);
    self->asking = TRUE;

    frameview_WantInputFocus(self->messageView, self->messageView);

    frameview_SetWantedLines(self->messageView, 1);
    Process(self, buffer, bufferSize);
    
    self->asking = FALSE;

    if (owatch_CheckAndDelete(checkit))
        view_WantInputFocus(self->oldInputFocus, self->oldInputFocus);

    if (self->punt) {
        self->punt = FALSE;
        return ERROR;
    }

    QueueErasure(self, text_GetLength(self->messageText));

    return 0;
}

int framemessage__AskForPasswd(self, priority, prompt, defaultString, buffer, bufferSize)
    struct framemessage *self;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize; /* Is actual sizeof buffer including NUL. */
{
    static struct style *passwdStyle = NULL;
    struct owatch_data *checkit=NULL;
    char *str = messitem_Replace(prompt);

    if (passwdStyle == NULL) {
            passwdStyle = style_New();
	    style_AddHidden(passwdStyle);
    }
    self->flags = 0;
    self->hasDefault = (defaultString != NULL);

    if (framemessage_Asking(self))
        return ERROR;

    if (USEDIALOG(priority) ) {
	return frame_AskForPasswd(self->frame, priority, str, defaultString, buffer, bufferSize);
    }
    BuildPrompt(self, str, defaultString);

    self->oldInputFocus = im_GetInputFocus(frameview_GetIM(self->messageView));
    checkit=owatch_Create(self->oldInputFocus);
    self->asking = TRUE;

    text_AlwaysAddStyle(self->messageText,
	text_GetLength(self->messageText)-1, 1, passwdStyle);

    frameview_WantInputFocus(self->messageView, self->messageView);


    frameview_SetWantedLines(self->messageView, 1);
    Process(self, buffer, bufferSize);
    
    self->asking = FALSE;

    if (owatch_CheckAndDelete(checkit))
        view_WantInputFocus(self->oldInputFocus, self->oldInputFocus);

    if (self->punt) {
        self->punt = FALSE;
        return ERROR;
    }

    QueueErasure(self, text_GetLength(self->messageText));

    return 0;
}

/* Hairy function... */
int framemessage__AskForStringCompleted(self, priority, prompt, defaultString, buffer, bufferSize, keystate, completionProc, helpProc, completionData, flags)
    struct framemessage *self;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize; /* Is actual sizeof buffer including NUL. */
    struct keystate *keystate;
    enum message_CompletionCode (*completionProc)(/* char *string, long rock, char *buffer, int buffersize */);
    int (*helpProc)(/* char *partialKeyword, long rock, int (*helpTextFunction)(), long helpTextRockchar */);
    long completionData;
    int flags;
{

/* The order things happen in this routine is very important. */
    char *qanswer=NULL;
    struct owatch_data *checkit=NULL;
    
    char *str = messitem_Replace(prompt);

    if (framemessage_Asking(self))
	return ERROR;
    if (USEDIALOG(priority) ) {
	return frame_AskForStringCompleted(self->frame, priority, str, defaultString, buffer, bufferSize, keystate, completionProc, helpProc, completionData, flags);
    }
    self->flags = flags;
    self->hasDefault = (defaultString != NULL);

    if ( ((struct textview *) self->messageView)->editor == VI &&
	 ((struct textview *) self->messageView)->viMode == COMMAND )
	frameview_ToggleVIMode(self->messageView);

    BuildPrompt(self, str, defaultString);
    self->textBuffer = buffer;
    self->maxTextSize = bufferSize;

    if (keystate != NULL) {
        keystate_SetObject(keystate, self->messageView);
        self->keystate = keystate;
    }

    self->completionProc = completionProc;
    self->helpProc = helpProc;
    self->completionData = completionData;

    if(frameview_GetIM(self->messageView))
	self->oldInputFocus=im_GetInputFocus(frameview_GetIM(self->messageView));
    else self->oldInputFocus=NULL;
    checkit=owatch_Create(self->oldInputFocus);
    self->asking = TRUE;

    frameview_WantInputFocus(self->messageView, self->messageView);

    frameview_SetWantedLines(self->messageView, 1);
    qanswer=im_GetAnswer();
    if(qanswer==NULL) {
	if(!im_AnswerWasCancel()) im_KeyboardProcessor();
	else CancelNoExit(self);
	if (!self->punt) {
	    framemessage_GetCurrentString(self, buffer, bufferSize);
	    if ((self->flags & message_NoInitialString) && *buffer == '\0' && defaultString != NULL)
		strncpy(buffer, defaultString, bufferSize);
	    im_RecordAnswer(buffer);
	} else im_RecordCancellation();
    } else {
	strncpy(buffer, qanswer, bufferSize-1);
	buffer[bufferSize-1]='\0';
    }
    self->completionProc = NULL;
    self->helpProc = NULL;

    self->textBuffer = NULL;
    self->maxTextSize = 0;

    self->asking = FALSE;
    
    frame_RemoveHelp(self->frame);
    
    if (owatch_CheckAndDelete(checkit))
        view_WantInputFocus(self->oldInputFocus, self->oldInputFocus);

    self->keystate = NULL;
    if (self->punt) {
        self->punt = FALSE;
        return ERROR;
    }

    QueueErasure(self, text_GetLength(self->messageText));

    return 0;
}
#ifdef THISCODEWORKS
/* This routine kludges together a linear prompt for a multiple choice question. */
static char *KludgePrompt(prompt, defaultChoice, choices, abbrevKeys)
    char *prompt;
    long defaultChoice;
    char *choices[];
    char *abbrevKeys;
{

    int i;
    long size = strlen(prompt) + 1;
    char *multipleChoicePrompt, *promptBuffer;

    for (i = 0; choices[i] != NULL; i++) {

        long len = strlen(choices[i]);

        if (len == 0)
            return NULL;
        size += len + 2; /* 2 for ", " */
        if (abbrevKeys != NULL) {
            if (abbrevKeys[i] == '\0')
                return NULL;
            if (abbrevKeys[i] == *choices[i]) /* Key equals first char of choice. */
                size += 2; /* 2 for "()" arround first letter. */
            else
                size += 3; /* for "(x)" before choice... */
        }
    }
    if (i == 0 || defaultChoice > i) /* Trap some bogus cases. */
        return NULL;

    if (defaultChoice != -1)
        if (abbrevKeys != NULL)
            size += 4; /* for "[x]?" at end. */
        else
            size += 3 + strlen(choices[defaultChoice]); /* for "[choice]?" at end. */

    promptBuffer = multipleChoicePrompt = (char *) malloc(size + 1);
    strcpy(promptBuffer, prompt);
    promptBuffer += strlen(multipleChoicePrompt);

    /* This crap fails if a choice is "". I trap this above. */
    for (i = 0; choices[i] != NULL; i++) {
        if (abbrevKeys != NULL) {
            *promptBuffer++ = '(';
            *promptBuffer++ = abbrevKeys[i];
            *promptBuffer++ = ')';
            strcat(promptBuffer, choices[i] + (abbrevKeys[i] == *choices[i]) ? 1 : 0);
        }
        else
            strcat(promptBuffer, choices[i]);
        strcat(promptBuffer, ", ");
        promptBuffer += strlen(promptBuffer);
    }

    if (defaultChoice != -1) {
        *promptBuffer++ = '[';
        if (abbrevKeys != NULL)
            *promptBuffer++ = abbrevKeys[i];
        else
            strcat(promptBuffer, choices[i]);
        *promptBuffer++ = ']';
        *promptBuffer++ = '?';
    }
    else { /* Mungify the ", " after the last item. */
        *--promptBuffer = '?';
        *--promptBuffer = ' ';
    }
}
#endif /* THISCODEWORKS */

int framemessage__MultipleChoiceQuestion(self, priority, prompt, defaultChoice, result, choices, abbrevKeys)
    struct framemessage *self;
    int priority;
    char *prompt;
    long defaultChoice;
    long *result;
    char **choices;
    char *abbrevKeys;
{
	struct owatch_data *checkit=NULL;
#ifdef THISCODEWORKS /* This is saracastic I assume (i.e. really should be #if 0). */
    char *tempPrompt;
#endif /* THISCODEWORKS  */

      char *str = messitem_Replace(prompt);

    self->flags = 0;

    if (framemessage_Asking(self))
        return ERROR;
#ifdef THISCODEWORKS
    if (USEDIALOG(priority) ) {
	return frame_MultipleChoiceQuestion(self->frame, priority, str, defaultChoice, result, choices, abbrevKeys);
    }

    if ((tempPrompt = KludgePrompt(str, defaultChoice, choices, abbrevKeys)) == NULL)
        return ERROR;

    /* if VI user, put them in input mode if necessary */
    if ( (self->messageView)->editor == VI && (self->messageView)->viMode == COMMAND )
	frameview_ToggleVIMode(self->messageView);

    BuildPrompt(self, tempPrompt, "");

    self->oldInputFocus = im_GetInputFocus(frameview_GetIM(self->messageView));
    checkit=owatch_Create(self->oldInputFocus);
    
    self->asking = TRUE;

    frameview_WantInputFocus(self->messageView, self->messageView);
    
    /* if this code is to be used it should do something about the answer queue in im */
    im_KeyboardProcessor();

    free(tempPrompt);

    if (!self->punt)
        ; /* Should assign result with appropriate thing. */

    self->asking = FALSE;

    if (owatch_CheckAndDelete(checkit))
        view_WantInputFocus(self->oldInputFocus, self->oldInputFocus);

    if (self->punt) {
        self->punt = FALSE;
        return ERROR;
    }

    QueueErasure(self, text_GetLength(self->messageText));

    return 0;
#else /* THISCODEWORKS */
	return frame_MultipleChoiceQuestion(self->frame, priority, str, defaultChoice, result, choices, abbrevKeys);
#endif /* THISCODEWORKS */
}

void CancelNoExit(self)
struct framemessage *self;
{
    if (framemessage_Asking(self)) {
	self->punt = TRUE;
	self->asking = FALSE; /* In case we get a display string before the interact falls through. */
    }
    else if(self->companion) msghandler_CancelQuestion(self->companion);
}

void framemessage__CancelQuestion(self)
    struct framemessage *self;
{

    if (framemessage_Asking(self)) {
        self->punt = TRUE;
        self->asking = FALSE; /* In case we get a display string before the interact falls through. */
        im_KeyboardExit();
    }
    else if(self->companion) msghandler_CancelQuestion(self->companion);
}

int framemessage__GetCurrentString(self, buffer, bufferSize)
    struct framemessage *self;
    char *buffer;
    int bufferSize; /* Is actual sizeof buffer including NUL. */
{

    int pos, length;

    if (!framemessage_Asking(self)){
	if(self->companion) 
	    return msghandler_GetCurrentString(self->companion, buffer, bufferSize);
	return NULL;
    }
    pos = text_GetFence(self->messageText);
    length = text_GetLength(self->messageText);
    if (bufferSize <= (length - pos))
        length = pos + bufferSize - 1;
    while (pos < length)
        *buffer++ = text_GetChar(self->messageText, pos++);
    *buffer = '\0';
    return frameview_GetDotPosition(self->messageView) - pos;
}

int framemessage__InsertCharacters(self, pos, string, len)
    struct framemessage *self;
    int pos;
    char *string;
    int len;
{
    if (!framemessage_Asking(self)){
	if(self->companion) 
	    return msghandler_InsertCharacters(self->companion, pos, string, len);
        return ERROR;
    }
    pos += text_GetFence(self->messageText);
    text_InsertCharacters(self->messageText, pos, string, len);
    text_NotifyObservers(self->messageText, 0);
}

int framemessage__DeleteCharacters(self, pos, len)
    struct framemessage *self;
    int pos;
    int len;
{

    if (!framemessage_Asking(self)){
	if(self->companion) 
	    return msghandler_DeleteCharacters(self->companion, pos, len);
        return ERROR;
    }

    frameview_SetWantedLines(self->messageView, 1);
    pos += text_GetFence(self->messageText);
    text_DeleteCharacters(self->messageText, pos, len);
    text_NotifyObservers(self->messageText, 0);
}

int framemessage__GetCursorPos(self)
    struct framemessage *self;
{
    if (!framemessage_Asking(self)){
	if(self->companion) 
	    return msghandler_GetCursorPos(self->companion);
        return ERROR;
    }

    return frameview_GetDotPosition(self->messageView) - text_GetFence(self->messageText);
}

int framemessage__SetCursorPos(self, pos)
    struct framemessage *self;
    int pos;
{

    if (!framemessage_Asking(self)){
	if(self->companion) 
	    return msghandler_SetCursorPos(self->companion,pos );
        return ERROR;
    }

    pos += text_GetFence(self->messageText);
    frameview_SetDotPosition(self->messageView, pos);
    frameview_SetDotLength(self->messageView, 0);
    frameview_WantUpdate(self->messageView, self->messageView);
}

boolean framemessage__Asking(self)
    struct framemessage *self;
{

    return self->asking;
}
void framemessage__SetCompanion(self,companion)
struct framemessage *self;
struct msghandler *companion;
{
    self->companion = companion;
}

boolean framemessage__InitializeClass(classID)
    struct classheader *classID;
{
    messageTimeout = environ_GetProfileInt("MessageTimeout", DEFAULTMESSAGETIMEOUT); /* Expressed in seconds. */

      messitem_InitMessFile();

    return TRUE;
}

void framemessage__Advice(self,pp)
struct framemessage *self;
enum message_Preference pp;
{
    frame_Advice(self->frame,pp);
}
