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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/message.c,v 2.14 1993/11/24 00:34:18 gk5g Exp $";
#endif

/* message.c
 * Provides application level interface to message handler facillities.
 */

#include <andrewos.h>
#include <class.h>
#include <msghndlr.ih>
#include <view.ih>
#include <im.ih>
#include <messitem.ih>
#include <message.eh>


int message__DisplayString(classID, view, priority, string)
    struct classheader *classID;
    struct view *view;
    int priority;
    char *string;
{
    char *str = messitem_Replace(string);
    struct msghandler *handler;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }
    return msghandler_DisplayString(handler, priority, str);
}

int message__AskForString(classID, view, priority, prompt, defaultString, buffer, bufferSize)
    struct classheader *classID;
    struct view *view;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize;
{
    char *str = messitem_Replace(prompt);
    struct msghandler *handler;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }
    return msghandler_AskForString(handler, priority, str, defaultString, buffer, bufferSize);
}

int message__AskForPasswd(classID, view, priority, prompt, defaultString, buffer, bufferSize)
    struct classheader *classID;
    struct view *view;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize;
{
    char *str = messitem_Replace(prompt);
    struct msghandler *handler;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }
    return msghandler_AskForPasswd(handler, priority, str, defaultString, buffer, bufferSize);
}

int message__AskForStringCompleted(classID, view, priority, prompt, defaultString, buffer, bufferSize, keystate, completionProc, helpProc, functionData, flags)
    struct classheader *classID;
    struct view *view;
    int priority;
    char *prompt, *defaultString, *buffer;
    int bufferSize;
    struct keymap *keystate;
    procedure completionProc, helpProc;
    long functionData;
    int flags;
{
    char *str = messitem_Replace(prompt);
    struct msghandler *handler;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    handler = (struct msghandler *) view_WantHandler(view, "message");
    if (handler == NULL) {
        return -1;
    }

    return msghandler_AskForStringCompleted(handler, priority, str, defaultString, buffer, bufferSize, keystate, completionProc, helpProc, functionData, flags);
}

int message__MultipleChoiceQuestion(classID, view, priority, prompt, defaultChoice, result, choices, abbrevKeys)
    struct classheader *classID;
    struct view *view;
    int priority;
    char *prompt;
    long defaultChoice;
    long *result;
    char **choices;
    char *abbrevKeys;
{
    struct msghandler *handler;
    char *nchoices[128];
    int i = 0;
    char *str = messitem_Replace(prompt);
    while (choices && *choices) {
	nchoices[i] = messitem_Replace(*choices);
	i++;
	choices++;
    }
    nchoices[i] = NULL;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    handler = (struct msghandler *) view_WantHandler(view, "message");
    if (handler == NULL) {
        return -1;
    }

    return msghandler_MultipleChoiceQuestion(handler, priority, str, defaultChoice, result, nchoices, abbrevKeys);
}

void message__CancelQuestion(classID, view)
    struct classheader *classID;
    struct view *view;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler != NULL) {
        msghandler_CancelQuestion(handler);
    }
}
void message__Advice(classID, view,pp)
    struct classheader *classID;
    struct view *view;
    enum message_Preference pp;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler != NULL) {
        msghandler_Advice(handler,pp);
    }
}

int message__GetCurrentString(classID, view, buffer, bufferSize)
    struct classheader *classID;
    struct view *view;
    char *buffer;
    int bufferSize;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_GetCurrentString(handler, buffer, bufferSize);
}

int message__InsertCharacters(classID, view, pos, string, len)
    struct classheader *classID;
    struct view *view;
    int pos;
    char *string;
    int len;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_InsertCharacters(handler, pos, string, len);
}

int message__DeleteCharacters(classID, view, pos, len)
    struct classheader *classID;
    struct view *view;
    int pos, len;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_DeleteCharacters(handler, pos, len);
}

int message__GetCursorPos(classID, view)
    struct classheader *classID;
    struct view *view;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_GetCursorPos(handler);
}

int message__SetCursorPos(classID, view, pos)
    struct classheader *classID;
    struct view *view;
    int pos;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_SetCursorPos(handler, pos);
}

boolean message__Asking(classID, view)
    struct classheader *classID;
    struct view *view;
{

    struct msghandler *handler = (struct msghandler *) view_WantHandler(view, "message");

    if (handler == NULL) {
        return -1;
    }

    return msghandler_Asking(handler);
}
