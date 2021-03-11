/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/* framemsg.H
 * Class for the frame's message line abstraction.
 *
 */

/* All buffer size arguments in this module are as returned by sizeof(buffer).
 * That is, they include the NUL at the end of the string.
 */
class framemessage[framemsg]: msghandler[msghndlr] {
    overrides:
        DisplayString(int priority, char *string) returns int;
        AskForString(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize) returns int;
        AskForPasswd(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize) returns int;
        AskForStringCompleted(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize, struct keystate *keystate,
                              procedure completionProc, procedure helpProc, long functionData, int flags) returns int;
        MultipleChoiceQuestion(int priority, char *prompt, long defaultChoice, long *result, char **choices, char *abbrevKeys) returns int;
        CancelQuestion();
/* These next calls are only applicable during an AskFor... operation. */
        GetCurrentString(char *buffer, int bufferSize) returns int;
        InsertCharacters(int pos, char *string, int len) returns int;
        DeleteCharacters(int pos, int len) returns int;
        GetCursorPos() returns int;
        SetCursorPos(int pos) returns int;
        Asking() returns boolean;
	Advice(enum message_Preference);

	methods:
	SetCompanion(struct msghandler *companion);

    classprocedures:
        FinalizeObject(struct framemessage *self);
        Create(struct frame *myFrame) returns struct framemessage *;
        InitializeClass() returns boolean;
	InitializeObject(struct framemessage *self) returns boolean;    

    data:
        struct msghandler *companion;
        struct frame *frame;
        struct frameview *messageView;
        struct text *messageText;
        struct view *oldInputFocus;
        struct buffer *realBuffer;
        long messageLen; /* Length of message displayed during an AskFor operation. */
        enum message_CompletionCode (*completionProc)();
        int (*helpProc)();
        long completionData;
        char *textBuffer;
        int maxTextSize;
        struct keystate *keystate;
        struct im_InteractionEvent *erasureEvent;
        char flags;
        char asking;
        char punt;
        char hasDefault; /* TRUE if defaultString in nonNULL. */
};
