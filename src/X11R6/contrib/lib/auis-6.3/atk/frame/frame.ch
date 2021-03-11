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


 


#define frame_VERSION 1

#define frame_MUSTANSWER -1

#define	frame_DefaultMenus	(1<<13)
#define	frame_BufferMenus	(1<<12)


#include <rect.h>

#include <message.ih>

class frame: lpair {
    methods:
        SetView(struct view *view);
        SetBuffer(struct buffer *buffer, boolean setInputFocus);
        SetCommandEnable(boolean enable);
        GetHelpBuffer() returns struct buffer *;
        SetHelpBuffer(struct buffer *buf, boolean setInputFocus) returns struct buffer *;
        SetTitle(char *title);
	Advice(enum message_Preference pp);
        DisplayString(int priority, char *string) returns int;
        AskForString(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize) returns int;
        AskForPasswd(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize) returns int;
        AskForStringCompleted(int priority, char *prompt, char *defaultString, char *buffer, int bufferSize, struct keystate *keystate,
                              procedure completionProc, procedure helpProc, long functionData, int flags) returns int;
	MultipleChoiceQuestion(int priority, char *prompt, long defaultChoice, long *result, char **choices, char *abbrevKeys) returns int;
	RemoveHelp();
	SetReturnFocus(struct view *req);
	ReturnFocus();
	
macromethods:
        GetView() ((self)->targetView)
        GetChildView() ((self)->childView)
        GetBuffer() ((self)->buffer)
        GetCommandEnable() ((self)->commandEnable)
        GetTitle() ((self)->title)
        GetQuitWindowFlag() ((self)->QuitWindowOnly)
        SetQuitWindowFlag(VAL) (((self)->QuitWindowOnly) = VAL)

    classprocedures:
        InitializeObject(struct frame *self) returns boolean;
        FinalizeObject(struct frame *self);
	Enumerate(procedure function, long functionData) returns struct frame *;
	FindFrameForBuffer(struct buffer *b) returns struct frame *;
	GetFrameInWindowForBuffer(struct buffer *b) returns struct frame *;
	Create(struct buffer *buffer) returns struct frame *;
	InitializeClass()returns boolean;
    overrides: /* Make it hard for people to access our lpair functions. */
        Update();
        FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
        WantHandler(char *handlerName) returns struct basicobject *;
        PostKeyState(struct keystate *keystate);
        PostMenus(struct menulist *menulist);
        ObservedChanged(struct observable *changed, long value);
	Hit(enum view_MouseAction action, long x, long y, long nclicks) returns struct view *;
	WantUpdate(struct view *v);
	WantNewSize(struct view *requestor);
	LinkTree(struct view *parent);
	
    data:
        struct frame *next;
        struct buffer *buffer;
        struct view *targetView;
        struct view *childView;
        char *title;
        struct text *messageText;
        struct frameview *messageView;
        struct framemessage *messageLine;
        int lineHeight; /* Height in pixels of the nominal one line message line. */
        struct frameview *dialogView;
        struct framemessage *dialogLine;
        struct keystate *keystate;
        struct menulist *menulist;
        char helpBuffer[16]; /* 16 is greater than sizeof("Help-Buffer-99") */
        boolean commandEnable;
	struct buffer *DialogBuffer;
	struct view *DialogTargetView;
	struct view *DialogBufferView;

    struct cursor *octcursor,*arrowcursor;
    struct fontdesc *myfontdesc;
    struct menulist *mymenus;
    struct keystate *mykeystate;
    enum message_Preference PositionalPreference;
    char **MultipleAnswers;
    struct rectangle AnswerBox;
    struct rectangle *HeightsOfAnswer;
    struct rectangle mesrec,bufferrec;
    struct pendingupdates *uplist;
    int IsAsking,
	IsBlocking,
	WildestAnswer, /* selected answer */
	DefaultWildestAnswer, /* default answer */
	StackPos, 
        hasDialogMessage,
	NumAnswerChoices,
        AwaitingFocus,
        UpdateRequested,
        buttonmaxwid;
        struct dataobject *object;
        boolean dataModified;
    boolean QuitWindowOnly;
    boolean drawn;
    boolean mono;
    int PotentialChoice;
    boolean UsingDialog, UseBuiltinDialogs;
    struct dialogv *dv;
    struct menulist *defaultmenulist;
    struct buffer *realBuffer;
    struct view *realView;
    boolean revertToReal;
    int height;
    struct view *returnFocus;
    struct buffer *deleteTarget;
};
