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


 

#define SM_STATE_NOSTATE -1
#define SM_STATE_READY 0
#define SM_STATE_INPROGRESS 1
#define SM_STATE_SENDING 4
#define SM_STATE_SENT 5
#define SM_STATE_VALIDATING 6
#define SM_STATE_VALIDATED 7
#define SM_STATE_VALIDATEFAILED 8

#define SM_RESET 0
#define SM_SIGN 1
#define SM_HIDE 2
#define SM_CLEAR 3
#define SM_BLIND 4

class sendmessage[sendmsg]: view {
overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    PostKeyState(struct keystate *keystate);
    PostMenus(struct menulist *menulist);
    WantUpdate(struct view *v);
    LinkTree(struct view *parent);
    UnlinkTree();
classprocedures:
    InitializeObject(struct sendmessage *self) returns boolean;
    FinalizeObject(struct sendmessage *self);
    InitializeClass() returns boolean;
methods:
    Clear() returns int ;
    ReadFromFile(char *SourceFile, Boolean DeleteIt) returns int ;
    Reset();
    AddHeaderLine(char *line) returns int ;
    AddToToHeader(char *line) returns int ;
    ResetSendingDot() returns int ;
    ResetFromParameters(char *ToName, char *Subject, char *CC, char *IncludeFile, int Delete) returns int ;
    Checkpoint() returns int ;
    WriteFile(char *FileName) returns int ;
    SetFoldersView(struct messages *mess) returns int ;
    CheckButtons();
    HasChanged() returns int;
    SetCurrentState(int state);
    AppendBugInfoToBody(int IsMessagesBug) returns int;
    AskEraseUnsentMail() returns int;
    QuoteBody();
    CheckRecipients() returns int;
    NewFoldersInNewWindow() returns struct folders *;
    NeedsCheckpointing() returns int;
data:
    struct lpair *SendLpair;
    struct scroll *HeadScroll, *BodyScroll;
    struct textview *HeadTextview, *BodyTextview;
    struct text *HeadText, *BodyText;
    struct sbutton_prefs *prefs;
    struct sbuttonv *buttons;
    /*
    struct butter *bclear, *bhide, *bblind, *breset, *bsign;
    struct butterview *bcv, *bhv, *bbv, *brv, *bsv; */
    struct style *DefaultStyle, *DefaultHeadStyle, *BoldStyle;
    struct folders *folders;
    struct menulist *mymenulist, *myheadmenulist;
    struct keystate *keys, *headkeys;
    long HeadModified, BodyModified; /* stored mod #'s for texts */
    int HeadCheckpoint, BodyCheckpoint;  /* Value of above at time of last checkpoint */
    int NeedsTemplate, CurrentState;
    char *CKPFileName, *PSMsg;
    struct frame *myframe;
    int HasSigned;
    struct lpair **randomlpairs;
};

