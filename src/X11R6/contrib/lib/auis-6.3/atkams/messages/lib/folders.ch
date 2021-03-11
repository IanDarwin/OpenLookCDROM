/***********************************************************
		Copyright IBM Corporation 1991

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

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


 

#define TEMPFAILSTRING " (Temporarily unavailable) \n"

#define BADFAILSTRING " (Error -- Directory unreadable) \n"

#define LIST_NEWONLY 0
#define LIST_SUBSCRIBED 1
#define LIST_MAIL_FOLDERS 2
#define LIST_ALL_FOLDERS 3
#define LIST_AS_REQUESTED 4

struct BEDirCache {
    struct environment *env;
    int pos, len, commentstart, commentlen, substatus, SkipMe;
    char *FullName, *ShortName;
};

class folders: messages {
overrides:
    PostKeyState(struct keystate *keystate);
    PostMenus(struct menulist *ml);
    Hit(enum view_MouseAction action, long x, long y, long nClicks) returns struct view *;
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    GetApplicationLayer() returns struct view *;
    DeleteApplicationLayer(struct view *scrollbar);
classprocedures:
    InitializeObject(struct folders *self) returns boolean;
    FinalizeObject(struct folders *self);
    InitializeClass() returns boolean;
methods:
    HandleAsyncPrefetch();
    AlterSubscriptionStatus(char *dir, char *shortname, int status) returns int;
    AlterFolderNames(char *name, char *nick, boolean doinsert);
    ActionHit(int substatus, char *fullname, char *nickname);
    HighlightFolder(char *name, char *comment);
    SimulateClick(boolean IsLeftClick);
    FindNextFolder(int *which, int *skipped);
    Reconfigure(int listcode);
    UpdateMsgs(int code, char *thingstoread[], boolean ShowHelp);
    NextFolder(boolean ShowFirst);
    SetCaptions(struct captions *c);
    SetVeryNarrow(boolean isnarrow);
    ExplainDir(char *fullname, char *nickname);
    SetSkip(char *fullname, boolean DoSkip);
    ExposeSend() returns struct sendmessage *;
    GrowWindow();
    WriteFormattedBodyFile(char *fname, char *captbuf) returns int;
    FolderOnDisplay(char *shortname, char *longname);
    ShowHelp();
    NewCaptionsInNewWindow() returns struct captions *;
    SetSendmessage(struct sendmessage *sm);
    ReadMail(boolean CheckMailbox);
    GetCaptions() returns struct captions *;
data:
    struct environment *HighlightEnv;
    struct BEDirCache *MainDirCache;
    int MainDirCacheSize, MainDirCacheCount;
    int HasSetUp:1;
    int ShowingAsRequested:1;
    struct style *Activefolderstyle; /* Bold */
    struct style *Normalfolderstyle; /* Indented */
    struct style *GlobalCapStyle; /* for the folders doc */
    struct style *IconicStyle; /* for icons from messages10 */
    struct style *BoldStyle;
    struct style *ItalicStyle;
    struct style *CenterStyle;
    struct style *BigCenterStyle;
    struct style *SmallCenterStyle;
    struct cursor *mycursor;
    struct keystate *mykeys;
    struct menulist *mymenulist;
    struct captions *mycaps;
    struct lpair *puntlp;
    int MailOnlyMode, CurrentConfiguration, VeryNarrow;
    struct sendmessage *sm;
    struct sbuttonv *buttons;
    struct scroll *myscroll;
    struct frame *myframe;
    struct sbutton_prefs *prefs;
    int NeedsToPrefetch;
};

