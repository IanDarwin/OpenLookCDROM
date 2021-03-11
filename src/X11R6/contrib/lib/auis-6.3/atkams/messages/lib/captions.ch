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


 

/* Definitions of the icons as characters */

#define ICON_MAIL 'c'
#define ICON_READMAIL 'f'
#define ICON_DUP 'e'

#define ICON_MARK 'j'
#define ICON_FOLDER 'r'

#define ICON_SEE 'g' /* Not currently used */
#define ICON_HELP 'h'
#define ICON_DELMAIL 'o'
#define ICON_POST 't'
#define ICON_DELPOST 'i'

#define ICON_READPOST 'n'
#define ICON_READDELMAIL 's'
#define ICON_READDELPOST 'b'

#define ICON_SUB_NONE 'B'
#define ICON_SUB_NORM 'C'
#define ICON_SUB_PRINT 'D'
#define ICON_SUB_ASK 'E'
#define ICON_SUB_ALL 'F'

/* Definitions of the icons as strings */

#define SICON_MAIL "c"
#define SICON_READMAIL "f"
#define SICON_DUP "e"

#define SICON_MARK "j"
#define SICON_FOLDER "r"

#define SICON_SEE "g" /* Not currently used */
#define SICON_HELP "h"
#define SICON_DELMAIL "o"
#define SICON_POST "t"
#define SICON_DELPOST "i"

#define SICON_READPOST "n"
#define SICON_READDELMAIL "s"
#define SICON_READDELPOST "b"

#define SICON_SUB_NONE "B"
#define SICON_SUB_NORM "C"
#define SICON_SUB_PRINT "D"
#define SICON_SUB_ASK "E"
#define SICON_SUB_ALL "F"

/* Constants used for the "ActOnMarked" procedures */

#define MARKACTION_DELETE 1
#define MARKACTION_UNDELETE 2
#define MARKACTION_CLASSIFYBYNAME 3
#define MARKACTION_PRINT 4
#define MARKACTION_APPENDBYNAME 5
#define MARKACTION_RESEND 6
#define MARKACTION_RESTORE 7
#define MARKACTION_EXCERPT 8
#define MARKACTION_REPLYSENDERS 9
#define MARKACTION_REPLYALL 10
#define MARKACTION_COPYBYNAME 11
#define MARKACTION_APPENDTOFILE 12
#define MARKACTION_APPENDTOFILERAW 13

struct CaptionCache {
    int offset, cuid;
    struct environment *env, *iconenv;
    char Date[AMS_DATESIZE];
    char Attributes[AMS_ATTRIBUTESIZE];
    long Chain;
    int IsMarked:1;
    int MayModify:1;
    int IsDup: 1;
};

class captions: messages {
overrides:
    PostKeyState(struct keystate *keystate);
    PostMenus(struct menulist *ml);
    Hit(enum view_MouseAction action, long x, long y, long nClicks) returns struct view *;
    GetInterface(char *interfaceName) returns char *;
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
classprocedures:
    InitializeObject(struct captions *self) returns boolean;
    FinalizeObject(struct captions *self);
    InitializeClass() returns boolean;
methods:
    ResetVisibleCaption();
    ClearAndUpdate(int ConsiderPurging, int SaveState);
    MakeCachedUpdates();
    AlterPrimaryFolderName(char *addname, char *delname);
    AlterDeletedIcon(int position, boolean delete);
    CapReverseSearch();
    DeleteVisibleMessage(boolean delete) returns int;
    DisplayNewBody(int thisCUID, linestart, linelen, struct environment *env) returns int;
    FindCUIDByDocLocation(int *position,  int *len, struct environment **envptr, int *whichcaption) returns int;
    GetBodyFromCUID(int cuid, int Mode, char *ContentTypeOverride) returns int;
    GuaranteeFetchedRange(int min, int max) returns int;
    InsertUpdatesInDocument(char *shortname, char *dname, boolean ShowFirst) returns int;
    MarkVisibleMessageStateofSeeing(boolean hasseen);
    ReportMarkedMessageCount();
    ShowMore(boolean mayscroll, boolean maygoon, boolean insistonmark) returns int;
    ToggleMark(struct CaptionCache *hc, int linestart);
    SearchAll();
    FileCurrent(char *FullName, char *NickName);
    FileMarked(char *FullName, char *NickName);
    ThisIsFlorida();
    FindRelatedMessages();
    SimulateClick(boolean IsLeftClick);
    PuntCurrent(boolean GoToNext);
    MarkRangeOfMessages();
    SendMessage(int code);
    SetFolders(struct folders *f);
    SetBodies(struct t822view *bv);
    SetLabel(char *label);
    ShowHelp();
    NewBodiesInNewWindow() returns struct t822view *;
    NewFoldersInNewWindow() returns struct folders *;
    BackUpCheckingMarks(boolean checkmarks);
    ActOnMarkedMessages(int markaction, char *string);
    GetLastClassification() returns char *;
    ClearMarks();
    AlterFileIntoMenus(boolean shrink);
    CloneMessage(int code);
    MarkCurrent();
    PrintVisibleMessage();
    Redisplay(int Mode, char *contenttype);
    GetFolders() returns struct folders *;
    GetBodView() returns struct t822view *;
macromethods:
    GetBodDoc() (struct text *) (t822view_GetDataObject(captions_GetBodView(self)))
data:
    struct t822view *BodView;
    int VisibleCUID;
    char VisibleSnapshot[AMS_SNAPSHOTSIZE];
    int HighlightStartPos;
    int HighlightLen;
    int StartOfRealBody;
    struct environment *HighlightEnv;
    struct environment *SouthPoint;
    int CurrentFormatting;
    int FolderSize, FetchedFromStart, FetchedFromEnd;
    int MarkCount;
    struct text *CaptText; 
    char *CommentText;
    int IsFullMail:1;
    struct style *ActiveCaptionStyle; /* Bold */
    struct style *NormalCaptionStyle; /* Indented */
    struct style *HighlightStyle; /* Italicize */
    struct style *GlobalCapStyle; /* for the captions doc */
    struct style *DeletedStyle; /* for deleted messages */
    struct style *ActiveDeletedStyle; /* active/deleted combination */
    struct style *IconicStyle; /* for icons from messages10 */
    struct style *UnderlinedIconicStyle; /* ditto, but underlined */
    struct style *MailStyle; /* Bigger, better? */
    struct style *FixedStyle; /* For fixed width display */
    struct scrollfns *textscrollinterface;
    struct cursor *mycursor;
    struct keystate *privkeys;
    struct menulist *privmenus;
    int OldMarkCount;
    int *OldMarks;
    struct CaptionCache *capcache;
    int captioncachecount, captioncachesize;
    char *FullName, *ShortName;
    int firstcuid, substatus;
    boolean MenusExpanded;
    struct folders *myfold;
    struct frame *myframe;
    int downdot; /* for click-drag message selections */
};
