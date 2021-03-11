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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/messages.c,v 2.53 1994/02/04 18:37:00 Zarf Exp $";
#endif


 

#include <andrewos.h>
#include <sys/param.h>
#include <cui.h>
#include <fdphack.h>
#include <errprntf.h>
#include <class.h>
#include <message.ih>
#include <complete.ih>
/* #include <keymap.ih> */
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <proctbl.ih>
#include <im.ih>
#include <text.ih>
#include <frame.ih>
#include <environ.ih>
#include <search.ih>

#include <ams.ih>
#include <amsutil.ih>

#include <text822v.ih>

#define dontDefineRoutinesFor_text822
#include <text822.ih>
#undef dontDefineRoutinesFor_text822

#include <captions.ih>
/* #include <options.ih> */
#include <messages.eh>

#ifndef NOTREESPLEASE
#include <org.ih>
#include <fldtreev.ih>
#define dontDefineRoutinesFor_tree
#include <tree.ih>
#undef dontDefineRoutinesFor_tree
#endif

extern		void AlterSubByName();
extern		void AppendMarked();
extern		void AppendMarkedToFile();
extern		AppendMessageToFile();
extern		void AppendMarkedToRawFile();
extern		AppendMessageToRawFile();
extern		void BSM_AppendDelPlease();
extern		void BSM_AppendOnto();
extern		void BSM_AppendPlease();
extern          void BSM_CheckNewPlease();
extern		void BSM_ClassifyPlease();
extern		void BSM_CopyPlease();
extern		void BSM_CreatePlease();
extern		void BSM_DeleteFolder();
extern		void BSM_DeletePlease();
extern		void BSM_DifferentContentType();
extern		void BSM_ShowRaw();
extern		void BSM_DummyQuit();
extern		void BSM_FileInto();
extern		void BSM_MarkCurrent();
extern		void BSM_ModifiableBody();
extern		void BSM_MorePlease();
extern		void BSM_NextPlease();
extern		void BSM_ReSendPlease();
extern          void BSM_ReadMailPlease();
extern		void BSM_RedisplayFixedWidth();
extern		void BSM_RedisplayFormat();
extern		void BSM_RedisplayNormal();
extern		void BSM_RedisplayRot13();
extern		void BSM_RefreshDisplayedFolder();
extern		void BSM_NextDigest();
extern		void BSM_PrevDigest();
extern		void BSM_RenamePlease();
extern		void BSM_ReplyToAll();
extern		void BSM_ReplyToBoth();
extern		void BSM_ReplyToReaders();
extern		void BSM_ReplyToSender();
extern		void BSM_RestoreAsDraft();
extern		void BSM_ScrollBackBody();
extern		void BSM_ScrollForwardBody();
extern		void BSM_SelectBody();
extern		void BSM_SendForward();
extern		void BSM_SendFresh();
extern		void BSM_SetPrinter();
extern          void BSM_ShowAllPlease();
extern          void BSM_ShowHelp();
extern          void BSM_ShowNewPlease();
extern          void BSM_ShowPersonalPlease();
extern          void BSM_ShowSubscribedPlease();
extern		void BSM_ShowTreePlease();
extern		void BSM_UndeletePlease();
extern		void BSearchFPlease();
extern		void BSearchRPlease();
extern		void BackUp();
extern		void CSearchFPlease();
extern		void CSearchRPlease();
extern          CheckMenuMasks();
extern		void ClassifyMarked();
extern		void ClearMarks();
extern          boolean ClearSM();
extern		void CopyMarked();
extern		void CountMarks();
extern		void DeleteMarked();
extern		void DeleteWindow();
extern		DirectlyClassify();
extern          void messages_DuplicateWindow();
extern		void ExcerptMarked();
extern		void ExpandFileIntoMenus();
extern          void FSearchFPlease();
extern          void FSearchRPlease();
extern		void FileByName();
extern		void FileIntoByName();
extern		void FileOntoByName();
extern		void FindAllCaptions();
extern		void FindRelatedMessages();
extern		void GSearchFPlease();
extern		void GSearchRPlease();
extern		GenNodeName();
extern          struct t822view *GetBodies();
extern          struct captions *GetCaptions();
extern          struct captions *GetCaptionsNoCreate();
extern		GetFolderName();
extern		char *GetLastResendName();
extern		void MarkRange();
extern		void MarkVisibleMessageUnseen();
extern		void MessagesBodiesCommand();
extern		void MessagesCaptionsCommand();
extern		void MessagesCompound();
extern		void MessagesFocusBodies();
extern		void MessagesFocusCaptions();
extern          void MessagesFocusFolders();
extern          void MessagesFoldersCommand();
extern          void MessagesSendmessageCommand();
extern		void MessagesTextviewCommand();
extern		void NextMarked();
extern		NoOp();
extern		OrgHit();
extern          PrepareAppendFileName();
extern		void PrevMarked();
extern		void PrintMarked();
extern		void PrintVisibleMessage();
extern		void PuntCurrent();
extern		void PurgeAllDeletions();
static		void QuitMessages();
extern		ReadByName();
extern		ReadNamedFolder();
extern		void ReplyAllMarked();
extern		void ReplySendersMarked();
extern		void ResendMarked();
extern		void RestoreOldMarks();
extern		void sm_SetMessagesOptions();
extern		SetSubStatus();
extern		void ShrinkFileIntoMenus();
extern		void SubscribeByName();
extern		void TextviewCompound();
extern		void ThisIsFlorida();
extern		void UnSubscribeByName();
extern		void UndeleteMarked();
extern		countdots();
extern		int messages__AppendOneMessageToFile();
extern		boolean messages__InitializeClass();
extern          boolean messages__InitializeObject();
extern		void messages__ResetFileIntoMenus();
extern		void messages__SetWhatIAm();

static struct keymap *messages_standardkeymap, *messages_permkeymap;
static struct menulist *messages_standardmenulist, *messages_permmenulist;

static int folderTreeExists = 0;

#define MENUMASK_MSGSHOWING 1
#define MENUMASK_MSGWRITABLE 2
#define MENUMASK_MSGDELETED 4
#define MENUMASK_MSGNOTDELETED 8
#define MENUMASK_HASMARKS 16
#define MENUMASK_SHOWMORENEXT 32
#define MENUMASK_SETQUITHERE 64
#define MENUMASK_MARKING 128
#define MENUMASK_FILEINTO 256
#define MENUMASK_THREEREPLIES 512
#define MENUMASK_MARKASUNREAD 1024
#define MENUMASK_FORMATMENUS 2048
#define MENUMASK_APPENDBYNAME 4096
#define MENUMASK_FILEINTOMENU 8192
#define MENUMASK_MARKEDEXTRAS 16384
#define MENUMASK_EXPANDEDMENUS 32768
#define MENUMASK_PUNTMENU 65536
#define MENUMASK_NOTTHREEREPLIES 131072

#define ISSTRING(s) ((long)(s) < 0 || (long)(s)>255)

/* permbindings: the key and menu bindings listed here always appear. */
static struct bind_Description messages_permbindings [] = {
    /* procname, keysequenece, key rock, menu string, menu rock, menu mask, proc, docstring, dynamic autoload */
    {NULL, "\033\t", NULL, NULL, NULL, 0, NULL, NULL, NULL}, /* No inset insertion */
    {"messages-dummy-quit", "\003", NULL, NULL, NULL, 0, BSM_DummyQuit, "dummy quit command"},
    {"messages-quit", "\030\003", NULL, "Quit~99", NULL, 0, QuitMessages, "Exit messages."},
    {"messages-current-search", "\023", NULL,  NULL, 0, NULL, GSearchFPlease, "Search forward in current view"},
    {"messages-current-rsearch", "\022", NULL, NULL, 0, NULL, GSearchRPlease, "Search backward in current view"}, 
    {"messages-send-message", "\030\023", NULL, "Send/Post Message~51", NULL, 0, BSM_SendFresh, "Send a message."},
    {"messages-delete-window", "\030\004", NULL, "Delete Window~89", 0, NULL, DeleteWindow, "Delete this messages subwindow."},

    {NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL},
};

/* standardbindings: the key bindings listed here only show up if you have the keystrokes option turned on. The menu bindings always appear. */
static struct bind_Description messages_standardbindings [] = {
    /* procname, keysequenece, key rock, menu string, menu rock, menu mask, proc, docstring, dynamic autoload */
    {"messages-subscribe-by-name", NULL, NULL, NULL, NULL, NULL, SubscribeByName, "Subscribe to a folder by name"},
    {"messages-unsubscribe-by-name", NULL, NULL, NULL, NULL, NULL, UnSubscribeByName, "Unsubscribe to a folder by name"},
    {"messages-alter-subscription-by-name", NULL, NULL, NULL, NULL, NULL, AlterSubByName, "Alter a subscription to a folder by name"},
    {"messages-purge-deletions", NULL, NULL, "Other~60,Purge Deletions~55", 0, 0, PurgeAllDeletions, "Purge all deleted messages."},
    {"messages-set-options", NULL, NULL, "Other~60,Set Options~44", 0, 0, (void (*)()) sm_SetMessagesOptions, "Set Messages Options"},
    {"messages-refresh-displayed-folder", NULL, NULL, NULL, NULL, 0, BSM_RefreshDisplayedFolder, "Refresh the folder currently on display"},
    {"messages-expose-new", NULL, NULL, "Message Folders~55,Expose Changed~12", NULL, 0, BSM_ShowNewPlease, "Show Changed folders"},
    {"messages-expose-all", NULL, NULL, "Message Folders~55,Expose All~14", NULL, 0, BSM_ShowAllPlease, "Show all folders"},
    {"messages-expose-subscribed", NULL, NULL, "Message Folders~55,Expose Subscribed~15", NULL, 0, BSM_ShowSubscribedPlease, "Show subscribed folders"},
    {"messages-expose-personal", NULL, NULL, "Message Folders~55,Expose Personal~16", NULL, MENUMASK_FILEINTO, BSM_ShowPersonalPlease, "Show personal folders"},
#ifndef NOTREESPLEASE
    {"messages-expose-tree", NULL, NULL, "Message Folders~55,Expose Tree~17", NULL, 0, BSM_ShowTreePlease, "Show all folders"},
#endif
    {"messages-create-folder", NULL, NULL, "Message Folders~55,Create~21", NULL, MENUMASK_FILEINTO, BSM_CreatePlease, "Create folder"},
    {"messages-delete-folder", NULL, NULL, "Message Folders~55,Delete~22", NULL, MENUMASK_FILEINTO, BSM_DeleteFolder, "Delete folder"},
    {"messages-rename-folder", NULL, NULL, "Message Folders~55,Rename~23", NULL, MENUMASK_FILEINTO, BSM_RenamePlease, "Rename folder"},
    {"messages-read-by-name", "\030\026", NULL, "Message Folders~55,Read By Name~51", 0, 0, (void (*)()) ReadByName, "Read any named message folder"},
    {"messages-read-mail", "\030\022", NULL, "Read Mail~41", NULL, 0, BSM_ReadMailPlease, "Read your mail"},
    {"messages-check-new-messages", "\030\001", NULL, "Other~60,Check New Messages~20", 0, NULL, BSM_CheckNewPlease, "Check for new messages"},
    {"messages-set-printer", NULL, NULL, "Other~60,Set Printer~45", 0, 0, (void (*)()) BSM_SetPrinter, "Set printer to use"},
    {"messages-print-current", "\033\020", NULL, "This Message~30,Print~41", NULL, MENUMASK_MSGSHOWING, (void (*)()) PrintVisibleMessage, "Print current message"},
    {"messages-show-help", "?", NULL, NULL, NULL, 0, BSM_ShowHelp, "Show basic help text"},
    {"messages-captions-search", NULL, NULL,  "Search/Spell~1,Captions Forward~31", 0, NULL, CSearchFPlease, "Search forward in captions"},
    {"messages-captions-rsearch", NULL, NULL, "Search/Spell~1,Captions Backward~32", 0, NULL, CSearchRPlease, "Search backward in captions"},
    {"messages-folders-search", NULL, NULL,  "Search/Spell~1,Folders Forward~21", 0, NULL, FSearchFPlease, "Search forward in folders"},
    {"messages-folders-rsearch", NULL, NULL, "Search/Spell~1,Folders Backward~22", 0, NULL, FSearchRPlease, "Search backward in folders"},
    {"messages-bodies-search", NULL, NULL,  "Search/Spell~1,Body Forward~41", 0, NULL, BSearchFPlease, "Search forward in bodies"},
    {"messages-bodies-rsearch", NULL, NULL, "Search/Spell~1,Body Backward~42", 0, NULL, BSearchRPlease, "Search backward in bodies"},
    {NULL, NULL, NULL,  "Search/Spell~1,Forward", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Search/Spell~1,Backward", 0, NULL, NULL, NULL},

    {NULL, NULL, NULL,  "Page,Insert Pagebreak", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Next Page", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Previous Page", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Insert Footnote", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Open Footnotes", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Close Footnotes", 0, NULL, NULL, NULL},
    {NULL, NULL, NULL,  "Page,Table of Contents", 0, NULL, NULL, NULL},

    {"messages-next-marked", NULL, NULL, "Marked Messages~25,Next~2", NULL, MENUMASK_HASMARKS, NextMarked, "Show next marked message."},
    {"messages-previous-marked", NULL, NULL, "Marked Messages~25,Previous~4", NULL, MENUMASK_HASMARKS, PrevMarked, "Show previous marked message."},
    {"messages-delete-marked", NULL, NULL, "Marked Messages~25,Delete All~10", NULL, MENUMASK_HASMARKS, DeleteMarked, "Delete all marked messages."},
    {"messages-undelete-marked", NULL, NULL, "Marked Messages~25,Undelete All~11", NULL, MENUMASK_HASMARKS, UndeleteMarked, "Undelete all marked messages."},
    {"messages-print-marked", NULL, NULL, "Marked Messages~25,Print All~20", NULL, MENUMASK_HASMARKS, PrintMarked, "Print all marked messages."},
    {"messages-clear-marks", NULL, NULL, "Marked Messages~25,Clear Marks~70", NULL, MENUMASK_HASMARKS, ClearMarks, "Clear out all marks on messages."},
    {"messages-count-marks", NULL, NULL, "Marked Messages~25,Count Marks~80", NULL, MENUMASK_HASMARKS, CountMarks, "Count marked messages."},


    {"messages-file-all-marked", NULL, NULL, "Send/File Marked~26,File All Into~20", NULL, MENUMASK_HASMARKS, ClassifyMarked, "File all marked messages into some folder."},
    {"messages-copy-all-marked", NULL, NULL, "Send/File Marked~26,Copy All Into~21", NULL, MENUMASK_HASMARKS | MENUMASK_MARKEDEXTRAS, CopyMarked, "File all marked messages into some folder."},
    {"messages-append-all-marked", NULL, NULL, "Send/File Marked~26,Append To Folder~22", NULL, MENUMASK_HASMARKS | MENUMASK_MARKEDEXTRAS, AppendMarked, "Append all marked messages onto some folder."},
    {"messages-append-all-to-file", NULL, NULL, "Send/File Marked~26,Append To File~30", NULL, MENUMASK_HASMARKS, AppendMarkedToFile, "Append all marked messages onto some file."},
    {"messages-append-all-to-file-raw", NULL, NULL, "Send/File Marked~26,Append To Raw File~30", NULL, MENUMASK_HASMARKS, AppendMarkedToRawFile, "Append all marked messages onto some file in RAW format."},
    {"messages-reply-to-senders", NULL, NULL, "Send/File Marked~26,Reply To Senders~40", NULL, MENUMASK_HASMARKS, ReplySendersMarked, "Reply to senders of all marked messages."},
    {"messages-reply-all-to-all", NULL, NULL, "Send/File Marked~26,Reply To All~42", NULL, MENUMASK_HASMARKS, ReplyAllMarked, "Reply to senders and readers of all marked messages."},
    {"messages-resend-all", NULL, NULL, "Send/File Marked~26,Resend All~50", NULL, MENUMASK_HASMARKS, ResendMarked, "Resend all marked messages."},
    {"messages-excerpt-all", NULL, NULL, "Send/File Marked~26,Excerpt All~60", NULL, MENUMASK_HASMARKS, ExcerptMarked, "Excerpt all marked messages into sendmessage window."},

    {"messages-restore-marks", NULL, NULL, "Search/Spell~1,Restore Old Marks~63", NULL, MENUMASK_MARKING, RestoreOldMarks, "Restore the most recent set of marks."},
    {"messages-find-all", NULL, 0, "Search/Spell~1,Mark By Keyword~51", 0, MENUMASK_MARKING, FindAllCaptions, "Mark all captions containing a given string."},
    {"messages-find-range", NULL, NULL, "Search/Spell~1,Mark By Date~52", NULL, MENUMASK_MARKING, MarkRange, "Mark all messages in a given date range."},
    {"messages-shrink-file-into-menus", NULL, NULL, "File Into...~40,Shrink Menu~99", NULL, MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU, ShrinkFileIntoMenus, "Shrink the list on the 'file into' menu."},
    {"messages-expand-file-into-menus", NULL, NULL, "File Into...~40,Expand Menu~99", NULL, MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU, ExpandFileIntoMenus, "Expand the list on the 'file into' menu."},
    {"messages-show-more", " ", NULL, "Show More~1", NULL, MENUMASK_SHOWMORENEXT, BSM_MorePlease, "Show more or next"},
    {"messages-show-next", "\n", NULL, "Show Next~2", NULL, MENUMASK_SHOWMORENEXT, BSM_NextPlease, "Show more or next"},
    {"messages-show-next", "n", NULL, NULL, NULL, 0, BSM_NextPlease, "Show next"},
    {"messages-scroll-back-body", "b", NULL, NULL, NULL, 0, BSM_ScrollBackBody, "Scroll body backward"},
    {"messages-scroll-forward-body", "v", NULL, NULL, NULL, 0, BSM_ScrollForwardBody, "Scroll body forward"},
    {"messages-show-previous", "p", NULL, NULL, NULL, 0, BackUp, "Show previous"},
    {"messages-delete-current", "d", NULL, "Delete~21", NULL, MENUMASK_MSGSHOWING | MENUMASK_MSGNOTDELETED, BSM_DeletePlease, "Delete current message"},
    {"messages-undelete-current", "u", NULL, "Undelete~21", NULL, MENUMASK_MSGSHOWING | MENUMASK_MSGDELETED, BSM_UndeletePlease, "Delete current message"},
    {"messages-classify-this", "c", NULL, NULL, NULL, 0, BSM_ClassifyPlease, "Classify current message"},
    {"messages-file-and-delete", NULL, NULL, "File Into...~40,By Name~77", NULL, MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU, BSM_FileInto, "File this into some folder & delete it if possible."},
    {"messages-classify-by-name", NULL, NULL, NULL, NULL, 0, FileIntoByName, "File this into some named folder."},
    {"messages-append-by-name", NULL, NULL, NULL, NULL, 0, FileOntoByName, "Append this onto some named folder."},
    {"messages-append-and-delete", NULL, NULL, "File Into...~40,Append By Name~88", NULL, MENUMASK_APPENDBYNAME | MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU, BSM_AppendOnto, "Append this to some folder & delete it if possible."},
    {"messages-copy-to-folder", "C", NULL, NULL, NULL, 0, BSM_CopyPlease, "Copy current message into folder"},
    {"messages-append-this", "A", NULL, NULL, NULL, 0, BSM_AppendPlease, "Append current message to a folder"},
    {"messages-append-delete", "a", NULL, NULL, NULL, 0, BSM_AppendDelPlease, "Append current message to a folder and delete it"},
    {"messages-reply-to-sender", "r", NULL, "Reply to Sender~31", NULL, MENUMASK_MSGSHOWING, BSM_ReplyToSender, "Reply to sender"},
    {"messages-forward-message", "f", NULL, "This Message~30,Forward To~11", NULL, MENUMASK_MSGSHOWING, BSM_SendForward, "Forward current message"},
    {"messages-mark-current", "M", NULL, NULL, NULL, 0, BSM_MarkCurrent, "Mark current message"},
    {"messages-reply-to-readers", NULL, NULL, "Reply to Readers~32", NULL, MENUMASK_MSGSHOWING | MENUMASK_THREEREPLIES, BSM_ReplyToReaders, "Reply to readers"},
    {"messages-reply-to-both", NULL, NULL, "Reply to Both~33", NULL, MENUMASK_MSGSHOWING | MENUMASK_THREEREPLIES, BSM_ReplyToBoth, "Reply to both"},
    {"messages-reply-to-all", "R", NULL, "Reply to All~33", NULL, MENUMASK_MSGSHOWING | MENUMASK_NOTTHREEREPLIES, BSM_ReplyToAll, "Reply to all"},
    {"messages-restore-as-draft", NULL, NULL, "This Message~30,Restore Draft~15", NULL, MENUMASK_MSGSHOWING | MENUMASK_MSGWRITABLE | MENUMASK_FILEINTO, BSM_RestoreAsDraft, "Restore as draft"},
    {"messages-set-quit-here", NULL, NULL, "Other~60,Set Quit Here~88", NULL, MENUMASK_SETQUITHERE, ThisIsFlorida, "Set the current message to be the first one seen next time."},
    {"messages-append-to-file", NULL, NULL, "This Message~30,Append to File~21", NULL, MENUMASK_MSGSHOWING, (void (*)()) AppendMessageToFile, "Append to file"},
    {"messages-append-to-raw-file", NULL, NULL, "This Message~30,Append to Raw File~21", NULL, MENUMASK_MSGSHOWING, (void (*)()) AppendMessageToRawFile, "Append to file in raw format"},
    {"messages-mark-as-unseen", NULL, NULL, "This Message~30,Mark as Unread~80", NULL, MENUMASK_MSGSHOWING | MENUMASK_MSGWRITABLE | MENUMASK_MARKASUNREAD, (void (*)()) MarkVisibleMessageUnseen, "Mark this message as unread."},
    {"messages-find-related", NULL, NULL, "Search/Spell~1,Mark Related Messages~53", NULL, MENUMASK_MARKING | MENUMASK_MSGSHOWING, FindRelatedMessages, "Find all captions related to the message on display"},
    {"messages-resend", NULL, NULL, "This Message~30,Resend To~12", NULL, MENUMASK_MSGSHOWING, BSM_ReSendPlease, "ReSend current message"},
    {"messages-redisplay-formatted", NULL, NULL, NULL, NULL, 0, BSM_RedisplayFormat, "Show interpreting formatting"},
    {"messages-redisplay-normal", NULL, NULL, NULL, NULL, 0, BSM_RedisplayNormal, "Show formatting normally"},
    {"messages-redisplay-fixed", NULL, NULL, "This Message~30,Fixed Width~32", NULL, MENUMASK_MSGSHOWING | MENUMASK_FORMATMENUS, BSM_RedisplayFixedWidth, "Show in fixed-width font"},
    {"messages-redisplay-rot13", NULL, NULL, "This Message~30,Descramble~31", NULL, MENUMASK_MSGSHOWING | MENUMASK_FORMATMENUS, BSM_RedisplayRot13, "Run rot-13 descrambling algorithm"},
    {"messages-select-body", NULL, NULL, NULL, NULL, 0, BSM_SelectBody, "Select the body of the message"},
    {"messages-next-digest", "\033n", NULL, NULL, NULL, NULL, BSM_NextDigest, "Find the next encapsulated message in the message body"},
    {"messages-previous-digest", "\033p", NULL, NULL, NULL, NULL, BSM_PrevDigest, "Find the previous encapsulated message in the message body"},
    {"messages-modifiable-body", NULL, NULL, NULL, NULL, 0, BSM_ModifiableBody, "Make the body of the message not be read-only"},
    {"messages-different-content-type", NULL, NULL, NULL, NULL, 0, BSM_DifferentContentType, "Display the body of a message using a different content-type header"},
    {"messages-show-as-plain-text", NULL, NULL, "This Message~30,Show Raw Body~33", NULL, MENUMASK_MSGSHOWING, BSM_ShowRaw, "Display the body of a message as if it were plain text"},
    {"messages-punt", ">", 1, "Other~60,Punt~94", 1, MENUMASK_PUNTMENU, (void (*)()) PuntCurrent, "Punt current folder and go to the next one"},
    {"messages-punt-and-stay", "~", 0, NULL, 0, 0, PuntCurrent, "Punt current folder but don't go on to the next one"},
    {"textview-compound", NULL, NULL, NULL, NULL, NULL, TextviewCompound, "Execute a compound textview operation"},
    {"messages-compound-operation", NULL, NULL, NULL, NULL, 0, MessagesCompound, "Execute a compound Messages operation"},
    {"messages-textview-compound", NULL, NULL, NULL, NULL, 0, MessagesTextviewCommand, "Execute a compound 'textview' operation on the folders"},
    {"messages-sendmessage-compound", NULL, NULL, NULL, NULL, 0, MessagesSendmessageCommand, "Execute a compound 'sendmessage' operation."},
    {"messages-folders-compound", NULL, NULL, NULL, NULL, 0, MessagesFoldersCommand, "Execute a compound 'captions' operation."},
    {"messages-captions-compound", NULL, NULL, NULL, NULL, 0, MessagesCaptionsCommand, "Execute a compound 'captions' operation."},
    {"messages-bodies-compound", NULL, NULL, NULL, NULL, 0, MessagesBodiesCommand, "Execute a compound 'captions' operation."},
    {"messages-focus-on-folders", NULL, NULL, NULL, NULL, 0, MessagesFocusFolders, "Make the current folders the input focus."},
    {"messages-focus-on-captions", NULL, NULL, NULL, NULL, 0, MessagesFocusCaptions, "Make the current captions the input focus."},
    {"messages-focus-on-bodies", NULL, NULL, NULL, NULL, 0, MessagesFocusBodies, "Make the current bodies area the input focus."},

    {NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL},
};

NoOp() {
    message_DisplayString(NULL, 10, "This command does nothing.");
}

int (*messtextv_ReverseSearchCmd)()		= NoOp,
(*messtextv_ScrollScreenBackCmd)()	= NoOp,
(*messtextv_ScrollScreenForwardCmd)()   = NoOp,
(*messtextv_ForwardSearchCmd)()		= NoOp;

boolean messages__InitializeClass(c) 
struct classheader *c;
{
    struct proctable_Entry *tempProc;

    class_Load("textview");
    if ((tempProc = proctable_Lookup("textview-search")) != NULL) {
	messtextv_ForwardSearchCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-reverse-search")) != NULL) {
	messtextv_ReverseSearchCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-prev-screen")) != NULL) {
	messtextv_ScrollScreenBackCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-next-screen")) != NULL) {
	messtextv_ScrollScreenForwardCmd = proctable_GetFunction(tempProc);
    }

    messages_standardmenulist = menulist_New();
    messages_standardkeymap = (struct keymap *)class_NewObject("keymap");
    bind_BindList(messages_standardbindings, messages_standardkeymap, messages_standardmenulist, &messages_classinfo);

    messages_permmenulist = menulist_New();
    messages_permkeymap = (struct keymap *)class_NewObject("keymap");
    bind_BindList(messages_permbindings, messages_permkeymap, messages_permmenulist, &messages_classinfo);
    return(TRUE);
}

#define BIGMENUCARD 25  /* Most menu items to construct on a single file into card */

void messages__ResetFileIntoMenus(self)
struct messages *self;
{
    static struct proctable_Entry *directlyclassifyproc = NULL;
    int i, max, smallmax, biggermax;
    char MenuString[25+MAXPATHLEN];

    ams_InitializeClassList();
    if (self->fileintomenulist) {
	menulist_ClearML(self->fileintomenulist);
    } else {
	self->fileintomenulist = menulist_Create(self);
    }

    if(self->expandedmenuslist) menulist_ClearML(self->expandedmenuslist);
    else self->expandedmenuslist=menulist_Create(self);

    if (ams_GetLastMenuClass() < 0) return;
    max = ams_GetClassListCount();
    smallmax = environ_GetProfileInt("messages.maxclassmenu", 8);
    biggermax = environ_GetProfileInt("messages.maxtotalclassmenu", BIGMENUCARD);
    if(biggermax>BIGMENUCARD) biggermax=BIGMENUCARD;

    if (smallmax > max) smallmax = max;
    if (!directlyclassifyproc) {
	directlyclassifyproc = proctable_DefineProc("messages-directly-classify", DirectlyClassify, &messages_classinfo, NULL, "");
    }
    for(i=max - 1; i>=0; --i) {
	if (i < biggermax) {
	    sprintf(MenuString, "File Into...~40,%s", ams_GetClassListEntry(i));
	} else {
	    sprintf(MenuString, "File Into (%d)~%d,%s", i/biggermax, 40+i/biggermax, ams_GetClassListEntry(i));
	}
	if(i<smallmax)  menulist_AddToML(self->fileintomenulist, MenuString, directlyclassifyproc,  i, MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU);
	else menulist_AddToML(self->expandedmenuslist, MenuString, directlyclassifyproc,  i, MENUMASK_MSGSHOWING | MENUMASK_FILEINTOMENU);
    }
}

void BSM_RefreshDisplayedFolder(self, CheckAll)
struct messages *self;
char *CheckAll;
{
    struct captions *c = GetCaptions(self);
    if (c->FullName) {
	char nickname[1+MAXPATHLEN], fullname[1+MAXPATHLEN];

	strcpy(nickname, c->ShortName);
	strcpy(fullname, c->FullName);
	if (ISSTRING(CheckAll)) {
	    ams_CUI_CheckMailboxes(ams_GetAMS(), fullname);
	    captions_ClearAndUpdate(c, TRUE, TRUE);
	}
	captions_InsertUpdatesInDocument(c, nickname, fullname, FALSE);
    } else {
	message_DisplayString(NULL, 10, "There is no folder on display.");
    }
}

void PurgeAllDeletions(self)
struct messages *self;
{
    ams_WaitCursor(TRUE);
    ams_CUI_PurgeMarkedDirectories(ams_GetAMS(), FALSE, FALSE);
    BSM_RefreshDisplayedFolder(self, FALSE); 
    ams_WaitCursor(FALSE);
}

void MessagesFocusCaptions(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);
    captions_WantInputFocus(c, c);
}

void MessagesFocusBodies(self)
struct messages *self;
{
    struct t822view *tv = GetBodies(self);
    t822view_WantInputFocus(tv, tv);
}

void TextviewCompound(tv, cmds)
struct textview *tv;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), tv, "textview", cmds);
}

void MessagesCompound(self, cmds)
struct messages *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "messages", cmds);
}

void MessagesTextviewCommand(self, cmds)
struct messages *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "textview", cmds);
}

void MessagesCaptionsCommand(self, cmds)
struct messages *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), GetCaptions(self), "captions", cmds);
}

void MessagesBodiesCommand(self, cmds)
struct messages *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), GetBodies(self), "t822view", cmds);
}

void messages__SetWhatIAm(self, w)
struct messages *self;
int w;
{
    self->WhatIAm = w;
}

void BSM_DummyQuit(self)
struct messages *self;
{
    message_DisplayString(NULL, 10, "Use ^X^C to quit.");
}

static void QuitMessages(self) 
struct messages *self;
{
    ams_CommitState(TRUE, FALSE, TRUE, TRUE);
}

void BSearchFPlease(self)
struct messages *self;
{
    messtextv_ForwardSearchCmd((struct textview *) GetBodies(self));
    messages_WantInputFocus(self, self);
}

void BSearchRPlease(self)
struct messages *self;
{
    messtextv_ReverseSearchCmd((struct textview *) GetBodies(self));
    messages_WantInputFocus(self, self);
}

void CSearchFPlease(self)
struct messages *self;
{
    messtextv_ForwardSearchCmd((struct textview *) GetCaptions(self));
    messages_WantInputFocus(self, self);
}

void CSearchRPlease(self)
struct messages *self;
{
    captions_CapReverseSearch(GetCaptions(self)); /* Handles virtual scroll bar */
}

void GSearchFPlease(self)
struct messages *self;
{
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    FSearchFPlease(self);
	    break;
	case WHATIAM_CAPTIONS:
	    CSearchFPlease(self);
	    break;
	case WHATIAM_BODIES:
	    BSearchFPlease(self);
	    break;
    }
}

void GSearchRPlease(self)
struct messages *self;
{
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    FSearchRPlease(self);
	    break;
	case WHATIAM_CAPTIONS:
	    CSearchRPlease(self);
	    break;
	case WHATIAM_BODIES:
	    BSearchRPlease(self);
	    break;
    }
}

void BSM_CreatePlease(self)
struct messages *self;
{
    char *s, *s2, ShortName[1+MAXPATHLEN], NewFullName[1+MAXPATHLEN], *TempPtr, ErrorText[100 + MAXPATHLEN];
    if (message_AskForString(NULL, 50, "Create folder: ", "", ShortName, sizeof(ShortName)) < 0) return;

    s = rindex(ShortName, '.');
    s2 = rindex(ShortName, '/');
    if (s2 > s) s = s2;
    if (s) *s++ = '\0';
    if (s == NULL) {
	sprintf(NewFullName, "%s/%s", ams_GetMailPath(), ShortName);
    } else {
	long code;
	code = ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &TempPtr);
	if (code) {
	    code = ams_MS_DisambiguateFile(ams_GetAMS(), ShortName, NewFullName, AMS_DISAMB_EXISTS);
	    if (code) {
		ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
		return;
	    } else {
		sprintf(ErrorText, "Warning -- %s is not a message directory; I assume it is a root", ShortName);
		message_DisplayString(NULL, 10, ErrorText);
	    }
	} else {
	    strcpy(NewFullName, TempPtr);
	}
	strcat(NewFullName, "/");
	strcat(NewFullName, s);
    }
    ams_WaitCursor(TRUE);
    message_DisplayString(NULL, 10, "Creating folder; please wait...");
    im_ForceUpdate();
    ams_CUI_CreateNewMessageDirectory(ams_GetAMS(), NewFullName, NewFullName); /* reports own errors */
    ams_WaitCursor(FALSE);
}

void BSM_DeleteFolder(self)
struct messages *self;
{
    char ShortName[1+MAXPATHLEN], *FullName;

    if (message_AskForString(NULL, 50, "Delete folder: ", "", ShortName, sizeof(ShortName)) < 0) return;
    if (ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &FullName)) {
	ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
	return;
    }
    ams_WaitCursor(TRUE);
    message_DisplayString(NULL, 10, "Removing folder; please wait...");
    im_ForceUpdate();
    ams_CUI_RemoveDirectory(ams_GetAMS(), FullName); /* reports own errors */
    ams_WaitCursor(FALSE);
}

void BSM_SetPrinter(self)
struct messages *self;
{
    char Pnamebuf[100], Prompt[110];
    static char LastPrinterName[100] = "";

    strcpy(Prompt, "Printer to use");
    if (LastPrinterName[0]) {
	strcat(Prompt, " [");
	strcat(Prompt, LastPrinterName);
	strcat(Prompt, "]");
    }
    strcat(Prompt, ": ");
    if (message_AskForString(NULL, 50, Prompt, "", Pnamebuf, sizeof(Pnamebuf)) < 0) return;
    if (Pnamebuf[0]) {
	if (!ams_CUI_SetPrinter(ams_GetAMS(), Pnamebuf)) {
	    strcpy(LastPrinterName, Pnamebuf);
	}
    }
}

void NextMarked(self)
struct messages *self;
{
    captions_ShowMore(GetCaptions(self), FALSE, FALSE, TRUE);
}

void PrevMarked(self)
struct messages *self;
{
    captions_BackUpCheckingMarks(GetCaptions(self), TRUE);
}

void DeleteMarked(self)
struct messages *self;
{
    captions_ActOnMarkedMessages(GetCaptions(self), MARKACTION_DELETE, NULL);
}

void UndeleteMarked(self)
struct messages *self;
{
    captions_ActOnMarkedMessages(GetCaptions(self), MARKACTION_UNDELETE, NULL);
}

void PrintMarked(self)
struct messages *self;
{
    captions_ActOnMarkedMessages(GetCaptions(self), MARKACTION_PRINT, NULL);
}

void ClassifyMarked(self, name)
struct messages *self;
char *name;
{
    char FName[10+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (!ISSTRING(name)) {
	GetFolderName(c, "File", FName);
	if (FName[0] == '\0') return;
	name = FName;
    }
    captions_ActOnMarkedMessages(c, MARKACTION_CLASSIFYBYNAME, name);
}

void CopyMarked(self, name)
struct messages *self;
char *name;
{
    char FName[10+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (!ISSTRING(name)) {
	GetFolderName(c, "Copy", FName);
	if (FName[0] == '\0') return;
	name = FName;
    }
    captions_ActOnMarkedMessages(c, MARKACTION_COPYBYNAME, name);
}

void AppendMarked(self, name)
struct messages *self;
char *name;
{
    char FName[10+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (!ISSTRING(name)) {
	GetFolderName(c, "Append", FName);
	if (FName[0] == '\0') return;
	name = FName;
    }
    captions_ActOnMarkedMessages(c, MARKACTION_APPENDBYNAME, name);
}

GetFolderName(self, prompt, FName)
struct captions *self;
char *prompt, *FName;
{
    char ErrorText[500];
    char *lc = captions_GetLastClassification(self);

    FName[0] = '\0';
    sprintf(ErrorText, "%s all %d marked messages into what folder [%s] : ",
	     prompt, self->MarkCount, lc);
    if (ams_GetFolderName(ErrorText, FName, MAXPATHLEN, "", FALSE)) return(-1);
    if (FName[0] == '\0') {
	strcpy(FName, lc);
    }
    return(0);
}

void AppendMarkedToFile(self)
struct messages *self;
{
    char Buf[1+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (PrepareAppendFileName(c, Buf)) return;
    captions_ActOnMarkedMessages(c, MARKACTION_APPENDTOFILE, Buf);
}    

void AppendMarkedToRawFile(self)
struct messages *self;
{
    char Buf[1+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (PrepareAppendFileName(c, Buf)) return;
    captions_ActOnMarkedMessages(c, MARKACTION_APPENDTOFILERAW, Buf);
}    

void BackUp(self)
struct messages *self;
{
    captions_BackUpCheckingMarks(GetCaptions(self), FALSE);
}

void BSM_DeletePlease(self)
struct messages *self;
{
    captions_DeleteVisibleMessage(GetCaptions(self), TRUE);
}

void BSM_UndeletePlease(self)
struct messages *self;
{
    captions_DeleteVisibleMessage(GetCaptions(self), FALSE);
}

void BSM_ClassifyPlease(self)
struct messages *self;
{
    captions_CloneMessage(GetCaptions(self), MS_CLONE_COPYDEL);
}

void BSM_CopyPlease(self)
struct messages *self;
{
    captions_CloneMessage(GetCaptions(self), MS_CLONE_COPY);
}

void FileByName(self, name, ReallyAppend)
struct messages *self;
char *name;
boolean ReallyAppend;
{
    int OpCode;
    struct captions *c = GetCaptions(self);

    if (c->VisibleCUID < 1) {
	message_DisplayString(NULL, 10, "There is no message on display.");
	return;
    }
    ams_WaitCursor(TRUE);
    OpCode = AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_COPYDEL : MS_CLONE_COPY;
    if (ReallyAppend) {
	OpCode = AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_APPENDDEL : MS_CLONE_APPEND;
    }
    if (!ams_CUI_CloneMessage(ams_GetAMS(), c->VisibleCUID, name, OpCode)) {
	/* cuilib reports errors, here we deal with success */
	if (OpCode == MS_CLONE_APPENDDEL || OpCode == MS_CLONE_COPYDEL) {
	    text_SetEnvironmentStyle(c->CaptText, c->HighlightEnv, c->ActiveDeletedStyle);
	    AMS_SET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_DELETED);
	    captions_WantUpdate(c, c);
	    captions_AlterDeletedIcon(c, c->HighlightStartPos, TRUE);
	    captions_PostMenus(c, NULL);
	}
    }
    ams_WaitCursor(FALSE);
}

void FileIntoByName(self, name)
struct messages *self;
char *name;
{
  if (!ISSTRING(name)) name = 0;
    FileByName(self, name, FALSE);
}

void FileOntoByName(self, name)
struct messages *self;
char *name;
{
  if (!ISSTRING(name)) name = 0;
    FileByName(self, name, TRUE);
}

void BSM_FileInto(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_CloneMessage(c, (AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY)) ? MS_CLONE_COPYDEL : MS_CLONE_COPY);
}

void BSM_AppendOnto(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_CloneMessage(c, (AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY)) ? MS_CLONE_APPENDDEL : MS_CLONE_APPEND);
}

void BSM_AppendPlease(self)
struct messages *self;
{
    captions_CloneMessage(GetCaptions(self), MS_CLONE_APPEND);
}

void BSM_AppendDelPlease(self)
struct messages *self;
{
    captions_CloneMessage(GetCaptions(self), MS_CLONE_APPENDDEL);
}

void BSM_ReplyToSender(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_SENDER);
}

void BSM_SendForward(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_FORWARD_FMT);
}

void BSM_MarkCurrent(self)
struct messages *self;
{
    captions_MarkCurrent(GetCaptions(self));
}

void PrintVisibleMessage(self)
struct messages *self;
{
    captions_PrintVisibleMessage(GetCaptions(self));
}

void BSM_ReplyToReaders(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_WIDE);
}

void BSM_ReplyToBoth(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_WIDER);
}

void BSM_RestoreAsDraft(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_REDRAFT);
}

void BSM_ReplyToAll(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);
    if (AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY)) {
	captions_SendMessage(c, AMS_REPLY_WIDER);
    } else {
	captions_SendMessage(c, AMS_REPLY_WIDE);
    }
}

void ThisIsFlorida(self)
struct messages *self;
{
    captions_ThisIsFlorida(GetCaptions(self));
}

AppendMessageToFile(self)
struct messages *self;
{
    char Buf[1+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (c->VisibleCUID < 0) {
	message_DisplayString(NULL, 25, "There is no message on display.");
	return(-1);
    }
    if (PrepareAppendFileName(c, Buf)) return(-1);
    messages_AppendOneMessageToFile(self, c->VisibleCUID, Buf, 0);
}

AppendMessageToRawFile(self)
struct messages *self;
{
    char Buf[1+MAXPATHLEN];
    struct captions *c = GetCaptions(self);

    if (c->VisibleCUID < 0) {
	message_DisplayString(NULL, 25, "There is no message on display.");
	return(-1);
    }
    if (PrepareAppendFileName(c, Buf)) return(-1);
    messages_AppendOneMessageToFile(self, c->VisibleCUID, Buf, 1);
}

static char LastAppendFileName[1+MAXPATHLEN] = "~/SavedMessages";

PrepareAppendFileName(self, Buf)
struct captions *self;
char *Buf;
{
    if (LastAppendFileName[0]) {
	strcpy(Buf, LastAppendFileName);
    } else {
	ams_TildeResolve(ams_GetAMS(), "~", Buf);
    }
    if (completion_GetFilename(NULL, "Append message body to file: ", Buf, Buf, MAXPATHLEN, FALSE, FALSE) == -1 ) {
	return (-1);
    }	
    return(0);
}

int messages__AppendOneMessageToFile(self, cuid, Buf, DoRaw)
struct messages *self;
int cuid;
char *Buf;
int DoRaw;
{
    char ErrorText[256], TmpFile[1+MAXPATHLEN], Splat[5000];
    FILE *fp, *rfp;
    int ShouldDelete, amtread, magicnum;

    ams_WaitCursor(TRUE);
    fp = fopen(Buf, "a");
    if (fp == NULL) {
	sprintf(ErrorText, "Sorry; can't open file '%s' (error %d).", ams_ap_Shorten(ams_GetAMS(), Buf), errno);
	message_DisplayString(NULL, 50, ErrorText);
	ams_WaitCursor(FALSE);
	im_ForceUpdate();
	return(-1);
    }
    if (ams_CUI_ReallyGetBodyToLocalFile(ams_GetAMS(), cuid, TmpFile, &ShouldDelete, !ams_CUI_SnapIsRunning(ams_GetAMS()))) {
	fclose(fp);
	ams_WaitCursor(FALSE);
	return(-1); /* error message already reported */
    }
    rfp = fopen(TmpFile, "r");
    if (!rfp) {
	fclose(fp);
	sprintf(ErrorText, "Cannot open local file %s for reading", ams_ap_Shorten(ams_GetAMS(), TmpFile));
	message_DisplayString(NULL, 50, ErrorText);
	ams_WaitCursor(FALSE);
	return(-1);
    }
    magicnum = ftell(fp);
    if (!DoRaw) fprintf(fp, "\\begindata{text822, %d}\n", magicnum);
    while ((amtread = fread(Splat, sizeof(char), sizeof(Splat), rfp)) > 0) {
	if (ams_fwriteallchars(ams_GetAMS(), Splat, amtread, fp) != amtread) {
	    sprintf(ErrorText, "Error writing file %s--sorry.", ams_ap_Shorten(ams_GetAMS(), Buf));
	    message_DisplayString(NULL, 50, ErrorText);
	    fclose(rfp);
	    fclose(fp);
	    ams_WaitCursor(FALSE);
	    return(-1);
	}
    }
    if (ferror(rfp)) {
	sprintf(ErrorText, "Error reading file %s--sorry.", ams_ap_Shorten(ams_GetAMS(), TmpFile));
	message_DisplayString(NULL, 50, ErrorText);
	fclose(rfp);
	fclose(fp);
	ams_WaitCursor(FALSE);
	return(-1);
    }
    fclose(rfp);
    if (ShouldDelete) {
	unlink(TmpFile);
    }
    if (!DoRaw) fprintf(fp, "\\enddata{text822, %d}\n", magicnum);
    if (vfclose(fp)) {
	sprintf(ErrorText, "Sorry; can't close file '%s' (error %d).", ams_ap_Shorten(ams_GetAMS(), Buf), errno);
	message_DisplayString(NULL, 50, ErrorText);
	ams_WaitCursor(FALSE);
	return(-1);
    } else {
	sprintf(ErrorText, "Wrote file '%s'", ams_ap_Shorten(ams_GetAMS(), Buf));
	message_DisplayString(NULL, 10, ErrorText);
	strcpy(LastAppendFileName, Buf);
    }
    ams_WaitCursor(FALSE);
    return(0);
}

void MarkVisibleMessageUnseen(self)
struct messages *self;
{
    struct captions *ci = GetCaptions(self);

    if (AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_MAYMODIFY) && ! AMS_GET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_UNSEEN) && ams_CUI_MarkAsUnseen(ams_GetAMS(), ci->VisibleCUID)) {
	return; /* error was reported */
    }
    AMS_SET_ATTRIBUTE(ci->VisibleSnapshot, AMS_ATT_UNSEEN);
    captions_MarkVisibleMessageStateofSeeing(ci, FALSE);
}

void FindRelatedMessages(self)
struct messages *self;
{
    captions_FindRelatedMessages(GetCaptions(self));
}

static char LastResendName[1+MAXPATHLEN] = "";

char *GetLastResendName() {return(LastResendName);}

void BSM_ReSendPlease(self, towhom)
struct messages *self;
char *towhom;
{
    char ShortName[1+MAXPATHLEN], Prompt[100+MAXPATHLEN];

    if (LastResendName[0]) {
	sprintf(Prompt, "Resend this message to [%s]: ", LastResendName);
    } else {
	strcpy(Prompt, "Resend this message to: ");
    }
    if (ISSTRING(towhom)) {
	strcpy(ShortName, towhom);
    } else {
	if (message_AskForString(NULL, 50, Prompt, "", ShortName, sizeof(ShortName)) < 0) return;
	if (ShortName[0] == '\0') strcpy(ShortName, LastResendName);
    }
    if (ShortName[0] == '\0') {
	message_DisplayString(NULL, 10, "Not resending the message -- no recipient specified.");
	return;
    }
    message_DisplayString(NULL, 10, "Resending message; please wait");
    ams_WaitCursor(TRUE);
    if (!ams_CUI_ResendMessage(ams_GetAMS(), GetCaptions(self)->VisibleCUID, ShortName)) {
	/* That routine reports its own errors & success, here we just remember the latest successful resend... */
	strcpy(LastResendName, ShortName);
    }
    ams_WaitCursor(FALSE);
}

void BSM_RedisplayFormat(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_Redisplay(c, c->CurrentFormatting ^ MODE822_FORMAT, NULL);
}

void BSM_RedisplayNormal(self)
struct messages *self;
{
    captions_Redisplay(GetCaptions(self), MODE822_NORMAL, NULL);
}

void BSM_RedisplayFixedWidth(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_Redisplay(c, c->CurrentFormatting ^ MODE822_FIXEDWIDTH, NULL);
}


void BSM_RedisplayRot13(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_Redisplay(c, c->CurrentFormatting ^ MODE822_ROT13, NULL);
}

void BSM_ShowRaw(self, ctype)
struct messages *self;
char *ctype;
{
    BSM_DifferentContentType(self, "text/plain");
}

void BSM_DifferentContentType(self, ctype)
struct messages *self;
char *ctype;
{
    char buf[1000];
    struct captions *c = GetCaptions(self);

    if (!ISSTRING(ctype)) {
	if (message_AskForString(NULL, 50, "Enter new content-type field: " , NULL, buf, sizeof(buf)) < 0) {
	    return;
	}
	ctype = buf;
    }
    captions_Redisplay(c, c->CurrentFormatting | MODE822_FORMAT, ctype);
}

void BSM_ModifiableBody(self) 
struct messages *self;
{
    struct text *text = (struct text *) t822view_GetDataObject(GetBodies(self));
    text_SetReadOnly(text, FALSE);
    text_NotifyObservers(text, 0);	/* Update menus and post key state */
}

void BSM_SelectBody(self) 
struct messages *self;
{
    struct t822view *tv = GetBodies(self);
    int bodstart = GetCaptions(self)->StartOfRealBody;

    t822view_SetDotPosition(tv, bodstart);
    t822view_SetDotLength(tv, text_GetLength((struct text *) t822view_GetDataObject(tv)) - bodstart);
}

void PuntCurrent(self, GoToNext)
struct messages *self;
int GoToNext;
{
    captions_PuntCurrent(GetCaptions(self), GoToNext);
}

void BSM_SendFresh(self)
struct messages *self;
{
    captions_SendMessage(GetCaptions(self), AMS_REPLY_FRESH);
}

void ReplySendersMarked(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);
    if (ClearSM(c)) return;
    captions_ActOnMarkedMessages(c, MARKACTION_REPLYSENDERS, NULL);
}

void ReplyAllMarked(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);
    if (ClearSM(c)) return;
    captions_ActOnMarkedMessages(c, MARKACTION_REPLYALL, NULL);
}

void ResendMarked(self, towhom)
struct messages *self;
char *towhom;
{
    char *s, Prompt[100+MAXPATHLEN], ShortResendAddress[1+MAXPATHLEN], *ResendAddress = NULL;

    s = GetLastResendName();
    if (*s) {
	sprintf(Prompt, "Resend these messages to [%s]: ", s);
    } else {
	strcpy(Prompt, "Resend these messages to: ");
    }
    if (ISSTRING(towhom)) {
	strcpy(ShortResendAddress, towhom);
    } else {
	if (message_AskForString(NULL, 50, Prompt, "", ShortResendAddress, sizeof(ShortResendAddress)) < 0) return;
	if (ShortResendAddress[0] == '\0') strcpy(ShortResendAddress, s);
    }
    if (ShortResendAddress[0] == '\0') {
	message_DisplayString(NULL, 10, "Not resending the messages -- no recipient specified.");
	return;
    }
    ams_WaitCursor(TRUE);
    if (ams_CUI_RewriteHeaderLine(ams_GetAMS(), ShortResendAddress, &ResendAddress)) {
	/* error reported */
	ams_WaitCursor(FALSE);
	return;
    }
    strcpy(s, ShortResendAddress); /* validated well, anyway */
    captions_ActOnMarkedMessages(GetCaptions(self), MARKACTION_RESEND, ResendAddress);
    if (ResendAddress) free(ResendAddress);
    ams_WaitCursor(FALSE);
}

void ExcerptMarked(self)
struct messages *self;
{
    captions_ActOnMarkedMessages(GetCaptions(self), MARKACTION_EXCERPT, NULL);
}

void ClearMarks(self)
struct messages *self;
{
    captions_ClearMarks(GetCaptions(self));
}

void CountMarks(self)
struct messages *self;
{
    char ErrorText[100];
    int ct = GetCaptions(self)->MarkCount;

    if (ct == 1) {
	strcpy(ErrorText, "There is one marked message.");
    } else {
	sprintf(ErrorText, "There are %s marked messages.", amsutil_cvEng(ct, 0, 1000));
    }
    message_DisplayString(NULL, 10, ErrorText);
}

void RestoreOldMarks(self)
struct messages *self;
{
    struct captions *c = GetCaptions(self);

    captions_GuaranteeFetchedRange(c, 0, c->FolderSize);
    captions_ActOnMarkedMessages(c, MARKACTION_RESTORE, NULL);
}

void FindAllCaptions(self)
struct messages *self;
{
    captions_SearchAll(GetCaptions(self));
}

void MarkRange(self)
struct messages *self;
{
    captions_MarkRangeOfMessages(GetCaptions(self));
}

void ExpandFileIntoMenus(self)
struct messages *self;
{
    captions_AlterFileIntoMenus(GetCaptions(self), FALSE);
}

void ShrinkFileIntoMenus(self)
struct messages *self;
{
    captions_AlterFileIntoMenus(GetCaptions(self), TRUE);
}

void BSM_MorePlease(self)
struct messages *self;
{
    captions_ShowMore(GetCaptions(self), TRUE, TRUE, FALSE);
}

void BSM_NextPlease(self)
struct messages *self;
{
    captions_ShowMore(GetCaptions(self), FALSE, TRUE, FALSE);
}

void BSM_ScrollBackBody(self)
struct messages *self;
{
    messtextv_ScrollScreenBackCmd(GetBodies(self));
}

void BSM_ScrollForwardBody(self)
struct messages *self;
{
    messtextv_ScrollScreenForwardCmd(GetBodies(self));
}

ReadByName(self) 
struct messages *self;
{
    char ShortName[256];

    if (ams_GetFolderName("Which folder do you want to read? ", ShortName, sizeof(ShortName)-1, "", TRUE)) return(-1);
    return(ReadNamedFolder(self, ShortName));
}

ReadNamedFolder(self, ShortName)
struct messages *self;
char *ShortName;
{
    char *FullName;
    int code;

    if (ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &FullName)) {
	if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
	    char ErrorText[1000];

	    sprintf(ErrorText, "%s is private; you do not have read-access.", ShortName);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, ams_mserrcode(ams_GetAMS()));
	} else {
	    ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
	}
	return(-1);
    }
    code = captions_InsertUpdatesInDocument(GetCaptions(self), ShortName, FullName, FALSE);
    if (!code) message_DisplayString(NULL, 10, "Done.");
    return(code);
}

void BSM_RenamePlease(self)
struct messages *self;
{
    char ShortName[1+MAXPATHLEN], *FullName, NewName[1+MAXPATHLEN];

    if (message_AskForString(NULL, 50, "Rename old folder: ", "", ShortName, sizeof(ShortName)) < 0) return;
    if (ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &FullName)) {
	ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
	return;
    }
    if (message_AskForString(NULL, 50, "New name: ", "", NewName, sizeof(NewName)) < 0) return;	
    ams_WaitCursor(TRUE);
    message_DisplayString(NULL, 10, "Renaming folder; please wait...");
    im_ForceUpdate();
    ams_CUI_RenameDir(ams_GetAMS(), FullName, NewName); /* routine reported errors */
    ams_WaitCursor(FALSE);
}

#ifndef NOTREESPLEASE
GenNodeName(tree, node, buf)
struct tree *tree;
struct tree_node *node;
char *buf;
{
    char *s;

    if (!node) return; /* Bottom out recursion */
    s =((node) ? node->name : NULL);
    if (!strncmp(s, "ROOT:", 5)) return; /* Another bottoming out */
    GenNodeName(tree, ((node) ? node->parent : NULL), buf);
    if (buf[0]) strcat(buf, ".");
    strcat(buf, s);
}

OrgHit(self, folderTreeView, node, action, x, y, clicks)
struct messages *self;
struct foldertreev *folderTreeView;
struct tree_node *node;
enum view_MouseAction action;
long x, y, clicks;
{
    char Buf[1+MAXPATHLEN];
    struct org *org = NULL;

    if (action == view_RightDown) {
	org = (struct org*)foldertreev_GetDataObject(folderTreeView);
	Buf[0] = NULL;
	GenNodeName(org->tree_data_object, node, Buf);
	ReadNamedFolder(self, Buf);
    }
}

countdots(s)
char *s;
{
    int dots = 0;

    while (*s) {
	if (*s++ == '.') ++dots;
    }
    return(dots);
}
#endif

void
messages__ObservedChanged( self, changed, change )
register struct messages   *self;
register struct observable *changed;
register long		      change;
{
    super_ObservedChanged(self,changed,change);
    switch(change) {
	case 1:
	    if(self->folderTree && ((struct foldertreev*)changed == self->folderTree)) {
		struct org *o=(struct org *)foldertreev_GetDataObject(self->folderTree);
		struct im *im = frame_GetIM(self->folderFrame);
		frame_SetView(self->folderFrame,NULL);
		im_SetView(im,NULL);
		frame_Destroy(self->folderFrame);
		im_Destroy(im);
		foldertreev_Destroy(self->folderTree);
		org_Destroy(o);
		self->folderTree = NULL;
		self->folderFrame = NULL;
		folderTreeExists--;
	    }
    }
}

void BSM_ShowTreePlease(self)
struct messages *self;
{
#ifndef NOTREESPLEASE
    struct org *o;
    struct foldertreev *folderTreeView;
    FILE *fp, *mfp;
    char PathElt[1+MAXPATHLEN], MapFile[1+MAXPATHLEN], ErrorText[100+MAXPATHLEN], RemoteMapFile[1+MAXPATHLEN], LineBuf[2000], OrgFileName[1+MAXPATHLEN], RootName[1+MAXPATHLEN], *s, LastName[1+MAXPATHLEN];
    int path, len1, len2;
    long errcode;

    ams_WaitCursor(TRUE);
    if(self->folderTree || (folderTreeExists > 0)) {
	message_DisplayString(self, 10, "A folder tree is already exposed.");
	ams_WaitCursor(FALSE);
	return;
    }
    ams_CUI_GenTmpFileName(ams_GetAMS(), OrgFileName);
    if ((fp = fopen(OrgFileName, "w")) == NULL) {
	message_DisplayString(self, 10, "Could not open output file.");
	ams_WaitCursor(FALSE);
	return;
    }
    fputs("Folder Tree\n{\n", fp);
    o = org_New();
    self->folderTree = folderTreeView = foldertreev_New();
    if (!o || !folderTreeView) {
	message_DisplayString(self, 10, "Could not create tree objects!");
	fclose(fp);
	ams_WaitCursor(FALSE);
	return;
    }
    foldertreev_SetDataObject(folderTreeView, o);
    foldertreev_SetHitHandler(folderTreeView, OrgHit, self);
    PathElt[0] = '\0';
    for (path=0; ; ++path) {
	if (ams_MS_GetSearchPathEntry(ams_GetAMS(), path, PathElt, MAXPATHLEN)) {
	    break;
	}
	if (ams_MS_NameSubscriptionMapFile(ams_GetAMS(), PathElt, MapFile)) {
	    sprintf(ErrorText, "MS can not generate subscription map file for %s", PathElt);
	    if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
		ams_ReportSuccess(ams_GetAMS(), ErrorText);
		continue; /* user may not have his own message dir, for example */
	    }
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, TRUE, errcode);
	    continue;
	}
	if (!ams_CUI_OnSameHost(ams_GetAMS())) {
	    strcpy(RemoteMapFile, MapFile);
	    ams_CUI_GenTmpFileName(ams_GetAMS(), MapFile);
	    if ((errcode = ams_CUI_GetFileFromVice(ams_GetAMS(), MapFile, RemoteMapFile)) != 0) {
		sprintf(ErrorText, "Cannot copy map file %s from server file %s", MapFile, ams_ap_Shorten(ams_GetAMS(), RemoteMapFile));
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, errcode);
		continue;
	    }
	    ams_MS_UnlinkFile(ams_GetAMS(), RemoteMapFile);
	}
	if ((mfp = fopen(MapFile, "r")) == NULL) {
	    sprintf(ErrorText, "Cannot open map file %s (for %s - error %d)", MapFile, PathElt, errno);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, FALSE, 0);
	    unlink(MapFile);
	    continue;
	}
	strcpy(RootName, PathElt);
	s = rindex(RootName, '/');
	if (s) *s = '\0';
	s = rindex(RootName, '/');
	if (s) {
	    *s++ = '\0';
	} else {
	    s = RootName;
	}
	fputs("ROOT: ", fp);
	fputs(s, fp);
	fputs("\n{\n", fp);
	LastName[0] = '\0';
	while(fgets(LineBuf, sizeof(LineBuf), mfp)) {
	    s = index(LineBuf, ':');
	    if (s) *s = '\0';
	    len1 = strlen(LineBuf);
	    len2 = strlen(LastName);
	    if ((len2 < len1)
		&& LineBuf[len2] == '.'
		&& !strncmp(LineBuf, LastName, len2)) {
		fputs("{\n", fp);
	    } else {
		len1 = countdots(LastName) - countdots(LineBuf);
		while (len1 > 0) {
		    fputs("}\n", fp);
		    --len1;
		}
		/* This next line is only necessary if there are oddities in the map file, but it has happened before... */
		while (len1 < 0) {
		    fputs("{\n", fp);
		    ++len1;
		}
	    }
	    s = rindex(LineBuf, '.');
	    if (s) {
		++s;
	    } else {
		s = LineBuf;
	    }
	    fputs(s, fp);
	    fputs("\n", fp);
	    strcpy(LastName, LineBuf);
	}
	fclose(mfp);
	unlink(MapFile); 
	len1 = countdots(LastName);
	while (len1 > 0) {
	    fputs("}\n", fp);
	    --len1;
	}
	fputs("}\n", fp);
    }
    fputs("}\n", fp);
    fclose(fp);
    fp = fopen(OrgFileName, "r");
    if (!fp) {
	message_DisplayString(self, 10, "Could not read map file.");
	ams_WaitCursor(FALSE);
	return;
    }
    org_Read(o, fp, 0);
    fclose(fp);
    unlink(OrgFileName); 
    self->folderFrame = ams_InstallInNewWindow(foldertreev_GetApplicationLayer(folderTreeView), "messages-tree", "Folder Tree", environ_GetProfileInt("foldertree.width", 600), environ_GetProfileInt("foldertree.height", 400), folderTreeView);
    foldertreev_AddObserver(self->folderTree,self);
    folderTreeExists++;
    ams_WaitCursor(FALSE);
#endif
}


void SubscribeByName(self)
struct messages *self;
{
    char buf[1+MAXPATHLEN];

    if (ams_GetFolderName("Subscribe to what folder: " , buf, sizeof(buf), NULL, TRUE) == 0) {
	SetSubStatus(buf, AMS_ALWAYSSUBSCRIBED);
    }
}

void UnSubscribeByName(self)
struct messages *self;
{
    char buf[1+MAXPATHLEN];

    if (ams_GetFolderName("Unsubscribe to what folder: " , buf, sizeof(buf), NULL, TRUE) == 0) {
	SetSubStatus(buf, AMS_UNSUBSCRIBED);
    }
}

void AlterSubByName(self)
struct messages *self;
{
    char buf[1+MAXPATHLEN];

    if (ams_GetFolderName("Alter subscription to what folder: " , buf, sizeof(buf), NULL, TRUE) == 0) {
	SetSubStatus(buf, amsutil_ChooseNewStatus(buf, AMS_ALWAYSSUBSCRIBED, TRUE));
    }
}

SetSubStatus(nickname, substatus)
char *nickname;
int substatus;
{
    long errcode;
    char *FullName, ErrorText[150+MAXPATHLEN];

    ams_WaitCursor(TRUE);
    if (ams_CUI_DisambiguateDir(ams_GetAMS(), nickname, &FullName)) {
	ams_CUI_ReportAmbig(ams_GetAMS(), nickname, "folder");
	return;
    }
    if ((errcode = ams_MS_SetSubscriptionEntry(ams_GetAMS(), FullName, nickname, substatus)) != 0) {
	ams_WaitCursor(FALSE);
	if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
	    sprintf(ErrorText, "'%s' is private; you don't have read-access or are unauthenticated.", nickname);
	} else if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS()))) {
	    sprintf(ErrorText, "%s: temporarily unavailable (net/server problem)", nickname);
	} else if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
	    sprintf(ErrorText, "Sorry; %s no longer exists, so you cannot subscribe to it.", nickname);
	} else {
	    sprintf(ErrorText, "Cannot set subscription entry to %s", nickname);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, errcode);
	    return;
	}
	message_DisplayString(NULL, 75, ErrorText);
	return;
    }
    ams_SubscriptionChangeHook(FullName, nickname, substatus, NULL);
    ams_WaitCursor(FALSE);
}
static char lastWindowWarning[] =
"This is the last window.";
static char *lastWindowChoices[] = {
    "Continue Running",
    "Quit Messages",
    NULL};

#define lastWindow_CANCEL 0
#define lastWindow_QUIT   1

    void DeleteWindow(self)
      struct messages *self;
    {
	if (ams_CountAMSViews() > 1) {
	    if (self->WhatIAm == WHATIAM_CAPTIONS) {
		captions_ClearAndUpdate((struct captions *)self, FALSE, TRUE);
	    }
	    ams_CommitState(FALSE, FALSE, FALSE, FALSE);
	    if (messages_GetIM(self)) im_Destroy(messages_GetIM(self));
	    messages_Destroy(self);
	}
	else {
	    long answer;
	    if (message_MultipleChoiceQuestion(NULL, 0,
					       lastWindowWarning, lastWindow_CANCEL,
					       &answer, lastWindowChoices, NULL)
		== -1)
		return;
	    switch(answer){
		case lastWindow_CANCEL:
		    return;

		case lastWindow_QUIT :
		    QuitMessages(self);
	    }
	}
    }

    DirectlyClassify(self, classnum)
      struct messages *self;
    int classnum;
    {
	int cuid, OpCode;
	char WhichClass[1+MAXPATHLEN], *s, *t;
	struct captions *c = GetCaptions(self);

	cuid = c->VisibleCUID;    
	OpCode = AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_COPYDEL : MS_CLONE_COPY;
	if (cuid < 1) {
	    message_DisplayString(NULL, 10, "There is no message on display.");
	    return(0);
	}
	ams_WaitCursor(TRUE);
	strcpy(WhichClass, ams_GetClassListEntry(classnum));
	s = index(WhichClass, '~');
	if (s) *s = '\0';
	t = WhichClass;
	if (*t == '*') {
	    ++t;
	    OpCode = AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY) ? MS_CLONE_APPENDDEL : MS_CLONE_APPEND;
	}
	if (!ams_CUI_CloneMessage(ams_GetAMS(), cuid, t, OpCode)) {
	    /* cuilib reports errors, here we deal with success */
	    if (OpCode == MS_CLONE_APPENDDEL || OpCode == MS_CLONE_COPYDEL) {
		text_SetEnvironmentStyle(c->CaptText, c->HighlightEnv, c->ActiveDeletedStyle);
		AMS_SET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_DELETED);
		captions_WantUpdate(c, c);
		captions_AlterDeletedIcon(c, c->HighlightStartPos, TRUE);
		captions_PostMenus(c, NULL);
		messages_PostMenus(self, NULL);
	    }
	}
	ams_WaitCursor(FALSE);
    }

    boolean messages__InitializeObject(c, mess)
      struct classheader *c;
    struct messages *mess;
    {
	mess->fileintomenulist = NULL;
	mess->mykeys = keystate_Create(mess, messages_standardkeymap);
	mess->mymenulist = menulist_DuplicateML(messages_standardmenulist, mess);
	mess->mypermkeys = keystate_Create(mess, messages_permkeymap);
	mess->mypermmenulist = menulist_DuplicateML(messages_permmenulist, mess);
	mess->expandedmenuslist = NULL;
	mess->WhatIAm = WHATIAM_UNDEFINED;
	mess->folderTree = NULL;
	mess->folderFrame = NULL;
	return(TRUE);
    }

    CheckMenuMasks(self)
      struct messages *self;
    {
	long mymask;
	struct captions *c; 

	mymask = ((amsutil_GetOptBit(EXP_SHOWMORENEXT) ? MENUMASK_SHOWMORENEXT : 0)
		  | (amsutil_GetOptBit(EXP_SETQUITHERE) ? MENUMASK_SETQUITHERE  : 0)
		  | (amsutil_GetOptBit(EXP_MARKING) ? MENUMASK_MARKING  : 0)
		  | (amsutil_GetOptBit(EXP_FILEINTO) ? MENUMASK_FILEINTO : 0)
		  | (amsutil_GetOptBit(EXP_THREEREPLIES) ? MENUMASK_THREEREPLIES : MENUMASK_NOTTHREEREPLIES)
		  | (amsutil_GetOptBit(EXP_MARKASUNREAD) ? MENUMASK_MARKASUNREAD : 0)
		  | (amsutil_GetOptBit(EXP_FORMATMENUS) ? MENUMASK_FORMATMENUS : 0)
		  | (amsutil_GetOptBit(EXP_APPENDBYNAME) ? MENUMASK_APPENDBYNAME : 0)
		  | (amsutil_GetOptBit(EXP_FILEINTOMENU) ? MENUMASK_FILEINTOMENU : 0)
		  | (amsutil_GetOptBit(EXP_MARKEDEXTRAS) ? MENUMASK_MARKEDEXTRAS : 0)
		  | (amsutil_GetOptBit(EXP_PUNTMENU) ? MENUMASK_PUNTMENU : 0));
	c = GetCaptionsNoCreate(self);
	if (c) {
	    if (c->VisibleCUID > 0) {
		mymask |= MENUMASK_MSGSHOWING;
		if (AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_MAYMODIFY)) {
		    mymask |= MENUMASK_MSGWRITABLE;
		}
		if (AMS_GET_ATTRIBUTE(c->VisibleSnapshot, AMS_ATT_DELETED)) {
		    mymask |= MENUMASK_MSGDELETED;
		} else {
		    mymask |= MENUMASK_MSGNOTDELETED;
		}
	    }
	    if (c->MarkCount > 0) mymask |= MENUMASK_HASMARKS;
	    if (!self->fileintomenulist) messages_ResetFileIntoMenus(self);
	    if(self->fileintomenulist) menulist_ClearChain(self->fileintomenulist);
	    if(self->expandedmenuslist) {
		if (c->MenusExpanded) {
		    menulist_ClearChain(self->expandedmenuslist);
		    menulist_ChainBeforeML(self->fileintomenulist, self->expandedmenuslist, self->expandedmenuslist);
		    menulist_SetMask(self->expandedmenuslist, mymask);
		}
	    }
	    menulist_SetMask(self->fileintomenulist, mymask);
	}
	menulist_SetMask(self->mymenulist, mymask);
    }

    void BSM_NextDigest(self)
      struct messages *self;
    {
	struct t822view *tv = (struct t822view *) GetBodies(self);
	struct text *t = (struct text *) t822view_GetDataObject(tv);
	struct SearchPattern *Pattern = NULL;
	char *tp = search_CompilePattern("\012--", &Pattern);
	int tmploc;

	if (tp) {
	    message_DisplayString(NULL, 10, tp);
	} else {
	    tmploc = search_MatchPattern(t, t822view_GetDotPosition(tv), Pattern);
	    if (tmploc >= 0) {
		t822view_SetDotPosition(tv, ++tmploc);
		t822view_SetTopPosition(tv, tmploc);
	    }
	}
    }

    void BSM_PrevDigest(self)
      struct messages *self;
    {
	struct t822view *tv = (struct t822view *) GetBodies(self);
	struct text *t = (struct text *) t822view_GetDataObject(tv);
	struct SearchPattern *Pattern = NULL;
	char *tp = search_CompilePattern("\012--", &Pattern);
	int tmploc;

	if (tp) {
	    message_DisplayString(NULL, 10, tp);
	} else {
	    tmploc = search_MatchPatternReverse(t, t822view_GetDotPosition(tv) - 2, Pattern);
	    if (tmploc >= 0) {
		t822view_SetDotPosition(tv, ++tmploc);
		t822view_SetTopPosition(tv, tmploc);
	    }
	}
    }
