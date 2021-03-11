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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/folders.c,v 1.46 1994/01/17 16:57:02 rr2b Exp $";
#endif


 

#include <andrewos.h> /* sys/file.h */
#include <cui.h>
#include <fdphack.h>
#include <hdrparse.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netinet/in.h>  /* for htonl, etc. */
#include <errprntf.h>
#include <ctype.h>
#include <rect.h>

#include <text.ih>
#include <message.ih>
#include <envrment.ih>
#include <bind.ih>
#include <proctbl.ih>
#include <sbutton.ih>
#include <sbuttonv.ih>
#include <environ.ih>
#include <lpair.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <style.ih>
#include <fontdesc.ih>

#include <ams.ih>
#include <amsutil.ih>
#include <sendmsg.ih>
#include <captions.ih>
#include <folders.eh>
#include <msgsvers.h>

int foldersDebugging=0;
#define mdebug(n, x) ((foldersDebugging & (n)) ? printf x : 0)
#ifdef DOTIMING
extern int GlobalTransactionCount;
#else /* DOTIMING */
#define Logstat(x,y,z)
#endif /* DOTIMING */

static struct keymap *folders_standardkeymap;
static struct menulist *folders_standardmenulist;

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

static char *ForgetItString = "Forget it -- do nothing.";

extern void folders_Warp(), folders_Expose(), folders_Hide(), folders_Vanish(), folders_ForceUpdate();

extern void folders_SimulateLeftClick();
extern void folders_UpFocus(), folders_DownFocus();
extern void folders_TextviewCompound();
extern void folders_FoldersCompound();
extern void FoldersTextviewCommand();
extern void FoldersSendmessageCommand();
extern void FoldersCaptionsCommand();
extern int ConsiderResettingDescription();

void folders__HandleAsyncPrefetch(ci)
struct folders *ci;
{
    int which, skipped;

    if (ci->CurrentConfiguration != LIST_ALL_FOLDERS) {
	folders_FindNextFolder(ci, &which, &skipped);
	if (which < ci->MainDirCacheCount) {
	    char Text[60+MAXPATHLEN];

	    sprintf(Text, "Starting pre-fetch of %s... ", ci->MainDirCache[which].ShortName);
	    message_DisplayString(NULL, 10, Text);
	    folders_ForceUpdate();
	    ams_MS_PrefetchMessage(ams_GetAMS(), ci->MainDirCache[which].FullName, NULL, FALSE);
	    strcat(Text, "done.");
	    message_DisplayString(NULL, 10, Text);
	    folders_ForceUpdate();
	}
    }
    ci->NeedsToPrefetch = FALSE;
}


char *
WhichIcon(substatus)
int substatus;
{
	char *whichicon;

	switch (substatus) {
	    case AMS_ASKSUBSCRIBED:
		whichicon = SICON_SUB_ASK;
		break;
	    case AMS_ALWAYSSUBSCRIBED:
		whichicon = SICON_SUB_NORM;
		break;
	    case AMS_SHOWALLSUBSCRIBED:
		whichicon = SICON_SUB_ALL;
		break;
	    case AMS_PRINTSUBSCRIBED:
		whichicon = SICON_SUB_PRINT;
		break;
	    default:
	    case AMS_UNSUBSCRIBED:
		whichicon = SICON_SUB_NONE;
		break;
	}
	return(whichicon);
}

void folders__ShowHelp(self)
struct folders *self;
{
    char InitString[50];
    int pos, len;
    static char *blurb1 = "by N. Borenstein\n";
    static char *altblurb1 = "by N. Borenstein and R. Glickstein\n";
    static char *blurb2 = "a multi-media interface to the Andrew Message System\n";
    static char *blurb3 = "also by J. Rosenberg, C. Everhart, A. Stoller, and R. Glickstein.\n\n";
    static char *altblurb3 = "also by J. Rosenberg, C. Everhart, and A. Stoller.\n\n";
    static int bobgvanity = -1;
    struct environment *et;
    struct text *mytext = (struct text *) folders_GetDataObject(self);

    ClearFolders(self);
    sprintf(InitString, "Messages %d.%d\n", MESSAGES_MAJOR_VERSION, MESSAGES_MINOR_VERSION);
    len = strlen(InitString);
    text_AlwaysInsertCharacters(mytext, 0, InitString, len);
    et = environment_InsertStyle(mytext->rootEnvironment, 0, self->BigCenterStyle, 1);
    environment_SetLength(et, len-1);
    pos = len;
    if (bobgvanity < 0) {
        bobgvanity = environ_GetProfileInt("bobgvanity", 0);
    }
    len = strlen(bobgvanity ? altblurb1 : blurb1);
    text_AlwaysInsertCharacters(mytext, pos, bobgvanity ? altblurb1 : blurb1, len);
    et = environment_InsertStyle(mytext->rootEnvironment, pos, self->SmallCenterStyle, 1);
    environment_SetLength(et, len-1);
    pos += len;
    len = strlen(blurb2);
    text_AlwaysInsertCharacters(mytext, pos, blurb2, len);
    et = environment_InsertStyle(mytext->rootEnvironment, pos, self->CenterStyle, 1);
    environment_SetLength(et, len-1);
    pos += len;
    len = strlen(bobgvanity ? altblurb3 : blurb3);
    text_AlwaysInsertCharacters(mytext, pos, bobgvanity ? altblurb3 : blurb3, len);
    et = environment_InsertStyle(mytext->rootEnvironment, pos, self->SmallCenterStyle, 1);
    environment_SetLength(et, len-1);

    folders_WantUpdate(self, self);
}

struct view *
folders__Hit(folders, action, x, y, nclicks)
struct folders *folders;
int x, y, nclicks;  
enum view_MouseAction action;
{
    if (action != view_LeftUp && action != view_RightUp) return((struct view *) folders);
    super_Hit(folders, view_LeftDown, x, y, 1);
    folders_SimulateClick(folders, (action == view_LeftUp));
    return((struct view *) folders);
}

void folders__SimulateClick(folders, IsLeftClick)
struct folders *folders;
boolean IsLeftClick;
{
    DoClick(folders, IsLeftClick, FALSE);
}

DoClick(folders, IsLeftClick, IgnorePosition)
struct folders *folders;
boolean IsLeftClick, IgnorePosition;
{
    int linestart, hitpos, i;

    folders_WantInputFocus(folders, folders);
    if (folders->MainDirCacheCount<=0) return;
    if (text_GetLength((struct text *) folders_GetDataObject(folders)) <= 0) return;
    hitpos = folders_GetDotPosition(folders);
    for (i=0; i<folders->MainDirCacheCount; ++i) {
	if (folders->MainDirCache[i].pos > hitpos) break;
    }
    --i;
    linestart = folders->MainDirCache[i].pos;
    folders_SetDotPosition(folders, linestart);
    folders_SetDotLength(folders, 0);
    if ((!IgnorePosition && (hitpos - linestart) < 4) || !IsLeftClick) {
	int myoffset;

	myoffset = hitpos - linestart;
	if ((myoffset > 1) && (myoffset < 5) && text_GetChar((struct text *) folders_GetDataObject(folders), linestart + 2) == ICON_FOLDER) {
	    if (IsLeftClick || !amsutil_GetOptBit(EXP_MARKING)) {
		captions_FileCurrent(folders_GetCaptions(folders), folders->MainDirCache[i].FullName, folders->MainDirCache[i].ShortName);
	    } else {
		captions_FileMarked(folders_GetCaptions(folders), folders->MainDirCache[i].FullName, folders->MainDirCache[i].ShortName);
	    }
	} else {
	    folders_ActionHit(folders, folders->MainDirCache[i].substatus, folders->MainDirCache[i].FullName, folders->MainDirCache[i].ShortName);
	}
	return;
    }
    ams_WaitCursor(TRUE);
    captions_InsertUpdatesInDocument(folders_GetCaptions(folders), folders->MainDirCache[i].ShortName, folders->MainDirCache[i].FullName, FALSE);
    ExposeCap(folders);
    ams_WaitCursor(FALSE);
}

RemoveFromBEDirCache(ci, longname, shortname)
struct folders *ci;
char *longname, *shortname;
{
    int i;

    for (i=0; i<ci->MainDirCacheCount; ++i) {
	if (!strcmp(longname, ci->MainDirCache[i].FullName)) {
	    struct BEDirCache *bdc;

	    /* found it */
	    bdc = &ci->MainDirCache[i];
	    if (ci->HighlightEnv == ci->MainDirCache[i].env) {
		ci->HighlightEnv = NULL;
	    }
	    if (bdc->FullName) free (bdc->FullName);
	    if (bdc->ShortName) free (bdc->ShortName);
	    if (bdc->len) {
		text_AlwaysDeleteCharacters((struct text *) folders_GetDataObject(ci), bdc->pos, bdc->len);
	    }
	    UpdateBEDirCachePositions(ci, bdc, -bdc->len);
	    while (i< ci->MainDirCacheCount - 1) {
		bcopy(&ci->MainDirCache[i+1], &ci->MainDirCache[i], sizeof(struct BEDirCache));
		++i;
	    }
	    --ci->MainDirCacheCount;
	    return;
	}
    }
}

AddToBEDirCache(ci, longname, shortname, substatus)
struct folders *ci;
char *longname, *shortname;
int substatus;
{
    int i = ci->MainDirCacheCount, j;

    ci->MainDirCache[i].pos = text_GetLength((struct text *) folders_GetDataObject(ci));
    if (longname == NULL) longname = "";
    ci->MainDirCache[i].FullName = malloc(1+strlen(longname));
    strcpy(ci->MainDirCache[i].FullName, longname);
    ci->MainDirCache[i].ShortName = malloc(1+strlen(shortname));
    strcpy(ci->MainDirCache[i].ShortName, shortname);
    ci->MainDirCache[i].substatus = substatus;
    if (++ci->MainDirCacheCount >= ci->MainDirCacheSize) {
	j = (ci->MainDirCacheSize * 2) - (ci->MainDirCacheSize / 2);
	ci->MainDirCache = (struct BEDirCache *) realloc(ci->MainDirCache, j * sizeof(struct BEDirCache));
	bzero(&ci->MainDirCache[ci->MainDirCacheSize], (j-ci->MainDirCacheSize) * sizeof(struct BEDirCache));
	ci->MainDirCacheSize = j;
    }
    return(i);
}

UpdateBEDirCachePositions(ci, entry, addedlen)
struct folders *ci;
struct BEDirCache *entry;
int addedlen;
{
    Boolean FoundIt = FALSE;
    int i;

    for (i=0; i<ci->MainDirCacheCount; ++i) {
	if (FoundIt) {
	    ci->MainDirCache[i].pos += addedlen;
	    ci->MainDirCache[i].commentstart += addedlen;
	} else if (&ci->MainDirCache[i] == entry) {
	    entry->len += addedlen;
	    FoundIt = TRUE;
	}
    }
}

SetupList(ci, code, thingstoread)
struct folders *ci;
int code;
char *thingstoread[];
{
    char *whattofree;
    char PathElt[MAXPATHLEN+1], MapFile[MAXPATHLEN+1], RemoteMapFile[MAXPATHLEN+1], ErrorText[256], *s=NULL, *shortname, *longname, *nextline, *t;
    int spcode = 0, substatus = 0, ShowUnsubscribed = 0, pathindex = 0;
    int HasNew;
    FILE *mfp;
    struct stat stbuf;
    long errcode;
    boolean HasCleared = FALSE;

    if (ci->HasSetUp) {
	ConsiderResettingDescription(ci, code, FALSE);
	return(0);
    }
    switch(code) {
	case LIST_ALL_FOLDERS:
	    ShowUnsubscribed = 1;
	    pathindex = 0;
	    break;
	case LIST_SUBSCRIBED:
	    ShowUnsubscribed = 0;
	    pathindex = 0;
	    break;
	case LIST_MAIL_FOLDERS:
	    ams_InitializeClassList();
	    ShowUnsubscribed = 1;
	    pathindex = AMS_MAILPATH;
	    break;
	case LIST_AS_REQUESTED:
	    break;
	case LIST_NEWONLY:
	    message_DisplayString(NULL, 10, "Checking to see which folders have new messages...");
	    folders_ForceUpdate();
	    break;
	default:
	    ams_ReportError(ams_GetAMS(), "Internal error --invalid parameter to setup folder list", ERR_CRITICAL, FALSE, 0);
	    return(-1);
    }

    do {
	if (code == LIST_NEWONLY) {
	    int changed, unavail, missing, slow, fast;

	    errcode = ams_MS_NameChangedMapFile(ams_GetAMS(), MapFile, ci->MailOnlyMode, TRUE, &changed, &unavail, &missing, &slow, &fast);
	    if (errcode) {
		ams_ReportError(ams_GetAMS(), "Cannot get list of changed folders", ERR_WARNING, TRUE, errcode);
		return(-1);
	    }
	    if (unavail > 0) {
		char Foobar[100];
		sprintf(ErrorText, "%s of ", amsutil_cvEng(changed, 1, 100));
		sprintf(Foobar, "%s subscriptions changed (%d unavailable, %d unchanged.).", amsutil_cvEng(slow+fast+unavail, 0, 100), unavail, slow+fast-changed);
		strcat(ErrorText, Foobar);
	    } else {
		char Foobar[100];
		sprintf(ErrorText, "%s of your ", amsutil_cvEng(changed, 1, 100));
		sprintf(Foobar, "%s subscriptions have changed (%d have nothing new).", amsutil_cvEng(slow+fast, 0, 100), slow+fast-changed);
		strcat(ErrorText, Foobar);
	    }
	    message_DisplayString(ci, 10, ErrorText); /* Force this message in right window */
	} else if (code == LIST_AS_REQUESTED) {
	    if (!thingstoread) break;
	} else {
	    if (code == LIST_SUBSCRIBED) {
		PathElt[0] = '\0';
	    } else {
		spcode = ams_MS_GetSearchPathEntry(ams_GetAMS(), pathindex, PathElt, MAXPATHLEN);
		if (pathindex != AMS_MAILPATH) ++pathindex;
		if (spcode) break;
	    }
	    if ((errcode = ams_MS_NameSubscriptionMapFile(ams_GetAMS(), PathElt, MapFile)) != 0) {
		sprintf(ErrorText, "MS can not generate subscription map file for %s", PathElt[0] ? PathElt : "subscribed list");
		if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
		    ams_ReportSuccess(ams_GetAMS(), ErrorText);
		    continue; /* user may not have his own message dir, for example */
		}
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, TRUE, errcode);
		continue;
	    }
	}
	if (code == LIST_AS_REQUESTED) {
	    int i;
	    for (i=0; thingstoread[i]; ++i) {
		AddSetupItem(ci, NULL, thingstoread[i], -1, FALSE, 1, &HasCleared);
	    }
	} else {
	    if (!ams_CUI_OnSameHost(ams_GetAMS()) && code != LIST_AS_REQUESTED) {
		strcpy(RemoteMapFile, MapFile);
		ams_CUI_GenTmpFileName(ams_GetAMS(), MapFile);
		if ((errcode = ams_CUI_GetFileFromVice(ams_GetAMS(), MapFile, RemoteMapFile)) != 0) {
		    sprintf(ErrorText, "Cannot copy map file %s from server file %s", MapFile, ams_ap_Shorten(ams_GetAMS(), RemoteMapFile));
		    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, errcode);
		}
		ams_MS_UnlinkFile(ams_GetAMS(), RemoteMapFile);
	    }
	    if ((mfp = fopen(MapFile, "r")) == NULL) {
		sprintf(ErrorText, "Cannot open map file %s (for %s - error %d)", MapFile, PathElt[0] ? PathElt : "subscribed list", errno);
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, FALSE, 0);
		unlink(MapFile);
		continue;
	    }
	    mdebug(4, ("Opened map file %s, reading it...\n", MapFile));
	    if (fstat(fileno(mfp), &stbuf)) {
		sprintf(ErrorText, "Cannot stat map file %s (for %s) - error %d", MapFile, PathElt[0] ? PathElt : "subscribed list", errno);
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, FALSE, 0);
		fclose(mfp);
		unlink(MapFile);
		continue;
	    }
	    s = malloc(1+stbuf.st_size);
	    if (!s) {
		sprintf(ErrorText, "Cannot allocate memory for map file %s (for %s)", MapFile, PathElt[0] ? PathElt : "subscribed list");
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, FALSE, 0);
		fclose(mfp);
		unlink(MapFile);
		continue;
	    }
	    whattofree = s;
	    if (read(fileno(mfp), s, stbuf.st_size) != stbuf.st_size) {
		sprintf(ErrorText, "Cannot read map file %s (for %s - error %d)", MapFile, PathElt[0] ? PathElt : "subscribed list", errno);
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_CRITICAL, FALSE, 0);
		free(whattofree);
		fclose(mfp);
		unlink(MapFile);
		continue;
	    }
	    s[stbuf.st_size] = '\0';
	    mdebug(4, ("Read map file %s, parsing it...\n", MapFile));
	    for ( ; *s; s = nextline) {
		nextline = index(s, '\n');
		if (nextline) *nextline++ = '\0';
		mdebug(4, ("Parsing %s\n", s));
		longname = index(s, ':');
		if (longname) {
		    *longname ++ = '\0';
		} else {
		    longname = s;
		}
		shortname = s;
		HasNew = 1;
		s = index(longname, ' ');
		if (s) {
		    *s++ = '\0';
		    t = index(s, ' ');
		    if (t) {
			*t++ = '\0';
			HasNew = atoi(t);
		    }
		    substatus = atoi(s);
		} else {
		    substatus = AMS_UNSUBSCRIBED;
		}
		if (substatus == AMS_UNSUBSCRIBED && !ShowUnsubscribed) continue;
		if (!HasNew && substatus != AMS_ASKSUBSCRIBED && substatus != AMS_SHOWALLSUBSCRIBED) continue;
		AddSetupItem(ci, longname, shortname, substatus, (code == LIST_NEWONLY), HasNew, &HasCleared);
	    }
	    free(whattofree);
	    fclose(mfp);
	    unlink(MapFile);
	    mdebug(4, ("Done with map file %s\n", MapFile));
	    folders_WantUpdate(ci, ci);
	}
    } while (!spcode && pathindex != AMS_MAILPATH && code != LIST_SUBSCRIBED && code != LIST_NEWONLY && code != LIST_AS_REQUESTED);
    ci->HasSetUp = 1;
    ConsiderResettingDescription(ci, code, TRUE);
    return(0);
}

AddSetupItem(ci, longname, shortname, substatus, showingnewstuff, HasNew, HasCleared)
struct folders *ci;
char *longname, *shortname;
int substatus, showingnewstuff, HasNew;
boolean *HasCleared;
{
    struct BEDirCache *bdcent;
    char *comm;
    int whichcache;

    if (!*HasCleared) {
	ClearFolders(ci);
	*HasCleared = TRUE;
    }
    whichcache = AddToBEDirCache(ci, longname, shortname, substatus);
    bdcent = &ci->MainDirCache[whichcache];
    if (substatus == AMS_ASKSUBSCRIBED || substatus == AMS_PRINTSUBSCRIBED || !HasNew) {
	bdcent->SkipMe = TRUE;
    } else {
	bdcent->SkipMe = FALSE;
    }
    if (substatus == AMS_PRINTSUBSCRIBED && showingnewstuff) {
	ams_CUI_PrintUpdates(ams_GetAMS(), longname, shortname);
    }
    if (!ci->VeryNarrow && showingnewstuff) {
	switch(substatus) {
	    case AMS_SHOWALLSUBSCRIBED:
		comm = HasNew ? " (Show-All Subscribed) " : " (Show-All Subscribed; Empty) ";
		break;
	    case AMS_ASKSUBSCRIBED:
		comm = HasNew ? " (Ask-Subscribed; Has New) " : " (Ask-Subscribed; No New) ";
		break;
	    default:
		comm = HasNew ? " (Has New Messages) " : " (Nothing New) ";
		break;
	}
    } else {
	comm = NULL;
    }
    InsertFolderNameInText(ci, bdcent, comm);
}

ClearFolders(ci)
struct folders *ci;
{
    int i;
    struct text *d;

    d = (struct text *) folders_GetDataObject(ci);
    text_ClearCompletely(d);
    text_SetGlobalStyle(d, ci->GlobalCapStyle);
    if (ci->MainDirCacheCount <= 0) {
	return;
    }
    for (i=0; i<ci->MainDirCacheCount; ++i) {
	free(ci->MainDirCache[i].FullName);
	free(ci->MainDirCache[i].ShortName);
    }
    ci->MainDirCacheCount = 0;
    ci->HighlightEnv = NULL;
    folders_SetDotPosition(ci, 0);
    folders_SetDotLength(ci, 0);
    folders_PostMenus(ci, NULL);
    folders_WantUpdate(ci, ci);
}

void folders__ActionHit(ci, substatus, FullName, nickname)
struct folders *ci;
int substatus;
char *FullName, *nickname;
{
    char    ErrorText[256], *ActionVector[10], Question[100+MAXPATHLEN];
    int result, quicknewstatus, vecsize;
    Boolean HasCurrent = FALSE, NowPlaying;
    struct captions *mainci;
    struct sendmessage *sm;
    long errcode;

    sprintf(Question, "What do you want to do with '%s'?", nickname);
    ActionVector[0] = Question;
    ActionVector[1] = ForgetItString;
    ActionVector[2] = "Explain what it is";
    if (substatus != AMS_UNSUBSCRIBED) {
	ActionVector[3] = "Unsubscribe";
	quicknewstatus = AMS_UNSUBSCRIBED;
    } else {
	ActionVector[3] = "Subscribe";
	quicknewstatus = AMS_ALWAYSSUBSCRIBED;
    }
    ActionVector[4] = "Alter subscription status";
    ActionVector[5] = "Post a message on it";
    mainci = folders_GetCaptions(ci);
    if (mainci->FullName && !strcmp(FullName, mainci->FullName)) {
	NowPlaying = TRUE;
	ActionVector[6] = "Update it (Find New Messages; Purge)";
    } else {
	NowPlaying = FALSE;
	ActionVector[6] = "See the messages";
    }
    vecsize = 7;
    if (!NowPlaying && amsutil_GetOptBit(EXP_FILEINTO)) {
	if (mainci->VisibleCUID > 0) {
	    ActionVector[vecsize++] = "File current message into it";
	    HasCurrent = TRUE;
	}
	if (mainci->MarkCount > 0) {
	    ActionVector[vecsize++] = "File marked messages into it";
	}
    }
    ActionVector[vecsize] = NULL;
    result = ams_ChooseFromList(ams_GetAMS(), ActionVector, 1);
    switch(result) {
	case 1:
	    break;
	case 2:
	    folders_ExplainDir(ci, FullName, nickname);
	    break;
	case 3:
	    substatus = quicknewstatus;
	    ams_WaitCursor(TRUE);
	    if ((errcode = ams_MS_SetSubscriptionEntry(ams_GetAMS(), FullName, nickname, substatus)) != 0) {
		ams_WaitCursor(FALSE);
		if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
		    sprintf(ErrorText, "'%s' is private; you don't have read-access or are unauthenticated.", nickname);
		} else if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS()))) {
		    sprintf(ErrorText, "%s: temporarily unavailable (net/server problem)", nickname);
		} else if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
		    sprintf(ErrorText, "Sorry; %s no longer exists so you cannot subscribe to it.", nickname);
		} else {
		    sprintf(ErrorText, "Cannot set subscription entry to %s", nickname);
		    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, errcode);
		    return;
		}
		message_DisplayString(NULL, 75, ErrorText);
		return;
	    }
	    ams_WaitCursor(FALSE);
	    folders_AlterSubscriptionStatus(ci, FullName, quicknewstatus, nickname);
	    break;
	case 4:
	    substatus = amsutil_ChooseNewStatus(nickname, substatus, TRUE);
	    ams_WaitCursor(TRUE);
	    if ((errcode = ams_MS_SetSubscriptionEntry(ams_GetAMS(), FullName, nickname, substatus)) != 0) {
		ams_WaitCursor(FALSE);
		if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
		    sprintf(ErrorText, "'%s' is private; you don't have read-access or are unauthenticated.", nickname);
		} else if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS()))) {
		    sprintf(ErrorText, "%s: temporarily unavailable (net/server problem)", nickname);
		} else if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
		    sprintf(ErrorText, "Sorry; %s no longer exists so you cannot subscribe to it.", nickname);
		} else {
		    sprintf(ErrorText, "Cannot set subscription entry to %s", nickname);
		    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, errcode);
		    return;
		}
		message_DisplayString(NULL, 75, ErrorText);
		return;
	    }
	    ams_WaitCursor(FALSE);
	    folders_AlterSubscriptionStatus(ci, FullName, substatus, nickname);
	    break;
	case 5:
	    /* Post on it */
	    sm = folders_ExposeSend(ci);
	    if (!sm) return;
	    sendmessage_ResetFromParameters(sm, nickname, NULL, NULL, NULL, 0);
	    sendmessage_CheckRecipients(sm);
	    break;
	case 6:
	    /* see the messages */
	    ams_WaitCursor(TRUE);
	    if (NowPlaying) { /* Force to consider purge */
		char *s, *t;

		s = malloc(1+strlen(nickname));
		t = malloc(1+strlen(FullName));
		strcpy(s, nickname);
		strcpy(t, FullName);
		nickname = s;
		FullName = t;
		/* In this case, nickname and FullName will be
		   freed by the following call */
		captions_ClearAndUpdate(mainci, TRUE, TRUE);
		ams_CUI_CheckMailboxes(ams_GetAMS(), FullName);
	    }
	    captions_InsertUpdatesInDocument(mainci, nickname, FullName, FALSE);
	    ams_WaitCursor(FALSE);
	    break;
	case 7:
	    if (HasCurrent) {
		/* file current */
		captions_FileCurrent(mainci, FullName, nickname);
		break;
	    }
	    /* drop through */
	case 8:
	    /* file marked */
	    captions_FileMarked(mainci, FullName, nickname);
	    break;
	default:
	    ams_ReportError(ams_GetAMS(), "Unexpected return value from message handler.", ERR_WARNING, FALSE, 0);
	    break;
    }
    return;
}


folders__AlterSubscriptionStatus(self, dir, status, shortname)
struct folders *self;
char *dir, *shortname;
int status;
{
    char *StatusString, ErrorText[256];

    AlterSubStatus(self, dir, status, shortname);
    switch (status) {
	case AMS_ALWAYSSUBSCRIBED:
	    StatusString = "You are now subscribed to %s.";
	    break;
	case AMS_ASKSUBSCRIBED: 
	    StatusString = "You are now 'Ask' subscribed to %s.";
	    break;
	case AMS_SHOWALLSUBSCRIBED: 
	    StatusString = "You are now 'ShowAll' subscribed to %s.";
	    break;
	case AMS_PRINTSUBSCRIBED: 
	    StatusString = "You are now 'Print' subscribed to %s.";
	    break;
	case AMS_UNSUBSCRIBED:
	default:
	    StatusString = "You are now not subscribed to %s.";
	    break;
    }
    sprintf(ErrorText, StatusString, shortname);
    message_DisplayString(NULL, 10, ErrorText);
    return(0);
}

AlterSubStatus(ci, dir, status, shortname)
struct folders *ci;
char *dir, *shortname;
int status;
{
    int i;
    struct BEDirCache *bdcent = NULL;

    if (!ci->HasSetUp) return(0);
    for (i=0; i<ci->MainDirCacheCount; ++i) {
	if (!strcmp(dir, ci->MainDirCache[i].FullName)
	    || ((ci->MainDirCache[i].FullName[0] == '\0')
		&& !strcmp(shortname, ci->MainDirCache[i].ShortName))) {
	    bdcent = &ci->MainDirCache[i];
	    break;
	}
    }
    if (!bdcent) return(0); /* Not in here */
    text_AlwaysInsertCharacters((struct text *) folders_GetDataObject(ci), bdcent->pos+1, WhichIcon(status), 1);
    text_AlwaysDeleteCharacters((struct text *) folders_GetDataObject(ci), bdcent->pos, 1);
    bdcent->substatus = status;
    folders_WantUpdate(ci, ci);
    return(0);
}

void folders__AlterFolderNames(ci, name, Nickname, DoInsert)
struct folders *ci;
char *name, *Nickname;
Boolean DoInsert;
{
    char DumBuf[1+MAXPATHLEN];
    int substatus, whichcache;
    struct BEDirCache *bdcent;
    long errcode;

    if (!ci->HasSetUp) return;
    if (DoInsert) {
	if (errcode = ams_MS_GetSubscriptionEntry(ams_GetAMS(), name, DumBuf, &substatus)) {
	    ams_ReportError(ams_GetAMS(), "Cannot get subscription entry", ERR_WARNING, TRUE, errcode);
	    substatus = AMS_UNSUBSCRIBED;
	}
	whichcache = AddToBEDirCache(ci, name, Nickname, substatus);
	bdcent = &ci->MainDirCache[whichcache];
	InsertFolderNameInText(ci, bdcent, NULL);
    } else {
	RemoveFromBEDirCache(ci, name, Nickname);
    }
    folders_WantUpdate(ci, ci);
}

void folders__HighlightFolder(self, name, CommText)
struct folders *self;
char *name, *CommText;
{
    if (self->CurrentConfiguration != LIST_ALL_FOLDERS) {	
	ams_PlanFolderPrefetch(self);
    }
    UnhighlightFolderName(self);
    HighlightFolderName(self, name, CommText);
}

UnhighlightFolderName(ci)
struct folders *ci;
{
    if (ci->HasSetUp && ci->HighlightEnv) {
	text_SetEnvironmentStyle((struct text *) folders_GetDataObject(ci), ci->HighlightEnv, ci->Normalfolderstyle);
    }
}

HighlightFolderName(ci, name, CommText)
struct folders *ci;
char *name, *CommText;
{
    int i;
    char NickName[1+MAXPATHLEN];

    if (!ci->HasSetUp || !name) return(0);
    ams_CUI_BuildNickName(ams_GetAMS(), name, NickName);
    for (i=0; i<ci->MainDirCacheCount; ++i) {
	if (ci->MainDirCache[i].FullName[0] == NULL) {
	    if (!strcmp(NickName, ci->MainDirCache[i].ShortName)) {
		long errcode;
		char DumBuf[100+MAXPATHLEN];

		errcode = ams_CUI_DisambiguateDir(ams_GetAMS(), NickName, &name);
		if (errcode) {
		    if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS()))) {
			sprintf(DumBuf, "%s: temporarily unavailable (net/server problem)", NickName);
		    } else if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
			sprintf(DumBuf, "'%s' is private; you don't have read-access or are unauthenticated.", NickName);
		    } else {
			sprintf(DumBuf, "The folder %s is not readable.", NickName);
		    }
		    message_DisplayString(NULL, 75, DumBuf);
		    return;
		}
		ci->MainDirCache[i].FullName = malloc(1+strlen(name));
		if (!ci->MainDirCache[i].FullName) return;
		strcpy(ci->MainDirCache[i].FullName, name);
	    }
	}
	if (!strcmp(name, ci->MainDirCache[i].FullName)) {
	    if (ci->MainDirCache[i].len == 0) {
		if (ci->MainDirCacheCount == 0) {
		    ClearFolders(ci);
		}
		InsertFolderNameInText(ci, &ci->MainDirCache[i], NULL);
		UpdateBEDirCachePositions(ci, &ci->MainDirCache[i], ci->MainDirCache[i].len);
	    }
	    HighlightSpecificFolderName(ci, name, CommText, i);
	    return;
	}
    }
    /* Did not find it, have to ADD it */
    folders_AlterFolderNames(ci, name, NickName, TRUE);
    /* I cannot believe my good fortune in having i already set right...  :-) */
    HighlightSpecificFolderName(ci, name, CommText, i);
}

HighlightSpecificFolderName(ci, name, CommText, i)
struct folders *ci;
char *name, *CommText;
int i;
{
    char Label[500];
    int insertct = 0, fpos;

    text_SetEnvironmentStyle((struct text *) folders_GetDataObject(ci), ci->MainDirCache[i].env, ci->Activefolderstyle);
    ci->HighlightEnv = ci->MainDirCache[i].env;
    BEDC_AddComment(ci, &ci->MainDirCache[i], CommText, &insertct);
    sprintf(Label, "%s %s", ci->MainDirCache[i].ShortName, CommText);
    UpdateBEDirCachePositions(ci, &ci->MainDirCache[i], insertct);
    folders_SetDotPosition(ci, ci->MainDirCache[i].pos);
    folders_WantUpdate(ci, ci);
    if (i+1 < ci->MainDirCacheCount) {
	fpos = ci->MainDirCache[i+1].pos + 1;
    } else {
	fpos = ci->MainDirCache[i].pos + 1;
    }
    if (!folders_Visible(ci, fpos)) {
	folders_SetTopPosition(ci, ci->MainDirCache[i].pos);
    }
    folders_WantUpdate(ci, ci);
}

void folders__FullUpdate(self, type, left, top, width, height)
struct folders *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    static int narrowlim = -1;

    folders_GetLogicalBounds(self, &Rect);
    if (narrowlim < 0) {
	narrowlim = environ_GetProfileInt("VeryNarrowFolders", 300);
    }
    super_FullUpdate(self, type, left, top, width, height);
    if (!self->mycursor) CreateFoldersCursor(self);
    folders_PostCursor(self, &Rect, self->mycursor);
    lpair_GetLogicalBounds(self->puntlp, &Rect);
    if (Rect.width < narrowlim) {
	if (!self->VeryNarrow) folders_SetVeryNarrow(self, TRUE);
    } else {
	if (self->VeryNarrow) folders_SetVeryNarrow(self, FALSE);
    }
}

InsertFolderNameInText(f, bdcent, comm)
struct folders *f;
struct BEDirCache *bdcent;
char *comm;
{
    struct environment *et;
    char IconBuf[10];
    char FoldChar = (amsutil_GetOptBit(EXP_FILEICONCAPTIONS)) ? ICON_FOLDER : ' ';
    int addlen, inspos = bdcent->pos;
    struct text *d = (struct text *) folders_GetDataObject(f);

    sprintf(IconBuf, "%s %c ", WhichIcon(bdcent->substatus), FoldChar);
    text_AlwaysInsertCharacters(d, inspos, IconBuf, 4);
    et = environment_InsertStyle(d->rootEnvironment, inspos, f->IconicStyle, 1);
    environment_SetLength(et, 3);
    inspos += 4;
    addlen = strlen(bdcent->ShortName);
    text_AlwaysInsertCharacters(d, inspos, bdcent->ShortName, addlen);
    et = environment_InsertStyle(d->rootEnvironment, inspos, f->Normalfolderstyle, 1);
    bdcent->env = et;
    bdcent->commentstart = inspos+addlen;
    bdcent->commentlen = 0;
    BEDC_AddComment(f, bdcent, comm, &addlen);
    text_AlwaysInsertCharacters(d, bdcent->commentstart+bdcent->commentlen, " \n", 2);
    environment_SetLength(et, addlen);
    bdcent->len = addlen + 6;
}

BEDC_AddComment(ci, bdc, comm, addedlen)
struct folders *ci;
struct BEDirCache *bdc;
char *comm;
int *addedlen;
{
    int newlen;

    if (ci->VeryNarrow) return(bdc->pos);
    if (!comm) comm = "";
    if (!bdc->commentstart) bdc->commentstart = text_GetLength((struct text *) folders_GetDataObject(ci));
    if (bdc->commentlen) text_AlwaysDeleteCharacters((struct text *) folders_GetDataObject(ci), bdc->commentstart, bdc->commentlen);
    newlen = strlen(comm);
    *addedlen += newlen - bdc->commentlen;
    bdc->commentlen = newlen;
    text_AlwaysInsertCharacters((struct text *) folders_GetDataObject(ci), bdc->commentstart, comm, newlen);
    return(bdc->commentstart + bdc->commentlen);
}

void folders__FindNextFolder(f, which, skipped)
struct folders *f;
int *which, *skipped;
{
    int pos, curr;

    *skipped = 0;
    if (f->HighlightEnv) {
	pos = environment_Eval(f->HighlightEnv);
    } else {
	pos = folders_GetDotPosition(f);
    }
    for (*which=0; *which<f->MainDirCacheCount && f->MainDirCache[*which].pos <= pos; ++*which) {
	;
    }
    curr = *which - 1;
    while (*which<f->MainDirCacheCount && f->MainDirCache[*which].SkipMe) {
	++*skipped;
	++*which;
    }
    if (*which >= f->MainDirCacheCount) {
	/* Wrap around to beginning and try once more before declaring "nothing new" */
	*which = 0;
	while (*which<curr && f->MainDirCache[*which].SkipMe) {
	    ++*skipped;
	    ++*which;
	}
	if (*which >= curr) *which = f->MainDirCacheCount;
    }
}

void folders__ReadMail(self, CheckMailbox)
struct folders *self;
Boolean CheckMailbox;
{
    char ShortName[1+MAXPATHLEN], *FullName, *fm;

    if (ams_CUI_DisambiguateDir(ams_GetAMS(), "mail", &FullName)
	 && ams_CUI_DisambiguateDir(ams_GetAMS(), "misc", &FullName)) {
	fm = ams_GetFirstMailClass();
	if (!fm) {
	    long mserrcode = ams_CUI_DisambiguateDir(ams_GetAMS(), fm, &FullName);
	    if (mserrcode) {
		ams_ReportError(ams_GetAMS(), "I don't know where your mail goes; sorry.", ERR_WARNING, TRUE, mserrcode);
		return;
	    }
	}
    }
    ams_CUI_BuildNickName(ams_GetAMS(), FullName, ShortName);
    if (CheckMailbox) {
	ams_WaitCursor(TRUE);
	ams_CUI_CheckMailboxes(ams_GetAMS(), FullName);
	ams_WaitCursor(FALSE);
    }
    captions_InsertUpdatesInDocument(folders_GetCaptions(self), ShortName, FullName, FALSE);
    return;
}

void folders__NextFolder(self, ShowFirst)
struct folders *self;
Boolean ShowFirst;
{
    int which, skipped;

    folders_FindNextFolder(self, &which, &skipped);
    if (which < self->MainDirCacheCount) {
	/* display this one */
	captions_InsertUpdatesInDocument(folders_GetCaptions(self), self->MainDirCache[which].ShortName, self->MainDirCache[which].FullName, ShowFirst);
    } else {
	if (skipped) {
	    message_DisplayString(NULL, 10, "This is the last changed folder on the list.");
	} else {
	    message_DisplayString(NULL, 10, "This is the last folder on the list.");
	}
    }
}

void folders__SetCaptions(self, c)
struct folders *self;
struct captions *c;
{
    self->mycaps = c;
}

void folders__SetSkip(self, fname, doskip)
struct folders *self;
char *fname;
boolean doskip;
{
    int i;

    for (i=0; i<self->MainDirCacheCount; ++i) {
	if (!strcmp(fname, self->MainDirCache[i].FullName)) {
	    self->MainDirCache[i].SkipMe = doskip;
	    break;
	}
    }
}

struct sendmessage *folders__ExposeSend(self)
struct folders *self;
{

    if (self->sm == NULL) {
	self->sm = sendmessage_New();
	sendmessage_SetFoldersView(self->sm, self);
	
	self->sm->myframe = ams_InstallInNewWindow(self->sm, "messages-send", "Message Composition", environ_GetProfileInt("sendmessage.width", -1), environ_GetProfileInt("sendmessage.height", -1), self->sm);
	if (self->sm->myframe == NULL) {
	    sendmessage_Destroy(self->sm);
	    self->sm = NULL;
	    return(NULL);
	}
	sendmessage_Reset(self->sm);
    }
    folders_Expose(sendmessage_GetIM(self->sm));
    if (amsutil_GetOptBit(EXP_WARPWINDOW)) {
	folders_Warp(sendmessage_GetIM(self->sm));
    }
    folders_ForceUpdate();
    sendmessage_WantInputFocus(self->sm, self->sm);
    return(self->sm);
}

void folders__GrowWindow(self) 
struct folders *self;
{
    struct rectangle Rect;
    struct im *im;

    folders_GetLogicalBounds(self, &Rect);
    im = folders_GetIM(self);
    if (im && amsutil_GetOptBit(EXP_GROWFOLDS) && (Rect.height > 0 && Rect.width > 0)) {
	if (amsutil_GetOptBit(EXP_VANISH)) {
	    folders_Vanish(im);
	} else {
	    folders_Hide(im);
	}
	folders_ForceUpdate();
	folders_Expose(im);
    }
}

void folders__FolderOnDisplay(self, nick, full)
struct folders *self;
char *nick, *full;
{
    if (nick) {
	if (folders_GetCaptions(self)->ShortName) {
	    strcpy(nick, folders_GetCaptions(self)->ShortName);
	} else {
	    *nick = '\0';
	}
    }
    if (full) {
	if (folders_GetCaptions(self)->FullName) {
	    strcpy(full, folders_GetCaptions(self)->FullName);
	} else {
	    *full = '\0';
	}
    }
}

void folders__SetVeryNarrow(self, vn)
struct folders *self;
boolean vn;
{
    self->VeryNarrow = vn;
    if (self->buttons) {
	boolean WantsPunt = amsutil_GetOptBit(EXP_PUNTBUTT);
	boolean WantsCommit = environ_GetProfileSwitch("messages.TurnOffCheckpointingAndIUnderstandTheDangersForMessages", 0) || environ_GetProfileSwitch("messages.CommitButton", 0);
	int buttpct;
	int ind=0;
	struct sbutton *bs=sbuttonv_ButtonData(self->buttons);

	if(WantsCommit || WantsPunt) {
	    buttpct = vn ? 30 : 100;
	} else buttpct=0;

	
    /* sorry about the hack (see DoButton), don't change the names of the buttons! */
	if(WantsPunt) {
	    sbutton_SetLabel(bs, 0, "Punt!");
	    sbutton_SetRock(bs, 0, (long)self);
	}
	if(WantsCommit) {
	    sbutton_SetLabel(bs, WantsPunt?1:0, "Commit");
	    sbutton_SetRock(bs, WantsPunt?1:0, (long)self);
	}

	if (vn) {
	    lpair_VTFixed(self->puntlp, self->buttons, self->myscroll, buttpct, 0);
	} else {
	    lpair_HFixed(self->puntlp, self->myscroll, self->buttons, buttpct, 0);
	}
    }
}

static void DoButton(b, action, ind, self)
struct sbutton *b;
enum view_MouseAction action;
int ind;
struct folders *self;
{
    /* sorry about the hack, don't change the names of the buttons! */

    switch(*sbutton_GetLabel(b, ind)) {
	case 'P':
	    if (action == view_RightUp) {
		captions_PuntCurrent(folders_GetCaptions(self), FALSE);
	    } else if (action == view_LeftUp) {
		captions_PuntCurrent(folders_GetCaptions(self), TRUE);
            }
            break;
	case 'C':
	    if (action == view_LeftUp || action == view_RightUp) {
		ams_CommitState(FALSE, FALSE, TRUE, FALSE);
            }
            break;
	default: ;
    }
}

static struct sbutton_list blist[]={
    {"Punt!", 0, NULL, FALSE},
    {NULL, 0, NULL, FALSE}
};

struct view *folders__GetApplicationLayer(self)
struct folders *self;
{
    struct sbutton *bs;
    self->myscroll = (struct scroll *) super_GetApplicationLayer(self);

    if(!self->myscroll) return NULL;

    self->puntlp=lpair_New();
    if(!self->puntlp) return NULL;
    
    self->prefs = sbutton_GetNewPrefs("folderoptions");
    if(self->prefs) {
	if(sbutton_GetFont(self->prefs)==NULL) sbutton_GetFont(self->prefs)=fontdesc_Create("andy", fontdesc_Bold, 12);
	sbutton_InitPrefs(self->prefs, "folderoptions");
	self->buttons = sbuttonv_CreateFilledSButtonv("nbutterv", self->prefs, blist);
	if(!self->buttons) return FALSE;
	bs=sbuttonv_ButtonData(self->buttons);
	sbutton_GetHitFunc(bs)=(procedure)DoButton;
	sbuttonv_GetVBorder(self->buttons)=environ_GetProfileInt("folderoptionsborder", 0);
	sbuttonv_GetHBorder(self->buttons)=sbuttonv_GetVBorder(self->buttons);
	sbuttonv_GetVSpacing(self->buttons)=environ_GetProfileInt("folderoptionspadding", 0);
	sbuttonv_GetHSpacing(self->buttons)=sbuttonv_GetVSpacing(self->buttons);
	
	sbutton_GetMattePrefs(bs)=sbutton_DuplicatePrefs(self->prefs, "folderoptionsmatte");
	if(sbutton_GetMattePrefs(bs)) {
	    sbutton_GetStyle(sbutton_GetMattePrefs(bs))=0;
	    sbutton_InitPrefs(sbutton_GetMattePrefs(bs), "folderoptionsmatte");
	}
	
    } else return NULL;
    folders_SetVeryNarrow(self, self->VeryNarrow); /* Forces lpair to be set right */
    return (struct view	*) self->puntlp;
}

void folders__DeleteApplicationLayer(self, ignored)
struct folders *self;
long ignored;
{
    
    if (self->puntlp) lpair_Destroy(self->puntlp);
    if(self->prefs) sbutton_FreePrefs(self->prefs);
    if(self->buttons) {
	if(sbuttonv_ButtonData(self->buttons)) sbutton_Destroy(sbuttonv_ButtonData(self->buttons));
	sbuttonv_Destroy(self->buttons);
    }
    super_DeleteApplicationLayer(self, self->myscroll);
    self->prefs=NULL;
    self->buttons=NULL;
    self->myscroll = NULL;
    self->puntlp = NULL;
}

struct captions *
folders__NewCaptionsInNewWindow(self)
struct folders *self;
{
    struct captions *cap = captions_New();

    folders_SetCaptions(self, cap);
    captions_SetFolders(cap, self);
    cap->myframe = ams_InstallInNewWindow(captions_GetApplicationLayer(cap), "messages-captions", "Message Captions", environ_GetProfileInt("captions.width", 600), environ_GetProfileInt("captions.height", 250), cap);
    return(cap);
}

void folders__SetSendmessage(self, sm)
struct folders *self;
struct sendmessage *sm;
{
    self->sm = sm;
}

struct captions *folders__GetCaptions(self)
struct folders *self;
{
    if (!self->mycaps) {
	folders_NewCaptionsInNewWindow(self);
    }
    return(self->mycaps);
}

ExposeCap(self)
struct folders *self;
{
#ifdef NOWAYJOSE
    struct im *myim;

    myim = captions_GetIM(folders_GetCaptions(self));
    if (myim) {
	folders_ExposeWindow(myim);
    }
#endif /* NOWAYJOSE */
}

static struct bind_Description folders_standardbindings [] = {
    /* procname, keysequence, key rock, menu string, menu rock, proc, docstring, dynamic autoload */
    {"folders-left-click-here", "!", TRUE, NULL, NULL, 0, folders_SimulateLeftClick, "Display what I am pointing at"},
    {"folders-right-click-here", "@", FALSE, NULL, NULL, 0, folders__SimulateClick, "Simulate right click on what I am pointing at"},
    {"folder-down-focus", "\030n", NULL, NULL, NULL, 0, folders_DownFocus, "Move input focus to captions"},
    {"folders-up-focus", "\030p", NULL, NULL, NULL, 0, folders_UpFocus, "Move input focus to bodies or sendmessage"},
    {"folder-down-focus", "\030\016", NULL, NULL, NULL, 0, folders_DownFocus, "Move input focus to captions"},
    {"folders-up-focus", "\030\020", NULL, NULL, NULL, 0, folders_UpFocus, "Move input focus to bodies or sendmessage"},
    {"textview-compound", NULL, NULL, NULL, NULL, NULL, folders_TextviewCompound, "Execute a compound textview operation"},
    {"folders-compound-operation", NULL, NULL, NULL, NULL, 0, folders_FoldersCompound, "Execute a compound folders operation"},
    {"folders-textview-compound", NULL, NULL, NULL, NULL, 0, FoldersTextviewCommand, "Execute a compound 'textview' operation on the folders"},
    {"folders-sendmessage-compound", NULL, NULL, NULL, NULL, 0, FoldersSendmessageCommand, "Execute a compound 'sendmessage' operation."},
    {"folders-captions-compound", NULL, NULL, NULL, NULL, 0, FoldersCaptionsCommand, "Execute a compound 'captions' operation."},
    {NULL, "\033~", NULL, NULL, NULL, 0, NULL, NULL, NULL}, /* Preserve read onliness */
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}
};

static void InitKeyMenusStyles(folders)
struct folders *folders;
{
    char *fontname;
    int fontsize;

    folders->mykeys = keystate_Create(folders, folders_standardkeymap);
    folders->mymenulist = menulist_DuplicateML(folders_standardmenulist, folders);

    folders->BigCenterStyle = style_New();
    style_SetFontSize(folders->BigCenterStyle, style_ConstantFontSize, 22);
    style_SetJustification(folders->BigCenterStyle, style_Centered);

    folders->CenterStyle = style_New();
    style_SetJustification(folders->CenterStyle, style_Centered);
    style_AddNewFontFace(folders->CenterStyle, (long) fontdesc_Italic);

    folders->SmallCenterStyle = style_New();
    style_SetJustification(folders->SmallCenterStyle, style_Centered);
    style_SetFontSize(folders->SmallCenterStyle, style_ConstantFontSize, 8);

    folders->BoldStyle = style_New();
    style_SetName(folders->BoldStyle, "Bold");
    style_AddNewFontFace(folders->BoldStyle, (long) fontdesc_Bold);

    folders->ItalicStyle = style_New();
    style_SetFontSize(folders->ItalicStyle, style_PreviousFontSize, 2);
    style_AddNewFontFace(folders->ItalicStyle, (long) fontdesc_Italic);

    folders->IconicStyle = style_New();
    style_SetFontFamily(folders->IconicStyle, "msgs");
    style_SetFontSize(folders->IconicStyle, style_ConstantFontSize, 10);

    folders->Activefolderstyle = style_New();
    if (amsutil_GetOptBit(EXP_FIXCAPTIONS)) {
	style_AddNewFontFace(folders->Activefolderstyle, (long) fontdesc_Bold | fontdesc_Fixed);
    } else {
	style_AddNewFontFace(folders->Activefolderstyle, (long) fontdesc_Bold);
    }
    style_SetNewLeftMargin(folders->Activefolderstyle, style_ConstantMargin, 20, style_RawDots);
    style_SetNewIndentation(folders->Activefolderstyle, style_ConstantMargin, -10, style_RawDots);

    folders->Normalfolderstyle = style_New();

    folders->GlobalCapStyle = style_New();
    style_SetJustification(folders->GlobalCapStyle, style_LeftJustified);
    style_SetNewLeftMargin(folders->GlobalCapStyle, style_ConstantMargin, 20, style_RawDots);
    style_SetNewIndentation(folders->GlobalCapStyle, style_ConstantMargin, -10, style_RawDots);

    fontsize = environ_GetProfileInt("messages.fontsize", 12);
    fontname = amsutil_GetDefaultFontName();
    style_SetFontSize(folders->GlobalCapStyle, style_ConstantFontSize, fontsize);
    style_SetFontFamily(folders->Activefolderstyle, fontname);
    style_SetFontFamily(folders->Normalfolderstyle, fontname);
    style_SetFontFamily(folders->GlobalCapStyle, fontname);
    style_SetFontFamily(folders->BoldStyle, fontname);
    style_SetFontFamily(folders->ItalicStyle, fontname);
}

static void OneTimeInitKeyMenus()
{
    folders_standardkeymap = keymap_New();
    folders_standardmenulist = menulist_New();
    bind_BindList(folders_standardbindings, folders_standardkeymap, folders_standardmenulist, &folders_classinfo);
}

boolean folders__InitializeClass(classID) 
struct classheader *classID;
{
    OneTimeInitKeyMenus();
    return(TRUE);
}

boolean folders__InitializeObject(c, folders)
struct classheader *c;
struct folders *folders;  
{
    struct text *mytext;

    folders_SetWhatIAm(folders, WHATIAM_FOLDERS);
    InitKeyMenusStyles(folders);
    folders->prefs=NULL;
    folders->buttons=NULL;
    folders->NeedsToPrefetch = FALSE;
    folders->myframe = NULL;
    folders->puntlp=NULL;
    folders->buttons = NULL;
    folders->myscroll = NULL;
    folders->sm = NULL;
    folders->mycursor = NULL;
    folders->HasSetUp = 0;
    folders->CurrentConfiguration = LIST_NEWONLY;
    folders->MailOnlyMode = 0;
    folders->VeryNarrow = 0;
    /* Initialize the BEDirCache */
    folders->MainDirCache = (struct BEDirCache *) malloc(25*sizeof(struct BEDirCache));
    bzero(folders->MainDirCache, 25 * sizeof(struct BEDirCache));
    folders->MainDirCacheSize = 25;
    folders->MainDirCacheCount = 0;

    mytext = text_New();
    text_SetGlobalStyle(mytext, folders->GlobalCapStyle);
    text_SetReadOnly(mytext, TRUE);
    folders_SetDataObject(folders, mytext);

    folders->HighlightEnv = NULL;
    ams_AddCheckpointFolder(folders);
    folders->mycaps = NULL;

    folders_ShowHelp(folders);
    return(TRUE);
}

void folders__FinalizeObject(c, self)
struct classheader *c;
struct folders *self;
{
    ams_RemoveCheckpointFolder(self);
    if (self->MainDirCache) {
	free(self->MainDirCache);
	self->MainDirCache = NULL;
    }
    if (self->sm) {
	sendmessage_SetFoldersView(self->sm, NULL);
    }
    if (self->mycaps) {
	captions_SetFolders(self->mycaps, NULL);
    }
    FinalizeProcStyleStuff(self);
    if (self->puntlp) lpair_Destroy(self->puntlp);
    if(self->prefs) sbutton_FreePrefs(self->prefs);
    if(self->buttons) {
	if(sbuttonv_ButtonData(self->buttons)) sbutton_Destroy(sbuttonv_ButtonData(self->buttons));
	sbuttonv_Destroy(self->buttons);
    }
    super_DeleteApplicationLayer(self, self->myscroll);
}
