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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/foldaux.c,v 1.22 1993/05/04 01:38:37 susan Exp $";
#endif


 

#include <andrewos.h>
#include <sys/param.h>
#include <errprntf.h>
#include <cui.h>
#include <fdphack.h>
#include <keystate.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <im.ih>
#include <frame.ih>
#include <message.ih>
#include <environ.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <cursor.ih>
#include <text.ih>
#include <filetype.ih>
#include <envrment.ih>

#include <ams.ih>
#include <amsutil.ih>
#include <captions.ih>

#define dontDefineRoutinesFor_sendmessage
#include <sendmsg.ih>
#undef dontDefineRoutinesFor_sendmessage
#include <text822v.ih>

#define AUXMODULE 1
#include <folders.eh>
#undef AUXMODULE

void folders_Warp(im)
struct im *im;
{

    if(im) {
	im_SetWMFocus(im);
	im_SetLastUsed(im);
    }

}

void folders_Expose(im)
struct im *im;
{
    if(im) im_ExposeWindow(im);
}

void folders_Hide(im)
struct im *im;
{
    if(im) im_HideWindow(im);
}

void folders_Vanish(im)
struct im *im;
{
    if(im) im_VanishWindow(im);
}

void folders_ForceUpdate()
{
    im_ForceUpdate();
}

void folders_TextviewCompound(tv, cmds)
struct textview *tv;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), tv, "textview", cmds);
}

void folders_FoldersCompound(self, cmds)
struct folders *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "folders", cmds);
}

void FoldersTextviewCommand(self, cmds)
struct folders *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "textview", cmds);
}

void FoldersMessagesCommand(self, cmds)
struct folders *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "messages", cmds);
}

void FoldersSendmessageCommand(self, cmds)
struct folders *self;
char *cmds;
{
    if (self->sm) {
	ams_GenericCompoundAction(ams_GetAMS(), self->sm, "sendmessage", cmds);
    }
}

void FoldersCaptionsCommand(self, cmds)
struct folders *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), folders_GetCaptions(self), "captions", cmds);
}

void folders_DownFocus(self)
struct folders *self;
{
    ams_Focus(folders_GetCaptions(self));
}

void folders_UpFocus(self)
struct folders *self;
{
    if (self->sm) {
	ams_Focus(self->sm->BodyTextview);
    } else {
	ams_Focus(captions_GetBodView(folders_GetCaptions(self)));
    }
}

void folders_SimulateLeftClick(self)
struct folders *self;
{
    DoClick(self, TRUE, TRUE);
}

void folders__PostKeyState(self, ks)
struct folders *self;
struct keystate *ks;
{
    self->mykeys->next = NULL;
    if (amsutil_GetOptBit(EXP_KEYSTROKES)) {
	if (ks) keystate_AddAfter(ks, self->mykeys);
	super_PostKeyState(self, self->mykeys);
    } else {
	super_PostKeyState(self, ks);
    }
}

void folders__PostMenus(self, ml)
struct folders *self;
struct menulist *ml;
{
    menulist_ClearChain(self->mymenulist);
    if (ml) menulist_ChainAfterML(self->mymenulist, ml, ml);
    super_PostMenus(self, self->mymenulist);
}

FinalizeProcStyleStuff(self)
struct folders *self;
{
    keystate_Destroy(self->mykeys);
    menulist_Destroy(self->mymenulist);
    style_Destroy(self->Activefolderstyle);
    style_Destroy(self->Normalfolderstyle);
    style_Destroy(self->GlobalCapStyle);
    style_Destroy(self->IconicStyle);
    style_Destroy(self->BoldStyle);
    style_Destroy(self->ItalicStyle);
    style_Destroy(self->CenterStyle);
    style_Destroy(self->BigCenterStyle);
    cursor_Destroy(self->mycursor);
}

CreateFoldersCursor(self)
struct folders *self;
{
    struct fontdesc *fd;

    fd = fontdesc_Create("icon", 0, 12);
    self->mycursor = cursor_Create(self);
    cursor_SetGlyph(self->mycursor, fd, 'R');
}

static int lastconfiguration = -999;

void folders__Reconfigure(self, listcode)
struct folders *self;
int listcode;
{
    if (lastconfiguration == listcode) return;
    ams_WaitCursor(TRUE);
    self->HasSetUp = 0;
    SetupList(self, listcode, NULL);
    lastconfiguration = self->CurrentConfiguration = listcode;
    folders_PostMenus(self, NULL);
    ams_WaitCursor(FALSE);
}

void folders__UpdateMsgs(self, mailonly, thingstoread, ShowHelp) 
struct folders *self;
int mailonly;
char *thingstoread[];
boolean ShowHelp;
{
    if (ShowHelp) {
	if (self->mycaps) {
	    if (self->mycaps->BodView) {
		t822view_ShowHelp(self->mycaps->BodView, NULL);
	    }
	    captions_ClearAndUpdate(self->mycaps, FALSE, TRUE);
	    captions_ShowHelp(self->mycaps);
	}
	folders_ShowHelp(self);
	im_ForceUpdate();
    }
    self->HasSetUp = 0;
    if (ams_OnlyMail(ams_GetAMS())) mailonly = 1;
    self->MailOnlyMode = mailonly;
    ams_WaitCursor(TRUE);
    if (thingstoread) {
	self->ShowingAsRequested = 1;
	SetupList(self, LIST_AS_REQUESTED, thingstoread);
	lastconfiguration = LIST_AS_REQUESTED;
    } else {
	ams_CUI_CheckMailboxes(ams_GetAMS(), mailonly ? ams_GetMailPath() : NULL);
	self->ShowingAsRequested = 0;
	SetupList(self, LIST_NEWONLY, NULL);
	lastconfiguration = LIST_NEWONLY;
    }
    if (!amsutil_GetOptBit(EXP_NOFIRSTFOLDER)) {
	if (self->MainDirCacheCount > 0) {
	    int which, lim;

	    lim = self->MainDirCacheCount;
	    for(which=0; which<lim && self->MainDirCache[which].SkipMe; ++which) {
		;
	    }
	    if (which < lim) {
		captions_InsertUpdatesInDocument(folders_GetCaptions(self), self->MainDirCache[which].ShortName, self->MainDirCache[which].FullName, FALSE);
	    }
	} else {
	    ClearFolders(self);
	    folders_ReadMail(self, FALSE);
	}
    }
    folders_WantInputFocus(self, self);
    ams_WaitCursor(FALSE);
}

static char *E1 = "   (NOT the currently-displayed folder)";
static char *E2 = "Folder name: ";
static char *E3 = "\nFolder type: ";
static char *E4 = "\nNumber of messages: ";
static char *E5 = "\nYour subscription status: ";
static char *E6 = "\n\nExplanation of this message folder:\n\n";
static char *E7 = "\n\nNo explanation of this folder is available, but here is the first message:\n\n";
static char *E8 = "\n\nNo explanation of this folder is available.";

void folders__ExplainDir(self, FullName, nickname)
struct folders *self;
char *FullName, *nickname;
{
    int ProtCode, MsgCount;
    char ErrorText[100+MAXPATHLEN], *TypeStr, *SubsStr, ExpFileName[1+MAXPATHLEN], LocalFileName[1+MAXPATHLEN];
    struct text *d;
    int pos = 0, substatus, ShouldDelete, fpos;
    long mcode;

    captions_ResetVisibleCaption(folders_GetCaptions(self));
    t822view_SetDotPosition(captions_GetBodView(folders_GetCaptions(self)), 0);
    t822view_SetDotLength(captions_GetBodView(folders_GetCaptions(self)), 0);
    t822view_FrameDot(captions_GetBodView(folders_GetCaptions(self)), 0);

    d = captions_GetBodDoc(folders_GetCaptions(self));

    mcode = ams_MS_GetDirInfo(ams_GetAMS(), FullName, &ProtCode, &MsgCount);
    if (mcode) {
	if (ams_AMS_ERRNO(ams_GetAMS()) == EACCES) {
	    sprintf(ErrorText, "'%s' is private; you don't have read-access or are unauthenticated.", nickname);
	} else if (ams_vdown(ams_GetAMS(), ams_AMS_ERRNO(ams_GetAMS()))) {
	    sprintf(ErrorText, "%s: temporarily unavailable (net/server problem)", nickname);
	} else if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
	    ams_CUI_HandleMissingFolder(ams_GetAMS(), FullName);
	    return;
	} else {
	    sprintf(ErrorText, "Cannot look up information about %s", FullName);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, TRUE, mcode);
	}
	message_DisplayString(NULL, 75, ErrorText);
	return;
    } else {
	TypeStr = ams_DescribeProt(ams_GetAMS(), ProtCode);
    }
    if (mcode = ams_MS_GetSubscriptionEntry(ams_GetAMS(), FullName, ErrorText, &substatus)) {
	ams_ReportError(ams_GetAMS(), "Cannot get subscription entry", ERR_WARNING, TRUE, mcode);
	SubsStr = "Lookup error";
    } else {
	switch(substatus) {
		case AMS_ALWAYSSUBSCRIBED:
		    SubsStr = "Subscribed";
		    break;
		case AMS_UNSUBSCRIBED:
		    SubsStr = "Not subscribed";
		    break;
		case AMS_ASKSUBSCRIBED:
		    SubsStr = "Ask-subscribed";
		    break;
		case AMS_SHOWALLSUBSCRIBED:
		    SubsStr = "Show-all subscribed";
		    break;
		case AMS_PRINTSUBSCRIBED:
		    SubsStr = "Print-subscribed";
		    break;
		default:
		    SubsStr = "unknown";
		    break;
	}
    }
    QAddToDoc(d, &pos, E2, strlen(E2), self->BoldStyle, strlen(E2) - 1);
    QAddToDoc(d, &pos, nickname, strlen(nickname), NULL, 0);
    if (folders_GetCaptions(self)->FullName && strcmp(FullName, folders_GetCaptions(self)->FullName)) {
	QAddToDoc(d, &pos, E1, strlen(E1), self->ItalicStyle, strlen(E1));
    }
    QAddToDoc(d, &pos, E3, strlen(E3), self->BoldStyle, strlen(E3) - 1);
    QAddToDoc(d, &pos, TypeStr, strlen(TypeStr), NULL, 0);
    QAddToDoc(d, &pos, E4, strlen(E4), self->BoldStyle, strlen(E4)-1);
    sprintf(ErrorText, "%d", MsgCount);
    QAddToDoc(d, &pos, ErrorText, strlen(ErrorText), NULL, 0);
    QAddToDoc(d, &pos, E5, strlen(E5), self->BoldStyle, strlen(E5)-1);
    QAddToDoc(d, &pos, SubsStr, strlen(SubsStr), NULL, 0);

    sprintf(ErrorText, "%s/%s", FullName, AMS_EXPLANATIONFILE);
    mcode = ams_MS_DisambiguateFile(ams_GetAMS(), ErrorText, ExpFileName, AMS_DISAMB_EXISTS);
    if (mcode) {
	if (ams_AMS_ERRNO(ams_GetAMS()) == ENOENT) {
	    if (MsgCount > 0) {
		FILE *fp;
		char SnapshotBuf[AMS_SNAPSHOTSIZE], LineBuf[2000], *objtype;
		int numbytes, bytesleft, cuid, IsDup, myid = 0;

		QAddToDoc(d, &pos, E7, strlen(E7), self->ItalicStyle, strlen(E7)-1);
		if (ams_CUI_GetHeaders(ams_GetAMS(), FullName, "000000", SnapshotBuf, AMS_SNAPSHOTSIZE, 0, &numbytes, &bytesleft, TRUE)) {
		    ams_ReportError(ams_GetAMS(), "Could not get first notice text", ERR_WARNING, TRUE, mcode);
		    return;
		}
		cuid = ams_CUI_GetCuid(ams_GetAMS(), AMS_ID(SnapshotBuf), FullName, &IsDup);
		if (ams_CUI_ReallyGetBodyToLocalFile(ams_GetAMS(), cuid, LocalFileName, &ShouldDelete, !ams_CUI_SnapIsRunning(ams_GetAMS()))) {
		    return; /* error already reported */
		}
		fp = fopen(LocalFileName, "r");
		if (!fp) {
		    ams_ReportError(ams_GetAMS(), "Could not open initial message to display it", ERR_WARNING, FALSE, 0);
		    return;
		}
		while (fgets(LineBuf, sizeof(LineBuf), fp)) {
		    if (LineBuf[0] == '\n') break;
		}
		fpos = ftell(fp);
		objtype = filetype_Lookup(fp, NULL, &myid, NULL);
		if (ftell(fp) == 0) {
		    fseek(fp, fpos, 0);
		}
		if (objtype && strcmp(objtype, "text")) {
		    myid = 0;
		    message_DisplayString(NULL, 80, "ATK message does not contain a top-level text object!");
		}
		text_SetReadOnly(d, FALSE);
		text_ReadSubString(d, pos, fp, 1);
		text_SetReadOnly(d, TRUE);
		fclose(fp);
		if (ShouldDelete) unlink(LocalFileName);
	    } else {
		QAddToDoc(d, &pos, E8, strlen(E8), self->ItalicStyle, strlen(E8)-1);
	    }
	} else {
	    ams_ReportError(ams_GetAMS(), "Cannot get explanation of messages folder", ERR_WARNING, TRUE, mcode);
	    QAddToDoc(d, &pos, E8, strlen(E8), self->ItalicStyle, strlen(E8)-1);
	}
    } else {
	int fd, bytes;
	char Splat[5000];

	QAddToDoc(d, &pos, E6, strlen(E6), self->ItalicStyle, strlen(E6)-1);
	ams_CUI_GenTmpFileName(ams_GetAMS(), LocalFileName);
	if (ams_CUI_GetFileFromVice(ams_GetAMS(), LocalFileName, ExpFileName)) {
	    ams_ReportError(ams_GetAMS(), "Cannot get explanation file from AFS", ERR_WARNING, TRUE, ams_mserrcode(ams_GetAMS()));
	    return;
	}

	fd = open(LocalFileName, O_RDONLY, 0644);
	if (fd<0) {
	    sprintf(ErrorText, "Cannot open local help file %s (%d)", LocalFileName, errno);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	    unlink(LocalFileName);
	    return;
	}
	while((bytes = read(fd, Splat, sizeof(Splat))) > 0) {
	    text_AlwaysInsertCharacters(d, pos, Splat, bytes);
	    pos += bytes;
	}
	close(fd);
	if (bytes<0) {
	    sprintf(ErrorText, "Cannot read from local help file %s (%d)", LocalFileName, errno);
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	}
        unlink(LocalFileName);
    }
    ExposeCap(self);
}

QAddToDoc(d, pos, text, tlen, ss, stylelen)
struct text *d;
char *text;
int tlen, stylelen, *pos;
struct style *ss;
{
    struct environment *et;

    text_AlwaysInsertCharacters(d, *pos, text, tlen);
    if (ss) {
	et = environment_InsertStyle(((struct text *)d)->rootEnvironment, *pos, ss, 1);
	environment_SetLength(et, stylelen);
    }
    *pos += tlen;
}

folders__WriteFormattedBodyFile(self, fname, captbuf)
struct folders *self;
char *fname, *captbuf;
{
    FILE *fp;
    struct text *t, *new = text_New();
    struct t822view *bv;
    struct captions *mycap;

    mycap = folders_GetCaptions(self);
    if (mycap->VisibleCUID >= 1) {
	strcpy(captbuf, AMS_CAPTION(mycap->VisibleSnapshot));
    } else {
	*captbuf = '\0';
    }
    ams_CUI_GenTmpFileName(ams_GetAMS(), fname);
    fp = fopen(fname, "w");
    if (!fp) return(-1);
    t = captions_GetBodDoc(mycap);
    bv = captions_GetBodView(mycap);
    text_SetWriteID(t, im_GetWriteID());
    if (t822view_GetDotLength(bv) > 0) {
	text_AlwaysCopyText(new, 0, t, t822view_GetDotPosition(bv), t822view_GetDotLength(bv));
    } else {
	text_AlwaysCopyText(new, 0, t, mycap->StartOfRealBody, text_GetLength(t) - mycap->StartOfRealBody);
    }
    text_Write(new, fp, text_UniqueID(new), 0);
    text_Destroy(new);
    return(fclose(fp));
}

ConsiderResettingDescription(ci, code, FirstTime)
struct folders *ci;
int code;
Boolean FirstTime;
{
    char Label[256], MessageText[256];
    char *PluralString;

    PluralString = (ci->MainDirCacheCount == 1) ? "" : "s";
    switch(code) {
	case LIST_ALL_FOLDERS:
	    sprintf(MessageText, "Exposed a list of all %s folders on your search path.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000));
	    sprintf(Label, "All %d Folders", ci->MainDirCacheCount);
	    break;
	case LIST_SUBSCRIBED:
	    sprintf(MessageText, "Exposed a list of the %s folder%s you subscribe to.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000), PluralString);
	    sprintf(Label, "%d Subscribed Folder%s", ci->MainDirCacheCount, PluralString);
	    break;
	case LIST_MAIL_FOLDERS:
	    sprintf(MessageText, "Exposed a list of %s personal mail folder%s.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000), PluralString);
	    sprintf(Label, "%d Mail Folder%s", ci->MainDirCacheCount, PluralString);
	    break;
	case LIST_AS_REQUESTED:
	    sprintf(MessageText, "Exposed a list of the %s folder%s you requested.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000), PluralString);
	    sprintf(Label, "%d Requested Folder%s", ci->MainDirCacheCount, PluralString);
	    break;
	case LIST_NEWONLY:
	    if (FirstTime) {
		/* printed a more detailed message elsewhere */
		MessageText[0] = '\0'; 
	    } else {
		if (ci->ShowingAsRequested) {
		    sprintf(MessageText, "Exposed a list of %s folder%s.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000), PluralString);
		} else {
		    sprintf(MessageText, "Exposed a list of your %s subscription%s with new messages.", amsutil_cvEng(ci->MainDirCacheCount, 0, 1000), PluralString);
		}
	    }
	    if (ci->ShowingAsRequested) {
		sprintf(Label, "%d Requested Folder%s", ci->MainDirCacheCount, PluralString);
	    } else {
		sprintf(Label, "%d Changed Folder%s", ci->MainDirCacheCount, PluralString);
	    }
	    break;
	default:
	    MessageText[0] = '\0';
	    Label[0] = '\0';
    }
    if (ci->myframe) frame_SetTitle(ci->myframe, Label);
    if (MessageText[0]) {
	message_DisplayString(NULL, 10, MessageText);
    }
}

