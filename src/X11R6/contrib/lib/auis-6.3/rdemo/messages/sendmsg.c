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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/rdemo/messages/RCS/sendmsg.c,v 1.3 1992/12/15 22:00:08 rr2b R6tape $";
#endif


 

#include <andrewos.h> /* sys/file.h */
#include <stdio.h>
#include <sys/param.h>
#include <util.h>
#include <pwd.h>
#include <ctype.h>

#include <fontdesc.ih>
#include <sbutton.ih>
#include <sbuttonv.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <search.ih>
#include <im.ih>
#include <envrment.ih>
#include <errprntf.h>
#include <rect.h>

#include <cui.h>
#include <fdphack.h>
#include <mailconf.h>
#include <mail.h>
#include <dropoff.h>
#include <environ.ih>
#include <complete.ih>

#include <ams.ih>
#include <amsutil.ih>
#include <text822.ih>
#include <folders.ih>
#define dontDefineRoutinesFor_captions
#include <captions.ih>
#undef dontDefineRoutinesFor_captions
#include <sendmsg.eh>

#include <unscribe.h>

/* constants for the Deliver() subroutine */
#define FORCE_ASK_ABOUT_FORMATTING 0
#define FORCE_SEND_FORMATTED 1
#define FORCE_SEND_UNFORMATTED 2

extern char *index();

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

void sendmessage_SetButtonFont(self, font)
struct sendmessage *self;
struct fontdesc *font;
{
    sbutton_GetFont(sbuttonv_ButtonData(self->buttons) ->prefs)=font;
}

void sendmessage__LinkTree(self, parent)
struct sendmessage *self;
struct view *parent;
{
    super_LinkTree(self,parent);
    if (self->SendLpair != NULL)  {
	lpair_LinkTree(self->SendLpair, self);
    }
}

void sendmessage__UnlinkTree(self)
struct sendmessage *self;
{
    super_UnlinkTree(self);
    if (self->SendLpair) {
	lpair_UnlinkTree(self->SendLpair);
    }
}

sendmessage__AddHeaderLine(self, headerline)
struct sendmessage *self;
char *headerline;
{
    struct textview *v = self->HeadTextview;
    int len;

    len = strlen(headerline);
    textview_SetDotPosition(v,0);
    text_InsertCharacters(self->HeadText, 0, headerline, len);
    text_InsertCharacters(self->HeadText, len, "\n", 1);
    textview_SetDotPosition(v, len);
    textview_FrameDot(v, len);
    textview_WantUpdate(v, v);
    MakeOneHeaderFieldBold(self, 0);
}

void BSSM_HeadersFocus(sm)
struct sendmessage *sm;
{
    textview_WantInputFocus(sm->HeadTextview, sm->HeadTextview);
}

void BSSM_BodyFocus(sm)
struct sendmessage *sm;
{
    textview_WantInputFocus(sm->BodyTextview, sm->BodyTextview);
}

void BSSM_DownFocus(sm)
struct sendmessage *sm;
{
    struct im *myim = sendmessage_GetIM(sm);

    if (myim && (im_GetInputFocus(myim) == (struct view *) sm->HeadTextview)) {
	textview_WantInputFocus(sm->BodyTextview, sm->BodyTextview);
    } else if (sm->folders) {
	ams_Focus(sm->folders);
    } else {
	ams_Focus(sendmessage_NewFoldersInNewWindow(sm));
    }
}

void BSSM_UpFocus(sm)
struct sendmessage *sm;
{
    struct im *myim = sendmessage_GetIM(sm);

    if (myim && (im_GetInputFocus(myim) == (struct view *) sm->BodyTextview)) {
	textview_WantInputFocus(sm->HeadTextview, sm->HeadTextview);
    } else if (sm->folders) {
	struct view *v;

	if (sm->folders->mycaps) {
	    if (sm->folders->mycaps->BodView) {
		v = (struct view *) sm->folders->mycaps->BodView;
	    } else {
		v = (struct view *) sm->folders->mycaps;
	    }
	} else {
	    v = (struct view *) sm->folders;
	}
	ams_Focus(v);
    } else {
	ams_Focus(sendmessage_NewFoldersInNewWindow(sm));
    }
}

static char dummyname[]="";

struct sbutton_list blist[]={
    {dummyname, (long)0, NULL, FALSE}, /* Reset */
    
    {dummyname, (long)EXP_SIGNMAIL, NULL, FALSE}, /* Will/Won't sign */

    {dummyname, (long)EXP_HIDEAFTER, NULL, FALSE}, /* Will/Won't hide */

    {dummyname, (long)EXP_CLEARAFTER, NULL, FALSE}, /* Will/Won't clear */

    {dummyname, (long)EXP_KEEPBLIND, NULL, FALSE}, /* Will/Won't keep copy */
    
    {NULL, (long)0, NULL, FALSE}
};

HandleButton(self, sendmessage, in, whichbut)
struct sbutton *self;
struct sendmessage *sendmessage;
long in;
long whichbut;
{
    char *Yes, *No;
    
    switch (whichbut) {
	case 0:
	    amsutil_SetOptBit(EXP_CLEARAFTER, amsutil_GetPermOptBit(EXP_CLEARAFTER));
	    amsutil_SetOptBit(EXP_SIGNMAIL, amsutil_GetPermOptBit(EXP_SIGNMAIL));
	    amsutil_SetOptBit(EXP_HIDEAFTER, amsutil_GetPermOptBit(EXP_HIDEAFTER));
	    amsutil_SetOptBit(EXP_KEEPBLIND, amsutil_GetPermOptBit(EXP_KEEPBLIND));
	    sendmessage_CheckButtons(sendmessage);
	    sendmessage_Reset(sendmessage);
	    return(0);
	case EXP_CLEARAFTER:
	    Yes = "Will Clear";
	    No = "Won't Clear";
	    break;
	case EXP_SIGNMAIL:
	    Yes = "Will Sign";
	    No = "Won't Sign";
	    break;
	case EXP_HIDEAFTER:
	    Yes = "Will Hide";
	    No = "Won't Hide";
	    break;
	case EXP_KEEPBLIND: 
	    Yes = "Will Keep Copy";
	    No = "Won't Keep Copy";
	    break;
	default: 
	    return(0);
    }
    if (amsutil_GetOptBit(whichbut)) {
	amsutil_SetOptBit(whichbut, 0);
	sbutton_SetLabel(self, in, No);
    } else {
	amsutil_SetOptBit(whichbut, 1);
	sbutton_SetLabel(self, in, Yes);
    }
    return(0);
}


void ReadTemplate(sm)
struct sendmessage *sm;
{
    if (sm->NeedsTemplate) {
	if (text_ReadTemplate(sm->BodyText, "messages", FALSE)) {
	    if (errno == ENOENT) {
		message_DisplayString(sm, 99, "Warning: No messages template on your template path; no styles available!");
	    } else {
		ams_ReportError(ams_GetAMS(), "Cannot open messages template.", ERR_WARNING, FALSE, 0);
	    }
	} else {
	    sm->NeedsTemplate = 0;
	}
    }
}

sendmessage__SetFoldersView(self, fold)
struct sendmessage *self;
struct folders *fold;
{
    self->folders = fold;
}

boolean sendmessage__InitializeObject(c, sendmessage)
struct classheader *c;
struct sendmessage *sendmessage;
{
    int lpaircount=0;
    struct sbutton *bs;
    struct lpair *headeroptlp;

    InitProcStuff(sendmessage);
    sendmessage->HasSigned = 0;
    sendmessage->myframe = NULL;
    sendmessage->CKPFileName = NULL;
    sendmessage->PSMsg = NULL;
    sendmessage->CurrentState = SM_STATE_NOSTATE;
    sendmessage->HeadModified = sendmessage->BodyModified = 0;
    sendmessage->folders = NULL;
    sendmessage->HeadText = text_New();
    sendmessage->BodyText = text_New();
    sendmessage->NeedsTemplate = 1;
    sendmessage->HeadTextview = textview_New();
    textview_SetDataObject(sendmessage->HeadTextview, sendmessage->HeadText);
    sendmessage->HeadScroll = (struct scroll *) textview_GetApplicationLayer(sendmessage->HeadTextview);
    sendmessage->BodyTextview = textview_New();
    textview_SetDataObject(sendmessage->BodyTextview, sendmessage->BodyText);
    sendmessage->BodyScroll = (struct scroll *) textview_GetApplicationLayer(sendmessage->BodyTextview);

    sendmessage->prefs = sbutton_GetNewPrefs("sendoptions");
    if(sendmessage->prefs) {
	if(sbutton_GetFont(sendmessage->prefs)==NULL) sbutton_GetFont(sendmessage->prefs)=fontdesc_Create("andy", fontdesc_Bold, 10);
	sbutton_InitPrefs(sendmessage->prefs, "sendoptions");
	sendmessage->buttons = sbuttonv_CreateFilledSButtonv("sbuttonv", sendmessage->prefs, blist);
	if(!sendmessage->buttons) return FALSE;
	bs=sbuttonv_ButtonData(sendmessage->buttons);
	
	sbuttonv_GetVBorder(sendmessage->buttons)=environ_GetProfileInt("sendoptionsborder", 0);
	sbuttonv_GetHBorder(sendmessage->buttons)=sbuttonv_GetVBorder(sendmessage->buttons);
	sbuttonv_GetVSpacing(sendmessage->buttons)=environ_GetProfileInt("sendoptionspadding", 0);
	sbuttonv_GetHSpacing(sendmessage->buttons)=sbuttonv_GetVSpacing(sendmessage->buttons);
	
	sbutton_GetMattePrefs(bs)=sbutton_DuplicatePrefs(sendmessage->prefs, "sendoptionsmatte");
	if(sbutton_GetMattePrefs(bs)) {
	    sbutton_GetStyle(sbutton_GetMattePrefs(bs))=0;
	    sbutton_InitPrefs(sbutton_GetMattePrefs(bs), "sendoptionsmatte");
	}
	
    } else return FALSE;

    
    sbutton_SetLabel(bs, SM_BLIND, amsutil_GetOptBit(EXP_KEEPBLIND) ? "Will Keep Copy" : "Won't Keep Copy");
    sbutton_SetLabel(bs, SM_CLEAR,  amsutil_GetOptBit(EXP_CLEARAFTER) ?  "Will Clear" : "Won't Clear");
    sbutton_SetLabel(bs, SM_SIGN, amsutil_GetOptBit(EXP_SIGNMAIL) ?  "Will Sign" : "Won't Sign");
    sbutton_SetLabel(bs, SM_HIDE, amsutil_GetOptBit(EXP_HIDEAFTER) ? "Will Hide" : "Won't Hide");
    sbutton_SetLabel(bs, SM_RESET, "Reset");
    sbutton_GetHitFunc(bs)=HandleButton;
    sbutton_GetHitFuncRock(bs)=(long)sendmessage;
    
#define HACKMAXLPAIRS 5
    sendmessage->randomlpairs=(struct lpair **)malloc(sizeof(struct lpair *)*(HACKMAXLPAIRS+1));
    
#define ADDLPAIR(l) do {\
    if(sendmessage->randomlpairs && lpaircount<HACKMAXLPAIRS) {\
    sendmessage->randomlpairs[lpaircount++]=(l);\
    sendmessage->randomlpairs[lpaircount]=NULL;\
    }\
} while(FALSE)

    ADDLPAIR(headeroptlp = lpair_New());
    
#undef ADDLPAIR
#undef HACKMAXLPAIRS
    
    lpair_HFixed(headeroptlp, sendmessage->HeadScroll, sendmessage->buttons, 115, 1);

    sendmessage->SendLpair = lpair_New();
    lpair_VSplit(sendmessage->SendLpair, headeroptlp, sendmessage->BodyScroll, 75, 1);

    InitStylesAndFonts(sendmessage);
    SetNotModified(sendmessage);

    textview_WantInputFocus(sendmessage->HeadTextview, sendmessage->HeadTextview);
    ams_AddCheckpointSendmessage(sendmessage);

    return(TRUE);
}

void sendmessage__FinalizeObject(c, self)
struct classheader *c;
struct sendmessage *self;
{   /* This is a bit bogus, because there isn't really a sendmessage dataobject/view split */
    struct lpair **lps=self->randomlpairs;

    
    /* OK, OK, I give up... hack around lpair's desire to unlinktree it's children when it is finalized.... rr2b */
    if(lps) while(*lps) {
	lpair_SetNth(*lps, 0, NULL);
	lpair_SetNth(*lps, 1, NULL);
	lps++;
    }
    

    ams_RemoveCheckpointSendmessage(self);
    if (self->PSMsg) {
	free(self->PSMsg);
	self->PSMsg = NULL;
    }
    if (self->CKPFileName) {
	free(self->CKPFileName);
	self->CKPFileName = NULL;
    }
    if (self->folders) {
	folders_SetSendmessage(self->folders, NULL);
    }
    DestroyProcStuff(self);
    DestroyStyles(self);
    lpair_Destroy(self->SendLpair);
    
    lps=self->randomlpairs;
    if(lps) while(*lps) {
	lpair_Destroy(*lps);
	lps++;
    }
    
    if(self->randomlpairs) {
	free(self->randomlpairs);
	self->randomlpairs=NULL;
    }
    textview_DeleteApplicationLayer(self->HeadTextview, self->HeadScroll);
    textview_DeleteApplicationLayer(self->BodyTextview, self->BodyScroll);
    if(self->buttons) {
	struct sbutton *bs=sbuttonv_ButtonData(self->buttons);
	if(bs) sbutton_Destroy(bs);
	sbuttonv_Destroy(self->buttons);
    }
    if(self->prefs) sbutton_FreePrefs(self->prefs);
/*   this is NOT currently needed:
  if(self->matteprefs) sbutton_FreePrefs(self->matteprefs); */
    
    text_Destroy(self->HeadText);
    text_Destroy(self->BodyText);
    textview_Destroy(self->HeadTextview);
    textview_Destroy(self->BodyTextview);
}

void sendmessage__SetCurrentState(sm, state)
struct sendmessage *sm;
{
    char *tit;

    if (sm->CurrentState == state) return;
    sm->CurrentState = state;
    switch(state) {
	case SM_STATE_NOSTATE:
	    tit = "Uninitialized";
	    break;
	case SM_STATE_READY:
	    tit = "Starting Fresh";
	    break;
	case SM_STATE_INPROGRESS:
	    tit = "Composing";
	    break;
	case SM_STATE_SENDING:
	    tit = "Sending/Posting";
	    break;
	case SM_STATE_SENT:
	    tit = "Sent/Posted";
	    break;
	case SM_STATE_VALIDATING:
	    tit = "Validating";
	    break;
	case SM_STATE_VALIDATED:
	    tit = "Validated";
	    break;
	case SM_STATE_VALIDATEFAILED:
	    tit = "Validation Failed";
	    break;
	default:
	    tit = "Unknown state";
	    break;
    }
    if (sm->myframe) SetMyFrameTitle(sm, tit);
}

void sendmessage__FullUpdate(sm, type, left, top, width, height)
    struct sendmessage *sm;
    enum view_UpdateType type;
    long left;
    long top;
    long width;
    long height;
{
    struct rectangle myrect;

    rectangle_SetRectSize(&myrect, 0, 0, sendmessage_GetLogicalWidth(sm), sendmessage_GetLogicalHeight(sm));
    lpair_InsertView(sm->SendLpair, sm, &myrect);
    lpair_FullUpdate(sm->SendLpair, type, left, top, width, height);
}

void sendmessage__Update(sendmessage)
struct sendmessage *sendmessage;  
{
    lpair_Update(sendmessage->SendLpair);
    if ((sendmessage->CurrentState != SM_STATE_INPROGRESS && sendmessage->CurrentState != SM_STATE_SENDING && sendmessage->CurrentState != SM_STATE_VALIDATING) && sendmessage_HasChanged(sendmessage)) {
	sendmessage_SetCurrentState(sendmessage, SM_STATE_INPROGRESS);
    }
}

struct view *
sendmessage__Hit(sendmessage, action, x, y, NumberOfClicks)
struct sendmessage *sendmessage;
enum view_MouseAction action;
long x, y, NumberOfClicks;
{
    return(lpair_Hit(sendmessage->SendLpair, action, x, y, NumberOfClicks));
}

ReadDraft(File, sendmessage)
char *File;
struct sendmessage *sendmessage;
{
    char FileBuf[1+MAXPATHLEN];

    ams_TildeResolve(ams_GetAMS(), File, FileBuf);
    sendmessage_ReadFromFile(sendmessage, FileBuf, FALSE);
}

void BSSM_FakeBug(sm, txt)
struct sendmessage *sm;
char *txt;
{
    ams_ReportError(ams_GetAMS(), txt ? txt : "One of my bits is missing!  Call an ambulance!", ERR_CRITICAL, FALSE, 0);
}

ComposeBugReport(sm)
struct sendmessage *sm;
{
    char FileName[1+MAXPATHLEN];
    FILE *fp;

    if (sendmessage_HasChanged(sm)) {
	if (!sendmessage_AskEraseUnsentMail(sm)) {
	    return(0);
	}
	sendmessage_Clear(sm);
    }
    ams_CUI_GenLocalTmpFileName(ams_GetAMS(), FileName);
    fp = fopen(FileName, "w");
    if (!fp) {
	return -1;
    }
    fputs("To: ", fp);
    fputs(ams_MessagesAutoBugAddress(ams_GetAMS()), fp);
    fputs("\nContent-Type: X-BE2; 12\nSubject: \nCC:\n\n\\begindata{text, 42}\n\\textdsversion{12}\n\\template{messages}\n\\majorheading{Messages/Sendmessage Bug Report Form}\n\n\nThe maintainers of this program are sincerely sorry if you have encountered a bug.  By filling out this report carefully and completely, you may be able to help us fix the bug relatively quickly.\n\n\n \\bold{Please enter a description of the bug}:\n\n\n\n \\bold{If the bug did not occur on this machine, please give the machine and CPU type, or enter 'uncertain'}:\n\n\n\n \\bold{If the bug did not occur using this version of the program, please state which version you were using below, or enter 'uncertain'}:\n\n\n\n \\bold{If you are running the Console program, please use the 'Write Log File' option and then insert the resulting file below}:\n\n\n\n \\italic{Thank you for your assistance and patience.  Below this point, you will see information that has been automatically added by the system in order to help understand the bug; please do not make any changes below this point.}.\n\n\\enddata{text, 42}", fp);
    fclose(fp);
    sendmessage_ReadFromFile(sm, FileName, TRUE);
    sendmessage_AppendBugInfoToBody(sm, TRUE);
}

sendmessage__AppendBugInfoToBody(sm, IsMessagesBug)
struct sendmessage *sm;
Boolean IsMessagesBug;
{
    char FileName[1+MAXPATHLEN];
    FILE *fp;

    ams_WaitCursor(TRUE);
    ams_CUI_GenLocalTmpFileName(ams_GetAMS(), FileName);
    fp = fopen(FileName, "w");
    if (!fp) {
	ams_WaitCursor(FALSE);
	message_DisplayString(sm, 10, "Could not open temporary file to write out bug report statistics--sorry.");
	return -1;
    }
    ams_WriteOutUserEnvironment(ams_GetAMS(), fp, IsMessagesBug);
    fclose(fp);
    DirectlyInsertFile(sm->BodyTextview, sm->BodyText, FileName, text_GetLength(sm->BodyText));
    unlink(FileName);
    ams_WaitCursor(FALSE);
    return(0);
}    

DirectlyInsertFile(tv, t, fname, pos)
struct textview *tv;
struct text *t;
char *fname;
int pos;
{
    FILE *fp;

    fp = fopen(fname, "r");
    if (!fp) {
	char ErrorText[100+MAXPATHLEN];
	sprintf(ErrorText, "The file %s could not be inserted.", ams_ap_Shorten(ams_GetAMS(), fname));
	message_DisplayString(tv, 50, ErrorText);
	im_ForceUpdate();
	return;
    }
    text_AlwaysInsertFile(t, fp, fname, pos);
    fclose(fp);
}

static char DraftFileNameBuf[MAXPATHLEN+1] = "~/Draft.mail";

sendmessage_SaveDraft(sendmessage)
struct sendmessage *sendmessage;
{
    if (completion_GetFilename(sendmessage, "Draft file to save: ", DraftFileNameBuf, DraftFileNameBuf, sizeof(DraftFileNameBuf), FALSE, FALSE) == -1 ) {
	return;
    }
    if (!sendmessage_WriteFile(sendmessage, DraftFileNameBuf)) {
	SetNotModified(sendmessage);
    }
}

sendmessage_RestoreDraft(sendmessage, dfile)
struct sendmessage *sendmessage;
char *dfile;
{
    int NeedReset =0;

    if (dfile && *dfile) {
	strcpy(DraftFileNameBuf, dfile);
    } else {
	if (sendmessage_HasChanged(sendmessage)) {
	    if (!sendmessage_AskEraseUnsentMail(sendmessage)) {
		return(0);
	    }
	    NeedReset = 1;
	    sendmessage_Clear(sendmessage);
	} 
	if (completion_GetFilename(sendmessage, "Draft file to restore: ", DraftFileNameBuf, DraftFileNameBuf, sizeof(DraftFileNameBuf), FALSE, TRUE) == -1 ) {
	    if (NeedReset) sendmessage_Reset(sendmessage);
	    return;
	}
    }
    ReadDraft(DraftFileNameBuf, sendmessage);
}

ForceSending(sendmessage)
struct sendmessage *sendmessage;
{
    return(Deliver(sendmessage, FORCE_SEND_FORMATTED));
}

ForceStripping(sendmessage)
struct sendmessage *sendmessage;
{
    return(Deliver(sendmessage, FORCE_SEND_UNFORMATTED));
}

sendmessage_DoDelivery(sendmessage)
struct sendmessage *sendmessage;
{
    return(Deliver(sendmessage, FORCE_ASK_ABOUT_FORMATTING));
}

Deliver(sendmessage, formathandlingcode)
struct sendmessage *sendmessage;
int formathandlingcode;
{
    message_DisplayString(sendmessage, 100,
			  "We are sorry, but sending messages from the demo account is disabled.");
    sendmessage_SetCurrentState(sendmessage, SM_STATE_SENT);
    return(0);
}

sendmessage__Clear(sendmessage)
struct sendmessage *sendmessage;
{
    text_ClearCompletely(sendmessage->BodyText);
    text_ClearCompletely(sendmessage->HeadText);
    text_SetGlobalStyle(sendmessage->BodyText, sendmessage->DefaultStyle);
    text_SetGlobalStyle(sendmessage->HeadText, sendmessage->DefaultHeadStyle);
    sendmessage->NeedsTemplate = TRUE;
    ReadTemplate(sendmessage);
    text822_ResetGlobalStyle(sendmessage->BodyText);

    textview_WantUpdate(sendmessage->BodyTextview, sendmessage->BodyTextview);
    textview_WantUpdate(sendmessage->HeadTextview, sendmessage->HeadTextview);

    SetNotModified(sendmessage);
    sendmessage_SetCurrentState(sendmessage, SM_STATE_READY);
    message_DisplayString(sendmessage, 10, "Ready to send a new message");
    textview_WantInputFocus(sendmessage->HeadTextview, sendmessage->HeadTextview);
    sendmessage->HasSigned = 0;
    return(0);
}

#define MAXHEADERLINE 1000

sendmessage__ReadFromFile(sendmessage, SourceFile, DeleteIt)
struct sendmessage *sendmessage;
char *SourceFile;
Boolean DeleteIt;
{
    FILE *fp;
    int len, start, ig;
    char ScrFormat[25], ContentType[25];
    char ErrorText[256], MyName[1+MAXPATHLEN];

    if (sendmessage_HasChanged(sendmessage) && !sendmessage_AskEraseUnsentMail(sendmessage)) return(0);
    ScrFormat[0] = '\0';
    ContentType[0] = '\0';
    if (!ams_CUI_OnSameHost(ams_GetAMS()) && access(SourceFile, R_OK)) {
	ams_CUI_GenTmpFileName(ams_GetAMS(), MyName);
	if (ams_CUI_GetFileFromVice(ams_GetAMS(), MyName, SourceFile)) {
	    return(-1);
	}
    } else {
	strcpy(MyName, SourceFile);
    }
    if ((fp = fopen(MyName, "r")) == NULL) {
	if (errno == ENOENT) {
	    sprintf(ErrorText, "There is no file named '%s'.", ams_ap_Shorten(ams_GetAMS(), MyName));
	    message_DisplayString(sendmessage, 10, ErrorText);
	} else {
	    sprintf(ErrorText, "Cannot open source file %s", ams_ap_Shorten(ams_GetAMS(), MyName));
	    ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	}
	return(-1);
    }
    if (sendmessage_Clear(sendmessage)) {
	fclose(fp);
	return(-1);
    }
    if (!text822_ReadIntoText(sendmessage->BodyText, fp, MODE822_NORMAL, NULL, &len, TRUE, &start, &ig, sendmessage->HeadText)) {
	ams_ReportError(ams_GetAMS(), "Could not read in text822 draft message", ERR_WARNING, FALSE, 0);
	fclose(fp);
	return(-1);
    }
    fclose(fp);
    if (!ams_CUI_OnSameHost(ams_GetAMS()) && access(SourceFile, R_OK)) {
	unlink(MyName);
    }
    if (DeleteIt) {
	/* Try local delete first */
	if (unlink(SourceFile)) {
	    long mserrcode; 
	    if (mserrcode = ams_MS_UnlinkFile(ams_GetAMS(), SourceFile)) {
		ams_ReportError(ams_GetAMS(), "Could not unlink a temporary file", ERR_WARNING, TRUE, mserrcode);
	    }
	}
    }
    sendmessage_ResetSendingDot(sendmessage);
    SetNotModified(sendmessage);
    MakeHeaderFieldsBold(sendmessage);
    textview_WantUpdate(sendmessage->HeadTextview, sendmessage->HeadTextview);
    textview_WantUpdate(sendmessage->BodyTextview, sendmessage->BodyTextview);
    return(0);
}

/* The following function returns 1 if all headers are filled in, zero
   otherwise.  It also sets the dot at the first empty header.  */

SetSendingDot(v, d)
struct textview *v;
struct text *d;
{
    register int pos, len, c, lastnewline = 0;
    Boolean FoundIt = FALSE, JustSawAColon = FALSE;
    int numcolons = 0;

    for (pos = 0, len = text_GetLength(d); !FoundIt && pos < len; ++pos) {
	c = text_GetChar(d, pos);
	switch(c) {
	    case ':':
		JustSawAColon = TRUE;
		++numcolons;
		break;
	    case '\012':
	    case '\015':
		if ((numcolons == 1) && JustSawAColon
		    /* Special hack to ignore empty CC header */
		&& ((pos - lastnewline) < 4 
		    || text_GetChar(d, ++lastnewline) != 'C'
		    || text_GetChar(d, ++lastnewline) != 'C'
		    || text_GetChar(d, ++lastnewline) != ':'))
		{
		    FoundIt = TRUE;
		}
		lastnewline = pos;
		numcolons = 0;
		JustSawAColon = FALSE;
		break;
	    case ' ':
	    case '\t':
		break;
	    default: 
		JustSawAColon = FALSE;
		break;
	}
    }
    if (pos-- < len) {
	textview_SetDotPosition(v, pos);
	textview_FrameDot(v, pos); 
	return(0);
    } else {
	textview_SetDotPosition(v, 0);
	textview_FrameDot(v, 0);
	return(1);
    }
}

static Submit(sendmessage, Unformat, Version, TrustDelivery)
struct sendmessage *sendmessage;
Boolean Unformat;
int Version, TrustDelivery;
{
    static int flags = 0;
    char FileName[MAXPATHLEN+1];

    if ((text_GetLength(sendmessage->BodyText) < 2) && !amsutil_GetOptBit(EXP_SENDEMPTY)) {
	int kids;

	kids = EnvViewCt(sendmessage->BodyText->rootEnvironment);
	if (kids <= 0) {
	message_DisplayString(sendmessage, 10, "You need at least 2 characters in the body of your message");
	return(-1);
	}
    }
    ams_CUI_GenTmpFileName(ams_GetAMS(), FileName);

    RemoveUselessHeaderLines(sendmessage);
    if (WriteOneFile(sendmessage, FileName, TRUE, FALSE, Version, TrustDelivery)) return(-1);
    UnlinkCKPFile(sendmessage);
    if (Unformat) {
	int code = ProduceUnscribedVersion(FileName);
	if (code) {
	    char ErrorText[256];

	    sprintf(ErrorText, "Unformatting error (%d, %d); cannot produce unformatted text", code, errno);
	    message_DisplayString(sendmessage, 10, ErrorText);
	    return(-1);
	}
    }
    sendmessage_SetCurrentState(sendmessage, SM_STATE_SENDING);
    flags = amsutil_GetOptBit(EXP_KEEPBLIND) ? AMS_SEND_BLINDYES : AMS_SEND_BLINDNO;
    im_ForceUpdate();
    if (ams_CUI_SubmitMessage(ams_GetAMS(), FileName, flags)) {
	sendmessage_SetCurrentState(sendmessage, SM_STATE_INPROGRESS);
	return(-1);
    }
    sendmessage_SetCurrentState(sendmessage, SM_STATE_SENT);
    SaveForPS(sendmessage);
    SetNotModified(sendmessage);
    im_ForceUpdate();
    return(0);
}

SetNotModified(sendmessage)
struct sendmessage *sendmessage;
{
    sendmessage->HeadModified = sendmessage->HeadCheckpoint = text_GetModified(sendmessage->HeadText);
    sendmessage->BodyModified = sendmessage->BodyCheckpoint = text_GetModified(sendmessage->BodyText);
    UnlinkCKPFile(sendmessage);
}

sendmessage__WriteFile(sendmessage, ViceFileName)
struct sendmessage *sendmessage;
char *ViceFileName;
{
    int code;

    code = WriteOneFile(sendmessage, ViceFileName, TRUE, FALSE, 12, 1);
    if (!code) SetNotModified(sendmessage);
    return(code);
}

static UnlinkCKPFile(sendmessage)
struct sendmessage *sendmessage;
{
    if (sendmessage->CKPFileName) {
	if (unlink(sendmessage->CKPFileName)) { /* Try local unlink first */
	    ams_MS_UnlinkFile(ams_GetAMS(), sendmessage->CKPFileName);
	}
    }
}

WriteOneFile(sendmessage, ViceFileName, OnVice, MayOverwrite, Version, TrustDelivery)
struct sendmessage *sendmessage;
char *ViceFileName;
Boolean OnVice, MayOverwrite, TrustDelivery;
int Version;
{
    FILE *fp;
    int i, lim, c = 0;
    struct text *d;
    Boolean SeeingAt;
    char ErrorText[MAXPATHLEN+100], LocalName[1+MAXPATHLEN], MyViceFileName[1+MAXPATHLEN];

    ams_TildeResolve(ams_GetAMS(), ViceFileName, MyViceFileName);
    if (OnVice && !ams_CUI_OnSameHost(ams_GetAMS())) {
	ams_CUI_GenLocalTmpFileName(ams_GetAMS(), LocalName);
    } else {
	strcpy(LocalName, MyViceFileName);
    }
    if (!MayOverwrite && strncmp(MyViceFileName, "/tmp/", 5) 
    && !access(LocalName, F_OK)
    && (ams_GetBooleanFromUser(ams_GetAMS(), "That file exists; do you want to overwrite it", 2) != 1)) {
	return(-1);
    }

    if ((fp = fopen(LocalName, "w")) == NULL) {
	sprintf(ErrorText, "Error -- cannot open local file %s", LocalName);
	message_DisplayString(sendmessage, 10, ErrorText);
	return(-1);
    }
#ifdef M_UNIX
    chmod(LocalName, 0600);
#else
    fchmod(fileno(fp), 0600);
#endif
    if (Version >= 10) {
	int kids;

	kids = EnvViewCt(sendmessage->BodyText->rootEnvironment);
	fprintf(fp, "X-Andrew-Message-Size: %d+%d\n", text_GetLength(sendmessage->BodyText) - kids, kids);
	fprintf(fp, "Content-Type: X-BE2; %d\n", Version);
	fprintf(fp, "If-Type-Unsupported: %s\n", TrustDelivery ? "alter" : "send");
    }
/*    environ_Put("TextWriteVersion12", "X"); /* phased out 12/27/88, now default */
    d = sendmessage->HeadText;
    SeeingAt = FALSE;
    for (i=0, lim = text_GetLength(d); i<lim; ++i) {
	c = text_GetChar(d, i);
	if (c == '@') {
	    if (SeeingAt) continue;
	    SeeingAt = TRUE;
	} else {
	    SeeingAt = FALSE;
	}
	putc(c, fp);
    }
    if (c != '\n') putc('\n', fp);
    putc('\n', fp);
    d = sendmessage->BodyText;

    text_Write(d, fp, im_GetWriteID(), 1);
    fputs("\n", fp); /* Mail should end with a newline, I think */
#ifdef M_UNIX
    chmod(LocalName,0600);
#else
    fchmod(fileno(fp), 0600);
#endif
    if (ferror(fp)) {
	sprintf(ErrorText, "Error in writing file %s", LocalName);
	message_DisplayString(sendmessage, 10, ErrorText);
	return(-1);
    }
    if(vfclose(fp)) {
	sprintf(ErrorText, "Error -- cannot close file %s", LocalName);
	message_DisplayString(sendmessage, 10, ErrorText);
	return(-1);
    }
    if (OnVice && !ams_CUI_OnSameHost(ams_GetAMS())) {
	if (ams_CUI_StoreFileToVice(ams_GetAMS(), LocalName, MyViceFileName)) {
	    sprintf(ErrorText, "Error -- cannot write file %s", MyViceFileName);
	    message_DisplayString(sendmessage, 10, ErrorText);
	    unlink(LocalName);
	    return(-1);
	}
	unlink(LocalName);
    }
    sprintf(ErrorText, "Wrote file %s", ams_ap_Shorten(ams_GetAMS(), MyViceFileName));
    message_DisplayString(sendmessage, 10, ErrorText);
    return(0);
}

void sendmessage__Reset(sendmessage)
struct sendmessage *sendmessage;
{
    static char *InitialText = "To: \nSubject: \nCC: ";

    if (sendmessage_HasChanged(sendmessage)) {
	if (!sendmessage_AskEraseUnsentMail(sendmessage)) {
	    return;
	}
    }
    sendmessage_Clear(sendmessage);
    text_InsertCharacters(sendmessage->HeadText, 0, InitialText, strlen(InitialText));
    SetSendingDot(sendmessage->HeadTextview, sendmessage->HeadText);
    SetNotModified(sendmessage);
    MakeHeaderFieldsBold(sendmessage);
}


MakeHeaderFieldsBold(self)
struct sendmessage *self;
{
    int i, len;
    struct text *t = self->HeadText;
    Boolean SawNewline = TRUE;
    char c;

    for (i=0, len = text_GetLength(t); i < len; ++i) {
	c = text_GetChar(t, i);
	if (SawNewline) {
	    if (!isspace(c)) {
		MakeOneHeaderFieldBold(self, i);
	    }
	}
	SawNewline = (c == '\n');
    }
}

MakeOneHeaderFieldBold(self, pos)
struct sendmessage *self;
int pos;
{
    int i, len;
    struct text *t = self->HeadText;

    for (i=pos, len = text_GetLength(t); i < len; ++i) {
	if (text_GetChar(t, i) == ':') { /* Found the end point */
	    struct environment *et;

	    et = environment_WrapStyle(t->rootEnvironment, pos, i-pos, self->BoldStyle);
	    environment_SetStyle(et, (pos > 0), TRUE);
	    return;
	}
    }
}

sendmessage__AddToToHeader(sm, line)
struct sendmessage *sm;
char *line;
{
    static char ToStates[] = "\nTo:";
    int pos, len, state = 1;
    struct textview *v;
    struct text *d;

    v = sm->HeadTextview;
    d = sm->HeadText;
    for (pos = 0, len = text_GetLength(d); pos < len; ++pos) {
	if (text_GetChar(d, pos) == ToStates[state]) {
	    ++state;
	} else {
	    state = 0;
	}
	if (state == 4) break;
    }
    for (++pos; pos < len && text_GetChar(d, pos) != '\n'; ++pos) {
    }
    text_InsertCharacters(d, pos, line, strlen(line));
    textview_WantUpdate(v,v);
}

sendmessage__ResetSendingDot(sm)
struct sendmessage *sm;
{
    if (SetSendingDot(sm->HeadTextview, sm->HeadText)) {
	textview_WantInputFocus(sm->BodyTextview, sm->BodyTextview);
    } else {
	textview_WantInputFocus(sm->HeadTextview, sm->HeadTextview);
    }
}

sendmessage__ResetFromParameters(sendmessage, ToName, Subject, CC, IncludeFile, Delete)
struct sendmessage *sendmessage;
char *ToName, *Subject, *CC, *IncludeFile;
int Delete;
{
    FILE *fp;
    struct text *d;
    struct textview *bv, *hv;
    char ErrorText[256], BigBuf[1000];

    if (sendmessage_HasChanged(sendmessage)) {
	if (!sendmessage_AskEraseUnsentMail(sendmessage)) {
	    return;
	}
    }
    hv = sendmessage->HeadTextview;
    bv = sendmessage->BodyTextview;
    if (sendmessage_Clear(sendmessage)) {
	return(-1);
    }
    d = sendmessage->HeadText;
    sprintf(BigBuf, "To: %s\nSubject: %s\nCC: %s",
	ToName ? ToName : "", Subject ? Subject : "", CC ? CC : "");
    text_InsertCharacters(d, 0, BigBuf, strlen(BigBuf));
    d = sendmessage->BodyText;
    if (IncludeFile && *IncludeFile) {
	if ((fp = fopen(IncludeFile, "r")) == NULL) {
	    if (errno == ENOENT) {
		sprintf(ErrorText, "There is no file named '%s'.", ams_ap_Shorten(ams_GetAMS(), IncludeFile));
		message_DisplayString(sendmessage, 10, ErrorText);
	    } else {
		sprintf(ErrorText, "Cannot open source file %s", ams_ap_Shorten(ams_GetAMS(), IncludeFile));
		ams_ReportError(ams_GetAMS(), ErrorText, ERR_WARNING, FALSE, 0);
	    }
	    return(-1);
	}
	text_Read(d, fp, 0);
	fclose(fp);
    }
    sendmessage_ResetSendingDot(sendmessage);
    MakeHeaderFieldsBold(sendmessage);
    textview_WantUpdate(hv, hv);
    textview_WantUpdate(bv, bv);
    if (Delete && IncludeFile && *IncludeFile) unlink(IncludeFile);
    return(0);
}


int sendmessage__CheckRecipients(sm)
struct sendmessage *sm;
{
    int tot, ext, format, strip, trust;

    return(CheckAndCountRecipients(sm, &tot, &ext, &format, &strip, &trust));
}

CheckAndCountRecipients(sm, tot, ext, totformat, totstrip, tottrust)
struct sendmessage *sm;
int *tot, *ext, *totformat, *totstrip, *tottrust;
{
    int code, code2, extct, extct2, totct, totct2, formatct, stripct, trustct, formatct2, stripct2, trustct2;
    char ErrorText[256];

    ams_WaitCursor(TRUE);
    message_DisplayString(sm, 10, "Validating recipient names; please wait.");
    sendmessage_SetCurrentState(sm, SM_STATE_VALIDATING);
    im_ForceUpdate();
    extct = extct2 = totct = totct2 = formatct = formatct2 = stripct = stripct2 = trustct = trustct2 = 0;
    code = ValidateHeader(sm, "\nTo:", &extct, &totct, &formatct, &stripct, &trustct);
    code2 = ValidateHeader(sm, "\nCC:", &extct2, &totct2, &formatct2, &stripct2, &trustct2);
    ams_WaitCursor(FALSE);
    im_ForceUpdate();
    *tot = totct + totct2;
    *ext = extct + extct2;
    *totformat = formatct + formatct2;
    *totstrip = stripct + stripct2;
    *tottrust = trustct + trustct2;
    sendmessage_SetCurrentState(sm, SM_STATE_VALIDATED);
    if (code || code2) {
	if (code >= 0 && code2 >= 0) {
	    code += code2;
	    if (*tot > 1) {
		if (*tot == code) {
		    strcpy(ErrorText, (code == 2) ? "Both" : "All");
		} else {
		    strcpy(ErrorText, amsutil_cvEng(code, 1, 1000));
		}
		strcat(ErrorText, " of the ");
		strcat(ErrorText, amsutil_cvEng(*tot, 0, 1000));
		strcat(ErrorText, " names you typed ");
		strcat(ErrorText, (code > 1) ? "are" : "is");
		strcat(ErrorText, " invalid.");
	    } else {
		strcpy(ErrorText, "The name you typed is invalid");
	    }
	    message_DisplayString(sm, 25, ErrorText);
	} else {
	    message_DisplayString(sm, 25, "Cannot parse the headers to validate them.");
	    code = -1;
	}
	sendmessage_SetCurrentState(sm, SM_STATE_VALIDATEFAILED);
	im_ForceUpdate();
	return(code);
    }
    if (*tot > 2) {
	sprintf(ErrorText, "All %s recipient names seem to be OK.", amsutil_cvEng(*tot, 0, 1000));
    } else if (*tot > 1) {
	strcpy(ErrorText, "Both recipient names seem to be OK.");
    } else {
	strcpy(ErrorText, "The recipient name you typed seems to be OK.");
    }
    message_DisplayString(sm, 10, ErrorText);
    im_ForceUpdate();
    return(0);
}

ValidateHeader(sm, lookfor, externalct, totct, formatct, stripct, trustct)
struct sendmessage *sm;
char *lookfor;
int *externalct, *totct, *formatct, *stripct, *trustct;
{
    char *tp, *old, *new, *s, *realname;
    int tpos, pos, newpos, len, i, errct, c;
    struct text *d;
    Boolean Searching = FALSE, SeeingAt = FALSE;
    struct textview *v;
    struct SearchPattern *pat;

    v = sm->HeadTextview;
    d = sm->HeadText;
    pat = NULL;
    tp = search_CompilePattern(lookfor, &pat);
    if (tp != 0) {
	return(-1);
    }
    text_InsertCharacters(d, 0, "\n", 1);
    pos = search_MatchPattern(d, 0, pat);
    text_DeleteCharacters(d, 0, 1);
    if (pos-- < 0) {
	return(0);
    }
    pos += strlen(lookfor);
    while (text_GetChar(d, pos) == ' ') ++pos;
    tp = search_CompilePattern("\n", &pat);
    Searching = TRUE;
    tpos = pos;
    while (Searching) {
	newpos = search_MatchPattern(d, tpos, pat);
	if (newpos < 0) {
	    newpos = text_GetLength(d);
	    Searching = FALSE;
	}
	c = text_GetChar(d, newpos+1);
	if (c != ' ' && c != '\t') {
	    Searching = FALSE;
	}
	tpos = newpos+1;
    }
    len = newpos -pos;
    s = old = malloc(2+len);
    new = malloc(4*len);
    for (i = pos; i<newpos; ++i) {
	c = text_GetChar(d, i);
	if (c == '@') {
	    if (SeeingAt) continue;
	    SeeingAt = TRUE;
	} else {
	    SeeingAt = FALSE;
	}
	*s++ = c;
    }
    *s = '\0';
    s = (char *) amsutil_StripWhiteEnds(old);    
    if (!*s) {
	free(old);
	free(new);
	return(0);
    }
    *totct = *externalct = 0;
    errct = ams_CUI_RewriteHeaderLineInternal(ams_GetAMS(), s, &realname, 25, totct, externalct, formatct, stripct, trustct);
    if (realname) {
	text_DeleteCharacters(d, pos, len);
	text_InsertCharacters(d, pos, realname, strlen(realname));
	free(realname);
    }
    textview_WantUpdate(v,v);
    free(old);
    free(new);
    return(errct);
}

sendmessage__Checkpoint(sm)
struct sendmessage *sm;
{
    static char *VCKPFileName = NULL, *TCKPFileName = NULL, *s, *ckpfile;
    char Msg[100+MAXPATHLEN], TmpFileName[1+MAXPATHLEN];

    if (sm->HeadCheckpoint == text_GetModified(sm->HeadText) && sm->BodyCheckpoint == text_GetModified(sm->BodyText)) {
	return(0);
    }
    strcpy(Msg, "Checkpointing message...");
    message_DisplayString(sm, 10, Msg);
    im_ForceUpdate();
    if (amsutil_GetOptBit(EXP_CKPONTMP)) {
	if (!TCKPFileName) {
	    ams_CUI_GenTmpFileName(ams_GetAMS(), TmpFileName);
	    TCKPFileName = malloc(1+strlen(TmpFileName));
	    if (!TCKPFileName) {
		message_DisplayString(sm, 99, "Out of memory!");
		return(-1);
	    }
	    strcpy(TCKPFileName, TmpFileName);
	}
	ckpfile = TCKPFileName;
    } else {
	if (!VCKPFileName) {
	    s = environ_GetProfile("messages.checkpointdir");
	    if (!s) s = "~";
	    ams_TildeResolve(ams_GetAMS(), s, TmpFileName);
	    strcat(TmpFileName, "/");
	    strcat(TmpFileName, ams_ams_genid(ams_GetAMS(), 1));
	    strcat(TmpFileName, ".CKP");
	    VCKPFileName = malloc(1+strlen(TmpFileName));
	    if (!VCKPFileName) {
		message_DisplayString(sm, 99, "Out of memory!");
		return(-1);
	    }
	    strcpy(VCKPFileName, TmpFileName);
	}
	ckpfile = VCKPFileName;
    }
    sm->CKPFileName = malloc(1+strlen(ckpfile));
    if (!sm->CKPFileName) {
	message_DisplayString(sm, 99, "Out of memory!");
	return(-1);
    }
    strcpy(sm->CKPFileName, ckpfile);
    if (!(WriteOneFile(sm, sm->CKPFileName, FALSE, TRUE, 12, 1))) {
	sm->HeadCheckpoint = text_GetModified(sm->HeadText);
	sm->BodyCheckpoint = text_GetModified(sm->BodyText);
    }
    strcat(Msg, " done.  Wrote CKP file in ");
    strcat(Msg, ams_ap_Shorten(ams_GetAMS(), sm->CKPFileName));
    strcat(Msg, ".");
    message_DisplayString(sm, 10, Msg);
    im_ForceUpdate();
    return(0);
}

RemoveUselessHeaderLines(sm)
struct sendmessage *sm;
{
    struct text *d;
    char *tp;
    struct SearchPattern *pat;
    int pos, orgpos, c;

    d = sm->HeadText;
    /* First we get rid of the CC line if it is empty */
    pat = NULL;
    tp = search_CompilePattern("CC:", &pat);
    if (tp != 0) {
	return(-1);
    }
    orgpos = pos = search_MatchPattern(d, 0, pat);
    if (pos >= 0) {
	pos += 3;
	while ((c = text_GetChar(d, pos)) == ' ' || c == '\t') ++pos;
	if (c=='\n' || c == -1 || c == 255) {
	    /* Aha!  We need to remove this sucker! */
	    text_DeleteCharacters(d, orgpos, pos-orgpos+1);
	    textview_WantUpdate(sm->HeadTextview, sm->HeadTextview);
	}
    }
    /* Now we get rid of any blank lines */
    pat = NULL;
    tp = search_CompilePattern("\n\n", &pat);
    if (tp != 0) {
	return(-1);
    }
    while (TRUE) {
	pos = search_MatchPattern(d, 0, pat);
	if (pos < 0) {
	    return(0);
	}
	text_DeleteCharacters(d, pos, 1);
    }
}

FileIntoFolder(sm, name)
struct sendmessage *sm;
char *name;
{
    char ShortName[1+MAXPATHLEN], *FullName, FileName[1+MAXPATHLEN], TmpFileName[1+MAXPATHLEN];
    long mserrcode;

    if (name && *name != '?') {
	strcpy(ShortName, name);
    } else {
	if (ams_GetFolderName("Save draft in what folder? ", ShortName, sizeof(ShortName), name ? ++name : "", FALSE)) return(-1);
    }
    ams_WaitCursor(TRUE);
    if (ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &FullName)) {
	ams_WaitCursor(FALSE);
	ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
	return(-1);
    }
    sprintf(TmpFileName, "~/SMTMP.%d", getpid());
    ams_TildeResolve(ams_GetAMS(), TmpFileName, FileName);
    ams_MS_UnlinkFile(ams_GetAMS(), FileName); /* Just to be sure */
    if (sendmessage_WriteFile(sm, FileName)) {
	ams_WaitCursor(FALSE);
	return(-1); /* error was reported */
    }
    mserrcode = ams_MS_AppendFileToFolder(ams_GetAMS(), FileName, FullName);
    ams_WaitCursor(FALSE);
    if (mserrcode) {
	ams_ReportError(ams_GetAMS(), "Could not append message draft to folder", ERR_WARNING, TRUE, mserrcode);
	ams_MS_UnlinkFile(ams_GetAMS(), FileName); /* ignore errors */
	return(-1);
    }
    /* Use FileName as scratchpad for message now */
    sprintf(FileName, "Filed message draft into folder: '%s'.", ShortName);
    message_DisplayString(sm, 10, FileName);
    SetNotModified(sm);
    return(0);
}
    

void sendmessage__CheckButtons(sendmessage)
struct sendmessage *sendmessage;
{
    struct sbutton *bs=sbuttonv_ButtonData(sendmessage->buttons);
    sbutton_SetLabel(bs, SM_CLEAR, amsutil_GetOptBit(EXP_CLEARAFTER) ? "Will Clear" : "Won't Clear");
    sbutton_SetLabel(bs, SM_SIGN, amsutil_GetOptBit(EXP_SIGNMAIL) ? "Will Sign" : "Won't Sign");
    sbutton_SetLabel(bs, SM_HIDE,  amsutil_GetOptBit(EXP_HIDEAFTER) ? "Will Hide" : "Won't Hide");
    sbutton_SetLabel(bs, SM_BLIND, amsutil_GetOptBit(EXP_KEEPBLIND) ? "Will Keep Copy" : "Won't Keep Copy");
}

	
/* Convert the named file into an unscribed file */
ProduceUnscribedVersion(FileName)
char *FileName;
{
    char TmpFileName[1+MAXPATHLEN], LineBuf[5000];
    FILE *fin, *fout;
    int ct, ucode;
    struct ScribeState ScribeState;
    char MyName[1+MAXPATHLEN];
    int sameHost = ams_CUI_OnSameHost(ams_GetAMS());

    ams_CUI_GenLocalTmpFileName(ams_GetAMS(), TmpFileName);
    if (!sameHost) {
	ams_CUI_GenTmpFileName(ams_GetAMS(), MyName);
	if (ams_CUI_GetFileFromVice(ams_GetAMS(), MyName, FileName)) {
	    return(-1);
	}
    }
    else strcpy(MyName, FileName);
    fin = fopen(MyName, "r");
    if (!fin) return(-1);
    fout = fopen(TmpFileName, "w");
    if (!fout) {
	fclose(fin);
	return(-2);
    }
    while (fgets(LineBuf, sizeof(LineBuf), fin)) {
	fputs(LineBuf, fout);
	if (LineBuf[0] == '\n') break;
    }
    ucode = ams_UnScribeInit(ams_GetAMS(), "12", &ScribeState);
    while ((ct = fread(LineBuf, sizeof(char), sizeof(LineBuf), fin)) > 0) {
	if (ams_UnScribe(ams_GetAMS(), ucode, &ScribeState, LineBuf, ct, fout) < 0) {
	    fclose(fin);
	    if (!sameHost)
		unlink(MyName);
	    fclose(fout);
	    unlink(TmpFileName);
	    return(-3);
	}
    }
    if (ams_UnScribeFlush(ams_GetAMS(), ucode, &ScribeState, fout)) {
	fclose(fin);
	if (!sameHost)
	    unlink(MyName);
	fclose(fout);
	unlink(TmpFileName);
	return(-4);
    }
    fclose(fin);
    if (vfclose(fout)) {
	unlink(TmpFileName);
	return(-5);
    }
    if (!sameHost) {
	/* should do some more error checking here */
	ams_MS_UnlinkFile(ams_GetAMS(),FileName);
	ams_CUI_StoreFileToVice(ams_GetAMS(),TmpFileName,FileName);
	unlink(MyName);
	unlink(TmpFileName);
    } else {
	rename(TmpFileName, FileName);
    }
    return(0);
}

void BSSM_SendmessageCompound(sm, cmds)
struct sendmessage *sm;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), sm, "sendmessage", cmds);
}

void BSSM_SendmessageFoldersCompound(sm, cmds)
struct sendmessage *sm;
char *cmds;
{
    if (sm->folders) {
	ams_GenericCompoundAction(ams_GetAMS(), sm->folders, "folders", cmds);
    } else {
	message_DisplayString(sm, 25, "There is no related folders view.");
    }
}

void BSSM_SendmessageMessagesCompound(sm, cmds)
struct sendmessage *sm;
char *cmds;
{
    if (sm->folders) {
	ams_GenericCompoundAction(ams_GetAMS(), sm->folders, "messages", cmds);
    } else {
	message_DisplayString(sm, 25, "There is no related messages view.");
    }
}

void SBSSM_TextviewCompound(tv, cmds)
struct textview *tv;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), tv, "textview", cmds);
}

void SBSSM_DoBodiesCommand(sm, cmds)
struct sendmessage *sm;
char *cmds;
{
    SBSSM_TextviewCompound(sm->BodyTextview, cmds);
}

void SBSSM_DoHeadersCommand(sm, cmds)
struct sendmessage *sm;
char *cmds;
{
    SBSSM_TextviewCompound(sm->HeadTextview, cmds);
}

RestoreFromPS(self)
struct sendmessage *self;
{
    char *ExpandedPS, *PStext;

    if (self->PSMsg == NULL) {
	message_DisplayString(self, 10, "There is nothing to which to add a PS.");
	return;
    }
    if (sendmessage_HasChanged(self)) {
	if (!sendmessage_AskEraseUnsentMail(self)) {
	    return(0);
	}
    } 
    sendmessage_Clear(self);
/*    text_ClearCompletely(self->HeadText);
    text_SetGlobalStyle(self->HeadText, self->DefaultHeadStyle); */
    ExpandedPS = malloc(10+strlen(self->PSMsg));
    if (ExpandedPS) {
	char *s, c;
	for (s=self->PSMsg; *s; ++s) {
	    if (!strncmp(s, "\nSubject: ", 10)) {
		break;
	    }
	}
	if (*s) {
	    s += 10;
	    c = *s;
	    *s = '\0';
	    strcpy(ExpandedPS, self->PSMsg);
	    *s = c;
	    if (AlreadyPS(s)) {
		strcat(ExpandedPS, "P");
	    } else {
		strcat(ExpandedPS, "PS -- ");
	    }
	    strcat(ExpandedPS, s);
	} else {
	    free(ExpandedPS);
	    ExpandedPS = NULL;
	}
    }
    PStext = ExpandedPS ? ExpandedPS : self->PSMsg;
    text_InsertCharacters(self->HeadText, 0, PStext, strlen(PStext));
    if (ExpandedPS) free(ExpandedPS);
    MakeHeaderFieldsBold(self);
    sendmessage_ResetSendingDot(self);
    textview_WantUpdate(self->HeadTextview, self->HeadTextview);
}

AlreadyPS(subj)
char *subj;
{
    Boolean UpToS = FALSE, SawP = FALSE;
    char *s;

    for (s=subj; *s; ++s) {
	if (UpToS) return(isspace(*s));
	if (*s == 'P') {
	    SawP = TRUE;
	    continue;
	}
	if (*s != 'S') return(0);
	if (!SawP) return(0);
	UpToS = TRUE;
    }
    return(0);
}

boolean
sendmessage__InitializeClass(c) 
struct classheader *c;
{
    return(OneTimeProcInit(&sendmessage_classinfo));
}

sendmessage__HasChanged(self)
struct sendmessage *self;
{
    if (self->HeadModified != text_GetModified(self->HeadText)) {
	return 1;
    }
    if (self->BodyModified != text_GetModified(self->BodyText)) {
	return 1;
    }
    return 0;
}

sendmessage__NeedsCheckpointing(self)
struct sendmessage *self;
{
    if (self->HeadCheckpoint != text_GetModified(self->HeadText)) {
	return 1;
    }
    if (self->BodyCheckpoint != text_GetModified(self->BodyText)) {
	return 1;
    }
    return 0;
}

void sendmessage__WantUpdate(sendmessage, v)
struct sendmessage *sendmessage;
struct view *v;
{
    super_WantUpdate(sendmessage, v);
    if (sendmessage->CurrentState != SM_STATE_INPROGRESS) {
	super_WantUpdate(sendmessage, sendmessage);
    }
}

sendmessage__AskEraseUnsentMail(self)
struct sendmessage *self;
{
    return(TRUE);
}

SaveForPS(self)
struct sendmessage *self;
{
    int len = text_GetLength(self->HeadText);
    if (self->PSMsg) free(self->PSMsg);
    self->PSMsg = malloc(1+len);
    text_CopySubString(self->HeadText, 0, len, self->PSMsg, FALSE);
    self->PSMsg[len] = NULL;
}

struct folders *
sendmessage__NewFoldersInNewWindow(self)
struct sendmessage *self;
{
    struct folders *f = folders_New();

    sendmessage_SetFoldersView(self, f);
    folders_SetSendmessage(f, self);
    ams_InstallInNewWindow(folders_GetApplicationLayer(f), "messages-folders", "Message Folders", environ_GetProfileInt("folders.width", 600), environ_GetProfileInt("folders.height", 120), f);
    return(f);
}

void sendmessage_DuplicateWindow(self)
struct sendmessage *self;
{
    struct sendmessage *s = sendmessage_New();

    s->myframe = ams_InstallInNewWindow(s, "messages-send", "Message composition", environ_GetProfileInt("sendmessage.width", -1), environ_GetProfileInt("sendmessage.height", -1), s);
    sendmessage_Reset(s);
}

