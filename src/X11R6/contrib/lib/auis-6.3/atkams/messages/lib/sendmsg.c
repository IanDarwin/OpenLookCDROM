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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/sendmsg.c,v 2.61 1993/05/05 19:49:43 susan Exp $";
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

#ifdef ibm032
#include <ams.h>
typedef short Boolean;
#else
#include <cui.h>
#endif
#include <fdphack.h>
#include <mailconf.h>
#include <mail.h>
#include <dropoff.h>
#include <environ.ih>
#include <message.ih>

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

extern int WriteOneFile();
extern void delete_sendmsg_win();

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

/* Constants to declare use of ATK or new multipart datastream */
#define MAILFORMAT_UNDEFINED 0
#define	MAILFORMAT_ANDREW 1	/* use atk datastream */
#define	MAILFORMAT_MULTIPART 2	/* use new mail standard */
#define	MAILFORMAT_MULTIPARTNOASK 3 /* New mail standard for ALL recips, without even asking about their receiving capability */
#define	MAILFORMAT_ASK 4	/* ask which data format to use */

Deliver(sendmessage, formathandlingcode)
struct sendmessage *sendmessage;
int formathandlingcode;
{
    int code, total, external, ans, StreamVersion = 12, TrustDelivery = 0, nkids, format, strip, trust, ThisMailFormat;
    Boolean Unformat = FALSE, SendingWithAMSDel;
    static int MailFormatToUse = MAILFORMAT_UNDEFINED;
    static char *ExternalQVec[] = {
	"The readers of this message may not recognize Andrew formatting.",
	"Cancel sending",
	"Remove formatting & send",
	"Send with formatting",
	NULL,	/* qv[4] is *always* set below */
	NULL
    };

    if (!sendmessage_HasChanged(sendmessage) && !ams_GetBooleanFromUser(ams_GetAMS(), "Message unchanged from last delivery/draft.  Send anyway", FALSE)) {
	return(0);
    }
    message_DisplayString(sendmessage, 10, "Message delivery in progress");
    if (MailFormatToUse == MAILFORMAT_UNDEFINED) {
	int len;
	char *fmt = environ_GetProfile("messages.mailsendingformat");
	if (!fmt) fmt = environ_GetConfiguration("mailsendingformat");
	if (!fmt) {
	    MailFormatToUse = MAILFORMAT_ASK;
	} else {
	    len = strlen(fmt);
	    while (*fmt && isspace(*fmt)) ++fmt;
	    if (!amsutil_lc2strncmp("atk", fmt, len)
		|| !amsutil_lc2strncmp("andrew", fmt, len)) {
		MailFormatToUse = MAILFORMAT_ANDREW;
	    } else if (!amsutil_lc2strncmp("mime", fmt, len)
		       || !amsutil_lc2strncmp("multipart", fmt, len)) {
		MailFormatToUse = MAILFORMAT_MULTIPART;
	    } else if (!amsutil_lc2strncmp("mime-force", fmt, len)
		       || !amsutil_lc2strncmp("multipart-force", fmt, len)) {
		MailFormatToUse = MAILFORMAT_MULTIPARTNOASK;
	    } else if (!amsutil_lc2strncmp("ask", fmt, len)) {
		MailFormatToUse = MAILFORMAT_ASK;
	    } else {
		message_DisplayString(sendmessage, 10, "Unrecognized 'mailsendingformat' configuration is being ignored.");

		MailFormatToUse = MAILFORMAT_ASK;
	    }
	}
    }
    ThisMailFormat = MailFormatToUse;
    total = external = format = strip = trust = 0;
    code = CheckAndCountRecipients(sendmessage, &total, &external, &format, &strip, &trust);
    if (code) {
	return(code);
    }
    if (amsutil_GetOptBit(EXP_SIGNMAIL) && !sendmessage->HasSigned) {
	FILE *fp;
	char fname[1+MAXPATHLEN], *fnamepattern;
	char home[1+MAXPATHLEN];
	
	strcpy(home,getenv("HOME"));

	nkids = environment_NumberOfChildren(sendmessage->BodyText->rootEnvironment);
	/* Note that we will recalculate this value after inserting the signature file */
	fnamepattern = (nkids > 0) ? ".sig.fmt" : ".sig";
	sprintf(fname,"%s/%s",home,fnamepattern);
	fp = fopen(fname, "r");
	if (!fp) {
	sprintf(fname,"%s/.signature",home);
	    fp = fopen(fname, "r");
	}
	if (!fp) {
	    static char *NoSignatureQVec[] = {
		"The file '~/.signature' could not be read",
		"Send unsigned",
		"Do not send",
		NULL
	    };
	    if (ams_ChooseFromList(ams_GetAMS(), NoSignatureQVec, 2) != 1) return(-1);
	} else {
	    int len;
	    PrepareBodyForSignature(sendmessage);
	    len = text_GetLength(sendmessage->BodyText);
	    text_AlwaysInsertFile(sendmessage->BodyText, fp, fname, len);
	    textview_WantUpdate(sendmessage->BodyTextview, sendmessage->BodyTextview);
	    fclose(fp);
	    sendmessage->HasSigned = 1;
	}
    }
    nkids = environment_NumberOfChildren(sendmessage->BodyText->rootEnvironment);
    switch (ams_CUI_DeliveryType(ams_GetAMS())) {
	case DT_AMS: case DT_AMSWAIT:
	    SendingWithAMSDel = TRUE; break;
	case DT_NONAMS:
	    SendingWithAMSDel = FALSE; break;
	default:
	    SendingWithAMSDel = (ams_CUI_UseAmsDelivery(ams_GetAMS()) >= 0);
    }
    if (formathandlingcode == FORCE_SEND_FORMATTED) {
	ans = 3;
    } else if (formathandlingcode == FORCE_SEND_UNFORMATTED) {
	ans = 2;
    } else if (nkids == 0) {
	ans = SendingWithAMSDel ? 4 : 2; /* trust delivery : unformatted */
	StreamVersion = 12;
    } else if (external > 0) {
	if (external == format) {
	    ans = 3;
	} else if (external == trust) {
	    ans = 4;
	} else if ((external == strip) && (external == total)) {
	    ans = 2;
	} else if (ThisMailFormat == MAILFORMAT_MULTIPARTNOASK) {
	    ans = 3;
	    ThisMailFormat = MAILFORMAT_MULTIPART; /* Probably redundant */
	} else if (ThisMailFormat == MAILFORMAT_ASK) {
	    static char *Question[] = {
		"The readers of this message may not recognize multimedia formatting.",
		"Cancel sending",
		"Remove formatting & send",
		"Send with formatting in Andrew Format",
		"Send with formatting in MIME (Internet standard) format",
		NULL
	    };
	    ans = ams_ChooseFromList(ams_GetAMS(), Question, 1);
	    if (ans == 4) {
		ans = 3;
		ThisMailFormat = MAILFORMAT_MULTIPART;
	    } else if (ans == 3) {
		ThisMailFormat = MAILFORMAT_ANDREW;
	    }
	} else {
#ifdef CMU_ENV
	    ans = 4;		/* Always trust the delivery system */
#else
	    ExternalQVec[4] = (SendingWithAMSDel ? "Trust the delivery system to remove it as needed" : NULL);
	    ans = ams_ChooseFromList(ams_GetAMS(), ExternalQVec, 1);
#endif
	}
    } else {
	ans = 4; /* Local recipients, Raw, raw, raw! */
	StreamVersion = 12;
    }
    switch(ans) {
	case 1:
	    message_DisplayString(sendmessage, 10, "Message not sent.");
	    return(-1);
	case 2:
	    /* send unformatted */
	    StreamVersion = 0;
	    ThisMailFormat = MAILFORMAT_ANDREW; /* makes unformat work right */
	    Unformat = TRUE;
	    break;
	case 3:
	    /* send raw */
	    break;
	case 4:
	    /* Trust delivery system */
	    TrustDelivery = 1;
	    break;
	default:
	    message_DisplayString(sendmessage, 10, "Unrecognized choice code; message not sent.");
	    return(-1);
    }
    if (nkids <= 0) ThisMailFormat = MAILFORMAT_ANDREW; /* handled right  there */
    if (!Unformat && ThisMailFormat == MAILFORMAT_ASK) {
	static char *whichfmt[] = {
	    "Which mail data format do you want to use?",
	    "Andrew 'native' data stream",
	    "MIME (Internet standard) format",
            "Cancel",
	    NULL
	};
	ans = ams_ChooseFromList(ams_GetAMS(), whichfmt, 1);
	if (ans == 3) {
	    message_DisplayString(sendmessage, 10, "Message not sent.");
	    return(-1);
	}
	ThisMailFormat = (ans == 1) ? MAILFORMAT_ANDREW : MAILFORMAT_MULTIPART;
    }
    ams_WaitCursor(TRUE);
    message_DisplayString(sendmessage, 10, "Sending message; please wait...");
    if (Submit(sendmessage, Unformat, StreamVersion, TrustDelivery, ((ThisMailFormat == MAILFORMAT_MULTIPART) || ThisMailFormat == MAILFORMAT_MULTIPARTNOASK)) == 0) {
	struct im *myim = sendmessage_GetIM(sendmessage);

	if (myim && amsutil_GetOptBit(EXP_HIDEAFTER)) {
	    if (amsutil_GetOptBit(EXP_VANISH)) {
		im_VanishWindow(myim);
	    } else {
		im_HideWindow(myim);
	    }
	    if (sendmessage->folders) {
		folders_GrowWindow(sendmessage->folders);
	    }
	}
	if (amsutil_GetOptBit(EXP_CLEARAFTER)) {
	    sendmessage_Reset(sendmessage);
	}
    }
    ams_WaitCursor(FALSE);
    im_ForceUpdate();
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
    char ErrorText[256], MyName[1+MAXPATHLEN];

    if (sendmessage_HasChanged(sendmessage) && !sendmessage_AskEraseUnsentMail(sendmessage)) return(0);
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

static Submit(sendmessage, Unformat, Version, TrustDelivery, UseMultipartFormat)
struct sendmessage *sendmessage;
Boolean Unformat, UseMultipartFormat;
int Version, TrustDelivery;
{
    static int flags = 0;
    char FileName[MAXPATHLEN+1];
    int EightBitText=0;

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
    if (WriteOneFile(sendmessage, FileName, TRUE, FALSE, Version, TrustDelivery, UseMultipartFormat, &EightBitText)) return(-1);
    UnlinkCKPFile(sendmessage);
    if (Unformat && !EightBitText) {
	int code = ProduceUnscribedVersion(FileName, NULL);
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
    flags |= AMS_SEND_ILLEGAL; /* checking for illegal chars takes too long on multimedia stuff -- we now trust the _WriteOther routines instead */
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

    code = WriteOneFile(sendmessage, ViceFileName, TRUE, FALSE, 12, 1, FALSE, NULL);
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
    im_SetDeleteWindowCallback(sendmessage_GetIM(sendmessage), delete_sendmsg_win, sendmessage);
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
       /* this was a work around for an old be1 bug it should not be needed anymore
	   if (c == '@') {
	    if (SeeingAt) continue;
	    SeeingAt = TRUE;
	} else {
	    SeeingAt = FALSE;
	}
      */
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
    strcpy(Msg, "CKP: ");
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
    if (!(WriteOneFile(sm, sm->CKPFileName, FALSE, TRUE, 12, 1, FALSE, NULL))) {
	sm->HeadCheckpoint = text_GetModified(sm->HeadText);
	sm->BodyCheckpoint = text_GetModified(sm->BodyText);
    }
/*    strcat(Msg, " done.  Wrote CKP file in ");*/
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
ProduceUnscribedVersion(FileName, OutputFP)
char *FileName;
FILE *OutputFP;
{
    char TmpFileName[1+MAXPATHLEN], LineBuf[5000];
    FILE *fin, *fout;
    int ct, ucode;
    struct ScribeState ScribeState;
    char MyName[1+MAXPATHLEN];
    int sameHost = ams_CUI_OnSameHost(ams_GetAMS());

    if (!OutputFP) {
	ams_CUI_GenLocalTmpFileName(ams_GetAMS(), TmpFileName);
	if (!sameHost) {
	    ams_CUI_GenTmpFileName(ams_GetAMS(), MyName);
	    if (ams_CUI_GetFileFromVice(ams_GetAMS(), MyName, FileName)) {
		return(-1);
	    }
	}
	else strcpy(MyName, FileName);
	fout = fopen(TmpFileName, "w");
	if (!fout) {
	    return(-2);
	}
    } else {
	fout = OutputFP;
	strcpy(MyName, FileName);
    }
    fin = fopen(MyName, "r");
    if (!fin) {
	if (fout != OutputFP) fclose(fout);
	return(-1);
    }
    if (!OutputFP) {
	while (fgets(LineBuf, sizeof(LineBuf), fin)) {
	    fputs(LineBuf, fout);
	    if (LineBuf[0] == '\n') break;
	}
    }
    ucode = ams_UnScribeInit(ams_GetAMS(), "12", &ScribeState);
    while ((ct = fread(LineBuf, sizeof(char), sizeof(LineBuf), fin)) > 0) {
	if (ams_UnScribe(ams_GetAMS(), ucode, &ScribeState, LineBuf, ct, fout) < 0) {
	    fclose(fin);
	    if (!sameHost)
		unlink(MyName);
	    if (fout != OutputFP) fclose(fout);
	    unlink(TmpFileName);
	    return(-3);
	}
    }
    if (ams_UnScribeFlush(ams_GetAMS(), ucode, &ScribeState, fout)) {
	fclose(fin);
	if (!sameHost)
	    unlink(MyName);
	if (fout != OutputFP) fclose(fout);
	unlink(TmpFileName);
	return(-4);
    }
    fclose(fin);
    if (!OutputFP) {
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
    return(ams_GetBooleanFromUser(ams_GetAMS(), "Do you want to erase the mail you have not yet sent", FALSE));
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
    im_SetDeleteWindowCallback(sendmessage_GetIM(self), delete_sendmsg_win, self);
    return(f);
}

void sendmessage_DuplicateWindow(self)
struct sendmessage *self;
{
    struct sendmessage *s = sendmessage_New();

    s->myframe = ams_InstallInNewWindow(s, "messages-send", "Message composition", environ_GetProfileInt("sendmessage.width", -1), environ_GetProfileInt("sendmessage.height", -1), s);
    im_SetDeleteWindowCallback(sendmessage_GetIM(self), delete_sendmsg_win, self);
    sendmessage_Reset(s);
}

