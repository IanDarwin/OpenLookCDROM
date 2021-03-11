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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/sendaux.c,v 1.43 1994/01/03 22:56:01 rr2b Exp $";
#endif


                                 

#include <andrewos.h>                  /* sys/file.h */
#include <stdio.h>
#include <sys/param.h>
#include <util.h>
#include <pwd.h>
#include <ctype.h>

#include <class.h>
#include <proctbl.ih>
/* #include <keymap.ih> */
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <style.ih>
#include <stylesht.ih>
#define dontDefineRoutinesFor_fontdesc
#include <fontdesc.ih>
#undef dontDefineRoutinesFor_fontdesc
#include <text.ih>
#include <envrment.ih>
#include <textv.ih>
#include <environ.ih>
#include <message.ih>
#include <im.ih>
#include <frame.ih>
#include <tree23.ih>
#include <complete.ih>
#define dontDefineRoutinesFor_nestedmark
#include <nstdmark.ih>
#undef dontDefineRoutinesFor_nestedmark

#include <cui.h>
#include <fdphack.h>
#include <ams.ih>
#include <amsutil.ih>
#include <folders.ih>
#define AUXMODULE 1
#include <sendmsg.eh>

static struct keymap *smkm, *smhkm;
static struct menulist *sm_menulist, *sm_hmenulist;

/* values for sendmessage menu mask */
#define SMMASK_FEWSTYLES 1
#define SMMASK_HASMESS 2
#define SMMASK_FORCESEND 4
#define SMMASK_CHECKRECIP 8
#define SMMASK_INSERTHEAD 16
#define SMMASK_FILEINTO 32

static int AbsentProcedure(self)
struct view    *self;
{
    message_DisplayString(self, 75, "Absent procedure - did not find a normal BE2 command in the proctable!");
}

static int      (*textv_EndOfLineCmd) () = AbsentProcedure,
                (*textv_OpenLineCmd) () = AbsentProcedure,
                (*textv_InsertNewlineCmd) () = AbsentProcedure,
                (*textv_PreviousLineCmd) () = AbsentProcedure,
                (*textv_InsertFileCmd) () = AbsentProcedure,
                (*textv_BeginningOfLineCmd) () = AbsentProcedure,
                (*textv_PlainestCmd) () = AbsentProcedure,
                (*textv_NextLineCmd) () = AbsentProcedure;

extern void     sendmesage_SetButtonFont(), ForceSending(), ForceStripping(), sendmessage__CheckRecipients(), UserWantsAHeader(), FileIntoFolder(), sendmessage__QuoteBody(), RestoreFromPS(), sendmessage__AppendBugInfoToBody(), BSSM_FakeBug(), BSSM_DownFocus(), BSSM_UpFocus(), BSSM_HeadersFocus(), BSSM_BodyFocus(), BSSM_Preview(), ComposeBugReport(), sendmessage__Reset(), sendmessage_DoDelivery(), sendmessage_InsertFile(), BeginLine(), NextLine(), PreviousLine(), BSSM_SendmessageFoldersCompound(), BSSM_SendmessageMessagesCompound(), BSSM_SendmessageCompound(), SetNotModified(), SBSSM_DoHeadersCommand(), SBSSM_DoBodiesCommand(), SBSSM_TextviewCompound(), sendmessage_DuplicateWindow();
int WriteOneFile();

static int AddSpecialHeaders(sm)
struct sendmessage *sm;
{
    struct textview *tv;
    int             ans, pos, oldpos, oldlen, useplus;
    char            buf[1000], head[2000], ShortName[1 + MAXPATHLEN], *FullName, *md;
    static char    *SVec[] = {
        "What kind of `special' action should this message take?",
        "Cancel",
        "Request a return-receipt",
        "Request a vote",
        "Announce an Enclosure",
        "Invite folder subscriptions",
        "Invite further redistribution",
        NULL
    };
    static char    *EVec[] = {
        "What do you want to call an 'enclosure'?",
        "Cancel",
        "The whole message",
        "A certain file",
        NULL
    };
    static char    *EnclosureString = "---- Enclosure ----";
    char            EnclosureBuf[50];

    ans = ams_ChooseFromList(ams_GetAMS(), SVec, 1);
    switch (ans) {
        case 1:
            /* nothing */
            message_DisplayString(sm, 10, "No special headers added.");
            return (-1);
        case 2:
            /* return receipt */
            md = ams_CUI_MailDomain(ams_GetAMS());
            useplus = ams_CUI_UseAmsDelivery(ams_GetAMS());
            if (useplus <= 0) useplus = ams_CheckAMSUseridPlusWorks(ams_GetAMS(), md);
            sprintf(head, "%s%s@%s", ams_CUI_WhoIAm(ams_GetAMS()), useplus > 0 ? "+" : "", md);
            if (message_AskForString(sm, 50, "Request acknowledgement to: ", head, buf, sizeof(buf)) >= 0) {
                char            Msg[500], *valaddr = NULL;

                sprintf(Msg, "Validating %s; please wait...", buf);
                message_DisplayString(sm, 10, Msg);
                im_ForceUpdate();
                ams_WaitCursor(TRUE);
                if (ams_CUI_RewriteHeaderLine(ams_GetAMS(), buf, &valaddr) || !valaddr) {
                    message_DisplayString(sm, 10, "Bad address specified -- not return receipt request.");
                    ams_WaitCursor(FALSE);
                    return (-1);
                }
                ams_WaitCursor(FALSE);
                strcpy(head, "Ack-to: ");
                strcat(head, valaddr);
                sendmessage_AddHeaderLine(sm, head);
                free(valaddr);
            }
            break;
        case 3:
            {
                /* vote */
                char            Question[500], Answer[100], ID[100], Addr[300], Prompt[100], DefaultID[100], *valaddr = NULL, *md;
                int             index = 1, HasWriteIn = 0, useplus;
                static int      votectr = 0;

                md = ams_CUI_MailDomain(ams_GetAMS());
                useplus = ams_CUI_UseAmsDelivery(ams_GetAMS());
                if (useplus <= 0) useplus = ams_CheckAMSUseridPlusWorks(ams_GetAMS(), md);
                sprintf(head, "%s%s@%s", ams_CUI_WhoIAm(ams_GetAMS()), useplus > 0 ? "+" : "", md);
                if (message_AskForString(sm, 50, "Send votes to: ", head, Addr, sizeof(Addr)) < 0) {
                    return (-1);
                }
                sprintf(Question, "Validating %s; please wait...", Addr);
                message_DisplayString(sm, 10, Question);
                im_ForceUpdate();
                ams_WaitCursor(TRUE);
                if (ams_CUI_RewriteHeaderLine(ams_GetAMS(), Addr, &valaddr) || !valaddr) {
                    message_DisplayString(sm, 10, "Bad address specified -- not composing vote request.");
                    ams_WaitCursor(FALSE);
                    return (-1);
                }
                ams_WaitCursor(FALSE);
                if (message_AskForString(sm, 50, "Please enter the vote question: ", NULL, Question, sizeof(Question)) < 0) {
                    free(valaddr);
                    return (-1);
                }
                QuoteProperly(Question);
                buf[0] = '\0';
                while (TRUE) {
                    sprintf(Prompt, "Please enter vote choice #%d [default: all done] : ", index);
                    if (message_AskForString(sm, 50, Prompt, NULL, Answer, sizeof(Answer)) < 0) {
                        free(valaddr);
                        return (-1);
                    }
                    if (Answer[0] == '\0')
                        break;
                    if (index++ > 1)
                        strcat(buf, ", ");
                    if (index > 8) {
                        message_DisplayString(sm, 10, "That's enough choices.");
                        break;
                    }
                    QuoteProperly(Answer);
                    strcat(buf, Answer);
                    if (Answer[0] == '*' && Answer[1] == '\0')
                        HasWriteIn = 1;
                }
                if (!HasWriteIn) {
                    if (ams_GetBooleanFromUser(ams_GetAMS(), "Do you want to allow write-in votes", FALSE)) {
                        if (index++ > 1)
                            strcat(buf, ", ");
                        strcat(buf, "*");
                    }
                }
                sprintf(DefaultID, "%s.%d.%d", ams_CUI_MachineName(ams_GetAMS()) ? ams_CUI_MachineName(ams_GetAMS()) : "unknown-host", getpid(), votectr++);
                if (message_AskForString(sm, 50, "Your ID for this vote: ", DefaultID, ID, sizeof(ID)) < 0) {
                    free(valaddr);
                    return (-1);
                }
                QuoteProperly(ID);
                strcpy(head, "Vote-Request: ");
                strcat(head, ID);
                strcat(head, ", ");
                strcat(head, Question);
                sendmessage_AddHeaderLine(sm, head);
                strcpy(head, "Vote-To: ");
                strcat(head, valaddr);
                sendmessage_AddHeaderLine(sm, head);
                free(valaddr);
                strcpy(head, "Vote-Choices: ");
                strcat(head, buf);
                sendmessage_AddHeaderLine(sm, head);
                break;
            }
        case 4:
            /* enclosure */
            ans = ams_ChooseFromList(ams_GetAMS(), EVec, 1);
            switch (ans) {
                case 1:
                    /* Nothing */
                    break;
                case 2:
                    /* the whole message */
                    sendmessage_AddHeaderLine(sm, "Enclosure: ");
                    break;
                case 3:
                    /* a certain file */
                    sprintf(EnclosureBuf, "Enclosure: %s", EnclosureString);
                    sendmessage_AddHeaderLine(sm, EnclosureBuf);
                    tv = sm->BodyTextview;
                    oldpos = textview_GetDotPosition(tv);
                    oldlen = textview_GetDotLength(tv);
                    pos = text_GetLength(sm->BodyText);
                    sprintf(EnclosureBuf, "\n%s\n", EnclosureString);
                    text_InsertCharacters(sm->BodyText, pos, EnclosureBuf, strlen(EnclosureBuf));
                    pos = text_GetLength(sm->BodyText);
                    textview_SetDotPosition(tv, pos);
                    textv_InsertFileCmd(tv);
                    pos = text_GetLength(sm->BodyText);
                    text_InsertCharacters(sm->BodyText, pos, EnclosureBuf, strlen(EnclosureBuf));
                    textview_SetDotPosition(tv, pos);
                    textview_SetDotLength(tv, strlen(EnclosureBuf));
                    textv_PlainestCmd(tv);
                    textview_SetDotPosition(tv, oldpos);
                    textview_SetDotLength(tv, oldlen);
                    break;
            }
            break;
        case 5:
            /* subscription */
            if (message_AskForString(sm, 50, "Invite subscriptions to what folder? ", NULL, ShortName, sizeof(ShortName)) < 0)
                return (-1);
            if (ams_CUI_DisambiguateDir(ams_GetAMS(), ShortName, &FullName)) {
                ams_CUI_ReportAmbig(ams_GetAMS(), ShortName, "folder");
                return (-1);
            }
            strcpy(head, "X-Andrew-DirectoryCreation: ");
            strcat(head, FullName);
            sendmessage_AddHeaderLine(sm, head);
            break;
        case 6:
            /* Invite redistribution */
            {
                char            Question[500], Addr[1000], *valaddr = NULL, head[1100];

                if (message_AskForString(sm, 50, "Invite redistribution to: ", "", Addr, sizeof(Addr)) < 0) {
                    return (-1);
                }
                sprintf(Question, "Validating %s; please wait...", Addr);
                message_DisplayString(sm, 10, Question);
                im_ForceUpdate();
                ams_WaitCursor(TRUE);
                if (ams_CUI_RewriteHeaderLine(ams_GetAMS(), Addr, &valaddr) || !valaddr) {
                    message_DisplayString(sm, 10, "Bad address specified -- not composing redistribution request.");
                    ams_WaitCursor(FALSE);
                    return (-1);
                }
                ams_WaitCursor(FALSE);
                strcpy(head, "X-Andrew-Redistribution-To: ");
                strcat(head, valaddr);
                sendmessage_AddHeaderLine(sm, head);
                free(valaddr);
                break;
            }
        default:
            message_DisplayString(sm, 10, "Unrecognized answer - no headers added.");
            return (-1);
    }
    message_DisplayString(sm, 10, "Done.");
    return (0);
}

QuoteProperly(str)
char           *str;
{
    if (strchr(str, '"') || strchr(str, '\\') || strchr(str, ',')) {
        char            MyBuf[2000];
        register char  *s, *t;

        /* Needs quoting */
        MyBuf[0] = '"';
        for (s = str, t = &MyBuf[1]; *s; ++s, ++t) {
            if ((*s == '"') || (*s == '\\')) {
                *t++ = '\\';
            }
            *t = *s;
        }
        *t++ = '"';
        *t = '\0';
        strcpy(str, MyBuf);
    }
}

void            QuitMessages(self)
struct sendmessage *self;
{
    ams_CommitState(TRUE, FALSE, TRUE, TRUE);
}

static char     lastWindowWarning[] =
"This is the last window.";
static char    *lastWindowChoices[] = {
    "Continue Running",
    "Quit Messages",
NULL};

#define lastWindow_CANCEL 0
#define lastWindow_QUIT   1

static void     DeleteWindow(self)
struct sendmessage *self;
{
    if (sendmessage_HasChanged(self)) {
        if (!sendmessage_AskEraseUnsentMail(self)) {
            return;
        }
        sendmessage_Clear(self);
    }
    if (ams_CountAMSViews() > 1) {
	ams_CommitState(FALSE, FALSE, FALSE, FALSE);
	if (sendmessage_GetIM(self)) {
	    struct im *im=sendmessage_GetIM(self);
	    if(self->myframe) frame_SetView(self->myframe, NULL);
	    im_SetView(im,NULL);
	    if(self->myframe) frame_Destroy(self->myframe);
	    im_Destroy(im);
	}
        sendmessage_Destroy(self);
    }
    else {
        long            answer;

        if (message_MultipleChoiceQuestion(NULL, 0,
                                        lastWindowWarning, lastWindow_CANCEL,
                                           &answer, lastWindowChoices, NULL)
            == -1)
            return;
        switch (answer) {
            case lastWindow_CANCEL:
                return;

            case lastWindow_QUIT:
                QuitMessages(self);
        }
    }
}

void delete_sendmsg_win(im, self)
struct im *im;
struct sendmessage *self;
{
    DeleteWindow(self);
}

void            BSSM_DummyQuit(self)
struct sendmessage *self;
{
    message_DisplayString(NULL, 10, "Use ^X^C to quit.");
}

void            ToggleClearButt(self)
struct sendmessage *self;
{
    HandleButton(self->buttons, self, SM_CLEAR, EXP_CLEARAFTER);
}

void            ToggleSignButt(self)
struct sendmessage *self;
{
    HandleButton(self, self, SM_SIGN, EXP_SIGNMAIL);
}

void            ToggleHideButt(self)
struct sendmessage *self;
{
    HandleButton(self, self, SM_HIDE, EXP_HIDEAFTER);
}

void            ToggleBlindButt(self)
struct sendmessage *self;
{
    HandleButton(self, self, SM_BLIND, EXP_KEEPBLIND);
}

void            SimulateResetButton(self)
struct sendmessage *self;
{
    HandleButton(self, self, SM_RESET, 0);
}

#define PREVIEW_ASK 0
#define PREVIEW_UNSCRIBE 1
#define PREVIEW_RAW 2

static char    *PreviewCmdVector[] = {NULL, "-p", "-z", NULL, NULL};
static char     PreviewProg[1 + MAXPATHLEN] = "";

void            BSSM_Preview(sm)
struct sendmessage *sm;
{
    DoPreview(sm, PREVIEW_ASK);
}

static char    *PrevQVec[] = {
    "Which kind of preview do you want?",
    "Cancel",
    "Show with formatting removed",
    "Show with raw formatting information",
    NULL
};

DoPreview(sm, code)
struct sendmessage *sm;
int             code;
{
    char            FileName[1 + MAXPATHLEN];
    int             ans, rcode;

    if (code == PREVIEW_ASK) {
        ans = ams_ChooseFromList(ams_GetAMS(), PrevQVec, 1);
        if (ans != 2 && ans != 3)
            return;
    }
    else {
        if (code == PREVIEW_UNSCRIBE) {
            ans = 2;
        }
        else {
            ans = 3;
        }
    }
    ams_CUI_GenLocalTmpFileName(ams_GetAMS(), FileName);
    if (WriteOneFile(sm, FileName, FALSE, TRUE, (ans == 2) ? 0 : 12, (code == PREVIEW_UNSCRIBE) ? 1 : 0, FALSE, NULL)) {
        return;                        /* error was reported */
    }
    if (!PreviewProg[0]) {
        strcpy(PreviewProg, environ_AndrewDir("/bin/ezprint"));
        PreviewCmdVector[0] = PreviewProg;
    }
    PreviewCmdVector[3] = FileName;
    if (ans == 2) {
        rcode = ProduceUnscribedVersion(FileName, NULL);
        if (rcode) {
            char            ErrorText[256];

            sprintf(ErrorText, "Unformatting error (%d, %d); cannot produce unformatted text", rcode, errno);
            message_DisplayString(sm, 10, ErrorText);
        }
    }
    rcode = osi_vfork();
    if (rcode == 0) {
        int             fd;

        /* I am a child */
        for (fd = getdtablesize(); fd > 2; --fd)
            close(fd);
        execv(PreviewCmdVector[0], PreviewCmdVector);
        _exit(-1);                     /* not reached */
    }
    if (rcode > 0) {
        message_DisplayString(sm, 10, "Preview window should appear soon.");
    }
    else {
        message_DisplayString(sm, 10, "Preview fork failed!");
    }
}
static struct bind_Description smheadbindings[] = {
    {"sendmessage-next-line", "\015", 1, NULL, NULL, NULL, NextLine, "control-n function", NULL},
    {"sendmessage-next-line", "\012", 1, NULL, NULL, NULL, NextLine, "control-n function", NULL},
    {"sendmessage-add-header", "\017", NULL, "Other~42,Insert Header~88", NULL, SMMASK_INSERTHEAD, UserWantsAHeader, "control-o function", NULL},
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}
};


static char DraftFileNameBuf[MAXPATHLEN+1] = "~/Draft.mail";

ReadDraft(File, sendmessage)
char *File;
struct sendmessage *sendmessage;
{
    char FileBuf[1+MAXPATHLEN];

    ams_TildeResolve(ams_GetAMS(), File, FileBuf);
    sendmessage_ReadFromFile(sendmessage, FileBuf, FALSE);
}

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

static struct bind_Description smbindings[] = {

    /*
     * procname, keysequenece, key rock, menu string, menu rock, proc,
     * docstring, dynamic autoload
     */
    {"sendmessage-toggle-clear-button", NULL, NULL, NULL, NULL, 0, ToggleClearButt, "simulate the 'clear' button"},
    {"sendmessage-toggle-hide-button", NULL, NULL, NULL, NULL, 0, ToggleHideButt, "simulate the 'hide' button"},
    {"sendmessage-toggle-sign-button", NULL, NULL, NULL, NULL, 0, ToggleSignButt, "simulate the 'sign' button"},
    {"sendmessage-toggle-blind-button", NULL, NULL, NULL, NULL, 0, ToggleBlindButt, "simulate the 'blind' button"},
    {"sendmessage-simulate-reset-button", NULL, NULL, NULL, NULL, 0, SimulateResetButton, "simulate the 'reset' button"},
    {"sendmessage-dummy-quit", "\003", NULL, NULL, NULL, 0, BSSM_DummyQuit, "dummy quit command"},
    {"sendmessage-quit", "\030\003", NULL, "Quit~99", NULL, 0, QuitMessages, "exit messages"},
    {"sendmessage-delete-window", "\030\004", NULL, "Delete Window~89", 0, NULL, DeleteWindow, "Delete sendmessage window"},
    {"sendmessage-duplicate", "\0302", NULL, NULL, NULL, 0, sendmessage_DuplicateWindow, "Open another sendmessage window"},
    {"sendmessage-force-send", NULL, NULL, "Send Formatted~23", NULL, SMMASK_FORCESEND, (void (*) ()) ForceSending, "Send mail even if formatted", NULL},
    {"sendmessage-force-strip", NULL, NULL, "Send Unformatted~24", NULL, SMMASK_FORCESEND, (void (*) ()) ForceStripping, "Send mail stripping all formatting", NULL},
    {"sendmessage-add-header", NULL, NULL, "Other~42,Insert Header~88", NULL, SMMASK_INSERTHEAD, UserWantsAHeader, "control-o function", NULL},
    {"sendmessage-check-recipients", NULL, NULL, "Other~42,Check Recipients~75", NULL, SMMASK_CHECKRECIP, (void (*) ()) sendmessage__CheckRecipients, "Check recipient list", NULL},
    {"sendmessage-file-into-folder", NULL, NULL, "Other~42,File Into Folder~55", NULL, SMMASK_FILEINTO, (void (*) ()) FileIntoFolder, "File draft message into a folder"},
    {"sendmessage-excerpt-body", "\030q", 0, "Other~42,Excerpt Body~36", NULL, SMMASK_HASMESS, (void (*) ()) sendmessage__QuoteBody, "Quote message on display"},
    {"sendmessage-PS", NULL, NULL, "Other~42,Send PS~97", NULL, NULL, (void (*) ()) RestoreFromPS, "Try to send another piece of mail to the same address(es)"},
    {"sendmessage-append-bug-info", NULL, NULL, NULL, NULL, 0, (void (*) ()) sendmessage__AppendBugInfoToBody, "Append bug report information to the message being composed."},
    {"sendmessage-fake-bug", NULL, NULL, NULL, NULL, 0, (void (*) ()) BSSM_FakeBug, "Fake a bug report"},
    {"sendmessage-down-input-focus", "\030n", NULL, NULL, NULL, 0, BSSM_DownFocus, "Move input focus down"},
    {"sendmessage-up-input-focus", "\030p", NULL, NULL, NULL, 0, BSSM_UpFocus, "Move input focus up"},
    {"sendmessage-down-input-focus", "\030\016", NULL, NULL, NULL, 0, BSSM_DownFocus, "Move input focus down"},
    {"sendmessage-up-input-focus", "\030\020", NULL, NULL, NULL, 0, BSSM_UpFocus, "Move input focus up"},
    {"sendmessage-focus-on-headers", NULL, NULL, NULL, NULL, 0, BSSM_HeadersFocus, "Input focus on headers"},
    {"sendmessage-focus-on-body", NULL, NULL, NULL, NULL, 0, BSSM_BodyFocus, "Input focus on body of outgoing message"},
    {"sendmessage-send-bug-report", NULL, 0, "Other~42,Compose Bug Report~99", NULL, NULL, (void (*) ()) ComposeBugReport, "Begin composing a messages/sendmessage bug report"},
    {"sendmessage-preview", NULL, 0, "Other~42,Preview Non-Andrew~77", NULL, NULL, BSSM_Preview, "Preview how a message will look to non-Andrew users"},
    {"sendmessage-special-headers", NULL, 0, "Other~42,Add Special Headers~98", NULL, NULL, (void (*) ()) AddSpecialHeaders, "Add special headers to the message"},
    {"sendmessage-clear-message", NULL, NULL, "Clear~60", 0, NULL, (void (*) ()) sendmessage__Reset, "Clear the message composition area"},
    {"sendmessage-send-message", "\030s", NULL, "Send/Post~70", NULL, NULL, (void (*) ()) sendmessage_DoDelivery, "Send/Post the current message"},
    {"sendmessage-save-draft", NULL, NULL, "Other~42,Save Draft~60", NULL, 0, (void (*) ()) sendmessage_SaveDraft, "Save the draft of a message"},
    {"sendmessage-restore-draft", NULL, NULL, "Other~42,Restore Draft~61", NULL, 0, (void (*) ()) sendmessage_RestoreDraft, "Restore the draft of a message"},
    {"sendmessage-insert-file", "\030\011", 0, "File,Insert File~35", NULL, 0, (void (*) ()) sendmessage_InsertFile, "Insert a file in the message header or body area"},

    {"sendmessage-beginning-of-header", "\001", NULL, NULL, NULL, NULL, BeginLine, "control-a function", NULL},
    {"sendmessage-next-line", "\016", NULL, NULL, NULL, NULL, NextLine, "control-n function", NULL},
    {"sendmessage-previous-header-line", "\020", NULL, NULL, NULL, NULL, PreviousLine, "control-p function", NULL},

    /* These are just setting up some proc table entries */
    {"sendmessage-folders-compound", NULL, NULL, NULL, NULL, NULL, BSSM_SendmessageFoldersCompound, "Execute a compound operation on the related folders view"},
    {"sendmessage-messages-compound", NULL, NULL, NULL, NULL, NULL, BSSM_SendmessageMessagesCompound, "Execute a compound operation on the related messages view"},
    {"sendmessage-compound-operation", NULL, NULL, NULL, NULL, NULL, BSSM_SendmessageCompound, "Execute a compound sendmessage operation"},
    {"sendmessage-set-not-modified", NULL, NULL, NULL, NULL, NULL, (void (*) ()) SetNotModified, "Mark the message composition area as 'not modified'"},
    {"sendmessage-headers-textview-compound", NULL, NULL, NULL, NULL, NULL, SBSSM_DoHeadersCommand, "Execute a compound textview command on the headers"},
    {"sendmessage-bodies-textview-compound", NULL, NULL, NULL, NULL, NULL, SBSSM_DoBodiesCommand, "Execute a compound textview command on the body"},
    {"textview-compound", NULL, NULL, NULL, NULL, NULL, SBSSM_TextviewCompound, "Execute a compound textview operation"},

    {NULL, NULL, NULL, "Region,ProgramExample", NULL, NULL, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Region,Enumerate", NULL, NULL, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Region,Itemize", NULL, NULL, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Region,Display", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Region,FormatNote", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Region,Indent", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,MajorHeading", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Heading", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Subheading", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Chapter", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Section", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Subsection", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, "Title,Paragraph", NULL, SMMASK_FEWSTYLES, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}
};


OneTimeProcInit(c)
struct classheader *c;
{
    struct proctable_Entry *tempProc;

    class_Load("textview");            /* make sure the textview is loaded
                                        * first */
    if ((tempProc = proctable_Lookup("textview-insert-file")) != NULL) {
        textv_InsertFileCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-end-of-line")) != NULL) {
        textv_EndOfLineCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-open-line")) != NULL) {
        textv_OpenLineCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-insert-newline")) != NULL) {
        textv_InsertNewlineCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-beginning-of-line")) != NULL) {
        textv_BeginningOfLineCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-next-line")) != NULL) {
        textv_NextLineCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-plainest")) != NULL) {
        textv_PlainestCmd = proctable_GetFunction(tempProc);
    }
    if ((tempProc = proctable_Lookup("textview-previous-line")) != NULL) {
        textv_PreviousLineCmd = proctable_GetFunction(tempProc);
    }
    smkm = (struct keymap *)class_NewObject("keymap"); /*keymap_New(); */
    sm_menulist = menulist_New();
    bind_BindList(smbindings, smkm, sm_menulist, c);
    smhkm = (struct keymap *)class_NewObject("keymap"); /*keymap_New(); */
    sm_hmenulist = menulist_New();
    bind_BindList(smheadbindings, smhkm, sm_hmenulist, c);
    return (TRUE);
}

InitProcStuff(sendmessage)
struct sendmessage *sendmessage;
{
    sendmessage->keys = keystate_Create(sendmessage, smkm);
    sendmessage->headkeys = keystate_Create(sendmessage, smhkm);
    sendmessage->mymenulist = menulist_DuplicateML(sm_menulist, sendmessage);
    sendmessage->myheadmenulist = menulist_DuplicateML(sm_hmenulist, sendmessage);
}

DestroyProcStuff(self)
struct sendmessage *self;
{
    keystate_Destroy(self->keys);
    menulist_Destroy(self->mymenulist);
}

void            sendmessage__PostKeyState(self, ks)
struct sendmessage *self;
struct keystate *ks;
{
    self->keys->next = NULL;
    if(ks) keystate_AddBefore(self->keys, ks);
    if (im_GetInputFocus(sendmessage_GetIM(self)) == (struct view *) self->HeadTextview) {
	self->headkeys->next = NULL;
	keystate_AddBefore(self->headkeys, self->keys);
	super_PostKeyState(self, self->headkeys);
    }
    else {
	super_PostKeyState(self, self->keys);
    }
}

void            sendmessage__PostMenus(self, menulist)
struct sendmessage *self;
struct menulist *menulist;
{
    long            newmask;

    menulist_ClearChain(self->mymenulist);
    newmask = ((self->folders) ? SMMASK_HASMESS : 0)
          | ((amsutil_GetOptBit(EXP_FILEINTO)) ? SMMASK_FILEINTO : 0)
          | ((amsutil_GetOptBit(EXP_FORCESEND)) ? SMMASK_FORCESEND : 0)
          | ((amsutil_GetOptBit(EXP_CHECKRECIP)) ? SMMASK_CHECKRECIP : 0)
          | ((amsutil_GetOptBit(EXP_INSERTHEADER)) ? SMMASK_INSERTHEAD : 0)
          | ((amsutil_GetOptBit(EXP_BIGSTYLES)) ? 0 : SMMASK_FEWSTYLES);
    menulist_SetMask(self->mymenulist, newmask);
    menulist_SetMask(self->myheadmenulist, newmask);
    if (menulist != self->mymenulist) {
        if (menulist)
            menulist_ChainAfterML(self->mymenulist, menulist, menulist);
    }
    if (im_GetInputFocus(sendmessage_GetIM(self)) == (struct view *) self->HeadTextview) {
        menulist_ChainAfterML(self->mymenulist, self->myheadmenulist, self->myheadmenulist);
    }
    super_PostMenus(self, self->mymenulist);
}

InitStylesAndFonts(sendmessage)
struct sendmessage *sendmessage;
{
    int             fontsize = environ_GetProfileInt("messages.fontsize", 12);
    char           *fontname = amsutil_GetDefaultFontName();

    sendmessage->DefaultStyle = style_New();
    style_SetFontFamily(sendmessage->DefaultStyle, fontname);
    style_SetFontSize(sendmessage->DefaultStyle, style_ConstantFontSize, fontsize);
    style_SetJustification(sendmessage->DefaultStyle, style_LeftJustified);
    style_SetNewLeftMargin(sendmessage->DefaultStyle, style_ConstantMargin, 25, style_RawDots);
    style_SetNewRightMargin(sendmessage->DefaultStyle, style_ConstantMargin, 25, style_RawDots);

    sendmessage->DefaultHeadStyle = style_New();
    style_SetFontFamily(sendmessage->DefaultHeadStyle, fontname);
    style_SetFontSize(sendmessage->DefaultHeadStyle, style_ConstantFontSize, fontsize);
    style_SetJustification(sendmessage->DefaultHeadStyle, style_LeftJustified);
    style_SetNewLeftMargin(sendmessage->DefaultHeadStyle, style_ConstantMargin, 25, style_RawDots);
    style_SetNewRightMargin(sendmessage->DefaultHeadStyle, style_ConstantMargin, 25, style_RawDots);
    style_SetNewIndentation(sendmessage->DefaultHeadStyle, style_LeftMargin, -20, style_RawDots);

    text_SetGlobalStyle(sendmessage->BodyText, sendmessage->DefaultStyle);
    text_SetGlobalStyle(sendmessage->HeadText, sendmessage->DefaultHeadStyle);

    sendmessage->BoldStyle = style_New();
    style_SetName(sendmessage->BoldStyle, "Bold");
    style_AddNewFontFace(sendmessage->BoldStyle, (long) fontdesc_Bold);
    style_SetFontFamily(sendmessage->BoldStyle, fontname);

/*    tmpfontdesc = fontdesc_Create("andy", fontdesc_Bold, 10);
    sendmessage_SetButtonFont(sendmessage, tmpfontdesc); */
}

DestroyStyles(self)
struct sendmessage *self;
{
    style_Destroy(self->DefaultStyle);
    style_Destroy(self->BoldStyle);
    style_Destroy(self->DefaultHeadStyle);
}

void            sendmessage__QuoteBody(self)
struct sendmessage *self;
{
    char            TempFile[1 + MAXPATHLEN], CaptBuf[AMS_CAPTIONSIZE + 1], *mycaps, BBName[1 + MAXPATHLEN];
    int             pos, len, orgpos, orglen, endpt, caplen;
    struct text    *t;
    struct style   *qss, *bss;

#define XCERPT "Excerpts from "

    if (!self->folders) {
        message_DisplayString(self, 10, "There is no associated messages object.");
        return;
    }
    ams_WaitCursor(TRUE);
    if (folders_WriteFormattedBodyFile(self->folders, TempFile, CaptBuf)) {
        message_DisplayString(self, 10, "Could not write out body to temp file.");
        ams_WaitCursor(FALSE);
        return;
    }
    t = self->BodyText;
    orgpos = pos = textview_GetDotPosition(self->BodyTextview);
    orglen = text_GetLength(t);
    amsutil_ReduceWhiteSpace(CaptBuf);
    mycaps = amsutil_StripWhiteEnds(CaptBuf);
    len = strlen(mycaps);
    text_InsertCharacters(t, pos, XCERPT, sizeof(XCERPT) - 1);
    pos += sizeof(XCERPT) - 1;
    folders_FolderOnDisplay(self->folders, BBName, NULL);
    if (BBName[0]) {
        text_InsertCharacters(t, pos, BBName, strlen(BBName));
        pos += strlen(BBName);
    }
    text_InsertCharacters(t, pos, ": ", 2);
    pos += 2;
    text_InsertCharacters(t, pos, mycaps, len);
    pos += len;
    caplen = pos - orgpos;
    text_InsertCharacters(t, pos, "\n\n", 2);
    pos += 2;
    text_AlwaysInsertFile(t, NULL, TempFile, pos);
    len = text_GetLength(t) - orglen;
    endpt = orgpos + len;
    if (text_GetChar(t, endpt) != '\n') {
        text_InsertCharacters(t, endpt++, "\n\n", 2);
    }
    else
        if (text_GetChar(t, endpt) != '\n') {
            text_InsertCharacters(t, endpt, "\n", 1);
        }
        else {
            --endpt;
        }

    /*
     * Note that endpt is actually one prior to the last character inserted.
     * This makes the styles work better below.
     */
    qss = stylesheet_Find(t->styleSheet, "quotation");
    bss = stylesheet_Find(t->styleSheet, "excerptedcaption");
    if (bss) {
        environment_WrapStyle(t->rootEnvironment, orgpos, caplen, bss);
    }
    if (qss) {
        int             envstart = -1;
        char            c;
        struct environment *et;

        for (pos = orgpos + caplen + 1; pos < endpt; ++pos) {
            c = text_GetChar(t, pos);
            if (c == '\n') {
                if (envstart >= 0) {
                    et = environment_WrapStyle(t->rootEnvironment, envstart, pos - envstart + 1, qss);
                    environment_SetStyle(et, FALSE, FALSE);
                    envstart = -1;
                }
            }
            else {
                if (envstart < 0) {
                    envstart = pos;
                }
            }
        }
        if (envstart >= 0) {
            et = environment_WrapStyle(t->rootEnvironment, envstart, pos - envstart + 1, qss);
            environment_SetStyle(et, FALSE, FALSE);
        }
    }
    else {
        message_DisplayString(self, 10, "There is no quotation style in the stylesheet!");
    }
    unlink(TempFile);
    textview_WantUpdate(self->BodyTextview, self->BodyTextview);
    ams_WaitCursor(FALSE);
}

void            UserWantsAHeader(self, head)
struct sendmessage *self;
char           *head;
{
    char            newheader[150], *s;
    struct textview *hv;
    struct im      *myim = sendmessage_GetIM(self);

    hv = self->HeadTextview;
    if (head || myim) {
	if(im_GetInputFocus(myim) != (struct view *) hv)
	    sendmessage_WantInputFocus(self,(struct view*) hv);
        if (head && *head != '?') {
            strcpy(newheader, head);
        }
        else {
            if (message_AskForString(hv, 50, "Enter the name of the header you want to add: ", head ? ++head : "", newheader, sizeof(newheader)) < 0)
                return;
        }
        if (newheader[0] == '\0')
            return;
        s = strchr(newheader, ':');
        if (!s)
            strcat(newheader, ": ");
        sendmessage_AddHeaderLine(self, newheader);
        textview_SetDotLength(hv, 0);
        textview_WantUpdate(hv, hv);
        message_DisplayString(hv, 10, "Done.");
    }
    else {
        textv_OpenLineCmd(self->BodyTextview);
    }
}

void            BeginLine(sm)
struct sendmessage *sm;
{
    struct textview *v;
    int             dot, len;
    char            c;
    struct im      *myim = sendmessage_GetIM(sm);

    v = sm->HeadTextview;
    if (myim && (im_GetInputFocus(myim) == (struct view *) v)) {
        textv_BeginningOfLineCmd(sm->HeadTextview);
        len = text_GetLength(sm->HeadText);
        dot = textview_GetDotPosition(v);
        c = text_GetChar(sm->HeadText, dot);
        if ((c == ' ') || (c == '\t')) {
            ++dot;
        }
        else {
            while ((c != ':') && (c != '\n') && (dot <= len)) {
                c = text_GetChar(sm->HeadText, ++dot);
            }
            c = text_GetChar(sm->HeadText, ++dot);
            while ((c == ' ') || (c == '\t') && (dot <= len)) {
                c = text_GetChar(sm->HeadText, ++dot);
            }
        }
        textview_SetDotPosition(v, dot);
        textview_WantUpdate(v, v);
    }
    else {
        textv_BeginningOfLineCmd(sm->BodyTextview);
    }
}

void            NextLine(sm, IsNewline)
struct sendmessage *sm;
int             IsNewline;
{
    struct textview *v;
    int             dot;
    struct im      *myim = sendmessage_GetIM(sm);

    v = sm->HeadTextview;
    if (myim && (im_GetInputFocus(myim) == (struct view *) v)) {
        dot = textview_GetDotPosition(v);
        textv_NextLineCmd(v);
        textv_EndOfLineCmd(v);
        if (dot == textview_GetDotPosition(v)) {
            textview_WantInputFocus(sm->BodyTextview, sm->BodyTextview);
        }
        else {
            textview_WantUpdate(v, v);
        }
    }
    else {
        if (IsNewline) {
            textv_InsertNewlineCmd(sm->BodyTextview);
        }
        else {
            textv_NextLineCmd(sm->BodyTextview);
        }
    }
}

void            PreviousLine(sm)
struct sendmessage *sm;
{
    int             dot;
    struct textview *v;
    struct im      *myim = sendmessage_GetIM(sm);

    v = sm->HeadTextview;
    if (myim && (im_GetInputFocus(myim) == (struct view *) v)) {
        textv_PreviousLineCmd(v);
        textv_EndOfLineCmd(v);
        textview_WantUpdate(v, v);
    }
    else {
        v = sm->BodyTextview;
        dot = textview_GetDotPosition(v);
        textv_PreviousLineCmd(v);
        if (dot == textview_GetDotPosition(v)) {
            textview_SetDotPosition(sm->HeadTextview, text_GetLength(sm->HeadText));
            textview_WantInputFocus(sm->HeadTextview, sm->HeadTextview);
        }
        else {
            textview_WantUpdate(v, v);
        }
    }
}

void            sendmessage_InsertFile(sendmessage, fname)
struct sendmessage *sendmessage;
char           *fname;
{
    struct textview *tv;
    struct text    *t;
    struct im      *myim = sendmessage_GetIM(sendmessage);

    if (myim && (im_GetInputFocus(myim) == (struct view *) sendmessage->HeadTextview)) {
        tv = sendmessage->HeadTextview;
        t = sendmessage->HeadText;
    }
    else {
        tv = sendmessage->BodyTextview;
        t = sendmessage->BodyText;
    }
    if (fname) {
        int             pos;

        pos = textview_GetDotPosition(tv);
        DirectlyInsertFile(tv, t, fname, pos);
    }
    else {
        textv_InsertFileCmd(tv);
    }
}

PrepareBodyForSignature(self)
struct sendmessage *self;
{
    struct text    *t = self->BodyText;
    struct textview *tv = self->BodyTextview;
    int             len = text_GetLength(t);
    char            c = text_GetChar(t, len - 1);
    int             dotpos = textview_GetDotPosition(tv);
    int             dotlen = textview_GetDotLength(tv);

    if (c != '\n') {
        text_AlwaysInsertCharacters(t, len, "\n", 1);
        ++len;
    }
    textview_SetDotPosition(tv, len - 1);
    textview_SetDotLength(tv, 1);
    textv_PlainestCmd(tv);
    textview_SetDotPosition(tv, dotpos);
    textview_SetDotLength(tv, dotlen);
}

SetMyFrameTitle(sm, tit)
struct sendmessage *sm;
char           *tit;
{
    frame_SetTitle(sm->myframe, tit);
}

static boolean  AddIfView(env, ct)
struct environment *env;
int            *ct;
{
    if (env) {
        *ct += EnvViewCt(env);
        if (env->type == environment_View)
            ++(*ct);
    }
    return (FALSE);
}

EnvViewCt(env)
struct environment *env;
{
    int             total = 0;
    struct nestedmark *nm;

    nm = (struct nestedmark *) env;
    if (nm->children) {
        tree23int_Enumerate(nm->children, AddIfView, &total);
    }
    return (total);
}
