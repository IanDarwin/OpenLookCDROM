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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/chzdirs.c,v 2.25 1993/08/25 20:35:51 susan Exp $";
#endif

/*
        chzdirs.c -- Routines for deciding in which directories to put mail

        This is some of the ugliest code in the message server.  It doesn't
                really need to be quite as ugly as it is, but the subtleties
                would make it moderately ugly in any event.

        Good luck reading it.

*/

#include <andrewos.h>
#include <ms.h>
#include <util.h>
#include <mailconf.h>
#include <hdrparse.h>
#include <stdio.h>
#include <pwd.h>
#include <sys/stat.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif                                 /* WHITEPAGES_ENV */
#include <mail.h>
#include <dropoff.h>

extern char    *DayArray[], *MonArray[];        /* Just the names of days and
                                                 * months */
extern char    *pop();
extern char    *StripWhiteEnds(), *GenAuthField();
extern FILE    *qopen();
extern char     Me[], *MyPrettyAddress, home[], MyMailDomain[];

#define StackAbort(err) AMS_RETURN_ERRCODE(EMSBADDIRSPEC, err, EVIA_CHOOSEDIRECTORIES)
#define CheckPoppability(x) if (StackSize(x) <= 0) {StackAbort(EIN_POP);}
#define PARSESTACK 0
#define NESTSTACK 1

int             RejectMessage(Msg, Text, RejectionsTo, RejectCC)
struct MS_Message *Msg;
char           *Text, *RejectionsTo, *RejectCC;
{
    FILE           *fp;
    char            RejTo[1500], BBM[500], TempFile[1 + MAXPATHLEN], *Cmd[3], ThisFormat[50], BigBuf[5000];
    int             i, errsave, code, IsBE2Format, SkippedBytes, bytesleft, bytestoread;

    if (Msg->ReplyTo == NULL) {
        BuildReplyField(Msg);
    }
    if (RejectionsTo && *RejectionsTo) {
        if (*RejectionsTo == '/') {
            struct MS_Directory *Dir;
            char           *MyHead, NewFileName[1 + MAXPATHLEN];

            MyHead = malloc(110 + strlen(MyPrettyAddress) + strlen(Text));
            if (!MyHead) {
                AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHOOSEDIRECTORIES);
            }
            sprintf(MyHead, "X-Rejection-Message: Message Server for %s rejected this message for the following reason:\n\t%s", MyPrettyAddress, Text);
            AddHeader(Msg, MyHead);
            if (ReadOrFindMSDir(RejectionsTo, &Dir, MD_APPEND)) {
                return (mserrcode);
            }
            if (BuildAttributesField(Msg)
                || BuildDateField(Msg, DATETYPE_FROMHEADER)
                || InventID(Msg)
                || BuildCaption(Msg, NULL, FALSE)) {
                errsave = mserrcode;
                CloseMSDir(Dir, MD_APPEND);
                mserrcode = errsave;
                return (mserrcode);
            }

            if (IsMessageAlreadyThere(Msg, Dir)) {
                CloseMSDir(Dir, MD_APPEND);
                return (0);
            }

            sprintf(NewFileName, "%s/+%s", Dir->UNIXDir, AMS_ID(Msg->Snapshot));
            if (WritePureFile(Msg, NewFileName, FALSE, 0664)) {
                errsave = mserrcode;
                CloseMSDir(Dir, MD_APPEND);
                return (mserrcode = errsave);
            }

            if (AppendMessageToMSDir(Msg, Dir)) {
                errsave = mserrcode;
                CloseMSDir(Dir, MD_APPEND);
                return (mserrcode = errsave);
            }

            if (CloseMSDir(Dir, MD_APPEND)) {
                return (mserrcode);
            }
            return (0);
        }
        Cmd[0] = RejectionsTo;
    }
    else {
        if (Msg->ParsedStuff->HeadBody[HP_RETURNPATH]) {
            strcpy(RejTo, "Message sender ");   /* for obscure RFC822 reasons */
            i = Msg->ParsedStuff->HeadBodyLen[HP_RETURNPATH];
            strncat(RejTo, Msg->ParsedStuff->HeadBody[HP_RETURNPATH], i);
            Cmd[0] = RejTo;
        }
        else {
            Cmd[0] = Msg->ReplyTo;
        }
    }
    if (RejectCC && *RejectCC) {
        Cmd[1] = RejectCC;
    }
    else {
        if (RejectCC) {
            sprintf(BBM, "BBoard.Maintainer@%s", MyMailDomain);
            Cmd[1] = BBM;
        }
        else
            Cmd[1] = NULL;
    }
    Cmd[2] = NULL;
    GenTempName(TempFile);
    if ((fp = fopen(TempFile, "w")) == NULL) {
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_REJECTMESSAGE);
    }
    GetFormatFromMessage(Msg, ThisFormat, sizeof(ThisFormat), &IsBE2Format);
    if (IsBE2Format) {
        fprintf(fp, "Content-Type: X-BE2; %s\nIf-Type-Unsupported: alter\n", ThisFormat);
    }
    fprintf(fp, "Date: %sFrom: Message Server for %s\nTo: %s\nSubject: Rejected Message\n", arpadate(), MyPrettyAddress, Cmd[0]);
    if (Cmd[1])
        fprintf(fp, "CC: %s\n\n", Cmd[1]);
    else
        fprintf(fp, "\n");
    if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET) < 0) {
        fclose(fp);
        unlink(TempFile);
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_REJECTMESSAGE);
    }
    if (IsBE2Format && (atoi(ThisFormat) >= 10)) {
        EmitBE2PrefixAndLSeekPastIt(Msg->OpenFD, fp, &SkippedBytes);
    }
    else {
        SkippedBytes = 0;
    }
    fprintf(fp, "The following message was delivered properly, but was automatically rejected\nby the recipient's message server for the following reason: \n\n>>>");
    fflush(fp);
    PrintQuotingFormatting(fp, Text, ThisFormat, strlen(Text));
    fflush(fp);
    fprintf(fp, " <<<\n\n-- The rejected message is shown below --\n\n");
    PrintQuotingFormatting(fp, Msg->RawBits, ThisFormat, Msg->HeadSize);
    bytesleft = Msg->FullSize - Msg->HeadSize - SkippedBytes;
    while (bytesleft > 0) {
        bytestoread = (bytesleft > (sizeof(BigBuf) - 1)) ? (sizeof(BigBuf) - 1) : bytesleft;
        if (read(Msg->OpenFD, BigBuf, bytestoread) != bytestoread) {
            fclose(fp);
            unlink(TempFile);
            AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_REJECTMESSAGE);
        }
        if (fwriteallchars(BigBuf, bytestoread, fp) != bytestoread) {
            fclose(fp);
            unlink(TempFile);
            AMS_RETURN_ERRCODE(errno, EIN_FWRITE, EVIA_REJECTMESSAGE);
        }
        bytesleft -= bytestoread;
    }
    errno = 0;
    if (ferror(fp) || feof(fp)) {
        fclose(fp);
        AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_REJECTMESSAGE);
    }
    if (vfclose(fp) != 0) {
        AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_REJECTMESSAGE);
    }
    code = dropoff(Cmd, TempFile, "<>", home, 0);
    unlink(TempFile);
    code = ConvertDropoffCode(code);
    if (code) {
        AMS_RETURN_ERRCODE(code, EIN_DROPOFF, EVIA_REJECTMESSAGE);
    }
    return (0);
}

static int      ConvertDropoffCode(code)
int             code;
{
    switch (code) {
        case D_OK:
        case D_OK_WARN:
        case D_LOCALQ:
            return (0);
        case D_CANT_QUEUE:
            return (EMSDROPOFFNOQUEUE);
        case D_BAD_PARMS:
            return (EMSDROPOFFBADPARMS);
        case D_TEMP_FAIL:
            return (EMSDROPOFFTEMPFAIL);
        case D_BAD_MESGFILE:
            return (EMSDROPOFFBADMSGFILE);
        case D_OSERR:
            return (EMSDROPOFFOSERR);
        default:
            return (EMSUNKNOWN);
    }
}

int             ResendMessageFromMailbox(Msg, Addressee, AddResendHeads)
struct MS_Message *Msg;
char           *Addressee;
Boolean         AddResendHeads;
{
    FILE           *fp;
    char            TempFile[1 + MAXPATHLEN], *retpath = NULL, *Cmd[2], BigBuf[5000], *authfield;
    int             savepos, code, bytesleft, bytestoread;

    GenTempName(TempFile);
    while (Msg->ParsedStuff->HeadBody[HP_AUTHENTICATED_AS]) {
        DeleteHeader(Msg, HP_AUTHENTICATED_AS);
    }
    while (Msg->ParsedStuff->HeadBody[HP_RECEIVED]) {

        /*
         * Remove all old Received headers - prevent loops and "too many hops"
         */
        DeleteHeader(Msg, HP_RECEIVED);
    }
    if ((fp = fopen(TempFile, "w")) == NULL) {
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RESENDMESSAGE);
    }
    if (AddResendHeads) {
        /* BOGUS: The call to newmid() in the following comes from submsg.c! */
        fprintf(fp, "ReSent-Message-ID: %s\nReSent-Date: %sReSent-From: %s\nReSent-To: %s\n", newmid(), arpadate(), MyPrettyAddress, Addressee);
    }
    if (fwriteallchars(Msg->RawBits, Msg->HeadSize, fp) != Msg->HeadSize) {
        fclose(fp);
        unlink(TempFile);
        AMS_RETURN_ERRCODE(errno, EIN_FWRITE, EVIA_RESENDMESSAGE);
    }
    bytesleft = Msg->FullSize - Msg->HeadSize;
    if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET) < 0) {
        fclose(fp);
        unlink(TempFile);
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_RESENDMESSAGE);
    }
    while (bytesleft > 0) {
        bytestoread = (bytesleft > (sizeof(BigBuf) - 1)) ? (sizeof(BigBuf) - 1) : bytesleft;
        if (read(Msg->OpenFD, BigBuf, bytestoread) != bytestoread) {
            fclose(fp);
            unlink(TempFile);
            AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_RESENDMESSAGE);
        }
        if (fwriteallchars(BigBuf, bytestoread, fp) != bytestoread) {
            fclose(fp);
            unlink(TempFile);
            AMS_RETURN_ERRCODE(errno, EIN_FWRITE, EVIA_RESENDMESSAGE);
        }
        bytesleft -= bytestoread;
    }
    if (vfclose(fp) != 0) {
        unlink(TempFile);
        AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_RESENDMESSAGE);
    }
    if (!AddResendHeads) {
        retpath = Msg->ParsedStuff->HeadBody[HP_RETURNPATH];
        if (retpath) {
            savepos = Msg->ParsedStuff->HeadBodyLen[HP_RETURNPATH];
            retpath[savepos] = '\0';
        }
    }
    Cmd[0] = Addressee;
    Cmd[1] = NULL;
    authfield = GenAuthField(Msg);
    if (!authfield) {
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_RESENDMESSAGE);
    }
    code = dropoff_auth(Cmd, TempFile, retpath, home, 0, authfield);
    free(authfield);
    unlink(TempFile);
    code = ConvertDropoffCode(code);
    if (code) {
        AMS_RETURN_ERRCODE(code, EIN_DROPOFF, EVIA_RESENDMESSAGE);
    }
    return (0);
}

