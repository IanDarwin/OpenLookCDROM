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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/submsg.c,v 2.25 1993/08/25 20:36:16 susan Exp $";
#endif

#include <andrewos.h>		/* sys/file.h */
#include <ms.h>
#include <stdio.h>
#include <ctype.h>
#include <hdrparse.h>
#include <dropoff.h>
#include <util.h>
#include <mailconf.h>
#include <parseadd.h>
#include <mail.h>

extern FILE    *fopen();
extern char     MAILLOGSTATFILE[], MyMailDomain[];


extern char     Me[], *MyPrettyAddress, *BCCto, home[], *NextAddress();
extern int      BCCFlag, IsLoggingMailStats, myvuid, errno, MS_DeliveryType;

static char     ThisHost[150] = "";


char           *newmid()
{
    static char     MidText[300];
    char           *IdBuf, *s;

    IdBuf = ams_genid(0);
    for (s = IdBuf; *s; ++s) {
        if (*s == ':')
            *s = '_';
    }
    sprintf(MidText, "<%s@%s>", IdBuf, MyMailDomain);
    return (MidText);
}

static char    *newreceived(ClientVersion)
char           *ClientVersion;
{
    static char     RecText[500];

    if (ThisHost[0] == '\0') {
        GetHostDomainName(ThisHost, sizeof(ThisHost));
        if (ThisHost[0] == '\0') {
            strcpy(ThisHost, "unknown-host");
        }
    }
#ifdef SYS_NAME
    sprintf(RecText, "Received: from %s\n          via MS.%d.%d.%s.%s;\n          %s", ClientVersion, MS_MAJORVERSION, MS_MINORVERSION, ThisHost, SYS_NAME, arpadate());
#else                                  /* SYS_NAME */
    sprintf(RecText, "Received: from %s\n          via MS.%d.%d.%s.%s;\n          %s", ClientVersion, MS_MAJORVERSION, MS_MINORVERSION, ThisHost, "unknown.CPU", arpadate());
#endif                                 /* SYS_NAME */
    return (RecText);
}

static char     LastGoodDeliveryFileName[1 + MAXPATHLEN] = "";

static          AddNamesToVector(pVec, index, maxindex, namelist, len)
char         ***pVec;
int            *index, *maxindex, len;
char           *namelist;
{
    char          **Vec, *onerecip, TempBuf[4000], SaveChar;
    PARSED_ADDRESS *AddrList;
    ADDRESS_COMMENT *ThisComm, *NextComm;
    int             newSize, seq;
    char          **newVec;

    if (!namelist || !*namelist || len == 0)
	return (0);
    debug(1, ("Adding names from list %s\n", namelist));
    SaveChar = namelist[len];
    namelist[len] = '\0';
    if (ParseAddressList(namelist, &AddrList) != PA_OK) {
	namelist[len] = SaveChar;
	AMS_RETURN_ERRCODE(EFAULT, EIN_PARSEADDR, EVIA_SUBMITMESSAGE);
    }
    namelist[len] = SaveChar;
    FOR_ALL_ADDRESSES(TempAddr, AddrList, {
        /* remove comments & phrase, unparse into onerecip */
        ThisComm = TempAddr->Comments;
        while (ThisComm) {
            NextComm = ThisComm->Next;
            if (ThisComm->Text)
                free(ThisComm->Text);
            free(ThisComm);
            ThisComm = NextComm;
        }
        TempAddr->Comments = NULL;
        if (TempAddr->RoutePhrase) {
            free(TempAddr->RoutePhrase);
            (TempAddr->RoutePhrase) = NULL;
        }
        if (UnparseOneAddress(TempAddr, UP_SPACES_TO_DOTS, TempBuf, sizeof(TempBuf), "", "    ", 69) != PA_OK) {
            FreeAddressList(AddrList);
            AMS_RETURN_ERRCODE(errno, EIN_UNPARSEADDR, EVIA_SUBMITMESSAGE);
        }
        onerecip = malloc(1 + strlen(TempBuf));
        if (!onerecip) {
            FreeAddressList(AddrList);
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBMITMESSAGE);
        }
        strcpy(onerecip, TempBuf);
        debug(1, ("Adding %s to submission vector\n", onerecip));
        if (*onerecip != '\0') {
            if ((*index) + 3 >= (*maxindex)) {
                newSize = 2 * (*index + 3) + 10;
                newVec = (char **) malloc(newSize * sizeof(char *));
                if (newVec == NULL) {
                    FreeAddressList(AddrList);
                    return (-1);
                }
                if (*pVec != NULL) {
                    Vec = *pVec;
                    for (seq = (*index) - 1; seq >= 0; --seq) {
                        newVec[seq] = Vec[seq];
                    }
                    free(Vec);
                }
                *pVec = newVec;
                *maxindex = newSize;
            }
            Vec = *pVec;
            Vec[(*index)++] = onerecip;
        }
    })
    FreeAddressList(AddrList);
    return (0);
}

MS_SubmitMessage(FileName, DeliveryOptions, ErrorMessage, ErrMsgLimit, ClientProgram)
char           *FileName;              /* Passed in */
int             DeliveryOptions;       /* Passed in */
char           *ErrorMessage;          /* Passed out */
int             ErrMsgLimit;           /* Passed in */
char           *ClientProgram;         /* Passed in */
{
    struct MS_Message *Msg;
    int             which = 0, maxwhich = 0, badparse = 0, linelen = 0, sawbadchar = 0, longlines = 0, code, errcode, bytesleft, bytestoread;
    FILE           *fp;
    char           *s, dropfile[1 + MAXPATHLEN], NewHeads[1000], BigBuf[5000], **SubmitVector = NULL;
    extern char     Dropoff_ErrMsg[];
    Boolean         KeptDirectBlind = FALSE;

    debug(1, ("Submitting message from file %s options %d\n", FileName, DeliveryOptions));
    if (MS_DeliveryType == 1) {
        AMS_RETURN_ERRCODE(EMSNODELIVERY, EIN_PARAMCHECK, EVIA_SUBMITMESSAGE);
    }
    strcpy(ErrorMessage, "unknown submission failure");
    if (ClientProgram) {
        for (s = ClientProgram; *s; ++s) {
            if (!isalnum(*s))
                *s = '.';
        }
    }
    if (!strcmp(FileName, LastGoodDeliveryFileName))
        return (0);                    /* was a retry */
    if (DeliveryOptions & AMS_SEND_DO_NOT_USE_THIS) {
        /* Backward compatibility, ugh... */
        DeliveryOptions = BCCFlag ? AMS_SEND_BLINDYES : 0;
    }
    if ((Msg = (struct MS_Message *) malloc(sizeof(struct MS_Message))) == NULL) {
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBMITMESSAGE);
    }
    bzero(Msg, sizeof(struct MS_Message));
    Msg->OpenFD = -1;
    if (ReadRawFile(FileName, Msg, FALSE)
        || ParseMessageFromRawBody(Msg)) {
        debug(4, ("Bad submission file\n"));
        FreeMessage(Msg, TRUE);
        return (mserrcode);            /* Already set */
    }

    /*
     * Don't let users forge ANYTHING.  The while loops are in case they try
     * multiple forged headers
     */
    if (DeliveryOptions & AMS_SEND_ISRESEND) {
        while (Msg->ParsedStuff->HeadBody[HP_RECEIVED]) {

            /*
             * Remove all old Received headers - prevent loops and too many
             * hops hopefully
             */
            DeleteHeader(Msg, HP_RECEIVED);
        }
        while (Msg->ParsedStuff->HeadBody[HP_REDISTRIBUTION]) {

            /*
             * Remove all old redistribution headers - prevent loops if
             * already redistributing (that's why we only do it on a resend
             */
            DeleteHeader(Msg, HP_REDISTRIBUTION);
        }
    }
    else {
        while (Msg->ParsedStuff->HeadBody[HP_FROM]) {
            NonfatalBizarreError("Deleting a user-supplied 'From' header line.");
            DeleteHeader(Msg, HP_FROM);
        }
        while (Msg->ParsedStuff->HeadBody[HP_SENDER]) {
            NonfatalBizarreError("Deleting a user-supplied 'Sender' header line.");
            DeleteHeader(Msg, HP_SENDER);
        }
    }

    /*
     * Even on a resend, we delete the authentication headers, but we don't
     * mention it.
     */
    while (Msg->ParsedStuff->HeadBody[HP_AUTHENTICATED_AS]) {
        if (!(DeliveryOptions & AMS_SEND_ISRESEND)) {
            NonfatalBizarreError("Deleting a user-supplied authentication header line.");
        }
        DeleteHeader(Msg, HP_AUTHENTICATED_AS);
    }
    if (DeliveryOptions & AMS_SEND_UNFORMATTED) {
        if (UnformatMessage(Msg)) {
            FreeMessage(Msg, TRUE);
            return (mserrcode);
        }
    }
    if (DeliveryOptions & AMS_SEND_INSISTFORMATTED) {
        if (Msg->ParsedStuff->HeadBody[HP_UNSUPPORTEDTYPE]) {
            DeleteHeader(Msg, HP_UNSUPPORTEDTYPE);
        }
        AddHeader(Msg, "If-Type-Unsupported: send");
    }
    if (DeliveryOptions & AMS_SEND_INSISTTRUST) {
        if (Msg->ParsedStuff->HeadBody[HP_UNSUPPORTEDTYPE]) {
            DeleteHeader(Msg, HP_UNSUPPORTEDTYPE);
        }
        AddHeader(Msg, "If-Type-Unsupported: alter");
    }
    if (!(DeliveryOptions & AMS_SEND_ILLEGAL)) {
        char            c;
        int             result;

        if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET) < 0) {
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_SUBMITMESSAGE);
        }
        while (result = read(Msg->OpenFD, &c, 1)) {
            if (!c || !isascii(c)) {
                ++sawbadchar;
                break;
            }
            if (c == '\n') {
                linelen = 0;
            }
            else
                if (++linelen > 200) {
                    ++longlines;
                    break;
                }
        }
        if (result < 0) {
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_SUBMITMESSAGE);
        }
        if (sawbadchar) {
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(EMSNONASCIIMAIL, EIN_PARAMCHECK, EVIA_SUBMITMESSAGE);
        }
        if (longlines) {
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(EMSLONGLINES, EIN_PARAMCHECK, EVIA_SUBMITMESSAGE);
        }
    }
    if (DeliveryOptions & AMS_SEND_ISRESEND) {
        sprintf(NewHeads, "%sReSent-Message-ID: %s\nReSent-Date: %sReSent-From: %s",
          newreceived(ClientProgram), newmid(), arpadate(), MyPrettyAddress);
    }
    else {
        sprintf(NewHeads, "%sMessage-ID: %s\nDate: %sFrom: %s",
          newreceived(ClientProgram), newmid(), arpadate(), MyPrettyAddress);
        if (Msg->ParsedStuff->HeadBody[HP_SUBJECT] == NULL) {   /* OK in resend */
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(EMSNOSUBJ, EIN_PARAMCHECK, EVIA_SUBMITMESSAGE);
        }
    }
    AddHeader(Msg, NewHeads);
    if (DeliveryOptions & AMS_SEND_ISRESEND) {
        badparse = AddNamesToVector(&SubmitVector, &which, &maxwhich, Msg->ParsedStuff->HeadBody[HP_RESENTTO], Msg->ParsedStuff->HeadBodyLen[HP_RESENTTO]);
    }
    else {
        badparse = AddNamesToVector(&SubmitVector, &which, &maxwhich, Msg->ParsedStuff->HeadBody[HP_TO], Msg->ParsedStuff->HeadBodyLen[HP_TO]);
        if (!badparse)
            badparse = AddNamesToVector(&SubmitVector, &which, &maxwhich, Msg->ParsedStuff->HeadBody[HP_CC], Msg->ParsedStuff->HeadBodyLen[HP_CC]);
    }

    if (badparse) {
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        return (badparse);
    }

    if (which <= (0) && (!(DeliveryOptions & AMS_SEND_BLINDYES))) {
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        debug(4, ("No recipients\n"));
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_SUBMITMESSAGE);
    }
    if (DeliveryOptions & AMS_SEND_BLINDYES) {
        struct MS_Directory *Dir;

        if (BCCto) {
            struct MS_CaptionTemplate Captemp;
            char            NewFileName[1 + MAXPATHLEN], *s2;

            bzero(&Captemp, sizeof(struct MS_CaptionTemplate));
            Captemp.basictype = BASICTEMPLATE_NORMAL;
            Captemp.datetype = DATETYPE_CURRENT;
            s2 = Msg->RawBits;         /* Why am I doing this?  I no longer
                                        * remember, sigh... */
            Msg->AuthUid = myvuid;
#ifdef AFS_ENV
            if (AMS_ViceIsRunning && Msg->AuthCell == NULL) {
                /* This is coming from us, really. */
                Msg->AuthCell = NewString(MyMailDomain);
            }
#endif                                 /* AFS_ENV */
            InventID(Msg);
            sprintf(NewFileName, "%s/+%s", BCCto, AMS_ID(Msg->Snapshot));
            if (!BuildAttributesField(Msg)
                && !BuildDateField(Msg, DATETYPE_CURRENT)
                && !BuildCaption(Msg, &Captemp, TRUE)
                && !ReadOrFindMSDir(BCCto, &Dir, MD_APPEND)) {
                if (!IsMessageAlreadyThere(Msg, Dir)) {
                    if (!WritePureFile(Msg, NewFileName, FALSE, 0664)) {
                        if (!AppendMessageToMSDir(Msg, Dir)) {
                            if (!CacheDirectoryForClosing(Dir, MD_APPEND)) {
                                KeptDirectBlind = TRUE;
                            }
                            else {
                                CloseMSDir(Dir, MD_APPEND);
                            }
                        }
                        else {
                            unlink(NewFileName);        /* User will get a copy
                                                         * through the mail */
                            CloseMSDir(Dir, MD_APPEND);
                        }
                    }
                    else {
                        CloseMSDir(Dir, MD_APPEND);
                    }
                }
                else {
                    CloseMSDir(Dir, MD_APPEND);
                }
            }
            Msg->RawBits = s2;
        }
        if (!KeptDirectBlind) {
            if (BCCto) {
                char            ErrorText[500];

                sprintf(ErrorText, "Sending your BCC through the mail after error in direct insertion. (%d, %d, %d)", AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
                NonfatalBizarreError(ErrorText);
            }
            if (!SubmitVector) {
                SubmitVector = (char **) malloc(sizeof(char *));
                if (!SubmitVector) {
                    FreeMessage(Msg, TRUE);
                    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBMITMESSAGE);
                }
            }
            SubmitVector[which] = malloc(1 + strlen(MyPrettyAddress));
            if (SubmitVector[which] == NULL) {
                FreeMessage(Msg, TRUE);
                FreeSubmitVector(SubmitVector, which);
                AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBMITMESSAGE);
            }
            strcpy(SubmitVector[which++], MyPrettyAddress);
        }
    }
    if (which <= (0)) {
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        strncpy(ErrorMessage, "There were no recipients specified, but a blind copy was kept.", ErrMsgLimit);
        AMS_RETURN_ERRCODE(EMSDROPOFFWARN, EIN_DROPOFF, EVIA_SUBMITMESSAGE);
    }


    SubmitVector[which] = NULL;

    GenTempName(dropfile);
    fp = fopen(dropfile, "w");
    if (!fp) {
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_SUBMITMESSAGE);
    }
#ifdef M_UNIX
    chmod(dropfile, 0600);
#else
    fchmod(fileno(fp), 0600);
#endif

    if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET) < 0) {
        unlink(dropfile);
        fclose(fp);
        FreeSubmitVector(SubmitVector, which);
        FreeMessage(Msg, TRUE);
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_SUBMITMESSAGE);
    }
    if (fwriteallchars(Msg->RawBits, Msg->HeadSize, fp) != Msg->HeadSize) {
        unlink(dropfile);
        fclose(fp);
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        AMS_RETURN_ERRCODE(errno, EIN_FWRITE, EVIA_SUBMITMESSAGE);
    }
    bytesleft = Msg->FullSize - Msg->HeadSize;
    while (bytesleft > 0) {
        bytestoread = (bytesleft > (sizeof(BigBuf) - 1)) ? (sizeof(BigBuf) - 1) : bytesleft;
        if ((read(Msg->OpenFD, BigBuf, bytestoread) != bytestoread) || (fwriteallchars(BigBuf, bytestoread, fp) != bytestoread)) {
            unlink(dropfile);
            fclose(fp);
            FreeMessage(Msg, TRUE);
            FreeSubmitVector(SubmitVector, which);
            AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_SUBMITMESSAGE);
        }
        bytesleft -= bytestoread;
    }
    if (ferror(fp) || feof(fp)) {
        int             dogs = errno;

        unlink(dropfile);
        fclose(fp);
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        AMS_RETURN_ERRCODE(dogs, EIN_FERROR, EVIA_SUBMITMESSAGE);
    }
    if (vfclose(fp)) {
        FreeMessage(Msg, TRUE);
        FreeSubmitVector(SubmitVector, which);
        AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_SUBMITMESSAGE);
    }
    code = dropoff(SubmitVector, dropfile, NULL, home, 0);
    if (IsLoggingMailStats) {
        FILE           *fp;

        fp = fopen(MAILLOGSTATFILE, "a");
        if (fp) {
#ifdef M_UNIX
	    chmod(dropfile,0600);
#else
            fchmod(fileno(fp), 0600);
#endif
            fputc('\n', fp);
            fwriteallchars(Msg->RawBits, Msg->HeadSize - 1, fp);
            fprintf(fp, "X-StatTrace: %s SENT %d bytes ; %s", Me, Msg->FullSize - Msg->HeadSize, arpadate());
            fclose(fp);
        }
    }
    unlink(dropfile);
    FreeMessage(Msg, TRUE);
    FreeSubmitVector(SubmitVector, which);

    strncpy(ErrorMessage, Dropoff_ErrMsg, ErrMsgLimit);

    switch (code) {
        case D_OK:
            strcpy(LastGoodDeliveryFileName, FileName);
            unlink(FileName);
            if ((which == 1) && !KeptDirectBlind && (DeliveryOptions & AMS_SEND_BLINDYES)) {
                strncpy(ErrorMessage, "There were no recipients specified, but a blind copy was queued.", ErrMsgLimit);
                AMS_RETURN_ERRCODE(EMSDROPOFFWARN, EIN_DROPOFF, EVIA_SUBMITMESSAGE);
            }
            return (0);
        case D_OK_WARN:
            errcode = EMSDROPOFFWARN;
            unlink(FileName);
            break;
        case D_LOCALQ:
            errcode = EMSDROPOFFLOCALQUEUE;
            unlink(FileName);
            break;
        case D_CANT_QUEUE:
            errcode = EMSDROPOFFNOQUEUE;
            break;
        case D_BAD_PARMS:
            errcode = EMSDROPOFFBADPARMS;
            break;
        case D_TEMP_FAIL:
            errcode = EMSDROPOFFTEMPFAIL;
            break;
        case D_BAD_MESGFILE:
            errcode = EMSDROPOFFBADMSGFILE;
            break;
        case D_OSERR:
            errcode = EMSDROPOFFOSERR;
            break;
        default:
            errcode = EMSUNKNOWN;
            break;
    }
    AMS_RETURN_ERRCODE(errcode, EIN_DROPOFF, EVIA_SUBMITMESSAGE);
}

FreeSubmitVector(SubmitVector, which)
char          **SubmitVector;
int             which;
{
    if (!SubmitVector)
        return;
    while (--which >= 0) {
        free(SubmitVector[which]);
    }
    free(SubmitVector);
}
