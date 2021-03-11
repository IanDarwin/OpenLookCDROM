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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/clonemsg.c,v 2.11 1992/12/15 21:18:15 rr2b R6tape $";
#endif

#include <stdio.h>
#include <ms.h>
#include <andrewos.h>                  /* sys/time.h */

extern FILE    *fopen();

MS_CloneMessage(SourceDirName, id, DestDirName, Code)
char           *SourceDirName, *id, *DestDirName;
int             Code;
{
    struct MS_Directory *SourceDir, *DestDir;
    struct MS_Message *Msg;
    char            MySnapshot[AMS_SNAPSHOTSIZE], SnapshotDum[AMS_SNAPSHOTSIZE], SourceFileName[1 + MAXPATHLEN];
    int             msgnum, errsave, SourceDirMode, PutAtEnd, msgnumdum;

    debug(1, ("MS_CloneMessage source %s obj %s id %s code %d\n", SourceDirName, DestDirName, id, Code));
    if (!strcmp(SourceDirName, DestDirName)) {
        AMS_RETURN_ERRCODE(EMSALREADYTHERE, EIN_PARAMCHECK, EVIA_CLONEMESSAGE);
    }
    switch (Code) {
        case MS_CLONE_APPEND:
            SourceDirMode = MD_READ;
            PutAtEnd = TRUE;
            break;
        case MS_CLONE_COPY:
            SourceDirMode = MD_READ;
            PutAtEnd = FALSE;
            break;
        case MS_CLONE_APPENDDEL:
            SourceDirMode = MD_WRITE;
            PutAtEnd = TRUE;
            break;
        case MS_CLONE_COPYDEL:
            SourceDirMode = MD_WRITE;
            PutAtEnd = FALSE;
            break;
        default:
            AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CLONEMESSAGE);
    }

    if (ReadOrFindMSDir(SourceDirName, &SourceDir, SourceDirMode) != 0) {
        return (mserrcode);
    }

    if (GetSnapshotByID(SourceDir, id, &msgnum, MySnapshot)) {
        errsave = mserrcode;
        CloseMSDir(SourceDir, SourceDirMode);
        return (errsave);
    }

    if (ReadOrFindMSDir(DestDirName, &DestDir, MD_APPEND)) {
        errsave = mserrcode;
        CloseMSDir(SourceDir, SourceDirMode);
        return (errsave);
    }

    errsave = GetSnapshotByID(DestDir, id, &msgnumdum, SnapshotDum);

    /*
     * This check might be obsolute now, but what else could we do with an ID
     * conflict?
     */
    if (!errsave || AMS_ERRNO != EMSNOSUCHMESSAGE) {
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        if (errsave) {
            return (errsave);
        }
        else {
            AMS_RETURN_ERRCODE(EMSALREADYTHERE, EIN_PARAMCHECK, EVIA_CLONEMESSAGE);
        }
    }
    if ((Msg = (struct MS_Message *) malloc(sizeof(struct MS_Message))) == NULL) {
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CLONEMESSAGE);
    }
    bzero(Msg, sizeof(struct MS_Message));
    Msg->OpenFD = -1;
    sprintf(SourceFileName, "%s/+%s", SourceDir->UNIXDir, id);
    if (ReadRawFile(SourceFileName, Msg, FALSE)
        || ParseMessageFromRawBody(Msg)) {
        FreeMessage(Msg, TRUE);
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        return (mserrcode);
    }
    bcopy(MySnapshot, Msg->Snapshot, AMS_SNAPSHOTSIZE);

    /* A new message should never be marked as deleted */
    AMS_UNSET_ATTRIBUTE(Msg->Snapshot, AMS_ATT_DELETED);

    if (IsMessageAlreadyThere(Msg, DestDir)) {
        FreeMessage(Msg, TRUE);
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        AMS_RETURN_ERRCODE(EMSALREADYTHERE, EIN_PARAMCHECK, EVIA_CLONEMESSAGE);
    }

    if (CopyMessageBody(SourceDir, DestDir, id, PutAtEnd ? 0 : conv64tolong(AMS_DATE(Msg->Snapshot)))) {
        errsave = mserrcode;
        FreeMessage(Msg, TRUE);
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        return (errsave);
    }

    if (PutAtEnd) {
        /* append, with or without copy */

        if (AppendMessageToMSDir(Msg, DestDir)) {
            errsave = mserrcode;
            CloseMSDir(SourceDir, SourceDirMode);
            CloseMSDir(DestDir, MD_APPEND);
            FreeMessage(Msg, TRUE);
            return (errsave);
        }
    }
    else {
        /* Put it in with old date field */
        char            SnapshotBuf[AMS_SNAPSHOTSIZE];
        int             inspos;

        for (inspos = DestDir->MessageCount - 1; inspos >= 0; --inspos) {
            if (GetSnapshotByNumber(DestDir, inspos, SnapshotBuf)) {
                errsave = mserrcode;
                CloseMSDir(SourceDir, SourceDirMode);
                CloseMSDir(DestDir, MD_APPEND);
                FreeMessage(Msg, TRUE);
                return (errsave);
            }
            if (strncmp(AMS_DATE(Msg->Snapshot), AMS_DATE(SnapshotBuf), AMS_DATESIZE) >= 0) {
                break;
            }
            if (RewriteSnapshotInDirectory(DestDir, inspos + 1, SnapshotBuf)) {
                errsave = mserrcode;
                CloseMSDir(SourceDir, SourceDirMode);
                CloseMSDir(DestDir, MD_APPEND);
                FreeMessage(Msg, TRUE);
                return (errsave);
            }
        }
        if (SetChainField(Msg, DestDir, FALSE)) {
            errsave = mserrcode;
            CloseMSDir(SourceDir, SourceDirMode);
            CloseMSDir(DestDir, MD_APPEND);
            FreeMessage(Msg, TRUE);
            return (errsave);
        }
        if (RewriteSnapshotInDirectory(DestDir, inspos + 1, Msg->Snapshot)) {
            errsave = mserrcode;
            CloseMSDir(SourceDir, SourceDirMode);
            CloseMSDir(DestDir, MD_APPEND);
            FreeMessage(Msg, TRUE);
            return (errsave);
        }
        ++DestDir->MessageCount;
    }

    if (CacheDirectoryForClosing(DestDir, MD_APPEND)) {
        errsave = mserrcode;
        CloseMSDir(SourceDir, SourceDirMode);
        CloseMSDir(DestDir, MD_APPEND);
        FreeMessage(Msg, TRUE);
        return (errsave);
    }

    if (Code == MS_CLONE_COPYDEL || Code == MS_CLONE_APPENDDEL) {
        AMS_SET_ATTRIBUTE(Msg->Snapshot, AMS_ATT_DELETED);
        if (RewriteSnapshotInDirectory(SourceDir, msgnum, Msg->Snapshot)) {
            errsave = mserrcode;
            CloseMSDir(SourceDir, SourceDirMode);
            FreeMessage(Msg, TRUE);
            return (errsave);
        }
        FreeMessage(Msg, TRUE);
        return (CacheDirectoryForClosing(SourceDir, SourceDirMode));
    }
    else {
        FreeMessage(Msg, TRUE);
        return (CloseMSDir(SourceDir, SourceDirMode));
    }
}

CopyMessageBody(SourceDir, DestDir, id, timetoset)
struct MS_Directory *SourceDir, *DestDir;
char           *id;
long            timetoset;
{
    char            FromName[1 + MAXPATHLEN], ToName[1 + MAXPATHLEN];
    int             saveerr, c;
    FILE           *fin, *fout;

    sprintf(FromName, "%s/+%s", SourceDir->UNIXDir, id);
    sprintf(ToName, "%s/+%s", DestDir->UNIXDir, id);
    fin = fopen(FromName, "r");
    if (fin == NULL) {
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_COPYMESSAGEBODY);
    }
    fout = fopen(ToName, "w");
    if (fout == NULL) {
        saveerr = errno;
        fclose(fin);                   /* ignore errors */
        AMS_RETURN_ERRCODE(saveerr, EIN_FOPEN, EVIA_COPYMESSAGEBODY);
    }
    while ((c = getc(fin)) != EOF) {
        putc(c, fout);
    }
    fclose(fin);                       /* Reading only, no error check needed */
    errno = 0;                         /* I am not sure ferror really sets
                                        * this, so we start out clean */
    if (ferror(fout) || feof(fout)) {
        saveerr = errno;
        fclose(fout);
        unlink(ToName);                /* If it fails, not much we can do
                                        * anyway */
        AMS_RETURN_ERRCODE(saveerr, EIN_FERROR, EVIA_COPYMESSAGEBODY);
    }
    if (vfclose(fout)) {
        AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_COPYMESSAGEBODY);
    }
    if (timetoset) {
        struct timeval  tvp[2];

        tvp[0].tv_sec = timetoset;
        tvp[0].tv_usec = 0;
        tvp[1].tv_sec = timetoset;
        tvp[1].tv_usec = 0;
#ifdef hpux
        if (utime(ToName, 0)) {
#else                                  /* hpux */
        if (utimes(ToName, tvp)) {
#endif                                 /* hpux */
            AMS_RETURN_ERRCODE(errno, EIN_UTIMES, EVIA_COPYMESSAGEBODY);
        }
    }
    return (0);
}
