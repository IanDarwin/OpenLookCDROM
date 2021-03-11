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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/apndfile.c,v 2.14 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <ms.h>

MS_AppendFileToFolder(FileName, FolderName)
char           *FileName, *FolderName;          /* BOTH IN */
{
    debug(1, ("MS_AppendFileToFolder %s %s\n", FileName, FolderName));
    return (AppendFileToFolder(FileName, FolderName, TRUE));
}

AppendFileToFolder(FileName, FolderName, DoDelete)
char           *FileName, *FolderName;
int             DoDelete;
{
    int             errsave = 0;
    struct MS_Directory *Dir = NULL;

    CloseDirsThatNeedIt();
    if (ReadOrFindMSDir(FolderName, &Dir, MD_APPEND)) {
        errsave = mserrcode;
        if(Dir) CloseMSDir(Dir, MD_APPEND);
        return (errsave);
    }
    errsave = AppendFileToMSDir(FileName, Dir, DoDelete);
    mserrcode = CloseMSDir(Dir, MD_APPEND);
    return (errsave ? errsave : mserrcode);
}


AppendFileToMSDir(FileName, Dir, DoDelete)
char           *FileName;
struct MS_Directory *Dir;
int             DoDelete;
{
    return (AppendFileToMSDirInternal(FileName, Dir, DoDelete, FALSE));
}

AppendFileToMSDirPreservingFileName(FileName, Dir, DoDelete)
char           *FileName;
struct MS_Directory *Dir;
int             DoDelete;
{
    return (AppendFileToMSDirInternal(FileName, Dir, DoDelete, TRUE));
}

AppendFileToMSDirInternal(FileName, Dir, DoDelete, TreatAsAlien)
char           *FileName;
struct MS_Directory *Dir;
int             DoDelete;
int             TreatAsAlien;
{
    struct MS_Message *Msg;
    struct MS_CaptionTemplate CapTemplate;
    int             saveerr;
    char            NewFileName[1 + MAXPATHLEN];

    Msg = (struct MS_Message *) malloc(sizeof(struct MS_Message));
    if (Msg == NULL) {
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_APPENDMESSAGETOMSDIR);
    }
    bzero(Msg, sizeof(struct MS_Message));
    Msg->OpenFD = -1;
    bzero(&CapTemplate, sizeof(struct MS_CaptionTemplate));
    CapTemplate.basictype = BASICTEMPLATE_NORMAL;
    CapTemplate.datetype = DATETYPE_FROMHEADER;
    if (ReadRawFile(FileName, Msg, DoDelete)
        || ParseMessageFromRawBody(Msg)
        || CheckAuthUid(Msg)
        || BuildDateField(Msg, DATETYPE_FROMHEADER)
        || BuildReplyField(Msg)
        || BuildAttributesField(Msg)
        || InventID(Msg)
        || BuildCaption(Msg, &CapTemplate, TRUE)) {
        saveerr = mserrcode;
        FreeMessage(Msg, TRUE);
        return (saveerr);
    }
    if (TreatAsAlien) {
        char           *s;

        s = strrchr(FileName, '/');
        if (s)
            *s++ = '\0';
        if (!s || strcmp(Dir->UNIXDir, FileName)
            || (strlen(s) >= AMS_IDSIZE)) {
            FreeMessage(Msg, TRUE);
            AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_APPENDMESSAGETOMSDIR);
        }
        strcpy(AMS_ID(Msg->Snapshot), s);
        DoDelete = FALSE;              /* Cannot allow it! */
    }
    if (IsMessageAlreadyThere(Msg, Dir)) {
        if (DoDelete)
            unlink(FileName);
        return (0);
    }
    if (!TreatAsAlien) {
        sprintf(NewFileName, "%s/+%s", Dir->UNIXDir, AMS_ID(Msg->Snapshot));
        if (WritePureFile(Msg, NewFileName, FALSE, 0664)) {
            saveerr = mserrcode;
            FreeMessage(Msg, TRUE);
            return (saveerr);
        }
    }
    if (AppendMessageToMSDir(Msg, Dir)) {
        FreeMessage(Msg, TRUE);
        unlink(NewFileName);           /* The old copy of the file is still in
                                        * place */
        return (mserrcode);
    }
    FreeMessage(Msg, TRUE);
    if (DoDelete)
        unlink(FileName);              /* Errors here are funny; better an
                                        * orphan file than a bogus error
                                        * message, though */
    return (0);
}
