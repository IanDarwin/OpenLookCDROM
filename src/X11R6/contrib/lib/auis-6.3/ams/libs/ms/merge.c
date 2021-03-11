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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/merge.c,v 2.19 1993/09/21 21:57:44 gk5g Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <stdio.h>
#include <sys/stat.h>
#include <ms.h>
#include <mailconf.h>

extern char home[];

long    MS_MergeDirectories (SourceDirName, DestDirName)
char   *SourceDirName,
       *DestDirName;
{
    Boolean UseDest;
    int errsave;
    struct MS_Directory *SourceDir, *DestDir;
    DIR *dirp;
    int ndfd, odfd, DestCount, SourceCount;
    DIRENT_TYPE *dirent;
    struct stat stbuf;
    char DestSnap[AMS_SNAPSHOTSIZE+1], SourceSnap[AMS_SNAPSHOTSIZE+1];
    char SourceFileName[1+MAXPATHLEN], DestFileName[1+MAXPATHLEN], NewFileName[1+MAXPATHLEN], FinalFileName[1+MAXPATHLEN];
    FILE *chfp;
    long clock;

    debug(1, ("Entering MS_MergeDirectories %s %s\n", SourceDirName, DestDirName));
    CloseDirsThatNeedIt();
    if ((dirp = opendir(SourceDirName)) == NULL) {
	debug(4, ("Can't open source directory %s\n", SourceDirName));
	errsave = errno;
	AMS_RETURN_ERRCODE(errsave, EIN_OPENDIR, EVIA_MERGEDIRS);
    }
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strcmp(dirent->d_name, ".")) continue;
	if (!strcmp(dirent->d_name, "..")) continue;
	sprintf(SourceFileName, "%s/%s", SourceDirName, dirent->d_name);
	if (stat(SourceFileName, &stbuf)) {
	    errsave = errno;
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(errsave, EIN_STAT, EVIA_MERGEDIRS);
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(EMSDIRHASKIDS, EIN_PARAMCHECK, EVIA_MERGEDIRS);
	}
    }
    sprintf(NewFileName, "%s/%s.merge", DestDirName, MS_DIRNAME);
    sprintf(FinalFileName, "%s/%s", DestDirName, MS_DIRNAME);
    ndfd = open(NewFileName, O_RDWR | O_CREAT | O_TRUNC, 0664);
    if (ndfd < 0) {
	errsave = errno;
	closedir(dirp);
	AMS_RETURN_ERRCODE(errsave, EIN_OPEN, EVIA_MERGEDIRS);
    }
    if (ReadOrFindMSDir(SourceDirName, &SourceDir, MD_READ)) {
	errsave = mserrcode;
	closedir(dirp);
	return(errsave);
    }
    if (ReadOrFindMSDir(DestDirName, &DestDir, MD_READ)) {
	errsave = mserrcode;
	CloseMSDir(SourceDir, MD_READ);
	closedir(dirp);
	return(errsave);
    }
    odfd = DestDir->fd;
    DestDir->fd = ndfd;
    DestDir->OpenMode = MD_WRITE;
    if (DestructivelyWriteDirectoryHead(DestDir)) {
	errsave = mserrcode;
	closedir(dirp);
	CloseMSDir(SourceDir, MD_READ);
	CloseMSDir(DestDir, MD_READ);
	close(odfd);
	return(errsave);
    }
    DestDir->fd = odfd;
    DestDir->OpenMode = MD_READ;
    DestSnap[0] = SourceSnap[0] = '\0';
    DestCount = SourceCount = 0;
    if (SourceDir->MessageCount > 0
	 && GetSnapshotByNumber(SourceDir, SourceCount, SourceSnap))
    {
	errsave = mserrcode;
	closedir(dirp);
	CloseMSDir(SourceDir, MD_READ);
	CloseMSDir(DestDir, MD_READ);
	close(ndfd);
	return(errsave);
    }
    if (DestDir->MessageCount > 0
	 && GetSnapshotByNumber(DestDir, DestCount, DestSnap))
    {
	errsave = mserrcode;
	closedir(dirp);
	CloseMSDir(SourceDir, MD_READ);
	CloseMSDir(DestDir, MD_READ);
	close(ndfd);
	return(errsave);
    }
    while (TRUE) { /* Will break out when necessary */
	if (DestSnap[0] && SourceSnap[0]) {
	    if (strcmp(AMS_DATE(DestSnap), AMS_DATE(SourceSnap)) > 0) {
		UseDest = FALSE;
	    } else {
		UseDest = TRUE;
	    }
	} else if (DestSnap[0]) {
	    UseDest = TRUE;
	} else if (SourceSnap[0]) {
	    UseDest = FALSE;
	} else {
	    break;
	}
	if (UseDest) {
	    if (writeall(ndfd, DestSnap, AMS_SNAPSHOTSIZE) < 0) {
		errsave = errno;
		closedir(dirp);
		CloseMSDir(SourceDir, MD_READ);
		CloseMSDir(DestDir, MD_READ);
		close(ndfd);
		AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_MERGEDIRS);
	    }
	    if (++DestCount < DestDir->MessageCount) {
		if (GetSnapshotByNumber(DestDir, DestCount, DestSnap)) {
		    errsave = mserrcode;
		    closedir(dirp);
		    CloseMSDir(SourceDir, MD_READ);
		    CloseMSDir(DestDir, MD_READ);
		    close(ndfd);
		    return(mserrcode);
		}
	    } else {
		DestSnap[0] = '\0';
	    }
	} else {
	    if (writeall(ndfd, SourceSnap, AMS_SNAPSHOTSIZE) < 0) {
		errsave = errno;
		closedir(dirp);
		CloseMSDir(SourceDir, MD_READ);
		CloseMSDir(DestDir, MD_READ);
		close(ndfd);
		AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_MERGEDIRS);
	    }
	    if (++SourceCount < SourceDir->MessageCount) {
		if (GetSnapshotByNumber(SourceDir, SourceCount, SourceSnap)) {
		    errsave = mserrcode;
		    closedir(dirp);
		    CloseMSDir(SourceDir, MD_READ);
		    CloseMSDir(DestDir, MD_READ);
		    close(ndfd);
		    return(mserrcode);
		}
	    } else {
		SourceSnap[0] = '\0';
	    }
	}
    }
    CloseMSDir(SourceDir, MD_READ);
    CloseMSDir(DestDir, MD_READ);
    if (vclose(ndfd)) {
	errsave = errno;
	closedir(dirp);
	AMS_RETURN_ERRCODE(errsave, EIN_VCLOSE, EVIA_MERGEDIRS);
    }
    rewinddir(dirp);
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strcmp(dirent->d_name, ".")) continue;
	if (!strcmp(dirent->d_name, "..")) continue;
	sprintf(SourceFileName, "%s/%s", SourceDirName, dirent->d_name);
	sprintf(DestFileName, "%s/%s", DestDirName, dirent->d_name);
	if (RenameEvenInVice(SourceFileName, DestFileName)) {
	    errsave = errno;
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(errsave, EIN_RENAME, EVIA_MERGEDIRS);
	}
    }
    closedir(dirp);
    if (RenameEvenInVice(NewFileName, FinalFileName)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_MERGEDIRS);
    }
    if(rmdir(SourceDirName) && errno != ENOENT) { /* Bogus vice kernel bug again */
	AMS_RETURN_ERRCODE(errno, EIN_RMDIR, EVIA_MERGEDIRS);
    }
    chfp = fopen(GlobalChangeSubsFile, "a");
    if (chfp) {
	clock = time(0);
	fprintf(chfp, "%s %s %s", SourceDirName, DestDirName, ctime(&clock));
	fclose(chfp);
    }
    return(EnsureNotInSubscriptionMap(SourceDirName));
}

long    MS_EditMessage (dirname, id, NewBodyFile, Reparse)
char   *dirname,
       *id,
       *NewBodyFile;
int	Reparse;
{
    int saveerr, c;
    struct MS_Directory *Dir;
    FILE *fpin, *fpout;
    char BodyFileName[1+MAXPATHLEN], TmpBodyFileName[1+MAXPATHLEN];

    debug(1, ("Entering MS_EditMessage dir %s id %s newfile %s reparse %d\n", dirname, id, NewBodyFile, Reparse));
    if (ReadOrFindMSDir(dirname, &Dir, MD_OK)) {
	return(mserrcode);
    }
    fpin = fopen(NewBodyFile, "r");
    if (fpin == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_EDITMESSAGE);
    }
    sprintf(BodyFileName, "%s/+%s", dirname, id);
    strcpy(TmpBodyFileName, BodyFileName);
    strcat(TmpBodyFileName, ".tmp");
    fpout = fopen(TmpBodyFileName, "w");
    if (fpout == NULL) {
	saveerr = errno;
	fclose(fpin);
	AMS_RETURN_ERRCODE(saveerr, EIN_FOPEN, EVIA_EDITMESSAGE);
    }
    while ((c = getc(fpin)) != EOF) {
	putc(c, fpout);
    }
    if (ferror(fpin) || ferror(fpout) || feof(fpout)) {
	fclose(fpin);
	fclose(fpout);
	unlink(TmpBodyFileName);
	AMS_RETURN_ERRCODE(EMSFILEERR, EIN_FERROR, EVIA_EDITMESSAGE);
    }
    fclose(fpin);
    if (vfclose(fpout)) {
	saveerr=errno;
	unlink(TmpBodyFileName);
	AMS_RETURN_ERRCODE(saveerr, EIN_VFCLOSE, EVIA_EDITMESSAGE);
    }
    if (RenameEvenInVice(TmpBodyFileName, BodyFileName)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_EDITMESSAGE);
    }
    if (Reparse) {
	int dummy;
	struct MS_Message *Msg;
	struct MS_CaptionTemplate CT;
	char OldSnapshot[AMS_SNAPSHOTSIZE];

	if (GetSnapshotByID(Dir, id, &dummy, OldSnapshot)) {
	    return(mserrcode);
	}
	Msg = (struct MS_Message   *) malloc (sizeof (struct MS_Message));
	if (Msg == NULL) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PROCNEWMSGS);
	}
	bzero(Msg, sizeof(struct MS_Message));
	Msg->OpenFD = -1;
	bzero(&CT, sizeof(struct MS_CaptionTemplate));
	CT.datetype = DATETYPE_CURRENT;
	CT.basictype = BASICTEMPLATE_NORMAL;
	if (ReadRawFile(BodyFileName, Msg, FALSE)
	    || ParseMessageFromRawBody(Msg)
	    || CheckAuthUid(Msg)
	    || BuildAttributesField(Msg)
	    || BuildCaption(Msg, &CT,
	    	(strncmp(dirname, home, strlen(home))) ? FALSE : TRUE))
	{
	    FreeMessage(Msg, TRUE);
	    return(mserrcode);
	}
	strcpy(AMS_DATE(Msg->Snapshot), AMS_DATE(OldSnapshot));
	if (MS_AlterSnapshot(dirname, id, Msg->Snapshot, ASS_REPLACE_ATT_CAPT)) {
	    FreeMessage(Msg, TRUE);
	    return(mserrcode);
	}
	FreeMessage(Msg, TRUE);
    }
    return(0);
}
