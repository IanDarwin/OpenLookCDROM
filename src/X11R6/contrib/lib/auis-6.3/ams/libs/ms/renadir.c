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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/renadir.c,v 2.12 1993/08/29 16:23:57 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/time.h */
#include <ms.h>
#include <stdio.h>
#include <mailconf.h>

long MS_RenameDir(OldName, NewName, NewFullName)
char *OldName, *NewName, *NewFullName;
{
    struct MS_Directory *Dir;
    char *s;

    if ((*NewName == '+') || strchr(NewName, ' ')) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_RENAMEDIR);
    }
    if (ReadOrFindMSDir(OldName, &Dir, MD_OK)) {
	return(mserrcode);
    }
    for (s= NewName; *s; ++s) {
	if (*s == '.') *s = '/';
    }
    if (FindTreeRoot(OldName, NewFullName, 0)) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FINDTREEROOT);
    }
    strcat(NewFullName, "/");
    strcat(NewFullName, NewName);
    if (RenameEvenInVice(OldName, NewFullName)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_RENAMEDIR);
    }
    if (DeleteFromDirCache(Dir)
    || HandleTreeNameChange(OldName, NewFullName)) {
	return(mserrcode);
    }
    return(0);
}

HandleTreeNameChange(OldName, NewName)
char *OldName, *NewName;
{
    char OldNick[1+MAXPATHLEN], NewNick[1+MAXPATHLEN], NewFullName[1+MAXPATHLEN], SubMapFile[1+MAXPATHLEN], NewSubMapFile[1+MAXPATHLEN], NewSubMapFile2[1+MAXPATHLEN], LineBuf[10+MAXPATHLEN+MAXPATHLEN], *fn, *suffix, OldRoot[1+MAXPATHLEN];
    FILE *rfp, *wfp, *wfp2, *chfp;
    int oldlen, errors = 0;
    long lasterror = 0, clock;
    char ThisTime[50];

    debug(1, ("Changing Tree from %s to %s\n", OldName, NewName));
    if (FindTreeRoot(OldName, OldRoot, 0)) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FINDTREEROOT);
    }
    clock = time(0);
    strcpy(ThisTime, ctime(&clock));
    strcpy(SubMapFile, OldRoot);
    strcat(SubMapFile, "/");
    strcat(SubMapFile, AMS_SUBSCRIPTIONMAPFILE);
    /* resolve symlinks on subscription map file write -- cn0h 12/13/91 */
    (void) DeSymLink(SubMapFile, NewSubMapFile, 1);
    if (abspath(NewSubMapFile, SubMapFile) != 0) strcpy(SubMapFile, NewSubMapFile);
    rfp = fopen(SubMapFile, "r");
    if (!rfp) AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    strcpy(NewSubMapFile, SubMapFile);
    strcat(NewSubMapFile, ".NEW");
    wfp = fopen(NewSubMapFile, "w");
    if (!wfp) {
	fclose(rfp);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    }
    chfp = fopen(GlobalChangeSubsFile, "a");
    if (!chfp && ((errno != ENOENT) && (errno != EACCES))) {
	fclose(rfp);
	fclose(wfp);
    }
    strcpy(NewSubMapFile2, SubMapFile);
    strcat(NewSubMapFile2, ".NEW2");
    wfp2 = fopen(NewSubMapFile2, "w");
    if (!wfp2) {
	if (chfp) fclose(chfp);
	fclose(rfp);
	fclose(wfp);
	unlink(NewSubMapFile);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    }
    oldlen = strlen(OldName);
    debug(256, ("Going through subscription map file\n"));
    while (fgets(LineBuf, sizeof(LineBuf)-1, rfp)) {
	StripWhiteEnds(LineBuf);
	debug(256, ("Handling line %s\n", LineBuf));
	fn = strchr(LineBuf, ':');
	if (fn) *fn++ = '\0';
	suffix = fn + oldlen;
	if (fn && !strncmp(fn, OldName, oldlen) && (*suffix == '\0' || *suffix == '/')) {
	    debug(256, ("This is a change!\n"));
	    strcpy(NewFullName, NewName);
	    strcat(NewFullName, suffix);
	    debug(256, ("New full name should be %s\n", NewFullName));
	    BuildNickName(NewFullName, NewNick);
	    fprintf(wfp2, "%s:%s\n", NewNick, NewFullName);
	    if (chfp) fprintf(chfp, "%s %s %s", fn, NewFullName, ThisTime);
	    BuildNickName(fn, OldNick);
	    if (HandleOneChange(NewFullName, NewNick, fn, OldNick)) {
		++errors;
		lasterror = mserrcode;
	    }
	} else {
	    if (fn) {
		fprintf(wfp, "%s:%s\n", LineBuf, fn);
	    } else {
		fprintf(wfp, "%s\n", LineBuf);
	    }
	}
    }
    fclose(rfp);
    if (ferror(wfp) || feof(wfp)) {
	fclose(wfp);
	fclose(wfp2);
	if (chfp) fclose(chfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_RENAMEDIR);
    }
    if (vfclose(wfp)) {
	fclose(wfp2);
	if (chfp) fclose(chfp);
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_RENAMEDIR);
    }
    if (ferror(wfp2) || feof(wfp2)) {
	fclose(wfp2);
	if (chfp) fclose(chfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_RENAMEDIR);
    }
    if (vfclose(wfp2)) {
	if (chfp) fclose(chfp);
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_RENAMEDIR);
    }
    if (chfp) {
	if (ferror(chfp) || feof(chfp)) {
	    fclose(chfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_RENAMEDIR);
	}
	if (vfclose(chfp)) {
	    AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_RENAMEDIR);
	}
    }
    if (MergeSubMaps(NewSubMapFile, NewSubMapFile2, SubMapFile)) {
	return(mserrcode);
    }
    if (errors) {
	if (errors > 1) {
	    char ErrorText[256];
	    sprintf(ErrorText, "There were %d errors in the rename -- rebuild/reindex may be necessary\n", errors);
	    NonfatalBizarreError(ErrorText);
	}
	return(lasterror);
    }
    return(0);
}

HandleOneChange(NewFullName, NewNick, OldFullName, OldNick)
char *NewFullName, *NewNick, *OldFullName, *OldNick;
{
    int status;
    char Scratch[1+MAXPATHLEN], dbuf[1+AMS_DATESIZE];

    if (MS_GetSubscriptionEntry(OldFullName, Scratch, &status)) {
	return(mserrcode);
    }
    if ((status != AMS_UNSUBSCRIBED && SetSubsEntry(OldFullName, OldNick, AMS_UNSUBSCRIBED))
    || SetSubsEntry(NewFullName, NewNick, status)
    || MS_GetAssociatedTime(OldFullName, dbuf, AMS_DATESIZE)
    || (dbuf[0] && MS_SetAssociatedTime(NewFullName, dbuf))
    || DropHint(NewFullName)
    || DropHint(OldFullName)
    || RemoveFromCrucialClassesPreference(OldFullName, NewFullName)) {
	return(mserrcode);
    }
    return(0);
}

MergeSubMaps(r1, r2, target)
char *r1, *r2, *target;
{
    char TempName[1+MAXPATHLEN], LBuf1[MAXPATHLEN+MAXPATHLEN+10], LBuf2[MAXPATHLEN+MAXPATHLEN+10], *s1, *s2;
    FILE *rf1, *rf2, *w;
    Boolean UseFirst;

    strcpy(TempName, target);
    strcat(TempName, ".MERGED");
    rf1 = fopen(r1, "r");
    if (!rf1) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    }
    rf2 = fopen(r2, "r");
    if (!rf2) {
	fclose(rf1);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    }
    w = fopen(TempName, "w");
    if (!w) {
	fclose(rf1);
	fclose(rf2);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_RENAMEDIR);
    }
    LBuf1[0] = '\0';
    LBuf2[0] = '\0';
    while (TRUE) {
	if (!LBuf1[0]) {
	    fgets(LBuf1, sizeof(LBuf1), rf1);
	}
	if (!LBuf2[0]) {
	    fgets(LBuf2, sizeof(LBuf2), rf2);
	}
	if (!LBuf1[0] && !LBuf2[0]) {
	    break; /* all done */
	}
	if (!LBuf1[0]) {
	    UseFirst = FALSE;
	} else if (!LBuf2[0]) {
	    UseFirst = TRUE;
	} else {
	    /* have to choose here */
	    s1 = strchr(LBuf1, ':');
	    if (s1) {
		++s1;
		s2 = strchr(LBuf2, ':');
		if (s2) {
		    ++s2;
		    if (strcmp(s1, s2) > 0) {
			UseFirst = FALSE;
		    } else {
			UseFirst = TRUE;
		    }
		} else {
		    UseFirst = TRUE;
		}
	    } else {
		UseFirst = FALSE;
	    }
	}
	if (UseFirst) {
	    fputs(LBuf1, w);
	    LBuf1[0] = '\0';
	} else {
	    fputs(LBuf2, w);
	    LBuf2[0] = '\0';
	}
    }
    fclose(rf1);
    fclose(rf2);
    if (ferror(w) || feof(w)) {
	fclose(w);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_RENAMEDIR);
    }
    if (vfclose(w)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_RENAMEDIR);
    }
    if (RenameEvenInVice(TempName, target)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_RENAMEDIR);
    }
    unlink(r1);
    unlink(r2);
    return(0);
}

