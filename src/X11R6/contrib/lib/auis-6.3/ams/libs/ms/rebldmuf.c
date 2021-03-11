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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rebldmuf.c,v 2.19 1992/12/15 21:20:51 rr2b R6tape $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <ms.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>

#define BIGLINE 1500

extern char *StripWhiteEnds(), *fixDate();

MS_RebuildMasterUpdateFiles(NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood) 
int *NumFastGood, *NumSlowGood, *NumBad, *NumAbsent, *NumProbablyGood;
{
    char PathElt[1+MAXPATHLEN];
    int i=0;

    debug(1, ("MS_RebuildMasterUpdateFiles\n"));
    *NumSlowGood = *NumFastGood = *NumBad = *NumAbsent = *NumProbablyGood = 0;
    while (MS_GetSearchPathEntry(i++, PathElt, MAXPATHLEN) == 0) {
	if (RebuildOneMasterUpdateFile(PathElt, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood)) return(mserrcode);
    }
    return(0);
}

MS_RebuildOneMasterUpdateFile(PathElt, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood) 
char *PathElt;
int *NumFastGood, *NumSlowGood, *NumBad, *NumAbsent, *NumProbablyGood;
{
    *NumSlowGood = *NumFastGood = *NumBad = *NumAbsent = *NumProbablyGood = 0;
    return(RebuildOneMasterUpdateFile(PathElt, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood));
}

RebuildOneMasterUpdateFile(PathElt, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood) 
char *PathElt;
int *NumFastGood, *NumSlowGood, *NumBad, *NumAbsent, *NumProbablyGood;
{
    int code, errsave, lockfd;
    char SubMapFile[MAXPATHLEN+1], *sdum,
    MasterUpdateFile[MAXPATHLEN+1], RealPath1[MAXPATHLEN+1], RealPath2[MAXPATHLEN+1];
    FILE *oldupfp, *submapfp;
    char SubMapLine[BIGLINE], OldUpdateLine[BIGLINE], *s;
    Boolean NeedToBumpSubs, NeedToBumpUpdate, SubsEOF, UpEOF;

    debug(2048, ("Checking %s\n", PathElt));
    sdum = strrchr(PathElt, '/');
    if (!sdum) {
	AMS_RETURN_ERRCODE(EMSNOTTREEROOT, EIN_PARAMCHECK, EVIA_REBUILDMASTERUPS);
    }
    if (strncmp(++sdum, MS_TREEROOT, sizeof(MS_TREEROOT)-1)) {
	AMS_RETURN_ERRCODE(EMSNOTTREEROOT, EIN_PARAMCHECK, EVIA_REBUILDMASTERUPS);
    }
    (void) DeSymLink(PathElt, RealPath1, 0);
    if (abspath(RealPath1, RealPath2) != 0) strcpy(RealPath2, RealPath1);
    if (MS_LockMUF(RealPath2, &lockfd)) {
	char ErrorText[100+MAXPATHLEN];
	sprintf(ErrorText, "Could not lock master update file in %s, continuing anyway", RealPath2);
	NonfatalBizarreError(ErrorText);
    }
    sprintf(MasterUpdateFile, "%s/%s", RealPath2, MS_MASTERUPDATE);
    oldupfp = fopen(MasterUpdateFile, "r");
    if (!oldupfp) {
	char ErrorText[100+MAXPATHLEN];
	if (lockfd != -1) close(lockfd);
	if (errno == ENOENT) return(0);
	sprintf(ErrorText, "Could not read master update file %s", ap_Shorten(MasterUpdateFile));
	NonfatalBizarreError(ErrorText);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_REBUILDMASTERUPS);
    }
    sprintf(SubMapFile, "%s/%s", RealPath2, AMS_SUBSCRIPTIONMAPFILE);
    submapfp = fopen(SubMapFile, "r");
    if (!submapfp) {
	char ErrorText[100+MAXPATHLEN];
	errsave = errno;
	fclose(oldupfp);
	if (lockfd != -1) close(lockfd);
	sprintf(ErrorText, "Could not read subscription map file %s", ap_Shorten(SubMapFile));
	NonfatalBizarreError(ErrorText);
	AMS_RETURN_ERRCODE(errsave, EIN_FOPEN, EVIA_REBUILDMASTERUPS);
    }
    NeedToBumpSubs = NeedToBumpUpdate = TRUE;
    SubsEOF = UpEOF = FALSE;
    while (!SubsEOF || !UpEOF) {
	if (NeedToBumpSubs) {
	    errno = 0;
	    if (!fgets(SubMapLine, sizeof(SubMapLine), submapfp)) {
		if (ferror(submapfp)) {
		    errsave = errno;
		    sprintf(SubMapLine, "fgets(%s): %s; treating as EOF.", ap_Shorten(SubMapFile), UnixError(errsave));
		    NonfatalBizarreError(SubMapLine);
/*		    fclose(submapfp);
		    fclose(oldupfp);
		    AMS_RETURN_ERRCODE(errsave, EIN_FGETS, EVIA_REBUILDMASTERUPS); */
		}
		SubMapLine[0] = '\0';
		SubsEOF = TRUE;
	    }
	    if (BadSubMapLine(SubMapLine)) continue;
	    NeedToBumpSubs = FALSE;
	}
	if (NeedToBumpUpdate) {
	    errno = 0;
	    if (!fgets(OldUpdateLine, sizeof(OldUpdateLine), oldupfp)) {
		if (ferror(oldupfp)) {
		    errsave = errno;
		    sprintf(OldUpdateLine, "fgets(%s): %s; treating as EOF.", ap_Shorten(MasterUpdateFile), UnixError(errsave));
		    NonfatalBizarreError(OldUpdateLine);
/*		    fclose(submapfp);
		    fclose(oldupfp);
		    AMS_RETURN_ERRCODE(errsave, EIN_FGETS, EVIA_REBUILDMASTERUPS); */
		}
		OldUpdateLine[0] = '\0';
		UpEOF = TRUE;
	    }
	    if (BadUpdFileLine(OldUpdateLine)) continue;
	    NeedToBumpUpdate = FALSE;
	}
	if (SubsEOF && UpEOF) break;
	code = CompareSubsAndUpdateLines(SubMapLine, OldUpdateLine);
	if (code > 0) {
	    if (CheckUpdateLine(OldUpdateLine, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood, TRUE)) {
		fclose(oldupfp);
		if (lockfd != -1) close(lockfd);
		return(mserrcode);
	    }
	    NeedToBumpUpdate = TRUE;
	} else if (code < 0) {
	    s = strchr(SubMapLine, ':');
	    if (s) ++s;
	    if (CheckUpdateLine(s ? s : SubMapLine, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood, FALSE)) {
		fclose(oldupfp);
		if (lockfd != -1) close(lockfd);
		return(mserrcode);
	    }
	    NeedToBumpSubs = TRUE;
	} else {
	    if (CheckUpdateLine(OldUpdateLine, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood, TRUE)) {
		fclose(oldupfp);
		if (lockfd != -1) close(lockfd);
		return(mserrcode);
	    }
	    NeedToBumpUpdate = TRUE;
	    NeedToBumpSubs = TRUE;
	}
    }
    fclose(submapfp);
    fclose(oldupfp);
    if (lockfd != -1) close(lockfd);
    return(0);
}    

/* This routine compares a line from a subscription map file and a line
    from a master update file and decides which comes first.
    It returns > 0 if the update line comes first,
    < 0 if the subs line comes first, and 0 if they refer to the same thing.
*/

CompareSubsAndUpdateLines(sline, uline)
char *sline, *uline;
{
    char *s;
    int code, len;

    if (!sline || !*sline) return(1);
    if (!uline || !*uline) return(-1);
    s = strchr(sline, ':');
    if (s) ++s;
    len = strlen(s) - 1; /* ignore trailing newline */
    code = strncmp(s, uline, len);
    if (code == 0) {
	if (uline[len] == ' ') {
	    return(0);
	} else {
	    return(-1);
	}
    }
    return(code);
}

char *
DescribeTimeInterval(interval)
long interval;
{
    static char DescBuf[30];

    if (interval < 60) {
	sprintf(DescBuf, "%d seconds", interval);
    } else {
	interval /= 60;
	if (interval < 60) {
	    sprintf(DescBuf, "%d minutes", interval);
	} else {
	    interval /= 60;
	    if (interval < 72) {
		sprintf(DescBuf, "%d hours", interval);
	    } else {
		interval /= 24;
		if (interval < 29) {
		    sprintf(DescBuf, "%d days", interval);
		} else {
		    sprintf(DescBuf, "%d weeks", interval/7);
		}
	    }
	}
    }
    return(DescBuf);
}

/* This routine checks out a line from a master update file, and writes
    out a hint when necessary.
*/

CheckUpdateLine(line, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood, LineIsFromUpdateFile)
char *line;
int *NumFastGood, *NumSlowGood, *NumBad, *NumAbsent, *NumProbablyGood;
Boolean LineIsFromUpdateFile;
{
    char *date, *stamp, SnapshotDum[AMS_SNAPSHOTSIZE], ErrorText[200+MAXPATHLEN], *s;
    long timestamp, errsave;
    int mistakes;
    struct stat stbuf;
    Boolean ParsedOldGuy = FALSE;
    struct MS_Directory *Dir;

    debug(256, ("CheckUpdateLine %s\n", line));
    /* In the past, illegal chars have gotten into MUF files from bad Ethernet interfaces, sigh... */

    mistakes = 0;
    for (s=line; *s; ++s) { /* The error message should be kind to console */
	if (!isprint(*s) && !isspace(*s)) {
	    sprintf(ErrorText, "Illegal character in %s file: ASCII %d (decimal)", LineIsFromUpdateFile ? "update" : "subscription map", *s);
	    NonfatalBizarreError(ErrorText);
	    *s = 'X';
	    ++mistakes;
	}
    }
    if (mistakes) {
	sprintf(ErrorText, "Ignoring illegal %s entry: %s", LineIsFromUpdateFile ? "master update file" : "subscription map", line);
	NonfatalBizarreError(ErrorText);
	return(0);
    }
    date = strchr(line, ' ');
    if (date) {
	*date++ = '\0';
	stamp = strchr(date, ' ');
	if (stamp) {
	    ParsedOldGuy = TRUE;
	    *stamp++ = '\0';
	    if (!*date) date = "000000";
	    if (strlen(date) >= (AMS_DATESIZE-1)) fixDate(date);
	    timestamp = atoi(stamp);
	    if (!stat(line, &stbuf)) {
		if (stbuf.st_mtime == timestamp) {
		    debug(256, ("It was a quickie\n"));
		    ++*NumFastGood;
		    return(0);
		}
	    }
	}
    }
    debug(256, ("Slow approach, sigh...\n"));
    line = StripWhiteEnds(line);
    if (ReadOrFindMSDir(line, &Dir, MD_READ)) {
	if (AMS_ERRNO == ENOENT) { /* It has been deleted */
	    ++*NumAbsent;
		sprintf(ErrorText, "The folder '%s' has recently been deleted.", ap_Shorten(line));
	    if (!LineIsFromUpdateFile) {
		strcat(ErrorText, "  It still appears in the subscription map.");
	    }
	    NonfatalBizarreError(ErrorText);
	    DropHint(line);  /* Ignore errors because sometimes the whole tree is gone */
	    return(0);
	}
	++*NumBad;
	if (AMS_ERRNO == EMSBADDIRFORMAT) {
	    /* Not much point in dropping a hint for this one, and the console message has already gone out... */
	    return(0);
	}
	if (AMS_ERRNO == EACCES) {
	    sprintf(ErrorText, "Permission denied trying to check folder %s", ap_Shorten(line));
	    CriticalBizarreError(ErrorText); /* Get attention */
	    return(0);
	}

	sprintf(ErrorText, "Could not check update date slowly for %s: %s", ap_Shorten(line), UnixError(AMS_ERRNO));
	NonfatalBizarreError(ErrorText);
	return(mserrcode);
    }
    if (Dir->MessageCount > 0) {
	if (GetSnapshotByNumber(Dir, Dir->MessageCount - 1, SnapshotDum)) {
	    errsave = mserrcode;
	    CloseMSDir(Dir, MD_READ);
	    return(errsave);
	}
    } else {
	strcpy(AMS_DATE(SnapshotDum), "000000");
    }
    CloseMSDir(Dir, MD_READ);
    if (ParsedOldGuy) {
	if (!strcmp(AMS_DATE(SnapshotDum), date)) {
	    debug(256, ("Entry for %s (%s) was correct but validated slowly.\n",  Dir->UNIXDir, date)); 
	    ++*NumSlowGood;
	    DropHint(Dir->UNIXDir); /* Ignore error code here */
	    return(0);
	} else {
	    int age;

	    age = time(0) - conv64tolong(AMS_DATE(SnapshotDum));
#define TOLERABLELAG 6* 60 * 60 /* six hours -- not worth complaining about? */
	    if (age > TOLERABLELAG) {
		++*NumBad;
		sprintf(ErrorText, "Entry for %s has needed correction for %s (%s->%s)", ap_Shorten(Dir->UNIXDir), DescribeTimeInterval(age), date, AMS_DATE(SnapshotDum));
		NonfatalBizarreError(ErrorText);
	    } else {
		++*NumProbablyGood;
		debug(256, ("Probably OK: %s has needed correction for just %s (%s->%s)", Dir->UNIXDir, DescribeTimeInterval(age), date, AMS_DATE(SnapshotDum)));
	    }
	    return(DropHint(Dir->UNIXDir));
	}
    } else {
	sprintf(ErrorText, "There was previously no entry for '%s', which is at least %s old", ap_Shorten(Dir->UNIXDir), DescribeTimeInterval(time(0) - conv64tolong(AMS_DATE(SnapshotDum))));
	NonfatalBizarreError(ErrorText);
	++*NumAbsent;
	return(DropHint(Dir->UNIXDir));
    }
}
