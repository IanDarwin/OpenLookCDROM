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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/enssubs.c,v 2.12 1992/12/15 21:18:15 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <stdio.h>

EnsureInSubscriptionMap(DirName)
char *DirName;
{
    return(EnsureSubMapCorrect(DirName, TRUE));
}

EnsureNotInSubscriptionMap(DirName)
char *DirName;
{
    return(EnsureSubMapCorrect(DirName, FALSE));
}

EnsureSubMapCorrect(DirName, DoesExist)
char *DirName;
Boolean DoesExist;
{

    FILE * rfp, *wfp;
    Boolean WroteIt;
    int     code;
    char    MapFileName[1 + MAXPATHLEN],
            TempMapFileName[1 + MAXPATHLEN],
	    MyLine[2 * MAXPATHLEN],
           *s, *t,
            NickName[1 + MAXPATHLEN],
            LineBuf[2 * MAXPATHLEN];

    debug(1,("EnsureSubMapCorrect %s %d\n", DirName, DoesExist));
    strcpy(MapFileName, DirName);
    if (FindTreeRoot(DirName, MapFileName, FALSE)) AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FINDTREEROOT);
    strcat(MapFileName, "/");
    strcat(MapFileName, AMS_SUBSCRIPTIONMAPFILE);
    BuildNickName(DirName, NickName);
    debug(16,("Map file name is %s, nickname %s\n", MapFileName, NickName));
    if (!CheckSubMapCorrect(DirName, MapFileName, NickName, DoesExist)) return(0);
    /* resolve symlinks on subscription map file write -- cn0h 11/22/91 */
    (void) DeSymLink(MapFileName, TempMapFileName, 1);
    if (abspath(TempMapFileName, MapFileName) != 0) strcpy(MapFileName, TempMapFileName);
    strcpy(TempMapFileName, MapFileName);
    strcat(TempMapFileName, ".NEW");
    wfp = fopen(TempMapFileName, "w");
    fchmod(fileno(wfp), 0664);
    if (!wfp) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ENSUREINSUBS);
    }
    rfp = fopen(MapFileName, "r");
    sprintf(MyLine, "%s:%s\n", NickName, DirName);
    if (!rfp) {
	if (errno != ENOENT) {
	    fclose(wfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ENSUREINSUBS);
	}
	debug(16, ("First entry in subscription map file.\n"));
	fputs(MyLine, wfp);
	if (ferror(wfp) || feof(wfp)) {
	    fclose(wfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_ENSUREINSUBS);
	}
    }
    else {
	WroteIt = !DoesExist;
	while (fgets(LineBuf, sizeof(LineBuf), rfp) != NULL) {
	    if (BadSubMapLine(LineBuf)) continue;
	    if (!WroteIt) {
		s = strchr(MyLine, ':');
		t = strchr(LineBuf, ':');
		if (!s || !t) {
		    fclose(rfp);
		    fclose(wfp);
		    AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_ENSUREINSUBS);
		}
		*s = *t = '\0';
		code = PreorderSubscriptionStrcmp(LineBuf, MyLine);
		*s = *t = ':';
		if (code >= 0) {
		    if (code > 0) fputs(MyLine, wfp);
		    WroteIt = TRUE;
		}
	    }
	    if (DoesExist || strcmp(LineBuf, MyLine)) {
		fputs(LineBuf, wfp);
	    }
	}
	if (!WroteIt) {
	    fputs(MyLine, wfp);
	}
	if (ferror(rfp) || ferror(wfp) || feof(wfp)) {
	    fclose(rfp);
	    fclose(wfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_ENSUREINSUBS);
	}
	fclose(rfp);
    }
    if (vfclose(wfp)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_ENSUREINSUBS);
    }
    if (RenameEvenInVice(TempMapFileName, MapFileName)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_ENSUREINSUBS);
    }
    return(0);
}

CheckSubMapCorrect(DirName, MapFileName, NickName, DoesExist)
char *DirName, *MapFileName, *NickName;
Boolean DoesExist;
{
    FILE * rfp;
    Boolean Answer = DoesExist ? FALSE : TRUE;
    char    MyLine[2 * MAXPATHLEN],
            LineBuf[2 * MAXPATHLEN];

    debug(1,("CheckSubMapCorrect %s %d\n", DirName, DoesExist));
    sprintf(MyLine, "%s:%s\n", NickName, DirName);
    rfp = fopen(MapFileName, "r");
    if (rfp) {
	while (fgets(LineBuf, sizeof(LineBuf), rfp) != NULL) {
	    if (!strcmp(MyLine, LineBuf)) {
		Answer = DoesExist ? TRUE : FALSE;
		break;
	    }
	}
	fclose(rfp);
    }
    return(Answer ? 0 : 1);
}
