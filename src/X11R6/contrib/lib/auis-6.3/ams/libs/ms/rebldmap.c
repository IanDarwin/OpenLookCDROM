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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rebldmap.c,v 2.13 1992/12/15 21:20:51 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <stdio.h>

MS_RebuildSubscriptionMaps() {
    int i = 0, code;
    char PathElt[1+MAXPATHLEN];

    while (MS_GetSearchPathEntry(i++, PathElt, MAXPATHLEN) == 0) {
	code = MS_RebuildOneSubscriptionMap(PathElt);
	if (code && ((errno != EACCES) || (i > 1))) return(code);
    }
    return(0);
}

MS_RebuildOneSubscriptionMap(PathElt)
char *PathElt;
{
    int code;
    char MapFile[MAXPATHLEN+1], RealMapFile[MAXPATHLEN+1], RealPath1[MAXPATHLEN+1], RealPath2[MAXPATHLEN+1], *sdum;
    FILE *mfp;

    debug(1, ("MS_RebuildOneSubscriptionMap: %s\n", PathElt));
    sdum = strrchr(PathElt, '/');
    if (!sdum) {
	AMS_RETURN_ERRCODE(EMSNOTTREEROOT, EIN_PARAMCHECK, EVIA_REBUILDSUBSCRIPTIONMAP)
    }
    if (strncmp(++sdum, MS_TREEROOT, sizeof(MS_TREEROOT)-1)) {
	AMS_RETURN_ERRCODE(EMSNOTTREEROOT, EIN_PARAMCHECK, EVIA_REBUILDSUBSCRIPTIONMAP);
    }
    (void) DeSymLink(PathElt, RealPath1, 0);
    if (abspath(RealPath1, RealPath2) != 0) strcpy(RealPath2, RealPath1);
    sprintf(RealMapFile, "%s/%s", RealPath2, AMS_SUBSCRIPTIONMAPFILE);
    (void) DeSymLink(RealMapFile, RealPath1, 1);
    if (abspath(RealPath1, RealMapFile) != 0) strcpy(RealMapFile, RealPath1);
    strcpy(MapFile, RealMapFile);
    strcat(MapFile, ".NEW");
    debug(4, ("Rebuilding file %s\n", MapFile));
    if ((mfp = fopen(MapFile, "w")) == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_REBUILDSUBSCRIPTIONMAP);
    }
    fchmod(fileno(mfp), 0664);
    if ((code = SubsTreeWalk(mfp, RealPath2)) != 0) {
	fclose(mfp);
	return(code);
    }
    if (ferror(mfp) || feof(mfp)) {
	fclose(mfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_REBUILDSUBSCRIPTIONMAP);
    }
    if (vfclose(mfp) != 0) {
	AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_REBUILDSUBSCRIPTIONMAP);
    }
    if (RenameEvenInVice(MapFile, RealMapFile)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_REBUILDSUBSCRIPTIONMAP);
    }
    return(0);
}    
