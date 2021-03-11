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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/namemap.c,v 2.16 1993/08/29 16:23:57 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <stdio.h>
#include <ms.h>
#include <sys/stat.h>

extern FILE *fopen();

MS_NameSubscriptionMapFile(Root, MapFile)
char *Root;  /* Parameter passed in from CUI */
char *MapFile; /* Buffer to be filled and sent back by this routine */
{
    char    PublicMap[MAXPATHLEN + 1], File1[MAXPATHLEN+1], File2[MAXPATHLEN+1];
    int     code;
    FILE *mfp, *mmfp;

    debug(1,("MS_NameSubscriptionMapFile %s\n", Root ? Root : "<no root>"));
    if (!Root || !*Root) {
	/* Want a subscription map for everything we subscribe to ONLY */
	GenTempName(MapFile);
	return(WriteSimpleSubsMap(MapFile));
    }
    (void) DeSymLink(Root, File1, 0);
    if (abspath(File1, File2) != 0) strcpy(File2, File1);
    sprintf(PublicMap, "%s/%s", File2, AMS_SUBSCRIPTIONMAPFILE);
    debug(4, ("Now it is time for my private map file\n"));
    GenTempName(MapFile);
    if ((mfp = fopen(PublicMap, "r")) == NULL) {
	if (errno == ENOENT) {
	    debug(4, ("Rebuilding public map file\n"));
	    if ((mfp = fopen(PublicMap, "w")) == NULL) {
		GenTempName(PublicMap);
		debug(4,("Have to rebuild in a temporary spot: %s (errno %d)\n", PublicMap, errno));
		if ((mfp = fopen(PublicMap, "w")) == NULL) {
		    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_NAMESUBSCRIPTIONMAP);
		}
	    }
	    if ((code = SubsTreeWalk(mfp, File2)) != 0) {
		fclose(mfp);
		return(code);
	    }
	    if (ferror(mfp) || feof(mfp)) {
		fclose(mfp);
		AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_NAMESUBSCRIPTIONMAP);
	    }
	    if (vfclose(mfp) != 0) {
		AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_NAMESUBSCRIPTIONMAP);
	    }
	    mfp = fopen(PublicMap, "r");
	}
	if (mfp == NULL) {
	    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_NAMESUBSCRIPTIONMAP);
	}
    }
    if ((mmfp = fopen(MapFile, "w")) == NULL) {
	fclose(mfp);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_NAMESUBSCRIPTIONMAP);
    }
    if ((code = BuildPrivateSubscriptionMap(mfp, mmfp, File2)) != 0) {
	fclose(mfp);
	fclose(mmfp);
	return(code);
    }
    if (ferror(mmfp) || feof(mmfp)) {
	fclose(mmfp);
	fclose(mfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_NAMESUBSCRIPTIONMAP);
    }
    if (vfclose(mmfp) != 0) {
	fclose(mfp);
	AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_NAMESUBSCRIPTIONMAP);
    }
    if (ferror(mfp)) {
	fclose(mfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_NAMESUBSCRIPTIONMAP);
    }
    if (vfclose(mfp) != 0) {
	AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_NAMESUBSCRIPTIONMAP);
    }
    return(0);
}
