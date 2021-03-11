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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/instinfo.c,v 2.12 1993/05/04 00:58:50 susan Exp $";
#endif

#include <ms.h>
#include <andrewos.h> /* sys/file.h */
#include <mailconf.h>
#include <hdrparse.h>
#include <stdio.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern char Me[], *MyPrettyAddress, home[];

MS_InstallWelcomeMessage(ParentName, InitDir, InitFile, ShortName)
char *ParentName, *InitDir, *InitFile, *ShortName;
{
    char DirName[MAXPATHLEN+1], SourceFileName[1+MAXPATHLEN], LineBuf[1000];
    FILE *rfp, *wfp;
    Boolean AtStart = TRUE;

    debug(1, ("MS_InstallWelcome Message %s %s %s\n", ParentName, InitFile, ShortName));
    sprintf(DirName, "%s/%s", ParentName, ShortName);
    if (InitDir && *InitDir) {
	sprintf(SourceFileName, "%s/+%s", InitDir, InitFile);
	if (AppendFileToFolder(SourceFileName, DirName, FALSE)) {
	    return(mserrcode);
	}
    } else {
	GenTempName(SourceFileName);
	rfp = fopen(InitFile, "r");
	if (!rfp) {
	    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ADDPARENTAL);
	}
	wfp = fopen(SourceFileName, "w");
	if (!wfp) {
	    fclose(rfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ADDPARENTAL);
	}
	fprintf(wfp, "Date: %sFrom:%s\nSubject: Welcome to %s\n", arpadate(), MyPrettyAddress, ShortName);
	while (fgets(LineBuf, sizeof(LineBuf), rfp)) {
	    if (AtStart) {
		if (LineBuf[0] == '\\') {
		    /* Bogus hardwiring of format 12 */
		    fprintf(wfp, "Content-type: X-BE2; 12\n\n");
		} else {
		    fprintf(wfp, "\n");
		}
		AtStart = FALSE;
	    }
	    fputs(LineBuf, wfp);
	}
	fclose(rfp);
	if (ferror(rfp) || feof(rfp)) {
	    fclose(rfp);
	    AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_ADDPARENTAL);
	}
	if (vfclose(wfp)) {
	    unlink(SourceFileName);
	    AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_ADDPARENTAL);
	}
	if (AppendFileToFolder(SourceFileName, DirName, FALSE)) {
	    unlink(SourceFileName);
	    return(mserrcode);
	}
    }
    if (AddParentalMessageFromFile(SourceFileName, ParentName, ShortName, DirName)) {
	if (!InitDir || !*InitDir) unlink(SourceFileName);
	AMS_RETURN_ERRCODE(EMSNOPARENT, AMS_ERRCAUSE, AMS_ERRVIA);
    }
    if (!InitDir || !*InitDir) {
	unlink(SourceFileName);
    }
    return(0);
}
