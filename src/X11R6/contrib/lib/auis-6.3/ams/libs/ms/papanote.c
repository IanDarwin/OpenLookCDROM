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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/papanote.c,v 2.20 1993/06/22 22:06:57 gk5g Exp $";
#endif

#include <ms.h>
#include <hdrparse.h>
#include <andrewos.h> /* sys/file.h */
#include <stdio.h>

extern char home[];

AddParentalMessage(Msg, PDirName, bbname, bbpath)
struct MS_Message *Msg;
char *PDirName, *bbname, *bbpath;
{
    char TempFile[1+MAXPATHLEN];

    GenTempName(TempFile);
    if (WritePureFile(Msg, TempFile, TRUE, 0664)) {
	return(mserrcode);
    }
    mserrcode = AddParentalMessageFromFile(TempFile, PDirName, bbname, bbpath);
    unlink(TempFile);
    return(mserrcode);
}

AddParentalMessageFromFile(FileName, PDirName, bbname, bbpath)
char *FileName, *PDirName, *bbname, *bbpath;
{
    char NewFileName[MAXPATHLEN+1], ThisFormat[50], LineBuf[1000];
    struct MS_Message *Msg;
    int IsBE2;
    FILE *rfp, *wfp;
    char boundary[50];
#ifdef INHIBIT_MIME_GENERATION
    Boolean InHead = TRUE, AtStart = TRUE;
#endif /* INHIBIT_MIME_GENERATION */

    debug(1, ("Add parental message:  pdir %s bbname %s bbpath %s\n", PDirName, bbname, bbpath));
    if (FindTreeRoot(PDirName, NewFileName, FALSE)) AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FINDTREEROOT);
    if (!strcmp(PDirName, NewFileName)) {
	debug(1, ("This is top-level, no parent note will be added\n"));
	return(0);
    }
    ReduceSlashes(bbpath);
    if ((Msg = (struct MS_Message *) malloc (sizeof (struct MS_Message))) == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_ADDPARENTAL);
    }
    bzero(Msg, sizeof(struct MS_Message));
    Msg->OpenFD = -1;
    if (ReadRawFile(FileName, Msg, FALSE)
    || ParseMessageFromRawBody(Msg)) {
	FreeMessage(Msg, TRUE);
	return(mserrcode);
    }
    GetFormatFromMessage(Msg, ThisFormat, sizeof(ThisFormat), &IsBE2) ;
    FreeMessage(Msg, TRUE);
    GenTempName(NewFileName);
    rfp = fopen(FileName, "r");
    if (!rfp) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ADDPARENTAL);
    }
    wfp = fopen(NewFileName, "w");
    if (!wfp) {
	fclose(rfp);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_ADDPARENTAL);
    }
    fprintf(wfp, "Subject: %s (new message group)\nX-Andrew-DirectoryCreation: %s\n", bbname, bbpath);
#ifndef INHIBIT_MIME_GENERATION
    sprintf(boundary, "Boundary_%s", ams_genid(1));
    fprintf(wfp, "Content-type: multipart/mixed; boundary=%s\n\n", boundary);
    fprintf(wfp, "\n--%s\n\nA new message group, %s, has been created as a subdirectory of this message group.\n\nA copy of the first message in the new group is reproduced below.\n\nIf it interests you, you may want to subscribe to the new message group.\n\n--%s\nContent-type: message/rfc822\n\n", boundary, bbname, boundary);
#endif /* INHIBIT_MIME_GENERATION */
    while (fgets(LineBuf, sizeof(LineBuf), rfp)) {
#ifdef INHIBIT_MIME_GENERATION
	if (InHead) {
	    if (LineBuf[0] == '\n') InHead = FALSE;
	} else if (AtStart) {
	    if (!IsBE2 || ((LineBuf[0] != '\\') && (LineBuf[0] != '}'))) {
		AtStart = FALSE;
		fprintf(wfp, "\n\nA new message group, %s, has been created as a subdirectory of this message group.\n\nA copy of the first message in the new group is reproduced below.\n\nIf it interests you, you may want to subscribe to the new message group.\n\n============================================================================\n\n", bbname);
	    }
	}
#endif /* INHIBIT_MIME_GENERATION */
	fputs(LineBuf, wfp);
    }	
    fclose(rfp);
#ifndef INHIBIT_MIME_GENERATION
    fprintf(wfp, "\n--%s\n\n", boundary);
#endif /* INHIBIT_MIME_GENERATION */
    if (ferror(wfp) || feof(wfp)) {
	fclose(wfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_ADDPARENTAL);
    }
    if (vfclose(wfp)) {
	unlink(NewFileName);
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_ADDPARENTAL);
    }
    if (AppendFileToFolder(NewFileName, PDirName, TRUE)) {
	unlink(NewFileName);
	return(mserrcode);
    }
    unlink(NewFileName);
    return(0);

}

