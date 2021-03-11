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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getdiri.c,v 2.16 1993/05/06 18:01:40 susan Exp $";
#endif

#include <ms.h>
#include <andrewos.h> /* sys/file.h */
#include <mailconf.h>

extern char home[], MyMailDomain[];

long    MS_GetDirInfo (DirName, ProtCode, MsgCount)
char   *DirName;
int    *ProtCode,
       *MsgCount;
{
    struct MS_Directory *Dir;
    char BoxName[1+MAXPATHLEN], RootName[1+MAXPATHLEN];

    debug(1, ("Entering MS_GetDirInfo %s\n", DirName));
    *ProtCode = AMS_DIRPROT_AWFUL;
    *MsgCount = -42;
    /* We open in MD_READ mode to make sure the count is up to date */
    if (ReadOrFindMSDir(DirName, &Dir, MD_READ)) {
	return(mserrcode);
    }
    CacheDirectoryForClosing(Dir, MD_READ);
    if (!Dir->Writable) {
	if (!strncmp(DirName, LOCALSEARCHPATHTEMPLATE, strlen(LOCALSEARCHPATHTEMPLATE))) {
	    *ProtCode = AMS_DIRPROT_LOCALBB;
	} else if (!strncmp(DirName, EXTERNALSEARCHPATHTEMPLATE, strlen(EXTERNALSEARCHPATHTEMPLATE))) {
	    *ProtCode = AMS_DIRPROT_EXTBB;
	} else if (!strncmp(DirName, OFFICIALSEARCHPATHTEMPLATE, strlen(OFFICIALSEARCHPATHTEMPLATE))) {
	    *ProtCode = AMS_DIRPROT_OFFBB;
	} else {
	    *ProtCode = AMS_DIRPROT_READ;
	}
    } else {
	if (FindTreeRoot(DirName, RootName, FALSE)) AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FINDTREEROOT);
	strncpy(BoxName, RootName, MAXPATHLEN);
	if (TransformPathRootToMailbox(BoxName)) {
	    strncpy(BoxName, RootName, MAXPATHLEN);
	    if (GetAssocMailbox(BoxName)) return (mserrcode);
	}
	debug(4, ("Checking mailbox %s\n", BoxName));
	if (access(BoxName, R_OK)) {
	    if (errno != EACCES && errno != ENOENT) {
		AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_GETDIRINFO);
	    }
	    *ProtCode = AMS_DIRPROT_MODIFY;
	} else {
	    char Tempname[1+MAXPATHLEN];

	    sprintf(Tempname, "%s/.MESSAGES/", home);
	    if (strncmp(Tempname, DirName, strlen(Tempname))) {
		*ProtCode = AMS_DIRPROT_MBOX;
	    } else {
		*ProtCode = AMS_DIRPROT_FULLMAIL;
	    }
	}
    }
    *MsgCount = Dir->MessageCount;
    return(0);
}
