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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/thiscell.c,v 2.15 1993/05/04 00:53:32 susan Exp $";
#endif


 

#include <andyenv.h>
#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>		/* strings.h */
#include <util.h>
#include <svcconf.h>
#include <errno.h>

#ifdef AFS_ENV
#include <afs/param.h>
#include <rx/xdr.h>
#include <sys/ioctl.h>
#include <afs/vice.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#endif /* AFS_ENV */

#define	MAXSIZE		200

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern int errno;

/*
  * GetCurrentWSCell
  *
  * Description:
  *	Return the string name of the cell to which the current workstation belongs.
  *
  * Arguments:
  *	Buf	: Pointer to character buffer in which we'll place the name of
  *		    the workstation's cell.
  *	size	: The max size available in the buffer.
  *
  * Returns:
  *	Any of the standard system error return codes.  Expected values are:
  *		0	    if everything went well.
  *		EINVAL	    if the AFS basename used was wrong.
  *		ENOENT   ditto
  *		EDOM	    if there wasn't enough room in the user's buffer.
  *
  * Side Effects:
  *	Nothing interesting.
  */

int GetCurrentWSCell(Buf, size) 
char *Buf;
int   size;
{
    char *CellName;
    long code;			/* Result of pioctl() call */
#ifdef AFS_ENV
    struct ViceIoctl blob;		/* I/O control area for Venus communication */
    char vcommbuff[MAXSIZE];	/* Communication buffer */
#endif /* AFS_ENV */

    CheckServiceConfiguration();
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	/*
	 * Prepare the pioctl I/O block.
	 */
	blob.in_size  = 0;
	blob.in       = 0;
	blob.out_size = MAXSIZE;
	blob.out      = vcommbuff;

	code = pioctl(ViceFile, _VICEIOCTL(31), &blob, 1);
	CellName = vcommbuff;
    } else
#endif /* AFS_ENV */
    {
	CellName = ThisDomain;
	code = 0;
    }

    if (code == 0) {
	/*
	 * Copy out the cell name into the supplied buffer if there's room.
*/
	if (size > strlen(CellName))
	    strcpy(Buf, CellName);
	else {
	    Buf[0] = '\0';    /*Put null string in buffer */
	    code = EDOM;
	}
    } else if (code < 0) code = errno;
    return(code);
}

/*
  * GetCellFromFileName
  *
  * Description:
  *	Given a file name, return the string name of the cell in which that file resides.
  *
  * Arguments:
  *	FileName    : Name of the file to locate.
  *	Buf	    : Pointer to character buffer in which we'll place the name of
  *			the cell hosting the above file.
  *	size	    : The max size available in the buffer.
  *
  * Returns:
  *	Any of the standard system error return codes.  Expected values are:
  *		0	    if everything went well.
  *		EINVAL	    if the AFS basename used was wrong.
  *		ENOENT   ditto, or if the file couldn't be found in AFS.
  *		EDOM	    if there wasn't enough room in the user's buffer.
  *
  * Side Effects:
  *	Nothing interesting.
  */

int GetCellFromFileName(FileName, Buf, size)
char *FileName;
char *Buf;
int   size;
{
    char *CellName;
    long code;			/* Result of pioctl() call */
#ifdef AFS_ENV
    struct ViceIoctl blob;		/* I/O control area for Venus communication */
    char vcommbuff[MAXSIZE];	/* Communication buffer */
#endif /* AFS_ENV */

    CheckServiceConfiguration();
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	/* Prepare the pioctl I/O block. */
	blob.in_size  = strlen(FileName) + 1;
	blob.in       = FileName;
	blob.out_size = MAXSIZE;
	blob.out      = vcommbuff;

	code = pioctl(FileName, _VICEIOCTL(30), &blob, 1);
	CellName = vcommbuff;
    } else
#endif /* AFS_ENV */
    {
	CellName = ThisDomain;
	code = 0;
    }

    if (code == 0) {
	/*
	 * Copy out the file's host cell name into the supplied buffer if there's room.
	 */
	if (size > strlen(CellName))
	    strcpy(Buf, CellName);
	else {
	    Buf[0] = '\0';  /*Put null string in buffer */
	    code = EDOM;
	}
    } else if (code < 0) code = errno;
    return(code);
}

#ifdef TESTINGONLYTESTING
main()
{
    char Foo[5000];
    int retcode, Ix;
    static char *FNs[] = {"/afs/andrew.cmu.edu/service/mailqs", "/afs/.psy.cmu.edu", "/afs/nosuchthing", "/tmp"};
#define numFNs	(sizeof(FNs) / sizeof(FNs[0]))

    fprintf(stderr, "\nTesting the pioctl support for Cellular AMS.\n\n");

    fprintf(stderr, "Getting this workstation's home cell...\n");
    retcode = GetCurrentWSCell(Foo, sizeof(Foo));
    if (retcode == 0)
	fprintf(stderr, "\tHome cell is '%s'\n", Foo);
    else
	fprintf(stderr, "GetCurrentWSCell: %s\n", UnixError(retcode));

    fprintf(stderr, "Finding out where files live...\n");

    for (Ix = 0; Ix < numFNs; ++Ix) {
	retcode = GetCellFromFileName(FNs[Ix], Foo, sizeof(Foo));
	if (retcode == 0)
	    fprintf(stderr, "\t%s:\t'%s'\n", FNs[Ix], Foo);
	else
	    fprintf(stderr, "GetCellFromFileName(``%s''): %s\n",
		    FNs[Ix], UnixError(retcode));
    }
}
#endif /* TESTINGONLYTESTING */
