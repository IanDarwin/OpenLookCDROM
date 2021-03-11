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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btcreate.c,v 2.16 1993/09/23 19:43:52 gk5g Exp $";
#endif

/* ************************************************************ *\
	btcreate.c
	Library routines for creating new B-trees.
	Include file ``bt.h'' declares the procedures for clients.
	Include file ``btint.h'' declares common structures for the implementation modules.
\* ************************************************************ */

#include <andrewos.h>
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */
#include <sys/stat.h>
#include <netinet/in.h>
#include <util.h>
#ifdef WHITEPAGES_ENV /* avoid makedepend "errors" */
#include <btint.h>
#endif /* WHITEPAGES_ENV  */
extern int errno;

static int Debugging;

int btcreate_SetDebugging(level)
int level;
{
    int OldLevel;
    OldLevel = Debugging;
    Debugging = level;
    return OldLevel;
}

/* Create a new, empty b-tree.
Declaration:
	extern bt_ErrorCode bt_Create(path, MaxFileSize, LockStyle, NULL);
	char *path;		path to target file name for root file
	int MaxFileSize;		approximate maximum file size in bytes
	int LockStyle;	how to do locking on this structure
NULL terminates the argument list.
*/
bt_ErrorCode bt_Create(path, MaxFileSize, LockStyle, Dum)
char *path;
int MaxFileSize, LockStyle;
int *Dum;
{
    /* Create a new b-tree. */

    struct btFile *bF;
    bt_ErrorCode RetVal;
    int fid, Chunk;

    if (Dum != NULL) return bterr_NotABTree;
    RetVal = b_InitbtFileStr(&bF);
    if (RetVal != bterr_NoError) return RetVal;
    bF->Head.BTMaxFileSize = MaxFileSize;
    bF->Head.BTLockStyle = LockStyle;
    bF->FileName = NewString(path);
    if (bF->FileName == NULL) {free(bF); return bterr_OutOfMemory;}
    RetVal = b_AddIndex(bF, 0);	/* define no cells there */
    if (RetVal != bterr_NoError) {free(bF->FileName); free(bF); return RetVal;}
    if (Debugging) fprintf(stderr, "Trying to open file ``%s'' just for reading:\n", bF->FileName);
    fid = open(bF->FileName, O_RDONLY, 0644);
    Chunk = errno;
    if (fid >= 0 || Chunk != ENOENT) {
	if (fid >= 0) close(fid);
	free(bF->FileName); free(bF->Index); free(bF);
	return bterr_FileSystemErrorBegin + (fid >= 0 ? EEXIST : Chunk);
    }
    if (Debugging) fprintf(stderr, "Trying to open file ``%s'' for reading and writing:\n", bF->FileName);
    fid = open(bF->FileName, O_RDWR | O_CREAT, 0644);
    if (fid < 0) {
	Chunk = errno;
	free(bF->FileName); free(bF->Index); free(bF);
	return (bterr_FileSystemErrorBegin + Chunk);
    }
    if (Debugging) fprintf(stderr, "Got a file (``%s'') open for reading and writing.\n", bF->FileName);
    Chunk = osi_ExclusiveLockNoBlock(fid);	/* create it and lock it */
    if (Chunk != 0) {
	Chunk = errno;
	VenusCancelStore(fid);
	close(fid);
	free(bF->FileName); free(bF->Index); free(bF);
	return (Chunk == EACCES ?
		bterr_NoLockPermission :
		bterr_FileSystemErrorBegin + Chunk);
    }
    if (Debugging) fprintf(stderr, "File is now locked.\n");
    bF->File = fdopen(fid, "r+");
    if (bF->File == NULL) {
	unlink(bF->FileName); close(fid);
	free(bF->FileName); free(bF->Index); free(bF);
	return bterr_OutOfMemory;
    }
    if (Debugging) fprintf(stderr, "fdopen succeeded.\n");
    bF->FileOrigin = ftell(bF->File);
    RetVal = b_WriteHeadIndex(bF);
    if (RetVal != bterr_NoError) {
	free(bF->FileName); free(bF->Index); free(bF);
	return RetVal;
    }
    if (Debugging) fprintf(stderr, "Returned from b_WriteHeadIndex.\n");
    if (vfclose(bF->File) == EOF) {
	Chunk = errno;
	unlink(bF->FileName);
	free(bF->FileName); free(bF->Index); free(bF);
	return (bterr_FileSystemErrorBegin + Chunk);
    }
    free(bF->FileName); free(bF->Index); free(bF);

    return bterr_NoError;
}
