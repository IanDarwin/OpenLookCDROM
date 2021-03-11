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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btr3.c,v 2.13 1993/09/23 19:53:03 gk5g Exp $";
#endif

/* ************************************************************ *\
	btr3.c
	More library routines for reading B-trees.
	Include file ``bt.h'' declares the procedures for clients.
	Include file ``btint.h'' declares common structures for the implementation modules.
\* ************************************************************ */

#include <andrewos.h> 	/* file, time, strings */
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */
#include <sys/stat.h>
#include <netinet/in.h>
#include <util.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include <btint.h>
#endif /* WHITEPAGES_ENV   */

extern int errno;

extern int br_Debugging;

static bt_ErrorCode JustMove(bC, ToEnd)
struct btC *bC;
int ToEnd;	/* whether to beginning or to end */
{
    int Idx, Flags, TreeDepth, RootNameLength, ValueLength, ThisByte;
    bt_ErrorCode RetVal;
    auto char NodeFileName[MAXPATHLEN+1];
    char *NFNEnd, *NFNPtr;
    struct btFile *bF;

    b_StoreFilePtr(&bC->FP, bC->Tree->Root);
    TreeDepth = bC->FP->Head.BTDepth;
    strcpy(NodeFileName, bC->Tree->Root->FileName);
    RootNameLength = strlen(NodeFileName);
    NFNEnd = &NodeFileName[RootNameLength];
    *NFNEnd++ = '.';
    for (;;) {
	if (bC->FP->IndexNum == 0) {
	    bC->IndexPos = 0;
	    bC->State = Null;
	    return bterr_NoError;
	}
	Idx = (ToEnd ? bC->FP->IndexNum - 1 : 0);
	RetVal = b_GetFlags(bC->FP, Idx, &Flags);
	if (RetVal != bterr_NoError) return RetVal;
	if (TreeDepth == 0 && (Flags & BTIsBrotherLink) == 0) {
	    bC->IndexPos = MAX(Idx, 0);
	    if (ToEnd) bC->State = AfterLast;
	    else bC->State = BeforeFirst;
	    if ((Flags & BTIsLeafPair) == 0) return bterr_BTreeDamaged;
	    else return bterr_NoError;
	}
	if ((Flags & BTIsLeafPair) != 0) return bterr_BTreeDamaged;
	RetVal = b_GetValueLength(bC->FP, Idx, &ValueLength);
	if (RetVal != bterr_NoError) return RetVal;
	if (ValueLength <= 0) return bterr_BTreeDamaged;
	if (ValueLength + RootNameLength >= (MAXPATHLEN-1))
	    return bterr_IntermediateNameTooLong;
	NFNPtr = NFNEnd;
	while ((--ValueLength) >= 0) {
	    ThisByte = fgetc(bC->FP->File);
	    if (ThisByte == EOF) return bterr_BTreeDamaged;
	    *NFNPtr++ = ThisByte;
	}
	*NFNPtr = '\0';		/* terminate it */
	bF = b_NewbtFileStr();
	if (bF == NULL) return bterr_OutOfMemory;
	bF->RefCount = 1;
	RetVal = b_ReadbtFile(bF, NodeFileName, FALSE);
	if (RetVal != bterr_NoError) {free(bF); return RetVal;}
	if ((Flags & BTIsBrotherLink) == 0) --TreeDepth;
	if (bF->Head.BTDepth != TreeDepth)
	{b_DecrRefCount(&bF); return bterr_BTreeDamaged;}
	b_StoreFilePtr(&bC->FP, bF);
	b_DecrRefCount(&bF);
    }
}

/* Make a new cursor that's a copy of the old one, pointing to the same place in the same file.
Declaration:
	extern bt_ErrorCode bt_CopyCursor(cursptr, curs);
	struct btCursor **cursptr, *curs;
*/
bt_ErrorCode bt_CopyCursor(cursptr, curs)
struct btCursor **cursptr, *curs;
{
    struct BTr *bt;
    struct btC *NewbC;
    struct btC *bC = (struct btC *) curs;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    bt = bC->Tree;
    if (bt->Root == NULL) return bterr_NotABTree;
    NewbC = (struct btC *) malloc(sizeof(struct btC));
    if (NewbC == NULL) return bterr_OutOfMemory;
    NewbC->Tag = btCTag;
    NewbC->Tree = bt;
    NewbC->FP = NULL;
    b_StoreFilePtr(&NewbC->FP, bC->FP);
    NewbC->IndexPos = bC->IndexPos;
    NewbC->State = bC->State;
    /* Now link it in to the chain hanging off of the B-tree record itself */
    NewbC->Next = bt->Cursors;
    bt->Cursors = NewbC;

    *cursptr = (struct btCursor *) NewbC;
    return bterr_NoError;
}

/* Move an existing cursor to before the start or past the end of the database.
  Declarations:
  extern bt_ErrorCode bt_CursorToStart(curs);
  struct btCursor *curs;
  extern bt_ErrorCode bt_CursorToEnd(curs);
  struct btCursor *curs;
  */
bt_ErrorCode bt_CursorToStart(curs)
struct btCursor *curs;
{
    struct btC *bC = (struct btC *) curs;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    return JustMove(bC, FALSE);
}

bt_ErrorCode bt_CursorToEnd(curs)
struct btCursor *curs;
{
    struct btC *bC = (struct btC *) curs;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    return JustMove(bC, TRUE);
}
