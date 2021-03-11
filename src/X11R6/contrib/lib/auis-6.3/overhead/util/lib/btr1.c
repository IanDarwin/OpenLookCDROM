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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btr1.c,v 2.13 1993/09/23 19:49:08 gk5g Exp $";
#endif

/* ************************************************************ *\
	btr1.c
	More library routines for reading B-trees.
	Include file ``bt.h'' declares the procedures for clients.
	Include file ``btint.h'' declares common structures for the implementation modules.
\* ************************************************************ */

#include <andrewos.h>	/* file, time, strings */
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

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

extern int br_Debugging;

/* Advance the cursor to the next key-value pair.
Declaration:
	extern bt_ErrorCode bt_NextCursor(curs);
	struct btCursor *curs;
*/
bt_ErrorCode bt_NextCursor(curs)
struct btCursor *curs;
{
    struct btC *bC = (struct btC *) curs;
    int Idx, Exact, Flags, RootNameLength, ValueLength, ThisByte;
    bt_ErrorCode RetVal;
    auto char NodeFileName[MAXPATHLEN+1];
    char *NFNEnd, *NFNPtr, *OldKey;
    struct btFile *bF;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    if (bC->State == UnInitialized) return bterr_UninitializedCursor;
    if (bC->FP == NULL || bC->FP->Tag != btFileTag) return bterr_CursorTreeDamaged;
    if (bC->FP->Head.BTDepth != 0) return bterr_NotAtKey;
    switch (bC->State) {
	case Null: return bterr_EmptyTree;
	case BeforeFirst: {
	    /*			if (bC->FP->IndexNum == 0)
	    {bC->State = Null; return bterr_EmptyTree;} */
	    Idx = 0;
	    break;
	    }
	case AfterLast: return bterr_NoNextKey;
	default: {Idx = bC->IndexPos + 1; break;}
    }
    if (Idx < bC->FP->IndexNum) {   /* the typical case: see if next key is a brother link */
	RetVal = b_GetFlags(bC->FP, Idx, &Flags);
	if (RetVal != bterr_NoError) return RetVal;
	if ((Flags & BTIsBrotherLink) == 0) {
	    bC->IndexPos = Idx;
	    bC->State = AtKey;
	    return bterr_NoError;
	}
    } else {			/* see if the old (current) key was a brother link */
	--Idx;
	RetVal = b_GetFlags(bC->FP, Idx, &Flags);
	if (RetVal != bterr_NoError) return RetVal;
	if ((Flags & BTIsBrotherLink) == 0) {
	    bC->State = AfterLast;	/* was no brother link */
	    return bterr_NoNextKey;
	}
    }
    /* OK, follow the brother link.  First we copy the key for the brother link. */
    bC->State = AtKey; bC->IndexPos = Idx;	/* Get the brother-link key */
    RetVal = bt_GetCursorKey(curs, &OldKey);	/* copy key for searching next node */
    if (RetVal != bterr_NoError) return RetVal;
    /* Follow a right-brother link */
    strcpy(NodeFileName, bC->Tree->Root->FileName);
    RootNameLength = strlen(NodeFileName);
    NFNEnd = &NodeFileName[RootNameLength];
    *NFNEnd++ = '.';
    for (;;) {
	RetVal = b_GetValueLength(bC->FP, Idx, &ValueLength);
	if (RetVal != bterr_NoError) {free(OldKey); return RetVal;}
	if (ValueLength <= 0) {free(OldKey); return bterr_BTreeDamaged;}
	if (ValueLength + RootNameLength >= MAXPATHLEN)
	{free(OldKey); return bterr_IntermediateNameTooLong;}
	NFNPtr = NFNEnd;
	while ((--ValueLength) >= 0) {
	    ThisByte = fgetc(bC->FP->File);
	    if (ThisByte == EOF) {free(OldKey); return bterr_BTreeDamaged;}
	    *NFNPtr++ = ThisByte;
	}
	*NFNPtr = '\0';		/* terminate it */
	bF = b_NewbtFileStr();
	if (bF == NULL) {free(OldKey); return bterr_OutOfMemory;}
	bF->RefCount = 1;		/* preset the ref count */
	RetVal = b_ReadbtFile(bF, NodeFileName, FALSE);
	if (RetVal != bterr_NoError) {free(bF); free(OldKey); return RetVal;}
	if (bF->Head.BTDepth != 0)
	{b_DecrRefCount(&bF); free(OldKey); return bterr_BTreeDamaged;}
	b_StoreFilePtr(&bC->FP, bF);
	b_DecrRefCount(&bF);	/* and decrement it when safe */
	RetVal = b_ScanNode(bC->FP, OldKey, &Idx, &Exact, &Flags);
	if (RetVal != bterr_NoError) {free(OldKey); return RetVal;}
	if (Exact == 0) {	/* we landed between keys; advance. */
	    if ((Idx+1) < bC->FP->IndexNum) {	/* not looking past last entry */
		++Idx;
		RetVal = b_GetFlags(bC->FP, Idx, &Flags);
		if (RetVal != bterr_NoError) {free(OldKey); return RetVal;}
	    } else {
		if ((Flags & BTIsBrotherLink) == 0) {
		    bC->State = AfterLast;
		    free(OldKey);
		    return bterr_NoNextKey;
		}
	    }
	}
	if (Idx < 0 || (Flags & BTIsBrotherLink) == 0) {
	    bC->IndexPos = Idx;
	    if (bC->IndexPos < 0) bC->IndexPos = 0;
	    bC->State = AtKey;
	    free(OldKey);
	    if (Idx >= 0 && (Flags & BTIsLeafPair) == 0)
		return bterr_BTreeDamaged;
	    else return bterr_NoError;
	} /* else is another brother link--search it, too. */
    }
}
