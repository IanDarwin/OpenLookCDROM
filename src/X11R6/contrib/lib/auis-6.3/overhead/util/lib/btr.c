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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btr.c,v 2.20 1993/09/23 19:46:18 gk5g Exp $";
#endif

/* ************************************************************ *\
	btr.c
	Library routines for reading B-trees.
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

int br_Debugging = 0;

int btr_SetDebugging(level)
int level;
{
    int OldLevel;
    OldLevel = br_Debugging;
    br_Debugging = level;
    return OldLevel;
}

unsigned short b_ReadNetShort(f)
FILE *f;
{	/* Read an unsigned short from file ``f'' in network byte order. */
    unsigned short ns;

    fread((char*)&ns, sizeof(ns), 1, f);
    return ((unsigned short) ntohs((short) ns));
}

unsigned long b_ReadNetLong(f)
FILE *f;
{	/* Read an unsigned long from file ``f'' in network byte order. */
    unsigned long nl;

    fread((char*)&nl, sizeof(nl), 1, f);
    return ((unsigned long) ntohl((long) nl));
}

struct btFile *b_NewbtFileStr()
{
    struct btFile *bF;

    bF = (struct btFile *) malloc(sizeof(struct btFile));
    if (bF == NULL) return NULL;
    bF->Tag = btFileTag;
    bF->FileName = NULL;
    bF->File = NULL;
    bF->Index = NULL;		/* should be changed by caller */
    bF->FileOrigin = 0;
    bF->IndexAlloc = 0;		/* same */
    bF->IndexNum = 0;		/* same */
    bF->RefCount = 0;			/* should be incremented by caller */
    bF->FileStat.st_dev = bF->FileStat.st_ino = 0;	/* just in case */

    return bF;
}

bt_ErrorCode b_ReadbtFile(bF, path, WantLock)
struct btFile *bF;
char *path;
int WantLock;
{
    int Res, MinHead, ErrVal;

    bF->FileName = NewString(path);
    if (bF->FileName == NULL) return bterr_OutOfMemory;
    for (;;) {
	errno = 0;
	bF->File = fopen(path, (WantLock ? osi_F_READLOCK : "r"));
	if (bF->File == NULL) {
	    free(bF->FileName); bF->FileName = NULL;
	    if (errno == 0) return bterr_OutOfMemory;
	    return bterr_FileSystemErrorBegin + errno;
	}
	if (! WantLock) break;	/* usual case--don't bother to lock */
	Res = osi_ExclusiveLockNoBlock(fileno(bF->File));
	if (Res == 0) break;	/* usual locking case--all OK */
	if (errno != EWOULDBLOCK && errno != EINVAL) {
	    free(bF->FileName); bF->FileName = NULL;
	    if (errno == EACCES) return bterr_NoLockPermission;
	    return bterr_FileSystemErrorBegin + errno;
	}
	/* AFS semantics: close file, re-open (to get fresh copy), and try again. */
	fclose(bF->File);	/* now loop back to try again */
    }
    bF->FileOrigin = ftell(bF->File);
    if (fstat(fileno(bF->File), &bF->FileStat) != 0)
    {free(bF->FileName); bF->FileName = NULL; fclose(bF->File);
    return bterr_FileSystemErrorBegin + errno;}
    if (bF->FileStat.st_size < 22*sizeof(unsigned long))
    {fclose(bF->File); free(bF->FileName); bF->FileName = NULL; return bterr_NotABTree;}
    errno = 0;
    for (Res = 0; Res <= BTFixedHeadSizeOffset; Res++) {
	/* get first [0-12] words (to get BTFixedHeadSize word) */
	bF->Head.BTarr[Res] = b_ReadNetLong(bF->File);
    }
    if (feof(bF->File) || ferror(bF->File)) {
	Res = errno;
	fclose(bF->File); free(bF->FileName); bF->FileName = NULL;
	return (bterr_FileSystemErrorBegin + 	(Res == 0 ? EIO : Res));
    }
    if (bF->Head.BTSignature != BTSignatureValue
	 || bF->Head.BTFixedHeadSize < 22)
    {fclose(bF->File); free(bF->FileName); bF->FileName = NULL; return bterr_NotABTree;}
    MinHead = MIN(BTarrSIZE, bF->Head.BTFixedHeadSize);
    for (Res = BTFixedHeadSizeOffset+1; Res < MinHead; Res++) {	/* get rest of header */
	bF->Head.BTarr[Res] = b_ReadNetLong(bF->File);
    }
    for (Res = MinHead; Res < BTarrSIZE; Res++) bF->Head.BTarr[Res] = 0;
    if (feof(bF->File) || ferror(bF->File)) {
	Res = errno;
	fclose(bF->File); free(bF->FileName); bF->FileName = NULL;
	return (bterr_FileSystemErrorBegin + 	(Res == 0 ? EIO : Res));
    }
    if (bF->Head.BTVersion != ThisBTVersion)
    {fclose(bF->File); free(bF->FileName); bF->FileName = NULL; return bterr_BTreeNotCurrVersion;}
    if ((bF->Head.BTKVFF != 0 && bF->Head.BTKVFF > (bF->FileStat.st_size+1))
	 || (bF->Head.BTIndexStart != 0 && bF->Head.BTIndexStart > bF->FileStat.st_size)
	 ||  bF->Head.BTIndexCount > bF->Head.BTIndexSize)
    {fclose(bF->File); free(bF->FileName); bF->FileName = NULL; return bterr_BTreeDamaged;}
    if (bF->Head.BTIndexSize)
	bF->Index = (unsigned long *) malloc(bF->Head.BTIndexSize * sizeof(unsigned long));
    else
	bF->Index = (unsigned long *) malloc(1 * sizeof(unsigned long));
    if (bF->Index == NULL)
    {fclose(bF->File); free(bF->FileName); bF->FileName = NULL; return bterr_OutOfMemory;}
    errno = 0;
    Res = fseek(bF->File, bF->FileOrigin + bF->Head.BTIndexStart, 0);
    ErrVal = errno;
    if (Res != 0 || feof(bF->File) || ferror(bF->File)) {
	free(bF->Index);
	fclose(bF->File);
	free(bF->FileName); bF->FileName = NULL;
	return (bterr_FileSystemErrorBegin + 	(ErrVal == 0 ? EIO : ErrVal));
    }
    errno = 0;
    for (Res = 0; Res < bF->Head.BTIndexSize; Res++)
	bF->Index[Res] = b_ReadNetLong(bF->File);
    ErrVal = errno;
    if (feof(bF->File) || ferror(bF->File)) {
	free(bF->Index);
	fclose(bF->File);
	free(bF->FileName); bF->FileName = NULL;
	return (bterr_FileSystemErrorBegin + 	(ErrVal == 0 ? EIO : ErrVal));
    }
    bF->IndexAlloc = bF->Head.BTIndexSize;
    bF->IndexNum = bF->Head.BTIndexCount;

    return bterr_NoError;
}

bt_ErrorCode b_ScanNode(bF, Key, IdxIdxPtr, WasExactPtr, FlagsPtr)
struct btFile *bF;
unsigned char *Key;
int *IdxIdxPtr, *WasExactPtr, *FlagsPtr;
{	/* Search the file *bF for the key Key.  Return via IdxIdx the index (in bF->Index) of the largest entry that is less than or equal to the given Key.  Return via WasExact whether the match was exact.  Return via Flags the flags for the matching entry. */

#define	DFSiz	50	/* must be greater than 2 */
    int     LowBd, UppBd, Mid, Res;
    unsigned char	*KeyP, DataFrag[DFSiz], *DataFragPtr;
    int	KeyLeft, KeyLen, DataFragLen, ThisGo, Flags;
    int	LowFlags = -1;	/* Flags for entry at LowBd-1 */

    /* Invariant: key > all elements in [0..LowBd) and key < all elements in
(UppBd..maxIx]. */
    LowBd = 0;
    UppBd = bF->IndexNum - 1;
    KeyLen = strlen((char*)Key) + 1;	/* read match for trailing NUL, too */
    while (LowBd <= UppBd) {
	Mid = (LowBd + UppBd) / 2;
	errno = 0;
	if (fseek(bF->File, bF->FileOrigin + bF->Index[Mid], 0) != 0)
	    return (bterr_FileSystemErrorBegin + (errno==0 ? EIO : errno));
	errno = 0;
	Res = fread((char*)&DataFrag[0], 1, DFSiz, bF->File);
	if (Res < 0 || ferror(bF->File))
	    return (bterr_FileSystemErrorBegin + (errno==0 ? EIO : errno));
	if (Res < 3 || DataFrag[0] != BTSeparatorByte)
	    return bterr_BTreeDamaged;
	Flags = DataFrag[1];
	DataFragPtr = &DataFrag[2];
	DataFragLen = Res - 2;
	KeyP = Key;
	KeyLeft = KeyLen;
	while (1) {
	    ThisGo = MIN(KeyLeft, DataFragLen);
	    Res = strncmp((char*)KeyP, DataFragPtr, ThisGo);
	    if (Res != 0) {
		if (Res < 0) UppBd = Mid - 1;
		else {LowBd = Mid + 1; LowFlags = Flags;}
		break;	/* return to enclosing While-loop */
	    }
	    /* ok, strings are equal as far as they go */
	    if ((KeyLeft -= ThisGo) == 0) {	/* end of string, incl. NUL */
		*IdxIdxPtr = Mid;
		*WasExactPtr = TRUE;
		*FlagsPtr = Flags;
		return bterr_NoError;
	    }
	    KeyP += ThisGo;	/* not end of key--keep reading */
	    DataFragPtr = &DataFrag[0];
	    errno = 0;
	    Res = fread((char*)DataFragPtr, 1, DFSiz, bF->File);
	    if (Res <= 0 || ferror(bF->File))
		return (bterr_FileSystemErrorBegin +
			(errno==0 ? EIO : errno));
	    DataFragLen = Res;	/* how many bytes we got */
	}
    }
    *IdxIdxPtr = UppBd;		/* Which should == LowBd - 1 */
    *WasExactPtr = FALSE;
    *FlagsPtr = LowFlags;
    return bterr_NoError;
}

int b_FileIsRoot(bF)
struct btFile *bF;
{	/* Return our guess as to whether the given btFile is the root of a tree. */

    if (bF->Head.BTID1 == 0 && bF->Head.BTID2 == 0) return TRUE;
    else return FALSE;
}

/* Open an existing b-tree.
  Declaration:
  extern bt_ErrorCode bt_Open(btptr, path, mode);
  struct BTree **btptr;	returns a pointer to malloc'ed storage in here
	    char *path;		path to target file name
	      char *mode;		"r" for reading, "w" for read and update (so far)
		  */
bt_ErrorCode bt_Open(btptr, path, mode)
struct BTree **btptr;
char *path, *mode;
{
    struct BTr *bt;
    struct btFile *bF;
    bt_ErrorCode RetVal;

    bt = (struct BTr *) malloc(sizeof(struct BTr));
    if (bt == NULL) return bterr_OutOfMemory;
    bt->Tag = BTrTag;
    bt->Cursors = NULL;
    if (strcmp(mode, "w") == 0) bt->WriteEnabled = TRUE;
    else if (strcmp(mode, "r") == 0) bt->WriteEnabled = FALSE;
    else {free(bt); return bterr_NoSuchMode;}

    bF = b_NewbtFileStr();
    if (bF == NULL) {free(bt); return bterr_OutOfMemory;}
    bt->Root = bF;
    bF->RefCount += 1;		/* increment reference count */
    RetVal = b_ReadbtFile(bF, path, FALSE);
    if (RetVal != bterr_NoError) {free(bF); free(bt); return RetVal;}
    if (! b_FileIsRoot(bF)) {fclose(bF->File); free(bF->Index); free(bF->FileName);
    free(bF); free(bt); return bterr_NotOpeningRoot;}

    *btptr = (struct BTree *) bt;
    return bterr_NoError;
}

bt_ErrorCode b_DecrRefCount(bFPtr)
struct btFile **bFPtr;
{	/* Decrement the reference count on the given file, deleting the file if the count hits zero. */
    int Res;
    struct btFile *bF = *bFPtr;

    if ((--bF->RefCount) > 0) return bterr_NoError;
    if (bF->Index != NULL) free(bF->Index);
    if (bF->FileName != NULL) free(bF->FileName);
    *bFPtr = NULL;
    if (bF->File != NULL)
	if (fclose(bF->File) != 0)
	{Res = errno; free(bF); return bterr_FileSystemErrorBegin + Res;}
    free(bF);
    return bterr_NoError;
} 

bt_ErrorCode b_StoreFilePtr(bFPtrLoc, NewPtrVal)
struct btFile **bFPtrLoc;
struct btFile *NewPtrVal;
{/* Assign NewPtrVal to bFPtrLoc, handling reference counts */
    bt_ErrorCode RetVal;

    if (NewPtrVal != NULL && NewPtrVal->Tag != btFileTag)
	return bterr_CursorTreeDamaged;
    if (*bFPtrLoc != NULL && (*bFPtrLoc)->Tag != btFileTag)
	return bterr_CursorTreeDamaged;
    if (*bFPtrLoc == NewPtrVal) return bterr_NoError;
    if (*bFPtrLoc != NULL) {
	RetVal = b_DecrRefCount(bFPtrLoc);
	if (RetVal != bterr_NoError) return RetVal;
    }
    *bFPtrLoc = NewPtrVal;
    if (NewPtrVal != NULL) NewPtrVal->RefCount += 1;
    return bterr_NoError;
}

/* Close a b-tree collection.
  Declaration:
  extern bt_ErrorCode bt_Close(btp);
  struct BTree *btp;		pointer to b-tree to close
    */
bt_ErrorCode bt_Close(btp)
struct BTree *btp;
{
    struct BTr *bt = (struct BTr *) btp;		/* regain internal access */
    struct btC *bC, *bCNext;
    bt_ErrorCode RetVal;

    if (bt->Tag != BTrTag) return bterr_NotABTree;
    for (bC = bt->Cursors; bC != NULL; bC = bCNext) {
	if (bC->Tag != btCTag || bC->Tree != bt) break;
	b_StoreFilePtr(&bC->FP, NULL);
	bCNext = bC->Next;
	free(bC);
    }
    bt->Cursors = NULL;
    RetVal = b_StoreFilePtr(&bt->Root, NULL);
    bt->Tag = -1;	/* in case the caller is holding on to a pointer to this storage */
    free(bt);
    return RetVal;
}

bt_ErrorCode b_GetFlags(bF, Idx, FlagsPtr)
struct btFile *bF;
int Idx, *FlagsPtr;
{
    int ThisByte;

    errno = 0;
    if (fseek(bF->File, bF->FileOrigin + bF->Index[Idx], 0) != 0)
	return (bterr_FileSystemErrorBegin + (errno==0 ? EIO : errno));
    ThisByte = fgetc(bF->File);
    if (ThisByte != BTSeparatorByte) return bterr_BTreeDamaged;
    ThisByte = fgetc(bF->File);
    if (ThisByte == EOF) return bterr_BTreeDamaged;
    *FlagsPtr = ThisByte;
    return bterr_NoError;
}

bt_ErrorCode b_GetValueLength(bF, Idx, LenPtr)
struct btFile *bF;
int Idx;
unsigned int *LenPtr;
{/* Get the number of bytes in the Value part of the Idx'th Key-Value pair in file bF.  Leave the bF's cursor pointing to the first Value byte. */

    unsigned short LenValue;
    int	ThisByte;
    bt_ErrorCode RetVal;

    RetVal = b_GetFlags(bF, Idx, &ThisByte);
    if (RetVal != bterr_NoError) return RetVal;
    do {
	ThisByte = getc(bF->File);
	if (ThisByte == EOF) return bterr_BTreeDamaged;
    } while (ThisByte != 0);
    LenValue = b_ReadNetShort(bF->File);
    if (feof(bF->File) || ferror(bF->File)) return bterr_BTreeDamaged;
    *LenPtr = LenValue;
    return bterr_NoError;
}

/* Create a cursor into an open b-tree.
  Declaration:
  extern bt_ErrorCode bt_NewCursor(btp, cursptr);
  struct BTree *btp;
  struct btCursor **cursptr;
  */
bt_ErrorCode bt_NewCursor(btp, cursptr)
struct BTree *btp;
struct btCursor **cursptr;
{
    struct BTr *bt = (struct BTr *) btp;
    struct btC *bC;

    if (bt == NULL || bt->Tag != BTrTag || bt->Root == NULL) return bterr_NotABTree;
    bC = (struct btC *) malloc(sizeof(struct btC));
    if (bC == NULL) return bterr_OutOfMemory;
    bC->Tag = btCTag;
    bC->Tree = bt;
    bC->FP = NULL;
    bC->State = UnInitialized;
    bC->IndexPos = -1;
    /* Now link it in to the chain hanging off of the B-tree record itself */
    bC->Next = bt->Cursors;
    bt->Cursors = bC;

    *cursptr = (struct btCursor *) bC;
    return bterr_NoError;
}

/* Remove a cursor into an open b-tree.
  Declaration:
  extern bt_ErrorCode bt_FreeCursor(curs);
  struct btCursor *curs;
  */
bt_ErrorCode bt_FreeCursor(curs)
struct btCursor *curs;
{
    struct btC *bC = (struct btC *) curs;
    struct btC *Rover;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    Rover = bC->Tree->Cursors;
    if (Rover == NULL || Rover->Tag != btCTag) return bterr_CursorTreeDamaged;
    else if (Rover == bC) {
	bC->Tree->Cursors = bC->Next;
    } else while (1) {
	if (Rover->Next == bC) {
	    Rover->Next = bC->Next;
	} else if (Rover->Next == NULL || Rover->Next->Tag != btCTag)
	    return bterr_CursorTreeDamaged;
	else Rover = Rover->Next;
    }
    b_StoreFilePtr(&bC->FP, NULL);
    bC->Tag = -1;	/* in case caller is hanging on to a pointer to this storage */
    free(bC);

    return bterr_NoError;
}

/* Give the current state of the cursor (from J. N. Gray, Notes for a Data Base Operating System).
  Declaration:
  extern enum bt_CursorState bt_GetCursorState(curs);
  struct btCursor *curs;
  The cursor-state Error will be returned if the argument doesn't seem to be a btCursor.
    */
enum bt_CursorState bt_GetCursorState(curs)
struct btCursor *curs;
{
    struct btC *bC = (struct btC *) curs;

    if (bC == NULL || bC->Tag != btCTag) return Error;
    else return bC->State;
}

/* Search for the entry matching the given key.
    If there's a match, leave the cursor pointing to the key; if none, leave the cursor in one of the other states.
	Declaration:
	extern bt_ErrorCode bt_Search(curs, key);
    struct btCursor *curs;
    char *key;
    */
bt_ErrorCode bt_Search(curs, key)
struct btCursor *curs;
unsigned char *key;
{
    struct btC *bC = (struct btC *) curs;
    int Idx, Exact, Flags, TreeDepth, RootNameLength, ThisByte;
    unsigned int ValueLength;
    bt_ErrorCode RetVal;
    auto char NodeFileName[MAXPATHLEN+1];
    char *NFNEnd, *NFNPtr;
    struct btFile *bF;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
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
	RetVal = b_ScanNode(bC->FP, key, &Idx, &Exact, &Flags);
	if (RetVal != bterr_NoError) return RetVal;
	if (TreeDepth == 0 && (Idx < 0 || (Flags & BTIsBrotherLink) == 0)) {
	    bC->IndexPos = Idx;
	    if (Exact) bC->State = AtKey;
	    else if (Idx < 0) {
		bC->IndexPos = 0;
		bC->State = BeforeFirst;
	    } else if ((Idx+1) == bC->FP->IndexNum) bC->State = AfterLast;
	    else bC->State = BetweenKeys;
	    if (Idx >= 0 && (Flags & BTIsLeafPair) == 0)
		return bterr_BTreeDamaged;
	    else return bterr_NoError;
	}

	if (Idx >= 0) {	/* allow check on Flags */
	  if (TreeDepth == 0) {
	    /* if this is a floor-level node, LeafPair should be on */
	    if ((Flags & BTIsLeafPair) == 0) return bterr_BTreeDamaged;
	  } else {
	    /* if this is above the floor node, LeafPair should be off */
	    if ((Flags & BTIsLeafPair) != 0) return bterr_BTreeDamaged;
	  }
	}


	RetVal = b_GetValueLength(bC->FP, Idx, &ValueLength);
	if (RetVal != bterr_NoError) return RetVal;
	if (ValueLength <= 0) return bterr_BTreeDamaged;
	if (ValueLength + RootNameLength >= (MAXPATHLEN-1))
	    return bterr_IntermediateNameTooLong;
	NFNPtr = NFNEnd;
	while ((int)(--ValueLength) >= 0) {
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

/* If the cursor is in AtKey state, return the number of bytes in the value of the key-value pair being pointed to.
  Declaration:
  extern bt_ErrorCode bt_GetCursorValueLen(curs, valueLen);
  struct btCursor *curs;
  unsigned int *valueLen;
  */
bt_ErrorCode bt_GetCursorValueLen(curs, valueLen)
struct btCursor *curs;
unsigned int *valueLen;
{
    struct btC *bC = (struct btC *) curs;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    if (bC->State != AtKey) return bterr_NotAtKey;
    return b_GetValueLength(bC->FP, bC->IndexPos, valueLen);
}

/* Similarly, if the cursor is in AtKey state, return the first valueLocSize bytes in the value of the key-value pair being pointed to.
    Declaration:
    extern bt_ErrorCode bt_GetCursorValueData(curs, valueLoc, valueLocSize, returnedLen);
  struct btCursor *curs;
  char *valueLoc;
  unsigned int valueLocSize, *returnedLen;
  */
bt_ErrorCode bt_GetCursorValueData(curs, valueLoc, valueLocSize, returnedLen)
struct btCursor *curs;
char *valueLoc;
unsigned int valueLocSize, *returnedLen;
{
    struct btC *bC = (struct btC *) curs;
    bt_ErrorCode RetVal;
    FILE *F;
    unsigned int valueLen;
    int BytesToRead, ThisByte;
    char *DestP;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    if (bC->State != AtKey) return bterr_NotAtKey;
    if (bC->FP == NULL || bC->FP->Tag != btFileTag) return bterr_CursorTreeDamaged;
    F = bC->FP->File;
    RetVal = b_GetValueLength(bC->FP, bC->IndexPos, &valueLen);
    if (RetVal != bterr_NoError) return RetVal;
    BytesToRead = MIN(valueLen, valueLocSize);
    *returnedLen = BytesToRead;
    DestP = valueLoc;
    while ((--BytesToRead) >= 0) {
	ThisByte = getc(F);
	if (ThisByte == EOF) {
	    return (ferror(F) ?
		    (bterr_FileSystemErrorBegin + errno) :
		    bterr_BTreeDamaged);
	}
	*DestP++ = ThisByte;
    }
    return bterr_NoError;
}
