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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btw.c,v 2.27 1993/09/23 19:58:14 gk5g Exp $";
#endif

/* ************************************************************ *\
	btw.c
	Library routines for writing B-trees.
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
static int Debugging = 0;

static struct osi_Times CurrTime;

struct btStack {	/* For stack of file nodes going down tree */
	struct btStack	*Up;
	struct btFile	*FP;
};

int btw_SetDebugging(level)
int level;
{ int OldLevel;
  OldLevel = Debugging;
  Debugging = level;
  return OldLevel;
}

static void WriteNetShort(f, s)
FILE *f;
unsigned short s;
{	/* Write the unsigned short ``s'' to file ``f'' in network byte order. */
	unsigned short ns;

	ns = (unsigned short) htons((short) s);
	fwriteallchars(&ns, sizeof(ns), f);
}

static void WriteNetLong(f, l)
FILE *f;
unsigned long l;
{	/* Write the unsigned long ``l'' to file ``f'' in network byte order. */
	unsigned long nl;

	nl = (unsigned long) htonl((long) l);
	fwriteallchars(&nl, sizeof(nl), f);
}

#define ThisBTVersion	1
bt_ErrorCode b_InitbtFileStr(bFPtr)
struct btFile **bFPtr;
{
	struct btFile *bF;

	bF = b_NewbtFileStr();
	if (bF == NULL) return bterr_OutOfMemory;

/* Now initialize the BTFixedHead structure. */
	bF->Head.BTSignature = BTSignatureValue;
	bF->Head.BTVersion = ThisBTVersion;
	bF->Head.BTMaxFileSize = 0;		/* should be changed by caller */
	bF->Head.BTDepth = 0;
	bF->Head.BTID1 = 0;
	bF->Head.BTID2 = 0;
	bF->Head.BTFixedHeadSize = (sizeof(struct BTFixedHead) + (sizeof(unsigned long) - 1))
					 / sizeof(unsigned long);
	bF->Head.BTLockStyle = btlock_UseNoLock;	/* should be overwritten */
	bF->Head.BTExpectedSize1 = 0;
	bF->Head.BTExpectedSize2 = 0;

/* ...and the environmental stuff */
	osi_GetTimes(&CurrTime);
	bF->Head.BTMWhere = getaddr();
	bF->Head.BTMWhere = (unsigned long) ntohl((long) bF->Head.BTMWhere);	/* so htonl() will be meaningful later */
	bF->Head.BTMWho = getuid();
	bF->Head.BTMWhoE = geteuid();
	bF->Head.BTCTime1 = CurrTime.Secs;
	bF->Head.BTCTime2 = CurrTime.USecs;
	bF->Head.BTMTime1 = CurrTime.Secs;
	bF->Head.BTMTime2 = CurrTime.USecs;

/* Now the dynamic file-size stuff */
	bF->Head.BTIndexStart = bF->Head.BTFixedHeadSize * sizeof(unsigned long);
	bF->Head.BTIndexCount = 0;
	bF->Head.BTIndexSize = 0;
	bF->Head.BTKVStart = 0;
	bF->Head.BTKVFF = 0;

	*bFPtr = bF;
	return bterr_NoError;
}

bt_ErrorCode b_AddIndex(bF, InitialIndexSize)
struct btFile *bF;
int InitialIndexSize;
{
	int nbytes = sizeof(unsigned long) * InitialIndexSize;

	if (nbytes == 0)
		nbytes = 1;
	bF->Index = (unsigned long *) malloc(nbytes);
	if (bF->Index == NULL) return bterr_OutOfMemory;
	bF->IndexAlloc = InitialIndexSize;
	bF->IndexNum = 0;

	bF->Head.BTIndexCount = 0;
	bF->Head.BTIndexSize = InitialIndexSize;
	bF->Head.BTKVStart = bF->Head.BTIndexStart
				+ (sizeof(unsigned long) * bF->Head.BTIndexSize);
	bF->Head.BTKVFF = bF->Head.BTKVStart;

	return bterr_NoError;
}

static char FileNameChar[256] = {		/* characters that it's OK to use in file names */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0000-0017 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0020-0037 */
	0,1,0,1,0,1,0,0,  0,0,0,1,1,1,1,0,	/* 0040-0057 */
	1,1,1,1,1,1,1,1,  1,1,1,0,0,1,0,0,	/* 0060-0077 */
	0,1,1,1,1,1,1,1,  1,1,1,1,1,1,1,1,	/* 0100-0117 */
	1,1,1,1,1,1,1,1,  1,1,1,0,0,0,0,1,	/* 0120-0137 */
	0,1,1,1,1,1,1,1,  1,1,1,1,1,1,1,1,	/* 0140-0157 */
	1,1,1,1,1,1,1,1,  1,1,1,0,0,0,0,0,	/* 0160-0177 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0200-0217 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0220-0237 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0240-0257 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0260-0277 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0300-0317 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0320-0337 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,	/* 0340-0357 */
	0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0		/* 0360-0377 */
};
static char Base64Chars[64] = {		/* for use in generating unique file names */
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
	'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
	'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
	'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
	'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
	'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
	'u', 'v', 'w', 'x', 'y', 'z', '+', '=' };

static bt_ErrorCode GenerateNewFile(bF, RootFileName, FirstKey, NewFileSuffixPtr)
struct btFile *bF;
char *RootFileName, *FirstKey;
char **NewFileSuffixPtr;
{/* Return a new file, locked and opened for writing, with name and FILE in the bF str.  Return the suffix (minus dot) via NewFileSuffixPtr.  */

    char *NewName, *NewNameSuffix, *NewNameSansDirs, *Suffix;
    int RootFileNameLength, RootFileNamePrefixLength, MaxKeyCharsInName;
#define GlobMaxKeyCharsInName 5
#define NumUniqueChars 3		/* must be no greater than sizeof(int)/6 = 5 */
    /* Curr impl of ID1 and ID2 implies that GlobMaxKeyCharsInName + NumUniqueChars <= 8 */
#define RandMod (1 << (NumUniqueChars * 6))
#define RandMask (RandMod - 1)
#define MaxPermanentErrs 6
#define NumIDs 2
    char KeyPrefix[GlobMaxKeyCharsInName + 1];
    char *PSrc, *PDst, *IDPtr1, *IDPtr2;
    unsigned long	IDCopy[NumIDs + 1];
    int IDCount1, IDCount2;
    int RandBase, RandIncr, BaseDum, CharsLeft, Chunk, NumChunks, fid;
    int PermanentErrsLeft, NumFilesToTry;

    RootFileNameLength = strlen(RootFileName);
    NewNameSansDirs = rindex(RootFileName, '/');
    if (NewNameSansDirs == NULL)
	RootFileNamePrefixLength = 0;
    else
	RootFileNamePrefixLength = NewNameSansDirs - RootFileName + 1;
    Chunk = MAXPATHLEN - 1 - RootFileNameLength;
    NumChunks = MAXNAMLEN - (RootFileNameLength - RootFileNamePrefixLength);
    MaxKeyCharsInName = MIN(Chunk, NumChunks) - 2 - NumUniqueChars;
    MaxKeyCharsInName = MIN(MaxKeyCharsInName, GlobMaxKeyCharsInName);
    if (MaxKeyCharsInName < 0) {
#ifdef ENAMETOOLONG
	return (bterr_FileSystemErrorBegin+ENAMETOOLONG);
#else /* ENAMETOOLONG */
#ifndef bterr_IntermediateNameTooLong
#define bterr_IntermediateNameTooLong 10
#endif /* bterr_IntermediateNameTooLong */
	return(bterr_IntermediateNameTooLong);
#endif /* ENAMETOOLONG */
    }
    PSrc = FirstKey;		/* manufacture the part of name from the key */
    PDst = KeyPrefix;
    CharsLeft = MaxKeyCharsInName;
    IDCount1 = NumIDs * sizeof(unsigned long);
    IDPtr1 = (char *) &IDCopy[0];
    for (;CharsLeft > 0 && *PSrc != '\0'; PSrc++)
	if (FileNameChar[*PSrc] != 0) {
	    *PDst++ = *PSrc;
	    --CharsLeft;
	    if (IDCount1 > 0) {*IDPtr1++ = *PSrc; --IDCount1;}
	}
    *PDst++ = '\0';		/* terminate the string */

    Suffix = (char *) malloc(10);	/* 8 chars data, one dot, one null. */
    if (Suffix == NULL) return bterr_OutOfMemory;
    NumChunks = strlen(RootFileName) + strlen(KeyPrefix) + NumUniqueChars + 3;
    NewName = (char *) malloc(NumChunks);
    if (NewName == NULL) {free(Suffix); return bterr_OutOfMemory;}
    sprintf(NewName, "%s.%s.", RootFileName, KeyPrefix);
    sprintf(Suffix, "%s.", KeyPrefix);	/* to return to caller */
    NewNameSuffix = &NewName[strlen(NewName)];	/* ptr to the null */
    NewNameSansDirs = rindex(NewName, '/');
    if (NewNameSansDirs == NULL) NewNameSansDirs = NewName;
    else NewNameSansDirs = &NewNameSansDirs[1];

    /* Now generate a sequence of file names */
    osi_GetTimes(&CurrTime);
    osi_SetZone();
    RandBase = (CurrTime.Secs ^ osi_SecondsWest ^ getuid()) & RandMask;
    RandIncr = (((CurrTime.Secs << 1) ^ CurrTime.USecs ^ getpid()) & RandMask) | 1;
    /* the final ``| 1'' makes the incr odd, so that it's relatively prime to RandMod (a power of 2). */
    PermanentErrsLeft = MaxPermanentErrs;
    NumFilesToTry = RandMod + 1;	/* try first file twice */
    for (;;) {
	RandBase = (RandBase + RandIncr) & RandMask;
	BaseDum = RandBase;
	PDst = NewNameSuffix;
	IDCount2 = IDCount1;
	IDPtr2 = IDPtr1;
	for (NumChunks = NumUniqueChars; NumChunks > 0; --NumChunks) {
	    *PDst++ = Base64Chars[BaseDum & 077];
	    if (IDCount2 > 0) {
		*IDPtr2++ = Base64Chars[BaseDum & 077];
		--IDCount2;
	    }
	    BaseDum = BaseDum >> 6;
	}
	*PDst = '\0';
	while (IDCount2 > 0) {*IDPtr2++ = '\0'; --IDCount2;}
	--NumFilesToTry;
	fid = open(NewName, O_RDONLY, 0644);
	if (fid >= 0) {
	    close(fid);
	    if (NumFilesToTry >= 0) continue;
	    free(Suffix); free(NewName);
	    return bterr_NoFileNamesLeft;
	}
	fid = open(NewName, O_RDWR | O_CREAT, 0644);
	if (fid < 0) {	/* analyze creation failure */
	    Chunk = errno;
	    switch (Chunk) {
		case EACCES:
		case ENOTDIR:
		case EISDIR:
		case EINVAL:
		case EFBIG:
		case ENOSPC:
		case EROFS:
		case ENXIO:
#ifdef ELOOP
		case ELOOP:
#endif /* ELOOP */
#ifdef ENAMETOOLONG
		case ENAMETOOLONG:
#endif /* ENAMETOOLONG */
#ifdef EDQUOT
		case EDQUOT:
#endif /* EDQUOT */
		    --PermanentErrsLeft;
		    break;
	    }
	    if (PermanentErrsLeft < 0 || NumFilesToTry < 0) {
		free(Suffix); free(NewName);
		return (bterr_FileSystemErrorBegin + Chunk);
	    }
	    else continue;	/* the while loop, looking for a good file */
	}
	Chunk = osi_ExclusiveLockNoBlock(fid);	/* create it and lock it */
	if (Chunk != 0) {
	    Chunk = errno;	/* assume somebody else is creating it also */
	    VenusCancelStore(fid);
	    close(fid);
	    if (NumFilesToTry < 0) {
		free(Suffix); free(NewName);
		return (Chunk == EACCES ?
			bterr_NoLockPermission :
			bterr_FileSystemErrorBegin + Chunk);
	    }
	    continue;		/* try another name */
	}
	break;		/* New file, with write-lock. */
    }
    bF->File = fdopen(fid, "r+");
    if (bF->File == NULL) {
	osi_UnLock(fid); VenusCancelStore(fid); close(fid);
	free(Suffix); free(NewName);
	return bterr_OutOfMemory;
    }
    bF->FileOrigin = ftell(bF->File);
    bF->Head.BTID1 = (unsigned long) ntohl((long) IDCopy[0]);
    bF->Head.BTID2 = (unsigned long) ntohl((long) IDCopy[1]);
    bF->FileName = NewName;
    strcat(Suffix, NewNameSuffix);

    *NewFileSuffixPtr = Suffix;
    return bterr_NoError;
}

bt_ErrorCode b_WriteHeadIndex(bF)
struct btFile *bF;
{
    int Chunk;

    /* Write the in-memory data to the file */
    rewind(bF->File);
    errno = 0;
    for (Chunk = 0; Chunk < bF->Head.BTFixedHeadSize; Chunk++) {
	WriteNetLong(bF->File, bF->Head.BTarr[Chunk]);
    }
    if (feof(bF->File) || ferror(bF->File)) {
	Chunk = errno;
	unlink(bF->FileName); fclose(bF->File); bF->File = NULL;
	return (bterr_FileSystemErrorBegin + 	(Chunk == 0 ? EIO : Chunk));
    }
    fseek(bF->File, bF->FileOrigin + bF->Head.BTIndexStart, 0);
    errno = 0;
    for (Chunk = 0; Chunk < bF->Head.BTIndexSize; Chunk++) {
	WriteNetLong(bF->File, bF->Index[Chunk]);
    }
    if (feof(bF->File) || ferror(bF->File)) {
	Chunk = errno;
	unlink(bF->FileName); fclose(bF->File); bF->File = NULL;
	return (bterr_FileSystemErrorBegin + 	(Chunk == 0 ? EIO : Chunk));
    }
    return bterr_NoError;
}

/* Modifying the data structure.  These functions do not use cursors, because they must determine for  themselves (within a lock) how to modify the database. */

static struct btStack *NewStackElt()
{
    struct btStack *bS;

    bS = (struct btStack *) malloc(sizeof(struct btStack));
    if (bS == NULL) return NULL;
    bS->Up = NULL;
    bS->FP = NULL;
    return bS;
}

static bt_ErrorCode FlushStack(bS)
struct btStack *bS;
{
    struct btStack *bSNext;

    for (; bS != NULL; bS = bSNext) {
	b_StoreFilePtr(&bS->FP, NULL);
	bSNext = bS->Up;
	free(bS);
    }
    return bterr_NoError;
}

static bt_ErrorCode LockFile(bF)
struct btFile *bF;
{ /* Lock a file after it's already opened.  Retry as Vice dictates. */
    char *OldFileName;
    int Res;
    bt_ErrorCode RetVal;

    Res = osi_ExclusiveLockNoBlock(fileno(bF->File));
    if (Res == 0) return bterr_NoError;
    if (errno != EWOULDBLOCK && errno != EINVAL && errno != EACCES) return (bterr_FileSystemErrorBegin + errno);
    OldFileName = bF->FileName;	/* grab to use in opening it again */
    bF->FileName = NULL;
    free(bF->Index);
    bF->Index = NULL;
    fclose(bF->File); bF->File = NULL;
    RetVal = b_ReadbtFile(bF, OldFileName, TRUE);	/* this will loop for us */
    free(OldFileName);
    return RetVal;
}

static bt_ErrorCode MoveRight(bC, key)
struct btC *bC;
char *key;
{/* This follows brother links, using locking, at any level of the tree.  It does not descend the tree. */
    int Idx, Exact, Flags, TreeDepth, RootNameLength, ValueLength, ThisByte;
    bt_ErrorCode RetVal;
    auto char NodeFileName[MAXPATHLEN+1];
    char *NFNEnd, *NFNPtr;
    struct btFile *bF;

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
	if (Idx < 0 || (Flags & BTIsBrotherLink) == 0) {
	    bC->IndexPos = Idx;
	    if (Exact) bC->State = AtKey;
	    else if (Idx < 0) {
		bC->IndexPos = 0;
		bC->State = BeforeFirst;
	    } else if ((Idx+1) == bC->FP->IndexNum) bC->State = AfterLast;
	    else bC->State = BetweenKeys;
	    return bterr_NoError;
	}
	RetVal = b_GetValueLength(bC->FP, Idx, &ValueLength);
	if (RetVal != bterr_NoError) return RetVal;
	if (ValueLength <= 0) return bterr_BTreeDamaged;
	if (ValueLength + RootNameLength >= MAXPATHLEN)
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
	RetVal = b_ReadbtFile(bF, NodeFileName, TRUE);
	if (RetVal != bterr_NoError) {free(bF); return RetVal;}
	osi_UnLock(fileno(bC->FP->File));
	if (bF->Head.BTDepth != TreeDepth)
	{b_DecrRefCount(&bF); return bterr_BTreeDamaged;}
	b_StoreFilePtr(&bC->FP, bF);
	b_DecrRefCount(&bF);
    }
}


enum OpKind {OpInsert, OpUpdate, OpDelete, OpCondUpdate, OpCondDelete};
/* Can't use sizeof() on the following, since they must agree with the data in the files as well as with sizeof(unsigned int) and sizeof(unsigned short), respectively. */
#define	INDEXELTSIZE	4
#define	BYTECOUNTSIZE	2
#define	FILEPTRSIZE	8

static bt_ErrorCode NewBasicFile(bF, NewbFPtr)
struct btFile *bF, **NewbFPtr;
{ /* Make a copy of the bF structure suitable for being the basis for a new copy of the file. */

    struct btFile *NewbF;
    bt_ErrorCode RetVal;

    RetVal = b_InitbtFileStr(&NewbF);
    if (RetVal != bterr_NoError) return RetVal;
    NewbF->Head.BTMaxFileSize =	bF->Head.BTMaxFileSize;
    NewbF->Head.BTDepth =		bF->Head.BTDepth;
    NewbF->Head.BTLockStyle =	bF->Head.BTLockStyle;
    NewbF->Head.BTExpectedSize1 =	bF->Head.BTExpectedSize1;
    NewbF->Head.BTExpectedSize2 =	bF->Head.BTExpectedSize2;
    NewbF->Head.BTID1 =		bF->Head.BTID1;
    NewbF->Head.BTID2 =		bF->Head.BTID2;
    NewbF->Head.BTCTime1 =	bF->Head.BTCTime1;
    NewbF->Head.BTCTime2 =	bF->Head.BTCTime2;

    *NewbFPtr = NewbF;
    return bterr_NoError;
}

static bt_ErrorCode CopyPart(bF, newbF, SOffsP, DOffsP, idxLow, idxHigh, idxOffset, WasLeftP)
struct btFile *bF, *newbF;
long int *SOffsP, *DOffsP;
int idxLow, idxHigh, idxOffset;
int *WasLeftP;
{/* Copy from bF to newbF, idxLow <= ix < idxHigh, updating globals. */
    int	Ix, Char;
    unsigned short	ShortLen;
    long int	LengthDum;

    for (Ix = idxLow; Ix < idxHigh; ++Ix) {
	errno = 0;
	if (bF->Index[Ix] != (*SOffsP)) return bterr_BTreeDamaged;
	if (newbF->Index[Ix+idxOffset] != (*DOffsP)) return bterr_BTreeDamaged;
	Char = getc(bF->File); (*SOffsP)++;
	if (Char != BTSeparatorByte) return bterr_BTreeDamaged;
	errno = 0;
	if (putc(BTSeparatorByte, newbF->File) == EOF) goto TransError;
	(*DOffsP)++;
	errno = 0;
	Char = getc(bF->File); (*SOffsP)++;
	if (Char == EOF) goto TransError;
	if (*WasLeftP) Char |= BTIsLeftmostKey;
	else Char &= ~BTIsLeftmostKey;
	errno = 0;
	if (putc(Char, newbF->File) == EOF) goto TransError;
	(*DOffsP)++;
	*WasLeftP = 0;
	do {
	    errno = 0;
	    Char = getc(bF->File); (*SOffsP)++;
	    if (Char == EOF) goto TransError;
	    errno = 0;
	    if (putc(Char, newbF->File) == EOF) goto TransError;
	    (*DOffsP)++;
	} while (Char != '\0');
	errno = 0;
	ShortLen = b_ReadNetShort(bF->File); (*SOffsP) += BYTECOUNTSIZE;
	if (errno != 0) goto TransError;
	WriteNetShort(newbF->File, ShortLen); (*DOffsP) += BYTECOUNTSIZE;
	if (errno != 0) goto TransError;
	LengthDum = ShortLen;
	while ((--LengthDum) >= 0) {
	    errno = 0;
	    Char = getc(bF->File); (*SOffsP)++;
	    if (Char == EOF) goto TransError;
	    errno = 0;
	    if (putc(Char, newbF->File) == EOF) goto TransError;
	    (*DOffsP)++;
	}
    }
    return bterr_NoError;

    TransError:	/* make a goto into a cheap signal */
      return (bterr_FileSystemErrorBegin + (errno == 0 ? EIO : errno));
}

static bt_ErrorCode AppendThis(newbF, DOffsP, NewFlags, Key, Val, ValLen)
struct btFile *newbF;
long int *DOffsP;
int NewFlags;
char *Key, *Val;
int ValLen;
{
    int DataLen;

    errno = 0;
    if (putc(BTSeparatorByte, newbF->File) == EOF) goto XferError;
    (*DOffsP)++;
    errno = 0;
    if (putc(NewFlags, newbF->File) == EOF) goto XferError;
    (*DOffsP)++;
    errno = 0;
    DataLen = strlen(Key) + 1;
    if (fwriteallchars(Key, DataLen, newbF->File) != DataLen) goto XferError;
    (*DOffsP) += DataLen;
    errno = 0;
    WriteNetShort(newbF->File, (unsigned short) ValLen);
    if (errno != 0) goto XferError;
    (*DOffsP) += BYTECOUNTSIZE;
    if (fwriteallchars(Val, ValLen, newbF->File) != ValLen) goto XferError;
    (*DOffsP) += ValLen;
    return bterr_NoError;

    XferError:	/* make a goto into a cheap signal */
      return (bterr_FileSystemErrorBegin + (errno == 0 ? EIO : errno));
}

/* Check that the entry in bF at position Pos is Key/OldValLoc/OldValLen. */
static bt_ErrorCode CheckOld(bF, Pos, SOffsP, Key, OldValLoc, OldValLen)
struct btFile *bF;
int Pos;
long int *SOffsP;
unsigned char *Key, *OldValLoc;
unsigned int OldValLen;
{
    int	Char;
    unsigned char	*Ptr;
    unsigned short	ShortLen;
    long int	LengthDum;

    errno = 0;
    if (bF->Index[Pos] != (*SOffsP)) return bterr_InternalInconsistency;
    Char = getc(bF->File); (*SOffsP)++;
    if (Char != BTSeparatorByte) return bterr_BTreeDamaged;
    errno = 0;
    Char = getc(bF->File); (*SOffsP)++;	/* don't care what Flags were */
    if (Char == EOF) goto TransError;
    Ptr = Key;
    --Ptr;
    do {
	errno = 0;
	Char = getc(bF->File); (*SOffsP)++;
	if (Char == EOF) goto TransError;
	++Ptr;
	if (Char != *Ptr) return bterr_OldValueDifferent;
    } while (Char != '\0' && *Ptr != '\0');
    if (Char != *Ptr) return bterr_OldValueDifferent;
    errno = 0;
    ShortLen = b_ReadNetShort(bF->File); (*SOffsP) += BYTECOUNTSIZE;
    if (errno != 0) goto TransError;
    if (ShortLen != OldValLen) return bterr_OldValueDifferent;
    LengthDum = ShortLen;
    Ptr = OldValLoc;
    while ((--LengthDum) >= 0) {
	errno = 0;
	Char = getc(bF->File); (*SOffsP)++;
	if (Char == EOF) goto TransError;
	if (Char != *Ptr) return bterr_OldValueDifferent;
	++Ptr;
    }
    return bterr_NoError;

    TransError:	/* make a goto into a cheap signal */
      return (bterr_FileSystemErrorBegin + (errno == 0 ? EIO : errno));
}

static bt_ErrorCode NewCompleteVersion(bC, Op, NewKey, NewVal, NewValLen,
		NewbFPtr, NewFileSuffixPtr, ovLoc, ovLen)
struct btC *bC;
struct btFile **NewbFPtr;
enum OpKind Op;
char *NewKey, *NewVal, **NewFileSuffixPtr, *ovLoc;
int NewValLen, ovLen;
{
/* Make a new version of the file pointed to by bC and return it via NewbFPtr.
  The result version is identical to the (entire) source version except modified
 by the given Op.  On insertion, the target location (the address of the cursor
 bC) has been modified by the Op so that the new key-value pair has been
 inserted; on updates, the target location has been replaced with the new
 key-value pair; and on deletion, the entry pointed to by the cursor has been
 removed.  The key-value pair is given as follows.  The key is the
 null-terminated string pointed to by NewKey.  The value is the NewValLen
 bytes addressed by the NewVal pointer. */

    struct btFile *bF, *NewbF;
    int	NewHigh, Ix, IxAdj, TempHigh, IndexDelta, DataDelta, WasLeft;
    bt_ErrorCode	RetVal;
    char	*FirstKey, *NewSuffix;
    long int	SOffs, DOffs;
    int	Char, Pos, FirstKeyWasAlloced;
    enum bt_CursorState	StateDum;
    
    DataDelta = IndexDelta = IxAdj = 0;
    bF = bC->FP;
    Pos = bC->IndexPos;
    /* Copy selected fields from the previous version */
    RetVal = NewBasicFile(bF, &NewbF);
    if (RetVal != bterr_NoError) return RetVal;

    WasLeft = 0;	/* Should our first Flags byte have BTIsLeftmostKey on? */
    if (bF->IndexNum > 0) {
	RetVal = b_GetFlags(bF, 0, &Ix);
	if (RetVal != bterr_NoError) return RetVal;
	if ((Ix & BTIsLeftmostKey) != 0) WasLeft = 1;
    } else WasLeft = 1;	/* in this case, existing is null, next in will be first */
    NewHigh = bF->IndexNum;
    if (Op == OpDelete || Op == OpCondDelete) --NewHigh;
    else if (Op == OpInsert) NewHigh++;
    RetVal = b_AddIndex(NewbF, NewHigh);
    if (RetVal != bterr_NoError) {free(NewbF); return RetVal;}
    NewbF->IndexNum = NewHigh;	/* the in-core Index */
    NewbF->Head.BTIndexCount = NewHigh;	/* the Index we'll be writing */

    switch (Op) {
	case OpInsert:
	    IxAdj = 1;
	    DataDelta = IndexDelta = INDEXELTSIZE;
	    DataDelta += (strlen(NewKey) + 3 + BYTECOUNTSIZE + NewValLen);
	    Ix = ((bC->State == Null || bC->State == BeforeFirst) ? 0 : Pos+1);
	    NewbF->Index[Ix] = (Ix < bF->IndexNum ?	/* write entry for new elt */
				bF->Index[Ix] : bF->Head.BTKVFF) + IndexDelta;
	    break;
	case OpCondDelete:
	case OpDelete:
	    IxAdj = -1;
	    DataDelta = IndexDelta = -INDEXELTSIZE;
	    Ix = (Pos == (bF->IndexNum - 1)) ? bF->Head.BTKVFF : bF->Index[Pos + 1];
	    DataDelta -= (Ix - bF->Index[Pos]);
	    break;
	case OpCondUpdate:
	case OpUpdate:
	    IxAdj = 0;
	    DataDelta = IndexDelta = 0;
	    Ix = (Pos == (bF->IndexNum - 1)) ? bF->Head.BTKVFF : bF->Index[Pos + 1];
	    DataDelta += (strlen(NewKey) + 3 + BYTECOUNTSIZE + NewValLen - Ix + bF->Index[Pos]);
	    break;
    }
    Ix = Pos;
    if (Op == OpInsert && (bC->State == Null || bC->State == BeforeFirst)) Ix = -1;
    else if (Op == OpDelete || Op == OpCondDelete) --Ix;
    for (; Ix >= 0; --Ix)
	NewbF->Index[Ix] = bF->Index[Ix] + IndexDelta;
    Ix = Pos + 1;
    if (Op == OpInsert && (bC->State == Null || bC->State == BeforeFirst)) Ix = 0;
    for (; Ix < bF->IndexNum; Ix++)
	NewbF->Index[Ix+IxAdj] = bF->Index[Ix] + DataDelta;
    NewbF->Head.BTKVStart = bF->Head.BTKVStart + IndexDelta;
    NewbF->Head.BTKVFF = bF->Head.BTKVFF + DataDelta;

    if (NewHigh > 0 && bF->IndexNum > 0) {
	bC->IndexPos = bF->IndexNum-1;
	if (bC->IndexPos > 1) bC->IndexPos = 1;
	StateDum = bC->State;
	bC->State = AtKey;
	RetVal = bt_GetCursorKey((struct btCursor *) bC, &FirstKey);
	bC->IndexPos = Pos;	/* restore the value */
	bC->State = StateDum;	/* and the state */
	if (RetVal != bterr_NoError) {
	    free(NewbF->Index); free(NewbF);
	    return RetVal;
	}
	FirstKeyWasAlloced = TRUE;
    } else {FirstKey = "azaza"; FirstKeyWasAlloced = FALSE;}

    RetVal = GenerateNewFile(NewbF, bC->Tree->Root->FileName, FirstKey, &NewSuffix);
    if (FirstKeyWasAlloced) free(FirstKey);
    if (RetVal != bterr_NoError) {free(NewbF->Index); free(NewbF);
    return RetVal;}
    if (bF->Head.BTID1 == 0 && bF->Head.BTID2 == 0)	/* replacing root? */
	NewbF->Head.BTID1 = NewbF->Head.BTID2 = 0;	/* keep it root */
    RetVal = b_WriteHeadIndex(NewbF);
    if (RetVal != bterr_NoError) {
	free(NewSuffix);
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return(RetVal);
    }

    /* Now write the key-value pairs themselves. */
    if (bF->IndexNum > 0) {
	SOffs = bF->Index[0];
	errno = 0;
	Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
	if (Char == -1) goto TransputError;
    }
    DOffs = ftell(NewbF->File) - NewbF->FileOrigin;
    NewbF->Head.BTKVStart = DOffs;
    TempHigh = Pos;
    if (Op == OpInsert && bC->State != Null && bC->State != BeforeFirst) TempHigh++;
    RetVal = CopyPart(bF, NewbF, &SOffs, &DOffs, 0, TempHigh, 0, &WasLeft);
    if (RetVal != bterr_NoError) {
	unlink(NewbF->FileName);
	fclose(NewbF->File);
	free(NewSuffix);
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return RetVal;
    }
    switch (Op) {
	case OpCondUpdate:
	    RetVal = CheckOld(bF, Pos, &SOffs, (unsigned char *)NewKey, (unsigned char *)ovLoc, ovLen);
	    if (RetVal != bterr_NoError) {
		unlink(NewbF->FileName);
		fclose(NewbF->File);
		free(NewSuffix);
		free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		return RetVal;
	    }
	    /*		FALL THROUGH		*/
	case OpUpdate:
	    if (Pos+1 < bF->IndexNum) {
		SOffs = bF->Index[Pos+1];
		errno = 0;
		Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
		if (Char == -1) goto TransputError;
	    }
	    /*		FALL THROUGH		*/
	case OpInsert:
	    /* Figure new flags */
	    Ix = 0;
	    if (NewbF->Head.BTDepth == 0) Ix |= BTIsLeafPair;
	    if (WasLeft) {Ix |= BTIsLeftmostKey; WasLeft = 0;}
	    RetVal = AppendThis(NewbF, &DOffs, Ix, NewKey, NewVal, NewValLen);
	    if (RetVal != bterr_NoError) {
		unlink(NewbF->FileName);
		fclose(NewbF->File);
		free(NewSuffix);
		free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		return RetVal;
	    }
	    break;
	case OpCondDelete:
	    RetVal = CheckOld(bF, Pos, &SOffs, (unsigned char *)NewKey, (unsigned char *)ovLoc, ovLen);
	    if (RetVal != bterr_NoError) {
		unlink(NewbF->FileName);
		fclose(NewbF->File);
		free(NewSuffix);
		free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		return RetVal;
	    }
	    /*		FALL THROUGH		*/
	case OpDelete:	/* just don't bother to write it */
	    if (Pos+1 < bF->IndexNum) {
		SOffs = bF->Index[Pos+1];
		errno = 0;
		Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
		if (Char == -1) goto TransputError;
	    }
	    break;
    }
    Ix = Pos + 1;
    if (Op == OpInsert && (bC->State == Null || bC->State == BeforeFirst)) --Ix;
    RetVal = CopyPart(bF, NewbF, &SOffs, &DOffs, Ix, bF->IndexNum, IxAdj, &WasLeft);
    if (RetVal != bterr_NoError) {
	unlink(NewbF->FileName);
	fclose(NewbF->File);
	free(NewSuffix);
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return RetVal;
    }
    if (DOffs != NewbF->Head.BTKVFF) goto TransputError;
    errno = 0;
    if (fflush(NewbF->File) == EOF) goto TransputError;
    *NewFileSuffixPtr = NewSuffix;
    *NewbFPtr = NewbF;
    return bterr_NoError;

    TransputError:	/* make a goto into a cheap signal */
      Char = errno;
    unlink(NewbF->FileName);
    fclose(NewbF->File);
    free(NewSuffix);
    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
    return(bterr_FileSystemErrorBegin + (Char==0 ? EIO : Char));
}

static bt_ErrorCode NewSplitHalf(bC, Op, newkey, newval, newvalLen, bFPtr,
		Split, IsRight, XKey, XVal, SfxPtr, ovLoc, ovLen)
struct btC *bC;
enum OpKind Op;
char *newkey, *newval, *ovLoc;
int newvalLen, ovLen;
struct btFile **bFPtr;
int Split, IsRight;
char *XKey, *XVal;
char **SfxPtr;
{
/* Make one of the split halves of the new version of the file pointed to by bC
 and return it via bFPtr.  If IsRight is true, we make the right half, else the left
 half.  Op, newkey, newval, newvalLen describe the operation to be performed
 while we're at it, and Split is the split point (lowest old index to be put into
 the new right half).  If this is to be a new left half, XKey and XVal describe
 the new brother-link key to be written as the rightmost KV pair in the new left
 half.  The routine will store via SfxPtr newly-allocated storage containing the
 file suffix of the new file (minus the copy of the root file name and the dot).
 */
    struct btFile *bF, *NewbF;
    int	NewHigh, Ix, ModThisHalf, IndexDelta, IsLeftmost, KVStartOff;
    int KVFFOff, IdxSrc1, IdxLimit1, IdxDiff1, IdxOff1, IdxSrc2;
    int IdxLimit2, IdxDiff2, IdxOff2;
    bt_ErrorCode	RetVal;
    char	*FirstKey, *NewSuffix;
    long int	SOffs, DOffs;
    int	Char, Pos, FirstKeyWasAlloced;
    enum bt_CursorState	StateDum;

    IdxLimit2 = IdxSrc2 = 0;
    bF = bC->FP;
    Pos = bC->IndexPos;
    if (Op == OpInsert && (bC->State == Null || bC->State == BeforeFirst)) Pos = -1;
    ModThisHalf = (Pos < Split);
    if (IsRight) ModThisHalf = ! ModThisHalf;
    /* Copy selected fields from the previous version */
    RetVal = NewBasicFile(bF, &NewbF);
    if (RetVal != bterr_NoError) return RetVal;

    IsLeftmost = 0;	/* Should our first Flags byte have BTIsLeftmostKey on? */
    if (! IsRight) {	/* BTIsLeftmostKey only possibly on in left half */
	if (bF->IndexNum > 0) {
	    RetVal = b_GetFlags(bF, 0, &Ix);
	    if (RetVal != bterr_NoError) return RetVal;
	    if ((Ix & BTIsLeftmostKey) != 0) IsLeftmost = 1;
	} else IsLeftmost = 1;	/* in this case, existing is null, next in will be first */
    }
    if (IsRight)
	NewHigh = bF->IndexNum - Split;
    else
	NewHigh = Split + 1;	/* count the new pointer link */
    IndexDelta = 0;
    if (ModThisHalf) {
	if (Op == OpDelete || Op == OpCondDelete)
	{--NewHigh; IndexDelta = - INDEXELTSIZE;}
	else if (Op == OpInsert)
	{++NewHigh; IndexDelta = INDEXELTSIZE;}
    }
    RetVal = b_AddIndex(NewbF, NewHigh);
    if (RetVal != bterr_NoError) {free(NewbF); return RetVal;}
    NewbF->IndexNum = NewHigh;	/* the in-core Index */
    NewbF->Head.BTIndexCount = NewHigh;	/* the Index we'll be writing */
    /* The basic alg is going to be:
      newIdx[[IdxSrc1..IdxLimit1) + IdxDiff1] := oldIdx[[IdxSrc1..IdxLimit1)] + IdxOff1;
							 and then, if ModThisHalf, some Op-specific stuff, then:
							     newIdx[[IdxSrc2..IdxLimit2) + IdxDiff2] := oldIdx[[IdxSrc2..IdxLimit2)] + IdxOff2;
													       and then, if this is a left half, the XKey/XVal pair.
														   */
    if (IsRight) {	/* Set up basic pointer values */
	IdxSrc1 = Split;
	IdxLimit1 = bF->IndexNum;
	IdxDiff1 = - Split;
	KVStartOff = IdxOff1 = IndexDelta - (Split * INDEXELTSIZE);
	IdxOff1 -= (bF->Index[Split] - bF->Index[0]);
	if (ModThisHalf) {
	    IdxLimit1 = Pos + 1;	/* oops, only this much please */
	    IdxSrc2 = Pos + 1;
	    IdxLimit2 = bF->IndexNum;
	}
    } else {
	IdxSrc1 = 0;
	IdxLimit1 = Split;
	IdxDiff1 = 0;
	IdxOff1 = IndexDelta - ((bF->IndexNum - Split) * INDEXELTSIZE);
	/* now figure out room we'll need for the brother link */
	IdxOff1 += INDEXELTSIZE;
	KVStartOff = IdxOff1;
	if (ModThisHalf) {
	    IdxLimit1 = Pos + 1;	/* oops, only this much please */
	    IdxSrc2 = Pos + 1;
	    IdxLimit2 = Split;
	}
    }
    IdxDiff2 = IdxDiff1;
    KVFFOff = IdxOff2 = IdxOff1;
    if (ModThisHalf) {
	switch (Op) {
	    case OpInsert:
		++IdxDiff2;
		IdxOff2 += (strlen(newkey) + 3 + BYTECOUNTSIZE + newvalLen);
		/* write entry for new elt while we're here */
		Ix = Pos + 1;
		NewbF->Index[Ix+IdxDiff1] = (Ix < bF->IndexNum ?
					     bF->Index[Ix] : bF->Head.BTKVFF) + IdxOff1;
		break;
	    case OpCondDelete:
	    case OpDelete:
		--IdxDiff2;
		Ix = (Pos == (bF->IndexNum - 1)) ? bF->Head.BTKVFF : bF->Index[Pos + 1];
		IdxOff2 -= (Ix - bF->Index[Pos]);
		--IdxLimit1;	/* don't write outside of the allocated space */
		break;
	    case OpCondUpdate:
	    case OpUpdate:
		Ix = (Pos == (bF->IndexNum - 1)) ? bF->Head.BTKVFF : bF->Index[Pos + 1];
		IdxOff2 += (strlen(newkey) + 3 + BYTECOUNTSIZE + newvalLen - Ix + bF->Index[Pos]);
		break;
	}
	KVFFOff = IdxOff2;
    }
    if (!IsRight) {
	KVFFOff -= (bF->Head.BTKVFF - bF->Index[Split]);
    }
    if (Debugging) {
	fprintf(stderr, "Splitting at level %d, writing %s node:\n",
		bF->Head.BTDepth, (IsRight ? "right" : "left"));
	fprintf(stderr, "  newIdx[[%d..%d) + %d] := oldIdx[[%d..%d)] + %d\n",
		IdxSrc1, IdxLimit1, IdxDiff1, IdxSrc1, IdxLimit1, IdxOff1);
	if (ModThisHalf) {
	    fprintf(stderr, "  then the op, %d\n", Op);
	    fprintf(stderr, "  newIdx[[%d..%d) + %d] := oldIdx[[%d..%d)] + %d\n",
		    IdxSrc2, IdxLimit2, IdxDiff2, IdxSrc2, IdxLimit2, IdxOff2);
	}
	if (!IsRight) fprintf(stderr, "  then add the XKey/XValue pair %s/%s.\n",
			      XKey, XVal);
    }
    /* Copy the index before the change */
    for (Ix = IdxSrc1; Ix < IdxLimit1; ++Ix)
	NewbF->Index[Ix+IdxDiff1] = bF->Index[Ix] + IdxOff1;
    /* Copy the index after the change */
    if (ModThisHalf)
	for (Ix = IdxSrc2; Ix < IdxLimit2; ++Ix)
	    NewbF->Index[Ix+IdxDiff2] = bF->Index[Ix] + IdxOff2;
    /* Update the dimensions in the header */
    NewbF->Head.BTKVStart = bF->Head.BTKVStart + KVStartOff;
    NewbF->Head.BTKVFF = bF->Head.BTKVFF + KVFFOff;
    /* Possibly add the brother link */
    if (!IsRight) {
	NewbF->Index[NewHigh-1] = NewbF->Head.BTKVFF;
	NewbF->Head.BTKVFF +=
	  (strlen(XKey) + 3 + BYTECOUNTSIZE + strlen(XVal));
    }

    /*	if (!IsRight) {
	FirstKey = XKey; FirstKeyWasAlloced = FALSE;
    } else */
    if (NewHigh > 0 && bF->IndexNum > 0) {
	bC->IndexPos = bF->IndexNum - 1;
	if (bC->IndexPos > (IdxSrc1+1)) bC->IndexPos = (IdxSrc1+1);
	StateDum = bC->State;
	bC->State = AtKey;
	RetVal = bt_GetCursorKey((struct btCursor *) bC, &FirstKey);
	bC->IndexPos = Pos;	/* restore the value */
	bC->State = StateDum;	/* and the state */
	if (RetVal != bterr_NoError) {
	    free(NewbF->Index); free(NewbF);
	    return RetVal;
	}
	FirstKeyWasAlloced = TRUE;
    } else {FirstKey = "ayaya"; FirstKeyWasAlloced = FALSE;}

    RetVal = GenerateNewFile(NewbF, bC->Tree->Root->FileName, FirstKey, &NewSuffix);
    if (FirstKeyWasAlloced) free(FirstKey);
    if (RetVal != bterr_NoError) {free(NewbF->Index); free(NewbF);
    return RetVal;}
    RetVal = b_WriteHeadIndex(NewbF);
    if (RetVal != bterr_NoError) {
	free(NewSuffix);
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return(RetVal);
    }

    /* Now write the key-value pairs themselves. */
    if (bF->IndexNum > IdxSrc1) {
	SOffs = bF->Index[IdxSrc1];
	errno = 0;
	Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
	if (Char == -1) goto TransputError;
    }
    DOffs = ftell(NewbF->File) - NewbF->FileOrigin;
    if (DOffs != NewbF->Head.BTKVStart) goto TransputError;	/* ** */
    Ix = IdxLimit1; if (ModThisHalf && Op != OpInsert) --Ix;
    RetVal = CopyPart(bF, NewbF, &SOffs, &DOffs, IdxSrc1, Ix, IdxDiff1, &IsLeftmost);
    if (RetVal != bterr_NoError) {
	unlink(NewbF->FileName);
	fclose(NewbF->File);
	free(NewSuffix);
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return RetVal;
    }
    if (ModThisHalf) {
	switch (Op) {
	    case OpCondUpdate:
		RetVal = CheckOld(bF, IdxLimit1-1, &SOffs, (unsigned char *)newkey, (unsigned char *)ovLoc, ovLen);
		if (RetVal != bterr_NoError) {
		    unlink(NewbF->FileName);
		    fclose(NewbF->File);
		    free(NewSuffix);
		    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		    return RetVal;
		}
		/*		FALL THROUGH		*/
	    case OpUpdate:
		if (IdxSrc2 < IdxLimit2) {
		    SOffs = bF->Index[IdxSrc2];
		    errno = 0;
		    Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
		    if (Char == -1) goto TransputError;
		}
		/*		FALL THROUGH		*/
	    case OpInsert:
		/* Figure new flags */
		Ix = 0;
		if (NewbF->Head.BTDepth == 0) Ix |= BTIsLeafPair;
		if (IsLeftmost) {Ix |= BTIsLeftmostKey; IsLeftmost = 0;}
		RetVal = AppendThis(NewbF, &DOffs, Ix, newkey, newval, newvalLen);
		if (RetVal != bterr_NoError) {
		    unlink(NewbF->FileName);
		    fclose(NewbF->File);
		    free(NewSuffix);
		    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		    return RetVal;
		}
		break;
	    case OpCondDelete:
		RetVal = CheckOld(bF, IdxLimit1, &SOffs, (unsigned char *)newkey, (unsigned char *)ovLoc, ovLen);
		if (RetVal != bterr_NoError) {
		    unlink(NewbF->FileName);
		    fclose(NewbF->File);
		    free(NewSuffix);
		    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
		    return RetVal;
		}
		/*		FALL THROUGH		*/
	    case OpDelete:	/* just don't bother to write it */
		if (IdxSrc2 < IdxLimit2) {
		    SOffs = bF->Index[IdxSrc2];
		    errno = 0;
		    Char = fseek(bF->File, SOffs + bF->FileOrigin, 0);
		    if (Char == -1) goto TransputError;
		}
		break;
	}
	RetVal = CopyPart(bF, NewbF, &SOffs, &DOffs,
			  IdxSrc2, IdxLimit2, IdxDiff2, &IsLeftmost);
	if (RetVal != bterr_NoError) {
	    unlink(NewbF->FileName);
	    fclose(NewbF->File);
	    free(NewSuffix);
	    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	    return RetVal;
	}
    }
    if (!IsRight) {
	/* Figure new flags */
	Ix = BTIsBrotherLink;
	if (NewbF->Head.BTDepth == 0) Ix |= BTIsLeafPair;
	if (IsLeftmost) {Ix |= BTIsLeftmostKey; IsLeftmost = 0;}
	RetVal = AppendThis(NewbF, &DOffs, Ix, XKey, XVal, strlen(XVal));
	if (RetVal != bterr_NoError) {
	    unlink(NewbF->FileName);
	    fclose(NewbF->File);
	    free(NewSuffix);
	    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	    return RetVal;
	}
    }
    if (DOffs != NewbF->Head.BTKVFF) goto TransputError;
    errno = 0;
    if (fflush(NewbF->File) == EOF) goto TransputError;
    *SfxPtr = NewSuffix;
    *bFPtr = NewbF;
    return bterr_NoError;

    TransputError:	/* make a goto into a cheap signal */
      Char = errno;
    unlink(NewbF->FileName);
    fclose(NewbF->File);
    free(NewSuffix);
    free(NewbF->Index); free(NewbF->FileName); free(NewbF);
    return(bterr_FileSystemErrorBegin + (Char==0 ? EIO : Char));
}

static bt_ErrorCode NewLevel(bFPtr, bF, bC, val1, key2, val2)
struct btFile **bFPtr, *bF;
struct btC *bC;
char *val1, *key2, *val2;
{
    /* Make a new root node at a new level (from tree assoc. with oldbF).  Store the new btFile via bFPtr.  The new root node is the result of splitting the old root node into the two parts reachable via val1 and val2, with the non-null key being key2. */
    struct btFile *NewbF;
    int	ErrSave;
    char *NewSfx;
    bt_ErrorCode	RetVal;
    long int	DOffs;

    /* Copy selected fields from the previous version */
    RetVal = NewBasicFile(bF, &NewbF);
    if (RetVal != bterr_NoError) return RetVal;

    RetVal = b_AddIndex(NewbF, 2);
    if (RetVal != bterr_NoError) {free(NewbF); return RetVal;}
    NewbF->IndexNum = 2;	/* the in-core Index */
    NewbF->Head.BTIndexCount = 2;	/* the Index we'll be writing */

    NewbF->Index[0] = NewbF->Index[1] = NewbF->Head.BTKVStart;
    NewbF->Index[1] += (3 + BYTECOUNTSIZE + strlen(val1));
    NewbF->Head.BTKVFF = NewbF->Index[1] +
      (strlen(key2) + 3 + BYTECOUNTSIZE + strlen(val2));

    RetVal = GenerateNewFile(NewbF, bC->Tree->Root->FileName, key2, &NewSfx);
    if (RetVal != bterr_NoError) {free(NewbF->Index); free(NewbF);
    return RetVal;}
    free(NewSfx);
    NewbF->Head.BTID1 = NewbF->Head.BTID2 = 0;	/* keep it root */
    NewbF->Head.BTDepth = bF->Head.BTDepth + 1;	/* at a new level */
    if (Debugging) fprintf(stderr, "** NEW B-TREE LEVEL: %d **\n",
			    NewbF->Head.BTDepth);
    RetVal = b_WriteHeadIndex(NewbF);
    if (RetVal != bterr_NoError) {
	free(NewbF->Index); free(NewbF->FileName); free(NewbF);
	return(RetVal);
    }

    /* Now write the key-value pairs themselves. */
    DOffs = ftell(NewbF->File) - NewbF->FileOrigin;
    NewbF->Head.BTKVStart = DOffs;
    /* First key (key itself is null string) */
    RetVal = AppendThis(NewbF, &DOffs, BTIsLeftmostKey, "", val1, strlen(val1));
    /* Second key */
    if (RetVal == bterr_NoError)
	RetVal = AppendThis(NewbF, &DOffs, 0, key2, val2, strlen(val2));
    if (DOffs != NewbF->Head.BTKVFF) RetVal = bterr_InternalInconsistency;
    if (RetVal != bterr_NoError) {
	VenusCancelStore(fileno(NewbF->File));
	unlink(NewbF->FileName);
	b_StoreFilePtr(&NewbF, NULL);
	return RetVal;
    }
    errno = 0;
    if (fflush(NewbF->File) == EOF) {
	ErrSave = errno;
	VenusCancelStore(fileno(NewbF->File));
	unlink(NewbF->FileName);
	b_StoreFilePtr(&NewbF, NULL);
	return(bterr_FileSystemErrorBegin + (ErrSave==0 ? EIO : ErrSave));
    }
    *bFPtr = NewbF;
    return bterr_NoError;
}

static bt_ErrorCode StoreLockedFile(bF, fdPtr)
struct btFile *bF;
int *fdPtr;
{/* Store the locked file bF.  If fdPtr is non-null, this routine leaves the file locked.  It stores via fdPtr a file descriptor to be unlocked when done, or a -1 if that failed.  UnlockFD(fd) will do the job. */
    int oldErrno;

    if (fdPtr == NULL) {	/* do the close and unlock here */
	errno = 0;
	if (vfclose(bF->File) == EOF) {
	    oldErrno = errno; unlink(bF->FileName);
	    free(bF->Index); free(bF->FileName); free(bF);
	    return (bterr_FileSystemErrorBegin + (oldErrno==0 ? EIO : oldErrno));
	}
	bF->File = NULL;
    } else {
	*fdPtr = -1;
	errno = 0;	/* in new Venus (10 Apr 87), fsync() should store the file. */
	if (fflush(bF->File) == EOF
	    || fsync(fileno(bF->File)) != 0
	    || (*fdPtr = dup(fileno(bF->File))) < 0) {
	    oldErrno = errno; unlink(bF->FileName);
	    b_StoreFilePtr(&bF, NULL);
	    *fdPtr = -1;
	    return (bterr_FileSystemErrorBegin + (oldErrno==0 ? EIO : oldErrno));
	}
	errno = 0;
	if (fclose(bF->File) == EOF) {	/* after the dup, shouldn't store */
	    oldErrno = errno; unlink(bF->FileName);
	    free(bF->FileName); free(bF->Index); free(bF);
	    *fdPtr = -1;
	    return (bterr_FileSystemErrorBegin + (oldErrno==0 ? EIO : oldErrno));
	}
	bF->File = NULL;
	VenusCancelStore(*fdPtr);	/* for dup'ed descriptor */
    }
    return bterr_NoError;
}

static bt_ErrorCode UnlockFD(fd)
int fd;
{/* Unlocks and closes a file descriptor. */
    osi_UnLock(fd);
    errno = 0;
    if (close(fd) < 0) return (bterr_FileSystemErrorBegin + (errno == 0 ? EIO : errno));
    return bterr_NoError;
}

static bt_ErrorCode InstallNew(oldbFptr, newbF, bt, fdPtr)
struct btFile **oldbFptr, *newbF;
struct BTr *bt;
int *fdPtr;
{/* Install the new file newbF in the loc pointed to by oldbFptr. */
/* Always returns with both files closed, unless replacing the root, in which case the new file is left open for reading and is pointed to by the root. */
/* If fdPtr is non-null, this routine will leave the new file locked and will store a file descriptor in fdPtr that is to be unlocked and closed when the caller is done with the new file.  -1 will be stored if the file is not still locked and open, for any reason. */
    char *copyOldName;
    int oldErrno, Val, WasRoot;
    struct btFile *oldbF = *oldbFptr;
    bt_ErrorCode RetVal;

    if (fdPtr != NULL) *fdPtr = -1;		/* initialize this */
    copyOldName = NewString(oldbF->FileName);
    if (copyOldName == NULL) {
	unlink(newbF->FileName);
	osi_UnLock(fileno(oldbF->File));
	b_StoreFilePtr(&newbF, NULL);
	b_StoreFilePtr(oldbFptr, NULL);
	return bterr_OutOfMemory;
    }
    /*  Only the *last* close on a file will automatically release a lock.  (via Kazar, 9 Apr 1987)  Yet, we expect that this fclose() will release the flock because it's a file we just created (and thus nobody else has open). */
    RetVal = StoreLockedFile(newbF, fdPtr);
    if (RetVal != bterr_NoError) {
	osi_UnLock(fileno(oldbF->File)); b_StoreFilePtr(oldbFptr, NULL);
	free(copyOldName);
	return RetVal;
    }
    errno = 0;
    Val = rename(newbF->FileName, copyOldName);
    oldErrno = errno;	/* in case it failed */
    osi_UnLock(fileno(oldbF->File));
    WasRoot = (oldbF == bt->Root);	/* TRUE iff we've replaced the root */
    b_StoreFilePtr(oldbFptr, NULL);
    if (Val != 0) {
	unlink(newbF->FileName); free(copyOldName);
	if (fdPtr != NULL) if (*fdPtr >= 0)
	{osi_UnLock(*fdPtr); close(*fdPtr); *fdPtr = -1;}
	free(newbF->FileName); free(newbF->Index); free(newbF);
	return (bterr_FileSystemErrorBegin + (oldErrno==0 ? EIO : oldErrno));
    }
    if (WasRoot == 0) {	/* not replacing root */
	free(copyOldName); free(newbF->FileName); free(newbF->Index); free(newbF);
	return bterr_NoError;
    }		/* Else, we have to read the new root and leave it hanging off bt. */
    free(newbF->FileName);	/* rename was successful */
    newbF->FileName = NULL;
    free(newbF->Index);
    newbF->Index = NULL;
    newbF->RefCount = 1;
    RetVal = b_ReadbtFile(newbF, copyOldName, FALSE);
    free(copyOldName);
    if (RetVal != bterr_NoError) {free(newbF); return RetVal;}
    b_StoreFilePtr(&bt->Root, newbF);	/* for future searches */
    b_DecrRefCount(&newbF);
    return bterr_NoError;
}

static int WhereToSplit(bC, Op, SizeDelta)
struct btC *bC;
enum OpKind Op;
int SizeDelta;
{/* Given that the node pointed to by bC needs to be split, and that the op will change the target size by SizeDelta, return a decent guess for the lower bound of the new right node. */
    int LB, UB, Mid, Size, TempSize, Trial;
    struct btFile *bF = bC->FP;

    Size = bF->Head.BTKVFF + bF->Index[0]
      + (bF->Head.BTIndexCount * INDEXELTSIZE);
    LB = 0; UB = bF->Head.BTIndexCount -1;
    while (LB <= UB) {
	Mid = (LB + UB) / 2;
	TempSize = Size;
	if (Mid < bC->IndexPos)
	    TempSize += SizeDelta;
	else
	    TempSize -= SizeDelta;
	Trial = 2 * ((Mid * INDEXELTSIZE) + bF->Index[Mid]);
	if (Trial == TempSize) {
	    UB = Mid;
	    break;
	} else if (Trial < TempSize)
	    LB = Mid + 1;
	else
	    UB = Mid - 1;
    }
    if ((Op == OpDelete || Op == OpCondDelete) && UB == bC->IndexPos) {
	if (UB > 0) --UB;		/* can't split at deleted node */
	else ++UB;
    }
    if (Debugging) fprintf(stderr, "WhereToSplit chooses %d from [0..%d).\n",
			    UB, bF->Head.BTIndexCount);
    return UB;
}

static bt_ErrorCode ChangeOp(bt, Op, key, valueLoc, valueLen, ovLoc, ovLen)
struct BTr *bt;
enum OpKind Op;
char *key, *valueLoc, *ovLoc;
unsigned int valueLen, ovLen;
{
    /* Carry out the specified transformation on the given b-tree. */

    struct btC *bC;	/* Temp cursor that we allocate */
    struct btStack *bS, *bSRoot;
    int Idx, Exact, Flags, TreeDepth, RootNameLength, ValueLength, ThisByte,
    SizeDelta, Psn, KVAllocedHere;
    bt_ErrorCode RetVal;
    auto char NodeFileName[MAXPATHLEN+1];
    char *NFNEnd, *NFNPtr, *Suffix;
    struct btFile *bF;

    SizeDelta = 0;

    if (bt == NULL || bt->Tag != BTrTag) return bterr_NotABTree;
    if (! bt->WriteEnabled) return bterr_OpenReadOnly;
    if (valueLen >= (unsigned short) -1) return bterr_ValueTooLarge;
    if (key != NULL) if (*key == '\0') return bterr_IllegalKey;
    bC = (struct btC *) malloc(sizeof(struct btC));
    if (bC == NULL) return bterr_OutOfMemory;
    bC->Tag = btCTag;
    bC->Tree = bt;
    bC->Next = NULL;
    bC->FP = NULL;
    bSRoot = NULL;
    KVAllocedHere = FALSE;
    b_StoreFilePtr(&bC->FP, bC->Tree->Root);
    TreeDepth = bC->FP->Head.BTDepth;
    strcpy(NodeFileName, bt->Root->FileName);
    RootNameLength = strlen(NodeFileName);
    NFNEnd = &NodeFileName[RootNameLength];
    *NFNEnd++ = '.';
    for (;;) {
	if (bC->FP->IndexNum == 0) {
	    bC->IndexPos = 0;
	    bC->State = Null;
	    break;
	}
	RetVal = b_ScanNode(bC->FP, key, &Idx, &Exact, &Flags);
	if (RetVal != bterr_NoError) {b_StoreFilePtr(&bC->FP, NULL);
	free(bC); FlushStack(bSRoot); return RetVal;}
	if (TreeDepth == 0) {
	    bC->IndexPos = Idx;
	    if (Exact) bC->State = AtKey;
	    else if (Idx < 0) {
		bC->IndexPos = 0;
		bC->State = BeforeFirst;
	    } else if ((Idx+1) == bC->FP->IndexNum) bC->State = AfterLast;
	    else bC->State = BetweenKeys;
	    if (Idx >= 0 && (Flags & BTIsLeafPair) == 0) {
		b_StoreFilePtr(&bC->FP, NULL); free(bC);
		FlushStack(bSRoot); return bterr_BTreeDamaged;
	    } else break;
	}
	/* push direct parents on stack */
	if (Idx < 0 || (Flags & BTIsBrotherLink) == 0) {
	    bS = NewStackElt();
	    if (bS == NULL)
	    {b_StoreFilePtr(&bC->FP, NULL); free(bC);
	    FlushStack(bSRoot); return bterr_OutOfMemory;}
	    bS->Up = bSRoot;
	    b_StoreFilePtr(&bS->FP, bC->FP);
	    bSRoot = bS;
	}
	if (Idx < 0) Idx = 0;	/* don't expect it, with null keys, but... */
	RetVal = b_GetValueLength(bC->FP, Idx, &ValueLength);
	if (RetVal != bterr_NoError) {b_StoreFilePtr(&bC->FP, NULL); free(bC);
	FlushStack(bSRoot); return RetVal;}
	if (ValueLength <= 0) {b_StoreFilePtr(&bC->FP, NULL); free(bC);
	FlushStack(bSRoot); return bterr_BTreeDamaged;}
	if (ValueLength + RootNameLength >= MAXPATHLEN)
	{b_StoreFilePtr(&bC->FP, NULL); free(bC); FlushStack(bSRoot);
	return bterr_IntermediateNameTooLong;}
	NFNPtr = NFNEnd;
	while ((--ValueLength) >= 0) {
	    ThisByte = fgetc(bC->FP->File);
	    if (ThisByte == EOF) {b_StoreFilePtr(&bC->FP, NULL); free(bC);
	    FlushStack(bSRoot); return bterr_BTreeDamaged;}
	    *NFNPtr++ = ThisByte;
	}
	*NFNPtr = '\0';		/* terminate it */
	bF = b_NewbtFileStr();
	if (bF == NULL) {b_StoreFilePtr(&bC->FP, NULL); free(bC);
	FlushStack(bSRoot); return bterr_OutOfMemory;}
	bF->RefCount = 1;
	RetVal = b_ReadbtFile(bF, NodeFileName, FALSE);
	if (RetVal != bterr_NoError)
	{free(bF); b_StoreFilePtr(&bC->FP, NULL); free(bC);
	FlushStack(bSRoot); return RetVal;}
	if ((Flags & BTIsBrotherLink) == 0) --TreeDepth;
	if (bF->Head.BTDepth != TreeDepth)
	{b_DecrRefCount(&bF); b_StoreFilePtr(&bC->FP, NULL); free(bC);
	FlushStack(bSRoot); return bterr_BTreeDamaged;}
	b_StoreFilePtr(&bC->FP, bF);
	b_DecrRefCount(&bF);
    }

    /* We've now done all the possible non-locked searches; switch to locked updates. */
    RetVal = LockFile(bC->FP);
    if (RetVal != bterr_NoError) {b_StoreFilePtr(&bC->FP, NULL); free(bC);
    FlushStack(bSRoot); return RetVal;}
    RetVal = MoveRight(bC, key);
    if (RetVal != bterr_NoError) {osi_UnLock(fileno(bC->FP->File));
    b_StoreFilePtr(&bC->FP, NULL); free(bC);
    FlushStack(bSRoot); return RetVal;}
    /* Now that we've found the right place in the file, can we do the given operation? */
    if (    (Op == OpInsert && bC->State == AtKey)
	 || (Op != OpInsert && bC->State != AtKey)	) {
	osi_UnLock(fileno(bC->FP->File));
	b_StoreFilePtr(&bC->FP, NULL); free(bC); FlushStack(bSRoot);
	return (Op == OpInsert ? bterr_NoDuplicateInsert : bterr_ModifyKeyMissing);
    }

    /* OK, so the operation is legitimate.  Carry it out. */
    DoInsertion:	/* label from Lehman/Yao paper */

      for (;;) {	/* Loop, going up the tree, until we find a safe node. */
	  /* Find the projected new node size */
	  Psn = bC->IndexPos;
	  Idx = (Psn == (bC->FP->IndexNum - 1)) ? bC->FP->Head.BTKVFF : bC->FP->Index[Psn + 1];
	  switch (Op) {
	      case OpInsert:
		  SizeDelta = INDEXELTSIZE + strlen(key) + 3 + BYTECOUNTSIZE + valueLen;
		  break;
	      case OpCondDelete:
		  /* First check to see if the length matches on the old entry size. */
		  if ((strlen(key) + 3 + BYTECOUNTSIZE + ovLen)	/* projected old size */
		      != (Idx - bC->FP->Index[Psn])) {			/* actual old size */
		      osi_UnLock(fileno(bC->FP->File));
		      b_StoreFilePtr(&bC->FP, NULL); free(bC); FlushStack(bSRoot);
		      if (KVAllocedHere) {free(key); free(valueLoc);}
		      return bterr_OldValueDifferent;
		  }
		  /* FALL THROUGH */
	      case OpDelete:
		  SizeDelta = - (Idx - bC->FP->Index[Psn] + INDEXELTSIZE);
		  break;
	      case OpCondUpdate:
		  /* First check to see if the length matches on the old entry size. */
		  if ((strlen(key) + 3 + BYTECOUNTSIZE + ovLen)	/* projected old size */
		      != (Idx - bC->FP->Index[Psn])) {			/* actual old size */
		      osi_UnLock(fileno(bC->FP->File));
		      b_StoreFilePtr(&bC->FP, NULL); free(bC); FlushStack(bSRoot);
		      if (KVAllocedHere) {free(key); free(valueLoc);}
		      return bterr_OldValueDifferent;
		  }
		  /* FALL THROUGH */
	      case OpUpdate:
		  SizeDelta = (strlen(key) + 3
			       + BYTECOUNTSIZE + valueLen - Idx + bC->FP->Index[Psn]);
		  break;
	  }
	  if ((bC->FP->Head.BTKVFF + SizeDelta) < bC->FP->Head.BTMaxFileSize
	      || bC->FP->IndexNum <= 2) {
	      FlushStack(bSRoot);	/* safe node */
	      RetVal = NewCompleteVersion(bC, Op, key, valueLoc, valueLen, &bF, &Suffix,
					  ovLoc, ovLen);
	      if (RetVal != bterr_NoError) {
		  osi_UnLock(fileno(bC->FP->File));
		  b_StoreFilePtr(&bC->FP, NULL); free(bC);
		  if (KVAllocedHere) {free(key); free(valueLoc);}
		  return RetVal;
	      }
	      free(Suffix);
	      RetVal = InstallNew(&bC->FP, bF, bt, NULL);
	      free(bC);
	      if (KVAllocedHere) {free(key); free(valueLoc);}
	      return RetVal;
	  } else {		/* Unsafe node.  Split the new one and insert another change. */
	      struct btFile *bF2, *bFR;
	      char *nKey, *nVal, *Sfx2;
	      enum bt_CursorState CopyState;
	      int CopyIdx;

	      bFR = NULL;	/* not splitting root yet */
	      if (Debugging) fprintf(stderr, "Splitting node at level %d\n",
				     bC->FP->Head.BTDepth);
	      Psn = WhereToSplit(bC, Op, SizeDelta);
	      CopyState = bC->State;
	      CopyIdx = bC->IndexPos;
	      bC->State = AtKey;
	      bC->IndexPos = Psn;	/* Read nKey from existing node */
	      RetVal = bt_GetCursorKey((struct btCursor *) bC, &nKey);
	      bC->State = CopyState;
	      bC->IndexPos = CopyIdx;
	      if (RetVal != bterr_NoError) {
		  osi_UnLock(fileno(bC->FP->File));
		  b_StoreFilePtr(&bC->FP, NULL); free(bC);
		  FlushStack(bSRoot);
		  if (KVAllocedHere) {free(key); free(valueLoc);}
		  return RetVal;
	      }					/* First build the new right half. */
	      RetVal = NewSplitHalf(bC, Op, key, valueLoc, valueLen, &bF2, Psn, 1,
				    NULL, NULL, &nVal, ovLoc, ovLen);  /* This gives nVal */
	      if (RetVal != bterr_NoError) {
		  osi_UnLock(fileno(bC->FP->File));
		  b_StoreFilePtr(&bC->FP, NULL); free(bC);
		  free(nKey);
		  FlushStack(bSRoot);
		  if (KVAllocedHere) {free(key); free(valueLoc);}
		  return RetVal;
	      }
	      RetVal = StoreLockedFile(bF2, NULL);	/* then store it. */
	      if (RetVal != bterr_NoError) {
		  osi_UnLock(fileno(bC->FP->File));
		  b_StoreFilePtr(&bC->FP, NULL); free(bC);
		  free(nKey); free(nVal);
		  FlushStack(bSRoot);
		  if (KVAllocedHere) {free(key); free(valueLoc);}
		  return RetVal;
	      }					/* Now, build the new left half. */
	      RetVal = NewSplitHalf(bC, Op, key, valueLoc, valueLen, &bF, Psn, 0,
				    nKey, nVal, &Sfx2, ovLoc, ovLen);
	      if (KVAllocedHere) {free(key); free(valueLoc); KVAllocedHere = FALSE;}
	      if (RetVal != bterr_NoError) {
		  unlink(bF2->FileName);
		  osi_UnLock(fileno(bC->FP->File));
		  b_StoreFilePtr(&bF2, NULL);
		  b_StoreFilePtr(&bC->FP, NULL); free(bC);
		  free(nKey); free(nVal);
		  FlushStack(bSRoot);
		  return RetVal;
	      }
	      /* If this is the root, we need a third node; otherwise, just install the new left half and continue. */
	      if (b_FileIsRoot(bC->FP)) {		/* it's the tree's root */
		  FlushStack(bSRoot);
		  RetVal = StoreLockedFile(bF, NULL);	/* Store the left half. */
		  if (RetVal != bterr_NoError) {
		      unlink(bF2->FileName);
		      osi_UnLock(fileno(bC->FP->File));
		      b_StoreFilePtr(&bF2, NULL);
		      b_StoreFilePtr(&bC->FP, NULL); free(bC);
		      free(nKey); free(nVal); free(Sfx2);
		      return RetVal;
		  }
		  RetVal = NewLevel(&bFR, bF, bC, Sfx2, nKey, nVal);
		  free(nKey); free(nVal); free(Sfx2);
		  if (RetVal != bterr_NoError) {
		      unlink(bF2->FileName); unlink(bF->FileName);
		      osi_UnLock(fileno(bC->FP->File));
		      b_StoreFilePtr(&bF, NULL);
		      b_StoreFilePtr(&bF2, NULL);
		      b_StoreFilePtr(&bC->FP, NULL); free(bC);
		      return RetVal;
		  }
		  RetVal = InstallNew(&bC->FP, bFR, bt, NULL);
		  if (RetVal != bterr_NoError) {
		      unlink(bF2->FileName); unlink(bF->FileName);
		      b_StoreFilePtr(&bF, NULL);
		      b_StoreFilePtr(&bF2, NULL);
		      free(bC);
		      return RetVal;
		  }
		  b_StoreFilePtr(&bF, NULL);
		  b_StoreFilePtr(&bF2, NULL);
		  free(bC);
		  return bterr_NoError;
	      } else {				/* not the root: just split the node. */
		  int oldFD;
		  /* Install the new left half, retaining the lock. */
		  free(Sfx2);
		  RetVal = InstallNew(&bC->FP, bF, bt, &oldFD);
		  if (RetVal != bterr_NoError) {
		      unlink(bF2->FileName);
		      b_StoreFilePtr(&bF2, NULL);
		      free(bC);
		      FlushStack(bSRoot);
		      free(nKey); free(nVal);
		      return RetVal;
		  }
		  /* The split nodes are now installed just fine, but we now must insert a pointer to the new node in the parent. */
		  key = nKey; valueLoc = nVal; valueLen = strlen(nVal);
		  KVAllocedHere = TRUE;
		  Op = OpInsert;	/* We'll be inserting into parent node */
		  if (bSRoot == NULL) {	/* Argh!  How could this happen? */
		      free(bC);
		      free(key); free(valueLoc);
		      UnlockFD(oldFD);
		      return bterr_InternalInconsistency;
		  }
		  bS = bSRoot;
		  bSRoot = bS->Up;
		  b_StoreFilePtr(&bC->FP, bS->FP);
		  b_StoreFilePtr(&bS->FP, NULL);
		  free(bS);		
		  /* Re-establish locked state on the parent file */
		  RetVal = LockFile(bC->FP);
		  if (RetVal == bterr_NoError) RetVal = MoveRight(bC, key);
		  if (RetVal != bterr_NoError) {
		      b_StoreFilePtr(&bC->FP, NULL);
		      free(bC);
		      free(key); free(valueLoc);
		      UnlockFD(oldFD);
		      FlushStack(bSRoot);
		      return RetVal;
		  }
		  RetVal = UnlockFD(oldFD);		/* Unlock old lower-down node */
		  if (RetVal != bterr_NoError) {
		      b_StoreFilePtr(&bC->FP, NULL);
		      free(bC);
		      free(key); free(valueLoc);
		      FlushStack(bSRoot);
		      return RetVal;
		  }
	      }		/* end: not the root; just split the node */
	  }		/* end: it wasn't safe */
      }	/* End of loop looking for a safe node */
}


/* Insert the given key/value pair into the database.  Returns an error if there is already an entry with that key.
Declaration:
	extern bt_ErrorCode bt_Insert(bt, key, valueLoc, valueLen);
	struct BTree *bt;
	char *key, *valueLoc;
	unsigned int valueLen;
*/
bt_ErrorCode bt_Insert(bt, key, valueLoc, valueLen)
struct BTree *bt;
char *key, *valueLoc;
unsigned int valueLen;
{
    return ChangeOp((struct BTr *) bt, OpInsert, key, valueLoc, valueLen, NULL, 0);
}

/* Similarly to bt_Insert, put the key/value pair into the database, overwriting the value currently associated with the given key.  If there is no existing entry with that key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_Replace(bt, key, valueLoc, valueLen);
	struct BTree *bt;
	char *key, *valueLoc;
	unsigned int valueLen;
*/
bt_ErrorCode bt_Replace(bt, key, valueLoc, valueLen)
struct BTree *bt;
char *key, *valueLoc;
unsigned int valueLen;
{
    return ChangeOp((struct BTr *) bt, OpUpdate, key, valueLoc, valueLen, NULL, 0);
}

/* Delete the record associated with the given key.  Returns an error if there is no such record.
Declaration:
	extern bt_ErrorCode bt_Delete(bt, key);
	struct BTree *bt;
	char *key;
*/
bt_ErrorCode bt_Delete(bt, key)
struct BTree *bt;
char *key;
{
    return ChangeOp((struct BTr *) bt, OpDelete, key, NULL, 0, NULL, 0);
}

/* Do a conditional replacement in the B-tree.  This operation will do an indivisible test-and-set on the database: if the value associated with the given key is oldValueLoc/oldValueLen, it will be replaced with newValueLoc/newValueLen.  If the existing value is not oldValueLoc/oldValueLen, the code bterr_OldValueDifferent will be returned and no replacement will be done.  As with bt_Replace, if there is no existing entry with this key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_CondReplace(bt, key, oldValueLoc, oldValueLen,
			newValueLoc, newValueLen);
	struct BTree *bt;
	char *key, *oldValueLoc, *newValueLoc;
	unsigned int oldValueLen, newValueLen;
*/
bt_ErrorCode bt_CondReplace(bt, key, oldValueLoc, oldValueLen, newValueLoc, newValueLen)
struct BTree *bt;
char *key, *oldValueLoc, *newValueLoc;
unsigned int oldValueLen, newValueLen;
{
    return ChangeOp((struct BTr *) bt, OpCondUpdate, key,
		     newValueLoc, newValueLen,
		     oldValueLoc, oldValueLen);
}

/* Do a conditional deletion in the B-tree.  This operation will do an indivisible test-and-set on the database: if the value associated with the given key is oldValueLoc/oldValueLen, it will be deleted.  If the existing value is not oldValueLoc/oldValueLen, the code bterr_OldValueDifferent will be returned and no deletion will be done.  As with bt_Delete, if there is no existing entry with this key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_CondDelete(bt, key, oldValueLoc, oldValueLen);
	struct BTree *bt;
	char *key, *oldValueLoc;
	unsigned int oldValueLen;
*/
bt_ErrorCode bt_CondDelete(bt, key, oldValueLoc, oldValueLen)
struct BTree *bt;
char *key, *oldValueLoc;
unsigned int oldValueLen;
{
    return ChangeOp((struct BTr *) bt, OpCondDelete, key, NULL, 0,
		     oldValueLoc, oldValueLen);
}
