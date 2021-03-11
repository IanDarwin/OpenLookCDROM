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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/btr4.c,v 2.15 1993/09/29 23:50:50 gk5g Exp $";
#endif

/* ************************************************************ *\

	btr4.c
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

extern int br_Debugging;

/* If the cursor is in AtKey state, return a pointer to freshly-allocated storage that holds a copy of the key to which the cursor points.
Declaration:
	extern bt_ErrorCode bt_GetCursorKey(curs, keyLoc);
	struct btCursor *curs;
	char **keyLoc;
*/
bt_ErrorCode bt_GetCursorKey(curs, keyLoc)
struct btCursor *curs;
char **keyLoc;
{
    struct btC *bC = (struct btC *) curs;
    struct btFile *bF;
    int ThisByte;
    char *KeyVal, *KeyValEnd, *NewKeyVal;
#define InitKeyAlloc 32
    int KeyLen, KeyAlloc;
    bt_ErrorCode RetVal;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    if (bC->State != AtKey) return bterr_NotAtKey;
    bF = bC->FP;
    if (bF == NULL) return bterr_CursorTreeDamaged;
    RetVal = b_GetFlags(bF, bC->IndexPos, &ThisByte);
    if (RetVal != bterr_NoError) return RetVal;
    KeyAlloc = InitKeyAlloc;
    KeyVal = (char *) malloc(KeyAlloc);
    if (KeyVal == NULL) return bterr_OutOfMemory;
    KeyValEnd = KeyVal;
    KeyLen = 0;
    do {
	ThisByte = getc(bF->File);
	if (ThisByte == EOF) {
	    free(KeyVal);
	    return (ferror(bF->File) ?
		    (bterr_FileSystemErrorBegin + errno) :
		    bterr_BTreeDamaged);
	}
	if (++KeyLen > KeyAlloc) {
	    NewKeyVal = (char *) malloc((KeyAlloc * 2));
	    if (NewKeyVal == NULL)
	    {free(KeyVal); return bterr_OutOfMemory;}
	    bcopy(KeyVal, NewKeyVal, KeyAlloc);
	    KeyValEnd += (NewKeyVal - KeyVal);
	    free(KeyVal);
	    KeyVal = NewKeyVal;
	    KeyAlloc = KeyAlloc * 2;
	}
	*KeyValEnd++ = ThisByte;
    } while (ThisByte != 0);
    NewKeyVal = (char*) realloc(KeyVal, KeyLen);
    if (NewKeyVal != NULL) KeyVal = NewKeyVal;

    *keyLoc = KeyVal;
    return bterr_NoError;
}

/* If the cursor is in AtKey state, allocate memory to contain the entire Value of the key-value pair being pointed to, read the Value into that memory, store the pointer to the Value in valueLoc, and its length into returnedLen.
Declaration:
	extern bt_ErrorCode bt_GetCursorValue(curs, valueLoc, returnedLen);
	struct btCursor *curs;
	char **valueLoc;
	unsigned int *returnedLen;
*/
bt_ErrorCode bt_GetCursorValue(curs, valueLoc, returnedLen)
struct btCursor *curs;
char **valueLoc;
unsigned int *returnedLen;
{
    struct btC *bC = (struct btC *) curs;
    bt_ErrorCode RetVal;
    FILE *F;
    char *ValueBuff;
    unsigned int valueLen;
    int ThisByte, RealLen;
    char *DestP;

    if (bC == NULL || bC->Tag != btCTag
	 || bC->Tree == NULL || bC->Tree->Tag != BTrTag)
	return bterr_CursorTreeDamaged;
    if (bC->State != AtKey) return bterr_NotAtKey;
    if (bC->FP == NULL || bC->FP->Tag != btFileTag) return bterr_CursorTreeDamaged;
    F = bC->FP->File;
    RetVal = b_GetValueLength(bC->FP, bC->IndexPos, &valueLen);
    if (RetVal != bterr_NoError) return RetVal;
    ValueBuff = (char *) malloc(valueLen);
    if (ValueBuff == NULL) return bterr_OutOfMemory;
    *returnedLen = valueLen;
    DestP = ValueBuff;
    RealLen = valueLen;
    while ((--RealLen) >= 0) {
	ThisByte = getc(F);
	if (ThisByte == EOF) {
	    free(ValueBuff);
	    return (ferror(F) ?
		    (bterr_FileSystemErrorBegin + errno) :
		    bterr_BTreeDamaged);
	}
	*DestP++ = ThisByte;
    }
    *valueLoc = ValueBuff;	/* publish the loc of the completed buffer */
    return bterr_NoError;
}
