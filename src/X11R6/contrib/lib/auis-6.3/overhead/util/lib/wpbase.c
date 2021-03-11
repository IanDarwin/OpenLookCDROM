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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/wpbase.c,v 2.22 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	wpbase.c
	Small library routines for White Pages lookups.
	Include file ``wp.h'' declares the procedures for clients.
\* ************************************************************ */

#include <andrewos.h>		/* sys/file.h sys/types.h sys/time.h strings.h*/
#include <system.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */
#include <sys/stat.h>
#include <pwd.h>
#include <util.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include <bt.h>
#include <wp.h>
#include <btwp.h>
#endif /* WHITEPAGES_ENV   */
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <afs/venus.h>
#endif /* #ifdef AFS_ENV */
#include <svcconf.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

int wp_Debugging = 0;

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */
#ifndef LogsYes
#define LogsYes 0
#endif /* LogsYes */

#if Logs
static int DoingTiming = 0;		/* Set by wp_SetTiming, cleared on wp_Terminate */
#endif /* Logs */
#if LogsYes
/*VARARGS2*/
static void Log(num, fmt, p1, p2, p3, p4, p5, p6)
int num; char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
    if (DoingTiming) Logstat("wpbase.c", num, fmt, p1, p2, p3, p4, p5, p6);
}
#endif /* LogsYes */

void w_LowerAll(S)
register char *S;
{
    for (; *S != '\0'; ++S) {
	if (*S <= 'Z' && *S >= 'A') *S += ('a' - 'A');
    }
}

wp_ErrorCode w_BTreeErr(code)
bt_ErrorCode code;
{	/* Translate B-tree error codes to our own version of them */
	if (code == bterr_NoError) return wperr_NoError;
	if (code >= 0 && code <= bterr_FileSystemErrorEnd)
	    return (code + wperr_BTreeBaseValue);
	return wperr_UndifferentiatedBTreeError;
}

/*
Translate field names to field indices for use in the various routines.  -1 is returned if the specified field is not found at this point.
*/
wp_FieldIndex wp_FieldNameToIndex(FName)
char *FName;
{
    int flIx;

    for (flIx = 0; flIx <= FldMAX; flIx++)
	if (strcmp(FName, wpFieldName[flIx]) == 0) return(flIx);
    return (-1);
}

int wp_SetDebugging(level)
int level;
{ int OldLevel;
OldLevel = wp_Debugging;
wp_Debugging = level;
return OldLevel;
}

int wp_SetTiming(level)
int level;
{
#if LogsYes
    int OldLevel;
    OldLevel = DoingTiming;
    DoingTiming = level;
    return OldLevel;
#else /* LogsYes */
    return 0;
#endif /* LogsYes */
}

void w_ZapIdSet(IS)
struct IdSet *IS;
{ /* Deallocate whatever in IS is allocated (carefully). */
    if (IS != Undef_IdSet && IS != Empty_IdSet) {
	if ((IS->Tag==0 || IS->Tag==IdSetTag) && IS->Ids != NULL) free(IS->Ids);
	free(IS);
    }
}

wp_ErrorCode w_GrowString(strP, ssizeP, NSize)
char **strP; int *ssizeP, NSize;
{/* Grow the string strP, accounting for its current size in ssizeP. */
    int newSize, tgtSize;
    char *newPtr;

    if (wp_Debugging) fprintf(stderr, "Growing string from %#x/%d (tgt %d)",
			       *strP, *ssizeP, NSize);
    tgtSize = (3 * NSize) / 2;
    if (*strP == NULL) {
	newSize = tgtSize;
	newPtr = malloc(newSize);
    } else {
	newSize = (*ssizeP + 2) * 2;		/* at least this big */
	if (newSize < tgtSize) newSize = tgtSize;
	newPtr = realloc(*strP, newSize);
    }
    if (newPtr == NULL) {	/* trashed */
	*strP = NULL;
	*ssizeP = 0;
	return wperr_OutOfMemory;
    }
    if (wp_Debugging) fprintf(stderr, " to %#x/%d.\n", newPtr, newSize);
    *strP = newPtr;
    *ssizeP = newSize;
    return wperr_NoError;
}

static char NOT_AN_IDENT[PKLEN] = {0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377};

/* Storage to hold the record as a whole */
static char *CursorVal = NULL;
static int CursorValSize = 0;

wp_ErrorCode w_LoadEntry(cd, IdKey)
register struct wp_CD *cd; char *IdKey;
{/* Load the identified entry into the Entries array. */
    char RecKey[PKLEN+4];
    bt_ErrorCode BTErr; wp_ErrorCode WPErr;
    char *FPtr, *FSpan, *FEnd, *namStart, *nPtr;
    int Fld, valLen, valLen2;
    char FldNam[100];

#if LogsYes
    Log(530, "w_LoadEntry called");
#endif /* LogsYes */
    strncpy(RecKey, IdKey, PKLEN);
    RecKey[PKLEN] = KeyTagSep;
    RecKey[PKLEN+1] = KeyTagR;
    RecKey[PKLEN+2] = '\0';
    if (wp_Debugging) fprintf(stderr, "w_LoadEntry(%.*s) called, key %s.\n",
			       PKLEN, IdKey, RecKey);
    BTErr = bt_Search(cd->cursor, RecKey);
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    if (bt_GetCursorState(cd->cursor) != AtKey) return wperr_BTreeTempFail;
#if LogsYes
    Log(531, "w_LoadEntry: bt_Search done, calling bt_GetCursorValueLen");
#endif /* LogsYes */
    BTErr = bt_GetCursorValueLen(cd->cursor, &valLen);
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    if (CursorValSize < (valLen+1)) {
	WPErr = w_GrowString(&CursorVal, &CursorValSize, (valLen+1));
	if (WPErr != wperr_NoError) return WPErr;
    }
#if LogsYes
    Log(532, "w_LoadEntry: bt_GetCursorValueLen done, calling bt_GetCursorValueData");
#endif /* LogsYes */
    BTErr = bt_GetCursorValueData(cd->cursor, CursorVal, valLen, &valLen2);
    if (valLen2 != valLen) return wperr_BTreeTempFail;
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    CursorVal[valLen] = '\0';	/* stop wild index() searches */
    /* OK, the given value is in CursorVal, with length valLen. */
#if LogsYes
    Log(533, "w_LoadEntry: bt_GetCursorValueData done; unpacking the data");
#endif /* LogsYes */
    strncpy(cd->LastRetrieved.RecID, NOT_AN_IDENT, PKLEN);
    for (Fld = 0; Fld <= FldMAX; ++Fld) {	/* null 'em out */
	if (cd->Entries[Fld] != NULL) cd->Entries[Fld][0] = '\0';
    }
    FPtr = CursorVal; FEnd = CursorVal + valLen;
    while (FPtr < FEnd) {	/* for each named field */
	FSpan = FPtr;
	for (;;) {	/* find end of this entry */
	    FSpan = index(FSpan, '\n');
	    if (FSpan >= FEnd) FSpan = NULL;
	    if (FSpan == NULL) {FSpan = FEnd; break;}
	    if (++FSpan >= FEnd) {FSpan = FEnd; break;}
	    if (*FSpan == '$') break;	/* leave it pointing at '$' */
	}
	namStart = FPtr;
	if (*namStart == '$') ++namStart;
	nPtr = FldNam;
	while (*namStart != ' ' && namStart < FSpan) *nPtr++ = *namStart++;
	if (nPtr >= &FldNam[sizeof FldNam]) {
	    fprintf(stderr, "FldNam overflow: %s\n", FPtr);
	    exit(2);
	}
	*nPtr = '\0';
	Fld = wp_FieldNameToIndex(FldNam);
	if (Fld >= 0) {
	    while (*namStart == ' ' && namStart < FSpan) ++namStart;
	    nPtr = FSpan;
	    --nPtr;
	    while (index(" \b\t\n\v\f\r", *nPtr) != NULL && nPtr > namStart)
		--nPtr;
	    ++nPtr;
	    if ((nPtr - namStart) >= cd->EntriesSize[Fld])
		WPErr = w_GrowString(&(cd->Entries[Fld]),
				     &(cd->EntriesSize[Fld]),
				     (nPtr - namStart + 1));
	    strncpy(cd->Entries[Fld], namStart, (nPtr - namStart));
	    cd->Entries[Fld][nPtr - namStart] = '\0';
	}
	FPtr = FSpan;	/* now look for the next field */
    }
    strncpy(cd->LastRetrieved.RecID, IdKey, PKLEN);
#if LogsYes
    Log(534, "w_LoadEntry returning");
#endif /* LogsYes */
    return wperr_NoError;
}

static char *PMKey = NULL;
static int PMKeyLen = 0;

wp_ErrorCode w_ProbeMatches(cd, BTIx, Probe, ISPtr)
struct wp_CD *cd; int BTIx; char *Probe; struct IdSet **ISPtr;
{
    struct IdSet *IS;
    int	IdCount, IdIx, PLen, DataLen, DataLen2;
    bt_ErrorCode BTErr; wp_ErrorCode WPErr;

    PLen = strlen(Probe);
#if LogsYes
    Log(780+BTIx, "w_ProbeMatches(%s) called, probe len %d",
	 btIndexNames[BTIx], PLen);
#endif /* LogsYes */
    if (wp_Debugging) fprintf(stderr, "w_ProbeMatches looking for entries with %s fields of ``%s''.\n",
			       btIndexNames[BTIx], Probe);
    PLen += 3;
    if (PMKeyLen < PLen) {
	WPErr = w_GrowString(&PMKey, &PMKeyLen, PLen);
	if (WPErr != wperr_NoError) return WPErr;
    }
    sprintf(PMKey, "%s%c%c", Probe, KeyTagSep, BTIxTags[BTIx]);
    w_LowerAll(PMKey);
#if LogsYes
    Log(611, "About to call bt_Search");
#endif /* LogsYes */
    cd->PrevPK[0] = '\0';
    BTErr = bt_Search(cd->cursor, PMKey);
#if LogsYes
    Log(612, "bt_Search returned; about to call bt_GetCursorValueLen");
#endif /* LogsYes */
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    if (bt_GetCursorState(cd->cursor) != AtKey) return wperr_NoKeysFound;
    BTErr = bt_GetCursorValueLen(cd->cursor, &DataLen);
#if LogsYes
    Log(613, "bt_GetCursorValueLen returned; about to build an IdSet");
#endif /* LogsYes */
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    if (DataLen <= 0) return wperr_BTreeTempFail;
    IdCount = (DataLen + (PKLEN-1)) / PKLEN;
    IS = (struct IdSet *) malloc(sizeof(struct IdSet));
    if (IS == NULL) return wperr_OutOfMemory;
    IS->Tag = IdSetTag;
    IS->IdCount = IdCount;
    IS->IdMax = IdCount;
    IS->Ids = (btwp_identifier *) malloc(IdCount * sizeof(btwp_identifier));
    if (IS->Ids == NULL) {free(IS); return wperr_OutOfMemory;}
#if LogsYes
    Log(614, "mallocs done; now calling bt_GetCursorValueData");
#endif /* LogsYes */
    BTErr = bt_GetCursorValueData(cd->cursor, IS->Ids[0].RecID, DataLen, &DataLen2);
    if (BTErr != bterr_NoError) {free(IS->Ids); free(IS); return w_BTreeErr(BTErr);}
    if (DataLen != DataLen2) {free(IS->Ids); free(IS); return wperr_BTreeTempFail;}

    if (wp_Debugging) {
	fprintf(stderr, "w_ProbeMatches returning %d btwp_id(s)", IdCount);
	for (IdIx = 0; IdIx < IdCount; IdIx++)
	    fprintf(stderr, " %.*s", PKLEN, IS->Ids[IdIx].RecID);
	fprintf(stderr, ".\n");
    }
    *ISPtr = IS;
#if LogsYes
    Log(790+BTIx, "w_ProbeMatches returning a set with %d IDs", IdCount);
#endif /* LogsYes */
    return wperr_NoError;
}

static void ZapPKSet(PKS)
wp_PrimeKeySet *PKS;
{ /* Deallocate whatever in PKS is allocated (carefully). */
    int	Idx;
    if (PKS != NULL) {
	if (/*(PKS->Tag==0 || PKS->Tag==PrimeKeySetTag) &&*/ PKS->Keys != NULL) {
	    for (Idx = PKS->KeyCount - 1; Idx >= 0; --Idx) {
		if (PKS->Keys[Idx] != NULL) free(PKS->Keys[Idx]);
	    }
	    free(PKS->Keys);
	}
	free(PKS);
    }
}

wp_ErrorCode w_IDtoPKSet(IS, PKSPtr, MaxResults, ResultNumberTruncated)
struct IdSet *IS; wp_PrimeKeySet **PKSPtr;
int MaxResults, *ResultNumberTruncated;
{
    wp_PrimeKeySet *PKs;
    int IdIx, CountToUse;

    CountToUse = IS->IdCount;
    if (MaxResults >= 0 && CountToUse > MaxResults) {
	CountToUse = MaxResults;
	*ResultNumberTruncated = 1;
    }
#if Logs
    Log(630, "w_IDtoPKSet building a %d-element PrimeKeySet", CountToUse);
#endif /* Logs */
    PKs = (wp_PrimeKeySet *) malloc(sizeof(wp_PrimeKeySet));
    if (PKs == NULL) return wperr_OutOfMemory;
    PKs->KeyCount = CountToUse;
    PKs->Keys = NULL;
    if (CountToUse > 0) {
	PKs->Keys = (wp_PrimeKey *) malloc(CountToUse * sizeof(wp_PrimeKey));
	if (PKs->Keys == NULL) {free(PKs); return wperr_OutOfMemory;}
    }
    for (IdIx = 0; IdIx < CountToUse; IdIx++){
	PKs->Keys[IdIx] = malloc(PKLEN+1);
	if (PKs->Keys[IdIx] == NULL) {
	    ZapPKSet(PKs); PKs = NULL;
	    return wperr_OutOfMemory;}
	strncpy(PKs->Keys[IdIx], IS->Ids[IdIx].RecID, PKLEN);
	PKs->Keys[IdIx][PKLEN] = '\0';
    }
    *PKSPtr = PKs;
#if Logs
    Log(636, "w_IDtoPKSet returning");
#endif /* Logs */
    return wperr_NoError;
}

/*  Clean up after the client is done with malloc'ed storage. */
wp_ErrorCode wp_DeAllocate(StrPtr)
    char **StrPtr;
/* union { wp_PrimeKeySet *PKSet; struct wp_SrchToken *ST;  wp_SearchToken XX;} StrPtr; */
{
    int ctr;
    struct wp_Constraint *CPtr;

    if (StrPtr == NULL) return wperr_NoError;

#if Logs
    Log(700, "wp_DeAllocate(0x%x) called", StrPtr);
#endif /* Logs */
    if ( ((wp_PrimeKeySet *) StrPtr)->KeyCount >= 0) { /* it's a pointer to PrimeKeySetStr */
	if (wp_Debugging) {
	    fprintf(stderr, "wp_DeAllocate(%#x) releasing a %d-key PrimeKeySet.\n", (wp_PrimeKeySet *) StrPtr, ((wp_PrimeKeySet *) StrPtr)->KeyCount);
	}
	if (((wp_PrimeKeySet *) StrPtr)->Keys != NULL) {
	    if (wp_Debugging) {
		fprintf(stderr, "PrimeKeySet has keys at %#x.\n", ((wp_PrimeKeySet *) StrPtr)->Keys);
	    }
	    for (ctr = ((wp_PrimeKeySet *) StrPtr)->KeyCount - 1; ctr >= 0; --ctr) {
		if (((wp_PrimeKeySet *) StrPtr)->Keys[ctr] != NULL) {
		    if (wp_Debugging) fprintf(stderr, "Releasing PKset key[%d]=%#x.\n", ctr, ((wp_PrimeKeySet *) StrPtr)->Keys[ctr]);
		    free(((wp_PrimeKeySet *) StrPtr)->Keys[ctr]);
		}
	    }
	    free(((wp_PrimeKeySet *) StrPtr)->Keys);
	}
	free((wp_PrimeKeySet *) StrPtr);
    } else if (((struct wp_SrchToken *) StrPtr)->Tag == SrchTokenTag) {
	if (wp_Debugging) {
	    fprintf(stderr, "wp_DeAllocate(%#x) releasing a wp_SrchToken.\n", (struct wp_SrchToken *) StrPtr);
	}
	CPtr = ((struct wp_SrchToken *) StrPtr)->Constraints;
	while (CPtr != NULL) {
	    ((struct wp_SrchToken *) StrPtr)->Constraints = CPtr->Next;
	    if (CPtr->Tag != ConstraintTag) return wperr_TokenMalformed;
	    free(CPtr->FieldContent);
	    free(CPtr);
	    CPtr = ((struct wp_SrchToken *) StrPtr)->Constraints;
	}
	free(((struct wp_SrchToken *) StrPtr)->Probe);
	free(((struct wp_SrchToken *) StrPtr));
    } else return wperr_NoSuchTokenKind;
    if (wp_Debugging) fprintf(stderr, "wp_DeAllocate returning.\n");
#if Logs
    Log(701, "wp_DeAllocate returning");
#endif /* Logs */
    return wperr_NoError;
}

/* Reading values from matched entries. */
/*
Once a prime key is determined, either from wp_Search or wp_Lookup, clients
 may obtain the values of fields of the corresponding entry by wp_Read.  All
 field values are strings.  The value is returned by making FValPtr point to
 static storage in a White Pages module; any subsequent White Pages call might
 overwrite the information, so copy it if you want to save it.  wperr_NoSuchKey
 is returned if the given key is not the prime key for an entry in the database;
 wperr_KeyError is returned if there is a format error in the prime key; and
 wperr_NoSuchField is returned if the given entry is found but it does not
 contain the given field.  wperr_FieldIndexOutOfBounds is returned if FieldIx
 is not the index for any field.
*/
wp_ErrorCode cwp_Read(cd, PKey, FieldIx, FValPtr)
struct wp_cd *cd; wp_PrimeKey PKey; wp_FieldIndex FieldIx; char **FValPtr;
{
    wp_ErrorCode RetCode;
    struct wp_CD *CD = (struct wp_CD *) cd;

#if LogsYes
    Log(200, "cwp_Read called");
#endif /* LogsYes */
    if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
    if (FieldIx < 0 || FieldIx > FldMAX) return wperr_FieldIndexOutOfBounds;
    if (strncmp(CD->LastRetrieved.RecID, PKey, PKLEN) != 0) {
	if (wp_Debugging) fprintf(stderr,
				  "Loading entry with identifier %.*s.\n", PKLEN, PKey);
	RetCode = w_LoadEntry(CD, PKey);
	if (RetCode != wperr_NoError)
	    return (RetCode == wperr_BTreeTempFail ?
		    wperr_IndexedRecordNotFound :
		    RetCode);
    }
    if (CD->Entries[FieldIx] == NULL) return wperr_NoSuchField;
    if (CD->Entries[FieldIx][0] == '\0') return wperr_NoSuchField;
    *FValPtr = CD->Entries[FieldIx];
#if LogsYes
    Log(201, "cwp_Read returning");
#endif /* LogsYes */
    return wperr_NoError;
}

int wp_RetryThis(cod)
wp_ErrorCode cod;
{/* Return non-0 if this error code should be retried and 0 if it shouldn't. */
    int btCod;

    if (cod >= wperr_BTreeBaseValue) {
	btCod = cod - wperr_BTreeBaseValue;
	if (btCod >= bterr_FileSystemErrorBegin && btCod <= bterr_FileSystemErrorEnd) {
	    btCod -= bterr_FileSystemErrorBegin;
	    return (btCod == ENOENT || tfail(btCod));
	} else {
	    switch (btCod) {
		case bterr_OutOfMemory:
		case bterr_NoFileNamesLeft:
		case bterr_BTreeNotCurrVersion:
		case bterr_BTreeDamaged:
		case bterr_NotOpeningRoot:
		case bterr_CursorTreeDamaged:
		case bterr_InternalInconsistency:
		    return TRUE;
		default:
		    return FALSE;
	    }
	}
    } else if (cod >= wperr_FileSystemErrorBegin && cod <= wperr_FileSystemErrorEnd) {
	return tfail(cod - wperr_FileSystemErrorBegin);
    } else {
	switch (cod) {
	    case wperr_OutOfMemory:
	    case wperr_IndeterminateResult:
	    case wperr_InternalError:
	    case wperr_UndifferentiatedFileSystemError:
	    case wperr_UndifferentiatedGritsError:
	    case wperr_UndifferentiatedBTreeError:
	    case wperr_BTreeTempFail:
	    case wperr_IndexedRecordNotFound:
		return TRUE;
	    default:
		return FALSE;
	}
    }
}

static wp_ErrorCode cwp_GetOnly(cd, DoNID, Probe, PKPtr)
struct wp_cd *cd; int DoNID; char *Probe; wp_PrimeKey *PKPtr;
{/* Get the NID or UID from the cell cd, with no constraints. */
    struct wp_CD *CD = (struct wp_CD *) cd;
    wp_ErrorCode RetVal;
    int ResultNumberTruncated, LoopNum;
    wp_PrimeKeySet *PKs;
    struct IdSet *IS;

    if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
    LoopNum = 1;
    for (;;) {		/* try several times on temp failure */
	PKs = NULL;
	IS = Undef_IdSet;
	RetVal = wperr_OutOfMemory;
	ResultNumberTruncated = 0;
	RetVal = w_ProbeMatches(CD,
				(DoNID ? BTIxNI : BTIxID),
				Probe, &IS);
	if (RetVal == wperr_NoError && IS->IdCount == 0)
	    RetVal = wperr_NoKeysFound;
	if (RetVal == wperr_NoError) break;	/* out of retry loop */
	else {
	    w_ZapIdSet(IS); IS = NULL;
	    if (!wp_RetryThis(RetVal)) return RetVal;
	    if (LoopNum >= 5) return (RetVal == wperr_BTreeTempFail ? wperr_IndexedRecordNotFound : RetVal);
	    ++LoopNum;
	    if (wp_Debugging)
		fprintf(stderr, "cwp_GetOnly got temp fail (%d/%d); retrying...\n", RetVal, LoopNum);
	    if (cwp_ReInitialize((struct wp_cd *) CD) != wperr_NoError) sleep(3);
	}
    }
    if (IS->IdCount > 1) {w_ZapIdSet(IS); return wperr_TooManyKeysFound;}
    RetVal = w_IDtoPKSet(IS, &PKs, 1, &ResultNumberTruncated);
    w_ZapIdSet(IS); IS = NULL;
    if (RetVal != wperr_NoError) return RetVal;
    if (ResultNumberTruncated) return wperr_TooManyKeysFound;
    else {
	*PKPtr = PKs->Keys[0];
	PKs->Keys[0] = NULL;
	RetVal = wp_DeAllocate((char **) PKs);
	if (RetVal != wperr_NoError) return RetVal;
	return wperr_NoError;
    }
}

wp_ErrorCode cwp_GetNIDOnly(cd, NID, PKPtr)
struct wp_cd *cd; int NID; wp_PrimeKey *PKPtr;
{/* Get the NID from the cell cd, with no constraints. */
    char ThisNID[16];
    sprintf(ThisNID, "%d", NID);
    return cwp_GetOnly(cd, 1, ThisNID, PKPtr);
}

wp_ErrorCode cwp_GetUIDOnly(cd, UID, PKPtr)
struct wp_cd *cd; char *UID; wp_PrimeKey *PKPtr;
{/* Get the UID from the cell cd, with no constraints. */
    return cwp_GetOnly(cd, 0, UID, PKPtr);
}

static struct wp_CD *CDRoot = NULL;

static void ClearCD(cd)
struct wp_CD *cd;
{
    int Ix;

    strncpy(cd->LastRetrieved.RecID, NOT_AN_IDENT, PKLEN);
    for (Ix = FldCOUNT - 1; Ix >= 0; --Ix) {
	cd->Entries[Ix] = NULL; cd->EntriesSize[Ix] = 0;
    }
    cd->PrevPK[0] = '\0';
}

wp_ErrorCode cwp_ReInitialize(cd)
struct wp_cd *cd;
{/* Re-open the B-tree. */
    bt_ErrorCode BTErr;
    char *wpRootName;
    char FileCell[100];
    struct wp_CD *CD = (struct wp_CD *) cd;
    struct BTree *Tree; struct btCursor *Curs;

    if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
    if (CD->TimesInited < 1) return wperr_NoError;

    wpRootName = malloc(strlen(CD->DirName) + 4);
    if (wpRootName == NULL) return wperr_OutOfMemory;
    sprintf(wpRootName, "%s/wp", CD->DirName);
    if (CD->CellName != NULL) {	/* check cell match */
	if (GetCellFromFileName(wpRootName, FileCell, sizeof(FileCell)) == 0) {
	    if (ULstrcmp(FileCell, CD->CellName) != 0) {
		free(wpRootName);
		return wperr_WPInWrongCell;
	    }
	}
    }
#ifdef AFS_ENV
    if (ViceIsRunning()) {
	struct ViceIoctl blob;
	blob.in = NULL;
	blob.in_size = 0;
	blob.out = NULL;
	blob.out_size = 0;
	(void) pioctl(ViceFile, VIOCCKBACK, &blob, 1);
    }
#endif /* #ifdef AFS_ENV */

    BTErr = bt_Open(&Tree, wpRootName, "r");
    free(wpRootName);
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    BTErr = bt_NewCursor(Tree, &Curs);
    if (BTErr != bterr_NoError) {
	bt_Close(Tree);
	return w_BTreeErr(BTErr);
    }
    if (wp_Debugging) fprintf(stderr, "cwp_ReInitialize: Called bt_Open and bt_NewCursor.\n");
    ClearCD(CD);

    bt_Close(CD->tree);
    CD->tree = Tree;
    CD->cursor = Curs;
    return wperr_NoError;
}

static wp_ErrorCode InitializeNewCD(cd, cellName)
struct wp_CD *cd; char *cellName;
{
    bt_ErrorCode BTErr;
    char *wpRootName;
    char FileCell[100];

    cd->Tag = wpcdTag;
    cd->Next = cd->Prev = cd;
    cd->TimesInited = 0;
    ClearCD(cd);

    wpRootName = malloc(strlen(cd->DirName) + 4);
    if (wpRootName == NULL) return wperr_OutOfMemory;
    sprintf(wpRootName, "%s/wp", cd->DirName);
    if (cellName != NULL) {	/* check cell match */
	if (GetCellFromFileName(wpRootName, FileCell, sizeof(FileCell)) == 0) {
	    if (ULstrcmp(FileCell, cellName) != 0) {
		free(wpRootName);
		return wperr_WPInWrongCell;
	    }
	}
    }

    BTErr = bt_Open(&(cd->tree), wpRootName, "r");
#if LogsYes
    Log(102, "bt_Open() returned; calling bt_NewCursor");
#endif /* LogsYes */
    free(wpRootName);
    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
    BTErr = bt_NewCursor(cd->tree, &(cd->cursor));
#if LogsYes
    Log(103, "bt_NewCursor returned.");
#endif /* LogsYes */
    if (BTErr != bterr_NoError) {
	bt_Close(cd->tree);
	return w_BTreeErr(BTErr);
    }
    if (wp_Debugging) fprintf(stderr, "Called bt_Open and bt_NewCursor.\n");
    /* Now link it on to the list of all CD records. */
    if (CDRoot != NULL) {
	cd->Next = CDRoot;
	cd->Prev = CDRoot->Prev;
	CDRoot->Prev->Next = cd;
	CDRoot->Prev = cd;
    }
    CDRoot = cd;	/* it was already a one-element list */

    cd->TimesInited = 1;
    return wperr_NoError;
}

wp_ErrorCode wp_InitializeDir(dirname, cdp)
char *dirname; struct wp_cd **cdp;
{
    struct wp_CD *cd, *head;
    wp_ErrorCode res;

    if (CDRoot != NULL) {
	head = NULL;
	for (cd = CDRoot; cd != head; cd = cd->Next) {
	    head = CDRoot;
	    if (strcmp(dirname, cd->DirName) == 0) {
		++(cd->TimesInited);
		*cdp = (struct wp_cd *) cd;
		return wperr_NoError;
	    }
	}
    }
    cd = (struct wp_CD *) malloc(sizeof(struct wp_CD));
    if (cd == NULL) return wperr_OutOfMemory;
    cd->CellName = NULL;
    cd->DirName = NewString(dirname);
    if (cd->DirName == NULL) {free(cd); return wperr_OutOfMemory;}
    res = InitializeNewCD(cd, NULL);
    if (res != wperr_NoError) {
	free(cd->DirName);
	free(cd);
	return res;
    }
    *cdp = (struct wp_cd *) cd;
    return wperr_NoError;
}

wp_ErrorCode wp_InitializeCell(cellname, cdp)
char *cellname; struct wp_cd **cdp;
{
    struct wp_CD *cd, *head;
    char *dirName;
    static char *OverrideName;
    static int OverrideChecked = 0;
    int DoOverride;
    wp_ErrorCode res;

    if (CDRoot != NULL) {
	head = NULL;
	for (cd = CDRoot; cd != head; cd = cd->Next) {
	    head = CDRoot;
	    if (cd->CellName != NULL
		&& strcmp(cellname, cd->CellName) == 0) {
		++(cd->TimesInited);
		*cdp = (struct wp_cd *) cd;
		return wperr_NoError;
	    }
	}
    }
    DoOverride = 0;
    CheckServiceConfiguration();
    if (ULstrcmp(cellname, ThisDomain) == 0) {
	if (OverrideChecked == 0) {
	    OverrideName = (char *) GetConfiguration("ThisDomainWPDirectory");
	    OverrideChecked = 1;
	}
	if (OverrideName != NULL && OverrideName[0] != '\0') DoOverride = 1;
    }
    if (DoOverride) {
	dirName = OverrideName;
    } else {
	dirName = malloc(strlen(cellname) + strlen(CellCommonPrefix) + strlen(CellCommonSuffix) + strlen(CellCommonWPDirSuffix) + 1);
	if (dirName == NULL) return wperr_OutOfMemory;
	strcpy(dirName, CellCommonPrefix);
	LCappend(dirName, cellname);
	strcat(dirName, CellCommonSuffix);
	strcat(dirName, CellCommonWPDirSuffix);
    }
    if (CDRoot != NULL) {
	head = NULL;
	for (cd = CDRoot; cd != head; cd = cd->Next) {
	    head = CDRoot;
	    if (strcmp(dirName, cd->DirName) == 0) {
		if (cd->CellName == NULL) {/* fill it in if we can */
		    auto char dirC[100];
		    if (!DoOverride && GetCellFromFileName(dirName, dirC, sizeof(dirC)) == 0
			&& ULstrcmp(dirC, cellname) != 0) {
			if (!DoOverride) free(dirName);
			return wperr_WPInWrongCell;
		    }
		    cd->CellName = NewString(cellname);
		}
		if (!DoOverride) free(dirName);
		++(cd->TimesInited);
		*cdp = (struct wp_cd *) cd;
		return wperr_NoError;
	    }
	}
    }
    cd = (struct wp_CD *) malloc(sizeof(struct wp_CD));
    if (cd == NULL) {
	if (!DoOverride) free(dirName);
	return wperr_OutOfMemory;
    }
    cd->CellName = NewString(cellname);
    if (cd->CellName == NULL) {
	if (!DoOverride) free(dirName);
	free(cd);
	return wperr_OutOfMemory;
    }
    if (DoOverride) {
	dirName = NewString(dirName);
	if (dirName == NULL) {
	    free(cd->CellName);
	    free(cd);
	    return wperr_OutOfMemory;
	}
    }
    cd->DirName = dirName;
    res = InitializeNewCD(cd, cellname);
    if (res != wperr_NoError) {
	free(dirName);
	free(cd->CellName);
	free(cd);
	return res;
    }
    *cdp = (struct wp_cd *) cd;
    return wperr_NoError;
}

wp_ErrorCode cwp_Terminate(cd)
struct wp_cd *cd;
{
    bt_ErrorCode BTErr;
    int Ix;
    struct wp_CD *CD = (struct wp_CD *) cd;

    if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
    if (CD->TimesInited < 1) CD->TimesInited = 1;
    if (--(CD->TimesInited) > 0) return wperr_NoError;
#if LogsYes
    Log(150, "cwp_Terminate() calling bt_Close");
#endif /* LogsYes */
    BTErr = bt_Close(CD->tree);
#if LogsYes
    Log(151, "bt_Close returned");
#endif /* LogsYes */
    for (Ix = FldCOUNT - 1; Ix >= 0; --Ix)
	if (CD->Entries[Ix] != NULL) free(CD->Entries[Ix]);
    if (CD->DirName != NULL) free(CD->DirName);
    if (CD->CellName != NULL) free(CD->CellName);

    if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);

    strncpy(CD->LastRetrieved.RecID, NOT_AN_IDENT, PKLEN);

    if (CDRoot == CD) CDRoot = CD->Next;
    if (CD == CD->Next) {	/* This is the only entry on the list */
	CDRoot = NULL;	/* don't point to it any more */
    } else {
	CD->Next->Prev = CD->Prev;	/* unlink this one from the list */
	CD->Prev->Next = CD->Next;
    }
    CD->Tag = 0;
    free(CD);

    return wperr_NoError;
}
