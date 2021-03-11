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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/wpname.c,v 2.22 1993/09/29 23:53:49 gk5g Exp $";
#endif

/* ************************************************************ *\
	wpname.c
	Library routines for White Pages lookups.
	Include file ``wp.h'' declares the procedures for clients.
\* ************************************************************ */

#include <andyenv.h>
#include <stdio.h>
#include <andrewos.h>		/* sys/file.h sys/types.h sys/time.h strings.h */
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
extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

extern int wp_Debugging;

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */
#ifndef LogsYes
#define LogsYes 0
#endif /* LogsYes */

static wp_ErrorCode CopyIdSet(Source, Dest)
struct IdSet *Source, **Dest;
{	/* Make a copy. */
	struct IdSet *D;
	int Ix;

	D = (struct IdSet *) malloc(sizeof(struct IdSet));
	if (D == NULL) return wperr_OutOfMemory;
	D->Tag = IdSetTag;
	D->IdCount = Source->IdCount;
	D->IdMax = Source->IdCount;
	D->Ids = (btwp_identifier *) malloc(Source->IdCount * sizeof(btwp_identifier));
	if (D->Ids == NULL) {free(D); return wperr_OutOfMemory;}
	for (Ix = Source->IdCount-1; Ix >= 0; --Ix)
		strncpy(D->Ids[Ix].RecID, Source->Ids[Ix].RecID, PKLEN);
	*Dest = D;
	return wperr_NoError;
}

#define NamSetTag -75
struct NamSet {
		int		Tag;
		int		NamCount;
		int		NamMax;
		char		**Nams;	/* the character-string tokens themselves */
		int		*NamSizs;	/* sizes of the malloc'ed space in Nams */
		int		SurName;
};
#define Undef_NamSet	((struct NamSet *) 0)
#define Empty_NamSet	((struct NamSet *) -1)

static void ZapNamSet(SS)
struct NamSet *SS;
{/* Carefully deallocate all things referred to by SS. */
	int Idx;

	if (SS != Undef_NamSet && SS != Empty_NamSet) {
		if ((SS->Tag==0 || SS->Tag==NamSetTag)) {
		    if (SS->Nams != NULL) {
			for (Idx = SS->NamCount-1; Idx >= 0; --Idx) {
				if (SS->Nams[Idx] != NULL) free(SS->Nams[Idx]);
			}
			free(SS->Nams);
		    }
		    if (SS->NamSizs != NULL) free(SS->NamSizs);
		}
		free(SS);
	}
}
#define InitNamSetSize 5
static struct NamSet *NewNamSet()
{ /* Allocate a new basic NamSet. */
	struct NamSet *SS;
	int Ix;

	SS = (struct NamSet *) malloc(sizeof(struct NamSet));
	if (SS == NULL) return Undef_NamSet;
	SS->Tag = NamSetTag;
	SS->NamCount = 0;
	SS->SurName = -1;
	SS->NamMax = InitNamSetSize;
	SS->Nams = (char **) malloc(InitNamSetSize * sizeof(char *));
	if (SS->Nams == NULL) {
		free(SS); return Undef_NamSet;
	}
	SS->NamSizs = (int *) malloc(InitNamSetSize * sizeof(int));
	if (SS->NamSizs == NULL) {
		free(SS->Nams); free(SS); return Undef_NamSet;
	}
	for (Ix = 0; Ix < SS->NamMax; ++Ix) {SS->Nams[Ix] = NULL; SS->NamSizs[Ix] = 0;}
	return SS;
}

static wp_ErrorCode FreshenNamSet(NSPtr)
struct NamSet **NSPtr;
{/* Make it appear that the NS that NSPtr points to is squeaky-clean. */
	struct NamSet *NS;
	int Ix;

	NS = *NSPtr;
	if (NS == Undef_NamSet || NS == Empty_NamSet) {
		NS = NewNamSet();
		if (NS == Undef_NamSet || NS == Empty_NamSet) return wperr_OutOfMemory;
		*NSPtr = NS;
		return wperr_NoError;
	}
	if (NS->Tag != NamSetTag) return wperr_InternalError;
	NS->NamCount = 0;
	NS->SurName = -1;
	for (Ix = 0; Ix < NS->NamMax; ++Ix) if (NS->Nams[Ix] != NULL) NS->Nams[Ix][0] = '\0';
	return wperr_NoError;
}

static wp_ErrorCode AddNamSet(NSPtr, Str)
struct NamSet **NSPtr; char *Str;
{ /* Add string Str to token set SS.  We need to malloc a copy of Str. */
	char	**NewNams;
	int	NewSize, NewStrSize, Ix, *NewNamSizs;
	wp_ErrorCode RetVal;
	struct NamSet *SS;

	SS = *NSPtr;
	if (SS->NamCount >= SS->NamMax) {
		NewSize = SS->NamCount + 4;
		NewNams = (char **) malloc(NewSize * sizeof(char *));
		if (NewNams == NULL) {
			ZapNamSet(SS); return wperr_OutOfMemory;
		}
		NewNamSizs = (int *) malloc(NewSize * sizeof(int));
		if (NewNamSizs == NULL) {
			ZapNamSet(SS); free(NewNams); return wperr_OutOfMemory;
		}
		for (Ix = SS->NamCount-1; Ix >= 0; --Ix) {
			NewNams[Ix] = SS->Nams[Ix];
			NewNamSizs[Ix] = SS->NamSizs[Ix];
		}
		for (Ix = NewSize-1; Ix >= SS->NamCount; --Ix) {
			NewNams[Ix] = NULL;
			NewNamSizs[Ix] = 0;
		}
		free(SS->Nams);
		SS->Nams = NewNams;
		free(SS->NamSizs);
		SS->NamSizs = NewNamSizs;
		SS->NamMax = NewSize;
	}
	NewStrSize = strlen(Str) + 1;
	Ix = SS->NamCount;
	if (NewStrSize > SS->NamSizs[Ix]) {
		RetVal = w_GrowString(&SS->Nams[Ix], &SS->NamSizs[Ix], NewStrSize);
		if (RetVal != wperr_NoError) {ZapNamSet(SS); return RetVal;}
	}
	strcpy(SS->Nams[Ix], Str);
	++SS->NamCount;
	*NSPtr = SS;
	return wperr_NoError;
}

#define TokSetTag -72
struct TokSet {
		int		Tag;
		int		TokCount;
		char		**Strs;	/* the character-string tokens themselves */
		int		Surname;
		struct IdSet	**Matches;
		struct IdSet	**Begins;
		struct IdSet	*SurnMatch;
		struct IdSet	*SurnBegin;
		char		**OvMaps;	/* for each token, the override string, if any */
		struct IdSet	**OvSets;		/* for each token, what the override string matches */
		char			**PhonGivenStrs;	/* for each token, the CanonGiven result */
		struct IdSet	**PhonGivenSets;	/* for each token, the exact phonetic matches */
		char			*PhonSurnStr;	/* the CanonSurn result */
		struct IdSet	*PhonSurnSet;	/* the exact CanonSurn matches */
		char			**PhonNickStrs;	/* for each token, the CanonNick result */
		struct NamSet	**PhonNickMaps;	/* for each token, the phonetic nickname mappings */
		struct IdSet	**PhonNickSets;	/* for each token, the nickname matches */
		struct IdSet	*PhonAbbrSurnSet;	/* what CanonSurn abbreviates */
};
#define Undef_Str	((char *) 0)
#define Empty_Str	((char *) -1)
static void ZapTokSet(SS)
struct TokSet *SS;
{/* Carefully deallocate all things referred to by SS. */
	int Idx;

	if (SS != NULL) {
		if ((SS->Tag==0 || SS->Tag==TokSetTag)) {
		    if (SS->Strs != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx) {
				if (SS->Strs[Idx] != NULL) free(SS->Strs[Idx]);
			}
			free(SS->Strs);
		    }
		    if (SS->Matches != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				w_ZapIdSet(SS->Matches[Idx]);
			free(SS->Matches);
		    }
		    if (SS->Begins != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				w_ZapIdSet(SS->Begins[Idx]);
			free(SS->Begins);
		    }
		    if (SS->OvMaps != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				if (SS->OvMaps[Idx] != Undef_Str && SS->OvMaps[Idx] != Empty_Str)
					free(SS->OvMaps[Idx]);
			free(SS->OvMaps);
		    }
		    if (SS->OvSets != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				w_ZapIdSet(SS->OvSets[Idx]);
			free(SS->OvSets);
		    }
		    if (SS->PhonGivenStrs != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				if (SS->PhonGivenStrs[Idx] != NULL) free(SS->PhonGivenStrs[Idx]);
			free(SS->PhonGivenStrs);
		    }
		    if (SS->PhonGivenSets != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				w_ZapIdSet(SS->PhonGivenSets[Idx]);
			free(SS->PhonGivenSets);
		    }
		    if (SS->PhonNickStrs != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				if (SS->PhonNickStrs[Idx] != NULL) free(SS->PhonNickStrs[Idx]);
			free(SS->PhonNickStrs);
		    }
		    if (SS->PhonNickMaps != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				ZapNamSet(SS->PhonNickMaps[Idx]);
			free(SS->PhonNickMaps);
		    }
		    if (SS->PhonNickSets != NULL) {
			for (Idx = SS->TokCount-1; Idx >= 0; --Idx)
				w_ZapIdSet(SS->PhonNickSets[Idx]);
			free(SS->PhonNickSets);
		    }
		    if (SS->SurnMatch != Undef_IdSet && SS->SurnMatch != Empty_IdSet)
			w_ZapIdSet(SS->SurnMatch);
		    if (SS->SurnBegin != Undef_IdSet && SS->SurnBegin != Empty_IdSet)
			w_ZapIdSet(SS->SurnBegin);
		    if (SS->PhonSurnStr != NULL) free(SS->PhonSurnStr);
		    if (SS->PhonSurnSet != Undef_IdSet && SS->PhonSurnSet != Empty_IdSet)
			w_ZapIdSet(SS->PhonSurnSet);
		    if (SS->PhonAbbrSurnSet != Undef_IdSet && SS->PhonAbbrSurnSet != Empty_IdSet)
			w_ZapIdSet(SS->PhonAbbrSurnSet);
		}
		free(SS);
	}
}
static struct TokSet *BuildTokSet(NS)
struct NamSet *NS;
{/* Extend the given NamSet to a TokSet; deallocate the NamSet on success. */
	struct TokSet *TS;
	int Tok;

	TS = (struct TokSet *) malloc(sizeof(struct TokSet));
	if (TS == NULL) return NULL;
	TS->Tag = TokSetTag;
	TS->TokCount = NS->NamCount;;
	TS->Surname = NS->SurName;
	TS->SurnMatch = TS->SurnBegin = TS->PhonSurnSet = TS->PhonAbbrSurnSet = Undef_IdSet;
	TS->PhonSurnStr = NULL;
	TS->Strs = (char **) malloc(NS->NamCount * sizeof(char *));
	TS->Matches = (struct IdSet **) malloc(NS->NamCount * sizeof(struct IdSet *));
	TS->Begins = (struct IdSet **) malloc(NS->NamCount * sizeof(struct IdSet *));
	TS->OvMaps = (char **) malloc(NS->NamCount * sizeof(char *));
	TS->OvSets = (struct IdSet **) malloc(NS->NamCount * sizeof(struct IdSet *));
	TS->PhonGivenStrs = (char **) malloc(NS->NamCount * sizeof(char *));
	TS->PhonGivenSets = (struct IdSet **) malloc(NS->NamCount * sizeof(struct IdSet *));
	TS->PhonNickStrs = (char **) malloc(NS->NamCount * sizeof(char *));
	TS->PhonNickMaps = (struct NamSet **) malloc(NS->NamCount * sizeof(struct NamSet *));
	TS->PhonNickSets = (struct IdSet **) malloc(NS->NamCount * sizeof(struct IdSet *));
	if (TS->Strs == NULL || TS->Matches == NULL || TS->Begins == NULL ||
	    TS->OvMaps == NULL || TS->OvSets == NULL || TS->PhonGivenStrs == NULL ||
	    TS->PhonGivenSets == NULL || TS->PhonNickStrs == NULL ||
	    TS->PhonNickMaps == NULL) {
		if (TS->Strs != NULL) free(TS->Strs);
		if (TS->Matches != NULL) free(TS->Matches);
		if (TS->Begins != NULL) free(TS->Begins);
		if (TS->OvMaps != NULL) free(TS->OvMaps);
		if (TS->OvSets != NULL) free(TS->OvSets);
		if (TS->PhonGivenStrs != NULL) free(TS->PhonGivenStrs);
		if (TS->PhonGivenSets != NULL) free(TS->PhonGivenSets);
		if (TS->PhonNickStrs != NULL) free(TS->PhonNickStrs);
		if (TS->PhonNickMaps != NULL) free(TS->PhonNickMaps);
		if (TS->PhonNickSets != NULL) free(TS->PhonNickSets);
		free(TS); return NULL;
	}
	for (Tok = TS->TokCount - 1; Tok >= 0; --Tok) {
		TS->Strs[Tok] = NS->Nams[Tok]; NS->Nams[Tok] = NULL;
		TS->Matches[Tok] = Undef_IdSet;
		TS->Begins[Tok] = Undef_IdSet;
		TS->OvMaps[Tok] = Undef_Str;
		TS->OvSets[Tok] = Undef_IdSet;
		TS->PhonGivenStrs[Tok] = NULL;
		TS->PhonGivenSets[Tok] = Undef_IdSet;
		TS->PhonNickStrs[Tok] = NULL;
		TS->PhonNickMaps[Tok] = Undef_NamSet;
		TS->PhonNickSets[Tok] = Undef_IdSet;
	}
	ZapNamSet(NS);
	return TS;
}

static char ToLower(C)
char C;
{ if (C >= 'A') if (C <= 'Z') C += ('a' - 'A');
   return C;
}

static void PrintSrchToken(file, ST)
FILE *file; struct wp_SrchToken *ST;
{
	struct wp_Constraint *CPtr;

	fprintf(file, "(%#x) Match ``%s''/%d", ST, ST->Probe, ST->SearchKind);
	CPtr = ST->Constraints;
	while (CPtr != NULL) {
		fprintf(file, ", with %s ", wpFieldName[CPtr->FieldNum]);
		switch (CPtr->ConstraintKind) {
			case wpc_WholeKey:	fprintf(file, "of"); break;
			case wpc_BeginKey:	fprintf(file, "starting with"); break;
			case wpc_ContainKey:	fprintf(file, "containing"); break;
			default:	fprintf(file, "<<%d>>", CPtr->ConstraintKind); break;
		}
		fprintf(file, " ``%s''", CPtr->FieldContent);
		CPtr = CPtr->Next;
	}
	fprintf(file, ".\n");
}

/*
Begin setting up for a new database search.  This initial search specifies that the NameProbe string is to be searched for according to the style given by the SearchKind parameter.  This search can therefore look, for example, at numeric IDs only, Unix IDs only, Unix IDs before names, Unix IDs before name abbreviations, and Unix IDs last of all.  The routine returns a pointer to an unspecified search token through the out parameter STPointer; this token can then be used for actual database searches.

This routine malloc's storage to hold the search token.  It is the client program's responsibility to call wp_DeAllocate of that storage to deallocate it.  If storage allocation fails, this routine will have allocated nothing and will return code wperr_OutOfMemory.
*/
wp_ErrorCode wp_SetUp(NameProbe, SearchKind, STPointer)
char *NameProbe;  int SearchKind; wp_SearchToken *STPointer;
{
	struct wp_SrchToken *ST;

#if Logs
	Log(710, "wp_SetUp called");
#endif /* Logs */
	ST = (struct wp_SrchToken *) malloc(sizeof(struct wp_SrchToken));
	if (ST == NULL) return wperr_OutOfMemory;
	ST->Tag = SrchTokenTag;
	ST->Constraints = NULL;
	ST->Probe = NewString(NameProbe);
	if (ST->Probe == NULL) {free(ST); return wperr_OutOfMemory;}
	ST->SearchKind = SearchKind;

	if (wp_Debugging) {fprintf(stderr, "wp_SetUp returns "); PrintSrchToken(stderr, ST);}

	*STPointer = (wp_SearchToken) ST;
#if Logs
	Log(711, "wp_SetUp returning");
#endif /* Logs */
	return wperr_NoError;
}

/*
Constrain the search implied by the search token pointed to by STPointer so
 that the field FieldNum must begin with the string given in FieldContent.
  ConstraintKind dictates how the given Content must relate to the contents of
 the field specified by FieldNum.  The STPointer pointer is modified to point to
 a new, also unspecified, piece of storage.

This routine malloc's storage to hold the search token.  It is the client
 program's responsibility to call wp_DeAllocate of that storage to deallocate it.
  If storage allocation fails, this routine will have allocated nothing and will
 return code wperr_OutOfMemory.
*/
wp_ErrorCode wp_Constrain(STPointer, FieldNum, Content, ConstraintKind)
wp_SearchToken *STPointer; wp_FieldIndex FieldNum;
char *Content; enum wp_ConstraintKind ConstraintKind;
{
	struct wp_SrchToken *ST = (struct wp_SrchToken *) *STPointer;
	struct wp_Constraint *CP;

#if Logs
	Log(720, "wp_Constrain called");
#endif /* Logs */
	CP = (struct wp_Constraint *) malloc(sizeof(struct wp_Constraint));
	if (CP == NULL) return wperr_OutOfMemory;
	CP->Tag = ConstraintTag;
	CP->FieldNum = FieldNum;
	CP->FieldContent = NewString(Content);
	if (CP->FieldContent == NULL) {free(CP); return wperr_OutOfMemory;}
	CP->ConstraintKind = ConstraintKind;

	CP->Next = ST->Constraints;	/* link it to ST's constraint list */
	ST->Constraints = CP;

	if (wp_Debugging) {fprintf(stderr, "wp_Constrain returns "); PrintSrchToken(stderr, ST);}

#if Logs
	Log(721, "wp_Constrain returning");
#endif /* Logs */
	return wperr_NoError;
}

/* Test whatever constraints are declared in the Constraints list of a search.
	Return TRUE iff the given identifier obeys the given constraint list.
*/
static wp_ErrorCode TestConstraints(cd, Ident, CList, Passed)
struct wp_CD *cd; btwp_identifier Ident; struct wp_Constraint *CList; int *Passed;
{
	struct wp_Constraint *CPtr;
	char *FContent;
	wp_ErrorCode WPErr;

	if (wp_Debugging) fprintf(stderr, "TestConstraints(%u, %#x) called.\n", Ident, CList);
	if (CList == NULL) {*Passed = TRUE; return wperr_NoError;}
					/* no constraints to test. */
#if Logs
	Log(600, "TestConstraints called on non-null constraints");
#endif /* Logs */
	*Passed = FALSE;	/* maybe overwrite it */
	for (CPtr = CList; CPtr != NULL; CPtr = CPtr ->Next) {
		if (CPtr->Tag != ConstraintTag) return wperr_NoError;
		if (strncmp(cd->LastRetrieved.RecID, Ident.RecID, PKLEN) != 0) {
			if (wp_Debugging) fprintf(stderr,
				"Loading entry with identifier %.*s.\n", PKLEN, Ident.RecID);
			WPErr = w_LoadEntry(cd, Ident.RecID);
			if (WPErr != wperr_NoError) return WPErr;
		}
		FContent = cd->Entries[CPtr->FieldNum];
		if (FContent == NULL) return wperr_NoError;	/* no field */
		if (FContent[0] == '\0') return wperr_NoError;	/* null field value */
		switch (CPtr->ConstraintKind) {
	  case wpc_WholeKey:
		if (ULstrcmp(FContent, CPtr->FieldContent) != 0) 	return wperr_NoError;
		break;
	  case wpc_BeginKey:
		if (ULstlmatch(FContent, CPtr->FieldContent) == 0)	return wperr_NoError;
		break;
	  case wpc_ContainKey:
		if (ULsindex(FContent, CPtr->FieldContent) == 0) 	return wperr_NoError;
		break;
	  default:
		if (wp_Debugging) fprintf(stderr,
			"Constraint kind %d not implemented.\n", CPtr->ConstraintKind);
		return wperr_NoError;
		}	/* end of switch stmt */
	}
#if Logs
	Log(602, "TestConstraints returning TRUE");
#endif /* Logs */
	*Passed = TRUE;
	return wperr_NoError;
}

/* Apply whatever constraints are declared in the Constraints list of a search.
	Return a constrained version of the IdSet.
	Consume the argument IdSet (deallocate or shrink);
	return a new version (allocated or shrunk). */
static wp_ErrorCode ApplyConstraints(cd, ISPtr, CList, ISCPtr)
struct wp_CD *cd; struct IdSet **ISPtr; struct wp_Constraint *CList; int *ISCPtr;
{
	struct IdSet *IS = *ISPtr;
	struct IdSet *NewIS;
	btwp_identifier *PSrc, *PDst;
	int	DstCount, Ix, Passed;
	wp_ErrorCode Ret;

	if (wp_Debugging)
		fprintf(stderr, "ApplyConstraints(%#x): %d identifiers, room for %d.\n",
				IS, IS->IdCount, IS->IdMax);
	if (CList == NULL) return wperr_NoError;	/* no constraints to apply. */

#if LogsYes
	Log(660, "ApplyConstraints called on non-null constraint list");
#endif /* LogsYes */
	/* For each ID in the set, apply the list of constraints to that ID. */
	/* apply the constraints to the given identifier. */
	if (*ISCPtr == 0) {
		Ret = CopyIdSet(*ISPtr, &NewIS);
		if (Ret != wperr_NoError) return Ret;
		*ISPtr = NewIS;
		*ISCPtr = 1;	/* we've made a copy */
		IS = NewIS;
	}
	DstCount = 0;
	PDst = IS->Ids;
	PSrc = PDst;
	for (Ix = IS->IdCount; Ix > 0; --Ix) {
		Ret = TestConstraints(cd, *PSrc, CList, &Passed);
		if (Ret != wperr_NoError) return Ret;
		if (Passed) {
			*PDst++ = *PSrc;
			DstCount++;
		}
		PSrc++;
	}

#if LogsYes
	Log(661, "ApplyConstraints: old %d, new %d", IS->IdCount, DstCount);
#endif /* LogsYes */
	if (DstCount == IS->IdCount) return wperr_NoError;	/* no restriction */
	if (DstCount == 0) {
		if (*ISCPtr) w_ZapIdSet(IS);
		*ISPtr = NULL;
		return wperr_NoKeysFound;
	}
/* Avoid making a copy of the shrunken set--just carry the extra memory around for now. */
	if (wp_Debugging) fprintf(stderr,
				"ApplyConstraints reduced ID list from %d to %d.\n",
				IS->IdCount, DstCount);
	IS->IdCount = DstCount;
#if LogsYes
	Log(662, "ApplyConstraints: returning the new set");
#endif /* LogsYes */
	return wperr_NoError;
}

static int AnyDigits(str)
char *str;
{/* Return 1 iff there are any digits in the argument string str. */
	register char *S;

	for (S = str; *S != '\0'; ++S) if (isdigit(*S)) return 1;
	return 0;
}

static struct IdSet *IntersectIDs(S1, S2)
struct IdSet *S1, *S2;
{ /* Assume that the IdSets S1 and S2 are sorted.
	Perform the operation ``S1 := S1 intersect S2'', overwriting S1.
*/
	btwp_identifier *PSrc, *PDst;
	int	DstCount, P1Count, P2, P2Max, IdIx;

	if (S1 == Empty_IdSet || S2 == Empty_IdSet) return Empty_IdSet;
#if LogsYes
	Log(680, "IntersectIDs given sets of size %d and %d", S1->IdCount, S2->IdCount);
#endif /* LogsYes */
	if (wp_Debugging)
		fprintf(stderr, "IntersectIDs(%#x (%d), %#x (%d)): ",
				S1, S1->IdCount, S2, S2->IdCount);
	DstCount = 0;
	PDst = S1->Ids;
	PSrc = PDst;
	P2 = 0;
	P2Max = S2->IdCount;
	for (P1Count = S1->IdCount; P1Count > 0 && P2 < P2Max; --P1Count) {
		while (strncmp(S2->Ids[P2].RecID, PSrc->RecID, PKLEN) < 0 && P2 < P2Max)
				++P2;
		if (P2 < P2Max && strncmp(PSrc->RecID, S2->Ids[P2].RecID, PKLEN) == 0) {
			strncpy(PDst->RecID, PSrc->RecID, PKLEN);
			++PDst;
			++DstCount;
		}
		PSrc++;
	}
	S1->IdCount = DstCount;
	if (wp_Debugging) {
		fprintf(stderr, "returns %d btwp_id(s)", DstCount);
		for (IdIx = 0; IdIx < DstCount; IdIx++)
			fprintf(stderr, " %.*s", PKLEN, S1->Ids[IdIx].RecID);
		fprintf(stderr, ".\n");
	}

#if LogsYes
	Log(681, "IntersectIDs returning set of size %d", DstCount);
#endif /* LogsYes */
	return S1;
}

static char *KMKey = NULL;
static int KMKeyLen = 0;

static wp_ErrorCode KeyMatch(cd, BTIx, Probe, ResPtr)
struct wp_CD *cd; int BTIx; char *Probe, **ResPtr;
{/* Returns via ResPtr any value that matches Probe viewed as a BTIx. */
	int	PLen, DataLen;
	bt_ErrorCode BTErr; wp_ErrorCode WPErr;
	char *KeyValue;

	PLen = strlen(Probe);
	if (wp_Debugging) fprintf(stderr, "KeyMatch looking for entries with %s fields of ``%s''.\n",
			btIndexNames[BTIx], Probe);
	PLen += 3;
	if (KMKeyLen < PLen) {
		WPErr = w_GrowString(&KMKey, &KMKeyLen, PLen);
		if (WPErr != wperr_NoError) return WPErr;
	}
	sprintf(KMKey, "%s%c%c", Probe, KeyTagSep, BTIxTags[BTIx]);
	w_LowerAll(KMKey);
	cd->PrevPK[0] = '\0';
	BTErr = bt_Search(cd->cursor, KMKey);
	if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
	if (bt_GetCursorState(cd->cursor) != AtKey) return wperr_NoKeysFound;
	BTErr = bt_GetCursorValue(cd->cursor, &KeyValue, &DataLen);
	if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);

	if (wp_Debugging)
		fprintf(stderr, "KeyMatch returning a %d-char string: ``%s''.\n",
				DataLen, KeyValue);
	*ResPtr = KeyValue;
	return wperr_NoError;
}

static char *PBKey = NULL;
static int PBKeyLen = 0;

static wp_ErrorCode w_ProbeBegins(cd, BTIx, Probe, ISPtr)
struct wp_CD *cd; int BTIx; char *Probe; struct IdSet **ISPtr;
{
	register struct IdSet *IS = Undef_IdSet;
	int IdCount, IdIx, PLen, DataLen, DataLen2, EachKeyLen, TempFail;
	int NewMax, NewSize, EachKeyMatches;
	bt_ErrorCode BTErr; wp_ErrorCode WPErr;
	enum bt_CursorState CState;
	char KeySfx[3], *EachKey;

	PLen = strlen(Probe);
#if LogsYes
	Log(760+BTIx, "w_ProbeBegins(%s) called, probe len %d",
			btIndexNames[BTIx], PLen);
#endif /* LogsYes */
	if (wp_Debugging) fprintf(stderr,
			"w_ProbeBegins looking for entries with %s fields beginning with ``%s''.\n",
			btIndexNames[BTIx], Probe);
	if (PLen == 0) {
		fprintf(stderr, "w_ProbeBegins on null probe: returning no-such-keys.\n");
		return wperr_NoKeysFound;
	}
	if (PBKeyLen < (PLen+1)) {
		WPErr = w_GrowString(&PBKey, &PBKeyLen, PLen+1);
		if (WPErr != wperr_NoError) return WPErr;
	}
	strcpy(PBKey, Probe);
	w_LowerAll(PBKey);
	KeySfx[0] = KeyTagSep;
	KeySfx[1] = BTIxTags[BTIx];
	KeySfx[2] = '\0';		/* wrap it */
#if LogsYes
	Log(621, "About to call bt_Search");
#endif /* LogsYes */
	cd->PrevPK[0] = '\0';
	BTErr = bt_Search(cd->cursor, PBKey);
#if LogsYes
	Log(622, "bt_Search returned; about to call bt_GetCursorValueLen");
#endif /* LogsYes */
	if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
	CState = bt_GetCursorState(cd->cursor);
	if (CState == AfterLast) return wperr_NoKeysFound;
	if (CState != AtKey) {
		BTErr = bt_NextCursor(cd->cursor);
		if (BTErr == bterr_NoNextKey) return wperr_NoKeysFound;
		else if (BTErr != bterr_NoError) return w_BTreeErr(BTErr);
	}
	TempFail = 0; EachKeyMatches = 0;
	while (bt_GetCursorState(cd->cursor) == AtKey) {
		BTErr = bt_GetCursorKey(cd->cursor, &EachKey);
		if (BTErr != bterr_NoError) {TempFail = 1; break;}
		EachKeyLen = strlen(EachKey) - 2;	/* now check type of key */
		if (strcmp(&EachKey[EachKeyLen], KeySfx) != 0)
			{free(EachKey); goto NxtKey;}
		EachKey[EachKeyLen] = '\0';	/* Truncate the typecode. */
		if (EachKeyLen > PLen) EachKeyLen = PLen;
		if (strncmp(PBKey, EachKey, EachKeyLen) != 0)
			{free(EachKey); break;}	/* done */
		BTErr = bt_GetCursorValueLen(cd->cursor, &DataLen);
#if LogsYes
		Log(623, "bt_GetCursorValueLen returned; about to build an IdSet");
#endif /* LogsYes */
		if (BTErr != bterr_NoError) {free(EachKey); break;}
		if (DataLen <= 0) {TempFail = 1; free(EachKey); break;}
		IdCount = (DataLen + (PKLEN-1)) / PKLEN;
		if (IS == Undef_IdSet) {
			IS = (struct IdSet *) malloc(sizeof(struct IdSet));
			if (IS == NULL) return wperr_OutOfMemory;
			IS->Tag = IdSetTag;
			NewMax = 16;
			if (NewMax < IdCount) NewMax = IdCount;
			IS->Ids = (btwp_identifier *) malloc(NewMax * sizeof(btwp_identifier));
			if (IS->Ids == NULL) {
				free(IS); free(EachKey);
				return wperr_OutOfMemory;
			}
			IS->IdCount = 0;
			IS->IdMax = NewMax;
		}
		NewSize = IdCount + IS->IdCount;
		if (NewSize > IS->IdMax) {	/* grow it */
			NewMax = (IS->IdMax + 2) * 2;
			if (NewMax < NewSize) NewMax = NewSize;
			if (wp_Debugging) fprintf(stderr,
					"w_ProbeBegins growing set from %d to %d (%d).\n",
					IS->IdMax, NewMax, NewSize);
			IS->Ids = (btwp_identifier *)
					realloc(IS->Ids, NewMax * sizeof(btwp_identifier));
			if (IS->Ids == NULL) {
				free(IS); free(EachKey);
				return wperr_OutOfMemory;
			}
			IS->IdMax = NewMax;
		}
		++EachKeyMatches;
		BTErr = bt_GetCursorValueData(cd->cursor,
				&IS->Ids[IS->IdCount], DataLen, &DataLen2);
		if (BTErr != bterr_NoError) {free(EachKey); break;}
		if (DataLen != DataLen2) {
			w_ZapIdSet(IS); free(EachKey);
			return wperr_BTreeTempFail;
		}
		IS->IdCount = NewSize;
		if (wp_Debugging > 1) fprintf(stderr, "w_ProbeBegins: ``%s'' adds %d\n",
				EachKey, IdCount);
		free(EachKey);
		/* This key's contribution is integrated.  Continue with the next one. */
NxtKey:
		BTErr = bt_NextCursor(cd->cursor);
		if (BTErr == bterr_NoNextKey) {BTErr = wperr_NoError; break;}
		if (BTErr != bterr_NoError) break;
	}
	if (BTErr != bterr_NoError) {
		w_ZapIdSet(IS);
		return (TempFail ? wperr_BTreeTempFail : w_BTreeErr(BTErr));
	}
	if (bt_GetCursorState(cd->cursor) == Error) {w_ZapIdSet(IS); return wperr_BTreeTempFail;}

	if (IS == Undef_IdSet) return wperr_NoKeysFound;
	NewSize = wpSortIxValue(IS->Ids, IS->IdCount);
	if (wp_Debugging) fprintf(stderr, "w_ProbeBegins: %d duplicates out of %d\n",
				(IS->IdCount - NewSize), IS->IdCount);
	IS->IdCount = NewSize;
	if (wp_Debugging) {
		fprintf(stderr, "w_ProbeBegins returning %d btwp_id(s)", IS->IdCount);
		for (IdIx = 0; IdIx < IS->IdCount; IdIx++)
			fprintf(stderr, " %.*s", PKLEN, IS->Ids[IdIx].RecID);
		fprintf(stderr, ".\n");
	}
	*ISPtr = IS;
#if LogsYes
	Log(770+BTIx, "w_ProbeBegins returning a %d-element IdSet", IdCount);
#endif /* LogsYes */
	return wperr_NoError;
}

static unsigned char *PCopy = NULL;
static int PCopyLen = 0;

static wp_ErrorCode BurstName(Probe, NSPtr)
unsigned char *Probe; struct NamSet **NSPtr;
{ /* Burst name Probe into its indexed components. */
	unsigned char *TPtr, SaveCh, *NS;
	int LastWasNTok;
	int CommaIx, TryPrev, J;
	wp_ErrorCode RetVal;
	struct NamSet *This;

#if Logs
	Log(650, "BurstName called");
#endif /* Logs */
	J = strlen((char*)Probe) + 1;
	if (PCopyLen < J) {
		RetVal = w_GrowString(&PCopy, &PCopyLen, J);
		if (RetVal != wperr_NoError) return RetVal;
	}
	strcpy((char*)PCopy, (char*)Probe);
	This = *NSPtr;
	RetVal = FreshenNamSet(&This);
	if (RetVal != wperr_NoError) {*NSPtr = Undef_NamSet; return RetVal;}
	TPtr = PCopy; --TPtr;
	NS = NULL;
	CommaIx = -2;
	LastWasNTok = 0;
	do {
		++TPtr;
		if (wpTokenChar[*TPtr] == 0) {
			if (LastWasNTok != 0) {	/* Token just terminated--save it. */
				SaveCh = *TPtr;
				*TPtr = '\0';
				RetVal = AddNamSet(&This, (char *)NS);
				*TPtr = SaveCh;
				if (RetVal != wperr_NoError) {*NSPtr = Undef_NamSet; return RetVal;}
				if (This == Undef_NamSet || This == Empty_NamSet) {
					*NSPtr = Undef_NamSet; return wperr_OutOfMemory;
				}
				if (*TPtr == ',') CommaIx = This->NamCount;
			}
		} else {
			if (LastWasNTok == 0) NS = TPtr;
		}
		LastWasNTok = wpTokenChar[*TPtr];
	} while (*TPtr != '\0' && *TPtr != ';');

/* Now mark which entries are to be considered surnames */
	TryPrev = FALSE;
	if (CommaIx >= 0 && CommaIx == (This->NamCount-1)) TryPrev = TRUE;
	else if (This->NamCount > 1) {
		NS = (unsigned char *) This->Nams[This->NamCount-1];
		SaveCh = ToLower(*NS);
		if (SaveCh == 'i' || SaveCh == 'j') {
			if (ULstrcmp(NS, "Jr") == 0
			  || ULstrcmp(NS, "III") == 0
			  || ULstrcmp(NS, "IV") == 0
			  || ULstrcmp(NS, "II") == 0) TryPrev = TRUE;
		}
	}
	if (TryPrev && (This->NamCount > 1)) {
		This->SurName = This->NamCount - 2;
	} else {
		This->SurName = This->NamCount - 1;
	}
	if (wp_Debugging) {
		fprintf(stderr, "Burst probe ``%s'' into names:", Probe);
		for (J = 0; J < This->NamCount; J++)
			fprintf(stderr, " ``%s%s", This->Nams[J],
				(This->SurName == J ? "''(s)" : "''"));
		fprintf(stderr, ".\n");
	}
#if Logs
	Log(651, "BurstName returning");
#endif /* Logs */
	*NSPtr = This;
	return wperr_NoError;
}

static wp_ErrorCode NickSetup(cd, Probe, StrLoc, MapLoc)
struct wp_CD *cd; char *Probe; char **StrLoc; struct NamSet **MapLoc;
{/* Probe is the probe string; StrLoc points to where the CanonNick result is stored; MapLoc points to where the set of names is stored. */
	wp_ErrorCode RetVal;
	char *MatchRes;

	if (*StrLoc == NULL) {
		MatchRes = CanonNick(Probe);
		if (MatchRes == NULL) return wperr_OutOfMemory;
		*StrLoc = MatchRes;
	}
	if (*MapLoc == Undef_NamSet) {
		RetVal = KeyMatch(cd, BTIxNk, *StrLoc, &MatchRes);
		switch(RetVal) {
		case wperr_NoKeysFound:
			if (wp_Debugging) fprintf(stderr,
					"No special nickname maps for ``%s''/``%s''.\n",
					Probe, *StrLoc);
			*MapLoc = Empty_NamSet;
			return wperr_NoError;
		case wperr_NoError:
			if (wp_Debugging) fprintf(stderr,
					"Special nickname maps for ``%s''/``%s'': ``%s''.\n",
					Probe, *StrLoc, MatchRes);
			RetVal = BurstName((unsigned char *)MatchRes, MapLoc);
			free(MatchRes);
			return RetVal;
		default:
			return RetVal;
		}
	}
	return wperr_NoError;
}

/* Define component matches: MEx exact, MAb abbreviation, MOv override nickname, MPh phonetic, MPA phonetic abbreviation. */
#define MEx 0
#define MAb 1
#define MOv 2
#define MPh 3
#define MPA 4

static struct NamSet *NP = Undef_NamSet;

static wp_ErrorCode MatchName(cd, NPtr, SS, AllMatch, SurM, OthM, LNLP, RsltP)
struct wp_CD *cd; char *NPtr; struct TokSet *SS; int AllMatch, SurM, OthM, LNLP, *RsltP;
{	/* Match the probes in TokSet SS against the name from database in NPtr. */
	/* LNLP is TRUE iff the last probe needs to match the last key. */
	/* AllMatch is TRUE iff we need an exact match, not just a component match. */
	/* Set *RsltP to TRUE if we matched or FALSE if we didn't, and return wperr_NoError. */
	int Prb, Dict, Nick;
	char *PhonVal;
	wp_ErrorCode RetVal;
	int ThisIsSurn, ThisMatch;
	struct NamSet *Nicks;

#if Logs
	Log(500, "MatchName called");
#endif /* Logs */
	if (wp_Debugging) {
		fprintf(stderr, "MatchName(``%s'', {", NPtr);
		for (Prb = 0; Prb < SS->TokCount; ++Prb)
			fprintf(stderr, "%s%s", SS->Strs[Prb],
				(SS->Surname == Prb ? "(S) " : " "));
		fprintf(stderr, "}, all=%d, sur=%d, oth=%d, lnlp=%d) called.\n",
			AllMatch, SurM, OthM, LNLP);
	}
	RetVal = BurstName((unsigned char *)NPtr, &NP);	/* Burst out this current name */
	if (RetVal != wperr_NoError) return RetVal;
	Dict = -1;	/* will advance through NP */
	for (Prb = 0; Prb < SS->TokCount; ++Prb) {
		ThisIsSurn = (Prb == SS->Surname && LNLP);
		ThisMatch = (ThisIsSurn ? SurM : OthM);
/* Policy: Names with digits don't do phonetics.  Must be a real abbreviation. */
		if (ThisMatch > MAb) if (AnyDigits(SS->Strs[Prb])) {
		    if (wp_Debugging) fprintf(stderr, "MatchName returning FALSE for %d-match of some-digits ``%s''.\n", ThisMatch, SS->Strs[Prb]);
		    *RsltP = FALSE;
		    return wperr_NoError;
		}
/* Policy: Real names are more than one letter long, so any single-letter probes must be initials.  Even if there are explicit initials in names in the database, count them as abbreviations, not exact matches. */
		if (SS->Strs[Prb][1] == '\0' && isalpha(SS->Strs[Prb][0])) {	/* if one letter long */
		    if (ThisMatch == MEx || ThisMatch == MPh) {
			if (wp_Debugging) fprintf(stderr, "MatchName returning FALSE for %d-match of initial ``%s''.\n", ThisMatch, SS->Strs[Prb]);
			*RsltP = FALSE;
			return wperr_NoError;
		    }
		}
		while (++Dict < NP->NamCount) {	/* See if SS[Prb] matches NP[Dict] */
		  if (wp_Debugging) fprintf(stderr, "Matching(%d,%d) probe ``%s'' against dict ``%s''.\n",
		  		ThisMatch, ThisIsSurn, SS->Strs[Prb], NP->Nams[Dict]);
		  if (LNLP) {
		  	if (Prb == SS->Surname) if (Dict != NP->SurName) continue;
		  	if (Prb != SS->Surname) if (Dict == NP->SurName) continue;
		  }
		  switch (ThisMatch) {
		    case MEx:
			if (ULstrcmp(SS->Strs[Prb], NP->Nams[Dict]) == 0) goto FoundProbe;
		    	break;
		    case MAb:
			if (ULstlmatch(NP->Nams[Dict], SS->Strs[Prb]) == 1) goto FoundProbe;
		    	break;
		    case MOv:
			if (SS->OvMaps[Prb] == Undef_Str) {
				RetVal = KeyMatch(cd, BTIxOv, SS->Strs[Prb],
						&SS->OvMaps[Prb]);
				if (RetVal == wperr_NoKeysFound) {
					SS->OvMaps[Prb] = Empty_Str;
					break;
				}
				if (RetVal != wperr_NoError) return RetVal;
			} else if (SS->OvMaps[Prb] == Empty_Str) break;
			if (ULstrcmp(SS->OvMaps[Prb], NP->Nams[Dict]) == 0) goto FoundProbe;
		    	break;
		    case MPh:
			if (ThisIsSurn) {
				PhonVal = CanonSurn(NP->Nams[Dict]);
				if (PhonVal == NULL) return wperr_OutOfMemory;
			        if (ULstrcmp(SS->PhonSurnStr, PhonVal) == 0) {
					free(PhonVal); goto FoundProbe;
				}
				free(PhonVal);
			} else {
				PhonVal = CanonGiven(NP->Nams[Dict]);
				if (PhonVal == NULL) return wperr_OutOfMemory;
			        if (ULstrcmp(SS->PhonGivenStrs[Prb], PhonVal) == 0) {
					free(PhonVal); goto FoundProbe;
				}
				free(PhonVal);
			}
		    	break;
		    case MPA:
			if (ThisIsSurn) {
				if (SS->PhonSurnStr == NULL) {
					SS->PhonSurnStr = CanonSurn(SS->Strs[Prb]);
					if (SS->PhonSurnStr == NULL) return wperr_OutOfMemory;
				}
				PhonVal = CanonSurn(NP->Nams[Dict]);
				if (PhonVal == NULL) return wperr_OutOfMemory;
			        if (ULstlmatch(PhonVal, SS->PhonSurnStr) == 1) {
					free(PhonVal); goto FoundProbe;
				}
				free(PhonVal);
			} else {
				if (SS->PhonNickMaps[Prb] == Undef_NamSet) {
					RetVal = NickSetup(cd, SS->Strs[Prb],
							&SS->PhonNickStrs[Prb],
							&SS->PhonNickMaps[Prb]);
					if (RetVal != wperr_NoError) return RetVal;
				}
				PhonVal = CanonGiven(NP->Nams[Dict]);
				if (PhonVal == NULL) return wperr_OutOfMemory;
			        if (ULstlmatch(PhonVal, SS->PhonNickStrs[Prb]) == 1) {
					free(PhonVal); goto FoundProbe;
				}
				Nicks = SS->PhonNickMaps[Prb];
				if (Nicks != Empty_NamSet) {
					for (Nick = 0; Nick < Nicks->NamCount; ++Nick) {
						if (ULstrcmp(Nicks->Nams[Nick], PhonVal) == 0) {
							free(PhonVal); goto FoundProbe;	/* matched look-aside */
						}
					}
				}
				free(PhonVal);
			}
		    	break;
		  }
		}
	FailMatch:		/* ran out of dictionary without matching probe */
		if (wp_Debugging) fprintf(stderr, "MatchName returning FALSE.\n");
#if Logs
		Log(501, "MatchName returning FALSE");
#endif /* Logs */
		*RsltP = FALSE;
		return wperr_NoError;

    FoundProbe:	/* end of for loop--getting next token to match */
	    if (wp_Debugging) fprintf(stderr, "MatchName matched token ``%s''.\n", SS->Strs[Prb]);
	}
	if (wp_Debugging) fprintf(stderr, "MatchName returning TRUE.\n");
#if Logs
	Log(502, "MatchName returning TRUE");
#endif /* Logs */
	*RsltP = TRUE;		/* Successfully matched all probe pieces in order. */
	return wperr_NoError;
}

static wp_ErrorCode MatchSequence(cd, ISPtr, SS, CList, AllMatch, SurM, OthM, LNLP, ISCPtr)
struct wp_CD *cd; struct IdSet **ISPtr; struct TokSet *SS;
struct wp_Constraint *CList; int AllMatch, SurM, OthM, LNLP; int *ISCPtr;
{ /* Find and return the identifers of entries, from set IS, in which at least one of the names has components that match the sequence of strings in SS.  SurM is MEx iff the surname must match exactly; OthM is MEx iff non-surnames must match exactly.  LNLP is true iff the surname in the probe needs to match the surname in the name string.  AllMatch is true iff we need a complete match for a name, not just a component match.
*/
	struct IdSet *IS, *NewIS;
	btwp_identifier *PSrc, *PDst;
	int	DstCount, Ix, Passed;
	char *NPtr, *NStart;
	wp_ErrorCode Ret;

#if LogsYes
	Log(510, "MatchSequence called (%d, %d, %d, %d)",
				AllMatch, SurM, OthM, LNLP);
#endif /* LogsYes */
	IS = *ISPtr;
	if (wp_Debugging)
		fprintf(stderr,
		    "MatchSequence(%#x/%d ids, %#x/%d strings, all=%d, sur=%d, oth=%d, lnlp=%d) called.\n",
		    IS, IS->IdCount, SS, SS->TokCount, AllMatch, SurM, OthM, LNLP);

	/* For each ID in the set, apply the constraints to that ID. */
	if (SS->Tag != TokSetTag) {
		fprintf(stderr, "Type check failure in MatchSequence: %d.\n",SS->Tag);
		return wperr_InternalError;
	}
	if (*ISCPtr == 0) {	/* insist on making a copy (sigh) */
		Ret = CopyIdSet(IS, &NewIS);
		if (Ret != wperr_NoError) return Ret;
		*ISPtr = NewIS;
		IS = NewIS;
		*ISCPtr = 1;
	}
	DstCount = 0;
	PDst = IS->Ids;
	PSrc = PDst;
	for (Ix = IS->IdCount; Ix > 0; --Ix) {
		if (strncmp(cd->LastRetrieved.RecID, PSrc->RecID, PKLEN) != 0) {
			if (wp_Debugging) fprintf(stderr,
				"Loading entry with identifier %.*s.\n", PKLEN, PSrc->RecID);
			Ret = w_LoadEntry(cd, PSrc->RecID);
			if (Ret != wperr_NoError) return Ret;
		}
		NPtr = cd->Entries[FldN];
		if (NPtr != NULL) {
		    Ret = MatchName(cd, cd->Entries[FldN], SS, AllMatch, SurM, OthM, LNLP, &Passed);
		    if (Ret != wperr_NoError) return Ret;
		    if (Passed) goto PassConstraint;
		}
		NPtr = cd->Entries[FldWN];
		if (NPtr != NULL) {
		    while (*NPtr != '\0') {	/* break into pieces between semicolons */
			NStart = NPtr;
			while (*NPtr != '\0' && *NPtr != ';') NPtr++;
			if (*NPtr == ';') NPtr++;	/* advance past for next time */
			Ret = MatchName(cd, NStart, SS, AllMatch, SurM, OthM, LNLP, &Passed);
			if (Ret != wperr_NoError) return Ret;
			if (Passed) goto PassConstraint;
		    }
		}
		goto FailConstraint;	/* nothing more to match */
	PassConstraint:		/* come here when this ID is acceptable */
		Ret = TestConstraints(cd, *PSrc, CList, &Passed);
		if (Ret != wperr_NoError) return Ret;
		if (Passed) {
			*PDst++ = *PSrc;
			DstCount++;
		}
	FailConstraint:		/* come here when this ID not to be kept */
		PSrc++;
	}

#if LogsYes
	Log(520, "MatchSequence: old count %d, new count %d", IS->IdCount, DstCount);
#endif /* LogsYes */
	if (DstCount == IS->IdCount) return wperr_NoError;	/* no restriction */
	if (DstCount == 0) {
		if (*ISCPtr != 0) w_ZapIdSet(IS);
		*ISPtr = NULL;
		return wperr_NoKeysFound;
	}
/* Don't allocate a shrunken set--just leave the extra room in there to flop around */
	if (wp_Debugging) fprintf(stderr,
				"MatchSequence reduced ID list from %d to %d.\n",
				IS->IdCount, DstCount);
	IS->IdCount = DstCount;
#if LogsYes
	Log(522, "MatchSequence returns newly-built IdSet");
#endif /* LogsYes */
	return wperr_NoError;
}

static wp_ErrorCode IntersectAndAssignIdSet(OutISPtr, OutCopied, OtherIS)
struct IdSet **OutISPtr, *OtherIS;
int *OutCopied;
{
/* Set *OutISPtr to be the intersection of its old contents and the set OtherIS.  If *OutISPtr is currently null, just assign OtherIS to it.  If it isn't null, obey the OutCopied flag to tell if it should be copied before modifying it.
*/
	wp_ErrorCode Ret;

	if (*OutISPtr == Undef_IdSet) {
		if (wp_Debugging) fprintf(stderr, "IAAIS just assigning\n");
		*OutISPtr = OtherIS;
		*OutCopied = 0;
	} else if (*OutISPtr == Empty_IdSet) {
		if (wp_Debugging) fprintf(stderr, "IAAIS just assigning\n");
		*OutCopied = 0;
	} else {
		if (*OutCopied == 0) {
			if (wp_Debugging) fprintf(stderr, "IAAIS copying\n");
			Ret = CopyIdSet(*OutISPtr, OutISPtr);
			if (Ret != wperr_NoError) return Ret;
			*OutCopied = 1;
		}
		if (wp_Debugging) fprintf(stderr, "IAAIS intersecting\n");
		*OutISPtr = IntersectIDs(*OutISPtr, OtherIS);
		if ((*OutISPtr)->IdCount == 0) return wperr_NoKeysFound;
	}
	return wperr_NoError;
}

static wp_ErrorCode UnionIDs(pS1, pS1Copied, pS2, pS2Copied)
struct IdSet **pS1, **pS2; int *pS1Copied, *pS2Copied;
{ /* Assume that the IdSets S1 and S2 are sorted.
	Perform the operation ``S1 := S1 union S2'', overwriting *pS1.
*/
	btwp_identifier *PI1, *PI2;
	int	DstCount, NewSize, IdIx;
	struct IdSet *S1, *S2, *NewS;
	wp_ErrorCode retVal;

	S1 = *pS1;
	S2 = *pS2;
	if (S1 == Undef_IdSet || S2 == Undef_IdSet) return wperr_InternalError;
	if (S2 == Empty_IdSet) return wperr_NoError;
	if (S1 == Empty_IdSet) {
		retVal = CopyIdSet(S2, &S1);
		if (retVal != wperr_NoError) return retVal;
		*pS1 = S1; *pS1Copied = TRUE;
		return wperr_NoError;
	}
	if (wp_Debugging)
		fprintf(stderr, "UnionIDs(%#x (%d)/%d, %#x (%d)/%d): ",
				S1, S1->IdCount, *pS1Copied, S2, S2->IdCount, *pS2Copied);
	DstCount = S1->IdCount + S2->IdCount;
	NewS = S1;
	if (*pS1Copied == 0 || DstCount > S1->IdMax) {
		NewSize = (3 * DstCount) / 2;	/* grow it */
		if (wp_Debugging) fprintf(stderr,
				"UnionIDs growing set from %d to %d (%d).\n",
				S1->IdMax, DstCount, NewSize);
		if (*pS1Copied == 0) {	/* Need to copy, not just grow it in place */
			NewS = (struct IdSet *) malloc(sizeof(struct IdSet));
			if (NewS == NULL) return wperr_OutOfMemory;
			NewS->Ids = (btwp_identifier *)
				malloc(NewSize * sizeof(btwp_identifier));
			if (NewS->Ids == NULL) {
				free(NewS);
				return wperr_OutOfMemory;
			}
			NewS->Tag = IdSetTag;
			NewS->IdMax = NewSize;
			PI1 = &NewS->Ids[0];
			PI2 = &S1->Ids[0];
			for (IdIx = S1->IdCount; IdIx > 0; --IdIx) {
				strncpy((char*)PI1, (char*)PI2, PKLEN);
				++PI1; ++PI2;
			}
			NewS->IdCount = S1->IdCount;
		} else {
			S1->Ids = (btwp_identifier *)
				realloc(S1->Ids, NewSize * sizeof(btwp_identifier));
			if (S1->Ids == NULL) return wperr_OutOfMemory;
			NewS = S1;
		}
		NewS->IdMax = NewSize;
/* now NewS points to the new record or to S1; NewS->IdCount is # ids there, NewS->IdMax is the room available. */
	}
	PI1 = &NewS->Ids[NewS->IdCount];
	PI2 = &S2->Ids[0];
	for (IdIx = S2->IdCount; IdIx > 0; --IdIx) {
		strncpy((char*)PI1, (char*)PI2, PKLEN);
		++PI1; ++PI2;
	}
	NewS->IdCount = DstCount;
	DstCount = wpSortIxValue(NewS->Ids, NewS->IdCount);
	if (wp_Debugging) fprintf(stderr, "UnionIDs: %d duplicates out of %d\n",
				(NewS->IdCount - DstCount), NewS->IdCount);
	NewS->IdCount = DstCount;
	*pS1 = NewS;
	*pS1Copied = TRUE;	/* either it was already copied or we just did it. */
	return wperr_NoError;
}

static wp_ErrorCode NickMatch(cd, Probe, StrLoc, MapLoc, ISPtr)
struct wp_CD *cd; char *Probe; char **StrLoc; struct NamSet **MapLoc; struct IdSet **ISPtr;
{/* Probe is the probe string; StrLoc points to where the CanonNick result is stored; MapLoc points to where the set of names is stored; and ISPtr is where the resultant set, if any, is to be stored. */
	wp_ErrorCode RetVal; int Ix, ISCopied, NewISCopied;
	struct IdSet *IS = Undef_IdSet, *NewIS = Undef_IdSet;

	if (wp_Debugging) fprintf(stderr, "Finding nicknames for ``%s''.\n", Probe);
	if (AnyDigits(Probe)) return wperr_NoKeysFound;
	RetVal = NickSetup(cd, Probe, StrLoc, MapLoc);
	if (RetVal != wperr_NoError) return RetVal;
	RetVal = w_ProbeBegins(cd, BTIxCG, *StrLoc, &IS);
	if (RetVal == wperr_NoKeysFound) IS = Empty_IdSet;
	if (RetVal != wperr_NoKeysFound && RetVal != wperr_NoError) return RetVal;
	if (wp_Debugging) fprintf(stderr, "Nickname abbr finds %d id(s)\n",
			(RetVal == wperr_NoError ? IS->IdCount : 0));
	ISCopied = NewISCopied = 1;
	if (*MapLoc != Undef_NamSet && *MapLoc != Empty_NamSet) {
		for (Ix = 0; Ix < (*MapLoc)->NamCount; ++Ix) {
			NewIS = Undef_IdSet;
			RetVal = w_ProbeMatches(cd, BTIxCG, (*MapLoc)->Nams[Ix],
					&NewIS);
			switch (RetVal) {
			case wperr_NoKeysFound:
				break;
			case wperr_NoError:
				RetVal = UnionIDs(&IS, &ISCopied, &NewIS, &NewISCopied);
				if (RetVal != wperr_NoError) return RetVal;
				break;
			default:
				w_ZapIdSet(IS); return RetVal;
			}
			if (wp_Debugging) fprintf(stderr, "Map[%d], ``%s'', adds %d id(s), totaling %d.\n",
					Ix, (*MapLoc)->Nams[Ix],
					(NewIS == Empty_IdSet || NewIS == Undef_IdSet ? 0 : NewIS->IdCount),
					(IS == Empty_IdSet || IS == Undef_IdSet ? 0 : IS->IdCount));
		}
	}
	w_ZapIdSet(NewIS);
	if (IS == Undef_IdSet || IS == Empty_IdSet) return wperr_NoKeysFound;
	*ISPtr = IS;
	return wperr_NoError;
}

static unsigned char *DoneP = NULL;
static int DonePLen = 0;

static wp_ErrorCode NameMatch(cd, Probe, NToks, Constraints,
			AllMatch, SurM, OthM, LNLP, ISPtr, ISCopied)
struct wp_CD *cd; char *Probe; struct TokSet *NToks; struct wp_Constraint *Constraints;
int AllMatch, SurM, OthM, LNLP;
struct IdSet **ISPtr;
int *ISCopied;
{	/* Do a pass of name-matching.  Result back in ISPtr. */
	wp_ErrorCode RetVal;
	struct IdSet *ResIS = Undef_IdSet, *NewIS;
	int NamePos, LongLen, LongPos, ThisLen, ThisMatch, ThisIsSurn;

	*ISCopied = 0;
	if (AllMatch) {
		RetVal = w_ProbeMatches(cd, BTIxNWN, Probe, &NewIS);
		if (RetVal != wperr_NoError) goto NoMatch;
		RetVal = IntersectAndAssignIdSet(&ResIS, ISCopied, NewIS);
		if (RetVal != wperr_NoError) goto NoMatch;
	} else {
	  if (NToks->TokCount == 0) return wperr_NoKeysFound;
	  if (DonePLen < NToks->TokCount) {
	  	RetVal = w_GrowString(&DoneP, &DonePLen, NToks->TokCount);
		if (RetVal != wperr_NoError) return RetVal;
	  }
	  for (NamePos = 0; NamePos < NToks->TokCount; NamePos++) DoneP[NamePos] = 0;

/* First try to establish an initial set quickly.  Do all the exact-match stuff, and use all the cached stuff from prior tries. */
	  for (NamePos = NToks->TokCount-1; NamePos >= 0; --NamePos) {
		NewIS = NULL;
		ThisIsSurn = (NToks->Surname == NamePos && LNLP);
		ThisMatch = (ThisIsSurn ? SurM : OthM);
/* Policy: Names with digits don't do phonetics. */
		if (ThisMatch > MAb) if (AnyDigits(NToks->Strs[NamePos])) {
		    if (wp_Debugging) fprintf(stderr, "Denying %d-match of some-digits ``%s''.\n", ThisMatch, NToks->Strs[NamePos]);
		    RetVal = wperr_NoKeysFound; goto NoMatch;
		}
/* Policy: Real names are more than one letter long, so any single-letter probes must be initials.  Even if there are explicit initials in names in the database, count them as abbreviations, not exact matches. */
		if (NToks->Strs[NamePos][1] == '\0' && isalpha(NToks->Strs[NamePos][0])) {	/* if one letter long */
		    if (ThisMatch == MEx || ThisMatch == MPh) {
			if (wp_Debugging) fprintf(stderr, "Denying %d-match of initial ``%s''.\n", ThisMatch, NToks->Strs[NamePos]);
			RetVal = wperr_NoKeysFound; goto NoMatch;
		    }
		}
		switch (ThisMatch) {

		  case MEx:	/* exact */
		    if (ThisIsSurn) {
			if (NToks->SurnMatch == Undef_IdSet) {
			    RetVal = w_ProbeMatches(cd, BTIxSk, NToks->Strs[NamePos], &NewIS);
			    if (RetVal == wperr_NoKeysFound) NToks->SurnMatch = Empty_IdSet;
			    if (RetVal != wperr_NoError) goto NoMatch;
			    NToks->SurnMatch = NewIS;
			    if (wp_Debugging) fprintf(stderr,
				"Saved surname IdSet, %d IDs, in pos %d, matching word ``%s''.\n",
				NewIS->IdCount, NamePos, NToks->Strs[NamePos]);
			} else if (NToks->SurnMatch == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->SurnMatch;
		    } else {
			if (NToks->Matches[NamePos] == Undef_IdSet) {
			    RetVal = w_ProbeMatches(cd, BTIxTk, NToks->Strs[NamePos], &NewIS);
			    if (RetVal == wperr_NoKeysFound) NToks->Matches[NamePos] = Empty_IdSet;
			    if (RetVal != wperr_NoError) goto NoMatch;
			    NToks->Matches[NamePos] = NewIS;
			    if (wp_Debugging) fprintf(stderr,
				"Saved IdSet, %d IDs, in pos %d, matching word ``%s''.\n",
				NewIS->IdCount, NamePos, NToks->Strs[NamePos]);
			} else if (NToks->Matches[NamePos] == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->Matches[NamePos];
		    }
		    break;

		  case MAb:	/* abbrev */
		    if (ThisIsSurn) {
			if (NToks->SurnBegin == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else if (NToks->SurnBegin != Undef_IdSet) { /* cached surn-begin */
			    NewIS = NToks->SurnBegin;
			} else NewIS = NULL;
		    } else {	/* not the surname */
			if (NToks->Begins[NamePos] == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else if (NToks->Begins[NamePos] == Undef_IdSet) {
			    NewIS = NULL;
			} else NewIS = NToks->Begins[NamePos];
		    }
		    break;

		  case MOv:	/* nickname override */
		    if (NToks->OvSets[NamePos] == Undef_IdSet) {
			if (NToks->OvMaps[NamePos] == Undef_Str) {
				RetVal = KeyMatch(cd, BTIxOv, NToks->Strs[NamePos],
							&NToks->OvMaps[NamePos]);
				if (RetVal == wperr_NoKeysFound) {
					NToks->OvMaps[NamePos] = Empty_Str;
					NToks->OvSets[NamePos] = Empty_IdSet;
				}
				if (RetVal != wperr_NoError) goto NoMatch;
			} else if (NToks->OvMaps[NamePos] == Empty_Str) {
				NToks->OvSets[NamePos] = Empty_IdSet;
				RetVal = wperr_NoKeysFound; goto NoMatch;
			}
			RetVal = w_ProbeMatches(cd, BTIxTk, NToks->OvMaps[NamePos], &NewIS);
			if (RetVal == wperr_NoKeysFound) NToks->OvSets[NamePos] = Empty_IdSet;
			if (RetVal != wperr_NoError) goto NoMatch;
			NToks->OvSets[NamePos] = NewIS;
			if (wp_Debugging) fprintf(stderr,
				"Saved IdSet, %d IDs, in pos %d, overriding nickname ``%s''.\n",
				NewIS->IdCount, NamePos, NToks->Strs[NamePos]);
		    } else if (NToks->OvSets[NamePos] == Empty_IdSet) {
			RetVal = wperr_NoKeysFound; goto NoMatch;
		    } else NewIS = NToks->OvSets[NamePos];
		    break;

		  case MPh:	/* phonetic */
		    if (ThisIsSurn) {
			if (NToks->PhonSurnSet == Undef_IdSet) {
			    if (NToks->PhonSurnStr == NULL) {
				NToks->PhonSurnStr = CanonSurn(NToks->Strs[NamePos]);
				if (NToks->PhonSurnStr == NULL) {
					RetVal = wperr_OutOfMemory; goto NoMatch;
				}
			    }
			    RetVal = w_ProbeMatches(cd, BTIxCS, NToks->PhonSurnStr, &NewIS);
			    if (RetVal == wperr_NoKeysFound) NToks->PhonSurnSet = Empty_IdSet;
			    if (RetVal != wperr_NoError) goto NoMatch;
			    NToks->PhonSurnSet = NewIS;
			    if (wp_Debugging) fprintf(stderr,
				"Saved surname IdSet, %d IDs, in pos %d, phon-matching word ``%s''.\n",
				NewIS->IdCount, NamePos, NToks->Strs[NamePos]);
			} else if (NToks->PhonSurnSet == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->PhonSurnSet;
		    } else {
			if (NToks->PhonGivenSets[NamePos] == Undef_IdSet) {
			    if (NToks->PhonGivenStrs[NamePos] == NULL) {
				NToks->PhonGivenStrs[NamePos] = CanonGiven(NToks->Strs[NamePos]);
				if (NToks->PhonGivenStrs[NamePos] == NULL) {
					RetVal = wperr_OutOfMemory; goto NoMatch;
				}
			    }
			    RetVal = w_ProbeMatches(cd, BTIxCG, NToks->PhonGivenStrs[NamePos], &NewIS);
			    if (RetVal == wperr_NoKeysFound)
					NToks->PhonGivenSets[NamePos] = Empty_IdSet;
			    if (RetVal != wperr_NoError) goto NoMatch;
			    NToks->PhonGivenSets[NamePos] = NewIS;
			    if (wp_Debugging) fprintf(stderr,
				"Saved IdSet, %d IDs, in pos %d, phon-matching word ``%s''.\n",
				NewIS->IdCount, NamePos, NToks->Strs[NamePos]);
			} else if (NToks->PhonGivenSets[NamePos] == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->PhonGivenSets[NamePos];
		    }
		    break;

		  case MPA:	/* phonetic abbrev */
		    if (ThisIsSurn) {
			if (NToks->PhonAbbrSurnSet == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else if (NToks->PhonAbbrSurnSet != Undef_IdSet) { /* cached surn-begin */
			    NewIS = NToks->PhonAbbrSurnSet;
			} else NewIS = NULL;
		    } else {	/* not the surname */
			if (NToks->PhonNickSets[NamePos] == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else if (NToks->PhonNickSets[NamePos] == Undef_IdSet) {
			    NewIS = NULL;
			} else NewIS = NToks->PhonNickSets[NamePos];
		    }
		    break;

		}

		if (NewIS != NULL) {	/* have something to try */
		    RetVal = IntersectAndAssignIdSet(&ResIS, ISCopied, NewIS);
		    if (RetVal != wperr_NoError) goto NoMatch;
		    DoneP[NamePos] = 1;
		}
	  }

/* OK, the easy/fast stuff is done.  Decide whether to evaluate further.  We need to if the above operations haven't found an initial set. */
/* The heuristics here may want to change if we ever get serious about making them work really well for nickname matches also.  Not soon; things will always work, even if not in the fastest way. */
	  for (;;) {
	    LongPos = LongLen = -1;	/* Find longest remaining string */
	    for (NamePos = 0; NamePos < NToks->TokCount; NamePos++)
		if (DoneP[NamePos] == 0) {
		    ThisLen = strlen(NToks->Strs[NamePos]);
		    if (ThisLen > LongLen) {LongLen = ThisLen; LongPos = NamePos;}
		}
	    if (LongPos < 0) break;	/* none left to do */
/* Should we evaluate this one?  If there's none so far, we have to.  Otherwise, the (Grits implementation) statistics are that it takes about 0.11 sec for each MatchSequence, and about 0.3+(0.24/LongLen^2) sec for a w_ProbeBegins.  But if we do a w_ProbeBegins, we'll save its result, and never re-do it. */
	    if (ResIS != Undef_IdSet && ResIS != Empty_IdSet) {
		if (wp_Debugging) fprintf(stderr,
			"Considering stopping early.  ResIS size %d, LongLen %d.\n",
			ResIS->IdCount, LongLen);
		if (ResIS->IdCount < (LongLen == 1 ? 4 : 2)) {
			if (wp_Debugging) fprintf(stderr, "Stopping early.\n");
			break;
		}
	    }
	    ThisIsSurn = (NToks->Surname == LongPos && LNLP);
	    ThisMatch = (ThisIsSurn ? SurM : OthM);
	    if (ThisMatch > MAb) if (AnyDigits(NToks->Strs[LongPos])) ThisMatch = MAb;
	    switch (ThisMatch) {
	      case MAb:	/* abbrev */
		if (ThisIsSurn) {
			if (NToks->SurnBegin == Undef_IdSet) {
				RetVal = w_ProbeBegins(cd, BTIxSk, NToks->Strs[LongPos], &NewIS);
				if (RetVal == wperr_NoKeysFound) NToks->SurnBegin = Empty_IdSet;
				if (RetVal != wperr_NoError) goto NoMatch;
				NToks->SurnBegin = NewIS;
				if (wp_Debugging) fprintf(stderr,
					"Saved surname IdSet, %d IDs, in pos %d, beginning word ``%s''.\n",
					NewIS->IdCount, LongPos, NToks->Strs[LongPos]);
			} else if (NToks->SurnBegin == Empty_IdSet) {
				RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->SurnBegin;
		} else {
			if (NToks->Begins[LongPos] == Undef_IdSet) {
			    RetVal = w_ProbeBegins(cd, BTIxTk, NToks->Strs[LongPos], &NewIS);
			    if (RetVal == wperr_NoKeysFound) NToks->Begins[LongPos] = Empty_IdSet;
			    if (RetVal != wperr_NoError) goto NoMatch;
			    NToks->Begins[LongPos] = NewIS;
			    if (wp_Debugging) fprintf(stderr,
				"Saved IdSet, %d IDs, in pos %d, beginning word ``%s''.\n",
				NewIS->IdCount, LongPos, NToks->Strs[LongPos]);
			} else if (NToks->Begins[LongPos] == Empty_IdSet) {
			    RetVal = wperr_NoKeysFound; goto NoMatch;
			} else NewIS = NToks->Begins[LongPos];
		}
		break;
	      case MPA:	/* phon abbrev */
		if (ThisIsSurn) {
		    if (NToks->PhonAbbrSurnSet == Undef_IdSet) {
			if (NToks->PhonSurnStr == NULL) {
			    NToks->PhonSurnStr = CanonSurn(NToks->Strs[LongPos]);
			    if (NToks->PhonSurnStr == NULL) {
				RetVal = wperr_OutOfMemory; goto NoMatch;
			    }
			}
			RetVal = w_ProbeBegins(cd, BTIxCS, NToks->PhonSurnStr, &NewIS);
			if (RetVal == wperr_NoKeysFound) NToks->PhonAbbrSurnSet = Empty_IdSet;
			if (RetVal != wperr_NoError) goto NoMatch;
			NToks->PhonAbbrSurnSet = NewIS;
			if (wp_Debugging) fprintf(stderr,
				"Saved surname IdSet, %d IDs, in pos %d, phon-beginning word ``%s''.\n",
				NewIS->IdCount, LongPos, NToks->Strs[LongPos]);
		    } else if (NToks->PhonAbbrSurnSet == Empty_IdSet) {
			RetVal = wperr_NoKeysFound; goto NoMatch;
		    } else NewIS = NToks->PhonAbbrSurnSet;
		} else {
		    if (NToks->PhonNickSets[LongPos] == Undef_IdSet) {
			RetVal = NickMatch(cd, NToks->Strs[LongPos],
					&NToks->PhonNickStrs[LongPos],
					&NToks->PhonNickMaps[LongPos], &NewIS);
			if (RetVal == wperr_NoKeysFound)
				NToks->PhonNickSets[LongPos] = Empty_IdSet;
			if (RetVal != wperr_NoError) goto NoMatch;
			NToks->PhonNickSets[LongPos] = NewIS;
			if (wp_Debugging) fprintf(stderr,
				"Saved IdSet, %d IDs, in pos %d, nicknames for word ``%s''.\n",
				NewIS->IdCount, LongPos, NToks->Strs[LongPos]);
		    } else if (NToks->PhonNickSets[LongPos] == Empty_IdSet) {
			RetVal = wperr_NoKeysFound; goto NoMatch;
		    } else NewIS = NToks->PhonNickSets[LongPos];
		}
		break;
	      default:
		NewIS = NULL;	/* should never happen; let's trap it. */
		break;
	    }
	    RetVal = IntersectAndAssignIdSet(&ResIS, ISCopied, NewIS);
	    if (RetVal != wperr_NoError) goto NoMatch;
	    DoneP[LongPos] = 1;
	  }
	}
	if (ResIS == Undef_IdSet || ResIS == Empty_IdSet) {
		RetVal = wperr_NoKeysFound; goto NoMatch;
	}
	RetVal = MatchSequence(cd, &ResIS, NToks, Constraints, AllMatch, SurM, OthM, LNLP, ISCopied);
	if (RetVal == wperr_NoError) {
		*ISPtr = ResIS;
		return wperr_NoError;
	}
    NoMatch:
	if (*ISCopied != 0) w_ZapIdSet(ResIS);
	return RetVal;
}

static wp_ErrorCode TryHeuristic(cd, WhichHeur, Probe, Constraints, ISPtr, ISCPtr)
struct wp_CD *cd; int WhichHeur; char *Probe; struct wp_Constraint *Constraints;
struct IdSet **ISPtr; int *ISCPtr;
{	/* Do the specified heuristic match on the name */
	static char HeurID[10];
	wp_ErrorCode RetVal;
	int I; char *Ptr;

	if (strlen(Probe) != 4) return wperr_NoKeysFound;
	strcpy(HeurID, Probe);
	for (I = 0, Ptr = HeurID; I < 4; I++, Ptr++)
		if (*Ptr <= 'Z' && *Ptr >= 'A') *Ptr += ('a' - 'A');
	if ('a' <= HeurID[1] && HeurID[1] <= 'z') {
		Ptr = &HeurID[2];
		if (*Ptr == 'o' || *Ptr == 'O') *Ptr = '0';
		else if (*Ptr == 'l' || *Ptr == 'i' || *Ptr == 'I') *Ptr = '1';
		else if (*Ptr == 'S' || *Ptr == 's') *Ptr = '5';
		else if (*Ptr == 'G' || *Ptr == 'g' || *Ptr == 'b') *Ptr = '6';
		else if (*Ptr == 'B') *Ptr = '8';
		else if (*Ptr == 'A') *Ptr = '4';
		if (wp_Debugging) fprintf(stderr, "Trying heuristic ID: %s\n", HeurID);
		RetVal = w_ProbeMatches(cd, BTIxID, HeurID, ISPtr);
		*ISCPtr = 1;
	} else RetVal = wperr_NoKeysFound;
	if (RetVal == wperr_NoError) RetVal = ApplyConstraints(cd, ISPtr, Constraints, ISCPtr);
	return RetVal;
}

/*
Carry out a search.  To look up a single entry, use wp_Lookup, described below.

The search specified in search token SrchToken is performed and the prime
 keys of the matching entries are returned via PKPtr.  The search will match as
 many entries as possible.  The parameter MatchQuality is used to store the
 nearness of the name match, as described in the definition of the Matchxxx
 constants.  The parameter MaxQuality can be specified by the client to
 indicate the most ambitious match to carry out.

So that resources are not wasted when searches match an unintentionally large
 number of entries, clients are asked to specify in MaxResults the maximum
 number of entries for which they would like the prime keys; a value of -1 for
 this parameter means to return keys for all matching entries.  If no entries are
 found, PKPtr is not written (storage is not allocated) and
 wperr_NoKeysFound is returned.  If the number of entries found is greater
 than MaxResults, a PrimeKeySet is constructed containing MaxResults prime
 keys, a pointer to it is stored into PKPtr, but wperr_TooManyKeysFound is
 nonetheless returned from the routine.

This routine malloc's storage to hold the prime keys.  It is the client program's
 responsibility to call wp_DeAllocate of that storage to deallocate it.  If
 storage allocation fails, this routine will have allocated nothing and will return
 code wperr_OutOfMemory.
*/
wp_ErrorCode cwp_Search(cd, SrchToken, MaxResults, MaxQuality, MatchQuality, PKPtr)
	struct wp_cd *cd;
	wp_SearchToken SrchToken; int MaxResults;
	int MaxQuality, *MatchQuality; wp_PrimeKeySetPtr *PKPtr;
{
	struct wp_CD *CD = (struct wp_CD *) cd;
	struct wp_SrchToken *ST = (struct wp_SrchToken *) SrchToken;
	wp_ErrorCode RetVal;
	int	ResultNumberTruncated, LoopNum;
	wp_PrimeKeySet	*PKs;
	struct IdSet		*IS, *NIS;
	struct NamSet	*Names;
	struct TokSet	*NToks;
	int ThisSearch, LastUIDSearch, Rule, ISCopied, NISCopied, HeurMch;
	enum RuleKind {ID, Name, Heur};
	static struct {
		enum RuleKind Kind; int Lookup, SurM, OthM, LNLP, AllMatch, HMch, MatchRes;
	} Rules[] = {
		{ID, LookupUIDFirst,		0, 0, 0, 0, MAb, MatchIDExact},
		{Name, LookupUIDWithFull,	MEx, MEx, 0, 1, MAb, MatchNameExact},
		{ID, LookupUIDOverPart,		0, 0, 0, 0, MAb, MatchIDNameExact},
		{Name, LookupUIDWithLastPart,	MEx, MEx, 1, 0, MAb, MatchNamePart},
		{ID, LookupUIDOverFirstAbbrev,	0, 0, 0, 0, MAb, MatchIDNamePart},
		{Name, LookupUIDWithFirstAbbrev,	MEx, MAb, 1, 0, MAb, MatchFirstNameAbbrev},
		{ID, LookupUIDOverAbbrev,	0, 0, 0, 0, MAb, MatchIDFirstNameAbbrev},
		{Name, LookupUIDWithLastNameAbbrev, MAb, MAb, 1, 0, MPh, MatchLastNameAbbrev},
		{Name, LookupUIDWithAnyNamePart, MEx, MEx, 0, 0, MPh, MatchAnyName},
		{Name, LookupUIDWithAnyNameAbbrev, MAb, MAb, 0, 0, MPh, MatchNameAbbrev},
		{ID, LookupUIDOverPhonetics,	0, 0, 0, 0, MPh, MatchIDNameAbbrev},
		{Name, 88,			MEx, MOv, 1, 0, MPh, MatchExOv},
		{Name, 91,			MAb, MOv, 1, 0, MPh, MatchAbOv},
		{Name, 94,			MEx, MPh, 1, 0, MPh, MatchExPh},
		{Name, 97,			MAb, MPh, 1, 0, MPh, MatchAbPh},
		{Name, 100,			MEx, MPA, 1, 0, MPh, MatchExPA},
		{Name, 103,			MAb, MPA, 1, 0, MPh, MatchAbPA},
		{Name, 106,			MPh, MEx, 1, 0, MPA, MatchPhEx},
		{Name, 109,			MPh, MAb, 1, 0, MPA, MatchPhAb},
		{Name, 112,			MPh, MOv, 1, 0, MPA, MatchPhOv},
		{Name, 115,			MPh, MPh, 1, 0, MPA, MatchPhPh},
		{Name, 118,			MPh, MPA, 1, 0, MPA, MatchPhPA},
		{Name, 121,			MOv, MOv, 0, 0, MPA, MatchAnyOv},
		{Name, 124,			MPh, MPh, 0, 0, MPA, MatchAnyPh},
		{Name, 127,			MPA, MEx, 1, 0, MPA+1, MatchPAEx},
		{Name, 130,			MPA, MAb, 1, 0, MPA+1, MatchPAAb},
		{Name, 133,			MPA, MOv, 1, 0, MPA+1, MatchPAOv},
		{Name, 136,			MPA, MPh, 1, 0, MPA+1, MatchPAPh},
		{Name, 139,			MPA, MPA, 1, 0, MPA+1, MatchPAPA},
		{Name, 142,			MPA, MPA, 0, 0, MPA+1, MatchAnyPA},
		{ID, LookupUIDLast,		0, 0, 0, 0, MPA+1, MatchIDPhonetics},
		{Heur, -1,			MPA+1, 0, 0, 1, MPA+1, MatchIDHeuristic}
	};
#define NumRules (sizeof(Rules) / sizeof(Rules[0]))

	if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
#if LogsYes
	Log(400, "wp_Search called on SearchKind %d", ST->SearchKind);
#endif /* LogsYes */
    LoopNum = 1;
    for (;;) {		/* try several times on temp failure */
	PKs = NULL;
	IS = NIS = Undef_IdSet;
	NToks = NULL;
	Names = Undef_NamSet;
	RetVal = wperr_OutOfMemory;
	ResultNumberTruncated = 0;
	ISCopied = NISCopied = 0;
	HeurMch = -1;
	if (ST->Tag != SrchTokenTag) return wperr_TokenMalformed;
	if (wp_Debugging) {fprintf(stderr, "wp_Search called on token "); PrintSrchToken(stderr, ST);}

	ThisSearch = ST->SearchKind;
	if (ThisSearch < LookupNIDOnly || ThisSearch > (10*LookupUIDLast))
		{RetVal = wperr_UnImplementedFunction; goto EndOfSwitch;}

	if (ThisSearch == LookupNIDOnly) {
	    RetVal = w_ProbeMatches(CD, BTIxNI, ST->Probe, &IS);
	    ISCopied = 1;
	    if (RetVal == wperr_NoError)
		RetVal = ApplyConstraints(CD, &IS, ST->Constraints, &ISCopied);
	    if (RetVal != wperr_NoError) goto EndOfSwitch;
	    *MatchQuality = MatchIDExact;
	    goto EndOfSwitch;
	} else if (ThisSearch == LookupUIDOnly) {
	    RetVal = w_ProbeMatches(CD, BTIxID, ST->Probe, &IS);
	    ISCopied = 1;
	    if (RetVal == wperr_NoError)
		RetVal = ApplyConstraints(CD, &IS, ST->Constraints, &ISCopied);
	    if (RetVal != wperr_NoError) goto EndOfSwitch;
	    *MatchQuality = MatchIDExact;
	    goto EndOfSwitch;
	}
/* Not a special-case search.  Do the usual search path stuff. */
#if Logs
	Log(410, "wp_Search checking for matches");
#endif /* Logs */
	RetVal = wperr_NoKeysFound;
	LastUIDSearch = LookupUIDFirst - 1000;
	for (Rule = 0; Rule < NumRules; ++Rule) {
#if LogsYes
	    Log(450, "wp_Search trying rule %d", Rule);
#endif /* LogsYes */
	    if (wp_Debugging) fprintf(stderr,
	        "Rule %d (%d): lookup %d, sur %d, oth %d, lnlp %d, allmatch %d, hmch %d, matchres %d\n",
		Rule, Rules[Rule].Kind, Rules[Rule].Lookup, Rules[Rule].SurM, Rules[Rule].OthM,
		Rules[Rule].LNLP, Rules[Rule].AllMatch, Rules[Rule].HMch, Rules[Rule].MatchRes);
	    switch (Rules[Rule].Kind) {
	case ID:
	case Name:
	    if (IS != Undef_IdSet && IS != Empty_IdSet) fprintf(stderr, "wp: gc idset: rule %d\n", Rule);
	    if (ThisSearch > LastUIDSearch && ThisSearch <= Rules[Rule].Lookup) {
		HeurMch = Rules[Rule].HMch;	/* this is when to look for heur match */
/* i.e., look for heur ID match with the name search for this match-style on surnames first shows */
		RetVal = w_ProbeMatches(CD, BTIxID, ST->Probe, &IS);
		ISCopied = 1;
		if (RetVal == wperr_NoError)
			RetVal = ApplyConstraints(CD, &IS, ST->Constraints, &ISCopied);
		if (RetVal == wperr_NoError && Rules[Rule].Kind == ID) {
			*MatchQuality = Rules[Rule].MatchRes;
			goto EndOfSwitch;
		}
		if (RetVal != wperr_NoKeysFound && RetVal != wperr_NoError) goto EndOfSwitch;
		if (RetVal == wperr_NoKeysFound) IS = Empty_IdSet;
	    }
	    LastUIDSearch = Rules[Rule].Lookup;
	    if (Rules[Rule].Kind == ID) break;	/* else if Name, fall through */

	    if (Rules[Rule].SurM == HeurMch) {
		HeurMch = -1;	/* reset this trigger */
		if (wp_Debugging) fprintf(stderr, "Triggering heuristic-ID search now.\n");
		RetVal = TryHeuristic(CD, Rules[Rule].AllMatch, ST->Probe, ST->Constraints,
				&NIS, &NISCopied);
		if (RetVal == wperr_NoError)
			RetVal = ApplyConstraints(CD, &NIS, ST->Constraints, &NISCopied);
		if (RetVal == wperr_NoKeysFound) NIS = Empty_IdSet;
		if (RetVal == wperr_NoError) {
		    if (IS != Undef_IdSet && IS != Empty_IdSet) {		/* we're merging two sets */
			RetVal = UnionIDs(&NIS, &NISCopied, &IS, &ISCopied);
			if (RetVal != wperr_NoError) goto EndOfSwitch;
		    }
		    if (ISCopied) w_ZapIdSet(IS);		/* deallocate; w_ZapIdSet is careful */
		    IS = NIS; NIS = Undef_IdSet;	/* move result to output area */
		    ISCopied = NISCopied; NISCopied = 0;
		}
		if (RetVal != wperr_NoKeysFound && RetVal != wperr_NoError) goto EndOfSwitch;
	    }
	    if (NToks == NULL) {	/* establish tokens */
		if (BurstName((unsigned char *)ST->Probe, &Names) != wperr_NoError)
			{RetVal = wperr_OutOfMemory; goto EndOfSwitch;}
		NToks = BuildTokSet(Names);
		if (NToks == NULL)
			{ZapNamSet(Names); RetVal = wperr_OutOfMemory; goto EndOfSwitch;}
	    }
	    RetVal = NameMatch(CD, ST->Probe, NToks, ST->Constraints,
			Rules[Rule].AllMatch, Rules[Rule].SurM,
			Rules[Rule].OthM, Rules[Rule].LNLP, &NIS, &NISCopied);
	    if (RetVal == wperr_NoError) {
		if (IS != Undef_IdSet && IS != Empty_IdSet) {		/* we're merging two sets */
			RetVal = UnionIDs(&NIS, &NISCopied, &IS, &ISCopied);
			if (RetVal != wperr_NoError) goto EndOfSwitch;
		}
		if (ISCopied) w_ZapIdSet(IS);		/* deallocate; w_ZapIdSet is careful */
		IS = NIS; NIS = Undef_IdSet;	/* move result to output area */
		ISCopied = NISCopied; NISCopied = 0;
		*MatchQuality = Rules[Rule].MatchRes;
		goto EndOfSwitch;
	    }
	    if (RetVal != wperr_NoKeysFound) goto EndOfSwitch;
	    if (IS != Undef_IdSet && IS != Empty_IdSet) {	/* no name match, but an ID match */
		*MatchQuality = Rules[Rule].MatchRes;
		RetVal = wperr_NoError;
		goto EndOfSwitch;
	    }
	    break;

	case Heur:
	    RetVal = TryHeuristic(CD, Rules[Rule].AllMatch, ST->Probe, ST->Constraints,
				&IS, &ISCopied);
	    if (RetVal == wperr_NoError) {
		*MatchQuality = Rules[Rule].MatchRes;
		goto EndOfSwitch;
	    }
	    if (RetVal != wperr_NoKeysFound) goto EndOfSwitch;
	    break;
	    }

/* end of switch cases; evaluate quality restrictions at end of each loop pass */
	    if (MaxQuality <= Rules[Rule].MatchRes) break;
	    if (RetVal != wperr_NoError && RetVal != wperr_NoKeysFound) break;
	}

/* Fell through the for loop without finding a match */
	RetVal = wperr_NoKeysFound;

EndOfSwitch: ;

#if Logs
	Log(490, "wp_Search will return code %d", RetVal);
#endif /* Logs */
	if (RetVal == wperr_NoError) if (IS->IdCount == 0) RetVal = wperr_NoKeysFound;
	if (RetVal == wperr_NoError) break;	/* out of retry loop */
	else {
		ZapTokSet(NToks); NToks = NULL;
		if (ISCopied) w_ZapIdSet(IS); IS = NULL;
		if (NISCopied) w_ZapIdSet(NIS); NIS = NULL;
		if (!wp_RetryThis(RetVal)) return RetVal;
		if (LoopNum >= 5) return (RetVal == wperr_BTreeTempFail ? wperr_IndexedRecordNotFound : RetVal);
		++LoopNum;
		if (wp_Debugging) fprintf(stderr, "wp_Search got temp fail (%d/%d); retrying...\n", RetVal, LoopNum);
		if (cwp_ReInitialize((struct wp_cd *) CD) != wperr_NoError) sleep(3);
	}
    }		/* end of LoopNum loop */

#if Logs
	Log(491, "wp_Search found %d matches", IS->IdCount);
#endif /* Logs */
	RetVal = w_IDtoPKSet(IS, &PKs, MaxResults, &ResultNumberTruncated);
	ZapTokSet(NToks); NToks = NULL;
	if (ISCopied) w_ZapIdSet(IS); IS = NULL;
	if (NISCopied) w_ZapIdSet(NIS); NIS = NULL;
	if (RetVal != wperr_NoError) return RetVal;
	*PKPtr = PKs;
#if LogsYes
	Log(492, "wp_Search returning");
#endif /* LogsYes */
	if (ResultNumberTruncated) return wperr_TooManyKeysFound;
	else return wperr_NoError;
}

/*
Carry out a lookup.  This is similar to a search, except an attempt is made to
 determine the single best match to the search criteria, and at most one key is
 returned.

The search specified in search token SrchToken is performed and the prime key
 of the matching entry is returned via PKPtr.  The parameter MatchQuality is
 used to store the nearness of the name match, as described in the definition of
 the Matchxxxx constants.  The parameter MaxQuality can be specified by the
 client to indicate the most ambitious match to carry out.

If no matching entries can be found, no storage is allocated, nothing is stored in
 PKPtr, the integer pointed to by MinMatchesFound is set to zero, and
 wperr_NoKeysFound is returned.  If the lookup cannot disambiguate between
 several possible matches, no storage is allocated, nothing is stored in PKPtr,
 the integer pointed to by MinMatchesFound is set to a lower bound for the
 number of matches found, and wperr_TooManyKeysFound is returned.

This routine malloc's storage to hold the prime key.  It is the client program's
 responsibility to call free (not wp_DeAllocate) of that storage to deallocate it.
  If storage allocation fails, this routine will have allocated nothing and will
 return code wperr_OutOfMemory.
*/
wp_ErrorCode cwp_Lookup(cd, SrchToken, MinMatchesFound, MaxQuality, MatchQuality, PKPtr)
	struct wp_cd *cd;
	wp_SearchToken SrchToken; int *MinMatchesFound;
	int MaxQuality, *MatchQuality; wp_PrimeKey *PKPtr;
{
	wp_ErrorCode RetVal;
	wp_PrimeKeySet *PKs;
	int QualityFound;
	struct wp_CD *CD = (struct wp_CD *) cd;

#define	MaxToAskFor 9

#if Logs
	Log(300, "cwp_Lookup called, calling cwp_Search");
#endif /* Logs */
	if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
	PKs = NULL;
	RetVal = cwp_Search(cd, SrchToken, MaxToAskFor, MaxQuality, &QualityFound, &PKs);
#if Logs
	Log(301, "cwp_Search returns to cwp_Lookup");
#endif /* Logs */
	switch (RetVal) {
	  case wperr_TooManyKeysFound:
		*MatchQuality = QualityFound;
		*MinMatchesFound = MaxToAskFor+1;
		RetVal = wp_DeAllocate((char **) PKs);
		if (RetVal != wperr_NoError) return RetVal;
		return wperr_TooManyKeysFound;
	  case wperr_NoKeysFound:
		*MinMatchesFound = 0;
		return wperr_NoKeysFound;
	  case wperr_NoError:
		*MatchQuality = QualityFound;
		if (PKs->KeyCount > 1) {
			*MinMatchesFound = PKs->KeyCount;
			RetVal = wp_DeAllocate((char **) PKs);
			if (RetVal != wperr_NoError) return RetVal;
#if Logs
			Log(330, "cwp_Lookup found too many keys");
#endif /* Logs */
			return wperr_TooManyKeysFound;
		}
		*PKPtr = PKs->Keys[0];
		PKs->Keys[0] = NULL;
		RetVal = wp_DeAllocate((char **) PKs);
		if (RetVal != wperr_NoError) return RetVal;
#if Logs
		Log(331, "cwp_Lookup returning a single key");
#endif /* Logs */
		return wperr_NoError;
	  default:
		if (PKs != NULL) (void) wp_DeAllocate((char **) PKs);
		return RetVal;
	}
}

/*
To find out all the fields that might exist in the database, use procedure
 wp_AllFields.  Fields are numbered beginning with zero.  This procedure
 overwrites FNamPtr with a pointer to a static string that is the name of the
 FieldIx'th field name.  To read all the fields of an entry given its prime key,
 use wp_AllFields to obtain the name of the field, then use wp_Read to find
 the contents of that field.  wp_AllFields will return
 wperr_FieldIndexOutOfBounds if the given index is not within bounds.

*/
wp_ErrorCode wp_AllFields(FieldIx, FNamPtr)
wp_FieldIndex FieldIx; char **FNamPtr;
{
    if (FieldIx < 0 || FieldIx > FldMAX) return wperr_FieldIndexOutOfBounds;
    *FNamPtr = wpFieldName[FieldIx];
    return wperr_NoError;
}
