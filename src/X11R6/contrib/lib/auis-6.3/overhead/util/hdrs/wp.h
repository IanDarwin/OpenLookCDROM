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


 

/* ************************************************************ *\
	wp.h
	Header file declaring the White Pages client intrface.
\* ************************************************************ */

/* Searching for entries matching a probe. */
typedef char *wp_PrimeKey;	/* would have preferred a hidden representation */
struct wp_PrimeKeySetStr { int KeyCount; wp_PrimeKey *Keys; };
	/* Keys can be treated like an array.  KeyCount counts its elements. */
typedef struct wp_PrimeKeySetStr wp_PrimeKeySet, *wp_PrimeKeySetPtr;

typedef char *wp_SearchToken;	/* pointer to PRIVATE */

/* Cell/directory token returned by wp_InitializeCell and wp_InitializeDir and passed to cwp_Search, cwp_Lookup, cwp_Read, and cwp_Generate. */
struct wp_cd {char **foo;};	/* pointer to PRIVATE */

/*
The white pages service provides two classes of routines: search routines for identifying records given some salient facts about those records, and read routines for extracting all attributes of those records.

The white pages service provides several different kinds of searches; the client chooses which one will be performed by selecting the LookupXXX parameter.  In general, when the search routines are successful, they indicate what kind of match succeeded by storing one of the MatchXXX values via a reference parameter.

Two searches, LookupNIDOnly and LookupUIDOnly, interpret the given probe only as a numeric Unix ID or a Unix login ID (a ``username'').  On a successful match, these searches will show match quality MatchIDExact.  LookupNIDOnly and LookupUIDOnly, without other constraints, may also be performed by the routines [c]wp_GetNIDOnly and [c]wp_GetUIDOnly.

All other searches perform full name lookup, attempting successively more
 permissive matches to the probe string in an attempt to find records with
 matching names.  The sequence of matches is not variable; the probe string is
 always matched as a full name (e.g. ``Craig Fulmer Everhart''), then as some
 complete part(s) of the name ("Fulmer" or "Craig Everhart"), and then as
 some abbreviation for one or more of the parts ("C Everh" or "C F Ev" or
 "everha").  Once matches at a given permissiveness level are found, the search
 routines stop searching further; all the entries returned as a match for a probe
 will have matched at the same quality/permissiveness level.

The name search is variable in that a match against the set of Unix login IDs is
 interpolated into the name match sequence at a place specified by the client.
  For instance, using the parameter LookupUIDFirst will first check if the
 probe matches a Unix login ID, and if that fails, the rest of the name search is
 carried out.  LookupUIDOverPart, as another example, causes the probe to be
 matched as a full name, then matched as a Unix login ID, then matched as one
 or more parts of a full name and so forth.  The login-ID match may be
 concurrent with a name match: LookupUIDWithLastPart will consider login IDs
 to be as good a match as surnames, so that a probe that matches the login ID
 of one entry and a surname of another will not match one in preference to
 another.

The name searches can return as their quality indication any of the values
 MatchNameExact, MatchNamePart, MatchFirstNameAbbrev,
 MatchLastNameAbbrev, MatchAnyName, MatchNameAbbrev,
 MatchNoHeuristics, or MatchIDHeuristic in addition to the quality code
 specific to the type of search.

The searches can be constrained so that they never attempt a match that would
 return a quality value numerically larger than a given value.

The searches work something like this:
	if (lookup == LookupUIDFirst) if (IDMatch) return MatchIDExact;
	if (probe matches full name or
		(lookup == LookupUIDWithFull & IDmatch)) return MatchNameExact;
	if (lookup == LookupUIDOverPart) if (IDMatch) return MatchIDNameExact;
	if (probe matches without abbreviations and surnames match or
		(lookup == LookupUIDWithLastPart & IDMatch)) return MatchNamePart;
	if (lookup == LookupUIDOverFirstAbbrev) if (IDMatch) return MatchIDNamePart;
	if (probe matches surnames without abbreviating them or
		(lookup == LookupUIDWIthFirstAbbrev & IDMatch)) return MatchFirstNameAbbrev;
	if (lookup == LookupUIDOverAbbrev) if (IDMatch) return MatchIDFirstNameAbbrev;
	if (probe matches surnames (even with abbreviations) or
		(lookup == LookupUIDWithLastNameAbbrev & IDMatch)) return MatchLastNameAbbrev;
	if (probe matches with no abbreviations or
		(lookup == LookupUIDWithAnyNamePart & IDMatch)) return MatchAnyName;
	if (probe matches or
		(lookup == LookupUIDWithAnyNameAbbrev & IDMatch)) return MatchNameAbbrev;
	if (lookup == LookupUIDLast) if (IDMatch) return MatchIDNameAbbrev;
	if (heuristic match(ID, set of IDs)) return MatchIDHeuristic;
	fail.
*/
#define LookupNIDOnly	0
#define LookupUIDOnly	1
#define LookupUIDFirst	10
#define LookupUIDWithFull	20
#define LookupUIDOverPart	30
#define LookupUIDWithLastPart	40
#define LookupUIDOverFirstAbbrev	50
#define LookupUIDWithFirstAbbrev	60
#define LookupUIDOverAbbrev	70
#define LookupUIDWithLastNameAbbrev	75
#define LookupUIDWithAnyNamePart	80
#define LookupUIDWithAnyNameAbbrev	85
#define LookupUIDOverPhonetics	86
#define LookupUIDLast		145

#define MatchIDExact		0
#define MatchNameExact	1
#define MatchIDNameExact	20
#define MatchNamePart		40
#define MatchIDNamePart	60
#define MatchFirstNameAbbrev	80
#define MatchIDFirstNameAbbrev	100
#define MatchLastNameAbbrev	106
#define MatchAnyName	114
#define MatchAnyEx	MatchAnyName
#define MatchNameAbbrev	120
#define MatchAnyAb	MatchNameAbbrev
#define MatchIDNameAbbrev	130
#define MatchNoHeuristics	MatchIDNameAbbrev
#define MatchExOv	133
#define MatchAbOv	136
#define MatchExPh	139
#define MatchAbPh	142
#define MatchExPA	145
#define MatchAbPA	148
#define MatchPhEx	151
#define MatchPhAb	154
#define MatchPhOv	157
#define MatchPhPh	160
#define MatchPhPA	163
#define MatchAnyOv	166
#define MatchAnyPh	169
#define MatchPAEx	172
#define MatchPAAb	175
#define MatchPAOv	178
#define MatchPAPh	181
#define MatchPAPA	184
#define MatchAnyPA	187
#define MatchIDPhonetics	200
#define MatchIDHeuristic	210
#define MatchAll		250


/* Index for the name of the field */
typedef int wp_FieldIndex;

/* Almost all procedures return an error code.  wp_NoError is generally the all-okay return.*/
typedef int wp_ErrorCode;
#define wperr_NoError 0
#define wperr_OutOfMemory 1
#define wperr_NoKeysFound 2
#define wperr_TooManyKeysFound 3
#define wperr_NoSuchKey 4
#define wperr_KeyError 5
#define wperr_NoSuchField 6
#define wperr_IndeterminateResult 7
#define wperr_FieldIndexOutOfBounds 8
#define wperr_NoSuchTokenKind 9
#define wperr_TokenMalformed 10
#define wperr_UnImplementedFunction 11
#define wperr_InternalError 12
#define wperr_UndifferentiatedFileSystemError 13
#define wperr_UndifferentiatedGritsError 14
#define wperr_UndifferentiatedBTreeError 15
#define wperr_BTreeTempFail 16
#define wperr_IndexedRecordNotFound 17
#define wperr_NotInited 18
#define wperr_NotACD 19
#define wperr_WPInWrongCell 20
#define wperr_NoPrimaryCell 21
#define wperr_MAX 21
#define wperr_FileSystemErrorBegin 400
#define wperr_FileSystemErrorEnd 999
#define wperr_BTreeBaseValue 2000
#define wperr_GritsBaseValue 3000
#define wperr_GritsTopValue 4999
/* to be followed by others... */

/* wp_ErrorString gives static English text interpreting an error code value.
Declaration:
	extern char *wp_ErrorString(codevalue);
	wp_ErrorCode codevalue;
*/
extern char *wp_ErrorString();


/* Initialize and terminate White Pages usage.
Declarations:
	extern wp_ErrorCode wp_Initialize();
	extern wp_ErrorCode wp_Terminate();

	extern wp_ErrorCode wp_InitializeCell(cellname, cdp);
		char *cellname; struct wp_cd **cdp;
	extern wp_ErrorCode wp_InitializeDir(dirname, cdp);
		char *dirname; struct wp_cd **cdp;
	extern wp_ErrorCode cwp_Terminate(cd);
		struct wp_cd *cd;

cwp_ReInitialize does a new open on the B-tree, discarding transient error state.
wp_RetryThis returns non-0 if the given error code corresponds to a condition that should be re-tried.

	extern wp_ErrorCode cwp_ReInitialize(cd);
		struct wp_cd *cd;
	extern int wp_RetryThis(cod);
		wp_ErrorCode cod;
*/
extern wp_ErrorCode wp_Initialize();
extern wp_ErrorCode wp_Terminate();
extern wp_ErrorCode wp_InitializeCell();
extern wp_ErrorCode wp_InitializeDir();
extern wp_ErrorCode cwp_Terminate();
extern wp_ErrorCode cwp_ReInitialize();
extern int wp_RetryThis();

/* Set the debugging and timing static ints (for maintainer use).  These both return the old values of the statics.  The debugging static stays at the specified value until changed; the timing static is turned off (set to 0) at a wp_Terminate.
Declarations:
	extern int wp_SetDebugging(val);
	int val;
	extern int wp_SetTiming(val);
	int val;
*/
extern int wp_SetDebugging();
extern int wp_SetTiming();

/*
Translate field names to field indices for use in the various routines.  -1 is returned if the specified field is not found at this point.
Declaration:
	extern wp_FieldIndex wp_FieldNameToIndex(FName);
	char *FName;
*/
extern wp_FieldIndex wp_FieldNameToIndex();

/*
Clean up after the client is done with malloc'ed storage.
Declaration:
	extern wp_ErrorCode wp_DeAllocate(StrPtr);
	union { wp_PrimeKeySet *PKSet; wp_SearchToken XX;} StrPtr;
*/
extern wp_ErrorCode wp_DeAllocate();

/*
Begin setting up for a new database search.  This initial search specifies that the NameProbe string is to be searched for according to the style given by the SearchKind parameter.  This search can therefore look, for example, at numeric IDs only, Unix IDs only, Unix IDs before names, Unix IDs before name abbreviations, and Unix IDs last of all.  The routine returns a pointer to an unspecified search token through the out parameter STPointer; this token can then be used for actual database searches.

This routine malloc's storage to hold the search token.  It is the client program's responsibility to call wp_DeAllocate of that storage to deallocate it.  If storage allocation fails, this routine will have allocated nothing and will return code wperr_OutOfMemory.

Declaration:
	extern wp_ErrorCode wp_SetUp(NameProbe, SearchKind, STPointer);
	char *NameProbe;  int SearchKind; wp_SearchToken *STPointer;

*/
extern wp_ErrorCode wp_SetUp();

/*
Constrain the search implied by the search token pointed to by STPointer so that the field FieldNum must begin with the string given in FieldContent.  ConstraintKind dictates how the given Content must relate to the contents of the field specified by FieldNum.  The STPointer pointer is modified to point to a new, also unspecified, piece of storage.

This routine malloc's storage to hold the search token.  It is the client program's responsibility to call wp_DeAllocate of that storage to deallocate it.  If storage allocation fails, this routine will have allocated nothing and will return code wperr_OutOfMemory.

Declaration:
	extern wp_ErrorCode wp_Constrain(STPointer, FieldNum, Content, ConstraintKind);
	wp_SearchToken *STPointer; wp_FieldIndex FieldNum;
	char *Content; enum wp_ConstraintKind ConstraintKind;
*/
enum wp_ConstraintKind {wpc_WholeKey, wpc_BeginKey, wpc_ContainKey};
extern wp_ErrorCode wp_Constrain();

/*
Carry out a search.  To look up a single entry, use wp_Lookup, described below.

The search specified in search token SToken is performed and the prime keys
 of the matching entries are returned via PKPtr.  The search will match as
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
 than MaxResults, a PrimeKeySet is constructed containing MaxResults+1
 prime keys, a pointer to it is stored into PKPtr, but
 wperr_TooManyKeysFound is nonetheless returned from the routine.

This routine malloc's storage to hold the prime keys.  It is the client program's
 responsibility to call wp_DeAllocate of that storage to deallocate it.  If
 storage allocation fails, this routine will have allocated nothing and will return
 code wperr_OutOfMemory.

Declaration:
    extern wp_ErrorCode wp_Search(SrchToken, MaxResults, MaxQuality, MatchQuality, PKPtr);
	wp_SearchToken SrchToken; int MaxResults;
	int MaxQuality, *MatchQuality; wp_PrimeKeySetPtr *PKPtr;

The cellular version is identical save for the additional extra parameter, which is expected to be the result of a prior call to wp_InitializeCell or wp_InitializeDir.
    extern wp_ErrorCode cwp_Search(cd, SrchToken, MaxResults, MaxQuality, MatchQuality, PKPtr);
	struct wp_cd *cd;
	wp_SearchToken SrchToken; int MaxResults;
	int MaxQuality, *MatchQuality; wp_PrimeKeySetPtr *PKPtr;

*/
extern wp_ErrorCode wp_Search();
extern wp_ErrorCode cwp_Search();

/*
Carry out a lookup.  This is similar to a search, except an attempt is made to determine the single best match to the search criteria, and at most one key is returned.

The search specified in search token SToken is performed and the prime key of the matching entry is returned via PKPtr.  The parameter MatchQuality is used to store the nearness of the name match, as described in the definition of the Matchxxxx constants.  The parameter MaxQuality can be specified by the client to indicate the most ambitious match to carry out.

If no matching entries can be found, no storage is allocated, nothing is stored in PKPtr, the integer pointed to by MinMatchesFound is set to zero, and wperr_NoKeysFound is returned.  If the lookup cannot disambiguate between several possible matches, no storage is allocated, nothing is stored in PKPtr, the integer pointed to by MinMatchesFound is set to a lower bound for the number of matches found, and wperr_TooManyKeysFound is returned.

This routine malloc's storage to hold the prime key.  It is the client program's responsibility to call free (not wp_DeAllocate) of that storage to deallocate it.  If storage allocation fails, this routine will have allocated nothing and will return code wperr_OutOfMemory.

Declaration:
    extern wp_ErrorCode wp_Lookup(SrchToken, MinMatchesFound, MaxQuality, MatchQuality, PKPtr);
	wp_SearchToken SrchToken; int *MinMatchesFound;
	int MaxQuality, *MatchQuality; wp_PrimeKey *PKPtr;

The cellular version is identical save for the additional extra parameter, which is expected to be the result of a prior call to wp_InitializeCell or wp_InitializeDir.
    extern wp_ErrorCode cwp_Lookup(cd, SrchToken, MinMatchesFound, MaxQuality, MatchQuality, PKPtr);
	struct wp_cd *cd;
	wp_SearchToken SrchToken; int *MinMatchesFound;
	int MaxQuality, *MatchQuality; wp_PrimeKey *PKPtr;

*/
extern wp_ErrorCode wp_Lookup();
extern wp_ErrorCode cwp_Lookup();

/* [c]wp_GetNIDOnly and [c]wp_GetUIDOnly do the equivalent of a wp_Lookup asking for LookupNIDOnly and LookupUIDOnly, with no constraints.  The cellular versions, as expected, take an initial parameter of type ``struct wp_cd'' to indicate the cell in which the calls should take place.
Declaration:
	wp_ErrorCode wp_GetNIDOnly(NID, PKPtr)
		int NID; wp_PrimeKey *PKPtr;
	wp_ErrorCode wp_GetUIDOnly(UID, PKPtr)
		char *UID; wp_PrimeKey *PKPtr;
	wp_ErrorCode cwp_GetNIDOnly(cd, NID, PKPtr)
		struct wp_cd *cd; int NID; wp_PrimeKey *PKPtr;
	wp_ErrorCode cwp_GetUIDOnly(cd, UID, PKPtr)
		struct wp_cd *cd; char *UID; wp_PrimeKey *PKPtr;
*/
extern wp_ErrorCode wp_GetNIDOnly();
extern wp_ErrorCode wp_GetUIDOnly();
extern wp_ErrorCode cwp_GetNIDOnly();
extern wp_ErrorCode cwp_GetUIDOnly();


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

Declaration:
	extern wp_ErrorCode wp_Read(PKey, FieldIx, FValPtr);
	wp_PrimeKey PKey; wp_FieldIndex FieldIx; char **FValPtr;

The cellular version is identical save for the additional extra parameter, which is expected to be the result of a prior call to wp_InitializeCell or wp_InitializeDir.
	extern wp_ErrorCode cwp_Read(cd, PKey, FieldIx, FValPtr);
	struct wp_cd *cd; wp_PrimeKey PKey; wp_FieldIndex FieldIx; char **FValPtr;

*/
extern wp_ErrorCode wp_Read();
extern wp_ErrorCode cwp_Read();

/*
To find out all the fields that might exist in the database, use procedure wp_AllFields.  Fields are numbered beginning with zero.  This procedure overwrites FNamPtr with a pointer to a static string that is the name of the FieldIx'th field name.  To read all the fields of an entry given its prime key, use wp_AllFields to obtain the name of the field, then use wp_Read to find the contents of that field.  wp_AllFields will return wperr_FieldIndexOutOfBounds if the given index is not within bounds.

Declaration:
	extern wp_ErrorCode wp_AllFields(FieldIx, FNamPtr)
	wp_FieldIndex FieldIx; char **FNamPtr;
*/
extern wp_ErrorCode wp_AllFields();

/* Generating all entries */
/*
To enumerate all the prime keys in the database, use wp_Generate(PKPtr).  Give this procedure a pointer to a null Prime Key and it will allocate the first PrimeKey and return it.  Give it back that PrimeKey and it will deallocate it, find the next one, and return the next one.  If there's no next one, it will return wperr_NoError and yet set your pointer to NULL.

Declaration:
	extern wp_ErrorCode wp_Generate(PKPtr)
	wp_PrimeKey *PKPtr;

The cellular version is identical save for the additional extra parameter, which is expected to be the result of a prior call to wp_InitializeCell or wp_InitializeDir.
	extern wp_ErrorCode cwp_Generate(cd, PKPtr)
	struct wp_cd *cd; wp_PrimeKey *PKPtr;
*/
extern wp_ErrorCode wp_Generate();
extern wp_ErrorCode cwp_Generate();
