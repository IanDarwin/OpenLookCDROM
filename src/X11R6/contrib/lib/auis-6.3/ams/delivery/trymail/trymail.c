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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/delivery/trymail/RCS/trymail.c,v 1.56 1993/08/30 19:12:56 rr2b Exp $";
#endif

/* ************************************************************ *\
	trymail.c
	This is the part of mail delivery service that is exported
	to client workstations for possible short-circuit evaluation.
\* ************************************************************ */

#include <truth.h>
#include <system.h>
#include <andrewos.h> /* sys/types.h strings.h sys/time.h sys/file.h */
#include <stdio.h>
#if !POSIX_ENV
/* These should be declared in stdio.h */
/*VARARGS1*/
extern int printf();
/*VARARGS2*/
extern int fprintf();
#endif
#include <sys/param.h>
#include <errno.h>
#include <ctype.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#include <sys/stat.h>
#include <signal.h>
#include <pwd.h>

#include <util.h>
#include <wp.h>
#include <qmail.h>
#ifdef AFS30_ENV
#ifdef CMUCS
#include <afs/pterror.h>
#define ERROR_TABLE_BASE_pr ERROR_TABLE_BASE_pt
#else
#include <afs/prerror.h>
#endif
#endif /* #ifdef AFS30_ENV */

#include <mailconf.h>
#include <mail.h>
#include <parseadd.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

#include <wkstr.h>
#include <trymail.h>

extern char *UnparseAddress();

int TMDebug, DoTiming;
int ReallyDeliver, IsExpired, IsExtraExpired, IsALittleExpired, IsSomeExpired, IsCrossCellExpired;
int MinimumFileSize, EnqueueDate, GlobalOptions;
char *GlobalForString, *GlobalDom;
static int ExitStatus = tmexit_OK;
static struct osi_Times TV;

int gotWPIndices;
wp_FieldIndex idxN, idxFwd, idxID, idxHD, idxEK, idxAf, idxDK, idxDP, idxNPA;

/* Globals containing the arguments */
char *AuthString, *InFileName, *TextRetPath;
FILE *InFile;
struct stat InFileStat;
int InFileErrno;
PARSED_ADDRESS *ParsedRetPath;
char *TextDestinations;
static int TextDestinationsLen;
PARSED_ADDRESS *ParsedDestinations;
int OrigDestinationCount;
static int AnyAuthentication = TRUE;
char *homeCell = NULL;

static PARSED_ADDRESS *LocalDelivered, *RemoteDelivered;
PARSED_ADDRESS *RootTemp;

#define InitialUnparseBuffSize	200
char *UnparseBuff;
static int UnparseBuffSize;

int ReturnTempFail;

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */
#ifndef LogsYes
#define LogsYes 0
#endif /* LogsYes */

#if LogsYes
/*VARARGS2*/
static void Log(num, fmt, p1, p2, p3, p4, p5, p6)
int num; char *fmt;
  { if (DoTiming) Logstat("trymail.c", num, fmt, p1, p2, p3, p4, p5, p6); }
#endif /* LogsYes */

void CheapPrint(f, Addr)
FILE *f; PARSED_ADDRESS *Addr;
{ /* Provide a cheap way to do unparsing for error recovery.  Write text to file f. */
	int HostCount, Ctr;

	fprintf(f, "<");
	HostCount = 0;
	FOR_ALL_REVERSE_HOSTS(Hst, Addr, {HostCount++;})
	switch (HostCount) {
    case 0:
	fprintf(f, "%s@%s", Addr->LocalPart, GlobalDom);
	break;
    case 1:
	fprintf(f, "%s", Addr->LocalPart);
	FOR_ALL_REVERSE_HOSTS(Hst, Addr, {fprintf(f, "@%s", Hst->Name);})
	break;
    default:
	Ctr = HostCount;
	FOR_ALL_REVERSE_HOSTS(Hst, Addr, {
		if (Ctr == 1) fprintf(f, ":%s@%s", Addr->LocalPart, Hst->Name);
		else {
			if (Ctr != HostCount) fprintf(f, ",");
			fprintf(f, "@%s", Hst->Name);
		}
		--Ctr;
	})
	break;
	}
	fprintf(f, ">");
}

char *AddrUnparse(Addr, ErrPtr)
PARSED_ADDRESS *Addr; long int *ErrPtr;
{ /* Unparse the address Addr and return it as a string, or NULL on any errors */
#define InitialUnPBuffSize	110
	char *UnPBuff;
	int UnPBuffSize, Code, NumUnparsed;
	PARSED_ADDRESS *HeadNext, *HeadPrev, *AddrNext, *AddrPrev;

#if Logs
	Log(180, "AddrUnparse called, ln %s", Addr->LocalPart);
#endif /* Logs */
	UnPBuff = malloc(InitialUnPBuffSize);
	if (UnPBuff == NULL) {*ErrPtr = PA_NO_MEM; return NULL;}
	UnPBuffSize = InitialUnPBuffSize;
/* Diddle the pointers so that UnparseAddressList gets a single-element list. */
	HeadNext = RootTemp->Next; HeadPrev = RootTemp->Prev;
	AddrNext = Addr->Next; AddrPrev = Addr->Prev;
	Addr->Next = Addr->Prev = RootTemp;
	RootTemp->Next = RootTemp->Prev = Addr;
	for (;;) {
#if Logs
		Log(181, "About to call UnparseAddressList");
#endif /* Logs */
		Code = UnparseAddressList(RootTemp,
				UP_SPACES_TO_DOTS | UP_NO_COMMENTS,
				UnPBuff, UnPBuffSize,
				"", "", 68, &NumUnparsed);
#if Logs
		Log(182, "UnparseAddressList returns to AddrUnparse");
#endif /* Logs */
		*ErrPtr = Code;
		if (Code == PA_OK) break;	/* leave UnPBuff alone */
		free(UnPBuff);
		UnPBuff = NULL;
		if (Code == PA_PARTIAL || Code == PA_NO_MEM) {	/* try to grow buffer */
			UnPBuffSize *= 2;	/* double the requested size */
			UnPBuff = malloc(UnPBuffSize);
			if (UnPBuff == NULL) {*ErrPtr = PA_NO_MEM; break;}
		} else break;
	}
/* Restore the pointers */
	RootTemp->Next = HeadNext; RootTemp->Prev = HeadPrev;
	Addr->Next = AddrNext; Addr->Prev = AddrPrev;
#if Logs
	Log(183, "AddrUnparse returning");
#endif /* Logs */
	return UnPBuff;	/* NULL on errors */
}

int DoUnparse(Addr, CodeLoc, Critical)
PARSED_ADDRESS *Addr; int *CodeLoc; int Critical;
{ /* Unparse the address Addr into UnparseBuff, growing that buffer if necessary. */
	int Code, NumUnparsed, RetVal, IsAtom;
	PARSED_ADDRESS *AddrNext = NULL, *AddrPrev = NULL;

	if (UnparseBuff == NULL || RootTemp == NULL) {	/* need initial buffer */
		UnparseBuff = malloc(InitialUnparseBuffSize);
		if (UnparseBuff == NULL || RootTemp == NULL) {
		    if (Critical) {
			fprintf(stdout, "%d%c", tmltf_MemoryExhausted, tmflag_End);
			CheapPrint(stdout, Addr);
			fprintf(stdout, " (Memory exhausted)\n");
			ExitStatus |= tmexit_SomeTempFail;
			return 0;
		    } else return 1;
		}
		UnparseBuffSize = InitialUnparseBuffSize;
	}
	IsAtom = (Addr->Kind != DUMMY_ADDRESS);
/* Diddle the pointers so that UnparseAddressList gets a single-element list. */
	if (IsAtom) {
		AddrNext = Addr->Next; AddrPrev = Addr->Prev;
		Addr->Next = Addr->Prev = RootTemp;
		RootTemp->Next = RootTemp->Prev = Addr;
	}
	for (;;) {
		Code = UnparseAddressList((IsAtom ? RootTemp : Addr),
				UP_SPACES_TO_DOTS | UP_NO_COMMENTS,
				UnparseBuff, UnparseBuffSize,
				"", "", 68, &NumUnparsed);
		if (Code == PA_PARTIAL || Code == PA_NO_MEM) {	/* try to grow buffer */
			UnparseBuffSize *= 2;	/* double the requested size */
			free(UnparseBuff);
			UnparseBuff = malloc(UnparseBuffSize);
			if (UnparseBuff == NULL) {
			    if (Critical) {
				fprintf(stdout, "%d%c", tmltf_MemoryExhausted, tmflag_End);
				CheapPrint(stdout, Addr);
				fprintf(stdout, " (Memory exhausted)\n");
				ExitStatus |= tmexit_SomeTempFail;
				RetVal = 0;
				break;
			    } else {*CodeLoc = PA_NO_MEM; RetVal = 1; break;}
			}
		} else {*CodeLoc = Code; RetVal = (Code == PA_OK ? 2 : 1); break;}
	}
/* Restore the pointers */
	if (IsAtom) {Addr->Next = AddrNext; Addr->Prev = AddrPrev;}
	return RetVal;
}

static void PrintAddr(f, CodeVal, MoreLines)
FILE *f; int CodeVal, MoreLines;
{/* Print the multi-line unparsed address, using code CodeVal for continuation lines.
Delete any trailing newline from the end of UnparseBuff.
*/
	char *BOL, *NextLine;

	NextLine = UnparseBuff;
	for (;;) {
		BOL = NextLine;
		NextLine = index(BOL, '\n');
		if (NextLine != NULL)
		    if (*++NextLine == '\0')
			{*--NextLine = '\0'; NextLine = NULL;}
		if (NextLine != NULL) {	/* now points past the newline */
			*--NextLine = '\0';
			fprintf(f, "%d%c%s\n", CodeVal, tmflag_More, BOL);
			*NextLine++ = '\n';
		} else {
			fprintf(f, "%d%c%s", CodeVal,
				(MoreLines ? tmflag_More : tmflag_End),
				BOL);
			break;
		}
	}
}

/*VARARGS2*/
static void GlobalRslt(CodeVal, Fmt, p1, p2, p3, p4, p5)
int CodeVal; char *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Issue a disposition message for the entire run, then exit. */
#if Logs
	Log(510, "GlobalRslt(%d...) called", CodeVal);
#endif /* Logs */
	fprintf(stdout, "%d%c", CodeVal, tmflag_End);
	fprintf(stdout, Fmt, p1, p2, p3, p4, p5);
	fprintf(stdout, "\n");
	fflush(stdout);
#if LogsYes
	Log(511, "GlobalRslt(%d...) exits.", CodeVal);
	if (DoTiming) {TermStats(); DoTiming = 0;}
#endif /* LogsYes */
#ifdef PLUMBFDLEAKS
	fdplumb_SpillGuts();
#endif /* PLUMBFDLEAKS */
	_exit(ExitStatus);
}

/*VARARGS2*/
static void GlobalTempFail(CodeVal, Fmt, p1, p2, p3, p4, p5)
int CodeVal; char *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Issue a message for a global temporary (transient) failure. */
	static char NewFmt[240];
	static char Subj[] = "1 error: Long-term undeliverable mail";
	int RetErrVal;

	ExitStatus |= tmexit_GlobalTempFail;
	if (IsExtraExpired) {
		RetErrVal = ReturnError(Subj, TRUE, 1, NULL, Fmt, p1, p2, p3, p4, p5);
		if (RetErrVal == 0)
			GlobalRslt(tm_EndOfInteraction, Fmt, p1, p2, p3, p4, p5);
		else {
			if (RetErrVal == 1 && ! ReturnTempFail)	/* we are trymail */
				strcpy(NewFmt, Fmt);
			else					/* we are switchmail */
				sprintf(NewFmt, "%s (Error %d issuing error)", Fmt,
					RetErrVal);
			GlobalRslt(tmgtf_ErrorReturningError, NewFmt, p1, p2, p3, p4, p5);
		}
	} else {
		GlobalRslt(CodeVal, Fmt, p1, p2, p3, p4, p5);
	}
}

/*VARARGS3*/
static void GlobalPermFail(CodeVal, Subj, Fmt, p1, p2, p3, p4, p5)
int CodeVal; char *Subj, *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Issue a message for a global permanent (persistent) failure. */
	static char NewSubj[240];
	int RetErrVal;

	ExitStatus |= tmexit_GlobalPermFail;
	RetErrVal = ReturnError(Subj, TRUE, 0, NULL, Fmt, p1, p2, p3, p4, p5);
	if (RetErrVal == 0)
		GlobalRslt(tm_EndOfInteraction, Fmt, p1, p2, p3, p4, p5);
	else {
		if (RetErrVal == 1 && ! ReturnTempFail)	/* we are trymail */
			strcpy(NewSubj, Subj);
		else					/* we are switchmail */
			sprintf(NewSubj, "%s (Error %d issuing error)", Subj, RetErrVal);
		GlobalRslt( (ReturnTempFail ? tmgtf_ErrorReturningError : CodeVal),
				"%s", NewSubj);
	}
}

/*VARARGS4*/
static int LocalRslt(CodeVal, Addr, Serious,
		Fmt, p1, p2, p3, p4, p5)
int CodeVal; PARSED_ADDRESS *Addr; int Serious;
char *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Issue a disposition message for one of the destinations.  Tie it off with a newline. */
	int Val, CVal;

#if Logs
	Log(500, "LocalRslt(%d...) called", CodeVal);
#endif /* Logs */
	Val = DoUnparse(Addr, &CVal, Serious);
	if (Val == 0) return tmltf_MemoryExhausted;	/* message already issued */
	else if (Val == 1) {		/* unparse error */
		fprintf(stdout, "%d%c", CodeVal, tmflag_End);
		CheapPrint(stdout, Addr);
		if (Fmt != NULL) {
			fprintf(stdout, " (Unparse error %d; ", CVal);
			fprintf(stdout, Fmt, p1, p2, p3, p4, p5);
			fprintf(stdout, ")");
		} else fprintf(stdout, " (Unparse error %d)", CVal);
	} else {
		PrintAddr(stdout, CodeVal, 0);
		if (Fmt != NULL) {
			fprintf(stdout, " (");
			fprintf(stdout, Fmt, p1, p2, p3, p4, p5);
			fprintf(stdout, ")");
		}
	}
	fprintf(stdout, "\n");
#if Logs
	Log(501, "LocalRslt returns.");
#endif /* Logs */
	return CodeVal;
}

/*VARARGS3*/
int AddrResult(CodeVal, Addr, Fmt, p1, p2, p3, p4, p5)
int CodeVal; PARSED_ADDRESS *Addr; char *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Issue a result message (successful, soft fail, or hard fail) for the address.
*/
	static char NewFmt[240];
	int RetErrVal;
	int IsSuccess = FALSE;
	static char Subj[] = "1 error: Long-term undeliverable mail";

	if (CodeVal >= tm_AddrDeliveredLowest && CodeVal <= tm_AddrDeliveredHighest)
		IsSuccess = TRUE;
	else if (CodeVal >= tm_AddrQueueLowest && CodeVal <= tm_AddrQueueHighest)
		ExitStatus |= tmexit_SomeTempFail;
	else if (CodeVal >= tm_AddrPermFailLowest && CodeVal <= tm_AddrPermFailHighest)
		ExitStatus |= tmexit_SomePermFail;
	if (IsSuccess || ! IsExtraExpired) {
		return LocalRslt(CodeVal, Addr, (IsSuccess ? 0 : 1), Fmt, p1, p2, p3, p4, p5);
	} else {
		RetErrVal = ReturnError(Subj, TRUE, 1, Addr, Fmt, p1, p2, p3, p4, p5);
		if (RetErrVal == 0)
			return LocalRslt(tmok_ErrorMsgReturned, Addr, 0, Fmt, p1, p2, p3, p4, p5);
		else {
			if (RetErrVal == 1 && ! ReturnTempFail)	/* we are trymail */
				strcpy(NewFmt, Fmt);
			else					/* we are switchmail */
				sprintf(NewFmt, "%s (Error %d issuing msg)", Fmt,
					RetErrVal);
			return LocalRslt( (ReturnTempFail ?
					tmltf_ErrorReturningMsg :
					tmlpf_ErrorReturningResp),
				Addr, 1, NewFmt, p1, p2, p3, p4, p5);
		}
	}
}

/*VARARGS4*/
int PermMessage(CodeVal, Addr, Subj, Fmt, p1, p2, p3, p4, p5)
int CodeVal; PARSED_ADDRESS *Addr; char *Subj, *Fmt;
char *p1, *p2, *p3, *p4, *p5;
{/* Return the permanent error condition in a piece of mail.  If this return attempt is successful, describe the condition with the special tmok_ErrorMsgReturned code; otherwise, issue a permanent-failure message to the caller.
*/
	static char NewSubj[240];
	int RetErrVal;

	RetErrVal = ReturnError(Subj, TRUE, 0, Addr,
				(Fmt == NULL ? Subj : Fmt), p1, p2, p3, p4, p5);
	if (RetErrVal == 0) {
		return AddrResult(tmok_ErrorMsgReturned, Addr, "%s; error message sent OK", Subj);
	} else {
		if (RetErrVal == 1 && ! ReturnTempFail)	/* we are trymail */
			strcpy(NewSubj, Subj);
		else					/* we are switchmail */
			sprintf(NewSubj, "%s (Error %d issuing msg)", Subj, RetErrVal);
		return AddrResult((ReturnTempFail ? tmltf_ErrorReturningMsg : CodeVal),
				Addr, NewSubj);
	}
}

int WantWPIndices()
{
#if Logs
	Log(224, "About to get WP indices");
#endif /* Logs */
	idxN =	wp_FieldNameToIndex("N");
	idxFwd =	wp_FieldNameToIndex("Fwd");
	idxID =	wp_FieldNameToIndex("ID");
	idxHD =	wp_FieldNameToIndex("HD");
	idxEK =	wp_FieldNameToIndex("EK");
	idxAf =	wp_FieldNameToIndex("Af");
	idxDK =	wp_FieldNameToIndex("DK");
	idxDP =	wp_FieldNameToIndex("DP");
	idxNPA = wp_FieldNameToIndex("NPA");
#if Logs
	Log(225, "The WP indices are ours.");
#endif /* Logs */
	if (idxN < 0 || idxFwd < 0 || idxID < 0 || idxHD < 0 || idxEK < 0 || idxAf < 0
	    || idxDK < 0 || idxDP < 0) {
		return FALSE;
	}
	gotWPIndices = TRUE;
	return TRUE;
}

int GetWPIndices(Addr)
PARSED_ADDRESS *Addr;
{
	if (! WantWPIndices()) {
		AddrResult(tmltf_CannotFindRequiredField, Addr,
				"Cannot find required field index");
		return FALSE;
	}
	return TRUE;
}

static int MemberP(Addr, AList)
PARSED_ADDRESS *Addr, *AList;
{
	FOR_ALL_ADDRESSES(Trial, AList, {
	    switch (Trial->Kind) {
	case SIMPLE_ADDRESS:
		if (Addr == Trial) return TRUE;
		break;
	case GROUP_ADDRESS:
		if (MemberP(Addr, Trial->Members)) return TRUE;
		break;
	default:
		break;
	    }
	})
	return FALSE;
}

static PARSED_ADDRESS *NewNullAddr()
{
	PARSED_ADDRESS *AL;

	AL = (PARSED_ADDRESS *) malloc(sizeof(PARSED_ADDRESS));
	if (AL != NULL) {
		AL->Kind = DUMMY_ADDRESS;
		AL->LocalPart = NULL;
		AL->Hosts = NULL;
		AL->Members = NULL;
		AL->RoutePhrase = NULL;
		AL->Comments = NULL;
		AL->Next = AL;
		AL->Prev = AL;
		AL->MD = NULL;
		AL->Extra = NULL;
		return AL;
	}
	return NULL;
}

static void AppendAddrTo(Addr, AddrList)
PARSED_ADDRESS *Addr, *AddrList;
{
	if (Addr->Next != NULL || Addr->Next != Addr) {
		/* unlink it from its old list first */
		Addr->Next->Prev = Addr->Prev;
		Addr->Prev->Next = Addr->Next;
	}
	Addr->Next = AddrList;
	Addr->Prev = AddrList->Prev;
	AddrList->Prev->Next = Addr;
	AddrList->Prev = Addr;
}

static void SafeAppendAddrTo(Addr, AddrList, Fwd)
PARSED_ADDRESS *Addr, *AddrList;
struct FwdLink *Fwd;
{
	struct FwdLink *FDum;

	for (FDum = Fwd; FDum != NULL; FDum = FDum->Parent) {
	    if (MemberP(Addr, FDum->RmtFwded)) return;
	}
	if (
	    ! MemberP(Addr, RemoteDelivered) &&
	    ! MemberP(Addr, LocalDelivered) &&
	    ! MemberP(Addr, AddrList)
	    ) AppendAddrTo(Addr, AddrList);
}

static int IsSameLocalPart(Addr, OtherLocal)
PARSED_ADDRESS *Addr; char *OtherLocal;
{/* Return TRUE iff the local-part of address Addr and the local-part OtherLocal, interpreted as local mailboxes in the same domain, map to the same delivery-system mailbox. */
	int Res;
	char *AddPrime, *AddSecond, *OthPrime, *OthSecond, *AddSave, *AddrDom;
	int AddType, OthType;

	AddrDom = Addr->Hosts->Prev->Name;
	if (AddrDom == NULL) return 0;
	Res = la_KindDomain(Addr, &AddType, &AddPrime, &AddSecond, AddrDom);
	if (Res != laerr_NoError) return 0;
	AddSave = Addr->LocalPart;
	Addr->LocalPart = OtherLocal;
	Res = la_KindDomain(Addr, &OthType, &OthPrime, &OthSecond, AddrDom);
	Addr->LocalPart = AddSave;
	if (Res != laerr_NoError) {if (AddPrime != NULL) free(AddPrime); return 0;}
	if (AddType != OthType) Res = 0;
	else {
		Res = 1;
		if (AddType == latype_LocalID) {
			if (strcmp(AddPrime, OthPrime) != 0) Res = 0;
/* Comment out the next line if delivery to foo+bar is to be suppressed when mail has already been delivered to foo+baz. */
			if (strcmp(AddSecond, OthSecond) != 0) Res = 0;
		} else if (AddType == latype_LocalName) {
			if (ULstrcmp(AddPrime, OthPrime) != 0) Res = 0;
		} else {
			if (AddPrime == NULL && OthPrime != NULL) Res = 0;
			else if (AddPrime != NULL && OthPrime == NULL) Res = 0;
			else if (AddPrime != NULL && OthPrime != NULL &&
				strcmp(AddPrime, OthPrime) != 0) Res = 0;
			if (AddSecond == NULL && OthSecond != NULL) Res = 0;
			else if (AddSecond != NULL && OthSecond == NULL) Res = 0;
			else if (AddSecond != NULL && OthSecond != NULL &&
				strcmp(AddSecond, OthSecond) != 0) Res = 0;
		}
	}
	if (AddPrime != NULL) free(AddPrime);
	if (OthPrime != NULL) free(OthPrime);
	return Res;
}

static int IsItOnList(Addr, AddrList, IsLocal)
PARSED_ADDRESS *Addr, *AddrList; int IsLocal;
{
	ADDRESS_HOST *GivenHost, *TrialHost;
	int SameAddr;

	FOR_ALL_ADDRESSES(Trial, AddrList, {
	    switch (Trial->Kind) {
	case SIMPLE_ADDRESS:
		if (IsLocal)
			SameAddr = IsSameLocalPart(Addr, Trial->LocalPart);
		else
			SameAddr = (strcmp(Addr->LocalPart, Trial->LocalPart) == 0);
		if (SameAddr) {
		    GivenHost = Addr->Hosts;
		    TrialHost = Trial->Hosts;
		    if (GivenHost == NULL && TrialHost == NULL) return TRUE;
		    if (GivenHost != NULL && TrialHost != NULL) {
			for (;;) {
			    GivenHost = GivenHost->Next;
			    TrialHost = TrialHost->Next;
			    if (GivenHost == Addr->Hosts && TrialHost == Trial->Hosts)
				return TRUE;	/* they both ended */
			    if (GivenHost == Addr->Hosts || TrialHost == Trial->Hosts)
				break;	/* one ended, the other didn't */
			    if (ULstrcmp(GivenHost->Name, TrialHost->Name) != 0)
				break;	/* different host names */
			}
		    }
		}
		break;
	case GROUP_ADDRESS:
		if (IsItOnList(Addr, Trial->Members, IsLocal)) return TRUE;
		break;
	default:
		break;
	    }
	})
	return FALSE;
}

static int ParseFwdString(Strg, OutAddr)
char *Strg; PARSED_ADDRESS **OutAddr;
{
	PARSED_ADDRESS *Addr;
	int PACode, Code2;
	char *NewN, *Ptr;

/* Flush newlines. */
	for (Ptr = Strg; *Ptr != '\0'; Ptr++) if (*Ptr == '\n' || *Ptr == '\r') *Ptr = ' ';

#if Logs
	Log(400, "ParseFwdString about to parse");
#endif /* Logs */
	PACode = ParseAddressList(Strg, &Addr);
#if Logs
	Log(401, "Parse result is %d", PACode);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "Parse of forwarding address ``%s'' returns %d.\n",
				Strg, PACode);
	if (PACode == PA_SYNTAX_ERROR && index(Strg, ',') == NULL) {
		/* Maybe addresses are separated by WSP rather than commas (groan) */
		NewN = NewString(Strg);
		if (NewN == NULL) return PA_NO_MEM;
		Ptr = NewN;
		while (isspace(*Ptr)) Ptr++;
		while (*Ptr != '\0') {
			while (*Ptr != '\0' && ! isspace(*Ptr)) Ptr++;
							/* replace delim */
			if (isspace(*Ptr)) *Ptr++ = ',';
			while (isspace(*Ptr)) Ptr++;
		}
#if Logs
		Log(402, "ParseFwdString trying again after adding commas");
#endif /* Logs */
		Code2 = ParseAddressList(NewN, &Addr);
#if Logs
		Log(403, "Second parse result is %d", Code2);
#endif /* Logs */
		if (TMDebug) fprintf(stderr, "Second-try parse of address ``%s'' returns %d.\n",
				NewN, Code2);
		free(NewN);
		if (Code2 == PA_OK) PACode = PA_OK;	/* it worked! */
	}
	if (PACode == PA_OK) *OutAddr = Addr;
	return PACode;
}

int WpOpen;
char WpOpenCell[150];
struct wp_cd *wpCD = NULL;
static wp_ErrorCode WpError;

wp_ErrorCode GetWPField(Key, FldIx, FResult)
wp_PrimeKey Key; wp_FieldIndex FldIx; char **FResult;
{
	wp_ErrorCode WpErr;
	char *NewPtr;

	WpErr = cwp_Read(wpCD, Key, FldIx, &NewPtr);
	if (WpErr == wperr_NoError) {
		*FResult = NewString(NewPtr);
		if (*FResult == NULL) return wperr_OutOfMemory;
		return wperr_NoError;
	}
	return WpErr;
}


int MakeFORString(Fwd, EndName, Dom, outFwd)
struct FwdLink *Fwd;
char *EndName, *Dom, **outFwd;
{ /* Make the FOR string to be used in local delivery.  EndName is the name for which any forwarding loop was discovered, and it's not on the Fwd chain.
*/
	char *FS; int FSLen;
	struct FwdLink *FDum;
	char **StrArr, **DomArr;
	int StrArrLen, Ix, FirstIx;

#if Logs
	Log(350, "MakeFORString called");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "Making FOR string:\n");
	StrArrLen = 0;
	if (GlobalForString != NULL) ++StrArrLen;
	for (FDum = Fwd; FDum != NULL; FDum = FDum->Parent)
	    if (FDum->Rewritten) ++StrArrLen;
	if (EndName != NULL) ++StrArrLen;
	if (StrArrLen == 0) {*outFwd = NULL; return 0;}	/* Nothing to record */

	StrArr = (char **) malloc(StrArrLen * sizeof(char *));
	if (StrArr == NULL) {*outFwd = NULL; errno = ENOMEM; return -1;}
	DomArr = (char **) malloc(StrArrLen * sizeof(char *));
	if (DomArr == NULL) {free(StrArr); *outFwd = NULL; errno = ENOMEM; return -1;}
	if (GlobalForString != NULL) {StrArr[0] = GlobalForString; DomArr[0] = GlobalDom;}
	Ix = StrArrLen - 1;
	if (EndName != NULL) {StrArr[Ix] = EndName; DomArr[Ix] = Dom; --Ix;}
	for (FDum = Fwd; FDum != NULL; FDum = FDum->Parent) {
	    if (FDum->Rewritten) {
		StrArr[Ix] = FDum->fAddr->LocalPart;
		DomArr[Ix] = FDum->fAddr->Hosts->Prev->Name;
		--Ix;
	    }
	}
	FirstIx = 0;
	if (StrArrLen > 1
		&& strcmp(StrArr[0], StrArr[1]) == 0
		&& strcmp(DomArr[0], DomArr[1]) == 0) FirstIx = 1;
	FSLen = 1;
	for (Ix = FirstIx; Ix < StrArrLen; ++Ix) {
		FSLen += strlen(StrArr[Ix]);
		FSLen += strlen(DomArr[Ix]);
	}
	FSLen += ((StrArrLen - 1) * 6);	/* Six characters for each extra name */
	FS = malloc(FSLen + 1);		/* leave room for the terminating NUL */
	if (FS == NULL) {
	    free(StrArr); free(DomArr);
	    *outFwd = NULL;
	    errno = ENOMEM; return -1;
	}
/* Treat the first one specially */
	strcpy(FS, StrArr[FirstIx]);
	if (index(StrArr[FirstIx], '@') == NULL) {strcat(FS, "@"); strcat(FS, DomArr[FirstIx]);}
	for (Ix = FirstIx+1; Ix < StrArrLen; ++Ix) {
		strcat(FS, " (->");	/* Four characters here */
		strcat(FS, StrArr[Ix]);
		if (ULstrcmp(DomArr[Ix-1], DomArr[Ix]) != 0) {
			strcat(FS, "@");
			strcat(FS, DomArr[Ix]);
		}
		strcat(FS, ")");	/* One character here */
	}
	free(StrArr); free(DomArr);
/* Now see to it that we're not adding lots of duplicate entries.  What we do is test whether the text we just added is identical to the trailing substring of the GlobalForString; if so, we truncate the result string so that it's the same null-terminated string as the GlobalForString. */
	if (GlobalForString != NULL) {
		Ix = strlen(GlobalForString);
		FSLen = strlen(FS) - Ix;	/* length of the added text */
		if (Ix > FSLen) {	/* GlobalForString is longer than what we just added */
			if (strncmp(&FS[Ix], &FS[Ix - FSLen], FSLen) == 0) {
				FS[Ix] = '\0';	/* truncate it right there: don't need the stuff we just added. */
			}
		}
	}
#if Logs
	Log(351, "MakeFORString returning.");
#endif /* Logs */
	*outFwd = FS;
	return 0;
}

static void CountAddresses(AddrList, pCount)
PARSED_ADDRESS *AddrList;
int *pCount;
{
/* Count the number of addressees are in the initial list */

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			(*pCount)++;
			break;
	case GROUP_ADDRESS:
			CountAddresses(ThisAddr->Members, pCount);
			break;
	default:
			break;
		}
	})
}

static void OpenWP(whatCell)
char *whatCell;
{/* Ensure an attempt to the white pages, leaving results in WpOpen and WpError */
	if (WpOpen != WpOpen_NotOpen && ULstrcmp(whatCell, WpOpenCell) != 0) {
		if (wpCD != NULL) cwp_Terminate(wpCD);
		wpCD = NULL;
		WpOpen = WpOpen_NotOpen;
		WpOpenCell[0] = '\0';
	}
	if (WpOpen == WpOpen_NotOpen) {
#if LogsYes
		if (DoTiming) wp_SetTiming(1);
		Log(205, "Calling wp_InitializeCell()");
#endif /* LogsYes */
		WpError = wp_InitializeCell(whatCell, &wpCD);
#if LogsYes
		Log(206, "wp_InitializeCell() returns %d", ErrCode);
#endif /* LogsYes */
		if (WpError == wperr_NoError) {
			WpOpen = WpOpen_Open;
			strcpy(WpOpenCell, whatCell);
		} else {
			WpOpen = WpOpen_CannotOpen;
			wpCD = NULL;
		}
	}
}

wp_ErrorCode IDtoName(ID, PStr, PAff, Dom)
int ID;
char **PStr, **PAff; char *Dom;
{/* Use white pages to put a pointer to a static string form of numeric uid ID via PStr.  Also, store a pointer to the given user's affiliation via PAff. */
	wp_ErrorCode Res;
	wp_PrimeKey KVal;
	char *NewPtr;

	if (TMDebug) fprintf(stderr, "IDtoName(%d) called.\n", ID);
	OpenWP(Dom);
	*PStr = NULL;
	*PAff = NULL;
	if (WpOpen != WpOpen_Open) return WpError;
	Res = cwp_GetNIDOnly(wpCD, ID, &KVal);
	if (Res != wperr_NoError) return Res;
	if (! gotWPIndices) {
		if (! WantWPIndices()) {free(KVal); return wperr_InternalError;}
	}
	Res = cwp_Read(wpCD, KVal, idxID, &NewPtr);
	if (Res != wperr_NoError) {free(KVal); return Res;}
	*PStr = NewPtr;
	Res = cwp_Read(wpCD, KVal, idxAf, &NewPtr);
	free(KVal);
	if (Res == wperr_NoError) *PAff = NewPtr;
	return wperr_NoError;
}

static wp_ErrorCode UserIDtoHD(UserID, HDStr, Dom)
char *UserID, *Dom;
char **HDStr;
{/* Use white pages to find the home directory for UserID and pass a pointer to it via HDStr. */
	wp_ErrorCode Res;
	wp_PrimeKey KVal;
	char *NewPtr;

	if (TMDebug) fprintf(stderr, "UserIDtoHD(%s) called.\n", UserID);
	OpenWP(Dom);
	if (WpOpen != WpOpen_Open) return WpError;
	Res = cwp_GetUIDOnly(wpCD, UserID, &KVal);
	if (Res != wperr_NoError) return Res;
	if (! gotWPIndices) {
		if (! WantWPIndices()) {free(KVal); return wperr_InternalError;}
	}
	Res = cwp_Read(wpCD, KVal, idxHD, &NewPtr);
	free(KVal);
	if (Res != wperr_NoError) return Res;
	*HDStr = NewPtr;
	return wperr_NoError;
}

int GetHeader(pHeader)
char **pHeader;
{	/* Read the header from the message into dynamic memory.  Return -2 if no memory, errno if errors encountered, 0 if all-OK.  If all-OK, pointer to header memory stored via pHeader. */
	char *Hdr, *HdrEnd;
	int N, HeaderLen;
	static int captured = FALSE;
#define InitialHeaderLen 508

	*pHeader = NULL;
	HeaderLen = MIN(InitialHeaderLen, InFileStat.st_size);
	for (;;) {		/* Get header into Header string */
#if Logs
		Log(201, "Looking for %d bytes of header", HeaderLen);
#endif /* Logs */
		Hdr = malloc(HeaderLen + 1);
		if (Hdr == NULL) return -2;
		rewind(InFile);
		errno = 0;
		N = fread(Hdr, sizeof(char), HeaderLen, InFile);
		if (N <= 0 || N > HeaderLen) {
			free(Hdr);
			Hdr = NULL;
			return (errno == 0 ? EIO : errno);
		}
		Hdr[N] = '\0';
		HdrEnd = ULsindex(Hdr, "\n\n");
		if (HdrEnd != NULL) {  /* save the double newline in Hdr */
			HdrEnd += 2;
			*HdrEnd = '\0';
			Hdr = realloc(Hdr, 	1 + HdrEnd - Hdr);
			break;	/* got the header */
		}
		if (HeaderLen >= InFileStat.st_size) break;	/* no body for message */
		free(Hdr);
		Hdr = NULL;
		HeaderLen = MIN(HeaderLen*2, InFileStat.st_size);
	}
#if Logs
	Log(210, "Got any message header: 0x%x", Hdr);
#endif /* Logs */
	*pHeader = Hdr;
	if (!captured) {
	    Capture(Hdr);
	    captured = TRUE;
	}
	return 0;
}

int MakeFile(Addr, Hdr, FileName, newITUVal)
PARSED_ADDRESS *Addr; char *Hdr, *FileName, *newITUVal;
{/* Make a copy of the message, with a Message-ID if necessary, in a temp file.  Return the name of the generated file in FileName, which should be a buffer of size MAXPATHLEN.  Return 0 for all-OK, or else an error code, having already generated an error message.  If newITUVal is non-NULL, we need to insist on an If-Type-Unsupported: header line with it as the value. */
	int TotalCharsRead, CharsRead, CharsWritten, errcopy;
	FILE *outF;
	auto char ChBuffer[1025];
	char *ituBegin, *ituEnd, *Dum;
	static char ITUname[] = "If-Type-Unsupported:";

	sprintf(FileName, "/usr/tmp/%s", ams_genid(1));
	errno = 0;
	outF = fopen(FileName, "w");
	if (outF == NULL) {
		if (Addr == NULL) return tmltf_MailTransputError;
		return AddrResult(tmltf_MailTransputError, Addr,
			"Can't create temp file %s: %s", FileName, UnixError(errno));
	}
	clearerr(InFile);
	rewind(InFile);
	if (newITUVal == NULL || BracketField(Hdr, ITUname, &Dum, &ituEnd, &ituBegin) == 0) {
		ituBegin = ituEnd = NULL;
	} else {
		++ituEnd;	/* point it at the character following the newline */
	}
	DefaultMsgID(Hdr, outF);	/* the major point of this exercise */
	TotalCharsRead = 0;
	errno = 0;
	CharsWritten = 0;
	if (newITUVal != NULL) {	/* prefix the whole header with the new value */
	    fputs(ITUname, outF); fputc(' ', outF); fputs(newITUVal, outF); fputc('\n', outF);
	    if (ituBegin != NULL) {	/* have to omit the old header line, now */
		if (ituBegin > Hdr) {	/* if old line not at beginning of the header */
			fwriteallchars(Hdr, ituBegin - Hdr, outF);
		}
		TotalCharsRead = (ituEnd - Hdr);
		fseek(InFile, TotalCharsRead, 0);	/* Skip up to the end of the old header */
	    }
	}
	for (;;) {
		CharsRead = fread(ChBuffer, sizeof(char), sizeof(ChBuffer)-1, InFile);
		CharsWritten = CharsRead;
		if (CharsRead == 0 && ( feof(InFile) || ferror(InFile) )) break;
		CharsWritten = fwriteallchars(ChBuffer, CharsRead, outF);
		if (CharsWritten != CharsRead) break;
		TotalCharsRead += CharsRead;
	}
	if (ferror(InFile) || CharsWritten != CharsRead) {
		errcopy = (errno == 0 ? EIO : errno);
		unlink(FileName); fclose(outF);
		if (Addr == NULL) return tmltf_MailTransputError;
		return AddrResult(tmltf_MailTransputError, Addr, "%s", UnixError(errcopy));
	}
	if (TotalCharsRead < TMGlobalMinimumSize
	    || TotalCharsRead < MinimumFileSize) {
		unlink(FileName); fclose(outF);
		if (Addr == NULL) return tmltf_MailTransputError;
		return AddrResult(tmltf_MailTransputError, Addr,
			"Input file too short--%d out of %d bytes", TotalCharsRead,
			(MinimumFileSize > TMGlobalMinimumSize ?
				MinimumFileSize : TMGlobalMinimumSize));
	}
	if (feof(outF) || ferror(outF)) {
		unlink(FileName); fclose(outF);
		if (Addr == NULL) return tmltf_MailTransputError;
		return AddrResult(tmltf_MailTransputError, Addr,
			"Error writing temp file %s: %s", FileName, UnixError(errno));
	}
	if (fclose(outF) == EOF) {
		unlink(FileName);
		if (Addr == NULL) return tmltf_MailTransputError;
		return AddrResult(tmltf_MailTransputError, Addr,
			"Error closing temp file %s: %s", FileName, UnixError(errno));
	}
	return 0;
}

static int WriteLocalMail(Addr, Hdr, WName, MBox, ForString, InLoop, Fwd, Dom, MBName)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
char *Hdr, *WName, *MBox, *ForString, *Dom, *MBName;
int InLoop;
{ /* Call vmail to deliver the mail.
	Handle all error conditions.
	Return -1 if there's no local mailbox, 0 if it was delivered,
	or a trymail.h error code otherwise.
*/
#define CharsToBuffer 1024
	long int retstatus, CharsRead, TotalCharsRead, AcctTime, WhenDead, ThisIsExpired;
	char ChBuffer[CharsToBuffer]; char *PDest, *midLine, *SPtr;
	struct stat SBuf;

#if Logs
	Log(300, "WriteLocalMail called");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "Doing local delivery on local name ``%s''.\n",
				Addr->LocalPart);
	PDest = NULL;
	if (MBox == NULL) {
		if (! InLoop) {
		    if (ULstrcmp(Dom, WorkstationCell) == 0) {
			PermMessage(tmlpf_NoMailbox, Addr, "No local mailbox for artificial mail destination.", NULL);
		    } else {
			ThisIsRemote(Addr, Fwd, Fwd);
			return tmltf_ForwardedOutOfAuth;
		    }
		}
		return -1;
	}
	if ((GlobalOptions & tmopt_HomeDelivery) == 0) {
	    return AddrResult(tmltf_MemoryExhausted, Addr, "Asked to postpone local mail");
	}
	if ((GlobalOptions & tmopt_NonAuthDelivery) == 0 && AnyAuthentication && CheckAMSDelivery(Dom) < 2) {
		ThisIsRemote(Addr, Fwd, Fwd);
		return tmltf_ForwardedOutOfAuth;
	}

	if (TMDebug) fprintf(stderr,
		"Considering calling vmail(NULL, %s, %s, %s, %s)\n",
			MBox, TextRetPath,
			(ForString == NULL ? "NULL" : ForString),
			(AuthString == NULL ? "NULL" : AuthString));

	if (! ReallyDeliver) {
		return AddrResult(tmok_RequestedNotToSend, Addr, "Requested not to send");
	}

	midLine = NULL;
	if (Hdr != NULL) {
	    PDest = AddrUnparse(Addr, &CharsRead);
	    if (PDest == NULL) {
		return AddrResult(tmltf_MemoryExhausted, Addr,
					"No memory: %d", CharsRead);
	    }
	    if (WasThisSent(Hdr, PDest, Addr) > 0) {
		free(PDest);
		return AddrResult(tmok_RedundantDelivery, Addr, "Already delivered");
	    }
	    retstatus = GetDefaultMsgID(Hdr, &midLine);
	    if (retstatus != 0) midLine = NULL;
	}
#if Logs
	if (DoTiming) VM_SetTiming(1);
#endif /* Logs */
#if Logs
	Log(310, "WriteLocalMail about to call VM_open");
#endif /* Logs */
	retstatus = VM_open(NULL, MBox, TextRetPath, ForString, AuthString, "trymail");
#if Logs
	Log(311, "VM_open returns %d to WriteLocalMail", retstatus);
#endif /* Logs */
	if (retstatus != EX_OK && ULstrcmp(Dom, GlobalDom) != 0) {
	    if (PDest != NULL) free(PDest);
	    ThisIsRemote(Addr, Fwd, Fwd);
	    return tmltf_ForwardedOutOfAuth;
	}
	switch (retstatus) {
	    case EX_OK:
		break;
	    case EX_TEMPFAIL:
		if (PDest != NULL) free(PDest);
		ThisIsExpired = IsExpired;
		WhenDead = EnqueueDate;
#ifdef EDQUOT
/* If it's ALittleExpired (over a day old), check whether we should reject it early.  We get into AcctTime the most recent of the mtime and ctime timestamps on the target directory and its parent, and if we're here because the destination is over quota and the latest of those timestamps was more than a couple of weeks old, we assume that the mail is going to a person who isn't really using the system, so we reject it back to the sender. */
		if (IsALittleExpired && !IsExpired) {
		    switch (VM_errordetail - vm_SystemErrorOffset) {
			case EDQUOT:
			    if (stat(MBox, &SBuf) == 0) {
				AcctTime = SBuf.st_mtime;
				if (AcctTime < SBuf.st_ctime) AcctTime = SBuf.st_ctime;
				SPtr = rindex(MBox, '/');
				if (*SPtr != '/') AcctTime = TV.Secs;  /* fail */
				else {
				    *SPtr = '\0';
				    if (stat(MBox, &SBuf) != 0) AcctTime = TV.Secs;  /* fail */
				    else {
					if (AcctTime < SBuf.st_mtime) AcctTime = SBuf.st_mtime;
					if (AcctTime < SBuf.st_ctime) AcctTime = SBuf.st_ctime;
				    }
				    *SPtr = '/';
				}
				if ((TV.Secs - AcctTime) > (AMS_ViceQueueLifetime + AMS_ExtraViceQueueLifetime)) {
				    WhenDead = AcctTime;
				    ThisIsExpired = TRUE;
				}
			    }
			    break;
			default:
			    break;
		    }
		}
#endif /* EDQUOT */
#ifdef EDQUOT
		if (ThisIsExpired && VM_errordetail == (vm_SystemErrorOffset + EDQUOT)) {
		    if (ULstrcmp(Dom, WorkstationCell) != 0) {
			ThisIsRemote(Addr, Fwd, Fwd);
			return tmltf_ForwardedOutOfAuth;
		    }
		    return PermMessage(tmlpf_CantOpen, Addr,
			"Mailbox consistently over quota",
			"The destination user has consistently been over quota for at least %d days (since %s), preventing mail from being delivered.  Please consider alternate methods for contacting this person.  The specific error message is:  %s.",
			  (TV.Secs - WhenDead) / (24*60*60), NiceTime(WhenDead),
			  VM_text);
		} else
#endif /* EDQUOT */
		    if (ThisIsExpired && VM_errordetail == (vm_SystemErrorOffset + ENOENT)) {
			if (ULstrcmp(Dom, WorkstationCell) != 0) {
			    ThisIsRemote(Addr, Fwd, Fwd);
			    return tmltf_ForwardedOutOfAuth;
			}
			return PermMessage(tmlpf_CantOpen, Addr,
			  "Missing mailbox directory",
	"The destination user has consistently had no %s directory for at least %d days (since %s), preventing mail from being delivered.  The specific error message is:  %s.",
			  MBName, (TV.Secs - WhenDead) / (24*60*60),
			  NiceTime(WhenDead), VM_text);
		} else if (ThisIsExpired && VM_errordetail == (vm_SystemErrorOffset + EACCES)) {
			if (ULstrcmp(Dom, WorkstationCell) != 0) {
			    ThisIsRemote(Addr, Fwd, Fwd);
			    return tmltf_ForwardedOutOfAuth;
			}
			return PermMessage(tmlpf_CantOpen, Addr,
			  "Inaccessible mailbox directory",
	"The destination user has consistently had an inaccessible %s directory for at least %d days (since %s), preventing mail from being delivered.  The specific error message is:  %s.",
			  MBName, (TV.Secs - WhenDead) / (24*60*60),
			  NiceTime(WhenDead), VM_text);
		}
		return AddrResult(tmltf_MailerExecFailure, Addr, "VM_open failed: %d, %s",
				retstatus, VM_text);
	    default:
		if (PDest != NULL) free(PDest);
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    return PermMessage(tmlpf_CantOpen, Addr, "Can't create mail file",
				"VM_open failed (%d): %s.", retstatus, VM_text);
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	}
	if (midLine != NULL) VM_printf("%s\n", midLine);
	clearerr(InFile);
	rewind(InFile);
#if Logs
	Log(312, "Input file cleared and rewound");
#endif /* Logs */
	TotalCharsRead = 0;
	errno = 0;
	for (;;) {
		CharsRead = fread(ChBuffer, sizeof(char), CharsToBuffer, InFile);
		if (CharsRead == 0 && ( feof(InFile) || ferror(InFile) )) break;
		retstatus = VM_write(ChBuffer, CharsRead);
		if (retstatus != EX_OK) break;
		TotalCharsRead += CharsRead;
	}
	if (ferror(InFile)) {
		if (errno == 0) errno = EIO;
		if (tfail(errno)) {
		    retstatus = EX_TEMPFAIL;
		} else {
		    if (PDest != NULL) free(PDest);
		    return AddrResult(tmltf_MailTransputError, Addr, "%s", UnixError(errno));
		}
	}
	if (retstatus != EX_OK && ULstrcmp(Dom, GlobalDom) != 0) {
	    if (PDest != NULL) free(PDest);
	    ThisIsRemote(Addr, Fwd, Fwd);
	    return tmltf_ForwardedOutOfAuth;
	}
	switch (retstatus) {
	    case EX_OK:
		break;
	    case EX_TEMPFAIL:
		if (PDest != NULL) free(PDest);
		return AddrResult(tmltf_MailTransputError, Addr, "VM_write failed: %d, %s",
				retstatus, VM_text);
	    default:
		if (PDest != NULL) free(PDest);
		return PermMessage(tmlpf_CantWrite, Addr, "Can't write mail file",
				"VM_write failed (%d): %s.", retstatus, VM_text);
	}
#if Logs
	Log(320, "File all read and written to VM_write: %d characters", TotalCharsRead);
#endif /* Logs */
	if (TotalCharsRead < TMGlobalMinimumSize || TotalCharsRead < MinimumFileSize) {
		(void) VM_close();
		if (PDest != NULL) free(PDest);
		return AddrResult(tmltf_MailTransputError, Addr,
			"Input file too short--%d out of %d bytes", TotalCharsRead,
			(MinimumFileSize > TMGlobalMinimumSize ?
				MinimumFileSize : TMGlobalMinimumSize));
	}
#if Logs
	Log(321, "About to call VM_close()");
#endif /* Logs */
	retstatus = VM_close();
#if Logs
	Log(322, "VM_close returns %d to WriteLocalMail", retstatus);
#endif /* Logs */
	if (retstatus != EX_OK && ULstrcmp(Dom, GlobalDom) != 0) {
	    if (PDest != NULL) free(PDest);
	    ThisIsRemote(Addr, Fwd, Fwd);
	    return tmltf_ForwardedOutOfAuth;
	}
	switch (retstatus) {
	    case EX_OK:
		break;
	    case EX_TEMPFAIL:
		if (PDest != NULL) free(PDest);
#ifdef EDQUOT
		if (IsExpired && VM_errordetail == (vm_SystemErrorOffset + EDQUOT)) {
			return PermMessage(tmlpf_CantOpen, Addr,
			  "Mailbox consistently over quota",
	"The destination user has consistently been over quota for at least %d days (since %s), preventing mail from being delivered.  The specific error message is:  %s.",
			  AMS_ViceQueueLifetime / (24*60*60), NiceTime(EnqueueDate),
			  VM_text);
		}
#endif /* EDQUOT */
		return AddrResult(tmltf_MailerSystemFailure, Addr, "VM_close failed: %d, %s",
				retstatus, VM_text);
	    default:
		if (PDest != NULL) free(PDest);
		return PermMessage(tmlpf_CantClose, Addr, "Can't close mail file",
				"VM_close failed (%d): %s.", retstatus, VM_text);
	}
	if (Hdr != NULL) ThisWasSent(Hdr, PDest, Addr, 0);	/* Record that this was delivered */
	if (PDest != NULL) free(PDest);
#if Logs
	Log(323, "WriteLocalMail returning");
#endif /* Logs */
	return AddrResult(tmok_DeliveredGoodPrint, Addr, WName);	/* WName can be NULL here */
}

enum ITUType {itu_none, itu_alter, itu_send, itu_return};

static enum ITUType scanITU(ValBegin, ValEnd)
char *ValBegin, *ValEnd;
{/* Scan an If-Type-Unsupported: value and return the code to which it corresponds. */
	enum ITUType Res;
	char *Dum, *CopyHdr; int CopyHdrSize;

	errno = 0;
	Res = itu_none;	/* Can be defaulted elsewhere */
	CopyHdrSize = (ValEnd - ValBegin) + 2;
	CopyHdr = malloc(CopyHdrSize);
	if (CopyHdr == NULL) {errno = ENOMEM; return Res;}
	Dum = Next822LPart(ValBegin, ValEnd, CopyHdr, CopyHdrSize);
	if (Dum != NULL) {
	    if (ULstrcmp(CopyHdr, "alter") == 0) Res = itu_alter;
	    else if (ULstrcmp(CopyHdr, "send") == 0) Res = itu_send;
	    else if (ULstrcmp(CopyHdr, "ignore") == 0) Res = itu_send;
	    else if (ULstrcmp(CopyHdr, "return") == 0) Res = itu_return;
	    else if (ULstrcmp(CopyHdr, "reject") == 0) Res = itu_return;
	    else errno = ENOENT;
	}
	free(CopyHdr);
	return Res;
}

/* forward reference to procedure */
enum FwdType {Forwarding, DistList, Group};
static void HandleForwarding(), HandleRecipient();

static int HandleFSMembers(Addr, GroupName, ForString, Fwd, PKey, Dom)
PARSED_ADDRESS *Addr;
char *GroupName, *ForString, *Dom;
struct FwdLink *Fwd;
wp_PrimeKey PKey;
{ /* Deliver mail to the members of an AFS group. */
#ifdef AFS30_ENV
	int Err, Count, NewLen, IsTransient;
	char *Members, *SP, *DP, *NewList;
#endif /* #ifdef AFS30_ENV */

	if (TMDebug) fprintf(stderr, "HandleFSMembers(addr, %s, %s, fwd) called.\n",
			GroupName, (ForString == NULL ? "null" : ForString));
#ifndef AFS30_ENV
	if (ULstrcmp(Dom, WorkstationCell) == 0) { /* reject here and now */
	    return PermMessage(tmlpf_LocalNameError, Addr, "No FS-group support",
			       "There is no support for learning about members of file-system groups in domain %s.", Dom);
	} else { /* pass to Dom for its authoritative dispensation. */
	    ThisIsRemote(Addr, Fwd, Fwd);
	    return tmltf_ForwardedOutOfAuth;
	}
#endif /* #ifndef AFS30_ENV */
#ifdef AFS30_ENV
	errno = 0;
	Members = NULL;
	Err = aq_GetGroupMembers(GroupName, Dom, &Members);
	if (Err != 0) {
	    IsTransient = TRUE;
	    if (Err == -ALQ_ERRNO) {
		if (!tfail(errno)) IsTransient = FALSE;
	    } else if (Err <= -ALQ_EPRS_BASE) {
		switch (-Err - ALQ_EPRS_BASE + ERROR_TABLE_BASE_pr) {
		    case PRNOENT:
		    case PRPERM:
		    case PRNOTGROUP: case PRNOTUSER:
		    case PRBADNAM:
			IsTransient = FALSE;
		}
	    } else {
		switch (-Err) {
		    case ALQ_NOT_GROUP:
		    case ALQ_SYSTEM_GROUP:
		    case ALQ_ENONAME:
			IsTransient = FALSE;
		}
	    }
	    if (Members != NULL) free(Members);
	    if (IsTransient) { /* Transient failure */
		return AddrResult(tmltf_MailerTempFailure, Addr, "Can't evaluate group %s@%s: %s", GroupName, Dom, aq_GetLastErrorMessage());
	    } else { /* Permanent failure */
		if (ULstrcmp(Dom, WorkstationCell) == 0) { /* authority */
		    return PermMessage(tmlpf_LocalNameError, Addr, "Name not a group",
				       "The name ``%s'' does not correspond to a file system group in domain %s: %s.", GroupName, Dom, aq_GetLastErrorMessage());
		} else { /* non-authority */
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	    }
	}
	Count = 1;
	for (SP = Members; *SP != '\0'; ++SP) if (*SP == '\n') ++Count;
	NewLen = strlen(Members) + Count*(strlen(Dom) + 3);
	NewList = malloc(NewLen);
	if (NewList == NULL) {
	    free(Members);
	    return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}
	DP = NewList;
	for (SP = Members; *SP != '\0'; ++SP) {
	    if (*SP != '\n') {
		*DP++ = *SP;
	    } else {
		*DP++ =	'+';	/* Blithely assume that group members are WP pw_names */
		*DP++ = '@';
		strcpy(DP, Dom);
		DP += strlen(Dom);
		*DP++ = ',';
	    }
	}
/* Group lists are not NL-terminated */
	if (*Members != '\0') {	/* if there were any list elements at all */
	    *DP++ = '+';
	    *DP++ = '@';
	    strcpy(DP, Dom);
	    DP += strlen(Dom);
	}
	*DP++ = '\0';
	free(Members);
	if (TMDebug) fprintf(stderr, "Contents of group %s (domain %s) is: ``%s''\n", GroupName, Dom, NewList);
	HandleForwarding(Addr, NULL, NULL, NewList, Fwd, PKey, DistList, Dom);
	free(NewList);
	return 0;
#endif /* #ifdef AFS30_ENV */
}

static int HandleFileList(Addr, FileName, ForString, Fwd, PKey, Dom)
PARSED_ADDRESS *Addr;
char *FileName, *ForString, *Dom;
struct FwdLink *Fwd;
wp_PrimeKey PKey;
{ /*	Deliver mail to a distribution list.  This is a heavier-weight operation than forwarding it locally, because the return-path is specified anew in the distribution list file.  We implement the altering of the return-path by checking if the new, computed return path is the same as the one given to this program on its command line.  If it is the same, we go ahead and deliver the mail here.  If it is different, we call the queuemail program with the new return-path and the given address as a destination.
	We compute the new return-path in one of two ways.  If the given distribution list file begins with ``Distribution-Errors-to:'', then the rest of that line is assumed to be the new return path.  If it does not begin with that string, the owner of the file is assumed to be the recipient for errors.  In any event, the remainder of the file is taken as an RFC822 address list.  Lines beginning with semicolons are assumed to be comments and are ignored.
	The addresses corresponding to the distribution list must follow the tag ``Distribution-Content:'' in the distribution list file.
  */
	char *NewRetPathText;
	PARSED_ADDRESS *NewRetPath, *OneAddr;
	FILE *DistFile;
	struct stat DistStat;
	char *Signature = "Distribution";
	char *ErrorsToTag = "Distribution-Errors-to:";
	char *ErrorsToSender = "Distribution-Errors-To-Originator:";
	char *AddressIntro = "Distribution-Content:";
	char *DITUField = "Distribution-If-Type-Unsupported:";
	char *ITUField = "If-Type-Unsupported:";
	char *FileContent, *NewContent, *Src, *Dst, *PMInCell;
	char *DestAddressList;
	int C, Err, ThisErrno, Count, HostCount, NewRetPathLen, PGrp, TimedOut;
	long int LongErr;
	wp_ErrorCode WPErr;
	int Argc; static char *Argv[23];
	FILE *queuem;
	auto char FileDom[100]; auto char fnBuff[1000];
	char *MsgHdr, *MsgDst, *FunkyDest, *Finger, *NewFinger, *FldBegin, *FldMiddle, *FldEnd;
	int RetPathOKAsIs, ITUCodeOKAsIs;
	enum ITUType ITUval, DITUval;

	if (TMDebug) fprintf(stderr, "HandleFileList(addr, %s, %s, fwd) called.\n",
			FileName, (ForString == NULL ? "null" : ForString));
	ITUval = itu_alter;
	DITUval = itu_none;
	errno = 0;
	DistFile = fopen(FileName, "r");
	if (DistFile == NULL && errno == 0) errno = ENFILE;
	ThisErrno = errno;
	if (DistFile != NULL) {
		Err = fstat(fileno(DistFile), &DistStat);
		if (Err < 0) {ThisErrno = errno; fclose(DistFile); DistFile = NULL;}
	}
	if (DistFile == NULL) {
		if (tfail(ThisErrno)) {
			return AddrResult(tmltf_FileUnreachable, Addr, UnixError(ThisErrno));
		} else {
		    if (ULstrcmp(Dom, WorkstationCell) == 0) {
			return PermMessage(tmlpf_FileUnreachable, Addr,
				"Distribution file unreachable",
				"The local file ``%s'' is to contain distribution information, but it is inaccessible: %s.",
				FileName, UnixError(ThisErrno));
		    } else {
			ThisIsRemote(Addr, Fwd, Fwd);
			return tmltf_ForwardedOutOfAuth;
		    }
		}
	}
	if ((DistStat.st_mode & S_IFMT) != S_IFREG) {
		fclose(DistFile);
		if ((DistStat.st_mode & S_IFMT) == S_IFDIR) {
			return PermMessage(tmlpf_FileUnreachable, Addr,
				"Distribution file is a directory",
				"The distribution list file %s is a directory, not a regular text file.",
				FileName);
		} else {
			return PermMessage(tmlpf_FileUnreachable, Addr,
				"Distribution file not a text file",
				"The distribution list file %s is not a regular text file, but is of type %#o.",
				FileName, (DistStat.st_mode & S_IFMT));
		}
	}
/* Don't complain about short files until they're an hour or more old */
	if (DistStat.st_size < 2) {
	    fclose(DistFile);
	    if (DistStat.st_mtime < (time(0) + 60*60)) {
		return PermMessage(tmlpf_FileUnreachable, Addr, "Zero-length distribution file",
			"The distribution list file %s is too short to be legitimate.", FileName);
	    } else {
		return AddrResult(tmltf_MailerTempFailure, Addr, "Zero-length distribution file");
	    }
	}
/* Now make sure we're working in the correct cell. */
	C = GetCellFromFileName(FileName, FileDom, sizeof(FileDom));
	if (C == EINVAL || C == ENOENT) {
		fclose(DistFile);
		return PermMessage(tmlpf_FileUnreachable, Addr,
			"Distribution file not in AFS",
			"A distribution list file must be in the Andrew File System.  The given file, %s, is not.",
			FileName);
	}
	if (C != 0) return AddrResult(tmltf_MailerTempFailure, Addr,
			(vdown(C) ? "AFS down for dist list file: %s"
				: "Can't find cell for file: %s"), UnixError(C));
	if (ULstrcmp(FileDom, Dom) != 0) {	/* Dist list file is in another cell! */
		int AMSDel = CheckAMSDelivery(FileDom);
		char *NewDom;

		fclose(DistFile);
		if (AMSDel == 0) return AddrResult(tmltf_MailerTempFailure, Addr,
			"Distribution file's cell, %s, might not run AMS Delivery", FileDom);
		if (AMSDel < 0 && ULstrcmp(Dom, WorkstationCell) == 0) {
		    NewDom = NewString(FileDom);
		    if (NewDom == NULL) return AddrResult(tmltf_MemoryExhausted, Addr, "No more storage");
		    free(Addr->Hosts->Prev->Name);
		    Addr->Hosts->Prev->Name = NewDom;
		    la_FreeMD(Addr->MD);
		    Addr->MD = NULL;
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
		if (AMSDel < 0) return PermMessage(tmlpf_FileUnreachable, Addr,
			"Distribution file not under AMS Delivery",
			"The distribution list file %s is in cell %s, which does not run the AMS Delivery System.",
			FileName, FileDom);
/* Rewrite the address to reflect the real home of the dist list file.  Then recurse! */
		NewDom = NewString(FileDom);
		if (NewDom == NULL) return AddrResult(tmltf_MemoryExhausted, Addr, "No more storage");
		free(Addr->Hosts->Prev->Name);
		Addr->Hosts->Prev->Name = NewDom;
		la_FreeMD(Addr->MD);
		Addr->MD = NULL;
		HandleRecipient(Addr, Fwd, FileDom);
		return 0;
	}
	FileContent = malloc(DistStat.st_size + 2);
	if (FileContent == NULL) {
		fclose(DistFile);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No more storage");
	}
	errno = 0;
	Err = fread(FileContent, 1, DistStat.st_size, DistFile);
	if (Err != DistStat.st_size) {
		ThisErrno = errno;
		fclose(DistFile); free(FileContent);
		return AddrResult(tmltf_MailTransputError, Addr, "Error reading dist list: %s",
				UnixError(ThisErrno));
	}
	fclose(DistFile);
	if (FileContent[Err - 1] != '\n') FileContent[Err++] = '\n';
	FileContent[Err] = '\0';
	if (TMDebug > 1) fprintf(stderr, "Dist file owned by UID %d, contents: ``%s''.\n",
				DistStat.st_uid, FileContent);
	NewContent = malloc(DistStat.st_size + 2);
	if (NewContent == NULL) {
		free(FileContent);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No more storage");
	}
	Src = FileContent;
	Dst = NewContent;
	C = 0;	/* 0 for saw-NL, 1 for normal, 2 for in-comment */
	for (Count = Err; Count > 0; Count--) {
		switch (C) {
		case 0:	if (*Src == ';') {C = 2; break;}
			C = 1;		/* and fall through */
		case 1:	if (*Src != '\0') *Dst++ = *Src;
			if (*Src == '\n') C = 0;
			break;
		case 2:	if (*Src == '\n') C = 0;
			break;
		}
		++Src;
	}
	*Dst++ = '\0';
	Count = (Dst - NewContent);
	NewContent = realloc(NewContent, Count);
	if (TMDebug > 1) fprintf(stderr, "Compacted dist file contents: ``%s''.\n", NewContent);
	free(FileContent);
/* Check the file content for reasonableness */
	Src = NewContent;
	while (*Src == '\n') ++Src;
	if ((*Src == '\0') || (ULstlmatch(Src, Signature) == 0)) {
		free(NewContent);
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    Dst = Src;
		    while (*Dst != '\n' && *Dst != '\0') ++Dst;
		    *Dst = '\0';
		    return PermMessage(tmlpf_BadForwardingInfo, Addr, "Not a distribution list file",
	"The non-comment portion of a distribution list file must begin with the word Distribution; the file ``%s'' does not (since it begins with the text ``%s''%s), and therefore is not a distribution list file.",
			FileName, Src, (ULstlmatch(Src, "\\begindata{") == 0 ? "" : ", indicating an ATK-formatted file"));
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	}
	Src = NewContent;
	for (Src = NewContent; *Src != '\0'; ++Src) {
	    if (! isascii(*Src)) {
		free(NewContent);
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    return PermMessage(tmlpf_BadForwardingInfo, Addr, "Not a plain-text file",
			"Distribution list file ``%s'' contains bytes that are not ordinary text.",
			FileName);
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	    }
	}
/* Now extract the new delivery parameters. */
	NewRetPathText = NULL;
	RetPathOKAsIs = FALSE;
	for (Finger = NewContent; ; Finger = NewFinger) {
	    for (;;) {
		Err = 0;
		for (Src = Finger; ; ++Src) {
		    if (*Src == '\0' || *Src == '\n') break;
		    if (! isspace(*Src)) {Err = 1; break;}
		}
		if (Err != 0) break;
		if (*Src == '\n') ++Src;
		Finger = Src;	/* skip lines with just spaces on them */
		if (*Finger == '\0') break;
	    }
	    if (*Finger == '\0') {
		free(NewContent);
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    return PermMessage(tmlpf_BadForwardingInfo, Addr, "Not a distribution list",
			"A distribution list file must contain the heading ``%s''; file ``%s'' does not.",
			AddressIntro, FileName);
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	    }
	    NewFinger = index(Finger, '\n') + 1;
	    if (ULstlmatch(Finger, ErrorsToTag)) {	/* Ret path given in the file */
		if (NewRetPathText != NULL) {
			free(NewContent);	/* only once, thanks */
			return PermMessage(tmlpf_BadForwardingInfo, Addr,
				"Multiple Distribution-Errors-To lines",
				"The distribution list file %s may have only one %s line.",
				FileName, ErrorsToTag);
		}
		Src = &Finger[strlen(ErrorsToTag)];
		while (*Src != '\0' && index("\t ", *Src) != NULL) ++Src;
		Dst = index(Src, '\n');
		do --Dst; while (*Dst != '\0' && index("\t ", *Dst) != NULL) ;
		C = *++Dst;
		*Dst = '\0';
		if (strcmp(Src, "<>") == 0) {	/* null ret-path always OK */
			NewRetPathText = NewString("<>");
			if (NewRetPathText == NULL) {
				free(NewContent);
				return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
			}
		} else {				/* not the null ret-path */
			ThisErrno = ParseAddressList(Src, &NewRetPath);
			if (ThisErrno == PA_SYNTAX_ERROR) {
				free(NewContent);
				if (ULstrcmp(Dom, WorkstationCell) == 0) {
				    return PermMessage(tmlpf_BadForwardingInfo, Addr,
					"Bad syntax for Distribution-Errors-To: address",
			"In distribution list file %s, the Distribution-Errors-To: address, ``%s'', has bad syntax.",
					FileName, Src);
				} else {
				    ThisIsRemote(Addr, Fwd, Fwd);
				    return tmltf_ForwardedOutOfAuth;
				}
			} else if (ThisErrno == PA_NO_MEM || ThisErrno == PA_PARTIAL) {
				free(NewContent);
				return AddrResult(tmltf_MemoryExhausted, Addr,
						"Error (%d) parsing destinations", ThisErrno);
			} else if (ThisErrno != PA_OK) {
				free(NewContent);
				return AddrResult(tmlpf_BadForwardingInfo, Addr,
					"Error (%d) parsing destinations", ThisErrno);
			}
			Count = 0;
			CountAddresses(NewRetPath, &Count);
			if (Count != 1) {
				FreeAddressList(NewRetPath);
				free(NewContent);
				return PermMessage(tmlpf_BadForwardingInfo, Addr,
					"not exactly one distribution-errors-to address",
	"The argument of Distribution-Errors-To: in the distribution list file %s, ``%s'', must be a single address, not %d addresses",
					FileName, Src, Count);
			}
			OneAddr = NewRetPath->Next;
			HostCount = 0;
			FOR_ALL_REVERSE_HOSTS(Hst, OneAddr, {HostCount++;})
			NewRetPathLen = strlen(OneAddr->LocalPart) + 3;
			if (HostCount == 0) {
				NewRetPathLen += (strlen(Dom) + 1);
			} else {
				NewRetPathLen += HostCount * 2;
				FOR_ALL_REVERSE_HOSTS(Hst, OneAddr,
					{NewRetPathLen += strlen(Hst->Name);})
			}
			NewRetPathText = malloc(NewRetPathLen);
			if (NewRetPathText == NULL) {
				FreeAddressList(NewRetPath);
				free(NewContent);
				return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
			}
			strcpy(NewRetPathText, "<");
			switch (HostCount) {
		    case 0:
			strcat(NewRetPathText, OneAddr->LocalPart);
			strcat(NewRetPathText, "@");
			strcat(NewRetPathText, Dom);
			break;
		    case 1:
			strcat(NewRetPathText, OneAddr->LocalPart);
			strcat(NewRetPathText, "@");
			FOR_ALL_REVERSE_HOSTS(Hst, OneAddr,
				{strcat(NewRetPathText, Hst->Name);})
			break;
		    default:
			Count = HostCount;
			FOR_ALL_REVERSE_HOSTS(Hst, OneAddr, {
				if (Count == 1) {
					strcat(NewRetPathText, ":");
					strcat(NewRetPathText, OneAddr->LocalPart);
					strcat(NewRetPathText, "@");
					strcat(NewRetPathText, Hst->Name);
				} else {
					if (Count != HostCount)
						strcat(NewRetPathText, ",");
					strcat(NewRetPathText, "@");
					strcat(NewRetPathText, Hst->Name);
				}
				Count--;
			})
			break;
			}
			strcat(NewRetPathText, ">");
			if (TMDebug) fprintf(stderr,
				"New ret path text is ``%s''.\n", NewRetPathText);
			FreeAddressList(NewRetPath);
		}
	    } else if (ULstlmatch(Finger, ErrorsToSender)) {    /* tag for leaving ret. path alone */
		Src = &Finger[strlen(ErrorsToSender)];
		while (*Src != '\0' 	&& index(" \t", *Src) != NULL) ++Src;
		Dst = index(Src, '\n');
		if (Dst != NULL) {
			--Dst;
			while (*Dst != '\0' && index(" \t", *Dst) != NULL && Dst > Src)
				--Dst;
			*++Dst = '\0';
		}
		if (	strlen(Src) == 0 ||
			ULstlmatch("yes", Src) ||
			ULstlmatch("true", Src) ||
			(ULstrcmp("on", Src) == 0) ||
			(strcmp("1", Src) == 0)	  ) RetPathOKAsIs = TRUE;
	    } else if (ULstlmatch(Finger, DITUField)) {    /* tag for specifying If-Type-Unsupported: value */
		Src = &Finger[strlen(DITUField)];
		while (*Src != '\0' 	&& index(" \t", *Src) != NULL) ++Src;
		Dst = index(Src, '\n');
		if (Dst != NULL) {
			--Dst;
			while (*Dst != '\0' && index(" \t", *Dst) != NULL && Dst > Src)
				--Dst;
			*++Dst = '\0';
		}
		DITUval = scanITU(Src, Dst);
		if (DITUval == itu_none) {
			free(NewContent);
			if (errno == ENOENT) {
			    if (ULstrcmp(Dom, WorkstationCell) == 0) {
				return PermMessage(tmlpf_BadForwardingInfo, Addr,
					"Bad code in dist list",
					"The %s field must be followed by a valid code such as ``send'' or ``return,'' rather than ``%s''.",
					DITUField, Src);
			    } else {
				ThisIsRemote(Addr, Fwd, Fwd);
				return tmltf_ForwardedOutOfAuth;
			    }
			} else {
				return AddrResult(tmltf_MemoryExhausted, Addr,
						"Out of memory scanning ITU code");
			}
		}
	    } else if (ULstlmatch(Finger, AddressIntro)) {	/* the start of the addresses */
		DestAddressList = &Finger[strlen(AddressIntro)];
		while (isspace(*DestAddressList)) ++DestAddressList;
		break;	/* OK--read the addresses now */
	    } else if (ULstlmatch(Finger, Signature)) {		/* some other Distribution field */
		;	/* allow for future expansion */
	    } else {
		free(NewContent);
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    return PermMessage(tmlpf_BadForwardingInfo, Addr, "Not a distribution list prefix",
	"The addresses contained in a distribution list file must be preceded with the heading ``%s''; those in the file ``%s'' are not.",
			AddressIntro, FileName);
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	    }
	}
/* All done reading initial lines--fill in defaulted values */
	if (NewRetPathText == NULL) {	/* need to extract ret path from owner */
		if (DistStat.st_uid < 0)
			WPErr = wperr_NoKeysFound;
		else
			WPErr = IDtoName(DistStat.st_uid, &Dst, &NewRetPathText, Dom);
		if (WPErr != wperr_NoError) {
			free(NewContent);
			if (WPErr == wperr_NoKeysFound
			  || WPErr == wperr_TooManyKeysFound) {
			      if (ULstrcmp(Dom, WorkstationCell) == 0) {
				return PermMessage(tmlpf_NoOwner, Addr, "No owner for distribution list",
	"Distribution list file ``%s'' has no Distribution-Errors-To: field; its owner number is %d in cell %s, which cannot be translated into a username to make a return-path because %s.",
				FileName, DistStat.st_uid, Dom,
				(WPErr == wperr_NoKeysFound ?
					"there is no matching username" :
				  (WPErr == wperr_TooManyKeysFound ?
					"there are multiple usernames for that number" :
				      wp_ErrorString(WPErr))) );
			      } else {
				  ThisIsRemote(Addr, Fwd, Fwd);
				  return tmltf_ForwardedOutOfAuth;
			      }
			} else {
				return AddrResult(tmltf_WhitePagesRunFailure, Addr,
						wp_ErrorString(WPErr));
			}
		}
		NewRetPathLen = strlen(Dst) + strlen(Dom) + 6;
		NewRetPathText = malloc(NewRetPathLen);
		if (NewRetPathText == NULL) {
			FreeAddressList(NewRetPath);
			free(NewContent);
			return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
		}
		sprintf(NewRetPathText, "<%s+@%s>", Dst, Dom);
	}

/* File malloced in NewContent, addresses pointed to by DestAddressList, new ret path is NewRetPathText (malloced).  See if this return path is what we're running as. */
	if (TMDebug) fprintf(stderr, "Old ret path ``%s'', new ``%s'':\n",
				TextRetPath, NewRetPathText);
/* If the return-paths are identical, we can go ahead and deliver the mail. */
	if (RetPathOKAsIs == 0)
	    if (strcmp(NewRetPathText, TextRetPath) == 0) RetPathOKAsIs = TRUE;
/* Now, try to stop recursive notifications.  If the existing return-path looks like the message is an error report, don't rewrite it to follow the distribution list mechanism. */
	if (RetPathOKAsIs == 0)
	    if (	strcmp(TextRetPath, "<>") == 0 ||
		ULstlmatch(TextRetPath, "<MAILER-DAEMON") ||
		ULstlmatch(TextRetPath, "<postman+") ||
		ULstlmatch(TextRetPath, "<postman#") ||
		ULstlmatch(TextRetPath, "<Postmaster@") ||
		ULstlmatch(TextRetPath, "<Postmast@") ||
		ULstlmatch(TextRetPath, "<MMAILR") ||
		ULstlmatch(TextRetPath, "<XMAILR") ||
		ULstlmatch(TextRetPath, "<Delivr") ||
		ULstlmatch(TextRetPath, "<Mailer@") ||
		ULstlmatch(TextRetPath, "<MAILER%")    ) RetPathOKAsIs = TRUE;
	if (RetPathOKAsIs == 0)
	   if (	TextRetPath[0] == '<'
	     &&	ULstlmatch(&TextRetPath[1], PostmasterName) != 0
	     &&	TextRetPath[strlen(PostmasterName) + 1] == '+') RetPathOKAsIs = TRUE;
	if (RetPathOKAsIs == 0) {
	    PMInCell = CheckAMSPMName(Dom);
	    if (PMInCell != NULL) {
		if (TextRetPath[0] == '<'
		    && ULstlmatch(&TextRetPath[1], PMInCell) != 0
		    && TextRetPath[strlen(PMInCell) + 1] == '+') RetPathOKAsIs = TRUE;
	    }
	}
/* Now check the If-Type-Unsupported: status. */
	ITUCodeOKAsIs = TRUE;
	MsgHdr = NULL;
	if (DITUval != itu_none) {	/* Check for agreement with the file we're sending. */
	    if (TMDebug) fprintf(stderr, "Dist list needs ITU code ``%s''; checking message...\n",
		(DITUval == itu_alter ? "alter" : DITUval == itu_send ? "send" : "return"));
	    Err = GetHeader(&MsgHdr);
	    if (Err != 0) {
		free(NewContent); free(NewRetPathText);
		return AddrResult(tmltf_MailerExecFailure, Addr,
				"can't read header: %s",
				(Err < 0 ? "no memory" : UnixError(Err)));
	    }
	    /* Having read the header, check to see if there's a field with the wrong value. */
	    if (BracketField(MsgHdr, "Content-Type:", &FldMiddle, &FldEnd, &FldBegin) == 1) {
		if (BracketField(MsgHdr, ITUField, &FldMiddle, &FldEnd, &FldBegin) == 1) {
		    ITUval = scanITU(FldMiddle, FldEnd);
		    if (ITUval == itu_none) {
			Err = errno;
			free(NewContent); free(NewRetPathText); free(MsgHdr);
			return AddrResult(tmltf_MailerExecFailure, Addr,
				"Can't decipher %s code: %s", ITUField,
				(Err == ENOENT ? "unrecognized" : UnixError(Err)));

		    }
		} else ITUval = itu_alter;	/* default if absent: ``alter'' */
		if (ITUval == DITUval) {
		    if (TMDebug) fprintf(stderr, "Message's ITU is the same.\n");
		} else {
		    if (TMDebug) fprintf(stderr, "Message's ITU is different: %s.\n",
			(ITUval == itu_alter ? "alter" : ITUval == itu_send ? "send" : "return"));
		    ITUCodeOKAsIs = FALSE;	/* aggh.  Have to enqueue it. */
		}
	    } else if (TMDebug) fprintf(stderr, "Message has no Content-type: field.\n");
	}

	if (RetPathOKAsIs && ITUCodeOKAsIs) {	/* OK--just deliver it */
		if (TMDebug) fprintf(stderr, "Leaving old ret-path and ITU code alone.\n");
		free(NewRetPathText); if (MsgHdr != NULL) free(MsgHdr);
		HandleForwarding(Addr, NULL, NULL, DestAddressList,
				Fwd, PKey, DistList, Dom);
		free(NewContent);
		return 0;
	}
/* Return paths or ITU codes are different--invoke queuemail with new parameters. */
	if (TMDebug) fprintf(stderr, "Ret path or ITU code different; invoking queuem.\n");
	free(NewContent);
	if (! ReallyDeliver) {
		if (TMDebug) fprintf(stderr, "NOT DELIVERING.\n");
		AddrResult(tmok_RequestedNotToSend, Addr, "Requested not to send");
		free(NewRetPathText); if (MsgHdr != NULL) free(MsgHdr);
		return 0;
	}

	FunkyDest = NULL;
	if (MsgHdr == NULL) {
		Err = GetHeader(&MsgHdr);
		if (Err != 0) {
			free(NewRetPathText);
			return AddrResult(tmltf_MailerExecFailure, Addr,
				"can't read header: %s",
				(Err < 0 ? "no memory" : UnixError(Err)));
		}
	}
	MsgDst = AddrUnparse(Addr, &LongErr);
	if (MsgDst == NULL) {
		free(MsgHdr); free(NewRetPathText);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No mem: %d", LongErr);
	}
	FunkyDest = malloc(strlen(MsgDst) + strlen(TextRetPath) + 12);
	if (FunkyDest == NULL) {
		free(MsgHdr); free(NewRetPathText); free(MsgDst);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory left");
	}
	sprintf(FunkyDest, "%s (from: %s)", MsgDst, TextRetPath);
	free(MsgDst);
	Err = WasThisSent(MsgHdr, FunkyDest, Addr);
	if (Err > 0) {
		free(MsgHdr); free(NewRetPathText); free(FunkyDest);
		return AddrResult(tmok_RedundantDelivery, Addr, "loop already expanded");
	}
	MsgDst = NULL;
	if (! ITUCodeOKAsIs) switch (DITUval) {
		case itu_alter:	MsgDst = "alter"; break;
		case itu_send:	MsgDst = "send"; break;
		case itu_return:	MsgDst = "return"; break;
		default:		break;
	}
	Err = MakeFile(Addr, MsgHdr, fnBuff, MsgDst);
	if (Err != 0) {free(MsgHdr); free(NewRetPathText); free(FunkyDest); return Err;}	/* message already issued */
	Argc = -1;
	Argv[++Argc] = queuemail;
	if (TMDebug) {Argv[++Argc] = "-D"; Argv[++Argc] = "-5";}
	Argv[++Argc] = "-f";
	Argv[++Argc] = fnBuff;
	Argv[++Argc] = "-r";
	Argv[++Argc] = RetPathOKAsIs ? TextRetPath : NewRetPathText;
	/* if (IsTrymail() == 0) Argv[++Argc] = "-s"; */	/* if switchmail, call switchmail */
	if (IsTrymail() == 0 || OrigDestinationCount > 10)
		Argv[++Argc] = "-z";	/* queue, don't process now */
	if (AuthString != NULL) {
		Argv[++Argc] = "-A";	/* generate an auth string in queuemail */
		Argv[++Argc] = AuthString;
	}
	if (ForString != NULL) {
		Argv[++Argc] = "-4";	/* generate For string in queuemail */
		Argv[++Argc] = ForString;
	}
	if (homeCell != NULL) {
		Argv[++Argc] = "-Ch";	/* specify a home cell for loop elimination. */
		Argv[++Argc] = homeCell;
	}
	Argv[++Argc] = "-T";	/* don't generate massive PM messages on full disks */
	Argv[++Argc] = "-a";	/* following is an address, even if starts with hyphen */
	MsgDst = malloc(strlen(Addr->LocalPart) + strlen(Dom) + 2);
	if (MsgDst == NULL) {
		free(NewRetPathText); free(MsgHdr); unlink(fnBuff); free(FunkyDest);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory left for dest");
	}
	sprintf(MsgDst, "%s@%s", Addr->LocalPart, Dom);
	Argv[++Argc] = MsgDst;
	Argv[++Argc] = NULL;
	if (TMDebug) {
		fprintf(stderr, "Sending mail to list with command: %s", Argv[0]);
		for (C = 1; Argv[C] != NULL; C++) fprintf(stderr, ", %s", Argv[C]);
		fprintf(stderr, "\n");
	}
	errno = 0;
	queuem = topen(Argv[0], Argv, "w", &PGrp);
	if (queuem == NULL) {
		ThisErrno = errno;
		free(NewRetPathText); free(MsgHdr); unlink(fnBuff); free(FunkyDest); free(MsgDst);
		return AddrResult(tmltf_MailerExecFailure, Addr, "queuemail: %s",
				UnixError(ThisErrno));
	}
	C = tclose(queuem, 10*60, &TimedOut);	/* ten minute timeout on mail dropoff */
	if (TimedOut) {
		killpg(PGrp, SIGKILL);
		free(NewRetPathText); free(MsgHdr); unlink(fnBuff); free(FunkyDest); free(MsgDst);
		return AddrResult(tmltf_MailerSystemFailure, Addr, "queuemail timed out");
	}
	free(NewRetPathText);
	free(MsgDst);
	unlink(fnBuff);
	if (C == 0) {
		ThisWasSent(MsgHdr, FunkyDest, ForString, 1);
		free(MsgHdr); free(FunkyDest);
		return AddrResult(tmok_DroppedOffToList, Addr,
				"Replaced delivery parameters");
	}
	free(FunkyDest); free(MsgHdr);
	if ((C & 0xff) != 0) {
		return AddrResult(tmltf_MailerSystemFailure, Addr,
			"queuemail got signal: %s", UnixSignal(C & 0x7f));
	} else if (C == (EX_TEMPFAIL << 8) || C == (1 << 8)) {
		return AddrResult(tmltf_MailerTempFailure, Addr, "temporary failure");
	} else {
		return AddrResult(tmlpf_MailerPermFailure, Addr,
			"queuemail returned: %s", UnixSysExits(C >> 8));
	}
}

static int ResolveTilde(Addr, PrimePtr, Dom)
PARSED_ADDRESS *Addr;
char **PrimePtr; char *Dom;
{/* Try to resolve a leading tilde in the current +dist+ or +dir-insert+ argument.  If all is well, rewrite the laPrime result and return 0.  If anything else comes up, issue an error message and return non-0. */
	char *OldPrime = *PrimePtr;
	char *NewPrime, *PrPtr, *HdPtr;
	int rErr;
	wp_ErrorCode wpErr;

	if (*OldPrime++ != '~') return 0;
	if (*OldPrime == '/') {	/* was ``~/'' form; we don't know who he was. */
		return PermMessage(tmlpf_AddresseeUnknown, Addr, "No userid given",
			"The address ``%s'' contains the filename form ``~/'', which only makes sense while you are logged in.", Addr->LocalPart);
	}
	PrPtr = index(OldPrime, '/');
	if (PrPtr == NULL) return 0;	/* not our problem */
	*PrPtr = '\0';	/* temporarily */
	wpErr = UserIDtoHD(OldPrime, &HdPtr, Dom);
/* Now--did we find the userid? */
	switch (wpErr) {
	case wperr_NoError:
		break;
	case wperr_NoKeysFound:
		rErr = PermMessage(tmlpf_AddresseeUnknown, Addr, "No such userid for file name",
			"There is no userid ``%s'' for address ``%s''.", OldPrime, Addr->LocalPart);
		*PrPtr = '/';	/* restore the '/' */
		return rErr;
	case wperr_TooManyKeysFound:
		rErr = PermMessage(tmlpf_AddresseeAmbiguous, Addr, "Multiple userids for file name",
			"There are multiple userids ``%s'' for address ``%s''.", OldPrime, Addr->LocalPart);
		*PrPtr = '/';	/* restore the '/' */
		return rErr;
	default:
		rErr = AddrResult(tmltf_WhitePagesRunFailure, Addr,
			"Cannot resolve userid ``%s'': %s.",
			OldPrime, wp_ErrorString(wpErr));
		*PrPtr = '/';	/* restore the '/' */
		return rErr;
	}
	*PrPtr++ = '/';	/* restore the '/' and bump PrPtr */
	NewPrime = malloc(strlen(HdPtr) + strlen(PrPtr) + 2);
	if (NewPrime == NULL) {
		return AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
	}
	/* Got the storage.  Set it properly and install it in place. */
	sprintf(NewPrime, "%s/%s", HdPtr, PrPtr);
	free(*PrimePtr);
	*PrimePtr = NewPrime;
	return 0;
}

static int DoDistListKind(Addr, ForString, PKey, Fwd, Dom)
PARSED_ADDRESS *Addr;
char *ForString, *Dom;
wp_PrimeKey PKey;
struct FwdLink *Fwd;
{	/* Local mail delivery for this address is indirect via a file--fetch it and use it. */
	wp_ErrorCode wpErr;
	char *Val, *FileName;
	int Res;

	wpErr = GetWPField(PKey, idxDP, &Val);
	if (wpErr != wperr_NoError) {
		return AddrResult(tmltf_WhitePagesRunFailure, Addr,
				"Can't find DP field for dist list: %s",
				wp_ErrorString(wpErr));
	}
	Res = ResolveTilde(Addr, &Val, Dom);
	if (Res != 0) {free(Val); return Res;}
	FileName = Val;
	while (isspace(*FileName)) ++FileName;
	for (Res = strlen(FileName) - 1; Res >= 0; Res--) {
		if (isspace(FileName[Res]))
			FileName[Res] = '\0';
		else
			break;
	}
	if (strlen(FileName) == 0 || *FileName != '/') {
		Res = PermMessage(tmlpf_ParameterError, Addr,
		    "Bad distribution list file name",
		    "The file named ``%s'' is to contain distribution information, yet that name is syntactically unusable.",
		    FileName);
		free(Val);
		return Res;
	}
	Res = HandleFileList(Addr, FileName, ForString, Fwd, PKey, Dom);
	free(Val);
	return Res;
}

static int DoOtherKind(Addr, ForString, PKey, Fwd, Dom)
PARSED_ADDRESS *Addr;
char *ForString, *Dom;
wp_PrimeKey PKey;
struct FwdLink *Fwd;
{/* This guy has no electronic mail box; suggest an alternate path for contact. */
	wp_ErrorCode wpErr;
	char *Val;


	wpErr = cwp_Read(wpCD, PKey, idxDP, &Val);
	if (wpErr != wperr_NoError && wpErr != wperr_NoSuchField) {
		return AddrResult(tmltf_WhitePagesRunFailure, Addr,
				"Can't find DP field: %s",
				wp_ErrorString(wpErr));
	}
	if (ULstrcmp(Dom, WorkstationCell) != 0) {
	    ThisIsRemote(Addr, Fwd, Fwd);
	    return tmltf_ForwardedOutOfAuth;
	} else {
	    return (PermMessage(tmlpf_NoMailbox, Addr, "No electronic mailbox",
			    "The addressee has no electronic mailbox%s.",
			    (wpErr == wperr_NoSuchField ||
			     strcmp(Val, "NONE") == 0) ?
			      " and has no forwarding address" : Val));
	}
}

static int DoMailKind(Addr, WName, MBox, ForString, InLoop, PKey, Val, Fwd, Dom, Hdr)
PARSED_ADDRESS *Addr;
char *WName, *MBox, *ForString, *Dom, *Hdr;
int InLoop;
wp_PrimeKey PKey;
char *Val;
struct FwdLink *Fwd;
{	/* Local mail delivery to address Addr is non-standard; the kind is given by the name Val.  We expect that this is a hook for expansion. */
	enum DKinds {kindUnknown, kindNNTP, kindDistList, kindPgmFmt, kindPgmStrip, kindOther, kindPaper};
	static struct {
		enum DKinds DKind;
		char *KName;
	} KindTable[] = {
		{kindNNTP,	"NNTP"},
		{kindDistList,	"DIST"},
		{kindPgmFmt,	"PGMFMT"},
		{kindPgmStrip,	"PGMSTRIP"},
		{kindOther,	"OTHER"},
	};
#define NumKinds (sizeof(KindTable) / sizeof(KindTable[0]))
	int Kind;
	enum DKinds ThisKind;

	ThisKind = kindUnknown;
	for (Kind = 0; Kind < NumKinds; Kind++) {
		if (ULstrcmp(Val, KindTable[Kind].KName) == 0)
			{ThisKind = KindTable[Kind].DKind; break;}
	}
	switch (ThisKind) {
    case kindNNTP:
		return DoNNTP(Addr, WName, MBox, ForString, InLoop, PKey);
    case kindDistList:
		if (InLoop) return -1;
		return DoDistListKind(Addr, ForString, PKey, Fwd, Dom);
    case kindPgmFmt:
    case kindPgmStrip:
		return DoProg(Addr, Hdr, ForString, PKey, Fwd, Dom, (ThisKind == kindPgmFmt));
    case kindOther:
		if (InLoop) return -1;
		return DoOtherKind(Addr, ForString, PKey, Fwd, Dom);
    case kindUnknown:
    default:
		if (InLoop) return -1;
		if (ULstrcmp(Dom, WorkstationCell) == 0) {
		    return PermMessage(tmlpf_NoSuchDelKind, Addr, "Unrecognized delivery kind",
			"Internal error: local delivery kind ``%s'' not recognized.", Val);
		} else {
		    ThisIsRemote(Addr, Fwd, Fwd);
		    return tmltf_ForwardedOutOfAuth;
		}
	}
}

static int DoLocalDelivery(Addr, Hdr, WName, MBox, ForString, InLoop, PKey, Fwd, Dom, MBName)
PARSED_ADDRESS *Addr;
char *Hdr, *WName, *MBox, *ForString, *Dom, *MBName;
int InLoop;
wp_PrimeKey PKey;
struct FwdLink *Fwd;
{
	wp_ErrorCode wpErr;
	char *Val;
	int Res;

	if (PKey == NULL)
		wpErr = wperr_NoSuchField;
	else
		wpErr = GetWPField(PKey, idxDK, &Val);
	if (wpErr == wperr_NoSuchField) {
		return WriteLocalMail(Addr, Hdr, WName, MBox, ForString,
					InLoop, Fwd, Dom, MBName);
	} else if (wpErr != wperr_NoError) {
		return AddrResult(tmltf_WhitePagesRunFailure, Addr,
			"Problem finding DK field: %s", wp_ErrorString(wpErr));
	} else {
		Res = DoMailKind(Addr, WName, MBox, ForString, InLoop,
				PKey, Val, Fwd, Dom, Hdr);
		free(Val);
		return Res;
	}
}

static int LocalMail(Addr, Hdr, WName, MBox, Fwd, PKey, Dom, MBName)
PARSED_ADDRESS *Addr;
char *Hdr, *WName, *MBox, *Dom, *MBName;
struct FwdLink *Fwd;
wp_PrimeKey PKey;
{ /* Set up the FOR string and call TryLocalDelivery to do direct delivery, handling all errors.
*/
	char *ForString;
	char *Prime, *Second;
	int Typ, UseLoc;

#if Logs
	Log(270, "LocalMail called");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "Trying local mail to name ``%s''.\n", Addr->LocalPart);
	UseLoc = FALSE;
	if (la_KindDomain(Addr, &Typ, &Prime, &Second, Dom) == laerr_NoError) {
		if (Typ == latype_LocalID && *Second != '\0') UseLoc = TRUE;
		if (Prime != NULL) free(Prime);
	}

#if Logs
	    Log(271, "LocalMail calls MakeFORString");
#endif /* Logs */
	if (MakeFORString(Fwd, (UseLoc ? Addr->LocalPart : NULL), Dom, &ForString) < 0) {
	    return AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
	}
#if Logs
	Log(272, "LocalMail about to call DoLocalDelivery");
#endif /* Logs */
	Typ = DoLocalDelivery(Addr, Hdr, WName, MBox, ForString, 0, PKey, Fwd, Dom, MBName);
#if Logs
	Log(273, "DoLocalDelivery returned to LocalMail");
#endif /* Logs */
	if (ForString != NULL) free(ForString);
	if (TMDebug) fprintf(stderr, "LocalMail returns %d.\n", Typ);
	return Typ;
}

/* Here's some of the design for what follows.  When HandleForwarding is called, it stacks a Fwd frame on the list, and in this frame is one of the lists of addresses for non-local delivery (queued there by HandleRecipient).  Thus, when HandleForwarding exits, it needs to take those queued addresses and call AppendRemote for it as well as listing it in RemoteDelivered.  What needs to stand in the way of this action is when we're doing forwarding-loop delivery and delivery to one of the local addresses fails.  In this case (which usually happens by a local person forwarding to their local mailbox and to an off-site address), we suppress the delivery of the non-local addresses.  The reason for this is that we are about to requeue the mail to be delivered to the local address later, and we expect that, otherwise, every time we retry the local mail we'll generate a copy to the non-local addresses also.  Thus, we scuttle the delivery of the stacked addresses.
 */

void ThisIsRemote(Addr, fromFwd, toFwd)
PARSED_ADDRESS *Addr;
struct FwdLink *fromFwd, *toFwd;
{ /* Append this address to the correct list, specified by toFwd.  How this address got here is specified by fromFwd.  They're different only when we're moving the queued addresses further down in the stack.
 */
	PARSED_ADDRESS *DestList;

	if (IsItOnList(Addr, RemoteDelivered, FALSE)) {
		if (TMDebug) fprintf(stderr, "HandleRcpt: %s already rmt delivered.\n",
					Addr->LocalPart);
		return;
	}
	DestList = RemoteDelivered;
	if (toFwd != NULL) {
		if (IsItOnList(Addr, toFwd->RmtFwded, FALSE)) {
			if (TMDebug) fprintf(stderr, "HandleRcpt: %s already rmt queued.\n",
					Addr->LocalPart);
			return;
		}
		DestList = toFwd->RmtFwded;
	}
	if (TMDebug) {
		fprintf(stderr,
		    "Rmt mail request, localpart %s, outer rmt domain %s:  queueing it on ",
		    Addr->LocalPart, Addr->Hosts->Prev->Name);
		if (toFwd == NULL)
			fprintf(stderr, "global list");
		else {
			fprintf(stderr, "list rooted with %s", toFwd->fAddr->LocalPart);
			if (toFwd->WholeName != NULL)
				fprintf(stderr, " (%s)", toFwd->WholeName);
		}
		fprintf(stderr, ".\n");
	}
	if (TMDebug) fprintf(stderr, "Addressed to remote domain %s.\n",
				Addr->Hosts->Prev->Name);
	AppendAddrTo(Addr, DestList);
	if (toFwd == NULL) {
		if (AppendRemote(Addr, fromFwd) != 0)
			AddrResult(tmltf_NonLocalHost, Addr, "non-local host");
	}
}

static void ForwardLoop(Addr, Hdr, WName, MBox, Fwd, MBName)
PARSED_ADDRESS *Addr;
char *Hdr, *WName, *MBox, *MBName;
struct FwdLink *Fwd;
{ /* Called after a forwarding loop has been detected.  Deliver mail to all addressees on the loop, after having set up the FOR string so that the loop is evident.
*/
	char *ForString, *NewDom;
	struct FwdLink *FDum;
	int AnySucceeded, Scuttling, DeliveryValue;

#if Logs
	Log(260, "ForwardLoop called--about to make For: string");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "Trying forwarding-loop mail to name ``%s'' (%s).\n",
				Addr->LocalPart, Addr->Hosts->Prev->Name);

	if (MakeFORString(Fwd, Addr->LocalPart, Addr->Hosts->Prev->Name, &ForString) < 0) {
		AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
		return;
	}
#if Logs
	Log(261, "MakeFORString returns 0x%x to ForwardLoop", ForString);
#endif /* Logs */
	AnySucceeded = FALSE;
	Scuttling = FALSE;
	for (FDum = Fwd; FDum != NULL; FDum = FDum->Parent) {
		if (! IsItOnList(FDum->fAddr, LocalDelivered, TRUE)) {
			if (FDum->Rewritten) SafeAppendAddrTo(FDum->fAddr, LocalDelivered, Fwd);
			if (TMDebug) fprintf(stderr, "Doing loop delivery to %s@%s, for %s.\n",
					FDum->fAddr->LocalPart,
					FDum->fAddr->Hosts->Prev->Name, ForString);
			if (FDum->KeyVal != NULL && ! Scuttling) {
#if Logs
			    Log(262, "ForwardLoop calling DoLocalDelivery on %s",
					FDum->fAddr->LocalPart);
#endif /* Logs */
			    NewDom = NewString(FDum->fAddr->Hosts->Prev->Name);
			    if (NewDom == NULL) {
				DeliveryValue = AddrResult(tmltf_MemoryExhausted,
					FDum->fAddr, "Out of memory");
			    } else {
				DeliveryValue = DoLocalDelivery(FDum->fAddr, Hdr,
					WName, MBox, ForString, 1,
					FDum->KeyVal, Fwd, NewDom, MBName);
				free(NewDom);
			    }
#if Logs
			    Log(263, "DoLocalDelivery returns to ForwardLoop");
#endif /* Logs */
			    if (DeliveryValue >= 0) AnySucceeded = TRUE;
			    if ((DeliveryValue >= tm_AddrQueueLowest &&
				 DeliveryValue <= tm_AddrQueueHighest)
				&& DeliveryValue != tmltf_ForwardedOutOfAuth)
					Scuttling = TRUE;
			}
			if (Scuttling) FDum->Scuttled = TRUE;
		}
		if (ULstrcmp(Addr->Hosts->Prev->Name,
				FDum->fAddr->Hosts->Prev->Name) == 0
		    && IsSameLocalPart(Addr, FDum->fAddr->LocalPart)) break;
	}
	if (! AnySucceeded) {
	    if (ULstrcmp(GlobalDom, WorkstationCell) == 0) {
		PermMessage(tmlpf_NoMailboxInLoop, Addr,
			"No local address in forwarding loop has a mailbox.",
			"The local addresses form a forwarding loop:\t%s\nNone of the addresses has a mailbox to which mail can be delivered.",
			ForString);
	    } else {
		ThisIsRemote(Addr, Fwd, Fwd);
	    }
	}
	free(ForString);
	if (TMDebug) fprintf(stderr, "ForwardLoop returns.\n");
}

static void AppendRmtElements(AddrList, Fwd)
PARSED_ADDRESS *AddrList;
struct FwdLink *Fwd;
{ /* Append the remote addresses in AddrList for remote delivery. */

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:		  /* stick them on parent */
			ThisIsRemote(ThisAddr, Fwd, Fwd->Parent);
			break;
	case GROUP_ADDRESS:
			AppendRmtElements(ThisAddr->Members, Fwd);
			break;
	default:
			break;
		}
	})
}

/* fwd ref */
static void HandleAddresses();

static void HandleForwarding(Addr, WName, MBox, FwdText, Fwd, PKey, Type, Dom)
PARSED_ADDRESS *Addr;
char *WName, *MBox, *FwdText, *Dom;
struct FwdLink *Fwd;
wp_PrimeKey PKey;
enum FwdType Type;
{
	PARSED_ADDRESS *FwdAddr;
	int FwdCode;
	char *TypeName;
	struct FwdLink *NewFwd;

#if Logs
	Log(250, "Handling forwarding--about to parse");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "HandleForwarding(%s, %s, ``%s'') called.\n",
				Addr->LocalPart, Dom, FwdText);

	FwdCode = ParseFwdString(FwdText, &FwdAddr);
#if Logs
	Log(251, "ParseFwdString returned %d", FwdCode);
#endif /* Logs */
	if (FwdCode == PA_SYNTAX_ERROR) {
	    if (ULstrcmp(Dom, WorkstationCell) == 0) {
		switch (Type) {
		    case Forwarding:
			PermMessage(tmlpf_MailFwdSyntaxError, Addr,
			"Syntax error in forwarding address",
			"The mail forwarding address for local user ``%s'' is ``%s'', which is syntactically invalid and cannot be interpreted.",
			Addr->LocalPart, FwdText);
			break;
		    case DistList:
			PermMessage(tmlpf_MailFwdSyntaxError, Addr,
			"Syntax error in distribution list contents",
			"The destination addresses of distribution list ``%s'' are syntactically invalid and cannot be interpreted.",
			Addr->LocalPart);
			break;
		    case Group:
			PermMessage(tmlpf_MailFwdSyntaxError, Addr,
			"Syntax error in group contents",
			"The destination addresses generated from group ``%s'' are syntactically invalid and cannot be interpreted.",
			Addr->LocalPart);
			break;
		}
	    } else {
		ThisIsRemote(Addr, Fwd, Fwd);
	    }
	    return;
	}
	if (FwdCode != PA_OK) {
	    switch (Type) {
		case Forwarding:
		    TypeName = "forwarding address";
		    break;
		case DistList:
		    TypeName = "distribution list contents";
		    break;
		case Group:
		    TypeName = "group contents";
		    break;
		default:
		    TypeName = "unknown address type";
		    break;
	    }
	    AddrResult(tmltf_MailFwdBadParse, Addr, "Cannot parse %s; code is %d", TypeName, FwdCode);
	    return;
	}
#if Logs
	Log(252, "Building new FwdLink structure");
#endif /* Logs */
	NewFwd = (struct FwdLink *) malloc(sizeof(struct FwdLink));
	if (NewFwd == NULL) {
		FreeAddressList(FwdAddr);
		AddrResult(tmltf_MemoryExhausted, Addr, "No more memory");
		return;
	}
	NewFwd->RmtFwded = NewNullAddr();
	if (NewFwd->RmtFwded == NULL) {
		FreeAddressList(FwdAddr);
		free(NewFwd);
		AddrResult(tmltf_MemoryExhausted, Addr, "No memory left");
		return;
	}
	NewFwd->Parent = Fwd;
	NewFwd->WholeName = WName;
	NewFwd->MailBox = MBox;
	NewFwd->KeyVal = PKey;
	NewFwd->fAddr = Addr;
	NewFwd->Scuttled = FALSE;
	NewFwd->Rewritten = TRUE;
#if Logs
	Log(253, "HandleForwarding calling HandleAddresses on result addresses");
#endif /* Logs */
	HandleAddresses(FwdAddr, NewFwd, Dom);
#if Logs
	Log(254, "HandleAddresses returned to HandleForwarding.");
#endif /* Logs */
	if (! NewFwd->Scuttled) AppendRmtElements(NewFwd->RmtFwded, NewFwd);
	FreeAddressList(NewFwd->RmtFwded);
	free(NewFwd);
#if Logs
	Log(255, "HandleForwarding calling FreeAddressList");
#endif /* Logs */
	FreeAddressList(FwdAddr);
#if Logs
	Log(256, "FreeAddressList returning to HandleForwarding");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "HandleForwarding(%s, ``%s'') returning.\n",
				Addr->LocalPart, FwdText);
}

static int EnsureCellOnOne(Addr, CellToAdd, oldMD)
PARSED_ADDRESS *Addr;
char *CellToAdd; struct MailDom *oldMD;
{/* Worker for EnsureOuterCell routine. */

	if (Addr->Hosts == Addr->Hosts->Prev	/* no host? */
	  || (ULstrcmp(Addr->Hosts->Prev->Name, CellToAdd) != 0	/* non-ams-delivery? */
	      && CheckAMSDelivery(Addr->Hosts->Prev->Name) <= 0)) {
	    if (Addr->MD == NULL || Addr->MD->Qual != mailhost_good) {
		ADDRESS_HOST *HostPtr;
		extern ADDRESS_HOST *MakeHost();
		char *NewHost;

		NewHost = NewString(CellToAdd);
		if (NewHost == NULL) return 1;
		HostPtr = MakeHost(NewHost);
		if (HostPtr == NULL) {
			free(NewHost);
			return 1;
		} else {
			la_FreeMD(Addr->MD);
			Addr->MD = NULL;
			AddHost(Addr, HostPtr);
			if (oldMD != NULL && ULstrcmp(CellToAdd, oldMD->Final) == 0 && oldMD->Qual == mailhost_good) {
			    Addr->MD = oldMD;
			    ++(oldMD->Refs);
			}
		}
	    }
	}
	return 0;
}

static int EnsureOuterCell(AddrList, CellToAdd, oldMD)
PARSED_ADDRESS *AddrList; char *CellToAdd; struct MailDom *oldMD;
{/* Ensure that the outermost host of every address on AddrList is an AMS_DeliverySystem cell or a known-good domain; if it's not, add CellToAdd.  If that fails, return 1; if everything works, return 0. */
	int RetVal;

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:		  /* stick them on parent */
			RetVal = EnsureCellOnOne(ThisAddr, CellToAdd, oldMD);
			if (RetVal != 0) return RetVal;
			break;
	case GROUP_ADDRESS:
			RetVal = EnsureOuterCell(ThisAddr->Members, CellToAdd, oldMD);
			if (RetVal != 0) return RetVal;
			break;
	default:
			break;
		}
	})
	return 0;
}

static void CrossCell(Addr, Fwd, OldDom, NewDom)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
char *OldDom, *NewDom;
{
    struct FwdLink *NewFwd;
    ADDRESS_HOST *OuterHost;
    struct MailDom *oldMD;

    NewFwd = (struct FwdLink *) malloc(sizeof(struct FwdLink));
    if (NewFwd == NULL) {
	AddrResult(tmltf_MemoryExhausted, Addr, "No more memory");
	return;
    }
    NewFwd->RmtFwded = NewNullAddr();
    if (NewFwd->RmtFwded == NULL) {
	free(NewFwd);
	AddrResult(tmltf_MemoryExhausted, Addr, "No memory left");
	return;
    }
    /* The outermost host will be the same host as NewDom.  Remove it here (rather than in HandleRecipient) so we can add it back later if memory allocation fails later. */
    OuterHost = Addr->Hosts->Prev;
    RemHost(OuterHost);	/* unlink this host from the Addr structure */
    oldMD = Addr->MD;
    Addr->MD = NULL;

    /* Fill in the rest of the NewFwd structure. */
    NewFwd->Parent = Fwd;
    NewFwd->WholeName = NULL;
    NewFwd->MailBox = NULL;
    NewFwd->KeyVal = NULL;
    NewFwd->fAddr = Addr;
    NewFwd->Scuttled = FALSE;
    NewFwd->Rewritten = FALSE;
#if Logs
    Log(253, "CrossCell calling HandleRecipient on result addresses");
#endif /* Logs */
    HandleRecipient(Addr, NewFwd, NewDom);
#if Logs
    Log(254, "HandleRecipient returned to CrossCell.");
#endif /* Logs */
    if (! NewFwd->Scuttled) {
	if (EnsureOuterCell(NewFwd->RmtFwded, NewDom, oldMD) == 0) {	/* OK */
	    FreeHost(OuterHost);
	    la_FreeMD(oldMD);
	    AppendRmtElements(NewFwd->RmtFwded, NewFwd);
	} else {	/* Failure: scuttle-equivalent; recover top-level Addr and report it. */
	    AddHost(Addr, OuterHost);	/* No possibility of failure here. */
	    la_FreeMD(Addr->MD);	/* Toss the new one */
	    Addr->MD = oldMD;   /* and re-plant the old one. */
	    AddrResult(tmltf_MemoryExhausted, Addr, "No memory left");
	}
    }
    FreeAddressList(NewFwd->RmtFwded);
    free(NewFwd);
    if (TMDebug) fprintf(stderr, "CrossCell(%s, %s) returning.\n", Addr->LocalPart, NewDom);
}

static int IsAFwdLoop(Addr, Fwd, Hdr, WName, MBox, MBName)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
char *Hdr, *WName, *MBox, *MBName;
{ /* Check for a forwarding loop--return TRUE if there was one, FALSE otherwise. */
	struct FwdLink *FDum;

	for (FDum = Fwd; FDum != NULL; FDum = FDum->Parent) {
	    if (FDum->Rewritten) {
		if (ULstrcmp(Addr->Hosts->Prev->Name,
			FDum->fAddr->Hosts->Prev->Name) == 0
		  && IsSameLocalPart(Addr, FDum->fAddr->LocalPart)) {
			if (TMDebug) fprintf(stderr, "IsAFwdLoop: %s on fwd chain.\n",
						Addr->LocalPart);
			ForwardLoop(Addr, Hdr, WName, MBox, Fwd, MBName);
			if (! IsItOnList(Addr, LocalDelivered, FALSE)) {
				if (TMDebug) fprintf(stderr, "Adding loc %s to delvr'd list.\n",
						Addr->LocalPart);
				SafeAppendAddrTo(Addr, LocalDelivered, Fwd);
			}
			return TRUE;
		}
	    }
	}
	return FALSE;
}

static void HandleRecipient(Addr, Fwd, DefaultDomain)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
char *DefaultDomain;
{
    ADDRESS_HOST *HostPtr;
    char *CanonID, *ForString, *fwdPrime, *fwdSecond, *SDum, *Hdr, *homeDir, *FwdInfo, *WName, *QWName, *MBox, *NewName, *MBName;
    wp_SearchToken STok;
    wp_PrimeKey KVal;
    wp_ErrorCode ErrCode, Err2;
    int MinMatch, OutMatch, ReturnResult, NewLen, NameSep;
    extern ADDRESS_HOST *MakeHost();
    int laErr, laType, fwdErr, fwdType, badLocalMatch;
    char *laPrime, *laSecond;
    FILE *Adv;

    MBox = NULL;
#if Logs
    Log(200, "HandleRecipient called");
#endif /* Logs */
    if (TMDebug) fprintf(stderr, "HandleRecipient called, ln %s.\n", Addr->LocalPart);
    Hdr = NULL;

    laErr = la_KindDomain(Addr, &laType, &laPrime, &laSecond, DefaultDomain);
#if Logs
    Log(201, "Adding its canonical name back, if necessary");
#endif /* Logs */
    if(Addr->Hosts == Addr->Hosts->Next) {
	CanonID = NewString(DefaultDomain);
	if (CanonID == NULL) {
	    if (laPrime != NULL) free(laPrime);
	    AddrResult(tmltf_MemoryExhausted, Addr, "No memory storage");
	    return;
	}
	HostPtr = MakeHost(CanonID);
	if (HostPtr == NULL) {
	    free(CanonID);
	    if (laPrime != NULL) free(laPrime);
	    AddrResult(tmltf_MemoryExhausted, Addr, "No memory storage");
	    return;
	}
	CanonID = NULL;		/* ptr is in Host str now */
	if (AddHost(Addr, HostPtr) != PA_OK) {
	    if (laPrime != NULL) free(laPrime);
	    AddrResult(tmltf_MemoryExhausted, Addr, "AddHost failure");
	    return;
	}
	la_FreeMD(Addr->MD);
	Addr->MD = NULL;
    }
    if (laErr != laerr_NoError && ULstrcmp(DefaultDomain, WorkstationCell) != 0) {
	ThisIsRemote(Addr, Fwd, Fwd);
	return;
    }
    switch (laErr) {
	case laerr_NoError:
	    break;
	case laerr_UnrecSpecial:
	    PermMessage(tmlpf_LocalNameError, Addr,
			"Unrecognized special address type",
			"The text between initial plus signs in the destination address does not name a valid special address type.");
	    return;
	case laerr_SyntaxError:
	    PermMessage(tmlpf_LocalNameError, Addr, "Syntax error in local address",
			"The local destination address is syntactically invalid.");
	    return;
	case laerr_BadSecond:
	    PermMessage(tmlpf_LocalNameError, Addr, "Blank space after plus or hash",
			"The local destination address has blank characters or newlines embedded in the part after the plus sign or hash mark, and is thus invalid.");
	    return;
	default:
	    AddrResult(tmltf_MemoryExhausted, Addr, la_ErrorString(laErr));
	    return;
    }
    if (laType == latype_Remote) {	/* Directed to a non-local host. */
	if (laPrime != NULL) free(laPrime);
	if (Addr->MD != NULL && Addr->MD->Final != NULL
	    && ULstrcmp(Addr->Hosts->Prev->Name, Addr->MD->Final) != 0) {
	    char *BetterHost = NewString(Addr->MD->Final);
	    if (BetterHost != NULL) {
		free(Addr->Hosts->Prev->Name);
		Addr->Hosts->Prev->Name = BetterHost;
	    }
	}
	if (ULstrcmp(Addr->Hosts->Prev->Name, DefaultDomain) != 0) {
	    ErrCode = CheckAMSDelivery(Addr->Hosts->Prev->Name);
	    if (ErrCode > 0) {
		char *HostCopy = NewString(Addr->Hosts->Prev->Name);
		if (HostCopy != NULL) {	/* Recurse! */
		    CrossCell(Addr, Fwd, DefaultDomain, HostCopy);
		    free(HostCopy);
		    return;
		}
	    }
	}
	ThisIsRemote(Addr, Fwd, Fwd);
	return;
    }

    ReturnResult = GetHeader(&Hdr);	/* Malloc a copy of the header */
    if (ReturnResult != 0) {
	if (laPrime != NULL) free(laPrime);
	AddrResult(tmltf_MemoryExhausted, Addr,
		   "can't read header: %s",
		   (ReturnResult > 0 ? UnixError(ReturnResult) : "no memory"));
	return;
    }

    /* If all the hosts were just removed, we're local; add back the preferred name of our host. */
#if Logs
    Log(203, "To local domain");
#endif /* Logs */
    errno = 0;
    MBName = CheckAMSMBName(DefaultDomain);
    if (MBName == NULL) {
	if (errno == 0 || tfail(errno)) {
	    AddrResult((errno == 0 || errno == ENOMEM ? tmltf_MemoryExhausted : tmltf_FileUnreachable), Addr, "Can't get mailbox name for domain %s: %s", DefaultDomain, UnixError(errno));
	} else {
	    if (ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
		PermMessage(tmlpf_ParameterError, Addr, "No Mailbox name", "Domain ``%s'' does not publish the name of its users' mailboxes: %s.", DefaultDomain, UnixError(errno));
	    } else {
		ThisIsRemote(Addr, Fwd, Fwd);
	    }
	}
	if (Hdr != NULL) free(Hdr);
	if (laPrime != NULL) free(laPrime);
	return;
    }
    /* Now see what kind of local address it is. */
    switch (laType) {
	case latype_LocalID:
	case latype_LocalName:
	    break;		/* fall through */
	case latype_DistList:
	    if (laPrime == NULL) {
		if (Hdr != NULL) free(Hdr);
		PermMessage(tmlpf_ParameterError, Addr, "No distribution list file name",
			    "No file was named to contain the distribution list.");
		return;
	    }
	    if (ResolveTilde(Addr, &laPrime, DefaultDomain) != 0) {free(laPrime); return;}
	    if (strlen(laPrime) < 2 || *laPrime != '/') {
		PermMessage(tmlpf_ParameterError, Addr, "Bad distribution list file name",
			    "The file named ``%s'' is to contain distribution information, yet that name is syntactically unusable.",
			    laPrime);
		if (Hdr != NULL) free(Hdr);
		free(laPrime);
		return;
	    }
	    /* Check to see if this local addressee has been delivered to. */
	    if (IsItOnList(Addr, LocalDelivered, TRUE)) {	/* yes */
		if (TMDebug) fprintf(stderr, "HandleRcpt finds dist list %s already delivered.\n", Addr->LocalPart);
		if (Hdr != NULL) free(Hdr);
		free(laPrime); return;
	    }
	    if (IsAFwdLoop(Addr, Fwd, Hdr, NULL, NULL, MBName))
	    {free(laPrime); if (Hdr != NULL) free(Hdr); return;}
	    if (MakeFORString(Fwd, Addr->LocalPart, DefaultDomain, &ForString) < 0) {
		AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
		free(laPrime);
		if (Hdr != NULL) free(Hdr);
		return;
	    }
	    HandleFileList(Addr, laPrime, ForString, Fwd, NULL, DefaultDomain); /* no local mailbox ever */
	    if (! IsItOnList(Addr, LocalDelivered, TRUE) && (Fwd == NULL ? TRUE : Fwd->Rewritten)) SafeAppendAddrTo(Addr, LocalDelivered, Fwd);
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    return;
	case latype_DirInsert:
	    if (laPrime == NULL) {
		if (Hdr != NULL) free(Hdr);
		PermMessage(tmlpf_ParameterError, Addr, "No directory name",
			    "No directory was named into which mail is to be inserted.");
		return;
	    }
	    if (ResolveTilde(Addr, &laPrime, DefaultDomain) != 0) {free(laPrime); return;}
	    if (strlen(laPrime) < 2 || *laPrime != '/') {
		if (Hdr != NULL) free(Hdr);
		PermMessage(tmlpf_ParameterError, Addr, "Bad directory name",
			    "Mail is to be inserted into the directory named ``%s'', yet that name is syntactically unusable.",
			    laPrime);
		free(laPrime);
		return;
	    }
	    /* Check to see if this local addressee has been delivered to. */
	    if (IsItOnList(Addr, LocalDelivered, TRUE)) {	/* yes */
		if (TMDebug) fprintf(stderr,
				     "HandleRcpt finds dir-insertion %s already done.\n",
				     Addr->LocalPart);
		if (Hdr != NULL) free(Hdr);
		free(laPrime); return;
	    }
	    if (IsAFwdLoop(Addr, Fwd, Hdr, NULL, NULL, MBName))
	    {free(laPrime); if (Hdr != NULL) free(Hdr); return;}
	    if (MakeFORString(Fwd, Addr->LocalPart, DefaultDomain, &ForString) < 0) {
		AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
		free(laPrime);
		if (Hdr != NULL) free(Hdr);
		return;
	    }
	    /* it's always a local mailbox */
	    WriteLocalMail(Addr, Hdr, NULL, laPrime, ForString, 0, Fwd, DefaultDomain, MBName);
	    if (! IsItOnList(Addr, LocalDelivered, TRUE) && (Fwd == NULL ? TRUE : Fwd->Rewritten)) SafeAppendAddrTo(Addr, LocalDelivered, Fwd);
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    return;
	case latype_FSMembers:
	    if (laPrime == NULL) {
		if (Hdr != NULL) free(Hdr);
		PermMessage(tmlpf_ParameterError, Addr, "No FS group name",
			    "No FS group was named to which to send mail.");
		return;
	    }
	    /* Check to see if this local addressee has been delivered to. */
	    if (IsItOnList(Addr, LocalDelivered, TRUE)) {	/* yes */
		if (TMDebug) fprintf(stderr, "HandleRcpt finds dist list %s already delivered.\n", Addr->LocalPart);
		if (Hdr != NULL) free(Hdr);
		free(laPrime); return;
	    }
	    if (IsAFwdLoop(Addr, Fwd, Hdr, NULL, NULL, MBName))
	    {free(laPrime); if (Hdr != NULL) free(Hdr); return;}
	    if (MakeFORString(Fwd, Addr->LocalPart, DefaultDomain, &ForString) < 0) {
		AddrResult(tmltf_MemoryExhausted, Addr, "Out of memory");
		free(laPrime);
		if (Hdr != NULL) free(Hdr);
		return;
	    }
	    HandleFSMembers(Addr, laPrime, ForString, Fwd, NULL, DefaultDomain); /* no local mailbox ever */
	    if (! IsItOnList(Addr, LocalDelivered, TRUE) && (Fwd == NULL ? TRUE : Fwd->Rewritten)) SafeAppendAddrTo(Addr, LocalDelivered, Fwd);
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    return;
	default:
	    AddrResult(tmlpf_LocalNameError, Addr,
		       "Unrecognized local address type: %d", laType);
	    if (laPrime != NULL) free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    return;
    }
    if (laPrime == NULL) {
	AddrResult(tmlpf_ParameterError, Addr, "Null address");
	if (Hdr != NULL) free(Hdr);
	return;
    }

    /* It's a local name, either latype_LocalID or latype_LocalName--try to evaluate it. */
    OpenWP(DefaultDomain);
    if (WpOpen == WpOpen_CannotOpen) {
	if (laPrime != NULL) free(laPrime);
	if (Hdr != NULL) free(Hdr);
	AddrResult(tmltf_WhitePagesInaccessible, Addr, wp_ErrorString(WpError));
	return;
    }
    if (TMDebug) fprintf(stderr, "raw=%s, laType=%d, prime=%s, second=%s\n",
			  Addr->LocalPart, laType, laPrime, (laSecond == NULL ? "NULL" : laSecond));
    STok = NULL;
    ErrCode = wp_SetUp(laPrime,
			(laType == latype_LocalID ?
			 LookupUIDOnly :
			 LookupUIDWithLastPart),
			&STok);
    if (ErrCode != wperr_NoError) {
	free(laPrime);
	if (Hdr != NULL) free(Hdr);
	AddrResult(tmltf_WhitePagesRunFailure, Addr, wp_ErrorString(ErrCode));
	return;
    }
#if Logs
    Log(212, "wp_SetUp returns %d--about to call wp_Lookup", ErrCode);
#endif /* Logs */
    KVal = NULL;
    ErrCode = cwp_Lookup(wpCD, STok, &MinMatch, MatchAll, &OutMatch, &KVal);
#if Logs
    Log(213, "cwp_Lookup returns %d", ErrCode);
#endif /* Logs */
    if (TMDebug) fprintf(stderr, "HandleRecipient: Lookup returns %d (%s).\n",
			  ErrCode, wp_ErrorString(ErrCode));
/* Use the Network Preferred Attribute when appropriate. */
    if (!gotWPIndices) {
	if (!GetWPIndices(Addr)) {
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    (void) wp_DeAllocate(STok);
	    if (KVal != NULL) free(KVal);
	    return;
	}
    }
    if (ErrCode == wperr_TooManyKeysFound && idxNPA >= 0 && AuthString != NULL) {
	int AuthFrom, outQuality, Ix, NPAix, NPAmax, thisNPA, numThisNPA; wp_ErrorCode wperr; char *NPA; wp_PrimeKeySet *KS;
	numThisNPA = 0;
	AuthFrom = atoi(AuthString);
	if (AuthFrom == 0) {
	    KS = NULL;
	    wperr = cwp_Search(wpCD, STok, -1, MatchIDNamePart, &outQuality, &KS);
	    if (wperr == wperr_NoError && KS != NULL) {
		NPAmax = NPAix = -100000;
		for (Ix = 0; Ix < KS->KeyCount; ++Ix) {
		    wperr = cwp_Read(wpCD, KS->Keys[Ix], idxNPA, &NPA);
		    if (wperr == wperr_NoError) {
			thisNPA = atoi(NPA);
			if (thisNPA == NPAmax) ++numThisNPA;
			else if (thisNPA > NPAmax) {
			    thisNPA = NPAmax;
			    NPAix = Ix;
			    numThisNPA = 1;
			}
		    }
		}
		if (NPAix >= 0 && numThisNPA == 1) {
/* Found a unique maximum-NPA entry!  Use it--fake the cwp_Lookup result. */
		    ErrCode = wperr_NoError;
		    OutMatch = outQuality;
		    if (KVal != NULL) free(KVal);
		    KVal = KS->Keys[NPAix];
		    KS->Keys[NPAix] = NULL;
		}
	    }
	    if (KS != NULL) wp_DeAllocate(KS);
	}
    }
#if Logs
    Log(220, "Entering case on wp_Lookup result");
#endif /* Logs */
    switch (ErrCode) {
	case wperr_NoError:
	    break;	/* fall through */
	case wperr_NoKeysFound:
	    if (ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
		PermMessage(tmlpf_AddresseeUnknown, Addr, "No such addressee",
			"There is no such addressee as ``%s'' in domain %s.",
			Addr->LocalPart, DefaultDomain);
	    } else {
		ThisIsRemote(Addr, Fwd, Fwd);
	    }
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    (void) wp_DeAllocate(STok);
	    return;
	case wperr_TooManyKeysFound:
	    if (Hdr != NULL) free(Hdr);
	    if (ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
		ReturnResult = ReturnAmbig(Addr, wpCD, STok, 0, MinMatch, OutMatch, DefaultDomain);
		if (ReturnResult == 0)
		    AddrResult(tmok_ErrorMsgReturned, Addr, "Ambiguous addressee (error message sent OK)");
		else if (ReturnResult == 1 && ! ReturnTempFail)	/* we are trymail */
		    AddrResult(tmlpf_AddresseeAmbiguous, Addr, "Addressee ambiguous");
		else if (ReturnTempFail)
		    AddrResult(tmltf_ErrorReturningMsg, Addr, "Ambiguous addressee (error %d sending message)", ReturnResult);
		else
		    AddrResult(tmlpf_AddresseeAmbiguous, Addr, "Addressee ambiguous--return-error value %d", ReturnResult);
	    } else {
		ThisIsRemote(Addr, Fwd, Fwd);
	    }
	    free(laPrime);
	    (void) wp_DeAllocate(STok);
	    return;
	default:
	    AddrResult(tmltf_WhitePagesRunFailure, Addr, wp_ErrorString(ErrCode));
	    free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    (void) wp_DeAllocate(STok);
	    return;
    }
    /* Can be ID match, or exact surname match with any amount of fuzziness in given name */
    /* This used to be the equivalent of: badLocalMatch = (OutMatch > MatchExPA); */
    badLocalMatch = (OutMatch > MatchExPA || OutMatch == MatchAbPh || OutMatch == MatchAbOv || (OutMatch > MatchFirstNameAbbrev && OutMatch <= MatchNoHeuristics));
    if (OutMatch > MatchNoHeuristics || badLocalMatch) {
	Adv = OpenPMAdvisory(
			     (badLocalMatch ? "Undeliverable heuristic" : "Deliverable heuristic"),
			     0, Addr, DefaultDomain);
	if (Adv != NULL) {
	    auto char BF[1000];
	    sprintf(BF, "White pages match was %d: ", OutMatch);
	    if (badLocalMatch) {
		strcat(BF, "too fuzzy for delivery");
	    } else {
		strcat(BF, "delivered anyway");
	    }
	    if (!gotWPIndices) WantWPIndices();
	    if (gotWPIndices) {
		Err2 = GetWPField(KVal, idxN, &SDum);
		if (Err2 == wperr_NoError) {
		    strcat(BF, " to ");
		    strcat(BF, SDum);
		    Err2 = GetWPField(KVal, idxID, &SDum);
		    if (Err2 == wperr_NoError) {
			strcat(BF, " (");
			strcat(BF, SDum);
			strcat(BF, "+)");
		    }
		}
	    }
	    strcat(BF, ".\n\n");
	    PMAdvPrint("%s", BF);
	    ClosePMAdvisory();
	}
    }
    if (badLocalMatch) {
	if (ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
	    ReturnResult = ReturnFuzzy(Addr, wpCD, STok, KVal, 0, MinMatch, OutMatch, DefaultDomain);
	    free(KVal);
	    if (Hdr != NULL) free(Hdr);
	    if (ReturnResult == 0)
		AddrResult(tmok_ErrorMsgReturned, Addr, "Fuzzily-matched addressee (error message sent OK)");
	    else if (ReturnResult == 1 && ! ReturnTempFail)	/* we are trymail */
		AddrResult(tmlpf_AddresseeFuzzy, Addr, "Addressee fuzzily-matched");
	    else if (ReturnTempFail)
		AddrResult(tmltf_ErrorReturningMsg, Addr, "Fuzzily-matched addressee (error %d sending message)", ReturnResult);
	    else
		AddrResult(tmlpf_AddresseeFuzzy, Addr, "Addressee matched fuzzily--return-error value %d", ReturnResult);
	} else {
	    ThisIsRemote(Addr, Fwd, Fwd);
	}
	free(laPrime);
	(void) wp_DeAllocate(STok);
	return;
    }
    (void) wp_DeAllocate(STok);
    /* OK, the string in KVal is the prime key for the unique local addressee. */
    if (! gotWPIndices) {
	if (! GetWPIndices(Addr)) {
	    free(KVal); free(laPrime);
	    if (Hdr != NULL) free(Hdr);
	    return;
	}
    }

    ErrCode = GetWPField(KVal, idxID, &CanonID);
    if (ErrCode == wperr_NoSuchField) {
	CanonID = 0;
    }
    else if (ErrCode != wperr_NoError) {
	AddrResult(tmltf_CannotFindRequiredField, Addr, "Cannot find required field");
	free(KVal); free(laPrime);
	if (Hdr != NULL) free(Hdr);
	return;
    }

    NameSep = CheckAMSNameSep(DefaultDomain);
    if (NameSep == 0) {
	AddrResult(tmltf_ViceInaccessible, Addr,
		   "Canon form of names in cell %s unknown: %s",
		   DefaultDomain, UnixError(errno));
	free(KVal); free(laPrime); if (Hdr != NULL) free(Hdr); return;
    }
    if (!CanonID && NameSep <= 0) NameSep = '_';
    FwdInfo = fwdPrime = fwdSecond = NULL;
    if (Fwd != NULL) {
	SDum = Addr->LocalPart;
	Addr->LocalPart = Fwd->fAddr->LocalPart;
	fwdErr = la_KindDomain(Addr, &fwdType,
			       &fwdPrime, &fwdSecond, DefaultDomain);
	Addr->LocalPart = SDum;
	if (fwdErr == laerr_NoError && fwdType == latype_LocalID)
	    FwdInfo = fwdSecond;
    }
    if (laType == latype_LocalID && *laSecond != '\0') NewLen = strlen(laSecond);
    else if (FwdInfo != NULL) NewLen = strlen(FwdInfo);
    else NewLen = 0;
    QWName = NULL;
    ErrCode = GetWPField(KVal, idxN, &WName);
    if (ErrCode == wperr_NoSuchField) WName = NULL;
    else if (ErrCode != wperr_NoError) {
	AddrResult(tmltf_MemoryExhausted, Addr, wp_ErrorString(ErrCode));
	free(KVal); free(laPrime); if (Hdr != NULL) free(Hdr); return;
    }
    if (NewLen == 0 && NameSep > 0 && WName != NULL) {
	char *Src;
	QWName = NewString(WName);
	if (QWName == NULL) {
	    free(WName);
	    AddrResult(tmltf_MemoryExhausted, Addr, "No more memory");
	    free(KVal); free(laPrime); if (Hdr != NULL) free(Hdr); return;
	}
	for (Src = QWName; *Src != '\0'; ++Src) if (*Src == ' ') *Src = NameSep;
	ErrCode = AddressMatchesUnambiguously(wpCD, QWName, KVal);
	if (ErrCode == wperr_TooManyKeysFound) {
	    free(QWName);
	    free(WName);
	    WName = QWName = NULL;
	}
	else if (ErrCode != wperr_NoError) {
	    AddrResult(tmltf_WhitePagesRunFailure, Addr, wp_ErrorString(ErrCode));
	    free(QWName); free(WName); free(KVal); free(laPrime);
	    if (Hdr != NULL) free(Hdr); return;
	}
    }
    if (NewLen == 0 && NameSep > 0 && WName != NULL) NewLen = strlen(QWName) + 1;
    else if (CanonID) NewLen += strlen(CanonID) + 2;
    else NewLen = strlen(Addr->LocalPart);
    NewName = malloc(NewLen);
    if (NewName == NULL) {
	AddrResult(tmltf_MemoryExhausted, Addr, "No memory storage");
	if (KVal != NULL) free(KVal);
	if (fwdPrime != NULL) free(fwdPrime);
	if (WName != NULL) free(WName);
	if (QWName != NULL) free(QWName);
	if (Hdr != NULL) free(Hdr);
	if (CanonID) free(CanonID);
	free(laPrime); return;
    }
    if (CanonID && laType == latype_LocalID && *laSecond != '\0') {
	sprintf(NewName, "%s+%s", CanonID, laSecond);
    } else if (CanonID && FwdInfo != NULL) {
	sprintf(NewName, "%s+%s", CanonID, FwdInfo);
    } else if (QWName != NULL) {
	strcpy(NewName, QWName);
    } else if (CanonID) {
	sprintf(NewName, "%s+", CanonID);
    } else {
	strcpy(NewName, Addr->LocalPart);
    }
    if (Addr->LocalPart != NULL) free(Addr->LocalPart);
    Addr->LocalPart = NewName;
    if (fwdPrime != NULL) free(fwdPrime);
    if (QWName != NULL) free(QWName);
    free(laPrime);
    if (CanonID) free(CanonID);
    CanonID = NULL;		/* LocalPart overwritten with Unix username/ID */
#if Logs
    Log(232, "Got the new local part--about to check for duplication");
#endif /* Logs */
    if (TMDebug) fprintf(stderr, "HandleRecipient checks %s for duplication.\n",
			  Addr->LocalPart);
    /* Check to see if this local addressee has been delivered to. */
    if (IsItOnList(Addr, LocalDelivered, TRUE)) {	/* yes */
	if (TMDebug) fprintf(stderr, "HandleRcpt finds loc %s already delivered.\n",
			     Addr->LocalPart);
	if (WName != NULL) free(WName);
	if (KVal != NULL) free(KVal);
	if (Hdr != NULL) free(Hdr);
	return;
    }
#if Logs
    Log(233, "Not a duplicate--going to get full-name and mailbox fields.");
#endif /* Logs */
    /* Fetch full name and mailbox info */
    ErrCode = cwp_Read(wpCD, KVal, idxHD, &QWName);
    if (ErrCode == wperr_NoError) {
	MBox = malloc(strlen(QWName) + strlen(MBName) + 2);
	if (MBox == NULL) ErrCode = wperr_OutOfMemory;
	else sprintf(MBox, "%s/%s", QWName, MBName);
    }
    if (ErrCode == wperr_NoSuchField) MBox = NULL;
    else if (ErrCode != wperr_NoError) {
	AddrResult(tmltf_MemoryExhausted, Addr, wp_ErrorString(ErrCode));
	if (WName != NULL) free(WName);
	free(KVal); if (Hdr != NULL) free(Hdr); return;
    }
#if Logs
    Log(234, "Got full name and mailbox--about to check for fwd loop.");
#endif /* Logs */
    /* Now check for a forwarding loop. */
    if (IsAFwdLoop(Addr, Fwd, Hdr, WName, MBox, MBName)) {
	if (WName != NULL) free(WName);
	if (MBox != NULL) free(MBox);
	if (KVal != NULL) free(KVal);
	if (Hdr != NULL) free(Hdr);
	return;		/* on forward chain--return */
    }
#if Logs
    Log(235, "Not on a forwarding loop--about to do forwarding");
#endif /* Logs */

    /* Not on any already-done list--handle here. */
    if (TMDebug) fprintf(stderr, "HandleRecipient checking fwd info for %s.\n",
			  Addr->LocalPart);
    FwdInfo = NULL;
#if Logs
    Log(238, "Finding WP forwarding");
#endif /* Logs */
    ErrCode = GetWPField(KVal, idxFwd, &FwdInfo);
#if Logs
    Log(239, "Got WP forwarding");
#endif /* Logs */
    if (ErrCode != wperr_NoSuchField) {		/* there's a fwd field! */
	if (ErrCode != wperr_NoError) {
	    AddrResult(tmltf_MemoryExhausted, Addr, wp_ErrorString(ErrCode));
	    if (FwdInfo != NULL) free(FwdInfo);
	    if (WName != NULL) free(WName);
	    if (MBox != NULL) free(MBox);
	    if (Hdr != NULL) free(Hdr);
	    free(KVal); return;
	}
	if (strcmp(FwdInfo, "**unknown**") == 0) {
	    free(FwdInfo);
	    if (TMDebug)
		fprintf(stderr, "HandleRcpt resolving unknown fwd:\n");
	    if (ULstrcmp(ThisDomain, DefaultDomain) == 0) {
#if Logs
		Log(240, "Trying to resolve unknown fwd");
#endif /* Logs */
		ErrCode = cwp_Read(wpCD, KVal, idxHD, &homeDir);
#if Logs
		Log(241, "Resolving: looked for home dir, code %d", ErrCode);
#endif /* Logs */
		if (ErrCode == wperr_NoError) {
		    OutMatch = FindForwarding(homeDir, &FwdInfo);
#if Logs
		    Log(242, "FindForwarding returned %d", OutMatch);
#endif /* Logs */
		    if (OutMatch == 0)	/* recovery worked! */
		    {if (FwdInfo == NULL) goto UnknownIsNull;
		    else goto UnknownNonNull;}
		    if (OutMatch != -1 && OutMatch != -2 && OutMatch != EIO && ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
			if (ULstrcmp(DefaultDomain, WorkstationCell) == 0) {
			    PermMessage(tmlpf_BadForwardingInfo, Addr, FindErrString(OutMatch), NULL);
			} else {
			    ThisIsRemote(Addr, Fwd, Fwd);
			}
		    } else {
			AddrResult(tmltf_MailFwdBadResolve, Addr,
				   FindErrString(OutMatch));
		    }
		} else {
		    AddrResult(tmltf_MailFwdUnknown, Addr,
			       "Fwd address unknown, and cannot identify home dir: %s",
			       wp_ErrorString(ErrCode));
		}
	    } else {
		AddrResult(tmltf_MailFwdUnknown, Addr, "Fwd address, in cell %s, unknown and undeterminable from here in cell %s", DefaultDomain, ThisDomain);
	    }
	    if (WName != NULL) free(WName);
	    if (MBox != NULL) free(MBox);
	    if (Hdr != NULL) free(Hdr);
	    free(KVal); return;
	}
	UnknownNonNull:
#if Logs
	  Log(245, "Got forwarding info");
#endif /* Logs */
	HandleForwarding(Addr, WName, MBox, FwdInfo,
			 Fwd, KVal, Forwarding, DefaultDomain);
	if (KVal != NULL) free(KVal);
#if Logs
	Log(246, "Forwarding handled for %s", Addr->LocalPart);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "HandleRcpt: Fwding done for %s.\n",
			     Addr->LocalPart);
	if (WName != NULL) free(WName);
	if (MBox != NULL) free(MBox);
	if (Hdr != NULL) free(Hdr);
	free(FwdInfo); return;
    }
    UnknownIsNull:
      if (TMDebug) fprintf(stderr, "HandleRcpt calling LocalMail on %s.\n", Addr->LocalPart);
    ReturnResult = LocalMail(Addr, Hdr, WName, MBox, Fwd, KVal, DefaultDomain, MBName);
    if (TMDebug) fprintf(stderr, "HandleRcpt returning after local mailing.\n");
    if (! IsItOnList(Addr, LocalDelivered, TRUE)) if ((Fwd == NULL ? TRUE : Fwd->Rewritten)) SafeAppendAddrTo(Addr, LocalDelivered, Fwd);
    if (KVal != NULL) free(KVal);
    if (WName != NULL) free(WName);
    if (MBox != NULL) free(MBox);
    if (Hdr != NULL) free(Hdr);
}

static void HandleAddresses(AddrList, Fwd, DefaultDomain)
PARSED_ADDRESS *AddrList;
struct FwdLink *Fwd;
char *DefaultDomain;
{
	if (TMDebug) {
		fprintf(stderr, "Entering HandleAddresses");
		if (Fwd == NULL) fprintf(stderr, ", Fwd null.\n");
		else fprintf(stderr, ", top Fwd name %s.\n", Fwd->fAddr->LocalPart);
	}
	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			HandleRecipient(ThisAddr, Fwd, DefaultDomain);
			break;
	case GROUP_ADDRESS:
			HandleAddresses(ThisAddr->Members, Fwd, DefaultDomain);
			break;
	default:
			break;
		}
		if (Fwd != NULL)
			if (Fwd->Scuttled) break;	/* don't handle everything */
	})
	if (TMDebug) {
		fprintf(stderr, "Leaving HandleAddresses");
		if (Fwd == NULL) fprintf(stderr, ", Fwd null.\n");
		else fprintf(stderr, ", top Fwd name %s.\n", Fwd->fAddr->LocalPart);
	}
}

static void DeliverAddresses(AddrList, DefaultDomain)
PARSED_ADDRESS *AddrList; char *DefaultDomain;
{/* Process the addresses given in AddrList.  Set up the global variables and call the recursive list-processing mechanism. */
	LocalDelivered = NewNullAddr();
	RemoteDelivered = NewNullAddr();
	if (LocalDelivered == NULL || RemoteDelivered == NULL) {
		GlobalTempFail(tmgtf_MemoryExhausted, "Memory exhausted");
	}
	HandleAddresses(AddrList, NULL, DefaultDomain);
}

static int CountCellHosts(AddrList, Domain)
PARSED_ADDRESS *AddrList; char *Domain;
{/* Count the addresses in AddrList that are destined for the given Domain. */
	auto int HowMany;

	HowMany = 0;
	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			if (ULstrcmp(ThisAddr->Hosts->Prev->Name, Domain) == 0)
				++HowMany;
			break;
	case GROUP_ADDRESS:
			HowMany += CountCellHosts(ThisAddr->Members, Domain);
			break;
	default:
			break;
		}
	})
	return HowMany;
}

static int AddAddresses(Hdr, Addrs, IxP, AddrList, Domain, DoneList)
char *Hdr; char **Addrs; int *IxP; PARSED_ADDRESS *AddrList, *DoneList; char *Domain;
{/* Unparse those addresses in AddrList that are destined for Domain, and append each to Addrs. */
/* Return 0 on failure and 1 on success. */
	auto char *ThisOne; long int Dum;

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			if (ULstrcmp(ThisAddr->Hosts->Prev->Name, Domain) == 0) {
			    ThisOne = AddrUnparse(ThisAddr, &Dum);
			    if (ThisOne == NULL) return 0;
			    if (WasThisSent(Hdr, ThisOne, ThisAddr) > 0) {
				free(ThisOne);
				AddrResult(tmok_RedundantDelivery, ThisAddr,
						"Already delivered");
				AppendAddrTo(ThisAddr, DoneList);
				break;
			    }
			    ++(*IxP);
			    Addrs[*IxP] = ThisOne;
			}
			break;
	case GROUP_ADDRESS:
			if (AddAddresses(Hdr, Addrs, IxP,
			    ThisAddr->Members, Domain, DoneList) == 0)
				return 0;
			break;
	default:
			break;
		}
	})
	return 1;
}

static void MoveMatching(AddrList, Domain, DoneList)
PARSED_ADDRESS *AddrList, *DoneList; char *Domain;
{/* Cross-cell dropoff worked!  Move the matching addresses to DoneList. */
    FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		       switch (ThisAddr->Kind) {
			   case SIMPLE_ADDRESS:
			       if (ULstrcmp(ThisAddr->Hosts->Prev->Name, Domain) == 0) {
				   AddrResult(tmok_DroppedOffToCell, ThisAddr,
					      "Dropped into queues for cell %s", Domain);
				   RemoteGotDelivered(ThisAddr);
				   AppendAddrTo(ThisAddr, DoneList);
			       }
			       break;
			   case GROUP_ADDRESS:
			       MoveMatching(ThisAddr->Members, Domain, DoneList);
			       break;
			   default:
			       break;
		       }
		       })
}

static void AuthCellDelivery(AddrList, homeCA)
PARSED_ADDRESS *AddrList; struct CellAuth *homeCA;
{/* Try to do inter-cell delivery to those cells in which we have some authentication. */
    struct CellAuth *ca;
    PARSED_ADDRESS *DoneList;
    int HowMany, Ix, RC, fd;
    unsigned long int Timeout;
    char **Addrs;
    char *Hdr;
    auto char fnBuff[1000];

    if ((GlobalOptions & tmopt_CrossCellDropoff) == 0) return;
    Timeout = 0;
    Hdr = NULL;
    fnBuff[0] = '\0';
    if (ca_UpdateCellAuths() != 0) return;
    ca = NULL;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	if (ca == homeCA) continue;	/* Cut spin-loops short. */
	if (Timeout == 0) Timeout = ((unsigned long int) time(NULL)) + 5*60;	/* five minutes from now */
	if (ca->ExpireTime < Timeout) continue;  /* Don't use tokens that are about to expire. */
	if (ca->UsesAMSDelivery == 0) CheckAMSDelivery(ca->CellName);
	if (ca->UsesAMSDelivery > 0) {
	    HowMany = CountCellHosts(AddrList, ca->CellName);
	    if (HowMany <= 0) continue;
	    DoneList = NewNullAddr();
	    if (DoneList == NULL) continue;
	    if (Hdr == NULL) {
		RC = GetHeader(&Hdr);
		if (RC != 0) {FreeAddressList(DoneList); continue;}
		RC = MakeFile(NULL, Hdr, fnBuff, NULL);
		if (RC != 0) {free(Hdr); Hdr = NULL;
		FreeAddressList(DoneList); continue;}
	    }
	    Addrs = (char **) malloc((HowMany+1) * sizeof(char *));
	    if (Addrs == NULL) {
		FreeAddressList(DoneList);
		continue;
	    }
	    for (Ix = 0; Ix <= HowMany; ++Ix) Addrs[Ix] = NULL;
	    Ix = -1;
	    if (AddAddresses(Hdr, Addrs, &Ix, AddrList, ca->CellName, DoneList) != 0 && Ix >= 0) {
		fd = open(fnBuff, O_RDONLY, 0644);
		if (fd >= 0) {
		    RC = tryvicequeues_cell(Addrs, fd, TextRetPath, AuthString,
					    0, GlobalForString, 0, ca->CellName,
					    MailQueueNamePrefix);
		    close(fd);
		    if (RC == Q_OK) {
			for (Ix = 0; Ix < HowMany; ++Ix) {
			    if (Addrs[Ix] != NULL) ThisWasSent(Hdr, Addrs[Ix], GlobalForString, 1);
			}
			MoveMatching(AddrList, ca->CellName, DoneList);
			if (TMDebug) {
			    fprintf(stderr, "AuthCellDelivery to queues for cell %s: delivered %s", ca->CellName, Addrs[0]);
			    for (Ix = 1; Ix < HowMany; ++Ix) fprintf(stderr, ", %s", Addrs[Ix]);
			    fprintf(stderr, "\n");
			}
		    }
		}
	    }
	    for (Ix = 0; Ix < HowMany; ++Ix)
		if (Addrs[Ix] != NULL) free(Addrs[Ix]);
	    free(Addrs);
/* Don't reclaim addresses because that causes switchmail's subsequent DealWithRemoteList to crap out in trying to refer to the (now moved) addresses.  Since we're exiting soon, we can afford to discard our pointers to this storage. */
	    /* FreeAddressList(DoneList); */ DoneList = NULL;
	}
    }
    if (Hdr != NULL) {free(Hdr); unlink(fnBuff);}
}

/*VARARGS3*/
static void UsageError(CodeVal, PgmName, Fmt, p1, p2, p3, p4, p5)
int CodeVal; char *PgmName, *Fmt;
{/* Complains of usage error; never returns. */
	char NewFmt[350];

	sprintf(NewFmt, "%s; usage: %s [-dDtMTwW] [-Eentrytime] [-Sminsize] [-Aauth] [-4forwhom] FileToSend ReturnPath",
			Fmt, PgmName);
	GlobalPermFail(CodeVal, "Usage error", NewFmt, p1, p2, p3, p4, p5);
}

static void ParseArguments(argc,argv)
int argc;
char *argv[];
{
	int thisarg, i;
	char *Swch;

	for (thisarg = 1; thisarg < argc && argv[thisarg][0] == '-'; ++thisarg) {
		Swch = &argv[thisarg][1];
		if (strcmp(Swch, "d") == 0) TMDebug = 1;
		else if (strcmp(Swch, "D") == 0) TMDebug = 2;
		else if (strcmp(Swch, "w") == 0) (void) wp_SetDebugging(1);
		else if (strcmp(Swch, "W") == 0) (void) wp_SetDebugging(2);
		else if (strcmp(Swch, "t") == 0) DoTiming = 1;
		else if (strcmp(Swch, "M") == 0) {
#ifdef ANDREW_MALLOC_ENV
		    (void) SetMallocCheckLevel(4);
#else /* ANDREW_MALLOC_ENV */
		    fprintf(stderr, "``-M'' option ignored because ANDREW_MALLOC_ENV option not defined\n");
#endif /* ANDREW_MALLOC_ENV */
		} else if (strcmp(Swch, "F") == 0) /* ignore obsolete switch */;
		else if (strcmp(Swch, "T") == 0) ReallyDeliver = FALSE;
		else if (strcmp(Swch, "R") == 0) /* ignore obsolete switch */;
		else if (Swch[0] == 'S') MinimumFileSize = atoi(&Swch[1]);
		else if (Swch[0] == 'A') AuthString = &Swch[1];
		else if (Swch[0] == 'E') EnqueueDate = atoi(&Swch[1]);
		else if (Swch[0] == '4') GlobalForString = &Swch[1];
		else if (Swch[0] == 'C') {/* Ignore most options.  (For future compatibility.) */;
			if (Swch[1] == 'h') homeCell = &Swch[2];	/* home cell */
			else if (Swch[1] == '1') GlobalOptions |= atoi(&Swch[2]);
			else if (Swch[1] == '0') GlobalOptions &= ~atoi(&Swch[2]);
		} else UsageError(tmgpf_UnknownOption, argv[0],
				"Unrecognized option: ``%s''", argv[thisarg]);
	}

	if (thisarg < argc) {
		InFileName = NewString(argv[thisarg++]);
	} else UsageError(tmgpf_MissingFileToSend, argv[0], "Missing FileToSend argument");
				
	if (thisarg < argc) {
		TextRetPath = NewString(argv[thisarg++]);
	} else UsageError(tmgpf_MissingReturnPath, argv[0], "Missing ReturnPath argument");
	for (i = strlen(TextRetPath) - 1; i >= 0 && TextRetPath[i] == ' '; --i) TextRetPath[i] = '\0';
				
	if (thisarg < argc) UsageError(tmgpf_ExtraArguments, argv[0],
				"Extra arguments beginning with ``%s''", argv[thisarg]);
}

static void SetExpiry()
{/* Set IsExpired and IsExtraExpired flags based on the EnqueueDate and the current time. */
	long age; u_long RidiculousAge;
	u_long DayMax;

	IsExpired = IsExtraExpired = IsALittleExpired = IsSomeExpired = IsCrossCellExpired = FALSE;
	if (EnqueueDate == 0) return;
/* Sanity checks on the time value.  EnqueueDate values only appear too old if they're between AMS_ViceQueueLifetime and RidiculousAge before now.  Otherwise we assume a clock change (a time warp).  RidiculousAge is set to be the greater of AMS_ViceQueueLifetimeCap and twice the sum of AMS_ViceQueueLifetime and AMS_ExtraViceQueueLifetime. */

	if (CheckAMSConfiguration() == 0 && osi_GetTimes(&TV) == 0) {
	    age = TV.Secs - EnqueueDate;
	    RidiculousAge = 2 * (AMS_ViceQueueLifetime + AMS_ExtraViceQueueLifetime);
	    if (RidiculousAge < AMS_ViceQueueLifetimeCap)
		RidiculousAge = AMS_ViceQueueLifetimeCap;
	    if (age > 0 && age < RidiculousAge) {
		if (age > AMS_ViceQueueLifetime) IsExpired = TRUE;
		if (age > (AMS_ViceQueueLifetime + AMS_ExtraViceQueueLifetime)) IsExtraExpired = TRUE;
		if (age > AMS_CrossCellQueueLifetime) IsCrossCellExpired = TRUE;
		DayMax = 24*60*60;  /* one day, in seconds, but ensure in bounds. */
		if (DayMax > AMS_ViceQueueLifetime) DayMax = AMS_ViceQueueLifetime;
		if (age > DayMax) IsALittleExpired = TRUE;
		if (age > ((DayMax + AMS_ViceQueueLifetime) / 2)) IsSomeExpired = TRUE;
	    }
	    if (TMDebug) fprintf(stderr,
		"Age: %d (%d,%d,%d): IsExpired=%d, IsExtraExpired=%d, IsALittleExpired=%d, IsSomeExpired=%d, IsCrossCellExpired=%d.\n", age,
			AMS_ViceQueueLifetime,
			AMS_ExtraViceQueueLifetime + AMS_ViceQueueLifetime,
			RidiculousAge, IsExpired, IsExtraExpired, IsALittleExpired, IsSomeExpired, IsCrossCellExpired);
	}
}

static void GetDestinations()
{/* Read the list of destinations from stdin until end-of-file.  Store them in TextDestinations. */
	char *NewDests, *NewEnd, *OldDests;
	int OneChar, CurUsed, CurSize;
#define InitTDSize 300

	NewDests = NewEnd = malloc(InitTDSize);
	if (NewDests == NULL)
		GlobalTempFail(tmgtf_MemoryExhausted, "Memory for arguments exhausted");
	CurUsed = 1;	/* account for the trailing null */
	CurSize = InitTDSize;
	while ((OneChar = getc(stdin)) != EOF) {
		if ((++CurUsed) > CurSize) {
			CurSize *= 2;	/* double the size */
			OldDests = NewDests;
			NewDests = realloc(NewDests, CurSize);
			if (NewDests == NULL)
				GlobalTempFail(tmgtf_MemoryExhausted,
						"Memory for arguments exhausted");
			NewEnd += (NewDests - OldDests);	/* adjust pointer */
		}
/* turn newlines to spaces for JR's parser. */
		if (isspace(OneChar)) OneChar = ' ';
		*NewEnd++ = OneChar;
	}
	*NewEnd = '\0';
	TextDestinations = NewDests;
	--NewEnd;
	while (NewEnd > TextDestinations && isspace(*NewEnd)) *NewEnd-- = '\0';
	++NewEnd;
	TextDestinationsLen = NewEnd - NewDests;
	if (TMDebug) fprintf(stderr, "Arg, len %d, is ``%s''.\n",
				TextDestinationsLen, TextDestinations);
}

#ifdef ANDREW_MALLOC_ENV
/*ARGSUSED*/
static int ReturnZero(i)
int i;
{/* Dummy for SetM0Handler */
	return 0;
}
#endif /* ANDREW_MALLOC_ENV */

main(argc, argv)
int argc;
char *argv[];
{
	int parse_code, statcode;
	wp_ErrorCode wpErr;
#ifdef ANDREW_MALLOC_ENV
	extern void SetM0Handler();
#endif /* ANDREW_MALLOC_ENV */
	int SetREErr, AMSDel, euid;
	FILE *Adv;
	char *DefaultDomain;
	struct CellAuth *homeCA;

	statcode = 0;
	WpOpen = WpOpen_NotOpen;

/* Now initialize the exported globals */
	TMDebug = 0;
	IsExpired = IsExtraExpired = IsALittleExpired = 0;
	ReallyDeliver = 1;
	DoTiming = 0;
	MinimumFileSize = -1;
	InFileName = NULL;
	TextRetPath = NULL;
	TextDestinations = NULL;
	UnparseBuff = NULL;
	UnparseBuffSize = 0;
	InFile = NULL;
	InFileStat.st_nlink = -1;
	ParsedRetPath = NULL;
	RootTemp = NULL;
	ParsedDestinations = NULL;
	gotWPIndices = FALSE;
	InFileErrno = -1;
	OrigDestinationCount = -1;
	AuthString = GlobalForString = NULL;
	EnqueueDate = 0;
	WpOpenCell[0] = '\0';
	wpCD = NULL;
	AnyAuthentication = TRUE;
	homeCell = NULL;
	GlobalOptions = (IsTrymail() ? (tmopt_HomeDelivery | tmopt_CrossCellDropoff) : -1);

#ifdef ANDREW_MALLOC_ENV
	SetM0Handler(ReturnZero);
#endif /* ANDREW_MALLOC_ENV */
	SetREErr = -1;
	euid = geteuid();
	if (getuid() != euid) {	/* Make real and effective UIDs the same */
		if (setreuid(euid, -1) != 0) SetREErr = errno;
	}

	RootTemp = NewNullAddr();
	ParseArguments(argc, argv);
#if LogsYes
	if (DoTiming) {
		if (InitStats("trymail", 0) != 0) {
			DoTiming = 0;
		} else {
			SetTransaction(time(0) % 1000);
			Log(100, "Starting");
		}
	}
#endif /* LogsYes */

	SetExpiry();
	if (RootTemp == NULL || InFileName == NULL || TextRetPath == NULL) {
		GetDestinations();
		GlobalTempFail(tmgtf_MemoryExhausted, "Memory for arguments exhausted");
	}
	if (SetREErr >= 0) {
		GetDestinations();
		GlobalTempFail(tmgtf_UIDError, "Cannot reset my real user ID: %s",
				UnixError(SetREErr));
	}
	if (CheckAMSConfiguration() != 0) {
		GetDestinations();
		GlobalTempFail(tmgtf_ConfigError, "AMS Configuration failed");
	}
#if Logs
	Log(101, "About to open input file %s", InFileName);
#endif /* Logs */
	errno = 0;
	InFile = fopen(InFileName, "r");
#if Logs
	Log(102, "fopen(%s) returned 0x%x", InFileName, InFile);
#endif /* Logs */
	if (InFile != NULL) statcode = fstat(fileno(InFile), &InFileStat);
	InFileErrno = errno;
	if (InFile == NULL || statcode != 0) {
		if (InFile != NULL) {fclose(InFile); InFile = NULL;}
		GetDestinations();
		if (tfail(InFileErrno))
			GlobalTempFail(tmgtf_InputFileOpenFailure,
					"Cannot read message text file ``%s'': %s",
					InFileName, UnixError(InFileErrno));
		else
			GlobalPermFail(tmgpf_InputFileOpenFailure, "Unreadable input file",
					"Cannot read message text file ``%s'': %s",
					InFileName, UnixError(InFileErrno));
	}
	if ((InFileStat.st_mode & S_IFMT) != S_IFREG) {
		fclose(InFile); InFile = NULL;
		GetDestinations();
		GlobalPermFail(tmgpf_InputFileOpenFailure, "Input file not a text file",
			"Message text file ``%s'' is not a regular text file, but is of type %#o.",
			InFileName, (InFileStat.st_mode & S_IFMT));
	}

	if (TMDebug < 2 &&
		    (TextRetPath[0] != '<' || TextRetPath[strlen(TextRetPath) - 1] != '>'))
		UsageError(tmgpf_RetPathLexicalError, argv[0],
			"ReturnPath ``%s'' must be in angle brackets", TextRetPath);
	if (TMDebug) fprintf(stderr, "Calling ParseAddressList on return address:\n");
#if Logs
	Log(105, "About to parse return path");
#endif /* Logs */
	if (strcmp(TextRetPath, "<>") == 0)
		{ParsedRetPath = NULL; parse_code = PA_OK;}
	else
		parse_code = ParseAddressList(TextRetPath, &ParsedRetPath);
#if Logs
	Log(106, "Return path parsed--parse_code %d", parse_code);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "ParseAddressList returned.\n");
	if (parse_code != PA_OK && parse_code != PA_SYNTAX_ERROR) {
		ParsedRetPath = NULL;
		GetDestinations();
		if (parse_code == PA_NO_MEM || parse_code == PA_PARTIAL) {
			GlobalTempFail(tmgtf_MemoryExhausted,
					"Error (%d) parsing return path ``%s''",
					parse_code, TextRetPath);
		} else {
			GlobalPermFail(tmgpf_RetPathParseError, "Bad return-path",
				"Error (%d) parsing return path ``%s''",
					parse_code, TextRetPath);
		}
	}

	if (InFileStat.st_size < TMGlobalMinimumSize || InFileStat.st_size < MinimumFileSize) {
		GetDestinations();
		if (InFileStat.st_size < MinimumFileSize)
		    GlobalPermFail(tmgpf_FileTooShort, "Message too short",
			"Size of message text file ``%s'' is %d--shorter than the specified %d",
			InFileName, InFileStat.st_size, MinimumFileSize);
		else
		    GlobalPermFail(tmgpf_ZeroLengthFile, "Message too short",
			"Size of message text file ``%s'' is %d--shorter than the necessary %d",
			InFileName, InFileStat.st_size, TMGlobalMinimumSize);
	}
	homeCA = NULL;
	if (homeCell != NULL) {
		parse_code = FindCell(homeCell, &homeCA);
		if (parse_code == 2) AnyAuthentication = FALSE;
		if (homeCA == NULL) {
		    if (parse_code > 0) {
			GlobalPermFail(tmgpf_CellError, "Cannot find named cell",
			"Cannot find %s for AMS home: code %d", homeCell, parse_code);
		    } else {
			GlobalTempFail(tmgtf_ConfigError, "Cannot find named cell",
			"Cannot find cell %s for AMS home now: code %d",
			homeCell, parse_code);
		    }
		}
		parse_code = SetAMSHomeCell(homeCA);
		if (parse_code != 0) {
		    if (parse_code > 0) {
			GlobalPermFail(tmgpf_CellError, "Cannot set home cell", "Cannot set cell %s as AMS home: %s", homeCell, AMSHome_errmsg);
		    } else {
			GlobalTempFail(tmgtf_ConfigError, "Cannot set home cell",
			"Cannot set cell %s as AMS home now: %s", homeCell, AMSHome_errmsg);
		    }
		}
	} else {
		if (FindAMSHomeCell(&homeCA) == 2) AnyAuthentication = FALSE;
	}
	DefaultDomain = (homeCA != NULL ? homeCA->CellName : ThisDomain);
	AMSDel = CheckAMSDelivery(DefaultDomain);
	if (AMSDel <= 0) {
		GetDestinations();
		GlobalTempFail(tmgtf_ConfigError,
			"Cell ``%s'' is not %srunning this delivery system: %d",
			DefaultDomain,
			(AMSDel < 0 ? "" : "necessarily "));
	}
	GlobalDom = DefaultDomain;
	if (ULstrcmp(DefaultDomain, ThisDomain) != 0) {
	    errno = 0;
	    parse_code = CkAMSCellConfig(DefaultDomain);
	    if (parse_code != 0) {  /* <0 is temp fail, >0 is errno-is-valid */
		GlobalTempFail(tmgtf_ConfigError, "Can't read config file %s for cell %s: %s", CellConfigMessageServer, DefaultDomain, (parse_code < 0 ? "out of memory" : UnixError(errno)));
	    }
	}
#if LogsYes
	Log(107, "About to read stdin for destinations");
#endif /* LogsYes */
	GetDestinations();
#if Logs
	Log(108, "Destinations read and stored--about to parse them");
#endif /* Logs */

	if (parse_code == PA_SYNTAX_ERROR) {
		ParsedRetPath = NULL;
		Adv = OpenPMAdvisory("Bad syntax in return-path", 1, NULL, DefaultDomain);
		if (Adv != NULL) {
			PMAdvPrint("Syntax error in the return-path.  Delivering the message anyway.\n\n");
			ClosePMAdvisory();
		}
	}

	if (TMDebug) fprintf(stderr, "Calling ParseAddressList on destinations:\n");
	if (strcmp(TextDestinations, "<>") == 0)
		{ParsedDestinations = NULL; parse_code = PA_OK;}
	else
		parse_code = ParseAddressList(TextDestinations, &ParsedDestinations);
#if Logs
	Log(109, "Destinations parsed--parse_code %d", parse_code);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "ParseAddressList returned.\n");
	if (parse_code != PA_OK) {
		if (parse_code == PA_SYNTAX_ERROR)
			GlobalPermFail(tmgpf_DestinationSyntaxError,
				"Bad syntax for destination list",
				"Syntax error in destination list");
		else if (parse_code == PA_NO_MEM || parse_code == PA_PARTIAL)
			GlobalTempFail(tmgtf_MemoryExhausted,
					"Error (%d) parsing destinations", parse_code);
		else
			GlobalPermFail(tmgpf_DestinationParseError,
				"Error parsing destination list",
				"Error (%d) parsing destinations", parse_code);
	}
	OrigDestinationCount = 0;
	if (ParsedDestinations != NULL)
		CountAddresses(ParsedDestinations, &OrigDestinationCount);
	if (TMDebug)
		fprintf(stderr, "CountAddresses finds %d destinations.\n", OrigDestinationCount);
#if Logs
	Log(110, "About to handle delivery");
#endif /* Logs */
	if (ParsedDestinations != NULL) DeliverAddresses(ParsedDestinations, DefaultDomain);
#if Logs
	Log(111, "Delivery done except for remote list");
#endif /* Logs */
	if (ParsedDestinations != NULL && homeCA != NULL)
		AuthCellDelivery(RemoteDelivered, homeCA);
	if (ParsedDestinations != NULL) DealWithRemoteList(RemoteDelivered);
	if (TMDebug) fprintf(stderr, "Calling FreeAddressList:\n");
#if LogsYes
	Log(112, "All delivery handled--freeing destination list");
#endif /* LogsYes */
	if (ParsedDestinations == NULL)
		parse_code = 0;
	else
		parse_code = FreeAddressList(ParsedDestinations);
#if Logs
	Log(113, "FreeAddressList returned");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "FreeAddressList returns %d.\n", parse_code);
	ParsedDestinations = NULL;
	fclose(InFile); InFile = NULL;
	FinishSent();
	if (WpOpen == WpOpen_Open) {
#if Logs
		Log(120, "About to shut down; calling cwp_Terminate()");
#endif /* Logs */
		wpErr = cwp_Terminate(wpCD);
#if Logs
		Log(121, "cwp_Terminate() returned %d", wpErr);
#endif /* Logs */
		if (wpErr != wperr_NoError)
			GlobalRslt(tm_EndOfInteraction, "cwp_Terminate returns %d (%s).",
					wpErr, wp_ErrorString(wpErr));
	}
	GlobalRslt(tm_EndOfInteraction, "End.");
}
