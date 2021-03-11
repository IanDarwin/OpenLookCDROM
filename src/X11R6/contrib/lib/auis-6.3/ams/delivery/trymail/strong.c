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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/delivery/trymail/RCS/strong.c,v 1.56 1993/12/08 22:57:09 gk5g Exp $";
#endif

/* ************************************************************ *\
	strong.c
	Subroutines for ``switchmail''--the trymail version that runs on
	postoffices and tries hard to deliver mail.  Its determinations
	(e.g., ``no such user'') are viewed as authoritative, and it thus
	is permitted to return mail to its sender.
\* ************************************************************ */

#include <andrewos.h>
#include <stdio.h>
#include <truth.h>
#include <sys/param.h>
#include <system.h>
#include <errno.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif
#include <pwd.h>
#include <ctype.h>
#include <ckndbm.h>
#ifdef POSIX_ENV
#include <limits.h>
#include <unistd.h>
#endif

#include <util.h>
#include <mailconf.h>
#include <mail.h>
#include <qmail.h>
#include <parseadd.h>
#include <wp.h>

extern int errno;

#include <trymail.h>
#include <wkstr.h>

static struct osi_Times TV;	/* This can be updated by anybody, any time, with osi_GetTimes() */

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */

#if Logs
/*VARARGS2*/
static void Log(num, fmt, p1, p2, p3, p4, p5, p6)
int num; char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
  { if (DoTiming) Logstat("strong.c", num, fmt, p1, p2, p3, p4, p5, p6); }
#endif /* Logs */

static char *FindErrs[] = {
/* -1 */	"Out of memory resolving forwarding address",
/* -2 */	"Temporary failure resolving forwarding address",
/* -3 */	"No home directory--Cannot resolve forwarding address",
/* -4 */	"Bad forwarding file--Cannot resolve forwarding address",
	NULL};
#define FindErrMax 4
char *FindErrString(code)
int code;
{	static char FEBuff[35];

	if (code >= 0) return UnixError(code);
	code = -code;
	if (code <= FindErrMax) return FindErrs[code - 1];	/* a 1-origin array */
	sprintf(FEBuff, "Find error -%d", code);
	return FEBuff;
}

int FindForwarding(homeDir, FwdString)
char *homeDir, **FwdString;
{
/* If we can't find the user's forwarding string in the database, try to find it at delivery time.
   Allocate a copy of the file. */
	struct stat Stat;
	FILE *f;
	char *FwdName, *FwdContent, *SrcP, *DstP;
	int Err, Tries;

	FwdName = malloc(strlen(homeDir) + strlen(ForwardingName) + 4);
	if (FwdName == NULL) return -1;
	sprintf(FwdName, "%s/%s", homeDir, ForwardingName);
	for (Tries = 0; Tries < 3; Tries++) {
#if Logs
		Log(100, "FindForwarding opening file %s", FwdName);
#endif /* Logs */
		f = fopen(FwdName, "r");
#if Logs
		Log(101, "fopen returns 0x%x, errno %d", f, errno);
#endif /* Logs */
		if (f != NULL) {
			if (fstat(fileno(f), &Stat) != 0) {fclose(f); continue;}
			if ((Stat.st_mode & S_IFMT) != S_IFREG)
				{fclose(f); free(FwdName); return -4;}
			FwdContent = malloc(Stat.st_size + 1);
			if (FwdContent == NULL)
				{fclose(f); free(FwdName); return -1;}
			errno = 0;
#if Logs
			Log(102, "About to read contents");
#endif /* Logs */
			Err = fread(FwdContent, sizeof(char), Stat.st_size, f);
#if Logs
			Log(103, "fread returns %d", Err);
#endif /* Logs */
			fclose(f); free(FwdName);
#if Logs
			Log(104, "Fwd file closed and name freed");
#endif /* Logs */
			if (Err != Stat.st_size) return (errno ? errno : EIO);
			SrcP = DstP = FwdContent;	/* now compress out nulls */
			while (--Err >= 0) {if (*SrcP != '\0') *DstP++ = *SrcP; ++SrcP;}
			*DstP++ = '\0';
			*FwdString = FwdContent;
			return 0;
		} else {
			Err = errno;
			if (vdown(Err)) {free(FwdName); return -2;}
			if (Err == EACCES) break;	/* assume no forwarding */
			if (stat(homeDir, &Stat) != 0) {
				free(FwdName);
				if (vdown(errno)) return -2;
				else return -3;
			}
		}
	}
	free(FwdName);
	*FwdString = NULL;
	return 0;		/* no forwarding */
}

/* Write tracing header line(s) to file f */
static void WriteReceived(f, For, With)
FILE *f;
char *For, *With;
{
	static char head[] = "Received: via switchmail";
	int curLen, forLeft;
#define maxLen 80
	char *fp, *bp, oldc;

	fputs(head, f); curLen = sizeof(head) - 1;
	if (With != NULL) {fprintf(f, " with %s", With); curLen += (6+strlen(With));}
	if (For != NULL) {
		fputs(" for ", f); curLen += 5;
		fp = For;
		forLeft = strlen(fp);
		while ((curLen + forLeft) > maxLen) {
			oldc = fp[maxLen - curLen];
			fp[maxLen - curLen] = '\0';
			bp = rindex(fp, ' ');
			fp[maxLen - curLen] = oldc;
			if (bp == NULL) bp = index(&fp[maxLen - curLen], ' ');
			if (bp == NULL) break;	/* can't break it any more */
			*bp = '\0';
			fputs(fp, f);
			*bp++ = ' ';		/* it had to have been a space */
			fp = bp;
			fputs("\n ", f); curLen = 1;
			forLeft = strlen(fp);
		}
		fputs(fp, f); curLen += forLeft;
	}
	fputc(';', f);
	if ((curLen+30) > maxLen) {
		fputs("\n ", f); curLen = 1;
	} else fputc(' ', f);
	fputs(arpadate(), f);		/* Result of arpadate() ends in '\n' */
}

int IsTrymail()
{/* Tell whether trymail.c should work hard. */
	return 0;
}

/* The order for using the following functions is:
    GetHeader;
    OpenStripBE;
	[OpenUnscribe; (AbortUnscribe | (StripBE; (AbortUnscribe | CloseUnscribe)))]*;
    CloseStripBE;
    free Header;
*/

struct BEStripper {
	char *Header, *HeaderEnd;
	char *xasfLineBegin, *xasfLineEnd;
	char *ctLineBegin, *ctLineEnd;
	char *ituLineBegin, *ituLineEnd;
	char *xamsLineBegin, *xamsLineEnd;
	char *UnScrValue;
	struct ScribeState *UnScribeState;
	int UnScribeCode;
	char DeleteContentType, DoUnScribe;
};

static char OSBE_Text[1000], OSBE_Brief[200];
enum Formats {Normal, AlwaysOK, NeverOK};

static int OpenStripBE(pBES, Hdr, Fmt)
struct BEStripper **pBES; char *Hdr; enum Formats Fmt;
{/* Allocate and initialize the BEStripper structure according to our outgoing message. */
/* AcceptsFormatting is FALSE in the normal case where the destination does not generally accept formatted mail, and is TRUE if the destination can accept formatted messages. */
	struct BEStripper *BES;
	static char xasfFieldName[] = "X-Andrew-ScribeFormat:";
	static char ctFieldName[] = "Content-Type:";
	static char ituFieldName[] = "If-Type-Unsupported:";
	static char xamsFieldName[] = "X-Andrew-Message-Size:";
	char *CopyHdr;
	char *ValPtr, *Val2, *xasfVal, *ctVal, Ch;
	int xasfCode, ctCode, CopyHdrSize;
	enum {itu_alter, itu_send, itu_return} ituCode;

	*pBES = NULL;
	BES = (struct BEStripper *) malloc(sizeof(struct BEStripper));
	if (BES == NULL) return 2;
	BES->HeaderEnd = NULL;
	BES->xasfLineBegin = BES->xasfLineEnd = NULL;
	BES->ctLineBegin = BES->ctLineEnd = NULL;
	BES->ituLineBegin = BES->ituLineEnd = NULL;
	BES->xamsLineBegin = BES->xamsLineEnd = NULL;
	BES->UnScrValue = NULL;
	BES->UnScribeCode = -1;
	BES->DoUnScribe = 0;
	
	BES->Header = Hdr;
	if (BES->Header != NULL) {
		BES->HeaderEnd = ULsindex(BES->Header, "\n\n");
		if (BES->HeaderEnd != NULL) {
			BES->HeaderEnd += 2;
			*BES->HeaderEnd = '\0';
		} else {
			BES->HeaderEnd = BES->Header + strlen(BES->Header);
		}
	}
	if (BES->Header == NULL || BES->HeaderEnd == NULL)
		{*pBES = BES; return 0;}	/* all OK--just no header */

	xasfCode = ctCode = -1;
	ituCode = itu_alter;
	xasfVal = ctVal = NULL;
	CopyHdrSize = strlen(Hdr) + 1;
	CopyHdr = malloc(CopyHdrSize);
	if (CopyHdr == NULL) {free(BES); return 8;}	/* out of memory */
/* First, look for the Content-Type: header. */
	if (Fmt == AlwaysOK || BracketField(BES->Header, ctFieldName,
	    &ctVal, &BES->ctLineEnd, &BES->ctLineBegin) == 0) {
		ctVal = BES->ctLineEnd = BES->ctLineBegin = NULL;
	} else {	/* We have a Content-Type: header; must deal with it. */
	    ValPtr = ctVal;
	    if (BracketField(BES->Header, ituFieldName,
	      &Val2, &BES->ituLineEnd, &BES->ituLineBegin) == 0) {
		Val2 = BES->ituLineEnd = BES->ituLineBegin = NULL;
		ituCode = itu_alter;	/* This is the default value */
	    } else {	/* and an If-Type-Unsupported: header */
	    	Val2 = Next822LPart(Val2, BES->ituLineEnd, CopyHdr, CopyHdrSize);
		if (Val2 != NULL) {
		    if (ULstrcmp(CopyHdr, "alter") == 0) ituCode = itu_alter;
		    else if (ULstrcmp(CopyHdr, "send") == 0) ituCode = itu_send;
		    else if (ULstrcmp(CopyHdr, "ignore") == 0) ituCode = itu_send;
		    else if (ULstrcmp(CopyHdr, "return") == 0) ituCode = itu_return;
		    else if (ULstrcmp(CopyHdr, "reject") == 0) ituCode = itu_return;
		    else {
			strcpy(OSBE_Brief, "Unrecognized If-Type-Unsupported code");
			sprintf(OSBE_Text, "Unrecognized value for %s field: ``%s''.\n",
				ituFieldName, CopyHdr);
			free(CopyHdr);
			return -2;
		    }
		}
	    }
	    /* Allow messages with totally-unrecognized Content-type: headers to pass through without alteration; pretend here that they don't have the Content-type: header at all.  Do checking on the sub-type of ``X-BE2'' or ``BE2'' content types, though. */
	    ValPtr = Next822LPart(ValPtr, BES->ctLineEnd, CopyHdr, CopyHdrSize);
	    /* OK, is there a token that we recognize here? */
	    if (ValPtr != NULL
	      && (ULstrcmp(CopyHdr, "X-BE2") == 0 || ULstrcmp(CopyHdr, "BE2") == 0)) {
		ctCode = -3;	/* There was a Content-Type: header of type BE2 */
		/* Complain if we don't understand (and can't translate) the sub-type */
		ValPtr = Next822LPart(ValPtr, BES->ctLineEnd, CopyHdr, CopyHdrSize);
		if (ValPtr != NULL && strcmp(CopyHdr, ";") == 0) {
		    ValPtr = Next822LPart(ValPtr, BES->ctLineEnd, CopyHdr, CopyHdrSize);
		    if (ValPtr != NULL) {
			ctCode = UnScribeInit(CopyHdr, &BES->UnScribeState);
			if (ctCode >= 0) UnScribeAbort(ctCode, &BES->UnScribeState);
			if (ctCode == -2) {free(CopyHdr); return 3;}
			else if (ctCode < 0) ctCode = -2;	/* unrecognized */
			if (ctCode >= 0) {
			    BES->UnScrValue = NewString(CopyHdr);
			    if (BES->UnScrValue == NULL) {free(CopyHdr); return 4;}	/* no memory */
			    BES->UnScribeCode = ctCode;
			    BES->DoUnScribe = 1;
			}
		    }
		}
	    }
	}
	/* Don't let folks send ATK-formatted text to netnews */
	if (Fmt == NeverOK && ituCode == itu_send && BES->DoUnScribe) ituCode = itu_alter;

/* Look for the X-Andrew-Message-Size header in case we delete it as we unscribe */
	if (BracketField(BES->Header, xamsFieldName,
	    &xasfVal, &BES->xamsLineEnd, &BES->xamsLineBegin) == 0) {
		BES->xamsLineEnd = BES->xamsLineBegin = NULL;
	}
/* Now look for the (old) X-Andrew-ScribeFormat: header. */
	if (Fmt == AlwaysOK || BracketField(BES->Header, xasfFieldName,
	    &xasfVal, &BES->xasfLineEnd, &BES->xasfLineBegin) == 0) {
		xasfVal = BES->xasfLineEnd = BES->xasfLineBegin = NULL;
	}
	if (xasfVal != NULL) {	/* We have the xasf header */
	    ValPtr = xasfVal;
	    ValPtr = Next822LPart(ValPtr, BES->xasfLineEnd, CopyHdr, CopyHdrSize);
	    if (ValPtr != NULL) {
		xasfCode = UnScribeInit(CopyHdr, &BES->UnScribeState);
		if (xasfCode >= 0) UnScribeAbort(xasfCode, &BES->UnScribeState);
		if (xasfCode == -2) {free(CopyHdr); return 5;}	/* out of memory */
		else if (xasfCode < 0) xasfCode = -2;	/* unrecognized */
		if (xasfCode >= 0) {
		    Val2 = NewString(CopyHdr);
		    if (Val2 == NULL) {free(CopyHdr); return 6;}  /* no memory */
		    if (BES->UnScrValue != NULL) free(BES->UnScrValue);
		    BES->UnScrValue = Val2;
		    BES->UnScribeCode = xasfCode;
		    BES->DoUnScribe = 1;
		}
	    }
	}
	free(CopyHdr); CopyHdr = NULL;

/* Now decide what UnScribe will be doing with this message. */
	if (xasfCode == -1 && ctCode == -1) {	/* No special headers. */
		BES->DeleteContentType = FALSE;	/* Pass verbatim. */
	} else if (ituCode == itu_alter && xasfCode < 0 && ctCode < 0) {
			/* No recognized content type, and need to alter. */
		strcpy(OSBE_Brief, "Cannot alter unknown content type");
		strcpy(OSBE_Text, "Cannot alter the message that says it is in ");
		if (xasfVal != NULL) {
			Ch = *BES->xasfLineEnd; *BES->xasfLineEnd = '\0';
			strcat(OSBE_Text, BES->xasfLineBegin);
			*BES->xasfLineEnd = Ch;
		} else {
			Ch = *BES->ctLineEnd; *BES->ctLineEnd = '\0';
			strcat(OSBE_Text, BES->ctLineBegin);
			*BES->ctLineEnd = Ch;
		}
		strcat(OSBE_Text, " because the named type is not recognized.");
		return -3;
	} else if (ituCode == itu_return) {
		strcpy(OSBE_Brief, "Unsupported content type");
		strcpy(OSBE_Text, "The mail transport system must alter the content type, ``");
		if (BES->UnScrValue != NULL) {
			strcat(OSBE_Text, BES->UnScrValue);
		} else {
		    if (xasfVal != NULL) {
			Ch = *BES->xasfLineEnd; *BES->xasfLineEnd = '\0';
			strcat(OSBE_Text, xasfVal);
			*BES->xasfLineEnd = Ch;
		    } else {
			Ch = *BES->ctLineEnd; *BES->ctLineEnd = '\0';
			strcat(OSBE_Text, ctVal);
			*BES->ctLineEnd = Ch;
		    }
		}
		strcat(OSBE_Text, "'', but the ");
		strcat(OSBE_Text, ituFieldName);
		strcat(OSBE_Text, " header value does not permit the alteration.");
		return -4;
	} else if (ituCode == itu_send) {
		BES->DeleteContentType = FALSE;	/* Pass everything through */
/*		if (BES->UnScrValue != NULL) */
/*			{free(BES->UnScrValue); BES->UnScrValue = NULL;} */
/*		BES->UnScribeCode = -1; */
		BES->DoUnScribe = 0;
	} else {	/* Need to unscribe it. */
		BES->DeleteContentType = TRUE;
	}
	if (TMDebug) fprintf(stderr,
		"OpenStripBE: %sUnScribing; %sdeleting Content-Type of ``%s'' (code %d).\n",
		(BES->DoUnScribe ? "" : "Not "),
		(BES->DeleteContentType ? "" : "not "),
		(BES->UnScrValue == NULL ? "<undef>" : BES->UnScrValue),
		BES->UnScribeCode);
	*pBES = BES;
	return 0;
}

static void CloseStripBE(BES)
struct BEStripper *BES;
{	/* Undo an OpenStripBE call. */
	if (BES != NULL) {
		if (BES->UnScrValue != NULL) free(BES->UnScrValue);
		free(BES);
	}
}

static void OpenUnscribe(BES)
struct BEStripper *BES;
{	/* Do the UnScribeInit call, allocating storage via BES->UnScribeState. */

	if (BES->Header != NULL && BES->UnScrValue != NULL && BES->DoUnScribe) {
		BES->UnScribeState = NULL;
		BES->UnScribeCode = UnScribeInit(BES->UnScrValue,
						&BES->UnScribeState);
			/* -2 is out of mem--try again later */
		if (BES->UnScribeCode == -1) {		/* not recognized */
			if (BES->UnScrValue != NULL) free(BES->UnScrValue);
			BES->Header = BES->UnScrValue = NULL;
			BES->DoUnScribe = 0;
		}
	}
}

static int CloseUnscribe(BES, OutF)
struct BEStripper *BES;
FILE *OutF;
{	/* Undo UnScribeInit after we've called any desired StripBEs (UnScribes). */
	int Res;

	if (BES->Header != NULL
	    && BES->UnScribeState != NULL
	    && BES->UnScribeCode >= 0
	    && BES->DoUnScribe != 0) {
		Res = UnScribeFlush(BES->UnScribeCode, &BES->UnScribeState, OutF);
		return Res;
	}
	return 0;
}

static void AbortUnscribe(BES)
struct BEStripper *BES;
{	/* Close off the UnScribeInit state without needing a valid file to send to */
	if (BES != NULL
	  && BES->UnScribeCode >= 0
	  && BES->UnScribeState != NULL
	  && BES->DoUnScribe != 0) {
		UnScribeAbort(BES->UnScribeCode, &BES->UnScribeState);
	}
}

static int StripBE(BES, OutF, refCharsRead)
struct BEStripper *BES;
FILE *OutF;
int *refCharsRead;
{	/* Send the mail message to file F, unscribing or not.  Return 0 if OK, non-0 if not.  If the return value is -1, errno will be valid. */
	int N, NMax, CharsRead, SuccCode, HdrPair, Sorted;
	char *LastHdrEnd;
#define TextBuffSize 1024
	char TextBuff[TextBuffSize+1];
	struct Omit {char *first, *last;} Omitted[4], Temp;
	int HighOmit = 0;

	*refCharsRead = -1;

	if (BES->Header != NULL) {	/* Write message piecemeal */
		/* Figure out which header sections to omit */
		if (BES->xasfLineBegin != NULL) {	/* Always omit these. */
			Omitted[HighOmit].first = BES->xasfLineBegin;
			Omitted[HighOmit].last = BES->xasfLineEnd + 1;
			++HighOmit;
		}
		if (BES->DeleteContentType) {
		    if (BES->ctLineBegin != NULL) {
			Omitted[HighOmit].first = BES->ctLineBegin;
			Omitted[HighOmit].last = BES->ctLineEnd + 1;
			++HighOmit;
		    }
		    if (BES->ituLineBegin != NULL) {
			Omitted[HighOmit].first = BES->ituLineBegin;
			Omitted[HighOmit].last = BES->ituLineEnd + 1;
			++HighOmit;
		    }
		    if (BES->xamsLineBegin != NULL) {
			Omitted[HighOmit].first = BES->xamsLineBegin;
			Omitted[HighOmit].last = BES->xamsLineEnd + 1;
			++HighOmit;
		    }
		}
		/* Sort the sections to be omitted */
		do {	/* bubble sort of three items */
		    Sorted = TRUE;
		    for (HdrPair = 1; HdrPair < HighOmit; ++HdrPair) {
			if (Omitted[HdrPair-1].first > Omitted[HdrPair].first) {
				Temp.first = Omitted[HdrPair].first;
				Temp.last = Omitted[HdrPair].last;
				Omitted[HdrPair].first = Omitted[HdrPair-1].first;
				Omitted[HdrPair].last = Omitted[HdrPair-1].last;
				Omitted[HdrPair-1].first = Temp.first;
				Omitted[HdrPair-1].last = Temp.last;
				Sorted = FALSE;
			}
		    }
		} while (! Sorted);
		LastHdrEnd = BES->Header;
		/* Now for each omitted section, write the section before it and skip this one */
		for (HdrPair = 0; HdrPair < HighOmit; ++HdrPair) {
		    NMax = Omitted[HdrPair].first - LastHdrEnd;
		    if (NMax > 0) {	/* a non-null piece */
			if (TMDebug) fprintf(stderr, "Sending first %d chars of ``%s''.\n",
					NMax, LastHdrEnd);
			SuccCode = fwriteallchars(LastHdrEnd, NMax, OutF);
			if (SuccCode <= 0) return -1;
		    }
		    LastHdrEnd = Omitted[HdrPair].last;
		}
		NMax = BES->HeaderEnd - LastHdrEnd;
		if (NMax > 0) {	/* send the last piece */
			if (TMDebug) fprintf(stderr, "Sending first %d chars of ``%s''.\n",
					NMax, LastHdrEnd);
			SuccCode = fwriteallchars(LastHdrEnd, NMax, OutF);
			if (SuccCode <= 0) return -1;
		}
		CharsRead = BES->HeaderEnd - BES->Header;
		if (CharsRead < InFileStat.st_size) {
		    if (fseek(InFile, (long) CharsRead, 0) < 0) return -1;
		    for (;;) {
			N = fread(TextBuff, 1, TextBuffSize, InFile);
			if (N <= 0) {
				if (ferror(InFile)) return -1;
				break;
			}
			CharsRead += N;
			TextBuff[N] = '\0';
			if (BES->DoUnScribe) {
			    if (TMDebug) fprintf(stderr,
				"Unscribing ``%s''.\n", TextBuff);
			    N = UnScribe(BES->UnScribeCode, &BES->UnScribeState,
				TextBuff, N, OutF);
			    if (N < 0) return N;
			} else {
			    if (TMDebug) fprintf(stderr,
				"Mailing verbatim ``%s''.\n", TextBuff);
			    SuccCode = fwriteallchars(TextBuff, N, OutF);
			    if (SuccCode <= 0) return -1;
			}
		    }
		}
	} else {	/* Write message verbatim (no unscribing) */
		rewind(InFile);
		CharsRead = 0;
		for (;;) {
			NMax = fread(TextBuff, 1, TextBuffSize, InFile);
			if (NMax <= 0) {
				if (ferror(InFile)) return -1;
				break;
			}
			CharsRead += NMax;
			TextBuff[NMax] = '\0';
			if (TMDebug) fprintf(stderr,
				"Mailing verbatim ``%s''.\n", TextBuff);
			SuccCode = fwriteallchars(TextBuff, NMax, OutF);
			if (SuccCode <= 0) return -1;
		}
	}
	*refCharsRead = CharsRead;
	return 0;
}

static char *cvEng(foo)
int foo;
{/* Returns pointer to static storage describing the integer foo.  For small integers, this is "zero", "one", and so forth; for integers outside the range, it's a pointer to overwritable static storage. */
	static char *Nms[] = {
		"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
		"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
		"seventeen", "eighteen", "nineteen", "twenty"};
#define NmSiz	(sizeof(Nms) / sizeof(Nms[0]))
	static char NmBuff[12];

	if (foo >= 0 && foo < NmSiz) return Nms[foo];
	sprintf(NmBuff, "%d", foo);
	return NmBuff;
}

static char *PostmanAddress = NULL;
static int SetPostmanAddress()
{/* Try to set the PostmanAddress cell. */
    char *pmName;
	if (PostmanAddress == NULL) {
	    pmName = CheckAMSPMName(GlobalDom);
	    if (pmName == NULL)	return 0;   /* Sorry, can't get it. */
		PostmanAddress = malloc(strlen(pmName) + strlen(GlobalDom) + 3);
		if (PostmanAddress == NULL) return 0;
		sprintf(PostmanAddress, "%s+@%s", pmName, GlobalDom);
	}
	return 1;
}

static char MsgBf[2000];

static int OpenEnclosure(fp, besP, lpsP, lineBuf, sizeLineBuf, whereGoP)
FILE *fp; struct BEStripper **besP; struct LinePromState **lpsP;
char *lineBuf; int sizeLineBuf, *whereGoP;
{/* Returns 0 if all OK, numbers between -30 and -36 on failures. */
	int Code, WasBOL;
	char *MsgHdr;
	struct BEStripper *bes = NULL;
	struct LinePromState *lps;

	*besP = NULL;
	*lpsP = NULL;
	lineBuf[0] = '\0';
	*whereGoP = 0;	/* default the values */
	Code = GetHeader(&MsgHdr);
	if (Code != 0) {fputc('\n', fp); return -30;}
	Code = OpenStripBE(&bes, MsgHdr, Normal);
	if (Code != 0) {
		fputc('\n', fp);
		free(MsgHdr); CloseStripBE(bes);
		if (Code > 0) return -31;	/* No memory */
		else return -32;		/* Permanent: OSBE_Brief and OSBE_Text. */
	}
/* We now have the BEStripper for the message.  What we really need to do is copy the formatting lines to the message we're generating. */
	if (bes->ctLineBegin != NULL) {
		fwriteallchars(bes->ctLineBegin, (bes->ctLineEnd - bes->ctLineBegin), fp);
		fputc('\n', fp);
/* Copy the If-type-unsupported: line from the rejected message */
		if (bes->ituLineBegin != NULL) {
		    fwriteallchars(bes->ituLineBegin, (bes->ituLineEnd - bes->ituLineBegin), fp);
		    fputc('\n', fp);
		} else {
		    fputs("If-Type-Unsupported: alter\n", fp);
		}
	}
	if (bes->xasfLineBegin != NULL) {
		fwriteallchars(bes->xasfLineBegin, (bes->xasfLineEnd - bes->xasfLineBegin), fp);
		fputc('\n', fp);
	}
	fputc('\n', fp);	/* Wrap up the header of the rejection message */
	Code = strlen(MsgHdr);
	*whereGoP = Code;
	if (fseek(InFile, Code, 0) == -1) {free(MsgHdr); CloseStripBE(bes); return -33;}
/* Now, for BE2 Datastream, see if we need to promote any prologue. */
	if (bes->UnScrValue != NULL && atoi(bes->UnScrValue) >= 10) {
	    WasBOL = 1;
	    if (BE2LinePromoteInit(&lps) != 0) {free(MsgHdr); CloseStripBE(bes); return -34;}
	    while (fgets(lineBuf, sizeLineBuf, InFile) != NULL) {
		if (WasBOL) {
			if (BE2LinePromote(lineBuf, lps) != 2) break;
		}
		Code = strlen(lineBuf);
		fwriteallchars(lineBuf, Code, fp);
		WasBOL = lineBuf[Code-1] == '\n';
		lineBuf[0] = '\0';
	    }
	    *lpsP = lps;
	    *whereGoP = ftell(InFile);
	}
	*besP = bes;
	return 0;
}

static int CloseEnclosure(fp, title, bes, lps, lineBuf, sizeLineBuf, whereGo)
FILE *fp; char *title; struct BEStripper *bes; struct LinePromState *lps;
char *lineBuf; int sizeLineBuf, whereGo;
{
	int C, WasBOL;
	static char sepchars[] = "\n----------------\n";

	sprintf(MsgBf, "%s%s", title, sepchars);
	PrintMaybeFoldQuotingFormatting(fp, MsgBf,
			(bes == NULL ? NULL : bes->UnScrValue),
			strlen(MsgBf), 1);
	if (bes != NULL) {
		PrintMaybeFoldQuotingFormatting(fp, bes->Header,
			bes->UnScrValue, strlen(bes->Header), 1);
		free(bes->Header);
		CloseStripBE(bes);
	}
	C = strlen(lineBuf);
	fwriteallchars(lineBuf, C, fp);
	fseek(InFile, whereGo, 0);
	if (lps != NULL) {
	    WasBOL = lineBuf[C - 1] == '\n';
	    while (fgets(lineBuf, sizeLineBuf, InFile) != NULL) {
		if (WasBOL) {
			if (BE2LinePromote(lineBuf, lps) != 0) break;
		}
		C = strlen(lineBuf);
		fwriteallchars(lineBuf, C, fp);
		WasBOL = lineBuf[C - 1] == '\n';
		lineBuf[0] = '\0';
	    }
	} else for (;;) {
		C = getc(InFile);
		if (C == EOF) break;
		(void) putc(C, fp);
	}
	PrintMaybeFoldQuotingFormatting(fp, sepchars,
			(bes == NULL ? NULL : bes->UnScrValue),
			sizeof(sepchars) - 1, 1);
	if (lps != NULL) {
	    fwriteallchars(lineBuf, strlen(lineBuf), fp);
	    for (;;) {
		C = getc(InFile);
		if (C == EOF) break;
		(void) putc(C, fp);
	    }
	    BE2LinePromoteEnd(lps);
	}
	return 0;
}

static int AdvReallyEnclose;
static char AdvFileName[128];
static FILE *AdvF;
static struct BEStripper *AdvBES;
static struct LinePromState *AdvLPS;
static char AdvLine[200], *AdvFmt;
static int AdvGo;

FILE *OpenPMAdvisory(Subj, EncloseMsg, Addr, Dom)
char *Subj; int EncloseMsg; PARSED_ADDRESS *Addr; char *Dom;
{/* Begin to return an advisory message to the postmaster.  If EncloseMsg is true, include the sender's original message in the error message.  Consult various globals for the sender's identity and information about the file containing the queued message.  Subj has *no* trailing newline.
Return a stdio FILE* on which the message text should be written, or NULL for error conditions.
 */
	int OutD, Stat2, AdvUnPOK; long int LongStat;
	char *UnPDest, *AdvBegin, *SDum;
	char MsgID[40];

#if Logs
	Log(300, "OpenPMAdvisory(%s, ...) called.", Subj);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "OpenPMAdvisory(``%s'' ...) called.\n", Subj);
	AdvReallyEnclose = EncloseMsg;
	if (SetPostmanAddress() == 0) return NULL;
	if (EncloseMsg && (InFile == NULL || InFileStat.st_nlink <= 0)) AdvReallyEnclose = FALSE;

	AdvUnPOK = -1; Stat2 = -1000;
	if (ParsedRetPath != NULL) AdvUnPOK = DoUnparse(ParsedRetPath, &Stat2, FALSE);
	if (AdvUnPOK != 2 && Stat2 == PA_NO_MEM) { /* temp fail in unparse */
		return NULL;
	}
	AdvBegin = NULL;
	if (AdvUnPOK == 2) {
		AdvBegin = &UnparseBuff[0];
		if (UnparseBuff[0] == '<' && UnparseBuff[strlen(UnparseBuff)-1] == '>') {
			if (index(UnparseBuff, ':') == NULL) {
				UnparseBuff[strlen(UnparseBuff)-1] = '\0';
				AdvBegin = &UnparseBuff[1];
			}
		}
	}

	(void) sprintf(AdvFileName, "/tmp/%s", ams_genid(1));
	OutD = open(AdvFileName, O_WRONLY|O_CREAT|O_TRUNC, 0600);
	if (OutD < 0) return NULL;
	AdvF = fdopen(OutD, "w");
	if (AdvF == NULL) {unlink(AdvFileName); close(OutD); return NULL;}
	fprintf(AdvF, "Date: %s", arpadate());
	fprintf(AdvF, "From: Mailer-daemon <%s>\n", PostmanAddress);
	fprintf(AdvF, "Subject: Advisory");
	if (Subj != NULL) fprintf(AdvF, ": %s", Subj);
	fprintf(AdvF, "\nTo: %s", PostmanAddress);
	strncpy(MsgID, ams_genid(0), sizeof(MsgID));
	for (SDum = MsgID; *SDum != '\0'; ++SDum) if (*SDum == ':') *SDum = '_';
	fprintf(AdvF, "\nMessage-ID: <%s@%s>\n", MsgID, GlobalDom);
	AdvBES = NULL;
	AdvLPS = NULL;
	AdvLine[0] = '\0';
	AdvGo = 0;
	if (AdvReallyEnclose) {
		Stat2 = OpenEnclosure(AdvF, &AdvBES, &AdvLPS,
				AdvLine, sizeof(AdvLine), &AdvGo);
		/* 0 is all-OK, -32 is permanent unrecognizable content-type */
		if (Stat2 != 0 && Stat2 != -32) {
			unlink(AdvFileName); fclose(AdvF); return NULL;
		}
			
	} else fputc('\n', AdvF);
	if (AdvBES != NULL) AdvFmt = AdvBES->UnScrValue;
	else AdvFmt = NULL;

	if (TMDebug) fprintf(stderr, "OpenPMAdvisory: header composed.\n");
/* Include origin in mail */
	if (AdvUnPOK != 2 && ParsedRetPath != NULL) {
		fputs("Return path: ``", AdvF);
		CheapPrint(AdvF, ParsedRetPath);
		UnPDest = "''\n";
		PrintMaybeFoldQuotingFormatting(AdvF, UnPDest, AdvFmt, strlen(UnPDest), 1);
	} else {
		sprintf(MsgBf, "Return path: ``%s''\n",
		    (AdvUnPOK == 2 ? AdvBegin :
			(TextRetPath != NULL && TextRetPath[0] != '\0'
					? TextRetPath
					: "<nobody>")));
		PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
	}
/* done trying to include origin */
	if (GlobalForString != NULL) {
		sprintf(MsgBf, "For: ``%s''\n", GlobalForString);
		PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
	}
	sprintf(MsgBf, "In cell: ``%s''\n", Dom);
	PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
	if (Addr == NULL && TextDestinations != NULL) {
		sprintf(MsgBf, "Destinations: ``%s''\n\n", TextDestinations);
		PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
	}
	if (Addr != NULL) {
		UnPDest = AddrUnparse(Addr, &LongStat);
		if (UnPDest != NULL) {
			sprintf(MsgBf, "Current destination: ``%s''\n\n", UnPDest);
			PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
			free(UnPDest);
		} else {
			fputs("Current destination: ``", AdvF);
			CheapPrint(AdvF, Addr);
			UnPDest = "''\n\n";
			PrintMaybeFoldQuotingFormatting(AdvF, UnPDest, AdvFmt, strlen(UnPDest), 1);
		}
	}

	if (ferror(InFile) || ferror(AdvF) || feof(AdvF)) {
		if (AdvBES != NULL) {free(AdvBES->Header); CloseStripBE(AdvBES);}
		if (AdvLPS != NULL) BE2LinePromoteEnd(AdvLPS);
		unlink(AdvFileName); fclose(AdvF); return NULL;
	}
	if (TMDebug) fprintf(stderr, "OpenPMAdvisory returning all-OK.\n");
#if Logs
	Log(350, "OpenPMAdvisory returning normally.");
#endif /* Logs */
	return AdvF;
}

/*VARARGS*/
void PMAdvPrint(Fmt, p1, p2, p3, p4, p5, p6)
char *Fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
	sprintf(MsgBf, Fmt, p1, p2, p3, p4, p5, p6);	/* *sigh* ignore overflow */
	MsgBf[sizeof(MsgBf)-1] = '\0';		/* insurance */
	PrintMaybeFoldQuotingFormatting(AdvF, MsgBf, AdvFmt, strlen(MsgBf), 1);
}

int ClosePMAdvisory()
{/* Finish returning an error message to the sender of a piece of mail.
Return 0 for all OK (error message composed and sent), and non-zero for error conditions.
 */
	int WrOK, Stat2, C, TimedOut;
	char *Argv[16];
	int Argc;

	if (AdvF == NULL) return -12;
#if Logs
	Log(400, "ClosePMAdvisory called");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "ClosePMAdvisory() called.\n");
	WrOK = TRUE;
	if (AdvReallyEnclose && !ferror(AdvF)) {
		CloseEnclosure(AdvF, "The undelivered message follows:",
				AdvBES, AdvLPS, AdvLine, sizeof(AdvLine), AdvGo);
		if (ferror(InFile)) WrOK = FALSE;
	}
	if (fflush(AdvF) < 0) {
		WrOK = FALSE;
	}
	if (ferror(AdvF) || feof(AdvF)) WrOK = FALSE;
	if (fclose(AdvF) < 0) {
		WrOK = FALSE;
	}
	if (! WrOK) {unlink(AdvFileName); return -5;}

	if (TMDebug) fprintf(stderr, "Message composed.\n");

/* Now the mail is composed; let's send it. */
	Argc = -1;
	Argv[++Argc] = queuemail;
	if (TMDebug) {Argv[++Argc] = "-D"; Argv[++Argc] = "-5";}
	Argv[++Argc] = "-f";
	Argv[++Argc] = AdvFileName;
	Argv[++Argc] = "-r";
	Argv[++Argc] = PostmanAddress;
	if (homeCell != NULL) {
		Argv[++Argc] = "-Ch";	/* set home cell */
		Argv[++Argc] = homeCell;
	}
	Argv[++Argc] = "-T";	/* don't generate massive PM messages on full disks */
	Argv[++Argc] = "-a";	/* following is an address, even if starts with hyphen */
	Argv[++Argc] = PostmanAddress;
	Argv[++Argc] = NULL;
	if (TMDebug) {
		fprintf(stderr, "Sending advisory with command: %s", Argv[0]);
		for (C = 1; Argv[C] != NULL; C++) fprintf(stderr, ", %s", Argv[C]);
		fprintf(stderr, "\n");
	}
#if Logs
	Log(410, "ClosePMAdvisory about to topen(queuemail)");
#endif /* Logs */
	AdvF = topen(Argv[0], Argv, "w", &Stat2);
#if Logs
	Log(411, "topen(queuemail) returns 0x%x", AdvF);
#endif /* Logs */
	if (AdvF == NULL) {unlink(AdvFileName); return -7;}
			/* nothing to send, really */
#if Logs
	Log(420, "ClosePMAdvisory about to tclose(queuemail)");
#endif /* Logs */
	C = tclose(AdvF, 10*60, &TimedOut);	/* ten minute timeout on mail dropoff */
	AdvF = NULL;
#if Logs
	Log(421, "tclose(queuemail) returns %d, TimedOut %d", C, TimedOut);
#endif /* Logs */
	if (TimedOut) {killpg(Stat2, SIGKILL); unlink(AdvFileName); return -8;}
	unlink(AdvFileName);
#if Logs
	Log(422, "File %s unlinked; returning.  C=%d", AdvFileName, C);
#endif /* Logs */
	return C;
}

static int NastyFailure, AllOK, ReallyEnclose, RetUnPOK, PostmanInvolvement;
static char *RetBegin, *RetDummy;
static char RetFileName[128];
static FILE *RetF;
static struct BEStripper *RetBES;
static struct LinePromState *RetLPS;
static char RetLine[200], *RetFmt;
static int RetGo;

static int OpenReturnError(Subj, EncloseMsg, ToPostman, Addr, Dom)
char *Subj; int EncloseMsg, ToPostman; PARSED_ADDRESS *Addr; char *Dom;
{/* Begin to return an error message to the sender of a piece of mail.  If EncloseMsg is true, include the sender's original message in the error message.  If ToPostman is 0, don't involve postman; if it's 1, send the report only to the postman; if it's 2, send it both to postman and to the sender.  Consult various globals for the sender's identity and information about the file containing the queued message.  Subj has *no* trailing newline.
Return 0 for all OK (error message composed and sent), and non-zero for error conditions.
 */
	int OutD, Stat2; long int LongStat;
	char *UnPDest, *SDum;
	char MsgID[40];

#if Logs
	Log(300, "OpenReturnError(%s, ...) called.", Subj);
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "OpenReturnError(``%s'' ...) called.\n", Subj);
	if ((GlobalOptions & tmopt_ReturnErrors) == 0 || ULstrcmp(Dom, WorkstationCell) != 0) {
		ReturnTempFail = TRUE;
		AllOK = FALSE;
		return -2;
	}
	AllOK = TRUE;
	NastyFailure = FALSE;
	ReturnTempFail = FALSE;
	ReallyEnclose = EncloseMsg;
	PostmanInvolvement = ToPostman;
	if (SetPostmanAddress() == 0) {ReturnTempFail = TRUE; AllOK = FALSE; return -16;}
	if (EncloseMsg && (InFile == NULL || InFileStat.st_nlink <= 0))
		{NastyFailure = TRUE; AllOK = FALSE; ReallyEnclose = FALSE;}

	RetUnPOK = -1; Stat2 = -1000;
	if (ParsedRetPath != NULL) RetUnPOK = DoUnparse(ParsedRetPath, &Stat2, FALSE);
	if (RetUnPOK != 2 && Stat2 == PA_NO_MEM) { /* temp fail in unparse */
		ReturnTempFail = TRUE;
		AllOK = FALSE;
		return -2;
	}
	if (RetUnPOK != 2 && (TextRetPath == NULL || strcmp(TextRetPath, "<>") != 0))
		AllOK = FALSE;
	RetBegin = NULL;
	RetDummy = "";		/* a pointer, but to a null string */
	if (RetUnPOK == 2) {
		RetBegin = &UnparseBuff[0];
		if (UnparseBuff[0] == '<' && UnparseBuff[strlen(UnparseBuff)-1] == '>') {
			if (index(UnparseBuff, ':') == NULL) {
				UnparseBuff[strlen(UnparseBuff)-1] = '\0';
				RetBegin = &UnparseBuff[1];
			} else RetDummy = "message sender ";
		}
	}

	(void) sprintf(RetFileName, "/tmp/%s", ams_genid(1));
	OutD = open(RetFileName, O_WRONLY|O_CREAT|O_TRUNC, 0600);
	if (OutD < 0) {
		if (tfail(errno)) ReturnTempFail = TRUE;
		return -3;
	}
	RetF = fdopen(OutD, "w");
	if (RetF == NULL) {unlink(RetFileName); close(OutD); ReturnTempFail=TRUE; return -4;}
	fprintf(RetF, "Date: %s", arpadate());
	fprintf(RetF, "From: Mailer-daemon <%s> (%s, %s)\n", PostmanAddress, PostmasterTitle, Organization);
	fprintf(RetF, "Subject: %s mail", (ReallyEnclose ? "Returned" : "Undeliverable"));
	if (Subj != NULL) fprintf(RetF, ": %s", Subj);
	fputs("\nTo: ", RetF);
	if (PostmanInvolvement == 1) fputs(PostmanAddress, RetF);
	else {
/* Include origin in mail */
		if (RetUnPOK == 2) {fputs(RetDummy, RetF); fputs(RetBegin, RetF);}
		else if (ParsedRetPath != NULL) CheapPrint(RetF, ParsedRetPath);
		else if (TextRetPath != NULL && TextRetPath[0] != '\0')
					fputs(TextRetPath, RetF);
		else fputs("<unknown>", RetF);
	}
/* done trying to include origin */
	if (PostmanInvolvement == 2) fprintf(RetF, "\nCC: %s", PostmanAddress);
	strncpy(MsgID, ams_genid(0), sizeof(MsgID));
	for (SDum = MsgID; *SDum != '\0'; ++SDum) if (*SDum == ':') *SDum = '_';
	fprintf(RetF, "\nMessage-ID: <%s@%s>\n", MsgID, GlobalDom);
	if (*Organization != '\0') fprintf(RetF, "Organization: %s\n", Organization);
	if (*DefaultSurfaceAddress != '\0' && strcmp(DefaultSurfaceAddress, "Anytown, Anywhere") != 0)
	    fprintf(RetF, "Postal-address: %s\n", DefaultSurfaceAddress);
	RetBES = NULL;
	RetLPS = NULL;
	RetLine[0] = '\0';
	RetGo = 0;
	if (ReallyEnclose) {
		Stat2 = OpenEnclosure(RetF, &RetBES, &RetLPS,
			RetLine, sizeof(RetLine), &RetGo);
		/* 0 is all-OK, -32 is permanent unrecognizable content-type */
		if (Stat2 != 0 && Stat2 != -32) {
			ReturnTempFail = TRUE; AllOK = FALSE;
			unlink(RetFileName); fclose(RetF); return Stat2;
		}
		if (Stat2 == -32) AllOK = FALSE;

	} else fputc('\n', RetF);
	if (RetBES != NULL) RetFmt = RetBES->UnScrValue;
	else RetFmt = NULL;

	if (TMDebug) fprintf(stderr, "OpenReturnError: header composed.\n");
	sprintf(MsgBf, "%s from ``", (ReallyEnclose ? "The following message" : "A message"));
/* Include origin in mail */
	if (RetUnPOK != 2 && ParsedRetPath != NULL) {
		PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
		MsgBf[0] = '\0';
		CheapPrint(RetF, ParsedRetPath);
		UnPDest = "''\n";
		PrintMaybeFoldQuotingFormatting(RetF, UnPDest, RetFmt, strlen(UnPDest), 1);
	} else {
		if (RetUnPOK == 2) strcat(MsgBf, RetBegin);
		else if (TextRetPath != NULL && TextRetPath[0] != '\0')
				strcat(MsgBf, TextRetPath);
		else strcat(MsgBf, "<nobody>");
	}
/* done trying to include origin */
	strcat(MsgBf, "''");
	if (Addr == NULL && TextDestinations != NULL) {
		strcat(MsgBf, " and addressed to ``");
		strcat(MsgBf, TextDestinations);
		strcat(MsgBf, "''");
	}
	if (GlobalForString != NULL) {
		strcat(MsgBf, " for ``");
		strcat(MsgBf, GlobalForString);
		strcat(MsgBf, "''");
	}
	if (Addr == NULL && TextDestinations == NULL) NastyFailure = TRUE;
	strcat(MsgBf, " could not be delivered");
	if (Addr != NULL) {
		UnPDest = AddrUnparse(Addr, &LongStat);
		if (UnPDest != NULL) {
			strcat(MsgBf, " to ``");
			strcat(MsgBf, UnPDest); free(UnPDest);
		} else {
			strcat(MsgBf, " to\n``");
			PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
			MsgBf[0] = '\0';
			CheapPrint(RetF, Addr);
		}
		strcat(MsgBf, "''");
	}

	if (ferror(RetF) || feof(RetF)) {
		if (RetBES != NULL) {free(RetBES->Header); CloseStripBE(RetBES);}
		if (RetLPS != NULL) BE2LinePromoteEnd(RetLPS);
		unlink(RetFileName); fclose(RetF); return -10;
	}
	if (TMDebug) fprintf(stderr, "OpenReturnError returning all-OK.\n");
#if Logs
	Log(350, "OpenReturnError returning normally.");
#endif /* Logs */
	return 0;
}

static int CloseReturnError()
{/* Finish returning an error message to the sender of a piece of mail.
Return 0 for all OK (error message composed and sent), and non-zero for error conditions.
 */
	int WrOK, Stat2, C, TimedOut;
	char *Argv[16];
	int Argc, AnyQueued;

	if (RetF == NULL) return -12;
#if Logs
	Log(400, "CloseReturnError called");
#endif /* Logs */
	if (TMDebug) fprintf(stderr, "CloseReturnError() called.\n");
	WrOK = TRUE;
	if (ReallyEnclose && !ferror(RetF)) {
		CloseEnclosure(RetF, "The undelivered message follows:",
				RetBES, RetLPS, RetLine, sizeof(RetLine), RetGo);
		if (ferror(InFile)) AllOK = WrOK = FALSE;
	}
	if (fflush(RetF) < 0) {
		AllOK = WrOK = FALSE;
		if (errno == ENOSPC) ReturnTempFail = TRUE;
	}
	if (ferror(RetF) || feof(RetF)) AllOK = WrOK = FALSE;
	if (fclose(RetF) < 0) {
		AllOK = WrOK = FALSE;
		if (errno == ENOSPC) ReturnTempFail = TRUE;
	}
	if (! WrOK) {unlink(RetFileName); return -5;}

	if (TMDebug) fprintf(stderr, "Message composed.\n");

/* Now the mail is composed; let's send it. */
	Argc = -1;
	Argv[++Argc] = queuemail;
	if (TMDebug) {Argv[++Argc] = "-D"; Argv[++Argc] = "-5";}
	Argv[++Argc] = "-f";
	Argv[++Argc] = RetFileName;
	Argv[++Argc] = "-r";
	if (AllOK)		/* someday just "<>" */
		Argv[++Argc] = PostmanAddress;
	else
		Argv[++Argc] = PostmanAddress;
	if (homeCell != NULL) {
		Argv[++Argc] = "-Ch";	/* set home cell */
		Argv[++Argc] = homeCell;
	}
	Argv[++Argc] = "-T";	/* don't generate massive PM messages on full disks */
	Argv[++Argc] = "-a";	/* following is an address, even if starts with hyphen */
	AnyQueued = 0;
	if (PostmanInvolvement != 1) {	/* send to sender unless just postman */
	    if (TextRetPath == NULL || strcmp(TextRetPath, "<>") != 0) {
		if (RetUnPOK == 2)
			Argv[++Argc] = RetBegin;
		else if (TextRetPath != NULL && TextRetPath[0] != '\0')
			Argv[++Argc] = TextRetPath;
		else {unlink(RetFileName); return -6;}
		AnyQueued = 1;
	    }
	}
	if (PostmanInvolvement != 0) {	/* send to postman unless just sender */
		Argv[++Argc] = PostmanAddress;
		AnyQueued = 1;
	}
	if (AnyQueued == 0) {unlink(RetFileName); return 0;}
	Argv[++Argc] = NULL;
	if (TMDebug) {
		fprintf(stderr, "Returning mail with command: %s", Argv[0]);
		for (C = 1; Argv[C] != NULL; C++) fprintf(stderr, ", %s", Argv[C]);
		fprintf(stderr, "\n");
	}
#if Logs
	Log(410, "CloseReturnError about to topen(queuemail)");
#endif /* Logs */
	RetF = topen(Argv[0], Argv, "w", &Stat2);
#if Logs
	Log(411, "topen(queuemail) returns 0x%x", RetF);
#endif /* Logs */
	if (RetF == NULL) {unlink(RetFileName); return -7;}
			/* nothing to send, really */
#if Logs
	Log(420, "CloseReturnError about to tclose(queuemail)");
#endif /* Logs */
	C = tclose(RetF, 10*60, &TimedOut);	/* ten minute timeout on mail dropoff */
#if Logs
	Log(421, "tclose(queuemail) returns %d, TimedOut %d", C, TimedOut);
#endif /* Logs */
	if (TimedOut) {killpg(Stat2, SIGKILL); unlink(RetFileName);
			ReturnTempFail = TRUE; return -8;}
	unlink(RetFileName);
#if Logs
	Log(422, "File %s unlinked; returning.  C=%d, NastyFailure=%d",
			RetFileName, C, NastyFailure);
#endif /* Logs */
	if (C == 0) {
		if (NastyFailure)
			return -11;
		else
			return 0;
	}
	if ((C & 0xff) != 0) switch (C & 0x7f) {	/* decode signals */
			case SIGBUS:
			case SIGSEGV:
			case SIGHUP:
			case SIGFPE:
#ifdef SIGXCPU
			case SIGXCPU:
#endif /* SIGXCPU */
#ifdef SIGUSR1
			case SIGUSR1:
#endif /* SIGUSR1 */
			    ReturnTempFail = TRUE;
			}
	else switch (C >> 8) {
		case 1:		/* we get this when queuemail can't be executed at all */
		case EX_TEMPFAIL:
		    ReturnTempFail = TRUE;
		}
	return C;
}

/*VARARGS4*/
int ReturnError(Subj, EncloseMsg, ToPostman, Addr, Fmt, p1, p2, p3, p4, p5)
char *Subj; int EncloseMsg, ToPostman; char *Fmt; PARSED_ADDRESS *Addr;
char *p1, *p2, *p3, *p4, *p5;
{/* Return an error message to the sender of a piece of mail.  If EncloseMsg is true, include the sender's original message in the error message.  If ToPostman is 0, don't involve postman; if it's 1, send the report only to the postman; if it's 2, send it both to postman and to the sender.  Consult various globals for the sender's identity and information about the file containing the queued message.  Fmt and p1 thru p5 make up a printf set of arguments describing the reason for returning the mail.  Subj has *no* trailing newline.
Return 0 for all OK (error message composed and sent), and non-zero for error conditions.
 */
	int C;

	C = OpenReturnError(Subj, EncloseMsg, ToPostman, Addr, GlobalDom);
	if (C != 0) return C;

	strcat(MsgBf, " because:\n");
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
	sprintf(MsgBf, Fmt, p1, p2, p3, p4, p5);
	strcat(MsgBf, "\n\n");
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);

	return CloseReturnError();
}

static char *MatchAdjective(MatchLevel)
int MatchLevel;
{/* Return a pointer to a string describing the level of the WP match given by MatchLevel. */
	if (MatchLevel < MatchExPA) return NULL;
	else if (MatchLevel < MatchAnyOv) return "approximate";
	else return "distant";
}

static char *MatchAdverb(MatchLevel)
int MatchLevel;
{/* Return a pointer to a string describing the level of the WP match given by MatchLevel. */
	char *Adj;
	static char Adv[30];

	Adj = MatchAdjective(MatchLevel);
	if (Adj == NULL) return "";
	(void) sprintf(Adv, " %sly", Adj);
	return Adv;
}

static char *MatchAn(MatchLevel)
int MatchLevel;
{/* Return a pointer to a string describing the level of the WP match given by MatchLevel. */
	char *Adj, *Article;
	static char An[30];

	Adj = MatchAdjective(MatchLevel);
	if (Adj == NULL) return "a";
	if (*Adj == 'a' || *Adj == 'e' || *Adj == 'i' || *Adj == 'o' || *Adj == 'u') Article = "an";
	else Article = "a";
	sprintf(An, "%s %s", Article, Adj);
	return An;
}

int ReturnAmbig(Addr, wpCD, STok, ToPostman, MinMatch, OutMatch, Dom)
PARSED_ADDRESS *Addr; struct wp_cd *wpCD; wp_SearchToken STok;
int ToPostman, MinMatch, OutMatch; char *Dom;
{/* Compose an error message about an ambiguous addressee.  Return zero for all-OK, non-zero for various error conditions.  Addr is the ambiguous addressee, STok the White Pages search token that produced the ambiguous result, and MinMatch and OutMatch the relevant results of the wp_Lookup: a lower bound for the number of matches for the token and the ambitiousness of the match that had to occur before any match was discovered.  If ToPostman is 0, don't involve postman; if it's 1, send the report only to the postman; if it's 2, send it both to postman and to the sender.
*/
    int C, OurMatchQuality, ThisIx;
#ifdef CMU_ENV
    int WasIDMatch, DoIDWarn;
#endif /* CMU_ENV */
    wp_PrimeKeySet *PKs;
    wp_ErrorCode wperr;
    char *Value, *Src, *Dst;
    FILE *Adv;
#define LONGNAME 70
    char NameCopy[LONGNAME+1], TempCopy[LONGNAME+1];
    char *PKCopy;
    /* MOSTMATCHES is the biggest set of Prime Keys we'll return for a given probe */
#define MOSTMATCHES 10

    C = OpenReturnError("Ambiguous address", TRUE, ToPostman, Addr, Dom);
    if (C != 0) return C;
    strcat(MsgBf, " because the local address ``");
    strcat(MsgBf, Addr->LocalPart);
    strcat(MsgBf, "'' is ambiguous in domain ");
    strcat(MsgBf, Dom);
    if (*Organization != '\0') {
	strcat(MsgBf, " (");
	strcat(MsgBf, Organization);
	strcat(MsgBf, ")");
    }

#ifdef CMU_ENV
    DoIDWarn = 1; WasIDMatch = 0;
#endif /* CMU_ENV */
    wperr = cwp_Search(wpCD, STok, MOSTMATCHES, MatchAll, &OurMatchQuality, &PKs);
    if (! gotWPIndices) WantWPIndices();
    if (! gotWPIndices ||
	 (wperr != wperr_NoError && wperr != wperr_TooManyKeysFound)) {
	strcat(MsgBf, " and");
	strcat(MsgBf, MatchAdverb(OutMatch));
	strcat(MsgBf, " matches at least ");
	strcat(MsgBf, cvEng(MinMatch));
	strcat(MsgBf, " users.  (Unfortunately, while trying to enumerate the matching users, ");
	if (wperr != wperr_NoError && wperr != wperr_TooManyKeysFound) {
	    strcat(MsgBf, "the error ``");
	    strcat(MsgBf, wp_ErrorString(wperr));
	    strcat(MsgBf, "'' occurred.)\n");
	} else {
	    strcat(MsgBf, "the necessary white pages indices could not be found.)\n");
	}
    } else {
	strcat(MsgBf, " and");
	strcat(MsgBf, MatchAdverb(OutMatch));
	if (wperr != wperr_NoError) {
	    strcat(MsgBf, " matches over ");
	    strcat(MsgBf, cvEng(MOSTMATCHES));
	    strcat(MsgBf, " users.\n");
	} else {
	    strcat(MsgBf, " matches the ");
	    strcat(MsgBf, cvEng(PKs->KeyCount));
	    strcat(MsgBf, " users who follow:\n");
	}
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
	if (OutMatch > MatchNoHeuristics) {
	    Adv = OpenPMAdvisory("Heuristic ambiguity", 0, Addr, Dom);
	    if (Adv != NULL) {
		PMAdvPrint("White pages match was %d: %s match of %s%s names.\n\n",
			   OutMatch, MatchAn(OutMatch),
			   (wperr == wperr_NoError ? "" : "over "),
			   cvEng(PKs->KeyCount));
		ClosePMAdvisory();
	    }
	}
	MsgBf[0] = '\0';
#ifdef CMU_ENV
	DoIDWarn = 0;
#endif /* CMU_ENV */
	if (wperr == wperr_NoError) {	/* don't return results for very ambiguous matches */
	    for (C = 0; C < PKs->KeyCount; C++) {
		PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
		wperr = cwp_Read(wpCD, PKs->Keys[C], idxN, &Value);
		if (wperr != wperr_NoError) {
#ifdef CMU_ENV
		    DoIDWarn = 1;
#endif /* CMU_ENV */
		    sprintf(MsgBf, "\tunreadable key <<%s>>: %s\n",
			    PKs->Keys[C], wp_ErrorString(wperr));
		    continue;
		}
		sprintf(MsgBf, "\t%s", Value);
		ThisIx = strlen(Value); TempCopy[0] = '\0';
		if (ThisIx <= LONGNAME) strcpy(TempCopy, Value);
		wperr = cwp_Read(wpCD, PKs->Keys[C], idxID, &Value);
		if (wperr != wperr_NoError) {
		    strcat(MsgBf, "@");
		    strcat(MsgBf, Dom);
		    if (wperr != wperr_NoSuchField) {
			strcat(MsgBf, " (unreadable key: ");
			strcat(MsgBf, wp_ErrorString(wperr));
			strcat(MsgBf, ")");
		    }
		    strcat(MsgBf, "\n");
#ifdef CMU_ENV
		    DoIDWarn = 1;
#endif /* CMU_ENV */
		    continue;
		}
		if (ULstrcmp(Addr->LocalPart, Value) == 0) {
#ifdef CMU_ENV
		    WasIDMatch = 1;
#endif /* CMU_ENV */
		    strcpy(NameCopy, TempCopy);
		    PKCopy = PKs->Keys[C];
		}
		strcat(MsgBf, " <");
		strcat(MsgBf, Value);
		strcat(MsgBf, "+@");
		strcat(MsgBf, Dom);
		strcat(MsgBf, ">");
		wperr = cwp_Read(wpCD, PKs->Keys[C], idxAf, &Value);
		if (wperr == wperr_NoError) {
		    strcat(MsgBf, " (");
		    strcat(MsgBf, Value);
		    strcat(MsgBf, ")");
		}
		wperr = cwp_Read(wpCD, PKs->Keys[C], idxFwd, &Value);
		if (wperr == wperr_NoError) {
		    strcat(MsgBf, " (fwd addr: ");
		    Dst = &MsgBf[strlen(MsgBf)];
		    for (Src = Value; *Src != '\0'; ++Src) {
			*Dst++ = (isprint(*Src) ? *Src : ' ');
		    }
		    *Dst++ = ')';
		    *Dst++ = '\0';
		}
		strcat(MsgBf, "\n");
	    }
	}
    }
    strcat(MsgBf, "\n");
    PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
#ifdef CMU_ENV
    if ((DoIDWarn || WasIDMatch) && ULstrcmp(Dom, GlobalDom) == 0) {
	sprintf(MsgBf, "(Local mail addressing at %s is such that mail to a userid ", Dom);
	if (WasIDMatch) {
	    strcat(MsgBf, "(e.g. ");
	    strcat(MsgBf, Addr->LocalPart);
	    strcat(MsgBf, "@");
	    strcat(MsgBf, Dom);
	    strcat(MsgBf, ") ");
	}
	strcat(MsgBf, "is not unambiguously delivered to the person with that userid");
	if (WasIDMatch && NameCopy[0] != '\0') {
	    strcat(MsgBf, " (e.g. ");
	    strcat(MsgBf, NameCopy);
	    strcat(MsgBf, ")");
	}
	strcat(MsgBf, ".  To send to the person with a given userid, ");
	if (WasIDMatch && NameCopy[0] != '\0') {
	    for (Value = NameCopy; *Value != '\0'; ++Value)
		if (*Value == ' ') *Value = '_';
	    wperr = AddressMatchesUnambiguously(wpCD, NameCopy, PKCopy);
	    if (wperr == wperr_NoError) {
		strcat(MsgBf, "either ");
	    }
	    else {
		NameCopy[0] = '\0';
	    }
	}
	strcat(MsgBf, "suffix the userid in the mail address with a plus sign, giving userid+@");
	strcat(MsgBf, Dom);
	if (WasIDMatch) {
	    strcat(MsgBf, " (e.g., ");
	    strcat(MsgBf, Addr->LocalPart);
	    strcat(MsgBf, "+@");
	    strcat(MsgBf, Dom);
	    strcat(MsgBf, ")");
	}
	if (WasIDMatch && NameCopy[0] != '\0') {
	    strcat(MsgBf, ", or address the mail to the user's real name, using periods or underscores in place of spaces (e.g., ");
	    strcat(MsgBf, NameCopy);
	    strcat(MsgBf, "@");
	    strcat(MsgBf, Dom);
	    strcat(MsgBf, ")");
	}
	strcat(MsgBf, ".)\n\n");
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
    }
#endif /* CMU_ENV */
    return CloseReturnError();
}

int ReturnFuzzy(Addr, wpCD, STok, PKey, ToPostman, MinMatch, OutMatch, Dom)
PARSED_ADDRESS *Addr; struct wp_cd *wpCD; wp_SearchToken STok; wp_PrimeKey PKey;
int ToPostman, MinMatch, OutMatch; char *Dom;
{/* Compose an error message about a fuzzily-matched addressee.  Return zero for all-OK, non-zero for various error conditions.  Addr is the fuzzily-matched addressee, STok the White Pages search token that produced the fuzzy match, and PKey, MinMatch, and OutMatch the relevant results of the wp_Lookup: the resultant prime key, a lower bound for the number of matches for the token, and the ambitiousness of the match that had to occur before any match was discovered.  If ToPostman is 0, don't involve postman; if it's 1, send the report only to the postman; if it's 2, send it both to postman and to the sender.
*/
	int C, ThisIx;
	wp_ErrorCode wperr;
	char *Value, *Ptr;
#define LONGNAME 70
	char NameCopy[LONGNAME+1];

	C = OpenReturnError("Fuzzily-matched address", TRUE, ToPostman, Addr, Dom);
	if (C != 0) return C;
	strcat(MsgBf, " because the address ``");
	strcat(MsgBf, Addr->LocalPart);
	strcat(MsgBf, "'' is not a sufficiently clear match to the name of any local user in domain ");
	strcat(MsgBf, Dom);
	if (*Organization != '\0') {
	    strcat(MsgBf, " (");
	    strcat(MsgBf, Organization);
	    strcat(MsgBf, ")");
	}
	strcat(MsgBf, ".  ");

	if (! gotWPIndices) WantWPIndices();
	if (! gotWPIndices) {
		strcat(MsgBf, "(The address is a fuzzy match for a local user, but the absence of the necessary white pages indices makes it impossible to give the user's name.)");
	} else {
		strcat(MsgBf, "Nonetheless, the address is ");
		strcat(MsgBf, MatchAn(OutMatch));
		strcat(MsgBf, " match for the user ");
		wperr = cwp_Read(wpCD, PKey, idxN, &Value);
		NameCopy[0] = '\0';
		if (wperr != wperr_NoError) {
			strcat(MsgBf, "((unreadable key ``");
			strcat(MsgBf, PKey);
			strcat(MsgBf, "'': ");
			strcat(MsgBf, wp_ErrorString(wperr));
			strcat(MsgBf, "))");
		} else {
			strcat(MsgBf, Value);
			ThisIx = strlen(Value);
			if (ThisIx <= LONGNAME) strcpy(NameCopy, Value);
		}
		wperr = cwp_Read(wpCD, PKey, idxAf, &Value);
		if (wperr == wperr_NoError) {
			strcat(MsgBf, " (");
			strcat(MsgBf, Value);
			strcat(MsgBf, ")");
		}
		strcat(MsgBf, " in domain ");
		strcat(MsgBf, Dom);
		if (NameCopy[0] != '\0') {
			for (Ptr = NameCopy; *Ptr != '\0'; ++Ptr)
				if (*Ptr == ' ') *Ptr = '_';
			/* Don't use NameCopy if it isn't unambiguous */
			wperr = AddressMatchesUnambiguously(wpCD, NameCopy, PKey);
			if (wperr != wperr_NoError) NameCopy[0] = '\0';
		}
		wperr = cwp_Read(wpCD, PKey, idxID, &Value);
		if (NameCopy[0] != '\0' || wperr == wperr_NoError) {
			strcat(MsgBf, ", who may be addressed ");
			if (NameCopy[0] != '\0') {
				if (wperr == wperr_NoError) strcat(MsgBf, "either ");
				strcat(MsgBf, "as ``");
				strcat(MsgBf, NameCopy);
				strcat(MsgBf, "@");
				strcat(MsgBf, Dom);
				strcat(MsgBf, "''");
				if (wperr == wperr_NoError) strcat(MsgBf, " or ");
			}
			if (wperr == wperr_NoError) {
				strcat(MsgBf, "as ``");
				strcat(MsgBf, Value);
				strcat(MsgBf, "+@");
				strcat(MsgBf, Dom);
				strcat(MsgBf, "''");
			}
		}
		strcat(MsgBf, ".");
	}
	strcat(MsgBf, "\n\n");
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
	return CloseReturnError();
}

static char *SqzAppend(Target, Start, End)
char *Target, *Start, *End;
{/* Copy chars in [Start..End) to Target, turning all strings of white space to single spaces */
	int State;
	register char *Tgt, *Src;

	Tgt = Target;
	State = -1;	/* -1 init, 0 last was space, 1 last wasn't space */
	for (Src = Start; Src < End; Src++) {
		if (State <= 0) {
			if (index(" \t\n\r", *Src) == NULL) {
				if (State == 0) *Tgt++ = ' ';
				State = 1; *Tgt++ = *Src;
			}
		} else {
			if (index(" \t\n\r", *Src) == NULL) *Tgt++ = *Src;
			else State = 0;
		}
	}
	return Tgt;
}

static char ourMsgID[150] = {'\0'};

int GetDefaultMsgID(Hdr, cPtr)
char *Hdr, **cPtr;
{/* If the message header in Hdr doesn't have a Message-ID (or a ReSent-Message-ID, if there are ReSent-xxx headers), make sure we have a message-ID to use, and return a pointer to the full header line via cPtr.
			 Return 0 for OK, >0 for permanent failures, <0 for temporary ones. */
    int SawReSent;
    char *Start, *Finish;
    static char thisMIDLine[200] = {'\0'};

    SawReSent = 0;
    /* Any ReSent-xxx fields? */
    if (BracketField(Hdr, "ReSent-Date:", &Start, &Finish, NULL) != 0) {
	SawReSent = 1;
    }
    /* Is there an appropriate Message-ID or ReSent-Message-ID field? */
    if (BracketField(Hdr, 
		      (SawReSent ? "ReSent-Message-ID:" : "Message-ID:"),
		      &Start, &Finish, NULL) != 0) return 0;	/* Already there. */
    /* We have to generate a message ID and write it. */
    if (ourMsgID[0] == '\0') {
	sprintf(ourMsgID, "<Added.%s@%s>", ams_genid(0), GlobalDom);
	for (Start = ourMsgID; *Start != '\0'; ++Start) if (*Start == ':') *Start = '_';
    }
    sprintf(thisMIDLine, "%sMessage-ID: %s", (SawReSent ? "ReSent-" : ""), ourMsgID);
    *cPtr = thisMIDLine;
    return 0;
}

int DefaultMsgID(Hdr, File)
char *Hdr; FILE *File;
{/* If the message header in Hdr doesn't have a Message-ID (or a ReSent-Message-ID, if there are ReSent-xxx headers), make sure we have a message-ID to use, and write the full header line to File.
Return 0 for OK, >0 for permanent failures, <0 for temporary ones. */
	int Res;
	char *midLine;

	midLine = NULL;
	Res = GetDefaultMsgID(Hdr, &midLine);
	if (Res != 0 || midLine == NULL) return Res;
	fputs(midLine, File); fputc('\n', File);
	return 0;
}

static int BuildKey(Hdr, DestText, pKey, UseDflt)
char *Hdr, *DestText, **pKey; int UseDflt;
{/* Build the database key we'll use for the header given in Hdr.  Return -23 if there's no Date: field, -24 if there's no From: field, and -25 if we can't allocate space for the header.  If there's space and everything, store the newly-allocated storage via pKey and return 0.
  */
	char *Key, *Start, *Finish, *attnS, *attnF;
	char *Dst;
	int KeySize, SawReSent, omiLen;

	SawReSent = 0;
	KeySize = strlen(DestText) + 8;
	omiLen = strlen(ourMsgID);
/* Any ReSent-xxx fields? */
	if (BracketField(Hdr, "ReSent-Date:", &Start, &Finish, NULL) != 0) {
		if (BracketField(Hdr, "ReSent-Message-ID:", &Start, &Finish, NULL) != 0) {
			KeySize += (Finish - Start);
		} else if (UseDflt && ourMsgID[0] != '\0') {
			Start = ourMsgID;
			Finish = &ourMsgID[omiLen];
			KeySize += omiLen;
		} else return -23;
		SawReSent = 1;
	}
	if (SawReSent == 0) {
		if (BracketField(Hdr, "Message-ID:", &Start, &Finish, NULL)) {
			KeySize += (Finish - Start);
		} else if (UseDflt && ourMsgID[0] != '\0') {
			Start = ourMsgID;
			Finish = &ourMsgID[omiLen];
			KeySize += omiLen;
		} else return -23;
	}
/* Check that the Message-ID: field contents are well-formed.  If they're not well-formed, then the message sender isn't necessarily taking care to make them site-wide unique.  Example: on 20 Sep 1988 a site in Italy (%VAXRMA.INFNET@IBOINFN.BITNET) is generating ``Message-Id: <0001>'' on every message that comes from that site. */
/* Simple checks for now. */
	Dst = Start;
	while (Dst < Finish && isspace(*Dst)) ++Dst;
	if (*Dst != '<') return -23;		/* Make sure it begins with '<', */
	while (Dst < Finish && *Dst != '@') ++Dst;
	if (Dst >= Finish || *Dst != '@') return -23;	/* has an embedded '@', */
	Dst = Finish-1;
	while (Dst > Start && isspace(*Dst)) --Dst;
	if (*Dst != '>') return -23;		/* and ends with '>'. */
/* Now prepare to append any Attention: field to the key. */
	attnF = NULL;
	if (BracketField(Hdr,
		(SawReSent ? "ReSent-Attention:" : "Attention:"),
		&attnS, &attnF, NULL) != 0) {
			KeySize += (attnF - attnS + 1);
	}
	Key = malloc(KeySize+1);
	if (Key == NULL) return -25;
	Dst = Key;
	Dst = SqzAppend(Dst, Start, Finish);
	*Dst++ = '~';
	Dst = SqzAppend(Dst, DestText, &DestText[strlen(DestText)]);
	if (attnF != NULL) {*Dst++ = '~'; Dst = SqzAppend(Dst, attnS, attnF);}
	*Dst++ = '\0';
/* don't bother:	Key = realloc(Key, Dst - Key);	/* in case squeezing helped */
	*pKey = Key;
	return 0;
}

#ifdef tst_NDBM
static DBM *sent_db = NULL;
#endif /* tst_NDBM */

int WasThisSent(Hdr, DestText, Addr)
char *Hdr, *DestText; PARSED_ADDRESS *Addr;
{/* Consult a database of recently-sent messages to determine if this message has been sent to this address recently.  Return < 0 if can't tell, 0 if it wasn't sent, +1 if it was and we reported it, > 1 if it was but we couldn't report it.  (-1 means errno is valid; -22 means this is trymail and we're not trying hard here.)
  */
#ifndef tst_NDBM
	return -1;
#else /* tst_NDBM */
	char *Key;
	char *Src;
	int Ret;
	datum dbKey, dbValue;
	int Date, RetLen, ForLen;
	char *RetPth, *ForStr;
	char *DuplDBName;

	Ret = BuildKey(Hdr, DestText, &Key, 0);
	if (Ret != 0) return Ret;
	if (TMDebug) fprintf(stderr, "Key for dup check is ``%s''.\n", Key);

	if (sent_db == NULL) {
		DuplDBName = malloc(strlen(DuplicateDB) + strlen(SentSuffix) + 4);
		if (DuplDBName == NULL) {free(Key); return -1;}
		sprintf(DuplDBName, "%s/%s", DuplicateDB, SentSuffix);
		sent_db = dbm_open(DuplDBName, O_RDWR, 0666);
		free(DuplDBName);
		if (sent_db == NULL) {free(Key); return -1;}
		if (TMDebug) fprintf(stderr, "Opened dbm database\n");
	}
	if (dbm_error(sent_db)) {
		if (TMDebug) fprintf(stderr, "SENT db has I/O error--ignoring.\n");
		free(Key);
		if (errno == 0) errno = EIO; return -1;	/* take no chances */
	}
	dbKey.dptr = Key;
	dbKey.dsize = strlen(Key);
	dbValue = dbm_fetch(sent_db, dbKey);
	if (dbValue.dptr == NULL) {free(Key); return 0;}
	if (dbValue.dsize < 12) {free(Key); return -6;}
/* Well!  The message was sent!  Now we have to complain about it. */
	if (TMDebug) fprintf(stderr, "Message was already sent to the recipient!\n");
/* The first four bytes of the value are the date at which the message was sent.  Following that value are eight digits, four plus four, giving the length of the Return-Path and the current For: clause.  If there is no For: clause, the second four digits will be zero. */
	Src = dbValue.dptr;
	Date = 0;
	for (Ret = 0; Ret < 4; Ret++) Date = (Date << 8) | *Src++;
	RetLen = 0;
	for (Ret = 0; Ret < 4; Ret++) RetLen = (RetLen * 10) + (*Src++ - '0');
	if (dbValue.dsize < (12 + RetLen)) {free(Key); return -7;}
	RetPth = malloc(RetLen + 1);
	if (RetPth == NULL) {free(Key); return 2;}
	strncpy(RetPth, &(dbValue.dptr[12]), RetLen);
	RetPth[RetLen] = '\0';
	ForLen = 0;
	ForStr = NULL;
	for (Ret = 0; Ret < 4; Ret++) ForLen = (ForLen * 10) + (*Src++ - '0');
	if (ForLen > 0) {
		if (dbValue.dsize < (12 + RetLen + ForLen)) {free(Key); free(RetPth); return -8;}
		ForStr = malloc(ForLen + 1);
		if (ForStr == NULL) {free(Key); free(RetPth); return 3;}
		strncpy(ForStr, &(dbValue.dptr[12 + RetLen]), ForLen);
		ForStr[ForLen] = '\0';
	}
	if (TMDebug) {
		fprintf(stderr, "Extracted value.  Date=%d (%s), RetPth=``%s''",
				Date, NiceTime(Date), RetPth);
		if (ForStr != NULL) fprintf(stderr, ", For=``%s''", ForStr);
		fprintf(stderr, ".\n");
	}
/* Now maybe we can compose the message */
	Ret = OpenReturnError("Duplicate delivery", TRUE, 1, Addr, GlobalDom);
	if (Ret != 0) {free(Key); free(RetPth); free(ForStr); return 4;}

	strcat(MsgBf, " because a message with the same ");
	strcat(MsgBf, (Key[0] == '~' ? "date and source" : "Message-ID"));
	strcat(MsgBf, " was already delivered to address ``");
	strcat(MsgBf, DestText);
	strcat(MsgBf, "'' on ");
	strcat(MsgBf, NiceTime(Date));
	strcat(MsgBf, ".  The return path then was ``");
	strcat(MsgBf, RetPth);
	if (ForStr != NULL) {
		strcat(MsgBf, "'', and it was being delivered for ``");
		strcat(MsgBf, ForStr);
	}
	strcat(MsgBf, "''.\n\n");
	PrintMaybeFoldQuotingFormatting(RetF, MsgBf, RetFmt, strlen(MsgBf), 1);
	free(Key);
	free(RetPth);
	if (ForStr != NULL) free(ForStr);

	Ret = CloseReturnError();
	if (Ret == 0) return 1;
	else return 5;
#endif /* tst_NDBM */
}

void ThisWasSent(Hdr, DestText, ForClause, AllowDflt)
char *Hdr, *DestText, *ForClause; int AllowDflt;
{/* Note in the database of recently-sent messages that a message with the given header was sent to the given destination. 
  */
#ifdef tst_NDBM
	char *Key, *Value;
	int ValLen, Dum, Count, TRPLen, ForLen;
	datum dbKey, dbValue;
	char *Dst, *DuplDBName;

	TRPLen = (TextRetPath == NULL ? 0 : strlen(TextRetPath));
	if (TRPLen > 400) TRPLen = 400;
	ForLen = (ForClause == NULL ? 0 : strlen(ForClause));
	if (ForLen > 400) ForLen = 400;
	ValLen = 4 + 4 + 4 + TRPLen + ForLen;
	Value = malloc(ValLen+1);
	if (Value == NULL) return;		/* too bad */

	osi_GetTimes(&TV);
	Dum = TV.Secs;		/* pack seconds of date into first four chars */
	Dst = &Value[3];
	for (Count = 3; Count >= 0; Count--) {
		*Dst-- = (Dum & 0xff);
		Dum >>= 8;
	}
	Dum = TRPLen;
	Dst = &Value[7];
	for (Count = 3; Count >= 0; Count--) {
		*Dst-- = (Dum % 10) + '0';
		Dum /= 10;
	}
	Dum = ForLen;
	Dst = &Value[11];
	for (Count = 3; Count >= 0; Count--) {
		*Dst-- = (Dum % 10) + '0';
		Dum /= 10;
	}
	if (TextRetPath != NULL) strncpy(&Value[12], TextRetPath, TRPLen);
	if (ForClause != NULL) strncpy(&Value[12+TRPLen], ForClause, ForLen);
	Value[12+TRPLen+ForLen] = '\0';
	if (TMDebug) fprintf(stderr, "Value[4 to inf] is ``%s''.\n", &Value[4]);
	Dum = BuildKey(Hdr, DestText, &Key, AllowDflt);
	if (Dum != 0) {free(Value); return;}
	if (TMDebug) fprintf(stderr, "Key for dup elim is ``%s''.\n", Key);

/* OK, now store the thing */
	if (sent_db == NULL) {
		DuplDBName = malloc(strlen(DuplicateDB) + strlen(SentSuffix) + 4);
		if (DuplDBName == NULL) {free(Key); free(Value); return;}
		sprintf(DuplDBName, "%s/%s", DuplicateDB, SentSuffix);
		sent_db = dbm_open(DuplDBName, O_RDWR, 0666);
		free(DuplDBName);
		if (sent_db == NULL) {free(Key); free(Value); return;}
	}
	if (dbm_error(sent_db)) {
		if (TMDebug) fprintf(stderr, "SENT db has I/O error--ignoring.\n");
		free(Key); free(Value); return;	/* don't take a chance */
	}
	Count = osi_ExclusiveLockNoBlock(dbm_dirfno(sent_db));
	if (TMDebug) fprintf(stderr, "flock(SENT.dir): %d; errno: %d/%s\n",
				Count, errno, UnixError(errno));
	if (Count < 0) {free(Key); free(Value); return;}	/* couldn't get the lock. */
	dbKey.dptr = Key;
	dbKey.dsize = strlen(Key);
	dbValue.dptr = Value;
	dbValue.dsize = ValLen;
	Dum = dbm_store(sent_db, dbKey, dbValue, DBM_REPLACE);
	if (TMDebug) fprintf(stderr, "Result from dbm-store: %d\n", Dum);
	osi_UnLock(dbm_dirfno(sent_db));
	free(Key);
	free(Value);
#endif /* tst_NDBM */
	return;
}

int FinishSent()
{ /* Close up any DBM database access; free file descriptors for the FD plumber. */
#ifdef tst_NDBM
	if (sent_db != NULL) {
		if (dbm_error(sent_db)) dbm_clearerr(sent_db);
		dbm_close(sent_db);
		sent_db = NULL;
	}
#endif /* tst_NDBM */
	return 0;
}

enum Trial {UnTried, Current, Finished, Redundant};

struct DelayedRemote {
	struct DelayedRemote	*Next;
	PARSED_ADDRESS	*RmtAddr;
	char			*FwdString;
	enum Trial		Status;
	int			FailCode1, FailCode2;
	char			*AddrPrint;
	long int			AddrReason;
};

static struct DelayedRemote *RemoteRoot = NULL;

int AppendRemote(Addr, Fwd)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
{
	struct DelayedRemote *DR;

#if Logs
	Log(170, "AppendRemote called to create a DelayedRemote record");
#endif /* Logs */
	DR = (struct DelayedRemote *) malloc(sizeof(struct DelayedRemote));
	if (DR == NULL) return 1;
	if (Fwd == NULL && GlobalForString == NULL) {
		DR->FwdString = NULL;
	} else {
		if (MakeFORString(Fwd, NULL, GlobalDom, &(DR->FwdString)) < 0) {
		    free(DR);
		    return 1;
		}
	}
	DR->RmtAddr = Addr;
	DR->Status = UnTried;
	DR->FailCode1 = DR->FailCode2 = 0;
	DR->AddrPrint = NULL;
	DR->AddrReason = 0;
	DR->Next = RemoteRoot;
	RemoteRoot = DR;

#if Logs
	Log(171, "AppendRemote returning after all saved.");
#endif /* Logs */
	return 0;
}

int RemoteGotDelivered(Addr)
PARSED_ADDRESS *Addr;
{/* The given Addr has been delivered.  Don't consider it here. */
	struct DelayedRemote *DR;

	for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
	    if (DR->RmtAddr == Addr) {
		DR->FailCode1 = 0;
		DR->FailCode2 = 3;
		DR->Status = Finished;
		if (TMDebug) fprintf(stderr, "RemoteGotDelivered: found ``%s''.\n", Addr->LocalPart);
		return 0;
	    }
	}
	if (TMDebug) fprintf(stderr, "RemoteGotDelivered: didn't find ``%s''.  Sigh.\n", Addr->LocalPart);
	return 1;
}

static int BackULcmp(p1, p2)
char *p1, *p2;
{/* Do like ULstrcmp, but look from the ends of the strings to the beginnings. */
	register char *pe1, *pe2;
	char c1, c2;

	pe1 = p1 + strlen(p1) - 1;
	pe2 = p2 + strlen(p2) - 1;
	while (pe1 >= p1 || pe2 >= p2) {
		if (pe1 < p1) return -1; if (pe2 < p2) return 1;
		c1 = *pe1; if (isupper(c1)) c1 = tolower(c1);
		c2 = *pe2; if (isupper(c2)) c2 = tolower(c2);
		if (c1 != c2) return (c1 < c2 ? -1 : 1);
		--pe1; --pe2;
	}
	return 0;
}

static CompDR(dr1, dr2)
struct DelayedRemote **dr1, **dr2;
{/* Comparison routine for sorting the DRList via qsort. */
/* First, ForString values need to sort together.  Within that, we have some freedom,
   even though we want mail going to the same host to be grouped together.
   Sort the domains right-end first. */

	int CR;
	PARSED_ADDRESS *a1, *a2;
	ADDRESS_HOST *h1, *h2;
	char *fh1, *fh2, *hn1, *hn2;

	CR = strcmp(
		((*dr1)->FwdString == NULL ? "" : (*dr1)->FwdString),
		((*dr2)->FwdString == NULL ? "" : (*dr2)->FwdString)	);
	if (CR != 0) return CR;
	a1 = (*dr1)->RmtAddr; a2 = (*dr2)->RmtAddr;
	fh1 = fh2 = NULL;
	if ((*dr1)->RmtAddr->MD->Fwds != NULL) fh1 = (*dr1)->RmtAddr->MD->Fwds[0];
	if ((*dr2)->RmtAddr->MD->Fwds != NULL) fh2 = (*dr2)->RmtAddr->MD->Fwds[0];
	h1 = a1->Hosts; h2 = a2->Hosts;
	if (h1 == NULL && h2 == NULL) return BackULcmp(a1->LocalPart, a2->LocalPart);
	if (h1 == NULL) return -1; if (h2 == NULL) return 1;
	h1 = h1->Prev; h2 = h2->Prev;
	for (;;) {
		hn1 = (fh1 == NULL ? h1->Name : fh1);
		hn2 = (fh2 == NULL ? h2->Name : fh2);
		if (h1 == a1->Hosts && h2 == a2->Hosts)
			return BackULcmp(a1->LocalPart, a2->LocalPart);
		if (h1 == a1->Hosts) return -1;
		if (h2 == a2->Hosts) return 1;
		CR = BackULcmp(hn1, hn2);
		if (CR != 0) return CR;
		if (fh1 != NULL) {fh1 = NULL;} else {h1 = h1->Prev;}
		if (fh2 != NULL) {fh2 = NULL;} else {h2 = h2->Prev;}
	}
}

#define MAXDELAY	(60*25)	/* Stop making forward progress after twenty-five minutes */
#define MAXRECIP	400	/* Max number of oldsendmail recipients */
#ifndef ARG_MAX
#define ARG_MAX		4096	/* Max number of chars in oldsendmail argv */
#endif

extern char **environ;

static int FinishBy;

static int TimeLeft()
{/* Return the number of seconds left before we should quit. */
	osi_GetTimes(&TV);
	return (FinishBy - TV.Secs);
}

void DealWithRemoteList(AddrList)
PARSED_ADDRESS *AddrList;
{
/* Once other delivery is finished, either print out or deliver to the remote addresses. */

	struct DelayedRemote *DR, *NextDR, *TopDR;
	int NumRecip, Recip, NMax, CharsRead, ArgVLen, ArgCCount, MaxArgVLen;
	int Hunk, RecipTry, RecipTarget, BreakTop, MustStop, NumOSMRuns;
	char **e, **NewArgv = NULL;
	char *MsgHdr, *PrevHost, *ThisHost;
	FILE *SendMail;
	int SuccCode, TimedOut, PGrp, FCode1, FCode2 = 0, WaitTime, DRCount, fd;
	struct BEStripper *BES = NULL;
	struct DelayedRemote **DRList = NULL;
	ADDRESS_HOST *Hst;
	auto char fName[MAXPATHLEN];
	auto char msgCase15[100+MAXPATHLEN];
	auto char msgCase16[100+MAXPATHLEN];
	extern char Qmail_ErrMsg[];
#define FixedArgs 5
#define OSMModeCell NewArgv[4]
	static char *ExNames[] = {		/* values for FCode1 and FailCode1 */
		NULL,			/* 0 -- success */
		NULL,			/* 1 -- no memory */
		"topen(sendmail) failed",	/* 2 */
		"StripBE failed",		/* 3 */
		NULL,			/* 4 -- input file too short */
		"CloseUnscribe failed",	/* 5 */
		"Sendmail timed out",	/* 6 */
		NULL,			/* 7 -- sendmail signalled */
		NULL,			/* 8 -- sendmail gave EX_TEMPFAIL */
		NULL,			/* 9 -- sendmail exited with unexpected value */
		NULL,			/* 10 -- OpenStripBE hard-failed */
		NULL,			/* 11 -- soft domain validation failure */
		NULL,			/* 12 -- hard domain validation failure */
		NULL,			/* 13 -- out of time for this address */
		NULL,			/* 14 -- CheckAMSDelivery gave 0 */
		NULL,			/* 15 -- Temp failure in cross-cell dropoff */
		NULL,			/* 16 -- Perm failure in cross-cell dropoff */
	};

	if (RemoteRoot == NULL) return;

	osi_GetTimes(&TV);
	FinishBy = TV.Secs + MAXDELAY;	/* Timestamp of when to finish */

	CharsRead = -1;
	MsgHdr = NULL;
	NMax = GetHeader(&MsgHdr);
	if (NMax != 0) goto Backup;

/* Unparse all the addresses */
	NumRecip = 0;
	for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
	    DR->AddrPrint = AddrUnparse(DR->RmtAddr, &DR->AddrReason);
	    if (DR->AddrPrint != NULL && DR->Status == UnTried) {
		NMax = WasThisSent(MsgHdr, DR->AddrPrint, DR->RmtAddr);
		if (NMax > 0) {
		    DR->Status = Redundant;
		} else {
		    ++NumRecip;
		}
	    }
	}
	if (NumRecip == 0) goto Backup;

	TimedOut = 0;
	for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
	    if (DR->Status == UnTried) {
		Hst = DR->RmtAddr->Hosts;
		/* Catch problem cases falling through from when we can't reject a cross-cell message. */
		if (Hst != NULL && Hst != Hst->Prev && DR->RmtAddr->MD == NULL) {
		    char *Prime, *Second; int outType, laerr;
		    Prime = NULL;
		    laerr = la_KindDomain(DR->RmtAddr, &outType, &Prime, &Second, GlobalDom);
		    if (Prime != NULL) free(Prime);
		}
		Hst = DR->RmtAddr->Hosts;
		if (Hst != NULL && Hst != Hst->Prev) {
		    Hst = Hst->Prev;
		    if (Hst->Name != NULL && Hst->Name[0] != '\0') {
			if (DR->RmtAddr->MD == NULL) {
			    DR->Status = Finished;
			    DR->FailCode1 = 13;
			} else if (DR->RmtAddr->MD->Qual == mailhost_bad || (!IsExpired && DR->RmtAddr->MD->Qual == mailhost_indeterminate)) {
			    DR->Status = Finished;
			    DR->FailCode1 =
			      (DR->RmtAddr->MD->Qual == mailhost_bad ? 12 : 11);
			}
		    }
		}
	    }
	}

	if (NumRecip == 0) goto Backup;

	NMax = OpenStripBE(&BES, MsgHdr, Normal);
	if (NMax < 0) {
	    for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
		DR->Status = Finished;
		DR->FailCode1 = 10;
		DR->FailCode2 = NMax;
	    }
	}
	if (NMax != 0) goto Backup;

/* Allocate the argv for the sendmail invocation */
	NewArgv = (char **) malloc(sizeof(char *) * (NumRecip + FixedArgs + 1));
	if (NewArgv == NULL) goto Backup;
	NewArgv[0] = "oldsendmail";
	NewArgv[1] = "-oi";
	NewArgv[2] = "-f";
	NewArgv[3] = TextRetPath;	/* NewArgv[4] is OSMModeCell, set later */

	DRList = (struct DelayedRemote **) malloc(sizeof(struct DelayedRemote *) * NumRecip);
	if (DRList == NULL) goto Backup;
	DRCount = 0;
	for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
		if (DR->Status == UnTried && DR->AddrPrint != NULL) {
			DRList[DRCount++] = DR;
		}
	}
	if (DRCount > 1) qsort(DRList, DRCount, sizeof(struct DelayedRemote *), CompDR);
	if (TMDebug && DRCount > 0) {
	    fputs("Sorted recipients:\n ", stderr);
	    for (Recip = 0; Recip < DRCount; ++Recip) {
		if (Recip > 0) fputs(",\n ", stderr);
		if (DRList[Recip]->FwdString != NULL)
		    fprintf(stderr, "{%s}", DRList[Recip]->FwdString);
		fputs(DRList[Recip]->AddrPrint, stderr);
		if (DRList[Recip]->RmtAddr->MD != NULL &&
		    DRList[Recip]->RmtAddr->MD->Fwds != NULL)
		    fprintf(stderr, "//%s",
			    DRList[Recip]->RmtAddr->MD->Fwds[0]);
	    }
	    fputs("\n", stderr);
	}

/* Attempt to do cross-cell delivery. */
	if ((GlobalOptions & tmopt_CrossCellDelivery) != 0) {
	  PrevHost = "";
	  for (Hunk = 0; Hunk < DRCount; ++Hunk) {
	    TopDR = DRList[Hunk];
	    if (TopDR->Status != UnTried || TopDR->AddrPrint == NULL) continue;
	    FCode1 = 0;
	    ThisHost = TopDR->RmtAddr->Hosts->Prev->Name;
	    if (ThisHost == NULL) continue;
	    if (ULstrcmp(ThisHost, GlobalDom) == 0) continue;  /* never ``cross'' to this cell. */
	    if (ULstrcmp(ThisHost, PrevHost) == 0) continue;    /* just failed; don't retry */
	    PrevHost = ThisHost;		/* Save for inner loop and next iteration */
	    PGrp = CheckAMSDelivery(PrevHost);
	    if (PGrp == 0) {
		if (IsCrossCellExpired) {	/* time it out early */
		    PGrp = -1;
		} else {
		    FCode1 = 14;
		    FCode2 = errno;
		}
	    }
	    if (PGrp < 0) continue;	/* this one isn't a candidate for cross-cell dropoff */
	    Recip = 0;
	    for (RecipTry = Hunk; RecipTry < DRCount; ++RecipTry) {
		DR = DRList[RecipTry];
		if (DR->Status != UnTried || DR->AddrPrint == NULL) continue;
		if (strcmp((TopDR->FwdString == NULL ? "" : TopDR->FwdString),
			   (DR->FwdString == NULL ? "" : DR->FwdString)) != 0) {
		    continue;
		}
		ThisHost = DR->RmtAddr->Hosts->Prev->Name;
		if (ThisHost == NULL) continue;
		if (ULstrcmp(ThisHost, PrevHost) != 0) continue;
		DR->Status = Current;
		if (TMDebug) fprintf(stderr, "--Cell %s adds: %s\n",
				     PrevHost, DR->AddrPrint);
		NewArgv[FixedArgs + Recip] = DR->AddrPrint;
		++Recip;
	    }
	    NewArgv[FixedArgs + Recip] = NULL;
	    if (TMDebug) fprintf(stderr, "Trying cross-cell delivery for %d addresses to cell %s; fwd=%s.\n", Recip, PrevHost, (TopDR->FwdString == NULL ? "NULL" : TopDR->FwdString));
	    if (FCode1 == 0) {
		FCode1 = MakeFile(NULL, MsgHdr, fName, NULL);
		if (FCode1 != 0) {FCode2 = FCode1; FCode1 = 1;}
		else {
		    fd = open(fName, O_RDONLY, 0644);
		    if (fd < 0) {
			FCode1 = 1; FCode2 = errno;
		    } else {
			if (! ReallyDeliver) {
			    FCode1 = Q_OK;
			    FCode2 = 2;
			    if (TMDebug) fprintf(stderr, "Not really dropping off\n");
			} else {
			    FCode1 = tryvicequeues_cell(&NewArgv[FixedArgs], fd,
							TextRetPath, AuthString, 0, TopDR->FwdString, 0,
							PrevHost, MailQueueNamePrefix);
			    if (TMDebug) fprintf(stderr, "Cell dropoff: %d/%d\n", FCode1, FCode2);
			    if (FCode1 == Q_OK) {FCode1 = 0; FCode2 = 1;}
			    else if (FCode1 == Q_TEMP_FAIL) {
				FCode1 = 15;
				strncpy(msgCase15, Qmail_ErrMsg, sizeof(msgCase15)-1);
			    } else {
				FCode2 = FCode1; FCode1 = 16;
				strncpy(msgCase16, Qmail_ErrMsg, sizeof(msgCase16)-1);
			    }
			}
			close(fd);
		    }
		    unlink(fName);
		}
	    }
	    for (RecipTry = Hunk; RecipTry < DRCount; ++RecipTry) {
		DR = DRList[RecipTry];
		if (DR->Status != Current || DR->AddrPrint == NULL) continue;
		DR->Status = Finished;
		DR->FailCode1 = FCode1;
		DR->FailCode2 = FCode2;
	    }
	  }
	}

/* Invoke sendmail once for each distinct ForString value */
	if ((GlobalOptions & tmopt_SendmailDropoff) != 0) {
	  NumOSMRuns = 0;
	  for (Hunk = 0; Hunk < DRCount; ++Hunk) {
	    TopDR = DRList[Hunk];
	    if (TopDR->Status != UnTried || TopDR->AddrPrint == NULL) continue;
	    PrevHost = NULL;
	    ArgVLen = ArgCCount = 0;
#ifdef _SC_ARG_MAX
	    MaxArgVLen = sysconf(_SC_ARG_MAX);
#else
	    MaxArgVLen = ARG_MAX;
#endif
	    for (e = environ; *e; e++) {
		MaxArgVLen -= strlen(*e) + 1;
	    }
	    MaxArgVLen -= 10;	/* Allow for some slop */

	    BreakTop = MustStop = -1;
	    RecipTarget = TimeLeft() / 25;	/* 25 seconds per recip */
	    if (RecipTarget > MAXRECIP) RecipTarget = MAXRECIP;
	    for (RecipTry = Hunk; RecipTry < DRCount; ++RecipTry) {
		DR = DRList[RecipTry];
		if (DR->Status != UnTried || DR->AddrPrint == NULL) continue;
		if (strcmp((TopDR->FwdString == NULL ? "" : TopDR->FwdString),
			   (DR->FwdString == NULL ? "" : DR->FwdString)) != 0) {
		    MustStop = RecipTry;	/* Must stop here */
		    break;
		}
		if (DR->RmtAddr->MD != NULL
		    && DR->RmtAddr->MD->Fwds != NULL) {
		    ThisHost = DR->RmtAddr->MD->Fwds[0];
		} else if (DR->RmtAddr->Hosts->Prev->Name != NULL) {
		    ThisHost = DR->RmtAddr->Hosts->Prev->Name;
		} else ThisHost = "";
		if (PrevHost != NULL &&
		    ULstrcmp(PrevHost, ThisHost) != 0) BreakTop = RecipTry;
		PrevHost = ThisHost;
		++ArgCCount; ArgVLen += (1 + strlen(DR->AddrPrint));
		if (ArgCCount >= RecipTarget || ArgVLen >= MaxArgVLen) {
		    if (BreakTop < 0) {
			MustStop = RecipTry;
			if (MustStop <= Hunk) MustStop = Hunk + 1;
		    } else {
			MustStop = BreakTop;
		    }
		    break;
		}
	    }
	    if (MustStop < 0) MustStop = DRCount;	/* Do 'em all */
	    OSMModeCell = (MustStop == DRCount ? "-odb" : "-odq");
	    if (TMDebug) fprintf(stderr,
				 "Trying %d (time: %d; tot: %d) recips [%d:%d] from [0:%d]; fwd=%s.\n",
				 MustStop - Hunk, RecipTarget, DRCount - Hunk,
				 Hunk, MustStop-1, DRCount-1,
				 (TopDR->FwdString == NULL ? "NULL" : TopDR->FwdString));
	    Recip = 0;
	    for (RecipTry = Hunk; RecipTry < MustStop; ++RecipTry) {
		DR = DRList[RecipTry];
		if (DR->Status != UnTried || DR->AddrPrint == NULL) continue;
		if (TMDebug) {
		    fprintf(stderr, "Including addr: %s", DR->AddrPrint);
		    if (DR->RmtAddr->MD != NULL
			&& DR->RmtAddr->MD->Fwds != NULL)
			fprintf(stderr, "[[%s]]",
				DR->RmtAddr->MD->Fwds[0]);
		    fprintf(stderr, "\n");
		}
		NewArgv[FixedArgs + Recip] = DR->AddrPrint;
		DR->Status = Current;
		++Recip;
	    }
	    NewArgv[FixedArgs + Recip] = NULL;
	    FCode1 = 0;		/* this request OK so far */
	    /* now deliver the mail and mark successfully attempted addressees as delivered */
	    if (TMDebug) {
		fprintf(stderr, "DealWithRemoteList about to call oldsendmail.\n");
		fprintf(stderr, "Args are: %s", NewArgv[0]);
		for (Recip = 1; NewArgv[Recip] != NULL; Recip++)
		    fprintf(stderr, ", %s", NewArgv[Recip]);
		fprintf(stderr, ".\n");
	    }
	    OpenUnscribe(BES);
	    if (BES->UnScrValue != NULL && BES->UnScribeCode == -2) {
		FCode1 = 1;
		goto LoopFail;
	    }
	    errno = 0;
	    if (ReallyDeliver)
		SendMail = topen(oldsendmail, NewArgv, "w", &PGrp);
	    else
		SendMail = fopen("/dev/null", "w");
	    if (SendMail == NULL) {
		AbortUnscribe(BES);
		FCode1 = 2;
		FCode2 = errno;
		goto LoopFail;
	    }
	    errno = 0;
	    WriteReceived(SendMail, TopDR->FwdString, NULL);
	    DefaultMsgID(BES->Header, SendMail);
	    FCode2 = StripBE(BES, SendMail, &CharsRead);
	    if (FCode2 != 0) {
		if (FCode2 == -1) FCode2 = errno;
		killpg(PGrp, SIGKILL);
		AbortUnscribe(BES);
		fclose(SendMail);
		FCode1 = 3;
		goto LoopFail;
	    }
	    if (CharsRead < TMGlobalMinimumSize || CharsRead < MinimumFileSize) {
		killpg(PGrp, SIGKILL);
		AbortUnscribe(BES);
		fclose(SendMail);
		FCode1 = 4;
		goto LoopFail;
	    }
	    if (CloseUnscribe(BES, SendMail) < 0) {
		FCode1 = 5; FCode2 = errno;
		killpg(PGrp, SIGKILL);
		fclose(SendMail);
		goto LoopFail;
	    }
	    /* All piped off to sendmail--now close and decipher */
	    TimedOut = 0;
	    WaitTime = TimeLeft();
	    errno = 0;
	    if (ReallyDeliver) {
		SuccCode = tclose(SendMail, WaitTime, &TimedOut);
	    } else {
		fclose(SendMail); SuccCode = 0;
	    }
	    if (TMDebug) fprintf(stderr, "SendMail ret status is %#x\n", SuccCode);
	    if (SuccCode == -1 || TimedOut != 0) {
		FCode1 = 6; FCode2 = errno;
		killpg(PGrp, SIGKILL);
		goto LoopFail;
	    }
	    if ((SuccCode & 0377) != 0) {
		FCode1 = 7; FCode2 = SuccCode & 0177;
		goto LoopFail;
	    }
	    FCode2 = SuccCode >> 8;
	    switch (FCode2) {
		case 0:
		case EX_NOUSER:
		case EX_NOHOST:
		    FCode1 = FCode2 = 0;
		    if (! ReallyDeliver) FCode2 = 2;
		    break;
		case EX_TEMPFAIL:
		case 139: case 1:
		    FCode1 = 8;
		    break;
		default:
		    FCode1 = 9;
		    break;
	    }
	    LoopFail:			/* Post the current status into each Current request */
	      for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
		  if (DR->Status == Current) {
		      DR->Status = Finished;
		      DR->FailCode1 = FCode1;
		      DR->FailCode2 = FCode2;
		  }
	      }
	    if (TimeLeft() < (5*60) || (++NumOSMRuns >= 3)) {
		/* Less than five minutes left, or third OSM run? */
		for (DR = RemoteRoot; DR != NULL; DR = DR->Next) {
		    if (DR->Status == UnTried) {
			DR->Status = Finished;
			DR->FailCode1 = 13;	/* no time left */
		    }
		}
	    }
	  }
	}
    Backup:
	if (NewArgv != NULL) {free(NewArgv); NewArgv = NULL;}
	if (DRList != NULL) {free(DRList); DRList = NULL;}
	CloseStripBE(BES); BES = NULL;
	/* Print results and dispose of records */
	for (DR = RemoteRoot; DR != NULL; DR = NextDR) {
	    if (DR->AddrPrint == NULL || DR->FailCode1 == 1 ||
		DR->Status == UnTried || DR->Status == Current)
		AddrResult(tmltf_MemoryExhausted, DR->RmtAddr, "out of mem (%d)", DR->FailCode2);
	    else if (DR->Status == Redundant)
		AddrResult(tmok_RedundantDelivery, DR->RmtAddr, "message delivered earlier");
	    else switch (DR->FailCode1) {
		case 0:
		    if (DR->FailCode2 == 2) {
			AddrResult(tmok_RequestedNotToSend, DR->RmtAddr,
				   "Requested to omit");
		    } else if (DR->FailCode2 == 3) {
			AddrResult(tmok_DroppedOffToCell, DR->RmtAddr,
				   "Delivered into authenticated cell");
		    } else if (DR->FailCode2 == 1) {
			AddrResult(tmok_DroppedOffToCell, DR->RmtAddr,
				   "Dropped into cellular queues");
		    } else {
			AddrResult(tmok_DroppedOffForNet, DR->RmtAddr,
				   "dropped off to net delivery");
		    }
		    if (MsgHdr != NULL && DR->AddrPrint != NULL)
			ThisWasSent(MsgHdr, DR->AddrPrint, DR->FwdString, 1);
		    break;
		case 2: case 5: case 6:
		    AddrResult(tmltf_MailerSystemFailure, DR->RmtAddr, "%s: code %s",
			       ExNames[DR->FailCode1], UnixError(DR->FailCode2));
		    break;
		case 3:
		    if (DR->FailCode2 > 0) {
			if (tfail(DR->FailCode2) || DR->FailCode2 == EPIPE || ULstrcmp(GlobalDom, WorkstationCell) != 0) {
			    AddrResult(tmltf_MailTransputError, DR->RmtAddr,
				       "StripBE: error %s", UnixError(DR->FailCode2));
			} else {
			    PermMessage(tmlpf_DropoffFailure, DR->RmtAddr,
					UnixError(DR->FailCode2),
					"StripBE ran into a permanent failure: %s",
					UnixError(DR->FailCode2));
			}
		    } else {
			switch (DR->FailCode2) {
			    case -2: case -3: case -4: case -5:
				AddrResult(tmlpf_DropoffFailure, DR->RmtAddr,
					   "internal UnScribe error: %d", DR->FailCode2);
				break;
			    case -6:
				PermMessage(tmlpf_DropoffFailure, DR->RmtAddr,
					    "internal UnScribe error",
					    "The message appears to have an unusually long style name, and is probably not really a base editor document.  This usually is a symptom of a bug in the message-composing program that you can circumvent by avoiding backslashes in your message text.");
				break;
			}
		    }
		    break;
		case 4:
		    AddrResult(tmltf_MailTransputError, DR->RmtAddr,
			       "Input file too short--%d out of %d bytes", CharsRead,
			       (MinimumFileSize > TMGlobalMinimumSize ?
				MinimumFileSize : TMGlobalMinimumSize));
		    break;
		case 7:
		    AddrResult(tmltf_MailerExecFailure, DR->RmtAddr,
			       "Sendmail aborted with signal: %s",
			       UnixSignal(DR->FailCode2));
		    break;
		case 8:
		    AddrResult(tmltf_MailerTempFailure, DR->RmtAddr,
			       "Sendmail returned a temporary failure");
		    break;
		case 9:
		    PermMessage(tmlpf_DropoffFailure, DR->RmtAddr,
				"Error in mail dropoff",
				"Sendmail complained about the message with error code %d, which means: %s.",
				DR->FailCode2, UnixSysExits(DR->FailCode2));
		    break;
		case 10:
		    if (ULstrcmp(GlobalDom, WorkstationCell) == 0) {
			PermMessage(tmlpf_DropoffFailure, DR->RmtAddr,
				OSBE_Brief, "%s", OSBE_Text);
		    } else {
			AddrResult(tmltf_CrossCellNonReject, DR->RmtAddr, "%s: %s", OSBE_Brief, OSBE_Text);
		    }
		    break;
		case 11:
		    AddrResult(tmltf_MailerTempFailure, DR->RmtAddr,
			       "Temp failure validating destination domain");
		    break;
		case 12:
		    if (ULstrcmp(GlobalDom, WorkstationCell) == 0) {
			PermMessage(tmlpf_DropoffFailure, DR->RmtAddr,
				"No such destination mail domain",
				"There is no such destination mail domain as ``%s''.",
				DR->RmtAddr->MD->Orig);
		    } else {
			AddrResult(tmltf_CrossCellNonReject, DR->RmtAddr, "No auth rej of dom %s for %s from %s", DR->RmtAddr->MD->Orig, GlobalDom, WorkstationCell);
		    }
		    break;
		case 13:
		    AddrResult(tmltf_MailerTempFailure, DR->RmtAddr,
			       "Out of time in this run");
		    break;
		case 14:
		    AddrResult(tmltf_MailerTempFailure, DR->RmtAddr, "Temp failure checking AMS delivery in remote cell: %s", UnixError(DR->FailCode2));
		    break;
		case 15:
		    AddrResult(tmltf_MailerTempFailure, DR->RmtAddr, "Temp failure in cross-cell dropoff, e.g. %s", msgCase15);
		    break;
		case 16:
		    AddrResult(tmltf_MailerExecFailure, DR->RmtAddr, "Persistent failure in cross-cell dropoff (code %d), e.g.: %s", DR->FailCode2, msgCase16);
		    break;
		default:
		    AddrResult(tmlpf_DropoffFailure, DR->RmtAddr,
			       "internal (unexpected) error in mail dropoff: %d/%d",
			       DR->FailCode1, DR->FailCode2);
		    break;
	    }
	    NextDR = DR->Next;
	    if (DR->FwdString != NULL) free(DR->FwdString);
	    if (DR->AddrPrint != NULL) free(DR->AddrPrint);
	    free(DR);
	}
	if (MsgHdr != NULL) {free(MsgHdr); MsgHdr = NULL;}
	RemoteRoot = NULL;
}

int DoProg(Addr, Hdr, ForString, PKey, Fwd, Dom, UseFmt)
PARSED_ADDRESS *Addr;
char *Hdr, *ForString, *Dom;
wp_PrimeKey PKey;
struct FwdLink *Fwd;
int UseFmt;
{/* Do local delivery by running a program.  Fundamentally, use popen(3) to spawn a subprocess and pipe the message to it on its standard input.  Construct the command argument to popen(3) by sprintf'ing the source and destination addresses into the user-specified template.  UseFmt is TRUE if the process expects an ATK-formatted message, FALSE if it expects a format-stripped message. */
    wp_ErrorCode wpErr;
    char *s, *d, *Pgm, *Arglist, *ProgBreak;
    int Foo, NMax, CharsRead, SuccCode, AnyOld, AnyNew, AnyBogus; long int LongStat;
    static char *DestStr, *CopyForString;
    FILE *RunningPgm;
    struct BEStripper *BES = NULL;
    struct stat sblock;
    static struct {char **Src; char Abbr;} Params[] = {
	{&TextRetPath, 'r'},
	{&DestStr, 'd'},
	{&CopyForString, '4'},
	{&AuthString, 'a'},
    };
#define numParams (sizeof(Params) / sizeof(Params[0]))
    char *QuotedParams[numParams];
    int ParamUses[numParams];

    if ((GlobalOptions & tmopt_ProgDelivery) == 0)
	return AddrResult(tmltf_NotImplementedHere, Addr, "Program delivery not called for");
    if (ULstrcmp(Dom, WorkstationCell) != 0) {
	ThisIsRemote(Addr, Fwd, Fwd);
	return tmltf_ForwardedOutOfAuth;
    }
    wpErr = GetWPField(PKey, idxDP, &Pgm);
    if (wpErr != wperr_NoError) {
	return AddrResult(tmltf_WhitePagesRunFailure, Addr, "Can't find DP field for program: %s", wp_ErrorString(wpErr));
    }
    CopyForString = ForString;
    DestStr = AddrUnparse(Addr, &LongStat);
    if (DestStr == NULL) {
	return AddrResult(tmltf_MemoryExhausted, Addr, "No memory: %d", LongStat);
    }
    for (Foo = 0; Foo < numParams; ++Foo) QuotedParams[Foo] = NULL;
    SuccCode = 1;
    for (Foo = 0; Foo < numParams; ++Foo) {
	QuotedParams[Foo] = malloc(2*strlen((*Params[Foo].Src == NULL ? "" : *Params[Foo].Src)) + 10);
	if (QuotedParams[Foo] == NULL) {SuccCode = 0; break;}
    }
    if (SuccCode == 0) {
	for (Foo = 0; Foo < numParams; ++Foo) {
	    if (QuotedParams[Foo] != NULL) free(QuotedParams[Foo]);
	}
	free(DestStr);
	return AddrResult(	tmltf_MemoryExhausted, Addr, "No memory");
    }
    for (Foo = 0; Foo < numParams; ++Foo) {
	ParamUses[Foo] = 0;
	d = QuotedParams[Foo];
	*d++ = '"';
	for (s = (*Params[Foo].Src == NULL ? "" : *Params[Foo].Src); *s != '\0'; ++s) {
	    if (*s == '\\' || *s == '\'' || *s == '"' || *s == '$') *d++ = '\\';
	    *d++ = *s;
	}
	*d++ = '"';
	*d++ = '\0';
    }
    free(DestStr);
    AnyOld = AnyNew = AnyBogus = 0;
    s = Pgm;
    while (*s != '\0') {
	if (*s == '%') {
	    SuccCode = 0;
	    ++s;
	    if (*s == '%') SuccCode = 1;
	    else if (*s == 's') {++AnyOld; SuccCode = 1;}
	    else for (Foo = 0; Foo < numParams; ++Foo) if (*s == Params[Foo].Abbr) {
		++AnyNew; SuccCode = 1;
		++ParamUses[Foo];
	    }
	    if (SuccCode == 0) ++AnyBogus;
	}
	++s;
    }
    if (AnyBogus > 0 || (AnyNew > 0 && AnyOld > 0) || AnyOld > 4) {
	for (Foo = 0; Foo < numParams; ++Foo) free(QuotedParams[Foo]);
	return(PermMessage(tmlpf_MailerPermFailure, Addr,
			   "Bad parameter for PGM mail",
			   "The DK parameter for this address is ``%s'', which contains %s %%-escape characters.",
			   AnyBogus > 0 ? "unrecognized" :
			   AnyNew > 0 && AnyOld > 0 ? "both old- and new-style" :
			   "too many"));
    }
    Foo = 0; SuccCode = AnyOld;
    while (SuccCode > 0) {
	++ParamUses[Foo];
	++Foo; --SuccCode;
    }
    SuccCode = 100 + strlen(Pgm);
    for (Foo = 0; Foo < numParams; ++Foo) SuccCode += ParamUses[Foo] * strlen(QuotedParams[Foo]);
    Arglist = malloc(SuccCode);
    if (Arglist == NULL) {
	for (Foo = 0; Foo < numParams; ++Foo) free(QuotedParams[Foo]);
	return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
    }
    s = Pgm; d = Arglist; SuccCode = 0;
    while (*s != '\0') {
	if (*s == '%') {
	    ++s;
	    if (*s == '%') *d++ = '%';
	    else if (*s == 's') {
		strcat(d, QuotedParams[SuccCode]);
		++SuccCode;
		d = &d[strlen(d)];
	    } else for (Foo = 0; Foo < numParams; ++Foo)
		if (*s == Params[Foo].Abbr) {
		    strcat(d, QuotedParams[Foo]);
		    d = &d[strlen(d)];
		    break;
		}
	} else *d++ = *s;
	++s;
    }
    *d = '\0';
    for (Foo = 0; Foo < numParams; ++Foo) free(QuotedParams[Foo]);
    ProgBreak = NULL;
    for (s = Arglist; *s != '\0'; ++s) {
	if (*s == ' ' || *s == '\t' || *s == ';' || *s == '&' || *s == '(' || *s == ')' || *s == '|' || *s == '<' || *s == '>' || *s == '\n') break;
    }
    if (*s != '\0') {
	ProgBreak = s;
	Foo = *s;
	*s = '\0';
	if (Arglist[0] != '\0') {
	    if (stat(Arglist, &sblock) < 0) {
		Foo = AddrResult(tmltf_MailerSystemFailure, Addr, "Can't stat %s: %s", Arglist, UnixError(errno));
		free(Arglist);
		return Foo;
	    }
	    if (((sblock.st_mode & S_IFMT) != S_IFREG) || (sblock.st_mode & 0111) == 0) {
		Foo = AddrResult(tmltf_MailerSystemFailure, Addr, "Unusable mode %#o for program %s", sblock.st_mode, Arglist);
		free(Arglist);
		return Foo;
	    }
	}
	*s = Foo;
    }
    NMax = OpenStripBE(&BES, Hdr, UseFmt ? AlwaysOK : Normal);
    if (NMax != 0) {
	free(Arglist);
	if (NMax < 0) {
	    return PermMessage(tmlpf_DropoffFailure, Addr, OSBE_Brief, "%s", OSBE_Text);
	} else {
	    return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}
    }
    OpenUnscribe(BES);
    if (BES->UnScrValue != NULL && BES->UnScribeCode == -2) {
	free(Arglist);
	CloseStripBE(BES); BES = NULL;
	return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
    }
    errno = 0;
    if (ReallyDeliver)
	RunningPgm = popen(Arglist, "w");
    else
	RunningPgm = fopen("/dev/null", "w");
    if (RunningPgm == NULL) {
	free(Arglist);
	AbortUnscribe(BES);
	CloseStripBE(BES); BES = NULL;
	return AddrResult(tmltf_MailerSystemFailure, Addr, "popen failed: %s", UnixError(errno));
    }
    errno = 0;
    if (ProgBreak != NULL) *ProgBreak = '\0';
    WriteReceived(RunningPgm, ForString, ProgBreak == NULL ? NULL : Arglist);
    free(Arglist);
    DefaultMsgID(BES->Header, RunningPgm);
    CharsRead = -1;
    Foo = StripBE(BES, RunningPgm, &CharsRead);
    if (Foo != 0) {
	AbortUnscribe(BES);
	fclose(RunningPgm);
	CloseStripBE(BES); BES = NULL;
	return AddrResult(tmltf_MailerSystemFailure, Addr, "StripBE failed: %s", UnixError(errno));
    }
    if (CharsRead < TMGlobalMinimumSize || CharsRead < MinimumFileSize) {
	AbortUnscribe(BES);
	fclose(RunningPgm);
	CloseStripBE(BES); BES = NULL;
	AddrResult(tmltf_MailTransputError, Addr, "Input file too short--%d out of %d bytes", CharsRead, (MinimumFileSize > TMGlobalMinimumSize ? MinimumFileSize : TMGlobalMinimumSize));
    }
    if (CloseUnscribe(BES, RunningPgm) < 0) {
	Foo = errno;
	fclose(RunningPgm);
	CloseStripBE(BES); BES = NULL;
	return AddrResult(tmltf_MailerSystemFailure, Addr, "CloseUnscribe failed: %s", UnixError(Foo));
    }
/* All piped off--now close and decipher */
    errno = 0;
    if (ReallyDeliver) {
	SuccCode = pclose(RunningPgm);
	if (TMDebug) fprintf(stderr, "RunningPgm ret status is %#x\n", SuccCode);
	if (SuccCode != 0) {
	    Foo = errno;
	    CloseStripBE(BES); BES = NULL;
	    if ((SuccCode & 0377) != 0) {
		return AddrResult(tmltf_MailerExecFailure, Addr, "Program aborted with signal: %s", UnixSignal(SuccCode & 0177));
	    } else {
		return AddrResult(tmltf_MailerSystemFailure, Addr, "Program exited with status %d", SuccCode >> 8);
	    }
	}
    } else {
	fclose(RunningPgm);
    }
    CloseStripBE(BES); BES = NULL;
    return AddrResult(tmok_DroppedOffToPgm, Addr, "dropped off to program");
}

int DoNNTP(Addr, WName, MBox, ForString, InLoop, PKey)
PARSED_ADDRESS *Addr;
char *WName, *MBox, *ForString;
int InLoop;
wp_PrimeKey PKey;
{	/* Deliver a piece of mail to netnews (via NNTP). */
	char *XmitText, *XmitUser, *XmitAffil, *MsgHdr, *XmitDest;
	int XTCount, XTSize, C, PStat, SigErr, WasTempFail, TimedOut, CharsRead;
	long int LongStat;
	char *XmitArgv[6];
	char ErrorReason[200];
	char CellName[150];
	char XmitFileName[130];
	struct BEStripper *BES = NULL;
	FILE *XmitFile; int XmitDesc;
	int XmitArgc;
	FILE *Xmit;
#define InitialXmitTextSize 100

	if ((GlobalOptions & tmopt_NNTPDropoff) == 0)
	    return AddrResult(tmltf_NotImplementedHere, Addr, "NNTP delivery not called for");
	(void) sprintf(XmitFileName, "/usr/tmp/%s", ams_genid(1));
	XmitDesc = open(XmitFileName, O_WRONLY|O_CREAT|O_TRUNC, 0600);
	if (XmitDesc < 0) {
		return AddrResult(tmltf_MailTransputError, Addr,
			"Can't create temp file %s: %s", XmitFileName, UnixError(errno));
	}
	XmitFile = fdopen(XmitDesc, "w");
	if (XmitFile == NULL) {
		unlink(XmitFileName); close(XmitDesc);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}
	XmitDest = AddrUnparse(Addr, &LongStat);
	if (XmitDest == NULL) {
		unlink(XmitFileName); close(XmitDesc);
		return AddrResult(tmltf_MemoryExhausted, Addr,
			"No memory: %d", LongStat);
	}
	C = GetHeader(&MsgHdr);
	if (C != 0) {
		unlink(XmitFileName); fclose(XmitFile); free(XmitDest);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}
	C = WasThisSent(MsgHdr, XmitDest, Addr);
	if (C > 0) {
		unlink(XmitFileName); fclose(XmitFile); free(XmitDest);
		return AddrResult(tmok_RedundantDelivery, Addr, "message already delivered");
	}
	C = OpenStripBE(&BES, MsgHdr, NeverOK);	/* never OK for NNTP: they delete headers */
	if (C != 0) {
		unlink(XmitFileName); fclose(XmitFile);
		free(XmitDest); free(MsgHdr); CloseStripBE(BES);
		if (C > 0 || ULstrcmp(GlobalDom, WorkstationCell) != 0) {
			return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
		} else {
			return PermMessage(tmlpf_DropoffFailure, Addr,
					OSBE_Brief, "%s", OSBE_Text);
		}
	}
	OpenUnscribe(BES);
	if (TMDebug) fprintf(stderr, "Does%s have header; UnScribeCode is %d.\n",
				(BES->xasfLineBegin == NULL ? " not" : ""),
				BES->UnScribeCode);
	if (BES->xasfLineBegin != NULL && BES->UnScribeCode == -2) {
		free(MsgHdr); unlink(XmitFileName); free(XmitDest);
		fclose(XmitFile); CloseStripBE(BES);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}
	/* DefaultMsgID(BES->Header, XmitFile); REMOVED 15Apr88 CFE; the NNTP receiver needs to be able to recognize retransmissions of the same article. */
	WriteReceived(XmitFile, ForString, NULL);
	C = StripBE(BES, XmitFile, &CharsRead);
	if (C != 0) {
		SigErr = errno;
		AbortUnscribe(BES); CloseStripBE(BES);
		free(MsgHdr); unlink(XmitFileName); fclose(XmitFile); free(XmitDest);
		if (C == -1) {
		    if (tfail(SigErr) || ULstrcmp(GlobalDom, WorkstationCell) != 0) {
			return AddrResult(tmltf_MailTransputError, Addr,
				"StripBE: error %s", UnixError(SigErr));
		    } else {
			return PermMessage(tmlpf_DropoffFailure, Addr, "StripBE failure",
			    "StripBE ran into a permanent failure: %s", UnixError(SigErr));
		    }
		} else {
		    if (C == -6) {
			return PermMessage(tmlpf_DropoffFailure, Addr,
				"internal UnScribe error",
"The message appears to have an unusually long style name, and is probably not really a base editor document.  This usually is a symptom of a bug in the message-composing program that you can circumvent by avoiding backslashes in your message text.");
		    } else {
			return AddrResult(tmlpf_DropoffFailure, Addr,
				"internal UnScribe error: %d", C);
		    }
		}
	}
	if (CloseUnscribe(BES, XmitFile) < 0) {
		SigErr = errno;
		CloseStripBE(BES);
		free(MsgHdr); unlink(XmitFileName); fclose(XmitFile); free(XmitDest);
		return AddrResult(tmltf_MailTransputError, Addr, UnixError(SigErr));
	}
	CloseStripBE(BES); BES = NULL;
	if (ferror(XmitFile) || feof(XmitFile)) {
		SigErr = (errno == 0 ? EIO : errno);
		unlink(XmitFileName); fclose(XmitFile);
		free(MsgHdr); free(XmitDest);
		return AddrResult(tmltf_MailTransputError, Addr,
			"Temp file %s write error: %s", XmitFileName, UnixError(SigErr));
	}
	if (fclose(XmitFile) == EOF) {
		SigErr = (errno == 0 ? EIO : errno);
		unlink(XmitFileName);
		free(MsgHdr); free(XmitDest);
		return AddrResult(tmltf_MailTransputError, Addr,
			"Can't close temp file %s: %s", XmitFileName, UnixError(SigErr));
	}
	if (CharsRead >= 0 &&
	    (CharsRead < TMGlobalMinimumSize || CharsRead < MinimumFileSize)) {
		free(MsgHdr); free(XmitDest);
		unlink(XmitFileName);
		return PermMessage(tmlpf_DropoffFailure, Addr, "Input file too short",
		 "Input file %s is only %d bytes long, but is required to be at least %d bytes long.",
			CharsRead,
			(TMGlobalMinimumSize > MinimumFileSize ?
				TMGlobalMinimumSize : MinimumFileSize));
	}
	XTSize = InitialXmitTextSize;
	XmitText = malloc(XTSize+1);
	if (XmitText == NULL) {
		unlink(XmitFileName);
		free(MsgHdr); free(XmitDest);
		return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
	}

	C = -1;
	if (AuthString != NULL && (isdigit(AuthString[0]) || AuthString[0] == '-')) {
		C = atoi(AuthString);
	} else {
		C = getvuid();
	}
	CellName[0] = '\0';
	XmitUser = index(AuthString, ';');
	if (XmitUser != NULL) {
	    strncpy(CellName, ++XmitUser, sizeof(CellName));
	    if ((XmitUser = index(CellName, ';')) != NULL) *XmitUser = '\0';
	    else CellName[0] = '\0';
	}
	if (CellName[0] == '\0') strncpy(CellName, GlobalDom, sizeof(CellName));
	XmitUser = XmitAffil = NULL;
	if (IDtoName(C, &XmitUser, &XmitAffil, CellName) != wperr_NoError) XmitUser = NULL;
	if (XmitUser == NULL)
	    strcpy(ErrorReason, NNTPuser);
	else
	    sprintf(ErrorReason, "%s+", XmitUser);
	if (XmitAffil != NULL && islower(*XmitAffil)) XmitAffil = NULL;
	XmitUser = malloc((XmitAffil == NULL ? 0 : strlen(XmitAffil)) + strlen(Organization) + 5);
	if (XmitUser != NULL) {
	    if (XmitAffil == NULL) strcpy(XmitUser, Organization);
	    else sprintf(XmitUser, "%s, %s", XmitAffil, Organization);
	}

	XmitArgc = -1;
	XmitArgv[++XmitArgc] = nntpxmit;
	XmitArgv[++XmitArgc] = XmitFileName;
	XmitArgv[++XmitArgc] = ErrorReason;
	XmitArgv[++XmitArgc] = XmitUser;
	XmitArgv[++XmitArgc] = NULL;

	if (TMDebug) {
		fprintf(stderr, "Sending NNTP with command: %s", XmitArgv[0]);
		for (C = 1; XmitArgv[C] != NULL; C++) fprintf(stderr, ", %s", XmitArgv[C]);
		fprintf(stderr, "\n");
	}
	WasTempFail = TimedOut = 0;
	errno = 0;
	Xmit = topen(XmitArgv[0], XmitArgv, "r", &PStat);
	if (Xmit == NULL) {
		unlink(XmitFileName);
		if (XmitUser != NULL) free(XmitUser);
		free(XmitText); free(XmitDest);
		free(MsgHdr);
		return AddrResult(tmltf_MailerExecFailure, Addr, "Can't exec %s: %s",
				XmitArgv[0], UnixError(errno));
	}
	XTCount = 0;
	for (;;) {
		C = getc(Xmit);
		if (C == EOF) break;
		if (XTCount >= XTSize) {
			XTSize *= 2;
			XmitText = realloc(XmitText, XTSize+1);
			if (XmitText == NULL) {
				killpg(PStat, SIGKILL);
				if (XmitUser != NULL) free(XmitUser);
				free(MsgHdr); free(XmitDest);
				unlink(XmitFileName);
				return AddrResult(tmltf_MemoryExhausted, Addr, "No memory");
			}
		}
		XmitText[XTCount++] = C;
	}
	while (XTCount > 0 && XmitText[XTCount-1] == '\n') --XTCount;
	XmitText[XTCount] = '\0';
	C = tclose(Xmit, 15*60, &TimedOut);	/* fifteen minute timeout on mail dropoff */
	if (TMDebug) fprintf(stderr, "nntpxmit returns code 0x%x, timeout %d, text ``%s''.\n",
				C, TimedOut, XmitText);
	if (XmitUser != NULL) free(XmitUser);
	if (TimedOut) {
		killpg(PStat, SIGKILL);
		strcpy(ErrorReason, "timed out");
		WasTempFail = 1;
	} else if (C == 0) {
		unlink(XmitFileName);
		ThisWasSent(MsgHdr, XmitDest, ForString, 1);
		free(MsgHdr);
		return AddrResult(tmok_DroppedOffForNet, Addr, "sent via NNTP");
	} else if ((C & 0xff) != 0) {
		(void) sprintf(ErrorReason, "signal %d (%s)",
				C & 0x7f, UnixSignal(C & 0x7f));
		WasTempFail = 1;
	} else {
		(void) sprintf(ErrorReason, "status %d (%s)",
				C >> 8, UnixSysExits(C >> 8));
		switch (C >> 8) {
		case 1:	/* we get this when nntpxmit wasn't executable */
		case EX_TEMPFAIL:
			WasTempFail = 1;
		}
	}
	free(MsgHdr); MsgHdr = NULL;
	free(XmitDest);
	if (WasTempFail || ULstrcmp(GlobalDom, WorkstationCell) != 0) {
		SigErr = AddrResult(tmltf_MailerSystemFailure, Addr, "nntpxmit %s: %s",
				ErrorReason, XmitText);
	} else {
		SigErr = PermMessage(tmlpf_MailerPermFailure, Addr,
			"Netnews posting failure",
			"The NNTP %s %s; its complaint was ``%s''.",
			(TimedOut ? "transmission" : "transmitter exited with"),
			ErrorReason, XmitText);
	}
	unlink(XmitFileName);
	free(XmitText);
	return SigErr;
}
