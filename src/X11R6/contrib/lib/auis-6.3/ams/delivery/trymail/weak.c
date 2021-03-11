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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/delivery/trymail/RCS/weak.c,v 1.15 1993/05/04 01:03:50 susan Exp $";
#endif

/* ************************************************************ *\
	weak.c
	Subroutines (often dummies) for ``trymail''--the trymail version that
	runs on workstations and does not try hard to deliver mail.  Its
	determinations (e.g., ``no such user'') are viewed as non-authoritative,
	and it thus does not generate error messages.  It leaves such
	determinations for ``switchmail'' running on the postoffice machines.
\* ************************************************************ */

#include <stdio.h>
#include <truth.h>
#include <andrewos.h> /* sys/types.h */
#include <errno.h>
#include <sys/stat.h>
#include <pwd.h>

#include <util.h>
#include <mail.h>
#include <parseadd.h>
#include <wp.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

#include <trymail.h>
#include <wkstr.h>

/*ARGSUSED*/
char *FindErrString(dummy)
int dummy;
{
	return "Forwarding address indeterminate";
}

/*ARGSUSED*/
int FindForwarding(homeDir, FwdString)
char *homeDir, **FwdString;
{
/* If we can't find the user's forwarding string in the database, try to find it at delivery time.
   Allocate a copy of the file. */

	return -1;
}

/*ARGSUSED*/
int WasThisSent(Hdr, DestText, Addr)
char *Hdr, *DestText; PARSED_ADDRESS Addr;
{/* Consult a database of recently-sent messages to determine if this message has been sent to this address recently.  Return < 0 if can't tell, 0 if it wasn't sent, +1 if it was and we reported it, > 1 if it was but we couldn't report it.  (-1 means errno is valid; -22 means this is trymail and we're not trying hard here.)
  */
	return -22;
}

int FinishSent()
{/* Close down the database of recently-sent messages, if it's open */
	return 0;
}

int IsTrymail()
{/* Tell whether trymail.c should take it easy. */
	return 1;
}

/*ARGSUSED*/
int GetDefaultMsgID(Hdr, cPtr)
char *Hdr, **cPtr;
{/* Return a pointer to a Message-ID if the Hdr lacks one. */
	*cPtr = NULL;
	return 0;
}

/*ARGSUSED*/
int DefaultMsgID(Hdr, File)
char *Hdr; FILE *File;
{/* Write a Message-ID to File if the Hdr lacks one. */
	return 0;
}

/*ARGSUSED*/
void ThisWasSent(Hdr, DestText, ForClause, AllowDflt)
char *Hdr, *DestText, *ForClause; int AllowDflt;
{/* Note in the database of recently-sent messages that a message with the given header was sent to the given destination.
  */
	return;
}

/*ARGSUSED*/
int AppendRemote(Addr, Fwd)
PARSED_ADDRESS *Addr;
struct FwdLink *Fwd;
{
	return 0;
}

/*ARGSUSED*/
int RemoteGotDelivered(Addr)
PARSED_ADDRESS *Addr;
{
	return 0;
}

static void DealWithRemoteRecipient(Addr)
PARSED_ADDRESS *Addr;
{
	AddrResult(tmltf_NonLocalHost, Addr, "non-local host");
}

void DealWithRemoteList(AddrList)
PARSED_ADDRESS *AddrList;
{
/* Once other delivery is finished, either print out or deliver to the remote addresses. */

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			DealWithRemoteRecipient(ThisAddr);
			break;
	case GROUP_ADDRESS:
			DealWithRemoteList(ThisAddr->Members);
			break;
	default:
			break;
		}
	})
}

/*ARGSUSED*/
int DoNNTP(Addr, WName, MBox, ForString, InLoop, PKey)
PARSED_ADDRESS *Addr;
char *WName, *MBox, *ForString;
int InLoop;
wp_PrimeKey PKey;
{	/* It's known Val is not standard delivery, so we punt to switchmail. */
	AddrResult(tmltf_NotImplementedHere, Addr, "NNTP delivery postponed");
	return tmltf_NotImplementedHere;
}

/*ARGSUSED*/
int DoProg(Addr, Hdr, ForString, PKey, Fwd, Dom, UseFmt)
PARSED_ADDRESS *Addr;
char *Hdr, *ForString, *Dom;
wp_PrimeKey PKey;
struct FwdLink *Fwd;
int UseFmt;
{	/* It's known Val is not standard delivery, so we punt to switchmail. */
	AddrResult(tmltf_NotImplementedHere, Addr, "Program delivery postponed");
	return tmltf_NotImplementedHere;
}

/*ARGSUSED*/ /*VARARGS4*/
int ReturnError(Subj, EncloseMsg, ToPostman, Addr, Fmt, p1, p2, p3, p4, p5)
char *Subj; int EncloseMsg, ToPostman; char *Fmt; PARSED_ADDRESS *Addr;
char *p1, *p2, *p3, *p4, *p5;
{/* Dummy for returning an error message--indicate that no error message was sent. */
	ReturnTempFail = FALSE;
	return 1;
}

/*ARGSUSED*/
int ReturnAmbig(Addr, wpCD, STok, ToPostman, MinMatch, OutMatch, Dom)
PARSED_ADDRESS *Addr; struct wp_cd *wpCD; wp_SearchToken STok;
int ToPostman, MinMatch, OutMatch; char *Dom;
{/* Dummy for composing and returning an error message about an ambiguous addressee. */
	ReturnTempFail = FALSE;
	return 1;
}

/*ARGSUSED*/
int ReturnFuzzy(Addr, wpCD, STok, PKey, ToPostman, MinMatch, OutMatch, Dom)
PARSED_ADDRESS *Addr; struct wp_cd *wpCD; wp_SearchToken STok; wp_PrimeKey PKey;
int ToPostman, MinMatch, OutMatch; char *Dom;
{/* Dummy for composing and returning an error message about a fuzzily-matched addressee. */
	ReturnTempFail = FALSE;
	return 1;
}

/*ARGSUSED*/
FILE *OpenPMAdvisory(Subj, EncloseMsg, Addr, Dom)
char *Subj; int EncloseMsg; PARSED_ADDRESS *Addr; char *Dom;
{/* Dummy for sending an advisory message to the postmaster. */
	return NULL;
}

void PMAdvPrint()
{
}

int ClosePMAdvisory()
{
	return 1;
}

