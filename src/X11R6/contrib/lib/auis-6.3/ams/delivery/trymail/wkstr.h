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
	wkstr.h
	Declaration file exporting interface to strong.c and weak.c.
\* ************************************************************ */

#include <fdplumb.h>
extern FILE *fopen();
extern FILE *popen();
extern FILE *topen();

struct FwdLink {
	struct FwdLink	*Parent;
	char		*WholeName;
	char		*MailBox;
	wp_PrimeKey	KeyVal;
	PARSED_ADDRESS *RmtFwded;
	PARSED_ADDRESS *fAddr;
	char		Scuttled;
	char		Rewritten;
};

/* Exported from trymail to the subprograms */
extern int AddrResult();
extern int PermMessage();
extern void CheapPrint();
extern int MakeFORString();
extern int MakeFile();
extern char *AddrUnparse();
extern int DoUnparse();
extern wp_ErrorCode GetWPField();
extern wp_ErrorCode IDtoName();
extern int CheckAMSDelivery();
extern int TMDebug;
extern int DoTiming, ReallyDeliver, IsExpired, IsExtraExpired, IsALittleExpired, IsSomeExpired, IsCrossCellExpired;
extern int MinimumFileSize, EnqueueDate, GlobalOptions;
#define TMGlobalMinimumSize 2
extern char *AuthString, *GlobalForString,
	*InFileName, *TextRetPath, *TextDestinations,
	*UnparseBuff, *GlobalDom;
extern FILE *InFile;
extern struct stat InFileStat;
extern int InFileErrno;
extern PARSED_ADDRESS *ParsedRetPath, *RootTemp, *ParsedDestinations;
extern int OrigDestinationCount;
extern char *homeCell;
extern int WpOpen;
#define WpOpen_NotOpen 0
#define WpOpen_CannotOpen 1
#define WpOpen_Open 2
extern char WpOpenCell[];
extern int gotWPIndices;
extern wp_FieldIndex idxN, idxFwd, idxID, idxHD, idxEK, idxAf, idxDK, idxDP, idxNPA;
extern int WantWPIndices(), GetWPIndices();
extern void ThisIsRemote();

/* Imported to trymail from the subprograms */

/* For determining whether there's really a forwarding file for a home directory */
extern char *FindErrString();
extern int FindForwarding();

/* Handle remote recipients.  AppendRemote gets a chance to construct something for each recipient, and DealWithRemoteList must handle each named remote list.  (RemoteGotDelivered can reclaim an AppendRemote entry if something need not be dealt with by DealWithRemoteList.)  If it wants, it can simply use the argument, which is a PARSED_ADDRESS list naming the saved remote addresses. */
extern int AppendRemote();
extern int RemoteGotDelivered();
extern void DealWithRemoteList();

/* ReturnError() sends off an error message to the Return-Path.  It returns 0 if there's every expectation that the message will get there, and a non-zero code otherwise.  ReturnAmbig() will compose and send an error message about an ambiguous recipient, returning similar error codes. */
extern int ReturnError();
extern int ReturnAmbig();
extern int ReturnTempFail;	/* Boolean flag set in ReturnError and ReturnAmbig */

/* OpenPMAdvisory() sets up to send an advisory note to the postmaster.  Use PMAdvPrint, like printf, to give the details of the report.  ClosePMAdvisory finishes up the message and sends it. */
extern FILE *OpenPMAdvisory();
extern void PMAdvPrint();
extern int ClosePMAdvisory();

/* DoNNTP does NNTP mail delivery (really NNTP posting), returning 0 on success and non-0 on failure, just like WriteLocalMail. */
extern int DoNNTP();

/* DoProg does PGMFMT and PGMSTRIP mail delivery, returning 0 on success and non-0 on failure, just like WriteLocalMail. */
extern int DoProg();

/* Return a pointer to a Message-ID if the Hdr lacks one. */
extern int GetDefaultMsgID();

/* Write a Message-ID to a file if a header lacks one. */
extern int DefaultMsgID();

/* Consult a database of recently-sent messages to determine if this message has been sent to this address recently.  Return < 0 if can't tell, 0 if it wasn't sent, +1 if it was and we reported it, > 1 if it was but we couldn't report it.  (-1 means errno is valid; -22 means this is trymail and we're not trying hard here.) */
extern int WasThisSent();

/* Note in the database of recently-sent messages that a message with the given header was sent to the given destination. */
extern void ThisWasSent();

/* Close down the database of recently-sent messages, if it's open */
extern int FinishSent();

/* Read the header from the message into dynamic memory.  Return -3 if this is trymail, -2 if no memory, errno if errors encountered, 0 if all-OK.  If all-OK, pointer to header memory stored via its parameter. */
extern int GetHeader();

/* Tell whether we should work hard. */
extern int IsTrymail();
