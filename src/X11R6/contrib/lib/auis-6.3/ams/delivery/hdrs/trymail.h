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
	trymail.h
	Declaration file for trymail service.
	Defines the constants used in the protocol between trymail and its client.
\* ************************************************************ */

/*
The exit status returned by trymail is constructed as the bitwise OR of values representing different conditions.
*/
/* No error conditions encountered. */
#define tmexit_OK 		0000
/* Delivery to some addressees failed but could be tried again later with some chance of success. */
#define tmexit_SomeTempFail	0001
/* Delivery to some addressees will never succeed. */
#define tmexit_SomePermFail	0002
/* A global temporary failure--requeue or try it again later. */
#define tmexit_GlobalTempFail	0040
/* A global permanent failure; retrying this invocation will never succeed. */
#define tmexit_GlobalPermFail	0100

/* Arguments to the -C1 and -C0 switches (decimal integers).  If present, these say what options to enable and what to disable.  Presence allows the named function.  Comment says whether the function is implemented only in switchmail or in both trymail and switchmail.  Switchmail defaults to doing everything; trymail defaults to only HomeDelivery and CrossCellDropoff. */
#define	tmopt_HomeDelivery	0001	/* Delivery to home directories (both) */
#define	tmopt_CrossCellDelivery	0002	/* Delivery to other cells' mail queues (switchmail) */
#define	tmopt_CrossCellDropoff	0004	/* Delivery to queues of cells in which caller is authenticated (both) */
#define	tmopt_SendmailDropoff	0010	/* Give mail to OldSendmailProgram (switchmail) */
#define	tmopt_NNTPDropoff	0020	/* Give mail to nntpxmit program (switchmail) */
#define	tmopt_ProgDelivery	0040	/* Give mail to PGMFMT/PGMSTRIP (switchmail) */
#define	tmopt_NonAuthDelivery	0100	/* Deliver to mailboxes in cells where you're not authenticated (both) */
#define	tmopt_ReturnErrors	0200	/* On persistent errors, dispose of mail by returning it to the sender (switchmail) */

/*
Each message returned from trymail is a digit string (without leading zeroes) followed by a flag character followed by text followed by a newline character.  Multi-line messages are handled by having the flag character for all lines but the last one be a hyphen, while the flag character for the last line of a message is a space.
*/
#define tmflag_More ('-')
#define tmflag_End (' ')

/*
Trymail encodes results in the values of the digit string.  Global error conditions result in a 7xx or an 8xx digit string being returned and no messages having been delivered.  Per-address conditions use 1xx, 2xx, or 3xx digit strings.  The string 999 is reserved for the end of the collection of addresses.
*/

#define tm_EndOfInteraction	999

#define tm_GlobalTempFailLowest	700
#define tm_GlobalTempFailHighest	799
#define tmgtf_MemoryExhausted	701
#define tmgtf_InputFileOpenFailure	705
#define tmgtf_ErrorReturningError	720
#define tmgtf_UIDError		730
#define tmgtf_ConfigError		740

#define tm_GlobalPermFailLowest	800
#define tm_GlobalPermFailHighest	899

#define tmgpf_UnknownOption	801
#define tmgpf_MissingFileToSend	802
#define tmgpf_MissingReturnPath	803
#define tmgpf_ExtraArguments	804
#define tmgpf_RetPathParseError	805
#define tmgpf_DestinationSyntaxError	806
#define tmgpf_DestinationParseError	807
#define tmgpf_RetPathLexicalError	808
#define tmgpf_ZeroLengthFile	809
#define tmgpf_FileTooShort		810
#define tmgpf_InputFileOpenFailure	811
#define tmgpf_CellError		812

/*
The following codes can be prefixed to messages regarding a single addressee.  The text portion of the message (all but the coded digit string) for a 2xx message is an RFC822-format address.  The text portions of all the other codes are free-format human-readable text.
*/
#define tm_AddrDeliveredLowest	100
#define tm_AddrDeliveredHighest	199
#define tmok_DeliveredGoodPrint	101
#define tmok_DeliveredCheapPrint	102
#define tmok_DroppedOffToPgm	130
#define tmok_DroppedOffForNet	140
#define tmok_DroppedOffToList	150
#define tmok_DroppedOffToCell	160
#define tmok_RequestedNotToSend	170
#define tmok_RedundantDelivery	180
#define tmok_ErrorMsgReturned	190

#define tm_AddrQueueLowest	200
#define tm_AddrQueueHighest	299
/* Codes for need for queueing. */
/* Address non-local: 21x. */
#define tm_AddrNonLocalLowest	210
#define tm_AddrNonLocalHighest	219
#define tmltf_NonLocalHost		210
/* Some temporary failure: 22x. */
#define tm_AddrTempFailLowest	220
#define tm_AddrTempFailHighest	229
#define tmltf_MemoryExhausted	220
#define tmltf_WhitePagesInaccessible	222
#define tmltf_WhitePagesRunFailure	223
#define tmltf_ErrorReturningMsg	224
#define tmltf_MailerExecFailure	225
#define tmltf_MailTransputError	226
#define tmltf_MailerSystemFailure	227
#define tmltf_MailerTempFailure	228
#define tmltf_ViceInaccessible	229
/* Error in forwarding: 23x. */
#define tm_AddrForwardedLowest	230
#define tm_AddrForwardedHighest	239
#define tmltf_MailForwarded	230
#define tmltf_MailFwdInternalError	231
#define tmltf_MailFwdUnknown	232
#define tmltf_MailFwdBadParse	233
#define tmltf_MailFwdBadResolve	234
#define tmltf_FileUnreachable	235
#define tmltf_ForwardedOutOfAuth	236
/* Generic internal error: 24x. */
#define tm_AddrInternalErrorLowest	240
#define tm_AddrInternalErrorHighest	249
#define tmltf_CannotFindRequiredField 240
/* Temp failures with alternate delivery: 25x. */
#define tm_AlternateDeliveryLowest 250
#define tm_AlternateDeliveryHighest 259
#define tmltf_NotImplementedHere	250
/* Temp failures due to cross-cell semantics: 26x. */
#define tm_CrossCellTempFailLowest 260
#define tm_CrossCellTempFailHighest 269
#define tmltf_CrossCellNonReject 261

#define tm_AddrPermFailLowest	300
#define tm_AddrPermFailHighest	399
/* Codes for kinds of permanent failures. */
/* Syntax error in addressee: 31x. */
#define tm_AddrSyntaxLowest	310
#define tm_AddrSyntaxHighest	319
#define tmlpf_MailFwdSyntaxError	310
#define tmlpf_LocalNameError	311
/* No such addressee in this site's collection (``No such user''): 32x. */
#define tm_AddrUnknownLowest	320
#define tm_AddrUnknownHighest	329
#define tmlpf_AddresseeUnknown	320
#define tmlpf_NoMailbox		321
#define tmlpf_NoMailboxInLoop	322
/* Addressee specification ambiguous: 33x. */
#define tm_AddrAmbiguousLowest	330
#define tm_AddrAmbiguousHighest	339
#define tmlpf_AddresseeAmbiguous	330
#define tmlpf_AddresseeFuzzy	331
/* Problems recognizing the outermost host: 34x. */
#define tm_AddrWhatHostLowest	340
#define tm_AddrWhatHostHighest	349
/* Problems finding forwarding information: 35x. */
#define tm_AddrForwardInfoLowest	350
#define tm_AddrForwardInfoHighest	359
#define tmlpf_BadForwardingInfo	350
#define tmlpf_FileUnreachable	351
#define tmlpf_NoOwner		352

#define tmlpf_MailerPermFailure	360
#define tmlpf_DropoffFailure		361
#define tmlpf_UnusableAddress	362
#define tmlpf_ErrorReturningResp	363

/* Permanent problems writing the mail: 37x. */
#define tm_MailWritingLowest	370
#define tm_MailWritingHighest	379
#define tmlpf_CantOpen		370
#define tmlpf_CantWrite		371
#define tmlpf_CantClose		372

/* Permanent problems with alternate delivery: 38x. */
#define tm_PermAltDelLowest	380
#define tm_PermAltDelHighest	389
#define tmlpf_NoSuchDelKind	380
#define tmlpf_ParameterError	381

