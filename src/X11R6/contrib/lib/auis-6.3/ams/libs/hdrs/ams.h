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

/*
	ams.h -- Include file for Andrew message server/CUI clients.
*/

/* Version numbers for server-client interface:  When the
	major versions are inconsistent, it spells trouble.
	Changing the version numbers (in amsvers.h) will cause
	the cui to reject an older message server, kill it, and
	try to reconnect.  If the reconnection yields another out of date
	server, however, it will try to muddle through after printing a
	warning message.  That might be a mistake.
*/
#include <amsvers.h>

#define NIL (char *) 0

#include <mserrno.h>

/* The following structure will be passed from the message server
	to clients as a "snapshot" of a message.  Other data
	available about the message will be available as text
	strings -- body, rawheaders, reply-to, wide-reply,
	and specific headers -- which are obtained via
	calls to the message server and are stored in different
	formats on the two ends (server and client).

   NOTE:  This data is packed into a single character array and
	accessed via macros to help insure portability, since we'll
	be reading the data in directly from a byte stream in a file.
*/

#define AMS_DATESIZE 7		/* Fixed size of compacted date */
#define AMS_CAPTIONSIZE 89	/* Fixed size of caption line */
#define AMS_CHAINSIZE 4		/* Fixed size of chain number */
#define AMS_MIDHASHSIZE 4	/* Fixed size of message id hash */
#define	AMS_REPLYHASHSIZE 4	/* Fixed size of in-reply-to or references hash */
#define AMS_ATTRIBUTESIZE 21	/* Fixed size of msg attributes */
#define AMS_IDSIZE 19		/* Fixed size of unique ID name */

#define AMS_SNAPSHOTSIZE (AMS_DATESIZE + AMS_CAPTIONSIZE + AMS_CHAINSIZE + AMS_MIDHASHSIZE + AMS_REPLYHASHSIZE + AMS_ATTRIBUTESIZE + AMS_IDSIZE)

#define AMS_DATEOFFSET 0
#define AMS_DATE(s) ((char *) (s))
#define AMS_CAPTIONOFFSET AMS_DATEOFFSET + AMS_DATESIZE
#define AMS_CAPTION(s) (((char *) (s)) + AMS_CAPTIONOFFSET)
#define AMS_CHAINOFFSET (AMS_CAPTIONOFFSET + AMS_CAPTIONSIZE)
#define AMS_CHAIN(s) (((char *) (s)) + AMS_CHAINOFFSET)
#define AMS_MIDHASHOFFSET (AMS_CHAINOFFSET + AMS_CHAINSIZE)
#define AMS_MIDHASH(s) (((char *) (s)) + AMS_MIDHASHOFFSET)
#define AMS_REPLYHASHOFFSET (AMS_MIDHASHOFFSET + AMS_MIDHASHSIZE)
#define AMS_REPLYHASH(s) (((char *) (s)) + AMS_REPLYHASHOFFSET)
#define AMS_ATTRIBUTEOFFSET (AMS_REPLYHASHOFFSET + AMS_REPLYHASHSIZE)
#define AMS_ATTRIBUTES(s) (((char *) (s)) + AMS_ATTRIBUTEOFFSET)
#define AMS_IDOFFSET (AMS_ATTRIBUTEOFFSET + AMS_ATTRIBUTESIZE)
#define AMS_ID(s) (((char *) (s)) + AMS_IDOFFSET)

/* The following define the meaning of the attributes field.
	The numbers are to be interpreted as bits in the successive
	bytes in the attributes field; they are best accessed through
	the AMS_GET_ATTRIBUTE, AMS_SET_ATTRIBUTE, and AMS_UNSET_ATTRIBUTE
	macros defined below. */

#define AMS_GET_ATTRIBUTE(s, a) (AMS_ATTRIBUTES(s)[(a)/8] & 1<<((a) % 8))
#define AMS_SET_ATTRIBUTE(s, a) (AMS_ATTRIBUTES(s)[(a)/8] |= 1<<((a) % 8))
#define AMS_UNSET_ATTRIBUTE(s, a) (AMS_ATTRIBUTES(s)[(a)/8] &= ~(1<<((a) % 8)))

#define AMS_ATT_RRR 0  		/* Return Receipt Requested */
#define AMS_ATT_ENCLOSURE 1		/* Parcel Post */
#define AMS_ATT_DELETED 2	/* Marked for deletion */
#define AMS_ATT_NEWDIR 3	/* Announcing a new message subdirectory */
#define AMS_ATT_FORMATTED 4	/* Multimedia format file */
#define AMS_ATT_MAYMODIFY 5	/* Message this user may alter */
#define AMS_ATT_UNSEEN 6	/* Message is marked as "unseen" */
#define AMS_ATT_UNAUTH 7	/* Message sender is unauthenticated */
#define AMS_ATT_FROMNET 8	/* Message sender is from remote machine */
#define AMS_ATT_VOTE 9		/* Message calls for a vote */
#define AMS_ATT_URGENT 10	/* User marked this message as Urgent */
#define AMS_ATT_CLOSED 11	/* User marked this messge as Closed */
#define	AMS_ATT_REPLIEDTO 12	/* Message has been replied to */
#define AMS_ATT_REDISTRIBUTION 13 /* Message has a redistribution specification */

/* The current configuration of UATTRs (user attributes) means that the
    last predefined attribute would be number 135.  */

#define AMS_ATT_LAST_UATTR ((21 * 8) - 1)
#define AMS_ATT_UATTR(a) (AMS_ATT_LAST_UATTR - (a))
#define AMS_NUM_UATTRS 32
#define AMS_ATTRNAMEMAX 16


/* The following defines the subscription file, which lists the subscription
	information for the current user */
#ifndef AMS_SUBSCRIPTIONMAPFILE
#if defined(hpux) || defined(M_UNIX)
#define AMS_SUBSCRIPTIONMAPFILE ".SubsMap"
#else /* hpux */
#define AMS_SUBSCRIPTIONMAPFILE ".SubscriptionMap"
#endif /* hpux */
#endif /* AMS_SUBSCRIPTIONMAPFILE */


#define AMS_UNSUBSCRIBED 0
#define AMS_ASKSUBSCRIBED 1
#define AMS_ALWAYSSUBSCRIBED 2
#define AMS_SHOWALLSUBSCRIBED 3
#define AMS_PRINTSUBSCRIBED 4

/* The following define the disambiguation calls */
/* They used to be TRUE or false -- those still are used but should be cleaned up */

#define AMS_DISAMB_EXISTS 0
#define AMS_DISAMB_DIREXISTS 1
#define AMS_DISAMB_FILEEXISTS 2
#define AMS_DISAMB_FILEMAYBECREATED 3
#define AMS_DISAMB_ISAFILE 4
#define AMS_DISAMB_ISADIR 5

/* Definitions for formulation of a reply file */

#define AMS_REPLY_FRESH 1
#define AMS_REPLY_FORWARD 2 /* Forward stripping out formatting */
#define AMS_REPLY_WIDE 3
#define AMS_REPLY_SENDER 4
#define AMS_REPLY_WIDER 5
#define AMS_REPLY_REDRAFT 6
#define AMS_REPLY_ACK 7
#define AMS_REPLY_FORWARD_FMT 8 /* Forward with formatting */
/*VARARGS*/
extern int dbgprintf();
/* XXX We can remove _IBMR2 from the next ifdef when we run the compiler
 * in full ANSI mode.
 */
#if !defined(__STDC__) && !defined(_IBMR2)
/*VARARGS*/
extern int printf();
/*VARARGS*/
extern int fprintf();
#endif /* __STDC__ */
/*VARARGS*/
extern int safefprintf();
/*VARARGS*/
extern int errprintf();
/*VARARGS*/
extern int GR_SEND();

/* Status for options for sending mail */

#define AMS_SEND_BLINDNO 0 /* Backward compatibility only -- not needed */

#define AMS_SEND_BLINDYES 1 /* Send a blind copy of this mail */
/* Op code 2 only used by PC Mesages 1.3; can be ignored */
#define AMS_SEND_ISRESEND 4 /* This is really a resent-from, rather than from, etc. */
#define AMS_SEND_DO_NOT_USE_THIS 8 /* Backward compatibility hack */
#define AMS_SEND_ILLEGAL 16 /* Override checks for illegal message bodies */
#define AMS_SEND_UNFORMATTED 32 /* Strip out formatting befor sending */
#define	AMS_SEND_INSISTFORMATTED 64 /* Sets If-Type-Unsupported to "send" */
#define	AMS_SEND_INSISTTRUST 128    /* Sets If-Type-Unsupported to "alter" */

/* Symbolic values for Boolean operations (e.g. MS_SetSnapshot) */

#define MS_SET 0
#define MS_AND 1
#define MS_OR 2

/* Codes for Alteringing snapshots with MS_AlterSnapshot */

#define ASS_REPLACE_ALL 0
#define ASS_REPLACE_ATTRIBUTES 1
#define ASS_OR_ATTRIBUTES 2
#define ASS_AND_ATTRIBUTES 3
#define ASS_REPLACE_ATT_CAPT 4

/* Codes for returning the results of white pages analysis on recipient names */

#define MSWP_GOODUSER 1 /* address parsed to a good local user name */
#define MSWP_GOODMSDIR 2 /* a good (postable) ms directory */
#define MSWP_CREATABLEMSDIR 3 /* a creatable ms directory */
#define MSWP_BADMSDIR 4	   /* an ms directory you can not post on */
#define MSWP_CRAP 5	/* Unrecognizable junk */
#define MSWP_AMBIGUOUS 6 /* Ambiguous name, choices placed in comments */
#define MSWP_VERYAMBIGUOUS 7 /* Ambiguous name, too many choices */
#define MSWP_GOODNETMAIL 8 /* A plausible piece of mail for the network */
#define MSWP_BADNETMAIL 9   /* Unreconized host name */
#define MSWP_TEMPFAIL 10	    /* A temporary failure to figure out the name */
#define MSWP_UNKNOWNNETMAIL 11  /* host name not resolvable */
#define MSWP_PROBABLYGOOD 12	/* Probably a good name, not positive */
#define MSWP_PROBABLYAMBIGUOUS 13	/* ambiguous plus temp failures */
#define MSWP_NOEXTPOSTING 14	/* External posting not enabled for user */
#define MSWP_PERSONALALIAS 15	/* Address is a personal mail alias */
#define MSWP_INVALIDUSER 16 /* Bad user name prefixed a hash mark */
#define MSWP_INVALIDMSGDIR 17 /* Bad suffix part in bb#foobar format */
#define MSWP_DISTLIST 18 /* Distribution list */
#define MSWP_UNKNOWNGOOD 19 /* Good address of unknown kind */
#define MSWP_BADDISTLIST 20 /* Bad spec for dist list */
#define MSWP_PROTDISTLIST 21 /* Dist list file is protected against user */
#define MSWP_BADALIAS 22 /* Alias body with non-printing characters */
#define MSWP_DISTLISTDIR 23 /* Dist list spec is a directory */
#define MSWP_DIRINSERT 24 /* Directory insertion */
#define MSWP_BADDIRINSERT 25 /* Bad spec for dir-insert */
#define MSWP_PROTDIRINSERT 26 /* Permission denied on dir-insert */
#define MSWP_DIRINSERTFILE 27 /* Dir to insert into is really a file */
#define MSWP_MATCHTOOFUZZY 28 /* Very fuzzy match on names */
#define MSWP_FUZZYAMBIGMATCH 29 /* Very fuzzy ambiguous match */
#define	MSWP_FUZZYTOOMANYMATCHES 30 /* Very ambiguous very fuzzy match */
#define MSWP_PROBABLYFUZZYAMBIG 31 /* Probably ambig, fuzzy, details failed */
#define MSWP_AMBIGWITHERRORS 32 /* Ambiguous name with lookups in list elaboration */
#define MSWP_GOODEXTMSDIR 33 /* Good external bboard directory name */
#define MSWP_EXTFORCEFORMAT 34 /* External mail, force send w/formatting */
#define MSWP_EXTFORCESTRIP 35 /* External mail, force send no formatting */
#define MSWP_EXTFORCETRUST 36 /* External mail, force trust delivery */
#define MSWP_EXTFORCEFORMATDIR 37 /* External bboard, force send w/formatting */
#define MSWP_EXTFORCESTRIPDIR 38 /* External bboard, force send no formatting */
#define MSWP_EXTFORCETRUSTDIR 39 /* External bboard, force trust delivery */
#define MSWP_FSMEMBERS 40	/* Delivery to an AFS groupname */
#define MSWP_BADFSMEMBERS 41	/* Not an AFS groupname */
#define MSWP_UNKNOWNFSMEMBERS 42 /* Not sure about (probably bad) AFS groupname */
#define MSWP_MAXAMBIGMATCHES 10 /* Maximum matches to ambiguous name */

/* The following are the codes for the MS_CloneMessage call, defining the
	various ways in which a message may be copied or moved to another
	directory. */

#define MS_CLONE_COPY 1 	/* Just copy the message */
#define MS_CLONE_COPYDEL 2	/* Copy it and mark it as deleted in source dir */
#define MS_CLONE_APPEND 3	/* Append to end of directory */
#define MS_CLONE_APPENDDEL 4	/* Append & copy */
#define MS_CLONE_SYMLINK 5	/* Symbolic link, not yet implemented */
#define MS_CLONE_MOVE 6		/* Fast move, may never be implemented */

/* The following code the protection status returned by the MS_GetDirInfo call */

#define AMS_DIRPROT_READ 1
#define AMS_DIRPROT_MODIFY 2
#define AMS_DIRPROT_MBOX 3
#define AMS_DIRPROT_AWFUL 4
#define AMS_DIRPROT_FULLMAIL 5
#define AMS_DIRPROT_LOCALBB 6
#define AMS_DIRPROT_EXTBB 7
#define AMS_DIRPROT_OFFBB 8

/* Constants for looking things up in MS_GetSearchPathEntry routine */

#define AMS_MAILPATH -1
#define AMS_OFFICIALPATH -2
#define AMS_LOCALPATH -3
#define AMS_EXTERNALPATH -4

/* Default mail directory */

#define AMS_DEFAULTMAILDIR "mail"

#define AMS_EXPLANATIONFILE ".MS_intro.txt"

/* The following line, when included, turns on fd leak plumbing */
#ifndef NOFDPLUMBING
#include <fdplumb.h>
#endif /* NOFDPLUMBING */

#define AMS_VALIDATION_ERR_PREFIX "(Address Validation Error: "

/* Constants for preference-handling routines */

#define AMS_GETPROFILESTRING 0
#define AMS_GETPROFILEINT 1
#define AMS_GETPROFILESWITCH 2
#define AMS_SETPROFILESTRING 3

/* The following is a bit tricky.  There is code that only works with a Vice kernel.
  If we compile on a non-Vice system, we can just ifdef it out with #ifdef AFS_ENV.
  However, we also want to be able to run the Vice binaries on a non-Vice system
  without a kernel panic, so we include a run-time check. */

extern int AMS_ViceIsRunning;  /* Must be initialized by calling CheckAMSConfiguration, in libmail */


#define UNUSEDATTRNAME "***UNUSED***"


/* Flags to the MS_PrintMessage command */

#define AMS_PRINT_OBSOLETE 1 /* Leftover from old GoFast Boolean parameter */
#define AMS_PRINT_FIXED 2
#define AMS_PRINT_ROT13 4


/* The root of message directory trees (formerly in ms.h) */

#define MS_TREEROOT ".MESSAGES" /* matches .MESSAGES* */
