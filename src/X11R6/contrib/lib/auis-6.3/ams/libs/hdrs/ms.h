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
	ms.h -- Include file for message server routines.

	NOTE:  This is only for the message server, not the CUI.
	       CUI clients should include cui.h instead.
*/

#include <ams.h>  /* Definitions common to server & client */
#include <sys/param.h> /* Need this for MAXPATHLEN */

/* Version numbers for message server.  These will probably only be
	used when the message server is asked by the client to identify
	itself. */

#include <msvers.h>

typedef short Boolean;
#define TRUE 1
#define FALSE 0

extern int MSDebugging;
#define debug(n, x) ((MSDebugging & (n)) ? dbgprintf x : 0)
/* Flags for debugging tell which procedures to debug.  A current list:
	(They should be ORed together as appropriate)
	1	Entry to each procedure
	2	Procedures in rawdb.c for raw database access 
	4	Debugging of "exported" routines (direct CUI calls)
	8	Choose Directories routines
	16	Miscellaneous other routines
	32	Show RPC packets received
	64	AMS_RETURN_ERRCODE macro
	128	Debug open and close msdirectory routines
	256	Debug profile-related routines (well, some of them)
	512	debug printing-related routines
	1024	trace all open and close calls (same value as in cui.h)
	2048	trace stuff related to the master update file
             4096	trace FLAMES calls, particularly error reporting
*/

/* The following is the major internal data structure used forams.h
 messages.
    A few notes:  The space for the snapshot is separately allocated,
    generally as part of a directory but not always, and must be
    separately freed when appropriate.  Each of the strings pointed to should also 
    be separately freed.  The "ParsedStuff" will also include the body,
    so that the Body field need not be separately freed.  The message server
    includes a FreeMessage procedure that should be used when a message
    is being freed.
*/


struct MS_Message {
    char *Snapshot; 
    char *ReplyTo; 		/* DITTO */
    char *WideReply;		/* DITTO */
    struct ParsedMsg *ParsedStuff;	/* DITTO */
    int AuthUid;		/* UID of authenticated sender */
    char *AuthCell;		/* Cell in which the authentication took place */
    char *AuthName;		/* Authenticated user name (to save wp lookups) */
    long RawFileDate;     
    int WeFiddled;	/* Non zero if RawMessage has been fooled with and
				thus the file needs to be rewritten */
    char *RawBits;	/* The raw RFC822 header, not the body */
    int FullSize;	/* Size that RawBits would be if all read in */
    int HeadSize;	/* Size that has been read in so far. */
    int OpenFD;
    int BodyOffsetInFD; /* Where the body really starts in the file */
};

/* Magic tag that is included at the beginning of every directory -- can be
	used to check if a file is an AMS directory, although it could
	conceivably change with changed version numbers */

/* At present, this string should not contain nulls because I do use
	normal string functions on it. */

#define AMS_DIRECTORY_PREFIX_STRING "\003\004\033\277BINARY FILE -- DO NOT EDIT!!!  \n`The mail transport mechanism is trivial.' --Jim Morris\n\n\003\033\277"

#define ATTNAMESLEN (AMS_NUM_UATTRS * AMS_ATTRNAMEMAX)
#define AMS_DIRHEADSIZE (ATTNAMESLEN + (1024 - ATTNAMESLEN) + AMS_SNAPSHOTSIZE)
/* The snapshot is scattered padding */

/* Structure for array of binary values AMS_CHAIN, AMS_MIDHASH, AMS_REPLYHASH. */
struct MS_IDs {
    long int Chn;
    unsigned long int midH, repH;
};
#define KRHashPrime (2147483629)
/* 2^30 - 2: a number that is greater than KRHashPrime but still fits into 31 bits. */
#define noKRHash (2147483646)

/* The following is the major internal data structure for a message
    directory.  Note that the contents of the directory itself are all
    fixed width, and are designed to be as portable as possible between
    different compilers.  This is done by not using structures at
    all, but instead reading the contents of the file into a single
    enormous character string and using macros to pull out the pieces.
*/

#define MS_DIRNAME ".MS_MsgDir"

struct MS_Directory {
    char *UNIXDir;		/* The UNIX directory in which the actual
				    message directory MsgDir is stored */
    char **AttrNames;		/* The names of the user-defined attributes */
    int AttrCount;		/* The number of such beasts */
    int MessageCount;		/* Number of msgs in directory */
    int fd;			/* Open file descriptor, or -1 */
    long CurPos;		/* Current position in file, or -1 */
    int DBMajorVersion;		/* To flag need for conversion */
    int MaxChainVal;		/* Largest message chain number in use */
    long FileDateWhenRead;	/* To flag outdated version */
    int CheckedWritable:1;	/* ON if we have checked write access */
    int Writable:1;		/* ON if user has write access to dir */
    int BadScavenge:1;	/* ON if the last scavenge didn't work */
    short OpenMode;		/* Symbolic value (below) for fd status */
    short MaxOpenMode;		/* Original fd status */
    char LastMsgDate[AMS_DATESIZE];  /* Value of date field of last message as
					of time FileDateWhenRead */
    int	LastIDHit;		/* Last hit in getsnapshotbyid routine */
    int NumOpens;		/* Number of opens minus number of closes */
    int	LastOpenTimestamp;	/* Timestamp of last open, to avoid checkpoint oddities */
    struct MS_IDs *IDs;	/* Pointer to an array of MS_IDs copying some relevant values */
    int	NumIDs;		/* A count of the number of MS_IDs allocated */
};

/* When you access a directory, you specify one of the following modes */

#define MD_OK 0 /* Just check that it exists and read header*/
#define MD_READ 1 /* Open current version for reading */
#define MD_WRITE 2 /* Open for writing and lock it */
#define MD_APPEND 3 /* Open for writing and lock it AND master update file */
#define MD_CREATE 4 /* Open for writing and lock it, creating or zeroing it */

/* The following defines an internal data structure that represents a 
    parsed message's headers. */

struct ParsedMsg {
    int HeadsAllocated;
    char **HeadName;	/* ALWAYS folded into lower case */
    char **HeadBody;	/* NOT null terminated */
    int *HeadBodyLen;
    Boolean *IsDuplicated;	/* Set if more than one such header */
};

/* For symbolic definitions for above, see hdrparse.h */

#define MAXDIRINSERTIONS 8  /* Max number of directories for 1 new msg */
		/* NOTE NOTE NOTE MAXDIRINSERTION of approx > 32 will trigger
			a horrible compiler bug on the RT PC.  See nsb. */
		/* Later note:  MAXDIRINSERTIONS much bigger than 8 will
			probably run us out of file descriptors.  Oh, when
			will we get 4.3?  */

#define MAXSTACKSTRING (MAXPATHLEN+50)	/* The largest thing that can be put in the stack,
					hence also the largest line in a dirspec */

#define DIRHASHSIZE 128		/* Should remain a power of 2 */


#define AMS_SUBSPROFFILE "~/.AMS.prof"

/* The following is the structure used for a subscription/profile entry */

struct SubscriptionProfile {
    char *sname; /* short name */
    char *key; /* long name */
    int status; /* subscription status code */
    int pathelt; /* -1 if not on path */
    int priority; /* For subscription ordering */
    char time64[AMS_DATESIZE]; /* From SetAssociatedTime */
    long filedate; /* Ditto */
    int NeedsFreed; /* If zero, sname & key are part of one big malloc */
    int HasChanged; /* for use in NameChangedMap */
};

/* The following constant is the maximum number of subdirectories a message
	directory can have.  It is used in the subscription map building
	routine SubsTreeWalk and the purging routine MS_Epoch.  Since these
	are relatively rarely used routines, directories with larger branching
	factors would break in relatively subtle ways (subscription editing
	and purging would be the only things to break), so I've made this
	constant extremely big.
*/

/* The following relate to the template used for building the one-line
	caption field.  This will grow eventually. */

#define DATETYPE_CURRENT 0
#define DATETYPE_FROMFILE 1
#define DATETYPE_FROMHEADER 2

#define BASICTEMPLATE_NORMAL 0
#define BASICTEMPLATE_NOFROM 1

struct MS_CaptionTemplate {
    int datetype; /* One of the DATETYPE_ constants defined above */
    int basictype; /* One of the BASICTEMPLATE_ constants defined above */
};

#define DEFAULTFLAMESFILENAME ".AMS.flames"
#define DEFAULTSPECFILENAME ".MS.spec"

#define PROCESSNEW_MBOX 0
#define PROCESSNEW_FASTRECON 1
#define PROCESSNEW_SLOWRECON 2

#define	MAXPATHELTS 25
struct SearchPathElement {
    char *Path;
    char *Cell;
    int HasValidated:1; /* Have we checked the validity of this path elt? */
    int HasMailbox:1; /* Should we bother checking this one's mailbox? */
    int HaveReadSubs:1; /* Have we read .SubscriptionMap into disambig cache */
    char *label; /* a symbolic name for the path element */
};
extern struct SearchPathElement SearchPathElements[];
extern int MS_NumDirsInSearchPath;

#define MS_MASTERDIR ".MS.Master"
#define MS_MASTERHINT "HINT_"
#define MS_MASTERUPDATE ".MS.Master/Update"
#define MS_MASTERUPDATELOCK ".MS.Master/Update.LOCK"
#define MS_UPDATE "Update"
#define MS_UPDATELOCK "Update.LOCK"

/* The following constants are used for putting special formatting into messages
    being prepared for forwarding or printing */

#define PR_SEPARATORLINE 0
#define PR_STARTSHRINK 1
#define PR_ENDSHRINK 2
#define PR_STARTSUPERSHRINK 3
#define PR_ENDSUPERSHRINK 4
#define PR_BODYSEPARATOR 5
#define PR_STARTBOLD 6
#define PR_ENDBOLD 7
#define PR_STARTBIGGER 8
#define PR_ENDBIGGER 9
#define PR_STARTTYPING 10
#define PR_ENDTYPING 11

extern char MAILSEARCHPATHTEMPLATE[];

struct FileTime {
    char *Name;
    int Time;
};
