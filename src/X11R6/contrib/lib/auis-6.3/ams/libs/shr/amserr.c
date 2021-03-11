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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/shr/RCS/amserr.c,v 2.15 1992/12/15 21:21:37 rr2b R6tape $";
#endif

/*
	amserr.c -- Decoding the codes for message server errors 

**********************************************************************************
				IMPORTANT NOTE
**********************************************************************************
	All additions to this file MUST be accompanied by a matching
	change to the constant definitions in mserrno.h.

			DO NOT CHANGE ONE WITHOUT THE OTHER

**********************************************************************************
*/

#define NODECLAREMSERRCODE
#include <mserrno.h>
#undef NODECLAREMSERRCODE

long mserrcode = 0;

char *ms_errlist[] = {
    "Problem with AFS", /* EMSVICE */
    "Bogus return from system call", /* EMSBOGUS */
    "Buffer too small", /* EMSBUFSIZE */
    "Action would overwrite", /* EMSWOULDOVERWRITE */
    "Bad message format", /* EMSBADMSGFORMAT */
    "Bad message directory format", /* EMSBADDIRFORMAT */
    "Unknown error", /* EMSUNKNOWN */
    "Invalid new mail distribution specification", /* EMSBADDDIRSPEC */
    "New mail has nowhere to go", /* EMSORPHANMSG */
    "New mail distribution specification line too long", /* EMSBIGDIRSPECLINE */
    "Improperly nested mail distribution specification", /* EMSDIRNEST */
    "Two different message directories have the same path name", /* EMSDIRNAMECOLLISON */
    "A line of your message server user profile is too long", /* EMSBIGPROFLINE */
    "User is unknown", /* EMSNOSUCHUSER */
    "No home directory", /* EMSNOHOME */
    "No such message",  /* EMSNOSUCHMESSAGE */
    "Cannot print message", /* EMSCANNOTPRINT */
    "Risky delivery -- watch console for results", /* EMSRISKYDELIVERY */
    "Directory has no parent", /* EMSNOPARENT */
    "Message destined for too many directories", /* EMSTOOMANYDIRS */
    "Message server returned explanatory text", /* EMSEXPLAINED */
    "Unrecognized output from trymail program", /* EMSTRYMAILERR */
    "Lock file is very new; simultaneous actions in progress?", /* EMSYOUNGMAIL */
    "Message directory should already be open", /* EMSDIRNOTOPEN */
    "Message directory is already open", /* EMSDIRALREADYOPEN */
    "Cannot open white pages", /* EMSWPOPENFAIL */
    "Cannot remove quotes from address", /* EMSNOUNQUOTE */
    "White pages setup failed", /* EMSWPSETUP */
    "White pages read failed", /* EMSWPREAD */
    "Failed to close more than one directory", /* EMSMULTIPLECLOSEERRS */
    "White pages answer makes no sense", /* EMSWPLIES */
    "You have lost your AFS Authentication", /* EMSUNAUTH */
    "Feature not yet implemented", /* EMSUNIMPL */
    "Message directory contains a subdirectory", /* EMSDIRHASKIDS */
    "Message directory reconstruction failed", /* EMSRECONFAILED */
    "Error reading or writing file", /* EMSFILEERR */
    "All outgoing mail must have a subject", /* EMSNOSUBJ */
    "Failure in delivery system", /* EMSDELIVERYFAIL */
    "Personal mail alias is too long", /* EMSBIGALIAS */
    "Would copy message onto itself", /* EMSALREADYTHERE */
    "No .AMS.flames file and cannot find default directory", /* EMSNOSPECFILE */
    "Unknown variable", /* EMSNOSUCHVAR */
    "Mail dropoff warning", /* EMSDROPOFFWARN */
    "Message queued locally", /* EMSDROPOFFLOCALQUEUE */
    "Message could not be queued", /* EMSDROPOFFNOQUEUE */
    "Bad dropoff parameters", /* EMSDROPOFFBADPARMS */
    "Temporary dropoff failure", /* EMSDROPOFFTEMPFAIL */
    "Bad message file", /* EMSDROPOFFBADMSGFILE */
    "Unknown operating system error", /* EMSDROPOFFOSERR */
    "Syntax error in local name", /* EMSBADLOCALSYNTAX */
    "White pages lookup failed", /* EMSWPLOOKUPFAIL */
    "Premature end of file", /* EMSPREMATUREEOF */
    "Message has too many headers", /* EMSTOOMANYHEADS */
    "Illegal name for message folder", /* EMSBADDIRNAME */
    "Syntax error", /* EMSSYNTAX */
    "Lock file is too new", /* EMSYOUNGLOCK */
    "You can not unsubscribe to this folder", /* EMSFASCISTSUBSCRIPTION */
    "End of destination list reached", /* EMSENDOFLIST */
    "Message has non-printable characters", /* EMSNONASCIIMAIL */
    "Message has lines too long for some mail systems", /* EMSLONGLINES */
    "Attribute already exists", /* EMSATTREXISTS */
    "Guardian Error", /* EMSGUARDIANERR */
    "Snap authentication failure", /* EMSSNAPAUTH */
    "You have no primary authenticated AFS identity", /* EMSNOVUID */
    "Not a message tree root", /* EMSNOTTREEROOT */
    "Out of memory", /* EMSNOMEM */ /* to avoid direct use of ENOMEM in client */
    "Not a valid printer name", /* EMSBADPRINTER */
    "White pages database is corrupted", /* EMSWPCORRUPTION */
    "SNAP Client Initialization failed", /* EMSSNAPINIT */
    "Too many print requests have been queued", /* EMSTOOMANYPRINTS */
    "A file could not be read in its entirety", /* EMSBADFILESIZE */
    "A Lisp syntax error was encountered",  /* EMSELISYNTAX */
    "A referenced Lisp symbol is unbound",  /* EMSELIUNBOUND */
    "A Lisp call was made to an undefined function",	/* EMSELIUNDEF */
    "A Lisp function defined with bad parameters",  /* EMSELIBADPARAM */
    "A Lisp function was called with bad arguments",	/* EMSELIBADARGS */
    "A referenced Lisp symbol doesn't exist",	/* EMSELINOSYM */
    "Wrong # of arguments to a Lisp function",	/* EMSELIARGLISTSIZE */
    "User's Lisp function signalled an error",  /* EMSELIUSERERROR */
    "Cross-cell configuration prevents all mail submission", /* EMSNODELIVERY */
    "Program version you are running is unsupported", /* EMSUNSUPPORTED */
    "MS_EliEval called with bogus result list", /* EMSEVALBOGUS */
    "'set hold' in .mailrc or Mail.rc prevents consuming mail", /* EMSHOLDSET */
    "Clock values appear totally out-of-range", /* EMSCLOCKBOGUS */
    "A Lisp call encountered a system error",   /* EMSELISYSERROR */
};

int ms_nerr = (sizeof ms_errlist) / (sizeof ms_errlist[0]) ; 

char *ms_errcauselist[] = {
    "unknown location",
    "fopen", /* EIN_FOPEN */
    "fclose", /* EIN_FCLOSE */
    "open", /* EIN_OPEN */
    "close", /* EIN_CLOSE */
    "stat", /* EIN_STAT */
    "read", /* EIN_READ */
    "write", /* EIN_WRITE */
    "fread", /* EIN_FREAD */
    "fwrite", /* EIN_FWRITE */
    "malloc", /* EIN_MALLOC */
    "fstat", /* EIN_FSTAT */
    "internal parameter check", /* EIN_PARAMCHECK */
    "opendir", /* EIN_OPENDIR */
    "size check on message file", /* EIN_SIZECHECK */
    "bcmp", /* EIN_BCMP */
    "mkdir", /* EIN_MKDIR */
    "lseek", /* EIN_LSEEK */
    "parse of new mail distribution specification", /* EIN_DIRSPECPARSE */
    "attempt to lock message directory", /* EIN_DIRECTORYLOCK */
    "improper nesting on else clause", /* EIN_ELSEPARSE */
    "improper nesting on endif clause", /* EIN_ENDIFPARSE */
    "stack overflow on push command", /* EIN_PUSH */
    "attempt to pop an empty stack", /* EIN_POP */
    "command interpreter", /* EIN_COMMANDPARSE */
    "file name resolution", /* EIN_DISAMB */
    "vclose", /* EIN_VCLOSE */
    "vfclose", /* EIN_VFCLOSE */
    "popen", /* EIN_POPEN */
    "pclose", /* EIN_PCLOSE */
    "getpwnam", /* EIN_GETPWNAM */
    "access", /* EIN_ACCESS */
    "path search", /* EIN_PATHSEARCH */
    "getenv", /* EIN_GETENV */
    "dup2", /* EIN_DUP2 */
    "flock", /* EIN_FLOCK */
    "wait", /* EIN_WAIT */
    "fork", /* EIN_FORK */
    "rindex", /* EIN_RINDEX */
    "ferror", /* EIN_FERROR */
    "rename", /* EIN_RENAME */
    "setsockopt", /* EIN_SETSOCKOPT */
    "t2open", /* EIN_T2OPEN */
    "truncate", /* EIN_TRUNCATE */
    "ftruncate", /* EIN_FTRUNCATE */
    "fgets", /* EIN_FGETS */
    "ParseAddressList", /* EIN_PARSEADDR */
    "UnparseAddressList", /* EIN_UNPARSEADDR */
    "HandleAddress", /* EIN_HANDLEADDRESS */
    "CloseMSDir", /* EIN_CLOSEMSDIR */
    "index", /* EIN_INDEX */
    "retry of previous operation", /* EIN_RETRY */
    "unlink", /* EIN_UNLINK */
    "rmdir", /* EIN_RMDIR */
    "directory reconstruction", /* EIN_RECON */
    "utimes", /* EIN_UTIMES */
    "qopen", /* EIN_QOPEN */
    "qclose", /* EIN_QCLOSE */
    "vfork", /* EIN_VFORK */
    "chmod", /* EIN_CHMOD */
    "pioctl", /* EIN_PIOCTL */
    "mspath preference check", /* EIN_MSPATHCHECK */
    "dropoff", /* EIN_DROPOFF */
    "la_kind", /* EIN_LAKIND */
    "unscribe", /* EIN_UNSCRIBE */
    "parsedate", /* EIN_PARSEDATE */
    "setprofilestring", /* EIN_SETPROF */
    "fdopen", /* EIN_FDOPEN */
    "vm_open", /* EIN_VMOPEN */
    "Old UCB-Mail-style locking", /* EIN_UCBMAILLOCK */
    "getvuid", /* EIN_GETVUID */
    "getvpwuid", /* EIN_GETVPWUID */
    "Snap_ClientInit", /* EIN_SNAPCLIENTINIT */
    "Snap_BeginConv", /* EIN_SNAPBEGINCONV */
    "GetCellFromFile", /* EIN_GETCELLFROMFILE */
    "GetCellFromUser", /* EIN_GETCELLFROMUSER */
    "GetCellFromWorkstation", /* EIN_GETCELLFROMWS */
    "system", /* EIN_SYSTEM */
    "Site-specific local name validation", /* EIN_LOCALNAMEDB */
    "Lisp Error",    /* EIN_ELI */
    "MS_EliEval",   /* EIN_MSELIEVAL */
    "readlink", /* EIN_READLINK */
    "reading of flames file(s)"
};

int ms_nerrcause = sizeof ms_errcauselist / sizeof ms_errcauselist[0];

char *ms_errvialist[] = {
    "unknown location", /* EVIA_UNKNOWN */
    "ProcessNewMesssages", /* EVIA_PROCNEWMSGS */
    "GetPartialFile", /* EVIA_GETPARTIALFILE */
    "CreateNewMessageDirectory", /* EVIA_CREATENEWMESSAGEDIRECTORY */
    "CreateNewMSDirectory", /* EVIA_CREATENEWMSDIRECTORY */
    "DestructivelyWriteDirectoryHead", /* EVIA_DESTRUCTIVELYWRITEDIR */
    "ReadOldMSDirectoryHead", /* EVIA_READOLDMSDIR */
    "WritePureFile", /* EVIA_WRITEPUREFILE */
    "ReadRawFile", /* EVIA_READRAWFILE */
    "ChooseDirectories", /* EVIA_CHOOSEDIRECTORIES */
    "BuildReplyField", /* EVIA_BUILDREPLY */
    "BuildWideReplyField", /* EVIA_BUILDWIDEREPLY */
    "ParseMessageFromRawBody", /* EVIA_PARSEMSGFROMRAWBODY */
    "AddToDirCache", 		/* EVIA_ADDTODIRCACHE */
    "ProcessCommand", /* EVIA_PROCESSCOMMAND */
    "SetAssociatedTime", /* EVIA_SETASSOCIATEDTIME */
    "GetAssociatedTime", /* EVIA_GETASSOCIATEDTIME */
    "ReadProfile", /* EVIA_READPROFILE */
    "WriteProfile", /* EVIA_WRITEPROFILE */
    "HeadersSince", /* EVIA_HEADERSSINCE */
    "PrintMessage", /* EVIA_PRINTMESSAGE */
    "ResolveAmbiguousFileName", /* EVIA_DISAMB */
    "GetSearchPathEntry", /* EVIA_GETSEARCHPATHENTRY */
    "NameSubscriptionMapFile", /* EVIA_NAMESUBSCRIPTIONMAP */
    "SubsTreeWalk", /* EVIA_SUBSTREEWALK */
    "WriteSubscription", /* EVIA_WRITESUBS */
    "ReadSubscription", /* EVIA_READSUBS */
    "OpenDebuggingPipescript", /* EVIA_OPENDEBUGGINGPIPESCRIPT */
    "NameReplyFile", /* EVIA_NAMEREPLYFILE */
    "RebuildSubscriptionMap", /* EVIA_REBUILDSUBSCRIPTIONMAP */
    "GetSnapshotFromDir", /* EVIA_GETSNAPSHOTFROMDIR */
    "PurgeDeletedMessages", /* EVIA_PURGEDELETEDMESSAGES */
    "DeleteThrough", /* EVIA_DELETETHROUGH */
    "Epoch", /* EVIA_EPOCH */
    "RejectMessage", /* EVIA_REJECTMESSAGE */
    "GetHeaderContents", /* EVIA_GETHEADERCONTENTS */
    "SubmitMessage", /* EVIA_SUBMITMESSAGE */
    "LockProfile", /* EVIA_LOCKPROFILE */
    "UnlockProfile", /* EVIA_UNLOCKPROFILE */
    "StorePartialFile", /* EVIA_STOREPARTIALFILE */
    "AddParentalMessage", /* EVIA_ADDPARENTAL */
    "CheckOpenMSDirectory", /* EVIA_CHECKOPENMSDIR */
    "Message Server Inititalization", /* EVIA_MSCUIINIT */
    "RewriteAddress", /* EVIA_REWRITEADDRESS */
    "AppendMessageToMSDir", /* EVIA_APPENDMESSAGETOMSDIR */
    "ReadOrFindMSDir", /* EVIA_READORFINDMSDIR */
    "ReconstructDirectory", /* EVIA_RECONSTRUCTDIRECTORY */
    "BuildCaption", /* EVIA_BUILDCAPTIONFIELD */
    "CheckMailboxes", /* EVIA_CHECKMAILBOXES */
    "OpenMSDirectory", /* EVIA_OPENMSDIR */
    "CloseMSDir", /* EVIA_CLOSEMSDIR */
    "GetSnapshotByID", /* EVIA_GETSNAPSHOTBYID */
    "GetSnapshotByNumber", /* EVIA_GETSNAPSHOTBYNUMBER */
    "RewriteSnapshotInDirectory", /* EVIA_REWRITESNAPSHOTINDIR */
    "PlanToCloseDir", /* EVIA_PLANTOCLOSEDIR */
    "CloseDirsThatNeedIt", /* EVIA_CLOSEDIRSTHATNEEDIT */
    "CloneMessage", /* EVIA_CLONEMESSAGE */
    "CopyMessageBody", /* EVIA_COPYMESSAGEBODY */
    "MergeDirectories", /* EVIA_MERGEDIRS */
    "GetDirectoryInfo", /* EVIA_GETDIRINFO */
    "RemoveDirectory", /* EVIA_REMOVEDIR */
    "MS_UnlinkFile", /* EVIA_UNLINKFILE */
    "EditMessage", /* EVIA_EDITMESSAGE */
    "ConvertOldMail", /* EVIA_CONVERTOLD */
    "EnsureInSubscriptonMap", /* EVIA_ENSUREINSUBS */
    "CheckPersonalAlias", /* EVIA_CHECKPERSONALALIAS */
    "RenameDirectory", /* EVIA_RENAMEDIR */
    "InitializeSearchPaths", /* EVIA_INITSEARCHPATHS */
    "PrefetchMessageDirectories", /* EVIA_PREFETCH */
    "PrefetchMessage", /* EVIA_PREFETCHMSG */
    "CheckMissingFolder", /* EVIA_CHECKMISSINGFOLDER */
    "UpdateMasterUpdateFile", /* EVIA_UPDATEUPDATES */
    "FindTreeRoot", /* EVIA_FINDTREEROOT */
    "LockMasterFile", /* EVIA_LOCKMASTERFILE */
    "UnlockMasterFile", /* EVIA_UNLOCKMASTERFILE */
    "WriteChangedSubsMap", /* EVIA_WRITECHANGEDSUBS */
    "RebuildMasterUpdateFiles", /* EVIA_REBUILDMASTERUPS */
    "SetSubscriptionEntry", /* EVIA_SETSUBSENTRY */
    "ValidateAndReplaceChunk", /* EVIA_VALCHUNK */
    "WriteUnscribedBodyFile", /* EVIA_WRITEUNSCRIBED */
    "DoIHaveMail", /* EVIA_DOIHAVEMAIL */
    "ParseDate", /* EVIA_PARSEDATE */
    "WriteAllMatches", /* EVIA_WRITEALLMATCHES */
    "HandlePreference", /* EVIA_HANDLEPREFERENCE */
    "CheckAuthentication", /* EVIA_CHECKAUTH */
    "ConvertOldStyleIncomingMail", /* EVIA_CONVERTINCOMING */
    "ResendMessage", /* EVIA_RESENDMESSAGE */
    "GetDirAttributes", /* EVIA_GETDIRATTRS */
    "AddAttribute", /* EVIA_ADDATTR */
    "DeleteAttribute", /* EVIA_DELATTR */
    "UnformatMessage", /* EVIA_UNFORMATMSG */
    "GuaranteeFullBody", /* EVIA_GUARANTEEFULLBODY */ /* OBSOLETE */
    "GetHeaderSize", /* EVIA_GETHEADERSIZE */
    "EmitBE2Prefix", /* EVIA_EMITBE2PREFIX */
    "Machine_Init", /* EVIA_MACHINEINIT */
    "DealWithMarkInProgress", /* EVIA_MARKINPROGRESS */
    "SetChainField", /* EVIA_SETCHAIN */
    "Flames_Handlenew",	/* EVIA_FLAMES_HANDLENEW */
    "cuisnap",	/*EVIA_CUISNAP*/
    "mssnap",	/*EVIA_MSSNAP*/
    "mseli",	/*EVIA_MSELI*/
    "mseli_lisp_code", /*EVIA_MSELILISP*/
    "MatchFolderName", /*EVIA_MATCHFOLDERNAME */
    "Scavenge", /* EVIA_SCAVENGE */
    "BuildDateField", /* EVIA_BUILDDATE */
};

int ms_nerrvia = sizeof ms_errvialist / sizeof ms_errvialist[0];

char *rpc_errlist[] = {
    "Error 0",					/* not used */
    "Error 1",					/* Never returned, used internally */
    "Unknown opcode",				/* RPC_BAD_OPCODE */
    "Client stub call parameters too large",	/* RPC_BAD_CALL_LENGTH_1 */
    "Client RPC buffer too large at server",	/* RPC_BAD_CALL_LENGTH_2 */
    "Return args to stub too large",		/* RPC_BAD_CALL_LENGTH_3 */
    "Return packet too large for client",	/* RPC_BAD_CALL_LENGTH_4 */
    "Reply never received -- timeout",		/* RPC_TIMEOUT */
    "Message server could not initialize properly", /* RPC_SERVERDOA */
};

int rpc_nerr = (sizeof rpc_errlist / sizeof rpc_errlist[0]);

char *snap_errlist[] = {
    "The maximum number of SNAP conversations has been exceeded.",
    "SNAP is unable to obtain more memory.",
    "You are not logged in OR temporary failure locating host machine.",
    "Residual error code -- no more retries -- should not happen.",
    "SNAP send failed; is the guardian running?",
    "Invalid op code to SNAP call.",
    "Invalid conversation id in SNAP call.",
    "SNAP buffer limit exceeded.",
    "SNAP was unable to create a socket.",
    "SNAP not initialized.",
    "Authentication failed -- password is probably wrong.",
    "Unsolicited SNAP reply.",
    "Server refused SNAP connection from client.",
    "Server did not respond within specified interval.",
    "Internal SNAP protocol error.",
    "Client and server have incompatible SNAP versions.",
    "SNAP select statement failed or was interrupted.",
    "SNAP operation failed; trying again might work.",
    "Server is dead.",
};

int snap_nerr = (sizeof snap_errlist / sizeof snap_errlist[0]);
