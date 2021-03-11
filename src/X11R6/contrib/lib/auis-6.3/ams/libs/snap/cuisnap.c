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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/snap/RCS/cuisnap.c,v 2.25 1994/03/29 03:43:37 rr2b Exp $";
#endif

/* 
		cuisnap.c -- CUI interface to SNAP rpc.

		This file contains the stubs of the routines that are implemented
	in the message server.	These stubs are called directly from the CUI &
	are responsible for packing arguments, making a remote-procedure call &
	unpacking any return arguments.
 */

#define PROCNAME static char *

#include <andrewos.h> /* sys/types.h */
#include <andyenv.h>
#include <stdio.h>
#include <util.h>
#include <snap.h>
#include <gasp.h>
#include <snapams.h>
#include <cui.h>
#include <errprntf.h>
#include <cuimach.h>
#include <mail.h>

int CUI_SnapIsRunning = 1;
long CUI_LastCallFinished = 0;

typedef char	bool;

char   *SnapVersionString=NULL;

extern char *snap_errlist[], *copy();

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern int  SNAP_debugmask;

extern long time();

long MS_UpdateState();

static int  MScid;		/* Must be set externally to SNAP cid of
				   remote/local MS */

/* Call/reply packets that may be used by everyone */
static	CallPacket * request;
static	ReturnPacket * reply;
static int  BufferSize;

static int  min (a, b)
int	a,
	b;
{
    return(a < b ? a : b);
}

static	bool ReplyLengthError (r)
	ReturnPacket * r;
{
    SNAP_integer len;

    (void) getint(ReturnPacketLength(r), &len);
    return((int) len > BufferSize);
}

static	void ReportReturnFailure (name, packet, code)
char   *name;
ReturnPacket * packet;
int	code;
{
    register int    i;

    fprintf(stderr, "[CUI]: Return failure (%d): 0x", code);
    for (i = 0; i < RETURN_PACKET_HEADER_LENGTH; i++)
	fprintf(stderr, "%02x",(unsigned char) packet[i]);
    fputc('\n', stderr);
}

static int makeUser(user, type, where, len)
char *user, *where; int type, len;
{
#ifdef AFS_ENV
	struct CellAuth *ca;

	if (AMS_ViceIsRunning && type == GASP_PWD_MULTI_TOKENS) {
		ca = NULL;
		FindAMSHomeCell(&ca);
		if (ca != NULL) {
			if ((strlen(user) + strlen(ca->CellName) + 2) >= len) return 1;
			strcpy(where, user);
			strcat(where, "/");
			strcat(where, ca->CellName);
			return 0;
		}
	}
#endif /* AFS_ENV */
	if (user && strlen(user) >= len) return 1;
	if (user != NIL)
	    strncpy(where, user, len);
	else
	    where[0] = '\0';
	return 0;
}

/* Saved connection parameters for reconnecting */
char   *ReconHost=NULL,
       *ReconUser=NULL,
       *ReconService=NULL;
int ReconType=0;

long	MS_CUI_Init (host, user, passwd, len, type, buffersize)
char   *host,
       *user,
       *passwd;
int	len, type, buffersize;
{
    int     result, AuthRetries = 0,
	    versions[4];
    char    ErrorText[500];
    SNAP_CPARMS parms;
    static char MSWithVersion[100];

    SnapVersionString = malloc(25);
    SNAP_ClientVersion(-1, versions);
    sprintf(SnapVersionString, "%d.%d", versions[0], versions[1]);

    InitializeLogging();

 /* Try to allocate SNAP buffers */
    BufferSize = buffersize;
    request = (CallPacket *) malloc(buffersize);
    reply = (ReturnPacket *) malloc(buffersize);
    if (request == NIL || reply == NIL) {
	AMS_RETURN_ERRCODE(EMSNOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
    }

 /* Try to save reconnection parameters */
    ReconHost = copy(host);
    ErrorText[0] = '\0';
    makeUser(user, type, ErrorText, sizeof(ErrorText));
    ReconUser = copy(ErrorText);
/*    ReconService = "MessageServer"; New protocol 15 Nov 88 CFE */
    sprintf(MSWithVersion, "MS.%d.%d", AMS_MAJOR_VERSION, AMS_MINOR_VERSION);
    ReconService = MSWithVersion;
    ReconType = type;

 /* Do SNAP initialization */
    result = SNAP_ClientInit();
    if (result != SNAP_SUCCESS) {
	sprintf(ErrorText, "SNAP_ClientInit failed: %d", result);
	ReportError(ErrorText, ERR_CRITICAL, FALSE);
	AMS_RETURN_ERRCODE(EMSSNAPINIT, EIN_SNAPCLIENTINIT, EVIA_MSCUIINIT);
    }

 /* Start up conversation */
    parms.maxtime = AMS_SNAP_TIMEOUT;
    parms.timeout = 15;
    parms.encryptlevel = SNAP_ENCRYPT;
authretry:
    MScid = SNAP_BeginConv("snap.guardian", host, ReconService, ReconUser, passwd, len, type, &parms);
    if (MScid < 0) {

	if (MScid == SNAP_GUARDIAN_ERROR) {
	    if (!parms.guardian_rc) {
		sprintf(ErrorText, "Guardian Error.  Return code %d.", parms.guardian_rc);
		ReportError(ErrorText, ERR_CRITICAL, FALSE);
		AMS_RETURN_ERRCODE(EMSGUARDIANERR, EIN_SNAPBEGINCONV, EVIA_MSCUIINIT);
	    }
	}
	if (MScid == SNAP_NOAUTHENTICATE) {
	    if (!GetBooleanFromUser("Authentication failed -- do you want to try a new password", TRUE)) {
		ReportError("Message server authentication probably failed.", ERR_CRITICAL, FALSE);
		AMS_RETURN_ERRCODE(EMSSNAPAUTH, EIN_SNAPBEGINCONV, EVIA_MSCUIINIT);
	    }
	    if (++AuthRetries < 2) {
		*passwd = '\0';
		GetNewPassword(&passwd, 0, user, host);
		len = strlen(passwd) + 1;
		type = GASP_PWD_STRING;
		goto authretry;
	    }
	}
	MScid = (MScid * -1) - 1002;
	AMS_RETURN_SNAP_ERRCODE(MScid);
    }
    parms.timeout = 1;
    SNAP_SetConvParms(MScid, &parms);
    return 0;
}

CUI_EndConversation() {
    MS_UpdateState();
    SNAP_EndConv(MScid, NIL, 0, NULL);
}

int	ReconnectMS (service)
char *service;
{
    char *MyPasswd;
    int MyPasswdLen;
    register int    rc;
    char    ErrorText[125];
    SNAP_CPARMS parms;

 /* 1st, close down old conversation */
    rc = SNAP_EndConv(MScid, NIL, 0, NULL);

    if (rc != SNAP_SUCCESS)
	debug(4,("Could not terminate old connection, trying to reconnect anyway"));

 /* Now, start up a new one */
    if (service) ReconService = copy(service);
    parms.maxtime = AMS_SNAP_TIMEOUT;
    parms.timeout = 5;
    parms.encryptlevel = SNAP_ENCRYPT;
    if (Machine_Init(&ReconHost, &ReconUser, &MyPasswd, &MyPasswdLen, &ReconType, 1)) {
	ReportError("Machine re-initialization problem", ERR_WARNING, TRUE);
	return(-1);
    }
    makeUser(ReconUser, ReconType, ErrorText, sizeof(ErrorText));
    MScid = SNAP_BeginConv("snap.guardian", ReconHost, ReconService, ErrorText, MyPasswd, MyPasswdLen, ReconType, &parms);
    if (MScid < 0) {
	MScid = (MScid * -1) - 1002;
	sprintf(ErrorText, "[CUI]: Reconnect failed: %s", snap_errlist[MScid]);
	ReportError(ErrorText, ERR_CRITICAL, FALSE);
	return MScid;
    }
    parms.timeout = 1;
    SNAP_SetConvParms(MScid, &parms);
    return SNAP_SUCCESS;
}

static int  CheckReply (proc, reply, result)
char   *proc;
ReturnPacket * reply;
int	result;
{
    SNAP_integer code;

    if (result < 0)
	return RPC_TIMEOUT;
    if (ReplyLengthError(reply)) {
	ReportReturnFailure(proc, reply, RPC_BAD_CALL_LENGTH_4);
	return RPC_BAD_CALL_LENGTH_4;
    }
    (void) getint(ReturnPacketError(reply), &code);
    if ((int) code != RPC_OK) {
	ReportReturnFailure(proc, reply,(int) code);
	return(int) code;
    }
    return RPC_OK;
}

int  CUI_RPCInProgress = 0;


DoRPC (name, request, length, reply, BufferSize, ConParm)
char   *name;
CallPacket * request;
int	length;
ReturnPacket * reply;
int	BufferSize;
SNAP_CPARMS * ConParm;
{
    register int    KeepTrying,
		    Retries = 0,
		    result;
    int     Restarts = 0;

    if (CUIDebugging & 16) {	/* Debugging SHOULD go to stdout -- nsb
				   5/16/86 */
	register int	i;

	fputs("[cui]: DoRPC(0x", stdout);
	for (i = 0; i < length; i++)
	    fprintf(stdout, "%02x",(unsigned char) request[i]);
	fputs(")\n", stdout);
    }
    CUI_LastCallFinished = 0;
    do {
	KeepTrying = CUI_RPC_BUGOUT;
	LogStart();
	result = SNAP_SendWithReply(MScid, request, length, reply, BufferSize, ConParm);
	LogEnd(name);
    /* See if it succeeded */
	result = CheckReply(name, reply, result);
	if (result == RPC_TIMEOUT) {
	    KeepTrying = ConsiderRetrying(name, Retries++, &Restarts);
	}
    } while (KeepTrying != CUI_RPC_BUGOUT);
    CUI_LastCallFinished = time(NULL);
    return(result);
}

long	MS_CreateNewMessageDirectory (DirName, Overwrite, BodyDirName)
char   *DirName;
int	Overwrite;
char   *BodyDirName;
{
    PROCNAME name = "MS_CreateNewMessageDirectory";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_CREATE_NEW_MESSAG);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putint(ptr,(SNAP_integer) Overwrite);
    ptr = putstr(ptr, BodyDirName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Extract the return value & yield it */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_AppendFileToFolder(FileName, FolderName)
char   *FileName, *FolderName;
{
    PROCNAME name = "MS_AppendFileToFolder";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_APPENDFILEFOLDER);
    ptr = putstr(CallPacketArgs(request), FileName);
    ptr = putstr(ptr, FolderName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Extract the return value & yield it */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ProcessNewMessages (SourceDir, NumGood, NumBad, NumLocks, ParseSpecFile, resultcode, FirstError, NumInProgress, Buf, buflim)
char   *SourceDir,
       *ParseSpecFile,
       *Buf;    /* This is a buffer to hold ELI error text */
int    *NumGood,
       *NumBad,
       *NumLocks,
       *resultcode,
       *NumInProgress,
       buflim;
long   *FirstError;
{
    PROCNAME name = "MS_ProcessNewMessages";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;
    char *dummy;

    cp.maxtime = 1800;		 /* half hour */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_PROCESS_NEW_MESSA);
    ptr = putstr(CallPacketArgs(request), SourceDir);
    ptr = putstr(ptr, ParseSpecFile);
    ptr = putint(ptr, (SNAP_integer) buflim);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    ptr = getint(ptr, &temp);
    *NumLocks = (int) temp;
    ptr = getint(ptr, &temp);
    *resultcode = (int) temp;
    ptr = getint(ptr, &temp);
    *FirstError = (long) temp;
    ptr = getint(ptr, &temp);
    *NumInProgress = (int) temp;
    (void) getstr(ptr, &dummy);
    strncpy(Buf, dummy, buflim);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_HeadersSince (FullDirName, datefield,
	    ReturnBuf, MaxReturn,
	    startbyte,
	    numbytes, bytesleft)
char   *FullDirName,
       *datefield,
       *ReturnBuf;
int	MaxReturn;
long	startbyte,
       *numbytes,
       *bytesleft;		/* ***	Added 8/19/86  for PC	*** */
{
    PROCNAME name = "MS_HeadersSince";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 120; /* Let ms have a little extra time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_HEADERS_SINCE);
    ptr = putstr(CallPacketArgs(request), FullDirName);
    ptr = putstr(ptr, datefield);
    ptr = putint(ptr,(SNAP_integer) MaxReturn);
    ptr = putint(ptr,(SNAP_integer) startbyte);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    (void) getint(ReturnPacketArgs(reply), &temp);
    bcopy(dummy, ReturnBuf, min(MaxReturn,(int) temp));
    ptr = getint(ptr, &temp);
    *numbytes = temp;
    ptr = getint(ptr, &temp);
    *bytesleft = temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_PrintMessage (DirName, id, flags, printer)
char   *DirName,
       *id, *printer;
int	flags;
{
    PROCNAME name = "MS_PrintMessage";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_PRINT_MESSAGE);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, id);
    ptr = putint(ptr,(SNAP_integer) flags);
    ptr = putstr(ptr, printer);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_PrefetchMessage (DirName, id, GetNext)
char   *DirName,
       *id;
int	GetNext;
{
    register char  *ptr;
    int length;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_PREFETCH_MSG);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, id);
    ptr = putint(ptr,(SNAP_integer) GetNext);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

#ifdef CHECKING_RETURNS_OLD_CODE
{
    PROCNAME name = "MS_PrefetchMessage";
    int result;
    SNAP_integer rval;
    /* 9/30/88 Why wait for returns from prefetch? -- NSB */
 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
    }
#endif /* CHECKING_RETURNS_OLD_CODE */
    SNAP_SendNoReply(MScid, request, length, NULL);
    CUI_RPCInProgress = 0;
    return(0);
}

long	MS_GetVersion (Buf, lim)
char   *Buf;
int	lim;
{
    PROCNAME name = "MS_GetVersion";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_VERSION);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) lim);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strncpy(Buf, dummy, lim);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_DebugMode (level, snap, malloc)
int	level,
	snap,
	malloc;
{
    PROCNAME name = "MS_DebugMode";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_DEBUG_MODE);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) level);
    ptr = putint(ptr,(SNAP_integer) snap);
    ptr = putint(ptr,(SNAP_integer) malloc);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_NameSubscriptionMapFile (Root, MapFile)
char   *Root,
       *MapFile;
{
    PROCNAME name = "MS_NameSubscriptionMapFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_NAME_SUBSCRIPTION);
    ptr = putstr(CallPacketArgs(request), Root);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(MapFile, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_DisambiguateFile (source, target, MustBeDir)
char   *source,
       *target;
int	MustBeDir;
{
    PROCNAME name = "MS_DisambiguateFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_DISAMBIGUATE_FILE);
    ptr = putstr(CallPacketArgs(request), source);
    ptr = putint(ptr,(SNAP_integer) MustBeDir);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(target, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_CheckMissingFolder (OldName, NewName)
char   *OldName,
       *NewName;
{
    PROCNAME name = "MS_CheckMissingFolder";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_CHECK_MISSING);
    ptr = putstr(CallPacketArgs(request), OldName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(NewName, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetPartialBody (DirName, id, Buf, BufLim, offset, remaining, ct)
char   *DirName,
       *id,
       *Buf;
int	BufLim;
long	offset,
       *remaining;		/* *** Added for PC  8/20/86  *** */
int    *ct;
{
    PROCNAME name = "MS_GetPartialBody";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_PARTIAL_BODY);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, id);
    ptr = putint(ptr,(SNAP_integer) BufLim);
    ptr = putint(ptr,(SNAP_integer) offset);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    (void) getint(ReturnPacketArgs(reply), &temp); /* Length of string */
    bcopy(dummy, Buf, min(BufLim,(int) temp));/* Don't copy too much */
#ifdef MAC
    foreign_to_local(Buf,min(BufLim,((int)temp)));
#endif /* MAC */
    ptr = getint(ptr, &temp);	/* Unpack remaining */
    *remaining = temp;
    ptr = getint(ptr, &temp);	/* Unpack ct */
    *ct = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetConfigurationParameters (MailDomain, len, UseAmsDelivery, UseNameSep, DeliveryType)
int *UseAmsDelivery, *UseNameSep, *DeliveryType, len;
char *MailDomain;
{
    PROCNAME name = "MS_GetConfigurationParameters";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GETCONFIGPARMS);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) len);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(MailDomain, dummy);
    ptr = getint(ptr, &temp);
    *UseAmsDelivery = temp;
    ptr = getint(ptr, &temp);
    *UseNameSep = temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_AndrewDir (AndrewDir, len)
char *AndrewDir;
int len;
{
    PROCNAME name = "MS_AndrewDir";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_ANDREWDIR);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) len);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(AndrewDir, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetPartialFile (FileName, Buf, BufLim, offset, remaining, ct)
char   *FileName,
       *Buf;
int	BufLim;
long	offset,
       *remaining;
int    *ct;
{
    PROCNAME name = "MS_GetPartialFile";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_PARTIAL_FILE);
    ptr = putstr(CallPacketArgs(request), FileName);
    ptr = putint(ptr,(SNAP_integer) BufLim);
    ptr = putint(ptr,(SNAP_integer) offset);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    (void) getint(ReturnPacketArgs(reply), &temp);/* Length of string */
    bcopy(dummy, Buf, min(BufLim,(int) temp));/* Don't copy too much */
#ifdef MAC
    foreign_to_local(Buf,min(BufLim,((int)temp)));
#endif /* MAC */
    ptr = getint(ptr, &temp);	/* Unpack remaining */
    *remaining = temp;
    ptr = getint(ptr, &temp);	/* Unpack ct */
    *ct = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_SetAssociatedTime (FullName, newvalue)
char   *FullName,
       *newvalue;
{
    PROCNAME name = "MS_SetAssociatedTime";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_SET_ASSOCIATED_TI);
    ptr = putstr(CallPacketArgs(request), FullName);
    ptr = putstr(ptr, newvalue);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetAssociatedTime (FullName, Answer, lim)
char   *FullName,
       *Answer;
int	lim;
{
    PROCNAME name = "MS_GetAssociatedTime";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_ASSOCIATED_TI);
    ptr = putstr(CallPacketArgs(request), FullName);
    ptr = putint(ptr,(SNAP_integer) lim);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strncpy(Answer, dummy, lim);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetSearchPathEntry (which, buf, lim)
int	which;
char   *buf;
int	lim;
{
    PROCNAME name = "MS_GetSearchPathEntry";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_SEARCH_PATH_E);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) which);
    ptr = putint(ptr,(SNAP_integer) lim);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strncpy(buf, dummy, lim);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_SetSubscriptionEntry (FullName, NickName, status)
char   *FullName,
       *NickName;
int	status;
{
    PROCNAME name = "MS_SetSubscriptionEntry";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_SET_SUBSCRIPTION_);
    ptr = putstr(CallPacketArgs(request), FullName);
    ptr = putstr(ptr, NickName);
    ptr = putint(ptr,(SNAP_integer) status);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetSubscriptionEntry (FullName, NickName, status)
char   *FullName,
       *NickName;
int    *status;
{
    PROCNAME name = "MS_GetSubscriptionEntry";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_SUBSCRIPTION_);
    ptr = putstr(CallPacketArgs(request), FullName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(NickName, dummy);
    (void) getint(ptr, &temp);
    *status = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_DomainHandlesFormatting (DomName, rslt)
char   *DomName;
int    *rslt;
{
    PROCNAME name = "MS_DomainHandlesFormatting";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request), (SNAP_integer) OP_MS_DOMAIN_FORMAT);
    ptr = putstr(CallPacketArgs(request), DomName);
    length = ptr - request;
    (void) putint(CallPacketLength(request), (SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    (void) getint(ReturnPacketArgs(reply), &temp);
    *rslt = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetNextSubsEntry (FullName, NickName, status)
char   *FullName,
       *NickName;
int    *status;
{
    PROCNAME name = "MS_GetSubscriptionEntry";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_NEXT_SUBSENT);
    ptr = putstr(CallPacketArgs(request), FullName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(FullName, dummy);
    ptr = getstr(ptr, &dummy);
    strcpy(NickName, dummy);
    (void) getint(ptr, &temp);
    *status = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_Die () {
    PROCNAME name = "MS_Die";
/*    register char *ptr; */
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_DIE);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

    result = SNAP_SendWithReply(MScid, request, length, reply, BufferSize, NULL);

    result = CheckReply(name, reply, result);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_OpenDebuggingPipescript (DoIt)
int	DoIt;
{
    PROCNAME name = "MS_OpenDebuggingPipescript";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_OPEN_DEBUGGING_PI);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) DoIt);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}


long	MS_RebuildSubscriptionMaps () {
    PROCNAME name = "MS_RebuildSubscriptionMaps";
/*    register char *ptr; */
    int     result,
	    length;
    SNAP_integer rval;
    SNAP_CPARMS cp;

    cp.maxtime = 300;		/* five minutes */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REBUILD_SUBSCRIPT);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_RebuildOneSubscriptionMap (PathElt) 
char *PathElt;
{
    PROCNAME name = "MS_RebuildOneSubscriptionMap";
/*    register char *ptr; */
    int     result,
	    length;
    SNAP_integer rval;
    SNAP_CPARMS cp;
    register char *ptr;

    cp.maxtime = 300;		/* five minutes */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    ptr = putstr(CallPacketArgs(request), PathElt);
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REBUILD_ONE);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_NameReplyFile (DirName, id, code, FileName)
char   *DirName,
       *id,
       *FileName;
int	code;
{
    PROCNAME name = "MS_NameReplyFile";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_NAME_REPLY_FILE);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, id);
    ptr = putint(ptr,(SNAP_integer) code);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(FileName, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GenTempFileName (FileName)
char   *FileName;
{
    PROCNAME name = "MS_GenTempFileName";
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GEN_TEMP_NAME);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(FileName, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_WriteAllMatchesToFile (ambigname, FileName)
char   *ambigname, /* IN */
       *FileName; /* OUT */
{
    PROCNAME name = "MS_WriteAllMatchesToFile";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_WRITE_ALL_MATCHES);
    ptr = putstr(CallPacketArgs(request), ambigname);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(FileName, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_Epoch (dirname, date64)
char   *dirname,
       *date64;
{
    PROCNAME name = "MS_Epoch";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_EPOCH);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putstr(ptr, date64);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_AlterSnapshot (dirname, id, NewSnapshot, Code)
char   *dirname,
       *id,
       *NewSnapshot;
int	Code;
{
    PROCNAME name = "MS_AlterSnapshot";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_ALTER_SNAPSHOT);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putstr(ptr, id);
    ptr = putbytes(ptr, NewSnapshot, AMS_SNAPSHOTSIZE);
    ptr = putint(ptr,(SNAP_integer) Code);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_PurgeDeletedMessages (dirname)
char   *dirname;
{
    PROCNAME name = "MS_PurgeDeletedMessages";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_PURGE_DELETED_MES);
    ptr = putstr(CallPacketArgs(request), dirname);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetSnapshot (dirname, id, SnapshotBuf)
char   *dirname,
       *id,
       *SnapshotBuf;
{
    PROCNAME name = "MS_GetSnapshot";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_SNAPSHOT);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putstr(ptr, id);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    bcopy(dummy, SnapshotBuf, AMS_SNAPSHOTSIZE);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetNthSnapshot (dirname, n, SnapshotBuf)
char   *dirname,
       *SnapshotBuf;
long n;
{
    PROCNAME name = "MS_GetNthSnapshot";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_NTH_SNAPSHOT);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putint(ptr, (SNAP_integer) n);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    bcopy(dummy, SnapshotBuf, AMS_SNAPSHOTSIZE);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetHeaderContents (dirname, id, HeaderName, HeaderTypeNumber, HeaderBuf, lim)
char   *dirname,
       *id,
       *HeaderName,
       *HeaderBuf;
int	HeaderTypeNumber,
	lim;
{
    PROCNAME name = "MS_GetHeaderContents";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_HEADER_CONTEN);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putstr(ptr, id);
    ptr = putstr(ptr, HeaderName);
    ptr = putint(ptr,(SNAP_integer) HeaderTypeNumber);
    ptr = putint(ptr,(SNAP_integer) lim);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(HeaderBuf, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ValidateAndReplaceChunk(FileName, inaddr, outaddr, outaddrsize, which, outcode)
char *FileName, *inaddr; /* IN */
char *outaddr; /* OUT */
int outaddrsize, which; /* IN */
int *outcode; /* OUT */
{
    PROCNAME name = "MS_ValidateAndReplaceChunk";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_VALIDATE_CHUNK);
    ptr = putstr(CallPacketArgs(request), FileName);
    ptr = putstr(ptr, inaddr);
    ptr = putint(ptr,(SNAP_integer) outaddrsize);
    ptr = putint(ptr,(SNAP_integer) which);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    bcopy(dummy, outaddr, outaddrsize);
    strcpy(outaddr, dummy);
    ptr = getint(ptr, &temp);
    *outcode = (int) temp;
    
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_HandlePreference(prog, pref, InVal, OutVal, OutLim, opcode, resulti, defaulti)
char *prog, *pref, *InVal; /* Passed IN */
char *OutVal; /* Passed OUT */
int OutLim, opcode, defaulti; /* Passed IN */
int *resulti; /* Passed OUT */
{
    PROCNAME name = "MS_HandlePreference";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_HANDLE_PREFERENCE);
    ptr = putstr(CallPacketArgs(request), prog);
    ptr = putstr(ptr, pref);
    ptr = putstr(ptr, InVal);
    ptr = putint(ptr,(SNAP_integer) OutLim);
    ptr = putint(ptr,(SNAP_integer) opcode);
    ptr = putint(ptr,(SNAP_integer) defaulti);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strncpy(OutVal, dummy, OutLim);
    ptr = getint(ptr, &temp);
    *resulti = (int) temp;
    
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_WriteUnscribedBodyFile(DirName, id, FileName)
char *DirName, *id, *FileName;
{
    PROCNAME name = "MS_WriteUnscribedBodyFile";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_WRITE_UNSCRIBED);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, id);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(FileName, dummy);
    
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_SubmitMessage (FileName, DeliveryOptions, ErrorMessage, ErrMsgLimit, ClientProgram)
char   *FileName;
int	DeliveryOptions,
	ErrMsgLimit;
char *ErrorMessage, *ClientProgram;	
{
    PROCNAME name = "MS_SubmitMessage";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;
    char   *dummy;
    SNAP_CPARMS cp;

    cp.maxtime = 300;		/* five minutes */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_SUBMIT_MESSAGE);
    ptr = putstr(CallPacketArgs(request), FileName);
    ptr = putint(ptr,(SNAP_integer) DeliveryOptions);
    ptr = putint(ptr,(SNAP_integer) ErrMsgLimit);
    ptr = putstr(ptr, ClientProgram);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(ErrorMessage, dummy);

    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_StorePartialFile (FileName, startpos, len, mode, Truncate, WhatToStore)
char   *FileName,
       *WhatToStore;
long	startpos;
int	len,
	mode,
	Truncate;
{
    PROCNAME name = "MS_StorePartialFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_STORE_PARTIAL_FIL);
    ptr = putstr(CallPacketArgs(request), FileName);
    ptr = putint(ptr,(SNAP_integer) startpos);
    ptr = putint(ptr,(SNAP_integer) len);
    ptr = putint(ptr,(SNAP_integer) mode);
    ptr = putint(ptr,(SNAP_integer) Truncate);
#ifdef MAC
    /*too bad there isn't a snap putbytes that translates
      end of line from local to foreign.  If ther was
      we wouldn't have to translate the users buffer back.
    */
    local_to_foreign(WhatToStore, len);
#endif /* MAC */
    ptr = putbytes(ptr, WhatToStore, len);
#ifdef MAC
    foreign_to_local(WhatToStore, len);
#endif /* MAC */
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_UpdateState () {
    PROCNAME name = "MS_UpdateState";
/*    register char *ptr; */
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_UPDATE_STATE);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}
long	MS_FastUpdateState () {
/*    register char *ptr; */
#ifdef OLDCODE
    PROCNAME name = "MS_FastUpdateState";
    int result;
    SNAP_integer rval;
#endif /* OLDCODE */
    int length;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arg */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_FAST_UPDATE);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC  -- no waiting as of 1/3/89 */
    SNAP_SendNoReply(MScid, request, length, NULL);
#ifdef OLDCODE
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
#endif /* OLDCODE */
    CUI_RPCInProgress = 0;
    return(0);
}
long	MS_InstallWelcomeMessage (ParentName, InitDir, InitFile, ShortName)
char   *ParentName,
       *InitDir,
       *InitFile,
       *ShortName;
{
    PROCNAME name = "MS_InstallWelcomeMessage";
    register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_INSTALL_WELCOME_M);
    ptr = putstr(CallPacketArgs(request), ParentName);
    ptr = putstr(ptr, InitDir);
    ptr = putstr(ptr, InitFile);
    ptr = putstr(ptr, ShortName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Extract the return value & yield it */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ReInitialize () {
     PROCNAME name = "MS_ReInitialze";
     register char  *ptr;
    int     length,
	    result;
    SNAP_integer rval,longlen;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    ptr= putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REINITIALIZE);
    ptr = putint(ptr,(SNAP_integer)AMS_MAJOR_VERSION);
    ptr = putint(ptr,(SNAP_integer)AMS_MINOR_VERSION);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Extract the return value & yield it */
    getint(ReturnPacketLength(reply), &longlen);
    ptr =  getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

/* get version dependant configurations string*/
long MS_GetVConfig(key,vers,result_str)
char *key;
char *vers;
char *result_str;
{PROCNAME name = "MS_GetVConfig";
 register char  *ptr;
 int length;
 int result;
 char *dummy;
 SNAP_integer rval;

 CUI_RPCInProgress = 1;

 /* Pack the arguments */
 (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_VERSION_CONFIG);
 ptr = putstr(CallPacketArgs(request),key);
 ptr = putstr(ptr,vers);
 length = ptr - request;
 (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
 result = DoRPC(name, request, length, reply, BufferSize, NULL);

 if (result != RPC_OK) {
  CUI_RPCInProgress = 0;
  return CUI_RPC_ERROR(result);
 }
 /* Extract the return value & yield it */
 ptr = getstr(ReturnPacketArgs(reply), &dummy);
 strcpy(result_str, dummy);
 (void) getint(ReturnPacketReturnValue(reply), &rval);
 CUI_RPCInProgress = 0;
 return(long) rval;
}

long	MS_ReconstructDirectory (DirName, NumGood, NumBad, TrustTimeStamp)
char   *DirName;
int    *NumGood,
       *NumBad,
	TrustTimeStamp;
{
    PROCNAME name = "MS_ReconstructDirectory";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_RECONSTRUCTDIRECT);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putint(ptr, (SNAP_integer) TrustTimeStamp);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ScavengeDirectory(DirName, Recurse, NumGood, NumBad, quiet, Purge)
char *DirName;
int Recurse, *NumGood, *NumBad, quiet, Purge;
/* added in Recurse, quiet, Purge; removed TrustTimeStamp */
{
    PROCNAME name = "MS_ScavengeDirectory";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_SCAVENGE);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putint(ptr, (SNAP_integer) Recurse);
    ptr = putint(ptr, (SNAP_integer) quiet);
    ptr = putint(ptr, (SNAP_integer) Purge);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_RebuildMasterUpdateFiles (NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood)
int    *NumFastGood,
       *NumSlowGood,
       *NumBad,
       *NumAbsent,
       *NumProbablyGood;
{
    PROCNAME name = "MS_RebuildMasterUpdateFiles";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REBUILDMASTERUP);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumFastGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumSlowGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    ptr = getint(ptr, &temp);
    *NumAbsent = (int) temp;
    ptr = getint(ptr, &temp);
    *NumProbablyGood = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_RebuildOneMasterUpdateFile (PathElt, NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood)
char *PathElt;
int    *NumFastGood,
       *NumSlowGood,
       *NumBad,
       *NumAbsent,
       *NumProbablyGood;
{
    PROCNAME name = "MS_RebuildOneMasterUpdateFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REINDEX_ONE);
    ptr = putstr(CallPacketArgs(request), PathElt);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumFastGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumSlowGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    ptr = getint(ptr, &temp);
    *NumAbsent = (int) temp;
    ptr = getint(ptr, &temp);
    *NumProbablyGood = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_NameChangedMapFile (MapFile, MailOnly, ListAll, NumChanged, NumUnavailable, NumMissingFolders, NumSlowpokes, NumFastFellas)
char   *MapFile;
int     MailOnly,
        ListAll,
       *NumChanged,
       *NumUnavailable,
       *NumMissingFolders,
       *NumSlowpokes,
       *NumFastFellas;
{
    PROCNAME name = "MS_NameChangedMapFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;
    char *dummy;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_NAMECHANGEDMAP);
    ptr = putint(CallPacketArgs(request), (SNAP_integer) MailOnly);
    ptr = putint(ptr, (SNAP_integer) ListAll);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(MapFile, dummy);
    ptr = getint(ptr, &temp);
    *NumChanged = (int) temp;
    ptr = getint(ptr, &temp);
    *NumUnavailable = (int) temp;
    ptr = getint(ptr, &temp);
    *NumMissingFolders = (int) temp;
    ptr = getint(ptr, &temp);
    *NumSlowpokes = (int) temp;
    ptr = getint(ptr, &temp);
    *NumFastFellas = (int) temp;
    ptr = getint(ReturnPacketArgs(reply), &temp);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_FindMailbox (pathelt, Buf)
int	pathelt;
char   *Buf;
{
    PROCNAME name = "MS_FindMailbox";
    register char  *ptr;
    int     result,
	    length;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_FIND_MAILBOX);
    ptr = putint(CallPacketArgs(request),(SNAP_integer) pathelt);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(Buf, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_CloneMessage (SourceDirName, id, DestDirName, Code)
char   *SourceDirName,
       *id,
       *DestDirName;
int	Code;
{
    PROCNAME name = "MS_CloneMessage";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_CLONE_MESSAGE);
    ptr = putstr(CallPacketArgs(request), SourceDirName);
    ptr = putstr(ptr, id);
    ptr = putstr(ptr, DestDirName);
    ptr = putint(ptr,(SNAP_integer) Code);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_MergeDirectories (SourceDirName, DestDirName)
char   *SourceDirName,
       *DestDirName;
{
    PROCNAME name = "MS_MergeDirectories";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_MERGE_DIRECTORIES);
    ptr = putstr(CallPacketArgs(request), SourceDirName);
    ptr = putstr(ptr, DestDirName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long MS_TakeHints(DoAll, ProtFailures)
int DoAll;
int *ProtFailures;
{
    PROCNAME name = "MS_TakeHints";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;


    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_TAKEHINTS);
    ptr = putint(CallPacketArgs(request), DoAll);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *ProtFailures = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetDirInfo (DirName, ProtCode, MsgCount)
char   *DirName;
int    *ProtCode,
       *MsgCount;
{
    PROCNAME name = "MS_GetDirInfo";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;


    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GETDIRINFO);
    ptr = putstr(CallPacketArgs(request), DirName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *ProtCode = (int) temp;
    ptr = getint(ptr, &temp);
    *MsgCount = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetDirAttributes (DirName, AttrCt, Attrs, SepChar, ShowEmpty)
char   *DirName;
int    *AttrCt;
char *Attrs;
int SepChar, ShowEmpty;
{
    PROCNAME name = "MS_GetDirAttributes";
    register char  *ptr;
    int     result,
	    length;
    char *dummy;
    SNAP_integer rval, temp;


    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GETDIRATTRS);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putint(ptr, (SNAP_integer) SepChar);
    ptr = putint(ptr, (SNAP_integer) ShowEmpty);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *AttrCt = (int) temp;
    ptr = getbytes(ptr, &dummy);
    bcopy(dummy, Attrs, AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX));
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ParseDate (indate, year, month, day, hour, min, sec, wday, gtm)
char   *indate;
int    *year, *month, *day, *hour, *min, *sec, *wday;
long   *gtm;
{
    PROCNAME name = "MS_ParseDate";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;


    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_PARSE_DATE);
    ptr = putstr(CallPacketArgs(request), indate);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *year = (int) temp;
    ptr = getint(ptr, &temp);
    *month = (int) temp;
    ptr = getint(ptr, &temp);
    *day = (int) temp;
    ptr = getint(ptr, &temp);
    *hour = (int) temp;
    ptr = getint(ptr, &temp);
    *min = (int) temp;
    ptr = getint(ptr, &temp);
    *sec = (int) temp;
    ptr = getint(ptr, &temp);
    *wday = (int) temp;
    ptr = getint(ptr, &temp);
    *gtm = (long) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_DoIHaveMail (count)
int    *count;
{
    PROCNAME name = "MS_DoIHaveMail";
    int     result,
	    length;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_DO_I_HAVE_MAIL);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    (void) getint(ReturnPacketArgs(reply), &temp);
    *count = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_CheckAuthentication (Authenticated)
int    *Authenticated;
{
    PROCNAME name = "MS_CheckAuthentication";
    int     result,
	    length;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_CHECKAUTHENTICATION);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    (void) getint(ReturnPacketArgs(reply), &temp);
    *Authenticated = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_SetDeathKnell (dk)
int    dk;
{
    PROCNAME name = "MS_SetDeathKnell";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_SET_DEATHKNELL);
    ptr = putint(CallPacketArgs(request), (SNAP_integer) dk);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_GetNewMessageCount (FullDirName, numnew, numtotal, LastOldDate, InsistOnFetch)
char   *FullDirName, *LastOldDate;
long    *numnew, *numtotal, InsistOnFetch;
{
    PROCNAME name = "MS_GetNewMessageCount";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    char *dummy;


    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_GET_NEW_MSG_CT);
    ptr = putstr(CallPacketArgs(request), FullDirName);
    ptr = putint(ptr, (SNAP_integer) InsistOnFetch);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *numnew = (int) temp;
    ptr = getint(ptr, &temp);
    *numtotal = (int) temp;
    ptr = getstr(ptr, &dummy);
    strcpy(LastOldDate, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_RemoveDirectory (DirName, MaxRemovals)
char   *DirName;
int MaxRemovals;
{
    PROCNAME name = "MS_RemoveDirectory";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_REMOVE_DIRECTORY);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putint(ptr, (SNAP_integer) MaxRemovals);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */

    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_UnlinkFile (FileName)
char   *FileName;
{
    PROCNAME name = "MS_UnlinkFile";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_UNLINK_FILE);
    ptr = putstr(CallPacketArgs(request), FileName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */

    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_DeleteAttr (DirName, AttrName)
char   *DirName, *AttrName;
{
    PROCNAME name = "MS_DeleteAttr";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_DELETEATTR);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, AttrName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */

    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_AddAttribute (DirName, AttrName, AttNum)
char   *DirName, *AttrName;
int *AttNum;
{
    PROCNAME name = "MS_AddAttribute";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_ADDATTR);
    ptr = putstr(CallPacketArgs(request), DirName);
    ptr = putstr(ptr, AttrName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */

    (void) getint(ReturnPacketReturnValue(reply), &rval);
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *AttNum = (int) temp;
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_EditMessage (dirname, id, NewBodyFile, Reparse)
char   *dirname,
       *id,
       *NewBodyFile;
int	Reparse;
{
    PROCNAME name = "MS_EditMessage";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_EDIT_MESSAGE);
    ptr = putstr(CallPacketArgs(request), dirname);
    ptr = putstr(ptr, id);
    ptr = putstr(ptr, NewBodyFile);
    ptr = putint(ptr, (SNAP_integer) Reparse);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);


 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);
    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_ConvertOldMail(NumGood, NumBad)
int    *NumGood,
       *NumBad;
{
    PROCNAME name = "MS_ConvertOldMail";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval, temp;
    SNAP_CPARMS cp;

    cp.maxtime = 3600;		/* one hour -- This is a rare call and can
				   take a very very long time */
    cp.encryptlevel = SNAP_ENCRYPT;
    cp.timeout = 20;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_CONVERT_OLD_MAIL);
    length = CALL_PACKET_HEADER_LENGTH;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, &cp);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }

 /* Unpack results */
    ptr = getint(ReturnPacketArgs(reply), &temp);
    *NumGood = (int) temp;
    ptr = getint(ptr, &temp);
    *NumBad = (int) temp;
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_RenameDir (OldName, NewName, NewFullName)
char   *OldName,
       *NewName,
       *NewFullName;
{
    PROCNAME name = "MS_RenameDir";
    register char  *ptr;
    int     length,
	    result;
    char   *dummy;
    SNAP_integer rval;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the arguments */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_RENAME_DIR);
    ptr = putstr(CallPacketArgs(request), OldName);
    ptr = putstr(ptr, NewName);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
    ptr = getbytes(ReturnPacketArgs(reply), &dummy);
    strcpy(NewFullName, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

long	MS_MatchFolderName (pat, OutFile)
char   *pat,
       *OutFile;
{
    PROCNAME name = "MS_MatchFolderName";
    register char  *ptr;
    int     result,
	    length;
    SNAP_integer rval;
    char   *dummy;

    CUI_RPCInProgress = 1;
 /* Check the length of the args */

 /* Pack the args */
    (void) putint(CallPacketOpCode(request),(SNAP_integer) OP_MS_MATCH_FOLDER_NAME);
    ptr = putstr(CallPacketArgs(request), pat);
    length = ptr - request;
    (void) putint(CallPacketLength(request),(SNAP_integer) length);

 /* Make the RPC */
    result = DoRPC(name, request, length, reply, BufferSize, NULL);

    if (result != RPC_OK) {
	CUI_RPCInProgress = 0;
	return CUI_RPC_ERROR(result);
    }
 /* Unpack results */
    (void) getstr(ReturnPacketArgs(reply), &dummy);
    strcpy(OutFile, dummy);
    (void) getint(ReturnPacketReturnValue(reply), &rval);
    CUI_RPCInProgress = 0;
    return(long) rval;
}

ConsiderRetrying(name, retries, restarts)
char   *name;
int	retries,
       *restarts;
{
    switch (HandleTimeout(name, retries, *restarts)) {
	case CUI_RPC_RESTART:
	    ++*restarts;
	    if (ReconnectMS(NULL) != SNAP_SUCCESS) {
		return(CUI_RPC_BUGOUT);
	    }
	    else {
		DidRestart();
	    }
	/* fall through */
	case CUI_RPC_RETRY:
	    return(CUI_RPC_RETRY);
	default:
	    return(CUI_RPC_BUGOUT);
    }
}

/* This allows the real getandpacktokens to be 
    called in the standalone version by dropoff on Andrew. */

AMS_getandpackALLtokens(pWhere, pWhereLen, pWhereMax, deb)
    char **pWhere;
    int *pWhereLen, *pWhereMax;
    int deb;
{
    return(GetAndPackAllTokens(pWhere, pWhereLen, pWhereMax, deb));
}

/* This is a no-op in the snapified version */

ConsiderLoggingRead() {};

/* This is not to be done in the snapified version */

MS_SetCleanupZombies() {
    ReportError("SNAPified applications cannot affect MS zombies!", ERR_WARNING, FALSE);
}
