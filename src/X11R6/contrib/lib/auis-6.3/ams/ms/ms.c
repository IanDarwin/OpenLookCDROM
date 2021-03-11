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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/ms/RCS/ms.c,v 2.29 1994/03/29 03:48:10 rr2b Exp $";
#endif


 

/*
		Message server process.
*/

#include <andrewos.h> /* sys/file.h sys/time.h */
#include <stdio.h>
#include <ms.h>
#include <sys/socket.h>
#include <sys/wait.h>

#include <gasp.h>
#include <snap.h>
#include <snapams.h>
#include <mserrno.h>
#include <errprntf.h>
#include <mailconf.h>

typedef char bool;
#define FALSE	0
#define TRUE	1

char ProgramName[100] = "ms";
char *CUI_ClientVersion= "ms $Revision: 2.29 $";

char *SnapVersionString;

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern *getprofile();
extern char home[];
extern int SNAP_debugmask, MSDebugging;

int MSDebugging = 0;

/* Forward Declarations */
static void Execute();

static int BufferSize;	/* Max buffer size */
static ReturnPacket *reply;
static char *ResultBuffer;
static int IWantToDie = 0;
/*seconds till death*/
#define NEVER_DIE (0x7fffffff)
#define NO_CONN_DIE_TIME (5*60) /*die in 5 min if no client*/
static long time_till_death;

/* Modifiable values */

	/* How long to wait for input before sending a count to guardian 
	   and doing other stuff, like updating state to the profile file. */
#define COUNT_STIMEOUT	61
#define COUNT_LTIMEOUT	301
extern int NeedToTimeOut;
extern int SwapPerformanceTimeout, SwapPerformanceBackoff, DeathKnell;
extern char Me[];

static void FetchExecute(clientfd)
int clientfd;
{
    int rfd, nfds;
    struct timeval timeout;
    static int ntimeouts = 0;
    char *request;
    static int last_noticed_conv_count= -23;

    /* Wait for some action */
    rfd = 1 << clientfd;
    if (SwapPerformanceTimeout>0 && (SwapPerformanceBackoff > ntimeouts)) {
	timeout.tv_sec = SwapPerformanceTimeout;
    } else {
	timeout.tv_sec = NeedToTimeOut ? COUNT_STIMEOUT : COUNT_LTIMEOUT;
    }
    /*can't defeat DeathKnell with long swap timeout*/
    if(time_till_death<=0)time_till_death=1;
    if(timeout.tv_sec>time_till_death)
	timeout.tv_sec=time_till_death;
    timeout.tv_usec = 0;

    nfds = select(32, &rfd, 0, 0, &timeout);
    if (nfds < 0) {
	FatalError("select failed");
	safeexit(1); /* not reached */
    }

    /* death is comming*/
    time_till_death -= timeout.tv_sec;

    /* See if it was a timeout */
    if (nfds != 0) {
	/* It must be input */
	ntimeouts = 0;
	if (rfd & (1<<clientfd)) {
	    int len, msgtype, cid;

	    len = SNAP_Accept(&request, &msgtype, &cid, COUNT_STIMEOUT);
	    if (len >= 0) {
		int new_noticed_count;
		Execute(request, len, msgtype, cid);
		/*we did something so refreash death time*/
		time_till_death=((DeathKnell==0)?NEVER_DIE:DeathKnell);
		new_noticed_count=SNAP_ConvCount();
		if(last_noticed_conv_count!=new_noticed_count)
		    GASP_Count(last_noticed_conv_count=new_noticed_count);
		if(last_noticed_conv_count==0)
		    time_till_death=NO_CONN_DIE_TIME;
		return;
	    }
	}
    }

    /* It was a timeout */
    ++ntimeouts;
    if (time_till_death<=2) {
	MS_UpdateState();
	errprintf("ms", ERR_MONITOR, 0, 0, "Message server for %s quitting due to inactivity", Me);
	safeexit(0);
    }
    MS_FastUpdateState();
    last_noticed_conv_count=SNAP_ConvCount();
    GASP_Count(last_noticed_conv_count);
}

main(argc, argv)
int argc;
char *argv[];
{
    SNAP_CPARMS connparms;	/* For setting connection parameters to SNAP */
    char *client;
    int clientfd, auth, result;
    char ErrorText[256];
    int cfd, versions[4];

    chdir("/tmp"); /* A designated repository for anonymous core dumps... */
    amsconfig(argc, argv, "ms");

    if (!access("/DebugMS", F_OK)) {
	SNAP_debugmask = 0xffff;
	MSDebugging = 1;
	MS_OpenDebuggingPipescript(1);
    } else {
	/* Redirect stdout & stderr */
	if (AMS_DevConsoleIsSacred) {
	    cfd = open("/dev/console", O_WRONLY, 0644);
	    if (cfd>=0) { /* If I could not open /dev/console, I just hope for the best */
		dup2(cfd, 1);
		dup2(cfd, 2);
		setlinebuf(fdopen(1, "w"));
		setlinebuf(fdopen(2, "w"));
		close(cfd);
	    } else {
		printf("Warning: ms cannot open /dev/console");
		fprintf(stderr, "Warning: ms cannot open /dev/console");
		fflush(stdout);
		fflush(stderr);
	    }
	}
    }

    /* Initialize ms code */ 	/* Get back max buffer size */
    SnapVersionString = malloc(25);
    SNAP_ServerVersion(-1, versions);
    sprintf(SnapVersionString, "%d.%d", versions[2], versions[3]);
    if (MS_Initialize(&BufferSize, TRUE)) {
	IWantToDie = mserrcode;
    }

    /* Don't fill up local disk with ms core dumps.  The earlier cd to /tmp
      was for core dumps before the call to MS_Initialize */
    chdir(home); 
    /* Try to allocate receive & reply buffers */
    reply = (ReturnPacket *) malloc(BufferSize);
    ResultBuffer = (char *) malloc(BufferSize);
    if (reply == NIL || ResultBuffer == NIL) {
	sprintf(ErrorText,
		"Can't allocate buffers: reply=0x%x, result=0x%x\n",
		reply, ResultBuffer);
	FatalError(ErrorText);
	safeexit(1); /* not reached */
    }
    *ResultBuffer = '\0';

    /* Intialize GASP & SNAP */
    connparms.maxtime = COUNT_STIMEOUT;
    connparms.timeout = 1;
    connparms.maxmsgsize = BufferSize;
    connparms.encryptlevel = SNAP_ENCRYPT;
    result = GASP_ServerInit(argc, argv, &connparms, &client, &clientfd, &auth);
    if (result != 0) {
	/*	sprintf(ErrorText, "[MS]: GASP_ServerInit failed: %d\n", result); */
	/*	FatalError(ErrorText); */
	/*	Try to be less cryptic here -- this is what users see when they */
	/*	hand-run ms by accident. */
	errprintf("ms", ERR_WARNING, 0, 0, "Unrecognizable command line arguments to message server %d", result);
	safeexit(1);
    }

    time_till_death=NO_CONN_DIE_TIME; /*schedual an early death if no client*/
    /* Now go into loop waiting for commands */
    for (;;) FetchExecute(clientfd);
}

static void ReplyFailure(proc, result)
char *proc;
int result;
{
    char ErrorText[256];

    sprintf(ErrorText, "SNAP_reply failure in routine %s: %d\n", proc, result);
    FatalError(ErrorText);
    safeexit(1);
}

static void SendError(code, cid)
int code, cid;
{
    (void) putint(ReturnPacketLength(reply), (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
    (void) putint(ReturnPacketError(reply), (SNAP_integer) code);
    (void) SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
}

static void ReportAndSendCallError(proc, packet, reason, cid)
char *proc;
CallPacket *packet;
int reason, cid;
{
    register int i;

    fprintf(stderr, "[MS]: Bad packet (%d): 0x", reason);
    for (i=0; i<CALL_PACKET_HEADER_LENGTH; i++)
	fprintf(stderr, "%02x", (unsigned char) packet[i]);
    fputc('\n', stderr);
    SendError(reason, cid);
}

static bool CallLengthError(packet)
CallPacket *packet;
{
    SNAP_integer len; 

    (void) getint(CallPacketLength(packet), &len);
    return (int) len > BufferSize;
}

/**************************************\
  * 				       *
  *  The stand-ins for the MS routines   *
      * 				       *
      \**************************************/

static void ms_create_new_message_directory(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_CreateNewMessageDirectory";
    register char *ptr;
    char *DirName, *BodyDirName;
    SNAP_integer Overwrite;

    /* Unpack call arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getint(ptr, &Overwrite);
    ptr = getstr(ptr, &BodyDirName);

    /* Call the MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_CreateNewMessageDirectory(DirName,
							       (int) Overwrite,
							       BodyDirName));

    if (type == SNAP_SENDWITHREPLY) {
	int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}


static void ms_append_file_to_folder(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_AppendFileToFolder";
    register char *ptr;
    char *FileName, *FolderName;

    /* Unpack call arguments */
    ptr = getstr(CallPacketArgs(buffer), &FileName);
    ptr = getstr(ptr, &FolderName);

    /* Call the MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_AppendFileToFolder(FileName, FolderName));

    if (type == SNAP_SENDWITHREPLY) {
	int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_process_new_messages(buffer, len, type, cid, opcode)
CallPacket *buffer;
int len, type, cid, opcode;
{
    static char name[] = "MS_ProcessNewMessages";
    register char *ptr;
    char *SourceDir, *ParseSpecFile;
    int NumGood, NumBad, NumLocks, NumInProgress, resultcode, FirstError, buflim;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &SourceDir);
    ptr = getstr(ptr, &ParseSpecFile);
    if (opcode == OP_MS_PROCESS_NEW_MESSA) {    /* If it's the new version, there's one more arg to unpack */
	(void) getint(ptr, &buflim);

	/* The real buflim is the requested one min'd with the avilable one */

	buflim = (buflim > BufferSize) ? BufferSize : buflim;
	*ResultBuffer = '\0';
    }
    else
	buflim = 0;

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ProcessNewMessages(SourceDir,
							&NumGood, &NumBad, 
							&NumLocks, ParseSpecFile,
							&resultcode, &FirstError,
							&NumInProgress,
							(buflim ? ResultBuffer : NULL),
							buflim));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	ptr = putint(ptr, (SNAP_integer) NumLocks);
	ptr = putint(ptr, (SNAP_integer) resultcode);
	ptr = putint(ptr, (SNAP_integer) FirstError);
	ptr = putint(ptr, (SNAP_integer) NumInProgress);
	if (buflim)
	    ptr = putstr(ptr, ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_reconstruct_directory(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ReconstructDirectory";
    register char *ptr;
    char *DirName;
    int NumGood, NumBad, TrustTimeStamp;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getint(ptr, &TrustTimeStamp);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ReconstructDirectory(DirName, 
							  &NumGood, &NumBad, TrustTimeStamp));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_scavenge_directory(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ScavengeDirectory";
    register char *ptr;
    char *DirName;
    int NumGood, NumBad, Recurse, Quiet, Purge;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getint(ptr, &Recurse);
    ptr = getint(ptr, &Quiet);
    ptr = getint(ptr, &Purge);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ScavengeDirectory(DirName, Recurse,
							  &NumGood, &NumBad, Quiet, Purge));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void generic_ms_rebuild_master_update(buffer, len, type, cid, IsOld)
CallPacket *buffer;
int len, type, cid, IsOld;
{    
    static char name[] = "MS_RebuildMasterUpdate";
    register char *ptr;
    int NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood;

    /* Unpack args */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RebuildMasterUpdateFiles(&NumFastGood, 
							      &NumSlowGood, &NumBad, &NumAbsent, &NumProbablyGood));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumFastGood);
	ptr = putint(ptr, (SNAP_integer) NumSlowGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	ptr = putint(ptr, (SNAP_integer) NumAbsent);
	if (!IsOld) ptr = putint(ptr, (SNAP_integer) NumProbablyGood);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_rebuild_master_update(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    generic_ms_rebuild_master_update(buffer, len, type, cid, 0);
}

static void ms_rebuild_master_update_old(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    generic_ms_rebuild_master_update(buffer, len, type, cid, 1);
}

static void generic_ms_rebuild_one_master_update_file(buffer, len, type, cid, IsOld)
CallPacket *buffer;
int len, type, cid, IsOld;
{

    static char name[] = "MS_RebuildOneMasterUpdateFile";
    register char *ptr;
    int NumFastGood, NumSlowGood, NumBad, NumAbsent, NumProbablyGood;
    char *PathElt;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &PathElt);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RebuildOneMasterUpdateFile(PathElt, &NumFastGood, &NumSlowGood, &NumBad, &NumAbsent, &NumProbablyGood));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumFastGood);
	ptr = putint(ptr, (SNAP_integer) NumSlowGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	ptr = putint(ptr, (SNAP_integer) NumAbsent);
	if (!IsOld) ptr = putint(ptr, (SNAP_integer) NumProbablyGood);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_rebuild_one_master_update_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    generic_ms_rebuild_one_master_update_file(buffer, len, type, cid, 0);
}

static void ms_rebuild_one_master_update_file_old(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    generic_ms_rebuild_one_master_update_file(buffer, len, type, cid, 1);
}


static void ms_name_changed_map(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_NameChangedMapFile";
    register char *ptr;
    int NumChanged, NumUnavailable, NumMissingFolders, NumSlowpokes, NumFastFellas, MailOnly, ListAll;

    /* Unpack args */
    ptr = getint(CallPacketArgs(buffer), &MailOnly);
    ptr = getint(ptr, &ListAll);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_NameChangedMapFile(ResultBuffer, MailOnly, ListAll, &NumChanged, &NumUnavailable, &NumMissingFolders, &NumSlowpokes, &NumFastFellas));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putint(ptr, (SNAP_integer) NumChanged);
	ptr = putint(ptr, (SNAP_integer) NumUnavailable);
	ptr = putint(ptr, (SNAP_integer) NumMissingFolders);
	ptr = putint(ptr, (SNAP_integer) NumSlowpokes);
	ptr = putint(ptr, (SNAP_integer) NumFastFellas);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_headers_since(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_HeadersSince";
    register char *ptr;
    char *FullDirName, *datefield;
    SNAP_integer MaxReturn, startbyte;
    int numbytes, bytesleft;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FullDirName);
    ptr = getstr(ptr, &datefield);
    ptr = getint(ptr, &MaxReturn);
    ptr = getint(ptr, &startbyte);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_HeadersSince(FullDirName, datefield, ResultBuffer, (int) MaxReturn, (int) startbyte, &numbytes, &bytesleft));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, numbytes);
	ptr = putint(ptr, (SNAP_integer) numbytes);
	ptr = putint(ptr, (SNAP_integer) bytesleft);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void real_ms_print_message(buffer, len, type, cid, GetPrinter)
CallPacket *buffer;
int len, type, cid, GetPrinter;
{
    static char name[] = "MS_PrintMessage";
    register char *ptr;
    char *DirName, *id, *printer;
    SNAP_integer flags;

    debug(1, ("PrintMessage with getprinter %d\n", GetPrinter));
    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &id);
    ptr = getint(ptr, &flags);
    if (GetPrinter) {
	ptr = getstr(ptr, &printer);
    } else {
	printer = NULL;
    }

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_PrintMessage(DirName, id, (int) flags, printer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_print_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    real_ms_print_message(buffer, len, type, cid, 1);
}

static void old_ms_print_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    real_ms_print_message(buffer, len, type, cid, 0);
}


static void ms_prefetch_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_PrefetchMessage";
    register char *ptr;
    char *DirName, *id;
    SNAP_integer GetNext;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &id);
    ptr = getint(ptr, &GetNext);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_PrefetchMessage(DirName, id, (int) GetNext));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_version(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetVersion";
    SNAP_integer lim;

    /* Get argument */
    (void) getint(CallPacketArgs(buffer), &lim);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetVersion(ResultBuffer, (int) lim));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;
	register char *ptr;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_debug_mode(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DebugMode";
    register char *ptr;
    SNAP_integer level, snap, malloc;

    /* Unpack args */
    ptr = getint(CallPacketArgs(buffer), &level);
    ptr = getint(ptr, &snap);
    (void) getint(ptr, &malloc);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_DebugMode((int) level,
					       (int) snap,
					       (int) malloc));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_name_subscription_map_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_NameSubscriptionMapFile";
    register char *ptr;
    char *Root;

    /* Unpack args */
    (void) getstr(CallPacketArgs(buffer), &Root);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_NameSubscriptionMapFile(Root, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_match_folder_name(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_MatchFolderName";
    register char *ptr;
    char *pat;

    /* Unpack args */
    (void) getstr(CallPacketArgs(buffer), &pat);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_MatchFolderName(pat, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_check_missing_folder(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_CheckMissingFolder";
    register char *ptr;
    char *source;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &source);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_CheckMissingFolder(source, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_disambiguate_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DisambiguateFile";
    register char *ptr;
    char *source;
    SNAP_integer MustBeDir;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &source);
    (void) getint(ptr, &MustBeDir);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_DisambiguateFile(source, ResultBuffer, (int) MustBeDir));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_partial_body(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetPartialBody";
    register char *ptr;
    char *DirName, *id;
    SNAP_integer BufLim, offset;
    int remaining, ct;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &id);
    ptr = getint(ptr, &BufLim);
    ptr = getint(ptr, &offset);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetPartialBody(DirName, id,
						    ResultBuffer,
						    (int) BufLim, (int) offset,
						    &remaining, &ct));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, ct);
	ptr = putint(ptr, (SNAP_integer) remaining);
	ptr = putint(ptr, (SNAP_integer) ct);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_config_parms(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetConfigurationParameters";
    register char *ptr;
    SNAP_integer usedeliv, usesep, deltype;

    /* Unpack args */
    ptr = getint(CallPacketArgs(buffer), &len);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetConfigurationParameters(ResultBuffer, len, &usedeliv, &usesep, &deltype));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putint(ptr, (SNAP_integer) usedeliv);
	ptr = putint(ptr, (SNAP_integer) usesep);
	ptr = putint(ptr, (SNAP_integer) deltype);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_vconfig(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetVConfig";
    char *ptr, *key, *vers;
    auto char buf[2*MAXPATHLEN];

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer),&key);
    ptr = getstr(ptr,&vers);

    buf[0]=0;
    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetVConfig(key,vers,buf));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), buf);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}


#ifdef NOTUSED
static void ms_andrewdir(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_AndrewDir";
    register char *ptr;

    /* Unpack args */
    ptr = getint(CallPacketArgs(buffer), &len);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_AndrewDir(ResultBuffer, len));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
#endif /* NOTUSED */

static void ms_get_partial_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetPartialFile";
    register char *ptr;
    char *FileName;
    SNAP_integer BufLim, offset;
    int remaining, ct;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FileName);
    ptr = getint(ptr, &BufLim);
    ptr = getint(ptr, &offset);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetPartialFile(FileName, ResultBuffer,
						    (int) BufLim, (int) offset,
						    &remaining, &ct));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, ct);
	ptr = putint(ptr, (SNAP_integer) remaining);
	ptr = putint(ptr, (SNAP_integer) ct);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_set_associated_time(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_SetAssociatedTime";
    register char *ptr;
    char *FullName, *newvalue;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FullName);
    ptr = getstr(ptr, &newvalue);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_SetAssociatedTime(FullName, newvalue));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_associated_time(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetAssociatedTime";
    register char *ptr;
    char *FullName;
    SNAP_integer lim;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &FullName);
    (void) getint(ptr, &lim);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetAssociatedTime(FullName, ResultBuffer, (int) lim));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;
	register char *ptr;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_search_path_entry(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetSearchPathEntry";
    register char *ptr;
    SNAP_integer which, lim;

    /* Get arguments */
    ptr = getint(CallPacketArgs(buffer), &which);
    (void) getint(ptr, &lim);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetSearchPathEntry((int) which, ResultBuffer, (int) lim));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;
	register char *ptr;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_set_subscription_entry(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_SetSubscriptionEntry";
    register char *ptr;
    char *FullName, *NickName;
    SNAP_integer status;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &FullName);
    ptr = getstr(ptr, &NickName);
    (void) getint(ptr, &status);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_SetSubscriptionEntry(FullName, NickName, (int) status));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_subscription_entry(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetSubscriptionEntry";
    register char *ptr;
    char *FullName;
    int status;

    /* Get arguments */
    (void) getstr(CallPacketArgs(buffer), &FullName);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetSubscriptionEntry(FullName, ResultBuffer, &status));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putint(ptr, (SNAP_integer) status);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_domain_handles_formatting(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DomainHandlesFormatting";
    register char *ptr;
    char *DomName;
    int retVal;

    /* Get arguments */
    (void) getstr(CallPacketArgs(buffer), &DomName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_DomainHandlesFormatting(DomName, &retVal));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;

	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) retVal);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_next_subs_entry(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetNextSubsEntry";
    register char *ptr;
    char *FullName, Results2[500];
    int status;

    /* Get arguments */
    (void) getstr(CallPacketArgs(buffer), &FullName);

    /* Initialize result buffer */
    strcpy(ResultBuffer, FullName);
    Results2[0] = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetNextSubsEntry(ResultBuffer, Results2, &status));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putstr(ptr, Results2);
	ptr = putint(ptr, (SNAP_integer) status);
	rlength = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_die(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_Die";

    /* Unpack args */

    /* Call MS routine SECOND on this one */

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketReturnValue(reply), (SNAP_integer) 0);
	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
    MS_Die(); /* Never returns */
}

static void ms_open_debugging_pipescript(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DebugMode";
    SNAP_integer DoIt;

    /* Unpack args */
    (void) getint(CallPacketArgs(buffer), &DoIt);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_OpenDebuggingPipescript((int) DoIt));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_rebuild_subscription_maps(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_RebuildSubscriptionMaps";

    /* Get argument */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RebuildSubscriptionMaps());

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;

	rlength = (SNAP_integer) RETURN_PACKET_HEADER_LENGTH;
	(void) putint(ReturnPacketLength(reply), rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_rebuild_one_subscription_map(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_RebuildOneSubscriptionMap";
    char *PathElt;

    /* Get argument */
    (void) getstr(CallPacketArgs(buffer), &PathElt);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RebuildOneSubscriptionMap(PathElt));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, rlength;

	rlength = (SNAP_integer) RETURN_PACKET_HEADER_LENGTH;
	(void) putint(ReturnPacketLength(reply), rlength);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, rlength, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_name_reply_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_NameReplyFile";
    register char *ptr;
    char *DirName, *id;
    SNAP_integer code;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &id);
    ptr = getint(ptr, &code);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_NameReplyFile(DirName, id, code,
						   ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_gen_temp_file_name(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GenTempFileName";
    register char *ptr;

    /* Unpack args */

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GenTempFileName(ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_write_all_matches_to_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_WriteAllMatchesToFile";
    register char *ptr;
    char *ambigname;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &ambigname);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_WriteAllMatchesToFile(ambigname, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_epoch(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_Epoch";
    register char *ptr;
    char *dirname, *date64;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    (void) getstr(ptr, &date64);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_Epoch(dirname, date64));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_alter_snapshot(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_AlterSnapshot";
    register char *ptr;
    char *dirname, *id, *NewSnapshot;
    int Code;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    ptr = getstr(ptr, &id);
    ptr = getbytes(ptr, &NewSnapshot);
    ptr = getint(ptr, &Code);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_AlterSnapshot(dirname, id, NewSnapshot, Code));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_purge_deleted_messages(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_PurgeDeletedMessages";
    char *dirname;

    /* Get arguments */
    (void) getstr(CallPacketArgs(buffer), &dirname);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_PurgeDeletedMessages(dirname));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_snapshot(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetSnapshot";
    register char *ptr;
    char *dirname, *id;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    ptr = getstr(ptr, &id);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetSnapshot(dirname, id, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, AMS_SNAPSHOTSIZE);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_nth_snapshot(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetSnapshot";
    register char *ptr;
    char *dirname;
    int n;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    ptr = getint(ptr, &n);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetNthSnapshot(dirname, n, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, AMS_SNAPSHOTSIZE);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_get_header_contents(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetHeaderContents";
    register char *ptr;
    char *dirname, *id, *HeaderName;
    SNAP_integer HeaderTypeNumber, lim;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    ptr = getstr(ptr, &id);
    ptr = getstr(ptr, &HeaderName);
    ptr = getint(ptr, &HeaderTypeNumber);
    ptr = getint(ptr, &lim);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetHeaderContents(dirname, id, HeaderName,
						       HeaderTypeNumber, ResultBuffer, lim));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_validate_chunk(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ValidateAndReplaceChunk";
    register char *ptr;
    char *FileName, *inaddr;
    SNAP_integer outaddrsize, which, outcode;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FileName);
    ptr = getstr(ptr, &inaddr);
    ptr = getint(ptr, &outaddrsize);
    ptr = getint(ptr, &which);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ValidateAndReplaceChunk(FileName, inaddr, ResultBuffer, outaddrsize, which, &outcode));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putbytes(ReturnPacketArgs(reply), ResultBuffer, outaddrsize);
	ptr = putint(ptr, outcode);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_handle_preference(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_HandlePreference";
    register char *ptr;
    char *prog, *pref, *InVal;
    SNAP_integer OutLim, opcode, defaulti, resulti;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &prog);
    ptr = getstr(ptr, &pref);
    ptr = getstr(ptr, &InVal);
    ptr = getint(ptr, &OutLim);
    ptr = getint(ptr, &opcode);
    ptr = getint(ptr, &defaulti);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_HandlePreference(prog, pref, InVal, ResultBuffer, OutLim, opcode, &resulti, defaulti));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putint(ptr, resulti);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_write_unscribed(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_WriteUnscribedBodyFile";
    register char *ptr;
    char *DirName, *id;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &id);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_WriteUnscribedBodyFile(DirName, id, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_submit_message(buffer, len, type, cid, opcode)
CallPacket *buffer;
int len, type, cid, opcode;
{
    static char name[] = "MS_SubmitMessage";
    register char *ptr;
    char *FileName, *ClientProgram;
    SNAP_integer DeliveryOptions, ErrMsgLimit;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FileName);
    ptr = getint(ptr, &DeliveryOptions);
    ptr = getint(ptr, &ErrMsgLimit);
    if (opcode == OLDOP_SUBMIT_MESSAGE) {
	ClientProgram = "Unknown Obsolete Interface";
    } else {
	ptr = getstr(ptr, &ClientProgram);
    }
    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_SubmitMessage(FileName, DeliveryOptions, ResultBuffer, ErrMsgLimit, ClientProgram));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_update_state(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_UpdateState";

    /* Unpack args */
    /* None */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_UpdateState());

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_fast_update_state(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_FastUpdateState";

    /* Unpack args */
    /* None */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_FastUpdateState());

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_store_partial_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_StorePartialFile";
    register char *ptr;
    char *FileName, *WhatToStore;
    SNAP_integer startpos, mylen, mode, Truncate;


    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &FileName);
    ptr = getint(ptr, &startpos);
    ptr = getint(ptr, &mylen);
    ptr = getint(ptr, &mode);
    ptr = getint(ptr, &Truncate);
    ptr = getbytes(ptr, &WhatToStore);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_StorePartialFile(FileName, startpos, mylen, mode, Truncate, WhatToStore));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* No return args */

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) (length = RETURN_PACKET_HEADER_LENGTH));
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_install_welcome_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_InstallWelcomeMessage";
    register char *ptr;
    char *ParentName, *InitDir, *InitFile, *ShortName;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &ParentName);
    ptr = getstr(ptr, &InitDir);
    ptr = getstr(ptr, &InitFile);
    ptr = getstr(ptr, &ShortName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_InstallWelcomeMessage(ParentName, InitDir, InitFile, ShortName));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) (length = RETURN_PACKET_HEADER_LENGTH));
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_reinitialize(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ReInitialize";
    int msresult;
    register char *ptr;

    /* Unpack args */

    /* Call MS routine */
    msresult = MS_ReInitialize();
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) msresult);
    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;
	ptr = putint(ReturnPacketArgs(reply),(SNAP_integer)AMS_MAJOR_VERSION);
	ptr = putint(ptr, (SNAP_integer)AMS_MINOR_VERSION);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
    if (msresult) {
	safeexit(-1);
    }
}

static void ms_rewrite_header_line(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_RewriteHeaderline";
    register char *ptr;
    char *old;
    int errcode;
    SNAP_integer newsize;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &old);
    ptr = getint(ptr, &newsize);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RewriteAddress(old, ResultBuffer, newsize, &errcode));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	ptr = putint(ptr, (SNAP_integer) errcode);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_find_mailbox(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_FindMailbox";
    register char *ptr;
    int pathelt;

    /* Unpack args */
    ptr = getint(CallPacketArgs(buffer), &pathelt);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_FindMailbox(pathelt, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_clone_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_CloneMessage";
    register char *ptr;
    char *SourceDirName, *id, *DestDirName;
    int Code;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &SourceDirName);
    ptr = getstr(ptr, &id);
    ptr = getstr(ptr, &DestDirName);
    ptr = getint(ptr, &Code);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_CloneMessage(SourceDirName, id, DestDirName, Code));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_merge_directories(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_MergeDirectories";
    register char *ptr;
    char *SourceDirName, *DestDirName;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &SourceDirName);
    ptr = getstr(ptr, &DestDirName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_MergeDirectories(SourceDirName, DestDirName));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_takehints(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_TakeHints";
    register char *ptr;
    register int length;
    int DoAll, ProtFailures;

    /* Get arguments */
    ptr = getint(CallPacketArgs(buffer), &DoAll);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_TakeHints(DoAll, &ProtFailures));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) ProtFailures);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_getdirinfo(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetDirInfo";
    register char *ptr;
    register int length;
    char *DirName;
    int ProtCode = -99, MsgCount = -99;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetDirInfo(DirName, &ProtCode, &MsgCount));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) ProtCode);
	ptr = putint(ptr, (SNAP_integer) MsgCount);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_getdirattributes(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetDirAttributes";
    register char *ptr;
    register int length;
    char *DirName;
    char Attrs[1+(AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX))];
    int AttrCt, SepChar, ShowEmpty;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getint(ptr, &SepChar);
    ptr = getint(ptr, &ShowEmpty);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetDirAttributes(DirName, &AttrCt, Attrs, SepChar, ShowEmpty));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) AttrCt);
	ptr = putbytes(ptr, Attrs, AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX));
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_parse_date(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ParseDate";
    register char *ptr;
    register int length;
    char *indate;
    int year, month, day, hour, min, sec, wday, gtm;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &indate);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ParseDate(indate, &year, &month, &day, &hour, &min, &sec, &wday, &gtm));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) year);
	ptr = putint(ptr, (SNAP_integer) month);
	ptr = putint(ptr, (SNAP_integer) day);
	ptr = putint(ptr, (SNAP_integer) hour);
	ptr = putint(ptr, (SNAP_integer) min);
	ptr = putint(ptr, (SNAP_integer) sec);
	ptr = putint(ptr, (SNAP_integer) wday);
	ptr = putint(ptr, (SNAP_integer) gtm);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_do_i_have_mail(buffer, len, type, cid)
      CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DoIHaveMail";
    register char *ptr;
    register int length;
    int count;

    /* Get arguments */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_DoIHaveMail(&count));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) count);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_check_authentication(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_CheckAuthentication";
    register char *ptr;
    register int length;
    int Authentication, rval;

    /* Get arguments */

    /* Call MS routine */
    rval = MS_CheckAuthentication(&Authentication);
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) rval);
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) Authentication);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
	if (!rval && !Authentication) {
	    /* I'm really unauthenticated, might as well die to avoid problems
	 with remote re-authentication */
	    NonfatalBizarreError("Message server terminating: I have lost my Vice authentication.");
	    exit(1);
	}
    }
}
static void ms_set_deathknell(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_SetDeathKnell";
    register int length;
    int dk;

    /* Get arguments */
    (void) getint(CallPacketArgs(buffer), &dk);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_SetDeathKnell(dk));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	length = CALL_PACKET_HEADER_LENGTH;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_get_new_msg_ct(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_GetNewMessageCount";
    register char *ptr;
    register int length;
    char *FullDirName;
    int numnew, numtotal, InsistOnFetch;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &FullDirName);
    ptr = getint(ptr, &InsistOnFetch);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_GetNewMessageCount(FullDirName, &numnew, &numtotal, ResultBuffer, InsistOnFetch));
    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) numnew);
	ptr = putint(ptr, (SNAP_integer) numtotal);
	ptr = putstr(ptr, ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_remove_directory(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_RemoveDirectory";
    register char *ptr;
    char *DirName;
    int MaxRemovals;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getint(ptr, &MaxRemovals);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RemoveDirectory(DirName, MaxRemovals));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_unlink_file(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_UnlinkFile";
    char *FileName;

    /* Get arguments */
    (void) getstr(CallPacketArgs(buffer), &FileName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_UnlinkFile(FileName));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_deleteattr(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_DeleteAttr";
    char *DirName, *AttrName;
    register char *ptr;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &AttrName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_DeleteAttr(DirName, AttrName));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_addattr(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_AddAttribute";
    char *DirName, *AttrName;
    int AttNum;
    register char *ptr;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &DirName);
    ptr = getstr(ptr, &AttrName);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_AddAttribute(DirName, AttrName, &AttNum));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) AttNum);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}
static void ms_edit_message(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_EditMessage";
    register char *ptr;
    char *dirname, *id, *NewBodyFile;
    int Reparse;

    /* Get arguments */
    ptr = getstr(CallPacketArgs(buffer), &dirname);
    ptr = getstr(ptr, &id);
    ptr = getstr(ptr, &NewBodyFile);
    ptr = getint(ptr, &Reparse);

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_EditMessage(dirname, id, NewBodyFile, Reparse));

    if (type == SNAP_SENDWITHREPLY) {
	register int result;

	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) RETURN_PACKET_HEADER_LENGTH);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, RETURN_PACKET_HEADER_LENGTH, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_convert_old_mail(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_ConvertOldMail";
    register char *ptr;
    int NumGood, NumBad;

    /* Unpack args */

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_ConvertOldMail(&NumGood, &NumBad));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putint(ReturnPacketArgs(reply), (SNAP_integer) NumGood);
	ptr = putint(ptr, (SNAP_integer) NumBad);
	length = ptr - reply;
	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void ms_rename_dir(buffer, len, type, cid)
CallPacket *buffer;
int len, type, cid;
{
    static char name[] = "MS_RenameDir";
    register char *ptr;
    char *OldName, *NewName;

    /* Unpack args */
    ptr = getstr(CallPacketArgs(buffer), &OldName);
    ptr = getstr(ptr, &NewName);

    /* Initialize result buffer */
    *ResultBuffer = '\0';

    /* Call MS routine */
    (void) putint(ReturnPacketReturnValue(reply),
		   (SNAP_integer) MS_RenameDir(OldName, NewName, ResultBuffer));

    if (type == SNAP_SENDWITHREPLY) {
	register int result, length;

	/* Pack return args */
	ptr = putstr(ReturnPacketArgs(reply), ResultBuffer);
	length = ptr - reply;

	(void) putint(ReturnPacketLength(reply), (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	/*
	 fprintf(stderr, "[MS]: Ready to reply, length = %d\n", length);
	 */
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
    }
}

static void Execute(buffer, len, type, cid)
char *buffer;
int len, type, cid;
{
    static char name[] = "Execute";
    register void (*proc)();
    register int i;
    SNAP_integer opcode;

    if (MSDebugging & 32) { /* Debugging SHOULD go to stdout -- nsb 5/16/86 */
	fputs("[MS]: Execute(0x", stdout);
	for (i=0; i<len; i++) fprintf(stdout, "%02x", (unsigned char) buffer[i]);
	fputs(")\n", stdout);
    }

    /* See if this is endconversation packet */
    if (type == SNAP_ENDCONV) {
	EndConversation(buffer, len, cid);
	return;
    }

    /* Check length of packet */
    if (CallLengthError(buffer)) {
	ReportAndSendCallError(name, buffer, RPC_BAD_CALL_LENGTH_2, cid);
	return;
    }
    /* Check to see if I had a fatal initialization error */

    if (IWantToDie) {
	int result, length;

	(void) putint(ReturnPacketReturnValue(reply),
		      (SNAP_integer) IWantToDie);
	length = RETURN_PACKET_HEADER_LENGTH;
	(void) putint(ReturnPacketLength(reply),
		      (SNAP_integer) length);
	(void) putint(ReturnPacketError(reply), (SNAP_integer) RPC_OK);
	result = SNAP_Reply(reply, length, cid);
	if (result < 0) ReplyFailure(name, result);
	safeexit(-1);
    }

    /* Extract op code & call stand-in routine */
    (void) getint(CallPacketOpCode(buffer), &opcode);
    switch ((int) opcode) {
	case OP_MS_CREATE_NEW_MESSAG:
	    proc = ms_create_new_message_directory;
	    break;
	case OP_MS_APPENDFILEFOLDER:
	    proc = ms_append_file_to_folder;
	    break;
	case OLDOP_MS_PROCESS_NEW_MESSA:  /* Changed, by bobg (26-Oct-88) */
	case OP_MS_PROCESS_NEW_MESSA:
	    proc = ms_process_new_messages;
	    break;
	case OP_MS_HEADERS_SINCE:
	    proc = ms_headers_since;
	    break;
	case OP_MS_GET_PARTIAL_FILE:
	    proc = ms_get_partial_file;
	    break;
	case OP_MS_GET_PARTIAL_BODY:
	    proc = ms_get_partial_body;
	    break;
	case OP_MS_SET_ASSOCIATED_TI:
	    proc = ms_set_associated_time;
	    break;
	case OP_MS_GET_ASSOCIATED_TI:
	    proc = ms_get_associated_time;
	    break;
	case OP_OLD_MS_PRINT_MESSAGE:
	    proc = old_ms_print_message;
	    break;
	case OP_MS_PRINT_MESSAGE:
	    proc = ms_print_message;
	    break;
	case OP_MS_GET_VERSION:
	    proc = ms_get_version;
	    break;
	case OP_MS_DEBUG_MODE:
	    proc = ms_debug_mode;
	    break;
	case OP_MS_DISAMBIGUATE_FILE:
	    proc = ms_disambiguate_file;
	    break;
	case OP_MS_GET_SEARCH_PATH_E:
	    proc = ms_get_search_path_entry;
	    break;
	case OP_MS_NAME_SUBSCRIPTION:
	    proc = ms_name_subscription_map_file;
	    break;
	case OP_MS_SET_SUBSCRIPTION_:
	    proc = ms_set_subscription_entry;
	    break;
	case OP_MS_GET_SUBSCRIPTION_:
	    proc = ms_get_subscription_entry;
	    break;
	case OP_MS_DIE:
	    proc = ms_die;
	    break;
	case OP_MS_OPEN_DEBUGGING_PI:
	    proc = ms_open_debugging_pipescript;
	    break;
	case OP_MS_REBUILD_SUBSCRIPT:
	    proc = ms_rebuild_subscription_maps;
	    break;
	case OP_MS_NAME_REPLY_FILE:
	    proc = ms_name_reply_file;
	    break;
	case OP_MS_EPOCH:
	    proc = ms_epoch;
	    break;
	case OP_MS_PURGE_DELETED_MES:
	    proc = ms_purge_deleted_messages;
	    break;
	case OP_MS_ALTER_SNAPSHOT:
	    proc = ms_alter_snapshot;
	    break;
	case OP_MS_GET_SNAPSHOT:
	    proc = ms_get_snapshot;
	    break;
	case OP_MS_GET_HEADER_CONTEN:
	    proc = ms_get_header_contents;
	    break;
	case OP_MS_SUBMIT_MESSAGE:
	case OLDOP_SUBMIT_MESSAGE:
	    proc = ms_submit_message;
	    break;
	case OP_MS_UPDATE_STATE:
	    proc = ms_update_state;
	    break;
	case OP_MS_FAST_UPDATE:
	    proc = ms_fast_update_state;
	    break;
	case OP_MS_STORE_PARTIAL_FIL:
	    proc = ms_store_partial_file;
	    break;
	case OP_MS_INSTALL_WELCOME_M:
	    proc = ms_install_welcome_message;
	    break;
	case OP_MS_REINITIALIZE:
	    proc = ms_reinitialize;
	    break;
	case OLDOP_REWRITEHEADERLINE:
	    proc = ms_rewrite_header_line;
	    break;
	case OP_MS_RECONSTRUCTDIRECT:
	    proc = ms_reconstruct_directory;
	    break;
	case OP_MS_FIND_MAILBOX:
	    proc = ms_find_mailbox;
	    break;
	case OP_MS_CLONE_MESSAGE:
	    proc = ms_clone_message;
	    break;
	case OP_MS_MERGE_DIRECTORIES:
	    proc = ms_merge_directories;
	    break;
	case OP_MS_GETDIRINFO:
	    proc = ms_getdirinfo;
	    break;
	case OP_MS_REMOVE_DIRECTORY:
	    proc = ms_remove_directory;
	    break;
	case OP_MS_UNLINK_FILE:
	    proc = ms_unlink_file;
	    break;
	case OP_MS_EDIT_MESSAGE:
	    proc = ms_edit_message;
	    break;
	case OP_MS_GET_NEXT_SUBSENT:
	    proc = ms_get_next_subs_entry;
	    break;
	case OP_MS_CONVERT_OLD_MAIL:
	    proc = ms_convert_old_mail;
	    break;
	case OP_MS_RENAME_DIR:
	    proc = ms_rename_dir;
	    break;
	case OP_MS_GET_NTH_SNAPSHOT:
	    proc = ms_get_nth_snapshot;
	    break;
	case OP_MS_GET_NEW_MSG_CT:
	    proc = ms_get_new_msg_ct;
	    break;
	case OP_MS_PREFETCH_MSG:
	    proc = ms_prefetch_message;
	    break;
	case OP_MS_CHECK_MISSING:
	    proc = ms_check_missing_folder;
	    break;
	case OP_MS_REBUILDMASTERUP_OLD:
	    proc = ms_rebuild_master_update_old;
	    break;
	case OP_MS_NAMECHANGEDMAP:
	    proc = ms_name_changed_map;
	    break;
	case OP_MS_WRITE_ALL_MATCHES:
	    proc = ms_write_all_matches_to_file;
	    break;
	case OP_MS_VALIDATE_CHUNK:
	    proc =  ms_validate_chunk;
	    break;
	case OP_MS_WRITE_UNSCRIBED:
	    proc =  ms_write_unscribed;
	    break;
	case OP_MS_DO_I_HAVE_MAIL:
	    proc =  ms_do_i_have_mail;
	    break;
	case OP_MS_SET_DEATHKNELL:
	    proc =  ms_set_deathknell;
	    break;
	case OP_MS_PARSE_DATE:
	    proc =  ms_parse_date;
	    break;
	case OP_MS_HANDLE_PREFERENCE:
	    proc = ms_handle_preference;
	    break;
	case OP_MS_CHECKAUTHENTICATION:
	    proc = ms_check_authentication;
	    break;
	case OP_MS_TAKEHINTS:
	    proc = ms_takehints;
	    break;
	case OP_MS_GETDIRATTRS:
	    proc = ms_getdirattributes;
	    break;
	case OP_MS_DELETEATTR:
	    proc = ms_deleteattr;
	    break;
	case OP_MS_ADDATTR:
	    proc = ms_addattr;
	    break;
	case OP_MS_REBUILD_ONE:
	    proc = ms_rebuild_one_subscription_map;
	    break;
	case OP_MS_REINDEX_ONE_OLD:
	    proc = ms_rebuild_one_master_update_file_old;
	    break;
	case OP_MS_GEN_TEMP_NAME:
	    proc = ms_gen_temp_file_name;
	    break;
	case OP_MS_REBUILDMASTERUP:
	    proc = ms_rebuild_master_update;
	    break;
	case OP_MS_REINDEX_ONE:
	    proc = ms_rebuild_one_master_update_file;
	    break;
	case OP_MS_GETCONFIGPARMS:
	    proc = ms_get_config_parms;
	    break;
	case OP_MS_MATCH_FOLDER_NAME:
	    proc = ms_match_folder_name;
	    break;
	case OP_MS_GET_VERSION_CONFIG:
	    proc = ms_get_vconfig;
	    break;
	case OP_MS_SCAVENGE:
	    proc = ms_scavenge_directory;
	    break;
	case OP_MS_DOMAIN_FORMAT:
	    proc = ms_domain_handles_formatting;
	    break;
	default:
	    {
	    char ErrorText[256];

	    sprintf(ErrorText, "Unrecognized operation code %d -- one RPC call aborted", opcode);
	    NonfatalBizarreError(ErrorText);
	    ReportAndSendCallError(name, buffer, RPC_BAD_OPCODE, cid);
	    return;
	    }
    }
    (*proc)(buffer, len, type, cid, opcode);
}

EndConversation(buffer, len, cid)
char *buffer;
int len, cid;
{
    /* Nothing to do yet */
}

Machine_HandleClientSignal() {} /* In the messageserver process, this is a no-op */
