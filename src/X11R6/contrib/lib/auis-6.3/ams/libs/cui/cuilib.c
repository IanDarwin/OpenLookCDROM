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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/cui/RCS/cuilib.c,v 2.64 1994/03/31 07:06:50 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/time.h sys/file.h */
#include <cui.h>
#include <errprntf.h>
#include <stdio.h>
#include <pwd.h>
#include <sys/param.h>
#include <ctype.h>
#include <cuimach.h>
#include <hdrparse.h>
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <sys/ioctl.h>
#include <afs/errors.h>
#include <afs/prs_fs.h>
#include <afs/venus.h>
#endif /* AFS_ENV */
#ifdef hpux
#include <sys/utsname.h>
#endif /* hpux */

#define HEADBUFSIZE (CUIMACH_GULPSIZE * AMS_SNAPSHOTSIZE)
 /* Size of header buffer for each SNAP transaction */
 /* Ought to remain a multiple of AMS_SNAPSHOTSIZE */

char *CUI_WhoIAm = NULL;
static char *MyPassword = NULL;
static int MyPasswordLen, MyPasswordType;
char *CUI_ClientVersion = (char *)NULL, *CUI_MachineName = (char *)NULL, *CUI_MachineType = (char *)NULL, *CUI_DeliveryInfoString = (char *)NULL;
char CUI_MailDomain[125] = "";
long CUI_UseAmsDelivery=0, CUI_UseNameSep=0, CUI_DeliveryType = -1;

/* Any new message server functions should be added to this list. */
extern long MS_ReInitialize(), MS_GetVersion(), MS_Die(), MS_GetConfigurationParameters(), MS_CreateNewMessageDirectory(), MS_FindMailbox(), MS_DisambiguateFile(), MS_ProcessNewMessages(), MS_HeadersSince(), MS_GetPartialBody(), MS_PrintMessage(), MS_NameReplyFile(), MS_AlterSnapshot(), MS_PurgeDeletedMessages(), MS_PurgeDeletedMessages(), MS_GetSnapshot(),
   MS_GetHeaderContents(), MS_SubmitMessage(), MS_UnlinkFile(), MS_ReconstructDirectory(), MS_ValidateAndReplaceChunk(), MS_GetPartialFile(), MS_WriteAllMatchesToFile(), MS_InstallWelcomeMessage(), MS_CloneMessage(), MS_GetAssociatedTime(), MS_SetAssociatedTime(), MS_StorePartialFile(), MS_RenameDir(), MS_GetDirInfo(), MS_RemoveDirectory(),
   MS_CheckMissingFolder(), MS_GetSubscriptionEntry(), MS_PrefetchMessage(), MS_HandlePreference(), MS_SetSubscriptionEntry(), MS_MergeDirectories(), MS_StorePartialFile(), MS_GetDirAttributes(), MS_AddAttribute(),MS_GetVConfig();

#if !POSIX_ENV
extern char *malloc (), *realloc ();
extern char *index (), *rindex();
#define strchr(s,c) index(s,c)
#define strrchr(s,c) rindex(s,c)
#endif

extern char *NextAddress(), *cvEng();
extern char *SnapVersionString;

/* Any CUI functions that return a long should be in this list */
long HandleAddress(), CUI_CacheDirName(), CUI_SetDirNode(), CUI_GetDirNode(), CUI_AlterSnapshot(), CUI_GetHeaders(), CUI_DisambiguateDir(), CUI_SetSubscriptionEntry(), CUI_MergeDirectories();

int (*CUI_GenericClientSignalHandler)() = NULL;

CUI_SetClientSignalHandler(h)
int (*h)();
{
    CUI_GenericClientSignalHandler = h;
}

void CUI_SetupDeliveryString() {
    if (CUI_DeliveryInfoString) free(CUI_DeliveryInfoString);
    CUI_DeliveryInfoString = malloc(4+
        (CUI_ClientVersion ? strlen(CUI_ClientVersion) : 7) + 
        (CUI_MachineName ? strlen(CUI_MachineName) : 7) + 
        (CUI_MachineType ? strlen(CUI_MachineType) : 7));
    sprintf(CUI_DeliveryInfoString, "%s.%s.%s", 
        (CUI_ClientVersion ? CUI_ClientVersion : "unknown"), 
        (CUI_MachineName ? CUI_MachineName : "unknown"), 
        (CUI_MachineType ? CUI_MachineType : "unknown"));
}
    
void
CUI_SetClientVersion(Vers)
char *Vers;
{
    if (CUI_ClientVersion) free (CUI_ClientVersion);
    CUI_ClientVersion = malloc(strlen(Vers) + 25 + (SnapVersionString?strlen(SnapVersionString):25));
    sprintf(CUI_ClientVersion, "%s.CUILIB.%d.%d.SNAP.%s", Vers, CUI_MAJORVERSION, CUI_MINORVERSION, SnapVersionString?SnapVersionString:"No.Snap.version.number.");
}

void
CUI_SetMachineName(s)
char *s;
{
    if (CUI_MachineName) free (CUI_MachineName);
    CUI_MachineName = malloc(strlen(s)+1);
    strcpy(CUI_MachineName, s);
}

void
CUI_SetMachineType(s)
char *s;
{
    int newLen;
#ifdef hpux
    struct utsname name;
    int callRes;
#endif /* hpux */

    if (CUI_MachineType) free (CUI_MachineType);
#ifdef hpux
    /* Code comes from HP, via ghoti 5/11/89 then cfe 5/16/89 */
    callRes = uname(&name);
    if (callRes == -1) {
	newLen = strlen(s);
    } else {
	newLen = 2 + strlen(name.machine);
    }
#else /* hpux */
    newLen = strlen(s);
#endif /* hpux */
    CUI_MachineType = malloc(newLen+1);
    if (CUI_MachineType != NULL) {
#ifdef hpux
	if (callRes == -1) {
	    strcpy(CUI_MachineType, s);
	} else {
	    strcpy(CUI_MachineType, "HP");
	    strcat(CUI_MachineType, name.machine);
	}
#else /* hpux */
	strcpy(CUI_MachineType, s);
#endif /* hpux */
    }
}

Boolean CUI_IsMagic = TRUE;

extern long gtime ();
extern char *StripWhiteEnds ();
extern int  Interactive;

int	CUIDebugging = 0;

struct cuidnode {
    int     index;
    struct cuidnode *next;
};

#define CUIDHASHMAX 512 	/* Should remain a power of 2 */
static struct cuidnode	AIDHash[CUIDHASHMAX];

#define CUIDIRHASHSIZE 64 /* Power of 2 */
struct CUIDirNode {
    char    *NickName;
    char    *FullName;
    unsigned FullNameHash;
    struct CUIDirNode  *Next;
    int NeedsPurging;
    unsigned int WasDeleted:1;
};
static struct CUIDirNode *CUIDirCache[CUIDIRHASHSIZE];

int CUI_CuidsInUse = 0;

#define CUIDALLOCATIONCHUNK 300
static int  NumCuidsAllocated = 0;

struct CuidCacheEnt {
    char    Id[AMS_IDSIZE];
    char   *Dir;
} *CuidCache = (struct CuidCacheEnt *)NULL;

extern int     CUI_SnapIsRunning;

#define MAXBUFFERSIZE 20000
#define MAXVERSTRING 50
char	CUI_VersionString[MAXVERSTRING] = "";

extern int  CUI_OnSameHost;
char *CUI_Rock = (char *)NULL;
static char *QVnodrop[] = {"Cross-cell configuration will prevent you from sending any mail in this session.", "Continue Anyway (just reading messages)", "Quit", NULL};
static char *QVlater[] = {"Cross-cell configuration will delay delivery of mail you send in this session for hours.", "Continue Anyway", "Quit", NULL};

long CUI_GetVersConfigString(key,dest)
char *key;
char *dest;
{return MS_GetVConfig(key,CUI_ClientVersion,dest);
}


int CUI_CheckVersion()
{ char result_buf[MAXPATHLEN];
  char *rmes;

  mserrcode = CUI_GetVersConfigString("expire",result_buf);
  /* if eval call failed, allow user to go ahead */
  if (mserrcode) return (mserrcode=0);
  rmes=strchr(result_buf,',');
  if(rmes==0)return(mserrcode=0);
  /*seperate the result code from comment message*/
  *rmes++ = 0;
  mserrcode = atoi(result_buf);
  /*if eval call succeded but told us to fail, do so*/
  if (mserrcode)
    ReportError(rmes, ERR_CRITICAL, TRUE);
   /*else, if not punting perhaps there is some comentary*/
  else if (*rmes == 0)
      return 0;
  /*not an error but we have something to say*/
  ReportSuccess(rmes);
  return -1; /*say we printed, may want to confirm to continue*/
}

/*
  do a MS_ReInitilize call.  print fancer error messages if it fails
*/
long cui_ms_reinitilize_with_fancy_error_messages()
{Boolean Decode = TRUE;
 char ErrorText[256];
 mserrcode = MS_ReInitialize();
 if(mserrcode==0)return mserrcode;
 strcpy(ErrorText, "Initialization failed; program terminated.");
 if (AMS_ERRCAUSE == EIN_MSPATHCHECK) {
   switch(AMS_ERRNO) {
    case ENOENT:
     strcpy(ErrorText, "An element of your mspath does not exist.  Please check and fix your mspath preference.");
     Decode = FALSE;
     break;
  case EACCES:
  case EMSUNAUTH:
    strcpy(ErrorText, "Unreadable mspath element; if you're authenticated, please check & fix your mspath preference.");
    Decode = FALSE;
    break;
  case EMSNOSUCHVAR:
    strcpy(ErrorText, "Your mspath preference refers to an unknown variable.  Please check & fix it.");
    Decode = FALSE;
    break;
  default:
    break;
   }
 }
 ReportError(ErrorText, ERR_CRITICAL, Decode);
 return(-1);
}

long CUI_Initialize(TimerFunction, rock)
int (*TimerFunction)();
char *rock;
{
    static int	HasInitialized = FALSE;
    char    *ThisHost;

    if (rock) CUI_Rock = rock;
    if (HasInitialized)
	return(0);
    debug(1,("Initializing CUI\n"));
    HasInitialized = TRUE;

    CheckEmsgConsistency();

    bzero(AIDHash, CUIDHASHMAX * sizeof(struct cuidnode));
    if (NumCuidsAllocated == 0) {
	NumCuidsAllocated = CUIDALLOCATIONCHUNK;
	CuidCache = (struct CuidCacheEnt   *) malloc (sizeof (struct CuidCacheEnt) * NumCuidsAllocated);
	bzero(CuidCache, NumCuidsAllocated * sizeof(struct CuidCacheEnt));
    }

    if (Machine_Init(&ThisHost, &CUI_WhoIAm, &MyPassword, &MyPasswordLen, &MyPasswordType, 0)) {
	ReportError("Machine initialization problem", ERR_CRITICAL, TRUE);
	return(-1);
    }

    if (MS_CUI_Init(ThisHost, CUI_WhoIAm, MyPassword, MyPasswordLen, MyPasswordType, MAXBUFFERSIZE)) {
	ReportError("Could not initialize connection to messageserver.", ERR_CRITICAL, TRUE);
	return(-1);
    }

    if (!CUI_OnSameHost) {
	extern char *ReconHost, ProgramName[];

	if (ReconHost!=NULL)
	     errprintf(ProgramName, ERR_WARNING, NIL, NIL, "Talking to message server on remote host %s", ReconHost);
    }

    if (TimerFunction) {
	(*TimerFunction)(CUI_Rock);
    } else {
	CUI_InitializeKeepalives();
    }

    sprintf(CUI_VersionString, "CUI %d.%d (AMS %d.%d, SNAP %s)",
	    CUI_MAJORVERSION, CUI_MINORVERSION, AMS_MAJOR_VERSION, AMS_MINOR_VERSION,
	    SnapVersionString);
    if (CUI_SnapIsRunning &&
      (cui_ms_reinitilize_with_fancy_error_messages()!=0))
	return -1;
    mserrcode = MS_GetConfigurationParameters(CUI_MailDomain, sizeof(CUI_MailDomain), &CUI_UseAmsDelivery, &CUI_UseNameSep, &CUI_DeliveryType);
    if (mserrcode) {
	ReportError("Could not get configuration parameters.", ERR_CRITICAL, TRUE);
	return(-1);
    }
    if (CUI_DeliveryType == 1) {	 /* DT_CANNOT in dropoff.h */
	int i;
	i = ChooseFromList(QVnodrop, 2);
	if (i == 2) exit(0);
    }
    if (CUI_DeliveryType == 3) {	/* DT_AMSWAIT in dropoff.h */
	int i;
	i = ChooseFromList(QVlater, 2);
	if (i == 2) exit(0);
    }
    return(0);
  }

CheckEmsgConsistency()
{
    char ErrorText[256], *s;
    extern int	ms_nerr,
#ifndef SMALL_MEMORY_MODEL
		ms_nerrcause,
		ms_nerrvia,
#endif /* SMALL_MEMORY_MODEL */
		rpc_nerr;

    s = NULL;
    if (EMSLASTERROR - EMSBASE + 1 != ms_nerr) {
	s = "number";
    }
#ifndef SMALL_MEMORY_MODEL
    if (EIN_LASTERRORLOCATION + 1 != ms_nerrcause) {
	s = "cause";
    }
    if (EVIA_LASTERROR + 1 != ms_nerrvia) {
	s = "location";
    }
#endif /* SMALL_MEMORY_MODEL */
    if (RPC_LASTERROR + 1 != rpc_nerr) {
	s = "RPC";
    }
    if (s) {
	sprintf(ErrorText, "Inconsistent error %s codes for RPC -- amserr.o miscompiled?", s);
	ReportError(ErrorText, ERR_WARNING, FALSE);
    }
}

CUI_CreateNewMessageDirectory(dir, bodydir)
char   *dir,
       *bodydir; /* Ignored 4/22/88 */
{
    char    ErrorText[256], *slash;

    debug(1,("Creating new message directory: %s\n", dir));
    dir = StripWhiteEnds(dir);
    slash = strrchr(dir, '/');
    if (!slash) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CREATENEWMSDIRECTORY);
    }
    ++slash;
    if ((*slash == '+') || strchr(slash, ' ') || strchr(slash, '.')) {
	sprintf(ErrorText, "Invalid message folder name: %s", ap_Shorten(dir));
	ReportSuccess(ErrorText);
	return(-1);
    }
    if ((mserrcode = MS_CreateNewMessageDirectory(dir, FALSE, dir))) {
	if (AMS_ERRNO == EMSWOULDOVERWRITE) {
	    ReportSuccess("That folder already appears to exist!");
	} else {
	    sprintf(ErrorText, "Cannot create new message folder %s", ap_Shorten(dir));
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    DirectoryChangeHook(dir, (char *)NULL, CUI_Rock);
    sprintf(ErrorText, "Created empty message folder %s", ap_Shorten(dir));
    ReportSuccess(ErrorText);
    return(0);
}

CUI_CheckMailboxes(ForWhat)
char   *ForWhat;
{
    int     i = 0;
    char    BoxName[1 + MAXPATHLEN],
	    ErrorText[256];

    debug(1,("CUI_CheckMailboxes %s\n", ForWhat ? ForWhat : "<ALL>"));
    for (;; ++i) {
	mserrcode = MS_FindMailbox(i, BoxName);
	if (mserrcode) {
	    switch (AMS_ERRNO) {
		case EINVAL:
		    return(0);	/* done */
		case ENOENT:
		case EACCES:
		    break;	/* Ignore it */
		default:
		    sprintf(ErrorText, "Cannot check mailbox %s", ap_Shorten(BoxName));
		    ReportError(ErrorText, ERR_WARNING, TRUE);
		    return(-1);
	    }
	}
	else {
	    char *slash = strrchr(BoxName, '/');
	    if (slash) *slash = '\0';
	    if (ForWhat && *ForWhat && strncmp(ForWhat, BoxName, strlen(BoxName))) {
		continue;
	    }
	    if (slash) *slash = '/';
	    CUI_CheckNewMessages(BoxName);
	}
    }
}

CUI_CheckNewMessages(arg)
char   *arg;
{
    int     Good = 0,
	    Bad = 0,
	    Locks = 0,
	    InProgress = 0,
	    status;
    long    FirstError;
    char   *cleanarg,
	    ErrorText[256 + ELI_ERROR_TEXT_BUFLEN],
	   *secondarg,
	    Mailbox[MAXPATHLEN + 1],
            SpecFile[MAXPATHLEN + 1],
            EliErrorText[ELI_ERROR_TEXT_BUFLEN + 1];

    debug(1,("Entering CheckNewMessages\n"));
    cleanarg = StripWhiteEnds(arg);
    SpecFile[0] = '\0';
    if (secondarg = strchr(cleanarg, ' ')) { /* should be more flexible in parsing */
	*secondarg++ = '\0';
	secondarg = StripWhiteEnds(secondarg);
	if ((mserrcode = MS_DisambiguateFile(secondarg, SpecFile, AMS_DISAMB_EXISTS)) != 0) {
	    CUI_ReportAmbig(secondarg, "file");
	    return(-1);
	}
    }
    if (*cleanarg == '\0') cleanarg = "~/Mailbox";
    if ((mserrcode = MS_DisambiguateFile(cleanarg, Mailbox, AMS_DISAMB_EXISTS)) != 0) {
	CUI_ReportAmbig(cleanarg, "directory");
	return(-1);
    }
    if (Interactive) {
	sprintf(ErrorText, "Looking for new mail in %s", ap_Shorten(Mailbox));
	ReportSuccess(ErrorText);
    } 
    mserrcode = MS_ProcessNewMessages(Mailbox, &Good, &Bad, &Locks, SpecFile, &status, &FirstError, &InProgress, EliErrorText, ELI_ERROR_TEXT_BUFLEN);
    if (status || mserrcode) {
	switch (status) {
	    case 1:
		sprintf(ErrorText, "Some messages could not be deleted from %s after being read", Mailbox);
		break;
	    case 2:
		sprintf(ErrorText, "Messages in %s could not be properly read and delivered (%d successes, %d failures (%d locked, %d in progress))", Mailbox, Good, Bad, Locks, InProgress);
		break;
	    case 3:
		sprintf(ErrorText, "Messages in %s could not be properly read and delivered (%d successes, %d failures (%d locked, %d in progress), some cleanup failures)", Mailbox, Good, Bad, Locks, InProgress);
		break;
	    default:
		sprintf(ErrorText, "Messages in %s could not be properly read and delivered (%d successes, %d failures (%d locked, %d in progress))", Mailbox, Good, Bad, Locks, InProgress);
		break;
        }
        if (*EliErrorText) {
            strcat(ErrorText, "   ");
            strcat(ErrorText, EliErrorText);
        }
	if ((Locks + InProgress > 0) && (Bad == (Locks + InProgress))) {
	    if (Good) {
		sprintf(ErrorText, "%d message%s read in from %s.", Good, Good > 1 ? "s were" : " was", ap_Shorten(Mailbox));
		if (Interactive) {
		    ReportSuccess(ErrorText);
		} else {
		    ReportError(ErrorText, ERR_WARNING, FALSE);
		}
	    }
	    if (Locks) {
		sprintf(ErrorText, "%d messages in %s were temporarily locked; try again in a moment.", Locks, ap_Shorten(Mailbox));
		ReportSuccess(ErrorText);
	    }
	    if (InProgress) {
		sprintf(ErrorText, "%d messages in %s were deliveries in progress; try again in a moment.", InProgress, ap_Shorten(Mailbox));
		ReportSuccess(ErrorText);
	    }
	    return(0);
	}
	if (mserrcode) {
	    ReportError(ErrorText, ERR_CRITICAL, !(*EliErrorText));
	} else if (FirstError) {
	    mserrcode = FirstError;
	    ReportError(ErrorText, ERR_CRITICAL, !(*EliErrorText));
	}
	return(mserrcode ? -1 : 0);
    }
    if (Bad) {
	sprintf(ErrorText, "%d messages could not be properly parsed from %s.", Bad, ap_Shorten(Mailbox));
	ReportError(ErrorText, ERR_CRITICAL, !(*EliErrorText));
    }
    if (Good) {
	if (Good != 1) {
	    sprintf(ErrorText, "%d messages were read in from %s.", Good, ap_Shorten(Mailbox));
	}
	else {
	    sprintf(ErrorText, "1 message was read in from %s.", ap_Shorten(Mailbox));
	}
	if (Interactive) {
	    ReportSuccess(ErrorText);
	} else {
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	}
    }
    if (!Good && !Bad) {
	if (Interactive) {
	    sprintf(ErrorText, "No new messages in %s", ap_Shorten(Mailbox));
	    ReportSuccess(ErrorText);
	}
    }
    return(0);
}

long
CUI_GetHeaders(DirName, date64, headbuf, limit, startbyte, nbytes, status, RegisterCuids)
char   *DirName,
       *date64,
       *headbuf;
int	limit, RegisterCuids;
long	startbyte,
	*nbytes,
	*status;			/* ***	 Added 8/19 for PC  *** */
{
    char   *s;
    int IsDup;

    debug(1,("Entering CUI_GetHeaders dir %s date %s limit %d startbyte %ld\n",
		DirName, date64, limit, startbyte));
    mserrcode = MS_HeadersSince(DirName, date64, headbuf, limit, startbyte, nbytes, status);
    debug(4,("Picked up %ld bytes status %ld\n", *nbytes, *status));
    for (s = headbuf; s - headbuf < *nbytes; s += AMS_SNAPSHOTSIZE) {
	if (RegisterCuids) GetCuid(AMS_ID(s), DirName, &IsDup);
	if (AMS_GET_ATTRIBUTE(s, AMS_ATT_MAYMODIFY) && AMS_GET_ATTRIBUTE(s, AMS_ATT_DELETED)) {
	    MarkDirectoryForPurging(DirName);
	}
    }
    return(mserrcode);
}


/* The following function associates a CUID (a temporary ID used only within
	a CUI session, for user convenience) with a real AMS ID.  The
	association must be consistent, that is, we must return the old cuid
	if we've seen this AMS ID before.
 */


HashAmsID(id)
char   *id;
{
    int     i,
	    total = 0;

    debug(1,("HashAmsID %s\n", id));
    for (i = 0; i < AMS_IDSIZE; ++i) {
	total += (int) id[i];
    }
    return(total & (CUIDHASHMAX - 1));
}

CUI_GetCuid(amsid, dirname, IsDup)
char   *amsid,
       *dirname;
int *IsDup;
{
    static int hashconflicts = 0;
    int     hashval;
    struct cuidnode *ctmp;
    struct CUIDirNode  *DNtmp;

    debug(1,("GetCUID %s %s %d", amsid, dirname, IsDup));
    *IsDup = 0;
    hashval = HashAmsID(amsid);
    debug(4,("Hashed to %d\n", hashval));
    ctmp = &AIDHash[hashval];
    if (ctmp->index > 0) {
	if (!strncmp(CuidCache[ctmp->index].Id, amsid, AMS_IDSIZE)) {
	    if (!strcmp(dirname, CuidCache[ctmp->index].Dir)) {
		debug(4,("Already had this value, returning old index\n"));
		return(ctmp->index);
	    }
	    *IsDup = 1;
	}
	while (ctmp->next != NULL) {
	    debug(32,("Ignoring a hash conflict (%d conflicts, %d cuids in use)\n", ++hashconflicts, CUI_CuidsInUse));
	    ctmp = ctmp->next;
	    if (!strncmp(CuidCache[ctmp->index].Id, amsid, AMS_IDSIZE)) {
		if (!strcmp(dirname, CuidCache[ctmp->index].Dir)) {
		    debug(4,("Already had this value, returning old index\n"));
		    return(ctmp->index);
		}
		*IsDup = 1;
	    }
	}
	debug(4,("Allocating a new node\n"));
	ctmp->next = (struct cuidnode  *) malloc (sizeof (struct cuidnode));
	ctmp = ctmp->next;
	ctmp->next = NULL;;
    }
    debug(4,("Adding new hash entry # %d\n", CUI_CuidsInUse));
    if (++CUI_CuidsInUse >= NumCuidsAllocated) {
	debug(4,("Allocating room for more cuids, sigh...\n"));
	NumCuidsAllocated *= 2;
	CuidCache = (struct CuidCacheEnt   *) realloc (CuidCache, sizeof (struct CuidCacheEnt) * NumCuidsAllocated);
    }
    strcpy(CuidCache[CUI_CuidsInUse].Id, amsid);
    ctmp->index = CUI_CuidsInUse;

    CUI_GetDirNode(dirname, &DNtmp);
    CuidCache[CUI_CuidsInUse].Dir = DNtmp->FullName;
    return(ctmp->index);
}

CUI_GetAMSID(cuid, id, dir)
int	cuid;
char  **id,
      **dir;
{
    debug(1,("GetAMSID %d\n", cuid));
    if (cuid > CUI_CuidsInUse || cuid <= 0)
	return(-1);
    *id = CuidCache[cuid].Id;
    *dir = CuidCache[cuid].Dir;
    return(0);
}

CUI_GetPartialBody(Buf, Max, cuid, offset, bytesunfetched, bodylen)
char   *Buf;
int	Max,
	cuid;
long	offset,
	*bytesunfetched;		/* *** Added for PC  8/20/86  *** */
int	*bodylen;
{
    char   *id,
	   *dir,
	    ErrorText[256];

    debug(1,("Looking for body of message %d\n", cuid));
    *bodylen = 0;
    *bytesunfetched = 0;
    if (cuid <= 0 || GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if ((mserrcode = MS_GetPartialBody(dir, id, Buf, Max, offset, bytesunfetched, bodylen)) != 0) {
	if (AMS_ERRNO == ENOENT) {
	    ReportSuccess("This message appears to have been recently deleted and purged.");
	}
	else {
	    sprintf(ErrorText, "Unexpected error: cannot read the body of message %d", cuid);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    return(0);
}

static char *CUI_PrinterName = NULL;

CUI_SetPrinter(printername)
char *printername;
{
    char ErrorText[256];

    mserrcode = MS_PrintMessage((char *)NULL, (char *)NULL, 0, printername);
    if (AMS_ERRNO == EMSBADPRINTER) {
	sprintf(ErrorText, "Sorry; there is no such printer as '%s'.", printername);
	ReportSuccess(ErrorText);
	return(-1);
    } else if (AMS_ERRNO != EINVAL) {
	sprintf(ErrorText, "Error: could not set printer", printername);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    if (CUI_PrinterName) free(CUI_PrinterName);
    if (printername) {
	CUI_PrinterName = malloc(1+strlen(printername));
	if (!CUI_PrinterName) {
	    ReportError("Out of memory!", ERR_CRITICAL, FALSE);
	    return(-1);
	}
	strcpy(CUI_PrinterName, printername);
    } else {
	CUI_PrinterName = NULL;
    }
    strcpy(ErrorText, "Future prints will be sent to ");
    if (CUI_PrinterName) {
	strcat(ErrorText, "printer '");
	strcat(ErrorText, CUI_PrinterName);
	strcat(ErrorText, "'.");
    } else {
	strcat(ErrorText, "your default printer.");
    }
    ReportSuccess(ErrorText);
    return(0);
}

CUI_PrintBodyFromCUID(cuid)
int	cuid;
{
    return(CUI_PrintBodyFromCUIDWithFlags(cuid, 0, (char *)NULL));
}

CUI_PrintBodyFromCUIDWithFlags(cuid, flags, printer)
int cuid;
int flags;
char *printer;
{
    char   *id,
	   *dir,
	    ErrorText[256];

    debug(1,("Trying to print message %d\n", cuid));
    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if ((mserrcode = MS_PrintMessage(dir, id, flags, printer ? printer : CUI_PrinterName)) != 0) {
	if (AMS_ERRNO == EWOULDBLOCK) {
	    ReportError("Printing is temporarily locked; try again in a moment.", ERR_WARNING, FALSE);
	} else {
	    ReportError("Could not print the message; sorry", ERR_WARNING, TRUE);
	}
	return(-1);
    }
    ReportSuccess("Message queued for printing as requested");
    return(0);
}

CUI_NameReplyFile(cuid, code, FileName)
int	cuid,
	code;
char   *FileName;
{
    char   *id,
	   *dir;

    debug(1,("Trying to construct reply to message %d\n", cuid));
    if (cuid <= 0 || GetAMSID(cuid, &id, &dir) != 0) {
	dir = id = "";
	code = AMS_REPLY_FRESH;
    }
    if ((mserrcode = MS_NameReplyFile(dir, id, code, FileName)) != 0) {
	ReportError("Message server could not build reply file", ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

long
CUI_AlterSnapshot(cuid, NewSnapshot, Code, dir)
int	cuid,
	Code;
char   *NewSnapshot,
      **dir;
{
    char   *id,
	    ErrorText[256];

    debug(1,("Altering snapshot of message %d\n", cuid));
    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (GetAMSID(cuid, &id, dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    return(mserrcode = MS_AlterSnapshot(*dir, id, NewSnapshot, Code));
}

CUI_DeleteMessage(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    bzero(SnapshotBuf, AMS_SNAPSHOTSIZE);
    AMS_SET_ATTRIBUTE(SnapshotBuf, AMS_ATT_DELETED);
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_OR_ATTRIBUTES, &dir)) {
	if (AMS_ERRNO == EMSNOSUCHMESSAGE) {
	    ReportSuccess("This message has already been deleted AND purged.");
	} else {
	    sprintf(ErrorText, "Could not delete message %d", cuid);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    return(MarkDirectoryForPurging(dir));
}

CUI_UndeleteMessage(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    bone(SnapshotBuf, AMS_SNAPSHOTSIZE);
    AMS_UNSET_ATTRIBUTE(SnapshotBuf, AMS_ATT_DELETED);
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_AND_ATTRIBUTES, &dir)) {
	sprintf(ErrorText, "Could not undelete message %d", cuid);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    BumpNeedsPurging(dir, -1);
    return(0);
}

CUI_MarkAsRead(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    bone(SnapshotBuf, AMS_SNAPSHOTSIZE);
    AMS_UNSET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UNSEEN);
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_AND_ATTRIBUTES, &dir)) {
	if (AMS_ERRNO == EWOULDBLOCK) {
	    ReportSuccess("Someone else has the folder locked; the message was not marked as 'seen'.");
	} else {
	    sprintf(ErrorText, "Could not mark message %d as 'seen'", cuid);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    return(0);
}

CUI_MarkAsUnseen(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    bzero(SnapshotBuf, AMS_SNAPSHOTSIZE);
    AMS_SET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UNSEEN);
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_OR_ATTRIBUTES, &dir)) {
	sprintf(ErrorText, "Could not mark message %d as unseen", cuid);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

static unsigned dirhashfunc(s)
char *s;
{
    int c;
    unsigned int result = 0;

    /* aargh.  need better hash function */
    while (c = *s++) result += c;
    return result;
}

long CUI_GetDirNode(shortname, Node)
char *shortname;
struct CUIDirNode **Node;
{
    return(CUI_SetDirNode(shortname, (char *)NULL, Node));
}

long CUI_CacheDirName(shortname, longname)
char *shortname, *longname;
{
    return 0;  /* Now obsolete -- jgm */
}

/* Argument "longname" now no longer used --jgm */
long CUI_SetDirNode(shortname, longname, Node)
char *shortname, *longname;
struct CUIDirNode **Node;
{
    char    NameBuf[MAXPATHLEN + 1], *s, *myname, mycopy[1+MAXPATHLEN], NickName[1+MAXPATHLEN], RootBuf[1+MAXPATHLEN];
    struct CUIDirNode *DNtmp;
    unsigned fullnamehash;
    int bucket;

    strcpy(mycopy, shortname);
    myname = StripWhiteEnds(mycopy);
    if (*myname	== '/')	{
	FindTreeRoot(myname, RootBuf, 0);
    } else {
	RootBuf[0] = '\0';
    }
    for (s = myname + strlen(RootBuf); *s; ++s) {
	if (*s == '.') {
	    *s = '/';   /* Convert cmu.general to cmu/general, etc. */
	}
    }
    if (*myname == '/') {
	fullnamehash = dirhashfunc(myname);
	for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	    if (fullnamehash == DNtmp->FullNameHash && !strcmp(DNtmp->FullName, myname)) {
		if (DNtmp->WasDeleted) {
		    continue;
		}
		*Node = DNtmp;
		return(0);
	    }
	}
	mserrcode = MS_DisambiguateFile(myname, NameBuf, AMS_DISAMB_DIREXISTS);
	if (mserrcode) return (mserrcode);
    }
    else {
	mserrcode = MS_DisambiguateFile(myname, NameBuf, AMS_DISAMB_DIREXISTS);
	if (mserrcode) return (mserrcode);
	fullnamehash = dirhashfunc(NameBuf);
	for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	    if (fullnamehash == DNtmp->FullNameHash && !strcmp(DNtmp->FullName, NameBuf)) {
		if (DNtmp->WasDeleted) {
		    continue;
		}
		*Node = DNtmp;
		return(0);
	    }
	}
    }
    DNtmp = (struct CUIDirNode *) malloc (sizeof (struct CUIDirNode));
    if (DNtmp == NULL) {
	AMS_RETURN_ERRCODE(EMSNOMEM, EIN_MALLOC, EVIA_DISAMB);
    }
    if (strchr(myname, '/')) {
	CUI_BuildNickName(myname, NickName);
	myname = NickName;
    }
    DNtmp->NickName = malloc(1+strlen(myname));
    DNtmp->FullName = malloc(1+strlen(NameBuf));
    if (DNtmp->NickName == NULL || DNtmp->FullName == NULL) {
	AMS_RETURN_ERRCODE(EMSNOMEM, EIN_MALLOC, EVIA_DISAMB);
    }
    DNtmp->FullNameHash = dirhashfunc(NameBuf);
    DNtmp->Next = CUIDirCache[DNtmp->FullNameHash%CUIDIRHASHSIZE];
    strcpy(DNtmp->NickName, myname);
    strcpy(DNtmp->FullName, NameBuf);
    DNtmp->NeedsPurging = 0;
    DNtmp->WasDeleted = FALSE;
    CUIDirCache[DNtmp->FullNameHash%CUIDIRHASHSIZE] = DNtmp;
    debug(32, ("Added to directory cache:  %s\n", DNtmp->FullName));
    *Node = DNtmp;
    return(0);
}
CUI_PurgeDeletions(arg)
char   *arg;
{
    char   ErrorText[256];
    struct CUIDirNode *DNtmp;

    if (!arg || !*arg) {
	return(CUI_PurgeMarkedDirectories(FALSE, FALSE));
    }
    if ((mserrcode = CUI_GetDirNode(arg, &DNtmp)) != 0) {
	CUI_ReportAmbig(arg, "folder"); 
	return(-1);
    }
    if (!DNtmp->WasDeleted) {
	if ((mserrcode = MS_PurgeDeletedMessages(DNtmp->FullName)) != 0) {
	    sprintf(ErrorText, "Cannot purge deleted messages from '%s'.", ap_Shorten(DNtmp->FullName));
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	    return(-1);
	}
	DNtmp->NeedsPurging = 0;
	sprintf(ErrorText, "Purged deleted messages from '%s'.", ap_Shorten(DNtmp->NickName));
	ReportSuccess(ErrorText);
    }
    return(0);
}

RemoveFromDirCache(name)
char *name;
{
    struct CUIDirNode *DNtmp;
    int fullnamehash = dirhashfunc(name);

    for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	if (fullnamehash == DNtmp->FullNameHash && !DNtmp->WasDeleted && !strcmp(DNtmp->FullName, name)) {
	    DNtmp->WasDeleted = 1;
	    return(0);
	}
    }
    return(-1);
}    



long CUI_DisambiguateDir(shortname, longname)
char   *shortname;
char  **longname;
{
    struct CUIDirNode *DNtmp;

    if (mserrcode = CUI_GetDirNode(shortname, &DNtmp))  {
	*longname = NULL;
	return(mserrcode);
    } else {
	*longname = DNtmp->FullName;
	return(0);
    }
}

CUI_DoesDirNeedPurging(Dname)
char *Dname;
{
    struct CUIDirNode *DNtmp;
    int fullnamehash = dirhashfunc(Dname);

    for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	if (fullnamehash == DNtmp->FullNameHash && !DNtmp->WasDeleted && !strcmp(DNtmp->FullName, Dname)) {
	    if (DNtmp->NeedsPurging && !DNtmp->WasDeleted) {
		return(1);
	    } else {
		return(0);
	    }
	}
    }
    return(0);
}

CUI_DirectoriesToPurge() {
    int     total = 0;
    struct CUIDirNode  *DNtmp;
    int bucket;

    for (bucket = 0; bucket < CUIDIRHASHSIZE; bucket++) {
	for (DNtmp = CUIDirCache[bucket]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	    if (DNtmp->NeedsPurging && !DNtmp->WasDeleted)
	      ++total;
	}
    }
    return(total); /* Number of directories in need of purging */
}

CUI_MarkDirectoryForPurging(dirname)
char   *dirname;
{
    return(BumpNeedsPurging(dirname, 1));
}

CUI_UnmarkDirectoryForPurging(dirname)
char   *dirname;
{
    printf("Obsolete call -- CUI_UnmarkDirectoryForPurging %s\n", dirname);
}

BumpNeedsPurging(dirname, change)
char *dirname;
int change;
{
    struct CUIDirNode  *DNtmp;
    char ErrorText[1000];
    int fullnamehash = dirhashfunc(dirname);

    for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	if (fullnamehash == DNtmp->FullNameHash && !DNtmp->WasDeleted && !strcmp(DNtmp->FullName, dirname)) {
	    DNtmp->NeedsPurging += change;
	    return(0);
	}
    }
    sprintf(ErrorText, "Cannot mark folder %s for later purging.", ap_Shorten(dirname));
    ReportError(ErrorText, ERR_WARNING, FALSE);
    return(-1);
}

static char *BigPurgeVector[] = {
    NULL,
    "Yes; purge them all",
    "No; do not purge any",
    "Purge selectively",
    0,
    0
};

CUI_PurgeMarkedDirectories(Ask, OfferQuit) 
Boolean Ask, OfferQuit;
{
    struct CUIDirNode  *DNtmp;
    char    ErrorText[256], Qtext[100], *LittlePurgeVector[6];
    int numtopurge, ans;
    int bucket;

    if (Ask) {
	numtopurge = CUI_DirectoriesToPurge();
	if (numtopurge <= 0) return(0);
	if (numtopurge > 1) {
	    if (numtopurge > 2) {
		sprintf(Qtext, "Do you want to purge all %s folders with deletions?", cvEng(numtopurge, 0, 1000));
	    } else {
		strcpy(Qtext, "Do you want to purge both folders with deletions?");
	    }
	    BigPurgeVector[0] = Qtext;
	    if (OfferQuit) {
		BigPurgeVector[4] = "Do not quit";
	    } else {
		BigPurgeVector[4] = NULL;
	    }
	    ans = ChooseFromList(BigPurgeVector, 2);
	    switch(ans) {
		case 1:
		    Ask = FALSE;
		    break;
		case 3:
		    break;
		case 4:
		    return(1);
		default:
		    return(0);
	    }
	}
    }
    for (bucket = 0; bucket < CUIDIRHASHSIZE; bucket++) {
	for (DNtmp = CUIDirCache[bucket]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	    if (DNtmp->NeedsPurging && !DNtmp->WasDeleted) {
		if (Ask) {
		    sprintf(Qtext, "Purge deleted messages from '%s'?", DNtmp->NickName);
		    LittlePurgeVector[0] = Qtext;
		    LittlePurgeVector[1] = "Yes";
		    LittlePurgeVector[2] = "No";
		    if (OfferQuit) {
			LittlePurgeVector[3] = "Do not quit";
			LittlePurgeVector[4] = NULL;
		    } else {
			LittlePurgeVector[3] = NULL;
		    }
		    ans = ChooseFromList(LittlePurgeVector, 2);
		    if (ans == 3) return(1);
		    if (ans != 1) continue;
		}
		if ((mserrcode = MS_PurgeDeletedMessages(DNtmp->FullName)) != 0) {
		    sprintf(ErrorText, "Cannot purge deleted messages from %s", ap_Shorten(DNtmp->FullName));
		    ReportError(ErrorText, ERR_WARNING, TRUE);
		    return(-1);
		}
		sprintf(ErrorText, "Purged deleted messages from folder %s", ap_Shorten(DNtmp->NickName));
		ReportSuccess(ErrorText);
		DNtmp->NeedsPurging = 0;
	    }
	}
    }
    return(0);
}

/* BOGUS -- This stuff should be cached; we should NOT have to always go back
    to the message server for snapshots, but should cache the most recent ones
 */

CUI_GetSnapshotFromCUID(cuid, SnapshotBuf)
int	cuid;
char   *SnapshotBuf;
{
    char   *id,
	   *dir,
	    ErrorText[256];

    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (mserrcode = MS_GetSnapshot(dir, id, SnapshotBuf)) {
	if (AMS_ERRNO == ENOENT) {
	    ReportSuccess("This message appears to have been recently deleted and purged.");
	} else {
	    sprintf(ErrorText, "Could not get the snapshot for message %d", cuid);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    return(0);
}

CUI_GetHeaderContents(cuid, HeaderName, HeaderTypeNumber, HeaderBuf, lim)
char   *HeaderName;
char   *HeaderBuf;
int	cuid,
	HeaderTypeNumber,
	lim;
{
    return(GetHeaderContents(cuid, HeaderName, HeaderTypeNumber, HeaderBuf, lim, TRUE));
}

GetHeaderContents(cuid, HeaderName, HeaderTypeNumber, HeaderBuf, lim, barfmissing)
char   *HeaderName;
char   *HeaderBuf;
int	cuid,
	HeaderTypeNumber,
	lim,
        barfmissing;
{
    char   *id,
	   *dir,
	    ErrorText[256];

    debug(1,("Trying to get header contents %d\n", cuid));
    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    mserrcode = MS_GetHeaderContents(dir, id, HeaderName, HeaderTypeNumber, HeaderBuf, lim);
    if (mserrcode) {
	if (!barfmissing && (AMS_ERRNO == EINVAL)) return(0);
	ReportError("Could not get the contents of the header", ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

CUI_BuildNickName(FullName, NickName)
char   *FullName,
       *NickName;
{
    return(BuildNickName(FullName, NickName));
}

/* This routine generates a temporary file name to be written by the
	message server */

CUI_GenTmpFileName(nmbuf)
char   *nmbuf;
{
    static unsigned long ctr = 0, hostid = 0, userid = 0, procid = 0;

    if (mserrcode = MS_GenTempFileName(nmbuf)) {
	if (hostid == 0) hostid = (unsigned long) getaddr();
	if (userid == 0) userid = getuid();
	if (procid == 0) procid = getpid();
	ReportError("Message server could not generate temporary file name", ERR_WARNING, TRUE);
	sprintf(nmbuf, "/tmp/AMS.%d", hostid ^ ((userid & 0xFF) << 24) | ((procid &0xFF) << 16) | (((ctr++) & 0xFF) << 8));
    }
}

CUI_SubmitMessage(InFile, DeliveryOpts)
char   *InFile;
int	DeliveryOpts;
{
    char    ErrorText[256], ErrorMessage[500];

    if (!CUI_DeliveryInfoString) {
	if (!CUI_ClientVersion) CUI_SetClientVersion("Unknown.Interface");
	if (!CUI_MachineName) CUI_SetMachineName("Unknown.Machine.Name");
	if (!CUI_MachineType) CUI_SetMachineType("Unknown.Machine.Type");
	CUI_SetupDeliveryString();
    }
    if ((mserrcode = MS_SubmitMessage(InFile, DeliveryOpts, ErrorMessage, sizeof(ErrorMessage), CUI_DeliveryInfoString)) != 0) {
	if (AMS_ERRNO == EINVAL && AMS_ERRCAUSE == EIN_PARAMCHECK && AMS_ERRVIA == EVIA_SUBMITMESSAGE) {
	    ReportSuccess("You must specify one or more recipients.");
	    return(-1);
	}
	if (AMS_ERRNO == EMSNOSUBJ) {
	    ReportSuccess("You must specify a subject.");
	    return(-1);
	}
	if (AMS_ERRNO == EMSDROPOFFWARN) {
	    ReportSuccess(ErrorMessage);
	    MS_UnlinkFile(InFile);
	    return(0);
	}
	if (AMS_ERRNO == EMSDROPOFFLOCALQUEUE) {
	    ReportSuccess(ErrorMessage);
	    MS_UnlinkFile(InFile);
	    return(0);
	}
	if (!(DeliveryOpts & AMS_SEND_ILLEGAL)) {
	    if (AMS_ERRNO == EMSNONASCIIMAIL) {
		if (GetBooleanFromUser("Your message contains unprintable characters.  Send it anyway", FALSE)) {
		    return(CUI_SubmitMessage(InFile, DeliveryOpts | AMS_SEND_ILLEGAL));
		}
		return(-1);
	    }
	    if (AMS_ERRNO == EMSLONGLINES) {
		if (GetBooleanFromUser("Your message has lines too long for many network mailers.  Send anyway", FALSE)) {
		    return(CUI_SubmitMessage(InFile, DeliveryOpts | AMS_SEND_ILLEGAL));
		}
		return(-1);
	    }
	}
	if (AMS_ERRNO == ENOENT) {
	/* Either something is really strange or we changed server hosts in the middle of sending or SNAP replies failed */
	    ReportError("Message submission probably failed -- results not guaranteed", ERR_CRITICAL, FALSE);
	}
	else {
	    sprintf(ErrorText, "Submission failed: %s", ErrorMessage);
	    ReportError(ErrorText, ERR_CRITICAL, TRUE);
	}
	return(-1);
    }
    ReportSuccess("Your message has been sent.");
    return(0);
}

CUI_ReconstructDirectory(arg, TrustTimeStamp)
char   *arg;
int TrustTimeStamp;
{
    char   *FullName,
	    NameBuf[1+MAXPATHLEN],
	    ErrorText[256];
    int     Good,
	    Bad;

    if (!arg || !*arg) {
	ReportError("You must supply the name of a message folder", ERR_WARNING, FALSE);
	return(-1);
    }
    if ((mserrcode = CUI_DisambiguateDir(arg, &FullName)) != 0) {
 	if ((mserrcode = MS_DisambiguateFile(arg, NameBuf, AMS_DISAMB_FILEEXISTS))) {
	    CUI_ReportAmbig(arg, "folder");
	    return(-1);
	}
	FullName = NameBuf;
    }
    sprintf(ErrorText, "Reconstructing %s; please wait...", ap_Shorten(FullName));
    ReportSuccess(ErrorText);
    if ((mserrcode = MS_ReconstructDirectory(FullName, &Good, &Bad, TrustTimeStamp)) != 0) {
	sprintf(ErrorText, "Cannot reconstruct message folder %s", ap_Shorten(FullName));
	ReportError(ErrorText, ERR_WARNING, TRUE);
	if (AMS_ERRNO == EACCES) {
	    ReportSuccess("To reconstruct, you need full access to the directory, its parent, and the root of the tree (.MESSAGES dir).");
	}
    	return(-1);
    }
    if (Good) {
	if (Bad) {
	    sprintf(ErrorText, "Mostly reconstructed folder %s: %d entries, %d failures", ap_Shorten(FullName), Good, Bad);
	}
	else {
	    sprintf(ErrorText, "Reconstructed folder %s with %s entries", ap_Shorten(FullName), cvEng(Good, 0, 1000));
	}
    }
    else {
	if (Bad) {
	    sprintf(ErrorText, "Empty folder after reconstructing %s: %d files could not be entered", ap_Shorten(FullName), Bad);
	}
	else {
	    sprintf(ErrorText, "Reconstructed EMPTY message folder %s", ap_Shorten(FullName));
	}
    }
    ReportSuccess(ErrorText);
    return(Bad ? -2 : 0);
}

static char *ClosingParen(s)
char *s;
{
    register int ctr = 0;

    while (*s) {
	switch (*s) {
	    case '(':
		++ctr;
		break;
	    case ')':
		if (--ctr == 0) {
		    return(s);
		} else if (ctr < 0) {
		    return(NULL);
		}
		break;
	    default:
		break;
	}
	++s;
    }
    return(NULL);
}

static char *CommentTexts[] = { /* indexed to MSWP_ constants */
    "", /* no zero value */
    "", /* MSWP_GOODUSER */
    "", /* MSWP_GOODMSDIR */
    "Would create new message folder)", /* MSWP_CREATABLEMSDIR */
    "No direct posting)", /* MSWP_BADMSDIR */
    "Recipient unknown)", /* MSWP_CRAP */
    "", /* MSWP_AMBIGUOUS */
    "", /* MSWP_VERYAMBIGUOUS */
    "", /* MSWP_GOODNETMAIL */
    "Unknown network address)", /* MSWP_BADNETMAIL */
    "Temporary address lookup failure)", /* MSWP_TEMPFAIL */
    "Temporary failure to validate host name)", /* MSWP_UNKNOWNNETMAIL */
    "Address probably valid, but not certain)", /* MSWP_PROBABLYGOOD */
    "Address ambiguous; temporary failures also occurred)", /* MSWP_PROBABLYAMBIGUOUS */
    "External bboard posting not enabled -- type 'help externalbb' for help)", /* MSWP_NOEXTPOSTING */
    "", /* MSWP_PERSONALALIAS */
    "No such user ID)", /* MSWP_INVALIDUSER */
    "No such message folder)", /* MSWP_INVLAIDMSGDIR */
    "", /* MSWP_DISTLIST */
    "Good address of unknown type)", /* MSWP_UNKNOWNGOOD */
    "Bad file name specified as distribution list)", /* MSWP_BADDISTLIST */
    "Distribution list file is read-protected)", /* MSWP_PROTDISTLIST */
    "Personal alias has non-printing characters)", /* MSWP_BADALIAS */
    "Distribution list file is a directory)", /* MSWP_DISTLISTDIR */
    "", /* MSWP_DIRINSERT */
    "Bad file name specified for directory insertion)", /* MSWP_BADDIRINSERT */
    "Directory to insert in is read-protected)", /* MSWP_PROTDIRINSERT */
    "Directory to insert in is a file)", /* MSWP_DIRINSERTFILE */
    "Very fuzzy name match)", /* MSWP_MATCHTOOFUZZY */
    "", /* MSWP_FUZZYAMBIGMATCH */
    "", /* MSWP_FUZZYTOOMANYMATCHES */
    "Address fuzzy and ambiguous; temporary failures also occurred)", /* MSWP_PROBABLYFUZZYAMBIG */
    "Ambiguous name; temporary failure listing possibile matches)", /* MSWP_AMBIGWITHERRORS */
    "", /* MSWP_GOODEXTMSDIR */
    "", /* MSWP_EXTFORCEFORMAT THIS AND THE FOLLOWING FIVE ARE THE EXTERNAL FORCING CODES */
    "", /* MSWP_EXTFORCESTRIP */
    "", /* MSWP_EXTFORCETRUST */
    "", /* MSWP_FORCEFORMATDIR */
    "", /* MSWP_FORCESTRIPDIR */
    "", /* MSWP_FORCETRUSTDIR */
    "", /* MSWP_FSMEMBERS */
    "Requires an AFS groupname)", /* MSWP_BADFSMEMBERS */
    "Could not determine groupness of name; errors were encountered)", /* MSWP_UNKNOWNFSMEMBERS */
    0
};
/* NOTE: IF ANY OF THE ABOVE ARE LONGER THAN 100, the constant PAD_FOR_COMM 
    MUST be raised below to remain 5 more than the longest. */
#define PAD_FOR_COMM 105

#define MAX_ADDR 4000

CUI_RewriteHeaderLine(text, newtext)
char *text, **newtext;
{
    int numfound = 0, externalct = 0, formatct = 0, trustct = 0, stripct = 0;

    return(CUI_RewriteHeaderLineInternal(text, newtext, 25, &numfound, &externalct, &formatct, &stripct, &trustct));
}

CUI_RewriteHeaderLineInternal(text, newtext, maxdealiases, numfound, externalct, formatct, stripct, trustct)
char *text, **newtext;
int maxdealiases, *numfound, *externalct, *formatct, *stripct, *trustct;
{
    char *s, FileName[1+MAXPATHLEN], ReplaceAddr[MAX_ADDR], ThisAddr[MAX_ADDR];
    int BadCount = 0, errcode, which, foundlastpass;

    debug(1, ("RewriteHeaderLine %s, maxaliases %d, numfound %d\n", text, maxdealiases, *numfound));
    *newtext = NULL;
    ReplaceAddr[0] = '\0';
    if (maxdealiases <= 0) {
	ReportError("There appears to be a loop in your personal alias definitions!", ERR_CRITICAL, FALSE);
	return(1);
    }
    CUI_GenTmpFileName(FileName);
    PutStringToViceFile(FileName, text);
    which = 0;
    foundlastpass = 1;
    while (TRUE) {
	mserrcode = MS_ValidateAndReplaceChunk(FileName, ReplaceAddr, ThisAddr, MAX_ADDR, which, &errcode);
	if (mserrcode) {
	    char *ErrText;
	    if (AMS_ERRNO == EMSENDOFLIST) break;
	    ErrText = malloc(100+strlen(text));
	    if (AMS_ERRNO == EFAULT || AMS_ERRNO == EMSBADLOCALSYNTAX) {
		if (ErrText) {
		    sprintf(ErrText, "Syntactically incorrect address: #%d in '%s'.", which+1, text);
		    ReportSuccess(ErrText);
		} else {
		    ReportSuccess("The address you typed is syntactically incorrect and could not be parsed.");
		}
	    } else {
		if (ErrText) {
		    sprintf(ErrText, "Cannot decode address #%d in '%s'.", which+1, text);
		    ReportError(ErrText, ERR_WARNING, TRUE);
		} else {
		    ReportError("Cannot decode the address specification", ERR_WARNING, TRUE);
		}
	    }
	    if (ErrText) free(ErrText);
	    errcode = MSWP_CRAP;
	    GetViceFileToNewString(FileName, newtext, TRUE);
	    MS_UnlinkFile(FileName);
	    return(1);
	}
	which += foundlastpass;
	foundlastpass = *numfound;
	if (HandleAddress(ThisAddr, ReplaceAddr, MAX_ADDR, errcode, maxdealiases, numfound, externalct, formatct, stripct, trustct)) {
	    ++BadCount;
	    s = CommentTexts[errcode];
	    if (s && *s) {
	        strcat(ReplaceAddr, " ");
	        strcat(ReplaceAddr, AMS_VALIDATION_ERR_PREFIX);
	        strcat(ReplaceAddr, s);
	    }
	}
	foundlastpass = *numfound - foundlastpass;
    }
    GetViceFileToNewString(FileName, newtext, TRUE);
    MS_UnlinkFile(FileName);
    return(BadCount);
}

static int dostat(afile,asize)
FILE *afile;
long *asize;
{if(fseek(afile,0L,2) == -1)	/*seek to end*/
  return TRUE;			/*fail if seek fails*/
 *asize=ftell(afile);		/*see where end of file is*/
 if(fseek(afile,0L,0) == -1)	/*seek to back the begining*/
   return TRUE;			/*fail if seek fails*/
 return FALSE;
}

GetViceFileToNewString(FileName, newtext, DoUnlink)
char *FileName, **newtext;
Boolean DoUnlink;
{
    debug(1, ("Get vice file %s to string\n", FileName));
    *newtext = NULL;
    if (CUI_OnSameHost) {
	int LocalFileSize;
	long longLocalFileSize;
	FILE *in_file;
	
	in_file=fopen(FileName,"r");
	if(in_file==NULL) {
	    ReportError("Cannot open local file", ERR_WARNING, FALSE);
	    return(1);
	  }

	if (dostat(in_file, &longLocalFileSize)) {
	    fclose(in_file);
	    ReportError("Cannot determine local file size",ERR_WARNING,FALSE);
	    return(1);
	}
     /*
	Since malloc and read take an integer punt if the file size
	won't fit into an integer
	*Warning* this means that there are messages that a minicomputer
	can handle that a micro can not.
     */
	if ((sizeof(int) < 4) &&
	    (longLocalFileSize > 30000L)) {
	    fclose(in_file);
	    ReportError("Local file is too big", ERR_WARNING, FALSE);
	    return(1);
	  }
	LocalFileSize=longLocalFileSize;

	*newtext = malloc(1+LocalFileSize);
	if (!*newtext) {
	    fclose(in_file);
	    ReportError("Out of memory!", ERR_WARNING, FALSE);
	    return(1);
	}

#ifdef CUI_READLIBERALLY
	fread(*newtext,1,LocalFileSize,in_file);
	if(ferror(infile))
#else /* CUI_READLIBERALLY */
	if (fread(*newtext,1,LocalFileSize,in_file) != LocalFileSize)
#endif /* CUI_READLIBERALLY */
	{
	    fclose(in_file);
	    free(*newtext);
	    *newtext = NULL;
	    ReportError("Cannot read from local file", ERR_WARNING, FALSE);
	    return(1);
	}
	fclose(in_file);
	*(*newtext + LocalFileSize) = '\0';
	if (DoUnlink) unlink(FileName);
	return(0);
    } else {
	char    CharBuf[256], *read_buf;
	int     bodylen;
	long    offset, bytesunfetched, buflen;
    
	offset = 0L;
	if ((mserrcode = MS_GetPartialFile(FileName, CharBuf, sizeof(CharBuf)-1, offset, &bytesunfetched, &bodylen)) != 0) {
		sprintf(CharBuf, "The file %s could not be read", ap_Shorten(FileName));
		ReportError(CharBuf, ERR_WARNING, TRUE);
		return(-1);
	}
	buflen = (long)bodylen + bytesunfetched;
	if (buflen != (long)(int)buflen) {
	    ReportError("Problem with mallocs that expect ints.", ERR_CRITICAL, FALSE);
	    return(-1);
	}
	*newtext = malloc(1+(int)buflen);
	if (!*newtext) {
	    ReportError("Out of memory!", ERR_WARNING, FALSE);
	    return(1);
	}

	read_buf = *newtext;
	do {
	    if ((mserrcode = MS_GetPartialFile(FileName, read_buf, (int) buflen, offset, &bytesunfetched, &bodylen)) != 0)
		break;
	    if (bodylen <= 0)
		break;
	    read_buf += bodylen;
	    offset += (long)bodylen;
	    buflen -= bodylen;
	} while (bytesunfetched > 0);

	(*newtext)[offset] = '\0';

	if (mserrcode || bodylen < 0) {
	    sprintf(CharBuf, "The file %s could not be read", ap_Shorten(FileName));
	    if (offset) strcat(CharBuf, " completely");
	    ReportError(CharBuf, ERR_WARNING, TRUE);
	    return(-1);
	}
    }
    return(0);
}


long HandleAddress(oldaddr, newaddr, newsize, errcode, maxdealiases, numfound, externalct, formatct, stripct, trustct)
char *oldaddr, *newaddr;
int newsize, errcode, maxdealiases, *numfound, *externalct, *formatct, *stripct, *trustct;
{
    int i, numchoices, len;
    char ErrorText[256], *s, *s2, *Qarray[MSWP_MAXAMBIGMATCHES+3], c;

    c = 0;
    s2 = NULL;

    debug(1, ("HandleAddress %s code %d maxdealiases %d numfound %d\n", oldaddr, errcode, maxdealiases, *numfound));
    if (!*oldaddr) {
	*newaddr = '\0';
	return(0);
    }
    len = strlen(oldaddr);
    s = oldaddr + len + 1;
    if (*s++ == '\001') {
	strncpy(newaddr, s, newsize);
    } else {
	strcpy(newaddr, "that address");
    }
    ++*numfound;
    switch(errcode) {
	    case MSWP_GOODUSER : /* address parsed to a good local user name */
	    case MSWP_GOODMSDIR : /* a good (postable) ms folder */
	    case MSWP_DISTLIST: /* a good distribution list */
	    case MSWP_DIRINSERT: /* a good dir-insert address */
	    case MSWP_FSMEMBERS: /* a good AFS group address */
		strncpy(newaddr, oldaddr, newsize);
		break;
	    case MSWP_GOODNETMAIL:
	    case MSWP_GOODEXTMSDIR:
		strncpy(newaddr, oldaddr, newsize);
		++*externalct;
		break;
	    case MSWP_EXTFORCEFORMAT: 
	    case MSWP_EXTFORCEFORMATDIR:
		strncpy(newaddr, oldaddr, newsize);
		++*externalct;
		++*formatct;
		break;
	    case MSWP_EXTFORCESTRIP:
	    case MSWP_EXTFORCESTRIPDIR:
		strncpy(newaddr, oldaddr, newsize);
		++*externalct;
		++*stripct;
		break;
	    case MSWP_EXTFORCETRUST:
	    case MSWP_EXTFORCETRUSTDIR:
		strncpy(newaddr, oldaddr, newsize);
		++*externalct;
		++*trustct;
		break;
	    case MSWP_UNKNOWNGOOD:
		sprintf(ErrorText, "%s: good address of unknown type", oldaddr);
		ReportSuccess(ErrorText);
		strncpy(newaddr, oldaddr, newsize);
		break;
	    case MSWP_CREATABLEMSDIR : /* a creatable ms folder */
		s = strchr(oldaddr, '@');
		if (!s) s = strchr(oldaddr, '(');
		if (s) {
		    c = *s;
		    *s = '\0';
		}
		sprintf(ErrorText, "%s: would create new message folder", newaddr);
		ReportSuccess(ErrorText);
		sprintf(ErrorText, "Do you really want to create %s", newaddr);
		if (s) {
		    *s = c;
		}
		if (!GetBooleanFromUser(ErrorText, FALSE)) {
		    return(1);
		}
		strncpy(newaddr, oldaddr, newsize);
		break;
	    case MSWP_BADMSDIR :	   /* an ms folder you can not post on */
	    case MSWP_NOEXTPOSTING :	   /* External posting not enabled for user */
		if (errcode == MSWP_NOEXTPOSTING) {
		    sprintf(ErrorText, "You are not authorized to post on external bboards such as %s", newaddr);
		    ReportSuccess(ErrorText);
		    strcpy(ErrorText, "Try 'help networks-access' or see the 'externalbb.help' help file for help.");
		} else {
		    sprintf(ErrorText, "You are not allowed to post on %s", newaddr);
		}
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_PROBABLYFUZZYAMBIG : /* ambig plus temp fail */
	    case MSWP_AMBIGWITHERRORS : /* ambig plus temp fail */
		sprintf(ErrorText, "Ambiguous name; temporary failure listing possibile matches: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_CRAP :	/* Unrecognizable junk */
		sprintf(ErrorText, "Unrecognized recipient: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_BADDISTLIST :	/* bad file name for dist list */
		sprintf(ErrorText, "Unknown distribution file: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_BADDIRINSERT :	/* bad file name for dir insert */
		sprintf(ErrorText, "Unknown insertion directory: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_BADFSMEMBERS: /* either no name, or a user name was given where a group name was required */
		sprintf(ErrorText, "Not an AFS group name:  '%s'",
			newaddr?newaddr:"<NULL>");
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_BADALIAS :	/* bad definition for personal alias */
		sprintf(ErrorText, "Bad definition of personal alias: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_PROTDISTLIST :	/* protected file name given for dist list */
		sprintf(ErrorText, "Protection violation on distribution file: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_DISTLISTDIR :	/* dist list is a dir */
		sprintf(ErrorText, "Distribution file is a directory: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_DIRINSERTFILE :	/* Dir to insert in is a file */
		sprintf(ErrorText, "Directory to insert in is a file: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_PROTDIRINSERT :	/* protected file name for dir-insert */
		sprintf(ErrorText, "Protection violation on directory insertion: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_INVALIDUSER :	/* Unrecognized user id */
		sprintf(ErrorText, "Invalid user ID: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_INVALIDMSGDIR :	/* Unrecognized folder name */
		sprintf(ErrorText, "Invalid message folder name: %s", newaddr);
		ReportSuccess(ErrorText);
		return(1);
	    case MSWP_BADNETMAIL :	/* Unrecognized host */
	    case MSWP_UNKNOWNNETMAIL : /* host name can not be verified */
		{
		char uglyname[1000], *paren;

		paren = strchr(oldaddr, '(');
		if (paren) {
		    *paren = '\0';
		    strcpy(uglyname, ++paren);
		    paren = strchr(uglyname, ')');
		    if (paren) *paren = '\0';
		} else {
		    strcpy(uglyname, "<bad parse>");
		}
		if (errcode ==MSWP_UNKNOWNNETMAIL) {
		    sprintf(ErrorText, "Cannot check validity of hostname (network or file servers may be down)");
		    ReportSuccess(ErrorText);
		    sprintf(ErrorText, "Are you sure the host '%s' really exists", uglyname);
		} else {
		    sprintf(ErrorText, "The host '%s' is unknown; try sending anyway", uglyname);
		}
		if (!GetBooleanFromUser(ErrorText, FALSE)) {
		    return(1);
		}
		++*externalct;
		strncpy(newaddr, oldaddr, newsize);
		break;
		}
	    case MSWP_VERYAMBIGUOUS :	/* Highly ambiguous name */
	    case MSWP_FUZZYTOOMANYMATCHES :	/* Highly ambiguous fuzzy name */
	    {
		char FileName[1+MAXPATHLEN];

		sprintf(ErrorText, "The name '%s' is very ambiguous.", oldaddr);
		ReportSuccess(ErrorText);
		if (GetBooleanFromUser("Very ambiguous name; do you want a full list of matches", FALSE)) {
		    mserrcode = MS_WriteAllMatchesToFile(oldaddr, FileName);
		    if (mserrcode) {
			ReportError("Could not get complete list of matches", ERR_WARNING, TRUE);
			return(1);
		    }
		    sprintf(newaddr, "%s %sAll matches listed in file %s)", oldaddr, AMS_VALIDATION_ERR_PREFIX, ap_Shorten(FileName));
		} else {
		    sprintf(newaddr, "%s %sAmbiguous address; too many matches)", oldaddr, AMS_VALIDATION_ERR_PREFIX);
		}
		return(1);
	    }
	    case MSWP_AMBIGUOUS : /* The name could mean many things */
	    case MSWP_PROBABLYAMBIGUOUS : /* ambig plus temp fail */
	    case MSWP_FUZZYAMBIGMATCH : /* The name could mean many many things */
		s = strchr(oldaddr, '(');
		if (s) s2 = ClosingParen(s);
		if (!s || !s2) {
		    sprintf(ErrorText, "Unparsable ambiguous name: %s", newaddr);
		    ReportSuccess(ErrorText);
		    return(1);
		}
		*s++ = '\0';
		*s2 = '\0';
		if (errcode == MSWP_PROBABLYAMBIGUOUS || errcode == MSWP_PROBABLYFUZZYAMBIG) {
		    sprintf(ErrorText, "Temporary validation failure; '%s' is probably ambiguous.", newaddr);
		} else {
		    sprintf(ErrorText, "The name '%s' is ambiguous.", newaddr);
		}
		ReportSuccess(ErrorText);
		if (errcode == MSWP_PROBABLYAMBIGUOUS || errcode == MSWP_PROBABLYFUZZYAMBIG) {
		    sprintf(ErrorText, "Temporary validation failure; what is '%s'?", newaddr);
		} else {
		    sprintf(ErrorText, "What did you mean by '%s'?", newaddr);
		}
		Qarray[0] = ErrorText;
		numchoices = 1;
		while (s) {
		    s2 = NextAddress(s);
		    if (s2) {
			*s2++ = '\0';
		    }
		    ReduceWhiteSpace(s);
		    Qarray[numchoices++] = s;
		    s = s2;
		}
		Qarray[numchoices] = "None of the Above";
		Qarray[numchoices+1] = NULL;
		i = ChooseFromList(Qarray, numchoices);
		if (i >= numchoices || i <= 0) {
		    newaddr[newsize-20-sizeof(AMS_VALIDATION_ERR_PREFIX)] = '\0';
		    strcat(newaddr, " ");
		    strcat(newaddr, AMS_VALIDATION_ERR_PREFIX);
		    strcat(newaddr, "ambiguous name)");
		    return(1);
		} else {
		    /* Good choice; needs to be revalidated */
		    char *mytext;
		    int mynumfound = 0;

		    if (mserrcode = CUI_RewriteHeaderLineInternal(Qarray[i], &mytext, --maxdealiases, &mynumfound, externalct, formatct, stripct, trustct)) {
			return(mserrcode);
		    }
		    *numfound = *numfound + mynumfound - 1;
		    strncpy(newaddr, mytext, newsize);
		}
		break;
	    case MSWP_PERSONALALIAS : /* A personal mail alias */
		{
		char *mytext;
		int mynumfound = 0;

		if (mserrcode = CUI_RewriteHeaderLineInternal(oldaddr, &mytext, --maxdealiases, &mynumfound, externalct, formatct, stripct, trustct)) {
		    return(mserrcode);
		}
		*numfound = *numfound + mynumfound - 1;
		strncpy(newaddr, mytext, newsize);
		break;
		}
	    case MSWP_PROBABLYGOOD: /* PARTIAL temporary failure */
	    case MSWP_TEMPFAIL : /* A temporary failure */
	    case MSWP_MATCHTOOFUZZY: /* A heuristic match */
	    case MSWP_UNKNOWNFSMEMBERS:	/* Possible temporary failure */
	    default:
		if (errcode == MSWP_PROBABLYGOOD || errcode == MSWP_TEMPFAIL || errcode == MSWP_UNKNOWNFSMEMBERS) {
		    ReportSuccess("Temporary failure; an AFS server or the network may be down.");
		    strcpy(ErrorText, "Temporary validation failure; please confirm :");
		} else if (errcode == MSWP_MATCHTOOFUZZY) {
		    strcpy(ErrorText, "Name match was uncertain; please confirm:");
		} else {
		    sprintf(ErrorText, "Unrecognized validation code %d; please confirm:", errcode);
		}
		Qarray[0] = ErrorText;
		Qarray[1] = oldaddr;
		Qarray[2] = "None of the above";
		Qarray[3] = NULL;
		i = ChooseFromList(Qarray, 2);
		if (i != 1) {
		    return(1);
		}
		strncpy(newaddr, oldaddr, newsize);
		break;
    }
    return(0);
}

CUI_CloneMessage(cuid, OrigDirName, Code)
int cuid, Code;
char *OrigDirName;
{
    char ErrorText[256], *newpart, *oldpart, *ParentDir, *id, *dir, FullNewDirName[1+MAXPATHLEN], PDirName[1+MAXPATHLEN], *NewDirName, *newpart2, DirName[1+MAXPATHLEN];

    debug(1,("Trying to clone message %d into %s code %d\n", cuid, OrigDirName, Code));
    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    strcpy(DirName, OrigDirName);
    if (mserrcode = CUI_DisambiguateDir(DirName, &NewDirName)) {
	if (AMS_ERRNO != ENOENT) {
	    CUI_ReportAmbig(DirName, "folder");
	    return(-1);
	}
	newpart = strrchr(DirName, '.');
	newpart2 = strrchr(DirName, '/');
	if (newpart2 > newpart) {
	    newpart = newpart2;
	}
	if (newpart) {
	    *newpart++ = '\0';
	    if (mserrcode = CUI_DisambiguateDir(DirName, &NewDirName)) {
		if (mserrcode = MS_DisambiguateFile(DirName, PDirName, AMS_DISAMB_EXISTS)) {
		    CUI_ReportAmbig(DirName, "folder");
		    return(-1);
		}
		NewDirName = PDirName;
		sprintf(ErrorText, "Warning: %s is not a message folder.", ap_Shorten(PDirName));
		ReportSuccess(ErrorText);
	    }
	    oldpart = DirName;
	} else {
	    newpart = DirName;
	    oldpart = NULL;
	    if (mserrcode = MS_DisambiguateFile("~/.MESSAGES", PDirName, AMS_DISAMB_EXISTS)) {
		ReportError("Cannot identify your root mail directory", ERR_WARNING, TRUE);
		return(-1);
	    }
	    NewDirName = PDirName;
	}
	sprintf(ErrorText, "'%s' does not exist.  Create it %s %s",
	    newpart,
	    oldpart ? "under" : "",
	    oldpart ? oldpart : "as a top-level mail folder");
	if (!GetBooleanFromUser(ErrorText, FALSE)) {
	    ReportSuccess("Nothing was created or filed.");
	    return(-1);
	}
	ParentDir =  NewDirName;
	strcpy(FullNewDirName, NewDirName);
	strcat(FullNewDirName, "/");
	strcat(FullNewDirName, newpart);
	if (CUI_CreateNewMessageDirectory(FullNewDirName, FullNewDirName)) {
	    return(-1); /* errors were reported */
	}
	NewDirName = FullNewDirName;
	GetAMSID(cuid, &id, &dir);
	mserrcode = MS_InstallWelcomeMessage(ParentDir, dir, id, newpart);
	if (mserrcode && AMS_ERRNO == EMSNOPARENT) {
	    if (oldpart) {
		ReportSuccess("Initial notice worked; parental notice failed.");
	    }
	    mserrcode = 0;
	}
	if (mserrcode) {
	    ReportError("Cannot install initial notice in new folder", ERR_WARNING, TRUE);
	    return (-1);
	}
	if (Code == MS_CLONE_COPYDEL || Code == MS_CLONE_APPENDDEL) {
	    sprintf(ErrorText, "Moved message %s into %s.", id, ap_Shorten(FullNewDirName));
	    ReportSuccess(ErrorText);
	    return(CUI_DeleteMessage(cuid));
	} else {
	    sprintf(ErrorText, "Copied message %s into %s.", id, ap_Shorten(FullNewDirName));
	    ReportSuccess(ErrorText);
	    return(0);
	}
    }
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (!strcmp(dir, NewDirName)) {
	sprintf(ErrorText, "Message %d is already in %s.  No change was necessary.", cuid, ap_Shorten(DirName));
	ReportSuccess(ErrorText);
	return(-1);
    }
    mserrcode = MS_CloneMessage(dir, id, NewDirName, Code);
    if (mserrcode) {
	if (AMS_ERRNO == EMSALREADYTHERE) {
	    sprintf(ErrorText, "Message %d was already in %s; no change was necessary.", cuid, ap_Shorten(DirName));
	    ReportSuccess(ErrorText);
	} else if (AMS_ERRNO == EACCES) {
	    sprintf(ErrorText, "You do not have write-access to the '%s' folder.", ap_Shorten(DirName));
	    ReportSuccess(ErrorText);
	} else {
	    char NickName[1+MAXPATHLEN];

	    CUI_BuildNickName(dir, NickName);
	    sprintf(ErrorText, "Cannot copy message from %s to ", ap_Shorten(NickName));
	    strcat(ErrorText, ap_Shorten(DirName));
	    strcat(ErrorText, " ("); strcat(ErrorText, id); strcat(ErrorText, ")");
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(mserrcode);
    }
    if (Code == MS_CLONE_COPYDEL || Code == MS_CLONE_APPENDDEL) {
	sprintf(ErrorText, "Moved message %s into %s.", id, ap_Shorten(DirName));
    } else {
	sprintf(ErrorText, "Copied message %s into %s.", id, ap_Shorten(DirName));
    }
    ReportSuccess(ErrorText);
    if (Code == MS_CLONE_COPYDEL || Code == MS_CLONE_APPENDDEL) {
	return(MarkDirectoryForPurging(dir));
    } else {
	return(0);
    }
}

char *copy (s)
register char  *s;
{
    register char  *new;
    if (!s || !*s)
	return NIL;		/* Check for NIL and 0-length  */
    new = malloc(strlen(s) + 1);
    if (new == NIL)
	return NIL;
    strcpy(new, s);
    return new;
}

#define TIMELEN 10

CUI_PrintUpdates(dname, nickname)
char *dname, *nickname;
{
    return(CUI_PrintUpdatesWithFlags(dname, nickname, 0, (char *)NULL));
}

CUI_PrintUpdatesWithFlags(dname, nickname, flags, printer)
char *dname, *nickname, *printer;
int flags;
{
    long cuid, numbytes, totalbytes, status;
    int IsDup;
    char date64[TIMELEN], ErrorText[256], newdate[TIMELEN], *s, headbuf[HEADBUFSIZE], *DirName;

    if ((mserrcode = CUI_DisambiguateDir(dname, &DirName)) != 0) {
	if (AMS_ERRNO == ENOENT) {
	    return(CUI_HandleMissingFolder(dname));
	} else if (vdown(AMS_ERRNO)) {
	    sprintf(ErrorText, "%s: temporarily unavailable (net/server problem)", ap_Shorten(nickname));
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	} else {
	    sprintf(ErrorText, "The folder %s is not readable.", ap_Shorten(nickname));
	    ReportError(ErrorText, ERR_CRITICAL, TRUE);
	}
	return(-1);
    }

    if ((mserrcode = MS_GetAssociatedTime(DirName, date64, TIMELEN)) != 0) {
	sprintf(ErrorText, "Couldn't get associated time for %s", ap_Shorten(DirName));
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    strncpy(newdate, date64, AMS_DATESIZE);
    totalbytes = 0;
    do {
	if (CUI_GetHeaders(DirName, date64, headbuf, HEADBUFSIZE, totalbytes, &numbytes, &status, FALSE) != 0) {
	    ReportError("Could not print all of the messages.", ERR_WARNING, TRUE);
	    break;
	}
	if (numbytes <= 0)
	    break;
	totalbytes += numbytes;
	for (s = headbuf; s - headbuf < numbytes; s += AMS_SNAPSHOTSIZE) {
	    cuid = GetCuid(AMS_ID(s), DirName, &IsDup);
/* 	    sprintf(ErrorText, "Printing '%s'", AMS_CAPTION(s)); */
/* The above line was simply too verbose to make PCMessages happy... */
	    sprintf(ErrorText, "Printing %d", cuid);
	    ReportSuccess(ErrorText);
	    if (CUI_PrintBodyFromCUIDWithFlags(cuid, flags, printer)) break;
	    strncpy(newdate, AMS_DATE(s), AMS_DATESIZE);
	}
    } while (status > 0);
    if (status < 0) {
	ReportError("Couldn't print all of the messages.", ERR_WARNING, TRUE);
    }
    if ((mserrcode = MS_SetAssociatedTime(DirName, newdate)) != 0) {
	sprintf(ErrorText, "Couldn't set associated time for %s", ap_Shorten(DirName));
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

CUI_StoreFileToVice(LocalFile, ViceFile)
char *LocalFile, *ViceFile;
{
    return(CUI_AppendFileToVice(LocalFile, ViceFile, 0L));
}

CUI_AppendFileToVice(LocalFile, ViceFile, offset)
char *LocalFile, *ViceFile;
long offset;
{
    FILE *fp;
    int pos, c;
    char BigBuf[WRITEFILECHUNK+1], ErrorText[256];

    fp = fopen(LocalFile, "r");
    if (!fp) {
	sprintf(ErrorText, "Cannot open local file %s", ap_Shorten(LocalFile));
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    pos = 0;
    while ((c = getc(fp)) != EOF) {
	BigBuf[pos++] = c;
	if (pos >= WRITEFILECHUNK) {
	    mserrcode = MS_StorePartialFile(ViceFile, offset, pos, 0600, TRUE, BigBuf);
	    if (mserrcode) {
		ReportError("Message server cannot store file", ERR_WARNING, TRUE);
		fclose(fp);
		return(-1);
	    }
	    offset += pos;
	    pos = 0;
	}
    }
    if (pos > 0 || offset == 0) {
	mserrcode = MS_StorePartialFile(ViceFile, offset, pos, 0600, TRUE, BigBuf);
	if (mserrcode) {
	    ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	    fclose(fp);
	    return(-1);
	}
    }
    fclose(fp);
    return(0);
}

CUI_GetFileFromVice(LocalFile, ViceFile)
char *LocalFile, *ViceFile;
{
    char    Buf[MAXBODY],
	    ErrorText[256];
    int     bodylen;
    long    offset,
	    bytesunfetched;	/* *** Added for PC  8/20/86  *** */
    FILE *fp;

    debug(1, ("Get local file %s %s\n", LocalFile, ViceFile));
    fp = fopen(LocalFile, "w");
    if (!fp) {
	sprintf(ErrorText, "Could not create the local file %s", ap_Shorten(LocalFile));
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    offset = 0;
    do {
	if ((mserrcode = MS_GetPartialFile(ViceFile, Buf, sizeof(Buf)-1, offset, &bytesunfetched, &bodylen)) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	Buf[bodylen] = '\0';
	fputs(Buf, fp);
	offset += bodylen;
    } while (bytesunfetched > 0);
    if (vfclose(fp)) {
	sprintf(ErrorText, "Could not write out file %s (%d)", ap_Shorten(LocalFile), errno);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (mserrcode || bodylen < 0) {
	sprintf(ErrorText, "The file %s could not be read", ap_Shorten(ViceFile));
	if (offset) strcat(ErrorText, " completely");
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

CUI_RenameDir(old, new)
char *old, *new;
{
    struct CUIDirNode  *DNtmp, *NewDN;
    int status, fullnamehash;
    char *OldName, NewName[1+MAXPATHLEN], ErrorText[256], SaveOldName[1+MAXPATHLEN];

    old = StripWhiteEnds(old);
    new = StripWhiteEnds(new);
    if (!*new) {
	ReportSuccess("No folder is allowed to be nameless.");
	return(-1);
    }
    if ((mserrcode = CUI_DisambiguateDir(old, &OldName)) != 0) {
	CUI_ReportAmbig(old, "folder");
	return(-1);
    }
    CUI_BuildNickName(old, SaveOldName);
    mserrcode = MS_RenameDir(OldName, new, NewName);
    if (mserrcode) {
#ifdef ELOOP
	if (AMS_ERRNO == ELOOP) {
	    ReportSuccess("You cannot make a folder a subfolder of itself!");
	} else
#endif
	if (!strcmp(OldName, NewName)) {
	    ReportSuccess("It is meaningless to rename a folder to itself!");
	} else if (AMS_ERRNO == EINVAL && AMS_ERRCAUSE == EIN_PARAMCHECK && AMS_ERRVIA == EVIA_RENAMEDIR) {
	    char Buf[100+MAXPATHLEN];

	    sprintf(Buf, "Rename failed: '%s' is not a valid folder name", NewName);
	    ReportSuccess(Buf);
	} else {
	    ReportError("Could not rename folder", ERR_WARNING, TRUE);
	}
	return(-1);
    }
    /* Now we need to clean up the cuilib cache entry for this folder */
    DirectoryChangeHook(NewName, OldName, CUI_Rock);
    fullnamehash = dirhashfunc(OldName);

    for (DNtmp = CUIDirCache[fullnamehash%CUIDIRHASHSIZE]; DNtmp != NULL; DNtmp = DNtmp->Next) {
	if (fullnamehash == DNtmp->FullNameHash && !DNtmp->WasDeleted && !strcmp(DNtmp->FullName, OldName)) {
	    DNtmp->WasDeleted = 1;
	    if (status = CUI_SetDirNode(NewName, (char *)0, &NewDN)) {
		return status;
	    }
	    NewDN->NeedsPurging += DNtmp->NeedsPurging;

	    break;
	}
    }

    sprintf(ErrorText, "Renamed %s to be ", ap_Shorten(SaveOldName));
    strcat(ErrorText, ap_Shorten(NewName));
    ReportSuccess(ErrorText);
    return(0);
}

CUI_RemoveDirectory(DirName)
char *DirName;
{
    int ProtCode, MsgCount;
    char ErrorText[256], NickName[1+MAXPATHLEN];

    mserrcode = MS_GetDirInfo(DirName, &ProtCode, &MsgCount);
    if (mserrcode) {
	sprintf(ErrorText, "Cannot get information about folder %s.", ap_Shorten(DirName));
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    if (MsgCount > 0) {
	CUI_BuildNickName(DirName, NickName);
	sprintf(ErrorText, "Folder %s has %s message%s; remove %s", NickName, cvEng(MsgCount, 0, 1000), (MsgCount > 1) ? "s" : "", (MsgCount > 1) ? "them all" : "it");
	if (!GetBooleanFromUser(ErrorText, FALSE)) {
	    return(0);
	}
    }
    mserrcode = MS_RemoveDirectory(DirName, MsgCount);
    if (mserrcode) {
	if (AMS_ERRNO == ENOTEMPTY) {
	    ReportSuccess("Someone apparently added a message to the folder right before you tried to remove it.");
	} else if (AMS_ERRNO == EMSDIRHASKIDS) {
	    ReportSuccess("That folder cannot be deleted while it still has sub-folders inside it.");
	} else {
	    sprintf(ErrorText, "Could not remove folder %s.", ap_Shorten(DirName));
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    DirectoryChangeHook((char *)NULL, DirName, CUI_Rock);
    RemoveFromDirCache(DirName);
    if (MsgCount > 0) {
	sprintf(ErrorText, "Deleted folder %s and all %s messages in it.", ap_Shorten(DirName), cvEng(MsgCount, 0, 1000));
    } else {
	sprintf(ErrorText, "Deleted empty folder %s", ap_Shorten(DirName));
    }
    ReportSuccess(ErrorText);
    return(0);
}

CUI_FreeCaches() {
    struct cuidnode *cdnp, *cdnp2;
    struct CUIDirNode *cdp;
    int i, bucket;

    for (i=0; i<CUIDHASHMAX; ++i) {
	cdnp = AIDHash[i].next;
	while (cdnp) {
	    cdnp2 = cdnp->next;
	    free(cdnp);
	    cdnp = cdnp2;
	}
	AIDHash[i].next = NULL;
	AIDHash[i].index = 0;
    }
    for (bucket = 0; bucket < CUIDIRHASHSIZE; bucket++) {
	cdp = CUIDirCache[bucket];
	while (cdp) {
	    CUIDirCache[bucket] = cdp->Next;
	    free(cdp->NickName);
	    free(cdp->FullName);
	    free(cdp);
	    cdp = CUIDirCache[bucket];
	}
    }
/*    for (i = 0; i<= CUI_CuidsInUse; ++i) { */
	/* Nothing to free -- yet... */
/*    } */
    CUI_CuidsInUse = 0;
    return(0);
}

int CUI_HandleFolderCreationNotice(cuid, Snapshot)
int cuid;
char *Snapshot;
{
    char    NewDir[MAXPATHLEN + 1],
	    Message[100 + MAXPATHLEN],
	    NickName[MAXPATHLEN + 1],
	    NewName[MAXPATHLEN + 1],
	    *DirName;
    int     substatus, saveerr;

    NewDir[0] = '\0';
    if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_DIRECTORYCREATION, NewDir, MAXPATHLEN) != 0) {
    /* error already reported */
	return(-1);
    }
    if (NewDir[0] == '\0') return(0);
    if (CUI_DisambiguateDir(NewDir, &DirName)) {
	if (vdown(AMS_ERRNO)) {
	    DirName = NewDir; /* hope for the best */
	} else {
	    if (AMS_ERRNO == ENOENT) {
		saveerr = mserrcode;
		if (MS_CheckMissingFolder(NewDir, NewName)) {
		    mserrcode = saveerr;
		} else {
		    mserrcode = CUI_DisambiguateDir(NewName, &DirName);
		}
	    }
	    if (AMS_ERRNO == ENOENT) {
		CUI_BuildNickName(NewDir, NickName);
		mserrcode = CUI_DisambiguateDir(NickName, &DirName);
	    }
	    if (mserrcode) {
		if (AMS_ERRNO == ENOENT) {
		    ReportSuccess("This message announces a new bboard, but it has apparently already been deleted.");
		} else if (AMS_ERRNO == EACCES) {
		    ReportSuccess("This is a private bboard and you are not authorized/authenticated to subscribe.");
		} else {
		    sprintf(Message, "Bogus new folder announcement -- cannot subscribe to %s!", ap_Shorten(NewDir));
		    ReportError(Message, ERR_WARNING, TRUE);
		}
		return(-1);
	    }
	}
    }
    if (mserrcode = MS_GetSubscriptionEntry(DirName, NickName, &substatus) != 0) {
	sprintf(Message, "Cannot get subscription information for %s", ap_Shorten(DirName));
	ReportError(Message, ERR_WARNING, TRUE);
	return(-1);
    }
    if (substatus == AMS_UNSUBSCRIBED) {
	if (NickName[0] == '\0') {
	    CUI_BuildNickName(DirName, NickName);
	}
	sprintf(Message, "Subscribe to %s", NickName);
	if (GetBooleanFromUser(Message, FALSE)) {
	    mserrcode = CUI_SetSubscriptionEntry(DirName, NickName, AMS_ALWAYSSUBSCRIBED);
	    if (mserrcode != 0) {
		if (AMS_ERRNO == EACCES) {
		    sprintf(Message, "'%s' is private; you don't have read-access or are unauthenticated.", ap_Shorten(NickName));
		} else if (vdown(AMS_ERRNO)) {
		    sprintf(Message, "%s: temporarily unavailable (net/server problem)", ap_Shorten(NickName));
		} else if (AMS_ERRNO == ENOENT) {
		    sprintf(Message, "Sorry; %s no longer exists, so you cannot subscribe to it.", ap_Shorten(NickName));
		} else {
		    sprintf(Message, "Cannot set subscription entry to %s", ap_Shorten(NickName));
		    ReportError(Message, ERR_WARNING, TRUE);
		    return(-1);
		}
		/* No bug report options here */
		ReportSuccess(Message);
		return(0);
	    } else {
		sprintf(Message, "Subscribed to %s", ap_Shorten(NickName));
		ReportSuccess(Message);
	    }
	} else {
	    ReportSuccess("No subscription added.");
	}
    }
    return(0);
}

static char *WriteIn = "Write-in Vote";

/* the following constant is arbitrary, and wrong */
/* this should be done dynamically */
/* however, if you hit this limit, you'd better have a real small font! */
#define VOTEVECMAX 103

int CUI_HandleVote(cuid, Snapshot)
int cuid;
char *Snapshot;
{
    char *s, *VoteVector[VOTEVECMAX], FileName[1+MAXPATHLEN], ErrorText[1000], VoteChoices[2000], VoteRequest[500], VoteTo[500];
    int VoteVecIndex = 0, ans, ix;
    FILE *fp;

    ReportSuccess("A vote is requested.  Your vote is optional and *NOT* secret.");
    if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_VOTEREQUEST, VoteRequest, sizeof(VoteRequest) - 1) != 0) {
    /* error already reported */
	return(-1);
    }
    if (VoteRequest[0]) {
	/* Process the three current vote headers */
	char *dumstr;

	if (FindQuotedString(VoteRequest, &VoteVector[0], &s) || VoteVector[0] == NULL) {
	    ReportSuccess("Badly-quoted string in vote header");
	    return(-1);
	}
	if (s == NULL) {	/* assume that the vote-ID is missing. */
	    VoteVector[2] = VoteVector[0];
	    VoteVector[0] = "<no vote-ID>";
	} else {
	    if (FindQuotedString(s, &VoteVector[2], &dumstr) || VoteVector[2] == NULL) {
		ReportSuccess("Badly-quoted string in vote header");
		return(-1);
	    }
	}
	if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_VOTETO, VoteTo, sizeof(VoteTo) - 1) != 0) {
	    /* error already reported */
	    return(-1);
	}
	VoteVector[1] = VoteTo;
	if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_VOTECHOICES, VoteChoices, sizeof(VoteChoices) - 1) != 0) {
	    /* error already reported */
	    return(-1);
	}
	s = VoteChoices;
	VoteVecIndex = 2;
	while(s) {
	    if (++VoteVecIndex >= VOTEVECMAX) {
		ReportSuccess("Too many choices in vote header!");
		return(-1);
	    }
	    if (FindQuotedString(s, &VoteVector[VoteVecIndex], &s)) {
		ReportSuccess("Badly-quoted string in vote header");
		return(-1);
	    }
	    if (!strcmp(VoteVector[VoteVecIndex], "*")) {
		VoteVector[VoteVecIndex] = WriteIn;
	    }
	}
    } else { /* look for old version for compatibility */
	if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_OLDVOTE, VoteChoices, sizeof(VoteChoices) - 1) != 0) {
	    /* error already reported */
	    return(-1);
	}
	if (VoteChoices[0]) { /* found old version */
	    VoteVector[0] = VoteChoices;
	    for (s=VoteChoices; *s; ++s) {
		if (*s == ':') {
		    if (*(s+1) == ':') { /* quoted colon */
			*++s = ' ';
			continue;
		    }
		    *s++='\0';
		    if (2 + VoteVecIndex >= VOTEVECMAX) {
			ReportSuccess("Too many choices in vote header!");
			return(-1);
		    }
		    if (!strcmp(VoteVector[VoteVecIndex], "*")) {
			VoteVector[VoteVecIndex] = WriteIn;
		    }
		    VoteVector[++VoteVecIndex] = s;
		}
	    }
	    if (!strcmp(VoteVector[VoteVecIndex], "*")) {
		VoteVector[VoteVecIndex] = WriteIn;
	    }
	} else {
	    ReportSuccess("The message server thinks this is a VOTE message, but I disagree.");
	    return(-1);
	}
	/* Now the vector is all set up, ready to ask query & send answer */
	if (VoteVecIndex < 3) {
	    ReportSuccess("Not enough choices in vote header!");
	    return(-1);
	}
    }
    ix = 0;
    for (ans = 0; ans <= VoteVecIndex; ++ans) {
	if (VoteVector[ans] != NULL) {
	    VoteVector[ans] = StripWhiteEnds(VoteVector[ans]);
	    ReduceWhiteSpace(VoteVector[ans]);
	    /* Omit zero-length vote choices */
	    if (VoteVector[ans][0] != '\0') {
		VoteVector[ix] = VoteVector[ans];
		++ix;
	    }
	}
    }
    VoteVecIndex = ix;
    VoteVector[VoteVecIndex] = "Not Voting";
    VoteVector[++VoteVecIndex] = NULL;
    ans = ChooseFromList(&VoteVector[2], VoteVecIndex - 3);
    if (ans != VoteVecIndex -3) {
#define SHORTVOTE 100
	char *MyVote, MyShortVote[SHORTVOTE+1];

	MyVote = VoteVector[ans+2];
	if (MyVote == WriteIn) {
	    char StringBuf[1000];

	    GetStringFromUser("Please enter your write-in vote:", StringBuf, sizeof(StringBuf), FALSE);
	    if (!StringBuf[0]) {
		ReportSuccess("No vote was registered.");
		return(0);
	    }
	    MyVote = StringBuf;
	}
	CUI_GenLocalTmpFileName(FileName);
	fp = fopen(FileName, "w");
	if (!fp) {
	    ReportError("Could not open local file to compose answer to vote", ERR_WARNING, FALSE);
	    return(-1);
	}
	strncpy(MyShortVote, MyVote, SHORTVOTE);
	MyShortVote[SHORTVOTE] = '\0';
	fprintf(fp, "To: %s\nSubject: %s (My Vote on '%s')\n\nMy vote on '%s' is '%s'.\n", VoteVector[1], MyShortVote, VoteVector[0], VoteVector[0], MyVote);
	fclose(fp);
	if (!CUI_OnSameHost) {
	    char VFileName[1+MAXPATHLEN];

	    CUI_GenTmpFileName(VFileName);
	    if (CUI_StoreFileToVice(FileName, VFileName)) {
		ReportError("Cannot store your vote on AFS", ERR_WARNING, TRUE);
		return(-1);
	    }
	    unlink(FileName);
	    strcpy(FileName, VFileName);
	}
	if (CUI_SubmitMessage(FileName, AMS_SEND_BLINDNO)) {
	    ReportError("Could not mail your vote", ERR_WARNING, TRUE);
	    unlink(FileName);
	    return(-1);
	}
	sprintf(ErrorText, "Mailed your vote (%s) to %s.", VoteVector[ans+2], VoteVector[1]);
	ReportSuccess(ErrorText);
    }
    return(0);
}

int CUI_HandleAckRequest(cuid,Snapshot)
int cuid;
char *Snapshot;
{
    char AckHeader[500], *dir, ErrorText[256];
    char SnapshotBuf[AMS_SNAPSHOTSIZE];

    if (!Snapshot) {
	if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) {
	    return(-1); 	/* Error message already reported */
	}
	Snapshot = SnapshotBuf;
    }

    if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_ACKTO, AckHeader, sizeof(AckHeader) - 1) != 0) {
	/* error already reported */
	return(-1);
    }
    if (GetBooleanFromUser("This message requests a receipt acknowledgement; Send one", TRUE)) {
	char FileName[1+MAXPATHLEN];

	if (CUI_NameReplyFile(cuid, AMS_REPLY_ACK, FileName)) {
	    ReportError("Could not generate acknowledgement message", ERR_WARNING, TRUE);
	    return(-1);
	}
	if (CUI_SubmitMessage(FileName, AMS_SEND_BLINDNO)) {
	    ReportError("Could not mail the acknowledgement", ERR_WARNING, TRUE);
	    unlink(FileName);
	    return(-1);
	}
	ReportSuccess("Acknowledgement sent as requested.");
	AMS_UNSET_ATTRIBUTE(Snapshot, AMS_ATT_RRR);
	if (CUI_AlterSnapshot(cuid, Snapshot, ASS_REPLACE_ATTRIBUTES, &dir)) {
	    if (AMS_ERRNO == EWOULDBLOCK) {
		ReportSuccess("Someone else has the folder locked; the message was not marked as 'acknowledged'.");
	    } else {
		sprintf(ErrorText, "Could not mark message %d as 'acknowledged'", cuid);
		ReportError(ErrorText, ERR_WARNING, TRUE);
	    }
	    return(-1);
	}
    }
    return(0);
}

int CUI_HandleEnclosure(cuid, Snapshot)
int cuid;
char *Snapshot;
{
    char *QVec[5];
    char SnapshotBuf[AMS_SNAPSHOTSIZE];
    int ans, IsFullEnclosure, IsFormatted;

    QVec[0] = "This Message contains an 'enclosure' inside it.  Action?";
    QVec[1] = "Take no action";
    QVec[2] = "Store it in a file";
#ifdef ENABLEFILTERING
    QVec[3] = "Pipe it through a command";
#else /* ENABLEFILTERING */
    QVec[3] = NULL;
#endif /* ENABLEFILTERING */
    QVec[4] = NULL;
    ans = ChooseFromList(QVec, 1);
    if (ans > 1) {
	char Separator[400], TmpFileName[1+MAXPATHLEN], LineBuf[1000], OutFileName[1+MAXPATHLEN];
	int ShouldDelete;
	FILE *rfp, *wfp;
	static char *FormatChoices[] = {
	    "This enclosure has Andrew formatting information",
	    "Use the Formatted version",
	    "Unformat it and use the plain text version",
	    NULL};

	if (ans == 3) {
	    GetStringFromUser("Pipe enclosure through what command: ", OutFileName, sizeof(OutFileName), FALSE);
	} else {
	    GetStringFromUser("Write enclosure into what file: ", OutFileName, sizeof(OutFileName), FALSE);
	}
	if (OutFileName[0] == '\0') return(-1);
	if (OutFileName[0] == '~') {
	    /* This is slightly bogus; the file written here is always
		a local file, but we allow the tilde references to do
		things right for a Vice-connected machine.  If the
		client machine does not have the file system mounted,
		than writing enclosures to files named using tilde-
		references will not work. Ideally, this would all
		get written via the messageserver, but is it really
		worth the effort?  We'll wait and see just how often
		the enclosure feature really is used... */
	    mserrcode = MS_DisambiguateFile(OutFileName, TmpFileName, AMS_DISAMB_FILEMAYBECREATED);
	    if (mserrcode) {
		CUI_ReportAmbig(OutFileName, "file");
		return(-1);
	    }
	    strcpy(OutFileName, TmpFileName);
	}
	if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_ENCLOSURE, Separator, sizeof(Separator) - 1) != 0) {
	    /* error already reported */
	    return(-1);
	}
	if (!Separator[0]) {
	    IsFullEnclosure = TRUE;
	} else {
	    IsFullEnclosure = FALSE;
	}
	strcat(Separator, "\n"); /* To match fgets behavior below */
	if (!Snapshot) {
	    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) {
		return(-1); 	/* Error message already reported */
	    }
	    Snapshot = SnapshotBuf;
	}
	IsFormatted = AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_FORMATTED);
	if (IsFormatted && (ChooseFromList(FormatChoices, 1) == 2)) {
	    char *dir, *id, VFileName[1+MAXPATHLEN]
;
	    if (GetAMSID(cuid, &id, &dir) != 0) {
		ReportError("I've lost track of what message had the enclosure.", ERR_WARNING, FALSE);
		return(-1);
	    }
	    mserrcode = MS_WriteUnscribedBodyFile(dir, id, VFileName);
	    if (mserrcode) {
		ReportError("Could not remove formatting from the message", ERR_WARNING, TRUE);
		return(-1);
	    }
	    CUI_GenTmpFileName(TmpFileName);
	    if (CUI_GetFileFromVice(TmpFileName, VFileName)) {
		ReportError("Could not copy file from AFS", ERR_WARNING, TRUE);
		return(-1);
	    }
	    MS_UnlinkFile(VFileName); /* might as well ignore errors */
	} else {
	    if (CUI_GetBodyToLocalFile(cuid, TmpFileName, &ShouldDelete)) {
		return(-1); /* error reported */
	    }
	}
	rfp = fopen(TmpFileName, "r");
	if (!rfp) {
	    ReportError("Could not open local copy of message file", ERR_WARNING, FALSE);
	    if (ShouldDelete) unlink(TmpFileName);
	    return(-1);
	}
#ifdef ENABLEFILTERING
	if (ans == 3) {
	    extern FILE *popen();
	    wfp = popen(OutFileName, "w");
	} else
#endif /* ENABLEFILTERING */
	    wfp = fopen(OutFileName, "w");
	if (!wfp) {
	    sprintf(Separator, "Could not write out enclosure (%d)", errno);
	    ReportError(Separator, ERR_WARNING, FALSE);
	    if (ShouldDelete) unlink(TmpFileName);
	    fclose(rfp);
	    return(-1);
	}
	while (TRUE) {
	    if (fgets(LineBuf, sizeof(LineBuf), rfp) == NULL) {
		ReportSuccess("Enclosure is improperly formatted");
		fclose(rfp);
		pfclose(wfp, ans == 3);
		if (ShouldDelete) unlink(TmpFileName);
		return(-1);
	    }
	    if (!strcmp(LineBuf, Separator)) break;
	    if (IsFormatted && !IsFullEnclosure) {
		if (!strncmp(LineBuf, "\\begindata", 10)
		    || !strncmp(LineBuf, "\\textdsversion", 14)
		    || !strncmp(LineBuf, "\\template", 9)) {
		    fputs(LineBuf, wfp);
		}
	    }
	}
	/* Found start of the enclosure */
	while (TRUE) {
	    if (fgets(LineBuf, sizeof(LineBuf), rfp) == NULL) {
		if (!IsFullEnclosure) {
		    ReportSuccess("Warning -- no end delimiter on enclosure");
		}
		break;
	    }
	    if (!IsFullEnclosure && !strcmp(LineBuf, Separator)) break;
	    fputs(LineBuf, wfp);
	}
	fclose(rfp);
	if (ShouldDelete) unlink(TmpFileName);
	if (pfclose(wfp, ans == 3)) {
	    sprintf(Separator, "Close of enclosure output failed (%d)", errno);
	    ReportError(Separator, ERR_WARNING, FALSE);
	} else {
	    sprintf(LineBuf, (ans == 3) ? "Filtered enclosure through command ``%s''" : "Wrote file ``%s''", OutFileName);
	    ReportSuccess(LineBuf);
	}
    }
    return(0);
}

long CUI_HandleRedistributionMessage(cuid, Snapshot)
int cuid;
char *Snapshot;
{
    char HeaderBuf[2000], Prompt[2100], *dir;

    if (CUI_GetHeaderContents(cuid, (char *) NULL, HP_REDISTRIBUTION, HeaderBuf, sizeof(HeaderBuf))) {
	return(-1);
    }
    sprintf(Prompt, "Resend this message to '%s'", HeaderBuf);
    if (GetBooleanFromUser(Prompt, FALSE)) {
	if (CUI_ResendMessage(cuid, HeaderBuf)) return(-1);
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_MAYMODIFY)) {
	    AMS_UNSET_ATTRIBUTE(Snapshot, AMS_ATT_REDISTRIBUTION);
	    if (CUI_AlterSnapshot(cuid, Snapshot, ASS_REPLACE_ATTRIBUTES, &dir)) {
		if (AMS_ERRNO == EWOULDBLOCK) {
		    ReportSuccess("Someone else has the folder locked; the message was not marked as 'redistributed'.");
		} else {
		    sprintf(HeaderBuf, "Could not mark message %d as 'redistributed'", cuid);
		    ReportError(HeaderBuf, ERR_WARNING, TRUE);
		}
		return(-1);
	    }
	}
    }
    return(0);
}

static int (*CustomizationProc)() = NULL;
static long CustomizationRock = 0;

CUI_SetHeaderCustomizationProc(p, rock)
int (*p)();
long rock;
{
    CustomizationProc = p;
    CustomizationRock = rock;
}

struct CustomizationHeader {
    char *headername;
    char *filter;
    struct CustomizationHeader *next;
} *FirstCustomizationHeader = NULL;

FillInCustomizationProcs() {
    static int HasFilledIn = FALSE;

    if (!HasFilledIn) {
	char FName[1+MAXPATHLEN], TmpFile[1+MAXPATHLEN], LBuf[1000], *s, *lbuf;
	FILE *fp;
	struct CustomizationHeader *tmpch;

	debug(1, ("Parsing ~/.headmagic\n"));
	if ((mserrcode = MS_DisambiguateFile("~/.headmagic", FName, AMS_DISAMB_FILEEXISTS)) != 0) {
	    if (AMS_ERRNO == ENOENT) {
		HasFilledIn = TRUE;
		return(0);
	    }
	    ReportError("The file ~/.headmagic can not be read", ERR_WARNING, TRUE);
	    return(-1);
	}
	CUI_GenLocalTmpFileName(TmpFile);
	if (CUI_GetFileFromVice(TmpFile, FName)) return(-1);
	fp = fopen(FName, "r");
	if (!fp) {
	    unlink(TmpFile);
	    return(-1);
	}
	while (fgets(LBuf, sizeof(LBuf), fp)) {
	    debug(1, ("Got line %s", LBuf));
	    lbuf = StripWhiteEnds(LBuf);
	    if (!*lbuf) continue;
	    tmpch = (struct CustomizationHeader *) malloc(sizeof(struct CustomizationHeader));
	    if (!tmpch) {
		fclose(fp);
		FreeCustomizationHeaders();
		unlink(TmpFile);
		return(-1);
	    }
	    s = strchr(lbuf, '\n');
	    if (s) *s = '\0';
	    s = strchr(lbuf, ':');
	    if (s) {
		*s++ = '\0';
		s = StripWhiteEnds(s);
	    }
	    lbuf = StripWhiteEnds(lbuf);
	    if (!*lbuf) continue;
	    tmpch->headername = malloc(1+strlen(lbuf));
	    if (!tmpch->headername) {
		fclose(fp);
		free(tmpch);
		FreeCustomizationHeaders();
		unlink(TmpFile);
		return(-1);
	    }
	    strcpy(tmpch->headername, lbuf);
	    LowerStringInPlace(tmpch->headername, strlen(tmpch->headername));
	    if (s && *s) {
		tmpch->filter = malloc(1+strlen(s));
		if (!tmpch->filter) {
		    fclose(fp);
		    free(tmpch->headername);
		    free(tmpch);
		    FreeCustomizationHeaders();
		    unlink(TmpFile);
		    return(-1);
		}
		strcpy(tmpch->filter, s);
	    } else {
		tmpch->filter = NULL;
	    }
	    tmpch->next = FirstCustomizationHeader;
	    FirstCustomizationHeader = tmpch;
	    debug(1, ("Got customization header %s filter %s\n", tmpch->headername, tmpch->filter ? tmpch->filter : ""));
	}
	HasFilledIn = TRUE;
	unlink(TmpFile);
	fclose(fp);
    }
    return(0);
}

FreeCustomizationHeaders() {
    struct CustomizationHeader *oldch, *tmpch = FirstCustomizationHeader;

    while (tmpch) {
	oldch = tmpch->next;
	free(tmpch->headername);
	if (tmpch->filter) free(tmpch->filter);
	free(tmpch);
	tmpch = oldch;
    }
    FirstCustomizationHeader = NULL;
}

void CUI_HandleCustomizationMessage(cuid, Snapshot)
int cuid;
char *Snapshot;
{
    struct CustomizationHeader *tmpch;
    char HeaderBuf[2000], CmdBuf[2000], *s, *CmdVec[3];
    int fds[2], tmpfd, code;
    FILE *fp;

    FillInCustomizationProcs();
    tmpch = FirstCustomizationHeader;
    while (tmpch) {
	if (GetHeaderContents(cuid, tmpch->headername, -1, HeaderBuf, sizeof(HeaderBuf), FALSE)) {
	    return;
	}
	if (HeaderBuf[0]) {
	    if (tmpch->filter) {
#ifndef ENABLEFILTERING
		ReportSuccess("This interface cannot run programs through a filter.");
#else /* ENABLEFILTERING */
		sprintf(CmdBuf, tmpch->filter, HeaderBuf);
		s = strchr(CmdBuf, ' ');
		if (s) *s++ = '\0';
		CmdVec[0] = CmdBuf;
		CmdVec[1] = s;
		CmdVec[2] = NULL;
		if (pipe(fds)) {
		    ReportSuccess("Could not open pipe to filter message");
		    return;
		}
		sprintf(HeaderBuf, "Filtering message through command '%s' arguments '%s'.", CmdVec[0], CmdVec[1] ? CmdVec[1] : "");
		ReportSuccess(HeaderBuf);
		code = osi_vfork();
		if (code <0) {
		    ReportError("Could not fork!", ERR_WARNING, FALSE);
		    return;
		}
		if (code == 0) {
		    /* I am a child.  Take stdin from pipe */
		    dup2(fds[0], 0);
		    for (tmpfd = getdtablesize(); tmpfd > 2; --tmpfd) {
			(void) close(tmpfd);
		    }
		    execv(CmdVec[0], CmdVec);
		    fprintf(stderr, "Could not execute filter!\n");
		    _exit(1);
		}
		close(fds[0]);
		fp = fdopen(fds[1], "w");
		if (!fp) {
		    sprintf(HeaderBuf, "Could not run filter '%s'.", CmdBuf);
		    ReportError(HeaderBuf, ERR_WARNING, FALSE);
		} else {
		    char LocalFile[1+MAXPATHLEN];
		    int shoulddelete;

		    CUI_GenLocalTmpFileName(LocalFile);
		    if (!CUI_GetBodyToLocalFile(cuid, LocalFile, &shoulddelete)) {
			FILE *rfp;
			char LBuf[2000];

			rfp = fopen(LocalFile, "r");
			if (rfp) {
			    while (fgets(LBuf, sizeof(LBuf), rfp)) {
				fputs(LBuf, fp);
			    }
			}
			fclose(rfp);
			if (shoulddelete) unlink(LocalFile);
		    }
		    if (fclose(fp)) {
			sprintf(HeaderBuf, "Filter '%s' apparently failed.", CmdBuf);
			ReportError(HeaderBuf, ERR_WARNING, FALSE);
		    } else {
			sprintf(HeaderBuf, "Filtered message through '%s'.", CmdBuf);
		    }
		}
#endif /* ENABLEFILTERING */
	    } else if (CustomizationProc) {
		(*CustomizationProc)(CustomizationRock, cuid, Snapshot);
	    } else {
		sprintf(HeaderBuf, "This interface cannot specially handle the '%s' header.", tmpch->headername);
		ReportSuccess(HeaderBuf);
	    }
	}
	tmpch = tmpch->next;
    }
}

CUI_ProcessMessageAttributes(cuid, Snapshot)
int cuid;
char *Snapshot;
{
	char SnapshotBuf[AMS_SNAPSHOTSIZE];

	if (!Snapshot) {
	    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) {
		return(-1); 	/* Error message already reported */
	    }
	    Snapshot = SnapshotBuf;
	}

	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_NEWDIR))
	    CUI_HandleFolderCreationNotice(cuid, Snapshot);
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_VOTE))
	    CUI_HandleVote(cuid, Snapshot);
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_RRR) && AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_MAYMODIFY))
	    CUI_HandleAckRequest(cuid, Snapshot);
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_ENCLOSURE))
	    CUI_HandleEnclosure(cuid, Snapshot);
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_REDISTRIBUTION))
	    CUI_HandleRedistributionMessage(cuid, Snapshot);
	CUI_HandleCustomizationMessage(cuid, Snapshot);
	return(0);
}

pfclose(fp, DoPclose)
FILE *fp;
Boolean DoPclose;
{
#ifdef ENABLEFILTERING
    if (DoPclose) {
	return(pclose(fp));
    }
#endif /* ENABLEFILTERING */
    return(vfclose(fp));
}
static Bogus_MakeBodyFileName(dir, id, buf)
char *dir, *id, *buf;
{
    sprintf(buf, "%s/+%s", dir, id);
}

CUI_GetBodyToLocalFile(cuid, FileName, ShouldDelete)
int cuid;
char *FileName;
int *ShouldDelete;
{
    return(CUI_ReallyGetBodyToLocalFile(cuid, FileName, ShouldDelete, TRUE));
}

CUI_ReallyGetBodyToLocalFile(cuid, FileName, ShouldDelete, MayFudge)
int cuid;
char *FileName;
int *ShouldDelete;
int MayFudge;
{
    char *id, *dir, ErrorText[256], BodyBuf[MAXBODY];
    int bodylen;
    long bytesremaining, offset=0;
    FILE *fp;

    if (cuid <=0 || GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    Bogus_MakeBodyFileName(dir, id, FileName);
    if (MayFudge && !access(FileName, R_OK)) {
	if (!CUI_SnapIsRunning) {
	    ConsiderLoggingRead(FileName);
	}
	*ShouldDelete = 0;
	return(0);
    }
    CUI_GenLocalTmpFileName(FileName);
    fp = fopen(FileName, "w");
    if (!fp) {
	ReportError("Cannot open temporary local file for message body", ERR_WARNING, FALSE);
	return(-1);
    }

    do {
	if (CUI_GetPartialBody(BodyBuf, MAXBODY-1, cuid, offset, &bytesremaining, &bodylen)) {
	    fclose(fp);
	    unlink(FileName);
	    return(-1); /* error reported */
	}
	offset += bodylen;
	if (fwriteallchars(BodyBuf, bodylen, fp) != bodylen) {
	    fclose(fp);
	    unlink(FileName);
	    ReportError("Write of local message body file failed", ERR_WARNING, FALSE);
	    return(-1);
	}
    } while (bytesremaining);
    fclose(fp); /* Not on vice, not vfclose */
    *ShouldDelete = 1;
    return(0);
}

CUI_PrefetchMessage(cuid, ReallyNext)
int cuid, ReallyNext;
{
    char ErrorText[256], *id, *dir;

    if (cuid <= 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
#ifdef NOTDEF /* Used to be #ifdef VICE, but I think this is really GARBAGE now  - nsb*/
    if (AMS_ViceIsRunning) {
    /* This is somewhat bogus.  If the client is compiled for VICE, and is
	running on a different vice-connected host from its server, and the client is 
	NOT actually attached to the right Vice file system, then the
	prefetching behavior will be sub-optimal, to say the least.
	However, this code should work well for non-Vice clients
	in any configuration, and for Vice clients which are
	either on the same host as their servers *or* are at least on the
	same Vice as their servers.  A better algorithm should exist...
    */
    if (!CUI_OnSameHost) {
	char FileName[1+MAXPATHLEN];
	struct ViceIoctl blob;

	Bogus_MakeBodyFileName(dir, id, FileName);
	blob.out_size = 0;
	blob.in_size = 0;
	if (pioctl(FileName, _VICEIOCTL(15), &blob)) {
	    AMS_RETURN_ERRCODE(errno, EIN_PIOCTL, EVIA_PREFETCHMSG);
	}
	return(0);
    }
    }
#endif /* NOTDEF  */
    return(MS_PrefetchMessage(dir, id, ReallyNext));
}

CUI_HandleMissingFolder(OldName)
char *OldName;
{
    char  NewNickName[1+MAXPATHLEN],
	  *QVector[10],
	  MessageText[500],
	  NewName[1+MAXPATHLEN],
	  DateBuf[1+AMS_DATESIZE],
	  OldNickName[1+MAXPATHLEN];
    int choice, oldsubstatus, newsubstatus, alreadysubscribed = 0;

    mserrcode = MS_GetSubscriptionEntry(OldName, OldNickName, &oldsubstatus);
    if (mserrcode) {
	ReportError("Cannot check for missing folder subscription information", ERR_WARNING, TRUE);
	return(1);
    }
    if (oldsubstatus == AMS_UNSUBSCRIBED) {
	sprintf(MessageText, "The folder %s does not exist.", ap_Shorten(OldName));
	ReportSuccess(MessageText);
	return(0);
    }
    mserrcode = MS_CheckMissingFolder(OldName, NewName);
    if (mserrcode) {
	ReportError("Cannot check for missing folder change information", ERR_WARNING, TRUE);
	return(2);
    }
    CUI_BuildNickName(OldName, OldNickName);
    if (NewName[0]) {
	/* There is a new version of this thing */
	mserrcode = MS_GetSubscriptionEntry(NewName, NewNickName, &newsubstatus);
	if (mserrcode) {
	    ReportError("Cannot check for new folder subscription information", ERR_WARNING, TRUE);
	    return(3);
	}
	if (newsubstatus != AMS_UNSUBSCRIBED) {
	    /* But we are already subscribed to the new thing */
	    alreadysubscribed = 1;
	}
    }
    if (NewName[0] && !alreadysubscribed) {
	/* There is a new version and we are not yet subscribed */
	CUI_BuildNickName(NewName, NewNickName);
	sprintf(MessageText, "'%s' has been moved to '%s'. Action?", OldNickName, NewNickName);
	QVector[0] = MessageText;
	QVector[1] = "Change your subscription accordingly";
	QVector[2] = "Just delete the subscription entirely";
	QVector[3] = "Do Nothing";
	QVector[4] = NULL;
	choice = ChooseFromList(QVector, 1);
    } else {
	/* It was simply deleted, or we are already subscribed to the new thing */
	if (alreadysubscribed) {
	    sprintf(MessageText, "%s has been merged into %s.  Unsubscribe to %s", OldNickName, NewName, OldNickName);
	} else {
	    sprintf(MessageText, "'%s' no longer exists.  Delete your subscription", OldNickName);
	}
	if (GetBooleanFromUser(MessageText, FALSE)) {
	    choice = 2;
	} else {
	    choice = 0;
	}
    }
    switch(choice) {
	case 1:
	    mserrcode = MS_GetAssociatedTime(OldName, DateBuf, sizeof(DateBuf));
	    if (!mserrcode) {
		mserrcode = MS_SetAssociatedTime(NewName, DateBuf);
	    }
	    if (mserrcode) { /* NOT just an ELSE clause */
		ReportSuccess("Cannot set profile entry; you may see some duplicates");
	    }
	    mserrcode = CUI_SetSubscriptionEntry(NewName, NewNickName, oldsubstatus);
	    if (mserrcode) {
		sprintf(MessageText, "Cannot set new subscription entry for %s; sorry!", ap_Shorten(NewNickName));
		ReportError(MessageText, ERR_WARNING, TRUE);
		return(4);
	    }
	    DirectoryChangeHook(NewName, (char *)NULL, CUI_Rock);
	    sprintf(MessageText, "You are now subscribed to %s.", ap_Shorten(NewNickName));
	    ReportSuccess(MessageText);
	    /* drop through */
	case 2:
	    mserrcode = CUI_SetSubscriptionEntry(OldName, OldNickName, AMS_UNSUBSCRIBED);
	    if (mserrcode) {
		sprintf(MessageText, "Cannot unsubscribe to old folder %s; sorry!", ap_Shorten(OldNickName));
		ReportError(MessageText, ERR_WARNING, TRUE);
		return(5);
	    }
	    DirectoryChangeHook((char *)NULL, OldName, CUI_Rock);
	    if (choice != 1) {
		sprintf(MessageText, "You are now unsubscribed from %s.", ap_Shorten(OldNickName));
		ReportSuccess(MessageText);
	    }
	    break;
	default:
	    break;
    }
    return(0);
}

static char ResendFormat[] = "Message Re-Sent to '%s'.";

static char *ExtVec[] = {
    "The readers of this message may not recognize Andrew formatting.",
    "Cancel resending", /* 3 -> 1 */
    "Remove formatting & send", /* 2 */
    "Send with formatting",  /* 1 -> 3 */
    NULL,
    NULL
};

CUI_ResendMessage(cuid, Tolist)
int cuid;
char *Tolist;
{
    char *NewToList, *Message;
    char *id, *dir, ErrorText[256], BodyBuf[MAXBODY], FileName[1+MAXPATHLEN];
    int bodylen, total = 0, external = 0, FormatFlag = 0, formatct = 0, stripct =  0, trustct = 0;
    long bytesremaining, offset=0;
    FILE *fp;
    char SnapshotBuf[AMS_SNAPSHOTSIZE];

    if (cuid <=0 || GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) {
	return(-1); 	/* Error message already reported */
    }
    if (CUI_RewriteHeaderLineInternal(Tolist, &NewToList, 25, &total, &external, &formatct, &stripct, &trustct)) {
	return(-1);
    }
    if ((external > 0) && AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_FORMATTED)) {
	int ans;
	char HeaderBuf[250];

	FormatFlag = AMS_SEND_INSISTFORMATTED; /* default for non-andrew content-type */
	if (!GetHeaderContents(cuid, "content-type", -1, HeaderBuf, sizeof(HeaderBuf), FALSE)) {
	    char *s = HeaderBuf;
	    while (*s && isspace(*s)) ++s;
	    if (!lc2strncmp ("x-be2", HeaderBuf, 5)) {
	if (external == formatct) {
	    FormatFlag = AMS_SEND_INSISTFORMATTED;
	} else if (external == trustct) {
	    FormatFlag = AMS_SEND_INSISTTRUST;
	} else if ((external == stripct) && (external == total)) {
	    FormatFlag = AMS_SEND_UNFORMATTED;
	} else {
		    ExtVec[4] = (CUI_UseAmsDelivery >= 0) ? "Trust the delivery system to remove it as needed" : NULL;
	    ans = ChooseFromList(ExtVec, 1);
	    switch(ans) {
		case 2:
		    FormatFlag = AMS_SEND_UNFORMATTED;
		    break;
		case 3:
		    FormatFlag = AMS_SEND_INSISTFORMATTED;
		    break;
		case 4:
		    FormatFlag = AMS_SEND_INSISTTRUST;
		    break;
		default:
		    free(NewToList);
		    return(-1);
		    }
		}
	    }
	}
    }
    CUI_GenLocalTmpFileName(FileName);
    fp = fopen(FileName, "w");
    if (!fp) {
	ReportError("Cannot open temporary local file for message body", ERR_WARNING, FALSE);
	free(NewToList);
	return(-1);
    }
    fprintf(fp, "ReSent-To: %s\n", NewToList);
    do {
	if (CUI_GetPartialBody(BodyBuf, MAXBODY-1, cuid, offset, &bytesremaining, &bodylen)) {
	    fclose(fp);
	    free(NewToList);
	    unlink(FileName);
	    return(-1); /* error reported */
	}
	offset += bodylen;
	if (fwriteallchars(BodyBuf, bodylen, fp) != bodylen) {
	    fclose(fp);
	    free(NewToList);
	    unlink(FileName);
	    ReportError("Write of local message body file failed", ERR_WARNING, FALSE);
	    return(-1);
	}
    } while (bytesremaining);
    fclose(fp); /* Not on vice, not vfclose */
    if (!CUI_OnSameHost) {
	char VFileName[1+MAXPATHLEN];

	CUI_GenTmpFileName(VFileName);
	if (CUI_StoreFileToVice(FileName, VFileName)) {
	    ReportError("Cannot store re-sent message on AFS", ERR_WARNING, TRUE);
	    free(NewToList);
	    return(-1);
	}
	unlink(FileName);
	strcpy(FileName, VFileName);
    }
    if (CUI_SubmitMessage(FileName, AMS_SEND_BLINDNO | AMS_SEND_ISRESEND | FormatFlag)) {
	unlink(FileName);
	free(NewToList);
	if ((AMS_ERRNO == EMSNONASCIIMAIL) || (AMS_ERRNO == EMSLONGLINES)) {
	    /* The user declined to send anyway; don't report an error */
	    return(0);
	}
	ReportError("Resend failed", ERR_WARNING, TRUE);
	return(-1);
    }
    Message = malloc(sizeof(ResendFormat)+strlen(NewToList));
    if (Message) {
	sprintf(Message, ResendFormat, NewToList);
	ReduceWhiteSpace(Message);
	ReportSuccess(Message);
	free(Message);
    } else {
	ReportSuccess("Message Re-Sent.");
    }
    free(NewToList);
    return(0);
}

CUI_ReportAmbig(name, atype)
char *name, *atype;
{
    char ErrorText[1000];

    sprintf(ErrorText, "There is no %s named '%s'.", atype, ap_Shorten(name));
    if (AMS_ERRNO == ENOENT) {
	ReportSuccess(ErrorText);
    } else {
	ReportError(ErrorText, ERR_WARNING, TRUE);
    }
}

CUI_GetProfileString(prog, pref, ValBuf, lim)
char *prog, *pref, *ValBuf;
int lim;
{
    char DumIn[1];
    int dummy;

    DumIn[0] = '\0';
    mserrcode = MS_HandlePreference(prog, pref, DumIn, ValBuf, lim, AMS_GETPROFILESTRING, &dummy, 0);
    if (mserrcode) {
	ReportError("Could not get preference from messageserver", ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}


CUI_GetProfileInt(prog, pref, def)
char *prog, *pref;
int def;
{
    char DumIn[1], DumOut[1];
    int intval;

    DumIn[0] = '\0';
    mserrcode = MS_HandlePreference(prog, pref, DumIn, DumOut, 1, AMS_GETPROFILEINT, &intval, def);
    if (mserrcode) {
	ReportError("Could not get preference from messageserver", ERR_WARNING, TRUE);
	return(def);
    }
    return(intval);
}



CUI_GetProfileSwitch(prog, pref, def)
char *prog, *pref;
int def;
{
    char DumIn[1], DumOut[1];
    int intval;

    DumIn[0] = '\0';
    mserrcode = MS_HandlePreference(prog, pref, DumIn, DumOut, 1, AMS_GETPROFILESWITCH, &intval, def);
    if (mserrcode) {
	ReportError("Could not get preference from messageserver", ERR_WARNING, TRUE);
	return(def);
    }
    return(intval);
}



CUI_SetProfileString(prog, pref, val)
char *prog, *pref, *val;
{
    char DumOut[1];
    int dummy;

    mserrcode = MS_HandlePreference(prog, pref, val, DumOut, 1, AMS_SETPROFILESTRING, &dummy, 0);
    if (mserrcode) {
	ReportError("Could not get preference from messageserver", ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

long
CUI_SetSubscriptionEntry(Name, NickName, status)
char *Name, *NickName;
int status;
{
    mserrcode = MS_SetSubscriptionEntry(Name, NickName, status);
    if (!mserrcode) {
	SubscriptionChangeHook(Name, NickName, status, CUI_Rock);
    }
    return(mserrcode);
}

long
CUI_MergeDirectories(FromDir, ToDir)
char *FromDir, *ToDir;
{
    mserrcode = MS_MergeDirectories(FromDir, ToDir);
    if (!mserrcode) {
	DirectoryChangeHook((char *)NULL, FromDir, CUI_Rock);
    }
    return(mserrcode);
}

/* The following routine looks for a series of strings, possibly quoted,
    separated by commas.  It will remove the quoting where necessary, and
    returns a pointer to the dequoted first item and the (still-quoted)
    remainder.  It should be noted that it may be possible that
    (*remainder == source), so the implementation must allow this.
*/

FindQuotedString(source, first, remainder)
char *source, **first, **remainder;
{
    char *sdum, *sdum2;

    while (isspace(*source)) ++source;
    if (*source != '"') {
	*first = source;
	sdum = strchr(source, ',');
	if (sdum) {
	    *sdum++ = '\0';
	    *remainder = sdum;
	} else {
	    *remainder = NULL;
	}
	return 0;
    }
    /* OK, here it is quoted, sigh... */
    *first = ++source;  /* the easy part */
    for (sdum = source; *sdum; ++sdum) {
	if (*sdum == '\\') {
	    for (sdum2 = sdum; *sdum2; ++sdum2) {
		*sdum2 = *(sdum2+1);
	    }
	    continue;
	}
	if (*sdum == '"') {
	    *sdum++ = '\0';
	    sdum2 = strchr(sdum, ',');
	    if (sdum2) {
		*remainder = ++sdum2;
	    } else {
		*remainder = NULL;
	    }
	    return 0;
	}
    }
    /* Quotation never ended! */
    return(-1);
}

PutStringToViceFile(ViceFile, text)
char *ViceFile, *text;
{
    int bytesleft, writelen, offset = 0;

    bytesleft = strlen(text)+1; /* The +1 guarantees that we copy something */

    while ( bytesleft ) {
	if (bytesleft > WRITEFILECHUNK) {
	    writelen = WRITEFILECHUNK;
	} else {
	    writelen = --bytesleft;
	}
	mserrcode = MS_StorePartialFile(ViceFile, (long)offset, writelen, 0600, TRUE, text);
	if (mserrcode) {
	    ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	    return(-1);
	}
	text += writelen;
	offset += writelen;
	bytesleft -= writelen;
    }
    return(0);
}

CUI_SetAttribute(cuid, attname)
int cuid;
char *attname;
{
    return(CUI_FixAttribute(cuid, attname, TRUE));
}

CUI_UnsetAttribute(cuid, attname)
int cuid;
char *attname;
{
    return(CUI_FixAttribute(cuid, attname, FALSE));
}

CUI_FixAttribute(cuid, attname, Set)
int cuid;
char *attname;
Boolean Set;
{
    char *id, *dir, Attrs[1+(AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX))], *s, *t, ErrorText[256];
    int AttrCt, i, TargetAttr = -1;
    
    if (strlen(attname) > AMS_ATTRNAMEMAX) {
	sprintf(ErrorText, "'%s' is too long for an attribute name.  (The limit is %d characters.)", attname, AMS_ATTRNAMEMAX);
	ReportSuccess(ErrorText);
	return(-1);
    }
    if (cuid <=0 || GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    mserrcode = MS_GetDirAttributes(dir, &AttrCt, Attrs, '\001', TRUE);
    if (mserrcode) {
	ReportError("Could not get attribute information", ERR_WARNING, TRUE);
	return(-1);
    }
    for (s=Attrs, i=0; s && i<AttrCt; ++i) {
	t = strchr(s, '\001');
	if (!t) break;
	*t++ = '\0';
	if (!strcmp(s, attname)) {
	    TargetAttr = i;
	    break;
	}
	s = t;
    }
    if (TargetAttr < 0) {
	sprintf(ErrorText, "There is no such attribute as %s.  Create it", attname);
	if (!GetBooleanFromUser(ErrorText, FALSE)) {
	    return(-1);
	}
	mserrcode = MS_AddAttribute(dir, attname, &TargetAttr);
	if (mserrcode) {
	    ReportError("Could not create new user-defined attribute", ERR_WARNING, TRUE);
	    return(-1);
	}
    }
    return(CUI_FixAttributeByNumber(cuid, TargetAttr, Set));
}

CUI_FixAttributeByNumber(cuid, attnum, Set)
int cuid, attnum;
Boolean Set;
{
    char SnapshotBuf[AMS_SNAPSHOTSIZE], *dir;

    debug(1, ("Fix attribute by number cuid %d attnum %d set %d\n", cuid, attnum, Set));
    if (attnum < 0 || attnum > AMS_NUM_UATTRS) {
	ReportError("Attribute number out of range", ERR_WARNING, TRUE);
	return(-1);
    }
    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) {
	return(-1); 	/* Error message already reported */
    }
    if (Set) {
	AMS_SET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UATTR(attnum));
    } else {
	AMS_UNSET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UATTR(attnum));
    }
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_REPLACE_ATTRIBUTES, &dir)) {
	return(-1); /* ditto */
    }
}

CUI_GetAttrName(dir, which, buf)
char *dir, *buf;
int which;
{
    char Attrs[1+(AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX))], *s, *t;
    int AttrCt, i;

    mserrcode = MS_GetDirAttributes(dir, &AttrCt, Attrs, '\001', TRUE);
    if (mserrcode || (AttrCt < (which+1))) {
	ReportError("Could not get attribute information", ERR_WARNING, mserrcode ? TRUE : FALSE);
	return(-1);
    }
    for (s=Attrs, i=0; s && i<AttrCt; ++i) {
       t = strchr(s, '\001');
       if (t) *t++ = '\0';
       if (i == which) {
	   strcpy(buf, s);
	   return(0);
       }
       s = t;
    }
    ReportError("Could not get attribute information", ERR_WARNING, FALSE);
    return(-1);
}

CUI_CopyViceFile(FromFile, ToFile)
char *FromFile, *ToFile;
{
    return(CUI_CopyViceFileTails(FromFile, 0L, ToFile, 0L));
}

CUI_CopyViceFileTails(FromFile, FromSkip, ToFile, ToSkip)
char *FromFile, *ToFile;
long FromSkip, ToSkip;
{
    int bodylen;
    char ErrorText[256], Buf[WRITEFILECHUNK+1];
    long    bytesunfetched;	/* *** Added for PC  8/20/86  *** */

    do {
	mserrcode = MS_GetPartialFile(FromFile, Buf, sizeof(Buf)-1, FromSkip, &bytesunfetched, &bodylen);
	if (mserrcode || (bodylen <= 0)) break;
	Buf[bodylen] = '\0';
	mserrcode = MS_StorePartialFile(ToFile, ToSkip, bodylen, 0600, TRUE, Buf);
	if (mserrcode) break;
	FromSkip += bodylen;
	ToSkip += bodylen;
    } while (bytesunfetched > 0);
    if (mserrcode || bodylen < 0) {
	sprintf(ErrorText, "Could not copy file %s to", ap_Shorten(FromFile));
	strcat(ErrorText, ap_Shorten(ToFile));
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

CUI_MarkRepliedTo(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    bzero(SnapshotBuf, AMS_SNAPSHOTSIZE);
    AMS_SET_ATTRIBUTE(SnapshotBuf, AMS_ATT_REPLIEDTO);
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, ASS_OR_ATTRIBUTES, &dir)) {
	if (AMS_ERRNO == EACCES) return(0); /* Why complain?  Easy out for bboards */
	sprintf(ErrorText, "Could not mark message %d as `replied to'", cuid);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

CUI_FlagUrgency(cuid, urgency)
int	cuid;
int urgency;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE],
	   *dir,
	    ErrorText[256];

    if (urgency) {
	bzero(SnapshotBuf, AMS_SNAPSHOTSIZE);
	AMS_SET_ATTRIBUTE(SnapshotBuf, AMS_ATT_URGENT);
    } else {
	bone(SnapshotBuf, AMS_SNAPSHOTSIZE);
	AMS_UNSET_ATTRIBUTE(SnapshotBuf, AMS_ATT_URGENT);
    }
    if (CUI_AlterSnapshot(cuid, SnapshotBuf, urgency ? ASS_OR_ATTRIBUTES : ASS_AND_ATTRIBUTES, &dir)) {
	sprintf(ErrorText, "Could not %sark message %d as 'urgent'", urgency ? "M" : "Unm", cuid);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

#define	VALIDATE_NO_VALIDATE	    0x00    /* don't validate this field */
#define	VALIDATE_AS_ADDRESS	    0x01    /* this field contain addresses */
#define	VALIDATE_AS_DIRECTORY	    0x02    /* this field contain directory */
#define	VALIDATE_AS_REFORMAT	    0x04    /* just reformt this field */

typedef struct need_validation_struct {
    char    *headerName;
    int	     headerLen;
    Boolean  validationType;
    Boolean  sawHeader;
    Boolean  required;
} NEEDS_VALIDATION;

NEEDS_VALIDATION VF_headers[] = {
    "To:",			    0, VALIDATE_AS_ADDRESS,    0, TRUE,
    "Cc:",			    0, VALIDATE_AS_ADDRESS,    0, FALSE,
    "Ack-To:",			    0, VALIDATE_AS_ADDRESS,    0, FALSE,
    "Vote-To:",			    0, VALIDATE_AS_ADDRESS,    0, FALSE,
    "X-Andrew-DirectoryCreation:",  0, VALIDATE_AS_DIRECTORY,  0, FALSE,
    "X-Andrew-Redistribution-To:",  0, VALIDATE_AS_ADDRESS,    0, FALSE,
    NIL,			    0, VALIDATE_NO_VALIDATE,   0, FALSE
};

ValidateAndWriteFile(AwaitingValidation, VFidx, OutFileName, offset_out, VEs, VDEs)
char *AwaitingValidation, *OutFileName;
int   VFidx;
long *offset_out;
int  *VEs, *VDEs;
{
    char *result;

    if (VF_headers[VFidx].validationType == VALIDATE_AS_DIRECTORY)
	*VDEs += ValidateDirname(AwaitingValidation, &result);
    else
	*VEs += CUI_RewriteHeaderLine(AwaitingValidation, &result);

    debug(2, ("Validated as: %s\n",result));

    if (result) {
	char *tmpresult;
	tmpresult = StripWhiteEnds(result); /* ensure no trailing newline */
	if (tmpresult && *tmpresult) {
	    if ((VF_headers[VFidx].validationType == VALIDATE_AS_DIRECTORY) && 
		(strlen(tmpresult) > (79 - VF_headers[VFidx].headerLen)))
		OutputLine(OutFileName, offset_out, "\n ");

	    OutputLine(OutFileName, offset_out, tmpresult);
	}
	free(result);
    }
    OutputLine(OutFileName, offset_out, "\n");
}

CUI_ValidateFile (InFileName, OutFileName)
char *InFileName, *OutFileName;
{
    int     bodylen, bytesleft = 0, ValidationErrors = 0, ValidDirErrors = 0, VFidx;
    long    offset_out = 0, offset_in = 0, bytesunfetched;
    char    BodyBuf[WRITEFILECHUNK+1], *UsefulStart = NULL, *searched, *errtxt;
    char    BigBuf[WRITEFILECHUNK], *AwaitingValidation = NULL;
    Boolean InHeaders = TRUE, in_tocc = FALSE;

    debug(1, ("Validating message from %s\n", InFileName));

    for (VFidx=0; VF_headers[VFidx].headerName != NIL; VFidx++) {
	VF_headers[VFidx].sawHeader = 0;
	VF_headers[VFidx].headerLen = strlen(VF_headers[VFidx].headerName);
    }

    CUI_GenTmpFileName(OutFileName);
    do {
	if ((mserrcode = MS_GetPartialFile(InFileName, BodyBuf+bytesleft,
					   WRITEFILECHUNK-bytesleft, offset_in, &bytesunfetched, &bodylen))
	    != 0) {
	    ReportError("Cannot read draft mail being validated.", ERR_WARNING, TRUE);
	    MS_UnlinkFile(OutFileName); /* ignore errors */
	    OutFileName[0] = '\0';
	    return(-1);
	}
	debug(2,("Read %d bytes at %ld with %ld to go.\n",bodylen, offset_in,
	      bytesunfetched));
	if (bodylen <= 0) break;
	offset_in += bodylen;
	UsefulStart=BodyBuf;
	BodyBuf[bodylen+bytesleft] = '\0';
	bytesleft=0;
	while (InHeaders && UsefulStart!=NIL) {
	    searched=strchr(UsefulStart,'\n');
	    if (searched!=NIL) {
		*searched++='\0';
	    } else if (bytesunfetched>0) {
		strcpy(BodyBuf, UsefulStart);
		bytesleft=strlen(BodyBuf);
		break;
	    }
	    if (in_tocc) {
		if (*UsefulStart == ' ' || *UsefulStart == '\t') {
		    AwaitingValidation = realloc(AwaitingValidation, strlen(UsefulStart) +
						 strlen(AwaitingValidation) + 2);
		    if (!AwaitingValidation) {
			MS_UnlinkFile(OutFileName); /* ignore errors */
			OutFileName[0] = '\0';
			return(-1);
		    }
		    strcat(AwaitingValidation, UsefulStart);
		} else {
		    ValidateAndWriteFile(AwaitingValidation, VFidx, OutFileName, &offset_out,
					 &ValidationErrors, &ValidDirErrors);
		    free(AwaitingValidation);
		    AwaitingValidation = NULL;
		    in_tocc = FALSE;
		}
	    }
	    if (!in_tocc) { /* NOT an else clause -- side effect on in_tocc above */
		for (VFidx=0; VF_headers[VFidx].headerName != NIL; VFidx++) {
		    strcpy(BigBuf, VF_headers[VFidx].headerName);
		    LowerStringInPlace(BigBuf, VF_headers[VFidx].headerLen);
		    if (!lc2strncmp(BigBuf, UsefulStart, VF_headers[VFidx].headerLen)) {
			OutputLine(OutFileName, &offset_out, VF_headers[VFidx].headerName);
			OutputLine(OutFileName, &offset_out, " ");
			VF_headers[VFidx].sawHeader++;
			in_tocc = TRUE;
			break;
		    }
		}
		if (in_tocc) {
		    AwaitingValidation = malloc(strlen(UsefulStart));
		    if (!AwaitingValidation) {
			MS_UnlinkFile(OutFileName); /* ignore errors */
			OutFileName[0] = '\0';
			return(-1);
		    }
		    strcpy(AwaitingValidation, UsefulStart+VF_headers[VFidx].headerLen);
		} else {
		    OutputLine(OutFileName, &offset_out, UsefulStart);
		    OutputLine(OutFileName, &offset_out, "\n");
		}
	    }
	    UsefulStart=searched;
	    if (searched && (*searched == '\n')) {
		InHeaders = FALSE;
		if (in_tocc) {
		    if (AwaitingValidation) {
			ValidateAndWriteFile(AwaitingValidation, VFidx, OutFileName,
					     &offset_out,&ValidationErrors, &ValidDirErrors);
			free(AwaitingValidation);
			AwaitingValidation = NULL;
		    }
		    in_tocc = FALSE;
		}
	    }
	}
	if (bytesleft>0) continue;
	if (!UsefulStart) break;
	OutputLine(OutFileName, &offset_out, UsefulStart);
    } while (bytesunfetched > 0);
    if (bytesleft>0 && UsefulStart!=NIL) {
	OutputLine(OutFileName, &offset_out, UsefulStart);
	OutputLine(OutFileName, &offset_out, "\n");
    }
    if (in_tocc) {
	InHeaders = TRUE;
	if (AwaitingValidation) free(AwaitingValidation);
    }
    if (InHeaders) {
	errtxt = "Please leave a blank line before the start of your message text.";
    } else {
	errtxt = BigBuf;
	for (VFidx=0; VF_headers[VFidx].headerName != NIL; VFidx++) {
	    if (VF_headers[VFidx].sawHeader > 1) {
		sprintf(BigBuf, "You have more than one '%s' line.", 
			VF_headers[VFidx].headerName);
		break;
	    }
	    if (VF_headers[VFidx].required && !VF_headers[VFidx].sawHeader) {
		sprintf(BigBuf, "You have not included a '%s' line.", 
			VF_headers[VFidx].headerName);
		break;
	    }
	}
	if (VF_headers[VFidx].headerName == NIL) {
	    if (ValidationErrors)
		sprintf(BigBuf,  "%d of the names you entered %s invalid.",
			ValidationErrors, (ValidationErrors == 1 ? "is" : "are"));
	    else if (ValidDirErrors)
		sprintf(BigBuf,  "%d of the directories you entered %s invalid.",
			ValidDirErrors, (ValidDirErrors == 1 ? "is" : "are"));
	    else
		return(0);	/* everything seems to be ok */
	}
    }

    ReportError(errtxt, ERR_WARNING, FALSE);
    return(-1);
}

static OutputLine (fname, offset, buffer)
char *fname, *buffer;
long *offset;
{
    int bodylen;

    bodylen=strlen(buffer);
    mserrcode=MS_StorePartialFile(fname, *offset, bodylen, 0644, TRUE, buffer);
    if (mserrcode) {
	ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	return(-1);
    }
    *offset += bodylen;
    return(0);
}

static ValidateDirname(dirname, result)
char *dirname, **result;
{
    int len;
    char *result2;

    *result = NIL;
    dirname = StripWhiteEnds(dirname);
    if (!dirname || !*dirname) return(0);

    len = strlen(dirname) + 1;
    *result = malloc((len > MAXPATHLEN+2)? len+2 : MAXPATHLEN+2);
    if (!*result) return(-1);

    if ((strchr(dirname, ',') != NIL) || (strlen(dirname) > MAXPATHLEN)) {
	strcpy(*result, dirname);
	return(1);
    }

    if (*dirname == '/') {/* already expanded? */
	mserrcode = MS_DisambiguateFile(dirname, *result, AMS_DISAMB_DIREXISTS);
    }
    else { /* expand it */
	mserrcode = CUI_DisambiguateDir(dirname, &result2);
	if (!mserrcode) {
	    free(*result);
	    *result = result2;
	}
    }

    if (mserrcode) {
	strcpy(*result, dirname);
	return(1);
    }

    return(0);
}
