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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/ams.c,v 1.46 1994/03/31 15:58:50 rr2b Exp $";
#endif

/* Until I come up with a better scheme, new functions here have to be added to SIX files -- ams.ch, amss.ch, amsn.ch (all identical specs) and the corresponding c files */ 

#include <andrewos.h>
#include <sys/param.h>
#include <util.h>
#include <ctype.h>
#include <errprntf.h>
#include <class.h>
#include <cui.h>
#include <fdphack.h>
#include <ams.eh>
#ifdef SNAP_ENV
#include <amss.ih>
#endif /* SNAP_ENV */
#include <amsn.ih>
#include <cursor.ih>
#include <im.ih>
#include <event.ih>
#include <frame.ih>
#include <view.ih>
#include <rect.h>
#include <sendmsg.ih>
#include <message.ih>
#include <text822v.ih>
#include <captions.ih>
#include <folders.ih>
#include <environ.ih>
#include <amsutil.ih>
#include <init.ih>

static int IWantSnap = 0;
int RestartTimer();

void ams__RemoveErrorDialogWindow(self)
struct ams *self;
{
}

void ams__SetWantSnap(c, wantsnap)
struct classheader *c;
int wantsnap;
{
    IWantSnap = wantsnap;
}

static long MyRock;

void ams__SetCUIRock(c, r)
struct classheader *c;
long r;
{
    MyRock = r;
}

static struct ams *myamsp=NULL;

struct ams *ams__GetAMS(c)
struct classheader *c;
{
    if(myamsp==NULL) {
	myamsp=ams_MakeAMS();
	return myamsp;
    }
    return(myamsp);
}

struct ams *ams__MakeAMS(c)
struct classheader *c;
{
    if (!myamsp) {
	message_DisplayString(NULL, 10, "Loading message server libraries...");
	im_ForceUpdate();
#ifdef SNAP_ENV
	if (IWantSnap) {
	    myamsp = (struct ams *) amss_New();
	} else {
	    myamsp = (struct ams *) amsn_New();
	}
#else /* SNAP_ENV */
	if (IWantSnap) {
	    fprintf(stderr, "No separate message server available: SNAP_ENV was not set at system build time.\n");
	    exit(-1);
	}
	myamsp = (struct ams *) amsn_New();
#endif /* SNAP_ENV */
	if (!myamsp || ams_CUI_Initialize(myamsp, NULL, MyRock)) {
	    if(myamsp) {
		ams_ReportError(myamsp, "Error initializing; program terminated.", ERR_FATAL, FALSE, 0);
	    } else fprintf(stderr, "Error initializing; program terminated.");
	    exit(-1); /* not reached */
	}
    }
    return myamsp;
}
static void ReportMissing(s)
char *s;
{
    fprintf(stderr, "The %s function is not included in the skeleton ams class.\n", s);
}

void ams__CUI_BuildNickName(self, shortname, longname)
struct ams *self;
char *shortname, *longname;
{
    ReportMissing("CUI_BuildNickName");
}

int ams__CUI_CheckMailboxes(self, forwhat) 
struct ams *self;
char *forwhat;
{
    ReportMissing("CUI_CheckMailboxes");
    return(0);
}

long ams__CUI_CloneMessage(self, cuid, DirName, code) 
struct ams *self;
int cuid;
char *DirName;
int code;
{
    ReportMissing("CUI_CloneMessage");
    return(0);
}

long ams__CUI_CreateNewMessageDirectory(self, dir, bodydir) 
struct ams *self;
char *dir, *bodydir;
{
    ReportMissing("CUI_CreateNewMessageDirectory");
    return(0);
}

long ams__CUI_DeleteMessage(self, cuid) 
struct ams *self;
int cuid;
{
    ReportMissing("CUI_DeleteMessage");
    return(0);
}

long ams__CUI_DeliveryType(self) 
struct ams *self;
{
    ReportMissing("CUI_DeliveryType");
    return(0);
}

long ams__CUI_DirectoriesToPurge(self) 
struct ams *self;
{
    ReportMissing("CUI_DirectoriesToPurge");
    return(0);
}

long ams__CUI_DisambiguateDir(self, shortname, longname) 
struct ams *self;
char *shortname, *longname;
{
    ReportMissing("CUI_DisambiguateDir");
    return(0);
}

long ams__CUI_DoesDirNeedPurging(self, name) 
struct ams *self;
char *name;
{
    ReportMissing("CUI_DoesDirNeedPurging");
    return(0);
}

void ams__CUI_EndConversation(self)
struct ams *self;
{
    ReportMissing("CUI_EndConversation");
}

long ams__CUI_GenLocalTmpFileName(self, name) 
struct ams *self;
char *name;
{
    ReportMissing("CUI_GenLocalTmpFileName");
    return(0);
}

long ams__CUI_GenTmpFileName(self, name) 
struct ams *self;
char *name;
{
    ReportMissing("CUI_GenTmpFileName");
    return(0);
}

long ams__CUI_GetFileFromVice(self, tmp_file, vfile) 
struct ams *self;
char *tmp_file, *vfile;
{
    ReportMissing("CUI_GetFileFromVice");
    return(0);
}

long ams__CUI_GetHeaderContents(self, cuid, hdrname, hdrnum, hdrbuf, lim)
struct ams *self;
int cuid;
char *hdrname;
int hdrnum;
char *hdrbuf;
int lim;
{
    ReportMissing("CUI_GetHeaderContents");
    return(0);
}

long ams__CUI_GetHeaders(self, dirname, date64, headbuf, lim, startbyte, nbytes, status, RegisterCuids)
struct ams *self;
char *dirname, *date64, *headbuf;
int lim, startbyte, *nbytes, *status, RegisterCuids;
{
    ReportMissing("CUI_GetHeaders");
    return(0);
}

long ams__CUI_GetSnapshotFromCUID(self, cuid, Sbuf) 
struct ams *self;
int cuid;
char *Sbuf;
{
    ReportMissing("CUI_GetSnapshotFromCUID");
    return(0);
}

long ams__CUI_HandleMissingFolder(self, dname) 
struct ams *self;
char *dname;
{
    ReportMissing("CUI_HandleMissingFolder");
    return(0);
}

long ams__CUI_Initialize(self, TimerFunction, rock) 
struct ams *self;
int (*TimerFunction)();
char *rock;
{
    ReportMissing("CUI_Initialize");
    return(0);
}

long ams__CUI_LastCallFinished(self) 
struct ams *self;
{
    ReportMissing("CUI_LastCallFinished");
    return(0);
}

char * ams__CUI_MachineName(self) 
struct ams *self;
{
    ReportMissing("CUI_MachineName");
    return(NULL);
}

char * ams__CUI_MailDomain(self) 
struct ams *self;
{
    ReportMissing("CUI_MailDomain");
    return(NULL);
}

long ams__CUI_MarkAsRead(self, cuid) 
struct ams *self;
int cuid;
{
    ReportMissing("CUI_MarkAsRead");
    return(0);
}

long ams__CUI_MarkAsUnseen(self, cuid) 
struct ams *self;
int cuid;
{
    ReportMissing("CUI_MarkAsUnseen");
    return(0);
}

long ams__CUI_NameReplyFile(self, cuid, code, fname) 
struct ams *self;
int cuid, code;
char *fname;
{
    ReportMissing("CUI_NameReplyFile");
    return(0);
}

long ams__CUI_OnSameHost(self) 
struct ams *self;
{
    ReportMissing("CUI_OnSameHost");
    return(0);
}

long ams__CUI_PrefetchMessage(self, cuid, ReallyNext) 
struct ams *self;
int cuid, ReallyNext;
{
    ReportMissing("CUI_PrefetchMessage");
    return(0);
}

long ams__CUI_PrintBodyFromCUIDWithFlags(self, cuid, flags, printer)
struct ams *self;
int cuid, flags;
char *printer; 
{
    ReportMissing("CUI_PrintBodyFromCUIDWithFlags");
    return(0);
}

void ams__CUI_PrintUpdates(self, dname, nickname)
struct ams *self;
char *dname, *nickname;
{
    ReportMissing("CUI_PrintUpdates");
}

long ams__CUI_ProcessMessageAttributes(self, cuid, snapshot) 
struct ams *self;
int cuid;
char *snapshot;
{
    ReportMissing("CUI_ProcessMessageAttributes");
    return(0);
}

long ams__CUI_PurgeDeletions(self, dirname) 
struct ams *self;
char *dirname;
{
    ReportMissing("CUI_PurgeDeletions");
    return(0);
}

long ams__CUI_PurgeMarkedDirectories(self, ask, OfferQuit) 
struct ams *self;
boolean ask, OfferQuit;
{
    ReportMissing("CUI_PurgeMarkedDirectories");
    return(0);
}

long ams__CUI_ReallyGetBodyToLocalFile(self, cuid, fname, ShouldDelete, MayFudge) 
struct ams *self;
int cuid;
char *fname;
int *ShouldDelete, MayFudge;
{
    ReportMissing("CUI_ReallyGetBodyToLocalFile");
    return(0);
}

long ams__CUI_RemoveDirectory(self, dirname)
struct ams *self;
char *dirname;
{
    ReportMissing("CUI_RemoveDirectory");
    return(0);
}
long ams__CUI_RenameDir(self, oldname, newname) 
struct ams *self;
char *oldname, *newname;
{
    ReportMissing("CUI_RenameDir");
    return(0);
}

void ams__CUI_ReportAmbig(self, name, atype)
struct ams *self;
char *name, *atype;
{
    ReportMissing("CUI_ReportAmbig");
}

long ams__CUI_ResendMessage(self, cuid, tolist) 
struct ams *self;
int cuid;
char *tolist;
{
    ReportMissing("CUI_ResendMessage");
    return(0);
}

long ams__CUI_RewriteHeaderLine(self, addr, newaddr) 
struct ams *self;
char *addr, *newaddr;
{
    ReportMissing("CUI_RewriteHeaderLine");
    return(0);
}

long ams__CUI_RewriteHeaderLineInternal(self, addr, newaddr, maxdealiases, numfound, externalcount, formatct, stripct, trustct)
struct ams *self;
char *addr, *newaddr;
int maxdealiases, *numfound, *externalcount, *formatct, *stripct, *trustct;
{
    ReportMissing("CUI_RewriteHeaderLineInternal");
    return(0);
}

char *ams__CUI_Rock(self)
struct ams *self;
{
    ReportMissing("CUI_Rock");
    return(NULL);
}

void ams__CUI_SetClientVersion(self, vers)
struct ams *self;
char *vers;
{
    ReportMissing("CUI_SetClientVersion");
}

long ams__CUI_SetPrinter(self, printername) 
struct ams *self;
char *printername;
{
    ReportMissing("CUI_SetPrinter");
    return(0);
}

long ams__CUI_SnapIsRunning(self) 
struct ams *self;
{
    ReportMissing("CUI_SnapIsRunning");
    return(0);
}

long ams__CUI_StoreFileToVice(self, localfile, vicefile) 
struct ams *self;
char *localfile, *vicefile;
{
    ReportMissing("CUI_StoreFileToVice");
    return(0);
}

long ams__CUI_SubmitMessage(self, infile, DeliveryOpts) 
struct ams *self;
char *infile;
long DeliveryOpts;
{
    ReportMissing("CUI_SubmitMessage");
    return(0);
}

long ams__CUI_UndeleteMessage(self, cuid) 
struct ams *self;
int cuid;
{
    ReportMissing("CUI_UndeleteMessage");
    return(0);
}

long ams__CUI_UseAmsDelivery(self) 
struct ams *self;
{
    ReportMissing("CUI_UseAmsDelivery");
    return(0);
}

long ams__CUI_UseNameSep(self) 
struct ams *self;
{
    ReportMissing("CUI_UseNameSep");
    return(0);
}

char * ams__CUI_VersionString(self) 
struct ams *self;
{
    ReportMissing("CUI_VersionString");
    return(NULL);
}

char* ams__CUI_WhoIAm(self) 
struct ams *self;
{
    ReportMissing("CUI_WhoIAm");
    return(NULL);
}

int ams__CUI_GetCuid(self, id, fullname, isdup)
struct ams *self;
char *id, *fullname;
int *isdup;
{
    ReportMissing("CUI_GetCuid");
    return(0);
}

long ams__MS_AppendFileToFolder(self, filename, foldername) 
struct ams *self;
char *filename, *foldername;
{
    ReportMissing("MS_AppendFileToFolder");
    return(0);
}

long ams__MS_CheckAuthentication(self, auth) 
struct ams *self;
long *auth;
{
    ReportMissing("MS_CheckAuthentication");
    return(0);
}

long ams__MS_DebugMode(self, mslevel, snaplevel, malloclevel) 
struct ams *self;
int mslevel, snaplevel, malloclevel;
{
    ReportMissing("MS_DebugMode");
    return(0);
}

long ams__MS_DisambiguateFile(self, source, target, MustBeDir) 
struct ams *self;
char *source, *target;
long MustBeDir;
{
    ReportMissing("MS_DisambiguateFile");
    return(0);
}

long ams__MS_FastUpdateState(self) 
struct ams *self;
{
    ReportMissing("MS_FastUpdateState");
    return(0);
}

long ams__MS_GetDirInfo(self, dirname, protcode, msgcount) 
struct ams *self;
char *dirname;
long *protcode, *msgcount;
{
    ReportMissing("MS_GetDirInfo");
    return(0);
}

long ams__MS_GetNewMessageCount(self, dirname, numnew, numtotal, lastolddate, InsistOnFetch)
struct ams *self;
char *dirname, *lastolddate;
long *numnew, *numtotal, InsistOnFetch;
{
    ReportMissing("MS_GetNewMessageCount");
    return(0);
}

long ams__MS_GetNthSnapshot(self, dirname, which, snapshotbuf)
struct ams *self;
char *dirname;
long which;
char *snapshotbuf;
{
    ReportMissing("MS_GetNthSnapshot");
    return(0);
}

long ams__MS_GetSearchPathEntry(self, which, buf, buflim) 
struct ams *self;
long which;
char *buf;
long buflim;
{
    ReportMissing("MS_GetSearchPathEntry");
    return(0);
}

long ams__MS_GetSubscriptionEntry(self, fullname, nickname, status)
struct ams *self;
char *fullname;
char *nickname;
long *status;
{
    ReportMissing("MS_GetSubscriptionEntry");
    return(0);
}

long ams__MS_NameChangedMapFile(self, mapfile, mailonly, listall, numchanged, numunavailable, nummissing, numslowpokes, numfastfellas)
struct ams *self;
char *mapfile;
long mailonly;
long listall;
long *numchanged;
long *numunavailable;
long *nummissing;
long *numslowpokes;
long *numfastfellas;
{
    ReportMissing("MS_NameChangedMapFile");
    return(0);
}

long ams__MS_NameSubscriptionMapFile(self, root, mapfile) 
struct ams *self;
char *root;
char *mapfile;
{
    ReportMissing("MS_NameSubscriptionMapFile");
    return(0);
}

long ams__MS_MatchFolderName(self, pat, filename) 
struct ams *self;
char *pat;
char *filename;
{
    ReportMissing("MS_MatchFolderName");
    return(0);
}

long ams__MS_ParseDate(self, indate, year, month, day, hour, min, sec, wday, gtm)
struct ams *self;
char *indate;
long *year;
long *month;
long *day;
long *hour;
long *min;
long *sec;
long *wday;
long *gtm;
{
    ReportMissing("MS_ParseDate");
    return(0);
}

long ams__MS_PrefetchMessage(self, dirname, id, getnext) 
struct ams *self;
char *dirname;
char *id;
long getnext;
{
    ReportMissing("MS_PrefetchMessage");
    return(0);
}

long ams__MS_SetAssociatedTime(self, fullname, newvalue) 
struct ams *self;
char *fullname;
char *newvalue;
{
    ReportMissing("MS_SetAssociatedTime");
    return(0);
}

void ams__MS_SetCleanupZombies(self, doclean)
struct ams *self;
long doclean;
{
    ReportMissing("MS_SetCleanupZombies");
}

long ams__MS_SetSubscriptionEntry(self, fullname, nickname, status)
struct ams *self;
char *fullname;
char *nickname;
long status;
{
    ReportMissing("MS_SetSubscriptionEntry");
    return(0);
}

long ams__MS_UnlinkFile(self, filename) 
struct ams *self;
char *filename;
{
    ReportMissing("MS_UnlinkFile");
    return(0);
}

long ams__MS_UpdateState(self)
struct ams *self;
{
    ReportMissing("MS_UpdateState");
    return(0);
}

long ams__MS_DomainHandlesFormatting(self, domname, retval)
struct ams *self;
char *domname;
long *retval;
{
    ReportMissing("MS_DomainHandlesFormatting");
    return(0);
}

void ams__ReportSuccess(self, s)
struct ams *self;
char *s;
{
    ReportMissing("ReportSuccess");
}


void ams__ReportError(self, s, level, decode, mserrcode)
struct ams *self;
char *s;
int level, decode;
long mserrcode;
{
    ReportMissing("ReportError");
}


int ams__GenericCompoundAction(self, v, prefix, cmds)
struct ams *self;
struct view *v;
char *prefix;
char *cmds;
{
    ReportMissing("GenericCompoundAction");
    return(0);
}

int ams__GetBooleanFromUser(self, prompt, defaultans)
struct ams *self;
char *prompt;
int defaultans;
{
    ReportMissing("GetBooleanFromUser");
    return(0);
}

int ams__GetStringFromUser(self, prompt, buf, len, ispass)
struct ams *self;
char *prompt, *buf;
int len, ispass;
{
    ReportMissing("GetStringFromUser");
    return(0);
}

int ams__TildeResolve(self, in, out)
struct ams *self;
char *in, *out;
{
    ReportMissing("TildeResolve");
    return(0);
}

int ams__OnlyMail(self)
struct ams *self;
{
    ReportMissing("OnlyMail");
    return(0);
}

char *ams__ap_Shorten(self, fname)
struct ams *self;
char *fname;
{
    ReportMissing("ap_Shorten");
    return(NULL);
}

int ams__fwriteallchars(self, s, len, fp)
struct ams *self;
char *s;
int len;
FILE *fp;
{
    ReportMissing("fwriteallchars");
    return(0);
}

long ams__mserrcode(self)
struct ams *self;
{
    ReportMissing("mserrcode");
    return(0);
}

int ams__vdown(self, errno)
struct ams *self;
int errno;
{
    ReportMissing("vdown");
    return(0);
}

int ams__AMS_ERRNO(self)
struct ams *self;
{
    ReportMissing("AMS_ERRNO");
    return(0);
}

void ams__SubtleDialogs(self, besubtle)
struct ams *self;
boolean besubtle;
{
    ReportMissing("SubtleDialogs");
}

char *ams__DescribeProt(self, code)
struct ams *self;
int code;
{
    ReportMissing("DescribeProt");
    return(NULL);
}

int ams__ChooseFromList(self, QVec, defans)
struct ams *self;
char **QVec;
int defans;
{
    ReportMissing("ChooseFromList");
    return(0);
}
int ams__CUI_GetAMSID(self, cuid, id, dir)
struct ams *self;
int cuid;
char **id, **dir;
{
    ReportMissing("ChooseFromList");
    return(0);
}

char *ams__MessagesAutoBugAddress(self)
struct ams *self;
{
    ReportMissing("MessagesAutoBugAddress");
    return(NULL);
}

int ams__UnScribe(self, ucode, ss, LineBuf, ct, fout)
struct ams *self;
int ucode;
struct ScribeState *ss;
char *LineBuf;
int ct;
FILE *fout;
{
    ReportMissing("UnScribe");
    return(0);
}

int ams__UnScribeFlush(self, ucode, ss, fout)
struct ams *self;
int ucode;
struct ScribeState *ss;
FILE *fout;
{
    ReportMissing("UnScribeFlush");
    return(0);
}

int ams__UnScribeInit(self, vers, ss)
struct ams *self;
char *vers;
struct ScribeState *ss;
{
    ReportMissing("UnScribeInit");
    return(0);
}

void ams__WriteOutUserEnvironment(self, fp, IsAboutMessages)
struct ams *self;
FILE *fp;
boolean IsAboutMessages;
{
    ReportMissing("WriteOutUserEnvironment");
}

int ams__CheckAMSUseridPlusWorks(self, dom)
struct ams *self;
char *dom;
{
    ReportMissing("CheckAMSUseridPlusWorks");
    return(0);
}

char *ams__ams_genid(self, isfilename)
struct ams *self;
boolean isfilename;
{
    ReportMissing("ams_genid");
    return(NULL);
}

static int CheckpointFrequency = -1;

/* We keep a list of all the sendmessage, captions, and folders views in creation.
  Currently, however, the captions list is not used. */

static struct smlist {
    struct sendmessage *s;
    struct smlist *next;
} *SmlistRoot = NULL;

static struct clist {
    struct captions *c;
    struct clist *next;
} *ClistRoot = NULL;

static struct flist {
    struct folders *f;
    struct flist *next;
} *FlistRoot = NULL;

static struct blist {
    struct t822view *b;
    struct blist *next;
} *BlistRoot = NULL;

void ams__SetCheckpointFrequency(c, n)
struct classheader *c;
int n;
{
    CheckpointFrequency = n;
    if (CheckpointFrequency < 0) {
	CheckpointFrequency = 60;
    } else if (CheckpointFrequency > 1000) {
	CheckpointFrequency = 1000;
    }
}

void ams__AddCheckpointCaption(cl, c)
struct classheader *cl;
struct captions *c;
{
    struct clist *ctmp;
    ctmp = (struct clist *) malloc(sizeof (struct clist));
    if (ctmp) {
	ctmp->next = ClistRoot;
	ctmp->c = c;
	ClistRoot = ctmp;
    }
}

void ams__AddCheckpointBodies(cl, b)
struct classheader *cl;
struct t822view *b;
{
    struct blist *btmp;
    btmp = (struct blist *) malloc(sizeof (struct blist));
    if (btmp) {
	btmp->next = BlistRoot;
	btmp->b = b;
	BlistRoot = btmp;
    }
}

void ams__AddCheckpointFolder(cl, f)
struct classheader *cl;
struct folders *f;
{
    struct flist *ctmp;
    ctmp = (struct flist *) malloc(sizeof (struct flist));
    if (ctmp) {
	ctmp->next = FlistRoot;
	ctmp->f = f;
	FlistRoot = ctmp;
    }
}

void ams__AddCheckpointSendmessage(c, sm)
struct classheader *c;
struct sendmessage *sm;
{
    struct smlist *smtmp;
    smtmp = (struct smlist *) malloc(sizeof (struct smlist));
    if (smtmp) {
	smtmp->next = SmlistRoot;
	smtmp->s = sm;
	SmlistRoot = smtmp;
    }
}

void ams__RemoveCheckpointFolder(cl, f)
struct classheader *cl;
struct folders *f;
{
    struct flist *ctmp = FlistRoot, *ctmpprev = NULL;
    while (ctmp) {
	if (ctmp->f != f) {
	    ctmpprev = ctmp;
	    ctmp = ctmp->next;
	} else {
	    if (ctmpprev) {
		ctmpprev->next = ctmp->next;
	    } else {
		FlistRoot = ctmp->next;
	    }
	    free(ctmp);
	    ctmp = ctmpprev ? ctmpprev->next : FlistRoot;
	}
    }
}

void ams__RemoveCheckpointBodies(cl, b)
struct classheader *cl;
struct t822view *b;
{
    struct blist *btmp = BlistRoot, *btmpprev = NULL;
    while (btmp) {
	if (btmp->b != b) {
	    btmpprev = btmp;
	    btmp = btmp->next;
	} else {
	    if (btmpprev) {
		btmpprev->next = btmp->next;
	    } else {
		BlistRoot = btmp->next;
	    }
	    free(btmp);
	    btmp = btmpprev ? btmpprev->next : BlistRoot;
	}
    }
}

void ams__RemoveCheckpointCaption(cl, c)
struct classheader *cl;
struct captions *c;
{
    struct clist *ctmp = ClistRoot, *ctmpprev = NULL;
    while (ctmp) {
	if (ctmp->c != c) {
	    ctmpprev = ctmp;
	    ctmp = ctmp->next;
	} else {
	    if (ctmpprev) {
		ctmpprev->next = ctmp->next;
	    } else {
		ClistRoot = ctmp->next;
	    }
	    free(ctmp);
	    ctmp = ctmpprev ? ctmpprev->next : ClistRoot;
	}
    }
}

void ams__RemoveCheckpointSendmessage(c, sm)
struct classheader *c;
struct sendmessage *sm;
{
    struct smlist *stmp = SmlistRoot, *stmpprev = NULL;
    while (stmp) {
	if (stmp->s != sm) {
	    stmpprev = stmp;
	    stmp = stmp->next;
	} else {
	    if (stmpprev) {
		stmpprev->next = stmp->next;
	    } else {
		SmlistRoot = stmp->next;
	    }
	    free(stmp);
	    stmp = stmpprev ? stmpprev->next : SmlistRoot;
	}
    }
}

void ams__WaitCursor(c, IsWait)
struct classheader *c;
boolean IsWait;
{
    static struct cursor *waitcursor = NULL;
    if (IsWait) {
	if (!waitcursor) {
	    waitcursor = cursor_Create(im_GetLastUsed());
	    cursor_SetStandard(waitcursor, Cursor_Wait);
	}
	im_SetProcessCursor(waitcursor);
    } else {
	im_SetProcessCursor(NULL);
    }
}

void ams__SubscriptionChangeHook(c, name, nick, status, mess)
struct classheader *c;
char *name, *nick;
int status;
struct messages *mess;
{
    struct flist *ctmp;

    for (ctmp = FlistRoot; ctmp; ctmp = ctmp->next) {
	folders_AlterSubscriptionStatus(ctmp->f, name, status, nick);
    }
}


static char **ClassList = NULL, **ClassListDirs = NULL;
static int ClassListSize = 0, ClassListCount = 0, LastMenuClass = 0, HasInitializedClassList = 0;
static char *FirstMailClass = NULL;

int ams__GetLastMenuClass(c)
struct classheader *c;
{
    return(LastMenuClass);
}

int ams__GetClassListCount(c)
struct classheader *c;
{
    return(ClassListCount);
}

char *ams__GetClassListEntry(c, i)
struct classheader *c;
int i;
{
    if (ClassList && i < ClassListCount) {
	return(ClassList[i]);
    } 
    return(NULL);
}

void ams__DirectoryChangeHook(c, adddir, deldir, rock)
struct classheader *c;
char *adddir, *deldir;
long rock;
{
    char Nick[1+MAXPATHLEN], *mp;
    int i, mplen;
    struct flist *ftmp;
    struct clist *ctmp;

    mp = ams_GetMailPath();
    mplen = strlen(mp);
    if (mplen == 0) return;
    if (adddir) {
	ams_CUI_BuildNickName(ams_GetAMS(), adddir, Nick);
	for (ftmp = FlistRoot; ftmp; ftmp = ftmp->next) {
	    folders_AlterFolderNames(ftmp->f, adddir, Nick, TRUE);
	}
	if (!strncmp(mp, adddir, mplen)) {
	    AddToClassList(Nick, adddir, TRUE);
	}
    }
    if (deldir) {
	for (ctmp = ClistRoot; ctmp; ctmp = ctmp->next) {
	    captions_AlterPrimaryFolderName(ctmp->c, adddir, deldir);
	}
	ams_CUI_BuildNickName(ams_GetAMS(), deldir, Nick);
	if (!strncmp(mp, deldir, mplen)) {
	    /* Delete from ClassList array */
	    for (i = 0; i < ClassListCount; ++i) {
		if (!strcmp(Nick, ClassList[i])) {
		    break;
		}
	    }
	    if (i < ClassListCount) {
		free(ClassList[i]);
		while (i+1 < ClassListCount) {
		    ClassList[i] = ClassList[i+1];
		    ++i;
		}
		--ClassListCount;
	    }
	}
	for (ftmp = FlistRoot; ftmp; ftmp = ftmp->next) {
	    folders_AlterFolderNames(ftmp->f, deldir, Nick, FALSE);
	}
    }
    ams_ResetClassList();
}

char *ams__GetFirstMailClass(c)
struct classheader *c;
{
    return(FirstMailClass);
}

int ams__InitializeClassList(c)
struct classheader *c;
{
    char TempName[1+MAXPATHLEN], LocalName[1+MAXPATHLEN], *s, *t, *t2;
    int MaxClassMenu;
    FILE *fp;

    if (HasInitializedClassList) return(0);
    ClassListSize = 20;
    strcpy(TempName, ams_GetMailPath());
    if (TempName[0] == '\0') return(-1);
    strcat(TempName, "/");
    strcat(TempName, AMS_SUBSCRIPTIONMAPFILE);
    FirstMailClass = NULL;

    ams_CUI_GenTmpFileName(ams_GetAMS(), LocalName);
    if (ams_CUI_GetFileFromVice(ams_GetAMS(), LocalName, TempName)) {
	return(-1);
    }
    fp = fopen(LocalName, "r");
    if (!fp) {
	unlink(LocalName);
	return(-1);
    }
    ClassList = (char **) malloc(ClassListSize * sizeof(char *));
    if (!ClassList) {
	unlink(LocalName);
	fclose(fp);
	return(-1);
    }
    ClassListDirs = (char **) malloc(ClassListSize * sizeof(char *));
    if (!ClassListDirs) {
	unlink(LocalName);
	fclose(fp);
	return(-1);
    }
    MaxClassMenu = environ_GetProfileInt("messages.maxclassmenu", 8);
    s = (char *) environ_GetProfile("messages.crucialclasses");
    if (s) {
	t = malloc(1+strlen(s));
	strcpy(t, s);
	s = t; /* permanent copy */
    }
    while (s && *s) {
	t = strchr(s, ',');
	if (t) *t++ = '\0';
	t2 = strchr(s, ':');
	if (t2) *t2++ = '\0';
	if (AddToClassList(s, t2, FALSE)) {
	    unlink(LocalName);
	    fclose(fp);
	    return(-1);
	}
	s = t;
    }
    LastMenuClass = ClassListCount;
    while (fgets(TempName, MAXPATHLEN, fp)) {
	s = strchr(TempName, ':');
	if (s) *s++ = '\0';
	if (!FirstMailClass) {
	    FirstMailClass = malloc(1+strlen(TempName));
	    if (!FirstMailClass) {
		unlink(LocalName);
		fclose(fp);
		return(-1);
	    }
	    strcpy(FirstMailClass, TempName);
	}
	if (AddToClassList(TempName, s, TRUE)) {
	    unlink(LocalName);
	    fclose(fp);
	    return(-1);
	}
    }
    unlink(LocalName);
    fclose(fp);
    LastMenuClass = ClassListCount;
    if (LastMenuClass > MaxClassMenu) LastMenuClass = MaxClassMenu;
    HasInitializedClassList = 1;
    return(0);
}


AddToClassList(TempName, FullName, CheckDups)
char *TempName, *FullName;
Boolean CheckDups;
{
    int i;
    char *MyName;

    MyName = amsutil_StripWhiteEnds(TempName);
    if (CheckDups) {
	for (i = 0; i<LastMenuClass; ++i) {
	    if (!strcmp(MyName, ClassList[i])) {
		return(0);
	    }
	}
    }
    ClassList[ClassListCount] = malloc(1+strlen(MyName));
    if (!ClassList[ClassListCount]) {
	while (--ClassListCount >= 0) {
	    free(ClassList[ClassListCount]);
	    free(ClassListDirs[ClassListCount]);
	}
	free(ClassList);
	ClassList = NULL;
	return(-1);
    }
    if (FullName) {
	amsutil_StripWhiteEnds(FullName);
	ClassListDirs[ClassListCount] = malloc(1+strlen(FullName));
	if (!ClassListDirs[ClassListCount]) {
	    while (--ClassListCount >= 0) {
		free(ClassList[ClassListCount]);
		free(ClassListDirs[ClassListCount]);
	    }
	    free(ClassListDirs);
	    ClassListDirs = NULL;
	    return(-1);
	}
	strcpy(ClassListDirs[ClassListCount], FullName);
    } else {
	ClassListDirs[ClassListCount] = NULL;
    }
    strcpy(ClassList[ClassListCount], MyName);
    if (++ClassListCount>= ClassListSize) {
	ClassListSize += 20;
	ClassList = (char **) realloc(ClassList, ClassListSize * sizeof(char *));
	if (!ClassList) {
	    return(-1);
	}
	ClassListDirs = (char **) realloc(ClassListDirs, ClassListSize * sizeof(char *));
	if (!ClassListDirs) {
	    return(-1);
	}
    }
    return(0);
}

char *ams__GetMailPath(c)
struct classheader *c;
{
    static char MailPath[1+MAXPATHLEN];
    static int hassetup = 0;
    long code;

    if (!hassetup) {
	code = ams_MS_GetSearchPathEntry(ams_GetAMS(), AMS_MAILPATH, MailPath, MAXPATHLEN);
	if (code) {
	    ams_ReportError(ams_GetAMS(), "Cannot get the name of your mail directory from the message server.", ERR_WARNING, TRUE, code);
	    MailPath[0] = '\0';
	} else {
	    hassetup = 1;
	}
    }
    return(MailPath);
}

void ams__ResetClassList(c)
struct classheader *c;
{
    struct clist *ctmp;
    struct flist *ftmp;
    struct blist *btmp;

    while (--ClassListCount >= 0) {
	free(ClassList[ClassListCount]);
	if (ClassListDirs[ClassListCount]) {
	    free(ClassListDirs[ClassListCount]);
	}
    }
    if (ClassList) free(ClassList);
    if (ClassListDirs) free(ClassListDirs);
    if (FirstMailClass) free(FirstMailClass);
    ClassList = NULL;
    ClassListDirs = NULL;
    FirstMailClass = NULL;
    ClassListCount = 0;
    ClassListSize = 0;
    LastMenuClass = 0;
    HasInitializedClassList = 0;

    ams_InitializeClassList();
    for (ctmp = ClistRoot; ctmp; ctmp = ctmp->next) {
	captions_ResetFileIntoMenus(ctmp->c);
    }
    for (ftmp = FlistRoot; ftmp; ftmp = ftmp->next) {
	folders_ResetFileIntoMenus(ftmp->f);
    }
    for (btmp = BlistRoot; btmp; btmp = btmp->next) {
	t822view_ResetFileIntoMenus(btmp->b);
    }
}

static struct DelayedUpdate {
    char *name;
    char date[AMS_DATESIZE+1];
    struct DelayedUpdate *next;
} *DelayedUpdates = NULL;

int ams__TryDelayedUpdates(c)
struct classheader *c;
{
    struct DelayedUpdate *dutmp;
    long mcode;

    while (DelayedUpdates) {
	mcode = ams_MS_SetAssociatedTime(ams_GetAMS(), DelayedUpdates->name, DelayedUpdates->date);
	if (mcode) return 1; /* Do not report these repeated errors */
	free(DelayedUpdates->name);
	dutmp = DelayedUpdates->next;
	free(DelayedUpdates);
	DelayedUpdates = dutmp;
    }
    return 0;
}

static char *NoCacheMem = "Out of memory; cannot save your unremembered profile information";

void ams__CacheDelayedUpdate(c, FullName, UpdateDate)
struct classheader *c;
char *FullName, *UpdateDate;
{
    struct DelayedUpdate *dutmp;

    dutmp = (struct DelayedUpdate *) malloc(sizeof(struct DelayedUpdate));
    if (!dutmp) {
	ams_ReportError(ams_GetAMS(), NoCacheMem, ERR_CRITICAL, FALSE, 0);
	return;
    }
    dutmp->name = malloc(1+strlen(FullName));
    if (!dutmp->name) {
	ams_ReportError(ams_GetAMS(), NoCacheMem, ERR_CRITICAL, FALSE, 0);
	free(dutmp);
	return;
    }
    strcpy(dutmp->name, FullName);
    strncpy(dutmp->date, UpdateDate, AMS_DATESIZE);
    dutmp->next = DelayedUpdates;
    DelayedUpdates = dutmp;
}

static void HandleInitProblem(rock, err)
long rock;
char *err;
{
    fprintf(stderr,"%s\n",err);
}

static struct init *ReadInitFile(fakeprogname, realprogname)
char *fakeprogname, *realprogname;
{
    char buffer[256], buffer1[256], *andrewDir, *sitename;
    struct init *init;
    boolean HadGlobalNameInit = FALSE;
    boolean siteinit = FALSE, sitegloinit = FALSE;
    char *home;
    struct init *oldinit=im_GetGlobalInit();
    
    if(!fakeprogname) return;

    if(!strcmp(fakeprogname, realprogname)) return NULL;

    init = init_New();
    im_SetGlobalInit(init);	/* tell im about it.    -wjh
			The init can then be modified by called procs.
			(retract below if no inits found)  */

    home = environ_Get("HOME");
    andrewDir = environ_AndrewDir(NULL);

    if((sitename = environ_GetConfiguration("SiteConfigName")) != NULL){
	sprintf(buffer, "%s/lib/%s.atkinit", andrewDir,sitename);
	sprintf(buffer1, "%s/lib/%s.%sinit", andrewDir,sitename, fakeprogname);
	if(access(buffer1, R_OK)>=0) {
	    sitegloinit = init_Load(init, buffer, (procedure) HandleInitProblem, (long) NULL,FALSE) >= 0;
	    siteinit = init_Load(init, buffer1, (procedure) HandleInitProblem, (long) NULL, FALSE) >= 0;
	}
    }
    
    /* try for .NAMEinit and quit if succeed */
    if (home != NULL)  {
	sprintf(buffer1, "%s/.%sinit", home, fakeprogname);
	if(access(buffer1, R_OK)>=0) {
	    if(sitename && !sitegloinit) {
		sitegloinit = init_Load(init, buffer, (procedure) HandleInitProblem, (long) NULL,FALSE) >= 0;
	    }

	    if ((init_Load(init, buffer1, (procedure) HandleInitProblem, (long) NULL, FALSE) >= 0)) 
		goto done;
	}
    }
    
    /* try for andrew/lib/global.NAMEinit and continue even if succeed */
    sprintf(buffer1, "%s/lib/global.%sinit", andrewDir, fakeprogname);
    if(access(buffer1, R_OK)>=0) {

	if(sitename && !sitegloinit) {
	    sitegloinit = init_Load(init, buffer, (procedure) HandleInitProblem, (long) NULL,FALSE) >= 0;
	}

	HadGlobalNameInit = init_Load(init, buffer1, (procedure) HandleInitProblem, (long) NULL, FALSE) >= 0;
    }

	
    /* try for ~/.atkinit or ~/.be2init  extending
	global.NAMEinit quit if succeed */
    if(HadGlobalNameInit) {
	if (home != NULL) {
	    sprintf(buffer, "%s/.atkinit", home);
	    if(init_Load(init, buffer, (procedure) HandleInitProblem, (long) NULL, FALSE) >= 0) 
		goto done;
	    sprintf(buffer, "%s/.be2init", home);
	    if (init_Load(init, buffer, (procedure) HandleInitProblem, (long) NULL, FALSE) >= 0 ) 
		goto done;
	}
    } else {
	/* If we failed to find initialization, discard the data structure we might have used */
	im_SetGlobalInit(oldinit);
	init_Destroy(init);
	return NULL;
    }
done:
    im_SetGlobalInit(oldinit);
    return init;
}

struct frame *ams__InstallInNewWindow(c, v, programname, windowname, w, h, focusv)
struct classheader *c;
struct view *v;
char *programname, *windowname;
int w, h;
struct view *focusv;
{
    struct frame *myframe;
    struct im *myim;
    char geompref[500], *geomspec;
    struct init *init;
    char *realprogname, *pname=im_GetProgramName();

      
    if((myframe = frame_New()) == NULL) {
	fprintf(stderr,"Could not allocate enough memory.\n");
	return(NULL);
    }
    if (w >= 0 && h >= 0) {
	im_SetPreferedDimensions(0, 0, w, h);
    }
    
    realprogname=(char *)malloc(strlen(pname)+1);
    if(realprogname) strcpy(realprogname, pname);
    
    im_SetProgramName(programname);
    strcpy(geompref, programname);
    strcat(geompref, ".geometry");
    geomspec = environ_GetProfile(geompref);
    if (geomspec) im_SetGeometrySpec(geomspec);
    myim = im_Create(NULL);
    if (!myim) {
	fprintf(stderr, "Could not create new window.\n");
	if(myframe) frame_Destroy(myframe);
	return(NULL);
    }
    init=ReadInitFile(programname, realprogname);
    if(init) {
	init_Destroy(myim->init);
	myim->init=init;
    }
    im_SetProgramName(realprogname);
    free(realprogname);
    
    frame_SetView(myframe, v);
    im_SetView(myim, myframe);
    frame_SetTitle(myframe, windowname);
    frame_PostDefaultHandler(myframe, "message", frame_WantHandler(myframe, "message"));
    im_Interact(FALSE); /* force the redisplay */
    view_WantInputFocus(focusv, focusv);
    view_WantUpdate(focusv, focusv);
    view_WantUpdate(v, v);
    return(myframe);
}

void ams__Focus(c, v)
struct classheader *c;
struct view *v;
{
    struct im *im;

    im = view_GetIM(v);
    if (im) {
#ifdef NOWAYJOSE
        im_ExposeWindow(im);
#endif /* NOWAYJOSE */
	im_SetWMFocus(im);
	im_SetLastUsed(im);
    }
    view_WantInputFocus(v, v);
}

static struct event *NextCKP = NULL;

void ams__TimerInit(c)
struct classheader *c;
{
    RestartTimer();
}

static DisplayAMS_ERRNO(prefix)
char *prefix;
{
    long myerrno;
    char ErrorText[1000];
    char *msg2;

    myerrno = ams_AMS_ERRNO(ams_GetAMS());
    switch (myerrno) {
#ifdef AFS_ENV
#ifdef EDQUOT
	case EDQUOT:
	    msg2 = "Volume over quota";
	    break;
#endif /* EDQUOT */
	case EACCES:
	    msg2 = "Permission denied; maybe authentication failure";
	    break;
#endif /* AFS_ENV */
	default:
	    if (ams_vdown(ams_GetAMS(), myerrno)) {
		msg2 = "Network/server failure";
	    } else {
		msg2 = (char *) UnixError(myerrno);
	    }
    }
    sprintf(ErrorText, "%s: %s.", prefix, msg2);
    message_DisplayString(NULL, 10, ErrorText);
}

static UpdateServerState() {
    static int updatect = 0; /* a hack to make sure we close everything eventually */

    message_DisplayString(NULL, 10, "Checkpointing message server state...");
    im_ForceUpdate();
    if (++updatect > 20) { /* every 20 minutes or so, do it the long way */
	updatect = 0;
	if (ams_MS_UpdateState(ams_GetAMS())) {
	    DisplayAMS_ERRNO("Checkpoint failed");
	    return(-1);
	}
    } else if (ams_MS_FastUpdateState(ams_GetAMS())) {
	DisplayAMS_ERRNO("Checkpoint failed");
	return(-1);
    }
    message_DisplayString(NULL, 10, "Checkpointing message server state... done.");
    return(0);
}

static struct im_InteractionEvent *MyInteractionEvent = NULL;

static HandleTimer()
{
    struct flist *ctmp;
    struct smlist *smtmp;
    boolean DidSomething = FALSE;
    static int CheckpointCounter = 0;
    static boolean DoCheckpoints = TRUE, DoPrefetch = TRUE;

    ams_WaitCursor(TRUE);
    if (CheckpointFrequency < 0) { /* First time */
	ams_SetCheckpointFrequency(environ_GetProfileInt("messages.checkpointfrequency", 30));
	DoCheckpoints = ! environ_GetProfileSwitch("messages.TurnOffCheckpointingAndIUnderstandTheDangersForMessages", 0);
	DoPrefetch = environ_GetProfileSwitch("messages.doprefetch", 1);
    }
    /* Make sure there is an absolute cap on intervals between ms checkpointing.    */
    if (DoCheckpoints && ++CheckpointCounter >= 5) {
	TimerReport(2);
	UpdateServerState(); /* errors irrelevant here */
	DidSomething = TRUE; /* Definitely want to wake up again soon */
	CheckpointCounter = 0;
    }
    if (DoPrefetch && !DidSomething) {
	for (ctmp = FlistRoot; ctmp; ctmp = ctmp->next) {
	    if (ctmp->f->NeedsToPrefetch) {
		TimerReport(0);
		folders_HandleAsyncPrefetch(ctmp->f);
		DidSomething = TRUE;
		break;
	    }
	}
    }
    if (DoCheckpoints && !DidSomething) {
	for (smtmp = SmlistRoot; smtmp; smtmp = smtmp->next) {
	    if (sendmessage_NeedsCheckpointing(smtmp->s)) {
		TimerReport(1);
		sendmessage_Checkpoint(smtmp->s);
		DidSomething = TRUE;
		break;
	    }
	}
    }
    if (DoCheckpoints && !DidSomething) {
	TimerReport(2);
	if (UpdateServerState()) {
	    DidSomething = TRUE; /* An error occurred -- try again sooner this way */
	}
	CheckpointCounter = 0;
    }
    if (DidSomething) {
	RestartTimer();
	MyInteractionEvent = 0;
    } else {
	NextCKP = NULL;
	MyInteractionEvent = im_SetInteractionEvent(im_GetLastUsed(), RestartTimer, 0, 0);
	TimerReport(4);
    }
    ams_WaitCursor(FALSE);
    im_ForceUpdate();
}

static RestartTimer() {
    int freq = (CheckpointFrequency > 0) ? CheckpointFrequency : 60;

    NextCKP = im_EnqueueEvent(HandleTimer, 0, event_SECtoTU(freq));
    TimerReport(3);
}

void ams__PlanFolderPrefetch(c, f)
struct classheader *c;
struct folders *f;
{
    f->NeedsToPrefetch = TRUE;
    if (NextCKP) {
	event_Cancel(NextCKP);
    } else if (MyInteractionEvent) {
	im_CancelInteractionEvent(im_GetLastUsed(), MyInteractionEvent);
	MyInteractionEvent = NULL;
    }
    NextCKP = im_EnqueueEvent(HandleTimer, 0, event_SECtoTU(5));
    TimerReport(5);
}

static TimerReport(code)
int code;
{
    static int ReportTimers = -1;

    if (ReportTimers < 0) {
	ReportTimers = environ_GetProfileSwitch("reporttimers", 0) ? 1 : 0;
    }
    if (ReportTimers) {
	long clock = time(0);

	switch(code) {
	    case 0:
		printf("AMS: prefetching");
		break;
	    case 1:
		printf("AMS: checkpointing sendmessage");
		break;
	    case 2:
		printf("AMS: checkpointing messageserver");
		break;
	    case 3:
		printf("AMS: re-enqueueing for %d seconds", CheckpointFrequency);
		break;
	    case 4:
		printf("AMS: waiting for next interaction event");
		break;
	    case 5:
		printf("AMS: Prefetch expected; rescheduling timer event SOON");
		break;
	    default:
		printf("AMS:  BOGUS TimerReport code");
	}
	printf(" at %s", ctime(&clock));
    }
}

void ams__CommitState(c, DoQuit, MustQuit, MayPurge, OfferQuit)
struct classheader *c;
boolean DoQuit, MustQuit, MayPurge, OfferQuit;
{
    int ChangedSendmessages = 0, purgecode;
    boolean SafeToExit = TRUE;
    struct clist *ctmp;
    struct smlist *smtmp;

    if (!DoQuit && !MustQuit) SafeToExit = FALSE;
    if (DoQuit) {
	for (smtmp = SmlistRoot; smtmp; smtmp = smtmp->next) {
	    if (sendmessage_HasChanged(smtmp->s)) {
		++ChangedSendmessages;
	    }
	}
	if (ChangedSendmessages) {
	    char Question[400], *quest;

	    if (ChangedSendmessages > 1) {
		sprintf(Question, "Do you want to erase the %d pieces of mail you have not yet sent", ChangedSendmessages);
		quest = Question;
	    } else {
		quest = "Do you want to erase the mail you have not yet sent";
	    }
	    if (!ams_GetBooleanFromUser(ams_GetAMS(), quest, FALSE)) {
		SafeToExit = FALSE;
	    }
	}
    }
    for (ctmp = ClistRoot; ctmp; ctmp = ctmp->next) {
	captions_MakeCachedUpdates(ctmp->c); /* Never returns errors, delays them */
    }
    if (MayPurge && ams_CUI_DirectoriesToPurge(ams_GetAMS())) {
	ams_WaitCursor(TRUE);
	if (amsutil_GetOptBit(EXP_PURGEONQUIT)) {
	    purgecode = ams_CUI_PurgeMarkedDirectories(ams_GetAMS(), FALSE, OfferQuit);
	} else {
	    purgecode = ams_CUI_PurgeMarkedDirectories(ams_GetAMS(), TRUE, OfferQuit);
	}
	ams_WaitCursor(FALSE);
	if (purgecode > 0) {
	    SafeToExit = FALSE;
	} else if (purgecode < 0) {
	    if (!MustQuit) DisplayAMS_ERRNO("Cannot purge deletions");
	    if (SafeToExit && !ams_GetBooleanFromUser(ams_GetAMS(), "Could not purge deletions; quit anyway", FALSE)) {
		SafeToExit = FALSE;
	    }
	}
    }
    if (ams_TryDelayedUpdates()) {
	if (!MustQuit) DisplayAMS_ERRNO("Cannot update your profile");
	if (SafeToExit && !ams_GetBooleanFromUser(ams_GetAMS(), "Could not update your profile; quit anyway", FALSE)) {
	    SafeToExit = FALSE;
	}
    }
    if (ams_MS_UpdateState(ams_GetAMS())) {
	if (!MustQuit) {
	    DisplayAMS_ERRNO("Cannot update state");
	    if (SafeToExit && !ams_GetBooleanFromUser(ams_GetAMS(), "Could not update messageserver state; quit anyway", FALSE)) {
		SafeToExit = FALSE;
	    }
	}
    }
    if (MustQuit || (DoQuit && SafeToExit)) {
	ams_CUI_EndConversation(ams_GetAMS());
	exit(0);
    }
}

int ams__CountAMSViews() {
    struct clist *ctmp;
    struct smlist *smtmp;
    struct flist *ftmp;
    struct blist *btmp;
    int ct = 0;

    for (smtmp = SmlistRoot; smtmp; smtmp = smtmp->next) {
	++ct;
    }
    for (ctmp = ClistRoot; ctmp; ctmp = ctmp->next) {
	++ct;
    }
    for (ftmp = FlistRoot; ftmp; ftmp = ftmp->next) {
	++ct;
    }
    for (btmp = BlistRoot; btmp; btmp = btmp->next) {
	++ct;
    }
    return(ct);
}

static FILE *GetCompletionFP(partial)
char *partial;
{
    char fname[1+MAXPATHLEN];
    long errcode;
    struct ams *myams = ams_GetAMS();
    FILE *fp;

    errcode = ams_MS_MatchFolderName(myams, partial, fname);
    if (errcode) {
	ams_ReportError(myams, "Could not get list of matching folders", ERR_CRITICAL, TRUE, errcode);
	return(NULL);
    }
    if (!ams_CUI_OnSameHost(myams)) {
	char TmpName[1+MAXPATHLEN];

	ams_CUI_GenLocalTmpFileName(myams, TmpName);
	if (errcode = ams_CUI_GetFileFromVice(myams, TmpName, fname)) {
	    ams_ReportError(myams, "Could not copy file from AFS", ERR_CRITICAL, TRUE, errcode);
	    return(NULL);
	}
	ams_MS_UnlinkFile(myams, fname);
	strcpy(fname, TmpName);
    }
    fp = fopen(fname, "r");
    if (!fp) {
	ams_ReportError(myams, "Could not open temporary file", ERR_CRITICAL, FALSE, 0);
    }
    unlink(fname); /* file remains around until closed */
    return(fp);
}

static void FolderHelp(partial, rock, helpfunc, helprock)
char *partial;
long rock;
int (*helpfunc)();
long helprock;
{
    char LineBuf[1000], *s;
    FILE *fp;
    int inlen;

    ams_WaitCursor(TRUE);
    fp = GetCompletionFP(partial);
    if (!fp) {
	ams_WaitCursor(FALSE);
	return;
    }
    inlen = strlen(partial);
    while (fgets(LineBuf, sizeof(LineBuf), fp)) {
	int len=strlen(LineBuf)-1;
	if(len>=0 && LineBuf[len]=='\n') LineBuf[len] = '\0';
	s = &LineBuf[inlen+1];
	if (strchr(s, '.')) continue;
	(*helpfunc)(helprock, message_HelpListItem, LineBuf, NULL);
    }
    fclose(fp);
    ams_WaitCursor(FALSE);
}

static enum message_CompletionCode FolderComplete(part, dummy, buf, size)
char *part, *buf;
long dummy, size;
{
    char LineBuf[1000], AnsBuf[1+MAXPATHLEN];
    FILE *fp;
    int ct = 0, i;
    boolean incomplete = FALSE;

    ams_WaitCursor(TRUE);
    fp = GetCompletionFP(part);
    if (!fp) {
	ams_WaitCursor(FALSE);
	return(message_Invalid);
    }
    AnsBuf[0] = '\0';
    while (fgets(LineBuf, sizeof(LineBuf), fp)) {
	LineBuf[strlen(LineBuf)-1] = '\0';
	++ct;
	if (AnsBuf[0]) {
	    for (i=0; AnsBuf[i] && LineBuf[i] && (AnsBuf[i] == LineBuf[i]); ++i) {
		;
	    }
	    if (AnsBuf[i]) {
		AnsBuf[i] = '\0';
		incomplete = LineBuf[i] ? TRUE : FALSE;
	    }
	} else {
	    strcpy(AnsBuf, LineBuf);
	}
    }
    ams_WaitCursor(FALSE);
    if (ct == 0) {
	strncpy(buf, part, size);
	return(message_Invalid);
    }
    strncpy(buf, AnsBuf, size);
    fclose(fp);
    if (ct == 1) return(message_Complete);
    return(incomplete ? message_Valid : message_CompleteValid);
}

int ams__GetFolderName(c, prompt, buf, buflen, defaultname, MustMatch)
struct classheader *c;
char *prompt, *buf, *defaultname;
int buflen;
boolean MustMatch;
{
    if (message_AskForStringCompleted(NULL, 25, prompt, defaultname, buf, buflen, NULL, FolderComplete, FolderHelp, 0 /* rock */, MustMatch ? message_MustMatch : 0)< 0) return(-1);
    return(0);
}
