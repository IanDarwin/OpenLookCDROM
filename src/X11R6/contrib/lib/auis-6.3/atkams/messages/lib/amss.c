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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/amss.c,v 1.12 1994/03/31 16:05:30 rr2b Exp $";
#endif

/* Until I come up with a better scheme, new functions here have to be added to SIX files -- ams.ch, amss.ch, amsn.ch (all identical specs) and the corresponding c files */ 

#include <class.h>
#include <ams.ih>
#include <amss.eh>
#include <im.ih>
#include <message.ih>
#include <mailconf.h>
#include <cui.h>

extern char *CUI_MachineName, CUI_MailDomain[], *CUI_Rock, *CUI_VersionString, *CUI_WhoIAm, *ap_Shorten(), *DescribeProt(), *ams_genid();

extern long CUI_DeliveryType, CUI_LastCallFinished, CUI_OnSameHost, CUI_SnapIsRunning, CUI_UseAmsDelivery, CUI_UseNameSep, mserrcode;

void amss__RemoveErrorDialogWindow(self)
struct amss *self;
{
    extern void ams_RemoveErrorWindow();
    ams_RemoveErrorWindow();
}

boolean amss__InitializeClass(c)
struct classheader *c;
{
    CheckAMSConfiguration();
    SetProgramVersion();
    return(TRUE);
}

void amss__CUI_BuildNickName(self, shortname, longname)
struct amss *self;
char *shortname, *longname;
{
    CUI_BuildNickName(shortname, longname);
}

int amss__CUI_CheckMailboxes(self, forwhat) 
struct amss *self;
char *forwhat;
{
    return(CUI_CheckMailboxes(forwhat));
}

long amss__CUI_CloneMessage(self, cuid, DirName, code) 
struct amss *self;
int cuid;
char *DirName;
int code;
{
    return(CUI_CloneMessage(cuid, DirName, code));
}

long amss__CUI_CreateNewMessageDirectory(self, dir, bodydir) 
struct amss *self;
char *dir, *bodydir;
{
    return(CUI_CreateNewMessageDirectory(dir, bodydir));
}

long amss__CUI_DeleteMessage(self, cuid) 
struct amss *self;
int cuid;
{
    return(CUI_DeleteMessage(cuid));
}

long amss__CUI_DeliveryType(self) 
struct amss *self;
{
    return(CUI_DeliveryType);
}

long amss__CUI_DirectoriesToPurge(self) 
struct amss *self;
{
    return(CUI_DirectoriesToPurge());
}

long amss__CUI_DisambiguateDir(self, shortname, longname) 
struct amss *self;
char *shortname, *longname;
{
    return(CUI_DisambiguateDir(shortname, longname));
}

long amss__CUI_DoesDirNeedPurging(self, name) 
struct amss *self;
char *name;
{
    return(CUI_DoesDirNeedPurging(name));
}

void amss__CUI_EndConversation(self)
struct amss *self;
{
    CUI_EndConversation();
}

long amss__CUI_GenLocalTmpFileName(self, name) 
struct amss *self;
char *name;
{
    return(CUI_GenLocalTmpFileName(name));
}

long amss__CUI_GenTmpFileName(self, name) 
struct amss *self;
char *name;
{
    return(CUI_GenTmpFileName(name));
}

long amss__CUI_GetFileFromVice(self, tmp_file, vfile) 
struct amss *self;
char *tmp_file, *vfile;
{
    return(CUI_GetFileFromVice(tmp_file, vfile));
}

long amss__CUI_GetHeaderContents(self, cuid, hdrname, hdrnum, hdrbuf, lim)
struct amss *self;
int cuid;
char *hdrname;
int hdrnum;
char *hdrbuf;
int lim;
{
    return(CUI_GetHeaderContents(cuid, hdrname, hdrnum, hdrbuf, lim));
}

long amss__CUI_GetHeaders(self, dirname, date64, headbuf, lim, startbyte, nbytes, status, RegisterCuids)
struct amss *self;
char *dirname, *date64, *headbuf;
int lim, startbyte, *nbytes, *status, RegisterCuids;
{
    return(CUI_GetHeaders(dirname, date64, headbuf, lim, startbyte, nbytes, status, RegisterCuids));
}

long amss__CUI_GetSnapshotFromCUID(self, cuid, Sbuf) 
struct amss *self;
int cuid;
char *Sbuf;
{
    return(CUI_GetSnapshotFromCUID(cuid, Sbuf));
}

long amss__CUI_HandleMissingFolder(self, dname) 
struct amss *self;
char *dname;
{
    return(CUI_HandleMissingFolder(dname));
}

static int TimerInit();
long amss__CUI_Initialize(self, TimerFunction, rock)
struct amss *self;
int (*TimerFunction)();
char *rock;
{
    message_DisplayString(NULL, 10, "Initializing Connection to Message Server...");
    im_ForceUpdate();
    if (!TimerFunction) TimerFunction = TimerInit;
    return(CUI_Initialize(TimerFunction, rock));
}

long amss__CUI_LastCallFinished(self) 
struct amss *self;
{
    return(CUI_LastCallFinished);
}

char * amss__CUI_MachineName(self) 
struct amss *self;
{
    return(CUI_MachineName);
}

char * amss__CUI_MailDomain(self) 
struct amss *self;
{
    return(CUI_MailDomain);
}

long amss__CUI_MarkAsRead(self, cuid) 
struct amss *self;
int cuid;
{
    return(CUI_MarkAsRead(cuid));
}

long amss__CUI_MarkAsUnseen(self, cuid) 
struct amss *self;
int cuid;
{
    return(CUI_MarkAsUnseen(cuid));
}

long amss__CUI_NameReplyFile(self, cuid, code, fname) 
struct amss *self;
int cuid, code;
char *fname;
{
    return(CUI_NameReplyFile(cuid, code, fname));
}

long amss__CUI_OnSameHost(self) 
struct amss *self;
{
    return(CUI_OnSameHost);
}

long amss__CUI_PrefetchMessage(self, cuid, ReallyNext) 
struct amss *self;
int cuid, ReallyNext;
{
    return(CUI_PrefetchMessage(cuid, ReallyNext));
}

long amss__CUI_PrintBodyFromCUIDWithFlags(self, cuid, flags, printer)
struct amss *self;
int cuid, flags;
char *printer; 
{
    return(CUI_PrintBodyFromCUIDWithFlags(cuid, flags, printer));
}

void amss__CUI_PrintUpdates(self, dname, nickname)
struct amss *self;
char *dname, *nickname;
{
    CUI_PrintUpdates(dname, nickname);
}

long amss__CUI_ProcessMessageAttributes(self, cuid, snapshot) 
struct amss *self;
int cuid;
char *snapshot;
{
    return(CUI_ProcessMessageAttributes(cuid, snapshot));
}

long amss__CUI_PurgeDeletions(self, dirname) 
struct amss *self;
char *dirname;
{
    return(CUI_PurgeDeletions(dirname));
}

long amss__CUI_PurgeMarkedDirectories(self, ask, OfferQuit) 
struct amss *self;
boolean ask, OfferQuit;
{
    return(CUI_PurgeMarkedDirectories(ask, OfferQuit));
}

long amss__CUI_ReallyGetBodyToLocalFile(self, cuid, fname, ShouldDelete, MayFudge) 
struct amss *self;
int cuid;
char *fname;
int *ShouldDelete, MayFudge;
{
    return(CUI_ReallyGetBodyToLocalFile(cuid, fname, ShouldDelete, MayFudge));
}

long amss__CUI_RemoveDirectory(self, dirname)
struct amss *self;
char *dirname;
{
    return(CUI_RemoveDirectory(dirname));
}
long amss__CUI_RenameDir(self, oldname, newname) 
struct amss *self;
char *oldname, *newname;
{
    return(CUI_RenameDir(oldname, newname));
}

void amss__CUI_ReportAmbig(self, name, atype)
struct amss *self;
char *name, *atype;
{
    CUI_ReportAmbig(name, atype);
}

long amss__CUI_ResendMessage(self, cuid, tolist) 
struct amss *self;
int cuid;
char *tolist;
{
    return(CUI_ResendMessage(cuid, tolist));
}

long amss__CUI_RewriteHeaderLine(self, addr, newaddr) 
struct amss *self;
char *addr, *newaddr;
{
    return(CUI_RewriteHeaderLine(addr, newaddr));
}

long amss__CUI_RewriteHeaderLineInternal(self, addr, newaddr, maxdealiases, numfound, externalcount, formatct, stripct, trustct)
struct amss *self;
char *addr, *newaddr;
int maxdealiases, *numfound, *externalcount, *formatct, *stripct, *trustct;
{
    return(CUI_RewriteHeaderLineInternal(addr, newaddr, maxdealiases, numfound, externalcount, formatct, stripct, trustct));
}

char *amss__CUI_Rock(self)
struct amss *self;
{
    return(CUI_Rock);
}

void amss__CUI_SetClientVersion(self, vers)
struct amss *self;
char *vers;
{
    CUI_SetClientVersion(vers);
}

long amss__CUI_SetPrinter(self, printername) 
struct amss *self;
char *printername;
{
    return(CUI_SetPrinter(printername));
}

long amss__CUI_SnapIsRunning(self) 
struct amss *self;
{
    return(CUI_SnapIsRunning);
}

long amss__CUI_StoreFileToVice(self, localfile, vicefile) 
struct amss *self;
char *localfile, *vicefile;
{
    return(CUI_StoreFileToVice(localfile, vicefile));
}

long amss__CUI_SubmitMessage(self, infile, DeliveryOpts) 
struct amss *self;
char *infile;
long DeliveryOpts;
{
    return(CUI_SubmitMessage(infile, DeliveryOpts));
}

long amss__CUI_UndeleteMessage(self, cuid) 
struct amss *self;
int cuid;
{
    return(CUI_UndeleteMessage(cuid));
}

long amss__CUI_UseAmsDelivery(self) 
struct amss *self;
{
    return(CUI_UseAmsDelivery);
}

long amss__CUI_UseNameSep(self) 
struct amss *self;
{
    return(CUI_UseNameSep);
}

char * amss__CUI_VersionString(self) 
struct amss *self;
{
    return(CUI_VersionString);
}

char* amss__CUI_WhoIAm(self) 
struct amss *self;
{
    return(CUI_WhoIAm);
}

int amss__CUI_GetCuid(self, id, fullname, isdup)
struct amss *self;
char *id, *fullname;
int *isdup;
{
    return(GetCuid(id, fullname, isdup));
}

long amss__MS_AppendFileToFolder(self, filename, foldername) 
struct amss *self;
char *filename, *foldername;
{
    return(MS_AppendFileToFolder(filename, foldername));
}

long amss__MS_CheckAuthentication(self, auth) 
struct amss *self;
long *auth;
{
    return(MS_CheckAuthentication(auth));
}

long amss__MS_DebugMode(self, mslevel, snaplevel, malloclevel) 
struct amss *self;
int mslevel, snaplevel, malloclevel;
{
    return(MS_DebugMode(mslevel, snaplevel, malloclevel));
}

long amss__MS_DisambiguateFile(self, source, target, MustBeDir) 
struct amss *self;
char *source, *target;
long MustBeDir;
{
    return(MS_DisambiguateFile(source, target, MustBeDir));
}

long amss__MS_FastUpdateState(self) 
struct amss *self;
{
    return(MS_FastUpdateState());
}

long amss__MS_GetDirInfo(self, dirname, protcode, msgcount) 
struct amss *self;
char *dirname;
long *protcode, *msgcount;
{
    return(MS_GetDirInfo(dirname, protcode, msgcount));
}

long amss__MS_GetNewMessageCount(self, dirname, numnew, numtotal, lastolddate, InsistOnFetch)
struct amss *self;
char *dirname, *lastolddate;
long *numnew, *numtotal, InsistOnFetch;
{
    return(MS_GetNewMessageCount(dirname, numnew, numtotal, lastolddate, InsistOnFetch));
}

long amss__MS_GetNthSnapshot(self, dirname, which, snapshotbuf)
struct amss *self;
char *dirname;
long which;
char *snapshotbuf;
{
    return(MS_GetNthSnapshot(dirname, which, snapshotbuf));
}

long amss__MS_GetSearchPathEntry(self, which, buf, buflim) 
struct amss *self;
long which;
char *buf;
long buflim;
{
    return(MS_GetSearchPathEntry(which, buf, buflim));
}

long amss__MS_GetSubscriptionEntry(self, fullname, nickname, status)
struct amss *self;
char *fullname;
char *nickname;
long *status;
{
    return(MS_GetSubscriptionEntry(fullname, nickname, status));
}

long amss__MS_NameChangedMapFile(self, mapfile, mailonly, listall, numchanged, numunavailable, nummissing, numslowpokes, numfastfellas)
struct amss *self;
char *mapfile;
long mailonly;
long listall;
long *numchanged;
long *numunavailable;
long *nummissing;
long *numslowpokes;
long *numfastfellas;
{
    return(MS_NameChangedMapFile(mapfile, mailonly, listall, numchanged, numunavailable, nummissing, numslowpokes, numfastfellas));
}

long amss__MS_NameSubscriptionMapFile(self, root, mapfile) 
struct amss *self;
char *root;
char *mapfile;
{
    return(MS_NameSubscriptionMapFile(root, mapfile));
}

long amss__MS_MatchFolderName(self, pat, filename) 
struct amss *self;
char *pat;
char *filename;
{
    return(MS_MatchFolderName(pat, filename));
}

long amss__MS_ParseDate(self, indate, year, month, day, hour, min, sec, wday, gtm)
struct amss *self;
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
    return(MS_ParseDate(indate, year, month, day, hour, min, sec, wday, gtm));
}

long amss__MS_PrefetchMessage(self, dirname, id, getnext) 
struct amss *self;
char *dirname;
char *id;
long getnext;
{
    return(MS_PrefetchMessage(dirname, id, getnext));
}

long amss__MS_SetAssociatedTime(self, fullname, newvalue) 
struct amss *self;
char *fullname;
char *newvalue;
{
    return(MS_SetAssociatedTime(fullname, newvalue));
}

void amss__MS_SetCleanupZombies(self, doclean)
struct amss *self;
long doclean;
{
    MS_SetCleanupZombies(doclean);
}

long amss__MS_SetSubscriptionEntry(self, fullname, nickname, status)
struct amss *self;
char *fullname;
char *nickname;
long status;
{
    return(MS_SetSubscriptionEntry(fullname, nickname, status));
}

long amss__MS_UnlinkFile(self, filename) 
struct amss *self;
char *filename;
{
    return(MS_UnlinkFile(filename));
}

long amss__MS_UpdateState(self)
struct amss *self;
{
    return(MS_UpdateState());
}

long amss__MS_DomainHandlesFormatting(self, domname, retval)
struct amss *self;
char *domname;
long *retval;
{
    return(MS_DomainHandlesFormatting(domname, retval));
}

void amss__ReportSuccess(self, s)
struct amss *self;
char *s;
{
    ReportSuccess(s);
}


void amss__ReportError(self, s, level, decode, err)
struct amss *self;
char *s;
int level, decode;
long err;
{
    if (decode) mserrcode = err;
    ReportError(s, level, decode);
}


int amss__GenericCompoundAction(self, v, prefix, cmds)
struct amss *self;
struct view *v;
char *prefix;
char *cmds;
{
    return(GenericCompoundAction(v, prefix, cmds));
}

int amss__GetBooleanFromUser(self, prompt, defaultans)
struct amss *self;
char *prompt;
int defaultans;
{
    return(GetBooleanFromUser(prompt, defaultans));
}

int amss__GetStringFromUser(self, prompt, buf, len, ispass)
struct amss *self;
char *prompt, *buf;
int len, ispass;
{
    return(GetStringFromUser(prompt, buf, len, ispass));
}

int amss__TildeResolve(self, in, out)
struct amss *self;
char *in, *out;
{
    return(TildeResolve(in, out));
}

int amss__OnlyMail(self)
struct amss *self;
{
    return(AMS_OnlyMail);
}

char *amss__ap_Shorten(self, fname)
struct amss *self;
char *fname;
{
    return(ap_Shorten(fname));
}

int amss__fwriteallchars(self, s, len, fp)
struct amss *self;
char *s;
int len;
FILE *fp;
{
    return(fwriteallchars(s, len, fp));
}

long amss__mserrcode(self)
struct amss *self;
{
    return(mserrcode);
}

int amss__vdown(self, errno)
struct amss *self;
int errno;
{
    return(vdown(errno));
}

int amss__AMS_ERRNO(self)
struct amss *self;
{
    return(AMS_ERRNO);
}

void amss__SubtleDialogs(self, besubtle)
struct amss *self;
boolean besubtle;
{
    SubtleDialogs(besubtle);
}

char *amss__DescribeProt(self, code)
struct amss *self;
int code;
{
    return(DescribeProt(code));
}

int amss__ChooseFromList(self, QVec, defans)
struct amss *self;
char **QVec;
int defans;
{
    return(ChooseFromList(QVec, defans));
}
int amss__CUI_GetAMSID(self, cuid, id, dir)
struct amss *self;
int cuid;
char **id, **dir;
{
    return(CUI_GetAMSID(cuid, id, dir));
}

char *amss__MessagesAutoBugAddress(self)
struct amss *self;
{
    return(MessagesAutoBugAddress);
}

int amss__UnScribe(self, ucode, ss, LineBuf, ct, fout)
struct amss *self;
int ucode;
struct ScribeState *ss;
char *LineBuf;
int ct;
FILE *fout;
{
    return(UnScribe(ucode, ss, LineBuf, ct, fout));
}

int amss__UnScribeFlush(self, ucode, ss, fout)
struct amss *self;
int ucode;
struct ScribeState *ss;
FILE *fout;
{
    return(UnScribeFlush(ucode, ss, fout));
}

int amss__UnScribeInit(self, vers, ss)
struct amss *self;
char *vers;
struct ScribeState *ss;
{
    return(UnScribeInit(vers, ss));
}

void amss__WriteOutUserEnvironment(self, fp, IsAboutMessages)
struct amss *self;
FILE *fp;
boolean IsAboutMessages;
{
    WriteOutUserEnvironment(fp, IsAboutMessages);
}

char *amss__ams_genid(self, isfilename)
struct amss *self;
boolean isfilename;
{
    return(ams_genid(isfilename));
}

int amss__CheckAMSUseridPlusWorks(self, dom)
struct amss *self;
char *dom;
{
    return(CheckAMSUseridPlusWorks(dom));
}

static int TimerInit() {
    ams_TimerInit();
}
