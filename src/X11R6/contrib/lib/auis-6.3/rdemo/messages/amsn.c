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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/rdemo/messages/RCS/amsn.c,v 1.4 1994/03/22 18:51:42 rr2b Exp $";
#endif

/* Until I come up with a better scheme, new functions here have to be added to SIX files -- ams.ch, amss.ch, amsn.ch (all identical specs) and the corresponding c files */ 

#include <class.h>
#include <ams.ih>
#include <im.ih>
#include <message.ih>
#include <amsn.eh>
#include <mailconf.h>
#include <cui.h>

extern char *CUI_MachineName, CUI_MailDomain[], *CUI_Rock, CUI_VersionString[], *CUI_WhoIAm, *ap_Shorten(), *DescribeProt(), *ams_genid();

extern long CUI_DeliveryType, CUI_LastCallFinished, CUI_OnSameHost, CUI_SnapIsRunning, CUI_UseAmsDelivery, CUI_UseNameSep, mserrcode;

boolean amsn__InitializeClass(c)
struct classheader *c;
{
    CheckAMSConfiguration();
    SetProgramVersion();
    MS_SetCleanupZombies(0);
    return(TRUE);
}

void amsn__CUI_BuildNickName(self, shortname, longname)
struct amsn *self;
char *shortname, *longname;
{
    CUI_BuildNickName(shortname, longname);
}

int amsn__CUI_CheckMailboxes(self, forwhat) 
struct amsn *self;
char *forwhat;
{
    return(CUI_CheckMailboxes(forwhat));
}

long amsn__CUI_CloneMessage(self, cuid, DirName, code) 
struct amsn *self;
int cuid;
char *DirName;
int code;
{
    return(CUI_CloneMessage(cuid, DirName, code));
}

long amsn__CUI_CreateNewMessageDirectory(self, dir, bodydir) 
struct amsn *self;
char *dir, *bodydir;
{
    return(CUI_CreateNewMessageDirectory(dir, bodydir));
}

long amsn__CUI_DeleteMessage(self, cuid) 
struct amsn *self;
int cuid;
{
    return(CUI_DeleteMessage(cuid));
}

long amsn__CUI_DeliveryType(self) 
struct amsn *self;
{
    return(CUI_DeliveryType);
}

long amsn__CUI_DirectoriesToPurge(self) 
struct amsn *self;
{
    return(CUI_DirectoriesToPurge());
}

long amsn__CUI_DisambiguateDir(self, shortname, longname) 
struct amsn *self;
char *shortname, *longname;
{
    return(CUI_DisambiguateDir(shortname, longname));
}

long amsn__CUI_DoesDirNeedPurging(self, name) 
struct amsn *self;
char *name;
{
    return(CUI_DoesDirNeedPurging(name));
}

void amsn__CUI_EndConversation(self)
struct amsn *self;
{
    CUI_EndConversation();
}

long amsn__CUI_GenLocalTmpFileName(self, name) 
struct amsn *self;
char *name;
{
    return(CUI_GenLocalTmpFileName(name));
}

long amsn__CUI_GenTmpFileName(self, name) 
struct amsn *self;
char *name;
{
    return(CUI_GenTmpFileName(name));
}

long amsn__CUI_GetFileFromVice(self, tmp_file, vfile) 
struct amsn *self;
char *tmp_file, *vfile;
{
    return(CUI_GetFileFromVice(tmp_file, vfile));
}

long amsn__CUI_GetHeaderContents(self, cuid, hdrname, hdrnum, hdrbuf, lim)
struct amsn *self;
int cuid;
char *hdrname;
int hdrnum;
char *hdrbuf;
int lim;
{
    return(CUI_GetHeaderContents(cuid, hdrname, hdrnum, hdrbuf, lim));
}

long amsn__CUI_GetHeaders(self, dirname, date64, headbuf, lim, startbyte, nbytes, status, RegisterCuids)
struct amsn *self;
char *dirname, *date64, *headbuf;
int lim, startbyte, *nbytes, *status, RegisterCuids;
{
    return(CUI_GetHeaders(dirname, date64, headbuf, lim, startbyte, nbytes, status, RegisterCuids));
}

long amsn__CUI_GetSnapshotFromCUID(self, cuid, Sbuf) 
struct amsn *self;
int cuid;
char *Sbuf;
{
    return(CUI_GetSnapshotFromCUID(cuid, Sbuf));
}

long amsn__CUI_HandleMissingFolder(self, dname) 
struct amsn *self;
char *dname;
{
    return(CUI_HandleMissingFolder(dname));
}

static int TimerInit();
long amsn__CUI_Initialize(self, TimerFunction, rock) 
struct amsn *self;
int (*TimerFunction)();
char *rock;
{
    long result;

    message_DisplayString(NULL, 10, "Initializing Internal Message Server...");
    im_ForceUpdate();
    if (!TimerFunction) TimerFunction = TimerInit;
    result = CUI_Initialize(TimerFunction, rock);
    setpag();
    return (result);
}

long amsn__CUI_LastCallFinished(self) 
struct amsn *self;
{
    return(CUI_LastCallFinished);
}

char * amsn__CUI_MachineName(self) 
struct amsn *self;
{
    return(CUI_MachineName);
}

char * amsn__CUI_MailDomain(self) 
struct amsn *self;
{
    return(CUI_MailDomain);
}

long amsn__CUI_MarkAsRead(self, cuid) 
struct amsn *self;
int cuid;
{
    return(CUI_MarkAsRead(cuid));
}

long amsn__CUI_MarkAsUnseen(self, cuid) 
struct amsn *self;
int cuid;
{
    return(CUI_MarkAsUnseen(cuid));
}

long amsn__CUI_NameReplyFile(self, cuid, code, fname) 
struct amsn *self;
int cuid, code;
char *fname;
{
    return(CUI_NameReplyFile(cuid, code, fname));
}

long amsn__CUI_OnSameHost(self) 
struct amsn *self;
{
    return(CUI_OnSameHost);
}

long amsn__CUI_PrefetchMessage(self, cuid, ReallyNext) 
struct amsn *self;
int cuid, ReallyNext;
{
    return(CUI_PrefetchMessage(cuid, ReallyNext));
}

long amsn__CUI_PrintBodyFromCUIDWithFlags(self, cuid, flags, printer)
struct amsn *self;
int cuid, flags;
char *printer; 
{
    return(CUI_PrintBodyFromCUIDWithFlags(cuid, flags, printer));
}

void amsn__CUI_PrintUpdates(self, dname, nickname)
struct amsn *self;
char *dname, *nickname;
{
    CUI_PrintUpdates(dname, nickname);
}

long amsn__CUI_ProcessMessageAttributes(self, cuid, snapshot) 
struct amsn *self;
int cuid;
char *snapshot;
{
    return(CUI_ProcessMessageAttributes(cuid, snapshot));
}

long amsn__CUI_PurgeDeletions(self, dirname) 
struct amsn *self;
char *dirname;
{
    return(CUI_PurgeDeletions(dirname));
}

long amsn__CUI_PurgeMarkedDirectories(self, ask, OfferQuit) 
struct amsn *self;
boolean ask, OfferQuit;
{
    return(CUI_PurgeMarkedDirectories(ask, OfferQuit));
}

long amsn__CUI_ReallyGetBodyToLocalFile(self, cuid, fname, ShouldDelete, MayFudge) 
struct amsn *self;
int cuid;
char *fname;
int *ShouldDelete, MayFudge;
{
    return(CUI_ReallyGetBodyToLocalFile(cuid, fname, ShouldDelete, MayFudge));
}

long amsn__CUI_RemoveDirectory(self, dirname)
struct amsn *self;
char *dirname;
{
    return(CUI_RemoveDirectory(dirname));
}
long amsn__CUI_RenameDir(self, oldname, newname) 
struct amsn *self;
char *oldname, *newname;
{
    return(CUI_RenameDir(oldname, newname));
}

void amsn__CUI_ReportAmbig(self, name, atype)
struct amsn *self;
char *name, *atype;
{
    CUI_ReportAmbig(name, atype);
}

long amsn__CUI_ResendMessage(self, cuid, tolist) 
struct amsn *self;
int cuid;
char *tolist;
{
    message_DisplayString(NULL, 100,
			  "We are sorry, but sending messages from the demo account is disabled.");
    return ((long) 0);
}

long amsn__CUI_RewriteHeaderLine(self, addr, newaddr) 
struct amsn *self;
char *addr, *newaddr;
{
    return(CUI_RewriteHeaderLine(addr, newaddr));
}

long amsn__CUI_RewriteHeaderLineInternal(self, addr, newaddr, maxdealiases, numfound, externalcount, formatct, stripct, trustct)
struct amsn *self;
char *addr, *newaddr;
int maxdealiases, *numfound, *externalcount, *formatct, *stripct, *trustct;
{
    return(CUI_RewriteHeaderLineInternal(addr, newaddr, maxdealiases, numfound, externalcount, formatct, stripct, trustct));
}

char *amsn__CUI_Rock(self)
struct amsn *self;
{
    return(CUI_Rock);
}

void amsn__CUI_SetClientVersion(self, vers)
struct amsn *self;
char *vers;
{
    CUI_SetClientVersion(vers);
}

long amsn__CUI_SetPrinter(self, printername) 
struct amsn *self;
char *printername;
{
    return(CUI_SetPrinter(printername));
}

long amsn__CUI_SnapIsRunning(self) 
struct amsn *self;
{
    return(CUI_SnapIsRunning);
}

long amsn__CUI_StoreFileToVice(self, localfile, vicefile) 
struct amsn *self;
char *localfile, *vicefile;
{
    return(CUI_StoreFileToVice(localfile, vicefile));
}

long amsn__CUI_SubmitMessage(self, infile, DeliveryOpts) 
struct amsn *self;
char *infile;
long DeliveryOpts;
{
    return(CUI_SubmitMessage(infile, DeliveryOpts));
}

long amsn__CUI_UndeleteMessage(self, cuid) 
struct amsn *self;
int cuid;
{
    return(CUI_UndeleteMessage(cuid));
}

long amsn__CUI_UseAmsDelivery(self) 
struct amsn *self;
{
    return(CUI_UseAmsDelivery);
}

long amsn__CUI_UseNameSep(self) 
struct amsn *self;
{
    return(CUI_UseNameSep);
}

char * amsn__CUI_VersionString(self) 
struct amsn *self;
{
    return(CUI_VersionString);
}

char* amsn__CUI_WhoIAm(self) 
struct amsn *self;
{
    return(CUI_WhoIAm);
}

int amsn__CUI_GetCuid(self, id, fullname, isdup)
struct amsn *self;
char *id, *fullname;
int *isdup;
{
    return(GetCuid(id, fullname, isdup));
}

long amsn__MS_AppendFileToFolder(self, filename, foldername) 
struct amsn *self;
char *filename, *foldername;
{
    return(MS_AppendFileToFolder(filename, foldername));
}

long amsn__MS_CheckAuthentication(self, auth) 
struct amsn *self;
long *auth;
{
    return(MS_CheckAuthentication(auth));
}

long amsn__MS_DebugMode(self, mslevel, snaplevel, malloclevel) 
struct amsn *self;
int mslevel, snaplevel, malloclevel;
{
    return(MS_DebugMode(mslevel, snaplevel, malloclevel));
}

long amsn__MS_DisambiguateFile(self, source, target, MustBeDir) 
struct amsn *self;
char *source, *target;
long MustBeDir;
{
    return(MS_DisambiguateFile(source, target, MustBeDir));
}

long amsn__MS_FastUpdateState(self) 
struct amsn *self;
{
    return(MS_FastUpdateState());
}

long amsn__MS_GetDirInfo(self, dirname, protcode, msgcount) 
struct amsn *self;
char *dirname;
long *protcode, *msgcount;
{
    return(MS_GetDirInfo(dirname, protcode, msgcount));
}

long amsn__MS_GetNewMessageCount(self, dirname, numnew, numtotal, lastolddate, InsistOnFetch)
struct amsn *self;
char *dirname, *lastolddate;
long *numnew, *numtotal, InsistOnFetch;
{
    return(MS_GetNewMessageCount(dirname, numnew, numtotal, lastolddate, InsistOnFetch));
}

long amsn__MS_GetNthSnapshot(self, dirname, which, snapshotbuf)
struct amsn *self;
char *dirname;
long which;
char *snapshotbuf;
{
    return(MS_GetNthSnapshot(dirname, which, snapshotbuf));
}

long amsn__MS_GetSearchPathEntry(self, which, buf, buflim) 
struct amsn *self;
long which;
char *buf;
long buflim;
{
    return(MS_GetSearchPathEntry(which, buf, buflim));
}

long amsn__MS_GetSubscriptionEntry(self, fullname, nickname, status)
struct amsn *self;
char *fullname;
char *nickname;
long *status;
{
    return(MS_GetSubscriptionEntry(fullname, nickname, status));
}

long amsn__MS_NameChangedMapFile(self, mapfile, mailonly, listall, numchanged, numunavailable, nummissing, numslowpokes, numfastfellas)
struct amsn *self;
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

long amsn__MS_NameSubscriptionMapFile(self, root, mapfile) 
struct amsn *self;
char *root;
char *mapfile;
{
    return(MS_NameSubscriptionMapFile(root, mapfile));
}

long amsn__MS_MatchFolderName(self, pat, filename) 
struct amsn *self;
char *pat;
char *filename;
{
    return(MS_MatchFolderName(pat, filename));
}

long amsn__MS_ParseDate(self, indate, year, month, day, hour, min, sec, wday, gtm)
struct amsn *self;
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

long amsn__MS_PrefetchMessage(self, dirname, id, getnext) 
struct amsn *self;
char *dirname;
char *id;
long getnext;
{
    return(MS_PrefetchMessage(dirname, id, getnext));
}

long amsn__MS_SetAssociatedTime(self, fullname, newvalue) 
struct amsn *self;
char *fullname;
char *newvalue;
{
    return(MS_SetAssociatedTime(fullname, newvalue));
}

void amsn__MS_SetCleanupZombies(self, doclean)
struct amsn *self;
long doclean;
{
    MS_SetCleanupZombies(doclean);
}

long amsn__MS_SetSubscriptionEntry(self, fullname, nickname, status)
struct amsn *self;
char *fullname;
char *nickname;
long status;
{
    return(MS_SetSubscriptionEntry(fullname, nickname, status));
}

long amsn__MS_UnlinkFile(self, filename) 
struct amsn *self;
char *filename;
{
    return(MS_UnlinkFile(filename));
}

long amsn__MS_UpdateState(self)
struct amsn *self;
{
    return(MS_UpdateState());
}

void amsn__ReportSuccess(self, s)
struct amsn *self;
char *s;
{
    ReportSuccess(s);
}


long amsn__MS_DomainHandlesFormatting(self, domname, retval)
struct amsn *self;
char *domname;
long *retval;
{
    return(MS_DomainHandlesFormatting(domname, retval));
}

void amsn__ReportError(self, s, level, decode, err)
struct amsn *self;
char *s;
int level, decode;
long err;
{
    if (decode) mserrcode = err;
    ReportError(s, level, decode);
}

int amsn__GenericCompoundAction(self, v, prefix, cmds)
struct amsn *self;
struct view *v;
char *prefix;
char *cmds;
{
    return(GenericCompoundAction(v, prefix, cmds));
}

int amsn__GetBooleanFromUser(self, prompt, defaultans)
struct amsn *self;
char *prompt;
int defaultans;
{
    return(GetBooleanFromUser(prompt, defaultans));
}

int amsn__GetStringFromUser(self, prompt, buf, len, ispass)
struct amsn *self;
char *prompt, *buf;
int len, ispass;
{
    return(GetStringFromUser(prompt, buf, len, ispass));
}

int amsn__TildeResolve(self, in, out)
struct amsn *self;
char *in, *out;
{
    return(TildeResolve(in, out));
}

int amsn__OnlyMail(self)
struct amsn *self;
{
    return(AMS_OnlyMail);
}

char *amsn__ap_Shorten(self, fname)
struct amsn *self;
char *fname;
{
    return(ap_Shorten(fname));
}

int amsn__fwriteallchars(self, s, len, fp)
struct amsn *self;
char *s;
int len;
FILE *fp;
{
    return(fwriteallchars(s, len, fp));
}

long amsn__mserrcode(self)
struct amsn *self;
{
    return(mserrcode);
}

int amsn__vdown(self, errno)
struct amsn *self;
int errno;
{
    return(vdown(errno));
}

int amsn__AMS_ERRNO(self)
struct amsn *self;
{
    return(AMS_ERRNO);
}

void amsn__SubtleDialogs(self, besubtle)
struct amsn *self;
boolean besubtle;
{
    SubtleDialogs(besubtle);
}

char *amsn__DescribeProt(self, code)
struct amsn *self;
int code;
{
    return(DescribeProt(code));
}

int amsn__ChooseFromList(self, QVec, defans)
struct amsn *self;
char **QVec;
int defans;
{
    return(ChooseFromList(QVec, defans));
}
int amsn__CUI_GetAMSID(self, cuid, id, dir)
struct amsn *self;
int cuid;
char **id, **dir;
{
    return(CUI_GetAMSID(cuid, id, dir));
}

char *amsn__MessagesAutoBugAddress(self)
struct amsn *self;
{
    return(MessagesAutoBugAddress);
}

int amsn__UnScribe(self, ucode, ss, LineBuf, ct, fout)
struct amsn *self;
int ucode;
struct ScribeState *ss;
char *LineBuf;
int ct;
FILE *fout;
{
    return(UnScribe(ucode, ss, LineBuf, ct, fout));
}

int amsn__UnScribeFlush(self, ucode, ss, fout)
struct amsn *self;
int ucode;
struct ScribeState *ss;
FILE *fout;
{
    return(UnScribeFlush(ucode, ss, fout));
}

int amsn__UnScribeInit(self, vers, ss)
struct amsn *self;
char *vers;
struct ScribeState *ss;
{
    return(UnScribeInit(vers, ss));
}

void amsn__WriteOutUserEnvironment(self, fp, IsAboutMessages)
struct amsn *self;
FILE *fp;
boolean IsAboutMessages;
{
    WriteOutUserEnvironment(fp, IsAboutMessages);
}

char *amsn__ams_genid(self, isfilename)
struct amsn *self;
boolean isfilename;
{
    return(ams_genid(isfilename));
}

int amsn__CheckAMSUseridPlusWorks(self, dom)
struct amsn *self;
char *dom;
{
    return(CheckAMSUseridPlusWorks(dom));
}

static int TimerInit() {
    ams_TimerInit();
}
