/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


/* Until I come up with a better scheme, new functions here have to be added to SIX files -- ams.ch, amss.ch, amsn.ch (all identical specs) and the corresponding c files */ 

class amss: ams {
    classprocedures:
      InitializeClass() returns boolean;
    overrides:
      GenericCompoundAction(struct view *v, char *prefix, char *cmds) returns int;
      GetBooleanFromUser(char *prompt, int defaultans) returns int;
      GetStringFromUser(char *prompt, char *buf, int len, int IsPassword) returns int;
      ChooseFromList(char **QVec, int defans) returns int;
      ReportError(char *text, int level, int Decode, long mserrcode);
      ReportSuccess(char *text);
      TildeResolve(char *in, char *out) returns int;
      OnlyMail() returns int;
      ap_Shorten(char *fname) returns char*;
      fwriteallchars(char *s, int len, FILE *fp) returns int;
      mserrcode() returns long;
      vdown(int errno) returns int;
      CUI_BuildNickName(char *shortname, char *longname);
      CUI_CheckMailboxes(char *forwhat) returns int;
      CUI_CloneMessage(int cuid, char *DirName, int code) returns long;
      CUI_CreateNewMessageDirectory(char *dir, char *bodydir) returns long;
      CUI_DeleteMessage(int cuid) returns long;
      CUI_DeliveryType() returns long;
      CUI_DirectoriesToPurge() returns long;
      CUI_DisambiguateDir(char *shortname, char *longname) returns long;
      CUI_DoesDirNeedPurging(char *name) returns long;
      CUI_EndConversation();
      CUI_GenLocalTmpFileName(char *name) returns long;
      CUI_GenTmpFileName(char *name) returns long;
      CUI_GetFileFromVice(char *tmp_file, char *vfile) returns long;
      CUI_GetHeaderContents(int cuid, char *hdrname, int hdrnum, char *hdrbuf, int lim) returns long;
      CUI_GetHeaders(char *dirname, char *date64, char *headbuf, int lim, int startbyte, int *nbytes, int *status, int RegisterCuids) returns long;
      CUI_GetSnapshotFromCUID(int cuid, char *Sbuf) returns long;
      CUI_HandleMissingFolder(char *dname) returns long;
      CUI_Initialize(proc TimerFunction, char *rock) returns long;
      CUI_LastCallFinished() returns long;
      CUI_MachineName() returns char *;
      CUI_MailDomain() returns char *;
      CUI_MarkAsRead(int cuid) returns long;
      CUI_MarkAsUnseen(int cuid) returns long;
      CUI_NameReplyFile(int cuid, int code, char *fname) returns long;
      CUI_OnSameHost() returns long;
      CUI_PrefetchMessage(int cuid, int ReallyNext) returns long;
      CUI_PrintBodyFromCUIDWithFlags(int cuid, int flags, char *printer) returns long;
      CUI_PrintUpdates(char *dname, char *nickname);
      CUI_ProcessMessageAttributes(int cuid, char *snapshot) returns long;
      CUI_PurgeDeletions(char *dirname) returns long;
      CUI_PurgeMarkedDirectories(boolean ask, boolean OfferQuit) returns long;
      CUI_ReallyGetBodyToLocalFile(int cuid, char *fname, int *ShouldDelete, int MayFudge) returns long;
      CUI_RemoveDirectory(char *dirname) returns long;
      CUI_RenameDir(char *oldname, char *newname) returns long;
      CUI_ReportAmbig(char *name, char *atype);
      CUI_ResendMessage(int cuid, char *tolist) returns long;
      CUI_RewriteHeaderLine(char *addr, char *newaddr) returns long;
      CUI_RewriteHeaderLineInternal(char *addr, char *newaddr, int maxdealiases, int *numfound, int *externalcount, int format, int strip, int trust) returns long;
      CUI_Rock() returns char *;
      CUI_SetClientVersion(char *vers);
      CUI_SetPrinter(char *printername) returns long;
      CUI_SnapIsRunning() returns long;
      CUI_StoreFileToVice(char *localfile, char *vicefile) returns long;
      CUI_SubmitMessage(char *infile, long DeliveryOpts) returns long;
      CUI_UndeleteMessage(int cuid) returns long;
      CUI_UseAmsDelivery() returns long;
      CUI_UseNameSep() returns long;
      CUI_VersionString() returns char *;
      CUI_WhoIAm() returns char*;
      CUI_GetCuid(char *id, char *fullname, int *isdup) returns int;
      CUI_GetAMSID(int cuid, char **id, char **dir) returns int;
      MS_AppendFileToFolder(char *filename, char *foldername) returns long;
      MS_CheckAuthentication(long *auth) returns long;
      MS_DebugMode(int mslevel, int snaplevel, int malloclevel) returns long;
      MS_DisambiguateFile(char *source, char *target, long MustBeDir) returns long;
      MS_FastUpdateState() returns long;
      MS_GetDirInfo(char *dirname, long *protcode, long *msgcount) returns long;
      MS_GetNewMessageCount(char *dirname, long *numnew, long *numtotal, char *lastolddate, long InsistOnFetch) returns long;
      MS_GetNthSnapshot(char *dirname, long which, char *snapshotbuf) returns long;
      MS_GetSearchPathEntry(long which, char *buf, long buflim) returns long;
      MS_GetSubscriptionEntry(char *fullname, char *nickname, long *status) returns long;
      MS_NameChangedMapFile(char *mapfile, long mailonly, long listall, long *numchanged, long *numunavailable, long * nummissing, long *numslowpokes, long *numfastfellas) returns long;
      MS_NameSubscriptionMapFile(char *root, char *mapfile) returns long;
      MS_ParseDate(char *indate, long *year, long *month, long *day, long *hour, long *min, long *sec, long *wday, long *gtm) returns long;
      MS_PrefetchMessage(char *dirname, char *id, long getnext) returns long;
      MS_SetAssociatedTime(char *fullname, char *newvalue) returns long;
      MS_SetCleanupZombies(long doclean);
      MS_SetSubscriptionEntry(char *fullname, char *nickname, long status) returns long;
      MS_UnlinkFile(char *filename) returns long;
      MS_UpdateState() returns long;
      MS_MatchFolderName(char *pattern, char *filename) returns long;
      MS_DomainHandlesFormatting(char *domname, long *retval) returns long;
      AMS_ERRNO() returns int;
      SubtleDialogs(boolean besubtle);
      DescribeProt(int code) returns char *;
      MessagesAutoBugAddress() returns char *;
      UnScribe(int ucode, struct ScribeState *ss, char *LineBuf, int ct, FILE *fout) returns int;
      UnScribeFlush(int ucode, struct ScribeState *ss, FILE *fout) returns int;
      UnScribeInit(char *vers, struct ScribeState *ss) returns int;
      WriteOutUserEnvironment(FILE *fp, boolean IsAboutMessages);
      ams_genid(boolean isfilename) returns char *;
      CheckAMSUseridPlusWorks(char *dom) returns int;
     RemoveErrorDialogWindow();
};
