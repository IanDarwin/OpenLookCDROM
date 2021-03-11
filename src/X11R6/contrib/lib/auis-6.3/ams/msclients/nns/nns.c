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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/nns.c,v 2.20 1993/09/29 20:15:15 gk5g Exp $";
#endif

/* nns - The Netnews Snarfer
** by Bob Glickstein and the Andrew Message System Group
** This program is a !&@^#% messageserver client.
*/

#include <andrewos.h>
#include <big.h>
#include <sys/stat.h>

#define ALT_GOURMAND_DIR ("/afs/andrew.cmu.edu/usr0/netbb/.MESSAGES/netnews/alt/gourmand")
#define ALT_GOURMAND_STR ("RECIPE:")
#define CLARI_DIR "/afs/andrew.cmu.edu/usr0/netbb/.MESSAGES/clari/"


/* The following structure holds global program options
*/

Globals_t       Globals;

int             MSDebugging = 0;       /* I shouldn't have to declare these */
unsigned char  *SnapVersionString = NULL;
char *CUI_ClientVersion= "nns $Revision: 2.20 $";

 /* ...or these... */

BizarreError(text, level)              /* Dummy function from cuinosnap, which
                                        * libmessageserver needs! */
char           *text;
int             level;
{
    fprintf(stderr, "<nns:warning>%s (%d)\n", text, level);
}

Machine_HandleClientSignal(signum, ActNormal)   /* Ditto */
int             signum, *ActNormal;
{
    printf("Machine_HandleClientSignal was called (signum=%d)\n", signum);
}

main(argc, argv, envp)
int             argc;
char          **argv, **envp;
{
    int             opt, status, dummy;
    GList_t         GList;
    extern int      optind;
    extern char    *optarg;

    Globals.Options.verbose = Globals.Options.runOnce = Globals.Options.dontDo = FALSE;
    Globals.Options.configFileName = DEFAULT_CONFIG_FILENAME;
    while ((opt = getopt(argc, argv, "1nvc:")) != EOF) {
	switch (opt) {
	    case 'c':
	        Globals.Options.configFileName = optarg;
		Verbiage(5, "Set configuration file name to:");
		Verbiage(5, optarg);
		break;
	    case 'v':
		++(Globals.Options.verbose);    /* Many -v's means very
					 * verbose */
		break;
	    case '1':
		Globals.Options.runOnce = TRUE;
		Verbiage(5, "Running only once");
		break;
	    case 'n':
		Globals.Options.dontDo = TRUE;
		Verbiage(5, "The don't-do flag was used, but is not yet implemented!");
		exit(NOTIMPLEMENTED);
		break;
	    default:
		fprintf(stderr, "Usage: %s [-1nv] [-c <configuration file>]\n", argv[0]);
		exit(STUPIDUSER);
		break;
	}
    }
    ConfInit(Globals.Options.configFileName);
    Verbiage(4, "Read config file");
    GLInit(&GList);
    Verbiage(4, "Initialized Grouplist");
    STInit(&(Globals.STable));
    Verbiage(4, "Initialized String Table");
    MS_Initialize(&dummy, FALSE);
    Verbiage(4, "Initialized Messageserver");
    if (Globals.Options.runOnce) {
	(void) ProcessDir(FailedDir, &GList);
	(void) rmdir(FailedDir); /* Will only work if empty, of course */
	if (!ConfIsPeakTime()) {
	    (void) ProcessDir(DelayedDir, &GList);
	    (void) rmdir(DelayedDir);
	}
	status = ProcessDir(ReadyBox, &GList);
	if (status == 0 && GetNewReadyBox()) status = ProcessDir(ReadyBox, &GList);
	if (status == CANT_OPEN_DIR) {
	    fprintf(stderr, "%s: Fatal error!  Couldn't open directory %s\n", argv[0], ReadyBox);
	    exit(DIRPROBLEMS);
	}
	MS_UpdateState();
	exit(0);
    }
    else {
	for (;;) {
	    (void) ProcessDir(FailedDir, &GList);
	    /* Will only work if empty, of course */
	    (void) rmdir(FailedDir);
	    if (!ConfCheckPeakTime()) {
		(void) ProcessDir(DelayedDir, &GList);
		(void) rmdir(DelayedDir);
	    }
	    status = ProcessDir(ReadyBox, &GList);
	    if (status == 0 && GetNewReadyBox()) status = ProcessDir(ReadyBox, &GList);
	    if (status <= 0) {
		switch (status) {
		case CANT_OPEN_DIR:
		    fprintf(stderr, "%s: Fatal error!  Couldn't open directory %s\n", argv[0], ReadyBox);
		    exit(DIRPROBLEMS);
		    break;
		case 0:
		    Verbiage(1, "Going to sleep");
		    sleep(RETRY_INTERVAL);
		}
	    }
	}
    }
}

int	       GetNewReadyBox()
{
    DIR		*dp, *opendir();
    DIRENT_TYPE *dirent, *readdir();
    char	oldestHoldDir[MAXPATHLEN + 1];
    char	HoldDirEntry[MAXPATHLEN + 1], *basename;
    struct stat sbuf;
    unsigned long oldestdirtime = -1L;

    dp = opendir(HoldDir);
    if (!dp) return 0;

    oldestHoldDir[0] = '\0';
    strcpy(HoldDirEntry, HoldDir);
    strcat(HoldDirEntry, "/");
    basename = HoldDirEntry + strlen(HoldDirEntry);

    while (dirent = readdir(dp)) {
	if (dirent->d_name[0] == '.') continue;
	strcpy(basename, dirent->d_name);
	if (stat(HoldDirEntry, &sbuf) < 0) continue;
	if (sbuf.st_mtime < oldestdirtime) {
	    oldestdirtime = sbuf.st_mtime;
	    strcpy(oldestHoldDir, HoldDirEntry);
	}
    }
    closedir(dp);
    if (oldestHoldDir[0] == '\0') return 0;

    if (rename(oldestHoldDir, ReadyBox) < 0) return 0;
    return 1;
}
 
int            ProcessDir(dir, GListp)
char           *dir;
GList_t        *GListp;
{
    int status;

    if ((status = GLBuildList(GListp, dir)) > 0) {
	Verbiage(2, "Built the grouplist; about to process...");
	ProcessList(GListp, dir);
	Verbiage(2, "Processed the grouplist");
	GLPurge(GListp);
    }
    else {
	if (status && status != CANT_OPEN_DIR) {
	    fprintf(stderr, "nns: Fatal error!  Unknown error in GLBuildList\n");
	    exit(UNKNOWN);
	}
    }
    return status;
}

void            ProcessList(gl, dir)
GList_t        *gl;
char           *dir;
{
    GListEntry_t   *gle;
    int             i, numUnlinks, numPosts, numFolders;
    struct MS_Directory Dir;
    char            currentFolder[MAXPATHLEN + 1], *gleFilename, *gleFolder, actualFile[MAXPATHLEN + 1], *filePtr;
    MCache_t        MCache;

    Verbiage(3, "In ProcessList");
    numUnlinks = numPosts = numFolders = 0;
    bzero(&Dir, sizeof(struct MS_Directory));
    Dir.fd = -1;
    Dir.LastIDHit = -1;
    Dir.OpenMode = -1;
    Dir.AttrNames = NULL;
    Dir.UNIXDir = NULL;
    Dir.IDs = NULL;
    Dir.NumIDs = 0;
    currentFolder[0] = '\0';
    strcpy(actualFile, dir);
    filePtr = actualFile + strlen(dir);
    *filePtr++ = '/';
    MCInit(&MCache);
    Verbiage(4, "Initialized Message Cache");
    for (i = 0; i < GLGetNum(gl); ++i) {
	gle = GLGetEntry(gl, i);
	if (!GLEGetIgnore(gle)) {
	    gleFilename = GLEGetFilename(gle);
	    Verbiage(1, "Processing file:");
	    Verbiage(1, gleFilename);
	    gleFolder = GLEGetFolder(gle);
	    Verbiage(1, "For folder:");
	    Verbiage(1, gleFolder);
	    if (!*currentFolder || strcmp(currentFolder, gleFolder)) {
		if (*currentFolder) {
		    CloseMSDir(&Dir, MD_APPEND);
		    Verbiage(2, "Closed folder:");
		    Verbiage(2, currentFolder);
		}
		bzero(&Dir, sizeof(struct MS_Directory));
		Dir.UNIXDir = gleFolder;
		Dir.fd = -1;       /* This is BOGUS, BOGUS, BOGUS */
		Dir.LastIDHit = -1;
		Dir.OpenMode = -1;
		Dir.AttrNames = NULL;
		Dir.AttrCount = -1;
		if (OpenMSDirectory(&Dir, MD_APPEND)) {
		    Verbiage(2, "Open of folder failed; ignoring this file");
		    GLIgnore(gl, gleFilename, dir);
		    currentFolder[0] = '\0';
		}
		else {
		    strcpy(currentFolder, gleFolder);
		    if (ReadOldMSDirectoryHead(&Dir)) {
			Verbiage(2, "Read of MS directory head failed; ignoring file");
			CloseMSDir(&Dir, MD_APPEND);
			GLIgnore(gl, gleFilename, dir);
			currentFolder[0] = '\0';
		    }
		    else {
			Verbiage(3, "Opened folder");
			++numFolders;
		    }
		}
	    }
	    if (*currentFolder) {   /* Was it a successful OpenMSDirectory? */
		strcpy(filePtr, gleFilename);
		if (!ProcessEntry(gle, actualFile, &Dir, &MCache, &numUnlinks)) {
		    Verbiage(2, "ProcessEntry failed; ignoring file");
		    GLIgnore(gl, gleFilename, dir);
		}
		else
		    ++numPosts;
	    }
	}
    }
    if (*currentFolder)
	CloseMSDir(&Dir, MD_APPEND);
    MCPurge(&MCache);                  /* If I write this code right, this
					 * should be a no-op */
    if (numUnlinks != 0)
	printf("# %d /usr/net\n", numUnlinks);  /* the only thing going to
						 * stdout. */
    if (numUnlinks != 0 || numPosts != 0 || numFolders != 0) {
	printf("(Processed %d files from dir %s to be %d postings in %d folders.)\n",
	       numUnlinks, dir, numPosts, numFolders);
    }
}


/* Returns success/failure */

int             ProcessEntry(gle, fullFilename, Dir, mc, UnlinkP)
GListEntry_t   *gle;
char           *fullFilename;
struct MS_Directory *Dir;
MCache_t       *mc;
int            *UnlinkP;
{
    struct MS_Message *Msg;
    int             shouldWrite, inCache;
    struct MS_CaptionTemplate CaptionTemplate;
    char            NewFileName[MAXPATHLEN + 1];

    Verbiage(3, "In ProcessEntry");
    if (ShouldAlreadyBeCached(gle) && (Msg = MCFind(mc, GLEGetFilename(gle)))) {
	Verbiage(2, "Message was cached");
	inCache = TRUE;
    }
    else {
	inCache = FALSE;
	if (!(Msg = (struct MS_Message *) malloc(sizeof(struct MS_Message))))
	    return (FALSE);
	bzero(Msg, sizeof(struct MS_Message));
	CaptionTemplate.datetype = DATETYPE_FROMHEADER;
	if (!strncmp(Dir->UNIXDir, CLARI_DIR, sizeof(CLARI_DIR)-1) &&
	    strcmp(Dir->UNIXDir+sizeof(CLARI_DIR)-1, "net/talk")) {
	    CaptionTemplate.basictype = BASICTEMPLATE_NOFROM;
	}
	else {
	    CaptionTemplate.basictype = BASICTEMPLATE_NORMAL;
	}
	if (ReadRawFile(fullFilename, Msg, TRUE)
	    || ParseMessageFromRawBody(Msg)
	    || AddNetnewsWideReplyHeader(Msg)
	    || (Msg->AuthUid = 0, Msg->AuthName = Msg->AuthCell = NULL, FALSE)
	    || InventID(Msg)
	    || BuildDateField(Msg)
	    || BuildReplyField(Msg)
	    || BuildAttributesField(Msg)
	    || BuildCaption(Msg, &CaptionTemplate, FALSE)) {
	    Verbiage(1, "Something failed building the message structure");
	    FreeMessage(Msg, TRUE);
	    return (FALSE);
	}
    }

    /*
      * Now we have a parsed message structure, whose file may or may not be
      * open, and which may or may not have just been created (alternative is
								* that it came from the cache)
      */

    if (Msg->OpenFD < 0) {             /* It needs to be opened */
	if ((Msg->OpenFD = open(fullFilename, O_RDONLY, 0)) < 0) {
	    Verbiage(2, "Couldn't reopen cached message; ignoring file.");
	    if (inCache) {
		MCDelete(mc, GLEGetFilename(gle));      /* This is cool, since
							 * GLIgnore will be
							 * called */
		Verbiage(2, "Message removed from cache");
	    }
	    FreeMessage(Msg, TRUE);
	    return (FALSE);
	}
	Verbiage(3, "Reopened cached message");
    }

    if (ShouldBeCached(gle)) {
	Verbiage(2, "Message belongs in cache");
	if (!inCache) {
	    Verbiage(2, "Trying to place into cache");
	    if (inCache = MCMake(mc, GLEGetFilename(gle), Msg))
		Verbiage(2, "Successfully placed in cache");
	    else
		Verbiage(2, "Caching failed");
	}
    }
    else {
	if (inCache) {
	    MCDelete(mc, GLEGetFilename(gle));
	    Verbiage(2, "Message removed from cache");
	    inCache = FALSE;
	}
    }

    shouldWrite = TRUE;

    /* Now for the alt.gourmand hack */
    if ((!strcmp(Dir->UNIXDir, ALT_GOURMAND_DIR))
	 && (!strncmp(Msg->ParsedStuff->HeadBody[HP_SUBJECT], ALT_GOURMAND_STR, strlen(ALT_GOURMAND_STR)))) {
	if (AddHeader(Msg,
		      "Content-Type: troff; 0; /usr/lib/tmac/tmac.an,/usr/lib/tmac/tmac.recipe")) {
	    if (inCache) {
		MCDelete(mc, GLEGetFilename(gle));
	    }
	    FreeMessage(Msg, TRUE);
	    return (FALSE);
	}
    }

    if (IsMessageAlreadyThere(Msg, Dir)) {
	Verbiage(1, "Duplicate message won't be added");
	shouldWrite = FALSE;
    }

    if (shouldWrite) {
	sprintf(NewFileName, "%s/+%s", Dir->UNIXDir, AMS_ID(Msg->Snapshot));

	Verbiage(1, "Mailbox file is going to database as file:");
	Verbiage(1, NewFileName);

	/*
	 * Note -- we used to try to rename the file, but this never works
	 * from a local disk into Vice
	 */

	if (WritePureFile(Msg, NewFileName, FALSE, 0644)) {
	    printf("nns (0): WritePureFile failed on Mailbox file %s\n     destined for %s,\n      with Msg->OpenFD of %d (ms: %d, %d, %d)\n", GLEGetFilename(gle), NewFileName, Msg->OpenFD, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
	    if (inCache) {
		MCDelete(mc, GLEGetFilename(gle));      /* This is cool, since
							 * GLIgnore will be
							 * called */
		Verbiage(2, "Message removed from cache");
	    }
	    FreeMessage(Msg, TRUE);
	    return (FALSE);
	}
	else {
	    Verbiage(2, "Wrote the file; adding to msgdir...");
	    if (AppendMessageToMSDir(Msg, Dir)) {
		Verbiage(1, "AppendMessageToMSDir failed; unlinking body...");
		unlink(NewFileName);
		if (inCache) {
		    MCDelete(mc, GLEGetFilename(gle));  /* This is cool, since
							 * GLIgnore will be
							 * called */
		    Verbiage(2, "Removed message from cache");
		}
		FreeMessage(Msg, TRUE);
		return (FALSE);
	    }
	}
    }


    if (Msg->OpenFD >= 0) {
	Verbiage(1, "Msg->OpenFD is open");
	close(Msg->OpenFD);            /* BOGUS: Return value?  Use vclose? */
	Verbiage(3, "Now it's closed");
	Msg->OpenFD = -1;
    }

    if (ShouldUnlink(gle)) {
	Verbiage(1, "Unlinking the file");
	if (unlink(fullFilename) == 0)
	    ++(*UnlinkP);              /* Ignore errors except for statistics. */
    }
    else
	Verbiage(2, "Not unlinking the file yet");

    if (!inCache)
	FreeMessage(Msg, TRUE);
    return (TRUE);
}

void            DotsToSlashesInPlace(string)
char           *string;
{
    char           *p = string;

    while (p = strchr(p, '.'))
	*(p++) = '/';
}

int             ShouldAlreadyBeCached(gle)
GListEntry_t   *gle;
{
    return (GLEGetBefore(gle) > 0);
}

int             ShouldBeCached(gle)
GListEntry_t   *gle;
{
    return (GLEGetAhead(gle) > 0);
}

int             ShouldRename(gle)
GListEntry_t   *gle;
{
    return (!GLEGetAhead(gle));
}

int             ShouldUnlink(gle)
GListEntry_t   *gle;
{
    return (!(GLEGetAhead(gle) || GLEGetIgnore(gle)));
}

void            Verbiage(level, string)
int             level;
char           *string;
{
    if (level <= Globals.Options.verbose)
	fprintf(stderr, "nns (%d): %s (ms: %d, %d, %d)\n", level, string, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
}

int             AddNetnewsWideReplyHeader(Msg)
struct MS_Message *Msg;
{
    char            header[GROUPSLINESIZE], *gptr, tmpchar, tmpchar2, *endptr, *end2, *folder, *p;
    int             first = TRUE, delay;

    sprintf(header, "X-Andrew-WideReply: ");
    if (gptr = Msg->ParsedStuff->HeadBody[HP_NEWSGROUPS]) {
	endptr = gptr + Msg->ParsedStuff->HeadBodyLen[HP_NEWSGROUPS];
	tmpchar = *endptr;
	*endptr = '\0';
	while (gptr < endptr) {
	    if (!(end2 = strchr(gptr, ',')))
		end2 = endptr;
	    tmpchar2 = *end2;
	    *end2 = '\0';
	    /* So we duplicate the work of GLAddEntries().  Big deal --jgm */
	    if (folder = ConfDirForGroup(gptr, &delay)) {
		/* Convert folder path into short name */
		while (folder && strncmp(folder, "/.MESSAGES", 10)) {
		    folder = strchr(folder+1, '/');
		}
		if (folder) folder = strchr(folder+1, '/');
		if (folder) {
		    folder++;	/* Skip leading slash */
		    /* Convert slashes to dots and append it to the header */
		    for (p = folder; *p; p++) {
			if (*p == '/') *p = '.';
		    }
		    if (first)
		      first = FALSE;
		    else
		      strcat(header, ",");
		    strcat(header, folder);
		}
	    }
	    *end2 = tmpchar2;
	    gptr = end2 + 1;
	}
	*endptr = tmpchar;
	if (first) {
	    Verbiage(1, "Empty newsgroups header; can't build widereply field");
	    return (0);
	}
	Verbiage(3, "Adding following widereply header:");
	Verbiage(3, header);
	return (AddHeader(Msg, header));
    }
    Verbiage(1, "No newsgroups header at all!");
    return (0);
}

char *xmalloc(size)
int size;
{
    char *retval = malloc(size);
    if (!retval) {
	fprintf(stderr, "nns: Virtual memory exhausted\n");
	exit(UNKNOWN);
    }
    return retval;
}
