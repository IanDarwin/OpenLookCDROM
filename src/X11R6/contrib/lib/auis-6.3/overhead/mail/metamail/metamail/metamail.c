/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/****************************************************** 
    Metamail -- A tool to help diverse mail readers 
                cope with diverse multimedia mail formats.

    Author:  Nathaniel S. Borenstein, Bellcore

 ******************************************************* */
#include <stdio.h>
#include <ctype.h>
#include <config.h>
#include <patchlevel.h>

#ifdef BORLAND
#define F_OK 0
extern unsigned _stklen = 16384;
extern char *mktemp(char *);
#define WRITE_BINARY	"w"
#else /* BORLAND */
#ifdef MICROSOFT
#include <malloc.h>
#include <stdlib.h>
#include <time.h>
#include <signal.h>
#define F_OK 0
#else
#include <pwd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/file.h>
#include <signal.h>

#ifndef AMIGA
#ifdef SYSV
#include <termio.h>
#include <unistd.h>
#else /* SYSV */
#include <sgtty.h>
#endif /* SYSV */
#endif /* AMIGA */
#endif /* MICROSOFT */
#endif /* BORLAND */
  
#if defined(bsdi)
#include <sys/ioctl_compat.h>
#include <system.h>
#endif

#ifdef SYSV
#define LPRTEMPLATE "lp %s"
#define LPRCOMMAND "lp"
#else
#define LPRTEMPLATE "lpr %s"
#define LPRCOMMAND "lpr"
#endif
#ifdef MSDOS
#define CATCOMMAND  "cat"
#define CATTEMPLATE "cat %s"
#define METAMAIL    "metamail"
#define TMPFILE_NAME_SIZE   128
#define MAX_FILE_NAME_SIZE 128
#define WRITE_BINARY	"wb"
#else /* MSDOS */
#ifdef AMIGA
extern char *MkRmScript();
#ifndef F_OK
#define F_OK (0)
#endif
#define CATCOMMAND  "Type"
#define CATTEMPLATE "Type %s"
#define METAMAIL    "metamail <*"
#define TMPFILE_NAME_SIZE     50
#define MAX_FILE_NAME_SIZE 256
#define WRITE_BINARY	"w"
#else /* AMIGA */
extern char **environ, *gets();
#define CATCOMMAND  "cat"
#define CATTEMPLATE "cat %s"
#define METAMAIL    "metamail"
#define TMPFILE_NAME_SIZE   1000
#define MAX_FILE_NAME_SIZE 1000
#define WRITE_BINARY	"w"
#endif /* AMIGA */
#endif /* MSDOS */

#ifndef NO_RLIMITS
#include <sys/resource.h>
#endif

#define CMDSIZE 1200 /* Maximum size of command to execute */

#define LINE_BUF_SIZE       2000
#ifndef MICROSOFT
extern char *malloc();
extern char *realloc();
#endif
extern char *getenv();
extern char *index();
extern char *rindex();
char fileToDelete[MAX_FILE_NAME_SIZE];

char *FindParam();
extern FILE *popen();
static char *nomem = "Out of memory!";
static char *mmversion = MM_VERSTRING;
static char *NoAskDefault = "text,text/plain,text/richtext";
static char *QuietDefault = CATCOMMAND;
static char *tmproot="";

struct MailcapEntry {
    char *contenttype;
    char *command;
    char *testcommand;
    int needsterminal;
    int copiousoutput;
    int needtofree;
    char *label;
    char *printcommand;
};

FILE *InputFP = NULL;

int MightAskBeforeExecuting = 1,
    DefinitelyNotTty = 0,
    MustNotBeTty = 0,
    MaybePageOutput = 0,
    MustPageOutput = 0,
    EatLeadingNewlines = 0,
    PrintSomeHeaders = 1,
    DoInBackground = 0,
    Quiet = 0,
    TransparentMode = 0,
    DeleteSourceFileWhenDone = 0,
    Is822Format = 1,
    DoDebug = 0,
    CParamsAlloced = 0,
    CParamsUsed = 0,
    YankMode = 0,
    UsingStandardInput = 0,
    PrintingMode = 0,
    JustWriteFiles = 0,
    ProcessingErrors = 0;

char *ContentType = NULL,
    *ContentEncoding = NULL,
    *MailerName = "unknown",
    *MailSubject = "Mail message",
    *MailFrom = "unknown sender",
    *MailSummary = "non-text mail message",
    *mailheaders = NULL,
    **CParams = NULL,
    **CParamValues = NULL,
    *JunkParameter = NULL;

#define ENCODING_NONE 0
#define ENCODING_BASE64 1
#define ENCODING_QUOTEDPRINTABLE 2
#define ENCODING_8BIT 3
#define ENCODING_UUENCODE -1	/* non-standard */
int EncodingCode = ENCODING_NONE;

struct part {
    char *ctype;
    char *fname;
    struct part *next;
};

struct NoAskItem {
    char *type;
    struct NoAskItem *next;
} *FirstNoAskItem = NULL,
  *FirstQuietItem = NULL;

#ifdef MICROSOFT
/* Need a function prototype for TryMailcapEntry because without it MSC
 * passes a pointer to the structure rather than the structure itself.
 */
TryMailcapEntry(struct MailcapEntry mc, char *SquirrelFile);
#endif

void PrintHeader();
void ConsumeRestOfPart();
void ParseContentParameters();

sigtype cleanup();

char *Cleanse(s) /* no leading or trailing space, all lower case */
char *s;
{
    char *tmp, *news;
    
    /* strip leading white space */
    while (*s && isspace((unsigned char) *s)) ++s;
    news = s;
    /* put in lower case */
    for (tmp=s; *tmp; ++tmp) {
        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
    }
    /* strip trailing white space */
    while (*--tmp && isspace((unsigned char) *tmp)) *tmp = 0;
    return(news);
}

char *UnquoteString(s)
char *s;
{
    char *ans, *t;

    if (*s != '"') return(s);
    ans = malloc(1+strlen(s));
    if (!ans) ExitWithError(nomem);
    ++s;
    t = ans;
    while (*s) {
        if (*s == '\\') {
            *t++ = *++s;
        } else if (*s == '"') {
            break;
        } else {
            *t++ = *s;
        }
        ++s;
    }
    *t = 0;
    return(ans);
}

sigtype
cleanup(signum) 
int signum;
{
    RestoreTtyState();
#if defined(MSDOS) || defined(AMIGA)
    exit(signum);
#else
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
#endif
}

char **Boundaries = NULL;
int BoundaryCt = 0, BoundaryAlloc = 0;
struct nextfile {
    char *filename;
    struct nextfile *next;
} *FileQueue=NULL, *LastInQueue = NULL;

void
ResetGlobals() {
    CParamsAlloced = 0;
    CParamsUsed = 0;

    ContentType = NULL;
    ContentEncoding = NULL;
    MailSubject = "Mail message";
    MailFrom = "unknown sender";
    MailSummary = "non-text mail message";
    mailheaders = getenv("MM_HEADERS");
    if (mailheaders) {
        char *s;
        s = malloc(15+strlen(mailheaders));
        if (!s) ExitWithError(nomem);
        sprintf(s, "MM_HEADERS=%s", mailheaders);
        mailheaders = s;
    }
    CParams = NULL;
    CParamValues = NULL;
    JunkParameter = NULL;
}

void modpath(auxpath)
char *auxpath;
{
    if (auxpath && *auxpath) {
        static char *newpath = 0;
	char *oldpath = newpath;
        char *path = getenv("PATH");

	if (!path) path = "";	/* give a default if no current path */
	newpath = malloc(7 + strlen(path) + strlen(auxpath));
        if (!newpath) ExitWithError(nomem);
        sprintf(newpath, "PATH=%s:%s", auxpath, path);
        putenv(newpath);
	if (oldpath) free(oldpath); 	/* free up any old allocated PATH */
    }
}

main(argc, argv)
int argc;
char **argv;
{
    int retcode;

    modpath(AUXPATH);
#ifndef MSDOS
    signal(SIGINT, cleanup);
#ifndef AMIGA
    signal(SIGILL, cleanup);
    signal(SIGTRAP, cleanup);
    signal(SIGIOT, cleanup);
    signal(SIGFPE, cleanup);
#ifndef linux
    signal(SIGEMT, cleanup);
    signal(SIGBUS, cleanup);
#endif
    signal(SIGSEGV, cleanup);
    signal(SIGTERM, cleanup);
#endif
#endif
#ifdef SIGXCPU
    signal(SIGXCPU, cleanup);
#endif
    tmproot = getenv("METAMAIL_TMPDIR");
    if (!tmproot) tmproot="/tmp";
    mailheaders = getenv("MM_HEADERS");
    if (mailheaders) {
        char *s;
        s = malloc(15+strlen(mailheaders));
        if (!s) ExitWithError(nomem);
        sprintf(s, "MM_HEADERS=%s", mailheaders);
        mailheaders = s;
    }
    fileToDelete[0] = 0;
    ProcessArguments(argc, argv); /* calls ExitWithError on error */
#if !defined(AMIGA) && !defined(MSDOS)
    if (MaybePageOutput) {
        /* Want to send stderr to pager too, sigh... */
        fflush(stderr);
        close(2);
        dup2(1, 2);
    }
#endif
    retcode = HandleMessage(NULL, 0);
    if (! UsingStandardInput) {
        fclose(InputFP);
    }
    if (fileToDelete[0]) {
        unlink(fileToDelete);
        fileToDelete[0] = 0;
    }
    while (FileQueue) {
        InputFP = fopen(FileQueue->filename, "r");
        if (!InputFP) ExitWithError("Can't read input file");
        if (DeleteSourceFileWhenDone) {
            strcpy(fileToDelete, FileQueue->filename);
        }
        ResetGlobals();
        retcode |= HandleMessage(NULL, 0);
        if (! UsingStandardInput) {
            fclose(InputFP);
        }
        if (fileToDelete[0]) {
            unlink(fileToDelete);
            fileToDelete[0] = 0;
        }
        FileQueue = FileQueue->next;
    }
    if (MustPageOutput) PauseForUser();
    exit(ProcessingErrors? -1 : retcode);
}

void
QueueNextFile(fname)
char *fname;
{
    struct nextfile *tmp = (struct nextfile *) malloc(sizeof (struct nextfile));
    if (!tmp) ExitWithError(nomem);
    tmp->filename = fname;
    tmp->next = NULL;
    if (FileQueue) {
        LastInQueue->next = tmp;
        LastInQueue = tmp;
    } else {
        FileQueue = tmp;
        LastInQueue = tmp;
    }
}

HandleMessage(SquirrelFile, nestingdepth)
char *SquirrelFile;
/* SquirrelFile, if non-NULL, is a place to save a recognized body instead of executing it. */
int nestingdepth;
{
    int FileWriteOnly = JustWriteFiles;

    if (FileWriteOnly) {
        if (!lc2strncmp(ContentType, "message/", 8)
             || !lc2strncmp(ContentType, "multipart/", 10)) {
            FileWriteOnly = 0; /* only leaf data */
        }
    }
    if (Is822Format) {
        Read822Prefix(SquirrelFile?0:1, nestingdepth);
    } else Is822Format = 1; /* this property is not recursive for multipart or message */
    PrepareMessage();
    if (!FileWriteOnly && !ProcessMailcapFiles(SquirrelFile)) return(0);
    if (!lc2strcmp(ContentType, "message")
         || !lc2strcmp(ContentType, "message/rfc822")) { 
        if (SquirrelFile) return(SaveSquirrelFile(SquirrelFile));
        ContentType = NULL; /* reset default */
        ContentEncoding = NULL; /* reset default */
        return(HandleMessage(NULL, nestingdepth+1)); /* simple recursion */
    }
    if (!lc2strncmp(ContentType, "multipart", 9)) { 
        char *boundary, *LineBuf, NewSquirrelFile[TMPFILE_NAME_SIZE];
        char *subtype = NULL;
        int currct, result, IsAlternative, WroteSquirrelFile, boundarylen;

        if (SquirrelFile) return(SaveSquirrelFile(SquirrelFile));
        boundary = FindParam("boundary");
        if (!boundary) boundary =JunkParameter; /* backward compatibility hack */
        if (!boundary) {
            ExitWithError("Bad message format -- multipart message has no boundary parameter!");
        }
        if (boundary[0] == '"') {
            boundary=UnquoteString(boundary);
        }
        subtype = index(ContentType, '/');
        if (subtype) {
            ++subtype;
            subtype = Cleanse(subtype);
        } else subtype = "mixed";
#ifndef MSDOS
        if (!MaybePageOutput) DoInBackground = strcmp(subtype, "parallel") ? 0 : 1;
#endif

        IsAlternative = strcmp(subtype, "alternative") ? 0 : 1;
        if (IsAlternative) {
            MkTmpFileName(NewSquirrelFile);
            WroteSquirrelFile = 0;
        }
        LineBuf = malloc(LINE_BUF_SIZE);
        if (!LineBuf) ExitWithError(nomem);
        sprintf(LineBuf, "--%s", boundary);
        strcpy(boundary, LineBuf);
        boundarylen = strlen(boundary);
        if (BoundaryCt >= BoundaryAlloc) {
            BoundaryAlloc += 5;
            if (Boundaries) {
                Boundaries = (char **) realloc(Boundaries, BoundaryAlloc*sizeof(char *));
            } else {
                Boundaries = (char **) malloc(BoundaryAlloc*sizeof(char *));
            }
            if (!Boundaries) ExitWithError(nomem);
        }
        Boundaries[BoundaryCt++] = boundary;
        if (DoDebug) printf("Handling multipart as built-in here.  Boundary: %s\n", boundary);
        while (fgets(LineBuf, LINE_BUF_SIZE, InputFP)) { /* find start */
            if (!strncmp(LineBuf, boundary, boundarylen)
                 && ((LineBuf[boundarylen] == '\n')
                      || (LineBuf[boundarylen] == '-'
                           && LineBuf[boundarylen+1] == '-'
                           && LineBuf[boundarylen+2] == '\n'))) {
                break;
            }
        }
        free(LineBuf);
        currct = BoundaryCt;
        while(currct == BoundaryCt) {
            if (!strcmp(subtype, "digest")) {
                ContentType = "message/rfc822";
            } else {
                ContentType = NULL; /* reset default */
            }
            ContentEncoding = NULL; /* reset default */
            if (IsAlternative) {
                result = HandleMessage(NewSquirrelFile, nestingdepth+1);
            } else{
                result = HandleMessage(NULL, nestingdepth+1);
            }
            if (result) {
                /* Need to consume the rest of the part */
                ConsumeRestOfPart(NULL);
            } else {
                ++WroteSquirrelFile;
            }
        }
        /* Now we've seen the last encapsulation boundary, but if there is a "postfix"
            we must throw it away.*/
        if (BoundaryCt > 0) {
            ConsumeRestOfPart(NULL);
        }
        if (IsAlternative) {
            if (WroteSquirrelFile) {
                int retcode; 
                char Cmd[TMPFILE_NAME_SIZE + 15];
                sprintf(Cmd, "%s %s", METAMAIL, NewSquirrelFile);
                fflush(stdout); fflush(stderr);
                retcode = system(Cmd);
#ifdef MSDOS
                retcode = 0;
#endif
                unlink(NewSquirrelFile);
                return(retcode);
            } else {
                printf("Cannot handle any part of multipart/alternative message\n");
            }
        }
        return(0);
    }
    if (!FileWriteOnly && !TryBuiltIns(SquirrelFile)) return(0);
    if (!SquirrelFile) { /* Very last resort -- unrecognized types */
        char Fname[MAX_FILE_NAME_SIZE], *suggestedname, SugBuf[MAX_FILE_NAME_SIZE];
        FILE *fp;
        int ans = 0, octetstream, ecode=0;
        suggestedname = FindParam("name");
        if (!suggestedname) {
            MkTmpFileName(SugBuf);
            suggestedname = SugBuf;
        } else if (suggestedname[0] == '"') {
            suggestedname=UnquoteString(suggestedname);
        }
        octetstream = ! lc2strcmp(ContentType, "application/octet-stream");
        if (MightAskBeforeExecuting
             && !DefinitelyNotTty) {
            if (FileWriteOnly) {
                printf("\nThis message contains '%s`-format data.\n", ContentType);
            } else if (octetstream) {
                printf("This message contains raw digital data, which can either be viewed as text\nor written to a file.\n");
            } else {
                printf("\nThis message contains data in an unrecognized format, %s,\nwhich can either be viewed as text or written to a file.\n", ContentType);
            }
            while (!ans) {
                if (FileWriteOnly) {
                    ans = 2;
                } else {
                    printf("\nWhat do you want to do with the %s data?\n1 -- See it as text\n2 -- Write it to a file\n3 -- Just skip it\n\n", octetstream ? "raw" : ContentType);
                    fgets(Fname, sizeof(Fname), stdin);
                    ans = atoi(Fname);
                }
                switch(ans) {
                    case 1:
                        TranslateInputToOutput(InputFP, stdout, EncodingCode, "text");
                        return(0);
                    case 2:
                        {
                        int needname = 1;
                        while (needname) {
                            needname = 0;
                            printf("Please enter the name of a file to which the data should be written\n(Default: %s) > ", suggestedname);
                            fflush(stdout);
                            fgets(Fname, sizeof(Fname), stdin);
                            Fname[strlen(Fname) - 1] = '\0'; /* bogus newline */
#if !defined(AMIGA) && !defined(MSDOS)
                            if (!Fname[0]) strcpy(Fname, suggestedname);
                            if (Fname[0] == '~' && Fname[1] == '/') {
                                char Fname2[1000];
                                int uid = getuid();
                                struct passwd *p;
                                p = getpwuid(uid);
                                if (!p) {
                                    MkTmpFileName(Fname);
                                    printf("Cannot figure out what ~ means, using temporary file %s instead\n", Fname);
                                } else {
                                    strcpy(Fname2, p->pw_dir);
                                    strcat(Fname2, "/");
                                    strcat(Fname2, Fname + 2);
                                    strcpy(Fname, Fname2);
                                }
                            }
#endif
                            if (access(Fname, F_OK) == 0) {
                                char *s, AnsBuf[50];
                                int overwriteans = -1;
                                do {
                                    printf("File %s exists.  Do you want to overwrite it (y/n) ?\n", Fname);
                                    s = gets(AnsBuf);
                                    if (!s) {
                                        overwriteans = 0;
                                    } else {
                                        while (s && *s && isspace((unsigned char) *s)) ++s;
                                        if (*s == 'y' || *s == 'Y' || !*s || *s == '\n') {
                                            overwriteans = 1;
                                        } else if (*s == 'n' || *s == 'N' || *s == 'q' || *s == 'Q') {
                                            overwriteans=0;
                                        }
                                    }
                                    if (overwriteans == -1) printf("Please answer yes or no.\n");
                                } while (overwriteans == -1);
                                if (overwriteans == 0) needname = 1;
                            }
                        }
                        }
                        break;
                    case 3:
                        ConsumeRestOfPart(NULL);
                        return(0);
                        break;
                    default:
                        ans = 0;
                        break;
                }
            }
        } else {
            int ct = 0;
            char *slash = rindex(suggestedname, '/');
#ifdef AMIGA
	    if (slash == NULL) {
                slash = rindex(suggestedname, ':');
            }
#endif
            if (slash)  {
                ++slash;
            } else {
                slash = suggestedname;
            }
            do {
                if (ct) {
#ifdef AMIGA
                    sprintf(Fname, "T:%d-%s", ct, slash);
                } else {
                    sprintf(Fname, "T:%s", slash);
#else
                    sprintf(Fname, "%s/%d-%s", tmproot, ct, slash);
                } else {
                    sprintf(Fname, "%s/%s", tmproot, slash);
#endif
                }
                ++ct;
            } while (access(Fname, F_OK) == 0);
            if (FileWriteOnly) {
                printf("\nThis message contains '%s`-format data.\n", ContentType);
            } else if (octetstream) {
                printf("\nThis message contains raw digital data,");
            } else {
                printf("\nThis message contains data in an unrecognized format, %s,", ContentType);
            }
            printf("\nwhich is being decoded and written to the file named \"%s\".\nIf you do not want this data, you probably should delete that file.\n", Fname);
        }
        if (Fname[0] == 0 || Fname[0] == '\n') {
            ConsumeRestOfPart(NULL);
        } else {
            fp = fopen(Fname, WRITE_BINARY);
            if (!fp) ExitWithError("Cannot open temporary file");
            TranslateInputToOutput(InputFP, fp, EncodingCode, ContentType);
            ecode = fclose(fp);
            if (ecode) {
                printf("Could not write file %s\n", Fname);
            } else {
                printf("Wrote file %s\n", Fname);
            }
        }
        if (!DefinitelyNotTty && MaybePageOutput && BoundaryCt > 0) PauseForUser();
        return(ecode);
    }
    return(-1); /* Unrecognized, really */
}

ProcessArguments(argc, argv)
int argc;
char **argv;
{
    int i, RunAsRootOK = 0;
    char *SourceFileName = NULL, *NoAskStr, *QuietStr;

    QuietStr = getenv("MM_QUIET");
    if (!QuietStr) {
        QuietStr=QuietDefault;
    }
    if (!strcmp(QuietStr, "1")) {
        Quiet = 1;
    } else {
        struct NoAskItem *qitem;
        char *s, *tmp;
        char *QuietCopy;

        Quiet = 0;
        QuietCopy = malloc(1+strlen(QuietStr));
        if (!QuietCopy) ExitWithError(nomem);
        strcpy(QuietCopy, QuietStr);
        for (tmp=QuietCopy; *tmp; ++tmp) {
            if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
        }
        do {
            s = index(QuietCopy, ',');
            if (s) *s++ = 0;
            qitem = (struct NoAskItem *) malloc(sizeof (struct NoAskItem));
            if (!qitem) ExitWithError(nomem);
            qitem->next = FirstQuietItem;
            qitem->type = QuietCopy;
            FirstQuietItem = qitem;
            QuietCopy = s;
        } while (QuietCopy);
    }
    if (getenv("MM_TRANSPARENT")) {
        TransparentMode = atoi(getenv("MM_TRANSPARENT")); /* Will not propogate recursively */
    }
    if (getenv("MM_RUNASROOT")) {
        RunAsRootOK = atoi(getenv("MM_RUNASROOT"));
    }
    if (getenv("MM_YANKMODE")) {
        YankMode = atoi(getenv("MM_YANKMODE")); /* Will not propogate recursively */
    }
    if (getenv("MM_DEBUG")) {
        DoDebug = atoi(getenv("MM_DEBUG"));
    }
    if (DoDebug) printf("Metamail Version %s, debugging turned on.\n", mmversion);
    NoAskStr = getenv("MM_NOASK");
    if (!NoAskStr) NoAskStr = NoAskDefault;
    if (!strcmp(NoAskStr, "1")) {
        MightAskBeforeExecuting = 0;
    } else {
        struct NoAskItem *nai;
        char *s, *tmp;
        char *NoAskCopy;

        NoAskCopy = malloc(1+strlen(NoAskStr));
        if (!NoAskCopy) ExitWithError(nomem);
        strcpy(NoAskCopy, NoAskStr);
        for (tmp=NoAskCopy; *tmp; ++tmp) {
            if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
        }
        do {
            s = index(NoAskCopy, ',');
            if (s) *s++ = 0;
            nai = (struct NoAskItem *) malloc(sizeof (struct NoAskItem));
            if (!nai) ExitWithError(nomem);
            nai->next = FirstNoAskItem;
            nai->type = NoAskCopy;
            FirstNoAskItem = nai;
            NoAskCopy = s;
        } while (NoAskCopy);
    }
    MailerName = getenv("MM_MAILER");
    if (!MailerName) MailerName = "unknown";
    if (getenv("MM_USEPAGER")) {
        MaybePageOutput = atoi(getenv("MM_USEPAGER"));
    }
    if ((getenv("MM_NOTTTY") && ((atoi(getenv("MM_NOTTTY"))) != 0))) {
        MustNotBeTty = 1;
    }
    if (MustNotBeTty
         || !isatty(0)
         || !isatty(1)) {
        DefinitelyNotTty = 1;
    }
    for (i=1; i<argc; ++i) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 'b':
		    Is822Format = 0;
		    break;
		case 'B':
#ifdef MSDOS
                    fprintf(stderr, "metamail warning: -B flag not supported on this system\n");
#else
		    DoInBackground = 1;
#endif
		    break;
                case 'c':
                    if (++i >= argc) usage();
                    ContentType = argv[i];
                    /* strip leading white space */
                    while (*ContentType && isspace((unsigned char) *ContentType)) ++ContentType;
                    StripTrailingSpace(ContentType);
                    ParseContentParameters(ContentType);
                    break;
                case 'd':
		    MightAskBeforeExecuting = 0;
		    break;
		case 'e':
		    EatLeadingNewlines = 1;
		    break;
		case 'E':
		    if (++i >= argc) usage();
		    ContentEncoding = argv[i];
		    break;
		case 'f':
		    if (++i >= argc) usage();
		    MailFrom = argv[i];
                    break;
                case 'h':
                    {
                    FILE *tmpfp;
                    PrintingMode = 1;
                    MightAskBeforeExecuting = 0;
#ifdef AMIGA
		    freopen("prt:", "w", stdout);
#else
                    /* Want to make all of stdout go to printer, and this 
                      is easier than changing every output statement,
                      at least on UNIX... */
                    tmpfp = popen(LPRCOMMAND, "w");
                    if (!tmpfp) {
                        ExitWithError("Cannot execute lpr command");
                    }
                    fflush(stdout);
                    close(1);
                    dup2(fileno(tmpfp), 1);
#endif
                    break;
                    }
		case 'm':
		    if (++i >= argc) usage();
		    MailerName = argv[i];
		    break;
		case 'p':
		    MaybePageOutput = 1;
		    break;
		case 'P':
                    MaybePageOutput = 1;
                    MustPageOutput = 1;
		    break;
		case 'r':
#ifdef MSDOS
                    fprintf(stderr, "metamail warning: -r flag not supported on this system\n");
#else
		    RunAsRootOK = 1;
#endif
                    break;
                case 'R':
#ifdef RESET_PROGRAM
                    system(RESET_PROGRAM);
                    if (DoDebug) printf("Executed reset\n");
#else
                    fprintf(stderr, "metamail warning: -R flag not supported on this system\n");
#endif
                    break;
		case 's':
		    if (++i >= argc) usage();
		    MailSubject = argv[i];
		    break;
                case 'T':
                    TransparentMode = 0;
                    break;
                case 'w':
                    JustWriteFiles = 1;
                    break;
                case 'q':
                    Quiet = 1;
		    PrintSomeHeaders = 0;
		    break;
		case 'x':
                    DefinitelyNotTty = 1;
                    MustNotBeTty = 1;
		    break;
                case 'y':
                    YankMode = 1;
                    break;
		case 'z':
		    DeleteSourceFileWhenDone = 1;
		    break;
		default:
		    usage();
	    }
	} else {
	    if (SourceFileName) {
                QueueNextFile(argv[i]);
	    } else {
		SourceFileName = argv[i];
	    }
	}
    }
    if (TransparentMode) {
        RunInNewWindow(argv, argc, &SourceFileName);
    }
    if (MaybePageOutput && DoInBackground) {
        fprintf(stderr, "metamail: -B is incompatible with -p, -P, and MM_USEPAGER\n");
        usage();
    }
    if (!Is822Format && !ContentType) {
	fprintf(stderr, "metamail:  -b requires -c.\n");
	usage();
    }
    if (DeleteSourceFileWhenDone && !SourceFileName) {
	fprintf(stderr, "metamail:  -z requires -f.\n");
	usage();
    }
#if !defined(AMIGA) && !defined(MSDOS)
    if (!RunAsRootOK && (getuid() == 0 || geteuid() == 0)) {
	fprintf(stderr, "You can not run MetaMail as root unless you use -r.");
	usage();
    }
#endif
    if (SourceFileName) {
        InputFP = fopen(SourceFileName, "r");
        if (!InputFP) ExitWithError("Can't read input file");
        if (DeleteSourceFileWhenDone) {
            strcpy(fileToDelete, SourceFileName);
        }
    } else {  /* input on stdin */
        UsingStandardInput = 1;
        if (MustNotBeTty) {
            InputFP = stdin;
            MaybePageOutput = 1;
            MightAskBeforeExecuting = 0;
        } else {
#if defined(MSDOS) || defined(AMIGA)
            InputFP = stdin;
            DefinitelyNotTty = 1;
            MaybePageOutput = 0;
#else
            int newfd = dup(0);
            FILE *newfp;
            if (newfd > 0) {
                InputFP = fdopen(newfd, "r");
                if (InputFP 
                     && ((newfp = fopen("/dev/tty", "r")) != NULL)
                     && !dup2(fileno(newfp), 0)) {
                    DefinitelyNotTty = 0;
                } else {
                    InputFP = stdin;
                    DefinitelyNotTty = 1;
                    MaybePageOutput = 0;
                }
            }
#endif
        }
    }
    if (DefinitelyNotTty && MaybePageOutput) {
        RunInNewWindow(argv, argc, &SourceFileName);
    }
    if (DefinitelyNotTty) {
        MaybePageOutput = 0;	/* Disable pager if I/O has been redirected */
    }
    return(0);
}

usage() {
    fprintf(stderr, "Usage:  metamail [-b] [-B] [-d] [-e] [-h] [-r] [-R] [-p]  [-P] [-x] [-y] [-z] [-c content-type] [-E content-transfer-encoding] [-f from-name] [-m mailername] [-s subject] [message-file-name]\n");
    ExitWithError(NULL);
}

RunInNewWindow(argv, argc, SourceFileNamePtr)
char **argv, **SourceFileNamePtr;
int argc;
{
    char *FullCmd, TmpName[TMPFILE_NAME_SIZE];
    int i, createdfile=0;
    if (!*SourceFileNamePtr) {
        char *LineBuf;
        FILE *fptmp;

        LineBuf = malloc(LINE_BUF_SIZE);
        if (!LineBuf) ExitWithError(nomem);
        /* Create it, ugh.  Also needs to affect later command. */
        MkTmpFileName(TmpName);
        DeleteSourceFileWhenDone = 1;
        fptmp = fopen(TmpName, WRITE_BINARY);
        if (!fptmp) ExitWithError("Can't open temporary file\n");
        while (fgets(LineBuf, LINE_BUF_SIZE, stdin)) {
            fputs(LineBuf, fptmp);
        }
        free(LineBuf);
        fclose(fptmp);
        *SourceFileNamePtr = TmpName;
        createdfile = 1;
    }
    FullCmd = malloc(CMDSIZE);
    if (!FullCmd) ExitWithError(nomem);
    if (TransparentMode) {
        /* In transparent mode, we want to produce stdout that is what we get in, and do EVERYTHING externally in a terminal window.  This is to make the truly brain-dead mailers like mailtool happy. I am NOT happy about having to do this.  */
        /* So, first we copy everything to stdout */
        sprintf(FullCmd, CATTEMPLATE, *SourceFileNamePtr);
        system(FullCmd); /* Cheesy way to do it */
        fflush(stdout); fflush(stderr);
    }
    /* Then we run ourselves in a terminal window */
    MailSummary = "Metamail"; /* for window label */
    CreateNewWindowPrefix(FullCmd);
    strcat(FullCmd, METAMAIL);
    strcat(FullCmd, " -P ");
    if (TransparentMode) strcat(FullCmd, "-T ");
    for (i=1; i<argc; ++i) {
        if (strncmp(argv[i], "-x", 2)
             && strncmp(argv[i], "-B", 2)
             && strncmp(argv[i], "-d", 2)) {
            strcat(FullCmd, argv[i]);
            strcat(FullCmd, " ");
        }
    }
    if (createdfile) {
        strcat(FullCmd, "-z ");
        strcat(FullCmd, *SourceFileNamePtr);
    }
    if (!MightAskBeforeExecuting) {
        strcat(FullCmd, " -d ");
        /* The special hack for -d is HORRIBLE, but xterm screws up with the -d option in the middle of the command line! */
    }
    if (DoInBackground) strcat(FullCmd, " &");
    DefinitelyNotTty = 0;
    SetUpEnvironment();
    if (DoDebug) fprintf(stderr, "Executing %s\n", FullCmd);
    fflush(stdout); fflush(stderr);
#ifdef MSDOS
    system(FullCmd);
    exit(0);	/* system() returns random number under MS-DOS */
#else
    exit(system(FullCmd));
#endif
}

/* Only one or the other set up builtins gets used,
  depending on whether or not we're in the middle of 
          a multipart/alternative body part */
struct MailcapEntry BuiltIns[] = {
    {"text/*", CATTEMPLATE, NULL, 0, 1, 0, "plain text", LPRTEMPLATE},
    {NULL, NULL, NULL, 0, 0, 0}};

struct MailcapEntry BuiltInsAlternative[] = {
    {"text/plain", CATTEMPLATE, NULL, 0, 1, 0, "plain text", LPRTEMPLATE},
    {NULL, NULL, NULL, 0, 0, 0}};

ProcessMailcapFiles(SquirrelFile) 
char *SquirrelFile;
{
    char *s, *pathcopy = NULL;
#ifdef MICROSOFT
    /* For the Microsoft compiler, we use the library function _searchenv
     * to find the mailcap file.  It will search all the directories
     * listed in the given environment variable (PATH in this case), then
     * construct a path to the file if it finds it.
     */
    /* BOGUS -- this only finds the FIRST mailcap file, we want ALL mailcap files */
    char path[128];

    _searchenv("mailcap", "PATH", path);
    if (path[0])
        if (!ProcessMailcapFile(path, SquirrelFile)) return(0);
    return(-1);
#else /* MICROSOFT */
#ifdef BORLAND
    char *path = getenv("MAILCAPS");
    if (!path)
       path = STDPATH;
#else /* BORLAND */
#ifdef AMIGA
    char *path = STDPATH;
#else /* AMIGA */
    char *path = getenv("MAILCAPS");
    if (!path) {
	char *andrewPath = (char *) AndrewDir("/etc/mailcap");
        int uid = getuid();
        struct passwd *p;
        p = getpwuid(uid);
        if (p) path = malloc(12+strlen(p->pw_dir) + sizeof(STDPATH) + strlen(andrewPath));
        if (!p || !path) ExitWithError(nomem);
	sprintf(path, "%s/.mailcap:%s:%s", p->pw_dir, andrewPath, STDPATH);
	pathcopy = path;
    } else 
#endif /* AMIGA */
#endif /* BORLAND */
    {	
        pathcopy = malloc(1+strlen(path));
        if (!pathcopy) ExitWithError(nomem);
        strcpy(pathcopy, path);
        path = pathcopy;
    }
    while(path) {
        s = index(path, PATH_SEPARATOR);
        if (s) *s++ = 0;
        if (!ProcessMailcapFile(path, SquirrelFile)) return(0);
        path = s;
    }
    if (pathcopy) free(pathcopy);
    return(-1);
#endif /* MICROSOFT */
}

TryBuiltIns(SquirrelFile) 
char *SquirrelFile;
{
    int i;
    /* Last resort -- for sites that didn't bother putting a "text" line in their mailcap files... */
    if (DoDebug) fprintf(stderr, "Looking for '%s' in built-in content-type handling settings.\n", ContentType);
    for (i=0; BuiltIns[i].contenttype; ++i) {
        if (!TryMailcapEntry(SquirrelFile ? BuiltInsAlternative[i] : BuiltIns[i], SquirrelFile))    return(0);
    }
    return(-1);
}

ProcessMailcapFile(file, SquirrelFile)
char *file, *SquirrelFile;
{
    struct MailcapEntry mc;
    FILE *fp = fopen(file, "r");

    if (DoDebug) fprintf(stderr, "Looking for '%s' in mailcap file '%s'.\n", ContentType, file);
    while (fp && !feof(fp)) {
        mc.needtofree = 0;
	if (GetMailcapEntry(fp, &mc)) {
            if (!TryMailcapEntry(mc, SquirrelFile)) {
                fclose(fp);
                return(0);
            }
	}
    }
    if (fp) fclose(fp);
    return(-1);
}

static char *ThingsToSkip[] = {
    "csh ",
    "sh ",
    "ksh ",
    NULL
};

char *ShortCommand(progname)
char *progname;
{
    int i;
    char *s, *oldprogname;
    static char FullProgName[500];

eatmore:
    while (*progname && (*progname == '(' || isspace((unsigned char) *progname))) {
        ++progname;
    }
    oldprogname = progname;
    for (i = 0; oldprogname == progname && ThingsToSkip[i]; ++i) {
        if (!strncmp(progname, ThingsToSkip[i], strlen(ThingsToSkip[i]))) {
            progname += strlen(ThingsToSkip[i]);
        }
    }
    if (*progname == '-') {
        ++progname;
        while (*progname && !isspace((unsigned char) *progname)) ++progname;
    }
    if (progname != oldprogname) goto eatmore;
    strcpy(FullProgName, progname);
    s = index(FullProgName, ' ');
    if (s) *s = 0;
    s = rindex(FullProgName, '/');
    if (s) {
	return(s+1);
    } else {
	return(FullProgName);
    }
}    

TryMailcapEntry(mc, SquirrelFile)
struct MailcapEntry mc;
char *SquirrelFile;
{
    StripTrailingSpace(mc.contenttype);
    if (DoDebug) fprintf(stderr, "Trying mailcap entry for '%s'.\n", mc.contenttype);
    if (PrintingMode && !mc.printcommand) return(-1);
    if (CtypeMatch(ContentType, mc.contenttype) && PassesTest(&mc)) {
        if (SquirrelFile) {
            return(SaveSquirrelFile(SquirrelFile));
        } else {
            char TmpFileName[TMPFILE_NAME_SIZE];
            MkTmpFileName(TmpFileName);
            return(ExecuteMailcapEntry(mc, TmpFileName, ContentType));
        }
    }
    if (mc.needtofree) {
        free(mc.contenttype);
        free(mc.command);
    }
    return(-1);
}

SaveSquirrelFile(SquirrelFile)
char *SquirrelFile;
{
    int j;
    FILE *outfp;
    outfp = fopen(SquirrelFile, WRITE_BINARY);
    if (!outfp) {
        fprintf(stderr, "Cannot open %s to squirrel away a portion of a multipart/alternative\n", SquirrelFile);
        return(-1);
    }
    fprintf(outfp, "Content-type: %s", ContentType);
    for (j=0; j<CParamsUsed; ++j) {
        fprintf(outfp, " ; ");
        fprintf(outfp, CParams[j]);
        fprintf(outfp, " = ");
        fprintf(outfp, CParamValues[j]);
    }
    fprintf(outfp, "\n\n"); 
    TranslateInputToOutput(InputFP, outfp, EncodingCode, ContentType);
    if (fclose(outfp)) {
        ExitWithError("fclose failed");
    }
    return(0);
}

ExecuteMailcapEntry(mc, TmpFileName, ThisContentType)
char *TmpFileName, *ThisContentType;
struct MailcapEntry mc;
{
    int resultcode=0, DidExecute, UsedTmpFileName;
    struct part *PartsWritten=NULL;
    char *s, *cmd;

    cmd = malloc(CMDSIZE);
    if (!cmd) ExitWithError(nomem);
    if (PrintingMode && !mc.printcommand) return(-1);
    BuildCommand(cmd, PrintingMode ? mc.printcommand : mc.command, TmpFileName, &UsedTmpFileName, &PartsWritten);
    if (DoDebug) fprintf(stderr, "Match!  Built command %s.\n", cmd);
    if (mc.copiousoutput && MaybePageOutput) {
        strcat(cmd, " | ");
        s = getenv("METAMAIL_PAGER");
        if (s && strncmp(s, "metamail", 8)) {
            /* If METAMAIL_PAGER is set to "metamail" we override it */
            strcat(cmd, s);
#ifndef AMIGA
            if (!strncmp(s, "less", 4) && strncmp(s+5, "-r", 2)) {
                fprintf(stderr, "Warning:  'less' without '-r' behaves badly for some mail types,\n\tnotably richtext.\n");
            }
#endif
        } else {
            strcat(cmd, "more");
        }
    }
    if (!DefinitelyNotTty) {
        SaveTtyState();
    }
    if (!NeedToAskBeforeExecuting(ThisContentType)
         || OKToRun(ThisContentType, cmd, mc.label)) {
        char *FullCmd;
        int ReallyNotTty;
#ifndef NO_RLIMITS
        /* Limit size of core dumps */
        struct rlimit rlp;

        rlp.rlim_cur = 0;
        rlp.rlim_max = 0;
        setrlimit(RLIMIT_CORE, &rlp); 
#endif
        FullCmd = malloc(CMDSIZE);
        if (!FullCmd) ExitWithError(nomem);
        ReallyNotTty = DefinitelyNotTty;
        if (mc.needsterminal
             && DefinitelyNotTty) {
            int j;
            sprintf(cmd, " %s -P -b -c '%s", METAMAIL, ThisContentType);
            for (j=0; j<CParamsUsed; ++j) {
                strcat(cmd, " ; ");
                strcatquoting(cmd, CParams[j]);
                strcat(cmd, " = ");
                strcatquoting(cmd, CParamValues[j]);
            }
            strcat(cmd, "' ");
            strcat(cmd, TmpFileName);
            CreateNewWindowPrefix(FullCmd);
            strcat(FullCmd, cmd);
            DefinitelyNotTty = 0; /* For recursive call */
        } else {
            strcpy(FullCmd, cmd);
        }
        DidExecute = 0;
        if (UsedTmpFileName || PartsWritten) {
            int isempty;
            if (PartsWritten) {
                isempty = 0;
            } else {
                isempty = WriteTmpFile(TmpFileName, ThisContentType);
            }
            if (!isempty || strncmp(ThisContentType, "text", 4)) {
                if (DoInBackground && !PartsWritten && !mc.needsterminal) {
#ifdef AMIGA
                    char TmpCmd[80], TmpScriptName[40];
                    sprintf(TmpCmd, "run execRmScript %s",
                            MkRmScript(FullCmd, TmpFileName, TmpScriptName));
#else
                    char TmpCmd[CMDSIZE];
                    sprintf(TmpCmd, "(%s; rm %s) &", FullCmd, TmpFileName);
#endif
                    DefinitelyNotTty = 1; /* in background */
                    SetUpEnvironment();
                    resultcode = ExecuteCommand(TmpCmd, 1);
                    ++DidExecute;
                } else {
                    SetUpEnvironment();
                    resultcode = ExecuteCommand(FullCmd, 1);
                    unlink(TmpFileName);
                    ++DidExecute;
                }
            } else { /* empty text part, hack to not say "more" */
                unlink(TmpFileName);
            }
        } else {
            FILE *tmpfp;
            SetUpEnvironment();
            (void) ExecuteCommand(FullCmd, 0);
            tmpfp = popen(FullCmd, WRITE_BINARY);
            TranslateInputToOutput(InputFP, tmpfp, EncodingCode, ThisContentType);
            resultcode = tmpfp ? pclose(tmpfp) : -1;
            ++DidExecute;
        }
        DefinitelyNotTty = ReallyNotTty;
        if (!DefinitelyNotTty && DidExecute) {
            RestoreTtyState();
            if (mc.copiousoutput && MaybePageOutput && BoundaryCt > 0) PauseForUser();
        }
        if (!resultcode) {
            free(FullCmd);
        } else {
            fprintf(stderr, "Command failed: %s\n", FullCmd);
            if (MaybePageOutput && BoundaryCt > 0) PauseForUser();
            ++ProcessingErrors;
        }
    } else {
        /* user does not want to execute command */
        if (!DefinitelyNotTty) {
            RestoreTtyState();
        }
        if (DoDebug) fprintf(stderr, "Not executing command.\n");
        if (!PartsWritten) ConsumeRestOfPart(NULL);
    }
    if (PartsWritten) {
        struct part *tp;
        char HeadFile[MAX_FILE_NAME_SIZE];

        while (PartsWritten) {
            tp=PartsWritten->next;
            free(PartsWritten->ctype);
            strcpy(HeadFile, PartsWritten->fname);
            strcat(HeadFile, "H");
            unlink(HeadFile);
            unlink(PartsWritten->fname);
            free(PartsWritten->fname);
            free(PartsWritten);
            PartsWritten=tp;
        }
    }
    if (!DefinitelyNotTty) {
        RestoreTtyState();
    }
    free(cmd);
    return(0);
}

PassesTest(mc)
struct MailcapEntry *mc;
{
    int result;
    char *cmd, TmpFileName[TMPFILE_NAME_SIZE];

    if (!mc->testcommand) return(1);
    MkTmpFileName(TmpFileName);
    cmd = malloc(CMDSIZE);
    if (!cmd) ExitWithError(nomem);
    BuildCommand(cmd, mc->testcommand, TmpFileName, NULL, NULL);
    if (DoDebug) printf("Executing test command: %s\n", cmd);
    result = system(cmd);
#ifdef MSDOS
    result = 0;	    /* system doesn't return a valid exit code on MS-DOS */
                    /* note that this makes the test feature useless... */
#endif
    free(cmd);
    return(!result);
}

char *
GetCommand(s, t)
char *s, **t;
{
    char *s2;
    int quoted = 0;
    s2 = malloc(strlen(s)*2); /* absolute max, if all % signs */
    if (!s2) ExitWithError(nomem);
    *t = s2;
    while (s && *s) {
	if (quoted) {
            if (*s == '%') *s2++ = '%'; /* Quote through next level, ugh! */

            *s2++ = *s++;
	    quoted = 0;
	} else {
	    if (*s == ';') {
                *s2 = 0;
		return(++s);
	    }
	    if (*s == '\\') {
		quoted = 1;
		++s;
	    } else {
		*s2++ = *s++;
	    }
	}
    }
    *s2 = 0;
    return(NULL);
}	

GetMailcapEntry(fp, mc)
FILE *fp;
struct MailcapEntry *mc;
{
    int rawentryalloc = 2000, len;
    char *rawentry, *s, *t, *LineBuf;

    LineBuf = malloc(LINE_BUF_SIZE);
    if (!LineBuf) ExitWithError(nomem);
    rawentry = malloc(1 + rawentryalloc);
    if (!rawentry) ExitWithError(nomem);
    *rawentry = 0;
    while (fgets(LineBuf, LINE_BUF_SIZE, fp)) {
	if (LineBuf[0] == '#') continue;
	len = strlen(LineBuf);
        if (LineBuf[len-1] == '\n') LineBuf[--len] = 0;
	if ((len + strlen(rawentry)) > rawentryalloc) {
	    rawentryalloc += 2000;
	    rawentry = realloc(rawentry, rawentryalloc+1);
	    if (!rawentry) ExitWithError(nomem);
	}
	if (LineBuf[len-1] == '\\') {
            LineBuf[len-1] = 0;
	    strcat(rawentry, LineBuf);
	} else {
	    strcat(rawentry, LineBuf);
	    break;
	}
    }
    free(LineBuf);
    for (s=rawentry; *s && isspace((unsigned char) *s); ++s) ;
    if (!*s) {
	/* totally blank entry -- quietly ignore */
	free(rawentry);
	return(0);
    }
    s = index(rawentry, ';');
    if (!s) {
	fprintf(stderr, "metamail: Ignoring invalid mailcap entry: %s\n", rawentry);
	free(rawentry);
	return(0);
    }
    *s++ = 0;
    mc->needsterminal = 0;
    mc->copiousoutput = 0;
    mc->needtofree = 1;
    mc->testcommand = NULL;
    mc->label = NULL;
    mc->printcommand = NULL;
    mc->contenttype = malloc(1+strlen(rawentry));
    if (!mc->contenttype) ExitWithError(nomem);
    strcpy(mc->contenttype, rawentry);
    t = GetCommand(s, &mc->command);
    if (!t) {
        free(rawentry);
        return(1);
    }
    while (s && *s && isspace((unsigned char) *s)) ++s;
    s = t;
    while (s) {
	char *arg, *eq;

        t = GetCommand(s, &arg);
/*        if (t) *t++ = 0; */
        eq = index(arg, '=');
        if (eq) *eq++ = 0;
        arg = Cleanse(arg);
	if (!strcmp(arg, "needsterminal")) {
	    mc->needsterminal = 1;
	} else if (!strcmp(arg, "copiousoutput")) {
	    mc->copiousoutput = 1;
        } else if (eq && !strcmp(arg, "test")) {
            mc->testcommand = eq;
        } else if (eq && !strcmp(arg, "description")) {
            mc->label = eq;
        } else if (eq && !strcmp(arg, "label")) { 
            mc->label = eq; /* bogus old name for description */
        } else if (eq && !strcmp(arg, "print")) {
            mc->printcommand = eq;
        } else if (eq && !strcmp(arg, "textualnewlines")) {
            ExceptionalNewline(mc->contenttype, atoi(eq));
	} else if (strcmp(arg, "notes")) { /* IGNORE notes field */
	    if (*arg && DoDebug) fprintf(stderr, "metamail: Ignoring mailcap flag: %s\n", arg);
	}
	s = t;
    }
    free(rawentry);
    return(1);
}

ExitWithError(txt)
char *txt;
{
    if (txt) fprintf(stderr, "metamail: %s\n", txt);
    exit(-1);
}

char *
FreshHeaderCopy(s)
char *s;
{
    char *t, *newcopy;
    int len;

    while (s && *s && isspace((unsigned char) *s) && *s != '\n') ++s;
    t = index(s, '\n');
    while (t && (*(t+1) == ' ' || *(t+1) == '\t')) {
        t = index(t+1, '\n');
    }
    len = t ? (t-s+1) : (strlen(s)+1);
    newcopy = malloc(len+3);/* two extra bytes for a bizarre bug caused by the fact that FindParam calls FreshHeaderCopy and sometimes tacks on "--". */
    if (!newcopy) ExitWithError(nomem);
    strncpy(newcopy, s, len);
    newcopy[len] = 0;
    return(newcopy);
}

Read822Prefix(PrintHeads, nestingdepth)
int PrintHeads, nestingdepth;
{
    int SawNewline = 1, bytes = 0, alloced = 1000, HasEncodedChars=0;
    int c, oldbytes;
    char *s, *t, *tmp;

    if (!PrintSomeHeaders) PrintHeads = 0;
    mailheaders = malloc(alloced+1);
    if (!mailheaders) ExitWithError(nomem);
    strcpy(mailheaders, "MM_HEADERS=\n");
    bytes = 12;
yankagain:
    t = mailheaders + bytes;
    oldbytes = bytes-1; /* a hack for YankMode */
    while ((c = getc(InputFP)) != EOF) {
        if (++bytes >= alloced) {
            alloced += 1000;
            mailheaders = realloc(mailheaders, alloced);
            if (!mailheaders) ExitWithError(nomem);
            t = mailheaders + bytes - 1;
        }
        if (c == '\n') {
            if (SawNewline) break;
            SawNewline = 1;
        } else SawNewline = 0;
        *t++ = c;
    }
    *t = 0;
    --bytes;
    if (c == EOF) {
        if (nestingdepth) {
            exit(ProcessingErrors);
        } else {
            if (YankMode) {
                ExitWithError("Could not extract a MIME message from the body\n");
            } else {
                ExitWithError("Could not find end of mail headers");
            }
        }
    }
    for (s=mailheaders+oldbytes; *s; ++s) {
        if (*s == '\n' && (*(s+1) != ' ') && (*(s+1) != '\t')) {
            if (!ContentType && !lc2strncmp(s, "\ncontent-type:", 14)) {
                ContentType = FreshHeaderCopy(s+14);
                StripTrailingSpace(ContentType);
                ParseContentParameters(ContentType);
                if (PrintHeads) maybephead(s+1);
            } else if (!ContentEncoding && !lc2strncmp(s, "\ncontent-transfer-encoding:", 27)) {
                ContentEncoding = FreshHeaderCopy(s+27);
                if (PrintHeads) maybephead(s+1);
            } else if (!lc2strncmp(s, "\nsubject:", 9)) {
                if (PrintHeads) maybephead(s+1);
                MailSubject = FreshHeaderCopy(s+9);
            } else if (!lc2strncmp(s, "\nfrom:", 6)) {
                if (PrintHeads) maybephead(s+1);
                MailFrom = FreshHeaderCopy(s+6);
            } else if (!lc2strncmp(s, "\ncontent-description:", 4)) {
                if(PrintHeads) maybephead(s+1);
                MailSubject = FreshHeaderCopy(s+21);
            } else {
                /* Print any with encoded variables */
                char *dum = s;
                while (dum) {
                    dum = index(dum, '?');
                    if (dum && *++dum == '=') break;
                }
                if (dum) {
                    char *nl = s+1;
                    while (nl) {
                        nl = index(nl, '\n');
                        if (nl && !isspace((unsigned char) *++nl)) break;
                    }
                    if (nl && nl > dum) ++HasEncodedChars;
                }
                if (HasEncodedChars) {
                    phead(s+1);
                } else if (PrintHeads) {
                    maybephead(s+1);
                }
            }
        }
    }
    /* Ugly, but effective */
    if (YankMode && !ContentType) {
        goto yankagain;
    }
    if (PrintHeads) printf("\n");
    if (!ContentType) {
        ContentType = "text/plain";
        CParamsUsed=0;
    }
    for (tmp=ContentType; *tmp; ++tmp) {
        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
    }
}

PrepareMessage() {
    int c;

    EncodingCode = ENCODING_NONE;
    if (ContentEncoding) {
        /* strip leading white space */
        while (*ContentEncoding && isspace((unsigned char) *ContentEncoding)) ++ContentEncoding;
        StripTrailingSpace(ContentEncoding);
        if (!lc2strcmp(ContentEncoding, "base64")) {
            EncodingCode = ENCODING_BASE64;
        } else if (!lc2strcmp(ContentEncoding, "quoted-printable")) {
            EncodingCode = ENCODING_QUOTEDPRINTABLE;
        } else if (!lc2strncmp (ContentEncoding, "x-uue", 5)) {
            fprintf (stderr, "WARNING:  Using nonstandard %s encoding, trying uuencode algorithm.\n", ContentEncoding);
 	    EncodingCode = ENCODING_UUENCODE;
        } else {
            if (lc2strcmp(ContentEncoding, "none")
                 && !lc2strcmp(ContentEncoding, "8bit")
                 && !lc2strcmp(ContentEncoding, "7bit")) {
                fprintf(stderr, "Ignoring unrecognized Content-Transfer-Encoding value: %s\n", ContentEncoding);
            }
        }
    }
    if (EatLeadingNewlines) {
        while ((c = getc(InputFP)) != EOF) {
            if (c != '\n') {
                ungetc(c, InputFP);
                break;
            }
        }
    }
    SetUpEnvironment();  
}

SetUpEnvironment() { 
    int i, j, environsize;
    char **newenviron, *mailervar, *summaryvar, *ctypevar, *s;
    static char ttyenv[15], debugenv[15], *noaskenv, pagerenv[15], *quietenv, rootenv[25];

#if !defined(AMIGA) && !defined(MSDOS)
    /* Hack to make the code look similar for unix & dos */
#define putenv(var)        newenviron[i++] = var;
    for (environsize=0; environ[environsize]; ++environsize) {
	;
    }
    newenviron = (char **) malloc(sizeof(char *) * (17+environsize));
    if (!newenviron) ExitWithError(nomem);
#endif
    mailervar = malloc(13+strlen(MailerName));
    if (!mailervar) ExitWithError(nomem);
    sprintf(mailervar, "MM_MAILER=%s", MailerName);
    summaryvar = malloc(26 + strlen(MailFrom) + strlen(MailSubject));
    if (!summaryvar) ExitWithError(nomem);
    sprintf(summaryvar, "MM_SUMMARY=%s (from %s)", MailSubject, MailFrom);
    MailSummary = summaryvar+11;
    EliminateNastyChars(MailSummary);
    i = 0;
    if (ContentType) {
        int ctypelen = 22+strlen(ContentType);
        for (j=0; j<CParamsUsed; ++j) {
            ctypelen += 6 + strlen(CParams[j]) + strlen(CParamValues[j]);
        }
        ctypevar = malloc(ctypelen);
        if (!ctypevar) ExitWithError(nomem);
        for (s=ContentType; *s; ++s) {
            if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
        }
        while (isspace((unsigned char) *--s)) *s = 0;
        sprintf(ctypevar, "MM_CONTENTTYPE=%s", ContentType);
        for (j=0; j<CParamsUsed; ++j) {
            strcat(ctypevar, " ; ");
            strcat(ctypevar, CParams[j]);
            strcat(ctypevar, " = ");
            strcat(ctypevar, CParamValues[j]);
        }
        putenv(ctypevar);
    }
    putenv(mailheaders ? mailheaders : "MM_HEADERS=unknown");
    putenv(mailervar);
    putenv(summaryvar);
    sprintf(ttyenv, "MM_NOTTTY=%d", DefinitelyNotTty);
    putenv(ttyenv);
    sprintf(debugenv, "MM_DEBUG=%d", DoDebug);
    putenv(debugenv);
    s = getenv("MM_QUIET");
    if (!s) s = QuietDefault;
    quietenv = malloc(15 + strlen(s));
    if (!quietenv) ExitWithError(nomem);
    if (Quiet) {
        strcpy(quietenv, "MM_QUIET=1");
    } else {
        sprintf(quietenv, "MM_QUIET=%s", s);
    }
    putenv(quietenv);
    s = getenv("MM_NOASK");
    if (!s) s = NoAskDefault;
    noaskenv = malloc(15 + strlen(s));
    if (!noaskenv) ExitWithError(nomem);
    if (MightAskBeforeExecuting) {
        sprintf(noaskenv, "MM_NOASK=%s", s);
    } else {
        strcpy(noaskenv, "MM_NOASK=1");
    }
    putenv(noaskenv);
    s = getenv("MM_RUNASROOT");
    if (!s) s = "0";
    sprintf(rootenv, "MM_RUNASROOT=%s", s);
    putenv(rootenv);
    sprintf(pagerenv, "MM_USEPAGER=%d", MaybePageOutput);
    putenv(pagerenv);
#if !defined(AMIGA) && !defined(MSDOS)
    for (j=0; j<environsize; ++j) {
        if (strncmp(environ[j], "MM_", 3) || !strncmp(environ[j], "MM_CHARSET", 10)) {
            putenv(environ[j]);
        }
    }
    newenviron[i] = NULL;
    environ = newenviron;
    if (DoDebug) {
        printf("Here is the environment:\n\n");
        system("printenv");
    }
#endif
}


#ifdef AMIGA
int
putenv(def)
char *def;
{
    char *cp;
    char nameBuf[100];
    FILE *envFile;

    if ((cp = index(def, '=')) == NULL || def == cp) {
        return(1);
    }

    strcpy(nameBuf, "ENV:");
    strncat(nameBuf, def, cp - def);
    nameBuf[(cp - def) + 4] = 0;
    cp++;               /* Now points to value part of environment string. */

    if ((envFile = fopen(nameBuf, "w")) == NULL) {
        return(2);
    }

    fputs(cp, envFile);
    fclose(envFile);

    return(0);
}
#endif

OKToRun(ctype, progname, label)
char *ctype, *progname, *label;
{
    char AnsBuf[100], *s;

    if (DoInBackground) return(1);
    while (1) {
        printf("\n");
        if (label) {
            printf("This message contains %s.\nDo you want to view it using the '%s' command (y/n) [y] ? ", label, ShortCommand(progname));
        } else {
            printf("This message contains '%s'-format data.\nDo you want to view it using the '%s' command (y/n) [y] ? ", ctype, ShortCommand(progname));
        }
        s = gets(AnsBuf);
        if (!s) return(0); /* EOF */
	while (s && *s && isspace((unsigned char) *s)) ++s;
	if (*s == 'y' || *s == 'Y' || !*s || *s == '\n') return(1);
	if (*s == 'n' || *s == 'N' || *s == 'q' || *s == 'Q') {
	    return(0);
	}
	printf("Please answer yes or no.\n");
    }
}

EliminateNastyChars(s)
char *s;
{
    if (s) for( ; *s ;++s) {
        if (isalnum((unsigned char) *s)) continue;
	if (index(" ,.;:/?\\|[]{}()*&^%#@-_=+~<>\"", *s)) continue;
	if (*s == '\'' || *s == '`') {
	    *s = '"';
	} else {
	    *s = ' ';
	}
    }
}

StripTrailingSpace(s)
char *s;
{
    char *t = s+strlen(s) -1;
    while (isspace((unsigned char) *t) && (t >= s)) *t-- = 0;
}

static char *KeyHeads=NULL;
static char **KeyHeadList;
static int KeyKeep = 0;

void setKeyHeads()
{
    char *s;
    int numkeys = 0;

    if ((KeyHeads = getenv("KEYHEADS")) != 0) {
        for (s=KeyHeads;*s;++s) if (isupper((unsigned char) *s)) *s=tolower((unsigned char) *s);
    } else if ((KeyHeads = getenv("KEYIGNHEADS")) != 0) {
	for (s=KeyHeads;*s;++s) if (isupper((unsigned char) *s)) *s=tolower((unsigned char) *s);
	KeyKeep = 1;
    } else {
	static char khtmp[] = "to:cc:subject:from:content-description:date";
	KeyHeads = khtmp;
    }
    for (s=KeyHeads; *s; ++s) if (*s == ':') ++numkeys;
    numkeys += 2;
    KeyHeadList = (char **) malloc((numkeys) * sizeof(char *));
    if (!KeyHeadList) ExitWithError(nomem);
    numkeys = 0;
    KeyHeadList[0] = KeyHeads;
    for(s=KeyHeads; *s; ++s) {
	if (*s == ':') {
	    *s = '\0';
	    KeyHeadList[++numkeys] = s+1;
	}
    }
    KeyHeadList[++numkeys] = NULL;
}

/* find the first colon in a header line which appears before any spaces or control characters */
char *
findcolon(hdr)
char *hdr;
{
    while (*hdr && !isspace(*hdr) && !iscntrl(*hdr))
	if (*hdr == ':') return hdr;
	else hdr++;
    return 0;
}

/* check the header given to see if it matches any in the KeyHeadList */
maybephead(hdr)
char *hdr;
{
    char *s;
    int numkeys=0;

    if (!KeyHeads) setKeyHeads();

    s = findcolon(hdr);
    if (s) {
	int len = s - hdr;
        for (numkeys=0; KeyHeadList[numkeys]; ++numkeys) {
            if (!strcmp(KeyHeadList[numkeys], "*")
                 || !lc2strncmp(hdr, KeyHeadList[numkeys], len)) {
		if (!KeyKeep) phead(hdr);
		return;
            }
        }
	if (KeyKeep) phead(hdr);
	return;
    }
    if (!strncmp(hdr, "From ", 5) || !strncmp(hdr, ">From ", 6)) {
	for (numkeys = 0; KeyHeadList[numkeys]; ++numkeys) {
	    if (!strcmp(KeyHeadList[numkeys], "*")
		 || !lc2strncmp(">from", KeyHeadList[numkeys], 5)) {
		if (!KeyKeep) phead(hdr);
		return;
	    }
	}
	if (KeyKeep) phead(hdr);
    }
}

/* This next routine prints out a mail header, and needs to deal with the new extended charset headers. */
phead(s)
char *s;
{
    char *t = s;

    while (1) {
	t = index(t, '\n');
	if (!t) break;
        if (!isspace((unsigned char) *(t+1))) {
            *t = 0;
	    break;
	} else ++t;
    }
    PrintHeader(s, 1);
    printf("\n");
    if (t) *t = '\n';
}

static char PrevCharset[100] = "us-ascii";

/* This is the part that actually handles the charset issues */
void PrintHeader(s, ShowLeadingWhitespace)
char *s;
int ShowLeadingWhitespace;
{
    char *charset, *encoding, *txt, *txtend, TmpFile[TMPFILE_NAME_SIZE];
    int ecode = ENCODING_NONE, CorrectedCharset = 0;
    FILE *fp;

    while (*s && (*s != '=')) {
        if (isspace((unsigned char) *s)) {
            if (ShowLeadingWhitespace) {
                putchar(' ');
            }
        } else {
            putchar(*s);
            if (!CorrectedCharset) {
                CorrectedCharset = 1;
                strcpy(PrevCharset, "us-ascii");
            }
        }
        if (!ShowLeadingWhitespace) {
            /* Only at most one leading space is ignored */
            ShowLeadingWhitespace = 1;
        }
        ++s;
    }
    if (!*s) return;
    if (*(s+1) != '?') {
        putchar('=');
        PrintHeader(++s, 1);
        return;
    }
    charset = s+2;
    encoding = index(charset, '?');
    if (!encoding) {
        putchar('=');
        PrintHeader(++s,1);
        return;
    }
    txt = index(encoding+1, '?');
    if (!txt) {
        putchar('=');
        PrintHeader(++s, 1);
        return;
    }
    txtend = txt;
    do {
        txtend = index(txtend+1, '?');
    } while(txtend && (*(txtend+1) != '='));
    if (!txtend) {
        putchar('=');
        PrintHeader(++s, 1);
    }
    /* Proper parse! Ready to dissect... */
    *encoding = 0;
    *txt = 0;
    *txtend = 0;
    if ((*(encoding+1) == 'q') || (*(encoding+1) == 'Q')) {
        ecode = ENCODING_QUOTEDPRINTABLE;
    } else if ((*(encoding+1) == 'b') || (*(encoding+1) == 'B')) {
        ecode = ENCODING_BASE64;
    } else {
        fprintf(stderr, "Bad encoding value in non-ASCII header string: %s\n", encoding+1);
    }
    if (lc2strcmp(charset, PrevCharset)) {
        char *s2, *charsetinuse;

        strcpy(PrevCharset, charset);
        for (s2=PrevCharset; *s2; ++s2) {
            if (isupper((unsigned char) *s2)) *s2 = tolower((unsigned char) *s2);
        }
        charsetinuse = getenv("MM_CHARSET");
        if (!charsetinuse || lc2strcmp(charsetinuse, PrevCharset)) {
            printf("[** %s charset **] ", charset);
        }
    }
    if (ecode == ENCODING_NONE) {
        printf(txt+1);
    } else {
        /* What follows is REALLY bogus, but all my encoding stuff is pipe-oriented right now... */
        MkTmpFileName(TmpFile);
        fp = fopen(TmpFile, WRITE_BINARY);
        if (!fp) {
            fprintf(stderr, "Could not open temporary file\n");
        } else {
            char *t;
            for (t=txt+1; *t; ++t) {
                if (*t == '_') {
                    putc(' ', fp);
                } else if (*t == '\n') {
                    putc(' ', fp);
                } else {
                    putc(*t, fp);
                }
            }
            fclose(fp);
            fp = fopen(TmpFile, "r");
            if (!fp) {
                fprintf(stderr, "Could not open temporary file\n");
            } else {
                TranslateInputToOutput(fp, stdout, ecode, "text");
                fclose(fp);
            }
            unlink(TmpFile);
        }
    }
    *encoding = '?';
    *txt = '?';
    *txtend = '?';
    PrintHeader(txtend + 2, 0);
}

BuildCommand(Buf, controlstring, TmpFileName, UsedTmpFileName, PartsWritten)
char *Buf, *controlstring, *TmpFileName;
int *UsedTmpFileName;
struct part **PartsWritten;
{
    char *from, *to, *s, *p, *tmp;
    int prefixed = 0, UsedBigFile=0, UsedLittleFiles=0, numparts=0;
    struct part *firstpart=NULL, *thispart=NULL, *tmppart=NULL;

    if (UsedTmpFileName) *UsedTmpFileName = 0;
    if (PartsWritten) *PartsWritten = NULL;
    for (from=controlstring, to=Buf; *from; ++from) {
        if (prefixed) {
            prefixed = 0;
            switch(*from) {
                case '%':
                    *to++ = '%';
                    break;
                case 'n':
                case 'F':
                    if (UsedBigFile) {
                        fprintf(stderr, "metamail: Bad mailcap entry: %s\n", controlstring);
                        ExitWithError("%%n and %%F are incompatible with %%s.  Execution terminated.");
                    }
                    if (!UsedLittleFiles) {
                        /* Set up parts */
                        char *LineBuf, *boundary, TmpFileNameBuf[TMPFILE_NAME_SIZE];
                        char *oldct, *oldce, *newct;
                        int currct, boundarylen, newctlen, j;
                        int oldcparmsused, oldcparamsalloced;
                        char **oldcparams, **oldcparamvalues;
                        FILE *headfp;

                        LineBuf = malloc(LINE_BUF_SIZE);
                        if (!LineBuf) ExitWithError(nomem);
                        boundary = FindParam("boundary");
                        if (!boundary) {
                            ExitWithError("Bad message format -- multipart message has no boundary parameter!");
                        }
                        if (boundary[0] == '"') {
                            boundary=UnquoteString(boundary);
                        }
                        sprintf(LineBuf, "--%s", boundary);
                        strcpy(boundary, LineBuf);
                        boundarylen = strlen(boundary);
                        if (BoundaryCt >= BoundaryAlloc) {
                            BoundaryAlloc += 5;
                            if (Boundaries) {
                                Boundaries = (char **) realloc(Boundaries, BoundaryAlloc*sizeof(char *));
                            } else {
                                Boundaries = (char **) malloc(BoundaryAlloc*sizeof(char *));
                            }
                            if (!Boundaries) ExitWithError(nomem);
                        }
                        Boundaries[BoundaryCt++] = boundary;
                        while (fgets(LineBuf, LINE_BUF_SIZE, InputFP)) { /* find start */
                            if (!strncmp(LineBuf, boundary, boundarylen)
                                 && ((LineBuf[boundarylen] == '\n')
                                      || (LineBuf[boundarylen] == '-'
                                           && LineBuf[boundarylen+1] == '-'
                                           && LineBuf[boundarylen+2] == '\n'))) {
                                break;
                            }
                        }
                        free(LineBuf);
                        currct = BoundaryCt;
                        oldct=ContentType;
                        oldce=ContentEncoding;
                        oldcparmsused = CParamsUsed;
                        oldcparamsalloced = CParamsAlloced;
                        oldcparams = CParams;
                        oldcparamvalues = CParamValues;
                        CParams = NULL;
                        CParamValues = NULL;
                        CParamsUsed = 0;
                        CParamsAlloced = 0;
                        while(currct == BoundaryCt) {
                            tmppart = (struct part *) malloc(sizeof(struct part));
                            if (!tmppart) ExitWithError(nomem);
                            if (firstpart) {
                                thispart->next = tmppart;
                                thispart = tmppart;
                            } else {
                                firstpart = thispart = tmppart;
                            }
                            tmppart->next = NULL;
                            ContentType=NULL;
                            ContentEncoding=NULL;
                            Read822Prefix(0,0);
                            PrepareMessage();
                            newctlen=5+strlen(ContentType);
                            for (j=0; j<CParamsUsed; ++j) {
                                newctlen += 11+strlen(CParams[j]) + strlen(CParamValues[j]);
                            }
                            newct= malloc(newctlen);
                            if (!newct) ExitWithError(nomem);
                            strcpy(newct, "'");
                            strcat(newct, ContentType);
                            for (j=0; j<CParamsUsed; ++j) {
                                strcat(newct, "; ");
                                strcat(newct, CParams[j]);
                                if (CParamValues[j][0] == '\"') {
                                    strcat(newct, " = ");
                                    strcat(newct, CParamValues[j]);
                                } else {
                                    strcat(newct, " = \"");
                                    strcat(newct, CParamValues[j]);
                                    strcat(newct, "\"");
                                }
                            }
                            strcat(newct, "'");
                            thispart->ctype = newct;
                            MkTmpFileName(TmpFileNameBuf);
                            thispart->fname = malloc(1+strlen(TmpFileNameBuf));
                            if (!thispart->fname) ExitWithError(nomem);
                            strcpy(thispart->fname, TmpFileNameBuf);
                            WriteTmpFile(thispart->fname, thispart->ctype);
                            strcat(TmpFileNameBuf, "H");
                            headfp = fopen(TmpFileNameBuf, "w");
                            if (headfp) {
                                fputs(mailheaders+12, headfp);
                                /* The +12 gets rid of MM_HEADERS=\n */
                                fclose(headfp);
                            }
                            ++numparts;
                        }
                        ContentType=oldct;
                        ContentEncoding=oldce;
                        CParamsUsed = oldcparmsused;
                        CParamsAlloced = oldcparamsalloced;
                        CParams = oldcparams;
                        CParamValues = oldcparamvalues;
                        /* Now we've seen the last encapsulation boundary, but if there is a "postfix"
                            we must throw it away.*/
                        if (BoundaryCt > 0) {
                            ConsumeRestOfPart(NULL);
                        }
                        *PartsWritten = firstpart;
                        UsedLittleFiles=1;
                    }
                    if (*from == 'n') {
                        char numbuf[10];
                        sprintf(numbuf, "%d", numparts);
                        strcpy(to, numbuf);
                        to += strlen(numbuf);
                    } else { /* %F */
                        for (tmppart = firstpart; tmppart != NULL; tmppart = tmppart->next) {
                            sprintf(to, "%s %s ", tmppart->ctype, tmppart->fname);
                            to += (strlen(tmppart->ctype) + strlen(tmppart->fname) + 2);
                        }
                    }
                    break;
                case 's':
                    if (UsedLittleFiles) {
                        fprintf(stderr, "metamail: Bad mailcap entry: %s\n", controlstring);
                        ExitWithError("%%n and %%F are incompatible with %%s.  Execution terminated.");
                    }
                    if (TmpFileName) {
                        strcpy(to, TmpFileName);
                        to += strlen(TmpFileName);
                        if (UsedTmpFileName) ++(*UsedTmpFileName);
                    }
                    UsedBigFile = 1;
                    break;
                case '{':
                    s = index(from, '}');
                    if (!s) {
                        fprintf(stderr, "Ignoring ill-formed parameter reference in mailcap file: %s\n", from);
                        break;
                    }
                    ++from;
                    *s = 0;
                    /* put in lower case */
                    for (tmp=from; *tmp; ++tmp) {
                        if (isupper((unsigned char) *tmp)) *tmp = tolower((unsigned char) *tmp);
                    }
                    p = FindParam(from);
                    if (p && p[0] == '"') {
                        p=UnquoteString(p);
                    }
                    if (!p) p = "";
                    *to++ = '\'';
                    strcpynoquotes(to, p);
                    to += strlen(p);
                    *to++ = '\'';
                    *s = '}'; /* restore */
                    from = s;
                    break;
                case 't':
                    /* type/subtype */
                    strcpynoquotes(to, ContentType);
                    to += strlen(ContentType);
                    break;
                default:
                    fprintf(stderr, "Ignoring unrecognized format code in mailcap file: %%%c\n", *from);
                    break;
            }
        } else if (*from == '%') {
            prefixed = 1;
        } else {
            *to++ = *from;
        }
    }
    *to = 0;
}

strcpynoquotes(t,f)
char *t, *f;
{
    while (*f) {
        if (*f != '\"' && *f != '\'' && *f != '`') *t++ = *f; else *t++=' ';
        ++f;
    }
}

WriteTmpFile(fname, ctype)
char *fname;
char *ctype;
{
    FILE *fpout;
    int retval = 0;

    fpout = fopen(fname, WRITE_BINARY);
    if (!fpout) {
        perror("WriteTmpFile");
        ExitWithError("Can't create temporary file");
    }
    TranslateInputToOutput(InputFP, fpout, EncodingCode, ctype);
    if (ftell(fpout) == 0) retval = 1;
    if (fclose(fpout)) ExitWithError("Can't write temporary file");
    return(retval);
}


TranslateInputToOutput(InputFP, OutputFP, Ecode, ctype)
FILE *InputFP, *OutputFP;
int Ecode;
char *ctype;
{
    int InMultipart = BoundaryCt > 0 ? 1 : 0;

    switch(Ecode) {
        case ENCODING_BASE64:
            from64(InputFP, OutputFP, InMultipart ? Boundaries : NULL, &BoundaryCt, DoesNeedPortableNewlines(ctype));
            break;
        case ENCODING_QUOTEDPRINTABLE:
            fromqp(InputFP, OutputFP, InMultipart ? Boundaries : NULL, &BoundaryCt);
            break;
 	case ENCODING_UUENCODE:
 	    fromuue(InputFP, OutputFP, InMultipart ? Boundaries: NULL, &BoundaryCt);
 	    break;
        default:
            ConsumeRestOfPart(OutputFP);
    }
#if !defined(AMIGA) && !defined(MSDOS)
    if (UsingStandardInput && feof(InputFP) && !freopen("/dev/tty", "r", stdin)) {
        fprintf(stderr, "Warning: Cannot freopen /dev/tty to stdin");
    } else InputFP = stdin;
#endif
}

CreateNewWindowPrefix(Prefix)
char *Prefix;
{
    char *override = getenv("TERMINAL_CMD");
    if (override) {
        strcpy(Prefix, override);
#ifdef AMIGA
    } else {
        /* The window should *not* run in background. We are thus unable
         * to use NewWsh or NewCLI.
         */
        /* strcpy(Prefix, "newwsh CMD "); */
        Prefix[0] = 0;
#else
    } else if (getenv("DISPLAY")) {
        /* X11 */
        strcpy(Prefix, "xterm -title '");
        strcat(Prefix, MailSummary);
        strcat(Prefix, "' -e ");
    } else if (getenv("WINDOW_PARENT")) {
        /* SunView */
        strcpy(Prefix, "shelltool ");
    } else if (getenv("WMHOST")) {
        /* old Andrew WM */
        strcpy(Prefix, "h19 ");
    } else {
        /* last resort is to look for /dev/tty */
        if (!freopen("/dev/tty", "r", stdin)){
            ExitWithError("Don't know how to create a terminal window");
        }
        InputFP = stdin;
        fprintf(stderr, "Warning, reopened /dev/tty, could be strange.\n");
        Prefix[0] = 0;
#endif
    }
}

int HasSavedTtyState=0;
#if !defined(AMIGA) && !defined(MSDOS)
#ifdef SYSV
static struct termio MyTtyStateIn, MyTtyStateOut;
#else
static struct sgttyb MyTtyStateIn, MyTtyStateOut;
#endif
#endif

SaveTtyState() {
    /* Bogus -- would like a good portable way to reset the terminal state here */
#if !defined(AMIGA) && !defined(MSDOS)
#ifdef SYSV
    ioctl(fileno(stdin), TCGETA, &MyTtyStateIn);
    ioctl(fileno(stdout), TCGETA, &MyTtyStateOut);
#else
    gtty(fileno(stdin), &MyTtyStateIn);
    gtty(fileno(stdout), &MyTtyStateOut);
#endif
    HasSavedTtyState = 1;
#endif
}

RestoreTtyState() {
#if !defined(AMIGA) && !defined(MSDOS)
#ifdef SYSV
    if (HasSavedTtyState) {
        ioctl(fileno(stdout), TCSETA, &MyTtyStateOut);
        ioctl(fileno(stdin), TCSETA, &MyTtyStateIn);
    }
#else
    if (HasSavedTtyState) {
        stty(fileno(stdout), &MyTtyStateOut);
        stty(fileno(stdin), &MyTtyStateIn);
    }
#endif
#endif
}

NeedToAskBeforeExecuting(type)
char *type;
{
    struct NoAskItem *nai;
    if (!MightAskBeforeExecuting || DoInBackground) return(0);
    for (nai = FirstNoAskItem; nai; nai = nai->next) {
        if (CtypeMatch(type, nai->type)) return(0);
    }
    return(1);
}

NeedToBeQuiet(cmd)
char *cmd;
{
    struct NoAskItem *nai;
    for (nai = FirstQuietItem; nai; nai = nai->next) {
        if (!lc2strcmp(nai->type, cmd)) return(1);
    }
    return(0);
}

CtypeMatch(ctype, pat)
char *ctype, *pat;
{
    int len;
    char pat2[200];

    if (!lc2strcmp(ctype, pat)) {
        return(1); /* exact match, case-insensitive */
    }
    if (index(pat, '/') == NULL) {
        /* implicit wildcard */
        strcpy(pat2, pat);
        strcat(pat2, "/*");
        pat = pat2;
    }
    len = strlen(pat);
    if ((pat[--len] == '*')
         && (pat[--len] == '/')
         && (!lc2strncmp(ctype, pat, len))
         && ((ctype[len] == '/') || (ctype[len] == '\0'))){
        /* wildcard match */
        return(1);
    }
    return(0);
}

ExecuteCommand(cmd, really)
char *cmd;
int really;
{
    int code;
    if (!Quiet || DoDebug) {
        if (!NeedToBeQuiet(ShortCommand(cmd))) {
            printf("---Executing: %s\n", DoDebug ? cmd : ShortCommand(cmd));
        } else if (EatLeadingNewlines) {
            printf("\n");
        } 
        fflush(stdout);
    }
    if (really) {
        fflush(stdout); fflush(stderr);
        code = system(cmd);
        if (DoDebug) printf("Command exit status: %d\n", code);
#ifdef MSDOS
	code = 0;	/* system doesn't return a valid exit code on MS-DOS */
#endif
        return(code);
    }
    return(0);
}

MkTmpFileName(name)
char *name;
{
#ifdef AMIGA
    strcpy(name, "T:mmXXXXXX");
    mktemp(name);
#else
#ifndef MSDOS
    sprintf(name, "%s/mm.XXXXXX", tmproot);
    mktemp(name);
#else
     strcpy(name, "TXXXXXX");
     if (!mktemp(name))
         name[0] = 0;
     else
         if (DoDebug) printf("temp name = \"%s\"\n", name);
#endif
#endif
}

#ifdef AMIGA
/* We need to execute a command and then remove a file "fileToRemove".
 * MkRmScript() creates a shell script that accomplishes this. The script
 * is written to a temporary file. The name of the script is returned.
 */
char *
MkRmScript(command, fileToRemove, nameBuf)
char *command;
char *fileToRemove;
char *nameBuf;
{
    FILE *script;

    MkTmpFileName(nameBuf);
    if ((script = fopen(nameBuf, "w")) == NULL) {
        fprintf(stderr, "Unable to open %s for writing\n", nameBuf);
        exit(1);
    }
    fprintf(script, ".BRA {\n.KET }\n%s\nDelete %s QUIET\n", command, fileToRemove);
    fclose(script);
    return(nameBuf);
}
#endif

void
ConsumeRestOfPart(outfp)
FILE *outfp;
{
    char *Buf;
    int c;

    if (BoundaryCt <= 0) {
        while ((c=getc(InputFP)) != EOF) {
            if (outfp) putc(c, outfp);
        }
        return;
    }
    Buf = malloc(LINE_BUF_SIZE);
    if (!Buf) ExitWithError(nomem);
    while (fgets(Buf, LINE_BUF_SIZE, InputFP)) {
        if ((BoundaryCt > 0)
             && (Buf[0] == '-')
             && (Buf[1] == '-')
             && PendingBoundary(Buf, Boundaries, &BoundaryCt)) {
            break;
        }
        if (outfp) fputs(Buf, outfp);
    }
    free(Buf);
}

char *paramend(s)
char *s;
{
    int inquotes=0;
    while (*s) {
        if (inquotes) {
            if (*s == '"') {
                inquotes = 0;
            } else if (*s == '\\') {
                ++s; /* skip a char */
            }
        } else if (*s == ';') {
            return(s);
        } else if (*s == '"') {
            inquotes = 1;
        }
        ++s;
    }
    return(NULL);
}        

void
ParseContentParameters(ct)
char *ct;
{
    char *s, *t, *eq;

    CParamsUsed = 0;
    s = index(ct, ';');
    if (!s) return;
    *s++ = 0;
    do {
        t = paramend(s);
        if (t) *t++ = 0;
        eq = index(s, '=');
        if (!eq) {
            fprintf(stderr, "Ignoring unparsable content-type parameter: '%s'\n", s);
            JunkParameter=Cleanse(s);
        } else {
            if (CParamsUsed >= CParamsAlloced) {
                CParamsAlloced += 10;
                if (CParams) {
                    CParams = (char **) realloc(CParams, (1+CParamsAlloced) * sizeof (char *));
                    CParamValues = (char **) realloc(CParamValues, (1+CParamsAlloced) * sizeof (char *));
                } else {
                    CParams = (char **) malloc((1+CParamsAlloced) * sizeof (char *));
                    CParamValues = (char **) malloc((1+CParamsAlloced) * sizeof (char *));
                }
                if (!CParams || !CParamValues) ExitWithError(nomem);
            }
            *eq++ = 0;
            s = Cleanse(s);
            CParams[CParamsUsed] = s;
            /* strip leading white space */
            while (*eq && isspace((unsigned char) *eq)) ++eq;
            /* strip trailing white space */
            StripTrailingSpace(eq);
            CParamValues[CParamsUsed++] = eq; 
            if (DoDebug) printf("NEW PARAMETER: %s VALUE: %s\n", s, eq);
        }
        s = t;
    } while (t);
}

char *FindParam(s)
char *s;
{
    int i;
    for (i=0; i<CParamsUsed; ++i) {
        if (!strcmp(s, CParams[i])) {
            return(CParamValues[i]);
        }
    }
    return(NULL);
}

#ifdef MSDOS
system2(s)
char *s;
{
    printf("system2: \"%s\"\n", s);
    return(0);
}
#endif

strcatquoting(s1, s2)
char *s1;
char *s2;
{
    strcat(s1, s2);
#ifdef NOTDEF
    while (*s1) ++s1;
    while (*s2) {
        if (*s2 == '\"' || *s2 == '\\') *s1++ = '\\';
        *s1++ = *s2++;
    }
    *s1 = '\0';
#endif
}

PauseForUser() {
#if defined(MSDOS) || defined(AMIGA)
    char Buf[100];
    printf("Press RETURN to go on\n");
    gets(Buf);
#else
    fflush(stdout);
    SaveTtyState();
    if (StartRawStdin() != -1) {
        printf("--Press any key to go on.--");
    } else {
        printf("Press RETURN to go on.\n");
    }
    fflush(stdout);
    getchar();
    RestoreTtyState();
    printf("\n");
#endif
}

StartRawStdin() {
#if !defined(AMIGA) && !defined(MSDOS)
#ifdef SYSV
    struct termio   orterm, fterm;
    ioctl(0, TCGETA, &orterm);	/* get current (i.e. cooked) termio */
    fterm = orterm;		/* get termio to modify */

    fterm.c_lflag &= ~ICANON;	/* clear ICANON giving raw mode */
    fterm.c_cc[VMIN] = 1;	/* set MIN char count to 1 */
    fterm.c_cc[VTIME] = 0;	/* set NO time limit */
    return ioctl(0, TCSETAW, &fterm);	/* modify termio for raw mode */
#else
    struct sgttyb ts;
    gtty(fileno(stdin), &ts);
    ts.sg_flags |= RAW;
    return stty(fileno(stdin), &ts);
#endif
#else
    return(-1);
#endif
}
