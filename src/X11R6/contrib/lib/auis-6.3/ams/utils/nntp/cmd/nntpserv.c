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

static char *nntpserv_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/cmd/RCS/nntpserv.c,v 1.6 1993/10/04 18:34:40 gk5g Exp $";

#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h strings.h */
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include <ckndbm.h>
#include <signal.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#include <sys/param.h>
#include <errprntf.h>
#include <errno.h>
#include <setjmp.h>
#include <netinet/in.h>
#include <svcconf.h>
#include <pwd.h>
#include <conf.h>

extern int errno;
extern char *arpadate();

char tmpdir[MAXPATHLEN+1];
char holddir[MAXPATHLEN+1];
char server[MAXPATHLEN+1];
char logfile[MAXPATHLEN+1];
char AuthString[1000];
char username[40];
extern FILE *ser_rd_fp, *ser_wr_fp;
extern jmp_buf env;
char dupfile[MAXPATHLEN+1];
char *progname = "nntpserv";
int nnt_stat = 0;
jmp_buf env;

static char tempfilename[MAXPATHLEN] = "";
static FILE *fileTemp;
static char artsbuf[MAX_STRLEN] = "";
static unsigned long int StartStamp, EndStamp;
static int numnew, numerr, numdups;

static Debugging = 0;

main(argc,argv)
int argc;
char **argv;
{
	register char *cp;
	int i, sval, Debug=0;
    
	CheckAMSConfiguration();
	for (i = 1; argv[i] != NULL && argv[i][0] == '-';){
		cp = argv[i++];
		switch (*++cp) {
		    case 'N':
			strcpy(dupfile, argv[i++]);
			break;
		    case 'h':
			strcpy(holddir, argv[i++]);
			break;
		    case 't':
			strcpy(tmpdir, argv[i++]);
			break;
		    case 's':
			strcpy(server, argv[i++]);
			break;
		    case 'f':
			strcpy(logfile, argv[i++]);
			break;
		    case 'A':
			strcpy(AuthString, argv[i++]);
			break;
		    case 'u':
			strcpy(username, argv[i++]);
			break;
		    case 'D':
			Debug = 1;
			break;
		    default:
		    usage:
			printf("Usage: %s [-N dupdb] -h holddir -t tmpdir -s server -f logfile [-A authstring] [-u username] [-D](debug)\n",
			       progname);
			exit(-2);
		}
	}

	if (AuthString[0] == '\0') {
		sprintf(AuthString, "0;%s;Network-Mail", ThisDomain);
	}

	if (holddir[0] == '\0' || tmpdir[0] == '\0' || server[0] == '\0'
	    || logfile[0] == '\0') {
	    goto usage;
	}

	if (Debug) printf("holddir %s tmpdir %s server %s\n",
	      holddir, tmpdir, server);

	if (server_create()) {
	    exit(1);
	}

	if (username[0] != '\0') {
	    struct passwd *pw;

	    pw = getpwnam(username);
	    if (!pw) {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, "can't find user %s",
			    username);
		exit(1);
	    }
	    setuid(pw->pw_uid);
	}

	for (;;) {
	    if (server_accept(server)) {
		exit(1);
	    }

	    sval = setjmp(env);
	    if (!sval)
	      nnt_stat = nntpserv(Debug);
	    server_disconnect();
	}
}


n_errprintf(application, type, log, id, format, s1, s2, s3, s4, s5, s6,
	    s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)
int type;
char *application, *log, *id, *format, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, 
     *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
    FILE *fp;

    errprintf(application, type, log, id, format, s1, s2, s3, s4, s5, s6,
	      s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
    
    if (!log || *log == 0) {
	if ((fp = fopen(logfile,"a")) == NULL) {
	    errprintf(application, ERR_CRITICAL, 0, 0,
		      "Cannot open log file %s (%d)", logfile, errno);
	    return(0);
	}
	if (type == ERR_CRITICAL) fprintf(fp, "<critical:%s>", application);
	else if (type == ERR_WARNING) fprintf(fp, "<warning:%s>", application);
	else fprintf(fp, "<%s>", application);


	fprintf(fp, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
		s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
	fprintf(fp, "\n");
	fflush(fp);
	fclose(fp);
    }
    return 0;
}



/* DBM maintenance stuff */
#ifdef tst_NDBM
static DBM *nn_db = NULL;

static void OpenDBM()
{
    extern char dupfile[];
    nn_db = NULL;
    if (dupfile[0] != '\0') nn_db = dbm_open(dupfile, O_RDWR, 0644);
    if (nn_db == NULL && dupfile[0] != '\0')
	n_errprintf(progname, ERR_CRITICAL, 0, 0,
		    "dbm_open(%s) failed: %s", dupfile, UnixError(errno));
}
static void CloseDBM()
{
    if (nn_db != NULL) dbm_close(nn_db);
    nn_db = NULL;
}
#endif /* tst_NDBM */

static void record(id)
char *id;
{
#ifdef tst_NDBM
    long now;
    datum key, value;

    if (nn_db == NULL) OpenDBM();
    if (nn_db == NULL) return;
    key.dptr = id;
    key.dsize = strlen(id);
    now = time(0);
    now = htonl(now);
    value.dptr = (char *) &now;
    value.dsize = sizeof(now);
    (void) dbm_store(nn_db, key, value, DBM_REPLACE);
    if (dbm_error(nn_db)) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0,
		    "dbm_store(%s) got error: %s; resetting...", id, UnixError(errno));
	CloseDBM();
    }
#endif /* tst_NDBM */
}

static int duplicate(id)
char *id;
{
#ifdef tst_NDBM
    datum key, dum;

    if (nn_db == NULL) OpenDBM();
    if (nn_db == NULL) return 0;
    key.dptr = id;
    key.dsize = strlen(id);
    dum = dbm_fetch(nn_db, key);
    if (dum.dptr != NULL) return 1;
    if (dbm_error(nn_db)) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0,
		    "dbm_fetch(%s) got error: %s; resetting...", id, UnixError(errno));
	CloseDBM();
    }
#endif /* tst_NDBM */
    return 0;
}

lostconn()
{
    if (fileTemp) fclose(fileTemp);
    (void) unlink(tempfilename);

    if (artsbuf[0] != '\0') unlink(artsbuf);
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "Lost connection (%d new, %d dups, %d errors).", numnew, numdups, numerr);
    nnt_stat = -2;
    longjmp(env, 1);
}

static int MightHang = 0;

timeout()
{
    if (MightHang == 0) return;

    if (fileTemp) fclose(fileTemp);
    (void) unlink(tempfilename);

    if (artsbuf[0] != '\0') unlink(artsbuf);
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "Process hung (%d new, %d dups, %d errors).", numnew, numdups, numerr);
    nnt_stat = -2;
    longjmp(env, 1);
}

#define NAMESIZE 32
typedef struct {
    char Name[NAMESIZE];
    long int time;
} FileList;

static int FComp(f1, f2)
FileList *f1, *f2;
{/* Comparator for qsort. */
    if (f1->time < f2->time) return -1;
    else if (f1->time > f2->time) return 1;
    else return 0;
}

static void SortFiles(files, n)
register FileList *files;
register int n;
{
    if (n > 1) qsort(files, n, sizeof(FileList), FComp);
}

static void FreeFiles(files, n)
register FileList *files;
register int n;
{
    if (n > 0) free(files);
}

#define INITIAL_DIR_SIZE    10
#define INCR_DIR_SIZE	    50

static FileList *GetDirEntries(dir, nfiles, err, onlyDirs)
char *dir;
register int *nfiles, *err, onlyDirs;
{
    DIR *dp;
    register DIRENT_TYPE *ent;
    static char msg[] = "Out of storage (%d) in GetDirEntries for \"%s\"";
    register FileList *files;
    register int fsize;	    /* Max # slots in files array */
    register int i;
    int dummy;
    char f[MAXPATHLEN+1];
    struct stat buf;

    *nfiles = 0;
    if (err == NULL) err = &dummy;
    *err = 0;
    dp = opendir(dir);
    if (dp == NULL) {
	*err = errno;
	return NULL;
    }

    /* Allocate initial space */
    fsize = INITIAL_DIR_SIZE;
    files = (FileList *) malloc(INITIAL_DIR_SIZE * sizeof(FileList));
    if (files == NULL) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, msg, 1, dir);
	closedir(dp);
	*err = -1;
	return NULL;
    }

    for (i=0, ent=readdir(dp); ent!=NULL; ent=readdir(dp)) {
	/* Ignore . and .. */
	if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
	if (DIRENT_NAMELEN(ent) >= NAMESIZE) {
#ifdef ENAMETOOLONG
	    *err = ENAMETOOLONG;
#else /* ENAMETOOLONG */
	    *err = EINVAL;
#endif /* ENAMETOOLONG */
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Name in dir ``%s'' too big: ``%s''\n",
			dir, ent->d_name);
	    continue;
	}

	if (i >= fsize) {
	    /* Need more space */
	    fsize += INCR_DIR_SIZE;
	    files = (FileList *) realloc(files, fsize*sizeof(FileList));
	    if (files == NULL) {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, msg, 2, dir);
		closedir(dp);
		*err = -1;
		return NULL;
	    }
	}

	/* Make sure file is still there */
	strcpy(f, dir);
	strcat(f, "/");
	strcat(f, ent->d_name);
	if (stat(f, &buf) < 0) {
	    if (errno == ENOENT)	/* Someone else picked it up */
		continue;

	    *err = errno;
	    n_errprintf(progname, ERR_CRITICAL, 0, 0,
			"Can't stat ``%s'': %s", f, UnixError(errno));
	    closedir(dp);
	    FreeFiles(files, i);
	    return NULL;
	}
	if (onlyDirs && ((buf.st_mode & S_IFMT) != S_IFDIR)) continue;

	files[i].time = buf.st_mtime;
	strcpy(files[i++].Name, ent->d_name);
    }

    closedir(dp);
    if (i == 0) {
	free(files);
	files = 0;
    }
    *nfiles = i;
    return files;
}

#define MINSECSPERMSG 5	/* Adaptively dinked with */

static char holdwritedir[MAXPATHLEN+1];
static int numHoldWrite = -1;

static void ckHold()
{/* Find the directory in holddir. */
    DIR *dirp; DIRENT_TYPE *n;
    int numfiles, gdeError;
    FileList *gde;

    numHoldWrite = -1;
    holdwritedir[0] = '\0';
    gde = GetDirEntries(holddir, &numfiles, &gdeError, 1);
    if (gdeError == ENOENT) numfiles = 0;
    else if (gdeError != 0) return;
    if (numfiles == 0) return;
    SortFiles(gde, numfiles);
    sprintf(holdwritedir, "%s/%s", holddir, gde[numfiles-1].Name);
    FreeFiles(gde, numfiles);
    dirp = opendir(holdwritedir);
    if (dirp == NULL) return;
    for (numfiles = 0;;) {
	n = readdir(dirp);
	if (n == NULL) break;
	if (strcmp(n->d_name, ".") != 0 && strcmp(n->d_name, "..") != 0)
	    ++numfiles;
    }
    closedir(dirp);
    numHoldWrite = numfiles;
}

static void InitDirs()
{/* Set up local storage. */
    ckHold();
    n_errprintf(progname, ERR_WARNING, 0, 0,
		 "InitDirs: holdwritedir %s, numHoldWrite %d",
		 holdwritedir, numHoldWrite);
}


#define MAX_IN_DIR 3000
extern char *ams_genid();
#define MAXTRIES 100 /* for lack of a better number */

static NewHoldDir()
{
    char newwrite[MAXPATHLEN+1];

    sprintf(newwrite, "%s/%s", holddir, ams_genid(1));
    if (mkdir(newwrite, 0755) == 0) {
	if (Debugging) n_errprintf(progname, ERR_WARNING, 0, 0,
				   "New hold dir: %s (prev %s with %d entries)",
				   newwrite, holdwritedir, numHoldWrite);
	strcpy(holdwritedir, newwrite);
	numHoldWrite = 0;
    }
}
    

int RenameFile(tempfilename)
char *tempfilename;
{
    char newfilename[MAXPATHLEN];
    int tries = 0;

    if (holdwritedir[0] == '\0' || numHoldWrite >= MAX_IN_DIR) {
	NewHoldDir();
    }

    while (tries < MAXTRIES) {
	sprintf(newfilename, "%s/%s", holdwritedir, ams_genid(1));
	if (rename(tempfilename, newfilename) != 0) {
	    if (errno == ENOENT) {
		/* someone snarfed the hold directory */
		NewHoldDir();
		continue;
	    }
		
	    n_errprintf(progname, ERR_WARNING, 0, 0, "Unable to rename '%s' to '%s'; trying again. (errno=%d)", tempfilename, newfilename, errno);
	    continue;
	}
	numHoldWrite++;
	return(0);
    }
    if (errno == 0) errno = ENOTBLK;
    return(errno);
}

dumpstats()
{
    /* to allow checking of progress */
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "new: %5d,   dup: %5d,   err: %5d  (wr %s)",
		 numnew, numdups, numerr, holdwritedir);
}

nntpserv(Debug) int Debug;
{
    char	line[MAX_STRLEN], msgid[MAX_STRLEN];
    char *cp, *lp;
    auto char Hdr[4000];
    char *HdrPtr;
    int InHeader, HdrLeft, LineLen;

    numnew = numerr = numdups = 0;
    Debugging = Debug;
    MightHang = 0;
    sprintf(tempfilename, "%s/%s", tmpdir, "nntp.tempfile");
    StartStamp = time(0);
    strcpy(line, ctime(&StartStamp));
    cp = strchr(line, '\n');
    if (cp) *cp = '\0';
    n_errprintf(progname, ERR_WARNING, 0, 0, "Starting at %s", line);
    if (numHoldWrite < 0) InitDirs();	/* Start directory-maintenance stuff. */
    signal(SIGALRM, timeout);
    alarm(7000); 	/* A little under two hours */
    errno = 0;
    signal(SIGPIPE, lostconn);
#ifdef SIGXFSZ
    signal(SIGXFSZ, dumpstats);
#else /* SIGXFSZ */
#ifdef SIGUSR2
    signal(SIGUSR2, dumpstats);
#else /* SIGUSR2 */
    signal(SIGHUP, dumpstats);
#endif /* SIGUSR2 */
#endif /* SIGXFSZ */
    for (;;) {
	int GotAny;
	int code;

	MightHang = 1;
	if (get_server(line, sizeof(line))) {
	    return 0;
	}
	MightHang = 0;
	if (!strcmp(line, "QUIT") || !strcmp(line, "quit")) {
	    put_server("205 ");
	    break;
	}
	if (strncmp(line, "IHAVE ", 6) && strncmp(line, "ihave ", 6)) {
	    put_server("500 huh?");
	    continue;
	}
	if (duplicate(line+6)) {
	    alarm(7000);
	    ++numdups;
	    put_server("435 ");
	    continue;
	}
	strcpy(msgid, line+6);
	fileTemp = fopen(tempfilename, "w");
	/* might want to do more secure checking to make sure I don't write over my own file? */
	if (fileTemp == NULL) {/* unsuccessful open */
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Unable to write '%s': %s (err %d)", tempfilename, UnixError(errno), numerr);
	    unlink(artsbuf);
	    put_server("436 ");
	    break;
	}
	fputs("X-Andrew-Authenticated-as: ", fileTemp);
	fputs(AuthString, fileTemp);
	fputs("\nReceived: via nntpserv with nntp; ", fileTemp);
	fputs(arpadate(), fileTemp);
	GotAny = 0;
	InHeader = 1;
	Hdr[0] = '\0';
	HdrPtr = Hdr;
	HdrLeft = sizeof(Hdr) - 2;
	MightHang = 1;
	put_server("335 ");
	while (!get_server(line, sizeof(line))) {
	    MightHang = 0;
	    if (!strcmp(line, "."))
		break;
	    if (!strncmp(line,"..",2)) {
		lp = &line[1];	/* .. => . */
	    } else {
		lp = line;
	    }
	    if (InHeader && ULstlmatch(lp, "Path: ") == 1 && (strncmp(lp+6, ThisDomain, ThisDomainLen) != 0 || lp[6+ThisDomainLen] != '!')) {
		fprintf(fileTemp, "Path: %s!%s\n", ThisDomain, lp+6);
	    } else {
		fputs(lp, fileTemp);
		fputc('\n', fileTemp);
	    }
	    if (InHeader) {
		LineLen = (strlen(lp) + 1);
		if (LineLen == 1) InHeader = 0;
		else if (HdrLeft < LineLen) InHeader = 0;
		else {
		    strcpy(HdrPtr, lp);
		    strcat(HdrPtr, "\n");
		    HdrPtr += LineLen;
		    HdrLeft -= LineLen;
		}
	    }
	    GotAny = 1;
	    MightHang = 1;
	}
	MightHang = 0;
	if (!GotAny) {
	    unlink(tempfilename); fclose(fileTemp); fileTemp = 0;
	    n_errprintf(progname, ERR_WARNING, 0, 0,
			"%s Zero-len file, msg id %s; error %d",
			server, msgid, numerr);
	    ++numerr;
	    put_server("437 ");
	    continue;
	}
	code = fclose(fileTemp);
	fileTemp = 0;
	if (code == EOF) /* fclose wasn't successful !! */
	  {
	      n_errprintf(progname, ERR_CRITICAL, 0, 0,
			  "Unable to close-write '%s' (%s)",
			  tempfilename, UnixError(errno));
	      unlink(artsbuf);
	      put_server("436 ");
	      break;
	  }
	if (code = RenameFile(tempfilename) != 0) {/* if rename failed !? */
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Unable to rename file (%s)", UnixError(code));
	    unlink(artsbuf);
	    put_server("436 ");
	    break;
	}
	record(msgid);
	Capture(Hdr);
	put_server("235 ");
	alarm(7000); 	/* If we're making progress, reset timer */
	++numnew;
    }
    n_errprintf(progname, ERR_WARNING, 0, 0,
		 "Transfer complete (%d new, %d dups, %d errors).",
		 numnew, numdups, numerr); 
    EndStamp = time(0);
    strcpy(line, ctime(&EndStamp));
    cp = strchr(line, '\n');
    if (cp) *cp = '\0';
    n_errprintf(progname, ERR_WARNING, 0, 0, "Ending at %s", line);

    return(0);
}
