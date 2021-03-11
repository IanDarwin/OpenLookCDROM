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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/nntppoll.c,v 1.43 1993/09/21 22:05:52 gk5g Exp $";
#endif

/*
 * Obtains all articles in the groups from the distributions specified
 * by GRP and DEST by polling a given server. All articles that are
 * new since a time contained in the file LAST_TIME will be obtained.
 * The time contained in this file is updated only if all articles are
 * successfully obtained. LAST_TIME also contains the exit status (1,
 * -1,-2) of the program. The NEWARTS file is a list of ids of the
 * articles that are new.
 * 
 *
*/
#include <stdio.h>
#include <ctype.h>
#include <system.h>
#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h strings.h */
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
#include <conf.h>
#include <respcode.h>

extern int errno;
extern char *arpadate();

char *DSTS = "<world,usa,pgh,net>";
char *GRPS = "comp.*,news.*,sci.*,rec.*,misc.*,soc.*,talk.*";

char *newarts = "newarts";
extern char tmpdir[];
extern char destdir[];
extern char holddir[];
extern char last_time[];
extern char server[];
extern char *logaddr;
extern char *progname;
extern int poll_interval;
#ifdef UseVMail
extern int vfclose();
extern char *VM_text[];
#endif /* UseVMail */
extern FILE *ser_rd_fp, *ser_wr_fp;
extern jmp_buf env;
extern int nnt_stat;
FILE *tfp, *popen();
extern char *newsgroups, *distribs;
extern char AuthString[];

#ifndef UseVMail
static char tempfilename[MAXPATHLEN] = "";
#else /* UseVMail */
static char temp[MAX_STRLEN] = "";
#endif /* UseVMail */
static char artsbuf[MAX_STRLEN] = "";
static unsigned long int StartStamp, EndStamp;
static int numnew, numerr, numdups;

static Debugging = 0;

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
#ifdef UseVMail
    char arfile[MAX_STRLEN], name[MAXPATHLEN];
    struct stat sbuf;

    GetHostDomainName(name, sizeof(name));
    sprintf(arfile, "%s/%s.%d.%d", tmpdir, name, getpid(), numnew);
    if ((stat(arfile, &sbuf) == 0) || (errno != ENOENT)) unlink(arfile);
#else /* UseVMail */
    (void) unlink(tempfilename);
#endif /* UseVMail */

    if (artsbuf[0] != '\0') unlink(artsbuf);
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "Lost connection (%d new, %d dups, %d errors).", numnew, numdups, numerr);
    closefiles();
    nnt_stat = nnt_exit(-2);
    longjmp(env, 1);
}

static int MightHang = 0;

timeout()
{
#ifdef UseVMail
    char arfile[MAX_STRLEN], name[MAXPATHLEN];
    struct stat sbuf;
#endif /* UseVMail */

    if (MightHang == 0) return;

#ifdef UseVMail
    GetHostDomainName(name, sizeof(name));
    sprintf(arfile, "%s/%s.%d.%d", tmpdir, name, getpid(), numnew);
    if ((stat(arfile, &sbuf) == 0) || (errno != ENOENT)) unlink(arfile);
#else /* UseVMail */
    (void) unlink(tempfilename);
#endif /* UseVMail */

    if (artsbuf[0] != '\0') unlink(artsbuf);
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "Process hung (%d new, %d dups, %d errors).", numnew, numdups, numerr);
    closefiles();
    nnt_stat = nnt_exit(-2);
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
    if (i == 0) free(files);
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

void OtherSleep(secs)
long int secs;
{/* Sleep for ``secs'' seconds before returning to the caller.  But poll our other tasks, too. */

    /* No longer any other tasks */
    sleep(secs);
}

#define MAX_IN_DIR 3000
extern char *ams_genid();
#ifndef UseVMail
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
#endif /* UseVMail */

dumpstats()
{
    /* to allow checking of progress */
    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		 "new: %5d,   dup: %5d,   err: %5d  (wr %s)",
		 numnew, numdups, numerr, holdwritedir);
}

nntppoll(Debug) int Debug;
{
#ifdef UseVMail
    int	vm_status;
#endif /* UseVMail */
    int	NumExpected, resp, ThisArt;
    char	line[MAX_STRLEN], lastbuf[MAXPATHLEN], *lasttm, abuf[MAX_STRLEN];
    char *cp, *lp;
    auto char Hdr[4000];
    char *HdrPtr;
    int InHeader, HdrLeft, LineLen;

    numnew = numerr = numdups = 0;
    Debugging = Debug;
    MightHang = 0;
#ifndef UseVMail
    sprintf(tempfilename, "%s/%s", tmpdir, "nntp.tempfile");
#endif /* UseVMail */
    StartStamp = time(0);
    strcpy(line, ctime(&StartStamp));
    cp = strchr(line, '\n');
    if (cp) *cp = '\0';
    n_errprintf(progname, ERR_WARNING, 0, 0, "Starting at %s", line);
    if (numHoldWrite < 0) InitDirs();	/* Start directory-maintenance stuff. */
    if (!(tfp = fopen(last_time, "r"))) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't open %s for reading: %s.", last_time, UnixError(errno));
	return(nnt_exit(-2));
    };
    if (!fgets(lastbuf, sizeof(lastbuf), tfp)) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Empty time file: %s", last_time);
	vfclose(tfp);
	return(nnt_exit(-2));
    }

    lasttm = &lastbuf[2];
    while (*lasttm == ' ') ++lasttm;
    if (Debugging) fprintf(stdout, "nntppoll: Last_time: %s\n", lasttm);
    fclose(tfp);
    lasttm[strlen(lasttm) - 1] = '\000';
    signal(SIGALRM, timeout);
    alarm(7000); 	/* A little under two hours */
    errno = 0;
    MightHang = 1;
    if (server_init(server)) {
	MightHang = 0;
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "%s: connect failed: ", server, UnixError(errno));
	return(nnt_exit(-2));
    };
    MightHang = 0;
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
    MightHang = 1;
    fprintf(ser_wr_fp, "NEWGROUPS %s GMT %s\r\n", lasttm, (distribs != NULL ? distribs : DSTS));
    fflush(ser_wr_fp);
    get_server(line, sizeof(line));
    MightHang = 0;
    if (*line != CHAR_OK) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "%s NEWGROUPS failed: ``%s''", server, line);
	return(nnt_exit(-1));
    };
    MightHang = 1;
    while (!get_server(line, sizeof(line))) {
	char *s;

	MightHang = 0;
	if (!strcmp(line, "."))
	    break;
	if (s = strchr(line, ' '))
	    *s = '\0';
	n_errprintf(progname, ERR_WARNING, 0, 0, "New newsgroup: %s", line);
	MightHang = 1;
    }
    MightHang = 0;
    sprintf(artsbuf, "%s.%d", newarts, getpid());
    if (!(tfp = fopen(artsbuf, "w"))) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't open %s for writing: %s", artsbuf, UnixError(errno));
	return(nnt_exit(-1));
    }
    MightHang = 1;
    fprintf(ser_wr_fp, "NEWNEWS %s %s GMT\r\n", newsgroups != NULL ? newsgroups : GRPS, lasttm);
    fflush(ser_wr_fp);
    get_server(line, sizeof(line));
    MightHang = 0;
    if (*line != CHAR_OK) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "%s NEWNEWS failed: ``%s''", server, line);
	unlink(artsbuf);
	fclose(tfp);
	return(nnt_exit(-1));
    };
    NumExpected=0;
    MightHang = 1;
    clearerr(tfp);
    while (!get_server(line, sizeof(line))) {
	MightHang = 0;
	if (!strcmp(line, "."))
	    break;
	if (line[0] != '\0') {
	    fputs(line, tfp); fputc('\n', tfp);
	    ++NumExpected;
	}
	MightHang = 1;
    };
    MightHang = 0;
    fflush(tfp);
    if (ferror(tfp) || feof(tfp)) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "write to %s failed: %s", artsbuf, UnixError(errno));
	unlink(artsbuf);
	fclose(tfp);
	return(nnt_exit(-1));
    }
    if (fclose(tfp) != 0) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "fclose failed writing %s: %s", artsbuf, UnixError(errno));
	unlink(artsbuf);
	return(nnt_exit(-1));
    }

    n_errprintf(progname, ERR_WARNING, 0, 0, "Number of articles expected: %d", NumExpected);
    if (!(tfp = fopen(artsbuf, "r")))  {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't read %s: %s", artsbuf, UnixError(errno));
	unlink(artsbuf);
	return(nnt_exit(-1));
    }
    ThisArt = 0;
    for (;;) {
	register char *s;
	int GotAny;
#ifndef UseVMail
	FILE *fileTemp;
	int code;
#endif /* UseVMail */

	clearerr(tfp); errno = 0;
	s = fgets(abuf, sizeof(abuf), tfp);
	if (s == NULL) {
	    if (ferror(tfp) || !feof(tfp)) {
		n_errprintf(progname, ERR_WARNING, 0, 0,
			    "Error reading msg-id file %s: %s (%d of %d)", artsbuf, UnixError(errno), ThisArt, NumExpected);
		unlink(artsbuf);
		fclose(tfp);
		return(nnt_exit(-1));
	    }
	    /* Must be EOF */
	    break;
	}
	++ThisArt;
	for (s = &abuf[strlen(abuf)-1]; s >= abuf; --s) {
	    if (isprint(*s) && *s != ' ') break;
	    *s = '\0';
	}
	if (duplicate(abuf)) {
	    alarm(7000);
	    ++numdups;
	    continue;
	}
	MightHang = 1;
	fputs("ARTICLE ", ser_wr_fp);
	fputs(abuf, ser_wr_fp);
	fputs("\r\n", ser_wr_fp);
	fflush(ser_wr_fp);
	get_server(line, sizeof(line));
	MightHang = 0;
	cp = line;
	while(*cp != '\0' && isascii(*cp) && !isdigit(*cp))
	    ++cp;	/* skip anything leading */
	if (*cp == '\0' || !isascii(*cp))
	    resp = 0;
	else resp = atoi(cp);
	if (Debugging && resp >=  300 && resp < 600)
	    printf("response: %d\n", resp);
	if (resp != 220) {
	    if (resp >= 300 && resp < 400) {
		s = "unexpected continuation code";
	    } else if (resp >= 400 && resp < 500) {
		s = "transient difficulty";
	    } else if (resp >= 500 && resp < 600) {
		s = "persistent problem";
	    } else {
		s = "bad response code";
	    }
	    n_errprintf(progname, ERR_CRITICAL, 0, logaddr, "%s %s: code %d, msg id %s: ``%s'' (err %d, %d of %d)", server, s, resp, abuf, line, numerr, ThisArt, NumExpected);
	    ++numerr;
	    continue;
	}
#ifndef UseVMail
	fileTemp = fopen(tempfilename, "w");
	/* might want to do more secure checking to make sure I don't write over my own file? */
	if (fileTemp == NULL) {/* unsuccessful open */
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Unable to write '%s': %s (err %d, %d of %d)", tempfilename, UnixError(errno), numerr, ThisArt, NumExpected);
	    unlink(artsbuf);
	    fclose(tfp); /*  tfp == newarts for reading */
	    return(nnt_exit(-1));
	}
	fputs("X-Andrew-Authenticated-as: ", fileTemp);
	fputs(AuthString, fileTemp);
	fputs("\nReceived: via nntppoll with nntp; ", fileTemp);
	fputs(arpadate(), fileTemp);
#else /* UseVMail */
	vm_status = VM_open(NULL, tmpdir, "<>", NULL, (AuthString[0] == '\0' ? NULL : AuthString), "nntppoll with nntp");
	if (vm_status != EX_OK) {
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "%s (%d) (%d of %d)", VM_text, vm_status, ThisArt, NumExpected);
	    unlink(artsbuf);
	    fclose(tfp);
	    return(nnt_exit(-1));
	}
#endif /* UseVMail */
	GotAny = 0;
	InHeader = 1;
	Hdr[0] = '\0';
	HdrPtr = Hdr;
	HdrLeft = sizeof(Hdr) - 2;
	MightHang = 1;
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
#ifndef UseVMail
		fprintf(fileTemp, "Path: %s!%s\n", ThisDomain, lp+6);
#else /* UseVMail */
		VM_printf("Path: %s!%s\n", ThisDomain, lp+6);
#endif /* UseVMail */
	    } else {
#ifndef UseVMail
		fputs(lp, fileTemp);
		fputc('\n', fileTemp);
#else /* UseVMail */
		VM_printf("%s\n", lp);
#endif /* UseVMail */
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
#ifndef UseVMail
	if (!GotAny) {
	    unlink(tempfilename); fclose(fileTemp);
	    n_errprintf(progname, ERR_CRITICAL, 0, 0,
			"%s Zero-len file, msg id %s; error %d in %d of %d articles (resp was %d, last line ``%s'')",
			server, abuf, numerr, ThisArt, NumExpected, resp, line);
	    ++numerr;
	    continue;
	}
	code = fclose(fileTemp);
#else /* UseVMail */
	vm_status = VM_close();
#endif /* UseVMail */
	if
#ifndef UseVMail
	    (code == EOF) /* fclose wasn't successful !! */
#else /* UseVMail */
	    (vm_status != EX_OK)
#endif /* UseVMail */
	  {
	      n_errprintf(progname, ERR_CRITICAL, 0, 0,
#ifndef UseVMail
			  "Unable to close-write '%s' (%s) (%d of %d)",
			  tempfilename, UnixError(errno),
#else /* UseVMail */
			  "%s (%d) (%d of %d)", VM_text, vm_status,
#endif /* UseVMail */
			  ThisArt, NumExpected);
	      unlink(artsbuf);
	      fclose(tfp); /*  tfp == reading newarts */
	      return(nnt_exit(-1));
	  }
#ifndef UseVMail
	if (code = RenameFile(tempfilename) != 0) {/* if rename failed !? */
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Unable to rename file (%s) (%d of %d)", UnixError(code), ThisArt, NumExpected);
	    unlink(artsbuf);
	    fclose(tfp);
	    return(nnt_exit(-1));
	}
#endif /* UseVMail */
	record(abuf);
	Capture(Hdr);
	alarm(7000); 	/* If we're making progress, reset timer */
	++numnew;
    }
    unlink(artsbuf);
    fclose(tfp);
    MightHang = 1;
    close_server();
    MightHang = 0;
    n_errprintf(progname, ERR_WARNING, 0, 0,
		 "Transfer complete (%d new, %d dups, %d errors).",
		 numnew, numdups, numerr); 
    EndStamp = time(0);
    strcpy(line, ctime(&EndStamp));
    cp = strchr(line, '\n');
    if (cp) *cp = '\0';
    n_errprintf(progname, ERR_WARNING, 0, 0, "Ending at %s", line);

    return(nnt_exit(1));
}
/* 3 ways program exits :
  Return -2 : Couldn't open time file, file not written.
   -1 : Other errors, file written with -1 and old time.
   1 : No errors, file written with 1 and new time.
*/
nnt_exit(nstat)
int nstat;
{
    FILE *fp;
    char lastbuf[MAXPATHLEN], *lasttm;
    char lfbuf[MAXPATHLEN];
    unsigned long int SlushStartStamp;
    struct tm *tmval;

    if (nstat != 1) {
	if (nstat == -1) {
	    if (fclose(ser_wr_fp) != 0) {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in closing (writing) socket descriptor: %s", UnixError(errno));
	    }
	    if (fclose(ser_rd_fp) != 0)  {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in closing (reading) socket descriptor: %s", UnixError(errno));
	    }
	}
	if (!(fp = fopen(last_time, "r"))) {
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't open %s for reading: %s", last_time, UnixError(errno));
	    return(-2);
	}
	if (!fgets(lastbuf, sizeof(lastbuf), fp)) {
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Empty time file: %s", last_time);
	    vfclose(fp);
	    return(-2);
	}
	fclose(fp);
    }
    strcpy(lfbuf, last_time);
    strcat(lfbuf, ".xx");
    errno = 0;
    fp = fopen(lfbuf, "w");
    if (fp == NULL) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't open %s for writing: %s", lfbuf, UnixError(errno));
	return(-2);
    }
    lasttm = &lastbuf[2];	/* what was previously contained in last_time */
    while (*lasttm == ' ') ++lasttm;
    if (lasttm[strlen(lasttm)-1] == '\n') lasttm[strlen(lasttm)-1] = '\0';
    if (nstat != 1) {
	fprintf(fp, "-1 %s\n", lasttm);
    } else {
	SlushStartStamp = StartStamp - (15*60);	/* fifteen minutes early */
	tmval = gmtime(&SlushStartStamp);
	if (Debugging) fprintf(stdout,
			       "Setting last_time to %02d%02d%02d %02d%02d%02d: %s",
			       tmval->tm_year, tmval->tm_mon + 1, tmval->tm_mday,
			       tmval->tm_hour, tmval->tm_min, tmval->tm_sec,
			       ctime(&SlushStartStamp));
	fprintf(fp, "1 %02d%02d%02d %02d%02d%02d\n",
		tmval->tm_year, tmval->tm_mon + 1, tmval->tm_mday,
		tmval->tm_hour, tmval->tm_min, tmval->tm_sec);
    }
    fflush(fp);
    if (ferror(fp) || feof(fp)) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in writing %s: %s", lfbuf, UnixError(errno));
	(void) unlink(lfbuf); (void) fclose(fp);
	return(-2);
    }
    if (vfclose(fp) != 0) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in closing-for-write %s: %s", lfbuf, UnixError(errno));
	(void) unlink(lfbuf);
	return(-2);
    }
    if (rename(lfbuf, last_time) != 0) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in renaming %s to %s: %s", lfbuf, last_time, UnixError(errno));
	(void) unlink(lfbuf);
	return(-2);
    }
    return(nstat);
}    

closefiles()
{
    int f, numnotopen=0, numclosed=0;

#ifdef tst_NDBM
    CloseDBM();
#endif /* tst_NDBM */
    for (f=getdtablesize(); f > 2; --f)
	if (close(f) == -1) {
	    if (errno == EBADF) {   /* not open */
		++numnotopen;
		continue;
	    }
	    if (Debugging) printf("close(%d) errno: %d\n", f, errno);
	}
	else ++numclosed;

    if (Debugging) printf("numclosed %d numnotopen %d\n", numclosed, numnotopen);
}


int move_files()
{
    char tmp_filename[MAXPATHLEN+1];
    register int i;
    int nfiles, err, movedcount;
#ifndef UseVMail
    int code;
#else /* UseVMail */
    char dest_filename[MAXPATHLEN+1];
#endif /* UseVMail */
    register FileList *files;

    files = GetDirEntries(tmpdir, &nfiles, &err, 0);
    if (err != 0) {
	n_errprintf(progname, ERR_CRITICAL, 0, 0,
		    "Can't open %s (err=%d).", tmpdir, err);
	return 0;
    }
    if (nfiles == 0) return 1;
    SortFiles(files, nfiles);

    if (numHoldWrite < 0) InitDirs();	/* Start directory-maintenance stuff. */
    movedcount = 0;
    for (i=0; i<nfiles; i++) {
	register char *name;

	name = files[i].Name;
	if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
	sprintf(tmp_filename, "%s/%s", tmpdir, name);
#ifdef UseVMail
	sprintf(dest_filename, "%s/%s", destdir, name);
	if (rename(tmp_filename, dest_filename) != 0)
#else /* UseVMail */
	if (code = RenameFile(tmp_filename) != 0)
#endif /* UseVMail */
	  {
#ifdef UseVMail
	      int err = errno;
#endif /* UseVMail */
	      n_errprintf(progname, ERR_CRITICAL, 0, 0,
#ifndef UseVMail
			  "Can't rename %s: %s", tmp_filename, UnixError(code)
#else /* UseVMail */
			  "Can't rename %s to %s: %s", tmp_filename, dest_filename, UnixError(err)
#endif /* UseVMail */
			  );
	      if (vdown(err) || err == EFBIG) break;
	  }
	else {
#ifndef UseVMail
	    n_errprintf(progname, ERR_WARNING, 0, 0, "Moved file from %s to %s", tmpdir, destdir);
	    ++movedcount;
#else /* UseVMail */
	    ++movedcount;
#endif /* UseVMail */
	}
    }
#ifdef UseVMail

    FreeFiles(files, nfiles);
    n_errprintf(progname, ERR_WARNING, 0, 0,
		 "%d files of %d moved to %s", movedcount, nfiles, destdir);
    return (movedcount==nfiles ? 1 : 0);
#endif /* UseVMail */
}

int del_files()
{
    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Del_files called: now moving from %s", tmpdir);
    return move_files();
}
