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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/cmd/RCS/main.c,v 1.15 1992/12/15 21:24:03 rr2b R6tape $";
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/file.h */
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <errprntf.h>
#include <util.h>
#include <mailconf.h>
#include <mail.h>
#include <conf.h>

char *newsgroups = NULL;
char *distribs = NULL;

char tmpdir[MAXPATHLEN+1];
char destdir[MAXPATHLEN+1];
char holddir[MAXPATHLEN+1];
char last_time[MAXPATHLEN+1];
char server[MAXPATHLEN+1];
char logfile[MAXPATHLEN+1];
#ifdef DEBUG
char	*logaddr=0;	    /*turn errprintf logging off when debugging */
#else /* DEBUG */
char	*logaddr="bb+andrew.daemons.nntppoll";
#endif /* DEBUG */
char AuthString[1000] = "\0";

char dupfile[MAXPATHLEN+1];

char *progname = "nntppoll";
int poll_interval= 1800;
int nnt_stat = 0;
jmp_buf env;

static int OnePassOnly = 0;

extern int vfclose();
extern int errno;

main(argc,argv)
int argc;
char **argv;
{
	FILE *fp;
	char lastbuf[MAXPATHLEN], *flag, *lasttm;
	register char *cp;
	int i, pid, FirstTime=1, sval, Debug=0;
#ifdef UseVMail
	int VMailInUse = 0;
#endif /* UseVMail */
    
	CheckAMSConfiguration();
	for (i = 1; argv[i] != NULL && argv[i][0] == '-';){
		cp = argv[i++];
		switch (*++cp) {
		    case 'n':
			newsgroups = argv[i++];
			n_errprintf(progname, ERR_WARNING, 0, 0,
				    "Newsgroups: \"%s\"", newsgroups);
			break;
		    case 'L':
			distribs = argv[i++];
			n_errprintf(progname, ERR_WARNING, 0, 0,
				    "Distributions: \"%s\"", distribs);
			break;
		    case 'N':
			strcpy(dupfile, argv[i++]);
			break;
		    case 'd':
			strcpy(destdir, argv[i++]);
			break;
		    case 'h':
			strcpy(holddir, argv[i++]);
			break;
		    case 't':
			strcpy(tmpdir, argv[i++]);
			break;
		    case 'i':
			poll_interval = atoi(argv[i++]);
			break;
		    case 'l':
			strcpy(last_time, argv[i++]);
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
		    case 'D':
			Debug = 1;
			break;
		    case 'O':
			OnePassOnly = 1;
			break;
		    case 'V':
#ifndef UseVMail
			printf ("The -V switch can only be used when compiled with -DUseVMail\n");
			/* fall through */
#else /* UseVMail */
			VMailInUse = 1;
			break;
#endif /* UseVMail */
		    default:
			printf("Usage: %s [-n newsgroups][-L distributions][-N dupdb][-d destdir][-h holddir][-t tmpdir][-i poll_interval][-l lasttime][-s server][-f logfile][-A authstring] [-D](debug) [-V](UseVMail)\n",
			       progname);
			return(nnt_exit(-2));
		}
	}
#ifdef UseVMail
	if(!VMailInUse){
	    printf("You must use the -V switch if you are going to use -DUseVMail\n");
	    printf("Usage: %s [-n newsgroups][-L distributions][-N dupdb][-d destdir][-h holddir][-t tmpdir][-i poll_interval][-l lasttime][-s server][-f logfile][-A authstring] [-D](debug) [-V](UseVMail)\n",
		   progname);
	    return(nnt_exit(-2));
	}
#endif /* UseVMail */

	if (AuthString[0] == '\0') {
		sprintf(AuthString, "0;%s;Network-Mail", ThisDomain);
	}

	if (!Debug) {
	    if (getuid() != 0) {
		struct CellAuth *ca;
		char *PMName;
		int OK;

		OK = 0;
		ca = NULL;
		FindAMSHomeCell(&ca);
		if (ca != NULL && ca->UserName == NULL) FillInCell(ca);
		if (ca != NULL && ca->UserName != NULL) {
		    PMName = CheckAMSPMName(ca->CellName);
		    if (PMName != NULL && strcmp(PMName, ca->UserName) == 0) OK = 1;
		}
		if (OK == 0) {
		    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Must be root or postman.");
		    exit(-1);
		}
		if (ca != NULL) CkAMSCellConfig(ca->CellName);
	    }

	    pid = fork();
	    if (pid == -1) n_errprintf(progname, ERR_CRITICAL, 0, 0, "Can't fork: ", errno);

	    if (pid != 0)
		exit(0);
	}



	if (Debug) printf("destdir %s holddir %s tmpdir %s server %s last_time %s poll_interval %d\n",
	      destdir, holddir, tmpdir, server, last_time, poll_interval);

    for (;;) {
	extern int move_files(), del_files();
	if (FirstTime) {
		FirstTime = 0;
	} else if (!FirstTime) {
		if (Debug) fprintf(stderr, "OtherSleep(%d)\n", poll_interval);
		OtherSleep(poll_interval);
	}

	if ((fp = fopen(last_time, "r")) == NULL) {
		if (!tfail(errno)) n_errprintf(progname, ERR_CRITICAL, 0, 0,
			    "Cannot open %s for reading (%d).",
			last_time, errno);
		FirstTime = 0;
		continue;
	};
	if (!fgets(lastbuf, sizeof(lastbuf), fp)) {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, "Empty time file: %s.",
			    last_time);
		vfclose(fp);
		continue;
	}
	fclose(fp);
	lastbuf[strlen(lastbuf)-1] = '\0';
	flag = &lastbuf[0];
	lasttm = &lastbuf[2];
	while (*lasttm == ' ') ++lasttm;
	nnt_stat = atoi(flag);	
	if (nnt_stat == 1 && !move_files())
		continue;
	else if (nnt_stat != 1 && !del_files()) 
		continue;

	if (Debug) printf("Flag: %d lasttm: %s\n", nnt_stat, lasttm);

	sval = setjmp(env);
	if (!sval)
	    nnt_stat = nntppoll(Debug);
	if (nnt_stat == 1) {
	    move_files();
	    if (OnePassOnly) {
		n_errprintf(progname, ERR_CRITICAL, 0, 0, "exiting");
		return 0;
	    }
	} else {
	    del_files();
	}
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
}
