/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 * 	   Copyright Carnegie Mellon University, 1992 *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/createinset/cmd/RCS/createinset.c,v 1.14 1993/05/04 01:14:42 susan Exp $";
#endif

/* createinset.c
	create a directory and initial files for an inset

	Invoked with a command line having the name of an inset:
		createinset insetname

	Builds a new subdirectory of the current directory containing the 
	files for an inset of the given name.

	The files are created by copying the dummy inset and 
	changing all instances of "dummy" to the given name.

*/

/*
 * $Log: createinset.c,v $
 * Revision 1.14  1993/05/04  01:14:42  susan
 * RCS Tree Split
 *
 * Revision 1.13.1.1  1993/02/02  01:42:17  rr2b
 * new R6tape branch
 *
 * Revision 1.13  1992/12/15  21:32:53  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.12  1992/12/15  00:41:01  rr2b
 * fixed disclaimerization
 *
 * Revision 1.11  1992/12/14  20:40:31  rr2b
 * disclaimerization
 *
 * Revision 1.10  1992/06/23  00:54:56  wjh
 * fixed type errors
 * .
 *
 * Revision 1.9  1992/06/22  21:58:36  wjh
 * added nulla.c and nulla.ch
 * .rcsi
 *
 * Revision 1.8  1991/09/12  16:11:30  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.7  1989/09/21  11:56:04  cfe
 * Include sys/param.h only if andrewos.h didn't include it.
 *
 * Revision 1.6  89/09/20  21:58:56  wjh
 * reinsert include of sys/param.h
 * 
 * Revision 1.5  89/09/11  12:12:18  cfe
 * New time standards.
 * 
 * Revision 1.4  89/09/09  09:50:19  wjh
 * change ".hlp" to ".help"
 * 
 * Revision 1.3  89/09/08  13:31:47  wjh
 * changed to null.hlp
 * combined infilelist and outfilelist
 * 
 * Revision 1.2  89/08/28  16:30:21  wjh
 * Makefile->Imakefile
 * delete nullt.c
 * 
 * Revision 1.1  89/08/28  09:45:46  wjh
 * Initial revision
 * 
 * Revision 1.6  88/05/14  17:03:35  wjh
 * changed to nullinset
 * 
 * Revision 1.5  88/04/29  11:43:57  wjh
 * fixed problem with dollar-Log: in code
 * 
 * Revision 1.4  88/04/25  16:24:50  wjh
 * create new log 
 * 
 * Revision 1.3  88/04/25  11:10:50  wjh
 * added query/reply and feedback
 * 
 * Revision 1.2  88/04/20  16:13:40  wjh
 * add MOREDOCS
 * 
 * Revision 1.1  88/04/20  15:32:37  wjh
 * Initial revision
 * 
 * Revision 1.0  88/08/25  10:00:00 wjh
 * Initial creation by WJHansen
 *
*/

#include <stdio.h>
#include <andrewos.h>	/* sys/file sys/time */
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif /* #ifndef MAXPATHLEN */
#include <pwd.h>

#include <class.h>		/* for boolean */
/* REVISE THE FOLLOWING 
 *	For changes in the list of files or names or locations.
 */

char *filelist[] = {
	"%s/Imakefile",
	"%s/%s.help",
	"%s/%s.ch",
	"%s/%s.c",
	"%s/%sv.ch",
	"%s/%sv.c",
	"%s/%svt.c",
	"%s/%sa.ch",
	"%s/%sa.c",
	NULL
};
char *sourcename = "null";
char *residence = "/lib/null";



char *insetname;		/* name chosen for new inset */
char *username;		/* the user's name (for logs) */

boolean debug;
#define DEBUG(operands)  {if (debug)(printf operands, fflush(stdout));}

/* check the result of a system call 
	if b is TRUE, is okay
	otherwise print msg and errno and exit  */
checkerrno(msg, b)
	char *msg;
	boolean b;
{
	if (b) return;
	perror(msg);
	exit(2);
}

usage() {
	printf("usage:  createinset <newdirectoryname>");
	exit(2);
}

main( argc, argv )
	register int	  argc;
	register char  **argv;
{
	FILE *inf, *outf;
	char **filename;
	short i;
	char cwd[MAXPATHLEN+1], cmd[MAXPATHLEN<<1], reply[40];
	long uid;
	struct passwd *passwd;
	char *dirname;

	/* compute username;  */
	uid = getuid();
	passwd = getpwuid(uid);
	if (passwd == 0) {
		/* no passwd entry.  punt.  use uid.  */
		static char x[20];
		sprintf(x, "user id %d", uid);
		username = x;
	}
	else username = (char *)strcpy(
			(char *)malloc(strlen(passwd->pw_name)), 
			passwd->pw_name );

	insetname = NULL;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-')
			switch (argv[i][1]) {
				case 'd': /* Debugging. */
					debug = TRUE;
					break;
				default: 
					printf("Usage: createinset insetname\n");
					exit(1);
			}
		else { /* It's the insetname (we hope) */
			insetname = argv[i];
		}
	}
	if (insetname == NULL  ||  *insetname == '\0') 
		usage();
	if (strlen(insetname) > 9) 
		printf(" ######## Warning.  Inset name should be 9 or fewer letters.  ########");

	checkerrno("getwd", getwd(cwd) != 0);
	printf("Create directory %s/%s [n] ? ", cwd, insetname);
	checkerrno("fgets", fgets(reply, 39, stdin) != 0);
	if (*reply != 'y') {
		printf("Cancelled.  You did not say \"yes\".\n");
		exit(1);
	}

	checkerrno("mkdir", mkdir(insetname, 0755) == 0);

	for (filename=filelist, i=0; *filename; filename++, i++) {}
	printf("Copying %d files\n", i);  fflush(stdout);

	dirname = (char *)AndrewDir(residence);

	for (filename=filelist, i=1; *filename; filename++, i++) {
		/* copy filename and convert it */
		int tf;
		char inname[100];
		char outname[50];
		sprintf(inname, *filename, dirname, sourcename);
		sprintf(outname, *filename, insetname, insetname);
		printf("%d ", i);   fflush(stdout);
		DEBUG(("Converting %s to %s\n", inname, outname));
		checkerrno(inname, (inf=fopen(inname, "r")) != NULL);
		checkerrno(outname, 
			(tf=open(outname, O_CREAT | O_WRONLY, 0644)) >= 0);
		checkerrno(outname, (outf=fdopen(tf, "w")) != NULL);

		convert(inf, outf);

		checkerrno(inname, fclose(inf) != EOF);
		checkerrno(outname, fclose(outf) != EOF);
	}

	printf("\nFiles are:\n");   fflush(stdout);
	sprintf(cmd, "ls -1a %s/%s\n", cwd, insetname);
	checkerrno("system(ls)", system(cmd) != 127);
}

/* copy inf to outf, converting all instances of "null" to insetname 
	The algorithm only works because the texts being processed
	do not contain the substring "nullinull" 
		where the latter portion of the word should be changed
*/
convert(inf, outf)
	register FILE *inf, *outf;
{
	register char firstletter = *sourcename;
	while (TRUE) {
		register c = getc(inf);
		if (c == EOF) return;
		if (c == firstletter) {
			/* found first letter, check for rest of source name */
			register char *test = sourcename, *tx = test;
			while (c == *tx) 
				c = getc(inf), tx++;
			/* c is first char that did not match */
			ungetc(c, inf);
			if (*tx) {
				/* there is something left, we failed to find sourcename */
				/* output contains everything before the firs letter
					tx points to first character that did not match */
				register char *ttx = test;
				while (ttx < tx)
					fputc(*ttx++, outf);
			}
			else  
				/* found sourcename,  output replacement */
				fprintf(outf, insetname);
		}
		else if (c == '$') {
			/* found a "$" check for continuation with "Log:" */
			register char *test = "Log:", *tx = test;
			fputc(c, outf);	/* output the '$' */
			while ((c=getc(inf)) == *tx) 
				tx++;
			/* c is first char that did not match */
			ungetc(c, inf);
			if (*tx) {
				/* there is something left, we failed to find "null" */
				/* output contains everything up to and including the "$"
					tx points to first character that did not match */
				register char *ttx = test;
				while (ttx < tx)
					fputc(*ttx++, outf);
			}
			else	/* found "$-Log:-$".   */
				ProcessLog(inf, outf);
		}
		else
			fputc(c, outf);
	}
}

ProcessLog(inf, outf)
	FILE *inf, *outf;
{
	time_t TM;
	struct tm *now;

	/* skip existing contents of log to end of comment */
	while ( ! feof(inf)  &&  (getc(inf) != '*'  ||  getc(inf) != '/')) {}

	/* what time is it now? */
	TM = osi_GetSecs();
	now = localtime(&TM);

	/* output new log entry */
	fprintf(outf, "Log:$ \n");
	fprintf(outf, " * Revision 1.0  %02d/%02d/%02d  %02d:%02d:%02d  %s\n",
		now->tm_year, now->tm_mon+1, now->tm_mday, 
		now->tm_hour, now->tm_min, now->tm_sec, username);
	fprintf(outf, " * Copied from /usr/andrew/lib/null\n");
	fprintf(outf, " */\n");
}

