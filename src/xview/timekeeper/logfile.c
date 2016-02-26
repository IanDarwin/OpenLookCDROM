/*
 * Code to open and write, etc. the timeRecorder program.
 * $Id: logfile.c,v 1.6 92/06/02 13:24:07 ian Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <malloc.h>
#if	0
#include <xview/xview.h>
#include <xview/panel.h>
#include "timer_ui.h"
extern timer_baseFrame_objects	*Timer_baseFrame;
#endif

static char *curlogfile;
static char *DOTFILE = ".timelog";
static FILE *fp;
extern char *progname;

openLog()
{
	char *h, *getenv();
	static char *p;

	if ((h = getenv("HOME")) == NULL) {
		fprintf(stderr, "%s: No HOME in environment!\n", progname);
		exit(1);
	}
	if (!p) {
		p = malloc(strlen(h)+strlen(DOTFILE)+2);
		strcpy(p, h);
		strcat(p, "/");
		strcat(p, DOTFILE);

		curlogfile = p;
	}

	if ((fp = fopen(p, "a")) == NULL) {
		extern int errno;
		int e = errno;
		fprintf(stderr,
			"%s: can't append to time log file ",
			progname);
		errno = e;
		perror(curlogfile);
		exit(1);
	}
}

closeLog()
{
	(void) fclose(fp);
}

/* Timestamp and Write an action (start, stop) and
 * a topic to the logfile.
 */
writeLog(a, t)
char *a, *t;
{
	time_t tl;
	struct tm *lt, *localtime();

	openLog();
	time(&tl);
	lt = localtime(&tl);
	fprintf(fp, "%4d/%02d/%02d\t%02d:%02d\t%s\t%s\n", 
		lt->tm_year+1900, lt->tm_mon+1, lt->tm_mday,
		lt->tm_hour, lt->tm_min, a, t);
	closeLog();
}

