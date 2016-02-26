/*
 * Code to read, write, update, etc. the topics list.
 * $Id: topics.c,v 1.7 92/06/02 13:22:38 ian Exp $
 */

#include <stdio.h>
#include <malloc.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include "timer_ui.h"
#include "timekeeper.h"
extern timer_baseFrame_objects	*Timer_baseFrame;

#define	MAXTOPICS	100
#define	TOPICLEN	30
static char *list[MAXTOPICS];
static int nlist = 0;
static char *curfilename = NULL;

void insertTopic();
extern char *progname;

readTopics()
{
	char *h, *p, *getenv(), buf[512];
	FILE *fp;

	if (!curfilename) {
	    if ((p=getenv("TASKFILE")) == NULL) {
		if ((h = getenv("HOME")) == NULL) {
			fprintf(stderr, "%s: No HOME in env\n", progname);
			exit(1);
		}
		p = malloc(strlen(h)+strlen(DOTFILE)+2);
		strcpy(p, h);
		strcat(p, "/");
		strcat(p, DOTFILE);
		curfilename = p;
	    }
	}

	xv_set(Timer_baseFrame->baseFrame,
		FRAME_RIGHT_FOOTER, curfilename,
		NULL);
	xv_set(Timer_baseFrame->topicList, XV_SHOW, FALSE, NULL);

	if (nlist)	/* clear out existing rows */
		xv_set(Timer_baseFrame->topicList,
			PANEL_LIST_DELETE_ROWS, 0, nlist,
			NULL);

	nlist = 0;

	if ((fp = fopen(curfilename, "r")) == NULL) {
		xv_set(Timer_baseFrame->baseFrame,
			FRAME_RIGHT_FOOTER, "Using default topics",
			XV_NULL);
		insertDefaultTopics();
	}
	else while (fgets(buf, sizeof buf, fp) != NULL)
		insertTopic(buf);

	xv_set(Timer_baseFrame->topicList, XV_SHOW, TRUE, NULL);

	fclose(fp);
}

writeTopics()
{
	static FILE *fp = NULL;
	int i;

	/* Open it only once */
	if (!fp)
		fp = fopen(curfilename, "w");
	else
		fseek(fp, 0L, 0);	/* rewind */

	if (!fp) {
		extern int errno;
		int e = errno;
		fprintf(stderr, "%s: can't save topics file ", progname);
		errno = e;
		perror(curfilename);
		exit(1);
	}

	for (i=0; i<nlist; i++) {
		fprintf(fp, "%s\n", list[i]);
	}

	fflush(fp);
	if (ferror(fp)) {
		perror("Writing topics file");
		fprintf(stderr,
			"%s: Caution: Topics file not written correctly\n",
			progname);
	}
}

insertDefaultTopics()
{
	register char **p;
	static char *defaults[] = {
		"Reading Mail",
		"Reading News",
		"Drinking Coffee",
		"Idle Chit-Chat",
		0
	};

	for (p=defaults; *p; ++p)
		insertTopic(*p);

	writeTopics();
}

char *strsave(s)
char *s;
{
	char *p = malloc(strlen(s)+1);
	if (p)
		strcpy(p, s);
	return p;
}

void
insertTopic(s)
char *s;
{
	if (nlist >= MAXTOPICS) {
		fprintf(stderr, "%s: Too many topics", progname);
		return;
	}
	if (*(s+strlen(s)-1) == '\n')
		*(s+strlen(s)-1) = '\0';
	xv_set(Timer_baseFrame->topicList,
		PANEL_LIST_INSERT,	nlist,
		PANEL_LIST_STRING,	nlist, s,
		NULL);
	list[nlist++] = strsave(s);
}

char *
getTopic(i)
int i;
{
	if (i>=0 && i < nlist)
		return list[i];
	else
		return "";
}
