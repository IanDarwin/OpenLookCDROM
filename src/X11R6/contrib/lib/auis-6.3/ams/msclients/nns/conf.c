/* Methods for the configuration data type for the Netnews snarfer
 * John G. Myers
 */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/nns/RCS/conf.c,v 1.8 1993/05/05 19:49:43 susan Exp $";
#endif


#include <big.h>
#include <ctype.h>

/* The various mailbox directories */
static char RootDir[MAXPATHLEN + 1] = "/usr/net";
char ReadyBox[MAXPATHLEN + 1];
char DelayedDir[MAXPATHLEN + 1];
char FailedDir[MAXPATHLEN + 1];
char ControlDir[MAXPATHLEN + 1];
char HoldDir[MAXPATHLEN + 1];

/* What the peak times are */ 
static int IsPeakHour[24];
static int IsPeakDay[7];

/* Nonzero if currently in peak-time mode */
static int IsPeak;

/*
 * Mappings between newsgroup hierarchies and bboard (sub-)roots.
 * The zero-th entry in BBmapping[] is the default mapping
 */
struct BBmapping {
    char *name;
    int namelen;
    char *path;			/* NULL for "drop this group" */
};
#define MAXMAPPING 256
static struct BBmapping BBmapping[MAXMAPPING];
int numBBmapping = 0;

/*
 * Group hierchy lists
 */
struct group {
    char *name;
    int namelen;
};
/* Hierarchies we receive */
static struct group *validGroups;
static int numValidGroups;
/* Hierarchies that don't get filed during peak times */
static struct group *delayedGroups;
static int numDelayedGroups;

static struct group *parseGroupList();

/*
 * Initialize this module by parsing the config file named in "fname"
 *
 * Commands that can be in the file are:
 *
 * # Comment lines, which start with the '#' character.
 *
 * dir /usr/net
 * # Specifies the directory that contains the ReadyBox, etc. to file
 * # messages from.
 *
 * groups comp,rec,news,alt,gnu,cmu.misc
 * # Specifies the newsgroup hierarchies that get filed.
 *
 * delayedgroups alt,talk
 * # Specifies the newsgroup hierarchies that get delayed during peak times.
 * 
 * peakhours 9-7pm,20 M-F
 * # Specifies the times and days that the delayedgroups get delayed.
 * # The above example means 9:00AM through 6:59PM and 8:00PM through 8:59PM
 * # on weekdays.  Days are:
 * #         Sunday        U or Su
 * #         Monday        M
 * #         Tuesday       T or Tu
 * #         Wednesday     W
 * #         Thursday      R or Th
 * #         Friday        F
 * #         Saturday      S or Sa
 *
 * bbroot default /afs/andrew.cmu.edu/usr0/netbb/.MESSAGES/netnews
 * bbroot cmu.ext drop
 * bbroot alt.sex.pictures /afs/andrew.cmu.edu/usr0/netbb/.MESSAGES/netnews/alt/binaries/pictures/erotica
 * # Specifies where newsgroups get filed.  The first 'bbroot' command
 * # must specify the "default" root, which is used if no other bbroot command
 * # matches.  The other bbroot commands are searched in order to find a
 * # root to put a given folder in.  The special root "drop" means to
 * # not file that newsgroup.
 */
void ConfInit(fname)
char *fname;
{
    FILE *config;
    char buf[1024];
    char *line, *p;
    int i, start, hour, day;

    config = fopen(fname, "r");
    if (!config) {
	fprintf(stderr, "nns: Fatal error!  Couldn't read config file ");
	perror(fname);
	exit(CONFIGERR);
    }

    while (fgets(buf, sizeof(buf), config)) {
	buf[strlen(buf)-1] = '\0'; /* Chop off newline */

	for (line = buf; *line && isspace(*line); line++);
	if (*line == '#' || *line == '\0') {
	    continue;
	}
	else if (!strncmp(line, "dir", 3) && isspace(line[3])) {
	    for (line += 3; *line && isspace(*line); line++);
	    for (p = line; *p && !isspace(*p); p++);
	    *p = '\0';
	    if (!*line) {
		fprintf(stderr, "nns: Fatal error!  'dir' command in config file needs an argument\n");
		exit(CONFIGERR);
	    }
	    strcpy(RootDir, line);
	}
	else if (!strncmp(line, "groups", 6) && isspace(line[6])) {
	    if (numValidGroups) {
		fprintf(stderr, "nns: Fatal error!  multiple 'groups' commands in config file\n");
		exit(CONFIGERR);
	    }

	    for (line += 6; *line && isspace(*line); line++);
	    for (p = line; *p && !isspace(*p); p++);
	    *p = '\0';
	    if (!*line) {
		fprintf(stderr, "nns: Fatal error!  'groups' command in config file needs an argument\n");
		exit(CONFIGERR);
	    }
	    validGroups = parseGroupList(line, &numValidGroups);
	}
	else if (!strncmp(line, "delayedgroups", 13) && isspace(line[13])) {
	    if (numDelayedGroups) {
		fprintf(stderr, "nns: Fatal error!  multiple 'delayedgroups' commands in config file\n");
		exit(CONFIGERR);
	    }

	    for (line += 13; *line && isspace(*line); line++);
	    for (p = line; *p && !isspace(*p); p++);
	    *p = '\0';
	    if (!*line) {
		fprintf(stderr, "nns: Fatal error!  'delayedgroups' command in config file needs an argument\n");
		exit(CONFIGERR);
	    }
	    delayedGroups = parseGroupList(line, &numDelayedGroups);
	}
	else if (!strncmp(line, "peakhours", 9) && isspace(line[9])) {
	    for (line += 9; *line && isspace(*line); line++);
	    if (!*line) {
		fprintf(stderr, "nns: Fatal error!  'peakhours' command in config file needs an argument\n");
		exit(CONFIGERR);
	    }

	    start = -1;
	    while (*line && !isspace(*line)) {
		if (!isdigit(*line)) {
		    fprintf(stderr, "nns: Fatal error!  'peakhours' command in config file contains unparseable time range\n");
		    exit(CONFIGERR);
		}
			
		for (hour=0; *line && isdigit(*line); line++) {
		    hour = hour * 10 + *line - '0';
		}
		if (*line == 'p' || *line == 'P') {
		    if (hour < 12) hour += 12;
		    line++;
		    if (*line == 'm' || *line == 'M') line++;
		}
		else if (*line == 'a' || *line == 'A') {
		    if (hour == 12) hour = 0;
		    line++;
		    if (*line == 'm' || *line == 'M') line++;
		}
		if (hour > 23) hour = 23;
		if (start >= 0 && hour == 0) hour = 24;
		if (start >= 0 && start < hour) {
		    for (i=start; i < hour; i++) {
			IsPeakHour[i] = 1;
		    }
		    start = -1;
		}
		else {
		    IsPeakHour[hour] = 1;
		}
		if (*line == '-') {
		    start = hour;
		    line++;
		}
		if (*line == ',') line++;
	    }

	    while (*line && isspace(*line)) line++;

	    if (!*line) {
		for (i=0; i<7; i++) IsPeakDay[i] = 1;
	    }
	    start = -1;
	    while (*line && !isspace(*line)) {
		switch(*line++) {
		case 'U':
		case 'u':
		    day = 0;
		    break;

		case 'M':
		case 'm':
		    day = 1;
		    break;

		case 'T':
		case 't':
		    if (*line == 'h' || *line == 'H') {
			day = 4;
			line++;
		    }
		    else if (*line == 'u' || *line == 'U') {
			day = 2;
			line++;
		    }
		    else day = 2;
		    break;

		case 'W':
		case 'w':
		    day = 3;
		    break;

		case 'R':
		case 'r':
		    day = 4;
		    break;

		case 'F':
		case 'f':
		    day = 5;

		case 'S':
		case 's':
		    if (*line == 'u' || *line == 'U') {
			day = 0;
			line++;
		    }
		    else if (*line == 'a' || *line == 'A') {
			day = 6;
			line++;
		    }
		    else day = 6;
		    break;

		default:
		    fprintf(stderr, "nns: Fatal error!  'peakhours' command in config file contains unparseable day-of-week range\n");
		    break;
		}
		    
		if (start >= 0 && start <= day ) {
		    for (i=start; i < day; i++) {
			IsPeakDay[i] = 1;
		    }
		    start = -1;
		}
		else {
		    IsPeakDay[day] = 1;
		}
		if (*line == '-') {
		    start = day;
		    line++;
		}
		if (*line == ',') line++;
	    }
	}
	else if (!strncmp(line, "bbroot", 6) && isspace(line[6])) {
	    for (line += 6; *line && isspace(*line); line++);
	    for (p = line; *p && !isspace(*p); p++);
	    if (!*line || !*p) {
		fprintf(stderr, "nns: Fatal error!  'bbroot' command in config file needs two arguments\n");
		exit(CONFIGERR);
	    }
	    *p = '\0';
		
	    if (numBBmapping == MAXMAPPING) {
		fprintf(stderr, "nns: Fatal error!  Too many 'bbroot' commands in config file\n");
		exit(CONFIGERR);
	    }

	    if (!strcmp(line, "default")) {
		if (numBBmapping != 0) {
		    fprintf(stderr, "nns: Fatal error!  Multiple 'bbroot default' commands in config file\n");
		    exit(CONFIGERR);
		}
		line = "";
	    }
	    else if (numBBmapping == 0) {
		fprintf(stderr, "nns: Fatal error!  'bbroot default' must be first 'bbroot' command in config file\n");
		exit(CONFIGERR);
	    }

	    BBmapping[numBBmapping].name = xmalloc(strlen(line)+1);
	    strcpy(BBmapping[numBBmapping].name, line);
	    BBmapping[numBBmapping].namelen = strlen(line);

	    for (line = p+1; *line && isspace(*line); line++);
	    for (p = line; *p && !isspace(*p); p++);
	    *p = '\0';
	    if (!*line) {
		fprintf(stderr, "nns: Fatal error!  'bbroot' command in config file needs two arguments\n");
		exit(CONFIGERR);
	    }
		    
	    if (!strcmp(line, "drop")) {
		BBmapping[numBBmapping].path = 0;
	    }
	    else {
		BBmapping[numBBmapping].path = xmalloc(strlen(line)+2);
		strcpy(BBmapping[numBBmapping].path, line);
		if (line[strlen(line)-1] != '/') {
		    strcat(BBmapping[numBBmapping].path, "/");
		}
	    }
	    numBBmapping++;
	}
	else {
	    fprintf(stderr, "nns: Fatal error!  unrecognized command '%s' in configuration file\n", line);
	    exit(CONFIGERR);
	}
    }
    fclose(config);

    if (!numBBmapping) {
	fprintf(stderr, "nns: Fatal error!  must have a 'bbroot default' command in configuration file\n");
	exit(CONFIGERR);
    }

    if (RootDir[strlen(RootDir)-1] != '/') strcat(RootDir, "/");

    strcpy(ReadyBox, RootDir);
    strcat(ReadyBox, "ReadyBox");

    strcpy(DelayedDir, RootDir);
    strcat(DelayedDir, "Delayed");

    strcpy(FailedDir, RootDir);
    strcat(FailedDir, "Failed");

    strcpy(ControlDir, RootDir);
    strcat(ControlDir, "Control");

    strcpy(HoldDir, RootDir);
    strcat(HoldDir, "Hold");

    ConfCheckPeakTime();
}

/*
 * Check to see whether or not it is currently a peak time and
 * set the peak-time mode state apropriately.  Returns nonzero
 * iff now in peak-time mode.
 */
int ConfCheckPeakTime()
{
    long now;
    struct tm *tm;
    char buf[MAXPATHLEN + 1];

    time(&now);
    tm = localtime(&now);

    IsPeak = (IsPeakHour[tm->tm_hour] && IsPeakDay[tm->tm_wday]);

    return IsPeak;

}

/*
 * Return nonzero if currently in peak-time mode.
 */
int ConfIsPeakTime()
{
    return IsPeak;
}

/*
 * Returns a pointer to a static buffer containing the directory
 * to file the given newsgroup in, or NULL if the newsgroup should
 * not be filed.  The integer pointed to by 'delayp' is set to nonzero
 * iff the group should be delayed, otherwise it is set to zero.
 * The static buffer is overwritten on the next call.
 */
char *ConfDirForGroup(newsgroup, delayp)
char *newsgroup;
int *delayp;
{
    int Ix;
    static char *p, path[MAXPATHLEN + 1];

    *delayp = 0;

    /* First check to see if group is accepted.  LOSTDIR is always accepted */
    if (numValidGroups) {
	for (Ix = 0; Ix < numValidGroups; Ix++) {
	    if (!strncmp(newsgroup, validGroups[Ix].name, validGroups[Ix].namelen)) {
		if (newsgroup[validGroups[Ix].namelen] == '.'
		    || newsgroup[validGroups[Ix].namelen] == '\0') break;
	    }
	}
	if (Ix == numValidGroups && strcmp(newsgroup, LOSTDIR)) {
	    return NULL;
	}
    }

    /* Next, check to see if group is delayed */
    if (IsPeak && numDelayedGroups) {
	for (Ix = 0; Ix < numDelayedGroups; Ix++) {
	    if (!strncmp(newsgroup, delayedGroups[Ix].name, delayedGroups[Ix].namelen)) {
		if (newsgroup[delayedGroups[Ix].namelen] == '.'
		    || newsgroup[delayedGroups[Ix].namelen] == '\0') break;
	    }
	}
	if (Ix < numDelayedGroups) {
	    *delayp = 1;
	}
    }

    /* Now, find out where this baby goes
     * Zero'th mapping is the default mapping, so we use that last.
     */
    for (Ix = 1; Ix < numBBmapping; Ix++) {
	if (!strncmp(newsgroup, BBmapping[Ix].name, BBmapping[Ix].namelen)) {
	    if (newsgroup[BBmapping[Ix].namelen] == '.'
		|| newsgroup[BBmapping[Ix].namelen] == '\0') break;
	}
    }
    if (Ix == numBBmapping) Ix = 0;

    if (!BBmapping[Ix].path) {
	return NULL;
    }

    strcpy(path, BBmapping[Ix].path);
    p = path + strlen(path);
    if (Ix == 0) {
	strcpy(p, newsgroup);
    }
    else if (newsgroup[BBmapping[Ix].namelen] == '.') {
	strcpy(p, newsgroup + BBmapping[Ix].namelen + 1);
    }
    else {
	p[-1] = '\0';		/* Nuke trailing slash */
    }
    for (;*p;p++) if (*p == '.') *p = '/';
    return path;
}

/*
 * Parse a comma-separated list of groups in 'str' into a newly allocated
 * 'struct group'.  Returns a pointer to the new group list and sets the
 * integer pointed to by 'nump' to the number of groups in the list.
 */
static struct group *parseGroupList(str, nump)
char *str;
int *nump;
{
    int num, Ix;
    char *p, *q;
    struct group *groups;
    
    p = xmalloc(strlen(str)+1);
    strcpy(p, str);

    num = 0;
    while (str != NULL) {
	q = index(str, ',');
	++num;
	str = q ? q+1 : NULL;
    }

    groups = (struct group *) xmalloc(num * sizeof(struct group));

    Ix = 0;
    while (p != NULL) {
	q = index(p, ',');
	if (q != NULL) *q++ = '\0';
	groups[Ix].name = p;
	groups[Ix].namelen = strlen(p);
	++Ix;
	p = q;
    }

    *nump = num;
    return groups;
}

