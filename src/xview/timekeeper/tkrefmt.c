/*
 * Convert timelogs from 
 * 1992/05/26 15:31 Start	RECORDING
 * to
 * 701641917	Start	RECORDING
 * (the number is the corresponding UNIX time_t).
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>

#define STREQ(s,t) (*(s)==*(t) && strcmp(s, t)==0)

char *progname;
int debug;
FILE *efopen();
struct stat statbuf;

void
warning(s1, s2)
char *s1;
char *s2;
{
	char *cmdname;
	extern int errno, sys_nerr;
	extern char *sys_errlist[];
	extern char *progname;
	extern char *getenv();

	cmdname = getenv("CMDNAME");
	if (cmdname != NULL && *cmdname != '\0')
		(void) fprintf(stderr, "%s:", cmdname);	/* No space after :. */
	if (progname != NULL)
		(void) fprintf(stderr, "%s: ", progname);
	(void) fprintf(stderr, s1, s2);
	if (errno > 0)
		if (errno < sys_nerr)
			(void) fprintf(stderr, " (%s)", sys_errlist[errno]);
		else
			(void) fprintf(stderr, " (errno=%d)", errno);
	(void) fputc('\n', stderr);
	errno = 0;
}

void
error(s1, s2)
char *s1;
char *s2;
{
	warning(s1, s2);
	exit(1);
}

FILE*
efopen(fn, mode)
char *fn, *mode;
{
	FILE *f = fopen(fn,mode);
	if (!f)
		error("Can't open file", fn);
	return f;
}


/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;
	FILE *in;
	extern int optind;
	extern char *optarg;
	void process();

	progname = argv[0];

	while ((c = getopt(argc, argv, "d")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr, "usage: %s xxx [file] ...\n", progname);
		exit(2);
	}

	if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			if (strcmp(argv[optind], "-") == 0)
				process(stdin, "-");
			else {
				in = efopen(argv[optind], "r");
				if (fstat(fileno(in), &statbuf) != 0)
					error("can't fstat %s", argv[optind]);
				if ((statbuf.st_mode & S_IFMT)==S_IFDIR)
					error("%s is a directory!",
								argv[optind]);
				process(in, argv[optind]);
				(void) fclose(in);
			}
	exit(0);
}

/*
 * process - process input file
 */
void
process(in, inname)
FILE *in;
char *inname;
{
	char line[256];
	void doline();

	while (fgets(line, sizeof line, in) != NULL)
		doline(line);
}

void
doline(s)
char *s;
{
	char *sdate, *stime, *rest;
	struct tm tm, *tmp;
	int yy,mm,dd,hour,minute;
	time_t t;

	sdate = strtok(s, " \t");
	stime = strtok(0, " \t");
	rest = s + strlen(sdate) + strlen(stime) +2;

	sscanf(sdate, "19%d/%d/%d", &yy, &mm, &dd);
	sscanf(stime, "%d:%d", &hour, &minute);

	if (debug)
		printf("yy %d mm %d dd %d, hour %d min %d, rest %s\n",
		yy, mm, dd, hour, minute, rest);

	t = time(0);
	tmp = localtime(&t);
	tm = *tmp;	/* struct copy to init. all fields in struct. */
	tm.tm_wday = tm.tm_yday = tm.tm_sec = 0;	/* clear some */

	tm.tm_year = yy;
	tm.tm_mon = mm - 1;
	tm.tm_mday = dd;
	tm.tm_hour = hour;
	tm.tm_min = minute;
	if (debug)
		printf("tm now contains %s", asctime(&tm));
	t = timelocal(&tm);
	if (debug)			/* no newline, ctime provides it. */
		printf("Time equiv: %s", ctime(&t));
	printf("%ld\t%s", t, rest);	/* no newline, fgets leaves it in */
	return;
}
