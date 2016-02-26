/*
 * coffee -- network display monitor to show status of the office coffee pot
 */

#include <stdio.h>

#include "coffee.h"

/* These must be all the same size AND in the same order as "stat". */
char states[][SSIZE+1] = {	/* thou shalt not omit the null byte */
	"?!!?",
	"full",
	"half",
	"ugh!",
	"em-t"
};
int Nstates = sizeof(states)/sizeof(states[0]);

char *progname = "xcoff";

static change_icon(enum coff_stat status, char *who, long when);

void
usage()
{
	fprintf(stderr, "Usage: %s [new|half|ugh|empty]|X|[xview-options]\n",
		progname);
}

print_status()
{
	struct info *p = get_info();
	if (p != NULL)
		printf("Coffee status %s, updated by %s at %s",
			p->sstat, p->uname, ctime(&(p->mtime)));
	else
		printf("Coffee status unknown (can't read file)\n");
}

/* This is the timer-driven callback; check for changes in the file or 
 * aging of the brew, and update accordingly.
 * Your window system code MUST arrange to call this every 30 seconds or so.
 */
void
checker()
{
	static struct info *p;
	long currtime = time(0);

	p = get_info();		/* read current status */

	if (!p) {
		change_icon(UNINIT, "read fail", currtime);
		return;
	}
	switch(p->nstat) {
	case UNINIT:
		change_icon(p->nstat, p->uname, p->mtime);
		break;		/* nothing to do */
	case FULL:
		if (currtime - p->mtime > HALFLIFE)
			change_icon(HALF, "expired", currtime);
		else
			change_icon(p->nstat, p->uname, p->mtime);
		/*FALLTHROUGH*/
	case HALF:
		if (currtime - p->mtime > DEATHTIME)
			change_icon(UGHH, "expired", currtime);
		else
			change_icon(p->nstat, p->uname, p->mtime);
		break;
	case UGHH:
		change_icon(p->nstat, p->uname, p->mtime);
		break;		/* nothing to do */
	case EMPTY:
		change_icon(p->nstat, p->uname, p->mtime);
		break;          /* nothing to do */
	}
}


/* These are convenience functions; they're also callable from X, and
 * in turn call WSiconify() in X, which must check for inittedness.
 */
void
set_full()
{
	set_file(FULL);
	WSiconify();
}

void
set_half()
{
	set_file(HALF);
	WSiconify();
}

void
set_ugh()
{
	set_file(UGHH);
	WSiconify();
}

void
set_empty()
{
	set_file(EMPTY);
	WSiconify();
}

main(int argc, char **argv)
{

	progname = argv[0];

	if (argc == 1) {
		usage();
		exit(0);
	}

	/* First check for non-X usage: command line option to set status */

	if (argc > 1) {
		char *firstarg = argv[1];
		if (strcmp(firstarg, "new") == 0) {
			set_full();
			exit(0);
		}
		if (strcmp(firstarg, "full") == 0) {
			set_full();
			exit(0);
		}
		if (strcmp(firstarg, "half") == 0) {
			set_half();
			exit(0);
		}
		if (strcmp(firstarg, "ugh") == 0) {
			set_ugh();
			exit(0);
		}
		if (strcmp(firstarg, "empty") == 0) {
			set_empty();
			exit(0);
		}
		if (strcmp(firstarg, "status") == 0) {
			print_status();
			exit(0);
		}
	}

	/* Else call an X routine that watches the file and keeps
	 * the requisite icon on the screen.
	 */
	WSinit(&argc, argv);
	WSrun();
	/*NOTREACHED*/

	/* The X routine never terminates unless the X server is shutdown */
}

static
change_icon(enum coff_stat status, char *who, long when)
{
	static enum coff_stat old_status = UNINIT;
	static char old_who[128] = "nOnEsEnSe";
	static long old_when = 0;

	if (status != old_status ||
		strcmp(who, old_who) != 0 ||
		when != old_when)
		WSset_icon(old_status=status,
			strncpy(old_who, who, sizeof old_who-1),
			old_when = when);
}
