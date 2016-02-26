/*
 * Routines that write and read the status file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <time.h>
#include <sys/stat.h>
#include "coffee.h"

enum coff_stat status;

void
set_file(enum coff_stat st)
{
	int fd;

#if	0
	printf("set file status to %d\n", st);
#endif
	status = st;

	if (unlink(STAT_FILE) < 0) {
		fprintf(stderr, "warning: unlink ");
		perror(STAT_FILE);
		/* Don't exit here, may just be "no such file", try creat it */
	}
	if ((fd = creat(STAT_FILE, 0644)) < 0) {
		fprintf(stderr, "creat ");
		perror(STAT_FILE);
		exit(1);
	}
	if (write(fd, states[(int)st], SSIZE) != SSIZE) {
		fprintf(stderr, "write ");
		perror(STAT_FILE);
		exit(1);
	}
	(void) close(fd);
}


struct info *
get_info()
{
	static struct info is, *ip = &is;
	struct stat statbuf;
	struct passwd *up, *getpwuid();
	static char buf[SSIZE];
	int i, fd;

	fd = open(STAT_FILE, 0);
	if (fd<0) {
		fprintf(stderr, "%s: Can't read ", progname);
		perror(STAT_FILE);
		exit(1);
	}
	if (lseek(fd, 0L, SEEK_SET) <0) {
		perror("Seek/rewind failed");
	}

	if (fstat(fd, &statbuf) < 0) {
		fprintf(stderr, "stat ");
		perror(STAT_FILE);
		return NULL;
	}

	if ((up = getpwuid(statbuf.st_uid)) == NULL) {
		printf("Can't getpwuid(%d)\n", statbuf.st_uid);
		ip->uname = "???";
	}
		else ip->uname = up->pw_name;

	ip->mtime = statbuf.st_mtime;

	if (read(fd, &buf, sizeof(buf)) != sizeof(buf)) {
		fprintf(stderr, "Read from ");
		perror(STAT_FILE);
		ip->sstat = "???";
	}
	else ip->sstat = buf;

	ip->nstat = UNINIT;
	for (i=0; i<NSTATES; i++)
		if(strcmp(buf, states[i]) == 0)
			ip->nstat = (enum coff_stat)i;

#if	0
	printf("Sstat %s nstat %d who %s when %s",is.sstat,is.nstat,is.uname,
	ctime(&is.mtime));
#endif
	(void) close(fd);
	return ip;
}
