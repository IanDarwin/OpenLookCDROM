#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/systeminfo.h>
#ifndef RUSAGE_SELF
#include <sys/procfs.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>

int
osi_ExclusiveLockNoBlock( fd )
int  fd;
{
    flock_t wL;

    wL.l_type = F_WRLCK;
    wL.l_whence = SEEK_SET;
    wL.l_start = wL.l_len = 0;
    return(fcntl(fd, F_SETLK, &wL));
}

int
osi_UnLock( fd )
int  fd;
{
    flock_t wL;

    wL.l_type = F_UNLCK;
    wL.l_whence = SEEK_SET;
    wL.l_start = wL.l_len = 0;
    return(fcntl(fd, F_SETLK, &wL));
}


sigset_t
sigblock(new)
sigset_t new;
{
    sigset_t s_new, s_old;
    s_new = new;
    sigprocmask(SIG_BLOCK,&s_new,&s_old);
    return s_old;
}

sigsetmask(new)
sigset_t new;
{
    sigset_t s_new = new;

    sigprocmask(SIG_SETMASK,&s_new,0);
}

char *
getwd(path)
char *path;
{
    return(getcwd(path, MAXPATHLEN));
}

int sigvec(Signal, Invec, Outvec)
int Signal;
struct sigaction *Invec, *Outvec; {
    return((int)sigaction(Signal, Invec, Outvec));
}

void
bcopy(s1, s2, len)
char *s1, *s2;
int len;
{
    memmove(s2, s1, len);
}

void
bzero(sp, len)
char *sp;
int len;
{
    memset(sp, 0, len);
}

int
bcmp(s1, s2, len)
char *s1, *s2;
int len;
{
    return(memcmp(s1, s2, len));
}

int
setreuid(ruid, euid)
int ruid, euid;
{
    setuid(ruid);
}

long
random()
{
    return(rand());
}

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * Scan the directory dirname calling select to make a list of selected
 * directory entries then sort using qsort and compare routine dcomp.
 * Returns the number of entries and a pointer to a list of pointers to
 * struct dirent (through namelist). Returns -1 if there were any errors.
 */

scandir(dirname, namelist, select, dcomp)
	char *dirname;
	struct dirent ***namelist;
	int (*select)(), (*dcomp)();
{
	register struct dirent *d, *p, **names;
	register int nitems;
	struct stat stb;
	long arraysz;
	DIR *dirp;

	if ((dirp = opendir(dirname)) == NULL)
		return(-1);
	if (fstat(dirp->dd_fd, &stb) < 0)
		return(-1);

	/*
	 * estimate the array size by taking the size of the directory file
	 * and dividing it by a multiple of the minimum size entry. 
	 */
	arraysz = (stb.st_size / 24);
	names = (struct dirent **)malloc(arraysz * sizeof(struct dirent *));
	if (names == NULL)
		return(-1);

	nitems = 0;
	while ((d = readdir(dirp)) != NULL) {
		if (select != NULL && !(*select)(d))
			continue;	/* just selected names */
		/*
		 * Make a minimum size copy of the data
		 */
		p = (struct dirent *)malloc(d->d_reclen);
		if (p == NULL)
			return(-1);
		p->d_ino = d->d_ino;
		p->d_reclen = d->d_reclen;
		p->d_off = d->d_off;
		strcpy(p->d_name, d->d_name);
		/*
		 * Check to make sure the array has space left and
		 * realloc the maximum size.
		 */
		if (++nitems >= arraysz) {
			if (fstat(dirp->dd_fd, &stb) < 0)
				return(-1);	/* just might have grown */
			arraysz = stb.st_size / 12;
			names = (struct dirent **)realloc((char *)names,
				arraysz * sizeof(struct dirent *));
			if (names == NULL)
				return(-1);
		}
		names[nitems-1] = p;
	}
	closedir(dirp);
	if (nitems && dcomp != NULL)
		qsort(names, nitems, sizeof(struct dirent *), dcomp);
	*namelist = names;
	return(nitems);
}

/*
 * Alphabetic order comparison routine for those who want it.
 */
alphasort(d1, d2)
	void *d1, *d2;
{
	return(strcmp((*(struct dirent **)d1)->d_name,
	    (*(struct dirent **)d2)->d_name));
}

long
gethostid() {

    char buf[128];

    if (sysinfo(SI_HW_SERIAL, buf, 128) == -1) {
	return((long) -1);
    }
    return(strtoul(buf,NULL,0));
}

int
getdtablesize() {
    return(sysconf(_SC_OPEN_MAX));
}

int
setlinebuf(f)
    FILE *f;
{
    return(setvbuf(f, NULL, _IOLBF, BUFSIZ));
}

char *
index(s, c)
char *s;
char c;
{
    return(strchr(s, (int) c));
}

char *
rindex(s, c)
char *s;
char c;
{
    return(strrchr(s, (int) c));
}
