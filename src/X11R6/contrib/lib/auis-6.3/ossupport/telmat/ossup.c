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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ossupport/telmat/RCS/ossup.c,v 1.1 1993/11/16 20:16:56 gk5g Exp $";
#endif

#include <stdio.h>
#include <andrewos.h>

#if defined(RENAME_IS_BROKEN)
/* This is due to a bug in rename() that limits it to 13 character filenames!
*/
int
sco_rename(old,new)
char *old,*new;
{
	int cc;
	unlink(new);
	cc = link(old,new);
	if (cc>=0) {
	  cc = unlink(old);
	  if (cc<0)
		unlink(new);
	}
	return cc;
}
#endif /* RENAME_IS_BROKEN */


#include <dirent.h>
#include <sys/stat.h>


#undef DIRSIZ
#define DIRSIZ(dp)  \
	((sizeof (struct dirent) - sizeof ((dp)->d_name) + \
	(strlen((dp)->d_name)+1) + 3) & ~3)


scandir(dirname, namelist, select, dcomp)
	char *dirname;
	struct dirent *(*namelist[]);
	int (*select)(), (*dcomp)();
{
	register struct dirent *d, *p, **names;
	register int nitems;
	register char *cp1, *cp2;
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
		p = (struct dirent *)malloc(DIRSIZ(d));
		if (p == NULL)
			return(-1);
		p->d_ino = d->d_ino;
		p->d_reclen = d->d_reclen;
		for (cp1 = p->d_name, cp2 = d->d_name; *cp1++ = *cp2++; );
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
	struct dirent **d1, **d2;
{
	return(strcmp((*d1)->d_name, (*d2)->d_name));
}

#include <signal.h>

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

#include <utime.h>

utimes(file, tvp)
char *file;
struct timeval *tvp;
{
	struct utimbuf times;
	times.actime = tvp[0].tv_sec;
	times.modtime = tvp[1].tv_sec;
	return utime(file,&times);
}

sco_gettimeofday(tp, tzp)
struct timeval *tp;
struct timezone *tzp;
{
	if (tp == NULL)
		return -1;
	tp->tv_sec = time(0);
	tp->tv_usec = 0;
}
