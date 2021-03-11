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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/subswalk.c,v 2.19 1993/09/21 22:00:33 gk5g Exp $";
#endif

#include <stdio.h>
#include <andrewos.h>
#include <sys/stat.h>
#include <ms.h>

/* This routine walks down a directory subtree corresponding to a message
	directory, and prints appropriate subscription information on the
	stream it is passed.  The named directory should be a message
	directory, or the results will be pretty useless and weird and will
	run extremely slowly (too many stats, which in MS dirs we avoid by
	not statting files that start with "+") */



SubsTreeWalk(outfile, dirname)
FILE *outfile;
char *dirname;
{/* Driver for the real recursive procedure, below. */
    int RC;
    char Name1[MAXPATHLEN+1], Name2[MAXPATHLEN+1];

    RC = DeSymLink(dirname, Name1, 0);
    if (RC != 0) {
	if (RC == -1) RC = errno;
	AMS_RETURN_ERRCODE(RC, EIN_STAT, EVIA_SUBSTREEWALK);
    }
    if (abspath(Name1, Name2) != 0) strcpy(Name2, Name1);
    return RealSubsTreeWalk(outfile, Name2, strlen(Name2));
}


static int RealSubsTreeWalk(outfile, dirname, rootlen)
FILE *outfile;
char *dirname;
int rootlen;
{
    DIR *dirp;
    DIRENT_TYPE *dirent;
    struct stat stbuf;
    char Prefix[MAXPATHLEN+1], *s;
    char **Children;
    int PathOffset, code, ChildrenCt=0, i, j, ChildrenAllocated = 200;

    debug(1, ("SubsTreeWalk %s\n", dirname));
    Children = (char **) malloc(ChildrenAllocated * sizeof(char *));
    if (!Children) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBSTREEWALK);
    }
    if ((dirp = opendir(dirname)) == NULL) {
	free(Children);
	AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_SUBSTREEWALK);
    }

    sprintf(Prefix, "%s/", dirname);
    PathOffset = strlen(Prefix);
    for (dirent = readdir(dirp); dirent != NULL; dirent = readdir(dirp)) {
	if (*dirent->d_name == '+' || *dirent->d_name == '.') {
	    continue;
	}
	Children[ChildrenCt] = malloc(PathOffset + strlen(dirent->d_name)+2);
	if (Children[ChildrenCt] == NULL) {
	    closedir(dirp);
	    while (--ChildrenCt >= 0) free (Children[ChildrenCt]);
	    free(Children);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBSTREEWALK);
	}
	sprintf(Children[ChildrenCt], "%s%s", Prefix, dirent->d_name);
	stat(Children[ChildrenCt], &stbuf);
	if ((stbuf.st_mode & S_IFMT) != S_IFDIR) {
	    debug(4, ("Skipping non-directory file %s\n", Children[ChildrenCt]));
	    continue;
	}
	if (++ChildrenCt >= ChildrenAllocated) {
	    ChildrenAllocated += 200;
	    Children = (char **) realloc(Children, ChildrenAllocated * sizeof(char *));
	    if (!Children) {
		closedir(dirp);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SUBSTREEWALK);
	    }
	}
    }
    closedir(dirp);
    for (i=0; i<ChildrenCt; ++i) {
	for (j=0; j<i; ++j) {
	    if (strcmp(Children[i], Children[j]) < 0) {
		s = Children[i];
		Children[i] = Children[j];
		Children[j] = s;
	    }
	}
    }
    for (i=0; i<ChildrenCt; ++i) {
	debug(4, ("full name is %s\n", Children[i]));
	for (s = Children[i]+rootlen+1; *s; ++s) {
	    putc(*s == '/' ? '.' : *s, outfile);
	}
	fprintf(outfile, ":%s\n", Children[i]);
	if ((code = RealSubsTreeWalk(outfile, Children[i], rootlen)) != 0) {
	    while (ChildrenCt-- > 0) free (Children[ChildrenCt]);
	    free(Children);
	    return(code);
	}
    }
    while (ChildrenCt-- > 0) free (Children[ChildrenCt]);
    free(Children);
    return(0);
}
