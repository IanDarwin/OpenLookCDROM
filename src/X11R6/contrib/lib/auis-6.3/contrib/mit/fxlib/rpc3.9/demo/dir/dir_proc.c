/* @(#)dir_proc.c	1.3 87/11/16 3.9 RPCSRC */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/rpc3.9/demo/dir/RCS/dir_proc.c,v 1.3 1992/12/15 21:53:21 rr2b R6tape $";
#endif

/*
 * dir_proc.c: remote readdir implementation
 */
#include <rpc/rpc.h>
#include <sys/dir.h>
#include "dir.h"

extern int errno;
extern char *malloc();
extern char *strcpy();

readdir_res *
readdir_1(dirname)
	nametype *dirname;
{
	DIR *dirp;
	struct direct *d;
	namelist nl;
	namelist *nlp;
	static readdir_res res; /* must be static! */
	
	/*
	 * Open directory
	 */
	dirp = opendir(*dirname);
	if (dirp == NULL) {
		res.errno = errno;
		return (&res);
	}

	/*
	 * Free previous result
	 */
	xdr_free(xdr_readdir_res, &res);

	/*
	 * Collect directory entries
	 */
	nlp = &res.readdir_res_u.list;
	while (d = readdir(dirp)) {
		nl = *nlp = (namenode *) malloc(sizeof(namenode));
		nl->name = malloc(strlen(d->d_name)+1);
		strcpy(nl->name, d->d_name);
		nlp = &nl->next;
	}
	*nlp = NULL;

	/*
	 * Return the result
	 */
	res.errno = 0;
	closedir(dirp);
	return (&res);
}
