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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/next_mach/RCS/dofix.c,v 1.4 1992/12/15 20:59:27 rr2b R6tape $";
#endif

/* 
	dofix.c - convert .o file into .do file

	Author:  John H Howard - April 9, 1987

	Modified June, 1991 by Rob Ryan for NeXT OS 2.0
 */


#include <stdio.h>

#include <andrewos.h> /* sys/file.h */


FixIt(orig, new)
char *orig, *new;
{
    char buf[1024];
    int err;
    sprintf(buf,"ld -r -x -S %s -o %s",orig, new);
    if(err=system(buf)) {
	fprintf(stderr, "dofix:command %s returned error code %d\n",buf,err);
	return;
    }
}

static char *ComputeOutputFileName (InputFileName, extension)
char *InputFileName;
char *extension;
{
    static char name[256];
    register char  *p, *q;
    char   *ext;

 /* copy the input name and look for the last '.' */

    for (p = InputFileName, q = name, ext = NULL; *p != '\0';) {
	if (*p == '/')		/* ignore period if '/' follows */
	    p++, q = name, ext = NULL;
	else
	    if ((*q++ = *p++) == '.')
		ext = q - 1;
    }
    if (ext == NULL)
	ext = q;
    *ext = '\0';

 /* overwrite the extension with new extension */

    strncat(name, extension, 255);
    if (strcmp(InputFileName, name) == 0)
	strncat(name, extension, 255);
    return name ;
}


/* main program */
main(argc, argp)
int argc;
char **argp;
{
    int infd;
    int outfd;
    int gotcha = 0;
    char *outname;
    char *EntryPointName = NULL;
    while (--argc > 0) {
      if (**++argp == '-') {
	switch (*++*argp) {
	case 'e':
	  if (*++*argp)
	    EntryPointName = *argp;
	  else {
	    EntryPointName = *++argp;
	    argc--;
	  }
	  break;
	default:
	  fprintf(stderr, "dofix:  Unknown switch -%c ignored\n", *argp);
	}
      } else {
	gotcha++;
	outname = ComputeOutputFileName(*argp, ".do");
	FixIt(*argp, outname);
      }
    }
    exit(0);
    /* Never run never would work, just a ref so InstallClassProgram is happy */
    doload(0,0,0,0,0);
}
