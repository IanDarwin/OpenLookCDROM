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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/dec_mach/RCS/dofix.c,v 1.3 1992/12/15 20:58:42 rr2b R6tape $";
#endif

/* 
	dofix.c - convert .o file into .do file

	Author:  Zalman Stern July 1989
 */

/* This version of dofix is much more simpler than ones for other machine types
 * since MIPS' ld does what we want...
 */

#include <stdio.h> /* For NULL in absence of stddef.h */

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

main(argc, argp)
int argc;
char **argp;
{
    char *InputFileName;
    char *OutputFileName;
    char *EntryPointName = NULL;
    int gotcha = 0;
    char CommandBuffer[1024];

    while (--argc > 0) {
	if (**++argp == '-') {
	    switch (*++*argp) {
	    case 'd':
		break;
	    case 'e':
		if (*++*argp)
		    EntryPointName = *argp;
		else {
		    EntryPointName = *++argp;
		    argc--;
		}
                if (EntryPointName[0] == '_')
                    EntryPointName++;
		break;
	    default:
		fprintf(stderr, "dofix:  Unknown switch -%c ignored\n", *argp);
	    }
	}
	else {
	    gotcha++;
            InputFileName = *argp;
	    OutputFileName = ComputeOutputFileName(InputFileName, ".do");
        }
    }
    if (gotcha == 0) {
        InputFileName = "-";
        InputFileName = "-";
    }

    sprintf(CommandBuffer, "ld -x -r -e %s %s -o %s", EntryPointName,
            InputFileName, OutputFileName);
    system(CommandBuffer);

    exit(0);
}
