/*
 * pswfile.c
 *
 * (c) Copyright 1988-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#include <stdio.h>
#include "pswversion.h"

extern int outlineno;		/* line number in output file */

extern FILE *header;
extern char headid[];
extern char *ifile;
extern char *hfile;
extern char *ofile;
#ifdef __MACH__
extern char *shlibInclude;
#endif /* __MACH__ */


#ifdef _NO_PROTO
#include <varargs.h>

/* for debugging */
myprintf (va_alist)
  va_dcl
{
	printf(va_alist);
}

#else /* _NO_PROTO */

#include <stdarg.h>

/* for debugging */
myprintf (char *fmt, ...)
{
    va_list args;

#if defined(__STDC__) || defined(AIXV3) || defined(ultrix)
    va_start(args, fmt);
#else
    va_start(args);
#endif

    vprintf(fmt, args);
    va_end(args);
}

#endif /* _NO_PROTO */

static int EmitVersion(f, infname, outfname)
    FILE *f;
    char *infname, *outfname;
{
    extern char *prog;
    fprintf(f,"/* %s generated from %s\n",outfname,infname);
    fprintf(f,"   by %s %s %s\n */\n\n",PSW_OS,prog,PSW_VERSION);
    return 4;  /* number of output lines */
}

InitHFile(){
    (void) EmitVersion(header, ifile, hfile);
    fprintf(header,"#ifndef %s\n#define %s\n",headid,headid);
}

FinishHFile() {
    fprintf(header,"\n#endif /* %s */\n",headid);
    fclose(header);
}

InitOFile() {
    outlineno += EmitVersion(stdout, ifile, ofile);
#ifdef __MACH__
    if( shlibInclude ) {
 		printf("#ifdef SHLIB\n");
 		printf("#include \"%s\"\n", shlibInclude );
 		printf("#endif\n");
 		outlineno += 3;
    }
#endif /* __MACH__ */
    printf("#include %s\n", FRIENDSFILE);
    printf("#include <string.h>\n\n");
    outlineno += 3;  /* UPDATE this if you add more prolog */
    printf("#line 1 \"%s\"\n",ifile);
    outlineno++;
}

