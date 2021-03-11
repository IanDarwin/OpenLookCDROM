/* ********************************************************************** *\
 *	   Copyright IBM Corporation 1988,1989 - All Rights Reserved	  *
 *	  For full copyright information see:'andrew/config/COPYRITE'     *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sco_i386/RCS/dofix.c,v 1.3 1992/12/15 20:59:27 rr2b R6tape $";
#endif

/*
	dofix.c - convert .o file into .do file

	Author:  John H Howard - April 9, 1987
 */


#include <stdio.h>
#include <andrewos.h> /* sys/file.h */
#include <a.out.h>
#include <setjmp.h>
#include <doload.h>

#include <../common/safe.h>

char *doload_getname();

/* set entry point */

FixEntryPoint(e, EntryPointName)
register struct doload_environment *e;
char *EntryPointName;
{
    register SYMENT *sp;
    register SYMENT *sbound;
    
    if (EntryPointName == NULL || *EntryPointName == NULL)
	return;

    /* the following compensates for the missing _ at the beginning of AIX routine names */
    if (*EntryPointName == '_')
	EntryPointName++;
    sp = sbound = e->symtab;
    (char *)sbound += e->filehdr.f_nsyms * SYMESZ;
    for (; sp < sbound; (char *)sp += SYMESZ) {
	if (sp->n_scnum != N_UNDEF
	    && (sp->n_sclass == C_EXT)
	    && strcmp(EntryPointName, doload_getname(e, sp)) == 0 ) {
	    e->aouthdr.entry = sp->n_value;
	    return;
	}

    } /* end of loop */
    printf("***entry point %s undefined\n", EntryPointName);
    fflush(stdout);
    e->problems += 1;
    return;
}

/* write new symbol table */

WriteNewSym(e, outFD)
register struct doload_environment *e;
int outFD;
{
    register int i;
    register char *newcp;
    long newstringsize;
    char *newstrings = NULL;
    SYMENT *sp;

    /* allocate new string table */

    for (newstringsize = sizeof newstringsize, i = 0, sp = e->newsym;
	i < e->newsymcount; i += 1, (char *)sp += SYMESZ) {
	if (sp->n_zeroes == 0)
	    newstringsize += strlen(e->stringtab + sp->n_offset) + 1;
    }
    if (doload_trace > 0)
	printf( " new symbol count %d, new string size %d\n",
		e->newsymcount, newstringsize ) ;
    newcp = newstrings = safe_malloc(e, newstringsize);
    *(long *)newcp = newstringsize;
    newcp += sizeof newstringsize;

    /* make a new string table */

    for (i = 0, sp = e->newsym; i < e->newsymcount; i += 1, (char *)sp += SYMESZ) {
	register char *oldcp ;
	register int n ;

	if (sp->n_zeroes == 0) {
	    oldcp = e->stringtab + sp->n_offset ;
	    n = strlen(oldcp) + 1;
	    bcopy(oldcp, newcp, n);
	    sp->n_offset = newcp - newstrings;
	    newcp += n;
	}
    }

    /* write symbols and strings */

    if (lseek(outFD, e->filehdr.f_symptr, 0) < 0) doload_punt(e, "symptr lseek failed");
    safe_write( e, outFD, (char *)e->newsym, e->newsymcount * SYMESZ);
    safe_write(e, outFD, newstrings, newstringsize);

    /* clean up */

    safe_free(newstrings);

    return;
}

/* read, fix, and write out module */

FixIt(inFD, outFD, EntryPointName)
int inFD;			/* open fd for .o file */
int outFD;			/* open fd for .do file */
char *EntryPointName;		/* entry point name */
{
    struct doload_environment E;
    register struct doload_environment *e;
    unsigned long n;	/* number of relocation items */
    long rcount;	/* number of relocation entries */
    long ssize; 	/* size of symbol table */
    int s;		/* section index */
    RELOC *rp;		/* relocation table pointer */

    if (doload_trace > 1)
	printf("FixIt(%d, %d, %s)\n", inFD, outFD, EntryPointName);

    /* set up environment */

    doload_setup(e = &E, inFD, Fix);
    if (setjmp(e->errorJump)) {
	doload_cleanup(e);
	return 99;
    }

    /* read module into memory */
    doload_read(e);

    /* fix up symbol table */
    doload_preset(e);

    /* repair relocation tables */
    doload_fixup(e);

    /* get entry point */
    FixEntryPoint(e, EntryPointName);

    /* write out result */

    e->filehdr.f_nsyms = e->newsymcount;
    safe_write(e, outFD, (char *)&e->filehdr, FILHSZ);
    safe_write(e, outFD, (char *)&e->aouthdr, e->filehdr.f_opthdr);

    for (s = 0; s < e->filehdr.f_nscns; s += 1) {
	safe_write(e, outFD, (char *)&e->scn[s], SCNHSZ);
    }

    for (s = 0, rcount = 0; s < e->filehdr.f_nscns; s += 1) {
	switch(e->scn[s].s_flags & 0xffff) {
	    case STYP_TEXT:
	    case STYP_DATA:
		if (lseek(outFD, e->scn[s].s_scnptr, 0) < 0) doload_punt(e, "section seek failed");
		safe_write(e, outFD, e->segptr[s], e->scn[s].s_size);
		rcount += e->scn[s].s_nreloc;
		break;

	    case STYP_BSS:
		rcount += e->scn[s].s_nreloc;
		break;
	}
    }

    if (rcount > 0) {
	rp = e->rtab;
	for (s = 0; s < e->filehdr.f_nscns; s += 1) {
	    switch(e->scn[s].s_flags & 0xffff) {
	    case STYP_TEXT:
	    case STYP_DATA:
	    case STYP_BSS:
		if (e->scn[s].s_nreloc > 0) {
		    if (lseek(outFD, e->scn[s].s_relptr, 0) < 0) doload_punt(e, "relocation seek failed");
		    safe_write(e, outFD, (char *)rp, e->scn[s].s_nreloc * RELSZ);
		    (char *)rp += e->scn[s].s_nreloc * RELSZ;
		}
		break;
	    }
	}
    }

    WriteNewSym(e, outFD);

    doload_cleanup(e);
    if (e->problems)
	return 98;
    else
        return 0;
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
	if (*p == '/')          /* ignore period if '/' follows */
	    p += 1, q = name, ext = NULL;
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
    int failed=0;

    while (--argc > 0) {
	if (**++argp == '-') {
	    switch (*++*argp) {
	    case 'd':
		doload_trace += 1;
		break;
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
	}
	else {
	    gotcha += 1;
	    outname = ComputeOutputFileName(*argp, ".do");
	    infd = open(*argp, O_RDONLY, 0);
	    if (infd < 0)
		fprintf(stderr, "dofix:  File %s not found\n", *argp);
	    else {
		outfd = open(outname, O_WRONLY+O_CREAT+O_TRUNC, 0644);
		if (outfd < 0) {
		    fprintf(stderr, "dofix:  Can not write file %s\n", outname);
		    perror("dofix");
		}
		else {
		    failed = FixIt(infd, outfd, EntryPointName);
		    close(outfd);
		}
		close(infd);
	    }
	}
    }
    if (gotcha == 0) {
	failed = FixIt(0, 1, EntryPointName);
    }
    exit(failed);
}
