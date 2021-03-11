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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/hp_68k/RCS/dofix.c,v 1.4 1992/12/15 20:59:09 rr2b R6tape $";
#endif

/* 
	dofix.c - convert .o file into .do file

	Author:  John H Howard - April 9, 1987
 */


#include <stdio.h>
#include <andrewos.h> /* sys/file.h */
#define a_lesyms a_syms
#define r_info relocation_info
#include <a.out.h>
#include <setjmp.h>
#include <doload.h>

#include <aixfix.h>

#include <../common/safe.h>

/* set entry point */

/*
 * NOTE - The RT uses entry points in the data segment, rather
 * than the text segment as is usually the case.  In order to
 * allow this, we represent data segment entry points by adding
 * the text segment size to the offset within the data segment.
 */

FixEntryPoint(e, EntryPointName)
register struct doload_environment *e;
char *EntryPointName;
{
    register struct nlist *sp;
    register struct nlist *sbound;
    if (EntryPointName == NULL || *EntryPointName == NULL)
	return;
    sp = e->symtab;
    sbound = (struct nlist *)((char *)sp + e->header.a_syms);
	    for (; sp < sbound; sp = nextSym(sp)) {
		if ((sp->n_type & N_TYPE) != N_UNDF && (sp->n_type & N_EXT) == N_EXT && ((sp->n_length) ? stabStrCmp(EntryPointName, sp) : strcmp(EntryPointName, "<<noname>>")) == 0) 
	    {
	    switch ( SYM_TYPE(sp) ) {
	    case N_DATA:
	    case N_TEXT:
		e->header.a_entry = sp->n_value;
		break;
	    default:
		fprintf( stderr,
		 "dofix:  invalid entry point relocation %x\n", SYM_TYPE(sp) ) ;
		e->problems++;
	    } /* end of switch */
	    return;
	} /* end of name match */
    } /* end of loop */
    fprintf(stderr, "dofix:  entry point %s undefined\n", EntryPointName);
    e->problems++;
    return;
}

/* fix up one relocation table entry */

extern struct globaltab {
    int (*entrypoint)();
    char *entryname;	/* symbolic name */
} globals[];
extern long globalcount;

FixRelocation(e, rp)
register struct doload_environment *e;
register struct relocation_info *rp;
{
    register int i;
    register int j;

    if (rp->r_segment == REXT) {
      register struct nlist *sp = e->stab_entries[rp->r_symbolnum];
      register struct nlist *nsp = e->newsym;
      char np_str[maxString];
      char *np = np_str;
      if (sp->n_length)
	stabStrCpy(np, sp)
      else
	np="<<noname>>";
	if ( SYM_TYPE(sp) == N_UNDF) {
	    if (sp->n_value == 0) {
		for (i = globalcount; --i >= 0 ; )
		    if (strcmp(globals[i].entryname, np) == 0)
			break;
		if (i < 0) {
		    fprintf(stderr, "dofix:  Undefined:  %s\n", np);
		    e->problems++;
		}
	    }
	for (j = 0; /* check if symbol already in new sybmol table */
	     j < e->newsymcount && stabStrCmp(np, nsp) != 0;
	     nsp = nextSym(nsp), j++) ;
	if (j >= e->newsymcount) {
	  int new_entry;
	  j = e->newsymcount++;
	  new_entry = e->newsymsize;
	  e->newsymsize += symSize(sp);
	  e->newsym = (struct nlist *)safe_realloc(e,
						    (char *)e->newsym,
						    e->newsymsize);
	  nsp = (struct nlist *)((char *)e->newsym + new_entry);
	  bcopy(sp, nsp, symSize(sp));
	}
	if (sp->n_value > nsp->n_value)
	  nsp->n_value = sp->n_value;
	    rp->r_symbolnum = j;
	} /* endif N_UNDF */
	else if ( SYM_TYPE(sp) != N_ABS) {
	    fprintf( stderr,
		     "dofix:  Relocatable symbol value (%s = %d, type %d)\n",
		      np, sp->n_value, SYM_TYPE(sp) );
	    e->problems++;
	}
	else if (sp->n_value != 0) {
	    fprintf( stderr, "dofix:  Nonzero symbol value (%s = %d)\n",
		     np, sp->n_value);
	    e->problems++;
	}
    }
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

    /* allocate new string table */

    safe_write(e, outFD, (char *)e->newsym, e->newsymsize);

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
    struct relocation_info *rp;

    if (doload_trace)
	printf("FixIt(%d, %d, %s)\n", inFD, outFD, EntryPointName);

    /* set up environment */

    doload_setup(e = &E, inFD, Fix);
    if (setjmp(e->errorJump)) {
	doload_cleanup(e);
	return;
    }

    /* read module into memory */
    doload_read(e);

    /* repair relocation tables */
    rp = e->rtab;
    for ( n = (e->header.a_trsize + e->header.a_drsize)/(sizeof *rp);
	  n > 0;
	  n--, rp++) {
	FixRelocation(e, rp);
    }

    /* get entry point */
    FixEntryPoint(e, EntryPointName);

    /* write out result */
    /* build new file from original file components. Strip out debugger info. */
    e->header.a_syms = e->newsymsize;
    e->header.a_dnttsize = 0;
    e->header.a_sltsize = 0;
    e->header.a_vtsize = 0;
  
    /* Loaded files should be contiguous storage with internal sections packed one against the next.
     * There are sharable versions of HPUX a.out files that page-allign the text, data and pascal
     * sections. The Andrew Makefiles do not generate such files. This is a fundamental assumption
     * made by dofix and doload; in particular, dofix.c:FixIt and doload.c:doload_read. See the
     * include file /usr/include/a.out.h for definitions of TEXT_OFFSET, DATA_OFFSET and
     * MODCAL_OFFSET. 
     */

    safe_write(e, outFD, (char *)&(e->header), (long)sizeof e->header);
    safe_write(e, outFD, e->text, (long)(e->header.a_text + e->header.a_data));
    WriteNewSym(e, outFD);
    safe_write(e, outFD, (char *)e->rtab, e->header.a_trsize + e->header.a_drsize);

    doload_cleanup(e);
    return ;
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
	    case 'd':
		doload_trace++;
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
	    gotcha++;
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
		    FixIt(infd, outfd, EntryPointName);
		    close(outfd);
		}
		close(infd);
	    }
	}
    }
    if (gotcha == 0) {
	FixIt(0, 1, EntryPointName);
    }
    exit(0);
}


