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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/aix_rt/RCS/dofix.c,v 1.4 1992/12/15 20:58:42 rr2b R6tape $";
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
    for (; sp < sbound; sp++) {
	if ( SYM_TYPE( sp ) != N_UNDF
	  && IS_EXTERN_SYM( sp )
	  && ( SYM_TYPE( sp ) == N_TEXT || SYM_TYPE( sp ) == N_DATA )
	  && strcmp( EntryPointName,
			( sp->_n._n_n._n_zeroes ? sp->_n._n_name
			: ( sp->_n._n_n._n_offset
			? (e->stringtab + sp->_n._n_n._n_offset )
			: "<<noname>>" ) ) ) == 0 )
	    {
	    switch ( SYM_TYPE(sp) ) {
	    case N_DATA:
	    /* AIX's "ld" puts DATA & BSS symbols
		at offset 0x10000000  beyond the start of TEXT.
		We will load DATA & BSS right after TEXT,
		all in the same Segment ( i.e. the BSS segment ) */
		e->header.a_entry = sp->n_value
				  - e->header.a_dbase + e->header.a_text ;
		break;
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
    long entrypoint;	/* entry point value */
    char *entryname;	/* symbolic name */
} globals[];
extern long globalcount;

FixRelocation(e, rp)
register struct doload_environment *e;
register struct relocation_info *rp;
{
    register int i;
    register int j;

    if ( IS_RP_EXTERN( rp ) ) {
	register struct nlist *sp = e->symtab + rp->r_symbolnum;
	char *np = sp->_n._n_n._n_zeroes ? sp->_n._n_name
	 : ( sp->_n._n_n._n_offset
		? (e->stringtab + sp->_n._n_n._n_offset )
		: "<<noname>>" ) ;
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
	    for ( j = e->newsymcount;
		--j >= 0
		&& strcmp( ( e->newsym[j]._n._n_n._n_zeroes
			? e->newsym[j]._n._n_name
			: ( e->stringtab + e->newsym[j]._n._n_n._n_offset ) ),
			np ) ;
		) ;
	    if (j < 0) {
		j = e->newsymcount++;
		e->newsym = (struct nlist *)
			    safe_realloc( e, (char *) e->newsym,
					  e->newsymcount * sizeof *(e->newsym));
		sp->n_numaux = 0 ;
		bcopy(sp, e->newsym + j, sizeof *sp);
	    }
	    if (sp->n_value > e->newsym[j].n_value)
		e->newsym[j].n_value = sp->n_value;
	    rp->r_symbolnum = j;
	} /* endif N_UNDF */
	else if ( SYM_TYPE(sp) != N_ABS) {
	    fprintf( stderr,
		     "dofix:  Relocatable symbol value (%s = %d, type %d)\n",
		      np, sp->n_value, SYM_TYPE(sp) );
	    e->problems++;
	}
	else if (sp->n_value != 0) {
	    if ( ( ( sp->n_sclass & N_CLASS ) != C_FILE )
	      && ( ( sp->n_sclass & N_CLASS ) != C_AUTO )
	      && ( ( sp->n_sclass & N_CLASS ) != C_REG )
	      && ( ( sp->n_sclass & N_CLASS ) != C_ARG )
	      && ( ( sp->n_sclass & N_CLASS ) != C_EOS ) ) {
		fprintf( stderr, "dofix:  Nonzero symbol value (%s = %d)\n",
			 np, sp->n_value);
		e->problems++;
	    }
	}
    }
    else { /* Non-Extern Symbol */
	if ( doload_trace )
	    printf(
"segment reference address = 0x%08X, segment = 0x%04X, type = 0x%04X\n",
		    rp->r_vaddr, rp->r_symndx, rp->r_type ) ;
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

    for (newstringsize = sizeof newstringsize, i = e->newsymcount; --i >= 0; )
	if ( ! e->newsym[i]._n._n_n._n_zeroes )
	    newstringsize += strlen( e->stringtab
				   + e->newsym[i]._n._n_n._n_offset ) + 1 ;
    if (doload_trace)
	printf( " new symbol count %d, new string size %d\n",
		e->newsymcount, newstringsize ) ;
    newcp = newstrings = safe_malloc(e, newstringsize);
    *(long *)newcp = newstringsize;
    newcp += sizeof newstringsize;

    /* make a new string table */

    for (i = 0; i < e->newsymcount; i++) {
	register char *oldcp ;
	register int n ;
	if ( ! e->newsym[i]._n._n_n._n_zeroes ) {
	    oldcp = e->stringtab + e->newsym[i]._n._n_n._n_offset ;
	    n = strlen(oldcp) + 1;
	    bcopy(oldcp, newcp, n);
	    e->newsym[i]._n._n_n._n_offset = newcp - newstrings;
	    newcp += n;
	}
    }

    /* write symbols and strings */

    safe_write( e, outFD, (char *) e->newsym,
		e->newsymcount * sizeof (struct nlist) );
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
    e->header.a_syms = e->newsymcount * sizeof(struct nlist);
/* Strip Away line numbers -- We don't read them so we can't write them */
    e->header.a_lnums = 0 ;
    safe_write(e, outFD, (char *)&(e->header), (long)sizeof e->header);
    if (lseek(outFD, (long)N_TXTOFF(e->header), 0) < 0)
	doload_punt(e, "seek to write text failed");
    safe_write(e, outFD, e->text, (long)(e->header.a_text + e->header.a_data));
    safe_write(e, outFD, (char *)e->rtab,
	       e->header.a_trsize + e->header.a_drsize);
    WriteNewSym(e, outFD);

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


