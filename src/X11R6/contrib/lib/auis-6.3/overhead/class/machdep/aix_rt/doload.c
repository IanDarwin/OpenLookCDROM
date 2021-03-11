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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/aix_rt/RCS/doload.c,v 1.7 1992/12/15 20:58:42 rr2b R6tape $";
#endif

/* 
	doload.c - dynamic loader for class system

	Author:  John H Howard - April 4, 1987
 */

#include <stdio.h>
#include <a.out.h>
#include <setjmp.h>
#include <doload.h>

#include <aixfix.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/stat.h>

char *malloc();
char *realloc();
long lseek();

int doload_trace=0;		/* nonzero if debugging */
char doload_extension[] = ".do";

#include "../common/safe.h"

/* initialize state */

void doload_setup(e, inFD, mode)
struct doload_environment *e;
int inFD;
doload_mode mode;
{
    e->mode = mode;
    e->fd = inFD;
    e->problems = 0;
    e->text = NULL;
    e->rtab = NULL;
    e->symtab = NULL;
    e->stringtab = NULL;
    e->newsym = NULL;
    e->newsymcount = 0;
    return;
}

/* tear down environment */

void doload_cleanup(e)
struct doload_environment *e;
{
    if (e->problems > 0) {
	e->problems = 0;
	doload_punt(e, "Errors while processing");
    }
    safe_free((char *)e->rtab);
    safe_free((char *)e->symtab);
    safe_free(e->stringtab);
    safe_free((char *)e->newsym);
    return ;
}

/* read module into memory */

doload_read(e)
struct doload_environment *e;
{
    long stringlen;	/* length of string table */

    /* read header */

    /* 
     * This code is very system dependent.  The headers
     * that the AIX ld command generates vary between 
     * 0x28 and 0x38 depending on whether the values for
     * the ending fields of the header contain values
     * other than the defaults that can be obtained from
     * the preceeding values.
     * 
     * When the header is read, if it is shorter than
     * 0x38 the last few fields in the record will be 
     * filled in and the header length forced to 0x38
     * so all software that uses the header can assume
     * the whole header is available.
     */

    safe_read(e, (char *)&(e->header.a_magic[0]), (long) 0x20);	/* read in the shortest form */
    if (e->header.a_hdrlen != (unsigned) 0x20) {		/* full length header? */
	safe_read(e, (char *)&(e->header.a_trsize), (unsigned long) e->header.a_hdrlen - (unsigned long) 0x20);    /* get the rest */
    }
    switch (e->header.a_hdrlen)	{   /* fall through to initialize whole thing */
	case 0x20:  e->header.a_trsize = (long) 0;
	case 0x24:  e->header.a_drsize = (long) 0;
	case 0x28:  e->header.a_tbase = (long) 0;
	case 0x2c:  e->header.a_dbase = e->header.a_text;
	case 0x30:  e->header.a_lnums = (long) 0;
	case 0x34:  e->header.a_toffs = (long) 0x38;
    }

    /*
     * This is the logical place to do this but macros
     * such as A_TEXTPOS use the value of a_hdrlen for 
     * the given file so we have to leave a_hdrlen in place
     * for now.  This is done at the end of the routine.
     */
/*    e->header.a_hdrlen = 0x38;	*/    /* force the length to full sized header */


    /* if desired, print a debugging message */

    if (e->mode == List)
	printf( "\nHEADER\n  magic= %x\n  text = %x\n  data = %x\n\
  bss  = %x\n  syms = %x\n  entry= %x\n  trsize=%x\n  drsize=%x\n",
		e->header.a_magic, e->header.a_text, e->header.a_data,
		e->header.a_bss, e->header.a_syms, e->header.a_entry,
		e->header.a_trsize, e->header.a_drsize);
    if (N_BADMAG(e->header))
	doload_punt(e, "file not in loader format");

    /* read text plus data */
    e->text = safe_malloc( e,
		 (long)(e->header.a_text + e->header.a_data + e->header.a_bss));
    e->data = e->text + e->header.a_text;
    safe_lseek(e, (long)N_TXTOFF(e->header), 0);
    safe_read(e, e->text, (long)(e->header.a_text + e->header.a_data));
    bzero(e->data + e->header.a_data, e->header.a_bss);

    /* read relocation information */

    if ( !A_HASRELS( e->header ) ) /* Else Don't Bother */
	doload_punt(e, "object hasn't any relocation info !!");
    else if (e->header.a_trsize + e->header.a_drsize > 0) {
	long rsize;		/* size of relocation info */

	rsize = e->header.a_trsize + e->header.a_drsize;
	e->rtab = (struct relocation_info *)safe_malloc(e, rsize);
	safe_lseek(e, A_TRELPOS(e->header), 0);
	safe_read(e, (char *)e->rtab, e->header.a_trsize );
	safe_lseek(e, A_DRELPOS(e->header), 0);
	safe_read(e, (char *)e->rtab + e->header.a_trsize,
		  e->header.a_drsize );
    }

    /* read symbol table */
    /* Hope symbol table comes Right after data relocations !! */
    e->symtab = (struct nlist *)safe_malloc(e, (long)e->header.a_syms);
    safe_read(e, (char *)e->symtab, (long)e->header.a_syms);

    /* read string table */
    /* COFF String table runs from end of linenum entries to end of file */
    {
    struct stat sbuf ;
    if ( fstat( e->fd, &sbuf ) < 0 )
	doload_punt(e, "fstat of object file desciptor failed");
    stringlen = sbuf.st_size - A_NAMEPOS( e->header ) ;
    e->stringtab = safe_malloc(e, stringlen);
    safe_lseek(e, A_NAMEPOS(e->header), 0);
    safe_read(e, (char *)e->stringtab, stringlen ) ;
    }


    /* had to defer this to the very end */
    e->header.a_hdrlen = 0x38;	    /* force the length to full sized header */

}

/* read and relocate module */
char *doload(inFD, name, bp, lenP, path) /* return pointer to entry point, */
				/* or NULL if error */
int inFD;			/* open fd for package file */
char *name;			/* name of package being loaded */
char **bp;			/* base address of package */
long *lenP;			/* size of text segment */
char *path;			/* Pathname of package being loaded */
				/* Path is used by the MACH loader, not this one */
{
    struct doload_environment E;
    register struct doload_environment *e;
    unsigned long n;	/* number of relocation items */
    struct relocation_info *rp;

    /* set up environment */

    doload_setup(e = &E, inFD, Load);
    if (setjmp(e->errorJump)) {
	doload_cleanup(e);
	return NULL;
    }

    /* read module into memory */

    doload_read(e);

    /* do relocation */

    if (e->header.a_syms)
	doload_preset(e);
    rp = e->rtab;
    for (n = (e->header.a_trsize)/(sizeof *rp); n > 0; n--, rp++) {
	doload_relocate(e, e->text + rp->r_address, rp);
    }
    for (n = (e->header.a_drsize)/(sizeof *rp); n > 0; n--, rp++) {
	doload_relocate(e, e->data + rp->r_address, rp);
    }

    /* all done */

    if (doload_trace)
	printf( " %s: text = 0x%.8x  data = 0x%.8x  entry = 0x%.8x\n",
		name, e->text, e->data, e->text + e->header.a_entry);

    if(bp!=NULL) *bp = e->text;
    if(lenP!=NULL) *lenP = e->header.a_text;

    doload_cleanup(e);

    return e->text + e->header.a_entry;
}

extern struct globaltab {
    long entrypoint;	/* entry point value */
    char *entryname;	/* symbolic name */
} globals[];
extern long globalcount;

/* preset global symbols */

static char *symtypename[] = {"UNDF", "ABS ", "TEXT", "DATA", "BSS ", "????" };


char *RelocType(i)
int i;
{
    i &= N_SECT ;
    return symtypename[i <= 4 ? i : 5];
}

doload_preset(e)
register struct doload_environment *e;
{
    register struct nlist *sp;
    register struct nlist *sbound;

    sp = e->symtab;
    sbound = (struct nlist *)((char *)sp + e->header.a_syms);

    for (; sp < sbound; sp++) {
	char *np = ( sp->_n._n_n._n_zeroes
	 	? sp->_n._n_name
	 	: (e->stringtab + sp->_n._n_n._n_offset ) ) ;

	if (e->mode == List) {
	    printf( " %.2x %.2x %.4x %.8x  %s %s %s\n",
		    sp->n_sclass, sp->n_numaux, sp->n_type, sp->n_value,
		    RelocType(sp->n_sclass),
		    ( IS_EXTERN_SYM( sp ) ? "EXT " : "    "),
		    np );
	}
	else if ( SYM_TYPE(sp) == N_UNDF) {
	    register int i;

	    for (i = globalcount;
		 --i >= 0 && strcmp(globals[i].entryname, np) != 0; ) ;
	    if (i >= 0)
		sp->n_value = globals[i].entrypoint;
	    else if (sp->n_value > 0) {

                unsigned long length = sp->n_value;

		sp->n_value = (unsigned long)safe_malloc(e, length);
                bzero(sp->n_value, length);
	    }
	    else {
		fprintf(stderr, "doload:  Undefined symbol: %s\n", np);
		e->problems++;
	    }
	    sp->n_sclass = N_ABS + N_EXT;
	} /* endif N_UNDF */
	/* Discard SymbolTable Auxiliary Entries */
	{
	register int auxcnt ;
	if ( auxcnt = sp->n_numaux ) {
	    sp->n_numaux = 0 ; /* Set Number to Zero Entries */
	    sp += auxcnt ; /* Skip Auxiliary Symbol Entries */
	}
	}
    }
}

/* compute relocation adjustment */

long adjust(e, tw, rp, format)
register struct doload_environment *e;
register long tw;
register struct relocation_info *rp;
char *format;
{
    if (e->mode == List)
	printf("  %s", format);
    if (IS_RP_EXTERN( rp )) {
	register struct nlist *sp = e->symtab + rp->r_symbolnum;
	char *np = sp->_n._n_n._n_zeroes
		? sp->_n._n_name
		: ( e->stringtab + sp->_n._n_n._n_offset ) ;
	if (e->mode == List) {
	    if (tw)
		(void) printf("%x+", tw);
	}
	if ( SYM_TYPE(sp) == N_UNDF && e->mode == Load)
	    doload_punt(e,
		"Internal botch - should have resolved in doload_preset");
	if (e->mode == List)
	    (void) printf( "%s=%x<%s>", np, sp->n_value,
		    RelocType(sp->n_sclass));
	else {
	    tw += sp->n_value;
	    switch ( SYM_TYPE(sp) ) {
	    case N_DATA:
	    case N_BSS:
		/* Relocate the data segment to right after the text */
		tw -= e->header.a_dbase - e->header.a_text ;
	    case N_TEXT:
		tw += (long) e->text;
	    case N_ABS:
		break;
	    case N_UNDF:
		if (IS_EXTERN_SYM( sp ))
		    break;
	    default:
		fprintf(stderr, "doload:  Unknown relocation in symbol.\n");
		fprintf( stderr, "  reltab:  %.8x %.6x %.2x\n",
			 rp->r_address, rp->r_symndx, *((char *)rp + 7));
		fprintf( stderr,
			 "  symtab[%.6x]: %.8x %.2x %.2x %.4x %.8x %s\n",
			 rp->r_symndx, sp->_n._n_n._n_offset,
			 sp->n_type & 0xFF, sp->n_type >> 8,
			 sp->n_sclass, sp->n_value, np);
		e->problems++;
	    }
	}
    } /* endif IS_RP_EXTERN( rp ) */
    else {
	if (e->mode == List)
	    printf( "%x<%s>", tw,
		    RelocType( - rp->r_symndx ));
	switch ( rp->r_symndx )
		{

	    /* AIX's "ld" puts DATA & BSS symbols
		at offset 0x10000000  beyond the start of TEXT.
		We will load DATA & BSS right after TEXT,
		all in the same Segment ( i.e. the BSS segment ) */
	    case S_DATA:
	    case S_BSS:
		tw += (long) e->data - e->header.a_dbase ;
		break;
	    case S_TEXT:
		tw += (long) e->text ;
	    case S_ABS:
		break;
	    default:
		doload_punt(e, "unknown symbol type");
	} /* end switch */
    } /* end else */
    return tw;
}

/* relocate one item */

doload_relocate(e, cp, rp)
register struct doload_environment *e;
register char *cp;
register struct relocation_info *rp;
{
    register long tw;

    switch (RP_LENGTH( rp )) {
    case 0:	/* 1 byte */
	tw = *cp;
  	if (IS_RP_PC_REL( rp )) {
  	    tw += rp->r_address;
  	    tw = adjust(e, tw, rp, "(pcrel)");
  	    tw -= (long)cp;
  	}
  	else
	    tw = adjust(e, tw, rp, "(char)");
	if (e->mode == Load) {
	    if (tw > 255)
		doload_punt(e, "byte displacement overflow");
	    *cp = tw;
	}
	break;
    case 1:	/* 2 bytes */
	tw = *(short *)cp;
	if (IS_RP_PC_REL( rp ))
	    doload_punt(e, "pc relative short relocation");
	tw = adjust(e, tw, rp, "(short)");
	if (e->mode == Load) {
	    if (tw < -32768 || tw > 32767)
		doload_punt(e, "short displacement overflow");
	    *(short *)cp = tw;
	}
	break;
    case 2:	/* 4 bytes */
	tw = ((*(short *)cp)<<16) + (*(unsigned short *)(cp+2));
	if (IS_RP_PC_REL( rp )) {

	/* the following kludge is taken from 4.2A's ld.c */

#define BI_LO_OP	0x88
#define BI_HI_OP	0x8f
#define BALA_OP		0x8a
#define BALI_OP		0x8c
#define CAU_OP		0xd8
#define JI_HI_OP	0x0f
#define MAX_POS_20BITS	524287
#define MAX_NEG_20BITS	-524288

	    int opcode = (tw>>24) & 0xff;
	    if (opcode >= BI_LO_OP && opcode <= BI_HI_OP) {
		if (opcode>>1 != BALA_OP>>1) { /* if not bala or balax */
		    int reg = (tw>>20) & 0x0f;
		    /* pick up a signed 20 bit value and multiply by two */	
		    tw = ((tw & 0xfffff) << 12) >> 11;
		    if (!IS_RP_EXTERN( rp ))
			tw += rp->r_address;	/* make it absolute */
		    tw = adjust(e, tw, rp, "(20bit)");
		    if (e->mode == Load) {
			tw -= (long)cp;		/* make it pc-relative */
			if (tw>>1 > MAX_POS_20BITS || tw>>1 < MAX_NEG_20BITS) {
			    tw += (long)cp;	/* make it absolute */
			    if ( opcode >> 1 == BALI_OP >> 1
			     && reg == 15 && ( tw & 0xff000000 ) == 0 ) {
				opcode = (opcode&1) | BALA_OP;
				tw = opcode<<24 | (tw&0xffffff);
			    } /* if bali 15 */
			    else {
				/* Fake long jump into the kernel */
				if ( rp->r_type == R_KCALL ) {
				    if ( opcode == BALI_OP )
					tw = 0x8a000c00 ; /* MAGIC */
				    else if ( opcode == BALI_OP + 1 )
					tw = 0x8b000c00 ; /* MAGIC */
				    else
					doload_punt(e, "KCALL 20-bit overflow");
				}
				else
				    doload_punt(e, "20-bit overflow");
			    } /* if !bali 15 */
			} /* if longer than 20 bits */
			else
			    tw = opcode<<24 | reg<<20 | tw>>1 & 0xfffff;
		    } /* if loading */
		} /* if not bala or balax */
		else { /* must be bala or balax */
		    tw = tw & 0xffffff;
		    tw = adjust(e, tw, rp, "(24bit)");
		    if (e->mode == Load) {
			if ( tw & 0xff000000 )
			    doload_punt(e, "24-bit overflow");
			tw = opcode<<24 | tw & 0xffffff;
		    }
		}
	    } /* if BI_LO_OP .. BI_HI_OP */
	    else  /* as or ld botch ??? */
		doload_punt(e,"Op-code invalid for 20/24 bit data. as/ld botch?");
	} /* if IS_RP_PC_REL( rp ) */
	else
	{
	    tw = adjust(e, tw, rp, "(word)");
	}
	if (e->mode == Load) {
	    *(short *)cp = tw >> 16;
	    *(short *)(cp + 2)= tw;
	}
	break;
    case 3:	/* split address */
	if (IS_RP_PC_REL( rp ))
	    doload_punt(e, "pc relative split relocation");
	if ( *(cp-2) == CAU_OP ) { /* low value is signed */
	    short low_half;
	    tw = (( *(short *)cp )<<16) + *(short *)(cp+4);
	    tw = adjust(e, tw, rp, "(splitcau)");
	    if (e->mode == Load) {
		low_half = tw & 0xffff;
		*(short *)cp = ((tw - low_half)>>16) & 0xffff;
		*(short *)(cp+4) = low_half;
	    }
	} else {   
	    tw = ( *(short *)(cp+4) )<<16 | *(unsigned short *)cp;
	    tw = adjust(e, tw, rp, "(split)");
	    if (e->mode == Load) {
		*(short *)cp = tw;	 /* low order two */
		*(short *)(cp+4) = tw>>16;
	    }
	}
	break;
    default:
	if ( rp->r_type != R_ABS )
	    doload_punt(e, "unknown relocation length");
    }
    return ;
}
