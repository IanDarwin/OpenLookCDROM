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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sun_sparc/RCS/doload.c,v 1.6 1992/12/15 20:59:52 rr2b R6tape $";
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

    safe_read(e, (char *)&(e->header), (long)sizeof e->header);
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

    if (e->header.a_trsize + e->header.a_drsize > 0) {
	long rsize;		/* size of relocation info */

	rsize = e->header.a_trsize + e->header.a_drsize;
	e->rtab = (struct relocation_info *)safe_malloc(e, rsize);
	safe_read(e, (char *)e->rtab, rsize);
    }

    /* read symbol table */
    /* Hope symbol table comes Right after data relocations !! */
    e->symtab = (struct nlist *)safe_malloc(e, (long)e->header.a_syms);
    safe_read(e, (char *)e->symtab, (long)e->header.a_syms);

    /* read string table */
    /* Hope string table comes Right after symbol table !! */
    if (read(e->fd, (char *)&stringlen, sizeof stringlen) == sizeof stringlen) {
	e->stringtab = safe_malloc(e, stringlen);
	safe_read( e, e->stringtab + sizeof stringlen,
		   stringlen - sizeof stringlen);
	bcopy((char *)&stringlen, e->stringtab, sizeof stringlen);
    }
}

/* read and relocate module */
void *(* doload(inFD, name, bp, lenP, path) )()
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

    return (void *(*)()) (e->text + e->header.a_entry);
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
    i = (i & N_TYPE) >> 1;
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
	char *np = ((sp->n_un.n_strx)
		 ? (e->stringtab + sp->n_un.n_strx) : "<<noname>>" ) ;

	if (e->mode == List) {
	    printf( " %.2x %.2x %.4x %.8x  %s %s %s\n",
		    sp->n_type, sp->n_other, sp->n_desc, sp->n_value,
		    RelocType(sp->n_type),
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
	    sp->n_type = N_ABS + N_EXT;
	} /* endif N_UNDF */
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
	char *np = ((sp->n_un.n_strx)
		 ? (e->stringtab + sp->n_un.n_strx) :  "<<noname>>");
	if (e->mode == List) {
	    if (tw)
		(void) printf("%x+", tw);
	}
	if ( SYM_TYPE(sp) == N_UNDF && e->mode == Load)
	    doload_punt(e,
		"Internal botch - should have resolved in doload_preset");
	if (e->mode == List)
	    (void) printf( "%s=%x<%s>", np, sp->n_value,
		    RelocType(sp->n_type));
	else {
	    tw += sp->n_value;
	    switch ( SYM_TYPE(sp) ) {
	    case N_DATA:
	    case N_BSS:
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
			 rp->r_address, rp->r_symbolnum, *((char *)rp + 7));
		fprintf( stderr,
			 "  symtab[%.6x]: %.8x %.2x %.2x %.4x %.8x %s\n",
			 rp->r_symbolnum, sp->n_un.n_strx, sp->n_type,
			 sp->n_other, sp->n_desc, sp->n_value, np);
		e->problems++;
	    }
	}
    } /* endif IS_RP_EXTERN( rp ) */
    else {
	if (e->mode == List)
	    printf( "%x<%s>", tw,
		    RelocType(rp->r_symbolnum));
	switch (rp->r_symbolnum & N_TYPE)
		{
	    case N_DATA:
	    case N_BSS:
	    case N_TEXT:
		tw += (long) e->text;
		break;
	    case N_ABS:
		if ((tw & 0xf00f0000) == 0xa0080000) {
		    register int i = (tw >> 20) & 0xFF;
		    char *np = (i < globalcount)
			     ? globals[i].entryname : "**INDEX TOO LARGE**";
		    if (e->mode == List)
			printf("  >>%s<<", np);
		    else if (i >= globalcount) {
			fprintf(stderr, "doload:  special index invalid\n");
			e->problems++;
		    }
		    else {
			tw = globals[i].entrypoint;
		    }
		}
		break;
	    default:
		doload_punt(e, "unknown symbol type");
	} /* end switch */
    } /* end else */
    return tw;
}

/* relocate one item */

#define HI10MASK 0xffc00000
#define HI22MASK 0xfffffc00
#define LOW10MASK 0x3ff
#define HI2MASK 0xc0000000
#define LOW30MASK 0x3fffffff
#define LOW22MASK 0x3fffff
#define HI3MASK 0xe000
#define LOW13MASK 0x1fff

doload_relocate(e, cp, rp)
register struct doload_environment *e;
register char *cp;
register struct relocation_info *rp;
{
    register long tw;

    switch (rp->r_type) {
    case RELOC_8: /* 1 byte */
    case RELOC_DISP8:
        tw = rp->r_addend;
	if (rp->r_type == RELOC_DISP8) {
	    tw += rp->r_address;
	    tw = adjust(e, tw, rp, "(pcrel byte)");
	    tw -= (long)cp;
	}
	else
            tw = adjust(e, tw, rp, "(byte)");
	if (e->mode == Load) {
	    if (tw > 255)
		doload_punt(e, "byte displacement overflow");
	    *cp = tw;
	}
	break;
    case RELOC_16: /* 2 bytes */
    case RELOC_DISP16:
        tw = rp->r_addend;
	if (rp->r_type == RELOC_DISP16) {
	    tw += rp->r_address;
	    tw = adjust(e, tw, rp, "(pcrel short)");
	    tw -= (long)cp;
	}
        else
            tw = adjust(e, tw, rp, "(short)");
	if (e->mode == Load) {
	    if (tw < -32768 || tw > 32767)
		doload_punt(e, "short displacement overflow");
	    *(short *)cp = tw;
	}
	break;
    case RELOC_32: /* 4 bytes */
    case RELOC_DISP32:
        tw = rp->r_addend;
	if (rp->r_type == RELOC_DISP32) {
	    tw += rp->r_address;
	    tw = adjust(e, tw, rp, "(pcrel word)");
	    tw -= (long)cp;
	}
	else
	    tw = adjust(e, tw, rp, "(word)");
	if (e->mode == Load) {
	    *(long *)cp = tw;
	}
	break;
    case RELOC_HI22:
        tw = rp->r_addend;
        tw = adjust(e, tw, rp, "(hi22)");
        if (e->mode == Load) {
            *(long *)cp = (*(long *)cp & HI10MASK) + (tw >> 10);
        }
        break;
    case RELOC_LO10:
        tw = rp->r_addend;
        tw = adjust(e, tw, rp, "(low10)");
        if (e->mode == Load) {
            *(long *)cp = (*(long *)cp & HI22MASK) + (tw & LOW10MASK);
        }
        break;
    case RELOC_WDISP30:
        tw = rp->r_addend;
        tw += rp->r_address;
        tw = adjust(e, tw, rp, "(pcrel 30 bit word offset)");
        tw -= (long)cp;
        if (e->mode == Load) {
            *(long *)cp = (*(long *)cp & HI2MASK) + ((tw >> 2) & LOW30MASK);
        }
        break;
    case RELOC_WDISP22:
        tw = rp->r_addend;
        tw += rp->r_address;
        tw = adjust(e, tw, rp, "(pcrel 22 bit word offset)");
        tw -= (long)cp;
        if (e->mode == Load) {
            *(long *)cp = (*(long *)cp & HI10MASK) + ((tw >> 2) & LOW22MASK);
        }
        break;
    case RELOC_22:
        tw = rp->r_addend;
        tw = adjust(e, tw, rp, "(22 bit)");
	if (e->mode == Load) {
            *(long *)cp = (*(long *)cp & HI10MASK) + (tw & LOW22MASK);
	}
	break;
    case RELOC_13:
        tw = rp->r_addend;
        tw = adjust(e, tw, rp, "(13 bit)");
	if (e->mode == Load) {
	    *(short *)cp = (*(short *)cp & HI3MASK) + (tw & LOW13MASK);
	}
	break;
    default:
	doload_punt(e, "unknown relocation type");
    }
}
