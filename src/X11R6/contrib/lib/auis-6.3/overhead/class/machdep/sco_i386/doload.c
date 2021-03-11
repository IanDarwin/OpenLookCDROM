/* ********************************************************************** *\
 *	   Copyright IBM Corporation 1988,1991 - All Rights Reserved	  *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sco_i386/RCS/doload.c,v 1.4 1992/12/15 20:59:52 rr2b R6tape $";
#endif

/*
	doload.c - dynamic loader for class system

	Author:  John H Howard - April 4, 1987
 */

#include <stdio.h>
#include <a.out.h>
#include <setjmp.h>
#include <doload.h>

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
	printf("%d problems found\n", e->problems);
	printf("doload: Errors while processing\n");
/*	e->problems = 0;
	doload_punt(e, "Errors while processing");*/
    }
    safe_free((char *)e->rtab);
    safe_free((char *)e->symtab);
    safe_free(e->stringtab);
    safe_free((char *)e->newsym);
    return ;
}

/* read module into memory */

void doload_read(e)
struct doload_environment *e;
{
    long stringlen;	/* length of string table */
    long csize; 	/* size of all contents */
    long rcount;	/* number of relocation entries */
    long ssize; 	/* size of symbol table */
    int s;		/* section index */
    RELOC *rp;		/* relocation table pointer */

    /* read file header */

    safe_read(e, (char *)&(e->filehdr), (long)FILHSZ);
    if (e->mode == List || doload_trace > 1) {
	printf("\nFILE HEADER\n");
	printf("  magic = %x\n",  e->filehdr.f_magic);
	printf("  nscns = %x\n",  e->filehdr.f_nscns);
	printf("  timdat= %lx\n", e->filehdr.f_timdat);
	printf("  symptr= %lx\n", e->filehdr.f_symptr);
	printf("  nsyms = %lx\n", e->filehdr.f_nsyms);
	printf("  opthdr= %x\n",  e->filehdr.f_opthdr);
	printf("  flags = %x\n",  e->filehdr.f_flags);
    }
    if (e->filehdr.f_magic != I386MAGIC
#ifndef M_UNIX
    && e->filehdr.f_magic != I386SVMAGIC
#endif
    )
	doload_punt(e, "this is not an AIX PS/2 COFF file (bad magic number)");

    /* read optional header */

    if (e->filehdr.f_opthdr > 0) {
	safe_read(e, (char *)&(e->aouthdr), (long)e->filehdr.f_opthdr);
	if (e->mode == List || doload_trace > 1) {
	    printf("\nAOUT HEADER\n");
	    printf("  magic = %x\n",  e->aouthdr.magic);
	    printf("  vstamp= %x\n",  e->aouthdr.vstamp);
	    printf("  tsize = %lx\n", e->aouthdr.tsize);
	    printf("  dsize = %lx\n", e->aouthdr.dsize);
	    printf("  bsize = %lx\n", e->aouthdr.bsize);
	    printf("  entry = %lx\n", e->aouthdr.entry);
	    printf("  dstart= %lx\n", e->aouthdr.text_start);
	    printf("  tstart= %lx\n", e->aouthdr.data_start);
	}
#ifndef M_UNIX
	switch (e->aouthdr.magic) {
	case MAG_SHROT:
	    break;
	case MAG_OVERLAY:
	case MAG_NSHWRT:
	case MAG_SHROTSEP:
	case MAG_DPSHROT:
	    if (e->mode == List)
		printf("WARNING:  Wrong aouthdr magic number\n");
	    else
		doload_punt("wrong aouthdr magic number");
	    break;
	default:
	    doload_punt(e, "this is not an AIX PS/2 COFF file (wrong aouthdr magic number)");
	} /* endswitch */
#endif
    }

    /* read section headers */

    if (e->filehdr.f_nscns > DOLOAD_MAXSCN)
	doload_punt(e, "too many sections in loader file");
    for (s = 0, csize = 0, rcount = 0; s < e->filehdr.f_nscns; s += 1) {
	safe_read(e, (char *)&(e->scn[s]), (long)SCNHSZ);
	if (e->mode == List || doload_trace > 1) {
	    printf("\nSECTION HEADER %d\n", s);
	    printf("  name  = %s\n",  e->scn[s].s_name);
	    printf("  paddr = %lx\n", e->scn[s].s_paddr);
	    printf("  vaddr = %lx\n", e->scn[s].s_vaddr);
	    printf("  size  = %lx\n", e->scn[s].s_size);
	    printf("  scnptr= %lx\n", e->scn[s].s_scnptr);
	    printf("  relptr= %lx\n", e->scn[s].s_relptr);
	    printf("  nreloc= %x\n",  e->scn[s].s_nreloc);
	    printf("  flags = %lx\n", e->scn[s].s_flags);
	}
	switch(e->scn[s].s_flags & 0xffff) {
	case STYP_TEXT:
	case STYP_DATA:
	case STYP_BSS:
	    csize += e->scn[s].s_size;
	    rcount += e->scn[s].s_nreloc;
	    break;
	 case STYP_INFO:
	 case STYP_REG:
	 case STYP_DSECT:
	    break;
	default:
	    printf("***Unknown section type (s_flags = 0x%x)\n", e->scn[s].s_flags & 0xffff);
	    fflush(stdout);
	    e->problems += 1;
	}
    }

    /* read text plus data */

    e->segptr[0] = safe_malloc(e, csize);
    for (s = 0; s < e->filehdr.f_nscns; s += 1) {

	if (s > 0)
	    e->segptr[s] = e->segptr[s-1] + e->scn[s-1].s_size;

	switch(e->scn[s].s_flags & 0xffff) {
	case STYP_TEXT:
	case STYP_DATA:
	case STYP_BSS:
#if 0
	    if ((s == 0 & e->scn[s].s_vaddr != 0) || (s > 0 && e->segptr[s] != e->segptr[s-1] + e->scn[s-1].s_size)) {
		printf("***Virtual address for section %d is inconsistent\n", s);
		fflush(stdout);
		e->problems += 1;
	    }
#endif
	    break;
	default:
	    if (s != 0)
		e->segptr[s] = e->segptr[s-1];
	}

	switch(e->scn[s].s_flags & 0xffff) {
	case STYP_TEXT:
	case STYP_DATA:
	    safe_lseek(e, e->scn[s].s_scnptr, 0);
	    safe_read(e, e->segptr[s], e->scn[s].s_size);
	    break;
	case STYP_BSS:
	    bzero(e->segptr[s], e->scn[s].s_size);
	    break;
	}
    }

    /* read relocation information */

    if (rcount > 0) {
	rp = e->rtab = (RELOC *)safe_malloc(e, rcount*RELSZ);
	for (s = 0; s < e->filehdr.f_nscns; s += 1) {
	    switch(e->scn[s].s_flags & 0xffff) {
	    case STYP_TEXT:
	    case STYP_DATA:
	    case STYP_BSS:
		if (e->scn[s].s_nreloc > 0) {
		    safe_lseek(e, e->scn[s].s_relptr, 0);
		    safe_read(e, (char *)rp, (long)(e->scn[s].s_nreloc * RELSZ));
		    ((char *)rp) += e->scn[s].s_nreloc * RELSZ;
		}
		break;
	    }
	}
    }

    /* read symbol table */

    safe_lseek(e, e->filehdr.f_symptr, 0);
    ssize = e->filehdr.f_nsyms * SYMESZ;
    e->symtab = (SYMENT *)safe_malloc(e, ssize);
    safe_read(e, (char *)e->symtab, ssize);

    /* read string table (assumed to come right after symbol table) */

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
    int s;			/* section index */

    /* set up environment */

    doload_setup(e = &E, inFD, Load);
    if (setjmp(e->errorJump)) {
	doload_cleanup(e);
	return NULL;
    }

    /* read module into memory */

    doload_read(e);

    /* do relocation */

    doload_preset(e);

    /* process contents */

    doload_fixup(e);

    /* all done */

    if (doload_trace > 0) {
	printf( " %s:", name);
	for (s = 0; s < e->filehdr.f_nscns; s += 1)
	    printf("  %.8s = 0x%.8x", e->scn[s].s_name, e->segptr[s]);
	printf( "  entry = 0x%.8x\n", e->segptr[0] + e->aouthdr.entry -
			e->scn[0].s_vaddr);
	fflush(stdout);
    }

    if (bp!=NULL)
	*bp = e->segptr[0];
    if (lenP!=NULL)
	*lenP = e->aouthdr.tsize;

    doload_cleanup(e);

    return (void *(*)()) (e->segptr[0] + e->aouthdr.entry -
		e->scn[0].s_vaddr);
}

/* get symbol name */

/* CAUTION - returns a pointer to a static */

char *doload_getname(register struct doload_environment *e, SYMENT *sp)
{
    char *np;
    static char shortname[SYMNMLEN+1];

    if (sp->n_zeroes != 0) {
	strncpy(shortname, sp->n_name, sizeof shortname);
	shortname[(sizeof shortname) - 1] = '\0';
	np = shortname;
    } else if (sp->n_offset > 0)
	np = e->stringtab + sp->n_offset;
    else
	np = "<<noname>>";
    return np;
}

extern struct globaltab {
    long entrypoint;	/* entry point value */
    char *entryname;	/* symbolic name */
} globals[];
extern long globalsize;

/* preset global symbols */

/* resolves undefined referenses to known globals in symbol table */

doload_preset(e)
register struct doload_environment *e;
{
    register SYMENT *sp;
    register SYMENT *sbound;
    int snum;
    char *np;
    int naux;
    register int i;
    long globalcount = globalsize / sizeof(struct globaltab);

    if (e->mode == List || doload_trace > 1) {
	printf("\nSYMBOL TABLE\n");
	printf("\n   #    value section type sclass nx\n");
    }

    sp = sbound = e->symtab;
    ((char *)sbound) += e->filehdr.f_nsyms * SYMESZ;

    for (snum = 0; sp < sbound; ((char *)sp) += SYMESZ, snum += 1) {
	np = doload_getname(e, sp);

	if (e->mode == List || doload_trace > 1) {
	    printf("%4d %.8x", snum, sp->n_value);
	    if (sp->n_scnum > 0 && sp->n_scnum <= e->filehdr.f_nscns)
		printf( " %7s", e->scn[sp->n_scnum - 1].s_name);
	    else if (sp->n_scnum == N_UNDEF)
		printf( " N_UNDEF");
	    else if (sp->n_scnum == N_DEBUG)
		printf( " N_DEBUG");
	    else
		printf( " %7d", sp->n_scnum );
	    printf(" %.4x", sp->n_type);
	    switch (sp->n_sclass) {
		case C_EXT:
		    printf("  C_EXT");
		    break;
		case C_STAT:
		    printf(" C_STAT");
		    break;
		case C_FILE:
		    printf(" C_FILE");
		    break;
		default:
		    printf(" %6d", sp->n_sclass);
	    }
	    printf(" %2d", sp->n_numaux);
	    printf(" %s\n", np);
	    fflush(stdout);
	}

	if (sp->n_scnum == N_DEBUG || sp->n_sclass != C_EXT || e->mode == List)
	    ;

	else if (sp->n_scnum == N_UNDEF) {

	    /* handle named common allocation */
	    if (e->mode == Load && sp->n_value > 0) {
		char *addr;
		long size;

		size = sp->n_value;
		addr = safe_malloc(e, size);
		bzero(addr, size);
		/* 
		 * We subtract the length from the pointer here
		 * because the references in the code have the length added.
		 * Why?  I do not know.
		 */
		sp->n_value = (unsigned long)addr - size;
		sp->n_scnum = N_ABS;
	    }

	    /* handle currently undefined symbols */
	    if (sp->n_value == 0) {
		if (strcmp(np, "etext") == 0) {
		    if (e->mode == Load) {
			sp->n_value = (long)e->segptr[0] + e->aouthdr.tsize;
			sp->n_scnum = N_ABS;
		    }
		}
		else {
		    for (i = globalcount; --i >= 0; ) {
			if (strcmp(globals[i].entryname, np) == 0)
			    break;
		    }
		    if (i < 0) {
			printf("***Undefined symbol: %s\n", np);
			fflush(stdout);
			e->problems += 1;
		    }
		    else if (e->mode == Load) {
			sp->n_value = globals[i].entrypoint;
			sp->n_scnum = N_ABS;
		    }
		}
	    }
	} /* endif N_UNDEF */
	for (naux = sp->n_numaux; naux > 0; naux--) {
	    ((char *)sp) += AUXESZ;
	    snum += 1;
	}
    }
}

/* compute relocation adjustment */

static long 
adjust(e, tw, rp, format)
register struct doload_environment *e;
register long tw;
register RELOC *rp;
char *format;
{
    SYMENT *sp;
    int s;
    register int j;
    SYMENT *nsp;

    if (e->mode == List || doload_trace > 1)
	printf("  %s", format);

    ((char *)sp) = (char *)e->symtab + rp->r_symndx * SYMESZ;
    s = sp->n_scnum;

    switch (s) {
	case N_ABS:
	    if (e->mode == List || doload_trace > 1)
		printf( "0x%x+%s(=0x%x)", tw, doload_getname(e, sp), sp->n_value);
	    tw += sp->n_value;
	    break;
	case N_UNDEF:
	    if (e->mode == List || doload_trace > 1) {
		printf("%s", doload_getname(e, sp));
		if (tw != 0)
		    printf("+0x%x", tw);
	    }
	    if (e->mode == Load) {
		printf("***undefined symbol %s\n", doload_getname(e, sp));
		fflush(stdout);
		e->problems += 1;
	    }
	    break;
	default:
	    if (s > 0 && s <= e->filehdr.f_nscns) {
/*printf("symndx %d, section %s\n",rp->r_symndx, e->scn[s-1].s_name);*/
		if (e->mode == List || doload_trace > 1) {
		    printf( "0x%x+%s", tw, e->scn[s - 1].s_name);
		    if (sp->n_value != 0)
			printf("(0x%x)", sp->n_value);
		}
		tw += (long)e->segptr[s - 1] - e->scn[s - 1].s_vaddr;
	    }
	    else {
		if (e->mode == List || doload_trace > 1)
		    printf( "0x%x<%d:0x%x>", tw, rp->r_symndx, s);
		printf("***symbol type (n_scnum=%d) unknown\n", s);
		fflush(stdout);
		e->problems += 1;
	    }
    } /* end switch */

    /* preserve selected entries in the new smaller symbol table */

    if (e->mode == Fix) {
	sp->n_numaux = 0;
	for (j = 0, nsp = e->newsym; j < e->newsymcount; j += 1, ((char *)nsp) += SYMESZ)
	    if (bcmp((char *)sp, (char *)nsp, SYMESZ) == 0)
		break;
	if (j >= e->newsymcount) {
	    e->newsymcount += 1;
	    e->newsym = (SYMENT *)safe_realloc(e, (char *)e->newsym, (long)(e->newsymcount * SYMESZ));
	    ((char *)nsp) = (char *)e->newsym + j * SYMESZ;
	    bcopy(sp, (char *)nsp, SYMESZ);
	}
	if (doload_trace > 1)
	    printf(" %d->%d", rp->r_symndx, j);
	rp->r_symndx = j;
    }

    return tw;
}

/* relocate one item */

static void
relocate(e, cp, rp)
register struct doload_environment *e;
register char *cp;
register RELOC *rp;
{
    register long tw;
    long pc;

    switch (rp->r_type) {

    case R_ABS:
	break;

    case R_DIR16:
	tw = *(short *)cp;
	tw = adjust(e, tw, rp, "(DIR16)");
	if (e->mode == Load) {
	    *(short *)cp = tw;
	}
	break;

    case R_DIR32:
/*
printf("relocating DIR32: cp = %08x, ", cp);
fflush(stdout);
printf("*cp = %08x\n",*((long *)cp));
*/

	tw = *((long *)cp);
/*printf("adjusting..");fflush(stdout);*/
	tw = adjust(e, tw, rp, "(DIR32)");
/*printf("now it's %08x.\n",tw);*/
	if (e->mode == Load)
	    *(long *)cp = tw;
	break;

    case R_PCRLONG:
	tw = *(long *)cp;
	pc = (long)cp + 4;
	tw = adjust(e, tw + (pc - (long)e->segptr[0] + e->scn[0].s_vaddr), rp, "(PCRLONG)") - pc;
	if (e->mode == Load)
	    *(long *)cp = tw;
	break;

    default:
	printf(" ***relocation type (r->type = %x) unknown\n", rp->r_type);
	fflush(stdout);
	e->problems += 1;

    }

    return;
}

static void printaddr(e, cp)
register struct doload_environment *e;
register unsigned long cp;
{
    printf("\n%.8x ", cp);
    return ;
}

/* format and display data */

static char *printdata(e, cp, base, bound)	/* returns bound */
register struct doload_environment *e;
register char *cp;
register char *base;
register char *bound;
{
    int k;

    if (cp < bound) {
	printaddr(e, (unsigned long)(cp - e->segptr[0]));
	for (k = (cp - e->segptr[0]) & 0x0f; k > 0; k--)
	    printf("   ");
	for (;;) {
	    printf(" %.2x", *cp++);
	    if (cp >= bound)
		break;
	    if (((cp - e->segptr[0]) & 0x0f) == 0)
		printaddr(e, (unsigned long)(cp - e->segptr[0]));
	}
    }
    return bound;
}

static int relocationbound(e, rp, bound)
register struct doload_environment *e;
register RELOC *rp;
int bound;
{
    long result;
#ifdef undef
    printf("\nRELOC %x\n", rp);
    printf("  r_vaddr  = 0x%lx\n", rp->r_vaddr);
    printf("  r_symndx = 0x%lx\n", rp->r_symndx);
    printf("  r_type   = %d\n",    rp->r_type);
#endif undef
/*printf("relocationbound(e, rp->r_vaddr=%08x, bound=%08x)\n",
rp->r_vaddr, bound);*/

    switch (rp->r_type) {

    case R_DIR16:
    case R_OPT16:
	result = rp->r_vaddr + 2;
	break;

    case R_DIR32:
    case R_PCRLONG:
    default:
	result = rp->r_vaddr + 4;
    }

    if (result > bound) {
	printf("\nrelocation address 0x%x out of range 0x%x!\n", result, bound);
	result = bound;
    }
    return result;
}

static void fixsegment(e, n, base, sp, rpp)
register struct doload_environment *e;
register unsigned long n;	/* number of relocation items */
register char *base;
SCNHDR *sp;
RELOC **rpp;
{
    register RELOC *rp = *rpp;
    char *cp;

    for (cp = base; n > 0; n--, ((char *)rp) += RELSZ) {
/*
printf("\nn=%04d ",n);
printf("printdata(%08x, %08x, %08x, %08x)\n",
e,cp,base,(base+relocationbound(e,rp,(base+sp->s_size))-sp->s_vaddr));
*/
	if (e->mode == List || doload_trace > 1)
	    cp = printdata(e, cp, base, (char *)(base + relocationbound(e, rp, (int)(base + sp->s_size))- sp->s_vaddr));
/*printf("relocate in base %08x: ",base);*/
	relocate(e, base + rp->r_vaddr - sp->s_vaddr, rp);
    }
    if (e->mode == List || doload_trace > 1) {
	cp = printdata(e, cp, base, base + sp->s_size);
	fflush(stdout);
    }

    *rpp = rp;
    return ;
}

/* fix up loaded text */

doload_fixup(e)
register struct doload_environment *e;
{
    RELOC *rp;
    int s;

    for (s = 0, rp = e->rtab; s < e->filehdr.f_nscns; s += 1) {

	if (e->mode == List || doload_trace > 1)
	    printf("\n\n%s SEGMENT %d\n", e->scn[s].s_name, s);

	switch(e->scn[s].s_flags & 0xffff) {
	case STYP_TEXT:
	case STYP_DATA:
	case STYP_BSS:
	    fixsegment(e, (unsigned long)e->scn[s].s_nreloc, e->segptr[s], &e->scn[s], &rp);
	    break;
	    }
	}
}
