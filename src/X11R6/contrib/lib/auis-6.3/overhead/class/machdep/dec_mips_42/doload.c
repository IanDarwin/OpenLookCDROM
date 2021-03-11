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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/dec_mips_42/RCS/doload.c,v 1.4 1992/12/15 20:59:09 rr2b R6tape $";
#endif

/* 
 *	doload.c - dynamic loader for class system
 *
 *	Author:  Zalman Stern July 1989
 *      (Rewritten to handle non-G0 code by Zalman Stern May 1991.)
 */

#include <stdio.h>
#include <a.out.h>
#include <setjmp.h>
#include <doload.h>
#include <mips/cachectl.h>

#include <andrewos.h> /* sys/types.h */

char *malloc();
char *realloc();
long lseek();

/* External variables defined during the final link process which provide a
 * pool of gp addressable space.
 */
extern char *mips_GlobalArea; 	/* The pool */
extern int mips_GlobalSize;	/* Amount of space left in the pool */

int doload_trace = 0;		/* nonzero if debugging */

extern struct globaltab {
    long entrypoint;	/* entry point value */
    char *entryname;	/* symbolic name */
} globals[];

extern long globalcount;

#include "../common/safe.h"

static char *read_section(e, offset, buffer, length)
    struct doload_environment *e;
    long offset;
    char *buffer;
    long length;
{

    if (buffer == NULL) {
        buffer = (char *) safe_malloc(e, length);
    }
    safe_lseek(e, offset, L_SET);
    safe_read(e, buffer, length);
    return buffer;
}

/* initialize state */
static void doload_setup(e, inFD, mode)
    struct doload_environment *e;
    int inFD;
    doload_mode mode;
{
    e->mode = mode;
    e->fd = inFD;
    e->problems = 0;
    e->sections = NULL;
    e->text = NULL;
    e->gp_pool = NULL;
    bzero(e->relocSection, sizeof(e->relocSection));
    e->symtab = NULL;
    e->stringtab = NULL;
    e->common_space = NULL;
    e->common_size = 0;
    return;
}

/* tear down environment */

static void doload_cleanup_success(e)
    struct doload_environment *e;
{
    struct doload_section *section;

    ENUMERATE_SECTIONS(section)
        safe_free((char *)section->rtab);
    safe_free((char *)e->sections);
    safe_free((char *)e->symtab);
    safe_free(e->stringtab);
}

static void doload_cleanup(e)
    struct doload_environment *e;
{
    if (e->problems > 0) {
	e->problems = 0;
	doload_punt(e, "Errors while processing");
    }

    doload_cleanup_success(e);

    safe_free((char *)e->text);
/* Return space to global area pool */
    if (e->gp_pool != NULL) {
        mips_GlobalSize += mips_GlobalArea - e->gp_pool;
        mips_GlobalArea = e->gp_pool;
    }
}

static struct globaltab *find_global(name)
    char *name;
{
    register struct globaltab *thisGlobal;

    for (thisGlobal = globals;
         thisGlobal < globals + globalcount &&
          strcmp(thisGlobal->entryname, name) != 0;
         thisGlobal++ )
        ;
    if (thisGlobal < globals + globalcount)
        return thisGlobal;
    else
        return NULL;
}

#define ROUNDUP4(value)  (((value) + 3) & (~0x3))
#define ROUNDUP8(value)  (((value) + 7) & (~0x7))

/* Find the amount of space needed for unallocated common symbols.
 * Doesn't included common symbols which have to be allocated in the global
 * area.
 */
static unsigned long common_space(e)
    struct doload_environment *e;
{
    pEXTR sp;
    pEXTR sbound;
    unsigned long common_total = 0;

    for (sp = e->symtab; sp < e->symtab + e->symheader.iextMax; sp++)
        if (find_global(e->stringtab + sp->asym.iss) == NULL && sp->asym.sc
	    == scCommon && sp->asym.value > 0)
            common_total += ROUNDUP8(sp->asym.value);
    return common_total;
}

static char *common_alloc(e, size)
    struct doload_environment *e;
    unsigned long size;
{
    char *ret_val;

    size = ROUNDUP8(size);

    if (size > e->common_size)
        doload_punt(e, "Internal error: out of common space.");

    ret_val = e->common_space;
    e->common_size -= size;
    e->common_space += size;
    
    return ret_val;
}

static char *gp_alloc(e, size)
    struct doload_environment *e;
    unsigned long size;
{
    char *ret_val;
 
    if (size > mips_GlobalSize)
        doload_punt(e, "Insufficient global area space to load module.");

    /* This is recorded so that space can be free'd in case of error. */
    if (e->gp_pool == NULL)
        e->gp_pool = mips_GlobalArea;

    /* Force 8 byte alignment if size is greater than 4. Yes, its a hack. */
    if (size > 4)
        ret_val = (char *) ROUNDUP4((unsigned long) mips_GlobalArea);
    else
        ret_val = (char *) ROUNDUP8((unsigned long) mips_GlobalArea);

    mips_GlobalSize -= (ret_val - mips_GlobalArea) + size;
    mips_GlobalArea = ret_val + size;

    return ret_val;
}

/* Used to specify different loading types for each section. */
#define FILL_IGNORE -1
#define FILL_READ 0
#define FILL_ZERO 1
#define FILL_MAX 1

#define WHERE_NORMAL 0
#define WHERE_GLOBAL 1
#define WHERE_MAX 1

struct doload_section_description {
    char *name;	/* Character string naming section. */
    int fill;	/* How to get contents of section. */
    int where;	/* Where to allocate memory for section. */
    int relSecNum;	/* Section number constant in relocation record. */
} sectionDescs[] = {
    _TEXT,	FILL_READ, WHERE_NORMAL, R_SN_TEXT,
    _RDATA,	FILL_READ, WHERE_NORMAL, R_SN_RDATA,
    _DATA,	FILL_READ, WHERE_NORMAL, R_SN_DATA,
    _SDATA,	FILL_READ, WHERE_GLOBAL, R_SN_SDATA,
    _SBSS,	FILL_ZERO, WHERE_GLOBAL, R_SN_SBSS,
    _LIT4,	FILL_READ, WHERE_GLOBAL, R_SN_LIT4,
    _LIT8,	FILL_READ, WHERE_GLOBAL, R_SN_LIT8,
    _BSS,	FILL_ZERO, WHERE_NORMAL, R_SN_BSS
};
    
/* read module into memory */

/* This routine reads all of the interesting sections of a object module into memory so they can be worked on by other parts of the code. Some of these sections are permanent while others are only sued during the loading phase and are freed afterwards. (List specific sections used by this code.) */
static doload_read(e)
    struct doload_environment *e;
{
    long stringlen;	/* length of string table */
    long sectionSizes[WHERE_MAX + 1];
    long totalRelocs = 0;
    long globalNeeded;
    struct doload_section *section;
    char *thisStart[WHERE_MAX + 1];

    /* read header */
    safe_read(e, (char *)&(e->filehdr), (long)sizeof e->filehdr);
    safe_read(e, (char *)&(e->aouthdr), (long)sizeof e->aouthdr);

    e->sections = (struct doload_section *) safe_malloc(e, e->filehdr.f_nscns *
                                                sizeof(struct doload_section));
    bzero(e->sections, e->filehdr.f_nscns * sizeof(struct doload_section));
    bzero(sectionSizes, sizeof(sectionSizes));

/* Sum sizes for sections so we can allocate space to laod them. */
    ENUMERATE_SECTIONS(section) {
        struct scnhdr *tempSection = &section->header;
        struct doload_section_description *thisDesc;

        safe_read(e, (char *)tempSection, (long)sizeof(*tempSection));
        for (thisDesc = sectionDescs; thisDesc < sectionDescs +
             (sizeof(sectionDescs) / sizeof(sectionDescs[0]))
             && strcmp(tempSection->s_name, thisDesc->name) != 0; thisDesc++)
            ;

        if (thisDesc < sectionDescs +
             (sizeof(sectionDescs) / sizeof(sectionDescs[0]))) {
            sectionSizes[thisDesc->where] += tempSection->s_size;
            totalRelocs += tempSection->s_nreloc;
            section->fill = thisDesc->fill;
            section->where = thisDesc->where;
	    e->relocSection[thisDesc->relSecNum] = section;
        }
        else if (tempSection->s_size != 0) {

            char buffer[200];

            sprintf(buffer, "doload: Unexpected non-zero section named %.100s encountered.", tempSection->s_name);
            doload_punt(e, buffer);
        }
        else
            section->fill = FILL_IGNORE;
    }

    if (strcmp(e->sections[0].header.s_name, _TEXT) != 0 ||
        e->sections[0].header.s_size == 0)
        doload_punt(e, "Missing or empty text section!");


    read_section(e, e->filehdr.f_symptr, &e->symheader, sizeof(e->symheader));
    e->symtab = (pEXTR) read_section(e, e->symheader.cbExtOffset, NULL,
                                     e->symheader.iextMax * sizeof(EXTR));
    e->stringtab = (char *) read_section(e, e->symheader.cbSsExtOffset, NULL,
                                         e->symheader.issExtMax);

    e->common_size = common_space(e);
    e->total_size = ROUNDUP8(sectionSizes[WHERE_NORMAL]) + e->common_size;
    e->text = safe_malloc(e, ROUNDUP8(sectionSizes[WHERE_NORMAL]) +
                             e->common_size);
    e->common_space = e->text + ROUNDUP8(sectionSizes[WHERE_NORMAL]);
    thisStart[WHERE_NORMAL] = e->text;
    thisStart[WHERE_GLOBAL] = gp_alloc(e, sectionSizes[WHERE_GLOBAL]);

/* Load per-section data. */
    ENUMERATE_SECTIONS(section) {
        long size;
        int where;
        unsigned long file_offset = section->header.s_relptr;

        if (section->fill == FILL_IGNORE)        
            continue;

        size = section->header.s_size;
        where = section->where;
        if (section->fill == FILL_READ)
            read_section(e, section->header.s_scnptr,
                         thisStart[where],
                         size);
        else
            bzero(thisStart[where], size);
        section->contents = thisStart[where];
        thisStart[where] += size; 

/* Get relocation info for each section.
 * Must handle case where number of relocations wouldn't fit in the short some
 * bozo defined for COFF to hold the number of relocations in...
 */
        if (section->header.s_flags & S_NRELOC_OVFL) {
            struct reloc tempReloc;

            read_section(e, file_offset, &tempReloc, sizeof(tempReloc));
            file_offset += sizeof(tempReloc);
            section->num_relocs = tempReloc.r_vaddr;
        }
        else
            section->num_relocs = section->header.s_nreloc;

        if (section->num_relocs != 0)
            section->rtab =
                (struct reloc *) read_section(e, file_offset, NULL, 
                                              section->num_relocs *
                                              sizeof(struct reloc));
    }
}

/* read and relocate module
 * Returns modules entrypoint or NULL if error.
 */
void *(* doload(inFD, name, bp, lenP, path) )()
    int inFD;			/* open fd for package file */
    char *name;			/* name of package being loaded */
    char **bp;			/* base address of package */
    long *lenP;			/* size of text segment */
    char *path;			/* Pathname of package being loaded */
				/* Path is used by the MACH loader, not this one */
{
    struct doload_environment E;
    register struct doload_environment *e = &E;
    unsigned long n;	/* number of relocation items */
    struct reloc *rp;
    struct doload_section *section;

    /* set up environment */
    doload_setup(e, inFD, Load);
    if (setjmp(e->errorJump)) {
	doload_cleanup(e);
	return NULL;
    }

    /* read module into memory */
    doload_read(e);

    /* Get addresses for known global symbols. Errors for unresolved */
    /* externals show up here. */
    if (e->symheader.iextMax != 0)
        doload_finish_resolution(e);

    /* Relocate each section in turn. Sum section sizes as side effect. */
    ENUMERATE_SECTIONS(section)
        relocate_section(e, section);

    /* all done */
    if (doload_trace)
	printf( " %s: text = 0x%.8x  data = 0x%.8x  entry = 0x%.8x\n",
		name, e->text, e->sections[1].contents, e->text +
               e->aouthdr.entry - e->aouthdr.text_start);

    if (bp != NULL)
        *bp = e->text;
    if (lenP != NULL)
        *lenP = e->total_size;

#if 0
    if (cacheflush(e->text, e->total_size, BCACHE) != 0) {
#else
/* Ultrix 3.0 does not have cacheflush in libc.a, however, the kernel on my
 * machine implements it and it is needed to make doindex work...
 */
#include <sys/syscall.h>
#include <sys/sysmips.h>

    if (syscall(SYS_sysmips, MIPS_CACHEFLUSH, e->text, e->total_size,
                BCACHE, 0) == -1) {
#endif
        perror("doload - cacheflush");
        doload_punt(e, "Could not flush caches in doload.");
    }

    doload_cleanup_success(e);

    return (void *(*)()) (e->text + e->aouthdr.entry - e->aouthdr.text_start);
}

/* Lookup table used to give useful information in error messages. This
 * table comes from /usr/include/symconst.h.
 */
static char *doload_symbolClasses[] = {
    "scNil",
    "scText",		/* text symbol */
    "scData",		/* initialized data symbol */
    "scBss",		/* un-initialized data symbol */
    "scRegister",	/* value of symbol is register number */
    "scAbs",		/* value of symbol is absolute */
    "scUndefined",	/* who knows? */
    "scCdbLocal",	/* variable's value is IN se->va.?? */
    "scBits",		/* this is a bit field */
    "scCdbSystem or scDbx",    /* variable's value is IN CDB's address space */
                               /* overlap dbx internal use */
    "scRegImage",	/* register value saved on stack */
    "scInfo",		/* symbol contains debugger information */
    "scUserStruct",	/* address in struct user for current process */
    "scSData",		/* load time only small data */
    "scSBss",		/* load time only small common */
    "scRData",		/* load time only read only data */
    "scVar",		/* Var parameter (fortran,pascal) */
    "scCommon",		/* common variable */
    "scSCommon",	/* small common */
    "scVarRegister",	/* Var parameter in a register */
    "scVariant",	/* Variant record */
    "scSUndefined",	/* small undefined(external) data */
    "scInit",		/* .init section symbol */
    "scBasedVar",	/* Fortran or PL/1 ptr based var */ 
    "scMax",
};

static char *symbol_class_name(class_number)
    int class_number;
{
    if (class_number < (sizeof(doload_symbolClasses) / sizeof(doload_symbolClasses[0])))
        return doload_symbolClasses[class_number];
    else
	return "<NOT IN TABLE>";
}

/* This routine rolls down the symbol table and finds those symbols which
 * also appear in the globals array. When it finds one, it fills in the
 * value with that from globals. It is an error to have undefined symbols
 * after this routine executes. doload_finish_resolution also allocates space
 * for common symbols.
 */
static doload_finish_resolution(e)
    struct doload_environment *e;
{
    register pEXTR sp;
    register pEXTR sbound;

    for (sp = e->symtab; sp < e->symtab + e->symheader.iextMax; sp++) {
 	if (sp->asym.sc == scUndefined || sp->asym.sc == scSUndefined ||
            sp->asym.sc == scCommon || sp->asym.sc == scSCommon) {
            char *name = e->stringtab + sp->asym.iss;
            struct globaltab *thisGlobal;

	    if ((thisGlobal = find_global(name)) != NULL)
		sp->asym.value = thisGlobal->entrypoint;
	    else if ((sp->asym.sc == scCommon ||
                      sp->asym.sc == scSCommon) &&
                     sp->asym.value > 0) {
                unsigned long length = sp->asym.value;

		if (sp->asym.sc == scSCommon)
		    sp->asym.value = (unsigned long) gp_alloc(e, length);
		else
		    sp->asym.value = (unsigned long) common_alloc(e, length);
                bzero(sp->asym.value, length);
	    }
	    else {
		fprintf(stderr, "doload:  Undefined symbol: %s, symbol class %s.\n", name, symbol_class_name(sp->asym.sc));
		e->problems++;
	    }
            sp->asym.sc = scAbs;
        }
    }
}

/* compute relocation adjustment */
static long adjust(e, tw, rp)
    struct doload_environment *e;
    long tw;
    struct reloc *rp;
{
    if (rp->r_extern) {
        pEXTR sp = e->symtab + rp->r_symndx;
        char *np = e->stringtab + sp->asym.iss;
        struct doload_section *section;

/* Maybe do this with a map array instead. */
#define SECTION_ADJ(sec_num) \
        section = e->relocSection[sec_num]; \
        if (section == NULL) \
            doload_punt(e, "External relocation for non-exisitient section.");\
        else \
            tw += sp->asym.value + (long) section->contents - \
                section->header.s_vaddr; \
        break;

	switch (sp->asym.sc) {
          case scAbs:
            tw += sp->asym.value;
            break;
          case scText:
            SECTION_ADJ(R_SN_TEXT);
          case scData:
            SECTION_ADJ(R_SN_DATA);
          case scBss:
            SECTION_ADJ(R_SN_BSS);
          case scRData:
            SECTION_ADJ(R_SN_RDATA);
          case scSData:
            SECTION_ADJ(R_SN_SDATA);
          case scSBss:
            SECTION_ADJ(R_SN_SBSS);
          case scUndefined:
          case scCommon:
            doload_punt(e, "Internal botch - common and undefined symbols should have been resolved in doload_preset"); 
            break;
          default:
            {
                char message[120];

                sprintf(message,
                    "Unexpected class (%.20s) in relocation for symbol %.50s .",
                        symbol_class_name(sp->asym.sc), np);
                doload_punt(e, message);
            }
            break;
        }
    } /* endif IS_RP_EXTERN( rp ) */
    else {
        struct doload_section *section;

	if (rp->r_symndx > 0 && rp->r_symndx < NUM_RELOC_SECTS &&
	    (section = e->relocSection[rp->r_symndx]) != NULL)
            tw += (long) section->contents - section->header.s_vaddr;
        else
            doload_punt(e, "Bad section number in non-external relocation entry");
    } /* end else */

    return tw;
}

/* relocate one item */

#define HI16MASK    0xffff0000
#define LOW16MASK   0x0000ffff
#define LOW26MASK       0x03ffffff
#define HI6MASK     0xfc000000

#ifdef MIPSEB /* BIG ENDIAN */
#define GETLOWHALF(addr) (*(((short *)(addr)) + 1))
#define SETLOWHALF(addr, value) ((*(((short *)(addr)) + 1)) = (value))
#else /* LITTLE ENDIAN */
#define GETLOWHALF(addr) (*(((short *)(addr))))
#define SETLOWHALF(addr, value) ((*(((short *)(addr)))) = (value))
#endif /* Byte order selection */

static void relocate_section(e, section)
    register struct doload_environment *e;
    struct doload_section *section;
{
    struct reloc *rp;
    struct reloc *sectionRelocBound = section->rtab + section->num_relocs;
    long offset = (long) section->contents - section->header.s_vaddr;

    for (rp = section->rtab; rp < sectionRelocBound; rp++) {
        long relocAddr = rp->r_vaddr + offset;
        unsigned long tw;
    
        switch (rp->r_type) {
            case R_REFHALF:
                tw = *(short *)relocAddr;
                tw = adjust(e, tw, rp);
                if (tw > 32767 || tw < -32768)
                    doload_punt(e, "R_REFHALF reloaction out of range.");
                *(short *)relocAddr = tw;
                break;
            case R_REFWORD:
                tw = *(long *)relocAddr;
                tw = adjust(e, tw, rp);
                *(long *)relocAddr = tw;
                break;
            case R_JMPADDR:
                tw = (*(long *)relocAddr & LOW26MASK) << 2;
/* Sign extend if external, otherwise make relative to relocation address. */
                if (rp->r_extern) {
                    if (tw & 0x08000000)
/* I don't know why this is 0xfc instead of 0xf0 but thats what the MIPS ld
 * code does...
 */
                        tw |= 0xfc000000;
                }
                else
                    tw |= (rp->r_vaddr + 4) & 0xf0000000;
                tw = adjust(e, tw, rp);
                if ((tw & 0xf0000000) != (((long) relocAddr + 4) & 0xf0000000))
                    doload_punt(e, "JUMPADDR relocation overflow.");
                *(long *)relocAddr = (*(long *)relocAddr & HI6MASK) +
                                     ((tw >> 2) & LOW26MASK);
                break;
            case R_REFHI:
                if (rp < sectionRelocBound) {
                    struct reloc *nextRp = rp + 1;

                    if (nextRp->r_type == R_REFLO) {
                        short highHalf; /* High 16 bits of final value. */
                        short lowHalf; /* Low 16 bits of final value. */
                        long nextRelocAddr = nextRp->r_vaddr + offset;
                    
/* I assume that the low half of this will be appropriately sign extended. */
                        tw = (GETLOWHALF(relocAddr) << 16) +
                             GETLOWHALF(nextRelocAddr);
                        tw = adjust(e, tw, rp);
                        lowHalf = tw & LOW16MASK;
                        highHalf = tw >> 16;
                        if (lowHalf < 0)
                            ++highHalf;
                        SETLOWHALF(relocAddr, highHalf);
/* Do second relocation since we have the value. Advance loop counter so
 * it doesn't get done again.
 */
                        SETLOWHALF(nextRelocAddr, lowHalf);
                        rp = nextRp;
                    }
                    else
                        doload_punt(e, "R_REFHI relocation record not followed by REFLO.");
                }
                else
                    doload_punt(e, "R_REFHI relocation record at end of relocations.");
                break;
            case R_REFLO: {
                short lowHalf;
                
                tw = GETLOWHALF(relocAddr);
                tw = adjust(e, tw, rp);
                lowHalf = tw & LOW16MASK;
                SETLOWHALF(relocAddr, lowHalf);
                break;
            }
            case R_LITERAL:
            case R_GPREL: {
                extern int _gp; /* MIPS' ld defines this label at the address in the gp register */
                long gpOffset;
                short lowHalf;
                
                tw = GETLOWHALF(relocAddr);
                if (!rp->r_extern)
                    tw += e->aouthdr.gp_value;
                tw = adjust(e, tw, rp);
                gpOffset = tw - (long)&_gp;
                if (gpOffset > 32767 || gpOffset < -32768)
                    doload_punt(e, "External R_GPREL/R_LITERAL relocation does not result in a gp addresable value.");
                lowHalf = (short)gpOffset;
                SETLOWHALF(relocAddr, lowHalf);
                break;
            }
#ifdef OLD
            case R_LITERAL:
/* If this ever happens, I think the code for R_GPREL will work for
 * literals too. Just put this case label under the one above for R_GPRREL
 * and try it...
 */
                doload_punt(e, "R_LITERAL relocation in doload_relocate.");
                break;
#endif /* OLD */
        }
    }
}
