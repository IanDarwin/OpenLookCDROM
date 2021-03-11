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

/* 
	doload.h - environment for dynamic loader

	Author:  Zalman Stern July 1989
 */

#include <machine/syms.h>

/* here is the state during a load */

typedef enum doload_mode_enum {
	Load,			/* .. loading .do file */
	List,			/* .. listing .do or .o file */
	Fix			/* .. converting .o to .do file */
} doload_mode;


struct doload_section {
    struct scnhdr header;
    struct reloc *rtab;		/* relocation table */
    unsigned long num_relocs;
    char *contents;
/* Needed for temp variables in doload_read */
    int fill;
    int where;
};

#define ENUMERATE_SECTIONS(loop_variable) \
    for (loop_variable = e->sections; \
	 loop_variable < e->sections + e->filehdr.f_nscns; \
	 loop_variable++)

/* Unfortunately, there is no direct way to derrive this from the include
 * file. In fact, there are higher values than R_SN_LIT4, but they are not
 * defined on all systems I want this code to compile on. If you run into
 * a problem, feel free to change this.
 */
#define NUM_RELOC_SECTS (10)

struct doload_environment {
    doload_mode mode;		/* operating mode */
    int fd;			/* input file descriptor */
    jmp_buf errorJump;		/* error exit */
    int problems;		/* count of problems encountered */
    struct filehdr filehdr;	/* header at beginning of a.out file */
    struct aouthdr aouthdr;	/* Where the entry point resides among other things. */
    char *text;			/* text segment */
    unsigned long total_size;	/* Size of all sections contigous in */
				/* e->text. */
    char *gp_pool;		/* Pointer to space allocated in GP */
                                /* addresable area maintained for this */
                                /* process. */
    struct doload_section *sections; /* Info on sections loaded. */
/* Next field used to map from relocation record's section number to
 * doload's section info structure.
 */
    struct doload_section *relocSection[NUM_RELOC_SECTS];
    HDRR symheader;
    pEXTR symtab;		/* symbol table */
    char *stringtab;		/* string table */
    char *common_space;		/* Free space for common pool allocation. */
    unsigned long common_size;	/* Ammount of space left in pool. */
};

extern int doload_trace;
#define doload_extension ".do"

extern void *(*doload() )();
