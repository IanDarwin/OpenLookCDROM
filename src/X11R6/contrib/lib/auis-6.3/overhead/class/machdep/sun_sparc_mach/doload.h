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

	Author:  John H Howard - April 9, 1987
 */



/* here is the state during a load */

typedef enum doload_mode_enum {
	Load,			/* .. loading .do file */
	List,			/* .. listing .do or .o file */
	Fix			/* .. converting .o to .do file */
} doload_mode;

#if defined(sun) && defined(sparc)
#define relocation_info reloc_info_sparc
#define r_symbolnum r_index
#endif

struct doload_environment {
    doload_mode mode;		/* operating mode */
    int fd;			/* input file descriptor */
    jmp_buf errorJump;		/* error exit */
    int problems;		/* count of problems encountered */
    struct exec header;		/* header at beginning of a.out file */
    char *text;			/* text segment */
    char *data;			/* data segment */
    struct relocation_info *rtab; /* relocation table */
    struct nlist *symtab;	/* symbol table */
    char *stringtab;		/* string table */
    struct nlist *newsym;	/* replacement symbol table */
    int newsymcount;		/* number of new symbols */
};

extern int doload_trace;

#define doload_extension ".do"

extern void *(*doload() )();

/*
 * Format of a relocation datum.
 */

/*
 * Sparc relocation types
 */
 
enum reloc_type
{
	RELOC_8,	RELOC_16,	RELOC_32,	/* simplest relocs    */
	RELOC_DISP8,	RELOC_DISP16,	RELOC_DISP32,	/* Disp's (pc-rel)    */
	RELOC_WDISP30,	RELOC_WDISP22,			/* SR word disp's     */
	RELOC_HI22,	RELOC_22,			/* SR 22-bit relocs   */
	RELOC_13,	RELOC_LO10,			/* SR 13&10-bit relocs*/
	RELOC_SFA_BASE,	RELOC_SFA_OFF13,		/* SR S.F.A. relocs   */
	RELOC_BASE10,	RELOC_BASE13,	RELOC_BASE22,	/* base_relative pic */
	RELOC_PC10,	RELOC_PC22,			/* special pc-rel pic*/
	RELOC_JMP_TBL,					/* jmp_tbl_rel in pic */
	RELOC_SEGOFF16,					/* ShLib offset-in-seg*/
	RELOC_GLOB_DAT, RELOC_JMP_SLOT, RELOC_RELATIVE, /* rtld relocs        */
};

/*
 * Format of a relocation datum.
 */
 
struct reloc_info_sparc	/* used when header.a_machtype == M_SPARC */
{
	unsigned long int r_address;	/* relocation addr (offset in segment)*/
	unsigned int	r_index   :24;	/* segment index or symbol index      */
	unsigned int	r_extern  : 1;	/* if F, r_index==SEG#; if T, SYM idx */
	int			  : 2;	/* <unused>			      */
	enum reloc_type r_type    : 5;	/* type of relocation to perform      */
	long int	r_addend;	/* addend for relocation value	      */
};
