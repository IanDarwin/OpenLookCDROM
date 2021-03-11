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

#define nlist nlist_

typedef enum doload_mode_enum {
	Load,			/* .. loading .do file */
	List,			/* .. listing .do or .o file */
	Fix			/* .. converting .o to .do file */
} doload_mode;

struct doload_environment {
    doload_mode mode;		/* operating mode */
    int fd;			/* input file descriptor */
    jmp_buf errorJump;		/* error exit */
    int problems;		/* count of problems encountered */
    struct exec header;		/* header at beginning of a.out file */
    char *text;			/* text segment */
    char *data;			/* data segment */
    struct r_info *rtab;		/* relocation table */
    struct nlist *symtab;	/* symbol table */
    char *stringtab;		/* string table */
    struct nlist *newsym;	/* replacement symbol table */
    int newsymcount;		/* number of new symbols */
    int newsymsize;		/* size of new symbol table */
    struct nlist **stab_entries; /* list of pointers to symbol table entries */
    int alloced_stab_entries,     /* number of slots malloced */
        filled_stab_entries;      /* number of slots filled */
};



extern int doload_trace;

#define doload_extension ".do"

extern void *(*doload() )();

#define maxString 256 /* max size string expected in a symbol table entry */
#define symSize(pSym) \
  (sizeof *pSym + pSym->n_length) /* struct size + string length (NO TRAILING ZERO!) */
#define nextSym(pSym) \
  (struct nlist *)((char *)pSym + symSize(pSym))
#define symString(pSym) \
  ((char *)pSym + sizeof *pSym)
#define stabStrCmp(str, pSym) \
  ((pSym->n_length == strlen(str)) ? \
   strncmp(symString(pSym), str, pSym->n_length) \
   : -1)
#define stabStrCpy(str, pSym) \
  { \
  bcopy(symString(pSym), str, pSym->n_length); \
  str[pSym->n_length] = '\0'; \
  }

