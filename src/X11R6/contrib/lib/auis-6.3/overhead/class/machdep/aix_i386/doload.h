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
	Load,                   /* .. loading .do file */
	List,                   /* .. listing .do or .o file */
	Fix                     /* .. converting .o to .do file */
} doload_mode;

#define DOLOAD_MAXSCN 8         /* max number of sections */

struct doload_environment {
    doload_mode mode;           /* operating mode */
    int fd;                     /* input file descriptor */
    jmp_buf errorJump;          /* error exit */
    int problems;               /* count of problems encountered */
    FILHDR filehdr;             /* COFF file header */
    AOUTHDR aouthdr;            /* auxiliary header */
    SCNHDR scn[DOLOAD_MAXSCN];  /* section headers */
    char *segptr[DOLOAD_MAXSCN];/* segment address */
    RELOC *rtab;                /* relocation table */
    SYMENT *symtab;             /* symbol table */
    char *stringtab;            /* string table */
    SYMENT *newsym;             /* replacement symbol table */
    int newsymcount;            /* number of new symbols */
};


extern int doload_trace;

#define doload_extension ".do"

extern void *(*doload() )();
