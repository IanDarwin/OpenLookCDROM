/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	$Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $
*/

/* 
 * array of global entry points for dynamic loader
 *
 * Defines a table of entry points and their names
 *
 */

/* macros which actually define entry points */

/*   globalref - direct reference to global symbol x */

#define globalref(x)	.globl x \
                        .data \
                        $$x: \
                         .asciiz	"x" \
                        .text \
                	.word	x, $$x
.text

/*   routine - reference to routine x - _x and (for ibm032) _.x */

#define routine(x)	globalref(x)

	.globl	globals
globals: /* beginning of entry point table */

/* from libclass */
	globalref(class_RoutineStruct)
	globalref(class_Error)

	routine(class_NewObject)
	routine(class_Load)
	routine(class_IsLoaded)
	routine(class_Lookup)
	routine(class_IsType)
	routine(class_IsTypeByName)
	routine(class_EnterInfo)
	routine(class_SetClassPath)
	routine(class_PrependClassPath)
	routine(class_GetEText)
	routine(class_GetTextBase)
	routine(class_GetTextLength)


/* common symbols referred to but not defined directly in libc.a */

	globalref(environ)		/* common symbol, defined nowhere */
	globalref(errno)		/* cerror */

/* do not delete the following line - make depends on it */

#include	<globalrefs._h>

	.text

	.globl	globalend
globalend:    .space 0

/* special cases for missing entry points under various machine types */

/* end of globals.s */
