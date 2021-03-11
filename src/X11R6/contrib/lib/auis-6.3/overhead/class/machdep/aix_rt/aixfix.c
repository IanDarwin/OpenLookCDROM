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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/aix_rt/RCS/aixfix.c,v 1.5 1992/12/15 20:58:42 rr2b R6tape $";
#endif


/*
 * This program reads, from STDIN, a series of identifiers, one to a line.
 * Each one is converted to a line suitable for inclusion in the
 * globals.spp file.  This program is required in order to construct
 * Andrew on AIX since the assemblers are so different between
 * BSD and AIX.
 */

#include <stdio.h>


#ifdef i386

void Emit(cp)
char *cp;

{
    (void) printf("\t.globl %s ; .text; .long %s, 1f ; .data; 1: .string \"%s\" \n", 
		   cp, cp, cp);
    return;
}

#else

void Emit(cp)
char *cp;

{
static char Label[14] = "LXYZZY000000";
static int Count = 0;

    sprintf(Label + 6, "%d", Count++ );	/* bump label */

    (void) printf("\t.globl %s ; .text; .long %s, %s ; .data; %s: .byte ", 
		   cp, cp, Label, Label);
    while (*cp != 0) {
	(void) printf("%d, ", *cp);
	cp++;
    }
    (void) printf("0\n");
    return;
}

#endif




main()
{
static char Ident[1024];

#ifdef i386
    Emit("class_RoutineStruct");
    Emit("class_Error");
    Emit("errno");
    Emit("environ");

    Emit("class_NewObject");
    Emit("class_Load");
    Emit("class_IsLoaded");
    Emit("class_Lookup");
    Emit("class_IsType");
    Emit("class_IsTypeByName");
    Emit("class_EnterInfo");
    Emit("class_SetClassPath");
    Emit("class_PrependClassPath");
    Emit("class_GetEText");
    Emit("class_GetTextBase");
    Emit("class_GetTextLength");
#else
    Emit("_class_RoutineStruct");
    Emit("_class_Error");
    Emit("_errno");
    Emit("_environ");

    Emit("_class_NewObject");
    Emit("_class_Load");
    Emit("_class_IsLoaded");
    Emit("_class_Lookup");
    Emit("_class_IsType");
    Emit("_class_IsTypeByName");
    Emit("_class_EnterInfo");
    Emit("_class_SetClassPath");
    Emit("_class_PrependClassPath");
    Emit("_class_GetEText");
    Emit("_class_GetTextBase");
    Emit("_class_GetTextLength");

    Emit(".class_NewObject");
    Emit(".class_Load");
    Emit(".class_IsLoaded");
    Emit(".class_Lookup");
    Emit(".class_IsType");
    Emit(".class_IsTypeByName");
    Emit(".class_EnterInfo");
    Emit(".class_SetClassPath");
    Emit(".class_PrependClassPath");
    Emit(".class_GetEText");
    Emit(".class_GetTextBase");
    Emit(".class_GetTextLength");
#endif 

    while (scanf("%s", Ident) != EOF) {
	if (Ident[0] != 0) { /* ignore empty lines */
	    Emit(&Ident[0]);
	}
    }	    
    return 0 ;
}


