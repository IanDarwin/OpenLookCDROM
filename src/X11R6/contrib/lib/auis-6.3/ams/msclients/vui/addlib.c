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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/addlib.c,v 2.7 1992/12/15 21:23:32 rr2b R6tape $";
#endif


 

/* Program to run on the PC that mimics the Unix ar command's syntax and 
generates the proper calls to the Microsoft library manager.  */
#ifdef IBMPC
#include <process.h>
#endif /* IBMPC */
#include <errno.h>
#include <signal.h>
#include <stdio.h>


main(argc, argv)
char **argv;
int argc;
{
char lib[256];
char *newlib, *nl2;
int rc = 0;
    argv++;
    strcpy(lib, "lib ");
/*  printf("on library %s\n", *argv); */
    strcat(lib, *argv++);
    while (*argv != NULL) {
       strcat(lib, " +");
       strcat(lib, *argv++);
    }
    strcat(lib, ";");
       rc = system(lib);
/*     printf("Exec: %s\n", lib); */
       if (rc) {
          switch (rc) {
          case ENOENT: printf("COMMAND.COM not found.\n"); break;
          case ENOMEM: printf("Insufficient memory.\n"); break;
          case E2BIG:  printf("Environment space > 32K\n"); break;
          case ENOEXEC: printf("COMMAND.COM has invalid format.\n"); break;
          }
       }
}
