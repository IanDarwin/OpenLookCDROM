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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/titles.c,v 1.4 1992/12/15 21:10:49 rr2b R6tape $";
#endif

/* titles.c */

#include <stdio.h>
#include <util.h>

static char *BeginInitialArgv = NULL, *EndInitialArgv;

void SetInitialArgs(argc, argv, envp)
int argc; char **argv, **envp;
{
    int Ix;

    BeginInitialArgv = argv[0];
    Ix = 0;
    if (envp != NULL) {
	for (Ix = 0; Ix < 60; ++Ix) if (envp[Ix] == NULL) break;
    }
    if (Ix > 0) {
	EndInitialArgv = envp[Ix-1] + strlen(envp[Ix-1]);
    } else {
	EndInitialArgv = argv[argc-1] + strlen(argv[argc-1]);
    }
}

/*VARARGS1*/
void SetProcTitle(str, a1, a2, a3, a4, a5)
char *str;
{/* Set the process title. */
    char *cp;
    char Title[1500];

    if (BeginInitialArgv == NULL) return;   /* must call SetInitialArgs first */
    sprintf(Title, str, a1, a2, a3, a4, a5);
    strncpy(BeginInitialArgv, Title, EndInitialArgv - BeginInitialArgv - 1);
    EndInitialArgv[-1] = '\0';
    cp = &BeginInitialArgv[strlen(BeginInitialArgv)];
    while (cp < EndInitialArgv) *cp++ = ' ';
}

#ifdef TESTINGONLYTESTING
main(argc, argv)
int argc; char **argv;
{
    char Str[500];
    char *cp;

    SetInitialArgs(argc, argv, NULL);
    for (;;) {
	fputs("Title for process: ", stdout); fflush(stdout);
	cp = fgets(Str, sizeof(Str), stdin);
	if (cp == NULL) break;
	for (cp = &Str[strlen(Str)-1]; cp >= Str; --cp) if (*cp == '\n') *cp = '\0';
	SetProcTitle("%s", Str);
    }
    exit(0);
}
#endif /* TESTINGONLYTESTING */
