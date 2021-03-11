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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/amsconf.c,v 2.8 1992/12/15 21:03:27 rr2b R6tape $";
#endif

/* amsconf.c -- Uses the /AndrewSetup file to see if all programs
	in the Andrew Message System should be run from an experimental
	binary directory 
*/

#include <andrewos.h>
#include <stdio.h>
#include <sys/param.h>
#include <errprntf.h>

#define CALLMEMADAM "__AMS__EXP__"
#define LOOKFOR "AMS-Binaries"

amsconfig(argc, argv, name)
int argc;
char **argv, *name;
{
    char *s, ExpDir[1+MAXPATHLEN], NewName[100];

    CheckAMSConfiguration();
    if (!strncmp(argv[0], CALLMEMADAM, sizeof(CALLMEMADAM) -1)) {
	return(0); /* Do not recurse */
    }
    if ((s = (char *) GetConfiguration(LOOKFOR)) == NULL) {
	return(1);
    }
    if (!*s) {
	return(0);
    }
    /* There really is an entry in the configuration file */
    errprintf(name, ERR_WARNING, 0, 0, "Running experimental %s from %s", name, s);
    sprintf(ExpDir, "%s/%s", s, name);
    sprintf(NewName, "%s%s", CALLMEMADAM, name);
    argv[0] = NewName;
    execv(ExpDir, argv);
    errprintf(name, ERR_WARNING, 0, 0, "Exec failed -- running wrong version");
    return(2);
}

