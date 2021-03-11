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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/openpipe.c,v 2.9 1992/12/15 21:20:00 rr2b R6tape $";
#endif

#include <stdio.h>
#include <andrewos.h>
#include <ms.h>
#include <mailconf.h>

extern char *getenv();
extern FILE *popen();

MS_OpenDebuggingPipescript(DoIt) 
int DoIt;  /* Turns it on or off */
{
    static FILE *ftmp = NULL;

    if (DoIt == 2) {
	char *TypescriptVector[2];
	char TypescriptCommand[100];

	sprintf(TypescriptCommand, AndrewDir("/bin/typescript"));
	TypescriptVector[0] = TypescriptCommand;
	TypescriptVector[1] = NULL;
	if (! osi_vfork()) {
	    int fd;

	    /* I am a child */
	    for (fd = getdtablesize(); fd > 2; --fd) close(fd);
	    execv(TypescriptVector[0], TypescriptVector);
	    NonfatalBizarreError("Could not exec typescript");
	    _exit(1);
	}
	return(0);
    }
    if (DoIt) {
	char PipescriptCommand[100];
	sprintf(PipescriptCommand, AndrewDir("/bin/pipescript -t 'MessageServer Debugging'"));
	if (ftmp) pclose(ftmp);
	ftmp = popen(PipescriptCommand, "w");
	if (ftmp == NULL) {
	    AMS_RETURN_ERRCODE(errno, EIN_POPEN, EVIA_OPENDEBUGGINGPIPESCRIPT);
	}
	if (dup2(fileno(ftmp), 1) == -1) {
	    AMS_RETURN_ERRCODE(errno, EIN_DUP2, EVIA_OPENDEBUGGINGPIPESCRIPT);
	}
    } else {
	if (ftmp) {  /* No need to do it if ftmp is null */
	    pclose(ftmp);
	    dup2(1, 2);
	    ftmp = NULL;
	}
    }
    return(0);
}
