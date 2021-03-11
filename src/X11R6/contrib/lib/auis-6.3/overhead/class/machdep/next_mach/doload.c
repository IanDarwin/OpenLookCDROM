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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/next_mach/RCS/doload.c,v 1.4 1992/12/15 20:59:27 rr2b R6tape $";
#endif


#include <stdio.h>
#include <andrewos.h>

#include <sys/loader.h>
#include <ldsyms.h>
#include <rld.h>
#include <streams/streams.h>
extern long globals;
long etext=(long)&globals;

static NXStream *err=NULL;

static char *files[]={
	NULL,
	NULL
};


int doload_trace=0;		/* nonzero if debugging */

static char entrynamesuffix[]="__GetClassInfo";

/* read and relocate module */
void *(* doload(inFD, name, bp, lenP, path) )()
/* or NULL if error */
int inFD;			/* open fd for package file */
char *name;			/* name of package being loaded */
char **bp;			/* base address of package */
long *lenP;			/* size of text segment */
char *path;			/* Pathname of package being loaded */
/* Path is used by the MACH loader (ie this one) */
{
    char entryname[256];
    void *(*entry)();
    char *p, *debugdir, debugpath[1024];
    if(doload_trace && (debugdir=(char *)getenv("DYNDEBUGDIR"))) {
	strcpy(debugpath, debugdir);
	strcat(debugpath,"/");
	strcat(debugpath, name);
    }	
    strcpy(entryname,"_");
    strncat(entryname, name, 254);
    p=index(entryname,'.');
    if(!p) p=entryname+strlen(entryname);
    if(strlen(entryname)+sizeof(entrynamesuffix)<255) strcpy(p, entrynamesuffix);
    if(!err) err=NXOpenFile(fileno(stderr), NX_WRITEONLY);
    if(doload_trace) printf("dynamic object path:%s\n", path);
    files[0]=path;
    if(!rld_load(err, NULL, files, (doload_trace&&debugdir)?debugpath:NULL)) {
	NXPrintf(err, "Failed to dynamically load %s.\n", path);
	(void) NXFlush(err);
	return NULL;
    }
    if(doload_trace) printf("looking up %s\n",entryname);
    if(!rld_lookup(err, entryname, (long *)&entry)) {
	NXFlush(err);
	return NULL;
    }
    (void) rld_unload_all(NULL, 0);
    if(doload_trace) printf("addr of entry point: %lx\n", (long *)entry);
    if(bp) *bp=(char *)malloc(8);
    if(lenP) *lenP=8;
    return entry;
}







