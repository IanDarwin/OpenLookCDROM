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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sun_sparc_51/RCS/doload.c,v 1.1 1993/07/15 15:37:12 rr2b Exp $";
#endif


/* 
	Replacement for doload.c for the SunOS 4.1/S5R4 dynamic loader.
 */

#include <stdio.h>
#include <dlfcn.h>
#include <string.h>

/*
 * SunOS 4.1 doesn't have any #defines for the second argument to
 * "dlopen()"; it just says "it has to be 1".  S5R4 has defines for
 * RTLD_LAZY, which makes it act the way SunOS 4.1 works, and RTLD_NOW,
 * which makes it bind symbols at "dlopen()" time.  Thus, we define
 * RTLD_LAZY as 1 (which is what it's defined as in S5R4) if it's not
 * defined (as is the case in SunOS 4.1).
 */
#ifndef RTLD_LAZY
#define	RTLD_LAZY	1
#endif

/* This should be removed, but the machine independent code declares
 * doload_Extension external expecting it to be defined in the machdep
 * code. I suppose one might want to set this to something other than ".do"
 * (like ".so") to conform to the SunOS 4.x/S5R4 convention but for now
 * we will do it the way it has always been done.
 */
char doload_extension[] = ".do";

int doload_trace=0;		/* nonzero if debugging */

/* doload: Load a dynamic object.
 *
 * Basically, this just calls the dynamic loader and relies on the
 * system to do the right thing.
 */
char *doload(inFD, name, bp, lenP, path) /* return pointer to entry point, */
				/* or NULL if error */
/* UNUSED */ int inFD;		/* open fd for package file. */
char *name;			/* name of package being loaded */
char **bp;			/* base address of package */
long *lenP;			/* size of text segment */
char *path;			/* Pathname of package being loaded */
{
    char *dummy;
    void *handle;
    char *EntryPoint = NULL;
    char epname[1024];	/* XXX - allocate dynamically? */
    register char *p;
        
/* Doindex is going to free the memory associated with a dynamic object
 * before loading another dynamic object to prevent memory bloat.
 *
 * Therefore this routine fakes something for doindex to free. Of course
 * the correct way to do this is to add a routine to the doload interface to
 * unload a dynamic object. (See the commented out doload_unload below.)
 * If doindex and all the doload files for other machines are ever fixed,
 * this code can be eliminated.
 *
 * Note that the loss of correct bp and len info is not to big a deal since
 * it is mostly just used for debugging; SunOS 4.1 doesn't do this very
 * well, but hopefully either S5R4 does it reasonably or will do so in the
 * future.
 */
    dummy = (char *) malloc(1);
    if (dummy == NULL)
        return NULL;
    *bp = dummy;
    *lenP = 1;

    /*
     * We assume class_Error and class_RoutineStruct are in the
     * dynamic-loading symbol table of the executable.
     */

    /*
     * Load package
     */
    handle = dlopen(path, RTLD_LAZY);
    if ( handle == NULL )
    {
        fprintf( stderr, "doload: Error loading package \"%s\" - %s\n", path, dlerror() );
        return NULL;
    }
        
    /*
     * Construct name of GetClassInfo symbol from the package name
     * by stripping off the suffix and appending "__GetClassInfo".
     * You can't get the entry point value from a shareable object
     * from the "dl*()" routines, which is why we have to do this
     * stuff.
     * We strip off the suffix because "doindex", curse its soul,
     * hands us the last component of the name of the *file* as
     * the package name, suffix and all.
     */
    (void) strcpy(epname, name);
    p = strrchr(epname, '.');
    if (p == NULL)
	p = epname + strlen(epname);
    (void) strcpy(p, "__GetClassInfo");

    /*
     * Return entry point
     */
    EntryPoint = (char *)dlsym( handle, epname );
    if ( EntryPoint == NULL )
    {
        fprintf( stderr, "doload: Error finding entry point of package \"%s\" - %s\n", path, dlerror() );
        return NULL;
    }
    if ( doload_trace )
        printf(" %s: entry = 0x%.8x\n", name, EntryPoint);
        
    return( EntryPoint );
}

#if 0
/* doload_free: Free the resources associated with a dynamically loaded */
 * object.
 * The programmer is responsible for making sure that no pointers into the
 * storage associated with the dynamic object are held. On most systems,
 * this routine will simply do a "free(base);". This routine is provided
 * for doindex to call. It cannot safely be used in most situations.
 *
 * If nobody actually uses "base" for anything other than handing it to
 * "doload_free()", "doload()" should be changed to pass the handle gotten
 * from "dlopen()" back as the base, since "dlclose()" takes that handle as
 * an argument.
 */ 
doload_free(name, base)
    char *name;
    char *base; /* Not a string. */
{
    if (dlclose(base))
    {
        fprintf( stderr, "doload_free: Error unloading package \"%s\" - %s\n", name, dlerror() );
    }
}
#endif
