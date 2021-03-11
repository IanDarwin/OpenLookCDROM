/*
 *      Copyright BellSouth 1991 - All Rights Reserved
 *      For full copyright information see:'andrew/config/COPYRITE.bls'.
 */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/hp_pa_risc/RCS/doload.c,v 1.6 1992/12/15 20:59:27 rr2b R6tape $";
#endif


/* 
 *	doload.c - dynamic loader for class system
 *          for HP 9000/800's running HPUX 7.0
 *      Written by David Anderson
 *	Thu 7 March 1991
 *
 *      Loosely based on Zalman's RS6000 version.
 */

#include <andrewos.h>
#include <stdio.h>
/* #include <sys/types.h> */
#include <sys/param.h> /* For MAXPATHLEN */
#include <stddef.h> /* For NULL and the like. */
#include <errno.h>
#include <andrdir.h>

#define TRUE 1
#define FALSE 0

/* This should be removed, but the machine independent code declares
 * doload_Extension external expecting it to be defined in the machdep
 * code. I suppose one might want to set this to something other than ".do"
 * but for now we will do it the way it has always been done.
 */
char doload_extension[] = ".do";

int doload_trace=0;		/* nonzero if debugging */

/* doload: Load a dynamic object.
 *
 * Basically this just uses HP's dynamic loader and relies on it
 * to do the right thing.
 */

char *doload(inFD, name, bp, lenP, path) /* return pointer to entry point, */
                                         /* or NULL if error */
/* UNUSED */ int inFD;			 /* open fd for package */
char *name;  		/* name of package being loaded */
char **bp;              /* base address of package */
long *lenP;		/* size of text segment */
char *path;		/* Pathname of package being loaded */
{
    static int firstTime = TRUE;
    int status;
    long i;
    char *entry;
    char *dummy;
    char entryname[256];
    char libpath[MAXPATHLEN];

/* In theory, one might be able to provide the correct values for bp and len,
 * but doindex is going to free the memory associated with a dynamic object
 * before loading another dynamic object to prevent memory bloat.
 *
 * Therefore this routine fakes something for doindex to free. Of course
 * the correct way to do this is to add a routine to the doload interface to
 * unload a dynamic object. (See the commented out doload_unload below.)
 * If doindex and all the doload files for other machines are ever fixed,
 * this code can be eliminated.
 *
 * Note that the loss of correct bp and len info is not to big a deal since
 * it is mostly just used for debugging.
 */
    dummy = (char *) malloc(1);
    if (dummy == NULL)
        return NULL;
    *bp = dummy;
    *lenP = 1;

    if (firstTime) {
        firstTime = FALSE;
        status = _init_loader();
        if (status != 0) {
            fprintf(stderr, "doload: can't init loader\n");
            return NULL;
        }

	if (define_symbols() != 0) {
	    fprintf(stderr, "doload: error defining globals\n");
	    return NULL;
	}

        /* Load millicode */
        sprintf(libpath, "%s/lib/milli.o", QUOTED_DEFAULT_ANDREWDIR_ENV);
        status = _load_file(libpath);
        if (status != 0) {
            fprintf(stderr, "doload: error loading %s\n", libpath);
            return NULL;
        }

        /* Load classproc */
        sprintf(libpath, "%s/lib/classproc.a", QUOTED_DEFAULT_ANDREWDIR_ENV);
	_declare_undefined_symbol("class_RoutineStruct");
	_declare_undefined_symbol("ClassEntry0");
        status = _load_file(libpath);
        if (status != 0) {
            fprintf(stderr, "doload: error loading %s\n", libpath);
            return NULL;
        }

#ifdef ANDREW_MALLOC_ENV
        /* Load malloc */
	_declare_undefined_symbol("malloc");
        sprintf(libpath, "%s/lib/libmalloc.a", QUOTED_DEFAULT_ANDREWDIR_ENV);
        status = _load_file(libpath);
        if (status != 0) {
            fprintf(stderr, "doload: error loading from %s\n", libpath);
        }
#endif /* ANDREW_MALLOC_ENV */
    }

    status = _load_file(path);
    if (status != 0) {
        fprintf(stderr, "doload: error loading %s from %s\n", name, path);
        return NULL;
    }

    for (i=0; name[i] != '\0' && name[i] != '.'; i++) {
	entryname[i] = name[i];
    }
    entryname[i] = '\0';
    strcat(entryname, "__GetClassInfo");
    entry = (char *) _get_symbol_value(entryname);
    if (entry == NULL) {
        fprintf(stderr, "doload: %s is undefined\n", entryname);
        return NULL;
    }

    if (doload_trace > 0) {
	fprintf(stderr, "doload: loaded %s from %s, entry: %lx\n",
		name, path, entry);
    }

    if (doload_trace > 0 && _get_unsats(NULL) != NULL) {
	char **symbol = NULL;

	while ((symbol = _get_unsats(symbol)) != NULL) {
	    fprintf(stderr, "doload: %s will be loaded from libc\n", *symbol);
	}
    }

    if (_get_unsats(NULL) != NULL) {
        status = _search_library("/lib/libc.a");
        if (status != 0) {
            fprintf(stderr, "doload: error loading from /lib/libc.a\n");
        }
    }

    if (doload_trace > 1) dump_all(doload_trace - 1);

    if (_get_unsats(NULL) != NULL) {
	char **symbol = NULL;

	while ((symbol = _get_unsats(symbol)) != NULL) {
	    fprintf(stderr, "doload: the symbol %s is unsatisfied\n", *symbol);
	}
    }

#if 0
    {
	char *name, *module;
	long rem;

	name = _get_symbol_name(0x40384d98, &module, &rem);
	if (name != NULL) {
	    fprintf(stderr, "doload: dying 0x%x after %s in %s\n",
		    rem, name, module);
	}
	name = _get_symbol_name(0x40308dd0, &module, &rem);
	if (name != NULL) {
	    fprintf(stderr, "doload: called from 0x%x after %s in %s\n",
		    rem, name, module);
	}
    }
#endif
    return entry;
}
