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


#ifndef class_DEFINED
#define class_DEFINED 1


/**
 ** Used by some of the routines generated
 ** by the class preprocessor.
 **/

#include <stdio.h>


/**
 ** some constants, typedef's, etc.
 **/

#define class_VERSIONNOTKNOWN -1

#ifdef NULL
#undef NULL
#endif /* NULL */
#define NULL 0

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define class_MAXMETHODSBOUND 150

/* 
 * Please remove this once the vax has a real C compiler.  %%%%
 * Thank, pgc.
 */
#ifndef __STDC__
#ifdef vax
#define	void char
#endif /* vax */
#endif /* __STDC__  */



enum class_Error_type {class_ErrNone, class_ErrLoad, class_ErrInit, class_ErrNotLoaded};

enum class_ErrorEnum { class_ErrorNoError, class_ErrorBadValue, class_ErrorNotInitialized};

typedef unsigned long class_DebugLevelType;
typedef enum class_ErrorEnum class_ErrorType;
typedef unsigned long class_VersionNumberType;




/**
 ** external objects
 **/

#ifdef _IBMR2	    /* Should be POSIX_ENV, but that requires <system.h> */
#include <stdlib.h>	    /* ... for malloc(), etc. */
#include <unistd.h>	    /* ... for other prototypes.  Maybe we shouldn't do this? */
#else
/* 
 * Please remove this once AIX221 has a real C compiler.  %%%%
 * Thank, pgc.
 *
 * Redefine void only for cc on AIX/RT.   -mrt
 */
#if defined(AIX) && !defined(i386) && !defined(__HIGHC__)
#define	void char
#endif /* AIX */

extern void free();
extern char *realloc();
extern char *malloc();
#endif /* _IBMR2 */

extern int errno;


extern struct basicobject_methods class_RoutineStruct;
extern enum class_Error_type class_Error;


typedef int boolean;
typedef int (*procedure)();
typedef void *pointer;



/**
 ** needed local data.
 **/

/* none needed for now */


/**
 ** structures used by the class system.
 **/

struct classheader  {
    unsigned long versionnumber;
    char *name;
    char *namekey;
    struct basicobject_methods *classprocedures;
};

struct classinfo {
    struct basicobject_methods *methods;
    struct basicobject_methods *procs;
    char *name;
    char *namekey;
    unsigned long versionnumber;
    char *superclassname;
    char *superclassnamekey;
    struct classinfo *superclass;
    boolean (*classinitfunc)();
    struct classheader *header;
    void *textbase; /* position of dynamically loaded code, NULL if staticloaded */
    unsigned long textlength; /* length of text segment */
};

/* This is a prototype definition for each class' method table. You should never
 * actually declare a "struct basicobject", only pointers to one should be
 * declared. When dealing with objects, the right amount of space for that
 * object's class must be used. The constant class_METHODBOUND is used to
 * appease bounds checking compilers. It is not really a hard constant, but you
 * may get warnings from your compiler if you exceed it.
 */

struct basicobject_methods {
    struct classinfo *info;
    long (*routines[class_MAXMETHODSBOUND])();
};

struct basicobject {
    struct basicobject_methods *methods;
};


/**
 ** 
 ** Routines and macros.
 ** 
 **/

#define	class_GetType(object)	(((struct basicobject *)(object))->methods->info)
#define	class_GetTypeName(object)   (((struct basicobject *)(object))->methods->info->name)
#define class_GetSuperType(object) (((struct basicobject *)(object))->methods->info->superclass)
#define class_GetSuperTypeName(object) (((struct basicobject *)(object))->methods->info->superclass->name)

/*
 * Routines to initialize and control the operation of the runtime system.
 */
extern class_ErrorType class_Init(/* void */);
extern class_ErrorType class_SetClassPath(/* char * path */);
extern char * class_GetClassPath(/* void */);
extern class_ErrorType class_SetDebugLevel(/* class_DebugLevelType level */);
extern class_DebugLevelType class_GetDebugLevel(/* void */);

/*
 * Routines to deal with classes.
 */
extern class_VersionNumberType class_GetVersion(/* char * name */);
extern struct classinfo *class_Load(/* char * name */);
extern struct classinfo *class_LoadByKey(/* char * name */);
extern boolean class_IsLoaded(/* char * name */);
extern struct basicobject *class_NewObject(/* char * name */);
extern boolean class_IsType(/* struct basicobject * testobject, struct basicobject * typeobject */);
extern boolean class_IsTypeByName(/* char * testname, char * typename */);



/* 
 * routines that help with profiling
 */
extern void *class_GetEText();
extern void *class_GetTextBase();
extern unsigned long class_GetTextLength();

#endif /* class_DEFINED */

