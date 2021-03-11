/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/telmat_svr4/RCS/class.h,v 1.1 1993/05/17 17:48:25 susan Exp $ */

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
#define TRUE 1
#define FALSE 0

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

#ifndef _IBMR2
/* 
 * Please remove this once AIX221 has a real C compiler.  %%%%
 * Thank, pgc.
 *
 * Redefine void only for cc on AIX/RT.   -mrt
 */
#if defined(AIX) && !defined(i386) && !defined(__HIGHC__)
#define	void char
#endif /* AIX */

#ifndef sys_telmat
extern void free();
extern char *realloc();
extern char *malloc();
#endif /* sys_telmat */
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



#define	class_GetType(object)	(((struct basicobject *)(object))->methods->info)
#define	class_GetTypeName(object)   (((struct basicobject *)(object))->methods->info->name)


/* 
 * routines that help with profiling
 */
extern void *class_GetEText();
extern void *class_GetTextBase();
extern unsigned long class_GetTextLength();


#endif /* class_DEFINED */

