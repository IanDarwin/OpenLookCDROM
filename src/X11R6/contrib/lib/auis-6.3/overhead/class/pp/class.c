/* ********************************************************************** *\
 *          Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/pp/RCS/class.c,v 2.63 1993/08/26 22:11:12 gk5g Exp $";
#endif

#include <andrewos.h> 
#include <sys/param.h>

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

/*
 * define these for the compiler 
 */
#ifndef _IBMR2
char *malloc(),*realloc();  /* should include stdlib.h */
#endif /* _IBMR2 */
extern int yylex();	    /* using lex */
#ifndef FLEX_ENV
extern char yytext[];	    
#else
extern char *yytext;
#endif /* FLEX_ENV */

/*
 * pick up parameters and constants
 */
#include <classpp.h>




/*
 * Hints, etc:
 *
 * -- Only errorexit(), main() and nametype are declared in
 *    a way that provides external visibility.  pgc
 *
 * -- The global variable is set by ParseFile() when
 *    topleve is TRUE.
 *
 * -- The -s (sloppy) switch should be eliminated but I
 *    just don't have the time to deal with fixing the
 *    definition files.  pgc
 *
 * -- The DependsOnly is a bit strange.  In general,
 *    the data is collected as though we we were 
 *    generating .ih and .eh files except that the
 *    ParseFile() routine is cut off after it places the 
 *    parent definition file name on the ParentInfo list.
 *
 * -- ???
 */


/*
 * Areas marked with %%% are known deficiencies that
 * should be fixed.
 *
 * Areas marked with ??? are old code I have not touched.
 * I have no idea what this stuff does.
 * 
 * pgc
 */


/*
 * Modification History:
 * ---- 
 *
 * *0* February 13, 1989
 * 
 * Base code as check out of RCS.
 * ----
 *
 * *1* February	13, 1989	pgc
 * 
 * Removed #ifdefs for USEGET. 
 * ----
 *
 * *2* February 13, 1989	pgc
 * 
 * General scrubbing up of the code.
 * ----
 *
 * *3* February 13, 1989	pgc
 *
 * This code modified by pgc to accept a new syntax.  
 * This change allows the name of a class and its superclass
 * to be followed by an optional base file name to use to 
 * hold that class's information.  As an example,
 *   class longclassname [lclsnm] : superlongclassname [slclsnm] {
 * is now valid and indicates that the files lclsnm.ih and lclsnm.eh
 * will be generated and slclsnm.ch will be searched of information
 * about the superclass.  The classname is also used to tell what the
 * name of the dynamic object will be such as lclsnm.do for the 
 * example class.  NOTE:  The base name for the .ih and .eh files is 
 * really obtained from the name of the file that is being process so
 * in this example the filename of the class header file must be
 * lclsnm.ch.  If this is not the case, the results are unpredictable.
 * ----
 *
 * March 23, 1989   pgc
 * 
 * Changed the generate *.ih files to include the 
 * entry foo_StaticEntry
 */


/*
 * Externally visible data:
 */
int nametype;	    /* used to communicate with lexer */


/*
 * Local data:
 */
static struct methods methodhead;	/* used to keep track of the methods */
static struct methods *methodlist;	/* as above */
static struct methods *methodend;	/* as above */

static FILE *importfile;		/* where to put the stuff for the .ih file */
static FILE *exportfile;		/* where to put the stuff for the .eh file */
static FILE *dependsfile;		/* where to put the stuff for the dependency info */

static char BaseFileName[MAXPATHLEN+1];		/* name of file with no extension */
static char HeaderFileName[MAXPATHLEN+1];	/* name of .ch file */
static char ImportFileName[MAXPATHLEN+1];	/* name of .ih file */
static char ExportFileName[MAXPATHLEN+1];	/* name of .eh file */

static char ParentBaseFileName[MAXPATHLEN+1];	/* base name of parent file */

static char DependsFileName[MAXPATHLEN+1];	/* name of dependency file if not STDOUT */

static int initializeobject;		/* TRUE if this class has data or initializeobject procedure was found */
static int initializeobjectdefined;	/* TRUE if an initializeobject procedure was found */
static int finalizeobject;		/* TRUE if a finalizeobject procedure was found */
static int allocate;			/* TRUE if a allocate procedure was found */
static int deallocate;			/* TRUE if a deallocate procedure was found */
static int initializeclass;		/* TRUE if an initializeclass procedure was found */
static struct methods *destroyplist;	/* linked list of all the destroyp procedures which apply. */

static int destroyp;			/* TRUE if an Destroyp procedure was found */

static int SilentMode;			/* TRUE if we are silent except for errors */
static int QuietMode;			/* TRUE if we are not issuing warnings */
static int DependsOnly;			/* TRUE we are only looking for superclasses */
static int SloppyMode;			/* TRUE if we are allowing "questionable" code to work */
static int IgnoreCurrentDirectory;	/* TRUE if current directory is not searched first by default */
static int MakingBaseObject;		/* TRUE if we are allowed to make a class with no superclass */

static char *DirectoryList[MAXINCLUDEPATHS];	/* place to store pointers to include directories */
static int NumDirectories;		/* number of these pointers */

static char FinalClassName[200];	/* the name of the class or package being created */
static char FinalClassNameKey[200];	/* the name key of the class or package being created */
static char FinalParentName[200];	/* this class's parent or '\0' */
static int classDefinition;		/* TRUE if building a class, FALSE for package */

#ifdef POSIX_ENV
static struct sigaction handler;        /* pointer to signal handler, used to catch signals */
#else
static struct sigvec handler;/* pointer to signal handler, used to catch signals */
#endif

static int usePrototypes;		/* TRUE if system is to generate prototypes */
static int generateDescriptionFile;	/* TRUE if system is to generate a description file for the class */
static int generateHeaderFiles = TRUE;	/* TRUE if system is to generate .ih and .eh files for the class */

static int routineCount = 0;
/*
 * structures and variables used to collect data while parsing
 * definition files.
 */
struct InfoStruct {
    struct InfoStruct *next;	/* pointer to next chunck of data */
    char *data;			/* the data */
};

static struct InfoStruct *ParentInfoHead;   /* used to keep track of superclasses */
static struct InfoStruct *ParentInfoTail;

static struct InfoStruct *DependsInfoHead;   /* used to keep track of dependencies */
static struct InfoStruct *DependsInfoTail;

static struct InfoStruct *DataInfoHead;   /* used to keep track of class data */
static struct InfoStruct *DataInfoTail;

static struct InfoStruct *ExtraInfoHead;   /* used to keep track other stuff in .ch file */
static struct InfoStruct *ExtraInfoTail;


/* 
 * 
 * 
 * Routines to deal with warnings and errors.
 * 
 * 
 */


/**
 ** Clean up and exit on catastrophic error.  The program exits with exit code
 ** ec after the message str is sent to stderr and stderr is flushed.
 **/
void errorexit(ec, str)
int ec;		/* exit code */
char *str;	/* error string to print on the way out */

{
/*
 * Don't bother checking return codes as there isn't
 * much that can be done at this point.
 */ 
    if (importfile != NULL)  {
	(void) fclose(importfile);
	(void) unlink(ImportFileName);
    }
    if (exportfile != NULL)  {
	(void) fclose(exportfile);
	(void) unlink(ExportFileName);
    }

/*
 * Emit the message and exit with exitcode.
 */
    (void) fprintf(stderr, "\nAndrew class preprocessor exiting abnormally!\n");
    (void) fprintf(stderr, "  processing file: %s\n", HeaderFileName);
    (void) fprintf(stderr, "  message: %s\n", str);
    (void) fprintf(stderr, "  exit code = %d.\n\n", ec);
    (void) fflush(stderr);
    exit(ec);
}


/**
 ** Another way to call errorexit() but this one stuffs param into
 ** base to create the message.  param must be a string pointer.
 **/
static void errorexitparam(ec, base, param)
int ec;		/* exit code */
char *base;	/* base of message string */
char *param;	/* string to stuff in base */

{
char message[MAXMESSAGESIZE+1];

    if ((strlen(base) + strlen(param)) < MAXMESSAGESIZE) {
	(void) sprintf(&message[0], base, param);	/* insert param in base */
    } else {
	(void) sprintf(&message[0], "!!ERROR BUILDING ERROR MESSAGE!!");
    }
    errorexit(ec, &message[0]);
}


/**
 ** Clean up and exit on catastrophic error.  The program exits with exit code
 ** ec after the message str is sent to stderr and stderr is flushed.
 **/
static void warning(level, str)
int level;	/* warning level */
char *str;	/* error string to print on the way out */

{
/*
 * Emit the message if the command line switches permit it.
 */
    switch(level) {
	case WARNING_SILENT:	/* emit unless we are silent */
	    if (! SilentMode) {
		(void) fprintf(stderr," WARNING:%s: - %s \n", HeaderFileName, str);
		(void) fflush(stderr);
	    }
	    break;

	case WARNING_QUIET:	/* emit unless we are silent */
	    if (! QuietMode) {
		(void) fprintf(stderr," WARNING (%s) - %s \n", HeaderFileName, str);
		(void) fflush(stderr);
	    }
	    break;

	case WARNING_SLOPPY:	/* always emit and exit if not "sloppy" */
	    (void) fprintf(stderr," WARNING - %s \n", str);
	    (void) fflush(stderr);
	    if (! SloppyMode) {
		errorexit(EXITCODE_SLOPPY, "this definition file should be cleaned up");
	    }
	    break;

	default:
	    errorexit(EXITCODE_BUG, "!! warning() called with illegal level !!");
	    break;
    }
}


/**
 ** Error handler for caught signals
 ** Left of unspecified type on purpose
 **/
#if defined(ANSI_C_SOURCE) && !defined(_NO_PROTO)
static void errorhandler(int sig)
#else
static
#ifdef POSIX_ENV
void
#endif
errorhandler(sig, code, scp)
int sig;
int code;
struct sigcontext *scp;
#endif
{
    errorexit(EXITCODE_SIGNAL, "killed by a caught signal");
}


/* 
 * 
 * 
 * Routines to get tokens in various ways.
 * 
 * 
 */


/** 
 ** Return the next token.  Just a new name for yylex().
 **/
static int getnexttoken()

{
    
    return yylex();
}


/** 
 ** Return the next token that is not either whitespace or a comment.
 **/
static int gettoken()

{
    int token;
    
    while ((token = getnexttoken()) != class_EOF && (token == class_WhiteSpace || token == class_Comment));
    if (token == class_EOF)  {
	errorexit(EXITCODE_DEFINITION, "premature EOF encountered while processing file");
    }
    return token;
}


/**
 ** If the passed token is is whitespace or a comment return the next token 
 ** that is not whitespace or a comment.
 **/
static int retrievetoken(intoken)
int intoken;

{
int token;

    token = intoken;	/* don't alter passed parameters */

    while (token == class_WhiteSpace || token == class_Comment)  {
	token = gettoken();
    }
    return token;
}
    

/* 
 * 
 * 
 * Routines to collect data.
 * 
 * 
 */


/**
 **
 **/
static void InsertInfo(Head, Tail, Data)
struct InfoStruct **Head;
struct InfoStruct **Tail;
char *Data;

{
struct InfoStruct *ThisInfo;

/* make a new data node */
    ThisInfo = (struct InfoStruct *) malloc(sizeof(struct InfoStruct));
    if (ThisInfo == NULL) {
	errorexit(EXITCODE_RESOURCES, "can not allocate enough storage to collect data");
    }

    ThisInfo->data = malloc(strlen(Data) + 1);	/* +1 for NULL terminator */
    if (ThisInfo->data == NULL) {
	errorexit(EXITCODE_RESOURCES, "can not allocate enough storage to collect data");
    }

    (void) strcpy(ThisInfo->data, Data);
    ThisInfo->next = NULL;  /* marks new end of data */
    
    
/* insert that node */
    if (*Head == NULL) {
	*Head = ThisInfo;
	*Tail = ThisInfo;
    } else {
	(*Tail)->next =	ThisInfo;   /* link into information chain */
	*Tail = ThisInfo;	    /* update tail pointer */
    }	
}
 

/**
 ** Add superclasses to collected information.
 **/
static void AddSuperClassInfo(name)
char *name;

{
    InsertInfo(&ParentInfoHead, &ParentInfoTail, name);
}


/**
 ** Add dependencies to collected information.
 **/
static void AddDependsInfo(name)
char *name;

{
    InsertInfo(&DependsInfoHead, &DependsInfoTail, name);
}


/**
 ** Add class data to collected information.
 **/
static void AddClassDataInfo(data)
char *data;

{
    InsertInfo(&DataInfoHead, &DataInfoTail, data);
}


/**
 ** Add extra to collected information.
 **/
static void AddExtraInfo(text)
char *text;

{
    InsertInfo(&ExtraInfoHead, &ExtraInfoTail, text);
}


/* 
 * 
 * 
 * Routines to help find the needed parent class 
 * definition files.
 * 
 * 
 */


/**
 ** Add another directory to the search path.
 **/
static void AddDirectory(dirname)
char *dirname;

{
    if (strlen(dirname)	< MAXPATHLEN) {		/* is the name within the system's bounds? */
	if (NumDirectories < MAXINCLUDEPATHS) {	/* have a finite number of entries */
	    DirectoryList[NumDirectories] = (char *) malloc(strlen(dirname) + 1);
	    if (DirectoryList[NumDirectories] == NULL) {
		errorexit(EXITCODE_RESOURCES, "not enough memory to add another search path");
	    }
	    (void) strcpy(DirectoryList[NumDirectories], dirname);
	    NumDirectories++;
	} else {
	    errorexit(EXITCODE_BUG, "too many include paths have been defined");
	}
    } else if (strlen(dirname) == 0) {		/* this shouldn't happen */
	errorexit(EXITCODE_BUG, "internal error in program...zero length path passed to AddDirectory");
    } else {
	errorexit(EXITCODE_COMMAND, "one of the include file directory names is too long");
    }
}
   

/** 
 ** Try to open a file along the current search paths.
 **/
static FILE *OpenFile(filename)
char *filename;

{
FILE *file;
char tempfile[MAXPATHLEN + 1];
int i;
    
    file = NULL;    /* initialize to cause error if nothing is found */

/* a quick sanity check */
    if (strlen(filename) < 1) {
	errorexit(EXITCODE_BUG, "an attempt was made to open a nameless file");
    }

/* first try the given name if allowed and not an absolute path */
    if ((!IgnoreCurrentDirectory) && (filename[0] != '/')) {
	file = fopen(filename, "r");
	(void) strcpy(tempfile,	filename);  /* needed for DependsOnly */
    }

/*
 * If above fails, look in the search paths if the
 * name doesn't start with a '/'.
 */
    if ((file == NULL) && (filename[0] != '/'))  {
	for (i = 0; i < NumDirectories; i++)  {
	    /* make sure the name fits in the temporary space */
	    if ((strlen(DirectoryList[i]) + strlen(filename) + 1) < MAXPATHLEN) {
		(void) sprintf(tempfile, "%s/%s", DirectoryList[i], filename);
	    } else {
		errorexitparam(EXITCODE_ACCESS, "the name of the file %s is too long", tempfile);
	    };
	    file = fopen(tempfile, "r");
	    /* if we find the file just return */
	    if (file != NULL) {
		break;	/* get out of this loop, we go it */
            }
	}
    }

    if (file == NULL) {
	errorexitparam(EXITCODE_ACCESS, "can not open the file %s", filename);
    }

    AddDependsInfo(tempfile); /* stash this away for later */

    return file;
}


/* 
 * 
 * 
 * Routines to generate the .ih and .eh files.
 * 
 * 
 */


/**
 ** Put the str and a number of parameters to both the import and export files.
 ** 
 ** There are a few versions of this because we can't count on 
 ** variable argument function support on all systems.
 **/
static void outstr0(str)	    /* just the string */
char *str;

{
    (void) fprintf(importfile, str);
    (void) fprintf(exportfile, str);
}

static void outstr1(str, a)	    /* 1 arg version */
char *str;
char *a;

{
    (void) fprintf(importfile, str, a);
    (void) fprintf(exportfile, str, a);
}

static void outstr2(str, a, b)	    /* 2 arg version */
char *str;
char *a;
char *b;

{
    (void) fprintf(importfile, str, a, b);
    (void) fprintf(exportfile, str, a, b);
}

static void outstr3(str, a, b, c)    /* 3 arg version */
char *str;
char *a;
char *b;
char *c;

{
    (void) fprintf(importfile, str, a, b, c);
    (void) fprintf(exportfile, str, a, b, c);
}



/**
 ** This is essentially the second half of the original
 ** parsefile() routine.  I have just isolated the bottom 
 ** half of parsefile() and moved it here.  A couple of
 ** changes have been made but in general, I have left
 ** this part of the program alone as I am too scared
 ** to attack it right now.  pgc
 ** 
 ** 
 ** I have done as little with this part as possible.  I see that the 
 ** variables FinalClaseName and FinalParentName must be set before entering
 ** this part.  In many places the generation code checks to see if 
 ** FinalClassName is an empty string (FinalParentName[0] == '\0')
 ** and modifies its behaviour appropriately. (I hope!)
 ** 
 ** I have changed the way error messages are produced.  I
 ** also added the initialization of a new field to the classinfo 
 ** and classheader structures.  This new field is used to figure 
 ** out where to get the code for a class that is going to be 
 ** dynamically loaded.
 **
 ** good luck.    pgc
 ** 
 ** 
 **/
static void GenerateRoutines()

{
char scratchbuf[200];	/* used by #define getFuncName(mp) for an sprintf() */
struct methods *mp;	/* for loop variable */
int ev;			/* for loop variable */
int methodCount;	/* ??? */
int parentMethodCount;	/* ??? */
int procedureCount;	/* ??? */
int errvalCount[errval_NUM];	/* ??? */

#ifdef getFuncName
#undef getFuncName
#endif /* getFuncName */
#define getFuncName(thismp) (sprintf(scratchbuf,"%s__%s",FinalClassName,thismp->name),scratchbuf)

    methodCount = 0;
    parentMethodCount = 0;
    procedureCount = 0;

    for(ev=0; ev<errval_NUM; ev++)
	errvalCount[ev]=0;

    for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
        if (mp->type==ptype_method && mp->macrodef==NULL)  {
            methodCount++;
	    if (mp->inherited)
		parentMethodCount++;
        }
	else if (mp->type==ptype_classproc && mp->defined){
            procedureCount++;
	    errvalCount[mp->errval]++;
	}
    }
    outstr0("\n#ifndef class_StaticInfoOnly\n");
    outstr1("\n#ifndef %s_PROGRAMMERVERSION\n",  FinalClassName);
    outstr1("#define %s_PROGRAMMERVERSION 1\n", FinalClassName);
    outstr1("#endif /* %s_PROGRAMMERVERSION */\n\n", FinalClassName);

    outstr1("#ifdef %s_VERSION\n", FinalClassName);
    outstr1("#undef %s_VERSION\n", FinalClassName);
    outstr1("#endif /* %s_VERSION */\n", FinalClassName);

    /* the packing of the errvalCount values into the version number here
     * is highly dependent on the max number of classprocedures.  Since
     * it is currently 50, a six bit field suffices for each of the two values
     * we must store.
     * The 0x80000 indicates a class (not a package).
         * %%%%%
         * I think the above is now incorrect as there are more than 50
         * possible class procedures per class.
     */
    /* One of the next two lines is written to both files.  An
     * outstr function has not been used because these two lines
     * have integers in the output.
     */

    if(classDefinition) {
	fprintf(importfile, "#define %s_VERSION ((((%s_PROGRAMMERVERSION << 16) ^ (%d << 8) ^ sizeof(struct %s)) & 0x7ffff) + 0x80000 + (%d << 20))\n\n", FinalClassName, FinalClassName, methodCount, FinalClassName, (errvalCount[errval_NULL]<<6)+errvalCount[errval_MinusOne]);
	fprintf(exportfile, "#define %s_VERSION ((((%s_PROGRAMMERVERSION << 16) ^ (%d << 8) ^ sizeof(struct %s)) & 0x7ffff) + 0x80000 + (%d << 20))\n\n", FinalClassName, FinalClassName, methodCount, FinalClassName, (errvalCount[errval_NULL]<<6)+errvalCount[errval_MinusOne]);
    } else {
	fprintf(importfile, "#define %s_VERSION ((((%s_PROGRAMMERVERSION << 16) ^ (%d << 8)) & 0x7ffff) + (%d << 20))\n\n", FinalClassName, FinalClassName, procedureCount, (errvalCount[errval_NULL]<<6)+errvalCount[errval_MinusOne]);
	fprintf(exportfile, "#define %s_VERSION ((((%s_PROGRAMMERVERSION << 16) ^ (%d << 8)) & 0x7ffff) + (%d << 20))\n\n", FinalClassName, FinalClassName, procedureCount, (errvalCount[errval_NULL]<<6)+errvalCount[errval_MinusOne]);
    }

    outstr0("\n#endif /* class_StaticInfoOnly */\n");
    /* generate the *_StaticLoad and *_StaticLoadThisClass stubs */

    outstr1("#define %s_StaticLoadOnlyThisClass()  \\\n", FinalClassName);
    outstr1("{ extern struct classinfo *%s__GetClassInfo(); \\\n", FinalClassName);
    outstr1(" extern struct classheader *%s_classheader_StaticExport; \\\n", FinalClassNameKey);
    outstr3("  (void) class_EnterInfo(NULL, %s_classheader_StaticExport->name, %s__GetClassInfo, NULL, %s_classheader_StaticExport->namekey); } \n \n", FinalClassNameKey, FinalClassName, FinalClassNameKey);

    outstr1("#define %s_StaticLoad()  \\\n", FinalClassName);
    outstr0("    { \\\n");
    if (FinalParentName[0]) {
	outstr1("    %s_StaticLoad(); \\\n", FinalParentName);
    }
    outstr1("    %s_StaticLoadOnlyThisClass();  \\\n", FinalClassName);
    outstr0("    } \n \n");



    /* generate the *_StaticEntry stub */

    outstr2("#define %s_StaticEntry  {%s_StaticLoad()} \n \n", FinalClassName, FinalClassName);

    if(strcmp(FinalClassNameKey, FinalClassName)!=0) outstr2("#define %s_StaticEntry  {%s_StaticLoad()} \n \n", FinalClassNameKey, FinalClassName);


    outstr0("#ifndef class_StaticInfoOnly\n");
    /*
         * this must be here to be visible to subclasses who don't specifically
     * include us
     */
    outstr1("static struct classheader %s_classheader = {\n",FinalClassName);
    outstr1("    %s_VERSION,\n", FinalClassName);
    outstr1("    \"%s\",\n", FinalClassName);
    outstr1("    \"%s\",\n", BaseFileName);
    
    outstr1("    (struct basicobject_methods *) %s,\n",
#ifdef CLASS_CTRAMPOLINE_ENV
	    "NULL"
#else /* CLASS_CTRAMPOLINE_ENV */
	    "&class_RoutineStruct"
#endif /* CLASS_CTRAMPOLINE_ENV */
	    );
    outstr0("};\n\n");

    if(importfile && exportfile) {
#ifdef CLASS_CTRAMPOLINE_ENV
	fprintf(exportfile, "#define %s_CLASSPROCEDURES ((!%s_classheader.classprocedures)?(((struct basicobject_methods *)class_Lookup(&%s_classheader)),%s_classheader.classprocedures):%s_classheader.classprocedures)\n", FinalClassName, FinalClassName, FinalClassName, FinalClassName, FinalClassName);

	if(FinalParentName && FinalParentName[0]) fprintf(exportfile, "#define %s_CLASSPROCEDURES ((!%s_classheader.classprocedures)?(((struct basicobject_methods *)class_Lookup(&%s_classheader)),%s_classheader.classprocedures):%s_classheader.classprocedures)\n", FinalParentName, FinalParentName, FinalParentName, FinalParentName, FinalParentName);

	fprintf(importfile, "#define %s_CLASSPROCEDURES ((!%s_classheader.classprocedures)?(((struct basicobject_methods *)class_Lookup(&%s_classheader)),%s_classheader.classprocedures):%s_classheader.classprocedures)\n", FinalClassName, FinalClassName, FinalClassName, FinalClassName, FinalClassName);
#else /* CLASS_CTRAMPOLINE_ENV */
	fprintf(exportfile, "#define %s_CLASSPROCEDURES (%s_classheader.classprocedures)\n", FinalClassName, FinalClassName);
	fprintf(importfile, "#define %s_CLASSPROCEDURES (%s_classheader.classprocedures)\n", FinalClassName, FinalClassName);
	
	if(FinalParentName && FinalParentName[0]) fprintf(exportfile, "#define %s_CLASSPROCEDURES (%s_classheader.classprocedures)\n", FinalParentName, FinalParentName);
#endif /* CLASS_CTRAMPOLINE_ENV */	

    }


    /* this is for getting the list of classes a particular
     .o file was compiled with, via nm blah.o... */
    if(importfile && exportfile) {
	fprintf(importfile, "int %s_classref_;\n", FinalClassNameKey);
	fprintf(exportfile, "int %s_classdef_;\n", FinalClassNameKey);
    }
    outstr0("#endif /* class_StaticInfoOnly */\n");
/* 
 * 
 * 
 * 
 * 
 */
    /* Close initial #ifndef.*/
    outstr1("\n#endif /* %s_DEFINED */\n\n", FinalClassName);


    if(exportfile) {
	fprintf(exportfile, "#if !defined(AUXMODULE) && !defined(class_StaticExportDEFINED)\n#define class_StaticExportDEFINED\nstruct classheader *%s_classheader_StaticExport = (&%s_classheader);\n#endif /* !defined(AUXMODULE) && !defined(class_StaticExportDEFINED) */\n\n", FinalClassNameKey, FinalClassName);
    }
    
    outstr2("#if !defined(%s_ROUTINESDEFINED) && !defined(dontDefineRoutinesFor_%s) && !defined(class_StaticEntriesOnly)\n",FinalClassName,FinalClassName);
    outstr1("#define %s_ROUTINESDEFINED\n\n",FinalClassName);

    if(classDefinition){
	/* don't use an outstr function as this has an integer */

        for (mp = methodlist->next; mp != NULL; mp = mp->next) {
            if (mp->type==ptype_method){
                outstr3("#define %s_%s(self%s) \\\n", FinalClassName, mp->name, mp->methodargs);
		if(mp->macrodef==NULL){
		    char proto[10000];

		    if (usePrototypes) {
			sprintf(proto, "struct %s *%s", FinalClassName, mp->argtypes);
		    }
		    else {
			proto[0] = '\0';
		    }

		    /* don't use an outstr function as this has	an integer */


		    fprintf(importfile, "    ((* ((%s (*)(%s))((self)->header.%s_methods->routines[%d]))) (self%s))\n",
			    mp->methodtype, proto, FinalClassName, mp->index, mp->methodargs);
		    fprintf(exportfile, "    ((* ((%s (*)(%s))((self)->header.%s_methods->routines[%d]))) (self%s))\n",
			    mp->methodtype, proto, FinalClassName, mp->index, mp->methodargs);
                }else
                    outstr1("%s\n",mp->macrodef);
            }
        }

        outstr0("\n");
    }

    if(classDefinition){
	char proto[10000];

	proto[0] = '\0';

	if (usePrototypes) {
	    strcpy(proto, "struct classheader *, unsigned long");
	}

	
        outstr1("#define %s_NewFromObject(self) \\\n", FinalClassName);
	outstr3("    ((* ((struct %s * (*)(%s))((self)->header.%s_methods->info->procs->routines[0])))", FinalClassName, proto, FinalClassName);
        outstr2(" (&%s_classheader, (self)->header.%s_methods->info->versionnumber))\n", FinalClassName, FinalClassName);

        outstr1("#define %s_New() \\\n", FinalClassName);
	(void) fprintf(importfile, "    (*((struct %s * (*)(%s)) (%s_CLASSPROCEDURES->routines[0])))(&%s_classheader,%s_VERSION)\n", FinalClassName, proto, FinalClassName, FinalClassName, FinalClassName);
        (void) fprintf(exportfile, "    (%s__New(NULL, %s_VERSION))\n", FinalClassName, FinalClassName);

	if (usePrototypes) {
	    sprintf(proto, "struct classheader *, struct %s *, unsigned long", FinalClassName);
	}

        outstr1("#define %s_Initialize(self) \\\n", FinalClassName);
        /* if this changes, also change in the definition of __Initialize */
	(void) fprintf(importfile, "    (*((void (*)(%s)) (%s_CLASSPROCEDURES->routines[2])))(&%s_classheader,self, %s_VERSION)\n", proto, FinalClassName, FinalClassName, FinalClassName);

        (void) fprintf(exportfile, "    (%s__Initialize(NULL, self, %s_VERSION))\n", FinalClassName, FinalClassName);

        outstr0("\n");

	if (usePrototypes) {
	    sprintf(proto, "struct classheader *, struct %s *", FinalClassName);
	}

        outstr1("#define %s_Destroy(self) \\\n", FinalClassName);
	outstr3("    ((* ((void (*)(%s)) ((self)->header.%s_methods->info->procs->routines[1]))) (&%s_classheader, self))\n", proto, FinalClassName, FinalClassName);

        outstr1("#define %s_Finalize(self) \\\n", FinalClassName);
        /* if this changes, also change in the definition of __Finalize */
	(void) fprintf(importfile, "    (*((void (*)(%s)) (%s_CLASSPROCEDURES->routines[3])))(&%s_classheader,self)\n", proto, FinalClassName, FinalClassName);
        (void) fprintf(exportfile, "    (%s__Finalize(NULL, self))\n", FinalClassName);
    }

    outstr0("\n");

    for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
	if (mp->type==ptype_classproc && mp->defined)  {
	    char proto[10000];

	    if (usePrototypes) {
		sprintf(proto, "struct classheader *%s", mp->argtypes);
	    }
	    else {
		proto[0] = '\0';
	    }

	    outstr3("#define %s_%s(%s) \\\n", FinalClassName, mp->name, mp->methodargs);
	    if (mp->argcount != 0)
		(void) fprintf(importfile, "    (*((%s (*)(%s)) (%s_CLASSPROCEDURES->routines[%d])))(&%s_classheader,%s)\n", mp->methodtype, proto, FinalClassName, mp->index, FinalClassName, mp->methodargs);
	    else
		(void) fprintf(importfile, "    (*((%s (*)(%s)) (%s_CLASSPROCEDURES->routines[%d])))(&%s_classheader)\n", mp->methodtype, proto, FinalClassName, mp->index, FinalClassName);
	    if (mp->argcount != 0)
		(void) fprintf(exportfile, "    ((%s) %s(NULL, %s))\n", mp->methodtype, getFuncName(mp), mp->methodargs);
	    else
		(void) fprintf(exportfile, "    ((%s) %s(NULL))\n", mp->methodtype, getFuncName(mp));

	}
    }

    outstr0("\n");

    for(mp=methodlist->next;mp!=NULL;mp=mp->next)
        if(mp->type==ptype_macro){
            outstr3("#define %s_%s(%s) \\\n",FinalClassName,mp->name,mp->methodargs);
            outstr1("    %s\n",mp->macrodef);
        }

    outstr0("\n");

    outstr2("#endif /* !defined(%s_ROUTINESDEFINED) && !defined(dontDefineRoutinesFor_%s ) && !defined(class_StaticEntriesOnly)*/\n\n",FinalClassName,FinalClassName);


/* 
 * 
 * 
 * We're done with the the .ih file at this point.  Everything
 * goes to the .eh file from here.
 * 
 * 
 */


    if(classDefinition){
        for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
            if (mp->type==ptype_method && mp->inherited)  {
		char proto[10000];

		if (usePrototypes) {
		    sprintf(proto, "struct %s *%s", FinalClassName, mp->argtypes);
		}
		else {
		    proto[0] = '\0';
		}

                (void) fprintf(exportfile, "#define super_%s(self%s)  \\\n", mp->name, mp->methodargs);
                if(mp->macrodef!=NULL)
		    (void) fprintf(exportfile,"	   %s_%s((struct %s *)self%s)\n",FinalParentName,mp->name,FinalParentName,mp->methodargs);
                else{
                    (void) fprintf(exportfile, "    ((* ((%s (*)(%s)) (%s_supermethods->routines[%d]))) (self%s))\n", mp->methodtype, proto, FinalClassName, mp->index, mp->methodargs);
                }
            }
        }

        (void) fprintf(exportfile,"\n");

        if (FinalParentName[0])  {
            (void) fprintf(exportfile, "extern struct basicobject_methods *%s_supermethods;\n", FinalClassName);
        }
    }

    (void) fprintf(exportfile, "\n#ifndef AUXMODULE\n\n");

    /*
     * this is used by the generated routines.
     */

    if ((FinalParentName[0] != '\0') && classDefinition)  {
	(void) fprintf(exportfile, "struct basicobject_methods *%s_supermethods;\n\n", FinalClassName);
    }

    if (classDefinition)  {
	(void) fprintf(exportfile, "struct %s_methods {\n", FinalClassName);
	(void) fprintf(exportfile, "    struct classinfo *info;\n");
	if (methodCount > 0)
	    (void) fprintf(exportfile,"    long (*routines[%d])();\n", methodCount);
	(void) fprintf(exportfile, "};\n\n");
    }
    
    (void) fprintf(exportfile, "struct %s_classprocedures {\n", FinalClassName);
    (void) fprintf(exportfile, "    struct classinfo *info;\n");
    (void) fprintf(exportfile, "    long (*routines[%d])();\n", (classDefinition) ? (procedureCount + NUMSPECIALPROCS) : procedureCount);
    (void) fprintf(exportfile, "};\n\n");
    
    if (classDefinition)  {
        for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
            if (mp->type==ptype_method && mp->defined && mp->macrodef==NULL)  {
		char proto[10000];

		if (usePrototypes) {
		    sprintf(proto, "struct %s *%s", FinalClassName, mp->realargtypes);
		}
		else {
		    proto[0] = '\0';
		}

		(void) fprintf(exportfile, "%s %s(%s);\n", mp->methodtype, getFuncName(mp), proto);
            }
        }

        (void) fprintf(exportfile, "\nstatic struct %s_methods %s_methodtable = {\n", FinalClassName, FinalClassName);
        (void) fprintf(exportfile, "    NULL", FinalClassName);

        for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
            if (mp->type==ptype_method && mp->macrodef==NULL)  {
                if (mp->defined)  {
		    (void) fprintf(exportfile, ",\n    (long (*)()) %s",getFuncName(mp));
                }
                else  {
                    (void) fprintf(exportfile, ",\n    NULL");
                }
            }
        }

        (void) fprintf(exportfile, "\n};\n\n");

	if (usePrototypes) {
	    (void) fprintf(exportfile, "struct %s *%s__New(struct classheader *, unsigned long);\n", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "void %s__Destroy(struct classheader *, struct %s *);\n", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "boolean %s__Initialize(struct classheader *, struct %s *, unsigned long);\n", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "void %s__Finalize(struct classheader *, struct %s *);\n", FinalClassName, FinalClassName);
	    if(destroyp) (void) fprintf(exportfile, "boolean %s__Destroyp(struct classheader *, struct %s *);\n", FinalClassName, FinalClassName);
	    if (initializeobject && ! initializeobjectdefined)
		(void) fprintf(exportfile, "boolean %s__InitializeObject(struct classheader *, struct %s *);\n", FinalClassName, FinalClassName);
	}
	else {
	    (void) fprintf(exportfile, "struct %s *%s__New();\n", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "void %s__Destroy();\n", FinalClassName);
	    (void) fprintf(exportfile, "boolean %s__Initialize();\n", FinalClassName);
	    (void) fprintf(exportfile, "void %s__Finalize();\n", FinalClassName);
	    if(destroyp) (void) fprintf(exportfile, "boolean %s__Destroyp();\n", FinalClassName);
	    if (initializeobject && ! initializeobjectdefined)
		(void) fprintf(exportfile, "boolean %s__InitializeObject();\n", FinalClassName);
	}
    }
    
    for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
	if (mp->type==ptype_classproc && mp->defined && !mp->special)  {
	    char proto[10000];

	    if (usePrototypes) {
		sprintf(proto, "struct classheader *%s", mp->realargtypes);
	    }
	    else {
		proto[0] = '\0';
	    }

	    (void) fprintf(exportfile, "%s %s(%s);\n", mp->methodtype, getFuncName(mp), proto);
	}
    }

    (void) fprintf(exportfile,"\nstatic struct %s_classprocedures %s_classproceduretable = {\n", FinalClassName, FinalClassName);
    (void) fprintf(exportfile, "    NULL");

    if (classDefinition) {
	(void) fprintf(exportfile,",\n");
	(void) fprintf(exportfile, "    (long (*)()) %s__New,\n", FinalClassName);
	(void) fprintf(exportfile, "    (long (*)()) %s__Destroy,\n", FinalClassName);
	(void) fprintf(exportfile, "    (long (*)()) %s__Initialize,\n", FinalClassName);
	(void) fprintf(exportfile, "    (long (*)()) %s__Finalize,\n", FinalClassName);
	if(destroyp) {
	    (void) fprintf(exportfile, "    (long (*)()) %s__Destroyp", FinalClassName);
	} else fprintf(exportfile,"    (long (*)()) NULL");
    }


    for (mp = methodlist->next; mp != NULL; mp = mp->next)
	if (mp->type==ptype_classproc && mp->defined && !mp->special)
	    (void) fprintf(exportfile,
			   ",\n    (long (*)()) %s", getFuncName(mp));

    (void) fprintf(exportfile, "\n};\n\n");




    /* create the class info structure */

    (void) fprintf(exportfile,"\nstatic struct classinfo %s_classinfo = {\n", FinalClassName);
    if (classDefinition)
	(void) fprintf(exportfile, "    (struct basicobject_methods *) &%s_methodtable,\n", FinalClassName);
    else
	(void) fprintf(exportfile, "    NULL,\n");
    (void) fprintf(exportfile, "    (struct basicobject_methods *) &%s_classproceduretable,\n", FinalClassName);
    (void) fprintf(exportfile, "    \"%s\",\n", FinalClassName);
    (void) fprintf(exportfile, "    \"%s\",\n", BaseFileName);
    (void) fprintf(exportfile, "    %s_VERSION,\n", FinalClassName);
    if (FinalParentName[0] != '\0') {	/* if no superclass use NULL for name instead of "" */
	(void) fprintf(exportfile, "    \"%s\",\n",	FinalParentName);
	(void) fprintf(exportfile, "    \"%s\",\n",	ParentBaseFileName);
    } else {
	(void) fprintf(exportfile, "    NULL,\n");
	(void) fprintf(exportfile, "    NULL,\n");
    }
    (void) fprintf(exportfile, "    NULL,\n");	    /* superclass info * */
    if (initializeclass){
	(void) fprintf(exportfile, "    (boolean (*)()) %s__InitializeClass,\n", FinalClassName);    /* routine InitializeClass this class */
    } else {
	(void) fprintf(exportfile, "    NULL,\n");	    /* no InitializeClass for this class */
    }
    (void) fprintf(exportfile, "    &%s_classheader,\n", FinalClassName);   /* pointer to class header */
    (void) fprintf(exportfile, "    NULL,\n");	    /* base of text */
    (void) fprintf(exportfile, "    0\n");	    /* length of text */
    (void) fprintf(exportfile, "};\n");


    /* the following are for classes only, not packages */
    if(classDefinition){
        (void) fprintf(exportfile, "\nstatic int classinitialized = FALSE;\n");

	if (usePrototypes) {
	    (void) fprintf(exportfile, "\nboolean %s__Initialize(", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID,");
	    (void) fprintf(exportfile, " struct %s *self,", FinalClassName);
	    (void) fprintf(exportfile, " unsigned long versionnumber)\n{\n");
	}
	else {
	    (void) fprintf(exportfile, "\nboolean %s__Initialize(classID, self, versionnumber)\n", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID;\n");
	    (void) fprintf(exportfile, "struct %s *self;\n", FinalClassName);
	    (void) fprintf(exportfile, "unsigned long versionnumber;\n{\n");
	}

        if (FinalParentName[0])  {
            /* print out expanded version of call to superclass' initialize
             * procedure, so we don't depend on parents macros
	     */
	    char proto[10000];

	    if (usePrototypes) {
		sprintf(proto, "struct classheader *, struct %s *, unsigned long", FinalClassName);
	    }
	    else {
		proto[0] = '\0';
	    }

            (void) fprintf(exportfile, "    /* %s_Initialize(self); */\n", FinalParentName, FinalParentName);
	    (void) fprintf(exportfile, "    if(!(*((boolean (*)(%s)) (%s_CLASSPROCEDURES->routines[2])))(&%s_classheader,self, %s_VERSION))\n", proto, FinalParentName, FinalParentName, FinalParentName);
	    (void) fprintf(exportfile, "\treturn FALSE;\n");
	}

        (void) fprintf(exportfile, "    if (! classinitialized)  {\n");
        (void) fprintf(exportfile, "\tclassinitialized = TRUE;\n");

        if (FinalParentName[0])  {
            (void) fprintf(exportfile, "\t%s_supermethods =self->header.%s_methods;\n", FinalClassName, FinalClassName);
            if (parentMethodCount > 0)  {
                (void) fprintf(exportfile, "\t{\n");
                (void) fprintf(exportfile, "\t    long (**p)() = &(%s_methodtable.routines[0]);\n", FinalClassName);
                (void) fprintf(exportfile, "\t    long (**q)() = &(self->header.%s_methods->routines[0]);\n", FinalClassName);
                (void) fprintf(exportfile, "\t    long i;\n\n");
                (void) fprintf(exportfile, "\t    for (i = 0; i < %d; i++)  {\n", parentMethodCount);
                (void) fprintf(exportfile, "\t\tif (*p == NULL)\n");
                (void) fprintf(exportfile, "\t\t    *p = *q;\n");
                (void) fprintf(exportfile, "\t\tp++;\n");
                (void) fprintf(exportfile, "\t\tq++;\n");
                (void) fprintf(exportfile, "\t    }\n");
                (void) fprintf(exportfile, "\t}\n");
            }
            (void) fprintf(exportfile, "\t%s_classinfo.superclass = %s_supermethods->info;\n", FinalClassName, FinalClassName);
        }

        (void) fprintf(exportfile, "\t%s_methodtable.info = &%s_classinfo;\n", FinalClassName, FinalClassName);
        (void) fprintf(exportfile, "    }\n");
        (void) fprintf(exportfile, "    self->header.%s_methods = (struct basicobject_methods *) &%s_methodtable;\n", FinalClassName, FinalClassName);
        if (initializeobject){
	    (void) fprintf(exportfile, "    if(!%s__InitializeObject(classID, self)){\n", FinalClassName);
	    if (FinalParentName[0])  {
		char proto[10000];

		if (usePrototypes) {
		    sprintf(proto, "struct classheader *, struct %s *", FinalClassName);
		}
		else {
		    proto[0] = '\0';
		}

		(void) fprintf(exportfile, "\t/* %s_Finalize(self); */\n", FinalParentName);
		(void) fprintf(exportfile, "\t(*((void (*)(%s)) (%s_CLASSPROCEDURES->routines[3])))(&%s_classheader,self);\n", proto, FinalParentName, FinalParentName);
	    }
	    (void) fprintf(exportfile, "\treturn FALSE;\n    }\n");
	}
	(void) fprintf(exportfile,"    return TRUE;\n}\n\n");

	if (usePrototypes) {
	    (void) fprintf(exportfile, "struct %s *%s__New(", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID,");
	    (void) fprintf(exportfile, " unsigned long versionnumber)\n{\n");
	}
	else {
	    (void) fprintf(exportfile, "struct %s *%s__New(classID, versionnumber)\n", FinalClassName, FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID;\n");
	    (void) fprintf(exportfile, "unsigned long versionnumber;\n{\n");
	}

        (void) fprintf(exportfile, "    struct %s *self;\n", FinalClassName);
        (void) fprintf(exportfile, "    if (versionnumber != class_VERSIONNOTKNOWN && versionnumber != %s_VERSION)  {\n", FinalClassName);
        (void) fprintf(exportfile, "\t(void) fprintf(stderr, \"Incompatible version of %s requested!\\n\");\n", FinalClassName);
/*        (void) fprintf(exportfile, "\treturn NULL;\n"); %%%%% think about this ...  */
        (void) fprintf(exportfile, "    }\n");
        if (allocate)
            (void) fprintf(exportfile, "    self = %s__Allocate(classID);\n", FinalClassName, FinalClassName);
        else
            (void) fprintf(exportfile, "    self = (struct %s *) malloc (sizeof(struct %s));\n", FinalClassName, FinalClassName);
        (void) fprintf(exportfile,"    if (! self)  {\n");
        (void) fprintf(exportfile,"\t(void) fprintf(stderr, \"Could not allocate %s object!\\n\");\n", FinalClassName);
        (void) fprintf(exportfile, "\treturn NULL;\n");
        (void) fprintf(exportfile, "    }\n");
	(void) fprintf(exportfile, "    if(!%s__Initialize(classID, self, versionnumber)){\n", FinalClassName);
	if(deallocate)
	    (void) fprintf(exportfile, "\t%s__Deallocate(classID, self);\n",FinalClassName);
	else
	    (void) fprintf(exportfile, "\tfree((char *)self);\n");
	(void) fprintf(exportfile,"\treturn NULL;\n    }\n");
        (void) fprintf(exportfile, "    return self;\n");
        (void) fprintf(exportfile, "}\n\n");

	if (usePrototypes) {
	    (void) fprintf(exportfile, "void %s__Finalize(", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID,");
	    (void) fprintf(exportfile, " struct %s *self)\n{\n", FinalClassName);
	}
	else {
	    (void) fprintf(exportfile, "void %s__Finalize(classID, self)\n", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID;\n");
	    (void) fprintf(exportfile, "struct %s *self;\n{\n", FinalClassName);
	}

        if (finalizeobject)
            (void) fprintf(exportfile, "    %s__FinalizeObject(classID, self);\n", FinalClassName);
        if (FinalParentName[0]){
            /* print out expanded version of call to superclass' finalize
             * procedure, so we don't depend on parents macros
             */
	    char proto[10000];

	    if (usePrototypes) {
		sprintf(proto, "struct classheader *, struct %s *", FinalClassName);
	    }
	    else {
		proto[0] = '\0';
	    }

            (void) fprintf(exportfile, "    /* %s_Finalize(self); */\n", FinalParentName);
            (void) fprintf(exportfile, "    (*((void (*)(%s)) (%s_CLASSPROCEDURES->routines[3])))(&%s_classheader,self);\n", proto, FinalParentName, FinalParentName);
        }
        (void) fprintf(exportfile, "}\n\n");

	if (usePrototypes) {
	    (void) fprintf(exportfile, "void %s__Destroy(", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID,");
	    (void) fprintf(exportfile, " struct %s *self)\n{\n", FinalClassName);
	}
	else {
	    (void) fprintf(exportfile, "void %s__Destroy(classID, self)\n", FinalClassName);
	    (void) fprintf(exportfile, "struct classheader *classID;\n");
	    (void) fprintf(exportfile, "struct %s *self;\n{\n", FinalClassName);
	}

	if(destroyplist) fprintf(exportfile, "    boolean reallydoit=TRUE;\n");
	for (mp = destroyplist; mp != NULL; mp = mp->nextdp)  {
	    if (mp->type==ptype_classproc && mp->name && mp->declaredIn)  {
		/* print out expanded version of call to superclass' finalize
  * procedure, so we don't depend on parents macros
		 */
		char proto[1024];

		if (usePrototypes) {
		    sprintf(proto, "struct classheader *, struct %s *", mp->declaredIn);
		}
		else {
		    proto[0] = '\0';
		}

		if(strcmp(FinalClassName, mp->declaredIn)!=0) {
		    (void) fprintf(exportfile, "\t/* %s_Destroyp(self); */\n", mp->declaredIn);
		    (void) fprintf(exportfile, "    if(!(*((boolean (*)(%s)) (%s_CLASSPROCEDURES->routines[4])))(&%s_classheader,self)) reallydoit=FALSE;\n", proto, mp->declaredIn, mp->declaredIn);
		} else fprintf(exportfile, "    if(!%s_Destroyp(self)) reallydoit=FALSE;\n", FinalClassName);
	    }
	}
	if(destroyplist) fprintf(exportfile, "    if(!reallydoit) return;\n");
	
        if (finalizeobject || FinalParentName[0])
            (void) fprintf(exportfile, "    %s__Finalize(classID, self);\n", FinalClassName);
        if (deallocate)  {
            (void) fprintf(exportfile, "    %s__Deallocate(classID, self);\n", FinalClassName);
        }
        else  {
            (void) fprintf(exportfile, "    free(self);\n");
        }
        (void) fprintf(exportfile, "}\n");
    }	/* bottom of if (classDefinition) */

    (void) fprintf(exportfile, "\nstruct classinfo *%s__GetClassInfo(classID, versionnumber", FinalClassName);
    (void) fprintf(exportfile, ")\nstruct classheader *classID;\n");
    (void) fprintf(exportfile, "unsigned long versionnumber;\n");
    (void) fprintf(exportfile, "{\n    if (versionnumber != class_VERSIONNOTKNOWN && versionnumber != %s_VERSION)  {\n", FinalClassName);
    (void) fprintf(exportfile, "\t(void) fprintf(stderr, \"Incompatible version of %s requested!\\n\");\n", FinalClassName);
/*    (void) fprintf(exportfile, "\treturn NULL;\n");  %%%%% think about this... */
    (void) fprintf(exportfile, "    }\n");
    (void) fprintf(exportfile, "    %s_classproceduretable.info = &%s_classinfo;\n", FinalClassName, FinalClassName);

    (void) fprintf(exportfile, "    return &%s_classinfo;\n", FinalClassName);
    (void) fprintf(exportfile, "}\n");
    (void) fprintf(exportfile, "\n");

    /* %%% try to remove this, please.  pgc */
    (void) fprintf(exportfile, "/* \n");
    (void) fprintf(exportfile, " * THIS IS NEEDED BECAUSE OF THE WAY makedo WORKS.\n");
    (void) fprintf(exportfile, " * HOPEFULLY, IT CAN BE REMOVED SOON.\n");
    (void) fprintf(exportfile, " */ \n");
    (void) fprintf(exportfile, " \n");

    if (strcmp(BaseFileName, FinalClassName) != 0) { /* put in stub for makedo */
	(void) fprintf(exportfile, "struct classinfo *%s__GetClassInfo(classID, versionnumber", BaseFileName);
	(void) fprintf(exportfile, ")\nstruct classheader *classID;\n");
	(void) fprintf(exportfile, "unsigned long versionnumber;\n");
	(void) fprintf(exportfile, "{\n");
	(void) fprintf(exportfile, "    return %s__GetClassInfo(classID, versionnumber", FinalClassName);
	(void) fprintf(exportfile, ");\n}\n");
    } else {
	(void) fprintf(exportfile, "/* this patch is not needed as the class name is the same as the file name */\n");
    }

    (void) fprintf(exportfile, " \n");
    (void) fprintf(exportfile, "/* \n");
    (void) fprintf(exportfile, " * END OF THIS PATCH. \n");
    (void) fprintf(exportfile, " */ \n");

    (void) fprintf(exportfile, " \n");
    (void) fprintf(exportfile, "#endif /* AUXMODULE */\n");

    return;

#undef getFuncName
}


/** 
 ** Emit an entry to include parent's file if this
 ** is not a package and this class has a parent.
 **/
static void GenerateParentInfo()

{
    /* tell folks were this stuff came from */
    outstr0("/* \n");
    outstr0(" * Include the superclass information\n");
    outstr0(" */ \n");

    if ((FinalParentName[0] != '\0') && classDefinition)  {
	outstr1("#define dontDefineRoutinesFor_%s\n",FinalParentName);
	outstr1("#include \"%s.ih\"\n", ParentBaseFileName);
	outstr1("#undef dontDefineRoutinesFor_%s\n\n",FinalParentName);
    } else {
	outstr1("/* %s has no super class */ \n", FinalClassName);
    }
    outstr0(" \n");
    outstr0(" \n");
}


/** 
 ** Emit data structure that is specific to this class.
 **/
static void GenerateClassStructure()
{
    struct InfoStruct *ThisPointer;	    /* used to reach data collected during parsing */


    outstr0("#ifndef class_StaticInfoOnly\n");
    /* tell folks were this stuff came from */
    outstr0("/* \n");
    outstr1(" * Data structure for %s \n", FinalClassName);
    outstr0(" */ \n");


    if (classDefinition)  {	    /* generate a data structure */
	outstr1("struct %s { \n", FinalClassName);
	if (FinalParentName[0] != '\0')  {
	    outstr0("    union {\n");
	}
	else  {
	    outstr0("    struct {\n");
	}
	outstr1("\tstruct basicobject_methods *%s_methods;\n", FinalClassName);

	/* emit all the superclass data structures */
	for (ThisPointer = ParentInfoHead; ThisPointer != NULL; ThisPointer = ThisPointer->next) {
	    outstr2("\tstruct %s %s;\n", ThisPointer->data, ThisPointer->data);
	}
	outstr0("    } header;");	/* bottom of existing data */

	/* now tack on new data for this class */
	for (ThisPointer = DataInfoHead; ThisPointer != NULL; ThisPointer = ThisPointer->next) {
	    outstr1("%s", ThisPointer->data);
	}
	outstr0("\n};\n\n");	/* close class data structure */
    } else {
	outstr1("/* %s is a package.  Packages don't have any data */ \n\n", FinalClassName);
    }


    outstr0(" \n");
    outstr0("/* \n");
    outstr1(" * End of data structure for %s.\n", FinalClassName);
    outstr0(" */ \n");
    outstr0(" \n\n");
    outstr0("#endif /* class_StaticInfoOnly */\n");
}


/** 
 ** Emit anything that was in the .ch file
 ** before or after the actual class or package
 ** definition.
 **/
static void GenerateExtra()
{
    struct InfoStruct *ThisPointer;	    /* used to reach data collected during parsing */

    outstr0("#ifndef class_StaticInfoOnly\n");
    /* tell folks were this stuff came from */
    outstr0("/* \n");
    outstr1(" * The following information was copied, as-is, from %s.\n", HeaderFileName);
    outstr0(" */ \n");
    outstr0(" \n");

    /* emit all the collected data */
    for (ThisPointer = ExtraInfoHead; ThisPointer != NULL; ThisPointer = ThisPointer->next) {
	outstr1("%s", ThisPointer->data);
    }

    /* all done with that stuff */

    outstr0("/* \n");
    outstr1(" * End of information copied from %s.\n", HeaderFileName);
    outstr0(" */ \n");
    outstr0(" \n");
    outstr0("#endif /* class_StaticInfoOnly */");
}


/** 
 ** Emit the standard prolog for the
 ** class system generated include files.
 **/
static void GenerateProlog()

{
    /* put in code to make sure this is only expanded once */
    outstr0(" \n");
    outstr1("#ifndef %s_DEFINED\n", FinalClassName);
    outstr1("#define %s_DEFINED 1\n", FinalClassName);
    outstr0(" \n");
    outstr0("#ifndef class_DEFINED\n");
    outstr0("#include <class.h>\n");
    outstr0("#endif /* class_DEFINED */\n");
    outstr0("/* \n");
    outstr0(" * This file was generated by the Andrew class system preprocessor.\n");
    outstr1(" * Don't make changes to this file as it is created from %s and will be\n", HeaderFileName);
    outstr1(" * overwritten.  Make changes to %s instead.\n", HeaderFileName);
    outstr0(" */ \n");
    outstr0(" \n");
}


/** 
 ** Emit the standard epilog for the
 ** class system generated include files.
 **/
static void GenerateEpilog()

{
}

static void PrintClassNamesFromInfo(descFile, info, parentInfo)
FILE *descFile;
struct InfoStruct *info;
struct InfoStruct *parentInfo;
{
    if (parentInfo != NULL) {
	PrintClassNamesFromInfo(descFile, parentInfo, parentInfo->next);
    }
    if (parentInfo != NULL) {
	fprintf(descFile, ", ");
    }
    fprintf(descFile, "%s", info->data);
}

static int CompareMethods(mp1, mp2)
struct methods **mp1;
struct methods **mp2;
{
    return (strcmp((*mp1)->name, (*mp2)->name));
}

static void GenerateDescription()
{
    FILE *descFile;
    struct methods *mp;
    char DescriptionFileName[1000];
    char *p;
    long i;
    long j;
    struct methods **routineTable;

    routineTable = (struct methods **) malloc(routineCount * sizeof(struct method *));

    for (mp = methodlist->next, i = 0; mp != NULL; mp = mp->next, i++) {
	routineTable[i] = mp;
    }

    qsort(routineTable, routineCount, sizeof(struct method *), CompareMethods);

    (void) sprintf(DescriptionFileName, "%s.desc", BaseFileName);

    descFile = fopen(DescriptionFileName, "w");

    if (descFile == NULL) {
	warning(WARNING_QUIET, "Could not open description file.");
	return;
    }

    fprintf(descFile, "%s: %s\nDeclared in: %s.ch\n", (classDefinition) ? "Class" : "Package", FinalClassName, BaseFileName);

    if (ParentInfoHead != NULL) {
	fprintf(descFile, "Subclass of: ");
	PrintClassNamesFromInfo(descFile, ParentInfoHead, ParentInfoHead->next);
	fprintf(descFile, "\n");
    }

    fprintf(descFile, "\n");

    for (i = 0; i < routineCount; i++) {
	mp = routineTable[i];
	if (mp->type==ptype_method && mp->macrodef == NULL){
	    fprintf(descFile, "Method:\t%s\n", mp->name);
	    fprintf(descFile, "\treturns:\t%s\n", (mp->methodtype != NULL) ? mp->methodtype : "void");
	    if ((p = mp->realargtypes) != NULL) {
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == ',') {
		    p++;
		}
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == '\0') {
		    p = "void";
		}
	    }
	    else {
		p = "error";
	    }
	    fprintf(descFile, "\targs:\t\t%s\n", p);
	    fprintf(descFile, "\tdeclared by:\t%s\n", mp->declaredIn);
	    fprintf(descFile, "\tdefined by:\t");
	    for (j = mp->definedInCnt - 1; j > 0; j--) {
		fprintf(descFile, "%s, ", mp->definedIn[j]);
	    }
	    fprintf(descFile, "%s\n", mp->definedIn[0]);
	    fprintf(descFile, "\tindex:\t\t%d\n\n", mp->index);
	}
    }
    fprintf(descFile, "\n");

    for (i = 0; i < routineCount; i++) {
	mp = routineTable[i];
	if (mp->type==ptype_method && mp->macrodef != NULL){
	    fprintf(descFile, "Macro Method:\t%s\n", mp->name);
	    if ((p = mp->realargtypes) != NULL) {
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == ',') {
		    p++;
		}
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == '\0') {
		    p = "void";
		}
	    }
	    else {
		p = "error";
	    }
	    fprintf(descFile, "\targs:\t\t%s\n", p);
	    fprintf(descFile, "\tdeclared by:\t%s\n", mp->declaredIn);
	    fprintf(descFile, "\tdefined by:\t");
	    for (j = mp->definedInCnt - 1; j > 0; j--) {
		fprintf(descFile, "%s, ", mp->definedIn[j]);
	    }
	    fprintf(descFile, "%s\n\n", mp->definedIn[0]);
	}
    }
   
    fprintf(descFile, "\n");

    for (i = 0; i < routineCount; i++) {
	mp = routineTable[i];
	if (mp->type==ptype_classproc && mp->defined){
	    fprintf(descFile, "Class Procedure:\t%s\n", mp->name);
	    fprintf(descFile, "\treturns:\t%s\n", (mp->methodtype != NULL) ? mp->methodtype : "void");
	    if ((p = mp->realargtypes) != NULL) {
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == ',') {
		    p++;
		}
		while (*p != '\0' && isspace(*p)) {
		    p++;
		}
		if (*p == '\0') {
		    p = "void";
		}
	    }
	    else {
		p = "error";
	    }
	    fprintf(descFile, "\targs:\t\t%s\n", p);
	    fprintf(descFile, "\tdefined by:\t");
	    for (j = mp->definedInCnt - 1; j > 0; j--) {
		fprintf(descFile, "%s, ", mp->definedIn[j]);
	    }
	    fprintf(descFile, "%s\n", mp->definedIn[0]);
	    fprintf(descFile, "\tindex:\t\t%d\n\n", mp->index);
	}
    }
   
    fprintf(descFile, "\n");

    for (i = 0; i < routineCount; i++) {
	mp = routineTable[i];
	if (mp->type==ptype_macro){
	    fprintf(descFile, "Macro:\t%s\n", mp->name);
	    fprintf(descFile, "\tdeclared by:\t%s\n", mp->declaredIn);
	    fprintf(descFile, "\tdefined by:\t");
	    for (j = mp->definedInCnt - 1; j > 0; j--) {
		fprintf(descFile, "%s, ", mp->definedIn[j]);
	    }
	    fprintf(descFile, "%s\n\n", mp->definedIn[0]);
	}
    }
   
    fprintf(descFile, "\n");

    fclose(descFile);
}
/** 
 ** Emit the dependencies.
 **/
static void GenerateDependencies()

{
struct InfoStruct *ThisPointer;	    /* used to reach data collected during parsing */

    /*
     * %%%% We shouldn't have to explicitly do this
     * for "class.h".  There should be a more general
     * way of doing this.
     */
    (void) OpenFile("class.h");    /* this will for a dependency for class.h */


    /* emit all the collected data */
    for (ThisPointer = DependsInfoHead; ThisPointer != NULL; ThisPointer = ThisPointer->next) {
	(void) fprintf(dependsfile, "%s %s: %s \n", ImportFileName, ExportFileName, ThisPointer->data);
    }

    /* don't forget us!! */
    (void) fprintf(dependsfile, "%s %s: %s \n", ImportFileName, ExportFileName, HeaderFileName);
}


/** 
 ** This routine will control the generation of 
 ** the .ih and .eh files.
 **/
static void GenerateFiles()

{
    if (DependsOnly) {
	GenerateDependencies();
    } else {
	if (generateHeaderFiles) {
	    GenerateProlog();		/* standard includes */
	    GenerateExtra();		/* the extra stuff from the .ch file */
	    GenerateParentInfo();		/* get to parent's info */
	    GenerateClassStructure();	/* class data structure */
	    GenerateRoutines();		/* generated routines, classheader, etc. */
	    GenerateEpilog();		/* paired with Prolog() */
	}
	if (generateDescriptionFile) {
	    GenerateDescription();
	}
    }
}


/* 
 * 
 * 
 * Routines to parse definition files.
 * 
 * 
 */


/**
 ** ???
 **/
static struct methods *searchmethods(str)
char *str;

{
    struct methods *mp;
    
    for (mp = methodlist->next; mp != NULL; mp = mp->next)  {
	if (*mp->name == *str && strcmp(mp->name, str) == 0) return mp;
    }
    return NULL;
}


/**
 ** Deal with the various class and method definitions.
 ** The last token read is used to the caller should read
 ** another token and use that value for further work.
 ** If an EOF is reached at any point we won't return
 ** as there is no way this isn't an error since we must
 ** find a "}" to close the class definition.
 **/
static void DoDefinitions(CurrentState, toplevel, ClassName, intoken)
enum ParseState	CurrentState;	/* what state the parser is in */
int toplevel;			/* TRUE if the top most definition file */
char *ClassName;		/* name of the currently processed class */
int intoken;			/* must keep track of this for caller */

{			    /* start of DoDefinitions */


struct methods *CurrentMethod;	    /* only used in top half */
int argcount;		/* ??? */
int curlen;		/* ??? */
int remlen;		/* ??? */
int curarglen;		/* ??? */
int remarglen;		/* ??? */
int currealarglen;		/* ??? */
int remrealarglen;		/* ??? */
int LastTokenType;	/* ??? */
int len;		/* ??? */
char *curstr;		/* ??? */
char *argstr;		/* ??? */
char *realargstr;		/* ??? */
char LastArg[1000];	/* ??? */
int ismacro;		/* ??? */
int token;		/* don't modify input parameters */
int FoundError;		/* used to keep track of multiple errors */


    /* initialize data */
    FoundError = 0;	/* start off with no errors */
    token = intoken;
    curstr = NULL;
    argcount = 0;


    ismacro = (CurrentState==class_InMacroMethods || CurrentState==class_InMacroOverrides || CurrentState==class_InMacros);

    CurrentMethod=searchmethods(yytext);
    if(CurrentMethod!=NULL && ismacro!=(CurrentMethod->macrodef!=NULL)){
	(void) fprintf(stderr,"Overrides must not mix macro/non-macro definitions (%s).\n",CurrentMethod->name);
	FoundError=1;
    }

    if (CurrentMethod!=NULL && CurrentState != class_InClassProcedures && CurrentState!=class_InMacros)  {
	if (CurrentState == class_InOverrides || CurrentState==class_InMacroOverrides)  {
	    CurrentMethod->defined = toplevel;
	    if (CurrentMethod->definedInCnt == CurrentMethod->definedInMax) {
		CurrentMethod->definedInMax *= 2;
		CurrentMethod->definedIn = (char **) realloc(CurrentMethod->definedIn, CurrentMethod->definedInMax * sizeof(char *));
	    }
	    CurrentMethod->definedIn[CurrentMethod->definedInCnt++] = ClassName;
	}
	else {
	    (void) fprintf(stderr, "Redefining existing method - %s\n", yytext);
	    FoundError = 1;
	}
    } else  {
	if (CurrentState == class_InOverrides || CurrentState==class_InMacroOverrides)  {
	    (void) fprintf(stderr, "Attempting to override a nonexistent method - %s\n", yytext);
	    (void) fprintf(stderr, "    Adding it as a normal method\n");
	    FoundError = 1;
	}

	if (CurrentMethod == NULL)  {
	    CurrentMethod = (struct methods *) malloc(sizeof(struct methods));
	    routineCount += 1;
	    methodend->next = CurrentMethod;
	    CurrentMethod->next = NULL;
	    methodend = CurrentMethod;
	    CurrentMethod->name = (char *) malloc(strlen(yytext) + 1);
	    strcpy(CurrentMethod->name, yytext);
	    CurrentMethod->definedInMax = 2;
	    CurrentMethod->definedIn = (char **) malloc(CurrentMethod->definedInMax * sizeof(char *));
	    CurrentMethod->definedIn[0] = ClassName;
	    CurrentMethod->definedInCnt = 1;
	    CurrentMethod->declaredIn = ClassName;
	    CurrentMethod->methodtype = NULL;
	    CurrentMethod->methodargs = NULL;
	    CurrentMethod->argtypes = NULL;
	    CurrentMethod->realargtypes = NULL;
	    CurrentMethod->argcount = 0;
	    CurrentMethod->errval=errval_Fail; /* default */
	    CurrentMethod->special=nametype==name_Destroyp;
	    if(nametype==name_Destroyp) {
		CurrentMethod->nextdp=destroyplist;
		destroyplist=CurrentMethod;
	    }
	}
	else if (CurrentState == class_InClassProcedures)  {
	    CurrentMethod->definedIn[0] = ClassName;
	    CurrentMethod->declaredIn = ClassName;
	    CurrentMethod->methodtype = NULL;
	    CurrentMethod->methodargs = NULL;
	    CurrentMethod->argtypes = NULL;
	    CurrentMethod->realargtypes = NULL;
	    CurrentMethod->argcount = 0;
	}

	CurrentMethod->defined = toplevel;
	switch(CurrentState){
	    case class_InClassProcedures:
		CurrentMethod->type=ptype_classproc;
		break;
	    case class_InMacros:
		CurrentMethod->type=ptype_macro;
		break;
	    case class_InMethods:
	    case class_InOverrides:
	    case class_InMacroMethods:
	    case class_InMacroOverrides:
		CurrentMethod->type=ptype_method;
		break;
	}
	CurrentMethod->inherited = ! toplevel;
    }

    if (nametype < name_RegularName)  {
	if (CurrentState != class_InClassProcedures)  {
	    (void) fprintf(stderr, "%s should only be a class procedure.\n", yytext);
	    FoundError = 1;
	}
	else if (toplevel)  {
	    switch (nametype)  {
		case name_InitializeObject:
		    initializeobject = TRUE;
		    initializeobjectdefined = TRUE;
		    break;
		case name_FinalizeObject:
		    finalizeobject = TRUE;
		    break;
		case name_Allocate:
		    allocate = TRUE;
		    break;
		case name_Deallocate:
		    deallocate = TRUE;
		    break;
		case name_InitializeClass:
		    initializeclass = TRUE;
		    break;
		case name_Destroyp:
		    destroyp = TRUE;
		    break;
	    }
	}
    }
    token = gettoken();
    if (token != class_LeftParen)  {
	(void) fprintf(stderr, "Missing left parenthesize in declaration of %s\n", CurrentMethod->name);
	(void) fprintf(stderr, "    Assuming no parameters\n");
	FoundError = 1;
    } else  {		/* if (token != class_LeftParen)  */
	int parenLevel=0;

	LastTokenType = token;
	remlen = curlen = INITIALARGSTRSIZE;
	curstr = (char *) malloc(curlen);

	if (usePrototypes || generateDescriptionFile) {
	    remarglen = curarglen = INITIALARGSTRSIZE;
	    argstr = (char *) malloc(curarglen);

	    remrealarglen = currealarglen = INITIALARGSTRSIZE;
	    realargstr = (char *) malloc(currealarglen);
	}

	if (CurrentMethod->type==ptype_method)  {
	    remlen -= 1;
	    *curstr = ',';
	}

	if (usePrototypes || generateDescriptionFile) {
	    if (CurrentMethod->type != ptype_macro) {
		remarglen -= 1;
		*argstr = ',';
		remrealarglen -= 1;
		*realargstr = ',';
	    }
	}

	argcount = 0;
	token = gettoken();
	if (token != class_RightParen)  {
	    char currentarg[10000];
	    int namePos = -1;

	    currentarg[0] = '\0';

	    while (TRUE)  {
		/* Scan args - last name before comma or right paren is the argument name */
		if (token == class_Comma || (token == class_RightParen && parenLevel==0))  {
		    if (LastTokenType != class_Name)  {
			/* Error */
		    }
		    if(parenLevel>0){
			(void) fprintf(stderr,"Missing right paren%s in the %d%s argument declaration of %s.\n", (parenLevel>1  ? "s" : ""), argcount+1, (argcount==0 ? "st" : (argcount==1 ? "nd" : (argcount==3 ? "rd" : "th"))), CurrentMethod->name);
			FoundError=1;
		    }
		    if ((len = strlen(LastArg)) + 2 > remlen)  {
			remlen += curlen;
			curstr = (char *) realloc(curstr, (curlen = 2*curlen));
		    }
		    strcpy(&curstr[curlen - remlen], LastArg);
		    remlen -= len;

		    if (usePrototypes || generateDescriptionFile) {
			if (namePos > 0) {
			    /* Remove identifier name */
			    char *p;

			    for (p = &currentarg[namePos + len]; *p != '\0'; p++) {
				currentarg[namePos] = *p;
				namePos++;
			    }
			    while (namePos > 0 && isspace(currentarg[namePos - 1])) {
				namePos--;
			    }

			    currentarg[namePos] = '\0';
			    
			    for (p = currentarg; isspace(*p); p++) {
			    }

			    if (*p == '\0') {
				strcpy(currentarg, " unknown");
				namePos = 8;
			    }

			    /* Now add it to the real argument list */

			    if (namePos + 2 > remrealarglen)  {
				remrealarglen += currealarglen;
				realargstr = (char *) realloc(realargstr, (currealarglen = 2*currealarglen));
			    }
			    strcpy(&realargstr[currealarglen - remrealarglen], currentarg);
			    remrealarglen -= namePos;

			    /* Determine if argument is a pointer to a structure*/
			    /* If so change it to be void * */

			    for (p = currentarg; *p != '\0' && isspace(*p); p++) {
			    }

			    if (strncmp(p, "struct ", 7) == 0 && index(p, '*') != NULL && index(p, '(') == NULL) {
				strcpy(currentarg, " void *");
				namePos = 7;
			    }

			    if (namePos + 2 > remarglen)  {
				remarglen += curarglen;
				argstr = (char *) realloc(argstr, (curarglen = 2*curarglen));
			    }
			    strcpy(&argstr[curarglen - remarglen], currentarg);
			    remarglen -= namePos;
			}
			else {
			    /* This is an error condition. Either the type or
			       the argument is missing */

			    (void) fprintf(stderr,"Missing type or identifier in the %d%s argument declaration of %s.\n", argcount+1, (argcount==0 ? "st" : (argcount==1 ? "nd" : (argcount==3 ? "rd" : "th"))), CurrentMethod->name);
			    FoundError=1;
			}
			namePos = -1;
			currentarg[0] = '\0';
		    }
			
		    argcount++;
		    if (token == class_Comma)  {
			strcpy(&curstr[curlen - remlen], ",");
			remlen -= 1;
			if (usePrototypes || generateDescriptionFile) {
			    strcpy(&argstr[curarglen - remarglen], ",");
			    remarglen -= 1;
			    strcpy(&realargstr[currealarglen - remrealarglen], ",");
			    remrealarglen -= 1;
			}
		    } else
			break;
		}
		else  {
		    strcat(currentarg, " ");
		    if(token == class_Name ||
		       token == class_Data ||
		       token == class_Class ||
		       token == class_Returns ||
		       token == class_Methods ||
		       token == class_Overrides ||
		       token == class_ClassProcedures) {
			char *str;

			if (nametype == name_ThisObject)
			    str = FinalClassName;
			else
			    str = yytext;

			strcpy(LastArg, str);
			LastTokenType = token;
			namePos = strlen(currentarg);
			strcat(currentarg, str);
		    }
		    else  {
			strcat(currentarg, yytext);

			if(token==class_RightParen)
			    parenLevel--;
			else if(token==class_LeftParen)
			    parenLevel++;
		    }
		}

		token = gettoken();
	    }
	}
    }
    if (CurrentMethod->methodargs)  {
	if (argcount != CurrentMethod->argcount)  {
	    (void) fprintf(stderr,"Mismatch in parameter list with previously defined defintion of the method %s\n", CurrentMethod->name);
	    FoundError = 1;
	}
	free(curstr);
    } else  {
	CurrentMethod->methodargs = (argcount)  ? curstr : "";
	CurrentMethod->argcount = argcount;
	if (usePrototypes || generateDescriptionFile) {
	    CurrentMethod->argtypes = (argcount)  ? argstr : "";
	    CurrentMethod->realargtypes = (argcount)  ? realargstr : "";
	}
    }

    if (ismacro) {
	/* macros just save the text up to the next non-escaped
	 * newline, but also automatically casts any occurances
	 * of "self" to the type in which the macro was originally
	 * defined.
	 */

#define CASTBEG "(&(self)->header." /* classname */
#define CASTEND ")"
#define CASTBEGL (sizeof(CASTBEG)-1)
#define CASTENDL (sizeof(CASTEND)-1)

	int size=1000,used=0, classnamelen=strlen(ClassName);
	char *def=malloc(size);

	while((token = getnexttoken()) != class_EOF && ((token != class_WhiteSpace) || (*yytext!='\n'))) {
	    char *str=(nametype==name_ThisObject ? FinalClassName : yytext);
	    int len=strlen(str);
	    int isself=(token==class_Name && nametype==name_Self && CurrentState!=class_InMacros);

	    /* we parenthesize the "self" token to
	     * make things consistent between our own
	     * and super class macros (i.e., a superclass
	     * has it's self parenthesized)
             */

	    if(isself)
		if(toplevel)
		    len=6; /* "(self)" */
		else
		    len=CASTBEGL+classnamelen+CASTENDL;

	    if(len+used+1>size){
		size+=size+len+1;
		def=realloc(def,size);
	    }

	    if(isself)
		if(toplevel)
		    strcpy(def+used,"(self)");
		else{
		    strcpy(def+used,CASTBEG);
		    strcpy(def+used+CASTBEGL,ClassName);
		    strcpy(def+used+CASTBEGL+
			   classnamelen,
			   CASTEND);
		}				    
	    else
		strcpy(def+used,str);

	    used+=len;
	}

	if(token==class_EOF) {
	    errorexit(EXITCODE_DEFINITION, "EOF encountered while processing macro in definition file");
	}

	CurrentMethod->macrodef=realloc(def,used+1);
    } else {	    /* if (ismacro) */
	/* a vague idea of the return type */
	short retType=0;
#define RT_UNS 1
#define RT_INT 2
#define RT_PTR 4
#define RT_VOID 8
#define RT_FLOAT 16
#define RT_LONG 32

	CurrentMethod->macrodef=NULL;
	while ((token = gettoken()) != class_EOF && token != class_Returns && token != class_Semi && token!=class_OnError)  {
	    if (token == class_EOF) {
		(void) fprintf(stderr, "EOF found where either a semicolon, \"returns\" or \"errval\" is expected\n");
	    } else {
		(void) fprintf(stderr, "%s found where either a semicolon, \"returns\" or \"errval\" is expected\n", yytext);
	    }
	    (void) fprintf(stderr, "   in declaration for %s - skipping word\n", CurrentMethod->name);
	    FoundError = 1;
	}
	if (token == class_Returns)  {
	    /* scan until you hit a semicolon putting resulting string into methodtype */
	    curlen = INITIALRETURNSTRSIZE;
	    curstr = (char *) malloc(curlen);
	    remlen = curlen;
	    token = gettoken();

	    while (token != class_Semi && token!=class_Comma)  {
		char *str=yytext;

		switch(token){
		    case class_Name:
			retType&=~RT_PTR;
			switch(nametype){
			    case name_ThisObject:
				str = FinalClassName; break;
			    case name_Unsigned:
				retType|=RT_UNS; break;
			    case name_Int:
			    case name_Boolean: /* defined in class.h */
				retType|=RT_INT; break;
			    case name_Long:
				retType=RT_LONG; break;
			    case name_Float:
				retType=RT_FLOAT; break;
			    case name_Pointer:
				retType=RT_PTR; break;
			    case name_Void:
				retType|=RT_VOID; break;
			}
			break;
		    case class_Other:
			if(*str=='*')
			    retType=RT_PTR;
			break;
		}

		if ((len = strlen(str))+2 > remlen)  {
		    remlen += curlen;
		    curstr = (char *) realloc(curstr, (curlen = 2*curlen));
		}
		strcpy(&curstr[curlen - remlen],str);
		remlen -= len;
		token = getnexttoken();
	    }

	    if(token==class_Comma){
		token=gettoken();
		if(token!=class_OnError){
		    (void) fprintf(stderr,"Expected \"onerror\" after the comma.\n");
		    FoundError=1;
		}
	    }

	    if (remlen == curlen) {
		(void) fprintf(stderr,"No return type given for method %s - assuming int\n", CurrentMethod->name);
		free(curstr);
		curstr="int";
		retType=RT_INT;
		CurrentMethod->errval=errval_MinusOne;
		FoundError = 1;
	    } else{
		if(CurrentMethod->type==ptype_classproc){
		    /* use some hueristics to decide failure
		     * mode for classprocedures
			 */
		    if(retType&RT_PTR)
			CurrentMethod->errval=errval_NULL;
		    else if((retType&(RT_INT|RT_LONG)) &&
			    !(retType&RT_UNS))
			CurrentMethod->errval=errval_MinusOne;
		    else
			CurrentMethod->errval=errval_Fail;
		}
	    }
	} else if(token==class_EOF){
	    errorexitparam(EXITCODE_DEFINITION, "End of file encountered while processing the declaration for %s ", CurrentMethod->name);
	} else {
	    curstr="void";
	    retType=RT_VOID;
	    CurrentMethod->errval=errval_Fail;
	}

	if(CurrentMethod->type==ptype_method &&
	   CurrentMethod->methodtype!=NULL &&
	   (CurrentMethod->retType!=retType || retType==0) &&
	   strcmp(CurrentMethod->methodtype,curstr)!=0){
	    (void) fprintf(stderr,
			   "Return type mismatch between %s and the method it is overriding (%s != %s).\n",
			   CurrentMethod->name,
			   CurrentMethod->methodtype,
			   curstr);
	    FoundError=1;
	}

	CurrentMethod->methodtype = curstr;
	CurrentMethod->retType=retType;

	if(token==class_OnError){
	    token=gettoken();
	    if(token==class_Name && nametype==name_Exit){
		token=gettoken();
		CurrentMethod->errval=errval_Fail;
	    }else if(token==class_Return){
		token=gettoken();
		if(strcmp(CurrentMethod->methodtype,"void")==0){
		    if(token!=class_Semi){
			(void) fprintf(stderr,"Void classprocedures cannot have an error return value specified; just use \"return\" by itself (%s).\n",CurrentMethod->name);
			FoundError=1;
		    }
		}else{
		    if(token!=class_Name)
			nametype=name_RegularName; /* just signal an error */
		    switch(nametype){
			case name_NULL:
			    CurrentMethod->errval=errval_NULL;
			    token=gettoken();
			    break;
			case name_MinusOne:
			    CurrentMethod->errval=errval_MinusOne;
			    token=gettoken();
			    break;
			case name_Exit:
			    CurrentMethod->errval=errval_Fail;
			    token=gettoken();
			    break;
			default:
			    (void) fprintf(stderr,"\"onerror return\" must be followed by one of: \"NULL\", \"-1\", \"exit\" for a non-void procedure (%s)\n",CurrentMethod->name);
			    FoundError=1;
		    }
		}
	    } else { 
		(void) fprintf(stderr,"\"onerror\" must be followed by either \"return\" or \"exit\" (%s).\n",CurrentMethod->name);
		FoundError=1;
	    }
	}
	if(token!=class_Semi){
	    (void) fprintf(stderr,"Expecting semicolon at the end of the declaration of %s.\n",CurrentMethod->name);
	    FoundError=1;
	}
    }

    if (FoundError != 0) {
	errorexit(EXITCODE_DEFINITION, " errors found while processing definition ");
    }
}


/**
 ** Parse the class definition header starting with the 
 ** class name and finishing with the "{".  Upon returning
 ** the caller will have to read a new token and that token 
 ** will be the token that follows the "{".  The ClassName,
 ** ParentClassName, ClassFileName, ParentClassFileName, and
 ** possible FinalClassName and FinalParentName will be 
 ** filled in and checked.   For packages the parent class items
 ** are left empty.
 **/
static void ParseHeader(toplevel, ClassName, ParentClassName, ClassNameKey, ParentClassNameKey)
int toplevel;
char *ClassName;
char *ParentClassName;
char *ClassNameKey;
char *ParentClassNameKey;

{
int token;

    /* clear these to start */
    ClassName[0] = '\0';
    ParentClassName[0] = '\0';
    ClassNameKey[0] = '\0';
    ParentClassNameKey[0] = '\0';
  

    /* get name of class name and stash that information */
    token = gettoken();
    if (token != class_Name)  {
	errorexit(EXITCODE_DEFINITION, "Missing class name in class/package definition");
    }
    (void) strcpy(ClassName, yytext);	/* move name to the proper place */
    if (toplevel) {			/* keep this forever if this is our class */
	(void) strcpy(FinalClassName, ClassName);
	strcpy(FinalClassNameKey, ClassName);
    }

    /* look for class name key if present */
    token = gettoken();
    if (token == class_LeftSquareBracket) { /* looks like the name key is specified */
	token = gettoken();
	if (token == class_Name) {
	    (void) strcpy(ClassNameKey, yytext);
	    if (toplevel) {
		strcpy(FinalClassNameKey, yytext);
	    }
	    token = gettoken();		/* get next bit of information */
	    if (token == class_RightSquareBracket) {
		token =	gettoken();	/* all OK, move on */
	    } else {
		errorexit(EXITCODE_DEFINITION, "didn't find ] in class name key specification");
	    }
	} else { /* oops! */
	    errorexit(EXITCODE_DEFINITION, "error in class name key specification");
	}
    }

    /* look for superclass info */
    if (token == class_Colon)  {
	/* Definition is a subclass of another class */

	if (!classDefinition)  {	/* quick check for packages */
	    errorexit(EXITCODE_DEFINITION, "Packages do not subclass other classes");
	}

	/* look for super class name and name key as above */
	token = gettoken();
	if (token != class_Name)  {
	    errorexit(EXITCODE_DEFINITION, "Missing superclass name in class definition");
	}
	(void) strcpy(ParentClassName, yytext);	    /* keep this */
	if (toplevel) {		/* keep this forever if this is our parent class */
	    (void) strcpy(FinalParentName, ParentClassName);
	}

	/* look for superclass name key if present */
	token = gettoken();
	if (token == class_LeftSquareBracket) { /* looks like the parent name key is specified */
	    token = gettoken();
	    if (token == class_Name) {
		(void) strcpy(ParentClassNameKey, yytext);
		token =	gettoken();		/* get next bit of information */
		if (token == class_RightSquareBracket) {
		    token = gettoken();	/* all OK, move on */
		} else {
		    errorexit(EXITCODE_DEFINITION, "didn't find ] in super class name key specification");
		}
	    } else { /* oops! */
		errorexit(EXITCODE_DEFINITION, "error in super class name key specification");
	    }
	}

	/* this had better be a "{" !! */
	if (token != class_LeftBrace)  {
	    errorexit(EXITCODE_DEFINITION, "Missing leftbrace in class/package declaration");
	}
    }


/* 
 * At this point, at least ClassName must be filled in.
 * Any other items that were present will also have values
 * other than the empty string.  This information can be
 * used to do some error checking and generate the 
 * missing pieces.
 */

/* now check and verify the names */


/* check these before taking defaults */
    if (toplevel && (! DependsOnly)) {
/*  turn off this test for now...pgc
	if (strcmp(ClassName, BaseFileName) != 0) {
	    warning(WARNING_QUIET, "class name does not match file name");
	}
*/
	if ((ClassNameKey[0] != '\0') && (strcmp(ClassNameKey, BaseFileName) != 0)) {
	    errorexit(EXITCODE_DEFINITION, "specified class name key does not match file name");
	}

	if ((ClassNameKey[0] == '\0') && (strlen(ClassName) > MAXFILENAMELEN)) {
	    warning(WARNING_SILENT, "long class names should use name keys");
	}

	if ((ParentClassNameKey[0] == '\0') && (strlen(ParentClassName) > MAXFILENAMELEN)) {
	    warning(WARNING_SILENT, "long super class names should use name keys");
	}

/* more tests could go here %%% */

    }

/* by default, fill in keys with names */
    if (ClassNameKey[0] == '\0') {
	(void) strcpy(ClassNameKey, ClassName);
    }
    if (ParentClassName[0] != '\0') {
	if (ParentClassNameKey[0] == '\0') {
	    (void) strcpy(ParentClassNameKey, ParentClassName);
	}
    }


/* now generate the base name to be used to access parent data */
    if (toplevel && (ParentClassNameKey[0] != '\0')) {
	(void) strcpy(ParentBaseFileName, ParentClassNameKey);
    }

/*
 * Now test the values.  Only test if this is the top level
 * and we are not making dependencies.  If we are making
 * dependencies we really don't care.
 */

    if (toplevel && (! DependsOnly)) {

	/* check that all classes have super classes */
	if (classDefinition && (ParentClassName[0] == '\0') && (! MakingBaseObject)) {
		warning(WARNING_SLOPPY, "this class has no superclass");
	}
	/* check that file name matches class name key */
	if (strcmp(BaseFileName, ClassNameKey)) {
	    warning(WARNING_SLOPPY, "file name does not match class name key");
	}

	/* check length of BaseFileName */
	if (strlen(BaseFileName) > /* MAXFILENAMELEN */ 12) {
	    warning(WARNING_SILENT, "long file names are not portable");
	}

/* more tests could go here %%% */

    }
}


/**
 ** Decide the next state for the parser depending on the 
 ** kind of token just received.
 **/
static void handleclasskeywords(intoken, CurrentState) 
int intoken;
enum ParseState	*CurrentState;	/* what state the parser is in */

{
int token;

    token = intoken;	/* don't alter passed parameters */

    if ((! classDefinition) && (token == class_Overrides || token==class_MacroOverrides || token==class_Methods || token==class_MacroMethods || token==class_Data)){
        errorexit(EXITCODE_DEFINITION, "packages can only have classprocedures and macros");
    }

    switch(token)  {
        case class_Overrides:
	    *CurrentState = class_InOverrides;
	    break;
	case class_ClassProcedures:
	    *CurrentState = class_InClassProcedures;
	    break;
	case class_Methods:
	    *CurrentState = class_InMethods;
	    break;
        case class_MacroMethods:
            *CurrentState=class_InMacroMethods;
            break;
        case class_MacroOverrides:
            *CurrentState=class_InMacroOverrides;
            break;
        case class_Macros:
            *CurrentState=class_InMacros;
            break;
	case class_Data:
	    *CurrentState = class_InData;
	    break;
	case class_RightBrace:
	    *CurrentState = class_Normal;
	    return;	    /* this one isn't followed by a : */
	default:
	    errorexitparam(EXITCODE_DEFINITION, "illegal keyword, %s, in class definition", yytext);
    }
    token = gettoken();
    if (token != class_Colon)  {
	errorexit(EXITCODE_DEFINITION, "missing colon after the keyword in the class definition");
    }
}

/**
 ** Update the method information retrieved for class.
 **/

static void UpdateClassInfo()
{
    long methodNum;
    long procNum;
    struct methods *mp;

    methodNum = 0;

    if (classDefinition) {
	procNum = NUMSPECIALPROCS;
    }
    else {
	procNum = 0;
    }
    
    for (mp = methodlist->next; mp != NULL; mp = mp->next) {
	if (mp->type == ptype_method && mp->macrodef == NULL) {
	    mp->index = methodNum++;
	}
	else if (mp->type == ptype_classproc && mp->defined) {
	    mp->index = procNum++;
	}
    }
}



/**
 ** Parse the input definition file.  If this is a subclass read the super
 ** class definition files too.  No data is emitted in this pass.  All the
 ** needed information is collected to be emitted during the generation 
 ** phase.
 **/
static void ParseFile(ThisFile, toplevel)
FILE *ThisFile;	    /* file from which we are currently reading the definitions */
int toplevel;	    /* TRUE if this is the first call to ParseFile() */

{							/* top of ParseFile() */
    int token;		/* only used in top half */
    char *ClassName;
    char ParentClassName[200];
    char ClassNameKey[MAXPATHLEN];
    char ParentClassNameKey[MAXPATHLEN];

    static enum ParseState	CurrentState;	/* what state the parser is in */
    int BraceLevel;		/* used to track nesting of {}'s in data section */


    ClassName =	(char *) malloc(200);	/* malloced so we can refer to it in the method structure */

    PushFile(ThisFile);		/* this will be pop'ed near the end of the this routine */



/* 
 * Since anything that is not a comment is supposed to be copied 
 * from the .ch to the .ih and .eh files we must save up all the 
 * data that comes before the class header.  After the class
 * header has been found and verified the the saved up data is 
 * sent to the .ih and .eh files if we are not making dependencies.
 */


/* 
 * Parse up to "class" or "package".  If 
 * this is the toplevel and we are not just making
 * a dependency list keep the data so we can emit it later.
 * the class or package token is left in token.
 */
    token = getnexttoken();	/* get first token */
    while((token != class_Class) && (token != class_Package)) {
	if (toplevel && (!DependsOnly)) {
	    AddExtraInfo(yytext);	/* save this for later */
	}
	token =	getnexttoken();	/* get next token */
    }
	
    /* pad this data */
    if (toplevel && (!DependsOnly)) {
	AddExtraInfo("\n\n");
    }
 

/*
 * Starting here parse off the class header consisting of:
 *   "class" classname {"["classplace"]"] [":" superclassname ["["superclassplace"]"]]  "{"
 *    or
 *   "package" classname [classplace] "{"
 * If a valid class header was found, open and initialize the Import and 
 * Export files and recursively call ParseFile() to get the superclass information.
 */

    if (toplevel) {
	classDefinition	= (token == class_Class);	/* remember what we are making */
    }

    if ((!toplevel) && (token == class_Package)) {	/* quick check for subclassing packages */
	    errorexit(EXITCODE_DEFINITION, "packages can not be subclassed");
    }

/*
 * After this call we will read a new token.  That token will be the token
 * that follows the { in the class definition header.
 */
    ParseHeader(toplevel, ClassName, ParentClassName, ClassNameKey, ParentClassNameKey);


/* now if this is a subclass, look in the superclass files */
    if (ParentClassName[0] != '\0')  {
	FILE *file;
	char FileName[MAXPATHLEN+1];

	(void) sprintf(FileName, "%s.ch", ParentClassNameKey);
	file = OpenFile(FileName);
	ParseFile(file, FALSE);
	(void) fclose(file);	    /* so what if this fails, we're only reading */
	AddSuperClassInfo(ParentClassName); /* stash this away for later */
    }

    if (DependsOnly) {
	goto DependsOnlyExit;
    }

/*
 * If we are making dependencies we will never get past this point of 
 * the routine.
 */


/*
 * We already parsed off the class header so we are now 
 * in the class portion, we have to call handleclasskeywords
 * to get in at the correct place.
 */

    /* get a new token and determine the CurrentState to start the loop */
    token = gettoken();	    
    token = retrievetoken(token);
    handleclasskeywords(token, &CurrentState);

    /* loop through the class definition until we get a "data:" or a "}"(class_Normal) */
    while ((CurrentState != class_InData) && (CurrentState != class_Normal)) {

	/* get token for next pass through the loop */
	token = gettoken();
	if (token == class_EOF) {
	    errorexit(EXITCODE_DEFINITION, "error in class definition file, EOF found before closing } ");
	}

	/* check for an obvious error */
	if ((token == class_Class) || (token == class_Package)) {
	    errorexit(EXITCODE_DEFINITION, "only one class or package is allowed per definition file ");
	}
	token =	retrievetoken(token);	/* make sure this isn't whitespace */
	if (token == class_Name)  {
	    DoDefinitions(CurrentState, toplevel, ClassName, token);
	} else {
	    handleclasskeywords(token, &CurrentState);
	}
    }

    /* if there is data to collect, collect it */
    if (CurrentState ==	class_InData) {
	BraceLevel = 0;	/* start at zero */
	token = getnexttoken();	/* get next token */
	/* only consider the right braces if we are not nested in {}'s */
	while(((token != class_RightBrace) || (BraceLevel > 0)) && (token != class_EOF)) {
	    if (token == class_LeftBrace) {
		BraceLevel += 1;
	    }
	    if (token == class_RightBrace) {
		BraceLevel -= 1;
		if (BraceLevel < 0) {
		    errorexit(EXITCODE_BUG, "!! error tracking {}s in data section!!");
		}
	    }
	    if (toplevel) {
		if (token != class_WhiteSpace && token != class_Comment)  {
		    initializeobject = TRUE;
		}
		AddClassDataInfo(yytext);	/* save this for later */
	    }
	    token = getnexttoken();	/* get next token */
	}
	if (token == class_EOF)	{
	    errorexit(EXITCODE_DEFINITION, "missing closing } in class definition");
	}
    }

    /* make sure there is a ";" after the "}" */
    token = gettoken();
    if (token != class_Semi) {
	errorexit(EXITCODE_DEFINITION, "missing ; after closing } of class definition");
    }


/* 
 * If this is the toplevel keep the data so we can emit it later.
 * When we get here there is a ";" in token.
 */
    token = getnexttoken();	/* get next token */
    while(token != class_EOF) {
	if (toplevel) {
	    AddExtraInfo(yytext);	/* save this for later */
	}
	token =	getnexttoken();	/* get next token */
    }
	
    /* pad this data */
    if (toplevel) {
	AddExtraInfo("\n\n");
    }
 

/*
 * Some quick checks for fairly obvious problems.
 */
    if (toplevel) {
	if (allocate != deallocate)  {
	    if (allocate)
		warning(WARNING_SILENT,"Allocate routine declared with no Deallocate routine\n");
	    else
		warning(WARNING_SILENT,"Deallocate routine declared with no Allocate routine\n");
	}

	if (initializeobjectdefined != finalizeobject)  {
	    if (finalizeobject)
		warning(WARNING_SILENT,"FinalizeObject routine declared with no InitializeObject routine\n");
	}
    }

DependsOnlyExit:
 
    PopFile();
    return;
}


/* 
 * 
 * 
 * Routines to handle the high level control of 
 * the preprocessor.
 * 
 * 
 */


/** 
 ** This is the top level of control for the 
 ** processing of the class definition file.
 **/
static void ProcessFile(ThisFile)
FILE *ThisFile;	    /* file from which we are currently reading the definitions */

{
    ParseFile(ThisFile,	TRUE);	/* TRUE indicates this is the toplevel file */
    UpdateClassInfo();
    GenerateFiles();		/* generate the .ih and .eh files */
}


/**
 ** Construct the various file names that will be needed
 ** using the file name from the command line.  After the
 ** names are generated, open the .ih and .eh files called
 ** importfile and exportfile respectively.
 **/
static void SetupFiles(HeaderFile)
FILE **HeaderFile;

{
    char *BasePointer;

    /* have to open this either way */
    *HeaderFile = fopen(&HeaderFileName[0], "r");
    if (*HeaderFile == NULL) {
	errorexitparam(EXITCODE_ACCESS, "can not open '%s' for reading", &HeaderFileName[0]);
    }


    /* make file names */

    /* point to first character of base filename */
    BasePointer = (char *) rindex(HeaderFileName, '/');
    if (BasePointer == NULL) {
	BasePointer = &HeaderFileName[0];
    } else {
	BasePointer++;
    }

    strcpy(&BaseFileName[0], BasePointer);

    /* strip off the ".ch" */
    BasePointer = (char *) rindex(BaseFileName, '.');
    if (BasePointer == NULL) {
	errorexit(EXITCODE_COMMAND, "class definition name does not end with '.ch'");
    }
    if ((strcmp(BasePointer, ".ch") != 0)) {
	errorexit(EXITCODE_COMMAND, "class definition name does not end with '.ch'");
    }
    *BasePointer = '\0';    /* chop off the end */

    /* now make other file names */
    (void) sprintf(ImportFileName, "%s.ih", BaseFileName);
    (void) sprintf(ExportFileName, "%s.eh", BaseFileName);

    if (DependsOnly) {
	if (DependsFileName[0] == '\0') {
	    dependsfile	= stdout;	    /* use stdout by default */
	} else {
	    dependsfile = fopen(&DependsFileName[0], "w");
	    if (dependsfile == NULL) {
		errorexitparam(EXITCODE_ACCESS, "can not open the dependency file, '%s', for writing", &DependsFileName[0]);
	    }
	}
    } else if (generateHeaderFiles) {
	importfile = fopen(&ImportFileName[0], "w");
	if (importfile == NULL) {
	    errorexitparam(EXITCODE_ACCESS, "can not open the .ih file, '%s', for writing", &ImportFileName[0]);
	}
	exportfile = fopen(&ExportFileName[0], "w");
	if (importfile == NULL) {
	    errorexitparam(EXITCODE_ACCESS, "can not open the .eh file, '%s', for writing", &ExportFileName[0]);
	}
    }
}


/**
 ** Close open files.  Check return codes to be certain
 ** everything went OK.
 **/
static void CleanupFiles(HeaderFile)
FILE *HeaderFile;

{
    (void) fclose(HeaderFile);	    /* so what if we can't close it, we're just reading */

    if (DependsOnly) {
	/* flush output */
	if (fflush(dependsfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error writing dependency file");
	}
	/* then make sure it closes OK */
	if (fclose(dependsfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error closing dependency file");
	}
    } else if (generateHeaderFiles) {
	/* flush output */
	if (fflush(importfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error writing import file");
	}
	if (fflush(exportfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error writing export file");
	}
	/* then make sure they close OK */
	if (fclose(importfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error closing import file");
	}
	if (fclose(exportfile) != 0) {
	    errorexit(EXITCODE_RESOURCES, "error closing export file");
	}
    }
}


/**
 ** Tell what the program is and what it does and exit
 **/
static void usage()

{
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "class -- Andrew class preprocessor\n");
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "  usage: class {[-h] [-i] [-Isearchpath] [-M] [-ddepfile] [-s] [-q] [-Q] [-B]} file\n");
    (void) fprintf(stderr, "    -h print this help message\n");
    (void) fprintf(stderr, "    -i remove current directory from front of search path\n");
    (void) fprintf(stderr, "    -I searchpath adds a directory to look in for the superclass definition\n");
    (void) fprintf(stderr, "    -M creates dependency information for a makefile\n");
    (void) fprintf(stderr, "    -d sends dependency information to depfile instead of stdout\n");
    (void) fprintf(stderr, "    -s relax error checking (allowing most old code to work) (sets -B)\n");
    (void) fprintf(stderr, "    -p generate prototypes for method/class procedure invocations\n");
    (void) fprintf(stderr, "    -q suppresses printing of warning messages to stderr\n");
    (void) fprintf(stderr, "    -Q suppresses all messages except for error messages\n");
    (void) fprintf(stderr, "    -B allows this class to not have a superclass\n");
    (void) fprintf(stderr, "    -D creates a description file classname.desc\n");
    (void) fprintf(stderr, "    -N do not generate .ih and .eh files (used with -D)\n");
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "    Please note: \n");
    (void) fprintf(stderr, "      -- if -d is used, -M must also be set.\n");
    (void) fprintf(stderr, "      -- the -d and -I switches allow no whitespace\n");
    (void) fprintf(stderr, "         between the switch and their argument.\n");
    (void) fprintf(stderr, "      -- null arguments for the -I switch are ignored.\n");
    (void) fprintf(stderr, "\n");
    errorexit(EXITCODE_COMMAND, "usage information has been sent to stderr");
}


/**
 ** Parse the arguments from the command line.
 **/
static void ParseArgs(argc, argv)
int argc;
char *argv[];

{				    /* start of ParseArgs() */
int i;	/* used to loop through array of args */

    for (i=1; i<argc; i++)  {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 'h':	/* print help message */
		    usage();
		    break;
		case 'M':	/* only look for superclasses */
		    if (argv[i][2] == '\0') {
			DependsOnly = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'd':	/* add directory to search paths */
		    if (argv[i][2] != '\0') {
			if (strlen(&argv[i][2]) < MAXPATHLEN) {
			    strcpy(&DependsFileName[0], &argv[i][2]);
			} else {
			    errorexit(EXITCODE_COMMAND, "the name of the dependency information file is too long");
			}
		    } else {
			usage();
		    };
		    break;
		case 'I':	/* add directory to search paths */
		    if (argv[i][2] != '\0') {	/* NULL include paths are ignored just like cpp does */
			AddDirectory(&argv[i][2]);
		    };
		    break;
		case 'i':	/* remove current directory from front of search list */
		    if (argv[i][2] == '\0') {
			IgnoreCurrentDirectory = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 's':	/* relax checking */
		    if (argv[i][2] == '\0') {
			SloppyMode = TRUE;
			MakingBaseObject = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'q':	/* be quiet */
		    if (argv[i][2] == '\0') {
			QuietMode = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'p':	/* generate prototypes */
		    if (argv[i][2] == '\0') {
			usePrototypes = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'D':
		    if (argv[i][2] == '\0') {
			generateDescriptionFile = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'N':
		    if (argv[i][2] == '\0') {
			generateHeaderFiles = FALSE;
		    } else {
			usage();
		    };
		    break;

		case 'Q':	/* be silent */
		    if (argv[i][2] == '\0') {
			SilentMode = TRUE;
			QuietMode = TRUE;
		    } else {
			usage();
		    };
		    break;
		case 'B':	/* allow class with no superclass */
		    if (argv[i][2] == '\0') {
			MakingBaseObject = TRUE;
		    } else {
			usage();
		    };
		    break;
		default:	/* complain */
		    usage();
	    };
	} else {    /* first char not a '-', it must be the file name */
	    if (HeaderFileName[0] == '\0') {
		if (strlen(argv[i]) < MAXPATHLEN) {
		    (void) strcpy(&HeaderFileName[0], argv[i]);
		} else {
		    errorexit(EXITCODE_COMMAND, "the name of the definition file is too long");
		}
	    } else {	/* a file name was already found */
		errorexit(EXITCODE_COMMAND, "class can only process one file at a time");
	    }
	}
    };

/* check to see that the file name was given */
    if (HeaderFileName[0] == '\0') {
	if (argc == 1) {    /* special case for no arguments */
	    usage();
	}
	errorexit(EXITCODE_COMMAND, "no file name was given");
    }

/* check for valid use of -i and -I */
    if (IgnoreCurrentDirectory && (NumDirectories <= 0)) {
	errorexit(EXITCODE_COMMAND, "must have at least one -I switch if -i is used");
    }
}


/**
 ** Initialize global variables.
 **/
static void GlobalInit()
{
    methodlist = &methodhead;
    methodend = &methodhead;

    importfile = NULL;
    exportfile = NULL;

    BaseFileName[0] = '\0';	/* initialize to empty strings */
    HeaderFileName[0] = '\0';
    ImportFileName[0] = '\0';
    ExportFileName[0] = '\0';

    ParentBaseFileName[0] = '\0';
    DependsFileName[0] = '\0';

    ParentInfoHead = NULL;   /* used to keep track of superclasses */
    ParentInfoTail = NULL;

    DependsInfoHead = NULL;   /* used to keep track of dependencies */
    DependsInfoTail = NULL;

    DataInfoHead = NULL;   /* used to keep track of class data */
    DataInfoTail = NULL;

    ExtraInfoHead = NULL;   /* used to keep track other stuff in .ch file */
    ExtraInfoTail = NULL;

    initializeobject = FALSE;
    initializeobjectdefined = FALSE;
    finalizeobject = FALSE;
    allocate = FALSE;
    deallocate = FALSE;

    initializeclass = FALSE;
    destroyp = FALSE;
    destroyplist = NULL;
    
    NumDirectories = 0;	    /* start off with no added search directories */

    DependsOnly = FALSE;
    QuietMode = FALSE;
    SilentMode = FALSE;
    SloppyMode = FALSE;
    IgnoreCurrentDirectory = FALSE;
    MakingBaseObject = FALSE;
    usePrototypes = FALSE;

    FinalClassName[0] = '\0';
    FinalParentName[0] = '\0';
}



/**
 ** Parse args, process file, the ususal stuff...
 **/
int main(argc, argv)
int argc;
char *argv[];

{
FILE *HeaderFile;	    /* stream for the base definition file */

    /* global initializations */
    GlobalInit();

    /* initialization of signal catching code */
#ifdef POSIX_ENV
    handler.sa_handler = errorhandler;
    sigemptyset(&handler.sa_mask);
    handler.sa_flags = 0;

    sigaction(SIGHUP, &handler, NULL);
    sigaction(SIGINT, &handler, NULL);
    sigaction(SIGPIPE, &handler, NULL);
    sigaction(SIGTERM, &handler, NULL);
#else
    handler.sv_handler = errorhandler;
    handler.sv_mask = 0;
    handler.sv_onstack = 0;

    sigvec(SIGHUP, &handler, NULL);
    sigvec(SIGINT, &handler, NULL);
    sigvec(SIGPIPE, &handler, NULL);
    sigvec(SIGTERM, &handler, NULL);
#endif


/*
 * Now, down to business.
 */

    ParseArgs(argc, argv);	/* if errors are found this doesn't return */
    SetupFiles(&HeaderFile);	/* construct names and open proper files */
    ProcessFile(HeaderFile);	/* do the work */
    CleanupFiles(HeaderFile);	/* clean up streams */

    return EXITCODE_OK;	/* if parsefile() returns it must have succeeded */
}
