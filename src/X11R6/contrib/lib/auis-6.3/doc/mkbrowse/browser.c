/* ********************************************************************** *\
 *          Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/doc/mkbrowse/RCS/browser.c,v 1.8 1993/10/13 20:09:58 gk5g Exp $";
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

#ifdef FLEX_ENV
extern char *yytext;
#else
extern char yytext[];	    
#endif

struct EntryStruct {
    char * Name;
    char * superclassName;
    struct EntryStruct *Next;
    struct EntryStruct *mysubClasses;
} *first=NULL;

int entrycount=0;
/*
 * pick up parameters and constants
 */
#include <browserpp.h>

/* This is a complete bastardization of the class preprocessor to simply take a list of .ch files and generate an org datastream giving the inheritance structure, it may be that this functionality should be merged into the real class program.
  -Rob Ryan (rr2b@andrew.cmu.edu) */
/* for more complete comments see the real class.c */

/*
 * Externally visible data:
 */
int nametype;	    /* used to communicate with lexer */

static char BaseFileName[MAXPATHLEN+1];		/* name of file with no extension */
static char HeaderFileName[MAXPATHLEN+1];	/* name of .ch file */

static int SilentMode;			/* TRUE if we are silent except for errors */
static int QuietMode;			/* TRUE if we are not issuing warnings */
static int SloppyMode;			/* TRUE if we are allowing "questionable" code to work */
static int IgnoreCurrentDirectory;	/* TRUE if current directory is not searched first by default */
static int MakingBaseObject=TRUE;		/* TRUE if we are allowed to make a class with no superclass */

static char *DirectoryList[MAXINCLUDEPATHS];	/* place to store pointers to include directories */
static int NumDirectories;		/* number of these pointers */

static char FinalClassName[200];	/* the name of the class or package being created */
static char FinalParentName[200];	/* this class's parent or '\0' */
static int classDefinition;		/* TRUE if building a class, FALSE for package */


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


    return file;
}







/* 
 * 
 * 
 * Routines to parse definition files.
 * 
 * 
 */


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
    }

    /* look for class name key if present */
    token = gettoken();
    if (token == class_LeftSquareBracket) { /* looks like the name key is specified */
	token = gettoken();
	if (token == class_Name) {
	    (void) strcpy(ClassNameKey, yytext);
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
    if (toplevel ) {
/*  turn off this test for now...pgc
	if (strcmp(ClassName, BaseFileName) != 0) {
	    warning(WARNING_QUIET, "class name does not match file name");
	}
*/
	if ((ClassNameKey[0] != '\0') && (strcmp(ClassNameKey, BaseFileName) != 0)) {
	    errorexit(EXITCODE_DEFINITION, "specified class name key does not match file name");
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

struct EntryStruct *AddClassEntry(first, entrycount, name, superclass)
struct EntryStruct *first;
int *entrycount;
char *name, *superclass;
{
    struct EntryStruct *new=(struct EntryStruct *)malloc(sizeof(struct EntryStruct));
    if(new==NULL) return first;
    new->Name=name;
    new->superclassName=superclass;
    new->Next=first;
    new->mysubClasses=NULL;
    (*entrycount)++;
    return new;
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
    char *ParentClassName;
    char ClassNameKey[MAXPATHLEN];
    char ParentClassNameKey[MAXPATHLEN];

    static enum ParseState	CurrentState;	/* what state the parser is in */
    int BraceLevel;		/* used to track nesting of {}'s in data section */


    ClassName =	(char *) malloc(200);	/* malloced so we can refer to it in the method structure */
    ParentClassName=(char *)malloc(200);

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
	if (toplevel ) {
	}
	token =	getnexttoken();	/* get next token */
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

    first=AddClassEntry(first,&entrycount, ClassName, ParentClassName);
    PopFile();
    return;
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
}


/**
 ** Close open files.  Check return codes to be certain
 ** everything went OK.
 **/
static void CleanupFiles(HeaderFile)
FILE *HeaderFile;

{
    (void) fclose(HeaderFile);	    /* so what if we can't close it, we're just reading */

}


/**
 ** Tell what the program is and what it does and exit
 **/
static void usage()

{
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "mkbrowse -- Generates an Org tree of the Class hierarchy\n");
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "  usage: mkbrowse *.ch>destfile\n");
    (void) fprintf(stderr, "    -h print this help message\n");
    (void) fprintf(stderr, "    -i remove current directory from front of search path\n");
    (void) fprintf(stderr, "    -I searchpath adds a directory to look in for the superclass definition\n");
    (void) fprintf(stderr, "    -s relax error checking (allowing most old code to work) (sets -B)\n");
    (void) fprintf(stderr, "    -q suppresses printing of warning messages to stderr\n");
    (void) fprintf(stderr, "    -Q suppresses all messages except for error messages\n");
    (void) fprintf(stderr, "    -B allows this class to not have a superclass\n");
    (void) fprintf(stderr, "\n");
    (void) fprintf(stderr, "    Please note: \n");
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
		

		
		case 'Q':	/* be silent */
		    if (argv[i][2] == '\0') {
			SilentMode = TRUE;
			QuietMode = TRUE;
		    } else {
			usage();
		    };
		    break;
		
		default:	/* complain */
		    usage();
	    };
	} else { 
	    if (strlen(argv[i]) < MAXPATHLEN) {
		FILE *HeaderFile;
		(void) strcpy(&HeaderFileName[0], argv[i]);
		fprintf(stderr,"Processing header file %s\n",HeaderFileName);
		SetupFiles(&HeaderFile);
		ParseFile(HeaderFile,TRUE);
		CleanupFiles(HeaderFile);	/* clean up streams */
	    } else {
		errorexit(EXITCODE_COMMAND, "the name of the definition file is too long");
	    }
	}
    };

/* check to see that the file name was given */
    if (HeaderFileName[0] == '\0') {
	if (argc == 1) {    /* special case for no arguments */
	    FILE *HeaderFile;
	    char buf[1024];
	    while(!feof(stdin) && fgets(buf, sizeof(buf)-1, stdin)) {
		char *p=index(buf, '\n');
		if(*p='\0') *p='\0';
		strcpy(&HeaderFileName[0], buf);
		fprintf(stderr,"Processing header file %s\n",HeaderFileName);
		SetupFiles(&HeaderFile);
		ParseFile(HeaderFile,TRUE);
		CleanupFiles(HeaderFile);	/* clean up streams */
		 }
	} else errorexit(EXITCODE_COMMAND, "no file name was given");
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
    
    BaseFileName[0] = '\0';	/* initialize to empty strings */
    HeaderFileName[0] = '\0';
    
    NumDirectories = 0;	    /* start off with no added search directories */

    QuietMode = FALSE;
    SilentMode = FALSE;
    SloppyMode = FALSE;
    IgnoreCurrentDirectory = FALSE;
    
    MakingBaseObject = TRUE;
    FinalClassName[0] = '\0';
    FinalParentName[0] = '\0';
}


static int compareclasses(x,y)
struct EntryStruct **x,**y;
{
    return strcmp((*x)->Name,(*y)->Name);
}

static int CheckSuperClass(list,count,superclass)
struct EntryStruct **list;
int count;
char *superclass;
{
    int top=0, bot=count-1;
    int cmp;
    
    while(bot!=top) {
	cmp=strcmp(list[(top+bot)/2]->Name, superclass);
	if(!cmp) return (top+bot)/2;
	if(cmp>0) bot=(top+bot-1)/2;
	else top=(top+bot+1)/2;
    }
    if(!strcmp(list[top]->Name, superclass)) return top;
    else return -1;
}

static void ShowSubClasses(t,depth)
struct EntryStruct *t;
int depth;
{
    struct EntryStruct *TempEntry;
    int i=depth;
    if(depth>0) while(i--) putchar(' ');
    puts(t->Name);
    if(depth<0) printf("{\n");
    for(TempEntry=t->mysubClasses;TempEntry;TempEntry=TempEntry->Next) {
	ShowSubClasses(TempEntry, depth<0?-1:depth+4);
    }
    if(depth<0) printf("}\n");
}
	
	
struct EntryStruct **sortedlist=NULL;


/**
 ** Parse args, process file, the ususal stuff...
 **/
int main(argc, argv)
int argc;
char *argv[];

{
    int i;
    struct EntryStruct *lasttop=NULL, *TempEntry;
    /* global initializations */
    GlobalInit();

/*
 * Now, down to business.
 */

    ParseArgs(argc, argv);	/* if errors are found this doesn't return */
    i=entrycount;
    sortedlist=(struct EntryStruct **)malloc(sizeof(struct EntryStruct *)*entrycount);
    for(TempEntry = first; TempEntry;) {
	i--;
	sortedlist[i]=TempEntry;
	TempEntry = TempEntry->Next;
	sortedlist[i]->mysubClasses=NULL;
	sortedlist[i]->Next=NULL;
    }
    qsort(sortedlist, entrycount, sizeof(struct EntryStruct *), compareclasses);
    for(i=0;i<entrycount;i++) {
	int index;
	if(sortedlist[i]->superclassName[0]==0) {
	    sortedlist[i]->Next=lasttop;
	    lasttop=sortedlist[i];
	    continue;
	}
	index=CheckSuperClass(sortedlist, entrycount, sortedlist[i]->superclassName);
	if(index<0) {
	    fprintf(stderr,"Superclass %s of %s doesn't exist!\n",sortedlist[i]->Name, sortedlist[i]->superclassName);
	    continue;
	}
	sortedlist[i]->Next=sortedlist[index]->mysubClasses;
	sortedlist[index]->mysubClasses=sortedlist[i];
    }
    printf("\\begindata{org,42}\n");
    printf("ATK\n{");
    while(lasttop) {
	ShowSubClasses(lasttop, -1);
	lasttop=lasttop->Next;
    }
    printf("}\n\\enddata{org,42}\n");
    return EXITCODE_OK;	/* if parsefile() returns it must have succeeded */
}
