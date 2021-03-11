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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/type/RCS/type.c,v 1.18 1994/01/06 15:39:37 wjh Exp $";
#endif





/*
 *    $Log: type.c,v $
 * Revision 1.18  1994/01/06  15:39:37  wjh
 * move initialization from parse.gra to type.c
 *
 * Revision 1.17  1993/12/15  19:19:52  wjh
 * fixed buffer lengths to make overrun less likely
 * 	ignore errors in reading from CLASSPATH entries
 * 	changed temp file names to end in .c  --TELMAT
 * 	fixed core-dump bugs in reading superclass .ch files
 *
 * Revision 1.16  1993/08/25  20:40:17  susan
 * backed out Telmat changes
 *
 * Revision 1.15  1993/08/20  20:23:03  susan
 * Changes submitted by Telmat Informatique
 *
 * Revision 1.14  1993/05/04  01:23:33  susan
 * RCS Tree Split
 *
 * Revision 1.12.1.1  1993/02/02  02:57:40  rr2b
 * new R6tape branch
 *
 * Revision 1.12  1992/12/15  21:37:21  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.11  1992/12/14  20:48:33  rr2b
 * disclaimerization
 *
 * Revision 1.10  1992/12/02  21:20:28  wjh
 * remove static loading of parse, sym, and type from chtest
 * improved messages from chtest
 * cause type_Declare... classprocedures to properly return error message
 * fix bogus declaration of grammarScope and garbageScope (was erroneously 'static')
 * .
 *
 * Revision 1.9  1992/11/26  02:33:17  wjh
 * converted to bison/parse and tlex
 * chtest: added -t switch to dump parse transitions
 *
 * .
 *
 * Revision 1.8  91/09/12  16:27:44  bobg
 * Update copyright notice and rcsid
 * 
 * Revision 1.7  1990/07/11  15:40:54  rr2b
 * fixed more null dereferences
 *
 * Revision 1.6  90/04/11  15:00:14  gk5g
 * Apply fix submitted wrt bugID 1214.
 * 
 * Revision 1.5  89/09/27  15:41:30  wjh
 * utilize variable 'touch' to avoid compiler warning
 * 
 * Revision 1.4  89/09/11  08:28:16  ghoti
 * fix illegal pointer combinations
 * 
 * Revision 1.3  89/08/26  18:20:45  gb17
 * finished adding type_Reset
 * 
 * Revision 1.2  89/08/25  14:19:02  gb17
 * Reset classprocedure added
 * 
 * Revision 1.1  89/08/22  15:29:54  wjh
 * Initial revision
 * 
 */


#include <ctype.h>
#include <setjmp.h>

#include <dataobj.ih>
#include <smpltext.ih>
#include <text.ih>
#include <sym.ih>
#include <environ.ih>

#include <tlex.ih>
#include <parse.ih>  /* the parser object */

#include <type.eh>
#include <parse.h>   /* internal definitions */
#include <type.h>

#include <err.h>


/* declarations for parser and lex 
		(referenced from type.tlc and parse.gra.tab.c) */

static struct tlex *lex;
static struct parse *parser;	

struct text *textBuf;
char *GetLineNumber();

sym_ScopeType grammarScope = sym_GLOBAL;  /* reserved words  */
sym_ScopeType garbageScope = sym_GLOBAL;  /* most ids and symbols */

static boolean foldtnames;	/* if set, yylex setTNAME will 
				be folded into setID. */
static struct toksym *toksymproto;	/* type for making ID symbols */

#include <parse.gra.tab.h>	/* token name defines */
#include <parse.ih>		/* parse object */
#include <parse.gra.tab.c>	/* parse tables */
#include <parsedesc.h>		/* declare parse_description */
#include <parsepre.h>		/* begin function 'reduceActions' */
#include <parse.gra.act>	/* body of function 'reduceActions' */
#include <parsepost.h>		/* end of function 'reduceActions' */

#include <type.tlc>	/* lexer */


/*******************************************************************************\
 *
 *	GLOBAL DATA
 *
\*******************************************************************************/

struct type *proto;

struct type_ctypes ctypes;
struct type *basicobject_methods;

sym_ScopeType tempScope;

long anonNum;	/* used to name labelless types */

sym_ScopeType structs, unions, enums, typedefs, classes;
sym_ScopeType typeScope;

char *parse_error;


/******************************************************************************\
 *
 *	PRIVATE FUNCTIONS
 *
\*******************************************************************************/


/* GETDECLARATION HELPER ROUTINES */

/* static buffer */

#define	decl_size 10240			/* size of the declaration buffer */
static char decl_buf[decl_size+1];	/* the declaration buffer */
static long decl_bufPos;

/* append a string to the declaration buffer */

void
appendbuf(str)
char * str;
{
    while(*str && decl_bufPos < decl_size)
	decl_buf[decl_bufPos++] = *(str++);

    decl_buf[decl_bufPos] = '\0';
}

/* given a field in a property list, append the definition of that and all subsequent fields.
 */

appendfields(self, depth)
struct type * self;
long depth;
{
    int i;

    if (self == NULL)
	return;

    appendfields(type_GetPrev(self), depth);

    for	(i = 0;	i < depth; ++i)   /* indent */
	appendbuf("\t");
    type_GetDeclaration_(self, 0, depth);
    appendbuf(";\n");
}

/* given the structure associated with an object, append the fields except for the header.
 */

appenddata(self, depth)
struct type * self;
long depth;
{
    int i;

    if (self == NULL || type_GetPrev(self) == NULL)
	return;

    appenddata(type_GetPrev(self), depth);

    for	(i = 0;	i < depth; ++i)   /* indent */
	appendbuf("\t");
    type_GetDeclaration_(self, 0, depth);
    appendbuf(";\n");
}

/* same as above, but use comma separated enum format */

appendconsts(self, depth)
struct type * self;
long depth;
{
    int i;

    if (self == NULL)
	return;

    appendconsts(type_GetPrev(self), depth);

    for	(i = 0;	i < depth; ++i)   /* indent */
	appendbuf("\t");

    type_GetDeclaration_(self, 0, depth);

    if (type_GetPrev(type_GetLast(self)) != self)
	/* self is not the last parameter in the list, so append comma */
	appendbuf(",");

    appendbuf("\n");
}


/* same as above, but use comma separated parameter format */

appendparams(self, depth)
struct type * self;
long depth;
{
    if (self == NULL)
	return;

    appendparams(type_GetPrev(self), depth);

    type_GetDeclaration_(self, 0, depth);

    if (type_GetPrev(type_GetLast(self)) != self)
	/* self is not the last parameter in the list, so append comma */
	appendbuf(", ");
}

/* recursively construct the declaration */

type_GetDeclaration_(self, opsPos, depth)
struct type * self;
long opsPos;
long depth;
{
    long i;
    char buf[1000];

    if (self == NULL)
	return;

    switch (type_GetOps(self)[opsPos])	{	/* append stuff for opsPos */

	case type_struct:
	    appendbuf("struct");
	    if (type_GetName(self)[0] != '$') {
		appendbuf(" ");
		appendbuf(type_GetName(self));
	    } else if (depth != 0)
		appendbuf(" { ... }");

	    if (depth == 0) {
		appendbuf(" {\n");
		appendfields(type_GetPrev(self), depth+1);
		for (i = 0; i <	depth; ++i)	    /* indent for '}' */
		    appendbuf("\t");
		appendbuf("}");
	    }

	    break;

	case type_union:
	    appendbuf("union");
	    if (type_GetName(self)[0] != '$') {
		appendbuf(" ");
		appendbuf(type_GetName(self));
	    } else if (depth != 0)
		appendbuf(" { ... }");

	    if (depth == 0) {
		appendbuf(" {\n");
		appendfields(type_GetPrev(self), depth+1);
		for (i = 0; i <	depth; ++i)	    /* indent for '}' */
		    appendbuf("\t");
		appendbuf("}");
	    }

	    break;

	case type_enum:
	    appendbuf("enum");
	    if (type_GetName(self)[0] != '$') {
		appendbuf(" ");
		appendbuf(type_GetName(self));
	    } else if (depth != 0)
		appendbuf(" { ... }");

	    if (depth == 0) {
		appendbuf(" {\n");
		appendconsts(type_GetPrev(self), depth+1);
		for (i = 0; i <	depth; ++i)	    /* indent for '}' */
		    appendbuf("\t");
		appendbuf("}");
	    }

	    break;

	case type_param:
	case type_field:
	    if (type_GetSuper(self) != ctypes.voidType) {
		type_GetDeclaration_(type_GetSuper(self), 0, depth+1);
		appendbuf(" ");
	    }
	    type_GetDeclaration_(self, opsPos+1, depth);
	    break;

	case type_method:
	case type_classmethod:
	    if (type_GetSuper(self) != ctypes.voidType) {
		appendbuf("\t");
		type_GetDeclaration_(type_GetSuper(self), 0, depth+1);
		appendbuf("\n");
	    }
	case type_macro:
	case type_classmacro:
	    appendbuf("\t");
	    type_GetDeclaration_(self, opsPos+1, depth);
	    appendbuf("(");
	    appendparams(type_GetPrev(self), depth+1);
	    appendbuf(")");
	    break;
	    
	case type_const:
	    type_GetDeclaration_(self, opsPos+1, depth);
	    sprintf(buf, " = %ld", type_GetVal(self));
	    appendbuf(buf);
	    break;

	case type_typedef:
	    if (depth == 0) {
		appendbuf("typedef ");
		type_GetDeclaration_(type_GetSuper(self), 0, depth+1);
		appendbuf(" ");
		type_GetDeclaration_(self, opsPos+1, depth);
	    } else 
		appendbuf(type_GetName(self));
	    break;

	case type_prim:
	    type_GetDeclaration_(self, opsPos+1, depth);
	    break;

	case type_class:
	case type_package:

	    if (type_GetOps(self)[opsPos] == type_class)
		appendbuf("class ");
	    else
		appendbuf("package ");

	    appendbuf(type_GetName(self));

	    if (type_GetSuper(self) != ctypes.basicobject) {
		appendbuf(" : ");
		appendbuf(type_GetName(type_GetSuper(self)));
	    }

	    appendbuf(" {\n");

	    if (type_GetClassInfo(self, noMacros) > 0) {
		appendbuf("\nmacromethods:\n");
		for (i = 0; i < type_GetClassInfo(self, noMacros); ++i) {
		    appendbuf("\n");
		    type_GetDeclaration_(type_GetClassInfo(self, macros)[i], 0, depth+1);
		    appendbuf("\n");
		}
	    }

	    if (type_GetClassInfo(self, noMethods) > 0) {
		appendbuf("\nmethods:\n");
		for (i = 0; i < type_GetClassInfo(self, noMethods); ++i) {
		    appendbuf("\n");
		    type_GetDeclaration_(type_GetClassInfo(self, methods)[i], 0, depth+1);
		    appendbuf(";\n");
		}
	    }

	    if (type_GetClassInfo(self, noClassMacros) > 0) {
		appendbuf("\nmacros:\n");
		for (i = 0; i < type_GetClassInfo(self, noClassMacros); ++i) {
		    appendbuf("\n");
		    type_GetDeclaration_(type_GetClassInfo(self, classMacros)[i], 0, depth+1);
		    appendbuf("\n");
		}
	    }

	    if (type_GetClassInfo(self, noClassMethods) > 0 ) {
		appendbuf("\nclassprocedures:\n");
		for (i = 0; i < type_GetClassInfo(self, noClassMethods); ++i) {
		    appendbuf("\n");
		    type_GetDeclaration_(type_GetClassInfo(self, classMethods)[i], 0, depth+1);
		    appendbuf(";\n");
		}
	    }

	    if (type_GetClassInfo(self, data) != NULL
		&& type_GetPrev(type_GetPrev(type_GetClassInfo(self, data))) != NULL) {
		appendbuf("\ndata:\n");
		appenddata(type_GetPrev(type_GetClassInfo(self, data)), depth+1);
	    }
	    appendbuf("}\n");
	    break;

	case NULL:
	    appendbuf(type_GetName(self));
	    break;

	    /* now, we consider the secondary type operations that might modify
	     * a primary operation.
	     */

	case type_ptr:
	    appendbuf("*");
	    type_GetDeclaration_(self, opsPos+1, depth);
	    break;


	case type_funcReturning:
	    if (type_GetOps(self)[opsPos+1] == type_ptr)
		appendbuf("(");
	    type_GetDeclaration_(self, opsPos+1, depth);
	    if (type_GetOps(self)[opsPos+1] == type_ptr)
		appendbuf(")");
	    appendbuf("()");
	    break;

	case type_array:
	    if (type_GetOps(self)[opsPos+1] == type_ptr)
		appendbuf("(");
	    type_GetDeclaration_(self, opsPos+1, depth);
	    if (type_GetOps(self)[opsPos+1] == type_ptr)
		appendbuf(")");
	    appendbuf("[]");
	    break;

	default:    
	    if (type_GetOps(self)[opsPos] > 0) {
		/* must be an array of size type_GetOps(self)[opsPos] */
		if (type_GetOps(self)[opsPos+1] == type_ptr)
		    appendbuf("(");
		type_GetDeclaration_(self, opsPos+1, depth);
		if (type_GetOps(self)[opsPos+1] == type_ptr)
		    appendbuf(")");
		appendbuf("[");
		sprintf(buf, "%ld", type_GetOps(self)[opsPos]);
		appendbuf(buf);
		appendbuf("]");
	    } else {
		/* something went wrong, probably no NULL termination of array */
		type_GetOps(self)[opsPos] = NULL;
		type_GetDeclaration_(self, opsPos, depth);
	    }
	    break;
    }
}

/* MANAGING THE METHODS TABLES */

#define tablesize 1000

struct classInfo *tables;
struct type *currentClass;

table_init()
{
    tables = (struct classInfo*) malloc(sizeof(struct classInfo));

    tables->methods = (struct type**) malloc(sizeof(struct type*)*tablesize);
    tables->classMethods = (struct type**) malloc(sizeof(struct type*)*tablesize);
    tables->macros = (struct type**) malloc(sizeof(struct type*)*tablesize);
    tables->classMacros = (struct type**) malloc(sizeof(struct type*)*tablesize);

    currentClass = NULL;
}

copyTable(from, to, size)
struct type **from, **to;
long size;
{
    int i;

    for (i = 0; i < size; ++i)
	to[i] = from[i];
}

table_setClass(class)
struct type *class;
{
    struct classInfo *classInfo = class->info.classInfo;
    currentClass = class;

    /* copy "number of" fields */
    tables->noMethods = classInfo->noMethods;
    tables->noClassMethods = classInfo->noClassMethods;
    tables->noMacros = classInfo->noMacros;
    tables->noClassMacros = classInfo->noClassMacros;

    /* copy "table" fields */
    copyTable(classInfo->methods, tables->methods, tables->noMethods);
    copyTable(classInfo->classMethods, tables->classMethods, tables->noClassMethods);
    copyTable(classInfo->macros, tables->macros, tables->noMacros);
    copyTable(classInfo->classMacros, tables->classMacros, tables->noClassMacros);

    /* copy remaining fields */
    tables->scope = classInfo->scope;
    tables->data = classInfo->data;
}

#define resize(ptr, size) if (ptr != NULL) free(ptr); \
        ptr = (struct type **) malloc(sizeof(struct type*)*size);

table_flush()
{
    struct classInfo *classInfo = currentClass->info.classInfo;

    /* copy "number of" fields */
    classInfo->noMethods = tables->noMethods;
    classInfo->noClassMethods = tables->noClassMethods;
    classInfo->noMacros = tables->noMacros;
    classInfo->noClassMacros = tables->noClassMacros;

    /* reallocate storage for tables */
    resize(classInfo->methods, classInfo->noMethods);
    resize(classInfo->classMethods, classInfo->noClassMethods);
    resize(classInfo->macros, classInfo->noMacros);
    resize(classInfo->classMacros, classInfo->noClassMacros);

    /* copy "table" fields */
    copyTable(tables->methods, classInfo->methods, classInfo->noMethods);
    copyTable(tables->classMethods, classInfo->classMethods, classInfo->noClassMethods);
    copyTable(tables->macros, classInfo->macros, classInfo->noMacros);
    copyTable(tables->classMacros, classInfo->classMacros, classInfo->noClassMacros);

    /* copy remaining fields */
    classInfo->scope = tables->scope;
    classInfo->data = tables->data;
}

table_addMethod(method, overrideFlag)
struct type * method;
boolean overrideFlag;
{
    type_SetScope(method, tables->scope);

    switch (type_GetOps(method)[0]) {

	case type_method:

	    /* override the method? */
	    if (overrideFlag) {
		int i;

		for (i = 0; i < tables->noMethods; ++ i)
		    if (strcmp(type_GetName(method),type_GetName(tables->methods[i]))
			== 0) {
			       tables->methods[i] = method;
			       return;
			       }
	    }
	    type_SetIndex(method, tables->noMethods);
	    tables->methods[tables->noMethods++] = method;

	    break;

	case type_classmethod:
	    type_SetIndex(method, tables->noClassMethods);
	    tables->classMethods[tables->noClassMethods++] = method;
	    break;

	case type_macro:

	    /* override the method? */
	    if (overrideFlag) {
		int i;

		for (i = 0; i < tables->noMacros; ++ i)
		    if (strcmp(type_GetName(method), type_GetName(tables->macros[i]))
			== 0) {
			       tables->macros[i] = method;
			       return;
			       }
	    }

	    tables->macros[tables->noMacros++] = method;

	    break;

	case type_classmacro:
	    tables->classMacros[tables->noClassMacros++] = method;
	    break;
    }
}


/* GETTING THE CLASSPATH FROM THE ENVIRONMENT */

char *
getClasspath()
{
	char *classpath = environ_Get("CLASSPATH");

	if (classpath==NULL)
		classpath = environ_AndrewDir("/dlib/atk");

	return classpath;
}


/* LEXER ERROR HANDLING */

/* return a string telling the line number (and possibly file) where pos came from */

	char *
GetLineNumber(text, pos)
	register struct text *text;
	register int pos;
{
    static char buf[2000];
    char filename[1000];
    long lineinc, line;

    lineinc = 0;
    for ( ; pos >= 0; pos--) {
	if (pos == 0 || text_GetChar(text, pos) == '\n') {
		lineinc++;
		if (text_GetChar(text, pos+1) == '#') {
			text_CopySubString(text, pos+1, 80, buf, FALSE);
			sscanf(buf, "# %ld %s", &line, filename);
			if (strcmp(filename, "\"/tmp/typeclass.c\"") == 0)
				sprintf(buf, "%ld", line+lineinc-2);
			else
				sprintf(buf, "%ld of %s", 
					line+lineinc-2, filename);
			return buf;
		}
	}
    }
    sprintf(buf, "%ld", lineinc);
    return buf;
}

/* called by parser when an error occurs */

	void
parseerror(parser, sev, str)
	struct parse *parser;
	int sev;
	char *str;
{
    char buf[80], *bufpos = buf;
    long pos, len, i;
    static char errbuf[2500];

    for (i = -3; i <= 0; ++i) {
	pos = tlex_RecentPosition(lex, i, &len);
	if (len + (bufpos-buf) > sizeof(buf) -1) break;
	text_CopySubString(textBuf, pos, len, bufpos, FALSE);

	bufpos += len;
	if (i != 0)
		*bufpos++ = ' ';
    }
    *bufpos = '\0';

    sprintf(errbuf, "type: \"%s\" reading '%s' on line %s\n",
	str, buf , GetLineNumber(textBuf, pos));
    parse_error = errbuf;
}

/* APPEND FIELDS TO HEADER STRUCTURE OF CLASS STRUCTURE */

appendSuperClasses(self, header)
struct type *self;
struct type *header;
{
    struct type *super, *data;
    char * name;

    if (type_GetSuper(self) == ctypes.basicobject)
	return;
    else {
	appendSuperClasses(type_GetSuper(self), header);

	if ((super = type_GetSuper(self)) != NULL
	    && (data = type_GetClassInfo(super, data)) != NULL
	    && (name = (char *)type_GetName(super)) != NULL) {

	    type_Create( data, name, header, type_field );
	}
	else {
	    printf("error in appendSuperClasses, self = %s\n", type_GetName(self));
	    return;
	}
    }
}

/* DEALING WITH THE INDEX FILES */

/* this set of routines creates a symbol table of the index files
	so it is easy to find the filename corresponding to a classname
 */

static toksym_scopeType fnmIndex = toksym_GLOBAL;

	static void 
StoreIndex(f)
	FILE *f;
{
	char *cx, *fnm, line[500];
	struct toksym *tok;
	line[sizeof(line)-1] = '\0';
	while (fgets(line, sizeof(line) - 2, f)) {
		for (cx = line; *cx && ! isspace(*cx); cx++) {}
		if ( ! *cx) continue;
		*cx++ = '\0';
		for ( ; *cx && isspace(*cx); cx++) {}
		fnm = cx;
		for ( ; *cx && ! isspace(*cx); cx++) {}
		if ( ! *cx) continue;
		*cx++ = '\0';
		if (strcmp(line, fnm) == 0) 
			/* names are the same: no need for a symbol */
			continue;
		tok = toksym_TDefine(line, toksymproto, fnmIndex);
		tok->info.str = strcpy((char *)malloc(cx - fnm), fnm);
	}	
}

initializeIndex()
{
    char * path = environ_Get("CLASSPATH");
    char buf[2500];
    char *pos;
    FILE *file;

    if (path == NULL || *path == '\0')
	return;

    if (fnmIndex == toksym_GLOBAL) 
	fnmIndex = toksym_TNewScope(toksym_GLOBAL);

    pos = buf;
    while (TRUE) {
	if (*path == ':' || *path == '\0') {
	    /* construct filename */
	    *pos++ = '/';
	    strcpy(pos, "index");

	    /* append contents to indexText */
	    file = fopen(buf, "r");
	    if (file != NULL) {
		StoreIndex(file);
		fclose(file);
	    }

	    /* exit or reset pos */
	    if (*path == '\0')
		break;
	    pos = buf;

	} else
	    *pos++ = *path;
	++path;
    }
}


/* given a classname, search the symbol table and find the corresponding
	filename from the symbol.
   NOTE: if no symbol entry is found, the routine returns the 'classname'
	argument.
*/
    char*
lookupFileName(classname)
    char *classname;
{
    struct toksym *tok = toksym_TFind(classname, fnmIndex);
    if (tok)
	return tok->info.str;
    return classname;
 }


/******************************************************************************\
 *
 *	CLASS METHODS
 *
\*******************************************************************************/

struct type_ctypes*
type__GetCtypes(ClassID)
struct classhdr *ClassID;
{
    return &ctypes;
}


void
type__Reset(ClassID)
struct classhdr *ClassID;
{
	sym_DestroyScope(typeScope);
	initTypeScope();
}


	char *
type__Declare(ClassID, text, loc, len)
	struct classhdr *ClassID;
	struct text *text;
	long loc, len;
{
    static char buf[500];
    FILE *file;
    char *pos, *path, pathBuf[2500];
    struct text *text_cpp = text_New();
    char nameBuf[200], filenameBuf[200], *err;
    char *sysbuf;

    parse_error = "";
    if ((path = environ_GetProfile("includepath")) == NULL)
	sprintf(pathBuf, ".:/usr/include:%s:%s/atk",
			environ_AndrewDir("/include"),
			environ_AndrewDir("/include"));
    else
      strcpy(pathBuf, path);
    path = pathBuf;

    /* create /tmp/typeclass.c */
    file = fopen("/tmp/typeclass.c", "w");
    if (file == NULL) {
	sprintf(buf, "could not create /tmp/typeclass.c");
	text_Destroy(text_cpp);
	return buf;
    }

    /* write text to /tmp/type.class */
    text_WriteSubString(text, loc, len, file, FALSE);

    fclose(file);

    /* run it through the C pre-processor */
    sysbuf = (char *)malloc(100+strlen(path));
#ifdef CMUCS
    sprintf(sysbuf, "/usr/cs/lib/cpp /tmp/typeclass.c -I");
#else
#if defined(sys_telmat) 
    sprintf(sysbuf, "gcc23 /tmp/typeclass.c -traditional -E -I"); 
#else /* sys_telmat */ 
    sprintf(sysbuf, "/lib/cpp /tmp/typeclass.c -E -I");
#endif
#endif
    for (pos = sysbuf+strlen(sysbuf); *path != '\0'; ++path, ++pos) {
	if (*path != ':')
	    *pos = *path;
	else {
	    *pos = ' ';
	    *++pos = '-';
	    *++pos = 'I';
	}
    }
    strcpy(pos, " > /tmp/typecpp.c");
    system(sysbuf);
    system("rm -f  /tmp/typeclass.c");
    free(sysbuf);

    /* open the cpp'ed file */
    file = fopen("/tmp/typecpp.c", "r");
    if (file == NULL) {
	sprintf(buf, "could not open /tmp/typecpp.c");
	text_Destroy(text_cpp);
	return buf;
    }

    /* read it into a text object */
    if (text_Read(text_cpp, file, 0) != 0) {
	sprintf(buf, "error reading /tmp/typecpp.c");
	system("rm -f  /tmp/typecpp.c");
	fclose(file);
	text_Destroy(text_cpp);
	return buf;
    }

    fclose(file);
    system("rm -f /tmp/typecpp.c");

    /* if a class is being declared, make sure the super class
     * is declared first.
     */
    tlex_SetText(lex, text_cpp, 0, text_GetLength(text_cpp));
    extractSuperClassInfo(nameBuf, filenameBuf);
    if (*nameBuf != '\0'
	&& *(err = type_DeclareClass(nameBuf)) != '\0') {
	if (strncmp("error in superclass", err, 19) == 0)
	    sprintf(buf, "%s", err);
	else
	    sprintf(buf, "error in superclass %s: %s", nameBuf, err);
	text_Destroy(text_cpp);
	return buf;
    }

    /* establish garbageScope */
    sym_DestroyScope(garbageScope);
    garbageScope = sym_NewScope(grammarScope);

    /* call the type compiler (see parse.gra) on text_cpp */
    tlex_SetText(lex, text_cpp, 0, text_GetLength(text_cpp));
    textBuf = text_cpp;
    yyparse_init();

    parse_Run(parser);

    text_Destroy(text_cpp);
    return parse_error;
}


char*
type__DeclareFromString(ClassID, definition)
struct classhdr *ClassID;
char *definition;
{
    struct text * text = text_New();
    char *error;

    text_ReplaceCharacters(text, 0, 0, definition, strlen(definition));

    error = type_Declare(text, 0, text_GetLength(text));

    text_Destroy(text);
  
    return error;
}


char*
type__DeclareFromFile(ClassID, filename)
struct classhdr *ClassID;
char *filename;
{
    static char buf[500];
    FILE *file;
    struct text *text = text_New();
    char *error;

    /* open the file */
    file = fopen(filename, "r");
    if (file == NULL) {
	sprintf(buf, "could not open %s", filename);
	return buf;
    }

    /* read it into a text object */
    if (text_Read(text, file, 0) != 0) {
	sprintf(buf, "error reading '%s'", filename);
	fclose(file);
	return buf;
    }
    fclose(file);

    /* use cpp notation to indicate filename */
    sprintf(buf, "# line 1 \"%s\"\n", filename);
    text_AlwaysInsertCharacters(text, (long) 0, buf, (long) strlen(buf));

    error = type_Declare(text, 0, text_GetLength(text));

    text_Destroy(text);
  
    return error;
 }



char*
type__DeclareClass(ClassID, classname)
struct classhdr *ClassID;
char *classname;
{
    char static buf[200];
    char  pathBuf[5000];
    char *path, *pos;
    char *returnVal;
    char *filename;
    char *pathname;

     if (type_Lookup(ctypes.basicobject, classname) != NULL) {
	return "";
    }

    filename = lookupFileName(classname);

    if (*filename == '\0') {
	sprintf(buf, "could not find %s in $CLASSPATH", classname);
	return buf;
    }

    path=environ_GetProfile("includepath");
    if(path!=NULL) strcpy(pathBuf, path);
    else sprintf(pathBuf, ".:/usr/include:%s:%s/atk", environ_AndrewDir("/include"), environ_AndrewDir("/include"));

    path = pathBuf;
    
    pathname = (char *)malloc(strlen(pathBuf)+200);
    pos = pathname; 
    *pos = '\0';
    while (TRUE)
	if (*path == ':' || *path == '\0')
	    if (*pathname != '\0') {
		*pos++ = '/';
		strcpy(pos, filename);
		strcat(pos, ".ch");
		returnVal = type_DeclareFromFile(pathname);
		if (strncmp(returnVal, "could not open ", 15) != 0) {
		    strcpy(buf, returnVal);
		    break;
		}
		pos = pathname;
		*pos = '\0';
		if (*path == ':')
		    ++path;
	    } else {
		sprintf(buf, "could not find '%s.ch' in '%s'", filename, pathBuf);
		break;
	    }
	else
	    *pos++ = *path++;

    free(pathname);
    return buf;
}

/* INITIALIZATION CLASS METHODS */


	static void
EnterReservedWord(rock, w, i)
	void *rock;
	char *w;
	int i;
{
	struct toksym *s;
	boolean new;
	s = toksym_TLocate(w, toksymproto, grammarScope, &new);
	s->toknum = i;
}

boolean
type__InitializeClass(ClassID)
struct classhdr *ClassID;
{   
    proto = type_New();
    toksymproto = toksym_New();

    /* initialize the temporary classInfo tables */
    table_init();

    initializeIndex();

    anonNum = 0;

    /* initialize lexer for compiler */
    lex = tlex_Create(&type_tlex_tables, NULL, NULL, 0, 0);
    parser = parse_Create(&parse_description, lex, reduceActions, 
			NULL, parseerror);


    /* set up scopes */
    grammarScope = sym_NewScope(sym_GLOBAL);
    garbageScope = sym_NewScope(grammarScope);

    parse_EnumerateReservedWords(parser, EnterReservedWord, NULL);

    initTypeScope();

    return TRUE;
}

initTypeScope()
{
    char buf[2000];
    struct type *temp;

    typeScope = sym_NewScope(grammarScope);

    structs = sym_NewScope(typeScope);
    unions = sym_NewScope(typeScope);
    enums = sym_NewScope(typeScope);
    classes = sym_NewScope(typeScope);

    typedefs = sym_NewScope(typeScope);
    tempScope = sym_NewScope(typeScope);

    /* initialize the ctypes structure */

    temp = type_New();

    type_ApplyOp(temp, type_prim);
    type_SetScope(temp, typedefs);
    ctypes.voidType = type_Create(temp, "void", NULL, type_prim);
    type_SetSuper(ctypes.voidType, ctypes.voidType);
    type_Destroy(temp);

    ctypes.structType = type_Create(ctypes.voidType, "struct", NULL, type_struct);
    ctypes.unionType = type_Create(ctypes.voidType, "union", NULL, type_union);
    ctypes.enumType = type_Create(ctypes.voidType, "enum", NULL, type_enum);
    type_SetScope(ctypes.structType, structs);
    type_SetScope(ctypes.unionType, unions);
    type_SetScope(ctypes.enumType, enums);

    ctypes.charType = type_Create(ctypes.voidType, "char", NULL, type_prim);
    ctypes.shortType = type_Create(ctypes.voidType, "short", NULL, type_prim);
    ctypes.intType = type_Create(ctypes.voidType, "int", NULL, type_prim);
    ctypes.longType = type_Create(ctypes.voidType, "long", NULL, type_prim);
    ctypes.ucharType = type_Create(ctypes.voidType, "unsigned char", NULL, type_prim);
    ctypes.ushortType = type_Create(ctypes.voidType, "unsigned short", NULL, type_prim);
    ctypes.uintType = type_Create(ctypes.voidType, "unsigned int", NULL, type_prim);
    ctypes.ulongType = type_Create(ctypes.voidType, "unsigned long", NULL, type_prim);
    ctypes.floatType = type_Create(ctypes.voidType, "float", NULL, type_prim);
    ctypes.doubleType = type_Create(ctypes.voidType, "double", NULL, type_prim);

    type_SetSize(ctypes.charType, sizeof(char));
    type_SetSize(ctypes.shortType, sizeof(short));
    type_SetSize(ctypes.intType, sizeof(int));
    type_SetSize(ctypes.longType, sizeof(long));
    type_SetSize(ctypes.ucharType, sizeof(unsigned char));
    type_SetSize(ctypes.ushortType, sizeof(unsigned short));
    type_SetSize(ctypes.uintType, sizeof(unsigned int));
    type_SetSize(ctypes.ulongType, sizeof(unsigned long));
    type_SetSize(ctypes.floatType, sizeof(float));
    type_SetSize(ctypes.doubleType, sizeof(double));
    type_SetSize(ctypes.enumType, sizeof(long));

    type_SetAlign(ctypes.charType, sizeof(char));
    type_SetAlign(ctypes.shortType, sizeof(short));
    type_SetAlign(ctypes.intType, sizeof(int));
    type_SetAlign(ctypes.longType, sizeof(long));
    type_SetAlign(ctypes.ucharType, sizeof(unsigned char));
    type_SetAlign(ctypes.ushortType, sizeof(unsigned short));
    type_SetAlign(ctypes.uintType, sizeof(unsigned int));
    type_SetAlign(ctypes.ulongType, sizeof(unsigned long));
    type_SetAlign(ctypes.floatType, sizeof(float));
    type_SetAlign(ctypes.doubleType, sizeof(double));
    type_SetAlign(ctypes.enumType, sizeof(long));

    sprintf(buf, "%s/class.h", environ_AndrewDir("/include"));
    type__DeclareFromFile(NULL, buf);

    ctypes.basicobject = type_Create(ctypes.voidType, "basicobject", NULL, type_prim);
    ctypes.basicobject->ops[0] = type_class;
    type_SetScope(ctypes.basicobject, classes);
    type_SetSuper(ctypes.basicobject, ctypes.basicobject);

    ctypes.basicobject->info.classInfo
      = (struct classInfo*) malloc(sizeof(struct classInfo));
    type_SetClassInfo(ctypes.basicobject, scope, sym_NewScope(classes));
    type_SetClassInfo(ctypes.basicobject, noMethods, 0);
    type_SetClassInfo(ctypes.basicobject, noClassMethods, 0);
    type_SetClassInfo(ctypes.basicobject, noMacros, 0);
    type_SetClassInfo(ctypes.basicobject, noClassMacros, 0);
    type_SetClassInfo(ctypes.basicobject, methods, NULL);
    type_SetClassInfo(ctypes.basicobject, classMethods, NULL);
    type_SetClassInfo(ctypes.basicobject, macros, NULL);
    type_SetClassInfo(ctypes.basicobject, classMacros, NULL);
    type_SetClassInfo(ctypes.basicobject, data,
		       type_Lookup(ctypes.structType, "basicobject"));

    basicobject_methods = type_Lookup(ctypes.structType, "basicobject_methods");

    type_DeclareFromString("typedef struct _iobuf FILE;");
}

boolean
type__InitializeObject(ClassID, self)
struct classhdr *ClassID;
register struct type  *self;
{
    /* create a void object */

    type_SetSuper(self, self);
    type_SetLast(self, self);
    type_SetPrev(self, NULL);

    self->ops = (operation *) malloc(sizeof(operation));
    self->ops[0] = NULL;

    type_SetSize(self, 0);
    type_SetAlign(self, 1);
    type_SetOffset(self, 0);

    type_SetReadLevel(self, 0);
    type_SetWriteLevel(self, 0);

    return TRUE;
}

void 
type__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
register struct type  *self;
{
    switch(type_GetOps(self)[0]) {

	case type_class:

	    if (type_GetClassInfo(self, methods) != NULL)
		free(type_GetClassInfo(self, methods));
	    if (type_GetClassInfo(self, classMethods) != NULL)
		free(type_GetClassInfo(self, classMethods));
	    if (type_GetClassInfo(self, macros) != NULL)
		free(type_GetClassInfo(self, macros));
	    if (type_GetClassInfo(self, classMacros) != NULL)
		free(type_GetClassInfo(self, classMacros));

	    free(self->info.classInfo);

	    break;

	case type_macro:
	case type_classmacro:
	    free(self->info.macro);
	    break;

	default:
	    break;
    }

    free(type_GetOps(self));
}


/******************************************************************************\
  *
  *	CLASS METHODS
  *
\*******************************************************************************/


struct type *
type__Create(self, name, owner, op)
struct type * self;
char * name;
struct type * owner;
operation op;
{
    char buf[80];
    struct type * new;
    sym_ScopeType scope;
 
    /* adjust size and alignment of owner */

    if ( owner != NULL 
	 && type_GetPrev(owner) != NULL 
	 && type_GetLast(owner) == owner) {

	struct type * prev = type_GetPrev(owner);
	short size = type_GetSize(owner);

	switch (type_GetOps(owner)[0]) {

	    case type_union:
		if (type_GetAlign(prev) > type_GetAlign(owner))
		    type_SetAlign(owner, type_GetAlign(prev));
		if (type_GetSize(prev) > type_GetSize(owner))
		    type_SetSize(owner, type_GetSize(prev));
		break;

	    case type_struct:
		if (type_GetAlign(prev) > type_GetAlign(owner))
		    type_SetAlign(owner, type_GetAlign(prev));
		while (size % type_GetAlign(prev) != 0)
		    ++size;
		type_SetOffset(prev, size);
		type_SetSize(owner, type_GetOffset(prev)+type_GetSize(prev));
		break;

	    default:
		break;
	}
    }

    if (op == type_lastAdded) {
	short size = type_GetSize(owner);
	while (size % type_GetAlign(owner) != 0)
	    ++size;
	type_SetSize(owner, size);
	return owner;
    }


    /* make sure owner is the last type in the list */
    owner = (owner == NULL) ? NULL : type_GetLast(owner);	

   /* determine name */

    if (name == NULL || *name == '\0') {

	/* if no name, assign a unique number with '$' prefix
               */

	sprintf(buf, "$%ld", ++anonNum);
	name = buf;
    }

    /* determine a scope in which the new type will be defined.
       */

    switch (op) {

	case type_struct:
	    scope = structs;
	    break;

	case type_union:
	    scope = unions;
	    break;

	case type_enum:
	    scope = enums;
	    break;

	case type_method:
	case type_classmethod:
	case type_macro:
	case type_classmacro:
	case type_prim:
	case type_typedef:
	case type_const:
	    scope = typedefs;
	    break;

	case type_class:
	case type_package:
	    scope = classes;
	    break;

	default:
	    if (owner == NULL)
		scope = tempScope;
	    else if (type_GetPrev(owner) == NULL)
		scope = sym_NewScope(type_GetScope(owner));
	    else
		scope = type_GetScope(type_GetPrev(owner));
	    break;
    }

     /* now that we have a name and a scope, we can try to enter it in the
      * symbol table.
      */

    new = (struct type*) sym_Define(name, proto, scope); 

    if (new == NULL) {	    /* it was already defined in that scope */

	new = type_Lookup(self, name);	/* find it */

	if (!((op == type_struct || op == type_union || op == type_enum)
	      && type_GetSize(new) == 0 && type_GetSuper(new) == self))
	{
	    /* it is not a struct union or enum that exists due to a
	         * forward definition, so create a dummy entry for it in
	         * the garbage. it will be disposed of eventually
	         */
		
	    scope = sym_NewScope(garbageScope);
	    new = (struct type*) sym_Define(name, proto, scope);
	}
    }


   /* we have successfully created a new type, now we can fill it in.
      */

    /* link into the property list between owner and type_GetPrev(owner)*/
    if (owner == NULL) {
	type_SetLast(new, new);
	type_SetPrev(new, NULL);
	owner = new;
    } else {
	type_SetLast(new, owner);
	type_SetPrev(new, type_GetPrev(owner));
	type_SetPrev(owner, new);
    }

    /* fill in other attributes */

    type_SetSuper(new, self);

    type_SetSize(new, type_GetSize(self));
    type_SetAlign(new, type_GetAlign(self));

    type_SetReadLevel(new, type_GetReadLevel(self));
    type_SetWriteLevel(new, type_GetWriteLevel(self));

    type_ApplyOp(new, op);

    return new;
}

struct type *
type__Lookup(self, name)
struct type * self;
char * name;
{
    sym_ScopeType scope;
    struct type * loc;

    self = type_GetLast(self);

    if (type_GetOps(self)[0] == type_class
	|| type_GetOps(self)[0] == type_package)
	scope = type_GetClassInfo(self, scope);
    else if (type_GetPrev(self) == NULL)
	scope = type_GetScope(self);
    else
	scope = type_GetScope(type_GetPrev(self));
 
    loc = (struct type*) sym_Find(name, scope);

    if (loc == NULL
	 && (type_GetOps(self)[0] == type_class
	     || type_GetOps(self)[0] == type_package)
	 && type_GetSuper(self) != ctypes.basicobject)
	loc = type_Lookup(type_GetSuper(self), name);

    return loc;
}

void
type__ApplyOp(self, op)
struct type * self;
operation op;
{
    int	size;
    int oldSize, oldAlign;

    if (op == type_lastAdded) {
	type_Create(self, "", self, type_lastAdded);
	return;
    }

    /* calculate the size of the old ops array */
    size = 0;
    while (type_GetOps(self)[size] != NULL)
	++size;

    /* reallocate and append the new op */
    self->ops = (operation*) realloc(self->ops, sizeof(operation)*(size+2));

    self->ops[size] = op;
    self->ops[size+1] = NULL;

    /* update according to op */

    oldSize = type_GetSize(self);
    oldAlign = type_GetAlign(self);

    switch (op) {


	case type_prim:
	case type_typedef:
	case type_struct:
	case type_union:
	case type_enum:
	case type_method:
	case type_classmethod:
	case type_macro:
	case type_classmacro:
	case type_field:
	case type_param:
	case type_funcReturning:
	    break;

	case type_package:
	case type_class:
	    {
	    char buf[500];
	    struct type *data, *realdata, *header, *methods;

	    /* if data has been defined already, put it in realdata */

	    realdata = type_Lookup(ctypes.structType, type_GetName(self));

	    /* create struct for data */

	    if (op == type_package)
		data = NULL;
	    else {
		data = type_Create(ctypes.structType, type_GetName(self), NULL, type_struct);

		/* create header */
		if (type_GetSuper(self) == ctypes.basicobject)
		    header = type_Create(ctypes.structType, "", NULL, type_struct);
		else
		    header = type_Create(ctypes.unionType, "", NULL, type_union);

		sprintf(buf, "%s_methods", type_GetName(self));
		methods = type_Create(basicobject_methods, buf, header, type_field);
		type_ApplyOp(methods, type_ptr);

		appendSuperClasses(self, header);
		type_ApplyOp(header, type_lastAdded);

		type_Create(header, "header", data, type_field);
	    }

	    if (realdata == NULL)
		realdata = data;

	    /* initialize ClassInfo property */
	    self->info.classInfo = (struct classInfo*) malloc(sizeof(struct classInfo));
	    type_SetClassInfo(self, scope, sym_NewScope(classes));

	    type_SetClassInfo(self, data, data);

	    if (op == type_class) {
		struct type *t;

		/* initialize the class methods table */
		type_SetClassInfo(self, noClassMethods, 4);
		type_SetClassInfo(self, classMethods, (struct type **)
				  malloc(sizeof(struct type *) * 4));

		/* create system defined class methods */
		type_GetClassInfo(self, classMethods)[0] =
		  type_Create(realdata, "New", NULL, type_classmethod);
		type_SetScope(type_GetClassInfo(self, classMethods)[0],
			      type_GetClassInfo(self, scope));
		type_ApplyOp(type_GetClassInfo(self, classMethods)[0], type_ptr);
		type_Create(ctypes.ulongType, "versionnumber",
			    type_GetClassInfo(self, classMethods)[0], type_param);

		type_GetClassInfo(self, classMethods)[1] =
		  type_Create(ctypes.voidType, "Destroy", NULL, type_classmethod);
		type_SetScope(type_GetClassInfo(self, classMethods)[1],
			      type_GetClassInfo(self, scope));
		t = type_Create(realdata, "self", type_GetClassInfo(self, classMethods)[1], type_param);
		type_ApplyOp(t, type_ptr);

		type_GetClassInfo(self, classMethods)[2] =
		  type_Create(type_Lookup(ctypes.voidType, "boolean"), 
			      "Initialize", NULL, type_classmethod);
		type_SetScope(type_GetClassInfo(self, classMethods)[2],
			      type_GetClassInfo(self, scope));
		t = type_Create(realdata, "self",type_GetClassInfo(self, classMethods)[2], type_param);
		type_ApplyOp(t, type_ptr);

		type_Create(ctypes.ulongType, "versionnumber",
			    type_GetClassInfo(self, classMethods)[2], type_param);

		type_GetClassInfo(self, classMethods)[3] =
		  type_Create(ctypes.voidType, "Finalize", NULL, type_classmethod);
		type_SetScope(type_GetClassInfo(self, classMethods)[3],
			      type_GetClassInfo(self, scope));
		t = type_Create(realdata, "self", type_GetClassInfo(self, classMethods)[3], type_param);
		type_ApplyOp(t, type_ptr);
	    } else { /* its a package, initialize to empty */
		type_SetClassInfo(self, noClassMethods, 0);
		type_SetClassInfo(self, classMethods, NULL);
	    }

	    /* initialize class macros table to empty */
	    type_SetClassInfo(self, noClassMacros, 0);
	    type_SetClassInfo(self, classMacros, NULL);

	    /* inherite macromethods table */
	    if (type_GetClassInfo(type_GetSuper(self), noMacros) > 0) {

		int i;
		struct type **table, **superTable;

		type_SetClassInfo(self, noMacros,
				  type_GetClassInfo(type_GetSuper(self), noMacros));
		type_SetClassInfo(self, macros, (struct type **)
				  (malloc(sizeof(struct type*)
					  * type_GetClassInfo(self, noMacros))));

		table = type_GetClassInfo(self, macros);
		superTable = type_GetClassInfo(type_GetSuper(self), macros);

		for ( i = 0; i < type_GetClassInfo(self, noMacros); ++ i)
		    table[i] = superTable[i];

	    } else {
		type_SetClassInfo(self, noMacros, 0);
		type_SetClassInfo(self, macros, NULL);
	    }

	    /* inherite methods table */
	    if (type_GetClassInfo(type_GetSuper(self), noMethods) > 0) {
		int i;
		struct type **table, **superTable;

		type_SetClassInfo(self, noMethods,
				  type_GetClassInfo(type_GetSuper(self), noMethods));
		type_SetClassInfo(self, methods, (struct type **)
				  (malloc(sizeof(struct type*)
					  * type_GetClassInfo(self, noMethods))));

		table = type_GetClassInfo(self, methods);
		superTable = type_GetClassInfo(type_GetSuper(self), methods);

		for ( i = 0; i < type_GetClassInfo(self, noMethods); ++ i)
		    table[i] = superTable[i];

	    } else {
		type_SetClassInfo(self, noMethods, 0);
		type_SetClassInfo(self, methods, NULL);
	    }

	    }

	    break;

	case type_const:
	    type_SetVal(self, type_GetPrev(self) != NULL ?
			type_GetVal(type_GetPrev(self))+1 : 0);
	    type_SetSuper(self, ctypes.longType);
	    type_SetSize(self, type_GetSize(ctypes.longType));
	    type_SetAlign(self, type_GetAlign(ctypes.longType));
	    break;

	case type_ptr:
	case type_array:
	    type_SetSize(self, (short) sizeof(void*));
	    type_SetAlign(self, (short) sizeof(void*));
	    break;

	default:
	    type_SetSize(self, (short) (op * oldSize));
	    type_SetAlign(self, (short) oldAlign);
	    break;

    }
}

char *
type__GetDeclaration(self, maxDepth)
struct type * self;
long maxDepth;
{
    decl_bufPos = 0;

    type_GetDeclaration_(self, 0, 0);

    return decl_buf;
}


void
type__Print(self)
struct type *self;
{
    printf("struct type at 0x%lx:\n\n", self);
    printf("%s\n", type_GetDeclaration(self));
    printf("\n");
    printf("scope           = %ld\n", type_GetScope(self));
    printf("size            = %hd\n", type_GetSize(self));
    printf("align           = %hd\n", type_GetAlign(self));
    printf("read/writeLevel = %hd/%hd\n",
	    type_GetReadLevel(self), type_GetWriteLevel(self));

    switch (type_GetOps(self)[0]) {

	case type_field:
	    printf("\n");
	    printf("offset = %ld\n", type_GetOffset(self));
	    break;

	case type_method:
	case type_classmethod:
	    printf("\n");
	    printf("index = %ld\n", type_GetIndex(self));
	    break;

	case type_macro:
	case type_classmacro:
	    printf("\n");
	    printf("definition = '%s'\n", type_GetMacro(self));
	    break;

	case type_const:
	    printf("\n");
	    printf("value = %ld\n", type_GetVal(self));
	    break;

	case type_class:
	case type_package:
	    printf("\n");
	    printf("noMethods = %ld\n", type_GetClassInfo(self, noMethods));
	    printf("noClassMethods = %ld\n", type_GetClassInfo(self, noClassMethods));
	    printf("noMacros = %ld\n", type_GetClassInfo(self, noMacros));
	    printf("noClassMacros = %ld\n", type_GetClassInfo(self, noClassMacros));
	    printf("scope = %ld\n", type_GetClassInfo(self, scope));
	    break;

	default:
	    break;
    }

    printf("\n\n");
}


boolean
type__IsValidAddress(self, addr)
struct type *self;
void *addr;
{
    if (addr == NULL)
	return FALSE;
    else if (((long)addr) % type_GetAlign(self) != 0)
	return FALSE;
    else {
	char *pos, touch = 0;
	int i;

	if (err_LookAhead())
	    return FALSE;

	for (i = 0, pos = (char *)addr; i < type_GetSize(self); ++i, ++pos)
	    touch += *pos;	/* (add the values to avoid compiler warning) */

	err_MarkEnd();
    }

    return TRUE;
}

void
type__SetValue(self, obj, newVal)
struct type *self;
void *obj, *newVal;
{
    register char *objPos = (char *)obj;
    register char *valPos = (char *)newVal;

    while (objPos < ((char*)obj)+type_GetSize(self))
	*(objPos++) = *(valPos)++;
}
