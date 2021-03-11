/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/*
ness.ch

	ness data object

	Provides for the source and object of one script.  Actually stored as a text.
	Has methods for compilation and execution.

	    */

/*
 *    $Log: ness.ch,v $
*Revision 1.24  1993/05/04  01:23:55  susan
*RCS Tree Split
*
*Revision 1.22.1.2  1993/02/08  17:34:02  wjh
*Changed level to Ness 1.8 (was 1.7)
*
*Revision 1.22.1.1  1993/02/02  03:02:13  rr2b
*new R6tape branch
*
*Revision 1.22  1992/12/16  04:12:38  wjh
*Ness version 1.7
*Added readrawfile and writerawfile.
*Readfile, readrawfile, writefile, writerawfile, and writeobject
*	all canonicalize the file name argument.  It may have leading ~
*	or embedded $environment variable.
*Nessruna also canonicalizes the ness file argument.
*If the argument to readfile is an ATK object data stream,
*	the result is a text with one element--the object.
*Error messages are printed with line numbers.
*The location of errors is relative to the beginning of the script
*	regradless of whether there is a warning text around it.
*
Revision 1.21  1992/12/15  00:49:09  rr2b
fixed disclaimerization

Revision 1.20  1992/12/14  20:49:20  rr2b
disclaimerization

Revision 1.19  1992/11/26  02:38:01  wjh
converted CorrectGetChar to GetUnsignedChar
moved ExtendShortSign to interp.h
remove xgetchar.h; use simpletext_GetUnsignedChar
nessrun timing messages go to stderr
replaced curNess with curComp
replaced genPush/genPop with struct compilation
created compile.c to handle compilation
moved scope routines to compile.c
converted from lex to tlex
convert to use lexan_ParseNumber
truncated logs to 1992 only
use bison and gentlex instead of yacc and lexdef/lex

.

Revision 1.18  92/06/05  16:39:31  rr2b
improved error reporting, and ensured deallocation
of sysmarks appropriately.

log removed Nov, 1992 -wjh

 * Revision 1.0  88/04/27  14:28:52  wjh
 * Copied from /usr/andrew/lib/dummy
 */
#include "error.h"
#include "nesssym.ih"

#define CURRENTSYNTAXLEVEL			1
/* 	Will get compile error if syntax level of program exceeds
	CURRENTSYNTAXLEVEL because that means we have
	a new program and an old compiler. 
*/
#define CURRENTMODIFICATIONLEVEL		8

/* nessrun generates the VERSION NUMBER as 
 *
 *	<CURRENTSYNTAXLEVEL>.<CURRENTMODIFICATIONLEVEL>
 *
 */

#define UNSPECIFIEDSYNTAXLEVEL -1
/* this value is assumed if the syntax level is otherwise unspecified */

/* values for NotifyObservers */
#define ness_NEWERROR 2		/* to notify the nessview */
#define ness_WARNINGTEXTCHANGED 3	/* to notify the nessview */

#if 0
/* enum ness_access {
	ness_codeInfrared, /* was tampered with.  Do not compile unless changed */
	ness_codeRed,	/* not even compiled */
	ness_codeOrange,	/* give compile errors for all modification ops */
	ness_codeYellow,	/* give compile errors for file system ops */
	ness_codeGreen,	/* prompt before first executing */
	ness_codeBlue	/* compile without prompting */
	ness_codeUV	/* no warningNotice and no dialog box */
};
#endif
#define ness_access unsigned long
#define ness_codeInfrared		-1
#define ness_codeRed		0
#define ness_codeOrange		3
#define ness_codeYellow		6
#define ness_codeGreen		10
#define ness_codeBlue		11
#define ness_codeUV		13

class ness : text
{

overrides:

	Read(/* struct ness *self, */ FILE *file, long id) returns long;
	ReadTemplate(/* struct ness *self, */ char *templateName, 
				boolean inserttemplatetext) returns long;
	HandleKeyWord(/* struct ness *self, */ long pos, char *keyword, FILE *file)
				returns long;
	Write (/* struct ness *self, */ FILE *file, long writeid, int level) returns long;
	NotifyObservers(/* struct ness *self, */ long status);
	SetReadOnly(/* struct ness *self, */ boolean readOnly);
	SetAttributes(struct attributes *attributes);
	
methods:

	ReadNamedFile(/* struct ness *self, */ unsigned char *name) returns long;
		/* read an object from file, checking that it is a ness */
	SetFilename(char *n);
	       /* sets the filename where the ness
		script came from, and should be
		saved to when operated on by the macro
		system. */
	EstablishViews(/* struct ness *self, */ struct view *child);
		/* set DefaultText and Arbiter using hueristics.  The 'child'
		  should be a child of the arbiter.  */

	Compile(/* struct ness *self */) returns struct errornode *;
		/* compile the source (needn't be called, Execute will call it) 
		  should be called to establish any event handlers */

	Execute(/* struct ness *self, */  char *func) returns struct errornode *;
		/* execute named function in the current script 
			using nessview as access to arbiter
			and using textview as subject for currentselection() */
		/* if value returned by compile or execute is NULL, it succeeded
		    otherwise value is a  (struct errornode *)
		*/

	ClearErrors(/* struct ness *self */);	/* empty the error chain */
	NextError(/* struct ness *self, */ struct errornode *curr) returns struct errornode *;
		/* finds next err after 'curr'.  If 'curr is NULL, finds first err 
			returns NULL if 'curr' is the last */
	PreviousError(/* struct ness *self, */ struct errornode *curr) returns struct errornode *;
		/* finds error before 'curr'.  If 'curr is NULL, finds first err 
			returns NULL if 'curr' is the first */

	Expose(/* struct ness *self */);	/* show the ness and first error */

	SetAccessLevel(/* struct ness * self, */ ness_access newlevel);
		/* set the access level for execution */
	AddWarningText(/* struct ness *self */);
		/* adds to the text the initial warning and final buttons.  make read/only  */
	RemoveWarningText(/* struct ness *self */);
		/* removes from the text the initial warning and final buttons. restores r/w  */
	GetOrigin(/* struct ness *self */) returns unsigned char *;
		/* returns the current origin string */
	GetOriginData(/* struct ness *self, */ char **date, char **author);
		/* extracts origin data.  Caller must free it. */

	dumpattrs(/* struct ness *self, */  FILE *file);
		/* print the attributes to 'file' */
	printerrors(/* struct ness *self, */  FILE *file) returns long;
		/* format and print error messages for 'self' to 'file' 
		    return number of errors */
	GetVarAddr(/* struct ness *self, */  char *var) 
				returns struct nessmark *;
		/* returns the address of the nessmark for the global
			variable named 'var' 
			returns NULL if there is none */

macromethods:

	SetName(/* struct ness *self, */ n)		(self->name = (n))
	GetName(/* struct ness *self */)		(self->name)
	SetArbiter(/* struct ness *self, */ v)		(self->Arbiter = (v))
	GetArbiter(/* struct ness *self */)		(self->Arbiter)
	SetDefaultText(/* struct ness *self, */ t)	(self->DefaultText = (t))
	GetDefaultText(/* struct ness *self */)	(self->DefaultText)
	SupplyMarkerArg(/* struct ness *self, */ a)	(self->arg = (a))
	GetNext(/* struct ness *self */)		(self->next)
	GetErrors(/* struct ness *self */)		(self->ErrorList)
	HasWarning(/* struct ness *self */)		(self->hasWarningText)
	NeedsDialogBox(/* struct ness *self */)	(self->DisplayDialogBox)
	SetNeedsDialogBox(/* struct ness *self, */ v)	(self->DisplayDialogBox = (v))
	GetFilename() (self->filename)

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct ness *self) returns boolean;
	FinalizeObject(struct ness *self);
	SetDebug(boolean d);
	GetList() returns struct ness *;
	PrintAllErrors(char *when) returns long;
		/* formats error messages for all loaded Nesses 
		    and prints them to stderr.   'when' is printed in header
		    return number of errors found */

data:

	struct ness *next;		/* list of all nesses for mapping errors */
	unsigned char *name;	/* reference name;  not owned by ness */

	/* management of this text */
	unsigned char *Origin;	/* syntax level \0 date file written \0 file writer */
	long syntaxlevel;		/* from the Origin value */
	long OriginalModValue;	/* used to test if user has modified the text */
	long ScriptLoc, 		/* position of first char of script */
		AfterScriptLoc,	/* position of first char after script */
		DeauthButtonLoc,	/* warning text loc of Deauth button */
		ScanButtonLoc,	/*   "   "   "  of Scan  " */
		CompileButtonLoc, /*   "   "   "  of Compile  " */
		AuthorButtonLoc;	/*   "   "   "  of Author  " */
	ness_access accesslevel;	/* control what operations allowed */
	struct errornode *ErrorList;	/* list of errors */
	boolean IsNowOriginator;	/* set T when modify the Origin value */ 
	boolean hasWarningText;	/* T if the warningtext is present */
	boolean DisplayDialogBox;	/* (for nessview) needs dialog box before compile */
	boolean PromptBeforeCompile;	/* T if should prompt before doing next compile */
	boolean ReadingTemplate;	/* just while in ReadTemplate */
	boolean ClassEnabled;	/* T if wants to access class methods, et al. */

	/* compilation information */
	boolean compiled;		/* T iff compiled okay and no text change */
	nesssym_scopeType outerScope;	/* symbol table in use */
	nesssym_scopeType constScope;	/* scope for constants */
		/* the first character of a name in constScope tells its type:
			"	short string.  name follows
			0	integer or real. 0x<hexvalue>
			'	one character constant. the char
			/	long string.  number
			&	object reference. the name
		*/
	struct nesssym *globals;	/* list of symbols and functions*/
	struct libnode *libnode;	/* if this ness is for a library file */
	struct libusenode *libuseList;  /* list of libraries used */
	long compilationid;		/* value for useid in libusenodes */

	/* used during compilation */
	struct nesssym **AttrDest;		/* where to store attributes */
	struct nesssym **saveAttrDest;	/* AttrDest saved at start of EXTEND */
	struct nesssym *CurrentObject;	/* the current EXTEND object */
	struct nesssym *InitFunc;		/* function to call to do initialization */

	/* runtime information */
	boolean needsInit;		/* T if there is unexecuted function init */
	boolean ToldUser;		/* if just displayed something in msgline */
	struct textview *DefaultText;  /* value of defaulttext() function */
	struct arbiterview *Arbiter;	/* access to name space */
	struct view *CurrentInset;
	struct nessmark *arg;	/* from SupplyMarkerArg */
	char *filename;
	long marks;
	boolean errorpending;
};

