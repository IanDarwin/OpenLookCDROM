/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
	Copyright Carnegie Mellon Unviersity 1992 - All Rights Reserved
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/ness.c,v 1.46 1993/12/21 18:47:07 rr2b Exp $";
#endif


/*
 *    $Log: ness.c,v $
 * Revision 1.46  1993/12/21  18:47:07  rr2b
 * made callDirLibs global and renamed to avoid polluting namespace.
 * BUG
 * (BUILD)
 *
 * Revision 1.45  1993/12/17  20:34:03  wjh
 * fix it so call ness-load in .atkinit can get files from nesslibpath
 *
 * Revision 1.44  1993/07/23  00:20:51  rr2b
 * Split off a version of CopyText which will copy surrounding
 * styles as well as embedded styles.
 *
 * Revision 1.43  1993/07/21  19:34:22  rr2b
 * Fixed to use CopyTextExactly
 *
 * Revision 1.42  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.40.1.3  1993/02/09  19:54:06  wjh
 * triv: corrected the line number printed in error messages;  whether or
 * not there is a warning message wrapped around the script, the message
 * now gives the line relative to counting the first line of the script as 1.
 *
 * Revision 1.40.1.2  1993/02/08  17:34:02  wjh
 * include warning text in line counts.
 * (Sigh.  If user has default warning text and gets an error message,
 * (the line number will not correspond to directly editing the file.))
 * Corrected error print to always print <<<
 * Removed blank line between messages.
 *
 * Revision 1.40.1.1  1993/02/02  03:02:02  rr2b
 * new R6tape branch
 *
 * Revision 1.40  1992/12/16  04:12:38  wjh
 * Ness version 1.7
 * Added readrawfile and writerawfile.
 * Readfile, readrawfile, writefile, writerawfile, and writeobject
 * 	all canonicalize the file name argument.  It may have leading ~
 * 	or embedded $environment variable.
 * Nessruna also canonicalizes the ness file argument.
 * If the argument to readfile is an ATK object data stream,
 * 	the result is a text with one element--the object.
 * Error messages are printed with line numbers.
 * The location of errors is relative to the beginning of the script
 * 	regradless of whether there is a warning text around it.
 *
 * Revision 1.39  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.38  1992/11/26  02:38:01  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.37  92/07/03  00:57:36  wjh
 * allowed file completion in arguments to 'call ness-load' in .xxxinit files
 * The arg to 'call ness-load' can also be the name of one of the
 * library files in one of the directories named in the nesspath preference.
 * .
 * 
 * Revision 1.36  1992/06/05  16:39:31  rr2b
 * improved error reporting, and ensured deallocation
 * of sysmarks appropriately.
 *
 * Revision 1.35  1992/03/18  19:24:39  wjh
 * Fixed core dump bug if ThisCell stuff is not initialized right.  -- wdc

 log truncated  Nov. 92  -wjh

 * Revision 1.0  88/04/27  14:28:53  wjh
 * Copied from /usr/andrew/lib/dummy
 */

#include <andrewos.h>	/* for strings.h */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <svcconf.h>
#include <util.h>

#include <nodeclss.h>
#include <ness.eh>
#include <dataobj.ih>
#include <view.ih>
#include <proctbl.ih>
#include <message.ih>
#include <text.ih>
#include <smpltext.ih>
#include <attribs.h>
#include <path.ih>
#include <arbiterv.ih>
#include <arbiter.ih>
#include <im.ih>
#include <frame.ih>
#include <buffer.ih>
#include <environ.ih>
#include <filetype.ih>
#include <mark.ih>
#include <stylesht.ih>

#include <nesssym.ih>
#include <interp.h>
#include <compdefs.h>
#include <envt.h>
#include <nevent.h>
#include <error.h>
#include <gen.h>
#include <call.h>


static struct ness *NessList = NULL;		/* list of all nesses 
				(so we can access library) */

static boolean WantsDialogBox = FALSE;

extern struct ness *InterpretationInProgress;   /* defined in interp.c */

static void dostmt();
static void procload();


static boolean debug;
#define DEBUG(s) {if (debug) {printf s ; fflush(stdout);}}
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))


/* releaseResources(self)
	release all data structures allocated for 'self' by a compilation
	XXX later need to delete nodes, object code, and SysMark space.
*/
releaseResources(self)
	struct ness *self;
{
	struct nesssym *g, *tg;
	struct libusenode *tuse;

	neventUnpost(self, debug);	/* remove old postings */

	/* delete from codeloc */
	for (g = self->globals;  g != NULL;  g = g->next) 
		if (g->flags == (flag_function | flag_ness))
			codelocForget(g);
		else if (g->flags == flag_xobj	) 
			/* traverse attributes of extended object */
			for (tg = nesssym_NGetINode(g, objnode)->attrs; 
					tg != NULL; tg = tg->next)
				if (tg->flags == (flag_function | flag_ness | flag_xfunc)
						|| tg->flags == flag_event)
					codelocForget(tg);

	if (self->outerScope != nesssym_GLOBAL)
		nesssym_NDestroyScope(self->outerScope);
	if (self->constScope != nesssym_GLOBAL)
		nesssym_NDestroyScope(self->constScope);
	self->outerScope = nesssym_GLOBAL;
	self->constScope = nesssym_GLOBAL;

	ness_ClearErrors(self);

	self->globals = NULL;
	self->compiled = FALSE;

	while (self->libuseList != NULL) {
		tuse = self->libuseList;
		self->libuseList = self->libuseList->next;
		libusenode_Destroy(tuse);
	}

	if(self->marks>=0) deallocSysMarks(self->marks);
	self->marks=(-1);
}

	static struct errornode *
execute(self, func)
	struct ness *self;
	char *func;
{
	struct nesssym *funcsym;
	ENTER(execute);

	if ( ! self->compiled) 
		self->ErrorList = ness_Compile(self);
	if (self->ErrorList != NULL)
		return self->ErrorList;

	for (funcsym = self->globals; funcsym != NULL; 
			funcsym = funcsym->next)
		if (strcmp(funcsym->header.sym.name, func) == 0) 
			break;
	if (funcsym == NULL) {
		unsigned char buf[200];
		sprintf(buf, "*Couldn't find %s\n", func);
		return (self->ErrorList = 
			errornode_Create(self, 0, 0, 0, freeze(buf), TRUE, self->ErrorList));
	}
	if (self->ErrorList == NULL)
		self->ErrorList = callInitAll(self);	/* sigh XXX check all libraries */

	if (self->ErrorList == NULL)
		self->ErrorList = interpretNess(
			((struct funcnode *)funcsym->header.toksym.info.node)
						->SysMarkOffset, 
			self->arg, self);
	if (self->ErrorList != NULL) {
		neventUnpost(self, FALSE);	/* remove old postings */
		MapRunError(self);
	}

	/* the arg is only used once ! */
	if (self->arg != NULL) {
		nessmark_Destroy(self->arg);
		self->arg = NULL;
	}
	LEAVE(execute);
	return self->ErrorList;
}


/* ThisUser()
	get user id and person name for the user
*/
	static unsigned char *
ThisUser()
{
	unsigned char *login = NULL, *name = NULL;
	struct CellAuth *thiscell;
	long uid, n;
	struct passwd *pw;
	static unsigned char buf[300];

	/* kerberos fails durig debugging as of Nov. 92 */
	if ( ! debug) CheckServiceConfiguration();
	if ( ! debug && ThisDomain  && FindCell(ThisDomain, &thiscell) == 0
			&& (FillInCell(thiscell), thiscell->WpError != -1)) {
		/* got name from cell */
		login = (unsigned char *)thiscell->UserName;
		name = (unsigned char *)thiscell->PersonName;
	}
	else if ((uid = getvuid()) != -1 && (pw = getvpwuid(uid)) != NULL) {
		/* got a name from passwd file */
		login = (unsigned char *)pw->pw_name;
		name = (unsigned char *)pw->pw_gecos;
	}
	if (login == NULL)
		login = (unsigned char *)"???";
	if (name == NULL)
		name = (unsigned char *)"Unknown user";

	strcpy(buf, login);
	strcat(buf, ":  ");
	n = strlen(buf);
	strncpy (buf+n, name, 299-n);
	buf[299] = '\0';
	return buf;
}

/* udate(self, d, u)
	Construct an origin value from the date d and user name n
	THIS VALUE IS FORGABLE.  Just edit the file with (say) ed.
*/
	static unsigned char *
udate(self, d, n)
	struct ness *self;
	unsigned char *d;
	unsigned char *n;
{
	static unsigned char buf[356];
	if (strlen(n) > 299) n[299] = '\0';
	sprintf(buf, "%02d\\\\%s\\\\%s\\\\00", self->syntaxlevel, d, n); 
	return buf;
}

/* ResetOrigin(self)
	create new origin information
	use current time and user
*/
	static void
ResetOrigin(self)
	struct ness *self;
{
	unsigned char *neworigin;
	long now = time(0);
	if (self->Origin != NULL) free(self->Origin);
	neworigin = udate(self, NiceTime(now), ThisUser());
	self->Origin = (unsigned char *)strcpy(malloc(strlen(neworigin) + 1), neworigin);
	self->IsNowOriginator = TRUE;
}

extern void ness_callDirLibs();
	
	boolean
ness__InitializeClass(ClassID)
	struct classhdr *ClassID;
{
	struct proctable_Entry *pe;
	WantsDialogBox = environ_GetProfileSwitch("NessUseDialogBoxInsteadOfWarning",
			FALSE);

	ness_callDirLibs();	/* find all library files */

	pe = proctable_DefineProc("ness-dostmt", dostmt, class_Load("view"),
			"ness", "Read and execute a Ness statement");
	if ( ! pe) return FALSE;

		/* ness-load is designed to be called from the -call- 
		facility of init.c.  Its first argument is to be the name of
		a file to be loaded, compiled, and posted.
		This file can use -extend proctable-  */
	pe = proctable_DefineProc("ness-load", procload, NULL,  "ness", 
			"Read, compile, and post procs from a named file");
	return (pe != NULL);
}

	boolean
ness__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct ness  *self;
{

	self->errorpending=FALSE;
	self->marks=(-1);
	self->Origin = NULL;
	self->syntaxlevel = CURRENTSYNTAXLEVEL;
	ResetOrigin(self);	/* IsNowOriginator & OriginalModValue */

	self->filename=NULL;
	self->ScriptLoc = -1;
	self->AfterScriptLoc = -1;
	self->DeauthButtonLoc = -1;
	self->ScanButtonLoc = -1;
	self->CompileButtonLoc = -1;
	self->AuthorButtonLoc = -1;
	ness_SetAccessLevel(self, ness_codeBlue);
	self->ErrorList = NULL;
	self->hasWarningText = FALSE;
	self->DisplayDialogBox = FALSE;
	self->PromptBeforeCompile = FALSE;
	self->ReadingTemplate = FALSE;
	self->ClassEnabled = FALSE;

	self->compiled = FALSE;
	self->outerScope = nesssym_GLOBAL;
	self->constScope = nesssym_GLOBAL;
	self->globals = NULL;
	self->libnode = NULL;
	self->libuseList = NULL;
	self->compilationid = im_GetWriteID();  /* give it a unique value */

	self->needsInit = FALSE;
	self->ToldUser = FALSE;
	self->DefaultText = NULL;
	self->Arbiter = NULL;
	self->CurrentInset = NULL;
	self->arg = NULL;
	ness_SetCopyAsText(self, TRUE);

	self->name = NULL;
	self->next = NessList;
	NessList = self;
	return TRUE;
}

static void WatchForArb();

	void 
ness__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct ness  *self;
{
	struct ness *n, *prev;

	releaseResources(self);
	
	if(self->Origin) {
		free(self->Origin);
		self->Origin = NULL;
	}

	/* remove from NessList */
	if (NessList == self)
		NessList = self->next;
	else {
		for (n = NessList; n != NULL && n != self; n = n->next) 
			prev = n; 
		if (n == self)
			prev->next = n->next;
	}
}

	void 
ness__SetDebug(ClassID, d)
	struct classhdr *ClassID;
	boolean d;
{
	debug = d;
	printf("Ness: debug is now %d\n", debug);
}

void ness__SetFilename(self, name)
struct ness *self;
char *name;
{
    char buf[1024];
    if(self->filename!=NULL) free(self->filename);
    if(name) {
	filetype_CanonicalizeFilename(buf, name, sizeof(buf)-1);
	buf[sizeof(buf)-1]='\0';
	self->filename=(char *)malloc(strlen(buf)+1);
	if(self->filename!=NULL) strcpy(self->filename,buf);
    } else self->filename=NULL;
}

/* ness__Write(self, file, writeID, level)
	write the text to a file as (more or less) a text
	revise or insert the initial line with author, date, and checksum
	be sure not to write the warning text or buttons
*/
	long 
ness__Write(self, file, writeID, level)
	struct ness *self;
	FILE *file;
	long writeID;
	int level;
{

	if (ness_GetWriteID(self) != writeID)  {
		ness_SetWriteID(self, writeID);
		fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),
			ness_GetID(self));
		fprintf(file, "\\origin{%s}\n", ness_GetOrigin(self));

		if (self->header.text.styleSheet->templateName)
			fprintf(file, "\\template{%s}\n",
				self->header.text.styleSheet->templateName);
		stylesheet_Write(self->header.text.styleSheet, file);
		if (self->hasWarningText)
			text_WriteSubString((struct text *)self, self->ScriptLoc, 
				self->AfterScriptLoc - self->ScriptLoc, file, TRUE);
		else
			text_WriteSubString((struct text *)self, 0, 
				ness_GetLength(self), file, TRUE);
		fprintf(file, "\\enddata{%s,%d}\n", class_GetTypeName(self),
			ness_GetID(self));
		fflush(file);
	}
	
	return ness_GetID(self);
}




void ness__SetAttributes(self, attributes)
struct ness *self;
struct attributes *attributes;
{
    super_SetAttributes(self, attributes);
    while (attributes) {
	if(strcmp(attributes->key, "filename")==0) {
	    ness_SetFilename(self, attributes->value.string);
	}
	attributes = attributes->next;
    }
}

/* ness__Read(self, file, id)
	read the file into the ness
	check the origin
*/
	long
ness__Read(self, file, id)
	struct ness *self;
	FILE *file;
	long id;
{
	long retval;
	if (self->ReadingTemplate)
		return super_Read(self, file, id);

	if (self->Origin) {
		free(self->Origin);
		self->Origin = NULL;
	}
	self->syntaxlevel = UNSPECIFIEDSYNTAXLEVEL;

	retval = super_Read(self, file, id);

	if (self->Origin == NULL) {
		/* self->Origin should have been set by HandleKeyWord, 
			but wasn't */
		unsigned char *neworigin;
		neworigin = udate(self, "unknown date",
						"????: Unknown User");
		self->Origin = (unsigned char *)strcpy(malloc(strlen(neworigin)+1), neworigin);
		self->IsNowOriginator = FALSE;
	}
	self->OriginalModValue = ness_GetModified(self);
	self->PromptBeforeCompile = TRUE;
	self->DisplayDialogBox = WantsDialogBox;	/* tell nessview to give dialog */
	if (self->accesslevel < ness_codeUV) {
		if ( ! WantsDialogBox) 
			ness_AddWarningText(self);
		ness_SetAccessLevel(self, ness_codeRed);
	}
	ness_SetReadOnly(self, TRUE);
	return retval;
}

/* ness__ReadTemplate(self, templateName, inserttemplatetext)
	read the template file
	set switch to tell __Read we are reading a template
*/
	long
ness__ReadTemplate(self, templateName, inserttemplatetext)
	struct ness *self;
	char *templateName;
	boolean inserttemplatetext;
{
	long val;
	self->ReadingTemplate = TRUE;
	val = super_ReadTemplate(self, templateName, inserttemplatetext);
	self->ReadingTemplate = FALSE;
	return val;
}


	long
ness__HandleKeyWord(self, pos, keyword, file)
	struct ness *self;
	long pos;
	char *keyword;
	FILE *file;
{
	if ( ! self->ReadingTemplate && strcmp(keyword, "origin") == 0)  {
		unsigned char buf[356], *bx = buf;
		long cnt = 0;
		int c;

		while ((c = getc(file)) != EOF && c != '}' && cnt < 355) {
			*bx++ = c;
			cnt++;
		}
		*bx = '\0';
		while (c != EOF && (c = getc(file)) != '\n') {}
		self->Origin = (unsigned char *)strcpy(malloc(strlen(buf)+1), buf);
		if (1 != sscanf(self->Origin, "%d", &self->syntaxlevel))
			self->syntaxlevel = UNSPECIFIEDSYNTAXLEVEL;
		self->IsNowOriginator = FALSE;
		return 0;
	}
	super_HandleKeyWord(self, pos, keyword, file);
}

	unsigned char *
ness__GetOrigin(self)
	struct ness *self;
{
	if ( ! self->IsNowOriginator 
			&& ness_GetModified(self) != self->OriginalModValue) 
		/* the current user has modified the text.  
			generate new origin information */
		ResetOrigin(self);
	return self->Origin;
}

	void
ness__GetOriginData(self, date, author)
	struct ness *self;
	char **date, **author;
{
	char *origin = (char *)ness_GetOrigin(self);
	char *start, *end;
	long n;

	/* (in early days, the origin fields were separated by single backslashes) */

	start = index(origin, '\\');
	if (start == NULL) {
		*date = freeze("(unknown date)");
		*author = freeze("(unknown author)");
		return;
	}

	while (*start == '\\') start++;
	end = index(start, '\\');
	if (end == NULL  ||  end-start > 300)  
		n = 28;  	/* (length of `date` value) */
	else n = end-start;
	*date = (char *)malloc(n+1);
	strncpy(*date, start, n);
	*((*date) + n) = '\0';

	start = end;
	while (*start == '\\') start++;
	end = rindex(start, '\\');
	if (end == NULL) n = strlen(start);
	else {
		if (*(end-1) == '\\') end--;
		n = end - start;
	}
	if (n > 299) n = 299;
	*author = (char *)malloc(n+1);
	strncpy(*author, start, n);
	*((*author) + n) = '\0';
}


/* ness__NotifyObservers(self, status)
	we override this call because it is made whenever textviewcmds
	modifies the underlying text
	We set the compiled flag to false.
*/
	void 
ness__NotifyObservers(self, status)
	register struct ness  *self;
	long status;
{
	super_NotifyObservers(self, status);

	/* doing the super_NotifyObservers first is important
		for menu updating via nessview_ObservedChanged */
	if (status != ness_WARNINGTEXTCHANGED)
		self->compiled = FALSE;
}


/* ness__SetReadOnly(self, readOnly)
	override this method to prevent being not read-only 
	while the warning text is in place
*/
	void 
ness__SetReadOnly(self, readOnly)
	register struct ness  *self;
	boolean readOnly;
{
	if (self->hasWarningText && ! readOnly) return;
	super_SetReadOnly(self, readOnly);
}


/* ness__ReadNamedFile(self, name)
	read an object from file, checking that it is a ness
	opens the file named by 'name' and checks that it is a ness data stream
	or a data stream which is a subclass of ness.  
	(Assumes name has previously been canonicalized.)
	Also reads and sets attributes.
	returns TRUE for a successful read and FALSE otherwise
*/
	long
ness__ReadNamedFile(self, name)
	struct ness *self;
	unsigned char *name;
{
	long val;
	FILE *inputfile;
	long c;
	
	inputfile = fopen(name, "r" );
	if (inputfile == NULL) {
		printf("File %s not found\n", name);
		return dataobject_PREMATUREEOF;
	}
	/* check to see if we might be a shell script !  */
	c = getc(inputfile);
	if (c == '#') {
		/* skip first line */
		while ( ! feof(inputfile)) {
			c = getc(inputfile);
			if (c == '\n') break;
		}
	}
	else ungetc(c, inputfile);

	val = ReadTextFileStream((struct text *)self, name, inputfile, FALSE);

	fclose(inputfile);
	
	return val;
}


/* ness__EstablishViews(self, child)
	ensure that DefaultText and Arbiter are set, if possible.
	First set the arbiter:
		if it is already non-NULL, do nothing
		if there is a defaulttext, do WantHandler from it
		otherwise do WantHandler from child
	Then set the defaulttext value:
		if it is already non-NULL, do nothing
		if there is an arbiter, ask it for "defaulttext"
			and then check its ancestors
		check the ancestors of the child
	The default text is changed if the ness is ever
	told about a child of the arbiter called "defaulttext".
*/
	void
ness__EstablishViews(self, child)
	register struct ness *self;
	struct view *child;
{
	struct textview *deftext = ness_GetDefaultText(self);
	struct arbiterview *arbview = ness_GetArbiter(self);
	struct view *textchild, *textsecond;

	initializeEnvt();	/* set up textviewClass for EstablishViews */

	/* 1. set self->Arbiter */

	if (arbview == NULL) {
		arbview = arbiterview_FindArb(
				(deftext != NULL) 
					? (struct view *)deftext 
					: (struct view *)child);
		ness_SetArbiter(self, arbview);
		if (debug)
			printf("arbiterview at 0x%lx\n", arbview);
	}


	/* 2. set self->DefaultText  */

	if (deftext == NULL && arbview != NULL) {
		struct view *dt;
		dt = arbiterview_GetNamedView(arbview, "defaulttext");
		deftext = (struct textview *)ProperPtr(dt, textviewClass);
		if (dt != NULL)
			DEBUG(("dt 0x%lx   deftext from dt 0x%lx\n", dt, deftext));
		if (deftext == NULL)
			/* try for a child of the arbview itself. */
			deftext = (struct textview *)ProperPtr((struct basicobject *)arbview,
					 textviewClass);
		if (deftext != NULL)
			DEBUG(("deftext from arbview 0x%lx\n", dt, deftext));
		if (deftext == NULL)
		    /* still no default text. search upward from arb and then child */
		    textchild = (struct view *)arbview, textsecond = (struct view *)child;
	}
	else 
		/* no default text and no arbiter.  search upward from child */
		textchild = (struct view *)child, textsecond = NULL;
	for ( ; deftext == NULL && textchild != NULL; 
				textchild = textsecond, textsecond = NULL) {
		register struct view *v;
		/* XXX BOGOSITY ALERT:  scan up tree for parent textview */
		for (v = textchild; v != NULL; v = v->parent)  {
			DEBUG(("parent is a %s\n",class_GetTypeName(v)));
			if (class_IsType(v, textviewClass)) {
				deftext = (struct textview *)v;
				break;
			}
		}
	}
	ness_SetDefaultText(self, deftext);
}



	struct errornode *
ness__Compile(self)
	register struct ness *self;
{
	static char *RedMsg = 
		":You must Empower this Ness before trying to compile";
	static char *RecurMsg = 
		":Ness cannot compile while an execution is in progress";
	
	ENTER(Compile);
	if (InterpretationInProgress != NULL) 
		return (self->ErrorList = 
			errornode_Create(self, 0, 0, 0, RecurMsg, 
						FALSE, NULL));

	releaseResources(self);

	if (self->accesslevel < ness_codeOrange)
		/* no compilation permitted */
		return (self->ErrorList = 
			errornode_Create(self, 0, 0, 0, RedMsg, 
						FALSE, NULL));
	self->needsInit = FALSE;

	/* DO THE COMPILATION */
	if (self->hasWarningText)
		self->ErrorList = compile(self, self->ScriptLoc, 
			self->AfterScriptLoc - self->ScriptLoc);
	else
		self->ErrorList = compile(self, 0, ness_GetLength(self));

	if (debug) 
		ness_dumpattrs(self, stdout);
	if (self->syntaxlevel > CURRENTSYNTAXLEVEL)
		self->ErrorList = errornode_Create(self, 0, 0, 0,
			":This compiler is out of date for this script", 
			TRUE, self->ErrorList);

	if (self->ErrorList != NULL) 
		return (self->ErrorList);
	self->needsInit = TRUE;
	if (self->accesslevel >= ness_codeGreen) {
		self->compiled = TRUE;
		self->PromptBeforeCompile = FALSE;
		neventPost(self, debug);		/* establish event handling */
	}

	LEAVE(Compile);
	return(NULL);
}

	struct errornode *
ness__Execute(self, func)
	register struct ness *self;
	char *func;
{
	return execute(self, func);
}

	void
ness__ClearErrors(self)
	struct ness *self;
{
	while (self->ErrorList != NULL) {
		struct errornode *t = self->ErrorList;
		self->ErrorList = t->next;
		errornode_Destroy(t);
	}
}

	struct errornode *
ness__NextError(self, curr)
	struct ness *self;
	struct errornode *curr;
{
	if (curr == NULL || self->ErrorList == NULL)
		return self->ErrorList;
	else 
		return curr->next;
}

/* ness__PreviousError(self, curr)
	return error node preceding curr
	if curr is NULL, return last error
	return NULL for the predecessor of first error node 
*/
	struct errornode *
ness__PreviousError(self, curr)
	struct ness *self;
	struct errornode *curr;
{
	struct errornode *pre = self->ErrorList;
	while (pre != NULL  &&  pre->next != curr)
		pre = pre->next;
	return pre;	/* NULL will indicate *front* of list */
}



	void
ness__dumpattrs(self, file)
	struct ness *self;
	FILE *file;
{
	dumpAttrList(file, self->globals);
}

	static void
dostmt(parentview)
	struct view *parentview;
{
	struct ness *tempNess = ness_New();
	unsigned char stmt [1025];
	unsigned char def[1025];
	unsigned char prompt[200];
	static unsigned char prefix [] = "function main()";
	static unsigned char suffix [] = ";end function;";
	struct errornode *success;

	ness_EstablishViews(tempNess, parentview);
	tempNess->CurrentInset = parentview;
	ness_SetAccessLevel(tempNess, ness_codeBlue);
	*def = '\0';
	strcpy(prompt, "Ness: ");
	while (TRUE) {
		if (message_AskForString(parentview, 0, 
				prompt, def, stmt, sizeof(stmt)-1) < 0)    {
			message_DisplayString(parentview, 0, "Cancelled");
			break;
		}
		ness_Clear(tempNess);	/* text_Clear() removes all text */
		ness_InsertCharacters(tempNess, 0, suffix, strlen(suffix));
		ness_InsertCharacters(tempNess, 0, stmt, strlen(stmt));
		ness_InsertCharacters(tempNess, 0, prefix, strlen(prefix));

		tempNess->ToldUser = FALSE;

		success = ness_Compile(tempNess);
		if (success == NULL) {
			message_DisplayString(parentview, 0, ". . .");
			success = execute(tempNess, "main");
		}
		if (success == NULL) {
			if ( ! tempNess->ToldUser)
				message_DisplayString(parentview, 0, "Done");
			break;
		}
		sprintf(prompt, "%s at %d.  Try again: ", success->msg+1,
				mark_GetPos(success->where) - strlen(prefix));
		ness_ClearErrors(tempNess);
		tempNess->compiled = FALSE;
		strcpy(def, stmt);
	}
	ness_Destroy(tempNess);
}

	static void
procload(filename)
	unsigned char *filename;
{
	/* XXX we ought to make a buffer for the object,
	 *	but done the simple way creates a Ness object in the file. 
	 *	if (buffer_FindBufferByFile(filename) == NULL) {
	 *		char *slash = (char *)rindex(filename, '/');
	 *		buffer_Create((slash == NULL) ? filename : slash + 1, 
	 *			filename, NULL, tempNess);
	 *	}
	 */

	char fullName[MAXPATHLEN+1];
	char *expandedname;
	FILE *inputfile;
	struct ness *tempNess;
	int n;
	struct nesssym *libsym;
	struct libnode *libnode;


	/* try to read the file */
	expandedname = path_UnfoldFileName(filename, fullName, 0);
	inputfile = fopen(expandedname, "r" );
	if (inputfile != NULL) {
		tempNess = ness_New();
		if (ReadTextFileStream((struct text *)tempNess, 
				expandedname, inputfile, FALSE) == 
				dataobject_NOREADERROR) {
			ness_SetAccessLevel(tempNess, ness_codeBlue);

			if (ness_Compile(tempNess) != NULL) {
				ness_PrintAllErrors(
					"ness-load Compilation");
				ness_Destroy(tempNess);
			}

			/* note that the tempNess is NOT usually destroyed.
			Its compiled code may be used by proctable entries */
		}
		else ness_Destroy(tempNess);
		fclose(inputfile);
		return;
	}

	/* can't read the file.  See if it is in library */

	if (*filename == '~' || (char *)index(filename, '/') != 0) {
		/* cannot be a library name */
cantread:
		fprintf(stderr, "ness-load: Could not read %s\n", filename);
		return;
	}

	n = strlen(filename);
	if (filename[n-1] == 'n' && filename[n-2] == '.') 
		/* remove .n to get library name */
		n -= 2;
	if (n > MAXPATHLEN) 
		n = MAXPATHLEN;
	strncpy(fullName, filename, n);
	fullName[n] = '\0';
	libsym = nesssym_NFind(fullName, LibScope);
	if (libsym == NULL) 
		goto cantread;

	libnode = nesssym_NGetINode(libsym, libnode);
	switch (libnode->status) {
	case NotRead:
	case NotCompiled:
		callCompLib(libnode);
		break;
	}
	switch (libnode->status) {
	case ReadFailed:
		goto cantread;
	case CompiledWithError:
		ness_PrintAllErrors("ness-load Compilation from library");
		break;
	}
}

void GetViewForError(self)
struct ness *self;
{
    struct buffer *b=buffer_FindBufferByData(self);

    if(!b) {
	char buf[256];

	buffer_GetUniqueBufferName("Ness-Errors", buf, sizeof(buf));
	b=buffer_Create(buf, NULL, NULL, self);
    }
    if(b) {
	frame_GetFrameInWindowForBuffer(b);
    }
    ness_NotifyObservers(self, ness_NEWERROR);
}

static struct frame *LibFrame = NULL;

	void
ness__Expose(self)
	struct ness *self;
{
	struct im *im;
	struct buffer *Buffer;
	unsigned char fullname[MAXPATHLEN+1];

	if (im_GetLastUsed() == NULL) {
		/* there aren't any active windows.  forget it. */
		return;
	}

	if (self->libnode != NULL) {
		/* it is a library file.  display it in the LibFrame */
		if (LibFrame == NULL) {
			/* we need to create a LibFrame window */
			im_SetProgramName("NessLibrary");
			im = im_Create(NULL);
			if (im == NULL) {	
	      			fprintf(stderr, "ness: Could not create new window.\n");
	     			return;
			}
			LibFrame = frame_New();
			if (LibFrame == NULL) {
	        			fprintf(stderr, "ness: Could not allocate enough memory to display library file.\n");
	      			return;
			}
			im_SetView(im, LibFrame);
			frame_PostDefaultHandler(LibFrame, "message", 
					frame_WantHandler(LibFrame, "message"));
			frame_SetCommandEnable(LibFrame, TRUE);
		}
		Buffer = buffer_FindBufferByData(self);
		if (Buffer == NULL) {
			strcpy(fullname, self->libnode->path);
			strcat(fullname, self->libnode->filename);
			Buffer = buffer_Create(ness_GetName(self), fullname, NULL, self);
		}
		frame_SetBuffer(LibFrame, Buffer, TRUE);
	}
	else {
	    self->errorpending=TRUE;
	    ness_NotifyObservers(self, ness_NEWERROR);
	    /* if nobody handled the error report try to get somebody who will. */
	    if(self->errorpending) GetViewForError(self);
	}
}

	struct ness *
ness__GetList(ClassID)
	struct classhdr *ClassID;
{
	return NessList;
}

/* ness__SetAccessLevel(self, level)
	sets the access level of the Ness
	checks the validity
	clears errors and sets to indicate that a compilation is needed
*/
	void
ness__SetAccessLevel(self, level)
	struct ness *self;
	long level;
{
	if (level < ness_codeInfrared) level = ness_codeRed;
	if (level > ness_codeUV) level = ness_codeGreen;

	self->accesslevel = level;
	self->compiled = FALSE;
}

	static void
xferfromwarning(self, pLoc, t, pPrevi, i, codelen)
	struct ness *self;
	struct text *t;
	long *pLoc, *pPrevi, i;
{
	long len = i - codelen - *pPrevi;
	ness_AlwaysCopyTextExactly(self, *pLoc, t, *pPrevi + 1, len);
	*pPrevi = i;
	*pLoc += len;
}

/* ness__AddWarningText(self)
	add the warning text and buttons to the ness text
	set read-only
*/
	/* copy text from t to self.
		The text in t has the form

			NESS script.  This inset is ...
			$name $date
			first part
			----
			----	Ness SCRIPT FOLLOWS
			----
			$script

			final part with buttons

		The $name string is replaced with the name field from the 
		origin and $date is replaced with the date.  
		$script will, in effect, be replaced with the script (in 
		fact the portion of the warningNotice before $script is 
		copied to before the script and the remainder is copied to 
		after the script.  The button locations are given with
		code $X, where X is one of A, S, D, or E.  The code is deleted
		and the position of the next character is taken as the 
		position of the button.  The capital letters are
			D for deauthenticate
			E for empower (compile)
			S for scan
			A for author mode
		(strange effects will be observed if $ is used otherwise)
 */
	void
ness__AddWarningText(self)
	struct ness *self;
{
	unsigned char *name;
	FILE *wtext;
	struct text *t;
	long i, len, loc, previ;
	unsigned char *date, *uname;

	ness_SetReadOnly(self, TRUE);
	if (self->hasWarningText) return;

#ifdef WJH
	name = (unsigned char *)"nesswarn.d";
#else
	name = (unsigned char *)environ_AndrewDir("/lib/ness/nesswarn.d");
#endif
	wtext = fopen(name, "r");
	if (wtext == NULL)
		return;
	t = text_New();
	ReadTextFileStream(t, name, wtext, FALSE);

	len = text_GetLength(t);
	if (len == 0) {
		/* sigh, file cannot be found */
		fprintf(stderr, "Cannot find %s", name);
		return;
	}

	ness_GetOriginData(self, &date, &uname);

	loc = 0;	/* where are we in the script */
	previ = -1;	/* just before start of text 
				to next xfer from warning */
	/* process each character of the warning text */
	for (i = 0; i < len; i++) {
		unsigned char c = text_GetChar(t, i);
		unsigned char *codex;
		if (c != '$') 
			continue;

		i++; /* advance input to char after '$' */
		c = text_GetChar(t, i);
		switch(c) {
		case 'A':  xferfromwarning(self, &loc, t, &previ, i, 2);
			self->AuthorButtonLoc = loc;
			break;
		case 'S':  xferfromwarning(self, &loc, t, &previ, i, 2);
			self->ScanButtonLoc = loc;
			break;
		case 'D':  xferfromwarning(self, &loc, t, &previ, i, 2);
			self->DeauthButtonLoc = loc;
			break;
		case 'E':  xferfromwarning(self, &loc, t, &previ, i, 2);
			self->CompileButtonLoc = loc;
			break;
		case 'd':
			codex = (unsigned char *)"date";
			while (*++codex) {
				i++;
				if (*codex != text_GetChar(t, i)) break;
			}
			if (*codex == '\0') {
				/* insert date */
				xferfromwarning(self, &loc, t, &previ, i, 5);
				ness_AlwaysInsertCharacters(self, loc, 
						date, strlen(date));
				loc += strlen(date);
			}
			break;
		case 'n':
			codex = (unsigned char *)"name";
			while (*++codex) {
				i++;
				if (*codex != text_GetChar(t, i)) break;
			}
			if (*codex == '\0') {
				/* insert name */
				xferfromwarning(self, &loc, t, &previ, i, 5);
				ness_AlwaysInsertCharacters(self, loc,
						uname, strlen(uname));
				loc += strlen(uname);
			}
			break;
		case 's':
			codex = (unsigned char *)"script";
			while (*++codex) {
				i++;
				if (*codex != text_GetChar(t, i)) break;
			}
			if (*codex == '\0') {
				/* insert surrounding text around script */
				xferfromwarning(self, &loc, t, &previ, i, 7);
				self->ScriptLoc = loc;
				loc = ness_GetLength(self);
				self->AfterScriptLoc = loc;
			}
			break;

		default: 
			/* just skip the '$' */
			i--;	/* reread the char after the '$' */
			break;
		}
	}
	xferfromwarning(self, &loc, t, &previ, i, 1);
	text_Destroy(t);
	self->hasWarningText = TRUE;
	if ( ! self->IsNowOriginator)
		self->OriginalModValue = ness_GetModified(self);

	if (date != NULL) free((char *)date);
	if (uname != NULL) free((char *)uname);
}

/* ness__RemoveWarningText(self)
	remove the warning text and set the Ness to read-write 
*/
	void
ness__RemoveWarningText(self)
	struct ness *self;
{
	long len1, len2;
	if ( ! self->hasWarningText) return;
	/* do the deletions in the order indicated (superstition: maybe it will fix a bug) */
	len1 = self->ScriptLoc;
	len2 = ness_GetLength(self) - self->AfterScriptLoc;
	ness_AlwaysDeleteCharacters(self, 0, len1);
	ness_AlwaysDeleteCharacters(self, self->AfterScriptLoc - len1, len2);
	self->hasWarningText = FALSE;
	if ( ! self->IsNowOriginator)
		self->OriginalModValue = ness_GetModified(self);

}


/* LineMsg(ness, loc, len)
	compute a (static char) msg describing the line number
	corresponding to loc.  If loc is after the end, report loc and len.
	If there is a warning text around the script, discount it.
	Cache the previous result to optimize for multiple messages in
	the same ness.
*/
	static char *
LineMsg(ness, loc, len)
	struct ness *ness;
	long loc, len;
{
	long lineno;
	long curloc, endloc;
	static char msg[100];
	static long prevloc, prevline, prevhas, previd = -999;  /* cache */

	lineno = 1;	/* first line is numbered 1 */

	if (ness->hasWarningText) {
		curloc = ness->ScriptLoc;
		endloc = ness->AfterScriptLoc;
	}
	else {
		curloc = 0;
		endloc = ness_GetLength(ness);
	}
	if (ness->compilationid == previd && loc >= prevloc
			&& prevhas == ness->hasWarningText) {
		lineno = prevline;
		curloc = prevloc;
	}
	prevloc = loc;
	previd = ness->compilationid;
	prevhas = ness->hasWarningText;

	while (loc > curloc && curloc < endloc) {
		if (ness_GetChar(ness, curloc) == '\n') 
			lineno++;
		curloc++;
	}
	prevline = lineno;
	
	if (curloc >= endloc)
		sprintf(msg, "location %d (len %d)", prevloc, len);
	else
		sprintf(msg, "line %d", lineno);

	return msg;
}

/* ness__printerrors(self, file)
	format and print error messages for 'self' to 'file'
	return number of errors 
*/
	long
ness__printerrors(self, file)
	struct ness *self;
	FILE *file;
{
	register struct errornode *list = ness_GetErrors(self);
	long cnt = 0;
	while (list != NULL) {
		register long loc, len, tloc, textend, c;
		cnt++;
		loc = mark_GetPos(list->where);
		len = mark_GetLength(list->where);
		if (list->execloc != 0)
			fprintf(file, "(Code location: %d)\n", list->execloc);
		fprintf(file, "%s - %s\n   source text is:  ", 
				LineMsg(self, loc, len), list->msg+1);
		textend = ness_GetLength(self);

		for (tloc = loc; 
			tloc >= 0 && ness_GetUnsignedChar(self, tloc) != '\n';
			tloc--) {}
		tloc++;	/* first character of line */
		for ( ; tloc < textend & (c = ness_GetUnsignedChar(self, tloc))
					!= '\n';  tloc++) {
			if (tloc == loc)  fprintf(file, " >>> ");
			fputc(c, file);
			if (tloc == loc+len - 1)  fprintf(file, " <<< ");
		}
		if (tloc <= loc+len - 1)  fprintf(file, " <<< ");
		fprintf(file, "\n");

		list = list->next;
	}
	return cnt;
}

/* ness__PrintAllErrors()
	formats error messages for all loaded Nesses and prints them to stderr
	'when' is printed in header
	return number of errors found
*/
	long
ness__PrintAllErrors(class, when)
	struct classheader *class;
	char *when;
{
	struct ness *n;
	long cnt = 0;
	struct errornode *errs;
	unsigned char *name;
	for (n = ness_GetList(); n != NULL; n = ness_GetNext(n)) {
		errs = ness_GetErrors(n);
		if (errs == NULL) 
			continue;
		name = ness_GetName(n);
		if (name != NULL)
			fprintf(stderr, "%s error(s) in %s\n", when, name);
		else 
			fprintf(stderr, "%s error(s):\n", when);
		cnt += ness_printerrors(n, stderr);
	}
	return cnt;
}

/* returns the address of the nessmark for the global
	variable named 'var'
   returns NULL if there is none such
*/
	struct nessmark *
ness__GetVarAddr(self, var)
	struct ness *self;
	char *var;
{
	struct nesssym *s;
	unsigned char tnm[100];
	unsigned char *nx;
	long ind;
	
	/* convert to lower case */
	strncpy(tnm, var, sizeof(tnm)-2);
	tnm[sizeof(tnm)-1] = '\0';  /* in case 'var' exceeds 200 bytes */
	for (nx = tnm; *nx; nx++)
		if (isupper(*nx))  
			*nx = tolower(*nx);	

	s = nesssym_NFind((unsigned char *)var, self->outerScope);
	if (s == NULL) 
		return NULL;

	ind=nesssym_NGetInfo(s, long);
	return (struct nessmark *)&SysMarkLowEnd[ind];
}
