/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nessv.c,v 1.22 1993/05/04 01:23:55 susan Exp $";
#endif






/*
 *   $Log: nessv.c,v $
 * Revision 1.22  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.21.1.1  1993/02/02  03:04:08  rr2b
 * new R6tape branch
 *
 * Revision 1.21  1993/01/08  16:33:17  rr2b
 * cutting down on duplicate global symbols
 *
 * Revision 1.20  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.19  1992/12/14  20:50:00  rr2b
 * disclaimerization
 *
 * Revision 1.18  1992/11/26  02:38:01  wjh
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
 * Revision 1.17  92/06/05  16:39:31  rr2b
 * added interface for visiting a named script, for creating
 * a ness script from a keyboard macro.
 * 
 * Revision 1.16  1991/09/12  16:26:46  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.15  1990/03/27  17:14:53  gk5g
 * Changed all calls to event_Enqueue to im_EnqueueEvent.  The former did not increment im's eventCount and therefore some of these events were not processed correctly.
 *
 * Revision 1.14  89/11/04  17:26:13  wjh
 * patch from Guy Harris to ness.c:
 * Do initializeEnvt() in ness_EstablishViews() to be sure the pointers are initialized.  This avoids "!!! Disasterous error..."
 * 
 * patch from Guy Harris to ness.c:
 * Change call.c so it passes the long variant of anytype to proc in callCfunc.  This again avoids a "!!! Disasterous error."  Similarly change the argument to free() in FreeTheMallocs.
 * 
 * Changed delimiter in origin line to double backslash to avoid error warning in sendmessage about illegal character in text.
 * 
 * Added ness_GetOriginData to extract fields from origin data.
 * 
 * ESC-~ gave funny results in a nessview if text wass initially read-only, but had no warning.  It is now fixed to keep read-onliness and the warning message in synch.
 * 
 * Implemented  pragmat:  --$Ness <level>   
 * Initial syntax level is CURRENTSYNTAXLEVEL.
 * Level before read is set to UNSPECIFIEDSYNTAXLEVEL   (i.e., -1)
 * After compile check syntax level and give error if too HIGH.
 * 
 * If the left operand of a catenation has a style, that style was imposed on the right operand, but should not have been.   It no longer is.
 * 
 * 
 * Revision 1.13  89/09/03  22:48:33  wjh
 * newness
 * 
 * Revision 1.12  89/06/23  17:23:16  wjh
 * (Items that should not be sent to downstairs are marked "R4".)
 * 
 * Added a call to CheckForInterrupt in the function calls and internal gotos.  Thus loops can be terminated with ^G.   (interp.c)  R4
 * 
 * Changed dokeys() so it uses im_DoKeySequence.  This means it is now possible to send a key sequence which involves the message line.  (interp.c)  R4
 * 
 * Implemented DoMenu(object, menustring).   This function causes the same behavior as if the user selected the menu option.  At present the menu string must be exactly as originally defined;  see the warning above for im_DoMenu().  (interp.c, call.c)  R4
 * 
 * Changed access to Ness library routines so they are always compiled.  (They used ot get the default of NoCompilation, so they were useless.)  (call.c)  
 * 
 * Removed a superflous {} pair.  {This is purely a cosmetic change.}  (nessmark.c) 
 * 
 * Fixed value_GetStringValue.  Formerly it was getting an invalid initial value.  {The fix was done by adding a call to nessmark_Initialize() in the stackString section.}  (nevent.c) 
 * 
 * Modified the data stream so errors will not occur when a ness object is the outermost object.  The fix was to add two bytes, "00", at the end of the origin string to prevent the former occurence of a spurious "\}".  (ness.c) 
 * 
 * Fixed menu handling so Ness and child menus get posted when there is a mouse click in an inset within the Ness.  Formerly neither set of menus was posted. (nessv.c) 
 * 
 * Fixed dostmt, the function called from ness-load, which is recommended to be bound to ESC-ESC.  It was using a NULL pointer, so people were getting core dumps if they typed ESC-ESC before doing a compile.  (ness.c) 
 * 
 * Avoided an infinite loop which occurred if a library function referred to a non-existent entry point within itself.  Did this by checking to see if the library function is already Compiling just before trying to compile it.  (call.c call.hn, call.h) 
 * 
 * Revised system marker allocation so the compilation will not get a subsequent error.  (gen.c)
 * 
 * Revised system marker allocation so it expands the space available if necessary. This will make it possible to compile larger programs.  (gen.c)
 * 
 * Changed the type of TType to long from struct object *.  This will allow compilation on stricter compilers.  (interp.h) 
 * 
 * Fixed nessmark_FinalizeObject so it would not reference a NULL pointer.  {Somehow the assembler noticed this bug!}  (nessmark.c) 
 * 
 * Changed functions which deal with constant strings to have (char *) as there argument type (SaveError, exprnode_Create, ReportError, ExprError, RunError, LocateErrorFunc, QueryReadOnly, makeConst, printallerrors) or return type (Freeze, argcounterr, argtypeerr).  This prevents compile errors on picky compilers.  (interp.c, error.c, call.c, ness.c, nessv.c, search.c, nevent.c, nessmark.c, nessrun.ci)  R4 
 * 
 * Changed Imakefile to store Imakefile in checkin rule.  (Imakefile)
 * 
 * 
 * Revision 1.11  89/06/01  16:01:21  wjh
 * campus release version
 * 
 * Revision 1.6  88/12/07  22:44:19  wjh
 * 
 * 9 Nov
 * implemented access level control
 * skip first line of script if starts with # (for shell script)
 * changed so the name of all initialization functions is init()
 * added ness-load
 * moved execution of init() from compilation to first execution
 * deferred compilation to first FullUpdate time
 * 
 * 22 Nov
 * proc table calls now work correctly with type-free procs  (the first arg can be anything)
 * added "cheat_" functions which will remain undocumented
 * changed inset() to a function
 * fixed some bugs in initial access
 * 
 * 25 November
 * added long strings
 * added Next Error menu option
 * made replace() work correctly in all cases
 * added class() and new()
 * 
 * 29 Nov
 * added ^<upper-case> and \e as characters in strings
 * added nextn() and length()
 * 
 * 6 Dec
 * added functions: parseint(), parsereal(), firstobject(), whereitwas(), replacewithobject(), addstyles(), nextstylegroup(), enclosingstylegroup(), clearstyles(), hasstyles()
 * catch bus and segmentation errors
 * 
 * 
 * Revision 1.5  88/11/23  14:43:40  wjh
 * proc table calls now work correctly with type-free procs  (the first arg can be anything)
 * added "cheat_" functions which will remain undocumented
 * changed inset() to a function
 * fixed some bugs in initial access
 * 
 * Revision 1.4  88/11/16  14:05:10  wjh
 * 
 * implemented access level control
 * skip first line of script if starts with # (for shell script)
 * changed so the name of all initialization functions is init()
 * added ness-load
 * moved execution of init() from compilation to first execution
 * deferred compilation to first FullUpdate time
 * 
 * Revision 1.3  88/11/01  17:40:29  wjh
 * 
 * an empty Ness is now given a template (ness.template or default.template)
 * implemented:  extend proctable on proc "name" ...
 * added currentmark()
 * fixed a core dump by not recompile during UnlinkTree()
 * 
 * Revision 1.2  88/10/26  11:53:25  wjh
 * fixed a malloc abort bug that happened when certain error messages were discarded
 * added call to init() when initial compilation is done
 * fixed so events are posted to views whenever the view name is registered with the arbiter
 * 
 * Revision 1.1  88/10/21  11:01:25  wjh
 * Initial revision
 * 
 * Revision 1.0  88/04/27  14:28:57  wjh
 * Copied from /usr/andrew/lib/dummy
 */


#include <andrewos.h>

#include <dataobj.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <mark.ih>
#include <bind.ih>
#include <im.ih>
#include <event.ih>
#include <view.ih>
#include <graphic.ih>
#include <rect.h>
#include <physical.h>
#include <fontdesc.ih>
#include <style.ih>
#include <buffer.ih>
#include <complete.ih>
#include <proctbl.ih>
#include <text.ih>
#include <frame.ih>
#include <framemsg.ih>
#include <message.ih>
#include <arbiterv.ih>
#include <cursor.ih>
#include <environ.ih>

/* include headers for the data object and THIS view */
#include <nessv.eh>
#include <ness.ih>
#include <textv.ih>
#include <nesssym.ih>
#include <error.h>
#include <compdefs.h>	/* for flag_xobj */

static boolean  debug;      /* This debug switch is toggled with ESC-^D-D */
#define DEBUG(s) {if (debug) {printf s ; fflush(stdout);}}
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))


#define	MenuOff		(0)
#define	MenuOrigin	(1<<0)
#define	MenuScan	(1<<1)
#define	MenuErrors	(1<<2)
#define	MenuAuthor	(1<<3)
#define	MenuMain	(1<<4)
#define	MenuEmpower	(1<<5)
#define	MenuUser	(1<<6)
#define	MenuDanger	(1<<7)

	static void 
PostMenus(self)
	struct nessview *self;
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;
	long menumask;
	DEBUG(("Enter PostMenus(%d)\n", self->HasIF));
	if (dobj->hasWarningText) menumask = MenuOrigin;
	else {
		menumask = MenuOrigin | MenuScan | MenuUser;
		if (ness_GetReadOnly(dobj)) menumask |= MenuAuthor;
		if ( ! dobj->compiled  || dobj->accesslevel < ness_codeGreen
				|| self->compmod != ness_GetModified(dobj))
			menumask |= MenuEmpower;
	}
	if (dobj->ErrorList) menumask |= (self->justscanned) ? MenuDanger : MenuErrors;
	if (dobj->accesslevel >= ness_codeGreen) menumask |= MenuMain;
	menulist_SetMask(self->Menus, menumask);
	DEBUG(("PostMenus mask = 0x%lx\n", menumask));
	if ( ! self->HasIF) 
		return;
	nessview_PostMenus(self, self->Menus);
	DEBUG(("Leave PostMenus\n"));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	User Interface 
 *	
 *	Routines called from keystrokes or menu
 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *defaultExecFunc = "main";

	static void
setExecFunc(self, funcname)
	struct nessview *self;
	char *funcname;
{
	if (self->ExecFunction != defaultExecFunc)
		free(self->ExecFunction);
	self->ExecFunction = (funcname == NULL || *funcname == '\0')
				? defaultExecFunc : freeze(funcname);
}


	static void
nessview_NextError(self, rock)
	register struct nessview *self;
	long rock;
{
	register struct ness *dobj = (struct ness *)self->header.view.dataobject;
	struct im *im = nessview_GetIM(self);

	if  (im && im_ArgProvided(im)) 
		self->CurrError = ness_PreviousError(dobj, self->CurrError);
	else
		self->CurrError = ness_NextError(dobj, self->CurrError);
	
	if (self->CurrError == NULL) {
		textview_CollapseDot(self->SourceText);
		message_DisplayString(self, 0, "No more errors");
	}
	else {
		/* select and view the error */
		textview_SetDotPosition(self->SourceText,
			mark_GetPos((self->CurrError)->where));
		textview_SetDotLength(self->SourceText,
			mark_GetLength((self->CurrError)->where));
		textview_FrameDot(self->SourceText,
			mark_GetPos((self->CurrError)->where));

		message_DisplayString(self, 0, (self->CurrError)->msg+1);
	}
}

/* FirstError(struct nessview *self)
	display message and highlight for first error 
*/
	void
nessview__FirstError(self)
	struct nessview *self;
{
	if (ness_GetErrors((struct ness *)self->header.view.dataobject) == NULL)
		return;

	if(nessview_GetDataObject(self)) ((struct ness *)nessview_GetDataObject(self))->errorpending=FALSE;
		
	if ( ! self->HasIF) {
		self->ErrorHighlightPending = TRUE;
		nessview_WantInputFocus(self, self);
	}
	else {
		self->ErrorHighlightPending = FALSE;
		self->CurrError = NULL;
		nessview_NextError(self, 0);	
	}
}


	static void
nessview_Compile(self, accesslevel)
	register struct nessview *self;
	ness_access accesslevel;
{
	register struct ness *dobj = (struct ness *)self->header.view.dataobject;
	struct errornode *errs;

	ness_SetAccessLevel(dobj, accesslevel);

	if (dobj->PromptBeforeCompile && accesslevel >= ness_codeGreen) {
		/* give a dialog box to verify the compile choice */
		long choice;
		static char *choices[] = {
			"Cancel - Keep Ness script inactive", 
			"Empower - Let script do its thing", 
			NULL
		};
		if (message_MultipleChoiceQuestion(NULL, 50, 
			"Do you really want to empower this Ness script?",
				0, &choice, choices, "ce") < 0 
				|| choice != 1) 
			return;
		dobj->PromptBeforeCompile = FALSE;
	}
	self->CurrError = NULL;
	ness_EstablishViews(dobj, self);
	dobj->CurrentInset = NULL;

	errs =  ness_Compile(dobj);
	self->compmod = ness_GetModified(dobj);

	if (errs == NULL) 
		message_DisplayString(self, 0, "Done");
	else 
		nessview_FirstError(self);
	self->justscanned = (accesslevel < ness_codeGreen);

	/* need to wantinputfocus, since Menu AND keybindings may have changed. */
	nessview_WantInputFocus(self, self);
	/* PostMenus(self); */
}

	static void
nessview_Execute(self, rock)
	register struct nessview *self;
	long rock;
{
	register struct ness *dobj = (struct ness *)self->header.view.dataobject;
	char buffer[1000];
	struct errornode *errs = NULL;

	self->CurrError = NULL;
	dobj->ToldUser = FALSE;

	if (im_ArgProvided(nessview_GetIM(self))) {
		/* prompt for name of function to execute */
		if (message_AskForString(self, 0, "Execute function: ", 
				self->ExecFunction, buffer, sizeof(buffer)) != -1)
			setExecFunc(self, buffer);
		else
			return;
	}

	if ( ! dobj->compiled) {
		nessview_Compile(self, dobj->accesslevel);
		errs = dobj->ErrorList;
		if ( ! dobj->compiled  && errs == NULL) {
			/* user probably refused permission */
			message_DisplayString(self, 0, "Not done");
			return;
		}
	}
	ness_EstablishViews(dobj, self);
	dobj->CurrentInset = NULL;

	if (errs == NULL) {
		/* if the function is defined, execute it.  
			If not and the function is the default
				and there are no extended objects, give error */
		/* sigh XXX this code is a lot like ness_Execute */
		boolean hasxobj;
		struct nesssym *funcsym;
		hasxobj = FALSE;

		for (funcsym = dobj->globals; funcsym != NULL; 
				funcsym = funcsym->next)
			if (funcsym->flags == flag_xobj)
				hasxobj = TRUE;
			else if (strcmp((char *)funcsym->header.sym.name, self->ExecFunction) == 0) 
				break;
		if (funcsym != NULL)
			errs = ness_Execute(dobj, self->ExecFunction);
		else if (self->ExecFunction != defaultExecFunc || ! hasxobj) {
			char buf[200];
			sprintf(buf, "Couldn't find %s\n", self->ExecFunction);
			message_DisplayString(self, 0, buf);
			PostMenus(self);	
			return;
		}
	}

	if (errs == NULL) {
		if ( ! dobj->ToldUser)
			message_DisplayString(self, 0, "Done");
	}
	else 
		nessview_FirstError(self);
	PostMenus(self);
}

	static void
ShowOrigin(self)
	struct nessview *self;
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;
	unsigned char buf[500];
	char *date, *author;

	ness_GetOriginData(dobj, &date, &author);
	sprintf(buf, "Probably last modified by %s on %s", author, date);
	free(date);
	free(author);
	message_DisplayString(self, 0, buf);
}

/* DeauthButton(self)
	processes a click on the Deauthentication button
	prompts the user to see if s/he really means it
	if yes, deauthenticate this process
*/
	static void
DeauthButton(self)
	struct nessview *self;
{
	/* XXX give dialog box to see if he really wants to */
	/* when this is implemented it should deauthenticate first 
		and then Empower-compile */

	message_DisplayString(self, 0, "Deauthentication not implemented");
}

/* AuthorButton(self, option)
	processes ESC-~, a click on the Author button, or one of the menu items
		Author Mode   and  Add Warning
	option 0 - remove warning text;    1 - add warning text
		-1  -  toggle read-only and make warning match
	prompts the user to see if s/he really means it
	if yes, change to author mode
*/
	static void
AuthorButton(self, option)
	struct nessview *self;
	long option;
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;

	nessview_WantInputFocus(self, self);
	if (dobj->hasWarningText  && 
			((ness_GetReadOnly(dobj)  && option != 1)
			|| ( ! ness_GetReadOnly(dobj)  && option == 0)))  {
		/*  In 3 of the 12 cases we remove the warning */
		/* prompt first to make sure the user wants to */
		long choice;
		static char *choices[] = {
			"Cancel - Leave script read-only", 
			"Author mode - Make script read-write", 
			NULL
		};
		if (message_MultipleChoiceQuestion(NULL, 50, 
			"Do you want to be able to modify this Ness script?",
				0, &choice, choices, "ca") < 0 
				|| choice != 1) 
			return;

		/* ok, go ahead */
		nessview_PostMenus(self, NULL);	/* remove menus (HACK TO GET AROUND BUG */
		ness_RemoveWarningText(dobj);
		textview_SetDotPosition(self->SourceText, 0);
		textview_SetDotLength(self->SourceText, 0);
		textview_FrameDot(self->SourceText, 0);
   		textview_WantUpdate(self->SourceText, self->SourceText);
		ness_NotifyObservers(dobj, ness_WARNINGTEXTCHANGED);
	}

	/* set read-onliness */
	ness_SetReadOnly(dobj, 
			(option==0) ? FALSE : (option==1) ? TRUE
				:  ! ness_GetReadOnly(dobj));

	/* add warning text if needed */
	if ( ! dobj->hasWarningText  &&  ness_GetReadOnly(dobj)) {
		ness_AddWarningText(dobj);
		textview_SetDotPosition(self->SourceText, 0);
		textview_FrameDot(self->SourceText, 0);
		ness_NotifyObservers(dobj, ness_WARNINGTEXTCHANGED);
	}
	PostMenus(self);
}

/* ScanButton(self)
	processes a click on the Scan button:
	compiles the text at level orange
*/
	static void
ScanButton(self)
	struct nessview *self;
{
	nessview_Compile(self, ness_codeOrange);
	nessview_WantInputFocus(self, self);		/* to ensure having Ness menus */
}

/* CompileButton(self)
	processes a click on the Compile button:
	compiles at level Green
*/
	static void
CompileButton(self)
	struct nessview *self;
{
	nessview_Compile(self, ness_codeGreen);
	nessview_WantInputFocus(self, self);		/* to ensure having Ness menus */
}

/* CompileMenu(self)
	processes a click on the Compile Menu:
	turns off the PromptBeforeCompile switch
	compiles at level Green
*/
	static void
CompileMenu(self)
	struct nessview *self;
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;
	dobj->PromptBeforeCompile = FALSE;
	nessview_Compile(self, ness_codeGreen);
}

	static void
ToggleDebug(self, rock)
	register struct nessview *self;
	long rock;
{
	debug = ! debug;
	ness_SetDebug(debug);
	printf("nessview debug is now %d\n", debug);  fflush (stdout);
}

/* Stuff for Appending to files, or to other Nesses */
static struct cursor *waitcursor=NULL;

static int waitcount=0;

/* Turn on the wait cursor so the user knows we're busy.  Keeps a count of the number of invocations of WaitOn, so that functions called from a caller of WaitOn won't
  screw up the state of the wait cursor. */
static void WaitOn()
{
    struct im *last=im_GetLastUsed();
    waitcount++;

    if(waitcount==1) {

	if(last==NULL || waitcursor!=NULL) return;

	waitcursor=cursor_Create(last);

	if(waitcursor!=NULL) {
	    cursor_SetStandard(waitcursor, Cursor_Wait);
	    im_SetProcessCursor(waitcursor);
	}
	
    }
}

/* Turns off the wait cursor if no one wants it on anymore. */
static void WaitOff()
{
    waitcount--;
    if(waitcount<=0) {
	waitcount=0;
	if(waitcursor!=NULL) {
	    im_SetProcessCursor(NULL);
	    cursor_Destroy(waitcursor);
	    waitcursor=NULL;
	}
    }
}

/* Append the src ness to the end ness.  First it compiles the src if it has not already been compiled.  Errors are sent to the src view and to stderr, a notice is displayed in a dialog if any errors occurred. If the src compiles successfullly, it is appended to the dest and the dest is compiled.  Any errors encountered in compiling the dest are treated the same way as errors compiling the src. */
static boolean DoAppend(self, src, dest)
struct nessview *self;
struct ness *src,*dest;
{
    long end;
    ness_SetAccessLevel(dest, ness_codeUV);
    ness_SetNeedsDialogBox(dest, FALSE);
    ness_SetReadOnly(dest, FALSE);

    ness_SetAccessLevel(src, ness_codeUV);
    ness_SetNeedsDialogBox(src, FALSE);
    
    if(!src->compiled) {
	WaitOn();
	message_DisplayString(self, 0, "Testing compilability...");

	im_ForceUpdate();

	if(ness_Compile(src)) {
	    ness_Expose(src);
	    ness_printerrors(src,stderr);
	    message_DisplayString(self, 0, "");
	    message_DisplayString(self, 100, "Error recompiling!");
	    WaitOff();
	    return FALSE;
	}
	WaitOff();
    }

    end=ness_GetLength(dest);

    if(end>0 && ness_GetChar(dest, end-1)!='\n') ness_InsertCharacters(dest, end, "\n", 1);

    ness_CopyText(dest, ness_GetLength(dest), src, 0, ness_GetLength(src));

    end=ness_GetLength(dest);

    if(end>0 && ness_GetChar(dest, end-1)!='\n') ness_InsertCharacters(dest, end, "\n", 1);
    
    WaitOn();
    message_DisplayString(self, 0, "Re-compiling.");

    im_ForceUpdate();

    if(ness_Compile(dest)) {
	ness_Expose(dest);
	ness_printerrors(dest,stderr);
	message_DisplayString(self, 0, "");
	message_DisplayString(self, 100, "Error recompiling! Some bindings may be unavailable.");
	WaitOff();
	return FALSE;
    }
    
    WaitOff();
    
    return TRUE;
}

/* Saves the buffer b to filename, and if src is in a buffer which is visible replaces it with the buffer b on the destination of the append. */
static void UpdateFile(self, src, b, filename)
struct nessview *self;
struct ness *src;
struct buffer *b;
char *filename;
{
    struct buffer *ob;
    WaitOn();
    message_DisplayString(self, 0, "Saving...");
    im_ForceUpdate();

    if(buffer_WriteToFile(b, filename, buffer_ReliableWrite|buffer_MakeBackup)<0) {
	message_DisplayString(self, 0, "");
	message_DisplayString(self, 100, "Write failed!");
	WaitOff();
	return;
    }
    buffer_SetIsModified(b, FALSE);
    WaitOff();

    message_DisplayString(self, 0, "Done re-compiling and saving.");

    ob=buffer_FindBufferByData(src);

    if(ob!=NULL) {
	struct frame *f=frame_GetFrameInWindowForBuffer(ob);
	if(f!=NULL) {
	    frame_SetBuffer(f, b, TRUE);
	    buffer_SetIsModified(b, FALSE);
	    buffer_Destroy(ob);
	}
    }
}


static struct style *fixed=NULL, *boldulined=NULL;

/* Structure used to provide help information and completion when asking for a script name. */
struct helpRock {
    procedure HelpWork; /* The function to call to add to the help text. */
    struct text *text; /* The Help text, so that styles can be added. */
    long rock;	    /* The rock, this is actually a pointer to a private structure in framemsg, which contains the text, but we don't know that :-( */
    char *partial;  /* The best completion so far. */
    struct ness *n; /* The current ness, so that it won't be listed or used as a valid completion. */
};

/* Get a name for a ness, either uses it's Adew name, or the tail of it's filename, if any.  Otherwise returns NULL. */
static char *GetName(n)
struct ness *n;
{
    char *name=(char *)ness_GetName(n), *p;
    if(name) return name;
    name=ness_GetFilename(n);
    if(name==NULL) return NULL;
    p=rindex(name, '/');
    if(p) return p+1;
    else return name;
}

/* Appends the name of the ness n to the help text if it matches the partial name in the help rock. */
static long match(n,h)
struct ness *n;
struct helpRock *h;
{
    char buf[1024];
    int len;
    char *name=GetName(n);
    if(name==NULL) return 0;
    if(!strncmp(name, h->partial, strlen(h->partial))) {
	bzero(buf,sizeof(buf));
	strncpy(buf,name,sizeof(buf)-1);
	(*h->HelpWork)(h->rock,message_HelpListItem,buf,NULL);
    }
    return 0;
}


static char *headingtxt="Active Ness Scripts\n";
/* This is the function called to provide help when the user uses '?' in a prompt for a Ness script. */
static void helpProc(partial,myrock,HelpWork,rock)
char *partial;
struct helpRock *myrock;
procedure HelpWork;
long rock;
{
    struct text *t=myrock->text;
    struct ness *n=ness_GetList();
    int len=strlen(headingtxt);
    if(!HelpWork) return;
    (*HelpWork)(rock,
		    message_HelpGenericItem,
		    headingtxt,
		    NULL);
    
    myrock->HelpWork=HelpWork;
    myrock->rock=rock;
    myrock->partial=partial;
    while(n) {
	if(n!=myrock->n) match(n, myrock);
	n=n->next;
    }
    text_AddStyle(myrock->text, 0, len-1, boldulined);
    text_AddStyle(myrock->text, len, text_GetLength(myrock->text)-len, fixed);
}

/* Does the actual completion by interating over all the nesses except the current one and ones with no name calling completion_CompletionWork. */
static enum message_CompletionCode mycomplete(partial, myrock, buffer, bufferSize)
char *partial;
struct helpRock *myrock;
char *buffer;
int bufferSize;
{
    struct result result;
    char textBuffer[1024];
    struct ness *n=ness_GetList();
    *textBuffer = 0;
    result.partial = partial;
    result.partialLen = strlen(partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer) - 1; /* Leave extra room for a NUL. */
    while(n) {
	if(n!=myrock->n && GetName(n)) completion_CompletionWork(GetName(n), &result);
	n=n->next;
    }
    strncpy(buffer, result.best, bufferSize);
    if (result.bestLen == bufferSize) /* Now make sure buffer ends in a NUL. */
	buffer[result.bestLen] = 0;
    return result.code;
}

static char sbuf[1024]=".atkmacros";

static long AskForScript(self, current, prompt, buf, bufsiz)
struct view *self;
struct ness *current;
char *prompt;
char *buf;
long bufsiz;
{
    struct ness *n=ness_GetList();
    struct helpRock myrock;
    struct framemessage *fmsg=(struct framemessage *)view_WantHandler(self,"message");
    struct buffer *b;

    /* This first part is a hack to get the help text object so that we can add styles to the help we give. */
    /* If the message handler installed is not a framemessage, punt. */
    if(!fmsg || !class_IsTypeByName(class_GetTypeName(fmsg),"framemessage")) return -1;

    /* Get the help buffer from the message handlers frame. */
    b=frame_GetHelpBuffer(fmsg->frame);
    /* If there is no help buffer then punt, otherwise
      remember the help text. */
    if(b!=NULL) myrock.text=(struct text *)buffer_GetData(b);
    else return -1;

    /* Set the "Current" ness, this is so that it will not be used as a valid answer ever. */
    myrock.n=current;

    /* does the default exist? */
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, sbuf)==0) break;
	n=n->next;
    }

    /* Actually ask, if n is non-NULL sbuf contains the name of an existing ness script which is the default answer. */
    if(message_AskForStringCompleted(self, 0, prompt, n?sbuf:NULL, buf, bufsiz, NULL, mycomplete, helpProc, &myrock, message_MustMatch)) {
	message_DisplayString(self,0,"Cancelled.");
	return -1;
    }
    /* Make this answer the new default. */
    strcpy(sbuf, buf);
    return 0;
}

/* Appends one Ness script to another.  If the rock is >255 it is assumed to be a pointer to the name of the destination Ness script, otherwise AskForScript is used to get the name of the destination. If the destination has a filename associated with it the file is updated to reflect the new contents of the script, throwing away any changes which had been made to the file. The destination Ness will be recompiled with the new code, and the source will be destroyed if it has a buffer. */
static void ScriptAppend(self, rock)
struct nessview *self;
long rock;
{
    char buf[1024];
    char *name=NULL;
    struct ness *n=ness_GetList();
    
    struct buffer *b;
    
    if(rock>255) name=(char *)rock;
    
    if(name==NULL) {
	if(AskForScript(self, nessview_GetDataObject(self), "Append to script:", buf, sizeof(buf))<0) return;
	name=buf;
    }
    
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, name)==0) break;
	n=n->next;
    }
    
    if(n==NULL) {
	message_DisplayString(self, 0, "Couldn't find the specified Ness script.");
	return;
    }
    
    if(!DoAppend(self, nessview_GetDataObject(self), n)) return;
 
    if(ness_GetFilename(n)==NULL) {
	message_DisplayString(self, 0, "Ness has no associated file, changes have NOT been saved.");
	return;
    }
    
    b=buffer_FindBufferByData(n);

    /* If we create the buffer here it will NOT
      destroy it's data when it goes away. */
    if(b==NULL) b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);

    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the Ness, changes have NOT been saved.");
	return;
    }

    UpdateFile(self, nessview_GetDataObject(self), b, ness_GetFilename(n));
}

static char dbuf[1024]="~/.atk.macros";

static char *appendchoices[]={
    "Yes",
    "No",
    NULL
};

/* Appends a Ness script to a file.  If the rock is >255 it is assumed to be a pointer to the filename of the destination, otherwise completion_GetFilename is used to get the filename of the destination. The destination is read in if necessary and the script is appended to it. The destination Ness will be recompiled with the new code, and the source will be destroyed if it has a buffer. Changes to the destination will be saved or a warning will be given. */
static void Append(self, rock)
struct nessview *self;
long rock;
{
    char buf[1024];
    char *filename=NULL, *p, *q;
    struct buffer *b, *ob;
    struct basicobject *data;
    struct ness *dest, *src=(struct ness *)nessview_GetDataObject(self);
    struct ness *n=ness_GetList();
    long result=2;
    boolean diff=FALSE;
    struct ness *nm=NULL;
    
    if(rock>255) filename=(char *)rock;
    if(filename==NULL) {
	if(completion_GetFilename(self, "Append to file:", dbuf, buf, sizeof(buf), FALSE, FALSE)<0) {
	    message_DisplayString(self, 0, "Cancelled.");
	    return;
	}
	filename=buf;
    }

    strcpy(dbuf, filename);
    q=rindex(filename, '/');
    if(q) {
	q++;
	while(n) {
	    p=GetName(n);
	    if(p && strcmp(p, q)==0) break;
	    n=n->next;
	}
    } else n=NULL;
    
    /* If the buffer already exists we leave it's data destroying flag intact. */
    b=buffer_FindBufferByFile(filename);

#if 0
    if(n && !(b && buffer_GetData(b)==(struct dataobject *)n)) {
	if(message_MultipleChoiceQuestion(self, 100, "Warning a Ness Script with that filename is currently active.", 0, &result, appendchoices, NULL)<0 || result==0) {
	    message_DisplayString(self, 0, "Cancelled!");
	    return;
	}
    }

    switch(result) {
	case 1: /* visiting existing script. */
	    b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);
	    break;
	case 2: /* visiting file. */

	    if(b==NULL) {
		b=buffer_GetBufferOnFile(filename, 0);
		/* If we created the buffer we don't want the data to go away when the buffer does. */
		if(b!=NULL) buffer_SetDestroyData(b, FALSE);
	    }
    }
#else
    if(n) {
	if(b) {
	    nm=(struct ness *)buffer_GetData(b);
	    if(nm!=n) diff=TRUE;
	} else diff=TRUE;
	if(diff) {
	    char buf[1500];
	    sprintf(buf, "Replace script from file %s?", ness_GetFilename(n));
	    if(message_MultipleChoiceQuestion(self, 100, buf, 0, &result, appendchoices, NULL)<0) {
		message_DisplayString(self, 0, "Cancelled!");
		return; 
	    }
	    if(result==0) ness_Destroy(n);
	    else message_DisplayString(self, 0, "Old Ness script not destroyed.");
	}
    }
    
    if(b==NULL) {
	b=buffer_GetBufferOnFile(filename, 0);
	if(b!=NULL) buffer_SetDestroyData(b, FALSE);
    }
#endif
    
    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the specified file.");
	return;
    }
    
    if(buffer_GetReadOnly(b)) {
	message_DisplayString(self, 0, "File is read only. Append cancelled.");
	buffer_Destroy(b);
	return;
    }
    
    data=(struct basicobject *)buffer_GetData(b);
    if(strcmp(class_GetTypeName(data), "ness")!=0) {
	message_DisplayString(self, 0, "The named file is not a ness file.");
	buffer_Destroy(b);
	return;
    }
    
    dest=(struct ness *)data;

    if(!DoAppend(self, src, dest)) return;

    UpdateFile(self, src, b, filename);
}

/* Visit an existing Ness script, brings up the specified Ness script in a buffer in a window. */
static void Visit(self, rock)
struct view *self;
long rock;
{
    struct ness *n=ness_GetList();
    char buf[1024];
    struct buffer *b;
    struct frame *f;
    char *name=NULL;

    if(rock>255) name=(char *)name;
    
    if(name==NULL) {
	if(AskForScript(self, NULL, "Visit script:", buf, sizeof(buf))<0) return;
	else name=buf;
    }
    
    while(n) {
	char *sn=GetName(n);
	if(sn && strcmp(sn, name)==0) break;
	n=n->next;
    }
    
    if(n==NULL) {
	message_DisplayString(self, 0, "Couldn't find the specified Ness script.");
	return;
    }

    /* Ensure that no unnecessary headaches occur... */
    ness_SetAccessLevel(n, ness_codeUV);
    ness_SetNeedsDialogBox(n, FALSE);
    ness_SetReadOnly(n, FALSE);
    
    b=buffer_FindBufferByData(n);
    
    if(b==NULL) b=buffer_Create(GetName(n), ness_GetFilename(n), NULL, n);

    if(b==NULL) {
	message_DisplayString(self, 0, "Couldn't get a buffer on the selected Ness script.");
	return;
    }

    f=frame_GetFrameInWindowForBuffer(b);

    if(f==NULL) {
	message_DisplayString(self, 0, "Couldn't get a window on the selected Ness script.");
	return;
    } else {
	struct view *v=frame_GetView(f);
	if(v!=NULL) view_WantInputFocus(v, v);
    }
}
    

/*  Menus and Keymaps
	The menus and keymaps are initialized to those of
	    EmbeddedMenus and EmbeddedKeymap in InitializeObject.
	The ApplicationMenus and ApplicationKeymap get
	    added to the menus and keymap in GetApplicationLayer

	Debugging can be toggled with the key sequence  ESC-^D -D
*/
static struct menulist  *EmbeddedMenus, *ApplicationMenus;
static struct keymap  *EmbeddedKeymap, *ApplicationKeymap;

static struct bind_Description EmbeddedBindings[]={
    {"nessview-append-to-file",NULL,0,"Ness,Append to File~40",0,0,Append,"Append the current buffer to a Ness file." },/*
    {"nessview-append-to-script",NULL,0,"Ness,Append to Script~45",0,0,ScriptAppend,"Append the current buffer to a Ness script." }, */
    {"nessview-next-error", "\030\016", 0, 	/* ^X^N */
			"Ness~10,Next Error~10", 0, MenuErrors, 
			nessview_NextError, 
			"See the source code for the next error", NULL},
    {"nessview-next-error", "\030\016", 0, 	/* ^X^N */
			"Ness~10,Next Danger~10", 0, MenuDanger, 
			nessview_NextError, 
			"See the source code for the next potentially dangerous statement",
			 NULL},
    {"nessview-show-origin", "\030\016O", 0, 	/* ^X^NO */
			"Ness~10,Show Origin~12", 0, MenuOrigin, 
			ShowOrigin, 
			"Show the author and date of last change", NULL},
    {"nessview-scan-for-danger", "\030\016S", 0, 	/* ^X^NS */
			"Ness~10,Scan for danger~15", 0, MenuScan, 
			ScanButton, 
			"Check for any statements that might be harmful", NULL},
    {"nessview-compile", "\033\016C", 0, 	/* ESC-^N-C */
			"Ness~10,Empower-compile~22", 0, MenuEmpower,
			CompileMenu, 
			"Compile the text in this view", NULL},
    {"nessview-execute", "\030\005", 0, 	/* ^X^E */
			"Ness~10,Do main()~24", 0, MenuMain, 
			nessview_Execute, 
			"Compile Ness and execute function main()", NULL},
    {"nessview-author-mode", "\033~", -1, 	/* ESC-~ */
			"Ness~10,Author mode~32", 0, MenuAuthor, 
			AuthorButton, 
			"Change to be an author", NULL},
    {"nessview-user-mode", "\033~", -1, 	/* ESC-~ */
			"Ness~10,Add warning~32", 1, MenuUser, 
			AuthorButton, 
			"Reinsert the warning text and be readonly", NULL},
    {"ness-invert-debug", "\033\004D",		/* ESC - ^D - D */
			0, 0, 0, 0, ToggleDebug,
			"Toggle the nessview debug flag", NULL},
	NULL
};
static struct bind_Description ApplicationBindings[]={
	NULL
};


/* winning contest entry from Adam Stoller:

A Ness Program Inset Exists In This Document

Do NOT Compile/Execute
Get Help On Ness
Scan Ness for Harmful Effects
Trust Script Author - Compile/Execute

*/

/* DisplayDialogBox(self, time)
	prompt the user as to whether to execute this Ness
 */
DisplayDialogBox(self, time)
	struct nessview *self;
	long time;  /* ignored */
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;
	long choice;

	static char *choices[] = {
		"Cancel - Keep Ness script inactive", 
		"Help - Add warning text to script", 
		"Scan - Check for dangerous statements", 
	/* 	"Deauthenticate this process",     */  "-  -  -", 
		"Empower - I trust the source of this script", 
		NULL
	};
	ENTER(nessview_DisplayDialogBox);
	if ( ! dobj->DisplayDialogBox) return;	/* (enqueued twice by initial updates) */
	dobj->DisplayDialogBox = FALSE;
kludgefornathaniel:
	self->dialogboxup = TRUE;
	if (message_MultipleChoiceQuestion(NULL, 50, 
				"Ness script here.  Do you trust the user you got it from?",
				0, &choice, choices, "chs-e") < 0 
			|| choice == 0) {
		ness_SetAccessLevel(dobj, ness_codeRed);
		choice = 0;
	}
	self->dialogboxup = FALSE;
	nessview_WantUpdate(self, self);	/* to get uninverted */
	switch (choice) {
	case 1:	/* Help - add warning text */
		ness_AddWarningText(dobj);
		DEBUG(("Added warning.  Author button at %d\n", 
			dobj->AuthorButtonLoc));
		textview_SetDotPosition(self->SourceText, 0);
		textview_FrameDot(self->SourceText, 0);
		ness_NotifyObservers(dobj, ness_WARNINGTEXTCHANGED);
		break;
	case 2:	/* Scan - compile at level orange */
		nessview_Compile(self, ness_codeOrange);
		break;

#ifdef notdef
	case 3:	/* Deauthenticate */
		DeauthButton(self);
		break;
#endif /* not def */
	case 3:	/* repeat the dialog box */
		goto kludgefornathaniel;

	case 4:	/* Compile - carte blanche */
		dobj->PromptBeforeCompile = FALSE;
		nessview_Compile(self, ness_codeGreen);
		break;
	default:
		message_DisplayString(self, 0, "Cancelled");
		break;
	}
	LEAVE(nessview_DisplayDialogBox);
}


	boolean
nessview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
	    char *dmf;
	    struct proctable_Entry *pe;
	    EmbeddedMenus = menulist_New();
	    ApplicationMenus = menulist_New();
	    EmbeddedKeymap = keymap_New();
	    ApplicationKeymap = keymap_New();
	    bind_BindList(EmbeddedBindings, EmbeddedKeymap, EmbeddedMenus,
			  &nessview_classinfo);
	    pe=proctable_DefineProc("nessview-visit-script", Visit, class_Load("frame"), "nessview", "Visit an existing Ness script.");
	    if(pe!=NULL) menulist_AddToML(EmbeddedMenus, "Ness,Visit Script~50", pe, NULL, 0);
	    bind_BindList(ApplicationBindings, ApplicationKeymap, ApplicationMenus,
			  &nessview_classinfo);
	    fixed=style_New();
	    if(!fixed) return FALSE;
	    boldulined=style_New();
	    if(!boldulined) {
		style_Destroy(fixed);
		return FALSE;
	    }
	    style_SetFontFamily(fixed, "AndyType");
	    style_AddNewFontFace(fixed, fontdesc_Fixed);
	    style_SetFontSize(fixed, style_ConstantFontSize,10);
	    style_Copy(fixed,boldulined);
	    style_AddUnderline(boldulined);
	    style_AddNewFontFace(boldulined,fontdesc_Bold);

	    style_SetFontSize(boldulined, style_ConstantFontSize,20);
	    dmf=environ_GetProfile("DefaultMacrosFile");
	    if(dmf && strlen(dmf)<sizeof(dbuf)-1) strcpy(dbuf, dmf);

	    return TRUE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	
 *	Override methods
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	boolean
nessview__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct nessview  *self;
{
	self->Menus = menulist_DuplicateML(EmbeddedMenus, self);
	/* ApplicationMenus are added in GetApplicationLayer */
	self->Keystate = keystate_Create(self, EmbeddedKeymap);

	self->SourceText = textview_New();
	nessview_SetView(self, self->SourceText);
	self->ExecFunction = defaultExecFunc;
	self->CurrError = NULL;
	self->compmod = -1;
	self->ButtonPending = 0;

	self->HasIF = FALSE;
	self->ErrorHighlightPending = FALSE;

	self->inverted = FALSE;
	self->dialogboxup = FALSE;
	self->justscanned = FALSE;

	return TRUE;
}

	void 
nessview__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct nessview  *self;
{
	menulist_Destroy(self->Menus);
	keystate_Destroy(self->Keystate);

	textview_Destroy(self->SourceText);
}

	void 
nessview__SetDataObject(self, dobj)
	register struct nessview  *self;
	register struct ness *dobj;
{
	ENTER(nessview_SetDataObject);

	if (dobj->header.text.templateName == NULL) {
		/* if there is no template, read default.template 
			Read text from template if there
			is not yet text in the document.  */
		boolean usetemplatetext = (ness_GetLength(dobj) == 0);
#ifdef notdef
		if (ness_ReadTemplate(dobj, "ness", usetemplatetext) < 0)
#endif /* notdef */
			ness_ReadTemplate(dobj, "default", usetemplatetext);
	}
	super_SetDataObject(self, dobj);
	textview_SetDataObject(self->SourceText, dobj);
	textview_WantUpdate(self->SourceText, self->SourceText);

	nessview_FirstError(self);
	setExecFunc(self, NULL);	

	self->compmod = ( ! dobj->compiled) ? -1 : ness_GetModified(dobj);

	LEAVE(nessview_SetDataObject);
}


	void
nessview__WantInputFocus(self, requestor)
	struct nessview *self;
	struct view *requestor;
{
ENTER(nessview__WantInputFocus);
	if (requestor == nessview_GetChild(self))
		/* if the request is for the textview, request it ourselves
			so we can post menus */
		super_WantInputFocus(self, self);
	else
		super_WantInputFocus(self, requestor);
LEAVE(nessview__WantInputFocus);
}

	void 
nessview__ReceiveInputFocus(self)
	register struct nessview  *self;
{
	ENTER(nessview_ReceiveInputFocus);
	self->MenusPostedRecently = FALSE;
	self->KeystatePostedRecently = FALSE;
	textview_ReceiveInputFocus(self->SourceText);
	self->HasIF = TRUE;

	if (! self->MenusPostedRecently) {
		PostMenus(self);
	}
	if ( ! self->KeystatePostedRecently) {
		self->Keystate->next = NULL;
		super_PostKeyState(self, self->Keystate);
	}
	if (self->ErrorHighlightPending) 
		nessview_FirstError(self);

	LEAVE(nessview_ReceiveInputFocus);
}


	void 
nessview__LoseInputFocus(self)
	register struct nessview  *self;
{
	ENTER(nessview_LoseInputFocus);
	if (self->HasIF) {
		textview_LoseInputFocus(self->SourceText);
		self->HasIF = FALSE;
	}
	/* menus and keystate are deactivated by parent */

	LEAVE(nessview_LoseInputFocus);
}

	void
nessview__PostMenus(self, menulist)
	register struct nessview  *self;
	struct menulist *menulist;
{
ENTER(nessview__PostMenus);
	if (menulist != self->Menus) {
		menulist_ClearChain(self->Menus);
		DEBUG(("chain cleared\n"));
		if (menulist != NULL)
			menulist_ChainAfterML(self->Menus, menulist, NULL);
		PostMenus(self);  /* self->hasIF is False, so won't actually post */
	}
	super_PostMenus(self, self->Menus);
	self->MenusPostedRecently = TRUE;
LEAVE(nessview__PostMenus);
}

	void
nessview__PostKeyState(self, keystate)
	register struct nessview  *self;
	struct keystate *keystate;
{
DEBUG(("nessview_PostKeyState(0x%lx)  self has 0x%lx\n", keystate, self->Keystate));
	if (keystate != self->Keystate)
		keystate_AddBefore(self->Keystate, keystate);
	super_PostKeyState(self, self->Keystate);
	self->KeystatePostedRecently = TRUE;
LEAVE(nessview__PostKeyState);
}


static	void
Invert(self)
	struct nessview *self;
{
	struct rectangle r;

	nessview_SetTransferMode(self, graphic_INVERT);
	nessview_GetVisualBounds(self, &r);
	nessview_FillRect(self, &r, nessview_BlackPattern(self));
}

	void
nessview__FullUpdate(self, type, left, top, width, height)
	register struct nessview  *self;
	register enum view_UpdateType  type;
	register long  left, top, width, height;
{
	register struct ness *dobj = (struct ness *)self->header.view.dataobject;
	
	DEBUG(("FullUpdate(%d)\n", type));

	if (type == view_FullRedraw || type == view_LastPartialRedraw) {
		if ( ! self->inverted)
			super_FullUpdate(self, type, left, top, width, height);
		else {
			/* is inverted */
			super_FullUpdate(self, view_FullRedraw, left, top, width, height);
			if (self->dialogboxup)
				Invert(self);
			else
				self->inverted = FALSE;
		}
		if (dobj != NULL  &&  dobj->DisplayDialogBox)
			nessview_WantUpdate(self, self);
	}
	else if (type != view_PartialRedraw || ! self->inverted)
		super_FullUpdate(self, type, left, top, width, height);


	PostMenus(self);
	LEAVE(nessview_FullUpdate);
}

	void
nessview__Update(self)
	register struct nessview  *self;
{
	register struct ness *dobj = (struct ness *)self->header.view.dataobject;
	struct errornode *errs = NULL;

	ENTER(nessview_Update);

	if (self->inverted)
		Invert(self);
	super_Update(self);
	if (self->inverted) {
		if (self->dialogboxup)
			Invert(self);
		else
			self->inverted = FALSE;
	}

	if (dobj != NULL && ! dobj->hasWarningText 
			&& dobj->DisplayDialogBox) {
		im_EnqueueEvent(DisplayDialogBox, self, 0);

		/* reverse video while dialog box is up */
		self->inverted = TRUE;
		Invert(self);
	}
	if (errs != NULL)
		nessview_FirstError(self);
	LEAVE(nessview_Update);
}


/* nessview__Hit(self, action, x, y, numberOfClicks)
	checks to see if there is a click on a button in novice text mode
*/
	struct view *
nessview__Hit(self, action, x, y, numberOfClicks)
	struct nessview *self;
	enum view_MouseAction action;
	long x, y;
	long numberOfClicks;
{
	struct ness *dobj = (struct ness *)self->header.view.dataobject;
	static long initialx, initialy;

	if ( ! dobj->hasWarningText || 
			(action != view_LeftDown
			&& action != view_LeftMovement
			&& action != view_LeftUp) )
		return super_Hit(self, action, x, y, numberOfClicks);

	if (action == view_LeftDown) {
		/* left mouse down and there is a warning text.
			If the down is near a button, set ButtonPending
		*/
		long tvx, tvy;
		long pos;
		struct view *temp;

		self->ButtonPending = 0;
		tvx = physical_GlobalXToLogicalX(textview_GetDrawable(self->SourceText),
				physical_LogicalXToGlobalX(nessview_GetDrawable(self), x));
		tvy = physical_GlobalYToLogicalY(textview_GetDrawable(self->SourceText),
				physical_LogicalYToGlobalY(nessview_GetDrawable(self), y));
		if (tvx < 0 || tvy < 0)
			return super_Hit(self, action, x, y, numberOfClicks);
		initialx = x;
		initialy = y;

		pos = textview_Locate(self->SourceText, tvx, tvy, &temp);

			DEBUG(("LeftDown at %d  x %d y %d (Scan Button at %d)\n", 
			pos, x, y, dobj->ScanButtonLoc));

		if (pos == dobj->ScanButtonLoc
				|| pos == dobj->ScanButtonLoc + 1)
			self->ButtonPending = 1;
		else if (pos == dobj->AuthorButtonLoc 
				|| pos == dobj->AuthorButtonLoc + 1)
			self->ButtonPending = 2;
		else if (pos == dobj->CompileButtonLoc 
				|| pos == dobj->CompileButtonLoc + 1)
			self->ButtonPending = 3;
#ifdef notdef
		else if (pos == dobj->DeauthButtonLoc
				|| pos == dobj->DeauthButtonLoc + 1)
			self->ButtonPending = 4;
#endif /* notdef */
	}

	if (self->ButtonPending == 0)
		return super_Hit(self, action, x, y, numberOfClicks);

	if (action == view_LeftMovement) {
		if (x - initialx > 5 || initialx - x > 5 || y - initialy > 5 || initialy - y > 5) {
				DEBUG(("Move too far:  x %d  y %d    #clicks %d\n", 
				x, y, numberOfClicks));
			self->ButtonPending = 0;
			super_Hit(self, view_LeftDown, initialx, initialy, numberOfClicks);
			return super_Hit(self, action, x, y, numberOfClicks);
		}
	}
	else if (action == view_LeftUp) {
		if (x - initialx > 3  ||  initialx - x > 3 || y - initialy > 3  ||  initialy - y > 3) {
			super_Hit(self, view_LeftDown, initialx, initialy, numberOfClicks);
			return super_Hit(self, action, x, y, numberOfClicks);
		}

		/* defer compile and scan in case the buttons are within a script which is
			itself within a text which has its mouse hits intercepted 
			by a Ness script */

		if (numberOfClicks == 1)
		switch (self->ButtonPending) {
		case 1:	im_EnqueueEvent(ScanButton, self, 0);
			break;
		case 2:	AuthorButton(self, 0);
			break;
		case 3:	im_EnqueueEvent(CompileButton, self, 0);
			break;
		case 4:	DeauthButton(self);
			break;
		}
	}
	return (struct view *)self;
}


/* nessview__DesiredSize(self, width, height, pass, desiredWidth, desiredHeight) 
	The parent calls this to find out how big the view would like to be.
	This routine sets 'desiredWidth' and 'desiredHeight' and returns a
		code value indicating which is flexible for further negotiation.
	The 'width' and 'height' parameters are tentative values the parent is suggesting.
	'pass' indicates which of 'width' and 'height' should be considered fixed.
	If neither is fixed, they may be arbitrary values. 
*/
	enum view_DSattributes
nessview__DesiredSize(self, width, height, pass, desiredWidth, desiredHeight) 
	register struct nessview *self;
	long width;
	long height;
	enum view_DSpass pass;
	long *desiredWidth;
	long *desiredHeight;
{
	DEBUG(("DesiredSize(...%d, %d, %d...)\n", width, height, pass));

	*desiredWidth = 550;
	*desiredHeight = 200;

	DEBUG(("Leave DesiredSize: %d x %d\n", *desiredWidth, *desiredHeight));
	return view_HeightFlexible | view_WidthFlexible;
}

	void
nessview__Print(self, file, processor, format, level)
	register struct nessview  *self;
	register FILE  file;
	register char  *processor;
	register char  *format;
	register boolean  level;
{
	DEBUG(("Ignore nessview__Print\n"));
}

/* ObservedChanged(self, changed, value)
	this is called when the ness is destroyed 
	and when NEWERROR is signalled for it
		in the latter case, we show the first error message
	if WARNINGTEXTCHANGED, we have to adjust the menus
*/
	void
nessview__ObservedChanged(self, changed, value)
	register struct nessview  *self;
	register struct ness *changed;
	long value;
{
	DEBUG(("Observed changed (value=%d)\n", value));
	if (value == ness_NEWERROR)
		/* display the error */
		nessview_FirstError(self);
	else if (value == ness_WARNINGTEXTCHANGED)
		nessview_WantUpdate(self, self);
	else if (value == 0)
		/* textviewcmds sends 0 */
		if (changed->compiled && self->compmod != ness_GetModified(changed))
			/* NB. ness_NotifyObservers sets compiled=FALSE _after_ it
				passes along the status.  Thus the above test is met
				only for the _first_ change.  */
			PostMenus(self);
		/* tell our parent data object that it has changed  XXX UGH */
		if (self->header.view.parent != NULL  &&
				self->header.view.parent->dataobject != NULL)
			dataobject_SetModified(self->header.view.parent->dataobject);
	LEAVE(nessview_ObservedChanged);

}
