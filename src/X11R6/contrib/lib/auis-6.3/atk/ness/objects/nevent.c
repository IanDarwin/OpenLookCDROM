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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nevent.c,v 1.42 1994/02/01 22:08:25 wjh Exp $";
#endif






#include <andrewos.h>	/* for index() */
#include <physical.h>
#include <util.h>		/* for FOLDEDEQ */

#include <im.ih>
#include <init.ih>
#include <view.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <message.ih>
#include <lpair.ih>
#include <arbiterv.ih>
#include <celv.ih>
#include <cel.ih>
#include <value.h>	/* for value_NEWVALUE */
#include <value.ih>
#include <valuev.ih>

/* for FrameMark */
#include <text.ih>
#include <textv.ih>
#include <atom.ih>
#include <observe.ih>

#include <nodeclss.h>
#include <nesssym.ih>
#include <nessmark.ih>
#include <nevent.h>
#include <error.h>
#include <gen.h>
#include <envt.h>
#include <compdefs.h>   /* curComp */
#include <tlex.ih>
#include <call.h>	/* for callInitAll */
#include <ness.ih>

/* data for functions reporting event parameters */
static long MouseX = 0;
static long MouseY = 0;
static long MouseAction = -99;
static long SavedClickCount = 1;
static unsigned char *LastKeys = NULL;
static unsigned char *LastMenu = NULL;

static struct init *globalInit = NULL;

#define PTSTOMARK(arg,iar)  ((TType)((struct nessmark *)arg)->header.nessmark_methods \
		== nessmarkHdr) ? TRUE :   \
		RunError(":not a pointer to a mark (uninitialized variable?)", iar);

extern struct basicobject *ProperPtr();
extern struct text *FrameMark();

/* This is used to generate a permanent copy of something, hopefully we will want this same string many times, and won't want too many different strings frozen. */
static char *pfreeze(name)
char *name;
{
    struct atom *a=atom_Intern(name);
    return atom_Name(a);
}


#define 		MAXNAME 100

	static unsigned char *
CelSymName(name)
	unsigned char *name;
{
	static unsigned char buf[MAXNAME+1];
	strncpy(buf+1, name, MAXNAME-1);
	buf[MAXNAME] = '\0';	/* truncate if too long */
	buf[0] = '&';
	return buf;
}

#undef 		MAXNAME

/* neventStartExtend(currobj, name)
	start compilation of an object extension
	check that there is no prior extension in progress (currobj == NULL)
	establish a new scope for global names
	uniquify the object name string to be different than a string constant
	and Return the unique object nesssym with its info field pointing at an objnode
*/
	struct nesssym *
neventStartExtend(currobj, name)
	struct nesssym *currobj;
	struct nesssym *name;
{
	struct nesssym *newobj;
	boolean new;
	struct classinfo *viewClass = NULL;
	char buf[100];
	boolean isView = FALSE;
	unsigned char *cname = CelSymName(nesssym_ToC(name, 
			&curComp->ness->header.text, buf, sizeof(buf)));
	if (currobj != NULL) 
		ReportError(":'Extend' is not allowed inside an 'extend' block", -1);

	newobj = nesssym_NLocate(cname, name, nesssym_NGetScope(name), &new);
	if ( ! new) {
		compPushScope(nesssym_NGetINode(newobj, objnode)->scope);
		return newobj;
	}

	newobj->flags = flag_xobj;
	newobj->type = Tptr;
	newobj->header.toksym.loc = name->header.toksym.loc;
	newobj->header.toksym.len = name->header.toksym.len;
	if (strncmp(cname, "&view:", 6) == 0) {
		/* set viewClass from the rest of the name */
		isView = TRUE;
		viewClass = class_Load(cname+6);
		if (viewClass == NULL)
			ReportError(":unknown view name", 0);
	}
	nesssym_NSetINode(newobj, objnode,
			objnode_Create(NULL, compNewScope(), 
				tlex_RecentPosition(curComp->tlex, -1, 0), 999,
				(struct celview *) viewClass,
				NULL, NULL,		/* pe's */
				NULL, NULL, FALSE, isView));	/* data */

	/* link this extend so codeloc entries from ON xxx are attached to the Ness */
	genLinkGlobal(newobj);
	return newobj;
}

/* neventFinishExtend(obj)
	complete processing of EXTEND ... END
	restore script scope
	return the obj
*/
	struct nesssym *
neventFinishExtend(obj)
	struct nesssym *obj;
{
	struct objnode *onode = nesssym_NGetINode(obj, objnode);
	long loc, len;
	loc = tlex_RecentPosition(curComp->tlex, 0, &len);
	onode->len = loc + len - onode->loc;
	compPopScope();
	return obj;
}

static struct eventnode *allevents=NULL;

/* neventStartEvent(currobj, e, spec)
	start processing an ON ... END:
	check that 'currobj' is not null 
	uniqify the name, hang a new eventnode off it,
	save 'e' in the eventnode for check at finish
	start a function definition and put object code loc in event node
	Return the new symbol
*/
	struct nesssym *
neventStartEvent(currobj, e, spec)
	struct nesssym *currobj;
	struct nesssym *e;
	struct nesssym *spec;
{
	struct nesssym *newevent;
	boolean new;
	unsigned char buf[400];
	nesssym_scopeType oldscope = curComp->scopes[curComp->scopex];
	static long nextevent = 0;
	struct text *newt;
	struct nessmark *newm;
	char *cname;
	struct eventnode *lastnode=allevents;

	newt = text_New();
	text_AlwaysCopyTextExactly(newt, 0, curComp->ness, 
			spec->header.toksym.loc,
			spec->header.toksym.len);
	BackSlashReduce(newt);
	newm = nessmark_New();   
	nessmark_Set(newm, newt, 0, text_GetLength(newt));
	cname = (char *)nessmark_ToC(newm);
	nessmark_Destroy(newm);		/* also destroys newt */

	sprintf(buf, "\\ Event %d (%s)", nextevent++, cname);

	if (currobj == NULL) 
		ReportError(":'on' is not allowed outside an 'extend' block", -2);
	newevent = nesssym_NLocate(buf, spec, oldscope, &new);
	newevent->flags = flag_event;
	newevent->type = Tfunc;
	allevents = eventnode_Create(genEnter(),
		oldscope, compNewScope(),
		ness_CreateMark(curComp->ness,
				tlex_RecentPosition(curComp->tlex, -2, 0), 
				999),
		NULL, e, 
		(unsigned char *)cname,	/* pass ownership of cname */
		curComp->ness,
		NULL,		/* no TriggerHolder */
		FALSE,		/* not enabled yet */
		NULL, 		/* no rock yet */
		allevents, 	/* chain all the events together. */
		&allevents 	/* where to store ptr to this eventnode */
	);
	nesssym_NSetINode(newevent, eventnode, allevents);

	/* update the ptr to where the ptr to the last node is kept. */
	if(lastnode) lastnode->meptr= &(allevents->next);

	return newevent;
}


/* neventFinishEvent(event, locals, e)
	complete processing ON ... END
	check 'e' versus value in event's eventnode
	finish the function and do fixups for it 
*/
	struct nesssym *
neventFinishEvent(event, locals, e)
	struct nesssym *event, *locals, *e;
{
	struct eventnode *enode = nesssym_NGetINode(event, eventnode);
	long loc, len;
	int bleah=((struct nessmark *)getcurfuncmark())-SysMarkLowEnd;
	
	if (e != enode->varsym) {
		char buf[50];
		sprintf(buf, "*Should end \"%s\"",
				  nesssym_NGetName(enode->varsym));
		ReportError(freeze(buf), -1);
	}
	enode->locallist = locals;
	loc = tlex_RecentPosition(curComp->tlex, 0, &len);
	mark_SetLength(enode->where,  
			loc + len - mark_GetPos(enode->where));

	if ( ! genExit(NULL, locals))
		SaveError(":More than 10 arguments and locals", 
			mark_GetPos(enode->where),
			mark_GetLength(enode->where));

	compPopScope();
	genLocStore(event);   genLinkGlobal(event);
	return event;
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\

	Run Time
	Event information return

\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = */

	static union stackelement *
unstackLong(NSP, iar, pLong)
	union stackelement *NSP;
	unsigned char *iar;
	long *pLong;
{
	if (NSP->l.hdr != longHdr)
		RunError(":arg should be an integer", iar);
	*pLong = NSP->l.v;
	return NSPopSpace(longstkelt); 
}


/* unstackObject(NSP, iar, pObj, type)
	pop an object value off the stack and into *pObj
	if 'type' is not NULL, check that *pObj is a non-NULL ptr to the given type
	Return the new value of NSP.
	Use iar for RunError.
	The types of pObj and type are specific to valueview, but the routine can 
	be used to pop any object by using casts on the arguments.
*/
	static union stackelement *
unstackObject(NSP, iar, pObj, type)
	union stackelement *NSP;
	unsigned char *iar;
	struct valueview **pObj;
	struct classinfo *type;
{
	if (NSP->p.hdr != ptrHdr)
		RunError(":arg should be an object", iar);
	if (type == NULL) {}
	else if (NSP->p.v == NULL)
		RunError(":ptr to object is NULL", iar);
	else {
		*pObj = (struct valueview *)
			ProperPtr((struct basicobject *)NSP->p.v, type);
		if (*pObj == NULL) {
			/* XXX duplicates code in call.c:callCFunc */
			char *buf;
			buf = malloc(60 + strlen(class_GetTypeName(NSP->p.v)) 
				+ strlen(class_GetTypeName(type)));
			sprintf(buf, 
				"*object is /%s/ and does not contain /%s/",
				class_GetTypeName(NSP->p.v),
				class_GetTypeName(type));
			RunError(buf, iar);
		}
	}
	return NSPopSpace(ptrstkelt); 
}

/* neventInfo(op, iar, ness)
	return information about the most recent interactive input
*/
	void
neventInfo(op, iar, ness)
	unsigned char op;
	unsigned char *iar;
	struct ness *ness;
{
	union stackelement *NSP = NSPstore;
	long v;
	boolean bv;
	unsigned char *s;
	struct view *vptr;
	struct valueview *vvwobj;
	struct value *vobj;

	switch (op) {
	case 'a':
		v = (long)view_LeftDown;
		goto stackLong;
	case 'b':
		v = (long)view_LeftUp;
		goto stackLong;
	case 'c':
		v = (long)view_LeftMovement;
		goto stackLong;
	case 'd':
		v = (long)view_RightDown;
		goto stackLong;
	case 'e':
		v = (long)view_RightUp;
		goto stackLong;
	case 'f':
		v = (long)view_RightMovement;
		goto stackLong;
	case 'k':
		if (LastKeys == NULL)
			RunError(":no keys event in progress", iar);
		s = LastKeys;
		goto stackString;
	case 'm':
		if (LastMenu == NULL)
			RunError(":no menu event in progress", iar);
		s = LastMenu;
		goto stackString;

	case 'n':		/* value_getvalue(obj) => long */
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		v = value_GetValue(vobj);
		goto stackLong;
	case 'o':		/* value_getarraysize(obj) => long  */
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		v = value_GetArraySize(vobj);
		goto stackLong;
	case 'p':		/* value_getstring(obj) => string */
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		s = (unsigned char *)value_GetString(vobj);
		goto stackString;
	case 'q':	{	/* value_getarrayelt(obj, index) => string */
		long index;
		unsigned char **sar;
		NSP = unstackLong(NSP, iar, &index);
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		if (index <0  ||  index > value_GetArraySize(vobj))
			RunError(":index out of bounds", iar);
		sar = (unsigned char **)value_GetStringArray(vobj);
		s = sar[index];
	}	goto stackString;
	case 'r':		/* value_setvalue(obj, long) */
		NSP = unstackLong(NSP, iar, &v);
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		value_SetValue(vobj, v);
		break;
	case 's':		/* value_setarraysize(obj, long)  */
		NSP = unstackLong(NSP, iar, &v);
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		if (v != value_GetArraySize(vobj)) {
			/* create new array of the given size
				XXX never freed  */ 			value_SetStrArrayAndSize(vobj, 
				calloc(v, sizeof(unsigned char *)), v);
					/* init to NULL ptrs */
		}
		break;

	case 't':	{	/* value_setstring(obj, str) */
		PTSTOMARK(&NSP->m, iar);
		s = nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		/* store a pointer to the malloced string returned by ToC
			XXX never freed */
		value_SetString(vobj, s);
	}	break;
	case 'u':	{	/* value_setarrayelt(obj, index, str) */
		long index;
		unsigned char **sar;
		PTSTOMARK(&NSP->m, iar);
		s = nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);	/* discard the marker now */
		NSP = unstackLong(NSP, iar, &index);
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		if (index <0  ||  index >= value_GetArraySize(vobj))
			RunError(":index out of bounds", iar);
		/* store the pointer to the malloced string into the malloced array
			XXX never freed */
		sar = (unsigned char **)value_GetStringArray(vobj);
		sar[index] = s;
		value_NotifyObservers(vobj, value_NEWVALUE);
	}	break;
	case 'v':		/* value_setnotify(obj, bool) */
		/* NSP = unstackBool(NSP, iar, &v); */
		if (NSP->b.hdr != boolHdr)
			RunError(":arg should be a boolean", iar);
		v = NSP->b.v;
		NSPopSpace(boolstkelt); 
		NSP = unstackObject(NSP, iar, &vvwobj, valueviewClass);
		vobj = (struct value *)vvwobj->header.view.dataobject;
		value_SetNotify(vobj, v);
		break;

	case 'w':		/* mouseaction */
		if (MouseAction == -99)
			RunError(":no mouse event in progress", iar);
		v = MouseAction;
		goto stackLong;
	case 'x':		/* mousex */
		if (MouseAction == -99)
			RunError(":no mouse event in progress", iar);
		v = MouseX;
		goto stackLong;
	case 'y':		/* mousey */
		if (MouseAction == -99)
			RunError(":no mouse event in progress", iar);
		v = MouseY;
		goto stackLong;
	case 'C':
		im_QueueCancellation();
		break;
	case 'F':		/* currentinputfocus */
		vptr = (im_GetLastUsed() == NULL) ? NULL
				:  im_GetInputFocus(im_GetLastUsed());	
		goto stackPointer;
	case 'H':  {	/* DoHit(inset, action, x, y) */
		/* XXX ought to guarantee an up for every transmitted down */
		long x, y, act;
		struct view *inset;
		static struct view *hitee = NULL, *hiteesource = NULL;
		NSP = unstackLong(NSP, iar, &y);
		NSP = unstackLong(NSP, iar, &x);
		NSP = unstackLong(NSP, iar, &act);
		/* now inset is at top of stack */
		if (NSP->p.hdr != ptrHdr)
			RunError(":arg should be an object", iar);
		inset = (struct view *)NSP->p.v;
		NSPopSpace(ptrstkelt);
		if (inset == NULL  ||  !  class_IsType(inset, viewClass))
			RunError(":first arg should be a view", iar);
		if (ness->CurrentInset == inset  &&  class_IsType(inset, celviewClass)) {
			/* we short cut around the celview to avoid retrapping a mouse hit */
			if (celview_GetApplication((struct celview *)inset) != NULL)
				inset = celview_GetApplication((struct celview *)inset);
			else if (celview_GetTrueChild((struct celview *)inset) != NULL)
				inset = celview_GetTrueChild((struct celview *)inset);
		}

		if (hiteesource != inset  ||  act == (long)view_LeftDown  ||  act == (long)view_RightDown) {
			hiteesource = inset;
			hitee = view_Hit(inset, act, x, y, SavedClickCount);  /* XXX do we need a click count? */
		}
		else {
			/* move or up, with inset the same as where we sent the last hit:
				 send to hitee */
			hitee = view_Hit(hitee,
			    act,
			    physical_GlobalXToLogicalX(view_GetDrawable(hitee),
				physical_LogicalXToGlobalX(view_GetDrawable(inset), x)),
			    physical_GlobalYToLogicalY(view_GetDrawable(hitee),
				physical_LogicalYToGlobalY(view_GetDrawable(inset),  y)),
			    SavedClickCount);	/* XXX do we need a click count? */
		}
	}	break;	/* end case 'H' */
	case 'I':		/* currentinset */
		vptr = ness->CurrentInset;
		if (vptr == NULL) 
			vptr = (struct view *)ness->DefaultText;
		goto stackPointer;
	case 'J':		/* currentwindow */
		vptr = ness->CurrentInset;
		if (vptr == NULL) 
			vptr = (struct view *)ness->DefaultText;
		if (vptr == NULL) 
			goto stackPointer;
		vptr = (struct view *)view_GetIM(vptr);
		goto stackPointer;

 	/* launchApplication(marker, title, programname, enablecommands)
	{"launchApplication", "UP", {Tvoid, Tstr, Tstr, Tstr, Tbool, Tend}, ness_codeGreen}, */

	case 'P':	{
		boolean enablecommands;
		char *pgmnm, * title;
		struct text *text;
		long pos, len;

		if (NSP->l.hdr != boolHdr)
			RunError(":last argument should be a boolean value", iar);
		enablecommands = NSP->b.v;
		NSP = popValue(NSP);

		PTSTOMARK(&NSP->m, iar);
		pgmnm = (char *)nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);

		PTSTOMARK(&NSP->m, iar);
		title = (char *)nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);

		PTSTOMARK(&NSP->m, iar);
		text = FrameMark(ness, &NSP->m, 
			title, pgmnm, enablecommands, &pos, &len);

		free(pgmnm);
		free(title);
		nessmark_Set(&NSP->m, text, pos, len);
	}	break;
	/* QueueAnswer(marker)
	 {"QueueAnswer", "UQ", {Tvoid, Tstr, Tend}, ness_codeGreen}, */
        case 'Q': {
		unsigned char *buf;
		PTSTOMARK(&NSP->m, iar);
		buf = nessmark_ToC(&NSP->m);
		im_QueueAnswer(buf);
		free(buf);
		NSP = popValue(NSP);
	}	break;
	case 'R':		/* isreadonly(string) */
		PTSTOMARK(&NSP->m, iar);
		bv = simpletext_GetReadOnly(nessmark_GetText(&NSP->m));
		NSP = popValue(NSP);
		goto stackBool;
	case 'T':	{	/* TellUser(msg) */
		unsigned char *buf;
		PTSTOMARK(&NSP->m, iar);
		buf = nessmark_ToC(&NSP->m);
		message_DisplayString(im_GetLastUsed(), 0, buf);
		im_ForceUpdate();
		free(buf);
		NSP = popValue(NSP);
		ness->ToldUser = TRUE;
	}	break;
	case 'U':	{	/* AskUser(prompt, default) */
		unsigned char *prompt, *defaultstring;
		unsigned char buf[301];

		PTSTOMARK(&NSP->m, iar);
		defaultstring = nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);
		PTSTOMARK(&NSP->m, iar);
		prompt = nessmark_ToC(&NSP->m);
		NSP = popValue(NSP);
		if (message_AskForString(im_GetLastUsed(), 0,  prompt, 
				defaultstring, buf, 300) < 0) {
			message_DisplayString(im_GetLastUsed(), 0, "Cancelled.");
			strcpy(buf, "CANCEL");
		}
		s = buf;
		free(prompt);			
		free(defaultstring);
		ness->ToldUser = FALSE;
	}	goto stackString;


	stackBool:
		NSPushSpace(boolstkelt); 
		NSP->b.hdr = boolHdr;
		NSP->b.v = bv;
		break;
	stackLong:
		NSPushSpace(longstkelt); 
		NSP->l.hdr = longHdr;
		NSP->l.v = v;
		break;
	stackString:
		NSPushSpace(nessmark); 
		nessmark_Initialize(&NSP->m);
		nessmark_MakeConst(&NSP->m, s);
		break;
	stackPointer:
		NSPushSpace(ptrstkelt); 
		NSP->p.hdr = ptrHdr;
		NSP->p.v = (struct basicobject *)vptr;
		break;
	}		/* end switch(op) */
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\

	Run Time
	Event handlers

\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = */

static boolean PostDebug = FALSE;	/* side arg to HandleNamedCell */

	void
InterpretEvent(obj, enode)
	struct view *obj;
	struct eventnode *enode;
{
	struct ness *ness = enode->parentness;
	struct view *RememberCurrentInset;
		/* The "CurrentInset" value in a normal Ness is its text 
			or maybe the inset active when ESC-ESC was picked
		   We save the currentinset value and replace it with the one
		   from the inset that had the event. */
	if ( ! enode->enabled) 
		return;
	ness_EstablishViews(ness, obj);
	RememberCurrentInset = ness->CurrentInset;
	ness->CurrentInset = obj;

	ness->ErrorList = callInitAll(ness);
	if (ness->ErrorList == NULL)
		ness->ErrorList = interpretNess(enode->SysMarkOffset, 
				NULL, ness);
	ness->CurrentInset = RememberCurrentInset;

	if (ness->ErrorList != NULL) {
		neventUnpost(ness, FALSE);	/* remove old postings */
		MapRunError(ness);
		ness_Expose(ness);
	}
}


/* MouseProcStub(obj, action, x, y, nclicks, enode)
	This procedure is called by a mouse event.
	From the xattr it gets everything it needs to initiate the proper Ness stmtList.
	 	these are the possible mouse actions:
			view_NoMouseEvent,
			view_LeftDown, view_LeftUp, view_LeftMovement,
			view_RightDown, view_RightUp, view_RightMovement
*/
	struct view *
MouseProcStub(obj, action, x, y, nclicks, enode)
	struct view *obj;	/* the celview */
	enum view_MouseAction action;
	long x, y, nclicks;
	struct eventnode *enode;
{
	/* XXX we need to scan xattr looking for other mouse event handlers */
	MouseX = x;
	MouseY = y;
	MouseAction = (long)action;
	SavedClickCount = nclicks;
	InterpretEvent(obj, enode);
	MouseAction = -99;
	SavedClickCount = 1;		/* yuck XXX should give this to client */
	return obj;
}

/* MenuProcStub(obj, enode)
	This procedure is called by a menu selection event.  
	From the xattr it gets everything it needs to initiate the proper Ness stmtList.
*/
	void
MenuProcStub(obj, enodeptr)
	struct view *obj;	/* the celview, I think. I will ignore it */
	char *enodeptr;
{
	struct eventnode *enode;
	struct eventnode *p=allevents;

	if(!enodeptr) return;
	enode=(struct eventnode *)atol(enodeptr);

	/* Make sure this event still exists. */
	while(p && p!=enode) p=p->next;

	if(!p) return;
	
	LastMenu = enode->spec;
	InterpretEvent(obj, enode);
	LastMenu = NULL;
}

/* KeyProcStub(obj, enode)
	This procedure is called by a key sequence event.
	From the enode it gets everything it needs to initiate the proper Ness stmtList.
*/
	void
KeyProcStub(obj, enodeptr)
	struct view *obj;	/* the celview, I think. I will ignore it */
	char *enodeptr;
{
	struct eventnode *enode;
	struct eventnode *p=allevents;
	
	if(!enodeptr) return;
	enode=(struct eventnode *)atol(enodeptr);

	/* Make sure this event still exists.  */
	while(p && p!=enode) p=p->next;

	if(!p) return;

	LastKeys = enode->spec;
	InterpretEvent(obj, enode);
	LastKeys = NULL;
}


EventEventStub(ness, obj, enode)
	struct ness *ness;
	struct view *obj;
	struct eventnode *enode;
{
	InterpretEvent(obj, enode);
}


/* PostMenuEvent(ness, onode, enode)
	the script 'ness' has an extension for object described by objnode 'onode'
		and having a Menu event 'enode'
	post it to onode->obj
*/
	static void
PostMenuEvent(ness, onode, enode)
	struct ness *ness;
	struct objnode *onode;
	struct eventnode *enode;
{
	unsigned char name[400];
	if (onode->menupe == NULL) {
		/* create (or find) menu pe for this class */
		sprintf(name, "%s-NessMenuEvent",
				class_GetTypeName(onode->obj));
		onode->menupe = proctable_DefineProc(pfreeze(name),
				MenuProcStub, class_GetType(onode->obj), 
				class_GetTypeName(onode->obj), 
				"Call Ness for a menu event");
	}
	if (onode->menulist == NULL) {
		/* need to create and post the menulist */
		onode->menulist = menulist_Create(onode->obj);
		if (onode->ExtendAView) {
			if (globalInit != NULL)
				init_AddMenuBinding(globalInit, 
					class_GetTypeName(onode->obj), 
					TRUE, onode->menulist);
		}
		else celview_SetMenulist(onode->obj, onode->menulist);
	}
	sprintf(name, "%d", enode);
	enode->rock=(char *)malloc(strlen(name)+1);
	if(enode->rock) {
	    strcpy(enode->rock, name);
	/* add the key sequence to the keymap */
		   menulist_AddToML(onode->menulist, enode->spec, onode->menupe, (long)enode->rock, 0);
		   enode->enabled = TRUE;
	}
}

/* PostKeysEvent(ness, onode, enode)
	the script 'ness' has an extension for object described by objnode 'onode'
		and having a Keys event 'enode'
	post it to onode->obj
*/
	static void
PostKeysEvent(ness, onode, enode)
	struct ness *ness;
	struct objnode *onode;
	struct eventnode *enode;
{
	unsigned char name[400];
	if (onode->keype == NULL) {
		/* create (or find) keys pe for this class */
		sprintf(name, "%s-NessKeysEvent",
				class_GetTypeName(onode->obj));
		onode->keype = proctable_DefineProc(pfreeze(name),
				KeyProcStub, 	class_GetType(onode->obj), 
				class_GetTypeName(onode->obj), 
				"Call Ness for a keys event");
	}
	if (onode->keymap == NULL) {
		/* need to create and post the keymap */
		onode->keymap = keymap_New();
		if (onode->ExtendAView) {
			if (globalInit != NULL)
				init_AddKeyBinding(globalInit, 
					class_GetTypeName(onode->obj),
					TRUE, onode->keymap);
		}
		else celview_SetKeymap(onode->obj, onode->keymap);
	}

	sprintf(name, "%d", enode);
	enode->rock=(char *)malloc(strlen(name)+1);
	if(enode->rock) {
	    strcpy(enode->rock, name);
	    /* add the key sequence to the keymap */
	    keymap_BindToKey(onode->keymap, enode->spec, onode->keype, enode->rock);
	    enode->enabled = TRUE;
	}
}

/* PostMouseEvent(ness, onode, enode)
	the script 'ness' has an extension for object described by objnode 'onode'
		and having a Mouse event 'enode'
	post it to onode->obj
*/
	static void
PostMouseEvent(ness, onode, enode)
	struct ness *ness;
	struct objnode *onode;
	struct eventnode *enode;
{
	if (onode->MouseEnabled || onode->ExtendAView) 
		return;
	celview_SetHitfunc(onode->obj, MouseProcStub, enode);
	onode->MouseEnabled = TRUE;
	enode->enabled = TRUE;
}


/* TryTrigger(obj, trigger, ness, enode)
	Try AddRecipient for 'obj' to see if it has the trigger
	If successful set enode->TriggerHolder and enode->enable
	return TRUE for success and FALSE for failure
*/
	static boolean
TryTrigger(obj, trigger, ness, enode)
	struct observable *obj;
	struct atom *trigger;
	struct ness *ness;
	struct eventnode *enode;
{
	if (obj == NULL) {
		return FALSE;
	}
	if (observable_AddRecipient(obj, trigger, ness, 
				EventEventStub, enode))  {
		enode->TriggerHolder = obj;
		enode->enabled = TRUE;
		return TRUE;
	}
	if (class_IsType(obj, viewClass)) {
	    boolean foo=TryTrigger(&(((struct view *)obj)->dataobject->header.observable), 
				trigger, ness, enode);
	    return foo;
	}
	return FALSE;
}


/*  PostEventEvent(ness, onode, enode)
	post  the event 'enode' to the object given by 'onode', using the 'ness'

	This event derives from   --  on event "..." --  in the source;
	it corresponds to a trigger defined by the target object.

	To process triggers we just do observable_AddRecipient().
	If it returns FALSE, we try another portion of the cel.
	We look at the cel, the celview, the child, and the true child,
	the child's data object, and the true child's data object.
*/
PostEventEvent(ness, onode, enode)
	struct ness *ness;
	struct objnode *onode;
	struct eventnode *enode;
{
	struct atom *trigger;

	if (FOLDEDEQ("becamevisible", enode->spec)) {
		enode->enabled = TRUE;
		return;
	}
	if (onode->ExtendAView) 
		return;
	trigger = atom_Intern(enode->spec);
	if (TryTrigger(&(onode->obj->header.observable), trigger, ness, enode))  
		return;
	if (class_IsType(onode->obj, celviewClass)) {
		struct celview *cv = (struct celview *)onode->obj;
		if (TryTrigger(&(celview_GetTrueChild(cv)->header.observable),
				trigger, ness, enode))  
			return;
		if (celview_GetApplication(cv)  !=  celview_GetTrueChild(cv)
				&&  TryTrigger(&(celview_GetApplication(cv)
						->header.observable),
					trigger, ness, enode))
			return;
	}
	if (class_IsType(onode->obj, imClass)) {
		struct view *v = ((struct im *)onode->obj)->topLevel;
		if (TryTrigger(&(v->header.observable), trigger, ness, enode))
			return;
		if (class_IsType(v, lpairClass)  &&
				TryTrigger(&(lpair_GetNth((struct lpair *)v,
						 0)->header.observable), 
					trigger, ness, enode))
			return;
	}
	/* well, we didn't find it.  
	XXX we ought to post an error message */
}


/* postevents(ness, attr)
	the script 'ness' has an extension for object with name 'attr'
	post all events intended for this object
*/
	static void
postevents(ness, attr)
	struct ness *ness;
	struct nesssym *attr;
{
	boolean repost;
	struct nesssym *xattr;
	struct objnode *onode;
	char buf[100];

	repost = FALSE;

	if (PostDebug)
		printf("Posting events for %s\n", 
			nesssym_ToC(attr, ness, buf, sizeof(buf)-1));

	/* traverse xattr's, posting each event */
	onode = nesssym_NGetINode(attr, objnode);
	for (xattr = onode->attrs; xattr != NULL; xattr = xattr->next) 
		if (xattr->flags == flag_event) {
			struct eventnode *enode = nesssym_NGetINode(xattr, eventnode);
			int n;
			if (PostDebug)
				printf("	%s event (%s)\n", 
					nesssym_NGetName(enode->varsym),
					enode->spec);
			xattr->parent.nesssym = attr;	/* ensure that parent is set */
			n = enode->varsym->header.toksym.toknum;
			if (n == ntMENU) {
				PostMenuEvent(ness, onode, enode);
				repost = TRUE;
			}
			else if (n == ntKEYS) {
				PostKeysEvent(ness, onode, enode);
				repost = TRUE;
			}
			else if (n == ntMOUSE) 
				PostMouseEvent(ness, onode, enode);
			else if (n == ntEVENT) 
				PostEventEvent(ness, onode, enode);
		} /* end if (and for) */

	if (repost  && ! onode->ExtendAView)  
		celview_Repost(onode->obj);
}

/* unpostevents(ness, attr, xattr)
	the script 'ness' has an extension for object with name 'attr'
	unpost all its events
*/
	static void
unpostevents(ness, attr, debug)
	struct ness *ness;
	struct nesssym *attr;
	boolean debug;
{
	register struct objnode *onode = nesssym_NGetINode(attr, objnode);
	struct nesssym *xattr;
	boolean repost;
	char buf[100];

	if (onode->ExtendAView) {
		if (onode->menulist != NULL && globalInit != NULL)
		    init_DeleteMenuBinding(globalInit, class_GetTypeName(onode->obj), TRUE, 
					onode->menulist);
		if (onode->keymap != NULL && globalInit != NULL)
			init_DeleteKeyBinding(globalInit, class_GetTypeName(onode->obj), TRUE, 
					      onode->keymap);
		if (onode->keymap != NULL)  {
		    keymap_Destroy(onode->keymap);
		    onode->keymap = NULL;
		}
		if (onode->menulist != NULL) {
		    menulist_Destroy(onode->menulist);
		    onode->menulist = NULL;
		}
		return;
	}
	/* it is an extended object   (ExtendAView is false)*/

	/* set onode->obj or give error
		look up the object by name again in case it has been cut from the base text */
	if (ness_GetArbiter(ness) == NULL  ||  
			(onode->obj=arbiterview_GetNamedCelview(
					ness_GetArbiter(ness),
					nesssym_ToC(attr, ness, buf, sizeof(buf)-1)))
				== NULL) {
		if (debug) {
			printf("unpostevents could not find \"%s\" in arb at 0x%lx\n",
				nesssym_ToC(attr, ness, buf, sizeof(buf)-1),
				ness_GetArbiter(ness));
		}
		return;
	}

	/* traverse xattr's, disabling each event */
	for (xattr = onode->attrs; xattr != NULL; xattr = xattr->next) {
		struct eventnode *enode 
				= nesssym_NGetINode(xattr, eventnode);
		if (xattr->flags == flag_event) 
			enode->enabled = FALSE;
		if (enode->TriggerHolder != NULL) {
			observable_DeleteRecipient(enode->TriggerHolder, 
				atom_Intern(enode->spec), ness);
			enode->TriggerHolder = NULL;
		}
	}

	repost = FALSE;

	if (onode->obj == NULL) 
		return;
	if (onode->menulist != NULL) {
		/* tell the object it has no menulist */
		celview_SetMenulist(onode->obj, NULL); 
		repost = TRUE;
	}
	if (onode->keymap != NULL)  {
		/* tell the object it has no keymap */
		celview_SetKeymap(onode->obj, NULL);
		repost = TRUE;
	}
	if (onode->MouseEnabled) {
		celview_SetHitfunc(onode->obj, NULL, 0);
		onode->MouseEnabled = FALSE;
	}

	if (repost)   celview_Repost(onode->obj);

	if (onode->keymap != NULL)  {
		keymap_Destroy(onode->keymap);
		onode->keymap = NULL;
	}
	if (onode->menulist != NULL) {
		menulist_Destroy(onode->menulist);
		onode->menulist = NULL;
	}
}

/* HandleNamedCel(cv, ness)
	This function is called each time an arbiter finds out about a named cel.
	It scans the NessList looking for a ness with an extend for a cel of that name.
	(The ness arg is ignored because arbiters may have multiple nesses.)
*/
	static void
HandleNamedCel(cv, null)
	struct celview *cv;
	struct ness *null;
{
	struct cel *c;
	unsigned char *tname;
	struct ness *n;
	struct nesssym *attr;
	struct objnode *onode;
	struct nesssym *xattr;

	c = (struct cel *)cv->header.view.dataobject;
	if (c == NULL  ||  cel_GetRefName(c) == NULL) return;
	tname = CelSymName((unsigned char *)cel_GetRefName(c));
	for (n = ness_GetList();  n != NULL;  n = n->next) {
		if ( ! n->compiled  ||  ness_GetArbiter(n) == NULL)  continue;
		if (cv->arb == NULL || ness_GetArbiter(n) != cv->arb) continue;
		if (strcmp(tname, "&defaulttext") == 0)
			/* We are being told of a child called "defaulttext" and n
			    is associated with cv->arb.  Set n->DefaultText.*/
			ness_SetDefaultText(n, (struct textview *)
					ProperPtr((struct basicobject *)cv, 
						textviewClass));
		attr = nesssym_NFind(tname, n->constScope);
		if (attr == NULL || attr->flags != flag_xobj) continue;

		/* 'attr' is the symbol entry for an EXTEND block for the given name */

		onode = nesssym_NGetINode(attr, objnode);
		onode->obj = cv;
		attr->parent.ness = n;	/* set parent */
		postevents(n, attr);

		/* check to see if it has an enode for 'becameVisible' */

		for (xattr = onode->attrs; xattr != NULL; xattr = xattr->next) 
		    if (xattr->flags == flag_event) {
			struct eventnode *enode = nesssym_NGetINode(xattr, eventnode);
			if (enode->varsym->header.toksym.toknum == ntEVENT &&
				    FOLDEDEQ ("becamevisible", enode->spec)) {
				/* it has "becameVisible": call it */
				EventEventStub(n, (struct view *)cv, enode);
				break;	/* from the for(xattr) */
			}
		    } /* end if (and for) */
		return;
	}
}



/* neventPost (ness)
	Post all events for the extended objects in ness->globals
*/
	void 
neventPost (ness, debug)
	struct ness *ness;
	boolean debug;
{
	struct arbiterview *arb = ness_GetArbiter(ness);
	struct nesssym *attr;

	PostDebug = debug;

	globalInit = im_GetGlobalInit();

	if (arb != NULL)
		arbiterview_AddHandler(arb, HandleNamedCel, NULL);

	/* process -extend "view:name"- */
	for (attr = ness->globals; attr != NULL; attr = attr->next) 
		/* visit each global entity in the script,
		   if it is an xobj with name beginning "&view:"
		   post the events */
		if (attr->flags == flag_xobj  
				&& nesssym_NGetINode(attr, objnode)->ExtendAView) {
			/* process its -on proc-s */
			attr->parent.ness = ness;	/* set parent */
			postevents(ness, attr); 
		}
}


/* neventUnpost (ness)
	Remove all events posted for the extended objects in ness->globals
*/
	void
neventUnpost (ness, debug)
	struct ness *ness;
	boolean debug;
{
	struct nesssym *attr;
	char buf[100];

	globalInit = im_GetGlobalInit();

	for (attr = ness->globals; attr != NULL; attr = attr->next) 
		/* visit each global entity in the script,
		   if it is an xobj, and has events, unpost them */
		if (attr->flags == flag_xobj) {
			if (debug)
				printf("Unposting events for %s\n",
					nesssym_ToC(attr, ness, buf, sizeof(buf)-1));
			attr->parent.ness = ness;	/* set parent */
			unpostevents(ness, attr, debug);
		}
}
