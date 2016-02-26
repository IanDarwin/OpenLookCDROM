#ident	"@(#)selection.c	26.19	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *	Sun design patents pending in the U.S. and foreign countries. See
 *	LEGAL_NOTICE file for terms of the license.
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"
#include "list.h"
#include "mem.h"


/* ===== global data ====================================================== */

Time SelectionTime;

/* ===== private data ===================================================== */

typedef struct _selection_registry {
    Atom selection;
    Bool (*handler)();
} SelectionRegistry;

/*
 * selectList is the list of selected clients; it's used for the PRIMARY 
 * selection.
 */
static List *selectList = NULL_LIST;

/*
 * selectionRegistry is the list of selections and their handler functions.
 */
static List *selectRegistry = NULL_LIST;

/* ===== externals ======================================================== */

extern Atom	AtomAtomPair;
extern Atom	AtomClientWindow;
extern Atom	AtomLength;
extern Atom	AtomListLength;
extern Atom	AtomName;
extern Atom	AtomMultiple;
extern Atom	AtomTargets;
extern Atom	AtomTimestamp;

/* ===== private functions ================================================ */

/*
 * processPrimaryTarget
 * 
 * Process the conversion of a single target for the PRIMARY selection.  Used
 * for both single requests and for MULTIPLE requests.  Returns True if the
 * conversion was successful, otherwise False.
 */
static Bool
processPrimaryTarget(dpy, requestor, target, property)
    Display *dpy;
    Window requestor;
    Atom target;
    Atom property;
{
    unsigned long data[10];	/* long enough for most things */
    unsigned char *propdata = (unsigned char *) data;
    int format, nelements, i;
    Client *cli;
    Atom type;
    Bool freedata = False;
    Window *wp;

    if (target == AtomTargets) {
	data[0] = AtomTargets;
	data[1] = AtomTimestamp;
	data[2] = AtomListLength;
	data[3] = XA_DRAWABLE;
	data[4] = AtomLength;
	data[5] = AtomMultiple;
	data[6] = AtomName;
	data[7] = AtomClientWindow;
	nelements = 8;
	type = XA_ATOM;
	format = 32;
    } else if (target == AtomTimestamp) {
	data[0] = SelectionTime;
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == AtomListLength) {
	data[0] = ListCount(selectList);
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == AtomLength) {
	data[0] = ListCount(selectList)*sizeof(long);
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == XA_DRAWABLE) {
	nelements = ListCount(selectList);
	propdata = (unsigned char *)
			MemCalloc(nelements, sizeof(unsigned long));
	freedata = True;
	wp = (Window *) propdata;
	i = 0;
	cli = NULL;
	while ((cli = EnumSelections(cli)) && (i<nelements))
	    *wp++ = PANEWINOFCLIENT(cli);
	type = XA_DRAWABLE;
	format = 32;
    } else if (target == AtomName) {
	int curlen = 0;
	int maxlen = 100;
	int tmplen;
	char *cp, *tmp;

	propdata = MemAlloc(maxlen);
	cp = (char *) propdata;

	cli = NULL;
	while (cli = EnumSelections(cli)) {
#ifdef OW_I18N_L4
	    if (cli->framewin != NULL && cli->framewin->fcore.name != NULL) {
		wchar_t *wcs = cli->framewin->fcore.name;
	        int n = wslen(wcs) * sizeof(wchar_t) + 1;
		tmp = (char*) MemAlloc(n);
		wcstombs(tmp, wcs, n);
	    }
#else
	    if (cli->framewin != NULL && cli->framewin->fcore.name != NULL)
		tmp = cli->framewin->fcore.name;
#endif
	    else
		tmp = "";

	    tmplen = strlen(tmp) + 1;
	    if (curlen + tmplen > maxlen) {
		maxlen += 100;
		propdata = MemRealloc(propdata, maxlen);
	    }
	    strcpy((char *) propdata+curlen, tmp);
	    curlen += tmplen;
	}

	nelements = curlen;
	type = XA_STRING;
	format = 8;
    } else if (target == AtomClientWindow) {
	data[0] = NoFocusWin;
	nelements = 1;
	type = XA_WINDOW;
	format = 32;
    } else {
	return False;
    }

    XChangeProperty(dpy, requestor, property, type, format, PropModeReplace,
		    (unsigned char *)propdata, nelements);

    if (freedata)
	MemFree(propdata);

    return True;
}


/*
 * handlePrimary
 * 
 * Respond to a SelectionRequest or SelectionClear event on the PRIMARY
 * selection.
 */
static void
handlePrimary(event)
    XEvent *event;
{
    XSelectionRequestEvent *request;
    XSelectionEvent response;
    Atom *pairs;
    unsigned long nitems, remain;
    int i;
    Bool writeback = False;

    switch (event->type) {
    case SelectionClear:
	ClearSelections(event->xany.display);
	return;
    case SelectionRequest:
	/* use code below */
	break;
    default:
	return;
    }

    request = (XSelectionRequestEvent *) event;

    /*
     * Fill in the response message.  We fill in the property field with None 
     * here.  If the conversion fails, we just send this message.  If a 
     * conversion succeeds, the property field is filled in appropriately.
     */

    response.type = SelectionNotify;
    response.serial = request->serial;
    response.requestor = request->requestor;
    response.selection = request->selection;
    response.time = request->time;
    response.target = request->target;
    response.property = None;

    if (request->target == AtomMultiple) {
	if (request->property != None) {
	    pairs = GetWindowProperty(request->display, request->requestor,
				      request->property, 0L, 100000L,
				      AtomAtomPair, 32, &nitems, &remain);
	    if (pairs != NULL) {
		/*
		 * Process each pair of atoms (target, property).  Watch out 
		 * for an odd last atom, and for property atoms of None.  If 
		 * the conversion fails, replace it with None in the original 
		 * property.
		 */
		for (i = 0; i+1 < nitems; i += 2) {
		    if (pairs[i+1] == None)
			continue;

		    if (!processPrimaryTarget(request->display,
			request->requestor, pairs[i], pairs[i+1]))
		    {
			pairs[i+1] = None;
			writeback = True;
		    }
		}

		if (writeback)
		    XChangeProperty(request->display, request->requestor,
			request->property, AtomAtomPair, 32, PropModeReplace,
			(unsigned char *) pairs, nitems);

		XFree((char *) pairs);
		response.property = request->property;
	    }
	}
    } else {
	/*
	 * Handle a single request.  If its property field is None, the
	 * requestor is using an obsolete draft of the ICCCM.  Per the
	 * suggestion in ICCCM section 2.2, use the target name as the
	 * property name.
	 */
	if (request->property == None)
	    request->property = request->target;

	if (processPrimaryTarget(request->display, request->requestor,
			 request->target, request->property))
	    response.property = request->property;
    }

    XSendEvent(request->display, request->requestor, False,
	       NoEventMask, (XEvent *)&response);
}


/* ===== public functions ================================================= */

/*
 * IsSelected
 *
 * Determine whether a client is selected.
 * 
 * REMIND: is this really necessary?  Why not just use cli->isSelected in open 
 * code?
 */
Bool
IsSelected(cli)
	Client *cli;
{
	return cli->isSelected;
}


/*
 * AddSelection
 *
 * Add this client to the list of clients on the PRIMARY selection and mark
 * the client as being selected.  Acquires the PRIMARY selection if necessary.
 */
int
AddSelection(cli, timestamp)
	Client *cli;
	Time timestamp;
{
	List *l = selectList;
	Client *tc;

	if (selectList == NULL_LIST)
	{
		/*
		 * Since we don't have anything selected, we must acquire the 
		 * selection.  If we don't actually get the selection, fail 
		 * silently.  This may occur if the user selects something 
		 * else after selecting a window, but we are slow to respond.
		 */
		XSetSelectionOwner(cli->dpy, XA_PRIMARY,
				   NoFocusWin, timestamp);
		if (NoFocusWin != XGetSelectionOwner(cli->dpy, XA_PRIMARY))
		    return;
	} else {
		/* First look to see if window is already listed. */
		for(tc = ListEnum(&l); tc != NULL; tc = ListEnum(&l))
		{
			if (tc == cli)
				return;
		}
	}

	/* If we get here the window wasn't already in the list. */
	selectList = ListCons(cli,selectList);
	cli->isSelected = True;

	/* Tell the window it is selected. */
	WinCallSelect(cli, True);
	SelectionTime = timestamp;
}


/*
 * RemoveSelection
 *
 * Remove a client from the PRIMARY selection list.  Returns True if client
 * was deselected; false if the client was not already selected.
 */
Bool
RemoveSelection(cli)
	Client *cli;
{
	List **l;

	for (l = &selectList ; *l != NULL; l = &((*l)->next))
	{
		if ((*l)->value == cli)
		{
			ListDestroyCell(l);
			cli->isSelected = False;
			WinCallSelect(cli,False);
			return True;
		}
	}
	return False;
}


/*
 * ToggleSelection
 *
 * Toggle a client's membership in the selection.  Returns a boolean 
 * indicating whether the client is a now member of the PRIMARY selection.
 */
Bool
ToggleSelection(cli, timestamp)
	Client *cli;
	Time timestamp;
{

	/* If already present, we want to deselect. */
	if (RemoveSelection(cli)) {
		return False;
	} else {
		AddSelection(cli, timestamp);
		return True;
	}
}


/*
 * ClearSelections
 *
 * Clear the list of clients on the PRIMARY selection.  This is called in
 * response to receiving a SelectionClear event or when olwm voluntarily gives
 * up the selection.  Note that this function does not relinquish the PRIMARY 
 * selection.  If that is desired, it is the responsibility of the caller.
 */
/*ARGSUSED*/
void
ClearSelections(dpy)
Display	*dpy;
{
	List *l;
	Client *cli;

	l = selectList;
	for(cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l))
	{
		cli->isSelected = False;
		WinCallSelect(cli,False);
	}
	ListDestroy(selectList);
	selectList = NULL_LIST;
}


/*
 * EnumSelections
 *
 * Enumerate the selected client structures.  Pass NULL to begin enumeration;
 * any non-NULL value thereafter will continue enumeration where it left off.
 * This function uses static data, so only one enumeration can happen at any
 * given time.  Returns NULL when list is exhausted.
 */
Client *
EnumSelections(foo)
	void *foo;
{
	static List *l;
	Client *ct;

	if (foo == NULL)
		l = selectList;

	if (l != NULL)
	{
		ct = l->value;
		l = l->next;
		return ct;
	}
	return NULL;
}


/*
 * SelectionRegister
 * 
 * Register a selection and its handler function.  The handler function should 
 * be declared as:
 * 
 *	Bool handler(selreqevent);
 *
 * Note that there is no way to unregister a selection.  That function isn't 
 * necessary at this time.
 */
void
SelectionRegister(selection, handler)
    Atom selection;
    Bool (*handler)();
{
    SelectionRegistry *reg;

    reg = MemNew(SelectionRegistry);
    reg->selection = selection;
    reg->handler = handler;
    selectRegistry = ListCons(reg, selectRegistry);
}


/*
 * SelectionResponse
 *
 * Handle a selection event.  Look up the selection in the selection registry 
 * and call the appropriate handler.  If there is match in the registry and 
 * the event is a SelectionRequest event, refuse the request.
 */
void
SelectionResponse(event)
    XEvent *event;
{
    SelectionRegistry *reg;
    List *l = selectRegistry;
    Atom selection;
    XSelectionEvent refusal;

    switch (event->type) {
    case SelectionClear:
	selection = event->xselectionclear.selection;
	break;
    case SelectionRequest:
	selection = event->xselectionrequest.selection;
	break;
    default:
	return;
    }

    for (reg = ListEnum(&l); reg != NULL; reg = ListEnum(&l)) {
	if (reg->selection == selection) {
	    (*(reg->handler))(event);
	    return;
	}
    }

    /*
     * We received an event for an unregistered selection.  Refuse any 
     * requests, and ignore SelectionClear events.
     */

    if (event->type == SelectionRequest) {
	refusal.type = SelectionNotify;
	refusal.requestor = event->xselectionrequest.requestor;
	refusal.selection = event->xselectionrequest.selection;
	refusal.time = event->xselectionrequest.time;
	refusal.target = event->xselectionrequest.target;
	refusal.property = None;

	XSendEvent(event->xany.display, refusal.requestor, False,
		   NoEventMask, (XEvent *) &refusal);
    }
}


/*
 * SelectionInit
 *
 * Register the handler for the PRIMARY selection.
 */
void
SelectionInit()
{
    SelectionRegister(XA_PRIMARY, handlePrimary);
}
