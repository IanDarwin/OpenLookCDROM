/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)selection.c 1.18 92/10/22";
#endif
#endif

#include <xview/sel_pkg.h>
#include <sspkg/rectobj.h>
#include "rectobj_impl.h"
#include <assert.h>

static	Rectobj_list	*selected_list;
static	int		num_selected;
static	Selection_owner	curr_owner;

#define call_selection_proc(rectobj, add, event)			\
		/*  Rectobj rectobj; int add; */			\
		{							\
		Proc_ptr	p;					\
		p = (Proc_ptr) xv_get(rectobj, RECTOBJ_SELECTION_PROC);	\
		if(p)							\
			(p)(rectobj, add, event);       		\
		}

Rectobj_list *
rectobj_get_selected_list()
{
	return(selected_list);
}


static void
rectobj_selection_own(sel_owner, event, own)
	Selection_owner	sel_owner;
	Event		*event;
	int		own;
{
	if(sel_owner) {
		/*
		 * Don't release if already released, or if already owned.
		 *
		 * Beware of recursion problem with selection lose proc, 
		 * which is called before SEL_OWN is set to TRUE.
		 */
		if(xv_get(sel_owner, SEL_OWN) == own) {
			return;
		}

		/*
		 * printf("%s selection\n", (own?"owning":"disowning"));
		 */
		if(event) {
			xv_set(sel_owner,
				SEL_OWN, own,
				SEL_TIME, &event_time(event),
				NULL);
		} else {
			xv_set(sel_owner,
				SEL_OWN, own,
				NULL);
		}
	}
}


static void
clear_selected(new, event)
	Rectobj	new;
	Event	*event;
{
	/* clear all but new from selected list */
	Rectobj		tmp;

	while(selected_list) {
		tmp = RECTOBJ_LIST_HANDLE(selected_list);
		selected_list = (Rectobj_list*) list_first(
				list_delete_node(selected_list));
		if(tmp != new) {
			rectobj_set_paint_style(tmp, event, RECTOBJ_NORMAL);
			num_selected--;
			RF_UNSET(RECTOBJ_PRIVATE(tmp), RF_SELECTED);
			call_selection_proc(tmp, FALSE, event);
		}
	}
}


void
rectobj_add_to_selected_list(new, exclusive, event)
	Rectobj         new;
	int             exclusive;
	Event		*event;
{
	/*
	 * To clear a selection use:
	 * 	rectobj_add_to_selected_list(NULL, TRUE, event);
	 */

	Rectobj_list   *new_node;
	Rectobj_info   *rinfo;
	Selection_owner sel_owner;
	int		previously_selected;

	if(new) {
		rinfo = RECTOBJ_PRIVATE(new);

		if(!RF_IS_SET(rinfo, RF_SELECTABLE))
			return;

		previously_selected = RF_IS_SET(rinfo, RF_SELECTED);
	}

	if(exclusive) {
		clear_selected(new, event);
		if(new && rinfo->parent &&
                   xv_get(rinfo->parent, RECTOBJ_RESTACK_CHILDREN))
			rectobj_set_stacking_position(new, 0x7FFFFFFF);
	}

	if(new) {
		if(!previously_selected) {

		    new_node = xv_alloc(Rectobj_list);
		    new_node->handle = (void *) new;
		    selected_list = (Rectobj_list *)
			list_concat(selected_list, new_node);
		    call_selection_proc(new, TRUE, event);

		   /*
		    * The object was not previously selected, own the
		    * selection and repaint in new state.
		    */
		    if(rectobj_upsearch(new, &sel_owner, 
				RECTOBJ_SELECTION_OWNER)) {
			/*
			 * Own the selection if application has created and
			 * attached a selection owner, and the previously
			 * selected list was empty.
			 */
			rectobj_selection_own(sel_owner, event, TRUE);
			curr_owner = sel_owner;
		    }
		    RF_SET(rinfo, RF_SELECTED);
		    num_selected++;
		}
	}

	if(num_selected == 0) {
		rectobj_selection_own(curr_owner, event, FALSE);
		curr_owner = NULL;
	}
}


void
rectobj_lose_selection()
{
	/*
	 * Intended to be called at a SEL_CLEAR event on the canvas window.
	 * This does not disown selection because the selection package is 
	 * supposed to do this before it gets here.
	 */
	curr_owner = NULL;
	clear_selected(NULL, NULL);
}


void
rectobj_del_from_selected_list(rectobj, event)
	Rectobj rectobj;
	Event	*event;
{
	Rectobj_list *node;
	Rectobj_info *rinfo = RECTOBJ_PRIVATE(rectobj);

	if(RF_IS_SET(rinfo, RF_SELECTED)) {
		RF_UNSET(rinfo, RF_SELECTED);
		if(node = list_find(selected_list, (void*)rectobj)) {
			selected_list = (Rectobj_list*) 
				list_first( list_delete_node(node) );
			num_selected--;
			call_selection_proc(rectobj, FALSE, event);
		}
		/*assert(node); should never be RF_SELECTED and not on list */
	}

	if(num_selected == 0) {
		rectobj_selection_own(curr_owner, event, FALSE);
		curr_owner = NULL;
	}
}




