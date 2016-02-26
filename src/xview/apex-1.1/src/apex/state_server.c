#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/state_server.c,v 1.1 93/01/06 03:27:51 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	state_server.c,v $
 * Revision 1.1  93/01/06  03:27:51  gounares
 * Initial revision
 * 
 */

/*
 * state_server.c
 * 
 * Maintain important state information across the whole apeX environment.
 * 
 * written by Alexander Gounares 9/27/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <xview/termsw.h>
#include <xview/canvas.h>
#include "editor.h"
#include "state_server.h"
#include "apex_shell.h"
#include "navigator.h"

typedef struct _Editor_list {
	Editor         *ped;
	NG             *png;
	struct _Editor_list *next;
}               Editor_list;

static Editor_list *pged_list = NULL;
static int      cgEditor = 0;

/*
 * add_editor -- add the new editor structure to the state.
 */
void
add_editor(ped)
    Editor         *ped;
{
	Editor_list    *ped_list;

	ped_list = (Editor_list *) acalloc(1, sizeof(Editor_list));

	ped_list->ped = ped;
	ped_list->next = pged_list;
	pged_list = ped_list;
}

/*
 * add_navigator -- add a new navigator structure to the state
 */
void
add_navigator(png)
    NG             *png;
{
	Editor_list    *ped_list;

	ped_list = (Editor_list *) acalloc(1, sizeof(Editor_list));

	ped_list->png = png;
	ped_list->next = pged_list;
	pged_list = ped_list;
}

/*
 * remove_editor -- remove an editor structure from the state
 */
void
remove_editor(ped)
    Editor         *ped;
{
	Editor_list    *pedl,
	              **ppedl;

	ppedl = &pged_list;

	for (pedl = pged_list; pedl != NULL; ppedl = &(pedl->next),
		pedl = pedl->next) {
		if (pedl->ped == ped) {
			*ppedl = pedl->next;
			free(pedl->ped);
			free(pedl);
			return;
		}
	}
	printf("editor not found\n");
	return;
}

/*
 * remove_navigator -- remove a navigator structure from the state
 */
void
remove_navigator(png)
    NG             *png;
{
	Editor_list    *pedl,
	              **ppedl;
	int             i;

	ppedl = &pged_list;

	for (pedl = pged_list; pedl != NULL; ppedl = &(pedl->next),
		pedl = pedl->next) {
		if (pedl->png == png) {
			*ppedl = pedl->next;
			free(png);
			free(pedl);
			return;
		}
	}
	printf("editor not found\n");
	return;
}

/*
 * remove_editor_by_client -- remove an editor structure by the frame value
 */
void
remove_editor_by_client(client)
    Notify_client   client;
{
	Editor_list    *pedl,
	              **ppedl;

	ppedl = &pged_list;

	for (pedl = pged_list; pedl != NULL; ppedl = &(pedl->next),
		pedl = pedl->next) {
		if (pedl->ped && (int) pedl->ped->frame == (int) client) {
			*ppedl = pedl->next;
			free(pedl->ped);
			free(pedl);
			return;
		} else if (pedl->png   /* && (int)pedl->png->frame ==
			    (int)client */ ) {
			/* remove_navigator(pedl->png); */
		}
	}

	printf("editor not found\n");
	return;
}

/*
 * get_editor_by_client -- return an editor structure by the frame value
 */
Editor         *
get_editor_by_client(client)
    Notify_client   client;
{
	Editor_list    *pedl,
	              **ppedl;

	ppedl = &pged_list;

	for (pedl = pged_list; pedl != NULL; ppedl = &(pedl->next),
		pedl = pedl->next) {
		if (pedl->ped && (int) pedl->ped->frame == (int) client)
			return pedl->ped;
	}

	return NULL;
}

/*
 * fLast_Editor -- returns 1 if there is only one editor visible, 0 otherwise
 */
int
fLast_Editor()
{
	return (pged_list->next) ? 0 : 1;
}

/*
 * get_next_editor_id -- return a unique small integer for the editor
 */
int
get_next_editor_id()
{
	return ++cgEditor;
}

/*
 * get_editor_array -- return a NULL-terminated array of all the editors
 * currently known to the state server.
 */
Editor        **
get_editor_array()
{
	int             count = 0;
	Editor        **rgped;
	Editor_list    *pedl;

	for (pedl = pged_list; pedl != NULL; pedl = pedl->next) {
		if (pedl->ped)
			count++;
	}

	rgped = (Editor **) acalloc(count + 1, sizeof(Editor *));

	count = 0;
	for (pedl = pged_list; pedl != NULL; pedl = pedl->next) {
		if (pedl->ped)
			rgped[count++] = pedl->ped;
	}

	return rgped;
}

/*
 * find_navigator -- given a canvas id, find the navigator structure, else
 * return  NULL
 */
NG             *
find_navigator(canvas)
    Canvas          canvas;
{
	Editor_list    *pedl;

	/*
	 * for (pedl = pged_list; pedl != NULL; pedl = pedl->next) if
	 * (pedl->png && pedl->png->canvas == canvas) return pedl->png;
	 */

	return NULL;
}
