#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/object_list.c,v 1.1 93/01/06 03:27:46 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	object_list.c,v $
 * Revision 1.1  93/01/06  03:27:46  gounares
 * Initial revision
 * 
 */

/*
 * object_list.c
 * 
 * Routines for a popup listing all available links.
 * 
 * written by Alexander Gounares 11/7/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>
 GNU General Public License Version 2
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>

 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/notice.h>

#include "editor.h"
#include "object_list.h"
#include "object.h"
#include "file_io.h"

extern int      CLIENT_DATA_KEY;

static char    *olp_help[] =
{
	"Object Link Lists",
	"B", "Show first link",
	"b", "    The 'Show first link' button will display the first link",
	"b", "in highlighted text.  If no text is highlighted, then the",
	"b", "first link in the currently visible text is displayed.",
	"b", " ",
	"B", "Linked Objects",
	"b", "    This list shows all of the links in the current document,",
	"b", "with the first line of the link description (if any).",
	"b", "Clicking on a link will display (underline) the link in the",
	"b", "editor window.",
	0};

/*
 * find_first_proc -- find the first object in either the selected text or
 * (if no text selected) the current field of view in the textsw
 */
static void
find_first_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	int             first,
	                last,
	                close;
	OLP            *polp = (OLP *) xv_get(item, PANEL_CLIENT_DATA);
	Object_list    *pol = get_block(polp->ped),
	               *pol_first = NULL;

	if (!pol) {
		textsw_file_lines_visible(polp->ped->textsw, &first, &last);
		first = textsw_index_for_file_line(polp->ped->textsw, first);
		last = textsw_index_for_file_line(polp->ped->textsw, last);
	} else {
		first = pol->ibegin;
		last = pol->iend;
	}

	/*
	 * now go through the list to check for the first link to be visible.
	 * Since the user may have added extra links, we have no guarantee on
	 * the order
	 */

	if (!polp->ped->pFile)
		return;

	if (!polp->ped->pFile->pOL) {
		char            szBuf[2024];

		sprintf(szBuf, "There are no links in file '%s' in Editor #%d",
			polp->ped->pFile->szFilename, polp->ped->id);
		notice_prompt(polp->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			szBuf,
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	close = last;

	for (pol = polp->ped->pFile->pOL; pol; pol = pol->next) {
		if (pol->ibegin >= first && pol->ibegin <= last) {

			/*
			 * this assumes that the last of equal matches is
			 * 'first'.  Remember that the link list is stored
			 * backwards with as far as the next and previous
			 * buttons are concerned
			 */
			if (pol->ibegin <= close) {
				pol_first = pol;
				close = pol->ibegin;
			}
		}
	}

	if (!pol_first) {
		notice_prompt(polp->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"No link found within the selected area",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	show_ol(polp->ped, pol_first);
}

/*
 * create_olp_proc -- create an object list popup.  This popup just contains
 * a list of all links in a file
 */
void
create_olp_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OLP            *pOLP;
	char            szBuf[100];
	Editor         *ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (ped->pOLP) {
		pOLP = (OLP *) ped->pOLP;
		rescan_olp(pOLP);
		xv_set(pOLP->frame, XV_SHOW, TRUE, NULL);
		return;
	}
	pOLP = (OLP *) acalloc(1, sizeof(OLP));
	pOLP->ped = ped;
	ped->pOLP = (void *) pOLP;

	sprintf(szBuf, "Links for Editor %d, file '%s'", pOLP->ped->id,
		(pOLP->ped->pFile) ? pOLP->ped->pFile->szFilename : "(NONE)");
	pOLP->frame = (Frame) xv_create(pOLP->ped->frame, FRAME_CMD,
		FRAME_LABEL, szBuf,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		XV_WIDTH, 300,
		NULL);

	pOLP->panel = (Panel) xv_get(pOLP->frame, FRAME_CMD_PANEL);
	xv_set(pOLP->panel, PANEL_LAYOUT, PANEL_VERTICAL,
		XV_WIDTH, 300,
		NULL);

	pOLP->first_button = (Panel_item) xv_create(pOLP->panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Show first link",
		PANEL_CLIENT_DATA, pOLP,
		PANEL_NOTIFY_PROC, find_first_proc,
		NULL);

	pOLP->list = (Panel_item) xv_create(pOLP->panel, PANEL_LIST,
		PANEL_LABEL_STRING, "Linked Objects:",
		PANEL_LAYOUT, PANEL_VERTICAL,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		PANEL_LIST_DISPLAY_ROWS, 7,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, olp_list_proc,
		PANEL_CLIENT_DATA, pOLP,
		NULL);

	/* now go ahead and put in items */
	rescan_olp(pOLP);

	window_fit_height(pOLP->panel);
	window_fit_height(pOLP->frame);

	set_help_button(pOLP->frame, pOLP->panel, olp_help);

	xv_set(pOLP->frame, XV_SHOW, TRUE, NULL);
}


/*
 * rescan_olp -- called when the rescan button is pushed
 */
void
rescan_olp(pOLP)
    OLP            *pOLP;
{
	char            szBuf[1024],
	                szBuf2[1024];
	Object_list    *pOL;
	int             nrows,
	                i,
	                width;

	if (!pOLP)
		return;

	nrows = (int) xv_get(pOLP->list, PANEL_LIST_NROWS);
	xv_set(pOLP->list, PANEL_LIST_DELETE_ROWS, 0, nrows, NULL);

	if (!pOLP->ped || !pOLP->ped->pFile)
		return;


	for (pOL = pOLP->ped->pFile->pOL; pOL != NULL; pOL = pOL->next) {
		pOL->ibegin = textsw_find_mark(pOLP->ped->textsw, pOL->begin);
		pOL->iend = textsw_find_mark(pOLP->ped->textsw, pOL->end);
		if (pOL->szDesc) {
			for (i = 0; i < 1000 && pOL->szDesc[i] != '\0' &&
				pOL->szDesc[i] != '\n'; i++)
				szBuf2[i] = pOL->szDesc[i];
			szBuf2[i] = '\0';
		} else
			strcpy(szBuf2, "(no description)");

		sprintf(szBuf, "Link to '%s': %.200s", pOL->szLink_filename, szBuf2);

		xv_set(pOLP->list, PANEL_LIST_INSERT, 0,
			PANEL_LIST_STRING, 0, szBuf,
			PANEL_LIST_CLIENT_DATA, 0, pOL,
			NULL);
	}

	/* resize this thing so it looks better :) */

	width = (int) xv_get(pOLP->list, XV_WIDTH);
	width = (width > 295) ? width + 5 : 300;

	xv_set(pOLP->frame, XV_WIDTH, width, NULL);
}

/*
 * olp_list_proc -- called when one of the items in the object list is pushed
 */
void
olp_list_proc(item, string, client_data, op, event)
    Panel_item      item;
    char           *string;
    caddr_t         client_data;
    Panel_list_op   op;
    Event          *event;
{
	OLP            *pOLP;

	pOLP = (OLP *) xv_get(item, PANEL_CLIENT_DATA);

	if (op == PANEL_LIST_OP_SELECT) {
		show_ol(pOLP->ped, (Object_list *) client_data);
	}
}
