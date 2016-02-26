#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/object.c,v 1.1 93/01/06 03:27:43 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	object.c,v $
 * Revision 1.1  93/01/06  03:27:43  gounares
 * Initial revision
 * 
 */

/*
 * object.c
 * 
 * (see object.h for description)
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
#include <stdlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview/seln.h>
#include <xview/termsw.h>
#include <xview/notice.h>
#include "editor.h"
#include "file_io.h"
#include "object.h"
#include "state_server.h"
#include "apex_shell.h"
#include "file_browser.h"
#include "object_list.h"
#include "apex_options.h"

extern int      CLIENT_DATA_KEY;
static Server_image finish_image,
                create_image;

static void     text_proc();
static void     exec_proc();
static void     text_proc2();
static void     apply_proc();
static void     cancel_proc();
static void     switch_proc();

static char     szNone[] = "(NONE)";

void            update_editor_after_edit();

static char    *oc_help[] =
{
	"Object Link Creation",
	"B", "Overview",
	"b", "    Links join two arbitrary sections of text, with an",
	"b", "optional description.  Since the descriptions can be",
	"b", "passed to a shell and executed, you can attach arbitrary",
	"b", "objects to your text (such as movies, audio comments, etc.)",
	"b", " ",
	"B", "Link Head / Link Description  /Link Tail",
	"b", "   This toggle allows you to switch between the three",
	"b", "components of a link.  Link heads must be a section of text",
	"b", "within an apeX Editor.  The link description is optional.",
	"b", "If no link tail is specified, it defaults to the link head",
	"b", " ",
	"B", "Link File...",
	"b", "    The 'Link File...' button allows you to easily link to",
	"b", "an entire file.  Remember that link filenames are relative",
	"b", "for portability, so you usually do not want to link to a file",
	"b", "in another directory",
	0};

static void 
file_link_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OC             *poc = (OC *) xv_get(item, XV_KEY_DATA, CLIENT_DATA_KEY);
	FB             *pfb = (FB *) xv_get(item, PANEL_CLIENT_DATA);
	char            szBuf[1024];

	if (!poc->pOL1) {
		notice_prompt(pfb->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"No head selected for this link",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	sprintf(poc->pOL1->szLink_filename, "%s", (char *) xv_get(pfb->file_item,
			PANEL_VALUE));
	poc->pOL1->link_id = 0;

	sprintf(szBuf, "Linked to file '%s'", poc->pOL1->szLink_filename);
	poc->szTail = strdup(szBuf);
	poc->szTailfile = strdup(poc->szTail);
}

static void 
create_file_link_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OC             *poc = (OC *) xv_get(item, PANEL_CLIENT_DATA);
	FB             *pfb;

	pfb = create_generic_browser(poc->frame, NULL);

	xv_set(pfb->frame, FRAME_LABEL, "apeX File Link Control",
		XV_SHOW, TRUE,
		NULL);

	pfb->btn1 = (Panel_item) xv_create(pfb->panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Link to File",
		XV_X, 180,
		XV_Y, 135,
		PANEL_CLIENT_DATA, pfb,
		XV_KEY_DATA, CLIENT_DATA_KEY, poc,
		PANEL_NOTIFY_PROC, file_link_proc,
		NULL);

	xv_set(pfb->file_item, PANEL_NOTIFY_PROC, file_link_proc,
		XV_KEY_DATA, CLIENT_DATA_KEY, poc,
		NULL);

}
static void 
free_poc(poc)
    OC             *poc;
{
	free(poc->szHead);
	if (poc->szHeadfile != szNone)
		free(poc->szHeadfile);
	free(poc->szDesc);
	free(poc->szTail);
	if (poc->szTailfile != szNone)
		free(poc->szTailfile);
}

static void 
poc_done_proc(frame)
    Frame           frame;
{
	OC             *poc = (OC *) xv_get(frame, XV_KEY_DATA, CLIENT_DATA_KEY);

	free_poc(poc);

	xv_destroy_safe(frame);
}

/*
 * obj_run_proc -- send the linked object text to the shell
 */
void
obj_run_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped;
	File           *pFile;
	Object_list    *pOL;
	static char     newline[] = "\n";

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (!ped->pOL_current) {
		return;
	}

	/*
	 * if a description exists, use that to run things...this ought to be
	 * user-selectable in some fashion ultimately however
	 */

	if (ped->pOL_current->szDesc) {
		show_shell();
		ttysw_input(get_shell(), ped->pOL_current->szDesc,
			strlen(ped->pOL_current->szDesc));
		return;
	}
	if ((pFile = load_file(ped->pOL_current->szLink_filename)) == NULL) {
		char            szBuf[1024];

		sprintf(szBuf, "File '%s' does not exist or is not readable",
			ped->pOL_current->szLink_filename);
		notice_prompt(ped->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			szBuf, NULL,
			NULL);
	}
	for (pOL = pFile->pOL; pOL != NULL; pOL = pOL->next) {
		if (pOL->id == ped->pOL_current->link_id) {
			break;
		}
	}
	if (pOL) {
		show_shell();

		ttysw_input(get_shell(), pFile->szText + pOL->ibegin,
			pOL->iend - pOL->ibegin);
		if (*(pFile->szText + pOL->iend) != '\n') {
			ttysw_input(get_shell(), newline, 1);
		}
	}
}

/*
 * obj_show_proc -- will create an editor and show the link text
 */
void
obj_show_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *pedNew,
	               *ped;
	Object_list    *pOL;

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (!ped->pOL_current) {
		return;
	}
	pedNew = new_editor(ped->pOL_current->szLink_filename, 0, EDITOR_SMALL);

	if (pedNew->pFile == NULL)
		return;

	for (pOL = pedNew->pFile->pOL; pOL != NULL; pOL = pOL->next) {
		if (pOL->id == ped->pOL_current->link_id) {
			break;
		}
	}
	if (pOL) {
		printf("trying to set selection at %d-%d\n",
			pOL->ibegin, pOL->iend);
		textsw_normalize_view(pedNew->textsw,
			pOL->ibegin);
		textsw_set_selection(pedNew->textsw, pOL->ibegin,
			pOL->iend + 1, 1);
	}
}

/*
 * update_object_status -- change elements of the editor to reflect which
 * object is selected
 */
void
update_object_status(ped)
    Editor         *ped;
{
	char            szBuf[1024];

	if (ped->pOL_current)
		sprintf(szBuf, "Link to '%s': %.50s",
			ped->pOL_current->szLink_filename,
			(ped->pOL_current->szDesc) ? ped->pOL_current->szDesc :
			"(no description)");
	else
		sprintf(szBuf, "No Object Selected");

	xv_set(ped->frame, FRAME_LEFT_FOOTER, szBuf, NULL);

	/* if there is a rescan popup up, do a rescan to keep it current */

	if (ped->pOLP)
		rescan_olp(ped->pOLP);
}

/*
 * show_ol -- highlight an object within a textsw
 */
void
show_ol(ped, pOL)
    Editor         *ped;
    Object_list    *pOL;
{
	pOL->ibegin = textsw_find_mark(ped->textsw, pOL->begin);
	pOL->iend = textsw_find_mark(ped->textsw, pOL->end);

	textsw_set_selection(ped->textsw, pOL->ibegin, pOL->iend + 1, 2);
	textsw_possibly_normalize(ped->textsw, pOL->ibegin);

	ped->pOL_current = pOL;

	update_object_status(ped);
}

/*
 * obj_next_proc -- show the next object link
 */
void
obj_next_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped;
	Object_list    *pOL;

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (ped->pFile == NULL)
		return;

	if (ped->pOL_current == NULL) {
		/* we want to get the one at the end of the list */
		for (pOL = ped->pFile->pOL; pOL != NULL && pOL->next != NULL;
			pOL = pOL->next);
		ped->pOL_current = pOL;
	} else {
		ped->pOL_current = ped->pOL_current->prev;
	}
	if (ped->pOL_current)
		show_ol(ped, ped->pOL_current);
	else
		update_object_status(ped);
}

/*
 * obj_prev_proc -- show the previous object link
 */
void
obj_prev_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped;

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (ped->pFile == NULL)
		return;

	if (ped->pOL_current == NULL) {
		ped->pOL_current = ped->pFile->pOL;
	} else {
		ped->pOL_current = ped->pOL_current->next;
	}
	if (ped->pOL_current)
		show_ol(ped, ped->pOL_current);
	else
		update_object_status(ped);
}

static void     establish_link();

/*
 * create_proc -- called when the create button is pressed. When file-level
 * object linking is implemented, this may need to change drastically.
 */
void
obj_create_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped,
	              **rgped,
	              **rgped_head;
	static Object_list *pOL,
	               *pOL2;
	static int      fMode = 1;

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (fMode) {

		if ((pOL = get_block(ped)) != NULL) {
			printf("found block between %d & %d\n",
				pOL->ibegin, pOL->iend);
			fMode = 0;
			xv_set(ped->tb_create_button,
				PANEL_LABEL_IMAGE, ped->finish_image,
				NULL);
			xv_set(ped->frame,
				FRAME_LEFT_FOOTER,
				"Select text section to link to, press FINISH",
				NULL);
		}
	} else {
		rgped_head = rgped = get_editor_array();
		for (; *rgped != NULL; rgped++) {
			if ((pOL2 = get_block(*rgped)) != NULL) {
				printf("other block at %d & %d\n",
					pOL2->ibegin, pOL2->iend);
				fMode = 1;

				xv_set(ped->tb_create_button,
					PANEL_LABEL_IMAGE, ped->create_image,
					NULL);
				xv_set(ped->frame,
					FRAME_LEFT_FOOTER, " ",
					NULL);
				establish_link(ped, pOL, *rgped, pOL2);
				break;
			}
		}
		free(rgped_head);
	}

}

/*
 * delete_proc -- remove a link from the list of objects
 */
void
obj_delete_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);
	Object_list    *pol;
	int             answer;
	char            szBuf[1000];

	if (!(ped->pOL_current && ped->pFile))
		return;

	sprintf(szBuf, "Linked to '%s'", ped->pOL_current->szLink_filename);
	answer = notice_prompt(ped->frame, event,
		NOTICE_FOCUS_XY, event_x(event), event_y(event),
		NOTICE_MESSAGE_STRINGS,
		"Do you really wish to delete this link?",
		szBuf,
		(ped->pOL_current->szDesc) ? ped->pOL_current->szDesc :
		"(no description)",
		NULL,
		NOTICE_BUTTON_YES, "Cancel",
		NOTICE_BUTTON_NO, "Delete",
		NULL);

	if (answer != NOTICE_NO)
		return;

	if (ped->pOL_current->prev)
		ped->pOL_current->prev->next = ped->pOL_current->next;
	else
		/* pOL_current is the first one */
		ped->pFile->pOL = ped->pOL_current->next;

	free(ped->pOL_current->szDesc);
	free(ped->pOL_current);

	ped->pOL_current = NULL;

	update_object_status(ped);

	ped->fEdited = 1;
	update_editor_after_edit(ped);

}

void
obj_desc_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (!ped->pOL_current)
		return;

	if (!ped->pOL_current->szDesc) {
		notice_prompt(item, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"No description available for this link",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	set_sp_ped(ped->pOL_current->szDesc, ped);
}

/*
 * get_block -- given an editor structure, check to see if there is a primary
 * selection in the textsw.  If so, then return and Object_list struct with
 * the relevant information, otherwise return NULL
 */
Object_list    *
get_block(pEditor)
    Editor         *pEditor;
{
	static Xv_server server;
	Seln_holder     holder;
	Seln_request   *response;
	char           *ptr;
	Object_list    *pOL = NULL;

	if (!server) {
		server = (Xv_server) xv_get(
			xv_get(pEditor->frame, XV_SCREEN),
			SCREEN_SERVER);
	}
	holder = selection_inquire(server, SELN_PRIMARY);

	if (seln_holder_same_client(&holder, pEditor->textsw)) {
		/* selection was in the given editor */
		response = selection_ask(server, &holder,
			SELN_REQ_FIRST, NULL,
			SELN_REQ_LAST, NULL,
			NULL);

		if (response->status == SELN_SUCCESS) {
			pOL = (Object_list *) acalloc(1, sizeof(Object_list));

			ptr = response->data;

			pOL->ibegin = *(int *) (ptr += sizeof(SELN_REQ_FIRST));
			ptr += sizeof(int);
			pOL->iend = *(int *) (ptr += sizeof(SELN_REQ_LAST));

			pOL->begin = textsw_add_mark(pEditor->textsw,
				pOL->ibegin, TEXTSW_MARK_DEFAULTS);
			pOL->end = textsw_add_mark(pEditor->textsw,
				pOL->iend, TEXTSW_MARK_DEFAULTS);
		}
	}
	return pOL;
}

/*
 * establish_link -- given a two editors and two object link structures, fill
 * in the relevant information to link the two together.
 */
static void
establish_link(ped1, pol1, ped2, pol2)
    Editor         *ped1,
                   *ped2;
    Object_list    *pol1,
                   *pol2;
{
	Object_list    *pTemp;

	if (!ped1)
		return;
	if (ped2) {
		pol1->id = ++(ped1->pFile->cObjects);
		pol2->id = ++(ped2->pFile->cObjects);

		pol1->link_id = pol2->id;
		pol2->link_id = pol1->id;

		sprintf(pol1->szLink_filename, "%s", ped2->pFile->szFilename);
		sprintf(pol2->szLink_filename, "%s", ped1->pFile->szFilename);

		add_apex(pol1->szLink_filename);
		add_apex(pol2->szLink_filename);

		pTemp = ped1->pFile->pOL;
		ped1->pFile->pOL = pol1;
		pol1->next = pTemp;
		if (pol1->next) {
			pol1->next->prev = pol1;
		}
		pol1->prev = NULL;
		ped1->fEdited = 1;

		pTemp = ped2->pFile->pOL;
		ped2->pFile->pOL = pol2;
		pol2->next = pTemp;
		if (pol2->next) {
			pol2->next->prev = pol2;
		}
		pol2->prev = NULL;
		ped2->fEdited = 1;
	} else {
		pol1->id = ++(ped1->pFile->cObjects);
		pol1->link_id = pol1->id;
		if (pol1->szLink_filename[0] == '\0') {
			sprintf(pol1->szLink_filename, "%s", ped1->pFile->szFilename);
			add_apex(pol1->szLink_filename);
		}
		pTemp = ped1->pFile->pOL;
		ped1->pFile->pOL = pol1;
		pol1->next = pTemp;
		if (pol1->next) {
			pol1->next->prev = pol1;
		}
		pol1->prev = NULL;
		ped1->fEdited = 1;
	}

	update_editor_after_edit(ped1);
	rescan_olp(ped1->pOLP);
	if (ped2) {
		update_editor_after_edit(ped2);
		rescan_olp(ped2->pOLP);
	}
}

static Menu     null_menu;

/*
 * create_oc -- create an object_link create control center
 */
OC             *
create_oc(ped)
    Editor         *ped;
{
	OC             *poc;
	char            szBuf[1024];
	int             width;

	if (!null_menu)
		null_menu = (Menu) xv_create(NULL, MENU,
			NULL);

	poc = (OC *) acalloc(1, sizeof(OC));

	poc->ped = ped;

	sprintf(szBuf, "Object Link Create for Editor #%d", ped->id);

	poc->frame = (Frame) xv_create(ped->frame, FRAME_CMD,
		FRAME_LABEL, szBuf,
		FRAME_DONE_PROC, poc_done_proc,
		XV_KEY_DATA, CLIENT_DATA_KEY, poc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);

	poc->file_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
		MENU_STRING, "Link to File...",
		NULL,
		NULL);

	poc->panel = (Panel) xv_get(poc->frame, FRAME_CMD_PANEL);

	xv_set(poc->panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

	poc->section_choice = (Panel_item) xv_create(poc->panel, PANEL_CHOICE,
		PANEL_CHOICE_STRINGS, "Link Head",
		"Link Description",
		"Link Tail",
		NULL,
		PANEL_VALUE, 1,
		PANEL_CLIENT_DATA, poc,
		PANEL_NOTIFY_PROC, switch_proc,
		NULL);

	poc->fState = 1;
	poc->szHeadfile = szNone;
	poc->szTailfile = szNone;

	poc->exec_choice = (Panel_item) xv_create(poc->panel, PANEL_CHECK_BOX,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOICE_STRINGS,
		"Executable Link",
		NULL,
		PANEL_CLIENT_DATA, poc,
		PANEL_ITEM_X, 350,
		XV_SHOW, FALSE,	       /* we don't use this feature currently */
		NULL);

	xv_set(poc->panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	poc->file_button = (Panel_item) xv_create(poc->panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Link File...",
		PANEL_CLIENT_DATA, poc,
	/* PANEL_ITEM_MENU, poc->file_menu, */
		PANEL_NOTIFY_PROC, create_file_link_proc,
		NULL);

	xv_set(poc->panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

	poc->file_label = (Panel_item) xv_create(poc->panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "(NONE)",
		XV_SHOW, FALSE,
		NULL);


	poc->text_button = (Panel_item) xv_create(poc->panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Use Highlighted Text",
		PANEL_ITEM_X, 350,
		PANEL_CLIENT_DATA, poc,
		PANEL_NOTIFY_PROC, text_proc,
		PANEL_INACTIVE, TRUE,
		NULL);

	window_fit_height(poc->panel);

	poc->text = (Textsw) xv_create(poc->frame, TEXTSW,
		WIN_BELOW, poc->panel,
		TEXTSW_TAB_WIDTH, get_tabsize(),
		WIN_COLUMNS, 80,
		WIN_ROWS, 10,
		XV_KEY_DATA, CLIENT_DATA_KEY, poc,
		NULL);

	poc->textsw_menu = (Menu) xv_get(poc->text, TEXTSW_MENU);

	poc->panel2 = (Panel) xv_create(poc->frame, PANEL,
		XV_X, 0,
		WIN_BELOW, poc->text,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	poc->apply_button = (Panel_item) xv_create(poc->panel2, PANEL_BUTTON,
		XV_X, 250,
		PANEL_LABEL_STRING, "Apply",
		PANEL_CLIENT_DATA, poc,
		PANEL_NOTIFY_PROC, apply_proc,
		NULL);

	poc->cancel_button = (Panel_item) xv_create(poc->panel2, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Cancel",
		PANEL_CLIENT_DATA, poc,
		PANEL_NOTIFY_PROC, cancel_proc,
		NULL);

	window_fit_height(poc->panel2);

	/*
	 * this is and ugly heinous hack to get the panels to size
	 * properly...at some leisurely point in time fix it (yeah right)
	 */

	width = (int) xv_get(poc->text, XV_WIDTH);

	xv_set(poc->panel, XV_WIDTH, width, NULL);
	xv_set(poc->panel2, XV_WIDTH, width, NULL);

	set_help_button(poc->frame, poc->panel, oc_help);

	window_fit(poc->frame);

	return poc;

}

/*
 * show_oc -- called when the create button is pushed...through an oc onto
 * the screen
 */
void
show_oc(item, event)
    Panel_item      item;
    Event          *event;
{
	Editor         *ped;
	OC             *poc;

	ped = (Editor *) xv_get(item, PANEL_CLIENT_DATA);

	if (!ped->pFile || ped->pFile->szFilename[0] == '\0') {
		notice_prompt(ped->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"There is no file associated with this text, so you",
			"cannot create a link yet.",
			"First save the file using the 'Save As' menu choice",
			"under the 'File' button and then try this operation again",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	poc = create_oc(ped);

	/* now check to see if there was text highlighted to begin with */

	if ((poc->pOL1 = get_block(ped)) != NULL) {
		char            szBuf[1024];
		int             size;

		sprintf(szBuf, "From file '%s' in Editor %d",
			ped->szFilename, ped->id);
		poc->szHeadfile = strdup(szBuf);
		size = poc->pOL1->iend - poc->pOL1->ibegin + 2;
		poc->szHead = (char *) acalloc(size, sizeof(char));
		xv_get(ped->textsw, TEXTSW_CONTENTS, poc->pOL1->ibegin,
			poc->szHead, size - 1);
		poc->ped1 = ped;
	}
	xv_set(poc->frame, XV_SHOW, TRUE, NULL);
}

/*
 * text_proc1 -- called to grab and enter the text of a highlight
 */
static void 
text_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OC             *poc;
	Editor        **rgped,
	              **rgped_head,
	              **pped;
	char            szBuf[1024],
	               *szTemp;
	int             size;
	Object_list   **ppol;
	char          **pszLabel;

	poc = (OC *) xv_get(item, PANEL_CLIENT_DATA);

	rgped_head = rgped = get_editor_array();

	switch (poc->fState) {
	case 0:
		ppol = &(poc->pOL1);
		pped = &(poc->ped1);
		pszLabel = &(poc->szHeadfile);
		break;
	case 2:
		ppol = &(poc->pOL2);
		pped = &(poc->ped2);
		pszLabel = &(poc->szTailfile);
		break;
	default:
		return;
	}

	for (; *rgped != NULL; rgped++) {
		if ((*ppol = get_block(*rgped)) != NULL) {
			if ((*rgped)->pFile == NULL ||
				(*rgped)->pFile->szFilename[0] == '\0')
				notice_prompt(poc->frame, event,
					NOTICE_FOCUS_XY, event_x(event), event_y(event),
					NOTICE_MESSAGE_STRINGS,
					"There is no file loaded into that editor",
					"You can only link text between two files",
					NULL,
					NOTICE_BUTTON_YES, "OK",
					NULL);

			sprintf(szBuf, "From file '%s' in Editor #%d",
				(*rgped)->szFilename, (*rgped)->id);
			if (*pszLabel != szNone)
				free(*pszLabel);
			*pszLabel = strdup(szBuf);
			xv_set(poc->file_label, PANEL_LABEL_STRING, szBuf, NULL);
			size = (*ppol)->iend - (*ppol)->ibegin + 2;
			szTemp = (char *) acalloc(size, sizeof(char));
			xv_get((*rgped)->textsw, TEXTSW_CONTENTS, (*ppol)->ibegin,
				szTemp, size - 1);
			textsw_reset(poc->text, 0, 0);
			xv_set(poc->text, TEXTSW_CONTENTS, szTemp, NULL);
			xv_set(poc->text, TEXTSW_FIRST, 0, NULL);
			*pped = (*rgped);
			free(szTemp);
			break;
		}
	}

	if (*rgped == NULL) {
		int             answer = notice_prompt(poc->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"Highlighted text not in an apeX editor",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
	}
	free(rgped_head);
}


/*
 * switch_proc -- called when one the link display mode choice is selected
 */
static void 
switch_proc(item, value, event)
    Panel_item      item;
    int             value;
    Event          *event;
{
	OC             *poc;
	int             size;

	poc = (OC *) xv_get(item, PANEL_CLIENT_DATA);

	/* first replace the old text */

	size = (int) xv_get(poc->text, TEXTSW_LENGTH);
	switch (poc->fState) {
	case 0:
		free(poc->szHead);
		poc->szHead = (char *) acalloc(size + 1, sizeof(char));
		xv_get(poc->text, TEXTSW_CONTENTS, 0, poc->szHead, size);
		break;
	case 1:
		free(poc->szDesc);
		poc->szDesc = (char *) acalloc(size + 1, sizeof(char));
		xv_get(poc->text, TEXTSW_CONTENTS, 0, poc->szDesc, size);
		break;
	case 2:
		free(poc->szTail);
		poc->szTail = (char *) acalloc(size + 1, sizeof(char));
		xv_get(poc->text, TEXTSW_CONTENTS, 0, poc->szTail, size);
		break;
	}

	textsw_reset(poc->text);

	switch (value) {
	case 0:
		xv_set(poc->file_label, PANEL_LABEL_STRING, poc->szHeadfile, NULL);
		xv_set(poc->file_label, XV_SHOW, TRUE, NULL);
		xv_set(poc->text_button, PANEL_INACTIVE, FALSE, NULL);

		if (poc->szHead)
			xv_set(poc->text, TEXTSW_CONTENTS, poc->szHead,
				TEXTSW_READ_ONLY, TRUE, NULL);
		xv_set(poc->text, TEXTSW_MENU, null_menu, NULL);
		break;
	case 1:
		xv_set(poc->file_label, XV_SHOW, FALSE, NULL);
		xv_set(poc->text_button, PANEL_INACTIVE, TRUE, NULL);

		if (poc->szDesc)
			xv_set(poc->text, TEXTSW_CONTENTS, poc->szDesc,
				TEXTSW_READ_ONLY, FALSE, NULL);
		xv_set(poc->text, TEXTSW_MENU, poc->textsw_menu, NULL);
		break;
	case 2:
		xv_set(poc->file_label, PANEL_LABEL_STRING, poc->szTailfile, NULL);
		xv_set(poc->file_label, XV_SHOW, TRUE, NULL);
		xv_set(poc->text_button, PANEL_INACTIVE, FALSE, NULL);

		if (poc->szTail)
			xv_set(poc->text, TEXTSW_CONTENTS, poc->szTail,
				TEXTSW_READ_ONLY, TRUE, NULL);
		xv_set(poc->text, TEXTSW_MENU, null_menu, NULL);
		break;
	}

	xv_set(poc->text, TEXTSW_FIRST, 0, NULL);

	poc->fState = value;
}

/*
 * apply_proc -- actually build the link
 */
static void
apply_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OC             *poc;

	poc = (OC *) xv_get(item, PANEL_CLIENT_DATA);

	/*
	 * the only time it's relevant to get the text at this point is if
	 * the current state is on the link description.  This way, we get
	 * the latest edits
	 */

	if (poc->fState == 1) {
		int             size = (int) xv_get(poc->text, TEXTSW_LENGTH);

		free(poc->szDesc);
		poc->szDesc = (char *) acalloc(size + 1, sizeof(char));
		xv_get(poc->text, TEXTSW_CONTENTS, 0, poc->szDesc, size);
	}
	if (!(poc->ped1 && poc->pOL1)) {
		notice_prompt(poc->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
			"You have not selected a head for this link",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	poc->pOL1->szDesc = strdup(poc->szDesc);

	establish_link(poc->ped1, poc->pOL1, poc->ped2, poc->pOL2);

	textsw_reset(poc->text, 0, 0);
	xv_set(poc->file_label, XV_SHOW, FALSE, NULL);

	free_poc(poc);

	poc->szHeadfile = poc->szTailfile = szNone;

	poc->szHead = poc->szTail = poc->szDesc = NULL;

	poc->ped1 = poc->ped2 = NULL;
	poc->pOL1 = poc->pOL2 = NULL;
}

/*
 * cancel_proc --called when the cancel button is pushed
 */
static void
cancel_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	OC             *poc;

	poc = (OC *) xv_get(item, PANEL_CLIENT_DATA);

	xv_set(poc->frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);

	free_poc(poc);

	textsw_reset(poc->text, 0, 0);
	xv_destroy_safe(poc->frame);
}
