/*
 * olxvman_stubs.c - Notify and event callback function stubs.
 * This file was generated by `gxv' from `olxvman.G'.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include "olxvman_ui.h"


/*
 * Global object definitions.
 */
olxvman_OLXVMan_objects	*Olxvman_OLXVMan;
olxvman_SearchPopup_objects	*Olxvman_SearchPopup;
olxvman_ManualPagePopup_objects	*Olxvman_ManualPagePopup;

#ifdef MAIN

/*
 * Instance XV_KEY_DATA key.  An instance is a set of related
 * user interface objects.  A pointer to an object's instance
 * is stored under this key in every object.  This must be a
 * global variable.
 */
Attr_attribute	INSTANCE;

main(int argc, char **argv)
{
	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	INSTANCE = xv_unique_key();
	
	/*
	 * Initialize user interface components.
	 * Do NOT edit the object initializations by hand.
	 */
	Olxvman_OLXVMan = olxvman_OLXVMan_objects_initialize(NULL, NULL);
	Olxvman_SearchPopup = olxvman_SearchPopup_objects_initialize(NULL, Olxvman_OLXVMan->OLXVMan);
	Olxvman_ManualPagePopup = olxvman_ManualPagePopup_objects_initialize(NULL, Olxvman_OLXVMan->OLXVMan);
	
	
	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(Olxvman_OLXVMan->OLXVMan);
	exit(0);
}

#endif


/*
 * Menu handler for `OptionMenu (Search...)'.
 */
Menu_item
SearchHandler(Menu_item item, Menu_generate op)
{
	olxvman_OLXVMan_objects * ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		fputs("olxvman: SearchHandler: MENU_NOTIFY\n", stderr);
		
		/* gxv_start_connections DO NOT EDIT THIS SECTION */

		/* gxv_end_connections */

		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `OptionMenu (Show Version)'.
 */
Menu_item
ShowVersionHandler(Menu_item item, Menu_generate op)
{
	olxvman_OLXVMan_objects * ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		fputs("olxvman: ShowVersionHandler: MENU_NOTIFY\n", stderr);
		
		/* gxv_start_connections DO NOT EDIT THIS SECTION */

		/* gxv_end_connections */

		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `OptionMenu (Print Current Page)'.
 */
Menu_item
PrintHandler(Menu_item item, Menu_generate op)
{
	olxvman_OLXVMan_objects * ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		fputs("olxvman: PrintHandler: MENU_NOTIFY\n", stderr);
		
		/* gxv_start_connections DO NOT EDIT THIS SECTION */

		/* gxv_end_connections */

		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `OptionMenu (Quit)'.
 */
Menu_item
QuitHandler(Menu_item item, Menu_generate op)
{
	olxvman_OLXVMan_objects * ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		fputs("olxvman: QuitHandler: MENU_NOTIFY\n", stderr);
		
		/* gxv_start_connections DO NOT EDIT THIS SECTION */

		/* gxv_end_connections */

		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Notify callback function for `ManualSections'.
 */
int
ManualSectionNotify(Panel_item item, char *string, Xv_opaque client_data, Panel_list_op op, Event *event, int row)
{
	olxvman_OLXVMan_objects *ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		fprintf(stderr, "olxvman: ManualSectionNotify: PANEL_LIST_OP_DESELECT: %s\n",string);
		break;

	case PANEL_LIST_OP_SELECT:
		fprintf(stderr, "olxvman: ManualSectionNotify: PANEL_LIST_OP_SELECT: %s\n",string);
		break;

	case PANEL_LIST_OP_VALIDATE:
		fprintf(stderr, "olxvman: ManualSectionNotify: PANEL_LIST_OP_VALIDATE: %s\n",string);
		break;

	case PANEL_LIST_OP_DELETE:
		fprintf(stderr, "olxvman: ManualSectionNotify: PANEL_LIST_OP_DELETE: %s\n",string);
		break;
	}
	
	/* gxv_start_connections DO NOT EDIT THIS SECTION */

	/* gxv_end_connections */

	return XV_OK;
}

/*
 * Notify callback function for `ManualPages'.
 */
int
ManualPageNotify(Panel_item item, char *string, Xv_opaque client_data, Panel_list_op op, Event *event, int row)
{
	olxvman_OLXVMan_objects *ip = (olxvman_OLXVMan_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		fprintf(stderr, "olxvman: ManualPageNotify: PANEL_LIST_OP_DESELECT: %s\n",string);
		break;

	case PANEL_LIST_OP_SELECT:
		fprintf(stderr, "olxvman: ManualPageNotify: PANEL_LIST_OP_SELECT: %s\n",string);
		break;

	case PANEL_LIST_OP_VALIDATE:
		fprintf(stderr, "olxvman: ManualPageNotify: PANEL_LIST_OP_VALIDATE: %s\n",string);
		break;

	case PANEL_LIST_OP_DELETE:
		fprintf(stderr, "olxvman: ManualPageNotify: PANEL_LIST_OP_DELETE: %s\n",string);
		break;
	}
	
	/* gxv_start_connections DO NOT EDIT THIS SECTION */

	/* gxv_end_connections */

	return XV_OK;
}

/*
 * Notify callback function for `ManualButton'.
 */
void
SearchNotify(Panel_item item, Event *event)
{
	olxvman_SearchPopup_objects *ip = (olxvman_SearchPopup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	fputs("olxvman: SearchNotify\n", stderr);
	
	/* gxv_start_connections DO NOT EDIT THIS SECTION */

	/* gxv_end_connections */

}

/*
 * Done callback function for `ManualPagePopup'.
 */
void
ManualPagePinned(Frame frame)
{
	fputs("olxvman: ManualPagePinned\n", stderr);
	xv_set(frame, XV_SHOW, FALSE, NULL);
	
	/* gxv_start_connections DO NOT EDIT THIS SECTION */

	/* gxv_end_connections */

}
