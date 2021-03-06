/*
 * resources_ui.c - User interface object initialization functions.
 * This file was generated by `gxv' from `resources.G'.
 * DO NOT EDIT BY HAND.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include "resources_ui.h"

/*
 * Create object `insertMenu' in the specified instance.
 */
Xv_opaque
resources_insertMenu_create(caddr_t ip, Xv_opaque owner)
{
	extern Menu_item	resource_insert_before(Menu_item, Menu_generate);
	extern Menu_item	resource_insert_after(Menu_item, Menu_generate);
	extern Menu_item	resource_insert_top(Menu_item, Menu_generate);
	extern Menu_item	resource_insert_bottom(Menu_item, Menu_generate);
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Before",
			MENU_GEN_PROC, resource_insert_before,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "After",
			MENU_GEN_PROC, resource_insert_after,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Top",
			MENU_GEN_PROC, resource_insert_top,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Bottom",
			MENU_GEN_PROC, resource_insert_bottom,
			NULL,
		MENU_DEFAULT, 5,
		MENU_GEN_PIN_WINDOW, owner, "",
		NULL);
	return obj;
}

/*
 * Initialize an instance of object `resourcesPopup'.
 */
resources_resourcesPopup_objects *
resources_resourcesPopup_objects_initialize(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	if (!ip && !(ip = (resources_resourcesPopup_objects *) calloc(1, sizeof (resources_resourcesPopup_objects))))
		return (resources_resourcesPopup_objects *) NULL;
	if (!ip->resourcesPopup)
		ip->resourcesPopup = resources_resourcesPopup_resourcesPopup_create(ip, owner);
	if (!ip->control)
		ip->control = resources_resourcesPopup_control_create(ip, ip->resourcesPopup);
	if (!ip->resourcesLabel)
		ip->resourcesLabel = resources_resourcesPopup_resourcesLabel_create(ip, ip->control);
	if (!ip->resourceList)
		ip->resourceList = resources_resourcesPopup_resourceList_create(ip, ip->control);
	if (!ip->insertButton)
		ip->insertButton = resources_resourcesPopup_insertButton_create(ip, ip->control);
	if (!ip->deleteButton)
		ip->deleteButton = resources_resourcesPopup_deleteButton_create(ip, ip->control);
	if (!ip->resourceField)
		ip->resourceField = resources_resourcesPopup_resourceField_create(ip, ip->control);
	return ip;
}

/*
 * Create object `resourcesPopup' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_resourcesPopup_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 343,
		XV_HEIGHT, 327,
		XV_LABEL, "Resources",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `control' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_control_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `resourcesLabel' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_resourcesLabel_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Resources",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `resourceList' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_resourceList_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	extern int		resource_list_notify(Panel_item, char *, Xv_opaque, Panel_list_op, Event *, int);
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "resources:resourceList",
		XV_X, 16,
		XV_Y, 40,
		PANEL_LIST_WIDTH, 300,
		PANEL_LIST_DISPLAY_ROWS, 10,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, resource_list_notify,
		NULL);
	return obj;
}

/*
 * Create object `insertButton' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_insertButton_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	extern Xv_opaque	dependencies_insertMenu_create(caddr_t, Xv_opaque);
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "resources:insertButton",
		XV_X, 96,
		XV_Y, 256,
		PANEL_LABEL_STRING, "Insert",
		PANEL_ITEM_MENU, resources_insertMenu_create((caddr_t) ip, ip->resourcesPopup),
		NULL);
	return obj;
}

/*
 * Create object `deleteButton' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_deleteButton_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	extern void		resource_delete(Panel_item, Event *);
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "resources:deleteButton",
		XV_X, 192,
		XV_Y, 256,
		PANEL_LABEL_STRING, "Delete",
		PANEL_NOTIFY_PROC, resource_delete,
		NULL);
	return obj;
}

/*
 * Create object `resourceField' in the specified instance.
 */
Xv_opaque
resources_resourcesPopup_resourceField_create(resources_resourcesPopup_objects *ip, Xv_opaque owner)
{
	extern Panel_setting	resource_enter_callback(Panel_item, Event *);
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "resources:resourceField",
		XV_X, 16,
		XV_Y, 296,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 30,
		PANEL_LABEL_STRING, "Resource:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, resource_enter_callback,
		NULL);
	return obj;
}

