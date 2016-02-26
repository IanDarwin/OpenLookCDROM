/********************************************************************************/
/* xview/menu.c --								*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include "fsptool.h"
#include "frame.h"
#include "file.h"
#include "menu.h"
#include "resource.h"
#include "create.h"
#include "batch.h"

#include "../lib/fsp.h"
#include "../lib/unix.h"
#include "../lib/cache.h"

/********************************************************************************/

extern Tool_Properties	tool_properties;

extern Base_frame	baseframe;
extern Sethost_frame	sethostframe;
extern Transfer_frame	transferframe;
extern Filter_frame	filterframe;
extern Action_frame	actionframe;
extern Hostlist_frame	hostlistframe;
extern Generic_frame	aboutfsptoolframe, aboutfspframe;
extern Local_frame	localframe;
extern Tool_frame	toolframe;
extern Dirlist_frame	dirlistframe;
extern Fsp_frame	fspframe;
extern Clients_frame	clientsframe;
extern Batch_frame	batchframe;

extern HostInfo	       *hosts_list[MAX_HOSTS];
extern int		hosts_loaded;

/********************************************************************************/

Menu make_menu ( void *notify_proc, char *items[] )

/* this fn creates an instance of a menu, with the specified notify procedure	*/
/* each menu item in incrementally numbered for MENU_CLIENT_DATA and has the	*/
/* appropriate MENU_STRING from items.						*/
/*										*/
/* NOTE: use of cnt++ outside of xv_create fn call, this fixes problems found	*/
/* when compiling FSPtool with gcc on Sun-3 machines.				*/

{ Menu menu = (Menu) xv_create(XV_NULL, MENU,
				MENU_NOTIFY_PROC, notify_proc,
				NULL);

  int cnt = 0;
  Menu_item item;


while (items[cnt]) {				/* -- list terminated with NULL */
    item = (Menu_item) xv_create(XV_NULL, MENUITEM,
				MENU_STRING,		c_strdup(items[cnt]),
				MENU_CLIENT_DATA,	cnt,
				MENU_RELEASE,
				NULL);

    cnt++;					/* -- fixes prob. on Sun-3s	*/
    xv_set(menu, MENU_APPEND_ITEM, item, NULL);
    }

return(menu);
}

/********************************************************************************/

Menu make_hosts_menu ( void *notify_proc )

/* this fn creates the list of hosts menu, functionality is as above. This fn	*/
/* has access to both the hosts_lists and hosts_loaded globals.			*/

{ Menu menu = (Menu) xv_create(XV_NULL, MENU,
				MENU_NOTIFY_PROC,	notify_proc,
				MENU_NROWS,		20,
				NULL);

  int loop;

  Menu_item item;

for (loop = 0; loop < hosts_loaded; loop++) {
    item = (Menu_item) xv_create(XV_NULL, MENUITEM,
				MENU_STRING,		hosts_list[loop]->alias,
				MENU_CLIENT_DATA,	loop,
				MENU_RELEASE,
				NULL);

    xv_set(menu, MENU_APPEND_ITEM, item, NULL);
    }

return(menu);
}

/********************************************************************************/

Menu make_cache_menu ()

/* this fn is called to generate the menu of directories in the remote cache	*/

{ Menu menu = (Menu) xv_create(XV_NULL, MENU,
				MENU_NOTIFY_PROC, cache_menu_proc,
				NULL);

  int loop, cnt;

  char **ptr = return_cache_contents(&cnt);

  Menu_item item;


for (loop = 0; loop < cnt; loop++,ptr++ ) {
    item = (Menu_item) xv_create(XV_NULL, MENUITEM,
				MENU_STRING,		*ptr,
				MENU_RELEASE,
				NULL);

    xv_set(menu, MENU_APPEND_ITEM, item, NULL);
    }

return(menu);
}

/********************************************************************************/

Menu make_batch_options_menu ()

/* this fn is called to generate the menu tree for the batch frame options menu	*/

{ Menu delete_submenu, transfer_submenu, menu;

delete_submenu = (Menu) xv_create(XV_NULL, MENU,
		MENU_NOTIFY_PROC, batch_options_delete_proc,
		MENU_ITEM,
			MENU_STRING,		"Delete Selected",
			MENU_CLIENT_DATA,	0,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Delete All",
			MENU_CLIENT_DATA,	1,
			NULL,
		NULL),

transfer_submenu = (Menu) xv_create(XV_NULL, MENU,
		MENU_NOTIFY_PROC, batch_options_transfer_proc,
		MENU_ITEM,
			MENU_STRING,		"Transfer Selected",
			MENU_CLIENT_DATA,	0,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Transfer All",
			MENU_CLIENT_DATA,	1,
			NULL,
		NULL),

menu = (Menu) xv_create(XV_NULL, MENU,
		MENU_NOTIFY_PROC, batch_options_menu_proc,
		MENU_ITEM,
			MENU_STRING,		"Add Files",
			MENU_CLIENT_DATA,	0,
			NULL,
		MENU_PULLRIGHT_ITEM,		"Delete",	delete_submenu,
		MENU_ITEM,
			MENU_STRING,		"Group Files",
			MENU_CLIENT_DATA,	2,
			NULL,
		MENU_PULLRIGHT_ITEM,		"Do Transfer",	transfer_submenu,
		NULL);

return(menu);
}

/********************************************************************************/

void cache_menu_proc ( Menu menu, Menu_item menu_item )

{
xv_set(baseframe.dir_text,PANEL_VALUE,(char*)xv_get(menu_item,MENU_STRING),NULL);
set_fsp_dir((char*) xv_get(menu_item,MENU_STRING));
update_dir_list();
}

/********************************************************************************/

void hosts_menu_proc ( Menu menu, Menu_item menu_item )

/* this function is called when the user selects an item from the Hosts menu */

{ int host_no;
 char buf[32];

if (!hosts_loaded)
     return;

host_no = xv_get(menu_item,MENU_CLIENT_DATA);

xv_set(sethostframe.host, PANEL_VALUE, hosts_list[host_no]->hostname, NULL);
xv_set(sethostframe.host, PANEL_CLIENT_DATA, 
		c_strdup(hosts_list[host_no]->hostname), NULL);

sprintf(buf,"%1d",hosts_list[host_no]->hostport);
xv_set(sethostframe.port, PANEL_VALUE, buf, NULL);
xv_set(sethostframe.port, PANEL_CLIENT_DATA, c_strdup(buf), NULL);
xv_set(sethostframe.panel, PANEL_CLIENT_DATA, host_no, NULL);
xv_set(sethostframe.frame, XV_SHOW, TRUE, NULL);

if (tool_properties.hostread)
    set_host((Panel_item)0,0,(Event*)NULL);

/* -- so that unpinned frame is not dismissed we set MENU_NOTIFY_STATUS		*/
/* -- to XV_ERROR, XView will not dismiss the frame when the menu pops down	*/
/* -- if this is set.								*/

xv_set(menu, MENU_NOTIFY_STATUS, XV_ERROR, NULL);
}

/********************************************************************************/

void view_menu_proc ( Menu menu, Menu_item menu_item )

/* this function is called when the user selects an item from the View menu */

{ int list_no;


switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 1:
	if ((list_no = get_next_selected(baseframe.dir_list)) > 0) {
	    do_exec_file((CacheData*) xv_get(baseframe.dir_list,
					PANEL_LIST_CLIENT_DATA,list_no));
	    }
	break;

    case 2:
	if (FRAMEUP(batchframe.frame) && tool_properties.menuclose)
	    unmapframe(batchframe.frame);
	else
	    xv_set(batchframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 3:
	if (FRAMEUP(transferframe.frame) && tool_properties.menuclose)
	    unmapframe(transferframe.frame);
	else
	    xv_set(transferframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 4:
	if (FRAMEUP(hostlistframe.frame) && tool_properties.menuclose)
	    unmapframe(hostlistframe.frame);
	else
	    xv_set(hostlistframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 5:
	if (FRAMEUP(aboutfspframe.frame) && tool_properties.menuclose)
	    unmapframe(aboutfspframe.frame);
	else
	    realize_aboutfsp();
	break;

    case 6:
	if (FRAMEUP(aboutfsptoolframe.frame) && tool_properties.menuclose)
	    unmapframe(aboutfsptoolframe.frame);
	else
	    xv_set(aboutfsptoolframe.frame, XV_SHOW, TRUE, NULL);
	break;

    default:
	if (FRAMEUP(localframe.frame) && tool_properties.menuclose)
	    unmapframe(localframe.frame);
	else {
	    xv_set(localframe.frame, XV_SHOW, TRUE, NULL);
	    update_local_dir_list();
	    }
    }
}

/********************************************************************************/

void local_file_menu_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when the an item is selected from the local file menu	*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    default:
	fput_files((CacheData*)NULL);
    }
}

/********************************************************************************/

void local_view_menu_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when the an item is selected from the local view menu	*/

{ int list_no;

switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    default:
	if ((list_no = get_next_selected(localframe.dir_list)) > 0) {
	    do_exec_local_file((CacheData*) xv_get(localframe.dir_list,
					PANEL_LIST_CLIENT_DATA,list_no));
	    }
	break;

    }
}

/********************************************************************************/

void batch_write_menu_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when an item is selected from the batch write menu		*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    default:
	batch_write_text();
	break;
    }
}

/********************************************************************************/

void batch_options_menu_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when an item is selected from the batch options menu	*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 0:
	add_to_batch_list();
	break;

    case 2:
	xv_set(batchframe.batch_list, PANEL_LIST_SORT, PANEL_FORWARD, NULL);
        left_footer(batchframe.frame, "Grouped batch files.");
	break;
    }

}

/********************************************************************************/

void batch_options_delete_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when an item is selected from batch frame delete submenu	*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 0:
	xv_set(batchframe.batch_list, PANEL_LIST_DELETE_SELECTED_ROWS, NULL);
        left_footer(batchframe.frame, "Deleted selected batch items.");
	break;

    case 1:
	xv_set(batchframe.batch_list, PANEL_LIST_DELETE_ROWS, 0,
		(int)xv_get(batchframe.frame, PANEL_LIST_NROWS), NULL);
	left_footer(batchframe.frame, "Deleted all batch items.");
	break;
    }

}

/********************************************************************************/

void batch_options_transfer_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when an item is selected from batch frame transfer submenu	*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 0:
	do_selected_batch_transfer();
	break;

    case 1:
	do_batch_transfer();
	break;
    }

}

/********************************************************************************/

void dir_list_menu_proc ( Menu menu, Menu_item menu_item )

/* this fn is called when an item is selected from the dir_list menu		*/

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 1:
	select_dir_files(baseframe.dir_list);
	break;

    case 2:
	fget_files((CacheData*) NULL);
	break;

    case 3:
	add_to_batch_list();
	break;

    default:
	unselect_dir_files(baseframe.dir_list);
	break;
    }
}

/********************************************************************************/

void file_menu_proc ( Menu menu, Menu_item menu_item )

/* this function is called when the user selects an item from the file menu */

{ int result;

switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 1:
	add_to_batch_list();
	break;

    case 2:
	if (FRAMEUP(filterframe.frame) && tool_properties.menuclose)
	    unmapframe(filterframe.frame);
	else
	    xv_set(filterframe.frame, XV_SHOW, TRUE, NULL);
        break;

    case 3:
	if (FRAMEUP(actionframe.frame) && tool_properties.menuclose)
	    unmapframe(actionframe.frame);
	else
	    xv_set(actionframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 4:
	result = notice_prompt(baseframe.frame, NULL,
			NOTICE_MESSAGE_STRINGS,
				"Do you really wish to Quit ?",
				NULL,
			NOTICE_BUTTON_YES,	"YES",
			NOTICE_BUTTON_NO,	"NO",
			NULL);

	if (result == NOTICE_YES)
	    notify_stop();

	break;

    default:
	fget_files((CacheData*) NULL);
    }
}

/********************************************************************************/

void properties_menu_proc ( Menu menu, Menu_item menu_item )

/* this function is called when the user selects an item from the property menu */

{
switch (xv_get(menu_item,MENU_CLIENT_DATA))
    {
    case 0:
	if (FRAMEUP(toolframe.frame) && tool_properties.menuclose)
	    unmapframe(toolframe.frame);
	else
	    xv_set(toolframe.frame, XV_SHOW, TRUE, NULL);
        break;

    case 1:
	if (FRAMEUP(dirlistframe.frame) && tool_properties.menuclose)
	    unmapframe(dirlistframe.frame);
	else
	    xv_set(dirlistframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 2:
	if (FRAMEUP(fspframe.frame) && tool_properties.menuclose)
	    unmapframe(fspframe.frame);
	else
	    xv_set(fspframe.frame, XV_SHOW, TRUE, NULL);
	break;

    case 3:
	if (FRAMEUP(clientsframe.frame) && tool_properties.menuclose)
	    unmapframe(clientsframe.frame);
	else
	    xv_set(clientsframe.frame, XV_SHOW, TRUE, NULL);
	break;

    default:
	get_layout();
	save_resources();
    }
}

/********************************************************************************/

void hostlist_menu_proc ( Menu menu, Menu_item menu_item )

{
}

/********************************************************************************/

