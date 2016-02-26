/********************************************************************************/
/* xview/menu.h --								*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtool_MENU_H_
#define _FSPtool_MENU_H_ 1

/********************************************************************************/

extern void hosts_menu_proc(Menu,Menu_item);
extern void view_menu_proc(Menu,Menu_item);
extern void file_menu_proc(Menu,Menu_item);
extern void fsp_menu_proc(Menu,Menu_item);
extern void local_file_menu_proc(Menu,Menu_item);
extern void local_view_menu_proc(Menu,Menu_item);
extern void properties_menu_proc(Menu,Menu_item);
extern void hostlist_menu_proc(Menu,Menu_item);
extern void dir_list_menu_proc(Menu,Menu_item);
extern void cache_menu_proc(Menu,Menu_item);
extern void batch_write_menu_proc(Menu,Menu_item);
extern void batch_options_menu_proc(Menu,Menu_item);
extern void batch_options_delete_proc(Menu,Menu_item);
extern void batch_options_transfer_proc(Menu,Menu_item);

extern Menu make_menu(void*,char*[]);
extern Menu make_hosts_menu(void*);

extern Menu make_cache_menu();
extern Menu make_batch_options_menu();

#endif

/********************************************************************************/

