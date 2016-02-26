/********************************************************************************/
/* frame.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/* Date   : 14/05/93								*/
/* Version: 0.6	(19/05/93)							*/
/********************************************************************************/

#ifndef _FSPtool_FRAME_H_
#define _FSPtool_FRAME_H_ 1

#include "create.h"

/********************************************************************************/

#define	item_name(a,b)	(strcmp(b,(char*)xv_get(a,PANEL_LABEL_STRING)) == 0)

#define FRAMEUP(a)	((int) xv_get(a, XV_SHOW))

/********************************************************************************/

extern void realize_aboutfsp();
extern void handle_frame(Panel_item,Event*);
extern void menu_handler(Menu,Xv_opaque);
extern void button_handler(Panel_item,Event*);
extern void left_footer(Frame,char*,...);
extern void right_footer(Frame,int,int);
extern void unmapframe(Frame);

extern void set_transferframe(const char*,int,int,int);
extern void set_transferdone(int,int);
extern void set_frame_busy(int);

extern void unselect_dir_files(Panel_item);
extern void select_dir_files(Panel_item);

extern Panel_setting handle_port_entry(Panel_item,Event*);
extern Panel_setting handle_host_entry(Panel_item,Event*);

extern void handle_panel_resize(Xv_Window,Event*,Notify_arg);
extern void handle_local_panel_resize(Xv_Window,Event*,Notify_arg);
extern void handle_batch_panel_resize(Xv_Window,Event*,Notify_arg);
extern void set_dirlist_size(Panel,Panel_item);
extern void set_tool_properties(Panel_item,int,Event*);
extern void set_dirlist_properties(Panel_item,int,Event*);
extern void set_fsp_properties(Panel_item,int,Event*);
extern void set_fsp_clients(Panel_item,int,Event*);
extern void set_host_clear(Panel_item,int,Event*);
extern void set_order_type(Panel_item,int,Event*);
extern void handle_baseframe(Xv_Window,Event*,Notify_arg);
extern void sethost_proc(Panel_item,int,Event*);
extern void set_host(Panel_item,int,Event*);
extern void do_transfer_callback(Panel_item,int,Event*);

extern void hosts_select_proc(Panel_item,char*,caddr_t,Panel_list_op,Event*);

extern void hostlist_edit_proc(Panel_item,int,Event*);
extern void hostlist_delete_proc(Panel_item,int,Event*);
extern void hostlist_clear_proc(Panel_item,int,Event*);
extern void hostlist_load_proc(Panel_item,int,Event*);
extern void hostlist_save_proc(Panel_item,int,Event*);

extern int  drop_notify_proc(Panel_item,unsigned int,Event*);

#endif

/********************************************************************************/

