
#pragma ident   "@(#)button_funcs.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

void	connect_proc(void);
void	local_properties(void);
void	remote_properties(void);
void	props_proc(void);
void	get_proc(void);
void	batchget_proc(void);
void	uncompress_proc(void);
void	compress_proc(void);
void	tar_proc(void);
void	create_tar_proc(void);
void	extract_proc(void);
void	put_proc(void);
void	batchput_proc(void);
void	remote_view(void);
void	local_view(void);
void	local_dir_view(void);
void	session_view(void);
void	status_view(void);
void	host_view(void);
void	schedule_view(void);
void	abort_proc(void);
int local_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);
int remote_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);
int send_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);
int receive_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);
void apply_changes(void);
void ftptool_props_apply_proc(void);
void directory_lists_props_apply_proc(void);
void viewers_props_apply_proc(void);
void ftptool_props_reset_proc(void);
void directory_lists_props_reset_proc(void);
void viewers_props_reset_proc(void);
void about_proc(void);
void	remote_sort_choice_proc(void);
void set_remote_sort_order(int val);
void	local_sort_choice_proc(void);
void set_local_sort_order(int val);
void	about_send_proc(void);
void	feedback_address_proc(Panel_item item, unsigned int value,
	Event *event);
void	feedback_send_proc(Panel_item item, Event *event);
void	feedback_cancel_proc(void);
Panel_setting	reject_spaces(Panel_item item, Event *event);
void	remote_delete_proc(void);
void	local_delete_proc(void);
void	show_load_receive_list_proc(void);
void	show_save_receive_list_proc(void);
void	show_load_send_list_proc(void);
void	show_save_send_list_proc(void);
void	load_send_list_proc(void);
void	save_send_list_proc(void);
void	load_receive_list_proc(void);
void	save_receive_list_proc(void);
void add_batch_send_proc(void);
void add_batch_receive_proc(void);
void	props_inf_check_box(Panel_item item, int value, Event *event);
void	repeat_check_box(Panel_item item, int value, Event *event);
void	quit_proc(void);
void switch_category(int value, int show);
void category_proc(void);
void remote_os_proc(void);
void	dismiss_local_window(void);
void	dismiss_host_window(void);
void	dismiss_about_window(void);
void	dismiss_file_props_window(Panel_item item, Event *event);
void	dismiss_schedule_window(void);
void	dismiss_status_window(void);
void dir_list_proc(void);
void ls_list_proc(void);
void list_remote_dir(void);
void save_layout_func(void);
void local_doubleclick(struct dirlist *tmp);
void remote_doubleclick(void);
void plus_proc(void);
void minus_proc(void);
Panel_setting   repeat_minute_check(Panel_item item, Event *event);
int host_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);

#else

void	connect_proc();
void	local_properties();
void	remote_properties();
void	props_proc();
void	get_proc();
void	batchget_proc();
void	uncompress_proc();
void	compress_proc();
void	tar_proc();
void	create_tar_proc();
void	extract_proc();
void	put_proc();
void	batchput_proc();
void	remote_view();
void	local_view();
void	local_dir_view();
void	session_view();
void	status_view();
void	host_view();
void	schedule_view();
void	abort_proc();
int local_list_proc();
int remote_list_proc();
int send_list_proc();
int receive_list_proc();
void apply_changes();
void ftptool_props_apply_proc();
void directory_lists_props_apply_proc();
void viewers_props_apply_proc();
void ftptool_props_reset_proc();
void directory_lists_props_reset_proc();
void viewers_props_reset_proc();
void about_proc();
void	remote_sort_choice_proc();
void set_remote_sort_order();
void	local_sort_choice_proc();
void set_local_sort_order();
void	about_send_proc();
void	feedback_address_proc();
void	feedback_send_proc();
void	feedback_cancel_proc();
Panel_setting	reject_spaces();
void	remote_delete_proc();
void	local_delete_proc();
void	show_load_receive_list_proc();
void	show_save_receive_list_proc();
void	show_load_send_list_proc();
void	show_save_send_list_proc();
void	load_send_list_proc();
void	save_send_list_proc();
void	load_receive_list_proc();
void	save_receive_list_proc();
void add_batch_send_proc();
void add_batch_receive_proc();
void	props_inf_check_box();
void	repeat_check_box();
void	quit_proc();
void switch_category();
void category_proc();
void remote_os_proc();
void	dismiss_local_window();
void	dismiss_host_window();
void	dismiss_about_window();
void	dismiss_file_props_window();
void	dismiss_schedule_window();
void	dismiss_status_window();
void dir_list_proc();
void ls_list_proc();
void list_remote_dir();
void save_layout_func();
void local_doubleclick();
void remote_doubleclick();
void plus_proc();
void minus_proc();
Panel_setting   repeat_minute_check();
int host_list_proc();

#endif
