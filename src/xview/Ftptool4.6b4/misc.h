
#pragma ident   "@(#)misc.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

void log_char(char ch);
void log_message(char *s);
char *ftp_error(char ch, char *def);
void footer_message(char *format, ...);
void local_footer_message(char *format, ...);
void right_footer_message(char *format, ...);
void local_right_footer_message(char *format, ...);
void schedule_footer_message(char *format, ...);
void status_footer_message(char *format, ...);
void timeout_disconnect(void);
void close_files(void);
void quit_ftp(void);
void disconnect(void);
void xfer_buttons_inactive(void);
void xfer_buttons_active(void);
void cursor_busy(void);
void cursor_normal(void);
void show_stats(struct file_property_window *file_props,
	struct dirlist *tmp);
void inactivate_props(struct file_property_window *file_props);
int ask_make_dir(char *s);
int make_dirs(char *s, int make_last);
int ask_make_remote_dir(char *s);
int make_remote_dirs(char *s, int make_last);
void set_geometry(char *s, Frame frame, int def_width, int def_height,
	int def_x, int def_y);
void save_geometry(char *s, Frame frame);
void justify_items(Panel panel, int resize);
void resize_text_item(Panel panel, Panel_item text_item);
char *linkval(char *string);
char *linkname(char *string);
void add_dismiss(Panel panel, Panel_item first, Panel_item dismiss);
void update_date(int doscheddefault);
Notify_value date_wrapper(void);
void local_show_items(void);
void remote_show_items(void);
int ping_server(void);
void caret_to_first(Panel_item item);
void fix_carets(void);

#else

void log_char();
void log_message();
char *ftp_error();
void footer_message();
void local_footer_message();
void right_footer_message();
void local_right_footer_message();
void schedule_footer_message();
void status_footer_message();
void timeout_disconnect();
void close_files();
void quit_ftp();
void disconnect();
void xfer_buttons_inactive();
void xfer_buttons_active();
void cursor_busy();
void cursor_normal();
void show_stats();
void inactivate_props();
int ask_make_dir();
int make_dirs();
int ask_make_remote_dir();
int make_remote_dirs();
void set_geometry();
void save_geometry();
void justify_items();
void resize_text_item();
char *linkval();
char *linkname();
void add_dismiss();
void update_date();
Notify_value date_wrapper();
void local_show_items();
void remote_show_items();
int ping_server();
void caret_to_first();
void fix_carets();

#endif
