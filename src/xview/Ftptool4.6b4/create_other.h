
#pragma ident   "@(#)create_other.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

void create_host_popup(void);
void create_session_log(void);
void create_about_window(void);
void create_feedback_window(void);
void create_file_property_window(struct file_property_window *file_props,
	char *header);
void create_tar_file_popup(void);
void create_load_save_popup(Frame *framep, Panel *textp, Panel *buttonp);
void create_schedule_window(void);
void create_status_window(void);

#else

void create_host_popup();
void create_session_log();
void create_about_window();
void create_feedback_window();
void create_file_property_window();
void create_tar_file_popup();
void create_load_save_popup();
void create_schedule_window();
void create_status_window();

#endif
