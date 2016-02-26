
#pragma ident   "@(#)tar_view.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

void handle_tarfile(char *filename);
void list_proc(void);
void doextract_proc(void);
void tar_extract_proc(void);
void start_tar(char *options, char *filename);
Notify_value input_func(Notify_client client, int fd);
Notify_value tar_destroy_func(Notify_client client, Destroy_status status);
void tar_quit_proc(void);

#else

void handle_tarfile();
void list_proc();
void doextract_proc();
void tar_extract_proc();
void start_tar();
Notify_value input_func();
Notify_value tar_destroy_func();
void tar_quit_proc();

#endif
