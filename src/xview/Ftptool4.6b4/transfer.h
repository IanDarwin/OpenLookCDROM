
#pragma ident   "@(#)transfer.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

int get_file(char *name, char *localname, int size);
int put_file(char *name, char *remote_name, int size);
int get_dir(char *parent_remote_dir, char *parent_local_dir,
	char *name, char *localname);
int put_dir(char *parent_remote_dir, char *parent_local_dir,
	char *name, char *localname);
char *make_path(char *parent, char *curdir);
void init_status(int total);
void end_status(void);
void update_status_label(char *direction, char *name, int size);
void update_status_gauge(long bytes);
int sum_local_dir(char *parent, char *dir);
int sum_local_size(void);
int sum_remote_dir(char *parent, char *dir);
int sum_remote_size(void);
int sum_remote_batch_size(void);
int sum_local_batch_size(void);

#else

int get_file();
int put_file();
int get_dir();
int put_dir();
char *make_path();
void init_status();
void end_status();
void update_status_label();
void update_status_gauge();
int sum_local_dir();
int sum_local_size();
int sum_remote_dir();
int sum_remote_size();
int sum_remote_batch_size();
int sum_local_batch_size();

#endif
