
#pragma ident   "@(#)host_list.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

void host_list_clean_proc(Panel_item item, Event *event);
void host_list_item_proc(Menu menu, Menu_item menu_item);
void host_list_add_proc(Menu menu, Menu_item menu_item);
void host_list_change_proc(Menu menu, Menu_item menu_item);
void host_save_proc(Menu menu, Menu_item menu_item);
void host_load_proc(Menu menu, Menu_item menu_item);
void update_timestamp(void);
void enter_host_info(int warnchange);
void host_list_delete_proc(Menu menu, Menu_item menu_item);
int host_list_use_proc(Panel_item item, char *string,
	Xv_opaque client_data, Panel_list_op op, Event *event);
struct hostlist *new_hostlist(void);
struct hostlist *add_hostalias(struct hostlist *head, char *aliasname,
	char *last_visited, char *proxy, char *host, char *login,
	char *password, char *account, int transfer_mode,
	char *remote_directory, char *local_directory, char *dir_parse,
	char *comment, int os_type);
void free_hostlist(struct hostlist *head);
struct hostlist *gethostlist(struct hostlist *head, char *aliasname);
int delete_hostlist(struct hostlist *head, char *aliasname);
void read_ftptoolrc(void);
int read_entry(int version, FILE *fp, char *aliasname, char *last_visited,
	char *proxy, char *host, char *login, char *password,
	int *transfer_mode, char *rdir, char *ldir, char *dir_parse,
	char *comment);
int read_newentry(int version, FILE *fp, char *aliasname, char *last_visited,
	char *proxy, char *host, char *login, char *password, char *account,
	int *transfer_mode, char *rdir, char *ldir, char *dir_parse,
	char *comment, int *os_type);
void read_oldftptoolrc(void);
void read_newftptoolrc(void);
void write_ftptoolrc(void);
int ftptoolrc_fd(int flags, int mode);
void reload_host_list_menu(struct hostlist *head);
char *create_dotfile(char *dotfile, int mode);
char *find_dotfile(char *dotfile);
int netrc_token(FILE *fp);
void host_append_netrc_proc(void);
void add_netrc(char *machine, char *login, char *password);
char key_to_char(char *key);
char *ftptool_encrypt(char *s, char *key);
char *ftptool_decrypt(char *s, char *key);
char *old_ftptool_decrypt(char *s, char *key);
struct hostlist *sort_hostlist(struct hostlist *head);
void host_window_update(struct hostlist *ent);

#else

void host_list_clean_proc();
void host_list_item_proc();
void host_list_add_proc();
void host_list_change_proc();
void host_save_proc();
void host_load_proc();
void update_timestamp();
void enter_host_info();
void host_list_delete_proc();
int host_list_use_proc();
struct hostlist *new_hostlist();
struct hostlist *add_hostalias();
void free_hostlist();
struct hostlist *gethostlist();
int delete_hostlist();
void read_ftptoolrc();
int read_entry();
int read_newentry();
void read_newftptoolrc();
void read_oldftptoolrc();
void write_ftptoolrc();
int ftptoolrc_fd();
void reload_host_list_menu();
char *create_dotfile();
char *find_dotfile();
int netrc_token();
void host_append_netrc_proc();
void add_netrc();
char key_to_char();
char *ftptool_encrypt();
char *ftptool_decrypt();
char *old_ftptool_decrypt();
struct hostlist *sort_hostlist();
void host_window_update();

#endif
