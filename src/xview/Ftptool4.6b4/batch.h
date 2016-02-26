
#pragma ident   "@(#)batch.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

struct batchlist *new_batchlist(void);
struct batchlist *add_batchname(Panel panel_list, char *name, mode_t mode,
	size_t size, char *dir);
void free_batchlist(Panel panel_list, int only_selected);
int batchentry_exists(Panel panel_list, char *name);
void receive_list_delete_proc(void);
void send_list_delete_proc(void);
void dobatchget(void);
void dobatchput(void);
int save_batch_list(Panel list, char *filename);
int load_batch_list(Panel list, char *filename);

#else

struct batchlist *new_batchlist();
struct batchlist *add_batchname();
void free_batchlist();
int batchentry_exists();
void receive_list_delete_proc();
void send_list_delete_proc();
void dobatchget();
void dobatchput();
int save_batch_list();
int load_batch_list();

#endif
