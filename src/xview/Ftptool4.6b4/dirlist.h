
#pragma ident   "@(#)dirlist.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

struct dirlist *new_dirlist(char *name, char *date, char *owner,
	char *group, mode_t mode, size_t size);
struct dirlist *add_dirname(struct dirlist *head, char *name, char *date,
	char *owner, char *group, mode_t mode, size_t size, int sort_mode,
	int sort_direction);
struct dirlist *add_dirlist_struct(struct dirlist *head,
	struct dirlist *dlist, int sort_mode, int sort_direction);
void free_dirlist(struct dirlist *head);
struct dirlist *sortupbyname(struct dirlist *head, char *name);
struct dirlist *sortupbydate(struct dirlist *head, char *date);
struct dirlist *sortupbysize(struct dirlist *head, size_t size);
long	datetotime(char *date);
int isearlier(char *date1, char *date2);
struct dirlist *sortdownbyname(struct dirlist *head, char *name);
struct dirlist *sortdownbydate(struct dirlist *head, char *date);
struct dirlist *sortdownbysize(struct dirlist *head, size_t size);
void clear_slist(Panel panel_list);
void actual_dirlist_to_slist(Panel panel_list, struct dirlist *head,
	mode_t type, int showdotfiles);
void dirlist_to_slist(Panel panel_list, struct dirlist *head);
void add_dotdot(struct dirlist *head);
struct dirlist *sort_dirlist(struct dirlist *head, int sort_mode,
	int sort_direction);
void hour_time(char *date, struct tm *tm);
void year_time(char *date, struct tm *tm);

#else

struct dirlist *new_dirlist();
struct dirlist *add_dirname();
struct dirlist *add_dirlist_struct();
void free_dirlist();
struct dirlist *sortupbyname();
struct dirlist *sortupbydate();
struct dirlist *sortupbysize();
long	datetotime();
int isearlier();
struct dirlist *sortdownbyname();
struct dirlist *sortdownbydate();
struct dirlist *sortdownbysize();
void clear_slist();
void actual_dirlist_to_slist();
void dirlist_to_slist();
void add_dotdot();
struct dirlist *sort_dirlist();
void hour_time();
void year_time();

#endif
