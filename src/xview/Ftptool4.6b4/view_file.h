
#pragma ident   "@(#)view_file.h 1.4     93/05/25"

#define	UNKNOWN		(-1)
#define	COMPRESSED	1

#ifdef USE_PROTOTYPES

int start_viewer(char *filename, int isremote);
void fork_viewer(char *program, char *filename);
void compress_file(char *filename, char *options);
void pipe_program(char *argv[]);
int view_local_file(char *name, int which, int *dirchanged);
int view_remote_file(char *name, size_t size);
struct extension_info *new_extension(char *extension, char *magic,
	char *program, int type);
struct extension_info *add_extension(struct extension_info *head,
	char *extension, char *magic, char *program, int type);
void free_extension(struct extension_info *cell);
void delete_extension(struct extension_info *head,
	char *extension);
int extension_exists(struct extension_info *head, char *extension);
int read_extensions(FILE *fp, char *extension, char *magic,
	char *program, int *type);
void load_extensions(void);
void save_extensions(void);
void load_extension_list(struct extension_info *extension_list,
	Panel panel_list);
void clear_extension_list(Panel panel_list);
int extension_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event);
int iscompressed(char *filename);
struct extension_info *type_by_extension(char *filename);
struct extension_info *type_by_magic(char *filename);
struct extension_info *type_by_either(char *filename);
void delete_extension_proc(Panel_item item, Event *event);
void add_extension_proc(Panel_item item, Event *event);
void change_extension_proc(Panel_item item, Event *event);
void enter_extension_info(int warnchange);
void free_argv(char **argv);
char **build_argv(char *program, char *filename);
void uncompress(char *filename);
void compress(char *filename);

#else

int start_viewer();
void fork_viewer();
void compress_file();
void pipe_program();
int view_local_file();
int view_remote_file();
struct extension_info *new_extension();
struct extension_info *add_extension();
void free_extension();
void delete_extension();
int extension_exists();
int read_extensions();
void load_extensions();
void save_extensions();
void load_extension_list();
void clear_extension_list();
int extension_list_proc();
int iscompressed();
struct extension_info *type_by_extension();
struct extension_info *type_by_magic();
struct extension_info *type_by_either();
void delete_extension_proc();
void add_extension_proc();
void change_extension_proc();
void enter_extension_info();
void free_argv();
char **build_argv();
void uncompress();
void compress();

#endif
