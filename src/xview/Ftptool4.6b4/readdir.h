
#pragma ident   "@(#)readdir.h 1.4     93/05/25"

#define	PERMS		1
#define	LINKS		2
#define	USER		3
#define	GROUP		4
#define	SIZE		5
#define	MONTH		6
#define	DAY		7
#define	TIME		8
#define	NAME		9
#define	SKIP		10
#define	NONUNIX		11
#define	LOWERNAME	12
#define	MAXTOKENS	12

/* for multiple directory formats */
/* must match strings in create_other.c */
#define	REMOTE_OS_UNIX	0
#define	REMOTE_OS_VMS	1
#define	REMOTE_OS_DOS	2
#define	REMOTE_OS_OTHER	3

#ifdef USE_PROTOTYPES

struct dirlist *read_local_dir(char *dirname);
struct dirlist *read_remote_dir(void);
int	parse_line(struct dirlist *head, char *line, int *temp_non_unix);
int	parse_line_ls(struct dirlist *head, char *line);
int	parse_line_pattern(struct dirlist *head, char *pattern,
	char *line, int *temp_non_unix);
int	parse_line_vms(struct dirlist *head, char *line, int *temp_non_unix);
int	parse_line_dos(struct dirlist *head, char *line, int *temp_non_unix);
char *dir_parse_to_pattern(char *dir_parse);
int unix_perms(char *s, int *temp_non_unix);
int vms_perms(char *s, int *temp_non_unix);
int vms_perms_set(char *s);
int vms_filetype(char *s);

#else

struct dirlist *read_local_dir();
struct dirlist *read_remote_dir();
int	parse_line();
int	parse_line_ls();
int	parse_line_pattern();
int	parse_line_vms();
int	parse_line_dos();
char *dir_parse_to_pattern();
int unix_perms();
int vms_perms();
int vms_perms_set();
int vms_filetype();

#endif
