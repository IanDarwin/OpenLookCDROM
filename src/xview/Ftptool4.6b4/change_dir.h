
#pragma ident   "@(#)change_dir.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

int	checkdir(char *dirname);
int dirwasmodified(void);
int change_local_dir(char *s, int force);
int change_remote_dir(char *s, int force);
char *expand_dirname(char *arg);
int	delete_local_dir(char *dir);
int delete_local_file(char *filename, int (*deletefunc)(const char *filename));
int	delete_remote_dir(char *dir);
int delete_remote_file(char *filename, char *deletecmd);
int up_one_level(void);

#else

int	checkdir();
int dirwasmodified();
int change_local_dir();
int change_remote_dir();
char *expand_dirname();
int	delete_local_dir();
int delete_local_file();
int	delete_remote_dir();
int delete_remote_file();
int up_one_level();

#endif
