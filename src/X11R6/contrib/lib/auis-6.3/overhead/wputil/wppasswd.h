
/* ************************************************************ *\
        wppasswd.h
        Header file declaring the passwd for use with makeboth.
	It ensures that uid and gid are represented by int.
\* ************************************************************ */

struct	wppasswd { /* see getpwent(3) */
	char	*pw_name;
	char	*pw_passwd;
	int	pw_uid;
	int	pw_gid;
#ifndef	__SYSTEM_FIVE
	int	pw_quota;	/* ULTRIX, BSD-4.2 */
#else /*	SYSTEM_FIVE */
	char	*pw_age;	/* System V */
#endif /*	__SYSTEM_FIVE */
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
};
