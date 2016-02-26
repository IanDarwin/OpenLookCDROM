
#pragma ident   "@(#)host_list_struct.h 1.3     93/05/25"

struct hostlist {
	struct hostlist *next;
	char	*aliasname;
	char	*last_visited;
	char	*proxy;
	char	*host;
	char	*login;
	char	*password;
	char	*account;
	int		os_type;
	char	*dir_parse;
	int		transfer_mode;
	char	*remote_directory;
	char	*local_directory;
	char	*comment;
};
