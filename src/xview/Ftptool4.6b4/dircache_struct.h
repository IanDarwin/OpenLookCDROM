
#pragma ident   "@(#)dircache_struct.h 1.3     93/05/25"

struct dirlist_header {
	struct dirlist_header *next;
	struct dirlist_header *prev;
	struct dirlist *dlist;
	char	*name;
	int		mtime;
};

struct dircache {
	int		ndirs;
	struct dirlist_header *first;
	struct dirlist_header *last;
};
