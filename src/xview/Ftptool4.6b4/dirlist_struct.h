
#pragma ident   "@(#)dirlist_struct.h 1.3     93/05/25"

#define	DATELEN 20

struct dirlist {
	struct dirlist *next;
	char	*name;
	char	*date;
	char	*owner;
	char	*group;
	mode_t	mode;
	size_t	size;
};

#define	SORTBYNAME	0
#define	SORTBYDATE	1
#define	SORTBYSIZE	2

#define	ASCENDING	0
#define	DESCENDING	1

#define	GROUPBYTYPE 0
#define	GROUPNONE	1
