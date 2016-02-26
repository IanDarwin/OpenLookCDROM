
#pragma ident   "@(#)view_file_struct.h 1.4     93/05/25"

#define	FILE_VIEWER	0
#define	FILE_DECOMPRESSOR	1

struct extension_info {
	struct extension_info	*next;
	char	*extension;
	char	*magic;
	char	*program;
	int	type;
};
