
#pragma ident   "@(#)schedule_struct.h 1.3     93/05/25"

struct schedule {
	char	*menu_name;
	int		direction;
	time_t	date;
	time_t	repeat_minutes;
	char	*filename;
	struct hostlist *hl;
	struct schedule *next;
};

#define	RECV	0
#define	SEND	1
