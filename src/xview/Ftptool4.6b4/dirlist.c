#include "ftptool.h"

#pragma ident   "@(#)dirlist.c 1.4     93/05/26"

#ifdef USE_PROTOTYPES
struct dirlist *new_dirlist(char *name, char *date, char *owner,
	char *group, mode_t mode, size_t size)
#else
struct dirlist *new_dirlist(name, date, owner, group, mode, size)
char	*name;
char	*date;
char	*owner;
char	*group;
mode_t	mode;
size_t	size;
#endif
{
	struct dirlist *tmp;

	tmp = (struct dirlist *)malloc(sizeof (struct dirlist));
	if (tmp == NULL)
		return (NULL);
	bzero((char *)tmp, sizeof (struct dirlist));
	tmp->name = (char *)malloc((unsigned int)(strlen(name) + 1));
	if (tmp->name == NULL) {
		free_dirlist(tmp);
		return (NULL);
	}

	tmp->date = (char *)malloc((unsigned int)(strlen(date) + 1));
	if (tmp->date == NULL) {
		free_dirlist(tmp);
		return (NULL);
	}

	tmp->owner = (char *)malloc((unsigned int)(strlen(owner) + 1));
	if (tmp->owner == NULL) {
		free_dirlist(tmp);
		return (NULL);
	}

	tmp->group = (char *)malloc((unsigned int)(strlen(group) + 1));
	if (tmp->group == NULL) {
		free_dirlist(tmp);
		return (NULL);
	}

	strcpy(tmp->name, name);
	strcpy(tmp->date, date);
	strcpy(tmp->owner, owner);
	strcpy(tmp->group, group);
	tmp->mode = mode;
	tmp->size = size;
	tmp->next = NULL;
	return (tmp);
}

#ifdef USE_PROTOTYPES
struct dirlist *add_dirname(struct dirlist *head, char *name, char *date,
	char *owner, char *group, mode_t mode, size_t size, int sort_mode,
	int sort_direction)
#else
struct dirlist *add_dirname(head, name, date, owner, group, mode,
	size, sort_mode, sort_direction)
struct dirlist *head;
char	*name;
char	*date;
char	*owner;
char	*group;
mode_t	mode;
size_t	size;
int sort_mode;
int	sort_direction;
#endif
{
	struct dirlist *tmp;
	struct dirlist *oldnext = NULL;

	if (non_unix)
		sort_mode = SORTBYNAME;
	switch (sort_mode) {
	default:
		fprintf(stderr, "Unknown sort mode in add_dirname.\n");
		/* Fall through */
	case SORTBYNAME:
		if (sort_direction == ASCENDING)
			tmp = sortupbyname(head, name);
		else
			tmp = sortdownbyname(head, name);
		break;
	case SORTBYDATE:
		if (sort_direction == ASCENDING)
			tmp = sortupbydate(head, date);
		else
			tmp = sortdownbydate(head, date);
		break;
	case SORTBYSIZE:
		if (sort_direction == ASCENDING)
			tmp = sortupbysize(head, size);
		else
			tmp = sortdownbysize(head, size);
		break;
	}
	oldnext = tmp->next;
	tmp->next = new_dirlist(name, date, owner, group, mode, size);
	if (tmp->next == NULL) {
		tmp->next = oldnext;
		return (NULL);
	}

	tmp->next->next = oldnext;
	return (head);
}

#ifdef USE_PROTOTYPES
struct dirlist *add_dirlist_struct(struct dirlist *head,
	struct dirlist *dlist, int sort_mode, int sort_direction)
#else
struct dirlist *add_dirlist_struct(head, dlist, sort_mode, sort_direction)
struct dirlist *head;
struct dirlist *dlist;
int sort_mode;
int	sort_direction;
#endif
{
	struct dirlist *tmp;

	if (non_unix)
		sort_mode = SORTBYNAME;
	switch (sort_mode) {
	default:
		fprintf(stderr, "Unknown sort mode in add_dirlist_struct.\n");
		/* Fall through */
	case SORTBYNAME:
		if (sort_direction == ASCENDING)
			tmp = sortupbyname(head, dlist->name);
		else
			tmp = sortdownbyname(head, dlist->name);
		break;
	case SORTBYDATE:
		if (sort_direction == ASCENDING)
			tmp = sortupbydate(head, dlist->date);
		else
			tmp = sortdownbydate(head, dlist->date);
		break;
	case SORTBYSIZE:
		if (sort_direction == ASCENDING)
			tmp = sortupbysize(head, dlist->size);
		else
			tmp = sortdownbysize(head, dlist->size);
		break;
	}
	dlist->next = tmp->next;
	tmp->next = dlist;
	return (head);
}

#ifdef USE_PROTOTYPES
void free_dirlist(struct dirlist *head)
#else
void free_dirlist(head)
struct dirlist *head;
#endif
{
	struct dirlist *tmp;

	while (head) {
		tmp = head->next;
		if (head->name)
			free(head->name);
		if (head->date)
			free(head->date);
		if (head->owner)
			free(head->owner);
		if (head->group)
			free(head->group);
		free((char *)head);
		head = tmp;
	}
}

/* alphabetical order */
#ifdef USE_PROTOTYPES
struct dirlist *sortupbyname(struct dirlist *head, char *name)
#else
struct dirlist *sortupbyname(head, name)
struct dirlist *head;
char	*name;
#endif
{
	struct dirlist *tmp;
	int		rval;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (ignore_case)
			rval = strcasecmp(name, tmp->next->name);
		else
			rval = strcmp(name, tmp->next->name);
		if (rval < 0)
			break; /* need to go before next entry. */
	}
	return (tmp);
}

/* least recently modified */
#ifdef USE_PROTOTYPES
struct dirlist *sortupbydate(struct dirlist *head, char *date)
#else
struct dirlist *sortupbydate(head, date)
struct dirlist *head;
char	*date;
#endif
{
	struct dirlist *tmp;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (isearlier(date, tmp->next->date))
			break; /* need to go before next entry. */
	}
	return (tmp);
}

/* smallest to largest */
#ifdef USE_PROTOTYPES
struct dirlist *sortupbysize(struct dirlist *head, size_t size)
#else
struct dirlist *sortupbysize(head, size)
struct dirlist *head;
size_t	size;
#endif
{
	struct dirlist *tmp;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next)
		if (size <= tmp->next->size)
			break; /* need to go before next entry. */
	return (tmp);
}

int cummonthdays[] = {
	0,
	31,
	59,
	90,
	120,
	151,
	181,
	212,
	243,
	273,
	304,
	334
};

#ifdef USE_PROTOTYPES
long	datetotime(char *date)
#else
long	datetotime(date)
char	*date;
#endif
{
	struct tm tm;

	/* "Aug 19 19:47" */
	/* "Jan 10  1990" */

	if (index(date, ':')) {
		hour_time(date, &tm);
		if (tm.tm_mon > current_month)
			tm.tm_year = current_year - 1;
		else
			tm.tm_year = current_year;
	} else {
		year_time(date, &tm);
		tm.tm_hour = 0;
		tm.tm_min = 0;
	}
	return (tm.tm_min + tm.tm_hour * 60
	    + (tm.tm_year * 365 + tm.tm_mday +
	    cummonthdays[tm.tm_mon]) * 1440);
}

#ifdef USE_PROTOTYPES
int isearlier(char *date1, char *date2)
#else
int isearlier(date1, date2)
char	*date1, *date2;
#endif
{
	long	time1;
	long	time2;


	time1 = datetotime(date1);
	time2 = datetotime(date2);

	if (time1 < time2)
		return (1);
	return (0);
}

/* reverse alphabetical order */
#ifdef USE_PROTOTYPES
struct dirlist *sortdownbyname(struct dirlist *head, char *name)
#else
struct dirlist *sortdownbyname(head, name)
struct dirlist *head;
char	*name;
#endif
{
	struct dirlist *tmp;
	int		rval;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (ignore_case)
			rval = strcasecmp(name, tmp->next->name);
		else
			rval = strcmp(name, tmp->next->name);
		if (rval > 0)
			break; /* need to go before next entry. */
	}
	return (tmp);
}

/* most recently modified */
#ifdef USE_PROTOTYPES
struct dirlist *sortdownbydate(struct dirlist *head, char *date)
#else
struct dirlist *sortdownbydate(head, date)
struct dirlist *head;
char	*date;
#endif
{
	struct dirlist *tmp;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (!isearlier(date, tmp->next->date)) {
			break; /* need to go before next entry. */
		}
	}
	return (tmp);
}

/* largest to smallest */
#ifdef USE_PROTOTYPES
struct dirlist *sortdownbysize(struct dirlist *head, size_t size)
#else
struct dirlist *sortdownbysize(head, size)
struct dirlist *head;
size_t	size;
#endif
{
	struct dirlist *tmp;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next)
		if (size >= tmp->next->size)
			break; /* need to go before next entry. */
	return (tmp);
}

#ifdef USE_PROTOTYPES
void clear_slist(Panel panel_list)
#else
void clear_slist(panel_list)
Panel panel_list;
#endif
{
	int		nitems, row;
	struct dirlist *dotdot;

	xv_set(panel_list,
		XV_SHOW, FALSE,
		NULL);
	nitems = xv_get(panel_list, PANEL_LIST_NROWS);
	if (nitems && (dotdot = (struct dirlist *)xv_get(panel_list,
		PANEL_LIST_CLIENT_DATA, 0))) {
		dotdot->next = NULL;
		free_dirlist(dotdot);
	}
	for (row = nitems - 1; row >= 0; row--) {
		xv_set(panel_list,
			PANEL_LIST_DELETE, row,
			PANEL_PAINT, PANEL_NONE,
			NULL);
	}
	panel_paint(panel_list, PANEL_CLEAR);
	xv_set(panel_list,
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void actual_dirlist_to_slist(Panel panel_list, struct dirlist *head,
	mode_t type, int showdotfiles)
#else
void actual_dirlist_to_slist(panel_list, head, type, showdotfiles)
Panel	panel_list;
struct dirlist *head;
mode_t	type;
int		showdotfiles;
#endif
{
	struct dirlist *tmp;
	int	row;
	Xv_font	entry_font;
	char *d;
	Server_image entry_glyph;

	row = 0;
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (type && ((type & S_IFMT) != (tmp->mode & S_IFMT)))
			continue;
		if ((tmp != head->next) &&
		    (tmp->name[0] == '.' && !showdotfiles))
			continue;
		d = tmp->date;
		if (!strcmp(tmp->date, "unknown"))
			d = "";
		if (tmp->size == -1)
			sprintf(scratch, "  %12s  %10s  %s", d, "",
			    tmp->name);
		else
			sprintf(scratch, "  %12s  %10d  %s", d, tmp->size,
			    tmp->name);

		switch (tmp->mode & S_IFMT) {
		case S_IFDIR:
			entry_font = bold_list_font;
			if (row != 0)
				entry_glyph = directory_glyph;
			else
				entry_glyph = dotdot_glyph;
			strcat(scratch, "/");
			break;
		case S_IFREG:
			entry_font = list_font;
			entry_glyph = file_glyph;
			break;
		case S_IFLNK:
			entry_font = list_font;
			entry_glyph = link_glyph;
			break;
		default:
			entry_font = list_font;
			entry_glyph = unknown_glyph;
			break;
		}
		xv_set(panel_list,
			PANEL_LIST_INSERT, row,
			PANEL_LIST_STRING, row, scratch,
			PANEL_LIST_FONT, row, entry_font,
			PANEL_LIST_GLYPH, row, entry_glyph,
			PANEL_LIST_CLIENT_DATA, row, tmp,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		row++;
	}

}

#ifdef USE_PROTOTYPES
void dirlist_to_slist(Panel panel_list, struct dirlist *head)
#else
void dirlist_to_slist(panel_list, head)
Panel	panel_list;
struct dirlist *head;
#endif
{
	struct dirlist *tmp;
	int	dotypesort = 0;
	int	showdotfiles = 0;

	xv_set(panel_list,
		XV_SHOW, FALSE,
		NULL);
	add_dotdot(head);

	if (panel_list == local_window.list) {
		showdotfiles = local_showdotfiles;
		if (group_local_files)
			dotypesort = 1;
	} else {
		/* panel_list == base_window.list */
		showdotfiles = remote_showdotfiles;
		if (group_remote_files)
			dotypesort = 1;
	}

	if (dotypesort) {
		/* have to insert backwards, since the routine inserts at 0 */
#ifdef S_IFIFO
		actual_dirlist_to_slist(panel_list, head, S_IFIFO,
		    showdotfiles);
#endif
#ifdef S_IFSOCK
		actual_dirlist_to_slist(panel_list, head, S_IFSOCK,
		    showdotfiles);
#endif
		actual_dirlist_to_slist(panel_list, head, S_IFBLK,
		    showdotfiles);
		actual_dirlist_to_slist(panel_list, head, S_IFCHR,
		    showdotfiles);
		actual_dirlist_to_slist(panel_list, head, S_IFLNK,
		    showdotfiles);
		actual_dirlist_to_slist(panel_list, head, S_IFREG,
		    showdotfiles);
		actual_dirlist_to_slist(panel_list, head, S_IFDIR,
		    showdotfiles);
	} else {
		actual_dirlist_to_slist(panel_list, head, 0, showdotfiles);
	}

	/* now remove '..' from list */
	tmp = head->next;
	head->next = head->next->next;
	tmp->next = NULL;

	/* all entries attached as client_data */
	xv_set(panel_list,
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void add_dotdot(struct dirlist *head)
#else
void add_dotdot(head)
struct dirlist *head;
#endif
{
	struct dirlist *tmp;

	tmp = new_dirlist("..", "unknown", "", "", S_IFDIR, (size_t)-1);
	if (tmp == NULL)
		return;

	tmp->next = head->next;
	head->next = tmp;
}

#ifdef USE_PROTOTYPES
struct dirlist *sort_dirlist(struct dirlist *head, int sort_mode,
	int sort_direction)
#else
struct dirlist *sort_dirlist(head, sort_mode, sort_direction)
struct dirlist *head;
int	sort_mode;
int	sort_direction;
#endif
{
	struct dirlist *tmp;
	struct dirlist *next;

	if (head == NULL)
		return (NULL);
	/* sort current list into new one */
	tmp = head->next;
	head->next = NULL;
	while (tmp) {
		next = tmp->next;
		tmp->next = NULL;
		(void) add_dirlist_struct(head, tmp, sort_mode, sort_direction);
		tmp = next;
	}
	return (head);
}

static char *abbrev_month[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

#ifdef USE_PROTOTYPES
void hour_time(char *date, struct tm *tm)
#else
void hour_time(date, tm)
char	*date;
struct tm *tm;
#endif
{
	char	month[10];
	int		i;

	bzero((char *)tm, sizeof (struct tm));
	sscanf(date, "%s%d%d:%d", month, &tm->tm_mday,
		&tm->tm_hour, &tm->tm_min);
	for (i = 0; i < 12; i++)
		if (!strncmp(month, abbrev_month[i], 3))
			break;
	if (i != 12)
		tm->tm_mon = i;
}

#ifdef USE_PROTOTYPES
void year_time(char *date, struct tm *tm)
#else
void year_time(date, tm)
char	*date;
struct tm *tm;
#endif
{
	char	month[10];
	int		i;

	bzero((char *)tm, sizeof (struct tm));
	sscanf(date, "%s%d%d", month, &tm->tm_mday, &tm->tm_year);
	for (i = 0; i < 12; i++)
		if (!strncmp(month, abbrev_month[i], 3))
			break;
	if (i != 12)
		tm->tm_mon = i;
	if (tm->tm_year > 1900)
		tm->tm_year -= 1900;
}
