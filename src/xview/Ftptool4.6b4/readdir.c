#include "ftptool.h"

#pragma ident   "@(#)readdir.c 1.10     93/09/28"

extern int dirlastmtime;

#ifdef USE_PROTOTYPES
struct dirlist *read_local_dir(char *dirname)
#else
struct dirlist *read_local_dir(dirname)
char	*dirname;
#endif
{
	struct dirlist *head;
	DIR	*dir;
	struct dirent *dp;
	struct passwd *psswd;
	struct group *grp;
	struct stat buf;
	time_t	curtime;
	time_t	time();
	char	date[DATELEN];
	struct tm *tm;
	char	owner[20];
	char	group[20];


	head = new_dirlist("", "", "", "", 0, (size_t)0);
	if (head == NULL) {
		fprintf(stderr, "Out of memory\n");
		goto out;
	}

	if (lstat(dirname, &buf) == -1) {
		local_footer_message("Could not stat directory %s.", dirname);
		goto out;
	}

	dirlastmtime = buf.st_mtime;

	if ((dir = opendir(dirname)) == NULL) {
		local_footer_message("Could not open directory %s.", dirname);
		goto out;
	}

	curtime = time((time_t *)NULL);


	while ((dp = readdir(dir)) != NULL) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		bzero((char *)&buf, sizeof (buf));
		if (lstat(dp->d_name, &buf) == -1) {
			buf.st_mode = 0;
			buf.st_uid = 0;
			buf.st_gid = 0;
			buf.st_size = 0;
			buf.st_mtime = 0;
		}
		tm = localtime(&buf.st_mtime);
		if ((curtime - buf.st_mtime) < (6 * 30 * 24 * 60 * 60))
			strftime(date, sizeof (date), "%h %e %H:%M", tm);
		else
			strftime(date, sizeof (date), "%h %e  %Y", tm);

		/* determine user and group ids */
		psswd = getpwuid(buf.st_uid);
		if (psswd == NULL) {
			sprintf(owner, "%d", buf.st_uid);
		} else
			strcpy(owner, psswd->pw_name);

		grp = getgrgid(buf.st_gid);
		if (grp == NULL) {
			sprintf(group, "%d", buf.st_gid);
		} else
			strcpy(group, grp->gr_name);

		/* if a link, format name to 'name -> link' as ftp would */
		if (S_ISLNK(buf.st_mode)) {
			char	*linkvalue;
			int		nbytes;

			linkvalue = (char *)malloc(MAXPATHLEN + 1);
			if ((nbytes = readlink(dp->d_name, linkvalue,
			    (int)buf.st_size)) == -1)
				strcpy(linkvalue, "unknown");
			else
				linkvalue[nbytes] = '\0';
			sprintf(scratch, "%s -> %s", dp->d_name, linkvalue);
			free(linkvalue);
		} else
			strcpy(scratch, dp->d_name);
		if (add_dirname(head, scratch, date, owner, group,
		    buf.st_mode, (size_t)buf.st_size, local_sort_mode,
		    local_sort_direction) == NULL)
			goto out;
	}
	closedir(dir);

out:
	if (head)
		return (head);
	return (NULL);
}

#ifdef USE_PROTOTYPES
struct dirlist *read_remote_dir(void)
#else
struct dirlist *read_remote_dir()
#endif
{
	struct dirlist *head = NULL;
	int	old_unix, temp_non_unix = 0;
	FILE	*din = NULL;
	int		vms_expect_dir;

restart:
	old_unix = temp_non_unix;
	head = new_dirlist("", "", "", "", 0, (size_t)0);
	if (head == NULL) {
		fprintf(stderr, "Out of memory\n");
		goto out;
	}
	/* read the remote directory, adding files and directories to a list */
	/* send dir command */
	if (non_unix || temp_non_unix) {
		din = open_remote_ls(1);
		vms_expect_dir = 0;
	} else {
		din = open_remote_ls(0);
		vms_expect_dir = 1;
	}
	if (din == NULL) {
		/* always have .. */
		return (head);
	}

	for (;;) {
		if (next_remote_line(din) == NULL)
			goto out;
		if (!strncmp(response_line, "226", 3) ||
		    !strncmp(response_line, "250", 3)) {
			/* done */
			log_message(response_line);
			break;
		}
		/* ignore blank lines */
		if (*response_line == '\n' || *response_line == '\0')
			continue;
		/* VMS also prints the directory name */
		if (remote_os_type == REMOTE_OS_VMS && vms_expect_dir) {
			/* first non-blank line is directory name */
			vms_expect_dir = 0;
			continue;
		}
		/* VMS goofy total line before 226 */
		if (!strncmp(response_line, "Total of", 8))
			continue;
		if (parse_line(head, response_line, &temp_non_unix)) {
			if (old_unix != temp_non_unix) {
				/* start over */
				free_dirlist(head);
				head = NULL;
				while (getc(din) != -1)
					/* NULL */;
				close_remote_ls(din);
				din = NULL;
				goto restart;
			} else
				goto out;
		}
	}

out:
	if (din)
		close_remote_ls(din);
	if (timedout) {
		if (head)
			free_dirlist(head);
		timeout_disconnect();
		return (NULL);
	}
	if (head)
		return (head);
	return (NULL);
}


#define	NULLCHECK() if (*curr == '\0') { *temp_non_unix = 1; return (1); }

#ifdef USE_PROTOTYPES
int	parse_line(struct dirlist *head, char *line, int *temp_non_unix)
#else
int	parse_line(head, line, temp_non_unix)
struct dirlist *head;
char	*line;
int		*temp_non_unix;
#endif
{
	if (non_unix || *temp_non_unix) {
		return (parse_line_ls(head, line));
	}
	switch (remote_os_type) {
	case REMOTE_OS_UNIX:
		return (parse_line_pattern(head, unix_dir_pattern, line,
		    temp_non_unix));
		break;
	case REMOTE_OS_VMS:
		return (parse_line_vms(head, line, temp_non_unix));
		break;
	case REMOTE_OS_DOS:
		return (parse_line_dos(head, line, temp_non_unix));
		break;
	case REMOTE_OS_OTHER:
		return (parse_line_pattern(head, other_dir_pattern, line,
		    temp_non_unix));
		break;
	}
	return (1);
}

#ifdef USE_PROTOTYPES
int	parse_line_ls(struct dirlist *head, char *line)
#else
int	parse_line_ls(head, line)
struct dirlist *head;
char	*line;
#endif
{
	char	*curr;
	static char	name[MAXPATHLEN + 1];
	char	*tmp;
	mode_t	type;

	/* Assume that if the first character is upper case, */
	/* it is non-unix (since ls never puts upper case as the type */
	/* And it appears that the filename is all the characters at the */
	/* start of the line up to but not including white space on VMS and */
	/* tops20. This would make it functional, partially at least */
	/* Also, may need to turn on temp_non_unix_mode, so that cd is */
	/* possible on anything, since we can't tell if they're directories. */
	/* or not */
	curr = line;

	while (*curr && isspace(*curr))
		curr++;
	tmp = name;
	while (*curr && !isspace(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	curr++;

	type = S_IFLNK;
	if (name[strlen(name)] == '/')
		type = S_IFDIR;

	if (add_dirname(head, name, "unknown", "unknown", "unknown", type,
		(size_t)-1, remote_sort_mode, remote_sort_direction) == NULL) {
		fprintf(stderr, "add_dirname failed!\n");
		return (1);
	}
	return (0);
}

#ifdef USE_PROTOTYPES
int	parse_line_pattern(struct dirlist *head, char *pattern,
	char *line, int *temp_non_unix)
#else
int	parse_line_pattern(head, pattern, line, temp_non_unix)
struct dirlist *head;
char	*pattern;
char	*line;
int		*temp_non_unix;
#endif
{
	/* default mode is a symbolic link. This is so if you don't have */
	/* UNIX PERMS, it can be either a file or a directory. Maybe.  */
	mode_t	mode = S_IFLNK;
	int		intmode;
	char	*curr;
	static char	date[20];
	static char	month[10];
	static char	day[10];
	static char	timeyear[10];
	static char	name[MAXPATHLEN + 1];
	static char owner[25];
	static char group[25];
	size_t	size = (size_t)-1;
	char	*tmp;
	char	*dirtmp;

	month[0] = '\0';
	day[0] = '\0';
	timeyear[0] = '\0';
	date[0] = '\0';
	name[0] = '\0';
	strcpy(owner, "unknown");
	strcpy(group, "unknown");

	curr = line;
	dirtmp = pattern;
	while (*dirtmp != '\0') {
		NULLCHECK();
		switch (*dirtmp) {
		case ' ':
			while (*curr && isspace(*curr))
				curr++;
			break;
		case SKIP:
			while (*curr && !isspace(*curr))
				curr++;
			break;
		case PERMS:
			intmode = unix_perms(curr, temp_non_unix);
			if (intmode == 0)
				return (0);
			else if (intmode == -1)
				return (1);
			mode = (mode_t)intmode;
			while (*curr && !isspace(*curr))
				curr++;
			break;
		case LINKS:
			/* dump link count */
			while (isdigit(*curr))
				curr++;
			break;
		case USER:
			/*
			 * this should be the user name, surrounded by
			 * white space
			 */
			tmp = owner;
			while (*curr && !isspace(*curr)) {
				*tmp = *curr;
				tmp++;
				curr++;
			}
			*tmp = '\0';
			break;
		case GROUP:
			/*
			 * this should be the group name, surrounded by
			 * white space
			 */
			tmp = group;
			while (*curr && !isspace(*curr)) {
				*tmp = *curr;
				tmp++;
				curr++;
			}
			*tmp = '\0';
			break;
		case SIZE:
			/* first test only true for UNIX case, where we have */
			/* seen the perms */
			if (S_ISCHR(mode) || S_ISBLK(mode)) {
				/* size is actually major, minor */
				while (isdigit(*curr) || *curr == ',')
					curr++;
				NULLCHECK();
				while (*curr && isspace(*curr))
					curr++;
				NULLCHECK();
				while (isdigit(*curr))
					curr++;
				size = -1;
			} else {
				sscanf(curr, "%d", &size);
				while (isdigit(*curr))
					curr++;
			}
			break;
		case MONTH:
			tmp = month;
			while (isalpha(*curr)) {
				*tmp = *curr;
				tmp++;
				curr++;
			}
			*tmp = '\0';
			break;
		case DAY:
			tmp = day;
			while (isdigit(*curr)) {
				*tmp = *curr;
				tmp++;
				curr++;
			}
			*tmp = '\0';
			break;
		case TIME:
			tmp = timeyear;
			while (isdigit(*curr) || (*curr == ':')) {
				*tmp = *curr;
				tmp++;
				curr++;
			}
			*tmp = '\0';
			break;
		case NAME:
		case LOWERNAME:
			tmp = name;
			/*
			 * The following test makes sure we have seen the
			 * PERMS field.  If not, the permissions will
			 * still be 0. symlinks normally have 777 for
			 * permissions
			 */
			if (mode != S_IFLNK && S_ISLNK(mode)) {
				while (*curr && *curr != '\n') {
					*tmp = *curr;
					tmp++;
					curr++;
				}
			} else {
				/* if token last in line, eat till newline */
				if (*(dirtmp + 1) == '\0') {
					while (*curr && *curr != '\n') {
						*tmp = *curr;
						tmp++;
						curr++;
					}
				} else {
					while (*curr && !isspace(*curr) &&
					    *curr != ';') {
						*tmp = *curr;
						tmp++;
						curr++;
					}
				}
				/* VMS */
				if (*curr == ';') {
					while (*curr && !isspace(*curr))
						curr++;
				}
			}
			*tmp = '\0';
			if (*dirtmp == LOWERNAME) {
				for (tmp = name; *tmp != '\0'; tmp++)
					if (isupper(*tmp))
						*tmp = tolower(*tmp);
			}
			break;
		default:
			if (*dirtmp == *curr) {
				curr++;
			} else {
				*temp_non_unix = 1;
				return (1);
			}
		}
		dirtmp++;
	}
	sprintf(date, "%s %2s %5s", month, day, timeyear);

	/* The wuarchive ftp server prints . and .. in the listing */
	/* ignore them */
	if ((name[0] == '.' && name[1] == '\0') ||
	    (name[0] == '.' && name[1] == '.' && name[2] == '\0')) {
		return (0);
	}

	if (add_dirname(head, name, date, owner, group, mode, size,
		remote_sort_mode, remote_sort_direction) == NULL) {
		fprintf(stderr, "add_dirname failed!\n");
		return (1);
	}
	return (0);
}

static char *abbrev_month[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};



#ifdef USE_PROTOTYPES
int	parse_line_vms(struct dirlist *head, char *line, int *temp_non_unix)
#else
int	parse_line_vms(head, line, temp_non_unix)
struct dirlist *head;
char	*line;
int		*temp_non_unix;
#endif
{
	/* default mode is a symbolic link. This is so if you don't have */
	/* UNIX PERMS, it can be either a file or a directory. Maybe.  */
	mode_t	mode = S_IFLNK;
	int		intmode;
	char	*curr;
	static char	date[20];
	static char	month[10];
	static char	day[10];
	static char	hour[10];
	static char	year[10];
	static char	name[MAXPATHLEN + 1];
	static char owner[25];
	static char group[25];
	static int partial = 0;
	size_t	size = (size_t)-1;
	char	*tmp;
	time_t	curtime, filetime;
	extern int cummonthdays[];
	int	i;
	int	founddash;
	int day_i, month_i, year_i, min_i, hour_i;

	month[0] = '\0';
	day[0] = '\0';
	hour[0] = '\0';
	year[0] = '\0';
	date[0] = '\0';
	if (partial == 0)
		name[0] = '\0';

	strcpy(owner, "unknown");
	strcpy(group, "unknown");

	curr = line;

	/* Format is */
	/*
	 *
	 * NETINFO_ROOT:[000000]
	 *
	 * 0README.;25            7   1-DEC-1991 16:55 NETINFO (RWD,RWD,,R)
	 * STATUS.19-MAR-1992;1
	 *                        7  19-MAR-1992 12:48 NETINFO (RWED,RWED,RE,RE)
	 * UOFA.DIR;1             2  15-SEP-1989 11:49 NETINFO (RWE,RWE,RE,RE)
	 *
	 */
	/*
	 * Blank lines are already ignored.
	 * Directory name should already be ignored too.
	 */
	if (partial == 0) {
		tmp = name;
		while (*curr && !isspace(*curr)) {
			*tmp = *curr;
			tmp++;
			curr++;
		}
		*tmp = '\0';
		/*
		 * Got the name. However, it may break lines into two, as
		 * STATUS above. After reading a name, see if we have to
		 * save and wait for the next line.
		 */
		tmp = curr;
		while (*tmp && isspace(*tmp))
			tmp++;
		if (*tmp == '\0') {
			partial = 1;
			return (0);
		}
	} else {
		/* already have name from previous call */
		partial = 0;
	}

	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* size, in 512 byte blocks? */
	/* could be missing. */
	tmp = curr;
	founddash = 0;
	while (!isspace(*tmp)) {
		if (*tmp == '-') {
			founddash = 1;
			break;
		}
		tmp++;
	}
	if (!founddash) {
		sscanf(curr, "%d", &size);
		while (isdigit(*curr))
			curr++;
		size *= 512;
	}

	while (isspace(*curr))
		curr++;

	/* Date: 1-DEC-1991 16:55 */
	tmp = day;
	while (isdigit(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	if (*curr != '-')
		return (1);
	curr++;
	NULLCHECK();

	tmp = month;
	while (isalpha(*curr)) {
		if (tmp != month && isupper(*curr))
			*tmp = tolower(*curr);
		else
			*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	if (*curr != '-')
		return (1);
	curr++;
	NULLCHECK();

	tmp = year;
	while (isdigit(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	if (*curr != ' ')
		return (1);
	curr++;
	NULLCHECK();

	tmp = hour;
	while (isdigit(*curr) || *curr == ':') {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	while (*curr && !isspace(*curr)) {
		curr++;
	}
	NULLCHECK();

	curr++;
	NULLCHECK();

	/*
	 * this should be the user name, surrounded by white space
	 */
	tmp = owner;
	while (!isspace(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	curr++;
	NULLCHECK();

	while (isspace(*curr))
		curr++;

	intmode = vms_perms(curr, temp_non_unix);
	if (intmode == 0)
		return (0);
	else if (intmode == -1)
		return (1);
	mode = (mode_t)(intmode | vms_filetype(name));

	day_i = atoi(day);
	month_i = 0;
	for (i = 0; i < 12; i++)
		if (!strncmp(month, abbrev_month[i], 3))
			break;
	if (i != 12)
		month_i = i;
	year_i = atoi(year);
	year_i -= 1970; /* time kept since 1970 */

	hour_i = min_i = 0;
	sscanf(hour, "%d:%d", &hour_i, &min_i);


	filetime = ((year_i * 365) + cummonthdays[month_i] + day_i) * 24 * 60;
	filetime += min_i + (hour_i * 60);
	curtime = time((time_t *)NULL);
	curtime /= 60; /* seconds -> minutes */


	if ((curtime - filetime) < (6 * 30 * 24 * 60))
		sprintf(date, "%s %2s %5s", month, day, hour);
	else
		sprintf(date, "%s %2s %5s", month, day, year);

	/* change name to lowercase, and remove ';num' */
	tmp = strrchr(name, ';');
	if (tmp != NULL)
		*tmp = '\0';
	for (tmp = name; *tmp != '\0'; tmp++)
		if (isupper(*tmp))
			*tmp = tolower(*tmp);
	if (add_dirname(head, name, date, owner, group, mode, size,
		remote_sort_mode, remote_sort_direction) == NULL) {
		fprintf(stderr, "add_dirname failed!\n");
		return (1);
	}
	return (0);
}

#ifdef USE_PROTOTYPES
int	parse_line_dos(struct dirlist *head, char *line, int *temp_non_unix)
#else
int	parse_line_dos(head, line, temp_non_unix)
struct dirlist *head;
char	*line;
int		*temp_non_unix;
#endif
{
	/* default mode is a symbolic link. This is so if you don't have */
	/* UNIX PERMS, it can be either a file or a directory. Maybe.  */
	mode_t	mode = S_IFLNK;
	char	*curr;
	static char	date[20];
	static char	month[10];
	static char	day[10];
	static char	year[10];
	static char	hour[10];
	static char	name[MAXPATHLEN + 1];
	size_t	size = (size_t)-1;
	char	*tmp;
	time_t	curtime, filetime;
	int	i;
	int	day_i, month_i, year_i, min_i, hour_i;
	extern int cummonthdays[];

	month[0] = '\0';
	day[0] = '\0';
	year[0] = '\0';
	hour[0] = '\0';
	date[0] = '\0';
	name[0] = '\0';

	/* Format is */
	/*
    	 * 	33430            IO.SYS   Tue Apr 09 05:00:00 1991
    	 * 	37394         MSDOS.SYS   Tue Apr 09 05:00:00 1991
    	 * 	47845       COMMAND.COM   Tue Apr 09 05:00:00 1991
	 * <dir>                    DOS   Wed Sep 23 19:43:32 1992
	 * <dir>                  WPWIN   Fri Oct 30 03:04:50 1992
     	 * 	9349         WINA20.386   Tue Apr 09 05:00:00 1991
	 * <dir>                VWCOMMS   Tue Dec 01 02:52:44 1992
     	 * 	123          CONFIG.SYS   Fri Nov 27 03:17:40 1992
     	 * 	589        AUTOEXEC.BAT   Tue Dec 01 02:42:42 1992
	 */
	curr = line;
	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* directory or file+size? */
	if (isdigit(*curr)) {
		/* file size */
		mode = S_IFREG;
		sscanf(curr, "%d", &size);
		while (isdigit(*curr))
			curr++;
	} else if (!strncasecmp(curr, "<dir>", 5)) {
		/* directory */
		mode = S_IFDIR;
		while (!isspace(*curr))
			curr++;
	} else {
		*temp_non_unix = 1;
		return (1);
	}
	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* name */
	tmp = name;
	while (!isspace(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* Skip day of week */
	while (!isspace(*curr))
		curr++;
	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* month */
	tmp = month;
	while (isalpha(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	while (isspace(*curr))
		curr++;
	NULLCHECK();

	/* day */
	tmp = day;
	while (isdigit(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	while (isspace(*curr))
		curr++;
	NULLCHECK();


	/* hour:minute:seconds */
	tmp = hour;
	while (isdigit(*curr) || *curr == ':') {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';
	while (!isspace(*curr))
		curr++;
	NULLCHECK();

	/* year */
	tmp = year;
	while (isdigit(*curr)) {
		*tmp = *curr;
		tmp++;
		curr++;
	}
	*tmp = '\0';

	day_i = atoi(day);
	month_i = 0;
	for (i = 0; i < 12; i++)
		if (!strncmp(month, abbrev_month[i], 3))
			break;
	if (i != 12)
		month_i = i;
	year_i = atoi(year);
	year_i -= 1970; /* time kept since 1970 */

	hour_i = min_i = 0;
	sscanf(hour, "%d:%d", &hour_i, &min_i);


	filetime = ((year_i * 365) + cummonthdays[month_i] + day_i) * 24 * 60;
	filetime += min_i + (hour_i * 60);
	curtime = time((time_t *)NULL);
	curtime /= 60; /* seconds -> minutes */


	if ((curtime - filetime) < (6 * 30 * 24 * 60))
		sprintf(date, "%s %2s %5s", month, day, hour);
	else
		sprintf(date, "%s %2s %5s", month, day, year);

	/* change name to lowercase */
	for (tmp = name; *tmp != '\0'; tmp++)
		if (isupper(*tmp))
			*tmp = tolower(*tmp);

	if (add_dirname(head, name, date, "unknown", "unknown", mode, size,
		remote_sort_mode, remote_sort_direction) == NULL) {
		fprintf(stderr, "add_dirname failed!\n");
		return (1);
	}
	return (0);
}

/*
 * take the dir_parse string, in the form of
 *	PERMS LINKS USER GROUP SIZE MONTH DAY TIME NAME
 * and return a NULL-terminated array of character values of the above
 *
 * The values would be control-characters, so they are not in danger
 * of being typed (right!). A ' ' represents whitespace, any other character
 * must be matched exactly.
 */

char	*lex_string;

#ifdef USE_PROTOTYPES
char *dir_parse_to_pattern(char *dir_parse)
#else
char *dir_parse_to_pattern(dir_parse)
char	*dir_parse;
#endif
{
	char 	pattern[MAXPATHLEN+1];
	char	*nextpos = pattern;
	char	*s;
	int		token;
	int		found_bitmask = 0;
	int		yylex();
	static char *tokval[] = {
		"",
		"PERMS",
		"LINKS",
		"USER",
		"GROUP",
		"SIZE",
		"MONTH",
		"DAY",
		"TIME",
		"NAME",
		"SKIP",
		"NONUNIX",
		"LOWERNAME",
	};

	lex_string = strdup(dir_parse);
	if (lex_string == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	s = lex_string;
	while ((token = yylex()) != 0) {
		*nextpos = (char)token;
		nextpos++;
		if (token <= MAXTOKENS) {
			if (found_bitmask & (1 << token)) {
				fprintf(stderr,
				    "Duplicate token %s in DIR template.\n",
				    tokval[token]);
				free(s);
				return (NULL);
			}
			if (token != SKIP)
				found_bitmask |= 1 << token;
		}
	}
	*nextpos = '\0';
	free(s);
	if ((found_bitmask & (1 << NONUNIX)) != 0) {
		pattern[0] = (char)NONUNIX;
		return (strdup(pattern));
	}
	/* make sure that we have a filename, at least */
	if ((found_bitmask & (1 << NAME)) == 0 &&
	    (found_bitmask & (1 << LOWERNAME)) == 0) {
		fprintf(stderr,
		    "You must specify a NAME token in the parse field.\n");
		return (NULL);
	}
	return (strdup(pattern));
}

#undef NULLCHECK
#define	NULLCHECK() if (*s == '\0') { *temp_non_unix = 1; return (-1); }

#ifdef USE_PROTOTYPES
int unix_perms(char *s, int *temp_non_unix)
#else
int unix_perms(s, temp_non_unix)
char	*s;
int		*temp_non_unix;
#endif
{
	int mode;

	switch (*s) {
	case 'd':
			mode = S_IFDIR;
			break;
	case 'F':
	case 'f':
	case 'm': /* Cray and convex migrated files */
	case '-':
			mode = S_IFREG;
			break;
	case 'l':
			mode = S_IFLNK;
			break;
	case 'b':
			mode = S_IFBLK;
			break;
	case 'c':
			mode = S_IFCHR;
			break;
	case 's':
			mode = S_IFSOCK;
			break;
	case 'p':
			mode = S_IFIFO;
			break;
	case 'D':
			mode = S_IFDIR;
			break;
	case 'B':
			mode = S_IFBLK;
			break;
	case 'C':
			mode = S_IFCHR;
			break;
	case 'S':
			mode = S_IFSOCK;
			break;
	case 'P':
			mode = S_IFIFO;
			break;
	case 't':
			if (!strncmp(s, "total", 5))
				return (0);
			/* fall through */
#ifdef notdef
	case '.':
		if (s[1] == ':') {
			ftperr = ftp_error(' ', "Permission denied");
			footer_message(ftperr);
			return (0);
		} else if (!strncmp(s, ". unreadable", 12)) {
			footer_message(". unreadable.");
			return (0);
		}
#endif
	default:
			*temp_non_unix = 1;
			return (-1);
			break;
	}

	s++;
	if (*s == ' ' || *s == '\0' || *s == '\t')
		return (mode); /* OK to not have permissions */
	NULLCHECK();
	/*
	 * Determine permissions.
	 */

#define	SET_PERM1(ch, val) \
	{ \
		if (*s == ch) {\
			mode |= val; \
		} else if (*s != '-') { \
			*temp_non_unix = 1; \
			return (-1); \
		} \
		s++; \
	}

#define	SET_PERM2(ch1, ch2, val) \
	{ \
		if (*s == ch1 || *s == ch2) {\
			mode |= val; \
		} else if (*s != '-') { \
			*temp_non_unix = 1; \
			return (-1); \
		} \
		s++; \
	}


	SET_PERM1('r', S_IRUSR);
	NULLCHECK();

	SET_PERM1('w', S_IWUSR);
	NULLCHECK();

	if (*s == 'S')
		*s = '-';
	SET_PERM2('x', 's', S_IXUSR);
	NULLCHECK();

	SET_PERM1('r', S_IRGRP);
	NULLCHECK();

	SET_PERM1('w', S_IWGRP);
	NULLCHECK();

	if (*s == 'S')
		*s = '-';
	SET_PERM2('x', 's', S_IXGRP);
	NULLCHECK();

	SET_PERM1('r', S_IROTH);
	NULLCHECK();

	SET_PERM1('w', S_IWOTH);
	NULLCHECK();

	if (*s == 'T')
		*s = '-';
	SET_PERM2('x', 't', S_IXOTH);
	NULLCHECK();

	return (mode);
}

#ifdef USE_PROTOTYPES
int vms_perms_set(char *s)
#else
int vms_perms_set(s)
char	*s;
#endif
{
	int	set = 0;
	char	*tmp;

	for (tmp = s; *tmp != '\0'; tmp++) {
		/* handle RWED */
		switch (*tmp) {
		case 'R':
			set |= S_IROTH;
			break;
		case 'W':
			set |= S_IWOTH;
			break;
		case 'E':
			set |= S_IXOTH;
			break;
		case 'D':
			/* ignore */
			break;
		default:
			return (set);
		}
	}
	return (set);
}

#ifdef USE_PROTOTYPES
int vms_perms(char *s, int *temp_non_unix)
#else
int vms_perms(s, temp_non_unix)
char	*s;
int		*temp_non_unix;
#endif
{
	int mode;

	/* s is of the form: */
	/* (RWED,RWED,RWED,RWED) */
	/* system, owner, group, world */

	if (*s != '(')
		return (-1);
	s++;
	NULLCHECK();
	/*
	 * Skip system.
	 */
	while (*s && *s != ',')
		s++;
	NULLCHECK();
	/* otherwise, it's a comma */
	s++;

	/* owner */
	mode = vms_perms_set(s) << 6;

	while (*s && *s != ',')
		s++;
	NULLCHECK();
	/* otherwise, it's a comma */
	s++;

	/* group */
	mode |= vms_perms_set(s) << 3;

	while (*s && *s != ',')
		s++;
	NULLCHECK();
	/* otherwise, it's a comma */
	s++;

	/* world */
	mode |= vms_perms_set(s);

	return (mode);
}

#ifdef USE_PROTOTYPES
int vms_filetype(char *s)
#else
int vms_filetype(s)
char	*s;
#endif
{
	char *period;

	period = strrchr(s, '.');
	if (period != NULL) {
		if (!strncmp(period, ".DIR", 4)) {
			/* Zap .DIR* for */
			*period = '\0';
			return (S_IFDIR);
		}
	}
	return (S_IFREG);
}
