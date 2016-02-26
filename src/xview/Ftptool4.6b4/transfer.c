#include "ftptool.h"

#pragma ident   "@(#)transfer.c 1.5     93/05/27"

#ifdef USE_PROTOTYPES
int get_file(char *name, char *localname, int size)
#else
int get_file(name, localname, size)
char	*name;
char	*localname;
int		size;
#endif
{
	char	*ftperr;
	int		rval, answer;

	footer_message("Receiving %s...", name);
	if (abort_transfer) {
		return (EINTR);
	}
	if (!batch_mode && confirmoverwrites && (access(localname,
	    F_OK) == 0)) {
#ifdef XVIEW3
		Xv_notice notice;

		notice = xv_create(base_window.panel, NOTICE,
			NOTICE_MESSAGE_STRINGS,
				localname,
				"already exists. Do you want to overwrite it?",
				NULL,
			NOTICE_BUTTON_YES,	"Yes, overwrite it.",
			NOTICE_BUTTON,		"No, stop the transfer.", 2,
			NOTICE_STATUS, &answer,
			XV_SHOW, TRUE,
			NULL);
		xv_destroy_safe(notice);
#else
		answer = notice_prompt(base_window.panel, NULL,
			NOTICE_MESSAGE_STRINGS,
				localname,
				"already exists. Do you want to overwrite it?",
				NULL,
			NOTICE_BUTTON_YES,	"Yes, overwrite it.",
			NOTICE_BUTTON,		"No, stop the transfer.", 2,
			NULL);
#endif
		if (answer != NOTICE_YES) {
			footer_message("");
			return (EINTR);
		}
	}

	xv_set(base_window.abort,
		PANEL_INACTIVE, FALSE,
		NULL);
	settype((int)xv_get(host_window.advanced.transfer_mode, PANEL_VALUE));
	rval = recvrequest("RETR", localname, name, "w", size);
	switch (rval) {
	case 1:
		/* non_fatal error */
		ftperr = index(response_line, ':');
		if (ftperr != NULL) {
			ftperr++;
			if (!strncmp(ftperr, "Permission", 10))
				rval = EPERM;
			else if (!strncmp(ftperr, "not a plain file", 16))
				rval = EISDIR;
			else
				rval = 0;
		} else
			rval = 0;
		break;
	case 2:
		rval = EIO;
		break;
	default:
		break;
	}

	return (rval);
}

#ifdef USE_PROTOTYPES
int put_file(char *name, char *remote_name, int size)
#else
int put_file(name, remote_name, size)
char	*name;
char	*remote_name;
int		size;
#endif
{
	char	*ftperr;
	int		rval;

	local_footer_message("Sending %s...", name);
	if (abort_transfer) {
		return (EINTR);
	}
	xv_set(base_window.abort,
		PANEL_INACTIVE, FALSE,
		NULL);

	settype((int)xv_get(host_window.advanced.transfer_mode, PANEL_VALUE));
	/* if unique, then use STOU */
	if (unique_remote_names)
		rval = sendrequest("STOU", name, remote_name, size);
	else
		rval = sendrequest("STOR", name, remote_name, size);
	switch (rval) {
	case 1:
		/* non_fatal error */
		ftperr = index(response_line, ':');
		if (ftperr != NULL) {
			ftperr++;
			if (!strncmp(ftperr, "Permission", 10))
				rval = EPERM;
			else
				rval = 0;
		} else
			rval = 0;
		break;
	case 2:
		rval = EIO;
		break;
	default:
		break;
	}

	return (rval);
}

#ifdef USE_PROTOTYPES
int get_dir(char *parent_remote_dir, char *parent_local_dir,
	char *name, char *localname)
#else
int get_dir(parent_remote_dir, parent_local_dir, name, localname)
char	*parent_remote_dir;
char	*parent_local_dir;
char	*name;
char	*localname;
#endif
{
	int		rval = 0;
	extern int errno;
	struct dirlist *head = NULL;
	struct dirlist *tmp;
	char	*ftperr;
	char	*rdir = NULL;
	char	*ldir = NULL;
	char	*lname = NULL;

	/* transfer a remote directory and its contents to the local machine */

	/* First, make a directory */
	if ((rval = make_dirs(localname, 1)) != 0) {
		if (rval != EEXIST) {
			/* XXX - this message is clobbered by 'done reading' */
			/* message */
			return (errno);
		}
	}

	/* cd to that dir */
	command("CWD %s", name);
	if (code != 250) {
		sprintf(scratch, "Remote cd to %s failed.", name);
		ftperr = ftp_error(' ', scratch);
		footer_message(ftperr);
		return (1);
	}
	/* local cd */
	if (chdir(localname) == -1) {
		/* go back up */
		footer_message("Local cd to %s failed.", localname);

		(void) command("CWD %s", parent_remote_dir);
		return (errno);
	}

	head = read_remote_dir();
	if (head == NULL)
		goto out;

	/* do files first */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (S_ISREG(tmp->mode)) {
			if ((rval = get_file(tmp->name, tmp->name,
			    tmp->size)) != 0)
				goto out;
		}
	}

	rdir = make_path(parent_remote_dir, name);
	if (rdir == NULL)
		goto out;

	ldir = make_path(parent_local_dir, localname);
	if (ldir == NULL)
		goto out;

	/* recursively do directories */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (!S_ISDIR(tmp->mode))
			continue;
		if ((rval = get_dir(rdir, ldir, tmp->name, tmp->name)) != 0)
			goto out;
	}

	/* try symlinks */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (!S_ISLNK(tmp->mode))
			continue;
		lname = linkname(tmp->name);
		if (lname == NULL)
			goto out;
		rval = get_file(lname, lname, -1);
		if (rval == EISDIR) {
			/* try as a directory */
			if ((rval = get_dir(rdir, ldir, lname, lname)) > 1) {
				/*
				if (rval == EEXIST) {
				}
				*/
				goto out;
			}
		} else if (rval != 0) {
			goto out;
		}
		free(lname);
		lname = NULL;
	}

out:
	if (head)
		free_dirlist(head);
	if (rdir)
		free(rdir);
	if (ldir)
		free(ldir);
	/* go back up */

	(void) command("CWD %s", parent_remote_dir);
	chdir(parent_local_dir);

	return (rval);
}

#ifdef USE_PROTOTYPES
int put_dir(char *parent_remote_dir, char *parent_local_dir,
	char *name, char *localname)
#else
int put_dir(parent_remote_dir, parent_local_dir, name, localname)
char	*parent_remote_dir;
char	*parent_local_dir;
char	*name;
char	*localname;
#endif
{
	extern int errno;
	int	rval = 0;
	struct dirlist *head = NULL;
	struct dirlist *tmp;
	char	*ftperr;
	char	*rdir = NULL;
	char	*ldir = NULL;
	char	*lname = NULL;

	/* transfer a local directory and its contents to the remote machine */

	/* First, make a directory */
	if (make_remote_dirs(name, 1)) {
	/* XXX - error? */
	/*
		goto out;
	*/
	}
	/* cd to that dir */
	if (command("CWD %s", name) == ERROR && code == 250) {
		sprintf(scratch, "Remote cd to %s failed.", name);
		ftperr = ftp_error(' ', scratch);
		local_footer_message(ftperr);
		return (1);
	}
	/* local cd */
	if (chdir(localname) == -1) {
		local_footer_message("Local cd to %s failed.", localname);
		/* go back up */
		(void) command("CWD %s", parent_remote_dir);
		return (errno);
	}

	head = read_local_dir(".");

	if (head == NULL) {
		rval = ENOMEM;
		goto out;
	}

	/* do files first */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (!S_ISREG(tmp->mode))
			continue;
		if ((rval = put_file(tmp->name, tmp->name, tmp->size)) != 0)
			goto out;
	}

	rdir = make_path(parent_remote_dir, name);
	if (rdir == NULL)
		goto out;

	ldir = make_path(parent_local_dir, localname);
	if (ldir == NULL)
		goto out;

	/* recursively do directories */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (!S_ISDIR(tmp->mode))
			continue;
		if ((rval = put_dir(rdir, ldir, tmp->name, tmp->name)) != 0)
			goto out;
	}

	/* try symlinks */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		if (!S_ISLNK(tmp->mode))
			continue;
		lname = linkname(tmp->name);
		if (lname == NULL)
			goto out;
		rval = put_file(lname, lname, -1);
		if (rval == EPERM) {
			/* try as a directory */
			if ((rval = put_dir(rdir, ldir, lname, lname)) > 1) {
				/*
				if (rval == EEXIST) {
				}
				*/
				goto out;
			}
		} else if (rval != 0) {
			goto out;
		}
		free(lname);
		lname = NULL;
	}

out:
	if (head)
		free_dirlist(head);
	if (rdir)
		free(rdir);
	if (ldir)
		free(ldir);
	/* go back up */
	(void) command("CWD %s", parent_remote_dir);
	chdir(parent_local_dir);

	return (rval);
}

#ifdef USE_PROTOTYPES
char *make_path(char *parent, char *curdir)
#else
char *make_path(parent, curdir)
char	*parent;
char	*curdir;
#endif
{
	char	*tmp;

	if (*curdir == '/') {
		tmp = (char *)strdup(curdir);
	} else {
		tmp = (char *)malloc((unsigned int)(strlen(parent) + 1 +
		    strlen(curdir) + 1));
		if (tmp == NULL)
			return (NULL);
		strcpy(tmp, parent);
		if (strcmp(parent, "/"))
			strcat(tmp, "/");
		strcat(tmp, curdir);
	}
	return (tmp);
}

#define	FILE_PERCENT	5.0
#define	TOTAL_PERCENT	5.0

static double last_percent;
static double last_total_percent;
static int total_so_far;
static int total_transfer_size;
static struct timeval start;

#ifdef USE_PROTOTYPES
void init_status(int total)
#else
void init_status(total)
int	total;
#endif
{
	int gettimeofday();

	total_so_far = 0;
	total_transfer_size = total;
	last_total_percent = 0;
	xv_set(status_window.total_gauge,
		PANEL_INACTIVE, FALSE,
		PANEL_VALUE, 0,
		NULL);
	(void) gettimeofday(&start, (struct timezone *)NULL);
	status_footer_message("Total transfer size: %d bytes.",
	    total_transfer_size);
}

#ifdef USE_PROTOTYPES
void end_status(void)
#else
void end_status()
#endif
{
	struct timeval stop;
	double	total_time;
	double	rate;
	char	*unit;
	int gettimeofday();

	(void) gettimeofday(&stop, (struct timezone *)NULL);
	xv_set(status_window.total_gauge,
		PANEL_INACTIVE, FALSE,
		PANEL_VALUE, 0,
		NULL);
	total_time = (stop.tv_sec + stop.tv_usec/1000000.0)
		- (start.tv_sec + start.tv_usec/1000000.0);
	rate = (double)total_so_far/total_time;
	if (rate >= 1048576.0) {
		unit = "Mbyte";
		rate /= 1048576.0;
	} else if (rate >= 1024.0) {
		unit = "Kbyte";
		rate /= 1024.0;
	} else
		unit = "byte";
	sprintf(scratch, "%.2f seconds, %.2f %ss/second", total_time,
	    rate, unit);
	status_footer_message(scratch);
	log_message(scratch);
}

#ifdef USE_PROTOTYPES
void update_status_label(char *direction, char *name, int size)
#else
void update_status_label(direction, name, size)
char	*direction;
char	*name;
int		size;
#endif
{
	static char sizestr[40];
	static char	string[MAXPATHLEN + 30];
	Rect	*butrect;

	sprintf(string, "%s %s", direction, name);
	xv_set(status_window.message,
		PANEL_LABEL_STRING, string,
		NULL);
	if (size >= 0) {
		sprintf(sizestr, "%d bytes", size);
		xv_set(status_window.size,
			PANEL_LABEL_STRING, sizestr,
			NULL);
		last_percent = 0;
		xv_set(status_window.total_gauge,
			PANEL_INACTIVE, FALSE,
			NULL);
	} else {
		xv_set(status_window.size,
			PANEL_LABEL_STRING, "Symbolic links not counted.",
			NULL);
		xv_set(status_window.total_gauge,
			PANEL_INACTIVE, TRUE,
			NULL);
	}
	butrect = (Rect *)xv_get(status_window.dismiss, XV_RECT);
	xv_set(status_window.dismiss,
		XV_X, xv_get(status_window.panel, XV_WIDTH) / 2
			- butrect->r_width / 2,
		NULL);
}


#ifdef USE_PROTOTYPES
void update_status_gauge(long bytes)
#else
void update_status_gauge(bytes)
long	bytes;
#endif
{
	double	percent;

	if (bytes <= 0) /* shouldn't happen */
		return;

	total_so_far += bytes;
	if (total_so_far > total_transfer_size)
		total_so_far = total_transfer_size;

	if (total_transfer_size > 0.0)
		percent = (double)total_so_far / (double)total_transfer_size *
		    100.0;
	else
		percent = 0.0;
	if (percent < 0.0)
		percent = 0.0;
	if (percent > 100.0)
		percent = 100.0;

	if (percent >= (last_total_percent + TOTAL_PERCENT)) {
		while ((last_total_percent + TOTAL_PERCENT) < percent)
			last_total_percent += TOTAL_PERCENT;
		xv_set(status_window.total_gauge,
			PANEL_VALUE, (int)last_total_percent,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
int sum_local_dir(char *parent, char *dir)
#else
int sum_local_dir(parent, dir)
char	*parent, *dir;
#endif
{
	int total = 0;
	char *ldir = NULL;
	struct dirlist *head = NULL;
	struct dirlist *tmp;

	ldir = make_path(parent, dir);
	if (ldir == NULL)
		return (0);

	if (chdir(ldir) == -1) {
		status_footer_message("Could not change to directory: %s",
				sys_errlist[errno]);
		goto out;
	}

	head = read_local_dir(".");

	if (head == NULL) {
		goto out;
	}

	/* do files first */
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		switch (tmp->mode & S_IFMT) {
		case S_IFREG:
			total += tmp->size;
			break;
		case S_IFDIR:
			total += sum_local_dir(ldir, tmp->name);
			break;
		default:
			break;
		}
	}
	if (chdir(parent) == -1) {
		status_footer_message("Could not change to parent: %s",
				sys_errlist[errno]);
	}

out:
	if (ldir)
		free(ldir);
	if (head)
		free_dirlist(head);
	return (total);
}

/*
 * Called to sum all the sizes from the currently selected local items.
 */
#ifdef USE_PROTOTYPES
int sum_local_size(void)
#else
int sum_local_size()
#endif
{
	int		nitems;
	int		row;
	int		total = 0;
	struct dirlist *tmp;

	status_footer_message("Determining send total...");
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			switch (tmp->mode & S_IFMT) {
			case S_IFDIR:
				total +=
				    sum_local_dir(local_dircache.first->name,
					tmp->name);
				break;
			case S_IFREG:
				total += tmp->size;
				break;
			case S_IFLNK:
				status_footer_message(
				    "Ignoring symlink %s...", tmp->name);
				log_message(
				    "Can only sum sizes of files and directories.\n");
				break;
			default:
				status_footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message(
				    "Can only transfer files and directories.\n");
				break;
			}
		}
	return (total);
}

#ifdef USE_PROTOTYPES
int sum_remote_dir(char *parent, char *dir)
#else
int sum_remote_dir(parent, dir)
char	*parent, *dir;
#endif
{
	int total = 0;
	char *rdir = NULL;
	struct dirlist *head = NULL;
	struct dirlist *tmp;

	if (!strcmp(dir, ".."))
		return (0);

	rdir = make_path(parent, dir);
	if (rdir == NULL)
		return (0);

	code = -1;
	command("CWD %s", rdir);
	if (code == 550) {
		goto out;
	}
	head = read_remote_dir();

	if (head == NULL) {
		goto out;
	}

	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		switch (tmp->mode & S_IFMT) {
		case S_IFREG:
			if (tmp->size != -1)
				total += tmp->size;
			else
				status_footer_message(
				    "Size of %s not available", tmp->name);
			break;
		case S_IFDIR:
			total += sum_remote_dir(rdir, tmp->name);
			break;
		default:
			break;
		}
	}
	(void) command("CWD %s", parent);

out:
	if (rdir)
		free(rdir);
	if (head)
		free_dirlist(head);
	return (total);
}

/*
 * Called to sum all the sizes from the currently selected local items.
 */
#ifdef USE_PROTOTYPES
int sum_remote_size(void)
#else
int sum_remote_size()
#endif
{
	int		nitems;
	int		row;
	int		total = 0;
	struct dirlist *tmp;
	int		mode;

	status_footer_message("Determining receive total...");
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(base_window.list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++)
		if (xv_get(base_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(base_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			if (non_unix)
				mode = S_IFREG;
			switch (mode) {
			case S_IFDIR:
				total +=
				    sum_remote_dir(remote_dircache.first->name,
				    tmp->name);
				break;
			case S_IFREG:
				if (tmp->size != -1)
					total += tmp->size;
				else
					status_footer_message(
					    "Size of %s not available",
					    tmp->name);
				break;
			case S_IFLNK:
				status_footer_message(
				    "Ignoring symlink %s...", tmp->name);
				log_message(
				    "Can only sum sizes of files and directories.\n");
				break;
			default:
				status_footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message(
				    "Can only transfer files and directories.\n");
				break;
			}
		}
	return (total);
}

#ifdef USE_PROTOTYPES
int sum_remote_batch_size(void)
#else
int sum_remote_batch_size()
#endif
{
	int		nitems;
	int		row;
	int		total = 0;
	struct batchlist *tmp;
	int		mode;

	status_footer_message("Determining receive total...");
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(schedule_window.receive_list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++) {
		tmp = (struct batchlist *)xv_get(schedule_window.receive_list,
			PANEL_LIST_CLIENT_DATA, row);
		mode = tmp->mode & S_IFMT;
		if (non_unix)
			mode = S_IFREG;
		switch (mode) {
		case S_IFDIR:
			total += sum_remote_dir(remote_dircache.first->name,
			    tmp->name);
			break;
		case S_IFREG:
			if (tmp->size != -1)
				total += tmp->size;
			else
				status_footer_message(
				    "Size of %s not available", tmp->name);
			break;
		case S_IFLNK:
			status_footer_message(
			    "Ignoring symlink %s...", tmp->name);
			log_message(
			    "Can only sum sizes of files and directories.\n");
			break;
		default:
			status_footer_message(
			    "Ignoring non-file/directory %s.", tmp->name);
			log_message(
			    "Can only transfer files and directories.\n");
			break;
		}
	}
	return (total);
}

#ifdef USE_PROTOTYPES
int sum_local_batch_size(void)
#else
int sum_local_batch_size()
#endif
{
	int		nitems;
	int		row;
	int		total = 0;
	struct batchlist *tmp;

	status_footer_message("Determining send total...");
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(schedule_window.send_list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++) {
		tmp = (struct batchlist *)xv_get(schedule_window.send_list,
			PANEL_LIST_CLIENT_DATA, row);
		switch (tmp->mode & S_IFMT) {
		case S_IFDIR:
			total += sum_local_dir(local_dircache.first->name,
			    tmp->name);
			break;
		case S_IFREG:
			total += tmp->size;
			break;
		case S_IFLNK:
			status_footer_message(
			    "Ignoring symlink %s...", tmp->name);
			log_message(
			    "Can only sum sizes of files and directories.\n");
			break;
		default:
			status_footer_message(
			    "Ignoring non-file/directory %s.", tmp->name);
			log_message(
			    "Can only transfer files and directories.\n");
			break;
		}

	}
	return (total);
}
