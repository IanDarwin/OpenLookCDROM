#include "ftptool.h"

#pragma ident   "@(#)batch.c 1.4     93/05/25"

#ifdef USE_PROTOTYPES
struct batchlist *new_batchlist(void)
#else
struct batchlist *new_batchlist()
#endif
{
	struct batchlist *tmp;

	tmp = (struct batchlist *)malloc(sizeof (struct batchlist));
	if (tmp == NULL)
		return (NULL);
	bzero((char *)tmp, sizeof (struct batchlist));
	tmp->name = NULL;
	return (tmp);
}

#ifdef USE_PROTOTYPES
struct batchlist *add_batchname(Panel panel_list, char *name, mode_t mode,
	size_t size, char *dir)
#else
struct batchlist *add_batchname(panel_list, name, mode, size, dir)
Panel	panel_list;
char	*name;
mode_t	mode;
size_t		size;
char	*dir;
#endif
{
	struct batchlist *tmp;
	int	nitems;
	char	*fullname;
	Xv_font	entry_font;

	if (!S_ISDIR(mode) && !S_ISREG(mode) && !S_ISLNK(mode))
		return (NULL);
	if (dir != NULL && *name != '/') {
		fullname =
		    (char *)malloc((unsigned int)(strlen(dir)+strlen(name)+2));
		if (fullname == NULL)
			return (NULL);
		strcpy(fullname, dir);
		if (strcmp(dir, "/"))
			strcat(fullname, "/");
		strcat(fullname, name);
	} else {
		fullname = strdup(name);
		if (fullname == NULL)
			return (NULL);
	}

	/* just add to the end */
	if (batchentry_exists(panel_list, fullname) != -1) {
		free(fullname);
		return (NULL);
	}
	tmp = new_batchlist();
	if (tmp == NULL)
		return (NULL);

	nitems = xv_get(panel_list, PANEL_LIST_NROWS);
	tmp->name = fullname;
	tmp->mode = mode;
	tmp->row = nitems;
	tmp->size = size;

	if (S_ISDIR(tmp->mode)) {
		entry_font = bold_list_font;
		sprintf(scratch, "%s/", tmp->name);
	} else {
		entry_font = list_font;
		strcpy(scratch, tmp->name);
	}

	xv_set(panel_list,
		PANEL_LIST_INSERT, nitems,
		PANEL_LIST_STRING, nitems, scratch,
		PANEL_LIST_FONT, nitems, entry_font,
		PANEL_LIST_CLIENT_DATA, nitems, tmp,
		PANEL_PAINT, PANEL_NONE,
		NULL);

	panel_paint(panel_list, PANEL_CLEAR);
	return (tmp);
}

#ifdef USE_PROTOTYPES
void free_batchlist(Panel panel_list, int only_selected)
#else
void free_batchlist(panel_list, only_selected)
Panel	panel_list;
int		only_selected;
#endif
{
	int	nitems, row;
	struct batchlist *tmp;

	nitems = xv_get(panel_list, PANEL_LIST_NROWS);
	for (row = nitems - 1; row >= 0; row--) {
		if (only_selected &&
			(xv_get(panel_list, PANEL_LIST_SELECTED, row) == FALSE))
			continue;
		tmp = (struct batchlist *)xv_get(panel_list,
			PANEL_LIST_CLIENT_DATA, row);
		if (only_selected)
			if (panel_list == schedule_window.receive_list)
				nreceiveitems--;
			else
				nsenditems--;
		free(tmp->name);
		free((char *)tmp);
		xv_set(panel_list,
			PANEL_LIST_DELETE, row,
			PANEL_PAINT, PANEL_NONE,
			NULL);
	}
	panel_paint(panel_list, PANEL_CLEAR);
}

#ifdef USE_PROTOTYPES
int batchentry_exists(Panel panel_list, char *name)
#else
int batchentry_exists(panel_list, name)
Panel	panel_list;
char	*name;
#endif
{
	int	nitems, row;
	struct batchlist *tmp;

	nitems = xv_get(panel_list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++) {
		tmp = (struct batchlist *)xv_get(panel_list,
			PANEL_LIST_CLIENT_DATA, row);
		if (!strcmp(tmp->name, name))
			return (row);
	}
	return (-1);
}

#ifdef USE_PROTOTYPES
void receive_list_delete_proc(void)
#else
void receive_list_delete_proc()
#endif
{
	free_batchlist(schedule_window.receive_list, 1);
}

#ifdef USE_PROTOTYPES
void send_list_delete_proc(void)
#else
void send_list_delete_proc()
#endif
{
	free_batchlist(schedule_window.send_list, 1);
}

#ifdef USE_PROTOTYPES
void dobatchget(void)
#else
void dobatchget()
#endif
{
	int 	nitems, row;
	struct batchlist *tmp;
	int		rval;
	int		mode;
	int		dirchanged = 0;

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (ping_server())
		goto out;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	init_status(sum_remote_batch_size());
	settype((int)xv_get(host_window.advanced.transfer_mode, PANEL_VALUE));
	/* loop over each element, and do a get, then unselect */
	nitems = xv_get(schedule_window.receive_list, PANEL_LIST_NROWS);
	for (row = nitems - 1; row >= 0; row--) {
		tmp = (struct batchlist *)xv_get(schedule_window.receive_list,
			PANEL_LIST_CLIENT_DATA, row);
		mode = tmp->mode & S_IFMT;
		if (non_unix) {
			/* we can't transfer whole dirs on non-unix machines */
			mode = S_IFREG;
		}
		switch (mode) {
		case S_IFDIR:
			if ((rval = get_dir(remote_dircache.first->name,
				local_dircache.first->name, tmp->name,
				tmp->name + 1)) != 0) {
				/* XXX? - fail or property here */
			}
			dirchanged++;
			break;
		case S_IFREG:
			if ((rval = make_dirs(tmp->name + 1, 0)) != 0) {
			/* XXX? - fail or property here */
			/*
				goto out;
			*/
			}
			if (get_file(tmp->name, tmp->name + 1, tmp->size))
				goto out;
			dirchanged++;
			break;
		case S_IFLNK:
			if ((rval = make_dirs(tmp->name + 1, 0)) != 0) {
			/* XXX? - fail or property here */
			/*
				goto out;
			*/
			}
			rval = get_file(tmp->name, tmp->name + 1, tmp->size);
			if (rval == EPERM) {
				if ((rval = get_dir(remote_dircache.first->name,
					local_dircache.first->name,
					tmp->name, tmp->name + 1)) != 0) {
				/* XXX? - fail or property here */
				}
			} else if (rval != 0) {
				goto out;
			}
			dirchanged++;
			break;
		default:
			footer_message("Ignoring non-file/directory %s.",
			    tmp->name);
			log_message("Can only transfer files.\n");
			break;
		}
		free(tmp->name);
		free((char *)tmp);
		xv_set(schedule_window.receive_list,
			PANEL_LIST_DELETE, row,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		panel_paint(schedule_window.receive_list, PANEL_CLEAR);
	}

out:
	if (dirchanged)
		change_local_dir(".", 1);
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, FALSE,
			NULL);
	update_status_label("Not", "transferring", (size_t)0);
	end_status();
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void dobatchput(void)
#else
void dobatchput()
#endif
{
	int 	nitems, row;
	struct batchlist *tmp;
	int		rval = 0;
	int		mode;
	int		dirchanged = 0;

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (ping_server())
		goto out;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	init_status(sum_local_batch_size());
	settype((int)xv_get(host_window.advanced.transfer_mode, PANEL_VALUE));

	/* loop over each element, and do a get, then unselect */
	nitems = xv_get(schedule_window.send_list, PANEL_LIST_NROWS);
	for (row = nitems-1; row >= 0; row--) {
		tmp = (struct batchlist *)xv_get(schedule_window.send_list,
			PANEL_LIST_CLIENT_DATA, row);
		mode = tmp->mode & S_IFMT;
		if (non_unix) {
			/* we can't transfer whole dirs on non-unix machines */
			mode = S_IFREG;
		}
		switch (mode) {
		case S_IFDIR:
			dirchanged++;
			if ((rval = put_dir(remote_dircache.first->name,
				local_dircache.first->name, tmp->name + 1,
				tmp->name)) != 0) {
				/* XXX? - fail or property here */
				/*
				goto out;
				*/
			}
			break;
		case S_IFREG:
			if ((rval = make_remote_dirs(tmp->name + 1, 0)) != 0) {
			/* XXX? - fail or property here */
			/*
				goto out;
			*/
			}
			dirchanged++;
			if ((rval = put_file(tmp->name, tmp->name+1,
			    tmp->size)) != 0)
				goto out;
			break;
		case S_IFLNK:
			if ((rval = make_remote_dirs(tmp->name + 1, 0)) != 0) {
			/* XXX? - fail or property here */
			/*
				goto out;
			*/
			}
			dirchanged++;
			rval = put_file(tmp->name, tmp->name+1, tmp->size);
			if (rval == EPERM) {
				if ((rval = put_dir(remote_dircache.first->name,
					local_dircache.first->name,
					tmp->name + 1, tmp->name)) != 0) {
					/* XXX? - fail or property here */
					/*
					goto out;
					*/
				}
			} else if (rval != 0) {
				goto out;
			}
			break;
		default:
			local_footer_message("Ignoring non-file/directory %s.",
			    tmp->name);
			log_message("Can only transfer files.\n");
			break;
		}
		free(tmp->name);
		free((char *)tmp);
		xv_set(schedule_window.send_list,
			PANEL_LIST_DELETE, row,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		panel_paint(schedule_window.send_list, PANEL_CLEAR);
	}


out:
	if (dirchanged)
		change_remote_dir(".", 1);
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, FALSE,
			NULL);
	update_status_label("Not", "transferring", (size_t)0);
	end_status();
	xfer_buttons_active();
	if (rval == ENOSPC) {
		disconnect();
	}
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
int save_batch_list(Panel list, char *filename)
#else
int save_batch_list(list, filename)
Panel	list;
char	*filename;
#endif
{
	FILE	*fp;
	int	isdir;
	extern char *sys_errlist[];
	int		nitems, row;
	struct batchlist *tmp;

	if ((fp = fopen(filename, "w")) == NULL) {
		footer_message("%s:%s", filename, sys_errlist[errno]);
		return (1);
	}
	/* save */
	fprintf(fp, "#name:isdir:size\n");
	nitems = xv_get(list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++) {
		tmp = (struct batchlist *)xv_get(list,
			PANEL_LIST_CLIENT_DATA, row);
		isdir = 0;
		if (S_ISDIR(tmp->mode))
			isdir = 1;
		fprintf(fp, "%s:%d:%d\n", tmp->name, isdir, tmp->size);
	}
	fclose(fp);
	return (0);
}

#ifdef USE_PROTOTYPES
int load_batch_list(Panel list, char *filename)
#else
int load_batch_list(list, filename)
Panel	list;
char	*filename;
#endif
{
	char	fname[MAXPATHLEN+1];
	int	isdir;
	size_t		size;
	mode_t		mode;
	int	ch;
	FILE	*fp;
	extern char *sys_errlist[];

	if ((fp = fopen(filename, "r")) == NULL) {
		footer_message("%s:%s", filename, sys_errlist[errno]);
		return (1);
	}
	if (list == schedule_window.receive_list)
		nreceiveitems = 0;
	else
		nsenditems = 0;
	free_batchlist(list, 0);

	/* load */
	/*
	 * Alias
	 * host
	 * login
	 * encrypted password
	 */

	for (;;) {
		ch = getc(fp);
		if (ch == EOF)
			break;
		if (ch == '#') {
			while (((ch = getc(fp)) != '\n') && (ch != EOF))
				/* null */;
			continue;
		} else
			ungetc(ch, fp);
		if (fscanf(fp, "%[^:]:%d:%d\n", fname, &isdir, &size) != 3)
			break;
		mode = S_IFREG;
		if (isdir)
			mode = S_IFDIR;
		if (add_batchname(list, fname, mode, size,
		    (char *)NULL) == NULL) {
			fclose(fp);
			return (1);
		}
	}
	fclose(fp);
	return (0);
}
