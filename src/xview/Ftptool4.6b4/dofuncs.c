#include "ftptool.h"

#pragma ident   "@(#)dofuncs.c 1.8     93/08/10"

#ifdef USE_PROTOTYPES
int	doconnect(void)
#else
int	doconnect()
#endif
{
	char	*ftphost;
	char	*proxyhost;
	char	*login;
	char	*password;
	char	*account;
	char	crap[50];
	int		rval;
	int		port;

#ifdef notdef
	if (!openlook_mode || (xv_get(host_window.frame, FRAME_CMD_PIN_STATE)
		== FRAME_CMD_PIN_OUT)) {
		xv_set(host_window.frame,
			XV_SHOW, FALSE,
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
			NULL);
	}
#endif
	textsw_reset(session_window.log, 0, 0);
	/* used to make sure we are connected */
	/* with PANEL_INACTIVE, don't need to anymore */
	footer_message("");
	local_footer_message("");
	password = (char *)xv_get(host_window.basic.password, PANEL_VALUE);
	if (*password == '\0')
		password = anonftp_password;

	login = (char *)xv_get(host_window.basic.login, PANEL_VALUE);
	if (*login == '\0') {
		login = "anonymous";
	}

	account = (char *)xv_get(host_window.basic.account, PANEL_VALUE);
	if (*account == '\0') {
		account = NULL;
	}

	ftphost = (char *)xv_get(host_window.basic.host, PANEL_VALUE);
	port = ftp_port;
	ftphost = parse_hostname(ftphost, &port);
	if (ftphost == NULL)
		return (-1);
	rval = openhost(ftphost, login, password, account, port);
	if (rval != 1)
		return (rval);
	if (!try_proxy)
		return (1);
	proxyhost = (char *)xv_get(host_window.advanced.proxy, PANEL_VALUE);
	/* crap for Iftp */
	/* login must be 'user@ftphost' */
	sprintf(crap, "%s@%s", login, ftphost);
	return (openhost(proxyhost, crap, password, account,
	    ftp_passthru_port));
}

#ifdef USE_PROTOTYPES
int openhost(char *ftphost, char *login, char *password,
	char *account, short port)
#else
int openhost(ftphost, login, password, account, port)
char	*ftphost;
char	*login;
char	*password;
char	*account;
short	port;
#endif
{
	char	*atsign;
	char	*auto_cd;
	int		rval = 0;

	timedout = 0;
	cursor_busy();
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(base_window.connect,
		PANEL_INACTIVE, TRUE,
		NULL);

	xv_set(host_window.basic.host,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(host_window.advanced.proxy,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(host_window.basic.login,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(host_window.basic.password,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(host_window.advanced.remote_auto_cd,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(host_window.advanced.local_auto_cd,
		PANEL_READ_ONLY, TRUE,
		NULL);

	xv_set(local_window.list,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, TRUE,
		NULL);

	xv_set(base_window.list,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(base_window.directory,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(tool_property_window.category,
		PANEL_INACTIVE, TRUE,
		NULL);

	start_busy_cycle();
	footer_message("Connecting...");
	rval = ftp_hookup(ftphost, port);
	switch (rval) {
	case 0:	/* error */
		rval = 2;
		goto out;
		break;
	case 1:
		/* couldn't resolve hostname: try proxy */
		goto out;
		break;
	default:
		rval = 0;
		break;
	}
	if ((rval = ftp_login(login, password, account)) == 0) {
		rval = 2;
		goto out;
	}
	footer_message("Initializing...");

	connected = 1;
	which_up_cmd = -1;

	xv_set(schedule_window.process,
		PANEL_INACTIVE, TRUE,
		NULL);

	update_timestamp();

	(void) strncpy(icon_label, (char *)xv_get(host_window.basic.host,
		PANEL_VALUE), 8);
	icon_label[8] = '\0';

	xv_set(frame_icon,
		ICON_LABEL, icon_label,
		NULL);

	xv_set(base_window.directory,
		PANEL_INACTIVE, FALSE,
		NULL);

	/*
	 * ftphost is not the name of outside machines.
	 * Need to extract outside name from login
	 */
	if ((atsign = index(login, '@')) != NULL)
		sprintf(scratch, "%s - %s", header_name, atsign + 1);
	else
		sprintf(scratch, "%s - %s", header_name, ftphost);
	xv_set(base_window.frame,
		XV_LABEL, scratch,
		NULL);


	remote_os_type = (int)xv_get(host_window.advanced.os_type, PANEL_VALUE);

	if (remote_os_type == REMOTE_OS_OTHER) {
		other_dir_pattern =
		    (char *)xv_get(host_window.advanced.dir_parse,
			PANEL_VALUE);
		if (*other_dir_pattern == '\0') {
			footer_message(
			    "No DIR template specified. Defaulting to UNIX.");
			remote_os_type = REMOTE_OS_UNIX;
			other_dir_pattern = NULL;
		} else if ((other_dir_pattern =
		    dir_parse_to_pattern(other_dir_pattern)) == NULL) {
			footer_message("Defaulting to UNIX.");
			remote_os_type = REMOTE_OS_UNIX;
		} else if (other_dir_pattern[0] == (char)NONUNIX) {
			non_unix = 1;
			remote_sort_mode = SORTBYNAME;
			xv_set(tool_property_window.directory_lists.remote_sort,
				PANEL_VALUE, remote_sort_mode,
				PANEL_INACTIVE, TRUE,
				NULL);
			set_remote_sort_order(SORTBYNAME);
		}
	}

	auto_cd = (char *)xv_get(host_window.advanced.remote_auto_cd,
	    PANEL_VALUE);
	if (*auto_cd == '\0' || change_remote_dir(auto_cd, 0))
		change_remote_dir(".", 0);
	footer_message("");
	auto_cd = (char *)xv_get(host_window.advanced.local_auto_cd,
	    PANEL_VALUE);
	if (*auto_cd != '\0' && strcmp(auto_cd, ".") != 0)
		change_local_dir(auto_cd, 0);
	local_footer_message("");
	change_local_list_menu();

	end_busy_cycle();
	cursor_normal();
	idle_timer_on();

	/* activate buttons */

	/* connect already inactive */
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, FALSE,
		PANEL_LABEL_STRING, "Disconnect",
		XV_HELP_DATA, "ftptool:DisconnectButton",
		NULL);

	xv_set(base_window.connect,
		PANEL_INACTIVE, FALSE,
		PANEL_LABEL_STRING, "Disconnect",
		XV_HELP_DATA, "ftptool:DisconnectButton",
		NULL);

	xv_set(host_window.basic.host,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.basic.login,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.basic.password,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.proxy,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.remote_auto_cd,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.local_auto_cd,
		PANEL_READ_ONLY, FALSE,
		NULL);

	xv_set(local_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(base_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(tool_property_window.category,
		PANEL_INACTIVE, FALSE,
		NULL);

	return (0);
out:
	end_busy_cycle();
	cursor_normal();
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.connect,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(host_window.basic.host,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.basic.login,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.basic.password,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.proxy,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.remote_auto_cd,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(host_window.advanced.local_auto_cd,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(base_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(local_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(base_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(tool_property_window.category,
		PANEL_INACTIVE, FALSE,
		NULL);
	return (rval);
}

#ifdef USE_PROTOTYPES
void	doget(void)
#else
void	doget()
#endif
{
	int 	nitems, row;
	struct dirlist *tmp;
	int		rval;
	int		mode;
	char	*name = NULL;
	int		dirchanged = 0;

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (dirwasmodified()) {
	/*
		goto out;
	*/
	}
	if (ping_server())
		goto out;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	init_status(sum_remote_size());
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(base_window.list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++)
		if (xv_get(base_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(base_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			if (non_unix) {
				/*
				 * cannot transfer whole directories
				 * on non-unix machines
				 */
				mode = S_IFREG;
			}
			switch (mode) {
			case S_IFDIR:
				dirchanged++;
				if ((rval = get_dir(remote_dircache.first->name,
					local_dircache.first->name,
					tmp->name, tmp->name)) != 0) {
					if (rval == EEXIST) {
						footer_message(
						    "%s already exists.",
						    tmp->name);
					}
					goto out;
				}
				remote_list_ndirs--;
				break;
			case S_IFREG:
				dirchanged++;
				if (get_file(tmp->name, tmp->name, tmp->size))
					goto out;
				remote_list_nfiles--;
				break;
			case S_IFLNK:
				name = linkname(tmp->name);
				if (name == NULL)
					goto out;
				/* try as a file */
				dirchanged++;
				rval = get_file(name, name, -1);
				if (rval == EISDIR) {
					/* try as a directory */
					if ((rval =
					    get_dir(remote_dircache.first->name,
						local_dircache.first->name,
						name, name)) > 1) {
						/*
						if (rval == EEXIST) {
						}
						 */
						goto out;
					}
				} else if (rval != 0) {
					goto out;
				}
				free(name);
				name = NULL;
				remote_list_nfiles--;
				break;
			default:
				remote_list_nothers--;
				footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message("Can only transfer files.\n");
				break;
			}
			xv_set(base_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			remote_show_items();
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
	if (name)
		free(name);
	change_remote_list_menu();
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void	doput(void)
#else
void	doput()
#endif
{
	int 	nitems, row;
	struct dirlist *tmp;
	int		rval = 0;
	int		mode;
	char	*name = NULL;
	int		dirchanged = 0;

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (dirwasmodified()) {
		goto out;
	}
	if (ping_server())
		goto out;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	init_status(sum_local_size());
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			if (non_unix) {
				/*
				 * cannot transfer whole directories
				 * on non-unix machines
				 */
				mode = S_IFREG;
			}
			switch (mode) {
			case S_IFDIR:
				dirchanged++;
				if ((rval =
				    put_dir(remote_dircache.first->name,
					local_dircache.first->name, tmp->name,
					tmp->name)) != 0) {
					goto out;
				}
				local_list_ndirs--;
				break;
			case S_IFREG:
				dirchanged++;
				if ((rval = put_file(tmp->name, tmp->name,
				    tmp->size)) != 0)
					goto out;
				local_list_nfiles--;
				break;
			case S_IFLNK:
				name = linkname(tmp->name);
				if (name == NULL)
					goto out;
				/* try as a file */
				dirchanged++;
				rval = put_file(name, name, -1);
				if (rval == EPERM) {
					if ((rval =
					    put_dir(remote_dircache.first->name,
						local_dircache.first->name,
						name, name)) != 0) {
						goto out;
					}
				} else if (rval != 0) {
					goto out;
				}
				local_list_nfiles--;
				free(name);
				name = NULL;
				break;
			default:
				local_list_nothers--;
				local_footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message("Can only transfer files.\n");
				break;
			}
			xv_set(local_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			local_show_items();
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
	if (name)
		free(name);
	change_local_list_menu();
	xfer_buttons_active();
	if (rval == ENOSPC) {
		disconnect();
	}
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void	dofileop(int which)
#else
void	dofileop(which)
int		which; /* uncompress or extract */
#endif
{
	int 	nitems, row;
	struct dirlist *tmp;
	int		mode;
	int		dirchanged = 0;
	char	*name = NULL;
	struct extension_info *ext_info;
	struct stat buf;
	extern char	*sys_errlist[];

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (dirwasmodified()) {
	/*
		goto out;
	*/
	}
	if (which == DOGETTARFILENAME) {
		xv_set(tar_frame,
			XV_SHOW, TRUE,
			NULL);
		goto out;
	} else if (which == DOTAR) {
		create_tar_file();
		goto out;
	}
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	/* loop over each selected element, and do a get, then unselect */
	for (row = 0; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			/* perhaps should recursively get directories? */
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			if (non_unix) {
				/*
				 * cannot do anything to whole directories
				 * on non-unix machines
				 */
				mode = S_IFREG;
			}
			switch (mode) {
			case S_IFDIR:
				local_footer_message("Ignoring directory.");
				local_list_ndirs--;
				break;
			case S_IFLNK:
			case S_IFREG:
				if (S_ISLNK(mode))
					name = linkname(tmp->name);
				else
					name = strdup(tmp->name);
				if (name == NULL) {
					fprintf(stderr, "Out of memory.\n");
					goto out;
				}
				if (stat(name, &buf) == -1) {
					local_footer_message(
					    "%s: %s.", name,
					    sys_errlist[errno]);
					goto out;
				}
				if (!S_ISREG(buf.st_mode)) {
					local_footer_message(
					    "%s is not a regular file.",
					    name);
					goto out;
				}
				switch (which) {
				case DOUNCOMPRESS:
					if (iscompressed(name)) {
						local_footer_message(
						    "Uncompressing %s.",
						    name);
						uncompress(name);
					} else
						local_footer_message(
						    "%s not compressed.\n",
						    name);
					break;
				case DOEXTRACT:
					if (iscompressed(name)) {
						char *dot;

						local_footer_message(
						    "Uncompressing %s.",
						    name);
						uncompress(name);
						dot = rindex(name, '.');
						if (dot) {
							*dot = '\0';
						}
					}
					ext_info = type_by_extension(name);
					if (ext_info &&
					    !strcmp(ext_info->extension,
					    ".tar")) {
						/* will do tar viewer */
						start_viewer(name, 0);
					} else {
						local_footer_message(
						    "%s is not a tar file.",
						    name);
					}
					break;
				case DOCOMPRESS:
					if (!iscompressed(name)) {
						local_footer_message(
						    "Compressing %s.", name);
						compress(name);
					} else {
						local_footer_message(
						    "%s already compressed.",
						    name);
					}
					break;
				}
				free(name);
				name = NULL;
				local_list_nfiles--;
				dirchanged++;
				break;
			default:
				local_list_nothers--;
				local_footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message("Ignoring non-file.\n");
				break;
			}
			xv_set(local_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			local_show_items();
		}

out:
	if (name)
		free(name);
	change_local_list_menu();
	if (dirchanged)
		change_local_dir(".", 1);
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;

}

#ifdef USE_PROTOTYPES
void	doview(int which)
#else
void	doview(which)
int		which; /* local or remote */
#endif
{
	int 	nitems, row;
	struct dirlist *tmp;
	Panel	list_panel;
	int		mode;
	int		dirchanged = 0;
	char	*name = NULL;

	xfer_buttons_inactive();
	abort_transfer = 0;
	if (dirwasmodified()) {
	/*
		goto out;
	*/
	}
	if (which == DOREMOTEVIEW) {
		list_panel = base_window.list;
		if (ping_server())
			goto out;
		if (show_status)
			xv_set(status_window.frame,
				XV_SHOW, TRUE,
				NULL);
		init_status(sum_remote_size());
	} else
		list_panel = local_window.list;
	/* loop over each selected element, and do a get, then unselect */
	nitems = xv_get(list_panel, PANEL_LIST_NROWS);
	for (row = 0; row < nitems; row++)
		if (xv_get(list_panel, PANEL_LIST_SELECTED, row)) {
			/* perhaps should recursively get directories? */
			tmp = (struct dirlist *)xv_get(list_panel,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			if (non_unix) {
				/*
				 * can't do anything to whole directories
				 * on non-unix machines
				 */
				mode = S_IFREG;
			}
			switch (mode) {
			case S_IFDIR:
				if (which == DOREMOTEVIEW) {
					footer_message(
					    "Cannot View a directory.");
					remote_list_ndirs--;
				} else {
					local_footer_message(
					    "Can't View a directory.");
					local_list_ndirs--;
				}
				break;
			case S_IFLNK:
			case S_IFREG:
				if (S_ISLNK(mode))
					name = linkname(tmp->name);
				else
					name = strdup(tmp->name);
				if (name == NULL) {
					fprintf(stderr, "Out of memory.\n");
					goto out;
				}
				if (which == DOREMOTEVIEW) {
					if (view_remote_file(name, tmp->size))
						goto out;
					remote_list_nfiles--;
				} else {
					if (view_local_file(name, which,
					    &dirchanged))
						goto out;
					local_list_nfiles--;
				}
				free(name);
				name = NULL;
				break;
			default:
				log_message("Can only transfer files.\n");
				if (which == DOREMOTEVIEW) {
					footer_message(
					    "Ignoring non-file/directory %s.",
					    tmp->name);
					remote_list_nothers--;
				} else {
					local_footer_message(
					    "Ignoring non-file/directory %s.",
					    tmp->name);
					local_list_nothers--;
				}
				break;
			}
			xv_set(list_panel,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			local_show_items();
			remote_show_items();
		}

out:
	if (name)
		free(name);
	if (which == DOREMOTEVIEW) {
		change_remote_list_menu();
		if (show_status)
			xv_set(status_window.frame,
				XV_SHOW, FALSE,
				NULL);
	} else
		change_local_list_menu();
	update_status_label("Not", "transferring", (size_t)0);
	end_status();
	if (dirchanged)
		change_local_dir(".", 1);
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void create_tar_file(void)
#else
void create_tar_file()
#endif
{
	int 	nitems, row;
	char	*filename = 0;
	struct dirlist *tmp;
	char	**argv;
	char	*slash;
	int		ix;


	xfer_buttons_inactive();

	if (dirwasmodified()) {
	/*
		goto out;
	*/
	}

	filename = (char *)xv_get(tar_text, PANEL_VALUE);
	if (*filename == '\0') {
		local_footer_message("Type in a name for the tar file.");
		goto out;
	}

	local_footer_message("Creating tar file %s.", filename);
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	/* slightly different - take ALL seletctions and put them in a */
	/* tar file */
	if ((argv = (char **)malloc((unsigned int)((3+nitems+1) *
	    sizeof (char *)))) == NULL) {
		fprintf(stderr, "Out of memory.\n");
		goto out;
	}
	ix = 3;
	for (row = 0; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			if (!S_ISDIR(tmp->mode) && !S_ISREG(tmp->mode) &&
			    !S_ISLNK(tmp->mode))
				continue;
			argv[ix] = strdup(tmp->name);
			if (argv[ix] == NULL) {
				fprintf(stderr, "Out of memory.\n");
				for (ix--; ix > 2; ix--)
					free(argv[ix]);
				free((char *)argv);
				goto out;
			}
			slash = index(argv[ix], '/');
			if (slash)
				*slash = '\0';
			ix++;
			if (S_ISDIR(tmp->mode))
				local_list_ndirs--;
			else
				local_list_nfiles--;
			xv_set(local_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			local_show_items();
		}
	argv[0] = "tar";
	argv[1] = "cvf";
	argv[2] = filename;
	argv[ix] = NULL;
	if (ix == 2)
		goto out;
	pipe_program(argv);
	xv_set(local_window.list,
		XV_SHOW, TRUE,
		NULL);
	for (ix--; ix > 2; ix--)
		free(argv[ix]);
	free((char *)argv);
	change_local_dir(".", 1);
out:
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void	doremotecd(int force)
#else
void	doremotecd(force)
int		force;
#endif
{
	if (which_remote_file == NULL)
		return;
	xfer_buttons_inactive();
	change_remote_dir(which_remote_file, force);
	free(which_remote_file);
	which_remote_file = NULL;
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
}
