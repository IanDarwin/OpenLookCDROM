#include "ftptool.h"

#pragma ident   "@(#)change_dir.c 1.7     93/11/05"

int dirlastmtime;

#ifdef USE_PROTOTYPES
int	checkdir(char *dirname)
#else
int	checkdir(dirname)
char	*dirname;
#endif
{
	struct stat buf;

	if (lstat(dirname, &buf) == -1) {
		if (errno == ENOENT)
			return (ENOENT);
		return (1);
	}

	if (buf.st_mtime > local_dircache.first->mtime) {
		return (1);
	}
	return (0);
}

#ifdef USE_PROTOTYPES
int dirwasmodified(void)
#else
int dirwasmodified()
#endif
{
	int	error;
	char	*dirname = local_dircache.first->name;

	if (cache_lookup(&local_dircache, dirname) == NULL)
		return (0);
	/* first now */

	error = checkdir(dirname);
	if (error == ENOENT) {
		if (xv_get(local_window.frame, XV_SHOW) == FALSE) {
			xv_set(local_window.frame,
				XV_SHOW, TRUE,
				NULL);
		}
		local_footer_message(
		    "Current directory was deleted. Changing to /tmp.");
		clear_slist(local_window.list);
		xv_set(local_window.list,
			XV_SHOW, TRUE,
			NULL);
		dircache_delete(&local_dircache, dirname);
		change_local_dir("/tmp", 0);
		return (1);
	} else if (error != 0) {
		local_footer_message(
		    "Current directory was modified. Rereading.");
		if (xv_get(local_window.frame, XV_SHOW) == FALSE) {
			xv_set(local_window.frame,
				XV_SHOW, TRUE,
				NULL);
		}
		clear_slist(local_window.list);
		xv_set(local_window.list,
			XV_SHOW, TRUE,
			NULL);
		dircache_delete(&local_dircache, dirname);
		change_local_dir(dirname, 0);
		return (1);
	}
	return (0);
}

#ifdef USE_PROTOTYPES
int change_local_dir(char *s, int force)
#else
int change_local_dir(s, force)
char	*s;
int		force;
#endif
{
	static char	cld[MAXPATHLEN + 2];
	extern char *sys_errlist[];
	struct dirlist *head = NULL;
	int		wasinactive;
	int		rval = 0;
	int		error;
	char	*dir = NULL;
#ifndef	ultrix
#if defined(SYSV) || defined(SYSV386)
	struct statvfs fsbuf;
#else
	struct statfs fsbuf;
#endif
#else
	struct	fs_data	fsbuf;
#endif

	wasinactive = xv_get(local_window.list, PANEL_INACTIVE);
	if (wasinactive == FALSE) {
		xv_set(local_window.list,
			PANEL_INACTIVE, TRUE,
			NULL);
	}
	cursor_busy();
	local_footer_message("Reading directory...");
	local_right_footer_message("");
	dir = expand_dirname(s);
	if (dir == NULL) {
		fprintf(stderr, "out of memory\n");
		goto out;
	}
	if (chdir(dir) == -1) {
		if (errno == ENOENT) {
			if ((rval = ask_make_dir(dir)) != 0) {
				if (rval == -1) {
					local_footer_message("");
					/* user cancelled. */
					goto out;
				}
				local_footer_message(
				    "Could not make directory: %s",
				    sys_errlist[rval]);
				goto out;
			}
			if (chdir(dir) == -1) {
				rval = errno;
				local_footer_message(
				    "Could not change to directory: %s",
				    sys_errlist[rval]);
				goto out;
			}
		} else {
			rval = errno;
			local_footer_message(
			    "Could not change to directory: %s",
			    sys_errlist[rval]);
			goto out;
		}
	}

	clear_slist(local_window.list);

	local_list_ndirs = 0;
	local_list_nfiles = 0;
	local_list_nothers = 0;
	change_local_list_menu();

	if (getcwd(cld, sizeof (cld)) == NULL) {
		/* Failure */
		goto out;
	}

	xv_set(local_window.directory,
		PANEL_VALUE, cld,
		NULL);

#if defined(SYSV) || defined(SYSV386)
	if (statvfs(cld, &fsbuf) == -1) {
		sprintf(scratch, "statvfs failed: %s", sys_errlist[errno]);
	}
#else
	if (statfs(cld, &fsbuf) == -1) {
		sprintf(scratch, "statfs failed: %s", sys_errlist[errno]);
	}
#endif
	else {
#ifndef	ultrix
		sprintf(scratch, "%d Kbytes (%d%% free)",
			(int)(fsbuf.f_bsize / 1024.0 * fsbuf.f_bavail),
			(int)(100.0 * fsbuf.f_bavail /
			    (fsbuf.f_blocks -
				(fsbuf.f_bfree - fsbuf.f_bavail))));
#else
		sprintf(scratch, "%d Kbytes (%d%% free)",
			(int)(fsbuf.fd_req.bfreen),
			(int)(100.0 * fsbuf.fd_req.bfreen /
			    (fsbuf.fd_req.btot -
				(fsbuf.fd_req.bfree - fsbuf.fd_req.bfreen))));
#endif
	}
	xv_set(local_window.space,
		PANEL_LABEL_STRING, scratch,
		NULL);

	if (force)
		dircache_delete(&local_dircache, cld);

	head = cache_lookup(&local_dircache, cld);
	if (head == NULL) {
		/* cache miss */
		head = read_local_dir(cld);
		if (head == NULL) {
			fprintf(stderr, "Out of memory\n");
			rval = 1;
			goto out;
		}
		/* add to cache */
		dircache_add(&local_dircache, cld, head);
		local_dircache.first->mtime = dirlastmtime;
	} else if ((error = checkdir(cld)) != 0) {
		dircache_delete(&local_dircache, cld);
		if (error == ENOENT) {
			local_footer_message("%s does not exist.", cld);
			goto out;
		}
		/* else reread */
		head = read_local_dir(cld);
		if (head == NULL) {
			fprintf(stderr, "Out of memory\n");
			rval = 1;
			goto out;
		}
		/* add to cache */
		dircache_add(&local_dircache, cld, head);
		local_dircache.first->mtime = dirlastmtime;
	}


	dirlist_to_slist(local_window.list, head);

	local_show_items();
	local_footer_message("");
out:
	cursor_normal();

	if (dir)
		free(dir);
	if (wasinactive == FALSE) {
		xv_set(local_window.list,
			PANEL_INACTIVE, FALSE,
			NULL);
	}
	XFlush(dpy);
	return (rval);
}

#ifdef USE_PROTOTYPES
int change_remote_dir(char *s, int force)
#else
int change_remote_dir(s, force)
char	*s;
int	force;
#endif
{
	char	*ftperr;
	char	crd[MAXPATHLEN + 1];
	struct dirlist *head = NULL;
	int		wasinactive;
	char	*dir = NULL;
	int	rval = 0;
	int	pwdfailed = 0;
	char	*quote;

	wasinactive = xv_get(base_window.list, PANEL_INACTIVE);
	if (wasinactive == FALSE) {
		xv_set(base_window.list,
			PANEL_INACTIVE, TRUE,
			NULL);
	}
	if (ping_server())
		goto out;
	cursor_busy();
	/* send cd command */
	footer_message("Reading directory...");
	right_footer_message("");
	dir = strdup(s);
	if (dir == NULL)
		goto out;
	if (!strcmp(dir, "..")) {
		if (up_one_level())
			goto out;
	} else if (strcmp(dir, ".")) {
		extern int code;

		code = -1;
		command("CWD %s", dir);
		/* success */
		/*
		 * 250 CWD command successful.
		 * 200 CWD command successful (OS9)
		 */
		/* failure */
		/*
		 * 451 error changing directory to <name>: 000:214 {000:###} (OS9) 
		 * 550 k: No such file or directory
		 */
		if (code == 451) {
			ftperr = ftp_error(' ', response_line);
			footer_message("Error changing to %s. (Not a Directory)", dir);
			rval = ENOTDIR;
			goto out;
		}

		if (code == 550) {
			ftperr = index(response_line, ':');
			if (ftperr == NULL) {
				rval = 1;
				goto out;
			}
			if (!strncmp(ftperr, ": Not a directory.", 18)) {
				footer_message("%s is not a directory.", dir);
				rval = ENOTDIR;
				goto out;
			}
			if ((rval = ask_make_remote_dir(dir)) != 0) {
				if (rval == -1) {
					footer_message("");
					/* user cancelled */
					goto out;
				}
				footer_message("Could not make directory.");
				goto out;
			}
			command("CWD %s", dir);
			if (code == 550) {
				sprintf(scratch,
				    "%s: No such file or directory.", dir);
				ftperr = ftp_error(' ', scratch);
				footer_message(ftperr);
				rval = 1;
				goto out;
			}
		}
	}

	clear_slist(base_window.list);

	remote_list_ndirs = 0;
	remote_list_nfiles = 0;
	remote_list_nothers = 0;

	change_remote_list_menu();

	/* set current directory */
	if (command("PWD") == ERROR && code == 500) {
		/*
		footer_message("pwd not recognized.");
		*/
		/* try quote xpwd */
		if (command("XPWD") == ERROR && code == 500) {
			footer_message("pwd and xpwd not recognized.");
			pwdfailed = 1;
		}
	}
	/* response */
	/*
	 * 25[17] "/" is current directory.
	 * 257 PWD: "/Print_Output" is current directory.
	 *
	 * Skip to first double-quote, since they seem to have that.
	 */
	crd[0] = '\0';
	if (pwdfailed) {
		force = 1;
	} else if (!strncmp(response_line, "257", 3) ||
	    !strncmp(response_line, "251", 3)) {
		quote = strchr(response_line, '\"');
		if (quote != NULL)
			sscanf (quote, "\"%[^\"]\"", crd);
	} else {
		footer_message("pwd or xpwd returned bad response.");
		force = 1;
	}
	xv_set(base_window.directory,
		PANEL_VALUE, crd,
		NULL);

	if (force)
		dircache_delete(&remote_dircache, crd);
	head = cache_lookup(&remote_dircache, crd);
	if (head == NULL) {
		/* cache miss */
		head = read_remote_dir();
		if (head == NULL) {
			rval = ETIMEDOUT;
			goto out;
		}
		dircache_add(&remote_dircache, crd, head);
	}


	dirlist_to_slist(base_window.list, head);

	remote_show_items();
out:
	if (rval == 0)
		footer_message("");
	cursor_normal();
	if (dir)
		free(dir);
/*
	if (head)
		free_dirlist(head);
*/
	if (wasinactive == FALSE) {
		xv_set(base_window.list,
			PANEL_INACTIVE, FALSE,
			NULL);
	}
	if (timedout)
		timeout_disconnect();
	XFlush(dpy);
	return (rval);
}

#ifdef USE_PROTOTYPES
char *expand_dirname(char *arg)
#else
char *expand_dirname(arg)
char	*arg;
#endif
{
	char 	*slash;
	char	*lastpart = "";
	char	*path;
	struct passwd *pwd;
	char	*firstpart = "";
	char	*s;

	if (arg[0] == '/' || (arg[0] != '~' && arg[0] != '$')) {
		path = strdup(arg);
		return (path);
	}
	s = strdup(arg);
	if (s == NULL)
		return (NULL);
	if ((slash = index(s, '/')) != NULL) {
		*slash = 0;
		lastpart = slash + 1;
	}
	switch (s[0]) {
	case '~': /* ~ or ~user */
		if (s[1] == '\0') {
			pwd = getpwuid(getuid());
			if (pwd == NULL) {
				footer_message(
				    "You are unknown to the system.");
				free(s);
				return (NULL);
			}
		} else {
			pwd = getpwnam(&s[1]);
			if (pwd == NULL) {
				footer_message("Unknown user %s.", &s[1]);
				free(s);
				return (NULL);
			}
		}
		firstpart = pwd->pw_dir;
		break;
	case '$': /* Environment variable */
		firstpart = getenv(&s[1]);
		if (firstpart == NULL) {
			footer_message("Unknown variable %s.", &s[1]);
			free(s);
			return (NULL);
		}
		break;
	}
	path = (char *)malloc((unsigned int)(strlen(firstpart) +
	    1 + strlen(lastpart) + 1));
	if (path == NULL) {
		footer_message("Memory allocation failed.");
		free(s);
		return (NULL);
	}
	if (lastpart[0] != '\0')
		sprintf(path, "%s/%s", firstpart, lastpart);
	else
		strcpy(path, firstpart);
	free(s);
	return (path);
}

#ifdef USE_PROTOTYPES
int	delete_local_dir(char *dir)
#else
int	delete_local_dir(dir)
char	*dir;
#endif
{
	struct dirlist *head = NULL;
	struct dirlist *tmp;
	extern char *sys_errlist[];
	int	rval = 0;

	if (chdir(dir) == -1) {
		local_footer_message("Can not change to %s: %s",
			dir, sys_errlist[errno]);
		return (errno);
	}
	head = read_local_dir(".");
	if (head == NULL) {
		fprintf(stderr, "Out of memory\n");
		rval = ENOMEM;
		goto out;
	}
	for (tmp = head->next; tmp != NULL; tmp = tmp->next)
		if (S_ISDIR(tmp->mode)) {
			if ((rval = delete_local_dir(tmp->name)) != 0)
				goto out;
		} else if ((rval = delete_local_file(tmp->name, unlink)) != 0)
				goto out;
out:
	if (head)
		free_dirlist(head);
	if (chdir("..") == -1) {
		local_footer_message("Can not cd ..: %s", sys_errlist[errno]);
		return (errno);
	}
	/* delete parent */
	if (rval == 0)
		rval = delete_local_file(dir, rmdir);
	return (rval);
}

#ifdef USE_PROTOTYPES
int delete_local_file(char *filename, int (*deletefunc)(const char *filename))
#else
int delete_local_file(filename, deletefunc)
char	*filename;
int		(*deletefunc)();
#endif
{
	int	answer;
	extern char *sys_errlist[];
#ifdef XVIEW3
	Xv_notice notice;
#endif

	if (confirmdeletes) {
		sprintf(scratch, "Really delete %s?", filename);
#ifdef XVIEW3
		notice = xv_create(base_window.panel, NOTICE,
			NOTICE_MESSAGE_STRINGS,
				scratch,
				NULL,
			NOTICE_BUTTON_YES, "Cancel",
			NOTICE_BUTTON_NO, "Delete",
			NOTICE_STATUS, &answer,
			XV_SHOW, TRUE,
			NULL);
		xv_destroy_safe(notice);
#else
		answer =  notice_prompt(base_window.panel, NULL,
			NOTICE_MESSAGE_STRINGS,
				scratch,
				NULL,
			NOTICE_BUTTON_YES, "Cancel",
			NOTICE_BUTTON_NO, "Delete",
			NULL);
#endif
	} else
		answer = NOTICE_NO;

	if (answer == NOTICE_NO) {
		local_footer_message("Deleting %s", filename);
		if ((*deletefunc)(filename) == -1) {
			local_footer_message("delete %s failed: %s", filename,
				sys_errlist[errno]);
			return (1);
		}
		return (0);
	}
	return (1); /* deletion aborted */
}

#ifdef USE_PROTOTYPES
int	delete_remote_dir(char *dir)
#else
int	delete_remote_dir(dir)
char	*dir;
#endif
{
	struct dirlist *head = NULL;
	struct dirlist *tmp;
	int	rval = 0;
	char	*ftperr;

	if (command("CWD %s", dir) == ERROR && code == 550) {
		sprintf(scratch, "Can not change to %s", dir);
		ftperr = ftp_error(' ', scratch);
		footer_message(ftperr);
		return (1);
	}
	head = read_remote_dir();
	if (head == NULL) {
		rval = ENOMEM;
		goto out;
	}
	for (tmp = head->next; tmp != NULL; tmp = tmp->next)
		if (S_ISDIR(tmp->mode)) {
			if ((rval = delete_remote_dir(tmp->name)) != 0)
				goto out;
		} else if ((rval = delete_remote_file(tmp->name, "DELE")) != 0)
				goto out;
out:
	if (head)
		free_dirlist(head);
	if (up_one_level())
		footer_message("Can not cd ..");

	/* delete parent */
	if (rval == 0)
		rval = delete_remote_file(dir, "RMD");
	return (rval);
}

#ifdef USE_PROTOTYPES
int delete_remote_file(char *filename, char *deletecmd)
#else
int delete_remote_file(filename, deletecmd)
char	*filename;
char	*deletecmd;
#endif
{
	char	*ftperr;
	int	answer;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	if (confirmdeletes) {
		sprintf(scratch, "Really delete %s?", filename);
#ifdef XVIEW3
		notice = xv_create(base_window.panel, NOTICE,
			NOTICE_MESSAGE_STRINGS,
				scratch,
				NULL,
			NOTICE_BUTTON_YES, "Cancel",
			NOTICE_BUTTON_NO, "Delete",
			NOTICE_STATUS, &answer,
			XV_SHOW, TRUE,
			NULL);
		xv_destroy_safe(notice);
#else
		answer = notice_prompt(base_window.panel, NULL,
			NOTICE_MESSAGE_STRINGS,
				scratch,
				NULL,
			NOTICE_BUTTON_YES, "Cancel",
			NOTICE_BUTTON_NO, "Delete",
			NULL);
#endif
	} else
		answer = NOTICE_NO;

	if (answer == NOTICE_NO) {
		footer_message("Deleting %s", filename);
		if (command("%s %s", deletecmd, filename) == ERROR) {
			sprintf(scratch, "delete %s failed", filename);
			ftperr = ftp_error(' ', scratch);
			footer_message(ftperr);
			return (1);
		}
		return (0);
	}
	return (1); /* deletion aborted */
}

#ifdef USE_PROTOTYPES
int up_one_level(void)
#else
int up_one_level()
#endif
{
	static struct {
		char	*cmd;
		char	*msg;
	} method[] = {
		{ "CDUP", "CDUP failed, trying XCUP" },
		{ "XCUP", "CDUP and XCUP failed, trying 'CD ..'" },
		{ "CWD ..", "CDUP, XCUP, and 'CD ..' failed.  Try manually." },
		{ NULL, NULL }
	};

	if (which_up_cmd != -1) {
		if (command(method[which_up_cmd].cmd) == ERROR &&
		    code >= 500) {
			footer_message(method[which_up_cmd].msg);
			which_up_cmd = -1;
		} else
			return (0);
	}
	for (which_up_cmd = 0; method[which_up_cmd].cmd; which_up_cmd++) {
		if (command(method[which_up_cmd].cmd) == ERROR &&
		    code >= 500)
			footer_message(method[which_up_cmd].msg);
		else
			return (0);
	}
	which_up_cmd = -1;
	return (1);

}
