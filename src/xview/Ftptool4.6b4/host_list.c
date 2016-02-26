#include "ftptool.h"

#pragma ident   "%Z%%M% %I%     %E%"

#ifdef USE_PROTOTYPES
void host_list_clean_proc(Panel_item item, Event *event)
#else
void host_list_clean_proc(item, event)
Panel_item	item;
Event	*event;
#endif
{
	xv_set(host_window.advanced.alias,
		PANEL_VALUE, "",
		NULL);
	xv_set(host_window.advanced.last_visited,
		PANEL_LABEL_STRING, "Never",
		NULL);
	xv_set(host_window.basic.host,
		PANEL_VALUE, "",
		NULL);
	if (!strcmp((char *)xv_get(item, PANEL_LABEL_STRING), "New")) {
		xv_set(host_window.basic.login,
			PANEL_VALUE, "",
			NULL);
		xv_set(host_window.basic.password,
			PANEL_VALUE, "",
			NULL);
	} else {
		xv_set(host_window.basic.login,
			PANEL_VALUE, "anonymous",
			NULL);
		xv_set(host_window.basic.password,
			PANEL_VALUE, anonftp_password,
			NULL);
		xv_set(host_window.basic.panel,
			PANEL_CARET_ITEM, host_window.basic.host,
			NULL);
	}
	xv_set(host_window.advanced.proxy,
		PANEL_VALUE, DEFAULT_PROXY,
		NULL);
	xv_set(host_window.advanced.transfer_mode,
		PANEL_VALUE, BINARY,
		NULL);
	xv_set(host_window.advanced.remote_auto_cd,
		PANEL_VALUE, ".",
		NULL);
	xv_set(host_window.advanced.local_auto_cd,
		PANEL_VALUE, ".",
		NULL);
	xv_set(host_window.advanced.os_type,
		PANEL_VALUE, REMOTE_OS_UNIX,
		NULL);
	xv_set(host_window.advanced.dir_parse,
		PANEL_VALUE, UNIX_DIR_PATTERN,
		XV_SHOW, FALSE,
		NULL);
	xv_set(host_window.advanced.comment,
		PANEL_VALUE, "",
		NULL);

	if (try_proxy) {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, FALSE,
			NULL);
	}
	xv_set(item,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}
#ifdef USE_PROTOTYPES
void host_window_update(struct hostlist *tmp)
#else
void host_window_update(tmp)
struct hostlist *tmp;
#endif
{
	xv_set(host_window.advanced.alias,
		PANEL_VALUE, tmp->aliasname,
		NULL);
	xv_set(host_window.advanced.last_visited,
		PANEL_LABEL_STRING, tmp->last_visited,
		NULL);
	xv_set(host_window.basic.host,
		PANEL_VALUE, tmp->host,
		NULL);
	xv_set(host_window.basic.login,
		PANEL_VALUE, tmp->login,
		NULL);
	xv_set(host_window.basic.password,
		PANEL_VALUE, tmp->password,
		NULL);
	xv_set(host_window.basic.account,
		PANEL_VALUE, tmp->account,
		NULL);
	xv_set(host_window.advanced.proxy,
		PANEL_VALUE, tmp->proxy,
		NULL);
	xv_set(host_window.advanced.transfer_mode,
		PANEL_VALUE, tmp->transfer_mode,
		NULL);
	xv_set(host_window.advanced.remote_auto_cd,
		PANEL_VALUE, tmp->remote_directory,
		NULL);
	xv_set(host_window.advanced.local_auto_cd,
		PANEL_VALUE, tmp->local_directory,
		NULL);
	xv_set(host_window.advanced.os_type,
		PANEL_VALUE, tmp->os_type,
		NULL);
	xv_set(host_window.advanced.dir_parse,
		PANEL_VALUE, tmp->dir_parse,
		NULL);
	xv_set(host_window.advanced.comment,
		PANEL_VALUE, tmp->comment,
		NULL);

	if (try_proxy) {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, FALSE,
			NULL);
	}
	if (tmp->os_type == REMOTE_OS_OTHER) {
		xv_set(host_window.advanced.dir_parse,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(host_window.advanced.dir_parse,
			XV_SHOW, FALSE,
			NULL);
	}
	if (!connected && auto_connect == TRUE) {
		dowhat = DOCONNECT;
		notify_stop();
	}
	fix_carets();
}

#ifdef USE_PROTOTYPES
void host_list_item_proc(Menu menu, Menu_item menu_item)
#else
void host_list_item_proc(menu, menu_item)
Menu	menu;
Menu_item menu_item;
#endif
{
	char	*alias = (char *)xv_get(menu_item, MENU_STRING);
	struct hostlist *tmp;

	tmp = gethostlist(hostlist_head, alias);
	if (tmp == NULL) {
		fprintf(stderr, "Entry %s in menu not found in list.\n", alias);
		exit(1);
	}
	host_window_update(tmp);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}


#ifdef USE_PROTOTYPES
void host_list_add_proc(Menu menu, Menu_item menu_item)
#else
void host_list_add_proc(menu, menu_item)
Menu	menu;
Menu_item menu_item;
#endif
{
	enter_host_info(1);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void host_list_change_proc(Menu menu, Menu_item menu_item)
#else
void host_list_change_proc(menu, menu_item)
Menu	menu;
Menu_item menu_item;
#endif
{
	enter_host_info(0);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void	host_save_proc(Menu menu, Menu_item menu_item)
#else
void	host_save_proc(menu, menu_item)
Menu	menu;
Menu_item	menu_item;
#endif
{
	write_ftptoolrc();
	list_changed = 0;
	timestamped = 0;
	reload_host_list_menu(hostlist_head);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void	host_load_proc(Menu menu, Menu_item menu_item)
#else
void	host_load_proc(menu, menu_item)
Menu	menu;
Menu_item	menu_item;
#endif
{
	int answer;
#ifdef XVIEW3
	Xv_notice	notice;
#endif

	if (timestamped || list_changed) {
#ifdef XVIEW3
		notice = xv_create(host_window.panel, NOTICE,
			NOTICE_MESSAGE_STRINGS,
				"Your host list has changed since the last save.",
				"Really load original?",
				NULL,
			NOTICE_BUTTON_YES, "Yes",
			NOTICE_BUTTON_NO, "No",
			NOTICE_STATUS, &answer,
			XV_SHOW, TRUE,
			NULL);
		xv_destroy_safe(notice);
#else
		answer = notice_prompt(host_window.panel, NULL,
			NOTICE_MESSAGE_STRINGS,
				"Your host list has changed since the last save.",
				"Really load original?",
				NULL,
			NOTICE_BUTTON_YES, "Yes",
			NOTICE_BUTTON_NO, "No",
			NULL);
#endif
		if (answer != NOTICE_YES)
			return;
	}
	free_hostlist(hostlist_head);
	hostlist_head = new_hostlist();
	read_ftptoolrc();
	list_changed = 0;
	timestamped = 0;
	reload_host_list_menu(hostlist_head);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void update_timestamp(void)
#else
void update_timestamp()
#endif
{
	time_t	t;
	time_t	time();
	char	*ctime();
	char	*s, *nl;
	char	*aliasname;
	struct hostlist *tmp;

	t = time((time_t *)NULL);
	s = ctime(&t);
	if ((nl = index(s, '\n')) != NULL)
		*nl = '\0';
	xv_set(host_window.advanced.last_visited,
		PANEL_LABEL_STRING, s,
		NULL);
	aliasname = (char *)xv_get(host_window.advanced.alias, PANEL_VALUE);
	tmp = gethostlist(hostlist_head, aliasname);
	if (tmp == NULL) {
		return;
	}
	free(tmp->last_visited);
	tmp->last_visited = strdup(s);
	if (tmp->last_visited == NULL) {
		fprintf(stderr, "Out of memory.\n");
		goto out;
	}

	timestamped++;
out:
	return;
}

#ifdef USE_PROTOTYPES
void enter_host_info(int warnchange)
#else
void enter_host_info(warnchange)
int		warnchange;
#endif
{
	char	*aliasname;
	char	*last_visited = "Never";
	char 	*proxy;
	char 	*host;
	char	*login;
	char	*password;
	char	*account;
	char	*comment;
	int		transfer_mode;
	int		os_type;
	char	*remote_directory;
	char	*local_directory;
	char	*dir_parse;
	int		answer;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	aliasname = (char *)xv_get(host_window.advanced.alias, PANEL_VALUE);
	if (aliasname[0] == '\0') {
		xv_set(host_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Please specify an alias name.",
			NULL);
		goto out;
	} else {
		xv_set(host_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "",
			NULL);
	}
	proxy = (char *)xv_get(host_window.advanced.proxy, PANEL_VALUE);
	host = (char *)xv_get(host_window.basic.host, PANEL_VALUE);
	login = (char *)xv_get(host_window.basic.login, PANEL_VALUE);
	password = (char *)xv_get(host_window.basic.password, PANEL_VALUE);
	account = (char *)xv_get(host_window.basic.account, PANEL_VALUE);
	transfer_mode = xv_get(host_window.advanced.transfer_mode, PANEL_VALUE);
	remote_directory = (char *)xv_get(host_window.advanced.remote_auto_cd,
		PANEL_VALUE);
	local_directory = (char *)xv_get(host_window.advanced.local_auto_cd,
		PANEL_VALUE);
	os_type = xv_get(host_window.advanced.os_type, PANEL_VALUE);
	dir_parse = (char *)xv_get(host_window.advanced.dir_parse, PANEL_VALUE);
	comment = (char *)xv_get(host_window.advanced.comment, PANEL_VALUE);

	if (gethostlist(hostlist_head, aliasname)) {
		if (warnchange) {
#ifdef XVIEW3
			notice = xv_create(host_window.panel, NOTICE,
				NOTICE_MESSAGE_STRINGS,
					"That alias exists. Do you really want to change it?",
					NULL,
				NOTICE_BUTTON_YES, "Yes",
				NOTICE_BUTTON_NO, "No",
				NOTICE_STATUS, &answer,
				XV_SHOW, TRUE,
				NULL);
			xv_destroy_safe(notice);
#else
			answer = notice_prompt(host_window.panel, NULL,
				NOTICE_MESSAGE_STRINGS,
					"That alias exists. Do you really want to change it?",
					NULL,
				NOTICE_BUTTON_YES, "Yes",
				NOTICE_BUTTON_NO, "No",
				NULL);
#endif
			if (answer != NOTICE_YES)
				goto out;
		}
		last_visited = (char *)xv_get(host_window.advanced.last_visited,
			PANEL_LABEL_STRING);
		delete_hostlist(hostlist_head, aliasname);
	}
	if ((hostlist_head = add_hostalias(hostlist_head, aliasname,
		last_visited, proxy, host, login, password, account,
		transfer_mode, remote_directory, local_directory,
		dir_parse, comment, os_type)) == NULL) {
		xv_set(host_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Add failed.",
			NULL);
		goto out;
	}
	list_changed = 1;
	reload_host_list_menu(hostlist_head);
out:
	return;
}

#ifdef USE_PROTOTYPES
void host_list_delete_proc(Menu menu, Menu_item menu_item)
#else
void host_list_delete_proc(menu, menu_item)
Menu	menu;
Menu_item menu_item;
#endif
{
	char	*aliasname;

	aliasname = (char *)xv_get(host_window.advanced.alias, PANEL_VALUE);

	if (gethostlist(hostlist_head, aliasname) == NULL) {
		xv_set(host_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "No such alias",
			NULL);
		return;
	}
	delete_hostlist(hostlist_head, aliasname);
	list_changed = 1;
	reload_host_list_menu(hostlist_head);
	xv_set(menu,
		MENU_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
struct hostlist *new_hostlist(void)
#else
struct hostlist *new_hostlist()
#endif
{
	struct hostlist *tmp;

	tmp = (struct hostlist *)malloc(sizeof (struct hostlist));
	if (tmp == NULL)
		return (NULL);
	bzero((char *)tmp, sizeof (struct hostlist));
	tmp->next = NULL;
	tmp->aliasname = NULL;
	tmp->last_visited = NULL;
	tmp->proxy = NULL;
	tmp->host = NULL;
	tmp->login = NULL;
	tmp->password = NULL;
	tmp->remote_directory = NULL;
	tmp->local_directory = NULL;
	tmp->dir_parse = NULL;
	tmp->comment = NULL;
	return (tmp);
}

#ifdef USE_PROTOTYPES
struct hostlist *add_hostalias(struct hostlist *head, char *aliasname,
	char *last_visited, char *proxy, char *host, char *login,
	char *password, char *account, int transfer_mode,
	char *remote_directory, char *local_directory, char *dir_parse,
	char *comment, int os_type)
#else
struct hostlist *add_hostalias(head, aliasname, last_visited, proxy,
	host, login, password, account, transfer_mode, remote_directory,
	local_directory, dir_parse, comment, os_type)
struct hostlist *head;
char	*aliasname;
char	*last_visited;
char	*proxy;
char	*host;
char	*login;
char	*password;
char	*account;
int		transfer_mode;
char	*remote_directory;
char	*local_directory;
char	*dir_parse;
char	*comment;
int		os_type;
#endif
{
	struct hostlist *tmp;
	struct hostlist *oldnext;
	int		rval = 0;

	/* add in sorted order */
	for (tmp = head; tmp->next != NULL; tmp = tmp->next)  {
		if (ignore_case)
			rval = strcasecmp(aliasname, tmp->next->aliasname);
		else
			rval = strcmp(aliasname, tmp->next->aliasname);
		if (rval < 0)
			break;
	}
	oldnext = tmp->next;
	tmp->next = new_hostlist();
	if (tmp->next == NULL) {
		tmp->next = oldnext;
		return (NULL);
	}

	tmp->next->aliasname = strdup(aliasname);
	if (tmp->next->aliasname == NULL) {
		goto out;
	}

	tmp->next->last_visited = strdup(last_visited);
	if (tmp->next->last_visited == NULL) {
		goto out;
	}

	tmp->next->proxy = strdup(proxy);
	if (tmp->next->proxy == NULL) {
		goto out;
	}

	tmp->next->host = strdup(host);
	if (tmp->next->host == NULL) {
		goto out;
	}

	tmp->next->login = strdup(login);
	if (tmp->next->login == NULL) {
		goto out;
	}

	tmp->next->password = strdup(password);
	if (tmp->next->password == NULL) {
		goto out;
	}

	tmp->next->account = strdup(account);
	if (tmp->next->account == NULL) {
		goto out;
	}

	tmp->next->transfer_mode = transfer_mode;

	tmp->next->remote_directory = strdup(remote_directory);
	if (tmp->next->remote_directory == NULL) {
		goto out;
	}

	tmp->next->local_directory = strdup(local_directory);
	if (tmp->next->local_directory == NULL) {
		goto out;
	}

	tmp->next->dir_parse = strdup(dir_parse);
	if (tmp->next->dir_parse == NULL) {
		goto out;
	}

	tmp->next->os_type = os_type;
	/* see if it's really UNIX from older versions */
	if (tmp->next->os_type == REMOTE_OS_OTHER &&
	    !strcmp(tmp->next->dir_parse, UNIX_DIR_PATTERN)) {
		tmp->next->os_type = REMOTE_OS_UNIX;
	}

	tmp->next->comment = strdup(comment);
	if (tmp->next->comment == NULL) {
		goto out;
	}

	tmp->next->next = oldnext;
	return (head);
out:
	tmp->next->next = NULL;
	free_hostlist(tmp->next);
	tmp->next = oldnext;
	return (NULL);
}

#ifdef USE_PROTOTYPES
void free_hostlist(struct hostlist *head)
#else
void free_hostlist(head)
struct hostlist *head;
#endif
{
	struct hostlist *tmp;

	while (head) {
		tmp = head->next;
		if (head->aliasname)
			free(head->aliasname);
		if (head->last_visited)
			free(head->last_visited);
		if (head->proxy)
			free(head->proxy);
		if (head->host)
			free(head->host);
		if (head->login)
			free(head->login);
		if (head->password)
			free(head->password);
		if (head->account)
			free(head->account);
		if (head->remote_directory)
			free(head->remote_directory);
		if (head->local_directory)
			free(head->local_directory);
		if (head->dir_parse)
			free(head->dir_parse);
		if (head->comment)
			free(head->comment);
		free((char *)head);
		head = tmp;
	}
}

#ifdef USE_PROTOTYPES
struct hostlist *gethostlist(struct hostlist *head, char *aliasname)
#else
struct hostlist *gethostlist(head, aliasname)
struct hostlist *head;
char	*aliasname;
#endif
{
	struct hostlist *tmp;

	for (tmp = head->next; tmp != NULL; tmp = tmp->next)
		if (!strcmp(aliasname, tmp->aliasname))
			return (tmp);
	return (NULL);
}

#ifdef USE_PROTOTYPES
int delete_hostlist(struct hostlist *head, char *aliasname)
#else
int delete_hostlist(head, aliasname)
struct hostlist *head;
char	*aliasname;
#endif
{
	struct hostlist *tmp;
	struct hostlist *tmp2;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (!strcmp(aliasname, tmp->next->aliasname)) {
			/* delete existing entry */
			tmp2 = tmp->next->next;
			tmp->next->next = NULL;
			free_hostlist(tmp->next);
			tmp->next = tmp2;
			return (1);
		}
	}
	return (0);
}

#ifdef USE_PROTOTYPES
void read_ftptoolrc(void)
#else
void read_ftptoolrc()
#endif
{
	int	fd;
	static char	aliasname[MAXCOMMENTLEN + 1];
	static char	host[MAXHOSTNAMELEN + 1];
	static char	login[MAXLOGINLEN + 1];
	static char	password[MAXHOSTNAMELEN + 1];
	static char	comment[MAXCOMMENTLEN + 1];
	FILE	*fp;
	int ch;

	fd = ftptoolrc_fd(O_RDONLY, 0600);
	if (fd == -1) {
		if (netrc_filename)
			host_append_netrc_proc();
		return;
	}
	/* ftptoolrc file format */
	/*
	 * Alias
	 * direct (ignored)
	 * host
	 * login
	 * password
	 * Comment
	 */
	fp = fdopen(fd, "r");
	if (fp == NULL) {
		close(fd);
		return;
	}
	/* see if it's a 'new' format */
	ch = getc(fp);
	switch (ch) {
	case '#':
		fclose(fp);
		read_oldftptoolrc();
		return;
	case '%':
		fclose(fp);
		read_newftptoolrc();
		return;
	default:
		break;
	}
	/* oldest format */
	ungetc(ch, fp);
	while (read_entry(0, fp, aliasname, (char *)NULL, (char *)NULL,
	    host, login, password, (int *)NULL, (char *)NULL, (char *)NULL,
	    UNIX_DIR_PATTERN, comment))
			if (add_hostalias(hostlist_head, aliasname, "Never",
			    DEFAULT_PROXY, host, login, password, "", BINARY,
			    ".", ".", UNIX_DIR_PATTERN, comment,
				REMOTE_OS_OTHER) == NULL)
					break;
	fclose(fp);
}

#ifdef USE_PROTOTYPES
int read_entry(int version, FILE *fp, char *aliasname, char *last_visited,
	char *proxy, char *host, char *login, char *password,
	int *transfer_mode, char *rdir, char *ldir, char *dir_parse,
	char *comment)
#else
int read_entry(version, fp, aliasname, last_visited, proxy, host,
	login, password, transfer_mode, rdir, ldir, dir_parse, comment)
int		version;
FILE	*fp;
char	*aliasname;
char	*last_visited;
char	*proxy;
char	*host;
char	*login;
char	*password;
int		*transfer_mode;
char	*rdir;
char	*ldir;
char	*dir_parse;
char	*comment;
#endif
{
	char	*nl;
	char	*dpasswd;

	if (fgets(aliasname, MAXCOMMENTLEN+1, fp) == NULL)
		if (feof(fp))
			return (0);
		else
			goto out;
	if ((nl = index(aliasname, '\n')) != NULL)
		*nl = '\0';
	if (version > 2) {
		if (fgets(last_visited, MAXPATHLEN+1, fp) == NULL)
			goto out;
		if ((nl = index(last_visited, '\n')) != NULL)
			*nl = '\0';
	}
	if (fgets(scratch, MAXHOSTNAMELEN+1, fp) == NULL)
		goto out;
	if (version > 0) {
		if (fgets(proxy, MAXHOSTNAMELEN+1, fp) == NULL)
			goto out;
		if ((nl = index(proxy, '\n')) != NULL)
			*nl = '\0';
	}
	if (fgets(host, MAXHOSTNAMELEN+1, fp) == NULL)
		goto out;
	if ((nl = index(host, '\n')) != NULL)
		*nl = '\0';
	if (fgets(login, MAXLOGINLEN+1, fp) == NULL)
		goto out;
	if ((nl = index(login, '\n')) != NULL)
		*nl = '\0';
	if (fgets(password, MAXPASSWORDLEN+1, fp) == NULL)
		goto out;
	if ((nl = index(password, '\n')) != NULL)
		*nl = '\0';
	if (version > 2) {
		/* decrypt it */
		if (version > 3)
			dpasswd = ftptool_decrypt(password, login_name);
		else
			dpasswd = old_ftptool_decrypt(password, login_name);
		if (dpasswd != NULL) {
			strcpy(password, dpasswd);
			free(dpasswd);
		}
	}
	if (version > 4) {
		if (fgets(scratch, MAXHOSTNAMELEN+1, fp) == NULL)
			goto out;
		*transfer_mode = atoi(scratch);
	}
	if (version > 0) {
		if (fgets(rdir, MAXPATHLEN+1, fp) == NULL)
			goto out;
		if ((nl = index(rdir, '\n')) != NULL)
			*nl = '\0';
	}
	if (version > 1) {
		if (fgets(ldir, MAXPATHLEN+1, fp) == NULL)
			goto out;
		if ((nl = index(ldir, '\n')) != NULL)
			*nl = '\0';
	}
	if (version > 2) {
		if (fgets(dir_parse, MAXPATHLEN+1, fp) == NULL)
			goto out;
		if ((nl = index(dir_parse, '\n')) != NULL)
			*nl = '\0';
	}
	if (fgets(comment, MAXCOMMENTLEN+1, fp) == NULL)
		goto out;
	if ((nl = index(comment, '\n')) != NULL)
		*nl = '\0';
	return (1);
out:
	fprintf(stderr, "bad entry for %s in %s\n", aliasname,
		FTPTOOL_RC);
	return (0);
}

#ifdef USE_PROTOTYPES
void read_oldftptoolrc(void)
#else
void read_oldftptoolrc()
#endif
{
	int	fd;
	static char	aliasname[MAXALIASLEN + 1];
	static char	last_visited[41];
	static int		transfer_mode;
	static char	proxy[MAXHOSTNAMELEN + 1];
	static char	host[MAXHOSTNAMELEN + 1];
	static char	login[MAXLOGINLEN + 1];
	static char	password[MAXPASSWORDLEN + 1];
	static char	remote_directory[MAXPATHLEN + 1];
	static char	local_directory[MAXPATHLEN + 1];
	static char	dir_parse[MAXPATHLEN + 1];
	static char	comment[MAXCOMMENTLEN + 1];
	FILE	*fp;
	int	version, patch;
	int	list_version;

	fd = ftptoolrc_fd(O_RDONLY, 0600);
	if (fd == -1) {
		if (netrc_filename)
			host_append_netrc_proc();
		return;
	}
	/* new ftptoolrc file format */
	/* first line */
	/* # Host List : Ftptool Version 3.3 */
	/*
	 * Alias
	 * direct (ignored)
	 * proxy
	 * host
	 * login
	 * password
	 * remote directory
	 * local directory
	 * dir parse line
	 * Comment
	 */
	fp = fdopen(fd, "r");
	if (fp == NULL) {
		close(fd);
		return;
	}
	/* first line is a comment line */
	fgets(comment, MAXCOMMENTLEN + 1, fp);
	sscanf(comment, "# Host List : Ftptool Version %d.%d\n",
	    &version, &patch);
	/* make sure it's not newer than we are */
	if (version > VERSION || (version == VERSION && patch > PATCHLEVEL)) {
		fprintf(stderr, "%s: %s is newer (%d.%d) than %s (%d.%d)\n",
		    program_name, FTPTOOL_RC, version, patch,
		    program_name, VERSION, PATCHLEVEL);
		exit(1);
	}
	if (version >= 4 && patch >= 1)
		list_version = 5;
	else if (version == 4 && patch == 0)
		list_version = 4;
	else if (version == 3 && patch == 3)
		list_version = 3;
	else if (version == 3 && patch == 2)
		list_version = 2;
	else
		list_version = 1;

	while (read_entry(list_version, fp,
		aliasname, last_visited, proxy, host, login, password,
		&transfer_mode, remote_directory, local_directory, dir_parse,
		comment)) {
			if (add_hostalias(hostlist_head, aliasname,
			    list_version >= 3 ? last_visited : "Never",
			    proxy, host, login, password, "",
			    list_version >= 5 ? transfer_mode : BINARY,
			    remote_directory,
			    list_version >= 2 ? local_directory : ".",
			    list_version >= 3 ? dir_parse : UNIX_DIR_PATTERN,
			    comment, REMOTE_OS_OTHER) == NULL)
					break;
	}
	fclose(fp);
}

#ifdef USE_PROTOTYPES
int read_newentry(int version, FILE *fp, char *aliasname, char *last_visited,
	char *proxy, char *host, char *login, char *password, char *account,
	int *transfer_mode, char *rdir, char *ldir, char *dir_parse,
	char *comment, int *os_type)
#else
int read_newentry(version, fp, aliasname, last_visited, proxy, host,
	login, password, account, transfer_mode, rdir, ldir, dir_parse, comment,
	os_type)
int		version;
FILE	*fp;
char	*aliasname;
char	*last_visited;
char	*proxy;
char	*host;
char	*login;
char	*password;
char	*account;
int		*transfer_mode;
char	*rdir;
char	*ldir;
char	*dir_parse;
char	*comment;
int		*os_type;
#endif
{
	char	*dpasswd;
	char	*daccount;
	static char linebuf[2 * MAXCOMMENTLEN + 1];

	/*
	 * Alias:Alias
	 * Last:Last Visited
	 * Proxy:proxy
	 * Host:host
	 * Login:login
	 * Password:password
	 * Account:account (or Acct for encrypted one)
	 * OS Type:os type
	 * DT:dir parse
	 * Mode:transfer_mode
	 * RCD:remote directory
	 * LCD:local directory
	 * Comment: comment
	 * --
	 */

	if (fgets(linebuf, sizeof (linebuf), fp) == NULL) {
		if (feof(fp))
			return (0);
		else
			goto out;
	}
	*aliasname = '\0';
	sscanf(linebuf, "Alias:%[^\n]", aliasname);

	if (fgets(linebuf, MAXPATHLEN+1, fp) == NULL)
		goto out;
	*last_visited = '\0';
	sscanf(linebuf, "Last:%[^\n]", last_visited);

	if (fgets(linebuf, MAXHOSTNAMELEN+1, fp) == NULL)
		goto out;
	*proxy = '\0';
	sscanf(linebuf, "Proxy:%[^\n]", proxy);

	if (fgets(linebuf, MAXHOSTNAMELEN+1, fp) == NULL)
		goto out;
	*host = '\0';
	sscanf(linebuf, "Host:%[^\n]", host);

	if (fgets(linebuf, MAXLOGINLEN+1, fp) == NULL)
		goto out;
	*login = '\0';
	sscanf(linebuf, "Login:%[^\n]", login);

	if (fgets(linebuf, MAXPASSWORDLEN+1, fp) == NULL)
		goto out;
	*password = '\0';
	sscanf(linebuf, "Password:%[^\n]", password);
	dpasswd = ftptool_decrypt(password, login_name);
	if (dpasswd != NULL) {
		strcpy(password, dpasswd);
		free(dpasswd);
	}

	/* Possible account field */
	if (fgets(linebuf, MAXPASSWORDLEN+1, fp) == NULL)
		goto out;
	*account = '\0';
	if (!strncmp(linebuf, "Account:", 8)) {
		sscanf(linebuf, "Account:%[^\n]", account);
		if (fgets(linebuf, MAXHOSTNAMELEN+1, fp) == NULL)
			goto out;
	} else if (!strncmp(linebuf, "Acct:", 5)) {
		sscanf(linebuf, "Acct:%[^\n]", account);
		if (fgets(linebuf, MAXHOSTNAMELEN+1, fp) == NULL)
			goto out;
		daccount = ftptool_decrypt(account, login_name);
		if (daccount != NULL) {
			strcpy(account, daccount);
			free(daccount);
		}
	}

	*scratch = '\0';
	sscanf(linebuf, "OS Type:%[^\n]", scratch);
	if (!strcmp(scratch, "UNIX"))
		*os_type = REMOTE_OS_UNIX;
	else if (!strcmp(scratch, "VMS"))
		*os_type = REMOTE_OS_VMS;
	else if (!strcmp(scratch, "DOS"))
		*os_type = REMOTE_OS_DOS;
	else
		*os_type = REMOTE_OS_OTHER;

	if (fgets(linebuf, MAXPATHLEN+1, fp) == NULL)
		goto out;
	*dir_parse = '\0';
	sscanf(linebuf, "DT:%[^\n]", dir_parse);

	if (fgets(linebuf, MAXHOSTNAMELEN+1, fp) == NULL)
		goto out;
	*scratch = '\0';
	sscanf(linebuf, "Mode:%[^\n]", scratch);
	*transfer_mode = atoi(scratch);

	if (fgets(linebuf, MAXPATHLEN+1, fp) == NULL)
		goto out;
	*rdir = '\0';
	sscanf(linebuf, "RCD:%[^\n]", rdir);

	if (fgets(linebuf, MAXPATHLEN+1, fp) == NULL)
		goto out;
	*ldir = '\0';
	sscanf(linebuf, "LCD:%[^\n]", ldir);

	if (fgets(linebuf, MAXCOMMENTLEN+1, fp) == NULL)
		goto out;
	*comment = '\0';
	sscanf(linebuf, "Comment:%[^\n]", comment);

	/* Dump separator */
	if (fgets(linebuf, MAXCOMMENTLEN+1, fp) == NULL)
		goto out;

	return (1);
out:
	fprintf(stderr, "%s: Bad entry for %s in %s\n",
		program_name, aliasname, FTPTOOL_RC);
	return (0);
}


#ifdef USE_PROTOTYPES
void read_newftptoolrc(void)
#else
void read_newftptoolrc()
#endif
{
	int	fd;
	static char	aliasname[MAXALIASLEN + 1];
	static char	last_visited[41];
	int		transfer_mode;
	int		os_type;
	static char	proxy[MAXHOSTNAMELEN + 1];
	static char	host[MAXHOSTNAMELEN + 1];
	static char	login[MAXLOGINLEN + 1];
	static char	password[MAXPASSWORDLEN + 1];
	static char	account[MAXPASSWORDLEN + 1];
	static char	remote_directory[MAXPATHLEN + 1];
	static char	local_directory[MAXPATHLEN + 1];
	static char	dir_parse[MAXPATHLEN + 1];
	static char	comment[MAXCOMMENTLEN + 1];
	FILE	*fp;
	int	version, patch;

	fd = ftptoolrc_fd(O_RDONLY, 0600);
	if (fd == -1) {
		if (netrc_filename)
			host_append_netrc_proc();
		return;
	}
	/* new ftptoolrc file format */
	/* first line */
	/* % Host List : Ftptool Version 4.4 */
	/*
	 * Alias:Alias
	 * Last:Last Visited
	 * Proxy:proxy
	 * Host:host
	 * Login:login
	 * Password:password
	 * OS Type:os type
	 * DT:dir parse
	 * Mode:transfer_mode
	 * RCD:remote directory
	 * LCD:local directory
	 * Comment: Comment
	 * --
	 */
	fp = fdopen(fd, "r");
	if (fp == NULL) {
		close(fd);
		return;
	}
	/* first line is a comment line */
	fgets(comment, MAXCOMMENTLEN + 1, fp);
	sscanf(comment, "%% Host List : Ftptool Version %d.%d\n",
	    &version, &patch);
	/* make sure it's not newer than we are */
	if (version > VERSION || (version == VERSION && patch > PATCHLEVEL)) {
		fprintf(stderr, "%s: %s is newer (%d.%d) than %s (%d.%d)\n",
		    program_name, FTPTOOL_RC, version, patch,
		    program_name, VERSION, PATCHLEVEL);
		exit(1);
	}

	while (read_newentry(0, fp,
	    aliasname, last_visited, proxy, host, login, password, account,
	    &transfer_mode, remote_directory, local_directory, dir_parse,
		comment, &os_type)) {
			if (add_hostalias(hostlist_head, aliasname,
			    last_visited, proxy, host, login, password, account,
			    transfer_mode, remote_directory, local_directory,
			    dir_parse, comment, os_type) == NULL)
					break;
	}
	fclose(fp);
}

#ifdef USE_PROTOTYPES
void write_ftptoolrc(void)
#else
void write_ftptoolrc()
#endif
{
	int	fd;
	struct hostlist *tmp;
	FILE	*fp;
	char	*epasswd;
	char	*eaccount;
	char	*type;

	fd = ftptoolrc_fd(O_WRONLY | O_TRUNC | O_CREAT, 0600);
	if (fd == -1) {
		perror("writing host list");
		return;
	}
	/* ftptoolrc file format */
	/*
	 * Alias:Alias
	 * Last:Last Visited
	 * Proxy:proxy
	 * Host:host
	 * Login:login
	 * Password:encrypted password
	 * Acct:encrypted account
	 * OS Type:os type
	 * DT:dir parse
	 * Mode:transfer_mode
	 * RCD:remote directory
	 * LCD:local directory
	 * Comment: Comment
	 * --
	 */
	fp = fdopen(fd, "w");
	if (fp == NULL) {
		close(fd);
		return;
	}
	fprintf(fp, "%% Host List : %s\n", header_name);
	for (tmp = hostlist_head->next; tmp != NULL; tmp = tmp->next) {
		epasswd = ftptool_encrypt(tmp->password, login_name);
		if (epasswd == NULL) {
			fprintf(stderr, "Out of memory.\n");
			exit(1);
		}
		eaccount = ftptool_encrypt(tmp->account, login_name);
		if (eaccount == NULL) {
			fprintf(stderr, "Out of memory.\n");
			exit(1);
		}
		fprintf(fp, "Alias:%s\n", tmp->aliasname);
		fprintf(fp, "Last:%s\n", tmp->last_visited);
		fprintf(fp, "Proxy:%s\n", tmp->proxy);
		fprintf(fp, "Host:%s\n", tmp->host);
		fprintf(fp, "Login:%s\n", tmp->login);
		fprintf(fp, "Password:%s\n", epasswd);
		fprintf(fp, "Acct:%s\n", eaccount);
		switch (tmp->os_type) {
		case REMOTE_OS_UNIX:
			type = "UNIX";
			break;
		case REMOTE_OS_VMS:
			type = "VMS";
			break;
		case REMOTE_OS_DOS:
			type = "DOS";
			break;
		case REMOTE_OS_OTHER:
		default:
			type = "Other";
			break;
		}
		fprintf(fp, "OS Type:%s\n", type);
		fprintf(fp, "DT:%s\n", tmp->dir_parse);
		fprintf(fp, "Mode:%d\n", tmp->transfer_mode);
		fprintf(fp, "RCD:%s\n", tmp->remote_directory);
		fprintf(fp, "LCD:%s\n", tmp->local_directory);
		fprintf(fp, "Comment:%s\n", tmp->comment);
		fprintf(fp, "--\n");
		free(eaccount);
		free(epasswd);
	}

	fclose(fp);
}

#ifdef USE_PROTOTYPES
int ftptoolrc_fd(int flags, int mode)
#else
int ftptoolrc_fd(flags, mode)
int		flags;
int		mode;
#endif
{
	char	*filename = NULL;
	int	fd;

	filename = find_dotfile(FTPTOOL_RC);
	if (filename == NULL) {
		if (flags == O_RDONLY) {
			/* try global one */
			filename = strdup(GLOBAL_FTPTOOLRC);
			if (filename == NULL) {
				return (-1);
			}
		} else {
			if ((filename = create_dotfile(FTPTOOL_RC,
			    0600)) == NULL)
				return (-1);
		}
	}

	/* try current directory */
	if ((fd = open(filename, flags, mode)) != -1) {
		free(filename);
		return (fd);
	}
	free(filename);
	return (-1);
}

#ifdef USE_PROTOTYPES
void reload_host_list_menu(struct hostlist *head)
#else
void reload_host_list_menu(head)
struct hostlist *head;
#endif
{
	Menu	menu = xv_get(host_window.hosts, PANEL_ITEM_MENU);
	int	nitems = xv_get(menu, MENU_NITEMS);
	int	row;
	struct hostlist *tmp;
	Menu_item	mi;
	int		maxwidth = 0;
	int		width = 0;
	double	cols;
#ifdef PIN_HOST_LIST
	Frame	frame;
	int		isshown;

	frame = (Panel)xv_get(menu, MENU_PIN_WINDOW);
	isshown = FALSE;
	if (frame)
		isshown = xv_get(frame, XV_SHOW);

	if (isshown) {
		xv_set(frame,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
			FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
			XV_SHOW, FALSE,
			NULL);
	}
#endif
	/* 2 items minimum, 1 for title */
	for (row = nitems; row > 1; row--) {
		xv_set(menu,
			MENU_REMOVE, row,
			NULL);
	}

	/* now the list */
	for (row = nitems; row > 0; row--) {
		xv_set(host_window.basic.list,
			PANEL_LIST_DELETE, row - 1,
			PANEL_PAINT, PANEL_NONE,
			NULL);
	}

	nhostlist_items = 0;
	for (tmp = head->next; tmp != NULL; tmp = tmp->next) {
		width = strlen(tmp->aliasname);
		maxwidth = MAX(maxwidth, width);
		mi = (Menu_item)xv_create(XV_NULL, MENUITEM,
			MENU_STRING, tmp->aliasname,
			MENU_NOTIFY_PROC, host_list_item_proc,
			MENU_RELEASE,
			NULL);
		if (mi == XV_NULL) {
			fprintf(stderr, "Out of memory for menu item.\n");
			exit(1);
		}
		xv_set(menu,
			MENU_APPEND_ITEM, mi,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		xv_set(host_window.basic.list,
			PANEL_LIST_INSERT, nhostlist_items,
			PANEL_LIST_STRING, nhostlist_items, tmp->aliasname,
			PANEL_LIST_CLIENT_DATA, nhostlist_items, tmp,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		nhostlist_items++;
	}
	if (nhostlist_items == 0) {
		mi = (Menu_item)xv_create(XV_NULL, MENUITEM,
			MENU_STRING, "No Hosts!",
			MENU_RELEASE,
			NULL);
		if (mi == XV_NULL) {
			fprintf(stderr, "Out of memory for menu item.\n");
			exit(1);
		}
		xv_set(menu,
			MENU_APPEND_ITEM, mi,
			NULL);
	}

	if (maxwidth == 0)
		maxwidth = 1;

	cols = 2.0 * nhostlist_items;
	cols /= maxwidth;
	cols = ceil(sqrt(cols));
	xv_set(menu,
		MENU_NCOLS, (int)cols,
		NULL);
	xv_set(host_window.basic.list,
		XV_SHOW, TRUE,
		NULL);
#ifdef PIN_HOST_LIST
	if (isshown) {
		xv_set(frame,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			XV_SHOW, TRUE,
			NULL);
	}
#endif
}

#ifdef USE_PROTOTYPES
char *create_dotfile(char *dotfile, int mode)
#else
char *create_dotfile(dotfile, mode)
char	*dotfile;
int		mode;
#endif
{
	char	*home;
	char	*filename = NULL;
	int	fd;

	home = getenv("HOME");
	if (home != NULL && home[0] != '\0') {
		/* try $HOME/dotfile */
		filename = malloc((unsigned int)(strlen(home) + 1 +
		    strlen(dotfile) + 1));
		if (filename == NULL)
			return (NULL);
		sprintf(filename, "%s/%s", home, dotfile);
		if ((fd = creat(filename, mode)) == -1) {
			free(filename);
			return (NULL);
		}
		close(fd);
		/* found it */
		return (filename);
	}
	filename = strdup(dotfile);
	if (filename == NULL)
		return (NULL);
	if ((fd = creat(filename, mode)) == -1) {
		free(filename);
		return (NULL);
	}
	close(fd);

	return (filename);
}

#ifdef USE_PROTOTYPES
char *find_dotfile(char *dotfile)
#else
char *find_dotfile(dotfile)
char	*dotfile;
#endif
{
	char	*home;
	char	*filename = NULL;

	home = getenv("HOME");
	if (home != NULL && home[0] != '\0') {
		/* try $HOME/dotfile */
		filename = malloc((unsigned int)(strlen(home) + 1 +
		    strlen(dotfile) + 1));
		if (filename == NULL)
			return (NULL);
		sprintf(filename, "%s/%s", home, dotfile);
		if (access(filename, F_OK) == -1) {
			free(filename);
			return (NULL);
		}
		/* found it */
		return (filename);
	}
	filename = strdup(dotfile);
	if (filename == NULL)
		return (NULL);
	if (access(filename, F_OK) == -1) {
		free(filename);
		return (NULL);
	}

	return (filename);
}

#define	MACHINE		1
#define	LOGIN		2
#define	PASSWORD	3
#define	ACCOUNT		4
#define	MACDEF		5

#ifdef USE_PROTOTYPES
int netrc_token(FILE *fp)
#else
int netrc_token(fp)
FILE	*fp;
#endif
{
	char	keyword[MAXPATHLEN + 1];
	int		ch, lastchar;

	for (;;) {
		if (fscanf(fp, "%s", keyword) == EOF)
			return (EOF);
		if (!strcmp(keyword, "machine"))
			return (MACHINE);
		if (!strcmp(keyword, "login"))
			return (LOGIN);
		if (!strcmp(keyword, "password"))
			return (PASSWORD);
		if (!strcmp(keyword, "macdef")) {
			lastchar = 'e';
			ch = 'f';
			do {
				lastchar = ch;
				ch = getc(fp);
			} while (ch != EOF && (ch != '\n' && lastchar != '\n'));
		}
	}
}


#ifdef USE_PROTOTYPES
void	host_append_netrc_proc(void)
#else
void	host_append_netrc_proc()
#endif
{
	FILE 	*fp;
	char	machine[MAXHOSTNAMELEN + 1];
	char	login[MAXHOSTNAMELEN + 1];
	char	password[MAXPASSWORDLEN+ 1];
	int		foundmachine = 0;

	if ((fp = fopen(netrc_filename, "r")) == NULL) {
		footer_message("%s exists but can not be read.",
		    netrc_filename);
		return;
	}
	/* read each line */
	/* looking for "machine" surrounded by white space */
	/* if we find it, then look for other tokens until EOF or "machine" */
	/* look for "login" name or "password" name */
	/* machine ray login demo password mypassword */

	for (;;) {
		switch (netrc_token(fp)) {
		case MACHINE:
			if (foundmachine)
				add_netrc(machine, login, password);
			foundmachine = 1;
			strcpy(machine, "");
			strcpy(login, "");
			strcpy(password, "");
			if (fscanf(fp, "%s", machine) == EOF) {
				goto out;
			}
			break;
		case LOGIN:
			if (!foundmachine || fscanf(fp, "%s", login) == EOF) {
				footer_message("Error in .netrc.");
				goto out;
			}
			break;
		case PASSWORD:
			if (!foundmachine ||
			    fscanf(fp, "%s", password) == EOF) {
				footer_message("Error in .netrc.");
				goto out;
			}
			break;
		case EOF:
			if (foundmachine)
				add_netrc(machine, login, password);
			goto out;
		default:
			break;
		}
	}

out:
	if (list_changed)
		reload_host_list_menu(hostlist_head);
	fclose(fp);
	return;
}

#ifdef USE_PROTOTYPES
void add_netrc(char *machine, char *login, char *password)
#else
void add_netrc(machine, login, password)
char	*machine;
char	*login;
char	*password;
#endif
{
	if (machine[0] == '\0' || login[0] == '\0') {
		footer_message("Incomplete .netrc entry.");
		return;
	}
	sprintf(scratch, "%s %s", machine, login);
	if (gethostlist(hostlist_head, scratch)) {
		footer_message("Alias \"%s\" already exists.", scratch);
		return;
	}
	if ((hostlist_head = add_hostalias(hostlist_head, scratch, "Never",
	    DEFAULT_PROXY, machine, login, password, "", BINARY, ".", ".",
	    UNIX_DIR_PATTERN, "From .netrc", REMOTE_OS_UNIX)) == NULL) {
		xv_set(host_window.frame,
			FRAME_LEFT_FOOTER, "Add failed",
			NULL);
		return;
	}
	list_changed = 1;
}

#ifdef USE_PROTOTYPES
char key_to_char(char *key)
#else
char key_to_char(key)
char	*key;
#endif
{
	char	*tmp = key;
	char	c = '\0';

	while (*tmp) {
		c ^= *tmp;
		tmp++;
	}
	c &= 0x7f;
	return (c);
}

#ifdef USE_PROTOTYPES
char *ftptool_encrypt(char *s, char *key)
#else
char *ftptool_encrypt(s, key)
char	*s;
char	*key;
#endif
{
	char	k;
	char	c;
	char	*es;
	char	*scanstr;
	char	*changestr;

	k = key_to_char(key);
	es = (char *)malloc((unsigned int)(2 * strlen(s) + 1));
	if (es == NULL) {
		fprintf(stderr, "Out of memory\n");
		return (NULL);
	}
	scanstr = s;
	changestr = es;
	while (*scanstr) {
		c = *scanstr++ ^ k;
		if (iscntrl(c)) {
			*changestr++ = '^';
			if (c == '\177')
				*changestr++ = '?';
			else
				*changestr++ = c + 'A';
		} else if (c == '^') {
			*changestr++ = '^';
			*changestr++ = '~';
		} else {
			*changestr++ = c;
		}
	}
	*changestr = '\0';
	return (es);
}

#ifdef USE_PROTOTYPES
char *ftptool_decrypt(char *s, char *key)
#else
char *ftptool_decrypt(s, key)
char	*s;
char	*key;
#endif
{
	char	k;
	char	c;
	char	*ds;
	char	*scanstr;
	char	*changestr;

	k = key_to_char(key);
	ds = (char *)malloc((unsigned int)(strlen(s) + 1));
	if (ds == NULL) {
		fprintf(stderr, "Out of memory\n");
		return (NULL);
	}
	scanstr = s;
	changestr = ds;
	while (*scanstr) {
		c = *scanstr++;
		if (c != '^') {
			*changestr++ = c ^ k;
		} else {
			c = *scanstr++;
			if (c == '?') {
				*changestr = '\177';
			} else if (c == '~') {
				*changestr = '^';
			} else {
				*changestr = c - 'A';
			}
			*changestr++ ^= k;
		}
	}
	*changestr = '\0';
	return (ds);
}

#ifdef USE_PROTOTYPES
char *old_ftptool_decrypt(char *s, char *key)
#else
char *old_ftptool_decrypt(s, key)
char	*s;
char	*key;
#endif
{
	char	k;
	char	*ds;
	char	*tmp;

	k = key_to_char(key);
	ds = strdup(s);
	if (ds == NULL) {
		fprintf(stderr, "Out of memory\n");
		return (NULL);
	}
	tmp = ds;
	while (*tmp) {
		*tmp++ ^= k;
	}
	return (ds);
}

#ifdef USE_PROTOTYPES
struct hostlist *sort_hostlist(struct hostlist *head)
#else
struct hostlist *sort_hostlist(head)
struct hostlist *head;
#endif
{
	struct hostlist *tmp;
	struct hostlist *newhead, *current;
	int	rval;

	newhead = new_hostlist();
	if (newhead == NULL)
		return (head);

	while (head->next != NULL) {
		/* remove first element */
		current = head->next;
		head->next = current->next;
		current->next = NULL;
		/* insert in proper place */
		for (tmp = newhead; tmp->next != NULL; tmp = tmp->next)  {
			if (ignore_case)
				rval = strcasecmp(current->aliasname,
				    tmp->next->aliasname);
			else
				rval = strcmp(current->aliasname,
				    tmp->next->aliasname);
			if (rval < 0) {
				break;
			}
		}
		current->next = tmp->next;
		tmp->next = current;
	}
	free_hostlist(head);
	return (newhead);
}
