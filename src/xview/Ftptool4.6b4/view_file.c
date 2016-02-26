#include "ftptool.h"

#pragma ident   "@(#)view_file.c 1.7     93/06/24"

#ifdef USE_PROTOTYPES
int start_viewer(char *filename, int isremote)
#else
int start_viewer(filename, isremote)
char	*filename;
int		isremote;
#endif
{
	struct extension_info *ext_info;
	char	*viewer = default_viewer;
	char	*ext = "default";
	int	type;
	char	**argv;


	ext_info = type_by_either(filename);
	if (ext_info != NULL) {
		/* magic number matched */
		viewer = ext_info->program;
		ext = ext_info->extension;
		type = ext_info->type;
	} else {
		/* both NULL, try default */
		viewer = default_viewer;
		ext = "default";
		type = FILE_VIEWER;
	}

	if (type == FILE_DECOMPRESSOR) {
		/* uncompress file */
		if (isremote)
			footer_message("Uncompressing %s.", filename);
		else
			local_footer_message("Uncompressing %s.", filename);
		argv = build_argv(viewer, filename);
		pipe_program(argv);
		free_argv(argv);
		return (COMPRESSED);
	}


	if (isremote) {
		footer_message("Starting %s viewer %s.", ext, viewer);
	} else {
		local_footer_message("Starting %s viewer %s.", ext, viewer);
	}
	fork_viewer(viewer, filename);
	return (0);
}

#ifdef USE_PROTOTYPES
char **build_argv(char *program, char *filename)
#else
char **build_argv(program, filename)
char *program, *filename;
#endif
{
	int	argc;
	char	*arg;
	char	**argv;
	int	percentf = 0;
	char	*p;

	/* count number of words */
	argc = 0;
	p = strdup(program);
	if (p == NULL) {
		fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	arg = p;
	while (*arg && isspace(*arg))
		arg++;
	while (*arg) {
		/* found an arg */
		argc++;
		/* skip arg */
		while (*arg && !isspace(*arg))
			arg++;
		/* skip WS */
		while (*arg && isspace(*arg))
			arg++;
	}
	if (argc == 0) {
		fprintf(stderr, "Null viewer.\n");
		exit(1);
	}
	/* allocate argv[]  (extra entry in case no %f specified) */
	argv = (char **)malloc((argc+2) * sizeof (char *));
	if (argv == NULL) {
		fprintf(stderr, "No memory. Cannot start viewer '%s'.\n",
			program);
		exit(1);
	}
	argc = 0;
	arg = strtok(p, " \t\n");
	while (arg) {
		if (!strcmp(arg, "%f")) {
			percentf++;
			argv[argc] = strdup(filename);
			if (argv[argc] == NULL) {
				fprintf(stderr, "Out of memory.\n");
				exit(1);
			}
		} else {
			argv[argc] = strdup(arg);
			if (argv[argc] == NULL) {
				fprintf(stderr, "Out of memory.\n");
				exit(1);
			}
		}
		argc++;
		arg = strtok((char *)NULL, " \t\n");
	}
	/* %f means file name */
	if (!percentf) {
		argv[argc] = strdup(filename);
		if (argv[argc] == NULL) {
			fprintf(stderr, "Out of memory.\n");
			exit(1);
		}
		argc++;
	}
	argv[argc] = NULL;
	free(p);
	return (argv);
}

#ifdef USE_PROTOTYPES
void free_argv(char **argv)
#else
void free_argv(argv)
char	**argv;
#endif
{
	char **tmp;

	tmp = argv;
	while (*tmp) {
		free(*tmp);
		tmp++;
	}
	free(argv);
}

#ifdef USE_PROTOTYPES
void fork_viewer(char *program, char *filename)
#else
void fork_viewer(program, filename)
char	*program;
char	*filename;
#endif
{
	int	pid;
	char	**argv;

	switch (pid = fork()) {
	case -1:
		perror("fork");
		break;
	case 0: /* child */
		argv = build_argv(program, filename);
		execvp(argv[0], argv);
		fprintf(stderr, "viewer '%s' failed:", program);
		perror("");
		exit(1);
	default: /* parent */
		notify_set_wait3_func(base_window.frame, notify_default_wait3,
		    pid);
		break;
	}
}

#ifdef USE_PROTOTYPES
int iscompressed(char *filename)
#else
int iscompressed(filename)
char	*filename;
#endif
{
	struct extension_info *ext_info;

	ext_info = type_by_either(filename);
	if (ext_info != NULL) {
		if (ext_info->type == FILE_DECOMPRESSOR) {
			return (1);
		} else {
			return (0);
		}
	} else {
		return (0);
	}
}

#ifdef USE_PROTOTYPES
void pipe_program(char *argv[])
#else
void pipe_program(argv)
char	*argv[];
#endif
{
	int	pid;
	int pipe_fds[2];
	char scratch[100];
	FILE	*fp;

	if (pipe(pipe_fds) == -1) {
		perror("ftptool:pipe");
		return;
	}

	switch (pid = fork()) {
	case -1:
		perror("fork");
		break;
	case 0: /* child */
		dup2(pipe_fds[1], 1);
		dup2(pipe_fds[1], 2);
		close(pipe_fds[0]);
		close(pipe_fds[1]);
		execvp(argv[0], argv);
		perror("can not start program");
		exit(1);
	default: /* parent */
		notify_set_wait3_func(base_window.frame, notify_default_wait3,
		    pid);
		close(pipe_fds[1]);
		break;
	}
	fp = fdopen(pipe_fds[0], "r");
	if (fp == NULL) {
		perror("fopen");
		close(pipe_fds[0]);
		return;
	}
	notify_do_dispatch();
	while (fgets(scratch, sizeof (scratch), fp) != NULL) {
		notify_no_dispatch();
		log_message(scratch);
		notify_do_dispatch();
	}
	notify_no_dispatch();
	fclose(fp);
}

#ifdef USE_PROTOTYPES
int view_local_file(char *name, int which, int *dirchanged)
#else
int view_local_file(name, which, dirchanged)
char	*name;
int		which;
int		*dirchanged;
#endif
{
	char *dot;
	struct stat buf;

	if (stat(name, &buf) == -1) {
		local_footer_message("%s: %s.", name, sys_errlist[errno]);
		return (1);
	}
	if (!S_ISREG(buf.st_mode)) {
		local_footer_message("%s is not a regular file.", name);
		return (1);
	}
	if (start_viewer(name, which == DOREMOTEVIEW) == COMPRESSED) {
		dot = rindex(name, '.');
		if (dot) {
			*dot = '\0';
			if (which == DOLOCALVIEW && dirchanged != NULL)
				*dirchanged = 1;
			start_viewer(name, which == DOREMOTEVIEW);
		}
	}
	return (0);
}

char *newname;

#ifdef USE_PROTOTYPES
int view_remote_file(char *name, size_t size)
#else
int view_remote_file(name, size)
char	*name;
size_t	size;
#endif
{
	static char *tmpname;
	char scratch[MAXPATHLEN+1];

	if (tmpname == NULL || (access(tmpname, F_OK) == -1 &&
	    errno == ENOENT)) {
		tmpname = tempnam("/var/tmp", "ftptl");
		if (tmpname == NULL || (mkdir(tmpname, 0777) == -1)) {
			footer_message("Can't create temporary directory.");
			return (1);
		}
	}
	sprintf(scratch, "%s/%s", tmpname, name);
	/* newname will be set if a unique local name is generated */
	newname = NULL;
	if (get_file(name, scratch, size)) {
		return (1);
	}

	if (newname)
		strcpy(scratch, newname);

	return (view_local_file(scratch, DOREMOTEVIEW, (int *)NULL));
}

#ifdef USE_PROTOTYPES
struct extension_info *new_extension(char *extension, char *magic,
	char *program, int type)
#else
struct extension_info *new_extension(extension, magic, program, type)
char	*extension;
char 	*magic;
char	*program;
int	type;
#endif
{
	struct extension_info *tmp;

	tmp = (struct extension_info *)malloc(sizeof (*tmp));
	if (tmp == NULL)
		return (NULL);

	tmp->next = NULL;
	tmp->extension = NULL;
	tmp->magic = NULL;
	tmp->program = NULL;

	tmp->extension = strdup(extension);
	if (tmp->extension == NULL)
		goto out;

	tmp->magic = strdup(magic);
	if (tmp->magic == NULL)
		goto out;

	tmp->program = strdup(program);
	if (tmp->program == NULL)
		goto out;

	tmp->type = type;
	return (tmp);
out:
	free_extension(tmp);
	return (NULL);
}

#ifdef USE_PROTOTYPES
void free_extension(struct extension_info *cell)
#else
void free_extension(cell)
struct extension_info *cell;
#endif
{
	if (cell->extension)
		free(cell->extension);
	if (cell->magic)
		free(cell->magic);
	if (cell->program)
		free(cell->program);
	free(cell);
}

#ifdef USE_PROTOTYPES
struct extension_info *add_extension(struct extension_info *head,
	char *extension, char *magic, char *program, int type)
#else
struct extension_info *add_extension(head, extension, magic, program, type)
struct extension_info *head;
char	*extension;
char 	*magic;
char	*program;
int	type;
#endif
{
	struct extension_info *new;
	struct extension_info *tmp;
	int		rval;

	new = new_extension(extension, magic, program, type);
	if (new == NULL) {
		fprintf(stderr, "%s: no memory for new extension.\n",
			program_name);
		return (NULL);
	}

	/* put in alphabetical order */
	for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
		if (ignore_case)
			rval = strcasecmp(extension, tmp->next->extension);
		else
			rval = strcmp(extension, tmp->next->extension);
		if (rval < 0)
			break;
	}

	new->next = tmp->next;
	tmp->next = new;
	return (head);
}

#ifdef USE_PROTOTYPES
int read_extensions(FILE *fp, char *extension, char *magic,
	char *program, int *type)
#else
int read_extensions(fp, extension, magic, program, type)
FILE	*fp;
char	*extension;
char	*magic;
char	*program;
int	*type;
#endif
{
	static char linebuf[2 * MAXPATHLEN + 1];
	static char typebuf[MAXPATHLEN + 1];

	/*
	 * Extension:
	 * Magic:
	 * Program:
	 * Type:
	 * --
	 */
	if (fgets(linebuf, sizeof (linebuf), fp) == NULL) {
		if (feof(fp))
			return (0);
		else
			goto out;
	}
	*extension = '\0';
	sscanf(linebuf, "Extension:%[^\n]", extension);

	if (fgets(linebuf, sizeof (linebuf), fp) == NULL)
		goto out;
	*magic = '\0';
	sscanf(linebuf, "Magic:%[^\n]", magic);

	if (fgets(linebuf, sizeof (linebuf), fp) == NULL)
		goto out;
	*program = '\0';
	sscanf(linebuf, "Program:%[^\n]", program);

	if (fgets(linebuf, sizeof (linebuf), fp) == NULL)
		goto out;

	/* Possible Type field */
	if (!strncmp(linebuf, "Type:", 5)) {
		sscanf(linebuf, "Type:%[^\n]", typebuf);
		if (!strcmp(typebuf, "decompressor"))
			*type = FILE_DECOMPRESSOR;
		else {
			/* file viewer */
			*type = FILE_VIEWER;
		}
		/* Dump separator */
		if (fgets(linebuf, sizeof (linebuf), fp) == NULL)
			goto out;
	}
	/* Dump separator */

	return (1);

out:
	fprintf(stderr, "%s: Bad entry for extension in %s.\n",
		program_name, FTPTOOL_TYPES);
	return (0);
}

#ifdef USE_PROTOTYPES
void load_extensions(void)
#else
void load_extensions()
#endif
{
	char	*ftptool_extensions;
	char	*archive_viewer;
	char	*postscript_viewer;
	FILE	*fp;
	static char extension[MAXPATHLEN + 1];
	static char magic[MAXPATHLEN + 1];
	static char program[MAXPATHLEN + 1];
	int	type;

	extension_list = new_extension("", "", "", FILE_VIEWER);
	if (extension_list == NULL) {
		fprintf(stderr, "%s: Out of memory for extension list.\n",
			program_name);
		return;
	}

	ftptool_extensions = find_dotfile(FTPTOOL_TYPES);
	if (ftptool_extensions == NULL) {
		/* not there, use resources/create initial one */

		archive_viewer = defaults_get_string(
		    "ftptool.ArchiveViewer", "Ftptool.ArchiveViewer",
		    program_name);
		add_extension(extension_list, ".tar", "", archive_viewer,
		    FILE_VIEWER);

		postscript_viewer = defaults_get_string(
		    "ftptool.PostScriptViewer", "Ftptool.PostScriptViewer",
		    "pageview %f");
		add_extension(extension_list, ".ps", "%!", postscript_viewer,
		    FILE_VIEWER);

		/* other stuff */

		add_extension(extension_list, ".gif", "GIF", "xv %f",
		    FILE_VIEWER);

		add_extension(extension_list, ".jpg",
		    "\\377\\330\\377\\340\\000\\020\\112\\106\\111",
		    "xv %f", FILE_VIEWER);

		add_extension(extension_list, ".ras", "\\131\\246\\152\\225",
		    "xv %f", FILE_VIEWER);

		add_extension(extension_list, ".Z", "\\037\\235",
		    "compress -dvf %f", FILE_DECOMPRESSOR);

		add_extension(extension_list, ".z", "", "gzip -dvf %f",
		    FILE_DECOMPRESSOR);

		add_extension(extension_list, ".gz", "", "gzip -dvf %f",
		    FILE_DECOMPRESSOR);

		extensions_changed = 1;

	} else {
		/* load list */
		fp = fopen(ftptool_extensions, "r");
		if (fp == NULL) {
			fprintf(stderr,
			    "%s: Could not open %s for reading (%s).\n",
			    program_name, ftptool_extensions,
			    sys_errlist[errno]);
			free(ftptool_extensions);
			return;
		}
		type = -1;
		while (read_extensions(fp, extension, magic, program, &type)) {
			if (add_extension(extension_list, extension, magic,
			    program, type)
				== NULL)
				break;
		}
		fclose(fp);
		free(ftptool_extensions);
		if (type == -1) {
			/* no compressors */
			add_extension(extension_list,
				".Z", "\\037\\235", "compress -dvf %f",
			    FILE_DECOMPRESSOR);
			add_extension(extension_list,
				".z", "", "gzip -dvf %f", FILE_DECOMPRESSOR);
			add_extension(extension_list,
				".gz", "", "gzip -dvf %f", FILE_DECOMPRESSOR);
			extensions_changed = 1;
		}
	}

	clear_extension_list(tool_property_window.viewers.list);
	load_extension_list(extension_list, tool_property_window.viewers.list);
}

#ifdef USE_PROTOTYPES
void save_extensions(void)
#else
void save_extensions()
#endif
{
	char	*filename = NULL;
	struct extension_info *tmp;
	FILE	*fp;
	extern char *sys_errlist[];
	char	*typename;

	/* create will also truncate, which we want to do here */
	filename = create_dotfile(FTPTOOL_TYPES, 0644);
	if (filename == NULL) {
		fprintf(stderr, "%s: Could not create ftptooltypes file.\n",
			program_name);
		return;
	}

	fp = fopen(filename, "w");
	if (fp == NULL) {
		fprintf(stderr, "%s: Could not open %s for writing (%s).\n",
			program_name, filename, sys_errlist[errno]);
		free(filename);
		return;
	}
	for (tmp = extension_list->next; tmp != NULL; tmp = tmp->next) {
		fprintf(fp, "Extension:%s\n", tmp->extension);
		fprintf(fp, "Magic:%s\n", tmp->magic);
		fprintf(fp, "Program:%s\n", tmp->program);
		switch (tmp->type) {
		case FILE_DECOMPRESSOR:
			typename = "decompressor";
			break;
		case FILE_VIEWER:
		default:
			typename = "file viewer";
			break;
		}
		fprintf(fp, "Type:%s\n", typename);
		fprintf(fp, "--\n");
	}
	fclose(fp);

	extensions_changed = 0;

	free(filename);
}

#ifdef USE_PROTOTYPES
void load_extension_list(struct extension_info *extension_list,
	Panel panel_list)
#else
void load_extension_list(extension_list, panel_list)
struct extension_info *extension_list;
Panel	panel_list;
#endif
{
	struct extension_info *tmp;
	int		row;

	row = 0;
	for (tmp = extension_list->next; tmp != NULL; tmp = tmp->next, row++) {
		xv_set(panel_list,
			PANEL_LIST_INSERT, row,
			PANEL_LIST_STRING, row, tmp->extension,
			PANEL_LIST_CLIENT_DATA, row, tmp,
			PANEL_PAINT, PANEL_NONE,
			NULL);
	}
	xv_set(panel_list,
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void clear_extension_list(Panel panel_list)
#else
void clear_extension_list(panel_list)
Panel panel_list;
#endif
{
	int	nitems;
	int	row;

	nitems = xv_get(panel_list, PANEL_LIST_NROWS);

	for (row = nitems - 1; row >= 0; row--) {
		xv_set(panel_list,
			PANEL_LIST_DELETE, row,
			PANEL_PAINT, PANEL_NONE,
			NULL);
	}
	panel_paint(panel_list, PANEL_CLEAR);
}

#ifdef USE_PROTOTYPES
int extension_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int extension_list_proc(item, string, client_data, op, event)
Panel_item	item;
char		*string;
Xv_opaque	client_data;
Panel_list_op	op;
Event		*event;
#endif
{
	struct extension_info *tmp = (struct extension_info *)client_data;

	switch (op) {
	case PANEL_LIST_OP_SELECT:
		xv_set(tool_property_window.viewers.extension,
			PANEL_VALUE, tmp->extension,
			NULL);
		caret_to_first(tool_property_window.viewers.extension);
		xv_set(tool_property_window.viewers.magic,
			PANEL_VALUE, tmp->magic,
			NULL);
		caret_to_first(tool_property_window.viewers.magic);
		xv_set(tool_property_window.viewers.program,
			PANEL_VALUE, tmp->program,
			NULL);
		caret_to_first(tool_property_window.viewers.program);
		xv_set(tool_property_window.viewers.type,
			PANEL_VALUE, tmp->type,
			NULL);
		return (XV_OK);
	default:
		return (XV_OK);
	}
}

#ifdef USE_PROTOTYPES
struct extension_info *type_by_extension(char *filename)
#else
struct extension_info *type_by_extension(filename)
char	*filename;
#endif
{
	struct extension_info *tmp;
	unsigned int	len;
	char	*ext;

	for (tmp = extension_list->next; tmp != NULL; tmp = tmp->next) {
		len = strlen(tmp->extension);
		if (strlen(filename) < len)
			continue;
		ext = filename + (strlen(filename) - len);
		if (!strcmp(ext, tmp->extension))
			break;
	}
	return (tmp);

}

#ifdef USE_PROTOTYPES
struct extension_info *type_by_magic(char *filename)
#else
struct extension_info *type_by_magic(filename)
char	*filename;
#endif
{
	struct extension_info *tmp;
	int		fd;
	char	buf[MAX_MAGIC_SIZE];
	char	*chp;
	char	ch;
	int		nread;
	int		count;

	if ((fd = open(filename, O_RDONLY)) == -1) {
		return (NULL);
	}
	notify_do_dispatch();
	nread = read(fd, buf, sizeof (buf));
	notify_no_dispatch();
	close(fd);
	if (nread == -1) {
		return (NULL);
	}

	for (tmp = extension_list->next; tmp != NULL; tmp = tmp->next) {
		count = 0;
		chp = tmp->magic;
		ch = *chp;
		if (ch == '\0') /* null magic number */
			continue;
		/* test and advance */
		while (count < nread && ch != '\0') {
			if (ch == '\\') {
				int	n = 3;
				int	val = 0;

				/* octal escape sequence */
				chp++;
				while (isdigit(*chp) && n > 0) {
					val *= 8;
					val += *chp - '0';
					chp++;
					n--;
				}
				/* point back one for later */
				chp--;
				ch = (char)val;
			}
			if (buf[count] == ch) {
				/* match */
				count++;
				chp++;
				ch = *chp;
			} else
				break;
		}
		if (ch == '\0') {
			break;
		}
	}
	return (tmp);
}

#ifdef USE_PROTOTYPES
struct extension_info *type_by_either(char *filename)
#else
struct extension_info *type_by_either(filename)
char	*filename;
#endif
{
	struct extension_info *type_ext;
	struct extension_info *type_magic;


	type_ext = type_by_extension(filename);
	type_magic = type_by_magic(filename);
	if (type_ext != NULL && type_magic == NULL) {
		/* extension matched, go for it */
		return (type_ext);
	} else if (type_ext == NULL && type_magic != NULL) {
		/* magic number matched */
		return (type_magic);
	} else if (type_ext == NULL && type_ext == type_magic) {
		/* both NULL */
		return (NULL);
	} else if (type_ext == type_magic) {
		/* they match, great! */
		return (type_ext);
	} else {
		/* they both aren't null, and don't match! */
		/* choose magic number, it's probably more correct */
		return (type_magic);
	}
}


#ifdef USE_PROTOTYPES
void delete_extension_proc(Panel_item item, Event *event)
#else
void delete_extension_proc(item, event)
Panel_item	item;
Event 		*event;
#endif
{
	char	*extension;

	xv_set(tool_property_window.frame,
		FRAME_LEFT_FOOTER, "",
		NULL);

	extension = (char *)xv_get(tool_property_window.viewers.extension,
		PANEL_VALUE);
	if (*extension == '\0') {
		xv_set(tool_property_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Please specify an extension.",
			NULL);
		return;
	}

	delete_extension(extension_list, extension);

	clear_extension_list(tool_property_window.viewers.list);
	load_extension_list(extension_list, tool_property_window.viewers.list);

	xv_set(item,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void add_extension_proc(Panel_item item, Event *event)
#else
void add_extension_proc(item, event)
Panel_item	item;
Event 		*event;
#endif
{
	enter_extension_info(1);
	xv_set(item,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void change_extension_proc(Panel_item item, Event *event)
#else
void change_extension_proc(item, event)
Panel_item	item;
Event 		*event;
#endif
{
	enter_extension_info(0);
	xv_set(item,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void enter_extension_info(int warnchange)
#else
void enter_extension_info(warnchange)
int	warnchange;
#endif
{
	int	answer;
	char *extension, *magic, *program;
	int	type;
#ifdef XVIEW3
	Xv_notice	notice;
#endif

	xv_set(tool_property_window.frame,
		FRAME_LEFT_FOOTER, "",
		NULL);
	extension = (char *)xv_get(tool_property_window.viewers.extension,
	    PANEL_VALUE);
	if (*extension == '\0') {
		xv_set(tool_property_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Please specify an extension.",
			NULL);
		return;
	}
	magic = (char *)xv_get(tool_property_window.viewers.magic, PANEL_VALUE);
	program = (char *)xv_get(tool_property_window.viewers.program,
	    PANEL_VALUE);
	if (*program == '\0') {
		xv_set(tool_property_window.frame,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "Please specify a program to run.",
			NULL);
		return;
	}
	type = xv_get(tool_property_window.viewers.type, PANEL_VALUE);

	if (extension_exists(extension_list, extension)) {
		if (warnchange) {
#ifdef XVIEW3
			notice = xv_create(tool_property_window.viewers.panel,
				NOTICE,
				NOTICE_MESSAGE_STRINGS,
					"That extension exists. Do you really want to change it?",
					NULL,
				NOTICE_BUTTON_YES, "Yes",
				NOTICE_BUTTON_NO, "No",
				NOTICE_STATUS, &answer,
				XV_SHOW, TRUE,
				NULL);
			xv_destroy_safe(notice);
#else
			answer = notice_prompt(
			    tool_property_window.viewers.panel, NULL,
				NOTICE_MESSAGE_STRINGS,
					"That extension exists. Do you really want to change it?",
					NULL,
				NOTICE_BUTTON_YES, "Yes",
				NOTICE_BUTTON_NO, "No",
				NULL);
#endif
			if (answer != NOTICE_YES)
				return;
		}
		delete_extension(extension_list, extension);
	}
	extensions_changed = 1;
	add_extension(extension_list, extension, magic, program, type);
	clear_extension_list(tool_property_window.viewers.list);
	load_extension_list(extension_list, tool_property_window.viewers.list);
}

#ifdef USE_PROTOTYPES
void delete_extension(struct extension_info *head, char *extension)
#else
void delete_extension(head, extension)
struct extension_info *head;
char	*extension;
#endif
{
	struct extension_info *tmp, *deleted;

	for (tmp = head; tmp->next != NULL; tmp = tmp->next)
		if (!strcmp(extension, tmp->next->extension)) {
			deleted = tmp->next;
			tmp->next = deleted->next;
			deleted->next = NULL;
			free_extension(deleted);
			extensions_changed = 1;
			return;
		}
}

#ifdef USE_PROTOTYPES
int extension_exists(struct extension_info *head, char *extension)
#else
int extension_exists(head, extension)
struct extension_info *head;
char	*extension;
#endif
{
	struct extension_info *tmp;

	for (tmp = head->next; tmp != NULL; tmp = tmp->next)
		if (!strcmp(extension, tmp->extension))
			return (1);
	return (0);
}

#ifdef USE_PROTOTYPES
void uncompress(char *filename)
#else
void uncompress(filename)
char	*filename;
#endif
{
	char	*uncompressor;
	struct extension_info *ext_info;
	char	**argv;

	ext_info = type_by_either(filename);
	if (ext_info != NULL) {
		if (ext_info->type != FILE_DECOMPRESSOR) {
			local_footer_message("%s is not compressed.\n",
			    filename);
			return;
		}
		uncompressor = ext_info->program;
	} else {
		local_footer_message("Unknown file type, cannot decompress.\n");
		return;
	}
	argv = build_argv(uncompressor, filename);
	pipe_program(argv);
	free_argv(argv);
}

#ifdef USE_PROTOTYPES
void compress(char *filename)
#else
void compress(filename)
char	*filename;
#endif
{
	char	**argv;

	argv = build_argv(default_compressor, filename);
	pipe_program(argv);
	free_argv(argv);
}
