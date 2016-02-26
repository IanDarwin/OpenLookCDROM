#include "ftptool.h"

#pragma ident   "@(#)tar_view.c 1.4     93/05/27"

static Panel_button_item	list_button;
static Panel_button_item	extract_button;
static Textsw	tar_output;
static Frame	cd_frame;
static Panel	cd_panel;
static Panel	cd_text;

static char *tarfile;

static int insert_error;

static unsigned short tar_icon_array[] = {
#include "./tar.viewer.icon"
};

#ifdef USE_PROTOTYPES
void handle_tarfile(char *filename)
#else
void handle_tarfile(filename)
char	*filename;
#endif
{
	char	dir[MAXPATHLEN + 2];
	Panel_button_item ex_button;
	Rect	*butrect;
	Icon	frame_icon;
	Server_image tar_glyph;

	if (getcwd(dir, sizeof (dir)) == NULL) {
		fprintf(stderr, "getwd: %s\n", dir);
		exit(1);
	}

	if (filename[0] != '/') {
		/* should be in current directory. */
		tarfile = (char *)malloc((unsigned int)(strlen(dir) + 1
		    + strlen(filename) + 1));
		if (tarfile == NULL) {
			fprintf(stderr, "malloc failed\n");
			exit(1);
		}
		sprintf(tarfile, "%s/%s", dir, filename);
	} else
		tarfile = strdup(filename);

	sprintf(scratch, "Tar File Viewer - %s", filename);

	base_window.frame = xv_create(XV_NULL, FRAME,
		XV_LABEL, scratch,
		XV_WIDTH, 430,
		NULL);

	tar_glyph = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, tar_icon_array,
		NULL);

	frame_icon = (Icon)xv_get(base_window.frame, FRAME_ICON);

	xv_set(frame_icon,
		ICON_IMAGE, tar_glyph,
		ICON_TRANSPARENT, TRUE,
		NULL);


	base_window.panel = xv_create(base_window.frame, PANEL,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		XV_HELP_DATA, "ftptool:TarFileViewer",
		NULL);

	list_button = xv_create(base_window.panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "List Contents",
		PANEL_NOTIFY_PROC, list_proc,
		XV_HELP_DATA, "ftptool:TarFileListContents",
		NULL);

	extract_button = xv_create(base_window.panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Extract Files",
		PANEL_NOTIFY_PROC, tar_extract_proc,
		XV_HELP_DATA, "ftptool:TarFileExtractFiles",
		NULL);

	openlook_mode = defaults_get_boolean(
		"ftptool.OpenLookMode", "Ftptool.OpenLookMode", TRUE);

	if (!openlook_mode) {
		tar_quit_button = xv_create(base_window.panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Quit",
			PANEL_NOTIFY_PROC, tar_quit_proc,
			XV_HELP_DATA, "ftptool:TarQuitButton",
			NULL);
	}

	xv_set(base_window.panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
	window_fit_height(base_window.panel);

	tar_output = xv_create(base_window.frame, TEXTSW,
		XV_LABEL, "Tar Output",
		XV_HEIGHT, 100,
		NULL);

	window_fit(base_window.frame);

	xv_set(base_window.frame, XV_SHOW, TRUE, NULL);

	cd_frame = xv_create(base_window.frame, FRAME_CMD,
		XV_LABEL, "Extract Directory",
		NULL);

	dpy = (Display *)xv_get(base_window.frame, XV_DISPLAY);
	cd_panel = xv_get(cd_frame, FRAME_CMD_PANEL);

	xv_set(cd_panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	cd_text = xv_create(cd_panel, PANEL_TEXT,
		PANEL_VALUE_DISPLAY_LENGTH, 40,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN + 1,
		PANEL_LABEL_STRING, "Directory: ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE, dir,
		XV_HELP_DATA, "ftptool:TarFileNameText",
		NULL);

	ex_button = xv_create(cd_panel, PANEL_BUTTON,
		PANEL_NOTIFY_PROC, doextract_proc,
		PANEL_LABEL_STRING, "Extract",
		XV_HELP_DATA, "ftptool:TarFileNameButton",
		NULL);

	xv_set(cd_panel,
		PANEL_DEFAULT_ITEM, ex_button,
		NULL);

	window_fit(cd_panel);
	window_fit(cd_frame);

	butrect = (Rect *)xv_get(ex_button, XV_RECT);
	xv_set(ex_button,
		XV_X, (int)xv_get(cd_panel, XV_WIDTH)/2 - butrect->r_width,
		NULL);

	notify_interpose_destroy_func(base_window.frame, tar_destroy_func);
	XFlush(dpy);
	notify_start();

	exit(0);
}

#ifdef USE_PROTOTYPES
void list_proc(void)
#else
void list_proc()
#endif
{
	xv_set(base_window.frame,
		FRAME_BUSY, TRUE,
		NULL);
	start_tar("tf", tarfile);
}

#ifdef USE_PROTOTYPES
void doextract_proc(void)
#else
void doextract_proc()
#endif
{
	char	*dir;
	extern char *sys_errlist[];
	int		rval;

	xv_set(base_window.frame,
		FRAME_BUSY, TRUE,
		NULL);
	dir = (char *)xv_get(cd_text, PANEL_VALUE);
	if (dir[0] == '\0') {
		footer_message("Please type in a directory name.");
		xv_set(base_window.frame,
			FRAME_BUSY, FALSE,
			NULL);
		return;
	}
	dir = expand_dirname(dir);
	if (dir == NULL) {
		fprintf(stderr, "Out of memory.\n");
		xv_set(base_window.frame,
			FRAME_BUSY, FALSE,
			NULL);
		return;
	}
	if (chdir(dir) == -1) {
		if (errno == ENOENT) {
			if ((rval = ask_make_dir(dir)) != 0) {
				if (rval != -1) {
					footer_message(
					    "Could not make directory. Reason: %s",
					    sys_errlist[rval]);
				}
				xv_set(base_window.frame,
					FRAME_BUSY, FALSE,
					NULL);
				return;
			}
			if (chdir(dir) == -1) {
				perror("chdir");
				exit(1);
			}
		} else {
			footer_message("Could not make directory. Reason: %s",
			    sys_errlist[errno]);
			xv_set(base_window.frame,
				FRAME_BUSY, FALSE,
				NULL);
			return;
		}
	}
	free(dir);
	start_tar("xvf", tarfile);
}

#ifdef USE_PROTOTYPES
void tar_extract_proc(void)
#else
void tar_extract_proc()
#endif
{
	int	x = xv_get(base_window.frame, XV_X) - 5;
	int y = xv_get(base_window.frame, XV_Y);
	int height = xv_get(cd_frame, XV_HEIGHT)  * 2;

	y -= height;
	if (x < 0 || x >= display_width)
		x = 0;
	if (y < 0 || y >= display_height)
		y = 0;
	xv_set(cd_frame,
		XV_X, x,
		XV_Y, y,
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void start_tar(char *options, char *filename)
#else
void start_tar(options, filename)
char	*options;
char	*filename;
#endif
{
	int		pipe_fds[2];
	int		pid;

	textsw_erase(tar_output, 0, TEXTSW_INFINITY);
	if (pipe(pipe_fds) == -1) {
		perror("pipe");
		exit(1);
	}
	insert_error = 0;
	switch (pid = fork()) {
	case -1:
		perror("fork");
		exit(1);
		break;
	case 0: /* child */
		/* dup standard out to pipe */
		dup2(pipe_fds[1], 1);
		dup2(pipe_fds[1], 2);
		close(pipe_fds[0]);
		close(pipe_fds[1]);
		execlp("tar", "tar", options, filename, (char *)NULL);
		perror("can not start viewer");
		exit(1);
	default: /* parent */
		close(pipe_fds[1]);
		notify_set_wait3_func(base_window.frame,
		    notify_default_wait3, pid);

		break;
	}

	notify_set_input_func(base_window.frame, input_func, pipe_fds[0]);
	notify_set_wait3_func(base_window.frame, notify_default_wait3, pid);
}

#ifdef USE_PROTOTYPES
Notify_value input_func(Notify_client client, int fd)
#else
Notify_value input_func(client, fd)
Notify_client	client;
int	fd;
#endif
{
	char	buf[80];
	int		nread;

	for (;;) {
		nread = read(fd, buf, sizeof (buf));
		if (nread == 0) {
			notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);
			close(fd);
			xv_set(base_window.frame, FRAME_BUSY, FALSE, NULL);
			break;
		}
		if (nread == -1)
			if (errno == EWOULDBLOCK)
				break;
			else {
				perror("read");
				exit(1);
			}
		if (!insert_error && textsw_insert(tar_output, buf,
		    nread) != nread)
			insert_error = 1;
		XFlush(dpy);
	}

	return (NOTIFY_DONE);
}

#ifdef USE_PROTOTYPES
Notify_value tar_destroy_func(Notify_client client, Destroy_status status)
#else
Notify_value tar_destroy_func(client, status)
Notify_client   client;
Destroy_status  status;
#endif
{
	switch (status) {
	case DESTROY_CHECKING:
		break;
	case DESTROY_CLEANUP:
		return (notify_next_destroy_func(client, status));
		break;
	case DESTROY_SAVE_YOURSELF:
		break;
	case DESTROY_PROCESS_DEATH:
		break;
	}
	return (NOTIFY_DONE);
}


#ifdef USE_PROTOTYPES
void	tar_quit_proc(void)
#else
void	tar_quit_proc()
#endif
{
	exit(0);
}
