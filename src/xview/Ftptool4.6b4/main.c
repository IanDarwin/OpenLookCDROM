
#define	EXTERN

#include "ftptool.h"

#pragma ident   "%Z%%M% %I%     %E%"

unsigned short icon_array[] = {
#include "./ftptool.icon"
};

static unsigned short busy1_icon_array[] = {
#include "./busy1.ftptool.icon"
};

static unsigned short busy2_icon_array[] = {
#include "./busy2.ftptool.icon"
};

static unsigned short busy3_icon_array[] = {
#include "./busy3.ftptool.icon"
};

static unsigned short busy4_icon_array[] = {
#include "./busy4.ftptool.icon"
};

static unsigned short directory_pr_array[] = {
#include "./directory.pr"
};

static unsigned short file_pr_array[] = {
#include "./file.pr"
};

static unsigned short dotdot_pr_array[] = {
#include "./dotdot.pr"
};

static unsigned short link_pr_array[] = {
#include "./link.pr"
};

static unsigned short unknown_pr_array[] = {
#include "./unknown.pr"
};

int nbusyicons = 4;
Server_image busy_glyphs[4];

Server_image ftptool_glyph;

static struct itimerval date_timer = {
	{300, 0},
	{300, 0},
};

#ifdef USE_PROTOTYPES
int main(int argc, char **argv)
#else
int main(argc, argv)
int		argc;
char	**argv;
#endif
{
	char	*helppath;
	int	closed = 0;
	struct servent *servent;

	/* FTP inits */
	data = -1;
	abrtflag = 0;
	verbose = 1;
	debug = 0;
	code = -1;
	cpend = 0;
	curtype = 0;
	crflag = 1;
	runique = 0;
	sendport = -1;

	bzero((char *)&local_dircache, sizeof (local_dircache));
	bzero((char *)&remote_dircache, sizeof (remote_dircache));

	bzero((char *)&schedule_list, sizeof (schedule_list));

	program_name = argv[0];

	/*
	 * Determine port numbers to use
	 */
	servent = getservbyname("ftp", "tcp");
	if (servent != NULL) {
		ftp_port = servent->s_port;
	} else {
		fprintf(stderr, "Couldn't find 'ftp' service. Using %d.\n",
			FTP_PORT);
		ftp_port = htons(FTP_PORT);
	}
	servent = getservbyname("ftp-passthru", "tcp");
	if (servent != NULL) {
		ftp_passthru_port = servent->s_port;
	} else {
		ftp_passthru_port = htons(FTP_PASSTHRU_PORT);
	}

	try_proxy = 1;

	sprintf(scratch, "Ftptool Version %d.%d (Beta4)", VERSION, PATCHLEVEL);
	/*
	sprintf(scratch, "Ftptool Version %d.%d", VERSION, PATCHLEVEL);
	 */
	header_name = strdup(scratch);
	if (header_name == NULL) {
		perror("malloc");
		exit(1);
	}

	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	if ((helppath = getenv("HELPPATH")) == NULL) {
		putenv("HELPPATH=/usr/local/lib/help:.");
	} else {
		sprintf(scratch, "HELPPATH=%s:/usr/local/lib/help:.", helppath);
		putenv(strdup(scratch));
	}

	load_xdefaults();

	netrc_filename = find_dotfile(".netrc");

	if (argc > 1) {
		handle_tarfile(argv[1]);
	}

	/* initialize UNIX pattern */
	unix_dir_pattern = dir_parse_to_pattern(UNIX_DIR_PATTERN);
	if (unix_dir_pattern == NULL) {
		perror("malloc");
		exit(1);
	}
	other_dir_pattern = NULL;

	ftptool_glyph = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, icon_array,
		NULL);

	directory_glyph = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, directory_pr_array,
		NULL);

	file_glyph = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, file_pr_array,
		NULL);

	dotdot_glyph = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, dotdot_pr_array,
		NULL);

	link_glyph = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, link_pr_array,
		NULL);

	unknown_glyph = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, unknown_pr_array,
		NULL);

	create_base_window();

	frame_icon = (Icon)xv_get(base_window.frame, FRAME_ICON);
	xv_set(frame_icon,
		ICON_IMAGE, ftptool_glyph,
		ICON_TRANSPARENT, TRUE,
		NULL);

	normal_cursor = xv_create(base_window.frame,
		CURSOR,
		CURSOR_SRC_CHAR, OLC_BASIC_PTR,
		CURSOR_MASK_CHAR, OLC_BASIC_MASK_PTR,
		CURSOR_OP, PIX_SRC | PIX_DST,
		NULL);

	busy_cursor = xv_create(base_window.frame,
		CURSOR,
		CURSOR_SRC_CHAR, OLC_BUSY_PTR,
		CURSOR_MASK_CHAR, OLC_BUSY_MASK_PTR,
		CURSOR_OP, PIX_SRC | PIX_DST,
		NULL);

	busy_glyphs[0] = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, busy1_icon_array,
		NULL);

	busy_glyphs[1] = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, busy2_icon_array,
		NULL);

	busy_glyphs[2] = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, busy3_icon_array,
		NULL);

	busy_glyphs[3] = (Server_image)xv_create(XV_NULL,
		SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, busy4_icon_array,
		NULL);

	create_local_window();

	create_host_popup();

	create_session_log();

	create_property_window();

	create_file_property_window(&local_file_properties,
		"Ftptool:Local File Properties");
	create_file_property_window(&remote_file_properties,
		"Ftptool:Remote File Properties");

	create_tar_file_popup();

	create_about_window();

	create_feedback_window();

	create_schedule_window();

	create_status_window();


	load_extensions();

	/* make it so ftp doesn't blow when the data connection is closed */
	/* if the remote filesystem fills up. */
	signal(SIGPIPE, SIG_IGN);

	/*
	 * Turn control over to XView.
	 */
	notify_interpose_destroy_func(base_window.frame, destroy_func);
	notify_set_signal_func(base_window.frame, sig_func, SIGINT,
	    NOTIFY_SYNC);
	notify_set_itimer_func(schedule_window.current_time, date_wrapper,
	    ITIMER_REAL, &date_timer, (struct itimerval *)NULL);
	change_local_dir(".", 0);

	xv_set(base_window.frame,
		XV_SHOW, TRUE,
		NULL);

	XFlush(dpy);

	update_date(1);

	closed = xv_get(base_window.frame, FRAME_CLOSED);

	XFlush(dpy);

	if (host_window.visible) {
		xv_set(host_window.frame,
			XV_SHOW, TRUE,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			FRAME_CLOSED, closed,
			NULL);
	}
	if (host_window.advanced.visible)
		plus_proc();

	XFlush(dpy);

	if (local_window.visible) {
		xv_set(local_window.frame,
			XV_SHOW, TRUE,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			FRAME_CLOSED, closed,
			NULL);
	}

	XFlush(dpy);

	if (schedule_window.visible) {
		xv_set(schedule_window.frame,
			XV_SHOW, TRUE,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			FRAME_CLOSED, closed,
			NULL);
	}

	XFlush(dpy);

	if (status_window.visible) {
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			FRAME_CLOSED, closed,
			NULL);
	}

	XFlush(dpy);

	if (logging) {
		xv_set(session_window.frame,
			XV_SHOW, TRUE,
#ifdef XVIEW3
			FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
			FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
			FRAME_CLOSED, closed,
			NULL);
	}

	XFlush(dpy);

	for (;;) {
		XFlush(dpy);
		notify_start();
		abort_transfer = 0;
		switch (dowhat) {
		case DOCONNECT:
			(void) doconnect();
			break;
		case DOGET:
			doget();
			break;
		case DOBATCHGET:
			dobatchget();
			break;
		case DOPUT:
			doput();
			break;
		case DOBATCHPUT:
			dobatchput();
			break;
		case DOLOCALVIEW:
		case DOREMOTEVIEW:
			doview(dowhat);
			break;
		case DOUNCOMPRESS:
		case DOEXTRACT:
		case DOCOMPRESS:
		case DOTAR:
		case DOGETTARFILENAME:
			dofileop(dowhat);
			break;
		case DOREMOTECD:
			doremotecd(0);
			break;
		case DOREMOTECDFORCE:
			doremotecd(1);
			break;
		case DOREMOTEDOUBLECLICK:
			remote_doubleclick();
			break;
		case DOSCHEDULE:
			doschedule();
			break;
		case DODIR:
		case DOLS:
			list_remote_dir();
			break;
		case DOQUIT:
			exit(0);
			break;
		}
	}
}

#ifdef USE_PROTOTYPES
void load_xdefaults(void)
#else
void load_xdefaults()
#endif
{
	char	*str;
	int		ct;
	char	*xapplresdir, res_file[MAXPATHLEN + 10];
	char	*ftptool_defaults;
	char	*ftptool_layout;

	if ((xapplresdir = getenv("XAPPLRESDIR")) != NULL) {
		sprintf(res_file, "%s/Ftptool", xapplresdir);
		if (access(res_file, R_OK) == 0) {
			defaults_load_db(res_file);
		}
	}
	ftptool_defaults = find_dotfile(FTPTOOL_DEFAULTS);
	if (ftptool_defaults) {
		defaults_load_db(ftptool_defaults);
		free(ftptool_defaults);
	}
	ftptool_layout = find_dotfile(FTPTOOL_LAYOUT);
	if (ftptool_layout) {
		defaults_load_db(ftptool_layout);
		free(ftptool_layout);
	}

	ct = defaults_get_integer(
	    "openWindows.multiClickTimeout",
	    "OpenWindows.MultiClickTimeout", 4);

	click_timeout = ct/10.0;

	drag_threshold = defaults_get_integer(
	    "openWindows.dragThreshold", "OpenWindows.DragThreshold", 5);

	logging = defaults_get_boolean(
	    "ftptool.LogSession", "Ftptool.LogSession", FALSE);

	keepalive = defaults_get_boolean(
	    "ftptool.KeepConnectionAlive", "Ftptool.KeepConnectionAlive",
	    FALSE);

	if (keepalive != FALSE)
		keepalive = 1 << 1;

	auto_connect = defaults_get_boolean(
	    "ftptool.AutoConnect", "Ftptool.AutoConnect", FALSE);

	show_status = defaults_get_boolean(
	    "ftptool.AutoShowStatus", "Ftptool.AutoShowStatus", FALSE);

	if (show_status != FALSE)
		show_status = 1 << 1;

	try_proxy = defaults_get_boolean(
		"ftptool.TryProxy", "Ftptool.TryProxy", FALSE);

	if (try_proxy != FALSE)
		try_proxy = 1 << 2;

	unique_local_names = defaults_get_boolean(
	    "ftptool.GenUniqueLocalNames", "Ftptool.GenUniqueLocalNames",
	    FALSE);

	unique_remote_names = defaults_get_boolean(
	    "ftptool.GenUniqueRemoteNames", "Ftptool.GenUniqueRemoteNames",
	    FALSE);

	if (unique_remote_names != FALSE)
		unique_remote_names = 1 << 1;

	str = defaults_get_string(
		"ftptool.TransferMode", "Ftptool.TransferMode", "Binary");

	confirmdeletes = defaults_get_boolean(
		"ftptool.ConfirmDeletes", "Ftptool.ConfirmDeletes", TRUE);

	confirmoverwrites = defaults_get_boolean(
	    "ftptool.ConfirmOverWrites", "Ftptool.ConfirmOverWrites",
	    FALSE);

	if (confirmoverwrites != FALSE)
		confirmoverwrites = 1 << 1;

	dircache_size = defaults_get_integer_check(
	    "ftptool.DirectoryCacheSize", "Ftptool.DirectoryCacheSize",
	    5, 0, 20);

	str = defaults_get_string(
	    "ftptool.SortField", "Ftptool.SortField", "Name");

	remote_sort_mode = SORTBYNAME;

	if (!strcmp(str, "Date"))
		remote_sort_mode = SORTBYDATE;
	else if (!strcmp(str, "Size"))
		remote_sort_mode = SORTBYSIZE;

	remote_sort_direction = defaults_get_integer(
		"ftptool.SortOrder", "Ftptool.SortOrder", ASCENDING);

	remote_showdotfiles = defaults_get_boolean(
		"ftptool.ShowHiddenFiles", "Ftptool.ShowHiddenFiles", FALSE);

	group_remote_files  = defaults_get_boolean(
		"ftptool.SortGrouping", "Ftptool.SortGrouping", FALSE);

	str = defaults_get_string(
		"ftptool.LocalSortField", "Ftptool.LocalSortField", "Name");

	local_sort_mode = SORTBYNAME;

	if (!strcmp(str, "Date"))
		local_sort_mode = SORTBYDATE;
	else if (!strcmp(str, "Size"))
		local_sort_mode = SORTBYSIZE;

	local_sort_direction = defaults_get_integer(
	    "ftptool.LocalSortOrder", "Ftptool.LocalSortOrder", ASCENDING);

	local_showdotfiles = defaults_get_boolean(
	    "ftptool.LocalShowHiddenFiles", "Ftptool.LocalShowHiddenFiles",
	    FALSE);

	group_local_files  = defaults_get_boolean(
	    "ftptool.LocalSortGrouping", "Ftptool.LocalSortGrouping", FALSE);

	anonftp_password = defaults_get_string(
	    "ftptool.InitialPassword", "Ftptool.InitialPassword",
	    (char *)NULL);


	if (anonftp_password)
		anonftp_password = strdup(anonftp_password);

	default_viewer = defaults_get_string(
	    "ftptool.DefaultViewer", "Ftptool.DefaultViewer", "textedit %f");

	default_viewer = strdup(default_viewer);

	default_compressor = defaults_get_string(
		"ftptool.DefaultCompressor", "Ftptool.DefaultCompressor",
		"compress -vf %f");

	default_compressor = strdup(default_compressor);

	openlook_mode = defaults_get_boolean(
		"ftptool.OpenLookMode", "Ftptool.OpenLookMode", TRUE);

	ignore_case = defaults_get_boolean(
		"ftptool.IgnoreCase", "Ftptool.IgnoreCase", FALSE);

	/* Windows */

	host_window.visible = defaults_get_boolean(
	    "ftptool.HostInfoVisible", "Ftptool.HostInfoVisible", FALSE);

	host_window.advanced.visible = defaults_get_boolean(
	    "ftptool.HostInfoAdvancedVisible",
	    "Ftptool.HostInfoAdvancedVisible", FALSE);

	local_window.visible = defaults_get_boolean(
	    "ftptool.LocalWindowVisible", "Ftptool.LocalWindowVisible",
	    FALSE);

	schedule_window.visible = defaults_get_boolean(
	    "ftptool.BatchWindowVisible", "Ftptool.BatchWindowVisible",
	    FALSE);

	status_window.visible = defaults_get_boolean(
	    "ftptool.StatusWindowVisible", "Ftptool.StatusWindowVisible",
	    FALSE);

	strcpy(base_window.geometry,
	    defaults_get_string("ftptool.RemoteWindowGeometry",
		"Ftptool.RemoteWindowGeometry", ""));

	strcpy(local_window.geometry,
	    defaults_get_string("ftptool.LocalWindowGeometry",
		"Ftptool.LocalWindowGeometry", ""));

	strcpy(host_window.geometry,
	    defaults_get_string("ftptool.HostWindowGeometry",
		"Ftptool.HostWindowGeometry", ""));

	strcpy(schedule_window.geometry,
	    defaults_get_string("ftptool.BatchWindowGeometry",
		"Ftptool.BatchWindowGeometry", ""));

	strcpy(session_window.geometry,
	    defaults_get_string("ftptool.SessionWindowGeometry",
		"Ftptool.SessionWindowGeometry", ""));

	strcpy(status_window.geometry,
	    defaults_get_string("ftptool.StatusWindowGeometry",
		"Ftptool.StatusWindowGeometry", ""));
}

#ifdef USE_PROTOTYPES
void set_xdefaults(void)
#else
void set_xdefaults()
#endif
{
	char	*str;

	defaults_set_boolean("ftptool.LogSession", logging & 0x1);

	defaults_set_boolean("ftptool.KeepConnectionAlive", keepalive & 0x2);

	defaults_set_boolean("ftptool.GenUniqueLocalNames",
	    unique_local_names & 0x1);

	defaults_set_boolean("ftptool.GenUniqueRemoteNames",
		unique_remote_names & 0x2);

	defaults_set_boolean("ftptool.ConfirmDeletes", confirmdeletes);

	defaults_set_boolean("ftptool.ConfirmOverWrites",
	    confirmoverwrites & 0x2);

	defaults_set_boolean("ftptool.AutoConnect", auto_connect & 0x1);

	defaults_set_boolean("ftptool.AutoShowStatus", show_status & 0x2);

	defaults_set_boolean("ftptool.TryProxy", try_proxy & 0x4);

	defaults_set_integer("ftptool.DirectoryCacheSize", dircache_size);

	switch (remote_sort_mode) {
	default:
		fprintf(stderr, "Unknown remote sort mode in set_xdefaults.\n");
		/* Fall through */
	case SORTBYNAME:
		str = "Name";
		break;
	case SORTBYDATE:
		str = "Date";
		break;
	case SORTBYSIZE:
		str = "Size";
		break;
	}
	defaults_set_string("ftptool.SortField", str);

	defaults_set_integer("ftptool.SortOrder", remote_sort_direction);

	defaults_set_boolean("ftptool.ShowHiddenFiles", remote_showdotfiles);

	defaults_set_boolean("ftptool.SortGrouping", group_remote_files);

	switch (local_sort_mode) {
	default:
		fprintf(stderr, "Unknown local sort mode in set_xdefaults.\n");
		/* Fall through */
	case SORTBYNAME:
		str = "Name";
		break;
	case SORTBYDATE:
		str = "Date";
		break;
	case SORTBYSIZE:
		str = "Size";
		break;
	}

	defaults_set_string("ftptool.LocalSortField", str);

	defaults_set_integer("ftptool.LocalSortOrder", local_sort_direction);

	defaults_set_boolean("ftptool.LocalShowHiddenFiles",
	    local_showdotfiles);

	defaults_set_boolean("ftptool.LocalSortGrouping", group_local_files);

	defaults_set_boolean("ftptool.OpenLookMode", openlook_mode);

	defaults_set_string("ftptool.DefaultViewer", default_viewer);

	defaults_set_string("ftptool.DefaultCompressor", default_compressor);

	defaults_set_string("ftptool.InitialPassword", anonftp_password);

}

#ifdef USE_PROTOTYPES
void save_xdefaults(void)
#else
void save_xdefaults()
#endif
{
	char    *filename = NULL;
	FILE	*fp;
	char	*str;
	char	*true = "True";
	char	*false = "False";

	filename = find_dotfile(FTPTOOL_DEFAULTS);
	if (filename == NULL)
		if ((filename = create_dotfile(FTPTOOL_DEFAULTS, 0644)) == NULL)
			return;
	if ((fp = fopen(filename, "w")) == NULL) {
		footer_message("Could not save defaults: %s",
		    sys_errlist[errno]);
		return;
	}
	fprintf(fp, "ftptool.AutoConnect:\t%s\n",
		(auto_connect == 0) ? false : true);
	fprintf(fp, "ftptool.AutoShowStatus:\t%s\n",
		((show_status & 2) == 0) ? false : true);
	fprintf(fp, "ftptool.ConfirmDeletes:\t%s\n",
		(confirmdeletes == 0) ? false : true);
	fprintf(fp, "ftptool.ConfirmOverWrites:\t%s\n",
		(confirmoverwrites == 0) ? false : true);
	fprintf(fp, "ftptool.DefaultCompressor:\t%s\n", default_compressor);
	fprintf(fp, "ftptool.DefaultViewer:\t%s\n", default_viewer);
	fprintf(fp, "ftptool.DirectoryCacheSize:\t%d\n", dircache_size);
	fprintf(fp, "ftptool.GenUniqueLocalNames:\t%s\n",
		((unique_local_names & 1) == 0) ? false : true);
	fprintf(fp, "ftptool.GenUniqueRemoteNames:\t%s\n",
		((unique_remote_names & 2) == 0) ? false : true);
	fprintf(fp, "ftptool.IgnoreCase:\t%s\n",
		(ignore_case == 0) ? false : true);
	fprintf(fp, "ftptool.InitialPassword:\t%s\n", anonftp_password);
	fprintf(fp, "ftptool.KeepConnectionAlive:\t%s\n",
		((keepalive & 2) == 0) ? false : true);

	fprintf(fp, "ftptool.LocalShowHiddenFiles:\t%s\n",
		(local_showdotfiles == 0) ? false : true);

	switch (local_sort_mode) {
	default:
		fprintf(stderr, "Unknown local sort mode in save_xdefaults.\n");
		/* Fall through */
	case SORTBYNAME:
		str = "Name";
		break;
	case SORTBYDATE:
		str = "Date";
		break;
	case SORTBYSIZE:
		str = "Size";
		break;
	}
	fprintf(fp, "ftptool.LocalSortField:\t%s\n", str);
	fprintf(fp, "ftptool.LocalSortGrouping:\t%d\n", group_local_files);
	fprintf(fp, "ftptool.LocalSortOrder:\t%d\n", local_sort_direction);

	fprintf(fp, "ftptool.LogSession:\t%s\n",
		((logging & 1) == 0) ? false : true);
	fprintf(fp, "ftptool.OpenLookMode:\t%s\n",
		(openlook_mode == 0) ? false : true);

	fprintf(fp, "ftptool.ShowHiddenFiles:\t%s\n",
		(remote_showdotfiles == 0) ? false : true);
	switch (remote_sort_mode) {
	default:
		fprintf(stderr,
		    "Unknown remote sort mode in save_xdefaults.\n");
		/* Fall through */
	case SORTBYNAME:
		str = "Name";
		break;
	case SORTBYDATE:
		str = "Date";
		break;
	case SORTBYSIZE:
		str = "Size";
		break;
	}
	fprintf(fp, "ftptool.SortField:\t%s\n", str);
	fprintf(fp, "ftptool.SortGrouping:\t%d\n", group_remote_files);
	fprintf(fp, "ftptool.SortOrder:\t%d\n", remote_sort_direction);

	fprintf(fp, "ftptool.TryProxy:\t%s\n",
		(try_proxy == 0) ? false : true);

	fclose(fp);
	footer_message("Defaults saved to %s.", filename);
	free(filename);
}
