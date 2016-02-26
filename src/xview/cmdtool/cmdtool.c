#ifndef lint
#ifdef sccs
static  char sccsid[] = "@(#)cmdtool.c 15.44 90/06/21";
#endif
#endif

/*
 * Copyright (c) 1985, 1987 by Sun Microsystems, Inc.
 */

/*
 *  cmd/shelltool - run a process in a tty subwindow
 */

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <xview/attr.h>
#include <xview/defaults.h>
#include <xview/xview.h>
#include <xview/scrollbar.h>
#include <xview/tty.h>
#include <xview/termsw.h>
#include <xview/textsw.h>
#ifdef ecd_help
#include <xview/help.h>
#endif

#ifdef STANDALONE
#define EXIT(n)		exit(n)
#else
#define EXIT(n)		return(n)
#endif

#define HEIGHTADJUST 19

extern	char *getenv();

#ifdef SUNVIEW1
extern  int  ttysw_is_already_dead();
#endif

static unsigned short tty_image[258] = {
#include <images/terminal.icon>
};

static unsigned short tty_mask_image[258] = {
#include <images/terminal_mask.icon>
};

static unsigned short cmd_image[258] = {
#include <images/cmdtool.icon>
};

static unsigned short cmd_mask_image[258] = {
#include <images/cmdtool_mask.icon>
};


print_usage(am_cmdtool, toolname)
	int	 am_cmdtool;
	char	*toolname;
{
	char	*mode_spec = (am_cmdtool)
				? "-P frequency] [-W size" :
				"-B boldstyle";
	
	(void)fprintf(stderr,
		"syntax: %s [-C] [-I initial_cmd] [%s] [program [args]]\n",
		toolname, mode_spec);
	if (!am_cmdtool) {
	    (void)fprintf(stderr,
		"-B	set bold style for this instance of %s\n", toolname);
	    (void)fprintf(stderr,
		"	where 'boldstyle' is a number from 1 to 8\n");
	}
	(void)fprintf(stderr,
		"-C	redirect console output to this instance of %s\n",
		toolname);
	(void)fprintf(stderr,
		"-I	'initial_cmd' is first command 'program' executes\n");
	(void)fprintf(stderr,
		"-L	run the .login file when starting the shell subprocess\n");
	if (am_cmdtool) {
	    (void)fprintf(stderr,
		"-P	checkpoint frequency for this %s, %s\n",
		toolname, "where 'frequency' is number");
	    (void)fprintf(stderr,
		"	of edits between checkpoints; %s\n",
		"a value of 0 means no checkpointing.");
	    (void)fprintf(stderr,
		"-W	wrap edit log for this %s %s\n", toolname,
		"at 'size' bytes; size is either");
	    (void)fprintf(stderr,
		"	at least 8096, or 0 which means no wrapping.\n");
	}
}

#ifdef STANDALONE
main(argc,argv)
#else
int shelltool_main(argc, argv)
#endif STANDALONE
	int argc;
	char **argv;
{
	int	am_cmdtool;
	Frame	base_frame;
	Tty	ttysw;
	Icon	tool_icon;
	char	*tool_name = argv[0];
	char	*shell_label = "shelltool";
	char	*cmd_label = "cmdtool";
	char	*console_label = " (CONSOLE) - ";
	char	frame_label[150];
	char	icon_label[30];
        char    *tmp_label1, *tmp_label2;
	int	become_console = 0;
	int	run_login	= 0;
	char	*bold_name = 0;
	char	*sh_argv[2];
	char	*init_cmd = 0;
	int	len;
	char	*filename = (char *)rindex(argv[0], '/');
	int	checkpoint = 0;
	int	edit_log_wraps_at = TEXTSW_INFINITY;
	int	tty_pid = 0;
	char	err_msg[50];
	static  Notify_value my_destroy_func();
	char    cmdline[50];
	char	cmdline2[50];
	Server_image  cmd_pixmap, cmd_mask_pixmap;

	
#ifdef GPROF
	if (argc > 1 && strcmp(argv[argc-1], "-gprof") == 0) {
	    moncontrol(1);
	    /* Pull the -gprof out of argc/v */
	    argc--;
	    argv[argc] = (char *)0;
	} else {
	    moncontrol(0);
	}
#endif
	
#ifdef DEBUG
	malloc_debug(0);   
#endif	
	
	/* This is required to initialize correctly */	
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

	if (filename)
		filename++;
	else
		filename = argv[0];
	am_cmdtool = (strcmp(filename, "cmdtool") == 0);

/*
	width_is_set =  am_cmdtool && 
		((defaults_exists("cmdtool.columns", "window.columns")) ||
		(defaults_exists("cmdtool.width", "window.width")));
	height_is_set = (defaults_exists("cmdtool.rows", "window.rows")) ||
		(defaults_exists("cmdtool.height", "window.height"));

*/	

	/*
	 *  Send the icon attr before argc, argv to give
	 *  commandline argument a chance to override.
	 *  A waste of space & time if commandline argument
	 *  is present.
	 */
	icon_label[0] = 0177;	/* del, highly unlikely as cmd arg */
	icon_label[1] = '\0';


	cmd_pixmap = (Server_image)xv_create(NULL, SERVER_IMAGE,
			     XV_WIDTH,               64,
			     XV_HEIGHT,              64,
			     SERVER_IMAGE_BITS,      am_cmdtool ?
					             cmd_image : tty_image,
			     NULL);

	cmd_mask_pixmap = (Server_image)xv_create(NULL, SERVER_IMAGE,
			     XV_WIDTH,               64,
			     XV_HEIGHT,              64,
			     SERVER_IMAGE_BITS,      am_cmdtool ?
					             cmd_mask_image : tty_mask_image,
			     NULL);

	tool_icon = xv_create((Xv_window)NULL, ICON,
			      ICON_IMAGE,	cmd_pixmap,
			      ICON_MASK_IMAGE,  cmd_mask_pixmap,
			      ICON_LABEL,	icon_label,
			      0);

	base_frame = xv_create((Xv_window)NULL, FRAME,
			FRAME_ICON,		tool_icon,
			0);
			
	if (base_frame == NULL) {
	    fprintf(stderr, "Cannot create base frame.  Process aborted.\n");
	    EXIT(1);
	}		

	/* Get ttysw related args */
	sh_argv[0] = NULL;
	sh_argv[1] = NULL;
	argv++;
	argc--;
	if (am_cmdtool) {
	    checkpoint =
		defaults_get_integer_check("cmdtool.checkpointFrequency",
			"Term.CheckpointFrequency", 0, 0, (int)TEXTSW_INFINITY);
	    edit_log_wraps_at =
		defaults_get_integer_check("cmdtool.maxLogFileSize",
			"Term.MaxLogFileSize",
			(int)TEXTSW_INFINITY, 0, (int)TEXTSW_INFINITY);
	}
	cmdline[0] = NULL;
	while (argc > 0 && **argv == '-') {
		switch (argv[0][1]) {
		case 'C':
			become_console = 1;
			strcat(cmdline, " -C ");
			break;
 		case 'h':	/* amazing stuff -- used to expect -\? for help.. */
 		case '-':	/* jcb 5/10/90 */
		case 'H':
		case '?':
			print_usage(am_cmdtool, tool_name);
			(void)xv_usage(tool_name);
			EXIT(1);
		case 'B':
			if (argc > 1) {
				argv++;
				argc--;
				bold_name = *argv;
			}
			break;
		case 'I':
			if (argc > 1) {
				argv++;
				argc--;
				init_cmd = *argv;
			}

			break;
		case 'P':
			checkpoint = atoi(argv[1]);
			strcat(cmdline, " -P ");
			strcat(cmdline, argv[1]);
			argc--, argv++;
			break;
		case 'M':
			edit_log_wraps_at = atoi(argv[1]);
			strcat(cmdline, " -M ");
			strcat(cmdline,  argv[1]);
			argc--, argv++;
			break;
		case 'L':	/* jcb 5/10/90 runs .login on startup */
			run_login++;
			break;
		default:
			;
		}
		argv++;
		argc--;
	}

	if (strlen(cmdline) > 0)
	    xv_set(base_frame, WIN_CMD_LINE, cmdline, 0);

	if (argc == 0) {
		argv = sh_argv;
		if ((argv[0] = getenv("SHELL")) == NULL)
			argv[0] = "/bin/sh";
	}

	/* if the user wants to run .login format shell, prefix '-' to name */
	if( run_login && argv != NULL ) {	/* jcb 5/10/90 */
		strcpy( cmdline2, "-"  );
		strcat( cmdline2, argv[0] );
		argv[0]	= cmdline2;
	}

	/* If FRAME_LABEL wasn't set by cmdline argument, set it */
	if ((tmp_label1 = (char *)xv_get(base_frame, FRAME_LABEL)) == NULL) {
		(void)strncpy(frame_label,
		  am_cmdtool ? cmd_label : shell_label, sizeof(frame_label));
		if (become_console) {
			(void)strncat(frame_label, console_label,
				sizeof(frame_label));
		} else {
			(void)strncat(frame_label, " - ", sizeof(frame_label));
		}
		(void)strncat(frame_label, *argv, sizeof(frame_label));
		(void)xv_set(base_frame, FRAME_LABEL, frame_label, 0);
	}
	tool_icon = (Icon)xv_get(base_frame, FRAME_ICON);
	if (((tmp_label2 = (char *) xv_get(tool_icon, ICON_LABEL)) == NULL) ||
	    *tmp_label2 == 0177) {
	    if (tmp_label1) {
		(void)strncpy(icon_label, tmp_label1, sizeof(icon_label));
            } else if (become_console) {
		(void)strncpy(icon_label, "CONSOLE", sizeof(icon_label));
	    } else {
		(void)strncpy(icon_label, *argv, sizeof(icon_label));
	    }
	    (void)xv_set(tool_icon, ICON_LABEL, icon_label, 0);
	}
	(void)xv_set(base_frame,
		     FRAME_ICON,		tool_icon,
#ifdef ecd_help
		     HELP_DATA, (become_console ? "xview:console" :
				    (am_cmdtool ? "xview:cmdtool" :
						"xview:shelltool")),
#endif
		     0);

	
	ttysw = xv_create(base_frame, TERMSW,   WIN_IS_CLIENT_PANE,
		  TTY_ARGV,			argv,
		  TTY_QUIT_ON_CHILD_DEATH,	TRUE,
		  TTY_CONSOLE,			become_console,
		  0);

	if (!(defaults_exists("window.width", "Window.Width") ||
	      defaults_exists("window.height", "window.height") ||
	      defaults_exists("window.geometry", "Window.Geometry"))) {
	    int cols, rows;
	    
	    cols = defaults_get_integer_check("window.columns", "Window.Columns", 
					      80, 1, 999);
	    rows = defaults_get_integer_check("window.rows", "Window.Rows",
					      35, 1, 999);
	    
	    xv_set(ttysw,
		   WIN_COLUMNS, cols,
		   WIN_ROWS, rows,
		   0);
	    window_fit(base_frame);
	}

        if (!am_cmdtool)
		xv_set(ttysw, TERMSW_MODE, TTYSW_MODE_TYPE, 0);
	if (bold_name) {
		(void)xv_set(ttysw, TTY_BOLDSTYLE_NAME, bold_name, 0);
	}
	if (am_cmdtool) {
	    (void) xv_set(ttysw,
			  TEXTSW_CHECKPOINT_FREQUENCY, checkpoint,
			  TEXTSW_WRAPAROUND_SIZE, edit_log_wraps_at,
			  0);
	}
	
	tty_pid = (int)xv_get(ttysw, TTY_PID);
#ifdef DEBUG
	(void)fprintf(stderr, "child pid = %d\n", tty_pid);
#endif DEBUG
	if (tty_pid == -1) {
	    strcpy(err_msg, (am_cmdtool) ? "Command" : "Shell");
	    strcpy(err_msg, " Tool: Out of swap space.  Cannot continue.\n");
	    (void) ttysw_output(ttysw, err_msg, strlen(err_msg));        
	} else if (init_cmd && ((len = strlen(init_cmd)) > 0)) {
	    if (init_cmd[len-1] != '\n') {
		init_cmd[len] = '\n';
		len++;
	    }
	    (void)ttysw_input(ttysw, init_cmd, len);
	}

	xv_main_loop(base_frame);
	
	EXIT(0);
}

