/*
 * xv_ok_script.c -- display a script and ask if it's OK to execute
 */

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/notice.h>
#include <sys/errno.h>

char	*filename;
Panel	panel;
Textsw	textsw;
Panel_item message;

doit()
{
	char *msg = "Could not run script under /bin/sh", *reason;
	extern int errno;
	int result;

	/* Mosaic tends to copy files & lose modes, so try chmod */
	(void)chmod(filename, 0755);

	execl(filename, filename, NULL);

	/* if not #!, try running it under sh */
	if (errno == ENOEXEC) {
		execl("/bin/sh", "default-sh", filename, NULL);
		msg =  "Could not run script under /bin/sh";
	}

	/* Exec failed, say why, and abort. */
	reason = strerror(errno);
	
	result = notice_prompt(panel, NULL,
		NOTICE_MESSAGE_STRINGS,
			msg, filename, reason, NULL,
		NOTICE_BUTTON_YES,      "OK",
		NULL);

	exit(1);
}

quit()
{
	exit(0);
}

main(argc,argv)
int     argc;
char    *argv[];
{
	Frame       frame;
	char *msg = "Do you wish to run this script?\n";
	Textsw_status status;

	/*
	 * Initialize XView, having it eat all args so we just get filename
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s script-file-to-OK\n", argv[0]);
		exit(1);
	}

	filename = argv[1];
	frame = (Frame)xv_create(XV_NULL, FRAME,
		XV_LABEL, filename,
		NULL);
	panel = (Panel)xv_create(frame, PANEL, NULL);
	message = (Panel_item)xv_create(panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING, msg,
		NULL);
	textsw = (Textsw)xv_create(panel, TEXTSW, 
		XV_X, 0, XV_Y, 30,
		XV_HEIGHT, 200,
		XV_WIDTH, 400,
		NULL);
	xv_set(textsw, TEXTSW_FILE, filename, NULL);
	xv_set(textsw, TEXTSW_READ_ONLY, NULL);

	xv_create(panel, PANEL_BUTTON,
		XV_X,  20, XV_Y, 240,
		PANEL_LABEL_STRING, "Yes, Run Script",
		PANEL_NOTIFY_PROC,  doit,
		NULL);

	xv_create(panel, PANEL_BUTTON,
		XV_X, 250, XV_Y, 240,
		PANEL_LABEL_STRING, "NO, do not run script",
		PANEL_NOTIFY_PROC,  quit,
		NULL);

	window_fit(panel);
	window_fit(frame);

	xv_main_loop(frame);

	exit(0);
}
