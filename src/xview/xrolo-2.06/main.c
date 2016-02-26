#ifndef lint
static char sccsid[] = "@(#)main.c	2.4 9/13/88";
#endif

/*
 *	Main - Setup the tool environment and dive into Sunview
 */


/*
 * -------------------------------------------------------------------------
 *	ROLO - A Sun Tool to implement a Rolodex-style list of notes
 *
 *	This code manipulates "cards" in a visual manner approximating
 *	a rolodex file.  All the cards are stored in one real file, the
 *	cards are seperated by a ^L (form-feed).  The default path
 *	name is $HOME/.rolo.  A different pathname may be specified at
 *	startup on the command line.  The pathname is relative to the
 *	user's home directory.
 *
 *	Due to bugs in the 3.0 distribution, especially with text subwindows,
 *	this code is only guaranteed to compile and run properly with 3.2
 *	or greater.
 *
 *	This code is public domain, anyone and everyone is welcome to it.
 *	All I ask is that my name and this notice remain on it.  If Sun would
 *	like to bundle it with their product they are welcome to do so,
 *	I only ask that the sources be included in the binary distribution.
 *
 *	Please return any fixes, improvements, gripes, etc to me.
 *
 *	Ron Hitchens		ronbo@vixen.uucp
 *	March 1987 (V1.0)	hitchens@cs.utexas.edu
 *	August 1988 (V2.0)
 * -------------------------------------------------------------------------
 */


#include <sys/param.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/svrimage.h>
#include <xview/icon.h>

#include "defs.h"



/* ------------------------------ Exports ---------------------------------- */

Textsw			rolocard;

char			*rolofile = ROLOFILE;


/* ------------------------------ Imports ---------------------------------- */

extern Panel		init_panel ();

extern Notify_value	rolo_destroy (), catch_resize ();

extern void		init_rolo ();

#ifdef hpux
extern char		*check_args (), *getenv (), *strcpy () ;
extern void		*calloc (), *malloc ();
#else  hpux
extern char		*check_args (), *getenv (), *strcpy (),
			*calloc (), *malloc ();
#endif  hpux

extern char             *exp_fname();

/* ------------------------------ Locals ----------------------------------- */

Frame		frame;

static Textsw		init_card ();

static int		scan_args ();

static char		*strsav ();

Notify_value rolocard_event();

static unsigned short	roloicon_data [] = {
#include "rolo.icon"
};

/* ------------------------------------------------------------------------- */



/*
 *	Main - Clean up inherited fds, make sure we're under SunView,
 *	create the frame and the subwindows, setup interposers then
 *	enter SunView.
 */


#ifdef STANDALONE
main (argc, argv)
#else
rolomain (argc, argv)
#endif
	int	argc;
	char	**argv;
{
	Panel	panel;
#ifdef hpux
#include <unistd.h>
	int	Argc, i, j = sysconf(_SC_OPEN_MAX) ;
#else  hpux
#endif  hpux
	extern void abort();
	char	**Argv, *p;
	Server_image roloicon_image;
	Icon roloicon;
	int text_width;
	char rolonamebuf[MAXNAMLEN];

	for (i = 3; i < j; i++)	{	/* free up all the fd's */
		(void) close (i);
	}

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 
/*			XV_ERROR_PROC, abort, /* */
			0);
	Argc = scan_args (argc, argv, &Argv);

	text_width = defaults_get_integer("xrolo.textwidth",
									  "xrolo.TextWidth",
									  WIN_EXTEND_TO_EDGE); 

	frame = xv_create (XV_NULL, FRAME,
		WIN_ERROR_MSG,			"Couldn't create base window",
		NULL);

    roloicon_image = (Server_image)xv_create(NULL, SERVER_IMAGE,
										 XV_WIDTH, 64,
										 XV_HEIGHT, 64,
										 SERVER_IMAGE_BITS, roloicon_data,
										 NULL);

	roloicon = (Icon) xv_create(NULL, ICON, 
								ICON_IMAGE, roloicon_image,
								NULL);
	xv_set(frame, FRAME_ICON,	roloicon, NULL);

	panel = init_panel (frame);
	window_fit_width(panel);
	window_fit (frame);

	rolocard = (Textsw) xv_create (frame, TEXTSW,
		WIN_X,			0,
		WIN_BELOW,		panel,
		XV_HEIGHT,		200,
		XV_WIDTH,		text_width,
		TEXTSW_CONTENTS,	"Rolo - by Ron Hitchens \n\
              and      \n\
          Luis Soltero  ",
		0);

	notify_interpose_event_func(textsw_first(rolocard),
								(Notify_func)rolocard_event,
								NOTIFY_SAFE);

	p = check_args (argc, argv);
	if (p != NULL) {
		rolofile = p;
	}

	strcpy(rolonamebuf, exp_fname(rolofile));
	rolofile = rolonamebuf;
	init_rolo (rolofile);

	notify_interpose_destroy_func (frame, rolo_destroy);
	notify_interpose_event_func (frame, catch_resize, NOTIFY_SAFE);

	/* clear the X cut buffer */
	textsw_set_selection(rolocard, 0, 0, 1);

	xv_main_loop (frame);

	exit (0);
}

 
Notify_value
rolocard_event(win, event, arg, type)
Xv_window	win;
Event		*event;
Notify_arg	arg;
Notify_event_type type;
{
	if (event_action(event) == ACTION_DRAG_LOAD &&
		!(int)xv_get(win, TEXTSW_READ_ONLY)) {
		char	name[MAXPATHLEN+1];
		
		if (xv_decode_drop(event, name, MAXPATHLEN+1) != -1) {
			xv_set(win, TEXTSW_FILE, name, 0);
			return NOTIFY_DONE;
		}
    }

    return notify_next_event_func(win, (Notify_event) event, arg, type);
}

/*
 *	Scan the command line args and build a dummy arg list containing
 *	the window args which will be overridden by Rolo's fascist sizing
 *	rules.  These will be passed into a xv_set() after the frame and
 *	the subwindows have been created.  These args must be copied because
 *	xv_create() will eat them.
 */

static
int
scan_args (argc, argv, Argv)
	int	argc;
	char	**argv, ***Argv;
{
	int	i, new_argc;
	char	**p, **new_argv;

	new_argv = (char **) calloc (argc, sizeof (char *));
	if (new_argv == (char **)0) {
		fprintf (stderr, "%s: Can't allocate temp arg list\n", *argv);
		exit (1);
	}

	new_argv [0] = strsav (argv [0]);
	new_argc = 1;
	for (i = 1, p = &new_argv [1]; i < argc; i++) {
		if ((strcmp (argv [i], "-Ww") == 0) ||
		    (strcmp (argv [i], "-width") == 0)) {
			if ((argc - i) < 2) {
				/* SunView will complain about missing args */
				continue;
			}
			*p++ = strsav (argv [i++]);
			*p++ = strsav (argv [i]);	/* one sub-arg */
			new_argc += 2;
			continue;
		}
		if ((strcmp (argv [i], "-Wh") == 0) ||
		    (strcmp (argv [i], "-height") == 0)) {
			if ((argc - i) < 2) {
				continue;
			}
			*p++ = strsav (argv [i++]);
			*p++ = strsav (argv [i]);	/* one sub-arg */
			new_argc += 2;
			continue;
		}
		if ((strcmp (argv [i], "-Ws") == 0) ||
		    (strcmp (argv [i], "-size") == 0)) {
			if ((argc - i) < 3) {
				continue;
			}
			*p++ = strsav (argv [i++]);
			*p++ = strsav (argv [i++]);	/* two sub-args */
			*p++ = strsav (argv [i]);
			new_argc += 3;
			continue;
		}
	}

	*Argv = new_argv;

	return (new_argc);
}


/*
 *	Glue for setting the tool name stripe.  Simply to avoid exporting
 *	the frame handle.
 */

void
set_stripe (p)
	char	*p;
{
	xv_set (frame, XV_LABEL, p, 0);
}


/*
 *	Glue for the pop-up message window.  The provided args are passed
 *	on to the popup routine along with the handle to the frame.
 */

/*VARARGS1*/
msg (s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	char	*s;
	long    a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	return (pop_msg (frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
}


/*
 *	Glue for the popup confirmer.
 */

/*VARARGS1*/
confirm (s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	char	*s;
	long    a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	return (pop_confirm (frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
}


/*
 *	Duplicate a string, return the address of the copy.
 */

static
char *
strsav (p)
	char	*p;
{
	char	*q = malloc (strlen (p) + 1);

	return (strcpy (q, p));
}

