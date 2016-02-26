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


#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/svrimage.h>
#include <xview/icon.h>

#include "defs.h"
static  u_short rolo_icon[] = 
{
#include "rolo.icon"
};
mpr_static( rolo_icon_pr, ICON_DEFAULT_WIDTH,
			ICON_DEFAULT_HEIGHT, 1, rolo_icon);



/* ------------------------------ Exports ---------------------------------- */

Textsw			rolocard;
Textsw			init_card();

char			*rolofile = ROLOFILE;
Frame			rolo_frame;


/* ------------------------------ Imports ---------------------------------- */

extern Panel		init_panel ();

extern Notify_value	rolo_destroy (), catch_resize ();

extern void		init_rolo ();

extern char		*check_args (), *getenv ();

extern Server_image	*make_glyphs();
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
	Icon	icon;
	int	i, j = getdtablesize ();
	char	*p;

	for (i = 3; i < j; i++)		/* free up all the fd's */
		(void) close (i);


	xv_init( XV_INIT_ARGC_PTR_ARGV,	&argc , argv , 0);
	icon =(Icon)xv_create(NULL, ICON, ICON_IMAGE, &rolo_icon_pr, 0);
	rolo_frame = (Frame)xv_create ( NULL, FRAME ,
			WIN_ERROR_MSG,		"Couldn't create window",
			FRAME_ICON,		 icon,
			FRAME_INHERIT_COLORS,	TRUE,
			XV_X,0,
			XV_Y,0,
			0 );

	p = check_args (argc, argv);
	if (p != NULL) {
		rolofile = p;
	}

	panel = init_panel (rolo_frame);
	rolocard = init_card (rolo_frame, panel);
	(void) chdir (getenv ("HOME"));
	init_rolo (rolofile);
	window_fit (rolo_frame);
	init_selections(panel);

	(void) notify_interpose_destroy_func (rolo_frame,
					rolo_destroy, NOTIFY_SAFE);
	notify_interpose_event_func (rolo_frame, catch_resize,
					NOTIFY_SAFE);

	window_main_loop (rolo_frame);

	exit (0);
}


/*
 *	Actually create the text subwindow where the the card text is displayed
 */

static
Textsw
init_card (fraim, panel)
	Frame	fraim;
	Panel	panel;
{
	Textsw	textsw;

	textsw = (Textsw) xv_create (fraim, TEXTSW,
		XV_HELP_DATA,		"rolo:rolocard",
		WIN_X,			0,
		WIN_BELOW,		panel,
		XV_HEIGHT,		xv_row( fraim, 12),
		TEXTSW_CONTENTS,"Rolo - by Ron Hitchens\nModified by Sun Microsystems for Xview",
		0);

	return (textsw);
}


/*
 *	Glue for setting the tool name stripe.  Simply to avoid exporting
 *	the frame handle.
 */

set_stripe (p)
	char	*p;
{
	xv_set (rolo_frame, XV_LABEL, p, 0);
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
	return (pop_msg (rolo_frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
}


/*
 *	Glue for the popup confirmer.
 */

/*VARARGS1*/
confirm (s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	char	*s;
	long    a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	return (pop_confirm (rolo_frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
}


