#ifndef lint
static char sccsid[] = "@(#)popup.c	2.2 8/12/88";
#endif

/*
 *	popup - pop up error dialog windows
 */


#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>
#include <stdio.h>


#define X_GAP		25
#define Y_GAP		30

/*
#define MAX(a,b)	(((a)>(b))?(a):(b))
#define MIN(a,b)	(((a)<(b))?(a):(b))
*/


/* ---------------------------- Imports -------------------------------- */

extern char		*sprintf ();


/* ---------------------------- Locals --------------------------------- */


/*
	This is used only with exits
*/
int
save_or_not_save(win,s)
Frame   win;
char    *s;
{
	return ( notice_prompt(
			(Frame)win,
			(Event *)		NULL,
			NOTICE_NO_BEEPING,	TRUE,
			NOTICE_MESSAGE_STRINGS,	s,0,
			NOTICE_BUTTON_YES,	"discard changes",
			NOTICE_BUTTON_NO,	"save changes",
			0) );
}

/*
	Show a popup window give a message and
	get an answer from the user.
*/
static
do_pop(win, s , show_cancel)
	Frame	win;
	char	*s;
	int	show_cancel;
{
	int	result;
		if(show_cancel)
			result = notice_prompt(
					(Frame)win,
					(Event *)		NULL,
					NOTICE_NO_BEEPING,	TRUE,
					NOTICE_MESSAGE_STRINGS,	s,0,
					NOTICE_BUTTON_YES,	"ok",
					NOTICE_BUTTON_NO,	"cancel",
					0);
		else
			result = notice_prompt(
					(Frame)win,
					(Event *)		NULL,
					NOTICE_NO_BEEPING,	FALSE,
					NOTICE_MESSAGE_STRINGS,	s,0,
					NOTICE_BUTTON_YES,	"ok",
					0);
	return(result);
}

/*
 *	Glue routine for a confirmer dialog box.  The first argument is
 *	the frame relative to which the dialog window is displayed.  The
 *	remaining arguments are a format string and generic args for
 *	the sprintf call.  The flag is passed to the dialog frame creation
 *	routine which indicates that both the OK and CANCEL buttons are
 *	to be displayed.
 */

/*VARARGS2*/
pop_confirm (frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	Frame	frame;
	char	*s;
	long	a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	char	temp [512];

	(void) sprintf (temp, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	return (do_pop (frame, temp, TRUE));
}


/*
 *	Glue routine for a simple info popup frame.  Only the OK button is
 *	displayed and the frame waits until it is clicked.
 */

/*VARARGS2*/
pop_msg (frame, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	Frame	frame;
	char	*s;
	long	a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	char	temp [512];

	(void) sprintf (temp, s, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	return (do_pop (frame, temp, FALSE));
}

