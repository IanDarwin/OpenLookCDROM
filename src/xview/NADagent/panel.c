#ifndef lint
static char sccsid[] = "@(#)panel.c	2.3 8/16/88";
#endif

/*
 *	Stuff dealing with the panel
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


#include <stdio.h>
#include <strings.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/defaults.h>
#include <xview/svrimage.h>
#include <sys/param.h>
#include <ctype.h>
#if !defined (sgi)
#include <alloca.h>
#endif
#include <string.h>

#include "defs.h"
#include "help.h"



/* ------------------------------ Exports ---------------------------------- */

void			show_card (), set_slider_max ();

Notify_value		rolo_destroy(), catch_resize();


/* ------------------------------ Imports ---------------------------------- */

extern Textsw		rolocard;

extern struct card	*first, *last, *current;

extern int		need_save;

extern char		*rolofile;

extern struct card	*make_card (), *insert_card (), *undelete_card (),
			*pop_card ();

extern Menu		gen_undelete (), gen_undelete_before();

extern Menu_item	check_stack ();

extern void		delete_card (), dispose_card (),
			push_card (), dump_rolo (),
			nuke_active_cards (), init_rolo (),
			sort_cards (), set_stripe (),
			read_rolo (), write_rolo ();

extern char		*re_comp(), *first_char (), *soundex(); 

extern caddr_t		undel_menu_card ();

extern char             *exp_fname();

/* ------------------------------ Locals ----------------------------------- */

Panel_item	regex_item, slider_item;

static int		panel_height, panel_width;

static int		mask_from_menu_value (), 
			filename_ok ();

int value_from_mask ();

static void		next_button (), next_button_next(),  next_button_S_next(),
  prev_button (), prev_button_prev(), prev_button_S_prev(),
  new_button (), new_card_after(), new_card_before(),
  delete_button (), delete_button_delete(), delete_button_undelete(),
  delete_button_undelete_before(), 
  file_button (), file_button_save(), file_button_reload(), file_button_sort(),
  file_button_sort_backwards(), file_button_io(),
  turn_on_case_insensitive_sort(),
  turn_off_case_insensitive_sort(), 

  done_button (), done_n_save(), done_n_save_exit(), done_n_exit(),
  find_button (), find_button_forward(), find_button_reverse(),
  turn_on_sloppy(), turn_on_egrep(), turn_on_soundex(),

  print_button(), print_entry(), print_all(), set_print_command(), print_regex(),

  list_button_main(), list_goto_button(), list_button (), list_button_match(),
  help_button(), 

  slider_proc (), button_event(), goto_card (),

  no_comprendo ();

extern void popup_send_mail(), dial_a_number(), set_phone_attributes(), 
	send_email_to_selection(), phone_button();

static Menu_item list_menu, find_menu, file_menu, regex_menu, print_menu;
char		*get_selection ();


/* prev new next delete */
static u_short		buttons1_image [] = {
#include "buttons1.icon"
};

static u_short phone_image[] = {
#include "phone.icon"
};

static u_short printer_image[] = {
#include "print.icon"   
};

static u_short find_image[] = {
#include "find.icon"
};

/* drawer (file), "?" (help), flag (finished) and list */
static u_short		buttons2_image [] = {
#include "buttons2.icon"
};

#undef pr_region
Server_image pr_region(i_image, i_w, x, y, w, h)
char *i_image;
{
	int i;
	int wb = w/8;
	int xb = x/8;
    int i_wb = i_w/8;
	char *image          = (char *)malloc(h*wb);
	Server_image retval;

	/* build the image */
	for( i = 0; i < h; i++ ) {
		bcopy(i_image + y*i_wb + xb + i*i_wb, image + i*wb, wb);
	}
	
	retval = (Server_image)xv_create(NULL, SERVER_IMAGE, 
									 XV_WIDTH, 32,
									 XV_HEIGHT, 32,
									 SERVER_IMAGE_BITS, image,
									 NULL
									 );

	return(retval);
}


/* ------------------------------------------------------------------------- */



/*
 *	Actually create the panel subwindow and all the items in it.
 */
static Panel	panel;
static Frame    frame;
Panel init_panel (_frame)
	Frame	_frame;
{
	int	panel_columns;
	Menu tmpmenu;
	char *defaults_set_search_type_str();

	frame = _frame;
	panel = xv_create (frame, PANEL,
					   PANEL_LAYOUT, PANEL_HORIZONTAL,
#ifndef OL
					   WIN_COLUMN_GAP, 20,
					   WIN_ROW_GAP, 30,
#endif
		NULL);

	/* 1st row */
	/* next prev new delete list file help done */
	tmpmenu = menu_create (
						   MENU_ACTION_ITEM, "    Next Card  ", next_button_next,
						   MENU_ACTION_ITEM, "(S) Last Card  ", next_button_S_next, 
						   NULL, NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,	pr_region(buttons1_image, 64,0,32,32,32),
#else
					  PANEL_LABEL_STRING, "Next",
#endif

					  PANEL_ITEM_MENU,	tmpmenu,
					  PANEL_NOTIFY_PROC,	next_button, 
					  XV_X, xv_col(panel, 0),
					  XV_Y, xv_row(panel, 0),
					  NULL);

	tmpmenu = menu_create (
						   MENU_ACTION_ITEM,"    Previous Card", prev_button_prev,
						   MENU_ACTION_ITEM, "(S) First Card   ", prev_button_S_prev,
						   NULL, NULL);
	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,	pr_region(buttons1_image, 64,0,0,32,32),
#else
					  PANEL_LABEL_STRING, "Prev",
#endif
					  PANEL_ITEM_MENU,	tmpmenu,
					  PANEL_NOTIFY_PROC,	prev_button,
					  XV_Y, xv_row(panel, 0),
					  0);

	tmpmenu = menu_create (
						   MENU_ACTION_ITEM, "    New Card After this One ", new_card_after,
						   MENU_ACTION_ITEM, "(S) New Card Before this One", new_card_before,
						   NULL, NULL);
	(void) xv_create (panel, PANEL_BUTTON,

#ifndef OL
					  PANEL_LABEL_IMAGE,	pr_region (buttons1_image, 64,32,0,32,32),
#else
					  PANEL_LABEL_STRING,  "New",
#endif

					  PANEL_ITEM_MENU,	tmpmenu,
					  PANEL_NOTIFY_PROC,	new_button,
					  XV_Y, xv_row(panel, 0),
					  0);

	tmpmenu =  xv_create (NULL, MENU,
				MENU_NOTIFY_PROC, delete_button_delete,
				MENU_ITEM,
					MENU_STRING,
						"    Delete this card        ",
					MENU_VALUE,		1,
					MENU_NOTIFY_PROC, delete_button_delete,
					0,
				MENU_ITEM,
					MENU_STRING,
						"(S) UnDelete a Card (after) ",
					MENU_NOTIFY_PROC, delete_button_undelete,
					MENU_GEN_PROC,		check_stack,
					MENU_GEN_PULLRIGHT,	gen_undelete, 
					MENU_CLIENT_DATA,	FALSE,
					0,
				MENU_ITEM,
						  MENU_STRING,
						  "(C) UnDelete a Card (before)",
						  MENU_NOTIFY_PROC, delete_button_undelete_before,
						  MENU_GEN_PROC,		check_stack,
						  MENU_GEN_PULLRIGHT,	gen_undelete_before, 
						  MENU_CLIENT_DATA,	TRUE,
						  NULL,
			   NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,	pr_region (buttons1_image,64,32,32,32,32),
#else
					  PANEL_LABEL_STRING, "Del",
#endif

					  PANEL_ITEM_MENU, tmpmenu,
					  PANEL_NOTIFY_PROC,	delete_button,
					  XV_Y, xv_row(panel, 0),
					  0);

#define SLOPPY_EXPR_STR	  "Sloppy Regex Match"
#define EGREP_EXPR_STR	  "Egrep(1) Regex Match"
#define SOUNDEX_EXPR_STR  "Soundex Match"
#define EGREP   0
#define SLOPPY  1
#define SOUNDEX 2

	regex_menu = menu_create(
						  MENU_ITEM,
						  MENU_STRING,
						  SLOPPY_EXPR_STR,
						  MENU_NOTIFY_PROC,
						  turn_on_sloppy,
						  NULL,
						  
						  MENU_ITEM,
						  MENU_STRING,
						  EGREP_EXPR_STR,
						  MENU_NOTIFY_PROC,
						  turn_on_egrep,
						  NULL,

						  MENU_ITEM,
						  MENU_STRING,
						  SOUNDEX_EXPR_STR,
						  MENU_NOTIFY_PROC,
						  turn_on_soundex,
						  NULL,

						  NULL);

	list_menu = menu_create (
							 MENU_ACTION_ITEM, "Show Index List of Cards",  list_button,
							
							 MENU_ITEM,
							 MENU_STRING, "(S) Go to Selected Entry",
							 MENU_NOTIFY_PROC, list_goto_button,
							 MENU_INACTIVE, TRUE,
							 NULL,
							
							 MENU_ACTION_ITEM, "(C) List of Cards Matching Regex",
							 list_button_match,

							 MENU_ITEM,
							 MENU_STRING,
							 defaults_set_search_type_str(),
							 MENU_PULLRIGHT, regex_menu,
							 MENU_CLIENT_DATA, -1,
							 NULL,

							 NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,pr_region(buttons2_image,64,32,32,32,32),
#else
					  PANEL_LABEL_STRING, "List",
#endif
					  PANEL_ITEM_MENU,	list_menu,
					  PANEL_NOTIFY_PROC, list_button_main,
					  XV_Y, xv_row(panel, 0),
					  NULL);

#define CASE_INSENSITIVE_SORT_STR "Case Insensitive Sort"
#define CASE_SORT_STR             "Case Sensitive Sort"

	tmpmenu = menu_create(
						  MENU_ITEM,
						  MENU_STRING,
						  CASE_INSENSITIVE_SORT_STR,
						  MENU_NOTIFY_PROC,
						  turn_on_case_insensitive_sort,
						  NULL,
						  
						  MENU_ITEM,
						  MENU_STRING,
						  CASE_SORT_STR,
						  MENU_NOTIFY_PROC,
						  turn_off_case_insensitive_sort,
						  NULL,

						  NULL);

	file_menu = menu_create (
							 MENU_ACTION_ITEM,
							 "      Save Cards to Disk  ",
							 file_button_save,

							 MENU_ACTION_ITEM,
							 "  (S) Reload From Disk    ",
							 file_button_reload,

							 MENU_ACTION_ITEM,
							 "  (C) Sort Cards          ",
							 file_button_sort,

							 MENU_ACTION_ITEM,
							 "(S+C) Sort Backwards      ",
							 file_button_sort_backwards,

							 MENU_CLIENT_DATA, 
							 defaults_get_integer("xrolo.caseinsensitivesort",
												  "xrolo.CaseInsensitiveSort",
												  1), /* sloppy regex by */
							                          /* default */

							 MENU_ITEM,
							 MENU_STRING, 
							 defaults_get_integer("xrolo.caseinsensitivesort",
												  "xrolo.CaseInsensitiveSort",
												  1) 
							 ? CASE_INSENSITIVE_SORT_STR : CASE_SORT_STR,
							 MENU_CLIENT_DATA, 1, 
							 MENU_PULLRIGHT, tmpmenu,
							 NULL,
						   
							 MENU_ACTION_ITEM,
							 "(C+M) File I/O",
							 file_button_io,

							 NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,pr_region(buttons2_image,64,0,0,32,32),
#else
					  PANEL_LABEL_STRING, "File",
#endif
					  PANEL_NOTIFY_PROC,	file_button,
					  PANEL_ITEM_MENU, file_menu,
					  XV_X, xv_col(panel, 0),
					  XV_Y, xv_row(panel, 1),
					  0);

	tmpmenu = menu_create (
						   MENU_ACTION_ITEM, "Display Help Message",
						   help_button, 
						   MENU_ACTION_ITEM, "Send Mail to Author",
						   popup_send_mail, 
						   NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,pr_region(buttons2_image,64,32,0,32,32),
#else
					  PANEL_LABEL_STRING, "Help",
#endif
					  PANEL_ITEM_MENU,	tmpmenu,
					  XV_Y, xv_row(panel, 1),
					  0);
	
	tmpmenu = menu_create (
						   MENU_ACTION_ITEM, 
						   "    Dial Selection", 
						   dial_a_number,
						   
						   MENU_ACTION_ITEM,
						   "(S) Set Phone Attributes",
						   set_phone_attributes,
						   
						   MENU_ACTION_ITEM,
						   "(C) Send E-mail to Selected Addr",
						   send_email_to_selection,

						   NULL);
	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE, xv_create(NULL, SERVER_IMAGE,
												  XV_WIDTH, 32,
												  XV_HEIGHT, 32,
												  SERVER_IMAGE_BITS,
												  phone_image,
												  NULL),
#else
					  PANEL_LABEL_STRING, "Tel",
#endif
					  PANEL_NOTIFY_PROC,	phone_button,
					  PANEL_ITEM_MENU,	tmpmenu,
					  NULL);


	print_menu = menu_create(
							 MENU_ACTION_ITEM, 
							 "Print Entry",
							 print_entry,
							 
							 MENU_ACTION_ITEM, 
							 "(S) Print All",
							 print_all,

							 MENU_ACTION_ITEM,
							 "(C) Print Records Matching Regex",
							 print_regex,

							 MENU_ITEM,
							 MENU_STRING,
							 defaults_set_search_type_str(),
							 MENU_PULLRIGHT, regex_menu,
							 MENU_CLIENT_DATA, -1,
							 NULL,
							 
							 MENU_ACTION_ITEM,
							 "(C+S) Set Print Command",
							 set_print_command,
							 NULL);

	(void) xv_create(panel, PANEL_BUTTON,
#ifndef OL
					 PANEL_LABEL_IMAGE, xv_create(NULL, SERVER_IMAGE,
												  XV_WIDTH, 32,
												  XV_HEIGHT, 32,
												  SERVER_IMAGE_BITS,
												  printer_image,
												  NULL),
#else
					 PANEL_LABEL_STRING, "Print",
#endif
					 PANEL_NOTIFY_PROC, print_button,
					 PANEL_ITEM_MENU, print_menu,
					 XV_Y, xv_row(panel, 1),
					 NULL);

	tmpmenu = menu_create (
						   MENU_ACTION_ITEM, 
						   "   Save Changes and Close  ", 
						   done_n_save,

						   MENU_ACTION_ITEM,
						   "(S) Exit Rolo, Save Changes ",
						   done_n_save_exit,
						   
						   MENU_ACTION_ITEM,
						   "(C) Exit, Don't Save Changes", 
						   done_n_exit,
						   NULL);
	(void) xv_create (panel, PANEL_BUTTON,
#ifndef OL
					  PANEL_LABEL_IMAGE,pr_region(buttons2_image,64,0,32,32,32),
#else
					  PANEL_LABEL_STRING, "Fini",
#endif
					  PANEL_NOTIFY_PROC,	done_button,
					  PANEL_ITEM_MENU,	tmpmenu,
					  XV_Y, xv_row(panel, 1),
					  NULL);


	find_menu = menu_create (
							 MENU_ACTION_ITEM,
							 "    Find Regular Expression, Forward",
							 find_button_forward,
						   
							 MENU_ACTION_ITEM,
							 "(S) Find Regular Expression, Reverse", 
							 find_button_reverse, 

							 MENU_CLIENT_DATA, 
							 defaults_set_search_type(), 
							 
							 MENU_ITEM,
							 MENU_STRING, 
							 defaults_set_search_type_str(),
							 MENU_CLIENT_DATA, -1,
							 MENU_PULLRIGHT, regex_menu,
							 NULL,
						   
						   NULL);

	(void) xv_create (panel, PANEL_BUTTON,
#ifdef  NEVER
					  PANEL_LABEL_IMAGE, xv_create(NULL, SERVER_IMAGE,
												   XV_WIDTH, 32,
												   XV_HEIGHT, 32,
												   SERVER_IMAGE_BITS,
												   find_image,
												   NULL),
#else
					  PANEL_LABEL_STRING, "Find",
#endif
					  PANEL_ITEM_MENU,	find_menu, 
					  PANEL_NOTIFY_PROC,	find_button,
					  XV_Y, xv_row(panel, 2),
					  XV_X, xv_col(panel, 0),
					  NULL);

	/*
	 * Tighten up the window around the buttons in the first row, this
	 * will be the width of the panel window, so we save that size for
	 * later reference.  We also ask for the width of the panel in
	 * columns for computing the width of the find pattern text item.
	 */
	window_fit_width (panel); 
	panel_width = (int) xv_get (panel, XV_WIDTH);
	panel_columns = (int) xv_get (panel, WIN_COLUMNS);
	regex_item = xv_create (panel, PANEL_TEXT,
							PANEL_BLINK_CARET,		TRUE,
							PANEL_LABEL_STRING,		"Expression:",
							PANEL_VALUE_DISPLAY_LENGTH,	10,
							XV_Y, xv_row(panel, 2),
							PANEL_VALUE_STORED_LENGTH,	80,
							PANEL_NOTIFY_PROC,	find_button,
							0);



	tmpmenu = menu_create (
				MENU_ITEM,
					MENU_STRING,	"Pick a card, any card",
					MENU_VALUE,	0,
					0,
				0);

	slider_item = xv_create (panel, PANEL_SLIDER,
							 PANEL_MIN_VALUE,	0,
							 PANEL_MAX_VALUE,	1,
							 PANEL_VALUE,		1,
							 PANEL_SLIDER_WIDTH, panel_width - 80, 
							 PANEL_SHOW_RANGE,	FALSE,
							 PANEL_SHOW_VALUE,	TRUE,
							 PANEL_NOTIFY_LEVEL,	PANEL_DONE,
							 PANEL_NOTIFY_PROC,	slider_proc,
							 PANEL_ITEM_MENU, tmpmenu,
							 XV_Y, xv_row(panel, 3),
							 XV_X, xv_col(panel, 0),
							 0);

	/*
	 * Adjust the position of the text item slightly for better aesthetic
	 * placement relative to the label in the Find button.  Do it after
	 * creating the slider so that the slider is placed relative to the
	 * original position of the text item, not the final position.
	 */
	xv_set (regex_item,
		XV_Y,	xv_get (regex_item, XV_Y) + 4,
		0);
	/*
	 * Tighten up the panel window in both directions around the final
	 * layout.  Save off the resulting height for later use.
	 */
	window_fit_height (panel); 
	panel_height = (int) xv_get (panel, XV_HEIGHT);

	return (panel);
}

/* ------------------------------------------------------------------------- */


/*		Panel item notification handlers		*/

/*
 *	Notification proc for the "next" button.  If the button is pressed
 *	with no shift, the card is advanced to the next one.  If pressed
 *	with the shift key, then jump to the last card.
 */

do_bozo(panel,p)
Panel panel;
struct card *p;
{
	static int	bozo = 0;	/* speak second time end is reached */
	if (p == NULL_CARD) {		/* did we step off end of the list? */ 
		if (panel == NULL || bozo != 0) {	/* have we already beeped once? */
			msg ("This is the last card");
		} else {
			window_bell (panel);
		}
		bozo = (bozo+1)&1;
		return(1);
	}
	return(0);
}

static int in_next_panel;
static void next_button_next(item, event)
Panel_item item;
Event *event;
{
	struct card	*p;
	if ( in_next_panel ) {
		in_next_panel = 0;
		return;
	}
	save_card (current);
	p = current->c_next;
	if ( !do_bozo(panel,p) )
	  show_card (p);		/* all is well, display the new card */
}

static void next_button_S_next(item, event)
Panel_item item;
Event *event;
{
	if ( in_next_panel ) {
		in_next_panel = 0;
		return;
	}
	save_card (current);
	show_card (last);		/* all is well, display the new card */
}

/*ARGSUSED*/
static
void
next_button (item, event)
	Panel_item	item;
	Event		*event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;
	switch (value_from_mask (event)) {
	  case SHIFT_CLICK:		/* click plus shift (menu item 2) */
		next_button_S_next(item, event);
		break;

	  default:			/* any other shift mask (or none) */
		next_button_next(item, event);
		break;
	}
	in_next_panel = 1; 
}


/*
 *	Notification proc for the "Previous" button.  Similar to the proc
 *	above for next card.
 */
static in_prev_panel;

static void prev_button_prev(item,event)
Panel_item item;
Event *event;
{
	struct card	*p;
	if ( in_prev_panel ) {
		in_prev_panel = 0;
		return;
	}
	save_card (current);
	p = current->c_prev;	/* move backwards one card */
	if ( !do_bozo(panel,p) )
	  show_card(p);
}

static void prev_button_S_prev(item,event)
Panel_item item;
Event *event;
{
	if ( in_prev_panel ) {
		in_prev_panel = 0;
		return;
	}
	save_card (current);
	show_card (first);
}

/*ARGSUSED*/
static void prev_button (item, event)
	Panel_item	item;
	Event		*event;
{

	if ( event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event)) {
	  case SHIFT_CLICK:
		prev_button_S_prev(item, event);
		break;

	default:
		prev_button_prev(item, event);
		break;
	}
	in_prev_panel = 1;
}


/*
 *	Notification for the "New Card" button.  The new card is inserted
 *	after the currently displayed card, unless the shift key is down,
 *	in which case it is inserted before the current card.
 */

static in_new_button;

static void new_card_after (item, event)
Panel_item	item;
Event		*event;
{
	struct	card	*c, *p;
	if ( in_new_button ) {
		in_new_button = 0;
		return;
	}
	save_card (current);
	c = make_card (NULL);
	if (c == NULL_CARD) {
		msg ("Can't allocate space for a new card");
		return;
	}
	p = current;			/* insert after current */
	p = insert_card (c, p);			/* insert c after p */
	need_save = TRUE;			/* Things have changed */
	set_slider_max (renumber (first));	/* update slider range */
	show_card (p);	
}

static void new_card_before(item, event)
	Panel_item	item;
	Event		*event;
{
	struct	card	*c, *p;	
	if ( in_new_button ) {
		in_new_button = 0;
		return;
	}
	save_card (current);
	c = make_card (NULL);
	if (c == NULL_CARD) {
		msg ("Can't allocate space for a new card");
		return;
	}
	p = current->c_prev;		/* c_prev may be NULL */
	p = insert_card (c, p);			/* insert c after p */
	need_save = TRUE;			/* Things have changed */
	set_slider_max (renumber (first));	/* update slider range */
	show_card (p);	
}

/*ARGSUSED*/
static void new_button (item, event)
	Panel_item	item;
	Event		*event;
{

	if ( event_action(event) == ACTION_MENU )
	  return;
	switch (value_from_mask (event)) {
	  case SHIFT_CLICK:			/* click+shift, insert before */
		new_card_before(item, event);
		break;

	  default:
		new_card_after(item, event);
		break;
	}
	in_new_button = 1;
}


/*
 *	Notification proc for the "Delete" button.  A plain click deletes
 *	the current card and pushes it on the deleted stack.  Shift and
 *	Control key modifiers undelete the card on the top of the stack.
 *	The undelete operations via the menu are special for the delete
 *	button.  Picking either undelete operation from the menu does not
 *	wind up as a call to this function.  Those operations are entirely
 *	handled by the menu code via action and gen procs.  See the menu
 *	procs in cards.c.  
 */

static int in_delete_button;
static void delete_button_delete (item, event)
Panel_item	item;
Event		*event;
{
	struct card	*p;
	if ( in_delete_button ) {
		in_delete_button = 0;
		return;
	}
	if (first == last) {
		/* if we're deleting the last card, add a dummy */
		(void) insert_card (make_card (NULL), NULL_CARD);
	}
	
	/* and the new current card will be... */
	save_card(current);
	p = (current == last) ? current->c_prev : current->c_next;
	delete_card (current);
	need_save = TRUE;
	set_slider_max( renumber(first));
	if ( !do_bozo(panel,p) )
	  show_card(p);
}

static void delete_button_undelete_before (item, event)
Panel_item	item;
Event		*event;
{
	struct card *p;
	if ( in_delete_button ) {
		in_delete_button = 0;
		return;
	}
	save_card (current);
	p = undelete_card (current->c_prev);	/* can be nil */
	need_save = TRUE;
	set_slider_max (renumber (first));
	if ( !do_bozo(panel,p) )
	  show_card(p);
}

static void delete_button_undelete (item, event)
Panel_item	item;
Event		*event;
{
	struct card *p;
	if ( in_delete_button ) {
		in_delete_button = 0;
		return;
	}
	save_card (current);
	p = undelete_card (current);
	need_save = TRUE;
	set_slider_max (renumber (first));
	if ( !do_bozo(panel,p) )
	  show_card(p);
}

/*ARGSUSED*/
static void delete_button (item, event)
Panel_item	item;
Event		*event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;


	switch (value_from_mask (event)) {
	  case PLAIN_CLICK:
		delete_button_delete(item, event);
		break;

	  case SHIFT_CLICK:
		delete_button_undelete(item, event);
		break;

	  case CTRL_CLICK:
		delete_button_undelete_before(item, event);
		break;
		
	  default:
		no_comprendo (event, "delete_button");
		return;
	}
	in_delete_button = 1;
}

/*
 *	Notification proc for the "File" button.  
 */

static int in_file_button;

static void file_button_save (item, event)
Panel_item	item;
Event		*event;
{
	if ( in_file_button ) {
		in_file_button = 0;
		return;
	}
	save_card (current);
	dump_rolo (first, rolofile);
}

static void file_button_reload (item, event)
Panel_item	item;
Event		*event;
{
	if ( in_file_button ) {
		in_file_button = 0;
		return;
	}
	save_card (current);
	if ((need_save == TRUE) && (verify_no_save () == FALSE)) {
		return;
	}
	nuke_active_cards ();
	init_rolo (rolofile);
}

find_field_no()
{
	int i = xv_get(rolocard, TEXTSW_INSERTION_POINT);
	int len = xv_get(rolocard, TEXTSW_LENGTH);
#if !defined(sgi)
	char *c = (char *)alloca(len+1);
#else
	char *c = (char *)malloc(len+1);
#endif
	char *tp = c+i;
	int field_cnt = 0;
	xv_get(rolocard, TEXTSW_CONTENTS, 0, c, len);
	if ( *tp == '\n' ) tp--;
	do {
		if ( *tp == '\n' ) 
		  field_cnt++;
	} while ( tp-- != c );
#if defined(sgi)
	free(c);
#endif
	return(field_cnt);
}

static void file_button_sort (item, event)
Panel_item	item;
Event		*event;
{
	if ( in_file_button ) {
		in_file_button = 0;
		return;
	}
	save_card (current);
	sort_cards (FALSE, file_menu, find_field_no());
	need_save = TRUE;
}

static void file_button_sort_backwards (item, event)
Panel_item	item;
Event		*event;
{
	if ( in_file_button ) {
		in_file_button = 0;
		return;
	}
	save_card (current);
	sort_cards (TRUE, file_menu, find_field_no());
	need_save = TRUE;
}

static Frame file_frame;
static Panel file_panel;
static Panel_item file_item;
static void load_from_file_proc()
{
	do_read_rolo(exp_fname(xv_get(file_item, PANEL_VALUE)));
}

static void save_to_file_proc()
{
	do_write_rolo(exp_fname(xv_get(file_item, PANEL_VALUE)));
}

static void file_cancel_proc()
{
	xv_set(file_frame, XV_SHOW, FALSE, NULL);
}

static void do_create_file_frame(item, event)
Panel_item item;
Event      *event;
{
	file_frame = xv_create(frame, FRAME,
						   FRAME_LABEL, "File IO",
						   FRAME_SHOW_LABEL, TRUE,
						   NULL);
/*
	file_panel = xv_get(file_frame, FRAME_CMD_PANEL);
*/
	file_panel = xv_create(file_frame, PANEL, NULL);

	file_item = xv_create(file_panel, PANEL_TEXT,
							 XV_X, 4,
							 XV_Y, 6,
							 PANEL_LABEL_STRING, "File Name:", 
							 PANEL_VALUE_STORED_LENGTH, 256,
							 PANEL_VALUE_DISPLAY_LENGTH, 30,
							 NULL);

	xv_create(file_panel, PANEL_BUTTON,
			  PANEL_LABEL_STRING, "Load From File",
			  XV_X, 4,
			  XV_Y, 35,
			  PANEL_NOTIFY_PROC, load_from_file_proc,
			  NULL);
	xv_create(file_panel, PANEL_BUTTON,
			  PANEL_LABEL_STRING, "Save To File",
			  XV_X, 120,
			  XV_Y, 35,
			  PANEL_NOTIFY_PROC, save_to_file_proc,
			  NULL);
	xv_create(file_panel, PANEL_BUTTON,
			  PANEL_LABEL_STRING, "Cancel",
			  XV_X, 220,
			  XV_Y, 35,
			  PANEL_NOTIFY_PROC, file_cancel_proc,
			  NULL);
	window_fit(file_panel);
	window_fit(file_frame);
}

static void do_file_io_panel()
{
	static int firsttime = 1;
	char *cp = get_selection();
	if (firsttime) {
		do_create_file_frame();
		firsttime = 0;
	}
	if ( cp != NULL ) {
		xv_set(file_item, PANEL_VALUE, cp, NULL);
	}
	xv_set(file_frame, XV_SHOW, TRUE, NULL);
}

static void file_button_io (item, event)
Panel_item	item;
Event		*event;
{
	if ( in_file_button ) {
		in_file_button = 0;
		return;
	}
	save_card (current);
	do_file_io_panel();
}

do_read_rolo(filename)
char *filename;
{
	if (filename_ok (filename) == FALSE) {
		return;
	}
	if ((need_save == TRUE) && (verify_no_save () == FALSE)) {
		return;
	}
	read_rolo (filename);
}

do_write_rolo(filename)
char *filename;
{
	if (filename_ok (filename) == FALSE) {
		return;
	}
	write_rolo (filename);
}
static void file_button (item, event)
Panel_item	item;
Event		*event;
{
	char		*filename;

	if ( event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event)) {
	  case PLAIN_CLICK:			/* Plain save */
		file_button_save(item, event);
		break;

	  case SHIFT_CLICK:			/* reload, no save first */
		file_button_reload(item, event);
		break;

	  case CTRL_CLICK:			/* sort ascending */
		file_button_sort(item, event);
		break;

	  case CTRL_SHIFT_CLICK:			/* sort descending */
		file_button_sort_backwards(item, event);
		break;

	  case META_CTRL_CLICK:			/* load named file */
		file_button_io(item, event);
		break;

	  default:				/* say what? */
		no_comprendo (event, "file_button");
		return;
	}
	in_file_button = 1;
}

static void turn_on_case_insensitive_sort(item, event)
Panel_item	item;
Event		*event;
{
	Menu_item file_item = xv_find(file_menu, MENUITEM, 
								  MENU_CLIENT_DATA, 1, NULL);
	if ( file_item == NULL ) {
		confirm("turn_on_case_insensitive_sort: Bad news!");
		return;
	}
	xv_set(file_item, MENU_STRING, CASE_INSENSITIVE_SORT_STR, NULL);
	xv_set(file_menu, MENU_CLIENT_DATA, 1, 0);
}

static void turn_off_case_insensitive_sort(item, event)
Panel_item	item;
Event		*event;
{
	Menu_item file_item = xv_find(file_menu, MENUITEM, 
								  MENU_CLIENT_DATA, 1, NULL);
	if ( file_item == NULL ) {
		confirm("turn_off_case_insensitive_sort: Bad news!");
		return;
	}
	xv_set(file_item, MENU_STRING, CASE_SORT_STR, NULL);
	xv_set(file_menu, MENU_CLIENT_DATA, 0);
}


/*
 *	Notification proc for the "Done" button.  This covers both closing
 *	the tool and exiting.  (The checkered flag button is supposed to
 *	convey the idea of "finished".  Yeah, I know, you got a better idea?)
 */

static int in_done_button;
static void done_n_save (item, event)
Panel_item	item;
Event		*event;
{
	if (in_done_button) {
		in_done_button = 0;
		return;
	}
	save_card (current);
	if (need_save) {
		dump_rolo (first, rolofile);
	}
	xv_set (frame, FRAME_CLOSED, TRUE, 0);
}

static void done_n_save_exit (item, event)
Panel_item	item;
Event		*event;
{
	if (in_done_button) {
		in_done_button = 0;
		return;
	}
	save_card (current);
	if (need_save) {
		dump_rolo (first, rolofile);
	}
	xv_destroy_safe (frame);
}

static void done_n_exit (item, event)
Panel_item	item;
Event		*event;
{
	if (in_done_button) {
		in_done_button = 0;
		return;
	}
	save_card (current);
	if (need_save && (verify_no_save () == FALSE)) {
		return;
	}
	need_save = FALSE;
	xv_destroy_safe (frame);
}

static void done_button (item, event)
	Panel_item	item;
	Event		*event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event)) {
	  case SHIFT_CLICK:			/* save cards and exit */
		done_n_save_exit(item,event);
		break;

	case CTRL_CLICK:			/* don't save and exit */
		done_n_exit(item,event);
		break;

	default:				/* save and go iconic */
		done_n_save(item,event);
		return;
	}

	in_done_button = 1;
	xv_destroy_safe (frame);
}


/*
 * Notification proc for print.
 */
static char Print_Command[256] = "lpr";
static int First_Print_Call = 1;
static int in_print_button = 0;

get_print_resource()
{
 	char *p = defaults_get_string("xrolo.printcommand",
 		"xrolo.PrintCommand", Print_Command);
 	if (p != NULL)
 		strcpy(Print_Command, p);
	First_Print_Call = 0;
}

static void print_button(item, event)
Panel_item item;
Event      *event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event)) {
	  case PLAIN_CLICK:			/* Plain save */
		print_entry(item, event);
		break;

	  case SHIFT_CLICK:			/* reload, no save first */
		print_all(item, event);
		break;

	  case CTRL_CLICK:			/* sort ascending */
		print_regex(item, event);
		break;

	  case CTRL_SHIFT_CLICK:			/* sort descending */
		set_print_command(item, event);
		break;

	  default:				/* say what? */
		no_comprendo (event, "file_button");
		return;
	}
	in_print_button = 1;
}

static void print_entry(item, event)
Panel_item item;
Event      *event;
{
	FILE *fp;
	if ( in_print_button ) {
		in_print_button = 0;
		return;
	}
	if (First_Print_Call)
	  get_print_resource();
	fp = popen(Print_Command, "w");
	if ( fp == NULL ) {
		confirm("Could not execute print command %s", Print_Command);
		return;
	}
	save_card(current);
	fprintf(fp, "%s\n", current->c_text);
	pclose(fp);
}

static void print_regex(item, event)
Panel_item item;
Event      *event;
{
	struct card *p;
	int search_type = xv_get(find_menu, MENU_CLIENT_DATA);
	char *match();
	FILE *fp;
	if ( in_print_button ) {
		in_print_button = 0;
		return;
	}
	if ( !init_find_button(search_type) )
	  return;
	if (First_Print_Call)
	  get_print_resource();
	fp = popen(Print_Command, "w");
	if ( fp == NULL ) {
		confirm("Could not execute print command %s", Print_Command);
		return;
	}
	save_card(current);
	for (p = first; p != NULL_CARD; p=p->c_next) {
		if ( match(p->c_text, search_type) )
		  fprintf(fp, "Record # %d \n%s\n\n", p->c_num, p->c_text);
	}
	pclose(fp);
}

static void print_all(item, event)
Panel_item item;
Event      *event;
{
	struct card *p;
	FILE *fp;
	if ( in_print_button ) {
		in_print_button = 0;
		return;
	}
	if (First_Print_Call)
	  get_print_resource();
	fp = popen(Print_Command, "w");
	if ( fp == NULL ) {
		confirm("Could not execute print command %s", Print_Command);
		return;
	}
	save_card(current);
	for (p = first; p != NULL_CARD; p=p->c_next)
	  fprintf(fp, "Record # %d \n%s\n\n", p->c_num, p->c_text);
	pclose(fp);
}

static Frame print_frame;
static Panel print_panel;
static Panel_item Command_item;

static void ok_proc()
{
	strcpy(Print_Command, (char *)xv_get(Command_item,PANEL_VALUE));
	xv_set(print_frame, WIN_SHOW, FALSE, NULL);
}

static void cancel_proc()
{
	xv_set(print_frame, WIN_SHOW, FALSE, NULL);
}

static void set_print_command(item, event)
Panel_item item;
Event      *event;
{
	static int firsttime = 1;
	if ( in_print_button ) {
		in_print_button = 0;
		return;
	}
	if ( firsttime ) {
		firsttime = 0;
		if (First_Print_Call)
		  get_print_resource();
		print_frame = xv_create(frame, FRAME,
								XV_LABEL, "Set Print Command",
								FRAME_SHOW_LABEL, TRUE,
								NULL);
		print_panel = xv_create(print_frame, PANEL, NULL);
		Command_item = xv_create(print_panel, PANEL_TEXT,
								 XV_X, 4,
								 XV_Y, 4,
								 PANEL_LABEL_STRING, "Print Command:", 
								 PANEL_VALUE, Print_Command,
								 PANEL_VALUE_STORED_LENGTH, 256,
								 PANEL_VALUE_DISPLAY_LENGTH, 40,
								 NULL);
		xv_create(print_panel, PANEL_BUTTON,
				  PANEL_LABEL_STRING, "   OK   ",
				  XV_X, 450,
				  XV_Y, 4,
				  PANEL_NOTIFY_PROC, ok_proc,
				  NULL);
		xv_create(print_panel, PANEL_BUTTON,
				  PANEL_LABEL_STRING, "Cancel",
				  XV_X, 450,
				  XV_Y, 35,
				  PANEL_NOTIFY_PROC, cancel_proc,
				  NULL);
		window_fit(print_panel);
		window_fit(print_frame);
	}
	xv_set(Command_item, PANEL_VALUE, Print_Command, 0);
	xv_set(print_frame, WIN_SHOW, TRUE, 0);
}

/*
 *	Notification proc for both the "Find" button and text item for
 *	the search pattern.  This proc will be called from the pattern item
 *	if you type return.  If this proc is called, and there is an active
 *	selection, that will be used as the search pattern.  The selection
 *	will also be inserted into the pattern item if it is currently empty.
 */
static void to_sloppy(str)
char *str;
{
	char tmpstr[MAX_SELN_LEN];
	char *ptr1 = str;
	char *ptr2 = tmpstr;
	while ( *ptr1 != '\0' ) {
		if ( isalpha(*ptr1) ) {
			*ptr2++ = '[';
			*ptr2++ = isupper(*ptr1) ? 
			  tolower(*ptr1) : toupper(*ptr1);
			*ptr2++ = *ptr1++;
			*ptr2++ = ']';
		} else {
			*ptr2++ = *ptr1++;
		}
	}
	*ptr2 ='\0';
	strcpy(str, tmpstr);
}

static  int in_find_button;
static	char	*e, regbuf [MAX_SELN_LEN];

init_find_button(search_type)
int search_type;
{
	static		int bozo = 0;

	save_card (current);

	/* if selection active, use it */
	if ((e = get_selection ()) != NULL) {
		char	*pe = (char *) xv_get(regex_item, PANEL_VALUE);

		(void) strcpy (regbuf, e);
		xv_set (regex_item, PANEL_VALUE, regbuf, 0);
	} else {
		/* else use panel value */
		(void) strcpy (regbuf, (char *) xv_get(regex_item, PANEL_VALUE));
	}

	/* clear the X cut buffer */
	textsw_set_selection(rolocard, 0, 0, 1);

	if (strlen (regbuf) == 0) {
		if (bozo) {
			msg ("Enter an expression to search for");
		} else {
			window_bell (panel);
			bozo++;
		}
		return(0);
	}

	bozo = 0;

	if ( search_type == SOUNDEX ) {
		strcpy(regbuf, soundex(regbuf));
		return(1);
	}
	if ( search_type == SLOPPY ) {
		to_sloppy(regbuf);
	}
	e = re_comp (regbuf);
	if (e != NULL) {
		msg ("Regular Expression error: %s", e);
		return(0);
	}
	return(1);
}

char *match(str, search_type)
char *str;
int search_type;
{
	char *cp, *regexstrstr();
	char *buff;

	if ( search_type != SOUNDEX ) {
		return ( re_exec(str) ? regexstrstr(str) : NULL);
	}

	
	/* soundex match */
#if !defined(sgi)
	buff = (char *)alloca(strlen(str)+1);
#else
	buff = (char *)malloc(strlen(str)+1);	
#endif
	strcpy(buff, str);
	cp = strtok(buff," \t\n\r");
	while( cp != NULL ) {
		if ( strncmp(soundex(cp), regbuf, 4) == 0 ) {
#if defined(sgi)
			free(buff);
#endif
			return(&str[(int)(cp - buff)]);
		}
		cp = strtok(NULL, " \t\n\r");
	}
#if defined(sgi)
	free(buff);
#endif
	return(NULL);
}

static void find_button_forward (item, event)
Panel_item	item;
Event		*event;
{
	struct card *p;
	char *cp;
	int search_type = xv_get(find_menu, MENU_CLIENT_DATA);
	if ( in_find_button ) {
		in_find_button = 0;
		return;
	}
	if ( !init_find_button(search_type) )
	  return;
	p = (current == last) ? first : current->c_next;
	while (p != current) {
		if ( cp=match(p->c_text, search_type)) {
			show_positioned_card (p, (int)(cp - p->c_text));
			return;
		}
		p = (p == last) ? first : p->c_next;
	}
	if (re_exec (p->c_text) != 1)		/* wrapped back to current */
	  window_bell (panel);
}

static void find_button_reverse (item, event)
Panel_item	item;
Event		*event;
{
	struct card *p;
	char *cp;
	int search_type = xv_get(find_menu, MENU_CLIENT_DATA);
	if ( in_find_button ) {
		in_find_button = 0;
		return;
	}
	if ( !init_find_button(search_type) )
	  return;
	p = (current == first) ? last : current->c_prev;
	while (p != current) {
		if (cp=match(p->c_text, search_type)) {
			show_positioned_card (p, (int)(cp - p->c_text));
			return;
		}
		p = (p == first) ? last : p->c_prev;
	}
	if (re_exec (p->c_text) != 1)		/* wrapped back to current */
		window_bell (panel);
}

/*ARGSUSED*/
static void find_button (item, event)
Panel_item	item;
Event		*event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;

	if (value_from_mask(event) == SHIFT_CLICK) {		/* search backwards */
		find_button_reverse(item,event);
		in_find_button = 1;
	}
}

defaults_set_search_type()
{
	int cc=defaults_get_integer("xrolo.searchtype", 
							   "xrolo.SearchType",
							   SLOPPY);
	switch(cc) {
	  case EGREP:
	  case SLOPPY:
	  case SOUNDEX:
		return(cc);
	}
	return(SLOPPY);
}

char *defaults_set_search_type_str()
{
	int cc = defaults_set_search_type();
	switch(cc) {
	  case EGREP:
		return(EGREP_EXPR_STR);
	  case SLOPPY:
		return(SLOPPY_EXPR_STR);
	  case SOUNDEX:
		return(SOUNDEX_EXPR_STR);
	}
}

static void turn_on_sloppy(item, event)
Panel_item	item;
Event		*event;
{
	Menu_item find_item = xv_find(find_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item list_item = xv_find(list_menu, MENUITEM,
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item print_item = xv_find(print_menu, MENUITEM,
								  MENU_CLIENT_DATA, -1, NULL);

	if ( find_item == NULL || list_item == NULL ) {
		confirm("turn_on_sloppy: Bad news! xrolo internal error");
		return;
	}
	xv_set(find_item, MENU_STRING, SLOPPY_EXPR_STR, NULL);
	xv_set(list_item, MENU_STRING, SLOPPY_EXPR_STR, NULL);
	xv_set(print_item, MENU_STRING, SLOPPY_EXPR_STR, NULL);
	xv_set(find_menu, MENU_CLIENT_DATA, SLOPPY, NULL);

}

static void turn_on_egrep(item, event)
Panel_item	item;
Event		*event;
{
	Menu_item find_item = xv_find(find_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item list_item = xv_find(list_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item print_item = xv_find(print_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	if ( find_item == NULL || list_item == NULL) {
		confirm("turn_on_egrep: Bad news! xrolo internal error");
		return;
	}
	xv_set(find_item, MENU_STRING, EGREP_EXPR_STR, NULL);
	xv_set(list_item, MENU_STRING, EGREP_EXPR_STR, NULL);
	xv_set(print_item, MENU_STRING, EGREP_EXPR_STR, NULL);
	xv_set(find_menu, MENU_CLIENT_DATA, EGREP, NULL);
}

static void turn_on_soundex(item, event)
Panel_item	item;
Event		*event;
{
	Menu_item find_item = xv_find(find_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item list_item = xv_find(list_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	Menu_item print_item = xv_find(print_menu, MENUITEM, 
								  MENU_CLIENT_DATA, -1, NULL);
	if ( find_item == NULL || list_item == NULL ) {
		confirm("turn_on_soundex: Bad news! xrolo internal error");
		return;
	}
	xv_set(find_item, MENU_STRING, SOUNDEX_EXPR_STR, NULL);
	xv_set(list_item, MENU_STRING, SOUNDEX_EXPR_STR, NULL);
	xv_set(print_item, MENU_STRING, SOUNDEX_EXPR_STR, NULL);
	xv_set(find_menu, MENU_CLIENT_DATA, SOUNDEX, NULL);
}

/*
 *	Notification proc for the "List" button.  The text window is used
 *	to display the first non-blank line of each card.
 */

static void (*def_notify_proc)() = NULL; 
static int in_list_button;

static void list_button_main(item, event)
Panel_item item;
Event *event;
{
	Menu_item  list_item = xv_find(list_menu, MENUITEM, 
							 MENU_NOTIFY_PROC, list_goto_button, NULL);
	if (event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event) ) {
	  case SHIFT_CLICK: 
		if (xv_get(list_item, MENU_INACTIVE) == FALSE) {
			list_goto_button(item, event);
		}
		break;
	  case CTRL_CLICK:
		list_button_match(item,event);
		break;
	  default:
		list_button(item, event);
		break;
	}
	in_list_button = 1;
}

char *catbuf(buf, str)
char *buf, *str;
{
	if (buf == NULL) {
		buf = malloc(strlen(str)+1);
		*buf = '\0';
	} else {
		buf = realloc(buf, strlen(buf)+strlen(str)+2);
	}
	strcat(buf, str);
	return(buf);
}

static void list_button_egrep();

static void list_button(item, event)
	Panel_item	item;
	Event		*event;
{
	list_button_egrep(item, event, -1);
}

static void list_button_match(item, event)
	Panel_item	item;
	Event		*event;
{
	int search_type = xv_get(find_menu, MENU_CLIENT_DATA);
	if ( !init_find_button(search_type) )
	  return;
	list_button_egrep(item, event, search_type);
}

/*ARGSUSED*/
static void list_button_egrep (item, event, search_type)
	Panel_item	item;
	Event		*event;
    int         search_type;
{
	struct card 	*p;
	void notify_proc();
	Menu_item list_item;

#ifndef DONT_USE_MALLOC_LIST
	char *index_buf = NULL;
	char *index_current;
	int len;
#endif

	if (in_list_button || def_notify_proc != NULL) {
		in_list_button = 0;
		return;
	}
	if (def_notify_proc == NULL )
	  def_notify_proc = (void (*)())xv_get(rolocard, TEXTSW_NOTIFY_PROC);
	
	list_item = xv_find(list_menu, MENUITEM, MENU_NOTIFY_PROC,
						list_goto_button, NULL);	
	xv_set(list_item, MENU_INACTIVE, FALSE, NULL);

	save_card (current);			/* save off pending changes */

	textsw_reset (rolocard, 0, 0);		/* clear the text window */

#ifndef DONT_USE_MALLOC_LIST

	/*  first calculate length of string to insert, then actually */
	/*  malloc the string and put the text into it.  even though this */
	/*  means two list traversals, for really large lists it will */
	/*  probably save a lot of time.  */

	len = 0;
	for (p = first; p != NULL_CARD; p = p->c_next) {
		char	*nl;
		char	line_buf [MAX_INDEX_LINE + 2];

		if ( search_type == -1 || match(p->c_text, search_type) ) {
			(void) sprintf (line_buf, "%3d: ", p->c_num);	/* prepend # */
			(void) strncat (line_buf, first_char (p->c_text),
							MAX_INDEX_LINE);
			line_buf [MAX_INDEX_LINE] = 0;	/* make sure it's terminated */
			
			(void) strcat (line_buf, "\n");	/* make sure of newline */
			nl = index (line_buf, '\n');
			*++nl = '\0';			/* chop at first line break */
			len += strlen (line_buf);
		}
	}
	index_buf = malloc (len);

	index_current = index_buf;	/* strcat slower than strcpy */
	for (p = first; p != NULL_CARD; p = p->c_next) {
		char	*nl;
		char	line_buf [MAX_INDEX_LINE + 2];

		if ( search_type == -1 || match(p->c_text, search_type) ) {
			(void) sprintf (line_buf, "%3d: ", p->c_num);	/* prepend # */
			(void) strncat (line_buf, first_char (p->c_text),
							MAX_INDEX_LINE);
			line_buf [MAX_INDEX_LINE] = 0;	/* make sure it's terminated */

			(void) strcat (line_buf, "\n");	/* make sure of newline */
			nl = index (line_buf, '\n');
			*++nl = '\0';			/* chop after first line break */
			strcpy (index_current, line_buf);
			index_current = index (index_current, '\n') + 1;
		}
	}

	window_set (rolocard, TEXTSW_MEMORY_MAXIMUM, len + 20000, 0);

	(void) textsw_insert (rolocard, index_buf, len);
	free (index_buf);
#else
	for (p = first; p != NULL_CARD; p = p->c_next) {
		char	*nl;
		char	line_buf [MAX_INDEX_LINE + 2];

		if ( search_type == -1 || match(p->c_text, search_type) ) {
			(void) sprintf (line_buf, "%3d: ", p->c_num);/* prepend number */
			textsw_insert (rolocard, line_buf, strlen (line_buf));

			(void) strncpy (line_buf, first_char (p->c_text),
							MAX_INDEX_LINE);
			line_buf [MAX_INDEX_LINE] = 0;	/* make sure it's terminated */
			
			(void) strcat (line_buf, "\n");	/* make sure of newline */
			nl = index (line_buf, '\n');
			*++nl = '\0';			/* chop at first line break */
			textsw_insert (rolocard, line_buf, strlen (line_buf));
		}
	}
#endif

	xv_set (rolocard, TEXTSW_INSERTION_POINT, 0, 0);	/* rewind */
	textsw_normalize_view (rolocard, 0);

	current = NULL_CARD;			/* indicate no card displayed */
	update_num_display (LISTALLCARDS);

	xv_set(rolocard, TEXTSW_NOTIFY_PROC, notify_proc, NULL);
	
}

static void list_goto_button(item, event)
Panel_item item;
Event      *event;
{
	int i = xv_get(rolocard, TEXTSW_INSERTION_POINT);
	int len = xv_get(rolocard, TEXTSW_LENGTH);
	int cardindex;
#if !defined(sgi)
	char *c = (char *)alloca(len+1);
#else
	char *c = (char *)malloc(len+1);
#endif
	
	xv_get(rolocard, TEXTSW_CONTENTS, 0, c, len);
	if (c[i] == '\n') i--;
	while ( c[i] != '\n'  && i != 0 )
	  i--;
	if (c[i] == '\n' ) i++;
	sscanf(c+i,"%d", &cardindex);
	turn_off_goto_button();
	goto_card(cardindex);
#if defined(sgi)
	free(c);
#endif
}

void notify_proc(textsw, attributes)
Textsw textsw;
Attr_avlist attributes;
{
	Attr_avlist attrs;
	void (*def_not_proc)() = def_notify_proc;
	for ( attrs = attributes; *attrs; attrs = attr_next(attrs) ) {
		if ( *attrs == TEXTSW_ACTION_USING_MEMORY ) {
			turn_off_goto_button();
		}
	}
	def_not_proc(rolocard, attributes);
}

turn_off_goto_button()
{
	Menu_item item = xv_find(list_menu, MENUITEM, 
							 MENU_NOTIFY_PROC, list_goto_button, NULL);
	if ( def_notify_proc != NULL ) {
		xv_set(rolocard, TEXTSW_NOTIFY_PROC, def_notify_proc, NULL);
		def_notify_proc = NULL;
	}
	xv_set(item, MENU_INACTIVE, TRUE, NULL);
}


/*
 *	Notification proc for the "Help" button.  The text window is used to
 *	display a help message.  The text of the help message is defined
 *	in help.h as an array of string pointers (there is too much text to
 *	be parsed as one long string token).
 */

/*ARGSUSED*/
static
void
help_button (item, event)
	Panel_item	item;
	Event		*event;
{
	int		i;

	save_card (current);			/* capture any pending mods */

	textsw_reset (rolocard, 0, 0);		/* empty the window */

	/* insert the help strings into the window in order */
	for (i = 0; i < sizeof (help_msg) / sizeof (char *); i++) {
  		textsw_insert (rolocard, help_msg [i], strlen (help_msg [i]));
	}

  	xv_set (rolocard, TEXTSW_INSERTION_POINT, 0, 0);	/* rewind */
  	textsw_normalize_view (rolocard, 0);

  	current = NULL_CARD;			/* indicate no card displayed */
  	update_num_display (HELPDISPLAYED);	/* set title bar */

}


/*
 *	Notification proc for the slider item on the panel.  This one's easy.
 */

/*ARGSUSED*/
static
void
slider_proc (item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	save_card (current);
	goto_card (value);
}

/* ----------------------------------------------------------------------- */

/*		Utility procs called by the event handlers above	*/


/*
 *	Set the range on the slider.  Lower bound is always 1, upper bound
 *	is set to the argument i.  The slider size is adjusted according to
 *	how many digits it takes to represent the value.  This looks a bit
 *	goofy because of the way the ATTR_COLS() macro works.  These macros
 *	do NOT return an integer which represents a number of pixels, they
 *	encode a value in the high bits which is interpreted later inside
 *	the library code.  This means you cannot say:
 *		x = panel_width - ATTR_COLS(n);
 *	See the macro definitions in <sunwindow/attr.h>
 */

void
set_slider_max (i)
	int	i;
{
	int	delta = 6;		/* space for 3 digits */

	if (i < 100) {
		delta = 5;		/* space for 2 digits */
	}

	if (i < 10) {
		delta = 4;		/* space for 1 digit */
	}

	xv_set (slider_item,
		PANEL_MIN_VALUE,	1,
		PANEL_MAX_VALUE,	i,
		0);
}


/*
 *	Display the card pointed to by p.  Reset the text window, which
 *	clears it.  Insert the text of the card into the window, then
 *	roll it back to the beginning.  After displaying the text of
 *	the card, update the display items on the panel and set the global
 *	current pointer to point at this card.
 */

void
show_card (p)
	struct	card	*p;
{
	show_positioned_card(p, 0);
}

show_positioned_card (p, pos)
struct	card	*p;
char *pos;
{
	textsw_reset (rolocard, 0, 0);
	textsw_insert (rolocard, p->c_text, strlen (p->c_text));
	xv_set (rolocard, TEXTSW_INSERTION_POINT, pos, 0);
	textsw_normalize_view (rolocard, pos);
	update_num_display (p->c_num);
	current = p;
}

/*
 *	Change to the card with the given index number.  The number is
 *	range checked, then searched for.  When found, the pointer to the
 *	card struct is passed to the function which actually changes
 *	to the new card.
 */

static
void
goto_card (i)
	int		i;
{
	struct	card	*p, *q;

	if ((i < 1) || (i > last->c_num)) {
		msg ("Sorry, don't have a card #%d", i);
		show_card (first);	/* show something we're sure of */
		return;			/* unlikely to happen */
	}

	/* reduce the search space a bit */
	if ((current != NULL_CARD) && (i >= current->c_num)) {
		q = current;
	} else {
		q = first;
	}

	for (p = q; p != NULL_CARD; p = p->c_next) {
		if (p->c_num == i) {
			show_card (p);
			return;
		}
	}

	msg ("Unexpected inconsistency, couldn't find card #%d", i);
}


/*
 *	Save off the contents of the card being displayed in the text window.
 *	If the data in the window has been modified by the user, we need to
 *	replace the data in the card struct with the contents of the window.
 *	Since there isn't a way to load some data into the window as an
 *	initial value and mark it as "clean", we need to use a brute force
 *	method to determine if any changes have been made.  We copy out
 *	the contents of the window and compare it to the data we inserted
 *	in the first place.  If they are the same, no change was made and
 *	there is nothing to do.  If they are different, we throw away the
 *	old contents and stash the pointer to the new in the card struct.
 */

save_card (p)
	struct	card 	*p;
{
	int	red, len;
	char	*c;

	if (p == NULL_CARD) {
		/*
		 * If nil, the text window is being used for an index list
		 * or help message.  If so, we know there is no card data
		 * to be saved, and that some button has been clicked.  So
		 * at this point we'll redisplay the last card that was
		 * displayed.  The slider item should still remember which
		 * one it was.
		 */
		goto_card ((int) xv_get (slider_item, PANEL_VALUE));
		return;
	}

	len = (int) xv_get (rolocard, TEXTSW_LENGTH);
	c = malloc (len + 1);
	red = (int) xv_get (rolocard, TEXTSW_CONTENTS, 0, c, len);
	if (red != len) {
		fprintf (stderr, "rolo: fetch error: red=%d, len=%d\n",
				red, len);
		return;
	}

	c [len] = '\0';
	if (strcmp (c, p->c_text) == 0) {
		free (c);			/* didn't change */
	} else {
		free (p->c_text);		/* changed and must save */
		p->c_text = c;
		need_save = TRUE;
	}
}


/*
 *	Update the displayed information which indicates which card is
 *	currently being displayed.  Provisions are made for the special
 *	cases where the help message or index list is being displayed.
 *	The tool name stripe and the slider value are set to indicate
 *	the number of the current card.
 */

update_num_display (i)
	int	i;
{
	char	buf [MAXPATHLEN + 20];		/* worst case */

	switch (i) {
	case LISTALLCARDS:
		(void) sprintf (buf, "%s - %s  (First lines of all %d cards)",
				NAME, rolofile, last->c_num);
		break;

	case HELPDISPLAYED:
		(void) sprintf (buf, "%s - %s  (Help Message)",
				NAME, rolofile);
		break;

	default:
		xv_set (slider_item, PANEL_VALUE, i, 0);
		(void) sprintf (buf, "%s - %s  (Card %d of %d)",
				NAME, rolofile, i, last->c_num);
		break;
	}

	set_stripe (buf);
}


/*
 *	Ask the user if they're really sure they don't want to save the
 *	changes they've made.  This function only really exists because
 *	the same question is asked in two places (I really hate duplicating
 *	code).
 */

verify_no_save ()
{
	return (confirm (
 		"You have made changes which will be lost.  Are you sure?"));
}


/*
 *	Complain about an unknown shiftmask in a click event.
 */

static
void
no_comprendo (event, p)
	Event	*event;
	char	*p;
{
	char	buf [100];

	(void) sprintf (buf, " %s: Huh?  Don't understand click value %d",
		p, value_from_mask (event));
	msg ("%s", buf);
}


/*
 *	Make sure a filename is reasonable.  This is important because the
 *	filename is provided via the selection mechanism, which could contain
 *	all manner of gibberish.
 */

static
int
filename_ok (p)
	char	*p;
{
	char	*q, *bad_chars = "!^&*()|`{}[]:;\\\"'<>?";

        if (p[0] == '\0') return(FALSE);
	for (q = p; *q != '\0'; q++) {
		if (( ! isgraph (*q)) || (index (bad_chars, *q) != NULL)) {
			msg ("Sorry, that looks like a bad filename to me");
			return (FALSE);
		}
	}

	return (TRUE);
}


/*
 *	Get the primary selection, copy it into a static buffer, up to a
 *	maximum, and return a pointer to it.  Return NULL if there is no
 *	current primary selection to get.
 */

char *
get_selection()
{
	static Seln_holder	holder;
	Seln_request	*buffer;
	static char 	sel_text [MAX_SELN_LEN + 1];
	Xv_server server = (Xv_server)xv_get(xv_get(frame, XV_SCREEN), SCREEN_SERVER);
	
	holder = selection_inquire (server, SELN_PRIMARY);
	buffer = selection_ask (server, &holder, SELN_REQ_CONTENTS_ASCII, NULL, NULL);

	(void) strncpy (sel_text, buffer->data + sizeof (SELN_REQ_CONTENTS_ASCII),MAX_SELN_LEN);

	if (strlen (sel_text) == 0 ) {
		/* empty string is no sel. */
		return (NULL);
	}

	return (sel_text);
}

/* ----------------------------------------------------------------------- */

/*		Panel event dispatcher, trap menu button events		*/


/*
 *	Convert a value representing a menu item into the corresponding
 *	event shift mask for faking a button click.  The menu value, which
 *	is 1-relative, is decremented by one, then the lowest three bits
 *	are examined and the corresponding shiftmask bits are set.
 */

/*
 *	Given an event, translate it into an integer number representing
 *	which logical choice it is.  The value returned will be in the
 *	range 0-7, made up of the three possible bits representing the
 *	states of the SHIFT, CONTROL and META shift keys.  The effect is
 *	that the number returned by this function will be one less than
 *	the value initially passed into mask_from_menu_value().
 */

int
value_from_mask (event)
	Event	*event;
{
	int	value = 0;

	/*
	 * These macros don't return TRUE or FALSE, exactly.  They return
	 * the corresponding bits from the shiftmask.  For example, the
	 * value of event_ctrl_is_down() is 0x30 if the control key was
	 * down.  That means you can't compare the result to TRUE.
	 */

	if (event_shift_is_down (event) != 0) {
		value |= 1;
	}

	if (event_ctrl_is_down (event) != 0) {
		value |= 2;
	}

	if (event_meta_is_down (event) != 0) {
		value |= 4;
	}

	return (value);
}

/* ----------------------------------------------------------------------- */

/*		Interposer functions watching for frame events		*/


/*
 *	Interposer proc for catching window resize events.  We're a little
 *	bit fascist here and insist that the frame remain at least big
 *	enough to display the whole panel and at least three lines of the
 *	text window.
 */

#define MIN_CARD_ROWS	3

Notify_value
catch_resize (frame, event, arg, type)
	Frame			frame;
	Event			*event;
	Notify_arg		arg;
	Notify_event_type	type;
{
	Panel			panel;
	int			width;
	int			height;
	int			card_height;
	int			frame_height;
	Notify_value		value;

	value = notify_next_event_func (frame, event, arg, type);

	if (event_id (event) != WIN_RESIZE) {
		return (value);
	}

	if ((int) xv_get (frame, FRAME_CLOSED) == TRUE) {
		return (value);
	}

	panel = (Panel) xv_get (frame, FRAME_NTH_SUBWINDOW, 1);


	width = (int) xv_get (panel, XV_WIDTH);

	if (width < panel_width) {
		xv_set (panel, XV_WIDTH, panel_width, 0);
		window_fit_width (frame);
	}


	height = (int) xv_get (panel, XV_HEIGHT);

	if (height < panel_height) {
		xv_set (panel, XV_HEIGHT, panel_height, 0);
		window_fit_height (frame);
	}

	card_height = (int) xv_get (rolocard, WIN_ROWS);
	if (card_height < MIN_CARD_ROWS) {
		xv_set (rolocard, WIN_ROWS, MIN_CARD_ROWS, 0);
		window_fit_height (frame);
	}

	/*
	 * This catches cases where the subwindows are completely clipped
	 * and don't shrink as far as the frame is concerned.
	 */
	card_height = (int) xv_get (rolocard, XV_HEIGHT);
	frame_height = (int) xv_get (frame, XV_HEIGHT);
	if (frame_height < (panel_height + card_height)) {
		window_fit_height (frame);
	}

	return (value);
}


/*
 *	Interposer function to catch destroy events.  We want to know when
 *	the tool about to be destroyed so that we can save any changes to the
 *	cards back out to disk.  This gets a little tricky because the
 *	text edit window will veto a tool destroy (selecting Quit from
 *	the tool menu) if there is any text in the window.  This is because
 *	from its point of view the text buffer has been modified because we
 *	inserted the initial contents of the card.  The way we get around
 *	this is by saving the contents of the window, resetting it, calling
 *	the rest of the notification chain, then restoring the contents of
 *	the window.  The destroy proc for the text window is called in that
 *	notification chain and it will see an empty window and not object to
 *	the tool being destroyed.  If this function is called again with
 *	a notification flag other than DESTROY_CHECKING, that means the tool
 *	is really going away (either the user OKed a quit or suntools is
 *	shutting down).  In that case we write the cards back out if they
 *	have been changed, without any fancy footwork.
 */

Notify_value
rolo_destroy (frame, status)
	Frame	frame;
	Destroy_status status;
{
	if (status == DESTROY_CHECKING) {
		Notify_value	s;

		save_card (current);
		textsw_reset (rolocard, 0, 0);		/* fake out textedit */
		s = notify_next_destroy_func (frame, status);
		show_card (current);
		return (s);
	}

	save_card (current);
	if (need_save) {
		dump_rolo (first, rolofile);
	}

	return (notify_next_destroy_func (frame, status));
}

#ifndef NULL
#include <stdio.h> 
#endif

char *regexstrstr(s1)
char *s1;
{
	while ( *s1 != '\0' && re_exec(s1) ) {
		s1++;
	}
	if ( *s1 != '\0' )
	  return(--s1);
	return( NULL );
}

