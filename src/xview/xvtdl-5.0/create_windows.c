/*
 *  Create_windows.c ==> Routines to setup the windows in the XVTDL
 *                       environment.
 *
 *  This includes calls to create cursors.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1993 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 *
 * $Log: create_windows.c,v $
 * Revision 4.2  1992/10/06  11:27:14  jipping
 * Bug fixes after beta test:
 *   + help files renamed
 *   + max values fixed for recurrence window set
 *   + arrows rearranged
 *
 * Revision 4.1  1992/09/15  13:26:09  jipping
 * Removed references to cursors (no longer needed).
 *
 * Revision 4.0  1992/09/15  11:19:50  jipping
 * Release 4.0 beta.  Changes include:
 *    * reworking the recurring interface
 *    * inclusion of the GUIDE generated deadline editor.
 *    * small rearranging of other items (for pleasing look)
 *
 * Revision 3.4  1992/07/31  18:46:46  jipping
 * Fixed XView 2.0 anomalies by simply changing the parent
 * of the recurring_frame.
 *
 * Revision 3.3  1992/07/31  18:09:58  jipping
 * Changed window dimensions for recurring window --> to use window_fit
 * instead of hardcoded width and height.
 *
 * Revision 3.2  1992/07/30  19:47:24  jipping
 * (1) Fixed a bug in the strftime call.
 * (2) Added interactive help references to window widgets.
 *
 * Revision 3.1  1992/07/28  12:05:51  jipping
 * Adjusted title on "List All" button.
 *
 * Revision 3.0  1992/07/27  18:34:40  jipping
 * Release 3.0 includes:
 * * rearranged items on the control panel
 * * added "List All..." and "Done" buttons
 * * added distinguishing macros for Xview versions 2 and 3
 * * added initializations for the properties and log editors
 *
 * Revision 2.3  1992/07/13  18:58:35  jipping
 * Altered window fitting calls to accomodate OW version 2.
 *
 * Revision 2.2  1992/07/13  15:53:32  jipping
 * Changed several panels' size to WIN_EXTEND_TO_EDGE to accomodate
 * correct resizing.
 *
 * Revision 2.1  1992/07/13  13:53:52  jipping
 * Placed all bitmaps for checks and buttons in a subdirectory.
 *
 * Revision 2.0  1992/07/10  17:17:53  jipping
 * Initial Release.
 *
 * Revision 2.0  1992/07/10  16:21:58  jipping
 * Initial Release
 *
 */

#include "globaldefs.h"

/*
 *  Window system declarations...callback routines and XView variables
 */

void tdl_events(), list_events();
void repaint_proc(), calendar_event_proc();
void close_list(), close_create();
void go_forward(), go_today(), go_backward(), quit();
void tdl_notify_proc();
void create_it(), edit(), start_print();
void create_recurring(), cancel_recurring(), choose_recurring(), close_recurring();
void choose_new_category(), change_entry_type(), choose_wom();
void pg(), copy(), cut(), paste();
void open_properties(), list_all_categories(), close_tdlist();
void edit_category(), delete_category(), move_priority();
void load_file(), save_file(), merge_file(), save_cat_file();
void create_item(), open_category_editor();
void display_category(), display_tree(), display_parent(), display_all();

extern Menu cat_menu();

Icon  tdl_icon, empty_tdl_icon;
Panel tdl_title, tdl_control, tdl_list;
Panel_item date_message, backward, goto_today, forward, categories, recurring;
Panel_item category_name, properties, entry_deadline;
Panel_item todo, create_entry, edit_entry, print_tdl, close_tdl, quit_tdl;
Panel_item list_all, file_actions;
Menu edit_menu, file_menu, create_menu, list_menu;

Frame entry_frame;
Panel entry_panel;
Panel_item entry_text, entry_done, entry_type, entry_category, entry_category_name;

Frame recurring_frame;
Panel recurring_panel;
Panel_item freq, day_of_week, day_of_month, weeks, week_of_month, months;
Panel_item recurring_done, recurring_cancel;

entry_entry_frame_objects	*entry_editor;

/*
 *  Image declarations -- bitmaps for glyphs and icons.
 */

static short notepad_bits[] = {
#include "bitmaps/tdl.pr"
};
Server_image notepad;

static short notepad_mask_bits[] = {
#include "bitmaps/tdl_mask.pr"
};
Server_image notepad_mask;

static short notepad_empty_bits[] = {
#include "bitmaps/tdl_empty.pr"
};
Server_image notepad_empty;

static short backward_bits[] = {
#include "bitmaps/backward.pr"
};
Server_image backward_image;

static short forward_bits[] = {
#include "bitmaps/forward.pr"
};
Server_image forward_image;

static short checked_off_bits[] = {
#include "bitmaps/checked_off.pr"
};
Server_image checked_off;

static short checked_on_bits[] = {
#include "bitmaps/checked_on.pr"
};
Server_image checked_on;

Server_image checks[10];
static short zero_bits[] = {
#include "bitmaps/0.pr"
};
static short one_bits[] = {
#include "bitmaps/1.pr"
};
static short two_bits[] = {
#include "bitmaps/2.pr"
};
static short three_bits[] = {
#include "bitmaps/3.pr"
};
static short four_bits[] = {
#include "bitmaps/4.pr"
};
static short five_bits[] = {
#include "bitmaps/5.pr"
};
static short six_bits[] = {
#include "bitmaps/6.pr"
};
static short seven_bits[] = {
#include "bitmaps/7.pr"
};
static short eight_bits[] = {
#include "bitmaps/8.pr"
};
static short nine_bits[] = {
#include "bitmaps/9.pr"
};

/*
 * **********************************************************************
 * This creates all the window stuff.  Note that it calls initialize
 * routines from GUIDE-developed code.
 */
create_windows()
{
	char day_title[50];

	/*
	 *  First off...glyphs...
    */
	checked_off = xv_create(NULL, SERVER_IMAGE,
							  XV_WIDTH,           16,
							  XV_HEIGHT,          16,
							  SERVER_IMAGE_DEPTH, 1,
							  SERVER_IMAGE_BITS,  checked_off_bits,
							  NULL);
	
	checked_on = xv_create(NULL, SERVER_IMAGE,
							  XV_WIDTH,           16,
							  XV_HEIGHT,          16,
							  SERVER_IMAGE_DEPTH, 1,
							  SERVER_IMAGE_BITS,  checked_on_bits,
							  NULL);

	checks[0] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  zero_bits,
								 NULL);
	checks[1] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  one_bits,
								 NULL);
	checks[2] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  two_bits,
								 NULL);
	checks[3] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  three_bits,
								 NULL);
	checks[4] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  four_bits,
								 NULL);
	checks[5] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  five_bits,
								 NULL);
	checks[6] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  six_bits,
								 NULL);
	checks[7] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  seven_bits,
								 NULL);
	checks[8] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  eight_bits,
								 NULL);
	checks[9] = xv_create(NULL, SERVER_IMAGE,
								 XV_WIDTH,           16,
								 XV_HEIGHT,          16,
								 SERVER_IMAGE_DEPTH, 1,
								 SERVER_IMAGE_BITS,  nine_bits,
								 NULL);

	/*** Startup the colors ***/
	gcm_initialize_colors(tdlist, fgcolor, bgcolor);

	/*
    *  Now, create the main todo list window. 
    *
    *  Start with the "title panel".
    */

	tdl_title = xv_create(tdlist, PANEL,
								 OPENWIN_SHOW_BORDERS,  TRUE,
								 XV_WIDTH,  WIN_EXTEND_TO_EDGE,
								 0);

	notepad = xv_create(NULL, SERVER_IMAGE,
							  XV_WIDTH,           64,
							  XV_HEIGHT,          64,
							  SERVER_IMAGE_DEPTH, 1,
							  SERVER_IMAGE_BITS,  notepad_bits,
							  NULL);

	notepad_mask = xv_create(NULL, SERVER_IMAGE,
									 XV_WIDTH,           64,
									 XV_HEIGHT,          64,
									 SERVER_IMAGE_DEPTH, 1,
									 SERVER_IMAGE_BITS,  notepad_mask_bits,
									 NULL);

	notepad_empty = xv_create(NULL, SERVER_IMAGE,
									  XV_WIDTH,           64,
									  XV_HEIGHT,          64,
									  SERVER_IMAGE_DEPTH, 1,
									  SERVER_IMAGE_BITS,  notepad_empty_bits,
									  NULL);

	tdl_icon = xv_create(XV_NULL, ICON,
								ICON_IMAGE,  notepad,
								ICON_MASK_IMAGE, notepad_mask,
								ICON_LABEL, "xvtdl",
								ICON_TRANSPARENT, TRUE,
								0);

	empty_tdl_icon = xv_create(XV_NULL, ICON,
										ICON_IMAGE,  notepad_empty,
										ICON_MASK_IMAGE, notepad_mask,
										ICON_TRANSPARENT, TRUE,
										0);

	xv_set(tdlist, FRAME_ICON, tdl_icon, 0);

	(void) xv_create(tdl_title, PANEL_MESSAGE,
						  PANEL_LABEL_IMAGE,  notepad,
						  XV_X, 280,
						  XV_Y, 10,
						  0);

	strftime(day_title, 50, "%A, %B %e, %Y", &today);
	date_message = xv_create(tdl_title, PANEL_MESSAGE,
									 PANEL_LABEL_STRING, day_title,
									 PANEL_LABEL_BOLD,   TRUE,
									 XV_X,  200,
									 XV_Y,  85,
									 0);

	window_fit_height(tdl_title);

   /*
    *  Now the control panel 
    */

	tdl_control = xv_create(tdlist, PANEL,
									WIN_BELOW, tdl_title,
									XV_WIDTH,  WIN_EXTEND_TO_EDGE,
									XV_X, 0,
									0);

	backward_image = xv_create(NULL, SERVER_IMAGE,
										XV_WIDTH,           32,
										XV_HEIGHT,          22,
										SERVER_IMAGE_DEPTH, 1,
										SERVER_IMAGE_BITS,  backward_bits,
										NULL);

	categories = xv_create(tdl_control, PANEL_ABBREV_MENU_BUTTON,
								  PANEL_LABEL_STRING, "Category:",
								  PANEL_ITEM_MENU,    cat_menu(category_head),
								  XV_HELP_DATA,       "xvtdl_main:categories",
								  XV_X,  120,
								  XV_Y,  7,
								  0);
	
	category_name = xv_create(tdl_control, PANEL_MESSAGE,
									  PANEL_LABEL_STRING, category_head->name,
									  XV_X, 220,
									  XV_Y,  7,
									  0);
									  
	
	forward_image = xv_create(NULL, SERVER_IMAGE,
									  XV_WIDTH,           32,
									  XV_HEIGHT,          22,
									  SERVER_IMAGE_DEPTH, 1,
									  SERVER_IMAGE_BITS,  forward_bits,
									  NULL);

	create_menu = xv_create(XV_NULL, MENU_COMMAND_MENU,
									MENU_ITEM,
								      MENU_STRING, "New Item",
								      MENU_GEN_PROC, create_item,
								      NULL,
									MENU_ITEM,
								      MENU_STRING, "New Category",
								      MENU_GEN_PROC, open_category_editor,
								      NULL,
									NULL);	

	create_entry = xv_create(tdl_control, PANEL_BUTTON,
									 PANEL_LABEL_STRING, "Create",
									 XV_HELP_DATA,       "xvtdl_main:create",
									 XV_X, 0,
									 XV_Y, 40,
									 PANEL_ITEM_MENU, create_menu,
									 0);

	edit_menu = xv_create(XV_NULL, MENU_COMMAND_MENU,
								 MENU_ITEM,
								    MENU_STRING, "Modify",
								    MENU_GEN_PROC, edit,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Copy",
								    MENU_GEN_PROC, copy,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Cut",
								    MENU_GEN_PROC, cut,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Paste",
								    MENU_GEN_PROC, paste,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "",
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "inc priority",
								    MENU_GEN_PROC, move_priority,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "dec priority",
								    MENU_GEN_PROC, move_priority,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "",
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Modify Category",
								    MENU_GEN_PROC, edit_category,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Delete Category",
								    MENU_GEN_PROC, delete_category,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "",
								    NULL,
#if 0
								 MENU_ITEM,
								    MENU_STRING, "Annotation...",
								    MENU_NOTIFY_PROC, open_properties,
								    NULL,
#endif
								 MENU_ITEM,
								    MENU_STRING, "Properties...",
								    MENU_NOTIFY_PROC, open_properties,
								    NULL,
								 MENU_GEN_PIN_WINDOW, tdlist, "Edit",
								 NULL);	

	edit_entry = xv_create(tdl_control, PANEL_BUTTON,
								  PANEL_LABEL_STRING, "Edit",
								  XV_HELP_DATA,       "xvtdl_main:edit",
								  XV_X, 90,
								  XV_Y, 40,
								  PANEL_ITEM_MENU, edit_menu,
								  0);

	list_menu = xv_create(XV_NULL, MENU_COMMAND_MENU,
								 MENU_ITEM,
								    MENU_STRING, "List All",
								    MENU_GEN_PROC, display_all,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "List Tree",
								    MENU_GEN_PROC, display_tree,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "List Parent",
								    MENU_GEN_PROC, display_parent,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "List Category",
								    MENU_GEN_PROC, display_category,
								    NULL,
								 MENU_GEN_PIN_WINDOW, tdlist, "List",
								 NULL);
	xv_set(list_menu, MENU_DEFAULT, 2, 0);
	
	list_all = xv_create(tdl_control, PANEL_BUTTON,
								PANEL_LABEL_STRING, "List All",
								XV_HELP_DATA,       "xvtdl_main:listall",
#ifdef XVIEW3
								PANEL_LABEL_WIDTH,  50,
#endif
								XV_X, 260,
								XV_Y, 40,
								PANEL_ITEM_MENU, list_menu,
								0);

	print_tdl = xv_create(tdl_control, PANEL_BUTTON,
								 PANEL_LABEL_STRING, "Print...",
								 XV_HELP_DATA,       "xvtdl_main:print",
#ifdef XVIEW3
								 PANEL_LABEL_WIDTH,  50,
#endif
								 XV_X, 350,
								 XV_Y, 40,
								 PANEL_NOTIFY_PROC,  start_print,
								 0);

	backward = xv_create(tdl_control, PANEL_BUTTON,
								XV_X,  0,
								XV_Y,  0,
								XV_HELP_DATA,       "xvtdl_main:backward",
								PANEL_LABEL_IMAGE, backward_image,
								PANEL_NOTIFY_PROC, go_backward,
								0);

	forward = xv_create(tdl_control, PANEL_BUTTON,
							  PANEL_LABEL_IMAGE, forward_image,
							  XV_HELP_DATA,      "xvtdl_main:forward",
							  XV_X,  60,
							  XV_Y,  0,
							  PANEL_NOTIFY_PROC, go_forward,
							  0);

	file_menu = xv_create(XV_NULL, MENU_COMMAND_MENU,
								 MENU_ITEM,
								    MENU_STRING, "Load",
								    MENU_GEN_PROC, load_file,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Merge",
								    MENU_GEN_PROC, merge_file,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Save",
								    MENU_GEN_PROC, save_file,
								    NULL,
								 MENU_ITEM,
								    MENU_STRING, "Save Category",
								    MENU_GEN_PROC, save_cat_file,
								    NULL,
								 MENU_GEN_PIN_WINDOW, tdlist, "File",
								 NULL);	

	file_actions = xv_create(tdl_control, PANEL_BUTTON,
								  PANEL_LABEL_STRING, "File",
								  XV_HELP_DATA,       "xvtdl_main:file",
								  XV_X, 90,
								  XV_Y, 65,
								  PANEL_ITEM_MENU, file_menu,
								  0);

	close_tdl = xv_create(tdl_control, PANEL_BUTTON, 
								 PANEL_LABEL_STRING, "Done",
								 XV_HELP_DATA,       "xvtdl_main:done",
#ifdef XVIEW3
								 PANEL_LABEL_WIDTH,  50,
#endif
								 XV_X, 0,
								 XV_Y, 65,
								 PANEL_NOTIFY_PROC, close_tdlist,
								 0);
	
	goto_today = (Panel_item) xv_create(tdl_control, PANEL_BUTTON,
													PANEL_LABEL_STRING, "Today",
													XV_HELP_DATA,       "xvtdl_main:today",
#ifdef XVIEW3
													PANEL_LABEL_WIDTH,  50,
#endif
													XV_X,  170,
													XV_Y,  40,
													PANEL_NOTIFY_PROC, go_today,
													0);

	quit_tdl = xv_create(tdl_control, PANEL_BUTTON,
								PANEL_LABEL_STRING, "Quit",
								XV_HELP_DATA,       "xvtdl_main:quit",
#ifdef XVIEW3
								PANEL_LABEL_WIDTH,  50,
#endif
								XV_X, 350,
								XV_Y, 65,
								PANEL_NOTIFY_PROC, quit,
								0);

	window_fit_height(tdl_control);

	/*
    *  Now the TDL list...
    */

	tdl_list = xv_create(tdlist, PANEL,
								WIN_BELOW, tdl_control,
								XV_WIDTH,  WIN_EXTEND_TO_EDGE,
								XV_HEIGHT, WIN_EXTEND_TO_EDGE,
								XV_X, 0,
								PANEL_BACKGROUND_PROC, list_events,
								PANEL_ACCEPT_KEYSTROKE, TRUE, 
								0);

	todo = xv_create(tdl_list, PANEL_LIST,
						  XV_HELP_DATA,       "xvtdl_main:todolist",
						  PANEL_LIST_WIDTH, 400,
						  PANEL_LIST_DISPLAY_ROWS, 10,
						  PANEL_CHOOSE_NONE, TRUE,
						  PANEL_CHOOSE_ONE, TRUE,
						  PANEL_NOTIFY_PROC, tdl_notify_proc,
						  PANEL_READ_ONLY, TRUE,
						  PANEL_ITEM_MENU, edit_menu,
						  0);


	/*
	 *  FInally, the calendar.  Notice that the calendar is a canvas...
    *  but is treated like an X11 graphics surface.
    */

	calendar = xv_create(tdlist, CANVAS,
								XV_X,           0,
								XV_Y,           0,
								XV_WIDTH,       175,
								XV_HEIGHT,      100,
								XV_HELP_DATA,           "xvtdl_main:calendar",
								CANVAS_X_PAINT_WINDOW,  TRUE,
								CANVAS_REPAINT_PROC,    repaint_proc,
								0);

    /* Set input mask */
	xv_set(canvas_paint_window(calendar),
			 WIN_EVENT_PROC,         calendar_event_proc,
			 WIN_IGNORE_EVENTS,
			    LOC_WINENTER, LOC_WINEXIT,
			    0,
			 WIN_CONSUME_EVENTS,
             WIN_MOUSE_BUTTONS,
			    0,
			 0);

	/** Tack an event proc onto the window **/
	xv_set(tdlist, WIN_EVENT_PROC, tdl_events, 0);

	/******  Entry Editor Window  *****/

	entry_editor = entry_entry_frame_objects_initialize(XV_NULL, tdlist);

	entry_frame = entry_editor->entry_frame;
	entry_category = entry_editor->entry_category;
	xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
	entry_category_name = entry_editor->entry_category_name;
	xv_set(entry_category_name, PANEL_LABEL_STRING, category_head->name, 0);
	recurring = entry_editor->recurring;
	entry_done = entry_editor->entry_done;
	entry_text = entry_editor->entry_text;
	entry_deadline = entry_editor->entry_deadline;

	/******  The recurrence editor *****/

	recurring_frame = xv_create(tdlist, FRAME_CMD,
										 FRAME_LABEL, "Recurrence Editor",
										 FRAME_CMD_PUSHPIN_IN, FALSE,
										 XV_WIDTH, 680,
/*										 XV_HEIGHT, 170,*/
										 0);

	recurring_panel = xv_create(recurring_frame, PANEL,
										 XV_X, 0,
										 XV_Y, 0,
										 XV_WIDTH, 680,
										 0);
	
	freq = xv_create(recurring_panel, PANEL_CHOICE,
				 XV_HELP_DATA,       "recurring:frequency",
				 PANEL_LABEL_STRING,   "Freq:",
				 PANEL_CHOICE_STRINGS, "None",
						                 "Daily",
				                       "Weekly",
				                       "BiWeekly",
				                       "Monthly",
				                       "Yearly",
				                       0,
				 PANEL_DISPLAY_LEVEL,  PANEL_ALL,
				 PANEL_VALUE_X,        130,
				 PANEL_VALUE_Y,        10,
				 PANEL_NOTIFY_PROC,    choose_recurring,
			    PANEL_VALUE,          0,
				 0);

	day_of_week = xv_create(recurring_panel, PANEL_CHOICE,
									XV_HELP_DATA,         "recurring:dow",
									PANEL_LABEL_STRING,   "Day of Week:",
									PANEL_CHOICE_STRINGS,
									        "Sunday",
									        "Monday",
				                       "Tuesday",
				                       "Wednesday",
				                       "Thursday",
									        "Friday",
				                       "Saturday",
				                       0,
									PANEL_DISPLAY_LEVEL,  PANEL_ALL,
									PANEL_CHOOSE_ONE,     FALSE,
									PANEL_VALUE_X,        130,
									PANEL_VALUE_Y,        40,
									PANEL_VALUE,          0,
									XV_SHOW,              FALSE,
									0);
	
	week_of_month = xv_create(recurring_panel, PANEL_CHOICE,
									XV_HELP_DATA,         "recurring:wom",
									PANEL_LABEL_STRING,   "Week of month:",
									PANEL_CHOICE_STRINGS,
									        "1st", "2nd", "3rd", "4th", "5th",
				                       0,
									PANEL_DISPLAY_LEVEL,  PANEL_ALL,
									PANEL_CHOOSE_ONE,     FALSE,
									PANEL_VALUE_X,        130,
									PANEL_VALUE_Y,        70,
									PANEL_VALUE,          0,
									PANEL_NOTIFY_PROC,    choose_wom,
									XV_SHOW,              FALSE,
									0);

   day_of_month = xv_create(recurring_panel, PANEL_NUMERIC_TEXT,
							XV_HELP_DATA,         "recurring:dom",
							PANEL_VALUE_X,        540,
							PANEL_VALUE_Y,        70,
							PANEL_LABEL_STRING,   "Day of month:",
							PANEL_VALUE_DISPLAY_LENGTH, 3,
							PANEL_MIN_VALUE, 1,
							PANEL_MAX_VALUE, 31,
							XV_SHOW,              FALSE,
							0);

	weeks = xv_create(recurring_panel, PANEL_NUMERIC_TEXT,
							XV_HELP_DATA,         "recurring:weeks",
							PANEL_VALUE_X,        130,
							PANEL_VALUE_Y,        70,
							PANEL_LABEL_STRING,   "# of weeks:",
							PANEL_VALUE_DISPLAY_LENGTH, 3,
							PANEL_MIN_VALUE, 0,
							PANEL_VALUE, 1,
							XV_SHOW,              FALSE,
							0);

	months = xv_create(recurring_panel, PANEL_NUMERIC_TEXT,
							XV_HELP_DATA,         "recurring:months",
							PANEL_VALUE_X,        130,
							PANEL_VALUE_Y,        100,
							PANEL_LABEL_STRING,   "# of months:",
							PANEL_VALUE_DISPLAY_LENGTH, 3,
							PANEL_MIN_VALUE, 1,
							XV_SHOW,              FALSE,
							0);

	recurring_cancel = xv_create(recurring_panel, PANEL_BUTTON, 
										  XV_HELP_DATA,       "recurring:cancel",
										  PANEL_LABEL_STRING, "Cancel",
										  PANEL_NOTIFY_PROC,  cancel_recurring,
										  PANEL_VALUE_X,      560,
										  PANEL_VALUE_Y,      40,
										  0);

	recurring_done = xv_create(recurring_panel, PANEL_BUTTON, 
										XV_HELP_DATA,       "recurring:done",
										PANEL_LABEL_STRING, "Done",
										PANEL_VALUE_X,      630,
										PANEL_VALUE_Y,      40,
										PANEL_NOTIFY_PROC,  close_recurring, 
										0);

	xv_set(recurring_panel, XV_HEIGHT, 60, 0);
	xv_set(recurring_frame, XV_HEIGHT, 60, 0);

	/*
    *  Initialize the rest by calling their routines
    */
	initialize_category_editor();
	initialize_print();
	initialize_props();
	initialize_log_editor();
	initialize_deadline();
	initialize_file_stuff();
}

/*
 * repaint_proc()
 *      Called to repaint the canvas in response to damage events
 *      and the initial painting of the canvas window.
 */
void
repaint_proc(canvas, paint_window, dpy, xwin, xrects)
Canvas        canvas;           /* Ignored */
Xv_Window     paint_window;     /* Ignored */
Display      *dpy;
Window        xwin;
Xv_xrectlist *xrects;           /* Ignored */
{
    GC gc = DefaultGC(dpy, DefaultScreen(dpy));

    XClearWindow(dpy, xwin);
	 print_calendar(curr_month, curr_year);
}

void reset_date_message()
{
	char day_title[50];
	struct tm *now;
   struct timeval tv;

   now = localtime(&tv.tv_sec);
	now->tm_mon = curr_month - 1;
	now->tm_mday = curr_day;
	now->tm_year = curr_year-1900;
	now->tm_wday = zeller(curr_month, curr_day, curr_year);
	
	strftime(day_title, 50, "%A, %B %e, %Y", now);
	xv_set(date_message, PANEL_LABEL_STRING, day_title, 0);
}
