/*
 * The main C routine for Robot. This sets up the GUI controls etc.
 * and catches instructions for Robot. The instrctions are then
 * sent to the FORTRAN program robot.f (or the f2c translated version)
 *	Robin Corbet,	PSU
 */

/* this is a kludge to enable gcc and Sun's FORTRAN to be used together */
int __main(){}
/* this is another one - believed to be needed for Sun's unbundled compiler? */
int MAIN_;


#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/textsw.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/xv_error.h>
#include <xview/dragdrop.h>
#include <xview/scrollbar.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>

#include "robot.h"
unsigned short int robot_bits[] = {
#include "robot_icon.h"
};
unsigned short plot_bits[] = {
#include "plot_icon.h"
};
unsigned short old_plot_bits[] = {
#include "old_plot_icon.h"
};
#include "patchlevel.h"


/* defines for log file disposal */

#define	CP "/bin/cp"
/* #define RM "/bin/rm" -- replaced with unlink */
#define MV "/bin/mv"

#ifdef USA
#define ANALYSE "Analyze"
#else
#define ANALYSE "Analyse"
#endif


char pathname[MAXPATHLEN];	/* stores path name for getwd */

long int	ilength = ILENGTH;
Frame           frame;
Frame		canvas_frame;
Frame		old_canvas_frame;
Xv_window	old_window;
static int	old_window_id;
static	int	no_of_canvases = 1;
static int	OLD_PLOT_KEY;
Panel		panel[NO_PANELS];
Textsw          text_window;
Canvas		canvas;
Display		*dpy;

Xv_Server		server;

Xv_drop_site	drop_site;
void drop_proc();

Bool	not_open_look = False;

/* pull-down menus */
Menu		menu[NMENUS];
Menu		sub_menu[NSUBMENUS];

Panel_item	alias_button;
static Menu	alias_menu;
void		make_alias_menu();

Menu_item	mi;

Panel_item	text_input;
Panel_item	mouser;
void		mouse_proc();

Panel_item	general_item;	/* multi-use item indicator */

char inst[ILENGTH];
struct robot Robot;
Bool	I_hate_typing = TRUE;
Bool	want_specials = FALSE; /* whether special 
				characters panel is to be shown */
Bool	test_specials = FALSE;
int	get_it_time = 0;
struct	help_text	help_name[HELPMAX];
int	help_number;

struct	alias_text	alias_name[ALIASMAX];
int	alias_number;

void no_consume();	/* stops Xview getting events */
void do_consume();	/* lets Xview get events again */
void hide_me();
void totext_();
void robotf_();
int  call_robot = FALSE;
void after_robot();
void repaint_panels();
char *helper();

/* facilities for dealing with log files etc. */
static char	log_file[61];
static char	info_file[61];
char	directory[100];
char	sys_buffer[140];

/* Text input panel */
Frame		get_text_frame;
Panel		get_text_panel;
Panel_item	get_text_string, get_text_message[4], specials_button;
void		grab_text();
char		prompt0[ILENGTH*2];
char		prompt1[ILENGTH*2];
char		prompt2[ILENGTH*2];
char		prompt3[ILENGTH*2];
int		nprompt = 0;
int		prompt_menu = 0;
#define		DEFAULT_PROMPT	" "
/* scrollable list input */
Frame		answer_select_frame;
Panel		answer_select_panel;
Panel_item	select_list;
void		answer_select(), answer_ok(), answer_click();
void		preset_();

/* for temporary use in switching on/off pinned menus */
Frame	subframe;
Panel	subpanel;

/* fitting panel */
Panel		fit_main_panel;
void		show_fitter();


/* information */
void		show_about(), about_frame_create();

/* history */
void		show_history();
Panel		history_panel;

/* text editor */
Panel		text_editor_panel;

/* axes */
void		show_axes();
Panel		axes_panel;

/* zoom */
void		show_zoom();
Panel		zoom_panel;

/* split window */
void		show_split();
Panel		split_panel;

/* special characters */
void		show_specials();
Panel		specials_panel;

/* the colour panel */
void		show_colour();
Panel		colour_panel;


/* declarations for Astro D 3D FITS panel */
void		show_ad(), ad_make(), ad_set();
Frame		ad_frame;
Panel		ad_panel; 
Panel_item	ad_name, ad_sub_panel[NO_AD_PANELS];

/* declarations for print panel */

void		show_print(), do_print(), print_now_call();
Frame		print_frame;
Panel		print_panel;
Panel_item	print_now, direction_choice, plotfile_input, plotdir_input,
			postscript_printer, postscript_number, ps_type, ps_scale;


/* The close down panel */
Notify_value	destroy_func();
void		finish(), finish_hide(), save_quit(), delete_quit();
void		save_continue(), clear_continue();
Frame		finish_frame;
Panel		finish_panel;
Panel_item	finish_dir_name, finish_log_name, finish_info_name;

Server_image	svr_image;
Server_image	plot_image;
Server_image	old_plot_image;
Icon		robot_icon;
Cursor		cursor;

void get_text(), get_button_text(), menu_proc(), to_robot(), make_menu();
void make_sub_menu();
void inactivate(), switch_off();
void activate(), switch_on(), switch_everything_on(), switch_fonts_on();
int robot_error();
int robot_x_error();



/* sub-menu items */
	static char	*arrow_menu_item_names[] = {
		"Arrow", "", 
		"ArrowLine", "ArrowFill", "ArrowHollow", "",
		"ArrowSize", "ArrowAngle", "",
		"SingleArrow", "DoubleArrow", "last",};

	static char	*font_menu_item_names[] = {
		"Font", 
		"Times", "Helvetica", "Courier",
		"AvantGarde", "Bookman", "HelveticaNarrow",
		"NewCentury", "Palatino", "",
		"Symbol", "Chancery", "Dingbats", "Kanji",
		"last",};

	static char	*text_size_menu_item_names[] = {
		"Text Size", 
		"10 Point", "12 Point", "14 Point", "18 Point",
		"last",};

	static char	*text_style_menu_item_names[] = {
		"Text Style", 
		"Normal", "Bold", "Italic", "Bold Italic", 
		"last",};

	static char	*line_style_menu_item_names[] = {
		"Line Style", "SolidLines", "DashedLines", "DottedLines", 
		"DotDashedLines",
		"last",};

	static char	*plot_mode_menu_item_names[] = {
		"Plot Mode", "SymbolSize", "FillStyle",

		"last",};



/* menu items */

	static char    *basic_menu_item_names[] = {
		"Datafile", "File", "Goplot", "New Page",
		"Rescale", "Reset", "last",};
	static char    *fit_menu_item_names[] = {
		"Curve Fit", "Polynomial Fit", "Linear Fit", "",
		"Plot Curve", "Plot Dashed Curve",
		"Curmodelplot", "Curmodelplot Dashed",
		"Plot Polynomial",
		"Plot Linear Fit", "", "Subtract Curve", "Add Curve",
		"Subtract Polynomial", "", "Convergence", "Fit Mode",
		"Grid", "last",};
	static char    *draw_menu_item_names[] = {
		"Line", "Arrow", "Dashed Line", "Box", "Filled Box", 
		"Circle", "Ellipse", "Arc", "Cursor", "", 
		"PolyLine", "Polygon", "PolyFill", "", 
		"Line Width 0",
		"Line Width 1", "Line Width 2",
		"Line Width 4", "Line Width 8", "",
		"Line Style", "",
		"User Axis", "Log Axis", "User Axis Label", 
		"Log Axis Label", "last",};
	static char   *words_menu_item_names[] = {
		"Text", "",
		"Text Size",
		"Text Style",
		"Font", "",
		"Text Angle", "last", };
	static char    *numbers_menu_item_names[] = {
		"X Arithmetic", "Y Arithmetic", "Z Arithmetic", "", 
		"X Function", "Y Function", "Z Function", "",
		"Xerrarith", "Yerrarith", "", "Variable", "Assign", "",
		"Log X", "Log Y", "Log X&Y", "", "Z Reset", "last",};
	static char    *io_menu_item_names[] = {
		"Table", "Infilenotext", "Infiletext", "Overplot",
		"Moreplot", "Type", "Overtype", "Write Data", "last",};
	static char    *style_menu_item_names[] = {
		"Plot Mode", "Title", "X Label", "Y Label", "Othertitles",
		"Data Limits", "Page Use", "Divide Window", "Select Window",
		"Titles", "No Titles", "Axes", "No Axes",
		"Label Axes", "Don't Label Axes",
		"Label X Axis", "Don't Label X Axis",
		"Label Y Axis", "Don't Label Y Axis",
		"Minor Tick Marks", "No Minor Tick Marks",
		"Linear Axes", "Logarithmic Axes",
		"Linear X Axis", "Log X Axis",
		"Linear Y Axis", "Log Y Axis",
		"Plot Grid", "Don't Plot Grid",
		"last",};
	static char    *analyse_menu_item_names[] = {
		"Sort", "Sort Y", "", "Smooth", "SmoothN", "Rebin",
		"", "Data and Fit Limits", "",
     		"Fold","Bin Fold","Find Period","Periodogram",
     		"Randomise","Bin fold best","Fold Best","", "CCF",
		"last",};
	static char	*statistics_menu_item_names[] = {
		"Moments", "Median", "Kendall", "York", "last",};
	static char    *astro_menu_item_names[] = {
		"RA Dec to Galactic", "RA Dec Plot", "RA Dec Grid", 
		"FITS", "FITS Table...", "last",};

	

Bool landscape;
void orient();

void	newpa_();
void	null_event_proc();
void	canvas_event_proc();

int	width, height; /* size of plot area */

void
main(argc, argv)
int	argc;
char	*argv[];
{
char label[ILENGTH];
int i;
	help_number = -1;
	alias_number = -1;
	OLD_PLOT_KEY = xv_unique_key();

	server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv,
			XV_ERROR_PROC,	robot_error,
			XV_X_ERROR_PROC,	robot_x_error,
				NULL);

/* check whether we specified landscape or portrait mode */
	width = MY_CANVAS_WIDTH;
	height = MY_CANVAS_HEIGHT;
	landscape = FALSE;
	not_open_look = False;

	for( i = 0; i < argc; i++){
		if(streq(argv[i], "-landscape")){
			height = MY_CANVAS_WIDTH;
			width = MY_CANVAS_HEIGHT;
			landscape = TRUE;
		}
		else if(streq(argv[i], "-notol")){
			not_open_look = True;
		}
	}
			

/* get default value for multi click */
	Robot.multiclick = defaults_get_integer(
		"openwindows.multiclicktimeout", 
			"OpenWindows.MultiClickTimeout", 4);


/* display version number at top of frame */
	sprintf(label, "Robot version %-1.2f patch  %d",
				VERSION, PATCHLEVEL);


	if(getenv("HOST") != NULL){
		strcpy(sys_buffer, (char *) getenv("HOST"));
		strcat(label, " (");
		strcat(label, sys_buffer);
		strcat(label, ")");
	}

/* explicitly set the height and the width of the frame -
 * this seems to alleviate some problems with trying to draw
 * to the canvas before the notify loop is up and running 
 * and all other initializations are finished if we do
 * a window_fit(frame) 
 */

	frame = (Frame) xv_create(NULL, FRAME,
				  XV_HELP_DATA, "robot:frame",
				  FRAME_LABEL, label,
				  XV_WIDTH, ROBOT_WIDTH,
				  FRAME_SHOW_FOOTER, TRUE,
				  NULL);

	dpy = (Display *) xv_get(frame, XV_DISPLAY);

	xv_set(frame, FRAME_LEFT_FOOTER,
		(char *)getwd(pathname),
		NULL);


	notify_interpose_destroy_func(frame, destroy_func);

/* Robot icon */

	svr_image = (Server_image)xv_create (XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 	64,
		XV_HEIGHT, 	64,
		SERVER_IMAGE_BITS,	robot_bits,
		NULL);
	robot_icon = (Icon)xv_create(XV_NULL, ICON,
			ICON_IMAGE,	svr_image,
			ICON_TRANSPARENT,	TRUE,
			XV_X,	100,
			XV_Y,	100,
			NULL); 
	xv_set(frame, FRAME_ICON, robot_icon, NULL); 





	panel[0] = (Panel) xv_create(frame, PANEL,
				  XV_HELP_DATA, "robot:panel0",
				  WIN_X,	0,
				  XV_WIDTH, ROBOT_WIDTH,
				  XV_HEIGHT, PANEL0_HEIGHT,
				  NULL);



/* Most common instructions go as buttons */
	i = 0;
	while  (strne(basic_menu_item_names[i], "last")) {
	xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING, basic_menu_item_names[i],
			XV_HELP_DATA, helper(basic_menu_item_names[i]),
			PANEL_NOTIFY_PROC,	get_button_text,
			NULL);

	i++;
	}

/* The quit button */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:tidy_up",
			PANEL_LABEL_STRING, "End...",
			PANEL_NOTIFY_PROC,	finish,
			NULL);




/* A button to change plotting colour  */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:colour",
#ifdef USA
			PANEL_LABEL_STRING, "Color...",
#else
			PANEL_LABEL_STRING, "Colour...",
#endif
			PANEL_NOTIFY_PROC,	show_colour,
			NULL);

/* and attach the colour frame to go with it */
	colour_frame_create(frame);

/* create a frame for browsing directories and selecting a file */
	dir_frame_create(frame);


/* panel to control disposal of files at the termination
 * of the program
 */

	finish_frame = xv_create(frame, FRAME_CMD,
				FRAME_LABEL, "Close Down",
				FRAME_SHOW_FOOTER,	TRUE,
				FRAME_CMD_PUSHPIN_IN, FALSE,
				NULL);
	finish_panel = (Panel) xv_get(finish_frame, FRAME_CMD_PANEL);

	xv_set(finish_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	finish_dir_name = xv_create(finish_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Directory:",
			PANEL_VALUE_DISPLAY_LENGTH, 30,
			XV_HELP_DATA,	"robot:finish_dir_name",
			NULL);

	finish_log_name = xv_create(finish_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Save log file as:",
			PANEL_VALUE_DISPLAY_LENGTH, 20,
			XV_HELP_DATA,	"robot:finish_log_name",
			NULL);

	finish_info_name = xv_create(finish_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Save results file as:",
			PANEL_VALUE_DISPLAY_LENGTH, 20,
			PANEL_NOTIFY_PROC, save_quit,
			XV_HELP_DATA,	"robot:finish_info_name",
			NULL);

	xv_create(finish_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Copy Log Files and Continue",
			PANEL_NOTIFY_PROC,	save_continue,
			XV_HELP_DATA,	"robot:save_continue",
			NULL);
	xv_create(finish_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Clear Log Files and Continue",
			PANEL_NOTIFY_PROC,	clear_continue,
			XV_HELP_DATA,	"robot:clear_continue",
			NULL);

	xv_set(finish_panel,
		PANEL_DEFAULT_ITEM, xv_create(finish_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Save Log Files and Quit",
			PANEL_NOTIFY_PROC,	save_quit,
			XV_HELP_DATA,	"robot:save_quit",
			NULL), NULL);

	xv_create(finish_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC,	delete_quit,
			PANEL_LABEL_STRING, "Delete Log Files and Quit",
			XV_HELP_DATA,	"robot:delete_quit",
			NULL);

	xv_create(finish_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC,	finish_hide,
			PANEL_LABEL_STRING, "Continue with Robot",
			XV_HELP_DATA,	"robot:finish_hide",
			NULL);

	window_fit(finish_panel);
	window_fit(finish_frame);


/* frame for getting text */
	get_text_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "Input Reply",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_DONE_PROC, grab_text,
			NULL);
	get_text_panel = xv_get(get_text_frame, FRAME_CMD_PANEL);
	xv_set(get_text_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	get_text_message[0] = xv_create(get_text_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING, DEFAULT_PROMPT, 
				NULL);
	get_text_message[1] = xv_create(get_text_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING, "", 
				NULL);
	get_text_message[2] = xv_create(get_text_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING, "", 
				NULL);
	get_text_message[3] = xv_create(get_text_panel, PANEL_MESSAGE,
				PANEL_LABEL_STRING, "", 
				NULL);
	get_text_string = xv_create(get_text_panel, PANEL_TEXT,
				PANEL_LABEL_STRING, "Reply>",
				PANEL_VALUE_DISPLAY_LENGTH, 30,
				PANEL_NOTIFY_PROC, grab_text,
				XV_HELP_DATA, "robot:reply",
				NULL);
	specials_button = xv_create(get_text_panel, PANEL_BUTTON,
				PANEL_NOTIFY_PROC, show_specials,
				PANEL_LABEL_STRING, "Special Characters...",
				XV_HELP_DATA, "robot:specials",
				XV_SHOW, FALSE,
				NULL);

	window_fit(get_text_panel);
	window_fit(get_text_frame);

/* or a panel list to use instead */

	answer_select_frame = xv_create(frame, FRAME_CMD,
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_DONE_PROC, answer_ok,
			NULL);
	answer_select_panel = xv_get(answer_select_frame, FRAME_CMD_PANEL);
	xv_set(answer_select_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	select_list = xv_create(answer_select_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	15,
			PANEL_LIST_WIDTH,		140,
			PANEL_NOTIFY_PROC,		answer_click,
			XV_HELP_DATA,			"robot:select_list",
			NULL);
	xv_create(answer_select_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING,	"OK",
			XV_HELP_DATA,	"robot:answer_ok",
			PANEL_NOTIFY_PROC,	answer_ok,
			NULL);

	window_fit(answer_select_panel);
	window_fit(answer_select_frame);



/* A button to control printing  */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:print",
			PANEL_LABEL_STRING, "Print...",
			PANEL_NOTIFY_PROC,	show_print,
			NULL);

/* and the command frame to go with it */

	print_frame = xv_create(frame, FRAME_CMD,
				FRAME_LABEL, "Print Properties",
				FRAME_SHOW_FOOTER,	TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
	print_panel = xv_get(print_frame, FRAME_CMD_PANEL);
	xv_set(print_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	plotdir_input = xv_create(print_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "PostScript output directory:",
			PANEL_VALUE_DISPLAY_LENGTH,	30,
			PANEL_VALUE_X,		210,
			PANEL_INACTIVE,		FALSE,
			XV_HELP_DATA,		"robot:postscript_dir",
			NULL);
	xv_set(plotdir_input, PANEL_VALUE, (char *) getwd(pathname), NULL);
	if(xv_get(plotdir_input, PANEL_VALUE) == NULL)
		xv_set(plotdir_input, PANEL_VALUE, 
			(char *) getenv("HOME"), NULL);
	plotfile_input = xv_create(print_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "PostScript file name:",
			PANEL_VALUE,	"temp.ps",
			PANEL_VALUE_DISPLAY_LENGTH,	30,
			PANEL_VALUE_X,		210,
			PANEL_VALUE_Y,		30,
			PANEL_INACTIVE,		FALSE,
			XV_HELP_DATA,		"robot:postscript_file",
			NULL);
	postscript_printer = xv_create(print_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Printer name (optional):",
			PANEL_VALUE,	(char *)getenv("PRINTER"),
			PANEL_VALUE_DISPLAY_LENGTH,	30,
			PANEL_VALUE_X,		210,
			PANEL_VALUE_Y,		55,
			XV_HELP_DATA,		"robot:postscript_printer",
			PANEL_INACTIVE,		TRUE,
			NULL);

	postscript_number = xv_create(print_panel, PANEL_NUMERIC_TEXT,
			PANEL_LABEL_STRING, "Number of copies:",
			PANEL_VALUE,	(char *)getenv("PRINTER"),
			PANEL_VALUE_DISPLAY_LENGTH,	3,
			PANEL_VALUE_X,		210,
			PANEL_VALUE_Y,		80,
			PANEL_VALUE,		1,
			PANEL_MIN_VALUE,	1,
			PANEL_MAX_VALUE,	10, /* to limit mistakes! */
			XV_HELP_DATA,		"robot:postscript_number",
			PANEL_INACTIVE,		TRUE,
			NULL);

	direction_choice = xv_create(print_panel, PANEL_CHOICE,
		XV_HELP_DATA,		"robot:direction_choice",
		PANEL_LABEL_STRING,	"Plot direction:",
		PANEL_CHOICE_STRINGS,	"Portrait", "Landscape", NULL,
		PANEL_VALUE,		0,
		PANEL_VALUE_X,		210,
		PANEL_VALUE_Y,		100,
		NULL);

	if(landscape) xv_set(direction_choice, PANEL_VALUE, 1, NULL);


	print_now = xv_create(print_panel, PANEL_CHOICE,
		XV_HELP_DATA,		"robot:print_now",
		PANEL_LABEL_STRING,	"Print Now?",
		PANEL_CHOICE_STRINGS,	"Yes", "No - write to file", NULL,
		PANEL_VALUE,		1,
		PANEL_VALUE_X,		210,
		PANEL_NOTIFY_PROC,	print_now_call,
		NULL);
	ps_type = xv_create(print_panel, PANEL_CHOICE,
		PANEL_LABEL_STRING,	"Postscript Type?",
		XV_HELP_DATA,		"robot:ps_type",
		PANEL_CHOICE_STRINGS,	"Ordinary", "EPS special", NULL,
		PANEL_VALUE,		0,
		PANEL_VALUE_X,		210,
		NULL);
	ps_scale = xv_create(print_panel, PANEL_CHOICE,
		PANEL_LABEL_STRING,	"Plot Scale?",
		XV_HELP_DATA,		"robot:ps_scale",
		PANEL_CHOICE_STRINGS,	"Same as display", "Fit to page", NULL,
		PANEL_VALUE,		0,
		PANEL_VALUE_X,		210,
		NULL);
	xv_set(print_panel, PANEL_DEFAULT_ITEM,
	   xv_create(print_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:do_it",
		PANEL_LABEL_STRING,	"Do It!",
		PANEL_NOTIFY_PROC,	do_print,
		PANEL_VALUE_X,		210,
		NULL),
	   NULL);

if(not_open_look){	
           xv_set(print_panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	   xv_create(print_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	window_fit(print_panel);
	window_fit(print_frame);



/* A frame to specify how Astro D 3D FITS files will be read */
	ad_make();


/* CURFIT control panel */
	fit_frame_create(frame, fit_main_panel);


/* A button to bring up the Astro D panel  */
/*	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:fits_table",
			PANEL_LABEL_STRING, "FITS Table...",
			PANEL_NOTIFY_PROC,	show_ad,
			NULL); */

	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:split_window",
			PANEL_LABEL_STRING, "Split Window...",
			PANEL_NOTIFY_PROC,	show_split,
			NULL);

	make_split_frame(frame);

/* A button to bring up the Fitting control panel  */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:fitter",
			PANEL_LABEL_STRING, "Fitter...",
			PANEL_NOTIFY_PROC,	show_fitter,
			NULL);

/* history of Robot commands */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:history",
			PANEL_LABEL_STRING, "History...",
			PANEL_NOTIFY_PROC,	show_history,
			NULL);

	make_history_frame(frame);

/* text editor used if text in plot is double clicked */
	make_text_editor_frame(frame);

/* Control frame for axes, tick sizes etc. */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:axis_controls",
			PANEL_LABEL_STRING, "Axes...",
			PANEL_NOTIFY_PROC,	show_axes,
			NULL);

	make_axes_frame(frame);

/* Control frame for zooming and panning */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:zoom_controls",
			PANEL_LABEL_STRING, "Zoom...",
			PANEL_NOTIFY_PROC,	show_zoom,
			NULL);

	make_zoom_frame(frame);

/* frame which shows "special" characters accessed via code
 * sequences */
	make_specials_frame(frame);
	

/* report errors by mail */
	xv_create(panel[0], PANEL_BUTTON,
			XV_HELP_DATA,	"robot:about",
			PANEL_LABEL_STRING, "About Robot...",
			PANEL_NOTIFY_PROC,	show_about,
			NULL);
	about_frame_create(frame);

/* orientation change */
	xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING, "Flip Orientation",
			XV_HELP_DATA,	"robot:flip",
			PANEL_NOTIFY_PROC, orient,
			NULL);

/* new window */

	xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING, "New Plot Area...",
			XV_HELP_DATA,	"robot:new_plot",
			PANEL_NOTIFY_PROC, get_button_text,
			NULL);


	make_alias_menu();

	make_sub_menu("Arrows", arrow_menu_item_names, ARROW_MENU);
	make_sub_menu("Fonts", font_menu_item_names, FONT_MENU);
	make_sub_menu("Text Size", text_size_menu_item_names, TEXT_SIZE_MENU);
	make_sub_menu("Text Style", text_style_menu_item_names,
						 TEXT_STYLE_MENU);
	make_sub_menu("Line Style", line_style_menu_item_names,
						 LINE_STYLE_MENU);
	make_sub_menu("Plot Modes", plot_mode_menu_item_names,
						PLOT_MODE_MENU);



/* The rest of the commands go as menu buttons */
	i =0;
	make_menu("Fit", fit_menu_item_names, i); i++;
	make_menu("Draw", draw_menu_item_names, i); i++;
	make_menu("Words", words_menu_item_names, i); i++;
	make_menu("Numbers", numbers_menu_item_names, i); i++;
	make_menu("I/O", io_menu_item_names, i); i++;
	make_menu("Style", style_menu_item_names, i); i++;
	make_menu(ANALYSE, analyse_menu_item_names, i); i++;
	make_menu("Stats.", statistics_menu_item_names, i); i++; 
	make_menu("Astro", astro_menu_item_names, i);

/* Inactiavate commands that don't yet make sense (e.g. plot 
 * results of fitting before a fit has been done)
 */
	inactivate();

/* and for those who like to type */




	text_input = (Panel) xv_create(panel[0], PANEL_TEXT,
			XV_HELP_DATA, "robot:text_input",
			PANEL_LABEL_STRING,	"Robot Command:",
			PANEL_NOTIFY_PROC,	get_text,
			PANEL_VALUE_DISPLAY_LENGTH,	30,
			PANEL_NEXT_ROW, -1,
			NULL);

/* set general item initially to text_input - may change inside program */
	general_item = text_input;

/* buttons to control whether the mouse or keyboard input
 * is the default for positioning lines, text etc.
 */

	mouser = xv_create(panel[0], PANEL_CHOICE,
			XV_HELP_DATA,		"robot:mouser",
			PANEL_LABEL_STRING,	"Get Coordinates:",
			PANEL_CHOICE_STRINGS, 
				"By Typing",
				"With Mouse", NULL,
			PANEL_NOTIFY_PROC,	mouse_proc,
			PANEL_VALUE,	1,
			NULL);

	window_fit(panel[0]);

/* and add a text and a plotting window */

	text_window = (Textsw) xv_create(frame, TEXTSW,
					 XV_WIDTH, 
						xv_get(panel[0], XV_WIDTH),
					 XV_HEIGHT, TEXT_HEIGHT,
					 WIN_BELOW, panel[0],
					 TEXTSW_MEMORY_MAXIMUM, 40000,
					 TEXTSW_WRAPAROUND_SIZE, 40000,
					 TEXTSW_READ_ONLY,	FALSE,
					 TEXTSW_HISTORY_LIMIT, 0,
					 TEXTSW_AGAIN_RECORDING, FALSE,
					 WIN_X, 0,
					 NULL);
/* Plot area image */

	plot_image = (Server_image)xv_create (XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 	64,
		XV_HEIGHT, 	64,
		SERVER_IMAGE_BITS,	plot_bits,
		NULL);

/* what the icon will become if we create another plotting area */

	old_plot_image = (Server_image)xv_create (XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 	64,
		XV_HEIGHT, 	64,
		SERVER_IMAGE_BITS,	old_plot_bits,
		NULL);
/* frame to hold the plot */
	canvas_frame = xv_create(NULL, FRAME,
					FRAME_LABEL, "Robot plot area",
				  	XV_WIDTH, width,
				  	XV_HEIGHT, height,
				  	FRAME_SHOW_FOOTER, TRUE,
			          	FRAME_ICON,
		(Icon)xv_create(XV_NULL, ICON,
			ICON_IMAGE,	plot_image,
			ICON_TRANSPARENT,	TRUE,
			XV_X,	100,
			XV_Y,	100,
			NULL), 
					NULL); 
					

	canvas = (Canvas) xv_create(canvas_frame, CANVAS,
				  XV_HELP_DATA, "robot:canvas",
				  WIN_BIT_GRAVITY, NorthGravity,
				  CANVAS_FIXED_IMAGE, FALSE,
				  NULL);

	xv_set(canvas_paint_window(canvas), 
		 WIN_EVENT_PROC, canvas_event_proc,
		 WIN_CONSUME_EVENTS, LOC_DRAG, NULL,
		 XV_KEY_DATA, OLD_PLOT_KEY, "1",
		 NULL);


	/* install scrollbars if/when we find a use for them! */

	/* xv_create(canvas, SCROLLBAR,
			SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
			SCROLLBAR_SPLITTABLE, TRUE,
			NULL); */

	notify_interpose_destroy_func(canvas_frame, destroy_func);

	/* create a menu associated with the canvas */
	make_canvas_menu(frame);

	/* same but menu for old plot areas */
	make_old_canvas_menu(frame);


	DnD_init(server, panel[0]);

	window_fit(frame);

	xv_set(canvas_frame, XV_SHOW, TRUE, NULL);
	xv_set(frame, XV_SHOW, TRUE, NULL);
	repaint_panels();


	pltopn(canvas_frame, canvas);
	notify_dispatch();
	gshow_();


/* A call to Robot so set-ups can be done there if required */
	strcpy(inst,"!idle");
	robotf_(inst); 
	bufrst_();	   /* Tell robot that all of the data has been used */

/* If there's more than one argument left assume it's the name of
 * a robot file to be executed */

/* Arguments left are presumed to be file names to process
 * unless it begins with a "-" */
	for(i = 2; i <= argc; i++){
	if(!streqn(argv[i-1], "-")){	
/* is it a .dat file? */
		if(streq(argv[i-1] + strlen(argv[i-1]) - strlen(".dat"),
			".dat")){
		    sprintf(inst, "plotfile %s!", argv[i-1]);}
/* default is an instructions file */
		else{
		    sprintf(inst, "file %s!", argv[i-1]);}

		xv_set(text_input, PANEL_VALUE, inst, NULL);
		robotf_(inst);
		xv_set(text_input, PANEL_VALUE, "", NULL);
		bufrst_();
	}
	}

/* we don't do an xv_main_loop so that when robot is called we
 * can switch off the notifier. This then enables new XView windows
 * to be popped up from within robot. The notifier is restarted
 * within robot if required.
 */
	while (1){
		notify_start();
		gshow_();


		if(call_robot)
		  {
		   call_robot = FALSE;
		   robotf_(inst);
		   after_robot();
		}
	} 
}

void
get_text(event)
Event	event;
{
char temp_text[ILENGTH];
	strcpy(inst, (char *)xv_get(text_input, PANEL_VALUE));

/* check to see if this is a request for a stored instruction */
	if(inst[0] == '^' || inst[0] == '!'){
		strcpy(temp_text, inst);
		strcpy(inst, (char *)repeat_history(temp_text));
	}
	I_hate_typing = FALSE;
	
	to_robot();
}

void
mouse_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
/* Specify use of mouse/keyboard */
{
	if(value == 0)
		strcpy(inst,"KEYBOARD");
	else if(value == 1)
		strcpy(inst,"MOUSE");
	else printf("ERROR IN MOUSE_PROC!");
	to_robot();
}



void
get_button_text(item, event)
Panel_item	item;
Event		*event;
{
	xv_set(item, PANEL_INACTIVE, TRUE, NULL);
	general_item = item;
	showit();
	strcpy(inst, (char *) xv_get(item, PANEL_LABEL_STRING));
	to_robot();
}

void
menu_proc(amenu, menu_item)
	Menu		amenu;
	Menu_item	menu_item;
{
/* Dismiss treated differently */
	if(streq((char *)xv_get(menu_item, MENU_STRING), DISMISS)){
		subframe = xv_get(amenu, MENU_PIN_WINDOW);
		if(subframe != NULL){
			xv_set(subframe,
				XV_SHOW, FALSE,
				FRAME_CMD_PUSHPIN_IN, FALSE,
				NULL);
		}
	}
	else{
		strcpy(inst, (char *) xv_get(menu_item, MENU_STRING));
		to_robot();
	}
}

void
to_robot()
{
	no_consume();
	xv_set(text_input, PANEL_VALUE, inst, NULL);
	make_history(inst);
/* Convert label strings to real Robot instructions as needed */
		showit();

	
	exchange(inst, "New Page", "NXTPAG");
	exchange(inst, "New Plot Area...", "NEWPLOTAREA");
	exchange(inst, "Curve Fit", "CURFIT");
	exchange(inst, "Polynomial Fit", "POLFIT");
	exchange(inst, "Linear Fit", "LINFIT");
	exchange(inst, "Plot Curve", "PLOTCUR");
	exchange(inst, "Plot Dashed Curve", "PLOTCURD");
	exchange(inst, "Curmodelplot Dashed", "CURMODELPLOTD");
	exchange(inst, "Plot Polynomial", "PLOTPOLY");
	exchange(inst, "Plot Linear Fit", "PLOTFIT");
	exchange(inst, "Subtract Curve", "SUBCUR");
	exchange(inst, "Add Curve", "ADDCUR");
	exchange(inst, "Subtract Polynomial", "SUBPOLY");
	exchange(inst, "Fit Mode", "FITMODE");
	exchange(inst, "Dashed Line", "DASHEDLINE");
	exchange(inst, "Filled Box", "FILLEDBOX");
	exchange(inst, "User Axis", "USERAXIS");
	exchange(inst, "Log Axis", "LOGAXIS");
	exchange(inst, "User Axis Label", "USERAXISLABEL");
	exchange(inst, "Log Axis Label", "LOGAXISLABEL");
	exchange(inst, "Text Angle", "TANGLE");
	exchange(inst, "Text Style", "TEXTSTYLE");
	exchange(inst, "Line Style", "LINESTYLE");
	exchange(inst, "Text Size", "TEXTSIZE");
	exchange(inst, "10 Point", "TENPOINT");
	exchange(inst, "12 Point", "TWELVEPOINT");
	exchange(inst, "14 Point", "FOURTEENPOINT");
	exchange(inst, "18 Point", "EIGHTEENPOINT");
	exchange(inst, "Bold Italic", "BOLDITALIC");
	exchange(inst, "X Arithmetic", "XARITH");
	exchange(inst, "Y Arithmetic", "YARITH");
	exchange(inst, "Z Arithmetic", "ZARITH");
	exchange(inst, "X Function", "XFUNCTION");
	exchange(inst, "Y Function", "YFUNCTION");
	exchange(inst, "Z Function", "ZFUNCTION");
	exchange(inst, "Log X", "LOGX");
	exchange(inst, "Log Y", "LOGY");
	exchange(inst, "Log X&Y", "LOGXY");
	exchange(inst, "Z Reset", "NXTPAG");
	exchange(inst, "Write Data", "WRITEDATA");
	exchange(inst, "No Titles", "NOTITLES");
	exchange(inst, "No Axes", "NOAXES");
	exchange(inst, "Label Axes", "LABELAXES");
	exchange(inst, "Don't Label Axes", "NOLABELAXES");
	exchange(inst, "Don't Label X Axis", "NOLABELXAXIS");
	exchange(inst, "Label X Axis", "LABELXAXIS");
	exchange(inst, "Don't Label Y Axis", "NOLABELYAXIS");
	exchange(inst, "Label Y Axis", "LABELYAXIS");
	exchange(inst, "Minor Tick Marks", "MINORAXES");
	exchange(inst, "No Minor Tick Marks", "NOMINORAXES");
	exchange(inst, "Linear Axes", "LINEARAXES");
	exchange(inst, "Linear X Axis", "LINEARXAXIS");
	exchange(inst, "Linear Y Axis", "LINEARYAXIS");
	exchange(inst, "Logarithmic Axes", "LOGAXES");
	exchange(inst, "Log X Axis", "LOGXAXIS");
	exchange(inst, "Log Y Axis", "LOGYAXIS");
	exchange(inst, "Plot Grid", "PLOTGRID");
	exchange(inst, "Don't Plot Grid", "NOPLOTGRID");
	exchange(inst, "Plot Mode", "PLOTMODE");
	exchange(inst, "X Label", "XLABEL");
	exchange(inst, "Y Label", "YLABEL");
	exchange(inst, "Data Limits", "DATALIMITS");
	exchange(inst, "Page Use", "PAGEUSE");
	exchange(inst, "Divide Window", "DIVWINDOW");
	exchange(inst, "Select Window", "SELWINDOW");
	exchange(inst, "Sort Y", "SORTY");
	exchange(inst, "Data and Fit Limits", "DFLIMITS");
	exchange(inst, "Bin Fold", "BINFOLD");
	exchange(inst, "Find Period", "FINDPERIOD");
	exchange(inst, "Bin Fold Best", "BINFOLDBEST");
	exchange(inst, "Fold Best", "FOLDBEST");
	exchange(inst, "RA Dec to Galactic", "RADECTOGAL");
	exchange(inst, "RA Dec Plot", "RADECPLOT");
	exchange(inst, "RA Dec Grid", "RADECGRID");
	exchange(inst, "Line Width 0", "WIDTH0");
	exchange(inst, "Line Width 1", "WIDTH1");
	exchange(inst, "Line Width 2", "WIDTH2");
	exchange(inst, "Line Width 4", "WIDTH4");
	exchange(inst, "Line Width 8", "WIDTH8");
	exchange(inst, "End", "END");
	exchange(inst, "end", "END");


/* convert to upper case to ease comparisons 
	- SEE IF WE CAN GET AWAY WITHOUT THIS
	for (i = 0; i < strlen(inst); i++){
		if(islower(inst[i])) inst[i] = toupper(inst[i]);
	} */
		
/* treat END differently */
	if(streq(inst, "END")){
		finish();
	}
/* specials case of FITS table! */
	else if(streq(inst, "FITS Table...")){
		show_ad();
		after_robot();
	}
	else{
		strcat(inst, "!");  /*saves worry about FORTRAN/C problems! */
		call_robot = TRUE;
		notify_stop();
	}



}

void
after_robot()
/* tidy up things after call to robot 
 * getting things to work correctly seems to be very sensitive
 * to the order in which things are done. do_consume must
 * be right at the end (apart from flushing the buffer) and
 * setting the PANEL_CARET_ITEM has to be towards the end
 * but defore do_consume */
{
		bufrst_();	   /* Tell robot that all of the data has
					been used up */
	strcpy(inst,"");
/* default is that we don't like to type unless we typed a command
 * in the text window */
	I_hate_typing = TRUE;
/* reset prompts */
	preset_();
/* reset the get_text input to show we've not called it at all */
	get_it_time = 0;
/* focus onto text input */
	/* xv_set(text_input, PANEL_VALUE, NULL, NULL); */
	/* win_set_kbd_focus(panel[0], xv_get(panel[0], XV_XID)); */ 
	xv_set(text_input, PANEL_VALUE, "", NULL);
	repaint_panels();
	panel_paint(text_input, PANEL_CLEAR);
	xv_set(panel[0], PANEL_CARET_ITEM, text_input, NULL); 
	do_consume();      /*  let the events through to XView again */
	if(xv_get(general_item, PANEL_INACTIVE) == TRUE)
		xv_set(general_item, PANEL_INACTIVE, FALSE, NULL);
	showit();
}


void
repaint_panels()
{
	panel_paint(panel[0], PANEL_NO_CLEAR);
	/* panel_paint(text_input, PANEL_NO_CLEAR); */

}


/* make sub-menus - these are to be attached to other menu
 * items as appropriate */
void
make_sub_menu(title, list, imenu)
	char            title[];
	char           *list[];
	int             imenu;
{
	int             i;
	if ((imenu + 1) > NSUBMENUS) {
		fprintf(stderr, "TOO MANY SUB-MENUS, EDIT THE SOURCE !!!");
		return;
	}
	sub_menu[imenu] = (Menu) xv_create(NULL, MENU,
					   MENU_GEN_PIN_WINDOW, frame, title,
					   MENU_NOTIFY_PROC, menu_proc,
					   NULL);
	i = 0;
	while (strne(list[i], "last")) {


		mi = xv_create(NULL, MENUITEM,
			       MENU_STRING, list[i],
			       NULL);
		xv_set(sub_menu[imenu], MENU_APPEND_ITEM, mi, NULL);
		/* make blank items inactive */
		if (streq(list[i], "")){
			xv_set(mi, MENU_INACTIVE, TRUE, NULL);
		}
		else{
			xv_set(mi, XV_HELP_DATA, helper(list[i]), NULL);
		}

		i++;
	}

/* Dismiss for non- Open Look window managers */
	if(not_open_look){
		mi = xv_create(NULL, MENUITEM,
			MENU_STRING, "",
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(sub_menu[imenu], MENU_APPEND_ITEM, mi, NULL);	
		mi = xv_create(NULL, MENUITEM,
			MENU_STRING, DISMISS,
			NULL);
		xv_set(sub_menu[imenu], MENU_APPEND_ITEM, mi, NULL);

}

}

/* create a menu and also attach it to a menu button */
void
make_menu(title, list, imenu)
char	title[];
char	*list[];
int	imenu;
{
int i;
	if((imenu  + 1) > NMENUS){
		 fprintf(stderr, "TOO MANY MENUS, EDIT THE SOURCE !!!");
		 return;
	}
	menu[imenu] = xv_create(NULL, MENU,
				 MENU_GEN_PIN_WINDOW, frame, title,
				 MENU_NOTIFY_PROC, menu_proc, 
				 NULL);
	i = 0;
	while  (strne(list[i], "last")) {
	 
	 mi = xv_create(NULL, MENUITEM,
			  MENU_STRING, list[i],
					NULL);	
	xv_set(menu[imenu], MENU_APPEND_ITEM, mi, NULL);
/* make blank items inactive */
	if(streq(list[i], ""))
		xv_set(mi, MENU_INACTIVE, TRUE, NULL);
	else
		xv_set(mi, XV_HELP_DATA, helper(list[i]), NULL);
/* attach a pull-right menu */
	if(streq(list[i], "Arrow")){
		xv_set(mi, MENU_PULLRIGHT,
				sub_menu[ARROW_MENU], NULL);
	}
	else if(streq(list[i], "Font")){
		xv_set(mi, MENU_PULLRIGHT, 
			sub_menu[FONT_MENU], NULL);
	}
	else if(streq(list[i], "Text Size")){
		xv_set(mi, MENU_PULLRIGHT, 
				sub_menu[TEXT_SIZE_MENU], NULL);
	}
	else if(streq(list[i], "Text Style")){
		xv_set(mi, MENU_PULLRIGHT, 
				sub_menu[TEXT_STYLE_MENU], NULL);
	}
	else if(streq(list[i], "Line Style")){
		xv_set(mi, MENU_PULLRIGHT, 
				sub_menu[LINE_STYLE_MENU], NULL);
	}
	else if(streq(list[i], "Plot Mode")){
		xv_set(mi, MENU_PULLRIGHT, 
				sub_menu[PLOT_MODE_MENU], NULL);
	}


	i++;
	}
/* add dismiss button for non-open look */

if(not_open_look){	

	mi = xv_create(NULL, MENUITEM,
		MENU_STRING, "",
		MENU_INACTIVE, TRUE,
		NULL);
	xv_set(menu[imenu], MENU_APPEND_ITEM, mi, NULL);

	mi = xv_create(NULL, MENUITEM,
		MENU_STRING, DISMISS,
		NULL);
	xv_set(menu[imenu], MENU_APPEND_ITEM, mi, NULL);

}

	if(imenu == 0){
		xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING,	title,
			XV_HELP_DATA,		helper(title),
			PANEL_ITEM_MENU,	menu[imenu],
			PANEL_NEXT_ROW,		-1,
			NULL);
	}
	else
		  xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING,	title,
			XV_HELP_DATA,		helper(title),
			PANEL_ITEM_MENU,	menu[imenu],
			NULL);




}

/* create the (so-far hidden) button for aliases */
void
make_alias_menu()
{
	alias_menu = xv_create(NULL, MENU,
				 MENU_GEN_PIN_WINDOW, frame, "Aliases",
				 MENU_NOTIFY_PROC, menu_proc, 
				 NULL);

	alias_button =  xv_create(panel[0], PANEL_BUTTON,
			PANEL_LABEL_STRING,	"Aliases",
			XV_HELP_DATA,		"robot:Aliases",
			PANEL_ITEM_MENU,	alias_menu,
			XV_SHOW, FALSE,
			NULL);
}

/* show the alias button */
void
salias_()
{
	xv_set(alias_button, XV_SHOW, TRUE, NULL);
/* Linux appears to loose the button name at times! Perhaps a 
 * problem with XView 3.2? */
	xv_set(alias_button, PANEL_LABEL_STRING, "Aliases", NULL);
}

/* hide the alias button */
void
halias_()
{
	xv_set(alias_button, XV_SHOW, FALSE, NULL);
}



/* new alias menu */
void
nalmen_()
{
	alias_number = -1;
	xv_set(alias_button, PANEL_ITEM_MENU, NULL, NULL);
	xv_destroy(alias_menu);
	alias_menu = xv_create(NULL, MENU,
				 MENU_GEN_PIN_WINDOW, frame, "Aliases",
				 MENU_NOTIFY_PROC, menu_proc, 
				 NULL);
}

/* attach alias menu to button */
void
atalmen_()
{
	xv_set(alias_button, PANEL_ITEM_MENU, alias_menu, NULL);
/* Linux appears to loose the button name at times! Perhaps a 
 * problem with XView 3.2? */
	xv_set(alias_button, PANEL_LABEL_STRING, "Aliases", NULL);
}


/* add alias to the Alias menu */
void
aalias2_(alias_name_in)
char	*alias_name_in;
{


	alias_number++;
	if(alias_number >= ALIASMAX) return;

	strcpy(alias_name[alias_number].text, alias_name_in);


	mi = xv_create(NULL, MENUITEM,
			    MENU_STRING, alias_name[alias_number].text,
			    NULL);


	xv_set(alias_menu, MENU_APPEND_ITEM, mi, NULL);







}



void
show_print()
/* Put up the command frame for controling printing  */
{
	xv_set(print_frame, XV_SHOW, TRUE,
			FRAME_LEFT_FOOTER, NULL,
			FRAME_CMD_PUSHPIN_IN, TRUE, 
			NULL);
}

void
print_now_call()
{
	if(xv_get(print_now, PANEL_VALUE) == 0){
		xv_set(plotfile_input, PANEL_INACTIVE, TRUE, NULL);
		xv_set(plotdir_input, PANEL_INACTIVE, TRUE, NULL);
		xv_set(postscript_printer, PANEL_INACTIVE, FALSE, NULL);
		xv_set(postscript_number, PANEL_INACTIVE, FALSE, NULL);
		}
	else{
		xv_set(plotfile_input, PANEL_INACTIVE, FALSE, NULL);
		xv_set(plotdir_input, PANEL_INACTIVE, FALSE, NULL);
		xv_set(postscript_printer, PANEL_INACTIVE, TRUE, NULL);
		xv_set(postscript_number, PANEL_INACTIVE, TRUE, NULL);
	}
}

void
psbusy_()
/* show that printing is in process */
{
	xv_set(print_frame,
		FRAME_BUSY, TRUE,
		NULL);

}

void
psfree_()
/* show that printing should have finished */
{
	xv_set(print_frame,
		FRAME_BUSY, FALSE,
		NULL);
}

void
printhow_()
/* Read instructions on how to do the printing from
 * the print frame and send them to the print procedures
 */
{
static char name[120];
int	direction, printwhen;
	strcpy(name, (char *)xv_get(plotdir_input, PANEL_VALUE));
	if(name[strlen(name)-1] != '/')
		strcat(name, "/");
	strcat(name, (char *) xv_get(plotfile_input, PANEL_VALUE));
	direction = xv_get(direction_choice, PANEL_VALUE);
	printwhen = xv_get(print_now, PANEL_VALUE);
	if(xv_get(ps_scale, PANEL_VALUE) == 0)
		ps_scale_set(TRUE);
	else
		ps_scale_set(FALSE);
/*
 * If EPS special is selected then the output bounding box is shrunk and
 * there's no "showpage" at the end of the file
 */
	if(xv_get(ps_type, PANEL_VALUE))
		eps_set();
	else
		eps_unset();
        ps_printer_set((char *)xv_get(postscript_printer, PANEL_VALUE));
	ps_number_set(xv_get(postscript_number, PANEL_VALUE));
	ps_print_set(name, direction, printwhen);
	ps_title_set((char *)xv_get(plotfile_input, PANEL_VALUE));
}

/* send print command to Robot */
void
do_print(item, event)
Panel_item	item;
Event	*event;
{
	strcpy(inst,"print");
	to_robot();
}





void
inactivate()
{
	switch_off("Plot Polynomial");
	switch_off("Plot Curve");
	switch_off("Plot Dashed Curve");
	switch_off("Plot Linear Fit");
	switch_off("Subtract Curve");
	switch_off("Add Curve");
	switch_off("Subtract Polynomial");
	switch_off("Select Window");
	switch_off("Curmodelplot");
	switch_off("Curmodelplot Dashed");
	switch_off("Titles");
	switch_off("Axes");
	switch_off("Label Axes");
	switch_off("Label X Axis");
	switch_off("Label Y Axis");
	switch_off("No Minor Tick Marks");
	switch_off("Linear Axes");
	switch_off("Linear X Axis");
	switch_off("Linear Y Axis");
	switch_off("Don't Plot Grid");
	switch_off("Infiletext");
/* switch off default text styles to show these are slected! */
	switch_off("Normal");
	switch_off("Times");
	switch_off("14 Point");
	switch_off("Line Width 0");
	switch_off("SolidLines");
/* sub-menu items */
	switch_off("ArrowLine");
	switch_off("SingleArrow");
}

void
switch_off(name)
	char            name[];
{
	int             i;

	for (i = 0; i < NMENUS; i++) {

		mi = xv_find(menu[i], MENUITEM,
			       MENU_STRING, name,
			       XV_AUTO_CREATE, FALSE,
			       NULL);

		if (mi != (NULL)) {
			xv_set(mi, MENU_INACTIVE, TRUE, NULL);
			return;
		}
	}
	for (i = 0; i < NSUBMENUS; i++) {

		mi = xv_find(sub_menu[i], MENUITEM,
			       MENU_STRING, name,
			       XV_AUTO_CREATE, FALSE,
			       NULL);

		if (mi != (NULL)) {
			xv_set(mi, MENU_INACTIVE, TRUE, NULL);
			return;
		}
	}
}






/* switch (mostly) on menu items depending on the input
 * instruction 
 * switch is called from inside robot to enable switch on/off
 * from both interactive and file input
 */

void
switcher_(inst2)
char	*inst2;
{
int	i;
char	instruction[ILENGTH];
	strncpy(instruction, inst2, ILENGTH-1);
/* remove trailing blanks */
	for (i = 0; i < strlen(instruction); i++){
		if(instruction[i] == ' ') 
			instruction[i] = (char) NULL;}
	activate(instruction);
}



void
activate(instruction)
char	*instruction;
{

	if(streq(instruction, "LINFIT") || streq(instruction, "YORK")) {
		switch_on("Plot Linear Fit");}
	else if(streq(instruction, "POLFIT")) {
		switch_on("Plot Polynomial");
		switch_on("Subtract Polynomial");}
	else if(streq(instruction, "CURFIT")){
		switch_on("Plot Curve");
		switch_on("Plot Dashed Curve");
		switch_on("Curmodelplot");
		switch_on("Curmodelplot Dashed");
		switch_on("Subtract Curve");
		switch_on("Add Curve"); }
	else if(streq(instruction, "DIVWINDOW")) {
		switch_on("Select Window"); }
	else if(streq(instruction, "INFILENOTEXT")){
		switch_off("Infilenotext");
		switch_on("Infiletext");}
	else if(streq(instruction, "INFILETEXT")){
		switch_on("Infilenotext");
		switch_off("Infiletext");}	
	else if(streq(instruction, "LABELAXES")){
		switch_off("Label Axes");
		switch_off("Label X Axis");
		switch_off("Label Y Axis");
		switch_on("Don't Label Axes");
		switch_on("Don't Label X Axis");
		switch_on("Don't Label Y Axis");}
	else if(streq(instruction, "NOLABELAXES")){
		switch_on("Label Axes");
		switch_on("Label X Axis");
		switch_on("Label Y Axis");
		switch_off("Don't Label Axes");
		switch_off("Don't Label X Axis");
		switch_off("Don't Label Y Axis");}
	else if(streq(instruction, "LABELXAXIS")){
		switch_off("Label X Axis");	
		switch_on("Don't Label X Axis");}	
	else if(streq(instruction, "NOLABELXAXIS")){
		switch_on("Label X Axis");	
		switch_off("Don't Label X Axis");}	
	else if(streq(instruction, "LABELYAXIS")){
		switch_off("Label Y Axis");	
		switch_on("Don't Label Y Axis");}	
	else if(streq(instruction, "NOLABELYAXIS")){
		switch_on("Label Y Axis");	
		switch_off("Don't Label Y Axis");}	
	else if(streq(instruction, "NOLABELAXES")){
		switch_off("Don't Label Axes");
		switch_on("Label Axes");
		switch_on("Label X Axis");
		switch_on("Label Y Axis"); }
	else if(streq(instruction, "NOTITLES")){
		switch_off("No Titles");
		switch_on("Titles"); }
	else if(streq(instruction, "TITLES")){
		switch_off("Titles");
		switch_on("No Titles"); }
	else if(streq(instruction, "AXES")){
		switch_off("Axes");
		switch_on("No Axes"); }
	else if(streq(instruction, "NOAXES")){
		switch_off("No Axes");
		switch_on("Axes"); }
	else if(streq(instruction, "MINORAXES")){
		switch_on("No Minor Tick Marks");
		switch_off("Minor Tick Marks"); }
	else if(streq(instruction, "NOMINORAXES")){
		switch_off("No Minor Tick Marks");
		switch_on("Minor Tick Marks"); }
	else if(streq(instruction, "LINEARAXES")){
		switch_off("Linear Axes");
		switch_off("Linear X Axis");
		switch_off("Linear Y Axis");
		switch_on("Log Y Axis");
		switch_on("Log X Axis");
		switch_on("Logarithmic Axes"); }
	else if(streq(instruction, "LOGAXES")){
		switch_on("Linear Axes");
		switch_on("Linear X Axis");
		switch_on("Linear Y Axis");
		switch_off("Log Y Axis");
		switch_off("Log X Axis");
		switch_off("Logarithmic Axes"); }
	else if(streq(instruction, "LOGXAXIS")){
		switch_on("Linear X Axis");
		switch_off("Log X Axis");}
	else if(streq(instruction, "LINEARXAXIS")){
		switch_off("Linear X Axis");
		switch_on("Log X Axis");}
	else if(streq(instruction, "NOPLOTGRID")){
		switch_off("Don't Plot Grid");
		switch_on("Plot Grid");}
	else if(streq(instruction, "PLOTGRID")){
		switch_on("Don't Plot Grid");
		switch_off("Plot Grid");}
	else if(streq(instruction, "LOGYAXIS")){
		switch_on("Linear Y Axis");
		switch_off("Log Y Axis");}
	else if(streq(instruction, "LINEARYAXIS")){
		switch_off("Linear Y Axis");
		switch_on("Log Y Axis");}
	else if(streq(instruction, "TWELVEPOINT")){
		switch_off("12 Point");
		switch_on("10 Point");
		switch_on("14 Point");
		switch_on("18 Point");}
	else if(streq(instruction, "FOURTEENPOINT")){
		switch_off("14 Point");
		switch_on("10 Point");
		switch_on("12 Point");
		switch_on("18 Point");}
	else if(streq(instruction, "EIGHTEENPOINT")){
		switch_off("18 Point");
		switch_on("10 Point");
		switch_on("12 Point");
		switch_on("14 Point");}
	else if(streq(instruction, "TENPOINT")){
		switch_off("10 Point");
		switch_on("12 Point");
		switch_on("14 Point");
		switch_on("18 Point");}
	else if(streq(instruction, "NORMAL")){
		switch_off("Normal");
		switch_on("Bold");
		switch_on("Italic");
		switch_on("Bold Italic");}
	else if(streq(instruction, "BOLD")){
		switch_off("Bold");
		switch_on("Normal");
		switch_on("Italic");
		switch_on("Bold Italic");}
	else if(streq(instruction, "ITALIC")){
		switch_off("Italic");
		switch_on("Normal");
		switch_on("Bold");
		switch_on("Bold Italic");}
	else if(streq(instruction, "BOLDITALIC") ||
		  streq(instruction, "BOLD_ITALIC")){
		switch_off("Bold Italic");
		switch_on("Normal");
		switch_on("Bold");
		switch_on("Italic");}
	else if(streq(instruction, "SYMBOL")){
		switch_fonts_on();
		switch_off("Symbol");}
	else if(streq(instruction, "TIMES")){
		switch_fonts_on();
		switch_off("Times");}
	else if(streq(instruction, "HELVETICA")){
		switch_fonts_on();
		switch_off("Helvetica");}
	else if(streq(instruction, "COURIER")){
		switch_fonts_on();
		switch_off("Courier");}
	else if(streq(instruction, "PALATINO")){
		switch_fonts_on();
		switch_off("Palatino");}
	else if(streq(instruction, "BOOKMAN")){
		switch_fonts_on();
		switch_off("Bookman");}
	else if(streq(instruction, "AVANTGARDE")){
		switch_fonts_on();
		switch_off("AvantGarde");}
	else if(streq(instruction, "NEWCENTURY")){
		switch_fonts_on();
		switch_off("NewCentury");}
	else if(streq(instruction, "HELVETICANARROW")){
		switch_fonts_on();
		switch_off("HelveticaNarrow");}
	else if(streq(instruction, "DINGBATS") ||
		  streq(instruction, "ZAPFDINGBATS")){
		switch_fonts_on();
		switch_off("Dingbats");}
	else if(streq(instruction, "CHANCERY")||
		  streq(instruction, "ZAPFCHANCERY")){
		switch_fonts_on();
		switch_off("Chancery");}
	else if(streq(instruction, "KANJI")){
		switch_fonts_on();
		switch_off("Kanji");}
	else if(streq(instruction, "WIDTH0")){
		switch_off("Line Width 0");
		switch_on("Line Width 1");
		switch_on("Line Width 2");
		switch_on("Line Width 4");
		switch_on("Line Width 8");}
	else if(streq(instruction, "WIDTH1")){
		switch_off("Line Width 1");
		switch_on("Line Width 0");
		switch_on("Line Width 2");
		switch_on("Line Width 4");
		switch_on("Line Width 8");}
	else if(streq(instruction, "WIDTH2")){
		switch_off("Line Width 2");
		switch_on("Line Width 0");
		switch_on("Line Width 1");
		switch_on("Line Width 4");
		switch_on("Line Width 8");}
	else if(streq(instruction, "WIDTH4")){
		switch_off("Line Width 4");
		switch_on("Line Width 0");
		switch_on("Line Width 1");
		switch_on("Line Width 2");
		switch_on("Line Width 8");}
	else if(streq(instruction, "WIDTH8")){
		switch_off("Line Width 8");
		switch_on("Line Width 0");
		switch_on("Line Width 1");
		switch_on("Line Width 2");
		switch_on("Line Width 4");}
	else if(streq(instruction, "SOLIDLINES")){
		switch_off("SolidLines");
		switch_on("DottedLines");
		switch_on("DashedLines");
		switch_on("DotDashedLines");}
	else if(streq(instruction, "DASHEDLINES")){
		switch_on("SolidLines");
		switch_on("DottedLines");
		switch_on("DotDashedLines");
		switch_off("DashedLines");}
	else if(streq(instruction, "DOTDASHEDLINES")){
		switch_on("SolidLines");
		switch_on("DottedLines");
		switch_on("DashedLines");
		switch_off("DotDashedLines");}
	else if(streq(instruction, "DOTTEDLINES")){
		switch_on("SolidLines");
		switch_off("DottedLines");
		switch_on("DashedLines");
		switch_on("DotDashedLines");}
	else if(streq(instruction, "SINGLEARROW")){
		switch_off("SingleArrow");
		switch_on("DoubleArrow");}
	else if(streq(instruction, "DOUBLEARROW")){
		switch_off("DoubleArrow");
		switch_on("SingleArrow");}
	else if(streq(instruction, "ARROWLINE")){
		switch_off("ArrowLine");
		switch_on("ArrowFill");
		switch_on("ArrowHollow");}
	else if(streq(instruction, "ARROWFILL")){
		switch_off("ArrowFill");
		switch_on("ArrowLine");
		switch_on("ArrowHollow");}
	else if(streq(instruction, "ARROWHOLLOW")){
		switch_off("ArrowHollow");
		switch_on("ArrowFill");
		switch_on("ArrowLine");}
/* slightly different - exclusive choice button */
	else if(streq(instruction, "MOUSE")){
			xv_set(mouser, PANEL_VALUE, 1, NULL);}
	else if(streq(instruction, "KEYBOARD")){
			xv_set(mouser, PANEL_VALUE, 0, NULL);}
/* Reset switches everything on then uses the same routine as at
 * initial start up to switch off selected items */
	else if(streq(instruction, "RESET")){
		switch_everything_on();
		axis_control_reset();
		inactivate();
	}
	else
/* an axis instruction? */
	axis_command_check(instruction);
}


void
switch_on(name)
char	name[];
{
	int	i;

	for (i = 0; i < NMENUS; i++) {
		mi = xv_find(menu[i], MENUITEM, 
			MENU_STRING,	name, 
			XV_AUTO_CREATE,	FALSE,
			NULL);
		if (mi != (NULL)) {
			xv_set(mi, MENU_INACTIVE, FALSE, NULL);
			return;
		}
	}
	for (i = 0; i < NSUBMENUS; i++) {
		mi = xv_find(sub_menu[i], MENUITEM, 
			MENU_STRING,	name, 
			XV_AUTO_CREATE,	FALSE,
			NULL);
		if (mi != (NULL)) {
			xv_set(mi, MENU_INACTIVE, FALSE, NULL);
			return;
		}
	}
}


/* switch on all the items contained in the list */

void
switch_everything_on()
{
	int             n, i, j;
	for (i = 0; i < NMENUS; i++) {
		n = xv_get(menu[i], MENU_NITEMS);
		for (j = n; j > 1; j--) {
			mi = xv_get(menu[i], MENU_NTH_ITEM, j);
				if (mi != (NULL)) {
				if(strne((char *) xv_get(mi, MENU_STRING), ""))
				  xv_set(mi, MENU_INACTIVE, FALSE, NULL);
			}
		}
	}
	for (i = 0; i < NSUBMENUS; i++) {
		n = xv_get(sub_menu[i], MENU_NITEMS);
		for (j = n; j > 1; j--) {
			mi = xv_get(sub_menu[i], MENU_NTH_ITEM, j);
			if (mi != (NULL)) {
				if (strne((char *) xv_get(mi, MENU_STRING), ""))
					xv_set(mi, MENU_INACTIVE, FALSE, NULL);
			}
		} 
	}
}



/* make all items in the FONT sub-menu active */
void
switch_fonts_on()
{
	int	n, j;
		n = xv_get(sub_menu[FONT_MENU], MENU_NITEMS);

		for (j = n; j > 1; j--) {
			mi = xv_get(sub_menu[FONT_MENU], MENU_NTH_ITEM, j);
				if (mi != (NULL)) {
				if(strne((char *)xv_get(mi, MENU_STRING), ""))
				  xv_set(mi, MENU_INACTIVE, FALSE, NULL);
			}
		}
	
}


void
totext2_(buffin)
/* send text to text window, rather than terminal */
char *buffin;
{	
static char caret[2] ="\012"; /* without "static" got funnies with get_text */

	xv_set(text_window, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, NULL);
	textsw_insert(text_window, buffin, non_z_length(buffin));
	textsw_insert(text_window, caret, 1);
	textsw_possibly_normalize(text_window,
		(Textsw_index) xv_get(text_window, TEXTSW_INSERTION_POINT));
	showit();
}






int
robot_error(object, avlist)
Xv_object	object;
Attr_avlist	avlist;
{
Attr_avlist     attrs;
Error_severity severity = ERROR_RECOVERABLE;
	char buferr[5];

    for (attrs = avlist; *attrs; attrs = attr_next(attrs)) {
	switch ((int) attrs[0]) {
	  case ERROR_SEVERITY:
		severity = attrs[1];
	    break;
        }
	}
	
	if(severity == ERROR_RECOVERABLE)
		printf("Recoverable error\n");
	else
		printf("Unrecoverable error\n");

	printf("%s\nDump core? (y/n) ", xv_error_format(object, avlist)); 
	fflush(stdout);
	if (gets(buferr) && (buferr[0] == 'y' || buferr[0] == 'Y'))
		abort();
	return XV_OK;
}

/* forced abort of program */
void
rabort_()
{
		abort();
}



int
robot_x_error(dpyin, event)
Display	*dpyin;
XErrorEvent	*event;
{
	char buferr[5];
	printf("WARNING - X ERROR DETECTED\n");
	printf("Abort? (y/n) ");
	fflush(stdout);
	if (gets(buferr) && (buferr[0] == 'y' || buferr[0] == 'Y'))
		abort();
	return XV_OK;
}

void
ad_make()
{
void	run_ad();
int	iloop;
static char    *panel_names[] = {
		"Minimum X", "Maximum X", "Minimum Y", "Maximum Y",
		"Minimum Grade", "Maximum Grade", 
"Split Event Threshold",};
static	int max_values[NO_AD_PANELS] = {10000, 10000, 10000, 10000, 3, 3, 4096};


	ad_frame = xv_create(frame, FRAME_CMD,
				FRAME_LABEL, "FITS Binary Table Reader",
				FRAME_SHOW_FOOTER,	TRUE,
				FRAME_CMD_PUSHPIN_IN, FALSE,
				NULL);
	ad_panel = (Panel) xv_get(ad_frame, FRAME_CMD_PANEL);
	xv_set(ad_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	for(iloop = 0; iloop < NO_AD_PANELS; iloop++){


 		ad_sub_panel[iloop] = xv_create(ad_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, panel_names[iloop],
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MAX_VALUE,	max_values[iloop],
				PANEL_VALUE_X, 200,
				NULL);
	}

	ad_name = xv_create(ad_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "FITS file name",
			PANEL_VALUE_DISPLAY_LENGTH, 30,
			PANEL_NOTIFY_PROC,	run_ad,
			NULL);

	xv_set(ad_panel, PANEL_DEFAULT_ITEM,
		xv_create(ad_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Read File",
			PANEL_NOTIFY_PROC,	run_ad,
			NULL),
		NULL);

if(not_open_look){
	
	   xv_set(ad_panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	   xv_create(ad_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	window_fit(ad_panel);
	window_fit(ad_frame);


}

void
show_ad()
/* Put up the command frame for setting Astro D file reading parameters  */
{
int	iloop, value;
char	name[61];
	xv_set(ad_frame, XV_SHOW, TRUE, NULL);
	xv_set(ad_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
	for(iloop = 0; iloop < NO_AD_PANELS; iloop++){

		fadget_(&iloop, &value);
		xv_set(ad_sub_panel[iloop],
			PANEL_VALUE, value,
			NULL);
	}
	fdnamg_(name);
	xv_set(ad_name, PANEL_VALUE, name, NULL);

}

void
run_ad()
/* get panel values and write to a FORTRAN common block
 * then run the FITS 3D reader 
 * Also puts instructions into the ROBOTLOG file */
{
int	iloop;
int	value;
int	n;
int	values[1];
void	fadset_();
static	char	label[] = "FITS3DRESTRICT";
		n = 1;
		for(iloop = 0; iloop < NO_AD_PANELS; iloop++){
			value = xv_get(ad_sub_panel[iloop],
				PANEL_VALUE);
			fadset_(&iloop, &value);
			saveinst_(label);
			values[0] = iloop;
			fiout_(values, &n);
			values[0] = value;
			fiout_(values, &n);
			
			
		}
/* and send the file name */
		if(non_z_length(xv_get(ad_name, PANEL_VALUE)) > 0){
		   fdnam_(xv_get(ad_name, PANEL_VALUE));
		   saveinst_(label);
		   values[0] = 7;
		   fiout_(values, &n);
		   savdata_(xv_get(ad_name, PANEL_VALUE));


		   strcpy(inst,"FITS3D");
		   to_robot();
		}
		else{
		   toarkt_("Error: no file name specified");
		}


}


void
no_consume()
{
int	i;


	for(i = 0; i < NO_PANELS; i++)
	 xv_set(panel[i],
	   WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
	 NULL);


/* disable pinned menus */
	for (i = 0; i < NMENUS; i++) {
		subframe = xv_get(menu[i], MENU_PIN_WINDOW);
		if (subframe != NULL) {
			subpanel = xv_get(subframe, FRAME_CMD_PANEL);
			if (subpanel != NULL) {
				xv_set(subpanel,
				       WIN_IGNORE_X_EVENT_MASK,
				       ButtonPressMask | ButtonReleaseMask,
				       NULL);
			}
		}
	}
	for(i = 0; i < NSUBMENUS; i++){
		subframe = xv_get(sub_menu[i], MENU_PIN_WINDOW);
		if (subframe != NULL) {
			subpanel = xv_get(subframe, FRAME_CMD_PANEL);
			if (subpanel != NULL) {
				xv_set(subpanel,
				       WIN_IGNORE_X_EVENT_MASK,
				       ButtonPressMask | ButtonReleaseMask,
				       NULL);
			}
		}
	}

	demo_canvas_off();

	xv_set(print_panel,
	WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);
	xv_set(colour_panel,
	WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);
	xv_set(history_panel,
	   WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);
	xv_set(axes_panel,
	   WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);
	xv_set(zoom_panel,
	   WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);
	xv_set(split_panel,
	   WIN_IGNORE_X_EVENT_MASK, ButtonPressMask | ButtonReleaseMask,
			NULL);

	xv_set(ad_panel,
	WIN_CONSUME_EVENTS,
			WIN_NO_EVENTS, WIN_REPAINT, NULL,
			NULL);

	xv_set(text_window,
	WIN_CONSUME_EVENTS,
		WIN_NO_EVENTS, NULL,
		NULL); 

}

void
do_consume()
{
int	i;


	for(i = 0; i < NO_PANELS; i++)
	 xv_set(panel[i],
		WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, 
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(text_input,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(text_window,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(print_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, 
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(colour_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(history_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(axes_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(zoom_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(split_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
	xv_set(ad_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);

	for (i = 0; i < NO_PANELS; i++)
	   xv_set(panel[i],
		WIN_CONSUME_X_EVENT_MASK, ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);

/* disable pinned menus */
	for (i = 0; i < NMENUS; i++) {
		subframe = xv_get(menu[i], MENU_PIN_WINDOW);
		if (subframe != NULL) {
			subpanel = xv_get(subframe, FRAME_CMD_PANEL);
			if (subpanel != NULL) {
				xv_set(subpanel,
				       WIN_CONSUME_X_EVENT_MASK,
 ExposureMask | KeyPressMask | VisibilityChangeMask,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
				       NULL);
			}
		}
	}
	for(i = 0; i < NSUBMENUS; i++) {
		subframe = xv_get(sub_menu[i], MENU_PIN_WINDOW);
		if (subframe != NULL) {
			subpanel = xv_get(subframe, FRAME_CMD_PANEL);
			if (subpanel != NULL) {
				xv_set(subpanel,
				       WIN_CONSUME_X_EVENT_MASK,
 ExposureMask | KeyPressMask | VisibilityChangeMask,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, LOC_DRAG,
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
				       NULL);
			}
		}
	}

	demo_canvas_on();

	xv_set(print_panel,
		WIN_CONSUME_X_EVENT_MASK, ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);
	xv_set(colour_panel,
		WIN_CONSUME_X_EVENT_MASK, ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);
	xv_set(history_panel,
		WIN_CONSUME_X_EVENT_MASK, ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);
	xv_set(ad_panel,
		WIN_CONSUME_X_EVENT_MASK, ExposureMask | KeyPressMask | VisibilityChangeMask,
		NULL);
}

/* should this routine be here? It returns the PID to a FORTRAN
routine */
	int fpid_()
{
	return (getpid());
}


void
cd_(dir, n)
char	*dir;
int	*n;
{
static char message1[] = "ERROR: Changing to directory";
static char message2[] = "Changed directory";
char	temp[ILENGTH];
char	direc[ILENGTH];
char	*d;


	strncpy(direc, dir, *n);
	direc[*n] = NULL;

/* null directory path is assumed to be cd to home */

	if(strlen(direc) <= 0){
		strcpy(direc, getenv("HOME"));
	}


/* "Home" directory? */
	if(direc[0] == '~'){
		strcpy(temp, (char *) getenv("HOME"));
		strcat(temp, "/");
		strcat(temp, direc+1);
		strcpy(direc, temp);
	}

/* change directory */
	if(chdir(direc)){
		strcpy(temp, message1);
		totext2_(temp);
		if(strlen(direc) > 0) totext2_(direc);
	}
	else{
		strcpy(temp, message2);
		totext2_(temp);	
		xv_set(frame, FRAME_LEFT_FOOTER,
			(char *)getwd(pathname),
			NULL);
	}
}



/*
 * double_click --check for double click with strings the same 
 */
int
double_click(last_string, then, this_string, now)
        char       *last_string;
        struct timeval  *then;
        char       *this_string;
        struct timeval  *now;
{
        struct timeval  delta;
	if strne(this_string, last_string)
                return 0;
        delta.tv_sec = now->tv_sec - then->tv_sec;
        if ((delta.tv_usec = now->tv_usec - then->tv_usec) < 0) {
                delta.tv_usec += 1000000;
                delta.tv_sec -= 1;
        }
        return (delta.tv_sec*10 + delta.tv_usec/100000) <= Robot.multiclick;
}



Notify_value
destroy_func(client, status)
/* prevent a quit from the frame shutting us down without
 * giving a chance to the user to save the log file
 * 
 * However, if "Quit" is called from an old plotting area we
 * simply destroy that window.
 */
Notify_client	client;
Destroy_status	status;
{
/* bring up the finish panel */
	if(client == canvas_frame || client == frame){
		finish();
		notify_veto_destroy(client);
		return NOTIFY_DONE;
	}
	else
		return notify_next_destroy_func(client, status);
	
}
	



void
finish()
/* called by the "end" button 
 * allows disposal of log/info files as required then halts
 */
{
char	log_file_out[61];
char	info_file_out[61];
char	*dirpointer;
	strcpy(log_file_out, "robotlog.rob");
	strcpy(info_file_out, "robot.inf");


/*
 * Start in the current (PWD) directory
 */


	xv_set(finish_frame, XV_SHOW, TRUE, NULL);
	xv_set(finish_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
	dirpointer = (char *)getwd(pathname);
	if(dirpointer == NULL)
		dirpointer = (char *)getenv("HOME");
	xv_set(finish_dir_name, PANEL_VALUE, 
		dirpointer, NULL);
	xv_set(finish_log_name, PANEL_VALUE, log_file_out, NULL);
	xv_set(finish_info_name, PANEL_VALUE, info_file_out, NULL);


}

void
save_quit()
/* Moves Robot log and info files into user specified names
 * and halts the program */
{

/* Do FORTRAN shutdowns required */
	strcpy(inst, "END!");
	robotf_(inst); 

	liflsh_();

	getfl_(log_file);
	getfi_(info_file);

	strcpy(directory, (char *) xv_get(finish_dir_name, PANEL_VALUE));
	if(directory[strlen(directory)-1] != '/')
		strcat(directory, "/");
	sprintf(sys_buffer, "%s %s %s", MV, log_file,
		strcat(directory,
		       (char *) xv_get(finish_log_name, PANEL_VALUE)));

	system(sys_buffer);


	strcpy(directory, (char *) xv_get(finish_dir_name, PANEL_VALUE));
	if(directory[strlen(directory)-1] != '/')
		strcat(directory, "/");
	sprintf(sys_buffer, "%s %s %s", MV, info_file,
		strcat(directory,
		(char *) xv_get(finish_info_name, PANEL_VALUE)));

	system(sys_buffer);

	finish_hide();
	fstop_();
}


void
save_continue()
/* Copies Robot log and info files into user specified names
 * and resumes the program */
{

	liflsh_();

	getfl_(log_file);
	getfi_(info_file);
	


	strcpy(directory, (char *) xv_get(finish_dir_name, PANEL_VALUE));
	if(directory[strlen(directory)-1] != '/')
		strcat(directory, "/");
	sprintf(sys_buffer, "%s %s %s", CP, log_file,
		strcat(directory,
		      (char *) xv_get(finish_log_name, PANEL_VALUE)));


	system(sys_buffer);

	strcpy(directory, (char *) xv_get(finish_dir_name, PANEL_VALUE));
	if(directory[strlen(directory)-1] != '/')
		strcat(directory, "/");
	sprintf(sys_buffer, "%s %s %s", CP, info_file,
		strcat(directory,
		(char *)xv_get(finish_info_name, PANEL_VALUE)));


	system(sys_buffer);

	finish_hide();
}

void
delete_quit()
/* Deletes Robot log and info files
 * then halts the program */
{
/* Do FORTRAN shutdowns required */
	strcpy(inst, "END!");
	robotf_(inst); 

	getfl_(log_file);
	getfi_(info_file);


	if(unlink(log_file))
		fprintf(stderr, "ERROR: problem deleting log file\n");
	if(unlink(info_file))
		fprintf(stderr, "ERROR: problem deleting info file\n");
	

	finish_hide();
	fstop_();
}

void
clear_continue()
/* Deletes Robot log and info files, opens new ones,
 * then continues with the program */
{
	getfl_(log_file);
	getfi_(info_file);


	/* do a FORTRAN close on the files first */
	clf_();
	/* now remove the files */
	if(unlink(log_file))
		fprintf(stderr, "ERROR: problem deleting log file\n");
	if(unlink(info_file))
		fprintf(stderr, "ERROR: problem deleting info file\n");

/* new log files via FORTRAN routine*/

	olf_();

	totext2_("CURRENT LOG/INFO FILES DELETED");
	totext2_("NEW LOG/INFO FILES CREATED");
	

	finish_hide();
}


void
finish_hide()
{
	xv_set(finish_frame, XV_SHOW, FALSE,
	                     FRAME_CMD_PUSHPIN_IN, FALSE,
			     NULL);
}


/* routine for assigning name to refer to in robot.info file */

char
*helper(name)
char	*name;
{
static char	default_name[] = "robot:Default";
	help_number++;
	if(help_number >= HELPMAX){
/* I'm too lazy to write a routine to do a malloc etc. */
		fprintf(stderr, "Error in helper, n = %d\n", help_number);
		return(default_name);
	}
	strcpy(help_name[help_number].text, "robot:");
	strcat(help_name[help_number].text, name);
	return(help_name[help_number].text);
}


/* routines for grabbing text from within robot 
 * note that these start and then stop the notifier in a similar
 * way to the main loop
 */

char buffreply1[ILENGTH];
int	got_text;
void
gettextxv_(buffreply)
char *buffreply;
{
int	nrows, i;
Bool	display_list;

	display_list = FALSE;

/*	printf("preset instruction is %s\n", inst);  */
/* remove old inserts from list */
	nrows = xv_get(select_list, PANEL_LIST_NROWS);
/*	for(i = nrows-1; i >= 0; i--)
		xv_set(select_list, PANEL_LIST_DELETE, i, NULL); */
	if(nrows > 0)
		xv_set(select_list, PANEL_LIST_DELETE_ROWS, 0, nrows, 
			NULL);
/* test adding some items */
	i = 0;
	get_it_time++;
	display_list = TRUE;
	if(streqn(inst, "PLOTMODE") && get_it_time == 1){
	xv_set(select_list, PANEL_LIST_STRING, i, "Bars", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Bars2", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Boxes", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "FilledBoxes", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Crosses", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Diamonds", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Ellipses", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Lines", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Dashedlines", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Histogram", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Filled-histogram", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Fill", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Bar-Graph", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Stacked-Bar", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Symbol", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Nice-symbol", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Pillar", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Spline", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "NoData", NULL); i++;
	}
	else if(streqn(inst, "XFUNCTION")
		|| streqn(inst, "YFUNCTION") 
		|| streqn(inst, "ZFUNCTION")){
	xv_set(select_list, PANEL_LIST_STRING, i, "Sqrt", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Square", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Cubert", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Cube", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Inverse", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Factorial", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Sine", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Cos", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Tan", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ASine", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ATan", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ACos", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Log", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "LogE", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Abs", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Exp", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Exp10", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Sinh", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Cosh", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Tanh", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "J0", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "J1", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Round", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Ceil", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Floor", NULL); i++;
	}
	else if((streqn(inst, "XARITH")
		|| streqn(inst, "YARITH")
		|| streqn(inst, "ZARITH") 
		|| streqn(inst, "XERRARITH") 
		|| streqn(inst, "YERRARITH")) 
			&& (get_it_time == 1)){
	xv_set(select_list, PANEL_LIST_STRING, i, "+", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "-", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "*", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "/", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "^", NULL); i++;
	}
	else if(streqn(inst, "CURFIT") && 
		(get_it_time - (get_it_time/2)*2 == 1)){
	xv_set(select_list, PANEL_LIST_STRING, i, "Sine", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Polynomial", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Gauss", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Lorentz", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Orbit", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Powerlaw", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Blackbody", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Triangle", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Tophat", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "User", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "END", NULL); i++;
	}
	else if(streqn(inst, "OTHERTITLES") &&
		(get_it_time == 1)){
	xv_set(select_list, PANEL_LIST_STRING, i, "Title2", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Title3", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Title4", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "XLabel2", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "XLabel3", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "XLabel4", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "YLabel2", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ZLabel", NULL); i++;
	}
	else if(prompt_menu == 1){
	xv_set(select_list, PANEL_LIST_STRING, i, "Box", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Filled-Box", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Circle", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Filled-Circle", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Diamond", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Filled-Diamond", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Triangle", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Filled-Triangle", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Cross", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Star", NULL); i++;
	}
	else if(streqn(inst, "FILLSTYLE") && get_it_time == 1){
	xv_set(select_list, PANEL_LIST_STRING, i, "Zero", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "PlotMinimum", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "PlotMaximum", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "DataMinimum", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "DataMaximum", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Specify", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Join", NULL); i++;
	}
	else if(streqn(inst, "FONT")){
	xv_set(select_list, PANEL_LIST_STRING, i, "Times", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Helvetica", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Courier", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "AvantGarde", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Bookman", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Helvetica Narrow", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "NewCentury", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Palatino", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Symbol", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ZapfChancery", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "ZapfDingbats", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Kanji", NULL); i++;
	}
	else if(streqn(inst, "TEXTSTYLE")){
	xv_set(select_list, PANEL_LIST_STRING, i, "Normal", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Bold", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Italic", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "Bold_Italic", NULL); i++;
	}
	else if(streqn(inst, "LINESTYLE")){
	xv_set(select_list, PANEL_LIST_STRING, i, "SolidLines", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "DashedLines", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "DottedLines", NULL); i++;
	xv_set(select_list, PANEL_LIST_STRING, i, "DotDashedLines", NULL); i++;
	}
	else
		display_list = FALSE;
	

	if((streqn(inst, "FILE") ||
	   streqn(inst, "PLOTFILE") ||
	   streqn(inst, "DATAFILE"))
		&& I_hate_typing)
		dir_show();
	else if(display_list && I_hate_typing)
	{
		xv_set(select_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
		xv_set(answer_select_frame, 
			XV_SHOW,	TRUE,
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_LABEL,	inst,
			FRAME_BUSY,	FALSE,
			NULL);
	}
	else{
		xv_set(get_text_message[0], PANEL_LABEL_STRING, prompt0, NULL);
		xv_set(get_text_message[1], PANEL_LABEL_STRING, prompt1, NULL);
		xv_set(get_text_message[2], PANEL_LABEL_STRING, prompt2, NULL);
		xv_set(get_text_message[3], PANEL_LABEL_STRING, prompt3, NULL);
		xv_set(get_text_frame, XV_SHOW, TRUE, NULL);
		xv_set(get_text_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		xv_set(get_text_string, PANEL_VALUE, "", NULL);
		xv_set(get_text_frame, FRAME_LABEL, inst, NULL);
		/* do we need special character support? */
		if(streqn(inst, "TEXT") ||
			streqn(inst, "TITLE") ||
			streqn(inst, "YLABEL") ||
			streqn(inst, "XLABEL")){
			xv_set(specials_button, XV_SHOW, TRUE, NULL);
			window_fit(get_text_panel);
			window_fit(get_text_frame);
			test_specials = TRUE;
		        if(want_specials) show_specials();
			
		}
	        else{
			test_specials = FALSE;
		}

	}
	gshow_();
	got_text = FALSE;
	while (got_text != TRUE)
		{
		notify_start();
		gshow_();
		if(got_text == TRUE)
		{
	strcpy(buffreply, buffreply1);
	repaint_panels();
	gshow_();
	notify_dispatch();
	notify_dispatch();
	gshow_();
	return;
		}
		}
}

void
preset_()
{
	nprompt = 0;
	prompt_menu = 0;
	strcpy(prompt0, DEFAULT_PROMPT);
	strcpy(prompt1, DEFAULT_PROMPT);
	strcpy(prompt2, DEFAULT_PROMPT);
	strcpy(prompt3, DEFAULT_PROMPT);
}


/* assign a number to indicate which menu to display,
 * this relies on the routine setting the menu knowing the
 * right ID to use */
void
smenu_(id)
int	*id;
{
	prompt_menu = *id;
}

void
sprompt2_(buffer)
char	*buffer;
{
	if(nprompt == 0)
		strncpy(prompt0, buffer, MIN(non_z_length(buffer)+1, ILENGTH-1));
	else if(nprompt == 1)
		strncpy(prompt1, buffer, MIN(non_z_length(buffer)+1, ILENGTH-1));
	else if(nprompt == 2)
		strncpy(prompt2, buffer, MIN(non_z_length(buffer)+1, ILENGTH-1));
	else if(nprompt == 3)
		strncpy(prompt3, buffer, MIN(non_z_length(buffer)+1, ILENGTH-1));
	nprompt++;
}

void
grab_text()
{
	strcpy(buffreply1, (char *) xv_get(get_text_string, PANEL_VALUE));
	xv_set(get_text_frame, XV_SHOW, FALSE, NULL);
	xv_set(get_text_frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(get_text_frame, XV_SHOW, FALSE, NULL);
/* make sure we get rid of the special character frame */
	if(test_specials){
	  if(specials_visible()){
		want_specials = TRUE;
	  }
	  else{
		want_specials = FALSE;
	  }
	}
	hide_specials();
	xv_set(specials_button, XV_SHOW, FALSE, NULL);
	got_text = TRUE;
	gshow_();
	notify_stop();
	gshow_();
	
}


/* add a "special" character string to existing text */
void
modify_text(temp_text)
char	*temp_text;
{
	strcpy(buffreply1, (char *) xv_get(get_text_string, PANEL_VALUE));
	strcat(buffreply1, temp_text);
	xv_set(get_text_string,
			PANEL_VALUE, buffreply1,
			NULL);
}
	
void
answer_ok()
{

/* find selected row */
/* for xview v3.0 */
	strcpy(buffreply1, (char *) xv_get(select_list, PANEL_LIST_STRING,
			   xv_get(select_list, PANEL_LIST_FIRST_SELECTED))); 
	xv_set(answer_select_frame, FRAME_BUSY, TRUE, NULL);
	xv_set(answer_select_frame, XV_SHOW, FALSE, NULL);
	xv_set(answer_select_frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(answer_select_frame, XV_SHOW, FALSE, NULL);
	got_text = TRUE;
	gshow_();
	notify_stop();
	gshow_();
}

/* call answer OK if we get a double click */
void
answer_click(item, string, client_data, op, event)
Panel_item	item;
char		*string;
Xv_opaque	client_data;
Panel_list_op	op;
Event		*event;
{
	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};

	if(op == PANEL_LIST_OP_SELECT){
	strcpy(buffreply1, string);
	now = event_time(event);
	if(double_click(last_string, &then, string, &now))
		answer_ok();
	strcpy(last_string, string);
	then = now;
	}
}



/* called by the directory browser code */
/* a1 is the directory, a2 is the file */
void
dir_finish(a1, a2)
char	*a1, *a2;
{
	strcpy(buffreply1, a1);
/* check if we have a / at the end */
	if(buffreply1[strlen(buffreply1)-1] != '/')
		strcat(buffreply1, "/");
	strcat(buffreply1, a2);
	got_text = TRUE;
	gshow_();
	notify_stop();
	gshow_();
}


/* a kludge to cope with stupid DECstations which don't
 * like XUniqueContext !

#include <X11/Xlib.h>
#undef XUniqueContext

int XUniqueContext()  ultrix declares XUniqueContext() to be  an int, 
	                 not an XContext (which is an int, anyway) 
{
      return((int)XrmUniqueQuark());
}
*/

/* definitely not to be called from something directly called
 * by the notifier!!! */
void
update_()
{
	notify_dispatch();
	gshow_();
}


void
orient()
{
	landscape = !landscape;
	if(landscape){
		width = MY_CANVAS_HEIGHT;
		height = MY_CANVAS_WIDTH;
		xv_set(direction_choice, PANEL_VALUE, 1, NULL);
	}
	else{
		width = MY_CANVAS_WIDTH;
		height = MY_CANVAS_HEIGHT;
		xv_set(direction_choice, PANEL_VALUE, 0, NULL);
	}
	/* xv_set(canvas_frame,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL); */
	xv_set(canvas,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
	window_fit(canvas_frame);
/* little window showing window gaps etc. */
	flip_demo();
	demo_gaps();
}


void
null_redraw(canvas_in, paint_window_in, repaint_area_in)
Canvas		canvas_in;
Xv_window	paint_window_in;
Rectlist	*repaint_area_in;

{
int	itemp;
		sscanf((char *)xv_get(paint_window_in, 
			XV_KEY_DATA, OLD_PLOT_KEY),
				"%d", &itemp);

		ark_redrawn(itemp);
}


void
newpa_()
{
char	old_label[ILENGTH];
char	number[10];
char	*cdata;

	sprintf(old_label, "Robot plot area %s",
		xv_get(canvas_paint_window(ark_current_canvas()), XV_KEY_DATA, OLD_PLOT_KEY));

	no_of_canvases++;
	sprintf(number, "%d", no_of_canvases);
	cdata = malloc(strlen(number)+1);
	strcpy(cdata, number);


	xv_set(canvas, XV_HELP_DATA, "robot:old_canvas",
			CANVAS_REPAINT_PROC, null_redraw,
/* if we install a repaint on old images remove the line below 
		        CANVAS_FIXED_IMAGE, TRUE, */
			NULL);

	xv_set(canvas_paint_window(canvas), 
		 WIN_EVENT_PROC, null_event_proc,
		 NULL);



	old_canvas_frame = canvas_frame;
 	xv_set(old_canvas_frame,
		FRAME_LABEL, old_label,
		FRAME_SHOW_FOOTER, FALSE,
		NULL);
	xv_set(xv_get(old_canvas_frame, FRAME_ICON),
			ICON_IMAGE, old_plot_image, NULL);




	canvas_frame = xv_create(NULL, FRAME,
					FRAME_LABEL, "Current Robot plot area",
				  	XV_WIDTH, width,
				  	XV_HEIGHT, height,
				  	FRAME_SHOW_FOOTER, TRUE,
			          	FRAME_ICON,
		(Icon)xv_create(XV_NULL, ICON,
			ICON_IMAGE,	plot_image,
			ICON_TRANSPARENT,	TRUE,
			XV_X,	100,
			XV_Y,	100,
			NULL), 
					NULL);
					

	canvas = (Canvas) xv_create(canvas_frame, CANVAS,
				  XV_HELP_DATA, "robot:canvas",
				  WIN_BIT_GRAVITY, NorthGravity,
				  CANVAS_FIXED_IMAGE, FALSE,
				  NULL);
	xv_set(canvas_paint_window(canvas), 
		 WIN_EVENT_PROC, canvas_event_proc,
		 WIN_CONSUME_EVENTS, LOC_DRAG, NULL,
		 XV_KEY_DATA, OLD_PLOT_KEY, cdata,
		 NULL);
	notify_interpose_destroy_func(canvas_frame, destroy_func);



	pltopn(canvas_frame, canvas);
	xv_set(canvas_frame, XV_SHOW, TRUE, NULL);

}


/* do nothing event proc for when we're using the cursor for
 * something else */
void
null_event_proc(window, event)
Xv_Window       window;
Event          *event;
{

			
	switch (event_action(event)) {
		case ACTION_SELECT:
			break;
		case ACTION_MENU:
			old_window = window;
			sscanf((char *)xv_get(window, 
			   XV_KEY_DATA, OLD_PLOT_KEY),
				"%d", &old_window_id);
			if(check_press())
			  show_old_canvas_menu(window, event,
				xv_get(window, XV_KEY_DATA, OLD_PLOT_KEY));
			break;
		default:
			return;
	}
}

/* Position the plot window at a specified place */
void
posplot_(x, y)
float	*x, *y;
{
Rect	rect;
	frame_get_rect(canvas_frame, &rect);
	rect.r_left = (int) *x;
	rect.r_top = (int) *y;
	frame_set_rect(canvas_frame, &rect);
}



void
kill_old_plot()
{

	sprintf(sys_buffer, "Really destroy plot number %d?",
				old_window_id);
	if(notice_prompt(frame, NULL,
			NOTICE_MESSAGE_STRINGS, 
				sys_buffer, NULL,
			NOTICE_BUTTON, "Yes", 1,
			NOTICE_BUTTON, "No", 0,
			NULL) == 0) return;

	ark_kill_plot(old_window_id);

}

/* make an old plot area the current plot area */
void
make_old_current()
{
char	old_label[ILENGTH];

	sprintf(old_label, "Robot plot area %s",
		xv_get(canvas_paint_window(ark_current_canvas()), XV_KEY_DATA, OLD_PLOT_KEY));

/* set current item to have old properties */

	xv_set(canvas_frame,
		FRAME_LABEL, old_label,
		NULL);

	xv_set(canvas_paint_window(ark_current_canvas()),
		WIN_EVENT_PROC, null_event_proc,
		NULL);

	xv_set(xv_get(canvas_frame, FRAME_ICON),
			ICON_IMAGE, old_plot_image, NULL);

/* make the old one current and set properties */


	canvas_frame = ark_old_to_current(old_window_id),

	xv_set(canvas_frame,
		FRAME_LABEL, "Current Plot Area",
		NULL);

	canvas = ark_current_canvas();

	xv_set(canvas_paint_window(ark_current_canvas()),
		WIN_EVENT_PROC, canvas_event_proc,
		NULL);

	xv_set(xv_get(canvas_frame, FRAME_ICON),
			ICON_IMAGE, plot_image, NULL);
/* bring canvas to front */
	xv_set(canvas_frame, XV_SHOW, TRUE, NULL);

/* determine whether this is a landscape or a portrait plot and
 * set the print panel accordingly */
	if(xv_get(canvas_frame, XV_WIDTH) > 
		    xv_get(canvas_frame, XV_HEIGHT)){
		landscape = TRUE;
	}
	else{
		landscape = FALSE;
	}

	if(landscape){
		xv_set(direction_choice, PANEL_VALUE, 1, NULL);
	}
	else{
		xv_set(direction_choice, PANEL_VALUE, 0, NULL);
	}
/* and the little window if needed */
	flip_demo();
	demo_gaps();


}


void
last_to_current()
{
	old_window_id = no_of_canvases;
	make_old_current();
}


void
print_old_plot()
{
	totext2_("Printing..");
	ark_printn(old_window_id);
	totext2_("Printed");
}

/* wipe_out(xp, yp, is)
float	xp, yp;
int	is;
{
	gcxor_();
	haltps_();
	printf("wiping at %.1f %.1f\n", xp, yp);
	symbol_(&xp, &yp, &is);
	startps_();
	gccopy_();
} */

void
canvas_event_proc(window, event)
	Xv_Window       window;
	Event          *event;
{
	static char     last_string[ILENGTH] = "";
	static char     coord_buff[ILENGTH] = "";
	static struct timeval then = {0, 0};
	static struct timeval now = {0, 0};

	int             ix, iy;
	static Bool     found_text = False;
	char            text[ILENGTH];
	float           xfound, yfound;
/*	static float    oldxfound, oldyfound;
	static int      symbol = 10; */

	int	islow, ifast, itext, itext_fast;

	switch (event_action(event)) {
	case MS_LEFT:
		break;
	case ACTION_MENU:
		if(check_press())
			show_canvas_menu(window, event);
		break;
	case LOC_DRAG:
/* to try to stop pop-ups when dragging */
		sprintf(last_string, "\\\\\\\\\\\\\\\\\\");
	case ACTION_SELECT:
		ix = event_x(event);
		iy = event_y(event);
		now = event_time(event);
		found_text = False;
		find_text(ix, iy, &found_text, text, &xfound, &yfound,
			   &islow, &ifast, &itext, &itext_fast);
		sprintf(coord_buff, "x = %g y = %g",
					xfound, yfound);
		xv_set(canvas_frame,
			FRAME_RIGHT_FOOTER, coord_buff,
			NULL);
		if (found_text) {
			xv_set(canvas_frame,
			FRAME_LEFT_FOOTER, text,
			NULL);
		/* either update the values if the window is already
		 * displayed - or pop-up the window if we get a double click
		 */
			if(text_editor_update() ||
			    double_click(last_string, &then,
					 text, &now)) {
				show_text_editor(islow, ifast, 
					itext, itext_fast, 
					xfound, yfound,
					text);
				xv_set(canvas_frame,
					FRAME_LEFT_FOOTER, "",
					NULL);
			}
			strcpy(last_string, text);
			then = now;
		}
		else {
			xv_set(canvas_frame,
			FRAME_LEFT_FOOTER, "",
			NULL);
		}
		break;
	default:
		return;
	}

}


void
show_controls()
{
	xv_set(frame, XV_SHOW, TRUE, NULL);
}


/* dismiss an arbitary command window which is the parent of the panel
 * which is the parent of the button pressed (grandparent?)
 */
void
hide_me(item, event)
Panel_item	item;
Event		*event;
{
	xv_set(xv_get(xv_get(item, XV_OWNER), XV_OWNER), XV_SHOW, FALSE, NULL);
	xv_set(xv_get(xv_get(item, XV_OWNER), XV_OWNER), 
		FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
}
