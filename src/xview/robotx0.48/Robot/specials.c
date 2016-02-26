/* show the special characters we can plot via text command etc. */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/xv_error.h>
#include <xview/font.h>

#include "robot.h"

#define CHANGEME	"unsettext"

extern Frame get_text_frame;

Frame	specials_frame;

Panel_item     specials_list;
Panel_item     symbols_list;
Panel_item     greek_list;
Panel_item     extras_list;
Panel_item     extras_hide_button;

Rect	rect1;
Rect	rect2;

static GC              gc_demo;
static XGCValues       gcvalues;
static Window		xwin;
static unsigned int	width, height, starty;
static int		big_width, small_width;
static Canvas		specials_canvas;
XFontStruct	*cur_font;
Xv_Font	font;

char	special_buffer[20];
char	special_buffer2[25];

int i, j;

int font_now = -1;
#define SYMBOL 0
#define ROMAN  1



void
font_check_set()
{
	if(cur_font == NULL){
/* fall back */
		font = (Xv_Font) xv_get(specials_frame, XV_FONT);
		cur_font = (XFontStruct *) xv_get(font, FONT_INFO);
	}
/* and check again */
	if(cur_font == NULL)
		fprintf(stderr, "Still an error in setting the font!");
	else
		XSetFont(dpy, gc_demo, cur_font->fid);
}


void
symbol_font()
{
	if(font_now == SYMBOL) return;

	   cur_font = 
		XLoadQueryFont(dpy, 
	"-adobe-symbol-medium-r-normal--18-180-75-75-p-107-adobe-fontspecific");

	if(cur_font == NULL)
		cur_font = XLoadQueryFont(dpy, "Symbol-18");
/* if we didn't manage to set that font use a "generic" symbol font */
	if(cur_font == NULL)
		cur_font = XLoadQueryFont(dpy, "Symbol");
	if(cur_font == NULL)
		cur_font = XLoadQueryFont(dpy, "symbol");

	font_check_set();

	font_now = SYMBOL;
}

void
roman_font()
{
	if(font_now == ROMAN) return;

	cur_font = XLoadQueryFont(dpy, "times-roman-18");
/* if we didn't manage to set that font use a "generic" roman font */
	if(cur_font == NULL)
		cur_font = XLoadQueryFont(dpy, "times-roman");

	font_check_set();
	/* XSetFont(dpy, gc_demo, cur_font->fid); */

	font_now = ROMAN;
}

void
show_specials()
{
/* position specials_frame below text prompt frame */
	frame_get_rect(get_text_frame, &rect1);
	frame_get_rect(specials_frame, &rect2);
	rect2.r_top = rect1.r_top + rect1.r_height;
	rect2.r_left = rect1.r_left;
	frame_set_rect(specials_frame, &rect2);

/* and display it */
	xv_set(specials_frame,	XV_SHOW, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
}

void
hide_specials()
{
	xv_set(specials_frame, XV_SHOW, FALSE, NULL);
	xv_set(specials_frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
/* overkill to try and make sure it works! */
	xv_set(specials_frame, XV_SHOW, FALSE, NULL);
	xv_set(specials_frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
}


Bool
specials_visible()
{
	if(xv_get(specials_frame, XV_SHOW))
		return(TRUE);
	else
		return(FALSE);
}



void
specials_ok()
{
	if(streq(special_buffer2, CHANGEME)){
		sprintf(special_buffer2, "\\{%s}", special_buffer);
	}
	modify_text(special_buffer2);

}

void
hide_extras()
{
	xv_set(extras_list, XV_SHOW, FALSE, NULL);
	xv_set(extras_hide_button, XV_SHOW, FALSE, NULL);
	xv_set(specials_panel, XV_WIDTH, small_width,
		NULL);
	xv_set(specials_frame, XV_WIDTH, small_width,
		NULL);
}

void
show_extras()
{
	xv_set(extras_list, XV_SHOW, TRUE, NULL);
	xv_set(extras_hide_button, XV_SHOW, TRUE, NULL);
	xv_set(specials_frame, XV_WIDTH, big_width,
		NULL);
}

struct name_code
{
	char name[15];
	char code[6];
};
static struct name_code names[] =
{
{"Super Script", "up"},
{"Sub Script", "dn"},
{"Back Space", "bs"},
{"", ""},
{"Italic", "if"},
{"Bold", "bf"},
{"Bold Italic", "bi"},
{"Normal", "nf"},
{"", ""},
{"Helvetica", "hf"},
{"Times", "tf"},
{"Courier", "cf"},
{"Symbol", "sf"},
{"", ""},
{"10 Point", "10"},
{"12 Point", "12"},
{"14 Point", "14"},
{"18 Point", "18"},
{"Bigger Font", "+"},
{"Smaller Font", "-"},
{"", ""},
{"Red", "red"},
{"Green", "green"},
{"Blue", "blue"},
{"Black", "black"},
{"White", "white"},
{"last", "last"},
};


/* call extra control sequence require special hard wired decoding */
void
extras_click(item, string, client_data, op, event)
Panel_item	item;
char		*string;
Xv_opaque	client_data;
Panel_list_op	op;
Event		*event;
{

	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};

	strcpy(special_buffer2, CHANGEME);
	if(streq(string, "")){
		return;
	}

	i = 0;
	while (strne(names[i].name, "last")){
		if(streq(string, names[i].name)){
			sprintf(special_buffer2, "\\%s",names[i].code);
			break;
		}
	i++;
	}

	XClearWindow(dpy, xwin);

	if(op == PANEL_LIST_OP_SELECT){
	roman_font();
	XDrawString(dpy, xwin, gc_demo, 5, starty, 
				special_buffer2, strlen(special_buffer2));
	now = event_time(event);
	if(double_click(last_string, &then, string, &now))
		specials_ok();
	strcpy(last_string, string);
	then = now;
	}
}

/* call specials OK if we get a double click */
void
symbols_click(item, string, client_data, op, event)
Panel_item	item;
char		*string;
Xv_opaque	client_data;
Panel_list_op	op;
Event		*event;
{
	char	small_buffer[1];
	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};

	strcpy(special_buffer2, CHANGEME);

	strcpy(special_buffer, string);

/* is it part of a symbol/value pair? */

	if(!(ark_symbol_code(string, small_buffer))){
		fflush(stdout);
		ark_greek_code(string, small_buffer);
	}
	XClearWindow(dpy, xwin);

	if(op == PANEL_LIST_OP_SELECT){
	symbol_font();
	XDrawString(dpy, xwin, gc_demo, 5, starty, 
				small_buffer, 1);
	now = event_time(event);
	if(double_click(last_string, &then, string, &now))
		specials_ok();
	strcpy(last_string, string);
	then = now;
	}
}

/* call specials OK if we get a double click */
void
specials_click(item, string, client_data, op, event)
Panel_item	item;
char		*string;
Xv_opaque	client_data;
Panel_list_op	op;
Event		*event;
{
	char	small_buffer[1];
	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};


	strcpy(special_buffer2, CHANGEME);

	strcpy(special_buffer, string);

/* is it part of a symbol/value pair? */

	ark_specials_code(string, small_buffer);


	XClearWindow(dpy, xwin);

	if(op == PANEL_LIST_OP_SELECT){
	roman_font();
	XDrawString(dpy, xwin, gc_demo, 5, starty, 
				small_buffer, 1);
	now = event_time(event);
	if(double_click(last_string, &then, string, &now))
		specials_ok();
	strcpy(last_string, string);
	then = now;
	}
}


void
make_specials_frame(frame)
Frame	frame;
{
char	string[20];

	specials_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "Special Characters",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_SHOW_FOOTER, FALSE,
			NULL);


	specials_panel = xv_get(specials_frame, FRAME_CMD_PANEL);
	xv_set(specials_panel,
			PANEL_LAYOUT, PANEL_VERTICAL,
			XV_HELP_DATA, "robot:specials_panel",
			NULL);

	symbols_list = xv_create(specials_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	15,
			PANEL_LIST_WIDTH,		110,
			PANEL_NOTIFY_PROC,		symbols_click,
			XV_HELP_DATA,			"robot:symbols_list",
			PANEL_LIST_TITLE,		"Symbols",
			NULL);

	greek_list = xv_create(specials_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	15,
			PANEL_LIST_WIDTH,		110,
			PANEL_NEXT_COL,			-1,
			PANEL_NOTIFY_PROC,		symbols_click,
			XV_HELP_DATA,			"robot:greek_list",
			PANEL_LIST_TITLE,		"Greek",
			NULL);

	specials_list = xv_create(specials_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	15,
			PANEL_LIST_WIDTH,		110,
			PANEL_NEXT_COL,			-1,
			PANEL_NOTIFY_PROC,		specials_click,
			XV_HELP_DATA,			"robot:specials_list",
			PANEL_LIST_TITLE,		"Others",
			NULL);

	xv_create(specials_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING,	"Add",
			PANEL_NEXT_COL,			-1,
			XV_HELP_DATA,	"robot:special_ok",
			PANEL_NOTIFY_PROC,	specials_ok,
			NULL);

	xv_create(specials_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING,	"Extras...",
			XV_HELP_DATA,	"robot:extras",
			PANEL_NOTIFY_PROC,	show_extras,
			NULL);

	extras_hide_button = xv_create(specials_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING,	"Hide Extras",
			XV_HELP_DATA,	"robot:hide_extras",
			PANEL_NOTIFY_PROC,	hide_extras,
			XV_SHOW,		FALSE,
			NULL);

if(not_open_look){
	
	   xv_create(specials_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	extras_list = xv_create(specials_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	15,
			PANEL_LIST_WIDTH,		110,
			PANEL_NEXT_COL,			-1,
			PANEL_NOTIFY_PROC,		extras_click,
			XV_HELP_DATA,			"robot:extras_list",
			PANEL_LIST_TITLE,		"Control",
			XV_SHOW,			FALSE,
			NULL);

	window_fit(specials_panel);

	specials_canvas = (Canvas) xv_create(specials_frame, CANVAS,
				  XV_HELP_DATA, "robot:specials_canvas",
				  WIN_X,    0,
				  WIN_BELOW, specials_panel,
				  XV_HEIGHT, 40,
				  XV_WIDTH, 65,
				  NULL);

	height = (int) xv_get(specials_canvas, XV_HEIGHT);
	width = (int) xv_get(specials_canvas, XV_WIDTH);

	starty = (height * 2)/3;

	xv_set(specials_canvas, 
		XV_X, xv_get(extras_hide_button, XV_X),
		XV_Y, xv_get(specials_list, XV_Y) +
		      xv_get(specials_list, XV_HEIGHT)
			- height, 
		NULL);
			

	window_fit(specials_frame);

	big_width = xv_get(specials_panel, XV_WIDTH);
	small_width = big_width -
		xv_get(extras_list, XV_WIDTH);


	gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
	gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
	gc_demo = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		       (GCForeground | GCBackground), &gcvalues);
	xwin = xv_get(canvas_paint_window(specials_canvas), XV_XID);

	symbol_font();

/* find symbol names */
	i = 0;

	get_symbol(i, string);
	strcpy(special_buffer, string);

	while (strne(string, "last")){
	   xv_set(symbols_list, PANEL_LIST_STRING, i, string, NULL);
	   i++;
	   get_symbol(i, string);
	}
/* and now the same thing for Greek characters */

	i = 0;
	get_greek(i, string);

	while (strne(string, "last")){
	   xv_set(greek_list, PANEL_LIST_STRING, i, string, NULL);
	   i++;
	   get_greek(i, string);
	}

/* special characters which are part of the "standard"
 * character set */

	i = 0;
	get_special(i, string);

	while (strne(string, "last")){
	   xv_set(specials_list, PANEL_LIST_STRING, i, string, NULL);
	   i++;
	   get_special(i, string);
	}

/* and control sequences rather than special characters themselves */

	i = 0;
	while (strne(names[i].name, "last")){
	   xv_set(extras_list, PANEL_LIST_STRING, i, names[i].name, NULL);
	   i++;
	}

	/* ensure width is right for lack of extras list */
	hide_extras();



}
