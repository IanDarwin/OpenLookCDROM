/* colour related routines called by main */
/* declarations for colour panel */
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "robot.h"

Frame		colour_frame;
Panel_item	set_red, set_blue, set_green, colour_choice;
Panel_item	specify_choice;
Panel_item	apply_button;
Canvas		colour_canvas;
Cms		control_cms;
static Xv_singlecolor colors[] = {
	{255, 255, 255},	/* white */
	{255, 25, 25},	/* red */
	{25, 255, 25},	/* green */
	{25, 25, 255},	/* blue */
	{255, 255, 25},	/* yellow */
	{255, 5, 255},	/* magenta */
	{25, 255, 255},	/* cyan */
	{25, 25, 25},	/* black */
/* There are 5's rather than 0's above as I thought the pure colours
 * looked too intense! The demo box does show the correct value though.
 */

};

#define BLACK	7
#define RED	1
#define GREEN	2
#define	BLUE	3
#define	YELLOW	4
#define	MAGENTA	5
#define	CYAN	6
#define	WHITE	0

#define ALL	0
#define JUSTDATA 1
#define REST	2

#define MAXCOLOR 255
#define MINCOLOR 0

#define NUM_COLORS	8

static Colormap	colormap;
static GC              gc_demo;
static XGCValues       gcvalues;
static Window		xwin;
static unsigned int	width, height;
static int xplot1 = 0;
static int yplot1 = 0;

static	int display_depth;


void
demo_colour(red, green, blue)
int red, green, blue;
{
XColor		my_color;
/* only if the display depth is large enough */
	if(display_depth < 2) return;
/* check values are within limits */
	red = MAX(red, MINCOLOR); red = MIN(red, MAXCOLOR);
	green = MAX(green, MINCOLOR); green = MIN(green, MAXCOLOR);
	blue = MAX(blue, MINCOLOR); blue = MIN(blue, MAXCOLOR);


	my_color.red = red << 8;
	my_color.green = green << 8;
	my_color.blue = blue << 8;
	if (XAllocColor (dpy, colormap, &my_color) == 0){
		toarkt_("Color allocation failed - too many colour changes");
		return;
		} 


	XSetForeground(dpy, gc_demo, my_color.pixel);
/* show a block of colour and also a single line */
	XFillRectangle(dpy, xwin, gc_demo, xplot1, yplot1, width/2, height);
	XDrawLine(dpy, xwin, gc_demo, width/2, height/2, width, height/2);
/* and free the value again - if this is not done then a lot of messing about
 * with the sliders can use up all colours (on an 8 bit display)
 */
	XFreeColors(dpy, colormap, (unsigned long *)&my_color.pixel, 1, 0);

}

void
send_colours(red, green, blue)
int	red, blue, green;
{
int 	which;

	demo_colour(red, green, blue);
/* find out what colour selection is to apply to */
	
	which = xv_get(specify_choice, PANEL_VALUE);
	if(which == JUSTDATA)
		sprintf(inst, "RGBDATA %d %d %d", red, green, blue);
	else if(which == REST)
		sprintf(inst, "RGBREST %d %d %d", red, green, blue);
	else
		sprintf(inst, "RGB %d %d %d", red, green, blue);

	xv_set(colour_frame,
#ifdef USA
		FRAME_LEFT_FOOTER, "Color change made",
#else
		FRAME_LEFT_FOOTER, "Colour change made",
#endif
		FRAME_RIGHT_FOOTER, inst,
		NULL);


	to_robot();
}



void
demo_initialise()
{
/* only if the display depth is large enough */

	height = (int) xv_get(colour_canvas, XV_HEIGHT);
	width = (int) xv_get(colour_canvas, XV_WIDTH);

	display_depth = DefaultDepth(dpy, 0);
	if(display_depth < 2) return;
	gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
	gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
	gc_demo = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		       (GCForeground | GCBackground), &gcvalues);
	xwin = xv_get(canvas_paint_window(colour_canvas), XV_XID);
	colormap = DefaultColormap(dpy, DefaultScreen(dpy));	

}

void
slider_demo(item, value, event)
Panel_item	item;
int		value;
Event		*event;
/* Extract colour values from sliders 
 * and demonstrate the colour in a box */
{
int	red, blue, green;


	red = (int) xv_get(set_red, PANEL_VALUE);
	green = (int) xv_get(set_green, PANEL_VALUE);
	blue = (int) xv_get(set_blue, PANEL_VALUE);


	if(xv_get(apply_button, PANEL_INACTIVE) == TRUE){
		xv_set(apply_button, PANEL_INACTIVE, FALSE, NULL);
		xv_set(colour_frame,
			FRAME_LEFT_FOOTER, NULL,
			FRAME_RIGHT_FOOTER, NULL,
			NULL);
	}
	
	demo_colour(red, green, blue);	

}



void
colour_select()
{
int	which_colour, value;
int	which;
int	red, green, blue;
	which = xv_get(specify_choice, PANEL_VALUE);
	if(which == ALL || which == REST){
		which_colour = 1; 
		colget_(&which_colour, &value); red = value;
		which_colour = 2; 
		colget_(&which_colour, &value); green = value;
		which_colour = 3; 
		colget_(&which_colour, &value); blue = value;
	}
	else if(which == JUSTDATA){
		which_colour = 4; 
		colget_(&which_colour, &value); red = value;
		which_colour = 5; 
		colget_(&which_colour, &value); green = value;
		which_colour = 6; 
		colget_(&which_colour, &value); blue = value;
	}
	else
		fprintf(stderr, "Error in colour select");

/* set slider to correct values */
	if(xv_get(set_red, PANEL_VALUE) != red){
		xv_set(set_red, PANEL_VALUE, red, NULL);}
	if(xv_get(set_green, PANEL_VALUE) != green){
		xv_set(set_green, PANEL_VALUE, green, NULL);}
	if(xv_get(set_blue, PANEL_VALUE) != blue){
		xv_set(set_blue, PANEL_VALUE, blue, NULL);}

	xv_set(apply_button, PANEL_INACTIVE, TRUE, NULL);
	xv_set(colour_frame,
		FRAME_LEFT_FOOTER, NULL,
		FRAME_RIGHT_FOOTER, NULL,
		NULL);

	demo_colour(red, green, blue);
}

void
show_colour()
/* Put up the command frame for setting plot colour  */
{
/* find out what the colours are right now 
	what_colour(&red, &green, &blue); */
	colour_select();
/*	xv_set(set_red, PANEL_VALUE, red, NULL);
	xv_set(set_green, PANEL_VALUE, green, NULL);
	xv_set(set_blue, PANEL_VALUE, blue, NULL); */
	xv_set(colour_frame, XV_SHOW, TRUE, NULL);
	xv_set(colour_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
	xv_set(colour_frame,
		FRAME_LEFT_FOOTER, NULL,
		FRAME_RIGHT_FOOTER, NULL,
		NULL);
/* and set up the demo box 
	demo_colour(red, green, blue); */
}


void
colour_values()
/* Extract colour values from sliders 
 * and call routine to set foreground colour */
{
int	red, blue, green;


	red = (int) xv_get(set_red, PANEL_VALUE);
	green = (int) xv_get(set_green, PANEL_VALUE);
	blue = (int) xv_get(set_blue, PANEL_VALUE);


/*	xv_set(colour_choice, PANEL_CHOOSE_NONE, TRUE, NULL); */

	xv_set(apply_button, PANEL_INACTIVE, TRUE, NULL);
	
	send_colours(red, green, blue);	

}


void
colour_button(item, value, event)
Panel_item	item;
int		value;
Event		*event;
/* Get colour values from a button press */
{
int	red, blue, green;
	if(value == BLACK) {
		red = 0; green = 0; blue = 0; }
	else if(value == RED) {
		red = 255; green = 0; blue = 0; }
	else if(value == GREEN) {
		red = 0; green = 255; blue = 0; }
	else if(value == BLUE) {
		red = 0; green = 0; blue = 255; }
	else if(value == YELLOW) {
		red = 255; green = 255; blue = 0; }
	else if(value == MAGENTA) {
		red = 255; green = 0; blue = 255; }
	else if(value == CYAN) {
		red = 0; green = 255; blue = 255; }
	else if(value == WHITE) {
		red = 255; green = 255; blue = 255; }
	else /* not a valid event */
		return;

	xv_set(set_red, PANEL_VALUE, red, NULL);
	xv_set(set_blue, PANEL_VALUE, blue, NULL);
	xv_set(set_green, PANEL_VALUE, green, NULL);

	if(xv_get(apply_button, PANEL_INACTIVE) == FALSE)
		xv_set(apply_button, PANEL_INACTIVE, TRUE);


	send_colours(red, green, blue);

}

/* create the colour control frame */
void
colour_frame_create(frame)
Frame frame;
{
	
	control_cms = (Cms) xv_create(NULL, CMS,
	CMS_CONTROL_CMS,	TRUE,
	CMS_SIZE,		CMS_CONTROL_COLORS + NUM_COLORS,
	CMS_COLORS,		colors,
	NULL);


	colour_frame = xv_create(frame, FRAME_CMD,
#ifdef USA
				FRAME_LABEL, 		"Color",
#else
				FRAME_LABEL, 		"Colour",
#endif
				FRAME_SHOW_FOOTER,	TRUE,
				FRAME_CMD_PUSHPIN_IN,	FALSE,
				WIN_CMS,		control_cms,
				NULL);
	colour_panel = (Panel) xv_get(colour_frame, FRAME_CMD_PANEL);

 	set_red = xv_create(colour_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Red",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_DIRECTION,	PANEL_VERTICAL,
				PANEL_ITEM_COLOR,	
					CMS_CONTROL_COLORS + RED,
				PANEL_MAX_VALUE,	255,
				PANEL_NOTIFY_PROC, slider_demo,
				XV_HELP_DATA,	"robot:color_slider",
				NULL);
 	set_green = xv_create(colour_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Green",
				PANEL_DIRECTION,	PANEL_VERTICAL,
				PANEL_ITEM_COLOR,	
					CMS_CONTROL_COLORS + GREEN,
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MAX_VALUE,	255,
				PANEL_NOTIFY_PROC, slider_demo,
				XV_HELP_DATA,	"robot:color_slider",
				NULL);
 	set_blue = xv_create(colour_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Blue",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_DIRECTION,	PANEL_VERTICAL,
				PANEL_ITEM_COLOR,	
					CMS_CONTROL_COLORS + BLUE,
				PANEL_MAX_VALUE,	255,
				PANEL_NOTIFY_PROC, slider_demo,
				XV_HELP_DATA,	"robot:color_slider",
				NULL);

	xv_set(colour_panel, PANEL_LAYOUT, PANEL_VERTICAL, 
				NULL);

	apply_button = xv_create(colour_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:colour_slider",
			PANEL_LABEL_STRING, "Apply Slider Values",
			PANEL_NOTIFY_PROC, colour_values,
			PANEL_INACTIVE,	TRUE,
				PANEL_ITEM_COLOR,	
					CMS_CONTROL_COLORS + BLACK,
			NULL);
	specify_choice = xv_create(colour_panel, PANEL_CHOICE,
			XV_HELP_DATA,	"robot:specify_choice",
#ifdef USA
			PANEL_LABEL_STRING, "Apply Color to?",
#else
			PANEL_LABEL_STRING, "Apply Colour to?",
#endif
				PANEL_ITEM_COLOR,	
					CMS_CONTROL_COLORS + BLACK,
			PANEL_CHOICE_STRING, ALL, "Everything",
			PANEL_CHOICE_STRING, JUSTDATA, "Just Data",
			PANEL_CHOICE_STRING, REST, "Everything BUT data",
			PANEL_VALUE, ALL,
			PANEL_NOTIFY_PROC, colour_select,
			NULL);


	colour_choice = xv_create(colour_panel, PANEL_CHOICE,
#ifdef USA
		PANEL_LABEL_STRING,	"Colors:",
#else
		PANEL_LABEL_STRING,	"Colours:",
#endif
		PANEL_CHOICE_STRING, WHITE, "White",
		PANEL_CHOICE_STRING, RED, "Red",
		PANEL_CHOICE_STRING, GREEN, "Green",
		PANEL_CHOICE_STRING, BLUE, "Blue",
		PANEL_CHOICE_STRING, YELLOW, "Yellow",
		PANEL_CHOICE_STRING, MAGENTA, "Magenta",
		PANEL_CHOICE_STRING, CYAN, "Cyan",
		PANEL_CHOICE_STRING, BLACK, "Black",
		NULL);
	xv_set(colour_choice, 
		PANEL_CHOICE_COLOR, RED, CMS_CONTROL_COLORS + RED,
		PANEL_CHOICE_COLOR, GREEN, CMS_CONTROL_COLORS + GREEN,
		PANEL_CHOICE_COLOR, BLUE, CMS_CONTROL_COLORS + BLUE,
		PANEL_CHOICE_COLOR, YELLOW, CMS_CONTROL_COLORS + YELLOW,
		PANEL_CHOICE_COLOR, MAGENTA, CMS_CONTROL_COLORS + MAGENTA,
		PANEL_CHOICE_COLOR, CYAN, CMS_CONTROL_COLORS + CYAN,
		PANEL_CHOICE_COLOR, WHITE, CMS_CONTROL_COLORS + WHITE,
		PANEL_CHOICE_COLOR, BLACK, CMS_CONTROL_COLORS + BLACK,
		PANEL_ITEM_COLOR,	CMS_CONTROL_COLORS + BLACK,
		PANEL_NOTIFY_PROC,	colour_button,
		PANEL_CHOOSE_ONE,	TRUE,
		PANEL_VALUE,		BLACK,
		XV_HELP_DATA,		"robot:colour_choice",
		NULL);

if(not_open_look){
	
           xv_set(colour_panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	   xv_create(colour_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}


	window_fit(colour_panel);

	colour_canvas = (Canvas) xv_create(colour_frame, CANVAS,
				  XV_HELP_DATA, "robot:colour_canvas",
				  WIN_X,    0,
				  WIN_BELOW, colour_panel,
				  XV_HEIGHT, 20,
				  XV_WIDTH, 50,
				  WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS + WHITE, 
				  NULL);




	window_fit(colour_frame);
/* set up windows stuff */
	demo_initialise();
}



