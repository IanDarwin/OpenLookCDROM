/* zoom/pan related controls */

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

#include "robot.h"

Frame	zoom_frame;
int	i;

#define N_SWITCHES 6
Panel_item	zoom_control[N_SWITCHES];
Panel_item	zoom_range;
Panel_item	pan_range;
/* Panel_item	reset_button;
Panel_item	cursor_button; */

static char *labels[] = {
	"Zoom In", "Zoom Out", "Pan Right", "Pan Left",
	"Pan Up", "Pan Down" };
static char *instructions[] = {
	"ZoomIn", "ZoomOut", "PanRight", "PanLeft",
	"PanUp", "PanDown" };


void
zoom_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
char	name[20];

	sprintf(name, "%s", (char *)xv_get(item, PANEL_LABEL_STRING));

	for(i = 0; i < N_SWITCHES; i++){
		if(streq(name, labels[i])){
			strcpy(inst, instructions[i]);
			break;
		}
	}

	to_robot();



}


void
zoom_range_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

	sprintf(inst, "ZOOMVALUE %d", (int) xv_get(zoom_range, PANEL_VALUE));


	to_robot();



}


void
pan_range_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

	sprintf(inst, "PANVALUE %d", (int) xv_get(pan_range, PANEL_VALUE));

	to_robot();

}

void
reset_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

	sprintf(inst, "RESCALE; N; G");

	to_robot();

}

void
cursor_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

	sprintf(inst, "DFLIMITS; N; G");

	to_robot();

}


void
make_zoom_frame(frame)
Frame	frame;
{

	zoom_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "Zoom/Pan Controls",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_SHOW_FOOTER, TRUE,
			NULL);
	zoom_panel = xv_get(zoom_frame, FRAME_CMD_PANEL);
	xv_set(zoom_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);


	for (i = 0; i < N_SWITCHES; i++){
	    zoom_control[i] =  xv_create(zoom_panel, PANEL_BUTTON,
			XV_HELP_DATA,		helper(labels[i]),
			PANEL_LABEL_STRING,	labels[i],
			PANEL_NOTIFY_PROC,	zoom_proc,
			NULL);
	}




	zoom_range = xv_create(zoom_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Zoom Value (%)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MAX_VALUE,	50,
				PANEL_NOTIFY_PROC, zoom_range_proc,
				XV_HELP_DATA,	"robot:zoom_range",
				PANEL_TICKS,		6,
                		PANEL_NEXT_COL,         -1,
				NULL);

	pan_range = xv_create(zoom_panel, PANEL_SLIDER,
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_LABEL_STRING, "Pan Value (%)",
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_MAX_VALUE,	50,
				PANEL_NOTIFY_PROC, pan_range_proc,
				XV_HELP_DATA,	"robot:pan_range",
				PANEL_TICKS,		6,
				XV_Y,	xv_get(zoom_control[2], XV_Y),
				PANEL_VALUE_X,	
			          xv_get(zoom_range, PANEL_VALUE_X),
				NULL);
/* Buttons for cursor selection (dflimits) and reset */

	xv_create(zoom_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:cursor_button",
			PANEL_LABEL_STRING,	"Use Cursor",
			PANEL_NOTIFY_PROC,	cursor_proc,
			NULL);

	xv_create(zoom_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:reset_button",
			PANEL_LABEL_STRING,	"Full Range",
			PANEL_NOTIFY_PROC,	reset_proc,
			NULL);



if(not_open_look){
	
	   xv_create(zoom_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}


	window_fit(zoom_panel);
	window_fit(zoom_frame);
}

void
show_zoom()
{
	xv_set(zoom_frame, XV_SHOW, TRUE,
			   FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
}



/* routines to make sure sliders have correct values */
void
spr_(value)
float *value;
{
	i = (int) *value;
	if(i != (int) xv_get(pan_range, PANEL_VALUE))
		xv_set(pan_range, PANEL_VALUE, i, NULL);
}
void
szr_(value)
float *value;
{
	i = (int) *value;
	if(i != (int) xv_get(zoom_range, PANEL_VALUE))
		xv_set(zoom_range, PANEL_VALUE, i, NULL);
}
