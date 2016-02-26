/* command axes */

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

Frame	axes_frame;
#define N_SWITCHES 6

Panel_item	axis_control[N_SWITCHES];

int	i;
static int n2 = 5;

static char *labels[] = {
	"Top Axis", "Bottom Axis", "Left Axis", "Right Axis",
	"X Tick Marks", "Y Tick Marks" };
static char *instructions[] = {
	"TopAxis", "NoTopAxis", "BottomAxis", "NoBottomAxis",
	"LeftAxis", "NoLeftAxis", "RightAxis", "NoRightAxis", 
	"InternalXTicks", "ExternalXTicks", "InternalYTicks",
	"ExternalYTicks",};

static char *labels2[] = {"X Tick Size", "Y Tick Size",
"Minor Tick Size", "Minor Tick Frequency", "Default Tick Size"};


static char *instructions2[] = {"XTickSize", "YTickSize",
"MinorTickSize", "MinorTickFrequency", "DefaultTickSize",};




void
axes_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
char	name[20];
	sprintf(name, "%s", (char *)xv_get(item, PANEL_LABEL_STRING));
	for(i = 0; i < N_SWITCHES; i++){
		if(streq(name, labels[i])){
			strcpy(inst, instructions[i*2+value]);
			break;
		}
	}
	to_robot();



}


void
axes2_proc(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
char	name[20];
	sprintf(name, "%s", (char *)xv_get(item, PANEL_LABEL_STRING));
	for(i = 0; i < N_SWITCHES; i++){
		if(streq(name, labels2[i])){
			strcpy(inst, instructions2[i]);
			break;
		}
	}
	to_robot();



}


void
make_axes_frame(frame)
Frame	frame;
{

	axes_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "Axes Controls",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_SHOW_FOOTER, TRUE,
			FRAME_LEFT_FOOTER, "See also \"Style\" menu",
			NULL);
	axes_panel = xv_get(axes_frame, FRAME_CMD_PANEL);
	xv_set(axes_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	for (i = 0; i < N_SWITCHES; i++){
	axis_control[i] = xv_create(axes_panel, PANEL_CHOICE,
			XV_HELP_DATA,		helper(labels[i]),
			PANEL_LABEL_STRING,	labels[i],
			PANEL_NOTIFY_PROC,	axes_proc,
			PANEL_CHOICE_STRINGS,
				"Draw", "Don't Draw", NULL,
			PANEL_VALUE,	0,
			PANEL_VALUE_X,		110,
			PANEL_LABEL_X,		5,
			NULL);
	if(i > 3) xv_set(axis_control[i], PANEL_CHOICE_STRINGS,
		"Internal", "External", NULL,
		NULL);
	}
	xv_set(axes_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
	xv_set(axes_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	for(i = 0; i < n2; i++){

	xv_create(axes_panel, PANEL_BUTTON,
			XV_HELP_DATA,		helper(labels2[i]),
			PANEL_LABEL_STRING,	labels2[i],
			PANEL_NOTIFY_PROC,	axes2_proc,
			NULL);
	}

if(not_open_look){
	
	   xv_create(axes_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}


	window_fit(axes_panel);
	window_fit(axes_frame);
}

void
show_axes()
{
	xv_set(axes_frame,	XV_SHOW, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
}


void
axis_control_reset()
{
	for(i = 0; i < N_SWITCHES; i++){
		xv_set(axis_control[i], PANEL_VALUE, 0,
				NULL);
	}
}

void
turn_on(string)
char	string[];
{
int i;
	for (i = 0; i < N_SWITCHES; i++){
		if(streq(string, labels[i]))
			xv_set(axis_control[i],
				PANEL_VALUE, 0,
				NULL);
	}
}

turn_off(string)
char	string[];
{
int i;
	for (i = 0; i < N_SWITCHES; i++){
		if(streq(string, labels[i]))
			xv_set(axis_control[i],
				PANEL_VALUE, 1,
				NULL);
	}
}
	

void
axis_command_check(instruction)
char instruction[];
{

	if(streq(instruction, "ALLAXES")){
		turn_on("Top Axis");
		turn_on("Bottom Axis");
		turn_on("Right Axis");
		turn_on("Left Axis");
	}
	else if(streq(instruction, "TOPAXIS")){
		turn_on("Top Axis");
	}
	else if(streq(instruction, "NOTOPAXIS")){
		turn_off("Top Axis");
	}
	else if(streq(instruction, "BOTTOMAXIS")){
		turn_on("Bottom Axis");
	}
	else if(streq(instruction, "NOBOTTOMAXIS")){
		turn_off("Bottom Axis");
	}
	else if(streq(instruction, "RIGHTAXIS")){
		turn_on("Right Axis");
	}
	else if(streq(instruction, "NORIGHTAXIS")){
		turn_off("Right Axis");
	}
	else if(streq(instruction, "LEFTAXIS")){
		turn_on("Left Axis");
	}
	else if(streq(instruction, "NOLEFTAXIS")){
		turn_off("Left Axis");
	}
	else if(streq(instruction, "INTERNALXTICKS")){
		turn_on("X Tick Marks");
	}
	else if(streq(instruction, "EXTERNALXTICKS")){
		turn_off("X Tick Marks");
	}
	else if(streq(instruction, "INTERNALYTICKS")){
		turn_on("Y Tick Marks");
	}
	else if(streq(instruction, "EXTERNALYTICKS")){
		turn_off("Y Tick Marks");
	}
	else if(streq(instruction, "INTERNALTICKS")){
		turn_on("X Tick Marks");
		turn_on("Y Tick Marks");
	}
	else if(streq(instruction, "EXTERNALTICKS")){
		turn_off("X Tick Marks");
		turn_off("Y Tick Marks");
	}
	else;
}
