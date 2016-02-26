#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>

#include "robot.h"

Menu canvas_menu;
Menu old_canvas_menu;

extern void orient(), show_controls(), show_print();
extern void kill_old_plot(), print_old_plot(), make_old_current();
extern void last_to_current();
extern void menu_proc();

/* create a menu for the current frame */
void
make_canvas_menu(frame)
Frame	frame;
{

	canvas_menu = xv_create(NULL, MENU,
			MENU_GEN_PIN_WINDOW, frame, "Plot Area Menu",
			MENU_ITEM,	
				MENU_STRING,	"Flip",
				MENU_NOTIFY_PROC, orient,
				NULL,
			MENU_ITEM,
				MENU_STRING,	"Controls to Front",
				MENU_NOTIFY_PROC, show_controls,
				NULL,
			MENU_ITEM,
				MENU_STRING,	"Print...",
				MENU_NOTIFY_PROC, show_print,
				NULL,
			NULL);

	if(not_open_look){
		xv_set(canvas_menu, MENU_APPEND_ITEM,
			xv_create(NULL, MENUITEM,
				MENU_STRING, "",
				MENU_INACTIVE, TRUE,
				NULL),
			NULL);	
		xv_set(canvas_menu, MENU_APPEND_ITEM, 
			xv_create(NULL, MENUITEM,
				MENU_STRING, DISMISS,
				MENU_NOTIFY_PROC, menu_proc,
				NULL),
			NULL);
	}
}

/* a much simpler menu for old plot areas */
void
make_old_canvas_menu(frame)
Frame	frame;
{
	old_canvas_menu = xv_create(NULL, MENU,
			MENU_TITLE_ITEM, "Old Plot Menu",

			MENU_ITEM,
				MENU_STRING, "Select Plot",
				MENU_NOTIFY_PROC, make_old_current,
				NULL,
			MENU_ITEM,
				MENU_STRING, "",
				MENU_INACTIVE, TRUE,
				NULL,
			MENU_ITEM,
				MENU_STRING, "Destroy Plot Area",
				MENU_NOTIFY_PROC, kill_old_plot,
				NULL,			NULL);
}

void show_canvas_menu(canvas, event)
Canvas	canvas;
Event	*event;
{
	menu_show(canvas_menu, canvas, event, NULL);
}
				
void show_old_canvas_menu(canvas, event, old_menu_title)
Canvas	canvas;
Event	*event;
char	*old_menu_title;
{
	xv_set(old_canvas_menu, 
		MENU_TITLE_ITEM, old_menu_title,
		NULL);
	menu_show(old_canvas_menu, canvas, event, NULL);
}
				
				
void hide_old_canvas_menu()
{
	xv_set(xv_get(old_canvas_menu, MENU_PIN_WINDOW), 
		XV_SHOW, FALSE, NULL);
}
