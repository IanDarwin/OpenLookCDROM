#include <stdio.h>
#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include "fractool.h"



extern int state;
extern int rulenum;
extern int rulegrid_type, initgrid_type;
extern int connect_flags, n_rules;
extern int last_x, last_y;
extern int slider_values[];
extern struct rules_hdr rh_array[];
extern struct curve *new_fractal, *fractal, *iterate();
extern struct curve *init_curve, *new_curve();
extern struct curve_rules *forming_rule;
extern int rule_ep0_x, rule_ep0_y, rule_ep1_x, rule_ep1_y;
extern struct toolsw *mainsw;
extern struct pixwin *main_pixwin;
extern Panel panel;
extern Panel_item slider1_pi, slider2_pi, slider3_pi, slider4_pi;
extern Panel_item connect_pi, close_pi, done_pi, clearall_pi;
extern Panel_item clearlastpoint_pi, defxform_pi, definit_pi;
extern Panel_item clearlastcurve_pi, iterate_pi;
extern Panel_item defopencurve_pi, defclosedcurve_pi;
extern Panel_item grid_pi;
extern Panel_item newstrand_pi, connectstart_pi, restart_pi;
extern Panel_item save_pi, restore_pi, filename_pi;
extern int working_on_a_new_curve, current_segment_closed;
extern int curvetype, first_x, first_y;
extern struct curve_rules *accumulated_rules, *rule_in_progress;
extern struct endpoint *new_endpoint();
extern struct curve *replicate_curve();
extern struct rules_hdr *first_rh;
extern struct pixrect *menu_empty_pr, *menu_square_pr, *menu_triangle_pr;

struct curve_rules *new_curverules();







iterate_notify(p, item)
Panel p;
Panel_item item;
{
        struct curve *new_fractal;
	int i;
        long total_all_sliders, last_prob;
        struct rules_hdr *rhp;

        new_fractal = iterate(fractal);
        discard_curve(fractal);
        fractal = new_fractal;
        display_curve(fractal, FALSE);
}


slider1_notify(item, value, event)
Panel_item item;
unsigned int value;
struct inputevent *event;
{
	slider_values[0] = (int)value;
}


slider2_notify(item, value, event)
Panel_item item;
unsigned int value;
struct inputevent *event;
{
        slider_values[1] = (int)value;
}

  
slider3_notify(item, value, event)
Panel_item item;
unsigned int value;
struct inputevent *event;
{
        slider_values[2] = (int)value;
}

  
slider4_notify(item, value, event)
Panel_item item;
unsigned int value;
struct inputevent *event;
{
        slider_values[3] = (int)value;
}


definit_notify(item, value, event)
Panel_item item;
int value;
struct inputevent *event;
{
	struct curve *c;

	/*************************
        if (event->ie_code != MS_RIGHT) 
                return(-1);
        ************************/

	state = DEFINING_INIT;
	redraw_panel();
	redraw_main_pixwin();

        c = new_curve();
        c->next = init_curve;
        init_curve = c;
        c->start = NULL;
}



clearlastpoint_notify()
{ 
	struct curve *cp;
	int done;
	struct endpoint *ep, *ep1;

	if (state == DEFINING_XFORM) { 
                return(0); 
        } 
        else if (state == DEFINING_INIT) {
		cp = init_curve;
		done = FALSE;
		while (init_curve != NULL && done == FALSE) {
			if ( (ep = init_curve->start) == NULL) {
				init_curve = init_curve->next;
				cp->next = NULL;
				discard_curve(cp);
				cp = init_curve;
			}
			else if (ep->next == NULL) {
				init_curve = init_curve->next;
				cp->next = NULL; 
                                discard_curve(cp);
				done = TRUE;
			}
			else {
				while (ep->next->next != NULL)
					ep = ep->next;
				free_endpoint(ep->next);
				ep->next = NULL;
				done = TRUE; 
                        }
		}
		redraw_main_pixwin();
	}
} 
 
 
clearlastcurve_notify()
{
	struct curve *c;

	if (state == DEFINING_XFORM) {
		return(0);
	}
	else if (state == DEFINING_INIT) {
		if ( (c = init_curve) == NULL)
			return(0);
		init_curve = init_curve->next;
		c->next = NULL;
		discard_curve(c);
                redraw_main_pixwin();
	}
}

 
clearall_notify()
{ 
	struct curve *c;

	if (state == DEFINING_XFORM) {
                return(0);
        }
        else if (state == DEFINING_INIT) {
		discard_curve(init_curve);
		init_curve = NULL;
                redraw_main_pixwin();
	}
} 
 
 
 
connect_notify(i, val, e)
Panel_item i;
unsigned int val;
struct inputevent *e;
{ 
	connect_flags = val;
} 
 
 
 
defxform_notify(item, value, event)
Panel_item item;
int value;
struct inputevent *event;
{ 
	struct rect rect;
	int fifth;

	if (event->ie_code != MS_RIGHT)
		return(-1);

	rulenum = value;
	state = DEFINING_XFORM;
	redraw_panel();

        win_getsize(mainsw->ts_windowfd, &rect);
        pw_writebackground(main_pixwin, 0, 0,
          rect.r_width, rect.r_height, PIX_CLR);
        fifth = rect.r_width / 5;
        rule_ep0_x = fifth;
        rule_ep1_x = rule_ep0_x + (((3*fifth) / GRIDSIZE) * GRIDSIZE);
        rule_ep0_y = rule_ep1_y = rect.r_height / 2;
        draw_a_dot(rule_ep0_x, rule_ep0_y);
        draw_a_dot(rule_ep1_x, rule_ep1_y);
	accumulated_rules = NULL;

	/********************************************************
	orient_rule_endpoints();
	draw_rule(rh_array[rulenum].rules, rule_ep0_x, rule_ep0_y,
		  rule_ep1_x, rule_ep1_y);
	curvetype = NONE;
	accumulated_rules = rh_array[rulenum].rules NULL;
	*********************************************************/

	if (rulegrid_type == SQUARE)
		draw_square_grid();
	else if (rulegrid_type == TRIANGULAR)
		draw_triangular_grid();
} 
 
 
 
done_notify()
{ 
        struct rect rect; 
	struct curve *c;

	if (state == DEFINING_XFORM) {
		if (curvetype != NONE) {
			rule_in_progress->flags = connect_flags;
			rule_in_progress->next = accumulated_rules;
			accumulated_rules = rule_in_progress;
		}
		rh_array[rulenum].rules = accumulated_rules;
	}

	if (state == DEFINING_INIT) {
		discard_curve(fractal);
		/* Throw away any empty curve headers. */
		while (init_curve != NULL && init_curve->start == NULL) {
			c = init_curve;
			init_curve = init_curve->next;
			c->next = NULL;
			discard_curve(c);
		}
		fractal = replicate_curve(init_curve);
	}

	state = RUNNING;
	redraw_panel();
	display_curve(fractal, FALSE);
} 
 
 
 
close_notify()
{ 
	int x, y;

	if (curvetype == NONE)
		return(0);

	if (curvetype == CLOSED) {
		if (connect_flags & CONNECT_INITIAL) {
			x = rule_ep0_x;
			y = rule_ep0_y;
		}
		else {
			x = first_x;
			y = first_y;
		}
		connect_flags &= (~CONNECT_TERMINAL);
	}
	else {
		if (connect_flags & CONNECT_TERMINAL) {
			x = rule_ep1_x;
			y = rule_ep1_y;
		}
		else
			return(0);
	}

	pw_vector(main_pixwin, last_x, last_y, x, y, PIX_SET, 1);

	if (!( (connect_flags & CONNECT_TERMINAL) &&
	       (curvetype == OPEN) ) )
		new_dot_in_rule(x, y);

	rule_in_progress->flags = connect_flags;
	rule_in_progress->next = accumulated_rules; 
	accumulated_rules = rule_in_progress;

	curvetype = NONE;
} 
 

defopencurve_notify()
{
	defcurve(OPEN);
}



defclosedcurve_notify()
{ 
	defcurve(CLOSED);
} 


defcurve(type)
int type;
{
	if (curvetype != NONE) {
		rule_in_progress->flags = connect_flags; 
        	rule_in_progress->next = accumulated_rules;  
        	accumulated_rules = rule_in_progress; 
	}

	curvetype = type;
	rule_in_progress = new_curverules();
	rule_in_progress->start = NULL;
	last_x = rule_ep0_x;
	last_y = rule_ep0_y;
}



grid_notify(item, value, event)
Panel_item item;
int value;
struct inputevent *event;
{
	if (event->ie_code != MS_RIGHT)
		return(-1);

	if (state == DEFINING_XFORM)
		rulegrid_type = value;
	else if (state == DEFINING_INIT)
		initgrid_type = value;

	redraw_main_pixwin();
}



connectstart_notify()
{
	struct endpoint *ep;

	/* init_curve must have at least 2 endpoints. */
	if (init_curve == NULL ||
	    init_curve->start == NULL ||
	    init_curve->start->next == NULL || 
            init_curve->start->next->next == NULL)
		return(0);

	ep = init_curve->start;
	while (ep->next != NULL)
		ep = ep->next;
	ep->next = new_endpoint();
	ep->next->x = init_curve->start->x;
	ep->next->y = init_curve->start->y;
	ep->next->next = NULL;
	pw_vector(main_pixwin, (int)ep->x, (int)ep->y,
		  (int)ep->next->x, (int)ep->next->y,
		  PIX_SET, NULL);
}



newstrand_notify()
{
	struct curve *c;

	c = new_curve();
	c->next = init_curve;
	init_curve = c;
	c->start = NULL;
}



restart_notify()
{
	discard_curve(fractal);
	fractal = replicate_curve(init_curve);
	display_curve(fractal, FALSE);
}



save_notify()
{
	char filename[80];

	strcpy(filename, (char *)panel_get_value(filename_pi));
	save(filename);
}


restore_notify() 
{ 
        char filename[80];
 
        strcpy(filename, (char *)panel_get_value(filename_pi));
	if (restore(filename) == -1)
		return(0);
	panel_set(slider1_pi, PANEL_VALUE, slider_values[0], 0);
        panel_set(slider2_pi, PANEL_VALUE, slider_values[1], 0);
        panel_set(slider3_pi, PANEL_VALUE, slider_values[2], 0);
        panel_set(slider4_pi, PANEL_VALUE, slider_values[3], 0);
        discard_curve(fractal);
        fractal = replicate_curve(init_curve);
        display_curve(fractal, FALSE);
} 
 


init_panel_items(font)
struct pixfont *font;
{
	iterate_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
			  panel_button_image(panel, "Iterate", 8, font),
			PANEL_NOTIFY_PROC, iterate_notify,
			PANEL_ITEM_X, PANEL_CU(1),
			PANEL_ITEM_Y, PANEL_CU(1),
			0);
	slider1_pi = panel_create_item(panel, PANEL_SLIDER,
			PANEL_LABEL_STRING, "Xform #1",
			PANEL_VALUE, 100,
			PANEL_MIN_VALUE, 0,
			PANEL_MAX_VALUE, 100,
                        PANEL_NOTIFY_PROC, slider1_notify,
			PANEL_ITEM_X, PANEL_CU(1),
                        PANEL_ITEM_Y, PANEL_CU(3),
			0);
	slider_values[0] = 100;
	slider2_pi = panel_create_item(panel, PANEL_SLIDER,
                        PANEL_VALUE, 0, 
                        PANEL_LABEL_STRING, "Xform #2",
                        PANEL_MIN_VALUE, 0, 
                        PANEL_MAX_VALUE, 100,  
                        PANEL_NOTIFY_PROC, slider2_notify,
			PANEL_ITEM_X, PANEL_CU(41),
                        PANEL_ITEM_Y, PANEL_CU(3),
                        0);
	slider3_pi = panel_create_item(panel, PANEL_SLIDER, 
                        PANEL_VALUE, 0, 
                        PANEL_LABEL_STRING, "Xform #3",
                        PANEL_MIN_VALUE, 0, 
                        PANEL_MAX_VALUE, 100,   
                        PANEL_NOTIFY_PROC, slider3_notify,
			PANEL_ITEM_X, PANEL_CU(1),
                        PANEL_ITEM_Y, PANEL_CU(4),
                        0); 
        slider4_pi = panel_create_item(panel, PANEL_SLIDER,  
                        PANEL_VALUE, 0,  
                        PANEL_LABEL_STRING, "Xform #4",
                        PANEL_MIN_VALUE, 0,  
                        PANEL_MAX_VALUE, 100,    
			PANEL_NOTIFY_PROC, slider4_notify,
			PANEL_ITEM_X, PANEL_CU(41), 
                        PANEL_ITEM_Y, PANEL_CU(4),
                        0); 
	slider_values[1] = slider_values[2] = slider_values[3] = 0;
	connect_pi = panel_create_item(panel, PANEL_TOGGLE,
			PANEL_LABEL_STRING, "Connect to",
			PANEL_NOTIFY_PROC, connect_notify,
			PANEL_CHOICE_STRINGS, "Start", "End", 0,
			PANEL_VALUE, 3,
			PANEL_ITEM_X, PANEL_CU(1),
                        PANEL_ITEM_Y, PANEL_CU(2),
			0);
	close_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
			  panel_button_image(panel, "Close", 6, font),
			PANEL_NOTIFY_PROC, close_notify,
			PANEL_ITEM_X, PANEL_CU(9), 
                        PANEL_ITEM_Y, PANEL_CU(1),
			0);
	done_pi = panel_create_item(panel, PANEL_BUTTON,
			PANEL_LABEL_IMAGE,
			  panel_button_image(panel, "Done", 5, font),
			PANEL_NOTIFY_PROC, done_notify,
			PANEL_ITEM_X, PANEL_CU(1),
                        PANEL_ITEM_Y, PANEL_CU(1),
			0);
	defxform_pi = panel_create_item(panel, PANEL_CHOICE,   
                        PANEL_LABEL_STRING, "Def Transform",
			PANEL_CHOICE_STRINGS,
			    "1", "2", "3", "4", 0,
			PANEL_DISPLAY_LEVEL, PANEL_NONE,
                        PANEL_FEEDBACK, PANEL_NONE,
                        PANEL_NOTIFY_PROC, defxform_notify,
                        PANEL_SHOW_MENU_MARK, FALSE,
                        PANEL_SHOW_ITEM, FALSE,
			PANEL_ITEM_X, PANEL_CU(61),
			PANEL_ITEM_Y, PANEL_CU(1),
			0);
	definit_pi = panel_create_item(panel, PANEL_BUTTON,    
                        PANEL_LABEL_IMAGE,
			  panel_button_image(panel, "Def initial", 12, font),
                        PANEL_NOTIFY_PROC, definit_notify,
			PANEL_ITEM_X, PANEL_CU(41), 
                        PANEL_ITEM_Y, PANEL_CU(1),
			0);
	defopencurve_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "Def Open Curve",15, font),
                        PANEL_NOTIFY_PROC, defopencurve_notify,
			PANEL_ITEM_X, PANEL_CU(31),
                        PANEL_ITEM_Y, PANEL_CU(1),
                        0);
	defclosedcurve_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel,"Def Closed Curve",17,font),
                        PANEL_NOTIFY_PROC, defclosedcurve_notify,
			PANEL_ITEM_X, PANEL_CU(51), 
                        PANEL_ITEM_Y, PANEL_CU(1),
                        0);
        clearall_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "Clear all", 10, font),
                        PANEL_NOTIFY_PROC, clearall_notify,
			PANEL_ITEM_X, PANEL_CU(1),
			PANEL_ITEM_Y, PANEL_CU(2)+5,
                        0);
        clearlastpoint_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel,"Clear last point",17,font),
                        PANEL_NOTIFY_PROC, clearlastpoint_notify,
			PANEL_ITEM_X, PANEL_CU(28), 
                        PANEL_ITEM_Y, PANEL_CU(2)+5,
                        0);
	clearlastcurve_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                         panel_button_image(panel,"Clear last strand",18,font),
                        PANEL_NOTIFY_PROC, clearlastcurve_notify,
			PANEL_ITEM_X, PANEL_CU(55), 
                        PANEL_ITEM_Y, PANEL_CU(2)+5,
                        0);
	grid_pi = panel_create_item(panel, PANEL_CHOICE,
                        PANEL_LABEL_STRING, "Grid",
                        PANEL_CHOICE_STRINGS,
                            "None", "Square", /*"Triangle",*/ 0,
			PANEL_MENU_CHOICE_IMAGES,
			    menu_empty_pr, menu_square_pr, /*menu_triangle_pr,*/ 0,
			PANEL_DISPLAY_LEVEL, PANEL_NONE,
                        PANEL_FEEDBACK, PANEL_NONE,
                        PANEL_NOTIFY_PROC, grid_notify,
                        PANEL_SHOW_MENU_MARK, FALSE,
                        PANEL_SHOW_ITEM, FALSE,
			PANEL_ITEM_X, PANEL_CU(28),  
                        PANEL_ITEM_Y, PANEL_CU(3)+10,
                        0);
	connectstart_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel,"Connect to Start",17,font),
			PANEL_NOTIFY_PROC, connectstart_notify,
			PANEL_SHOW_ITEM, FALSE,
			PANEL_ITEM_X, PANEL_CU(28),
			PANEL_ITEM_Y, PANEL_CU(1),
			0);
	newstrand_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "New Strand", 11, font),
                        PANEL_NOTIFY_PROC, newstrand_notify,
                        PANEL_SHOW_ITEM, FALSE,
			PANEL_ITEM_X, PANEL_CU(55),
                        PANEL_ITEM_Y, PANEL_CU(1),
			0);
	restart_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "Restart", 8, font),
			PANEL_NOTIFY_PROC, restart_notify,
			PANEL_SHOW_ITEM, FALSE,
			PANEL_ITEM_X, PANEL_CU(21),
			PANEL_ITEM_Y, PANEL_CU(1),
                        0);
	save_pi = panel_create_item(panel, PANEL_BUTTON,
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "Save", 5, font),
                        PANEL_NOTIFY_PROC, save_notify,
                        PANEL_SHOW_ITEM, FALSE,
                        PANEL_ITEM_X, PANEL_CU(1),
                        PANEL_ITEM_Y, PANEL_CU(6),
			0);
	restore_pi = panel_create_item(panel, PANEL_BUTTON, 
                        PANEL_LABEL_IMAGE,
                          panel_button_image(panel, "Restore", 8, font),
                        PANEL_NOTIFY_PROC, restore_notify, 
                        PANEL_SHOW_ITEM, FALSE, 
                        PANEL_ITEM_X, PANEL_CU(21), 
                        PANEL_ITEM_Y, PANEL_CU(6), 
                        0);
	filename_pi = panel_create_item(panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "File:",  
                        PANEL_SHOW_ITEM, FALSE,  
                        PANEL_ITEM_X, PANEL_CU(41),  
                        PANEL_ITEM_Y, PANEL_CU(6),  
                        0);
}




