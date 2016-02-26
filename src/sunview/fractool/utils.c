#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include "fractool.h"
#include <stdio.h>




extern Panel_item slider1_pi, slider2_pi, slider3_pi, slider4_pi;
extern Panel_item connect_pi, close_pi, done_pi, clearall_pi;
extern Panel_item clearlastpoint_pi, defxform_pi, defxform_pi, definit_pi;
extern Panel_item clearlastcurve_pi, iterate_pi;
extern Panel_item defopencurve_pi, defclosedcurve_pi;
extern Panel_item grid_pi;
extern Panel_item newstrand_pi, connectstart_pi, restart_pi;
extern Panel_item save_pi, restore_pi, filename_pi;
extern int state, initgrid_type, rulegrid_type, rulenum;
extern int rule_ep0_x, rule_ep0_y, rule_ep1_x, rule_ep1_y;
extern struct pixwin *main_pixwin;
extern struct pixrect *squaregrid_pr, *triangrid_pr, *mem_image;
extern struct toolsw *mainsw;
extern struct rules_hdr *first_rh, rh_array[];
extern struct curve *init_curve, *fractal;
extern int redrawing, start_over;
extern struct curve_rules *accumulated_rules;

extern struct curve *new_curve(), *expand_curve();
extern struct endpoint *new_endpoint();



all_panel_items_off()
{
        panel_set(iterate_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(slider1_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(slider2_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(slider3_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(slider4_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(connect_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(close_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(done_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(clearall_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(clearlastcurve_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(clearlastpoint_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(defxform_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(definit_pi, PANEL_SHOW_ITEM, FALSE, 0);
	panel_set(defopencurve_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(defclosedcurve_pi, PANEL_SHOW_ITEM, FALSE, 0);
	panel_set(grid_pi, PANEL_SHOW_ITEM, FALSE, 0);
	panel_set(newstrand_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(connectstart_pi, PANEL_SHOW_ITEM, FALSE, 0); 
        panel_set(restart_pi, PANEL_SHOW_ITEM, FALSE, 0);
	panel_set(save_pi, PANEL_SHOW_ITEM, FALSE, 0);
        panel_set(restore_pi, PANEL_SHOW_ITEM, FALSE, 0); 
        panel_set(filename_pi, PANEL_SHOW_ITEM, FALSE, 0);
}



redraw_panel()
{
	all_panel_items_off();

	if (state == RUNNING) {
		panel_set(iterate_pi, PANEL_SHOW_ITEM, TRUE, 0);
	        panel_set(slider1_pi, PANEL_SHOW_ITEM, TRUE, 0);
       		panel_set(slider2_pi, PANEL_SHOW_ITEM, TRUE, 0);
       		panel_set(slider3_pi, PANEL_SHOW_ITEM, TRUE, 0);
       		panel_set(slider4_pi, PANEL_SHOW_ITEM, TRUE, 0);
		panel_set(defxform_pi, PANEL_SHOW_ITEM, TRUE, 0);
        	panel_set(definit_pi, PANEL_SHOW_ITEM, TRUE, 0);
		panel_set(restart_pi, PANEL_SHOW_ITEM, TRUE, 0);
		panel_set(save_pi, PANEL_SHOW_ITEM, TRUE, 0);
                panel_set(restore_pi, PANEL_SHOW_ITEM, TRUE, 0);
                panel_set(filename_pi, PANEL_SHOW_ITEM, TRUE, 0);
	}
	else if (state == DEFINING_XFORM) {
		panel_set(connect_pi, PANEL_SHOW_ITEM, TRUE, 0);
        	panel_set(close_pi, PANEL_SHOW_ITEM, TRUE, 0);
        	panel_set(done_pi, PANEL_SHOW_ITEM, TRUE, 0);
/**********************************************************************
        	panel_set(clearall_pi, PANEL_SHOW_ITEM, TRUE, 0);
        	panel_set(clearlastcurve_pi, PANEL_SHOW_ITEM, TRUE, 0);
        	panel_set(clearlastpoint_pi, PANEL_SHOW_ITEM, TRUE, 0);
************************************************************************/
       		panel_set(defopencurve_pi, PANEL_SHOW_ITEM, TRUE, 0);
	        panel_set(defclosedcurve_pi, PANEL_SHOW_ITEM, TRUE, 0);
		panel_set(grid_pi, PANEL_SHOW_ITEM, TRUE, 0);

	}
	else if (state == DEFINING_INIT) {
		panel_set(done_pi, PANEL_SHOW_ITEM, TRUE, 0);
		panel_set(newstrand_pi, PANEL_SHOW_ITEM, TRUE, 0); 
                panel_set(connectstart_pi, PANEL_SHOW_ITEM,  TRUE, 0); 
                panel_set(grid_pi, PANEL_SHOW_ITEM, TRUE, 0);  
                panel_set(clearall_pi, PANEL_SHOW_ITEM, TRUE, 0);
                panel_set(clearlastcurve_pi, PANEL_SHOW_ITEM, TRUE, 0);
                panel_set(clearlastpoint_pi, PANEL_SHOW_ITEM, TRUE, 0);
	}
}



draw_a_dot(x, y)
int x, y;
{
	pw_vector(main_pixwin, x-2, y-1, x-2, y+1, PIX_SET, 1);
        pw_vector(main_pixwin, x-1, y-2, x-1, y+2, PIX_SET, 1); 
        pw_vector(main_pixwin, x, y-2, x, y+2, PIX_SET, 1);  
        pw_vector(main_pixwin, x+1, y-2, x+1, y+2, PIX_SET, 1);   
        pw_vector(main_pixwin, x+2, y-1, x+2, y+1, PIX_SET, 1);
}


draw_a_rect_dot(x, y)
int x, y;
{
        pr_vector(mem_image, x-2, y-1, x-2, y+1, PIX_SET, 1);
        pr_vector(mem_image, x-1, y-2, x-1, y+2, PIX_SET, 1);
        pr_vector(mem_image, x, y-2, x, y+2, PIX_SET, 1);
        pr_vector(mem_image, x+1, y-2, x+1, y+2, PIX_SET, 1);
        pr_vector(mem_image, x+2, y-1, x+2, y+1, PIX_SET, 1);
}



orient_for_grid(xp, yp)
int *xp, *yp;
{
	struct curve *c;

	if (state == DEFINING_INIT) {
		if (initgrid_type == SQUARE) {
			c = init_curve;
			while (c != NULL && c->start == NULL)
				c = c->next;
			if (c == NULL)
				*xp = *yp = 0;
			else {
				*xp = (int)c->start->x % GRIDSIZE;
				*yp = (int)c->start->y % GRIDSIZE;
			}
		}
		else if (initgrid_type == TRIANGULAR) {
			*xp = *yp = 0;
		}
	}

	else if (state == DEFINING_XFORM) {
		if (rulegrid_type == SQUARE) {
			*xp = rule_ep0_x % GRIDSIZE;
			*yp = rule_ep0_y % GRIDSIZE;
		}
		else if (rulegrid_type == TRIANGULAR) {
                        *xp = *yp = 0; 
                }
	}
}



draw_square_grid()
{
	struct rect rect;
	int startx, starty, i, j, h_offset, v_offset;

	win_getsize(mainsw->ts_windowfd, &rect);

	orient_for_grid(&startx, &starty);

	pw_replrop(main_pixwin, startx, starty,
	  rect.r_width, rect.r_height,
          PIX_SRC | PIX_DST, squaregrid_pr, 0, 0);
}



draw_triangular_grid()
{
	struct rect rect;
        int startx, starty;

	win_getsize(mainsw->ts_windowfd, &rect);

	orient_for_grid(&startx, &starty);

	pr_replrop(main_pixwin, startx, starty,
	  rect.r_width, rect.r_height,
	  PIX_SRC | PIX_DST, triangrid_pr, 0, 0);
}



struct curve *replicate_curve(original)
struct curve *original;
{
	struct curve *copy, *c, *c1;
	struct endpoint *eps, *epd;

	copy = NULL;

	while (original != NULL) {
		c = new_curve();	/* Append new curve to   */
		if (copy == NULL)	/* to end of "copy" list */
			copy = c;
		else {
			c1 = copy;
			while (c1->next != NULL)
				c1 = c1->next;
			c1->next = c;
		}
		c->next = NULL;
		c->start = NULL;	/* Replicate endpoints */
		if (original->start != NULL) {
			eps = original->start;
			epd = new_endpoint();
			epd->x = eps->x;
			epd->y = eps->y;
			c->start = epd;
			while(eps->next != NULL) {
				epd->next = new_endpoint();
				epd = epd->next;
				eps = eps->next;
				epd->x = eps->x;
				epd->y = eps->y;
			}
			epd->next = NULL;
		}			/* Endpoints now replicated */
		original = original->next;
	}

	return(copy);
}



hollow_dot(x, y)
int x, y;
{
	pw_rop(main_pixwin, x-1, y-1, 3, 3, PIX_CLR, NULL, 0, 0);
	pw_put(main_pixwin, x, y, 1);
}



draw_rule(r, x0, y0, x1, y1)
struct curve_rules *r;
int x0, y0, x1, y1;
{
	struct curve *c, *c_init;
	struct endpoint *ep0, *ep1;
	struct rules_hdr rhdr;

	if (r->start == NULL) {
		clear_main_pixwin();
		draw_a_dot(x0, y0);
		draw_a_dot(x1, y1);
	}
	else {
		c_init = new_curve();	
		c_init->next = NULL;

		rhdr.probability = 65537;
		rhdr.rules = r;
		rhdr.next = NULL;

       		c_init = new_curve();   
       		c_init->next = NULL;
		ep0 = new_endpoint();
		ep1 = new_endpoint();
		c_init->start = ep0;
		ep0->next = ep1;
		ep1->next = NULL;
		ep0->x = x0;
		ep0->y = y0;
		ep1->x = x1;
		ep1->y = y1;
	
		c = expand_curve(c_init);
		display_curve(c, TRUE);
		discard_curve(c);
		discard_curve(c_init);
	}
}



orient_rule_endpoints()		/* Computes rule_ep0_x et al. */
{
	struct rect rect;
	int fifth;

	win_getsize(mainsw->ts_windowfd, &rect);
	fifth = rect.r_width / 5;
	rule_ep0_x = fifth;
	rule_ep1_x = rule_ep0_x + (((3*fifth) / GRIDSIZE) * GRIDSIZE);
        rule_ep0_y = rule_ep1_y = rect.r_height / 2;
}



clear_main_pixwin()
{
	struct rect rect;

	win_getsize(mainsw->ts_windowfd, &rect);
	pw_writebackground(main_pixwin, 0, 0,
	  rect.r_width, rect.r_height, PIX_CLR);
}



redraw_main_pixwin()
{
	if (redrawing != 0) {		/* If interrupt */
		start_over = 1;
		return(0);
	}

	if (state == RUNNING) {
		display_curve(fractal, FALSE);
		return(0);
	}

	else if (state == DEFINING_INIT) {
		display_curve(init_curve, TRUE);
		if (initgrid_type == SQUARE)
			draw_square_grid();
		else if (initgrid_type == TRIANGULAR)
			draw_triangular_grid();
	}

	else if (state == DEFINING_XFORM) {
		clear_main();
		orient_rule_endpoints();
		draw_a_dot(rule_ep0_x, rule_ep0_y);
		draw_a_dot(rule_ep1_x, rule_ep1_y);
		accumulated_rules = NULL;
		/*********************************
                orient_rule_endpoints();
		draw_rule(rh_array[rulenum].rules,
		  rule_ep0_x, rule_ep0_y, rule_ep1_x, rule_ep1_y);
                *********************************/ 
		if (rulegrid_type == SQUARE)
                        draw_square_grid();
		else if (rulegrid_type == TRIANGULAR) 
                        draw_triangular_grid();
	}
}


clear_main()
{
	struct rect rect;

        win_getsize(mainsw->ts_windowfd, &rect);
        pw_writebackground(main_pixwin, 0, 0,
          rect.r_width, rect.r_height, PIX_CLR);
}



