#include <stdio.h>
#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include "fractool.h"
#include <math.h>



extern int rule_ep0_x, rule_ep0_y, rule_ep1_x, rule_ep1_y;
extern int first_x, first_y, last_x, last_y;
extern int current_segment_closed, connect_flags;
extern int state, curvetype;
extern int rulegrid_type, initgrid_type;
extern struct curve_rules *rule_in_progress, *accumulated_rules;
extern struct pixwin *main_pixwin;
extern struct curve *init_curve;
extern struct endpoint *selected_dot;
extern double hypot();

struct curve_rules *new_curverules();
struct rule *coords_to_rule(), *new_rule();
struct endpoint *new_endpoint();



new_dot_in_rule(x, y)
int x, y;
{
	struct rule *r, *rptr;
	int middle;

	if (curvetype == NONE)
		return(0);

	snap_to_grid(&x, &y, rule_ep0_x, rule_ep0_y);

	draw_a_dot(x, y);
	r = coords_to_rule(x, y);
	r->next = NULL;

	if (rule_in_progress->start == NULL) {
		rule_in_progress->start = r;
		first_x = x;
		first_y = y;
		if (connect_flags & CONNECT_INITIAL)
			pw_vector(main_pixwin, rule_ep0_x, rule_ep0_y,
			  x, y, PIX_SET, 1);
	}
	else {
		rptr = rule_in_progress->start;
		while (rptr->next != NULL)
			rptr = rptr->next;
		rptr->next = r;
		pw_vector(main_pixwin, last_x, last_y, x, y, PIX_SET, 1);
	}

	last_x = x;
	last_y = y;
}



struct rule *coords_to_rule(x, y)
int x, y;
{
	struct rule *r;
	double base, len, dx, dy;

	r = new_rule();

	base = (double)(rule_ep1_x - rule_ep0_x);
	dy = (double)(y - rule_ep0_y);
	dx = (double)(x - rule_ep0_x);
	len = hypot(dy, dx);
	r->rho = len / base;
	r->theta = atan2(dy, dx);
	while (r->theta < 0)
		r->theta += TWO_PI;

	return(r);
}



print_rules(cr)
struct curve_rules *cr;
{
	struct rule *r;

	while(cr) {
                printf("NEXT RULE: \n");
		printf("flags = %d\n", cr->flags);
                r = cr->start;
                while(r) {
                        printf("\tRho= %f\tTheta = %f\n", r->rho, r->theta);
                        r = r->next;
                }        
                cr = cr->next;
	}
}



closest_of_3(n, a, b, c)
int n, a, b, c;
{
	int d1, d2, d3;

	d1 = abs(n - a);
	d2 = abs(n - b);
	d3 = abs(n-c);

	if ((d1 < d2) && (d1 < d3))
		return(a);
	else if (d2 < d3)
		return(b);
	else
		return(c);
}


get_first_ep_in_init_curve(xp, yp)
int *xp, *yp;
{
	struct curve *c;
 
        c = init_curve;
        while (c != NULL && c->start == NULL)
                c = c->next;
        if (c == NULL)
		*xp = *yp = NULL;
	else {
		*xp = c->start->x;
		*yp = c->start->y;
	}
}




new_dot_in_init_curve(x, y)
int x, y;
{
	struct endpoint *ep, *ep1;
	int refx, refy;

	get_first_ep_in_init_curve(&refx, &refy);

	if (initgrid_type != NONE)
		snap_to_grid(&x, &y, refx, refy);
	draw_a_dot(x, y);

	ep = new_endpoint();
	ep->x = x;
	ep->y = y;
	ep->next = NULL;

	if (init_curve == NULL)
		newstrand_notify();

	if (init_curve->start == NULL)
		init_curve->start = ep;
	else {
		ep1 = init_curve->start;
		while(ep1->next != NULL)
			ep1 = ep1->next;
		ep1->next = ep;
		pw_vector(main_pixwin, x, y, (int)ep1->x, (int)ep1->y,
		  PIX_SET, 1);
	}
}



snap_to_grid(xp, yp, refx, refy)
int *xp, *yp, refx, refy;
{
	int middle, gtype;

	if (state == DEFINING_XFORM)
		gtype = rulegrid_type;
	else if (state == DEFINING_INIT)
		gtype = initgrid_type;
	else
		return(0);

	if (gtype == SQUARE) {
                middle = refx + (((*xp - refx) / GRIDSIZE) * GRIDSIZE);
                *xp = closest_of_3(*xp, middle, middle + GRIDSIZE, middle - GRIDSIZE);
                middle = refy + (((*yp - refy) / GRIDSIZE) * GRIDSIZE);
                *yp = closest_of_3(*yp, middle, middle + GRIDSIZE, middle - GRIDSIZE);
        }
}




struct endpoint *pick_init_dot(x, y)
int x, y;
{
	struct curve *c;
	struct endpoint *ep, *closest_ep;
	int dist, min_dist, dx, dy;

	closest_ep = NULL;
	min_dist = 10000000;

	c = init_curve;
	while (c != NULL) {
		if (c->start != NULL) {
			ep = c->start;
			while (ep != NULL) {
				dx = x - (int)ep->x;
				dy = y - (int)ep->y;
				dist = dx*dx + dy*dy;
				if (dist < min_dist) {
					min_dist = dist;
					closest_ep = ep;
				}
				ep = ep->next;
			}
		}
		c = c->next;
	}

	if (min_dist > 256)
		return(NULL);
	else {
		if (closest_ep != NULL)
			hollow_dot((int)closest_ep->x, (int)closest_ep->y);
		return(closest_ep);
	}
}




drop_init_dot(x, y)
int x, y;
{
	struct curve *c;
	struct endpoint *ep;
	float selected_x, selected_y;
	int refx, refy;

	if (selected_dot == NULL)
		return(0);

	selected_x = selected_dot->x;
	selected_y = selected_dot->y;
	get_first_ep_in_init_curve(&refx, &refy);

	c = init_curve;
	while (c != NULL) {
		ep = c->start;
		while (ep != NULL) {
			if (ep->x == selected_x &&
			    ep->y == selected_y) {
				if (initgrid_type != NONE)
					snap_to_grid(&x, &y, refx, refy);
                                ep->x = x;
                                ep->y = y;
			}
			ep = ep->next;
		}
		c = c->next;
	}

	redraw_main_pixwin();
}

