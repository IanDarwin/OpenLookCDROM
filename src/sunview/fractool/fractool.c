#include <stdio.h>
#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include <math.h>
#include "fractool.h"





struct rules_hdr *first_rh;
struct rules_hdr rh_array[4];
struct curve *free_curve_list, *fractal, *init_curve;
struct endpoint *free_endpoint_list, *selected_dot;
struct endpoint *pick_init_dot();
struct rule *free_rule_list;
struct curve_rules *free_cr_list;
int rule_ep0_x, rule_ep0_y, rule_ep1_x, rule_ep1_y;
int first_x, first_y, last_x, last_y;
int current_segment_closed;
int connect_flags;
struct curve_rules *forming_rule;
int working_on_a_new_curve;
int slider_values[4];
int curvetype, rulenum;
struct curve_rules *accumulated_rules, *rule_in_progress; 
int rulegrid_type, initgrid_type;
int fastmath;

struct tool *tool;
struct toolsw *panel_subwindow, *mainsw;
struct toolsw *gfxsw_createtoolsubwindow();
Panel panel;
Panel_item iterate_pi, slider1_pi, slider2_pi, slider3_pi, slider4_pi;
Panel_item connect_pi, close_pi, done_pi, clearall_pi;
Panel_item clearlastcurve_pi, clearlastpoint_pi, defxform_pi, definit_pi;
Panel_item defopencurve_pi, defclosedcurve_pi;
Panel_item grid_pi;
Panel_item newstrand_pi, connectstart_pi, restart_pi;
Panel_item save_pi, restore_pi, filename_pi;
int iterate_notify();
int slider1_notify(), slider2_notify(), slider3_notify(), slider4_notify();
int connect_notify(), close_notify(), done_notify(), clearall_notify();
int clearlastcurve_notify(), clearlastpoint_notify();
int defxform_notify(), definit_notify(), newcurve_notify();
int sigwinchcatcher(), mainsw_selected(), mainsw_sigwinch();
struct pixwin *main_pixwin;
struct pixfont *font;
extern struct pixfont *pw_pfsysopen();
struct inputevent mainsw_ie;
struct inputmask mainsw_im;

struct pixrect *mem_create();
struct curve *new_curve(), *expand_one_seg(), *iterate();
struct curve *expand_curve(), *apply_one_ruleset();
struct curve *append_curve();
struct rule *new_rule();
struct endpoint *new_endpoint(), *compute_endpoint_according_to_rule();
float endpoints_to_angle();
extern double sqrt(), sin(), cos(), atan2(), hypot();
extern double my_sin(), my_cos();
struct curve_rules init_cr0, init_cr1;
int state;
double step, sines[N_SINES], base_length, base_angle;
struct pixrect *squaregrid_pr, *triangrid_pr;
struct pixrect *menu_empty_pr, *menu_square_pr, *menu_triangle_pr;
int redrawing, start_over;
struct pixrect *mem_image;

static short icon_data[] = {
#include "icon"
};
mpr_static(ic_mpr, 64, 64, 1, icon_data);
static  struct icon icon = {64, 64, (struct pixrect *)0, 0, 0, 64, 64,
            &ic_mpr, 0, 0, 0, 0, (char *)0, (struct pixfont *)0,
            ICON_BKGRDGRY};


main(argc, argv)
int argc;
char **argv;
{
	struct curve *crv, *new_crv, *newest_curve;

        if (argc != 1   &&   argc != 3) {
		printf("Usage: %s [-a listfilename]\n", argv[0]);
		exit(1);
	}
	if (argc == 3   &&   (argv[1][0] != '-' && argv[1][1] != 'a')) { 
                printf("Usage: %s [-a listfilename]\n", argv[0]); 
                exit(1);    
        }

	redrawing = start_over = 0;
	mem_image = NULL;

	free_curve_list = NULL;
	free_endpoint_list = NULL;
	free_cr_list = NULL;
	connect_flags = 3;

	init_default_curve();
	init_rules();
	init_sines();
	rulegrid_type = initgrid_type = NONE;
	init_pixrects();
	state = RUNNING;

	if (argc == 3)
		auto_run(argv[2]);
	
	font = pw_pfsysopen();
	tool = tool_make(WIN_LABEL, "FracTool",
			 WIN_ICON, &icon,
			 WIN_HEIGHT, 700,
			 0);
	panel_subwindow = panel_create(tool, 0);
        panel = (Panel)panel_subwindow->ts_data;
	
	init_panel_items(font);
	panel_fit_height(panel);
        redraw_panel();

	mainsw = tool_createsubwindow(tool, "",
          TOOL_SWEXTENDTOEDGE,TOOL_SWEXTENDTOEDGE);
        mainsw->ts_io.tio_handlesigwinch = mainsw_sigwinch;
        mainsw->ts_io.tio_selected = mainsw_selected;
        main_pixwin = pw_open(mainsw->ts_windowfd);
        input_imnull(&mainsw_im);
        win_setinputcodebit(&mainsw_im, MS_LEFT);
	win_setinputcodebit(&mainsw_im, MS_MIDDLE);
	mainsw_im.im_flags |= IM_NEGEVENT;
        win_setinputmask(mainsw->ts_windowfd, &mainsw_im, NULL, WIN_NULLLINK);

        signal(SIGWINCH,sigwinchcatcher);
	tool_install(tool);
	tool_select(tool,0);
	tool_destroy(tool);
}




sigwinchcatcher()
{
        tool_sigwinch(tool);
}        


mainsw_sigwinch()
{
        pw_damaged(main_pixwin);
	redraw_main_pixwin();
	pw_donedamaged(main_pixwin);
}


	
display_curve(curve, dotflag)
struct curve *curve;
int dotflag;
{
	struct endpoint *e;
	int x0, x1, y0, y1;
	struct rect rect;
	struct curve *c;

Disp:
	redrawing = 1;
	if (mem_image != NULL)
		pr_destroy(mem_image);
	win_getsize(mainsw->ts_windowfd, &rect);
	mem_image = mem_create(rect.r_width, rect.r_height, 1);

	c = curve;
	while (c) {
                e = c->start;
		if (e != NULL) {
			x0 = e->x;
			y0 = e->y;
			e = e->next;
			if (e == NULL) {
				if (dotflag == TRUE)
                                        draw_a_rect_dot(x0, y0);
				c = c->next;
				continue;
			}
			x1 = e->x;
       		        y1 = e->y;
       		        while(e) {
				pr_vector(mem_image, x0, y0, x1, y1,
				  PIX_SET, 1);
				if (dotflag == TRUE) {
					draw_a_rect_dot(x0, y0);
					draw_a_rect_dot(x1, y1);
				}
				x0 = x1;
				y0 = y1;
				e = e->next;
				if (e) {
					x1 = e->x;
					y1 = e->y;
				}
			}
		}
		c = c->next;
	}

	redrawing = 0;
	if (start_over != 0) {
		start_over = 0;
		goto Disp;
	}

	pw_rop(main_pixwin, 0, 0, rect.r_width, rect.r_height,
	       PIX_SRC, mem_image, 0, 0);
}

			

/*
 *
 *	Appends the curve pointed to by "curve" to the
 * curve pointed to by "list".  Works best when curve
 * is shorter than list.
 *	Returns a pointer to the composite list.
 *
 */

struct curve *append_curve(curve, list)
struct curve *curve, *list;
{
	struct curve *c;

	c = curve;
	if (c == NULL)
		return(list);
	while (c->next != NULL)
		c = c->next;
	c->next = list;
	return(curve);
}






/*
 *
 * 	Iterates the list of curves pointed to by "fractal",
 * according to the set of rules pointed to by "first_rh".
 * Returns a pointer to a new list of curves.
 *
 */

struct curve *iterate(fractal)
struct curve *fractal;
{
	struct curve *new_fractal, *current_curve, *returned_curve;
        int i;
        long total_all_sliders, last_prob;
        struct rules_hdr *rhp;

	/* Make first_rh point to list of applicable rules. */
        first_rh = NULL;
        total_all_sliders = 0;
        for (i=0; i<4; i++)
                if (rh_array[i].rules != NULL &&
                    slider_values[i] > 0) {
                        total_all_sliders += slider_values[i];
                        rh_array[i].next = first_rh;
                        first_rh = &rh_array[i];
                        rh_array[i].probability = slider_values[i];
                }
        if (first_rh == NULL)
                return(NULL);
        rhp = first_rh;
        last_prob = 0;
        while (rhp != NULL) {
                rhp->probability =
                  last_prob + ((rhp->probability * 32768) / total_all_sliders);
                last_prob = rhp->probability;
                rhp = rhp->next;
        }

	new_fractal = NULL;
	current_curve = fractal;
	while (current_curve != NULL) {
		returned_curve = expand_curve(current_curve);
		new_fractal = append_curve(returned_curve, new_fractal);
		current_curve = current_curve->next;
	}
	return(new_fractal);
}



/*
 *
 *	Expands the curve pointed to by input_curve into a linked list
 * of curves ("expansion"), a pointer to which is returned.  A curve is
 * a connected set of line segments, and may be either open or closed.
 *
 */

struct curve *expand_curve(input_curve)
struct curve *input_curve;
{
	struct curve *expansion, *expanded_seg;
	struct endpoint *ep0, *ep1;

	ep0 = input_curve->start;
	if (ep0 == NULL)
		return(NULL);
	ep1 = ep0->next;
	expansion = NULL;
	while (ep1 != NULL) {
		expanded_seg = expand_one_seg(ep0, ep1);
		expansion = append_curve(expanded_seg, expansion);
		ep0 = ep1;
		ep1 = ep1->next;
	}
	return(expansion);
}





struct curve *expand_one_seg(from, to)
struct endpoint *from, *to;
{
	long toss;
	struct rules_hdr *rh;
	struct curve_rules *rules;
        struct curve *curve_in_progress, *c;
	float x0, x1, y0, y1;
        
	x0 = from->x;
	y0 = from->y;
	x1 = to->x;
	y1 = to->y;
	base_length = hypot((x0-x1), (y0-y1));
        base_angle = atan2(y1-y0, x1-x0);

	curve_in_progress = NULL;

	rh = first_rh;
	if (rh->probability < 32760) {
		toss = random() & 32767;
		while (toss > rh->probability)
			rh = rh->next;
	}
	rules = rh->rules;

	while (rules != NULL) {
		c = apply_one_ruleset(from, to, rules);
		curve_in_progress = append_curve(c, curve_in_progress);
		rules = rules->next;
	}
	return(curve_in_progress);
}


	


struct curve *apply_one_ruleset(from, to, ruleset)
struct endpoint *from, *to;
struct curve_rules *ruleset;
{
        struct curve *c;
	struct rule *current_rule;
	struct endpoint *first_ep, *last_ep, *junk_ep, *terminal_ep;

	first_ep = new_endpoint();
	first_ep->x = from->x;
	first_ep->y = from->y;
	last_ep = first_ep;
	current_rule = ruleset->start;

	while (current_rule != NULL) {
		last_ep->next = compute_endpoint_according_to_rule(first_ep,
		  current_rule);
		last_ep = last_ep->next;
		current_rule = current_rule->next;
	}

	if ((ruleset->flags & CONNECT_INITIAL) == 0) {
		junk_ep = first_ep;
		first_ep = first_ep->next;
		free_endpoint(junk_ep);
	}

	if (ruleset->flags & CONNECT_TERMINAL) {
		terminal_ep = new_endpoint();
		terminal_ep->x = to->x;
		terminal_ep->y = to->y;
		terminal_ep->next = NULL;
		last_ep->next = terminal_ep;
	}
	else
		last_ep->next = NULL;

        c = new_curve();
        c->next = NULL;
	c->start = first_ep;
	return(c);
}





struct endpoint *compute_endpoint_according_to_rule(ep0, rule)
struct endpoint *ep0;
struct rule *rule;
{
	float total_angle, scale_factor;
	struct endpoint *new_ep;

	new_ep = new_endpoint();
	scale_factor = base_length * rule->rho;
	total_angle = base_angle + rule->theta;
	new_ep->x = ep0->x  +  scale_factor * my_cos(total_angle);
	new_ep->y = ep0->y  +  scale_factor * my_sin(total_angle);
	return(new_ep);
}



mainsw_selected()
{
	int x, y;

        if (input_readevent(mainsw->ts_windowfd, &mainsw_ie) != 0)
                return(-1);
	x = mainsw_ie.ie_locx;
	y = mainsw_ie.ie_locy;

	if (state == DEFINING_XFORM)
		if (mainsw_ie.ie_code == MS_LEFT &&
		    mainsw_ie.ie_flags != IE_NEGEVENT)
			new_dot_in_rule(x, y);

	if (state == DEFINING_INIT)
		if (mainsw_ie.ie_code == MS_LEFT &&
		    mainsw_ie.ie_flags != IE_NEGEVENT)
                        new_dot_in_init_curve(x, y);
		else if (mainsw_ie.ie_code == MS_MIDDLE)
			if (mainsw_ie.ie_flags != IE_NEGEVENT)
				selected_dot = pick_init_dot(x, y);
			else
				drop_init_dot(x, y);
}

