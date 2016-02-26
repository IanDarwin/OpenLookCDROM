/*
 *      These routines save and restore to/from files.
 * The file format is:
 *
 * INITIAL SHAPE:
 *  Curve:
 *   x0
 *   y0
 *   x1
 *   y1
 *  Done curve.
 *  Curve:
 *   x2
 *   y2
 *   x3
 *   y3
 *   x4
 *   y4
 *  Done curve.
 * DONE INITIAL SHAPE.
 * RULES
 * 2 rule(s)
 *  Rule 0:
 *   Slider = 91
 *   Rule:
 *    Flags = 3
 *    Rho = xxxxx
 *    Theta = xxxxx
 *    Rho = xxxxx
 *    Theta = xxxxx
 *   End rule.
 *   Rule: 
 *    Flags = 1
 *    Rho = xxxxx
 *    Theta = xxxxx 
 *    Rho = xxxxx 
 *    Theta = xxxxx 
 *   End rule.
 *  Done Rule 0.
 *  Rule 2:
 *   Slider = 65
 *   Rule: 
 *    Flags = 2
 *    Rho = xxxxx
 *    Theta = xxxxx 
 *    Rho = xxxxx 
 *    Theta = xxxxx
 *    Rho = xxxxx 
 *    Theta = xxxxx  
 *   End rule.
 *  Done Rule 2
 * DONE ALL RULES.
 *
 */



#include <stdio.h>
#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include "fractool.h"


extern int slider_values[];
extern struct rules_hdr rh_array[];
extern struct curve *init_curve;
extern Panel_item slider1_pi, slider2_pi, slider3_pi, slider4_pi;

extern struct curve *new_curve();
extern struct endpoint *new_endpoint();
extern struct curve_rules *new_curverules();
extern struct rule *new_rule();
struct curve *restore_initial_strand();


save(filename)
char *filename;
{
	FILE *f;

	f = fopen(filename, "w");
	if (f == NULL)
		return(0);

	save_initial(f);
	save_rules(f); 
	fclose(f);
}


save_initial(f)
FILE *f;
{
	struct curve *c;

	fputs("INITIAL SHAPE:\n", f);
	c = init_curve;
	while (c != NULL) {
		save_init_strand(f, c);
		c = c->next;
	}
	fputs("DONE INITIAL SHAPE.\n", f);
}


save_init_strand(f, c)
FILE *f;
struct curve *c;
{
	struct endpoint *e;

	e = c->start;
	if (e == NULL)
		return(0);
	fputs(" Curve:\n", f);
	while (e != NULL) {
		fprintf(f, "  %f\n", e->x);
		fprintf(f, "  %f\n", e->y);
		e = e->next;
	}
	fputs(" Done curve.\n", f);
}



save_rules(f)
FILE *f;
{
	int i, nrules;

	fputs("RULES:\n", f);

	nrules = 0;
	for (i=0; i<=3; i++)
		if (rh_array[i].rules != NULL)
			nrules++;

	fprintf(f, "%d rule(s)\n", nrules);

	for (i=0; i<=3; i++) 
                if (rh_array[i].rules) 
			save_a_rule(f, i);

	fputs("DONE.\n", f);
}


save_a_rule(f, rulenum)
FILE *f;
int rulenum;
{
	struct curve_rules *cr;

	fprintf(f, " Rule %d:\n", rulenum);
	fprintf(f, " Slider = %d\n", slider_values[rulenum]);
	cr = rh_array[rulenum].rules;
	while (cr != NULL) {
		save_rulestrand(f, cr);
		cr = cr->next;
	}
	fprintf(f, " End rule %d:\n", rulenum);
}



save_rulestrand(f, cr)
FILE *f;
struct curve_rules *cr;
{
	struct rule *rule;

	rule = cr->start;
	if (rule == NULL)
		return(0);

	fprintf(f, "  Rule:\n");
	fprintf(f, "   Flags = %d\n", cr->flags);

	while (rule != NULL) {
		fprintf(f, "   Rho = %f\n", rule->rho);
		fprintf(f, "   Theta = %f\n", rule->theta);
		rule = rule->next;
	}

	fprintf(f, "  End Rule.\n");
}


/* Returns 0 if successful, -1 if trouble. */
restore(filename)
char *filename;
{
	FILE *f;

	f = fopen(filename, "r");
	if (f == NULL)
		return(-1);
	if (restore_initial(f) == -1) {
		fclose(f);
		return(-1);
	}
	restore_rules(f);
	fclose(f);
	return(0);
}


restore_initial(f)
FILE *f;
{
	char buffer[80];
	int done;
	struct curve *c, *curve;

	if (fgets(buffer, 80, f) == NULL)
		return(-1);
	if (strncmp(buffer, "INITIAL SHAPE", 13) != 0)
		return(-1);

	/* At this point we trust the file format. */
	init_curve = NULL;
	done = 0;
	while (done == 0) {
		c = restore_initial_strand(f);
		if (c == NULL)
			done = 1;
		else {			/* Put c at end of init_curve list */
			c->next = NULL;
			if (init_curve == NULL)
				init_curve = c;
			else {
				curve = init_curve;
				while (curve->next != NULL)
					curve = curve->next;
				curve->next = c;
			}
		}
	}
}



struct curve *
restore_initial_strand(f)
FILE *f;
{
        char buffer[80];
        int done;
	struct curve *c;
	struct endpoint *ep, *ep1;

	fgets(buffer, 80, f);
	if (buffer[0] == 'D')
		return(NULL);

	c = new_curve();
	c->next = NULL;
	c->start = NULL;
	done = 0;
	while (done == 0) {
		fgets(buffer, 80, f);
		if (buffer[1] == 'D')
			done = 1; 
                else {
			ep = new_endpoint();
			ep->next = NULL;
			sscanf(&buffer[2], "%f", &(ep->x));
			fgets(buffer, 80, f);
			sscanf(&buffer[2], "%f", &(ep->y));
			if (c->start == NULL)
				c->start = ep;
			else {
				ep1 = c->start;
				while (ep1->next !=NULL)
					ep1 = ep1->next;
				ep1->next = ep;
			}
		}
	}

	return(c);
}



restore_rules(f)
FILE *f;
{
	int i, nrules;
	char buffer[80];

	for (i=0; i<=3; i++) {
		slider_values[i] = 0;
		rh_array[i].rules = NULL;
	}

	fgets(buffer, 80, f);		/* "RULES" */
	fgets(buffer, 80, f);           /* "n rules" */
	nrules = buffer[0] - '0';
	while (nrules > 0) {
		restore_one_rule(f);
		nrules--;
	}
}



restore_one_rule(f)
FILE *f;
{
        int rulenum, done, slider_val;
        char buffer[80];

	fgets(buffer, 80, f);           /* " Rule n" */
	rulenum = buffer[6] - '0';
	fgets(buffer, 80, f);           /* " Slider = xxxx" */
	slider_val = atoi(&buffer[10]);
	slider_values[rulenum] = slider_val;
	if (rulenum == 0)
		panel_set(slider1_pi, PANEL_VALUE, slider_val, 0);
	else if (rulenum == 1) 
                panel_set(slider2_pi, PANEL_VALUE, slider_val, 0);
        else if (rulenum == 2)  
                panel_set(slider3_pi, PANEL_VALUE, slider_val, 0);
        else if (rulenum == 3)  
                panel_set(slider4_pi, PANEL_VALUE, slider_val, 0);

	done = 0;
	while (done != 1) {
		fgets(buffer, 80, f);           /* "  Rule" or " End rule n" */
		if (buffer[1] == 'E')
			done = 1;
		else
			restore_one_curverule(f, rulenum);
	}
}


restore_one_curverule(f, rulenum)
FILE *f;
int rulenum;
{
	int done; 
        char buffer[80];
	struct curve_rules *cr, *cr1;
	struct rule *r, *r1;

	cr = new_curverules();
	cr->next = NULL;
	cr->start = NULL;

	fgets(buffer, 80, f);           /* "   Flags = x" */
	cr->flags = buffer[11] - '0';

	done = 0;
	while (done != 1) {
		fgets(buffer, 80, f);   /* "   Rho = xxx" or "  End" */
		if (buffer[2] == 'E')
			done = 1;
		else {
			r = new_rule();
			r->next = NULL;
			sscanf(&buffer[9], "%f", &(r->rho));
			fgets(buffer, 80, f);   /* "   Theta = xxx" */
			sscanf(&buffer[11], "%f", &(r->theta));
			if (cr->start == NULL)
				cr->start = r;
			else {
				r1 = cr->start;
				while (r1->next != NULL)
					r1 = r1->next;
				r1->next = r;
			}
		}
	}

	if (rh_array[rulenum].rules == NULL)
		rh_array[rulenum].rules = cr;
	else {
		cr1 = rh_array[rulenum].rules;
		while (cr1->next != NULL)
			cr1 = cr1->next;
		cr1->next = cr;
	}
}

