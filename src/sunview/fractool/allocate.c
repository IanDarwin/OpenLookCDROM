#include "fractool.h"

#define NULL    0 


extern struct curve *free_curve_list;
extern struct endpoint *free_endpoint_list;
extern struct rule *free_rule_list;
extern struct curve_rules *free_cr_list;




struct curve *new_curve()
{
        struct curve *nc;
 
        if (free_curve_list == NULL)
                return((struct curve *)malloc(sizeof(struct curve)));
        else {
                nc = free_curve_list;
                free_curve_list = free_curve_list->next;
                return(nc);
        }
}



struct endpoint *new_endpoint()
{
        struct endpoint *ep;
 
        if (free_endpoint_list == NULL)
                return((struct endpoint *)malloc(sizeof(struct endpoint)));
        else {
                ep = free_endpoint_list;
                free_endpoint_list = free_endpoint_list->next;
                return(ep);
        }
}
 
 
 
struct rule *new_rule()
{
        struct rule *r;
 
        if (free_rule_list == NULL)
                return((struct rule *)malloc(sizeof(struct rule)));
        else {
                r = free_rule_list;
                free_rule_list = free_rule_list->next;
                return(r);
        }
}
 
 

struct curve_rules *new_curverules()
{
	struct curve_rules *cr;

	if (free_cr_list == NULL)
		return((struct curve_rules *)malloc(sizeof(struct curve_rules)));
        else {
                cr = free_cr_list;
		free_cr_list = free_cr_list->next;
		return(cr);
	}
}



free_curverule(cr)
struct curve_rules *cr;
{
	cr->next = free_cr_list;
	free_cr_list = cr;
}



 
free_curve(c)
struct curve *c;
{
        c->next = free_curve_list;
        free_curve_list = c;
}
 
 
 
free_endpoint(e)
struct endpoint *e;
{
        e->next = free_endpoint_list;
        free_endpoint_list = e;
}
 

discard_curve(c)
struct curve *c;
{
        struct endpoint *ep;
        struct curve *cstart, *cprev;
 
        if (c == NULL)
                return(0);

	cstart = c;
	while (c != NULL) {
		ep = c->start;
		if (ep != NULL) {
			while (ep->next != NULL)
				ep = ep->next;
			ep->next = free_endpoint_list;
			free_endpoint_list = c->start;
		}
		cprev = c;
		c = c->next;
	}

	cprev->next = free_curve_list;
	free_curve_list = cstart;
}

