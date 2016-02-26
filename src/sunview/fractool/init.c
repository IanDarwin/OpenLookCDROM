#include <stdio.h>
#include <suntool/tool_hs.h>
#include "fractool.h"


extern struct rules_hdr rh_array[], *first_rh;
extern struct curve_rules init_cr0, init_cr1;
extern struct curve *fractal, *init_curve;
extern struct pixrect *squaregrid_pr, *triangrid_pr;
extern struct pixrect *menu_empty_pr, *menu_square_pr, *menu_triangle_pr;

struct curve *new_curve(), *replicate_curve();
struct endpoint *new_endpoint();
struct rule *new_rule(), *numbers_to_rule();
struct curve_rules *new_curverules();


extern struct pixrect *mem_create();


init_default_curve()
{
        struct endpoint *e1, *e2, *e3, *e4;

        fractal = new_curve();
        fractal->next = NULL;

	fractal->start = e1 = new_endpoint();
        e2 = new_endpoint();
        e3 = new_endpoint();
        e4 = new_endpoint();

	e1->x = e4->x = 120;
	e2->x = 480;
	e3->x = 300;
	e1->y = e2->y = e4->y = 110;
	e3->y = 421.76;

        e1->next = e2; 
        e2->next = e3; 
        e3->next = e4; 
        e4->next = NULL;

	init_curve = replicate_curve(fractal);
}


init_rules()
{
        struct rule *r1, *r2, *r3, *nullrule;

	rh_array[0].next = &rh_array[1];
	rh_array[1].next = NULL;

        rh_array[0].rules = &init_cr0;
        init_cr0.next = NULL;
        init_cr0.flags = init_cr1.flags = 3;
        r1 = new_rule();
        r2 = new_rule();
        r3 = new_rule();
        init_cr0.start = r1; 
        r1->next = r2; 
        r2->next = r3; 
        r3->next = NULL;                 
        r1->rho = .3333333333333333;
	r2->rho = 0.57735;
	r3->rho = .6666666666666666;
        r1->theta = 0;
	r2->theta = 11 * (TWO_PI / 12);  /* 30 degrees */
        r3->theta = 0;
        init_cr1.next = NULL;
        nullrule = new_rule();
        nullrule->next = NULL;
        nullrule->rho = 0.55;
        nullrule->theta = 0.149;
        init_cr1.start = nullrule;
        rh_array[1].rules = &init_cr1;

	init_rules_2_and_3();

}

init_rules_2_and_3()
{
	static double rhos1[] = {0.352941,
				 0.647059,
				 0.670691,
				 0.394600,
				 0.352941};
	static double thetas1[] = {0.000000,
				   0.000000,
				   0.266252,
				   0.463648,
				   0.000000};
        static double rhos2[] = {0.294118,
				 0.376654,
				 0.744065,
				 0.705882};
	static double thetas2[] = {0.000000, 
                                   0.674741,
				   0.321751,
				   0.000000};
	struct curve_rules *indent_cr, *carpet_cr1, *carpet_cr2;

	indent_cr = new_curverules();
	indent_cr->start = numbers_to_rule(4, rhos2, thetas2);
	indent_cr->next = NULL;
	indent_cr->flags = 3;
	rh_array[2].rules = indent_cr;

	carpet_cr1 = new_curverules();
        carpet_cr2 = new_curverules();
	carpet_cr1->start = numbers_to_rule(5, rhos1, thetas1);
	carpet_cr2->start = numbers_to_rule(4, rhos2, thetas2);
	carpet_cr1->next = carpet_cr2;
	carpet_cr2->next = NULL;
	carpet_cr1->flags = 0;
	carpet_cr2->flags = 3;
	rh_array[3].rules = carpet_cr1;
}
	

struct rule *numbers_to_rule(len, rhos, thetas)
int len;
double *rhos, *thetas;
{
	struct rule *r, *r1, *return_rule;

	return_rule = NULL;
	while (len-- > 0) {
		r = new_rule();
		r->rho = *rhos;
		rhos++;
		r->theta = *thetas;
		thetas++;
		r->next = NULL;
		if (return_rule == NULL)
			return_rule = r;
		else {
			r1 = return_rule;
			while (r1->next != NULL)
				r1 = r1->next;
			r1->next = r;
		}
	}
	return(return_rule);
}



init_pixrects()
{
	init_squaregrid_pr();
	init_triangrid_pr();
	init_menu_prs();
}



init_squaregrid_pr()
{
	static int sqgr_coords[6] = {0, 4, 8, 11, 14, 18};
	int i;

	squaregrid_pr = mem_create(GRIDSIZE, GRIDSIZE, 1);
	for (i=0; i<6; i++) {
		pr_put(squaregrid_pr, 0, sqgr_coords[i], 1);
		pr_put(squaregrid_pr, sqgr_coords[i], 0, 1);
	}
}




init_triangrid_pr()
{
	int j;

	triangrid_pr = mem_create(GRIDSIZE, GRIDHEIGHT*2, 1);
	pr_vector(triangrid_pr, 0, GRIDHEIGHT-1, GRIDSIZE-1, GRIDHEIGHT-1,
		PIX_SET, 1);
	pr_vector(triangrid_pr, 0, (2*GRIDHEIGHT)-1, GRIDSIZE-1, (2*GRIDHEIGHT)-1,
		PIX_SET, 1);
	pr_vector(triangrid_pr, 1, 0, GRIDSIZE-2, GRIDHEIGHT*2, PIX_SET, 1);
	pr_vector(triangrid_pr, GRIDSIZE-1, 1, 1, GRIDHEIGHT*2, PIX_SET, 1);
}



init_menu_prs()
{
	menu_empty_pr = mem_create(1, 1, 1);

	menu_square_pr = mem_create(13, 13, 1);
	pr_vector(menu_square_pr, 0, 0, 0, 10, PIX_SET, 1);
        pr_vector(menu_square_pr, 10, 10, 0, 10, PIX_SET, 1); 
        pr_vector(menu_square_pr, 0, 0, 10, 0, PIX_SET, 1); 
        pr_vector(menu_square_pr, 10, 0, 10, 10, PIX_SET, 1);

	menu_triangle_pr = mem_create(15, 15, 1); 
        pr_vector(menu_triangle_pr, 7, 0, 0, 14, PIX_SET, 1);
	pr_vector(menu_triangle_pr, 7, 0, 14, 14, PIX_SET, 1);
        pr_vector(menu_triangle_pr, 0, 14, 14, 14, PIX_SET, 1);
}

