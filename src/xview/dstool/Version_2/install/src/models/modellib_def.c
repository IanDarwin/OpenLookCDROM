/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
/* This is a configuration file for dynamical systems */
#include <modellib.h>

struct  DS_DataS	DS_model;

char	*DS_Category[] = { "Standard Mappings",         /* category 0 */
			   "Standard Vector Fields",	/* category 1 */
			   "Special Dynamical Systems",  /* category 2 */
 			   "Parser"
			   };

/* ------------------------------------------------------------------ */
/*   declare names of initialization routines                         */
/* ------------------------------------------------------------------ */
extern int	lorenz_init(), cubic_init(), vdpol_init();
extern int	standard_init(), henon_init(), ko_init();
extern int	hh_init(), d4_init(), pendulum1d_init();
extern int 	parserwin_open();
extern int	d3_symm_map_init(), double_toroid_init(),
		duffing_init(), field_worfolk_init(),
		logistic_init(),mathieu_init(),
		perturbed_dtoroid_init(), rlag_init(),
		simple_torus_map_init(), symm_red_init(),
                four_param_init(), lp4_init();
extern int parserwin_open();


/* ------------------------------------------------------------------ */
/* fill structure according to: { category, title, init_routine }     */
/* the first system in the struct will be the default system          */
/* ------------------------------------------------------------------ */
struct	DS_DataS	DS_Sel[]={
       { 1, "Lorenz System", lorenz_init },
       { 0, "Standard Map", standard_init },
       { 0, "Kim-Ostlund Torus Map", ko_init },
       { 0, "Complex Henon Map", henon_init },
       { 0, "D3 Symmetric Mapping",d3_symm_map_init },
       { 0, "The Logistic Map", logistic_init },
       { 0, "Simple Torus Map", simple_torus_map_init },
       { 1, "Planar Cubic Vector Field", cubic_init },
       { 1, "Forced Van der Pol Eqns", vdpol_init },
       { 1, "Duffing's Equations", duffing_init },
       { 1, "1 dof Damped Driven Pendulum", pendulum1d_init },
       { 1, "Mathieu Equation", mathieu_init },
       { 2, "Hodgkin-Huxley Equations", hh_init },
       { 2, "Rinzel Lee Model + A Current", rlag_init },
       { 2, "D4 Symmetric Vector Field", d4_init },
       { 2, "Symmetry Reduced Planar Equations",symm_red_init }, 
       { 2, "Double Toroid",double_toroid_init },
       { 2, "The Field-Worfolk Equations", field_worfolk_init },
       { 2, "Perturbed Double Toroid",perturbed_dtoroid_init },
       { 2, "Four parameter system (Salvador)", four_param_init },
       { 2, "LP Neural Model 4", lp4_init },
       { 3, "Parsed dynamical system...", parserwin_open}
     };

/* ------------------------------------------------------------------ */
/* do not edit beyond this line                                       */
/* ------------------------------------------------------------------ */
int	N_DS = sizeof(DS_Sel) / sizeof(struct DS_DataS);
int	N_DS_Category = sizeof(DS_Category) / sizeof(char *);
