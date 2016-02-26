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
#ifndef COMPLIB_HEADER
#define COMPLIB_HEADER

#include <manifold.h>
#include <history.h>

#define DIVISOR_TOO_SMALL 1.e-15

#define ZERO 0
#define PLUS 1
#define MINUS -1
#define NO_CROSSING 2
#define CROSSING 3
#define CROSSING_NEW 4
#define CROSSING_OLD 5

#define BAD_CROSSING_CONDITION  -2
#define LAST_STEP_TOO_SMALL     -3
#define STEP_OUTSIDE_INTERVAL   -4
#define EXCEED_MAX_ITER         -5


typedef struct
    {
      int	(*fstop_func_ptr)();	/* pointer to function which evaluates user-defined relation */
      int	num_fstop_indices;	/* number of fstop function instances */
      int	*fstop_indices;		/* list of indices, one for each fstop instance */
      int	*cross_orient;		/* orientation of desired crossing */
      int	*cross_mode;		/* method of stopping desired at the boundary */
      double	*target_values;		/* minimum values target */
      double	*target_tols;		/* maximum target values */
    }fstop_group;

struct	Prop_DataS	
    {
      int	(*integ_driver)();	/* if vector field, fct ptr to integrator */
      int	(*function)();		/* fct ptr to dynamical system RHS */
      int	(*aux_function)();	/* fct ptr to dynamical system aux funcs */
      int	(*inverse)();		/* if mapping, fct ptr to explicit inverse */
      int	(*dfdx)();		/* fct ptr to analytic jacobian for matrix of spatial derivatives */
      int	(*dxdt)();		/* fct ptr to analytic jacobian for vector of temporal derivatives */
      int	(*fstop)();		/* pointers to function which specifies stopping conditions. */
      int	ph_space_dim;		/* number of phase space variables */
      int	parameter_dim;		/* number of parameter variables */
      int	function_dim;		/* number of user-defined auxilary functions */
      int	direction;		/* FORWARD or BACKWARD */
      int	enable_nsteps;		/* vf: can the selected integ algorithm prop a fixed # of steps? */
      int	enable_fstop;		/* vf: can the selected integ algorithm prop to a function value? */
      int	enable_tstop;		/* vf: can the selected integ algorithm prop to a final time? */
      int	fixed_step_flag;	/* vf: does the selected integ routine use fixed steps? */
      int	iter_request;		/* number of iterations requested */
      int	iterations;		/* number of iterations performed */
      int	prop_mode;		/* propagation mode (PROP_NSTEP, PROP_TF, PROP_FSTOP) */
      int	prop_segsize;		/* trajectory data block generation size */
      int	symbol;			/* plotting symbol */
      int	table_color;		/* colortable entry for plotting */
      int	sys_color;		/* system colormap entry for plotting */
      int	mapping_flag;		/* is this a mapping? */
      int	inverse_flag;		/* if so, is the inverse FALSE, APPROX_INV, or EXPLICIT_INV? */
      int	jacobian_flag;		/* what method to obtain jacobian (ANALYTIC, FORW_DIFF, CEN_DIFF)? */
      int	f_iter;			/* if a map and this entry=k, compute images of f^k */
      int	f_skip;			/* if a vf, save points at f_skip intervals (1=every pt, 2=every other pt, etc..) */
      int       start_to_save;          /* # of computed points NOT to plot/save from beginning of traj */ /* paw  4/14/92 */
      int  	*i_wkspace;		/* integer workspace */
      int	*panel_int_values;	/* integer list of prop panel data fields */
      int	*panel_choice_values;	/* integer list of choice widget items from prop panel */
      int	panel_option;		/* fixed choice item from prop panel */
						/*  mapping:  jacobian selection        */
						/*  vector field:  integrator selection */
      int	num_fstop_grps;		/* number of active fstop groups */
/*      int	*varb_fstop_active;*/	/* active phase space variables for PROP_FSTOP evaluation */
/*      int	*func_fstop_active;*/	/* active aux functions for PROP_FSTOP evaluation */
      double	*panel_dp_values;	/* double list of prop panel data fields */
      double	time_step;		/* if vector field, length of integration timestep */
      double	estim_step;		/* if variable step integ, estim of next stepsize */
      double	final_time;		/* if PROP_MODE is PROP_TF, terminal objective time  */
      double	diverg_cutoff;		/* threshold signaling component of prop state diverging - halt propagation */
      double	*state;			/* spatial initial conditions + time in last position */
      double	*parameters;		/* current parameter values */
      double	*min;			/* local copy of pm(GET_LIST, Traj_Ds_Object, Varb_Min, 0, ph_space_dim-1,.. */
      double	*max;			/* local copy of pm(GET_LIST, Traj_Ds_Object, Varb_Max, 0, ph_space_dim-1,.. */
/*      double	*varb_fstop_values;*/	/* target phase space variables values for PROP_FSTOP propagation */
/*      double	*func_fstop_values;*/	/* target aux function values for PROP_FSTOP propagation */
      double  	*workspace;		/* double workspace */
      double    **traj_segment;		/* return list for propagated trajectory points */
      Manifold  *manifold;		/* description of the phase space */
      fstop_group **fstop_grp_list;	/* list of fstop structures, one structure for each group. */
      History_Struct  * history_list;	/* structure to hold history for multi-step
					  integrators. Set to NULL initially. */
      void (*plot_traj)();              /* routine to plot trajectory points */
    };

#endif
