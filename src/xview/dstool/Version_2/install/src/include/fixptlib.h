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
#ifndef FIXPTLIB_HEADER
#define FIXPTLIB_HEADER

/* --------------------------------
   data structure for fixed point/periodic orbit algorithms

   -------------------------------- */
#include <complib.h>
#include <memory.h>

struct	Fixpt_DataS	{
  int		map_period;	/* fixed points of f^(map_period) */
  double        vf_period;	/* fixed points of time flow vf_period, currently only 0 */
  int		algorithm;	/* NEWTON or SECANT */
  int		pick_pt;	/* 0 for Monte Carlo, else choose random points */
  int		ntrials;	/* the number of Monte Carlo trials */
  int		nsteps;		/* the max number of iterates for fixpt algorithm */
  int		fptype;		/* the type of the found fixed point */
  int           fp_map_period;	/* the period of the found fixed point (maps only) */
  char          name[20];	/* the name of the fptype */
  int           setting;        /* 0 if partial view, 1 if full view of panel settings PAW 12/2/91 */
  int		*indx;		/* workspace for ludcmp() and lubksb() */
  double	*x1;		/* point in phase space, usually the fixed point */
  double	*x2;		/* point in phase space */
  double	*x3;		/* point in phase space */
  double	*fx;		/* point in phase space */
  double	**jacobian;     /* entries indexed from 0 */
  double	**jact;		/* temp jacobian space for rg(); 
				   assumes indices run from 1 to
				   var_dim rather than from 0 */
  double        **eval;		/* entry eval[0][i] is the real part
				   and eval[1][i] is the imaginary part
				   of the i'th eigenvalue where i 
				   starts from 0 */
  double        **evectors;	/* i'th eigenvector in the i'th column 
				   indexed starting from 0 */
  double        fd_step;	/* info from periodic window */
  double	dup;
  double	varb_conv;
  double        funct_conv;
  double        eigen_dist;
  int           stab_pts;          
  int           stab_steps;
  int           unstab_pts;
  int           unstab_steps;
  memory	memory;
  struct Prop_DataS	prop_cntl;
		};

#endif


