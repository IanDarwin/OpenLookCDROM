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
/* --------------------------------------------------
   driver for computing fixed points and periodic orbits

   -------------------------------------------------- */
#include <stdio.h>
#include <math.h>

#include <constants.h>
#include <prop.h>
#include <modellib.h>
#include <fixptlib.h>
#include <pm.h>

/*
 * distance_sqr(n,p1,p2,work,manifold)
 *              n the dimension of the points
 *              p1,p2,work points 
 *              manifold is the manifold of the current DS
 *
 * This function computes the L2 norm squared of the distance
 * between the two points.  The points are considered as elements of an
 * n-1 dimensional space.  The n-th coordinate is IGNORED! (usually time!)
 *
 * last modified: 7/11/91  paw
 */
double
  distance_sqr(n, p1, p2, work, manifold)
double *p1, *p2, *work;
int n;
Manifold *manifold;
{
  double lift();
  
  return(lift(n-1,p1,p2,work,manifold));
}


/* 
 * fixpt_test(p,n)
 *           p ptr to the integer parameters
 *           n number of parameters
 *
 * procedure to test whether a flow is some type of fixed point or
 * a manifold.
 *
 * last modified: 8/15/91  paw
 */
int
fixpt_test(p,n)
int n;
int *p;
{
  if (n!=1) return(FALSE);
  if (abs(p[0])<10) return(TRUE);
  return(FALSE);
}


/* 
 * manifold_test(p,n)
 *           p ptr to the integer parameters
 *           n number of integer parameters
 *
 * procedure to test whether a flow is some type of fixed point or
 * a manifold.
 *
 * last modified: 8/15/91  paw
 */
int
manifold_test(p,n)
int n;
int *p;
{
  if (n != 1) return(FALSE);
  if (abs(p[0])<10) return(FALSE);
  return(TRUE);
}


/*
 * fp_compute(fp)
 *            fp pointer to the fixed point data structure
 *
 * procedure to actually compute fixed points given a fixed point struct
 * which contains all the necessary information.
 * The results are stored in the memory object in the data struct.
 *
 * last modified: 12/2/91  paw
 */
int
fp_compute(fp)
     struct Fixpt_DataS *fp;
{
  int i, j,
      np,			/* the number of points in the orbit */
      status = 0;
  int color[3],			/* the color and symbol data */
      new_found = 0;		/* the number of new fixed points found */
  double dist, min, max, distance_sqr(); 
  int var_dim = fp->prop_cntl.ph_space_dim - 1;
  double dup_sqr = fp->dup * fp->dup;
  int format = *((int *) pm(GET, "Defaults.Precision", NULL));

  for (i=0; i<fp->ntrials  && status >= 0; i++) 
    {
      if (!fp->pick_pt) 
	{
	  for (j=0; j<var_dim; j++) 	  /* get random point, do not adjust time */
	    {
	      min = *((double *) pm(GET, "Defaults.Varb_Min", j, NULL));
	      max = *((double *) pm(GET, "Defaults.Varb_Max", j, NULL));
	      rnd_vector(1, fp->x1+j, &min, &max);
	    }
	  /* set time from selected point panel time */
	  fp->x1[var_dim] = *((double *) pm(GET, "Selected.Varb_Ic", var_dim, NULL));
	}

      fp->fp_map_period = fp->map_period;

      switch (fp->algorithm) 
	{
	case NEWTON :
	  status = mnewt(fp);
	  break;
	case SECANT :
	  status = msecant(fp); 
	  break;
	}

      if (status==1) 
	{
	  /* found a fixed point, do we have it already? */
	  /* reset the time coordinate (will this ever change? - paw) */
	  fp->x1[var_dim] = *((double *) pm(GET, "Selected.Varb_Ic", var_dim, NULL));
          if (fp->memory)
	    dist = memory_closest_distance(fp->memory, fp->x1, distance_sqr, 
					   fp->prop_cntl.workspace,fp->prop_cntl.manifold, fixpt_test);
          else dist = -1.0;
	  if (dist<0 || dist>dup_sqr) 
	    {
	      /* new fixed point */
	      new_found++;

	      /* find actual period and number of points to plot */
	      if (fp->prop_cntl.mapping_flag)
		{
		  for (j=0; j<fp->prop_cntl.ph_space_dim; j++)
		    fp->prop_cntl.traj_segment[0][j] = fp->x1[j];
		  fp->fp_map_period = 1;
		  for (j=1; j<fp->map_period; j++)
		    {
		      iter_forw(fp->prop_cntl.function, 1, fp->prop_cntl.traj_segment[j],
				fp->prop_cntl.traj_segment[j-1], 
				fp->prop_cntl.parameters, fp->prop_cntl.ph_space_dim, 
				1.0, fp->prop_cntl.workspace, fp->prop_cntl.manifold);
		      if (distance_sqr(fp->prop_cntl.ph_space_dim, fp->x1, 
				       fp->prop_cntl.traj_segment[j],
				       fp->prop_cntl.workspace, fp->prop_cntl.manifold) > dup_sqr)
			(fp->fp_map_period)++;
		      else j = fp->map_period;
		    }
		  np = fp->fp_map_period;
		}
	      else np = 1;

	      /* print information */
	      fprintf(stdout,"\nNEW fixed point at: ");
	      for (j=0; j<=var_dim; j++) fprintf(stdout,"%.*lg  ",format,fp->x1[j]);
	      if (fp->prop_cntl.mapping_flag) fprintf(stdout,"\nPeriod %d",fp->fp_map_period);
	      fp_eval(fp);
	      fprintf(stdout,"\nEigenvalues: ");
	      for (j=0; j<var_dim; j++) 
		fprintf(stdout,"%.*lg+%.*lgI ",format,fp->eval[0][j],format,fp->eval[1][j]);
	      fprintf(stdout,"\n");
	      fp_get_type(fp);
	      fp_get_attributes(fp);
	      fprintf(stdout,"Type: %s \n",fp->name);

	      /* add to memory */
              if (fp->memory)
		{
		  if (memory_start_new_flow(fp->memory, 1, 0, 0, np, 2*var_dim, 1)) return(-1);
		  color[0] = -1;
		  color[1] = fp->prop_cntl.sys_color;
		  color[2] = fp->prop_cntl.symbol;
		  for (j=0; j<var_dim; j++)
		    {
		      fp->prop_cntl.workspace[2*j] = fp->eval[0][j];
		      fp->prop_cntl.workspace[2*j+1] = fp->eval[1][j];
		    }
		  memory_add_point(fp->memory, fp->x1, fp->prop_cntl.parameters, color, NULL, NULL,
				   fp->prop_cntl.workspace, &(fp->fptype));
		  if (fp->prop_cntl.mapping_flag && (fp->fp_map_period > 1)) 
		    {
		      /* put whole orbit into memory */
		      for (j=1; j<fp->fp_map_period; j++) 
			memory_add_point(fp->memory, fp->prop_cntl.traj_segment[j], 
					 NULL, NULL, NULL, NULL, NULL, NULL);
		    }
		}
	    }
	  else
	    {
	      /* duplicate fixed point */
	      if (fp->setting > 0)
		{
		  fprintf(stdout,"\nDUPLICATE fixed point at: ");
		  for (j=0; j<=var_dim; j++) fprintf(stdout,"%.*lg  ",format,fp->x1[j]);
		  fprintf(stdout,"\n");
		}
	    }
	}
      else if (status > 1 && fp->setting > 0)
	{
	  /* did not find fixed point (but no error) so write out reason */
          if (status == 2)
	    fprintf(stdout,"\nNO CONVERGENCE - increase allowed number of iterations.\n");
          else if (status == 3)
            fprintf(stdout,"\nNO CONVERGENCE - singular matrix (try a different algorithm).\n");
          else if (status == 4)
            fprintf(stdout,"\nNO CONVERGENCE - decrease size of minimum step.\n");
          else
            fprintf(stdout,"\nNO CONVERGENCE - unknown reason!\n");
	}
      /* check interrupt status */
      if (interrupt()) status= -2;
    }
  return(new_found);
}

