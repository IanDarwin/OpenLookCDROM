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
#include <stdio.h>
#include <math.h>
#include <manifold.h>
#include <constants.h>

#define	DIFF_STEP_MIN	1.0e-10
#define	DIFF_STEP_MAX	1.0e10

/* ------------------------------------------------------------------------------------------
   This file contains the following routines for computing derivative matricies:

       dfdx( matrix, n, h, state, parameters, k, manifold,
	     map_flag, ds_func, ds_dfunc, mode, work)
	  computes the spatial jacobian of the function, ds_func, evaluated
	  at the state, time and parameter values provided.

       dfdt( vector, n, h, state, parameters, k, manifold,
	     map_flag, ds_func, ds_dfunc, mode, work)
	  computes the vector of time derivatives of the function, ds_func evaluated
	  at the state, time and parameter values provided.

       dfdp( matrix, n, m, h, state, parameters, k, manifold,
	     map_flag, ds_func, ds_dfunc, mode, work)
	  computes the jacobian of the function, ds_func, wrt the parameter variables
	  evaluated at the state, time and parameter values provided.

	where,

	   matrix		is a pre-allocated return array for the deriv values
	   n			dimension of the state space      
	   m			dimension of the parameter space
	   h			stepsizes for numerical differencing
	   state		values of the state variables and time (in nth position)
	   parameters		values of the parameter variables
	   k			
	   manifold		ptr to manifold definition structure
	   map_flag             flag indicating ds_func is an iterative map:
				  0 = vector field ; 1 = forward map ; -1 = backwards map
	   ds_func		func ptr for ds definition function
	   ds_dfunc		func ptr for the analytic derivatives
	   mode			computation mode: valid values are global constants
				ANALYTIC, FORWD_DIFF and CEN_DIFF.
	   work			pre-allocated workspace for use by the proc
   

   Function value on return indicates status:

	 3    analytic jacobian not defined; dentral diff jacobian returned instead
	-2    dimension error in passwd arguments
	-3    analytic jacobian defined
	-4    stepsize list value error

   ------------------------------------------------------------------------------------------ */


int     
dfdx( matrix, n, h, state, parameters, k, manifold, map_flag, ds_func, ds_dfunc, mode, work)
Manifold	*manifold;
int		n, k, map_flag, mode, (*ds_func)(), (*ds_dfunc)();
double		**matrix, *state, *parameters, *work, *h;
{
	int	status, i, j, l;
	double	*x, *u, *v, *w, *A, *unused_work;
	double	min_list(), max_list(), lift();

	status = 0;
	if (map_flag && k<=0) return(-1);

	switch(mode)
	   {
	    case ANALYTIC:
	       if( ds_dfunc == NULL ) return(-3);
	       ds_dfunc(matrix, state, parameters);
	       if (!map_flag) break;

	       x = work;		/* partition workspace */
	       v = work + n + 1; 
	       w = v + n;
	       A = w + n;
	       unused_work = A + n*n;

	       for(i=0; i<n; i++)
		  dcopy(n, &matrix[i][0], 1, &A[i*n], 1);
	       dcopy(n+1,state,1,x,1);
	       while ( --k )
		  {iter_forw(ds_func, 1, v, x, parameters, n+1, (double) map_flag, unused_work, manifold);
	           ds_dfunc(matrix, v, parameters);

		   for(j=0; j<n; j++)
		      {for(i=0; i<n; i++)				/* compute matrix*A and place result  */
		         {w[i] = 0.0;					/* in A                               */
		          for(l=0; l<n; l++)
		 	    w[i] = w[i] + matrix[i][l]*A[l*n+j];
		         }
		       for(i=0; i<n; i++)
			  A[i*n+j] = w[i];
		      }
	           dcopy(n,v,1,x,1);
		  }
	       
	       for(i=0; i<n; i++)
		  dcopy(n, &A[i*n], 1, &matrix[i][0], 1);
	       break;
	    case FORW_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       x = work;		/* partition the workspace */
	       u = work + n + 1;
	       v = u + n;
	       unused_work = v + n;

	       if(map_flag)
      		   iter_forw(ds_func, k, u, state, parameters, n+1, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(u, state, parameters);		/* u contains f(state) or f^k(state) */
		   	     
	       dcopy(n+1,state,1,x,1);
	       for(j=0; j<n; j++)
		   {x[j] += h[j];				/* x = state + h[j]*e(j) */
		    project(n, x, manifold);
		    if(map_flag)
		      {iter_forw(ds_func, k, v, x, parameters, n+1, (double) map_flag, unused_work, manifold); 
		       lift(n, u, v, v, manifold); }
	            else
		      ds_func(v, x, parameters);
   
		    for(i=0; i<n; i++)
		       matrix[i][j] = (1.0/h[j]) * (v[i]-u[i]);
		    x[j] = state[j];
		  }
	       break;	
	    case CEN_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       x = work;		/* partition the workspace */
	       u = work + n + 1;
	       v = u + n;
	       unused_work = v + n;

	       dcopy(n+1,state,1,x,1);
	       for(j=0; j<n; j++)
		   {x[j] += h[j];				/* x = state + h[j]*e(j) */
		    project(n, x, manifold);
		    if(map_flag)
		      {iter_forw(ds_func, k, u, x, parameters, n+1, (double) map_flag, unused_work, manifold); 
		       (void) lift(n, u, v, v, manifold);
		      }
	            else
		      ds_func(u, x, parameters);		/* u = f(state+h[j]*e(j)) or f^k(state+h[j]*e(j)) */

		    x[j] -= 2.0*h[j];				/* x = state - h[j]*e(j) */
		    project(n, x, manifold);
		    if(map_flag)
		      {iter_forw(ds_func, k, v, x, parameters, n+1, (double) map_flag, unused_work, manifold); 
		       (void) lift(n, u, v, v, manifold);
		      }
	            else
		      ds_func(v, x, parameters);
   
		    for(i=0; i<n; i++)
		       matrix[i][j] = (0.5/h[j]) * (u[i]-v[i]); 
		    x[j] = state[j];
		  }
	       break;	
 	     }
	return(status);
}


int     
dfdp( matrix, n, m, h, state, parameters, k, manifold, map_flag, ds_func, ds_dfunc, mode, work)
Manifold	*manifold;
int		n, m, k, map_flag, mode, (*ds_func)(), (*ds_dfunc)();
double		**matrix, *state, *parameters, *work, *h;
{
	int	status, i, j;
	double	*p, *u, *v, *unused_work;
	double	min_list(), max_list();

	status = 0;

	switch(mode)
	   {
	    case ANALYTIC:
	       if(ds_dfunc == NULL) return(-3);
	       ds_dfunc(matrix, state, parameters);
	       break;
	    case FORW_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       p = work;		/* partition the workspace */
	       u = work + m;
	       v = u + n;
	       unused_work = v + n;

	       if(map_flag)
      		   iter_forw(ds_func, k, u, state, parameters, n, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(u, state, parameters);	/* u contains f(state) or f^k(state) */
		   	     
	       dcopy(m,parameters,1,p,1);
	       for(j=0; j<m; j++)
		   {p[j] += h[j];			/* p = parameters + h[j]*e(j) */
		    if(map_flag)
		      iter_forw(ds_func, k, v, state, p, n, (double) map_flag, unused_work, manifold); 
	            else
		      ds_func(v, state, p);
   
		    for(i=0; i<n; i++)
		       matrix[i][j] = (1.0/h[j]) * (v[i]-u[i]);
		    p[j] = parameters[j];
		  }
	       break;	
	    case CEN_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       p = work;		/* partition the workspace */
	       u = work + m;
	       v = u + n;
	       unused_work = v + n;

	       dcopy(m,parameters,1,p,1);
	       for(j=0; j<m; j++)
		   {p[j] += h[j];		/* p = parameters + h[j]*e(j) */
		    if(map_flag)
		      iter_forw(ds_func, k, u, state, p, n, (double) map_flag, unused_work, manifold); 
	            else
		      ds_func(u, state, p);	/* u = f(parameters+h[j]*e(j)) or f^k(parameters+h[j]*e(j)) */

		    p[j] -= 2.0*h[j];		/* p = parameters - h[j]*e(j) */
		    if(map_flag)
		      iter_forw(ds_func, k, v, state, p, n, (double) map_flag, unused_work, manifold); 
	            else
		      ds_func(v, state, p);
   
		    for(i=0; i<n; i++)
		       matrix[i][j] = (0.5/h[j]) * (u[i]-v[i]);
		    p[j] = parameters[j];
		  }
	       break;	
 	     }
  return(status);
}


int     
dfdt( vector, n, h, state, parameters, k, manifold, map_flag, ds_func, ds_dfunc, mode, work)
Manifold	*manifold;
int		n, k, map_flag, mode, (*ds_func)(), (*ds_dfunc)();
double		*vector, *state, *parameters, *work, h;
{
	int	status, i;
	double	*u, *v, *unused_work, t0;
	double	min_list(), max_list();

	status = 0;

	switch(mode)
	   {
	    case ANALYTIC:
	       if(ds_dfunc == NULL) return(-3);
	       ds_dfunc(vector, state, parameters);
               break;
	    case FORW_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       u = work;
	       v = u + n;
	       unused_work = v + n;

	       t0 = state[n];
	       if(map_flag)
      		   iter_forw(ds_func, k, u, state, parameters, n, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(u, state, parameters);		/* u contains f(state) or f^k(state) */
		   	     
	       state[n] = t0 + h;			
	       if(map_flag)
		   iter_forw(ds_func, k, v, state, parameters, n, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(v, state, parameters);
  	      
	       for(i=0; i<n; i++)
		    vector[i] = (1.0/h) * (v[i]-u[i]);
	       state[n] = t0;
	       break;	
	    case CEN_DIFF:
	       if(min_list(n,h)<DIFF_STEP_MIN || max_list(n,h)>DIFF_STEP_MAX) return(-4);

	       u = work;
	       v = u + n;
	       unused_work = v + n;

	       t0 = state[n];
	       state[n] += h;
	       if(map_flag)
		   iter_forw(ds_func, k, u, state, parameters, n, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(u, state, parameters);	

	       state[n] -= 2.0*h;
	       if(map_flag)
		   iter_forw(ds_func, k, v, state, parameters, n, (double) map_flag, unused_work, manifold); 
	       else
		   ds_func(v, state, parameters);
   
	       for(i=0; i<n; i++)
		    vector[i] = (0.5/h) * (u[i]-v[i]);
	       state[n] = t0;
	       break;	
 	     }
  return(status);
}

/*  ------------------------------------------------------------------------------------------------------------------
    routines to be added to vector utilities */

double
min_list(n, vector)
int	n;
double	*vector;
{
	int	count;
	double	min_value, temp;

	if (n==0) return(0.0);

	min_value = fabs(vector[0]);
	for(count=1; count<n; count++)
	  {
	    temp = fabs(vector[count]);
	    if (temp < min_value) min_value = temp;
	  }
	return(min_value);
}


double
max_list(n, vector)
int	n;
double	*vector;
{
	int	count;
	double	temp, max_value;

	if (n==0) return(0.0);

	max_value = fabs(vector[0]);
	for(count=1; count<n; count++)
	  {
	    temp = fabs(vector[count]);
	    if (temp > max_value) max_value = temp;
	  }
	return(max_value);
}
