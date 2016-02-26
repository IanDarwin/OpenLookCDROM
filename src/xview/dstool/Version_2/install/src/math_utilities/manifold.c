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
#include <manifold.h>
#include <math.h>
#include <constants.h>

/*
  lift().  Pass in x and y, points on the manifold.  
  For PERIODIC manifolds,  lift() computes various lifts of y onto the covering space.
  the distance (on the covering space) between x (in the fundamental domain) and the lifted ys 
  are then calculated.  If X is the fiber over y, then lift() returns in closest_pt the element 
  of X which is closest to x.  Calls of the form   lift( v_dim, x, y, y, ... ) are permitted.
  lift() returns the square of the distance from x to closest_pt.
*/
double
  lift( v_dim, x, y, closest_pt, manifold )
int		v_dim;			     /* dim of phase space NOT including time */
double          *x,*y,*closest_pt;	     /* y is lifted with respect to x.  The lift returns in closest_pt */
Manifold	*manifold;
{
  int                   i,j,k;
  double                coord,y0,temp_dist,dist=0.0;
  double		period;

  for ( j=0; j<v_dim; j++ )			  /* in case there are no periodic variables */
    {
      closest_pt[j] = y[j];
      dist += (x[j] - y[j]) * (x[j] - y[j]);
    }

  if ( manifold->type == PERIODIC )
    {
      for ( j=0; j<v_dim; j++ )
	if ( manifold->periodic_varb[j] )
	  {
	    period = manifold->period_end[j] - manifold->period_start[j];
	    coord = y0 = y[j];
	    dist = 0.0;
	    for ( k=0; k<v_dim; k++ )		  /* euclid dist_sqr; do here to save overhead of euclid_dist_sqr() */
/*	      dist += (x[k] - y[k]) * (x[k] - y[k]);*/
	      dist += (x[k] - closest_pt[k]) * (x[k] - closest_pt[k]);
	    for (i= -1; i<=1; i+=2)		  /* loop works b/c of independence of coordinates */
	      {
		closest_pt[j] = y0 + (double) i*period; /* change coord by amount of period */
		temp_dist = 0.0;
		for ( k=0; k<v_dim; k++ ) 
		  temp_dist += (x[k] - closest_pt[k]) * (x[k] - closest_pt[k]);
		if ( temp_dist < dist )		  /* new closest pt; save component */
		  {
		    dist = temp_dist;
		    coord = closest_pt[j];
		  }
	      }
	    closest_pt[j] = coord;
	  }
    }
  return ( dist );
}

/*
  project() takes a point p and projects it onto the manifold.
  For PERIODIC manifolds, p is on the covering space and project() returns (in p) the
  projection of p onto the fundamental domain.
*/
int
  project( v_dim, p, manifold)
int		v_dim;				  /* dim of phase space NOT including time */
double		*p;				  /* pt on covering space to project */
Manifold	*manifold;
{
  double	period;

  if ( manifold->type == PERIODIC )
    while ( v_dim-- )
      if ( manifold->periodic_varb[v_dim] )
	{
	  if (( period = manifold->period_end[v_dim] - manifold->period_start[v_dim] ) != 0.0)
	    p[v_dim] -= period * floor( (p[v_dim] - manifold->period_start[v_dim])/period );
	}
}

/*
  project_and_flag() takes a point p and projects it onto the manifold.
  For PERIODIC manifolds, p is on the covering space and project() returns (in p) the
  projection of p onto the fundamental domain.
  If the final point differs from the initial, TRUE is returned, else FALSE is returned.
*/
int
  project_and_flag( v_dim, p, manifold)
int		v_dim;				  /* dim of phase space NOT including time */
double		*p;				  /* pt on covering space to project */
Manifold	*manifold;
{
  double	period, diff;
  int status = FALSE;

  if ( manifold->type == PERIODIC )
    while ( v_dim-- )
      if ( manifold->periodic_varb[v_dim] )
	{
	  if (( period = manifold->period_end[v_dim] - manifold->period_start[v_dim] ) != 0.0)
	    {
	      diff = period * floor( (p[v_dim] - manifold->period_start[v_dim])/period );
	      if (diff != 0.0)
		{
		  p[v_dim] -= diff;
		  status = TRUE;
		}
	    }
	}
  return status;
}
