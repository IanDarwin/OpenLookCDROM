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

/*
  iter_forw() calculates the kth image of x under the function f, ie, y = f^k (x).  If (part of)
  the phase space is periodic, then y is projected onto the relevant tori.
*/
int
  iter_forw( f, k, y, x, p, ph_sp_dim, time_step, v, manifold)
int		(*f)();				  /* the function to iterate */
int		k;				  /* compute this many iterates of f; ie, return f^k(x) */
double		*y,*x,*p;			  /* vectors for f^k(x), x, and parameters. */
int		ph_sp_dim;			  /* dimension of phase space including time */
double		time_step;			  /* +1.0 if we are iterating a forward map; -1.0 for an inverse map */
double		*v;				  /* workspace of length ph_sp_dim */
Manifold	*manifold;
{
  int		i;
  int		v_dim = ph_sp_dim - 1;

  if ( k == 1 )					  /* quick and easy */
    {
      f(y,x,p);  y[v_dim] = x[v_dim] + time_step; /* iterate and increment time */
      project( v_dim, y, manifold );
    }
  else						  /* utilize temp storage */
    {
      for ( i=0; i < ph_sp_dim; i++ )		  /* copy time too */
	v[i] = x[i];
      while (k--)
	{
	  f(y,v,p);   y[v_dim] = v[v_dim] + time_step; /* iterate and increment time */
	  project( v_dim, y, manifold );
	  for ( i=0; i < ph_sp_dim; i++ )	  /* copy time too */
	    v[i] = y[i];
	}
    }
}


