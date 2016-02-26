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
#include <constants.h>
#include <iterate.h>
#include <complib.h>

/* 
  implicit_iter() uses Newton's method to compute the solution  x  which satisfies the equation f(x) - c = 0.
  The initial guess is passed in as input; it returns (hopefully) as the solution x.
  If an acceptable solution vector is found, 0 is returned;  if no solution is found after newt_iter
  newton steps, then the return is NO_CONVERGENCE; if a singular matrix occurs, the return is SINGULAR.
  If the newton step gets to small, NO_CONVERGENCE is returned.
*/
int
  implicit_iter(f, jacobian, jacobian_switch, newt_iter, k, m, x, c, param, ph_sp_dim, time_step, h, tolx, tolf, manifold, y, indx)

int		(*f)();				/* pointer to governing equation */
int		(*jacobian)();			/* pointer to function which computes jacobian (analytic or numeric) */
int		jacobian_switch;		/* flag to indicate mode of jacobian calculation */
int		newt_iter;			/* number of iterations of Newton's method for each guess */
int		k;				/* work with the kth iterate of f, ie, f^k */
double		**m;				/* allocated v_dim^2 matrix of (spatial) partial derivs (jacobian) */
double  	*x;				/* guessed pt used to eval fnct and partials; x = soln on return */
double		*c;				/* Current state.  We want x such that f(x)=c. */
double  	*param;			        /* parameters */
int		ph_sp_dim;			/* dimensions of variables (x) including time */
double  	time_step;			/* +1.0 if we are iterating a forward map; -1.0 for an inverse map */
double		*h;				/* vector of stepsizes in each coordinate direction; the finite diff
						   pt is x+h.   NOTE: No element of this vector can be zero! */
double		tolx;				/* for finite-diff, convergence if | x - x_approx | < tolx */
double		tolf;				/* for finite-diff, convergence if | f(x) - f(x_approx) | < tolf */
Manifold	*manifold;
double		*y;				/* workspace vector at least (v_dim)^2 + 3*v_dim elements */
int		*indx;				/* integer workspace vector of v_dim elements */

{
  int		i;
  int		v_dim = ph_sp_dim - 1;
  int		singular = FALSE;
  int		ludcmp(), lubksb(), project();
  double	lift(),	L2_norm_sqr();
  double	d;
  double	*v;

  v = y + ph_sp_dim + 1;			  /* remaining workspace */
  
  project( v_dim, c, manifold );		  /* project onto manifold */
  project( v_dim, x, manifold );

  iter_forw(f,k,y,x,param,ph_sp_dim,time_step,v,manifold); /* great guess? */
  if ( lift( v_dim, c, y, y, manifold ) <= tolf*tolf )	  /* y <-- pt on same euclidean (tangent or covering) space */
      return ( 0 );		                  /* success! lift() returns distance from c to lifted y*/
  
  while ( (newt_iter--) && !singular )		  /* begin newton's method */
    {
      if ( dfdx( m, ph_sp_dim-1, h, x, param, k, manifold, (int)time_step, f, jacobian, jacobian_switch, v ) == -3)
	dfdx( m, ph_sp_dim-1, h, x, param, k, manifold, (int)time_step, f, jacobian, FORW_DIFF, v ); /* if jacobian==NULL */
      iter_forw(f,k,y,x,param,ph_sp_dim,time_step,v,manifold); 
      if ( lift( v_dim, c, y, y, manifold ) <= tolf*tolf )
	return ( 0 );
      for ( i=0; i<v_dim; i++)			  /* otherwise, revise guess */
	y[i] = c[i] - y[i];			  /* save (spatialpart of) c - f^k(x) in the workspace y */
      
      if ( ludcmp( m,v_dim,indx,&d ) )		  /* singular matrix! handle it gracefully */
	singular = TRUE;
      else
	{
	  lubksb( m,v_dim,indx,y );		  /* solve D(f^k(x)) v = c - f^k(x); soln returned in y */
	  if ( L2_norm_sqr(v_dim,y) <= tolx*tolx )
	    return ( NO_CONVERGENCE );
	  for ( i=0; i<v_dim; i++ )
	    x[i] += y[i];			  /* new (spatial) guess */
	  project( v_dim, x, manifold );
	}
    }
  return( singular? SINGULAR : NO_CONVERGENCE );  /* failed to converge within newt_iter iterations */
}




