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
/*
  blas.c consists of Basic Linear Algebra Subroutines.   blas.c is not intended to be a general purpose 
  BLAS package.  It is intended to be an efficient way to handle vectorial problems arising in dynamics. 
  Since these routines are called many times within nested loops, I have tried 
  to write them without requiring that they allocate memory.  Those routines which return a vector 
  (eg, proj_vector(), orthog_vector(),...) have been written so that the user must allocate memory 
  for the object and pass in a pointer to the allocated space.
  */
#include <math.h>
#include <malloc.h>

#ifdef NOT_SUN
#include <not_sun_supplements.h>
#endif

/* 
  print_vector() prints n vector components
  */
print_vector(n,v)
int		n;
double 		*v;
{
  while ( n-- )
    printf("%g  ", *v++);
  printf("\n");	
}

/* 
  dot() returns the dot (inner) product of vectors u and v
  */
double 
  dot(dim,u,v)
int		dim;
double 	*u,*v;
{
  double s=0.0;
  
  while ( dim-- )
    s += *u++ * *v++;
  return ( s );
}

/* 
  L2_norm() returns the L2 norm of vector v
  */
double 
  L2_norm(dim,v)
int		dim;
double 	*v;
{
  double s=0.0;
  
  while ( dim-- )
    s += *v * *v++;
  return ( sqrt(s) );
}

/* 
  L2_norm_sqr() returns the square of the L2 norm of vector v
  */
double 
  L2_norm_sqr(dim,v)
int		dim;
double 	*v;
{
  double s=0.0;
  
  while ( dim-- )
    s += *v * *v++;
  return ( s );
}

/* 
  euclid_dist() returns the Euclidean distance between vectors u and v
  */
double 
  euclid_dist(dim,u,v)
int		dim;
double 	*u,*v;
{
  double s=0.0,r;
  
  while ( dim-- )
    {
      r = *u++ - *v++;
      s += r * r;
    }
  return ( sqrt(s) );
}

/* 
  euclid_dist_sqr() returns the square of the Euclidean distance between vectors u and v
  */
double 
  euclid_dist_sqr(dim,u,v)
int		dim;
double 	*u,*v;
{
  double s=0.0,r;
  
  while ( dim-- )
    {
      r = *u++ - *v++;
      s += r * r;
    }
  return ( s );
}

/* 
  normalize() returns in v the unit vector in the direction of v
  */
double *
  normalize(dim,v)
int		dim;
double 	*v;
{
  double 	norm, L2_norm();
  
  if ( (norm = L2_norm(dim,v)) != 0.0 )  /* if v is zero vector, return v. MAKE THIS LESS THAN MACH_EPS*/
    {
      while ( dim-- )		  /* otherwise normalize */
	*(v+dim) /= norm;
    }
  return ( v );
}

/*
  proj_vector() returns the vector projection of u onto the subspace spanned by v.
  The projection is returned in w, which is assumed to be a valid pointer to doubles.
  The return vector is the projection p = (<u,v> / <v,v>)v  of u onto v.
  Calls of the form proj_vector(dim, w, v, w)  and  proj_vector(dim, u, w, w) are permitted.
  */
double *
  proj_vector(dim,u,v,w)
int		dim;
double 	*u,*v,*w;
{
  double coef=0.0,dot();
  
  if ( !(coef = dot(dim,v,v) ) )
    coef = 0.0;
  else
    coef = dot(dim,u,v) / coef;
  while ( dim--  )
    *(w+dim) = coef * *(v+dim);
  return (w); 
}

/*
  orthog_vector() returns the vector orthogonal to u "in the direction of v" More precisely, it
  returns the projection of v onto the orthogonal complement of u.
  The projection is returned in w, which is assumed to be a valid pointer to doubles.
  Calls of the form orthog_vector(dim, w, v, w)  and  orthog_vector(dim, u, w, w)  are permitted.
  The return vector is calculated as v-p where  p = (<u,v> / <u,u>)u  is the projection
  of v onto u.
  */
double *
  orthog_vector(dim,u,v,w)
int		dim;
double 		*u,*v,*w;
{
  double 		coef=0.0,dot();
  
  if ( !(coef = dot(dim,u,u) ) )
    coef = 0.0;
  else
    coef = dot(dim,u,v) / coef;
  while ( dim-- )
    *(w+dim) = *(v+dim) - coef * *(u+dim);
  return (w);
}

/*
  cos_between() returns the cosine of the angle between vectors u and v.
  cos(theta) = <u,v> / |u||v|
  */
double
  cos_between(dim,u,v)
int		dim;
double	*u,*v;
{
  double	dot(),L2_norm();
  
  return ( dot(dim,u,v) / L2_norm(dim,u) / L2_norm(dim,v) );
}

/*
  same_side() takes three vectors, c, v, and u.  In general, span<c,v> determines a plane.
  If c and v are linearly dependent, 0 is returned.  Otherwise, we ask the question, "does u lie
  in the same half-plane as v?"  If so, 1 is returned; if not, -1 is returned.
  */
int
  same_side(dim, c, v, u)
int		dim;
double		*c, *v, *u;
{
  int		r;
  double	*orthog_vector();
  double	component,dot();
  double	* w = (double *)malloc(dim*sizeof(double));
  
  orthog_vector(dim,c,v,w);
  r = 0;
  while ( (r < dim) && (( component = *(w+r) ) == 0.0) )     /* check if w=0, ie, if c and v are linearly dependent */
    r++;
  if ( component == 0.0 )				    /* w=0. Test fails. */
    return ( 0 );
  r = ( dot(dim,u,w) > 0  ?  1  :  -1 );		    /* w and u on same side?  */
  free ( w );
  return( r );
}
