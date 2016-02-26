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
#include <stddef.h>
#include <math.h>
#include <time.h>

/*
  rnd_vector() uses drand48() to produce a pseudo-random vector.
  Input the vector length n, an allocated vector of length n, and two n-vectors
  containing the min and max values for the i_th coordinate, 0 <= i <= n-1;
  Examples:
      double y, little = -1, big = 1;
      rnd_vector( 1, &y, &little, &big );    <---- y is p-random between -1 and 1

      double x[3], min[3], max[3];
      min[0] = 0.0; min[1] = -3.14159; min[2] = -5.;
      max[0] = 1.0; max[1] =  3.14159; max[2] =  5.;
      rnd_vector( 3, x, min, max );          <---- x[0] is p-random between 0 and 1,
      						   x[1] is p-random between -PI and PI
      						   x[2] is p-random between -5 and 5

  The decision to use drand48() was made cautiously.  While drand48() is not the
best pseudo-random number generator, we believe it is adequate for many
applications, such as generating monte-carlo initial guesses for root-finding
algorithms.  The user is warned, however, against using drand48() in
applications which require a high degree of statistical "randomness."  drand48()
is merely a linear congruential algorithm using 48 bit integer arithmetic (and
utilizing the highest 32 bits to compute a return value).  We refer the user to
Knuth, SEMINUMERICAL ALGORITHMS (1981) or Press et al, NUMERICAL RECIPES (1988)
for more information about pseudo-random numbers.

  drand48() was chosen over other pseudo-random number generators because 
  1) it is faster than some others which have better statistical properties; 
  2) it is more readily available, thus increasing the portability of dstool.  
     This is why, for example, we did not choose an additive pseudo-random 
     number generator such as d_addrans_().  
*/

int
  rnd_vector( n, x, min, max )
int		n;
double		*x, *min, *max;
{
  double	drand48();
  void		srand48();
  static int	first_time = 1;
  
  if ( first_time )
    {
      first_time = 0;
      srand48( (long) time( (time_t *)0 ) ); /* seed the random number generator by using internal clock */
    }
  while ( n-- )
    x[n] = min[n] + (max[n] - min[n]) * drand48();
}
