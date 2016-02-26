#include "surfmodl.h"
void Shellpts(Xpt,Ypt,Npts)
points *Xpt,*Ypt;
int Npts;
{
    int Dist;              /* sorting distance */
    int K;              /* genl sorting indexes */
    int I;              /* genl sorting indexes */



/* Determine the initial value of Dist by finding the largest power
  of 2 less than Npts, and subtracting 1 from it. The final step in
  this calculation is performed inside the main sorting loop.
*/
  Dist = 4;
  while ( (Dist < Npts) )
    Dist = Dist + Dist;
  Dist = Dist - 1;

/* Main sorting loop. The outer loop is executed once per pass. */
  while ( (Dist > 1) ) {
    Dist = Dist / 2;
    for ( K = 0 ; K < (Npts - Dist) ; K++) {
      I = K;
      while ( (I >= 0) ) {
        /* This stmt. is the comparison. It also controls moving values
          upward after an exchange. */
        if ( (Ypt[I] > Ypt[I+Dist]) ||
          ((Ypt[I] == Ypt[I+Dist]) && (Xpt[I] > Xpt[I+Dist])) ) {
          /* The next 2 stmts. perform the exchange */
          swapint (&Xpt[I], &Xpt[I+Dist]);
          swapint (&Ypt[I], &Ypt[I+Dist]);
        }  else
          I = -1;      /* stop the while loop! */
        I = I - Dist;
      } /* while */
    } /* for K */
  } /* while Dist */
} /* procedure Shellpts */

void shellshades(Xpt,Ypt,Shpt,Npts)
points *Xpt,*Ypt;
realpts *Shpt;
int Npts;
{
    int Dist;              /* sorting distance */
    int K;              /* genl sorting indexes */
    int I;              /* genl sorting indexes */


/* Determine the initial value of Dist by finding the largest power
  of 2 less than Npts, and subtracting 1 from it. The final step in
  this calculation is performed inside the main sorting loop.
*/
  Dist = 4;
  while ( (Dist < Npts) )
    Dist = Dist + Dist;
  Dist = Dist - 1;

/* Main sorting loop. The outer loop is executed once per pass. */
  while ( (Dist > 1) ) {
    Dist = Dist / 2;
    for ( K = 0 ; K < (Npts - Dist); K++ ) {
      I = K;
      while ( (I >= 0) ) {
        /* This stmt. is the comparison. It also controls moving values
          upward after an exchange. */
        if ( (Ypt[I] > Ypt[I+Dist]) ||
          ((Ypt[I] == Ypt[I+Dist]) && (Xpt[I] > Xpt[I+Dist])) ) {
          /* The next 2 stmts. perform the exchange */
          swapint (&Xpt[I], &Xpt[I+Dist]);
          swapint (&Ypt[I], &Ypt[I+Dist]);
          swapreal (&Shpt[I], &Shpt[I+Dist]);
        }  else
          I = -1;      /* stop the while loop! */
        I = I - Dist;
      } /* while */
    } /* for K */
  } /* while Dist */
} /* procedure Shellshades */
