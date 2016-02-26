#include "surfmodl.h"
void shelsurf(Surfmin,Surfmax,Nsurf)
surfaces *Surfmin,*Surfmax;
int Nsurf;
{ 
    int Dist;              /* sorting distance */
    int K, I;              /* genl sorting indexes */
    int Vert;              /* vertex number */
    int Vert1, Vert2;      /* vertices to swap */


/* Determine the initial value of Dist by finding the largest power
  of 2 less than Nsurf, and subtracting 1 from it. The final step in
  this calculation is performed inside the main sorting loop.
*/
  Dist = 4;
  while ( Dist < Nsurf) 
    Dist = Dist + Dist;
  Dist = Dist - 1;

/* Main sorting loop. The outer loop is executed once per pass. */
  while ( (Dist > 1) ) {
    Dist = Dist / 2;
    for ( K = 0 ; K < (Nsurf - Dist); K++ ) {
      I = K;
      while ( (I >= 0) ) {
        /* This stmt. is the comparison. It also controls moving values
          upward after an exchange. */
        if ( (Surfmax[I] > Surfmax[I+Dist]) ||
          ((Surfmax[I] == Surfmax[I+Dist]) && (Surfmin[I] > Surfmin[I+Dist]))
          ) {
          /* The next 6 stmts. perform the exchange */
          swapreal (&Surfmax[I], &Surfmax[I+Dist]);
          swapreal (&Surfmin[I], &Surfmin[I+Dist]);
          swapint  (&Matl[I], &Matl[I+Dist]);
          swapint  (&Nvert[I], &Nvert[I+Dist]);
          /* Swap all the vertices */
            swapint (&Connect[I], &Connect[I+Dist]);
        }  else
          I = -1;      /* stop the while loop! */
        I = I - Dist;
      } /* while */
    } /* for K */
  } /* while Dist */
} /* procedure Shelsurf */
