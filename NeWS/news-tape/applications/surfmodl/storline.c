#include "surfmodl.h"
void storline(X1,Y1,X2,Y2,Xpt,Ypt,Npts)
int X1,Y1,X2,Y2;
points *Xpt,*Ypt;
int *Npts;
{
 int X,Y ;                   /* current point being stored */
    double Xfact;                     /* factor for (X,Y) interpolation */
    int Ylow, Yhigh;            /* range of for loop */
    boolean First;                  /* flag first dot of line */

  First = TRUE;
  if (  Y2== Y1 ) {
     if(*Npts < MAXPTS + 1) {
       Xpt[*Npts] = X1;
       Ypt[(*Npts)++] = Y1;
       Xpt[*Npts] = X2;
       Ypt[(*Npts)++] = Y2;
     } else *Npts = -1;
     return;
   } else {
    Xfact = (double)(X2-X1) / (double)(Y2-Y1); 
  }
  if ( ( Y1> Y2) ) { 
      Ylow = Y2;
      Yhigh = Y1 ;
    } else {
      Ylow = Y1;
      Yhigh= Y2;
  }

/* Store the line segment, making sure there is not more than one X
  value for any given Y (unless Y1 = Y2, in which case only the two
  endpoints should be saved).
*/
/* Make sure the entire line isn't out of bounds */
  if ( (Ylow <= Gymax) && ( Yhigh>= Gymin) ) {
    for ( Y = Ylow ;Y <= Yhigh; Y++  ) {
        X = X1 + round((Y-Y1) * Xfact);
      if ( (*Npts <= MAXPTS) ) {
        Xpt[*Npts] = X;
        Ypt[*Npts] = Y;
      }
      (*Npts)++;
    }  /* for Y */
  } /* if Ylow... */

/* Flag error condition if array dimension exceeded */
  if ( (*Npts > MAXPTS) )
    *Npts = -1;
}  /* Procedure Storline */
