#include "surfmodl.h"
void Storshades(X1,Y1,X2,Y2,Shade1,Shade2,Xpt,Ypt,Shpt,Npts)
int X1,Y1,X2,Y2;
double Shade1,Shade2;
points *Xpt,*Ypt;
realpts *Shpt;
int *Npts;
{
    int X, Y;                   /* current point being stored */
    double Xfact;                     /* factor for (X,Y) interpolation */
    double Shfact;                    /* factor for shade interpolation */
    int Ylow, Yhigh;            /* range of for loop */
    boolean Firstx;                 /* flag first dot of line */
    boolean Firstsh;                /* flag first shade of line */
    double Shade;                     /* shade at each pixel */

  Firstx = TRUE;
  Firstsh = TRUE;
  if (Y2 ==Y1 ) {	/* store only the two endpoints */
     if ( *Npts < (MAXPTS+ 1)) {
       Xpt[*Npts] = X1;
       Ypt[*Npts] = Y1;
       Shpt[(*Npts)++] = Shade1;
       Xpt[*Npts] = X2;
       Ypt[*Npts] = Y2;
       Shpt[(*Npts)++] = Shade2;
     } else *Npts = -1;
     return;
  } else {
     Xfact= (double)(X2-X1) / (double)(Y2-Y1);
  }
  if ( Y1>Y2 ) {
      Ylow= Y2;
      Yhigh= Y1;
    }  else {
      Ylow =Y1 ;
      Yhigh =Y2 ;
  }
  Shfact = (Shade2 - Shade1) / (Y2 - Y1);

/* Store the line segment, making sure there is not more than one X
  value for any given Y (unless Y1 = Y2, in which case only the two
  endpoints should be saved).
*/
/* Make sure the entire line isn't out of bounds */
  if ( ( Ylow<=Gymax ) && ( Yhigh>= Gymin) ) {
    for ( Y = Ylow ;Y <= Yhigh; Y++  ) {
      X = X1 + round((Y-Y1) * Xfact);
      Shade = Shade1 + (Y - Y1) * Shfact;
      if ( *Npts <= MAXPTS ) {
        Xpt[*Npts] = X;
        Ypt[*Npts] = Y;
        Shpt[*Npts] = Shade;
      }
      (*Npts)++;
    }  /* for Y */
  } /* if Ylow... */

/* Flag error condition if array dimension exceeded */
  if ( (*Npts > MAXPTS) )
    *Npts = -1;
}  /* Procedure Storshades */
