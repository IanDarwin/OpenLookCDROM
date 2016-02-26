#include "surfmodl.h"
#include <values.h>
/* SETNORMAL and NORMALIZE were written by Joseph Mackey */
/* Variables shared between NORMALIZE and SETNORMAL: */
 double Xfactor,Yfactor;    /* factors for XYadjust */
 double fabs();

boolean setnormal(Xfotran,Yfotran,XYmax)
double Xfotran,Yfotran,*XYmax;
{

  int i;                     /*counter for traversing trans arrays*/
  double Xmin	;      /*minimum and maximum x & y values*/
  double Ymin	;      /*minimum and maximum x & y values*/
  double Xmax	;      /*minimum and maximum x & y values*/
  double Ymax	;      /*minimum and maximum x & y values*/
  boolean setnorm;

 /* setnormal */

  
  setnorm = TRUE;
  Xmax = -MAXFLOAT;
  Xmin = MAXFLOAT;
  Ymax = -MAXFLOAT;
  Ymin = MAXFLOAT;
  if (*XYmax == 0) *XYmax = -MAXFLOAT;

  if (XYadjust> 1.0 ) {
    Xfactor = 1.0;
    Yfactor = 1.0 / XYadjust ;
  }  else {
    Xfactor = XYadjust;
    Yfactor = 1.0;
  }

  for ( i=0 ; i < Nnodes; i++ ) {
    if (Xtran [i]>Xmax )
      Xmax=Xtran[i];
    if ( Xtran[i]<Xmin )
      Xmin=Xtran[i];
    if ( Ytran[i]>Ymax )
      Ymax=Ytran[i];
    if ( Ytran[i]<Ymin )
      Ymin=Ytran[i];
  }

  if ( (Xfotran>Xmax) || (Xfotran<Xmin) || (Yfotran>Ymax) || (Yfotran<Ymin) )
    setnorm = FALSE;

  if (*XYmax == -MAXFLOAT) {

      for ( i=0 ; i < Nnodes ; i++) {
         if ( fabs(Xtran[i]-Xfotran) > *XYmax )
             *XYmax = fabs(Xtran[i]-Xfotran);
         if (fabs(Ytran[i]-Yfotran) > *XYmax )
             *XYmax = fabs(Ytran[i]-Yfotran);
      }
  }
  return setnorm;
} /* function Setnormal */

void normalize(Xtr,Ytr,Xfotran,Yfotran,XYmax)
double *Xtr,*Ytr;
double Xfotran,Yfotran,XYmax;
{
/*   Note: 0.35333 is 0.5 * (6.625 / 9.375).
    9.375 is X dimension of screen in inches; 6.625 is Y dimension. */
    *Xtr = (Gxmax+Gxmin) / 2 -
        round((Xfotran-*Xtr)*0.5*(Gxmax-Gxmin)*Magnify*Xfactor/XYmax);
    *Ytr = (Gymax+Gymin) / 2 -
        round((Yfotran-*Ytr)*0.5*(Gymax-Gymin)*Magnify*Yfactor/XYmax);
} /* procedure Normalize*/

