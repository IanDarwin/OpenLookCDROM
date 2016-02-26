#include "surfmodl.h"
#include <math.h>
/* The following variables are shared between Setorigin and Perspective */
    double D;     /* distance from eye to point viewed */
    double Cal, Cbe, Cga;     /* cosines of the angles */
    double R;
    boolean Zyflag;  /* true if the view is closely oriented to the
                               X-Y plane */

void setorigin()
{
    double Al, Be, Ga;      /* viewing angles */
    double Sga, Sbe;      /* sines of the viewing angles */

  if ( (Viewtype == 0) ) {
    D = sqrt (sqr(Xfocal-Xeye) + sqr(Yfocal-Yeye) + sqr(Zfocal-Zeye));
    Cal = (Xfocal-Xeye) / D;
    Cbe = (Yfocal-Yeye) / D;
    Cga = (Zfocal-Zeye) / D;
    Zphi = acos(-Cga);
    Ztheta = 0;
    R = D * sin(Zphi) ;
    if (R != 0) Ztheta = acos((Xeye - Xfocal) / R);
    Al = acos (Cal);
    Be = acos (Cbe);
    Ga = acos (Cga);
    Sga = sin (Ga);
    if ( (Sga >= 0.0001) ) {
      R = 1 / Sga;
      Zyflag = TRUE;
    }  else {
      Sbe = sin(Be);
      R = 1 / Sbe;
      Zyflag = FALSE;
    }
  } /* if Viewtype */
}  /* procedure setorigin */

void perspect(X,Y,Z,Xt,Yt,Zt)
double X,Y,Z;
double *Xt,*Yt,*Zt;
{
 double Dnm;       /* denominator of the general fraction */

  if ( (Viewtype == 0) ) {
    Dnm = (X - Xeye)*Cal + (Y - Yeye)*Cbe + (Z - Zeye)*Cga;
    if ( (Dnm == 0.0) )
      Dnm = 0.0001;
    *Zt = D / Dnm;
    if (Zyflag ) {
      *Xt = ((Xeye +*Zt *(X-Xeye) - Xfocal)*Cbe - (Yeye + *Zt*(Y-Yeye) - Yfocal)
            *Cal) * R;
      *Yt = (Zeye + *Zt*(Z-Zeye) - Zfocal) * R;
    }  else {
      *Xt = ((Zeye + *Zt*(Z-Zeye) - Zfocal)*Cal - (Xeye + *Zt*(X-Xeye) - Xfocal)
            *Cga) * R;
      *Yt = (Yeye + *Zt*(Y-Yeye) - Yfocal) * R;
    }
  }  else if ( (Viewtype == 1) ) {
    *Xt = X ;
    *Yt = Y;
    *Zt  = -Z;
  }  else if ( (Viewtype == 2) ) {
    *Xt = X;
    *Yt = Z;
    *Zt = Y;
  }  else {
    *Xt = Y;
    *Yt = Z;
    *Zt = -X;
  } /* if Viewtype */
}  /* procedure Perspect */
