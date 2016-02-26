#include "surfmodl.h"
#include "math.h"
#include <values.h>
void minmax(Surfmin,Surfmax,Nsurf)
surfaces *Surfmin,*Surfmax;
int Nsurf;
{
    int Surf;                   /* surface # */
    int Vert;                   /* vertex # */
    double Zval;                      /* Z-coord of node */

  for ( Surf = 0 ; Surf < Nsurf ; Surf++) {
    Surfmin[Surf] = MAXFLOAT;
    Surfmax[Surf] = -MAXFLOAT;
    for ( Vert = 0 ; Vert < Nvert[Surf]; Vert++ ) {
       Zval = Ztran[konnec (Surf, Vert)];
      if ( (Zval < Surfmin[Surf]) )
        Surfmin[Surf] = Zval;
      if ( (Zval > Surfmax[Surf]) )
        Surfmax[Surf] = Zval;
    }
  }
} /* procedure Minmax */
