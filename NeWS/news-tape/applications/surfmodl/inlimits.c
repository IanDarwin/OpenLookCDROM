#include "surfmodl.h"
#include "values.h"

/* determine if point (X,Y) is inside Surface Surf and 
	define bounding box: (Xmin,Ymin) to (Xmax,Ymax)
 */

boolean inlimits(X,Y,Surf)
int X,Y,Surf;
{
    int Node;
    int Vert;
    int Xt, Yt;
    boolean Inlimits;

  Xmin = MAXFLOAT;
  Xmax = -MAXFLOAT;
  Ymin = MAXFLOAT;
  Ymax = -MAXFLOAT;
  for ( Vert = 0 ; Vert < Nvert[Surf]; Vert++ ) {
    Node = konnec (Surf, Vert);
    Xt = round (Xtran[Node]);
    Yt = round (Ytran[Node]);
    if ( ( Xt< Xmin) )
       Xmin = Xt;
    if ( ( Xt> Xmax) )
       Xmax= Xt;
    if ( ( Yt< Ymin) )
       Ymin= Yt;
    if ( ( Yt> Ymax) )
       Ymax = Yt ;
  }
  if ( ( X>= Xmin) && (X <= Xmax) && ( Y>= Ymin) && ( Y<= Ymax) )
    Inlimits = TRUE;
  else
    Inlimits = FALSE;
  return Inlimits;
} /* Function Inlimits */
