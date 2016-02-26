#include "surfmodl.h"
boolean onscreen(Surf)
int Surf;
{
    int Vert;                    /* vertex # */
    int Node;                    /* node # of vertex */
    boolean On;                      /* result to be returned */
    boolean Onscreen;

  if ( (Magnify <= 1.0) )
    /* If no magnification, then by definition all surfaces are on the
      screen */
    Onscreen = TRUE;
  else {
    Vert = 0;
    On = FALSE;
    while ( (Vert <= Nvert[Surf]) && (! On) ) {
      Node = konnec (Surf, Vert) ;
      if ( (Xtran[Node] >= Gxmin) )
        On = TRUE;
      Vert++;
    }
    if ( (! On) )
      Onscreen = FALSE;
    else {
      Vert = 0;
      On = FALSE;
      while ( (Vert <= Nvert[Surf]) && (! On) ) {
        Node = konnec (Surf, Vert) ;
        if ( (Xtran[Node] <= Gxmax) )
          On = TRUE;
        Vert++;
      }
      if ( (! On) )
        Onscreen = FALSE;
      else {
        Vert = 0;
        On = FALSE;
        while ( (Vert <= Nvert[Surf]) && (! On) ) {
          Node = konnec (Surf, Vert) ;
          if ( (Xtran[Node] >= Gymin) )
            On = TRUE;
          Vert++;
        }
        if ( (! On) )
          Onscreen = FALSE;
        else {
          Vert = 0;
          On = FALSE;
          while ( (Vert <= Nvert[Surf]) && (! On) ) {
            Node = konnec (Surf, Vert) ;
            if ( (Xtran[Node] <= Gymax) )
              On = TRUE;
            Vert++;
          }
          if ( (! On) )
            Onscreen = FALSE;
          else
            Onscreen = TRUE;
        } /* Gymax */
      } /* Gymin */
    } /* Gxmax */
  } /* Gxmin */
  return Onscreen;
} /* procedure Onscreen */
