#include "surfmodl.h"
void wireframe()
{
    int Node;       /* node # */
    int Surf;       /* surface # */

  if ( Viewchanged() ) {
    menumsg ("Transforming to 2-D...");
/* Transform from 3-D to 2-D coordinates */
    setorigin();
    for ( Node = 0 ; Node < Nnodes ; Node++)
      perspect (Xworld[Node], Yworld[Node], Zworld[Node],
                &Xtran[Node],  &Ytran[Node],  &Ztran[Node]);

/* Set plotting limits and normalize transformed coords to screen coords */
    perspect (Xfocal, Yfocal, Zfocal, &Xfotran, &Yfotran, &Zfotran);
    if ( (! setnormal (Xfotran, Yfotran, &XYmax)) ) {
      menumsg ("Warning: Focal point is off the screen.\n");
    }

/* Normalize all the nodes */
    for ( Node = 0 ; Node < Nnodes; Node++ )
      normalize (&Xtran[Node], &Ytran[Node], Xfotran, Yfotran, XYmax);
  } /* if Viewchanged */

/* Draw the outline of each surface */
  setgmode();
  for ( Surf = 0 ; Surf < Nsurf ; Surf++)
    border (Surf, Color[Matl[Surf]],Saturation[Matl[Surf]]);
  drawaxes (Xfotran, Yfotran, XYmax);
  leavegraphic();
} /*procedure Wireframe*/

