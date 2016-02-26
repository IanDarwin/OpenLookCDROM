#include "surfmodl.h"
void badsurf()
{
  leavegraphic();
  printf ("Error: You have attempted to plot a concave surface.\n");
  printf ("  This surface should be broken into at least two smaller\n");
  printf ("  surfaces. Alternatively, you may possibly be able to\n");
  printf ("  plot this surface anyway from a different angle or\n");
  printf ("  with a lower magnification factor.\n");
  exit();
};  /* procedure BADSURF */


void fillsurf(Surf,Color,Saturation,Shade)
int Surf;
double Color,Saturation;
double Shade;
{
    points *Xpt, *Ypt;            /* pts on edges of surface */
    int Vert;               /* vertex number */
    int Pcolor;             /* actual color to plot with */
    int Fmod;               /* mod for filling function */
    int Ishade;             /* int version of shade (0..16) */
    char *malloc();


  if ( (onscreen (Surf)) ) {


  /* define fill path for surface */

    gsetgray((float)Shade);
    gcolor(Color,Saturation);

    Surfpath(Surf);

    gfill();

    return;

  } /* if onscreen */
} /* procedure FILLSURF */

/* create path around surface of polygon */

Surfpath(Surf)
int Surf;
{
    int Node1;
    int Vert;

    Node1 = konnec(Surf,Nvert[Surf] - 1);

    gmoveto(round(Xtran[Node1]),round(Ytran[Node1]));

    for ( Vert = 0 ; Vert < Nvert[Surf]; Vert++ ) {
      Node1 = konnec (Surf, Vert) ;
      glineto(round(Xtran[Node1]),round(Ytran[Node1]));
    } /* for Vert */

} /* end Surfpath */
