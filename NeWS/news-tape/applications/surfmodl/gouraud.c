#include "surfmodl.h"
#include "math.h"
void gouraud()
/* Make a surface model dawing of the object with Gouraud interpolation 
of surface shading */
{
    int Node;       /* node # */
    int Surf;       /* surface # */
    double Xfotran,Yfotran, Zfotran;          /* transformed focal pt. */
    double Shade;          /* shade of surface */
    double Shade2;          /* shade of 2nd side of surface */
    static nodearray *Shades;     /* shade at each node */
    static int *Nshades;   /* # shades to average per node */
    static surfaces *Sshade;      /* shade at each surface */
    int Vert;       /* vertex # */
    boolean Interp;       /* flag interpolated shading */
    char *malloc();
    double shading(),fabs();

/* allocate memory for surface sort ant shades */

  if (Surfmin)
    Surfmin = (surfaces *)realloc(Surfmin,Nsurf * sizeof(surfaces));
  else
    Surfmin = (surfaces *)malloc(Nsurf * sizeof(surfaces));
  if (Surfmax)
    Surfmax = (surfaces *)realloc(Surfmax,Nsurf * sizeof(surfaces));
  else
    Surfmax = (surfaces *)malloc(Nsurf * sizeof(surfaces));
  if (Sshade)
     Sshade = (surfaces *)realloc(Sshade,Nsurf * sizeof(surfaces));
  else
     Sshade = (surfaces *)malloc(Nsurf * sizeof(surfaces));

  if (Nshades)
     Nshades = (int *)realloc(Nshades,Nnodes * sizeof(int));
  else
     Nshades = (int *)malloc(Nnodes * sizeof(int));

  if(Shades)
     Shades = (nodearray *)realloc(Shades,Nnodes * sizeof(nodearray));
  else
     Shades = (nodearray *)malloc(Nnodes * sizeof(nodearray));

  if ( (Shadowing) ) {
    shadows (Shades);
    for ( Node = 0; Node < Nnodes; Node++ )
      Nshades[Node] = 0;
  }  else
    for ( Node = 0 ; Node <Nnodes; Node++ ) {
      Shades[Node] = 0.0;
      Nshades[Node] = 0;
    }

  if ( (Viewchanged()) || (Shadowing) || (! Sorted) ) {
    menumsg ("Transforming to 2-D...");
/* Transform from 3-D to 2-D coordinates */
    setorigin();
    for ( Node = 0 ; Node < Nnodes; Node++ )
      perspect (Xworld[Node], Yworld[Node], Zworld[Node],
                &Xtran[Node],  &Ytran[Node],  &Ztran[Node]);

/* Set plotting limits and normalize transformed coords to screen coords */
    perspect (Xfocal, Yfocal, Zfocal, &Xfotran, &Yfotran, &Zfotran);
    if ( (! setnormal (Xfotran, Yfotran, &XYmax)) ) {
      menumsg ("Warning: Focal point is off the screen.");
    }

/* Normalize all the nodes */
    for ( Node = 0 ; Node < Nnodes ; Node++ )
      normalize (&Xtran[Node], &Ytran[Node], Xfotran, Yfotran, XYmax);
    /* Initialize all nodal shades to zero */

    menumsg ("Sorting surfaces...");
    minmax (Surfmin, Surfmax, Nsurf);
    shelsurf (Surfmin, Surfmax, Nsurf);
    Sorted = TRUE;
  } /* if Viewchanged */

  setshade();                            /* Setup for shading calculations */

/* Compute the cumulative shading at every node (sum the shades due to
  all surrounding surfaces */
  menumsg ("Computing shades...\n");
  for ( Surf = 0 ; Surf < Nsurf; Surf++ ) {
    if ( (Nsides == 2) ) {
      /* Use only the side of the surface with the brightest shade */
      Shade = shading (Surf, 1);
      Shade2 = shading (Surf, 2);
      if ( (Shade2 > Shade) )
        Shade = Shade2;
    } else
      Shade = shading (Surf, 1);
    /* Surface shade */
    Sshade[Surf] = Shade;
    /* Nodal shade */
    for ( Vert = 0 ; Vert < Nvert[Surf] ; Vert++) {
      Node = konnec (Surf, Vert);
      if ( (Shade >= 0.0) && (Shades[Node] >= 0.0) ) {
        Shades[Node] = Shades[Node] + Shade;
        Nshades[Node] = Nshades[Node] + 1;
      }
    } /* for Vert */
  } /* for Surf */

/* Now average out the nodal shading */
  for ( Node = 0 ; Node < Nnodes ; Node++)
    if ( (Nshades[Node] > 0) )
      Shades[Node] = Shades[Node] / Nshades[Node];

/* Now plot all the surfaces, with Gouraud shading */
  setgmode();
  for ( Surf = 0 ; Surf < Nsurf; Surf++ ) {
    if ( (Sshade[Surf] >= 0.0) ) {
      Interp = TRUE;
      /* If any nodal shade varies from the average (surface) shade by more
        than Epsilon, then don't use interpolated shading (unless the node
        is in a shadow, in which case you should interpolate anyway) */
      for ( Vert = 0 ; Vert < Nvert[Surf] ; Vert++) {
        Node = konnec (Surf, Vert);
        if ( (fabs(Shades[Node] - Sshade[Surf]) > Epsilon) &&
           (Shades[Node] >= 0.0) )
          Interp = FALSE;
      }
      if ( (Interp) )
        intrfill (Surf, Color[Matl[Surf]], Saturation[Matl[Surf]],Shades);
       else
        fillsurf (Surf, Color[Matl[Surf]], Saturation[Matl[Surf]],Sshade[Surf]);
    } /* if Sshade */
  } /* for Surf */
  drawaxes (Xfotran, Yfotran, XYmax);
  leavegraphic();
} /*procedure Gouraud*/

