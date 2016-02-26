#include "surfmodl.h"
void surface()
{
    int Node;       /* node # */
    int Surf;	    /* surface # */

    char Cont;
    double Shade;          /* shade of surface */
    int Node1;       /* 1st node of surface */
    static nodearray *Shades;     /* shade at each node */
    int Count;       /* # vertices in shadow */
    int Vert;       /* vertex # */
    double Xtmp, Ytmp;
    char *malloc();
    double shading();

    if (Shades)
       Shades = (nodearray *)realloc(Shades,Nnodes*sizeof(nodearray));
    else
       Shades = (nodearray *)malloc(Nnodes*sizeof(nodearray));

  if ( (Shadowing) )
    shadows (Shades);
  else
    for ( Node = 0 ; Node < Nnodes ; Node++)
      Shades[Node] = 0.0;

  if ( (Viewchanged()) || (Shadowing) || (! Sorted) ) {
    if (Surfmin)
        Surfmin = (surfaces *)realloc(Surfmin,Nsurf*sizeof(surfaces));
    else 
        Surfmin = (surfaces *)malloc(Nsurf*sizeof(surfaces));
    if (Surfmax) 
	Surfmax = (surfaces *)realloc(Surfmax,Nsurf*sizeof(surfaces));
    else
	Surfmax = (surfaces *)malloc(Nsurf*sizeof(surfaces));
    menumsg ("Transforming to 2-D...");
/* Transform from 3-D to 2-D coordinates */
    setorigin();
    for ( Node = 0 ; Node < Nnodes ; Node++ )
      perspect (Xworld[Node], Yworld[Node], Zworld[Node],
                &Xtran[Node],  &Ytran[Node],  &Ztran[Node]);

/* Set plotting limits and normalize transformed coords to screen coords */
    perspect (Xfocal, Yfocal, Zfocal, &Xfotran, &Yfotran, &Zfotran);
    if ( (! setnormal (Xfotran, Yfotran, &XYmax)) ) {
      menumsg ("Warning: Focal point is off the screen.\n");
    }
    cutplane(Zcutnear,&Ztrannear);

/* Normalize all the nodes */
    for ( Node = 0 ; Node < Nnodes ; Node++)
      normalize (&Xtran[Node], &Ytran[Node], Xfotran, Yfotran, XYmax);

    menumsg ("Sorting surfaces...");
    minmax (Surfmin, Surfmax, Nsurf);
    shelsurf (Surfmin, Surfmax, Nsurf);
    Sorted = TRUE;
  } /* if Viewchanged */

  setshade();                            /* Setup for shading calculations */
  setgmode();
  for ( Surf = 0 ; Surf < Nsurf ; Surf++) {
    Count = 0;
    if (Surfmax[Surf] < Ztrannear ) { 
    if ( (Shadowing) ) { 
    for ( Vert = 0 ; Vert < Nvert[Surf] ; Vert++) 
	if ( (Shades[konnec (Surf, Vert)] < 0.0) )
            Count++;
    }
/* In a shadow if any vertex of the surface is in shadow */
    if ( (Count < 1) ) {
      /* Not in shadow */
      Node1 = konnec (Surf, 0) - 1;
      if ( (Nsides == 2) ) {
        /* do the secondary surface first, if desired */
        Shade = shading (Surf, 2);
        if ( (Shade >= 0.0) )
          fillsurf (Surf, Color[Matl[Surf]], Saturation[Matl[Surf]],Shade);
      }
      Shade = shading (Surf, 1);
      if ( (Shade >= 0.0) )
        fillsurf (Surf, Color[Matl[Surf]], Saturation[Matl[Surf]],Shade);
    }  else
      fillsurf (Surf, Color[Matl[Surf]], Saturation[Matl[Surf]],Ambient[Matl[Surf]]);
  }
  }

  drawaxes (Xfotran, Yfotran, XYmax);
  leavegraphic();
} /*procedure SURFACE */

