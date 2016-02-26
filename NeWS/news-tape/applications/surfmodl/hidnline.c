#include "surfmodl.h"
#include "math.h"
void hiddenline()
{
    int Node;       /* node # */
    int Surf;       /* surface # */
    char Cont;
    char *malloc();
    double Xtmp,Ytmp;

  if ( (Viewchanged()) || (! Sorted) ) {
    menumsg ("Transforming to 2-D...");
  if (Surfmin)
     Surfmin = (surfaces *)realloc(Surfmin,Nsurf * sizeof(surfaces));
  else
     Surfmin = (surfaces *)malloc(Nsurf * sizeof(surfaces));
  if (Surfmax)
     Surfmax = (surfaces *)realloc(Surfmax,Nsurf * sizeof(surfaces));
  else
     Surfmax = (surfaces *)malloc(Nsurf * sizeof(surfaces));
/* Transform from 3-D to 2-D coordinates */
    setorigin();
    for ( Node = 0 ; Node < Nnodes ; Node++)
      perspect (Xworld[Node], Yworld[Node], Zworld[Node],
                &Xtran[Node],  &Ytran[Node],  &Ztran[Node]);

/* Set plotting limits and normalize transformed coords to screen coords */
    perspect (Xfocal, Yfocal, Zfocal, &Xfotran, &Yfotran, &Zfotran);
    if ( (! setnormal (Xfotran, Yfotran, &XYmax)) ) {
      menumsg ("Warning: Focal point is off the screen.");
    }

/* Transform Clip planes */
    cutplane(Zcutnear,&Ztrannear);

/* Normalize all the nodes */
    for ( Node = 0 ; Node <Nnodes ; Node++)
      normalize (&Xtran[Node], &Ytran[Node], Xfotran, Yfotran, XYmax);

    menumsg ("Sorting surfaces...");
    minmax (Surfmin, Surfmax, Nsurf);
    shelsurf (Surfmin, Surfmax, Nsurf);
    Sorted = TRUE;
  } /* if Viewchanged */

  setgmode();
  for ( Surf = 0 ; Surf < Nsurf ; Surf++)
    if ( ((Nsides == 2) || (visible (Surf, 1)))
	&& (Surfmax[Surf] < Ztrannear) ) {

    /* First fill the surface in with black (white)*/
      gsetgray(1.0);
      gcolor(0.0,0.0);
      Surfpath(Surf);
      gsave();
      gfill();
      grestore();
      gsetgray(0.0);
      gstroke();
  /*    fillsurf (Surf, (double)0, (double)0,(double)0); */

	/* Now draw the border */
     /* border (Surf, Color[Matl[Surf]],Saturation[Matl[Surf]]);  */
    }
  drawaxes (Xfotran, Yfotran, XYmax);
  leavegraphic();
} /*procedure Hiddenline*/

/* Calculate Transformed position of cut plane a distance D 
	from the eyepoint */
cutplane(D,Ztran)
double D, *Ztran;
{
	double Xcut,Ycut,Zcut,Xtmp,Ytmp;
	Xcut = Xeye  - D*sin(Zphi)*cos(Ztheta);
	Ycut = Yeye  - D*sin(Zphi)*sin(Ztheta); 
	Zcut = Zeye  - D*cos(Zphi);
	perspect(Xcut,Ycut,Zcut,&Xtmp,&Ytmp,Ztran);
} /* function cutplane */
