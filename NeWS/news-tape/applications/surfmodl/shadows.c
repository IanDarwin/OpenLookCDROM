#include "surfmodl.h"
void shadows(Shades)
nodearray *Shades;
{
    int Node;       /* node # */
    int Surf;       /* surface # */
    double Xfotran;          /* transformed focal pt. */
    double Yfotran;          /* transformed focal pt. */
    double Zfotran;          /* transformed focal pt. */
    double XYmax;          /* max coordinate */
    double Txeye;          /* temps for eye coords */
    double Tyeye;          /* temps for eye coords */
    double Tzeye;          /* temps for eye coords */
    int Tgxmin;  /* temps for plot limits */
    int Tgxmax;  /* temps for plot limits */
    int Tgymin;  /* temps for plot limits */
    int Tgymax;  /* temps for plot limits */
    double Tmagnify;          /* temp for magnification */
    boolean Result;       /* result of normalize setup */
    char *malloc();
    boolean insurf();



  menumsg ("Computing shadows...");
/* Preserve the old status before changing to shadowing values */
  Txeye = Xeye;
  Tyeye = Yeye;
  Tzeye = Zeye;
  Tgxmin = Gxmin;
  Tgxmax = Gxmax;
  Tgymin = Gymin;
  Tgymax = Gymax;
  Tmagnify = Magnify;

/* New values to transform all nodes/surfaces as if they were being seen
  by the primary light source. NOTE: This only works with a single light
  source!
*/
  Xeye = Xlite[0];
  Yeye = Ylite[0];
  Zeye = Zlite[0];
  Gxmin = 0;         /* shorten search time by using a coarser grid */
  Gxmax = 100;
  Gymin = 0;
  Gymax = 100;
  Magnify = 1.0;

/* Transform from 3-D to 2-D coordinates */
  setorigin();
  for ( Node = 0 ; Node < Nnodes ; Node++ )
    perspect (Xworld[Node], Yworld[Node], Zworld[Node],
                 &Xtran[Node],  &Ytran[Node],  &Ztran[Node]);

/* Set plotting limits and normalize transformed coords to shadow-test coords */
  perspect (Xfocal, Yfocal, Zfocal, &Xfotran, &Yfotran, &Zfotran);
  Result = setnormal (Xfotran, Yfotran, &XYmax);

/* Normalize all the nodes */
  for ( Node = 0 ; Node < Nnodes ; Node++ ) {
    normalize (&Xtran[Node], &Ytran[Node], Xfotran, Yfotran, XYmax);
    /* Initialize the Shades to zero */
    Shades[Node] = 0.0;
  }
  minmax (Surfmin, Surfmax, Nsurf);
  shelsurf (Surfmin, Surfmax, Nsurf);

  for ( Node = 0 ; Node < Nnodes ; Node++ ) {
    /* Check every surface closer to the light source to see if it blocks
    the light from this node. */
    Surf = Nsurf -1;
    while ( (Surf >= 0) ) {
      if ( (Surfmax[Surf] <= Ztran[Node]) )
        /* Gone past this node; flag that none of the remaining surfaces can
        possibly be in front of it. */
        Surf = -1;
      else if ( (Surfmin[Surf] > Ztran[Node]) ) {
        /* Only check surfaces that are completely in front of the node
        (avoids surfaces that contain the node being considered in front
        of the node) */
        if ( (insurf (round(Xtran[Node]),round(Ytran[Node]),Surf)) ) {
          Shades[Node] = -1.0;
          /* Flag to stop the loop */
          Surf = -1;
        }
      } /* if Surfmax */
      Surf--;
    } /* while */
  } /* for Node */

/* Done; now put things back the way we found them */
  Xeye = Txeye;
  Yeye = Tyeye;
  Zeye = Tzeye;
  Gxmin = Tgxmin;
  Gxmax = Tgxmax;
  Gymin = Tgymin;
  Gymax = Tgymax;
  Magnify = Tmagnify;
} /* Procedure Shadows */

/* Function insurf: 
	uses non-zero winding rule to determine if pt (x,y) is in Surf */

#define sign(x) (x < 0? -1 : 1)
#define between(x1,x,x2) (sign(x - x1)*sign(x - x2) < 0)

boolean insurf(X,Y,Surf)
int X,Y,Surf;
{
    int Node1,Node2,Vert,dx,dy,h,Xtmp,Ytmp;
    boolean inside;

    inside = 0;

    if (inlimits (X, Y, Surf)) {

	Node2 = konnec(Surf,Nvert[Surf]-1);

	for (Vert = 0; Vert < Nvert[Surf] ; Vert++) {
	    Node1 = Node2;
	    Node2 = konnec(Surf,Vert);
	    dx = Xtran[Node2] - Xtran[Node1];
	    dy = Ytran[Node2] - Ytran[Node1];
	    h = X * dy - Y * dx;
	    if (h != 0) {

	    /* compute intersection point */

		Xtmp = (Ytran[Node1] * dx - Xtran[Node2] * dy) / h;
		Ytmp = Y * Xtmp / X;
		if (between(Xtran[Node1],Xtmp,Xtran[Node2]) &&
		    between(Ytran[Node1],Ytmp,Ytran[Node2]) )

		/* add sign of intersection direction */

		    inside += sign (h);
	    }
        }
    }
    return(inside);
} /* Function insurf */
