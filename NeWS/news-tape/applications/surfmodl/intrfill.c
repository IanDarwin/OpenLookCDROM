static char *RCSid = "$Header: intrfill.c,v 1.2 87/05/29 07:48:07 dwf Locked $";

/*
 * $Log:	intrfill.c,v $
 * Revision 1.2  87/05/29  07:48:07  dwf
 * Modified for NeWS to do gouraud shading
 * 
 */

#include "surfmodl.h"
void intrfill(Surf,Color,Saturation,Shades)
int Surf;
double Color, Saturation;
nodearray *Shades;
{
    int Npts;               /* #points on edges of the surface */
    int Nextpt;             /* Next point to use for filling */
    int Node1, Node2;       /* node numbers of endpts of line */
    points *Xpt, *Ypt;            /* pts on edges of surface */
    realpts *Shpt;               /* shade at each point */
    int Vert;               /* vertex number */
    double Shade1, Shade2;        /* Shades at endpoint nodes */
    char *malloc();
    int Ysave;

  if ( (onscreen (Surf)) ) {

    Xpt = (points *)malloc(MAXPTS * sizeof(points));
    Ypt = (points *)malloc(MAXPTS * sizeof(points));
    Shpt = (realpts *)malloc(MAXPTS * sizeof(realpts));

 /*   gcolor(Color,Saturation); */
    Npts = 0;
    Node2 = konnec(Surf,Nvert[Surf]-1);
    for ( Vert = 0 ; Vert < Nvert[Surf] ; Vert++ ) {
      Node1 = Node2;
      Node2 = konnec (Surf, Vert) ;
      Shade1 = Shades[Node1];
      Shade2 = Shades[Node2];
      if ( (Shade1 < 0.0) )
        Shade1 = Ambient[Matl[Surf]];
      if ( (Shade2 < 0.0) )
        Shade2 = Ambient[Matl[Surf]];
      Storshades (trunc(Xtran[Node1]), trunc(Ytran[Node1]),
                  trunc(Xtran[Node2]), trunc(Ytran[Node2]),
                  Shade1, Shade2, Xpt, Ypt, Shpt, &Npts);
      if ( (Npts < 0) )
        badsurf();
    } /* for Vert */


/* Sort the line segment points, first by Y, then by X */
    shellshades (Xpt, Ypt, Shpt, Npts);

/* Now draw the filled surface */
    inlimits(0,0,Surf);
    gnewpath();
    Surfpath(Surf);
    gsetimage(Xmin,Ymin,Xmax,Ymax);
    Nextpt = Npts - 1;
    Ysave = 0;
    while ( Nextpt > 0  ) {
      if  (Ypt[Nextpt] == Ypt[Nextpt-1]) { 
         if (Xpt[Nextpt] != Xpt[Nextpt-1])  {
        	intrdraw (Xpt[Nextpt-1], Xpt[Nextpt], Ypt[Nextpt], 
                  (float)Shpt[Nextpt-1], (float)Shpt[Nextpt]);
	        Ysave = Ypt[Nextpt-1];
        	Nextpt -=  2;
	  } else 
		Nextpt-- ; 	/* skip duplicate points */
      }  else  {
	 if(Ypt[Nextpt] != Ysave) /* plot only if row has not been drawn */
             intrplot (Xpt[Nextpt], Ypt[Nextpt], (float)Shpt[Nextpt]); 
         Nextpt--;
      }
    } /* while */
    if ( (Nextpt == 0) )
      intrplot (Xpt[Nextpt], Ypt[Nextpt], (float)Shpt[Nextpt]); 
    gshowimage(); 
    gnewpath();
    free(Xpt); free(Ypt); free(Shpt);
  } /* if onscreen */
} /* Procedure Intrfill */

