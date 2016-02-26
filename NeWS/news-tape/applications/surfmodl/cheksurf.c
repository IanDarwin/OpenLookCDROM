#include "surfmodl.h"
boolean cheksurf(X,Y,Surf)
int X,Y,Surf;
{
 int Npts;        /* # points on outline of surface */
 points *Xpt, *Ypt;     /* coordinates of surface outline */
 int Nextpt;      /* next point on outline to look at */
 int Node1, Node2;/* endpoints of line segment to store */
 int Vert;        /* vertex number */
 boolean Cheksurf;
 char *malloc();

 Xpt = (points *)malloc(MAXPTS * sizeof(points));
 Ypt = (points *)malloc(MAXPTS * sizeof(points));

  if ( inlimits (X, Y, Surf) ) {
    Npts = 0;
    for ( Vert = 0 ; Vert < Nvert[Surf]-1; Vert++ ) {
      Node1 = konnec (Surf, Vert) ;
      Node2 = konnec (Surf, Vert+1) ;
      storline (round(Xtran[Node1]), round(Ytran[Node1]),
                round(Xtran[Node2]), round(Ytran[Node2]), Xpt, Ypt, &Npts);
      if ( (Npts < 0) )
        badsurf();
    } /* for Vert */
/* One last line to close the polygon */
    Node1 = konnec (Surf, Nvert[Surf] - 1) ;                /* last node */
    Node2 = konnec (Surf, 0) ;                          /* first node */
    storline (round(Xtran[Node1]), round(Ytran[Node1]),
              round(Xtran[Node2]), round(Ytran[Node2]), Xpt, Ypt, &Npts);
    if ( (Npts < 0) )
      badsurf();

/* Sort the line segment points, first by Y, then by X */
    Shellpts (Xpt, Ypt, Npts);

/* Now check every point in the interior of the surface to find (X,Y) */
    Nextpt = 0;
    while ( (Nextpt < Npts - 1) && (Nextpt >= 0) ) {
      if ( (Ypt[Nextpt] == Y) ) {
        if ( (abs(Xpt[Nextpt] - Xpt[Nextpt+1]) > 1) &&
            (Ypt[Nextpt] == Ypt[Nextpt+1]) ) {
          if ( (Xpt[Nextpt] <= X) && (Xpt[Nextpt+1] >= X) )
            /* Point found; flag to stop the while loop */
            Nextpt = -1;
           else
            Nextpt = Nextpt + 2;
        }  else if ( (Xpt[Nextpt] == X) )
          /* Point found; flag to stop the while loop */
          Nextpt = -1;
         else
          Nextpt++;
      }  else /* if Ypt */
        Nextpt++;
    } /* while */
    if ( (Nextpt == Npts - 1) )
      if ( (Xpt[Nextpt] == X) )
        /* Point found; flag to stop the while loop */
        Nextpt = -1;
    if ( Nextpt == -1 )
      Cheksurf = TRUE;
     else
      Cheksurf = FALSE;
  }  else /* if onscreen */
    Cheksurf = FALSE;
  free(Xpt); free(Ypt);
  return Cheksurf;
} /* Function Cheksurf */
