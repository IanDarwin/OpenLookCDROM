#include "preproc.h"

/* Read an existing SOLMODL data file, and reflect all the data across
  the X axis. */

/* CONST */ 
#define  MAXNODES  1000      /* maximum # of nodes in the entire solid */
#define  MAXCONNECT  3000    /* maximum # of connections in entire solid */
#define  MAXSURF  1000       /* maximum # of surfaces in entire solid */
                            /* (MAXSURF = MAXCONNECT / 3) */
#define  MAXMATL  30         /* maximum # of materials in entire solid */
#define  MAXVAR  20          /* maximum # of numeric inputs on a line */
#define  MAXLITE  20         /* maximum # of light sources */

       typedef char text80;
       typedef double vartype[MAXVAR+1];
       typedef double nodearray[MAXNODES+1];

   nodearray Xworld, Yworld, Zworld;
      /* world coordinates of each node */
      int Connect[MAXCONNECT+1];
      /* surface connectivity data */
      int Nvert[MAXSURF+1];
      /* # vertices per surface */
      int Matl[MAXSURF+1];
      /* material number of each surface */
      double R1[MAXMATL+1], R2[MAXMATL+1], R3[MAXMATL+1];
      /* material reflectivity constants */
      int Color[MAXMATL+1];
      /* material color number */
      double Xlite[MAXLITE+1], Ylite[MAXLITE+1], Zlite[MAXLITE+1];
      /* coords of light sources */
      double Intensity[MAXLITE+1];
      /* light source intensities */

      char Flpurpose[128];              /* title for plot */
      int Maxvert;                    /* max # vertices per surface */
      int Nsurf;                      /* # surfaces */
      int Nnodes;                     /* # nodes */
      int Nlite;                      /* # light sources */
      int Nmatl;                      /* number of materials */
      int Nsides;                     /* #sides of surface used (1 or 2)*/

/* Two important functions for decoding the Connect array: */

int KONNEC(Surf,Vert)
int Surf,Vert;
{
  Konnec = Connect[(Surf-1) * Maxvert + Vert];
}; /* function Konnec */

/* Procedure include files */
/*$I INREAL.PAS */                         /* procedure INREAL */
/*$I READFILE.PAS */                       /* procedure READFILE */

    FILE * Outfile;
    int Mat;
    int Node;
    int Surf;
    int Vert;

 main()
{
  /*LINE*/printf ("Reading file ", paramstr(1));
  readfile (paramstr(1));
  assign (Outfile, paramstr(2));
  /*LINE*/printf ("Writing file ", paramstr(2));
  rewrite (Outfile);
  /*LINE*/printf (Outfile, Flpurpose);
  /*LINE*/printf (Outfile, 3);
  /*LINE*/printf (Outfile, Nmatl:3, Nnodes*2:5, Nsurf*2:5, Maxvert:3, Nsides:3);
  for ( Mat = 1 ; Nmatl )
    /*LINE*/printf (Outfile, R1[Mat]:7:3, R2[Mat]:7:3, R3[Mat]:7:3, " ",Color[Mat]:3);
  for ( Node = 1 ; Nnodes )
    /*LINE*/printf (Outfile, Xworld[Node]:7:3, Yworld[Node]:7:3, Zworld[Node]:7:3);

/* Reflecting Nodes */
  for ( Node = 1 ; Nnodes )
    /*LINE*/printf (Outfile, -Xworld[Node]:7:3, Yworld[Node]:7:3, Zworld[Node]:7:3);
  for ( Surf = 1 ; Nsurf ) {
    printf (Outfile, Nvert[Surf]:3, Matl[Surf]:3);
    for ( Vert = 1 ; Nvert[Surf] )
      printf (Outfile, konnec (Surf, Vert):5);
    /*LINE*/printf (Outfile);
  }
/* Reflecting Surfaces */
  for ( Surf = 1 ; Nsurf ) {
    printf (Outfile, Nvert[Surf]:3, Matl[Surf]:3);
    for ( Vert = 1 ; Nvert[Surf] ) {
      Node = konnec (Surf, Nvert[Surf]-Vert+1);
/* If node has X=0 then use the old node number instead of the new.
  Makes interpolation across X=0 possible. */
      if ( (Xworld[Node] != 0.0) )
        printf (Outfile, (Node + Nnodes):5)
      ; else
        printf (Outfile, Node:5);
    }
    /*LINE*/printf (Outfile);
  }
  close (Outfile);
} /* Program Reflect */
