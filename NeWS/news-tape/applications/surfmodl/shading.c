#include "surfmodl.h"
#include "math.h"
/* type vector = array[1..3] of real; */

void normal(V)
   double *V;
{
    double Vmag;                /* magnitude of the vector */
    int J;                /* index */
  Vmag = sqrt (sqr (V[0]) + sqr (V[1]) + sqr (V[2]) );
  if ( (Vmag > 0.0) )
    for ( J = 0 ; J < 3; J++ )
      V[J] = V[J] / Vmag;
} /* procedure NORMAL */

/* Common variable for SETSHADE and SHADING */
 vector S[MAXLITE][3];   /* light source vectors */

void setshade()
{
    int Lite;
    int J;

  for ( Lite = 0 ; Lite < Nlite; Lite++ ) {
    S[Lite][0] = Xlite[Lite] - Xfocal;
    S[Lite][1] = Ylite[Lite] - Yfocal;
    S[Lite][2] = Zlite[Lite] - Zfocal;
    normal (S[Lite]);
  }
} /* procedure SETSHADE */

double shading(Surf,Side)
int Surf,Side;
{
    vector A[3];                /* vector from 1st to 2nd node of surface */
    vector B[3];                /* vector from 1st to 3rd node of surface */
    vector N[3];                /* vector normal to surface */
    vector E[3];                /* vector from 1st node to eye */
    vector D[3];                /* difference from source to surface normal */
    vector R[3];                /* vector from 1st node to reflected light */
    int J;               /* index */
    int Node1;           /* 1st node # */
    int Node2;           /* 2nd node # */
    int Node3;           /* 3rd node # */
    double Vmag;               /* magnitude of vector, reflected lite to eye */
    double Cumshade;           /* cumulative shade (from multiple light sources)*/
    int Lite;            /* light source number */
    double CosN;               /* cosine of angle from light to surface normal */
    double CosS;               /* cosine of angle from reflected light to eye */
    double Shading;


  if ( (Side == 1) ) {
    Node1 = konnec (Surf, 0) ;
    Node2 = konnec (Surf, 1) ;
    Node3 = konnec (Surf, 2) ;
  }  else {
    Node1 = konnec (Surf, 0) ;
    Node2 = konnec (Surf, 2) ;
    Node3 = konnec (Surf, 1) ;
  }
  A[0] = Xworld[Node2] - Xworld[Node1];
  A[1] = Yworld[Node2] - Yworld[Node1];
  A[2] = Zworld[Node2] - Zworld[Node1];
  B[0] = Xworld[Node3] - Xworld[Node1];
  B[1] = Yworld[Node3] - Yworld[Node1];
  B[2] = Zworld[Node3] - Zworld[Node1];

/* Vector cross product N = A X B */
  N[0] = A[1]*B[2] - A[2]*B[1];
  N[1] = A[2]*B[0] - A[0]*B[2];
  N[2] = A[0]*B[1] - A[1]*B[0];
  normal(N);

  E[0] = Xeye - Xworld[Node1];
  E[1] = Yeye - Yworld[Node1];
  E[2] = Zeye - Zworld[Node1];
  normal(E);

/* Is surface visible to eye? */
  if ( (E[0]*N[0] + E[1]*N[1] + E[2]*N[2] < 0.0) )
    Shading = -1.0;
  else {
    Cumshade = Ambient[Matl[Surf]];
    for ( Lite = 0 ; Lite < Nlite; Lite++ ) {
      for (J  = 0 ; J < 3 ; J++)
        D[J] = S[Lite][J] - N[J];
      /* Does surface face away from light source? */
      CosN = S[Lite][0]*N[0] + S[Lite][1]*N[1] + S[Lite][2]*N[2];
      if ( (CosN < 0.0) )
        /* Cumshade := Cumshade + 0.0;*/ /* this light source doesn't contribute*/
	;
       else {
        for ( J = 0 ; J < 3 ; J++)
          R[J] = N[J] - D[J];
        normal(R);
        /* Find magnitude of vector from reflected light to eye (divided by 2) */
        Vmag = sqrt (sqr(E[0]-R[0]) + sqr(E[1]-R[1]) + sqr(E[2]-R[2])) / 2.0;
        if ( (Vmag > 1.0) )
          Vmag = 1.0;
        CosS = 1.0 - sqr(Vmag);
        Cumshade = Cumshade + Intensity[Lite] * (R1[Matl[Surf]] *
               pow(CosS, R2[Matl[Surf]]) + R3[Matl[Surf]] * CosN);
      } /* if sqr(D[1]... */
    } /* for Lite */
    Shading = Cumshade;
  } /* if sqr(E[1]... */
  return(Shading);
} /* function SHADING */

boolean visible(Surf,Side)
int Surf,Side;
{
    vector A[3];                /* vector from 1st to 2nd node of surface */
    vector B[3];                /* vector from 1st to 3rd node of surface */
    vector N[3];                /* vector normal to surface */
    vector E[3];                /* vector from 1st node to eye */
    int Node1;           /* 1st node # */
    int Node2;           /* 2nd node # */
    int Node3;           /* 3rd node # */
    int Visible;

  if ( (Side == 1) ) {
    Node1 = konnec (Surf, 0) ;
    Node2 = konnec (Surf, 1) ;
    Node3 = konnec (Surf, 2) ;
  }  else {
    Node1 = konnec (Surf, 2) ;
    Node2 = konnec (Surf, 1) ;
    Node3 = konnec (Surf, 0) ;
  }
  A[0] = Xworld[Node2] - Xworld[Node1];
  A[1] = Yworld[Node2] - Yworld[Node1];
  A[2] = Zworld[Node2] - Zworld[Node1];
  B[0] = Xworld[Node3] - Xworld[Node1];
  B[1] = Yworld[Node3] - Yworld[Node1];
  B[2] = Zworld[Node3] - Zworld[Node1];

/* Vector cross product N = A X B */
  N[0] = A[1]*B[2] - A[2]*B[1];
  N[1] = A[2]*B[0] - A[0]*B[2];
  N[2] = A[0]*B[1] - A[1]*B[0];
  normal(N);

  E[0] = Xeye - Xworld[Node1];
  E[1] = Yeye - Yworld[Node1];
  E[2] = Zeye - Zworld[Node1];
  normal(E);

/* Is surface visible to eye? */
  if ( (E[0]*N[0] + E[1]*N[1] + E[2]*N[2] < 0.0) )
    Visible = FALSE;
  else
    Visible = TRUE;
  return Visible;
} /* function VISIBLE */

