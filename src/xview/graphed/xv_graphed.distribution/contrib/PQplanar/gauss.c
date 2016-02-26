/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : gauss.c                                        */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen.h"
#include "listen1.h"
#include "dfs.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simptrei.h"
#include "simptrei.h"
#include <stdio.h>
#include <math.h>


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN gauss(A,X,B)
        REAL        A[2][2];
        REAL        X[2],B[2];

BEGIN
  INTEGER        i,j;
  REAL                h1,h2;

  i = iif( (fabs(A[0][0]) > fabs(A[1][0])) , 0, 1);
  j = 1-i;

  IF A[i][0] == 0.0 THEN
    RETURN(FALSE);
   ELSE
    h1 = A[j][0] / A[i][0];
    h2 = A[j][1] - h1*A[i][1];
    IF f_equal(h2,0.0) THEN
      RETURN(FALSE);
     ELSE
      X[1] = (B[j] - h1*B[i])/ h2;
      X[0] = (B[i] - A[i][1]*X[1])/A[i][0];
      RETURN(TRUE);
    END;
  END;
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN new_node(x0,y0,w0,x1,y1,w1,p_x2,p_y2)
        INTEGER        x0,y0,x1,y1;
        INTEGER        *p_x2,*p_y2;
        REAL        w0,w1;
BEGIN
  REAL        A[2][2];
  REAL        X[2],B[2];

  B[0] = (REAL)(x1-x0);
  B[1] = (REAL)(y1-y0);
  A[0][0] = COS(w0);
  A[0][1] = COS(w1);
  A[1][0] = SIN(w0);
  A[1][1] = SIN(w1);
  IF gauss(A,X,B) THEN
    *p_x2 = (x0+X[0]*A[0][0]);
    *p_y2 = (y0+X[0]*A[1][0]);
    RETURN(TRUE);
   ELSE
    RETURN(FALSE);
  END;
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN schnitt_gerade_gerade(x0,y0,x1,y1,x2,y2,x3,y3)
        INTEGER        x0,y0,x1,y1;
        INTEGER        x2,y2,x3,y3;
  BEGIN
    REAL        A[2][2];
    REAL        X[2],B[2];

    B[0] = (REAL)(x2-x0);
    B[1] = (REAL)(y2-y0);
    A[0][0] = (REAL)(x1-x0);
    A[1][0] = (REAL)(y1-y0);
    A[0][1] = (REAL)(x2-x3);
    A[1][1] = (REAL)(y2-y3);
    IF gauss(A,X,B) THEN
      IF f_greater(X[0],0.0) AND f_greater(1.0,X[0]) THEN
        RETURN(TRUE)
      ENDIF;
    ENDIF;
    RETURN(FALSE);
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void vertausche(r,s,n)
     INTEGER r, s ; 
     INTEGER n ; 
  BEGIN
     INTEGER i;
     REAL    z;

    FOR_TO(i,1,n)
      z = get_matrix_element(r,i);
      set_matrix_element(r,i, get_matrix_element(s,i));
      set_matrix_element(s,i, z);
    ENDFOR;
    z = get_B_element(r);
    set_B_element(r, get_B_element(s));
    set_B_element(s, z);
  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

INTEGER search_max(sp,vz,bz)
       INTEGER sp,vz,bz;
  BEGIN
       INTEGER  max     = 0;
       REAL     maxwert = 0.0 ;
       INTEGER  i;

  FOR_TO(i,vz,bz)
    IF  f_greater(fabs(get_matrix_element(i,sp)),maxwert)  THEN
      maxwert = fabs(get_matrix_element(i,sp));
      max = i;
    ENDIF;
  ENDFOR;
  RETURN(max);
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void neutralisiere_zeile(z,j,Zeile,Spalte,m,n)
        REAL z;
        INTEGER j,Zeile,Spalte,m,n;
  BEGIN
        INTEGER k;

          FOR_TO(k,Spalte,n)
            set_matrix_element(j,k,
                      get_matrix_element(j,k) - get_matrix_element(Zeile,k)*z) ;
          ENDFOR;
          set_B_element(j, get_B_element(j) - get_B_element(Zeile)*z);
          IF get_B_element(j) < 0 THEN 
            FOR_TO(k,1,n)
              IF f_equal(get_matrix_element(j,k),0.0) THEN
                 set_matrix_element(j,k,0.0);
               ELSE
                 set_matrix_element(j,k, - get_matrix_element(j,k));
              ENDIF;
            ENDFOR;
            set_B_element(j, -get_B_element(j));
          ENDIF;
  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void neutralisiere_zeilen(Zeile,Spalte,m,n)
        INTEGER Zeile,Spalte,m,n;
  BEGIN
        INTEGER j;
        REAL z;

      FOR_DOWNTO(j,Zeile-1,1)
          z = get_matrix_element(j,Spalte) / get_matrix_element(Zeile,Spalte);
          neutralisiere_zeile(z,j,Zeile,Spalte,m,n);
        ENDFOR;
        FOR_TO(j,Zeile+1,m)
          z = get_matrix_element(j,Spalte) / get_matrix_element(Zeile,Spalte);
          neutralisiere_zeile(z,j,Zeile,Spalte,m,n);
        ENDFOR;
  ENDPROC

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

INTEGER rang_matrix(m, n) 
     INTEGER m, n ; 

BEGIN
  INTEGER Zeile,max;
  INTEGER i,j,k;

  Zeile = 1;
  FOR_TO(i,1,n)
    IF Zeile <= m THEN
      max = search_max(i,Zeile,m);
      IF max > 0 THEN
        IF max != Zeile THEN
          vertausche(max,Zeile,n);
        ENDIF;
        neutralisiere_zeilen(Zeile,i,m,n);
        Zeile ++ ;
      ENDIF;
    ENDIF;
  ENDFOR;
  FOR_TO(j,1,m)
     FOR_TO(k,1,n)
        IF f_equal(get_matrix_element(j,k),0.0) THEN
          set_matrix_element(j,k,0.0);
        ENDIF;
     ENDFOR;
  ENDFOR;
  printf("Rang = %d \n",Zeile-1);
  RETURN(Zeile-1);
ENDPROC;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


void normalisiere_matrix(Zeile,Spalte)
        INTEGER Zeile,Spalte;
  BEGIN
        INTEGER j,k;

    FOR_TO(j,1,Zeile)
      IF get_B_element(j) < 0 THEN 
        FOR_TO(k,1,Spalte)
          IF f_equal(get_matrix_element(j,k),0.0) THEN
            set_matrix_element(j,k,0.0);
           ELSE
            set_matrix_element(j,k, - get_matrix_element(j,k));
          ENDIF;
        ENDFOR;
        set_B_element(j, -get_B_element(j));
      ENDIF;
   ENDFOR;
 ENDPROC;



