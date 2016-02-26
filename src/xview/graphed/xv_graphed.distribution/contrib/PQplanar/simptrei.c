/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : simptrei.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 18.09.90                                       */
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
#include "simplex.h"
#include "simplexh.h"
#include "simptrei.h"
#include "plan_sf.h"
#include <stdio.h>
#include <math.h>



arrm2 B;

arrm2 X,XW,XH;

arrn  C;

arrn_bool Cost_zero;

arrn_bool deleted;

arrm  W,U,N,M,omega;

REAL F;

BOOLEAN nofeas, nosol;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void korigiere_Vektoren(Azeilen,Aspalten)
        INTEGER Azeilen,Aspalten;
  BEGIN

    INTEGER i;

      FOR_TO(i,1,Azeilen)            
          XH[get_M_element(omega[i])] = X[i] ;
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

void simplex_treiber(kleiner_gleich,m, n)
         INTEGER kleiner_gleich,m, n ; 

  BEGIN
    IF get_simplex_mode() == MODE_EINFACHER_SIMPLEX THEN
      normalisiere_matrix(m,n);
      psimplex(m, n,B,X,C,omega,&F ,&nofeas, &nosol);
      korigiere_Vektoren(m,n);
     ELSE
      simplex_to_MPS(kleiner_gleich,m,n);
      call_MPS();
      MPS_cost0_to_simplex(m,n);
      MPS_basic_to_simplex(m,n);
    ENDIF;
  END;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void set_start_matrix_element(m,n,wert)
        INTEGER m, n ;
        REAL    wert;
  BEGIN
    set_matrix_element(get_U_element(m),get_W_element(n),wert);
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

REAL get_start_matrix_element(m,n)
        INTEGER m, n ;
  BEGIN
    RETURN(get_matrix_element(get_U_element(m),get_W_element(n)));
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

void set_B_element(m,wert)
        INTEGER m ;
        REAL    wert;
  BEGIN
    B[m] = wert;
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

void set_start_B_element(m,wert)
        INTEGER m ;
        REAL    wert;
  BEGIN
    B[get_U_element(m)] = wert;
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

REAL get_B_element(m) 
        INTEGER m ;
  BEGIN
    RETURN(B[m]);
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

REAL get_start_B_element(m) 
        INTEGER m ;
  BEGIN
    RETURN(B[get_U_element(m)]);
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

void set_Cost_element(n,wert)
        INTEGER n ;
        REAL    wert;
  BEGIN
    C[n] = wert;
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

void set_start_Cost_element(n,wert)
        INTEGER n ;
        REAL    wert;
  BEGIN
    C[get_W_element(n)] = wert;
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

REAL get_Cost_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(C[n]);
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

void set_cost0_element(n,wert)
        INTEGER n ;
        BOOLEAN    wert;
  BEGIN
    Cost_zero[n] = wert;
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

BOOLEAN get_cost0_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(Cost_zero[n]);
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

void set_deleted_element(n,wert)
        INTEGER n ;
        BOOLEAN    wert;
  BEGIN
    deleted[n] = wert;
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

BOOLEAN get_deleted_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(deleted[n]);
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

void set_X_element(n,wert)
        INTEGER n ;
        REAL    wert;
  BEGIN
    XH[n] = wert;
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

REAL get_X_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(XH[n]);
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

void set_W_element(n,wert) 
        INTEGER n ,wert;
  BEGIN
    W[n] = wert;
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

INTEGER get_W_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(W[n]);
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

INTEGER get_max_W_element(n) 
        INTEGER n ;
  BEGIN
         INTEGER i;
    FOR_DOWNTO(i,n,1)
      IF W[i] > 0 THEN
        RETURN(W[i]);
      ENDIF;
    ENDFOR;
    RETURN(0);
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

void set_U_element(n,wert) 
        INTEGER n ,wert;
  BEGIN
    U[n] = wert;
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

INTEGER get_U_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(U[n]);
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

INTEGER get_max_U_element(n) 
        INTEGER n ;
  BEGIN
         INTEGER i;
    FOR_DOWNTO(i,n,1)
      IF U[i] > 0 THEN
        RETURN(U[i]);
      ENDIF;
    ENDFOR;
    RETURN(0);
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

void set_M_element(n,wert) 
        INTEGER n ,wert;
  BEGIN
    M[n] = wert;
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

INTEGER get_M_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(M[n]);
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

void set_N_element(n,wert) 
        INTEGER n ,wert;
  BEGIN
    N[n] = wert;
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

INTEGER get_N_element(n) 
        INTEGER n ;
  BEGIN
    RETURN(N[n]);
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

REAL get_optimal_wert()
  BEGIN
    RETURN(F);
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

INTEGER get_maxm()
  BEGIN
    RETURN(maxm);
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

INTEGER get_maxn()
  BEGIN
    RETURN(maxn);
  END

