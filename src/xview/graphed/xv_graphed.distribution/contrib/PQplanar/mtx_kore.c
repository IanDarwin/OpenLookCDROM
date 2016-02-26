/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : mtx_kore.c                                     */
/*        AUTOR : Uwe Schnieders                                  */
/*        UPDATE: 23.07.90                                         */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen1.h"
#include "dfs.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simplexh.h"
#include "simptrei.h"
#include <stdio.h>
#include <math.h>


void streiche_spalte(j,Azeilen,Aspalten)
        INTEGER j;
        INTEGER Azeilen;
        INTEGER Aspalten;
BEGIN
  INTEGER i,k;

  FOR_TO(i,1,Azeilen)
    FOR_TO(k,j,Aspalten-1)
      set_matrix_element(i,k, get_matrix_element(i,k+1)) ;
    END;
  END;
END

void streiche_zeile(j,Azeilen,Aspalten)
        INTEGER j;
        INTEGER Azeilen;
        INTEGER Aspalten;
BEGIN
  INTEGER i,k;

  FOR_TO(i,1,Aspalten)
    FOR_TO(k,j,Azeilen-1)
      set_matrix_element(k,i, get_matrix_element(k+1,i)) ;
    END;
  END;
END
/*****************************************************************/

void korigiere_B(j,Azeilen)
        INTEGER j;
        INTEGER Azeilen;
BEGIN
  INTEGER i;

  FOR_TO(i,1,Azeilen)
    IF f_not_zero(get_matrix_element(i,j)) THEN
        set_B_element(i,get_B_element(i) - 
                        get_X_element(get_M_element(j))*get_matrix_element(i,j)) ;
    ENDIF;
  ENDFOR;
ENDPROC

/*****************************************************************/

void streiche_element_B(j,Aspalten)
        INTEGER j;
        INTEGER Aspalten;
BEGIN
    INTEGER k;

    FOR_TO(k,j,Aspalten-1)
      set_B_element(k,get_B_element(k+1)) ;
    ENDFOR; 
ENDPROC


void streiche_element_C(j,Aspalten)
        INTEGER j;
        INTEGER Aspalten;
BEGIN
    INTEGER k;

    FOR_TO(k,j,Aspalten-1)
      set_Cost_element(k,get_Cost_element(k+1)) ;
    ENDFOR; 
ENDPROC


void streiche_element_X(j,Aspalten)
        INTEGER j;
        INTEGER Aspalten;
BEGIN
    INTEGER k;

    FOR_TO(k,j,Aspalten)
      set_X_element(k,get_X_element(k+1)) ;
    ENDFOR; 
ENDPROC

/*****************************************************************/

void streiche_element_M(j,Aspalten)
        INTEGER j;
        INTEGER Aspalten;
BEGIN
    INTEGER k;

    FOR_TO(k,j,Aspalten-1)
      set_M_element(k,get_M_element(k+1)) ;
    ENDFOR; 
    set_M_element(Aspalten,0) ;

ENDPROC
/*****************************************************************/

void streiche_element_N(j,Azeilen)
        INTEGER j;
        INTEGER Azeilen;
BEGIN
    INTEGER k;

    FOR_TO(k,j,Azeilen-1)
      set_N_element(k,get_N_element(k+1)) ;
    ENDFOR; 
    set_N_element(Azeilen,0) ;
    
ENDPROC
/*****************************************************************/

INTEGER count_not_zero(x,sp,Aspalten)
        INTEGER x;
        INTEGER *sp;
        INTEGER Aspalten;
BEGIN
  INTEGER i,count;
  
  count = 0;
  FOR_TO(i,1,Aspalten)
    IF f_not_zero(get_matrix_element(x,i)) THEN
      count ++ ;
      *sp = i;
    END;
  END;
  RETURN(count);
END
/*****************************************************************/

INTEGER nur_eine_eins(x,Aspalten)
        INTEGER x;
        INTEGER Aspalten;
BEGIN
        INTEGER sp;
  IF count_not_zero(x,&sp,Aspalten) == 1 THEN 
    RETURN(sp);
  ENDIF;
  RETURN(0);
END

/*****************************************************************/

BOOLEAN keine_eins(x,Aspalten)
        INTEGER x;
        INTEGER Aspalten;
BEGIN
       INTEGER sp;

  IF Aspalten > 0 THEN
    RETURN(count_not_zero(x,&sp,Aspalten) == 0);
  ENDIF;
  RETURN(FALSE);
END

/*****************************************************************/

void  streiche_in_Abbildung_zeile(element,max)
        INTEGER element,max;
  BEGIN
        INTEGER i;
    set_U_element(element,0);
    FOR_TO(i,element+1,max)
      IF get_U_element(i) > 0 THEN
        set_U_element(i,get_U_element(i)-1);
      ENDIF;
    ENDFOR;
  ENDPROC;


void  streiche_in_Abbildung_spalte(element,max)
        INTEGER element,max;
  BEGIN
        INTEGER i;
    set_W_element(element,0);
    FOR_TO(i,element+1,max)
      IF get_W_element(i) > 0 THEN
        set_W_element(i,get_W_element(i)-1);
      ENDIF;
    ENDFOR;
  ENDPROC;


void  streiche_zeile_in_start_matrix(zeile,Anzahl_Gleichungen,Anzahl_Variablen)
        INTEGER zeile,Anzahl_Gleichungen,Anzahl_Variablen;
  BEGIN
#if TEST
    printf(" streiche_zeile_in_start_matrix %d \n", zeile);
#endif
    streiche_element_B(get_U_element(zeile),get_max_U_element(Anzahl_Gleichungen));
    streiche_element_N(get_U_element(zeile),get_max_U_element(Anzahl_Gleichungen));
    streiche_zeile(get_U_element(zeile),
                   get_max_U_element(Anzahl_Gleichungen),
                   get_max_W_element(Anzahl_Variablen)    );

    streiche_in_Abbildung_zeile(zeile,Anzahl_Gleichungen);

  ENDPROC;


void  streiche_spalte_in_start_matrix(zeile,spalte,Anzahl_Gleichungen,Anzahl_Variablen)
        INTEGER zeile,spalte,Anzahl_Gleichungen,Anzahl_Variablen;
  BEGIN
    set_deleted_element(spalte,TRUE);
#if TEST
    printf(" streiche_spalte_in_start_matrix %d \n", spalte);
#endif
    IF zeile > 0 THEN
      set_X_element(spalte,get_start_B_element(zeile) /
                           get_start_matrix_element(zeile,spalte));
    ENDIF;
    korigiere_B(get_W_element(spalte),
                          get_max_U_element(Anzahl_Gleichungen));
    streiche_element_M(get_W_element(spalte),get_max_W_element(Anzahl_Variablen));
    streiche_element_C(get_W_element(spalte),get_max_W_element(Anzahl_Variablen));
    streiche_spalte(get_W_element(spalte),
                   get_max_U_element(Anzahl_Gleichungen),
                   get_max_W_element(Anzahl_Variablen)    );

    streiche_in_Abbildung_spalte(spalte,Anzahl_Variablen);

  ENDPROC;



void  korigiere_start_matrix(spalte,Anzahl_Winkel,Anzahl_Grund_Gleich,
                                    Anzahl_Gleichungen,Anzahl_Variablen)
        INTEGER spalte,Anzahl_Winkel,Anzahl_Grund_Gleich,
                       Anzahl_Gleichungen,Anzahl_Variablen;
  BEGIN
    streiche_spalte_in_start_matrix(0,Anzahl_Winkel+1+spalte,
                                    Anzahl_Gleichungen,Anzahl_Variablen);
    streiche_spalte_in_start_matrix(0,spalte,
                                    Anzahl_Gleichungen,Anzahl_Variablen);
    streiche_zeile_in_start_matrix(Anzahl_Grund_Gleich+spalte,
                                    Anzahl_Gleichungen,Anzahl_Variablen);
  ENDPROC;


BOOLEAN unoetige_spalte(zeile,spalte,Anzahl_Grund_Gleich,Anzahl_Variablen)
        INTEGER *zeile,*spalte;
        INTEGER Anzahl_Grund_Gleich,Anzahl_Variablen;
  BEGIN
        INTEGER i,result;

    FOR_TO(i,1,get_max_U_element(Anzahl_Grund_Gleich))
      IF (result = nur_eine_eins(i,get_max_W_element(Anzahl_Variablen)))  > 0 
        THEN
          (*spalte) = get_M_element(result);
          (*zeile)  = get_N_element(i);
          RETURN(TRUE);
      ENDIF
    ENDFOR;
    RETURN(FALSE);
  ENDPROC


INTEGER unoetige_zeile(Anzahl_Grund_Gleich,Anzahl_Variablen)
        INTEGER Anzahl_Grund_Gleich,Anzahl_Variablen;
  BEGIN
        INTEGER i;

    FOR_TO(i,1,get_max_U_element(Anzahl_Grund_Gleich))
      IF keine_eins(i,get_max_W_element(Anzahl_Variablen)) THEN
        RETURN(get_N_element(i));
      ENDIF
    ENDFOR;
    RETURN(0);    
  ENDPROC


