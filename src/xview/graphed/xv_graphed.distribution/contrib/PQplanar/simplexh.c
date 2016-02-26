/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : simplexh.c                                     */
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
#include "edgelist.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simplexh.h"
#include "neck.h"
#include "necklist.h"
#include "adj.h"
#include "interfac.h"
#include "mtx_auf.h"
#include "mtx_test.h"
#include "plan_sf.h"
#include "simptrei.h"
#include <stdio.h>
#include <math.h>


INTEGER Anzahl_Knoten;
INTEGER Anzahl_Winkel;
INTEGER Anzahl_Flaechen;
INTEGER Anzahl_Grund_Gleich;
INTEGER Anzahl_Grund_Var;
INTEGER Anzahl_Gleichungen;
INTEGER Anzahl_Variablen;

REAL        maxmin,minmax;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


void erzeuge_Tableu(graph,Neck_list)
        Sgraph        graph;
        NECK_LIST     Neck_list;
  BEGIN
        INTEGER i;

    Anzahl_Knoten   = number_off_nodes(graph);
    Anzahl_Winkel   = GRAPH_WINKELZAHL(graph);
    Anzahl_Flaechen = (lengthlist(Neck_list)-1);
    Anzahl_Variablen = 2*Anzahl_Winkel+1;
    Anzahl_Grund_Var = 2*Anzahl_Winkel+1;
    Anzahl_Grund_Gleich = Anzahl_Knoten+Anzahl_Flaechen;
    Anzahl_Gleichungen  = Anzahl_Grund_Gleich+Anzahl_Winkel;
 
    setze_Zaehler_null(graph);
    init_Matrix(Anzahl_Gleichungen,Anzahl_Variablen+Anzahl_Winkel);
    winkelsummen_to_Matrix(graph,Anzahl_Variablen);    
    neue_Zeile_im_Tableu(Neck_list,&Anzahl_Flaechen,Anzahl_Knoten,Anzahl_Variablen);    
    Anzahl_Grund_Gleich = Anzahl_Knoten+Anzahl_Flaechen;

    FOR_TO(i,1,Anzahl_Grund_Gleich)
      set_matrix_element(i,2*Anzahl_Winkel+1+i,1.0);
      set_Cost_element(2*Anzahl_Winkel+1+i,100.0);
    ENDFOR;

    Anzahl_Variablen = 2*Anzahl_Winkel+1+Anzahl_Grund_Gleich;

    Anzahl_Gleichungen  = Anzahl_Grund_Gleich+Anzahl_Winkel;

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

void umbruch(i)
INTEGER        i;
BEGIN
  IF i MOD 10 == 0 THEN
    printf("\n");
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

BOOLEAN some_cost_are_zero()
  BEGIN
        INTEGER i;

    FOR_TO(i,1,Anzahl_Winkel)
      IF get_cost0_element(i) AND
         NOT(get_deleted_element(i))   THEN
         RETURN(TRUE);
       ELSE
        IF get_cost0_element(Anzahl_Winkel+1+i) AND
           NOT(get_deleted_element(Anzahl_Winkel+1+i))   THEN
          RETURN(TRUE);
        ENDIF
      ENDIF;
    ENDFOR;
    RETURN(FALSE);
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


void streiche_unoetiges(zusaetzlich,range1,range2,
                                    Anzahl_Gleichungen,Anzahl_Variablen,f,g,h)
        BOOLEAN zusaetzlich;
        INTEGER range1,range2,Anzahl_Gleichungen,Anzahl_Variablen;
        INTEGER (*f)();
        INTEGER (*g)();
        INTEGER (*h)();
  BEGIN
         INTEGER spalte,zeile;

#if TEST
     printf(" streiche_unoetiges : \n");
#endif

     WHILE unoetige_spalte(&zeile,&spalte,range1,range2) DO
       streiche_spalte_in_start_matrix(zeile,spalte,
                                             Anzahl_Gleichungen,Anzahl_Variablen);
       IF zusaetzlich THEN
         streiche_spalte_in_start_matrix(0,
                                      f(spalte),Anzahl_Gleichungen,Anzahl_Variablen);
         streiche_zeile_in_start_matrix(h(spalte),Anzahl_Gleichungen,Anzahl_Variablen);
       ENDIF;
     ENDDO;

     WHILE (zeile = unoetige_zeile(range1,range2)) > 0  DO
       streiche_zeile_in_start_matrix(zeile,Anzahl_Gleichungen,Anzahl_Variablen);

     ENDDO;
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


Local INTEGER zweite_spalte(x)
        INTEGER x;
  BEGIN
    RETURN(Anzahl_Winkel+1+x);
  ENDPROC


Local INTEGER dritte_spalte(x)
        INTEGER x;
  BEGIN
    RETURN(2*Anzahl_Winkel+1+x);
  ENDPROC


Local INTEGER zugehoerige_zeile(x)
        INTEGER x;
  BEGIN
    RETURN(Anzahl_Grund_Gleich+x);
  ENDPROC


BOOLEAN  change_matrix()
  BEGIN
        INTEGER i;

    FOR_DOWNTO(i,Anzahl_Winkel,1)
      IF NOT(get_cost0_element(i))  AND 
            f_zero(get_X_element(i)) AND 
            NOT (get_deleted_element(i))        THEN 
         streiche_spalte_in_start_matrix(0,i,Anzahl_Gleichungen,Anzahl_Variablen);
         streiche_spalte_in_start_matrix(0,Anzahl_Winkel+1+i,
                                         Anzahl_Gleichungen,Anzahl_Variablen);
         streiche_zeile_in_start_matrix(Anzahl_Grund_Gleich+i,
                                        Anzahl_Gleichungen,Anzahl_Variablen);
        
       ELSE
         IF NOT(get_cost0_element(Anzahl_Winkel+1+i))  AND 
              f_zero(get_X_element(Anzahl_Winkel+1+i)) AND 
              NOT (get_deleted_element(Anzahl_Winkel+1+i))        THEN 
           streiche_spalte_in_start_matrix(0,i,Anzahl_Gleichungen,Anzahl_Variablen);
           streiche_spalte_in_start_matrix(0,Anzahl_Winkel+1+i,
                                           Anzahl_Gleichungen,Anzahl_Variablen);
           streiche_zeile_in_start_matrix(Anzahl_Grund_Gleich+i,
                                          Anzahl_Gleichungen,Anzahl_Variablen);
         ENDIF;
      ENDIF;
    ENDFOR;

     IF (get_max_W_element(Anzahl_Variablen) == 0) OR
        (get_max_U_element(Anzahl_Gleichungen) == 0) THEN
           RETURN(TRUE)
     ENDIF;

     streiche_unoetiges(TRUE,Anzahl_Grund_Gleich,Anzahl_Gleichungen,
                             Anzahl_Grund_Var,Anzahl_Variablen,
                             zweite_spalte,dritte_spalte,zugehoerige_zeile);

     IF (get_max_W_element(Anzahl_Variablen) == 0) OR
        (get_max_U_element(Anzahl_Gleichungen) == 0) THEN
           RETURN(TRUE)
     ENDIF;

     RETURN(FALSE);
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

void min_max_winkel()
  BEGIN
        /*    Minimiere den maximalen Winkel                     */
        /*    min max X[i]                                       */

    INTEGER i;

    FOR_TO(i,1,Anzahl_Winkel)          /* x[i] - x[n+1] <= 0   */
      set_matrix_element(Anzahl_Grund_Gleich+i,i, 1.0);
      set_matrix_element(Anzahl_Grund_Gleich+i,Anzahl_Winkel+1, -1.0);
      set_matrix_element(Anzahl_Grund_Gleich+i,Anzahl_Winkel+1+i, 1.0);
                                                /* -x[i] + x[n+1]  + SCHLUPF = 0   */
      set_B_element(Anzahl_Grund_Gleich+i, 0.0);

    ENDFOR;
  
    set_Cost_element(Anzahl_Winkel+1, 1.0);             /* min x[n+1]        */

    FOR_TO(i,1,Anzahl_Winkel)          /* x[i] >= 1   */
      set_matrix_element(Anzahl_Grund_Gleich+Anzahl_Winkel+i,i, 1.0);
      set_matrix_element(Anzahl_Grund_Gleich+Anzahl_Winkel+i,Anzahl_Variablen+i, -1.0);
      set_B_element(Anzahl_Grund_Gleich+Anzahl_Winkel+i, 1.0);

    ENDFOR;
    Anzahl_Variablen = 3*Anzahl_Winkel+1;
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

void max_min_winkel()
  BEGIN
        /*    Maximiere den minimalen Winkel                     */
        /*    max min X[i]                                       */

    INTEGER i;

    FOR_TO(i,1,Anzahl_Variablen)           /* x[i] - x[n+1] >= 0   */
      set_matrix_element(Anzahl_Grund_Gleich+i,i, 1.0);
      set_matrix_element(Anzahl_Grund_Gleich+i,Anzahl_Winkel+1, -1.0);
      set_matrix_element(Anzahl_Grund_Gleich+i,Anzahl_Winkel+1+i, -1.0); 
                                                 /* x[i] - x[n+1]  - SCHLUPF = 0   */
      set_B_element(Anzahl_Grund_Gleich+i, 0.0);

    END;
  
    set_Cost_element(Anzahl_Winkel+1, -1.0);             /* max x[n+1]        */
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

void simplex_verfahren(graph,Neck_list)
        Sgraph        graph;
        ADRlist        Neck_list;

  BEGIN
    INTEGER i;

    IF (get_maxm() < Anzahl_Gleichungen) OR (get_maxn() < Anzahl_Variablen) THEN
      printf(" Problem zu gross !!!!!!!!!!!!! \n");
    ELSE

      erzeuge_Tableu(graph,Neck_list);

      printf("Simplex Verfahren \n");

      max_min_winkel(); 

      simplex_treiber(Anzahl_Grund_Gleich,Anzahl_Gleichungen,Anzahl_Variablen);

      print_ergebniss(Anzahl_Gleichungen,Anzahl_Variablen);

      IF get_simplex_mode() == MODE_MPS_SIMPLEX THEN
        FOR_TO(i,1,Anzahl_Variablen)
          set_deleted_element(i,FALSE);
        ENDFOR;
        WHILE some_cost_are_zero() DO
          IF change_matrix() THEN 
            break;
           ELSE
            simplex_treiber(Anzahl_Grund_Gleich,Anzahl_Gleichungen,
                            Anzahl_Variablen);
            print_ergebniss(Anzahl_Gleichungen,Anzahl_Variablen);    
          ENDIF;    
        ENDDO;
      ENDIF;
    ENDIF;
  END

