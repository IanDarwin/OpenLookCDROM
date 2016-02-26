/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : mtx_auf.c                                      */
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
#include "listen.h"
#include "listen1.h"
#include "dfs.h"
#include "adj.h"
#include "interfac.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simplexh.h"
#include "simptrei.h"
#include "edgelist.h"
#include "neck.h"
#include "necklist.h"
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

void init_Matrix_Zeile(Zeile,Aspalten)
        INTEGER Zeile,Aspalten;
  BEGIN
    INTEGER j;

    FOR_TO(j,1,Aspalten)
      set_matrix_element(Zeile,j,0.0);
    END;
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

void init_start_Matrix_Zeile(Zeile,Aspalten)
        INTEGER Zeile,Aspalten;
  BEGIN
    init_Matrix_Zeile(get_U_element(Zeile),Aspalten)
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

void init_Matrix(Azeilen,Aspalten)
        INTEGER Azeilen,Aspalten;
  BEGIN
    INTEGER i,j;

#if TEST
    printf("Init Matrix \n");
#endif

    FOR_TO(i,1,Azeilen)
      set_B_element(i,0.0);
      FOR_TO(j,1,Aspalten)
        set_matrix_element(i,j,0.0);
      END;
    END;
    
    FOR_TO(i,1,Aspalten)
      set_Cost_element(i,0.0);
    END;

    FOR_TO(i,1,Aspalten)
      set_M_element(i,i);
    END;

    FOR_TO(i,1,Aspalten)
      set_W_element(i,i);
    END;

    FOR_TO(i,1,Azeilen)
      set_N_element(i,i);
    END;

    FOR_TO(i,1,Azeilen)
      set_U_element(i,i);
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

void winkelsummen_to_Matrix(graph,Aspalten)
        Sgraph graph;
        INTEGER Aspalten;

  BEGIN
    Snode   node;
    ADDRlist work;
    INTEGER Zeile;
    REAL ausgleich;

    Zeile = 0;

    for_all_nodes(graph,node)
      INC(Zeile,1);
      init_Matrix_Zeile(Zeile,Aspalten);
      ausgleich = 0.0;
      for_edgelist(node,work)
        IF EDGELIST_FIX(work) THEN
          INC(ausgleich,EDGELIST_FIX_XW(work));
         ELSE
          set_matrix_element(Zeile,EDGELIST_VAR(work),1.0);
        ENDIF;
      end_for_edgelist(node);
      set_B_element(Zeile,360.0 - ausgleich);
    end_for_all_nodes(graph,node);
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

void neue_Zeile_im_Tableu(Neck_list,Zeile,Anzahl_Knoten,Anzahl_Variablen)
        NECK_LIST     Neck_list;
        INTEGER *Zeile ;
        INTEGER Anzahl_Knoten;
        INTEGER Anzahl_Variablen;
  BEGIN
    NECK     Neck;
    EDGELIST ecke;
    INTEGER N;
    REAL ausgleich;
    
    (*Zeile) = 0;
    for_all_elements(Neck_list,Neck);
      IF   NOT LAST_ELEMENT   THEN
        INC((*Zeile),1);
        erhoehe_Zaehler(Neck);
        init_Matrix_Zeile((Anzahl_Knoten+(*Zeile)),Anzahl_Variablen);
        ausgleich = 0.0;
        for_all_neck_elements(Neck,(char*)ecke)
          IF EDGELIST_FIX(NECK_EDGELIST(ecke)) THEN
            INC(ausgleich,EDGELIST_FIX_XW(NECK_EDGELIST(ecke)));
           ELSE
            set_matrix_element((Anzahl_Knoten+(*Zeile)),
                                EDGELIST_VAR(NECK_EDGELIST(ecke))
                               ,1.0);
          ENDIF;
        end_for_all_neck_elements(Neck);
        N = dlengthlist(Neck);
        IF LAST_ELEMENT THEN
          set_B_element(Anzahl_Knoten+(*Zeile),
                       (REAL)(N) * 180.0 + 360.0 - ausgleich);
         ELSE
          set_B_element(Anzahl_Knoten+(*Zeile),
                       (REAL)(N -2 ) * 180.0  - ausgleich);
        ENDIF;
        IF  f_equal(get_B_element(Anzahl_Knoten+(*Zeile)),0.0) THEN
          (*Zeile) -- ;
        ENDIF;
      ENDIF;
    end_for_all_elements;
  END;

