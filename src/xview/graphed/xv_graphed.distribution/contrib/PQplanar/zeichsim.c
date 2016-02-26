/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : zeichsim.c                                      */
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
#include "adj.h"
#include "interfac.h"
#include "dfs.h"
#include "plzeich.h"
#include "gauss.h"
#include "sgraphhi.h"
#include "mtx_auf.h"
#include "mtx_test.h"
#include "simptrei.h"
#include <stdio.h>
#include <math.h>


/*****************************************************************/

Local INTEGER Max_Gleichungen;
Local INTEGER Max_Variablen;


Local INTEGER Anzahl_Knoten;
Local INTEGER Anzahl_Kanten;
Local INTEGER Anzahl_Grund_Gleich;
Local INTEGER Anzahl_Kanten_Gleich;
Local INTEGER Anzahl_Gleichungen;
Local INTEGER Anzahl_Variablen;

Local INTEGER definierte_knoten;
Local INTEGER definierte_kanten;

/*****************************************************************/

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void Zeile_node_fix(node,x,y)
        Snode node;
        INTEGER x,y;
  BEGIN  
        INC(definierte_knoten,1);
        init_Matrix_Zeile(2*definierte_kanten+2*definierte_knoten-1,Max_Variablen);
        init_Matrix_Zeile(2*definierte_kanten+2*definierte_knoten,Max_Variablen);

        set_matrix_element(2*definierte_kanten+2*definierte_knoten-1,
                           2*NODE_VAR(node)-1, 1.0);
        set_B_element(2*definierte_kanten+2*definierte_knoten-1, (REAL)x);
        set_matrix_element(2*definierte_kanten+2*definierte_knoten,
                           2*NODE_VAR(node), 1.0);
        set_B_element(2*definierte_kanten+2*definierte_knoten, (REAL)y);
  ENDPROC;

/*****************************************************************/

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void set_kanten_Gleischung(zeile,spalte1,spalte2,spalte3,steigung)
        INTEGER zeile,spalte1,spalte2,spalte3;
        REAL    steigung;
  BEGIN
          set_start_matrix_element(zeile,spalte1,  1.0);
          set_start_matrix_element(zeile,spalte2, -1.0);
          set_start_matrix_element(zeile,spalte3, steigung);
          set_start_B_element(zeile, 0.0);
  ENDPROC


void set_kanten_Gleischung_x(zeile,edge)
        INTEGER zeile;
        Sedge  edge;
  BEGIN
    set_kanten_Gleischung(zeile,2*NODE_VAR(edge->snode)-1,
                                2*NODE_VAR(edge->tnode)-1,
                                2*Anzahl_Knoten + definierte_kanten,
                                EDGE_X(edge));
  ENDPROC


void set_kanten_Gleischung_y(zeile,edge)
        INTEGER zeile;
        Sedge  edge;
  BEGIN
    set_kanten_Gleischung(zeile,2*NODE_VAR(edge->snode),
                                2*NODE_VAR(edge->tnode),
                                2*Anzahl_Knoten + definierte_kanten,
                                EDGE_Y(edge));
  ENDPROC


Local INTEGER zweite_spalte(x)
        INTEGER x;
  BEGIN
    RETURN(definierte_kanten+1+x);
  ENDPROC


Local INTEGER zugehoerige_zeile(x)
        INTEGER x;
  BEGIN
    RETURN(Anzahl_Grund_Gleich+x);
  ENDPROC


void fuelle_matrix(graph)
        Sgraph graph;

  BEGIN
        Snode node;
        Sedge edge;
        INTEGER i;

    Anzahl_Knoten   = number_off_nodes(graph);
    Anzahl_Kanten   = number_off_edges_undirected(graph);

    Max_Gleichungen = Anzahl_Knoten + 3*Anzahl_Kanten+1;
    Max_Variablen   = 4*Anzahl_Kanten + 2*Anzahl_Knoten + 2*Anzahl_Kanten+1;

    init_Matrix(Max_Gleichungen,Max_Variablen);

    definierte_kanten = 0;
    definierte_knoten = 0;

/* Wenn kein Knoten bestimmt beliebigen Knoten festlegen */

    IF  number_off_fix_nodes(graph) == 0  THEN
      Zeile_node_fix(first_node_in_graph(graph),2000,2000);
    ENDIF;

    for_all_nodes(graph,node);
 
/* Falls Knoten FIX , seine Koordinaten eintragen  */

     IF NODE_FIX(node) THEN
        Zeile_node_fix(node,snode_x(node),snode_y(node));      
      ENDIF;

/* Alle Kanten durchgehen, und Kantengleichungen eintragen  */

      for_unique_sourcelist (node,edge);
        IF defined_edge(edge) /* AND NOT(EDGE_FIX(edge)) */ THEN
          INC(definierte_kanten,1); 

          init_Matrix_Zeile(2*definierte_kanten+2*definierte_knoten-1,
                            Max_Variablen);
          init_Matrix_Zeile(2*definierte_kanten+2*definierte_knoten,
                            Max_Variablen);
 
          set_kanten_Gleischung_x(2*definierte_kanten+2*definierte_knoten-1,edge);
          set_kanten_Gleischung_y(2*definierte_kanten+2*definierte_knoten,edge);
     
        ENDIF;
      end_for_unique_sourcelist (node,edge);
    end_for_all_nodes(graph,node);

    Anzahl_Grund_Gleich = 2*definierte_kanten+2*definierte_knoten;
    Anzahl_Variablen   = 2*Anzahl_Knoten+2*definierte_kanten+1;
    Anzahl_Kanten_Gleich = 2*definierte_kanten;    
    Anzahl_Gleichungen   = Anzahl_Grund_Gleich + definierte_kanten + 1;

/*  Laenge alle Kanten des Graphen beschraenken   */

    init_start_Matrix_Zeile(Anzahl_Grund_Gleich+1,Max_Variablen);
    FOR_TO(i,1,definierte_kanten )
        set_start_matrix_element(Anzahl_Grund_Gleich+1,2*Anzahl_Knoten+i, 1.0);
    END;
    set_start_B_element(Anzahl_Grund_Gleich+1, definierte_kanten * 225.0);


/*  Maximiere minimale Kantenlaenge  */

    FOR_TO(i,1,definierte_kanten)
        init_start_Matrix_Zeile(Anzahl_Grund_Gleich+1+i,Max_Variablen);
        set_start_matrix_element(Anzahl_Grund_Gleich+1+i,2*Anzahl_Knoten+i, 1.0);
        set_start_matrix_element(Anzahl_Grund_Gleich+1+i,
                           2*Anzahl_Knoten+definierte_kanten+1, -1.0);
        set_start_matrix_element(Anzahl_Grund_Gleich+1+i,
                           2*Anzahl_Knoten+definierte_kanten+1+i, -1.0);
        set_start_B_element(Anzahl_Grund_Gleich+1+i, 0.0);
    END;
    set_start_Cost_element(2*Anzahl_Knoten+definierte_kanten+1, -1.0);

    print_ergebniss(Anzahl_Gleichungen,Anzahl_Variablen);

    streiche_unoetiges(FALSE,Anzahl_Gleichungen,Max_Gleichungen,
                       Max_Variablen,Max_Variablen,
                       zweite_spalte,zweite_spalte,zugehoerige_zeile);
    

/*   Schlupfvariablen fuer die numerische Genauigkeit   */

    FOR_TO(i,1,Anzahl_Kanten_Gleich)
      IF get_U_element(2*definierte_knoten+i) > 0 THEN
        set_start_matrix_element(2*definierte_knoten+i,Anzahl_Variablen+i,1.0);
        set_start_matrix_element(2*definierte_knoten+i,
                                 Anzahl_Variablen+i+Anzahl_Kanten_Gleich,-1.0);
        set_start_Cost_element(Anzahl_Variablen+i,1000.0);
        set_start_Cost_element(Anzahl_Variablen+i+Anzahl_Kanten_Gleich,1000.0);
      ENDIF;
    ENDFOR;

    Anzahl_Variablen   = 2*Anzahl_Knoten+
                         2*Anzahl_Kanten_Gleich + 
                         2*definierte_kanten+1;
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

void besetze_koordinaten(graph)
        Sgraph graph;
  BEGIN
    Snode node;
    REAL    Xsumme = 0.0;
    REAL    Ysumme = 0.0;
    REAL    Xquer  = 0.0;
    REAL    Yquer  = 0.0;

    for_all_nodes(graph,node);
      INC(Xsumme,get_X_element(NODE_VAR(node)*2 - 1 )) ;
      INC(Ysumme,get_X_element(NODE_VAR(node)*2 )    ) ;
    end_for_all_nodes(graph,node);

    Xquer = Xsumme / number_off_nodes(graph);
    Yquer = Ysumme / number_off_nodes(graph);

    for_all_nodes(graph,node);
      snode_x(node) =  get_X_element(NODE_VAR(node)*2 - 1 ) ;
      snode_y(node) =  get_X_element(NODE_VAR(node)*2 )     ;
    end_for_all_nodes(graph,node);

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

void compute_koordinaten(graph)
        Sgraph graph;
  BEGIN
    INTEGER i;
    REAL    summe;

    fuelle_matrix(graph);
      
    IF get_max_W_element(2*Anzahl_Knoten) > 0 THEN
       simplex_treiber(0,Anzahl_Gleichungen,Anzahl_Variablen);
    ENDIF;

    print_ergebniss(Anzahl_Gleichungen,Anzahl_Variablen);

    summe = 0.0;
    FOR_TO(i,2*Anzahl_Knoten+2*Anzahl_Kanten+2,Anzahl_Variablen)
      summe = summe + get_X_element(i);
    ENDFOR;
    printf("Summe der Koordinaten-Abweichungen : %f \n ",summe);

    besetze_koordinaten(graph);

  END

