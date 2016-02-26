/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : plzeich.c                                      */
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
#include "adj.h"
#include "interfac.h"
#include "edgelist.h"
#include "plzeich.h"
#include "gauss.h"
#include "sgraphhi.h"
#include "simplex.h"
#include "simplexh.h"
#include "simptrei.h"
#include "neck.h"
#include "necklist.h"
#include "triang.h"
#include "plan_sf.h"
#include "kan_sort.h"
#include "stnumber.h"


#include <stdio.h>
#include <math.h>




INTEGER n,e;

BOOLEAN phase1;

NECK_LIST Neck_list;

NECK Aussen_eck;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void    besetze_datenstrucktur(graph)
        Sgraph graph;
  BEGIN
    Snode        snode;
    Sedge        sedge;

    GRAPH_WINKELZAHL(graph) = 0;
    for_all_nodes(graph,snode)
      IF NOT(NODE_FIX(snode)) THEN
         snode->x = 0;
         snode->y = 0;
      ENDIF;
      NODE_GRAD(snode) = dlengthlist(NODE_EDGELIST(snode));
      NODE_REACHED(snode) = FALSE;
      NODE_ZAEHLER(snode) = 0;
      for_unique_sourcelist(snode,sedge)
          EDGE_FIX(sedge) = FALSE;
          EDGE_X(sedge) = 0.0;
          EDGE_Y(sedge) = 0.0;
      end_for_unique_sourcelist(snode,sedge)
        
    end_for_all_nodes(graph,snode);
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

void debug_winkel(graph)
        Sgraph graph;
  BEGIN
    Snode node;
    EDGELIST ecke;

    printf("Errechnete Winkel im Graphen : \n ");
    for_all_nodes(graph,node);
      print_node(node);
      printf(" : \n");
      dfor_all_elements(NODE_EDGELIST(node),(char*)ecke);
        print_node(EDGELIST_NODE(EDGELIST_CORES(ecke)));
        print_node(EDGELIST_NODE(EDGELIST_CORES(EDGELIST_SUC(ecke))));
        IF EDGELIST_FIX(ecke) THEN
          printf(" %f \n",EDGELIST_FIX_XW(ecke));
         ELSE 
          printf(" %f \n",get_X_element(EDGELIST_VAR(ecke)));
        ENDIF;
      dend_for_all_elements(NODE_EDGELIST(node));
    end_for_all_nodes(graph,node);
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

void vergebe_variable(work)
    EDGELIST work;
  BEGIN
    INC(GRAPH_WINKELZAHL(EDGELIST_NODE(work)->graph),1);
    EDGELIST_VAR(work) = GRAPH_WINKELZAHL(EDGELIST_NODE(work)->graph);
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

void vergebe_node_variable(node)
    Snode   node;
  BEGIN
    INC(GRAPH_KNOTENZAHL(node->graph),1);
    NODE_VAR(node) = GRAPH_KNOTENZAHL(node->graph);
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

void vergebe_variablen(graph)
     Sgraph graph;
  BEGIN
    Snode   node;
    EDGELIST work;

    printf("vergebe Variablen \n ");

    GRAPH_WINKELZAHL(graph) = 0;
    GRAPH_KNOTENZAHL(graph) = 0;
    for_all_nodes(graph,node);
      printf("vergebe Variablen um : ");
      print_node(node); printf("\n");
      vergebe_node_variable(node);
      work = NODE_EDGELIST(node);
      REPEAT
        IF NOT(EDGELIST_FIX(work)) THEN
          vergebe_variable(work);
        ENDIF;
        work = EDGELIST_SUC(work);
      UNTIL( (work == NODE_EDGELIST(node)) );
    end_for_all_nodes(graph,node);  
  END


#define defined_node(node) NODE_REACHED(node)

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN defined_edge(edge)
     Sedge edge;
  BEGIN
    RETURN( f_not_zero(EDGE_X(edge)) OR f_not_zero(EDGE_Y(edge)) );
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

BOOLEAN exist_edge(edge)
     Sedge edge;
  BEGIN
    RETURN( edge != (Sedge)NIL );
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

void define_edge(ecke,winkel)
        EDGELIST ecke;
        REAL        winkel;
  BEGIN
             IF  EDGELIST_NODE(ecke)->nr < 
                          EDGELIST_NODE(EDGELIST_CORES(ecke))->nr THEN 
               EDGE_X(EDGELIST_EDGE(ecke)) = COS(winkel);
               EDGE_Y(EDGELIST_EDGE(ecke)) = SIN(winkel);
             ELSE
               EDGE_X(EDGELIST_EDGE(ecke)) = -COS(winkel);
               EDGE_Y(EDGELIST_EDGE(ecke)) = -SIN(winkel);
             END;
             printf(" Define EDGE ");
             printf("  %f  ",winkel);
             print_node(EDGELIST_NODE(ecke));
             print_node(EDGELIST_NODE(EDGELIST_CORES(ecke)));
             printf("  %f  %f \n ",EDGE_X(EDGELIST_EDGE(ecke)),EDGE_Y(EDGELIST_EDGE(ecke)));

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

void put_fix_edges_to_queue(graph,Ecken_Queue)
        Sgraph      graph;
        NECK        *Ecken_Queue;
  BEGIN
        Snode       snode;
        EDGELIST    edgelist_element;

    DINIT_LIST((*Ecken_Queue));
    for_all_nodes(graph,snode)
      for_edgelist(snode,edgelist_element)
        IF EDGE_FIX(EDGELIST_EDGE(edgelist_element)) THEN
          IF ST_NUMBER(EDGELIST_NODE(EDGELIST_CORES(edgelist_element))) 
                                                              < ST_NUMBER(snode) THEN
            DPUSH((*Ecken_Queue),edgelist_element);
            RETURN;
          ENDIF;
        ENDIF;
      end_for_edgelist(snode)         
    end_for_all_nodes(graph,snode)
    
  ENDPROC


#define MAKE_UNDEFINED(x) EDGE_X(x) = 0.0 ; \
                          EDGE_Y(x) = 0.0 ; \
		          printf("loesche Werte der Kante : "); \
                          print_node(x->snode); \
                          print_node(x->tnode); \
                          printf(" \n ");

REAL get_relative_winkel(edgelist)
        EDGELIST edgelist;
  BEGIN
        REAL help;

            IF EDGELIST_FIX(edgelist) THEN
              help = EDGELIST_FIX_XW(edgelist);
             ELSE
              help = get_X_element(EDGELIST_VAR(edgelist));
            ENDIF;
    RETURN(help);
  ENDPROC



REAL get_abs_winkel(edgelist)
        EDGELIST edgelist;
  BEGIN
        REAL help;

            IF EDGE_FIX(EDGELIST_EDGE(edgelist)) THEN
              help = r_winkel(EDGELIST_ELEMENT(edgelist));
             ELSE
              help = EDGELIST_XW(edgelist);
            ENDIF;
    RETURN(help);
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

void korigiere_winkel(graph,Aussen_eck,winkel)
        Sgraph      graph;
        NECK        Aussen_eck;
        REAL        winkel;
  BEGIN
    REAL        help,dreh_winkel;
    EDGELIST    ecke,cores_ecke,andere_ecke,hilf;
    NECK        Ecken_Queue;
    Snode       snode,node,node1;
    Sedge       sedge;

    for_all_nodes(graph,snode)
      for_unique_sourcelist(snode,sedge)
          EDGE_REACHED(sedge) = FALSE;
      end_for_unique_sourcelist(snode,sedge)        
    end_for_all_nodes(graph,snode);
    
    DINIT_LIST(Ecken_Queue);
    IF number_off_fix_edges_undirected(graph) > 0 THEN 
      put_fix_edges_to_queue(graph,&Ecken_Queue);
     ELSE
      dreh_winkel = winkel;
      DPUSH(Ecken_Queue,(char*)NECK_EDGELIST(Aussen_eck));
      EDGELIST_XW(NECK_EDGELIST(Ecken_Queue)) = dreh_winkel;
      hilf = EDGELIST_CORES(NECK_EDGELIST(Ecken_Queue));
      EDGELIST_XW(hilf) = ADD(180.0,dreh_winkel);
    ENDIF;


    WHILE NOT(DIS_EMPTY_LIST(Ecken_Queue)) DO
      DPOP_ELEMENT(Ecken_Queue,ecke,EDGELIST);
      node = EDGELIST_NODE(ecke);
      cores_ecke = EDGELIST_CORES(ecke);
      node1 = EDGELIST_NODE(cores_ecke);
      dreh_winkel = get_abs_winkel(ecke);

      printf("Betrachte die Gerade von : ");
      print_node(node);
      printf(" nach : ");
      print_node(node1);
      printf("\n");
      printf("Drehwinkel : %f \n",dreh_winkel);


        dreverse_for_all_elements(ecke,(char*)andere_ecke);
          IF NOT(EDGE_REACHED(EDGELIST_EDGE(andere_ecke))) THEN
            help = get_relative_winkel(andere_ecke);
            dreh_winkel = ADD(dreh_winkel , help);
            EDGELIST_XW(andere_ecke) = dreh_winkel;
            EDGELIST_XW(EDGELIST_CORES(andere_ecke)) = ADD(180.0,dreh_winkel);
            define_edge(andere_ecke,dreh_winkel);
            DQUEUE(Ecken_Queue,EDGELIST_CORES(andere_ecke));
            EDGE_REACHED(EDGELIST_EDGE(andere_ecke))  = TRUE;
           ELSE 
            dreh_winkel = get_abs_winkel(andere_ecke);
          ENDIF;
        dreverse_end_for_all_elements(ecke);
    ENDDO;
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

void bestimme_winkel(graph,Aussen_eck)
        Sgraph      graph;
        NECK        Aussen_eck;
  BEGIN
    printf("Bestimme_winkel \n ");
    korigiere_winkel(graph,Aussen_eck,0.0);
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

NECK  bestimme_Aussen_eck(Neck_list)
        NECK_LIST        Neck_list;
  BEGIN
    NECK        Neck;

    for_all_elements(Neck_list,Neck);
      IF LAST_ELEMENT THEN
        RETURN(Neck);
      END
    end_for_all_elements;
    RETURN( (NECK)NIL );
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

void plzeich(graph,Neck_list)
     Sgraph    graph;
     NECK_LIST Neck_list;
  BEGIN

    n = number_off_nodes(graph);
    e = number_off_edges_undirected(graph);
    vergebe_variablen(graph);
    Aussen_eck = bestimme_Aussen_eck(Neck_list);
    printf(" Aussen_Eck : ");
    print_Neck(Aussen_eck);
    simplex_verfahren(graph,Neck_list);
    debug_winkel(graph);
    bestimme_winkel(graph,Aussen_eck);
    compute_koordinaten(graph);
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

INTEGER compute_Level(graph,N)
     Sgraph    graph;
     INTEGER   N;
  BEGIN
     Snode     node;
     Sedge     edge;
     BOOLEAN   found = FALSE;

    for_all_nodes(graph,node);
      for_sourcelist(node,edge)
        IF NODE_LEVEL(edge->tnode) == 0 THEN
          found = TRUE;
          NODE_LEVEL(edge->tnode) = N + 1;
        ENDIF;
      end_for_sourcelist(node,edge)
    end_for_all_nodes(graph,node);  
    IF found THEN
      RETURN( compute_Level(graph,N+1));
     ELSE
      RETURN(N);
    ENDIF
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

INTEGER compute_node_Level(graph,Neck)
     Sgraph    graph;
     NECK      Neck;
  BEGIN
     Snode     node;
     NECK      neckelement;

    for_all_nodes(graph,node);
      NODE_LEVEL(node) = 0;
    end_for_all_nodes(graph,node);  
    for_all_neck_elements(Neck,neckelement)
      NODE_LEVEL(EDGELIST_NODE(NECK_EDGELIST(neckelement))) = 1;
    end_for_all_neck_elements(Neck);       
    RETURN(compute_Level(graph,1));
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

void  delete_edgelistelement(node,edgelistelement)
    Snode     node;
    EDGELIST *edgelistelement;
  BEGIN
     char      *dummy;

    dummy =  dpoplist(edgelistelement);
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

void  delete_node_edgelist(node)
     Snode     node;
  BEGIN

    WHILE NODE_EDGELIST(node) != NIL DO

      NODE_AUSSEN(EDGELIST_NODE(EDGELIST_CORES(NODE_EDGELIST(node)))) = TRUE;
      delete_edgelistelement(EDGELIST_NODE(EDGELIST_CORES(NODE_EDGELIST(node))),
                             &(EDGELIST_CORES(NODE_EDGELIST(node)))  );

      NODE_EDGELIST(EDGELIST_NODE(EDGELIST_CORES(NODE_EDGELIST(node)))) =
                                        EDGELIST_CORES(NODE_EDGELIST(node));

      delete_edgelistelement(node,&(NODE_EDGELIST(node)));
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

void  delete_node_with_level(graph,N)
        Sgraph    graph;
        INTEGER N;
  BEGIN
     Snode     node;
     ADRlist   merke;

    INIT_LIST(merke);
    for_all_nodes(graph,node);
        IF NODE_LEVEL(node) == N THEN
          PUSH_LIST(merke,node);
          delete_node_edgelist(node);
        ENDIF; 
    end_for_all_nodes(graph,node); 

    for_all_elements(merke,node)
      remove_node(node);
    end_for_all_elements; 
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

void optimiere_von_innen_nach_aussen(graph,Neck_list,N,Maxlevel)
        Sgraph    graph;
        NECK_LIST Neck_list;
        INTEGER N,Maxlevel;
  BEGIN
        Sgraph    graph2;

      printf(" optimiere_von_innen_nach_aussen %d %d ",N,Maxlevel);
      IF N < Maxlevel THEN
        graph2 = copy_sgraph_with_edgelist(graph);
        delete_node_with_level(graph2,N);
        draw_with_ausseneck_mode(graph2);
        store_xy_back(graph2,graph);
        fixiere_graph(graph2,graph);
        plzeich(graph,Neck_list);
        destroy_graph(&graph2);
       ELSE
        plzeich(graph,Neck_list);
      ENDIF;
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

void  optimiere_von_aussen_nach_innen(graph,Neck_list,N)
        Sgraph    graph;
        NECK_LIST Neck_list;
        INTEGER N;
  BEGIN
        Sgraph    graph2;

      IF N > 1  THEN
        graph2 = copy_sgraph_with_edgelist(graph);
        delete_node_with_level(graph2,N);
        draw_with_ausseneck_mode(graph2);
        store_xy_back(graph2,graph);
        destroy_graph(&graph2);
        plzeich(graph,Neck_list);
       ELSE
        plzeich(graph,Neck_list);
      ENDIF;
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

void  draw_with_optimierungsrichtung_mode(graph,Neck_list)
     Sgraph    graph;
     NECK_LIST Neck_list;
  BEGIN
    IF  get_optimierungsrichtung_mode() == MODE_GLOBALE_OPITMIERUNG THEN
      plzeich(graph,Neck_list);
     ELSE
      Aussen_eck = bestimme_Aussen_eck(Neck_list);
      IF get_optimierungsrichtung_mode() == MODE_INNEN_NACH_AUSSEN THEN
        optimiere_von_innen_nach_aussen(graph,Neck_list,
                                        1,compute_node_Level(graph,Aussen_eck));
       ELSE
        optimiere_von_aussen_nach_innen(graph,Neck_list,
                                        compute_node_Level(graph,Aussen_eck));
      ENDIF;
    ENDIF;
  END


