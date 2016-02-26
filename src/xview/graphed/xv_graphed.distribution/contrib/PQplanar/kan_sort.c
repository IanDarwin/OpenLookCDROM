/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : kan_sort.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 25.07.90                                       */
/*****************************************************************/
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI
#include SLISTI
#include GRAPHEDI

#include "sgraphhi.h"
#include "modula.h"
#include "listen.h"
#include "listen1.h"
#include "adj.h"
#include "dfs.h"
#include "edgelist.h"
#include "neck.h"
#include "necklist.h"
#include "stnumber.h"
#include "plzeich.h"

#include "plan_sf.h"
#include "interfac.h"
#include "kan_sort.h"
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

BOOLEAN compute_kantensortierung(Work_sgraph)
        Sgraph Work_sgraph;
  BEGIN
         BOOLEAN sortierung_ok;
         Snode        s, t;
         Sedge        e;

    IF get_kantensortierung_mode() == MODE_AUS_PLANARTEST THEN
      s = first_node_in_graph(Work_sgraph);
      e = s -> slist;
      IF !e THEN
         e = s -> tlist;
         t = e -> snode;
        ELSE
         t = e -> tnode;
      ENDIF;

      IF get_undirected_st_number (Work_sgraph,s,t ) THEN
        IF planar(Work_sgraph) THEN 
          message("The graph is planar.\n");
          sortierung_ok = TRUE;
#if TEST
          printf("Kantenliste zu Knoten mit hoeherer ST-Nummer : \n");
          kanten_ausgabe(Work_sgraph);
#endif
          init_DFS(Work_sgraph);
          init_DFS2(Work_sgraph);
          DFS(Work_sgraph,find_Snode_with_number(Work_sgraph,1));
         ELSE
          message("The graph is not planar.\n");
          sortierung_ok = FALSE;
        ENDIF
       ELSE
          message("The graph is not ST-numbered.\n");
          sortierung_ok = FALSE;
      ENDIF
       
     ELSE
      IF test_planar_drawing(Work_sgraph) THEN
        IF koordinaten_to_vektoren(Work_sgraph) THEN
          sortierung_ok = TRUE;
         ELSE
          sortierung_ok = FALSE;
          message("The graph is not drawn planar.\n");
        ENDIF;
       ELSE
        sortierung_ok = FALSE;
        message("The graph ist not drawn planar.\n");
      ENDIF;
    ENDIF;
    IF sortierung_ok THEN
#if TEST
       printf("Kantenliste zu allen Knoten  : \n");
       kanten_ausgabe2(Work_sgraph);    
       kanten_ausgabe3(Work_sgraph);    
#endif
    ENDIF;
    RETURN(sortierung_ok);
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

void kanten_ausgabe(graph)
        Sgraph       graph;
BEGIN
  Snode node;
  EDGELIST ecke;

#if TEST
  for_all_nodes(graph,node) 
     print_node(node);
     printf(" : ");
     dfor_all_elements(NODE_ADJ_LIST(node),(char*)ecke)
       print_node(OTHER_NODE(node,EDGELIST_EDGE(ecke) ) );
     dend_for_all_elements(NODE_ADJ_LIST(node));
     printf("\n"); 
  end_for_all_nodes(graph,node);
#endif
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

void kanten_ausgabe2(graph)
        Sgraph       graph;
BEGIN
  Snode node;
  EDGELIST ecke;

#if TEST
  for_all_nodes(graph,node) 
     print_node(node);
     printf(" : ");
     for_edgelist(node,ecke)
       print_node(OTHER_NODE(node,EDGELIST_EDGE(ecke) ) );
     end_for_edgelist(node);
     printf("\n"); 
  end_for_all_nodes(graph,node);
#endif
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

void kanten_ausgabe3(graph)
        Sgraph       graph;
BEGIN
  Snode node;
  EDGELIST ecke;

#if TEST
  printf("CORES : \n");
  for_all_nodes(graph,node) 
     print_node(node);
     printf(" : ");
     for_edgelist(node,ecke)
       print_node(EDGELIST_NODE(EDGELIST_CORES(ecke) ) );
     end_for_edgelist(node);
     printf("\n"); 
  end_for_all_nodes(graph,node);
#endif
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

REAL winkel_zwische_zwei_kanten(x1,y1,x2,y2)
    REAL x1,y1,x2,y2;
  BEGIN
    RETURN (180.0 / M_PI) * acos( x1*x2 + y1*y2);
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

REAL abs_winkel_der_kanten(x1,y1)
    REAL x1,y1;
  BEGIN
        REAL result;
    IF y1 >= 0.0 THEN
      result = winkel_zwische_zwei_kanten(x1,y1,1.0,0.0);
     ELSE
      result = 360.0 - winkel_zwische_zwei_kanten(x1,y1,1.0,0.0);
    ENDIF;
    RETURN(result);
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

REAL winkel(edge)
        Sedge edge;
  BEGIN
        REAL result;

    IF EDGE_Y(edge) >= 0.0 THEN
      result = winkel_zwische_zwei_kanten(EDGE_X(edge),EDGE_Y(edge),1.0,0.0);
     ELSE
      result = 360.0 - winkel_zwische_zwei_kanten(EDGE_X(edge),EDGE_Y(edge),1.0,0.0);
    ENDIF;
    RETURN( result );
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

REAL r_winkel(a)
        edge_list_Ptr a;
  BEGIN
        REAL result;
        Sedge edge;
        Snode cores;

    edge = a->edge;
    cores = OTHER_NODE(a->node,edge);
    IF a->node->nr < cores->nr THEN 
      result = winkel(a->edge);
     ELSE
      result = ADD(180,winkel(a->edge));
    ENDIF;
    RETURN(result);
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

BOOLEAN less_winkel(a,b)
        edge_list_Ptr a,b;
  BEGIN
    print_node(a->node);
    print_node(OTHER_NODE(a->node,a->edge));
    printf(" : %f \n ",r_winkel(a));
    print_node(b->node);
    print_node(OTHER_NODE(b->node,b->edge));
    printf(" : %f \n ",r_winkel(b));

    RETURN(r_winkel(a) < r_winkel(b) )
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

void compute_vektor(edge)
        Sedge edge;
  BEGIN
        REAL     x_dif,y_dif;
        REAL    abstand;

    x_dif = (REAL)(NODE_OLD_X(edge->tnode) - NODE_OLD_X(edge->snode));
    y_dif = (REAL)(NODE_OLD_Y(edge->tnode) - NODE_OLD_Y(edge->snode));
#if TEST
    printf(" compute_vektor : %f %f \n",x_dif,y_dif);
#endif
    abstand = sqrt( (x_dif*x_dif) + (y_dif*y_dif) );
 
   IF mein_unique_edge(edge) THEN
      EDGE_X(edge) = x_dif / abstand ;
      EDGE_Y(edge) = y_dif / abstand ;
     ELSE
      EDGE_X(edge) = - x_dif / abstand ;
      EDGE_Y(edge) = - y_dif / abstand ;
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

void compute_vektor_with_new_xy(edge)
        Sedge edge;
  BEGIN
        REAL    x_dif,y_dif;
        REAL    abstand;

    x_dif = (REAL)(NODE_X(edge->tnode) - NODE_X(edge->snode));
    y_dif = (REAL)(NODE_Y(edge->tnode) - NODE_Y(edge->snode));
#if TEST
    printf(" compute_vektor_new : %f %f \n",x_dif,y_dif);
#endif
    abstand = sqrt( (x_dif*x_dif) + (y_dif*y_dif) );
    IF f_equal(abstand,0.0) THEN
      EDGE_X(edge) = 0.0 ;
      EDGE_Y(edge) = 0.0 ;
     ELSE
      IF mein_unique_edge(edge) THEN
        EDGE_X(edge) = x_dif / abstand ;
        EDGE_Y(edge) = y_dif / abstand ;
       ELSE
        EDGE_X(edge) = x_dif / abstand ;
        EDGE_Y(edge) = y_dif / abstand ;
      ENDIF;
    ENDIF;

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

BOOLEAN equal_edgelist_edge(ecke_element,edge)
      edge_list_Ptr ecke_element;
      Sedge         edge;
  BEGIN
    RETURN(equal_edge(ecke_element->edge,edge));
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

void conect_edgelist(Work_sgraph)
        Sgraph Work_sgraph;
  BEGIN
        Snode node;
        Sedge edge;
        EDGELIST ecke1,ecke2;

    for_all_nodes(Work_sgraph,node) 
      for_unique_sourcelist(node,edge)
         ecke1 = dsearchlist(NODE_EDGELIST(node),(char*)edge,equal_edgelist_edge);
         ecke2 = dsearchlist(NODE_EDGELIST(edge->tnode),(char*)edge,equal_edgelist_edge);
         IF (ecke1 == NIL) OR (ecke2 == NIL) THEN
           printf("Fehler bei conect_edgelist \n");
          ELSE
           edgelist_verbinde(ecke1,ecke2);
         ENDIF;
      end_for_unique_sourcelist(node,edge)
    end_for_all_nodes(Work_sgraph,node);

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

BOOLEAN koordinaten_to_vektoren(Work_sgraph) 
        Sgraph Work_sgraph;
  BEGIN
        Snode node;
        Sedge edge;

    for_all_nodes(Work_sgraph,node) 
      DCLEAR_LIST(NODE_EDGELIST(node));
      for_sourcelist(node,edge)
        compute_vektor(edge);
        PUSH_EDGELIST(NODE_EDGELIST(node),node,edge);
      end_for_sourcelist(node,edge)
      SORT_EDGELIST(NODE_EDGELIST(node),less_winkel);
    end_for_all_nodes(Work_sgraph,node);

    conect_edgelist(Work_sgraph);

    RETURN(TRUE);  
  END

