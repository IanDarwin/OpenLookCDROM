/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : interfac.c                                     */
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
#include "edgelist.h"
#include "neck.h"
#include "necklist.h"

#include "plan_sf.h"
#include "interfac.h"
#include "kan_sort.h"
#include "plzeich.h"
#include "local_op.h"
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

graph_att_Ptr make_graph_att()
  BEGIN
    graph_att_Ptr hilf;

    NEW(hilf);
    hilf->winkelzahl = 0;
    hilf->knotenzahl = 0;
    RETURN( hilf );
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

node_att_Ptr make_new_node_att()
  BEGIN
    node_att_Ptr hilf;
   
    NEW(hilf);
    hilf->old_nr       = 0;
    hilf->old_x        = 0;
    hilf->old_y        = 0;

    EDGELIST_INIT(hilf->edgelist);
    hilf->zaehler      = 0;
    hilf->grad         = 0;
    hilf->aussen       = FALSE;
    hilf->reached      = FALSE;
    hilf->fix          = FALSE;
    DINIT_LIST(hilf->adj.adj_list);
    hilf->adj.direction = TRUE;
    RETURN( hilf );
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

node_att_Ptr make_node_att(node)
      Snode   node;
  BEGIN
    node_att_Ptr hilf;
   
    hilf = make_new_node_att();
    hilf->old_nr       = node->nr;
    hilf->old_x        = snode_x(node);
    hilf->old_y        = snode_y(node);

    RETURN( hilf );
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

edge_att_Ptr make_edge_att()
  BEGIN
    edge_att_Ptr hilf;
   
    NEW(hilf);
    hilf->PQnode = NIL;
    hilf->x  = 0.0;
    hilf->y  = 0.0;
    hilf->fix = FALSE; 
    hilf->reached = FALSE;
    RETURN( hilf );
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

void loesche_aussen_node_attribute(graph)
        Sgraph graph;
  BEGIN
        Snode node;

  for_all_nodes(graph,node)
    NODE_AUSSEN(node) = FALSE; 
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

void loesche_fix_attribute(graph)
        Sgraph graph;
  BEGIN
        Snode node;
        Sedge edge;

  for_all_nodes(graph,node)
     NODE_FIX(node) = FALSE;
     for_unique_sourcelist(node,edge)
       EDGE_FIX(edge) = FALSE;
     end_for_unique_sourcelist(node,edge)
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

void setze_aussen_punkte(Work_sgraph,info)
        Sgraph Work_sgraph;
        Sgraph_proc_info info;
  BEGIN
        Slist l;

    CASE info->selected OF
      SGRAPH_SELECTED_NONE : 
              error ("Nothing selected.\n");
              BREAK
      SGRAPH_SELECTED_SNODE :
              loesche_aussen_node_attribute(Work_sgraph);
              setze_aussen_node(Work_sgraph,info->selection.snode->nr);
              BREAK
      SGRAPH_SELECTED_SEDGE :
              loesche_aussen_node_attribute(Work_sgraph);
              setze_aussen_node(Work_sgraph,info->selection.sedge->snode->nr);
              setze_aussen_node(Work_sgraph,info->selection.sedge->tnode->nr);
              BREAK
      SGRAPH_SELECTED_GROUP: 
              loesche_aussen_node_attribute(Work_sgraph);
              for_slist(info->selection.group,l)
                 setze_aussen_node(Work_sgraph,attr_data_of_type(l,Snode)->nr);
              end_for_slist(info->selection.group,l)
    ENDCASE;

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

void        setze_fixe_punkte(Work_sgraph,info)
        Sgraph Work_sgraph;
        Sgraph_proc_info info;
  BEGIN
        Slist l;

    CASE info->selected OF
      SGRAPH_SELECTED_NONE : 
              error ("Nothing selected.\n");
              BREAK
      SGRAPH_SELECTED_SNODE :
              loesche_fix_attribute(Work_sgraph);
              setze_fix_node(Work_sgraph,info->selection.snode);
              BREAK
      SGRAPH_SELECTED_SEDGE : 
              loesche_fix_attribute(Work_sgraph);
              setze_fix_edge(Work_sgraph,info->selection.sedge);
              BREAK
      SGRAPH_SELECTED_GROUP: 
              loesche_fix_attribute(Work_sgraph);
              for_slist(info->selection.group,l)
                 setze_fix_node(Work_sgraph,attr_data_of_type(l,Snode));
              end_for_slist(info->selection.group,l)

    ENDCASE;

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

void    draw_with_optimize_mode(Work_sgraph,Neck_list)
        Sgraph Work_sgraph;
        NECK_LIST Neck_list;
  BEGIN
#if TEST
    printf(" draw_with_optimize_mode \n ");
#endif
    IF get_optimize_mode() == MODE_TRAINGULATION THEN
     
      IF PQ_triangulation(Work_sgraph,&Neck_list)   THEN 
        message("Triangualtion.\n ");
        kanten_ausgabe3(Work_sgraph);
        besetze_datenstrucktur(Work_sgraph);
        Neck_list = compute_Neck_List(Work_sgraph);
       ELSE 
        message("Graph is already triangulated.\n");
      ENDIF;
    ENDIF;
    draw_with_optimierungsrichtung_mode(Work_sgraph,Neck_list);
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

void  draw_with_ausseneck_mode(graph)
        Sgraph graph;
  BEGIN
    NECK_LIST Neck_list;

#if TEST
    printf(" draw_with_ausseneck_mode \n ");
#endif
    besetze_datenstrucktur(graph);
    Neck_list = compute_Neck_List(graph);
    makiere_Aussenknoten(graph,Neck_list);
    draw_with_optimize_mode(graph,Neck_list);
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

void   draw_planar(Work_sgraph,info)
  Sgraph Work_sgraph;
  Sgraph_proc_info info;
  BEGIN
    Sgraph       copy_graph;




      IF compute_kantensortierung(Work_sgraph) THEN

        copy_graph = copy_sgraph_with_edgelist(Work_sgraph);

        draw_with_ausseneck_mode(copy_graph);
       
        store_xy_back(copy_graph,Work_sgraph); 
        store_xy_back_ueber_nr(Work_sgraph,info->sgraph); 

        force_repainting();
        destroy_graph(&copy_graph);
       
       ELSE
        error ("Cannot draw graph.\n");
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

void setze_aussen_node(graph,nr)
        Sgraph       graph;
        INTEGER      nr;
  BEGIN
    Snode   snode;

    snode = find_Snode_with_number(graph,nr);
    message("Node %s marked as external.\n ",snode ->label);
    NODE_AUSSEN(snode) = TRUE;
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

void setze_fix_node(graph,info_node)
        Sgraph       graph;
        Snode        info_node;
  BEGIN
    Snode   snode;

    snode = find_Snode_with_number(graph,info_node->nr);
    NODE_FIX(snode) = TRUE;
    message("Node %s fixed.\n ",snode ->label);
    snode_x(snode) = snode_x(info_node);
    snode_y(snode) = snode_y(info_node);
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

void setze_fix_edge(graph,info_edge)
        Sgraph       graph;
        Sedge        info_edge;
  BEGIN
    Snode   snode,tnode;
    Sedge   edge;

    snode = find_Snode_with_number(graph,info_edge->snode->nr);
    tnode = find_Snode_with_number(graph,info_edge->tnode->nr);
    edge  = find_edge(snode,tnode);
    EDGE_FIX(edge) = TRUE;
    compute_vektor(edge);
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

void   store_xy_back(Work_sgraph,sgraph)
  Sgraph Work_sgraph;
  Sgraph sgraph;
  BEGIN
    Snode node,hilf;
    for_all_nodes(Work_sgraph,node) 
      hilf = NODE_OLD_NODE(node);
      hilf->x = node->x ;
      hilf->y = node->y ;
    end_for_all_nodes(Work_sgraph,node);       
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

void   store_xy_back_ueber_nr(Work_sgraph,sgraph)
  Sgraph Work_sgraph;
  Sgraph sgraph;
  BEGIN
    Snode node,hilf;
    for_all_nodes(Work_sgraph,node) 
      hilf = find_Snode_with_number(sgraph,NODE_OLD_NR(node));
      hilf->x = node->x ;
      hilf->y = node->y ;
    end_for_all_nodes(Work_sgraph,node);       
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

void   fixiere_winkel(edgelistelement)
    EDGELIST edgelistelement;
  BEGIN
    REAL winkel;

    EDGELIST_FIX(edgelistelement) = TRUE;
    winkel = winkel_zwische_zwei_kanten(EDGE_X(EDGELIST_EDGE(edgelistelement)),
                                      EDGE_Y(EDGELIST_EDGE(edgelistelement)),
                                      EDGE_X(EDGELIST_EDGE(EDGELIST_SUC(edgelistelement))),
                                      EDGE_Y(EDGELIST_EDGE(EDGELIST_SUC(edgelistelement))));

    EDGELIST_FIX_XW(edgelistelement) = winkel;
#if TEST
    printf(" fixiere_winkel : %f \n", winkel);
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

void   fixiere_edge(edge)
    Sedge    edge;
  BEGIN
    EDGE_FIX(edge) = TRUE;
    compute_vektor_with_new_xy(edge);
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

void   fixiere_graph(Work_sgraph,sgraph)
  Sgraph Work_sgraph;
  Sgraph sgraph;
  BEGIN
    Snode    node,hilf;
    Sedge    edge;
    EDGELIST edgelistelement;

    for_all_nodes(Work_sgraph,node) 
      hilf = NODE_OLD_NODE(node);
      NODE_FIX(hilf) = TRUE;
    end_for_all_nodes(Work_sgraph,node); 

    for_all_nodes(sgraph,node) 
      IF NODE_FIX(node) THEN
        for_sourcelist(node,edge)
           IF NODE_FIX(edge->tnode) THEN
             fixiere_edge(edge);
           ENDIF;
        end_for_sourcelist(node,edge);
      ENDIF;
    end_for_all_nodes(sgraph,node); 
      
    for_all_nodes(sgraph,node)
      for_edgelist(node,edgelistelement)
         IF EDGE_FIX(EDGELIST_EDGE(edgelistelement)) AND
            EDGE_FIX(EDGELIST_EDGE(EDGELIST_SUC(edgelistelement))) THEN 
            fixiere_winkel(edgelistelement);
         ENDIF;
      end_for_edgelist(node);
    end_for_all_nodes(sgraph,node); 
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

BOOLEAN equal_graph(graph1,graph2)
        Sgraph graph1;
        Sgraph graph2;
  BEGIN
        Snode node1,node2;
 
   IF (graph1 == empty_graph) OR (graph2 == empty_graph) THEN
      RETURN(FALSE)
    ENDIF;
    IF number_off_nodes(graph1) != number_off_nodes(graph2) THEN
      RETURN(FALSE)
    ENDIF;
    IF number_off_edges_undirected(graph1) != 
             number_off_edges_undirected(graph2)  THEN
      RETURN(FALSE)
    ENDIF;
    for_all_nodes (graph1,node1);
       node2 = find_Snode_with_number(graph2,node1->nr);
       IF node2 == empty_node THEN 
         RETURN(FALSE)
        ELSE
         IF number_off_edges_at_node_undirected(node1) != 
              number_off_edges_at_node_undirected(node2)  THEN
            RETURN(FALSE);
         ENDIF;
       ENDIF;
    end_for_all_nodes (graph1,node1);

    RETURN(TRUE);    
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

void  draw_local(Work_sgraph,list)
        Sgraph Work_sgraph;
        Slist  list;
  BEGIN
    printf(" draw_local \n");

    IF test_planar_drawing(Work_sgraph) THEN
      IF koordinaten_to_vektoren(Work_sgraph) THEN
        verbessere_graph(Work_sgraph,list,16,1);
       ELSE
        error ("Graph is not drawn planar.\n");
      ENDIF;
     ELSE
      message("Graph is not drawn planar.\n");
    ENDIF;
  ENDPROC
