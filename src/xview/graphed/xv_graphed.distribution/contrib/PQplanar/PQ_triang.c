/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : triang.c                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 14.08.90                                       */
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
#include "simplex.h"
#include "simplexh.h"
#include "neck.h"
#include "adj.h"
#include "interfac.h"
#include "necklist.h"
#include "plan_sf.h"
#include "triang.h"
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

void  tiefen_triangulierung(edgelist)
    EDGELIST edgelist;
  BEGIN
    EDGELIST ecke,first_new,second_new;

    EDGELIST_REACHED(edgelist) = TRUE;
    printf(" tiefen_triangulierung : ");
    print_node(EDGELIST_NODE(edgelist));
    print_node(EDGELIST_NODE(EDGELIST_CORES(edgelist)));
    printf("\n");

    ecke = edgelist;

    IF NOT(EDGELIST_AUSSEN(ecke)) THEN 
        IF EDGELIST_NODE(EDGELIST_CORES(EDGELIST_SUC(ecke))) !=
           EDGELIST_NODE(EDGELIST_CORES(EDGELIST_PRE(EDGELIST_CORES(ecke))))  THEN
           IF NOT(find_edge(EDGELIST_NODE(EDGELIST_SUC(ecke))
                    ,EDGELIST_NODE(EDGELIST_CORES(EDGELIST_PRE(EDGELIST_CORES(ecke))))
                           )
                 ) THEN
              insert_new_edge( EDGELIST_SUC(ecke),
                               EDGELIST_CORES(EDGELIST_PRE(EDGELIST_CORES(ecke))),
                              &first_new,&second_new);
          ENDIF;
        ENDIF;
    ENDIF;


    dfor_all_elements(edgelist,(char*)ecke)
      IF NOT( EDGELIST_REACHED(EDGELIST_CORES(ecke)) ) THEN
        tiefen_triangulierung(EDGELIST_CORES(ecke));
      ENDIF;      
    dend_for_all_elements(edgelist)

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

void  tiefensuche_triangulierung(graph,Necklist) 
        Sgraph      graph;
        NECK_LIST  *Necklist;
  BEGIN
        Snode   node;
        EDGELIST ecke;

    for_all_nodes(graph,node)
      for_edgelist(node,ecke)
        EDGELIST_REACHED(ecke) = FALSE;
      end_for_edgelist(node)
    end_for_all_nodes(graph,node);
     
    node = first_node_in_graph(graph);
    tiefen_triangulierung(NODE_EDGELIST(node));
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

void  punkt_triangulierung(graph,Necklist,Neck)
        Sgraph      graph;
        NECK_LIST  *Necklist;
        NECK        Neck;
  BEGIN
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

void  knoten_sortierung_triangulierung(graph,Necklist,Neck)
        Sgraph      graph;
        NECK_LIST  *Necklist;
        NECK        Neck;
  BEGIN

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

void  strahl_triangulierung(Necklist,Neck)
        NECK_LIST  *Necklist;
        NECK        Neck;
  BEGIN
    NECK    Drei_Eck;

          WHILE grad(Neck) > 3 DO
            IF splitte_strahl(&Neck,&Drei_Eck) THEN
              Necklist_push(Necklist,Drei_Eck);
            ENDIF;
          ENDDO;
          Necklist_push(Necklist,Neck);
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

void  rand_triangulierung(Necklist,Neck)
        NECK_LIST  *Necklist;
        NECK        Neck;
  BEGIN
    NECK    Drei_Eck;

          WHILE grad(Neck) > 3 DO
            IF splitte_rand(&Neck,&Drei_Eck) THEN
              Necklist_push(Necklist,Drei_Eck);
            ENDIF;
          ENDDO;
          Necklist_push(Necklist,Neck);
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

BOOLEAN PQ_triangulation(graph,Necklist)
        Sgraph      graph;
        NECK_LIST  *Necklist;
  BEGIN
    NECK    Neck;
    BOOLEAN result;

    printf("Triangulation \n ");
    result = FALSE;
    IF get_triangulierungs_mode() == MODE_TIEFENSUCHE_TRIANGULIERUNG THEN
      tiefensuche_triangulierung(graph,Necklist);
      result = TRUE;
     ELSE
      for_all_elements((*Necklist),Neck)
        IF NOT(LAST_ELEMENT) THEN
          IF grad(Neck) > 3 THEN
            result = TRUE;
            Necklist_delete_element(Necklist,Neck);
            IF get_triangulierungs_mode() == MODE_STRAHL_TRIANGULIERUNG THEN
              strahl_triangulierung(Necklist,Neck);
             ELSE
              IF get_triangulierungs_mode() == MODE_RAND_TRIANGULIERUNG THEN
                 rand_triangulierung(Necklist,Neck);
                ELSE
                 IF get_triangulierungs_mode() == MODE_PUNKT_EIN_TRIANGULIERUNG THEN
                   punkt_triangulierung(graph,Necklist,Neck);
                  ELSE
                   knoten_sortierung_triangulierung(graph,Necklist,Neck);
                 ENDIF;
              ENDIF;
            ENDIF;
          ENDIF;
        ENDIF;
      end_for_all_elements;
    ENDIF;
    RETURN(result);
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

void    insert_new_edge(first,third,first_new,third_new)
    EDGELIST first;
    EDGELIST third;
    EDGELIST *first_new;
    EDGELIST *third_new;
  BEGIN
    Sedge edge;
    printf(" insert_new_edge : ");
    print_node(EDGELIST_NODE(first));
    printf(" vor: ");
    print_node(EDGELIST_NODE(EDGELIST_CORES(first)));
    print_node(EDGELIST_NODE(third));
    printf(" vor: ");
    print_node(EDGELIST_NODE(EDGELIST_CORES(third)));
    printf("\n");

    (*first_new) = edgelist_insert_before(first);
    (*third_new) = edgelist_insert_before(third);
    edgelist_verbinde((*first_new),(*third_new));
    vergebe_variable((*first_new));
    vergebe_variable((*third_new));
    edge = make_edge(EDGELIST_NODE((*first_new)),EDGELIST_NODE((*third_new)),
                     make_attr(ATTR_DATA,make_edge_att()));
    EDGELIST_EDGE((*first_new)) =  edge;
    EDGELIST_EDGE((*third_new)) =  edge;

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

BOOLEAN splitte(Neck,Drei_Eck)
        NECK *Neck;
        NECK *Drei_Eck;
  BEGIN
    EDGELIST first;
    EDGELIST third;
    EDGELIST first_new;
    EDGELIST third_new;

    printf("Splitte \n ");
    print_Neck((*Neck));
    Neck_init(Drei_Eck);
    first = Neck_nth_element((*Neck),1);
    third = Neck_nth_element((*Neck),3);
    IF NOT(find_edge(EDGELIST_NODE(first),EDGELIST_NODE(third))) THEN 
      insert_new_edge(EDGELIST_SUC(first),EDGELIST_SUC(third),&first_new,&third_new);
      Neck_push(Drei_Eck,third_new);
      Neck_push(Drei_Eck,Neck_nth_element((*Neck),2));
      Neck_push(Drei_Eck,first);

      Neck_set_nth_element((*Neck),1,first_new);
      Neck_delete_nth_element((*Neck),2);

      print_Neck((*Neck));
      printf("Drei- ");
      print_Neck((*Drei_Eck));
      RETURN(TRUE);
     ELSE
      (*Neck) = EDGELIST_SUC((*Neck)) ;
      RETURN(FALSE);
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

BOOLEAN splitte_strahl(Neck,Drei_Eck)
        NECK *Neck;
        NECK *Drei_Eck;
  BEGIN
    RETURN(splitte(Neck,Drei_Eck));
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

BOOLEAN splitte_rand(Neck,Drei_Eck)
        NECK *Neck;
        NECK *Drei_Eck;
  BEGIN
    IF splitte(Neck,Drei_Eck) THEN
      (*Neck) = EDGELIST_SUC((*Neck));
      RETURN(TRUE);
     ELSE
      RETURN(FALSE);
    ENDIF;  
  END;
 
