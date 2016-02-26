/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : necklist.c                                      */
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
#include "edgelist.h"
#include "plzeich.h"
#include "gauss.h"
#include "sgraphhi.h"
#include "simplex.h"
#include "neck.h"
#include "necklist.h"
#include "plan_sf.h"
#include "adj.h"
#include "interfac.h"
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

BOOLEAN Necklist_equal_element(e1,e2)
        char *e1,*e2;
  BEGIN
    RETURN(  e1 == e2 );
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

void Necklist_delete_element(Necklist,Neck)
        NECK_LIST  *Necklist;
        NECK       Neck;
  BEGIN
    (*Necklist) = deleteElement((*Necklist),(char*)Neck,Necklist_equal_element);
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

void Necklist_push(Necklist,Neck)
        NECK_LIST  *Necklist;
        NECK       Neck;
  BEGIN
    PUSH_LIST((*Necklist),Neck);
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

void print_necklist(Necklist)
        NECK_LIST  Necklist;
  BEGIN
    NECK       Neck;

    for_all_elements(Necklist,Neck)
      print_Neck(Neck);
    end_for_all_elements;
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

NECK_LIST compute_Neck_List(Work_sgraph)
        Sgraph Work_sgraph;
  BEGIN
        NECK_LIST  Neck_list;

    INIT_LIST(Neck_list);
    suche_Neck_im_graphen(Work_sgraph,&Neck_list);
    print_necklist(Neck_list);
    RETURN(Neck_list);
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

void makiere_Aussenknoten(graph,Neck_list)
        Sgraph graph;
        NECK_LIST  Neck_list;

  BEGIN
        Snode  node;
        NECK   Neck,Neck_element;
        EDGELIST ecke;

    for_all_nodes(graph,node)
      NODE_AUSSEN(node) = FALSE;
      for_edgelist(node,ecke)
        EDGELIST_AUSSEN(ecke) = FALSE;
      end_for_edgelist(node)
    end_for_all_nodes(graph,node)
    for_all_elements(Neck_list,Neck)
      IF LAST_ELEMENT THEN
        for_all_neck_elements(Neck,Neck_element)
           EDGELIST_AUSSEN(NECK_ECKE(Neck_element)) = TRUE;
           NODE_AUSSEN(EDGELIST_NODE(NECK_ECKE(Neck_element))) = TRUE;
        end_for_all_neck_elements(Neck);
      ENDIF;
    end_for_all_elements;
  ENDPROC;

