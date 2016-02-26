/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : local_op.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 09.10.90                                       */
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
#include "interfac.h"
#include "kan_sort.h"
#include "local_sf.h"
#include <math.h>



#define LI_FLOAT  ADRlist


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Local REAL distanz_zwischen_zwei_punkten(x1,y1,x2,y2)
    INTEGER x1,y1,x2,y2;
  BEGIN
    RETURN(sqrt((REAL)(x1-x2)*(REAL)(x1-x2)+(REAL)(y1-y2)*(REAL)(y1-y2)));
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

Local void normiere_vektor(x,y)
   REAL *x,*y;
  BEGIN
        REAL laenge;

    laenge = sqrt((*x)*(*x) + (*y)*(*y));
    (*x) = (*x) / laenge;
    (*y) = (*y) / laenge;
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

Local REAL berechne_winkel_zwischen_drei_punkten(x1,y1,x2,y2,x3,y3)
    INTEGER x1,y1,x2,y2,x3,y3;
  BEGIN
    REAL kx1,ky1,kx2,ky2;
    REAL result;
    REAL w1,w2;
/*
    printf(" berechne_winkel_zwischen_drei_punkten %d %d %d %d %d %d \n",x1,y1,x2,y2,x3,y3);
*/
    kx1 = (REAL)x1-x2;
    ky1 = (REAL)y1-y2;
    kx2 = (REAL)x3-x2;
    ky2 = (REAL)y3-y2;
    normiere_vektor(&kx1,&ky1);
    normiere_vektor(&kx2,&ky2);
    w1 = abs_winkel_der_kanten(kx1,ky1);
    w2 = abs_winkel_der_kanten(kx2,ky2);
    result = w1 - w2;
    IF result < 0 THEN 
      result = result + 360.0;
    ENDIF;
/*
    printf(" berechne_winkel_zwischen_drei_punkten %f \n",result);
*/
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

Local REAL berechne_winkel_vor_gerade(x,y,edgelist)
        INTEGER x;
        INTEGER y;
        EDGELIST edgelist;
  BEGIN
         Snode node1,node2;

     node1 = EDGELIST_NODE(EDGELIST_CORES(edgelist));
     node2 = EDGELIST_NODE(EDGELIST_CORES(EDGELIST_PRE(EDGELIST_CORES(edgelist))));
/*
#if TEST 
     printf(" berechne_winkel_vor_gerade ");
     print_node(node1);
     print_node(node2);
     printf("\n");
#endif
*/
     RETURN(berechne_winkel_zwischen_drei_punkten(NODE_OLD_X(node2),NODE_OLD_Y(node2),
                                                  NODE_OLD_X(node1),NODE_OLD_Y(node1),
                                                  x,y));
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

Local REAL berechne_winkel_nach_gerade(x,y,edgelist)
        INTEGER x;
        INTEGER y;
        EDGELIST edgelist;
  BEGIN
         Snode node1,node2;

     node1 = EDGELIST_NODE(EDGELIST_CORES(edgelist));
     node2 = EDGELIST_NODE(EDGELIST_CORES(EDGELIST_SUC(EDGELIST_CORES(edgelist))));
/*
#if TEST
     printf(" berechne_winkel_nach_gerade ");
     print_node(node1);
     print_node(node2);
     printf("\n");
#endif
*/
     RETURN(berechne_winkel_zwischen_drei_punkten(x,y,
                                           NODE_OLD_X(node1),NODE_OLD_Y(node1),
                                           NODE_OLD_X(node2),NODE_OLD_Y(node2)));
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

Local REAL berechne_winkel_fuer_punkt(x,y,edgelist)
        INTEGER x;
        INTEGER y;
        EDGELIST edgelist;
  BEGIN
         Snode node1,node2;

     node1 = EDGELIST_NODE(EDGELIST_CORES(edgelist));
     node2 = EDGELIST_NODE(EDGELIST_CORES(EDGELIST_SUC(edgelist)));
/*
#if TEST
     printf(" berechne_winkel_fuer_punkt ");
     print_node(node1);
     print_node(node2);
     printf("\n");
#endif
*/
     RETURN(berechne_winkel_zwischen_drei_punkten(
                                           NODE_OLD_X(node1),NODE_OLD_Y(node1),
                                           x,y,
                                           NODE_OLD_X(node2),NODE_OLD_Y(node2)));

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

Local REAL  init_winkel()
  BEGIN
    IF get_local_optimize_mode() == LOCAL_MODE_MAX_MIN THEN
      RETURN(360.0);
     ELSE
      RETURN(0.0);
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

Local REAL  init_distanz()
  BEGIN
    IF get_local_optimize_mode() == LOCAL_MODE_MAX_MIN THEN
      RETURN(10000000.0);
     ELSE
      RETURN(0.0);
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

Local REAL  init_knoten_wert()
  BEGIN
    IF get_knoten_mode() == LOCAL_MODE_SMALEST THEN
      RETURN(1000000000.0);
     ELSE
      RETURN(0.0);
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

Local BOOLEAN better(w1,w2) 
        REAL w1,w2;
  BEGIN
     IF get_local_optimize_mode() == LOCAL_MODE_MAX_MIN THEN
      RETURN(f_greater(w1,w2));
     ELSE
      RETURN(f_greater(w2,w1));
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

Local BOOLEAN better_pointer(w1,w2) 
        REAL *w1,*w2;
  BEGIN
    RETURN(f_greater((*w2),(*w1)));
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

Local BOOLEAN better_knoten(w1,w2) 
        REAL w1,w2;
  BEGIN
     IF get_knoten_mode() == LOCAL_MODE_SMALEST THEN
      RETURN(f_greater(w1,w2));
     ELSE
      RETURN(f_greater(w2,w1));
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

Local LI_FLOAT list_around(x_dist,y_dist,node)
        INTEGER x_dist;
        INTEGER y_dist;
        Snode   node;
  BEGIN
        REAL min_winkel;
        REAL *w;
        EDGELIST edgelist;
        INTEGER x,y;
        REAL dist,max_dist,min_dist,summe,anzahl;
        LI_FLOAT result;
/*
#if TEST
    printf (" min_winkel_around %d %d ",x_dist,y_dist);
    print_node(node);
    printf("\n");
#endif
*/
  INIT_LIST(result);
  x = NODE_OLD_X(node) + x_dist;
  y = NODE_OLD_Y(node) + y_dist;
  IF get_optimize_object_mode() == LOCAL_OPTIMIZE_OBJECT_WINKEL THEN

    for_edgelist(node,edgelist)
      NEW(w);
      (*w) = berechne_winkel_fuer_punkt(x,y,edgelist);
      INSERT_SORT(result,w,better_pointer);
      NEW(w);
      (*w) = berechne_winkel_vor_gerade(x,y,edgelist);
      INSERT_SORT(result,w,better_pointer);
      NEW(w);
      (*w) = berechne_winkel_nach_gerade(x,y,edgelist);
      INSERT_SORT(result,w,better_pointer);
    end_for_edgelist(node)
   ELSE
    for_edgelist(node,edgelist)
      NEW(w);
      (*w) = distanz_zwischen_zwei_punkten(x,y,
                           NODE_OLD_X(EDGELIST_NODE(EDGELIST_CORES(edgelist))),
                           NODE_OLD_Y(EDGELIST_NODE(EDGELIST_CORES(edgelist)) ));
      INSERT_SORT(result,w,better_pointer);
    end_for_edgelist(node);  
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

Local REAL varianz(list)
        LI_FLOAT list;
  BEGIN
        REAL result;
        REAL *w;
        REAL summe,anzahl,durchschnitt;

    anzahl = 0.0;
    summe = 0.0;
    for_all_elements(list,(char*)w)
      anzahl = anzahl + 1;
      summe = summe + (*w);
    end_for_all_elements;
    durchschnitt = summe / anzahl;
    summe = 0.0;
    for_all_elements(list,(char*)w)
      summe = summe + ((durchschnitt - (*w)) * (durchschnitt - (*w)));
    end_for_all_elements;
    result = summe / anzahl;
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

Local REAL list_to_real(list)
        LI_FLOAT list;
  BEGIN
        REAL result;
        REAL *w;

    IF get_local_optimize_mode() == LOCAL_MODE_MAX_MIN THEN
      w = (REAL*)first_element(list);
      result = (*w);
     ELSE
      IF get_local_optimize_mode() == LOCAL_MODE_MAX_MIN THEN
        w = (REAL*)last_element(list);
        result = (*w);
       ELSE
        result = varianz(list);
      ENDIF;
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

Local REAL min_winkel_around(x_dist,y_dist,node)
        INTEGER x_dist;
        INTEGER y_dist;
        Snode   node;
  BEGIN
        LI_FLOAT list;
        REAL result;

    list = list_around(x_dist,y_dist,node);
    result = list_to_real(list);
    CLEAR_LIST(list);
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

Local void move_node(x_dist,y_dist,node)
        INTEGER x_dist;
        INTEGER y_dist;
        Snode   node;
  BEGIN
      NODE_OLD_X(node) = NODE_OLD_X(node) + x_dist;
      NODE_OLD_Y(node) = NODE_OLD_Y(node) + y_dist;
      node_set(graphed_node(node),NODE_POSITION,NODE_OLD_X(node),NODE_OLD_Y(node),0);
      force_repainting();
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

Local BOOLEAN besserer_min_winkel_around(x_dist,y_dist,min_vor,node)
        INTEGER x_dist;
        INTEGER y_dist;
        REAL *min_vor;
        Snode   node;
  BEGIN
        BOOLEAN result;
        REAL    hilf;
/*
#if TEST
    printf(" besserer_min_winkel_around %d %d %f",x_dist,y_dist,*min_vor);
    print_node(node);
    printf("\n");
#endif
*/
    result = FALSE;
    hilf = min_winkel_around(x_dist,y_dist,node);
    IF better(hilf,(*min_vor)) THEN
      result = TRUE;
      (*min_vor) = hilf;
      IF get_richtungs_mode() == LOCAL_MODE_ALL THEN
        move_node(x_dist,y_dist,node);
      ENDIF;
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

Local BOOLEAN verbessere_node(node,dist,x_dist,y_dist,min_vor)
        Snode   node;
        INTEGER dist;
        INTEGER *x_dist,*y_dist;
        REAL *min_vor;
  BEGIN
        BOOLEAN verbessert;

#if TEST
    printf(" verbessere_node %d ",dist);
    print_node(node);
    printf("\n");
#endif

    (*min_vor) = min_winkel_around(0,0,node);
    verbessert = FALSE;

      IF besserer_min_winkel_around(dist,0,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = dist;
        (*y_dist) = 0;
      ENDIF;
      IF besserer_min_winkel_around(dist,dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = dist;
        (*y_dist) = dist;
      ENDIF;
      IF besserer_min_winkel_around(dist,-dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = dist;
        (*y_dist) = -dist;
      ENDIF;
      IF besserer_min_winkel_around(0,dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = 0;
        (*y_dist) = dist;
      ENDIF;
      IF besserer_min_winkel_around(0,-dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = 0;
        (*y_dist) = -dist;
      ENDIF;
      IF besserer_min_winkel_around(-dist,0,min_vor,node) THEN
       verbessert = TRUE;
        (*x_dist) = -dist;
        (*y_dist) = 0;
      ENDIF;
      IF besserer_min_winkel_around(-dist,dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = -dist;
        (*y_dist) = dist;
      ENDIF;
      IF besserer_min_winkel_around(-dist,-dist,min_vor,node) THEN
        verbessert = TRUE;
        (*x_dist) = -dist;
        (*y_dist) = -dist;
      ENDIF;
      IF verbessert THEN
        IF get_richtungs_mode() == LOCAL_MODE_BEST_DIRECTION THEN
          move_node((*x_dist),(*y_dist),node);
        ENDIF;
      ENDIF;
    RETURN(verbessert);
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

void    verbessere_graph(Work_sgraph,list,max_dist,min_dist)
        Sgraph     Work_sgraph;
        Slist      list;
        INTEGER    max_dist,min_dist;
  BEGIN
        INTEGER  dist;
        BOOLEAN  verbessert;
        Snode    best_node,node;
        INTEGER  best_x,best_y,x_dist,y_dist;
        REAL     min_vor;
        REAL     min_winkel;
        Slist    list_element;

#if TEST
    printf(" verbessere_graph \n");
#endif
    dist = max_dist;
    WHILE dist > min_dist DO
      verbessert = TRUE;
      WHILE verbessert DO
        verbessert = FALSE;
        min_winkel = init_knoten_wert();
        for_slist(list,list_element)
          node = attr_data_of_type(list_element,Snode);
          IF verbessere_node(node,dist,&x_dist,&y_dist,&min_vor) THEN
            IF better_knoten(min_winkel,min_vor) THEN
              verbessert = TRUE;
              min_winkel = min_vor;
              best_node = node;
              best_x    = x_dist;
              best_y    = y_dist;
            ENDIF;
          ENDIF;     
        end_for_slist(list,list_element);
       IF verbessert THEN
          IF get_richtungs_mode() == LOCAL_MODE_BEST_NODE THEN
            move_node(best_x,best_y,best_node);
          ENDIF;
       ENDIF;

      ENDDO;
      dist = dist DIV 2 ;
    ENDDO;
        for_all_nodes(Work_sgraph,node)
          node->x = NODE_OLD_X(node);
          node->y = NODE_OLD_Y(node);
        end_for_all_nodes(Work_sgraph,node);

  ENDPROC


       
