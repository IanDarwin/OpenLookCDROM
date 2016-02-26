/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : neck.c                                         */
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
#include "sgraphhi.h"
#include "simplex.h"
#include "neck.h"
#include "necklist.h"
#include "plan_sf.h"
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

BOOLEAN node_full(node)
        Snode node;
  BEGIN
    RETURN( NODE_ZAEHLER(node) == NODE_GRAD(node) );
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

void print_Neck(Neck)
     NECK Neck;
  BEGIN
#if TEST
     NECK Neck_element;

    printf("NECK: ");
    for_all_neck_elements(Neck,Neck_element)
      print_node(EDGELIST_NODE(NECK_ECKE(Neck_element)));
      print_node(EDGELIST_NODE(EDGELIST_CORES(NECK_ECKE(Neck_element))));      
    end_for_all_neck_elements(Neck);
    printf(" \n");
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

BOOLEAN finde_Neck(anfang,Neck)
     EDGELIST  anfang;
     NECK *Neck;
  BEGIN
    EDGELIST work;
    Snode    node;

    work = anfang;
    REPEAT
      IF NOT(NODE_FULL(EDGELIST_NODE(work))) THEN 
        node = EDGELIST_NODE(work);
        print_node(node);
        DQUEUE((*Neck),work);
        work = EDGELIST_CORES(work)->pre;
       ELSE
        RETURN(FALSE);
      END;
    UNTIL(work == anfang);
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

void setze_Zaehler_null(graph)
        Sgraph        graph;
  BEGIN
    Snode    node;
    EDGELIST ecke;

    for_all_nodes(graph,node);
      NODE_ZAEHLER(node) = 0;
      for_edgelist(node,ecke)
        EDGELIST_FULL(ecke) = FALSE;       
      end_for_edgelist(node)
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

void erhoehe_Zaehler(Neck)
     NECK Neck;
  BEGIN
    NECK   ecke;

    for_all_neck_elements(Neck,ecke)
      EDGELIST_FULL(NECK_ECKE(ecke)) = TRUE;
      NODE_ZAEHLER(EDGELIST_NODE(NECK_ECKE(ecke))) ++;
    end_for_all_neck_elements(Neck);
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

INTEGER anzahl_aussenknoten(Neck)
        NECK Neck;
  BEGIN
        NECK Neck_element;
        INTEGER anzahl_der_aussenknoten = 0 ;

    for_all_neck_elements(Neck,Neck_element)
      IF NODE_AUSSEN(EDGELIST_NODE(NECK_ECKE(Neck_element))) THEN
        INC(anzahl_der_aussenknoten,1);
      ENDIF;
    end_for_all_neck_elements(Neck);
    RETURN(anzahl_der_aussenknoten);
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

BOOLEAN less_groesstes_Neck(list1,list2)
        NECK list1,list2;
  BEGIN
      IF anzahl_aussenknoten(list1) < anzahl_aussenknoten(list2) THEN 
        RETURN( TRUE );
       ELSE
        IF anzahl_aussenknoten(list1) > anzahl_aussenknoten(list2) THEN 
          RETURN (FALSE);
         ELSE
          RETURN(dlengthlist(list1) < dlengthlist(list2));
        ENDIF;
      ENDIF;
  END


/*****************************************************************/

BOOLEAN less_kleinstes_Neck(list1,list2)
        NECK list1,list2;
  BEGIN
      IF anzahl_aussenknoten(list1) < anzahl_aussenknoten(list2) THEN 
        RETURN( TRUE );
       ELSE
        IF anzahl_aussenknoten(list1) > anzahl_aussenknoten(list2) THEN 
          RETURN (FALSE);
         ELSE
          RETURN(dlengthlist(list1) > dlengthlist(list2));
        ENDIF;
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

REAL groesse_Neck(Neck)
        NECK Neck;
  BEGIN
        INTEGER xmax = 0,xmin = 10000,ymax = 0,ymin = 10000;
        INTEGER x,y;
        NECK Neck_element;

    for_all_neck_elements(Neck,(char*)Neck_element)
      x = NODE_OLD_X(EDGELIST_NODE(NECK_EDGELIST(Neck_element)));
      y = NODE_OLD_Y(EDGELIST_NODE(NECK_EDGELIST(Neck_element)));
      IF xmax < x THEN
        xmax = x;
      ENDIF;
      IF ymax < y THEN
        ymax = y;
      ENDIF;
      IF xmin > x THEN
        xmin = x;
      ENDIF;
      IF ymin > y THEN
        ymin = y;
      ENDIF;
    end_for_all_neck_elements(Neck);
    RETURN( (REAL)(xmax-xmin)*(ymax-ymin) );
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

BOOLEAN less_groesste_Ausdehnung(list1,list2)
        NECK list1,list2;
  BEGIN
      IF anzahl_aussenknoten(list1) < anzahl_aussenknoten(list2) THEN 
        RETURN( TRUE );
       ELSE
        IF anzahl_aussenknoten(list1) > anzahl_aussenknoten(list2) THEN 
          RETURN (FALSE);
         ELSE
          RETURN(groesse_Neck(list1) > groesse_Neck(list2));
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


void  verbinde_Neck_Edgelist(Neck)
        NECK Neck;
  BEGIN
        NECK Neck_element;

    for_all_neck_elements(Neck,Neck_element)
       EDGELIST_NECK(NECK_EDGELIST(Neck_element)) = Neck_element;
    end_for_all_neck_elements(Neck);
      
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

void suche_Neck_im_graphen(graph,Neck_list)
        Sgraph        graph;
        NECK_LIST        *Neck_list;
  BEGIN
    Snode node;
    EDGELIST         work;
    NECK        Neck;
    BOOLEAN     (*less)();

    IF get_ausseneck_mode() == MODE_MAXIMALES_NECK THEN
      less = less_groesstes_Neck;
     ELSE
      IF get_ausseneck_mode() == MODE_MINIMALES_NECK THEN       
        less = less_kleinstes_Neck;
       ELSE
        less = less_groesste_Ausdehnung;
      ENDIF;
    ENDIF;

#if TEST
    printf("Suche N-Eck eim Graphen \n");
#endif

    setze_Zaehler_null(graph);

    for_all_nodes(graph,node)  
      IF NOT(NODE_FULL(node)) THEN 
        work = NODE_EDGELIST(node);
        REPEAT
          IF NOT(EDGELIST_FULL(work))  THEN
             NECK_INIT(Neck);
             printf("\n");
             IF finde_Neck(work,&Neck) THEN
              erhoehe_Zaehler(Neck);
              INSERT_SORT(*Neck_list,Neck,less);
              verbinde_Neck_Edgelist(Neck);
            END;
          END;
          work = work->pre;
        UNTIL( ( work == NODE_EDGELIST(node) )  OR
                NODE_FULL(node)  );
        printf("\n");
      END;
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

INTEGER grad(Neck)
        NECK Neck;
  BEGIN
    RETURN(dlengthlist(Neck));
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

void Neck_push(Neck,edgelist)
        NECK *Neck;
        EDGELIST edgelist;
  BEGIN
    DPUSH((*Neck),edgelist);
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

void Neck_init(Neck)
        NECK *Neck;
  BEGIN
    NECK_INIT((*Neck));
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

EDGELIST Neck_nth_element(Neck,N)
        NECK    Neck;
        INTEGER N;
  BEGIN
    RETURN((EDGELIST)dnth_element(Neck,N));
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

void Neck_set_nth_element(Neck,N,edgelist)
        NECK    Neck;
        INTEGER N;
        EDGELIST edgelist;
  BEGIN
    dset_nth_element(Neck,N,(char*)edgelist);   
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

void Neck_delete_nth_element(Neck,N)
        NECK    Neck;
        INTEGER N;
  BEGIN
    delete_nth_element(Neck,N);   
  END;


