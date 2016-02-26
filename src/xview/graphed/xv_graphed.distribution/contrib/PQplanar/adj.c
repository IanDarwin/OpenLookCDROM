/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : adj.c                                          */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
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
/*                                                               */
/*                                                               */
/*****************************************************************/

#include "path.h"
#include STDI
#include SGRAPHI
#include "modula.h"
#include "listen1.h"
#include "adj.h"
#include "sgraphhi.h"
#include "interfac.h"


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


void reverse_adj(node)
        Snode        node;
  BEGIN
    NODE_ADJ_DIRECTION(node) = NOT(NODE_ADJ_DIRECTION(node)) ;
    DREVERSE_LIST(NODE_ADJ_LIST(node)) ;
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION : void corect_adj_list(graph)                       */
/*                                                               */
/*   INPUT   : graph mit einer sortierten Adjazensliste          */
/*                                                               */
/*   OUTPUT  :  Es wird geprueft ob die Sortierung korreckt ist, */
/*              und                                              */
/*                                                               */
/*****************************************************************/


void corect_adj_list(graph)
  Sgraph graph;
  BEGIN
    INTEGER j,n;
    Snode   Hilf;

    n = number_off_nodes(graph);
    FOR_TO(j,1,n)
      Hilf = find_Snode_with_number(graph,j);
      IF NOT NODE_ADJ_DIRECTION(Hilf) THEN
#if TEST
        printf("Korrecktur der Adjazensliste bei : ");
        print_node(Hilf);
        printf(" \n ");
#endif
        IF NODE_ADJ_DIRECT_NODE(Hilf) != NIL THEN
          reverse_adj(NODE_ADJ_DIRECT_NODE(Hilf));
        END;
      END ;
    END
  END
 
