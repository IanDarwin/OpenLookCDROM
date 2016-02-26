/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : planar.c                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*                                                               */
/* Die function 'planar(graph)' setzt voraus das der Graph       */
/* st-nummeriert ist .                                           */
/* Die function 'planar(graph)' liefert einen boolschen Wer      */
/* zurueck und wenn der Graph planar ist,so enthaelt jeder       */
/* Knoten eine geordnete Listen von Knoten mit hoeherer Nummer.  */
/*                                                               */
/* Diese Liste haengt an 'node->attrs.data' und                  */
/* die Strucktur ist in  'adj.c' beschrieben .                   */
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

#include "modula.h"
#include "listen.h"
#include "planar.h"
#include "pqtree.h"
#include "pqtreehi.h"
#include "sgraphhi.h"
#include "listen1.h"
#include "adj.h"
#include "interfac.h"
#include "stnumber.h"


/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef struct 
        {      Sedge edge; 
               PQTYPE type;
        } *Wr_node_attributes;

       
PQTree WorkTree;
PQNode Root_T_S;


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern PQNode  Build_Tree_with_Set();
extern void    Bubble();
extern int     Reduce();
extern void    dump_pq_tree();
extern PQNode  Root();
extern void    replace_with();
extern void    replace_full_sons_off_Qnode();


/*****************************************************************/
/*                                                                 */
/*  FUNCTION : init_WorkTree                                     */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

void init_WorkTree()
  BEGIN

#if TEST
    printf("begin init_WorkTree \n");
#endif
    
    WorkTree = make_graph( make_attr(ATTR_DATA,NIL ));

    set_graphlabel(WorkTree,"Work-Tree");

    WorkTree->directed = TRUE ;
#if TEST
    printf("end init_WorkTree \n");
#endif
  END

/*****************************************************************/
/*                                                                 */
/*  FUNCTION : Build_Set_with_lower_nr                             */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

edge_Set Build_Set_with_lower_nr(graph,x)
  Sgraph graph;
  int x;
  BEGIN
    Snode node;
    Sedge edge;
    edge_Set lower_Set;

    lower_Set = createlist();
    node = find_Snode_with_number(graph,x);

    for_sourcelist (node,edge);
      IF ST_NUMBER(edge->tnode) > x THEN
        lower_Set = pushlist(lower_Set,(char*)edge);
      END;
    end_for_sourcelist (node,edge);

    RETURN(lower_Set);
  END

/*****************************************************************/
/*                                                                 */
/*  FUNCTION : Build_Set_with_higher_nr                              */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

edge_Set Build_Set_with_higher_nr(graph,x)
  Sgraph graph;
  int x;
  BEGIN
    Snode node;
    Sedge edge;
    edge_Set higher_Set;

    higher_Set = createlist();
    node = find_Snode_with_number(graph,x);

    for_sourcelist (node,edge);
      IF ST_NUMBER(edge->tnode) < x THEN
        higher_Set = pushlist(higher_Set,(char*)edge);
      END;
    end_for_sourcelist (node,edge);

    RETURN(higher_Set);
  END


/*****************************************************************/
/*                                                                 */
/*  FUNCTION :  planar_test                                             */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

int pqtree_planar_test(graph)
  Sgraph graph;
  BEGIN
    PQNode         T,T2;
    edge_Set        U,S,S2;
    int         j,n;
    Snode        Hilf;

#if TEST
    printf("begin pqtree_planar_test \n");
#endif
   n = number_off_nodes(graph);
    U = Build_Set_with_higher_nr(graph,n);
    T = Build_Tree_with_Set(U);
    FOR_DOWNTO(j , n-1 , 1 )
#if TEST
      dump_pq_tree(WorkTree);
#endif
      S = Build_Set_with_lower_nr(graph,j);
      Bubble(T,S);
      IF NOT Reduce(S) THEN
#if TEST
        dump_pq_tree(WorkTree);
#endif
        RETURN( FALSE );
       ELSE
#if TEST
        dump_pq_tree(WorkTree);
#endif
        Hilf = find_Snode_with_number(graph,j);
        DCLEAR_LIST(NODE_ADJ_LIST(Hilf));
        NODE_ADJ_DIRECTION(Hilf) = TRUE;
        NODE_ADJ_DIRECT_NODE(Hilf) = NIL;

        Root_T_S = Root(WorkTree->nodes,S);
        S2 = Build_Set_with_higher_nr(graph,j);
        T2 = Build_Tree_with_Set(S2);
        IF PQ_NODE_TYP(Root_T_S) == QNode THEN
          replace_full_sons_off_Qnode(Root_T_S,T2,Hilf);
          IF Qnode_entartet(Root_T_S) THEN
            convert_Qnode_to_Pnode(Root_T_S);
          END
         ELSE
          replace_sons_off_PNode(Root_T_S,T2,Hilf);
        END;
        
      END;
    END;


    Hilf = find_Snode_with_number(graph,n);
    DCLEAR_LIST(NODE_ADJ_LIST(Hilf));
    NODE_ADJ_DIRECTION(Hilf) = TRUE;
    NODE_ADJ_DIRECT_NODE(Hilf) = NIL;

#if TEST
    printf("end pqtree_planar_test \n");
#endif
    RETURN( TRUE );
  END

/*****************************************************************/
/*                                                                 */
/*  FUNCTION :  planar                                             */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

BOOLEAN planar(graph)
  Sgraph graph;
  BEGIN
#if TEST
   printf("begin planar \n");
#endif
    IF number_off_edges_undirected(graph) > 3*number_off_nodes(graph) - 6 THEN
      RETURN(FALSE);
     ELSE
      init_PQTree();
      init_WorkTree();
      IF pqtree_planar_test(graph) THEN
        corect_adj_list(graph);
#if TEST
        printf("end planar \n");
#endif
        RETURN( TRUE );
       ELSE
#if TEST
       printf("end planar \n");
#endif
        RETURN( FALSE );
      END;
    END;
  END
