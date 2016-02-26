/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : pqtreehi.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "listen.h"
#include "modula.h"
#include "planar.h"
#include "pqtree.h"
#include "pqtreehi.h"
#include "listen1.h"
#include "adj.h"
#include "dfs.h"
#include "interfac.h"
#include "edgelist.h"



/*****************************************************************/
/*                                                               */
/*  FUNCTION : Reduce                                            */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    BOOLEAN Reduce(Set_of_edges  ) 
         ADRlist Set_of_edges ;

    BEGIN /* Reduce */ 
        BOOLEAN Error = FALSE; 
        BOOLEAN Root,Success ; 
        PQNode AktNode, Father ;
        INTEGER Anz_of_edges ;
        Sedge edge;

      Anz_of_edges = lengthlist(Set_of_edges );
      Root = FALSE;
      CreateQueue;
      for_all_elements(Set_of_edges,(char*)edge);
        PutOnQueue(EDGE_PQNODE(edge));
      end_for_all_elements; 
      WHILE ( NOT EmptyQueue()) AND ( NOT (Error)) DO
        GetFromQueue(&AktNode);
        switch(PQ_NODE_TYP(AktNode))
        BEGIN
          case Leaf :
            PQ_NODE_LAB(AktNode) = full;
            break;
          case FNode :
            PQ_NODE_LAB(AktNode) = PQ_NODE_LAB(PQ_NODE_FS(AktNode));
            break;
          case PNode :
            IF PQ_NODE_PLC(AktNode) == Anz_of_edges THEN
              Root = TRUE;
              Error = Error OR ( NOT EmptyQueue());
            END; 
            MatchPTemplates(&AktNode, Root, &Error);
            break;
          case QNode :
            IF PQ_NODE_PLC(AktNode) == Anz_of_edges THEN
              Root = TRUE; 
              Error = Error OR ( NOT EmptyQueue());
            END; 
            MatchQTemplates(&AktNode, Root, &Error, &Success);
            break;
        END; 
        IF ( NOT (Error)) AND ( NOT Root) THEN
          Father = PQ_NODE_FA(AktNode);
          IF Father != NIL THEN
            DEC(PQ_NODE_PCC(Father), 1);
            IF
            PQ_NODE_PCC(Father) == 0 THEN
              PutOnQueue(Father)
            END;
          END
        END
        /* While */
      END
      return( NOT Error);
    END  /* Reduce */



/*****************************************************************/
/*                                                               */
/*  FUNCTION : Put_Sons_from_PNode_to_list                       */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void Put_Sons_from_PNode_to_list(Root,node)
        PQNode         Root;
        Snode        node;
BEGIN
    PQNode Run;
    DirectionType Direc;

    IF PQ_NODE_TYP(Root) == Leaf THEN
       PUSH_EDGELIST(NODE_ADJ_LIST(node),node,PQ_NODE_EDGE(Root) );
     ELSE
      Run = PQ_NODE_FS(Root);
      Direc = DetermineDirec(Run);
      WHILE (Run != NIL) DO
        IF PQ_NODE_TYP(Run) == Leaf THEN
          PUSH_EDGELIST(NODE_ADJ_LIST(node),node,PQ_NODE_EDGE(Run) );
         ELSE
          Put_Sons_from_PNode_to_list(Run,node);
        END;
        Run = next_Son(Direc,Run);
      END;
    END;
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION : Put_full_Sons_from_QNode_to_list                  */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void Put_full_Sons_from_QNode_to_list(Root,node)
        PQNode         Root;
        Snode        node;
BEGIN
    PQNode Run;
    DirectionType Direc;

    
    Run = PQ_NODE_FS(Root);
    Direc = DetermineDirec(Run);
    WHILE (Run != NIL) DO
        IF PQ_NODE_LAB(Run) == full THEN
          IF PQ_NODE_TYP(PQ_NODE_FS(Run)) == Leaf THEN
            PUSH_EDGELIST(NODE_ADJ_LIST(node),node,PQ_NODE_EDGE(PQ_NODE_FS(Run)) );
           ELSE
            Put_Sons_from_PNode_to_list(PQ_NODE_FS(Run),node);
          END;
        END;
      Run = next_Son(Direc,Run);
    END;

END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :  replace_full_sons_off_Qnode                      */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void replace_full_sons_off_Qnode(Root,T2,SourceNode)
        PQNode         Root;
        PQNode         T2;
        PQNode        SourceNode;
  BEGIN
    PQNode Run;
    DirectionType Direc;

    Put_full_Sons_from_QNode_to_list(Root,SourceNode);
    NODE_ADJ_DIRECTION(SourceNode) = PQ_NODE_DIRECT(Root);
    NODE_ADJ_DIRECT_NODE(SourceNode) = PQ_NODE_DIRECT_NODE(Root);

    IF T2 != NIL THEN 

      PQ_NODE_DIRECT(Root) = TRUE;
      PQ_NODE_DIRECT_NODE(Root) = SourceNode;

      Run = PQ_NODE_FS(Root);
      Direc = DetermineDirec(Run);
      WHILE PQ_NODE_LAB(Run) != full DO
        Run = next_Son(Direc,Run)
      END;
      replace_node(PQ_NODE_FS(Run),T2);

      Run = next_Son(Direc,Run);

      WHILE (Run != NIL) DO
        IF PQ_NODE_LAB(Run) == full THEN
          delete_PQ_node(Root,Run);
        END;
        Run = next_Son(Direc,Run);
      END; 
    END;
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :  replace_sons_off_PNode                           */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


void replace_sons_off_PNode(Rnode,T2,node)
  PQNode         Rnode,T2;
  Snode                node;
  BEGIN
    Put_Sons_from_PNode_to_list(Rnode,node);
    IF T2 != NIL THEN 
      replace_node(Rnode,T2); 
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

BOOLEAN Qnode_entartet(Root)
  PQNode  Root;
  BEGIN
    RETURN(FALSE);
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

void convert_Qnode_to_Pnode(Root)
  PQNode  Root;
  BEGIN
  END


