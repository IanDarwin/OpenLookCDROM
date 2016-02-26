/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : pqtree.c                                       */
/*        AUTOR : Uwe Schnieders 	                         */
/*        UPDATE: 23.07.90                                         */
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
#include "interfac.h"

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern PQTree WorkTree;

/*****************************************************************/
/* Varialble fuer die queue */
      BOOLEAN Error ;

      QueuePtr HeadQueue, TailQueue ; 

      INTEGER KnotenNumber = 0 ;
      INTEGER ElementsOnQueue ;

/*****************************************************************/
/* Modul-Global */
static        PQNode        Root_T_S;
static  Attributes  attributes_NIL;
static       DirectionType Direction;




/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void init_PQTree()
  BEGIN
    attributes_NIL.data = NIL ;
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

PQNode make_PQTree_node(graph)
        PQTree         graph;
  BEGIN
    return(make_node(graph,attributes_NIL));
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

   DirectionType DetermineDirec( Node )
    PQNode Node;
    BEGIN
      DirectionType DetermineDirecResult ;

      IF PQ_NODE_LN(Node) != NIL THEN
        DetermineDirecResult = left;
      ELSE
        DetermineDirecResult = right;
      END;
      return( DetermineDirecResult);
    END ;

    PQNode next_Son(Direc,Run)
      DirectionType Direc;
      PQNode Run;
      BEGIN
        IF Direc == left THEN 
          return( PQ_NODE_LN(Run))
         ELSE 
          return( PQ_NODE_RN(Run))
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

    void DefineNode(AktNode , Typ , Father, LeftNeighbour,
                    RightNeighbour, FirstChild, SecondChild ,
                    Edge )

                    PQNode    AktNode ;
                    NodeType  Typ ;
                    PQNode    Father, LeftNeighbour,
                              RightNeighbour, FirstChild,
                              SecondChild ;
                    Sedge     Edge ;
    BEGIN
      PQNodePtr help;

      INC(KnotenNumber, 1);
      AktNode->nr = KnotenNumber;
      NEW(help);
      set_nodeattrs(AktNode,make_attr(ATTR_DATA,help));
      PQ_NODE_TYP(AktNode) = Typ;
      PQ_NODE_FA(AktNode)  = Father;
      PQ_NODE_LN(AktNode)  = LeftNeighbour;
      PQ_NODE_RN(AktNode)  = RightNeighbour;
      PQ_NODE_FS(AktNode)  = FirstChild;
      PQ_NODE_SS(AktNode)  = SecondChild;
      PQ_NODE_EDGE(AktNode)= Edge;
      PQ_NODE_PCC(AktNode) = 0;
      PQ_NODE_PLC(AktNode) = 0;
      PQ_NODE_MARK(AktNode) = unmarked;
      PQ_NODE_LAB(AktNode) = empty;

      PQ_NODE_DIRECT(AktNode) = TRUE;
      PQ_NODE_DIRECT_NODE(AktNode) = (Snode)NIL;
    END ;


    void Concat( Node, List )
    PQNode  *Node, *List ;
    BEGIN
      PQ_NODE_RN(*Node) = (*List);
      PQ_NODE_LN(*Node) = NIL;
      IF (*List) != NIL THEN
        PQ_NODE_LN(*List) = (*Node);
      END; 
      (*List) = (*Node);
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

    void FindPartialFNodes( Node, P1, P2 ,
                            EmptyCount, FullCount,
                            PartialCount, ChildCount ) 
     PQNode *Node, *P1, *P2 ;
     INTEGER *EmptyCount, *FullCount,
             *PartialCount, *ChildCount; 

    BEGIN 
      PQNode  Akt;

      Akt = PQ_NODE_FS(*Node);
      IF PQ_NODE_RN(Akt) == NIL THEN
        Akt = PQ_NODE_SS(*Node);
      END; 
      (*EmptyCount) = 0;
      (*PartialCount) = 0;
      (*FullCount) = 0;
      (*ChildCount) = 0;
      (*P1) = NIL;
      (*P2) = NIL;
      WHILE Akt != NIL DO 
        INC((*ChildCount), 1);
        switch( PQ_NODE_LAB(Akt) )
        BEGIN
          case empty : INC((*EmptyCount), 1);
                       break;
          case full  : INC((*FullCount), 1);
                       break;
          case partial: INC((*PartialCount), 1);
                        IF (*PartialCount) == 1 THEN
                          (*P1) = Akt;
                         ELSE 
                          (*P2) = Akt;
                        END;
                        break;
        END; 
        Akt = PQ_NODE_RN(Akt);
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

    void Reverse( List )
    PQNode List ;
    BEGIN
        PQNode Next, Last ;
        DirectionType Direction ; 

      Last = NIL; 
      Direction = DetermineDirec((List));
      WHILE (List) != NIL DO
        IF Direction == left THEN 
          Next = PQ_NODE_LN(List);
          PQ_NODE_LN(List) = Last;
          PQ_NODE_RN(List) = NIL;
          IF Last != NIL THEN 
            PQ_NODE_RN(Last) = (List);
          END
        ELSE 
          Next = PQ_NODE_RN(List);
          PQ_NODE_RN(List) = Last;
          PQ_NODE_LN(List) = NIL;
          IF Last != NIL THEN 
            PQ_NODE_LN(Last) = (List);
          END;
        END; 
        Last = (List);
        (List) = Next;
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

    void DeletePQTree( AktNode )
       PQNode AktNode;
    BEGIN 
        PQNode Run ;
        DirectionType Direc ; 

      IF AktNode != NIL THEN 
        switch( PQ_NODE_TYP(AktNode) )
        BEGIN
          case QNode :
          case PNode :
            Run = PQ_NODE_FS(AktNode);
            Direc = DetermineDirec(Run);
            WHILE (Run != NIL) DO
              DeletePQTree(Run);
              IF Direc == left THEN 
                Run = PQ_NODE_LN(Run);
              ELSE 
                Run = PQ_NODE_RN(Run);
              END; 
            END; 
            break;
          case FNode :
            DeletePQTree( PQ_NODE_FS(AktNode) );
            break;
        END; /* Case */ 
        remove_node(AktNode);
      END; /* If AktNode != Nil */ 
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

    void CreateQueue()
    BEGIN
      HeadQueue = NIL; 
      TailQueue = NIL; 
      ElementsOnQueue = 0; 
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

    BOOLEAN EmptyQueue() 
    BEGIN 
      BOOLEAN EmptyQueueResult ;

      EmptyQueueResult = (HeadQueue == NIL);
      return( EmptyQueueResult );
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

   void GetFromQueue(First )
    PQNode *First;
    BEGIN 
      QueuePtr Help ;
 
      IF HeadQueue != NIL THEN
        Help = HeadQueue; 
        (*First) = Help->Inhalt;
        IF Help == TailQueue THEN 
          TailQueue = NIL
        END; 
        HeadQueue = Help->Next;
        DISPOSE(Help);
        DEC(ElementsOnQueue, 1);
      ELSE 
        (*First) = NIL
      END 
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

    void PutOnQueue( Next)
    PQNode Next;
    BEGIN 
      QueuePtr Help ;
 
      NEW(Help);
      Help->Inhalt = Next;
      Help->Next = NIL;
      IF TailQueue != NIL THEN 
        TailQueue->Next = Help
      ELSE 
        HeadQueue = Help
      END; 
      TailQueue = Help; 
      INC(ElementsOnQueue, 1);
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

    PQNode Build_Tree_with_Set(Set ) 
           ADRlist Set ;

      BEGIN /*  */ 
          PQNode LeafList, Next,Root ;
          INTEGER NumberOfEdges  ;
          Sedge edge;

        NumberOfEdges = lengthlist(Set);
        IF NumberOfEdges > 0 THEN 
          LeafList = NIL; 
          Root = make_PQTree_node(WorkTree);
          for_all_elements(Set,(char*)edge);
            Next = make_PQTree_node(WorkTree);
            DefineNode(Next, Leaf, Root, (PQNode)NIL, (PQNode)NIL, 
                       (PQNode)NIL, (PQNode)NIL, edge);
            Concat(&Next, &LeafList);
            EDGE_PQNODE(edge) = Next;
          end_for_all_elements; 

          IF NumberOfEdges == 1 THEN 
            remove_node(Root);
            Root = LeafList;
            PQ_NODE_FA(Root) = NIL;
          ELSE 
            DefineNode(Root, PNode, (PQNode)NIL, (PQNode)NIL, (PQNode)NIL ,                                LeafList, (PQNode)NIL, (Sedge)NIL)
          END
        END
        return( Root )
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

   void ReplaceFNode( AktNode, FNode, FullSon, EmptySon ) 
      PQNode AktNode, FNode, FullSon, EmptySon ; 
      BEGIN 
        IF PQ_NODE_LN(FullSon) != NIL THEN
          Reverse(FullSon)
        END; 
        IF PQ_NODE_LN(FNode) == NIL THEN
          IF PQ_NODE_LAB(PQ_NODE_RN(FNode)) != empty THEN
            Reverse(PQ_NODE_FS(AktNode))
          END
        ELSE 
          IF PQ_NODE_LAB(PQ_NODE_LN(FNode)) == empty THEN
            Reverse(PQ_NODE_FS(AktNode))
          END; 
        END;
        PQ_NODE_LN(FullSon)   = PQ_NODE_LN(FNode);
        PQ_NODE_RN(EmptySon)  = PQ_NODE_RN(FNode);
        IF PQ_NODE_LN(FNode) != NIL THEN
          PQ_NODE_RN(PQ_NODE_LN(FNode)) = FullSon
        ELSE 
          IF PQ_NODE_FS(AktNode) == FNode THEN
            PQ_NODE_FS(AktNode) = FullSon
          ELSE 
            PQ_NODE_SS(AktNode) = FullSon
          END; 
          PQ_NODE_FA(FullSon) = AktNode;
        END; 
        IF PQ_NODE_RN(FNode) != NIL THEN
          PQ_NODE_LN(PQ_NODE_RN(FNode)) = EmptySon
        ELSE 
          IF  PQ_NODE_FS(AktNode) == FNode THEN
            PQ_NODE_FS(AktNode) = EmptySon
          ELSE 
            PQ_NODE_SS(AktNode) = EmptySon
          END; 
          PQ_NODE_FA(EmptySon) = AktNode;
        END; 
        remove_node( PQ_NODE_FS(FNode) );
        remove_node(FNode);
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

      void DetermineFullEmptySon(PF1, FullSon,EmptySon ) 
          PQNode *PF1, *FullSon,   *EmptySon ; 
      BEGIN 
        IF PQ_NODE_LAB(PQ_NODE_FS( (*PF1) )) == full THEN
          (*FullSon)  = PQ_NODE_FS((*PF1));
          (*EmptySon) = PQ_NODE_SS((*PF1));
        ELSE
          (*FullSon)  = PQ_NODE_SS((*PF1));
          (*EmptySon) = PQ_NODE_FS((*PF1));
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

      void TemplateQ2( AktNode, PF1 ) 
          PQNode AktNode, PF1 ;

      BEGIN 
        PQNode  FullSon, EmptySon ;

        DetermineFullEmptySon(&PQ_NODE_FS(PF1) , &FullSon, &EmptySon);
        ReplaceFNode(AktNode, PF1, FullSon, EmptySon);
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

      void TemplateQ3( AktNode, PartialField1,  PartialField2 ) 
           PQNode AktNode, PartialField1,  PartialField2 ; 

      BEGIN 
        PQNode  FullSon, EmptySon ;

        DetermineFullEmptySon(&PQ_NODE_FS(PartialField1) , &FullSon,
                              &EmptySon);
        ReplaceFNode(AktNode, PartialField1, FullSon, EmptySon);
        DetermineFullEmptySon(&PQ_NODE_FS(PartialField2) , &FullSon,
                              &EmptySon);
        ReplaceFNode(AktNode, PartialField2, FullSon, EmptySon);
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

      void NextElement(Son)
      PQNode *Son ;
      BEGIN 
        IF Direction == left THEN 
          (*Son) = PQ_NODE_LN((*Son))
        ELSE 
          (*Son) = PQ_NODE_RN((*Son))
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

      BOOLEAN SkipRest(Son ,
                       Labelakt ) 
           PQNode *Son ;
           LabType   Labelakt ; 

        BEGIN 
          BOOLEAN  Ende ; 
          BOOLEAN  SkipRestResult ;
 
          Ende = FALSE; 
          WHILE ((*Son) != NIL) AND ( NOT Ende) DO
            IF PQ_NODE_LAB((*Son)) != Labelakt THEN
              Ende = TRUE
            ELSE 
              NextElement(Son)
            END
          END; 
          SkipRestResult = ((*Son) == NIL);
          return( SkipRestResult);
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

    void SkipToOtherLabel(Son ,
                          Labelakt ) 
              PQNode *Son ;
              LabType   Labelakt ; 

        BEGIN 
          BOOLEAN  Ende ; 

          Ende = FALSE; 
          WHILE ((*Son) != NIL) AND ( NOT Ende) DO
            IF PQ_NODE_LAB((*Son)) != Labelakt THEN
              Ende = TRUE
            ELSE
              NextElement(Son)
            END
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

    BOOLEAN TstConsNonRoot0P( Son )
        PQNode Son ;
        BEGIN 
          BOOLEAN TstConsNonRoot0PResult ;

          IF PQ_NODE_LAB(Son) == empty THEN
            SkipToOtherLabel(&Son, empty);
            TstConsNonRoot0PResult = SkipRest(&Son, full)
          ELSE 
            SkipToOtherLabel(&Son, full);
            TstConsNonRoot0PResult = SkipRest(&Son, empty)
          END; 
          return( TstConsNonRoot0PResult);
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

     BOOLEAN TstConsNonRoot1P(Son )
        PQNode Son ;
        BEGIN 
          BOOLEAN  Consec ; 
          BOOLEAN  TstConsNonRoot1PResult ; 

          Consec = TRUE; 
          switch( PQ_NODE_LAB(Son) )
          BEGIN
            case empty :
              SkipToOtherLabel(&Son, empty);
              Consec = (PQ_NODE_LAB(Son) == partial);
              NextElement(&Son);
              Consec = Consec AND SkipRest(&Son, full);
              break;
            case full :
              SkipToOtherLabel(&Son, full);
              Consec = (PQ_NODE_LAB(Son) == partial);
              NextElement(&Son);
              Consec = Consec AND SkipRest(&Son, empty);
              break;
            case partial :
              NextElement(&Son);
              Consec = SkipRest(&Son, PQ_NODE_LAB(Son) );
              break;
          END; 
          TstConsNonRoot1PResult = Consec; 
          return( TstConsNonRoot1PResult);
        END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    BOOLEAN TstConsRoot0P(Son )
        PQNode Son ;
        BEGIN 
          BOOLEAN  Consec ; 
          BOOLEAN  TstConsRoot0PResult ; 

          Consec = TRUE; 
          SkipToOtherLabel(&Son, empty);
          IF Son != NIL THEN 
            SkipToOtherLabel(&Son, full);
            IF Son != NIL THEN 
              Consec = Consec AND SkipRest(&Son, empty)
            END;
          END; 
          TstConsRoot0PResult = Consec; 
          return( TstConsRoot0PResult);
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

    BOOLEAN TstConsRoot1P(Son )
        PQNode Son ;
        BEGIN 
          BOOLEAN Consec ; 
          BOOLEAN TstConsRoot1PResult ;

          Consec = TRUE; 
          SkipToOtherLabel(&Son, empty);
          IF PQ_NODE_LAB(Son) == full THEN
            SkipToOtherLabel(&Son, full);
            Consec = Consec AND (PQ_NODE_LAB(Son) == partial);
            NextElement(&Son);
            Consec = Consec AND SkipRest(&Son, empty);
          ELSE 
            NextElement(&Son);
            SkipToOtherLabel(&Son, full);
            IF Son != NIL THEN 
              Consec = Consec AND SkipRest(&Son, empty)
            END; 
          END; 
          TstConsRoot1PResult = Consec; 
          return( TstConsRoot1PResult);
        END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

     BOOLEAN TstConsRoot2P( Son )
        PQNode Son ;
        BEGIN 
          BOOLEAN  Consec ; 
          BOOLEAN  TstConsRoot2PResult ; 

          SkipToOtherLabel(&Son, empty);
          Consec = (PQ_NODE_LAB(Son) == partial);
          NextElement(&Son);
          SkipToOtherLabel(&Son, full);
          Consec = Consec AND (PQ_NODE_LAB(Son) == partial);
          NextElement(&Son);
          Consec = Consec AND SkipRest(&Son, empty);
          TstConsRoot2PResult = Consec; 
          return( TstConsRoot2PResult);
        END ;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      BOOLEAN TstConsecutive( AktNode ,
                              Root , 
                              PartialCount )
           PQNode AktNode ;
           BOOLEAN   Root ; 
           INTEGER   PartialCount ;
      BEGIN /* TstConsecutive */ 
        BOOLEAN TstConsecutiveResult ;

        Direction = DetermineDirec(PQ_NODE_FS(AktNode));
        IF Root THEN 
          switch(PartialCount)
          BEGIN
            case 0 :
              TstConsecutiveResult = TstConsRoot0P(PQ_NODE_FS(AktNode));
              break;
            case 1 :
              TstConsecutiveResult = TstConsRoot1P(PQ_NODE_FS(AktNode));
              break;
            case 2 :
              TstConsecutiveResult = TstConsRoot2P(PQ_NODE_FS(AktNode));
              break;
            default :
              TstConsecutiveResult = FALSE
          END
        ELSE 
          switch(PartialCount)
          BEGIN
            case 0 :
              TstConsecutiveResult = TstConsNonRoot0P(PQ_NODE_FS(AktNode));
              break;
            case 1 :
              TstConsecutiveResult = TstConsNonRoot1P(PQ_NODE_FS(AktNode));
              break;
            default:

              TstConsecutiveResult = FALSE
          END
        END;
        return( TstConsecutiveResult);
      END ; /* TstConsecutive */ 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    void MatchQTemplates(AktNode ,
                         Root , 
                         Error, Success )
         PQNode *AktNode ;
         BOOLEAN    Root ; 
         BOOLEAN    *Error, *Success ;
    BEGIN 
      PQNode PF1, PF2 ;
      INTEGER EmptyCount, FullCount, PartialCount, ChildCount ; 

      (*Error) = FALSE;
      FindPartialFNodes(AktNode, &PF1, &PF2, &EmptyCount, &FullCount,
                        &PartialCount, &ChildCount); 
      IF  NOT TstConsecutive(*AktNode, Root, PartialCount) THEN
        (*Error) = TRUE;
        (*Success) = FALSE;
      ELSE 
        switch(PartialCount)
        BEGIN
          case 0 : 
            IF EmptyCount == ChildCount THEN 
              PQ_NODE_LAB(*AktNode) = empty
            ELSE 
              IF FullCount == ChildCount THEN
                PQ_NODE_LAB(*AktNode) = full
               ELSE 
                 PQ_NODE_LAB(*AktNode) = partial
               END;
            END;
            break;
          case 1 :
            TemplateQ2(*AktNode, PF1);
            PQ_NODE_LAB(*AktNode) = partial;
            break;
          case 2 :
            TemplateQ3(*AktNode, PF1, PF2);
            PQ_NODE_LAB(*AktNode) = partial;
            break;
        END;
        (*Success) = TRUE;
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

      void TemplateP0( AktNode )
      PQNode AktNode ;
      BEGIN
        PQ_NODE_LAB(AktNode) = empty;
      END  


      void TemplateP1(AktNode )
      PQNode AktNode ;
      BEGIN
        PQ_NODE_LAB(AktNode) = full;
      END 


      void Append(Node, List )
      PQNode *Node,*List;
      BEGIN
        IF (*List) == NIL THEN
          (*List) = (*Node)
        ELSE
          IF PQ_NODE_LN((*List)) == NIL THEN
            PQ_NODE_LN((*List)) = (*Node);
            PQ_NODE_RN((*Node)) = (*List);
            PQ_NODE_LN((*Node)) = NIL;
          ELSE 
            PQ_NODE_RN((*List)) = (*Node);
            PQ_NODE_LN((*Node)) = (*List);
            PQ_NODE_RN((*Node)) = NIL;
          END;
          PQ_NODE_FA((*Node)) = PQ_NODE_FA((*List));
          PQ_NODE_FA((*List)) = NIL;
          (*List) = (*Node);
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

      void FindSignedSons(AktNode, EmptyList,FullList,PartialList , 
                          EmptyCount, FullCount,PartialCount , 
                          NodeLabel ) 
           PQNode *AktNode, *EmptyList,
                 *FullList,*PartialList ; 
           INTEGER   *EmptyCount, *FullCount,
                     *PartialCount ; 
           LabType   *NodeLabel ; 
      BEGIN 
        PQNode  Akt, Next ;
        INTEGER  NumberOfSons ; 
        DirectionType  Direction ; 

        (*EmptyList) = NIL;
        (*FullList) = NIL;
        (*PartialList) = NIL;
        (*EmptyCount) = 0;
        (*FullCount) = 0;
        (*PartialCount) = 0;
        Akt = PQ_NODE_FS((*AktNode));
        NumberOfSons = 0; 
        Direction = DetermineDirec(Akt);
        WHILE (Akt != NIL) DO
          INC(NumberOfSons, 1);
          IF Direction == left THEN 
            Next = PQ_NODE_LN(Akt)
          ELSE 
            Next = PQ_NODE_RN(Akt)
          END; 
          switch(PQ_NODE_LAB(Akt))
          BEGIN
            case empty :
              Concat(&Akt, EmptyList);
              INC((*EmptyCount), 1);
              break;
            case full :
              Concat(&Akt, FullList);
              INC((*FullCount), 1);
              break;
            case partial  :
              Concat(&Akt, PartialList);
              INC((*PartialCount), 1);
              break;
          END; /* Case */ 
          Akt = Next; 
        END; /* While */ 
        IF (*EmptyCount) == NumberOfSons THEN
          (*NodeLabel) = empty
        ELSE
          IF  (*FullCount) == NumberOfSons THEN
            (*NodeLabel) = full
          ELSE 
          (*NodeLabel) = partial
          END 
        END
      END  /* FindSignedSons */ 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      void DefFather(Father, List )
           PQNode Father, List ;
      BEGIN
        DirectionType  Direction ; 

        Direction = DetermineDirec(List);
        WHILE (List != NIL) DO
          PQ_NODE_FA(List) = Father;
          IF Direction == left THEN 
            List = PQ_NODE_LN(List)
          ELSE 
            List = PQ_NODE_RN(List)
          END; 
        END; 
      END ; 


 /*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

     void ZusammenFassen(Qknoten, List ,
                          ListCount ,
                          ListType ) 
           PQNode *Qknoten, *List ;
                  INTEGER   ListCount ;
                  LabType   ListType ; 
      BEGIN 
        PQNode  PKnoten, FKnoten ;

        IF ListCount > 0 THEN
          FKnoten = make_PQTree_node(WorkTree);
          IF ListCount == 1 THEN 
            PKnoten = (*List);
            PQ_NODE_FA(PKnoten) = FKnoten;
            PQ_NODE_LN(PKnoten) = NIL;
            PQ_NODE_RN(PKnoten) = NIL;
          ELSE
            PKnoten = make_PQTree_node(WorkTree);
            DefineNode(PKnoten, PNode, FKnoten, (PQNode)NIL, (PQNode)NIL, *List,
                        (PQNode)NIL, (Sedge)NIL); 
            DefFather(PKnoten, *List);
          END; 
          DefineNode(FKnoten, FNode, *Qknoten, (PQNode)NIL, (PQNode)NIL, PKnoten,
                      (PQNode)NIL, (Sedge)NIL); 
          PQ_NODE_LAB(FKnoten) = ListType;
          IF PQ_NODE_LAB(PQ_NODE_FS((*Qknoten))) == ListType THEN
            Append(&FKnoten, &PQ_NODE_FS((*Qknoten)));
            PQ_NODE_FS((*Qknoten)) = FKnoten;
          ELSE 
            Append(&FKnoten, &PQ_NODE_SS((*Qknoten)));
            PQ_NODE_SS((*Qknoten)) = FKnoten;
          END;
        END;
      END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    void TemplateP2(AktNode, EmptyList, FullList ,
                    EmptyCount, FullCount ,
                    Error )
         PQNode *AktNode, *EmptyList, *FullList ;
         INTEGER   EmptyCount, FullCount ;
         BOOLEAN   *Error ;
      BEGIN
        PQNode NewPNode ;

        IF EmptyCount == 0 THEN
          PQ_NODE_FS((*AktNode)) = (*FullList)
        ELSE
          IF FullCount == 1 THEN
            Concat(FullList, (EmptyList));
            PQ_NODE_FS((*AktNode)) = (*FullList);
          ELSE
            NewPNode = make_PQTree_node(WorkTree);
            DefineNode(NewPNode, PNode, *AktNode,(PQNode)NIL, (PQNode)NIL,
                       *FullList, (PQNode)NIL, (Sedge)NIL);
            DefFather(NewPNode, *FullList);
            Concat(&NewPNode, EmptyList);
            PQ_NODE_FS((*AktNode))= NewPNode;
          END
        END
      END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      void TemplateP3(AktNode, EmptyList, FullList ,
                      EmptyCount, FullCount , 
                      Error ) 
           PQNode *AktNode, *EmptyList, *FullList ;
           INTEGER   EmptyCount, FullCount ; 
           BOOLEAN   *Error ; 
      BEGIN 
        PQNode  PFull, PEmpty, FFull, FEmpty ;

        IF (EmptyCount == 0) OR (FullCount == 0) THEN
          (*Error) = TRUE
        ELSE 
          FEmpty = make_PQTree_node(WorkTree);
          FFull = make_PQTree_node(WorkTree);
          IF EmptyCount == 1 THEN 
            PEmpty = (*EmptyList);
            PQ_NODE_FA(PEmpty) = FEmpty;
          ELSE 
            PEmpty = make_PQTree_node(WorkTree);
            DefineNode(PEmpty, PNode, FEmpty, (PQNode)NIL, (PQNode)NIL, 
                       *EmptyList,(PQNode)NIL, (Sedge)NIL); 
            DefFather(PEmpty, *EmptyList);
          END; 
          IF FullCount == 1 THEN 
            PFull = (*FullList);
            PQ_NODE_FA(PFull) = FFull;
          ELSE 
            PFull = make_PQTree_node(WorkTree);
            DefineNode(PFull, PNode, FFull, (PQNode)NIL, (PQNode)NIL, *FullList,
                        (PQNode)NIL, (Sedge)NIL); 
            DefFather(PFull, *FullList);
          END; 
          DefineNode(FEmpty, FNode, *AktNode, FFull, (PQNode)NIL, PEmpty,
                     (PQNode)NIL, (Sedge)NIL); 
          DefineNode(FFull, FNode, *AktNode, (PQNode)NIL, FEmpty, PFull,
                      (PQNode)NIL, (Sedge)NIL); 
          PQ_NODE_LAB(FFull) = full;
          PQ_NODE_LAB(FEmpty) = empty;
          PQ_NODE_TYP((*AktNode)) = QNode;
          PQ_NODE_FS((*AktNode)) = FFull;
          PQ_NODE_SS((*AktNode)) = FEmpty;
          PQ_NODE_LAB((*AktNode)) = partial;
        END; 
      END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      void TemplateP4(AktNode, EmptyList, FullList,PartialList , 
                      EmptyCount, FullCount,PartialCount )
           PQNode *AktNode, *EmptyList, *FullList,*PartialList ; 
           INTEGER   EmptyCount, FullCount,  PartialCount ;
      BEGIN 
        PQNode  PFull, FFull ;

        ZusammenFassen(PartialList, FullList, FullCount, full);
        IF EmptyCount == 0 THEN 
          PQ_NODE_TYP((*AktNode)) = QNode;
          PQ_NODE_FS((*AktNode)) = PQ_NODE_FS((*PartialList));
          PQ_NODE_SS((*AktNode)) = PQ_NODE_SS((*PartialList));
          PQ_NODE_FA(PQ_NODE_FS((*PartialList))) = (*AktNode);
          PQ_NODE_FA(PQ_NODE_SS((*PartialList))) = (*AktNode);
          remove_node((*PartialList));
        ELSE 
          Concat(PartialList, EmptyList);
          PQ_NODE_FS((*AktNode)) = (*PartialList);
        END; 
      END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      void TemplateP5(AktNode, EmptyList, FullList,PartialList , 
                      EmptyCount, FullCount,PartialCount ) 
           PQNode *AktNode, *EmptyList, *FullList,
                 *PartialList ; 
           INTEGER   EmptyCount, FullCount,
                     PartialCount ; 
      BEGIN 
        IF FullCount > 0 THEN 
          ZusammenFassen(PartialList, FullList, FullCount, full)
        END; 
        IF EmptyCount > 0 THEN 
          ZusammenFassen(PartialList, EmptyList, EmptyCount,
                         empty)
        END;
        PQ_NODE_TYP((*AktNode)) = QNode;
        PQ_NODE_FS((*AktNode)) = PQ_NODE_FS((*PartialList));
        PQ_NODE_SS((*AktNode)) = PQ_NODE_SS((*PartialList));
        PQ_NODE_FA(PQ_NODE_FS((*AktNode))) = (*AktNode);
        PQ_NODE_FA(PQ_NODE_SS((*AktNode))) = (*AktNode);
        PQ_NODE_LAB((*AktNode)) = partial;
        remove_node((*PartialList));
      END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

      void TemplateP6(AktNode, EmptyList, FullList,PartialList , 
                      EmptyCount, FullCount,PartialCount ) 
           PQNode *AktNode, *EmptyList, *FullList,
                 *PartialList ; 
           INTEGER   EmptyCount, FullCount,
                     PartialCount ; 
      BEGIN
        PQNode  PNode1, PNode2, FSon1, FSon2, Help ;

        PNode1 = (*PartialList);
        PNode2 = PQ_NODE_RN(PNode1);
        IF FullCount > 0 THEN 
          ZusammenFassen(&PNode1, FullList, FullCount, full)
        END; 
        IF PQ_NODE_LAB(PQ_NODE_FS(PNode1)) == full THEN
          FSon1 = PQ_NODE_FS(PNode1)
        ELSE 
          FSon1 = PQ_NODE_SS(PNode1)
        END; 
        IF PQ_NODE_LAB(PQ_NODE_FS(PNode2)) == full THEN
          FSon2 = PQ_NODE_FS(PNode2)
        ELSE
          FSon2 = PQ_NODE_SS(PNode2)
        END; 
        IF PQ_NODE_LN(FSon1) != NIL THEN
          Reverse(FSon1)
        END; 
        IF PQ_NODE_RN(FSon2) != NIL THEN
          Reverse(FSon2)
        END; 
        PQ_NODE_LN(FSon1) = FSon2;
        PQ_NODE_RN(FSon2) = FSon1;
        IF PQ_NODE_FS(PNode1) == FSon1 THEN
          Help = PQ_NODE_SS(PNode1)
        ELSE 
          Help = PQ_NODE_FS(PNode1)
        END;
        IF PQ_NODE_FS(PNode2) == FSon2 THEN
          PQ_NODE_FS(PNode2) = Help
        ELSE 
          PQ_NODE_SS(PNode2) = Help
        END; 
        remove_node(PNode1);
        IF EmptyCount > 0 THEN 
          Concat(&PNode2, EmptyList);
          PQ_NODE_FS((*AktNode)) = PNode2;
        ELSE 
          PQ_NODE_TYP((*AktNode)) = QNode;
          PQ_NODE_FS((*AktNode)) = PQ_NODE_FS(PNode2);
          PQ_NODE_SS((*AktNode)) = PQ_NODE_SS(PNode2);
          PQ_NODE_FA(PQ_NODE_FS(PNode2)) = (*AktNode);
          PQ_NODE_FA(PQ_NODE_SS(PNode2)) = (*AktNode);
          remove_node(PNode2);
        END; 
      END ; 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    void MatchPTemplates(AktNode ,
                         Root , 
                         Error ) 
          PQNode *AktNode ;
          BOOLEAN   Root ; 
          BOOLEAN   *Error; 
    BEGIN /* Match_P_Templates */ 
        PQNode EmptyList, FullList, PartialList ;
        INTEGER EmptyCount, FullCount, PartialCount ; 
        LabType NodeLabel ; 

      (*Error) = FALSE;
      IF PQ_NODE_TYP(*AktNode) != PNode THEN
        (*Error) = TRUE
      ELSE 
        FindSignedSons(AktNode, &EmptyList, &FullList, &PartialList,
                       &EmptyCount, &FullCount, &PartialCount,
                       &NodeLabel);
        IF Root THEN 
          switch(PartialCount)
          BEGIN
            case 0 :
              TemplateP2(AktNode, &EmptyList, &FullList, EmptyCount,
                         FullCount, Error);
              break;
            case 1 :
              TemplateP4(AktNode, &EmptyList, &FullList,
                         &PartialList,  EmptyCount, FullCount,
                         PartialCount);
              break;
            case 2 :
              TemplateP6(AktNode, &EmptyList, &FullList,
                         &PartialList, EmptyCount, FullCount,
                         PartialCount);
              break;
            default: 
              (*Error) = TRUE;
          END
        ELSE 
          switch(PartialCount)
          BEGIN
            case 0 :
              IF FullCount == 0 THEN
                TemplateP0(*AktNode)
              ELSE
                IF   EmptyCount == 0 THEN 
                  TemplateP1(*AktNode)
                ELSE 
                  TemplateP3(AktNode, &EmptyList, &FullList,
                             EmptyCount, FullCount, Error)
                END
              END;
              break;
            case 1 :
              TemplateP5(AktNode, &EmptyList, &FullList,
                         &PartialList, EmptyCount, FullCount,
                         PartialCount);
              break;
            default : 
              (*Error) = TRUE;
              PQ_NODE_LAB((*AktNode)) = NodeLabel;
          END; 
        END; 
      END; 
    END  /* Match_P_Templates */ 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

    void ClearAndFather( AktNode, Father )
        PQNode AktNode, Father;
    BEGIN 
        PQNode  Run ;
        DirectionType  Direc ; 
        INTEGER  PC, PL ; 

        IF AktNode != NIL THEN
          PQ_NODE_FA(AktNode) = Father;
          PQ_NODE_LAB(AktNode) = empty;
          switch(PQ_NODE_TYP(AktNode))
          BEGIN
            case QNode :
            case PNode :
              PC = 0; 
              PL = 0; 
              Run = PQ_NODE_FS(AktNode);
              Direc = DetermineDirec(Run);
              WHILE (Run != NIL) DO
                ClearAndFather(Run, AktNode);
                IF PQ_NODE_PLC(Run) != 0 THEN
                  INC(PC, 1);
                  INC(PL, PQ_NODE_PLC(Run));
                END; 
                IF Direc == left THEN 
                  Run = PQ_NODE_LN(Run)
                ELSE 
                  Run = PQ_NODE_RN(Run)
                END; 
              END; 
              PQ_NODE_PCC(AktNode) = PC;
              PQ_NODE_PLC(AktNode) = PL;
              break;
            case FNode :
              ClearAndFather(PQ_NODE_FS(AktNode), AktNode);
              IF PQ_NODE_PLC(PQ_NODE_FS(AktNode)) != 0 THEN
                PQ_NODE_PCC(AktNode) = 1;
                PQ_NODE_PLC(AktNode) = PQ_NODE_PLC(PQ_NODE_FS(AktNode));
              ELSE 
                PQ_NODE_PCC(AktNode) = 0;
                PQ_NODE_PLC(AktNode) = 0;
              END; 
          END; /* Case */ 
        END; /* If AktNode != Nil */ 
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

    void Bubble(Wurzel ,Set_of_edges )
         PQNode        Wurzel ;
         ADRlist Set_of_edges ;

    BEGIN /* Bubble */
      Sedge edge;

      for_all_elements(Set_of_edges,(char*)edge);
        PQ_NODE_PLC(EDGE_PQNODE(edge)) = 1;
      end_for_all_elements; 
       
      ClearAndFather(Wurzel, (PQNode)NIL);
      for_all_elements(Set_of_edges,(char*)edge);
        PQ_NODE_PLC(EDGE_PQNODE(edge)) = 0;
      end_for_all_elements; 
    END  /* Bubble */ 


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

int        search_root ( AktNode, PL )
        PQNode        AktNode;
        int        PL;
{
        int                result;
        PQNode                Run;
        DirectionType        Direc ; 

        result = 0;
        IF AktNode != NIL THEN
          switch( PQ_NODE_TYP(AktNode) )
          BEGIN
            case Leaf  :
              result = PQ_NODE_PLC(AktNode);
              if ((result == PL) && !Root_T_S) Root_T_S=AktNode;
              break;
            case QNode :
            case PNode :
              Run = PQ_NODE_FS(AktNode);
              Direc = DetermineDirec(Run);
              WHILE (Run != NIL) DO
                result = result + search_root ( Run, PL );
                IF Direc == left THEN 
                  Run = PQ_NODE_LN(Run)
                ELSE 
                  Run = PQ_NODE_RN(Run)
                END; 
              END;
              if ((result == PL) && !Root_T_S) Root_T_S=AktNode;
              break;
            case FNode :
              result = search_root ( PQ_NODE_FS(AktNode), PL );
              break;
          END; /* Case */ 
        END; /* If AktNode != Nil */ 
        return ( result );
}

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

PQNode        Root(tree,set)
        PQNode        tree;
        ADRlist        set;
BEGIN
        Sedge        edge;
        int        dummy;

        for_all_elements( set, (char*)edge );
                PQ_NODE_PLC( EDGE_PQNODE(edge )) = 1;
        end_for_all_elements;

        Root_T_S = NULL;
        dummy = lengthlist(set);
        dummy = search_root ( tree, dummy );

        for_all_elements( set, (char*)edge );
                PQ_NODE_PLC( EDGE_PQNODE(edge) ) = 0;
        end_for_all_elements;

        return ( Root_T_S );
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

void replace_with(Rnode,T2)
  PQNode Rnode,T2;
  BEGIN

    PQ_NODE_RN(T2) = PQ_NODE_RN(Rnode);
    IF PQ_NODE_RN(Rnode) != NIL THEN
      PQ_NODE_LN(PQ_NODE_RN(Rnode)) = T2
    END;

    PQ_NODE_LN(T2) = PQ_NODE_LN(Rnode);
    IF PQ_NODE_LN(Rnode) != NIL THEN
      PQ_NODE_RN(PQ_NODE_LN(Rnode)) = T2
    END;

    PQ_NODE_FA(T2) = PQ_NODE_FA(Rnode);

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

void delete_PQ_node(Root,Rnode)
  PQNode Root,Rnode;
  BEGIN
    PQNode help;

    IF PQ_NODE_RN(Rnode) != NIL THEN
      PQ_NODE_LN(PQ_NODE_RN(Rnode)) = PQ_NODE_LN(Rnode)
    END;
    IF PQ_NODE_LN(Rnode) != NIL THEN
      PQ_NODE_RN(PQ_NODE_LN(Rnode)) = PQ_NODE_RN(Rnode)
    END;

    help = next_Son( DetermineDirec( Rnode),Rnode); 

    IF PQ_NODE_FS(Root) == Rnode THEN
        PQ_NODE_FS(Root) =  help
    END;

    IF PQ_NODE_SS(Root) == Rnode THEN
        PQ_NODE_SS(Root) = help
    END;

    DeletePQTree(Rnode);
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

void replace_node(Rnode,T2)
  PQNode Rnode,T2;
  BEGIN
    replace_with(Rnode,T2);
    PQ_NODE_FS(PQ_NODE_FA(Rnode)) = T2;
    DeletePQTree(Rnode);    
  END
    





