/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : pqtree.h                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define  PQ_NODE_ATTRS(n)         \
         attr_data_of_type(n,PQNodePtr)
#define  PQ_NODE_TYP(n)                  \
         ( PQ_NODE_ATTRS(n) -> Typ )
#define PQ_NODE_FA(n)     \
         ( PQ_NODE_ATTRS(n) -> Father            )
#define PQ_NODE_FS(n)     \
         ( PQ_NODE_ATTRS(n) -> FirstSon     )
#define PQ_NODE_SS(n)     \
         ( PQ_NODE_ATTRS(n) -> SecondSon  )
#define PQ_NODE_LN(n)     \
         ( PQ_NODE_ATTRS(n) -> LeftNeighbour  )
#define PQ_NODE_RN(n)     \
         ( PQ_NODE_ATTRS(n) -> RightNeighbour )
#define PQ_NODE_EDGE(n)     \
         ( PQ_NODE_ATTRS(n) ->  edge     )

#define PQ_NODE_KENNUNG(n)     \
         ( PQ_NODE_ATTRS(n) ->  Kennung     )
#define PQ_NODE_LAB(n)     \
         ( PQ_NODE_KENNUNG(n). Lab    )
#define PQ_NODE_MARK(n)     \
         ( PQ_NODE_KENNUNG(n) .Mark    )
#define PQ_NODE_PCC(n)     \
         ( PQ_NODE_KENNUNG(n). PertChildCount )
#define PQ_NODE_PLC(n)     \
         ( PQ_NODE_KENNUNG(n). PertLeafCount )

#define PQ_NODE_DIRECT(n)  \
         ( PQ_NODE_ATTRS(n) ->  list_direction )
#define PQ_NODE_DIRECT_NODE(n)  \
         ( PQ_NODE_ATTRS(n) ->  list_direction_node )


#define Qnode   1
#define Pnode   2
#define Leave   3



/*****************************************************************/

typedef     enum {PNode, QNode, FNode, Leaf, RNode} NodeType ;
typedef     enum {empty, full, partial} LabType ;
typedef     enum {unmarked, queued, blocked, unblocked, visited} MarkType ;
typedef     enum {left, right} DirectionType ;


typedef      struct { 
                LabType Lab ;
                MarkType Mark ; 
                int PertChildCount, PertLeafCount ;
              } KennRec;

typedef      struct {
                  NodeType Typ ;
                  Snode Father, FirstSon, SecondSon,
                            LeftNeighbour, RightNeighbour ;
                  Sedge edge ;
                  KennRec Kennung ;
                    BOOLEAN list_direction;
                    Snode   list_direction_node;
                } PQNodeRec ;

typedef     PQNodeRec *PQNodePtr  ;

typedef      struct queueRec {
                 Snode Inhalt ;
                 struct queueRec *Next ;
               } QueueRec ;
typedef     QueueRec *QueuePtr  ;


/*****************************************************************/

extern  void          init_PQTree();

extern  void          GetFromQueue( );

extern  void          PutOnQueue( );

extern  void          CreateQueue();

extern  Snode         next_Son();

extern  void          MatchPTemplates();

extern  void          MatchQTemplates();

extern  void          remove_all_edges();

extern  void          build_new_edges();

extern  DirectionType DetermineDirec();

extern  void          replace_node();

extern  void          delete_PQ_node();
