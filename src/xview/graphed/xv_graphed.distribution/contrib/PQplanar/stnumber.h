/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : stnumber.h                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 07.08.90                                       */
/*****************************************************************/
/*        This file contains the routine                         */
/*        st_number to compute a ST-Numbering                    */
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
/*       DEFINES                                                 */
/*****************************************************************/

#define MAXINT                32000
#define UNDEF                -1
#define REACHED                0
#define ST_NEW                0
#define OLD                1
#define CYCLE                0
#define TREE                1

#define MIN(a,b)        ( (a) < (b) ? a : b )
#define MAX(a,b)        ( (a) > (b) ? a : b )

#define ST_NUMBER(node)  (node)->nr

#define    ST_GRAPH_ATTRS(g) \
        (  ( (st_graph_attributes *)( (g)->attrs.data ) )        )
#define    ST_GRAPH_MARK(g) \
        (  ST_GRAPH_ATTRS(g) -> mark                                )

#define    ST_NODE_ATTRS(n) \
        (  ( (st_node_attributes  *)( (n)->attrs.data ) )        )
#define        ST_NODE_MARK(n) \
        (  ST_NODE_ATTRS(n) -> mark                                )
#define        PRE_ORDER_NUM(n) \
        (  ST_NODE_ATTRS(n) -> pre_order_num                        )
#define        LV(n) \
        (  ST_NODE_ATTRS(n) -> lv                                )
#define CYCLE_W(n) \
        (  ST_NODE_ATTRS(n) -> cycle_w                                )
#define W_CYCLE(n) \
        (  ST_NODE_ATTRS(n) -> w_cycle                                )
#define TREE_W(n) \
        (  ST_NODE_ATTRS(n) -> tree_w                                )
#define PARENT(n) \
        (  ST_NODE_ATTRS(n) -> parent                                )
#define FITTING(n) \
        (  ST_NODE_ATTRS(n) -> fitting                                )

#define        ST_EDGE_ATTRS(e) \
        (  ( (st_edge_attributes  *)( (e)->attrs.data ) )        )
#define        ST_EDGE_MARK(e) \
        (  ST_EDGE_ATTRS(e) -> mark                                )
#define        ST_EDGE_TYPE(e) \
        (  ST_EDGE_ATTRS(e) -> type                                )


/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/


typedef        struct
        {
                int        mark;
                Attributes attrs;               /* to save the old attributes */
        } st_graph_attributes;

typedef        struct
        {
                int        mark;                /* to mark a node v ST_NEW or OLD            */
                int        pre_order_num;        /* of the depth-first spanning tree */
                int        lv;                /* the value L(v)=min{....}            */
                LIST        cycle_w;        /* list of nodes w such that {v,w}
                                           is cycle-edge and v is contained
                                           in the path from r to w            */
                LIST        w_cycle;        /* list of nodes w such that {v,w}
                                           is cycle-edge and w is contained
                                           in the path from r to v            */
                LIST        tree_w;                /* list of nodes w such that {v,w}
                                           is tree-edge and w is son of v   */
                Snode        parent;                /* the node u that is parent of v   */
                Snode        fitting;        /* a node n such that n=L(v) or
                                           L(n)=L(v)                            */
                Attributes   attrs;        /* to save the old attributes */
        } st_node_attributes;

typedef        struct
        {
                int        mark;                /* to mark an edge e ST_NEW or OLD            */
                int        type;                /* to mark an edge e TREE or CYCLE  */
                Attributes attrs;             /* to save the old attributes */
        } st_edge_attributes;


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void  st_number ( );



