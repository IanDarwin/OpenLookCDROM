/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : interfac.h                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 25.07.90                                       */
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
/*****************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef struct { INTEGER winkelzahl;
                 INTEGER knotenzahl;
               } * graph_att_Ptr;

/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define GRAPH_ATTRS(graph)   attr_data_of_type(graph,graph_att_Ptr)

#define GRAPH_WINKELZAHL(graph)  \
       (GRAPH_ATTRS(graph)->winkelzahl)

#define GRAPH_KNOTENZAHL(graph)  \
       (GRAPH_ATTRS(graph)->knotenzahl)


/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef struct { INTEGER old_nr;
                 INTEGER old_x;
                 INTEGER old_y;
                 Snode   old_node;
                 Snode   copy_node;
                 INTEGER level;
                 INTEGER var;
                 ADDRlist edgelist ;
                 INTEGER grad;
                 INTEGER zaehler;
                 BOOLEAN aussen;
                 adj_rec adj;
                 BOOLEAN reached;
                 BOOLEAN fix;
               } *node_att_Ptr;

/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define NODE_X(node)  node->x
#define NODE_Y(node)  node->y

#define NODE_ATTRS(node)   attr_data_of_type(node,node_att_Ptr)

#define NODE_OLD_NR(node)   \
     (NODE_ATTRS(node)->old_nr) 

#define NODE_OLD_X(node)   \
     (NODE_ATTRS(node)->old_x) 

#define NODE_OLD_Y(node)   \
     (NODE_ATTRS(node)->old_y) 

#define NODE_OLD_NODE(node)   \
     (NODE_ATTRS(node)->old_node) 

#define NODE_COPY_NODE(node)   \
     (NODE_ATTRS(node)->copy_node) 

#define NODE_LEVEL(node)   \
     (NODE_ATTRS(node)->level) 

#define NODE_VAR(node)   \
     (NODE_ATTRS(node)->var) 

#define NODE_EDGELIST(node)   \
     (NODE_ATTRS(node)->edgelist) 

#define NODE_FULL(node)       \
     node_full(node) 

#define NODE_ZAEHLER(node)    \
     (NODE_ATTRS(node)->zaehler) 

#define NODE_GRAD(node)    \
     (NODE_ATTRS(node)->grad) 

#define NODE_AUSSEN(node)    \
     (NODE_ATTRS(node)->aussen) 

#define NODE_ADJ(node)    \
     (NODE_ATTRS(node)->adj) 

#define NODE_ADJ_LIST(node)    \
     (NODE_ADJ(node).adj_list) 

#define NODE_ADJ_DIRECTION(node)    \
     (NODE_ADJ(node).direction) 

#define NODE_ADJ_DIRECT_NODE(node)    \
     (NODE_ADJ(node).direct_node) 

#define NODE_REACHED(node)    \
     (NODE_ATTRS(node)->reached) 

#define NODE_FIX(node)    \
     (NODE_ATTRS(node)->fix) 




/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef struct { Snode PQnode;
                 REAL x;
                 REAL y;
                 BOOLEAN fix;
                 BOOLEAN reached;
               } *edge_att_Ptr;

/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define EDGE_ATTRS(edge)   attr_data_of_type(edge,edge_att_Ptr)

#define EDGE_X(edge)          \
     (EDGE_ATTRS(edge)->x)

#define EDGE_Y(edge)          \
     (EDGE_ATTRS(edge)->y)

#define EDGE_FIX(edge)          \
     (EDGE_ATTRS(edge)->fix)

#define EDGE_REACHED(edge)          \
     (EDGE_ATTRS(edge)->reached)

#define EDGE_PQNODE(edge)          \
     (EDGE_ATTRS(edge)->PQnode)



/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern graph_att_Ptr make_graph_att();

extern node_att_Ptr make_new_node_att();

extern node_att_Ptr make_node_att();

extern edge_att_Ptr make_edge_att();

extern void        init_working_graph();

extern void        setze_aussen_punkte();

extern void        setze_fixe_punkte();

extern void        draw_planar();

extern void        draw_local();

extern void        graph_on_window();

extern void        kanten_ausgabe();

extern void        kanten_ausgabe2();

extern void        kanten_ausgabe3();

extern void        setze_aussen_node();

extern void        setze_fix_node();

extern void        setze_fix_edge();

extern void        store_xy_back();

extern void        store_xy_back_ueber_nr();

