/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : sgraphhi.h                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define for_all_edges(node,edge,other_node,statment) \
     for_sourcelist(node,edge);                      \
       other_node=edge->tnode;                       \
       statment;                                     \
     end_for_sourcelist(node,edge);                  \
     if( node->graph->directed ) {                   \
       for_targetlist(node,edge);                    \
         other_node=edge->snode;                     \
         statment;                                   \
       end_for_targetlist(node,edge);                \
     }

#define OTHER_NODE(node,edge)                        \
     iif((edge)->snode == (node) ,(edge)->tnode,(edge)->snode) 

#define for_unique_sourcelist(node,edge) \
     for_sourcelist(node,edge)           \
       IF mein_unique_edge(edge) THEN

#define end_for_unique_sourcelist(node,edge) \
       ENDIF;                                \
     end_for_sourcelist(node,edge) 


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern int    number_off_vertices();

extern int    number_off_vertices();

extern Snode  find_Snode_with_number();

extern Sedge  find_edge();

extern void   print_node();

extern Sgraph copy_sgraph ();

extern Sgraph	copy_sgraph_with_edgelist ();

extern void   destroy_graph();

