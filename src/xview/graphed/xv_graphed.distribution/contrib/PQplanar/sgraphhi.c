/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : sgraphhi.c                                     */
/*        AUTOR : Uwe Schnieders 				 */
/*        UPDATE: 23.07.90					 */
/*****************************************************************/
/*                                                               */
/* Hilfsroutinen fuer die Sgraph-Implementation                  */
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
#include "listen1.h"
#include "edgelist.h"
#include "adj.h"
#include "interfac.h"
#include "sgraphhi.h"


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN mein_unique_edge(edge)
        Sedge edge;
  BEGIN
    RETURN (edge->snode->nr < edge->tnode->nr)
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

BOOLEAN equal_edge(edge1,edge2)
        Sedge edge1;
        Sedge edge2;
  BEGIN
    RETURN (edge1 == edge2) OR ((edge1->tnode == edge2->snode) AND 
                                (edge2->tnode == edge1->snode)      );
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

int number_off_nodes(graph)
  Sgraph graph;
  BEGIN
    int x = 0;
    Snode node;
    for_all_nodes (graph,node);
      x++;
    end_for_all_nodes (graph,node);
    RETURN( x );
  END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


int number_off_fix_nodes(graph)
  Sgraph graph;
  BEGIN
    int x = 0;
    Snode node;
    for_all_nodes (graph,node);
      IF NODE_FIX(node) THEN
        x++;
      ENDIF;
    end_for_all_nodes (graph,node);
    RETURN( x );
  END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


int number_off_edges_at_node_undirected(node)
        Snode node;
  BEGIN
    int x = 0;
    Sedge edge;
    for_unique_sourcelist (node,edge);
        x++;
    end_for_unique_sourcelist (node,edge);
    RETURN( x  );
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

int number_off_edges_undirected(graph)
  Sgraph graph;
  BEGIN
    int x = 0;
    Snode node;
    for_all_nodes (graph,node);
       x = x + number_off_edges_at_node_undirected(node);
    end_for_all_nodes (graph,node);
    RETURN( x  );
  END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


int number_off_fix_edges_undirected(graph)
  Sgraph graph;
  BEGIN
    int x = 0;
    Snode node;
    Sedge edge;
    for_all_nodes (graph,node);
      for_unique_sourcelist (node,edge);
          IF EDGE_FIX(edge) THEN
            x++;
          ENDIF;
      end_for_unique_sourcelist (node,edge);
    end_for_all_nodes (graph,node);
    RETURN( x );
  END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


int number_off_new_edges(graph)
  Sgraph graph;
  BEGIN
    int x = 0;
    Snode node;
    EDGELIST edge;

    for_all_nodes (graph,node);
      for_edgelist(node,edge);
        x++;
      end_for_edgelist(node);
    end_for_all_nodes (graph,node);
    RETURN( x  DIV 2 );
  END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


Snode find_Snode_with_number(graph,x)
  Sgraph graph;
  int x;
  BEGIN
    Snode node;
    for_all_nodes (graph,node)
      IF node->nr == x THEN
        RETURN(node);
      END;
    end_for_all_nodes (graph,node);
    RETURN(NIL);
  END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


Sedge find_edge(n1,n2)
	Snode n1,n2;
BEGIN
  Sedge edge;

  for_unique_sourcelist (n1,edge);
    IF edge->tnode == n2 THEN
        RETURN(edge);
    END;
  end_for_unique_sourcelist (n1,edge);

  for_unique_sourcelist (n2,edge);
    IF edge->tnode == n1 THEN
        RETURN(edge);
    END;
  end_for_unique_sourcelist (n2,edge);

  RETURN( NIL );
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

void print_node(node)
	Snode node;
  BEGIN
    printf("%d (%s) ", node->nr,node->label);
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

Sgraph	copy_sgraph (ssgraph)
Sgraph	ssgraph;
{
	Snode		ssnode;
	Sedge		ssedge;
	Sgraph		sgraph;
	Snode		snode;
	Sedge		sedge;
	
	Snode		source_node;
	Snode		target_node;
        Edgeline        new_straigth_edgeline,dummy;

		
	sgraph = make_graph(make_attr(ATTR_DATA,make_graph_att()));

        sgraph->directed = FALSE;

	for_all_nodes (ssgraph, ssnode) 
		snode= make_node (sgraph,make_attr(ATTR_DATA,make_node_att(ssnode)));
		snode->nr = ssnode->nr;
		snode->graphed = ssnode->graphed;
		set_nodelabel (snode,
			strsave (ssnode->label));

		attr_data(ssnode) = (char*)snode;

                NODE_OLD_NODE(snode) = ssnode;
	 end_for_all_nodes (ssgraph, ssnode);
			
        for_all_nodes (ssgraph, ssnode) 
	  for_sourcelist (ssnode, ssedge) 
/* Commented out MH 13/10/91
            new_straigth_edgeline = new_edgeline(ssedge->snode->x,ssedge->snode->y);
            dummy = add_to_edgeline(new_straigth_edgeline,
                                    ssedge->tnode->x,ssedge->tnode->y);
            edge_set(graphed_edge(ssedge),EDGE_LINE,new_straigth_edgeline,0);
*/
            IF ssnode->nr < ssedge->tnode->nr THEN
               source_node = attr_data_of_type(ssedge->snode,Snode);
               target_node = attr_data_of_type(ssedge->tnode,Snode);
               IF NOT(find_edge(source_node,target_node)) THEN
                 sedge = make_edge (source_node,target_node,
                                    make_attr(ATTR_DATA,make_edge_att())) ;
                 sedge->graphed = ssedge->graphed;
                 set_edgelabel (sedge, strsave (ssedge->label));
               ENDIF;
            ENDIF;
          end_for_sourcelist (ssnode, ssedge);
          for_targetlist (ssnode, ssedge) 
            IF ssnode->nr < ssedge->snode->nr THEN
               source_node = attr_data_of_type(ssedge->snode,Snode);
               target_node = attr_data_of_type(ssedge->tnode,Snode);
               IF NOT(find_edge(source_node,target_node)) THEN              
                  sedge = make_edge (source_node,target_node,
                                     make_attr(ATTR_DATA,make_edge_att()));
               ENDIF;
               sedge->graphed = ssedge->graphed;
               set_edgelabel (sedge, strsave (ssedge->label));
            ENDIF;
          end_for_targetlist (ssnode, ssedge);

	end_for_all_nodes (ssgraph, ssnode);

	printf("end of copy sgraph \n");

	return sgraph;
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

Sgraph	copy_sgraph_with_edgelist (ssgraph)
Sgraph	ssgraph;
{
	Snode		ssnode;
	Sedge		ssedge;
	Sgraph		sgraph;
	Snode		snode;
	Sedge		sedge;
	
	Snode		source_node;
	Snode		target_node;

        EDGELIST   edgelist_element;
        Snode      tnode;

		
	sgraph = make_graph(make_attr(ATTR_DATA,make_graph_att()));

        sgraph->directed = FALSE;

	for_all_nodes (ssgraph, ssnode) 
		snode= make_node (sgraph,make_attr(ATTR_DATA,make_node_att(ssnode)) );
		snode->nr = ssnode->nr;
		snode->x = ssnode->x;
		snode->y = ssnode->y;
		snode->graphed = ssnode->graphed;
		set_nodelabel (snode,
			strsave (ssnode->label));

		NODE_COPY_NODE(ssnode) = snode;
                NODE_OLD_NODE(snode) = ssnode;

                NODE_OLD_X(snode)   = NODE_OLD_X(ssnode);
                NODE_OLD_Y(snode)   = NODE_OLD_Y(ssnode);
                NODE_LEVEL(snode)   = NODE_LEVEL(ssnode);
                NODE_AUSSEN(snode)  = NODE_AUSSEN(ssnode);
                NODE_FIX(snode)     = NODE_FIX(ssnode);

	 end_for_all_nodes (ssgraph, ssnode);
			
        for_all_nodes (ssgraph, ssnode) 
	  for_sourcelist (ssnode, ssedge) 
            IF ssnode->nr < ssedge->tnode->nr THEN
               source_node = NODE_COPY_NODE(ssedge->snode);
               target_node = NODE_COPY_NODE(ssedge->tnode);
               IF NOT(find_edge(source_node,target_node)) THEN
                 sedge = make_edge (source_node,target_node,
                                    make_attr(ATTR_DATA,make_edge_att())) ;
                 sedge->graphed = ssedge->graphed;
                 set_edgelabel (sedge, strsave (ssedge->label));
               ENDIF;
            ENDIF;
          end_for_sourcelist (ssnode, ssedge);
        end_for_all_nodes (ssgraph, ssnode);


        for_all_nodes (ssgraph, ssnode) 
          for_edgelist(ssnode,edgelist_element)
             source_node = NODE_COPY_NODE(EDGELIST_EDGE(edgelist_element)->snode);
             target_node = NODE_COPY_NODE(EDGELIST_EDGE(edgelist_element)->tnode);
             sedge       = find_edge(source_node,target_node);
             snode       = NODE_COPY_NODE(ssnode);
             tnode       = OTHER_NODE(snode,sedge);
             QUEUE_NODE_EDGELIST(snode,sedge);
          end_for_edgelist(ssnode);
        end_for_all_nodes (ssgraph, ssnode);
 
        conect_edgelist(sgraph);
        kanten_ausgabe3(sgraph);

	printf("end of copy sgraph \n");

	return sgraph;
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

BOOLEAN schnitt_edge_edge(edge1,edge2)
        Sedge   edge1;
        Sedge   edge2;
  BEGIN
    RETURN(schnitt_gerade_gerade(edge1->snode->x,edge1->snode->y,
                                 edge1->tnode->x,edge1->tnode->y,
                                 edge2->snode->x,edge2->snode->y,
                                 edge2->tnode->x,edge2->tnode->y));
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

BOOLEAN schnitt_graph_edge(graph,edge1)
        Sgraph  graph;
        Sedge   edge1;
  BEGIN
        Snode   node;
        Sedge   edge2;

    for_all_nodes(graph,node)
      for_unique_sourcelist(node,edge2)
        IF NOT(equal_edge(edge1,edge2)) THEN
          IF schnitt_edge_edge(edge1,edge2) THEN
            RETURN(TRUE);
          ENDIF;
        ENDIF;
      end_for_unique_sourcelist(node,edge2)
    end_for_all_nodes(graph,node)
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

BOOLEAN test_planar_drawing(graph)
        Sgraph  graph;
  BEGIN
        Snode   node;
        Sedge   edge;

    for_all_nodes(graph,node)
      for_unique_sourcelist(node,edge)
        IF schnitt_graph_edge(graph,edge) THEN
          RETURN(FALSE);
        ENDIF;
      end_for_unique_sourcelist(node,edge)
    end_for_all_nodes(graph,node)
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

void destroy_edge_attrs(edge)
    Sedge edge;
  BEGIN
    free(attr_data(edge));
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

void destroy_edge(edge)
    Sedge edge;
  BEGIN
    IF attr_data(edge) != NIL THEN
      destroy_edge_attrs(edge);
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

void destroy_node_attrs(node)
        Snode node;
  BEGIN
    DCLEAR_LIST_AND_ELEMENTS(NODE_EDGELIST(node));
    DCLEAR_LIST(NODE_ADJ_LIST(node));
    free(attr_data(node));
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

void destroy_node(node)
        Snode node;
  BEGIN
    IF attr_data(node) != NIL THEN
      destroy_node_attrs(node);
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

void destroy_graph_attrs(graph)
        Sgraph graph;
  BEGIN
    IF attr_data(graph) != NIL THEN
      free(attr_data(graph));
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

void destroy_all_attributes_of_graph(graph)
        Sgraph *graph;
  BEGIN
        Snode node;
        Sedge edge;

    IF (*graph) != NIL THEN
      for_all_nodes((*graph),node)
        for_sourcelist(node,edge)
          destroy_edge(edge);
        end_for_sourcelist(node,edge);
        destroy_node(node);
      end_for_all_nodes((*graph),node)

      destroy_graph_attrs((*graph));
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

void destroy_graph(graph)
        Sgraph *graph;
  BEGIN
        Snode node;
        Sedge edge;

    IF (*graph) != NIL THEN
      destroy_all_attributes_of_graph(graph);
      remove_graph((*graph))
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

void convert_list_from_graph_to_graph(list,sgraph1,sgraph2) 
        Slist list;
        Sgraph sgraph1;
        Sgraph sgraph2;
  BEGIN
        Slist    list_element;
        Snode    node1;

    for_slist(list,list_element)
      node1 = attr_data_of_type(list_element,Snode);
      list_element->attrs = make_attr(ATTR_DATA,
                                      find_Snode_with_number(sgraph2,node1->nr));
    end_for_slist(list,list_element);
  ENDPROC


