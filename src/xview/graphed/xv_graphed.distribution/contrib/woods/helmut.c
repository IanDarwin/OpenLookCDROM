/* (C) Universitaet Passau 1986-1991 */
/**********************************************************/
/*                                                        */
/*                        HELMUT.C                        */
/*                                                        */
/**********************************************************/

#include <std.h>
#include <sgraph.h>
#include "listen1.h"
#include "modula.h"
#include "adj.h"
#include "edgelist.h"
#include "interfac.h"

static int  int_dummy;
static char *p_char_dummy;

extern void print_sgraph();
extern void print_xy_edge_attributes();
extern char *sprintf();
extern void woods_drawing();
extern void free_xy_attrs();
extern Sgraph make_sun_size();

Global int MAX_NR,G_breite,G_hoehe;

#define OTHER_NODE(n,e) \
	( iif(e->snode == n,e->tnode,e->snode))

void     hkanten_ausgabe2(graph)
        Sgraph  graph;
{
        Snode node;
        Sedge edge;
        ADDRlist list;
        for_all_nodes(graph,node) 
          message("%d(%s) : ",node->nr,node->label);
          dfor_all_elements(attr_data_of_type(node,ADDRlist),(char*)list);
		edge = (Sedge)list->elem;
             message(" %d(%s) ", (OTHER_NODE(node,edge))->nr,(OTHER_NODE(node,edge))->label);
          dend_for_all_elements((ADDRlist)attr_data_of_type(node,ADDRlist));
          message("\n"); 
        end_for_all_nodes(graph,node);
}

void hkanten_ausgabe(graph)
        Sgraph       graph;
{
  Snode node;
  EDGELIST ecke;
  for_all_nodes(graph,node) 
     message("node : %d(%s)\n    to node :",node->nr,node->label);
     dfor_all_elements(NODE_ADJ_LIST(node),(char*)ecke);
      message(" %d(%s) ",OTHER_NODE(node,EDGELIST_EDGE(ecke))->nr,OTHER_NODE(node,EDGELIST_EDGE(ecke))->label);
     dend_for_all_elements(NODE_ADJ_LIST(node));
     message("\n"); 
  end_for_all_nodes(graph,node);
}
Sedge find_edge_in_other_graph(node,other_node)
Snode node;
Snode other_node;
{
Sedge hilf;
   for_sourcelist(node,hilf)
	{
        if(hilf->tnode->nr == NODE_OLD_NR(other_node)) return(hilf);
   } end_for_sourcelist(node,hilf);
   for_targetlist(node,hilf)
	{
        if(hilf->snode->nr == NODE_OLD_NR(other_node)) return(hilf);
    } end_for_targetlist(node,hilf);
message("ERROR in from_uwe_to_helmut\n");
}

void from_uwe_to_helmut(Work_sgraph,sgraph)
  Sgraph Work_sgraph;
  Sgraph sgraph;
{
ADDRlist Hilf; 
Sedge elem,last = NULL;
Snode n,hilf;
EDGELIST ecke;
for_all_nodes(Work_sgraph,n)
	{ 
         hilf = (Snode)find_Snode_with_number(sgraph,NODE_OLD_NR(n));
	 hilf->x = n->nr;
	 last = NULL;
   	 Hilf = NULL;
   	 dfor_all_elements(NODE_ADJ_LIST(n),(char*)ecke);
		 elem = EDGELIST_EDGE(ecke);
		 if(last == NULL)DPUSH(Hilf,find_edge_in_other_graph(hilf,OTHER_NODE(n,elem)));
		 else {
	   	      if((OTHER_NODE(n,elem) != OTHER_NODE(n,last)))
	   	  	 DPUSH(Hilf,find_edge_in_other_graph(hilf,OTHER_NODE(n,elem))); }
		 last = elem;
   	 dend_for_all_elements(NODE_ADJ_LIST(n));
         hilf->attrs.data = (char*)Hilf;
	} end_for_all_nodes(Work_sgraph,n)
}

Global void init_attrs_data(g)
Sgraph g;
{
Snode n;
Sedge e;
for_all_nodes(g,n)
	{
	for_sourcelist(n,e)
		{
		e->attrs.data = nil;
	} end_for_sourcelist(n,e)
	if(g->directed)
		{
		for_targetlist(n,e)
			{
			e->attrs.data = nil;
		} end_for_targetlist(n,e)
		}
	n->attrs.data = nil;
} end_for_all_nodes(g,n)
}

/************    Dump-Routinen   ******************/

Local void	print_graph_attributes (file, g)
FILE	*file;
Sgraph	g;
{}
Local void	print_node_attributes (file, n)
FILE	*file;
Snode	n;
{
if (n->x >= 0 || n->y >= 0 )printf("{$ %d %d $}",n->x,n->y);
}
Local void	print_edge_attributes (file, e)
FILE	*file;
Sedge	e;
{}

Global void druck(g, print_graphattribut,print_nodeattribut,print_edgeattribut)
void	(*print_graphattribut)();
void	(*print_nodeattribut)();
void	(*print_edgeattribut)();
Sgraph  g;
{
static char	fname[] = "helmut.gra";
auto   FILE	*outfile;
if( g != empty_graph)
	{
	if ( !(outfile = freopen ("output.gra", "w", stdout) 
			/*fopen( fname, "w")*/		))
		{
		p_char_dummy = sprintf("error opening %s\n",fname);
		exit(0);
		}
	print_sgraph(outfile,g,print_graphattribut,print_nodeattribut,
				print_edgeattribut);
	if( fclose(outfile))
		{
		p_char_dummy = sprintf("error closing %s\n",fname);
		exit(0);
		}
	printf("Graph was saved as  %s \n",fname);
	}
}




