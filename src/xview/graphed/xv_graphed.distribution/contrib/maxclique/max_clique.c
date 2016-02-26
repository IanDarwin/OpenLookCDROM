/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : maxclique.c						*
 * Aufgabe : Umwandlung von Slist in L_node_list Datenstruktur		*
 *           Stellt die Funktionen zur Verfuegung, die fuer die Berech-	*
 *           nung der Clique benoetigt werden.				*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 21.8.90							*
 ************************************************************************/
 
#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>

#include "gfunc.h"
#include "liste.h"
#include "clique.h"


static Slist Nodelist_to_Slist ( list )
L_node_list list;
	/*
	Erzeugt die Datenstruktur Slist und traegt darin die Knoten
	aus l ein.
	*/
	{	
		int nr;
		Slist l_new = empty_slist;
		L_for_node_list (list, nr)
		{	l_new = add_to_slist (l_new,
				    make_attr(ATTR_DATA,
					 (char *) L_list_entry(list, nr)));
		} L_end_for_node_list (list, nr);
		return l_new;
	}
	
	
static L_node_list Slist_to_Nodelist ( list )
Slist list;
	/*
	Erzeugt eine L_node_list Datenstruktur, die die Knoten aus list
	enthaelt.
	*/
	{	
		Slist l;
		L_node_list l_new = L_create_list (30);
		for_slist(list, l)
		{	L_append (l_new, attr_data_of_type(l, Snode));
		} end_for_slist(list, l);
		return l_new;
	}
	
 
Slist	M_find_max_clique_in_list ( list  )
Slist	list;
	/*
	Bestimmt die maximale Clique auf einer Menge von Knoten.
	*/
	{	L_node_list nlist   = Slist_to_Nodelist ( list );
		L_node_list result  = C_find_clique_in_list ( nlist );
		Slist       sresult = Nodelist_to_Slist( result );
		L_close_list (nlist);
		L_close_list (result);
		return sresult;
	}
	
	
Slist	M_find_max_clique         ( graph )
Sgraph	graph;
	/*
	Bestimmt die maximale Clique im gesamten Graphen.
	*/
	{	L_node_list result  = C_find_clique ( graph );
		Slist       sresult = Nodelist_to_Slist ( result );
		L_close_list (result);
		return sresult;
	}
	
	
Sedge	M_check_double_edges      ( graph )
Sgraph	graph;
	/*
	Ueberprueft, ob es irgendwo im Graph Mehrfachkanten gibt. Falls nicht
	wird NULL zurueckgeliefert, sonst ein Zeiger auf eine der Mehrfach-
	kanten.
	*/
	{		return (G_has_double_edges(graph));
	}



/************************************************************************/
/*									*/
/*			      MaxClique menu entry			*/
/*									*/
/************************************************************************/
 
extern	Sedge	M_check_double_edges      ( /* Sgraph graph */ );
extern	Slist	M_find_max_clique_in_list ( /* Slist  list  */ );
extern	Slist	M_find_max_clique         ( /* Sgraph graph */ );

static void main_proc_maxclique(info)
Sgraph_proc_info info;
	/*
	Computes a max-clique of an undirected Graph. If a group of nodes
	is marked, the clique will be computed on these nodes, otherwise
	on the whole graph.
	*/
{	
	Slist result;
	Sedge e;
	Sgraph g = info->sgraph;
	
	/* Gueltigkeit des Graphen ueberpruefen */

	if (g == empty_sgraph) {
		warning ("empty graph\n");
		return;
	}
	
	if (g->directed)
	{	bell();
		message("Max-Clique: the graph must be undirected.\n");
		return;
	}
	e = M_check_double_edges(g);
	if (e != NULL)
	{	bell();
		if (e->snode == e->tnode)
			message("Max-Clique: no loops allowed.\n");		
		else
			message("Max-Clique: there may be only one edge between two nodes.\n");
		info->new_sgraph = info->sgraph;
		info->new_selected = SGRAPH_SELECTED_SEDGE;
		info->new_selection.sedge = e;
		return;
	}
	
	if (info->selected == SGRAPH_SELECTED_GROUP)
		result = M_find_max_clique_in_list (info->selection.group);
	else
		result = M_find_max_clique (info->sgraph);
	if (result != NULL)
	{
		info->new_selected		= SGRAPH_SELECTED_GROUP;
		info->new_sgraph		= info->sgraph;
		info->new_selection.group	= result;
		info->no_changes		= true;
		info->no_structure_changes	= true;
	}
}



char     *menu_max_clique (menu, menu_item) 
char	 *menu;
char     *menu_item;
{
	return (call_sgraph_proc (main_proc_maxclique));
}


