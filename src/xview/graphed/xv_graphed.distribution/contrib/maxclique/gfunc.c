/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : gfunc.c							*
 * Aufgabe : Allgemeine Graf-Funktionen					*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 21.10.89							*
 ************************************************************************/

#include <std.h>
#include <sgraph.h>

int G_knoten_anzahl( g )
Sgraph g;
	/*
	Zaehlt die Anzahl der in "graph" vorkommenden Knoten.
	*/
	{	Snode node;
		int i = 0;
		for_all_nodes(g,node)
			i++;
		end_for_all_nodes(g,node);
		return (i);
	}

int G_kanten_anzahl( knoten )
Snode knoten;
	/*
	Zaehlt die Kanten eines Graphen, die an  "knoten" haengen.
	*/
	{	Sedge edge;
		int i = 0;
		for_sourcelist(knoten, edge)
			i++;
		end_for_sourcelist(knoten, edge);
		return (i);
	}


bool G_exist_edge(knoten1,knoten2)
Snode knoten1, knoten2;
	/*
	Liefert TRUE, falls zwischen Knoten1 und Knoten2 eine Kante existiert.
	*/
	{       Sedge edge;
		for_sourcelist(knoten2,edge)
		{	if (edge->tnode == knoten1)
				return TRUE;
		} end_for_sourcelist(knoten2,edge);
		return FALSE;
	}


Sedge G_has_double_edges(g)
Sgraph g;
	/*
	Bestimmt, ob irgendwelche Kanten mehrfach auftreten, d.h. ob 
	zwischen je zwei Knoten hoechstens eine Kanten existiert.
	Falls eine Kante gefunden wird, wird dies zurueckgegeben, sonst
	NULL.
	*/
	{	Snode n;
		for_all_nodes(g,n)
		{	Sedge e;
			for_sourcelist(n,e)
			{	Sedge lauf = e->ssuc; 
				while (lauf != n->slist)
				{	if (e->tnode == lauf->tnode)
						return e;
					lauf = lauf->ssuc;
				}					
			} end_for_sourcelist(n,e);
		} end_for_all_nodes(g,n);
		return NULL;
	}


