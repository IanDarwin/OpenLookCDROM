/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : clique.c                                                   *
 * Aufgabe : Berechnen einer maximalen Clique                           *
 *                                                                      *
 * Autor   : Torsten Bachmann                                           *
 * Datum   : 22.11.89                                                   *
 ************************************************************************/

#include <std.h>
#include <sgraph.h>
#include "gfunc.h"
#include "liste.h"
#include "kclique.h"


static int max_cliquen_kanten(l)
L_node_list l;
	/*
	Ermittelt die Zahl der Kanten, mit denen in der Knotenmenge l die
	groesste Clique gebildet werden kann. Eine 4-Clique benoetigt z.B.
	mindestens 4 Knoten mit jeweils mindestens 3 Kanten. Allgemein be-
	deutet dies, dass eine k-Clique nur vorhanden sein kann, wenn die-
	ser Graph mindestes k Knoten mit jeweils mindestens k-1 Kanten ent-
	haelt. Ermittelt wird daher das groesste k-1, sodass genug Knoten
	und Kanten fuer eine k-Clique vorhanden sind.
	*/
	{       int knoten_anz, *edges_per_node, max;
		knoten_anz = l->curlen;
		{       /* Ermitteln der Knotenzahl je Kantenanzahl */
			int nr;
			edges_per_node = (int *) calloc(knoten_anz+1, sizeof(int));
			G_Test_Stack_Overflow(edges_per_node);
			L_for_node_list(l,nr)
			{	edges_per_node[attr_flags(L_list_entry(l,nr))]++;
			} L_end_for_node_list(g,nr);
		}
		{       /* Ermitteln der maximalen benoetigten Kantenzahl */
			int i = 0;
			max = knoten_anz + 1;
			while (max>i)
			{       i = i + edges_per_node[--max];
			}
		}
		free(edges_per_node);
		return(max);
	}


static L_node_list  make_search_list(maxkanten,l)
int maxkanten;
L_node_list l;
	/*
	Erzeugt eine Liste mit den Knoten die als Startpunkte genutzt werden.
	Diese Knoten muessen mindestens 2 und duerfen hoechstens maxkanten
	Kanten haben. Sie werden absteigend sortiert.
	Koennte durch sortiertes Einfuegen in list schneller werden.
	*/
	{       L_node_list list;
		Snode n;
		int hilf, nr;
		list = L_create_list(l->curlen);
		for (hilf = maxkanten; hilf >= 1; hilf--)
		{	L_for_node_list(l,nr)
			{       n = L_list_entry(l,nr);
				if (attr_flags(n) == hilf)
				{	L_append(list,n);
					if (hilf == 1)
						return list;
				   /* Eine 2-Clique ist schon die kleinste,  */
				   /* daher reicht 1 Eintrag mit einer Kante */
				}
			}
			L_end_for_node_list(l,nr);
		}
		return list;
	}



static L_node_list search_clique(inlist)
L_node_list inlist;
	/*
	Sucht die maximale Clique in der Knotenliste list heraus. In den
	Attributfeldern dieser Knoten muessen die jeweiligen Kantenzahlen
	eingetragen sein. Die Liste inlist wird nicht veraendert.
	*/
	{	L_node_list searchlist, savelist, list;
		int maxknoten, maxkanten;
		/* Groesstmoeglichen Grad der Clique bestimmen */
		maxknoten = (maxkanten = max_cliquen_kanten(inlist)) +1;
		/* Existieren ueberhaupt keine Kanten mehr ? */
		if (maxknoten == 1)
		{       savelist = L_create_list(1);
			L_append(savelist,L_list_entry(inlist,0));
			return savelist;
		}
		savelist = L_create_list(0);

		/* Existiert nur eine 2-clique, so finde zwei verbundene Knoten. */
		if (maxkanten == 1)
		{	int nr1, nr2;
			Snode node1, node2;
			L_for_node_list (inlist, nr1)
			{	L_for_node_list (inlist, nr2)
				{	node1 = L_list_entry (inlist, nr1);
					node2 = L_list_entry (inlist, nr2);
					if (G_exist_edge(node1, node2))
					{	L_append (savelist, node1);
						L_append (savelist, node2);
						return savelist;
					}
				} L_end_for_node_list (inlist, nr);
			} L_end_for_node_list (inlist, nr);
		
		}

		/* Liste zur Abarbeitung der Knoten herstellen */
		searchlist = make_search_list(maxkanten, inlist);

		/* Alle Knoten dieser Liste ueberpruefen, und Maximum be-	*/
		/* stimmen.  Falls Knoten mit maximaler Knotenzahl  ge-		*/
		/* funden => Abbruch der Schleife.				*/
		{	Snode n;
			int min, nr;
			min = 1;
			L_for_node_list (searchlist,nr)
			{       n = L_list_entry(searchlist,nr);
				list = K_knoten_clique(n,min,attr_flags(n),inlist);
				if (list->curlen == maxknoten)
				{	L_close_list(savelist);
					savelist = list;
					break;
				}
				if (list->curlen > min)
				{	min = list->curlen-1;
					L_close_list(savelist);
					savelist = list;
				}
				else
					L_close_list(list);
			} L_end_for_node_list (searchlist,nr);
		}
		L_close_list(searchlist);
		return savelist;
	}


L_node_list C_find_clique_in_list(list)
L_node_list list;
	/*
	Berechnet die maximale Clique einer Knotenmenge. Als Eingabe wird
	diese Knotenmenge als Liste erwartet. Die Ausgabe erfolgt ebenfalls
	in einer Liste.
	*/
	{       int nr;
		/* Leerer Graph ? */
		if (list->curlen == 0)
			return(L_create_list(0));

		/* Ermittelt die Zahl der Kanten je Knoten innerhalb des */
		/* durch die Knotenliste list induzierten Teilgraphen.   */
		L_for_node_list(list,nr)
		{	Sedge e;
			int count = 0;
			Snode n = L_list_entry(list,nr);
			for_sourcelist(n,e)
			{	int nr2;
				L_for_node_list(list,nr2)
				{	if (L_list_entry(list,nr2) == e->tnode)
					{	count++;
						break;
					}
				} L_end_for_node_list(list,nr2);
			} end_for_sourcelist(n,e);
			attr_flags(n) = count;
		} L_end_for_node_list(list,nr);
		return search_clique(list);
	}




L_node_list C_find_clique ( g )
Sgraph g;
	/*
	Sucht in dem Graphen g eine maximale Clique heraus. Als Ergebnis wird
	eine Liste zurueckgeliefert, die diese Knoten enthaelt.
	*/
	{	/* Erstellen einer Liste mit den Knoten des Graphen */
		L_node_list list, result;
		Snode n;
		list = L_create_list(G_knoten_anzahl(g));
		for_all_nodes(g,n)
		{	L_append(list,n);
			attr_flags(n) = G_kanten_anzahl(n);
		} end_for_all_nodes(g,n);
		result = search_clique(list);
		L_close_list(list);
		return result;
	}


