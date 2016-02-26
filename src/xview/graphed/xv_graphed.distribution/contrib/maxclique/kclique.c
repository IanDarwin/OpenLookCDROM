/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : kclique.c							*
 * Aufgabe : Bestimmt die groesste Clique, die an einem Knoten haengt.	*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 18.11.89							*
 ************************************************************************/

#include <std.h>
#include <sgraph.h>
#include "gfunc.h"
#include "liste.h"
#include "clique.h"		/* ACHTUNG: Verschraenkte Rekursion !!!	*/


static L_node_list determ, undeterm;
/*	determ enthaelt alle Knoten, die garantiert zur maximalen Clique
	gehoeren. undeterm enthaelt die Knoten, von denen die Zugehoerig-
	keit zur Clique noch nicht bestimmt werden konnte.
*/
static int min, max;
static Snode homenode;
/*      Diese drei Variablen erhalten die Parameter, mit denen K_knoten_clique
	aufgerufen wird. min enthaelt die Cliquengroesse, die mindestens er-
	reicht werden muss. max enthaelt die hoechstmoegliche Groesse. homenode
	ist der Ursprungsknoten fuer die Suche.
*/
static L_node_list base;
/*	base speichert die aktuelle Knotenbasis, auf der die Berechnungen
	stattfinden, d.h. den durch die Knotenliste induzierten Teilgraphen.
*/


static void listen_init()
	/*
	Ermittelt alle Knoten, die eine Verbindung zu homenode haben, und nimmt
	sie in die globale Liste undeterm auf. node selbst wird in determ ein-
	getragen, da der Startknoten immer zur Clique dazugehoert.
	*/
	{       Sedge e;
		undeterm = L_create_list(max);
		determ = L_create_list(min);
		L_append(determ,homenode);
		for_sourcelist(homenode,e)
		{       int nr;
			L_for_node_list(base,nr)
			{	if (L_list_entry(base,nr) == e->tnode)
					L_append(undeterm,e->tnode);
			} L_end_for_node_list(base,nr);
		} end_for_sourcelist(homenode,e);
	}


static bool has_edge_to_every_undeterm(node)
Snode node;
	/*
	Liefert TRUE, falls von allen Knoten aus undeterm Kanten zu node
	existieren.
	*/
	{       Sedge e;
		Snode n;
		int nr;
		bool endflag;
		L_for_node_list(undeterm, nr)
		{	if (node != (n=L_list_entry(undeterm,nr)))
			{	endflag = TRUE;
				for_sourcelist(node,e)
				{	if (e->tnode==n)
					{	endflag = FALSE;
						break;
					}
				} end_for_sourcelist(node,e);
				if (endflag)
					return FALSE;
			}
		} L_end_for_node_list(undeterm,nr);
		return TRUE;
	}


static void copy_determ_nodes()
	/*
	Kopiert alle Knoten von determ nach undeterm die zu den anderen Knoten
	aus undeterm eine Verbindung haben. Diese Knoten muessen immer in der
	maximalen Clique liegen.
	*/
	{	int nr;
		L_for_node_list(undeterm,nr)
		{       if (has_edge_to_every_undeterm(L_list_entry(undeterm,nr)))
			{       L_move(undeterm,determ,nr--);
				if (min<determ->curlen)
					min = determ->curlen;
			}
		} L_end_for_node_list(undeterm,nr);
	}



static bool has_edge_to_none_undeterm(node)
Snode node;
	/*
	Liefert TRUE, falls von Node zu keinem Knoten aus undeterm eine
	Kante existiert.
	*/
	{	Snode n;
		int nr;
		L_for_node_list(undeterm,nr)
		{	n = L_list_entry(undeterm,nr);
			if ( (n!=node) && G_exist_edge(n,node))
				return FALSE;
		} L_end_for_node_list(undeterm,nr);
		return TRUE;
	}


static void suspend_0_edge_nodes()
	/*
	Loescht aus undeterm alle Knoten, die zu keinem anderen Knoten aus
	undeterm eine Verbindung haben.
	*/
	{       int nr;
		L_for_node_list(undeterm,nr)
		{       if (has_edge_to_none_undeterm(L_list_entry(undeterm,nr)) )
			L_delete_entry(undeterm,nr--);
		} L_end_for_node_list(undeterm,nr);
	}


static void unvollst_clique()
	/*
	Bestimmt in der uebriggebliebenen Knotenmenge undeterm die maximale
	Clique. Die beste gefundene Knotenmenge wird nach determ uebernommen.
	Da nur noch Knoten in undeterm sind, die garantiert nicht alle eine
	Clique bilden, werden einige Spezialfaelle gesondert behandelt.
	*/
	{	L_node_list list;
		int nr;
		/* Einen Knoten zwischenspeichern, falls     */
		/* suspend_0_edge_nodes alle Knoten loescht. */
		Snode savenode = L_list_entry(undeterm,0);
		suspend_0_edge_nodes();
		if (determ->curlen >= max)
			return;
		switch (undeterm->curlen)
		{case 0:{       L_append(determ,savenode);
				break;
			}
		case 2:	{	L_move(undeterm,determ,0);
				L_move(undeterm,determ,1);
				break;
			}
		default:{       /* Globale Variablen "auf den Stack */
				/* bringen"			    */
				{	L_node_list s_det = determ;
					L_node_list s_undet = undeterm;
					Snode s_hnode = homenode;
					int s_min = min;
					int s_max = max;
					list = C_find_clique_in_list(undeterm);
					max = s_max;
					min = s_min;
					homenode = s_hnode;
					undeterm = s_undet;
					determ = s_det;
				}
				L_for_node_list(list,nr)
				{	L_move(list,determ,nr--);
				} L_end_for_node_list(list,nr);
				L_close_list(list);
				break;
			}
		} /* switch */
	}


L_node_list K_knoten_clique(node, mincliq, maxcliq, nodebase)
Snode node;
int mincliq;
int maxcliq;
L_node_list nodebase;
	/*
	Ermittelt die groesste Clique, die am Knoten node haengt, und liefert
	als Funktionsergebnis eine Liste mit diesen Knoten zurueck. maxcliq
	und mincliq sind Maximal- und Minimalwerte fuer die zu erreichende
	Clique und werden fuer Abbruchbedingungen genutzt, falls keine Verbes-
	serung mehr moeglich ist. In solch einem Fall wird eine leere Liste
	zurueckgegeben.
	*/
	{	max = maxcliq;
		min = mincliq;
		homenode = node;
		base = nodebase;
		if (min >= max)
			return L_create_list(0);
		listen_init();
		if (undeterm->curlen >= min)
		{	copy_determ_nodes();
			if (undeterm->curlen > 0)
				unvollst_clique();
		}
		else
		{	L_close_list(determ);
			determ = L_create_list(0);
		}
		L_close_list(undeterm);
		L_minimize_mem(determ);
		return determ;
	}


L_node_list K_knoten_list_clique(list)
L_node_list list;
	/*
	Erwartet als Eingabe eine Liste von Knoten, und berechnet die groes-
	ste Clique, die all diese Knoten enthaelt. Die uebergebenen Knoten-
	menge muss schon eine Clique bilden.
	!!!  N u r   i n   P l a n u n g  !!!
	*/
	{	return list;
		/*
		Arbeitet analog zu K_knoten_clique, bis	auf die Initiali-
		sierung, bei der alle Knoten aus list auf einmal in determ
		aufgenommen werden muessen.
		*/
	}
