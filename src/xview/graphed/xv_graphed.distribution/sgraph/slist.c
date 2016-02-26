/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				slist.c					*/
/*									*/
/************************************************************************/

#include "std.h"
#include "slist.h"
#include "sgraph.h"

/************************************************************************/
/*									*/
/*		    GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Slist	new_slist           (attrs)				*/
/*	Slist	add_to_slist        (slist, attrs)			*/
/*	Slist	subtract_from_slist (slist, attrs)			*/
/*									*/
/*	Slist	add_slists          (g1,g2)				*/
/*	Slist	add_slists_disjoint (g1,g2)				*/
/*	Slist	subtract_slists     (g1,g2)				*/
/*									*/
/*	Slist	free_slist (slist)					*/
/*	Slist	copy_slist (slist)					*/
/*									*/
/*	int	slist_contains_exactly_one_element (slist)		*/
/*	Slist	contains_slist_element             (slist, attrs)	*/
/*	Slist	contains_slist_sgraph              (slist, attrs)	*/
/*	int	slist_intersects_slist             (slist1, slist2)	*/
/*									*/
/*	Slist	make_slist_of_sgraph (sgraph)				*/
/*	Slist	make_slist_of_sourcelist (snode)			*/
/*	Slist	make_slist_of_targetlist (snode)			*/
/*									*/
/*	int	size_of_slist (slist)					*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*		    LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	static	Slist	add_immediately_to_slist	(slist, snode)	*/
/*	static	Slist	subtract_immediately_from_slist	(slist, g)	*/
/*									*/
/************************************************************************/

/************************************************************************/
/*									*/
/*			Datenstruktur Slist				*/
/*									*/
/************************************************************************/
/*									*/
/*	'Slist' is a set of (pointers to) snodes.			*/
/*									*/
/*	typedef	struct	slist	{					*/
/*									*/
/*		Attra		attrs;					*/
/*		struct	slist	*pre,suc;				*/
/*									*/
/*	}								*/
/*									*/
/*	A slist is a double linked list of pointers to snodes :		*/
/*	snode	*the* snode						*/
/*	pre,suc	pointer to predecessor / successor			*/
/*									*/
/*======================================================================*/
/*									*/
/*	All procedures workingon slist do have their own memory		*/
/*	management.							*/
/*									*/
/************************************************************************/
/*									*/
/*	Slist	new_slist (snode)					*/
/*									*/
/*	Creates a new slist with snode as the only element.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	add_immediately_to_slist (slist, attrs)			*/
/*									*/
/*	Fuegt snode in die Gruppe slist ein. Falls slist == empty_slist,*/
/*	so wird eine komplette neue Gruppe erzeugt.			*/
/*	ACHTUNG : Es wird keine Abfrage dahingehend vorgenommen, ob der	*/
/*	Knoten bereits in der Gruppe enthalten ist.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	add_to_slist (slist, snode)				*/
/*									*/
/*	Wie oben, es wird aber nur dann eingefuegt, wenn der Knoten	*/
/*	nicht schon in der Gruppe enthalten war.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	subtract_immediately_from_slist (slist, g)		*/
/*									*/
/*	Entfernt das Element g (Gruppenelement !) aus slist.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	subtract_from_slist (slist, snode)			*/
/*									*/
/*	Entfernt snode aus slist (falls vorhanden).			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	add_slists (g1, g2)					*/
/*									*/
/*	Fuegt die Elemente von g2 zu g1 hinzu. ACHTUNG : Seiteneffekt	*/
/*	auf die Elemente von g1 !					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	add_slists_disjoint (g1, g2)				*/
/*									*/
/*	Bildet eine neue Gruppe, die aus der disjunkten Vereinigung von	*/
/*	g1 und g2 besteht.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	subtract_slists (g1, g2)				*/
/*									*/
/*	Entfernt alle in g2 enthaltenen Elemente aus g1. ACHTUNG :	*/
/*	Seiteneffekt auf die Elemente in g1 !				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	free_slist (slist)					*/
/*									*/
/*	Speicherfreigabe fuer die Elemente in slist. Die obigen		*/
/*	Prozeduren erledigen im uebrigen die Speicherverwaltung		*/
/*	selbsttaetig, so dass free_slist lediglich am Ende der Arbeit	*/
/*	fuer Aufraeumarbeiten benoetigt wird.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	copy_slist (slist)					*/
/*									*/
/*	Gibt eine vollstaendige Kopie von slist zurueck.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	slist_contains_exactly_one_element (slist)		*/
/*									*/
/*	Prueft, ob slist aus genau einem Knoten besteht.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	contains_slist_element (slist, attrs)			*/
/*									*/
/*	contains_slist_element bestimmt das Element von slist, das	*/
/*	attrs enthaelt. Falls es kein solches gibt, wird empty_slist	*/
/*	zurueckgegeben.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Rect	compute_rect_around_goup (slist)			*/
/*									*/
/*	Berechnet das die Gruppe umfassende Rechteck.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Slist	make_slist_of_sgraph (sgraph)				*/
/*									*/
/*	Bildet eine Gruppe, die alle Knoten von sgraph umfasst.		*/
/*									*/
/************************************************************************/



Slist		new_slist (attrs)
Attributes	attrs;
{
	Slist	new;
	
	new = (Slist)mymalloc (sizeof (struct slist));
	
	new->attrs = attrs;
	new->suc  = new;
	new->pre  = new;
	
	return	new;
}


Slist		add_immediately_to_slist (slist, attrs)
Slist		slist;
Attributes	attrs;
{
	/* Warning : no check about double elements	*/
	
	Slist	new;
	
	new = new_slist (attrs);
	if (slist != empty_slist) {
		new->suc      = slist;
		new->pre      = slist->pre;
		new->suc->pre = new;
		new->pre->suc = new;
	} else {
		slist = new;
	}
	
	return	slist;
}


Slist		add_to_slist (slist, attrs)
Slist		slist;
Attributes	attrs;
{
	if (contains_slist_element (slist, attrs) == empty_slist)
		return add_immediately_to_slist (slist, attrs);
	else
		return	slist;
}


Slist	subtract_immediately_from_slist (slist, g)
Slist	slist, g;
{
	if (g == empty_slist) {
		; /* nothing to do ... */
	} else if (slist->suc == slist) {
		/* einelementige Gruppe	*/
		slist = empty_slist;
	} else if (g == slist) {
		/* Anfangselement	*/
		slist->suc->pre = slist->pre;
		slist->pre->suc = slist->suc;
		slist = slist->suc;
	} else {
		g->pre->suc = g->suc;
		g->suc->pre = g->pre;
	}
	myfree (g);
	
	return slist;
}


Slist		subtract_from_slist (slist, attrs)
Slist		slist;
Attributes	attrs;
{
	Slist	g;
	
	if ((g = contains_slist_element (slist, attrs)) != empty_slist)
		return subtract_immediately_from_slist (slist, g);
	else
		return slist;
}


Slist	add_slists (g1, g2)
Slist	g1, g2;
{
	Slist	g;
	
	for_slist (g2, g) {
		g1 = add_to_slist (g1, g->attrs);
	} end_for_slist (g2, g);
	
	return	g1;
}


Slist	add_slists_disjoint (g1, g2)
Slist	g1, g2;
{
	Slist	g;
	Slist	disjoint_slist;
	
	disjoint_slist = empty_slist;
	
	for_slist (g1, g) {
		if (contains_slist_element (g2, g->attrs) == empty_slist)
			disjoint_slist = add_immediately_to_slist (
				disjoint_slist, g->attrs);
	} end_for_slist (g1, g);

	for_slist (g2, g) {
		if (contains_slist_element (g1, g->attrs) == empty_slist)
			disjoint_slist = add_immediately_to_slist (
				disjoint_slist, g->attrs);
	} end_for_slist (g2, g);
	
	return	disjoint_slist;
}


Slist	subtract_slists (g1, g2)
Slist	g1, g2;
{
	Slist	g;
	
	for_slist (g2, g) {
		g1 = subtract_from_slist (g1, g->attrs);
	} end_for_slist (g2, g);
	
	return g1;
}


void	free_slist (slist)
Slist	slist;
{
	Slist	suc, g;
	
	g = slist;
	if (slist != empty_slist) do {
		suc = g->suc;
		myfree (g);
		g = suc;
	} while (g != slist);
}


Slist	copy_slist (slist)
Slist	slist;
{
	Slist	g, copy;
	
	copy = empty_slist;
	
	for_slist (slist, g) {
		copy = add_to_slist (copy, g->attrs);
	} end_for_slist (slist, g);
	
	return copy;
}


int	slist_contains_exactly_one_element (slist)
Slist	slist;
{
	if (slist != empty_slist)
		return slist == slist->suc;
	else
		return FALSE;
}


Slist		contains_slist_element (slist, attrs)
Slist		slist;
Attributes	attrs;
{
	Slist	g;
	
	for_slist (slist, g) {
		if (memcmp ((char *)(&(g->attrs)), (char *)(&(attrs)), sizeof (union attributes)) == 0)
			return g;
	} end_for_slist (slist, g);
	
	return empty_slist;
}



int	slist_intersects_slist (slist1, slist2)
Slist	slist1, slist2;
{
	Slist	g1,g2;

	if (slist1 == empty_slist || slist2 == empty_slist)
		return FALSE;
	
	for_slist (slist1, g1) {
		if (contains_slist_element (slist2, g1->attrs) != empty_slist)
			return TRUE;
	} end_for_slist (slist1, g1);
	
	for_slist (slist2, g2) {
		if (contains_slist_element (slist1, g2->attrs) != empty_slist)
			return TRUE;
	} end_for_slist (slist2, g2);

	return FALSE;
}



Slist	make_slist_of_sgraph (sgraph)
Sgraph	sgraph;
{
	Snode	snode;
	Slist	slist_to_make;
	
	slist_to_make = empty_slist;
	
	for_all_nodes (sgraph, snode) {
		slist_to_make = add_to_slist (slist_to_make, make_attr (ATTR_DATA, snode));
	} end_for_all_nodes (sgraph, snode);
	
	return slist_to_make;
}



Slist	make_slist_of_sourcelist (snode)
Snode	snode;
{
	Sedge	sedge;
	Slist	slist_to_make;
	
	slist_to_make = empty_slist;
	
	for_sourcelist (snode, sedge) {
		slist_to_make = add_to_slist (slist_to_make, make_attr (ATTR_DATA, sedge->tnode));
	} end_for_sourcelist (snode, sedge);
	
	return slist_to_make;
}



Slist	make_slist_of_targetlist (snode)
Snode	snode;
{
	Sedge	sedge;
	Slist	slist_to_make;
	
	slist_to_make = empty_slist;
	
	for_targetlist (snode, sedge) {
		slist_to_make = add_to_slist (slist_to_make, make_attr (ATTR_DATA, sedge->snode));
	} end_for_targetlist (snode, sedge);
	
	return slist_to_make;
}


int	size_of_slist (slist)
Slist	slist;
{
	Slist	g;
	int	count = 0;
	
	for_slist (slist, g) {
		count ++;
	} end_for_slist (slist, g);
	
	return count;
}
