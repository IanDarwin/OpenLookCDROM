/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                             H  I  E  R  .  C                                  */
/*                                                                               */
/*********************************************************************************/

#include "def.h"
#include "slist.h"
					
Global void prepare(g)
Sgraph g;

/* Initialisierungen im Graph g */

{
	Snode n;
	Sedge e;
	for_all_nodes(g,n)
		attr_flags(n) = 0;
		n->y = 0;
		for_sourcelist(n,e)
			attr_flags(e) = 0;
		end_for_sourcelist(n,e);
	end_for_all_nodes(g,n);
}


Local void mark_as_dummy(n)
Snode n;
{
	attr_flags(n) = 1;
}


Global bool is_dummy(node)
Snode node;
{
	return (attr_flags(node) == 1);
}


Local void mark(e)
Sedge e;

/* markiert eine Kante (um sie spaeter zu loeschen) */

{
	attr_flags(e) = 1;
}


Local bool marked(e)
Sedge e;
{
	return(attr_flags(e) == 1);
}


Local delete_marked_edges(n)
Snode n;
{
	
	bool weiter;
	Sedge e, e1;
	weiter = (((e1) = (n)->slist) != empty_edge);
	while (weiter) {
		e = e1;
		weiter = ((e1 = (e)->ssuc) != (n)->slist); 
		if (marked(e)) remove_edge(e);
		}		
}


Local void set_level(n,i)
Snode n;
int i;

/* weist dem Knoten n das Level i zu, falls er nicht schon ein groesseres Level hat */
/* Die direkten Nachfolger von n bekommen rekursiv das Level i+1 */

{
	Sedge e;
	if (i > maxlevel)
		maxlevel = i;
	if (level(n) <= i)	
	{
		level(n) = i;
		for_sourcelist(n,e)
			set_level(e->tnode,i+1);
		end_for_sourcelist(n,e);
	}
}


Global void add_dummies(g)
Sgraph g;

/* in die Kanten ueber mehrere Level werden Dummy-Knoten eingefuegt */

{
	Snode n, new_n;
	Sedge e, e1, e2;

	for_all_nodes(g,n)
		for_sourcelist(n,e)
			if(level(e->tnode)-level(n)>1)
				{
				new_n = make_node(g,make_attr(ATTR_DATA,NULL));
				mark_as_dummy(new_n);
				level(new_n)=level(n)+1;
				
				e1 = make_edge(n,new_n,make_attr(ATTR_DATA,NULL));
				e1->graphed = e->graphed;
				set_edgelabel(e1,e->label);
				
				e2 = make_edge(new_n,e->tnode,make_attr(ATTR_DATA,NULL));
				e2->graphed = e->graphed;
				set_edgelabel(e2,e->label);
				mark(e);

				}
			end_for_sourcelist(n,e);
		delete_marked_edges(n);
	end_for_all_nodes(g,n);
}

	
Global void init_positions(g)
Sgraph g;

/* initialisiert die waagrechten Positionen der Knoten */
/* und berechnet die Anzahl der Knoten pro Level (nodes_of_level) */

{
	Snode n;
	
	/* nodes_of_level mit 0 initialisieren */
	{
		int i;
		for (i=0;i<SIZE; i++)
		nodes_of_level[i]=0;
	}

	for_all_nodes(g,n)
		n->x = (++(nodes_of_level[level(n)]));
	end_for_all_nodes(g,n);
}


Global void make_hierarchy(g)
Sgraph g;
{
	Snode n;

	for_all_nodes (g,n)
	{
		if ((n->tlist) == empty_edge)		
			set_level(n,0);
	} end_for_all_nodes (g,n);
}


Global void set_horizontal_positions(g, unit)
Sgraph g;
int    unit;
{
	Snode n;
	for_all_nodes(g,n)
		n->x *= unit;
	end_for_all_nodes(g,n)
}


Global void set_vertical_positions(g, unit)
Sgraph g;
int    unit;
{
	Snode n;
	for_all_nodes(g,n)
		n->y += 1;
		n->y *= unit;
	end_for_all_nodes(g,n)
}


Global void remove_dummies(g)
Sgraph g;

/* entfernt die Dummy-Knoten und stellt die langen Kanten wieder her, */
/* wobei die Positionen der Dummies in Attributes gespeichert werden  */

{
	Snode n, n1, n2;
	Sedge e, new_e;
	Slist remove_node_slist, l;
	Attributes a;
	int *pos;
	int i;

	/* Kantenattribute initialisieren */
	
	for_all_nodes(g,n)
		for_sourcelist(n,e)
			attr_data_of_type(e,int *) = (int *)NULL;
		end_for_sourcelist(n,e);
	end_for_all_nodes(g,n);

	/* Kanten ueber mehr als ein Level wiederherstellen */

	for_all_nodes(g,n)
		if (is_dummy(n) == FALSE)
		for_sourcelist(n,e)
			 if (attr_data_of_type(e, int *) == (int *)NULL)
				if (is_dummy(e->tnode))
			  	{
					pos = (int *)calloc(2*maxlevel+3, sizeof(int));
					pos[0] = n->x;
					pos[1] = n->y;
					i = 2;
					n1 = e->tnode;
					do 
					{	
					/* Positionen aller nachfolgenden dummy-Knoten speichern */
						n2 = n1->slist->tnode;
						pos[i++] = n1->x;
						pos[i++] = n1->y;
						n1 = n2;	
					} while (is_dummy(n2));	
					pos[i++] = n2->x;
					pos[i++] = n2->y;
					pos[i] = 0;
					new_e = make_edge(n,n2,make_attr(ATTR_DATA, NULL));
					new_e->graphed = e->graphed;
					set_edgelabel(new_e,e->label);
					attr_data_of_type(new_e, int *) = pos;
				}
				else
				{
				
					pos = (int *)calloc(5, sizeof(int));
					pos[0] = n->x;
					pos[1] = n->y;
					n1 = e->tnode;
					pos[2] = n1->x;
					pos[3] = n1->y;
					pos[4] = 0;
					attr_data_of_type(e, int *) = pos;
				}
		end_for_sourcelist(n,e);
	end_for_all_nodes(g,n);
	
	/* dummy - Knoten loeschen, Slists MH 3/10/91 */

	remove_node_slist = empty_slist;
	for_all_nodes (g, n) {
		if (is_dummy(n)) {
			remove_node_slist = add_immediately_to_slist (remove_node_slist,
				make_attr (ATTR_DATA, (char *)n));
		}
	} end_for_all_nodes (g, n);
		
	if (remove_node_slist != empty_slist) {
		for_slist (remove_node_slist, l) {
			remove_node (attr_data_of_type (l, Snode));
		} end_for_slist (remove_node_slist, l);
		free_slist (remove_node_slist);
	}

}
/***** E n d e   H I E R . C *********************************************************/






