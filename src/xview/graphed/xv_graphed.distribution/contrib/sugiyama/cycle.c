/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                          C  Y  C  L  E  .  C                                  */
/*                                                                               */
/*********************************************************************************/

#include "std.h"
#include "sgraph.h"

/*********************************************************************************/
/* Fuer jeden Knoten n des Graphs werden in suc_list die Knoten gespeichert,     */
/* zu denen ein Pfad existiert. Falls n selbst in dieser Liste enthalten ist,    */
/* so ist dies der Beweis fuer Zykel im Graph.				         */
/*********************************************************************************/

Local struct node_rec {
	Snode node;
	struct node_rec *next;
} *suc_list;

Local bool is_element(n,list)
Snode n;
struct node_rec *list;

{	
	if (list == NULL)
		return (FALSE);
	else if (list->node == n)
		return (TRUE);
	else 
		return (is_element(n, list->next));
}

Local void insert(n)
Snode n;
{
	struct node_rec *new_n;
	Sedge e;
	
	if (is_element(n, suc_list) == FALSE)
	{
		new_n = (struct node_rec *)malloc(sizeof(struct node_rec));
		new_n->node = n;
		new_n->next = suc_list;
		suc_list = new_n;
		for_sourcelist(n,e)
			insert(e->tnode, suc_list);
		end_for_sourcelist(n,e);
	}
}

Local void delete_list()
{
	struct node_rec *el;
	while (suc_list != NULL)
	{
		el = suc_list;
		suc_list = suc_list->next;
		free(el);
	}
}


Local void make_suc_list(n)
Snode n;
{
	
	Sedge e;

	delete_list();
	for_sourcelist(n,e)
		insert(e->tnode);
	end_for_sourcelist(n,e);
}

Global bool find_cycles(g)
Sgraph g;
{
	Snode n;

	suc_list = NULL;	
	for_all_nodes(g,n)
		make_suc_list(n);
		if (is_element(n, suc_list))
			return(TRUE);
	end_for_all_nodes(g,n);
	return(FALSE);
}


/******* E n d e   C Y C L E . C ****************************************************/



