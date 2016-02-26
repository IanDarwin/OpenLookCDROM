/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                               P  O  S  .  C                                   */
/*                                                                               */
/*********************************************************************************/

#include "def.h"

#define MD 1 	/* minimaler horizontaler Abstand zwischen 2 Knoten */
		
typedef struct node_data{
	Snode	node;
	int	priority;
	bool	done;
}Node_data; 

Local Node_data *nl;



Local int upper_connectivity(node)
Snode node;
{	
	int result;
	Sedge edge;
	
	result = 0;
	for_targetlist(node,edge)
		result++;
	end_for_targetlist(node,edge);
	return(result);
}

	
Local int lower_connectivity(node)
Snode node;
{
	int result;
	Sedge edge;
	
	result = 0;	
	for_sourcelist(node,edge)
		result++;
	end_for_sourcelist(node,edge);
	return(result);
}


Local int upper_barycenter(node)
Snode node;
{
	int result = 0;
	Sedge edge;
		
	for_targetlist(node,edge)
		result += (edge->snode->x);
	end_for_targetlist(node,edge);
	if (result == 0)
		return (0.0);
	else 	
		return(result/upper_connectivity(node));
}

	
Local int lower_barycenter(node)
Snode node;
{
	int result = 0;
	Sedge edge;
		
	for_sourcelist(node,edge)
		result += (edge->tnode->x);
	end_for_sourcelist(node,edge);
	if (result == 0) 
		return(0.0);
	else 
		return(result/lower_connectivity(node));
}


Local 	void	sort(n)

/* sortiert nl nach den horizontalen Knotenpositionen */

int n; /* Zahl der Elemente von nl */
{
	int i, j;
	Node_data h;
	for (j = n-1; j > 0; j--)
		for (i = 0; i < j; i++)
			if (nl[i].node->x > nl[i+1].node->x)
			{
				/* Elemente vertauschen */
				h = nl[i];
				nl[i] = nl[i + 1];
				nl[i+1] = h;
   			}
}


Local 	void	make_node_list_up(g, l)
Sgraph	g;
int	l;
{
	extern bool is_dummy();
	Snode n;
	int i = 0;

	for_all_nodes(g,n)
		if (level(n) == l)
		{
			nl[i].node = n;
			nl[i].done = FALSE;
			if (is_dummy(n))
				nl[i].priority = nodes_of_level[l+1] + 1;
			else
				nl[i].priority = lower_connectivity(n);
			i++;
		}
	end_for_all_nodes(g,n);
	sort(nodes_of_level[l]);
}


Local 	void	make_node_list_down(g, l)
Sgraph	g;
int	l;

{
	extern bool is_dummy();
	Snode n;
	int i = 0;

	for_all_nodes(g,n)
		if (level(n) == l)
		{
			nl[i].node = n;
			nl[i].done = FALSE;
			if (is_dummy(n))
				nl[i].priority = nodes_of_level[l-1] + 1;
			else
				nl[i].priority = upper_connectivity(n);
			i++;
		}
	end_for_all_nodes(g,n);
	sort(nodes_of_level[l]);
}		

	
Local	int	find_next(n)

/* liefert den Index des Elements mit der hoechsten Prioritaet, das noch nicht bearbeitet wurde */

int n; /* Anzahl der Knoten in nl */
{
	int index, i;
	int highest_priority = 0;
		
	for (i = 0; i < n; i++)
		if ((nl[i].priority >= highest_priority) && (nl[i].done == FALSE))
		{
			index = i;
			highest_priority = nl[i].priority;
		}
	return(index);
}


Local	void	_down(l)

int l; /* aktuelles Level */
{
	int i; /* Zaehlvariable fuer die Anzahl der bereits behandelten Knoten */
	int index; /* Index in nl des aktuellen Knoten */
	int j; /* Laufvariable: Indizes der zu nl[index] benachbarten Knoten */
	int optimal_position;
	int distance;
	int possible_distance;

	for (i = 0; i < nodes_of_level[l]; i++)
	{	
		index = find_next(nodes_of_level[l]);
		if ((optimal_position = upper_barycenter(nl[index].node)) == 0)
			optimal_position = nl[index].node->x;
		if (optimal_position < nl[index].node->x)
		{
			distance = nl[index].node->x - optimal_position;
			
			/* Aus den Abstaenden der benachbarten noch nicht behandelten */
			/* Knoten wird berechnet, wie weit eine Verschiebung maximal  */
			/* moeglich ist. */

			possible_distance = 0;
			j = index;
			do
			{
				if (j > 0)
					possible_distance += nl[j].node->x
					 - nl[j-1].node->x - MD;
				else /* j == 0 , d.h. kein Knoten links */
					possible_distance += nl[j].node->x - MD;
				j--;
			}
			while ((j >= 0) && !(nl[j].done));

			if (possible_distance < distance)
				distance = possible_distance;
			
			/* Der Knoten nl[index] sowie evtl. benachbarte Knoten werden verschoben */

			j = index;
			while (distance > 0)
			{
				int d;
				int k; 
				
				if (j == 0)
					d = distance;
				else
					d = minimum(nl[j].node->x 
						- nl[j-1].node->x - MD, distance);
				for (k = j; k <= index; k++)
					nl[k].node->x -= d;
				j--;
				distance -= d;
			}
		}

		else /* optimal_position >= nl[index].node->x */
		{	
			distance = optimal_position - nl[index].node->x;
			
			possible_distance = 0;
			j = index;
			do
			{
				if (j < nodes_of_level[l]-1)
					possible_distance += nl[j+1].node->x
						- nl[j].node->x - MD;
				else /* j == nodes_of_level[l]-1, d.h. kein Knoten rechts */
					possible_distance += distance;
				j++;
			}
			while ((j < nodes_of_level[l]) && !(nl[j].done));
			
			if (possible_distance < distance)
				distance = possible_distance;

			j = index;
			while (distance > 0)
			{
				int d;
				int k; 
				
				if (j == nodes_of_level[l]-1)
					d = distance;
				else
					d = minimum(nl[j+1].node->x - nl[j].node->x - MD, distance);
				for (k = index; k <= j; k++)
					nl[k].node->x += d;
				j++;
				distance -= d;
			}
		}
		nl[index].done = TRUE;
	}
}



Local	void	_up(l)
int l; /* aktuelles Level */
{
	int i; /* Zaehlvariable fuer die Anzahl der bereits behandelten Knoten */
	int index; /* Index in nl des aktuellen Knoten */
	int j; /* Laufvariable: Indizes der zu nl[index] benachbarten Knoten */
	int optimal_position;
	int distance;
	int possible_distance;

	for (i = 0; i < nodes_of_level[l]; i++)
	{	
		index = find_next(nodes_of_level[l]);
		if ((optimal_position = lower_barycenter(nl[index].node)) == 0)
			optimal_position = nl[index].node->x;
		if (optimal_position < nl[index].node->x)
		{
			distance = nl[index].node->x - optimal_position;

			possible_distance = 0;
			j = index;
			do
			{
				if (j > 0)
					possible_distance += nl[j].node->x
						- nl[j-1].node->x - MD;
				else /* j == 0, d.h. kein Knoten links */
					possible_distance += nl[0].node->x - MD;
				j--;
			}
			while ((j >= 0) && !(nl[j].done));
						
			if (possible_distance < distance)
				distance = possible_distance;
			
			j = index;
			while (distance > 0)
			{
				int d;
				int k; 
				
				if (j == 0)
					d = distance;
				else
					d = minimum(nl[j].node->x - nl[j-1].node->x - MD, distance);
				for (k = j; k <= index; k++)
					nl[k].node->x -= d;
				j--;
				distance -= d;
			}
		}

		else /* optimal_position >= nl[index].node->x */
		{
			distance = optimal_position - nl[index].node->x;

			possible_distance = 0;
			j = index;
			do
			{
				if (j < nodes_of_level[l]-1)
					possible_distance += nl[j+1].node->x
						- nl[j].node->x - MD;
				else /* j == nodes_of_level[l]-1, d.h. kein Knoten rechts */
					possible_distance += distance;
				j++;
			}
			while ((j < nodes_of_level[l]) && !(nl[j].done));
			
			if (possible_distance < distance)
				distance = possible_distance;
			
			j = index;
			while (distance > 0)
			{
				int d;
				int k; 
				
				if (j == nodes_of_level[l]-1)
					d = distance;
				else
					d = minimum(nl[j+1].node->x - nl[j].node->x - MD, distance);
				for (k = index; k <= j; k++)
					nl[k].node->x += d;
				j++;
				distance -= d;
			}
		}
		nl[index].done = TRUE;
	}
}


Global	void	improve_positions(g)
Sgraph g;
{
	int t = maximum(1, maxlevel/2);
	int i;

	/* DOWN */
	for (i = 1; i<= maxlevel; i++)
	{
		nl = (Node_data *)calloc(nodes_of_level[i], sizeof(Node_data));
		make_node_list_down(g, i);
		_down(i);
		cfree(nl);
	}
	/* UP */
	for (i = maxlevel - 1; i >= 0; i--)
	{
		nl = (Node_data *)calloc(nodes_of_level[i], sizeof(Node_data));
		make_node_list_up(g, i);
		_up(i);
		cfree(nl);
	}
	/* DOWN */
	for (i = t; i<= maxlevel; i++)
	{
		nl = (Node_data *)calloc(nodes_of_level[i], sizeof(Node_data));
		make_node_list_down(g, i);
		_down(i);
		cfree(nl);
	}
}

/************** E n d e   P O S . C **********************************************/


