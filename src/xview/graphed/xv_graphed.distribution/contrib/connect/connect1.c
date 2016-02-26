/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source	   Connectivity tests	   Sep'91  Torsten Bachmann */

#include <std.h>
#include <slist.h>
#include <sgraph.h>


/****************************************************************************
 *									    *
 * Functions in this module:						    *
 *									    *
 *--------------------------------------------------------------------------*
 *  bool test_sgraph_connected(g)					    *
 *  Sgraph g;								    *
 *									    *
 *	A graph is connected, if between every two nodes exists a path.     *
 *	In directed graphs edge-directions will be ignored.		    *
 *	Extension: An empty graph is connected. 			    *
 *	Complexity: O(n)  with n = #edges * #nodes			    *
 *									    *
 *--------------------------------------------------------------------------*
 *  bool test_sgraph_strongly_connected(g)				    *
 *  Sgraph g;								    *
 *									    *
 *	A directed graph is strongly connected, if between every two nodes  *
 *	exists a directed path from every node to every other node.	    *
 *	For undirected graphs, test_sgraph_strongly_connected gives the     *
 *	same result as test_sgraph_connected.				    *
 *	Extension: An empty graph is strongly connected.		    *
 *	Complexity: O(n)  with n = #edges * #nodes			    *
 *                                                                          *
 *--------------------------------------------------------------------------*
 *  bool test_sgraph_biconnected(g)					    *
 *  Sgraph g;								    *
 *									    *
 *	A connected graph, in which a discretionary node can be removed and *
 *	which is still connected is called a biconnected graph. 	    *
 *	Extension: An empty graph is biconnected.			    *
 *	Complexity: O(n)  with n = #edges * #nodes			    *
 *									    *
 *--------------------------------------------------------------------------*
 *  bool test_sgraph_strongly_biconnected(g)				    *
 *  Sgraph g;								    *
 *									    *
 *	A directed graph is strongly biconnected, if the graph is strongly  *
 *	connected and still strongly connected, if any discretionary node   *
 *	is removed.							    *
 *	For undirected graphs, test_sgraph_strongly_biconnected gives the   *
 *	same result as test_sgraph_biconnected.				    *
 *	Complexity: O(n^2)  with n = #edges * #nodes			    *
 *									    * 
 ****************************************************************************/


static void dfs_out (n)
Snode n;
{
    Sedge e;
    attr_flags(n) = TRUE;
    for_sourcelist(n,e)
    {
	if (!attr_flags(e->tnode)) dfs_out(e->tnode);
    } end_for_sourcelist(n,e);
}
 

static void dfs_in_and_out (n)
Snode n;
{
    Sedge e;
    attr_flags(n) = TRUE;
    for_sourcelist(n,e)
    {
	if (!attr_flags(e->tnode)) dfs_in_and_out(e->tnode);
    } end_for_sourcelist(n,e);
    for_targetlist(n,e)
    {
	if (!attr_flags(e->snode)) dfs_in_and_out(e->snode);
    } end_for_targetlist(n,e);
}
 

/*--------------------------- connectivity test -----------------------------*/
bool test_sgraph_connected(g)
Sgraph g;
{
    Snode n;
    if (g == empty_sgraph || g->nodes == empty_node) return TRUE;
    for_all_nodes(g,n)
    {
        attr_flags(n) = FALSE;
    } end_for_all_nodes(g,n);

    /* A graph is connected, if from one node all other nodes are reachable. */
    if (g->directed)
	dfs_in_and_out (last_node_in_graph(g));
    else
	dfs_out (last_node_in_graph(g));

    for_all_nodes(g,n)
    {
	if (!attr_flags(n)) return FALSE;
    } end_for_all_nodes(g,n);
    return TRUE;
}

/*------------- routines for strongly and biconnectivity tests --------------*/
typedef struct numbers	/* used for biconnectivity and strongly connectivity */
{
	int dfnumber;	/* calculated in dfs_bi */
	int low;	/* calculated in dfs_bi_postorder */
	int visited;	/* needed for dfs_bi_postorder */
	int visited2;	/* needed for dfs_bi_check */
}
	*Numbers;	


static int init_attrs(g)
Sgraph g;
{
    Snode n;
    Numbers dummy;
    int i = 0;
    for_all_nodes(g,n)
    {
        dummy = (Numbers) malloc(sizeof (struct numbers) );
	n->attrs = make_attr(ATTR_DATA,dummy);
	dummy->dfnumber = 0;
	dummy->low = 0;  
	dummy->visited = FALSE;
	dummy->visited2 = FALSE;
	i++;
    } end_for_all_nodes(g,n);
    return i;   /* Value needed in test_sgraph_strongly_biconnected */
}


static void clean_attrs(g)
Sgraph g;
{
    Snode n;
    for_all_nodes(g,n)
    {
    	free(attr_data_of_type(n, Numbers));
    	n->attrs = make_attr(ATTR_DATA, NULL);
    } end_for_all_nodes(g,n);
}



/*------------------------------ SnodeStack ---------------------------------*/
typedef struct snodeStack
{
    struct snodeStack	*next;
    Snode		snode;
}
    *SnodeStack;

#define empty_snode_stack ((SnodeStack) NULL)


static
int snode_stack_isempty(st)
SnodeStack st;
{
    return st == empty_snode_stack;
}


/* check, if n is somewhere in the stack st */
static bool is_on_snode_stack(st, n)
SnodeStack st;
Snode n;
{
    while (st != empty_snode_stack)
    {
    	if (st->snode == n) return TRUE;
    	st = st->next;
    }
    return FALSE;
}


static
SnodeStack snode_stack_push(st, n)
SnodeStack st;
Snode n;
{
    SnodeStack dummy;
    if (n != empty_snode)
    {
	dummy = (SnodeStack) malloc(sizeof(struct snodeStack));
	dummy->next = st;
	dummy->snode = n;
    }
    return dummy;
}

static
Snode snode_stack_top(st)
SnodeStack st;
{
    return st->snode;
}

static
SnodeStack snode_stack_pop(st)
SnodeStack st;
{
    SnodeStack dummy;
    dummy = st;
    if (st != empty_snode_stack)
    {
	st = st->next;
	free(dummy);
    }
    return st;
}


/*---------------------- strongly connectivity test -------------------------*/
static int count, components;
static SnodeStack st;

static void searchc(n)		/* Derived from AHO p192 */
Snode n;
{
    Sedge e;
    attr_data_of_type(n, Numbers)->visited = TRUE;
    attr_data_of_type(n, Numbers)->dfnumber = count;
    attr_data_of_type(n, Numbers)->low = count;
    count++;
    st = snode_stack_push(st,n);
    for_sourcelist(n,e) {
	if (!attr_data_of_type(e->tnode, Numbers)->visited)
	{
	    searchc(e->tnode);
	    attr_data_of_type(n, Numbers)->low = minimum( attr_data_of_type(n, Numbers)->low,
						          attr_data_of_type(e->tnode, Numbers)->low );
	}
	else
	{
	    if ( (attr_data_of_type(e->tnode, Numbers)->dfnumber < attr_data_of_type(n, Numbers)->dfnumber) &&
		 (is_on_snode_stack(st, e->tnode)) )
		attr_data_of_type(n, Numbers)->low = minimum( attr_data_of_type(e->tnode, Numbers)->dfnumber,
							      attr_data_of_type(n, Numbers)->low );
	}
    } end_for_sourcelist(n,e);
    if (attr_data_of_type(n, Numbers)->low == attr_data_of_type(n, Numbers)->dfnumber)
    {
	Snode x;
	do {
	    x = snode_stack_top(st);
	    st = snode_stack_pop(st);
	    attr_data_of_type(x, Numbers)->visited2 = TRUE;
	} while (n != x);
	components++;
    }
}

bool test_sgraph_strongly_connected(g)
Sgraph g;
{
    Snode n;
    bool result;
    if (g == empty_sgraph || g->nodes == empty_node) return TRUE;
    if (!g->directed)
    	return test_sgraph_connected(g);
    init_attrs(g);
    count = 1;
    st = empty_snode_stack;
    components = 0;
    searchc(g->nodes);

    /* the graph is strongly connected, if one strongly connected component */
    /* can be found, and all nodes are covered by this component. */
    if (components == 1 && st == empty_snode_stack)
    {
	result = TRUE;
        for_all_nodes(g,n) {
	    if ( !(attr_data_of_type(n, Numbers))->visited2 )
	    {
		result = FALSE;
		break;
	    }
	} end_for_all_nodes(g,n);
    } else result = FALSE;

    clean_attrs(g);
    while (st != empty_snode_stack)
    {
    	SnodeStack dummy;
	dummy = st;
	st = st->next;
	free (dummy);	
    }
    return result;
}


/*-------------------------- biconnectivity test ----------------------------*/
static bool dfs_bi (n,nextnumber)
Snode n;
int *nextnumber;
{
    Sedge e;
    int childs = 0;
    attr_data_of_type(n, Numbers)->dfnumber = ++(*nextnumber);
    for_sourcelist(n,e)
    {
	if (attr_data_of_type(e->tnode, Numbers)->dfnumber == 0)
	{
	    dfs_bi(e->tnode, nextnumber);
	    childs++;
	}
    } end_for_sourcelist(n,e);
    if (n->graph->directed)
    {	
        for_targetlist(n,e)
        {
	    if (attr_data_of_type(e->snode, Numbers)->dfnumber == 0)
	    {
		dfs_bi(e->snode, nextnumber);
	        childs++;
	    }
        } end_for_targetlist(n,e);
    }
    return childs > 1;
    /*
     * If the root of the dfs-tree has more than one child, 
     * the graph can not be biconnected.
     */
}
 

static int dfs_bi_postorder (n, father)
Snode n;
Snode father;
{
    int lowmin, m;
    Sedge e;
    attr_data_of_type(n, Numbers)->visited = TRUE;
    lowmin = attr_data_of_type(n, Numbers)->dfnumber;
    for_sourcelist(n,e)
    {
	if (!(attr_data_of_type(e->tnode, Numbers)->visited))
	{
	    m = dfs_bi_postorder(e->tnode,n);
	    lowmin = minimum(lowmin, m);
	}
	else
	{
	    if (e->tnode != father)
		lowmin = minimum(lowmin, attr_data_of_type(e->tnode, Numbers)->dfnumber);
	}
    } end_for_sourcelist(n,e);
    if (n->graph->directed)
    {   
        for_targetlist(n,e)
        {
	    if (!attr_data_of_type(e->snode, Numbers)->visited) 
	    {
	    	/* Goes down in the dfs tree */
	    	m = dfs_bi_postorder(e->snode, n);
		lowmin = minimum(lowmin, m);
	    }
	    else
	    {    /* Follow back edge, but filter out the edge to the parent */
	        if (e->tnode != father)
		    lowmin = minimum(lowmin, attr_data_of_type(e->snode, Numbers)->dfnumber);
	    }
	} end_for_targetlist(n,e);
    }
    attr_data_of_type(n, Numbers)->low = lowmin;
    return lowmin;
}
  
/*
 * returns FALSE, if an articulation point is found, 
 * i.e. TRUE, if the graph is biconnected 
 */
static bool dfs_bi_check (n)
Snode n;
{
    Sedge e;
    attr_data_of_type(n, Numbers)->visited2 = TRUE;
    for_sourcelist(n,e)
    {
	if (!attr_data_of_type(e->tnode, Numbers)->visited2)
	{
	    if (   attr_data_of_type(e->tnode, Numbers)->low 
	        >= attr_data_of_type(n,        Numbers)->dfnumber) return FALSE;
	    if (!dfs_bi_check(e->tnode)) return FALSE;
	}
    } end_for_sourcelist(n,e);
    if (n->graph->directed)
    {
        for_targetlist(n,e)
        {
	    if (!attr_data_of_type(e->snode, Numbers)->visited2)
	    {
	        if (   attr_data_of_type(e->snode, Numbers)->low 
	            >= attr_data_of_type(n,        Numbers)->dfnumber) return FALSE;
	        if (!dfs_bi_check(e->snode)) return FALSE;
	    }
        } end_for_targetlist(n,e);
    }
    return TRUE;
}
 
static bool bi_check(n)                   /* Don`t check root-node */
Snode n;
{
    Sedge e;
    attr_data_of_type(n, Numbers)->visited2 = TRUE;
    for_sourcelist(n,e)
    {
         if (!dfs_bi_check(e->tnode)) return FALSE;
    } end_for_sourcelist(n,e);
    if (n->graph->directed)
    {
        for_targetlist(n,e)
        {
            if (!dfs_bi_check(e->snode)) return FALSE;
        } end_for_targetlist(n,e);
    }
    return TRUE;
}


    /*
     * Algorithm from Aho, Hopcraft, Ullman, Data Structures and Algorithms (p. 245)
     *
     * 1) Perform a depth-first search of the graph, computing dfnumber[v] for each
     *    vertex. In essence dfnumber orders the vertices as in a preorder traversal
     *    the depth-first spanning tree.
     *
     * 2) For each vertex v, compute low[v], which is the smallest dfnumber of v or
     *    of any vertex w reachable from v by following down zero or more tree edges
     *    to a descendant x of v (x may be v) and then following a back edge (x,w).
     *    We compute low[v] for all vertices by visiting the vertices in a postorder
     *    traversal. When we process v, we have computed low[y] for every child y of v.
     *    We take low[v] to be the minimum of
     *    - dfnumber[v],
     *    - dfnumber[z] for any vertex z for which there is a back edge (v,z),
     *    - low[y] for any child y of v.
     *
     * 3) Now we find the articulation points as follows:
     *    - The root is an articulation point if and only if it has two or more children.
     *    -  vertex v other than the root is an articulation point if and only if there
     *       is some child w of v such that low[w] >= dfnumber[v]. In this case v discon-
     *       nects w and its descendants from the rest of the graph. Conversely, if
     *       low[w] < dfnumber[v], then there must be a way to get from w down the tree
     *       and back to a proper ancestor of v (the vertex whose dfnumber is low[w]),
     *       and therefore deletion of v does not disconnect w or its descendants from 
     *       the rest of the graph.
     */
    

bool test_sgraph_biconnected(g)
Sgraph g;
{
    Snode n;
    int nr = 1;
    bool result;
    
    if (g == empty_sgraph || g->nodes == empty_node) return TRUE;
    init_attrs(g);
    nr = 0;
    if (dfs_bi (first_node_in_graph(g), &nr))
    {
	/* root is an articulation point */
	clean_attrs(g);
	return FALSE;
    }

    /* For step 2), we require simple connectivity. */
    for_all_nodes(g,n)
    {
     	if (attr_data_of_type(n, Numbers)->dfnumber == 0)
     	{    
	    clean_attrs(g);
	    return FALSE;
	}
    } end_for_all_nodes(g,n);

    /* Step 2) of algorithm */
    dfs_bi_postorder(first_node_in_graph(g), first_node_in_graph(g));

    /* Step 3) of algorithm  */
    result = bi_check(first_node_in_graph(g));
    clean_attrs(g);
    return result;
}

/*---------------------- strongly biconnectivity test -----------------------*/

static bool test_sgraph_without_node_strongly_connected(node)
Snode node;
{   
    int maxcount;
    bool result;
    Snode n;
    Sgraph g = node->graph;
    if (node == node->suc) return TRUE;	/* graph has only one node */
    maxcount = init_attrs(g); 		/* count can't be greater than #nodes */
    
    attr_data_of_type(node, Numbers)->visited  = TRUE;
    attr_data_of_type(node, Numbers)->visited2 = TRUE;
    attr_data_of_type(node, Numbers)->dfnumber = maxcount+1;
    attr_data_of_type(node, Numbers)->low      = maxcount+1;
    
    st = empty_snode_stack;
    count = 1;				/* global variable needed in searchc */
    components = 0;			/* --- " --- */
    searchc(node->suc);

    /* the graph is strongly connected, if one strongly connected component */
    /* can be found, and all nodes are covered by this component. */
    if (components == 1 && st == empty_snode_stack)
    {
	result = TRUE;
        for_all_nodes(g,n) {
	    if ( !(attr_data_of_type(n, Numbers))->visited2 )
	    {
		result = FALSE;
		break;
	    }
	} end_for_all_nodes(g,n);
    } else result = FALSE;

    clean_attrs(g);
    while (st != empty_snode_stack)
    {
    	SnodeStack dummy;
	dummy = st;
	st = st->next;
	free (dummy);	
    }
    return result;
}


bool test_sgraph_strongly_biconnected(g)
Sgraph g;
{
    Snode n;
    bool result;
    if (g == empty_sgraph || g->nodes == empty_node) return TRUE;
    if (!g->directed)
    	return test_sgraph_biconnected(g);
    if (!test_sgraph_strongly_connected(g)) return FALSE;
    if (!test_sgraph_biconnected(g)) return FALSE;
    for_all_nodes(g, n)
    {
    	if (!test_sgraph_without_node_strongly_connected(n)) return FALSE;
    } end_for_all_nodes (g,n);
    return TRUE;
}
    
