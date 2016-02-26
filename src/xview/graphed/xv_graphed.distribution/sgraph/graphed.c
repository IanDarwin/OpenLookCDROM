/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

/* Sgraph includes	*/

#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"



/************************************************************************/
/*									*/
/*		Interface Procedures Sgraph --> GraphEd			*/
/*									*/
/************************************************************************/


Graphed_graph	graphed_graph (sgraph)
Sgraph		sgraph;
{
	return	(Graphed_graph)(sgraph->graphed);
}


Graphed_node	graphed_node (snode)
Snode		snode;
{
	return	(Graphed_node)(snode->graphed);
}


Graphed_edge	graphed_edge (sedge)
Sedge		sedge;
{
	return	(Graphed_edge)(sedge->graphed);
}
