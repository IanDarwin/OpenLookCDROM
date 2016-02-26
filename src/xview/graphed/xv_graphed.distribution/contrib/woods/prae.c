/******************************************************/
/*					  	      */
/*                     PRAE.C                         */
/*					     	      */
/*  (enthaelt alles, was vor basic.c benoetigt wird)  */
/*						      */
/******************************************************/

#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include "listen_macros.h"
#include "basic.h"

int int_dummy;

/************ Dump - Routinen **********************/

void print_list(file,l)
FILE *file;
LIST l;
{
X_Y n;
dfor_all_elements(l, (char *)n);
	printf("  %d %d  ",n->x,n->y );
dend_for_all_elements(l);
}

Global void print_xy_edge_attributes (file,e)
FILE *file;
Sedge e;
{
printf("\t {$ EL " );
print_list(file, XY_EDGE_COORD(e));
printf( "  $} ");
}

Global void print_xy_node_attributes (file,n)
FILE *file;
Snode n;
{
Sedge e;
dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
	int_dummy = fprintf(file, " %s %s ",e->snode->label,e->tnode->label);
dend_for_all_elements(XY_NODE_EDGES(n));
int_dummy = fprintf(file, "\n");
}
Global void message_xy_node_attributes (n)
Snode n;
{
Sedge e;
dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
	message(" %s %s ",e->snode->label,e->tnode->label);
dend_for_all_elements(XY_NODE_EDGES(n));
message("\n");
}

Global	void	print_sgraph (file,g, print_graph_attributes,
                                      print_node_attributes,
                                      print_edge_attributes)
FILE		*file;
Sgraph		g;
void		(*print_graph_attributes)();
void		(*print_node_attributes)();
void		(*print_edge_attributes)();
{
	Snode	n;
	Sedge	e;
	
	printf ("GRAPH \"%s\" = %s\n",
	         g->label, iif(g->directed, "DIRECTED", "UNDIRECTED"));
	print_graph_attributes (file, g);
	for_all_nodes (g,n) {
		printf ("%d ", n->nr);
		print_node_attributes (file, n);
		printf ( "\"%s\" ",  n->label);
		for_sourcelist (n,e) {
			if (g->directed ||
			    (e->snode->nr < e->tnode->nr)) {
				/* print edge				*/
				/* Caution : in undirected graphs, each*/
				/* edge is printed only once !		*/
				printf ("%d ", e->tnode->nr);
				print_edge_attributes (file, e);
				printf (" \"%s\" ", e->label);
			}
		} end_for_sourcelist (n,e);
		printf ( ";\n");
	} end_for_all_nodes (g,n);
	printf ( "END\n");
}

/******************* Alloc - Routinen ******************/

char *alloc_node_attrs()
{return (malloc(sizeof(xy_node_attributes)));}
char *alloc_edge_attrs()
{return (malloc(sizeof(xy_edge_attributes)));}

void init_edge_attrs(e)
xy_edge_attributes *e;
{
e->type = REALY;
e->horizontal = VERTICAL;
e->coordinates = NULL;
e->abscissa = 0;
}

void init_node_attrs(n,data,nr)
xy_node_attributes *n;
ADDRlist data;
int nr;
{
n->highedges = data;
n->edges = NULL;
n->data = 0;
n->oldnr = nr;
}

Global void init_xy_attrs(g)
Sgraph g;
{
Snode n;
Sedge e;
ADDRlist data;
Edgeline el;
for_all_nodes (g,n)
	{
	data = attr_data_of_type(n,ADDRlist);
	n->attrs.data = nil;
	n->attrs.data = alloc_node_attrs();
	init_node_attrs(XY_NODE_ATTRS(n),data,n->nr);
	for_sourcelist(n,e)
		{
		e->attrs.data = alloc_edge_attrs();
		init_edge_attrs(XY_EDGE_ATTRS(e));
		el=new_edgeline(n->x,n->y);
		(void)add_to_edgeline(el,OTHER_NODE(n,e)->x,OTHER_NODE(n,e)->y);
		edge_set(graphed_edge(e),ONLY_SET,EDGE_LINE,el,0);
	} end_for_sourcelist(n,e)
	n->nr = n->x;
	n->y = 0;
	n->x = 0;
} end_for_all_nodes(g,n)
}

Global Snode find_Snode_with_lowest_number(g)
Sgraph g;
{
Snode node,n;
n = g->nodes;
for_all_nodes(g,n)
	{
	if(n->nr < node->nr) node = n;
} end_for_all_nodes(g,n);
return(node);
}

/********** erzeugt Dummy-Kante ***********************/

Global Sedge createedge(snode,tnode)
Snode snode,tnode;
{
Sedge new_edge;
new_edge = (Sedge)malloc(sizeof(struct sedge));
new_edge->snode = snode;
new_edge->tnode = tnode;
new_edge->label = nil;
new_edge->spre = nil;
new_edge->ssuc = nil;
new_edge->tpre = nil;
new_edge->tsuc = nil;
new_edge->attrs.data = alloc_edge_attrs();
init_edge_attrs(XY_EDGE_ATTRS(new_edge));
XY_EDGE_TYPE(new_edge) = DUMMY;
return (new_edge);
}

/******************************************************************/
/*								  */
/* Folgende Routinen dienen zur richtigen Fortbewegung im Graphen */
/*								  */
/******************************************************************/

Global Sedge spre(n,e)
Sedge e;
Snode n;
{
Sedge f;
dfor_all_elements(XY_NODE_EDGES(n),(char *)f);
	if(f == e) return((Sedge)Forhilf->pre->elem);
dend_for_all_elements(XY_NODE_EDGES(n));
}
Global Sedge ssuc(n,e)
Sedge e;
Snode n;
{
Sedge f;
dfor_all_elements(XY_NODE_EDGES(n),(char *)f);
	if(f == e) return((Sedge)Forhilf->suc->elem);
dend_for_all_elements(XY_NODE_EDGES(n));
}
Global DLIST Ssuc(n,e)
Sedge e;
Snode n;
{
Sedge f;
dfor_all_elements(XY_NODE_EDGES(n),(char *)f);
	if(f == e) return((LIST)Forhilf->suc);
dend_for_all_elements(XY_NODE_EDGES(n));
}
Global DLIST otheredge(n,e)
Sedge e;
Snode n;
{
Sedge f;
dfor_all_elements(XY_NODE_EDGES(n),(char *)f);
	if(OTHER_NODE(n,e) == OTHER_NODE(n,f)) return((LIST)Forhilf);
dend_for_all_elements(XY_NODE_EDGES(n));
}
Global Sedge OTHEREDGE(n,e)
Sedge e;
Snode n;
{
DLIST liste;
/*if((!n->graph->directed)&&(XY_EDGE_TYPE(e) == REALY)) return(e->tsuc);*/
liste = (LIST)otheredge(n,e);
return((Sedge)liste->elem);
}

/***********************************************************/
/*							   */
/* Mit folgenden zwei Prozeduren werden Dummy-Kanten in    */
/*      interne Struktur des Graphen eingebaut             */
/*							   */
/***********************************************************/

Global void make_dummy_right(n,e,nr)
Sedge e;
Snode n;
int nr;
{
DLIST liste;
Sedge dummy,f;
Snode fsnode,dummynode;
int i;
DINIT_LIST(liste);
f = e;
fsnode = OTHER_NODE(n,e);
for(i = 0;i<nr;i++) 
	{
	f = ssuc(fsnode,f);
	fsnode = OTHER_NODE(fsnode,f);
	f = OTHEREDGE(fsnode,f);
	}
dummynode = OTHER_NODE(fsnode,ssuc(fsnode,f));
if(dummynode != n)
	{
	dummy = createedge(n,fsnode);
	liste = XY_NODE_EDGES(n);
	XY_NODE_EDGES(n) = (DLIST)otheredge(n,e);
	DPUSH(XY_NODE_EDGES(n),dummy);
	XY_NODE_EDGES(n) = liste;
	dummy = createedge(fsnode,n);
	liste = XY_NODE_EDGES(fsnode);
	XY_NODE_EDGES(fsnode) = (DLIST)Ssuc(fsnode,f);
	DPUSH(XY_NODE_EDGES(fsnode),dummy);
	XY_NODE_EDGES(fsnode) = liste;
	}
}
Global void make_dummy_left(n,e,nr)
Sedge e;
Snode n;
int nr;
{
DLIST liste;
Sedge dummy,f;
Snode fsnode,dummynode;
int i;
DINIT_LIST(liste);
f = e;
fsnode = OTHER_NODE(n,e);
for(i = 0;i<nr;i++) 
	{
	f = spre(fsnode,f);
	fsnode = OTHER_NODE(fsnode,f);
	f = OTHEREDGE(fsnode,f);
	}
dummynode = OTHER_NODE(fsnode,spre(fsnode,f));
if(dummynode != n)
	{
	dummy = createedge(n,fsnode);
	liste = XY_NODE_EDGES(n);
	XY_NODE_EDGES(n) = (DLIST)Ssuc(n,OTHEREDGE(n,e));
	DPUSH(XY_NODE_EDGES(n),dummy);
	XY_NODE_EDGES(n) = liste;
	dummy = createedge(fsnode,n);
	liste = XY_NODE_EDGES(fsnode);
	XY_NODE_EDGES(fsnode) = (DLIST)otheredge(fsnode,			
			OTHEREDGE(OTHER_NODE(fsnode,f),f));
	DPUSH(XY_NODE_EDGES(fsnode),dummy);
	XY_NODE_EDGES(fsnode) = liste;
	}
}

/************************************************************/
/*							    */
/* biconnect wandert alle Flaechen im Graphen aus, sucht    */
/* sich die Groesste und legt sie als 'outer face' um den   */
/* Graphen. Mit einigen Zeilen Ergaenzung ueberprueft       */
/* biconnect den Graphen auf zweifachen Zusammenhang  oder  */
/* kann diese durch Einfuegen von Kanten herstellen.        */
/*       						    */
/************************************************************/

Global Sgraph biconnect(g)
Sgraph g;
{
Snode n,cursornode,Maxnode,dummynode;
Sedge e,dummy,cursor,Maxedge;
int Maxcount = 1,face = 1,i,count;
for_all_nodes(g,n)
	{
	dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
		if((XY_EDGE_TYPE(e) == REALY)&&(XY_EDGE_ABS(e) == 0))
			{
			count = 1;
			cursor = e;
			XY_EDGE_ABS(e) = face;
			cursornode = OTHER_NODE(n,e);
			while(cursornode != n)
				{ 
				cursor = (Sedge)(otheredge(cursornode,cursor))
						->suc->elem;
				XY_EDGE_ABS(cursor) = face;
				cursornode = OTHER_NODE(cursornode,cursor);
				count++;
				}
			if(count > Maxcount)
				{
				Maxcount = count;
				Maxedge = e;
				Maxnode = n;
				}
			face++;
			}
	dend_for_all_elements(XY_NODE_EDGES(n));
} end_for_all_nodes(g,n);
if(Maxcount > 3)
	{
	/*if(Maxnode == Maxedge->tnode)
		Maxedge = OTHEREDGE(Maxnode,Maxedge);*/
	dummynode = Maxnode;
	dummy = Maxedge;
	Maxnode = OTHER_NODE(Maxnode,Maxedge);
	g->nodes = Maxnode;
	for(i = 0;i< (Maxcount-1)/2;i++) 
		{
		dummy = spre(dummynode,dummy);
		dummynode = OTHER_NODE(dummynode,dummy);
		dummy = OTHEREDGE(dummynode,dummy);
		}
        {
                Attributes empty_attrs;
                empty_attrs.data = NULL;
	        e = make_edge(Maxnode,dummynode,empty_attrs);
	}
	g->nodes->slist = e;
	mein_st_number(g);
	remove_edge(e);
	make_dummy_left(Maxnode,Maxedge,((Maxcount-1)/2));
	XY_NODE_EDGES(Maxnode) = (DLIST)(otheredge(Maxnode,Maxedge))->suc;
	for_all_nodes(g,n)
		{
		n->y = -1;
	} end_for_all_nodes(g,n)
	ordinate1(g);
	}
return(g);
}

Global Sedge reverseedge(n,e)
Snode n;
Sedge e;
{
Sedge g,f = e;
if(!e->snode->graph->directed)return(e->tsuc);
	else 
	{
	if(n == e->tnode)
		{
		for_sourcelist(n,g)
			{
			if(g->tnode == e->snode)f = g;
		} end_for_sourcelist(n,g)
	} else 	{
		for_targetlist(n,g)
			{
			if(g->snode == e->tnode)f = g;
		} end_for_targetlist(n,g)
	}
	return(f);
	}
}
Global Sedge dreverseedge(n,e)
Snode n;
Sedge e;
{
if(!e->snode->graph->directed)return(e->tsuc);
	else 
	return(e);
}

/*******************************************************/
/* Mit completelist erzeuge ich aus der nur teilweisen */
/* Liste der Kanten um einen Knoten eine Vollstaendige */
/*******************************************************/
Global Sgraph complete_segments(g)
Sgraph g;
{
Snode n;
Sedge e;
X_Y co,co_new;
if(!g->directed) return(g);
for_all_nodes(g,n)
	{
	for_sourcelist(n,e)
		{
		if(XY_EDGE_COORD(e) == NULL)continue;
		if(( ((X_Y)(XY_EDGE_COORD(e))->elem)->x != n->x) || 
		   ( ((X_Y)(XY_EDGE_COORD(e))->elem)->y != n->y ))
			XY_EDGE_COORD(e) = REVERSE_LIST(XY_EDGE_COORD(e));
		if(reverseedge(e->tnode,e) != e)
		   {
		   CLEAR_LIST(XY_EDGE_COORD(reverseedge(e->tnode,e)));
		   dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
		      co_new = (X_Y)make_coord(co->x,co->y);
		      PUSH(XY_EDGE_COORD(reverseedge(e->tnode,e)),co_new);
	  	   dend_for_all_elements(XY_EDGE_COORD(e));
	  	   }
	} end_for_sourcelist(n,e)
} end_for_all_nodes(g,n)
return(g);
}

Global Sgraph completelist(g)
Sgraph g;
{
DLIST queue,work;
Snode n;
Sedge e,f;
int level = 1;
INIT_LIST(queue);
dfor_all_elements( XY_NODE_HIGH( (Snode)find_Snode_with_number(g,1)),(char *)e);
	QUEUE(queue,e);
dend_for_all_elements( XY_NODE_HIGH((Snode)find_Snode_with_number(g,1)));
while(!DIS_EMPTY_LIST(queue))
	{
	DINIT_LIST(work);
	dfor_all_elements(queue,(char *)e);
		n = HIGH_NODE(e);
		if(n->y == level)
			{
			if(DIS_EMPTY_LIST(XY_NODE_EDGES(n)))
				{
				dfor_all_elements(XY_NODE_HIGH(n),(char *)f);
					QUEUE(work,f);
				dend_for_all_elements(XY_NODE_HIGH(n));
				}
			PUSH(XY_NODE_EDGES(n),reverseedge(n,e));
			}
		else QUEUE(work,e);
	dend_for_all_elements(queue);
	level++;
	CLEAR_LIST(queue);
	queue = work;
	}
for_all_nodes(g,n)
	{
	dfor_all_elements(XY_NODE_HIGH(n),(char *)e);
		QUEUE(XY_NODE_EDGES(n),e);
	dend_for_all_elements(XY_NODE_HIGH(n));
} end_for_all_nodes(g,n)
return(g);
}


