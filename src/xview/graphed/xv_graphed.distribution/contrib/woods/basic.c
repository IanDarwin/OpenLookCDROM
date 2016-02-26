/* (C) Universitaet Passau 1986-1991 */
/*******************************************************/
/*							*/
/*			  BASIC.C			*/
/*							*/
/* (enthaelt den Grundalgorithmus zur Berechnung der    */
/*			Koordinaten)			*/
/*							*/
/* uebergeben wird ein st-nummerierter Graph ohne       */
/* doppelte Kanten oder self-loops, MAX_NR ist mit      */
/* Nummer des t-Knoten initialisiert und fuer jeden     */
/* Knoten ist eine Liste der Kanten zu ihm aufgebaut    */
/********************************************************/

#include <std.h>
#include <sgraph.h>
#include "listen_macros.h"
#include "basic.h"


extern Sgraph biconnect();
extern Sgraph completelist(), complete_segments();
extern void make_dummy_left();
extern void make_dummy_right();
extern LIST otheredge();
extern Sedge ssuc(),spre();
extern Sedge OTHEREDGE(),reverseedge();
extern int mein_test_st_number(),MAX_NR,G_hoehe,G_breite;


Global X_Y make_coord(xnew,ynew)
int xnew,ynew;
{
X_Y newcoord;
newcoord = (X_Y)malloc(sizeof(struct x_y));
newcoord->x = xnew;
newcoord->y = ynew;
return(newcoord);
}

static LIST reverselist(liste)
LIST liste;
{
LIST neueliste;
X_Y xy;
INIT_LIST(neueliste);
while(!IS_EMPTY_LIST(liste))
	{
	POP_XY(liste,xy);
	PUSH(neueliste,xy);
	}
return(neueliste);
}

/*******************************************************/
/*    ordinate1 berechnet die y-Koordinaten,           */
/*    laesst aber keine horizontalen Kanten zu         */
/*******************************************************/

Global void ordinate1(graph)
Sgraph graph;
{
Snode n;
Sedge e;
LIST liste;
LIST queue;
INIT_LIST(liste);
INIT_LIST(queue);
((Snode)find_Snode_with_number(graph,1))->y = 0;
DQUEUE(queue,(Snode)find_Snode_with_number(graph,1));
while(! IS_EMPTY_LIST(queue))
	{
	DPOP_NODE(queue,n);
	if(XY_NODE_EDGES(n) == NULL) liste = XY_NODE_HIGH(n);
	else liste = XY_NODE_EDGES(n);
	dfor_all_elements(liste,(char *)e);
		if((OTHER_NODE(n,e)->nr > n->nr)&&(n->y >= OTHER_NODE(n,e)->y))
			{
			OTHER_NODE(n,e)->y = n->y + 1;
			DQUEUE(queue,OTHER_NODE(n,e));
			}
	dend_for_all_elements(liste);
	}
}


int is_left(e,etnode)
Sedge e;
Snode etnode;
{
if(((XY_EDGE_HORIZ(e) == LEFT)&&(e->tnode == etnode)) ||
   ((XY_EDGE_HORIZ(e)== RIGHT)&&(e->snode == etnode)))
	return(1); else return(0);
}

int is_right(e,etnode)
Sedge e;
Snode etnode;
{
if(((XY_EDGE_HORIZ(e) == RIGHT)&&(e->tnode == etnode)) ||
   ((XY_EDGE_HORIZ(e)== LEFT)&&(e->snode == etnode)))
	return(1); else return(0);
}

/************************************************************/
/*  ordinate2 berechnet die y-Koordinaten wobei waagerechte */
/*  Kanten zugelassen werden                                */
/************************************************************/

static void ordinate2(g)
Sgraph g;
{
int k;
Snode n,etnode,epretnode,essuctnode;
Sedge e,essuc,epre,le,fe;
LIST queue,enewlist;
INIT_LIST(queue);
INIT_LIST(enewlist);
for_all_nodes(g,n)
	{
	n->y = 0;
	if(n->nr != 1 && n->nr != MAX_NR)
		{
		e = (Sedge)(XY_NODE_EDGES(n)->elem);
		while(OTHER_NODE(n,e)->nr < n->nr)
			{
			XY_NODE_EDGES(n) = XY_NODE_EDGES(n)->suc;
			e = (Sedge)(XY_NODE_EDGES(n)->elem);
			}
		while(OTHER_NODE(n,e)->nr > n->nr)
			{
			XY_NODE_EDGES(n) = XY_NODE_EDGES(n)->suc;
			e = (Sedge)(XY_NODE_EDGES(n)->elem);
			}
		}
} end_for_all_nodes(g,n)

((Snode)find_Snode_with_number(g,1))->y = 0;
dfor_all_elements(XY_NODE_EDGES((Snode)find_Snode_with_number(g,1)),(char *)e);
	OTHER_NODE((Snode)find_Snode_with_number(g,1),e)->y = 1;
	QUEUE_NODE(queue,OTHER_NODE((Snode)find_Snode_with_number(g,1),e));
dend_for_all_elements(XY_NODE_EDGES((Snode)find_Snode_with_number(g,1)));

while(! IS_EMPTY_LIST(queue))
	{
	POP_NODE(queue,n);
	if(n->nr == MAX_NR) continue;
	dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
		essuc = ssuc(n,e);
		etnode = OTHER_NODE(n,e);
		essuctnode = OTHER_NODE(n,essuc);
		if((etnode->nr < n->nr)&&(essuctnode->nr > n->nr))
			{
		     if((is_left(e,etnode))&&(etnode->y >= n->y)) 
				fe = e; else fe = essuc;
			}
		if((etnode->nr > n->nr)&&(essuctnode->nr < n->nr))
			{
		  	if((is_right(essuc,essuctnode))&&(essuctnode->y >= n->y))
				le = essuc; else le = e;
			}
	dend_for_all_elements(XY_NODE_EDGES(n));

	dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
		etnode = OTHER_NODE(n,e);
		if((etnode->nr < n->nr)||(n->y < etnode->y)) continue;
		if(etnode->nr == MAX_NR)
			{
			etnode->y = n->y + 1;
			continue;
			}
		epre = spre(etnode,(Sedge)(OTHEREDGE(etnode,e)));
		epretnode = OTHER_NODE(etnode,epre);
		essuc = ssuc(etnode,(Sedge)(OTHEREDGE(etnode,e)));
		essuctnode = OTHER_NODE(etnode,essuc);
		if(e == fe)
			{
			if((!is_right(essuc,essuctnode))&&
					(etnode->nr < epretnode->nr))
/*			if(((epretnode->nr > n->nr)&&(XY_EDGE_HORIZ(epre)
			  != RIGHT))||((XY_EDGE_HORIZ(epre) == RIGHT) &&
			   (epretnode->y >= n->y )))*/

				{
				etnode->y = n->y;
				if(etnode == e->tnode)
				   {
				   XY_EDGE_HORIZ((Sedge)OTHEREDGE(etnode,
					e)) = RIGHT;
				   XY_EDGE_HORIZ(e) = LEFT;
				} else {
				   XY_EDGE_HORIZ((Sedge)OTHEREDGE(n,
					e)) = LEFT;
				   XY_EDGE_HORIZ(e) = RIGHT;
					} 

				if((is_right(epre,epretnode)) &&
				   (epretnode->y == etnode->y))
					{
					XY_EDGE_HORIZ(epre)=VERTICAL;
					XY_EDGE_HORIZ(OTHEREDGE(
					epretnode,epre))=VERTICAL;
					epretnode->y = 0;
					QUEUE_NODE(queue,etnode);
					}
				/*if(etnode->y != n->y)*/
					QUEUE_NODE(queue,etnode);
				continue;
				}
			}
		if(e == le)
			{
			if((!is_left(epre,epretnode))&&
					(etnode->nr < essuctnode->nr))
/*			if(((essuctnode->nr > n->nr)&&(XY_EDGE_HORIZ(essuc)
			  != LEFT))||((XY_EDGE_HORIZ(essuc) == LEFT) &&
			   (essuctnode->y >= n->y )))*/
				{

				etnode->y = n->y;
				if(etnode == e->tnode)
				   {
				   XY_EDGE_HORIZ((Sedge)OTHEREDGE(etnode,
					e)) = LEFT;
				   XY_EDGE_HORIZ(e) = RIGHT;
				} else {
				   XY_EDGE_HORIZ((Sedge)OTHEREDGE(n,
					e)) = RIGHT;
				   XY_EDGE_HORIZ(e) = LEFT;
				} 

				if((is_left(essuc,essuctnode))&&
				   (essuctnode->y == etnode->y))
					{
					XY_EDGE_HORIZ(essuc)=VERTICAL;
					XY_EDGE_HORIZ(OTHEREDGE(
					essuctnode,essuc))=VERTICAL;
					essuctnode->y = 0;
					QUEUE_NODE(queue,etnode);
					}
				/*if(etnode->y != n->y)	*/								QUEUE_NODE(queue,etnode);
				continue;
				}
			}
		etnode->y = n->y + 1;
		QUEUE_NODE(queue,etnode);
	dend_for_all_elements(XY_NODE_EDGES(n));
}
k = 2;
while(k < MAX_NR)
	{
	for_all_nodes(g,n)
		{
		if(n->nr == k)
			{
			k++;
			dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
				if((n->nr > OTHER_NODE(n,e)->nr)||
				  (XY_EDGE_HORIZ(e) == VERTICAL) ||
				  (OTHER_NODE(n,e)->y != n->y)) continue;
				if(XY_EDGE_HORIZ(e) == RIGHT) 
					{
					if(e->snode == n)
				  	 make_dummy_right(e->tnode,e,1);
					else make_dummy_left(e->snode,OTHEREDGE(n,e),1);
					}
				if(XY_EDGE_HORIZ(e) == LEFT) 
					{
					if(e->snode == n)
				  	 make_dummy_left(e->tnode,e,1);
					else make_dummy_right(e->snode,OTHEREDGE(n,e),1);
					}
			dend_for_all_elements(XY_NODE_EDGES(n));
			}
	} end_for_all_nodes(g,n)
}
}


void abszissa(g)
Sgraph g;
{
LIST work,newwork;
Snode n;
Sedge e,going_in_edge,f;
X_Y co,co_new;
int level = 1;
int segment,int_dummy;
G_hoehe = 0;
G_breite = 0;
for_all_nodes(g,n)
	{
	if(n->nr != 1)
		{
		e = (Sedge)(XY_NODE_EDGES(n)->elem);
		while(OTHER_NODE(n,e)->y > n->y)
			{
			XY_NODE_EDGES(n) = XY_NODE_EDGES(n)->suc;
			e = (Sedge)(XY_NODE_EDGES(n)->elem);
			}
		if((OTHER_NODE(n,e)->y == n->y)&&
		(((XY_EDGE_HORIZ(e)==RIGHT)&&(e->tnode != n))||
		 ((XY_EDGE_HORIZ(e)==LEFT) &&(e->tnode == n))))
			{
			XY_NODE_EDGES(n) = XY_NODE_EDGES(n)->suc;
			e = (Sedge)(XY_NODE_EDGES(n)->elem);
			}
		}
} end_for_all_nodes(g,n)
n = (Snode)find_Snode_with_number(g,1);
n->x = 0;
INIT_LIST(work);
INIT_LIST(newwork);
dfor_all_elements(XY_NODE_EDGES(n),(char *)e);
	XY_EDGE_ABS(e) = 0;
	if(XY_EDGE_TYPE(e) == REALY) QUEUE(XY_EDGE_COORD(e),make_coord(0,0));
	QUEUE(work,e);
dend_for_all_elements(XY_NODE_EDGES(n));
while(!IS_EMPTY_LIST(work))
    {
/*************************** 1.Pass ************************************/
    segment = 0;
    going_in_edge = nil;
    dfor_all_elements( work,(char *)e);
	if(going_in_edge == nil)
	    {
	    if (TNODE(e)->y > level)
		{if(XY_EDGE_TYPE(e) == REALY)segment ++;}
		else going_in_edge = e;
	    }
	else {
	     if(TNODE(e)->y > level)
		{
		if(XY_EDGE_TYPE(e) == REALY) segment++;
		segment++;
		going_in_edge = nil;
		}
	     else {
		if(TNODE(going_in_edge) != TNODE(e)) segment++;
		going_in_edge = e;
	      }
	}
     dend_for_all_elements(work);
     if(going_in_edge != nil) segment++;
     if(segment > G_breite) G_breite = segment;
/****************************** 2.Pass *********************************/
     segment = -(segment - 1)/2;
     INIT_LIST(newwork);
     going_in_edge = nil;
     dfor_all_elements(work,(char *)e);
	if(going_in_edge == nil)
	    {
	    if(TNODE(e)->y > level)
		{
		XY_EDGE_ABS(e) = segment;
		QUEUE(newwork,e);
		if(XY_EDGE_TYPE(e) == REALY)
		    {
		    QUEUE(XY_EDGE_COORD(e),make_coord(segment,level));
		    segment++;
		    }
	     } else {
		going_in_edge = e;
		TNODE(e)->x = segment;
	      }
	} else
	{
	    if(TNODE(e)->y > level)
	    	{
		TNODE(going_in_edge)->x = segment;
		if(XY_EDGE_TYPE(going_in_edge) == REALY) 
		   QUEUE(XY_EDGE_COORD(going_in_edge),make_coord(segment,level));
		XY_EDGE_ABS(going_in_edge) = segment;
		dfor_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)),(char *)f);
		    if(OTHER_NODE(TNODE(going_in_edge),f)->y >
				TNODE(going_in_edge)->y)
			{
			XY_EDGE_ABS(f) = segment;
			if(XY_EDGE_TYPE(f) == REALY) QUEUE(XY_EDGE_COORD(f),
				make_coord(segment,level));
			QUEUE(newwork,f);
			}
		    if((OTHER_NODE(TNODE(going_in_edge),f)->y ==
				TNODE(going_in_edge)->y)&&
			(OTHER_NODE(TNODE(going_in_edge),f)->nr >
				TNODE(going_in_edge)->nr))
			{
			QUEUE(XY_EDGE_COORD(f),make_coord(segment,level));
			if(XY_EDGE_HORIZ(f) == RIGHT)
			{
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1; 
		           }
			} else {
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1; 
		           }
			} 
		     }
		dend_for_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)));
		segment++;
		XY_EDGE_ABS(e) = segment;
		QUEUE(newwork,e);
		if(XY_EDGE_TYPE(e) == REALY)
			{
			QUEUE(XY_EDGE_COORD(e),make_coord(segment,level));
			segment++;
			}
		going_in_edge = nil;
	     } else
	     {
	     if(XY_EDGE_TYPE(going_in_edge) == REALY) QUEUE(XY_EDGE_COORD(going_in_edge),
			make_coord(segment,level));
	     TNODE(e)->x = segment;
	     TNODE(going_in_edge)->x = segment;
	     if(TNODE(going_in_edge) != TNODE(e))
		{
		XY_EDGE_ABS(going_in_edge) = segment;
		dfor_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)),(char *)f);
		    if(OTHER_NODE(TNODE(going_in_edge),f)->y >
				TNODE(going_in_edge)->y)
			{
			XY_EDGE_ABS(f) = segment;
			if(XY_EDGE_TYPE(f) == REALY) QUEUE(XY_EDGE_COORD(f),
				make_coord(segment,level));
			QUEUE(newwork,f);
			}
		    if((OTHER_NODE(TNODE(going_in_edge),f)->y ==
				TNODE(going_in_edge)->y)&&
			(OTHER_NODE(TNODE(going_in_edge),f)->nr >
				TNODE(going_in_edge)->nr))
			{
			QUEUE(XY_EDGE_COORD(f),make_coord(segment,level));
			if(XY_EDGE_HORIZ(f) == RIGHT)
			{
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1; 
		           }
			} else {
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1; 
		           }
			}
		     }
		dend_for_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)));
		segment++;
		}
	      going_in_edge = e;
	      }
        }
    dend_for_all_elements(work);
    if(going_in_edge != nil)
	{
	TNODE(going_in_edge)->x = segment;
	if(XY_EDGE_TYPE(going_in_edge) == REALY) 
	   QUEUE(XY_EDGE_COORD(going_in_edge),make_coord(segment,level));
	XY_EDGE_ABS(going_in_edge) = segment;
	dfor_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)),(char *)f);
	    if(OTHER_NODE(TNODE(going_in_edge),f)->y >
				TNODE(going_in_edge)->y)
		{
		XY_EDGE_ABS(f) = segment;
		if(XY_EDGE_TYPE(f) == REALY) QUEUE(XY_EDGE_COORD(f),
			make_coord(segment,level));
		QUEUE(newwork,f);
		}
	    if((OTHER_NODE(TNODE(going_in_edge),f)->y ==
				TNODE(going_in_edge)->y)&&
		(OTHER_NODE(TNODE(going_in_edge),f)->nr >
				TNODE(going_in_edge)->nr))
		{
		QUEUE(XY_EDGE_COORD(f),make_coord(segment,level));
		if(XY_EDGE_HORIZ(f) == RIGHT)
			{
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1; 
		           }
			} else {
			if(f->snode == TNODE(going_in_edge))
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment-1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment - 1;
		           }
			else
			   {
			    QUEUE(XY_EDGE_COORD(f),make_coord(segment+1,level));
			    OTHER_NODE(TNODE(going_in_edge),f)->x = segment + 1; 
		           }
			}
	     }
	dend_for_all_elements(XY_NODE_EDGES(TNODE(going_in_edge)));
     }
     CLEAR_LIST(work);
     work = newwork;
     level++;
          }
G_hoehe = level;
for_all_nodes(g,n)
	{
	for_sourcelist(n,e)
		{ 
		if(XY_EDGE_COORD(e) == NULL)continue;
		if(( ((X_Y)(XY_EDGE_COORD(e))->elem)->x != n->x) || 
		   ( ((X_Y)(XY_EDGE_COORD(e))->elem)->y != n->y ))
			XY_EDGE_COORD(e) = reverselist(XY_EDGE_COORD(e));
		if(reverseedge(e->tnode,e) != e)
		   {
		   CLEAR_LIST(XY_EDGE_COORD(reverseedge(e->tnode,e)));
		   dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
		      co_new = make_coord(co->x,co->y);
		      PUSH(XY_EDGE_COORD(reverseedge(e->tnode,e)),co_new);
	  	   dend_for_all_elements(XY_EDGE_COORD(e));
	  	   }
	} end_for_sourcelist(n,e)
	/*if(g->directed)
	{
	for_targetlist(n,e)
		{ 
		if(XY_EDGE_COORD(e) == NULL)continue;
		if(( ( (X_Y)((XY_EDGE_COORD(e))->pre)->elem)->x != n->x || 
		     ( (X_Y)((XY_EDGE_COORD(e))->pre)->elem)->y != n->y))
			XY_EDGE_COORD(e) = reverselist(XY_EDGE_COORD(e));
		if(reverseedge(n,e) != e)
		   {
		printf("error\n");
		   CLEAR_LIST(XY_EDGE_COORD(reverseedge(n,e)));
		   dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
		      co_new = make_coord(co->x,co->y);
		      PUSH(XY_EDGE_COORD(reverseedge(n,e)),co_new);
	  	   dend_for_all_elements(XY_EDGE_COORD(e));
	  	   }
	} end_for_targetlist(n,e)
	}*/
} end_for_all_nodes(g,n);
message("width %d, heigth %d\n",G_breite,G_hoehe);
}


/**********************************************************/
/*                                                        */
/*     der Hauptalgorithmus, der die x-Koordinaten der    */
/*     Knoten berechnet und die Segmente der Kanten       */
/*     erzeugt.						  */
/*							  */
/**********************************************************/

Global void woods_drawing(g)
Sgraph g;
{
Snode Gnode;
Sedge Gedge;
Gnode = g->nodes;
Gedge = Gnode->slist;
init_xy_attrs(g);
if(!mein_test_st_number(g)) 
	{
	message("Der Graph ist nicht ST-nummeriert\n");
	return;
	}
ordinate1(g);
g = completelist(g);
if(woods_settings.LargeFace)
	{
	message("Moving Largest Face outside\n");
	g = biconnect(g); 
	}
if(woods_settings.Horizontal)
	{
	message("Looking for horizontal edges\n");
	ordinate2(g);
	}
abszissa(g);

/*g = complete_segments(g);*/
graph_test(g);
g = (Sgraph)make_sun_size(g); 
copy_xy_to_edge(g);
free_xy_attrs(g);
g->nodes = Gnode;
Gnode->slist = Gedge;
}



