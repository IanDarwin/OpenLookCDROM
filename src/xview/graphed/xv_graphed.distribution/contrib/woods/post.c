/*********************************************************/
/*							 */
/*                       POST.C 			 */
/*							 */
/*    (enthaelt die Postprozessorfunktionen)		 */
/*  bis jetzt die free-Prozedur ,eine Funktion zur       */
/*  Dehnung aller Koordinaten fuer den Sunbildschirm     */
/*  (z.B. Graphed)					 */
/*							 */
/*********************************************************/

#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include "listen_macros.h"
#include "basic.h"

extern int G_breite,G_hoehe;
extern Sedge OTHEREDGE();

/**********************************************************************/
/**************** Dehnung der Koordinaten fuer die Sun ****************/
/**********************************************************************/

static int Breite = 3000;
static int Hoehe = 4000;
static int Raster = 60;
static int ydown,deltax,deltay;

static int make_x(x)
int x;
{
/* Orig. Code
int xnew;
xnew = (Breite/2)+(deltax*x);
xnew = maximum(0,xnew);
xnew = minimum(Breite,xnew);
xnew = xnew + 500;
return(xnew);
*/
	return	woods_settings.horizontal_distance * x;
}

static int make_y(y)
int y;
{
/* Orig. Code
int ynew;
ynew = ydown - (deltay*y);
ynew = maximum(0,ynew);
ynew = minimum(Hoehe,ynew);
return(ynew);
*/
	return	woods_settings.vertical_distance * y;
}

Global Sgraph make_sun_size(g)
Sgraph g;
{
Snode n;
Sedge e;
X_Y co;
/* Orig. Code
deltax = Breite/(G_breite + 1);
deltay = Hoehe/(G_hoehe + 1);
if((deltax*10)/deltay > 15 ) deltax = (deltay*15)/10;
if((deltay*15)/deltax > 10 ) deltay = (deltax*10)/15;
if(deltay > Raster) deltay = Raster;
if(deltax > Raster) deltax = Raster;
ydown = Hoehe - 5 - ((Hoehe - deltay*G_hoehe)/2);
for_all_nodes(g,n)
	{
	n->x = make_x(n->x);
	n->y = make_y(n->y);
	for_sourcelist(n,e)
		{
		dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
			co->x = make_x(co->x);
			co->y = make_y(co->y);
		dend_for_all_elements(XY_EDGE_COORD(e));
	} end_for_sourcelist(n,e)
} end_for_all_nodes(g,n)
return(g);
*/

	for_all_nodes(g,n) {
		n->x = make_x(n->x);
		n->y = make_y(n->y);
		for_sourcelist(n,e) {
			dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
				co->x = make_x(co->x);
				co->y = make_y(co->y);
			dend_for_all_elements(XY_EDGE_COORD(e));
		} end_for_sourcelist(n,e)
	} end_for_all_nodes(g,n)
	return(g);

}


/********************************************************************/
/********************** die FREE-Prozedur ***************************/
/********************************************************************/

static void free_node_xy_attrs(p)
char *p;
{
CLEAR_LIST(((xy_node_attributes *)p)->highedges);
CLEAR_LIST(((xy_node_attributes *)p)->edges);
free(p);
}
void free_edge_xy_attrs(p)
char *p;
{
CLEAR_LIST(((xy_edge_attributes *)p)->coordinates);
free(p);
}


Global void free_xy_attrs(g)
Sgraph g;
{
Snode n;
Sedge e;
X_Y co;
for_all_nodes(g,n)
	{
	for_sourcelist(n,e) {
		dreverse_for_all_elements(XY_EDGE_COORD(e),(char *)co);
			free(co);
		dreverse_end_for_all_elements(XY_EDGE_COORD(e));
		free_edge_xy_attrs(e->attrs.data);
	} end_for_sourcelist(n,e)
	n->nr = XY_NODE_OLDNR(n);
	free_node_xy_attrs(n->attrs.data);
} end_for_all_nodes(g,n)
}

Local int edge_test(el,e)
Edgeline el;
Sedge e;
{
Edgeline hilf;
int up,oldy;
if( e->snode->nr > e->tnode->nr) up = 1;
	else up = 0;
oldy = edgeline_y(el);
for_edgeline(el,hilf)
	if(hilf == el)continue;
	if(up) {
		if(oldy >= edgeline_y(hilf)) return(1);
	} else  if(oldy <= edgeline_y(hilf)) return(1);
	oldy = edgeline_y(hilf);
end_for_edgeline(el,hilf)
return(0);
}

Local  void copy_xy_edge_coord(n,e)
Snode n;
Sedge e;
{
X_Y co;
Edgeline el;
el=new_edgeline(n->x,n->y);
dreverse_for_all_elements(XY_EDGE_COORD(e),(char *)co);
	if ((co->x!=n->x)||(co->y!=n->y))
		{
		(void)add_to_edgeline(el,co->x,co->y);
		}
dreverse_end_for_all_elements(XY_EDGE_COORD(e));
edge_set(graphed_edge(e),ONLY_SET,EDGE_LINE,el,0);
}

Global void copy_xy_to_edge(g)
Sgraph g;
{
Snode n;
Sedge e;
for_all_nodes(g,n)
	{
	node_set(graphed_node(n),ONLY_SET,NODE_POSITION,n->x,n->y,0);
} end_for_all_nodes(g,n)
for_all_nodes(g,n)
	{
	for_sourcelist(n,e) {
		if(n == (Snode)sedge_real_source(e))
		   copy_xy_edge_coord((Snode)sedge_real_source (e),e);
	} end_for_sourcelist(n,e)
} end_for_all_nodes(g,n)
}



Local int edge_xy_test(e)
Sedge e;
{
X_Y co;
int up,oldy;
if( e->snode->nr < e->tnode->nr) up = 1;
	else up = 0;
oldy = e->snode->y;
dfor_all_elements(XY_EDGE_COORD(e),(char *)co);
	if(up) {
		if(oldy > co->y) return(1);
	} else  if(oldy < co->y) return(1);
	oldy = co->y;
dend_for_all_elements(XY_EDGE_COORD(e));
return(0);
}

Global void graph_test(g)
Sgraph g;
{
Snode n;
Sedge e;
for_all_nodes(g,n)
	{
	for_sourcelist(n,e) {
		 if (edge_xy_test(e)) 
			message("Error in PQ planarity test.\n");
	} end_for_sourcelist(n,e)
} end_for_all_nodes(g,n)
}



