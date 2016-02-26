/* (C) Universitaet Passau 1986-1991 */
/***************************************************/
/*						   */
/*                     BASIC.H              	   */
/*						   */
/*    (enthaelt die Strukturen fuer BASIC.C        */
/*   sowie alle Makros, um auf diese zuzugreifen)  */
/*						   */
/***************************************************/

#define DUMMY 	1
#define REALY	0

#define VERTICAL	0
#define RIGHT		1
#define LEFT		2

#define OTHER_NODE(n,e) \
	( iif(e->snode == n,e->tnode,e->snode))
#define HIGH_NODE(e) \
	(iif(e->tnode->y == -1,e->tnode, \
	iif(e->tnode->y > e->snode->y,e->tnode,e->snode)))
#define TNODE(e) \
	(iif(e->tnode->nr > e->snode->nr,e->tnode,e->snode))
#define NODE_ATTRS(n) \
	( ( (attributes *)( (n)->attrs.data ) ) )
#define HIGH(n) \
	( NODE_ATTRS(n)->highedges )

#define XY_NODE_ATTRS(n) \
	( ( (xy_node_attributes *)( (n)->attrs.data ) ) )
#define XY_NODE_HIGH(n) \
	( XY_NODE_ATTRS(n)->highedges )
#define XY_NODE_EDGES(n) \
	( XY_NODE_ATTRS(n)->edges )
#define XY_NODE_DATA(n) \
	( XY_NODE_ATTRS(n)->data )
#define XY_NODE_OLDNR(n) \
	( XY_NODE_ATTRS(n)->oldnr )

#define XY_EDGE_ATTRS(e) \
	( ( (xy_edge_attributes *)( (e)->attrs.data ) ) )
#define XY_EDGE_TYPE(e) \
	( XY_EDGE_ATTRS(e)->type )
#define XY_EDGE_ABS(e) \
	( XY_EDGE_ATTRS(e)->abscissa )
#define XY_EDGE_HORIZ(e) \
	( XY_EDGE_ATTRS(e)->horizontal )
#define XY_EDGE_COORD(e) \
	( XY_EDGE_ATTRS(e)->coordinates )

typedef struct x_y /* dient als Koordinatentupel */
	{
	int x,y;
	} *X_Y;
typedef struct
	{
	DLIST highedges;
	} attributes;
typedef struct  /* highedges enthaelt alle Kanten zu hoeheren Knoten */
	{	/* edges enthaelt alle Kanten */
	DLIST highedges,edges;
	int data;
	int oldnr;
	} xy_node_attributes;
typedef struct
	{
	int abscissa;/*Laufvariable fuer die y-Koordinate*/
	int type;     /*entweder dummy oder echt*/
	int horizontal;/*die Richtung:rechts,links oder vertikal*/
	DLIST coordinates;/*die Koordinaten fuer die Segmente der Kante*/
	} xy_edge_attributes;

extern char *sprintf();

typedef	struct {
	int	LargeFace;
	int	Horizontal;
	int	size_defaults_x, size_defaults_y;
	int	vertical_distance, horizontal_distance;
}
	Woods_settings;
			
extern	Woods_settings	woods_settings;
