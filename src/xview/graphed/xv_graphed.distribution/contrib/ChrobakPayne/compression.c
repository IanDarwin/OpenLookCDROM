/* (C) Universitaet Passau 1986-1991 */
/****************************************************************
 **                                                            **
 **     Hier sind die Unterprogramme aufgefuehrt, die die      **
 **     verschiedenen Kompressionsvarianten aus dem Benutzer-  **
 **     menue beinhalten.                                      **
 **                                                            **
 ****************************************************************/


#include "decl.h"

int comp_direction = 0;


void make_row ()
			/* Bestimmt die Reihenfolge der Knoten. */

{
	change_y_coord_with_y ();
	sort_y_coordinates ();
	change_y_coord_with_y ();
	
} /* make_row */




double bound (n, n1, n2, np)
			/* Gibt den, bzgl. einer Geraden zw. den Knoten "n1" und "n2" moeglichen, */
			/* minimalen y-Wert fuer den aktuellen Knoten "n" aus.                    */
struct node *n, *n1, *n2;
double np;

{
	double div, result;
	
	
	if (n2->x_offset != n1->x_offset) {
		div = (((double) (n->x_offset - n1->x_offset))
	        / ((double) (n2->x_offset - n1->x_offset)));
		result = ((n1->y_coord + (n2->y_coord - n1->y_coord) * div));
	}
	else {
		if (n->x_offset == n1->x_offset) {
			if (n1->y_coord > n2->y_coord) {
				result = n1->y_coord;
			}
			else {
				result = n2->y_coord;
			};
		}
		else {
			result = (-1.0);
		};
	};
	if ((result < n->y_coord) && (result > np)) {
		np = result;
	};
	
	return np;
	
} /* bound */
		
	

void compress (s, kind)
		/* Legt fuer alle Knoten einen neuen Gitterpunkt mit moeglichst kleinem */
		/* y - Wert fest. Der alte x - Wert bleibt.                             */
Sgraph s;
int kind;

{
	struct node *gra;
	struct nlist *hneigh1, *hneigh2;
	struct node *hnode, *exnode1, *exnode2, *exnode3, *top, *h_row;
	double b, newpoint, y_buf;
	int i, r1, r2, r3, sec, sec2;
	Snode n;
	
	sec = -1;
	sec2 = -3;
	
	make_row ();
	
	update_exterior_face_situation (sec);
	
	if (plan == FALSE) {
		return;
	};
	
	if (sec == -1) {
		sec = -2;
	}
	else {
		sec = -1;
	};
	
	if (firstasc->y_coord > 0) {
		gra = graph;
		
		while (gra != NULL) {
			gra->y_coord = gra->y_coord - firstasc->y_coord;
			gra = gra->next;
		};
	};
		
	newpoint = -1.0;
	b = -1.0;
	h_row = firstasc;
	
	do  {
		newpoint = -1.0;
		b = -1.0;	
		hneigh1 = h_row->clockw;
		
		do {
			if (is_neighbor (hneigh1->node, hneigh1->clockw->node)) {
				newpoint = bound (h_row, hneigh1->node, hneigh1->clockw->node, newpoint);
			};
			
			hneigh1 = hneigh1->clockw;
		} while (hneigh1 != h_row->clockw);
	
		if ((h_row->exterior == 1) && (h_row->y_coord > 0)) {
			newpoint = test_exterior_nodes (firstdesc, h_row, newpoint, sec2);
			
			if (plan == FALSE) {
				return;
			};
			
			if (sec2 == -3) {
				sec2 = -4;
			}
			else {
				sec2 = -3;
			};
		};
		
		if (kind == 2) {
			if (((int)(newpoint + 1)) < h_row->y_coord) {
				h_row->y_coord--;
			};   /* Falls kind == 2, faellt der Knoten nur 1 Einheit. */
		}
		else {
			h_row->y_coord = (int)(newpoint + 1);
		};
		
		if (kind == 1) {
			for_all_nodes (s, n) {
				if (n->nr == h_row->number) {
					if (comp_direction == 0) {
						node_set (graphed_node (n),  
					    	      NODE_POSITION,
						          n->x,
						          (new_zero_y - ((h_row->y_coord) * grid)),
						          0);
						n->y = (new_zero_y - ((h_row->y_coord) * grid));
					}
					else {
						node_set (graphed_node (n), 
						          NODE_POSITION,
						          (new_zero_x + ((h_row->y_coord) * grid)),
						          n->y,
						          0);
						n->x = (new_zero_x  + ((h_row->y_coord) * grid));
					};
					
					force_repainting ();
				};
			} end_for_all_nodes (s, n);
		};
		
				
		h_row = h_row->ascend;
	} while (h_row != firstasc);
	
} /* compress */



double test_exterior_nodes (firstdesc, h_row, newpoint, sec)
			/* Testet, ob ein Knoten am "Rand" den Knoten "h_row" beim Fallen */
			/* behindert.                                                     */
struct node *firstdesc, *h_row;
double newpoint;
int sec;

{
	struct node *hnode, *exnode1;
	
	hnode = firstdesc;
	exnode1 = h_row;
	
	do  {
		if (comp_direction == 0) {
			if ((exnode1->exnext->in == 1) 
			&& ((hnode->y_coord <= exnode1->exnext->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))
			&& (hnode->x_offset > exnode1->exnext->x_offset)
			&& (hnode->x_offset <= exnode1->x_offset)) {
				newpoint = bound (exnode1, exnode1->exnext, hnode, newpoint);
			};
			if ((exnode1->in == 1) 
			&& ((hnode->y_coord <= exnode1->exlast->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))
			&& (hnode->x_offset < exnode1->exlast->x_offset)
			&& (hnode->x_offset >= exnode1->x_offset)) {
				newpoint = bound (exnode1, exnode1->exlast, hnode, newpoint);
			};
			if ((exnode1->x_offset >= hnode->exlast->x_offset)
			&& (exnode1->x_offset <= hnode->x_offset)
			&& ((hnode->exlast->y_coord <= exnode1->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))) {
				newpoint = bound (exnode1, hnode->exlast, hnode, newpoint);
			};
			if ((exnode1->x_offset >= hnode->exnext->x_offset)
			&& (exnode1->x_offset <= hnode->x_offset)
			&& ((hnode->exnext->y_coord <= exnode1->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))) {
				newpoint = bound (exnode1, hnode->exnext, hnode, newpoint);
			};
				
			hnode->processed = sec;
			hnode = hnode->exnext; 
					
		}
		else {
			if ((exnode1->exlast->in == 1) 
			&& ((hnode->y_coord <= exnode1->exlast->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))
			&& (hnode->x_offset > exnode1->exlast->x_offset)
			&& (hnode->x_offset <= exnode1->x_offset)) {
				newpoint = bound (exnode1, exnode1->exlast, hnode, newpoint);
			};
			if ((exnode1->in == 1) 
			&& ((hnode->y_coord <= exnode1->exnext->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))
			&& (hnode->x_offset < exnode1->exnext->x_offset)
			&& (hnode->x_offset >= exnode1->x_offset)) {
				newpoint = bound (exnode1, exnode1->exnext, hnode, newpoint);
			};
			if ((exnode1->x_offset >= hnode->exnext->x_offset)
			&& (exnode1->x_offset <= hnode->x_offset)
			&& ((hnode->exnext->y_coord <= exnode1->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))) {
				newpoint = bound (exnode1, hnode->exnext, hnode, newpoint);
				};
			if ((exnode1->x_offset >= hnode->exlast->x_offset)
			&& (exnode1->x_offset <= hnode->x_offset)
			&& ((hnode->exlast->y_coord <= exnode1->y_coord) 
			|| (hnode->y_coord <= exnode1->y_coord))) {
				newpoint = bound (exnode1, hnode->exlast, hnode, newpoint);
			};
						
			hnode->processed = sec;
			hnode = hnode->exlast; 		
		};
	} while ((hnode != firstdesc) && (hnode->processed != sec)); 
	
	if ((hnode != firstdesc) && (hnode->processed == sec)) {
		
		plan = FALSE;
	};
	
	return newpoint;
	
} /* test_exterior_nodes */




void update_exterior_face_situation (sec)
			/* Aktuallisiert die Situation der Knoten auf dem "Rand" des Graphen. */
int sec;

{
	struct node *h1, *h2;
	
	h1 = firstasc;
	h1->top = 0;
	
	if ((h1->exlast == NULL) || (h1->exnext == NULL)) {
		plan = FALSE;
		message ("Planar embedding is wrong.\n");
		return;
	};
	
	if (comp_direction == 0) { /* Graph normal. */
		if (h1->x_offset < h1->exlast->x_offset) {
			h1->in = 1;
		}
		else {
			h1->in = 0;
		};
	}
	else { /* Graph an Winkelhalbierender gespiegelt. */
		if (h1->x_offset < h1->exnext->x_offset) {
			h1->in = 1;
		}
		else {
			h1->in = 0;
		};
	};
	
	do {
		h2 = h1;
		
		if ((h1->exlast == NULL) || (h1->exnext == NULL)) {
			plan = FALSE;
			message ("Planar embedding is wrong.\n");
			return;
		};
		
		if (comp_direction == 0) {
			h1->processed = sec;
			h1 = h1->exnext;
		}
		else {
			h1->processed = sec;
			h1 = h1->exlast;
		};
		
		if (h1 == firstasc) {
			h1->top = 1;
		}
		else {
			if (h1 == firstdesc) {
				h1->top = 0;
			}
			else {
				h1->top = h2->top;
			};
		};
		
		if ((h2->in == 0) && (h2->x_offset > h1->x_offset)) {
			h1->in = 1;
		}
		else {
			if ((h2->in == 1) && (h2->x_offset <= h1->x_offset)) {
				h1->in = 0;
			}
			else {
				h1->in = h2->in;
			};
		};
	} while ((h1 != firstasc) && (h1->processed != sec));
	
	if ((h1 != firstasc) && (h1->processed == sec)) {
		plan = FALSE;
		return;
	};
	
} /* update_exterior_face_situation */




void change_coordinates ()
			/* "Spiegelt den Graphen an der Winkelhalbierenden im */
			/* ersten Quadranten.                                 */

{
	struct node *hgraph;
	int buf;
	
	hgraph = graph;
	
	if (comp_direction == 0) {
		comp_direction = 1;
	}
	else {
		comp_direction = 0;
	};
	
	while (hgraph != NULL) {
		buf = hgraph->x_offset;
		hgraph->x_offset = hgraph->y_coord;
		hgraph->y_coord = buf;
		
		buf = hgraph->x;      
		hgraph->x = hgraph->y;
		hgraph->y = buf;      
		
		hgraph = hgraph->next;
	};
	
} /* change_coordinates */



void zoom (s)
		/* Streckt den Graphen um das Doppelte (von der Ecke links unten aus). */
Sgraph s;

{
	Snode n;
	int x_shift, y_shift;
	
	x_shift = 0;
	y_shift = 0;
	
	find_origin (s);
	
	for_all_nodes(s, n) {
		n->x = new_zero_x + (2 * (n->x - new_zero_x));
		
		if ((n->x < 0) && (n->x < x_shift)) {
			x_shift = n->x;
		};
		
		n->y = new_zero_y - (2 * (new_zero_y - n->y));
		
		if ((n->y < 0) && (n->y < y_shift)) {
			y_shift = n->y;
		};
	} end_for_all_nodes(s, n);
	
	if ((x_shift < 0) || (y_shift < 0)) {
		if (x_shift < 0) {
			x_shift = x_shift - grid;
		};
		
		if (y_shift < 0) {
			y_shift = y_shift - grid;
		};
				
		for_all_nodes (s, n) {
			n->x = n->x - x_shift;
			n->y = n->y - y_shift;
		} end_for_all_nodes (s, n);
	};
	
} /* zoom */




int test_changes ()
			/* Prueft, ob sich beim letzten Kompressionsdurchgang */
			/* eine Koordinate geaendert hat.                     */

{
	struct node *hgraph, *lgraph;
	
	hgraph = graph;
	lgraph = lastgra;
	
	while ((hgraph != NULL) && (lgraph != NULL)) {
		if ((hgraph->x_offset != lgraph->x_offset)
		   || (hgraph->y_coord != lgraph->y_coord)) {
			return (TRUE);
		}
		else {
			hgraph = hgraph->next;
			
			lgraph = lgraph->next;
		};
	};
	
	return (FALSE);
	
} /* test_changes */



void store_last_graph ()
			/* Speichert den letzten Graphen in "lastgra" ab. */

{
	struct node *hgraph, *lgraph;

	hgraph = graph;
	
	if (lastgra != NULL) {
		free_the_graph (lastgra);
	};
	
	lastgra = (struct node *) malloc (sizeof (struct node));
	lgraph = lastgra;
	
	while (hgraph != NULL) {
		lgraph->x_offset = hgraph->x_offset;
		lgraph->y_coord = hgraph->y_coord;
		lgraph->neigh = NULL;
		
		if (hgraph->next != NULL) {
			lgraph->next = (struct node *) malloc (sizeof (struct node));
			lgraph = lgraph->next;
		}
		else {
			lgraph->next = NULL;
		};	
		
		hgraph = hgraph->next;
	};
	
} /* store_last_graph */
 



void change_y_coord_with_y ()
			/* Tauscht die "GraphEd"-Koordinaten mit den Ergebnis- */
			/* koordinaten. (Aus Speicherplatzgruenden).           */

{
	int buf;
	struct node *n;
	
	n = graph;
	
	while (n != NULL) {
		buf =  (-1) * (n->y);
		n->y = (-1) * (n->y_coord);
		n->y_coord = buf;
		n = n->next;
	};
	
} /* change_y_coord_with_y */




void make_exterior_face_path ()
			/* Verbindet die Knoten des "Randes" zu einer Liste. */

{
	struct node *maxn, *nd, *hnd;
	struct nlist *ng1, *ng2;
	
	maxn = max_node (graph);
	nd  = maxn;
	ng1 = nd->co_clockw;
	ng2 = nd->co_clockw;
	
	
	do {
		ng1->exterior2 = 1;
		(ng1->visited) = 1;	
		ng2 = left_neigh_at_nei (nd, ng1);
		nd->exterior = 1;               
		hnd = nd;
		nd = ng1->node;
		hnd->exlast = nd;
		nd->exnext = hnd;	
		ng1 = ng2;
	} while (nd != maxn);
	
} /* make_exterior_face_path */



void adjust_grid ()
			/* Prueft, ob der Eingabegraph auf einem Gitter liegt,    */
			/* und uebernimmt dieses bzw. bestimmt die Gittereinheit. */

{

	struct node *gra, *minx, *miny;
	int m_x, m_y, d_y;
	

	gra = graph;
	grid = 64;
	
	change_coordinates ();
	minx = max_node (gra);
	change_coordinates ();
	m_x = minx->x;
	
	miny = max_node (gra);
	m_y = miny->y;
	
	miny = min_node (gra);
	d_y = miny->y;

	while (gra != NULL) {
		if (((((gra->y) - m_y) % 64) == 0) && (grid >= 64)) {
			grid = 64;
		}
		else {
			if (((((gra->y) - m_y) % 32) == 0) && (grid >= 32)) {
				grid = 32;
			}
			else {
				if (((((gra->y) - m_y) % 16) == 0) && (grid >= 16)) {
					grid = 16;
				}
				else {
					grid = 1;
				};
			};
		};
		if (((((gra->x) - m_x) % 64) == 0) && (grid >= 64)) {
			grid = 64;
		}
		else {
			if (((((gra->x) - m_x) % 32) == 0) && (grid >= 32)) {
				grid = 32;
			}
			else {
				if (((((gra->x) - m_x) % 16) == 0) && (grid >= 16)) {
					grid = 16;
				}
				else {
					grid = 1;
				};
			};
		};
		gra = gra->next;
	};
	
	gra = graph;

	while (gra != NULL) {
		gra->x_offset = (((gra->x) - m_x) / grid);
		gra->y_coord  = ((d_y - (gra->y)) / grid);
		gra = gra->next;
	};


/*
	grid = chrobak_payne_settings.grid;
	
	gra = graph;

	while (gra != NULL) {
		gra->x_offset = (((gra->x) - m_x) / grid);
		gra->y_coord  = ((d_y - (gra->y)) / grid);
		gra = gra->next;
	};
*/
	
} /* adjust_grid */
