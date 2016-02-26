/* (C) Universitaet Passau 1986-1991 */
/*************************************************************************
 **                                                                     **
 **     "take_input.c" enthaelt die Programmteile, in denen die         **
 **     Eingabegraphen von der "GraphEd"-Datenstruktur an die           **
 **     programmeigene Adjazenzliste uebergeben werden.                 **
 **     Hier werden die Nachbarn der Knoten in eine zirkulare           **
 **     Ordnung gebracht. Die Gitterweite und der "Ursprungspunkt"      **
 **     fuer die Ausgabegraphen werden ebenfalls hier bestimmt.         **
 **                                                                     **
 *************************************************************************/




#include <math.h>
#include "decl.h"




void take_graph (sgraph)
			/* Baut die programmeigene Adjazenzliste auf. */
Sgraph sgraph;

{
	int i, k, list_size, size;
	struct node *pgraph;
	struct nlist *h;
	Snode node;
	Sedge edge, nextedge;
	Edgeline el;
	
	
	lastgra = NULL;
	
	plan = TRUE;
	
	new_zero_x = 100000;
	new_zero_y = 0;
		


	
	if ((sgraph == NULL) || (sgraph->nodes == NULL)) {
		plan = FALSE;
		return;
	};
	
	graph = (struct node *) malloc (sizeof(struct node));
	pgraph = firstdesc = graph;
	i = 0;
	
	for_all_nodes (sgraph, node) {
		gsize=i;
		pgraph->number      = node->nr;
		pgraph->x           = node->x;
		pgraph->y           = node->y;
		pgraph->degree      = 0;
		pgraph->up_degree   = 0;
		pgraph->dn_degree   = 0;
		pgraph->kind        = 0;
		pgraph->clockw      = NULL;
		pgraph->co_clockw   = NULL;
		pgraph->up          = NULL;
		pgraph->dn          = NULL;
		pgraph->dist        = 0;
		pgraph->label       = -1;
		pgraph->x_offset    = 0;
		pgraph->y_coord     = 0;
		pgraph->placenext   = NULL;
		pgraph->placelast   = NULL;
		pgraph->processed   = FALSE;
		pgraph->in_cn_list  = FALSE;
		pgraph->left        = NULL;
		pgraph->right       = NULL;
		pgraph->exterior    = 0;
		pgraph->exnext      = NULL;
		pgraph->exlast      = NULL;
		pgraph->in          = 0;
		pgraph->top         = 0;
		
		
		if ((node->x > 0) && (node->x < new_zero_x)) {
			new_zero_x = node->x;
		};
		
		if ((node->y > 0) && (node->y > new_zero_y)) {
			new_zero_y = node->y;
		};
		
		
		if (node->slist != NULL) {
			pgraph->neigh  = (struct nlist *) malloc (sizeof(struct nlist));
			h = pgraph->neigh;
			
			for_sourcelist (node, edge) {
				
				el = copy_edgeline ((Edgeline) edge_get (graphed_edge (edge), EDGE_LINE));

				while ((el != NULL) && (el->suc != NULL) && 
				      (el->suc->suc != NULL) && (el->suc->suc != el)) {
					el = remove_from_edgeline (el->suc);
				};
				
				edge_set (graphed_edge (edge),
					EDGE_LINE, el,
					0);
				
				nextedge = edge->ssuc;
				
				if (node->nr == edge->snode->nr) {
					h->nr = edge->tnode->nr;
				}
				else {
					h->nr = edge->snode->nr;
				}; 
				
				pgraph->degree++;
				h->orig      = 1;
				h->visited   = 0;
				h->exterior2     = 0;
				h->clockw    = NULL;
				h->co_clockw = NULL;
				h->angle     = 0.0;
				
				if (nextedge != node->slist) {
					h->next = (struct nlist *) malloc (sizeof(struct nlist));
					h = h->next;
				}
				else {
					h->next = NULL;
				};
				
			} end_for_sourcelist (node, edge);
		};
		
		if (node->tlist != NULL) {
			if (node->slist == NULL) {
				pgraph->neigh  = (struct nlist *) malloc (sizeof(struct nlist));
				h = pgraph->neigh;
			}
			else {
				h->next = (struct nlist *) malloc (sizeof(struct nlist));
				h = h->next;
			};
			
			for_targetlist (node, edge) {
				
				nextedge = edge->tsuc;
				
				if (node->nr == edge->snode->nr) {
					h->nr = edge->tnode->nr;
				}
				else {
					h->nr = edge->snode->nr;
				};
				
				pgraph->degree++;
				h->orig = 1;
				h->visited   = 0;
				h->exterior2     = 0;
				h->clockw    = NULL;
				h->co_clockw = NULL;
				h->angle     = 0.0;
				
				if (nextedge != node->tlist) {
					h->next = (struct nlist *) malloc (sizeof(struct nlist));
					h = h->next;
				}
				else {
					h->next = NULL;
				};
				
			} end_for_targetlist (node, edge);
		}
		else {
			if (node->slist == NULL) {
				pgraph->neigh = NULL;
			};
		};

		i++;
		
		if (node != last_node_in_graph(sgraph)) {
			pgraph->next = pgraph->descend = (struct node *) malloc (sizeof(struct node));
			pgraph->descend->ascend = pgraph;
			pgraph = pgraph->next;
		}
		else {
			pgraph->next = NULL;
			pgraph->descend = firstdesc;
			firstdesc->ascend = pgraph;
			firstasc = pgraph;
		};
		
	} end_for_all_nodes (sgraph, node);
	
	grid = 0;
	
	adjust_grid ();
	find_grid_origin ();
		
	pgraph = NULL;
	fill_neighbors ();
	sort_y_coordinates ();
	sort_neighbors ();
	
	if (plan == FALSE) {
		message ("Planar embedding is wrong!\n");
		bell ();
		return;
	};
	
	sort_neighbors2 ();
	
} /* take_graph */




double angle (node1, node2)
			/* Berechnet die Winkel der Kanten zw. den Knoten und */
			/* ihren Nachbarn.                                    */
struct node *node1, *node2;
{
	int y, x1, y1, x2, y2;
	double angle1;
	x1 = node1->x;
	y1 = -node1->y;
	x2 = node2->x;
	y2 = -node2->y;
	y = (y2 - y1);
	
	if ((x2 != x1) && (y != 0)) {
		angle1 = atan((double)((double)(x2 - x1)/(double)y));
	}
	else {
		if (y == 0) {
			angle1 = M_PI_2;
		}
		else {
			angle1 = 0.0;
		};
	};
	
	if ((x2 >= x1) && (y2 >= y1)) {
			angle1 = angle1;
	}
	else {
		if ((x2 < x1) && (y2 > y1)) {
			angle1 = (2 * M_PI) + angle1;
		}
		else {
			angle1 = M_PI + angle1;		};
	};

	return angle1;
	
} /* angle */




void sort_neighbors ()
			/* Bringt die Nachbarn der Knoten in eine zirkulare Ordnung. */

{
	struct node *gra;
	struct nlist *nei, *buf, *buf1, *buf2;
	gra = graph;
	
	while (gra != NULL) {
		nei = gra->neigh;
		
		if (nei != NULL) {
			gra->clockw = gra->neigh;
			gra->co_clockw = gra->neigh;
			
			if (gra->number == nei->node->number) {
				plan = FALSE;
				return;
			}
			else {
				nei->angle = angle (gra, nei->node);	
			};
					
			nei = nei->next;
			
			if (gra->neigh->next == NULL) {
				plan = FALSE;
				return;
			};
			
			while (nei != NULL) {
						
				if (gra->number == nei->node->number) {
					plan = FALSE;
					return;
				}
				else {
					nei->angle = angle (gra, nei->node);
				};
				
				if (gra->clockw->angle > nei->angle) {
					buf = gra->clockw;
					gra->clockw = nei;
					nei->clockw = buf;
					buf->co_clockw = nei;
				}
				else {
					if (gra->clockw->angle < nei->angle) {
						buf2 = gra->clockw;
							buf1 = buf2->clockw;
							
							while ((buf1 != NULL) && (buf1->angle < nei->angle)) {
								buf2 = buf1;
								buf1 = buf1->clockw;
							};
							
							nei->clockw = buf1;
							buf2->clockw = nei;
							nei->co_clockw = buf2;
							
							if (buf1 != NULL) {
								buf1->co_clockw = nei;
							} 
							else {
								gra->co_clockw = nei;
							};
					}
					else {
						plan = FALSE;
						return;
					};
				};
				nei = nei->next;
			};
		}
		else {
			plan = FALSE;
			return;
		};
	
		gra->clockw->co_clockw = gra->co_clockw;
		gra->co_clockw->clockw = gra->clockw;
		gra = gra->next;
	};
	
} /* sort_neighbors */




void find_origin (sgraph)
			/* Sucht die linke untere Ecke eines gedachten */
			/* Rechtecks um den Graphen.                   */
Sgraph sgraph;

{
	Snode node;
	
	new_zero_x = 100000;
	new_zero_y = 0;
	
	for_all_nodes (sgraph, node) {
		if ((node->x > 0) && (node->x < new_zero_x)) {
			new_zero_x = node->x;
		};
	
		if ((node->y > 0) && (node->y > new_zero_y)) {
			new_zero_y = node->y;
		};
	} end_for_all_nodes (sgraph, node);

} /* find_origin */




int adjust_grid_cp ()
			/* Bestimmt die Gittereinheit fuer den Graphen. */
			
{
	int size, gr;
/*
	size = 2 * (gsize + 1) - 4;
	
	if (size <= 17) {
		gr = 64;
	}
	else {
		if (size <= 35) {
			gr = 32;
		}
		else {
			gr = 16;
		};
	};
	
	return (gr);
*/

	return	chrobak_payne_settings.grid / 2;

} /* adjust_grid_cp */




void sort_neighbors2 ()
			/* Ordnet die Nachbarn der Knoten nach "absteigenden" (dn) */
			/* und "aufsteigenden" (up) Nachbarn.                      */

{
	struct node *gra;
	struct nlist *cl, *h_up, *h_dn;
	int flag;
	
	gra = graph;
	
	while (gra != NULL) {
		gra->up = NULL;
		gra->dn = NULL;
		gra->up_degree = 0;
		gra->dn_degree = 0;
		flag = FALSE;
		cl = gra->clockw;
		
		do {
			if ((0.0 <= cl->angle) && (cl->angle <= M_PI_2)) {
				if (gra->up == NULL) {
					gra->up = cl;
					flag = TRUE;
				};
				gra->up_degree++;
			}
			else {
				if ((M_PI_2 < cl->angle) && (cl->angle <= (3*M_PI_2))) {
					if (gra->dn == NULL) {
						gra->dn = cl;
					};
					
					gra->dn_degree++;
				}
				else {
						if ((gra->up == NULL) || (flag == TRUE)) {
							gra->up = cl;
							flag = FALSE;
						};
						
						gra->up_degree++;
				};
			};
			cl = cl->clockw;
			
		} while (cl != gra->clockw);
		
		if ((gra->up_degree == 0) && (gra->dn_degree > 0)) {
			gra->kind = -1;
		}
		else {
			if ((gra->up_degree > 0) && (gra->dn_degree == 0)) {
				gra->kind = 1;
			}
			else {
				gra->kind = 0;
			};
		};

		gra = gra->next;
	};
	
} /* sort_neighbors2 */
