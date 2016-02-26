/* (C) Universitaet Passau 1986-1991 */
/****************************************************************
 **                                                            **
 **     "regular.c" enthaelt die Programmteile, die den        **
 **     Eingabegraphen regularisieren.                         **
 **                                                            **
 ****************************************************************/


#include "decl.h"




void set_kind_of_nodes (poly)
			/* Untersucht, ob die Knoten regulaer sind. */
struct polygon *poly;

{
	struct polynode *pn1, *pn2;
	
	pn1 = poly->firstnode;
	pn2 = pn1->next;
	
	do {
		if (pn1->pnode->y < pn2->pnode->y) {
			(pn2->kind)++;
			(pn1->kind)--;
		}
		else {
			if (pn1->pnode->y > pn2->pnode->y) {
				(pn1->kind)++;
				(pn2->kind)--;
			};
		};
	
		pn1 = pn1->next;
		pn2 = pn1->next;
	} while (pn1 != poly->firstnode);
	
} /* set_kind_of_nodes */




void sort_y_coordinates ()
			/* Ordnet die Knoten nach ihren y-Werten. */

{
	struct node *p, *pn0, *pn1, *pn2, *local, *maxele, *pri;
	int is_sort, loop;
	
	p = graph;
	loop = 0;
	maxele = firstdesc;
	pri = p;
	
	do {
		pn0 = firstdesc->ascend;
		pn1 = firstdesc;
		pn2 = pn1->descend;
		is_sort = TRUE;
	
		if ((pn1->y > pn2->y) || ((pn1->y == pn2->y) && (pn1->x < pn2->x))) {
			firstdesc = pn2;
		};
		
		do {	
			if ((pn1->y > pn2->y) || ((pn1->y == pn2->y) && (pn1->x < pn2->x))) {
				is_sort = FALSE;
				local = pn1;
				
				pn0->descend = pn2;
				pn2->ascend  = pn0;
				pn1->descend = pn2->descend;
				pn2->descend->ascend = pn1;
				pn2->descend = pn1;
				pn1->ascend = pn2;
				pn0 = pn2;
				pn2 = pn1->descend;
			}
			else {
				local = pn2;
				pn0 = pn1;
				pn1 = pn2;
				pn2 = pn2->descend;
			};		   
		} while ((pn2 != maxele) && (pn2 != firstdesc));
	
		maxele = local;
		loop++;
	} while ((loop != gsize) && (is_sort == FALSE));
	
	firstasc = firstdesc->ascend;
	
} /* sort_y_coordinates */




void sort_y_coordinates_2 (p)
			/* Ordnet die Knoten von Polygon "p" nach y-Werten. */
struct polygon *p;

{
	struct polynode *pn0, *pn1, *pn2, *local, *maxele, *loop, *pri;
	int is_sort;
	
	maxele = p->firstdesc;
	pri = loop = p->firstnode;
	
	do {
		pn0 = p->firstdesc->ascend;
		pn1 = p->firstdesc;
		pn2 = pn1->descend;
		is_sort = TRUE;
	
		if (pn1->pnode->y > pn2->pnode->y) {
			p->firstdesc = pn2;
		};
		
		do {	
			if (pn1->pnode->y > pn2->pnode->y) {
				is_sort = FALSE;
				local = pn1;
				
				pn0->descend = pn2;
				pn2->ascend  = pn0;
				pn1->descend = pn2->descend;
				pn2->descend->ascend = pn1;
				pn2->descend = local;
				local->ascend = pn2;
				
				pn0 = pn2;
				pn2 = pn1->descend;
			}
			else {
				local = pn2;
				
				pn0 = pn1;
				pn1 = pn2;
				pn2 = pn2->descend;
			};		   
		} while ((pn2 != maxele) && (pn2 != p->firstdesc));
	
		maxele = local;
		loop = loop->next;
	} while ((loop != p->firstnode->last) && (is_sort == FALSE));
	
	p->firstasc = p->firstdesc->ascend;
	
} /* sort_y_coordinates_2 */




void regularisation (sgraph)
			/* Hauptprozedur zur Regularisierung. */
Sgraph sgraph;

{
	struct node      *nd;
	struct location_edge *ll;
	int direction;
	
	mark_exterior_face ();
	
	if (plan == FALSE) {
		return;
	};
	
	exterior_face_situation ();
	
	if (plan == FALSE) {
		return;
	};
	
	direction = -1;
	nd = firstdesc;
	init_location_list (nd);	
	ll = location_list;
	insert_edges_in_location_list (nd, ll, direction);
	nd = nd->descend;

	while (nd != firstasc->ascend) {
		regular_one_direction (sgraph, nd, direction);
		nd = nd->descend;
	};
	
	direction = 1;
	nd = firstasc;
	init_location_list (nd);
	ll = location_list;
	insert_edges_in_location_list (nd, ll, direction);
	nd = nd->ascend;

	while (nd != firstdesc->descend) {
		nd->kind = ((-1) * (nd->kind));
		regular_one_direction (sgraph, nd, direction);
		nd = nd->ascend;
	};
		
} /* regularisation */




int at_left_of_edge (v, n1, n2)
			/* Prueft, ob Knoten 'v" links von Kante "(n1, n2)" liegt. */
struct node *v, *n1, *n2;

{
	struct node *a, *b;
	int sp, left;
	
	if (n1->y < n2->y) {
		a = n1;
		b = n2;
	}
	else {
		a = n2;
		b = n1;
	};
	
	sp = (((a->y - b->y) * (v->x - a->x)) + ((b->x - a->x) * (v->y - a->y)));
	
	if (sp > 0) {
		left = 1;
	}
	else {
		left = 0;
	};
	
	return left;
	
} /* at_left_of_edge */




void insert_edges_in_location_list (n, ll, direction)
			/* Fuegt die Kanten, die unterhalb von Knoten "n" */
			/* liegen, in die Liste "ll" ein.                 */
struct node *n;
struct location_edge *ll;
int direction;

{
	struct location_edge *ll_buf;
	struct nlist *nei, *last_nei;
	
	ll_buf = ll->next;
	
	if (direction == -1) {
		if (n->kind == 0) {
			nei = n->up->co_clockw;
			last_nei = n->dn->co_clockw;
		}
		else {
			last_nei = nei = n->co_clockw;
		};
	}
	else {
		if (n->kind == 0) {
			nei = n->up;
			last_nei = n->dn;
		}
		else {
			last_nei = nei = n->up;
		};
	};
	
	do {
		ll->next = (struct location_edge *) malloc (sizeof (struct location_edge));
		ll->next->last = ll;
		ll = ll->next;
		ll->rel_node = n;
		ll->node1 = n;
		ll->node2 = nei->node;
		
		if (direction == -1) {
			nei = nei->co_clockw;
		}
		else {
			nei = nei->clockw;
		};
		
	} while (nei != last_nei);
	
	ll->next = ll_buf;
	if (ll->next != NULL) {
		ll->next->last = ll;
	};
	
} /* insert_edges_in_location_list */




struct location_edge * delete_edges_in_location_list (n, ll)
			/* Loescht die Kanten oberhalb von Knoten "n" aus */
			/* der Liste "ll".                                */
struct node *n;
struct location_edge *ll;

{
	struct location_edge *hll, *hll_buf, *free_ll;
	
	hll = location_list;
	
	while ((hll != NULL) && (hll->node1 != n) && (hll->node2 != n)) {
		hll = hll->next;
	};
	
	if (hll != NULL) {
		hll->last->rel_node = n;
		hll_buf = hll->last;
		
		while ((hll != NULL) && ((hll->node1 == n) || (hll->node2 == n))) {
			free_ll = hll;
			hll = hll->next;
			free (free_ll);
		};
		
		hll_buf->next = hll;
		
		if (hll != NULL) {
			hll->last = hll_buf;
		};
		
		ll = hll_buf;
	};
	
	return ll;
	
} /* delete_edges_in_location_list */




void init_location_list (n)
			/* Initialisiert die Kantenliste fuer die Regularisierung */
			/* in eine Richtung.                                      */
struct node *n;

{
	struct location_edge *ll;
	struct node *dummy1, *dummy2;

	location_list = (struct location_edge *) malloc (sizeof (struct location_edge));
	ll = location_list;
	ll->rel_node = n;
	dummy1 = (struct node *) malloc (sizeof (struct node));
	dummy2 = (struct node *) malloc (sizeof (struct node));
	dummy1->number = -1;
	dummy2->number = -2;
	dummy1->x = -1;
	dummy1->y = 10;
	dummy2->x = -1;
	dummy2->y = -10;
	ll->node1 = dummy1;
	ll->node2 = dummy2;
	ll->last = NULL;
	ll->next = NULL;
	
} /* init_location_list */




void regular_one_direction (sgraph, n, direction)
			/* Regularisiert den graphen in eine Richtung. */
Sgraph sgraph;
struct node *n;
int direction;

{
	struct location_edge *ll;
	int left, last;
	
	ll = location_list;
	left = FALSE;
	last = FALSE;
	
	while ((left == FALSE) && (ll->next != NULL)) {
		ll = ll->next;
		left = at_left_of_edge (n, ll->node1, ll->node2);
	};
	
	if (left == TRUE) {
		ll = ll->last;
	};
	
	if (n->kind < 0) {
		insert_edges_in_location_list (n, ll, direction);
		
		if ((is_neighbor (ll->rel_node, n) == FALSE) && 
		   (exterior_edge (n, direction) == FALSE) /*(ll != location_list) &&
		   (ll->next != NULL) && (last == FALSE)*/) {
			add_edge (sgraph, ll->rel_node->number, n->number);
		};
		
		last = FALSE;
		ll->rel_node = n;
	}
	else {
		if (n->kind > 0) {
			ll = delete_edges_in_location_list (n, ll);
		}
		else {
			ll = delete_edges_in_location_list (n, ll);
			insert_edges_in_location_list (n, ll, direction);
		};
	};

} /* regular_one_direction */




void exterior_face_situation ()
			/* Bestimmt die Situation der "Rand"-Knoten (Lage am Graphen). */

{
	struct node *h1, *h2;
	
	h1 = firstasc;
	h1->top = 0;
	
	if (h1->exnext == NULL) {
		message ("Planar embedding is wrong!\n");
		bell ();
		plan = FALSE;
		return;
	};
	
	if ((h1->exnext != NULL) && (h1->x < h1->exlast->x)) {
		h1->in = 1;
	}
	else {
		h1->in = 0;
	};
	
	do {
		h2 = h1;
		
		if (h1->exnext != NULL) {
			h1 = h1->exnext;
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
		
		if ((h2->in == 0) && (h2->x > h1->x)) {
			h1->in = 1;
		}
		else {
			if ((h2->in == 1) && (h2->x <= h1->x)) {
				h1->in = 0;
			}
			else {
				h1->in = h2->in;
			};
		};
		
	} while ((h1 != firstasc) && (h1->exnext != NULL));
	
	if ((h1 != firstasc) && (h1->exnext == NULL)) {
		message ("Planar embedding is wrong!\n");
		bell ();
		plan = FALSE;
		return;
	};
	
} /* exterior_face_situation */




int exterior_edge (n, direction)
			/* Prueft, ob eine Regularisierungskante ausserhalb des */
			/* Graphen liegt.                                       */
struct node *n;
int direction;

{
	int ret = 0;
	
	if (n->exterior == 1) {
		if (((direction == -1) && (n->in == 0) && (n->exnext->in == 0)) ||
		   ((direction == 1) && (n->in == 1) && (n->exnext->in == 1)) ||
		   ((n->in == 0) && (n->exnext->in == 1) && (n->clockw->node == n->exnext)) ||
		   ((n->in == 1) && (n->exnext->in == 0) && (n->clockw->node == n->exnext)))     {
			ret = 1;
		};
	};
	
	return ret;
	
} /* exterior_edge */
