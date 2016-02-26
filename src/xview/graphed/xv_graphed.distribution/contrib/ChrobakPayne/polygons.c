/* (C) Universitaet Passau 1986-1991 */
#include "decl.h"


/*
struct polynode {              / Knoten eines Polygons. /
	struct node       *pnode;
	struct polynode   *ascend;
	struct polynode   *descend;
	struct polynode   *next;
	struct polynode   *last;
	int               kind;  / Regularisierung. /
};

struct polygon {                / Element der Polygonliste. /
	int             nr;
	struct polynode *firstnode;
	struct polynode *firstasc;
	struct polynode *firstdesc;
	struct polygon  *nextpoly;
};

struct polygon *polygonlist, *plist, *exnode;
int pcounter;
*/


void mark_exterior_face ()

{
	struct node  *gra, *nd, *hnd;
	struct nlist *ng1, *ng2, *test_ng1, *test_ng2;
	
	gra = firstdesc;
	nd  = gra;
	ng1 = nd->co_clockw;
	ng2 = nd->co_clockw;
	
	do {                             /* "exterior" des Randes auf "1" setzen. */
		ng1->exterior2 = 1;
	/*	(ng1->visited) = 1;
		
		printf ("\n v");
	*/	
		test_ng1 = ng1;
		test_ng2 = ng2;
		
		ng2 = left_neigh_at_nei (nd, ng1);
		nd->exterior = 1;               /* Wird in "compression.c" benutzt. */
		
		nd->processed = TRUE;
		
		hnd = nd;
		nd = ng1->node;
		hnd->exlast = nd;
		nd->exnext = hnd;
		ng1 = ng2;
	} while ((nd != gra) && (nd->exterior != 1));
	
	if ((nd != gra) && (nd->exterior == 1)) {
		message ("Planar embedding is wrong!\n");
		bell ();
		plan = FALSE;
		return;
	};
}

	
	
void decomposition (s)
Sgraph s;

{	
	struct node  *minn, *maxn, *gra, *nd, *hnd;
	struct nlist *ng1, *ng2;
	struct polynode *p;
	
	gra = firstdesc;
	pcounter = 0;

	nd  = gra;    
	ng1 = nd->co_clockw;
	ng2 = nd->co_clockw;
	

	
	do {                             /* "visited" des Randes auf "1" setzen. */
		ng1->exterior2 = 1;
		
		if ((nd->processed) == TRUE) {
			(ng1->visited) = 1;
			nd->processed = FALSE;
		}
		else {
			message ("Planar embedding is wrong!\n");
			bell ();
			plan = FALSE;
			return;
		};
		
	/*	
		printf ("\nA: nr: %d;", ng1->nr);
	*/	
		ng2 = left_neigh_at_nei (nd, ng1);
		nd->exterior = 1;               /* Wird in "compression.c" benutzt. */
		
		/*nd->processed = FALSE;*/
		
		hnd = nd;
		nd = ng1->node;
		hnd->exlast = nd;
		nd->exnext = hnd;
		ng1 = ng2;
	} while (nd != gra);

	
	

	polygonlist = plist = (struct polygon *) malloc (sizeof (struct polygon));
	
	while ((gra != NULL) && (gra != firstasc)) {
		ng1 = gra->clockw;
	/*	
		printf ("\n\n%d:", gra->number);
	*/	
		
		do {
		/*
			printf ("\n Edge %d - %d:", gra->number, ng1->nr);
		*/	
			if ((ng1 != NULL) && (ng1->visited == 0)) {
		/*		
				printf ("vis. = %d, ", ng1->visited);
		*/		
				store_polygon (gra, ng1);
				
				if (plan == FALSE) {
					return;
				};
				
		/*	}
			else {
				if (ng1 != NULL) {	
					printf ("\nV: nd: %d: nei: %d;", gra->number, ng1->nr);
				}
				else {
					printf ("\nN: nd: %d, nei: %d;", gra->number, ng1->nr);
				};*/
			};
			
			ng1 = ng1->clockw;
		} while ((ng1 != NULL) && (ng1 != gra->clockw));
		
		gra = gra->descend;
	};
	plist->nextpoly = NULL;
	

}


void store_polygon (nd, nei)
struct node  *nd;
struct nlist *nei;

{
	struct node *hnd;
	struct nlist *hnei1, *hnei2;
	struct polynode *pn;
/*	int is_exnode = 1;
	
	hnd = nd;
	hnei1 = nei;
	hnei2 = nei;
*/	
		
	if (pcounter != 0) {
		plist->nextpoly = (struct polygon *) malloc (sizeof (struct polygon));
		plist = plist->nextpoly;
	};
		
	pn  = (struct polynode *) malloc (sizeof (struct polynode));
	plist->firstnode = plist->firstdesc = pn;
	pcounter++;
	plist->nr = pcounter;
		
	hnd = nd;
	hnei1 = nei;
	hnei2 = nei;
	/*
	printf ("\n store nr. %d:", plist->nr);
	*/	
	
	do {
		(hnei1->visited) = 1;
		pn->pnode = hnd;
		pn->kind = 0;
		/*pn->ascend = NULL;
		pn->descend = NULL;*/
			
	/*	
		printf ("\nS: nd: %d, nei: %d; ", hnd->number, hnei1->nr);
	*/	
	
		hnd->processed = TRUE;
		
		hnei2 = left_neigh_at_nei (hnd, hnei1);
		hnd = hnei1->node;
		hnei1 = hnei2;
			
		if (hnd != nd) {
			pn->descend = pn->next = (struct polynode *) malloc (sizeof (struct polynode));
			pn->descend->ascend = pn->next->last = pn;
			pn = pn->next;
		}
		else {
			pn->descend = pn->next = plist->firstnode;
			/*pn->descend = NULL;*/
			plist->firstnode->ascend = plist->firstnode->last = pn;
			plist->firstasc = pn;
		};
	} while ((hnd != nd) && ((hnd->processed) == FALSE))/*&& ((hnei1->visited) == 0)*/;
	
	if ((hnd != nd) && ((hnd->processed) == TRUE))/*((hnei1->visited) == 1)*/ {
		plan = FALSE;
		message ("Planar embedding is wrong!\n");
		bell ();
		return;
	}
	else {
		pn = plist->firstnode;
		
		do {
			pn = pn->next;
		/*	
			printf ("\nF: nd: %d;", pn->pnode->number);
		*/
			pn->pnode->processed = FALSE;
		} while (pn != (plist->firstnode));
	};
		
	merge_chains_of_polygon (plist);
/*	
	printf ("firstasc: %d;\n", plist->firstasc->pnode->number);
*/
}



struct nlist *left_neigh_at_nei (nd, nei)
struct node  *nd;
struct nlist *nei;

{
	struct node  *hnd;
	struct nlist *hnei;
/*	
	printf ("\n  left_neigh_at_nei (%d, %d)  ", nd->number, nei->nr);
*/	
	hnd = nei->node;
	hnei = hnd->neigh;
	
	while ((hnei != NULL) && (hnei->node != nd)) {
		hnei = hnei->next;
	};
	
	/*if (hnei->co_clockw->nr == nd->nr) {
		planar = FALSE;
	};*/
	
	return (hnei->co_clockw);
}



struct node *max_node (node)
struct node *node;

{
	struct node *maxn;
	maxn = node;
	
	while (node != NULL) {
		node = node->next;
		if ((node !=NULL) && (node->y < maxn->y)) {
			maxn = node;
		};
	};
	return maxn;
}



struct node *min_node (node)
struct node *node;

{
	struct node *minn;
	minn = node;
	
	while (node != NULL) {
		node = node->next;
		if ((node !=NULL) && (node->y > minn->y)) {
			minn = node;
		};
	};
	return minn;
}



void merge_chains_of_polygon (plist)
struct polygon *plist;

{
	struct polynode *p, *l, *r, *new;
	
	p = plist->firstdesc;
	l = p->last;
	r = p->next;
	while (r != l) {
		if (l->pnode->y < r->pnode->y) {
			new = l;
			l = l->last;
		}
		else {
			new = r;
			r = r->next;
		};
		p->descend = new;
		new->ascend = p;
		p = new;
	};
	p->descend = r;
	r->ascend = p;
	/*r->descend = NULL;*/
	plist->firstasc = r;
}


void polygon_triangulation (sgraph, plist)
Sgraph sgraph;
struct polygon *plist;

{
	struct polynode *pn, *stack, *s, *top, *bottom;
	int e1, e2, right_chain, number_of_pnodes = 0, count_added_edges = 0;
	
	
	pn = plist->firstdesc;
	do {
		number_of_pnodes++;
		pn = pn->next;
	} while (pn != plist->firstdesc);
	
	stack = top = plist->firstdesc->descend;
	bottom = plist->firstdesc;
	
	pn = top->descend;
	do {
		e1 = is_neighbor (pn->pnode, bottom->pnode);
		e2 = is_neighbor (pn->pnode, top->pnode);
		
		if ((e1 == TRUE) && (e2 == FALSE)) {
		/*
			printf ("a: bot. %d, top %d, pn %d;\n", 
			       bottom->pnode->number, top->pnode->number, pn->pnode->number);
		*/	
			s = bottom->descend;
			while (s != top->descend) {
				add_edge (sgraph, s->pnode->number, pn->pnode->number);
				count_added_edges++;
				s = s->descend;
			};
			bottom = top;
			top = pn;
		}
		else {
			if ((e2 == TRUE) && (e1 == FALSE)) {
			/*
				printf ("b: bot. %d, top %d, pn %d;\n",
				       bottom->pnode->number, top->pnode->number, pn->pnode->number);
			*/	
				if (top == pn->last) {
					right_chain = TRUE;
				}
				else {
					right_chain = FALSE;
				};
				s = top->ascend;
				while ((top != bottom) 
				      && (convex_angle (right_chain, s->pnode, pn->pnode, top->pnode) == TRUE)) {
					add_edge (sgraph, s->pnode->number, pn->pnode->number);
					count_added_edges++;
					top = s;
					s = s->ascend;
				};
				top->descend = pn;
				pn->ascend = top;
				top = pn;
			}
			else {
			/*	
				printf ("c: bot. %d, top %d, pn %d;\n",
				       bottom->pnode->number, top->pnode->number, pn->pnode->number);
			*/		
				s = bottom->descend;
				while (s != top) {
					add_edge (sgraph, s->pnode->number, pn->pnode->number);
					count_added_edges++;
					s = s->descend;
				};
				top = pn;
			};
		};
		pn = pn->descend;
	/*	
		printf ("\ne1: %d, e2: %d; cae: %d, nop: %d", e1, e2, count_added_edges, number_of_pnodes);
	*/
	
	} while (/*((e1 == FALSE) || (e2 == FALSE)) ||*/ (count_added_edges != (number_of_pnodes-3)));
}


int convex_angle (right_chain, v, n1, n2)
int right_chain;
struct node *v, *n1, *n2;

{
	int ca;
	
	if (right_chain == TRUE) {
		ca = at_left_of_edge (v, n1, n2);
	}
	else {
		v->x *= (-1);
		n1->x *= (-1);
		n2->x *= (-1);
		ca = at_left_of_edge (v, n1, n2);
		v->x *= (-1);
		n1->x *= (-1);
		n2->x *= (-1);
	};
	return ca;
}
