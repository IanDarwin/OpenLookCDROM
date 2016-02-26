/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************
 **                                                                  **
 **     "numbering.c" enthaelt die Prozeduren, mit deren Hilfe       **
 **     die Kanonische Numerierung erstellt wird.                    **
 **                                                                  **
 **********************************************************************/


#include "decl.h"
#include <math.h>


void initialisation ()
			/* Bestimmung des Startdreiecks fuer die Kanonische */
			/* Numerierung.                                     */

{
	struct node *pgraph, *hgraph, *htree;
	int    i, j, y, hy;
	double angle1, hangle;
	
	pgraph = graph;
	first = pgraph;
	
	for (i = 1; pgraph != NULL; pgraph = pgraph->next, i++) {
		if ((pgraph->y > first->y) ||
		   ((pgraph->y == first->y) && (pgraph->x < first->x))) {
			first = pgraph;
		};
	};
	
	first->x_offset  = 0;
	first->y_coord   = 0;
	first->left      = NULL;
	firstcont        = first;
	prelast = knode1 = first;
	
	second = first->up->co_clockw->node;
	third  = first->up->co_clockw->co_clockw->node;
		
	first->right    = third;
	third->x_offset = 1;
	third->y_coord  = 1;
	third->left     = NULL;
	third->right    = second;
	place           = third;
	
	second->x_offset  = 1;
	second->y_coord   = 0;
	second->left      = NULL;
	second->right     = NULL;
	lastcont = knode2 = second;
	
	first->label  = 2;
	second->label = 2;
	third->label  = 2;
	
	bintree = first;
	
} /* initialisation */



int is_neighbor (contnode, testnode)
			/* Testet, ob "contnode" und "testnode" benachbart sind. */
struct node *contnode, *testnode;

{
	struct nlist *hneigh;
	
	hneigh = testnode->neigh;
	
	while ((hneigh != NULL) && (contnode != NULL)) {
		if (hneigh->node->number == contnode->number)
			return(TRUE);
		hneigh = hneigh->next;
	};
	
	return(FALSE);
	
} /* is_neighbor */



void contour_neighbors (testnode)
			/* Bestimmt die Konturnachbarn von "testnode". */
struct node *testnode;

{
	struct node *hnode;
	
	hnode = bintree;
	
	while ((hnode != NULL) && (is_neighbor(hnode, testnode) == FALSE)) {
		hnode = hnode->right;
	};
	
	if ((hnode != NULL) && (is_neighbor(hnode, testnode) == TRUE)) {
		fc = hnode;
		pl = hnode;
		
		hnode = hnode->right;
	};
	
	while ((hnode != NULL) && (is_neighbor(hnode, testnode) == TRUE)) {
		lc = hnode;
		hnode = hnode->right;
		
		if ((hnode != NULL) && (is_neighbor(hnode, testnode) == TRUE)) {
			pl = lc;
		};
	};
	
	firstcont = fc;
	lastcont  = lc;
	prelast   = pl;
	
} /* contour_neighbors */




void canonical_numbering ()
			/* Bestimmt die Kanonische Numerierung  des Graphen. */

{
	struct node *process, *cn_list_end, *p, /**pb, *pbuffer,*/ *nprocess;
	struct nlist *nei;
	int last, i, pro, pcounter;
	
	first->in_cn_list = TRUE;
	first->label = -1;
	cn_list = first;
	second->placelast = first;
	second->in_cn_list = TRUE;
	second->label = -1;
	first->placenext = second;
	third->label = -1;
	cn_list_end = second;
	process = cn_list;
	i = 1;
	
	/*pbuffer = (struct node *) malloc (sizeof (struct node));*/
	
	while (process != NULL) {
		/*pbuffer = NULL;
		pb = pbuffer;*/
		process->processed = i;
		i++;
		pcounter = 0;
		nei = process->co_clockw;
		
		do {
			if ((nei != NULL) && (nei->node->label != 1)) {  /* 1 */
				if (nei->node->label == -1) {                /* 2 */
					nei->node->label = 0;
				}
				else {
					if ((nei->node->label == 0) &&           /* 3 */
					   (neighboring_edge_test (process, nei) > 0)) {
					   		pro = placing_test (nei, process);
							
							if (pro < 2) {           /* 10 */
								nei->node->label = 1;
							};     /* 10 */
					}
					else {
						if ((nei->node->label == 0) &&     /* 11 */
						   (neighboring_edge_test (process, nei) == 0)) {
							nei->node->label = 2;
						}
						else {
							if ((nei->node->label > 1) &&            /* 12 */
							   (neighboring_edge_test (process, nei) == 2)) {
								nei->node->label = (nei->node->label) - 1;
								
								if (nei->node->label == 1) {
									pro = placing_test (nei, process);
								
									if (pro < 2) {           /* 10 */
										nei->node->label = 1;
									}
									else {
										nei->node->label = 0;
									};
								};
							}
							else {
								if ((nei->node->label >= 1) &&        /* 13 */
								   (neighboring_edge_test (process, nei) == 0)) {
									nei->node->label = (nei->node->label) + 1;
									
								}; /* 13 */
							}; /* 12 */
						}; /* 11 */
					}; /* 3 */
				}; /* 2 */
				
				if ((nei->node->label == 1) &&          /* 14 */
				   (nei->node->in_cn_list == FALSE)) {
					nei->node->in_cn_list = TRUE;
					cn_list_end->placenext = nei->node;
					nei->node->placelast = cn_list_end;
					cn_list_end = nei->node;
				};       /* 14 */
				
				if ((nei->node->label > 1) && (nei->node->in_cn_list == TRUE)) {    /* 15 */
					nei->node->in_cn_list = FALSE;
					
					if (nei->node->placelast != NULL) {           /* 16 */
						nei->node->placelast->placenext = nei->node;
					};              /* 16 */
					
					if (nei->node->placenext == NULL) {    /* 17 */
						cn_list_end = nei->node->placelast;
					}
					else {
						nei->node->placenext->placelast = nei->node->placelast;
					};        /* 17 */
					
					nei->node->placelast = NULL;
					nei->node->placenext = NULL;
				}; /* 15 */
			}; /* 1 */
			
			nei = nei->co_clockw;
			
		} while (nei != process->co_clockw);    /* do, "process" - Nachbarn durchlaufen */			
		
		
		process = process->placenext;	
	};   /* while, "process" durchlaufen */
	
} /* canonical_numbering */




int placing_test (neighbor, process)
			/* Prueft, ob "process" plaziert werden darf. */

struct nlist *neighbor;
struct node *process;

{
	struct nlist *nei, *nei2, *nei3, *nei4;
	struct node *pro_node1, *pro_node2;
	int pro, last_pro, side, ex_situation;
	
	nei = neighbor;
	nei2 = nei->node->co_clockw;
	
	if (nei->clockw->node->processed > 0) {
		side = 1;
		pro_node1 = nei->clockw->node;
		pro_node2 = nei->clockw->clockw->node;
	}
	else {
		side = -1;
		pro_node1 = nei->co_clockw->node;
		pro_node2 = nei->co_clockw->co_clockw->node;
	};
	
	if ((is_neighbor (pro_node1, pro_node2) == TRUE) && (pro_node1 != nei->node)) {
		ex_situation = 1;
	}
	else {
		ex_situation = 0;
	};
					   		
	if (nei2->clockw->node->in_cn_list == 1) {  /* 4 */
		if ((nei2->clockw->node->exterior == 1)) {  /* 5 */
			last_pro = 2;
		}
		else {
			last_pro = 1;
		}; /* 5 */
	}
	else {
		last_pro = 0;
	}; /* 4 */
						
	pro = 0;
						
	if ((nei->node != first) && (nei->node != second) && (nei->node != third)) { /* 6 */
		do {				
			if (nei2->node->in_cn_list == 1) {       /* 7 */
				if (((nei2->node->exterior == 1) && (last_pro == 2) &&  /* 8 */
				   (nei->node->degree > 2)) ||
				   (last_pro == 0)) {
				   					
					if ((last_pro == 2) &&
					   (nei2->node->exnext != nei2->clockw->node) &&
					   (nei2->node->exnext != nei->node)) {
						pro--;
						
					};
					
					pro++;
				};                             /* 8 */
				
				if (nei2->node->exterior == 1) {   /* 9 */
					last_pro = 2;
				}
				else {
					last_pro = 1;
				};       /* 9 */
			}
			else {
				last_pro = 0;
			};     /* 7 */		
						
			nei2 = nei2->co_clockw;
		} while (nei2 != nei->node->co_clockw);    /* do */
	}; /* 6 */
	
	return pro;
	
} /* placing_test */




int neighboring_edge_test (process, nei)
			/* Untersucht die Nachbarn von "process" daraufhin */
			/* ab, ob "process" als naechster Knoten plaziert  */
			/* werden kann.                                    */
struct node  *process;
struct nlist *nei;

{
	int c, cc;
	int sit = 0;
	c = cc = 0;

	
	if (nei->clockw->node->processed > 0) {
		c = 1;
	};
	
	if (nei->co_clockw->node->processed > 0) {
		cc = 1;
	};
	
	if ((process->exterior == 1) && (nei->node->exterior == 1)) {
		if ((nei->co_clockw->node->exterior == 1) && 
		   (nei->co_clockw->node->exnext == process) && 
		   (process->exnext == nei->node)) {
			cc = 0;
		}
		else {
			if ((nei->clockw->node->exterior == 1) && 
			   (nei->node->exnext == process) &&
			   (process->exnext == nei->clockw->node)) {
				c = 0;
			};
		};
			
	};
	
	sit = c + cc;
	
	return sit;
	
} /* neighboring_edge_test */
