/* (C) Universitaet Passau 1986-1991 */
/*************************************************************
 **                                                         **
 **     Hier stehen der Algorithmus von Chrobak und         **
 **     Payne und die abgeaenderten Versionen zur           **
 **     Flaechenreduzierung.                                **
 **                                                         **
 *************************************************************/


#include "decl.h"
#include <math.h>


void nejia_assila_idea (sum)
			/* Realisierung der Idee von Nejia Assila. */
int sum;

{
	
	if ((firstcont->y_coord != lastcont->y_coord) && 
	   ((firstcont->right == lastcont) || (firstcont->right->right == lastcont))) {
		
		if (((firstcont->right != lastcont) && (firstcont->y_coord < lastcont->y_coord) && 
		   (firstcont->right->y_coord <= firstcont->y_coord )) ||
		   ((firstcont->right == lastcont) && (firstcont->y_coord < lastcont->y_coord))) {
			place->x_offset = 0 - firstcont->y_coord + lastcont->y_coord;
			place->y_coord = lastcont->y_coord;
		    lastcont->x_offset = 2;
		    
		}
		else {
			if (((firstcont->right == lastcont) && (firstcont->y_coord > lastcont->y_coord)) ||
			   ((firstcont->right != lastcont) && (firstcont->y_coord > lastcont->y_coord) && 
			   (lastcont->y_coord >= firstcont->right->y_coord))) {
				place->x_offset = 2;
		 		place->y_coord = firstcont->y_coord;
				
			}
			else {
				place->x_offset = 
		  		     0.5 * (0 - firstcont->y_coord + sum + lastcont->y_coord);
				place->y_coord  = 
				     0.5 * (0 + firstcont->y_coord + sum + lastcont->y_coord);
			};
		}
	}
	else {
		place->x_offset = 
		     0.5 * (0 - firstcont->y_coord + sum + lastcont->y_coord);
		place->y_coord  = 
		     0.5 * (0 + firstcont->y_coord + sum + lastcont->y_coord);
	};
	
} /* nejia_assila_idea */



void accumulate_offsets (htree, offset)
			/* Berechnung der endgueltigen x-Werte der Knoten. */
struct node *htree;
int         offset;

{
	if (htree != NULL) {
		htree->x_offset = htree->x_offset + offset;
		
		if (htree->left != NULL) {
			accumulate_offsets(htree->left, htree->x_offset);
		};
		
		if (htree->right != NULL) {
			accumulate_offsets(htree->right, htree->x_offset);
		};
	};
	
} /* accumulate_offsets */



void stretching (kind)
			/* Dehnphase des Chrobak-Payne-Algorithmus. */
int kind;


{
	if ((firstcont != NULL) && (firstcont->right != NULL) && (lastcont != NULL)) {
		if ((kind == 1) || (kind == 3)) {
			firstcont->right->x_offset = firstcont->right->x_offset + 1;
			lastcont->x_offset         = lastcont->x_offset + 1;
		}
		else {
			if ((firstcont->right->x_offset == (firstcont->right->y_coord - firstcont->y_coord))
		       || (lastcont->x_offset == (prelast->y_coord - lastcont->y_coord)))  {
				firstcont->right->x_offset = firstcont->right->x_offset + 1;
				lastcont->x_offset         = lastcont->x_offset + 1;
			}
		}
	}
	else {
		message ("Canonical numbering is wrong!\n");
		return;
	};
	
} /* stretching */



void chrobak_payne_algorithm (kind) 
			/* Die vier Variationen des Chrobak-Payne-Algorithmus. */
int kind;

{
	struct node *htree;
	int k, sum;
	
	if (grid < 16) {
		grid = adjust_grid_cp ();
		find_grid_origin ();
	};
	
	
	htree = bintree;

	initialisation ();	
	canonical_numbering ();
	
	place = cn_list;
	
	while ((place != NULL) && (place != third->placenext)) {
		place = place->placenext;
	};
	
	
	for (k = 3; ((place != NULL) && (k <= gsize)); k++) {
				
		contour_neighbors (place);
		                                                   /* Dehnen. */
		stretching (kind);
		
		sum = lastcont->x_offset;            /* Berechnung der Offsets. */
		htree = firstcont->right;
		
		while ((htree != NULL) && (htree != lastcont)) {
			sum = sum + htree->x_offset;
			htree = htree->right;
		};

		if (kind <= 2) {
			place->x_offset = 
			     0.5 * (0 - firstcont->y_coord + sum + lastcont->y_coord);
			place->y_coord  = 
			     0.5 * (0 + firstcont->y_coord + sum + lastcont->y_coord);
		}
		else {
			nejia_assila_idea (sum);
			
		};
		
		lastcont->x_offset = sum - place->x_offset;
		
		if (firstcont != prelast) {
			firstcont->right->x_offset = 
			firstcont->right->x_offset - place->x_offset;
		};
		
		place->right = lastcont;            /* Installieren in den Baum. */
		
		if (firstcont != prelast) {
			place->left = firstcont->right;
			prelast->right = NULL;
		}
		else {
			place->left = NULL;
		};
		
		firstcont->right = place;
		place = place->placenext;
	};
	
	accumulate_offsets(bintree, 0);
	
} /* chrobak_payne_algorithm */
