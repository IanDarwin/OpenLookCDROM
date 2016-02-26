/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                               D  E  F  .  H                                   */
/*                                                                               */
/*********************************************************************************/

#include "std.h"
#include "sgraph.h"

#define	SIZE	250	/* maximale Anzahl der Knoten pro Level */
#define	level(n)	((n)->y)

#define HD 70	/* horizontaler Abstand zwischen den Knoten */
#define VD 100 	/* vertikaler Abstand zwischen den Knoten */

extern int maxlevel;		 	/* groesstes auftretendes Level */
extern int nodes_of_level[SIZE];	/* Anzahl der Knoten pro Level  */

/* Diese beiden Variablen erhalten die Werte in "hier.c" */


typedef	struct {
	int	vertical_distance;
	int	horizontal_distance;
	int	it1, it2;	/* Iterations phase 1 / phase 2 */
	int	size_defaults_x, size_defaults_y;
}
	Sugiyama_settings;

extern	Sugiyama_settings sugiyama_settings;
extern	void	save_sugiyama_settings ();


/************* E n d e   D E F . H ***********************************************/



