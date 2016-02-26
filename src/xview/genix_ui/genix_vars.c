/*
 * Global variables for Genix.
 * Update genix.h after you add/change this file.
 */

#include "genix.h"

int Changed = 0;		/* If any "Apply" since last Save. */
char *CurrentFileName = 0;

Person *allp[MAXPEOPLE];
int np = 0;
Couple *allc[MAXCOUPLES];
int nc = 0;
