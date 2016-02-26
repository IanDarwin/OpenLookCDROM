/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#ifndef HISTORY_HEADER
#define HISTORY_HEADER

#include <stdio.h>

/*  Package to manage a circular list of vectors. Especially
 when the list is viewed as a queue. */

/* temporary til #include <constants.h> */
#define TRUE 1
#define FALSE 0
#define NO_ERROR 0
#define MINOR_ERROR 1
/* 
  An empty list is signalled by first == EMPTY.
  A full list is signalled by avail == FULL.
  The last element in a list is signalled by 
       elt +1 % max_items == avail if the list is non_full
  or by 
       elt +1 % max_items == first if the list is full.
*/
#define EMPTY -1
#define FULL -2

typedef 
    struct history_struct_s {
	double **data;		/* array allocated by dmatrix 
				   of max_items rows, each with
				   data_dim components. */
	int
	    data_dim,		/* number of doubles per row */
	    max_items,		/* number of rows */
	    first,		/* row of first item */
	    avail;		/* row with first free space */
    } History_Struct;

/*
  When the list is nonempty, returns the successor to n 
  modulo history_struct->max_items.
*/
extern int
    succ();	/* ARGS: (history_struct_p, n) */


/*
  Return the number of elements in the history structure;
*/
extern int
    num_elements();		/* ARGS: (history_struct_p) */

/*
  Returns TRUE if the list is full.
 */
extern int 
    is_full();	/* ARGS: (history_struct_p) */

/*
  Returns TRUE if the list is empty.
 */
int 
    is_empty();			/* ARGS: (history_struct_p) */

/* 
  returns TRUE if n is the last element in the history structure 
*/
extern int 
    is_last();			/* ARGS: (history_struct_p,n) */

/*
  Allocate a new history structure.
  */
extern History_Struct
    * create_history_struct();	/* ARGS: (max_items,dim) */

/*
  Remove all entries without de-allocating.
*/ 
extern int
    empty_history_struct();	/* ARGS: (history_struct_p) */

/*
  Free up a previously allocated structure.
*/
extern int
    free_history_struct();	/* ARGS: (history_struct_p) */

/*
  Return a pointer to the n'th element
  (a vector of length history_struct_p->data_dim)
  in the circular list *history_struct_p.
  Does not check for em,pty list or
  n'th item being out of range.
*/
double *
    elt();			/* ARGS: (history_struct_p,n) */

/*
  Remove the first element of the circular list from the list
  and return a pointer to it.
*/
extern double
    * dequeue();		/* ARGS: (history_struct_p) */

/* 
  Enqueue the contents of source_vector into
  the (avail entry of the) history structure,
*/  
extern int
    enqueue();			/* ARGS: (history_struct_p, source_vector) */

/* print out the history struct */
extern int
    print_hist();		/* ARGS: (history_struct_p) */

#endif
