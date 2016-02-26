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
#include <stdlib.h>
/*  Package to manage a circular list of vectors. */

#include "history.h"

/*
  When the list is nonempty, returns the successor to n 
  modulo history_struct->max_items. 
*/
int
    succ(history_struct_p,n)

History_Struct
    * history_struct_p;
int 
    n;
{
    if (n < history_struct_p->max_items - 1) 
	return  n+1;
    else
	return 0;
}

/*
  Return the number of elements in the history structure.
*/
int
    num_elements(history_struct_p)

History_Struct
    * history_struct_p;

{
    if (history_struct_p->first == EMPTY)
	return 0;
    if (history_struct_p->avail == FULL)
	return history_struct_p->max_items;
    else if (history_struct_p->first < history_struct_p->avail)
	return history_struct_p->avail - history_struct_p->first;
    else
	return (history_struct_p->max_items - history_struct_p->first) + 
	    history_struct_p->avail;
	
}
/*
  Returns TRUE if the list is full.
 */
int 
    is_full(history_struct_p)

History_Struct
   * history_struct_p;

{
    if (history_struct_p->avail == FULL)
	return TRUE;
    else
	return FALSE;
}

/*
  Returns TRUE if the list is empty.
 */
int 
    is_empty(history_struct_p)

History_Struct
    * history_struct_p;

{
    if (history_struct_p->first == EMPTY)
	return TRUE;
    else
	return FALSE;
}

/* 
  returns TRUE if n is the last element in the history structure 
*/
int 
    is_last(history_struct_p,n)
History_Struct
    * history_struct_p;
int
    n;
{
   if (history_struct_p->avail == FULL)
       return (((n + 1) % history_struct_p->max_items) == 
	       history_struct_p->first);
   else
       return (((n + 1) % history_struct_p->max_items) == 
	       history_struct_p->avail);
}

/*
  Allocate a new history structure.
  */
History_Struct
    * create_history_struct(max_items,dim)

int
    max_items,
    dim;

{

    extern double 
	**dmatrix();

    History_Struct
	* new_struct = (History_Struct *) malloc(sizeof(History_Struct));

    if (new_struct != (History_Struct *) NULL) {
	new_struct->data = dmatrix(0, max_items , 0, dim -1) ;
	new_struct->max_items = max_items;
	new_struct->data_dim = dim;
	new_struct->first = EMPTY;
	new_struct->avail = 0;
    }
    
    return new_struct;
    
}

/*
  Remove all entries without de-allocating.
*/ 
int
    empty_history_struct(history_struct_p)

History_Struct
    * history_struct_p;
{
    history_struct_p->first = EMPTY;
    history_struct_p->avail = 0;  
}

/*
  Free up a previously allocated structure.
*/
int
    free_history_struct(history_struct_p)

History_Struct
    * history_struct_p;
{

    if (history_struct_p != (History_Struct *) NULL) {
	free_dmatrix(history_struct_p->data,0,history_struct_p->max_items - 1,
		     0,history_struct_p->data_dim - 1);
	free(history_struct_p);
    }
    return NO_ERROR;
    
}

/*
  Return a pointer to the n'th element
  (a vector of length history_struct_p->data_dim)
  in the circular list *history_struct_p.
  The list starts at 0.
  Does not check for empty list or
  n'th item being out of range.
*/
double *
    elt(history_struct_p,n)

History_Struct
    * history_struct_p;
int 
    n;

{
    return 
	history_struct_p->data[(history_struct_p->first + n) %
			       history_struct_p->max_items];
}

/*
  Remove the first element of the circular list from the list
  and return a pointer to it.
  */
double
    * dequeue(history_struct_p)

History_Struct
    * history_struct_p;
{
    int
	first_elt = history_struct_p->first;
    
    if (first_elt == EMPTY) {
	return (double *) NULL;
    }
    else if (succ(history_struct_p,first_elt) == history_struct_p->avail) {
	/* one element queue */
	history_struct_p->first = EMPTY;
	history_struct_p->avail = 0;
    }
    else 
	/* general case */
	history_struct_p->first = succ(history_struct_p,first_elt);

    /* if queue was full, mark new available space */
    if (history_struct_p->avail == FULL)
	history_struct_p->avail = first_elt;

    return history_struct_p->data[first_elt];
}

/* 
  Enqueue the contents of source_vector into
  the (avail entry of the) history structure,
*/  
int
    enqueue(history_struct_p, source_vector)

History_Struct
    * history_struct_p;
double *
    source_vector;

{
    int 
	i;

    if (is_empty(history_struct_p)) {
	history_struct_p->first = 0;
	history_struct_p->avail = 0; /* will copy to here shortly */
    }


    /* dcopy(history_struct_p->data_dim,source_vector,1,
       history_struct_p->data[avail], 1)*/
    if (history_struct_p->avail == FULL)
	return MINOR_ERROR;
    else
	for (i = 0; i < history_struct_p->data_dim; i++)
	    history_struct_p->data[history_struct_p->avail][i] = source_vector[i];
    
    history_struct_p->avail = succ(history_struct_p, history_struct_p->avail);

    if (history_struct_p->avail == history_struct_p->first)
	history_struct_p->avail = FULL;
    
    return NO_ERROR;

}

/* print out the history struct */
int
    print_hist(history_struct_p)	       

History_Struct 
    *history_struct_p;

{
    int 
	after_last,
	i,
	j;

    fprintf(stderr,"Max_items: %d   Data_Dim: %d   First: %d   Avail: %d\n",
	    history_struct_p->max_items,
	    history_struct_p->data_dim,
	    history_struct_p->first,
	    history_struct_p->avail);
    if (! is_empty(history_struct_p)) {
	if (is_full(history_struct_p))
	    after_last = history_struct_p->first;
	else
	    after_last = history_struct_p->avail;

	/* print first element - 
	   special treatment needed for
	   full list case since stopping condition in traversal
	   below will be i==first then */
	for (j=0 ; j < history_struct_p->data_dim; j++) 
	    fprintf(stderr,"%g ",history_struct_p->data[history_struct_p->first][j]);
	fprintf(stderr,"\n");

	/* print remainder */
	for(i = succ(history_struct_p,history_struct_p->first); 
	    i != after_last;
	    i = succ(history_struct_p,i)) {
	    for (j=0 ; j < history_struct_p->data_dim; j++) 
		fprintf(stderr,"%g ",history_struct_p->data[i][j]);
	    fprintf(stderr,"\n");
	}
    }
}
