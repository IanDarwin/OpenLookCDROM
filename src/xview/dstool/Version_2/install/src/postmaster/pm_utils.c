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
/*
 * pm_utils.c
 *
 * Procedures:
 *   pm_type()
 *
 */
#include <stdio.h>

#include <pm_hash.h>

/*
 * returns the type of a postmaster element, and how
 * many (if a list type).
 *
 */
int
  pm_type(element, n, len)
char *element;
int *n, *len;
{
  struct pm_list *element_ptr, *pm_hash_lookup();

  element_ptr = pm_hash_lookup(element);
  if (element_ptr == NULL) return 0;
  
  if (n != NULL) *n = element_ptr->list_size;
  if (len != NULL) *len = element_ptr->string_max_len;
  return(element_ptr->type);
}
