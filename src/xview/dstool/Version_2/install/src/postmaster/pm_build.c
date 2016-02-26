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
#include <stdio.h>
#include <stdlib.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <constants.h>
#include <pm_hash.h>
#include <pm.h>

/* ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------   

   This collection of utilities is used to create the postmaster data objects.

   Procedures included in this file are:

	pm_add_object()
	pm_delete_object()
        pm_add_element()
	pm_delete_element()
	pm_valid_obj_name()
	pm_valid_elmt_name()
	pm_split_name()

   last change:  4/10/93  (mrm)

   		 8/4/93: added pm_name_components() and pm_name_separators()

   ----------------------------------------------------------------------------   
   ---------------------------------------------------------------------------- */




/* ----------------------------------------------------------------------------
   pm_add_object() is used to add an object to the postmaster data structure.  

   Arguments:
     
       name   (input)      (string) label of object to be added     

   Return value: 

       Returns an (int) status flag of NO_ERROR is successfully completed; 
       otherwise returns a flag indicating failure mode (intrep. by 
       pm_status() routine].


   last change:  4/11/93 (mrm)
   ----------------------------------------------------------------------------  */


int
pm_add_object( name )
char   *name;
{
  int                status = NO_ERROR;
  struct pm_list     *new_entry, *pm_hash_add(), *pm_hash_lookup();

  if( pm_valid_obj_name( name ) != NO_ERROR ) return( PM_SETUP_ERROR );

  if( pm_hash_lookup(name) == NULL )		/* Is this object name in use?                */
    {
     new_entry = pm_hash_add( name );           /*   No.  Create a new hash table entry under */
                                                /*   this label...                            */
     new_entry->category = PM_OBJECT;           /*   ...and initialize the category.          */
     new_entry->next_elem = NULL;               /*   Make SURE it doesn't point anywhere!     */
    }
  else
    return( PM_SETUP_ERROR );                   /* This name is in use.  Set error return.    */

  return( status );
}



/* ----------------------------------------------------------------------------
   pm_delete_object() is used to delete an object, and all it's dependent
   elements, from the postmaster data structure.

   Arguments:
     
       name   (input)      (string) label of object to be deleted   

   Return value: 

       Returns an (int) status flag of NO_ERROR is successfully completed; 
       otherwise returns a flag indicating failure mode (intrep. by 
       pm_status() routine].


   last change:  4/11/93 (mrm)
   ----------------------------------------------------------------------------  */

int
pm_delete_object( name )
char   *name;
{
  int              status = NO_ERROR;
  struct pm_list   *pm_hash_lookup(), *obj_ptr, *elemt_ptr;

  if( pm_valid_obj_name( name ) != NO_ERROR ) return( PM_SETUP_ERROR );

  obj_ptr = pm_hash_lookup(name);
  if( obj_ptr != NULL )		                      /* Is this object name in use?          */
    {
     elemt_ptr = obj_ptr->next_elem;  

     if( elemt_ptr != NULL )
       {
        while( elemt_ptr->next_elem != NULL )         /* get LAST element in obj/elem list    */
	    elemt_ptr = elemt_ptr->next_elem;

        while( elemt_ptr->prev_elem != NULL )         /* Destroy the linked element list by   */
	    {					      /* proceeding BACKwards.                */
             elemt_ptr = elemt_ptr->prev_elem; 
	     pm_rm_element( (elemt_ptr->next_elem)->label );
	    }
        pm_rm_element( elemt_ptr->label );            /* Kill 1st element in list.            */
       }

     pm_hash_rm( name );                              /* Remove object.                       */
    }
  else
    return( PM_SETUP_ERROR );                         /* This object name is not in use.      */

  return( status );
}


/* ----------------------------------------------------------------------------
   pm_add_element() is used to add an element to an already-existing object
   within the postmaster data structure.  If the element already exists, no
   action is taken, except returning an error status flag.  Elements are always
   at the HEAD of the object/element list, and the existing elements are PUSHED
   down the list.  

   The obj/element list is bidirectionally indexed by link-list pointers:

	     element_pointer->next_elem      next element in list
	     element_pointer->prev_elem      previous element in list

   The FIRST element in the list (closest to the object) is signalled by

	     element_pointer->prev_elem == NULL      .
   
   The LAST element in the list (furthest from the object) is signalled by

	     element_pointer->next_elem == NULL      .


   Arguments:
     
       name   (input)      (string) label of element to be added    
       type   (input)      (int) data type associated with new element

   Return value: 

       Returns an (int) status flag of NO_ERROR is successfully completed; 
       otherwise returns a flag indicating failure mode (intrep. by 
       pm_status() routine].


   last change:  4/11/93 (mrm)
   ----------------------------------------------------------------------------  */

int
pm_add_element( name, type )
char   *name;
int    type;
{
  int                status = NO_ERROR;
  char               *obj_name, *elmt_name;
  struct pm_list     *pushed_elemt, *new_elemt, *obj_ptr, *pm_hash_add(), *pm_hash_lookup();

  if( pm_valid_elmt_name( name ) != NO_ERROR ) return( PM_SETUP_ERROR );        /* invalid elmt name!        */
  if( pm_hash_lookup(name) != NULL ) return( PM_SETUP_ERROR );			/* elmt name already in use! */

  obj_name = (char *) calloc( strlen(name)+1 , sizeof( char ) );
  elmt_name = (char *) calloc( strlen(name)+1 , sizeof( char ) );

  pm_split_name( name, obj_name, elmt_name );

  obj_ptr = pm_hash_lookup(obj_name);
  if( obj_ptr != NULL )		               /* Is this object name in use?                */
    {
     new_elemt = pm_hash_add( name ); 

     if(  obj_ptr->next_elem == NULL )         /* Is this the first element added?           */
       {
        new_elemt->next_elem = NULL;           /*    Yes. Make SURE this element doesn't     */
        new_elemt->prev_elem = NULL;           /*    point anywhere.                         */
       }
     else
       {
	pushed_elemt = obj_ptr->next_elem;     /*    No.  Push the 1st elemnt down and       */
	pushed_elemt->prev_elem = new_elemt;   /*    insert the new element at the head of   */
	new_elemt->next_elem = pushed_elemt;   /*    the list.  Reset the prev/next ptrs     */
	new_elemt->prev_elem = NULL;
       }
     obj_ptr->next_elem = new_elemt;           /*    Make object point at the new element.   */
                                                
     new_elemt->category = PM_ELEMENT;           
     new_elemt->type = type;
     new_elemt->list_size = 0;
     new_elemt->string_max_len = 0;
     switch(type)
       {
       case INT:
	 new_elemt->data.int_data = 0;
	 break;
       case INT_LIST:
	 new_elemt->data.int_list_data = (int *) NULL;
	 break;
       case DBL:
	 new_elemt->data.double_data = 0.0;
	 break;
       case DBL_LIST:
	 new_elemt->data.double_list_data = (double *) NULL;
	 break;
       case STRNG:
	 new_elemt->data.string_data = (char *) NULL;
	 break;
       case STRNG_LIST:
	 new_elemt->data.string_list_data = (char **) NULL;
	 break;
       case ADDRS:
       case MEMRY:
	 new_elemt->data.addr_data = (void *) NULL;
	 break;
       case FNCT:
	 new_elemt->data.fnct_ptr = ( int (*)()) NULL;
	 break;
       }
   }
  else
    return( PM_SETUP_ERROR );                  /* This name is in use.  Set error return.    */

  free( obj_name );			       /* dealloc temp space for obj/elemt strings   */
  free( elmt_name );

  return( status );
}


/* ----------------------------------------------------------------------------
   pm_rm_element() is used to delete an element from an object within the 
   postmaster data structure.  Remaining elements are POPPED up the list.

   Arguments:
     
       name   (input)      (string) label of element to be deleted   

   Return value: 

       Returns an (int) status flag of NO_ERROR is successfully completed; 
       otherwise returns a flag indicating failure mode (intrep. by 
       pm_status() routine].


   last change:  4/11/93 (mrm)
   ----------------------------------------------------------------------------  */


int
pm_rm_element( name )
char   *name;
{
  int                status = NO_ERROR;
  char               *obj_name, *elmt_name;
  struct pm_list     *element_ptr, *obj_ptr, *popped_element, *last_element, *pm_hash_lookup();

  if( pm_valid_elmt_name( name ) != NO_ERROR ) return( PM_SETUP_ERROR );        /* invalid elmt name!    */

  element_ptr = pm_hash_lookup(name); /* 8/20 */
  if( element_ptr == NULL ) return( PM_SETUP_ERROR );			        /* elmt name not in use! */

  popped_element = element_ptr->next_elem;
  last_element = element_ptr->prev_elem;

  if( last_element == NULL )	               /* Is this the first element in obj/elem list? */
    {
     obj_name = (char *) calloc( strlen(name) , sizeof( char ) );     /* Split name into obj/element parts.. */
     elmt_name = (char *) calloc( strlen(name) , sizeof( char ) );
     pm_split_name( name, obj_name, elmt_name );
     obj_ptr = pm_hash_lookup(obj_name);                              /* ...and fetch ptr to obj.            */

     obj_ptr->next_elem = popped_element;                             /* Set new ptr values for next element,*/
     if( popped_element != NULL)
         popped_element->prev_elem = NULL;                            /* and flag as first element.          */
     
     free( obj_name );			       /* dealloc temp space for obj/elemt strings   */
     free( elmt_name );
    }
  else								      /* Not first element in obj/elem list. */
    {
     last_element->next_elem = popped_element;                        /* Set prev and next element ptrs.     */
     if( popped_element != NULL)
	      popped_element->prev_elem = last_element;          
    }

  status = pm_dealloc_elem( name );
  if( status == NO_ERROR )
     status = pm_hash_rm( name );

  return( status );
}




/* ----------------------------------------------------------------------------
   pm_dealloc_elem() releases any space allocated for the data segment of the 
   element structure.  Notice that this action DEPENDS UPON THE DATA TYPE.

      Arguments:

	name   (input)      (string) label of object to be added

      Return value:

	Returns an (int) status flag of NO_ERROR is successfully completed;
	otherwise returns a flag indicating failure mode (intrep. by pm_status()
	routine].

   last change:  4/11/93 (mrm)
   ---------------------------------------------------------------------------- */

pm_dealloc_elem( name ) 
char   *name;
{
  int             status = NO_ERROR;
  struct pm_list  *element, *pm_hash_lookup();

  element = pm_hash_lookup( name );         /* fetch ptr to element */

  switch( element->type )
    {
     case INT_LIST:
       free_ivector( element->data.int_list_data, 0, element->list_size);
       break;
     case DBL_LIST:
       free_dvector( element->data.double_list_data, 0, element->list_size);
       break;
     case STRNG:
     case STRNG_LIST:
       free( element->data.string_list_data );
       break;
    }

  return( status );
}


/* ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------   
   Operators on the object/element postmaster structure.   These include
   database query functions.

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------  */ 

/* 
 *  This proc executes a function stored at this postmaster entry
 *
 *  last change:  4/14/93  (mrm)
*/
  

int
pm_fnct_exec( name )
char   *name;
{
  int                status = NO_ERROR;
  struct pm_list     *element_ptr, *pm_hash_lookup();

  if( pm_valid_elmt_name( name ) != NO_ERROR ) return( PM_SETUP_ERROR );        /* invalid elmt name!    */

  element_ptr = pm_hash_lookup(name);
  if( element_ptr == NULL ) return( PM_SETUP_ERROR );			        /* elmt name not in use! */

  if( element_ptr->category != PM_ELEMENT ||                                    /* exec operation not allowed */
      element_ptr->type     != FNCT          ) return( PM_SETUP_ERROR );        /* on this entry!             */

  if( element_ptr->data.fnct_ptr == NULL ) return( PM_SETUP_ERROR );            /* null jump address!    */

  (element_ptr->data.fnct_ptr)();                                               /* jump to address!      */

  return( status );
}


/* ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------   
   Utilities for handling the (string-based) object/element labels.

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------  */ 

/* 
   Boolean proc to check that an object label respects the naming conventions.
*/

int
pm_valid_obj_name( name )
char    *name;
{
  int   status = NO_ERROR;

  return( status );
}


/* 
   Boolean proc to check that an element label respects the naming conventions.
*/

int
pm_valid_elmt_name( name )
char    *name;
{
  int   status = NO_ERROR;

  return( status );
}


/* 
  this little proc takes a string of the form <object name>.<element name>
  and splits it into two strings.  It accomplishes this using the string
  token utilities (in anticipation of longer ownership chains).

  last change:  4/13/93 (mrm)
*/

int
pm_split_name( name, obj_name, elemt_name )
char     *name, *obj_name, *elemt_name;
{
   int  status = NO_ERROR;
   char *token, *strtok();

   strcpy( obj_name, name );           
   token = strtok( obj_name, ".");     /* 1st call divides into tokens separated by '\0' */
/*   token = strtok( '\0',"."); */
   token = strtok( NULL,".");     	/* subsequent calls return tokens (in order)      */
   strcpy( elemt_name, token );

   return( status );
}

/*
  Takes name ( a string ), copies it to name_copy,
  counts the number of components within name, sets
  this value to *num_cmpt_p,  overwrites  a '\0' in name_copy
  after each component and returns as cmpt_endpt[i] (0 <=i <= *num_cmpt_p)
  pointers in name_copy to the first character of these components.

  Separators of components are the characters '\0' or '.'. 

  Thus component i of name may be represented as an ordinary C string
  beginning with the character pointed to by cmpt_endpt[i].
  where 0 <= i < *num_cmpt_p.

  When successful, this routine returns *num_cmpt_p; when unsuccessful it
  returns the negative value MAJOR_ERROR.

  It is assumed that memory for cmpt_endpt is at least *num_cmpt_p +1 long,
  where here the value of num_cmpt_p at entry is used. Should the decompositition
  of the string name not be exhausted after *num_cmpt_p components, remaining
  components are ignored.

  This routine does not alter name. It assumes name_copy is pre-allocated.

 */
int
pm_name_components(name,name_copy,cmpt_endpt,num_cmpt_p)

char 	*name,*name_copy;
char 	**cmpt_endpt;
int    	*num_cmpt_p;

{
   int  status = NO_ERROR;
   char *token;
   int num = 0;

    if (*num_cmpt_p <= 0)
	return MAJOR_ERROR;

   strcpy( name_copy, name );

   if (name_copy[0] == '\0') {
       *num_cmpt_p = 0;
       cmpt_endpt[0] = name_copy;
       return *num_cmpt_p;
   }

   token = strtok(name_copy, ".");
   if (token != NULL) {
       cmpt_endpt[num++] = token;
   }

   while ((token != (char *) NULL) && 
	  ((token = strtok(NULL, ".")) != (char *) NULL) && 
	  (num < *num_cmpt_p))
	  cmpt_endpt[num++] = token;

   *num_cmpt_p = num;
   
   
   return( *num_cmpt_p );
}

/*
  Takes name ( a string ), counts the number of components within name, sets
  this value to *num_cmpt_p,  and returns as cmpt_endpt[i] (0 <=i <= *num_cmpt_p)
  pointers to these separators.

  Separators are the characters '\0' or '.'. 

  The value of cmpt_endpt[0] is set to name-1. Thus component i of name begins with
  the character pointed to by cmpt_endpt[i]+1 and ends just before that pointed to by
  cmpt_endpt[i+1] where 0 <= i < *num_cmpt_p.

  When successful, this routine returns *num_cmpt_p; when unsuccessful it
  returns the negative value MAJOR_ERROR.

  It is assumed that memory for cmpt_endpt is at least *num_cmpt_p +1 long,
  where here the value of num_cmpt_p at entry is used. Should the decompositition
  of the string name not be exhausted after *num_cmpt_p components, remaining
  components are ignored.

  This routine does not copy or alter name.
 */
int
pm_name_separators(name,cmpt_endpt,num_cmpt_p)

char 	*name;
char 	**cmpt_endpt;
int    	*num_cmpt_p;
{
    int num = 0;

    if (*num_cmpt_p <= 0)
	return -1;
    else 
	cmpt_endpt[num] = name -1;
    
    while ((num < *num_cmpt_p) && (*(++name) != '\0'))
	if (*name == '.')
	    cmpt_endpt[++num] = name;

    if (*name == '\0')
	cmpt_endpt[++num] = name;

    *num_cmpt_p = num;

    return num;
    
    
}
