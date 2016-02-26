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
#include <constants.h>
#include <pm_hash.h>
#include <pm.h>

/* static struct pm_list *hashtab[HASHSIZE];*/

struct pm_list *hashtab[HASHSIZE];
/* -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   
   This library contains the hash table utilities used by the postmaster to 
   construct data object/element components.  The basic design is outlined
   in:

     Kernighan, B.W. and D.M. Richie, "The C Programming Language",
     Perntice Hall Software Series, Second Edition, pps. 143-146.

   Functions Included:

	  pm_hash_lookup()
	  pm_hash_add()
	  pm_hash_rm()
	  pm_get_hash()

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- 
   ----------------------------------------------------------------------------- */





/* -----------------------------------------------------------------------------
   pm_hash_lookup() :  returns the address of the structure in the hash-table
		       linked-list associated with the string label.  If no
		       match is found, NULL is returned.

   Arguments:
    
	label (input)  :    ptr to string label used to index hash table 

   Function value: 

	ptr to structure of type >> pm_list << if successful;  NULL otherwise

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */

/* create empty postmaster */
pm_init()
{
int	i;

  for (i=0; i < HASHSIZE; i++)
      hashtab[i] = (struct pm_list *) NULL;

  return NO_ERROR;
}

struct pm_list
*pm_hash_lookup( label )
char    *label;
{
  struct pm_list     *current;
  unsigned           pm_get_hash();

  for(current=hashtab[pm_get_hash(label)]; current != NULL; current = current->next_in_hash)
    if( strcmp(label, current->label) == 0 )
       return( current);                        /* found! */
  
  return( (struct pm_list *) NULL );				/* search failed. */
}


/* -----------------------------------------------------------------------------
   pm_hash_add() :    add a new entry into the hash table, under the label
		      supplied.  

   Arguments:

       label (input)  :  label used to identify entry in hash table

   Function value:

       Address of new entry in hash table if successful;  NULL otherwise

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */


struct pm_list  *pm_hash_lookup();
char *strdup();

struct pm_list 
*pm_hash_add( label )
char    *label;
{
  struct pm_list      *current=NULL;
  unsigned            pm_get_hash(), hashval;

  if( (current = pm_hash_lookup(label)) == NULL)
    {
     current = (struct pm_list *) malloc(sizeof(*current));		/* alloc space for new table entry */
     if( current == NULL || (current->label = strdup(label)) == NULL)   /* If alloc failed, rtn NULL       */
	return( NULL );
     hashval = pm_get_hash(label);

     current->next_in_hash = hashtab[hashval];
     current->prev_in_hash = NULL;
     if( hashtab[hashval] != NULL )
        hashtab[hashval]->prev_in_hash = current;

     hashtab[hashval] = current;

     current->label = (char *) calloc( strlen(label)+1 , sizeof(char) );/* alloc space for entry label... */
     strcpy( current->label, label );			                /* and COPY into struct.          */
     current->savable = SAVE_SETTINGS;

     return( current );				                        /* rtn address of new entry */
    }

  return( NULL );				                        /* !! entry already present !! */
}




/* -----------------------------------------------------------------------------
   pm_hash_rm()  :    remove an entry from the hash table

   Arguments:

       label (input)  :  label used to identify entry in hash table

   Function value:

       Returns integer NO_ERROR if successful; non-zero otherwise.

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */

int
pm_hash_rm( label )
char    *label;
{
  struct pm_list *current, *prev, *next;
  int            h, status=NO_ERROR;
  unsigned       pm_get_hash();

  h = pm_get_hash( label );		/* hash value of label */

  for( current = hashtab[h]; current != NULL; current = current->next_in_hash)   /* walk down linked-list.. */ 
     {
      if( strcmp(label, current->label) == 0)				     /* ..looking for label.    */
	 break;
      prev = current;
     }
  
  if( current != NULL)				/* If label was found in list..    */
     {
      if( current->prev_in_hash == NULL )	/* ..if first entry...             */
	{
	 hashtab[h] = current->next_in_hash;	/* ..reset last entry's next ptr.  */
	 if( hashtab[h] != NULL )
	     hashtab[h]->prev_in_hash = NULL;
        }
      else
	{
	 prev = current->prev_in_hash;
	 next = current->next_in_hash;
	 prev->next_in_hash = next;
	 next->prev_in_hash = prev;
        }

      free(current->label);			/* free allocated space            */
      free(current);
     }
  else
     status = -1;				/* entry not found!                */

  return( status );
}



/* -----------------------------------------------------------------------------
   pm_get_hash()  :    fetch hash based on label string

   Arguments:

       label (input)  :  label used to identify entry in hash table

   Function value:

       unsigned int value of hash corresponding to label

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */

unsigned
pm_get_hash( label )
char *label;
{
  unsigned     hashval;

  for( hashval = 0; *label != '\0'; label++ )
     hashval = *label + 31*hashval;

  return( hashval % HASHSIZE );
}


/* -----------------------------------------------------------------------------
   pm_hash_dump()  :    dump the  current hash table

   Arguments: none

   Function value:
     
     returns non-zero status integer if error encountered.

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */

void
pm_hash_dump()
{
  struct pm_list   *current;
  int              i, status = NO_ERROR;

  for( i=0; i<HASHSIZE; i++)                   /* loop through all the hashtable elements */
    {
     current = hashtab[i];		       /*   get addr stored in this element       */
     if( current != NULL ) 
       {
	 fprintf(stderr,"%3d",i);            /* linked-list attached to this element!   */
	 while( current != NULL)
	   {                                   /* dump data attached to this node         */
	    fprintf(stderr,"  <%X>",current);
	    fprintf(stderr,"[%s,(%X,%X),(%X,%X)] ->",
	       current->label,current->prev_in_hash,current->next_in_hash,
	       current->prev_elem,current->next_elem);
	    current = current->next_in_hash;
           }
         fprintf(stderr,"NULL\n");
	}
    }

}



/* -----------------------------------------------------------------------------
   pm_dump()  :    dump the current postmaster data structure (object/element
		   format)

   Arguments: none

   Function value:
     
     returns non-zero status integer if error encountered.

   last change:  4/10/93  (mrm)
   ----------------------------------------------------------------------------- */

void
pm_dump()
{
  struct pm_list   *current, *ptr;
  int              i, status = NO_ERROR;

  for( i=0; i<HASHSIZE; i++)                   /* loop through all the hashtable elements */
    {
     current = hashtab[i];		       /*   get addr stored in this element       */
     if( current != NULL ) 
        {
	 while( current != NULL)
	   {                                   /* dump data attached to this node         */
	    if( current->category == PM_OBJECT )
	      {
	       fprintf(stderr,"  <%X>",current);
	       fprintf(stderr,"[%s,%X] \n",current->label,current->next_elem);
               ptr = current->next_elem;
	       while( ptr != NULL)
		 {
                  fprintf(stderr,"         <%X>",ptr);
		  fprintf(stderr,"[%s,%X,%X] \n",ptr->label,ptr->prev_elem,ptr->next_elem);
		  ptr = ptr->next_elem;
		 }
               fprintf(stderr,"\n");
              }
	    current = current->next_in_hash;
           }
	}
    }

}
