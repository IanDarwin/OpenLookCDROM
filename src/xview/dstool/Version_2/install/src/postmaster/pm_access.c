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
#include <stdio.h>
#include <varargs.h>

#include <memory.h>
#include <constants.h> 
#include <pm.h>
#include <pm_hash.h>


/* -------------------------------------------------------------------------------------------------

   pm_access() provides the main entry point in manipulating the object/element data stored in
   in postmaster.   Its primary purpose is to direct action on the data structure, based on the
   operation requested by a request through pm().

   Arguments:

	operation  (input)  [int] code representing the required operation.  May take one of
			    the values:

			       CREATE_OBJ           RM_OBJ          CREATE_ELEM
			       RM_ELEM              GET             GET_LIST
			       PUT                  PUT_LIST        INIT
			       CLEAR                EXEC

                            Unchanged on exit.

        label      (input)  [string] name of the object or object/element to be acted upon.
			    Unchanged on exit.

        pargptr    (input)  [va_list ptr] Pointer to the C-structure which defines the 
			    variable-length argument list.  

        pdata_type (output) Type of data returned, if appropriate.

    Function return value:

        Integer status flag indicating normal termination (NO_ERROR), or type of failure.


    last change:  4/11/93 (mrm) 

   ------------------------------------------------------------------------------------------------- */


int
pm_access(operation, label, pargptr, pdata_type)
int     operation, *pdata_type;
char    *label;
va_list *pargptr;
{
  int   status = NO_ERROR, data_type,int_value;

  struct pm_list     *element_ptr, *pm_hash_lookup();

  switch( operation )
     {
      case CREATE_OBJ:				/* Creation and removal of objects      */
         status = pm_add_object( label );       /* and elements are handled separately  */
	 break;
      case RM_OBJ:
         status = pm_delete_object( label );
	 break;
      case CREATE_ELEM:
	 data_type = (int) va_arg(*pargptr,int);
         status = pm_add_element( label, data_type );
	 break;
      case RM_ELEM:
         pm_rm_element( label );
	 break;
     case EXEC:
	 pm_fnct_exec( label );
	 break;
     case GET_SAVABLE:
     case PUT_SAVABLE:
         if( pm_valid_elmt_name( label ) != NO_ERROR )   /* Is this a valid element name?     */
	     return( PM_SETUP_ERROR );                   /*   Nope.  return err status.       */

         element_ptr = pm_hash_lookup( label );          /* Does this element exist?          */
	 if( element_ptr == NULL )
	   {
	     fprintf(stderr, "dstool: pm unrecognized element - \"%s\"\n", label);
	     return( PM_SETUP_ERROR );                   /*   Nope.  return err status.       */
	   }

	 pm_savable_rw(operation,&(element_ptr->savable),pargptr);
	 break;

      default:                                  /* All remaining operations occur on          */
						/* DATA stored within the elements            */

         if( pm_valid_elmt_name( label ) != NO_ERROR )   /* Is this a valid element name?     */
	     return( PM_SETUP_ERROR );                   /*   Nope.  return err status.       */

         element_ptr = pm_hash_lookup( label );          /* Does this element exist?          */
	 if( element_ptr == NULL )
	   {
	     fprintf(stderr, "dstool: pm unrecognized element - \"%s\"\n", label);
	     return( PM_SETUP_ERROR );                   /*   Nope.  return err status.       */
	   }

	 switch( element_ptr->type )
	   {
	    case INT:
	      status = i_rw( operation, &(element_ptr->data.int_data), pargptr, pdata_type );
	      break;
            case INT_LIST:
	      status = i_list_rw( operation, &(element_ptr->data.int_list_data),
				  &(element_ptr->list_size), pargptr, pdata_type);
              break;
	    case DBL: 
	      status = d_rw( operation, &(element_ptr->data.double_data), pargptr, pdata_type );
	      break;
            case DBL_LIST:
	      status = d_list_rw( operation, &(element_ptr->data.double_list_data),
				  &(element_ptr->list_size), pargptr, pdata_type);
              break;
            case ADDRS:
	      status = addr_rw( operation, &(element_ptr->data.addr_data), pargptr, pdata_type );
              break;
            case MEMRY:
	      status = mem_ptr_rw( operation, &(element_ptr->data.addr_data),  pargptr, pdata_type);
              break;
            case STRNG:
	      status = c_rw( operation, &(element_ptr->data.string_data),
			     &(element_ptr->string_max_len), pargptr, pdata_type );
              break;
            case STRNG_LIST:
	      status = c_list_rw( operation, &(element_ptr->data.string_list_data),
				  &(element_ptr->list_size), &(element_ptr->string_max_len),
				 pargptr, pdata_type);
	      break;
            case FNCT:
	      status = addr_rw( operation, &(element_ptr->data.fnct_ptr), pargptr, pdata_type );
              break;
	   }

	 break;
     }

  pm_set_error(status,"pm_access() ");
  if (status < 0) fprintf(stderr, " Name: %s  Op: %d\n", label, operation);
  return( status );
}


/* ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------

   The following procedures perform the manipulation of each data type. 

      Included:
	
	  i_list_rw()       
	  d_list_rw()
	  i_rw()
	  d_rw()
	  c_list_rw()
	  mem_ptr_rw()
	  addr_rw()
	  c_rw()
	  imax_nonneg()

   last change: 4/11/93 (mrm)

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------ */

/*extern int             i_varb;  */               /* temp storage if returning an integer or integer list */
extern int             *i_varb_ptr;                 /* temp storage if returning an integer or integer list */
/* extern double          d_varb; */                /* temp storage if returning double or double list */
extern double          *d_varb_ptr;                 /* temp storage if returning double or double list */
extern char            *c_varb_ptr;            /* temp storage if returning a character string */
extern void            *addr_ptr;              /* temp storage if returning an address */




/* ------------------------------------------------------------
   routine to handle storage control for a integer list item.

   last change:	Oct 1, 1992
   ------------------------------------------------------------ */

int
i_list_rw( operation, varb_ptr, list_dim, argptr, pdata_type )
int	operation, *list_dim, *pdata_type;
int	**varb_ptr;
va_list *argptr;
{
  int           index, start, stop, i, error_code ;
  int		*ivector(), *put_addr, *get_addr, free_ivector();

  error_code = 0;

  if( operation == INIT)

    {if(*list_dim != 0)
       {/*if(*list_dim == 0) return(0);*/
	free_ivector(*varb_ptr,0,*list_dim);
	error_code = VARB_INITIALIZED_ERROR;				/* varb already initialized! */
	/* *list_dim = 0; */}
     *list_dim = va_arg(*argptr,int);
     *varb_ptr = ivector(0,*list_dim);
     if(*varb_ptr == NULL) 
	 error_code= MEMORY_ALLOC_ERROR; 	/* memory allocation error overrides */
     				/* INIT error */
     else
	 for (i=0; i<*list_dim; i++)
	     *(*varb_ptr+i) = 0;}
  else if ( operation == PUT)

    {if(*list_dim != 0)
       {index = va_arg(*argptr,int);
	*(*varb_ptr+index) = va_arg(*argptr,int);} 
    else
      error_code = LIST_NOT_ALLOC_ERROR;}				/* list storage not already allocated ! */

  else if ( operation == PUT_LIST )

    {start = va_arg(*argptr,int);
     stop = va_arg(*argptr,int);
     get_addr = va_arg(*argptr,int *);		
     if(start>=0 && stop<*list_dim) 
       {if(get_addr != NULL)
	  {for(i=0; i<=stop-start; i++)
	     *(*varb_ptr+i+start) = get_addr[i];} 
       else
	  error_code = NULL_ADDRESS_ERROR;}	/* NULL address provided to get data from */
     else
       error_code= INDEX_BOUNDS_ERROR;}		/* start or stop index outside bounds */

  else if ( operation == GET)

    {index = va_arg(*argptr,int);
     if(index>=0 && index<*list_dim)
       {/* i_varb = *(*varb_ptr+index);*/
	 i_varb_ptr = *varb_ptr+index;
	*pdata_type = INT;}
     else
       error_code = INDEX_BOUNDS_ERROR;}	/* start or stop index outside bounds */

  else if ( operation == GET_LIST )
    {start = va_arg(*argptr,int);
     stop = va_arg(*argptr,int);
     put_addr = va_arg(*argptr,int *);
     if(start>=0 && stop<*list_dim) 
       {if(put_addr != NULL)
	  {for(i=0; i<=stop-start; i++)
	     put_addr[i] = *(*varb_ptr+i+start);
	   *pdata_type = INT;}              
       else
	 error_code = NULL_ADDRESS_ERROR;}	/* NULL address provided to put data into */
     else
       error_code= INDEX_BOUNDS_ERROR;}		/* start or stop index outside bounds */

  else if ( operation == CLEAR)

    {if(*list_dim == 0) return(0);
       free_ivector(*varb_ptr,0,*list_dim);
     *list_dim = 0;}


  return(error_code);
}





/* ------------------------------------------------------------
   routine to handle storage control for a double list item.

   last change:	Oct 1, 1992
   ------------------------------------------------------------ */

int
d_list_rw( operation, varb_ptr, list_dim, argptr, pdata_type )
int	operation, *list_dim, *pdata_type;
double	**varb_ptr;
va_list *argptr;
{
  int             index, start, stop, i, error_code ;
  double		*dvector(), *put_addr, *get_addr;
  int            free_dvector();

  error_code = 0;

  if( operation == INIT)

    {if(*list_dim != 0)
       {/* if(*list_dim == 0) return(0); */
	free_dvector(*varb_ptr,0,*list_dim);
	error_code = VARB_INITIALIZED_ERROR;				/* varb already initialized! */
	/* *list_dim = 0; */}
     *list_dim = va_arg(*argptr,int);
     *varb_ptr = dvector(0,*list_dim);
     if(*varb_ptr == NULL) 
	 error_code= MEMORY_ALLOC_ERROR;	/* memory allocation error overrides */
  				/* INIT error */
     else
	 for(i=0;i<*list_dim;i++)
	     *(*varb_ptr+i) = 0.0;}
  else if ( operation == PUT)

    {if(*list_dim != 0)
       {index = va_arg(*argptr,int);
	*(*varb_ptr+index) = va_arg(*argptr,double);} 
    else
      error_code = LIST_NOT_ALLOC_ERROR;}		/* list storage not already allocated ! */

  else if ( operation == PUT_LIST )

    {start = va_arg(*argptr,int);
     stop = va_arg(*argptr,int);
     get_addr = va_arg(*argptr,double *);		
     if(start>=0 && stop<*list_dim) 
       {if(get_addr != NULL)
	  {for(i=0; i<=stop-start; i++)
	     *(*varb_ptr+i+start) = get_addr[i];} 
       else
	 error_code = NULL_ADDRESS_ERROR;}			/* NULL address provided to get data from */
     else
       error_code= INDEX_BOUNDS_ERROR;}				/* start or stop index outside bounds */

  else if ( operation == GET)

    {index = va_arg(*argptr,int);
     if(index>=0 && index<*list_dim)
       {/* d_varb = *(*varb_ptr+index); */
	 d_varb_ptr = *varb_ptr+index;
	*pdata_type = DBL;}
     else
       error_code = INDEX_BOUNDS_ERROR;}			/* start or stop index outside bounds */

  else if ( operation == GET_LIST )
    {start = va_arg(*argptr,int);
     stop = va_arg(*argptr,int);
     put_addr = va_arg(*argptr,double *);
     if(start>=0 && stop<*list_dim) 
       {if(put_addr != NULL)
	  {for(i=0; i<=stop-start; i++)
	     put_addr[i] = *(*varb_ptr+i+start);
	   *pdata_type = DBL;}              
       else
	 error_code = NULL_ADDRESS_ERROR;}			/* NULL address provided to put data into */
     else
       error_code= INDEX_BOUNDS_ERROR;}				/* start or stop index outside bounds */

  else if ( operation == CLEAR)

    {if(*list_dim == 0) return(0);
     free_dvector(*varb_ptr,0,*list_dim);
     *list_dim = 0;}


  return(error_code);
}










/* ------------------------------------------------------------
   routine to handle storage control for a integer value item.

   last change:	Oct 1, 1992
   ------------------------------------------------------------ */

int
i_rw( operation, value, argptr, pdata_type )
int	operation, *value, *pdata_type;
va_list *argptr;
{
  int	error_code;

  error_code = 0;
  if ( operation == PUT)
    *value = (int) va_arg(*argptr,int);
  else if ( operation == GET)
    {/*i_varb = *value; */
      i_varb_ptr = value;
     *pdata_type = INT;} 
  else if ( operation == CLEAR)
    *value = 0;

	
  return(error_code);
}





/* ------------------------------------------------------------
   routine to handle storage control for a double value item.

   last change:	31 December 1990
   ------------------------------------------------------------ */

int
d_rw( operation, value, argptr, pdata_type )
int	operation, *pdata_type;
double  *value;
va_list *argptr;
{
  int	error_code;

  error_code = 0;
  if ( operation == PUT)
    *value = (double) va_arg(*argptr,double);
  else if ( operation == GET)
    {/* d_varb = *value; */
      d_varb_ptr = value; 
     *pdata_type = DBL;} 
  else if ( operation == CLEAR)
    *value = 0.0;

  return(error_code);
}





/* ------------------------------------------------------------
   routine to handle storage control for a character string 
   list item.

   last change:	31 December 1990
   ------------------------------------------------------------ */

int
c_list_rw( operation, varb_ptr, list_dim, item_length, argptr, pdata_type )
int	operation, *list_dim, *item_length, *pdata_type;
char ***varb_ptr;
va_list *argptr;
{
  int           index, i, error_code ;
  char		*put_addr, *get_addr; /* *calloc() */
  int           free_ivector(), imax_nonneg();

  error_code = 0;
  if( operation == INIT)
    {*list_dim = va_arg(*argptr,int);
     if(*list_dim != 0) {
	 *item_length = va_arg(*argptr,int);
	 *varb_ptr = (char **) calloc( *list_dim, sizeof(char *)); 
	 if(*varb_ptr == NULL) 
	     error_code= MEMORY_ALLOC_ERROR;	/* memory allocation error overrides */
                                /* INIT error */
	 else {
	     for(i=0;i<*list_dim;i++) {
		 *(*varb_ptr+i) = (char *) calloc( *item_length, sizeof(char));
		 if(*(*varb_ptr+i) == NULL) 
		     error_code= MEMORY_ALLOC_ERROR;
	     }
	 }
     }
 } 
  else if ( operation == PUT)
    {index = va_arg(*argptr,int);
     get_addr = (char *) va_arg(*argptr, char *);
     for(i=0;i<imax_nonneg( (int) *item_length, (int) strlen(get_addr) );i++)
       *(*(*varb_ptr+index)+i) = get_addr[i];  
     *(*(*varb_ptr+index)+i) = '\0';}
  else if ( operation == PUT_LIST )
    return(1);
  else if ( operation == GET)
    {index = va_arg(*argptr,int);
     put_addr = (char *) va_arg(*argptr, char *);
     c_varb_ptr = put_addr;
     for(i=0;i<imax_nonneg( (int) *item_length, (int) strlen(*(*varb_ptr+index)) );i++)
       put_addr[i] = *(*(*varb_ptr+index)+i);
     put_addr[i] = '\0';
     *pdata_type = STRNG;}
  else if ( operation == GET_LIST )
    return(1);                     
  else if ( operation == CLEAR)
    {if(*list_dim == 0) return(1);
     for(i=0;i<*list_dim;i++)
       free(*(*varb_ptr+i)); 
     free(*varb_ptr);
     *item_length = 0;
     *list_dim = 0;}

  return(error_code);
}



/* ------------------------------------------------------------
   routine to handle storage control for a memory object           

   last change:	29 January 1991
   ------------------------------------------------------------ */

int
mem_ptr_rw( operation, varb_ptr, argptr, pdata_type )
int	operation, *pdata_type;
void	**varb_ptr;
va_list *argptr;
{
  int             vdim, pdim, error_code ;
  int             mem_type, size = 1000, traj_length = 1000;

  error_code = 0;

  if( operation == INIT)
    {
      memory_destroy( (memory) *varb_ptr );
      mem_type = (int) va_arg(*argptr,int);
      vdim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
      pdim = *((int *) pm(GET, "Model.Param_Dim", NULL));
      *varb_ptr = (void *) memory_create( mem_type, size, traj_length, vdim, pdim, COLOR_DIM);
    }

  else if ( operation == PUT)
    {if(*varb_ptr==NULL)
       *varb_ptr = va_arg(*argptr,void *);
    else
      error_code = RESET_POINTER_ERROR;}		/* cannot reset mem pointer with PUT! */

  else if ( operation == PUT_LIST )
    error_code = INAPT_OP_ERROR;		/* inappropriate on a mem object */

  else if ( operation == GET)
    {addr_ptr = (void *) *varb_ptr;
     *pdata_type = ADDRS;}

  else if ( operation == GET_LIST )
    error_code = INAPT_OP_ERROR;		/* inappropriate on a mem object */

  else if ( operation == CLEAR)
    memory_destroy( *varb_ptr );


  return(error_code);
}



/* ------------------------------------------------------------
   routine to handle storage of the address for some object

   last change:	29 January 1991
   ------------------------------------------------------------ */

int
addr_rw( operation, varb_ptr, argptr, pdata_type )
int	operation, *pdata_type;
void	**varb_ptr;
va_list *argptr;
{
  int             error_code ;

  error_code = 0;

  if( operation == INIT)
    *varb_ptr = NULL;

  else if ( operation == PUT)
    {if(*varb_ptr==NULL)
       *varb_ptr = va_arg(*argptr,void *);
    else
      error_code = RESET_POINTER_ERROR;}		/* cannot reset pointer with PUT! */

  else if ( operation == PUT_LIST )
    error_code = INAPT_OP_ERROR;		/* inappropriate on a address object */

  else if ( operation == GET)
    {addr_ptr = (void *) *varb_ptr;
     *pdata_type = ADDRS;}

  else if ( operation == GET_LIST )
    error_code = INAPT_OP_ERROR;		/* inappropriate on a mem object */

  else if ( operation == CLEAR)
    *varb_ptr = NULL;


  return(error_code);
}



/* ------------------------------------------------------------
   routine to handle storage control for a character string 
   item.

   last change:	25 June 1991
   ------------------------------------------------------------ */

int
c_rw( operation, varb_ptr, item_length, argptr, pdata_type )
int	operation, *item_length, *pdata_type;
char **varb_ptr;
va_list *argptr;
{
  int             	i, error_code, imax_nonneg();
  char			*put_addr, *get_addr; /* *calloc() */

  error_code = 0;
  if( operation == INIT)
    {*item_length = va_arg(*argptr,int);
     *varb_ptr = (char *) calloc( *item_length, sizeof(char *));}
  else if ( operation == PUT)
    {get_addr = (char *) va_arg(*argptr, char *);
     for(i=0;i<imax_nonneg( (int) *item_length, (int) strlen(get_addr) );i++)
       *(*varb_ptr+i) = get_addr[i];  
     *(*varb_ptr+i) = '\0';}
  else if ( operation == PUT_LIST )
    return(1);
  else if ( operation == GET)
    {put_addr = (char *) va_arg(*argptr, char *);
     c_varb_ptr = put_addr;
     for(i=0;i<imax_nonneg( (int) *item_length, (int) strlen(*varb_ptr) );i++)
       put_addr[i] = *(*varb_ptr+i);
     put_addr[i] = '\0';
     *pdata_type = STRNG;}
  else if ( operation == GET_LIST )
    return(1);                     
  else if ( operation == CLEAR)
    {free(*varb_ptr);
     *item_length = 0;}

  return(error_code);
}


int
imax_nonneg( arg1, arg2 )
int	arg1, arg2;
{
   int    value;

   value = (arg2<arg1)? arg2:arg1;
   if( value < 0 ) value = 0;
   return( value );
}

int
    pm_savable_rw(operation,value_ptr,argptr)
int	operation,*value_ptr;
va_list	*argptr;

{
  int 	error_code = NO_ERROR;

  if ( operation == PUT_SAVABLE)
    *value_ptr = (int) va_arg(*argptr,int);
  else if ( operation == GET_SAVABLE)
/*    i_varb = *value_ptr; */
    i_varb_ptr = value_ptr;

  return(error_code);
}

/* Returns TRUE if a postmaster entry with key "label"
already exists and FALSE otherwise. */
int
  pm_exists_entry(label)
char 
  * label;

{

  extern struct pm_list * pm_hash_lookup();

  if (pm_hash_lookup(label) == (struct pm_list *) NULL )
    return FALSE;
  else
    return TRUE;

}

int
  pm_entry_exists(label)
char 
  * label;
{
  return pm_exists_entry(label);
}
