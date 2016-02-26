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

/* -----------------------------------------------------------------------
   postmaster main entry point 
   Remark:  Variable argument length routines are not highly portable -
	    this code will require conversion if dstool is ported to 
	    another architecture!!    

   last change:  Sep 25, 1992
   ----------------------------------------------------------------------- */


/* Changed from i_varb to i_varb_ptr so that multiple postmaster calls
inside a function call will not interfere with each other. A pointer to the
underlying int isside the postmaster data structure is now used for 
communication between pm_access and pm instead of (the address of) the global
variable i_varb. Similarly d_varb. ab 5/95*/

/* int		i_varb;	*/		/* temp storage if returning an integer or integer list */
int		*i_varb_ptr;			/* temp storage if returning an integer or integer list */
/* double		d_varb;*/			/* temp storage if returning double or double list */
double		*d_varb_ptr;			/* temp storage if returning double or double list */
char		*c_varb_ptr;		/* temp storage if returning a character string */
void 		*addr_ptr;		/* temp storage if returning an address */


void *        
pm(va_alist)
va_dcl

{
  extern  int	valid_op();
  int		object,		operation;

  static int error_code;

  int  		get_op,		first_get,	fetch_status;

  char          label[MAX_LABEL_LEN];

  va_list	argptr;         
  int		data_type;	/* flag to indicate return data type */

  va_start(argptr);		/* initialize the variable-length argument list */

  get_op = first_get = FALSE;
  error_code = 0;

  operation = (int) va_arg(argptr,int);					/* fetch operation id code  */
  while( fetch_status=valid_op(operation) == TRUE && error_code >= 0)   /* For as long as ops are   */
    {                                                                   /*   valid and no errs...   */
      if(first_get == TRUE && (operation==GET || operation==GET_LIST || operation == GET_SAVABLE))
	{error_code = MULTIPLE_GET_ERROR;
	 break;} 

      strcpy( label, (char *) va_arg(argptr,int) );  			/* copy obj/elem label      */

      error_code = pm_access(operation, label, &argptr, &data_type);

      if(operation == GET || operation == GET_LIST  || operation == GET_SAVABLE)
	{first_get = TRUE;
	 get_op = operation;}

      operation = (int) va_arg(argptr,int);
      if(operation == NULL) break;
    }

  va_end(argptr);
  if(fetch_status == FALSE) error_code= INAPT_OP_ERROR;
	
  if (get_op == GET )
    {pm_set_error(0,"pm_get");
     if (data_type == DBL )
/*       return( (void *) &d_varb); */
       return( (void *) d_varb_ptr);
     else if (data_type == INT)
/*       return( (void *) &i_varb); */
      return( (void *) i_varb_ptr); 
     else if (data_type == ADDRS)
       return( (void *) addr_ptr); 
     else if (data_type == STRNG)
       return( (void *) c_varb_ptr);
     else return (void *) NULL;}   /* added 8/20 */
  else  if (get_op == GET_SAVABLE ) 
    {pm_set_error(0,"pm_get_savable");
/*       return( (void *) &i_varb);} */
       return( (void *) i_varb_ptr);}
  else
    {
      return( (void *) &error_code);
    }
}



/* ------------------------------------------------------------------------------
   check for vaild postmaster operation request

   last change: 1 January 1991
   ------------------------------------------------------------------------------ */
int
valid_op(operation)
int	operation;
{
  if(operation==INIT        || operation==PUT      || 
     operation==GET         || operation==CLEAR    || 
     operation==GET_LIST    || operation==PUT_LIST ||
     operation==CREATE_OBJ  || operation==RM_OBJ   || 
     operation==CREATE_ELEM || operation==RM_ELEM  ||
     operation==GET_SAVABLE || operation==PUT_SAVABLE ||
     operation==EXEC        || operation==NULL)

    return(TRUE);
  else
    return(FALSE);
}


char	*pos_errors[]={	"normal exit",
			"Variable already initialized",
			"NULL address provided in a fetch call. No action."};

char	*neg_errors[]={ "normal exit",
			"Memory allocation failure.",
			"More than one GET or GETLIST op requested in a pm() call.",
			"Invalid data structure request.",
			"Invalid operation on a data structure requested.",
			"List item not initialized before use.",
			"Incorrect bounds requested on a list access.",
			"Cannot reset an address pointer with PUT."};



/* ------------------------------------------------------------------------------
   proc used to set the postmaster error handler

   last change: 16 Sept 1991
   ------------------------------------------------------------------------------ */

static	int	last_error;
static	char	error_set_by[30];

pm_set_error(error_code,called_by)
int	error_code;
char	*called_by;
{

  sprintf(error_set_by,"%s",called_by); 

  if( ( last_error >= 0 && error_code < 0) || 
      ( last_error < 0  && error_code < 0) || 
        last_error == 0   			) last_error = error_code;

  if(error_code<0)
    {
     fprintf(stderr, "Error in postmaster data structure: %s   Error code = %d. \n",error_set_by,last_error);
     fprintf(stderr, "%s \n",neg_errors[abs(error_code)]);
    }
}


/* ------------------------------------------------------------------------------
   proc used to fetch the last postmaster error code   
    
     args in:	op		0  returns only integer status code; otherwise,
				   also return string error description

		addr		pre-allocated character string;  if op != 0, 
				then dump string descriptor into addr
 				
   last change: 16 Sept 1991
   ------------------------------------------------------------------------------ */

int
pm_get_error( op, addr )
int	op;
char	*addr;
{
  int	i;
  char	*error_str;

  error_str = ( last_error >= 0 )? pos_errors[last_error]:neg_errors[abs(last_error)];

  if( op != 0 )
    {
     for(i=0; i< (int) strlen( error_str ); i++)
       addr[i] = error_str[i];
    }
  return( (int) last_error );
}

