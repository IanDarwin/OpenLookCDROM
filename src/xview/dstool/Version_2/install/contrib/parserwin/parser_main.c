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
 * TOP LEVEL ROUTINES FOR THE DYNAMICAL SYSTEM PARSER
 *
 *
 *
 */
#include <stdlib.h>
#include "parser.h"
#include "parser_defs.h"

void 
*parser_create_ds()
{
  s_ds ds;
  int i;

  ds = (s_ds) malloc(sizeof(symbolic_ds));
  if (!ds) return NULL;
  for (i=0; i<N_EQ_NODES; i++)
    {
      ds->node[i] = NULL;
      ds->needs_temp_fn[i] = FALSE;
    }
  ds->temp_fn_values = NULL;
  ds->temp_token_str_stack = NULL;
  p_scanner(ds,"",PARSER_NEW);
  return( (void *) ds);
}

int parser_add_string(ds,str)
void *ds;
char *str;
{
  return(p_scanner((s_ds) ds, str, PARSER_CONTINUE));
}


int 
parser_print_ds(file,ds)
void *ds;
FILE *file;
{
  fprintf(file,"\nCOMPLETE DUMP OF SYMBOLIC DYNAMICAL SYSTEM\n");
  p_dump_ds(file, (s_ds) ds);
  return OK;
}

int 
parser_build_ds(ds)
void *ds;
{	
  int status = OK;
  
  if ((status = p_convert_token_strings((s_ds) ds)) != OK)
    {
      p_clear_all_token_strings((s_ds) ds);
      return status;
    }
  if (((s_ds) ds)->temp_token_str_stack)
    {
      p_clear_all_token_strings((s_ds) ds);
      return FAILURE;
    }
  if ((status = p_build_temp_fn((s_ds) ds)) != OK) return status;
  if ((status = p_build_tree((s_ds) ds)) != OK) return status;
  status = p_scan_built_tree((s_ds) ds);
  if (status != OK)
    fprintf(stdout,"parser_build_ds: invalid parser tree, error %d \n",status);
  return status;
}


int 
parser_get_n_vars(ds)
void *ds;
{
  return (p_count_eq_node_length(((s_ds) ds)->node[VAR_NODE]));
}

int 
  parser_get_n_pars(ds)
void *ds;
{
  return (p_count_eq_node_length(((s_ds) ds)->node[PAR_NODE]));
}

int 
parser_get_n_aux_fns(ds)
void *ds;
{
  return (p_count_eq_node_length(((s_ds) ds)->node[AUX_FN_NODE]));
}

int 
parser_get_n_temps(ds)
void *ds;
{
  return (p_count_eq_node_length(((s_ds) ds)->node[TEMP_FN_NODE]));
}

char 
*parser_get_var_name(ds,i)
{
  return( p_get_name_from_node(((s_ds) ds)->node[VAR_NODE],i));
}

char 
*parser_get_par_name(ds,i)
{
  return( p_get_name_from_node(((s_ds) ds)->node[PAR_NODE],i));
}

char 
*parser_get_aux_fn_name(ds,i)
{
  return( p_get_name_from_node(((s_ds) ds)->node[AUX_FN_NODE],i));
}

char 
*parser_get_temp_name(ds,i)
{
  return( p_get_name_from_node(((s_ds) ds)->node[TEMP_FN_NODE],i));
}

/*
 * evaluate RHS of dynamical system
 */
int 
parser_eval(ds,f,x,p)
void *ds;
double *f, *x, *p;
{
  p_eval_node_list((s_ds) ds, f, x, p, VAR_NODE);
  return OK;
}

/*
 * evaluate RHS of dynamical system
 */
int 
parser_eval_aux_fn(ds,f,x,p)
void *ds;
double *f, *x, *p;
{
  p_eval_node_list((s_ds) ds, f, x, p, AUX_FN_NODE);
  return OK;
}

int
parser_destroy_ds(ds)
void *ds;
{
  int i;

  if (ds)
    {
      for (i=0; i<N_EQ_NODES; i++)
	p_destroy_eq_node_list(((s_ds) ds)->node[i]);
      free(((s_ds) ds)->temp_fn_values);
      free(((s_ds) ds)->eval_stack);
      p_destroy_token_str_stack(&(((s_ds) ds)->temp_token_str_stack));
    }
  return OK;
}


int parser_get_var_ic(ds,n,pic)
void *ds;
int n;
double *pic;
{
  int status=FALSE;
  token *tok;

  if (ds && ((s_ds) ds)->node[INITIAL_NODE])
    {
      if (tok = ((s_ds) ds)->node[INITIAL_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[INITIAL_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == VAR && tok->u.var == n)
	    {
	      if (tok->next && tok->next->kind == CON)
		{
		  *pic = tok->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    }

  return status;
}


int parser_get_param_ic(ds,n,pic)
void *ds;
int n;
double *pic;
{
  int status=FALSE;
  token *tok;

  if (ds && ((s_ds) ds)->node[INITIAL_NODE])
    {
      if (tok = ((s_ds) ds)->node[INITIAL_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[INITIAL_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == PAR && tok->u.par == n)
	    {
	      if (tok->next && tok->next->kind == CON)
		{
		  *pic = tok->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    }

  return status;
}


int parser_get_var_periodic(ds,n,pmin,pmax)
void *ds;
int n;
double *pmin, *pmax;
{
  int status=FALSE;
  token *tok;

  if (ds && ((s_ds) ds)->node[PERIODIC_NODE])
    {
      if (tok = ((s_ds) ds)->node[PERIODIC_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[PERIODIC_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == VAR && tok->u.var == n)
	    {
	      if (tok->next && tok->next->kind == CON &&
		  tok->next->next && tok->next->next->kind == CON)
		{
		  *pmin = tok->next->u.con;
		  *pmax = tok->next->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    }
  return status;
}


int parser_get_var_range(ds,n,pmin,pmax)
void *ds;
int n;
double *pmin, *pmax;
{
  int status=FALSE;
  token *tok;

  if (ds && ((s_ds) ds)->node[RANGE_NODE])
    {
      if (tok = ((s_ds) ds)->node[RANGE_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[RANGE_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == VAR && tok->u.var == n)
	    {
	      if (tok->next && tok->next->kind == CON &&
		  tok->next->next && tok->next->next->kind == CON)
		{
		  *pmin = tok->next->u.con;
		  *pmax = tok->next->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    }
  return status;
}

int parser_get_param_range(ds,n,pmin,pmax)
void *ds;
int n;
double *pmin, *pmax;
{
  int status=FALSE;
  token *tok;

  if (ds && ((s_ds) ds)->node[RANGE_NODE])
    {
      if (tok = ((s_ds) ds)->node[RANGE_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[RANGE_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == PAR && tok->u.par == n)
	    {
	      if (tok->next && tok->next->kind == CON &&
		  tok->next->next && tok->next->next->kind == CON)
		{
		  *pmin = tok->next->u.con;
		  *pmax = tok->next->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    }
  return status;
}

int parser_get_aux_fn_range(ds,n,pmin,pmax)
void *ds;
int n;
double *pmin, *pmax;
{
  int status=FALSE;
  token *tok;

/*  if (ds && ((s_ds) ds)->node[RANGE_NODE])
    {
      if (tok = ((s_ds) ds)->node[RANGE_NODE]->eqn);
      else (tok = ((s_ds) ds)->node[RANGE_NODE]->eval_tree);
      while (tok)
	{
	  if (tok->kind == VAR  && tok->u.var == n)
	    {
	      if (tok->next && tok->next->kind == CON &&
		  tok->next->next && tok->next->next->kind == CON)
		{
		  *pmin = tok->next->u.con;
		  *pmax = tok->next->next->u.con;
		  status = OK;
		}
	      tok = NULL;
	    }
	  else tok = tok->next;
	}
    } */
  return status;
}


int
parser_write_ds_def(ds, fp)
void *ds;
FILE *fp;
{
  return(p_write((s_ds) ds,((s_ds) ds)->node[VAR_NODE],"'",fp));
}


int
parser_write_aux_fn(ds, fp)
void *ds;
FILE *fp;
{
  return(p_write((s_ds) ds,((s_ds) ds)->node[AUX_FN_NODE],"",fp));
}


int
parser_write_temp(ds, fp)
void *ds;
FILE *fp;
{
  return(p_write((s_ds) ds,((s_ds) ds)->node[TEMP_FN_NODE],"",fp));
}


int
parser_writeC_ds_def(ds, fp)
void *ds;
FILE *fp;
{
  return(p_writeC((s_ds) ds,((s_ds) ds)->node[VAR_NODE],((s_ds) ds)->needs_temp_fn[VAR_NODE],"f",fp));
}


int
parser_writeC_aux_fn(ds, fp)
void *ds;
FILE *fp;
{
  return(p_writeC((s_ds) ds,((s_ds) ds)->node[AUX_FN_NODE],((s_ds) ds)->needs_temp_fn[AUX_FN_NODE],"f",fp));
}


