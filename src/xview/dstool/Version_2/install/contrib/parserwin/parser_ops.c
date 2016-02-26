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
 * parser_ops.c
 *
 * The operational routines in the parser/symbolic ds package
 */
#include <stdlib.h>
#include "parser_defs.h"

/* breaks strings into token strings and adds to list */
/* the flag may be PARSER_NEW or PARSER_CONTINUE */
p_scanner(ds,s, flag)
s_ds ds;
char *s;
int flag;
{
  static token_str **current_stack = NULL;
  /* static node_name last_keyword; */
  token_str *ts, *temp;
  eq_node *node;
  node_name node_index;
  int i, j, new_var, status = OK;
  char *str;

  if (flag == PARSER_NEW || current_stack==NULL)
    current_stack = &(ds->temp_token_str_stack);

  while (((*s) != '\0') && (status == OK))
    {
      while (isspace(*s))
	s++;
      if (isdigit(*s) || (*s == '.'))
	{
	  s += p_get_num(s, &ts);
	  p_push_token_str(current_stack,ts);
	}
      else if (ismathc(*s))
	{
	  s += p_get_fc(s, &ts);
	  if (*(ts->str) == '=')
	    {
	      /* back up one and change stacks */
	      temp = p_pop_token_str(current_stack);
	      if (!temp || !isalpha(*(temp->str)))
		{
		  fprintf(stdout,"p_scanner: misplaced equals sign: = %s\n",s);
		  status = FAILURE;
		}
	      else
		{
		  /* Is it a new variable or aux function? */
		  str = temp->str;
		  i = strlen(str);
		  if (str[i-1] == '\'')
		    {
		      /* it wants to be a new variable */
		      new_var = TRUE;
		      str[i-1] = '\0';
		    }
		  else new_var = FALSE;
		  if (is_known_id(ds,str) == TRUE)
		    {
		      fprintf(stdout,"p_scanner: you cannot reassign %s = %s\n",str,s);
		      status = FAILURE;
		    }
		  else 
		    {
		      if (new_var == TRUE) node_index = VAR_NODE;
		      else if (*current_stack && ((i=is_key((*current_stack)->str)) != -1))
			{
			  p_destroy_token_str_stack(&ts);
			  ts = p_pop_token_str(current_stack);
			  node_index = i;
			}
		      else if (p_count_eq_node_length(ds->node[VAR_NODE]) == 0)
			node_index = TEMP_FN_NODE;
		      else node_index = AUX_FN_NODE;
		      if (key_data_type[node_index] == EQUATION)
			{
			  p_add_name_to_node(ds,node_index,str);
			  if ((i=is_name_in_node(ds,node_index,str)) >= 0)
			    {
			      node = ds->node[node_index];
			      for (j=0; j<i; j++) node = node->next;
			      current_stack = &(node->eqn_str);
			    }
			  else
			    {
			      fprintf(stdout,"p_scanner: BUG!\n");
			      status = FAILURE;
			    }
			}
		      else
			{
			  fprintf(stdout,"p_scanner: Cannot assign with keyword %s %s = %s\n",
				  key_dict[i], str, s);
			  status = FAILURE;
			}
		    }
		}
	      p_destroy_token_str_stack(&ts);
	      p_destroy_token_str_stack(&temp);
	    }
	  else p_push_token_str(current_stack,ts);
	}
      else if (isalpha(*s) || (*s == '_'))
	{
	  s += p_get_id(s, &ts);
	  if ((i = is_key(ts->str)) == -1)
	    p_push_token_str(current_stack,ts);
	  else
	    {
	      if (key_data_type[i] == EQUATION)
		{
		  current_stack = &(ds->temp_token_str_stack);
		  p_push_token_str(current_stack,ts);
		  /* last_keyword = key_indx[i]; */
		}
	      else if (key_data_type[i] == INFO)
		{
		  status = p_add_name_to_node(ds, i, "\0");
		  if (status == OK)
		    {
		      /* last_keyword = key_indx[i]; */
		      current_stack = &(ds->node[i]->eqn_str);
		    }
		  else
		    fprintf(stdout,"p_scanner: duplicate keyword %s\n",key_dict[i]);
		  p_destroy_token_str_stack(&ts);
		}
	      else
		{
		  status = FAILURE;
		  fprintf(stdout,"p_scanner: program error at %s\n",ts->str);
		  p_destroy_token_str_stack(&ts);
		}
	    }

	}
      else if((*s) != '\0')
	{
	  /* skip character! */
	  fprintf(stdout,"p_scanner: skipping %c\n",*s);
	  s++;
	}
    }
  return OK;
}


/* *********************************************
 * routines to convert token strings to tokens
 *
 * ********************************************* */

/*
 * p_convert_eq_node()
 *
 * tokenize a single node
 *
 */
int
p_convert_eq_node(ds, node, type)
s_ds ds;
eq_node *node;
node_name type;
{
  int t,status = OK;
/*  double atof(); */
  token *new, *new2;
  token_str *current;
  char *s;

  while ((current = p_pop_token_str(&(node->eqn_str))) && (status == OK))
    {
      s = current->str;
      if (isdigit(*s) || (*s == '.'))
	{
	  /* string is numeric */
	  if (new = p_create_token(CON))
	    new->u.con = atof(s);
	  else status = OUT_OF_MEMORY;
	}
      else if (isalpha(*s))
	{
	  /* string is alphanumeric, do we know it already, or is it a new param? */
	  if ((t=is_name_in_node(ds,VAR_NODE,s)) >= 0)
	    {
	      if (new = p_create_token(VAR))
		new->u.var = t;
	      else status = OUT_OF_MEMORY;
	    }
	  else if ((t=is_name_in_node(ds,TEMP_FN_NODE,s)) >= 0)
	    {
	      ds->needs_temp_fn[type] = TRUE;
	      if (new = p_create_token(TEMP_FN))
		new->u.temp_fn = t;
	      else status = OUT_OF_MEMORY;
	    }
	  else if ((t=is_name_in_node(ds,AUX_FN_NODE,s)) >= 0)
	    {
	      fprintf(stdout,"p_convert_eq_node: No aux fns on RHS!\n");
	      status = EQN_BADLY_FORMED;
	    }
	  else if ((t=is_con(s)) >= 0)
	    {
	      if (new = p_create_token(CON))
		new->u.con = con_indx[t];
	      else status =  OUT_OF_MEMORY;
	    }      
	  else if ((t=is_uop(s)) >= 0)
	    {
	      if (new = p_create_token(UOP))
		new->u.uop = uop_indx[t];
	      else status =  OUT_OF_MEMORY;
	    }      
	  else if ((t=is_bop(s)) >= 0)
	    {
	      if (new = p_create_token(BOP))
		new->u.bop = bop_indx[t];
	      else status =  OUT_OF_MEMORY;
	    }   
	  else
	    {
	      /* it's a parameter */
	      p_add_name_to_node(ds,PAR_NODE,s);
	      if ((t=is_name_in_node(ds, PAR_NODE,s)) >= 0)
		{
		  if (new = p_create_token(PAR))
		    new->u.par = t;
		  else status = OUT_OF_MEMORY;
		}
	      else status = PROGRAM_ERROR;
	    }
	}
      else if (ismathc(*s))
	{
	  if ((*s == '-') && 
	      ( key_data_type[type] == INFO ||
	       !node->eqn_str || (node->eqn_str->str && 
				     ismathc(*node->eqn_str->str) &&
				     (*node->eqn_str->str != ')'))))
	    {
	      /* tag - as unary op */
	      if (new = p_create_token(UOP))
		new->u.uop = MINUS;
	      else status = OUT_OF_MEMORY;
	    }
	  else
	    {
	      if (new = p_create_token(BOP))
		new->u.bop = *s;
	      else status = OUT_OF_MEMORY;
	    }
	}
      else
	{
	  fprintf(stdout,"p_convert_eq_node: ERROR token not recognized: %s\n",s);
	  status = PROGRAM_ERROR;
	}
      
      if (new) 
	{
	  switch (key_data_type[type])
	    {
	    case EQUATION:
	      /* check if we should add a multiplication operator */
	      if ( node->eqn && (new->kind != UOP) &&
		  ( (new->kind != BOP) || (new->u.bop == ')') ) &&
		  ( (node->eqn->kind != BOP) || (node->eqn->u.bop == '(')  ) )
		{
		  if (new2 = p_create_token(BOP))
		    new2->u.bop = '*';
		  else status = OUT_OF_MEMORY;
		  if (new2) p_push_token(&(node->eqn), new2);
		}
	      break;
	    case INFO:
	      if (new->kind == UOP)
		{
		  new2 = p_pop_token(&(node->eqn));
		  if (new2 && new2->kind == CON)  /* apply unary operator to const */
		    {
		      new2->u.con = uop_fn[new->u.uop](new2->u.con);
		      p_destroy_token_stack(&new);
		      new = new2;
		    }
		  else
		    {
		      fprintf(stdout,"p_convert_eq_node: illegal computation.\n");
		      status = EQN_BADLY_FORMED;
		      if (new2) p_push_token(&(node->eqn), new2);
		    }
		}
	      else if (new->kind != VAR && new->kind != PAR &&
		       new->kind != TEMP_FN && new->kind != CON)
		{
		  fprintf(stdout,"p_convert_eq_node: computation not allowed on rhs.\n");
		  status = EQN_BADLY_FORMED;
		}
	      break;
	    case BLANK:
	    default:
	      fprintf(stdout,"p_convert_eq_node: eqns formed incorrectly.\n");
	      status = EQN_BADLY_FORMED;
	    }
	  p_push_token(&(node->eqn), new);
	  new = NULL;
	}
    }
  p_destroy_token_str_stack(&current);
  return status;
}



/*
 * p_convert_eq_node_list()
 *
 * tokenize whole node list
 *
 */
p_convert_eq_node_list(ds,node, type)
s_ds ds;
eq_node *node;
node_name type;
{
  int status = OK;
  
  while (node && (status == OK))
    {
      status = p_convert_eq_node(ds, node, type);
      node = node->next;
    }
  return status;
}


/*
 * p_convert_token_strings(ds)
 *
 * take token strings and convert to tokens as
 *
 */
p_convert_token_strings(ds)
s_ds ds;
{
  int i, status = OK;

  for (i=0; (i<N_EQ_NODES) && (status == OK) ; i++)
    status = p_convert_eq_node_list(ds, ds->node[i], i);
  return status;
}


/*
 * setup structure for use of temporary variables
 */
p_build_temp_fn(ds)
s_ds ds;
{
  int n, i;
  
  n = p_count_eq_node_length(ds->node[TEMP_FN_NODE]);
  if (n>0)
    {
      ds->temp_fn_values = (double *) malloc(n*sizeof(double));
      if (!ds->temp_fn_values) return OUT_OF_MEMORY;
      for (i=0; i<n; i++)
	ds->temp_fn_values[i] = 0.0;
    }
  return OK;
}

/* ***************************************************
 * routines for building the evaluation tree
 *
 *
 * *************************************************** */


/*
 * routine to build evaluation tree at a single node
 */
int
p_build_tree_at_node(node)
eq_node *node;
{
  token *work_stack = NULL;
  token *t;
  int status = OK;
  token *next_token = node->eqn;	/* paw  5/31/92  */
  
/*  while ((status == OK) && (t = p_pop_token(&(node->eqn))))
    { */
  while ((status== OK) && (t=p_copy_token(next_token)))	/* paw  5/31/92  */
    {
      next_token = next_token->next;			/* paw  5/31/92  */
      switch (t->kind)
	{
	case VAR:
	case PAR:
	case CON:
	case TEMP_FN:
	  p_push_token_on_bottom(&(node->eval_tree),t);
	  break;
	case UOP:
	  p_push_token(&work_stack,t);
	  break;
	case BOP:
	  if (t->u.bop == '(')
	    p_push_token(&work_stack,t);
	  else if (t->u.bop == ')')
	    {
	      p_destroy_token_stack(&t);
	      do
		{
		  t = p_pop_token(&work_stack);
		  if (!t)
		    {
		      fprintf(stdout, "p_build_tree_at_node: unbalanced parentheses\n");
		      status = EQN_BADLY_FORMED;
		    }
		  else if ( (t->kind != BOP) || (t->u.bop != '(') )
		    {
		      p_push_token_on_bottom(&(node->eval_tree),t);
		    }
		  else p_destroy_token_stack(&t); /* it was the left parens */
		}
	      while ( t != NULL);
	    }
	  else
	    {
	      while  (work_stack && ( priority(work_stack) >= priority(t)))
		p_push_token_on_bottom(&(node->eval_tree), p_pop_token(&work_stack));
	      p_push_token(&work_stack, t);
	    }
	  break;
	}
    }

  while (work_stack && (status == OK))
    {
      t=p_pop_token(&work_stack);
      p_push_token_on_bottom(&(node->eval_tree), t);
      if ((t->kind == BOP) && (t->u.bop == '('))
	{
	  fprintf(stdout, "p_build_tree_at_node: unbalanced parentheses\n");
	  status = EQN_BADLY_FORMED;
	}
    }
  if (status != OK)
    {
      p_destroy_token_stack(&work_stack);
    }

  return status;
}


/*
 * routine to build evaluation tree for node list
 */
int
p_build_node_list(node)
eq_node *node;
{
  int status = OK;
  
  while (node && (status== OK))
    {	
      status = p_build_tree_at_node(node);
      node = node->next;
    }	
  return status;
}

/*
 * routine to build whole evaluation tree
 */
int
p_build_tree(ds)
s_ds ds;
{
  int i, status = OK;
  
  for (i=0; (i<N_EQ_NODES) && (status == OK); i++)
    status = p_build_node_list(ds->node[i]);
  if (status != OK) return status;
  /*
    scan for illegal trees
    setup array for evaluation
    */
  return status;
}


/* ***********************************************
 * evaluation routines
 *
 * *********************************************** */


/*
 * evaluate a single equation expression
 */
p_eval_tree(eqn, value, x, p, tempf, stack)
token *eqn;
double *value, *x, *p, *tempf, *stack;
{
  int status = OK;
  int st = 0;
  double op1, op2;

  while (eqn)
    {
      switch(eqn->kind)
	{
	case VAR:
	  stack[st++] = x[eqn->u.var];
	  break;
	case PAR:
	  stack[st++] = p[eqn->u.par];
	  break;
	case TEMP_FN:
	  stack[st++] = tempf[eqn->u.temp_fn];
	  break;
	case CON:
	  stack[st++] = eqn->u.con;
	  break;
	case UOP:
	  if (st>=0)
	    stack[st-1] = uop_fn[eqn->u.uop](stack[st-1]);
	  else
	    status = EQN_BADLY_FORMED;
	  break;
	case BOP:
	  if (st < 2)
	    status = EQN_BADLY_FORMED;
	  else
	    {
	      op2 = stack[--st];
	      op1 = stack[--st];
	      switch (eqn->u.bop)
		{
		case '*':
		  op1 *= op2;
		  break;
		case '+':
		  op1 += op2;
		  break;
		case '-':
		  op1 -= op2;
		  break;
		case '/':
		  op1 /= op2; /* divide by zero! */
		  break;
		case '^':
		  op1 = pow(op1,op2);
		  break;
		case '%':
		  op1 = fmod(op1,op2);
		  break;
		}
	      stack[st++] = op1;
	    }
	  break;
	}
      eqn = eqn->next;
    }

  if ( (status == OK) && (st==1) )
    {
      *value = *stack;
    }
  else
    {
      if (status == OK) status = EQN_BADLY_FORMED;
    }

  return status;
}


/*
 * evaluate all the nodes in a list
 */
p_eval_node_list(ds, f, x, p, node_indx)
s_ds ds;
double *f, *x, *p;
node_name node_indx;
{
  eq_node *n = ds->node[node_indx];
  int status = OK;

  if ((ds->needs_temp_fn[node_indx] == TRUE) && (node_indx != TEMP_FN_NODE))
    {
      status = p_eval_node_list(ds, ds->temp_fn_values, x, p, TEMP_FN_NODE);
    }
  while ((status==OK) && n)
    {
      status = p_eval_tree(n->eval_tree, f++, x, p, ds->temp_fn_values, ds->eval_stack);
      n = n->next;
    }
  return status;
}

/* **************************************************
 * routines to examine a built tree and set up
 * data structures necessary for speedy evaluation!
 * ************************************************** */

/*
 * p_check_tree()
 *
 * scans a single eqnode and returns maxdepth
 * or -1 if the tree will not evaluate OK
 *
 */
int
p_check_tree(tree)
token *tree;
{
  int max_depth = 0;
  int depth = 0;

  if (!tree) return 0;
  while (tree)
    {
      switch (tree->kind)
	{
	case VAR:
	case PAR:
	case CON:
	case TEMP_FN:
	  depth++;
	  break;
	case BOP:
	  depth--;
	case UOP:
	  if (depth<1) return -1;
	  break;
	default: /* invalid type */
	  return -1;
	}
      if (depth > max_depth) max_depth = depth;
      tree = tree->next;
    }
  if (depth==1) return max_depth;
  return -1;
}

int
p_check_node(n)
eq_node *n;
{
  int depth=0, max_depth = 0;

  while (n)
    {
      depth = p_check_tree(n->eval_tree);
      if (depth == -1) return -1;
      if (depth == 0) return 0;
      if (depth > max_depth) max_depth = depth;
      n = n->next;
    }
  return max_depth;
}

/*
 * p_scan_built_tree
 *
 * scans the built tree for errors which would occur when evaluating
 * equation types.
 * It also finds the largest stack necesary to compute expressions
 * and allocates memory in the ds accordingly.
 *
 */
p_scan_built_tree(ds)
s_ds ds;
{
  int status = OK;
  int i, n_dbls = 0, max_dbls = 0;

  for (i=0; i<N_EQ_NODES; i++)
    {
      if ((key_data_type[i] == EQUATION) && ds->node[i])
	{
	  n_dbls = p_check_node(ds->node[i]);
	  if (n_dbls == -1) return EQN_BADLY_FORMED;
	  if (n_dbls == 0) return MISSING_RHS;
	  if (n_dbls > max_dbls) max_dbls = n_dbls;
	}
    }

  if  (max_dbls > 0)
    {
      ds->eval_stack = (double *) malloc(max_dbls * sizeof(double));
      if (!ds->eval_stack) status = OUT_OF_MEMORY;
    }

  return status;
}


/* ****************************************
 *
 *   routines to write C code
 *
 * **************************************** */

p_writeC(ds, nd, temp_flag, name, fp)
s_ds ds;
eq_node *nd;
int temp_flag;
char *name;
FILE *fp;
{
  int i,status = OK;
  token *t;

  if (temp_flag==TRUE)
    {
      fprintf(fp, "\tdouble TEMP[%d];\n", p_count_eq_node_length(ds->node[TEMP_FN_NODE]));
      p_writeC(ds, ds->node[TEMP_FN_NODE], FALSE, "TEMP", fp); 
    }
  for(i=0; nd; i++, nd=nd->next)
    {
      fprintf(fp,"\t%s[%d] = ",name,i);
      p_writeC_token_list(fp,nd->eqn);
      fprintf(fp,";\n");
    }
  return status;
}

/* ****************************************
 *
 *   routines to equations in ENGLISH
 *
 * **************************************** */

p_write(ds, nd, str, fp)
s_ds ds;
eq_node *nd;
char *str;
FILE *fp;
{
  int i,status = OK;
  token *t;

  if (fp==stdout) fprintf(stdout,"\n");
  while (nd)
    {
      fprintf(fp, "\t%s%s = ", nd->name, str);
      p_write_token_list(fp, ds, nd->eqn);
      fprintf(fp, "\n");
      nd = nd->next;
    }
  return status;
}

