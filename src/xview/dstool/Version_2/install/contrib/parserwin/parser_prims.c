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
 * parser_prims.c
 *
 * The low level routines in the parser/symbolic ds package
 */
#include <stdlib.h>
#include "parser_defs.h"

/* define the unary operator dictionary and index */
char    *uop_dict[] = {
		"sin",
		"cos",
		"tan",
		"asin",
		"acos",
		"atan",
		"sinh",
		"cosh",
		"tanh",
		"log",
		"ln",
		"exp",
		"abs",
		"sqrt",
		"minus",
		"sign",
		""
		};
char    *uop_C_dict[] = {
		"sin",
		"cos",
		"tan",
		"asin",
		"acos",
		"atan",
		"sinh",
		"cosh",
		"tanh",
		"log",
		"ln",
		"exp",
		"fabs",
		"sqrt",
		"-",
		"sign",
		""
		};
uop_t   uop_indx[] = {
		SIN,
		COS,
		TAN,
		ASIN,
		ACOS,
		ATAN,
		SINH,
		COSH,
		TANH,
		LOG,
		LN,
		EXP,
		ABS,
		SQRT,
		MINUS,
		SIGN,
		0
		};

PFD uop_fn[] = {
  sin,
  cos,
  tan,
  asin,
  acos,
  atan,
  sinh,
  cosh,
  tanh,
  log10,
  log,
  exp,
  fabs,
  sqrt,
  minus,
  sign,
  NULL };


/* define the binary operator dictionary and index */
char    *bop_dict[] = {
		"mod",
		""
		};
char    bop_indx[] = {
		'%',
		0
		};

/* define the constants dictionary and index */
char    *con_dict[] = {
		"PI",
		"pi",
		"E",
		"TWOPI",
		"twopi",
		""
		};
double  con_indx[] = {
		3.141592653589,
		3.141592653589,
		2.718281828459,
		6.283185307179,
		6.283185307179,
		0.
		};

/* define the keywords dictionary */
char *key_dict[] = {
  " DYNAMICAL SYSTEM",
  " PARAMETERS",
  " AUXILIARY FUNCTIONS",
  "TEMP",
  "PERIODIC",
  "INITIAL",
  "RANGE",
  ""};
node_name key_indx[] = {
  VAR_NODE,
  PAR_NODE,
  AUX_FN_NODE,
  TEMP_FN_NODE,
  PERIODIC_NODE,
  INITIAL_NODE,
  RANGE_NODE,
0};
node_type key_data_type[] = {
  EQUATION,
  BLANK,
  EQUATION,
  EQUATION,
  INFO,
  INFO,
  INFO,
0};

/* tests for math character */
int
ismathc(c)
char c;
{
  static char math_symbols[] = "*+-/^%=()";
  int i;

  for (i=0; i< (int) strlen(math_symbols); i++)
    if (c == math_symbols[i]) return TRUE;
  return(FALSE);
}





/* **********************************
 * token string primitives
 * ********************************** */

/* NOTE: creation does not copy string! */
token_str 
*p_create_token_str(str)
char *str;
{
  token_str *ts;
  
  ts = (token_str *) malloc(sizeof(token_str));
  ts->next = NULL;
  ts->str = str;
  return ts;
}

int
p_push_token_str(pstack, ts)
token_str **pstack, *ts;
{
  ts->next = *pstack;
  *pstack = ts;
  return OK;
}

token_str
*p_pop_token_str(pstack)
token_str **pstack;
{
  token_str *ts;

  ts = *pstack;
  if (ts) 
    {
      *pstack=ts->next;
      ts->next = NULL;
    }
  return ts;
}

/* note strings are destroyed ! */
int 
p_destroy_token_str_stack(pstack)
token_str **pstack;
{
  token_str *ptr;

  while (ptr = p_pop_token_str(pstack))
    {
      free(ptr->str);
      free(ptr);
    }
  return OK;
}

int 
p_clear_token_strings_in_node_list(node)
eq_node *node;
{
  while (node)
    {
      p_destroy_token_str_stack(&(node->eqn_str));
      node = node->next;
    }
  return OK;
}

int p_clear_all_token_strings(ds)
s_ds ds;
{
  int i;

  p_destroy_token_str_stack(&(ds->temp_token_str_stack));
  for (i=0; i<N_EQ_NODES; i++)
    p_clear_token_strings_in_node_list(ds->node[i]);
  return OK;
}

/* *************************************************************
 * routines to scan and break input strings into string tokens
 *
 * ************************************************************* */


/* read a number */
int p_get_num(s,ts)
char *s;
token_str **ts;
{
	char    *t;
	int     i = 0;

	while (isdigit(*(s+i)) || (*(s+i) == '.'))
		i++;
	t = (char *) malloc(i+1);
	strncpy(t, s, i);
	t[i] = '\0';
	*ts = p_create_token_str(t);
	return (i);
}

/* read an identifier */
int p_get_id(s, ts)
char    *s;
token_str **ts;
{
	char    *t;
	int     i = 0;

	while (isalnum(*(s+i)) || (*(s+i) == '_') || (*(s+i) == '.') || (*(s+i) == '\''))
		i++;
	t = (char *) malloc(i+1);
	strncpy(t, s, i);
	t[i] = '\0';
	*ts = p_create_token_str(t);
	return (i);
}

/* read first character */
int p_get_fc(s,ts)
char    *s;
token_str **ts;
{
	char    *t;

	t = (char *) malloc(2);
	t[0] = *s;
	t[1] = '\0';
	*ts = p_create_token_str(t);
	return (1);
}


/* ****************************************************
 * routines to recognize token strings
 *
 * **************************************************** */

/* used to search for var, par, aux_fn, ... */
int is_name_in_node(ds,node_indx,t)
s_ds ds;
node_name node_indx;
char    *t;
{
  int     i;
  eq_node *node = ds->node[node_indx];

  for (i = 0; node; i++)
    {
      if (node->name && (strcmp(t, node->name) == 0)) return (i);
      node = node->next;
    }
  return (-1);
}

/* is t a keyword ? */
int is_key(t)
char *t;
{
  int i;
  
  for (i = 0; strlen(key_dict[i]) != 0; i++)
    if (strcmp(t, key_dict[i]) == 0) return i;
  return -1;
}


/* is t a special constant? */
int is_con(t)
char    *t;
{
  int i;

  for (i = 0; strlen(con_dict[i]) != 0; i++)
    if (strcmp(t, con_dict[i]) == 0) return (i);
  return (-1);
}

/* is t a unary operator? */
int is_uop(t)
char *t;
{
  int i;

  for (i = 0; strlen(uop_dict[i]) != 0; i++)
    if (strcmp(t, uop_dict[i]) == 0) return (i);
  return (-1);
}

/* is t a binary operator? */
int is_bop(t)
char *t;
{
  int i;

  for (i = 0; strlen(bop_dict[i]) != 0; i++)
    if (strcmp(t, bop_dict[i]) == 0) return (i);
  return (-1);
}

/* is t a name in any eq_node or a reserved word (an identifier) ? */
int is_known_id(ds,t)
s_ds ds;
char *t;
{
  int i;
  
  for (i=0; i<N_EQ_NODES; i++)
    if (is_name_in_node(ds,i,t) != -1) return TRUE;
  if (is_key(t) != -1) return TRUE;
  if (is_con(t) != -1) return TRUE;
  if (is_uop(t) != -1) return TRUE;
  if (is_bop(t) != -1) return TRUE;
  return FALSE;
}


/* ******************************************
 * routines to manipulate eq_nodes
 *
 * ****************************************** */

/* note, node name is copied into node! */
eq_node
*p_create_eq_node(name_str)
char *name_str;
{
  eq_node *new;

  new = (eq_node *) malloc(sizeof(eq_node));
  if (!new) return NULL;
  new->name = (char *) malloc((strlen(name_str)+ 1)*sizeof(char));
  strcpy(new->name,name_str);
  if (!new->name)
    {
      free(new);
      return NULL;
    }
  new->eval_tree = NULL;
  new->eqn = NULL;
  new->eqn_str = NULL;
  new->next = NULL;
  return new;
}

int
p_destroy_eq_node(node)
eq_node *node;
{
  if (!node) return OK;
  free(node->name);
  p_destroy_token_str_stack(&node->eqn_str);
  p_destroy_token_stack(&node->eqn);
  free(node);
  return OK;
}

int
p_destroy_eq_node_list(node)
eq_node *node;
{
  eq_node *ptr;

  while (node)
    {
      ptr = node->next;
      p_destroy_eq_node(node);
      node = ptr;
    }
  return OK;
}

int 
p_add_eq_node(ppnode,s)
eq_node **ppnode;
char *s;
{
  eq_node **plist, *new;
  
  if (!(new = p_create_eq_node(s))) return OUT_OF_MEMORY;
  /* scroll to end of list and add */
  for (plist=ppnode; *plist; plist = &((*plist)->next));
  *plist = new;
  return OK;
}

int 
p_add_name_to_node(ds,node_indx,s)
s_ds ds;
node_name node_indx;
char *s;
{
  if (is_name_in_node(ds,node_indx,s) != -1) return DUPLICATE_NAME;
  return( p_add_eq_node(&ds->node[node_indx],s) );
}

int
p_count_eq_node_length(n)
eq_node *n;
{
  int i=0;
  while (n)
    {
      n = n->next;
      i++;
    }
  return i;
}

char
*p_get_name_from_node(n,i)
eq_node *n;
int i;
{
  int j=0;
  
  while (n && j<i)
    {
      n = n->next;
      j++;
    }
  if (j==i && n) return n->name;
  return NULL;
  }

/* *********************************************
 * routines to manipulate tokens 
 *
 * ********************************************* */

/*
 * p_create_token
 */
token 
*p_create_token(kind)
kind_t kind;
{
  token *t;

  t = (token *) malloc(sizeof(token));
  if (!t) return NULL;
  t->kind = kind;
  t->next = NULL;
  return t;
}

/*
 * p_copy_token
 */
token
*p_copy_token(t)
token *t;
{
  token *new;

  if (!t) return NULL;
  if (!(new=p_create_token(t->kind))) return NULL;
  switch(t->kind)
    {
    case VAR:
      new->u.var = t->u.var;
      break;
    case PAR:
      new->u.par = t->u.par;
      break;
    case TEMP_FN:
      new->u.temp_fn = t->u.temp_fn;
      break;
    case CON:
      new->u.con = t->u.con;
      break;
    case UOP:
      new->u.uop = t->u.uop;
      break;
    case BOP:
      new->u.bop = t->u.bop;
      break;
    }
  return new;
   
}

int
p_push_token(pstack, t)
token **pstack, *t;
{
  t->next = *pstack;
  *pstack = t;
  return OK;
}

token
*p_pop_token(pstack)
token **pstack;
{
  token *t;

  t = *pstack;
  if (t) 
    {
      *pstack=t->next;
      t->next = NULL;
    }
  return t;
}

int
p_push_token_on_bottom(pstack, t)
token **pstack, *t;
{
  token *tok = *pstack;
  
  while( tok )
    {
      pstack = &(tok->next);
      tok = tok->next;
    }
  *pstack = t;
  return OK;
}

token
*p_pop_token_from_bottom(pstack)
token **pstack;
{
  token *t;

  t = *pstack;
  if (!t) return NULL;
  while(t->next)
    {
      pstack = &(t->next);
      t = t->next;
    }
  *pstack = NULL;
  return t;
}


int 
p_destroy_token_stack(pstack)
token **pstack;
{
  token *ptr;

  while (ptr = p_pop_token(pstack)) 
    {
      free(ptr);
    }
  return OK;
}


/* *******************************************
 * printing routines
 *
 * ******************************************* */

int
p_dump_token_list(file,list)
FILE *file;
token *list;
{
  while (list)
    {
      if (list->kind == VAR) fprintf(file,"x[%d] ",list->u.var);
      else if (list->kind == PAR) fprintf(file,"p[%d] ",list->u.par);
      else if (list->kind == TEMP_FN) fprintf(file,"TEMP[%d] ",list->u.temp_fn);
      else if (list->kind == CON) fprintf(file,"%lg ",list->u.con);
      else if (list->kind == BOP) fprintf(file,"%c ",list->u.bop);
      else if (list->kind == UOP) fprintf(file,"%s ",uop_dict[list->u.uop]);
      else fprintf(file,"UNKNOWN_TOKEN ");
      list = list->next;
    }
}

int
p_write_token_list(file,ds,list)
FILE *file;
s_ds ds;
token *list;
{
  token *temp;
  while (list)
    {
      if (list->kind == VAR) fprintf(file,"%s", p_get_name_from_node(ds->node[VAR_NODE], list->u.var));
      else if (list->kind == PAR) 
	fprintf(file,"%s", p_get_name_from_node(ds->node[PAR_NODE], list->u.par));
      else if (list->kind == TEMP_FN) 
	fprintf(file,"%s", p_get_name_from_node(ds->node[TEMP_FN_NODE], list->u.temp_fn));
      else if (list->kind == CON) 
	fprintf(file,"%.14lg", list->u.con);
      else if (list->kind == BOP) 
	fprintf(file,"%c",list->u.bop);
      else if (list->kind == UOP)
	{ /* may want to make more elaborate */
	    fprintf(file,"%s",uop_C_dict[list->u.uop]);
	}
      else fprintf(file,"UNKNOWN_TOKEN ");
      list = list->next;
    }
}

int
p_writeC_token_list(file,list)
FILE *file;
token *list;
{
  token *temp;
  while (list)
    {
      if (list->kind == VAR) fprintf(file,"x[%d]",list->u.var);
      else if (list->kind == PAR) fprintf(file,"p[%d]",list->u.par);
      else if (list->kind == TEMP_FN) fprintf(file,"TEMP[%d]",list->u.temp_fn);
      else if (list->kind == CON) fprintf(file,"%.14lg",list->u.con);
      else if (list->kind == BOP) fprintf(file,"%c",list->u.bop);
      else if (list->kind == UOP)
	{ /* may want to make more elaborate */
	    fprintf(file,"%s",uop_C_dict[list->u.uop]);
	}
      else fprintf(file,"UNKNOWN_TOKEN ");
      list = list->next;
    }
}



int
p_dump_eq_node_list(file,node, type)
FILE *file;
eq_node *node;
node_name type;
{
  while(node)
    {
      fprintf(file,"%s",node->name);
      if (type == VAR_NODE)
	fprintf(file,"' = ");
      else if (key_data_type[type] == EQUATION)
	fprintf(file," = ");
      p_dump_token_str_list(file,node->eqn_str);
      p_dump_token_list(file,node->eqn);
      fprintf(file," <==> ");
      p_dump_token_list(file,node->eval_tree);
      fprintf(file,"\n");
      node = node->next;
    }
  return OK;
}

int
p_dump_token_str_list(file,list)
FILE *file;
token_str *list;
{
  while (list)
    {
      fprintf(file,"%s ",list->str);
      list = list->next;
    }
  return OK;
}
      
int
p_dump_ds(file,ds)
s_ds ds;
FILE *file;
{
  int i;

  for (i=0; i<N_EQ_NODES; i++)
    {
      if (ds->node[i])
	{
	  fprintf(file,"%s\n", key_dict[i]);
	  p_dump_eq_node_list(file, ds->node[i], i);
	}
    }
  if (ds->temp_token_str_stack)
    {
      fprintf(file,"TOKEN STRING LIST:\n");
      p_dump_token_str_list(file,ds->temp_token_str_stack);
    }
  return OK;
}
 
/* *************************************
 * Special functions to teach the system
 *
 * ************************************* */

/* unary minus */
double minus(op)
double op;
{
  return(-op);
}

/* compute priority */
int
priority(t)
token *t;
{
  if (t->kind == UOP) return 16;
  else if (t->kind == BOP)
    {
      switch (t->u.bop)
	{
	case '(': return 0;

	case '%': return 1;
	  
	case '+':
	case '-': return 2;

	case '*':
	case '/': return 4;

	case '^': return 8;
	}
    }
  return -1;
}
