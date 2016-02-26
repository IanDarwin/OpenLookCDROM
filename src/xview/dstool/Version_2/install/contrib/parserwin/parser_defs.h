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
 * parser_defs.h
 * 
 * The header file for the parser/symbolic dynamical system library
 *
 */

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <ctype.h>
#include <stdio.h>
#include <math.h>

extern double sign();

#define TRUE 1
#define FALSE 0
#define OK 2
#define PARSER_NEW 3
#define PARSER_CONTINUE 4
#define FAILURE -1
#define OUT_OF_MEMORY -2
#define DUPLICATE_NAME -3
#define EQN_BADLY_FORMED -4
#define PROGRAM_ERROR -5
#define MISSING_RHS -6

/* token identifiers */
typedef enum {
        UOP, BOP, CON, VAR, PAR, TEMP_FN
	} kind_t;

typedef enum {
  VAR_NODE=0, PAR_NODE, AUX_FN_NODE, TEMP_FN_NODE, PERIODIC_NODE, INITIAL_NODE, RANGE_NODE,
  DUMMY
  } node_name;
#define N_EQ_NODES DUMMY

typedef enum {
  EQUATION, INFO, BLANK
} node_type;

/* unary operator types */
typedef enum {
        SIN, COS, TAN,
        ASIN, ACOS, ATAN,
        SINH, COSH, TANH,
        LOG, LN, EXP,
        ABS, SQRT, MINUS, SIGN
        } uop_t;


/* the item in a list of string tokens */
typedef struct token_str_item {
  char *str;
  struct token_str_item *next;
} token_str;


/* the item in a list of tokens */
typedef struct token_item {
  kind_t kind;
  union {
    uop_t uop;
    char bop;
    int var;
    int par;
    int aux_fn;
    int temp_fn;
    double con;
  } u;
  struct token_item *next;
} token;


typedef struct eq_node_item {
  char *name;
  token *eval_tree;
  token *eqn;
  token_str *eqn_str;
  struct eq_node_item *next;
} eq_node;


/* the full information on the symbolic dynamical system */
typedef struct {
  eq_node *node[N_EQ_NODES];
  double *temp_fn_values;
  int needs_temp_fn[N_EQ_NODES];
  token_str *temp_token_str_stack;
  double *eval_stack;
} symbolic_ds, *s_ds;

typedef struct dlist_item {
  double value;
  struct dlist_item *next;
} dlist;


typedef double (*PFD)(); /* ptr to function which returns a double */

extern char *uop_dict[];
extern char *uop_C_dict[];
extern uop_t uop_indx[];
extern PFD uop_fn[];
extern char *bop_dict[];
extern char bop_indx[];
extern char *con_dict[];
extern double con_indx[];
extern char *key_dict[];
extern node_name key_indx[];
extern node_type key_data_type[];

extern token_str *p_create_token_str();
extern int p_push_token_str();
extern token_str *p_pop_token_str();
extern int p_destroy_token_str_stack();
extern int p_clear_token_strings_in_node_list();
extern int p_clear_all_token_strings();

extern int p_get_num(), p_get_id(), p_get_fc();
extern int is_name_in_node();
extern int is_key(), is_con(), is_uop(), is_bop();
extern int is_known_id();

extern eq_node *p_create_eq_node();
extern int p_destroy_eq_node();
extern int p_destroy_eq_node_list();
extern int p_add_eq_node();
extern int p_add_name_to_node();
extern int p_count_eq_node_length();
extern char *p_get_name_from_node();

extern token *p_create_token();
extern token *p_copy_token();
extern int p_push_token();
extern token *p_pop_token();
extern int p_push_token_on_bottom();
extern token *p_pop_token_from_bottom();
extern int p_destroy_token_stack();

extern dlist *p_create_dlist();
extern int p_push_d();
extern double p_pop_d();
extern int p_destroy_d_stack();

extern int p_dump_token_list();
extern int p_writeC_token_list();
extern int p_write_token_list();
extern int p_dump_eq_node_list();
extern int p_dump_token_str_list();
extern int p_dump_ds();

extern double minus();
extern int priority();
extern int ismathc();
