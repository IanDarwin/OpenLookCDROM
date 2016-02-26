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
 * parser.h
 * 
 * The header file for the parser/symbolic dynamical system library
 *
 */

#define PARSER_OK 2
#define PARSER_OUT_OF_MEMORY -2

#define INDEP_VF "time"
#define INDEP_MAP "iter"
#define INDEP_MIN_VF 0.0
#define INDEP_MIN_MAP 0.0
#define INDEP_MAX_VF 10000.0
#define INDEP_MAX_MAP 10000.0
#define VAR_IC_VF 0.0
#define VAR_IC_MAP 0.0
#define VAR_MIN_VF -10.0
#define VAR_MIN_MAP -1.0
#define VAR_MAX_VF 10.0
#define VAR_MAX_MAP 1.0
#define PAR_IC_VF 1.0
#define PAR_IC_MAP 1.0
#define PAR_MIN_VF -10.0
#define PAR_MIN_MAP -10.0
#define PAR_MAX_VF 10.0
#define PAR_MAX_MAP 10.0
#define FN_MIN_VF -10.0
#define FN_MIN_MAP -10.0
#define FN_MAX_VF 10.0
#define FN_MAX_MAP 10.0


extern void *parser_create_ds();
/*
    parser_create_ds()

    Creates a symbolic dynamical system structure and
    returns a pointer to it (cast to a void *).  If memory
    allocation fails, then a NULL ptr is returned.
*/



extern int parser_add_string();
/*
    parser_add_string(ds, str);
    ds is a ptr to the symbolic dynamical system
    str is a string of input to the parser

    This routine interprets the input and starts to build the dynamical system.
    It may be called repetitively to build the system.
    If it is accepted then, it returns PARSER_OK.
    If it is not accepted then appropriate error
    codes are returned.
*/



extern int parser_build_ds();
/*
    parser_build_ds(ds);
    ds is a ptr to the symbolic dynamical system

    This routine builds the evaluation tree.  It should be
    called after all the input strings have been added
    using parser_add_string and before any evaluation is attempted.
    If successful at building an evaluation tree, it will return PARSER_OK.
    If not other error messages are returned.
*/



extern int parser_get_n_vars();
/*
    parser_get_n_vars(ds);
    ds is a ptr to the symbolic dynamical system

    This routine returns the number of variables in the symbolic dynamical system.
    It may be called before calling parser_build_ds, or even when the system
    is not completely built (ie, more parser_add_string calls)
*/



extern int parser_get_n_params();
/*
    parser_get_n_params(ds);
    ds is a ptr to the symbolic dynamical system

    This routine returns the number of parameters in the symbolic dynamical system.
    It may be called before calling parser_build_ds, or even when the system
    is not completely built (ie, more parser_add_string calls)

*/



extern int parser_get_n_aux_fns();
/*
    parser_get_n_aux_fns(ds);
    ds is a ptr to the symbolic dynamical system

    This routine returns the number of auxiliary functions in the symbolic dynamical system.
    It may be called before calling parser_build_ds, or even when the system
    is not completely built (ie, more parser_add_string calls)

*/



extern int parser_get_n_temps();
/*
    parser_get_n_temps(ds);
    ds is a ptr to the symbolic dynamical system

    This routine returns the number of temporary functions in the symbolic dynamical system.
    It may be called before calling parser_build_ds, or even when the system
    is not completely built (ie, more parser_add_string calls)

*/



extern char *parser_get_var_name();
/*
    parser_get_var_name(ds,i)
    ds is a ptr to the symbolic dynamical system
    i is index to the variable you want, starting with 0

    Returns a ptr to the i'th variable name, counting from 0.
    It returns NULL if the index is out of bounds.
*/



extern char *parser_get_par_name();
/*
    parser_get_par_name(ds,i)
    ds is a ptr to the symbolic dynamical system
    i is index to the parameter you want, starting with 0

    Returns a ptr to the i'th parameter name, counting from 0.
    It returns NULL if the index is out of bounds.
*/



extern char *parser_get_aux_fn_name();
/*
    parser_get_aux_fn_name(ds,i)
    ds is a ptr to the symbolic dynamical system
    i is index to the auxiliary function you want, starting with 0

    Returns a ptr to the i'th auxiliary function name, counting from 0.
    It returns NULL if the index is out of bounds.
*/



extern char *parser_get_temp_name();
/*
    parser_get_aux_fn_name(ds,i)
    ds is a ptr to the symbolic dynamical system
    i is index to the temporary function you want, starting with 0

    Returns a ptr to the i'th temporary function name, counting from 0.
    It returns NULL if the index is out of bounds.
*/



extern int parser_eval();
/*
    parser_eval(ds, f, x, p)
    ds is a ptr to the symbolic dynamical system
    f,x,p are vectors of doubles

    This routine takes x and p as the phase space and parameter
    input respectively to the vector field/mapping and returns
    the vector result in f.  f,x,p must all be allocated to the
    correct dimensions before calling this routine.
    (additionally, a parser_build_ds call must be made before
    any evaluations can be performed)
    It returns PARSER_OK if the evaluation was succesful,
    else an error code is returned.
*/



extern int parser_eval_aux_fn();
/*
    parser_eval_aux_fn(ds, f, x, p)
    ds is a ptr to the symbolic dynamical system
    f,x,p are vectors of doubles

    This routine takes x and p as the phase space and parameter
    input respectively to the auxiliary functions, and returns
    the vector result in f.  f,x,p must all be allocated to the
    correct dimensions before calling this routine.
    (additionally, a parser_build_ds call must be made before
    any evaluations can be performed)
    It returns PARSER_OK if the evaluation was succesful,
    else an error code is returned.
*/



extern int parser_destroy_ds();
/*
  parser_destroy_ds(ds)
  ds is a ptr to a symbolic dynamical system

  This routines deallocates frees all memory allocated to the
  symbolic dynamical system.  The structure pointed to by ds is
  destroyed.
*/



extern int parser_print_ds();
/*
    parser_print_ds(file,ds);
    file is where you  wnat the output to go
    ds is a ptr is the symbolic dynamical system

    This routines dumps the contents of a symbolic dynamical system
    to a file.
*/

extern int parser_get_var_ic();
extern int parser_get_param_ic();
/*
  parser_get_var_ic(ds,n,pval);
  parser_get_param_ic(ds,n,pval);
  ds is a ptr to the symbolic dynamical system
  n is the index of the variable or parameter
  pval is a pointer to a double
  
  This routine puts the initial value of the specified variable
  or parameter in *pval.
  If this has not been specified then the routine returns FALSE
  else it returns OK.
*/

extern int parser_get_var_periodic();
/*
  parser_get_var_periodic(ds,n,p_low,p_high);
  ds is a ptr to the symbolic dynamical system
  n is the index of the variable or parameter
  p_low and p_high are pointers to doubles
  
  This routine puts the min and max values of the specified periodic variable
  in *p_low and *p_high.
  If this has not been specified then the routine returns FALSE
  else it returns OK.
*/

extern int parser_get_var_range();
extern int parser_get_param_range();
extern int parser_get_aux_fn_range();
/*
  parser_get_var_range(ds,n,p_min, p_max);
  parser_get_param_range(ds,n,p_min, p_max);
  parser_get_aux_fn_range(ds,n,p_min, p_max);
  ds is a ptr to the symbolic dynamical system
  n is the index of the variable, parameter, or aux fn
  p_min and p_max are pointers to doubles
  
  This routine puts the default range of the specified variable,
  parameter or aux fn in *p_min and *p_max.
  If this has not been specified then the routine returns FALSE
  else it returns OK.
*/


extern int parser_writeC_ds_def();
extern int parser_writeC_aux_fn();
/*
  parser_writeC_ds_def(ds, f);
  parser_writeC_aux_fn(ds, f);
  ds is a ptr to the symbolic dynamical system
  f is a ptr to a FILE

  This routine attempts to write C language code
  to evaluate the dynamical system or auxiliary functions.
  The procedure parser_build_ds must be called before this
  routine.
*/


extern int parser_write_ds_def();
extern int parser_write_aux_fn();
extern int parser_write_temp();
/*
  parser_write_ds_def(ds, f);
  parser_write_aux_fn(ds, f);
  parser_write_temp(ds, f);
  ds is a ptr to the symbolic dynamical system
  f is a ptr to a FILE

  This routine writes the equation it is using
  to evaluate the dynamical system, temporary functions, or auxiliary functions.
  The procedure parser_build_ds must be called before this
  routine.
*/
