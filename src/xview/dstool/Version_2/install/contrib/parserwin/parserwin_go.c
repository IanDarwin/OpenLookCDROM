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
#include <manifold.h>
#include <constants.h>
#include <defaults.h>
#include <version.h>
#include <stdlib.h>

#include "parser.h"

static void *symbolic_ds;

int
parser_ds_def(f,x,p)
double f[],x[],p[];
{
  int status = parser_eval(symbolic_ds, f, x, p);
  if (status != PARSER_OK)
    fprintf(stdout,"parser_ds_def: PARSER ERROR in evaluation.\n");
  return 0;
}

int
parser_ds_func(f,x,p)
double f[],x[],p[];
{
  int status = parser_eval_aux_fn(symbolic_ds,f,x,p);
  if (status != PARSER_OK)
    fprintf(stdout,"parser_ds_def: PARSER ERROR in evaluation.\n");
  return 0;
}

int
parser_ds_jac(f,x,p)
double f[],x[],p[];
{
  int status = PARSER_OK;

  if (status != PARSER_OK)
    fprintf(stdout,"parser_ds_def: PARSER ERROR in evaluation.\n");
  return 0;
}

int
parser_ds_inv(f,x,p)
double f[],x[],p[];
{
  int status = PARSER_OK;

  if (status != PARSER_OK)
    fprintf(stdout,"parser_ds_def: PARSER ERROR in evaluation.\n");
  return 0;
}


/*
 * contains the code for implementing parsable dynamical systems
 *
 */
int
parserwin_go(ds_type, buffer)
int ds_type;
char *buffer;
{
  int i, n_param, status = PARSER_OK;
  int n_varb;
  void *ds;
  
  int *ivector(), free_ivector(), free_dvector();
  double *dvector();
  char **variable_names;
  double *variables, *variable_min, *variable_max;
  double *parameters, *parameter_min, *parameter_max;
  int n_funct;
  char **funct_names;
  double *funct_min, *funct_max;
  char **parameter_names;

  int mapping_toggle, inverse_toggle;
  char *indep_varb_name;
  double indep_varb_min, indep_varb_max;

  int manifold_type;
  int	*periodic_varb;
  double   *period_start;
  double   *period_end;
  int            (*def_name)()=parser_ds_def;
  int            (*jac_name)()=NULL;
  int            (*aux_func_name)()=parser_ds_func;
  int            (*inv_name)()=NULL;
  int            (*dfdt_name)()=NULL;
  int            (*dfdparam_name)()=NULL;



  ds = parser_create_ds();
  status = parser_add_string(ds,buffer);

  if (status != PARSER_OK)
    {
      fprintf(stdout,"parser_go: Could not parse strings.\n");
      parser_destroy_ds(ds);
      return status;
    }

  status = parser_build_ds(ds);
  if (status != PARSER_OK)
    {
      fprintf(stdout,"parser_go: Could not build tree from equations.\n");
      parser_destroy_ds(ds);
      return status;
    }
  /* parser_print_ds(stdout,ds); */

  /* discard old tree and use new one! */
  parser_destroy_ds(symbolic_ds);
  symbolic_ds = ds;

  if (ds_type)
    {
      /* mapping */
      mapping_toggle = TRUE;
      inverse_toggle = FALSE;
      indep_varb_name = INDEP_MAP;
      indep_varb_min = INDEP_MIN_MAP;
      indep_varb_max = INDEP_MAX_MAP;
      manifold_type = EUCLIDEAN;
    }
  else
    {
      /* vector field */
      mapping_toggle = FALSE;
      inverse_toggle = FALSE;
      indep_varb_name = INDEP_VF;
      indep_varb_min = INDEP_MIN_VF;
      indep_varb_max = INDEP_MAX_VF;
      manifold_type = EUCLIDEAN;
    }

  n_varb = parser_get_n_vars(ds);
  variable_names = (char **) calloc(n_varb, sizeof(char *));
  variables = dvector(0, n_varb-1);
  variable_min = dvector(0, n_varb-1);
  variable_max = dvector(0, n_varb-1);
  periodic_varb = ivector(0, n_varb-1);
  period_start = dvector(0, n_varb-1);
  period_end = dvector(0, n_varb-1);

  for (i=0; i<n_varb; i++)
    {
      variable_names[i] = parser_get_var_name(ds,i);
      variables[i] = ds_type ? VAR_IC_MAP : VAR_IC_VF;
      parser_get_var_ic(ds,i,variables+i);
      variable_min[i] = ds_type ? VAR_MIN_MAP : VAR_MIN_VF;
      variable_max[i] = ds_type ? VAR_MAX_MAP : VAR_MAX_VF;
      parser_get_var_range(ds,i,variable_min+i,variable_max+i);
      periodic_varb[i] = FALSE;
      period_start[i] = 0.0;
      period_end[i] = 0.0;
      if (parser_get_var_periodic(ds,i,period_start+i,period_end+i)==PARSER_OK)
	{
	  periodic_varb[i] = TRUE;
	  manifold_type = PERIODIC;
	}
    }

  n_param = parser_get_n_pars(ds);
  parameter_names = (char **) calloc(n_param, sizeof(char *));
  parameters = dvector(0, n_param-1);
  parameter_min = dvector(0, n_param-1);
  parameter_max = dvector(0, n_param-1);
  for (i=0; i<n_param; i++)
    {
      parameter_names[i] = parser_get_par_name(ds, i);
      parameters[i] = ds_type ? PAR_IC_MAP : PAR_IC_VF;
      parser_get_param_ic(ds,i,parameters+i);
      parameter_min[i] = ds_type ? PAR_MIN_MAP : PAR_MIN_VF;
      parameter_max[i] = ds_type ? PAR_MAX_MAP : PAR_MAX_VF;
      parser_get_param_range(ds,i,parameter_min+i,parameter_max+i);
    }

  n_funct = parser_get_n_aux_fns(ds);
  funct_names = (char **) calloc(n_funct, sizeof(char *));
  funct_min = dvector(0, n_funct-1);
  funct_max = dvector(0, n_funct-1);
  for (i=0; i<n_funct; i++)
    {
      funct_names[i] = parser_get_aux_fn_name(ds, i);
      funct_min[i] = ds_type ? FN_MIN_MAP : FN_MIN_VF;
      funct_max[i] = ds_type ? FN_MAX_MAP : FN_MAX_VF;
      parser_get_aux_fn_range(ds,i,funct_min+i,funct_max+i);
    }


#include <ds_define.c>

  cfree(variable_names);
  cfree(parameter_names);
  cfree(funct_names);
  free_dvector(variables, 0, n_varb-1);
  free_dvector(variable_min, 0, n_varb-1);
  free_dvector(variable_max, 0, n_varb-1);
  free_dvector(parameters, 0, n_param-1);
  free_dvector(parameter_min, 0, n_param-1);
  free_dvector(parameter_max, 0, n_param-1);
  free_dvector(period_start, 0, n_varb-1);
  free_dvector(period_end, 0, n_varb-1);
  free_ivector(periodic_varb, 0, n_varb-1);
  free_dvector(funct_min, 0, n_funct-1);
  free_dvector(funct_max, 0, n_funct-1);

  return status;
}


/*
 * the code for writing c-files out of the parser window info
 *
 */
int
parserwin_writec(ds_type, ds_name, buffer, fp)
int ds_type;
char *ds_name, *buffer;
FILE *fp;
{
  void *ds;
  int status, n_varb, n_param, n_funct, n_temp, i, manifold_type=EUCLIDEAN, flag;
  double defaultd, temp;
  char name[30];

  ds = parser_create_ds();
  status = parser_add_string(ds,buffer);

  if (status != PARSER_OK)
    {
      fprintf(stdout,"parser_writec: Could not parse strings.\n");
      parser_destroy_ds(ds);
      return status;
    }

  status = parser_build_ds(ds);
  if (status != PARSER_OK)
    {
      fprintf(stdout,"parser_writec: Could not build tree from equations.\n");
      parser_destroy_ds(ds);
      return status;
    }

  strncpy(name, ds_name, 30);

  n_varb = parser_get_n_vars(ds);
  n_param = parser_get_n_pars(ds);
  n_funct = parser_get_n_aux_fns(ds);
  n_temp = parser_get_n_temps(ds);
  for (i=0; i<n_varb; i++)
    if (parser_get_var_periodic(ds,i,&defaultd,&temp)==PARSER_OK) manifold_type = PERIODIC;

  fprintf(fp, "/*\n * %s - automatically generated model file\n * %s\n * %s\n */\n", 
	  DSTOOL_TITLE, get_user_info(), get_the_time());
  fprintf(fp, "#include <model_headers.h>\n\n");

  /* write out definition with translation table  */
  fprintf(fp, "/*\n");
  parser_write_ds_def(ds, fp);
  parser_write_aux_fn(ds, fp);
  if (n_temp>0) fprintf(fp,"\n   where\n");
  parser_write_temp(ds,fp);
  fprintf(fp,"\n\n\tTranslation table:\n\n");
  for (i=0; i<n_varb; i++)
    fprintf(fp, "\t\tx[%d] <--> %s\n", i, parser_get_var_name(ds, i));
  for (i=0; i<n_param; i++)
    fprintf(fp, "\t\tp[%d] <--> %s\n", i, parser_get_par_name(ds, i));
  for (i=0; i<n_temp; i++)
    fprintf(fp, "\t\tTEMP[%d] <--> %s\n", i, parser_get_temp_name(ds, i));
  fprintf(fp, "*/\n\n");

  fprintf(fp, "/* function used to define dynamical system */\n");
  fprintf(fp, "int %s_ds_func(f,x,p)\ndouble *f,*x,*p;\n{\n\n", name);
  parser_writeC_ds_def(ds, fp);
  fprintf(fp, "\n\treturn;\n}\n\n");

  /* write out auxiliary functions */
  fprintf(fp, "/* function used to define aux functions */\n");
  if (n_funct == 0) fprintf(fp,"/*\n");
  fprintf(fp, "int %s_aux_func(f,x,p)\ndouble *f,*x,*p;\n{\n", name);
  parser_writeC_aux_fn(ds, fp);
  fprintf(fp, "\n\treturn;\n}\n");
  if (n_funct == 0) fprintf(fp,"*/\n");

  /* write out jacobian stub */
  fprintf(fp, "\n/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.\n");
  fprintf(fp, "\tinput explicit jacobian in the  form\n");
  fprintf(fp, "\tm[i][j] = d f_i / d x_j; (starting with 0)\n");
  fprintf(fp, "*/\n/*\nint %s_jac(m,x,p)\ndouble **m,*x,*p;\n{\n", name);
  fprintf(fp, "\n\treturn;\n}\n*/\n\n");

  /* write out init routine */
  fprintf(fp, "/* function used to define default data */\n");
  fprintf(fp, "int %s_init()\n{\n", name);

  fprintf(fp, "\tint n_varb=%d;\n",n_varb);
  fprintf(fp, "\tstatic char *variable_names[]={\"%s\"", parser_get_var_name(ds,0));
  for (i=1; i<n_varb; i++) fprintf(fp, ",\"%s\"", parser_get_var_name(ds,i));
  defaultd = ds_type ? VAR_IC_MAP : VAR_IC_VF;
  parser_get_var_ic(ds,0,&defaultd);
  fprintf(fp, "};\n\tstatic double variables[]={%lg", defaultd);
  for (i=1; i<n_varb; i++)
    {
      defaultd = ds_type ? VAR_IC_MAP : VAR_IC_VF;
      parser_get_var_ic(ds, i, &defaultd);
      fprintf(fp, ",%lg",defaultd);
    }
  defaultd = ds_type ? VAR_MIN_MAP : VAR_MIN_VF;
  parser_get_var_range(ds,0,&defaultd, &temp);
  fprintf(fp, "};\n\tstatic double variable_min[]={%lg", defaultd);
  for (i=1; i<n_varb; i++)
    {
      defaultd = ds_type ? VAR_MIN_MAP : VAR_MIN_VF;
      parser_get_var_range(ds, i, &defaultd, &temp);
      fprintf(fp, ",%lg",defaultd);
    }
  defaultd = ds_type ? VAR_MAX_MAP : VAR_MAX_VF;
  parser_get_var_range(ds,0, &temp, &defaultd);
  fprintf(fp, "};\n\tstatic double variable_max[]={%lg", defaultd);
  for (i=1; i<n_varb; i++)
    {
      defaultd = ds_type ? VAR_MAX_MAP : VAR_MAX_VF;
      parser_get_var_range(ds, i, &temp, &defaultd);
      fprintf(fp, ",%lg",defaultd);
    }
  fprintf(fp,"};\n\n");

  fprintf(fp, "\tstatic char *indep_varb_name=\"%s\";\n", ds_type ? INDEP_MAP : INDEP_VF);
  fprintf(fp, "\tdouble indep_varb_min=%lg;\n", ds_type ? INDEP_MIN_MAP : INDEP_MIN_VF);
  fprintf(fp, "\tdouble indep_varb_max=%lg;\n\n", ds_type ? INDEP_MAX_MAP : INDEP_MAX_VF);

  fprintf(fp, "\tint n_param=%d;\n",n_param);
  fprintf(fp, "\tstatic char *parameter_names[]={\"%s\"", parser_get_par_name(ds,0));
  for (i=1; i<n_param; i++) fprintf(fp, ",\"%s\"", parser_get_par_name(ds,i));
  defaultd = ds_type ? PAR_IC_MAP : PAR_IC_VF;
  parser_get_param_ic(ds,0,&defaultd);
  fprintf(fp, "};\n\tstatic double parameters[]={%lg", defaultd);
  for (i=1; i<n_param; i++)
    {
      defaultd = ds_type ? PAR_IC_MAP : PAR_IC_VF;
      parser_get_param_ic(ds, i, &defaultd);
      fprintf(fp, ",%lg",defaultd);
    }
  defaultd = ds_type ? PAR_MIN_MAP : PAR_MIN_VF;
  parser_get_param_range(ds,0,&defaultd, &temp);
  fprintf(fp, "};\n\tstatic double parameter_min[]={%lg", defaultd);
  for (i=1; i<n_param; i++)
    {
      defaultd = ds_type ? PAR_MIN_MAP : PAR_MIN_VF;
      parser_get_param_range(ds, i, &defaultd, &temp);
      fprintf(fp, ",%lg",defaultd);
    }
  defaultd = ds_type ? PAR_MAX_MAP : PAR_MAX_VF;
  parser_get_param_range(ds,0, &temp, &defaultd);
  fprintf(fp, "};\n\tstatic double parameter_max[]={%lg", defaultd);
  for (i=1; i<n_param; i++)
    {
      defaultd = ds_type ? PAR_MAX_MAP : PAR_MAX_VF;
      parser_get_param_range(ds, i, &temp, &defaultd);
      fprintf(fp, ",%lg",defaultd);
    }
  fprintf(fp,"};\n\n");
  
  fprintf(fp, "\tint n_funct=%d;\n",n_funct);
  fprintf(fp, "\tstatic char *funct_names[]={\"%s\"", parser_get_aux_fn_name(ds,0));
  for (i=1; i<n_funct; i++) fprintf(fp, ",\"%s\"", parser_get_var_name(ds,i));
  defaultd = ds_type ? FN_MIN_MAP : FN_MIN_VF;
  parser_get_aux_fn_range(ds,0,&defaultd, &temp);
  fprintf(fp, "};\n\tstatic double funct_min[]={%lg", defaultd);
  for (i=1; i<n_funct; i++)
    {
      defaultd = ds_type ? FN_MIN_MAP : FN_MIN_VF;
      parser_get_aux_fn_range(ds, i, &defaultd, &temp);
      fprintf(fp, ",%lg",defaultd);
    }
  defaultd = ds_type ? FN_MAX_MAP : FN_MAX_VF;
  parser_get_aux_fn_range(ds,0, &temp, &defaultd);
  fprintf(fp, "};\n\tstatic double funct_max[]={%lg", defaultd);
  for (i=1; i<n_funct; i++)
    {
      defaultd = ds_type ? FN_MAX_MAP : FN_MAX_VF;
      parser_get_aux_fn_range(ds, i, &temp, &defaultd);
      fprintf(fp, ",%lg",defaultd);
    }
  fprintf(fp,"};\n\n");

  fprintf(fp, "\tint manifold_type=%s;\n", ((manifold_type==EUCLIDEAN) ? "EUCLIDEAN" : "PERIODIC"));
  flag = parser_get_var_periodic(ds, 0, &defaultd, &temp);
  fprintf(fp, "\tstatic int periodic_varb[]={%s", ((flag==PARSER_OK) ? "TRUE" : "FALSE"));
  for (i=1; i<n_varb; i++)
    {
      flag = parser_get_var_periodic(ds, i, &defaultd, &temp);
      fprintf(fp, ",%s", ((flag==PARSER_OK) ? "TRUE" : "FALSE"));
    }
  flag = parser_get_var_periodic(ds, 0, &defaultd, &temp);
  fprintf(fp, "};\n\tstatic double period_start[]={%lg", ((flag==PARSER_OK) ? defaultd : 0.0));
  for (i=1; i<n_varb; i++)
    {
      flag = parser_get_var_periodic(ds, i, &defaultd, &temp);
      fprintf(fp, ",%lg", ((flag==PARSER_OK) ? defaultd : 0.0));
    }
  flag = parser_get_var_periodic(ds, 0, &temp, &defaultd);
  fprintf(fp, "};\n\tstatic double period_end[]={%lg", ((flag==PARSER_OK) ? defaultd : 1.0));
  for (i=1; i<n_varb; i++)
    {
      flag = parser_get_var_periodic(ds, i, &temp, &defaultd);
      fprintf(fp, ",%lg", ((flag==PARSER_OK) ? defaultd : 0.0));
    }
  fprintf(fp,"};\n\n");

  fprintf(fp, "\tint mapping_toggle=%s;\n", (ds_type==0 ? "FALSE" : "TRUE"));
  fprintf(fp, "\tint inverse_toggle=FALSE;\n\n");

  fprintf(fp, "\tint (*def_name)()=%s_ds_func;\n", name);
  fprintf(fp, "\tint (*jac_name)()=NULL;\n");
  fprintf(fp, "\tint (*aux_func_name)()=");
  if (n_funct==0) fprintf(fp,"NULL;\n");
  else fprintf(fp,"%s_aux_func;\n",name);
  fprintf(fp, "\tint (*inv_name)()=NULL;\n");
  fprintf(fp, "\tint (*dfdt_name)()=NULL;\n");
  fprintf(fp, "\tint (*dfdparam_name)()=NULL;\n\n");

  fprintf(fp, "#include <ds_define.c>\n}\n\n");

  parser_destroy_ds(ds);
  return status;
}
