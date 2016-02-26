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
 * save_procs.c contains routines to save dstool configuration and memory objects to a data file 
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <math.h>
#include <constants.h>
#include <memory.h>
#include <pm.h>
#include <saveload.h>
#include <pm_hash.h>


/*
 * save_go()	writes data to file in format determined by option
 * Arguments: TRUE means overwrite file even if it exists; FALSE means ask user whether
 * 	to overwrite or cancel save.
 * Returns 0 = successful save; -1 = cannot open file
 *         1 = file exists: ask for overwrite permission. 
 */
int
save_go(force)
int	force;
{
  FILE          *fp,*fopen();
  char		*strcpy(),*strcat();
  int		status=0;
  char          *fname =  (char *) calloc(SIZE_OF_FNAME,sizeof(char)); 
  char          *dirname =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));

  dirname = strcat(strcat((char *)pm(GET, "Save.Directory", dirname, NULL), "/"), 
		   (char *)pm(GET, "Save.Filename", fname, NULL) );
  if (check_file_to_read(dirname) && !force )	/* file exists; ask user for permission */
    status = 1;
  else if (check_file_to_write(dirname))
    {
      fp = fopen(dirname,"w");
      status = save_to_file(fp);
      fclose(fp);
    }
  else status = -1;

  free(fname);
  free(dirname);
  return( status );
}

/*
 * save_to_file() writes data to file in format determined by options.  The main driver.
 */
int
save_to_file(fp)
FILE	*fp;
{
  char	 	*get_the_time(),*get_user_info(),*get_ds_name();

  fprintf(fp,"#  This data file is read and written by dstool\n#  Saved By: %s\n#  %s\n#\n", /* header */
	  get_user_info(), get_the_time());
  fprintf(fp,"pm PUT Load.Model_Name \"%s\"\npm EXEC Load.If_New_Model_Fnct\n\n", get_ds_name());

  if ( *((int *)pm(GET, "Save.Config", NULL)) && /* save configuration FIRST!*/
      *((int *) pm(GET, "Control.Mode", NULL)) == WINDOWS_MODE)
    save_config(fp);

  if ( *((int *)pm(GET, "Save.Settings", NULL)))  /* save the settings */
    save_settings(fp);

  if ( *((int *)pm(GET, "Save.Traj", NULL)) )	 /* save trajectories */
    {
      write_data_obj(fp,(memory) pm(GET, "Memory.Traj", NULL),"Traj");
      write_data_obj(fp,(memory) pm(GET, "Memory.Mult", NULL),"Mult");
    }

  if ( *((int *)pm(GET, "Save.Fixpt", NULL)) )	 /* save fixed points */
    write_data_obj(fp,(memory) pm(GET, "Memory.Fixed", NULL),"Fixed");

  if ( *((int *)pm(GET, "Save.Cont", NULL)) )	 /* save continuation */
    write_data_obj(fp,(memory) pm(GET, "Memory.Cont", NULL),"Cont");

  if ( *((int *)pm(GET, "Save.Param", NULL)) )	 /* save parameters */
    write_data_obj(fp,(memory) pm(GET, "Memory.Param", NULL),"Param");

  if ( *((int *)pm(GET, "Save.Select", NULL)) )	 /* save selected points */
    write_data_obj(fp,(memory) pm(GET, "Memory.Sel_Pt", NULL),"Sel_Pt");

  if ( *((int *)pm(GET, "Save.Funct", NULL)) )      /* save function */
    save_func(fp);
  return 0;
}

/*
 * save_to_file() writes data to file in format determined by options.  The main driver.
 */
int
save_to_file_1p1(fp)
FILE	*fp;
{
  char	 	*get_the_time(),*get_user_info(),*get_ds_name();

  fprintf(fp,"#  This data file is read and written by dstool\n#  Saved By: %s\n#  %s\n#\n", /* header */
	  get_user_info(), get_the_time());
  fprintf(fp,"pm PUT Load.Model_Name %s\npm EXEC Load.If_New_Model_Fnct\n\n", get_ds_name());

  if ( *((int *)pm(GET, "Save.Config", NULL)) && /* save configuration FIRST!*/
      *((int *) pm(GET, "Control.Mode", NULL)) == WINDOWS_MODE)
    save_config(fp);

  if ( *((int *)pm(GET, "Save.Settings", NULL)))  /* save the settings */
    save_settings(fp);

  if ( *((int *)pm(GET, "Save.Traj", NULL)) )	 /* save trajectories */
    {
      write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Traj", NULL),"New_Traj");
      write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Mult", NULL),"New_Mult");
    }

  if ( *((int *)pm(GET, "Save.Fixpt", NULL)) )	 /* save fixed points */
    write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Fixed", NULL),"New_Fxpt");

  if ( *((int *)pm(GET, "Save.Cont", NULL)) )	 /* save continuation */
    write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Cont", NULL),"New_Cont");

  if ( *((int *)pm(GET, "Save.Param", NULL)) )	 /* save parameters */
    write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Param", NULL),"New_Param");

  if ( *((int *)pm(GET, "Save.Select", NULL)) )	 /* save selected points */
    write_data_obj_1p1(fp,(memory) pm(GET, "Memory.Sel_Pt", NULL),"New_Sel_Pt");

  if ( *((int *)pm(GET, "Save.Funct", NULL)) )      /* save function */
    save_func(fp);
  return 0;
}




/* 
 save_settings()   saves window settings to specified file
*/
int
    save_settings(fp)
FILE		
    *fp;		
{
    struct pm_list   
	*current, /* pointer to an entry in the hash table; other than for
		     traversal, utilized only when category is PM_OBJECT */
	*elem_ptr; /* pointer to an entry of category PM_ELEMENT */
    int              
	i, status = NO_ERROR;

    for( i=0; i<HASHSIZE; i++) {  /* loop through all the hashtable elements */
	current = hashtab[i];	  /*   get addr stored in this element       */
	while( current != NULL) { /* dump any pm_object data headed by this node */
	    if(( current->category == PM_OBJECT) &&
	       (current->savable == SAVE_SETTINGS)) {
		fprintf(fp,"\n# %s Object:\n",current->label);
		elem_ptr = current->next_elem;
		while( elem_ptr != NULL) { /* traverse the list of elements in this group */
		    if (elem_ptr->savable == SAVE_SETTINGS)
			write_pm_obj(fp,elem_ptr);
		    elem_ptr = elem_ptr->next_elem;
		}
	    }
	    current = current->next_in_hash;
	}
    }
    return NO_ERROR;
}

int
    write_pm_obj(fp,elem_ptr)
FILE
    *fp;
struct pm_list
    *elem_ptr;
{
    int
	format = SAVE_PRECISION,	/* for now, constant precision */ 
	i;

    switch (elem_ptr->type) {
    case INT:
	fprintf(fp,"pm PUT %s %d\n",elem_ptr->label,elem_ptr->data.int_data);
	break;
    case INT_LIST:
	if (elem_ptr->list_size > 0) {
	    fprintf(fp,"pm PUT_LIST %s %d %d",elem_ptr->label,0,elem_ptr->list_size-1);
	    for (i=0; i < elem_ptr->list_size; i++)
		fprintf(fp," %d",elem_ptr->data.int_list_data[i]);
	    fprintf(fp,"\n");
	}
	break;
    case DBL:
	fprintf(fp,"pm PUT %s %.*lg\n",elem_ptr->label,format,elem_ptr->data.double_data);
	break;
    case DBL_LIST:
	if (elem_ptr->list_size > 0) {
	    fprintf(fp,"pm PUT_LIST %s %d %d",elem_ptr->label,0,elem_ptr->list_size-1);
	    for (i=0; i < elem_ptr->list_size; i++)
		fprintf(fp," %.*lg",format,elem_ptr->data.double_list_data[i]);
	    fprintf(fp,"\n");
	}
	    break;
    case STRNG:
	if (strcmp(elem_ptr->data.string_data,"\0"))
	    fprintf(fp,"pm PUT %s \"%s\"\n",elem_ptr->label,elem_ptr->data.string_data);
	break;
    case STRNG_LIST:
/*
	if (elem_ptr->list_size > 0) {
	    fprintf(fp,"pm PUT_LIST %s %d %d",elem_ptr->label,0,elem_ptr->list_size-1);
	    for (i=0; i < elem_ptr->list_size; i++)
		fprintf(fp," %s",elem_ptr->data.string_list_data[i]);
	    fprintf(fp,"\n");

	}
*/
	break;
    case ADDRS:
    case MEMRY:
    case FNCT:
    default:
	break;
    }
}

/* 
 save_settings_1p1()   saves window settings to specified file
 in the format of dstool 1.1.
*/
save_settings_1p1(fp)
FILE		*fp;		
{
  int		v_dim,p_dim,f_dim,format, i;
  char		*get_ds_name();
  double        *x,*dvector();
  int           *k,*ivector();

  get_n_all_types(&v_dim, &p_dim, &f_dim);
  x = dvector(0, v_dim+p_dim+f_dim-1);
  k = ivector(0, v_dim+p_dim+f_dim-1);

/*  format = *((int *) pm( GET, "Defaults.Precision", NULL )); */
    format = SAVE_PRECISION;	/* for now, constant precision */
	   
  fprintf(fp,"# Varb_Dim     %d\n# Param_Dim    %d\n# Varb_Ic      ", v_dim,  p_dim);
  pm(GET_LIST, "Selected.Varb_Ic", 0, v_dim-1, x, NULL);
  for (i=0; i<v_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Varb_Fc      ");
  pm(GET_LIST, "Selected.Varb_Fc", 0, v_dim-1, x, NULL);
  for (i=0; i<v_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Varb_Min     ");
  pm(GET_LIST, "Defaults.Varb_Min", 0, v_dim-1, x, NULL);
  for (i=0; i<v_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Varb_Max     ");
  pm(GET_LIST, "Defaults.Varb_Max", 0, v_dim-1, x, NULL);
  for (i=0; i<v_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);

  fprintf(fp,"\n# Param        ");
  pm(GET_LIST, "Selected.Param_Ic", 0, p_dim-1, x, NULL);
  for (i=0; i<p_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Param_Min    ");
  pm(GET_LIST, "Defaults.Param_Min", 0, p_dim-1, x, NULL);
  for (i=0; i<p_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Param_Max    ");
  pm(GET_LIST, "Defaults.Param_Max", 0, p_dim-1, x, NULL);
  for (i=0; i<p_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);

  fprintf(fp,"\n# Function_Dim %d", f_dim);
  fprintf(fp,"\n# Function_Min ");
  pm(GET_LIST, "Defaults.Funct_Min", 0, f_dim-1, x, NULL);
  for (i=0; i<f_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp,"\n# Function_Max ");
  pm(GET_LIST, "Defaults.Funct_Max", 0, f_dim-1, x, NULL);
  for (i=0; i<f_dim; i++)    fprintf(fp,"%.*lg ", format,  x[i]);
  
  fprintf(fp,"\n# Vf_Period    %.*lg\n# Map_Period   %d\n# Algorithm    %d\n# Guess        %d\n# Guess_Num    %d\n# Setting      %d",
	  format, *((double *) pm(GET, "Fixed.Vf_Period", NULL)),
	  *((int *)    pm(GET, "Fixed.Map_Period", NULL)),
	  *((int *)    pm(GET, "Fixed.Algorithm", NULL)),
	  *((int *)    pm(GET, "Fixed.Guess", NULL)),
	  *((int *)    pm(GET, "Fixed.MC_Guesses", NULL)),
	  *((int *)    pm(GET, "Fixed.Setting", NULL))
	  );
  fprintf(fp,"\n# Num_Iters    %d\n# Dups         %.*lg\n# Var_Conv     %.*lg\n# Funct_Conv   %.*lg\n# FD_Step      %.*lg",
	  *((int *)    pm(GET, "Fixed.Num_Iters", NULL)),
	  format, *((double *) pm(GET, "Fixed.Dups", NULL)),
	  format, *((double *) pm(GET, "Fixed.Var_Conv", NULL)),
	  format, *((double *) pm(GET, "Fixed.Funct_Conv", NULL)),
	  format, *((double *) pm(GET, "Fixed.FD_Step", NULL))
	  );
  fprintf(fp,"\n# Eigen_Dist   %.*lg\n# Stab_Points  %d\n# Stab_Steps   %d\n# Unstab_Points %d\n# Unstab_Steps %d",
	  format, *((double *) pm(GET, "Fixed.Eigen_Dist", NULL)),
	  *((int *) pm(GET, "Fixed.Stab_Points", NULL)),
	  *((int *) pm(GET, "Fixed.Stab_Steps", NULL)),
	  *((int *) pm(GET, "Fixed.Unstab_Points", NULL)),
	  *((int *) pm(GET, "Fixed.Unstab_Steps", NULL))
	  );

  fprintf(fp,"\n# Start_Save_Points  %d\n# Total_Iterates     %d\n# Skip_Size          %d\n# Stepsize           %.*lg",
	  *((int *)    pm(GET, "Flow.Start_Save_Points", NULL)),
	  *((int *)    pm(GET, "Flow.Total_Iterates", NULL)),
	  *((int *)    pm(GET, "Flow.Skip_Size", NULL)),
	  format, *((double *) pm(GET, "Flow.Stepsize", NULL))
	  );
  fprintf(fp,"\n# Stopping_Condition %d\n# Diverg_Cutoff      %.*lg\n# Final_Time         %.*lg",
	  *((int *)    pm(GET, "Flow.Stopping_Condition", NULL)),
	  format, *((double *) pm(GET, "Flow.Diverg_Cutoff", NULL)),
	  format, *((double *) pm(GET, "Flow.Final_Time", NULL))
	  );
  fprintf(fp, "\n# Varb_Events        ");
  pm( GET_LIST, "Flow.Varb_Events", 0, v_dim-1, k, NULL);
  for (i=0; i<v_dim; i++)    
    fprintf(fp,"%d ",  k[i]);
  fprintf(fp, "\n# Funct_Events       ");
  pm( GET_LIST, "Flow.Funct_Events", 0, f_dim-1, k, NULL);
  for (i=0; i<f_dim; i++)    
    fprintf(fp,"%d ",  k[i]);
  fprintf(fp, "\n# Varb_Event_Values  ");
  pm( GET_LIST, "Flow.Varb_Event_Values", 0, v_dim-1, x, NULL);
  for (i=0; i<v_dim; i++)    
    fprintf(fp,"%.*lg ", format,  x[i]);
  fprintf(fp, "\n# Funct_Event_Values ");
  pm( GET_LIST, "Flow.Funct_Event_Values", 0, f_dim-1, x, NULL);
  for (i=0; i<f_dim; i++)    
    fprintf(fp,"%.*lg ",  format, x[i]);
  
  fprintf(fp,"\n# Disp_Points      %d\n# Clipping         %d\n# Recording        %d\n# Def_Symbol_Index %d\n# Def_Size_Index   %d\n# Precision        %d",
	  *((int *)    pm(GET, "Defaults.Disp_Points", NULL)),
	  *((int *)    pm(GET, "Defaults.Clipping", NULL)),
	  *((int *)    pm(GET, "Defaults.Recording", NULL)),
	  *((int *)    pm(GET, "Defaults.Def_Symbol_Index", NULL)),
	  *((int *)    pm(GET, "Defaults.Def_Size_Index", NULL)),
	  *((int *)    pm(GET, "Defaults.Precision", NULL))
	  );  

  fprintf(fp,"\n# Mult_Load_Choice    %d\n# Mult_Transformation %d\n# Images              %d\n# Mult_Trans_Param    %.*lg",
	  *((int *)    pm(GET, "Mult.Mult_Load_Choice", NULL)),
	  *((int *)    pm(GET, "Mult.Mult_Transformation", NULL)),
	  *((int *)    pm(GET, "Mult.Images", NULL)),
	  format, *((double *) pm(GET, "Mult.Mult_Trans_Param", NULL))
	  );  
  fprintf(fp, "\n# Mult_Points         ");
  pm( GET_LIST, "Mult.Mult_Points", 0, v_dim+p_dim-1, k, NULL);
  for (i=0; i<v_dim+p_dim; i++)    
    fprintf(fp,"%d ", k[i]);
  fprintf(fp, "\n# Mult_Radius         ");
  pm( GET_LIST, "Mult.Mult_Radius", 0, v_dim+p_dim-1, x, NULL);
  for (i=0; i<v_dim+p_dim; i++)    
    fprintf(fp,"%.*lg ",  format, x[i]);  


  fprintf(fp, "\n");
  free_dvector(x, 0, v_dim+p_dim+f_dim-1);
  free_ivector(k, 0, v_dim+p_dim+f_dim-1);
}




/*
save_func()   saves function values to data file
*/
save_func(fp)
FILE	*fp;
{
  double		*pt, *dvector();
  int			get_ds_func();
  memory          	mem;
  int			v_dim = *( (int *)pm( GET, "Model.Varb_Dim", NULL) ); 
  int			p_dim = *( (int *)pm( GET, "Model.Param_Dim", NULL) ); 
  int			f_dim = *( (int *)pm( GET, "Model.Funct_Dim", NULL) ); 

  if( f_dim <=0 ) return( 1 );

  pt = dvector(0,v_dim+p_dim+f_dim-1);	/* allocate temp space for impt quantities */ 

  mem = (memory) pm(GET, "Memory.Traj", NULL);
  write_func(fp, mem, "Trajectory", v_dim, p_dim, f_dim, pt);

  mem = (memory) pm(GET, "Memory.Mult", NULL);
  write_func(fp, mem, "Multiple", v_dim, p_dim, f_dim, pt);

  mem = (memory) pm(GET, "Memory.Fixed", NULL);
  write_func(fp, mem, "Fixed Point", v_dim, p_dim, f_dim, pt);

  mem = (memory) pm(GET, "Memory.Cont", NULL);
  write_func(fp, mem, "Continuation", v_dim, p_dim, f_dim, pt);

  mem = (memory) pm(GET, "Memory.Sel_Pt", NULL);
  write_func(fp, mem, "Selected Point", v_dim, p_dim, f_dim, pt);

  free_dvector( pt, 0, v_dim+p_dim+f_dim-1 );
}


write_func(fp, mem, label, v_dim, p_dim, f_dim, pt)
FILE     *fp;
char     *label;
memory   mem;
int      v_dim, p_dim, f_dim;
double   *pt;
{
  int 	(*aux_f)();
  double *varb, *param;
  int    total = v_dim + p_dim + f_dim;
  int    format;

/*  aux_f = (void *) pm( GET, "Model.Aux_Function", NULL );*/
  aux_f =  (int (*)()) pm( GET, "Model.Aux_Function", NULL );
  if ( aux_f == NULL ) return( 1 );

  format = SAVE_PRECISION;  
  if (memory_reset_read(mem) == 0) 
    while (memory_read_next_flow(mem, NULL, NULL, NULL, NULL, NULL) == 0)
      {
	  fprintf(fp,"pm PUT Cur_Memory.Fcn_Data_Type %s\n# Varb_Dim %d\n# Param_Dim %d\n# Function_Dim %d\nSET func_object {\n", 
		  label, v_dim, p_dim, f_dim);
	while (memory_read_next_traj(mem, NULL, NULL, NULL) == 0)
	  {
	    fprintf(fp,"{\n");	/* open bracket for trajectory */
	    while (memory_read_next_point(mem, &varb, &param, NULL, NULL, NULL) == 0)
	      {
		dcopy( v_dim, varb, 1, pt, 1 );               /* copy varb into beginning of pt */
		dcopy( p_dim, param, 1, &pt[v_dim], 1 );      /* copy param into middle of pt */
		aux_f( &pt[v_dim+p_dim], varb, param );       /* function values at end of pt */
		write_double_point( fp, total, format, pt );
	      }
	    fprintf(fp,"}\n"); /* write closing bracket for traj */
	  }
	  fprintf(fp,"}\n\n");	/* write closing bracket for flow */
      }
}

write_func_1p1(fp, mem, label, v_dim, p_dim, f_dim, pt)
FILE     *fp;
char     *label;
memory   mem;
int      v_dim, p_dim, f_dim;
double   *pt;
{
  int 	(*aux_f)();
  double *varb, *param;
  int    total = v_dim + p_dim + f_dim;
  int    format;

/*  aux_f = (void *) pm( GET, "Model.Aux_Function", NULL );*/
  aux_f =  (int (*)()) pm( GET, "Model.Aux_Function", NULL );
  if ( aux_f == NULL ) return( 1 );

  format = SAVE_PRECISION;  
  if (memory_reset_read(mem) == 0) 
    while (memory_read_next_flow(mem, NULL, NULL, NULL, NULL, NULL) == 0)
      {
	while (memory_read_next_traj(mem, NULL, NULL, NULL) == 0)
	  {
	    fprintf(fp,"# New_Func for memory object: %s\n# Varb_Dim %d\n# Param_Dim %d\n# Function_Dim %d\n", 
		    label, v_dim, p_dim, f_dim);
	    while (memory_read_next_point(mem, &varb, &param, NULL, NULL, NULL) == 0)
	      {
		dcopy( v_dim, varb, 1, pt, 1 );               /* copy varb into beginning of pt */
		dcopy( p_dim, param, 1, &pt[v_dim], 1 );      /* copy param into middle of pt */
		aux_f( &pt[v_dim+p_dim], varb, param );       /* function values at end of pt */
		write_double_point_1p1( fp, total, format, pt );
	      } 
	  }
      }
}

/*
write_data_obj() writes to file the contents of a memory data object.
The objects handled are trajectories, fixed points, continuation data, parameter data,
and selected points.
*/
int
  write_data_obj(fp, ptr, label)
FILE		*fp;
memory		ptr;
char		*label;
{
  int	  	i, n_doubles, n_ints, n_dparams, n_iparams, n_trajs, n_points;
  int           *p_ints, *p_iparams;
  double       	*p_doubles, *p_dparams;
  int	        format;
  int           first_obj;

  /* format  = *((int *) pm( GET, "Defaults.Precision", NULL )); */
  format = SAVE_PRECISION;					 /* for now, constant precision */
  
  /* note if no memory obj exists of requested form, then control falls through this loop */
  if (memory_reset_read(ptr) == 0) 
      while ( memory_vanilla_read_next_flow(ptr, &n_trajs, &p_dparams, &n_dparams, &p_iparams, &n_iparams) == 0) {
	/*	fprintf(fp,"# %s\n# Objects %d\n# Double_Params %d : ",label,n_trajs,n_dparams);*/
	  fprintf(fp,"pm PUT Cur_Memory.Mem_Type %s\n",label);
	  fprintf(fp,"pm PUT Cur_Memory.Num_Objects %d\n",n_trajs);
	  fprintf(fp,"pm PUT Cur_Memory.Num_Header_Doubles %d\n", n_dparams);
	  fprintf(fp,"pm PUT Cur_Memory.Num_Header_Integers %d\n", n_iparams);
	  if (n_dparams > 0) {
	      fprintf(fp,"pm PUT_LIST Cur_Memory.Header_Doubles %d %d",0,n_dparams -1);
	      for (i=0; i < n_iparams; i++)
		  fprintf(fp," %.*lg",format,p_dparams[i]);
	      fprintf(fp,"\n");
	  }
	if (n_iparams > 0) {
	    fprintf(fp,"pm PUT_LIST Cur_Memory.Header_Integers %d %d",0,n_iparams -1);
	    for (i=0; i < n_iparams; i++)
		fprintf(fp," %d",p_iparams[i]);
	    fprintf(fp,"\n");
	}
	  first_obj = TRUE;
	  while ( memory_vanilla_read_next_traj(ptr, &n_points, &n_doubles, &n_ints) == 0 ) {
	      if (first_obj) { 
/*		  fprintf(fp,"# Doubles= %d Integers= %d\n",n_doubles,n_ints);*/
		  fprintf(fp,"pm PUT Cur_Memory.Num_Body_Doubles %d\n", n_doubles);
		  fprintf(fp,"pm PUT Cur_Memory.Num_Body_Integers %d\n", n_ints);
/*		  fprintf(fp,"# pm PUT Cur_Memory.Object_Num_Points %d\n",n_points); */
		  fprintf(fp,"SET new_object {\n");
		  first_obj = FALSE;
	      }
/*	      fprintf(fp,"# pm PUT Cur_Memory.Object_Num_Points %d\n",n_points);*/
/*	      fprintf(fp,"SET new_object {\n"); */
	      fprintf(fp,"{\n"); /* open trajectory bracket */
	      while ( memory_vanilla_read_next_point(ptr, &p_doubles, &p_ints) == 0) {
		  write_double_point(fp, n_doubles, format, p_doubles);
		  write_integer_point(fp, n_ints, p_ints);
	      }
	      fprintf(fp,"}\n");/* close trajectory bracket */
	  }
	  if (first_obj == FALSE) /* means there were some trajectories */
	      fprintf(fp,"}\n\n"); /* close off new_object bracket */
      }		  
}

/*
write_data_obj() writes to file the contents of a memory data object.
The objects handled are trajectories, fixed points, continuation data, parameter data,
and selected points.
*/
int
  write_data_obj_1p1(fp, ptr, label)
FILE		*fp;
memory		ptr;
char		*label;
{
  int	  	n_doubles, n_ints, n_dparams, n_iparams, n_trajs, n_points;
  int           *p_ints, *p_iparams;
  double       	*p_doubles, *p_dparams;
  int	        format;
  int           first_obj;

  /* format  = *((int *) pm( GET, "Defaults.Precision", NULL )); */
  format = SAVE_PRECISION;					 /* for now, constant precision */
  
  /* note if no memory obj exists of requested form, then control falls through this loop */
  if (memory_reset_read(ptr) == 0) 
    while ( memory_vanilla_read_next_flow(ptr, &n_trajs, &p_dparams, &n_dparams, &p_iparams, &n_iparams) == 0)
      {
	fprintf(fp,"# %s\n# Objects %d\n# Double_Params %d : ",label,n_trajs,n_dparams);
	if (n_dparams > 0) 
	  write_double_point_1p1(fp, n_dparams, format, p_dparams);
	else
	  fprintf(fp,"\n");
	fprintf(fp,"# Integer_Params %d : ",n_iparams);
	if (n_iparams > 0)
	  write_integer_point_1p1(fp, n_iparams, p_iparams);
	else
	  fprintf(fp,"\n");
	first_obj = TRUE;
	while ( memory_vanilla_read_next_traj(ptr, &n_points, &n_doubles, &n_ints) == 0 )
	  {
	    if (first_obj) 
	      { 
		fprintf(fp,"# Doubles= %d Integers= %d\n",n_doubles,n_ints);
		first_obj = FALSE;
	      }
	    fprintf(fp,"# New_Obj:  %d Points\n",n_points);
	    while ( memory_vanilla_read_next_point(ptr, &p_doubles, &p_ints) == 0)
	      {
		write_double_point_1p1(fp, n_doubles, format, p_doubles);
		write_integer_point_1p1(fp, n_ints, p_ints);
	      }
	  }
      }		  
}


