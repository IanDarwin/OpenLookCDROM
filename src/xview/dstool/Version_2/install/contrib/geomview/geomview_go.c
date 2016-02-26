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

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif


#include <xview/xview.h>

#include <pm.h>
#include <constants.h>
#include <memory.h>
#include <filters.h>
#include <twoD.h>
#include "geomview_cui.h"
#include "geomview.h"

extern geomview_window1_objects 
	*geomview_ip;

int
geomview_view_go()

{
    FILE
      *dest_fp;
    int 
      get_display_type(),
      valid_twoD_id(),
      *ivector(),
      number_sys_colors,
      number_traj_colors,
      window_number,
      n_colors,
      *red_table,
      *green_table,
      *blue_table,
      file_read_status,
      cancel_status,
      file_write_status;
    char 
      *pathname =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char)),
      *strng =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));

    window_number = *((int *) pm( GET, "Geomview.Window_Number", NULL ));
    if (!valid_twoD_id(window_number)) {
      fprintf(stderr,"Invalid window number %d.\n",window_number);
      return(MAJOR_ERROR);
    }
    
    if(get_display_type(window_number) != TRUE ) {
      fprintf(stderr,"Sorry, the geomview panel requires a color display.\n");
      return(MAJOR_ERROR);
    }

    number_sys_colors = *((int *) pm( GET,  "Color.Sys_Colormap_Size", NULL));
    number_traj_colors = *((int *) pm( GET,  "Color.Traj_Colormap_Size", NULL));
    n_colors = number_sys_colors + number_traj_colors;

    red_table = ivector(0,n_colors-1);
    pm(GET_LIST, "Color.Red_Table", 0, n_colors-1, red_table, NULL);
    
    green_table = ivector(0,n_colors-1);
    pm(GET_LIST, "Color.Green_Table", 0, n_colors-1, green_table, NULL);
    
    blue_table = ivector(0,n_colors-1);
    pm(GET_LIST, "Color.Blue_Table", 0, n_colors-1, blue_table, NULL);

    fprintf(stderr,"Sending to geomview:\n");
    if ((*(int *) pm(GET,"Geomview.Destination",NULL)) == GEOMVIEW)
      dest_fp = stdout;
    else if ((*(int *) pm(GET,"Geomview.Destination",NULL)) == TOFILE) {
      pm(GET, "Geomview.Directory", strng, NULL);
      strcpy(pathname,strng);
      pm(GET, "Geomview.Filename", strng, NULL);
      strcat(strcat(pathname, "/"), strng );
      file_read_status = check_file_to_read(pathname);
      if (file_read_status) { 
	/* file exists */
	cancel_status = error_notice_option
	  (geomview_ip->controls1, "Specified files exists.", 
	   "Cancel Save", "Overwrite File");
	if (cancel_status)
	  return MINOR_ERROR;	 
      }

      file_write_status = check_file_to_write(pathname);
      if (!file_write_status) {
	error_notice(geomview_ip->controls1, "Saving not successful.  Cannot open file");
	return MINOR_ERROR;	 
      }
      else 
	dest_fp = fopen(pathname,"w");
    }
    geomview_using_window
      (dest_fp,window_number,n_colors,number_sys_colors,
       red_table,green_table,blue_table);
    free(pathname);
    free(strng);
    
  }


/* Display all memory objects valid for window_number. */
int
	geomview_using_window(dest_fp,window_num, n_colors, n_sys_colors, red_table, green_table, blue_table)

FILE
	*dest_fp;
int	
	window_num,
	n_colors,
	n_sys_colors,
	*red_table,
	*green_table,
	*blue_table;

{
    extern int valid_twoD_id();
    memory	mem_obj_ptr;

    if(!valid_twoD_id(window_num)) return(-1);

    if( mem_obj_ptr = (memory) pm(GET, "Memory.Traj",  NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors, n_sys_colors, red_table, green_table, blue_table,0 );
    if( mem_obj_ptr = (memory) pm(GET, "Memory.Mult", NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors, n_sys_colors, red_table, green_table, blue_table,1 );
    if( mem_obj_ptr = (memory) pm(GET, "Memory.Fixed", NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors, n_sys_colors,red_table, green_table, blue_table,2 );
/*    if( mem_obj_ptr = (memory) pm(GET, "Memory.Cont", NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors,n_sys_colors,red_table, green_table, blue_table,3 ); */
    if( mem_obj_ptr = (memory) pm(GET, "Memory.Sel_Pt", NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors, n_sys_colors,red_table, green_table, blue_table,4 );
    if( mem_obj_ptr = (memory) pm(GET, "Memory.Param", NULL) )
	mem_obj_to_geomview(dest_fp, mem_obj_ptr, window_num, n_colors,n_sys_colors,red_table, green_table, blue_table,5 );
    if ( *((int *) pm( GET, "Geomview.Destination", NULL )) == TOFILE)
      fclose(dest_fp);
    return(NO_ERROR);
  }


struct  Filter_DataS     filter_cntl;

/*  Display memory object in window number window_number. */
int
	mem_obj_to_geomview(dest_fp, mem_obj, window_number, n_colors, n_sys_colors, red_table, green_table, blue_table,tag)
memory	
	mem_obj;
FILE
	*dest_fp;
int	
	window_number,
	n_colors,
	n_sys_colors,
	*red_table,
	*green_table,
	*blue_table,
	tag;
{
    int 
	valid_twoD_id(),
	get_color(),
	color_index,
	seg_index,
	memory_stored_points(),
	i,num_points=0,
	reset_win_dim();
    double 
	x, y, z;
    double 
	*points, *params,*func_values, *dvector();
    int 
	mem_obj_type = memory_get_type( mem_obj );
    int 
	func_dim, *color;
    static char	*
	tag_names[]= {"traj_mem","mult_mem","fixed_mem","sel_mem","param_mem"};
/*	tag_names[]= {"traj_mem","mult_mem","fixed_mem","cont-mem","sel_mem","param_mem"};*/

    if ( !valid_twoD_id( window_number ) || !valid_mem_to_win( mem_obj_type, window_number) ) return(-1);

    filter_cntl.window_id = window_number;
    reset_win_dim(window_number);

    func_dim = *((int *)pm(GET,"Model.Funct_Dim",NULL));
    func_values = dvector(0,func_dim-1);

    if (memory_reset_read(mem_obj) == 0) {
	if ((num_points = memory_stored_points(mem_obj)) > 0) {
	    fprintf(dest_fp,"(geometry %s {\n" , tag_names[tag]);
	    fprintf(dest_fp,"VECT\n%d %d %d\n",num_points,num_points,num_points);
	    for (i=0; i < num_points; i++) {
		fprintf(dest_fp,"1 1\n");
	    }
	}
	while (memory_read_next_flow(mem_obj, NULL, NULL, NULL, NULL, NULL) == 0) {
	    while (memory_read_next_traj(mem_obj, NULL, NULL, NULL) == 0) {
		while (memory_read_next_point(mem_obj, &points, &params, &color, 
					      NULL, NULL) == 0) {
		    switch (*((int *) pm(GET, "Geomview.Active_Hor_Type",NULL))) {
		    case PHASE_SPACE_VARB:
			x = points[*((int *) pm(GET, "Geomview.Active_Hor_Index",NULL))];
			break;
		    case PARAMETER_VARB:
			x = params[*((int *) pm(GET, "Geomview.Active_Hor_Index",NULL))];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			x = func_values[*((int *) pm(GET, "Geomview.Active_Hor_Index",NULL))];
			break;
		    }

		    switch (*((int *) pm(GET, "Geomview.Active_Ver_Type",NULL))) {
		    case PHASE_SPACE_VARB:
			y = points[*((int *) pm(GET, "Geomview.Active_Ver_Index",NULL))];
			break;
		    case PARAMETER_VARB:
			y = params[*((int *) pm(GET, "Geomview.Active_Ver_Index",NULL))];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			y = func_values[*((int *) pm(GET, "Geomview.Active_Ver_Index",NULL))];
			break;
		    }

		    switch (*((int *) pm(GET, "Geomview.Active_Depth_Type",NULL))) {
		    case PHASE_SPACE_VARB:
			z = points[*((int *) pm(GET, "Geomview.Active_Depth_Index",NULL))];
			break;
		    case PARAMETER_VARB:
			z = params[*((int *) pm(GET, "Geomview.Active_Depth_Index",NULL))];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			z = func_values[*((int *) pm(GET, "Geomview.Active_Depth_Index",NULL))];
			break;
		    }

		    fprintf(dest_fp,"%g %g %g\n",x,y,z);
		} 
	    }
	}
    }
    if (memory_reset_read(mem_obj) == 0) {
	while (memory_read_next_flow(mem_obj, NULL, NULL, NULL, NULL, NULL) == 0) {
	    while (memory_read_next_traj(mem_obj, NULL, NULL, NULL) == 0) {
		while (memory_read_next_point(mem_obj, &points, &params, &color, 
					      NULL, NULL) == 0) {

		    filter_cntl.state = points;
		    filter_cntl.parameters = params;
		    filter_cntl.alt_color_index = color[0];
		    filter_cntl.pick_color_index = color[1];
		    if( color[0]<0 )
			color_index = -color[1];
		    else
			color_index = get_color(&filter_cntl);

		    if (color_index >= 0)
			seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color_index] + n_sys_colors;
/*			seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color_index]]; */
		    else
			seg_index = -color_index;
/*			seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[-color_index];*/
		    
		    fprintf(dest_fp,"%g %g %g 1.0\n",(double)(red_table[seg_index]/255.0),(double)(green_table[seg_index]/255.0),(double)(blue_table[seg_index]/255.0));
		} 
	    }
	}
    }
    if (num_points > 0) {
	fprintf(dest_fp,"})\n\n");
	num_points = 0;
    }
    fflush(dest_fp);

    free_dvector(func_values,0,func_dim-1);
    return(0);
}


