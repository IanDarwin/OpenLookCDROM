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
#include <xview/xview.h>

#include <pm_main.h>
#include <ui_init.h>
#include <constants.h>
#include <memory.h>
#include <filters.h>
#include <twoD.h>
#include "geomview_cui.h"
#include "geomview.h"

int
geomview_view_go()
{
    int 
	get_display_type(),
	valid_twoD_id(),
	*ivector(),
	number_sys_colors,
	number_traj_colors,
	n_colors,
	*red_table,
	*green_table,
	*blue_table;

    if (!valid_twoD_id(geomview_cntl.window_num)) {
	fprintf(stderr,"Invalid window number %d.\n",geomview_cntl.window_num);
	return(-1);
    }

    if(get_display_type(geomview_cntl.window_number) != TRUE ) {
	fprintf(stderr,"Sorry, the geomview panel requires a color display.\n");
	return(-1);
    }

    number_sys_colors = *((int *) pm( GET, Colormap_Object, Sys_Colormap_Size, NULL));
    number_traj_colors = *((int *) pm( GET, Colormap_Object, Traj_Colormap_Size, NULL));
    n_colors = number_sys_colors + number_traj_colors;

    red_table = ivector(0,n_colors-1);
    pm(GET_LIST, Colormap_Object, Red_Table, 0, n_colors-1, red_table, NULL);
    
    green_table = ivector(0,n_colors-1);
    pm(GET_LIST, Colormap_Object, Green_Table, 0, n_colors-1, green_table, NULL);
    
    blue_table = ivector(0,n_colors-1);
    pm(GET_LIST, Colormap_Object, Blue_Table, 0, n_colors-1, blue_table, NULL);

    fprintf(stderr,"Sending to geomview:\n");
    geomview_using_window(geomview_cntl.window_number,n_colors,red_table,green_table,blue_table);
}

/* Display all memory objects valid for window_number. */
int
	geomview_using_window(window_num,n_colors,red_table,green_table,blue_table)
int	
	window_num,
	n_colors,
	*red_table,
	*green_table,
	*blue_table;

{
    extern int valid_twoD_id();
    memory	mem_obj_ptr;

    if(!valid_twoD_id(window_num)) return(-1);

    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Traj_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Mult_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Fixed_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Cont_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Sel_Pt_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    if( mem_obj_ptr = (memory) pm(GET, Mem_Obj_Cntl, Param_Mem_Ptr, NULL) )
	mem_obj_to_geomview( mem_obj_ptr, window_num );
    return(0);
}


struct  Filter_DataS     filter_cntl;

/*  Display memory object in window number window_num. */
int
	mem_obj_to_geomview( mem_obj, window_num)
memory	
	mem_obj;
int	
	window_num;
{
    int 
	valid_twoD_id(),
	get_color(),
	color_index,
	seg_index,
	memory_stored_points(),
	i,num_points,
	reset_win_dim();
    double 
	x, y, z;
    double 
	*points, *params,*func_values, *dvector();
    int 
	mem_obj_type = memory_get_type( mem_obj );
    int 
	func_dim, *color;

    if ( !valid_twoD_id( window_num ) || !valid_mem_to_win( mem_obj_type, window_num) ) return(-1);

    filter_cntl.window_id = window_num;
    reset_win_dim(window_num);

    func_dim = *((int *)pm(GET,Traj_Ds_Object,Function_Dim,NULL));
    func_values = dvector(0,func_dim-1);

    if (memory_reset_read(mem_obj) == 0) {
	if ((num_points = memory_stored_points(mem_obj)) > 0) {
	    fprintf(stdout,"VECT\n%d %d %d\n",num_points,num_points,num_points);
	    for (i=0; i < num_points; i++) {
		fprintf(stdout,"1 1\n");
	    }
	}
	while (memory_read_next_flow(mem_obj, NULL, NULL, NULL, NULL, NULL) == 0) {
	    while (memory_read_next_traj(mem_obj, NULL, NULL, NULL) == 0) {
		while (memory_read_next_point(mem_obj, &points, &params, &color, 
					      NULL, NULL) == 0) {
		    switch (geomview_cntl.active_hor_type) {
		    case PHASE_SPACE_VARB:
			x = points[geomview_cntl.active_hor_index];
			break;
		    case PARAMETER_VARB:
			x = params[geomview_cntl.active_hor_index];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			x = func_values[geomview_cntl.active_hor_index];
			break;
		    }

		    switch (geomview_cntl.active_ver_type) {
		    case PHASE_SPACE_VARB:
			y = points[geomview_cntl.active_ver_index];
			break;
		    case PARAMETER_VARB:
			y = params[geomview_cntl.active_ver_index];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			y = func_values[geomview_cntl.active_ver_index];
			break;
		    }

		    switch (geomview_cntl.active_depth_type) {
		    case PHASE_SPACE_VARB:
			z = points[geomview_cntl.active_depth_index];
			break;
		    case PARAMETER_VARB:
			z = params[geomview_cntl.active_depth_index];
			break;
		    case FUNCTION_VARB:
			get_ds_func(func_values, points, params);
			z = func_values[geomview_cntl.active_depth_index];
			break;
		    }

		    fprintf(stdout,"%g %g %g\n",x,y,z);
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
			seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color_index]]; 
		    else
			seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[-color_index];
		    
		    fprintf(stdout,"%g %g %g 1.0",(double)(red_table[seg_index]/255.0),(double)(green_table[seg_index]/255.0),(double)(blue_table[seg_index]/255.0));
		} 
	    }
	}
    }
    free_dvector(func_values,0,func_dim-1);
    return(0);
}


/*

	if(color_index >= 0)
		seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color_index] ); 
	else
		seg_index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[-color_index]);


	if(color>=0)
		XSetForeground(dpy,gc,TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[
			       (int) TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color] ]); 
	else
		XSetForeground(dpy,gc,TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[-color]);


     pm( PUT, Colormap_Object, Sys_Colormap_Size, *number_sys_colors, NULL);
     pm( PUT, Colormap_Object, Traj_Colormap_Size, *number_traj_colors, NULL);

     pm( INIT,  Colormap_Object, Red_Table, n_colors,
         PUT_LIST, Colormap_Object, Red_Table, 0, n_colors-1, red, NULL);
     pm( INIT,  Colormap_Object, Green_Table, n_colors,
         PUT_LIST, Colormap_Object, Green_Table, 0, n_colors-1, green, NULL);
     pm( INIT,  Colormap_Object, Blue_Table, n_colors,
         PUT_LIST, Colormap_Object, Blue_Table, 0, n_colors-1, blue, NULL);

*/
