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
save.c - Notify and event callback functions.
*/
#include <xview/xview.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <math.h>
#include <ui_init.h>
#include <constants.h>
#include <memory.h>
#include <twoD.h>
#include <twoD_opt.h>
#include <twoD_ip.h>
#include <pm.h>
#include <pm_hash.h>
#include <saveload.h>
#include "save_ui.h"

static save_win_objects *save_ip = NULL;

/*
 * Menu handler for `filemenu (Save...)'.
 */
Menu_item
  save_handler(item, op)
Menu_item	item;
Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    save_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Save",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		save_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		save_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}


/* 
save_open()  displays the save window, creating it if necessary.
*/
save_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;

  if (!save_ip)
    {
      save_ip = save_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Save",save_ip->win,POPUP_WINDOW);
      pm(PUT_SAVABLE,"Win_Names.Save",SAVE_NONE,
	 NULL);
    }
  if (use_default == SET_WIN_CONFIG){
    rect = (Rect *)calloc(1,sizeof(Rect));
    frame_get_rect(save_ip->win,rect);	/* get the current configuration */
    rect->r_left = (short) left;
    rect->r_top = (short) top;
    if (width>0) rect->r_width = (short) width;
    if (height>0) rect->r_height = (short) height;
    frame_set_rect(save_ip->win,rect);	/* set the new configuration */
    free(rect);
  }
  save_data_refresh();
  mark_window_open("Save");
  xv_set(save_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(save_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}

int
save_close()
{
    mark_window_closed("Save");
    if(save_ip) {
	xv_set(save_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(save_ip->win, XV_SHOW, FALSE, NULL);	
    }
}
/*
 * save_data_refresh() uses postmaster info to update save window
 */
save_data_refresh()
{
  save_win_objects *ip = save_ip;
  char		strng[SIZE_OF_DIR_PLUS_FNAME];

  if (!ip ) return; 

  pm(GET, "Save.Directory", strng, NULL);
  xv_set(ip->directory, PANEL_VALUE, strng, NULL);
  pm(GET, "Save.Filename", strng, NULL);
  xv_set(ip->filename, PANEL_VALUE, strng, NULL);
  xv_set(ip->option, PANEL_VALUE, get_save_option(), NULL);
}

/*
 * save_read_window() reads info from the save window into the postmaster
 */
save_read_window()
{
  save_win_objects *ip = save_ip;
  char *strng;
  int  option_code, *checkbox, *ivector();

  if (!ip) return;

  checkbox = ivector(0,N_OPTIONS);

  strng = (char *) xv_get(ip->directory, PANEL_VALUE);		       /* Directory Name */
  pm(PUT, "Save.Directory", strng, NULL);

  strng = (char *) xv_get(ip->filename, PANEL_VALUE);		       /* Filename */
  pm(PUT, "Save.Filename", strng, NULL);

  option_code = (int) xv_get(ip->option, PANEL_VALUE);		       /* options */

  unmask(N_OPTIONS, checkbox, option_code);	  /* utility to decode mask to decide selected option */
  
  pm(PUT, "Save.Settings", checkbox[0], 
     PUT, "Save.Config",   checkbox[1], 
     PUT, "Save.Traj",     checkbox[2], 
     PUT, "Save.Fixpt",    checkbox[3], 
     PUT, "Save.Cont",     checkbox[4], 
     PUT, "Save.Param",    checkbox[5], 
     PUT, "Save.Select",   checkbox[6], 
     PUT, "Save.Funct",     checkbox[7], 
     NULL);
  free_ivector(checkbox,0,N_OPTIONS);
}

/* 
 * get_save_option() reads the postmaster flags for which objects are to be
 * saved and translates those into an integer corresponding to the option exclusive setting.
 */
int
get_save_option()
{
  int  option_code, checkbox[N_OPTIONS];
  int  mask();

  checkbox[0] = *((int *)pm(GET, "Save.Settings", NULL));
  checkbox[1] = *((int *)pm(GET, "Save.Config", NULL));
  checkbox[2] = *((int *)pm(GET, "Save.Traj", NULL));
  checkbox[3] = *((int *)pm(GET, "Save.Fixpt", NULL));
  checkbox[4] = *((int *)pm(GET, "Save.Cont", NULL));
  checkbox[5] = *((int *)pm(GET, "Save.Param", NULL));
  checkbox[6] = *((int *)pm(GET, "Save.Select", NULL));
  checkbox[7] = *((int *)pm(GET, "Save.Funct", NULL));

  return ( mask(N_OPTIONS, checkbox, &option_code) );		 /* utility to code option into a mask */
}

/*
*/
int
    write_win_info(fp,rect,label, w_left, w_top)
    FILE		*fp;
    Rect		**rect;
    char		*label;
    int             w_left, w_top;	/* correction for window manager misinfo about position of window */
{
    Xv_opaque		ip_win;
/*    extern char		*WINDOW_OBJECTS_NAME;*/
    char		compound_label[MAX_LABEL_LEN];
    int			*pm_result;

    sprintf(compound_label,"%s.%s.%s","Win","Open_Status",label);
    pm_result = (int *) pm(GET,compound_label,NULL);

    if ((! pm_result) || (! *pm_result))
	return NO_ERROR;

    sprintf(compound_label,"%s.%s.%s","Win","Handle",label);
    ip_win = (Xv_opaque) pm(GET,compound_label,NULL);

    if (!ip_win)
	return MINOR_ERROR;

    frame_get_rect(ip_win,*rect);

    fprintf(fp,"pm PUT Win.Current %s\n",label);

    sprintf(compound_label,"%s.%s","Win.Locn",label);
    fprintf(fp,"pm PUT_LIST %s %d %d %d %d %d %d\n",
	    compound_label,
	    0,3,
	    (*rect)->r_left+w_left, (*rect)->r_top+w_top,(*rect)->r_width,(*rect)->r_height);

    fprintf(fp,"pm EXEC Win.Open_Current\n\n");
    
}


/*
write_win_rect()  writes to file the rectangle (XView structure) defining an open window.
*/
write_win_info1p1(fp,ip,rect,label, w_left, w_top)
FILE		*fp;
Xv_opaque	ip;
Rect		**rect;
char		*label;
int             w_left, w_top;	/* correction for window manager misinfo about position of window */
{
        frame_get_rect(ip,*rect);
        fprintf(fp,"# Configuration %s %d %d %d %d\n",
	   label,(*rect)->r_left+w_left, (*rect)->r_top+w_top,(*rect)->r_width,(*rect)->r_height);
}

/*
*/
int
    write_twoD_win_info(fp,rect, window_number,w_left, w_top)
    FILE		*fp;
    Rect		**rect;
    int			window_number;
    int             	w_left, w_top;	/* correction for window manager misinfo about position of window */
{
    Xv_opaque		ip_win;
/*    extern char		*WINDOW_OBJECTS_NAME;*/
    char		compound_label[MAX_LABEL_LEN];
    int			v_dim,p_dim,n,
    			*pm_result;
    int           	format = SAVE_PRECISION;

    v_dim = *((int *) pm(GET,"Model.Varb_Dim", NULL));
    p_dim = *((int *) pm(GET,"Model.Param_Dim", NULL));

    sprintf(compound_label,"TwoD_Win.Handle.%d",window_number);
    ip_win = (Xv_opaque) pm(GET,compound_label,NULL);

    if (!ip_win)
	return MINOR_ERROR;

    frame_get_rect(ip_win,*rect);

    fprintf(fp,"pm PUT TwoD_Win.Window_Number %d\n",window_number);
    fprintf(fp,"pm PUT_LIST TwoD_Win.Locn %d %d %d %d %d %d\n", 0,3,
	    (*rect)->r_left+w_left, (*rect)->r_top+w_top,
	    (*rect)->r_width,(*rect)->r_height);
    fprintf(fp,"pm EXEC TwoD_Win.Open_Corresponding\n\n");

    fprintf(fp,"pm PUT TwoD_Win.Symbol_Index %d\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index);
    fprintf(fp,"pm PUT TwoD_Win.Sym_Size_Index %d\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index);
    fprintf(fp,"pm PUT TwoD_Win.Bg_Color_Index %d\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index);
    fprintf(fp,"pm PUT TwoD_Win.Show_Cmap %d\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Show_Cmap);
    fprintf(fp,"pm PUT TwoD_Win.Cmap_Type_Index %d\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index);
    n = TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index;
    if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type == 
	PARAMETER_VARB)
      n += v_dim;
    else if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type ==
	     FUNCTION_VARB)
      n+= v_dim+p_dim;
    fprintf(fp,"pm PUT TwoD_Win.Depth_Coord %d\n", n);
    fprintf(fp,"pm PUT TwoD_Win.Depth_Coord_Min %.*lg\n", format,
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Min);
    fprintf(fp,"pm PUT TwoD_Win.Depth_Coord_Max %.*lg\n", format,
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Max);
    fprintf(fp,"pm PUT TwoD_Win.Cmap_File %s\n",
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_File);
    fprintf(fp,"pm PUT TwoD_Win.Hor_Min %.*lg\n",
	    format,TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min);
    fprintf(fp,"pm PUT TwoD_Win.Hor_Max %.*lg\n",
	    format,TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max);
    fprintf(fp,"pm PUT TwoD_Win.Vert_Min %.*lg\n",
	    format,TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min);
    fprintf(fp,"pm PUT TwoD_Win.Vert_Max %.*lg\n",
	    format,TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max);
    fprintf(fp,"pm PUT TwoD_Win.Hor_Type %d\n",
	    TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type);
    fprintf(fp,"pm PUT TwoD_Win.Ver_Type %d\n",
	    TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type);
    fprintf(fp,"pm PUT TwoD_Win.Hor_Index %d\n",
	    TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index);
    fprintf(fp,"pm PUT TwoD_Win.Ver_Index %d\n",
	    TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index);
    fprintf(fp,"pm EXEC TwoD_Win.Update_Current\n\n");

}

save_config(fp)
FILE		*fp;
{
  Rect		*rect;
  int		window_number, w_left, w_top, v_dim, p_dim, n;
  int           format = SAVE_PRECISION;
  int		list_number=0; 	/* keeps track of how many 2D windows there are. */
                                /* If the window numbers are (say) 2,4,5 then the
                                      list_number's associated with these windows are 0,1,2 */
/*  extern char 	*WINDOW_NAMES_OBJ_NAME;*/
  struct pm_list
      *pm_hash_lookup(),
      *current, /* pointer to an entry in the hash table; other than for
		     traversal, utilized only when category is PM_OBJECT */
      *elem_ptr; /* pointer to an entry of category PM_ELEMENT */


  v_dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *) pm(GET, "Model.Param_Dim", NULL));

  rect = (Rect *)calloc(1,sizeof(Rect));

  if (save_ip) 
    if ( (int)xv_get(save_ip->win,WIN_SHOW) ) 
      /* window manager doesn't place frames where we tell it. */
      /* Fool it: figure out how much it's off and compensate. */
      {							   
        frame_get_rect(save_ip->win, rect);
	w_left = rect->r_left; w_top = rect->r_top;
	save_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, 
		  rect->r_width, rect->r_height);
        frame_get_rect(save_ip->win, rect);
	w_left -= rect->r_left; /* correct for window manager misinformation */
	w_top -= rect->r_top; 
	save_open(SET_WIN_CONFIG,rect->r_left+2*w_left, rect->r_top+2*w_top, 
		  rect->r_width, rect->r_height);
      }

  /* Traverse the list of named windows */
  current = pm_hash_lookup("Win_Names");
  elem_ptr = current->next_elem;
  while( elem_ptr != NULL) { /* traverse the list of elements in this group */
      if (elem_ptr->savable)
	  write_win_info(fp,&rect,elem_ptr->data.string_data, w_left, w_top);
      elem_ptr = elem_ptr->next_elem;
  }

  /* Treat twoD windows separately */
  for (window_number=0; window_number<=get_max_twoD(); window_number++)
    if (valid_twoD_id(window_number)) {
	  write_twoD_win_info(fp,&rect,window_number,w_left, w_top);
    }
  
}

/*
save_config1p1()   saves current configuration of dstool to data file
*/
save_config1p1(fp)
FILE		*fp;
{
  Rect		*rect;
  int		window_number, w_left, w_top, v_dim, p_dim, n;
  int           format = SAVE_PRECISION;
  int		list_number=0; 	/* keeps track of how many 2D windows there are. */
                                /* If the window numbers are (say) 2,4,5 then the
                                      list_number's associated with these windows are 0,1,2 */

  v_dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *) pm(GET, "Model.Param_Dim", NULL));

  rect = (Rect *)calloc(1,sizeof(Rect));
  if (save_ip) 
    if ( (int)xv_get(save_ip->win,WIN_SHOW) ) 
      /* window manager doesn't place frames where we tell it. */
      /* Fool it: figure out how much it's off and compensate. */
      {							   
        frame_get_rect(save_ip->win, rect);
	w_left = rect->r_left; w_top = rect->r_top;
	save_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, 
		  rect->r_width, rect->r_height);
        frame_get_rect(save_ip->win, rect);
	w_left -= rect->r_left; /* correct for window manager misinformation */
	w_top -= rect->r_top; 
	save_open(SET_WIN_CONFIG,rect->r_left+2*w_left, rect->r_top+2*w_top, 
		  rect->r_width, rect->r_height);
      }
  
  write_win_info1p1(fp,cmd_ip->win,&rect,"Cmd_Win", w_left, w_top);
/*
  if (ui_windows->orbit_win) 
    if ( (int)xv_get(ui_windows->orbit_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->orbit_win->win,&rect,"Orbit_Win", w_left, w_top);
  if (ui_windows->periodic_win)
    if ( (int)xv_get(ui_windows->periodic_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->periodic_win->win,&rect,"Periodic_Win", w_left, w_top);
  if (ui_windows->selected_win)
    if ( (int)xv_get(ui_windows->selected_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->selected_win->win,&rect,"Selected_Win", w_left, w_top);
  if (ui_windows->function_win)
    if ( (int)xv_get(ui_windows->function_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->function_win->win,&rect,"Function_Win", w_left, w_top);
  if (ui_windows->defaults_win)
    if ( (int)xv_get(ui_windows->defaults_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->defaults_win->win,&rect,"Defaults_Win", w_left, w_top);
  if (ui_windows->mult_win)
    if ( (int)xv_get(ui_windows->mult_win->win,WIN_SHOW) )
      write_win_info1p1(fp,ui_windows->mult_win->win,&rect,"Mult_Win", w_left, w_top);
*/
  if (cmd_ip->win) 
    write_win_info1p1(fp,cmd_ip->win,&rect,"Cmd_Win", w_left, w_top);
  for (window_number=0; window_number<=get_max_twoD(); window_number++)	/* handle the twoD windows separately */
    if (valid_twoD_id(window_number))
      {
	write_win_info1p1(fp,twoD_ip[window_number]->twoD_win->win,&rect,"TwoD_Win", w_left, w_top);
	fprintf(fp,"\tWindow_Number %d   Symbol    %d   Size %d           BG_Color %d\n\tShow_Cmap     %d   Cmap_Type %d   ",
		list_number, 
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index,
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index,
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index,
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Show_Cmap,
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index
		);
	n = TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index;
	if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type == PARAMETER_VARB)
	  n += v_dim;
	else if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type == FUNCTION_VARB)
	  n+= v_dim+p_dim;
	fprintf( fp, "Depth_Coord %d    DC_Min %.*lg    DC_Max %.*lg\n\tColor_Table   %s\n",
		n,
		format, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Min,
		format, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Max,
		TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_File
		);
	fprintf( fp, "\tHor_Min %.*lg   Hor_Max %.*lg    Ver_Min %.*lg   Ver_Max %.*lg\n",
		format, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		format, TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		format, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		format, TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max
		);
	fprintf( fp, "\tHor_Type %d  Ver_Type %d  Hor_Index %d  Ver_Index %d\n",
		TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index
		);
	list_number++;
      }
  free(rect);
}

