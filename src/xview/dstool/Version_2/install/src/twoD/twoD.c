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
#include <math.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/notice.h>
#include <gcm.h>
#include <gdd.h>

#include <constants.h>
#include <ui_init.h>
#include <memory.h>
#include <modellib.h>
#include <filters.h>
#include <pm.h>
#include <pm_hash.h>
#include <defaults.h>
#include "twoD_opt_def.h"
#include "twoD_def.h"
#include "twoD_ip.h"


TwoD_Ds_List_Item	**TwoD_Ds; 
TwoD_Opt_Ds_List_Item	**TwoD_Opt_Ds; 

twoD_list_item **twoD_ip = NULL;

static	int	max_num_twoD=0;	  /* maximum number of initialized windows (ie, array size) */ 


/*
 * Menu handler for `viewmenu (2-D image)'.
 */
Menu_item
twoD_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
/*  if (op == MENU_NOTIFY) 
    twoD_open(-1,DEFAULT_WIN_CONFIG,0,0,0,0); */
    int
	window_number,
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    twoD_open(-1,DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"TwoD_Win.Locn",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION)) {
		window_number = *((int *) pm(GET,"TwoD_Win.Window_Number",NULL));
		twoD_open(window_number,SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    }
	    else
		twoD_open(-1,DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}

/*  --------------------------------------------------------------------------------------------
    twoD_open()  displays the twoD window, creating it (and associated twoD_opt) if necessary.
 
    --------------------------------------------------------------------------------------------  */
int
twoD_open(window_number,use_default,left,top,width,height)
int   window_number;
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  char	label[MAX_LABEL_LEN];
	

  if (!valid_twoD_id(window_number)) {
      window_number = twoD_new_win();

      sprintf(label,"TwoD_Win.Handle.%d",window_number);
      pm(CREATE_ELEM, label, ADDRS,NULL);
      pm(PUT,label,twoD_ip[window_number]->twoD_win->win,NULL);

      sprintf(label,"TwoD_Win.Opt.Handle.%d",window_number);
      pm(CREATE_ELEM, label, ADDRS,NULL);
      pm(PUT,label,twoD_ip[window_number]->twoD_opt_win->win,NULL);

      sprintf(label,"TwoD_Win.Open_Status.%d",window_number);
      pm(CREATE_ELEM, label, INT,NULL);
      pm(PUT,label,TRUE,NULL);

      sprintf(label,"TwoD_Win.Opt.Open_Status.%d",window_number);
      pm(CREATE_ELEM, label, INT,NULL);
      pm(PUT,label,FALSE,NULL);
  }
	
  if(use_default == SET_WIN_CONFIG){
    rect = (Rect *)calloc(1,sizeof(Rect));
    frame_get_rect(twoD_ip[window_number]->twoD_win->win,rect);
    rect->r_left = (short) left;
    rect->r_top = (short) top;
    if(width>0) rect->r_width = (short) width;
    if(height>0) rect->r_height = (short) height;
    frame_set_rect(twoD_ip[window_number]->twoD_win->win,rect);
    free(rect);
  }
  xv_set(twoD_ip[window_number]->twoD_win->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
	
  return(window_number);
}

void
twoD_field_manager()
{
  int i;

  for(i=get_min_twoD(); i<=get_max_twoD(); i++) 
    if ( valid_twoD_id(i) ) 
      {
	twoD_field_manager_n(i);             /* calls set_twoD_defaults */
	opt_field_manager(i);
      }
  
}

/* Create the custom fields for a twoD window. */
twoD_field_manager_n(window_number)
int	window_number;
{
  char	name[MAX_LEN_VARB_NAME];
  int	i, n_total;
  int n_varb, n_param, n_funct;
  
  twoD_win_objects        *ip = twoD_ip[window_number]->twoD_win;
  get_n_all_types(&n_varb, &n_param, &n_funct);

  if(twoD_ip[window_number]->twoD_win)
    {
     xv_destroy(twoD_ip[window_number]->twoD_win->hor);
     xv_destroy(twoD_ip[window_number]->twoD_win->ver);
    }

  ip->hor = twoD_win_hor_create(ip, ip->pan, n_varb + n_param + n_funct);
  ip->ver = twoD_win_ver_create(ip, ip->pan, n_varb + n_param + n_funct);
  xv_set(ip->hor,PANEL_VALUE,0,NULL);
  if(n_varb>1) 
    xv_set(ip->ver,PANEL_VALUE,1,NULL);
  else
    xv_set(ip->ver,PANEL_VALUE,0,NULL);

  n_total = n_varb + n_param + n_funct; 
  for(i=0;i<n_varb;i++)
    {pm( GET, "Model.Varb_Names", i, name , NULL);
     xv_set(ip->hor,PANEL_CHOICE_STRING,i,name,NULL); 
     xv_set(ip->ver,PANEL_CHOICE_STRING,i,name,NULL);}
  for(i=n_varb;i<n_varb+n_param;i++)
    {pm( GET, "Model.Param_Names", i-n_varb , name , NULL);
     xv_set(ip->hor,PANEL_CHOICE_STRING,i,name,NULL); 
     xv_set(ip->ver,PANEL_CHOICE_STRING,i,name,NULL);}
  for(i=n_varb+n_param;i<n_total;i++)
    {pm( GET, "Model.Funct_Names", i-n_varb-n_param , name , NULL);
     xv_set(ip->hor,PANEL_CHOICE_STRING,i,name,NULL); 
     xv_set(ip->ver,PANEL_CHOICE_STRING,i,name,NULL);}

  set_twoD_defaults(window_number);
}

/* Refresh the data displayed in the twoD window. */
twoD_data_refresh(window_number)
int	window_number;
{
  char	strng[20],title[MAX_LEN_DS_TITLE];
  int	format  = *((int *) pm( GET, "Defaults.Precision", NULL ));
  twoD_win_objects        *ip = twoD_ip[window_number]->twoD_win;
  char	*get_ds_name();
  int   index;

  sprintf(title,"DsTool: %s", get_ds_name());
  xv_set(ip->win, FRAME_LABEL, title, NULL);

  /* set horizontal min and max */
  sprintf(strng, "%.*lg", format,TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min);
  xv_set(ip->hormin, PANEL_VALUE,strng, NULL);
  sprintf(strng, "%.*lg", format,TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max);
  xv_set(ip->hormax, PANEL_VALUE,strng, NULL); 

  /* set vertical min and max */
  sprintf(strng, "%.*lg", format,TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min);
  xv_set(ip->vermin, PANEL_VALUE,strng, NULL);
  sprintf(strng, "%.*lg", format,TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max);
  xv_set(ip->vermax, PANEL_VALUE,strng, NULL);

  /* set ver and hor stack settings */
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index;
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PARAMETER_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index + 
                    *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == FUNCTION_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index + 
                    *((int *) pm( GET, "Model.Varb_Dim", NULL )) +
                    *((int *) pm( GET, "Model.Param_Dim", NULL ));
  xv_set( twoD_ip[window_number]->twoD_win->hor, PANEL_VALUE, index, NULL);

  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index;
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PARAMETER_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index + 
                    *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == FUNCTION_VARB)
    index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index + 
                    *((int *) pm( GET, "Model.Varb_Dim", NULL )) +
                    *((int *) pm( GET, "Model.Param_Dim", NULL ));
  xv_set( twoD_ip[window_number]->twoD_win->ver, PANEL_VALUE, index, NULL);
}


/* --------------------------------------------------------------------------------
   initializes fields on twoD windows

   --------------------------------------------------------------------------------  */
int
set_twoD_fields(window_number, hor_type,hor_index,ver_type,ver_index)
int     window_number, hor_type,hor_index,ver_type,ver_index;
{
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type = hor_type;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index = hor_index;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type = ver_type;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index = ver_index;
 
  use_default_size(window_number);                /* load default data into window */
  twoD_data_refresh(window_number);		  /* set new panel values and refresh */
}

/* Read the min and max values of the vertical coordinates and store in twoD_Ds. */
twoD_hor_reset(window_number)
int	window_number;
{
  char *min, *max;
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB){
    min = "Defaults.Varb_Min";
    max = "Defaults.Varb_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PARAMETER_VARB){
    min = "Defaults.Param_Min";
    max = "Defaults.Param_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == FUNCTION_VARB){
    min = "Defaults.Funct_Min";
    max = "Defaults.Funct_Max";
  }

  TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min =
    *((double *) pm( GET, min, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index, NULL));
  TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max = 
    *((double *) pm( GET, max, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index, NULL));
}


/* Read the min and max values of the vertical coordinates and store in twoD_Ds. */
twoD_ver_reset(window_number)
int	window_number;
{
  char *min, *max;

  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB){
    min = "Defaults.Varb_Min";
    max = "Defaults.Varb_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PARAMETER_VARB){
    min = "Defaults.Param_Min";
    max = "Defaults.Param_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == FUNCTION_VARB){
    min = "Defaults.Funct_Min";
    max = "Defaults.Funct_Max";
  }

  TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min = 
    *((double *) pm( GET, min, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index, NULL));
  TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max =
    *((double *) pm( GET, max, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index, NULL));
}


/*
 * routine to read hor and ver min max fields from window into the data structure
 */
twoD_read_scale(window_number)
int window_number;
{
  double        value;
  twoD_win_objects    *ip;

  if (window_number == -1) return;

  ip = twoD_ip[window_number]->twoD_win;

  value = atof( (char *) xv_get(ip->hormin, PANEL_VALUE));
  TwoD_Ds[ window_number ]->TwoD_Win_Ds->Hor_Min = value;
  value = atof( (char *) xv_get(ip->hormax, PANEL_VALUE));
  TwoD_Ds[ window_number ]->TwoD_Win_Ds->Hor_Max = value;
  value = atof( (char *) xv_get(ip->vermin, PANEL_VALUE));
  TwoD_Ds[ window_number ]->TwoD_Win_Ds->Vert_Min = value;
  value = atof( (char *) xv_get(ip->vermax, PANEL_VALUE));
  TwoD_Ds[ window_number ]->TwoD_Win_Ds->Vert_Max = value;

}

/* create a new twoD window  */
int
twoD_new_win()
{
  Cms		cms;
  extern Cms	set_colorsegment(), set_nocolor();
  int 		*window_number,number_sys_colors, number_traj_colors;
  twoD_win_objects	*new_win, *twoD_win_objects_initialize();
  twoD_opt_win_objects	*new_opt_win, *twoD_opt_win_objects_initialize();

  void		twoD_pm_install();

  /* initialize windows and colormaps */
  cms = set_colorsegment( &number_sys_colors, &number_traj_colors);
  /* create a new twoD window */
  new_win = twoD_win_objects_initialize(NULL, cmd_ip->win, cms);	
  new_opt_win = twoD_opt_win_objects_initialize(NULL, new_win->win);

  /* save this pointer */
  window_number = (int *) calloc(1,sizeof(int));			
  /* allocate space & initialize structs */
  *window_number = install_new_twoD(new_win, new_opt_win);  

  xv_set(canvas_paint_window(new_win->canvas), XV_KEY_DATA, MODEL_MENU_ITEM_KEY, window_number, 0);
  xv_set(canvas_paint_window(new_win->twoD_cbar), XV_KEY_DATA, MODEL_MENU_ITEM_KEY, window_number, 0);
/*  gcm_initialize_colors(new_win->pan, 
			Panel_Color_Choice[(int)fmod((double) *window_number,(double)Num_Panel_Colors)], NULL); */

  /* display window and initialize to default values */
  twoD_field_manager_n(*window_number);
  set_twoD_defaults(*window_number);
  set_twoD_opt_fields(*window_number);

  /* find out if display is color or monochrome and attach new colormap if needed */
  if( (int) get_display_type(*window_number) == TRUE )
    {
	gcm_initialize_colors
	    (new_win->pan, Panel_Color_Choice[(int)fmod((double) *window_number, (double)Num_Panel_Colors)], NULL);
	set_colormap( *window_number, number_sys_colors, number_traj_colors, cms);
	set_colortable(*window_number);
	/* xv_set( new_win->canvas, WIN_BACKGROUND_COLOR, 
	       *((int *) pm(GET, "Defaults.Bgnd_Color", NULL) ));   */

      }
  else
    {cms = (Cms) set_nocolor( *window_number, &number_sys_colors, &number_traj_colors);
     set_colormap( *window_number, number_sys_colors, number_traj_colors, cms);}
	
  return(*window_number);
}



/* ----------------------------------------------------
   allocate space for new twoD view window

   arguments:  pointers to new canvas window and option window structures
   return:     -1  memory allocation failed
		n  index number of new window   ( n>0 )  
   ---------------------------------------------------- */

int  
install_new_twoD(new_win,new_opt_win)
twoD_win_objects	*new_win;		/* pointer to new window structure */
twoD_opt_win_objects	*new_opt_win;		/* pointer to new option window structure */
{
  int			i, new_num;
  twoD_list_item	**temp;
  TwoD_Ds_List_Item	**temp_ds;
  TwoD_Opt_Ds_List_Item	**temp_opt_ds;

  new_num = free_twoD_num();

  if (new_num == -1) {
    new_num = max_num_twoD++;

    /* allocate new memory */
    if (!(temp = (twoD_list_item **) calloc(max_num_twoD, sizeof(twoD_list_item *))))  return(-1); 
    if (!(temp_ds = (TwoD_Ds_List_Item **) calloc(max_num_twoD, sizeof(TwoD_Ds_List_Item *)))) return(-1); 
    if (!(temp_opt_ds = (TwoD_Opt_Ds_List_Item **) 
	  calloc(max_num_twoD, sizeof(TwoD_Opt_Ds_List_Item *)))) return(-1); 

    for (i=0; i<(max_num_twoD-1); i++) { /* copy over old stuff */ 
      temp[i] = twoD_ip[i];	
      temp_ds[i] = TwoD_Ds[i];	
      temp_opt_ds[i] = TwoD_Opt_Ds[i];
    }

    if (twoD_ip) cfree(twoD_ip); /* deallocate old memory */
    if (TwoD_Ds) cfree(TwoD_Ds);
    if (TwoD_Opt_Ds) cfree(TwoD_Opt_Ds);

    twoD_ip = temp;
    TwoD_Ds = temp_ds;		/* add reference data to new structures */
    TwoD_Opt_Ds = temp_opt_ds;

  }

  if (!(twoD_ip[new_num] = (twoD_list_item *) calloc(1, sizeof(twoD_list_item))))  return(-1); 
  if (!(TwoD_Ds[new_num] = (TwoD_Ds_List_Item *) calloc(1, sizeof(TwoD_Ds_List_Item)))) return(-1); 
  if (!(TwoD_Ds[new_num]->TwoD_Win_Ds = (TwoD_Win_Cntl_Ds_Objects *) 
	calloc(1, sizeof(TwoD_Win_Cntl_Ds_Objects)))) return(-1); 
  if (!(TwoD_Opt_Ds[new_num] = (TwoD_Opt_Ds_List_Item *) 
	calloc(1, sizeof(TwoD_Opt_Ds_List_Item)))) return(-1); 
  if (!(TwoD_Opt_Ds[new_num]->TwoD_Opt_Win_Ds = (TwoD_Opt_Win_Cntl_Ds_Objects *) 
	calloc(1, sizeof(TwoD_Opt_Win_Cntl_Ds_Objects)))) return(-1); 

  twoD_ip[new_num]->twoD_win = new_win; /* add new window */
  twoD_ip[new_num]->twoD_opt_win = new_opt_win;
	
  return(new_num);
}


/* ----------------------------------------------------
   returns the maximum valid window number

   arguments:   none
   return:      -1	no windows initialized 
		n	maximum window index
   ---------------------------------------------------- */
int
get_max_twoD()
{
  if( max_num_twoD == 0)
    return(-1); 
  else 
    return( max_num_twoD-1 );
}



/* ----------------------------------------------------
   returns the minimum valid window number       
   (may be different than 1 since first window may have been
    released by user)
   last change:  12 December 1990

   arguments:   none
   return:      -1	no windows initialized 
		n	minimum window index

   RETURN:	0 at all times! 
   ---------------------------------------------------- */
int
get_min_twoD()
{
  return (0);
}




/* ----------------------------------------------------
   returns TRUE if window_number is a valid twoD id
   identifier and FALSE otherwise, even if it is not 
   currently displayed.
   ---------------------------------------------------- */
int
valid_twoD_id( window_number)
int	window_number;
{
  if (window_number < 0) return(FALSE);
  if (window_number >= max_num_twoD) return(FALSE);
  if (!TwoD_Ds[window_number]) return(FALSE);
  return(TRUE);
}

/* Return the first free window number or -1 if all up to
   max_num_twoD are in use. */
int
free_twoD_num()
{
  int 	i;

  for(i=0; i<max_num_twoD; i++) {
    if (valid_twoD_id(i) == FALSE) return(i);
  }
  return(-1);
}

/* --------------------------------------------------------------------
   routine returns the twoD window number from either a window handle
   or associated option panel handle
   -------------------------------------------------------------------- */

int
get_twoD_number(handle)
void	*handle;
{
  int	count;

  for (count=0; count<max_num_twoD; count++) {
    if (twoD_ip[count]) {
      if ( twoD_ip[count]->twoD_win == (twoD_win_objects *) handle ) {
	return(count);
      }
      if ( twoD_ip[count]->twoD_opt_win == (twoD_opt_win_objects *) handle )  {
	return(count); 
      }
    }
  }
  return(-1);
}
         

/* Delete a view window and free associated memory */
void
delete_twoD_win(twoD_win)
twoD_win_objects	*twoD_win;
{
  int	n;

  char	label[MAX_LABEL_LEN];

  /* find out which window */
  if ( (n = get_twoD_number(twoD_win)) == -1) 
    {
      /* error - invalid window */
      return;
    }

  if (xv_destroy_safe(twoD_win->win) == XV_OK) 
    {
      /* destroy twoD_opt window too! */
      /* release memory */  
      cfree(twoD_ip[n]->twoD_win);
      cfree(twoD_ip[n]->twoD_opt_win);
      cfree(twoD_ip[n]);
      cfree(TwoD_Ds[n]->TwoD_Win_Ds);
      cfree(TwoD_Ds[n]);
      cfree(TwoD_Opt_Ds[n]->TwoD_Opt_Win_Ds);
      cfree(TwoD_Opt_Ds[n]);
	
      /* make the window entries all NULL ; as more windows are opened, they will be used again as needed */
      twoD_ip[n] = NULL;
      TwoD_Ds[n] = NULL;
      TwoD_Opt_Ds[n] = NULL;
    }

  /* clean up postmaster entries */
  sprintf(label,"TwoD_Win.Handle.%d",n);
  pm(RM_ELEM,label,NULL);

  sprintf(label,"TwoD_Win.Opt.Handle.%d",n);
  pm(RM_ELEM,label,NULL);

  sprintf(label,"TwoD_Win.Open_Status.%d",n);
  pm(RM_ELEM,label,NULL);

  sprintf(label,"TwoD_Win.Opt.Open_Status.%d",n);
  pm(RM_ELEM,label,NULL);
}


/* ---------------------------------------------------------------------------
   routine to fetch twoD window canvas and panel handles

   arguments:	n	twoD window number
   return:		handle which must be cast into the proper data type
   --------------------------------------------------------------------------- */
get_twoD_handle(window_number,data_type)
int	window_number;
int 	data_type;
{
  twoD_win_objects    *ip = twoD_ip[window_number]->twoD_win;

  switch (data_type) {
  case CANVAS_HANDLE :
    return( ip->canvas );
  case CANVAS_PW_HANDLE :
    return( canvas_paint_window(ip->canvas) );
  case CBAR_CANVAS_HANDLE :
    return( ip->twoD_cbar );
  case CBAR_PW_HANDLE :
    return( canvas_paint_window(ip->twoD_cbar) );
  case CBAR_LT_HANDLE :
    return( ip->cbar_lt );
  case CBAR_RT_HANDLE :
    return( ip->cbar_rt );
  case CBAR_LT_PAN_HANDLE :
    return( ip->controls1 );
  case CBAR_RT_PAN_HANDLE :
    return( ip->controls2 );
  case PANEL_HANDLE :
    return( ip->pan );
  case WINDOW_HANDLE :
    return( ip->win );
  }
  return(NULL);
}



/*  -----------------------------------------------------------------------------
    Function: use_default_size
    Description: changes the min & max fields of a twoD window to default values

    -----------------------------------------------------------------------------  */
int
use_default_size(window_number)
int window_number;
{
  twoD_hor_reset(window_number);
  twoD_ver_reset(window_number);
  twoD_data_refresh(window_number);
}



/*-------------------------------------------------------------------------------
  Function: set_default_size
  Description: makes the default range of the twoD coordinates equal the value 
  currently displayed.

  -------------------------------------------------------------------------------  */
int
set_default_size(window_number)
int window_number;
{
  char *min, *max;
	 
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB){
    min = "Defaults.Varb_Min";
    max = "Defaults.Varb_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PARAMETER_VARB){
    min = "Defaults.Param_Min";
    max = "Defaults.Param_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == FUNCTION_VARB){
    min = "Defaults.Funct_Min";
    max = "Defaults.Funct_Max";
  }
	
  pm( PUT, min, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index, 
     (double) TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min, NULL);
  pm( PUT, max, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index, 
     (double) TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max, NULL);
	
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB){
    min = "Defaults.Varb_Min";
    max = "Defaults.Varb_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PARAMETER_VARB){
    min = "Defaults.Param_Min";
    max = "Defaults.Param_Max";
  }
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == FUNCTION_VARB){
    min = "Defaults.Funct_Min";
    max = "Defaults.Funct_Max";
  }
	
  pm( PUT, min, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index, 
     (double) TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min, NULL);
  pm( PUT, max, (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index, 
     (double) TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max, NULL);
}

	
/* --------------------------------------------------------------------------------
   sets the twoD and twoD opt windows to default conditions
    
   --------------------------------------------------------------------------------  */
int
set_twoD_defaults(window_number)
int             window_number;
{
 
  /* set default twoD fields; protect against 1D maps (?)  */
  if( *((int *) pm( GET, "Model.Varb_Dim", NULL )) > 1 )
    set_twoD_fields(window_number,
		    PHASE_SPACE_VARB,0,	/* hor type  and  hor index */
		    PHASE_SPACE_VARB,1); /* ver type  and  ver index */
  else
    set_twoD_fields(window_number,
		    PHASE_SPACE_VARB,0,	/* hor type  and  hor index */
		    PHASE_SPACE_VARB,0); /* ver type  and  ver index */ 
}


/*  --------------------------------------------------------------------------------
    returns the label for the horizontal coord
 
   ---------------------------------------------------------------------------------  */
char *
get_hor_label(window_number)
int             window_number;
{
  int             index;
  char *code_name;
  char            *name = (char *)calloc(MAX_LEN_VARB_NAME,sizeof(char));
	
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PHASE_SPACE_VARB)
    code_name = "Model.Varb_Names";
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PARAMETER_VARB)
    code_name = "Model.Param_Names";
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == FUNCTION_VARB)
    code_name = "Model.Funct_Names";
  index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index;
  pm( GET, code_name, index, name , NULL);
  return(name);
}



/* --------------------------------------------------------------------------
   returns the label for the vertical coord
 
   --------------------------------------------------------------------------  */
char *
get_ver_label(window_number)
int             window_number;
{
  int             index;
  char *code_name;
  char            *name = (char *)calloc(MAX_LEN_VARB_NAME,sizeof(char));
	
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PHASE_SPACE_VARB)
    code_name = "Model.Varb_Names";
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PARAMETER_VARB)
    code_name = "Model.Param_Names";
  else if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == FUNCTION_VARB)
    code_name = "Model.Funct_Names";
  index = TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index;
  pm( GET, code_name, index, name , NULL);
  return(name);
}


/*  -------------------------------------------------------------------------------------------
    compute colorbar cell based on x-coordinate of cursor position

    -------------------------------------------------------------------------------------------  */

int
cbar_cell_index( window_number, xcoord )
int	window_number;
int	xcoord;
{
  int	index;

  if(xcoord<=CBAR_CELL_WIDTH)
    index = 0;
  else
    {xcoord = xcoord - CBAR_CELL_WIDTH - 1;
     index = 1 + (int) floor( ((double) xcoord) / ((double) (1+CBAR_CELL_WIDTH)) );}

  index = index + (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index;
  if( index < 0 )
    index += TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors;
  else if( index >= TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors )
    index -= TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors;
  return(index);
}


/* --------------------------------------------------------------------------
   routine to clear all active canvases (currently open or not)

   -------------------------------------------------------------------------- */
clear_all_canvases()

{
  int		count;

  for( count=0; count<=get_max_twoD(); count++ )
    if (valid_twoD_id(count)) 
      clear_canvas(count);
}



/* -------------------------------------------------------------------------- 
   routine to clear an active canvas prescribed by index
   (currently open or not) 

   -------------------------------------------------------------------------- */
clear_canvas(window_number)
int	window_number;
{
  Xv_window       paint_window;

  paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
  XClearWindow( (Display *) xv_get(paint_window, XV_DISPLAY), (Window) xv_get(paint_window, XV_XID));
}



/* ----------------------------------------------------------------------------------
   request colormap segment for systems which support color graphics

   ---------------------------------------------------------------------------------- */
Cms
set_colorsegment(number_sys_colors, number_traj_colors)
int	*number_sys_colors, *number_traj_colors;
{
  int			red[MAX_COLORS], green[MAX_COLORS], blue[MAX_COLORS], n_colors, i;
  static int		first_window=1;
  static Xv_singlecolor	colors[MAX_COLORS];
  static Cms		cms;

  if(first_window==TRUE)
    {
     n_colors = ld_colormap( SYS_COLORMAP, TRAJ_COLORMAP, red, green, blue,
			    number_sys_colors, number_traj_colors);
     if (n_colors < 0) 
	 stop_execution("Unable to load colormap.\nPlease check the values of the environment variables DSTOOL and DSTOOL_COLOR_DIR.");
     pm( PUT, "Color.Sys_Colormap_Size", *number_sys_colors, NULL);
     pm( PUT, "Color.Traj_Colormap_Size", *number_traj_colors, NULL);
     for(i=0;i<n_colors;i++)
       {colors[i].red = red[i];
        colors[i].green = green[i];
        colors[i].blue = blue[i];}
/*     cms = (Cms) xv_create( NULL, CMS, CMS_SIZE, n_colors, CMS_COLORS, colors, CMS_TYPE, XV_DYNAMIC_CMS, NULL); */
     cms = (Cms) xv_create( NULL, CMS, CMS_SIZE, n_colors, CMS_COLORS, colors, NULL); 
     pm( INIT,  "Color.Red_Table", n_colors,
         PUT_LIST, "Color.Red_Table", 0, n_colors-1, red, NULL);
     pm( INIT,  "Color.Green_Table", n_colors,
         PUT_LIST, "Color.Green_Table", 0, n_colors-1, green, NULL);
     pm( INIT,  "Color.Blue_Table", n_colors,
         PUT_LIST, "Color.Blue_Table", 0, n_colors-1, blue, NULL);
     first_window = FALSE;
    }
  *number_sys_colors = *( (int *)  pm( GET, "Color.Sys_Colormap_Size", NULL) );
  *number_traj_colors = *( (int *) pm( GET, "Color.Traj_Colormap_Size", NULL) );

  return( (Cms) cms);
}


/* --------------------------------------------------------------------------------
   set graphics context (GC) and set twoD window data

   -------------------------------------------------------------------------------- */

set_colormap(window_number, number_sys_colors, number_traj_colors, cms)
int	window_number, number_sys_colors, number_traj_colors;
Cms		cms;
{
  Xv_window       	paint_window;
  Display         	*display;
  XGCValues       	gc_val;
  XID     		canvas_xid;
  unsigned long		*color_ptr;
/*  static	char		stipple_bits[4] = {0xAA,0xAA,0x55,0x55}; */
  static	char		stipple_bits[4] = {'\252','\252','\125','\125'};


  paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
  display = (Display *)xv_get(paint_window,XV_DISPLAY);
  canvas_xid = (XID) xv_get(paint_window,XV_XID);
  gc_val.stipple = XCreateBitmapFromData(display,canvas_xid,stipple_bits,16,2);
  TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc = (GC) XCreateGC(display,canvas_xid,GCStipple,&gc_val);
  color_ptr = (unsigned long *) xv_get(cms, CMS_INDEX_TABLE);
  TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Sys_Colors = number_sys_colors;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors = number_traj_colors; 
  TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors = color_ptr;
  TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors = color_ptr + number_sys_colors;

  paint_window = (Xv_window) get_twoD_handle(window_number,CBAR_PW_HANDLE);
  display = (Display *)xv_get(paint_window,XV_DISPLAY);
  canvas_xid = (XID) xv_get(paint_window,XV_XID);
  TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc = (GC) XCreateGC(display,canvas_xid,GCStipple,&gc_val);
}



/*  --------------------------------------------------------------------------------------
    read colortable file 

    -------------------------------------------------------------------------------------- */
int
set_colortable(window_number)
int	window_number;
{
  twoD_opt_win_objects    *ip = twoD_ip[window_number]->twoD_opt_win;
  char                    fname[SIZE_OF_DIR_PLUS_FNAME];
  int			cmap_size, i, number_of_entries, index;

  /* get directory */
  strcpy(fname, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Dir);
  strcat(fname, "/");
  strcat(fname, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_File);
  /* check if file exists */
  if (!check_file_to_read(fname))
    {
     error_notice(ip->pan, "Invalid file or path or colormap cannot be read");
/*     if( TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors > 0 ) return;       mrm */
    }
  cmap_size = TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors;
  number_of_entries = ld_colortable(fname, window_number);
  TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors = number_of_entries;
  for (i=0; i<number_of_entries; i++)
    {
     index = TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[i];
     if(index >= cmap_size-1) 
        TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[i] = 
	  (int) fmod( (double) index, (double) cmap_size); 
    }
}


 
/* -----------------------------------------------------------------------
   load a colormap from a vector file

   ----------------------------------------------------------------------- */
int
ld_colormap(sys_color_filename, traj_color_filename, red, green, blue, number_sys_colors, number_traj_colors) 
char *sys_color_filename, *traj_color_filename;
int *red, *green, *blue, *number_sys_colors, *number_traj_colors;
{
  FILE 		*fp;
  int 		i, number_colors;
  int		get_dstool_path();
  static int    n_def_sys_co = 6, n_def_traj_co = 4;
  static int	def_sys_red[6] = {255, 0, 240, 255, 0, 0};
  static int	def_sys_green[6] = {255, 0, 250, 0, 255, 0};
  static int	def_sys_blue[6] = {255, 0, 255, 0, 0, 255}; 
  static int	def_traj_red[5] =   {255, 0, 0, 0};
  static int	def_traj_green[5] = {0, 255, 0, 0};
  static int	def_traj_blue[5] =  {0, 0, 255, 0}; 
  static char	dirname[SIZE_OF_DIR_PLUS_FNAME],
		main_dirname[SIZE_OF_DIR_PLUS_FNAME],
		fname[SIZE_OF_DIR_PLUS_FNAME];
 
  if( get_dstool_path( dirname, DSTOOL_COLOR_DIR ))
     {
      strcpy( fname, dirname ); strcat( fname, "/" ); strcat( fname, sys_color_filename);
      fp = fopen( fname, "r" );
      if (fp == NULL) 
        { /* try $DSTOOL/colormaps */
	 fprintf(stderr,"ld_colormap: $DSTOOL_COLOR_DIR/colormaps/%s not found.\nChecking $DSTOOL/colormaps/%s now.\n",sys_color_filename,sys_color_filename);
	 get_dstool_path( main_dirname, DSTOOL_DIR );
	 strcpy(dirname,main_dirname); strcat(dirname, "/colormaps");
	 strcpy( fname, dirname ); strcat( fname, "/" ); strcat( fname, sys_color_filename);
         fp = fopen( fname, "r" );
        }
      if (fp==NULL)  
	{
         fprintf(stderr,"ld_colormap: can't open colormap file. \n");
         return(-1);
        }
      fscanf(fp,"%d",&number_colors);		/* read system colormap size and values */
      for(i=0;i<number_colors;i++)
	  if(fscanf(fp, "%d %d %d", &red[i], &green[i], &blue[i]) == EOF) break; 
      fclose(fp);
      *number_sys_colors = number_colors;

      strcpy( fname, dirname ); strcat( fname, "/" ); strcat( fname, traj_color_filename);
      fp = fopen( fname, "r" );
      if (fp==NULL)  
	{
         fprintf(stderr,"traj color colormap: can't open colormap file \n");
         return(-1);
        }
 
      fscanf(fp,"%d",&number_colors);		/* read traj colormap size and values */
      for(i= *number_sys_colors;i<number_colors + *number_sys_colors;i++)
         if(fscanf(fp, "%d %d %d", &red[i], &green[i], &blue[i]) == EOF) break;  
      fclose(fp);

      *number_traj_colors = number_colors;
     }
  else
     {
      for(i=0; i<n_def_sys_co; i++)
	{
	 red[i] = def_sys_red[i];
	 green[i] = def_sys_green[i];
	 blue[i] = def_sys_blue[i]; 
        }
      for(i=0; i<n_def_traj_co; i++)
	{
	 red[i + n_def_sys_co] = def_traj_red[i];
	 green[i + n_def_sys_co] = def_traj_green[i];
	 blue[i + n_def_sys_co] = def_traj_blue[i]; 
        }
      *number_traj_colors = n_def_traj_co;
      *number_sys_colors = n_def_sys_co;
     }

  return(*number_sys_colors + *number_traj_colors);
}


/* -----------------------------------------------------------------------

   Load a colortable from file filename and store it in 
   TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable.

   ----------------------------------------------------------------------- */
int
ld_colortable(filename, window_number)   
char *filename;
int window_number;
{
  FILE          *fp;
  int           i, number_entries, index;
  static int    n_def_traj_co = 4;
 
  if (check_file_to_read(filename))
    {
     fp = fopen( filename, "r" );
     fscanf(fp,"%d",&number_entries);		/* read colortable size and values */
     for(i=0;i<number_entries;i++)
        {
	 if(fscanf(fp, "%d", &index) == EOF) break; 
         TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[i] = index;
	}
     fclose(fp);
    }
  else
    {
     number_entries = n_def_traj_co; 
     for(i=0; i<n_def_traj_co; i++)
       TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[i] = i;
    }
  return(number_entries);
}



/* --------------------------------------------------------------------------------------
   proc returns TRUE if the display hardware supports color graphics, FALSE otherwise

   -------------------------------------------------------------------------------------- */
int
get_display_type(window_number)
int	window_number;
{
  Display		*dpy;
  Xv_window       paint_window;

  paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
  dpy = (Display *)xv_get(paint_window, XV_DISPLAY);

  if( DefaultDepth(dpy, DefaultScreen(dpy)) < 2) return(FALSE);
  return(TRUE);
}

/* --------------------------------------------------------------------------------------
   proc to set dstool for monochrome graphics

   -------------------------------------------------------------------------------------- */

Cms
set_nocolor(window_number, number_sys_colors, number_traj_colors)
int	window_number;
int     *number_sys_colors, *number_traj_colors;
{
  static int	first_time=TRUE;
  int		i;
  Xv_singlecolor	*colors;
  unsigned long	*color_ptr;
  static	Cms	new_cms;


  if(first_time == TRUE)
    {colors = (Xv_singlecolor *) calloc(*number_sys_colors+1,sizeof(Xv_singlecolor));
     colors[0].red = colors[0].green = colors[0].blue = 255;
     for(i=1;i<*number_sys_colors;i++)
       colors[i].red = colors[i].green = colors[i].blue = 0;
     colors[*number_sys_colors].red = colors[*number_sys_colors].green = colors[*number_sys_colors].blue = 0;
     new_cms = (Cms) xv_create( NULL, CMS,
			       CMS_SIZE, *number_sys_colors+1, CMS_COLORS, colors, NULL);  }

  xv_set( (Canvas) get_twoD_handle(window_number,CANVAS_HANDLE), WIN_CMS, new_cms, NULL);
  xv_set( (Canvas) get_twoD_handle(window_number,CBAR_CANVAS_HANDLE), WIN_CMS, new_cms, NULL); 
  color_ptr = (unsigned long *) xv_get(new_cms, CMS_INDEX_TABLE);
  TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors = color_ptr;
  TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors = color_ptr + *number_sys_colors;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors = 1;
  TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[0] = 0;
  pm( PUT, "Color.Traj_Colormap_Size", 1, NULL);
  *number_traj_colors = 1;

  hide_colorbar(window_number); 
  color_opt_off(window_number);

  return ( (Cms) new_cms );
}

/*  --------------------------------------------------------------------------------
    proc used to hide the colorbar and scrolling buttons on a prescribed 2D window

    -------------------------------------------------------------------------------- */

hide_colorbar(window_number)
int	window_number;
{
  xv_set( (Canvas) get_twoD_handle(window_number,CBAR_CANVAS_HANDLE), WIN_SHOW, FALSE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_LT_HANDLE), PANEL_SHOW_ITEM, FALSE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_RT_HANDLE), PANEL_SHOW_ITEM, FALSE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_LT_PAN_HANDLE), PANEL_SHOW_ITEM, FALSE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_RT_PAN_HANDLE), PANEL_SHOW_ITEM, FALSE, NULL);
  xv_set( (Canvas) get_twoD_handle(window_number,CANVAS_HANDLE), XV_Y, 63, NULL);
  window_fit_height( (Xv_opaque) get_twoD_handle(window_number, PANEL_HANDLE) );
  window_fit_height( (Xv_opaque) get_twoD_handle(window_number, WINDOW_HANDLE) );
}



/*  --------------------------------------------------------------------------------
    proc used to show the colorbar and scrolling buttons on a prescribed 2D window

    -------------------------------------------------------------------------------- */
show_colorbar(window_number)
int	window_number;
{
  int	get_display_type();

  xv_set( (Canvas) get_twoD_handle(window_number,CBAR_CANVAS_HANDLE), WIN_SHOW, TRUE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_LT_HANDLE), PANEL_SHOW_ITEM, TRUE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_RT_HANDLE), PANEL_SHOW_ITEM, TRUE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_LT_PAN_HANDLE), PANEL_SHOW_ITEM, TRUE, NULL);
  xv_set( (Xv_opaque) get_twoD_handle(window_number,CBAR_RT_PAN_HANDLE), PANEL_SHOW_ITEM, TRUE, NULL);
  xv_set( (Canvas) get_twoD_handle(window_number,CANVAS_HANDLE), XV_Y, 88, NULL);
  window_fit_height( (Xv_opaque) get_twoD_handle(window_number, PANEL_HANDLE) );
  window_fit_height( (Xv_opaque) get_twoD_handle(window_number, WINDOW_HANDLE) );
}


point_to_ic(win,absc,ordin)
int win;
double absc,ordin;
{
  int index;
	
  index = (int) TwoD_Ds[win]->TwoD_Win_Ds->Active_Ver_Index;
  switch ( (int) TwoD_Ds[win]->TwoD_Win_Ds->Active_Ver_Type )
    {case PHASE_SPACE_VARB:
       pm( PUT, "Selected.Varb_Ic", index, ordin, NULL);
       break;
     case PARAMETER_VARB:
       pm( PUT, "Selected.Param_Ic", index , ordin, NULL);
       break; 
     case FUNCTION_VARB:
       break; }
	  
  index = (int) TwoD_Ds[win]->TwoD_Win_Ds->Active_Hor_Index;
  switch ( (int) TwoD_Ds[win]->TwoD_Win_Ds->Active_Hor_Type )
    {case PHASE_SPACE_VARB:
       pm( PUT, "Selected.Varb_Ic", index, absc, NULL);
       break;
     case PARAMETER_VARB:
       pm( PUT, "Selected.Param_Ic", index , absc, NULL);
       break;    
     case FUNCTION_VARB:
       break; }  
}



/* ----------------------------------------------------------------------------------
   returns the number of parameters active for display on this window

   ---------------------------------------------------------------------------------- */

int  
get_num_param(window_number)
int             window_number;
{
  int		count;

  count = 0;
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == PARAMETER_VARB)
    count = 1;
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == PARAMETER_VARB)
    count = count + 1;
  return(count);
}


/* ----------------------------------------------------------------------------------
   returns the number of functions active for display on this window
 
   ---------------------------------------------------------------------------------- */
 
int  
get_num_func(window_number)
int             window_number;
{
  int             count=0;    
 
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type == FUNCTION_VARB)
    count += 1;
  if( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type == FUNCTION_VARB)
    count += 1;
  return(count);
}

/*  Boolean function returning TRUE if a memory object of type
    mem_obj_type should be displayed in window number window_id. */
int
valid_mem_to_win( mem_obj_type, window_id )
int	mem_obj_type, window_id;
{
  extern int pm_entry_exists();
  int	num_active_param,plot_type;

  num_active_param = get_num_param(window_id);
  switch (mem_obj_type)
    {
    case PARAM_MEMORY:
      if (num_active_param == 2) return(TRUE);
      break;
    case TRAJ_MEMORY:
      if ((num_active_param == 0) || (num_active_param == 1)) return(TRUE);
      break; 
    case MULT_MEMORY:
      return(TRUE);
      break; 
    case FIXPT_MEMORY:
      if (num_active_param == 0) return(TRUE);
      break;
    case SEL_PT_MEMORY: 
      return(TRUE);
      break;
    case CONT_MEMORY:		/* fjw 9/28/92 */
      /* fjw; ith bit on ==> want to plot to windows with i params;
	 eg, 0101 ==> plot to windows with 0 or 2 params */
      if (pm_entry_exists("Cont.Plot_Type") != NULL)
	/* continutation window is active */
	plot_type = *((int *)pm(GET, "Cont.Plot_Type", NULL));  
      else
	plot_type = -1;
      if (plot_type < 0 || plot_type > 7)  /* field not initialized?; default = plot to all win */
	return(TRUE);
      else
	return(plot_type & ((int) pow( 2., (double) num_active_param)) );
      break; 
    }
  return(FALSE);
}


/*   Display a memory object (if valid) in all windows. */
mem_all_win( mem_obj )
     memory	mem_obj;
{
  int	i;
  int	mem_obj_type = memory_get_type( mem_obj );
  
  for(i=get_min_twoD();i<=get_max_twoD();i++)
    {
      if( valid_twoD_id(i)  &&
	 valid_mem_to_win(mem_obj_type, i) ) mem_to_win_id( mem_obj, i );
    } 
}


/* Refresh all windows (including associated popups) associated with window number window_id  */
int
refresh_win_id( window_id)
int	window_id;
{
  if(!valid_twoD_id(window_id)) return(-1);
  clear_canvas( window_id ); 
  paint_win_id( window_id );
  return(0);
}

/* Refresh all windows (including associated popups). */
int
refresh_all_win()
{
  int	i;

  for(i=get_min_twoD();i<=get_max_twoD();i++)
    if( valid_twoD_id(i) ) refresh_win_id( i );
}

/* Display all memory objects valid for window_number. */
int
paint_win_id( window_id)
int	window_id;
{
  memory	mem_obj_ptr;

  if(!valid_twoD_id(window_id)) return(-1);

  if( mem_obj_ptr = (memory) pm(GET, "Memory.Traj", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
  if( mem_obj_ptr = (memory) pm(GET, "Memory.Mult", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
  if( mem_obj_ptr = (memory) pm(GET, "Memory.Fixed", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
  if( mem_obj_ptr = (memory) pm(GET, "Memory.Cont", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
  if( mem_obj_ptr = (memory) pm(GET, "Memory.Sel_Pt", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
  if( mem_obj_ptr = (memory) pm(GET, "Memory.Param", NULL) )
    mem_to_win_id( mem_obj_ptr, window_id );
return(0);
}


/*  Display memory object in window number window_id. */
int
mem_to_win_id( mem_obj, window_id)      /*   PAW - changed for memory object */
memory	mem_obj;
int	window_id;
{
  int valid_twoD_id();
  double x, y;
  double *points, *params, *dvector();
  int mem_obj_type = memory_get_type( mem_obj );
  int func_dim, *color;
  struct  Filter_DataS     filter_cntl;


  if ( !valid_twoD_id( window_id ) || !valid_mem_to_win( mem_obj_type, window_id) ) return(-1);

  filter_cntl.function = (int (*)()) pm(GET, "Model.Aux_Function", NULL);
  filter_cntl.window_id = window_id;
  reset_win_dim(window_id);

  func_dim = *((int *)pm(GET,"Model.Funct_Dim",NULL));
  filter_cntl.func = dvector(0, func_dim-1);

  if (memory_reset_read(mem_obj) == 0) 
    {
      while (memory_read_next_flow(mem_obj, NULL, NULL, NULL, NULL, NULL) == 0)  
	{
	  while (memory_read_next_traj(mem_obj, NULL, NULL, NULL) == 0)
	    {
	      while (memory_read_next_point(mem_obj, &points, &params, &color, 
					    NULL, NULL) == 0)
		{
		  switch ( (int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Hor_Type)
		    {
		    case PHASE_SPACE_VARB:
		      x = points[(int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Hor_Index];
		      break;
		    case PARAMETER_VARB:
		      x = params[(int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Hor_Index];
		      break;
		    case FUNCTION_VARB:
		      filter_cntl.function(filter_cntl.func, points,params);
		      x = filter_cntl.func[TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Hor_Index];
		      break;
		    }
		  switch ( (int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Ver_Type)
		    {
		    case PHASE_SPACE_VARB:
		      y = points[(int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Ver_Index];
		      break;
		    case PARAMETER_VARB:
		      y = params[(int) TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Ver_Index];
		      break;
		    case FUNCTION_VARB:
		      filter_cntl.function(filter_cntl.func, points,params);
		      y = filter_cntl.func[TwoD_Ds[window_id]->TwoD_Win_Ds->Active_Ver_Index];
		      break;
		    }
		  filter_cntl.state = points;
		  filter_cntl.parameters = params;
		  filter_cntl.alt_color_index = color[0];
		  filter_cntl.pick_color_index = color[1];
		  if( color[0]<0 )
		    plot_symbol( window_id, x, y, -color[1], color[2] );
		  else
		    plot_symbol( window_id, x, y, (int) get_color(&filter_cntl), color[2] );
		} 
	    }
	}
    }
  free_dvector(filter_cntl.func,0,func_dim-1);
return(0);
}

/* Plot n points at coordinates (x[i],y[i]) to window number window_number using
  color and symbol type. */
int
list_to_win_id( window_id, x, y, n, color, symbol_type )
int	window_id;
double	*x, *y;
int	color, symbol_type, n;
{
  int     i, valid_twoD_id();

  if (!valid_twoD_id( window_id )) return(-1);


  for (i=0; i<n; i++)  
    plot_symbol( window_id, x[i], y[i], color, symbol_type );
  return(0);
}

/* Reset the window viewport to its defaults  */
reset_win_dim(window_number)
int     window_number;
{
  int     		valid_twoD_id();
  int                     width, height;
  Xv_window               paint_window;
 
  if (!valid_twoD_id( window_number )) return(-1);
 
  paint_window = (Xv_window) get_twoD_handle(window_number,CANVAS_PW_HANDLE);
 
  width = (int) xv_get(paint_window, XV_WIDTH);
  height = (int) xv_get(paint_window, XV_HEIGHT);
  TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X = 0;
  TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y = 0;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max = width;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max = height;
  return(0);
}



/*
 *  Update all vaild colorbars by window number
*/

paint_all_cbars()
{
  int	i;

  for(i=get_min_twoD();i<=get_max_twoD();i++)
    if( valid_twoD_id(i) ) paint_cbar_win_id( i );
}

clear_cbar( window_number )
int	window_number;
{
  Xv_window       paint_window;

  paint_window = (Xv_window) get_twoD_handle(window_number,CBAR_PW_HANDLE);
  XClearWindow( (Display *) xv_get(paint_window, XV_DISPLAY), (Window) xv_get(paint_window,XV_XID));
}


/*
 *  Repaint proc with window number argument  
 */
paint_cbar_win_id(window_number)
int	window_number;
{
  int width, height, i, xstart,ystart,num_cells,color_count, max_num_cells;
  int     get_twoD_number(), index, get_alt_color();
  Display *display;
  Window  cbar_xid;
  Xv_window       paint_window;

  if(!valid_twoD_id(window_number) || !TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors ||
     !TwoD_Ds[window_number]) return;
  if(get_display_type(window_number) == FALSE) return;

  paint_window = (Xv_window) get_twoD_handle(window_number,CBAR_PW_HANDLE);
  display = (Display *)xv_get(paint_window, XV_DISPLAY);
  width = (int) xv_get(paint_window, XV_WIDTH);
  max_num_cells = (int) floor( ((double) width)/( (double) (CBAR_CELL_WIDTH+1)) );
  if(max_num_cells > TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors)
      num_cells = TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors;
  else
      num_cells = max_num_cells;
  height = (int) xv_get(paint_window, XV_HEIGHT);
  if( num_cells >= max_num_cells - 4  && TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index != 2)
    num_cells -= 4;
  xstart = ystart = 0;
  cbar_xid = (Window) xv_get(paint_window, XV_XID);
  color_count = (int) TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index;

  for(i=0;i<num_cells;i++)
    {
      index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[
		(int) TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[color_count] ];
      XSetForeground(display,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc, index);

      XFillRectangle(display,cbar_xid,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc,
		     xstart,ystart,CBAR_CELL_WIDTH,height);
      xstart = xstart + CBAR_CELL_WIDTH + 1;
      color_count = color_count + 1;
      if(color_count>=(int) TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors)
	color_count = 0;
    }

  if( max_num_cells - 4 > TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors)
    xstart = width - 4*CBAR_CELL_WIDTH - 1; 

  if( TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index != 2)
    {if( TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index == 0)
       index = get_alt_color() + 1;
    else if( TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index == 1)
      index = *((int *) pm( GET, "Color.Pick_Color_Choice", NULL));
       index = TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Traj_Colors[
								     (int) TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable[index] ];
       XSetForeground(display,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc, 0 );
       XFillRectangle(display,cbar_xid,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc,
		      xstart,ystart,(width-xstart),height);
       XSetForeground(display,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc, index ); 
       XFillRectangle(display,cbar_xid,TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Gc,
		      xstart+3,ystart+3,(width-xstart-6),height-6);
     }
 	    
}






/*--------------------------------------------------------------
 * Function:    zoom_box
 * Description: Allows user to zoom in/out graphically
 */
void
zoom_box(win, event, arg)
     Xv_window win;
     Event *event;
     Notify_arg arg;
{
  extern void do_zoom(), draw_box();
  twoD_win_objects      *ip = (twoD_win_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
  int zoom_type, window_number, mult_type;
  static int mult_flag;		/* is this a mult box? */
  static int x1, y1, x2, y2;	/* corners of box */
  static char drawing_on;	/* currently drawing box ? */


  switch(event_action(event)) {
  case ACTION_SELECT:
    if (event_is_down(event) && (event_shift_is_down(event) || event_ctrl_is_down(event))) {
      /* record whether this is resizing or mult_conditions */
      if (event_shift_is_down(event)) mult_flag = 0;
      else mult_flag = 1;
      /* Start drawing */
      drawing_on = 1; 
      x2 = x1 = event_x(event);
      y2 = y1 = event_y(event);
      draw_box(win, x1, y1, x2, y2);
    }
    else if (event_is_up(event) && (drawing_on)) {
      /* Drawing finished-- do the zoom */
      if ((abs(x2-x1) < 6) && (abs(y2-y1) < 6))
	draw_box(win, x1, y1, x2, y2);
      else if (!mult_flag) 
	{
	  /* ZOOM, send a notice to get direction */
	  zoom_type = notice_prompt(
				    win, NULL,
				    NOTICE_NO_BEEPING, TRUE,
				    NOTICE_MESSAGE_STRINGS, 
				    "Choose a direction to zoom.", NULL,
				    NOTICE_FOCUS_XY, x2, y2,
				    NOTICE_BUTTON_YES, "Zoom in",
				    NOTICE_BUTTON,     "Zoom out", 2,
				    NOTICE_BUTTON_NO,  "Cancel", 
				    NULL);
	  draw_box(win, x1, y1, x2, y2);
	  if (zoom_type != 0) 
	    {
	      window_number = get_twoD_number(ip);
	      do_zoom(window_number, zoom_type, x1, y1, x2, y2);  
	    }
	}
      else
	{
	  /* do MULT */
	  mult_type = notice_prompt(win, NULL,
				    NOTICE_NO_BEEPING, TRUE,
				    NOTICE_MESSAGE_STRINGS,
				    "Choose MULTIPLE region option.",NULL,
				    NOTICE_FOCUS_XY, x2,y2,
				    NOTICE_BUTTON_YES, "Forwards",  /* PAW */
				    NOTICE_BUTTON, "Backwards", 2,
				    NOTICE_BUTTON_NO, "Cancel",
				    NULL);
	  draw_box(win, x1, y1, x2, y2);
	  if (mult_type != 0) 
	    {
	      window_number = get_twoD_number(ip);
	      if (window_number != -1)
		do_mult(window_number, mult_type, x1, y1, x2, y2);
	    }
	}
      drawing_on = 0;		/* finished drawing box */
    }
    break;
  case LOC_DRAG:
    if (drawing_on) {
      draw_box(win, x1, y1, x2, y2);
      x2 = event_x(event);
      y2 = event_y(event);
      draw_box(win, x1, y1, x2, y2);
    }
    break;
  }
}

void
draw_box(win, x1, y1, x2, y2)
Xv_window win;
int	x1, x2, y1, y2;
{
  Display *display = (Display *)xv_get(win, XV_DISPLAY);
  XID paint_xid = (XID)xv_get(win, XV_XID);
  twoD_win_objects      *ip = (twoD_win_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
  int window_number = get_twoD_number(ip);
  GC gc = TwoD_Ds[window_number]->TwoD_Win_Ds->Canvas_Gc; 
  int	hold;
  unsigned long get_default_foreground();

  if(x1>x2 && y1>y2)
    {
      hold = x1; x1 = x2; x2 = hold;
      hold = y1; y1 = y2; y2 = hold;
    }
  else if(x2>x1 && y1>y2)
    {
      hold = y1; y1 = y2; y2 = hold;		
    }
  else if(x1>x2 && y2>y1)
    {
      hold = x1; x1 = x2; x2 = hold;
    }

/*  XSetForeground(display, gc, 3);*/
  XSetForeground(display, gc, get_default_foreground(window_number));
  XSetFunction(display, gc, GXxor); /* Set logical func = XOR (double draw = erase) */
  XDrawRectangle(display,paint_xid,gc,x1,y1,(x2-x1),(y2-y1));
  XSetFunction(display, gc, GXcopy);
}


/*--------------------------------------------------------------
 * Function:    do_zoom
 * Description: Calculates new viewing bndries to get desired
 *              zoom effect.
 * Args in:     win;  xv paint window handle
 *              direcn;  1=zoom in, else out
 *              x1,y1;  one corner of zoom box on canvas
 *              x2,y2;  diagonally opposite corner
 */ 
void 
do_zoom(window_number, direcn, x1, y1, x2, y2)
     int direcn, window_number;
     int x1, y1, x2, y2;
{
  int px_umax, px_umin, px_vmax, px_vmin;
 
  double pxtorx(), pytory();
  double new_xmin, new_ymin, new_xmax, new_ymax;
  double umin, umax, vmin, vmax;

  px_umax = (x2 > x1) ?  x2 : x1;
  umax = pxtorx(  px_umax,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  px_umin = (x2 > x1) ?  x1 : x2;
  umin = pxtorx(  px_umin,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  px_vmax = (y2 > y1) ?  y2 : y1;
  vmin = pytory(  px_vmax,	/* remember: screen coordinates are */
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min, /* inverted!!			    */
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
  px_vmin = (y2 > y1) ?  y1 : y2;
  vmax = pytory(  px_vmin,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
		TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
		TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);
     
  if (direcn == 1) {		/* Zoom in */
    TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max = umax;
    TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min = umin;
    TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max = vmax;
    TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min = vmin;
  }
  else {			/* Zoom out */
    new_xmax = (TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max*
		(TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max-TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min-umin) + 
		TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min*umax)/(umax-umin);
    new_xmin = (TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min*
		(TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max-TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min) -
		(umin-TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min)*new_xmax)/(TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max-umin);
    TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max = new_xmax;
    TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min = new_xmin;
    new_ymax = (TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max*
		(TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max-TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min-vmin) + 
		TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min*vmax)/(vmax-vmin);
    new_ymin = (TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min*
		(TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max-TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min) -
		(vmin-TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min)*new_ymax)/(TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max-vmin);
    TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max = new_ymax;
    TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min = new_ymin;
  }
  twoD_data_refresh(window_number);
  refresh_win_id(window_number);
}
/*
  Return the pixel value which when xored with the current
  background color, gives the next color in the system colormap.
*/
unsigned long
    get_default_foreground(window_number)

int
    window_number;

{
    return 
	(unsigned long)
	    (TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index + 1] ^
	     TwoD_Ds[window_number]->TwoD_Win_Ds->TwoD_Sys_Colors[TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index]);
}
								
