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
 * load.c - Notify and event callback functions.
 */
#include <stdio.h>
#include <math.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/notice.h>
#include <gdd.h>

#include <ui_init.h>
#include <constants.h>
#include <pm.h>
#include <saveload.h>
#include <twoD.h>
#include <twoD_opt.h>
#include <memory.h>
#include <modellib.h>
#include "load_ui.h"

static load_win_objects *load_ip = NULL;

/*
 * Menu handler for `filemenu (Load ...)'.
 */
Menu_item
load_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    load_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Load",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		load_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		load_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  if(op == MENU_NOTIFY)
    load_open(DEFAULT_WIN_CONFIG,0,0,0,0);
  return item;
}


/* 
 * load_open()  displays the load window, creating it if necessary.
 */
int
  load_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  
  if (!load_ip)
    {
      load_ip = load_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Load",load_ip->win,POPUP_WINDOW);
      pm(PUT_SAVABLE,"Win_Names.Load",SAVE_NONE,
	 NULL);
      load_field_manager();	/* calls load_init() */
    }
  if (use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(load_ip->win,rect); /* get the current configuration */
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if (width>0) rect->r_width = (short) width;
      if (height>0) rect->r_height = (short) height;
      frame_set_rect(load_ip->win,rect); /* set the current configuration */
      free(rect);
    }
  load_data_refresh();
  mark_window_open("Load");
  xv_set(load_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(load_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}

int
load_close()
{
    mark_window_closed("Load");
    if(load_ip) {
	xv_set(load_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(load_ip->win, XV_SHOW, FALSE, NULL);	
    }
}

/*
 * load_data_refresh() uses postmaster info to update load window
 */
load_data_refresh()
{
  load_win_objects *ip = load_ip;
  char		strng[SIZE_OF_DIR_PLUS_FNAME];
  int		i, v_dim, p_dim, *v_box, *p_box, *ivector();

  if (!ip ) return; 

  pm(GET, "Load.Directory", strng, NULL);
  xv_set(ip->directory, PANEL_VALUE, strng, NULL);
  pm(GET, "Load.Filename", strng, NULL);
  xv_set(ip->filename, PANEL_VALUE, strng, NULL);
  pm(GET, "Load.Data_Type", strng, NULL);
  if (strcmp(strng,"Memory.Traj"))
    xv_set(ip->type, PANEL_VALUE, 0, NULL);
  else if (strcmp(strng,"Memory.Fixed"))
    xv_set(ip->type, PANEL_VALUE, 1, NULL);
  else if (strcmp(strng,"Memory.Cont"))
    xv_set(ip->type, PANEL_VALUE, 2, NULL);
  else if (strcmp(strng,"Memory.Param"))
    xv_set(ip->type, PANEL_VALUE, 3, NULL);
  else if (strcmp(strng,"Memory.Sel_Pt"))
    xv_set(ip->type, PANEL_VALUE, 4, NULL);
  else xv_set(ip->type, PANEL_VALUE, 0, NULL);

  xv_set(ip->color,PANEL_VALUE, *((int *)pm(GET, "Load.Color_Flag", NULL))? 0:1, NULL);
  xv_set(ip->symbol,PANEL_VALUE, *((int *)pm(GET, "Load.Symbol_Flag", NULL))? 0:1, NULL);

  v_dim = *((int *)pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *)pm(GET, "Model.Param_Dim", NULL));
  v_box = ivector(0,v_dim-1); 
  p_box = ivector(0,p_dim-1); 
  pm(GET_LIST, "Load.Varb_Index", 0, v_dim-1, v_box, NULL);
  pm(GET_LIST, "Load.Param_Index", 0, p_dim-1, p_box, NULL);
  xv_set(ip->varb, XV_SHOW, FALSE, NULL); /* so that refresh doesn't scroll the list */
  for(i=v_dim-1; i>=0; i--)	
    xv_set(ip->varb,PANEL_LIST_SELECT, i, v_box[i], NULL);
  xv_set(ip->varb, XV_SHOW, TRUE, NULL); 
  xv_set(ip->param, XV_SHOW, FALSE, NULL); /* so that refresh doesn't scroll the list */
  for(i=p_dim-1; i>=0; i--)
    xv_set(ip->param,PANEL_LIST_SELECT, i, p_box[i], NULL);
  xv_set(ip->param, XV_SHOW, TRUE, NULL);

  i = *((int *)pm(GET, "Load.Format_Flag", NULL));
  xv_set(ip->option,PANEL_VALUE, i, NULL);

  if (i<2) 
    {
      xv_set(ip->win, XV_HEIGHT, 175, NULL);     /* hide bottom of panel */
      xv_set(ip->color,  PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->symbol, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->type,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->varb,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->param,  PANEL_INACTIVE, TRUE, NULL);
    }
  else 
    {
      xv_set(ip->color,  PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->symbol, PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->type,   PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->varb,   PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->param,  PANEL_INACTIVE, FALSE, NULL);
      window_fit_height(ip->pan);          
      window_fit_height(ip->win);
    }
  free_ivector(v_box, 0, v_dim-1);
  free_ivector(p_box, 0, p_dim-1);
}

/*
 * load_read_window() reads info from the load window into the postmaster
 */
load_read_window()
{
  load_win_objects *ip = load_ip;
  char *strng;
  int	temp, i, v_dim, p_dim, *v_box, *p_box, *ivector();

  if (!ip) return;

  strng = (char *) xv_get(ip->directory, PANEL_VALUE);		       /* Directory Name */
  pm(PUT, "Load.Directory", strng, NULL);

  strng = (char *) xv_get(ip->filename, PANEL_VALUE);		       /* Filename */
  pm(PUT, "Load.Filename", strng, NULL);

  temp = (int) xv_get(ip->option, PANEL_VALUE);
  pm(PUT, "Load.Format_Flag", temp, NULL);

  temp = (int) xv_get(ip->type, PANEL_VALUE);
  switch( temp )
    {
    case 0:
      pm(PUT, "Load.Data_Type", "Memory.Traj", NULL);
      break;
    case 1:
      pm(PUT, "Load.Data_Type", "Memory.Fixed", NULL);
      break;
    case 2:
      pm(PUT, "Load.Data_Type", "Memory.Cont", NULL);
      break;
    case 3:
      pm(PUT, "Load.Data_Type", "Memory.Param", NULL);
      break;
    case 4:
      pm(PUT, "Load.Data_Type", "Memory.Sel_Pt", NULL);
      break;
    }

  temp = (int) xv_get(ip->color, PANEL_VALUE);
  pm(PUT, "Load.Color_Flag", temp? FALSE: TRUE, NULL);

  temp = (int) xv_get(ip->symbol, PANEL_VALUE);
  pm(PUT, "Load.Symbol_Flag", temp? FALSE: TRUE, NULL);

  v_dim = *((int *)pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *)pm(GET, "Model.Param_Dim", NULL));
  v_box = ivector(0,v_dim-1); 
  p_box = ivector(0,p_dim-1); 
  for(i=v_dim-1; i>=0; i--)	/* go backwards so that refresh doesn't scroll down the listg */
    v_box[i] = (int) xv_get(ip->varb, PANEL_LIST_SELECTED, i);
  for(i=p_dim-1; i>=0; i--)
    p_box[i] = (int) xv_get(ip->param, PANEL_LIST_SELECTED, i);
  pm(PUT_LIST, "Load.Varb_Index", 0, v_dim-1, v_box,
     PUT_LIST, "Load.Param_Index", 0, p_dim-1, p_box,
     NULL);
  free_ivector(v_box, 0, v_dim-1);
  free_ivector(p_box, 0, p_dim-1);
}

/*
 * load_field_manager() updates info on load window.  Modifies labels on buttons
 */
load_field_manager()
{
  int			i,v_dim,p_dim;
  char			name[MAX_LEN_VARB_NAME];
  static int		num_varb=0, num_param=0;
  load_win_objects  	*ip = load_ip;
  
  if (!ip) return;
  
  v_dim = *((int *) pm( GET, "Model.Varb_Dim", NULL ));	/* get new values */
  p_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  
  if ( num_varb > 0 ) 
    {						  /* destroy old objects */
      for (i=num_varb-1; i>=0; i--)
	xv_set(ip->varb, PANEL_LIST_DELETE, i, NULL);
      num_varb = 0;
    }
  if ( num_param > 0 ) 
    {
      for (i=num_param-1; i>=0; i--)
	xv_set(ip->param, PANEL_LIST_DELETE, i, NULL);
      num_param = 0;
    }

  for ( i=0; i<v_dim; i++) 
    {						  /* assign new labels */
      pm( GET, "Model.Varb_Names", i, name , NULL);
      xv_set(ip->varb, PANEL_LIST_INSERT, i, PANEL_LIST_STRING,  i, name,  NULL);
    }   
  num_varb = v_dim;
  for ( i=0; i<p_dim; i++) 
    {						  /* assign new labels */
      pm( GET, "Model.Param_Names", i, name , NULL);
      xv_set(ip->param, PANEL_LIST_INSERT, i, PANEL_LIST_STRING, i, name, NULL);
    }   
  num_param = p_dim;
}


/*
 * load_from_batch() enables the user to load a dstool format file without having
 * windows open.  It is used upon startup of dstool to load data from a configuration file.
 */
load_from_batch()
{
  char	*dir = (char *)calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));
  char	*test, *getcwd();
  char	*filename = (char *)calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));
  char  *strcat(), *strcpy();
  int   mode_flag = *((int *) pm(GET, "Control.Mode", NULL));

  /*  getwd(dir); */			/* may be system specific */
  getcwd(dir, SIZE_OF_DIR_PLUS_FNAME);

  if ( mode_flag == WINDOWS_MODE ) 
    {
      pm(GET, "Control.Infile", filename, NULL);
      test = (char *)calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));
      test = strcat(strcat(strcpy(test,dir), "/"), filename );
      if( check_file_to_read(test) ) /* Allow config file to not exist or to be unreadable. */
	{
	  pm(PUT, "Load.Directory", dir, /* set load filename = config filename based in CURRENT directory*/
	     PUT, "Load.Filename", filename, NULL);
	  load_driver(1);		/* display errors on cmd window (always created) */
	}
      free(test);
    }
  else
    {
      pm(GET, "Control.Infile", filename, NULL);
      pm(PUT, "Load.Directory", dir, /* set load filename = config filename based in CURRENT directory*/
	 PUT, "Load.Filename", filename, NULL);
      load_driver(-1);		/* display errors to stderr */
    }
  free(dir);
  free(filename);
}

/*
 * load_driver() calls load_go() and monitors errors.  Errors are displayed depending on mode:
 *    mode = 0  <==> display errors on load_win 
 *    mode > 0  <==> display errors on cmd_win 
 *    mode < 0  <==> write errors to stderr
 */
load_driver(mode)
int   mode;
{
  Panel		pan;
  int			status;
  int			keep_going=TRUE, force_varb=FALSE, force_param=FALSE;

  if (mode > 0)
    pan = (Panel) cmd_ip->pan;
  else if (mode == 0)
    pan = (Panel) load_ip->pan;

  while( keep_going && (status = load_go(&force_varb, &force_param)) )
    switch (status)
      {
      case -1:
	if(mode >= 0) 
	  error_notice(pan, "Cannot find or open file. Check directory and file names.");
	else
	  system_mess_proc(1, "Cannot find or open input file. Check directory and file names.");
	keep_going = FALSE;
	break;
      case -2:
	if(mode >= 0)
	  error_notice(pan, "Unformatted trajectory data cannot contain parameters.");
	keep_going = FALSE;
	break;
      case -3:
	if (mode >= 0)
	  error_notice(pan, "Loading not successful.  Cannot initialize memory!");
	else
	  system_mess_proc(1,"Loading not successful.  Cannot initialize memory!");
 	keep_going = FALSE;
	break;
      case -4:
	if (mode >= 0)
	  if ( !error_notice_option(pan, "Dimension of file variables different from current variable dimension", 
				    "Cancel Load","Try to Continue"))
	    {
	      force_varb = TRUE;
	      keep_going = TRUE;
	    }
	  else
	    keep_going = FALSE;
	else
	  {
	    system_mess_proc(1, "Dim of file variables different from current dim: Aborting load!");
	    keep_going = FALSE;
	  }
	break;
      case -5:
	if (mode >= 0)
	  if ( !error_notice_option(pan, "Dimension of file parameters different from current parameter dimension", 
				    "Cancel Load","Try to Continue"))
	    {
	      force_param = TRUE;
	      keep_going = TRUE;
	    }
	  else
	    keep_going = FALSE;
	else
	  {
	    system_mess_proc(1, "Dim of file parameters different from current dim: Aborting load!");
	    keep_going = FALSE;
	  }
	break;
      } 
  cmd_data_refresh();		/* update showing number of points */
  return;
}


/*
 * load_config()   loads dstool configuration data from specified file.  
 * This  routine essentially restores the windowing system to a previous state.
 */
load_config(fp)
FILE    *fp;
{
  char		label[SIZE_OF_GEN_INPUT],word[SIZE_OF_GEN_INPUT];
  Rect		*rect;
  int		window_number, wn, ijunk, djunk;
  int		saved_list_number; /* If the win nums are (say) 2,4,5 then the list_number's associated with them are 0,1,2 */
  int		v_dim, p_dim, f_dim, list_number = 0;  
  
  if( (*((int *) pm(GET, "Control.Mode", NULL))) != WINDOWS_MODE)
    return;

  v_dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *) pm(GET, "Model.Param_Dim", NULL));
  f_dim = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  rect = (Rect *)calloc(1,sizeof(Rect));
  fscanf(fp,"%s",label);
  fscanf(fp,"%hd %hd %hd %hd",
	 &(rect->r_left), &(rect->r_top), &(rect->r_width), &(rect->r_height));
  if (!strcmp(label,"Cmd_Win")) 
    cmd_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Orbit_Win"))
    orbit_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Periodic_Win"))
    periodic_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Selected_Win"))
    sel_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Function_Win"))
    funct_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Defaults_Win"))
    def_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"Mult_Win"))
    mult_open(SET_WIN_CONFIG,rect->r_left, rect->r_top, rect->r_width, rect->r_height);
  else if (!strcmp(label,"TwoD_Win"))		  /* handle twoD windows separately */
    {
      fscanf(fp, "%s %d", word, &saved_list_number);
      for ( wn=0; wn <=get_max_twoD(); wn++)
	{
	  if ( valid_twoD_id( wn ) )
	    {
	      if (list_number == saved_list_number)   /* an open window can be moved */
		{
		  window_number = twoD_open(wn,SET_WIN_CONFIG,rect->r_left,rect->r_top,rect->r_width,rect->r_height);
		}
	      list_number++;
	    }
	}				
      if (list_number <= saved_list_number)	      /* not enough windows; open one */
	{
	  window_number = get_max_twoD() + 1;
	  window_number = twoD_open(window_number,SET_WIN_CONFIG,rect->r_left,rect->r_top,rect->r_width,rect->r_height);
	}
      
      if (get_display_type(window_number)) /* this is a color display */
	{
	  fscanf(fp, "%s %d %s %d %s %d %s %d %s %d %s %d %s %lg %s %lg %s %s", 
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Show_Cmap),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Min),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Max),
		 word, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_File
		 );
	  if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index < v_dim)	/* correct for indexing - paw */
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PHASE_SPACE_VARB;
	  else if ( (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index -= v_dim) < p_dim)
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PARAMETER_VARB;
	  else if ( (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index -= p_dim) < f_dim)
	    TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = FUNCTION_VARB;
	  else
	    {
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index = 0;
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PHASE_SPACE_VARB;
	    }
	}		
      else	   /* this is a B/W display; don't change TwoD_Opt_Ds fields having to do with color */
	{
	  fscanf(fp, "%s %d %s %d %s %d %s %d %s %d %s %d %s %lg %s %lg %s %s", 
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index),
		 word, &(TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index),
		 word, &ijunk, word, &ijunk, word, &ijunk, word, &ijunk, word, &djunk,
		 word, &djunk, word, word );
	}
      
      fscanf(fp, "%s %lg %s %lg %s %lg %s %lg %s %d %s %d %s %d %s %d", 
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index),
	     word, &(TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index)
	     );
      twoD_opt_win_refresh( window_number );
      twoD_data_refresh( window_number );
      colortable_refresh( window_number );
      bg_color_refresh( window_number, TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index );
    }
  free(rect);
}

