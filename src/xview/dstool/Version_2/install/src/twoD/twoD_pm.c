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
#include <defaults.h>
#include "twoD_opt.h"
#include "twoD.h"
#include "twoD_ip.h"

static char *TWOD_OBJ_NAME = "TwoD_Win";

static char *TWOD_WIN[] = {
    "TwoD_Win.Window_Number","TwoD_Win.Locn","TwoD_Win.Symbol_Index",
    "TwoD_Win.Sym_Size_Index","TwoD_Win.Bg_Color_Index",
    "TwoD_Win.Show_Cmap","TwoD_Win.Cmap_Type_Index",
    "TwoD_Win.Depth_Coord","TwoD_Win.Depth_Coord_Min",
    "TwoD_Win.Depth_Coord_Max","TwoD_Win.Cmap_File",
    "TwoD_Win.Hor_Min","TwoD_Win.Hor_Max",
    "TwoD_Win.Vert_Min","TwoD_Win.Vert_Max",
    "TwoD_Win.Hor_Type","TwoD_Win.Ver_Type",
    "TwoD_Win.Hor_Index","TwoD_Win.Ver_Index",
    "TwoD_Win.Open_Current", "TwoD_Win.Close_Current",
    "TwoD_Win.Open_Corresponding",
    "TwoD_Win.Update_Current"
  };

typedef enum {
  WINDOW_NUMBER=0,TWOD_LOCN,SYMBOL_INDEX,
  SYM_SIZE_INDEX,BG_COLOR_INDEX,
  SHOW_CMAP,CMAP_TYPE_INDEX,
  DEPTH_COORD_INDEX,DEPTH_COORD_MIN,
  DEPTH_COORD_MAX,CMAP_FILE,
  HOR_MIN,HOR_MAX,
  VERT_MIN,VERT_MAX,
  HOR_TYPE,VER_TYPE,
  HOR_INDEX,VER_INDEX,
  OPEN_CURRENT, CLOSE_CURRENT,
  OPEN_CORRESPONDING,
  UPDATE_CURRENT
  } TWOD_WIN_t;

/*
  Create appropriate postmaster fields for loading twoD windows.
*/
void
    twoD_pm_install()
{
  int
      open_current_twoD_window(),
      open_corresponding_twoD_window(),
      close_current_twoD_window();
  void
      update_current_twoD_window();

  pm(CREATE_OBJ, TWOD_OBJ_NAME,
     CREATE_ELEM, TWOD_WIN[WINDOW_NUMBER], INT,
     PUT, TWOD_WIN[WINDOW_NUMBER], 0,
     CREATE_ELEM, TWOD_WIN[TWOD_LOCN], INT_LIST,
     INIT,TWOD_WIN[TWOD_LOCN],4,
     PUT,TWOD_WIN[TWOD_LOCN],0,NO_LOCATION,
     CREATE_ELEM, TWOD_WIN[SYMBOL_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[SYM_SIZE_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[BG_COLOR_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[SHOW_CMAP], INT,
     CREATE_ELEM, TWOD_WIN[CMAP_TYPE_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[DEPTH_COORD_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[DEPTH_COORD_MIN], DBL,
     CREATE_ELEM, TWOD_WIN[DEPTH_COORD_MAX], DBL,
     CREATE_ELEM, TWOD_WIN[CMAP_FILE], STRNG,
     INIT,TWOD_WIN[CMAP_FILE], MAX_LONG_STR,
     CREATE_ELEM, TWOD_WIN[HOR_MIN], DBL,
     CREATE_ELEM, TWOD_WIN[HOR_MAX], DBL,
     CREATE_ELEM, TWOD_WIN[VERT_MIN], DBL,
     CREATE_ELEM, TWOD_WIN[VERT_MAX], DBL,
     CREATE_ELEM, TWOD_WIN[HOR_TYPE], INT,
     CREATE_ELEM, TWOD_WIN[VER_TYPE], INT,
     CREATE_ELEM, TWOD_WIN[HOR_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[VER_INDEX], INT,
     CREATE_ELEM, TWOD_WIN[OPEN_CURRENT],FNCT,
     PUT, TWOD_WIN[OPEN_CURRENT],open_current_twoD_window,
     CREATE_ELEM, TWOD_WIN[CLOSE_CURRENT],FNCT,
     PUT, TWOD_WIN[CLOSE_CURRENT],close_current_twoD_window,
     CREATE_ELEM, TWOD_WIN[OPEN_CORRESPONDING],FNCT,
     PUT, TWOD_WIN[OPEN_CORRESPONDING],open_corresponding_twoD_window,
     CREATE_ELEM, TWOD_WIN[UPDATE_CURRENT],FNCT,
     PUT, TWOD_WIN[UPDATE_CURRENT], update_current_twoD_window,
     NULL);

}

/*
  Fill the twoD window postmaster fields based on
  default choices.
*/
int
    twoD_pm_from_defaults()
{

  int 
      dim = *((int *) pm(GET, "Model.Varb_Dim", NULL)),
      vert_coord_index = (dim > 1) ? 1 : 0,
      depth_coord_index = (dim > 2) ? 2 : 0;
  char 
      str[SIZE_OF_FNAME];

  pm(CREATE_OBJ, TWOD_OBJ_NAME,
     PUT, TWOD_WIN[SYMBOL_INDEX], 
     * ((int *)pm(GET,"Defaults.Symbol_Index",NULL)),
     PUT, TWOD_WIN[SYM_SIZE_INDEX],
     * ((int *)pm(GET,"Defaults.Size_Index",NULL)),
     PUT, TWOD_WIN[BG_COLOR_INDEX],
     * ((int *)pm(GET,"Defaults.Bgnd_Color",NULL)),
     PUT, TWOD_WIN[SHOW_CMAP],
     * ((int *)pm(GET,"Defaults.Show_Color",NULL)),
     PUT, TWOD_WIN[CMAP_TYPE_INDEX],
     * ((int *)pm(GET,"Defaults.Cmap_Type",NULL)),
     PUT, TWOD_WIN[DEPTH_COORD_INDEX],
	depth_coord_index,
     PUT, TWOD_WIN[DEPTH_COORD_MIN],
	* ((double *)pm(GET,"Defaults.Varb_Min", depth_coord_index, NULL)),
     PUT, TWOD_WIN[DEPTH_COORD_MAX],
	* ((double *)pm(GET,"Defaults.Varb_Max", depth_coord_index, NULL)),
     PUT, TWOD_WIN[CMAP_FILE],
	   (char *)pm(GET,"Defaults.Ctable_File",str,NULL),
     PUT, TWOD_WIN[HOR_MIN],
	   * ((double *)pm(GET,"Defaults.Varb_Min", 0, NULL)),
     PUT, TWOD_WIN[HOR_MAX],
	   * ((double *)pm(GET,"Defaults.Varb_Max", 0, NULL)),
     PUT, TWOD_WIN[VERT_MIN],
	   * ((double *)pm(GET,"Defaults.Varb_Min", vert_coord_index, NULL)),
     PUT, TWOD_WIN[VERT_MAX],
	* ((double *)pm(GET,"Defaults.Varb_Max", vert_coord_index, NULL)),
     PUT, TWOD_WIN[HOR_TYPE], PHASE_SPACE_VARB,
     PUT, TWOD_WIN[VER_TYPE], PHASE_SPACE_VARB,
     PUT, TWOD_WIN[HOR_INDEX], 0,
     PUT, TWOD_WIN[VER_INDEX], vert_coord_index,
/*     PUT, TWOD_WIN[OPEN_CURRENT],FNCT,
     PUT, TWOD_WIN[OPEN_CURRENT],open_current_twoD_window,*/
     NULL);

}

/*
  Postmaster reset procedure called when new model is loaded.
*/
void
    twoD_pm_reset()
{
    twoD_pm_from_defaults();
  
}

/*
  Fill the twoD structure for window number based on postmaster info.
*/
int
    twoD_struct_from_pm(window_number)
{
  int
      v_dim, p_dim, f_dim;
  
  if( (*((int *) pm(GET, "Control.Mode", NULL))) != WINDOWS_MODE)
      return;

  v_dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *) pm(GET, "Model.Param_Dim", NULL));
  f_dim = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  if (valid_twoD_id(window_number)) {
      if (get_display_type(window_number)) { /* this is a color display */
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index =
	      *((int *) pm(GET,TWOD_WIN[SYMBOL_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index =
	      *((int *) pm(GET,TWOD_WIN[SYM_SIZE_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Bg_Color_Index =
	      *((int *) pm(GET,TWOD_WIN[BG_COLOR_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Show_Cmap =
	      *((int *) pm(GET,TWOD_WIN[SHOW_CMAP],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_Type_Index =
	      *((int *) pm(GET,TWOD_WIN[CMAP_TYPE_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index =
	      *((int *) pm(GET,TWOD_WIN[DEPTH_COORD_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Min =
	      *((double *) pm(GET,TWOD_WIN[DEPTH_COORD_MIN],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Max =
	      *((double *) pm(GET,TWOD_WIN[DEPTH_COORD_MAX],NULL));
	  pm(GET,TWOD_WIN[CMAP_FILE],TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Cmap_File,NULL);
	  if (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index < v_dim)	/* correct for indexing - paw */
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PHASE_SPACE_VARB;
	  else if ( (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index -= v_dim) < p_dim)
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PARAMETER_VARB;
	  else if ( (TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index -= p_dim) < f_dim)
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = FUNCTION_VARB;
	  else {
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Depth_Coord_Index = 0;
	      TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Active_Depth_Type = PHASE_SPACE_VARB;
	  }
      }
      else {  /* this is a B/W display; don't change TwoD_Opt_Ds fields having to do with color */
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Symbol_Index =
	      *((int *) pm(GET,TWOD_WIN[SYMBOL_INDEX],NULL));
	  TwoD_Opt_Ds[window_number]->TwoD_Opt_Win_Ds->Sym_Size_Index =
	      *((int *) pm(GET,TWOD_WIN[SYM_SIZE_INDEX],NULL));
      }
      TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min = 
	  *((double *) pm(GET,TWOD_WIN[HOR_MIN],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max =
	  *((double *) pm(GET,TWOD_WIN[HOR_MAX],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min =
	  *((double *) pm(GET,TWOD_WIN[VERT_MIN],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max =
	  *((double *) pm(GET,TWOD_WIN[VERT_MAX],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type =
	  *((int *) pm(GET,TWOD_WIN[HOR_TYPE],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type =
	  *((int *) pm(GET,TWOD_WIN[VER_TYPE],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index =
	  *((int *) pm(GET,TWOD_WIN[HOR_INDEX],NULL));
      TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index =
	  *((int *) pm(GET,TWOD_WIN[VER_INDEX],NULL));
  }
}

/*
  Uses "TwoD_Win.Window_Number" pm entry to
  determine which window is current.
*/
int
    open_current_twoD_window()
{
    Menu_item	
	item = (Menu_item) NULL;
    Menu_generate	
	op = MENU_NOTIFY;

    Menu_item 
	twoD_handler();

    int
	status;
    
    twoD_handler(item,op);

    /* avoid accidental re-use */
    pm(PUT,TWOD_WIN[TWOD_LOCN],0,NO_LOCATION,NULL);
    
    return NO_ERROR;
}

/*
  Uses "TwoD_Win.Window_Number" pm entry to
  determine which window is current.
  Adjusts this entry translating it from an absolute
  index to an actual index in TwoD_Ds.
  (As was done in version 1.x of load code for
  twoD windows.)
*/
int
    open_corresponding_twoD_window()
{
    Menu_item	
	item = (Menu_item) NULL;
    Menu_generate
	op = MENU_NOTIFY;

    Menu_item 
	twoD_handler();

    int
	stored_window_number,
	status;
    
    stored_window_number = *((int *) pm(GET,"TwoD_Win.Window_Number",NULL));
    pm(PUT,"TwoD_Win.Window_Number",
       absolute_number_to_window_number(stored_window_number),
       NULL);
    twoD_handler(item,op);

    /* avoid accidental re-use */
    pm(PUT,TWOD_WIN[TWOD_LOCN],0,NO_LOCATION,NULL);
    
    return NO_ERROR;
}


int
    close_current_twoD_window()
{
    int
	window_number = * ((int *) pm(GET,"TwoD_Win.Window_Number",NULL));

    twoD_win_objects 
	*ip;

    Xv_opaque
	win;

    char
	label[MAX_LABEL_LEN];

    if (!valid_twoD_id(window_number))
	return MAJOR_ERROR;

    sprintf(label,"TwoD_Win.Handle.%d",window_number);
    win = (Xv_opaque) pm(GET,label,NULL);

    if (!win)
	return MAJOR_ERROR;

    ip = (twoD_win_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
  
    delete_print_win(get_twoD_number(ip));
    delete_twoD_win(ip);

}

/*
  Return a window number (to be used as an index in TwoD_Ds) which corresponds
  to the n'th active window. Counting of windows starts with 0. If there is no
  active n'th window, the value to be used in opening the next window is
  returned.
*/
int
    absolute_number_to_window_number(n)
int
    n;
{
    int
	list_number = 0,
	index_number = -1,
	wn;

    for (wn=0; wn <=get_max_twoD(); wn++) {
	if (valid_twoD_id(wn)) {
	    if (list_number == n)
		index_number = wn;
	    list_number++;
	}
    }				
    if (index_number == -1)
	index_number = get_max_twoD() + 1;

    return index_number;

    
}

/*
 * Take postmaster stuff and make twoD window reflect it
 *
 */
void
  update_current_twoD_window()
{
  int n = *((int *) pm(GET, "TwoD_Win.Window_Number", NULL));
  char *str = calloc(SIZE_OF_DIR_PLUS_FNAME, sizeof(char));

  if (!valid_twoD_id(n)) return;

  set_sym(n, *((int *) pm(GET, "TwoD_Win.Symbol_Index", NULL)));
  set_size(n, *((int *) pm(GET, "TwoD_Win.Sym_Size_Index", NULL)));
  set_bg_color(n, *((int *) pm(GET, "TwoD_Win.Bg_Color_Index", NULL)));
  set_showcol(n, *((int *) pm(GET, "TwoD_Win.Show_Cmap", NULL)));
  set_cmap_type(n, *((int *) pm(GET, "TwoD_Win.Cmap_Type_Index", NULL)));
  set_depthcoord(n, *((int *) pm(GET, "TwoD_Win.Depth_Coord", NULL)));
  set_dcoord_min(n, *((double *) pm(GET, "TwoD_Win.Depth_Coord_Min", NULL)));
  set_dcoord_max(n, *((double *) pm(GET, "TwoD_Win.Depth_Coord_Max", NULL)));
  pm(GET, "TwoD_Win.Cmap_File", str, NULL);
  set_cmap_fname(n, str);

  TwoD_Ds[n]->TwoD_Win_Ds->Active_Hor_Type = 
    *((int *) pm(GET, "TwoD_Win.Hor_Type", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Active_Hor_Index =  
    *((int *) pm(GET, "TwoD_Win.Hor_Index", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Active_Ver_Type = 
    *((int *) pm(GET, "TwoD_Win.Ver_Type", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Active_Ver_Index =  
    *((int *) pm(GET, "TwoD_Win.Ver_Index", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Hor_Min =
    *((double *) pm(GET, "TwoD_Win.Hor_Min", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Hor_Max =
    *((double *) pm(GET, "TwoD_Win.Hor_Max", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Vert_Min =
    *((double *) pm(GET, "TwoD_Win.Vert_Min", NULL));
  TwoD_Ds[n]->TwoD_Win_Ds->Vert_Max =
    *((double *) pm(GET, "TwoD_Win.Vert_Max", NULL));

  twoD_opt_win_refresh(n);
  twoD_data_refresh(n);
  colortable_refresh(n);
  bg_color_refresh(n, *((int *) pm(GET, "TwoD_Win.Bg_Color_Index", NULL)));

  free(str);
  return;
}
