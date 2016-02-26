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
#include <math.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/notice.h>
#include <gdd.h>
#include <gcm.h>
#include <xview/cms.h>

#include <ui_init.h>
#include "twoD.h"
#include "twoD_opt.h"
#include "twoD_ip.h"
#include <constants.h>
#include <plot.h>
#include <pm.h>

/*
* Event callback function for `cbar_lt'.
*/
void
cbar_lt_handler(item, event)
Panel_item	item;
Event		*event;
 {
 	panel_default_handle_event(item, event);
 }
 
 /*
  * Notify callback function for `cbar_lt'.
  */
 void
 cbar_lt_notify(item, event)
 	Panel_item	item;
 	Event		*event;
 {
	extern void	twoD_cbar_repaint();
	int	index, get_twoD_number(), window_number;
	Canvas	canvas;
	Window	paint_window;
 	twoD_win_objects	*ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	window_number = get_twoD_number(ip);
	canvas = (Canvas) get_twoD_handle(window_number,CBAR_CANVAS_HANDLE);
	paint_window = (Xv_window) get_twoD_handle(window_number,CBAR_PW_HANDLE);
	index = TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index;
	index = index + 1;
	if(index>=TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors) index = 0;
	TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index = index;
        twoD_cbar_repaint(canvas,paint_window,NULL);  
 	
 }
 
 /*
  * Event callback function for `twoD_cbar'.
  */
 Notify_value
 twoD_cbar_handler(win, event, arg, type)
 	Xv_window	win;
 	Event		*event;
 	Notify_arg	arg;
 	Notify_event_type type;
 {
	int	window_number;
 	char	name[MAXPATHLEN];
 	twoD_win_objects	*ip = (twoD_win_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
 	
 
	window_number = get_twoD_number(ip);

	switch (event_action(event)) 
	   {
/*	   obsolete 
   	    case ACTION_DRAG_LOAD:
 		if (gdd_get_drag_name(win, name) != -1) 
 		break;
*/
 	    case ACTION_MENU:
		if( event_is_down(event)) {
 		    Menu	menu = (Menu) xv_get(win, WIN_MENU);
 
 		    if (menu)
 			menu_show(menu, win, event, 0); }
	        break;
	    case ACTION_SELECT:
		if(event_is_down(event) & !(event_ctrl_is_down(event)) & !(event_shift_is_down(event))) 
		   pm( PUT, "Color.Pick_Color_Choice", (int) cbar_cell_index(window_number, (int) event_x(event)),NULL);
		   paint_cbar_win_id(window_number);
		break;
           }

 	return notify_next_event_func(win, (Notify_event) event, arg, type);
 }
 
 
 /*
  * Notify callback function for `cbar_rt'.
  */
 void
 cbar_r_notify(item, event)
 	Panel_item	item;
 	Event		*event;
 {
	int	index, get_twoD_number(), window_number;
	Canvas	canvas;
	Window	paint_window;
	void twoD_cbar_repaint();
 	twoD_win_objects	*ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	window_number = get_twoD_number(ip);
	canvas = (Canvas) get_twoD_handle(window_number,CBAR_CANVAS_HANDLE);
	paint_window = (Xv_window) get_twoD_handle(window_number,CBAR_PW_HANDLE);
	index = TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index;
	index = index - 1;
	if(index<0) index = TwoD_Ds[window_number]->TwoD_Win_Ds->Total_Traj_Colors-1;
	TwoD_Ds[window_number]->TwoD_Win_Ds->Cbar_Index = index;
        twoD_cbar_repaint(canvas,paint_window,NULL);  
 	
 }


/*
 * Create object 'cursorpos' in the specified instance
 */
Xv_opaque
twoD_win_cursorpos_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;

	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 90,
		XV_Y, 45,
		XV_WIDTH, 1,
		XV_HEIGHT, 13,
		NULL);
	return obj;
}


/*
 * Event callback function for `canvas'.
 */
Notify_value
canvas_handler(win, event, arg, type)
	Xv_Window	win;
	Event		*event;
	Notify_arg	arg;
	Notify_event_type type;
{
  double	pxtorx(),pytory();

  Xv_opaque		canvas = xv_get(win, CANVAS_PAINT_CANVAS_WINDOW);
  twoD_win_objects	*ip = (twoD_win_objects *) xv_get(canvas, XV_KEY_DATA, INSTANCE);
  int			format,window_number, update_stored_points();
  char	curposstr[128];
  double cursor_absc, cursor_ordin;
	

  window_number = get_twoD_number(ip);
  reset_win_dim(window_number);
  if( !valid_twoD_id(window_number) )  return(NOTIFY_UNEXPECTED);

  cursor_absc = pxtorx( (int) event_x(event),
				TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Min,
				TwoD_Ds[window_number]->TwoD_Win_Ds->Hor_Max,
				TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_X,
				TwoD_Ds[window_number]->TwoD_Win_Ds->Px_Max);
  cursor_ordin = pytory ( (int) event_y(event) ,
				  TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Min,
				  TwoD_Ds[window_number]->TwoD_Win_Ds->Vert_Max,
				  TwoD_Ds[window_number]->TwoD_Win_Ds->P_Origin_Y,
				  TwoD_Ds[window_number]->TwoD_Win_Ds->Py_Max);

  switch (event_action(event)) 
    {
    case LOC_WINEXIT:	   /* pointer exits canvas; clear coordinates */
      xv_set(ip->cursorpos, PANEL_LABEL_STRING, "", NULL); 
      break;

    case LOC_MOVE:	  /* pointer moved within window; write cursor location */
      format = *((int *) pm(GET, "Defaults.Precision", NULL));
      sprintf(curposstr,"(%.*lg,%.*lg)",format,cursor_absc,format,cursor_ordin );
      xv_set(ip->cursorpos, PANEL_LABEL_STRING, curposstr, NULL); 
      break;

    case ACTION_ADJUST:			       /* ADJUST button event (middle) */
      if (event_is_down(event))
	{	
	  if (event_shift_is_down(event))
	    mouse_shift_adjust(window_number, cursor_absc, cursor_ordin);
	  else if (event_ctrl_is_down(event))
	    mouse_control_adjust(window_number, cursor_absc, cursor_ordin);
	  else
	    mouse_adjust(window_number, cursor_absc, cursor_ordin);
	}
      break;

    case ACTION_SELECT:
      if (event_is_down(event)) /* SELECT button depressed */
	{
	  if (event_shift_is_down(event))
	    mouse_shift_select(window_number, cursor_absc, cursor_ordin);
	  else if (event_ctrl_is_down(event))
	    mouse_control_select(window_number, cursor_absc, cursor_ordin);
	  else	
	    mouse_select(window_number, cursor_absc, cursor_ordin);
	}
      break;

    case ACTION_MENU:
      if (event_is_down(event)) /* MENU button depressed */
	{
	  if (event_shift_is_down(event)) 
	    mouse_shift_menu(window_number, cursor_absc, cursor_ordin);
	  else if (event_ctrl_is_down(event))
	    mouse_control_menu(window_number, cursor_absc, cursor_ordin);
	  else
	    mouse_menu(window_number, cursor_absc, cursor_ordin);
	}
      break;
    }

  return notify_next_event_func((Notify_client) win, (Notify_event) event, arg, type);
}


/*
 * Repaint callback function for `canvas'.
 */
void
twoD_canvas_repaint(canvas, paint_window, rects)
        Canvas          canvas;
        Xv_window       paint_window;
        Rectlist        *rects;
{
        int                     *window_number;
        window_number = (int *) xv_get(paint_window,XV_KEY_DATA,MODEL_MENU_ITEM_KEY,NULL);
	reset_win_dim(*window_number);
        paint_win_id( *window_number );
}
 
/*
 * Repaint callback function for `canvas'.
 */
void
twoD_cbar_repaint(canvas, paint_window, rects)
        Canvas          canvas;
        Xv_window       paint_window;
        Rectlist        *rects;
{
        int                     *window_number;
        window_number = (int *) xv_get(paint_window,XV_KEY_DATA,MODEL_MENU_ITEM_KEY,NULL);
        paint_cbar_win_id( *window_number );
}


/*
 * Background event callback for twoD_win panel
 *
 * This is a function generated to handle popup windows in
 * a controls area for twoD windows.
 */
void
panel_background_handler(panel,event)
Xv_opaque       panel;
Event           *event;
{
  if (event_action(event) == ACTION_MENU && event_is_down(event)) {
    Menu menu = (Menu) xv_get(panel, WIN_MENU);
    if (menu) menu_show(menu, panel, event, 0);
  }
}


/*
 * Menu handler for 'panel_menu (Close)'
 *
 */
Menu_item
panel_menu_quit(item, op)
Menu_item       item;
Menu_generate   op;
{
  void  delete_twoD_win();
  twoD_win_objects *ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

  if (op == MENU_NOTIFY) {
    /* close the window from which this menu was chosen */
    delete_print_win(get_twoD_number(ip)); 
    delete_twoD_win(ip);
  }

  return(item);
}


/*
 * procedure called when the quit button from frame menu is selected
 *
 */
void
    twoD_done_proc(frame)
Frame frame;
{
    void 	delete_twoD_win();
    twoD_win_objects *ip = (twoD_win_objects *) xv_get(frame, XV_KEY_DATA, INSTANCE);
    delete_print_win(get_twoD_number(ip));
    delete_twoD_win(ip);
}


/*
 * Notify callback function for `hor'.
 */
int
hor_notify(item, value, event)
        Panel_item      item;
        int             value;
        Event           *event;
{
  int     n_total,get_twoD_number(),type,index;
  twoD_win_objects        *ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  int     window_number = (int) get_twoD_number(ip);
  int     n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  int     n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int     n_funct = *((int *) pm( GET, "Model.Funct_Dim", NULL ));

  n_total = n_varb + n_param + n_funct;

  if( value < n_varb) {
    type = PHASE_SPACE_VARB;
    index = value;
  }
  else if (n_varb<=value && value<n_varb+n_param){
    type = PARAMETER_VARB;
    index = value-n_varb;
  }
  else if (n_varb+n_param<=value && value<n_total){
    type = FUNCTION_VARB;
    index = value-n_varb-n_param;
  }
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Type = type;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Hor_Index = index;
  twoD_hor_reset(window_number);
  twoD_data_refresh( window_number );
}

/*
 * Notify callback function for `ver'.
 */
int
ver_notify(item, value, event)
        Panel_item      item;
        int             value;
        Event           *event;
{
  int     n_total,get_twoD_number(),type,index;
  twoD_win_objects        *ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  int     window_number = (int) get_twoD_number(ip);
  int     n_varb =  *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  int     n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int     n_funct = *((int *) pm( GET, "Model.Funct_Dim", NULL ));
 
  n_total = n_varb + n_param + n_funct;
 
  if( value < n_varb) {
    type = PHASE_SPACE_VARB;
    index = value;
  }
  else if (n_varb<=value && value<n_varb+n_param){
    type = PARAMETER_VARB;
    index = value-n_varb;
  }
  else if (n_varb+n_param<=value && value<n_total){
    type = FUNCTION_VARB;
    index = value-n_varb-n_param;
  }
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Type = type;
  TwoD_Ds[window_number]->TwoD_Win_Ds->Active_Ver_Index = index;
  twoD_ver_reset(window_number);
  twoD_data_refresh( window_number );
}
 
 
/*
 * Menu handler for `optionsmenu (Refresh)'.
 */
Menu_item
win_refresh_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  int   window_number,get_twoD_number();
 
  twoD_win_objects *ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
 
  if(op == MENU_NOTIFY) {
    window_number = get_twoD_number(ip);
    if (window_number != -1) 
      {
	opt_data_refresh(window_number);
	twoD_read_scale(window_number);
	twoD_data_refresh(window_number);
	refresh_win_id(window_number);
      }
    else
      {
	system_mess_proc(1,"win_refresh_handler: ERROR .  BAD WINDOW ID.");
      }
  }
  return item;
}

/*
 * Menu handler for `optionsmenu (Display...)'.
 */
Menu_item
twoD_opt_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  twoD_win_objects * ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  twoD_opt_win_objects  *get_twoD_opt();
  int   win_number;

 
         
  if (op == MENU_NOTIFY) 
    {
      if ((win_number = get_twoD_number(ip)) != -1)
	{
	  if (!(twoD_ip[win_number]->twoD_opt_win))
	    twoD_ip[win_number]->twoD_opt_win = twoD_opt_win_objects_initialize(NULL, cmd_ip->win);
	  xv_set(twoD_ip[win_number]->twoD_opt_win->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
	  if( (int) get_display_type(win_number) == TRUE )
	      gcm_initialize_colors
		  (twoD_ip[win_number]->twoD_opt_win->pan,
		   Panel_Color_Choice[(int) fmod((double) win_number, (double) Num_Panel_Colors)] , NULL);
	}
    }    
  return item;
}
 
 
/*
 * Menu handler for `optionsmenu (Print...)'.
 */
Menu_item
print_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  twoD_win_objects * ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  int           get_twoD_number();
         
  if(op == MENU_NOTIFY){
    print_open(get_twoD_number(ip),DEFAULT_WIN_CONFIG,0,0,0,0);
  }
  return item;
}

/*
 * Menu handler for `optionsmenu (Size)'.
 */
Menu_item
win_size_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  return item;
}


/*
 * Menu handler for `sizemenu (Use Default)'.
 */
Menu_item
use_def_size_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  twoD_win_objects * ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

  int   window_number;

  if(op == MENU_NOTIFY){
    window_number = get_twoD_number(ip);
    use_default_size(window_number);
    twoD_data_refresh(window_number);
  }
  return item;
}

/*
 * Menu handler for `sizemenu (Use Default)'.
 */
Menu_item
set_def_size_handler(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  twoD_win_objects * ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

  int   window_number;

  if(op == MENU_NOTIFY){
    window_number = get_twoD_number(ip);
    set_default_size(window_number);
    def_data_refresh();
  }
  return item;
}

/*
 * Notify callback function for `min-max' fields
 */
Panel_setting
min_max_notify(item, event)
        Panel_item      item;
        Event           *event;
{
  int   get_twoD_number(),      window_number;

  twoD_win_objects      *ip = (twoD_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  window_number =  get_twoD_number(ip);
  twoD_read_scale(window_number);
  twoD_data_refresh(window_number);

  return panel_text_notify(item, event);
}





