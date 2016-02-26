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
/* Browser.c 
 * 
 * standard routines for browser window
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <xview/xview.h>

#include <ui_init.h>
#include <constants.h>
#include "browser_cui.h"

#include <pm.h>
#include <memory.h>

static browser_win_objects *browser_ip = NULL;

/* 
 * browser_open()
 *
 * displays the browser window, creating it if necessary.
 */
browser_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  void browser_install(), browser_field_manager();

  if (browser_ip == NULL)
    {
      /* create the window, make the owner the dstool cmd panel */
      browser_ip = browser_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Browser",browser_ip->win,POPUP_WINDOW);
      browser_install();
      browser_field_manager();
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(browser_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(browser_ip->win,rect);
      free(rect);
    }

  /* show the window */
  mark_window_open("Browser");
  xv_set(browser_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(browser_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
}

int
browser_close()
{
    mark_window_closed("Browser");
    if(browser_ip) {
	xv_set(browser_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(browser_ip->win, XV_SHOW, FALSE, NULL);	
    }
}

/*
 * browser_init_head()
 *
 * initializes data structure for browser header fields
 */
browser_init_head(n_doubles, n_ints)
     int n_doubles, n_ints;
{
  int status=0;
  return(status);
}


/*
 * browser_init_body()
 *
 * initializes data structure for browser body fields
 */
browser_init_body(n_doubles, n_ints)
     int n_doubles, n_ints;
{
  int status=0;
  return(status);
}



/*
 * browser_field_manager()
 *
 * manager for the custom panel items
 */
void
browser_field_manager()
{
  int i, n_varb, n_param, status=0;
  char name[MAX_LEN_VARB_NAME];
  static int total_fields=0;
  void browser_reset(), browser_data_refresh();

  /* exit now it the window has not been created */
  if (browser_ip == NULL || status != 0) return;

  /* initialize the data structure for the new dynamical system */
  browser_reset();

  /* destroy the custom items */
  if (total_fields > 0)
    {
      for (i=0; i<total_fields; i++)
	{
	  xv_destroy(browser_ip->vars[i]);
	}
      cfree((char *) browser_ip->vars);
    }

  /* create memory setting if first time */
  if (browser_ip->memory == NULL)
    {
      browser_ip->memory = browser_win_memory_create(browser_ip, browser_ip->pan, 5);
      xv_set(browser_ip->memory, 
	     PANEL_CHOICE_STRING, 0, "Trajectory",
	     PANEL_CHOICE_STRING, 1, "Multiple",
	     PANEL_CHOICE_STRING, 2, "Fixed Point",
	     PANEL_CHOICE_STRING, 3, "Parameter",
	     PANEL_CHOICE_STRING, 4, "Selected Point",
	     PANEL_CHOICE_STRING, 5, "Continuation",
	     NULL);
    }

  /* find out how many fields */
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  total_fields = n_varb+n_param+3;

  /* allocate memory for the array of text fields */
  browser_ip->vars = (Xv_opaque *) calloc(total_fields, sizeof(Xv_opaque));
  if (browser_ip->vars == NULL)
    {
      total_fields = 0;
      system_mess_proc(1,"browser_field_manager: Memory allocation error.");
      return;
    }
  
  /* create custom fields */
  for(i=0; i<n_varb; i++)
    {
      browser_ip->vars[i] = browser_win_vars_create(browser_ip, browser_ip->pan, i);
      pm(GET, "Model.Varb_Names", i, name, NULL);
      xv_set(browser_ip->vars[i], PANEL_LABEL_STRING, name, NULL);
    }
  for(i=0; i<n_param; i++)
    {
      browser_ip->vars[n_varb+i] = browser_win_vars_create(browser_ip, browser_ip->pan, n_varb+i);
      pm(GET, "Model.Param_Names", i, name, NULL);
      xv_set(browser_ip->vars[n_varb+i], PANEL_LABEL_STRING, name, NULL);
    }
  browser_ip->vars[n_varb+n_param] = browser_win_vars_create(browser_ip, browser_ip->pan, n_varb+n_param);
  xv_set(browser_ip->vars[n_varb+n_param], PANEL_LABEL_STRING, "Alt Color", NULL);
  browser_ip->vars[n_varb+n_param+1] = browser_win_vars_create(browser_ip, browser_ip->pan, n_varb+n_param+1);
  xv_set(browser_ip->vars[n_varb+n_param+1], PANEL_LABEL_STRING, "Pick Color", NULL);
  browser_ip->vars[n_varb+n_param+2] = browser_win_vars_create(browser_ip, browser_ip->pan, n_varb+n_param+2);
  xv_set(browser_ip->vars[n_varb+n_param+2], PANEL_LABEL_STRING, "Symbol", NULL);

  window_fit(browser_ip->pan);
  window_fit(browser_ip->win);

  /* write data into fields */
  browser_data_refresh();

  return;
}


/*
 * browser_fm_head()
 *
 * field manager for browser - deals with header fields
 */
browser_fm_head(n_doubles, n_ints)
int n_ints, n_doubles;
{
  static int total_fields=0;
  int n, i, status = 0;
/*  char *calloc(), *realloc();*/

  status = browser_init_head(n_doubles, n_ints);

  /* exit now if window is not created or mem error */
  if (browser_ip == NULL || status != 0) return(status);

  n = n_doubles+n_ints;
  if (total_fields > n)
    {
      /* destroy some fields */
      for (i=n; i<total_fields; i++)
	xv_destroy(browser_ip->header[i]);
      total_fields = n;
    }
  else if (total_fields < n)
    {
      /* add some fields */
      if (browser_ip->header)
	browser_ip->header = (Xv_opaque *) realloc(browser_ip->header, n*sizeof(Xv_opaque));
      else
	browser_ip->header = (Xv_opaque *) calloc(n, sizeof(Xv_opaque));
      for (i=total_fields; i<n; i++)
	browser_ip->header[i] = browser_win_header_create(browser_ip, browser_ip->pan, i);
      total_fields = n;
    }
  window_fit(browser_ip->pan);
  window_fit(browser_ip->win);
  return(status);
}


/*
 * browser_fm_body()
 *
 * field manager for browser - deals with body fields
 */
browser_fm_body(n_doubles, n_ints)
int n_ints, n_doubles;
{
  static int total_fields=0;
  int n, i, status = 0;
/*  char *calloc(), *realloc();*/

  status = browser_init_body(n_doubles, n_ints);

  /* exit now if window is not created or mem error */
  if (browser_ip == NULL || status != 0) return(status);

  n = n_doubles+n_ints;
  if (total_fields > n)
    {
      /* destroy some fields */
      for (i=n; i<total_fields; i++)
	xv_destroy(browser_ip->body[i]);
      total_fields = n;
    }
  else if (total_fields < n)
    {
      /* add some fields */
      if (browser_ip->body)
	browser_ip->body = (Xv_opaque *) realloc(browser_ip->body, n*sizeof(Xv_opaque));
      else
	browser_ip->body = (Xv_opaque *) calloc(n, sizeof(Xv_opaque));
      for (i=total_fields; i<n; i++)
	browser_ip->body[i] = browser_win_body_create(browser_ip, browser_ip->pan, i);
      total_fields = n;
    }
  window_fit(browser_ip->pan);
  window_fit(browser_ip->win);
  return(status);
}



/*
 * browser_read_window()
 *
 * Routine to read data from all items in the browser window
 */
void
    browser_read_window()
{
  int mem_type;
  char *mem_code = NULL;

  /* if no window, then nothing to read */
  if (browser_ip == NULL) return;

  pm(PUT, "Browser.Flow_Num", (int) xv_get(browser_ip->flow, PANEL_VALUE),
     PUT, "Browser.Traj_Num", (int) xv_get(browser_ip->traj, PANEL_VALUE),
     PUT, "Browser.Point_Num", (int)xv_get(browser_ip->point, PANEL_VALUE),
     PUT, "Browser.Highlight", (int)xv_get(browser_ip->highlight, PANEL_VALUE),
     NULL);

  mem_type = (int) xv_get(browser_ip->memory, PANEL_VALUE);
  if (mem_type == 0) mem_code = "Memory.Traj";
  else if (mem_type == 1) mem_code = "Memory.Mult";
  else if (mem_type == 2) mem_code = "Memory.Fixed";
  else if (mem_type == 3) mem_code = "Memory.Param";
  else if (mem_type == 4) mem_code = "Memory.Sel_Pt";
  else if (mem_type == 5) mem_code = "Memory.Cont";
  else 
    fprintf(stdout,"debug_memdump_go: Not a valid MEMORY object.\n");

  if (mem_code != NULL)
    pm(INIT, "Browser.Memory",
       PUT, "Browser.Memory", (void *) pm(GET, mem_code, NULL),
       NULL);
  
  return;
}

/*
 * browser_data_refresh()
 *
 * routine to refresh the data in all items in the browser window
 */
void
    browser_data_refresh()
{
  int status=0, t_num, p_num, i, *p_color, n_varb, n_param, format;
  int hi_num, hf_num, bi_num, bf_num, *p_hi, *p_bi;
  double *p_varb, *p_param, *p_hf, *p_bf;
  char str[30];
  int browser_flow_num, browser_traj_num, browser_point_num,
    head_status=0, body_status=0;
  memory browser_memory;

  /* if no window, then nothing to refresh */
  if (browser_ip == NULL) return;
  
  xv_set(browser_ip->flow, PANEL_VALUE, 
	 *((int *) pm(GET, "Browser.Flow_Num", NULL)), NULL);
  xv_set(browser_ip->traj, PANEL_VALUE,
	 *((int *) pm(GET, "Browser.Traj_Num", NULL)), NULL);
  xv_set(browser_ip->point, PANEL_VALUE, 
	 *((int *) pm(GET, "Browser.Point_Num", NULL)), NULL);
  
  /* should also update memory settings, but there is no
     way to change memory object except with this, so it
     will always be up to date.
     same for highlight! */
  
  /* now update data point */
  browser_memory = (memory) pm(GET, "Browser.Memory", NULL);
  status = memory_reset_read(browser_memory);
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  browser_clear_panel(n_varb+n_param+3);
  pm(PUT, "Browser.Set_Code", 0, NULL);
  
  format = *((int *) pm(GET, "Defaults.Precision", NULL));

  browser_highlight_erase();

  /* count flows */
  i = memory_nflows(browser_memory);
  if (i>0) sprintf(str,"(1-%ld)",i);
  else sprintf(str,"(none)");
  xv_set(browser_ip->flowmes, PANEL_LABEL_STRING, str, NULL);
  browser_flow_num = *((int *) pm(GET, "Browser.Flow_Num", NULL));
  if (browser_flow_num<1 || browser_flow_num > i)
    {
      /* invalid flow selected */
      browser_fm_head(0,0);
      browser_fm_body(0,0);
    }
  else
    {
      /* start from first flow again */
      status = memory_reset_read(browser_memory);
      for (i=0; i<browser_flow_num && status==0; i++)
	status = memory_read_next_flow(browser_memory, &t_num, &p_hf, &hf_num, &p_hi, &hi_num);
      if (t_num>0)
	sprintf(str,"(1-%ld)",t_num);
      else sprintf(str,"(none)");
      xv_set(browser_ip->trajmes, PANEL_LABEL_STRING, str, NULL);
      head_status = browser_fm_head(hf_num, hi_num);
      if (!head_status)
	{
	  for (i=0; i<hf_num; i++)
	    {
	      sprintf(str,"%.*lg",format,p_hf[i]);
	      xv_set(browser_ip->header[i], PANEL_VALUE, str, NULL);
	    }
	  for (i=0; i<hi_num; i++)
	    {
	      sprintf(str,"%ld",p_hi[i]);
	      xv_set(browser_ip->header[hf_num+i], PANEL_VALUE, str, NULL);
	    }
	}
      browser_traj_num = *((int *) pm(GET, "Browser.Traj_Num", NULL));
      for (i=0; i<browser_traj_num && status==0; i++)
	status = memory_read_next_traj(browser_memory, &p_num, &bf_num, &bi_num);
      if (status)
	{
	  /* invalid traj selected */
	  browser_fm_body(0,0);
	}
      else
	{
	  if (p_num>0)
	    sprintf(str,"(1-%ld)",p_num);
	  else sprintf(str,"(none)");
	  xv_set(browser_ip->pointmes, PANEL_LABEL_STRING, str, NULL);
	  browser_point_num = *((int *) pm(GET, "Browser.Point_Num", NULL));
	  status = memory_set_read(browser_memory, browser_flow_num, 
				   browser_traj_num, browser_point_num, 
				   NULL, NULL, NULL, NULL, NULL, NULL);
	  if (status)
	    {
	      /* invalid point selected */
	      browser_fm_body(0,0);
	    }
	  else
	    {
	      /* OK we have a point to display ! */
	      status = memory_read_next_point(browser_memory, &p_varb, &p_param, &p_color, 
					      &p_bf, &p_bi);
	      if (p_varb)
		{
		  pm(PUT_LIST, "Browser.Vars", 0, n_varb-1, p_varb, NULL);
		  for (i=0; i<n_varb; i++)
		    {
		      sprintf(str, "%.*lg",format,p_varb[i]);
		      xv_set(browser_ip->vars[i], PANEL_VALUE, str, NULL);
		    }
		  pm(PUT, "Browser.Set_Code", 
		     *((int *) pm(GET, "Browser.Set_Code", NULL))+1, NULL);
		}
	      if (p_param)
		{
		 pm(PUT_LIST, "Browser.Params", 0, n_param-1, p_param, NULL);
		 for (i=0; i<n_param; i++)
		    {
		      sprintf(str, "%.*lg",format,p_param[i]);
		      xv_set(browser_ip->vars[n_varb+i], PANEL_VALUE, 
			     str, NULL);
		    }
		  pm(PUT, "Browser.Set_Code", 
		     *((int *) pm(GET, "Browser.Set_Code", NULL))+2, NULL);
		}
	      if (p_color)
		{
		  pm(PUT_LIST, "Browser.Color", 0, 2, p_color, NULL);
		  for (i=0; i<3; i++)
		    {
		      sprintf(str, "%ld", p_color[i]);
		      xv_set(browser_ip->vars[n_varb+n_param+i], PANEL_VALUE, 
			     str, NULL);
		    }
		  pm(PUT, "Browser.Set_Code", 
		     *((int *) pm(GET, "Browser.Set_Code", NULL))+4, NULL);
		}
	      body_status = browser_fm_body(bf_num, bi_num);
	      if (!body_status)
		{
		  for (i=0; i<bf_num; i++)
		    {
		      sprintf(str,"%.*lg",format,p_bf[i]);
		      xv_set(browser_ip->body[i], PANEL_VALUE, str, NULL);
		    }
		  for (i=0; i<bi_num; i++)
		    {
		      sprintf(str,"%ld",p_bi[i]);
		      xv_set(browser_ip->body[bf_num+i], PANEL_VALUE, str, NULL);
		    }
		}
	    }
	}
    }
  
  if (*((int *) pm(GET, "Browser.Highlight", NULL))) 
    browser_highlight_draw();
  
  return;
}

/*
 * browser_clear_panel()
 *
 * clears point information from the panel
 * pass in the number of vars text fields to clear
 */
browser_clear_panel(n)
int n;
{
  int i;
  
  xv_set(browser_ip->flowmes, PANEL_LABEL_STRING, "", NULL);
  xv_set(browser_ip->trajmes, PANEL_LABEL_STRING, "", NULL);
  xv_set(browser_ip->pointmes, PANEL_LABEL_STRING, "", NULL);
  
  for (i=0; i<n; i++)
    xv_set(browser_ip->vars[i], PANEL_VALUE, "", NULL);
}


/*
 * browser_handler()
 *
 * Menu handler for `panelmenu (Browser...)'.
 */
Menu_item
  browser_handler(item, op)
Menu_item item;
Menu_generate op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    browser_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Browser",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		browser_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		browser_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}

