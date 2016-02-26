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
#include <sys/param.h>
#include <sys/types.h>
#include <math.h>
#include <xview/xview.h>
#include <xview/cms.h>
#include <xview/panel.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>
#include <gdd.h>
#include <gcm.h>

#include <filters.h>
#include <ui_init.h>
#include <constants.h>
#include <defaults.h>
#include <memory.h>
#include <twoD.h>
#include <pm.h>
#include <plot.h>
#include <symbols.h>
#include <print.h>
#include "print_ui.h"

/*static struct Plot_Pt plot;*/
struct Plot_Pt plot;
static print_win_objects *print_ip = NULL;

/*
 *  print_open()  displays the print window, creating it if necessary.
 */
print_open(window_num,use_default,left,top,width,height)
int   window_num;     /* view window calling print_open */
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  print_win_objects 	*print_win_objects_initialize();
  char            *get_hor_label(),*get_ver_label();
 
  if (!print_ip)
    {
      print_ip = print_win_objects_initialize(NULL, cmd_ip->win);
    }
  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(print_ip->win,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(print_ip->win,rect);
      free(rect);
    }

  /* called every time that printing is requested. Set owner and labels. */
  pm(PUT, "Print.Owner", window_num, 
     PUT, "Print.Hor_Label", get_hor_label(window_num),
     PUT, "Print.Ver_Label", get_ver_label(window_num),
     NULL);
  print_data_refresh();
  xv_set(print_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, WIN_SHOW, TRUE, 
	 WIN_FRONT, NULL);
}
 
/*
 * delete_print_win() is called whenever a view window is destroyed.  
 * If the view window "owns" the print window, then get rid of the print window
 */
int
delete_print_win(window_num)
int		window_num;
{
  if(print_ip) 
    if(window_num == *((int *)pm(GET, "Print.Owner", NULL)) )
      {
	xv_set(print_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, WIN_SHOW, FALSE, NULL);
	pm(PUT, "Print.Owner", -1, NULL); /* no owner */
      }
  return(0);
}


/*
 * print_data_refresh() uses postmaster info to update print window
 * Last change: fjw 7/30/92
 */
print_data_refresh()
{
  int		temp;
  print_win_objects *ip = print_ip;
  char		strng[SIZE_OF_DIR_PLUS_FNAME];

  if (!ip || *((int *)pm(GET, "Print.Owner", NULL)) < 0 ) 
    return; /* not created or no owner */

  xv_set(ip->owner, PANEL_ITEM_COLOR,
	 gcm_color_index(Panel_Color_Choice[*((int *)
                                  pm(GET, "Print.Owner", NULL)) ]),NULL);
  
  pm(GET, "Print.Title", strng, NULL);
  xv_set(ip->title, PANEL_VALUE, strng, NULL);

  pm(GET, "Print.Hor_Label", strng, NULL);
  xv_set(ip->hor_label, PANEL_VALUE, strng, NULL);

  pm(GET, "Print.Ver_Label", strng, NULL);
  xv_set(ip->ver_label, PANEL_VALUE, strng, NULL);

  xv_set(ip->color, PANEL_VALUE, 
	 *((int *)pm(GET, "Print.Color_Flag", NULL))? 0 : 1, NULL);

  temp = *((int *)pm(GET, "Print.File_Flag", NULL));
  xv_set(ip->destination, PANEL_VALUE, temp? 1 : 0, NULL);

  if (temp)
    {
      pm(GET, "Print.Directory", strng, NULL);
      xv_set(ip->dir, PANEL_LABEL_STRING, "Directory:", 
	     PANEL_VALUE, strng, NULL);
      pm(GET, "Print.Filename", strng, NULL);
      xv_set(ip->name, PANEL_INACTIVE, FALSE, PANEL_VALUE, strng, NULL);
    }
  else
    {
      pm(GET, "Print.Printer_Name", strng, NULL);
      xv_set(ip->dir, PANEL_LABEL_STRING, "Printer", 
	     PANEL_VALUE, strng, NULL);
      xv_set(ip->name, PANEL_INACTIVE, TRUE, NULL);
    }


  xv_set(ip->settings, PANEL_VALUE, 
	*((int *)pm(GET, "Print.Show_Settings", NULL))? 1: 0, NULL);
  xv_set(ip->sys_info, PANEL_VALUE, 
	*((int *)pm(GET, "Print.Show_Info", NULL))? 0: 1, NULL);
  xv_set(ip->font, PANEL_VALUE, 
	( *((int *)pm(GET, "Print.Font", NULL))/4), NULL); /* mod 4 gives font family (4 fonts per family)*/
  xv_set(ip->typeface, PANEL_VALUE, 
	( *((int *)pm(GET, "Print.Font", NULL))%4), NULL); /* remainder gives font within family */
  xv_set(ip->titlesize, PANEL_VALUE, 
	*((int *)pm(GET, "Print.Title_Pt", NULL)), NULL);
  xv_set(ip->labelsize, PANEL_VALUE, 
	*((int *)pm(GET, "Print.Label_Pt", NULL)), NULL);
  temp = *((int *)pm(GET, "Print.Landscape", NULL));
  xv_set(ip->landscape, PANEL_VALUE, temp? 0: 1, NULL);

  /* An 8.5 x 11" page is 612 x 792 pts.  We calculate where the offset must be to approx center 
     the plot, given that the bounding box is set the way the user wants, and that the 
     plot is about 30 pts below absolute center, to allow displaying the header information */
  sprintf(strng, "To Center: Offset = ( %3d, %3d )",		       /* update message */
	temp? (int) (325. + .5* (*((int *)pm(GET, "Print.BBox_Ver_Length", NULL)))):
	      (int) (305. - .5* (*((int *)pm(GET, "Print.BBox_Hor_Length", NULL)))),
	temp? (int) (400. - .5* (*((int *)pm(GET, "Print.BBox_Hor_Length", NULL)))):
	      (int) (370. - .5* (*((int *)pm(GET, "Print.BBox_Ver_Length", NULL)))) );
  xv_set(ip->suggestion, PANEL_LABEL_STRING, strng, NULL);

  temp = *((int *)pm(GET, "Print.Show_BBox", NULL));
  xv_set(ip->bound_box, PANEL_VALUE, temp? 0: 1, NULL);
  xv_set(ip->num_ticks_x, PANEL_VALUE, *((int *)pm(GET, "Print.Num_Hor_Ticks", NULL)), 
	 NULL);
  xv_set(ip->num_ticks_y, PANEL_VALUE, *((int *)pm(GET, "Print.Num_Ver_Ticks", NULL)), 
	 NULL);

  xv_set(ip->hor_len, PANEL_VALUE, 
	*((int *)pm(GET, "Print.BBox_Hor_Length", NULL)), NULL);
  xv_set(ip->ver_len, PANEL_VALUE, 
	*((int *)pm(GET, "Print.BBox_Ver_Length", NULL)), NULL);
  xv_set(ip->hor_offset, PANEL_VALUE, 
	*((int *)pm(GET, "Print.BBox_Hor_Offset", NULL)), NULL);
  xv_set(ip->ver_offset, PANEL_VALUE, 
	*((int *)pm(GET, "Print.BBox_Ver_Offset", NULL)), NULL);
  xv_set(ip->connect, PANEL_VALUE, 
	*((int *)pm(GET, "Print.Connect", NULL))? 0: 1, NULL);
  xv_set(ip->labelrange, PANEL_VALUE, 
	 *((int *)pm(GET, "Print.Label_Range", NULL))? 0: 1, NULL);

  if ( *((int *)pm(GET, "Print.Show_Settings", NULL)) )
    {
      xv_set(ip->sys_info,    PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->font,        PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->typeface,    PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->titlesize,   PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->labelsize,   PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->landscape,   PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->bound_box,   PANEL_INACTIVE, FALSE, NULL);
      temp = *((int *)pm(GET, "Print.Show_BBox", NULL));
      xv_set(ip->num_ticks_x, PANEL_INACTIVE, temp? FALSE:TRUE, NULL);
      xv_set(ip->num_ticks_y, PANEL_INACTIVE, temp? FALSE:TRUE, NULL);
      xv_set(ip->hor_len,     PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->ver_len,     PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->hor_offset,  PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->ver_offset,  PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->connect,     PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->labelrange,  PANEL_INACTIVE, FALSE, NULL);
      xv_set(ip->suggestion,  PANEL_INACTIVE, FALSE, NULL);
      window_fit_height(ip->pan);
      window_fit_height(ip->win);
    }
  else
    {
      temp = (int) xv_get(ip->print_button, XV_Y); /* shrink window to just below print button */
      xv_set(ip->win, XV_HEIGHT, temp+20, NULL);
      xv_set(ip->sys_info,    PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->font,        PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->typeface,    PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->titlesize,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->labelsize,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->landscape,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->bound_box,   PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->num_ticks_x, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->num_ticks_y, PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->hor_len,     PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->ver_len,     PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->hor_offset,  PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->ver_offset,  PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->connect,     PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->labelrange,  PANEL_INACTIVE, TRUE, NULL);
      xv_set(ip->suggestion,  PANEL_INACTIVE, TRUE, NULL);
    }
}

/*
 *  print_read_window() reads info from the print window into the postmaster
 *  Last change: fjw 7/30/92
 */
print_read_window()
{
  print_win_objects *ip = print_ip;
  char *strng;
  int  temp;

  if (!ip) return;

  /* Owner is not updated in this routine */
  
  strng = (char *) xv_get(ip->title, PANEL_VALUE);		       /* Title */
  pm(PUT, "Print.Title", strng, NULL);

  strng = (char *) xv_get(ip->hor_label, PANEL_VALUE);		       /* Hor_Label */
  pm(PUT, "Print.Hor_Label", strng, NULL);

  strng = (char *) xv_get(ip->ver_label, PANEL_VALUE);		       /* Ver_Label */
  pm(PUT, "Print.Ver_Label", strng, NULL);

  temp = ((int) xv_get(ip->destination, PANEL_VALUE) == 1)? TRUE: FALSE; /* File_Flag */
  pm(PUT, "Print.File_Flag", temp, NULL);

  strng = (char *) xv_get(ip->dir, PANEL_VALUE);		       /* Directory/Printer_Name */
  if (temp)
    pm(PUT, "Print.Directory", strng, NULL);
  else
    pm(PUT, "Print.Printer_Name", strng, NULL);

  temp = ( (int) xv_get(ip->color, PANEL_VALUE) == 0)? TRUE: FALSE;    /* Color_Flag */
  pm(PUT, "Print.Color_Flag", temp, NULL);

  strng = (char *) xv_get(ip->name, PANEL_VALUE);		       /* Filename */
  pm(PUT, "Print.Filename", strng, NULL);
  
  temp = ( (int) xv_get(ip->settings, PANEL_VALUE) == 1)? TRUE: FALSE; /* Show_Settings */
  pm(PUT, "Print.Show_Settings", temp, NULL);

  temp = ( (int) xv_get(ip->sys_info, PANEL_VALUE) == 0)? TRUE: FALSE; /* Show_Info */
  pm(PUT, "Print.Show_Info", temp, NULL);

  temp = 4 * (int) xv_get(ip->font, PANEL_VALUE) + (int) xv_get(ip->typeface, PANEL_VALUE); /* ont */
  pm(PUT, "Print.Font", temp, NULL); /* index into font list */

  temp = (int) xv_get(ip->titlesize, PANEL_VALUE);		       /* Title_Pt */
  pm(PUT, "Print.Title_Pt", temp, NULL);
  
  temp = (int) xv_get(ip->labelsize, PANEL_VALUE);		       /* Label_Pt */
  pm(PUT, "Print.Label_Pt", temp, NULL);

  temp = ( (int) xv_get(ip->landscape, PANEL_VALUE) == 0)? TRUE: FALSE;	/* Landscape */
  pm(PUT, "Print.Landscape", temp, NULL);

  temp = ( (int) xv_get(ip->bound_box, PANEL_VALUE) == 0)? TRUE: FALSE;	/* Show_BBox */
  pm(PUT, "Print.Show_BBox", temp, NULL);

  temp = (int) xv_get(ip->num_ticks_x, PANEL_VALUE);		       /* Num_Hor_Ticks */
  pm(PUT, "Print.Num_Hor_Ticks", temp, NULL);

  temp = (int) xv_get(ip->num_ticks_y, PANEL_VALUE);		       /* Num_Ver_Ticks */
  pm(PUT, "Print.Num_Ver_Ticks", temp, NULL);

  temp =  (int) xv_get(ip->hor_len, PANEL_VALUE);		       /* BBox_Hor_Length */
  pm(PUT, "Print.BBox_Hor_Length", temp,  NULL);

  temp = (int) xv_get(ip->ver_len, PANEL_VALUE);		       /* BBox_Ver_Length */
  pm(PUT, "Print.BBox_Ver_Length", temp,  NULL);

  temp = (int) xv_get(ip->hor_offset, PANEL_VALUE);		       /* BBox_Hor_Offset */
  pm(PUT, "Print.BBox_Hor_Offset", temp, NULL);

  temp = (int) xv_get(ip->ver_offset, PANEL_VALUE);		       /* BBox_Ver_Offset */
  pm(PUT, "Print.BBox_Ver_Offset", temp, NULL);

  temp = ( (int) xv_get(ip->connect, PANEL_VALUE) == 0)? TRUE: FALSE;  /* Connect */
  pm(PUT, "Print.Connect", temp, NULL);

  temp = ( (int) xv_get(ip->labelrange, PANEL_VALUE) == 0)? TRUE: FALSE;  /* Label_Range */
  pm(PUT, "Print.Label_Range", temp, NULL);
}


/*
 * print_update_range() gets the horizontal and vertical range from the twoD
 * window which "owns" the print panel.
 */
print_update_range()
{
  int	win = *((int *)pm(GET, "Print.Owner", NULL));

  pm(PUT, "Print.HorMin", TwoD_Ds[ win ]->TwoD_Win_Ds->Hor_Min,
     PUT, "Print.HorMax", TwoD_Ds[ win ]->TwoD_Win_Ds->Hor_Max,
     PUT, "Print.VerMin", TwoD_Ds[ win ]->TwoD_Win_Ds->Vert_Min,
     PUT, "Print.VerMax", TwoD_Ds[ win ]->TwoD_Win_Ds->Vert_Max,
     NULL);
}






