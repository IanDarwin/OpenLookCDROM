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
 * tclwin.c - Notify and event callback functions.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/notice.h>
#include <gdd.h>

#include <ui_init.h>
#include <constants.h>
#include <pm.h>
#include "tclwin_ui.h"

/* local globals! */
static char 
  tcl_directory[SIZE_OF_DIR_PLUS_FNAME],
  tcl_filename[SIZE_OF_DIR_PLUS_FNAME];

static tclwin_win_objects *tclwin_ip= NULL;

/* 
 * tclwin_open()  displays the tclwin window, creating it if necessary.
 */
int
  tclwin_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect  *rect;
  
  if (!tclwin_ip)
    {
      tclwin_ip = tclwin_win_objects_initialize(NULL, cmd_ip->win);
      register_win_and_type("Tcl",tclwin_ip->win,POPUP_WINDOW);
      tclwin_field_manager();	/* calls tclwin_init() */
    }
  if (use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(tclwin_ip->win,rect); /* get the current configuration */
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if (width>0) rect->r_width = (short) width;
      if (height>0) rect->r_height = (short) height;
      frame_set_rect(tclwin_ip->win,rect); /* set the current configuration */
      free(rect);
    }
  tclwin_data_refresh();
  mark_window_open("Tcl");
  xv_set(tclwin_ip->win, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(tclwin_ip->win, WIN_SHOW, TRUE, WIN_FRONT, 0);
}

/*
 * tclwin_data_refresh() uses local info to update tclwin window
 */
tclwin_data_refresh()
{
  tclwin_win_objects *ip = tclwin_ip;

  if (!ip ) return; 

  xv_set(ip->directory, PANEL_VALUE, tcl_directory, NULL);
  xv_set(ip->filename, PANEL_VALUE, tcl_filename, NULL);
  xv_set(ip->option, PANEL_VALUE, 
	 *((int *) pm(GET, "Tcl.Verbose", NULL)), NULL);
}

int
tcl_close()
{
    mark_window_closed("Tcl");
    if(tclwin_ip) {
	xv_set(tclwin_ip->win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
        xv_set(tclwin_ip->win, XV_SHOW, FALSE, NULL);	
    }
}

/*
 * tclwin_read_window() reads info from the tclwin window into the postmaster
 */
tclwin_read_window()
{
  tclwin_win_objects *ip = tclwin_ip;
  int verbose;
  char *strng;

  if (!ip) return;

  strng = (char *) xv_get(ip->directory, PANEL_VALUE);	/* Directory Name */
  strcpy(tcl_directory, strng);

  strng = (char *) xv_get(ip->filename, PANEL_VALUE);	 /* Filename */
  strcpy(tcl_filename, strng);

  verbose = (int) xv_get(ip->option, PANEL_VALUE);
  pm(PUT, "Tcl.Verbose", verbose, NULL);

}


/*
 * tclwin_field_manager() updates info on tclwin window.
 */
tclwin_field_manager()
{
  int			i,v_dim,p_dim;
  char			name[MAX_LEN_VARB_NAME];
  static int		num_varb=0, num_param=0;
  tclwin_win_objects  	*ip = tclwin_ip;
  
  if (!ip) return;
  
  tclwin_init();		
}


/*
 * tclwin_init() called to initialize entries of tclwin object 
 * to default values
 */
tclwin_init()
{
  char  dirname[SIZE_OF_DIR_PLUS_FNAME];
  int   get_cur_dir(), tclwin_data_refresh();
  int	temp, i, v_dim, p_dim, *v_box, *p_box, *ivector();
  

  tclwin_data_refresh();
  pm(PUT,"Tcl.Echo",1,NULL);
  get_cur_dir(dirname);			/* get defaults from system */
  strcpy(tcl_directory,dirname);
  tcl_filename[0] = '\0';

}

/*
 * tclwin_handler()
 *
 * Menu handler for `panelmenu (Tcl...)'.
 */
Menu_item
  tclwin_handler(item, op)
Menu_item item;
Menu_generate op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    tclwin_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Tcl",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		tclwin_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		tclwin_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}

  return item;
}



/*
 * tclwin_driver() calls tclwin_go() and monitors errors.  
 * Errors are displayed depending on mode:
 *    mode = 0  <==> display errors on tclwin_win 
 *    mode > 0  <==> display errors on cmd_win 
 *    mode < 0  <==> write errors to stderr
 */
tclwin_driver(mode)
int   mode;
{
  Panel		pan;
  int			status;
  int			keep_going=TRUE;

  if (mode > 0)
    pan = (Panel) cmd_ip->pan;
  else if (mode == 0)
    pan = (Panel) tclwin_ip->pan;

  while( keep_going && (status = tclwin_go()) )
    switch (status)
      {
      case -1:
	if(mode >= 0) 
	  error_notice(pan, "Cannot find or open file. Check directory and file names.");
	else
	  system_mess_proc(1, "Cannot find or open input file. Check directory and file names.");
	keep_going = FALSE;
	break;
      } 

  return;
}


/*
 * tclwin_go()	reads data from file
 * Returns 0 = successful load; -1 = cannot open or find file;
 */
int
tclwin_go()
{
  FILE          *fp,*fopen();
/*   char		*strcpy(),*strcat(); */
  int		status=0, tcl_go();
  char          dirname[SIZE_OF_DIR_PLUS_FNAME];
  int i;

  strcpy(dirname,tcl_directory);
  i = strlen(dirname);
  if (dirname[i-1] != '/')
    strcat(dirname,"/");
  strcat(dirname,tcl_filename);
  if ( check_file_to_read(dirname) )	
    status = tcl_readfile(dirname);
  else				/* file does not exist */
    status = -1;

  return status;
}


