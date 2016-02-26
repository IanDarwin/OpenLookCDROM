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
 * ui_init.c
 *
 * ui_init()
 * ui_window_objects_initialize()
 *
 */



#include <stdio.h>
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
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/rect.h>
#include <constants.h>

#include <ui_init.h>
#include <defaults.h>


/*
 * Instance XV_KEY_DATA key.  An instance is a set of related
 * user interface objects.  A pointer to an object's instance
 * is stored under this key in every object.  This must be a
 * global variable.
 */
Attr_attribute	INSTANCE;


ui_init(pargc, argv)
int *pargc;
char **argv;
{
	/* Textsw_status	status; */
	
	/* XView initialization command */
	xv_init(XV_INIT_ARGC_PTR_ARGV, pargc, argv, 0);
	INSTANCE = xv_unique_key();

	/* 
	 * Initialize dstool window setup
	 */

	/* position the command window */
	cmd_open(SET_WIN_CONFIG,1,2,0,0);

	/* set up the models choice */
	win_install_models();

	return(0);
}




