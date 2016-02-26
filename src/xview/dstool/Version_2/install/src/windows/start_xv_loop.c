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
#include "ui_init.h"
#include <defaults.h>
#include <pm.h>


void
  start_xv_loop(argc,argv)
int argc;
char **argv;
{
  /* initialize xview and create command panel */
  if ( ui_init(&argc, argv) ) 
       stop_execution("window initialization failure.");

  /* replace argc/argv list with stripped down version */
  pm(PUT, "Control.Argc", argc,
     INIT, "Control.Argv", argc, MAX_CMD_LINE_ARG_LEN,
     PUT_LIST, "Control.Argv", 0, argc-1, argv, NULL);

  /* load default configuration files ... */
  load_startup_data();

  /* pass control to xview */
  xv_main_loop(cmd_ip->win);
}







