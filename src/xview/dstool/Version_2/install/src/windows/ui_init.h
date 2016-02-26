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
#ifndef UI_INIT_HEADER
#define UI_INIT_HEADER

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/generic.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/textsw.h>
#include <xview/window.h>
#include <xview/xv_xrect.h>
#include <xview/rect.h>

#include "cmd/cmd_ui.h"

#define NO_LOCATION -1
extern cmd_win_objects *cmd_ip;

/* constants for XV_KEY_DATA  (should not conflict with INSTANCE ) */
extern Attr_attribute	INSTANCE;

#endif


