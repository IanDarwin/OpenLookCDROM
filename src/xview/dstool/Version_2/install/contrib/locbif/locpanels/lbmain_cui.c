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
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include "lbmain_cui.h"
#include "lbmain.h"

lbmain_locbif_pu_objects *
lbmain_locbif_pu_objects_initialize(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (lbmain_locbif_pu_objects *) calloc(1, sizeof (lbmain_locbif_pu_objects))))
		return (lbmain_locbif_pu_objects *) NULL;
	if (!ip->locbif_pu)
		ip->locbif_pu = lbmain_locbif_pu_locbif_pu_create(ip, owner);
	if (!ip->lb_main_cntl)
		ip->lb_main_cntl = lbmain_locbif_pu_lb_main_cntl_create(ip, ip->locbif_pu);
	if (!ip->lbparamsel)
		ip->lbparamsel = lbmain_locbif_pu_lbparamsel_create(ip, ip->lb_main_cntl);
        if (!ip->lbforw)
		ip->lbforw = lbmain_locbif_pu_lbforw_create(ip, ip->lb_main_cntl);
	if (!ip->lbcontpu)
		ip->lbcontpu = lbmain_locbif_pu_lbcontpu_create(ip, ip->lb_main_cntl);
	if (!ip->lbsolverp)
		ip->lbsolverp = lbmain_locbif_pu_lbsolverp_create(ip, ip->lb_main_cntl);
	if (!ip->lbdisplayp)
		ip->lbdisplayp = lbmain_locbif_pu_lbdisplayp_create(ip, ip->lb_main_cntl);
	if (!ip->lbactvarb)
		ip->lbactvarb = lbmain_locbif_pu_lbactvarb_create(ip, ip->lb_main_cntl);
	if (!ip->lbmode)
		ip->lbmode = lbmain_locbif_pu_lbmode_create(ip, ip->lb_main_cntl, LB_EQUIL);
	if (!ip->lbdirect)
		ip->lbdirect = lbmain_locbif_pu_lbdirect_create(ip, ip->lb_main_cntl);
	if (!ip->lbfilerd)
		ip->lbfilerd = lbmain_locbif_pu_lbfilerd_create(ip, ip->lb_main_cntl);
	if (!ip->lbfilename)
		ip->lbfilename = lbmain_locbif_pu_lbfilename_create(ip, ip->lb_main_cntl);
        if (!ip->lbcontinue)
		ip->lbcontinue = lbmain_locbif_pu_lbcontinue_create(ip, ip->lb_main_cntl);
	if (!ip->lbback)
		ip->lbback = lbmain_locbif_pu_lbback_create(ip, ip->lb_main_cntl);
  	if (!ip->lbbifmode)
		ip->lbbifmode = lbmain_locbif_pu_lbbifmode_create(ip, ip->lb_main_cntl); 
	if (!ip->lbpause)
		ip->lbpause = lbmain_locbif_pu_lbpause_create(ip, ip->lb_main_cntl);
        if (!ip->lbstatepu)
		ip->lbstatepu = lbmain_locbif_pu_lbstatepu_create(ip, ip->lb_main_cntl);
	return ip;
}

/*
 * Create object `locbif_pu' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_locbif_pu_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	void		lbmain_done_proc(); 
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 401,
		XV_HEIGHT, 382,
		XV_LABEL, "locbif Main Panel",
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, lbmain_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `lb_main_cntl' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lb_main_cntl_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}


/*
 * Create object `lbforw' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbforw_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbforw_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 32,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Forward",
		PANEL_NOTIFY_PROC, lbforw_notify,
		NULL);
	return obj;
}


/*
 * Create object `lbcontinue' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbcontinue_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbcontinue_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 119,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Continue",
		PANEL_NOTIFY_PROC, lbcontinue_notify,
		NULL);
	return obj;
}

/*
 * Create object `lbback' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbback_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbback_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 212,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Backwards",
		PANEL_NOTIFY_PROC, lbback_notify,
		NULL);
	return obj;
}



/*
 * Create object `lbbifmode' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbbifmode_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbmode_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 56,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Mode:",
		PANEL_NOTIFY_PROC, lbmode_notify,
		PANEL_CHOICE_STRINGS,
			"Equilibria of ODEs",
			"Fixed Points of Maps",
			"Periodic Orbits (Autonomous ODEs)",
			"Periodic Orbits (Non-Autonomous ODEs)",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	return obj;
}

/*
 * Create object `lbpause' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbpause_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbmain_locbif_pu_lbpause_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 25,
		XV_Y, 88,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Pause:",
		PANEL_NOTIFY_PROC, lbmain_locbif_pu_lbpause_notify_callback,
		PANEL_CHOICE_STRINGS,
			"Off",
			"Special",
			"Iterate",
			NULL,
		NULL);
	return obj;
}


/*
 * Create object `lbparamsel' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbparamsel_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern int		lbmain_sel_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 128,
		PANEL_LIST_WIDTH, 200,
		PANEL_LIST_DISPLAY_ROWS, 4,
		PANEL_LABEL_STRING, "Selected Parameters:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, lbmain_sel_notify,
		PANEL_LIST_STRINGS,
			" ",
			" ",
			" ",
			" ",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `lbcontpu' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbcontpu_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbmain_cont();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 235,
		PANEL_LABEL_STRING, "Cont Panel",
		PANEL_NOTIFY_PROC, lbmain_cont,
		NULL);
	return obj;
}

/*
 * Create object `lbsolverp' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbsolverp_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbmain_solvr_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 110,
		XV_Y, 235,
		PANEL_LABEL_STRING, "Orbit Panel",
		PANEL_NOTIFY_PROC, lbmain_solvr_notify,
		NULL);
	return obj;
}

/*
 * Create object `lbdisplayp' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbdisplayp_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbmain_dsply_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 205,
		XV_Y, 235,
		PANEL_LABEL_STRING, "Display Panel",
		PANEL_NOTIFY_PROC, lbmain_dsply_notify,
		NULL);
	return obj;
}


/*
 * Create object `lbstatepu' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbstatepu_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbcurst_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 315,
		XV_Y, 235,
		PANEL_LABEL_STRING, "Cur State",
		PANEL_NOTIFY_PROC, lbcurst_notify,
		NULL);
	return obj;
}

/*



/*
 * Create object `lbactvarb' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbactvarb_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 264,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Active Parameters:  ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_UNDERLINED, FALSE,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `lbmode' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbmode_create(ip, owner, mode)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
	int		mode;
{
	Xv_opaque	obj;

	if(mode == LB_EQUIL)
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 288,
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Curve Type:",
		PANEL_CHOICE_STRINGS,
			"Orbit (0)",
			"Curve (1)",
			"Equilibrium (1)",
			"Fold (2)",
			"Hopf (2)",
			"Double Eigenvalue (2)",
			"Double Zero (3)",
			"Fold + Hopf (3)",
			"Double Hopf (3)",
			"Cusp (3)",
			"Hopf + Lyapunov Zero (3)",
			"Fold + Extr (3)",
			"Hopf + Extr (3)",
			"Double Zero + Cusp (4)",
			"Hopf + Cusp (4)",
			"Double Zero + Extr (4)",
			"Fold + Hopf + Extr (4)",
			NULL,
		PANEL_VALUE, 0,
		NULL); 

	if(mode == LB_FPTS)
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 288,
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Curve Type:",
		PANEL_CHOICE_STRINGS,
			 "Orbit (0)",
			 "Curve (1)",
			 "Fixed point (1)",
			 "Fold (2)",
			 "Hopf (2)",
			 "Flip (2)",
			 "Double Multiplier (2)",
			 "Double Fold (3)",
			 "Double Flip (3)",
			 "Fold + Hopf (3)",
			 "Flip + Hopf (3)",
			 "Fold + Flip (3)",
			 "Cusp (3)",
			 "Fold + Extr (3)",
			 "Hopf + Extr (3)",
			 "Flip + Extr (3)",
			NULL,
		PANEL_VALUE, 0,
		NULL); 

        if(mode == LB_PERIODIC_AUTO)
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 288,
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Curve Type:",
		PANEL_CHOICE_STRINGS,
			"Orbit (0)",
			"Curve (2)",
			"Limit Cycle (2)",
			"Fold (3)",
			"Hopf (3)",
			"Flip (3)",
			"Double Multiplier (3)",
			"Double Fold (4)",
			"Double Flip (4)",
			"Fold + Hopf (4)",
			"Flip + Hopf (4)",
			"Fold + Flip (4)",
			"Cusp (4)",
			"Fold + Extr (4)",
			"Hopf + Extr (4)",
			"Flip + Extr (4)",
			NULL,
		PANEL_VALUE, 0,
		NULL);

	if(mode == LB_PERIODIC_NAUTO)
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 288,
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Curve Type:",
		PANEL_CHOICE_STRINGS,
			 "Orbit (0)",
			 "Curve (1)",
			 "Periodic solutions (1)",
			 "Fold (2)",
			 "Hopf (2)",
			 "Flip (2)",
			 "Double Multiplier (2)",
			 "Double Fold (3)",
			 "Double Flip (3)",
			 "Fold + Hopf (3)",
			 "Flip + Hopf (3)",
			 "Fold + Flip (3)",
			 "Cusp (3)",
			 "Fold + Extr (3)",
			 "Hopf + Extr (3)",
			 "Flip + Extr (3)",
			NULL,
		PANEL_VALUE, 0,
		NULL); 

	return obj;
}

/*
 * Create object `lbdirect' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbdirect_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern Panel_setting	lbmain_dir_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 328,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Directory:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbmain_dir_notify,
		NULL);
	return obj;
}

/*
 * Create object `lbfilerd' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbfilerd_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbrdfile_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 344,
		PANEL_LABEL_STRING, "Read Interface File",
		PANEL_NOTIFY_PROC, lbrdfile_notify,
		NULL);
	return obj;
}

/*
 * Create object `lbfilename' in the specified instance.
 */
Xv_opaque
lbmain_locbif_pu_lbfilename_create(ip, owner)
	lbmain_locbif_pu_objects	*ip;
	Xv_opaque	owner;
{
	extern Panel_setting	lbfname_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 24,
		XV_Y, 352,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Filename:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbfname_notify,
		NULL);
	return obj;
}

