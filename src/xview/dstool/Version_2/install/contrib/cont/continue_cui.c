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
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>
#include "continue_cui.h"
#include "continue.h"


/*
 * Initialize an instance of object `cont_pu'.
 */
continue_cont_pu_objects *
continue_cont_pu_objects_initialize(ip, owner)
	continue_cont_pu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (continue_cont_pu_objects *) calloc(1, sizeof (continue_cont_pu_objects))))
		return (continue_cont_pu_objects *) NULL;
	if (!ip->cont_pu)
		ip->cont_pu = continue_cont_pu_cont_pu_create(ip, owner);
	if (!ip->cont_cntl)
		ip->cont_cntl = continue_cont_pu_cont_cntl_create(ip, ip->cont_pu);
	if (!ip->mode)
		ip->mode = continue_cont_pu_mode_create(ip, ip->cont_cntl);
	if (!ip->forward)
		ip->forward = continue_cont_pu_forward_create(ip, ip->cont_cntl);
	if (!ip->back)
		ip->back = continue_cont_pu_back_create(ip, ip->cont_cntl);
	if (!ip->contnu)
		ip->contnu = continue_cont_pu_contnu_create(ip, ip->cont_cntl);
	if (!ip->check)
		ip->check = continue_cont_pu_check_create(ip, ip->cont_cntl);
	if (!ip->clear)
		ip->clear = continue_cont_pu_clear_create(ip, ip->cont_cntl);
	if (!ip->statepu)
		ip->statepu = continue_cont_pu_statepu_create(ip, ip->cont_cntl);
	if (!ip->iters)
		ip->iters = continue_cont_pu_iters_create(ip, ip->cont_cntl);
	if (!ip->settings)
		ip->settings = continue_cont_pu_settings_create(ip, ip->cont_cntl);
	if (!ip->parafix)
		ip->parafix = continue_cont_pu_parafix_create(ip, ip->cont_cntl);
	if (!ip->jacupdate)
		ip->jacupdate = continue_cont_pu_jacupdate_create(ip, ip->cont_cntl);
	if (!ip->abserr)
		ip->abserr = continue_cont_pu_abserr_create(ip, ip->cont_cntl);
	if (!ip->relerr)
		ip->relerr = continue_cont_pu_relerr_create(ip, ip->cont_cntl);
	if (!ip->minstp)
		ip->minstp = continue_cont_pu_minstp_create(ip, ip->cont_cntl);
	if (!ip->maxstp)
		ip->maxstp = continue_cont_pu_maxstp_create(ip, ip->cont_cntl);
	if (!ip->htan)
		ip->htan = continue_cont_pu_htan_create(ip, ip->cont_cntl);
	if (!ip->debuglvl)
		ip->debuglvl = continue_cont_pu_debuglvl_create(ip, ip->cont_cntl);
	if (!ip->search)
		ip->search = continue_cont_pu_search_create(ip, ip->cont_cntl);
        if (!ip->cview)
		ip->cview = continue_cont_pu_cview_create(ip, ip->cont_cntl);
	return ip;
}

/*
 * Create object `cont_pu' in the specified instance.

 */
Xv_opaque
continue_cont_pu_cont_pu_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
        Xv_opaque       win_image;
	extern void     cont_done();

	static unsigned short   win_bits[] = {
#include "cont.icon"
			};

	win_image = xv_create(XV_NULL, SERVER_IMAGE,
			SERVER_IMAGE_BITS, win_bits,
			SERVER_IMAGE_DEPTH, 1,
			XV_WIDTH, 64,
			XV_HEIGHT, 64, NULL);
	
/*	obj = xv_create(owner, FRAME_CMD, */
	obj = xv_create(owner, FRAME,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 379,
		XV_HEIGHT, 295,
		XV_LABEL, "Equilibrium Continuation",
		XV_SHOW, FALSE,
		FRAME_CLOSED, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
/*		FRAME_CMD_PUSHPIN_IN, TRUE,  */
		FRAME_DONE_PROC, cont_done,
		FRAME_ICON, xv_create(XV_NULL, ICON,
		ICON_IMAGE, win_image,NULL),
		NULL);
/*	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL); */
	return obj;
}

/*
 * Create object `cont_cntl' in the specified instance.

 */
Xv_opaque
continue_cont_pu_cont_cntl_create(ip, owner)
	caddr_t		ip;
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
/*	gcm_initialize_colors(obj, "Light Steel Blue", NULL); */
	return obj;
}

/*
 * Create object `mode' in the specified instance.

 */
Xv_opaque
continue_cont_pu_mode_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	int		i;
	extern int	cont_mode_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 75,                                /* XV_Y, 64, */
		XV_WIDTH, 68,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 67,
		PANEL_VALUE_Y, 75,                       /* PANEL_VALUE_Y, 64, */
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, N_Cont,
		PANEL_LABEL_STRING, "Mode: ",
		PANEL_NOTIFY_PROC, cont_mode_notify,
		PANEL_CHOICE_STRINGS,
			"",
			0,
		NULL);
        
	for(i=0; i<N_Cont; i++)
	  xv_set(obj, PANEL_CHOICE_STRING, i, Cont_Sel[i].Cont_Name, NULL);

	return obj;
}

/*
 * Create object `forward' in the specified instance.

 */
Xv_opaque
continue_cont_pu_forward_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		forwd_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 15,
		XV_WIDTH, 68,
		XV_HEIGHT, 19,
		PANEL_ITEM_COLOR, gcm_color_index("Black"),
		PANEL_LABEL_STRING, "Forward",
		PANEL_NOTIFY_PROC, forwd_notify,
		NULL);
	return obj;
}

/*
 * Create object `back' in the specified instance.

 */
Xv_opaque
continue_cont_pu_back_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		backwd_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 170,
		XV_Y, 15,
		XV_WIDTH, 82,
		XV_HEIGHT, 19,
		PANEL_ITEM_COLOR, gcm_color_index("Black"),
		PANEL_LABEL_STRING, "Backwards",
		PANEL_NOTIFY_PROC, backwd_notify,
		NULL);
	return obj;
}

/*
 * Create object `check' in the specified instance.

 */
Xv_opaque
continue_cont_pu_check_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		check_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 28,
		XV_Y, 105,
		XV_WIDTH, 116,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 128,
		PANEL_VALUE_Y, 105,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Monitor Switch:",
		PANEL_NOTIFY_PROC, check_notify,
		PANEL_CHOICE_STRINGS,
			"Off",
			"On",
			0,
		NULL);
	return obj;
}

/*
 * Create object `clear' in the specified instance.

 */
Xv_opaque
continue_cont_pu_clear_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		clr_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 302,
		XV_Y, 15,
		XV_WIDTH, 50,
		XV_HEIGHT, 19,
		PANEL_INACTIVE, TRUE,
		PANEL_LABEL_STRING, "Clear",
		PANEL_NOTIFY_PROC, clr_notify,
		NULL);
	return obj;
}


/*
 * Create object `statepu' in the specified instance.

 */
Xv_opaque
continue_cont_pu_statepu_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		statepu_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 300,
		XV_Y, 45,
		XV_WIDTH, 50,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "State...",
		PANEL_NOTIFY_PROC, statepu_notify,
		NULL);
	return obj;
}



/*
 * Create object `iters' in the specified instance.

 */
Xv_opaque
continue_cont_pu_iters_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	iters_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 55,
		XV_WIDTH, 147,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Iterations: ",
		PANEL_VALUE_X, 97,
		PANEL_VALUE_Y, 55,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, iters_notify,
		NULL);
	return obj;
}

/*
 * Create object `settings' in the specified instance.

 */
Xv_opaque
continue_cont_pu_settings_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 250,
		XV_WIDTH, 84,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 83,
		PANEL_VALUE_Y, 250,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Settings:",
		PANEL_NOTIFY_PROC, settings_notify,
		PANEL_CHOICE_STRINGS,
			"Hide",
			"Show",
			0,
		NULL);
	return obj;
}

/*
 * Create object `parafix' in the specified instance.

 */
Xv_opaque
continue_cont_pu_parafix_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		parafix_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 300,
		XV_WIDTH,113 ,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 85,
		PANEL_VALUE_Y, 300,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Para Fix: ",
		PANEL_NOTIFY_PROC, parafix_notify,
		PANEL_CHOICE_STRINGS,
			"Vary",
			"Fix",
			0,
		NULL);
	return obj;
}

/*
 * Create object `jacupdate' in the specified instance.

 */
Xv_opaque
continue_cont_pu_jacupdate_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		jacupdate_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 330,
		XV_WIDTH, 122,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 121,
		PANEL_VALUE_Y, 330,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Jacob Update: ",
		PANEL_NOTIFY_PROC, jacupdate_notify,
		PANEL_CHOICE_STRINGS,
			"Each Step",
			"Initial Step",
			0,
		NULL);
	return obj;
}

/*
 * Create object `abserr' in the specified instance.

 */
Xv_opaque
continue_cont_pu_abserr_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	abserr_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 360,
		XV_WIDTH, 235,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Absolute Error: ",
		PANEL_VALUE_X, 130,
		PANEL_VALUE_Y, 360,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, abserr_notify,
		NULL);
	return obj;
}

/*
 * Create object `relerr' in the specified instance.

 */
Xv_opaque
continue_cont_pu_relerr_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	relerr_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 385,
		XV_WIDTH, 234,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Relative Error:  ",
		PANEL_VALUE_X, 129,
		PANEL_VALUE_Y, 385,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, relerr_notify,
		NULL);
	return obj;
}

/*
 * Create object `minstp' in the specified instance.

 */
Xv_opaque
continue_cont_pu_minstp_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	minstp_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 410,
		XV_WIDTH, 236,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Minimum Step: ",
		PANEL_VALUE_X, 131,
		PANEL_VALUE_Y, 410,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, minstp_notify,
		NULL);
	return obj;
}

/*
 * Create object `maxstp' in the specified instance.

 */
Xv_opaque
continue_cont_pu_maxstp_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	maxstp_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 435,
		XV_WIDTH, 237,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Maximum Step: ",
		PANEL_VALUE_X, 132,
		PANEL_VALUE_Y, 435,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, maxstp_notify,
		NULL);
	return obj;
}

/*
 * Create object `htan' in the specified instance.

 */
Xv_opaque
continue_cont_pu_htan_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	htan_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 460,
		XV_WIDTH, 238,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Sugg. Stepsize:  ",
		PANEL_VALUE_X, 133,
		PANEL_VALUE_Y, 460,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, htan_notify,
		NULL);
	return obj;
}

/*
 * Create object `debuglvl' in the specified instance.

 */
Xv_opaque
continue_cont_pu_debuglvl_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	debuglvl_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 485,
		XV_WIDTH, 239,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Debug Level:     ",
		PANEL_VALUE_X, 134,
		PANEL_VALUE_Y, 485,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, debuglvl_notify,
		NULL);
	return obj;
}

/*
 * Create object `contnu' in the specified instance.

*/
Xv_opaque
continue_cont_pu_contnu_create(ip, owner)
caddr_t         ip;
Xv_opaque       owner;
{
	Xv_opaque       obj;
	extern void             contnu_notify();

	obj = xv_create(owner, PANEL_BUTTON,
			XV_KEY_DATA, INSTANCE, ip,
			XV_X, 90,
			XV_Y, 15,
			XV_WIDTH, 74,
			XV_HEIGHT, 19,
		        PANEL_NOTIFY_PROC, contnu_notify,
			PANEL_LABEL_STRING, "Continue", NULL);
	return
	obj;
}



/*
 * Create object `contpara' in the specified instance.

 */
Xv_opaque
contpara_create(ip, owner, n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	extern int	contpara_notify();
	Xv_opaque	obj;

	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 260,  
		XV_Y, 250,
		XV_WIDTH, 72,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 290, 
		PANEL_VALUE_Y, 250,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "Cont. Para:",
		PANEL_NOTIFY_PROC, contpara_notify,
		PANEL_CHOICE_STRINGS,
			" ",
			0,
		NULL);
	return obj;
}




/*
 * Create object `augparam' in the specified instance.

*/
Xv_opaque
augparam_create(ip, owner)
caddr_t         ip;
Xv_opaque       owner;
{
	Xv_opaque       obj;
	extern int	augparam_notify();

	obj = xv_create(owner, PANEL_LIST, XV_KEY_DATA, INSTANCE, ip,
		XV_X, 15,
		XV_Y, 140,
		PANEL_LIST_WIDTH, 200,
		XV_HEIGHT, 74,
		PANEL_LABEL_STRING, "Aug. Parameters: ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LIST_DISPLAY_ROWS, 4,
		PANEL_READ_ONLY, FALSE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, augparam_notify,
		NULL);
	return
	obj;
}


/*
 * Create object `search' in the specified instance. 
 */
Xv_opaque
continue_cont_pu_search_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		search_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 300,
		XV_Y, 105,
		XV_WIDTH, 90,
		XV_HEIGHT, 19,
		/* PANEL_INACTIVE, TRUE, */
		PANEL_LABEL_STRING, "Search",
		PANEL_NOTIFY_PROC, search_notify,
		NULL);
	return obj;
}



/*
 * Create object `cview' in the specified instance.
 */
Xv_opaque
continue_cont_pu_cview_create(ip, owner)
        caddr_t         ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern void		cview_notify();
	
	obj = xv_create(owner, PANEL_TOGGLE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 185,
		XV_Y, 300,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "View Type: ",
		PANEL_NOTIFY_PROC, cview_notify,
		PANEL_CHOICE_STRINGS,
			"0",
			"1",
			"2",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	return obj;
}



