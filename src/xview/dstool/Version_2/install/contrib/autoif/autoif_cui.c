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
#include "autoif_cui.h"


/*
 * Initialize an instance of object `coautord'.
 */
autoif_coautord_objects *
autoif_coautord_objects_initialize(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
        /* Popup & Cntl Items */

	if (!ip && !(ip = (autoif_coautord_objects *) calloc(1, sizeof (autoif_coautord_objects))))
		return (autoif_coautord_objects *) NULL;
	if (!ip->coautord)
		ip->coautord = autoif_coautord_coautord_create(ip, owner);
	if (!ip->coautocntl)
		ip->coautocntl = autoif_coautord_coautocntl_create(ip, ip->coautord);

        /* Action Buttons */

	if (!ip->coarinfo)
		ip->coarinfo = autoif_coautord_coarinfo_create(ip, ip->coautocntl);
	if (!ip->coaradd)
		ip->coaradd = autoif_coautord_coaradd_create(ip, ip->coautocntl);
	if (!ip->coargo)
		ip->coargo = autoif_coautord_coargo_create(ip, ip->coautocntl);

        /* Choice Items */     

	if (!ip->coartype)
		ip->coartype = autoif_coautord_coartype_create(ip, ip->coautocntl);
	if (!ip->coarmode)
		ip->coarmode = autoif_coautord_coarmode_create(ip, ip->coautocntl);
	if (!ip->coarsh)
		ip->coarsh = autoif_coautord_coarsh_create(ip, ip->coautocntl);
	if (!ip->coaraatm)
		ip->coaraatm = autoif_coautord_coaraatm_create(ip, ip->coautocntl);
	if (!ip->coarftype)
		ip->coarftype = autoif_coautord_coarftype_create(ip, ip->coautocntl);

        /* Text Items */

	if (!ip->coarcurblk)
		ip->coarcurblk = autoif_coautord_wr_create(ip, ip->coautocntl, "Entry:", 16, 128, 8, TRUE, FALSE);
	if (!ip->coaritp)
		ip->coaritp = autoif_coautord_wr_create(ip, ip->coautocntl, "Type:", 16, 152, 24, FALSE, TRUE);
	if (!ip->coaribr)
		ip->coaribr = autoif_coautord_wr_create(ip, ip->coautocntl, "Ibr:    ", 16, 304, 8, TRUE, TRUE);
	if (!ip->coarntpl)
		ip->coarntpl = autoif_coautord_wr_create(ip, ip->coautocntl, "Ntpl: ", 160, 304, 8, TRUE, TRUE);
	if (!ip->coarntot)
		ip->coarntot = autoif_coautord_wr_create(ip, ip->coautocntl, "Ntot:  ", 16, 328, 8, TRUE, TRUE);
	if (!ip->coarnar)
		ip->coarnar = autoif_coautord_wr_create(ip, ip->coautocntl, "Nar:  ", 160, 328, 8, TRUE, TRUE);
	if (!ip->coarlab)
		ip->coarlab = autoif_coautord_wr_create(ip, ip->coautocntl, "Lab:   ", 16, 352, 8, TRUE, TRUE);
	if (!ip->coarntst)
		ip->coarntst = autoif_coautord_wr_create(ip, ip->coautocntl, "Ntst: ", 160, 352, 8, TRUE, TRUE);
	if (!ip->coarnfpar)
		ip->coarnfpar = autoif_coautord_wr_create(ip, ip->coautocntl, "Nfpar:", 16, 376, 8, TRUE, TRUE);
	if (!ip->coarncol)
		ip->coarncol = autoif_coautord_wr_create(ip, ip->coautocntl, "Ncol: ", 160, 376, 8, TRUE, TRUE);
	if (!ip->coarisw)
		ip->coarisw = autoif_coautord_wr_create(ip, ip->coautocntl, "Isw:    ", 16, 400, 8, TRUE, TRUE);
	if (!ip->coarnpar)
		ip->coarnpar = autoif_coautord_wr_create(ip, ip->coautocntl, "Npar: ", 160, 400, 8, TRUE, FALSE);
	if (!ip->coartblks)
		ip->coartblks = autoif_coautord_wr_create(ip, ip->coautocntl, "Total Entries:", 16, 440, 8, TRUE, TRUE);
	if (!ip->coardir)
		ip->coardir = autoif_coautord_wr_create(ip, ip->coautocntl, "Directory:", 16, 472, 15, TRUE, FALSE);
	if (!ip->coarfile)
		ip->coarfile = autoif_coautord_wr_create(ip, ip->coautocntl, "Filename:", 16, 496, 15, TRUE, FALSE);

	return ip;
}

/*
 * Create object `coautord' in the specified instance.
 */
Xv_opaque
autoif_coautord_coautord_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	void		autoif_done_proc(); 
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 307,
		XV_HEIGHT, 210,
		XV_LABEL, "AUTO Graphics Interface",
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, autoif_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `coautocntl' in the specified instance.
 */
Xv_opaque
autoif_coautord_coautocntl_create(ip, owner)
	autoif_coautord_objects	*ip;
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
 * Create object `coarinfo' in the specified instance.
 */
Xv_opaque
autoif_coautord_coarinfo_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		coarinfo_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Read Info",
		PANEL_NOTIFY_PROC, coarinfo_notify,
		NULL);
	return obj;
}

/*
 * Create object `coaradd' in the specified instance.
 */
Xv_opaque
autoif_coautord_coaradd_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		coaradd_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 104,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Add to Mem",
		PANEL_NOTIFY_PROC, coaradd_notify,
		NULL);
	return obj;
}

/*
 * Create object `coargo' in the specified instance.
 */
Xv_opaque
autoif_coautord_coargo_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		coargo_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 216,
		XV_Y, 16,
		PANEL_LABEL_STRING, "Go",
		PANEL_NOTIFY_PROC, coargo_notify,
		NULL);
	return obj;
}

/*
 * Create object `coartype' in the specified instance.
 */
Xv_opaque
autoif_coautord_coartype_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		autoif_coautord_coartype_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TOGGLE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 56,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Data:",
		PANEL_NOTIFY_PROC, autoif_coautord_coartype_notify_callback,
		PANEL_CHOICE_STRINGS,
			"Trajectory",
			"Continuation",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	return obj;
}

/*
 * Create object `coarmode' in the specified instance.
 */
Xv_opaque
autoif_coautord_coarmode_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		autoif_coautord_coarmode_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 88,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Mode:",
		PANEL_NOTIFY_PROC, autoif_coautord_coarmode_notify_callback,
		PANEL_CHOICE_STRINGS,
			"Search",
			"Read All",
			NULL,
		NULL);
	return obj;
}



/*
 * Create object `coarsh' in the specified instance.
 */
Xv_opaque
autoif_coautord_coarsh_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		autoif_coautord_coarsh_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 176,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Settings:",
		PANEL_NOTIFY_PROC, autoif_coautord_coarsh_notify_callback,
		PANEL_CHOICE_STRINGS,
			"Hide",
			"Show",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `coaraatm' in the specified instance.
 */
Xv_opaque
autoif_coautord_coaraatm_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void		autoif_coautord_coaraatm_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 264,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Auto Add-to-Mem",
		PANEL_NOTIFY_PROC, autoif_coautord_coaraatm_notify_callback,
		PANEL_CHOICE_STRINGS,
			"Off",
			"On",
			NULL,
		NULL);
	return obj;
}




/*
 * Create object `coarftype' in the specified instance.
 */
Xv_opaque
autoif_coautord_coarftype_create(ip, owner)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
{
	extern void             autoif_coautord_coarftype_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 224,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "File Type: ",
		PANEL_NOTIFY_PROC, autoif_coautord_coarftype_notify_callback,
		PANEL_CHOICE_STRINGS,
			"BV",
			"Algebraic",
			NULL,
		NULL);
	return obj;
}




/*
 * Create text field object in the specified instance.
 */
Xv_opaque
autoif_coautord_wr_create(ip, owner, label, x, y, length, show_line, access_mode)
	autoif_coautord_objects	*ip;
	Xv_opaque	owner;
	int		x, y, length, show_line, access_mode;
	char		*label;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, x,
		XV_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, length,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, label,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_UNDERLINED, show_line,
		PANEL_READ_ONLY, access_mode, 
		NULL);
	return obj;
}



