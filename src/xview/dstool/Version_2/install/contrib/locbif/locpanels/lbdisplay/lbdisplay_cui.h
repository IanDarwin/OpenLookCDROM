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
#ifndef	lbdisplay_HEADER
#define	lbdisplay_HEADER

extern Attr_attribute	INSTANCE;

typedef struct {
	Xv_opaque	lbdisplaypu;
	Xv_opaque	lbdisplaycntl;
	Xv_opaque	lbdsoldot;
	Xv_opaque	lbdisound;
	Xv_opaque	lbdiflash;
	Xv_opaque	lbdmessag;
	Xv_opaque	lbdmaxnpt;
	Xv_opaque	lbdinit;
} lbdisplay_lbdisplaypu_objects;

extern lbdisplay_lbdisplaypu_objects	*lbdisplay_lbdisplaypu_objects_initialize();

extern Xv_opaque	lbdisplay_lbdisplaypu_lbdisplaypu_create();
extern Xv_opaque	lbdisplay_lbdisplaypu_lbdisplaycntl_create();
extern Xv_opaque	lbdisplay_rw_create();
#endif
