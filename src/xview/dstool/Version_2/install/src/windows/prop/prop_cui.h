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
#ifndef	prop_HEADER
#define	prop_HEADER

extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	win;
	Xv_opaque	pan;
	Xv_opaque	prop_select;
	Xv_opaque	*prop_usr_select;
	Xv_opaque	*dfields;
	Xv_opaque	*ifields;
} prop_win_objects;

extern prop_win_objects	*prop_win_objects_initialize();

extern Xv_opaque	prop_win_win_create();
extern Xv_opaque	prop_win_pan_create();
extern Xv_opaque	prop_win_dfields_create();
extern Xv_opaque	prop_win_ifields_create();
extern Xv_opaque	prop_select_create();
extern Xv_opaque	prop_usr_select_create();
#endif
