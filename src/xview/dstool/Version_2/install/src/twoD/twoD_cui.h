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
#ifndef	twoD_HEADER
#define	twoD_HEADER

extern Attr_attribute	INSTANCE;

extern Xv_opaque	twoD_optionsmenu_create();
extern Xv_opaque	twoD_sizemenu_create();

typedef struct {
	Xv_opaque	win;
	Xv_opaque	pan;
	Xv_opaque	hor;
	Xv_opaque	hormin;
	Xv_opaque	hormax;
	Xv_opaque	options;
	Xv_opaque	ver;
	Xv_opaque	vermin;
	Xv_opaque	vermax;
	Xv_opaque	canvas;
	Xv_opaque	controls1;
	Xv_opaque	cbar_lt;
	Xv_opaque	twoD_cbar;
	Xv_opaque	controls2;
	Xv_opaque	cbar_rt;
	Xv_opaque       cursorpos;
} twoD_win_objects;

extern twoD_win_objects	*twoD_win_objects_initialize();

extern Xv_opaque	twoD_win_win_create();
extern Xv_opaque	twoD_win_pan_create();
extern Xv_opaque	twoD_win_hor_create();
extern Xv_opaque	twoD_win_hormin_create();
extern Xv_opaque	twoD_win_hormax_create();
extern Xv_opaque	twoD_win_options_create();
extern Xv_opaque	twoD_win_ver_create();
extern Xv_opaque	twoD_win_vermin_create();
extern Xv_opaque	twoD_win_vermax_create();
extern Xv_opaque	twoD_win_canvas_create();
extern Xv_opaque	twoD_win_controls1_create();
extern Xv_opaque	twoD_win_cbar_lt_create();
extern Xv_opaque	twoD_win_twoD_cbar_create();
extern Xv_opaque	twoD_win_controls2_create();
extern Xv_opaque	twoD_win_cbar_rt_create();
extern Xv_opaque        twoD_win_cursorpos_create();
#endif
