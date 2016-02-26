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
#ifndef	orbit_HEADER
#define	orbit_HEADER

/*
 * orbit_cui.h - User interface object and function declarations.
 */

extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	win;
	Xv_opaque	pan;
	Xv_opaque	forwards;
	Xv_opaque	backwards;
	Xv_opaque	contin;
	Xv_opaque	clearlast;
	Xv_opaque	clearall;
	Xv_opaque       interrupt;
	Xv_opaque	start;
	Xv_opaque	stop;
	Xv_opaque	skip;
	Xv_opaque	stepsize;
	Xv_opaque	stop_cond;
	Xv_opaque	propagation;
	Xv_opaque	*stop_event;
} orbit_win_objects;

extern orbit_win_objects	*orbit_win_objects_initialize();

extern Xv_opaque	orbit_win_win_create();
extern Xv_opaque	orbit_win_pan_create();
extern Xv_opaque	orbit_win_forwards_create();
extern Xv_opaque	orbit_win_backwards_create();
extern Xv_opaque	orbit_win_contin_create();
extern Xv_opaque	orbit_win_clearlast_create();
extern Xv_opaque	orbit_win_clearall_create();
extern Xv_opaque	orbit_win_interrupt_create();
extern Xv_opaque	orbit_win_start_create();
extern Xv_opaque	orbit_win_stop_create();
extern Xv_opaque	orbit_win_skip_create();
extern Xv_opaque	orbit_win_stepsize_create();
extern Xv_opaque	orbit_win_stop_cond_create();
extern Xv_opaque	orbit_win_propagation_create();
extern Xv_opaque	orbit_win_stop_event_create();
#endif
