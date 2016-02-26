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
#ifndef	cntstate_HEADER
#define	cntstate_HEADER

extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	cntstatepu;
	Xv_opaque	cntl;
	Xv_opaque	sel;
	Xv_opaque	update;
	Xv_opaque	*state;
	Xv_opaque	*parm;
	Xv_opaque	*func;
	Xv_opaque	*charact;
	Xv_opaque	status;
} cntstate_objects;

extern cntstate_objects	   *cntstate_objects_initialize();

extern Xv_opaque	cntstate_cntstatepu_create();
extern Xv_opaque	cntstate_cntl_create();
extern Xv_opaque	cntstate_sel_create();
extern Xv_opaque	cntstate_update_create();
extern Xv_opaque	cntstate_state_create();
extern Xv_opaque	cntstate_parm_create();
extern Xv_opaque	cntstate_func_create();
extern Xv_opaque	cntstate_charact_create();
extern Xv_opaque	cntstate_status_create();
#endif
