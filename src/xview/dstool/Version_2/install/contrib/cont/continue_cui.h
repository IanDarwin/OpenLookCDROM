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
#ifndef	continue_HEADER
#define	continue_HEADER


extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	cont_pu;
	Xv_opaque	cont_cntl;
	Xv_opaque	mode;
	Xv_opaque	forward;
	Xv_opaque	back;
	Xv_opaque	check;
	Xv_opaque	clear;
	Xv_opaque	statepu;
	Xv_opaque	iters;
	Xv_opaque	settings;
	Xv_opaque	parafix;
	Xv_opaque	jacupdate;
	Xv_opaque	abserr;
	Xv_opaque	relerr;
	Xv_opaque	minstp;
	Xv_opaque	maxstp;
	Xv_opaque	htan;
	Xv_opaque	debuglvl;
	Xv_opaque       augparam;
	Xv_opaque	contpara;
	Xv_opaque       contnu;
	Xv_opaque       search;
	Xv_opaque       cview;
} continue_cont_pu_objects;

extern continue_cont_pu_objects	*continue_cont_pu_objects_initialize();

extern Xv_opaque	continue_cont_pu_cont_pu_create();
extern Xv_opaque	continue_cont_pu_cont_cntl_create();
extern Xv_opaque	continue_cont_pu_mode_create();
extern Xv_opaque	continue_cont_pu_forward_create();
extern Xv_opaque	continue_cont_pu_back_create();
extern Xv_opaque	continue_cont_pu_check_create();
extern Xv_opaque	continue_cont_pu_clear_create();
extern Xv_opaque	continue_cont_pu_statepu_create();
extern Xv_opaque	continue_cont_pu_iters_create();
extern Xv_opaque	continue_cont_pu_settings_create();
extern Xv_opaque	continue_cont_pu_parafix_create();
extern Xv_opaque	continue_cont_pu_jacupdate_create();
extern Xv_opaque	continue_cont_pu_abserr_create();
extern Xv_opaque	continue_cont_pu_relerr_create();
extern Xv_opaque	continue_cont_pu_minstp_create();
extern Xv_opaque	continue_cont_pu_maxstp_create();
extern Xv_opaque	continue_cont_pu_htan_create();
extern Xv_opaque	continue_cont_pu_debuglvl_create();
extern Xv_opaque	continue_cont_pu_contnu_create();
extern Xv_opaque	contpara_create();
extern Xv_opaque	augparam_create();
extern Xv_opaque	continue_cont_pu_search_create();
extern Xv_opaque	continue_cont_pu_cview_create();

#endif
