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
#ifndef	autoif_HEADER
#define	autoif_HEADER

extern Attr_attribute	INSTANCE;

typedef struct {
	Xv_opaque	coautord;
	Xv_opaque	coautocntl;
	Xv_opaque	coarinfo;
	Xv_opaque	coaradd;
	Xv_opaque	coargo;
	Xv_opaque	coartype;
	Xv_opaque	coarmode;
	Xv_opaque	coarcurblk;
	Xv_opaque	coaritp;
	Xv_opaque	coarsh;
	Xv_opaque	coaraatm;
	Xv_opaque	coaribr;
	Xv_opaque	coarntpl;
	Xv_opaque	coarntot;
	Xv_opaque	coarnar;
	Xv_opaque	coarlab;
	Xv_opaque	coarntst;
	Xv_opaque	coarnfpar;
	Xv_opaque	coarncol;
	Xv_opaque	coarisw;
	Xv_opaque	coarnpar;
	Xv_opaque	coartblks;
	Xv_opaque	coardir;
	Xv_opaque	coarfile;
	Xv_opaque	coarftype;
} autoif_coautord_objects;

extern autoif_coautord_objects	*autoif_coautord_objects_initialize();

extern Xv_opaque	autoif_coautord_coautord_create();
extern Xv_opaque	autoif_coautord_coautocntl_create();
extern Xv_opaque	autoif_coautord_coarinfo_create();
extern Xv_opaque	autoif_coautord_coaradd_create();
extern Xv_opaque	autoif_coautord_coargo_create();
extern Xv_opaque	autoif_coautord_coartype_create();
extern Xv_opaque	autoif_coautord_coarmode_create();
extern Xv_opaque	autoif_coautord_coarcurblk_create();
extern Xv_opaque	autoif_coautord_wr_create();
extern Xv_opaque	autoif_coautord_coarsh_create();
extern Xv_opaque	autoif_coautord_coaraatm_create();
extern Xv_opaque	autoif_coautord_coardir_create();
extern Xv_opaque	autoif_coautord_coarfile_create();
extern Xv_opaque	autoif_coautord_coarftype_create();
#endif
