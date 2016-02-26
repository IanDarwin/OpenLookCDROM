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
#ifndef	lbcont_HEADER
#define	lbcont_HEADER

extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	lbcontpu;
	Xv_opaque	lbcontcntl;
	Xv_opaque	lbch0crv;
	Xv_opaque	lbcalgcrv;
	Xv_opaque	lbchmxcrv;
	Xv_opaque	lbcangcrv;
	Xv_opaque	lbcdhcrv;
	Xv_opaque	lbcdhjac;
	Xv_opaque	lbcmaxit;
	Xv_opaque	lbcepscrv;
	Xv_opaque	lbcmodit;
	Xv_opaque	lbcepscrs;
	Xv_opaque	lbcepszer;
	Xv_opaque	lbciprsng;
	Xv_opaque	lbcepsext;
} lbcont_lbcontpu_objects;

extern lbcont_lbcontpu_objects	*lbcont_lbcontpu_objects_initialize();

extern Xv_opaque	lbcont_lbcontpu_lbcontpu_create();
extern Xv_opaque	lbcont_lbcontpu_lbcontcntl_create();
extern Xv_opaque	lbcont_lbcontpu_rw_create();
extern Xv_opaque	lbcont_lbcontpu_lbcalgcrv_create();
extern Xv_opaque	lbcont_lbcontpu_lbcmaxit_create();
extern Xv_opaque	lbcont_lbcontpu_lbcmodit_create();
extern Xv_opaque	lbcont_lbcontpu_lbciprsng_create();
#endif
