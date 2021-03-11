/* $XConsortium: phgtype.h,v 5.3 94/04/17 20:41:49 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef PHGTYPES_H_INCLUDED
#define PHGTYPES_H_INCLUDED

/* Common typedefs for the PHIGS library */

#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

#ifndef MIN
#define MIN( a, b)	(((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX( a, b)	(((a) > (b)) ? (a) : (b))
#endif
#ifndef FABS
#define FABS( f) 	(((f) < 0.0) ? -(f) : (f))
#endif

#define PHG_UNIT_VEC(v)		\
  { double len;							 \
    len = sqrt( (v)->x*(v)->x + (v)->y*(v)->y + (v)->z*(v)->z ); \
    (v)->x /= len, (v)->y /= len, (v)->z /= len;                 \
  }

#define PHG_DOT_PROD(v1, v2)	\
    ((v1)->x*(v2)->x + (v1)->y*(v2)->y + (v1)->z*(v2)->z)

#define PHG_MAG_V3( v) 		\
    (sqrt( (v)->delta_x * (v)->delta_x + (v)->delta_y * (v)->delta_y + (v)->delta_z * (v)->delta_z))

#define PHG_MAG_V2( v) \
    (sqrt( (v)->delta_x * (v)->delta_x + (v)->delta_y * (v)->delta_y))

#define PHG_ZERO_TOLERANCE		1.0e-30

#define PHG_NEAR_ZERO( s) \
    (_ABS(s) < PHG_ZERO_TOLERANCE)

#define PHG_ZERO_MAG( s) \
    ((s) < PHG_ZERO_TOLERANCE)

#ifndef Malloc
#define Malloc( _size )	malloc((unsigned)(_size))
#endif

typedef struct {
    char	*buf;	/* scratch buffer */
    unsigned	size;	/* size of buffer, in bytes */
} Phg_scratch;

#define PHG_SCRATCH_SPACE(_sc, _size) \
    ((_sc)->size >= (_size) ? (_sc)->buf \
	: phg_grow_scratch( (_sc), (unsigned)(_size) ))

typedef struct _Cp_struct	*Cp_handle;
typedef struct _Css_structel    *El_handle;
typedef struct _Css_ssl         *Struct_handle;
typedef struct _Css_struct      *Css_handle;
typedef struct _Ar_struct	*Ar_handle;
typedef struct _Ws		*Ws_handle;
typedef struct _Wst		*Wst_handle;
typedef struct _Sin_event_queue	*Input_q_handle;

typedef struct {
    Ppoint3	pt;		/* text point */
    Pvec3	dir[2];		/* direction vectors */
    Pint	length;		/* string length, including terminator! */
    char	*string;	/* text string */
} Phg_text3;

typedef struct {
    Ppoint	pt;		/* text point */
    Pint	length;		/* string length, including terminator! */
    char	*string;	/* text string */
} Phg_text;

typedef struct {
    Ppoint3	ref_pt;
    Ppoint3	offset;
    Pint	length;		/* string length, including terminator! */
    char	*string;
} Phg_anno_text_rel3;

typedef struct {
    Ppoint	ref_pt;
    Ppoint	offset;
    Pint	length;		/* string length, including terminator! */
    char	*string;
} Phg_anno_text_rel;

typedef struct { /* complete tx rep, including PHIGS+ and SUNPHIGS extensions*/
    Pint	font;	/* text font for codeset 0*/
    Pint	font1;	/* text font for codeset 1*/
    Pint	font2;	/* text font for codeset 2*/
    Pint	font3;	/* text font for codeset 3*/
    Ptext_prec	precision;	/* text precision */
    Pfloat	exp;	/* character expansion factor */
    Pfloat	space;	/* character spacing */
    Pgcolr	colour;	/* text colour index */
} Phg_tx_bundl;

typedef struct {
    Pint	charset0;
    Pint	charset1;
    Pint	charset2;
    Pint	charset3;
} Phg_charsets;

typedef struct {
    Pint		num_sets;
    Pint		total_pts;	/* total number of points */
    Ppoint_list3	*sets;
} Phg_fa_set3;

typedef struct {
    Pint		num_sets;
    Pint		total_pts;	/* total number of points */
    Ppoint_list		*sets;
} Phg_fa_set;

typedef struct {
    Pparal	paral;  		/* parallelogram */
    Pint_size	dim;			/* dimension */
    Pint	*colr;  		/* colour array */
} Phg_cell_array3;

typedef struct {
    Prect	rect;			/* rectangle */
    Pint_size	dim;			/* dimension */
    Pint	*colr;  		/* colour array */
} Phg_cell_array;

typedef struct {
    Pparal      paral;                 /* parallelogram */
    Pint_size	dim;                    /* dimension */
    Pint        colour_model;
    Pcoval      *colr;                  /* colour array */
} Phg_ext_cell_arr3;

typedef struct {
    Ppoint3	pt;			/* pattern ref. point */
    Pvec3	vecs[2];		/* vectors */
} Phg_pat_pt_vecs;

typedef struct {
    Paspect	attr_id;		/* attribute id */
    Pasf	asf;			/* asf */
} Phg_asf_info;

typedef struct {
    Pfloat		*mat3;		/* 3d transformation matrix */
    Pcompose_type	comptype;	/* composition type */
} Phg_local_xform3;

typedef struct {
    Pfloat		*mat;		/* 2d transformation matrix */
    Pcompose_type	comptype;	/* composition type */
} Phg_local_xform;

typedef struct {
    Pint		op;
    Phalf_space_list3	hsplst;
} Phg_mclip_vol3;

typedef struct {
    Pint		op;
    Phalf_space_list	hsplst;
} Phg_mclip_vol;

typedef struct {
    Pfloat		xyzw[4];
} Phg_p3pt;

/* NURB Curve */
typedef struct {
    Pint		order;
    Prational		rationality;
    Pfloat		tstart, tend;	/* parameter limits */
    Pfloat_list		knots;
    Pint		npts;
    Ppoint4		*points;
} Phg_nurb_curve_data;

typedef struct {
    Phg_nurb_curve_data	data;
} Phg_nurb_curve;

/* NURB Surface */

typedef struct {
    Pint		u_order;
    Pint		v_order;
    Prational		rationality;
    Pfloat_list		uknots;
    Pfloat_list		vknots;
    Ppcs_dims		npts;		/* number of surface control points */
    Ppoint4		*grid;		/* control points, array of 4D */
    Pint		nloops;		/* number of trim loops */
    Ptrimcurve_list	*trimloops;	/* array of trim loops */
    Pint		num_tcurves;	/* total number of all trim curves */
    Pint		num_tknots;	/* total number of all trim knots */
    Pint		num_2D_tpoints;	/* number of non-rational points */
    Pint		num_3D_tpoints;	/* number of rational points */
} Phg_nurb_surf_data;

typedef struct {
    Phg_nurb_surf_data	data;
} Phg_nurb_surf;

typedef struct {
    Pint		type;
    Pfloat		value;
} Phg_curv_approx;

typedef struct {
    Pint		type;
    Pfloat		u_val, v_val;
} Phg_surf_approx;

typedef struct {
    Pint		id;
    Pgse_data		rec;
} Phg_gse;

typedef struct {
    Pint		id;
    Ppoint_list3	pts;
    Pgdp_data3		rec;
} Phg_gdp3;

typedef struct {
    Pint		id;
    Ppoint_list		pts;
    Pgdp_data		rec;
} Phg_gdp;

typedef struct {
   Pint		vflag;		/* data per vertex */
   Pint		colour_model;   /* colour model */
   Pint		num_sets;	/* number of sets */
   Pint		num_vertices;	/* total number of vertices */
   Pfacet_vdata_list3 *vdata;	/* array of vertex data lists */
} Phg_pl_set3_d;

typedef struct {
   Pint			fflag;		/* data per facet type */
   Pint			vflag;		/* data per vertex type */
   Pint			eflag;		/* edge data flag */
   Pint			colour_model;   /* colour model */
   Pint			num_sets;	/* number of sets */
   Pint			num_vertices;	/* total number of vertices */
   Pfacet_data3		fdata;		/* facet data */
   Pfacet_vdata_list3	*vdata;		/* array of vertex data lists */
   Pedge_data_list	*edata;		/* array edge data lists */
} Phg_fa_set3_d;

typedef struct {
    Pint        	num_facets;	/* number of facets */
    Pfacet_data_arr3	facetdata;	/* array of  facet data */
} Phg_facet_data_list3;	/* facet data list */

typedef struct {
   Pint		fflag;	/* data per facet type */
   Pint		vflag;		/* data per vertex type */
   Pint		colour_model;   /* colour model */
   Phg_facet_data_list3 fdata;	/* facet list  */
   Pfacet_vdata_list3 vdata;	/* vertex list  */
} Phg_tri_str3;

typedef struct {
   Pint		fflag;	/* data per facet type */
   Pint		vflag;		/* data per vertex type */
   Pint		colour_model;   /* colour model */
   Pint_size	dim;		/* dimension of cells */
   Phg_facet_data_list3 fdata;	/* data per facet */
   Pfacet_vdata_list3 vdata;	/* vertex list */
} Phg_quad_mesh3;

typedef struct {
   Pint			fflag;		/* data per facet type */
   Pint			vflag;		/* data per vertex type */
   Pint			eflag;		/* edge data flag */
   Pint			colour_model;   /* colour model */
   Pint			num_sets;	/* number of fill area sets */
   int			num_contours;	/* total number of contours */
   int			num_vindices;	/* total number of vertex indices */
   Phg_facet_data_list3 	fdata;		/* data per facet  */
   Pedge_data_list_list	*edata;		/* flags for edges per vertex */
   Pfacet_vdata_list3 	vdata;		/* vertex list */ 
   Pint_list_list	*vlist;		/* vertex index lists */
} Phg_sofas3;

typedef struct {
   Pint		method;		/* shading method */
} Phg_line_sh_method;

typedef struct {
   Pint		method;		/* shading method */
} Phg_int_sh_method;

typedef struct {
   Pint_list	act_set;	/* activation list */
   Pint_list	deact_set;	/* deactivation list */
} Phg_light_src_state;

typedef struct {
    Pint		type;
    Ppara_surf_characs	data;
} Phg_para_surf_charac; /* parametric surface characteristics */

typedef union {
    Pint		idata;		/* integer valued data */
    Pfloat		fdata;		/* float valued data */
    Ppoint_list3	ptlst3;		/* list of 3d points */
    Ppoint_list		ptlst;		/* list of 2d points */
    Phg_text3		text3;		/* text parameters */
    Phg_text		text;		/* text parameters */
    Phg_fa_set3		fa_set3;	/* fill area set data */
    Phg_fa_set		fa_set;		/* fill area set data */
    Phg_cell_array3 	cell_array3;	/* cell array data */
    Phg_cell_array 	cell_array;	/* cell array data */
    Phg_ext_cell_arr3	ext_cell_arr3;	/* extended cell array data */
    Ptext_prec		txprec;		/* text precision */
    Pvec		char_up;	/* char up vector */
    Ptext_path		txpath;		/* text path */
    Ptext_align		txalign;	/* text alignment */
    Pint_style		interstyle;	/* interior style */
    Pedge_flag		edgef;		/* edge flag */
    Ppoint		pt;		/* pat. size; pat ref pt */
    Phg_pat_pt_vecs 	pat_pt_vecs;	/* pattern ref. point, vectors */
    Pint_list		name_set;	/* name sets */
    Phg_asf_info	asf_info;	/* asf info */
    Phg_local_xform3 	local_xform3;	/* 3d xform matrix + composition type */
    Phg_local_xform 	local_xform;	/* 2d xform matrix + composition type */
    Pfloat		*mat3;		/* 3d transformation matrix */
    Pfloat		*mat;		/* 2d transformation matrix */
    Phg_gdp3		gdp3;		/* gdp3 data */
    Phg_gdp		gdp;		/* gdp data */
    Phg_gse		gse;		/* gse data */
    Pdata		appl_data;	/* application data */
    Phg_mclip_vol3	mclip_vol3;	/* operator + half spaces */
    Phg_mclip_vol	mclip_vol;	/* operator + half spaces */
    Pclip_ind		clip_ind;	/* clipping indicator */
    Phg_anno_text_rel3	anno_text_rel3;	/* points and text */
    Phg_anno_text_rel	anno_text_rel;	/* points and text */
    Phg_nurb_curve	nurb_curve;	/* NURB curve */
    Phg_nurb_surf	nurb_surf;	/* NURB surface */
    Phg_curv_approx	curv_approx;	/* curve approximation criteria*/
    Phg_surf_approx	surf_approx;	/* surface approximation criteria */
    Phg_pl_set3_d	pl_set3_d;	/* polyline set 3 with data */
    Phg_fa_set3_d	fa_set3_d;	/* fill area set 3 with data */
    Phg_tri_str3	tri_strip3;	/* triangle strip 3 with data */
    Phg_quad_mesh3	quad_mesh3;	/* quadralateral mesh 3 with data */
    Phg_sofas3		sofas3;		/* set of fill area set 3 with data */
    Phg_line_sh_method	line_shad;	/* line shading method */
    Phg_int_sh_method	int_shad;	/* interior shading method */
    Prefl_props		refl_props;	/* surface area properties */
    Phg_light_src_state	light_state;	/* light source state */
    Pgcolr		colour;		/* general colour */
    Pcull_mode		cullmode;	/* culling mode */
    Pdisting_mode	distgmode;	/* distingguishing mode */
    Phg_para_surf_charac psc;
} Phg_el_data;

typedef struct {
    unsigned		size;
    pexElementInfo	*oc;	/* points to the whole OC */
} Phg_pex_oc;

typedef struct {
    Pelem_type		op;
    Phg_el_data		data;
} Phg_el_info;

typedef struct {
    int		length;
    char	*string;
} Phg_string;

typedef struct {
    Pint	view_ind;	/* view index */
    Ppoint3	position;	/* point */
} Ploc3;

typedef struct {
    Pint	view_ind;	/* view index */
    Ppoint	position;	/* point */
} Ploc;

typedef struct {
    Pint	view_ind;	/* view index */
    Pint	num_points;	/* number of points in stroke */
    Ppoint3	*points;	/* points in stroke */
} Pstroke3;

typedef struct {
    Pint	view_ind;	/* view index */
    Pint	num_points;	/* number of points in stroke */
    Ppoint	*points;	/* points in stroke */
} Pstroke;

typedef struct {
    Pin_status	status;	/* status of request */
    Pint	choice;	/* choice value */
} Pchoice;

typedef struct {
    Pin_status	status;	/* pick status */
    Ppick_path	pick_path;	/* pick path */
} Ppick;

typedef union {
        Ploc3		loc;
        Pstroke3	stk;
        Pfloat		val;
        Pchoice		chc;
        Ppick		pik;
        Phg_string	str;
} Phg_inp_event_data;

typedef struct {
    CARD16 	major_version;
    CARD16 	minor_version;
    CARD32 	release_number;
    CARD32 	subset_info;
    char	*vendor;
} Phg_pex_ext_info;

typedef struct {
    struct {
	unsigned coefs_calculated : 1;
    }			flags;
    /* Input: display-dependent constants */
    Pfloat	xr, yr, Yr;
    Pfloat	xg, yg, Yg;
    Pfloat	xb, yb, Yb;
    Pfloat	xw, yw, Yw;
    /* Cached: derived values */
    Pfloat	zr, zg, zb;
    Pfloat	Xw, Zw;
    Pfloat	k1, k2, k3, k4, k5, k6, k7, k8, k9;
    Pfloat	Tr, Tg, Tb;
} Phg_chroma_info;

typedef struct {
    Pint		method;
    Pcolr_map_data	rec;
} Phg_colr_map_rep;

/* Holdovers from the old binding. */
/* TODO: Prefix these names. */
typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    Ploc3	loc;	/* initial locator information */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Ploc_data3	record;	/* data record */
} Plocst3;

typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    Pstroke3	stroke;	/* initial stroke */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pstroke_data3	record;	/* data record */
} Pstrokest3;

typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    Pfloat	val;	/* initial value */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    int		counts[4];	/* string lengths of label strings */
    Pval_data3	record;	/* data record */
} Pvalst3;

typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    Pchoice	choice;	/* initial choice number and status */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pchoice_data3	record;	/* data record */
} Pchoicest3;

typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    Pint_list	inclusion_filter;	/* pick inclusion filter */
    Pint_list	exclusion_filter;	/* pick exclusion filter */
    Ppick	pick;	/* pick path and status */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Ppick_data3	record;	/* data record */
    Ppath_order	order;	/* path order */
} Ppickst3;

typedef struct {
    Pop_mode	mode;	/* operating mode */
    Pecho_switch	esw;	/* echo switch */
    char	*string;	/* initial string */
    Pint	pet;	/* prompt/echo type */
    Plimit3	e_volume;	/* echo volume */
    Pstring_data3	record;	/* data record */
} Pstringst3;

typedef struct {
    Pint	ws;	/* workstation identifier */
    Pin_class	class;	/* device class */
    Pint	dev;	/* logical input device number */
} Pevent;

#endif
