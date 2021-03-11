/* $XConsortium: auto_box.c,v 5.5 94/04/17 20:44:09 hersh Exp $ */

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

#include <stdio.h>
#include <errno.h>
#include <math.h>

#ifndef M_PI
#define M_PI  3.14159265358979323846
#endif /* M_PI */


#ifdef USE_X_DRAWABLE
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#endif /*USE_X_DRAWABLE*/

#include <phigs/phigs.h>

/* Workstation ids */
#define WS1	1

/* Colors */
#define BLACK	0
#define WHITE	1
#define RED	2
#define GREEN	3
#define BLUE	4
#define YELLOW	5
#define CYAN	6
#define MAGENTA	7

/* Structure names */
#define ROOT 0
#define CUBE 1
#define SUB  2

/* Labels */
#define STYLE   1 
#define QUAD_1  101 
#define QUAD_2	102 
#define QUAD_3	103 
#define QUAD_4	104 
#define QUAD_1_1 105 
#define QUAD_4_1 106 

#define WIN_X	200
#define WIN_Y	200
#define WIN_W	400
#define WIN_H	400

#define PFILLAREASET3

#ifdef PFILLAREASET3
#define	PFILLAREA_CALL	pfill_area_set3( &side_list )
#else /*PFILLAREASET3*/
#define	PFILLAREA_CALL	pfill_area3( &side )
#endif /*PFILLAREASET3*/

#ifdef USE_X_DRAWABLE
extern void init_window();
#endif /*USE_X_DRAWABLE*/

/* Globals */
Ppoint3	    pt;
Pvec3	    shift;
Pfloat	    x_ang, y_ang, z_ang;
Pvec3	    scale;
Pmatrix3    bldmat, bldmat2, bldmat3, bldmat4;

static void
setup(n, mat)
int n;
float mat[4][4];
{
    int i, j;

    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++) {
	    mat[i][j] = 0;
	    if (i == j) mat[i][j] = 1.0;
	}
    }
} 

static void
rotate_views()
{
    float   theta;

    Pint err;

/*
 * ... enable editing in the replace mode -- this will be used to update
 *        rotation angles in 4 views
 */
    pset_disp_upd_st( WS1, PDEFER_WAIT, PMODE_UWOR);
    pset_edit_mode( PEDIT_REPLACE);
/*
 * ...  set some constants for building the matrix
 */
    pt.x =  10.; pt.y = 10.; pt.z = 10.;
    shift.delta_x = 0; shift.delta_y = 0; shift.delta_z = 0;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
/*
 * ...  make identity matrices
 */
    setup( 4, bldmat);
    setup( 4, bldmat2);
    setup( 4, bldmat3);
    setup( 4, bldmat4);
/*
 * ...  open the structure for editing
 */

    popen_struct( SUB);
/*
 * ... increment the angle in a postive angle
 */

    for (theta = 0; theta < 4 * M_PI; theta +=.4) {
/*
 * ...  set the pointer and the angles, 
 * ...	find the label, 
 * ...	build the matrix, 
 * ...	update the view
 */
	pset_elem_ptr( 0);
/*
 * ... viewport 1
 */
	x_ang = theta;
	y_ang = 0;
	z_ang = 0;
	pset_elem_ptr_label( 1);
	poffset_elem_ptr( 1);
	pbuild_tran_matrix3(	&pt, &shift, x_ang, y_ang, z_ang,
				&scale, &err, bldmat);
	pset_local_tran3( bldmat, PTYPE_REPLACE);
/*
 * ... viewport 2
 */
	x_ang = 0;
	y_ang = theta;

	pset_elem_ptr_label( 2);
	poffset_elem_ptr( 1);

	pbuild_tran_matrix3(	&pt, &shift, x_ang, y_ang, z_ang,
				&scale,&err,bldmat2);
	pset_local_tran3( bldmat2, PTYPE_REPLACE);
/*
 * ... viewport 3
 */

	y_ang = 0;
	z_ang = theta;

	pset_elem_ptr_label( 3);
	poffset_elem_ptr( 1);

	pbuild_tran_matrix3(	&pt, &shift, x_ang, y_ang, z_ang,
				&scale, &err, bldmat3);
	pset_local_tran3(bldmat3, PTYPE_REPLACE);
/*
 * ... viewport 4
 */
	x_ang = theta;
	y_ang = theta;
	z_ang = theta;

	pset_elem_ptr_label(4);
	poffset_elem_ptr(1);

	pbuild_tran_matrix3(	&pt, &shift, x_ang, y_ang, z_ang,
				&scale, &err, bldmat4);
	pset_local_tran3(bldmat4, PTYPE_REPLACE);
		
	pupd_ws(WS1, PFLAG_PERFORM);
    }
/*
 *  close the structure being edited
 */
    pclose_struct();
}

static void
scale_views(scale_factor)
float scale_factor;
{

    float  i;
    Pint err;

/*
 * ... enable editing in the replace mode -- this will be used to update
 *	the scale matrix in 4 views
 */
    pset_disp_upd_st( WS1, PDEFER_WAIT, PMODE_UWOR);
    pset_edit_mode( PEDIT_REPLACE);
/*
 * ...  open the structure for editing
 */

    popen_struct(SUB);
/*
 * ... increment the scale matrix 
 */

    for (i = 1; i < 4 ; i += .2) {
/*
 * ...  set the pointer and the angles, find the label, build the matrix, update
 * ...  the view
 */
	pset_elem_ptr(0);
/*
 * ... viewport 1
 */
	scale.delta_x = scale.delta_x * scale_factor; 
	scale.delta_y = scale.delta_y * scale_factor; 
	scale.delta_z = scale.delta_z * scale_factor;
	pset_elem_ptr_label( 1);
	poffset_elem_ptr( 1);
	pbuild_tran_matrix3(&pt, &shift, x_ang, y_ang, z_ang, 
			    &scale, &err, bldmat);
	pset_local_tran3( bldmat, PTYPE_REPLACE);
/*
 * ... viewport 2
 */
	pset_elem_ptr_label(2);
	poffset_elem_ptr(1);

	pbuild_tran_matrix3(&pt, &shift, x_ang, y_ang, z_ang,
			    &scale, &err, bldmat2);
	pset_local_tran3(bldmat2, PTYPE_REPLACE);
/*
 * ... viewport 3
 */

	pset_elem_ptr_label(3);
	poffset_elem_ptr(1);

	pbuild_tran_matrix3(&pt, &shift, x_ang, y_ang, z_ang,
			    &scale, &err, bldmat3);
	pset_local_tran3(bldmat3, PTYPE_REPLACE);
/*
 * ... viewport 4
 */

	pset_elem_ptr_label(4);
	poffset_elem_ptr(1);

	pbuild_tran_matrix3(&pt, &shift, x_ang, y_ang, z_ang,
			    &scale, &err, bldmat4);
	pset_local_tran3(bldmat4, PTYPE_REPLACE);

	pupd_ws(WS1, PFLAG_PERFORM);
    }
/*
 *  close the structure being edited
 */
    pclose_struct();
}

/*
 *
 *
 *
 */

static void
load_box()
{
    Ppoint3	p[5];
    Pvec3	xlv;
    Pmatrix3	mx;
    Pvec3	scale;
    Pvec3	shift;
    Pint	err;
    Pfloat	xang, yang, zang;
    Ppoint_list_list3 side_list;
    Ppoint_list3 side;

    side.num_points = 4;
    side.points = p;
    side_list.num_point_lists = 1;
    side_list.point_lists = &side;


    setup(4, mx);

    popen_struct(CUBE);
    plabel(STYLE);
    pset_int_style(PSTYLE_SOLID );
    pset_hlhsr_id(PHIGS_HLHSR_ID_ON );
/*
 * ... make a square to make a front and back of the cube
 */
    p[0].x = 5; p[0].y = 5; p[0].z = 5;
    p[1].x = 15; p[1].y = 5; p[1].z = 5;
    p[2].x = 15; p[2].y = 15; p[2].z = 5;
    p[3].x = 5; p[3].y = 15; p[3].z = 5;

/* front */
    xlv.delta_x = 0; xlv.delta_y = 0; xlv.delta_z = 10;
    ptranslate3( &xlv, &err, mx);
    pset_local_tran3( mx, PTYPE_REPLACE);

    pset_int_colr_ind( RED);
    PFILLAREA_CALL;

/* back */

    xlv.delta_x = 5; xlv.delta_y = 10; xlv.delta_z = 5;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
    xang = 3.14; yang = 0.0; zang = 0.0; 
    shift.delta_x = 0.; shift.delta_y = 0.; shift.delta_z = 0;
    pbuild_tran_matrix3(&xlv, &shift, xang, yang, zang,
			&scale, &err, mx);
    pset_local_tran3(mx, PTYPE_REPLACE);

    pset_int_colr_ind(CYAN);
    PFILLAREA_CALL;

/* right */
    xlv.delta_x = 5; xlv.delta_y = 5; xlv.delta_z = 5;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
    xang = 0.0; yang = 3.14*1.5; zang = 0.0; 
    shift.delta_x = 0.; shift.delta_y = 0.; shift.delta_z = 0;
    pbuild_tran_matrix3(&xlv, &shift, xang, yang, zang,
			&scale, &err, mx);
    pset_local_tran3(mx, PTYPE_REPLACE);

    pset_int_colr_ind(GREEN);
    PFILLAREA_CALL;

/* left */
    xlv.delta_x = 15; xlv.delta_y = 5; xlv.delta_z = 5;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0; 
    xang = 0.0; yang = -3.14*1.5; zang = 0.0; 
    shift.delta_x = 0.; shift.delta_y = 0.; shift.delta_z = 0;
    pbuild_tran_matrix3(&xlv, &shift, xang, yang, zang,
			&scale, &err, mx);
    pset_local_tran3(mx, PTYPE_REPLACE);

    pset_int_colr_ind(BLUE);
    PFILLAREA_CALL;

/* bottom */
    xlv.delta_x = 5; xlv.delta_y = 5; xlv.delta_z = 5;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
    xang = -1.5*3.14; yang = 0.0; zang = 0.0; 
    shift.delta_x = 0.; shift.delta_y = 0.; shift.delta_z = 0;
    pbuild_tran_matrix3(&xlv, &shift, xang, yang, zang,
			&scale, &err, mx);
    pset_local_tran3(mx, PTYPE_REPLACE);

    pset_int_colr_ind( YELLOW);
    PFILLAREA_CALL;

/* top */
    xlv.delta_x = 5; xlv.delta_y = 15; xlv.delta_z = 5;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
    xang = 1.5*3.14; yang = 0.0; zang = 0.0; 
    shift.delta_x = 0.; shift.delta_y = 0.; shift.delta_z = 0;
    pbuild_tran_matrix3(&xlv, &shift, xang, yang, zang,
			&scale, &err, mx);
    pset_local_tran3(mx, PTYPE_REPLACE);

    pset_int_colr_ind(MAGENTA);
    PFILLAREA_CALL;

    pclose_struct();
}
    
Pview_map3 view_map = {
    { -20.0, 20.0, -20.0, 20.0},
    { 0.0, .5, 0.5, 1.0, 0.0, 1.0},
    PTYPE_PERSPECT,
    { 0., 0., 1000.0},
    0.0,
    -40.0,
    40.0
};

Pview_map3 view_map2 = {
    { -20.0, 20.0, -20.0, 20.0},
    { 0.5, 1.0, 0.5, 1.0, 0.0, 1.0},
    PTYPE_PERSPECT,
    { 0., 0., 1000.0},
    0.0,
    -40.0,
    40.0
};

Pview_map3 view_map3 = {
    { -20.0, 20.0, -20.0, 20.0},
    { 0.0, .5, 0., .5, 0.0, 1.0},
    PTYPE_PERSPECT,
    { 0., 0., 1000.0},
    0.0,
    -40.0,
    40.0
};

Pview_map3 view_map4 = {
    { -20.0, 20.0, -20.0, 20.0},
    { 0.5, 1.0, 0.,.5, 0.0, 1.0},
    PTYPE_PERSPECT,
    { 0., 0., 1000.0},
    0.0,
    -40.0,
    40.0
};

static void
load_views(ws_id)
Pint ws_id; 
{
    static Ppoint3 vrp = {0., 0., 0 };	    /*  origin */
    static Pvec3 vpn = {1.0, 1.0, 1.0 };    /*  view plane normal */
    static Pvec3 vup = {0.0, 1.0, 0.0 };    /*  view up vector */
	
    Pview_rep3 vrep;			    /*  view structure */

    Pint err;

    peval_view_ori_matrix3( &vrp, &vpn, &vup, &err, vrep.ori_matrix);

    if (err != 0) (void) printf( "view orientation error ");

    peval_view_map_matrix3(&view_map, &err, vrep.map_matrix);

    if (err != 0) (void) printf( "view mapping error ");

    vrep.clip_limit =  view_map.proj_vp;
    vrep.xy_clip = PIND_NO_CLIP;
    vrep.back_clip = PIND_NO_CLIP;
    vrep.front_clip = PIND_NO_CLIP;
    pset_view_rep3( ws_id, 1, &vrep);
/*
 *   viewport # 2
 */
    peval_view_ori_matrix3( &vrp, &vpn, &vup, &err, vrep.ori_matrix);

    if (err != 0) (void) printf( "view orientation error ");

    peval_view_map_matrix3 ( &view_map2, &err, vrep.map_matrix);

    if (err != 0) (void) printf( "view mapping error ");

    vrep.clip_limit =  view_map2.proj_vp;
    vrep.xy_clip = PIND_NO_CLIP;
    vrep.back_clip = PIND_NO_CLIP;
    vrep.front_clip = PIND_NO_CLIP;
    pset_view_rep3( ws_id, 2, &vrep);
/*
 *   viewport # 3
 */
    peval_view_ori_matrix3( &vrp, &vpn, &vup, &err, vrep.ori_matrix);

    if (err != 0) (void) printf( "view orientation error ");

    peval_view_map_matrix3( &view_map3, &err, vrep.map_matrix);

    if (err != 0) (void) printf( "view mapping error ");

    vrep.clip_limit =  view_map3.proj_vp;
    vrep.xy_clip = PIND_NO_CLIP;
    vrep.back_clip = PIND_NO_CLIP;
    vrep.front_clip = PIND_NO_CLIP;
    pset_view_rep3( ws_id, 3, &vrep);
/*
 *   viewport # 4
 */
    peval_view_ori_matrix3( &vrp, &vpn, &vup, &err, vrep.ori_matrix);

    if (err != 0) (void) printf ("view orientation error ");

    peval_view_map_matrix3( &view_map4, &err, vrep.map_matrix);

    if (err != 0) (void) printf( "view mapping error ");

    vrep.clip_limit =  view_map4.proj_vp;
    vrep.xy_clip = PIND_NO_CLIP;
    vrep.back_clip = PIND_NO_CLIP;
    vrep.front_clip = PIND_NO_CLIP;
    pset_view_rep3( ws_id, 4, &vrep);
}
static void
build_css()
{
    Ppoint	line[2];
    Ppoint_list p_line;
    Pmatrix3	r1, r2, r3, r4;
    Ppoint	text_pt;
    char	text_str[32];

    setup( 4, r1);
    setup( 4, r2);
    setup( 4, r3);
    setup( 4, r4);

/*
 * ... open the root structure
 */
    popen_struct( ROOT);
    pset_hlhsr_mode( WS1, 1);
  
/*
 * ...  set up lines that split the viewports
 */
    pset_linewidth( 3.0);
    pset_line_colr_ind( WHITE);
    line[0].x = .5; line[0].y = 0.0; 
    line[1].x = .5; line[1].y = 1.0;
    p_line.num_points = 2;
    p_line.points = line;
    ppolyline( &p_line);

    line[0].x = 0.0; line[0].y = .5;
    line[1].x = 1.0; line[1].y = .5;
    ppolyline( &p_line);
/*
 * ... text to explain each viewport
 */
    pset_text_colr_ind( YELLOW);
    pset_text_font(-2);		    /* Server will use default font here */
    pset_char_ht( .025);
    text_pt.x = 0; text_pt.y = .52;
    strcpy( text_str, " X Rotation");
    plabel( QUAD_2);
    ptext( &text_pt, text_str);
   
    text_pt.x = .5; text_pt.y = .58;
    strcpy( text_str, " Y Rotation");
    plabel( QUAD_1);
    ptext( &text_pt, text_str);
    text_pt.y = .52;
    strcpy( text_str, " Backfaces Culled");
    plabel( QUAD_1_1 );
    ptext( &text_pt, text_str);

    text_pt.x = 0; text_pt.y = .02;
    strcpy( text_str, " Z Rotation");
    plabel( QUAD_3);
    ptext( &text_pt, text_str);

    text_pt.x = .5; text_pt.y = .08;
    strcpy( text_str, " XYZ Rotation");
    plabel( QUAD_4);
    ptext( &text_pt, text_str);
    text_pt.y = .02;
    strcpy( text_str, " Frontfaces Culled");
    plabel( QUAD_4_1 );
    ptext( &text_pt, text_str);

    pset_linewidth( 1.0);

    pexec_struct( SUB);
    pclose_struct();

    popen_struct( SUB);
    pset_view_ind( 1);
    plabel( 1);
    pset_local_tran3( r1, PTYPE_PRECONCAT);

    /* Cull no faces in this view */
    pset_face_cull_mode(PCULL_NONE);

    pexec_struct( CUBE);

    pset_view_ind( 2);
    plabel( 2);
    pset_local_tran3( r2, PTYPE_PRECONCAT);

    /* Cull backfaces in this view */
    pset_face_cull_mode(PCULL_BACKFACE);

    pexec_struct( CUBE);

    pset_view_ind( 3);
    plabel( 3);
    pset_local_tran3( r3, PTYPE_PRECONCAT);

    /* Cull no faces in this view */
    pset_face_cull_mode(PCULL_NONE);

    pexec_struct( CUBE);

    pset_view_ind( 4);
    plabel( 4);
    pset_local_tran3( r4, PTYPE_PRECONCAT);

    /* Cull frontfaces in this view */
    pset_face_cull_mode(PCULL_FRONTFACE);

    pexec_struct( CUBE);
    pclose_struct();
}

static void
edit_css()
{
    popen_struct(CUBE);
    pset_elem_ptr(0);
    pset_elem_ptr_label(STYLE);
    poffset_elem_ptr(1);
    pset_int_style(PSTYLE_HOLLOW);
/*
 * ... close the structure being edited
 */
    pclose_struct();
}

static void
relabel_quadrants()
{
    char text_str[32];
    Ppoint text_pt;
/*
 * ... relabel the quadrants
 */
    popen_struct( ROOT);
    strcpy( text_str, " Zooming ");
    text_pt.x = 0; text_pt.y = .52;
    pset_elem_ptr( 0);
    pset_elem_ptr_label( QUAD_2);
    poffset_elem_ptr( 1);
    ptext( &text_pt, text_str);

    text_pt.x = .5; text_pt.y = .52;
    pset_elem_ptr_label( QUAD_1);
    poffset_elem_ptr( 1);
    ptext( &text_pt, text_str);

    text_pt.x = 0; text_pt.y = .02;
    pset_elem_ptr_label( QUAD_3);
    poffset_elem_ptr( 1);
    ptext( &text_pt, text_str);

    text_pt.x = .5; text_pt.y = .02;
    pset_elem_ptr_label( QUAD_4);
    poffset_elem_ptr( 1);
    ptext( &text_pt, text_str);

    /* Remove the "culling" text labels from the quadrants */

    pset_elem_ptr (0);
    pset_elem_ptr_label( QUAD_1_1);
    poffset_elem_ptr( 1);
    pdel_elem();
    pset_elem_ptr_label( QUAD_4_1);
    poffset_elem_ptr( 1);
    pdel_elem();

    pclose_struct();
}

static void
init_phigs()
{
#ifdef USE_X_DRAWABLE
    extern Display *appl_display;
    extern Window appl_window;
    Pconnid_x_drawable conn;
#endif /*USE_X_DRAWABLE*/

    Pmatrix3 s;
    Pmatrix3 mat;
    Pmatrix3 r1, r2, r3, r4;

#ifdef USE_X_DRAWABLE
    conn.display = appl_display;
    conn.drawable_id = appl_window;
#endif /*USE_X_DRAWABLE*/

/* Initialization of matrices */

    setup( 4, mat);
    setup( 4, s);

    setup( 4, r1);
    setup( 4, r2);
    setup( 4, r3);
    setup( 4, r4);
/*
 * ...  set some constants for building the matrix
 */
    pt.x =  10.; pt.y = 10.; pt.z = 10.;
    shift.delta_x = 0; shift.delta_y = 0; shift.delta_z = 0;
    scale.delta_x = 1.0; scale.delta_y = 1.0; scale.delta_z = 1.0;
/*
 * ...  make identity matrices
 */
    setup(4, bldmat);
    setup(4, bldmat2);
    setup(4, bldmat3);
    setup(4, bldmat4);
/*
 *   ... open a phigs workstation 
 */
    popen_phigs( NULL, 0);
    {
	Psys_st	sys_state;

	pinq_sys_st( &sys_state );
	if (sys_state != PSYS_ST_PHOP) { exit(1); }
    }
#ifdef USE_X_DRAWABLE
    popen_ws(WS1, (Pconnid)(&conn), phigs_ws_type_x_drawable);
#else /*!USE_X_DRAWABLE*/
    popen_ws(WS1, (Pconnid)NULL, phigs_ws_type_x_tool);
#endif /*USE_X_DRAWABLE*/
    {
	Pws_st  ws_state;
 
	pinq_ws_st(&ws_state);
	if (ws_state != PWS_ST_WSOP) { exit(3); }
    }

/*
 * ... initialize the viewports
 */
    load_views(WS1);
/*
 * ... load the structure containing the cube
 */
    load_box(); 

    build_css();

}

static void
init()
{
#ifdef USE_X_DRAWABLE
    init_window();
#endif /* USE_X_DRAWABLE */
    init_phigs();
}

static void
draw_image()
{
    float scale_factor;

/*
 * ... post the structure  --- display it
 */

    ppost_struct (WS1, ROOT, 0.0);
#ifdef IGNORE
    strcpy(buf,"Program will auto rotate and scale ... then exit");
    pmessage( WS1, buf);
#endif /*IGNORE*/
/*
 * ... rotate the four views
 */
    rotate_views();

/*
 * ... turn off solids and make wireframe
 */
    edit_css();
/*
 * ... rotate the four views in wireframe
 */
    rotate_views();

    relabel_quadrants();

/*
 * ... scale down the four views
 */
    scale_factor = .5;
    scale_views( scale_factor );
/*
 * ... scale up the four views
 */
    scale_factor = 2.0;
    scale_views( scale_factor );

/*
 * test traversal
 */
    predraw_all_structs( WS1, PFLAG_ALWAYS);
}

static void
quit_program()
{
/*    clean up and close 
 *    the workstation and Phigs
 */
    sleep(5);
    pclose_ws(WS1);
    pclose_phigs();
}

main()
{
    init();
    draw_image();
    quit_program();
    exit(0);
}
