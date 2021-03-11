/* $XConsortium: beach_ball.c,v 5.8 94/04/17 20:44:12 hersh Exp $ */
/***********************************************************

Copyright (c) 1989,1990, 1991  X Consortium

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

Copyright 1989,1990, 1991 by Sun Microsystems, Inc. 

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

/*
 * Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <X11/Xosdefs.h>
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern int rand();
#endif
#include <math.h>
#ifdef PEX_SI_PHIGS
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#endif /*PEX_SI_PHIGS*/
#include <phigs/phigs.h>
#include <X11/Xfuncs.h>

#ifdef USE_X_DRAWABLE
extern void init_window();
extern Display *appl_display;
extern Window appl_window;
#endif /*USE_X_DRAWABLE*/

extern int	facet_sphere();

#define	WS_ID 		1
#define MAX_SPHERES	100
#define SPHERE		1
#define LOCATIONS	2
#define ROOT		3

#define LINTSTYLE	1
#define LEDGEFLAG	2

#define BLACK	    0
#define WHITE	    1
#define RED	    2
#define GREEN	    3
#define BLUE	    4
#define YELLOW	    5

#define WC_MIN	-5.0
#define WC_MAX	 5.0

#define TIME_INC       1.0
#define VELOCITY       0.2

#ifdef SYSV
#define MAX_RAND    32767
#else
#define MAX_RAND    2147483647
#endif
#define PFILLAREASET3

#ifndef MIN
#define MIN( a, b)    (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX( a, b)    (((a) > (b)) ? (a) : (b))
#endif

/* Sphere velocity and location data. */
typedef struct {
    Pvec3	  velocity;
    Pmatrix3	  position;
} Sphere_data;

static Pmatrix3      identity = { 1.0, 0.0, 0.0, 0.0,
				  0.0, 1.0, 0.0, 0.0,
				  0.0, 0.0, 1.0, 0.0,
				  0.0, 0.0, 0.0, 1.0 };

static Sphere_data    sphere_data[MAX_SPHERES];
static int	sphere_count = 0;
static int	num_spheres = 1;

/* Radius of spheres. */
static double	 radius = 1.0;

/* Viewing parameters */
static Pview_rep3   rep;
static Pview_map3   map;
static Ppoint3	    vrp = { 0.0, 0.0, 0.0};
static Pvec3	    vup = { 0.0, 0.0, 1.0};
static Pvec3	    vpn = { .680, -1.0, .260};

static void
random_velocity(v)
Pvec3 *v;
{

    int	    Xr, Yr;
    Pfloat  Xv, Yv, Zv, XYv, V2;

    Xv = Yv = Zv = 0.0;
    V2 = VELOCITY * VELOCITY;

    Xr = rand();
    Xv = ((float)Xr/(float)MAX_RAND) * (VELOCITY/sqrt(3.0));
    if ( Xr % 2 )
			Xv = - Xv;

    Yr = rand();
    Yv = ((float)Yr/(float)MAX_RAND) * sqrt(V2 - Xv * Xv);
    if ( Yr % 2 )
			Yv = - Yv;

    XYv = sqrt(Xv * Xv + Yv * Yv);
    if ( XYv < VELOCITY ) {
			Zv = sqrt(V2 - XYv * XYv);
			if ( rand() % 2 )
				Zv = - Zv;
    }

    v->delta_x = Xv;
    v->delta_y = Yv;
    v->delta_z = Zv;
}

static void
add_sphere()
{
    register Sphere_data *data;

    if (sphere_count < MAX_SPHERES) {
    data = &sphere_data[sphere_count++];
    bcopy(identity, data->position, sizeof(Pmatrix3));
    random_velocity(&data->velocity );

    pset_edit_mode(PEDIT_INSERT);
    popen_struct(LOCATIONS);
	pset_local_tran3(data->position, PTYPE_REPLACE);
	pexec_struct(SPHERE);
    pclose_struct();
    pset_edit_mode(PEDIT_REPLACE);
    }
}

static void
init_view_mapping()
{
    map.proj_type = PTYPE_PERSPECT;
    map.proj_ref_point.z = 6.0 * WC_MAX;
    map.win.x_min = WC_MIN; map.win.x_max = WC_MAX;
    map.win.y_min = WC_MIN; map.win.y_max = WC_MAX;
    map.front_plane = 2.0 * WC_MAX;
    map.back_plane =  2.0 * WC_MIN;
    map.view_plane =  0.4 * map.proj_ref_point.z;
    map.proj_ref_point.x = (map.win.x_min + map.win.x_max) / 2.0;
    map.proj_ref_point.y = (map.win.y_min + map.win.y_max) / 2.0;
    map.proj_vp.x_min = 0.0; map.proj_vp.x_max = 1.0;
    map.proj_vp.y_min = 0.0; map.proj_vp.y_max = 1.0;
    map.proj_vp.z_min = 0.0; map.proj_vp.z_max = 1.0;
}

static void
eval_view_rep(rep)
Pview_rep3  *rep;
{
    Pint err;

    peval_view_map_matrix3( &map, &err, rep->map_matrix);

    if (err) (void) fprintf(stderr, "Error from eval mapping %d\n", err);

    peval_view_ori_matrix3( &vrp, &vpn, &vup, &err, rep->ori_matrix);

    if (err) (void) fprintf(stderr, "Error from eval orientation %d\n", err);
    
}

static void
set_view_rep()
{
    eval_view_rep(&rep);
    rep.clip_limit = map.proj_vp;
    rep.xy_clip = rep.back_clip = rep.front_clip = PIND_CLIP;
    pset_view_rep3(1, 1, &rep);
}

static void
build_sphere()
{
    Ppoint_list_list3 facets_list;
    Ppoint_list3 *facets;
    int	    num_facets, num_lat = 6, num_long = 8;
    register int   i;

    num_facets = facet_sphere(radius, num_lat, num_long, &facets);
    (void) printf("numfacets = %d \n", num_facets);
    popen_struct( SPHERE);
    for (i = 0; i < num_facets; i++) {
	pset_int_colr_ind( (i % 6) + 2 );
	facets_list.num_point_lists = 1;
	facets_list.point_lists = &facets[i];
#ifdef PFILLAREASET3
	pfill_area_set3( &facets_list);
#else /* PFILLAREASET3 */
	pfill_area3(&facets[i]);
#endif /* PFILLAREASET3 */
    }
    pclose_struct();
    free(facets);
}

static void
build_css(edge_flag, int_style)
Pedge_flag  edge_flag;
Pint_style  int_style;
{
    Ppoint3     axes_origin, axes_length;
    Pint	axes_color[3];
    Pint	err;
    Pmatrix3    mat, mat2, newmat;

    axes_origin.x = axes_origin.y = axes_origin.z = 0;
    axes_length.x = axes_length.y = axes_length.z = 1.5;
    axes_color[0] = RED; axes_color[1] = GREEN; axes_color[2] = BLUE;

    popen_struct(ROOT);
    pset_view_ind(1);
    pset_hlhsr_id(PHIGS_HLHSR_ID_ON);
    pset_int_colr_ind(YELLOW);
    pset_edge_colr_ind(GREEN);
    plabel(LEDGEFLAG);
    pset_edge_flag(edge_flag);
    plabel(LINTSTYLE);
    pset_int_style(int_style);
    pexec_struct(LOCATIONS);

    protate_y(3.14/2, &err, mat2);
    protate_x(3.14/2, &err, mat);
    pcompose_matrix3(mat, mat2, &err, newmat);
    pset_local_tran3(newmat,PTYPE_REPLACE);
    pclose_struct();
    build_sphere();
}

#define NEW_RI( _v, _dt, _r ) \
    { \
    (_r) += (_v) * (_dt); \
    if ( (_r) >= WC_MAX ) { \
	 (_r) = 2.0 * WC_MAX - (_r); \
	 (_v) = -(_v); \
    } else if ( (_r) <= WC_MIN ) { \
	 (_r) = 2.0 * WC_MIN - (_r); \
	 (_v) = -(_v); \
    } \
    }

static void
move_spheres()
{
    register Sphere_data  *data;
    register int	  i;    

    popen_struct(LOCATIONS);
    pset_elem_ptr(1);
    for (i = 0; i < sphere_count; i++) {
	data = &sphere_data[i];
	NEW_RI(data->velocity.delta_x, TIME_INC, data->position[0][3])
	NEW_RI(data->velocity.delta_y, TIME_INC, data->position[1][3])
	NEW_RI(data->velocity.delta_z, TIME_INC, data->position[2][3])
	pset_local_tran3(data->position, PTYPE_REPLACE);
	poffset_elem_ptr(2);
    }
    pclose_struct();
}

static void
init_phigs()
{
	register long i;

#ifdef USE_X_DRAWABLE
	Pconnid_x_drawable conn;
#endif /*USE_X_DRAWABLE*/

    Pedge_flag  edge_flag = PEDGE_OFF;
    Pint_style  int_style = PSTYLE_SOLID; 
    Pint	hidden_surf = PHIGS_HLHSR_MODE_NONE;

    popen_phigs( (char *)NULL, PDEF_MEM_SIZE);
    {
	Psys_st sys_state;

	pinq_sys_st( &sys_state );
	if (sys_state != PSYS_ST_PHOP) { exit(1); }
    }


#ifdef USE_X_DRAWABLE
    conn.display = appl_display;
    conn.drawable_id = appl_window;

    popen_ws(WS_ID, (Pconnid)(&conn),phigs_ws_type_x_drawable);
#else /* !USE_X_DRAWABLE */
    popen_ws(WS_ID, (Pconnid)NULL, phigs_ws_type_x_tool);
#endif /*USE_X_DRAWABLE*/

    init_view_mapping();
    build_css( edge_flag, int_style);
    pset_disp_upd_st(1, PDEFER_WAIT, PMODE_NIVE);
    pset_hlhsr_mode(WS_ID, hidden_surf);
    set_view_rep();

    for (i = 0; i < num_spheres; i++) add_sphere();

    ppost_struct(WS_ID, ROOT, 1.0);
    pset_disp_upd_st(1, PDEFER_ASAP, PMODE_NIVE);
    pset_edit_mode(PEDIT_REPLACE);
}

static void
init()
{
#ifdef USE_X_DRAWABLE	
	init_window();
#endif /*USE_X_DRAWABLE*/
	init_phigs();
}
static void
draw_image()
{
    clock_reset();
    for (;;) {
	pset_disp_upd_st(WS_ID, PDEFER_WAIT, PMODE_NIVE);
	move_spheres();
	pset_disp_upd_st(WS_ID, PDEFER_ASAP, PMODE_NIVE);
    }
}

static void
handle_args(argc, argv)
int argc;
char *argv[];
{
    if (argc > 1) num_spheres = atoi(argv[1]);

    if (!num_spheres) {
	(void) printf("%s: can't display 0 beach_balls\n",
	argv[0], num_spheres);
	exit(1);
    }
}

main(argc,argv)
int argc;
char *argv[];
{
    handle_args(argc, argv);
    init();
    draw_image();
}

