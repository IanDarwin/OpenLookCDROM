/* squig.c - draw a squiggley tubular pattern
**
** Copyright (C) 1990, 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#define NUM_BRANCHES 8
# define BRANCH_CIRCLE_G 0
# define BRANCH_CIRCLE_S 1
# define BRANCH_TWOLOBE_T 2
# define BRANCH_TWOLOBE_B 3
# define BRANCH_FOURSTAR 4
# define BRANCH_FIVESTAR 5
# define BRANCH_FOURCIRCLE 6
# define BRANCH_BOB 7

#define NOMINAL_RADIUS 40
#define MAX_RADIUS 60

#define MAX_POINTS_PER_OBJECT 2000

#define END_TOKEN 120

#define SIZE_CYCLES 50

#define COLOR_CYCLES 40
#define MAX_D_COLOR 3

#define MAX_D_COLOR_OFFSET 0.2

#define NUM_SINCOS 3000


#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include <sys/file.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <raster.h>

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

/* Forward routines. */
static char* my_malloc();
static void terminate();
static void signit();
static void spline3();

/* Globals. */
char* argv0;
struct raster* disp;
static struct raster_colormap* oldmap;
static struct raster_colormap* map;
int color;
long timebase;
u_char red[256], grn[256], blu[256];
int maplenm2, cutoff1, cutoff2;
int cred[3], cgrn[3], cblu[3];
int dred[3], dgrn[3], dblu[3];

struct a_point
    {
    union
	{
	long offset;
	struct
	    {
	    short x, y;
	    } xy;
	} u;
    u_char color;
    };

int branch_index, object_index;
int object_counts[NUM_BRANCHES];
int* point_counts[NUM_BRANCHES];
struct a_point** points[NUM_BRANCHES];

int point_index, object_clock;
int d_object_index;
float color_offset, d_color_offset;
int i_color_offset, prev_i_color_offset;

float sins[NUM_SINCOS], coss[NUM_SINCOS];
struct a_point objectbuf[MAX_POINTS_PER_OBJECT];

static char*
my_malloc( size )
    int size;
    {
    char* p;

    p = malloc( (unsigned) size );
    if ( p == (char*) 0 )
	{
	(void) fprintf( stderr, "out of memory\n" );
	exit( 1 );
	}
    return p;
    }

static void
save_point( x, y )
    int x, y;
    {
    static int prevx, prevy;

    if ( point_index == 0 || x != prevx || y != prevy )
	{
	prevx = x;
	prevy = y;
	if ( point_index >= MAX_POINTS_PER_OBJECT )
	    {
	    (void) fprintf(
		stderr, "too many points in object %d,%d\n",
		branch_index, object_index );
	    exit( 1 );
	    }
	if ( color )
	    objectbuf[point_index].u.offset =
		x - y * disp->linelongs / sizeof(u_long);
	else
	    {
	    objectbuf[point_index].u.xy.x = x;
	    objectbuf[point_index].u.xy.y = -y;
	    }
	++point_index;
	}
    }

static void
save_object()
    {
    register int count, i;

    point_counts[branch_index][object_index] = count = point_index;
    save_point( END_TOKEN, 0 );
    if ( color )
	for ( i = 0; i < count; ++i )
	    objectbuf[i].color = maplenm2 * i / count + 2;

    points[branch_index][object_index] = (struct a_point*) my_malloc(
	( count + 1 ) * sizeof(struct a_point) );
    memcpy( (char*) points[branch_index][object_index], (char*) objectbuf,
	( count + 1 ) * sizeof(struct a_point) );
    }

static void
putcolormap()
    {
    register int i;
    static int first = 1;

    if ( first )
	{
	map->red[0] = map->grn[0] = map->blu[0] = 255;
	map->red[1] = map->grn[1] = map->blu[1] = 0;
	first = 0;
	}
    for ( i = 0; i < maplenm2; ++i )
	{
	map->red[i + 2] = red[( i + i_color_offset ) % maplenm2];
	map->grn[i + 2] = grn[( i + i_color_offset ) % maplenm2];
	map->blu[i + 2] = blu[( i + i_color_offset ) % maplenm2];
	}
    if ( raster_colormap_set( disp, map ) != 0 )
	{
	(void) fprintf( stderr, "%s: error setting colormap\n", argv0 );
	exit( 1 );
	}
    }

static void
draw_object( x, y )
    int x, y;
    {
    register struct a_point* cp;

    if ( color )
	{
	register u_char* center;
	register int offset;

	center = (u_char*) ( disp->pixels + y * disp->linelongs ) + x;
	for ( cp = points[branch_index][object_index];
	      (offset = (++cp)->u.offset) != END_TOKEN; )
	    *( center + offset ) = cp->color;
	}
    else
	{
	/* Black & white mode. */
	register u_char c = 0;

	for ( cp = points[branch_index][object_index];
	      (++cp)->u.xy.x != END_TOKEN; )
	    {
	    raster_put( disp, x + cp->u.xy.x, y + cp->u.xy.y, ( c >> 2 ) & 1 );
	    ++c;
	    }
	}
    }

static void
object_drawproc( x, y )
    int x, y;
    {
    register unsigned long r;

    ++object_clock;
    r = ( rand() / 23 );

    /* Adjust object? */
    if ( object_clock % SIZE_CYCLES == 0 )
	{
	register int i;

	/* First draw old object with new position, to avoid gaps. */
	draw_object( x, y );

	/* Advance object index, changing delta if necessary or random. */
	if ( ( r % 46301 ) % 20 == 0 )
	    d_object_index = ( rand() / 23 ) % 3 - 1;
	i = object_index + d_object_index;
	if ( i < 0 )
	    {
	    do
		{
		d_object_index = ( rand() / 23 ) % 3 - 1;
		i = object_index + d_object_index;
		}
	    while ( i < 0 );
	    }
	else if ( i >= object_counts[branch_index] )
	    {
	    if ( ( r % 46307 ) % 2 == 0 )
		{
		i = object_counts[branch_index] - 1;
		d_object_index = 0;
		}
	    else
		{
		do
		    {
		    d_object_index = ( rand() / 23 ) % 3 - 1;
		    i = object_index + d_object_index;
		    }
		while ( i >= object_counts[branch_index] );
		}
	    }
	object_index = i;

	/* Switch branches? */
	if ( object_index == 0 )
	    switch ( ( rand() / 23 ) % 16 )
		{
		case  0:
		case  1:
		case  2: branch_index = BRANCH_CIRCLE_G; break;

		case  3:
		case  4:
		case  5: branch_index = BRANCH_CIRCLE_S; break;

		case  6: branch_index = BRANCH_TWOLOBE_T; break;
		case  7: branch_index = BRANCH_TWOLOBE_B; break;

		case  8:
		case  9: branch_index = BRANCH_FOURSTAR; break;

		case 10:
		case 11: branch_index = BRANCH_FIVESTAR; break;

		case 12:
		case 13: branch_index = BRANCH_FOURCIRCLE; break;

		case 14:
		case 15: branch_index = BRANCH_BOB; break;
		}
	}

    if ( color )
	{
	int newcolormap = 0;

	/* Adjust color offset. */
	if ( r % 60 == 0 )
	    {
	    register float f;

	    for (;;)
		{
		f = d_color_offset + ( ( rand() / 23 ) % 2001 - 1000 ) / 30000.0;
		if ( f <= MAX_D_COLOR_OFFSET && f >= -MAX_D_COLOR_OFFSET )
		    break;
		}
	    d_color_offset = f;
	    }
	color_offset += d_color_offset;
	/* Got to be careful to stay positive, since % is NOT modulo. */
	if ( color_offset < 0.0 )
	    color_offset += maplenm2;
	i_color_offset = color_offset;
	if ( i_color_offset != prev_i_color_offset )
	    {
	    newcolormap = 1;
	    prev_i_color_offset = i_color_offset;
	    }

	/* Adjust colors. */
	if ( object_clock % COLOR_CYCLES == 0 )
	    {
	    register int i, j;

	    for ( i = 0; i < 3; ++i )
		{
		for (;;)
		    {
		    j = cred[i] + dred[i];
		    if ( j >= 0 && j < 256 ) break;
		    dred[i] = ( rand() / 23 ) % ( MAX_D_COLOR * 2 ) - MAX_D_COLOR;
		    if ( dred[i] <= 0 ) --dred[i];
		    }
		cred[i] = j;
		for (;;)
		    {
		    j = cgrn[i] + dgrn[i];
		    if ( j >= 0 && j < 256 ) break;
		    dgrn[i] = ( rand() / 23 ) % ( MAX_D_COLOR * 2 ) - MAX_D_COLOR;
		    if ( dgrn[i] <= 0 ) --dgrn[i];
		    }
		cgrn[i] = j;
		for (;;)
		    {
		    j = cblu[i] + dblu[i];
		    if ( j >= 0 && j < 256 ) break;
		    dblu[i] = ( rand() / 23 ) % ( MAX_D_COLOR * 2 ) - MAX_D_COLOR;
		    if ( dblu[i] <= 0 ) --dblu[i];
		    }
		cblu[i] = j;
		}

	    for ( i = 0; i < maplenm2; ++i )
		if ( i < cutoff1 )
		    {
		    red[i] = cred[0] + ( cred[1] - cred[0] ) *
			( (float) i / cutoff1 );
		    blu[i] = cblu[0] + ( cblu[1] - cblu[0] ) *
			( (float) i / cutoff1 );
		    grn[i] = cgrn[0] + ( cgrn[1] - cgrn[0] ) *
			( (float) i / cutoff1 );
		    }
		else if ( i < cutoff2 )
		    {
		    red[i] = cred[1] + ( cred[2] - cred[1] ) *
			( ( (float) i - cutoff1 ) / ( cutoff2 - cutoff1 ) );
		    blu[i] = cblu[1] + ( cblu[2] - cblu[1] ) *
			( ( (float) i - cutoff1 ) / ( cutoff2 - cutoff1 ) );
		    grn[i] = cgrn[1] + ( cgrn[2] - cgrn[1] ) *
			( ( (float) i - cutoff1 ) / ( cutoff2 - cutoff1 ) );
		    }
		else
		    {
		    red[i] = cred[2] + ( cred[0] - cred[2] ) *
			( ( i - cutoff2 ) / ( maplenm2 - cutoff2 ) );
		    blu[i] = cblu[2] + ( cblu[0] - cblu[2] ) *
			( ( i - cutoff2 ) / ( maplenm2 - cutoff2 ) );
		    grn[i] = cgrn[2] + ( cgrn[0] - cgrn[2] ) *
			( ( i - cutoff2 ) / ( maplenm2 - cutoff2 ) );
		    }

	    newcolormap = 1;
	    }

	/* Store new colors. */
	if ( newcolormap )
	    putcolormap();
	}

    /* And draw it. */
    draw_object( x, y );
    }

static void
init_sincos()
    {
    int i;
    float f, df;

    df = 2.0 * M_PI / NUM_SINCOS;
    for ( i = 0, f = 0.0; i < NUM_SINCOS; ++i, f += df )
	{
	sins[i] = sin( f );
	coss[i] = cos( f );
	}
    }

static void
measure_circle( type )
    int type;
    {
    int count, i;
    float r, dr;

    branch_index = type;
    switch ( type )
	{
	case BRANCH_CIRCLE_G:
	count = ( MAX_RADIUS - NOMINAL_RADIUS ) * 2 + 1;
	dr = 0.5;
	break;

	case BRANCH_CIRCLE_S:
	count = NOMINAL_RADIUS;
	dr = -0.5;
	break;
	}
    object_counts[branch_index] = count;
    point_counts[branch_index] = (int*) my_malloc( count * sizeof(int) );
    points[branch_index] = (struct a_point**) my_malloc(
	count * sizeof(struct a_point*) );
    for ( object_index = 0, r = NOMINAL_RADIUS;
	  object_index < count; ++object_index, r += dr )
	{
	point_index = 0;
	for ( i = 0; i < NUM_SINCOS; ++i )
	    save_point( (int) ( r * sins[i] ), (int) ( r * coss[i] ) );
	save_object();
	}
    }

static void
measure_lissajous( type )
    int type;
    {
    int count, i;
    float r, rp, u, up, x, y;

    branch_index = type;
    count = 55;
    object_counts[branch_index] = count;
    point_counts[branch_index] = (int*) my_malloc( count * sizeof(int) );
    points[branch_index] = (struct a_point**) my_malloc(
	count * sizeof(struct a_point*) );
    r = NOMINAL_RADIUS;
    for ( object_index = 0; object_index < count; ++object_index )
	{
	up = (float) object_index / count;
	u = 1.0 - up;
	rp = r * ( 1.0 + up / 2.0 );
	point_index = 0;
	for ( i = 0; i < NUM_SINCOS; ++i )
	    {
	    switch ( type )
		{
		case BRANCH_TWOLOBE_T:
		x = u * sins[i] + up * sins[( 2 * i ) % NUM_SINCOS];
		y = coss[i];
		break;

		case BRANCH_TWOLOBE_B:
		x = u * sins[i] - up * sins[( 2 * i ) % NUM_SINCOS];
		y = coss[i];
		break;
		}
	    save_point( (int) ( rp * x ), (int) ( rp * y ) );
	    }
	save_object();
	}
    }

static void
measure_polygon( bi, n, xs, ys, count )
    int bi, n, count;
    float* xs;
    float* ys;
    {
    int i, l;
    float r, u, up, v, vp, x, xp, y, yp;

    branch_index = bi;
    object_counts[branch_index] = count;
    point_counts[branch_index] = (int*) my_malloc( count * sizeof(int) );
    points[branch_index] = (struct a_point**) my_malloc(
	count * sizeof(struct a_point*) );
    r = NOMINAL_RADIUS;
    for ( object_index = 0; object_index < count; ++object_index )
	{
	up = (float) object_index / count;
	u = 1.0 - up;
	point_index = 0;
	for ( i = 0; i < NUM_SINCOS; ++i )
	    {
	    l = i * n / NUM_SINCOS;
	    vp = (float) ( i * n % NUM_SINCOS ) / NUM_SINCOS;
	    v = 1.0 - vp;
	    xp = v * xs[l] + vp * xs[(l+1) % n];
	    yp = v * ys[l] + vp * ys[(l+1) % n];
	    x = r * ( u * sins[i] + 1.5 * up * xp );
	    y = r * ( u * coss[i] + 1.5 * up * yp );
	    save_point( (int) x, (int) y );
	    }
	save_object();
	}
    }

static void
measure_fourstar()
    {
    static float xs[] = {
	0.0, 0.2, 1.0, 0.2, 0.0, -0.2, -1.0, -0.2 };
    static float ys[] = {
	1.0, 0.2, 0.0, -0.2, -1.0, -0.2, 0.0, 0.2 };

    measure_polygon( BRANCH_FOURSTAR, sizeof(xs) / sizeof(*xs), xs, ys, 40 );
    }

static void
measure_fivestar()
    {
    static float xs[] = {
	0.0000, 0.2245, 0.9511, 0.3633, 0.5878,
	0.0000, -0.5878, -0.3633, -0.9511, -0.2245 };
    static float ys[] = {
	1.0000, 0.3090, 0.3090, -0.1180, -0.8090,
	-0.3820, -0.8090, -0.1180, 0.3090, 0.3090 };

    measure_polygon( BRANCH_FIVESTAR, sizeof(xs) / sizeof(*xs), xs, ys, 40 );
    }

static void
measure_fourcircle()
    {
    int count, i, l, j;
    float r, u, up, v, x, xp, y, yp;

    branch_index = BRANCH_FOURCIRCLE;
    object_counts[branch_index] = count = 65;
    point_counts[branch_index] = (int*) my_malloc( count* sizeof(int) );
    points[branch_index] = (struct a_point**) my_malloc(
	count * sizeof(struct a_point*) );
    r = NOMINAL_RADIUS;
    for ( object_index = 0; object_index < count; ++object_index )
	{
	up = (float) object_index / count;
	u = 1.0 - up;
	point_index = 0;
	for ( i = 0; i < NUM_SINCOS; ++i )
	    {
	    l = i * 4 / NUM_SINCOS;
	    j = ( i * 4 + 5 * NUM_SINCOS / 8 ) % NUM_SINCOS;
	    switch ( l )
		{
		case 0:
		xp =  0.5 * sins[j] + 0.7071;
		yp =  0.5 * coss[j] + 0.7071;
		break;
		case 1:
		xp =  0.5 * coss[j] + 0.7071;
		yp = -0.5 * sins[j] - 0.7071;
		break;
		case 2:
		xp = -0.5 * sins[j] - 0.7071;
		yp = -0.5 * coss[j] - 0.7071;
		break;
		case 3:
		xp = -0.5 * coss[j] - 0.7071;
		yp =  0.5 * sins[j] + 0.7071;
		break;
		}
	    x = r * ( u * sins[i] + up * xp );
	    y = r * ( u * coss[i] + up * yp );
	    save_point( (int) x, (int) y );
	    }
	save_object();
	}
    }

static void
measure_bob()
    {
    static float xs[] = {
	0.0000, 0.1428, 0.2619, 0.3809, 0.5000,
	0.5952, 0.6905, 0.7143, 0.7143, 0.7143,
	0.7381, 0.7143, 0.6905, 0.6667, 0.6428,
	0.5476, 0.5238, 0.5000, 0.4524, 0.3571,
	0.2857, 0.1905, 0.0714, -0.0476, -0.1905,
	-0.2857, -0.4047, -0.5000, -0.5952, -0.7143,
	-0.7857, -0.8095, -0.7143, -0.5952, -0.4761,
	-0.3809, -0.4524, -0.5000, -0.5238, -0.5714,
	-0.6428, -0.6667, -0.6905, -0.6905, -0.6667,
	-0.6905, -0.6905, -0.6667, -0.6428, -0.5714,
	-0.476, -0.3571, -0.3095, -0.1429 };
    static float ys[] = {
	1.0000, 1.0000, 0.9762, 0.9524, 0.8571,
	0.7381, 0.6428, 0.5238, 0.3809, 0.2381,
	0.1190, 0.0000, -0.1190, -0.2381, -0.3571,
	-0.4762, -0.5952, -0.7143, -0.8333, -0.9524,
	-1.0714, -1.1666, -1.1904, -1.1666, -1.1666,
	-1.0714, -1.1190, -1.2381, -1.3333, -1.3095,
	-1.1904, -1.0714, -0.9524, -0.9762, -1.0238,
	-0.9286, -0.8095, -0.6905, -0.5714, -0.4524,
	-0.3333, -0.2143, -0.0952, 0.0476, 0.1667,
	0.2857, 0.4286, 0.5476, 0.6667, 0.7857,
	0.9047, 0.9524, 0.9762, 1.0000 };

    measure_polygon( BRANCH_BOB, sizeof(xs) / sizeof(*xs), xs, ys, 40 );
    }

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    int i;
    int thiscx, thiscy, nextcx, nextcy, prevex, prevey, nextex, nextey;
    char* usage = "usage:  %s\n";

    (void) signal( SIGHUP, terminate );
    (void) signal( SIGINT, terminate );
    (void) signal( SIGTERM, terminate );
    (void) signal( SIGFPE, terminate );

    timebase = time( 0 );

    argv0 = argv[0];
    if ( argc != 1 )
	{
	(void) fprintf( stderr, usage, argv0 );
	exit( 1 );
	}

    disp = raster_coloropen();
    if ( disp != (struct raster*) 0 )
	{
	color = 1;
	oldmap = raster_colormap_get( disp );
	if ( oldmap == (struct raster_colormap*) 0 )
	    {
	    (void) fprintf(
		stderr, "%s: error getting old colormap\n", argv0 );
	    exit( 1 );
	    }
	}
    else
	{
	disp = raster_open( "/dev/fb" );
	if ( disp == (struct raster*) 0 )
	    {
	    (void) fprintf( stderr, "%s: error opening display\n", argv0 );
	    exit( 1 );
	    }
	color = 0;
	(void) fprintf( stderr, "Your display does not have 8-bit color.\n" );
	(void) fprintf(
	    stderr, "Proceeding with worthless black & white mode.\n" );
	}

    srand( (int) ( time( 0 ) ^ getpid() ) );

    if ( color )
	{
	/* Set up colormap. */
	maplenm2 = map->len - 2;
	cutoff1 = maplenm2 / 3;
	cutoff2 = 2 * maplenm2 / 3;
	for ( i = 0; i < maplenm2; ++i )
	    red[i] = grn[i] = blu[i] = 0;
	for ( i = 0; i < 3; ++i )
	    {
	    cred[i] = cgrn[i] = cblu[i] = 0;
	    dred[i] = ( rand() / 23 ) % ( MAX_D_COLOR - 1 ) + 1;
	    dgrn[i] = ( rand() / 23 ) % ( MAX_D_COLOR - 1 ) + 1;
	    dblu[i] = ( rand() / 23 ) % ( MAX_D_COLOR - 1 ) + 1;
	    }
	color_offset = 0.0;
	d_color_offset = 0.0;
	i_color_offset = prev_i_color_offset = 0;
	putcolormap();
	}

    /* Measure the objects. */
    (void) fprintf( stderr, "Initializing..." );  (void) fflush( stderr );
    init_sincos();
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_circle( BRANCH_CIRCLE_S );
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_circle( BRANCH_CIRCLE_G );
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_lissajous( BRANCH_TWOLOBE_T );
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_lissajous( BRANCH_TWOLOBE_B );
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_fourstar();
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_fivestar();
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_fourcircle();
    (void) fprintf( stderr, "." );  (void) fflush( stderr );
    measure_bob();
    (void) fprintf( stderr, " done.\n" );
    sleep( 1 );

    /* Initialize spline points. */
    thiscx = ( rand() / 23 ) % ( disp->width - 2 * MAX_RADIUS ) + MAX_RADIUS;
    thiscy = ( rand() / 23 ) % ( disp->height - 2 * MAX_RADIUS ) + MAX_RADIUS;
    nextcx = ( rand() / 23 ) % ( disp->width - 2 * MAX_RADIUS ) + MAX_RADIUS;
    nextcy = ( rand() / 23 ) % ( disp->height - 2 * MAX_RADIUS ) + MAX_RADIUS;
    nextex = ( nextcx + thiscx ) / 2;
    nextey = ( nextcy + thiscy ) / 2;

    /* Clear to black and sign the corner. */
    raster_op(
	disp, 0, 0, disp->width, disp->height, RAS_SRC|RAS_COLOR(1),
	(struct raster*) 0, 0, 0 );
    signit( disp );

    /* Main loop. */
    branch_index = BRANCH_CIRCLE_G;
    object_index = 0;
    d_object_index = 1;
    object_clock = 0;
    for (;;)
	{
	thiscx = nextcx;
	thiscy = nextcy;
	nextcx = ( rand() / 23 ) % ( disp->width - 2 * MAX_RADIUS ) + MAX_RADIUS;
	nextcy = ( rand() / 23 ) % ( disp->height - 2 * MAX_RADIUS ) + MAX_RADIUS;
	prevex = nextex;
	prevey = nextey;
	nextex = ( nextcx + thiscx ) / 2;
	nextey = ( nextcy + thiscy ) / 2;
	spline3(
	    prevex, prevey, thiscx, thiscy, nextex, nextey,
	    object_drawproc );
	}
    }

static void
terminate()
    {
    if ( color )
	(void) raster_colormap_set( disp, oldmap );
    raster_op(
	disp, 0, 0, disp->width, disp->height, RAS_CLEAR,
	(struct raster*) 0, 0, 0 );
    exit( 0 );
    }

/* Signature stuff. */

u_long posk_pixels[] = {
    0xffffff00, 0xfe7e7f00, 0xfe7e7f00, 0xfe7cff00, 0xfcfcff00, 0xfcf9ff00,
    0xf9f3ff00, 0xf9f20100, 0xf4e7f100, 0xecf3f100, 0xfcf3f100, 0xfcf3f100,
    0xfcf33100, 0xfcf03100, 0xfcf1f100, 0xfcf3f100, 0xfcf3f300, 0xf8f33300,
    0xf8f00300, 0xf8ffe700, 0xfcfff700, 0xfdffff00, 0xffffff00, 0xffffff00,
    0xffefff00, 0xffc3ff00, 0xffe3ff00, 0xffe1ff00, 0xfff1ff00, 0xfff1ff00,
    0xfff1ff00, 0xfff1ff00, 0xfff00100, 0xf0000100, 0x83f1ff00, 0xfff1ff00,
    0xfff1ff00, 0xfff1ff00, 0xfff1ff00, 0xfff1ff00, 0xfff1ff00, 0xfff1ff00,
    0xfff11f00, 0xfe000700, 0xe00fc700, 0xe7ffff00, 0xffffff00, 0xfc7ff700,
    0xc07fc300, 0xce7c2700, 0xc03fcf00, 0xc13bcf00, 0xcf3d9f00, 0xc03c9f00,
    0xc07e3f00, 0xcefc3f00, 0xcc098700, 0xc027c100, 0xcff3f100, 0xfff3ff00,
    0xfff07f00, 0xff80ff00, 0xfff3ff00, 0xff000f00, 0xf8000700, 0xf8ffff00,
    0xffffff00, 0xffffff00, 0xfffe7f00, 0xfffe3f00, 0xfc7f3f00, 0xfe3f3f00,
    0xffff3f00, 0xffff3300, 0xf1fb3100, 0xf9f33d00, 0xfff32f00, 0xffdb0700,
    0xffde0700, 0xffbe0f00, 0xffbf8f00, 0xff3f9f00, 0xff7f1f00, 0xfe7f3f00,
    0xfe7e7f00, 0xfcfcff00, 0xfcf9ff00, 0xf8f3ff00, 0xf1e7ff00, 0xf9cfff00,
    0xff9fff00, 0xffffff00
    };
struct raster posk = { 24, 92, 1, 1, posk_pixels, RAS_STATIC, (caddr_t) 0 };

static void
signit( r )
    struct raster* r;
    {
    raster_op(
	r, r->width - posk.width, r->height - posk.height,
	posk.width, posk.height, RAS_SRC|RAS_COLOR(1), &posk, 0, 0 );
    }

/* Call-back DDAs taken from libppm. */

#define DDA_SCALE 8192
#define abs(x) ((x) < 0 ? -(x) : (x))

static void
line( x0, y0, x1, y1, drawprocP )
    int x0, y0, x1, y1;
    void (*drawprocP)();
    {
    /* Special case zero-length lines. */
    if ( x0 == x1 && y0 == y1 )
	{
	(*drawprocP)( x0, y0 );
	return;
	}

    /* Draw, using a simple DDA. */
    if ( abs( x1 - x0 ) > abs( y1 - y0 ) )
	{
	/* Loop over X domain. */
	register long dy, srow;
	register int dx, col, row, prevrow;

	if ( x1 > x0 )
	    dx = 1;
	else
	    dx = -1;
	dy = ( y1 - y0 ) * DDA_SCALE / abs( x1 - x0 );
	prevrow = row = y0;
	srow = row * DDA_SCALE + DDA_SCALE / 2;
	col = x0;
	for (;;)
	    {
	    if ( row != prevrow )
		{
		(*drawprocP)( col, prevrow );
		prevrow = row;
		}
	    (*drawprocP)( col, row );
	    if ( col == x1 )
		break;
	    srow += dy;
	    row = srow / DDA_SCALE;
	    col += dx;
	    }
	}
    else
	{
	/* Loop over Y domain. */
	register long dx, scol;
	register int dy, col, row, prevcol;

	if ( y1 > y0 )
	    dy = 1;
	else
	    dy = -1;
	dx = ( x1 - x0 ) * DDA_SCALE / abs( y1 - y0 );
	row = y0;
	prevcol = col = x0;
	scol = col * DDA_SCALE + DDA_SCALE / 2;
	for (;;)
	    {
	    if ( col != prevcol )
		{
		(*drawprocP)( prevcol, row );
		prevcol = col;
		}
	    (*drawprocP)( col, row );
	    if ( row == y1 )
		break;
	    row += dy;
	    scol += dx;
	    col = scol / DDA_SCALE;
	    }
	}
    }

#define SPLINE_THRESH 3

static void
spline3( x0, y0, x1, y1, x2, y2, drawprocP )
    int x0, y0, x1, y1, x2, y2;
    void (*drawprocP)();
    {
    register int xa, ya, xb, yb, xc, yc, xp, yp;

    xa = ( x0 + x1 ) / 2;
    ya = ( y0 + y1 ) / 2;
    xc = ( x1 + x2 ) / 2;
    yc = ( y1 + y2 ) / 2;
    xb = ( xa + xc ) / 2;
    yb = ( ya + yc ) / 2;

    xp = ( x0 + xb ) / 2;
    yp = ( y0 + yb ) / 2;
    if ( abs( xa - xp ) + abs( ya - yp ) > SPLINE_THRESH )
	spline3( x0, y0, xa, ya, xb, yb, drawprocP );
    else
	line( x0, y0, xb, yb, drawprocP );

    xp = ( x2 + xb ) / 2;
    yp = ( y2 + yb ) / 2;
    if ( abs( xc - xp ) + abs( yc - yp ) > SPLINE_THRESH )
	spline3( xb, yb, xc, yc, x2, y2, drawprocP );
    else
	line( xb, yb, x2, y2, drawprocP );
    }
