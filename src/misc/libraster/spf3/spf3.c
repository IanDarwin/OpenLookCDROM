/* spf3.c - spline fun #3
**
** Copyright (C) 1989, 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#define POINTS 5
#define MAXDELTA 3
#define MAXCOLORDELTA 3

#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <sys/types.h>
#include <raster.h>

static struct raster* disp;
static struct raster_fb* dispfb;
static struct raster_colormap* oldmap;
static struct raster_colormap* map;

static void signit();
static void terminate();

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    int argn, forward, backward;
    int i, t, color;
    int x[POINTS], y[POINTS], dx[POINTS], dy[POINTS];
    int px, py, nx, ny, zx, zy;
    int dred, dgrn, dblu;
    unsigned char nred, ngrn, nblu, tred, tgrn, tblu;
    char* usage = "usage:  %s [-f|-b]\n";

    argn = 1;
    forward = 0;
    backward = 0;

    if ( argn < argc )
	{
	if ( strcmp( argv[argn], "-f" ) == 0 )
	    forward = 1;
	else if ( strcmp( argv[argn], "-b" ) == 0 )
	    backward = 1;
	else
	    {
	    (void) fprintf( stderr, usage, argv[0] );
	    exit( 1 );
	    }
	++argn;
	}

    if ( argn != argc )
	{
	(void) fprintf( stderr, usage, argv[0] );
	exit( 1 );
	}

    disp = raster_coloropen();
    if ( disp == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "%s: can't open color display\n", argv[0] );
	exit( 1 );
	}
    dispfb = (struct raster_fb*) disp->data;

    srand( (int) ( time( 0 ) ^ getpid() ) );
    signal( SIGHUP, terminate );
    signal( SIGINT, terminate );
    signal( SIGTERM, terminate );

    /* Save old state. */
    oldmap = raster_colormap_get( disp );
    if ( oldmap == (struct raster_colormap*) 0 )
	{
	(void) fprintf( stderr, "%s: error getting old colormap\n", argv[0] );
	exit( 1 );
	}

    nred = ngrn = nblu = 0;
    map = raster_colormap_alloc( dispfb->cmsize );
    if ( map == (struct raster_colormap*) 0 )
	{
	(void) fprintf( stderr, "%s: error allocating colormap\n", argv[0] );
	exit( 1 );
	}
    map->red[0] = map->grn[0] = map->blu[0] = 255;
    for ( color = 1; color < map->len; ++color )
	map->red[color] = map->grn[color] = map->blu[color] = 0;
    if ( raster_colormap_set( disp, map ) != 0 )
	{
	(void) fprintf( stderr, "%s: error setting colormap\n", argv[0] );
	exit( 1 );
	}
    raster_op(
        disp, 0, 0, disp->width, disp->height, RAS_SRC|RAS_COLOR(1), 0, 0, 0 );
    signit( disp );

    dred = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
    if ( dred <= 0 ) --dred;
    dgrn = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
    if ( dgrn <= 0 ) --dgrn;
    dblu = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
    if ( dblu <= 0 ) --dblu;
    for ( i = 0; i < POINTS; ++i )
	{
	x[i] = ( rand() / 23 ) % disp->width;
	y[i] = ( rand() / 23 ) % disp->height;
	dx[i] = ( rand() / 23 ) % ( MAXDELTA * 2 ) - MAXDELTA;
	if ( dx[i] <= 0 ) --dx[i];
	dy[i] = ( rand() / 23 ) % ( MAXDELTA * 2 ) - MAXDELTA;
	if ( dy[i] <= 0 ) --dy[i];
	}

    /* Main loop. */
    color = 1;
    for (;;)
	{
	/* Choose new color. */
	if ( ++color == map->len ) color = 2;
	for (;;)
	    {
	    t = (int) nred + dred;
	    if ( t >= 0 && t < map->len ) break;
	    dred = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
	    if ( dred <= 0 ) --dred;
	    }
	map->red[color] = nred = t;
	for (;;)
	    {
	    t = (int) ngrn + dgrn;
	    if ( t >= 0 && t < map->len ) break;
	    dgrn = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
	    if ( dgrn <= 0 ) --dgrn;
	    }
	map->grn[color] = ngrn = t;
	for (;;)
	    {
	    t = (int) nblu + dblu;
	    if ( t >= 0 && t < map->len ) break;
	    dblu = ( rand() / 23 ) % ( MAXCOLORDELTA * 2 ) - MAXCOLORDELTA;
	    if ( dblu <= 0 ) --dblu;
	    }
	map->blu[color] = nblu = t;
	if ( raster_colormap_set( disp, map ) != 0 )
	    {
	    (void) fprintf( stderr, "%s: error setting colormap\n", argv[0] );
	    exit( 1 );
	    }

	/* Move the points. */
	for ( i = 0; i < POINTS; ++i )
	    {
	    for (;;)
		{
		t = x[i] + dx[i];
		if ( t >= 0 && t < disp->width ) break;
		dx[i] = ( rand() / 23 ) % ( MAXDELTA * 2 ) - MAXDELTA;
		if ( dx[i] <= 0 ) --dx[i];
		}
	    x[i] = t;
	    for (;;)
		{
		t = y[i] + dy[i];
		if ( t >= 0 && t < disp->height ) break;
		dy[i] = ( rand() / 23 ) % ( MAXDELTA * 2 ) - MAXDELTA;
		if ( dy[i] <= 0 ) --dy[i];
		}
	    y[i] = t;
	    }

	/* Draw the figure. */
	px = zx = ( x[0] + x[POINTS-1] ) / 2;
	py = zy = ( y[0] + y[POINTS-1] ) / 2;
	for ( i = 0; i < POINTS-1; ++i )
	    {
	    nx = ( x[i+1] + x[i] ) / 2;
	    ny = ( y[i+1] + y[i] ) / 2;
	    raster_spline3(
		disp, px, py, x[i], y[i], nx, ny, RAS_SRC|RAS_COLOR(color) );
	    px = nx;
	    py = ny;
	    }
	raster_spline3(
	    disp, px, py, x[POINTS-1], y[POINTS-1], zx, zy,
	    RAS_SRC|RAS_COLOR(color) );

	/* Cycle colormap. */
	if ( forward )
	    {
	    if ( color % 2 == 0 )
		{
		/* Cycle colors forward. */
		tred = map->red[map->len - 1];
		tgrn = map->grn[map->len - 1];
		tblu = map->blu[map->len - 1];
		for ( i = map->len - 2; i >= 2; --i )
		    {
		    map->red[i+1] = map->red[i];
		    map->grn[i+1] = map->grn[i];
		    map->blu[i+1] = map->blu[i];
		    }
		map->red[2] = tred;
		map->grn[2] = tgrn;
		map->blu[2] = tblu;
		}
	    }
	else if ( backward )
	    {
	    /* Cycle colors backward. */
	    tred = map->red[2];
	    tgrn = map->grn[2];
	    tblu = map->blu[2];
	    for ( i = 3; i < map->len; ++i )
		{
		map->red[i-1] = map->red[i];
		map->grn[i-1] = map->grn[i];
		map->blu[i-1] = map->blu[i];
		}
	    map->red[map->len - 1] = tred;
	    map->grn[map->len - 1] = tgrn;
	    map->blu[map->len - 1] = tblu;
	    map->red[color] = nred;
	    map->grn[color] = ngrn;
	    map->blu[color] = nblu;
	    }
	}
    }

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

static void
terminate()
    {
    (void) raster_colormap_set( disp, oldmap );
    raster_op( disp, 0, 0, disp->width, disp->height, RAS_CLEAR, 0, 0, 0 );
    exit( 0 );
    }
