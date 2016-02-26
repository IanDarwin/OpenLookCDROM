#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <sys/types.h>
#include <raster.h>
#include <sys/file.h>

void raster_rotatepow2square();

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    int argn, delay;
    struct raster* disp_r;
    int wid, hgt, maxpow2;
    int dir, x, y, pow2;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>] [-delay <msec>]\n";

    argn = 1;
    fb = "/dev/fb";
    delay = 0;
    while ( argn < argc && argv[argn][0] == '-' )
	{
	if ( strcmp( argv[argn], "-fb" ) == 0 ||
	     strcmp( argv[argn], "-f" ) == 0 )
	    {
	    ++argn;
	    fb = argv[argn];
	    }
	else if ( strcmp( argv[argn], "-delay" ) == 0 ||
		  strcmp( argv[argn], "-dela" ) == 0 ||
		  strcmp( argv[argn], "-del" ) == 0 ||
		  strcmp( argv[argn], "-de" ) == 0 ||
		  strcmp( argv[argn], "-d" ) == 0 )
	    {
	    ++argn;
	    delay = atoi( argv[argn] );
	    }
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

    if ( ( disp_r = raster_open( fb ) ) == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "%s: error opening display\n", argv[0] );
	exit( 1 );
	}

    wid = disp_r->width;
    hgt = disp_r->height;
    for ( maxpow2 = 1;
	 ( 2 << maxpow2 ) <= wid && ( 2 << maxpow2 ) <= hgt;
	 ++maxpow2 )
	;

    srand( (int) ( time( 0 ) ^ getpid( ) ) );

    for (;;)
	{
	pow2 = ( rand() / 23 ) % ( maxpow2 + 1 );
	x = ( rand() / 23 ) % ( wid - ( 1 << pow2 ) );
	y = ( rand() / 23 ) % ( hgt - ( 1 << pow2 ) );
	dir = ( rand() / 23 ) % 2;
	raster_rotatepow2square( disp_r, x, y, pow2, dir );
	if ( delay != 0 )
#ifdef NO_POLL
	    usleep( (unsigned) delay * 1000 );
#else /*NO_POLL*/
	    poll( 0, 0, (unsigned) delay );
#endif /*NO_POLL*/
	}
    }

static struct raster* temp1 = (struct raster*) 0;
static struct raster* temp2 = (struct raster*) 0;
static struct raster* mask = (struct raster*) 0;

void
raster_rotatepow2square( r, x, y, pow2, cw )
    struct raster* r;
    int x, y, pow2, cw;
    {
    int s, h, ts, th, tq;

    if ( pow2 < 2 )
	return;
    s = 1 << pow2;
    h = s / 2;

    /* Make sure that temps and mask are big enough and the right depth. */
    if ( temp1 != (struct raster*) 0 )
	if ( temp1->width < s || temp1->height < s ||
	     temp1->depth != r->depth )
	    {
	    raster_free( temp1 );
	    temp1 = (struct raster*) 0;
	    raster_free( temp2 );
	    temp2 = (struct raster*) 0;
	    raster_free( mask );
	    mask = (struct raster*) 0;
	    }
    if ( temp1 == (struct raster*) 0 )
	{
	temp1 = raster_alloc( s, s, r->depth );
	temp2 = raster_alloc( s, s, r->depth );
	mask = raster_alloc( s, s, 1 );
	if ( temp1 == (struct raster*) 0 || temp2 == (struct raster*) 0 ||
	     mask == (struct raster*) 0 )
	    {
	    (void) fprintf( stderr, "error allocating temp rasters\n" );
	    exit( 1 );
	    }

	}

    /* Initialize mask to the upper left quadrant. */
    raster_op( mask, 0, 0, s, s, RAS_CLEAR, (struct raster*) 0, 0, 0 );
    raster_op( mask, 0, 0, h, h, RAS_SET, (struct raster*) 0, 0, 0 );

    for ( ts = s, th = ts / 2, tq = th / 2;
	  pow2 > 0;
	  --pow2, ts = th, th = tq, tq /= 2 )
	{
	if ( cw )
	    raster_op( temp1,  0,  0, s-th, s-th, RAS_SRC,    r,    x, y+th );
	else
	    raster_op( temp1,  0,  0, s-th, s-th, RAS_SRC,    r, x+th,    y );
	raster_op( temp1,  0,  0,    s,    s, RAS_AND,  mask,    0,    0 );

	if ( cw )
	    raster_op( temp2, th,  0, s-th, s-th, RAS_SRC,    r,    x,    y );
	else
	    raster_op( temp2, th,  0, s-th, s-th, RAS_SRC,    r, x+th, y+th );
	raster_op( temp2, th,  0, s-th, s-th, RAS_AND,  mask,    0,    0 );
	raster_op( temp1, th,  0, s-th, s-th,  RAS_OR, temp2,   th,    0 );

	if ( cw )
	    raster_op( temp2, th, th, s-th, s-th, RAS_SRC,    r, x+th,    y );
	else
	    raster_op( temp2, th, th, s-th, s-th, RAS_SRC,    r,    x, y+th );
	raster_op( temp2, th, th, s-th, s-th, RAS_AND,  mask,    0,    0 );
	raster_op( temp1, th, th, s-th, s-th,  RAS_OR, temp2,   th,   th );

	if ( cw )
	    raster_op( temp2,  0, th, s-th, s-th, RAS_SRC,    r, x+th, y+th );
	else
	    raster_op( temp2,  0, th, s-th, s-th, RAS_SRC,    r,    x,    y );
	raster_op( temp2,  0, th, s-th, s-th, RAS_AND,  mask,    0,    0 );
	raster_op( temp1,  0, th, s-th, s-th,  RAS_OR, temp2,    0,   th );

	/* And copy back to the screen. */
	raster_op(    r,  x,  y,    s,    s, RAS_SRC, temp1,    0,    0 );

	/* Refine mask. */
	raster_op( mask,  0,  0, s-tq, s-tq, RAS_AND, mask, tq, tq );
	raster_op( mask,  0, th,    s, s-th,  RAS_OR, mask,  0,  0 );
	raster_op( mask, th,  0, s-th,    s,  RAS_OR, mask,  0,  0 );
	}
    }
