#include <stdio.h>
#include <sys/types.h>
#include <raster.h>

#define SIZE 10
#define REPS 70

main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* r;
    struct raster_font* rf;
    int argn;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>]\n";

    argn = 1;
    fb = "/dev/fb";
    if ( argn < argc && argv[argn][0] == '-' )
	{
	if ( strcmp( argv[argn], "-fb" ) == 0 ||
	     strcmp( argv[argn], "-f" ) == 0 )
	    {
	    ++argn;
	    fb = argv[argn];
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

    r = raster_open( fb );
    if ( r == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "couldn't open frame buffer\n" );
	exit( 1 );
	}

    rf = raster_defaultfont();
    if ( rf == (struct raster_font*) 0 )
	{
	(void) fprintf( stderr, "couldn't open font\n" );
	exit( 1 );
	}

    if ( raster_op( r, 0, 0, r->width, r->height, RAS_SET, (struct raster*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}

    if ( raster_text( r, 0, 20, RAS_SRC, rf, "CLEAR" ) != 0 )
	{
	(void) fprintf( stderr, "raster_text() failed\n" );
	exit( 1 );
	}
    sq( r, (struct raster*) 0, 0, 50, RAS_CLEAR, 0 );

    if ( raster_text( r, 128, 20, RAS_SRC, rf, "INVERT" ) != 0 )
	{
	(void) fprintf( stderr, "raster_text() failed\n" );
	exit( 1 );
	}
    sq( r, (struct raster*) 0, 128, 50, RAS_INVERT, 0 );

    if ( raster_text( r, 256, 20, RAS_SRC, rf, "~SRC" ) != 0 )
	{
	(void) fprintf( stderr, "raster_text() failed\n" );
	exit( 1 );
	}
    sq( r, r, 256, 50, RAS_INVERTSRC, 0 );

    if ( raster_text( r, 384, 20, RAS_SRC, rf, "~SRC-L" ) != 0 )
	{
	(void) fprintf( stderr, "raster_text() failed\n" );
	exit( 1 );
	}
    sq( r, r, 384, 50, RAS_INVERTSRC, -1 );

    if ( raster_text( r, 512, 20, RAS_SRC, rf, "~SRC-R" ) != 0 )
	{
	(void) fprintf( stderr, "raster_text() failed\n" );
	exit( 1 );
	}
    sq( r, r, 512, 50, RAS_INVERTSRC, 1 );

    exit( 0 );
    }

sq( d, s, x, y, rop, dx )
    struct raster* d;
    struct raster* s;
    int x, y, rop, dx;
    {
    int i;

    for ( i = 0; i < REPS; ++i, ++x, y += SIZE + 1 )
	{
	if ( raster_op( d, x + dx, y, SIZE, SIZE, rop, s, x, y ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }

	}
    }
