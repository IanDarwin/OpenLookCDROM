#include <stdio.h>
#include <sys/types.h>
#include <raster.h>

#define SIZE 100

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* r;
    struct raster_font* rf;
    int argn;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>]\n";
    int xoffset;
    char buf[20];

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

    for ( xoffset = 0; xoffset < 33; ++xoffset )
	{
	if ( raster_op( r, 0, 0, r->width, r->height, RAS_SET, (struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	(void) sprintf( buf, "%d", xoffset );
	if ( raster_text( r, 20, 20, RAS_SRC, rf, buf ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_text() failed\n" );
	    exit( 1 );
	    }
	
	ov( r, xoffset, -1, 0 );
	ov( r, xoffset, -1, -1 );
	ov( r, xoffset, 0, -1 );
	ov( r, xoffset, 1, -1 );
	ov( r, xoffset, 1, 0 );
	ov( r, xoffset, 1, 1 );
	ov( r, xoffset, 0, 1 );
	ov( r, xoffset, -1, 1 );
	sleep( 1 );
	}

    exit( 0 );
    }

ov( r, xoffset, deltax, deltay )
    struct raster* r;
    int xoffset, deltax, deltay;
    {
    int x, y, i;

    x = r->width / 2 + deltax * SIZE * 3 + xoffset;
    y = r->height / 2 + deltay * SIZE * 3;

    for ( i = 1; i <= SIZE; i += 3 )
	{
	if ( raster_op( r, x - i, y - i, i * 2, 1, RAS_CLEAR,
			(struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	if ( raster_op( r, x + i, y - i, 1, i * 2, RAS_CLEAR,
			(struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	if ( raster_op( r, x - i, y + i, i * 2 + 1, 1, RAS_CLEAR,
			(struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	if ( raster_op( r, x - i, y - i, 1, i * 2, RAS_CLEAR,
			(struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	}

    x = x - SIZE / 2;
    y = y - SIZE / 2;
    if ( raster_op( r, x + deltax, y + deltay, SIZE + 1, SIZE + 1, RAS_SRC,
		    r, x, y ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}
    }
