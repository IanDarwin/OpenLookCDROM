#include <stdio.h>
#include <sys/types.h>
#include <raster.h>

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* r;
    struct raster_font* rf;
    int x, y;
    int l, c;
    char s[2];
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

    x = ( r->width - 12 * rf->width * 2 ) / 2;
    y = ( r->height - 8 * rf->height * 2 ) / 2;

    if ( raster_op( r, 0, 0, r->width, r->height, RAS_SET, (struct raster*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}
    if ( raster_op( r, x, 0, 1, r->height, RAS_CLEAR, (struct raster*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}
    if ( raster_op( r, 0, y, r->width, 1, RAS_CLEAR, (struct raster*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}

    for ( s[0] = ' ', s[1] = '\0', l = c = 0; s[0] <= '~'; ++s[0], ++c )
	{
	if ( c >= 12 )
	    {
	    c = 0;
	    ++l;
	    }
	if ( raster_text( r, x + c * rf->width * 2, y + l * rf->height * 2, RAS_SRC, rf, s ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_text() failed\n" );
	    exit( 1 );
	    }
	}

    exit( 0 );
    }
