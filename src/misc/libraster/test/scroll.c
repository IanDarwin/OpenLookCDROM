#include <stdio.h>
#include <sys/types.h>
#include <raster.h>

#define SCROLLBY 22
#define SCROLLCOUNT 500

main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* r;
    int argn, delay;
    char* fb;
    int i;
    char* usage = "usage:  %s [-fb <framebuffer>] [-delay <msec>]\n";

    argn = 1;
    fb = "/dev/fb";
    delay = 0;
    if ( argn < argc && argv[argn][0] == '-' )
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

    r = raster_open( fb );
    if ( r == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "couldn't open frame buffer\n" );
	exit( 1 );
	}

    for ( i = 0; i < SCROLLCOUNT; ++i )
	{
	if ( raster_op( r, 0, 0, r->width, r->height - SCROLLBY, RAS_SRC, r, 0, SCROLLBY ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	if ( raster_op( r, 0, r->height - SCROLLBY, r->width, SCROLLBY, RAS_CLEAR, (struct raster*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_op() failed\n" );
	    exit( 1 );
	    }
	if ( delay != 0 )
#ifdef NO_POLL
	    usleep( (unsigned) delay * 1000 );
#else /*NO_POLL*/
	    poll( 0, 0, (unsigned) delay );
#endif /*NO_POLL*/
	}

    exit( 0 );
    }
