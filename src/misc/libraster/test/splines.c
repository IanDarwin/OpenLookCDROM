#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <sys/types.h>
#include <raster.h>

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* disp_r;
    int wid, hgt;
    register int x0, y0, x1, y1, x2, y2;
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

    if ( ( disp_r = raster_open( fb ) ) == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "%s: error opening display\n", argv[0] );
	exit( 1 );
	}

    wid = disp_r->width;
    hgt = disp_r->height;

    if ( raster_op( disp_r, 0, 0, wid, hgt, RAS_SET, (struct raster*) 0, 0 , 0 ) != 0 )
	{
	(void) fprintf( stderr, "raster_op() failed\n" );
	exit( 1 );
	}

    srand( (int) ( time( 0 ) ^ getpid() ) );

    x1 = ( rand() / 23 ) % wid;
    y1 = ( rand() / 23 ) % hgt;
    x2 = ( rand() / 23 ) % wid;
    y2 = ( rand() / 23 ) % hgt;
    for (;;)
	{
	x0 = x1;
	y0 = y1;
	x1 = x2;
	y1 = y2;
	x2 = ( rand() / 23 ) % wid;
	y2 = ( rand() / 23 ) % hgt;
	raster_spline3( disp_r, x0, y0, x1, y1, x2, y2, RAS_CLEAR );
	}
    }
