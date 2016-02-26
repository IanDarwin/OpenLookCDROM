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
    int wid, hgt, size, x, y, op;
    int argn, delay;
    char* fb;
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

    if ( ( disp_r = raster_open( fb ) ) == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "%s: error opening display\n", argv[0] );
	exit( 1 );
	}

    wid = disp_r->width;
    hgt = disp_r->height;

    srand( (int) ( time( 0 ) ^ getpid( ) ) );

    for (;;)
	{
	size = ( rand() / 23 ) % 100 + 100;
	x = ( rand() / 23 ) % ( wid - size );
	y = ( rand() / 23 ) % ( hgt - size );
	switch ( ( rand() / 23 ) % 3 )
	    {
	    case 0:
	    op = RAS_CLEAR;
	    break;
	    case 1:
	    op = RAS_INVERT;
	    break;
	    case 2:
	    op = RAS_SET;
	    break;
	    }
	raster_op( disp_r, x, y, size, size, op, (struct raster*) 0, 0, 0 );
	if ( delay != 0 )
#ifdef NO_POLL
	    usleep( (unsigned) delay * 1000 );
#else /*NO_POLL*/
	    poll( 0, 0, (unsigned) delay );
#endif /*NO_POLL*/
	}
    }
