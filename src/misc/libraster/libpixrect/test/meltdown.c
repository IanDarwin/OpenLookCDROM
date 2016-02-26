/* pmelt - melt a sun screen by displacing random squares */

#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <pixrect/pixrect.h>

#define SIZE 128

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    struct pixrect* disp_pr;
    int wid, hgt, maxx, maxy;
    register int x, y, r, nx, ny;
    int argn, delay;
    char* fb;
    static int xoffsets[8] = { 0, 1, 1, 1, 0, -1, -1, -1 };
    static int yoffsets[8] = { 1, 1, 0, -1, -1, -1, 0, 1 };
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

    if ( ( disp_pr = pr_open( fb ) ) == (struct pixrect*) 0 )
	{
	(void) fprintf( stderr, "%s: error opening display\n", argv[0] );
	exit( 1 );
	}

    wid = disp_pr->pr_size.x;
    hgt = disp_pr->pr_size.y;
    maxx = wid - SIZE;
    maxy = hgt - SIZE;

    srand( (int) ( time( 0 ) ^ getpid( ) ) );

    for (;;)
	{
	x = ( rand() / 23 ) % maxx;
	y = ( rand() / 23 ) % maxy;
	r = ( rand() / 23 ) % 8;
	nx = x + xoffsets[r];
	ny = y + yoffsets[r];
	pr_rop( disp_pr, nx, ny, SIZE, SIZE, PIX_SRC, disp_pr, x, y );
	if ( delay != 0 )
#ifdef NO_POLL
	    usleep( (unsigned) delay * 1000 );
#else /*NO_POLL*/
	    poll( 0, 0, (unsigned) delay );
#endif /*NO_POLL*/
	}
    }
