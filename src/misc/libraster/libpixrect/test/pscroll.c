#include <stdio.h>
#include <pixrect/pixrect.h>

#define SCROLLBY 22
#define SCROLLCOUNT 500

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    Pixrect* r;
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

    r = pr_open( fb );
    if ( r == (Pixrect*) 0 )
	{
	(void) fprintf( stderr, "couldn't open frame buffer\n" );
	exit( 1 );
	}

    for ( i = 0; i < SCROLLCOUNT; ++i )
	{
	if ( pr_rop( r, 0, 0, r->pr_size.x, r->pr_size.y - SCROLLBY, PIX_SRC, r, 0, SCROLLBY ) != 0 )
	    {
	    (void) fprintf( stderr, "pr_rop() failed\n" );
	    exit( 1 );
	    }
	if ( pr_rop( r, 0, r->pr_size.y - SCROLLBY, r->pr_size.x, SCROLLBY, PIX_CLR, (Pixrect*) 0, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "pr_rop() failed\n" );
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
