#include <stdio.h>
#include <pixrect/pixrect.h>
#include <pixrect/pixfont.h>

#define REPS 500

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    Pixrect* r;
    Pixfont* rf;
    int i, x, y, l;
    int argn, delay;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>] [-delay <msec>]\n";
    static char* lines[8] = {
	" !\"#$%&'()*+",
	",-./01234567",
	"89:;<=>?@ABC",
	"DEFGHIJKLMNO",
	"PQRSTUVWXYZ[",
	"\\]^_`abcdefg",
	"hijklmnopqrs",
	"tuvwxyz{|}~" };

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

    rf = pf_default();
    if ( rf == (Pixfont*) 0 )
	{
	(void) fprintf( stderr, "couldn't open font\n" );
	exit( 1 );
	}

    for ( i = 0, x = y = 0; i < REPS; ++i, ++x, ++y )
	{
	for ( l = 0; l < sizeof(lines) / sizeof(*lines); ++l )
	    if ( pr_text( r, x, y + l * rf->pf_defaultsize.y, PIX_SRC, rf,
			  lines[l] ) != 0 )
		{
		(void) fprintf( stderr, "pr_text() failed\n" );
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
