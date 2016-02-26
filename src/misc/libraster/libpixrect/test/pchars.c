#include <stdio.h>
#include <pixrect/pixrect.h>
#include <pixrect/pixfont.h>

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    Pixrect* r;
    Pixfont* rf;
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

    x = ( r->pr_size.x - 12 * rf->pf_defaultsize.x * 2 ) / 2;
    y = ( r->pr_size.y - 8 * rf->pf_defaultsize.y * 2 ) / 2;

    if ( pr_rop( r, 0, 0, r->pr_size.x, r->pr_size.y, PIX_SET, (Pixrect*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "pr_rop() failed\n" );
	exit( 1 );
	}
    if ( pr_rop( r, x, 0, 1, r->pr_size.y, PIX_CLR, (Pixrect*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "pr_rop() failed\n" );
	exit( 1 );
	}
    if ( pr_rop( r, 0, y, r->pr_size.x, 1, PIX_CLR, (Pixrect*) 0, 0, 0 ) != 0 )
	{
	(void) fprintf( stderr, "pr_rop() failed\n" );
	exit( 1 );
	}

    for ( s[0] = ' ', s[1] = '\0', l = c = 0; s[0] <= '~'; ++s[0], ++c )
	{
	if ( c >= 12 )
	    {
	    c = 0;
	    ++l;
	    }
	if ( pr_text( r, x + c * rf->pf_defaultsize.x * 2, y + l * rf->pf_defaultsize.y * 2, PIX_SRC, rf, s ) != 0 )
	    {
	    (void) fprintf( stderr, "pr_text() failed\n" );
	    exit( 1 );
	    }
	}

    exit( 0 );
    }
