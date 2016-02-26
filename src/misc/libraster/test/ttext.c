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
    int argn, nlines, x, y, l;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>]\n";
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

    nlines = sizeof(lines) / sizeof(*lines);
    y = r->height / 2 - nlines / 2 * rf->height;

    x = r->width / 4;
    (void) raster_text( r, x, y - rf->height, RAS_SRC, rf, "regular text" );
    for ( l = 0; l < nlines; ++l )
	if ( raster_text( r, x, y + l * rf->height,
			  RAS_SRC, rf, lines[l] ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_text() failed\n" );
	    exit( 1 );
	    }

    x = r->width / 2;
    (void) raster_ttext(
	r, x, y - rf->height, RAS_SRC, rf, "transparent text" );
    for ( l = 0; l < nlines; ++l )
	if ( raster_ttext( r, x, y + l * rf->height,
			  RAS_SRC, rf, lines[l] ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_ttext() failed\n" );
	    exit( 1 );
	    }

    x = r->width * 3 / 4;
    (void) raster_ttext(
	r, x, y - rf->height, RAS_XOR, rf, "transparent XOR text" );
    for ( l = 0; l < nlines; ++l )
	if ( raster_ttext( r, x, y + l * rf->height,
			  RAS_XOR, rf, lines[l] ) != 0 )
	    {
	    (void) fprintf( stderr, "raster_ttext() failed\n" );
	    exit( 1 );
	    }

    exit( 0 );
    }
