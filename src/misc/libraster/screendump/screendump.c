/* screendump - screen dumping program
**
** Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <raster.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/time.h>

#define max(a,b) ( ((int)(a)) > ((int)(b)) ? (a) : (b) )

/* Externals . */
extern long strtol();

/* Forward routines. */
void usage();

/* Globals. */
static char* argv0;

void
main( argc, argv )
int argc;
char* argv[];
    {
    int argn, rt, x, y, w, h;
    char* fb_name;
    struct raster* fb;
    struct raster* r;
    struct raster_colormap* cm;

    argv0 = *argv;

    /* Parse flags. */
    argn = 1;
    rt = RT_BYTE_ENCODED;
    x = y = 0;
    w = h = -1;
    fb_name = (char*) 0;
    while ( argn < argc && argv[argn][0] == '-' && argv[argn][0] != '\0' )
	{
	if ( strncmp(argv[argn],"-standard",max(strlen(argv[argn]),2)) == 0 )
	    {
	    rt = RT_STANDARD;
	    }
	else if ( strncmp(argv[argn],"-rle",max(strlen(argv[argn]),2)) == 0 )
	    {
	    rt = RT_BYTE_ENCODED;
	    }
	else if ( strcmp( argv[argn], "-x" ) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    x = atol( argv[argn] );
	    if ( x < 0 )
		{
		(void) fprintf(
		    stderr, "%s: x-coord must be non-negative\n", argv0 );
		exit( 1 );
		}
	    }
	else if ( strcmp( argv[argn], "-y" ) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    y = atol( argv[argn] );
	    if ( y < 0 )
		{
		(void) fprintf(
		    stderr, "%s: y-coord must be non-negative\n", argv0 );
		exit( 1 );
		}
	    }
	else if ( strcmp( argv[argn], "-X" ) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    w = atol( argv[argn] );
	    }
	else if ( strcmp( argv[argn], "-Y" ) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    h = atol( argv[argn] );
	    }
	else if ( strncmp(argv[argn],"-fb",max(strlen(argv[argn]),2)) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    fb_name = argv[argn];
	    }
	else
	    usage();
	++argn;
	}
    
    if ( argn != argc )
	usage();

    /* Open framebuffer. */
    fb = raster_open( fb_name == (char*) 0 ? "/dev/fb" : fb_name );
    if ( fb == (struct raster*) 0 )
	{
	(void) fprintf(
	    stderr, "%s: can't open framebuffer %s\n", argv0,
	    fb_name == (char*) 0 ? "/dev/fb" : fb_name );
	exit( 1 );
	}
    
    /* Check width and height. */
    if ( w == -1 )
	w = fb->width - x;
    else
	{
	if ( x + w > fb->width )
	    {
	    (void) fprintf( stderr, "%s: width is too large\n", argv0 );
	    exit( 1 );
	    }
	}
    if ( h == -1 )
	h = fb->height - y;
    else
	{
	if ( y + h > fb->height )
	    {
	    (void) fprintf( stderr, "%s: height is too large\n", argv0 );
	    exit( 1 );
	    }
	}

    /* Make a sub-raster, if necessary. */
    if ( x == 0 && y == 0 && w == fb->width && h == fb->height )
	r = fb;
    else
	{
	r = raster_subregion( fb, x, y, w, h );
	if ( r == (struct raster*) 0 )
	    {
	    (void) fprintf( stderr, "%s: raster_subregion() failed\n", argv0 );
	    exit( 1 );
	    }
	}

    /* Get the colormap, if any. */
    if ( fb->depth == 1 )
	cm = (struct raster_colormap*) 0;
    else
	{
	cm = raster_colormap_get( fb );
	if ( cm == (struct raster_colormap*) 0 )
	    {
	    (void) fprintf(
		stderr, "%s: raster_colormap_get() failed\n", argv0 );
	    exit( 1 );
	    }
	}

    /* Write it. */
    if ( raster_write( stdout, r, rt, cm ) != 0 )
	{
	(void) fprintf( stderr, "%s: raster_write() failed\n", argv0 );
	exit( 1 );
	}

    /* All done. */
    if ( r != fb )
	raster_free( r );
    raster_free( fb );
    exit( 0 );
    }

void
usage()
    {
    char* use =
"usage:  %s [-standard|-rle] [-x coord][-y coord][-X width][-Y height]\n\
        [-fb <framebuffer>]\n";

    (void) fprintf( stderr, use, argv0 );
    exit( 1 );
    }
