/* dumprast - dump out a rasterfile as C structs
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

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    struct raster* r;
    char* usage = "usage:  %s [rasterfile]\n";
    char basename[200];
    char* cp;
    FILE* f;
    struct raster_colormap* cm;

    if ( argc == 1 )
	{
	(void) strcpy( basename, "noname" );
	f = stdin;
	}
    else if ( argc == 2 )
	{
	if ( strchr( argv[1], '/' ) != (char*) 0 )
	    {
	    cp = strchr( argv[1], '/' );
	    if ( cp != (char*) 0 )
		(void) strcpy( basename, cp + 1 );
	    else
		(void) strcpy( basename, argv[1] );
	    }
	else
	    {
	    (void) strcpy( basename, argv[1] );
	    }
	cp = strchr( basename, '.' );
	if ( cp != (char*) 0 )
	    *cp = '\0';

	f = fopen( argv[1], "r" );
	if ( f == (FILE*) 0 )
	    {
	    (void) fprintf( stderr, "couldn't open rasterfile %s\n", argv[1] );
	    exit( 1 );
	    }
	}
    else
	{
	(void) fprintf( stderr, usage, argv[0] );
	exit( 1 );
	}

    r = raster_read( f, &cm );
    if ( r == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "couldn't read rasterfile %s\n", argv[1] );
	exit( 1 );
	}

    if ( raster_dump( stdout, r, basename ) != 0 )
	{
	(void) fprintf( stderr, "%s: raster_dump failed\n", argv[0] );
	exit( 1 );
	}

    exit( 0 );
    }
