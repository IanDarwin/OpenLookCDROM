/* dumpfont - write out a vfont as C structs
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
    struct raster_font* rf;
    char* usage = "usage:  %s <fontname>\n";
    char fullname[200];
    char basename[200];
    char* cp;

    if ( argc != 2 )
	{
	(void) fprintf( stderr, usage, argv[0] );
	exit( 1 );
	}

    if ( strchr( argv[1], '/' ) != (char*) 0 )
	{
	(void) strcpy( fullname, argv[1] );
	cp = strchr( argv[1], '/' );
	if ( cp != (char*) 0 )
	    (void) strcpy( basename, cp + 1 );
	else
	    (void) strcpy( basename, argv[1] );
	}
    else
	{
	(void) sprintf( fullname, "/usr/lib/fonts/fixedwidthfonts/%s", argv[1] );
	(void) strcpy( basename, argv[1] );
	}
    cp = strchr( basename, '.' );
    if ( cp != (char*) 0 )
	*cp = '\0';

    rf = raster_fontopen( fullname );
    if ( rf == (struct raster_font*) 0 )
	{
	(void) fprintf( stderr, "couldn't open font %s\n", fullname );
	exit( 1 );
	}

    if ( raster_fontdump( stdout, rf, basename ) != 0 )
	{
	(void) fprintf( stderr, "%s: raster_fontdump failed\n", argv[0] );
	exit( 1 );
	}

    exit( 0 );
    }
