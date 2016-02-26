/* raster_dump.c - dump rasters and fonts as C structs
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
#include "raster.h"


/* Writes a raster as C structs.  Returns 0 on success, -1 on failure.  */
int
raster_dump( f, r, name )
    FILE* f;
    struct raster* r;
    char* name;
    {
    u_long* lp;
    int i;

    (void) fprintf( f, "u_long %s_pixels[] = {", name );
    for ( i = 0, lp = r->pixels; i < r->height * r->linelongs; ++i, ++lp )
	{
	if ( i == 0 )
	    (void) fprintf( f, "\n    " );
	else if ( i % 6 == 0 )
	    (void) fprintf( f, ",\n    " );
	else
	    (void) fprintf( f, ", " );
	(void) fprintf( f, "0x%08x", *lp );
	}
    (void) fprintf( f, "\n    };\n" );

    (void) fprintf(
	f, "struct raster %s = { %d, %d, %d, %d, %s_pixels,\n",
	name, r->width, r->height, r->depth, r->linelongs, name );
    (void) fprintf( f, "    RAS_STATIC, (caddr_t) 0 };\n" );

    return 0;
    }

/* Writes a font as C structs.  Returns 0 on success, -1 on failure.  */
int
raster_fontdump( f, rf, name )
    FILE* f;
    struct raster_font* rf;
    char* name;
    {
    int i;
    char rastername[100];
    static char* asciiname[256] = {
	"NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	"BS", "HT", "NL", "VT", "NP", "CR", "SO", "SI",
	"DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	"CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US",
	"SP", "bang", "dquote", "hash", "dollar", "percent", "amper", "squote",
	"lparen", "rparen", "star", "plus", "comma", "dash", "dot", "slash",
	"dig0", "dig1", "dig2", "dig3", "dig4", "dig5", "dig6", "dig7",
	"dig8", "dig9", "colon", "semi", "less", "equal", "greater", "qmark",
	"at", "A", "B", "C", "D", "E", "F", "G",
	"H", "I", "J", "K", "L", "M", "N", "O",
	"P", "Q", "R", "S", "T", "U", "V", "W",
	"X", "Y", "Z", "lbracket", "bslash", "rbracket", "hat", "ubar",
	"bquote", "a", "b", "c", "d", "e", "f", "g",
	"h", "i", "j", "k", "l", "m", "n", "o",
	"p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "lbrace", "vbar", "rbrace", "tilde", "DEL",
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};

    for ( i = 0; i < 256; ++i )
	if ( rf->chars[i].r != (struct raster*) 0 )
	    {
	    if ( asciiname[i] != (char*) 0 )
		(void) sprintf( rastername, "%s_%s", name, asciiname[i] );
	    else
		(void) sprintf( rastername, "%s_%d", name, i );
	    if ( raster_dump( f, rf->chars[i].r, rastername ) != 0 )
		return -1;
	    (void) fprintf( f, "\n" );
	    }

    (void) fprintf( f, "struct raster_font %s = {\n", name );
    (void) fprintf( f, "    %d, %d, ", rf->width, rf->height );
    if ( rf->flags & RASFONT_FIXEDWIDTH )
	{
	if ( rf->flags & RASFONT_NOVERTICALMOVEMENT )
	    (void) fprintf( f,
	     "RASFONT_STATIC|RASFONT_FIXEDWIDTH|RASFONT_NOVERTICALMOVEMENT, " );
	else
	    (void) fprintf( f, "RASFONT_STATIC|RASFONT_FIXEDWIDTH, " );
	}
    else
	{
	if ( rf->flags & RASFONT_NOVERTICALMOVEMENT )
	    (void) fprintf( f, "RASFONT_STATIC|RASFONT_NOVERTICALMOVEMENT, " );
	else
	    (void) fprintf( f, "RASFONT_STATIC, " );
	}
    (void) fprintf( f, "(caddr_t) 0,\n" );

    for ( i = 0; i < 256; ++i )
	{
	if ( rf->chars[i].r != (struct raster*) 0 )
	    if ( asciiname[i] != (char*) 0 )
		(void) fprintf( f, "    &%s_%s, ", name, asciiname[i] );
	    else
		(void) fprintf( f, "    &%s_%d, ", name, i );
	else
	    (void) fprintf( f, "    0, " );
	(void) fprintf(
	    f, "%d, %d, %d, %d",
	    rf->chars[i].homex, rf->chars[i].homey,
	    rf->chars[i].nextx, rf->chars[i].nexty );
	(void) fprintf( f, ",\n" );
	}
    (void) fprintf( f, "    };\n" );
    (void) fprintf( f, "\n" );

    return 0;
    }
