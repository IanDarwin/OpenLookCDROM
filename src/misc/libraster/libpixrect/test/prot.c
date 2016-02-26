/* prot - rot a Sun screen by rotating random squares
**
** The rotation algorithm is similar to the one in "Smalltalk-80: The
** Language and Implementation", page 408.  In particular, the mask
** refinement step is the same.  However, the rest is different, simpler,
** doesn't flash white bars on the screen, and runs slightly faster (same
** number of blits but smaller area), at the cost of three off-screen
** temp areas instead of two.  I consider this modified version fairly
** obvious, in case anyone is getting any stupid ideas about patenting
** it.  The original version was non-obvious, but memory has gotten a
** lot cheaper since 1983.
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

#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif /*NO_STDLIB_H*/
#include <stdio.h>
#include <pixrect/pixrect_hs.h>

#define PIX_AND ( PIX_SRC & PIX_DST )
#define PIX_OR ( PIX_SRC | PIX_DST )

void pr_rotatepow2square();

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    int argn, delay;
    struct pixrect* disp_pr;
    int wid, hgt, maxpow2;
    int x, y, pow2, cw;
    char* fb;
    char* usage = "usage:  %s [-fb <framebuffer>] [-delay <msec>]\n";

    argn = 1;
    fb = "/dev/fb";
    delay = 0;
    while ( argn < argc && argv[argn][0] == '-' )
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
    for ( maxpow2 = 1;
	 ( 2 << maxpow2 ) <= wid && ( 2 << maxpow2 ) <= hgt;
	 ++maxpow2 )
	;

    srand( (int) ( time( 0 ) ^ getpid( ) ) );

    for (;;)
	{
	pow2 = ( rand() / 23 ) % ( maxpow2 + 1 );
	x = ( rand() / 23 ) % ( wid - ( 1 << pow2 ) );
	y = ( rand() / 23 ) % ( hgt - ( 1 << pow2 ) );
	cw = ( rand() / 23 ) % 2;
	pr_rotatepow2square( disp_pr, x, y, pow2, cw );
	if ( delay != 0 )
#ifdef NO_POLL
	    usleep( (unsigned) delay * 1000 );
#else /*NO_POLL*/
	    poll( 0, 0, (unsigned) delay );
#endif /*NO_POLL*/
	}
    }

static struct pixrect* temp1 = (struct pixrect*) 0;
static struct pixrect* temp2 = (struct pixrect*) 0;
static struct pixrect* mask = (struct pixrect*) 0;

void
pr_rotatepow2square( pr, x, y, pow2, cw )
    struct pixrect* pr;
    int x, y, pow2, cw;
    {
    int s, h, ts, th, tq;

    if ( pow2 < 2 )
	return;
    s = 1 << pow2;
    h = s / 2;

    /* Make sure that temps and mask are big enough and the right depth. */
    if ( temp1 != (struct pixrect*) 0 )
	if ( temp1->pr_size.x < s || temp1->pr_size.y < s ||
	     temp1->pr_depth != pr->pr_depth )
	    {
	    pr_destroy( temp1 );
	    temp1 = (struct pixrect*) 0;
	    pr_destroy( temp2 );
	    temp2 = (struct pixrect*) 0;
	    pr_destroy( mask );
	    mask = (struct pixrect*) 0;
	    }
    if ( temp1 == (struct pixrect*) 0 )
	{
	temp1 = mem_create( s, s, pr->pr_depth );
	temp2 = mem_create( s, s, pr->pr_depth );
	mask = mem_create( s, s, 1 );
	}

    /* Initialize mask to the upper left quadrant. */
    pr_rop( mask, 0, 0, s, s, PIX_CLR, (struct pixrect*) 0, 0, 0 );
    pr_rop( mask, 0, 0, h, h, PIX_SET, (struct pixrect*) 0, 0, 0 );

    for ( ts = s, th = ts / 2, tq = th / 2;
	  pow2 > 0;
	  --pow2, ts = th, th = tq, tq /= 2 )
	{
	if ( cw )
	    pr_rop( temp1,  0,  0, s-th, s-th, PIX_SRC,    pr,    x, y+th );
	else
	    pr_rop( temp1,  0,  0, s-th, s-th, PIX_SRC,    pr, x+th,    y );
	pr_rop( temp1,  0,  0,    s,    s, PIX_AND,  mask,    0,    0 );

	if ( cw )
	    pr_rop( temp2, th,  0, s-th, s-th, PIX_SRC,    pr,    x,    y );
	else
	    pr_rop( temp2, th,  0, s-th, s-th, PIX_SRC,    pr, x+th, y+th );
	pr_rop( temp2, th,  0, s-th, s-th, PIX_AND,  mask,    0,    0 );
	pr_rop( temp1, th,  0, s-th, s-th,  PIX_OR, temp2,   th,    0 );

	if ( cw )
	    pr_rop( temp2, th, th, s-th, s-th, PIX_SRC,    pr, x+th,    y );
	else
	    pr_rop( temp2, th, th, s-th, s-th, PIX_SRC,    pr,    x, y+th );
	pr_rop( temp2, th, th, s-th, s-th, PIX_AND,  mask,    0,    0 );
	pr_rop( temp1, th, th, s-th, s-th,  PIX_OR, temp2,   th,   th );

	if ( cw )
	    pr_rop( temp2,  0, th, s-th, s-th, PIX_SRC,    pr, x+th, y+th );
	else
	    pr_rop( temp2,  0, th, s-th, s-th, PIX_SRC,    pr,    x,    y );
	pr_rop( temp2,  0, th, s-th, s-th, PIX_AND,  mask,    0,    0 );
	pr_rop( temp1,  0, th, s-th, s-th,  PIX_OR, temp2,    0,   th );

	/* And copy back to the screen. */
	pr_rop(    pr,  x,  y,    s,    s, PIX_SRC, temp1,    0,    0 );

	/* Refine mask. */
	pr_rop( mask,  0,  0, s-tq, s-tq, PIX_AND, mask, tq, tq );
	pr_rop( mask,  0, th,    s, s-th,  PIX_OR, mask,  0,  0 );
	pr_rop( mask, th,  0, s-th,    s,  PIX_OR, mask,  0,  0 );
	}
    }
