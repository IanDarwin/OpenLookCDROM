/* screenload - screen loading program
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
#include <string.h>
#include <sys/types.h>
#include <raster.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/time.h>
#include <signal.h>

#define max(a,b) ( ((int)(a)) > ((int)(b)) ? (a) : (b) )

/* Externals . */
extern long strtol();
extern long time();

/* Forward routines. */
static void usage();
static void load_one();
static void randomize();
static void alarm_handler();

/* Globals. */
static char* argv0;
static int fill_flag, tile_flag, pause_flag, random_flag;
static int fill_color;
static struct raster* fill_r;
static u_long gray_bits[] = { 0xeeeeeeee, 0xbbbbbbbb, 0xdddddddd, 0x77777777 };
static struct raster gray_r = {
    32, 4, 1, 1, gray_bits, RAS_STATIC, (caddr_t) 0 };
static int sleep_msec;
static struct itimerval itv;
static u_long random_bits[32];
static struct raster random_r = {
    32, 32, 1, 1, random_bits, RAS_STATIC, (caddr_t) 0 };
static struct raster hex_r = {
    32, -1, 1, 1, (u_long*) 0, RAS_STATIC, (caddr_t) 0 };
static char* fb_name;
static struct raster* fb;
static int alarm_semaphore;

void
main( argc, argv )
int argc;
char* argv[];
    {
    int argn;
    FILE* fp;

    argv0 = *argv;

    /* Parse flags. */
    argn = 1;
    fb_name = (char*) 0;
    fill_flag = 1;
    fill_r = (struct raster*) 0;
    fill_color = -1;
    tile_flag = 0;
    pause_flag = 0;
    sleep_msec = 0;
    while ( argn < argc && argv[argn][0] == '-' && argv[argn][0] != '\0' )
	{
	if ( strncmp(argv[argn],"-black",max(strlen(argv[argn]),2)) == 0 )
	    {
	    fill_color = -1;
	    }
	else if ( strncmp(argv[argn],"-white",max(strlen(argv[argn]),2)) == 0 )
	    {
	    fill_color = 0;
	    }
	else if ( strncmp(argv[argn],"-index",max(strlen(argv[argn]),2)) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    fill_color = atoi( argv[argn] );
	    }
	else if ( strncmp(argv[argn],"-gray",max(strlen(argv[argn]),2)) == 0 ||
	          strncmp(argv[argn],"-grey",max(strlen(argv[argn]),2)) == 0 )
	    {
	    fill_r = &gray_r;
	    }
	else if (strncmp(argv[argn],"-hexfill",max(strlen(argv[argn]),2)) == 0)
	    {
	    int count, i;
	    short* bits;

	    ++argn;
	    if ( argn >= argc )
		usage();
	    count = atoi( argv[argn] );
	    if ( count <= 0 )
		usage();
	    bits = (short*) malloc( (unsigned) ( 2 * count * sizeof(short) ) );
	    if ( bits == (short*) 0 )
		{
		(void) fprintf(
		    stderr, "%s: out of memory allocating for hex pattern\n",
		    argv0 );
		exit( 1 );
		}
	    for ( i = 0; i < count; ++i )
		{
		++argn;
		if ( argn >= argc )
		    usage();
		bits[i * 2] = (short) strtol( argv[argn], (char**) 0, 16 );
		}
	    hex_r.height = count;
	    hex_r.pixels = (u_long*) bits;
	    fill_r = &hex_r;
	    }
	else if ( strncmp(argv[argn],"-random",max(strlen(argv[argn]),2)) == 0 )
	    {
	    random_flag = 1;
	    fill_r = &random_r;
	    srand( (int) time( (time_t*) 0 ) ^ getpid() );
	    }
	else if ( strncmp(argv[argn],"-nofill",max(strlen(argv[argn]),2)) == 0 )
	    {
	    fill_flag = 0;
	    }
	else if ( strncmp(argv[argn],"-tile",max(strlen(argv[argn]),2)) == 0 )
	    {
	    tile_flag = 1;
	    }
	else if ( strncmp(argv[argn],"-pause",max(strlen(argv[argn]),2)) == 0 )
	    {
	    pause_flag = 1;
	    }
	else if ( strncmp(argv[argn],"-sleep",max(strlen(argv[argn]),2)) == 0 )
	    {
	    ++argn;
	    if ( argn >= argc )
		usage();
	    sleep_msec = atoi( argv[argn] );
	    alarm_semaphore = 1;	/* so that first time doesn't wait */
	    if ( signal( SIGALRM, alarm_handler ) != 0 )
		{
		(void) fprintf(
		    stderr, "%s: error setting sleep alarm handler\n", argv0 );
		exit( 1 );
		}
	    itv.it_interval.tv_sec = 0;
	    itv.it_interval.tv_usec = 0;
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
    
    /* Open framebuffer. */
    fb = raster_open( fb_name == (char*) 0 ? "/dev/fb" : fb_name );
    if ( fb == (struct raster*) 0 )
	{
	(void) fprintf(
	    stderr, "%s: can't open framebuffer %s\n", argv0,
	    fb_name == (char*) 0 ? "/dev/fb" : fb_name );
	exit( 1 );
	}

    if ( argn == argc )
	{
	/* No rasterfiles given - load from stdin. */
	load_one( stdin );
	}
    else
	{
	/* Loop through remaining args. */
	while ( argn < argc )
	    {
	    /* Open rasterfile. */
	    if ( strcmp( argv[argn], "-" ) == 0 )
		fp = stdin;
	    else
		{
		fp = fopen( argv[argn], "r" );
		if ( fp == (FILE*) 0 )
		    {
		    perror( argv[argn] );
		    exit( 1 );
		    }
		}


	    /* Load it. */
	    load_one( fp );

	    /* Close rasterfile. */
	    if ( fp != stdin )
		(void) fclose( fp );

	    ++argn;
	    }
	}

    /* All done. */
    exit( 0 );
    }

static void
usage()
    {
    char* use =
"usage:  %s [-black] [-white] [-index <color>] [-gray]\n\
        [-hexfill <count> <data> ...] [-random] [-nofill] [-tile]\n\
        [-pause] [-sleep <msec>] [-fb <framebuffer>] [<rasterfile> ...]\n";

    (void) fprintf( stderr, use, argv0 );
    exit( 1 );
    }

static void
load_one( fp )
    FILE* fp;
    {
    struct raster* r;
    struct raster_colormap* cm;
    int dx, dy;

    /* Load rasterfile into raster. */
    r = raster_read( fp, &cm );
    if ( r == (struct raster*) 0 )
	{
	(void) fprintf( stderr, "%s: error reading rasterfile\n", argv0 );
	exit( 1 );
	}

    /* Can we display this raster? */
    if ( r->depth > fb->depth )
	{
	/* No.  Was the framebuffer name defaulted? */
	if ( fb_name == (char*) 0 )
	    {
	    /* Ok, then check if there's a color framebuffer available. */
	    raster_free( fb );
	    fb = raster_coloropen();
	    if ( fb == (struct raster*) 0 )
		{
		(void) fprintf(
		    stderr, "%s: can't display color rasterfile\n", argv0 );
		exit( 1 );
		}
	    }
	else
	    {
	    (void) fprintf(
		stderr, "%s: can't display color rasterfile on %s\n", argv0,
		fb_name );
	    exit( 1 );
	    }
	}

    /* If we're randomizing the background, do it now. */
    if ( random_flag )
	randomize();

    /* Ready to display - if the -sleep flag was specified, wait until the
    ** proper moment.
    */
    if ( sleep_msec > 0 )
	{
	while ( alarm_semaphore == 0 )
	    (void) sigpause( 0 );
	/* Reset the timer.  (Can't use the self-resetting mode for
	** various reasons.)
	*/
	alarm_semaphore = 0;
	itv.it_value.tv_sec = sleep_msec / 1000;
	itv.it_value.tv_usec = ( sleep_msec % 1000 ) * 1000;
	if ( setitimer( ITIMER_REAL, &itv, (struct itimerval *) 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "%s: error setting sleep timer\n", argv0 );
	    exit( 1 );
	    }
	}

    /* Ok, let's display. */
    if ( cm != (struct raster_colormap*) 0 )
	if ( raster_colormap_set( fb, cm ) != 0 )
	    {
	    (void) fprintf(
		stderr, "%s: raster_colormap_set() failed\n", argv0 );
	    exit( 1 );
	    }
    if ( tile_flag )
	{
	if ( raster_replsrc(
		fb, 0, 0, fb->width, fb->height,
		RAS_SRC | RAS_COLOR( fill_color ), r, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "%s: raster_replsrc() failed\n", argv0 );
	    exit( 1 );
	    }
	}
    else
	{
	/* Figure out the offset to center the image. */
	dx = fb->width - r->width;
	dy = fb->height - r->height;
	if ( dx < 0 || dy < 0 )
	    {
	    (void) fprintf(
		stderr, "%s: warning, image too big for display\n", argv0 );
	    dx = dy = 0;
	    }
	else
	    {
	    dx /= 2;
	    dy /= 2;
	    }
	/* Display background if necessary. */
	if ( fill_flag && ( dx > 0 || dy > 0 ) )
	    if ( fill_r == (struct raster*) 0 )
		{
		if ( raster_op(
			fb, 0, 0, fb->width, fb->height,
			RAS_SET | RAS_COLOR( fill_color ), fill_r, 0, 0 ) != 0 )
		    {
		    (void) fprintf( stderr, "%s: raster_op() failed\n", argv0 );
		    exit( 1 );
		    }
		}
	    else
		{
		if ( raster_replsrc(
			fb, 0, 0, fb->width, fb->height,
			RAS_SRC | RAS_COLOR( fill_color ), fill_r, 0, 0 ) != 0 )
		    {
		    (void) fprintf(
			stderr, "%s: raster_replsrc() failed\n", argv0 );
		    exit( 1 );
		    }
		}
	/* Display image. */
	if ( raster_op(
		fb, dx, dy, r->width, r->height,
		    RAS_SRC | RAS_COLOR( fill_color ), r, 0, 0 ) != 0 )
	    {
	    (void) fprintf( stderr, "%s: raster_op() failed\n", argv0 );
	    exit( 1 );
	    }
	}

    /* Free raster. */
    raster_free( r );
    if ( cm != (struct raster_colormap*) 0 )
	raster_colormap_free( cm );

    /* If -pause was specified, wait for a newline. */
    if ( pause_flag )
	{
	int c;

	do
	    {
	    c = getchar();
	    }
	while ( c != EOF && c != '\n' );
	}
    }

static void
randomize()
    {
    int i;

    for ( i = 0; i < sizeof( random_bits ) / sizeof( *random_bits ); ++i )
	random_bits[i] = rand() ^ ( rand() * 2 );	/* top bit too */
    }

static void
alarm_handler()
    {
    alarm_semaphore = 1;
    }
