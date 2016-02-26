/* raster.c - system-dependent parts of raster library
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

#ifndef NO_UNISTD_H
#include <unistd.h>
#endif /*NO_UNISTD_H*/
#include <stdio.h>
#include "raster.h"
#include <malloc.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

#ifdef OLD_FBIO_H_LOC
#include <sun/fbio.h>
#else /*OLD_FBIO_H_LOC*/
#include <sys/fbio.h>
#endif /*OLD_FBIO_H_LOC*/

extern caddr_t mmap();

/* Frees/closes a raster. */
void
raster_free( r )
    struct raster* r;
    {
    if ( ( r->flags & RAS_SUBREGION ) != 0 )
	{}
    else if ( ( r->flags & RAS_FRAMEBUFFER ) != 0 )
	{
	/* SYS */
	register struct raster_fb* rfb = (struct raster_fb*) r->data;

	close( rfb->fd );
	free( (char*) rfb );
	}
    else
	{
	/* Regular raster. */
	if ( ( r->flags & RAS_STATIC ) == 0 )
	    free( (char*) r->pixels );
	}
    if ( ( r->flags & RAS_STATIC ) == 0 )
	free( (char*) r );
    }

/* Opens a frame buffer as a raster.  Returns (struct raster*) 0 on failure. */
struct raster*
raster_open( fbname )
    char* fbname;
    {
    /* SYS */
    register struct raster* r;
    register struct raster_fb* rfb;
    int fd;
    struct fbgattr attr;
    struct fbtype type;
    off_t pix_offset;
    int pix_size, pix_longs;
    int map_size, ru_map_size;
    unsigned long pagemask;
    caddr_t addr, mapaddr;

    fd = open( fbname, O_RDWR );
    if ( fd < 0 )
	return (struct raster*) 0;

    r = (struct raster*) malloc( sizeof(struct raster) );
    rfb = (struct raster_fb*) malloc( sizeof(struct raster_fb) );
    if ( r == (struct raster*) 0 || rfb == (struct raster_fb*) 0 )
	{
	(void) fprintf( stderr, "raster_open: malloc failure\n" );
	close( fd );
	return (struct raster*) 0;
	}

    /* Get frame buffer type - try real type first, in case of emulation. */
    if ( ioctl( fd, FBIOGATTR, &attr ) == 0 )
	type = attr.fbtype;
    else
	{
	if ( ioctl( fd, FBIOGTYPE, &type ) < 0 )
	    {
	    perror( "raster_open - FBIOGTYPE" );
	    close( fd );
	    free( r );
	    free( rfb );
	    return (struct raster*) 0;
	    }
	}
    r->width = type.fb_width;
    r->height = type.fb_height;
    r->depth = type.fb_depth;
    r->flags = RAS_FRAMEBUFFER;
    r->data = (caddr_t) rfb;
    rfb->cmsize = type.fb_cmsize;
    rfb->fd = fd;

    /* Default mmap params - map whole frame buffer, zero offset to pixels. */
    pix_size = type.fb_size;
    pix_offset = 0;

    switch ( type.fb_type )
	{

	case FBTYPE_SUN2BW:
	/* Default mmap params ok. */
	break;

	case FBTYPE_SUN2COLOR:
	if ( r->depth == 1 )
	    {
	    pix_size = r->width * r->height / 8;
	    pix_offset = 0;
#ifdef notdef
	    /* Enable only the first plane and make all even pixels be white,
	    ** while all odd pixels are black.
	    */
	    CG2Mfb.u_ppmask->u_ppmask.ppmask = 1;
	    CG2Mfb.u_csr->u_csr.csr.update_cmap = 0;
	    for ( i = 0; i < 256; i += 2 )
		{
		CG2Mfb.u_cmap->u_cmap.cmap.redmap[i] =
		    CG2Mfb.u_cmap->u_cmap.cmap.greenmap[i] =
			CG2Mfb.u_cmap->u_cmap.cmap.bluemap[i] = 255;
		CG2Mfb.u_cmap->u_cmap.cmap.redmap[i+1] =
		    CG2Mfb.u_cmap->u_cmap.cmap.greenmap[i+1] =
			CG2Mfb.u_cmap->u_cmap.cmap.bluemap[i+1] = 0;
		}
	    CG2Mfb.u_csr->u_csr.csr.update_cmap = 1;
#endif /*notdef*/
	    }
	else
	    {
	    pix_size = r->width * r->height;
	    pix_offset = 0x100000;
#ifdef notdef
	    /* Enable all planes. */
	    CG2Cfb.u_ppmask->u_ppmask.ppmask = 0xFF;
#endif /*notdef*/
	    }
	break;

	case FBTYPE_SUN3COLOR:
	pix_offset = 128 * 1024 * 2;
	pix_size = r->width * r->height;
	break;

	case FBTYPE_SUN4COLOR:
	pix_offset = 128 * 1024 * 2;
	pix_size = r->width * r->height;
	break;

#ifdef FBTYPE_SUNFAST_COLOR
	case FBTYPE_SUNFAST_COLOR:
	/* pix_offset = 0x16000; */
	pix_offset = 128 * 1024 * 2;
	pix_size = r->width * r->height;
	break;
#endif /*FBTYPE_SUNFAST_COLOR*/

	case FBTYPE_SUN1BW:
	case FBTYPE_SUN1COLOR:
	case FBTYPE_SUN2GP:
#ifdef FBTYPE_SUN5COLOR
	case FBTYPE_SUN5COLOR:
#endif /*FBTYPE_SUN5COLOR*/
#ifdef FBTYPE_MEMCOLOR
	case FBTYPE_MEMCOLOR:
#endif /*FBTYPE_MEMCOLOR*/
#ifdef FBTYPE_SUNROP_COLOR
	case FBTYPE_SUNROP_COLOR:
#endif /*FBTYPE_SUNROP_COLOR*/
#ifdef FBTYPE_SUNFB_VIDEO
	case FBTYPE_SUNFB_VIDEO:
#endif /*FBTYPE_SUNFB_VIDEO*/
#ifdef FBTYPE_SUNGIFB
	case FBTYPE_SUNGIFB:
#endif /*FBTYPE_SUNGIFB*/
#ifdef FBTYPE_SUNGPLAS
	case FBTYPE_SUNGPLAS:
#endif /*FBTYPE_SUNGPLAS*/
#ifdef FBTYPE_SUNGP3
	case FBTYPE_SUNGP3:
#endif /*FBTYPE_SUNGP3*/
#ifdef FBTYPE_SUNGT
	case FBTYPE_SUNGT:
#endif /*FBTYPE_SUNGT*/
	(void) fprintf(
	    stderr, "raster_open: fb_type %d is unsupported\n", type.fb_type );
	close( fd );
	free( r );
	free( rfb );
	return (struct raster*) 0;

	default:
	(void) fprintf(
	    stderr, "raster_open: fb_type %d is unknown\n", type.fb_type );
	close( fd );
	free( r );
	free( rfb );
	return (struct raster*) 0;
	}

    map_size = pix_offset + pix_size;

    /* It's not precisely clear that we have to round up
    ** map_size to the nearest page boundary but there are
    ** rumors that this is a good idea and that it shouldn't
    ** hurt anything.
    */
#ifdef NO_SYSCONF
    pagemask = getpagesize() - 1;
#else /*NO_SYSCONF*/
    pagemask = sysconf( _SC_PAGESIZE ) - 1;
#endif /*NO_SYSCONF*/
    ru_map_size = ( map_size + pagemask ) & ~pagemask;

    addr = 0;
#ifdef OLD_MMAP
    /* If we are running pre-SunOS 4.0 then we first need to
    ** allocate some address range for mmap() to replace.
    */
    addr = (caddr_t) valloc( ru_map_size );
    if ( addr == 0 )
	{
	(void) fprintf( stderr, "raster_open: valloc failure\n" );
	close( fd );
	free( r );
	free( rfb );
	return (struct raster*) 0;
	}
#endif /*OLD_MMAP*/
    /* In SunOS 4 and 5, the standard C library mmap() system call
    ** wrapper will automatically add a _MAP_NEW flag for us.
    ** In pre-4.0 mmap(), success returned 0 but now it returns the
    ** newly mapped starting address. The test for mapaddr
    ** being 0 below will handle this difference correctly.
    */
    mapaddr = (caddr_t) mmap(
	addr, ru_map_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, (off_t) 0 );
    if ( mapaddr == (caddr_t) -1 )
	{
	(void) fprintf( stderr, "raster_open: mmap failure\n" );
	close( fd );
	free( r );
	free( rfb );
	return (struct raster*) 0;
	}
    if ( mapaddr == 0 )
	mapaddr = addr;

    r->pixels = (u_long*) ( mapaddr + pix_offset );

    /* Figure out linelongs.  There ought to be a better way. */
    pix_longs = pix_size / sizeof(u_long);
    r->linelongs = pix_longs / r->height;
    if ( r->linelongs * r->height != pix_longs )
	{
	(void) fprintf( stderr, "raster_open: problem computing linelongs\n" );
	close( fd );
	free( r );
	free( rfb );
	return (struct raster*) 0;
	}

    /* NOTE: If your frame buffer's lines aren't a multiple of a
    ** longword, you're probably out of luck.
    */

    return r;
    }

/* Opens a color frame buffer if there is one.  Returns (struct raster*) 0 on
** failure.
*/
struct raster*
raster_coloropen()
    {
    static char *color_fbnames[] = {
        "/dev/cgone0", "/dev/cgone1",
        "/dev/cgtwo0", "/dev/cgtwo1",
        "/dev/cgthree0", "/dev/cgthree1",
        "/dev/cgfour0", "/dev/cgfour1",
        "/dev/cgfive0", "/dev/cgfive1",
        "/dev/cgsix0", "/dev/cgsix1",
        "/dev/cgeight0", "/dev/cgeight1",
        "/dev/cgnine0", "/dev/cgnine1",
        "/dev/gpone0a", "/dev/gpone1a",
        "/dev/fb", (char*) 0
        };
    char** n;
    struct raster* r;

    for ( n = color_fbnames; *n != (char*) 0; ++n )
        {
	r = raster_open( *n );
	if ( r != (struct raster*) 0 )
	    {
	    if ( r->depth > 1 )
		return r;
	    raster_free( r );
	    }
        }
    return (struct raster*) 0;
    }

/* Blanks the screen.  Returns 0 on success, -1 on failure.  This might
** be implemented as actual video blanking, or it might just load black
** into all colormap entries (and disable further colormap changes).
*/
int
raster_video_off( r )
    struct raster* r;
    {
    /* SYS */
    register struct raster_fb* rfb;
    int v = FBVIDEO_OFF;

    if ( ( r->flags & RAS_FRAMEBUFFER ) == 0 )
	return -1;
    rfb = (struct raster_fb*) r->data;
    if ( ioctl( rfb->fd, FBIOSVIDEO, &v ) < 0 )
	{
	perror( "raster_video_off - FBIOSVIDEO" );
	return -1;
	}
    return 0;
    }

/* Re-enables video.  Returns 0 on success, -1 on failure. */
int
raster_video_on( r )
    struct raster* r;
    {
    /* SYS */
    register struct raster_fb* rfb;
    int v = FBVIDEO_ON;

    if ( ( r->flags & RAS_FRAMEBUFFER ) == 0 )
	return -1;
    rfb = (struct raster_fb*) r->data;
    if ( ioctl( rfb->fd, FBIOSVIDEO, &v ) < 0 )
	{
	perror( "raster_video_on - FBIOSVIDEO" );
	return -1;
	}
    return 0;
    }

/* Allocates a colormap structure and returns the frame buffer's
** current colormap, or (struct raster_colormap*) 0 on failure.  The raster
** must be one returned by raster_open(), not raster_alloc(); i.e. a frame
** buffer.
*/
struct raster_colormap*
raster_colormap_get( r )
    register struct raster* r;
    {
    /* SYS */
    register struct raster_fb* rfb;
    register struct raster_colormap* cm;
    struct fbcmap fbcm;

    if ( ( r->flags & RAS_FRAMEBUFFER ) == 0 )
	return (struct raster_colormap*) 0;
    rfb = (struct raster_fb*) r->data;
    cm = raster_colormap_alloc( rfb->cmsize );
    fbcm.index = cm->ind;
    fbcm.count = cm->len;
    fbcm.red = cm->red;
    fbcm.green = cm->grn;
    fbcm.blue = cm->blu;
    if ( ioctl( rfb->fd, FBIOGETCMAP, &fbcm ) < 0 )
	{
	perror( "raster_colormap_get - FBIOGETCMAP" );
	raster_colormap_free( cm );
	return (struct raster_colormap*) 0;
	}
    return cm;
    }

/* Sets a frame buffer's colormap.  The raster must be one returned
** by raster_open(), not raster_alloc(); i.e. a frame buffer.  Returns
** 0 on success, -1 on failure.
*/
int
raster_colormap_set( r, cm )
    register struct raster* r;
    register struct raster_colormap* cm;
    {
    /* SYS */
    register struct raster_fb* rfb;
    struct fbcmap fbcm;

    if ( ( r->flags & RAS_FRAMEBUFFER ) == 0 )
	return -1;
    rfb = (struct raster_fb*) r->data;
    fbcm.index = cm->ind;
    fbcm.count = cm->len;
    fbcm.red = cm->red;
    fbcm.green = cm->grn;
    fbcm.blue = cm->blu;
    if ( ioctl( rfb->fd, FBIOPUTCMAP, &fbcm ) < 0 )
	{
	perror( "raster_colormap_set - FBIOPUTCMAP" );
	return -1;
	}
    return 0;
    }
