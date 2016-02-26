/*
 * (c) Copyright 1993 Scott Oaks.  See LEGAL_NOTICE file for terms of the
 * license
 */

#ifdef IDENT
#ident "@(#)pixmap.c	1.3 olvwm version 09 Feb 1994"
#endif

/*
 * Various pixmap support routines:  given a filename, functions in this
 * file will create a 1 or n-bit pixmap from a bitmap, GIF, or XBM image.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/XWDFile.h>

#ifdef XPM
#ifndef NO_PIXEL_FIX
/* The problem with xpm.h is that it includes <X11/Intrinsic.h> to get
 * the typedef for Pixel.  Intrinsic.h, however, also typedefs Boolean
 * (to char).  This causes the compiler to fail on the Boolean typedef
 * in <olgx/olgx.h> (which is, of course, also char). <olgx/olgx.h> is
 * included within "win.h".
 * This kludge typedefs Pixel and defines _XtIntrinsic_h so that the
 * real Intrinsic.h does not get included.
 */
#define _XtIntrinsic_h
typedef unsigned long   Pixel;      /* Index into colormap              */
#endif  /* NO_PIXEL_FIX */
#include <xpm.h>
#endif XPM

#include "i18n.h"
#include <olgx/olgx.h>

#include "olwm.h"
#include "ollocale.h"
#include "globals.h"
#include "screen.h"
#include "mem.h"

_swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
        sp = bp + 3;
        c = *sp;
        *sp = *bp;
        *bp++ = c;
        sp = bp + 1;
        c = *sp;
        *sp = *bp;
        *bp++ = c;
        bp += 2;
    }  
}

/*
 * GIF support
 */
unsigned ImageSize(image)
     XImage *image;
{
    if (image->format != ZPixmap)
      return(image->bytes_per_line * image->height * image->depth);

    return((unsigned)image->bytes_per_line * image->height);
}

int readGifFile(dpy, drawable, fn, w, h, pPix, hotx, hoty, colormap,
		ncolors, colors)
Display *dpy;
Drawable drawable;
char *fn;
unsigned int *w, *h;
Pixmap *pPix;
int *hotx, *hoty;
Colormap *colormap;
int *ncolors;
XColor **colors;

{
      FILE *fin;
      int ret;
      int screen;
      XImage *in_image, *out_image, *ReadGIF();
      XWDFileHeader header;
      XVisualInfo vinfo, *vinfos;
      XColor color;
      GC gc;
      XGCValues gc_val;


      if (fn && ((fin = fopen (fn, "r")) == NULL))
              return BitmapOpenFailed;

      ret = BitmapFileInvalid;

      FreePixmapColors(dpy, *ncolors, *colors, colormap);
      if ((in_image = ReadGIF(dpy, fin, ncolors, colors)) != NULL)
      {
              screen = DefaultScreen(dpy);

              /* Get the color map */

              /* Create the output image */
              out_image = XCreateImage(dpy, DefaultVisual(dpy, screen),
                               in_image->depth,
                               in_image->format,
                               in_image->xoffset, NULL,
                               in_image->width, in_image->height,
                               XBitmapPad(dpy), 0);
              out_image->data = (char *) MemAllocN(ImageSize(out_image));
              doPseudo(dpy, colormap, *ncolors, *colors, in_image,
				out_image);
              MemFree (in_image->data);
              MemFree (in_image);

              if (out_image->depth == 1)
              {
                      if (*ncolors && XAllocColor(dpy, *colormap, &(*colors)[1]))
                              gc_val.foreground = (*colors)[1].pixel;
                      else
                              gc_val.foreground = BlackPixel (dpy, screen);

                      if (*ncolors && XAllocColor(dpy, *colormap, &(*colors)[0]))
                              gc_val.background = (*colors)[0].pixel;
                      else
                              gc_val.background = WhitePixel (dpy, screen);
              }
              else
              {
                      gc_val.background = XGetPixel(out_image, 0, 0);
                      gc_val.foreground = 0;
              }

              *pPix = XCreatePixmap (dpy, drawable, out_image->width,
				out_image->height, out_image->depth);
              if (gc = XCreateGC(dpy, *pPix, GCForeground|GCBackground,
				&gc_val))
              {
                      XPutImage(dpy, *pPix, gc, out_image, 0, 0, 0, 0,
                              out_image->width, out_image->height);
                      XFreeGC(dpy, gc);

                      *w = out_image->width;
                      *h = out_image->height;

                      ret = BitmapSuccess;
              }
              else
                      ret = BitmapNoMemory;

              XDestroyImage(out_image);
      }

       /* close the input file */
      (void) fclose(fin);

      return ret;
}

FreePixmapColors(dpy, ncolors, colors, colormap)
    Display *dpy;
    int ncolors;
    XColor *colors;
    Colormap *colormap;
{
int i;

    if (ncolors) {
	for (i = 0; i < ncolors; i++)
	    if (colors[i].flags) {
		XFreeColors (dpy, *colormap, &(colors[i].pixel), 1, 0);
	    }

	MemFree (colors);
    }
}

doPseudo(dpy, colormap, ncolors, colors, in_image, out_image)
Display *dpy;
Colormap *colormap;
int ncolors;
XColor *colors;
register XImage *in_image, *out_image;

{
    register int i, x, y;
    register XColor *color;

    for (i = 0; i < ncolors; i++)
	colors[i].flags = 0;
/* Re-map colors? */
    if (GRV.MaxMapColors < ncolors)
    {
	XColor *badColor, *okColor, *theColor;
	double this, min;
	XColor *new_palette, *reduce();

/* Find a a good set of colors to use */
	new_palette = reduce (in_image, ncolors, colors, GRV.MaxMapColors);

/* Re-map all colors to the closest allowed colors */

/* Loop through the original colors */
	for (y=0; y<ncolors; y++)
	{
	    badColor = &colors[y];
	    min = (double) 0xffff * (double) 0xffff * 3.0;

/* Loop through the new allowed colors */
	    for (x=0; x < GRV.MaxMapColors; x++)
	    {
		okColor = &new_palette[x];

/* Calculate the difference */
		this = ((double) (okColor->red) - (double) (badColor->red)) *
		       ((double) (okColor->red) - (double) (badColor->red))
		     + ((double) (okColor->green) - (double)
							(badColor->green)) *
		       ((double) (okColor->green) - (double)
							(badColor->green))
		     + ((double) (okColor->blue) - (double) (badColor->blue))
			* ((double) (okColor->blue) - (double)
					(badColor->blue));

/* Closest so far? */
		if (this < min)
		{
		    min = this;
		    theColor = okColor;
		}
	    }

/* OK, got the closest */
	    badColor->red = theColor->red;
	    badColor->green = theColor->green;
	    badColor->blue = theColor->blue;
	}

	MemFree (new_palette);
    }

/* Now transform the pixel values to those allocated by the server */
    for (y = 0; y < in_image->height; y++)
    {
	for (x = 0; x < in_image->width; x++)
	{
	    color = &colors[XGetPixel(in_image, x, y)];

	    if (!color->flags)
	    {
		color->flags = DoRed | DoGreen | DoBlue;

		if (!XAllocColor(dpy, *colormap, color))
		{
		    char buf[1024];
		    sprintf(buf, "%s\n%s\n%s",
			"Can't allocate enough colors for all the pixmaps",
			"Either use fewer color pixmaps or set",
			"OpenWindows.MaxMapColors to a smaller number");
		    ErrorGeneral(buf);
#ifdef not
		    /* This is no good -- then we have colormap flashing all
		     * over the place.  And it only sometimes works */
		    *colormap = XCopyColormapAndFree(dpy, *colormap);
		    XAllocColor(dpy, *colormap, color);
#endif
		}
	    }
	    XPutPixel(out_image, x, y, color->pixel);
	}
    }
}

/*
 * Sun Icon support (not presently working)
 *
 */
#ifdef SUNICON
SunReadIconFile(dpy, drawable, filename, width, height, bitmap)
    Display     *dpy;
    Window      drawable;
    char        *filename;
    int         *width, *height;
    Pixmap      *bitmap;

{
FILE    *fp;
char    s[256];
int     version = 0, depth = 0, iconbitsz = 0;
char    *data, *pd;
int     icon_word, xbm_bits, bit_loop, xbm_loop, xbm_bytes_per_icon_word;
int     xbm_byte_count, width_loop, height_loop, icon_words_per_line, c;
Bool    half_of_last;

    fp = fopen(filename, "r");
    if (!fp)
        return False;
    fgets(s, 255, fp);
    sscanf(s, "/* Format_version=%d, Width=%d, Height=%d, Depth=%d, Valid_bits_per_item=%d",
           &version, width, height, &depth, &iconbitsz);
    if (version != 1 || depth != 1 || (iconbitsz != 16 && iconbitsz != 32))
{
        ErrorWarning(GetString("An invalid SunIcon file was named as a bitmap"));
        fclose(fp);
        return False;
    }
    if (*width % 16) {
        ErrorWarning(GetString("An SunIcon file with an invalid width was named as a bitmap"));
        fclose(fp);
        return False;
    }
    data = MemAlloc((*width / 8) * *height);
    pd = data;

    while ((c = getc(fp)) != EOF) {
        if (c == '*') {
            do {
                if (((c = getc(fp)) != EOF) ** (c == '/'))
                    break;
            } while (c == '*');
            if (c == '/')
                break;
        }
    }    
    if (c == EOF) {
        ErrorWarning(GetString("An invalid SunIcon file was named as a bitmap"));
        fclose(fp);
        return False;
    }

    xbm_bytes_per_icon_word = iconbitsz / 8;
    icon_words_per_line = *width / iconbitsz;
    if ( (*width % iconbitsz) != 0 ) {
        /* a 32-bit icon image that only uses half of the last icon_word */
        icon_words_per_line += 1;
        half_of_last = True;
    }
    else half_of_last = False;
    for (height_loop = *height; --height_loop >= 0; ) {
        for ( width_loop = icon_words_per_line ; --width_loop >= 0 ; ) {
            /* position on 0 of 0xFFFF */
            while ( ((c = getc(fp)) != EOF) && (c != '0') ) {
                ;       /* nothing else to do */
            }   /* endwhile */
            c = fscanf(fp, "x%X", &icon_word);
            if ( (width_loop == 0) && half_of_last )
                xbm_byte_count = xbm_bytes_per_icon_word / 2;
            else xbm_byte_count = xbm_bytes_per_icon_word;
            for (xbm_loop = xbm_byte_count ; --xbm_loop >= 0 ; ) {
                xbm_bits = 0;
                for (bit_loop = 1 ; bit_loop <= 8 ; ++bit_loop) {
                    xbm_bits <<= 1;
                    xbm_bits |= (icon_word & 0x0001);
                    icon_word >>= 1;
                }
                *(pd + xbm_loop) = xbm_bits;
            }
            pd += xbm_bytes_per_icon_word;
        }
    }    

    *bitmap = XCreateBitmapFromData(dpy, drawable, data, *width, *height);
    fclose(fp);
    if (!*bitmap)
        return False;
    return True;
}
#endif SUNICON

#ifdef NOT
/*
 * Bitmap support
 */
static Bool
makePixmapFromBitmap(dpy, scrInfo, bitmapfile, pixmap)
        Display         *dpy;
        ScreenInfo      *scrInfo;
        char            *bitmapfile;
        Pixmap          *pixmap;                /* RETURN */
{
        char            *bmPath;
        Pixmap          bitmap;
        unsigned int    width,height;
        int             x,y;
        int             status = BitmapNoMemory;
        GC              gc;
        XGCValues       gcv;
        Bool            freeBitmap = False;
 
        status = XReadBitmapFile(dpy,scrInfo->rootid,bmPath,
                                &width,&height,&bitmap,&x,&y);
	if (!status)
	    freeBitmap = True;

        /*
         *      Create a screen depth pixmap from the bitmap
         */
        gcv.foreground = scrInfo->colorInfo.workspaceBitmapFg;
        gcv.background = scrInfo->colorInfo.workspaceBitmapBg;
        gc = XCreateGC(dpy,scrInfo->rootid,
                        GCForeground|GCBackground,&gcv);
        *pixmap = XCreatePixmap(dpy,scrInfo->rootid,
                        width,height,scrInfo->depth);
        XCopyPlane(dpy,bitmap,*pixmap,gc,0,0,width,height,0,0,1);
        XFreeGC(dpy,gc);

        if (freeBitmap)
            XFreePixmap(dpy,bitmap);

        return True;
}
#endif

typedef enum { BadFormat, XBitmapFormat,
                SunIconFormat, XPixmapFormat, GifFormat } ImageFormat;

ImageFormat
imageFileFormat(filename)
    char        *filename;

{
FILE    *fp;
char    s[256];
int     dummy;
 
    if ((fp = fopen(filename, "r")) == NULL)
        return BadFormat;
    fgets(s, 255, fp);
    fclose(fp);
#ifdef XPM
    if (strncmp(s, "/* XPM */", 9) == 0) /* XPM 3.x format */
        return XPixmapFormat;
    if (strncmp(s, "! XPM", 5) == 0)    /* XPM 2.x format */
        return XPixmapFormat;
#endif
    if (strncmp(s, "GIF", 3) == 0)
        return GifFormat;
    if (s[0] == '#')
        return XBitmapFormat;
    if (sscanf(s, "/* Format_version=%d", &dummy) < 1)
        return BadFormat;
    return SunIconFormat;
}      

/*
 * Bitmap Search Path
 */
static  char    **bitmapSearchPath;             /* bitmap search path */

/*
 * makeBitmapSearchPath
 *
 *      Construct bitmap search path as follows:
 *              $OPENWINHOME/etc/workspace/patterns
 *              $OPENWINHOME/include/X11/include/bitmaps
 *              /usr/X11/include/X11/include/bitmaps
 *
 * REMIND: this should be cleaned up so that it doesn't use a fixed-size
 * array.
 */
static void
makeBitmapSearchPath()
{
        char    bmPath[MAXPATHLEN];
        char    *owHome;
        int     i = 0;;

        if ((owHome = getenv("OPENWINHOME")) == NULL)
                owHome = "/usr/openwin";

        bitmapSearchPath = (char **)MemAlloc(4 * sizeof(char *));

        (void)sprintf(bmPath, "%s/etc/workspace/patterns",owHome);
        bitmapSearchPath[i++] = MemNewString(bmPath);

        (void)sprintf(bmPath, "%s/include/X11/bitmaps",owHome);
        bitmapSearchPath[i++] = MemNewString(bmPath);

        bitmapSearchPath[i++] = MemNewString("/usr/X11/include/X11/bitmaps");

        bitmapSearchPath[i] = (char *)NULL;
}

/*
 * findBitmapFile
 *      Finds a bitmap file in the bitmap search path;
 *      Returns a dynamically allocated string containing the
 *      fullpath to the bitmap file.
 */
static char *
findBitmapFile(fileName)
        char    *fileName;
{
        char    **dir;
        char    fullPath[MAXPATHLEN];
	char	*new, *ExpandPath();
        int     i;
	Bool	freeFile = False;

        if (bitmapSearchPath == NULL)
                makeBitmapSearchPath();

	if ((new = ExpandPath(fileName, False)) != NULL) {
	    fileName = new;
	    freeFile = True;
	}

        if (fileName[0] == '/' && (access(fileName, R_OK) == 0)) {
                new = MemNewString(fileName);
	 	if (freeFile)
		    MemFree(fileName);
                return new;
	}

        for (dir = bitmapSearchPath; *dir; dir++) {

                (void)sprintf(fullPath,"%s/%s",*dir,fileName);

                if (access(fullPath, R_OK) == 0) {
                	new = MemNewString(fullPath);
	 		if (freeFile)
		    	    MemFree(fileName);
                	return new;
		}
        }

	if (freeFile)
		MemFree(fileName);
        return (char *)NULL;
}

/*
 * Global Functions
 */

/*
 * MakePixmap
 *
 *  Given a filename, return a pixmap.  Currently only supports X11 bitmap
 *  files, X11 pixmap files, and GIF files, but will someday support Sun
 *  Icon files too
 *
 * REMIND:  What about different screens and colors?
 */

Bool
MakePixmap(dpy, scrInfo, filename, pixinfo)
Display         *dpy;
ScreenInfo      *scrInfo;
char            *filename;
PixInfo         *pixinfo;      /* RETURN */
{
Drawable  drawable = scrInfo->rootid;
int       depth    = scrInfo->depth;
Pixmap    bitmap;
int       xhot, yhot;
XGCValues       gcv;
GC	  gc;
int       rval;
char     *newname;
Bool	  freeFilename = False;
#ifdef XPM
XpmAttributes   xpmAttr;
#endif
 
        if ((newname = findBitmapFile(filename)) != NULL) {
            filename = newname;
	    freeFilename = True;
	}
        rval = False;
        switch ( imageFileFormat(filename) ) {
            case XBitmapFormat:
                if (XReadBitmapFile(dpy, drawable,
                            filename, (unsigned int *) &(pixinfo->width),
                            (unsigned int *) &(pixinfo->height),
                            &bitmap, &xhot, &yhot) != BitmapSuccess) {
                        goto FAILURE;
                }
                pixinfo->pixmap = XCreatePixmap(dpy, drawable,
                        pixinfo->width, pixinfo->height, depth);
                gcv.foreground = pixinfo->fg;
                gcv.background = pixinfo->bg;
                gc = XCreateGC(dpy, drawable, GCForeground|GCBackground, &gcv);
                XCopyPlane(dpy, bitmap, pixinfo->pixmap,
                        gc,
                        0, 0, pixinfo->width, pixinfo->height, 0, 0, 1);
                XFreePixmap(dpy, bitmap);
                XFreeGC(dpy, gc);
                rval = True;
                break;
#ifdef SUNICON
            case SunIconFormat:
                if (!SunReadIconFile(dpy, drawable, filename,
                        &pixlabel->width, &pixlabel->height, &bitmap))
                     goto FAILURE;
                pixlabel->pixmap = XCreatePixmap(dpy, drawable,
                        pixlabel->width, pixlabel->height, depth);
                XCopyPlane(dpy, bitmap, pixlabel->pixmap,
                        scrInfo->gc[ROOT_GC], 0, 0,
                        pixlabel->width, pixlabel->height, 0, 0, 1);
                XFreePixmap(dpy, bitmap);
                rval = True;
                break;
#endif
#ifdef XPM
            case XPixmapFormat:
                xpmAttr.valuemask = XpmVisual | XpmColormap | XpmDepth;
                xpmAttr.visual = scrInfo->visual;
                xpmAttr.colormap = scrInfo->colormap;
                xpmAttr.depth = depth;
                if ((rval = XpmReadFileToPixmap(dpy, drawable, filename,
                            &(pixinfo->pixmap),
                            (Pixmap *)0, /* ignore shape mask for now */
                            &xpmAttr)) != PixmapSuccess) {
                        goto FAILURE;
                }
                pixinfo->width = xpmAttr.width;
                pixinfo->height = xpmAttr.height;
                break;
#endif
            case GifFormat:
                if (readGifFile(dpy, drawable, filename,
                        &pixinfo->width, &pixinfo->height, &bitmap,
			&xhot, &yhot, &scrInfo->colormap,
			&pixinfo->ncolors, &pixinfo->colors) != BitmapSuccess)
                    goto FAILURE;
                pixinfo->pixmap = XCreatePixmap(dpy, scrInfo->rootid,
                                        pixinfo->width, pixinfo->height,
                                        scrInfo->depth);
		gcv.function = GXcopy;
		gcv.plane_mask = AllPlanes;
		gc = XCreateGC(dpy, scrInfo->rootid,
				GCFunction | GCPlaneMask,
				&gcv);
                XCopyArea(dpy, bitmap, pixinfo->pixmap,
				gc, 0, 0,
                                pixinfo->width, pixinfo->height, 0, 0);
                XFreePixmap(dpy, bitmap);
		XFreeGC(dpy, gc);
                rval = True;
                break;

            default:
                goto FAILURE;
        }
        rval = True;
FAILURE:
        if (freeFilename != NULL)
            MemFree(newname);
        return rval;
}
