/* xwd.h:
 *
 * portable-ized xwd file information.  they could have made this easier.
 *
 * jim frost 07.24.90
 *
 * Copyright 1989 Jim Frost.  See included file "copyright.h" for complete
 * copyright information.
 */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


#define XWD_VERSION 7 /* XWD version we support */

typedef struct {
  byte header_size[4];      /* Size of the entire file header (bytes). */
  byte file_version[4];	    /* XWD_FILE_VERSION */
  byte pixmap_format[4];    /* Pixmap format */
  byte pixmap_depth[4];	    /* Pixmap depth */
  byte pixmap_width[4];     /* Pixmap width */
  byte pixmap_height[4];    /* Pixmap height */
  byte xoffset[4];          /* Bitmap x offset */
  byte byte_order[4];       /* MSBFirst, LSBFirst */
  byte bitmap_unit[4];      /* Bitmap unit */
  byte bitmap_bit_order[4]; /* MSBFirst, LSBFirst */
  byte bitmap_pad[4];       /* Bitmap scanline pad */
  byte bits_per_pixel[4];   /* Bits per pixel */
  byte bytes_per_line[4];   /* Bytes per scanline */
  byte visual_class[4];     /* Class of colormap */
  byte red_mask[4];         /* Z red mask */
  byte green_mask[4];       /* Z green mask */
  byte blue_mask[4];        /* Z blue mask */
  byte bits_per_rgb[4];     /* Log2 of distinct color values */
  byte colormap_entries[4]; /* Number of entries in colormap */
  byte ncolors[4];          /* Number of Color structures */
  byte window_width[4];     /* Window width */
  byte window_height[4];    /* Window height */
  byte window_x[4];         /* Window upper left X coordinate */
  byte window_y[4];         /* Window upper left Y coordinate */
  byte window_bdrwidth[4];  /* Window border width */
} GenericXWDHeader;

typedef struct {
  unsigned int header_size;      /* Size of the entire file header (bytes). */
  unsigned int file_version;     /* XWD_FILE_VERSION */
  unsigned int pixmap_format;    /* Pixmap format */
  unsigned int pixmap_depth;     /* Pixmap depth */
  unsigned int pixmap_width;     /* Pixmap width */
  unsigned int pixmap_height;    /* Pixmap height */
  unsigned int xoffset;          /* Bitmap x offset */
  unsigned int byte_order;       /* MSBFirst, LSBFirst */
  unsigned int bitmap_unit;      /* Bitmap unit */
  unsigned int bitmap_bit_order; /* MSBFirst, LSBFirst */
  unsigned int bitmap_pad;       /* Bitmap scanline pad */
  unsigned int bits_per_pixel;   /* Bits per pixel */
  unsigned int bytes_per_line;   /* Bytes per scanline */
  unsigned int visual_class;     /* Class of colormap */
/*unsigned int red_mask;*/       /* Z red mask */
/*unsigned int green_mask;*/     /* Z green mask */
/*unsigned int blue_mask;*/      /* Z blue mask */
/*unsigned int bits_per_rgb;*/   /* Log2 of distinct color values */
  unsigned int colormap_entries; /* Number of entries in colormap */
  unsigned int ncolors;          /* Number of Color structures */
/*unsigned int window_width;*/   /* Window width */
/*unsigned int window_height;*/  /* Window height */
/*unsigned int window_x;*/       /* Window upper left X coordinate */
/*unsigned int window_y;*/       /* Window upper left Y coordinate */
/*unsigned int window_bdrwidth;*//* Window border width */
} XWDHeader;

typedef struct {
  byte pixel[4]; /* pixel value for this color */
  byte red[2];   /* red intensity */
  byte green[2]; /* green intensity */
  byte blue[2];  /* blue intensity */
  byte flags;    /* XColor flags (ignored) */
  byte pad;
} XWDColor;

#define NOT_XWD 0 /* definitely not an XWD file */
#define XWD_MSB 1 /* looks like XWD file created on MSBFirst machine */
#define XWD_LSB 2 /* looks like XWD file created on LSBFirst machine */
