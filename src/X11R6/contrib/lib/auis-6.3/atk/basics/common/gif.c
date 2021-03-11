/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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

/* gif.c:
 *
 * adapted from code by kirk johnson (tuna@athena.mit.edu).  most of this
 * code is unchanged. -- jim frost 12.31.89
 *
 * gifin.c
 * kirk johnson
 * november 1989
 *
 * routines for reading GIF files
 *
 * Copyright 1989 Kirk L. Johnson (see the included file
 * "kljcpyrght.h" for complete copyright information)
 */
#ifndef _KLJ_COPYRIGHT_

/****
  Copyright 1989, 1990 Kirk L. Johnson

  Permission to use, copy, modify, distribute, and sell this
  software and its documentation for any purpose is hereby granted
  without fee, provided that the above copyright notice appear in
  all copies and that both that copyright notice and this
  permission notice appear in supporting documentation. The
  author makes no representations about the suitability of this
  software for any purpose. It is provided "as is" without express
  or implied warranty.

  THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
  OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
****/

#ifndef __SABER__
static char *KLJCopyright = "Copyright 1989, 1990 Kirk L. Johnson";
#endif
#define _KLJ_COPYRIGHT_
#endif

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/gif.c,v 1.7 1994/02/17 20:26:28 rr2b Exp $";
#endif


#include <andrewos.h>
#include <image.ih>
#include <gif.h>
#include <gif.eh>

/****
 **
 ** local #defines
 **
 ****/

#define PUSH_PIXEL(p)                                       \
{                                                           \
  if (pstk_idx == PSTK_SIZE)                                \
    gifin_fatal("pixel stack overflow in PUSH_PIXEL()");    \
  else                                                      \
    pstk[pstk_idx++] = (p);                                 \
}


/****
 **
 ** local variables
 **
 ****/

static int interlace_start[4]= { /* start line for interlacing */
  0, 4, 2, 1
};

static int interlace_rate[4]= { /* rate at which we accelerate vertically */
  8, 8, 4, 2
};

static BYTE file_open  = 0;     /* status flags */
static BYTE image_open = 0;

static FILE *ins;              /* input stream */

static int  root_size;          /* root code size */
static int  clr_code;           /* clear code */
static int  eoi_code;           /* end of information code */
static int  code_size;          /* current code size */
static int  code_mask;          /* current code mask */
static int  prev_code;          /* previous code */

/*
 * NOTE: a long is assumed to be at least 32 bits wide
 */
static long work_data;          /* working bit buffer */
static int  work_bits;          /* working bit count */

static BYTE buf[256];           /* byte buffer */
static int  buf_cnt;            /* byte count */
static int  buf_idx;            /* buffer index */

static int table_size;          /* string table size */
static int prefix[STAB_SIZE];   /* string table : prefixes */
static int extnsn[STAB_SIZE];   /* string table : extensions */

static BYTE pstk[PSTK_SIZE];    /* pixel stack */
static int  pstk_idx;           /* pixel stack pointer */


/****
 **
 ** global variables
 **
 ****/

static int  gifin_rast_width;          /* raster width */
static int  gifin_rast_height;         /* raster height */
static BYTE gifin_g_cmap_flag;         /* global colormap flag */
static int  gifin_g_pixel_bits;        /* bits per pixel, global colormap */
static int  gifin_g_ncolors;           /* number of colors, global colormap */
static BYTE gifin_g_cmap[3][256];      /* global colormap */
static int  gifin_bg_color;            /* background color index */
static int  gifin_color_bits;          /* bits of color resolution */

static int  gifin_img_left;            /* image position on raster */
static int  gifin_img_top;             /* image position on raster */
static int  gifin_img_width;           /* image width */
static int  gifin_img_height;          /* image height */
static BYTE gifin_l_cmap_flag;         /* local colormap flag */
static int  gifin_l_pixel_bits;        /* bits per pixel, local colormap */
static int  gifin_l_ncolors;           /* number of colors, local colormap */
static BYTE gifin_l_cmap[3][256];      /* local colormap */
static BYTE gifin_interlace_flag;      /* interlace image format flag */

extern int WriteGIF();


/*
 * open a GIF file, using s as the input stream
 */

static int 
gifin_open_file(s)
    FILE *s;
{
  /* make sure there isn't already a file open */
  if (file_open)
    return GIFIN_ERR_FAO;

  /* remember that we've got this file open */
  file_open = 1;
  ins       = s;

  /* check GIF signature */
  if (fread(buf, 1, GIF_SIG_LEN, ins) != GIF_SIG_LEN)
    return GIFIN_ERR_EOF;

  buf[GIF_SIG_LEN] = '\0';
  if ((strcmp((char *) buf, GIF_SIG) != 0) &&
      (strcmp((char *) buf, GIF_SIG_89) != 0))
    return GIFIN_ERR_BAD_SIG;

  /* read screen descriptor */
  if (fread(buf, 1, GIF_SD_SIZE, ins) != GIF_SD_SIZE)
    return GIFIN_ERR_EOF;

  /* decode screen descriptor */
  gifin_rast_width   = (buf[1] << 8) + buf[0];
  gifin_rast_height  = (buf[3] << 8) + buf[2];
  gifin_g_cmap_flag  = (buf[4] & 0x80) ? 1 : 0;
  gifin_color_bits   = ((buf[4] & 0x70) >> 4) + 1;
  gifin_g_pixel_bits = (buf[4] & 0x07) + 1;
  gifin_bg_color     = buf[5];

  if (buf[6] != 0)
    return GIFIN_ERR_BAD_SD;

  /* load global colormap */
  if (gifin_g_cmap_flag)
  {
    gifin_g_ncolors = (1 << gifin_g_pixel_bits);

    if (gifin_load_cmap(gifin_g_cmap, gifin_g_ncolors) != GIFIN_SUCCESS)
      return GIFIN_ERR_EOF;
  }
  else
  {
    gifin_g_ncolors = 0;
  }

  /* done! */
  return GIFIN_SUCCESS;
}


/*
 * open next GIF image in the input stream; returns GIFIN_SUCCESS if
 * successful. if there are no more images, returns GIFIN_DONE. (might
 * also return various GIFIN_ERR codes.)
 */

static int 
gifin_open_image()
{
  int i;
  int separator;

  /* make sure there's a file open */
  if (!file_open)
    return GIFIN_ERR_NFO;

  /* make sure there isn't already an image open */
  if (image_open)
    return GIFIN_ERR_IAO;

  /* remember that we've got this image open */
  image_open = 1;

  /* skip over any extension blocks */
  do
  {
    separator = fgetc(ins);
    if (separator == GIF_EXTENSION)
    {
      if (gifin_skip_extension() != GIFIN_SUCCESS)
        return GIFIN_ERR_EOF;
    }
  }
  while (separator == GIF_EXTENSION);

  /* check for end of file marker */
  if (separator == GIF_TERMINATOR)
    return GIFIN_DONE;

  /* make sure we've got an image separator */
  if (separator != GIF_SEPARATOR)
    return GIFIN_ERR_BAD_SEP;

  /* read image descriptor */
  if (fread(buf, 1, GIF_ID_SIZE, ins) != GIF_ID_SIZE)
    return GIFIN_ERR_EOF;

  /* decode image descriptor */
  gifin_img_left       = (buf[1] << 8) + buf[0];
  gifin_img_top        = (buf[3] << 8) + buf[2];
  gifin_img_width      = (buf[5] << 8) + buf[4];
  gifin_img_height     = (buf[7] << 8) + buf[6];
  gifin_l_cmap_flag    = (buf[8] & 0x80) ? 1 : 0;
  gifin_interlace_flag = (buf[8] & 0x40) ? 1 : 0;
  gifin_l_pixel_bits   = (buf[8] & 0x07) + 1;

  /* load local colormap */
  if (gifin_l_cmap_flag)
  {
    gifin_l_ncolors = (1 << gifin_l_pixel_bits);

    if (gifin_load_cmap(gifin_l_cmap, gifin_l_ncolors) != GIFIN_SUCCESS)
      return GIFIN_ERR_EOF;
  }
  else
  {
    gifin_l_ncolors = 0;
  }

  /* initialize raster data stream decoder */
  root_size = fgetc(ins);
  clr_code  = 1 << root_size;
  eoi_code  = clr_code + 1;
  code_size = root_size + 1;
  code_mask = (1 << code_size) - 1;
  work_bits = 0;
  work_data = 0;
  buf_cnt   = 0;
  buf_idx   = 0;

  /* initialize string table */
  for (i=0; i<STAB_SIZE; i++)
  {
    prefix[i] = NULL_CODE;
    extnsn[i] = i;
  }

  /* initialize pixel stack */
  pstk_idx = 0;

  /* done! */
  return GIFIN_SUCCESS;
}

/*
 * try to read next pixel from the raster, return result in *pel
 */

static int gifin_get_pixel(pel)
     int *pel;
{
  int  code;
  int  first;
  int  place;

  /* decode until there are some pixels on the pixel stack */
  while (pstk_idx == 0)
  {
    /* load bytes until we have enough bits for another code */
    while (work_bits < code_size)
    {
      if (buf_idx == buf_cnt)
      {
        /* read a new data block */
        if (gifin_read_data_block() != GIFIN_SUCCESS)
          return GIFIN_ERR_EOF;

        if (buf_cnt == 0)
          return GIFIN_ERR_EOD;
      }

      work_data |= ((long) buf[buf_idx++]) << work_bits;
      work_bits += 8;
    }

    /* get the next code */
    code        = work_data & code_mask;
    work_data >>= code_size;
    work_bits  -= code_size;

    /* interpret the code */
    if (code == clr_code)
    {
      /* reset decoder stream */
      code_size  = root_size + 1;
      code_mask  = (1 << code_size) - 1;
      prev_code  = NULL_CODE;
      table_size = eoi_code + 1;
    }
    else if (code == eoi_code)
    {
      /* Ooops! no more pixels */
      return GIFIN_ERR_EOF;
    }
    else if (prev_code == NULL_CODE)
    {
      gifin_push_string(code);
      prev_code = code;
    }
    else
    {
      if (code < table_size)
      {
        first = gifin_push_string(code);
      }
      else
      {
        place = pstk_idx;
        PUSH_PIXEL(NULL_CODE);
        first = gifin_push_string(prev_code);
        pstk[place] = first;
      }

      gifin_add_string(prev_code, first);
      prev_code = code;
    }
  }

  /* pop a pixel off the pixel stack */
  *pel = (int) pstk[--pstk_idx];

  /* done! */
  return GIFIN_SUCCESS;
}

/*
 * close an open GIF file
 */

static int gifin_close_file()
{
  /* make sure there's a file open */
  if (!file_open)
    return GIFIN_ERR_NFO;

  /* mark file (and image) as closed */
  file_open  = 0;
  image_open = 0;

  /* done! */
  return GIFIN_SUCCESS;
}

/*
 * load a colormap from the input stream
 */

static int gifin_load_cmap(cmap, ncolors)
     BYTE cmap[3][256];
     int  ncolors;
{
  int i;

  for (i=0; i<ncolors; i++)
  {
    if (fread(buf, 1, 3, ins) != 3)
      return GIFIN_ERR_EOF;
    
    cmap[GIF_RED][i] = buf[GIF_RED];
    cmap[GIF_GRN][i] = buf[GIF_GRN];
    cmap[GIF_BLU][i] = buf[GIF_BLU];
  }

  /* done! */
  return GIFIN_SUCCESS;
}
 
/*
 * skip an extension block in the input stream
 */

static int gifin_skip_extension()
{

  /* get the extension function byte */
  fgetc(ins);

  /* skip any remaining raster data */
  do
  {
    if (gifin_read_data_block() != GIFIN_SUCCESS)
      return GIFIN_ERR_EOF;
  }
  while (buf_cnt > 0);

  /* done! */
  return GIFIN_SUCCESS;
}

/*
 * read a new data block from the input stream
 */

static int gifin_read_data_block()
{
  /* read the data block header */
  buf_cnt = fgetc(ins);

  /* read the data block body */
  if (fread(buf, 1, buf_cnt, ins) != buf_cnt)
    return GIFIN_ERR_EOF;

  buf_idx = 0;

  /* done! */
  return GIFIN_SUCCESS;
}

/*
 * push a string (denoted by a code) onto the pixel stack
 * (returns the code of the first pixel in the string)
 */

static int gifin_push_string(code)
     int code;
{
  int rslt;

  while (prefix[code] != NULL_CODE)
  {
    PUSH_PIXEL(extnsn[code]);
    code = prefix[code];
  }

  PUSH_PIXEL(extnsn[code]);
  rslt = extnsn[code];

  return rslt;
}

/*
 * add a new string to the string table
 */

static gifin_add_string(p, e)
     int p;
     int e;
{
  prefix[table_size] = p;
  extnsn[table_size] = e;

  if ((table_size == code_mask) && (code_size < 12))
  {
    code_size += 1;
    code_mask  = (1 << code_size) - 1;
  }

  table_size += 1;
}

/*
 * semi-graceful fatal error mechanism
 */

static gifin_fatal(msg)
     char *msg;
{
  printf("Error reading GIF file: %s\n", msg);
  exit(0);
}

static void 
tellAboutImage( name )
    char *name;
{
  printf("%s is a %dx%d %sGIF image with %d colors\n", name,
	 gifin_img_width, gifin_img_height,
	 (gifin_interlace_flag ? "interlaced " : ""),
	 (gifin_l_cmap_flag ? gifin_l_ncolors : gifin_g_ncolors));
}

int
gif__Load( gif, fullname, fp )
    struct gif *gif;
    char *fullname;
    FILE *fp;
{ 
  FILE *f;
  int x, y, pixel, pass, scanlen;
  byte *pixptr, *pixline;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open gif file %s.\n", fullname);
	  return(-1);
      }
  }
  if ((gifin_open_file(f) != GIFIN_SUCCESS) || /* read GIF header */
      (gifin_open_image() != GIFIN_SUCCESS)) {  /* read image header */
    gifin_close_file();
    fclose(f);
    fprintf(stderr, "Couldn't read GIF image.\n");
    return(-1);
  }
  gif_newRGBImage(gif, gifin_img_width, gifin_img_height, (gifin_l_cmap_flag ?
							 gifin_l_pixel_bits :
							 gifin_g_pixel_bits));
  for (x = 0; x < gifin_g_ncolors; x++) {
    gif_RedPixel(gif, x) = gifin_g_cmap[GIF_RED][x] << 8;
    gif_GreenPixel(gif, x) = gifin_g_cmap[GIF_GRN][x] << 8;
    gif_BluePixel(gif, x) = gifin_g_cmap[GIF_BLU][x] << 8;
  }
  gif_RGBUsed(gif) = gifin_g_ncolors;

  /* if image has a local colormap, override global colormap
   */

  if (gifin_l_cmap_flag) {
    for (x = 0; x < gif_RGBSize(gif); x++) {
      gif_RedPixel(gif, x) = gifin_g_cmap[GIF_RED][x] << 8;
      gif_GreenPixel(gif, x) = gifin_g_cmap[GIF_GRN][x] << 8;
      gif_BluePixel(gif, x) = gifin_g_cmap[GIF_BLU][x] << 8;
    }
    gif_RGBUsed(gif) = gifin_l_ncolors;
  }

  /* interlaced image -- futz with the vertical trace.  i wish i knew what
   * kind of drugs the GIF people were on when they decided that they
   * needed to support interlacing.
   */

  if (gifin_interlace_flag) {
    scanlen = gif_Height(gif) * gif_Pixlen(gif);

    /* interlacing takes four passes to read, each starting at a different
     * vertical point.
     */

    for (pass = 0; pass < 4; pass++) {
      y = interlace_start[pass];
      scanlen = gif_Width(gif) * gif_Pixlen(gif) * interlace_rate[pass];
      pixline = gif_Data(gif) + (y * gif_Width(gif) * gif_Pixlen(gif));
      while (y < gifin_img_height) {
	pixptr = pixline;
	for (x = 0; x < gifin_img_width; x++) {
	  if (gifin_get_pixel(&pixel) != GIFIN_SUCCESS) {
	    printf("%s: Short read within image data\n", fullname);
	    y = gifin_img_height; x = gifin_img_width;
	  }
	  valToMem(pixel, pixptr, gif_Pixlen(gif));
	  pixptr += gif_Pixlen(gif);
	}
	y += interlace_rate[pass];
	pixline += scanlen;
      }
    }
  }

  /* not an interlaced image, just read in sequentially
   */

  else {
    pixptr = gif_Data(gif);
    for (y = 0; y < gifin_img_height; y++)
      for (x = 0; x < gifin_img_width; x++) {
	if (gifin_get_pixel(&pixel) != GIFIN_SUCCESS) {
	  printf("%s: Short read within image data\n", fullname);
	  y = gifin_img_height; x = gifin_img_width;
	}
	valToMem(pixel, pixptr, gif_Pixlen(gif));
	pixptr += gif_Pixlen(gif);
      }
  }
  gifin_close_file();
  fclose(f);
  return(0);
}

int 
gif__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ 
    FILE *f;
    unsigned int ret;

  if (!(f = fopen(fullname, "r")))
      return(0);
  if ((gifin_open_file(f) == GIFIN_SUCCESS) &&
      (gifin_open_image() == GIFIN_SUCCESS)) {
      tellAboutImage(fullname);
      ret= 1;
  }
  else
      ret= 0;
  gifin_close_file();
  fclose(f);
  return(ret);
}

long
gif__Read( self, file, id )
    struct gif *self;
    FILE *file;
    long id;
{
	if(gif_Load(self, NULL, file) == 0) {
	    gif_Compress(self);
	    return(dataobject_NOREADERROR);
	}
    else
	return(dataobject_BADFORMAT);
}

long
gif__Write( self, file, writeID, level )
    struct gif *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
gif__WriteNative( self, file, filename )
    struct gif *self;
    FILE *file;
    char *filename;
{
    int colorstyle = 0; /* 0 == color; 1 == greyscale; 2 == b/w */
    byte r[256], g[256], b[256];
    register int i;

    switch(gif_Type(self)) {
	case IBITMAP:
	    gif_Bit2Grey(self);
	    break;
	case ITRUE:
	    gif_Reduce(self, 256);
	    break;
    }

    for(i = 0; i < gif_RGBUsed(self); i++) {
	*(r + i) = gif_RedPixel(self, i) >> 8;
	*(g + i) = gif_GreenPixel(self, i) >> 8;
	*(b + i) = gif_BluePixel(self, i) >> 8;
    }

    if(file) {
	WriteGIF(file, gif_Data(self), gif_Width(self), gif_Height(self), r, g, b, gif_RGBUsed(self), colorstyle);
    }
    else {
	FILE *f;
	if(filename) {
	    if(f = fopen(filename, "w")) {
		WriteGIF(f, gif_Data(self), gif_Width(self), gif_Height(self), r, g, b, gif_RGBSize(self), colorstyle);
		fclose(f);
	    }
	    else {
		/* no file pointer was passed and filename couldn't be opened */
		printf("Couldn't open file %s\n", filename);
		return(-1);
	    }
	}
	else {
	    /* no file pointer or filename was passed */
	    return(-1);
	}
    }
    return(0);
}
