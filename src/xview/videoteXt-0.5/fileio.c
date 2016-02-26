/*
 * fileio.c: Routines to read/write VTX-files and write ASCII, GIF and PPM-files
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 * The GIF-routines were taken from Jef Poskanzer's PBMplus-package, so the
 * following copyright applies to parts of this file:
 */

/* ppmtogif.c - read a portable pixmap and produce a GIF file
**
** Based on GIFENCOD by David Rowley <mgardi@watdscu.waterloo.edu>.A
** Lempel-Zim compression based on "compress".
**
** Modified by Marcel Wijkstra <wijkstra@fwi.uva.nl>
**
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** The Graphics Interchange Format(c) is the Copyright property of
** CompuServe Incorporated.  GIF(sm) is a Service Mark property of
** CompuServe Incorporated.
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <linux/vtx.h>
#include "cct.h"
#include "vtxdecode.h"
#include "vtxqueue.h"
#include "vtxtools.h"
#include "dialog.h"
#include "hotlist.h"
#include "xinit.h"
#include "xevents.h"
#include "fileio.h"


static XImage *vtx_image;
static const int vtx_img_red[8] =   {   0, 255,   0, 255,   0, 255,   0, 255 };
static const int vtx_img_green[8] = {   0,   0, 255, 255,   0,   0, 255, 255 };
static const int vtx_img_blue[8] =  {   0,   0,   0,   0, 255, 255, 255, 255 };

typedef int (*ifunptr) (int, int);	/* Pointer to function returning an int */


static int GIFEncode (FILE* fp, int GWidth, int GHeight, int GInterlace, int Background,
    int Transparent, int BitsPerPixel, int* Red, int* Green, int* Blue, ifunptr GetPixel);
static void PPMEncode(FILE *file, int width, int height, int *red, int *green, int *blue,
    ifunptr GetPixel);
static XImage* create_vtx_image(void);
static int vtx_getpixel(int x, int y);



/* Load vtx- or INtv-file & decode it. fc and fname are unused
 */
int
load_vtx(FILE *file) {
  byte_t tmp_buffer[VTX_PAGESIZE];
  vtx_pageinfo_t tmp_inf;
  int pos, tmpbits;
  unsigned char tmpstr[256];
  struct stat st;

  if (fscanf(file, "VTXV%c", tmpstr) != 1) {
    if (fstat(fileno(file), &st) < 0) {
      return -1;
    /* The stupid INtv format doesn't use a header, so we have to use the file-length instead */
    } else if (st.st_size != 1008) {
      confirm_notice("Error: Magic number missing.\nThis is no VideoteXt- or INtv-file.");
      return 1;
    }
    memset(&tmp_inf, 0, sizeof(tmp_inf));			/* Read ITV-file */
    rewind(file);
    for (pos = 0; pos <= 23; pos++) {
      fseek(file, 2, SEEK_CUR);
      fread(tmp_buffer + pos * 40, 40, 1, file);
    }
    /* The first 8 bytes in the INtv-format usually contain garbage (or data I don't understand) */
    memset(tmp_buffer, vtx_mkparity(' '), 8 * sizeof(byte_t));
    for (pos = 0; pos <= 2; pos++) {
      tmpstr[pos] = tmp_buffer[8 + pos];
      vtx_chkparity(&tmpstr[pos]);
    }
    tmpstr[3] = '\0';
    sscanf(tmpstr, "%3x", &tmp_inf.pagenum);
    if (!vtx_chkpgnum(tmp_inf.pagenum, TRUE)) {
      tmp_inf.pagenum = 0;
    }
  } else {
    if (tmpstr[0] != '2') {
      confirm_notice("Error: Incompatible file format,\npossibly written by old version.");
      return 1;
    }
    tmp_inf.pagenum = fgetc(file) + 0x100 * fgetc(file);	/* Read VTX-file */
    tmp_inf.hour = fgetc(file);
    tmp_inf.minute = fgetc(file);
    tmp_inf.charset = fgetc(file);
    tmpbits = fgetc(file);
    tmp_inf.delete = !!(tmpbits & 0x80);
    tmp_inf.headline = !!(tmpbits & 0x40);
    tmp_inf.subtitle = !!(tmpbits & 0x20);
    tmp_inf.supp_header = !!(tmpbits & 0x10);
    tmp_inf.update = !!(tmpbits & 8);
    tmp_inf.inter_seq = !!(tmpbits & 4);
    tmp_inf.dis_disp = !!(tmpbits & 2);
    tmp_inf.serial = (tmpbits & 1);
    tmpbits = fgetc(file);
    tmp_inf.notfound = !!(tmpbits & 0x80);
    tmp_inf.pblf = !!(tmpbits & 0x40);
    tmp_inf.hamming = !!(tmpbits & 0x20);
    fread(tmp_buffer, VTX_PAGESIZE, 1, file);
  }
  if (feof(file) || ferror(file)) {
    return -1;
  }

  vtxwin.page->info = tmp_inf;
  vtx_buffer[0] = vtx_mkparity(0);
  vtx_buffer[7] = vtx_mkparity(7);
  memcpy(vtx_buffer + 8, tmp_buffer + 8, VTX_PAGESIZE - 8);
  decode_page(vtx_buffer, vtxwin.page, 0, 23);
  xv_set(checkbox, PANEL_VALUE, (int)xv_get(checkbox, PANEL_VALUE) | 1, NULL);
  vtxwin.stopped = 2;
  vtxwin.update = SCR_UPDATE;
  update_pagenumdisp(vtxwin.page->info.pagenum, vtxwin.page->info.minute, 1, 1);
  return 0;
}


void
save_vtx(FILE *file) {
  int pos;
  vtx_pageinfo_t *inf = &vtxwin.page->info;

  fputs("VTXV2", file);
  fputc(inf->pagenum & 0xff, file);
  fputc(inf->pagenum / 0x100, file);
  fputc(inf->hour & 0xff, file);
  fputc(inf->minute & 0xff, file);
  fputc(inf->charset & 0xff, file);
  fputc(inf->delete << 7 | inf->headline << 6 | inf->subtitle << 5 | inf->supp_header << 4 |
      inf->update << 3 | inf->inter_seq << 2 | inf->dis_disp << 1 | inf->serial << 0, file);
  fputc(inf->notfound << 7 | inf->pblf << 6 | inf->hamming << 5, file);
  for (pos = 0; pos < VTX_PAGESIZE; pos++) {
    fputc(vtx_buffer[pos], file);
  }
}


void
export_ascii(FILE *file) {
  int pos;

  for (pos = 0; pos < VTX_PAGESIZE; pos++) {
    if ((vtxwin.page->attrib[pos] & VTX_HIDDEN) && vtxwin.hidden) {
      fputc(' ', file);
    } else {
      fputc(vtx2iso_table[vtxwin.page->chr[pos]], file);
    }
    if (pos % 40 == 39)
      fputc('\n', file);
  }
}


void
export_gif(FILE *file) {
  vtx_image = create_vtx_image();
  GIFEncode(file, vtxfonts[vtxwin.font].width * 40, vtxfonts[vtxwin.font].height * 24, 0, 0,
      -1, 3, (int*)vtx_img_red, (int*)vtx_img_green, (int*)vtx_img_blue, vtx_getpixel);
  XDestroyImage(vtx_image);
}


void
export_ppm(FILE *file) {
  
  vtx_image = create_vtx_image();
  PPMEncode(file, vtxfonts[vtxwin.font].width * 40, vtxfonts[vtxwin.font].height * 24,
      (int*)vtx_img_red, (int*)vtx_img_green, (int*)vtx_img_blue, vtx_getpixel);
  XDestroyImage(vtx_image);
}


static XImage*
create_vtx_image(void) {
  vtxpage_t tmp_page;
  vtxpgwin_t tmp_vtxwin;
  XImage *image;
  int width, height;

  width = vtxfonts[vtxwin.font].width * 40;
  height = vtxfonts[vtxwin.font].height * 24;
  tmp_vtxwin = vtxwin;
  tmp_vtxwin.curr_disp = &tmp_page;
  tmp_vtxwin.winid = XCreatePixmap(dpy, rootid, width, height, 1);
  tmp_vtxwin.flash_state = 0;
  tmp_vtxwin.draw_bm = 1;
  tmp_vtxwin.gc = XCreateGC(dpy, tmp_vtxwin.winid, 0, NULL);
  x_vtx_redraw(&tmp_vtxwin, 0, 0, 39, 23, TRUE);
  XFreeGC(dpy, tmp_vtxwin.gc);
  image = XGetImage(dpy, tmp_vtxwin.winid, 0, 0, width, height, 1, XYPixmap);
  XFreePixmap(dpy, tmp_vtxwin.winid);
  return image;
}


static int
vtx_getpixel(int x, int y) {
  if (XGetPixel(vtx_image, x, y)) {
    return vtxwin.page->attrib[(y / vtxfonts[vtxwin.font].height) * 40 +
        x / vtxfonts[vtxwin.font].width] & VTX_COLMASK;
  } else {
    return (vtxwin.page->attrib[(y / vtxfonts[vtxwin.font].height) * 40 +
        x / vtxfonts[vtxwin.font].width] >> 3) & VTX_COLMASK;
  }
}


static void
PPMEncode(FILE *file, int width, int height, int *red, int *green, int *blue, ifunptr GetPixel) {
  int row, col, pixel;

  fprintf(file, "P6\n%d %d\n255\n", width, height);
  for (row = 0; row < height; row++) {
    for (col = 0; col < width; col++) {
      pixel = (*GetPixel)(col, row);
      fputc(red[pixel], file);
      fputc(green[pixel], file);
      fputc(blue[pixel], file);
    }
  }
}



/*****************************************************************************
 *
 * GIFENCODE.C    - GIF Image compression interface
 *
 * GIFEncode( FName, GHeight, GWidth, GInterlace, Background, Transparent,
 *            BitsPerPixel, Red, Green, Blue, GetPixel )
 *
 *****************************************************************************/

/*
 * a code_int must be able to hold 2**BITS values of type int, and also -1
 */
typedef int             code_int;

typedef long int          count_int;

#define TRUE 1
#define FALSE 0

static void BumpPixel ( void );
static int GIFNextPixel ( ifunptr getpixel );
static void Putword ( int w, FILE* fp );
static int compress ( int init_bits, FILE* outfile, ifunptr ReadValue );
static int output ( code_int code );
static int cl_block ( void );
static void cl_hash ( count_int hsize );
static void char_init ( void );
static void char_out ( int c );
static void flush_char ( void );

static int Width, Height;
static int curx, cury;
static long CountDown;
static int Pass = 0;
static int Interlace;

/*
 * Bump the 'curx' and 'cury' to point to the next pixel
 */
static void
BumpPixel()
{
        /*
         * Bump the current X position
         */
        ++curx;

        /*
         * If we are at the end of a scan line, set curx back to the beginning
         * If we are interlaced, bump the cury to the appropriate spot,
         * otherwise, just increment it.
         */
        if( curx == Width ) {
                curx = 0;

                if( !Interlace )
                        ++cury;
                else {
                     switch( Pass ) {

                       case 0:
                          cury += 8;
                          if( cury >= Height ) {
                                ++Pass;
                                cury = 4;
                          }
                          break;

                       case 1:
                          cury += 8;
                          if( cury >= Height ) {
                                ++Pass;
                                cury = 2;
                          }
                          break;

                       case 2:
                          cury += 4;
                          if( cury >= Height ) {
                             ++Pass;
                             cury = 1;
                          }
                          break;

                       case 3:
                          cury += 2;
                          break;
                        }
                }
        }
}

/*
 * Return the next pixel from the image
 */
static int
GIFNextPixel( getpixel )
ifunptr getpixel;
{
        int r;

        if( CountDown == 0 )
                return EOF;

        --CountDown;

        r = ( * getpixel )( curx, cury );

        BumpPixel();

        return r;
}

/* public */

static int
GIFEncode( fp, GWidth, GHeight, GInterlace, Background, Transparent,
           BitsPerPixel, Red, Green, Blue, GetPixel )

FILE* fp;
int GWidth, GHeight;
int GInterlace;
int Background;
int Transparent;
int BitsPerPixel;
int Red[], Green[], Blue[];
ifunptr GetPixel;
{
        int B;
        int RWidth, RHeight;
        int LeftOfs, TopOfs;
        int Resolution;
        int ColorMapSize;
        int InitCodeSize;
        int i;

        Interlace = GInterlace;

        ColorMapSize = 1 << BitsPerPixel;

        RWidth = Width = GWidth;
        RHeight = Height = GHeight;
        LeftOfs = TopOfs = 0;

        Resolution = BitsPerPixel;

        /*
         * Calculate number of bits we are expecting
         */
        CountDown = (long)Width * (long)Height;

        /*
         * Indicate which pass we are on (if interlace)
         */
        Pass = 0;

        /*
         * The initial code size
         */
        if( BitsPerPixel <= 1 )
                InitCodeSize = 2;
        else
                InitCodeSize = BitsPerPixel;

        /*
         * Set up the current x and y position
         */
        curx = cury = 0;

        /*
         * Write the Magic header
         */
        fwrite( Transparent < 0 ? "GIF87a" : "GIF89a", 1, 6, fp );

        /*
         * Write out the screen width and height
         */
        Putword( RWidth, fp );
        Putword( RHeight, fp );

        /*
         * Indicate that there is a global colour map
         */
        B = 0x80;       /* Yes, there is a color map */

        /*
         * OR in the resolution
         */
        B |= (Resolution - 1) << 5;

        /*
         * OR in the Bits per Pixel
         */
        B |= (BitsPerPixel - 1);

        /*
         * Write it out
         */
        fputc( B, fp );

        /*
         * Write out the Background colour
         */
        fputc( Background, fp );

        /*
         * Byte of 0's (future expansion)
         */
        fputc( 0, fp );

        /*
         * Write out the Global Colour Map
         */
        for( i=0; i<ColorMapSize; ++i ) {
                fputc( Red[i], fp );
                fputc( Green[i], fp );
                fputc( Blue[i], fp );
        }

	/*
	 * Write out extension for transparent colour index, if necessary.
	 */
	if ( Transparent >= 0 ) {
	    fputc( '!', fp );
	    fputc( 0xf9, fp );
	    fputc( 4, fp );
	    fputc( 1, fp );
	    fputc( 0, fp );
	    fputc( 0, fp );
	    fputc( Transparent, fp );
	    fputc( 0, fp );
	}

        /*
         * Write an Image separator
         */
        fputc( ',', fp );

        /*
         * Write the Image header
         */

        Putword( LeftOfs, fp );
        Putword( TopOfs, fp );
        Putword( Width, fp );
        Putword( Height, fp );

        /*
         * Write out whether or not the image is interlaced
         */
        if( Interlace )
                fputc( 0x40, fp );
        else
                fputc( 0x00, fp );

        /*
         * Write out the initial code size
         */
        fputc( InitCodeSize, fp );

        /*
         * Go and actually compress the data
         */
        if (compress( InitCodeSize+1, fp, GetPixel ) < 0) {
#if 0
          fclose( fp );
#endif
          return -1;
        }

        /*
         * Write out a Zero-length packet (to end the series)
         */
        fputc( 0, fp );

        /*
         * Write the GIF file terminator
         */
        fputc( ';', fp );

        /*
         * And close the file
         */
#if 0
        fclose( fp );
#endif
        return 0;
}

/*
 * Write out a word to the GIF file
 */
static void
Putword( w, fp )
int w;
FILE* fp;
{
        fputc( w & 0xff, fp );
        fputc( (w / 256) & 0xff, fp );
}


/***************************************************************************
 *
 *  GIFCOMPR.C       - GIF Image compression routines
 *
 *  Lempel-Ziv compression based on 'compress'.  GIF modifications by
 *  David Rowley (mgardi@watdcsu.waterloo.edu)
 *
 ***************************************************************************/

/*
 * General DEFINEs
 */

#define BITS    12

#define HSIZE  5003            /* 80% occupancy */

typedef        unsigned char   char_type;

/*
 *
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:  Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *              Jim McKie               (decvax!mcvax!jim)
 *              Steve Davies            (decvax!vax135!petsd!peora!srd)
 *              Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *              James A. Woods          (decvax!ihnp4!ames!jaw)
 *              Joe Orost               (decvax!vax135!petsd!joe)
 *
 */
#include <ctype.h>

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

static int n_bits;                        /* number of bits/code */
static int maxbits = BITS;                /* user settable max # bits/code */
static code_int maxcode;                  /* maximum code, given n_bits */
static code_int maxmaxcode = (code_int)1 << BITS; /* should NEVER generate this code */
#define MAXCODE(n_bits)        (((code_int) 1 << (n_bits)) - 1)

static count_int htab [HSIZE];
static unsigned short codetab [HSIZE];
#define HashTabOf(i)       htab[i]
#define CodeTabOf(i)    codetab[i]

static code_int hsize = HSIZE;                 /* for dynamic table sizing */

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**BITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i) CodeTabOf(i)
#define tab_suffixof(i)        ((char_type*)(htab))[i]
#define de_stack               ((char_type*)&tab_suffixof((code_int)1<<BITS))

static code_int free_ent = 0;                  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

static int offset;
static long int in_count = 1;            /* length of input */
static long int out_count = 0;           /* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE* g_outfile;

static int ClearCode;
static int EOFCode;

static int
compress( init_bits, outfile, ReadValue )
int init_bits;
FILE* outfile;
ifunptr ReadValue;
{
    register long fcode;
    register code_int i /* = 0 */;
    register int c;
    register code_int ent;
    register code_int disp;
    register code_int hsize_reg;
    register int hshift;

    /*
     * Set up the globals:  g_init_bits - initial number of bits
     *                      g_outfile   - pointer to output file
     */
    g_init_bits = init_bits;
    g_outfile = outfile;

    /*
     * Set up the necessary values
     */
    offset = 0;
    out_count = 0;
    clear_flg = 0;
    in_count = 1;
    maxcode = MAXCODE(n_bits = g_init_bits);

    ClearCode = (1 << (init_bits - 1));
    EOFCode = ClearCode + 1;
    free_ent = ClearCode + 2;

    char_init();

    ent = GIFNextPixel( ReadValue );

    hshift = 0;
    for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L )
        ++hshift;
    hshift = 8 - hshift;                /* set hash code range bound */

    hsize_reg = hsize;
    cl_hash( (count_int) hsize_reg);            /* clear hash table */

    if (output( (code_int)ClearCode ) < 0)
      return -1;

    while ( (c = GIFNextPixel( ReadValue )) != EOF ) {  /* } */

        ++in_count;

        fcode = (long) (((long) c << maxbits) + ent);
        i = (((code_int)c << hshift) ^ ent);    /* xor hashing */

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        } else if ( (long)HashTabOf (i) < 0 )      /* empty slot */
            goto nomatch;
        disp = hsize_reg - i;           /* secondary hash (after G. Knott) */
        if ( i == 0 )
            disp = 1;
probe:
        if ( (i -= disp) < 0 )
            i += hsize_reg;

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        }
        if ( (long)HashTabOf (i) > 0 )
            goto probe;
nomatch:
        if (output ( (code_int) ent ) < 0)
          return -1;
        ++out_count;
        ent = c;
        if ( free_ent < maxmaxcode ) {  /* } */
            CodeTabOf (i) = free_ent++; /* code -> hashtable */
            HashTabOf (i) = fcode;
        } else {
                if (cl_block() < 0)
                  return -1;
        }
    }
    /*
     * Put out the final code.
     */
    if (output( (code_int)ent ) < 0)
      return -1;
    ++out_count;
    if (output( (code_int) EOFCode ) < 0)
      return -1;
    return 0;
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static unsigned long cur_accum = 0;
static int cur_bits = 0;

static unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

static int
output( code )
code_int  code;
{
    cur_accum &= masks[ cur_bits ];

    if( cur_bits > 0 )
        cur_accum |= ((long)code << cur_bits);
    else
        cur_accum = code;

    cur_bits += n_bits;

    while( cur_bits >= 8 ) {
        char_out( (unsigned int)(cur_accum & 0xff) );
        cur_accum >>= 8;
        cur_bits -= 8;
    }

    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */
   if ( free_ent > maxcode || clear_flg ) {

            if( clear_flg ) {

                maxcode = MAXCODE (n_bits = g_init_bits);
                clear_flg = 0;

            } else {

                ++n_bits;
                if ( n_bits == maxbits )
                    maxcode = maxmaxcode;
                else
                    maxcode = MAXCODE(n_bits);
            }
        }

    if( code == EOFCode ) {
        /*
         * At EOF, write the rest of the buffer.
         */
        while( cur_bits > 0 ) {
                char_out( (unsigned int)(cur_accum & 0xff) );
                cur_accum >>= 8;
                cur_bits -= 8;
        }

        flush_char();

        fflush( g_outfile );

        if( ferror( g_outfile ) )
                return -1;
    }
    return 0;
}

/*
 * Clear out the hash table
 */
static int
cl_block ()             /* table clear for block compress */
{

        cl_hash ( (count_int) hsize );
        free_ent = ClearCode + 2;
        clear_flg = 1;

        if (output( (code_int)ClearCode ) < 0)
          return -1;
        return 0;
}

static void
cl_hash(hsize)          /* reset code table */
register count_int hsize;
{

        register count_int *htab_p = htab+hsize;

        register long i;
        register long m1 = -1;

        i = hsize - 16;
        do {                            /* might use Sys V memset(3) here */
                *(htab_p-16) = m1;
                *(htab_p-15) = m1;
                *(htab_p-14) = m1;
                *(htab_p-13) = m1;
                *(htab_p-12) = m1;
                *(htab_p-11) = m1;
                *(htab_p-10) = m1;
                *(htab_p-9) = m1;
                *(htab_p-8) = m1;
                *(htab_p-7) = m1;
                *(htab_p-6) = m1;
                *(htab_p-5) = m1;
                *(htab_p-4) = m1;
                *(htab_p-3) = m1;
                *(htab_p-2) = m1;
                *(htab_p-1) = m1;
                htab_p -= 16;
        } while ((i -= 16) >= 0);

        for ( i += 16; i > 0; --i )
                *--htab_p = m1;
}

/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/

/*
 * Number of characters so far in this 'packet'
 */
static int a_count;

/*
 * Set up the 'byte output' routine
 */
static void
char_init()
{
        a_count = 0;
}

/*
 * Define the storage for the packet accumulator
 */
static char accum[ 256 ];

/*
 * Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void
char_out( c )
int c;
{
        accum[ a_count++ ] = c;
        if( a_count >= 254 )
                flush_char();
}

/*
 * Flush the packet to disk, and reset the accumulator
 */
static void
flush_char()
{
        if( a_count > 0 ) {
                fputc( a_count, g_outfile );
                fwrite( accum, 1, a_count, g_outfile );
                a_count = 0;
        }
}
