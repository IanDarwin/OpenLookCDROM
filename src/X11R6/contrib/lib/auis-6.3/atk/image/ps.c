/* ps.c - Interface between PostScript files and image */

/* Copyright 1992, 1994, Carnegie Mellon University - All rights reserved */

/*
 * This file is derived from a file with the following copyright:

 ****************************************************************
 * Copyright 1989, 1990, 1991, 1992 by John Bradley and
 *                       The University of Pennsylvania
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright
 * notice appear in all copies and that both the copyright notice and this
 * permission notice appear in supporting documentation.
 *
 * The software may be modified for your own purposes, but modified versions
 * may not be distributed.
 *
 * This software is provided "as is" without any expressed or implied warranty.
 *
 * The author may be contacted via:
 *    US Mail:   John Bradley
 *               GRASP Lab, Room 301C
 *               3401 Walnut St.
 *               Philadelphia, PA  19104
 *
 *    Phone:     (215) 898-8813
 *    EMail:     bradley@cis.upenn.edu
 *********************************************************

 * Permission has been granted by the above copyright holder to distribute 
 * this derived file under the following conditions:

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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/ps.c,v 1.14 1994/03/17 20:33:20 wjh Exp $";
#endif

#include <andrewos.h>
#include <math.h>
#include <image.ih>
#define AUXMODULE 1
#include <imagev.eh>

#define PIX2INCH 72.0   /* # of pixels per inch, at 100% scaling */

#define USNORMAL	0
#define A4		1
#define B5		2
#define USLEGAL		3
#define Bsize		4
#define A4by5		5
#define A35mm		6

/* sizes of pages in inches */
static double paperSize[7][2] = { { 8.500, 11.000},   /* US NORMAL */
				  { 8.267, 11.811},   /* A4 */
				  { 7.283, 10.630},   /* B5 */
				  { 8.500, 14.000},   /* US LEGAL */
				  {11.000, 17.000},   /* B-size */
				  { 3.875,  4.875},   /* 4 by 5 */
				  { 0.945,  1.417}};  /* 35mm (24x36) */

/* size of l+r margin and t+b margin.  image is centered */
static double margins[7][2] = { { 1.000, 1.000},   /* US NORMAL */
				{ 1.000, 1.000},   /* A4 */
				{ 1.000, 1.000},   /* B5 */
				{ 1.000, 1.000},   /* US LEGAL */
				{ 1.000, 1.000},   /* B-size */
				{ 0.275, 0.275},   /* 4 by 5 */
				{ 0.078, 0.078}};  /* 35mm (24x36) */

/* RANGE forces a to be in the range b..c (inclusive) */
#define RANGE(a,b,c) { if (a < b) a = b;  if (a > c) a = c; }

static void setScale();
void centerImage(), writePS();
static void psColorImage(), psColorMap(), epsPreview();
static void psRleCmapImage();
static int  rle_encode(), writeBWStip();

/* local variables */
static double sz_inx, sz_iny;     /* image size, in inches */
static double pos_inx, pos_iny;   /* top-left offset of image, in inches */
static int dpix, dpiy;		/* # of image pixels per inch */
static double psizex, psizey;	/* current paper size, in inches */
static int colorType; 

/***************************************************/
static void setPaper()
{
  psizex = paperSize[USNORMAL][0];
  psizey = paperSize[USNORMAL][1];
}

/***************************************************/
static void setScale( self, xscale, yscale )
    struct imagev *self;
    int xscale, yscale;
{
    struct image *image = (struct image *) imagev_GetDataObject(self);
    double hsx, hsy;
    int w = image_Width(image) , h = image_Height(image);

    sz_inx = (double) w / PIX2INCH * (xscale / 100.0);  
    sz_iny = (double) h / PIX2INCH * (yscale / 100.0);  

  /* round to integer .001ths of an inch */
    sz_inx = floor(sz_inx * 1000.0 + 0.5) / 1000.0;
    sz_iny = floor(sz_iny * 1000.0 + 0.5) / 1000.0;

    dpix = (int) (PIX2INCH / (xscale / 100.0));  
    dpiy = (int) (PIX2INCH / (yscale / 100.0));  

  /* make sure 'center' of image is still on page */
    hsx = sz_inx/2;  hsy = sz_iny/2;
    RANGE(pos_inx, -hsx, psizex-hsx);
    RANGE(pos_iny, -hsy, psizey-hsy);

  /* round to integer .001ths of an inch */
   pos_inx = floor(pos_inx * 1000.0 + 0.5) / 1000.0;
   pos_iny = floor(pos_iny * 1000.0 + 0.5) / 1000.0;
}


/***************************************************/
static void centerImage()
{
  pos_inx = psizex/2 - sz_inx/2;
  pos_iny = psizey/2 - sz_iny/2;

  /* round to integer .001ths of an inch */
  pos_inx = floor(pos_inx * 1000.0 + 0.5) / 1000.0;
  pos_iny = floor(pos_iny * 1000.0 + 0.5) / 1000.0;
}


/***************************************************/
void writePS( self, fp, wpts, hpts, toplevel )
    struct imagev *self;
    FILE *fp;
    int *wpts, *hpts;
    int toplevel;
{
    struct image *image = (struct image *) imagev_GetDataObject(self);
    struct image *new = image_New();
    int i, j, q, err, rpix, gpix, bpix, nc;
    int iw, ih, ox, oy, slen, lwidth, bits, colorps, w, h;
    byte *inpix, rmap[256], gmap[256], bmap[256];
    boolean rle = TRUE;

    if(self->orig && image != self->orig)
	image = self->orig;
    sz_inx = sz_iny = pos_inx = pos_iny = psizex = psizey = 0;
    colorType = image_Type(image);
    dpix = dpiy = 0;
    nc = image_RGBUsed(image);
    w = image_Width(image);
    h = image_Height(image);

    setPaper();
    setScale(self, 100, 100);

  /* printed image will have size iw,ih (in picas) */
    iw = (int) (sz_inx * 72.0 + 0.5);
    if(wpts) *wpts = iw;

    ih = (int) (sz_iny * 72.0 + 0.5);   
    if(hpts) *hpts = ih;
  /* compute offset to bottom-left of image (in picas) */
    ox = (int) (pos_inx * 72.0 + 0.5);
    oy = (int) ((psizey - (pos_iny + sz_iny)) * 72.0 + 0.5);

    if(toplevel < 0) {
	/*** write PostScript header ***/
	fprintf(fp,"%%!PS-Adobe-2.0 EPSF-2.0\n");
	fprintf(fp,"%%%%Creator: ATK image\n");

	fprintf(fp,"%%%%BoundingBox: %d %d %d %d\n", ox, oy, ox+iw, oy+ih);

	fprintf(fp,"%%%%Pages: 1\n");
	fprintf(fp,"%%%%DocumentFonts:\n");
	fprintf(fp,"%%%%EndComments\n");
    }

   image_Duplicate(image, new);
   switch (colorType) {
       case IRGB:
	   slen = w;
	   bits = 8;
	   colorps = 1;
	   break;
       case ITRUE:
	   image_Reduce(new, 256);
	   slen = w*3;
	   bits = 8;
	   colorps = 1;
	   break;
       case IBITMAP:
	   slen = (w+7)/8;
	   bits = 1;
	   colorps = 0;
	   break;
       case IGREYSCALE:
	   slen = w;
	   bits = 8;
	   colorps = 1;
	   break;
       default:
	   break;
   }
   colorType = image_Type(new);
   inpix = image_Data(new);
   nc = image_RGBUsed(new);
   if(! BITMAPP(new) )
       for(i = 0; i < nc; i++) {
	   *(rmap + i) = image_RedPixel(new, i) >> 8;
	   *(gmap + i) = image_GreenPixel(new, i) >> 8;
	   *(bmap + i) = image_BluePixel(new, i) >> 8;
       }

   if(FALSE)
       epsPreview(fp, inpix, colorType, w, h);

   if(toplevel == 0){
       fprintf(fp,"%%%%EndProlog\n\n");
       fprintf(fp,"%%%%Page: 1 1\n\n");

       fprintf(fp,"%% remember original state\n");
       fprintf(fp,"/origstate save def\n\n");
   }

   fprintf(fp,"%% build a temporary dictionary\n");
   fprintf(fp,"20 dict begin\n\n");

   if (colorType == IBITMAP || !rle) {
       fprintf(fp,"%% define string to hold a scanline's worth of data\n");
       fprintf(fp,"/pix %d string def\n\n", slen);
   }

   if(toplevel < 0) {
       fprintf(fp,"%% lower left corner\n");
       fprintf(fp,"%d %d translate\n\n",ox,oy);
   }

   fprintf(fp,"%% size of image (on paper, in 1/72inch coords)\n");
   fprintf(fp,"%d %d scale\n\n",iw,ih);

   if (colorType == IBITMAP) {   /* 1-bit dither code uses 'image' */
       fprintf(fp,"%% dimensions of data\n");
       fprintf(fp,"%d %d %d\n\n",w,h,bits);

       fprintf(fp,"%% mapping matrix\n");
       fprintf(fp,"[%d 0 0 %d 0 %d]\n\n", w, -h, h);

       fprintf(fp,"{currentfile pix readhexstring pop}\n");
       fprintf(fp,"image\n");
    
    /* write the actual image data */

       err = writeBWStip(fp, inpix, "", w, h);
   }

   else {      /* all other formats */
       byte *rleline;
       unsigned long outbytes = 0;

    /* if we're using color, make sure 'colorimage' is defined */
       if (colorps) psColorImage(fp);
    
       if (rle) {   /* write colormap and rle-colormapped image funct */
	   psColorMap(fp, colorps, nc, rmap, gmap, bmap);
	   psRleCmapImage(fp, colorps);
       }

       fprintf(fp,"%d %d %d\t\t\t%% dimensions of data\n",w,h,bits);
       fprintf(fp,"[%d 0 0 %d 0 %d]\t\t%% mapping matrix\n", w, -h, h);

       if (rle) fprintf(fp,"rlecmapimage\n");
       else {
	   fprintf(fp,"{currentfile pix readhexstring pop}\n");
	   if (colorps) fprintf(fp,"false 3 colorimage\n");
	   else fprintf(fp,"image\n");
       }

    /* dump the image data to the file */
       err = 0;

       if (rle) {
	   rleline  = (byte *) malloc(w * 2);  /* much worse than possible */
       }

       for (i = 0; i < h && err != EOF; i++) {
	   int rlen;
	   lwidth = 0;
	   putc('\n',fp);

	   if (rle) {  /* write rle-encoded colormapped image data */
	       rlen = rle_encode(inpix, rleline, w);
	       inpix += w;
	       outbytes += rlen;

	       for (j=0; j<rlen && err != EOF; j++) {
		   err = fprintf(fp,"%02x", rleline[j]);
		   lwidth += 2;

		   if (lwidth>70) { putc('\n',fp); lwidth = 0; }
	       }
	   }
	   else {  /* write non-rle raw (gray/rgb) image data */
	       for (j=0; j<w && err != EOF; j++) {
		   rpix = rmap[*inpix];
		   gpix = gmap[*inpix];
		   bpix = bmap[*inpix];
	  
		   if (colorps) { 
		       err = fprintf(fp,"%02x%02x%02x",rpix,gpix,bpix);
		       lwidth+=6;
		   }
      
		   else {  /* greyscale */
		       q = (rpix*21 + gpix*32 + bpix*11) / 64;
		       err = fprintf(fp,"%02x", q);
		       lwidth+=2;
		   }

		   if (lwidth>70) { putc('\n',fp); lwidth = 0; }
		   inpix++;
	       }
	   }
       }

       if (rle) {
	   free(rleline);
	   fprintf(fp,"\n\n");
	   fprintf(fp,"%%\n");
	   fprintf(fp,"%% Compression made this file %.2lf%% %s\n",
		   100.0 * ((double) outbytes) / 
		   ((double) image_Width(new) * image_Height(new) * ((colorps) ? 3 : 1)),
		   "of the uncompressed size.");
	   fprintf(fp,"%%\n");
       }
   }

   if(toplevel < 0) 
       fprintf(fp,"\n\nshowpage\n\n");

   fprintf(fp,"%% stop using temporary dictionary\n");
   fprintf(fp,"end\n\n");

   if(toplevel == 0) {
       fprintf(fp,"%% restore original state\n");
       fprintf(fp,"origstate restore\n\n");
   }

   fprintf(fp,"%%%%Trailer\n");

   image_Destroy(new);
}


/**********************************************/
static int rle_encode(scanline, rleline, wide)
     byte *scanline, *rleline;
     int wide;
{
  /* generates a rle-compressed version of the scan line.
   * rle is encoded as such:
   *    <count> <value>                      # 'run' of count+1 equal pixels
   *    <count | 0x80> <count+1 data bytes>  # count+1 non-equal pixels
   *
   * count can range between 0 and 127
   *
   * returns length of the rleline vector
   */
  
  int  i, j, blocklen, isrun, rlen;
  byte block[256], pix;
  
  blocklen = isrun = rlen = 0;

  for (i=0; i<wide; i++) {
    /* there are 5 possible states:
     *   0: block empty.
     *   1: block not empty, block is  a run, current pix == previous pix
     *   2: block not empty, block is  a run, current pix != previous pix
     *   3: block not empty, block not a run, current pix == previous pix
     *   4: block not empty, block not a run, current pix != previous pix
     */

    pix = scanline[i];

    if (!blocklen) {                    /* case 0:  empty */
      block[blocklen++] = pix;
      isrun = 1;
    }

    else if (isrun) {
      if (pix == block[blocklen-1]) {   /* case 1:  isrun, prev==cur */
	block[blocklen++] = pix;
      }
      else {                            /* case 2:  isrun, prev!=cur */
	if (blocklen>1) {               /*   we have a run block to flush */
	  rleline[rlen++] = blocklen-1;
	  rleline[rlen++] = block[0];
	  block[0] = pix;               /*   start new run block with pix */
	  blocklen = 1;
	}
	else {
	  isrun = 0;                    /*   blocklen<=1, turn into non-run */
	  block[blocklen++] = pix;
	}
      }
    }
	
    else {   /* not a run */
      if (pix == block[blocklen-1]) {   /* case 3:  non-run, prev==cur */
	if (blocklen>1) {               /*  have a non-run block to flush */
	  rleline[rlen++] = (blocklen-1) | 0x80;
	  for (j=0; j<blocklen; j++)
	    rleline[rlen++] = block[j];

	  block[0] = pix;               /*  start new run block with pix */
	  blocklen = isrun = 1;
	}
	else {
	  isrun = 1;                    /*  blocklen<=1 turn into a run */
	  block[blocklen++] = pix;
	}
      }
      else {                            /* case 4:  non-run, prev!=cur */
	block[blocklen++] = pix;
      }
    }

    if (blocklen == 128) {   /* max block length.  flush */
      if (isrun) {
	rleline[rlen++] = blocklen-1;
	rleline[rlen++] = block[0];
      }

      else {
	rleline[rlen++] = (blocklen-1) | 0x80;
	for (j=0; j<blocklen; j++) 
	  rleline[rlen++] = block[j];
      }

      blocklen = 0;
    }
  }

  if (blocklen) { /* flush last block */
      if (isrun) {
	  rleline[rlen++] = blocklen-1;
	  rleline[rlen++] = block[0];
      }

      else {
	  rleline[rlen++] = (blocklen-1) | 0x80;
	  for (j=0; j<blocklen; j++) 
	      rleline[rlen++] = block[j];
      }
  }
  return rlen;
}
	  
	    
/**********************************************/
static void psColorImage(fp)
FILE *fp;
{
  /* spits out code that checks if the PostScript device in question knows about the 'colorimage' operator.  If it doesn't, it defines 'colorimage' in terms of image (ie, generates a greyscale image from RGB data) */

  fprintf(fp,"%% define 'colorimage' if it isn't defined\n");
  fprintf(fp,"%%   ('colortogray' and 'mergeprocs' come from xwd2ps\n");
  fprintf(fp,"%%     via xgrab)\n");
  fprintf(fp,"/colorimage where   %% do we know about 'colorimage'?\n");
  fprintf(fp,"  { pop }           %% yes: pop off the 'dict' returned\n");
  fprintf(fp,"  {                 %% no:  define one\n");
  fprintf(fp,"    /colortogray {  %% define an RGB->I function\n");
  fprintf(fp,"      /rgbdata exch store    %% call input 'rgbdata'\n");
  fprintf(fp,"      rgbdata length 3 idiv\n");
  fprintf(fp,"      /npixls exch store\n");
  fprintf(fp,"      /rgbindx 0 store\n");
  fprintf(fp,"      /grays npixls string store  %% str to hold the result\n");
  fprintf(fp,"      0 1 npixls 1 sub {\n");
  fprintf(fp,"        grays exch\n");
  fprintf(fp,"        rgbdata rgbindx       get 20 mul    %% Red\n");
  fprintf(fp,"        rgbdata rgbindx 1 add get 32 mul    %% Green\n");
  fprintf(fp,"        rgbdata rgbindx 2 add get 12 mul    %% Blue\n");
  fprintf(fp,"        add add 64 idiv      %% I = .5G + .31R + .18B\n");
  fprintf(fp,"        put\n");
  fprintf(fp,"        /rgbindx rgbindx 3 add store\n");
  fprintf(fp,"      } for\n");
  fprintf(fp,"      grays\n");
  fprintf(fp,"    } bind def\n\n");

  fprintf(fp,"    %% Utility procedure for colorimage operator.\n");
  fprintf(fp,"    %% This procedure takes two procedures off the\n");
  fprintf(fp,"    %% stack and merges them into a single procedure.\n\n");
  
  fprintf(fp,"    /mergeprocs { %% def\n");
  fprintf(fp,"      dup length\n");
  fprintf(fp,"      3 -1 roll\n");
  fprintf(fp,"      dup\n");
  fprintf(fp,"      length\n");
  fprintf(fp,"      dup\n");
  fprintf(fp,"      5 1 roll\n");
  fprintf(fp,"      3 -1 roll\n");
  fprintf(fp,"      add\n");
  fprintf(fp,"      array cvx\n");
  fprintf(fp,"      dup\n");
  fprintf(fp,"      3 -1 roll\n");
  fprintf(fp,"      0 exch\n");
  fprintf(fp,"      putinterval\n");
  fprintf(fp,"      dup\n");
  fprintf(fp,"      4 2 roll\n");
  fprintf(fp,"      putinterval\n");
  fprintf(fp,"    } bind def\n\n");

  fprintf(fp,"    /colorimage { %% def\n");
  fprintf(fp,"      pop pop     %% remove 'false 3' operands\n");
  fprintf(fp,"      {colortogray} mergeprocs\n");
  fprintf(fp,"      image\n");
  fprintf(fp,"    } bind def\n");
  fprintf(fp,"  } ifelse          %% end of 'false' case\n");
  fprintf(fp,"\n\n\n");
}


/**********************************************/
static void psColorMap(fp, color, nc, rmap, gmap, bmap)
     FILE *fp;
     int color, nc;
     byte *rmap, *gmap, *bmap;
{
  /* spits out code for the colormap of the following image
     if !color, it spits out a mono-ized graymap */

  int i;

  fprintf(fp,"%% define the colormap\n");
  fprintf(fp,"/cmap %d string def\n\n\n", nc * ((color) ? 3 : 1));

  fprintf(fp,"%% load up the colormap\n");
  fprintf(fp,"currentfile cmap readhexstring\n");

  for (i=0; i<nc; i++) {
    fprintf(fp,"%02x%02x%02x ", rmap[i],gmap[i],bmap[i]);
    if ((i%10) == 9) fprintf(fp,"\n");
  }
  if (i%10) fprintf(fp,"\n");
  fprintf(fp,"pop pop   %% lose return values from readhexstring\n\n\n");
		 
}


/**********************************************/
static void psRleCmapImage(fp, color)
FILE *fp;
{
  /* spits out code that defines the 'rlecmapimage' operator */

  fprintf(fp,"%% rlecmapimage expects to have 'w h bits matrix' on stack\n");
  fprintf(fp,"/rlecmapimage {\n");
  fprintf(fp,"  /buffer 1 string def\n");
  fprintf(fp,"  /rgbval 3 string def\n");
  fprintf(fp,"  /block  384 string def\n\n");

  fprintf(fp,"  %% proc to read a block from file, and return RGB data\n");
  fprintf(fp,"  { currentfile buffer readhexstring pop\n");
  fprintf(fp,"    /bcount exch 0 get store\n");
  fprintf(fp,"    bcount 128 ge\n");
  fprintf(fp,"    {  %% it's a non-run block\n");
  fprintf(fp,"      0 1 bcount 128 sub\n");
  fprintf(fp,"      { currentfile buffer readhexstring pop pop\n\n");

  if (color) {
    fprintf(fp,"        %% look up value in color map\n");
    fprintf(fp,"%s/rgbval cmap buffer 0 get 3 mul 3 getinterval store\n\n",
	    "        ");
    fprintf(fp,"        %% and put it in position i*3 in block\n");
    fprintf(fp,"        block exch 3 mul rgbval putinterval\n");
    fprintf(fp,"      } for\n");
    fprintf(fp,"      block  0  bcount 127 sub 3 mul  getinterval\n");
    fprintf(fp,"    }\n\n");
  }
  else {
    fprintf(fp,"        %% look up value in gray map\n");
    fprintf(fp,"%s/rgbval cmap buffer 0 get 1 getinterval store\n\n",
	    "        ");
    fprintf(fp,"        %% and put it in position i in block\n");
    fprintf(fp,"        block exch rgbval putinterval\n");
    fprintf(fp,"      } for\n");
    fprintf(fp,"      block  0  bcount 127 sub  getinterval\n");
    fprintf(fp,"    }\n\n");
  }

  fprintf(fp,"    { %% else it's a run block\n");
  fprintf(fp,"      currentfile buffer readhexstring pop pop\n\n");

  if (color) {
    fprintf(fp,"      %% look up value in colormap\n");
    fprintf(fp,"%s/rgbval cmap buffer 0 get 3 mul 3 getinterval store\n\n",
	    "      ");
    fprintf(fp,"%s0 1 bcount { block exch 3 mul rgbval putinterval } for\n\n",
	    "      ");
    fprintf(fp,"      block 0 bcount 1 add 3 mul getinterval\n");
  }
  else {
    fprintf(fp,"      %% look up value in graymap\n");
    fprintf(fp,"      /rgbval cmap buffer 0 get 1 getinterval store\n\n");
    fprintf(fp,"      0 1 bcount { block exch rgbval putinterval } for\n\n");
    fprintf(fp,"      block 0 bcount 1 add getinterval\n");
  }

  fprintf(fp,"    } ifelse\n");
  fprintf(fp,"  } %% end of proc\n");

  if (color) fprintf(fp,"  false 3 colorimage\n");
        else fprintf(fp,"  image\n");

  fprintf(fp,"} bind def\n\n\n");
}



/**********************************************/
static void epsPreview(self, fp, pic)
    struct imagev *self;
    FILE *fp;
    byte *pic;
{
    struct image *image = (struct image *) imagev_GetDataObject(self);
    byte *prev;
    int w = image_Width(image), h = image_Height(image);

  /* put in an EPSI preview */
  
    if (colorType != IBITMAP) { /* have to generate a preview */
     image_Dither(image);
    }
    prev = image_Data(image);
  
    fprintf(fp,"%%%%BeginPreview: %d %d %d %d\n", w, h, 1, 
	    (w/(72*4) + 1) * h);

    writeBWStip(fp, prev, "% ", w, h);

    fprintf(fp,"%%%%EndPreview\n");
}

static unsigned char hex[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};

static unsigned char invhex[16] = {
	'f', 'e', 'd', 'c', 'b', 'a', '9', '8',
	'7', '6', '5', '4', '3', '2', '1', '0'
};

/***********************************/
static int writeBWStip(fp, pic, prompt, w, h)
    FILE *fp;
    byte *pic;
    char *prompt;
    int  w, h;
{
  /* write the given 'pic' (B/W stippled, 1 byte per pixel, 0=blk,1=wht) out as hexadecimal, max of 72 hex chars per line. returns '0' if everythings fine, 'EOF' if writing failed */
  int err = 0, i, j, lwidth;
  int widthbytes = (w+7) >> 3;
  unsigned char *tbl = ((TRUE) ? invhex : hex);

  for (i = 0, lwidth = 0; i < h; i++) {
      fprintf(fp, "%s", prompt);
      for (j = 0; j < widthbytes; j++) {
	  fputc(*(tbl + ((*pic) / 16)), fp); 
	  fputc(*(tbl + ((*pic) & 15)), fp);
	  pic++;
	  lwidth += 2;
	  if (lwidth >= 72) { 
	      fprintf(fp, "\n%s", prompt); 
	      lwidth = 0; 
	  }
      }
      fprintf(fp, "\n");
  }
  return err;
}




