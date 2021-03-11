/* -*-C-*-
********************************************************************************
*
* File:         xgif.c
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/xgif.c,v 2.1 1994/06/06 15:47:59 npm Exp $
* Description:  Convert a GIF file to an XImage. The code in this file is 
*		incredibly crufty. It is based on "xgiftools" distribution
*		and was quickly hacked to get it to work w/ WINTERP.
* Author:       Alfred Kayser (AKayser@dnpap.et.tudelft.nl), John Bradley
*		(bradley@cis.upenn.edu), Patrick J. Naughton (naughton@wind.sun.com),
*		Niels Mayer (mayer@netcom.com).
* Created:      6 Mar 92 12:47:22 GMT
* Modified:     Sun Jun  5 04:13:14 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* gif2ras.c Copyright (c) 1988, 1989 by Patrick J. Naughton (naughton@wind.sun.com)
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Alfred Kayser, John Bradley and
* Patrick J. Naughton not be used in advertising or publicity pertaining to
* distribution of the software without specific, written prior permission. 
* Enterprise Integration Technologies, Hewlett-Packard Company, Niels Mayer,
* Alfred Kayser, John Bradley and Patrick J. Naughton makes no representations 
* about the suitability of this software for any purpose. It is provided "as is" 
* without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* ALFRED KAYSER, JOHN BRADLEY AND PATRICK J. NAUGHTON DISCLAIMS ALL
* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
* OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER, ALFRED KAYSER, JOHN
* BRADLEY OR PATRICK J. NAUGHTON BE LIABLE FOR ANY SPECIAL, INDIRECT OR
* CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
* DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
* TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE. THE AUTHORS SHALL HAVE NO LIABILITY WITH
* RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS
* BY THIS file OR ANY PART THEREOF.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/widgets/RCS/xgif.c,v 2.1 1994/06/06 15:47:59 npm Exp $";

#if 0 /* NPM: COMMENTOUT */
#include "xgif.h"
#endif /* NPM: COMMENTOUT */

/* include files */
#include <stdio.h>
#include <math.h>
#include <ctype.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef unsigned char byte;

/* X stuff -- pretty lame use of global variables here, but that's how the 'xgif' code is -- NPM */
static Display*	theDisp;
static int	dispcells;
static Colormap	theCmap;
static Visual*	theVisual;
static XImage*	theImage;

/* global vars -- pretty lame use of global variables here, but that's how the 'xgif' code is -- NPM */
#define MAX_NUM_COLORS 256
static unsigned long  cols[MAX_NUM_COLORS];
static XColor         defs[MAX_NUM_COLORS];
static int            quiet;

static Bool ReadImageData(/* FILE  *fp, char* *err_string */);
static Bool DecodeImage(/* char* *err_string */);
static int  ReadCode(/* void */);
static void AddToPixel(/* byte Index */);
static void ReadColormap(/* FILE *fp */);

#if 0 /* NPM: COMMENTOUT */
static Colormap LocalCmap;
static Window   mainW;
static int      local;
static int      DEBUG;
static GC       theGC;
static Window   rootW;
static int      dispWIDE, dispHIGH;
static int      iWIDE,iHIGH;
static int	theScreen;
static char     *cmd;
typedef int	boolean;
static int	Offset,		/* Offset in output array */
#define ADDTOPIXEL(a) if (Quick) Image[Offset++]=a; else AddToPixel(a)
#endif /* NPM: COMMENTOUT */


#define IMAGESEP 0x2c
#define EXTENSION 0x21
#define INTERLACEMASK 0x40
#define COLORMAPMASK 0x80

#define ALLOCATED 3

static int EGApalette[16][3] = {
  {0,0,0},       {0,0,128},     {0,128,0},     {0,128,128}, 
  {128,0,0},     {128,0,128},   {128,128,0},   {200,200,200},
  {100,100,100}, {100,100,255}, {100,255,100}, {100,255,255},
  {255,100,100}, {255,100,255}, {255,255,100}, {255,255,255} };

static int BitOffset,	        /* Bit Offset of next code */
    XC, YC,                     /* Output X and Y coords of current pixel */
    Pass,			/* Used by output routine if interlaced pic */
    Width, Height,		/* image dimensions */
    BytesPerScanline,		/* bytes per scanline in output raster */
    ColorMapSize,		/* number of colors */
    Background,			/* background color */
    NumUsed,			/* Number of colors really used */
    CodeSize,			/* Code size, read from GIF header */
    ReadMask;			/* Code AND mask for current code size */

static Bool Interlace, HasColormap;

static byte *Image;		/* The result array */
				/* NPM: "Image" created by LoadGIF()-->DecodeImage().
				         Normally, destroyed by CloseGif(), which gets
					 called from main()->ShowGif() when main() exits.
					 For WINTERP, we destroy this data after we have
					 copied XImage 'theImage' to an off-screen pixmap
					 in tic_GIF.c:Tango_GIF_Image_Class_Method_ISNEW(). */
static byte *Raster;		/* The raster data stream, unblocked */
				/* NPM: "Raster" created by LoadGIF()->ReadImageData().
					Destroyed by LoadGIF()-->DecodeImage(). */

    /* The GIF headers found in the file */
static byte gifheader[13];
static byte imageheader[9];
static byte colormap[3*MAX_NUM_COLORS];

    /* The hash table used by the decompressor */
static int  Prefix[4096];
static int  Suffix[4096];

    /* An output array used by the decompressor */
static byte OutCode[1025];

    /* The color map, read from the GIF header */
static byte Red[MAX_NUM_COLORS], Green[MAX_NUM_COLORS], Blue[MAX_NUM_COLORS], used[MAX_NUM_COLORS];

static char id[] = "GIF";

static Bool LoadGIF(fname, err_string)
     char* fname;		/* IN: file-name of GIF */
     char* *err_string;		/* OUT: if routine returns False, returns a string w/ error message */
{
    register byte ch;
    FILE *fp;

    BitOffset = 0,		/* Bit Offset of next code */
    XC = 0, YC = 0,		/* Output X and Y coords of current pixel */
#if 0 /* NPM: COMMENTOUT */
    Offset = 0,                 /* Offset in output array */
#endif /* NPM: COMMENTOUT */
    Pass = 0;			/* Used by output routine if interlaced pic */
    ColorMapSize = 0;
    Image = NULL;

    fp = fopen(fname,"r");

    if (!fp) {
      *err_string = "File not found";
      return (False);
    }

    if ( (fread(gifheader, sizeof(gifheader), 1, fp)!=1)
      || (strncmp((char*) gifheader, id, 3)!=0) ) {
      *err_string = "Not a GIF file";
      fclose(fp);
      return (False);
    }
    if (strncmp((char*) gifheader+3, "87a", 3) && strncmp((char*) gifheader+3,"89a",3))
	fprintf(stderr,"Warning: %s contains unknown version %c%c%c",
		fname,		/* NPM added this -- missing in original :-( */
		gifheader[3],gifheader[4],gifheader[5]);
    HasColormap = ((gifheader[10] & COLORMAPMASK) ? True : False);
    ColorMapSize = 1 << (gifheader[10]&7)+1;

    Background = gifheader[11];		/* background color... not used. */

/* Read in global colormap. */
    if (HasColormap) ReadColormap(fp);

/* Check for image extension */
    while ((ch=getc(fp)) == EXTENSION)
    {
	getc(fp);			/* skip extension code */
	while ((ch=getc(fp))>0)
	    fseek(fp, ch, 1);		/* skip it */
    }
	
    if (ch != IMAGESEP) {
      *err_string = "corrupt GIF file (no image separator)";
      fclose(fp);
      return (False);
    }

    fread(imageheader,sizeof(imageheader),1,fp);

    Width = imageheader[4] + 0x100 * imageheader[5];
    Height = imageheader[6] + 0x100 * imageheader[7];

    if (!quiet) 
        fprintf(stdout,"%s: %dx%dx%d\n", fname, Width, Height, ColorMapSize);

    Interlace = ((imageheader[8] & INTERLACEMASK) ? True : False);

    if (imageheader[8] & COLORMAPMASK) 
    {
        HasColormap = True;
        ColorMapSize = 1 << (imageheader[8]&7)+1;
        ReadColormap(fp);
    }
    CodeSize = getc(fp); 
    if (!ReadImageData(fp, err_string)) {
      /* *err_string -- set by ReadImageData() */
      fclose(fp);
      return (False);
    }

    fclose(fp);

    if (!DecodeImage(err_string)) { 
      /* *err_string -- set by DecodeImage() */
      free(Raster);		/* free storage allocated in ReadImageData() */
      return (False);
    }

    return (True);
}


static Bool ReadImageData(fp, err_string)
     FILE  *fp;			/* IN: file */
     char* *err_string;		/* OUT: if routine returns False, returns a string w/ error message */
{
/* Read the raster data.  Here we just transpose it from the GIF array
 * to the Raster array, turning it from a series of blocks into one long
 * data stream, which makes life much easier for ReadCode().
 */
    long filesize, filepos;
    int ch;
    byte *ptr1;

    /* find the size of the file */
    filepos = ftell(fp);
    fseek(fp, 0L, 2);
    filesize = ftell(fp)-filepos;
    fseek(fp, filepos, 0);

    if (!(Raster = (byte *) malloc((unsigned) (filesize * sizeof(byte))))) {
      *err_string = "Not enough memory to store gif data";
      return (False);
    }

    ptr1 = Raster;
    while ((ch = getc(fp))>0)
    {
	if (fread(ptr1, 1, ch, fp)<ch) {
	  *err_string = "corrupt GIF file (unblock)";
	  free(Raster);		/* free storage allocated above */
	  return (False);
	}
        ptr1 += ch;
    } 

    return (True);
}


static Bool DecodeImage(err_string)
     char* *err_string;		/* OUT: if routine returns False, returns a string w/ error message */
{
/* Start reading the raster data. First we get the intial code size
 * and compute decompressor constant values, based on this code size.
 */
#if 0 /* NPM: COMMENTOUT */
int Quick;			/* True, when not interlaced and local Cmap */
#endif /* NPM: COMMENTOUT */
int InitCodeSize,		/* Starting code size, used during Clear */
    InCode,			/* Value returned by ReadCode */
    MaxCode,			/* limiting value for current code size */
    ClearCode,			/* GIF clear code */
    EOFCode,			/* GIF end-of-information code */
    CurCode, OldCode,		/* Decompressor variables */
    FreeCode,			/* Decompressor, next free slot in hashtable */
    OutCount = 0,		/* Decompressor output 'stack count' */
    FinChar,			/* Decompressor variable */
    BitMask;			/* AND mask for data size */

    BitMask = ColorMapSize - 1;

    ClearCode = (1 << CodeSize);
    EOFCode = ClearCode + 1;
    FreeCode = ClearCode + 2;

/* The GIF spec has it that the code size is the code size used to
 * compute the above values is the code size given in the file, but the
 * code size used in compression/decompression is the code size given in
 * the file plus one. (thus the ++).
 */

    CodeSize++;
    InitCodeSize = CodeSize;
    MaxCode = (1 << CodeSize);
    ReadMask = MaxCode - 1;

/* Allocate the X Image */
    if (!(Image = (byte *) malloc((unsigned) (Width*Height * sizeof(byte))))) {
	*err_string = "not enough memory for image";
	/* NPM: 'Raster' storage allocated in ReadImageData() is freed by caller. */
	return (False);
      }

    if (!(theImage = XCreateImage(theDisp, theVisual, 8, ZPixmap, 0, (char*) Image,
                             Width, Height, 8, Width))) {
      *err_string = "unable to create XImage";
      free(Image);		/* XDestroyImage(theImage) would normally free 'Image', but since it failed... */
      /* NPM: 'Raster' storage allocated in ReadImageData() is freed by caller. */
      return (False);
    }
    BytesPerScanline = Width;

/* Decompress the file, continuing until you see the GIF EOF code.
 * One obvious enhancement is to add checking for corrupt files here.
 */
#if 0 /* NPM: COMMENTOUT */
    Quick = (local && !Interlace);
    Offset = 0; 
    if (DEBUG) fprintf(stderr,"Decoding...\n");
#endif /* NPM: COMMENTOUT */
    InCode = ReadCode();
    while (InCode != EOFCode) {

/* Clear code sets everything back to its initial value, then reads the
 * immediately subsequent code as uncompressed data.
 */

	if (InCode == ClearCode) {
	    CodeSize = InitCodeSize;
	    MaxCode = (1 << CodeSize);
	    ReadMask = MaxCode - 1;
            FreeCode = ClearCode + 2;
	    CurCode = OldCode = InCode = ReadCode();
	    FinChar = CurCode & BitMask;
#if 0 /* NPM: REPLACE */
	    ADDTOPIXEL(FinChar);
#else /* NPM: REPLACE */
	    AddToPixel(FinChar);
#endif /* NPM: REPLACE */
	}
	else {

/* If not a clear code, then must be data: save same as CurCode */

	    CurCode = InCode;

/* If greater or equal to FreeCode, not in the hash table yet;
 * repeat the last character decoded
 */

	    if (CurCode >= FreeCode) {
		CurCode = OldCode;
		OutCode[OutCount++] = FinChar;
	    }

/* Unless this code is raw data, pursue the chain pointed to by CurCode
 * through the hash table to its end; each code in the chain puts its
 * associated output code on the output queue.
 */

	    while (CurCode > BitMask) {
		if (OutCount >= 1024) {
		  *err_string = "Corrupt GIF file (OutCount)!";
		  XDestroyImage(theImage); /* deallocate XImage allocated by LoadGIF()-->DecodeImage(),
					      XDestroyImage() deallocates theImage->data == Image */
		  /* NPM: 'Raster' storage allocated in ReadImageData() is freed by caller. */
		  return (False);
                }
		OutCode[OutCount++] = Suffix[CurCode];
		CurCode = Prefix[CurCode];
	    }

/* The last code in the chain is treated as raw data. */

	    /* OutCode[OutCount++] = FinChar = CurCode &BitMask*/;
	    FinChar = CurCode & BitMask;
#if 0 /* NPM: REPLACE */
	    ADDTOPIXEL(FinChar);
#else /* NPM: REPLACE */
	    AddToPixel(FinChar);
#endif /* NPM: REPLACE */


/* Now we put the data out to the Output routine.
 * It's been stacked LIFO, so deal with it that way...  */
	    while (OutCount>0)
#if 0 /* NPM: REPLACE */
		ADDTOPIXEL(OutCode[--OutCount]);
#else /* NPM: REPLACE */
		AddToPixel(OutCode[--OutCount]);
#endif /* NPM: REPLACE */


/* Build the hash table on-the-fly. No table is stored in the file. */

	    Prefix[FreeCode] = OldCode;
	    Suffix[FreeCode] = FinChar;
	    OldCode = InCode;

/* Point to the next slot in the table.  If we exceed the current
 * MaxCode value, increment the code size unless it's already 12.  If it
 * is, do nothing: the next code decompressed better be CLEAR
 */

	    FreeCode++;
	    if (FreeCode >= MaxCode) {
		if (CodeSize < 12) {
		    CodeSize++;
		    MaxCode *= 2;
		    ReadMask = (1 << CodeSize) - 1;
		}
	    }
	}
	InCode = ReadCode();
    }
    free(Raster);

   return (True);
}


/* Fetch the next code from the raster data stream.  The codes can be
 * any length from 3 to 12 bits, packed into 8-bit bytes, so we have to
 * maintain our location in the Raster array as a BIT Offset.  We compute
 * the byte Offset into the raster array by dividing this by 8, pick up
 * three bytes, compute the bit Offset into our 24-bit chunk, shift to
 * bring the desired code to the bottom, then mask it off and return it. 
 */
static int ReadCode()
{
    int RawCode, ByteOffset, BitShift;

    ByteOffset = BitOffset / 8;
    BitShift = BitOffset % 8;
    BitOffset += CodeSize;
    if (BitShift+CodeSize<8)
	return (Raster[ByteOffset]>>BitShift) & ReadMask;
    else
    {
        RawCode = Raster[ByteOffset] + (0x100 * Raster[ByteOffset + 1]);
        if (BitShift+CodeSize >= 16)
	    RawCode += (0x10000 * Raster[ByteOffset + 2]);
        return((RawCode>>BitShift) & ReadMask);
    }
}


static void AddToPixel(Index)
byte Index;
{
    if (YC<Height) /* Might be of importance when reading interlaced gifs */
        Image[YC*BytesPerScanline+XC] = Index;
    if (!used[Index]) { used[Index]=True; NumUsed++; }
    if (++XC == Width)
    {
	XC = 0;
	if (Interlace)
	{
	    switch (Pass) 
            {
	    case 0: YC += 8; if (YC >= Height) { Pass++; YC = 4; } break;
            case 1: YC += 8; if (YC >= Height) { Pass++; YC = 2; } break;
            case 2: YC += 4; if (YC >= Height) { Pass++; YC = 1; } break;
            case 3: YC += 2; break;
            default: break;
	    }
	}
	else
	    YC++;
    }
}



static Bool ColorDicking(err_string)
     char* *err_string;		/* OUT: if routine returns False, returns a string w/ error message */
{
    /* we've got the picture loaded, we know what colors are needed. get 'em */
    int i,j;
    register byte *ptr;

    if (!HasColormap)
    {
#if 0 /* NPM: COMMENTOUT */
	if (DEBUG) fprintf(stderr,"Using EGA palette as default\n");
#endif /* COMMENTOUT */
        for (i=0; i<ColorMapSize; i++) {
            Red[i] = EGApalette[i&15][0];
            Green[i] = EGApalette[i&15][1];
            Blue[i] = EGApalette[i&15][2];
            used[i] = True;
	    }
    }

    for (i=j=0; i<ColorMapSize; i++)
    {
        if (
#if 0 /* NPM: COMMENTOUT */
	    local ||
#endif /* NPM: COMMENTOUT */
	    used[i])
	{
	    defs[i].red   = Red[i]<<8;
	    defs[i].green = Green[i]<<8;
	    defs[i].blue  = Blue[i]<<8;
	    defs[i].flags = DoRed | DoGreen | DoBlue;
	    defs[i].pixel = i;
        }
    }
#if 0 /* NPM: COMMENTOUT */
    if (local && HasColormap)
    { 
	LocalCmap=XCreateColormap(theDisp,mainW,theVisual,AllocAll);
	XStoreColors(theDisp,LocalCmap,defs,ColorMapSize);
	return;
    }
#endif /* NPM: COMMENTOUT */

    if (!quiet) fprintf(stdout,"Allocating %d colors...\n",NumUsed);

    /* Allocate the X colors for this picture */
    for (i=j=0; i<ColorMapSize; i++)
    {
      if (
#if 0  /* NPM: COMMENTOUT */
	  local || 
#endif /* NPM: COMMENTOUT */
	  used[i])
	{
	  if (!XAllocColor(theDisp,theCmap,&defs[i])
	      || defs[i].red>>8   != Red[i] /* Not an exact match! -- fixed by NPM */
	      || defs[i].green>>8 != Green[i] /* fixed by NPM */
	      || defs[i].blue>>8  != Blue[i] /* fixed by NPM */
	      ) { 
	    j++;
	    defs[i].pixel = 0xffff;
	  }
	  else {
	    used[i]=ALLOCATED;
	  }
	  cols[i] = defs[i].pixel;
        }
      else cols[i]=i;
    }
    if (j) 		/* failed to pull it off */
    {
      XColor ctab[MAX_NUM_COLORS];
      int dc = (dispcells<MAX_NUM_COLORS) ? dispcells : MAX_NUM_COLORS;

      if (!quiet)
	fprintf(stdout,"Failed to allocate %d out of %d colors. \n%s",
		j,NumUsed,  "   Searching for resembling colors\n");

      /* read in the color table */
      for (i=0; i<dc; i++) ctab[i].pixel = i;
      XQueryColors(theDisp, theCmap, ctab, dc);
                
      /* run through the used colors.  any used color that has a pixel
	 value of 0xffff wasn't allocated.  for such colors, run through
	 the entire X colormap and pick the closest color */

      for (i=0; i<ColorMapSize; i++)
	{
	  if (used[i] && cols[i]==0xffff) /* an unallocated pixel */
	    {
	      long mdist = 0x7FFFFFFL; /* MAXLONG */
	      int close = -1;
	      long d;
	      long r = Red[i]<<8;
	      long g = Green[i]<<8;
	      long b = Blue[i]<<8;

	      for (j=0; j<dc; j++)
		{
		  d = 5*abs(r-ctab[j].red)
		    + 3*abs(g-ctab[j].green) +
		      1*abs(b-ctab[j].blue);
		  if (d<mdist) { mdist=d; close=j; }
                }
	      /* NPM: fixed this s.t. XAllocColor() is called to "reserve"
		 the color. Otherwise, other apps may clobber some of the
		 colors chosen (but not allocated) by the routines below.
		 Note that some read/write colors may match, but will not
		 satisfy XAllocColor(), which allocates read-only colors. */
	      if ((close<0) || !XAllocColor(theDisp,theCmap,&ctab[close])) {
		/* NPM: deallocate any colors already allocated by this routine */
		unsigned long pixels[MAX_NUM_COLORS];
		for (j = i = 0 ; (i < ColorMapSize) ; i++) { 
		  if (used[i] == ALLOCATED)
		    pixels[j++] = cols[i];
		}
		XFreeColors(theDisp, theCmap, pixels, j, 0L);
		/* NPM: XImage allocated by DecodeImage() is destroyed by caller... */
		*err_string = "Can't allocate a resembling color.";
		return (False);
	      }
	      else {
		used[i] = ALLOCATED; /* NPM: if XAllocColor() succeeded, then mark the color as allocated */
                cols[i] = ctab[close].pixel;
	      }
            }
        }
    }

    if (!quiet) fprintf(stdout, "Building XImage...\n");
    ptr = Image;
    for (i=0; i<Height; i++)
        for (j=0; j<Width; j++,ptr++) 
            *ptr = (byte) cols[*ptr];

    return (True);
}


static void ReadColormap(fp)
FILE *fp;
{
    byte *ptr=colormap;
    int i;

#if 0 /* COMMENTOUT */
    if (DEBUG) fprintf(stderr,"Reading Color map...\n");
#endif /* NPM: COMMENTOUT */
    fread(colormap, ColorMapSize, 3, fp);
    for (i = 0; i < ColorMapSize; i++) {
        Red[i] = (*ptr++);
        Green[i] = (*ptr++);
        Blue[i] = (*ptr++);
        used[i] = 0;
    }
    NumUsed=0;
}


static CloseGif()
{
#if 0 /* NPM: COMMENTOUT */
    if (LocalCmap)
    {
        XFreeColormap(theDisp, LocalCmap);
	LocalCmap=NULL;
    }  
    else
    {
#endif /* NPM: COMMENTOUT */
        int i,j;
	unsigned long pixels[MAX_NUM_COLORS];
	 
        for (j=i=0;i<ColorMapSize;i++)
            if (used[i]==ALLOCATED)
		pixels[j++]=cols[i];
        XFreeColors(theDisp, theCmap, pixels, j, 0L);
#if 0 /* NPM: COMMENTOUT */
    }
#endif /* NPM: COMMENTOUT */
    if (Image)
    {
	free(Image);
	Image=NULL;
    }
}


/*****************************************************************************
 * NPM's attempt at rationalizing the crappy programming in this file.
 * If this routine returns 'True', you should deallocate the returned XImage
 * via XDestroyImage(). (XDestroyImage() deallocates the XImage and the 'Image'
 * data (XImage->data) alloc'd in DecodeImage()).
 *
 * If this routine returns 'True', then the returned value *alloc_cols is an
 * array (of length *num_alloc_cols) of Pixel values allocated by XAllocColor().
 * When the GIF image here is no longer being displayed or used, the caller
 * should deallocate the colors via
 * XFreeColors(disp, colormap, alloc_cols, num_alloc_cols, 0L).
 * 'alloc_cols' should be deallocated via free() once this is done.
 ****************************************************************************/
Bool GIF_To_XImage(disp, scrn, cmap, fname, quiet_p, err_string, ximage, num_alloc_cols, alloc_cols)
     Display*       disp;	/* IN: display */
     Screen*        scrn;	/* IN: screen */
     Colormap       cmap;	/* IN: colormap */
     char*          fname;	/* IN: filename of GIF */
     Bool           quiet_p;	/* IN: True if don't want debug info printed, else False */
     char*          *err_string;     /* OUT: if routine returns False, returns a string w/ error message */
     XImage*        *ximage;	     /* OUT: if routine returns True, returns XImage* of GIF */
     int            *num_alloc_cols; /* OUT: if routine returns True, gives the number of colors allocated by XAllocColor() */
     unsigned long* *alloc_cols;     /* OUT: if routine returns True, gives the array of pixel values alloc'd by XAllocColor() */
{
  int i, j;
  unsigned long* allocated_pixels_array;

  quiet = quiet_p;

  /* See xgif/xmain.c:ShowGif() */
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "if (!theDisp) OpenDisplay();" */
  theDisp = disp; 
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "theScreen = DefaultScreen(theDisp);" */
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "theCmap = DefaultColormap(theDisp, theScreen);" */
  theCmap = cmap;
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "rootW = RootWindow(theDisp, theScreen);" */
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "theGC = DefaultGC(theDisp, theScreen);" */
  theVisual = DefaultVisualOfScreen(scrn);
  dispcells = CellsOfScreen(scrn);
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "dispWIDE  = DisplayWidth(theDisp, theScreen);" */
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "dispHIGH  = DisplayHeight(theDisp, theScreen);" */
  /* from xgif/xmain.c:ShowGif()-->OpenDisplay(): "if (dispcells<=2) FatalError("This program requires a color display, pref. 8 bits.");" */
  if (dispcells<=2) {
    *err_string = "GIF images require a color display, preferably 8 bits.";
    return (False);
  }

  /* from xgif/xmain.c:ShowGif(): "theImage= NULL;" */
  *ximage = theImage = NULL;

  /* from xgif/xmain.c:ShowGif(): Open/Read the File */
  if (!LoadGIF(fname, err_string)) {
    /* *err_string set by LoadGIF() */
    return (False);
  }

  /* from xgif/xmain.c:ShowGif(): "ColorDicking();" */
  if (!ColorDicking(err_string)) { /* this figures out colors, then writes 'Image', which was allocated in LoadGIF()-->DecodeImage(). */
    /* *err_string set by ColorDicking() */
    XDestroyImage(theImage);	/* deallocate XImage allocated by LoadGIF()-->DecodeImage(),
				   XDestroyImage() deallocates theImage->data == Image */
    return (False);
  }

  /* At this point 'Image' array has been written, and 'XImage' "theImage" is valid... */
  /* from xgif/xmain.c:ShowGif(): "EventLoop();"
     --> on expose calls DrawWindow()
     --> XPutImage() to draw 'theImage' XImage onto window.
     For Xtango, we create the off-screen pixmap in Tango_GIF_Image_Class_Method_ISNEW(). */

  /* xgif/xmain.c:ShowGif(): "CloseGif();" to free 'Image' -- for Xtango,
     we call XDestroyImage() in Tango_GIF_Image_Class_Method_ISNEW() after creating the
     off-screen pixmap (since we no longer need a client side copy of the GIF); CloseGif()
     also deallocates any allocated colors -- for this routine, we return an array of 
     allocated colors which the caller must free via
     "XFreeColors(disp, colormap, alloc_cols, num_alloc_cols, 0L)." */
  if (!(allocated_pixels_array = (unsigned long *) malloc((unsigned) ((ColorMapSize + 1) * sizeof(unsigned long))))) {
    *err_string = "not enough memory for allocated_pixels_array";
    XDestroyImage(theImage);	/* deallocate XImage allocated by LoadGIF()-->DecodeImage(),
				   XDestroyImage() deallocates theImage->data == Image */
    return (False);
  }

  for (j = i = 0 ; (i < ColorMapSize) ; i++) {
    if (used[i] == ALLOCATED)
      allocated_pixels_array[j++] = cols[i];
  }

  *num_alloc_cols = j;
  *alloc_cols     = allocated_pixels_array;
  *ximage         = theImage;

#ifdef WINTERP_DEBUG_2
  fprintf(stdout, "GIF_To_XImage(): alloc'd %d colors, colormapsize=%d\n",
	  j, ColorMapSize);
  fflush(stdout);
#endif /* WINTERP_DEBUG_2 */

  return(True);
}
