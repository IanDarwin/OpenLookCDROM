/* jpeg.c - routines for reading and writing jpeg images */

/* Copyright Carnegie Mellon University, 1993 1994 - All rights reserved */

/*
 * This file is derived from a file with the following copyrights:

 ****************************************************************
 * Portions Copyright 1989, 1990, 1991, 1992 by John Bradley and
 *                                The University of Pennsylvania
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

#ifdef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/jpeg.c,v 1.16 1994/03/17 20:23:23 wjh Exp $";
#endif

#include <andrewos.h>
#include <setjmp.h>
#include <image.ih>
#include <jpeg.eh>
#include <jinclude.h>

static int pWIDE, pHIGH;
static byte r[256],g[256],b[256];
static byte *pic = NULL;

static void jselwxv();
static int writeJFIF();

static int colorType, numColors, quality;
static byte *image8, *image24;

static byte *CurImagePtr;
static byte *pic24 = NULL;
static long filesize;
static jmp_buf jmpState;
static external_methods_ptr emethods;

long
jpeg__WriteNative(jpeg, file, filename)
    struct jpeg *jpeg;
    FILE *file;
    char *filename;
{
    FILE *fp;
    int i, nc, rv, w, h, size;
    register byte *ip, *ep, *inpix;

  /* get the image into a format that the JPEG software can grok on.  Also, open the output file, so we don't waste time doing this format conversion if we won't be able to write it out */

    pWIDE = pHIGH = 0;
    pic = image24 = pic24 = CurImagePtr = NULL;

    pWIDE = jpeg_Width(jpeg);
    pHIGH = jpeg_Height(jpeg);
    colorType = jpeg_Type(jpeg);
    numColors = nc = jpeg_RGBUsed(jpeg);
  /* see if we can open the output file before proceeding */
    if(!(fp = file) &&
       !(fp = fopen(filename, "w"))) {
	printf("jpeg: couldn't open file %s\n", filename);
       return(-1);
    }

   switch(colorType) {
	case IBITMAP:
	    jpeg_Bit2Grey(jpeg);
	case IGREYSCALE:
	    image8 = jpeg_Data(jpeg);
	    break;
	case IRGB:
	    jpeg_Expand(jpeg);
	    image24 = jpeg_Data(jpeg);
	    break;
	case ITRUE:
	    image24 = jpeg_Data(jpeg);
	    break;
    }
    colorType = jpeg_Type(jpeg);
    quality = jpeg->saveQuality;
  /* in any event, we've got some valid image.  Do the JPEG Thing */
    rv = writeJFIF(fp);
    
    if(!file && fp)
	fclose(fp);
    return(rv);
}


/*********************************************/
/**** INTERFACE CODE FOR THE JPEG LIBRARY ****/
/*********************************************/

/********* JPEG DECOMPRESSION FUNCTIONS **********/

/**************************************************/
static void xv_jpeg_monitor(cinfo, loopcnt, looplimit)
  decompress_info_ptr cinfo;
  long loopcnt, looplimit;
{
#ifdef FOO  
  int a,b;
  double percent;

  a = cinfo->completed_passes;
  b = cinfo->total_passes;

  percent = ((a + ((double) loopcnt / looplimit)) / (double) b) * 100.0;

  fprintf(stderr,"jpeg: %lf done.  loop: %ld, %ld  pass: %d, %d\n",
	  percent, loopcnt, looplimit, a, b);
#endif
}

static void
d_ui_method_selection(cinfo)
  decompress_info_ptr cinfo;
{
  /* select output colorspace & quantization parameters */
  if (cinfo->jpeg_color_space == CS_GRAYSCALE) {
      int i;
      cinfo->out_color_space = CS_GRAYSCALE;
      cinfo->quantize_colors = FALSE;
  }
  else {
      cinfo->out_color_space = CS_RGB;
  }

  jselwxv(cinfo);
}


/**************************************************/
static void
output_init (cinfo)
  decompress_info_ptr cinfo;
{
  pWIDE = cinfo->image_width;
  pHIGH = cinfo->image_height;

  if (cinfo->out_color_space == CS_GRAYSCALE || 
      cinfo->quantize_colors == TRUE) {
    pic = (byte *) malloc(pWIDE * pHIGH);
    CurImagePtr = pic;
  }
  else {
    pic24 = (byte *) malloc(pWIDE * pHIGH * 3);
    CurImagePtr = pic24;
  }
}


/**************************************************/
static void
put_color_map (cinfo, num_colors, colormap)
  decompress_info_ptr cinfo;
  int num_colors;
  JSAMPARRAY colormap;
{
  int i;

  numColors = num_colors;
  for (i = 0; i < num_colors; i++) {
      r[i] = GETJSAMPLE(colormap[0][i]);
      g[i] = GETJSAMPLE(colormap[1][i]);
      b[i] = GETJSAMPLE(colormap[2][i]);
  }
}

static void
put_pixel_rows (cinfo, num_rows, pixel_data)
  decompress_info_ptr cinfo;
  int num_rows;
  JSAMPIMAGE pixel_data;
{
  JSAMPROW ptr0, ptr1, ptr2;
  long col;
  long width = cinfo->image_width;
  int row;
  static unsigned int totrows = 0;

  if (cinfo->out_color_space == CS_GRAYSCALE || 
      cinfo->quantize_colors == TRUE) {
      for (row = 0; row < num_rows; row++) {
	  ptr0 = pixel_data[0][row];
	  for (col = width; col > 0; col--) {
	      *CurImagePtr++ = GETJSAMPLE(*ptr0++);
	  }
	  totrows++;
      }
  }
  else {
      for (row = 0; row < num_rows; row++) {
	  ptr0 = pixel_data[0][row];
	  ptr1 = pixel_data[1][row];
	  ptr2 = pixel_data[2][row];
	  for (col = width; col > 0; col--) {
	      *CurImagePtr++ = GETJSAMPLE(*ptr0++);
	      *CurImagePtr++ = GETJSAMPLE(*ptr1++);
	      *CurImagePtr++ = GETJSAMPLE(*ptr2++);
	  }
	  totrows++;
      }
  }
}

static void output_term (cinfo)
     decompress_info_ptr cinfo;
{
}

static void jselwxv(cinfo)
     decompress_info_ptr cinfo;
{
  cinfo->methods->output_init = output_init;
  cinfo->methods->put_color_map = put_color_map;
  cinfo->methods->put_pixel_rows = put_pixel_rows;
  cinfo->methods->output_term = output_term;
}

static void JPEG_Message (msgtext)
  char *msgtext;
{
  char tempstr[200];

  sprintf(tempstr, msgtext,
	  emethods->message_parm[0], emethods->message_parm[1],
	  emethods->message_parm[2], emethods->message_parm[3],
	  emethods->message_parm[4], emethods->message_parm[5],
	  emethods->message_parm[6], emethods->message_parm[7]);
}


/**************************************************/
static void JPEG_Error (msgtext)
  char *msgtext;
{
  char tempstr[200];
  
  sprintf(tempstr, msgtext,
	  emethods->message_parm[0], emethods->message_parm[1],
	  emethods->message_parm[2], emethods->message_parm[3],
	  emethods->message_parm[4], emethods->message_parm[5],
	  emethods->message_parm[6], emethods->message_parm[7]);
  fprintf(stderr, "%s\n", tempstr);
  (*emethods->free_all) ();	/* clean up memory allocation */
  longjmp(jmpState,1);
}

/*******************************************/
int
jpeg__Load( jpeg, fullname, fp )
  struct jpeg *jpeg;
  char *fullname;
  FILE *fp;
/*******************************************/
{
  FILE *f;
  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open gif file %s.\n", fullname);
	  return(-1);
      }
  }
  return(LoadJFIF(jpeg, fullname, f));
}

/*******************************************/
int LoadJFIF(jpeg, fname, f)
  struct jpeg *jpeg;
  char *fname;
  FILE *f;
{
  int rtval;
  /* These three structs contain JPEG parameters and working data.
   * They must survive for the duration of parameter setup and one
   * call to jpeg_decompress; typically, making them local data in the
   * calling routine is the best strategy.
   */
  struct Decompress_info_struct cinfo;
  struct Decompress_methods_struct dc_methods;
  struct External_methods_struct e_methods;

  numColors = 0;
  pWIDE = pHIGH = 0;
  pic = image8 = image24 = pic24 = CurImagePtr = NULL;
  /* Set up the input file */

  if((cinfo.input_file = f) == NULL && fname &&
       (cinfo.input_file = fopen(fname, "r")) == NULL)
      return(-1);

  /* figure out the file size (for Informational Purposes Only) */
  fseek(cinfo.input_file, 0L, 2);
  filesize = ftell(cinfo.input_file);
  fseek(cinfo.input_file, 0L, 0);

  /* Set up longjmp for error recovery out of JPEG_Error */
  if(setjmp(jmpState)) {
    fclose(cinfo.input_file);	/* close input file */
    return(-1);		/* no further cleanup needed */
  }

  cinfo.output_file = NULL;	/* if no actual output file involved */

  /* Initialize the system-dependent method pointers. */
  cinfo.methods  = &dc_methods;
  cinfo.emethods = &e_methods;
  emethods = &e_methods;	/* save struct addr for possible access */

  e_methods.error_exit = JPEG_Error; /* provide my own error/message rtns */
  e_methods.trace_message = JPEG_Message;
  e_methods.trace_level = 0;	/* no tracing, thank you */
  e_methods.num_warnings = 0;	/* no warnings emitted yet */
  e_methods.first_warning_level = 0; /* display first corrupt-data warning */
  e_methods.more_warning_level = 3; /* but suppress additional ones */
  jselmemmgr(&e_methods);	/* memory allocation routines */
  dc_methods.d_ui_method_selection = d_ui_method_selection;

  /* Set up default JPEG parameters. */
  j_d_defaults(&cinfo, TRUE);

  /* set up our progress-monitoring function */
  cinfo.methods->progress_monitor = xv_jpeg_monitor;
  
  /* Set up to read a JFIF or baseline-JPEG file. */
  /* A smarter UI would inspect the first few bytes of the input file */
  /* to determine its type. */
  jselrjfif(&cinfo);

  /* Do it! */
  jpeg_decompress(&cinfo);

  if (pic24) {
      jpeg_newTrueImage(jpeg, cinfo.image_width, cinfo.image_height);
      bcopy(pic24, jpeg_Data(jpeg), 3 * cinfo.image_width * cinfo.image_height);
      free(pic24);
  }
  else {
      unsigned int i, w = cinfo.image_width, h = cinfo.image_height;

      if(cinfo.out_color_space == CS_GRAYSCALE)
	  jpeg_newGreyImage(jpeg, w, h, cinfo.data_precision);
      else
	  jpeg_newRGBImage(jpeg, w, h, cinfo.data_precision);
      bcopy(pic, jpeg_Data(jpeg), w * h);
      if(cinfo.out_color_space == CS_GRAYSCALE) {
	  jpeg_RGBUsed(jpeg) = jpeg_RGBSize(jpeg) = numColors = 256;
	  for(i = 0; i < numColors-1; i++)
	      jpeg_RedPixel(jpeg, i) =
		jpeg_GreenPixel(jpeg, i) =
		jpeg_BluePixel(jpeg, i) = i << 8;
      }
      else {
	  for(i = 0; i < numColors; i++) {
	      jpeg_RedPixel(jpeg, i) = r[i] << 8;
	      jpeg_GreenPixel(jpeg, i) = g[i] << 8;
	      jpeg_BluePixel(jpeg, i) = b[i] << 8;
	  }
	  jpeg_RGBUsed(jpeg) = jpeg_RGBSize(jpeg) = numColors;
      }
      free(pic);
  }
  /* Close input file */
  fclose(cinfo.input_file);

  /* Got it! */
  return 0;
}


/********* JPEG COMPRESSION FUNCTIONS **********/

/**************************************************/
static void c_ui_method_selection(cinfo)
     compress_info_ptr cinfo;
{
  /* If the input is gray scale, generate a monochrome JPEG file. */
  if (cinfo->in_color_space == CS_GRAYSCALE)
    j_monochrome_default(cinfo);
}


/**************************************************/
static void input_init (cinfo)
     compress_info_ptr cinfo;
{
  int w,h;
  if (colorType == IGREYSCALE) {
    cinfo->input_components = 1;
    cinfo->in_color_space = CS_GRAYSCALE;
    CurImagePtr = image8;
  }
  else {
    cinfo->input_components = 3;
    cinfo->in_color_space = CS_RGB;
    CurImagePtr = image24;
  }

  w = pWIDE;  h = pHIGH;

  cinfo->image_width = w;
  cinfo->image_height = h;
  cinfo->data_precision = 8;
}


/**************************************************/
static void get_input_row(cinfo, pixel_row)
     compress_info_ptr cinfo;
     JSAMPARRAY        pixel_row;
{
  JSAMPROW ptr0, ptr1, ptr2;
  long col;
  static unsigned int totrows = 0;

  if (cinfo->input_components == 1) {
    ptr0 = pixel_row[0];
    for (col = cinfo->image_width; col > 0; col--) {
      *ptr0++ = *CurImagePtr++;
    }
    totrows++;
  }
  else {
    ptr0 = pixel_row[0];
    ptr1 = pixel_row[1];
    ptr2 = pixel_row[2];
    for (col = cinfo->image_width; col > 0; col--) {
      *ptr0++ = *CurImagePtr++;
      *ptr1++ = *CurImagePtr++;
      *ptr2++ = *CurImagePtr++;
    }
    totrows++;
  }
}


/**************************************************/
static void input_term (cinfo)
     compress_info_ptr cinfo;
{
  /* no work required */
}


/**************************************************/
static void jselrxv(cinfo)
     compress_info_ptr cinfo;
{
  cinfo->methods->input_init = input_init;
  cinfo->methods->get_input_row = get_input_row;
  cinfo->methods->input_term = input_term;
}



/*******************************************/
static int writeJFIF(fp)
  FILE *fp;
{
  int retval;
  struct Compress_info_struct cinfo;
  struct Compress_methods_struct c_methods;
  struct External_methods_struct e_methods;

  
  /* Initialize the system-dependent method pointers. */
  cinfo.methods  = &c_methods;
  cinfo.emethods = &e_methods;

  /* Set up longjmp for error recovery out of JPEG_Error */
  if(retval = setjmp(jmpState))
    return retval;		/* no further cleanup needed */

  e_methods.error_exit = JPEG_Error; /* provide my own error/message rtns */
  e_methods.trace_message = JPEG_Message;
  e_methods.trace_level = 0;	/* no tracing, thank you */
  jselmemmgr(&e_methods);	/* memory allocation routines */
  c_methods.c_ui_method_selection = c_ui_method_selection;

  /* set up our progress-monitoring function */
  cinfo.methods->progress_monitor = xv_jpeg_monitor;
  
  /* Select input and output */
  cinfo.input_file  = NULL;
  cinfo.output_file = fp;       /* already open */

  j_c_defaults(&cinfo, quality, FALSE);
  jselrxv(&cinfo);		/* we'll be reading from memory */
  jselwjfif(&cinfo);		/* and writing to JFIF file format */

  /* Do it! */
  jpeg_compress(&cinfo);
  
  return 0;
}

long
jpeg__Read( self, file, id )
    struct jpeg *self;
    FILE *file;
    long id;
{
    if(jpeg_Load(self, NULL, file) == 0) {
	jpeg_Compress(self);
	return(dataobject_NOREADERROR);
    }
    else
	return(dataobject_BADFORMAT);
}

long
jpeg__Write( self, file, writeID, level )
    struct jpeg *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

int 
jpeg__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{
    FILE *f;
    int status = 0;
    boolean ret = FALSE;

    if(f = fopen(fullname, "r")) {
	unsigned char c1, c2, c3, c4;
	unsigned char jfif[5];

	if(fscanf(f, "%c%c%c%c", &c1, &c2, &c3, &c4) == 4 &&
	   c1 == 0xFF && c2 == 0xD8 && c3 == 0xFF && c4 == 0xE0) {
	    getc(f); getc(f);
	    if((status = fscanf(f, "%4s", jfif)) == 1) {
		jfif[4] = (char)0;
		if(!strncmp(jfif, "JFIF", 4)) ret = TRUE;
	    }
	}
	fclose(f);
    }
    return(ret);
}

boolean
jpeg__InitializeObject( classID, self )
    struct classheader *classID;
    struct jpeg *self;
{
    self->saveQuality = 75;
    return(TRUE);
}

void
jpeg__FinalizeObject( classID, self )
    struct classheader *classID;
    struct jpeg *self;
{}
