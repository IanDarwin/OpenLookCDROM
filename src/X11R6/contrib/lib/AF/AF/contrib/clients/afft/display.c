/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* display.c - Display functions for realtime fft program */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/display.c,v 1.13 1994/06/03 17:27:31 jg Exp $*/

#include    "afft.h"

#include    <X11/Xlib.h>
#include    <math.h>
#include    <string.h>
#include    <assert.h>
#include    <AF/AFUtils.h>

int ssize;                    /* sample size, in bytes */
int phase = 0;
double peakvalue;             /* autoscaling of log amplitude */
double inputpeak;             /* audio device format peak value */

int	oscwidth;
int	osccycle;
int	oscorigin;

#define FALSE 0
#define min(a, b) ((a > b) ? b : a)
#define max(a, b) ((a < b) ? b : a)
#define LIMIT(l, v, h) (max(l, min(v, h)))

int wwidth = WIDTH;
int wheight= HEIGHT;
int wblank = 0;

int fftlength = BUFSIZE;   /* length of transform */
int stride = BUFSIZE;	   /* amount to skip ahead on each cycle */
short *ainbuf;          /* audio buffer (short) */
float *oscbuf;		   /* buffer for oscilloscope display */
float *afbuf;              /* in place real fft */
float *window;             /* windowing function, scaling rolled in */
unsigned char *fbuff;	   /* buffer for reading from file */
int framesize = 256;
int frameheight = 64;
unsigned char *imdata;     /* array of data, scaled to 0..31 */
unsigned char *scdata;     /* image memory */
unsigned char *oscdata;    /* oscilloscope image memory */
unsigned int *framecolors;
unsigned char *stripedata;

Display	*xdp;
Display *display;
int     screen;
Window	xw;
Window  oscw;
GC	xgc;
Colormap colormap;
unsigned long pixels[NCOLORS];
XColor colors [NCOLORS];
XImage *image;
XImage *oscimage;
XImage *vstripe;
Screen *pScreen;
Visual *pVisual;
unsigned int   visualClass;
int            bytesPerPixel;
int            bitsPerPixel;
unsigned long  blackPixel;
unsigned long  whitePixel;

AC ac = NULL;
AC acPlay = NULL;
AFAudioConn *aud;
ATime atime;

static	void	SetupFFT (Window);
static void pixelFill (caddr_t buffer, unsigned int pixel, int count);
static void pixelSet (caddr_t buffer, unsigned int pixel, int pixIndex);

/*
 * TCL command procedure that runs the realtime FFT display
 *
 * Called after initial parameter are set up and the window
 *   has been mapped
 *
 * Returns only after the window hierarchy is destroyed in response
 *   to the "Exit" button
 *
 */

int
FFTRun	    	(ClientData data, Tcl_Interp *interp, int argc, char **argv)

{
    Window  	theWindow;
    int	    	iter;

    display = Tk_Display ((Tk_Window) data);
#ifdef DEBUG
    XSynchronize (display, 1);
#endif
    screen = Tk_ScreenNumber ((Tk_Window) data);
    Tcl_GetInt (interp, argv [1], (int *) &theWindow);
    SetupFFT (theWindow);
    iter = 1;
    if (soundfile == NULL) {
        while (tk_NumMainWindows > 0) {
    	    iter += 1;
	    if (spectro_flag) {
    	    	getdata (iter);
	    	spectrogram (iter);
	    } else {
    	    	getdata (iter);
	    	waterfall (iter);
	    }
    	    while (Tk_DoOneEvent (1) != 0) {
	    	;
    	    }
    	}
    } else {
    	FileFFT();
    }
    return TCL_OK;
}

static void
SetupFFT    (Window theWindow)

{
    int	    i, stat;

  SetSource();
  if((ainbuf = (short *) malloc(16384*ssize)) == NULL) {
    fprintf(stderr, "Couldn't allocate record buffer\n");
    exit(1);
  }
  if ((imdata = (unsigned char *) malloc(framesize)) == NULL) {
    fprintf(stderr, "Couldn't allocate imdata\n");
    exit(1);
  }
  if ((framecolors = (unsigned int *) 
       malloc(frameheight*sizeof(unsigned int))) == NULL) {
    fprintf(stderr, "Couldn't allocate framecolors\n");
    exit(1);
  }
  if ((oscbuf = (float *) malloc(BUFSIZE * sizeof(float))) == NULL) {
    fprintf(stderr, "Couldn't allocate oscbuf\n");
    exit(1);
  }
  if ((afbuf = (float *) malloc(BUFSIZE * sizeof(float))) == NULL) {
    fprintf(stderr, "Couldn't allocate afbuf\n");
    exit(1);
  }
  if ((fbuff = (unsigned char *)
       malloc (FBSIZE)) == NULL) {
    fprintf (stderr, "Couldn't allocate file buffer\n");
    exit (1);
  }
  if ((window = (float *) malloc(BUFSIZE * sizeof(float))) == NULL) {
    fprintf(stderr, "Couldn't allocate window\n");
    exit(1);
  }

  if (display == (Display *) NULL)
  {
    printf("XOpenDisplay returned 0\n");
    abort();
  }
  pScreen = XScreenOfDisplay (display, screen);
  pVisual = XDefaultVisual (display, screen);
  colormap = DefaultColormap (display, screen);
/*  colormap = DefaultColormap (display, DefaultScreen(display)); */
  visualClass = pVisual->class;
  bitsPerPixel = XDefaultDepth (display, screen);
  if ( bitsPerPixel > 16 ) bitsPerPixel = 32;
  else if ( bitsPerPixel > 8 ) bitsPerPixel = 16;
  else bitsPerPixel = 8;
  bytesPerPixel = (bitsPerPixel >> 3);
  if (visualClass == GrayScale || visualClass == StaticGray)
  {
    color_flag = 0;
  }

  if (visualClass == PseudoColor || visualClass == GrayScale)
  {
    stat = XAllocColorCells(display, colormap, 0, (void *) NULL, 0, pixels, NCOLORS);
    if (stat == 0)
    {
      printf("XAllocColorCells returned 0\n");
      abort();
    }
    for (i = 0; i < NCOLORS; i++) {
      colors[i].pixel = pixels[i];
      colors[i].flags = DoRed + DoGreen + DoBlue;
      colors[i].pad = 0;
    }

  }

  if ((stripedata = (unsigned char *) 
       malloc(frameheight * bytesPerPixel)) == NULL) {
    fprintf(stderr, "Couldn't allocate stripedata\n");
    exit(1);
  }

  if (color_flag) {
    CreateSpectral ();
  } else {
    CreateGrays ();
  }

  for (i = 0; i < frameheight; i += 1)
  {
    framecolors[i] = pixels[(i * NCOLORS) / frameheight];
    pixelSet ((caddr_t) stripedata, framecolors[i], frameheight - i);
  }

  blackPixel = XBlackPixel (display, screen);
  whitePixel = XWhitePixel (display, screen);

  xw = theWindow;
  xgc = XCreateGC(display, XDefaultRootWindow(display), 0, NULL);
  XSetGraphicsExposures(display, xgc, FALSE);
  XSetFunction(display, xgc, GXcopy);
  XSetWindowColormap (display, xw, colormap);
  XSetForeground (display, xgc, pixels[NCOLORS - 1]);
  XSetBackground (display, xgc, pixels[0]);
  XSetWindowBackground (display, xw, pixels [0]);
  XClearWindow (display, xw);

  if (spectro_flag)
  {
    iwidth = 2;
    iheight = MAXFRAMEHEIGHT;
  }
  else
  {
    iwidth = MAXFRAMEWIDTH;
    iheight = SWATHHEIGHT;
  }
  scdata = (unsigned char *) malloc(MAXFRAMEWIDTH * MAXFRAMEHEIGHT * bytesPerPixel);
  image = XCreateImage(display, pVisual, XDefaultDepth (display, screen),
    ZPixmap, 0, (char *) scdata, MAXFRAMEWIDTH, MAXFRAMEHEIGHT, 32, 0);

  vstripe = XCreateImage(display, pVisual, XDefaultDepth (display, screen),
    ZPixmap, 0, (char *) stripedata, 1, frameheight, 8, 1);

  oscdata = (unsigned char *) malloc (OSCWIDTH * 32 * bytesPerPixel);
  oscimage = XCreateImage(display, pVisual, XDefaultDepth (display, screen),
    ZPixmap, 0, (char *) oscdata, OSCWIDTH, 32, 32, 0);

  ClearData();
  SetPeak();
  SetWindow();
  SetupOsc();
  if (ac != NULL) atime = AFGetTime(ac);

  updateColors ();
}

/* return magnitude of floating point complex number */

double mag(float *c)
{
  double a, b;
  a = c[0];
  a = a * a;
  b = c[1];
  b = b * b;
  return( sqrt(a+b));
}

/* compute spectrum of one frame */

void
ComputeSpectrum	(int limit)

{
  double    val, top;
  int	    i, j, lim, k, span;

  rfft (afbuf, fftlength);
  if (!dc_flag)
  {
    afbuf[0] = 0.0;
  }
  top = (double) (limit - 1) / (maxvalue - minvalue);
  lim = fftlength / 2;
  span = 256 / lim;
  j = 0;
  for (i = 0; i < lim; i += 1)
  {
    val = mag(&afbuf[2*i]);
    if (log_flag)
    {
      val = max(0.0000000001, val);
      val = ((log10(val) - peakvalue) * (- gain)) +3.0;  /* dB down from peakvalue */
    } else {
      val = val / peakvalue;
    }
    val = top * (LIMIT(minvalue, val, maxvalue) - minvalue);
    assert(val >= 0);
    assert(val < limit);
    for (k = 0; k < span; k++) {
      imdata[j++] = val;
    }
  }
}

void
ClearData()

{
  register  unsigned  int    color;

  color = framecolors [0];
  pixelFill ((caddr_t) scdata, color, MAXFRAMEHEIGHT * MAXFRAMEWIDTH);
}

void
SetPeak()

{
    int	    i;

  /* build a full amplitude sine wave, to measure peak fft value */
  for (i = 0; i < fftlength; i += 1)
  {
    afbuf[i] = sin((6.2818 * 8.0 * (double) i) / (double) fftlength) ;
  }
  rfft (afbuf, fftlength);
  peakvalue = 0.00000000001;
  for (i = 0; i < fftlength / 2; i += 2)
    peakvalue = max(peakvalue,  mag(&afbuf[i]));
  if (log_flag) peakvalue = log10(peakvalue);
/*  printf("peak = %lf\n", peakvalue);	*/
}

void
SetWindow()

{
    int	    i;
    double  cosine_term;
    double  scale_term;

    switch (window_flag) {
    	case W_NONE:
    	    for (i = 0; i < fftlength; i += 1) {
      	    	window[i] = 1.0 / inputpeak;
    	    }
	    break;

    	case W_HAMMING:
    	    for (i = 0; i < fftlength; i += 1) {
	    	cosine_term = cos((2.0 * M_PI * (double) i) /(double) (fftlength-1));
      	    	window[i] = (0.54 - 0.46 * cosine_term ) / inputpeak;
    	    }
	    break;

    	case W_HANNING:
    	    for (i = 0; i < fftlength; i += 1) {
	    	cosine_term = cos((2.0 * M_PI * (double) i) /(double) (fftlength-1));
      	    	window[i] = (0.50 - 0.50 * cosine_term ) / inputpeak;
    	    }
	    break;

    	case W_TRIANGULAR:
	    scale_term = (double) (fftlength-1) * inputpeak;
	    for (i = 0; i < fftlength; i += 1) {
	    	if (i < fftlength / 2) {
	    	    window[i] = 2.0 * (double) i / scale_term;
		} else {
	    	    window[i] = 2.0 * (double) (fftlength - i) / scale_term;
		}
	    }
	    break;
    }
}

getdata(int lc)
{
  double top;
  int i;
  float *afp, *wfp, *ofp;
  unsigned char *abp;
  ATime ctime;
  if (sine_demo_flag)
  {
    lc %= 200;
    if (lc >= 100) lc = 199 - lc;
    top = 6.28 * 20.0 * ((double) lc);
    for (i = 0; i < fftlength; i += 1)
    {
      oscbuf [i] = (float) sin( (top * ((double) i) ))
        * (double) inputpeak; 
      afbuf[i] = oscbuf [i] * window[i];
    }
  }
  else
  {
    if (!rt_flag) {
	ctime = AFGetTime(ac);
	if ((atime + 2000) < ctime) {	/* if seriously behind ... */
	    atime = ctime;		/* catch up */
	}
    }
    if (stride < fftlength) {
	memmove ((char *) ainbuf, (char *) &ainbuf [stride], 
		(fftlength - stride) * ssize);
	AFRecordSamples (ac, atime - stride, stride * ssize, 
		(unsigned char *)&ainbuf [fftlength - stride], ABlock);
	atime += stride;
    } else {
	AFRecordSamples (ac, atime - fftlength, fftlength * ssize, 
		(unsigned char *)ainbuf, ABlock);
	atime += fftlength;
    }
    ofp = &oscbuf[0];
    afp = &afbuf[0];
    wfp = &window[0];
    if (print_power_flag) printf("pwr %lf\n", 
			AFPower((unsigned char *) ainbuf, fftlength, LIN16));
    for (i = 0; i < fftlength; i += 1)
    {
      *ofp = (float) ainbuf[i];
      *afp++ =  *ofp++ * *wfp++;
    }
  }
}

void SetupOsc()

{
    if (sine_demo_flag) {
	osccycle = 1;
    } else {
	osccycle = BUFSIZE / fftlength;
    }
    oscwidth = min (fftlength, 256);
    oscorigin = (WIDTH - oscwidth) / 2;
    XClearWindow (display, oscw);
}

/* Do the oscilloscope display */

void oscilloscope ()

{
    int  dotpos;
    int  i, j;
    int  width;
    static int  cycle = 0;

    if (--cycle <= 0) {
	cycle = osccycle;
	pixelFill ((caddr_t) oscdata, whitePixel, 32 * OSCWIDTH);
	for (i = 0; i < oscwidth; i++) {
	    dotpos = 16 - 15.0 * oscbuf [i] / inputpeak;
	    if (dotpos >= 0 && dotpos < 32) {
		pixelSet ((caddr_t) oscdata, blackPixel, i + dotpos * OSCWIDTH);
	    }
	}
	XPutImage (display, oscw, xgc, oscimage, 0, 0, oscorigin, 0, oscwidth, 32);
    }
}

/* Do spectrogram style display */

void spectrogram (int lc)
{
  int row;

  if (scope_flag) oscilloscope();
  ComputeSpectrum (NCOLORS);
  for (row = 0; row < framesize; row += 1)
  {
    pixelSet ((caddr_t) scdata, pixels[imdata[(framesize - 1) - row]],
	      (row * MAXFRAMEWIDTH));
  }
  /* copy the new swatch onto the screen */
  /* args are srcx, srcy, dstx, dsty, width, height */
  XPutImage(display, xw, xgc, image, 0, 0, lc % wwidth, 0, 
    1, framesize);
}

/* Do waterfall style display */

void waterfall (int lc)

{
  int row, col, rowidx, llim, hlim;
  unsigned char *from, *to;
  int framestart;
  int count;

    if (scope_flag) oscilloscope();


/*  what to do:
 *
 * 1.  blt the old image up and to the right
 * 2.  compute the new spectrum
 * 3.  copy the new spectrum to the bottom of the image
 *
 */

/* blt the screen, including top 2 lines of the frame,
    up and to the right */

/****
  XCopyArea (display, xw, xw, xgc, 0, 2,
    wwidth - 1, wheight - frameheight + 1, 1, 0);
****/

  XCopyArea (display, xw, xw, xgc, 0, wblank + 2,
    wwidth - 1, wheight - frameheight + 1 - wblank, 1, wblank);

/* compute new spectrum */

  ComputeSpectrum (frameheight);  

    /* build the new image swath */
    /* copy the existing screen up and over by 2, 1 */

    framestart = (wheight - frameheight) * iwidth;

    from = &scdata[2 * iwidth * bytesPerPixel];
    to = &scdata[1 * bytesPerPixel];
    count = (frameheight - 2) * iwidth * bytesPerPixel;
    bcopy ((char *) from, (char *) to, count);

    /* clear out the first column */

    pixelSet ((caddr_t) scdata, framecolors [0], 0);
    pixelSet ((caddr_t) scdata, framecolors [0], iwidth);

    /* draw in the new data, bottom  up in each column */

    for (col = 0; col < framesize; col += 1)
    {
      to = &scdata[(((frameheight - 1) * iwidth) + col) * bytesPerPixel];
      count = imdata[col];
      for (row = 0; row <= count; row += 1)
      {
	pixelSet ((caddr_t) to, framecolors[row], 0);
        to -= (iwidth * bytesPerPixel);
      }
    }
    XPutImage(display, xw, xgc, image, 0, 0, 0, wheight - frameheight, 
      iwidth, frameheight);
}

/*
 * Find a suitable default device (the first device not connected to the phone),
 *  returns device number.
 *
 * Returns -1 if no suitable device can be found.
 */
int FindDefaultDevice(AFAudioConn *aud)
{
        AFDeviceDescriptor *aDev;
        int     i;

        for(i=0; i<ANumberOfAudioDevices(aud); i++) {
                aDev = AAudioDeviceDescriptor(aud, i);
                if(aDev->inputsFromPhone == 0 && aDev->outputsToPhone == 0
		   && aDev->recNchannels == 1)
                        return i;
        }
        return -1;
}

void
SetSource()

{
    AFSetACAttributes attributes;

    if (ac == NULL) {
	if ((aud = AFOpenAudioConn ("")) != NULL) {
	    if(device < 0)
		device = FindDefaultDevice(aud);

	    attributes.rec_gain =  0;
	    attributes.type =  LIN16;

	    ac = AFCreateAC (aud, device, ACRecordGain | ACEncodingType,
		         &attributes);
	    AFSync(aud, 0);

	    attributes.play_gain =  0;
	    attributes.type =  MU255;
	    acPlay = AFCreateAC (aud, device, ACPlayGain | ACEncodingType,
		         &attributes);
	    AFSync(aud, 0);
	}
    }
    if (sine_demo_flag) {
	ssize = 2;
    } else if (soundfile != NULL) {
	ssize = 1;
    } else {
	if (ac != NULL) {
	    ssize = AF_sample_sizes[LIN16].bytes_per_unit;
	} else {
	    fprintf (stderr, "can't open audio connection\n");
	    exit (1);
	}
    }
    if (ssize == 1) {
	inputpeak = 8031.0;
    } else {
	inputpeak = 32768.0;
    }
}

int
OSCRun	    	(ClientData data, Tcl_Interp *interp, int argc, char **argv)

{
    Tcl_GetInt (interp, argv [1], (int *) &oscw);
    return TCL_OK;
}

static void
pixelFill (caddr_t buffer, unsigned int pixel, int count)
{
  int i;

  switch (bytesPerPixel) {
  case 1:
  {
    unsigned char *data = (unsigned char *) buffer;

    for ( i = 0; i < count; i++ )
    {
      *data++ = (unsigned char) pixel;
    }
  }
  break;

  case 2:
  {
    unsigned short *data = (unsigned short *) buffer;

    for ( i = 0; i < count; i++ )
    {
      *data++ = (unsigned short) pixel;
    }
  }
  break;

  case 4:
  {
    unsigned int *data = (unsigned int *) buffer;

    for ( i = 0; i < count; i++ )
    {
      *data++ = (unsigned int) pixel;
    }
  }
  break;

  default:
    abort();
  }
}

static void
pixelSet (caddr_t buffer, unsigned int pixel, int pixIndex)
{
  switch (bytesPerPixel) {
  case 1:
  {
    unsigned char *data = (unsigned char *) buffer;

    data[pixIndex] = (unsigned char) pixel;
  }
  break;

  case 2:
  {
    unsigned short *data = (unsigned short *) buffer;

    data[pixIndex] = (unsigned short) pixel;
  }
  break;

  case 4:
  {
    unsigned int *data = (unsigned int *) buffer;

    data[pixIndex] = (unsigned int) pixel;
  }
  break;

  default:
    abort();
  }
}

void
updateColors (void)
{
  int i;

  if (visualClass == PseudoColor || visualClass == GrayScale)
  {
    XStoreColors (display, colormap, colors, NCOLORS);
  } else {
    for (i = 0; i < NCOLORS; i++) {
      colors[i].flags = DoRed + DoGreen + DoBlue;
      colors[i].pad = 0;
      XAllocColor (display, colormap, &colors[i]);
      pixels[i] = colors[i].pixel;
    }

    for (i = 0; i < frameheight; i += 1)
    {
      framecolors[i] = pixels[(i * NCOLORS) / frameheight];
      pixelSet ((caddr_t) stripedata, framecolors[i], frameheight - i);
    }

    XSetForeground(display, xgc, pixels[NCOLORS - 1]);
    XSetBackground(display, xgc, pixels[0]);
    XSetWindowBackground (display, xw, pixels [0]);
  }
  ClearData();
  XClearWindow (display, xw);
}

