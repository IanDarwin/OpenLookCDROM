
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

/* afft.h - header file for fft application */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/afft.h,v 1.6 1994/06/03 17:27:31 jg Exp $*/

#include    <stdio.h>
#include    <AF/AFlib.h>
#include    <tcl.h>
#include    <tk.h>
#include    "mycolor.h"

#define SPEAKER 1
  /* 
   * 0 - local phone device
   * 1 - local speaker/mic
   * 2 - hi-fi left channel
   * 3 - hi-fi right channel
   */

#define NCOLORS	    	32
#define SWATHHEIGHT 	64
#define BUFSIZE         512	/* buffer size, in samples */

#define	WIDTH	370        /* default window size (waterfall) */
#define	HEIGHT	256        /* default window height (waterfall) */
#define OSCWIDTH 256	   /* width of oscilloscope window */

#define MAXFRAMEHEIGHT	512
#define MAXFRAMEWIDTH	288   	/* Must be multiple of 4    */

    	    	    	    	/* Must be at least
				    framesize + frameheight / 2 */

#define	GRAYSCALE   	0   	/* Colors - grayscale	*/
#define	SPECTRAL    	1	/* Colors - full spectrum */
#define	LINEAR	    	2   	/* Colors - linear ramp	*/

#define	W_NONE	    	0   	/* Window function - none   	    	*/
#define	W_HAMMING   	1   	/* Window function - Hamming	    	*/
#define	W_HANNING   	2   	/* Window function - Hanning   	    	*/
#define	W_TRIANGULAR	3   	/* Window function - Triangular (Bartlett) */

#define	FBSIZE	    	65536	/* Size of File buffer	*/

/* Function Prototypes */

void	spectrogram 	(int);
void	waterfall   	(int);
int 	FFTRun	    	(ClientData, Tcl_Interp *, int, char **);
int     OSCRun	    	(ClientData, Tcl_Interp *, int, char **);

void	ClearData   	(void);
void	SetSource   	(void);
void	SetPeak     	(void);
void	SetWindow   	(void);
void    SetupOsc        (void);
void    updateColors    (void);

int 	FFTConfigure  	(ClientData, Tcl_Interp *, int, char **);
void	CreateGrays 	(void);
void	CreateRamp  	(void);
void	CreateSpectral	(void);

void	FileFFT	    	(void);
void    SetFileTime     (void);

void	fftinit	    	(int size);
void	cfft	    	(float data[], int npoints);
void	rfft	    	(float data[], int npoints);

/* Global Variables */

extern	FILE	    *soundfile;
extern	int 	    device;
extern	double	    gain;
extern	int	    reqgain;

extern	int 	    fftlength;
extern  int         stride;
extern  float       *oscbuf;
extern	float	    *afbuf;
extern	float	    *window;
extern  unsigned char *fbuff;

extern	int 	    color_flag;
extern	int 	    dc_flag;
extern	int 	    log_flag;
extern	int 	    max_flag;
extern	int 	    min_flag;
extern	int 	    sine_demo_flag;
extern	int 	    spectro_flag;
extern	int 	    print_power_flag;
extern	int 	    rt_flag;
extern	int 	    window_flag;
extern  int         scope_flag;

extern	int 	    reqmin;
extern	int 	    reqmax;
extern	int	    initmin;
extern  int         initmax;

extern	double	    minvalue;
extern	double	    maxvalue;

extern	int 	    iwidth;
extern	int 	    iheight;
extern  int         wblank;

/* AF-context variables */

extern	AC  	    ac;
extern	AC  	    acPlay;
extern	ATime	    atime;

/* X-context variables */

extern  Display	    *display;
extern	Window	    xw;

extern	Colormap    colormap;
extern	XColor	    colors [NCOLORS];
extern	unsigned    long    pixels [NCOLORS];
extern  Screen      *pScreen;
extern  Visual      *pVisual;
extern  unsigned int visualClass;

/* TK-context variables */

Tk_Window   mainWindow;	    /* Main Window of the Application	*/
