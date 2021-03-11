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

/* configure.c - interactive configuration for realtime fft program */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/configure.c,v 1.4 1994/06/03 17:27:31 jg Exp $*/

#include    "afft.h"

#include	<stdio.h>
#include	<X11/Xlib.h>
#include    	<signal.h>
#include    	<sys/time.h>

#include <math.h>
#include <string.h>
#include <AF/AFlib.h>
#include <assert.h>

int
FFTConfigure	(ClientData data, Tcl_Interp *interp, int argc, char **argv)

{
    int	    new;
    double  dnew;

    if (strcmp (argv [1], "color") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", color_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &color_flag);
	    switch (color_flag) {

	    	case GRAYSCALE:
      	    	    CreateGrays();
		    updateColors ();
		    break;

		case LINEAR:
    	    	    CreateRamp();
		    updateColors ();
		    break;

		case SPECTRAL:
    	    	    CreateSpectral();
		    updateColors ();
		    break;
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "dc") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", dc_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &dc_flag);
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "fftlength") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", fftlength);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &fftlength);
    	    SetPeak();
	    SetWindow();
	    SetupOsc();
	    if (soundfile != NULL && ac != NULL) {
		SetFileTime();
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "file") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "0x%x", soundfile);
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "gain") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", reqgain);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &new);
	    gain = new;
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "height") == 0) {
    	if (argc == 2) {
	    if (wblank == 0) {
		sprintf (interp->result, "%d", 1);
	    } else
	      sprintf (interp->result, "%d", 0);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &new);
	    if (new == 0) {
		wblank = 128;
		XClearArea (display, xw,
			    0, 0, WIDTH, wblank, 1);
	    } else {
		wblank = 0;
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "initmax") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", initmax);
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "initmin") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", initmin);
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "log") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", log_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &log_flag);
	    SetPeak();
    	    SetWindow();
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "max") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", reqmax);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &new);
	    if (new > reqmin) {
	    	reqmax = new;
	    	maxvalue = (log_flag) ? reqmax : reqmax / 1000.0;
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "min") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", reqmin);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &new);
	    if (new < reqmax) {
	    	reqmin = new;
	    	minvalue = (log_flag) ? reqmin : reqmin / 1000.0;
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "mode") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", spectro_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &new);
	    if (new != spectro_flag) {
	    	spectro_flag = new;
    	    	if (spectro_flag) {
    	    	    iwidth = 2;
    	    	    iheight = MAXFRAMEHEIGHT;
    	    	} else {
    	    	    iwidth = MAXFRAMEWIDTH;
    	    	    iheight = SWATHHEIGHT;
    	    	}
		ClearData();
    	    	XClearWindow (display, xw);
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "scope") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", scope_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &scope_flag);
	    if (scope_flag == 0) {
		SetupOsc();
	    }
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "source") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", sine_demo_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &sine_demo_flag);
    	    SetSource();
	    SetWindow();
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "stride") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", stride);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &stride);
	    if (soundfile != NULL && ac != NULL) {
		SetFileTime();
	    }
	    SetupOsc();
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else if (strcmp (argv [1], "window") == 0) {
    	if (argc == 2) {
	    sprintf (interp->result, "%d", window_flag);
	} else if (argc == 3) {
    	    Tcl_GetInt (interp, argv [2], &window_flag);
    	    SetWindow();
	} else {
	    Tcl_AppendResult (interp, "Wrong number of args \"",
	    	argv [1], "\"", NULL);
	    return TCL_ERROR;
	}
    } else {
    	Tcl_AppendResult (interp, "Bad option \"", argv [1],
	    "\": must be dc, mode or source", NULL);
	return TCL_ERROR;
    }
    return TCL_OK;    
}

void
CreateGrays()

{
    int	    i;

    for (i = 0; i < NCOLORS; i += 1) {
      colors[i].red = (i * (65536 / NCOLORS));
      colors[i].green = (i * (65536 / NCOLORS));
      colors[i].blue =  (i * (65536 / NCOLORS));
    }
}

/* interpolate colors between these two */

HSV hsvstart = { 0.75, 1.0, 0.5 };
HSV hsvstop = { 1.25, 1.0, 1.0 };

void
CreateRamp()

{
    int	    i;
    RGB	    rgb;
    HSV	    hsvinc;
    HSV	    hsv;

    hsvinc.h = (hsvstop.h - hsvstart.h) / (double) NCOLORS;
    hsvinc.s = (hsvstop.s - hsvstart.s) / (double) NCOLORS;
    hsvinc.v = (hsvstop.v - hsvstart.v) / (double) NCOLORS;
    hsv = hsvstart;
    for (i = 0; i < NCOLORS; i += 1) {
      hsv.h = (hsv.h + hsvinc.h);
      hsv.s = (hsv.s + hsvinc.s);
      hsv.v = (hsv.v + hsvinc.v);
      if (hsv.h > 1.0) hsv.h -= 1.0;
      if (hsv.s > 1.0) hsv.s -= 1.0;
      if (hsv.v > 1.0) hsv.v -= 1.0;
      rgb = RGB_RGBFromHSV(hsv);
      colors[i].red = (65535.0 * rgb.r);
      colors[i].green = (65535.0 * rgb.g);
      colors[i].blue = (65535.0 * rgb.b);
    }
}

typedef	struct	{
    int     count;
    double  h;
    double  s;
    double  v;
} colorSpec;

static	colorSpec   spectrum [] = {
    {0,     	0.637, 0.750, 0.000},	/* Black    */
    {5,     	0.637, 0.750, 0.500},	/* Blue	    */
    {5,     	0.512, 1.000, 0.500},	/* Lt. Blue */
    {2,     	0.300, 1.000, 0.500},	/* Green    */
    {4,     	0.167, 1.000, 0.500},	/* Yellow   */
    {4,     	0.083, 1.000, 0.500},	/* Orange   */
    {4,     	0.000, 1.000, 0.500},	/* Red	    */
    {4,	    	0.000, 1.000, 0.780},	/* Pink	    */
    {4,	    	0.000, 1.000, 1.000},	/* White    */
    {0,	    	0.000, 0.000, 0.000}	/* End Marker */
};

void
CreateSpectral()

{
    colorSpec	*curr;
    colorSpec	*next;
    HSV	    	hsv;
    HSV	    	incr;
    RGB	    	rgb;
    int	    	indx;
    int	    	pixel;
    double  	count;

    pixel = 0;
    curr = spectrum;
    next = curr + 1;
    while (next->count != 0) {
    	count = next->count;
    	hsv.h = curr->h;
    	hsv.s = curr->s;
    	hsv.v = curr->v;
    	incr.h = next->h - curr->h;
	if (incr.h > 0.5) {
	    incr.h -= 1.0;  	    	/* Assume shortest path */
	} else if (incr.h < -0.5) {
	    incr.h += 1.0;
	}
	incr.h /= count;
    	incr.s = (next->s - curr->s) / count;
    	incr.v = (next->v - curr->v) / count;
	for (indx = 0; indx < next->count; indx++) {
      	    if (hsv.h > 1.0) hsv.h -= 1.0;
	    if (hsv.h < 0.0) hsv.h += 1.0;
      	    if (hsv.s > 1.0) hsv.s -= 1.0;
      	    if (hsv.v > 1.0) hsv.v -= 1.0;
	    rgb = RGB_RGBFromHSV (hsv);
assert (pixel < NCOLORS);
	    colors [pixel].red = 65535.0 * rgb.r;
	    colors [pixel].green = 65535.0 * rgb.g;
	    colors [pixel].blue = 65535.0 * rgb.b;
	    hsv.h += incr.h;
	    hsv.s += incr.s;
	    hsv.v += incr.v;
	    pixel++;
	}
	curr = next;
	next++;
    }
}
