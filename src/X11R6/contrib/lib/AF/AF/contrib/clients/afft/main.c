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

/* main.c - Main Line for realtime fft program */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/main.c,v 1.5 1994/06/03 17:27:31 jg Exp $*/

#include    "afft.h"

static	void	MapWindow    (Tk_Window);
static	void	ParseCmdLine (int, char**);

typedef	struct {
    	char	    *name;  /* Name of command		*/
	Tcl_CmdProc *proc;  /* Pointer to procedure	*/
	ClientData  data;   /* Client data		*/
} TKCallbacks, *TKCallPtr;

static void TKShell	(
    	Tk_Window *,        /* Return for main window	*/
    	char *,	    	    /* Name of shell window	*/
	char *,	    	    /* Class name		*/
	char *,	    	    /* File name of tcl script	*/
	TKCallbacks *	    /* Array of callbacks	*/
);

/* Definitions of Tcl callbacks */

static	TKCallbacks AfftCallbacks[] = {
    {"setfft", 	    	FFTConfigure,	NULL},
    {"runfft",	    	FFTRun,	    	NULL},
    {"runosc",          OSCRun,         NULL},
    {NULL,  	    	NULL,	    	NULL} /**** LIST TERMINATOR ****/
};

/* Definition and Initialization of Globals */

int device = -1;	      	/* audio device */
FILE *soundfile = NULL;	      	/* sound sample input file */
double gain;           		/* dB = 20 log voltage ratio */
int reqgain = -20;

int rt_flag = 0;              	/* real time */
int dc_flag = 1;              	/* display dc component */
int max_flag = 0;             	/* use user supplied maxvalue */
int min_flag = 0;             	/* use user supplied minvalue */
int print_power_flag = 0;     	/* in ulaw case, print power of each block */
int window_flag = 1;          	/* apply Hanning window */
int spectro_flag = 0;         	/* use spectrogram display instead of waterfall */
int log_flag = 0;             	/* use log amplitude */
int sine_demo_flag = 0;       	/* don't use audio data */
int color_flag = 0;           	/* use colors in the display */
int scope_flag = 1;             /* enable oscilloscope display */

double minvalue;              	/* min value for display scaling */
double maxvalue;              	/* max value for display scaling */
int reqmin; 	    	      	/* requested min value */
int reqmax; 	    	      	/* requested max value */
int initmin;			/* initial min value */
int initmax;			/* initial max value */

int iwidth;                 	/* image width */
int iheight;                	/* image height */

/**********************************/

void
main	(int argc, char **argv)

{
    ParseCmdLine (argc, argv);
    TKShell (&mainWindow, *argv, *argv, "afft.tcl", AfftCallbacks);
}

/* Parse the command line.           */

static void
ParseCmdLine (argc, argv)
int	argc;
char	*argv[];

{
  int i;

  RGB_InitColor();
  for ( i = 1; i < argc; i++ ) {
    if ( strcmp( argv[i], "-d" ) == 0)
    {
      if(++i < argc)
        device = atoi(argv[i]);
      else
      {
        fprintf(stderr,"%s: missing device\n", argv[0]);
        exit(1);
      }
    } 
     else if ( strcmp( argv[i], "-file" ) == 0)
    {
      if(++i < argc)      
      {
      	    if (strcmp (argv [i], "-") == 0) {
	      soundfile = stdin;
      	    } else if ((soundfile = fopen (argv[i], "r")) == NULL) {
	      fprintf (stderr, "%s: can't open %s for input\n",
	      	    argv[0], argv[i]);
	      exit(1);
	    }
      } else {
      	    fprintf (stderr, "%s: missing file name\n", argv[0]);
	    exit(1);
      }
    }
     else if ( strcmp( argv[i], "-gain" ) == 0)
    {
      if(++i < argc)
      {
        if (sscanf(argv[i],"%d",&reqgain) != 1) {
            fprintf(stderr,"%s: missing gain\n", argv[0]);
            exit(1);
        }
      }
    }
     else if (strcmp (argv [i], "-length") == 0)
    {
      if (++i < argc)
	{
	    if (sscanf (argv [i], "%d", &fftlength) != 1) {
		fprintf (stderr, "%s: missing length\n", argv [0]);
		exit (1);
	    }
	    if ((fftlength != 512) &&
		(fftlength != 256) &&
		(fftlength != 128) &&
		(fftlength != 64)) {
		fprintf (stderr, "%s: illegal length\n", argv [0]);
		exit (1);
	    }
	}
    }
     else if (strcmp (argv [i], "-stride") == 0)
    {
      if (++i < argc)
	{
	    if (sscanf (argv [i], "%d", &stride) != 1) {
		fprintf (stderr, "%s: missing stride value\n", argv [0]);
		exit (1);
	    }
	    if ((stride != 512) &&
		(stride != 256) &&
		(stride != 128) &&
		(stride != 64)) {
		fprintf (stderr, "%s: illegal stride value\n", argv [0]);
		exit (1);
	    }
	}
    }
     else if ( strcmp( argv[i], "-max" ) == 0)
    {
      if(++i < argc)
      {
        if (sscanf(argv[i],"%d",&initmax) != 1) {
            fprintf(stderr,"%s: missing maxvalue\n", argv[0]);
            exit(1);
        }
        max_flag = 1;
      }
    } 
     else if ( strcmp( argv[i], "-min" ) == 0)
    {
      if(++i < argc)
      {      
        if (sscanf(argv[i],"%d",&initmin) != 1)
        {
        fprintf(stderr,"%s: missing silentlevel\n", argv[0]);
        exit(1);
        }
        min_flag = 1;
      }
    } 
     else if ( strcmp( argv[i], "-log" ) == 0)
      log_flag = 1;
     else if ( strcmp( argv[i], "-color" ) == 0)
      color_flag = 1;
     else if ( strcmp( argv[i], "-nodc" ) == 0)
      dc_flag = 0;
     else if ( strcmp( argv[i], "-printpower" ) == 0)
      print_power_flag = 1;
     else if ( strcmp( argv[i], "-realtime" ) == 0)
      rt_flag = 1;
     else if ( strcmp( argv[i], "-nowindow" ) == 0)
      window_flag = 0;
     else if ( strcmp( argv[i], "-sine" ) == 0)
      sine_demo_flag = 1;
     else if ( strcmp( argv[i], "-spec" ) == 0)
      spectro_flag = 1;
     else if ( strcmp( argv[i], "-noscope" ) == 0)
      scope_flag = 0;
     else {
     	fprintf (stderr, "%s: invalid command line option: %s\n",
	  argv [0], argv [i]);
	exit (1);
     }
  }
  if (!max_flag)
    initmax = log_flag ?  0 : 100;
  if (!min_flag)
    initmin = log_flag ? -100 : 0;
}

static void
TKShell	(Tk_Window *windowReturn,
    	char *windowName,
	char *class,
	char *script,
	TKCallPtr cbPtr)

{
    FILE        *file;
    char        path [512];
    int     	result;
    Tk_Window	mainWindow;
    Tcl_Interp  *theInterpreter;

    strcpy (path, script);
    if ((file = fopen (path, "r")) == NULL) {
	strcpy (path, TCLPATH);
	strcat (path, script);
	if ((file = fopen (path, "r")) == NULL) {
	    fprintf (stderr, "can't open %s\n", path);
	    exit (1);
	}
    }
    fclose (file);
    theInterpreter = Tcl_CreateInterp();
    if ((mainWindow = Tk_CreateMainWindow (theInterpreter, NULL, windowName, 
	"afft")) == NULL) {
	fprintf (stderr, "%s\n", theInterpreter->result);
	exit (1);
    }
    if (windowReturn != NULL) {
    	*windowReturn = mainWindow;
    }
    Tk_SetClass (mainWindow, class);
    Tk_DoWhenIdle ((Tk_IdleProc *) MapWindow, mainWindow);
    if (cbPtr) {
    	while (cbPtr->name != NULL) {
	    if (cbPtr->data == NULL) {
    	    	Tcl_CreateCommand (
	    	    theInterpreter, cbPtr->name,
		    cbPtr->proc, mainWindow, NULL);
	    } else {
    	    	Tcl_CreateCommand (
	    	    theInterpreter, cbPtr->name,
		    cbPtr->proc, cbPtr->data, NULL);
	    }
    	    cbPtr++;
    	}
    }
    result = Tcl_EvalFile (theInterpreter, path);
    if (result != TCL_OK) {
    	fprintf (stderr, "%s\n", theInterpreter->result);
	exit (0);
    }
    Tk_MainLoop();
    exit (0);
}

static void
MapWindow   	(Tk_Window theWindow)

{
    while (Tk_DoOneEvent (TK_IDLE_EVENTS) != 0) {
    	;
    }
    Tk_MapWindow (theWindow);
}
