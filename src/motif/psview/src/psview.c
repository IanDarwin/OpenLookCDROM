/*
 * PsView - A PostScript file previewer and display server
 *
 * Author:        François Bourdoncle
 * Organization:  DIGITAL Paris Research Laboratory
 * Email:         bourdoncle@prl.dec.com
 */

#include "psview.h"
#ifdef	Solaris2
#include <sys/filio.h>
#endif
#include "version.h"
#include "cursor.h"

/*
 * Types
 */

typedef enum {FixedScale, FullPage, FullHeight, FullWidth} PageMode;

typedef enum {Empty, Add, PutBack, Remove, Discard} QueueAction;

typedef enum {BeginDrag, DoDrag, EndDrag} ThumbAction;

/*
 * Macros
 */

#define BUFFERLEN (2 * PIPE_BUF)
#define REVERT 300
#define MAXPAGES 10000
#define TIMEOUT 25000
#define PROLOG 100  

#ifndef HELPFILE
#define HELPFILE "psview.help"
#endif

#ifndef PRINTCOMMAND
#define PRINTCOMMAND "lpr %s"
#endif

#define ChildMode (parent != None)

#define PSWidth  ((int) Width - (VScroll ? ScrollBarWidth : 0))
#define PSHeight ((int) Height - (HScroll ? ScrollBarWidth : 0))

#define COS(x) cos(M_PI * x / 180.0)
#define SIN(x) sin(M_PI * x / 180.0)

#define Normalize(angle) (angle - 360.0 * floor(angle/360.0))

#define WaitCursor WaitCursors[CurrentWaitCursor]

#define engine Engines[EngineNumber]

/*
 * Extern variables
 */

extern char **environ;

/*
 * Global variables
 */

static Display *dpy = NULL;
       Window window = None, pswindow = None, parent = None;
static Atom XA_WM_DELETE_WINDOW, XA_WM_PROTOCOLS;
static Pixmap pixmap = None;
static Engine *Engines[] = {&DPS_Engine, &GS_Engine};
static int EngineNumber;
static PageMode PSMode = FullPage;
static time_t FileModificationTime;
static u_short FileMode;
static long darkgrey, lightgrey, white, black;

static GC
  updategc = (GC) 0,
  selectgc = (GC) 0,
  crossgc = (GC) 0,
  invertgc = (GC) 0,
  darkgreygc = (GC) 0,
  lightgreygc = (GC) 0,
  whitegc = (GC) 0,
  blackgc;

static Cursor
  URCursor, ULCursor, LRCursor, LLCursor,
  WatchCursor, ArrowCursor, CrossCursor,
  WaitCursors[CURSORS],
  BigArrowCursor,
  HScrollCursor, VScrollCursor;

static XRectangle
  PSRect, PixmapRect, BBoxRect,
  HScrollRect, VScrollRect,
  HThumbRect, VThumbRect;

static int
  screen = 0,
  std_in = 0,        /* File descriptor of input stream */
  StdInStatus = -1,
  Prologues = 0,
  PresentationMode = 0,
  BigArrowMode = 0,
  XSocket,
  CurrentPage = 0,
  CurrentWaitCursor = 0,
  CursorDirection = 1,
  ButtonPressed = 0,
  WMTitle,
  WMBorderWidth,
  PSTextPosition = 0,
  PSTextLength = 0,
  PSMaxLength = 0,
  PSTrailerLength = 0,
  Revert = -1,
  UpdateDelay = 2,
  PSFirstPage = 1,
  PSPageNumber = 1,
  PSPageMaxNumber = 1,
  PSPageBegin = 0,
  PSPageBeginArray[MAXPAGES + 1], /* This should be enough... */
  PSPageLengthArray[MAXPAGES + 1],
  PSPageLength = 0,
  MXScroll, MYScroll,          /* Maximum number of scrolls */
  RX0, RY0, RX1, RY1,          /* Rectangle coordinates */
  PSGX0 = 0, PSGY0 = 0,        /* Position of window */
  ZoomX0 = 0, ZoomY0 = 0,      /* Window's zoomed position */
  ZoomWidth, ZoomHeight,       /* Window's zoomed dimensions */
  DeltaX = 0, DeltaY = 0,
  ScrollBarWidth = 20,
  DummySize1 = 200,            /* Initial (iconified) window size */
  DummySize2 = 150;            /* First, dummy, "MoveResize" value */

static char
  Buffer[BUFFERLEN],
  *PsView,                     /* Name of PsView's executable */
  *IncludeFile = NULL,
  PSFileName[PATH_MAX],      /* File name */
  PSWindowName[1000],          /* Window name */
  UserPrintCommand[1000],
  *Formats = NULL,
  *SocketName = NULL,
  *HelpFile = HELPFILE,
  *PSGeometry = NULL,
  *PSText = NULL,
  *PSPath = NULL,
  *PrintCommand,
  *display = NULL,
  *POSTSCRIPT = NULL,
  *PIXMAP = NULL;

static Bool
  Exiting = False,
  Done = False,
  CreatingPixmap = False,
  PixmapError = False,
  SavingPixmap = False,
  FixedGeometry = False,
  UserTitle = False,
  ConstrainedGeometry = False,
  SmoothScrolling = True,
  Smooth = False,
  ViewClip = False,
  TitleMode = False,
  BatchMode = False,
  PrintMode = False,
  ExecutePrintCommand = False,
  HelpMode = False,
  ClearPageMode = False,
  StreamMode = False,
  StandardInputMode = False,
  ThumbDrag = False,
  Printing = False,
  RotatingCursor = False,
  UnstructuredMode = False,
  WatchProgress = True,
  FirstDisplay = True,
  Initializing = True,
  ScrollPrevious = False,
  FileOutOfDate = False,
  AutoUpdate = True,
  Verbose = False,
  LowerWindow = False,
  NoImage = False,
  AutoQuitMode = False,
  AutoDieMode = False,
  EndOfStream = False,
  FixedBBox = False,
  FixedWidth = False,
  FixedHeight = False,
  NumMult = True,
  Virtual = False,
  DoBackingStore = False,
  DoBackup = True,
  ForceAdjust = False,
  NumNegative = False,
  NoArgument = True,
  ChildGeometry = False,
  HScroll = False, VScroll = False;

static double
  PSAngle = 0.0,               /* Current rotation angle */
  PSScale = 1.0,
  NumArg = 0.0, NumBasis,      /* Command argument */
  XScale,                      /* Ratio: screen pixel/PostScript point */
  YScale,
  MaxWidth, MaxHeight,         /* Screen dimensions */
  Width, Height,               /* Window's dimensions */
  XScroll = 0.0, YScroll = 0.0,/* Number of scrolls */
  DXScroll, DYScroll,          /* Scroll ratios */
  BX0 = 0.0, BY0 = 0.0,        /* Default bounding box (A4 format) */
  BX1 = 595.0, BY1 = 842.0, 
  CX0, CY0, CX1, CY1,
  HPercentage, VPercentage;

struct {
  Bool before;
  int length;
  char *name;
  char *string;
} PSPrologue[PROLOG];
    
struct {
  int GX0, GY0;
  double X0, Y0, X1, Y1, Width, Height, Scale, Mode, Angle;
  Bool FixedGeometry;
} PSStack[REVERT];

struct {
  char *name;
  double width, height;
} PredefinedFormats[] = {
  {"Letter", 612.0, 792.0},
  {"Tabloid", 792.0, 1224.0},
  {"Ledger", 1224.0, 792.0},
  {"Legal", 612.0, 1008.0},
  {"Statement", 396.0, 612.0},
  {"Executive", 540.0, 720.0},
  {"A3", 842.0, 1190.0},
  {"A4", 595.0, 842.0},
  {"A5", 420.0, 595.0},
  {"B4", 729.0, 1032.0},
  {"B5", 516.0, 729.0},
  {"Folio", 612.0, 936.0},
  {"Quarto", 610.0, 780.0},
  {"10x14", 720.0, 1008.0}};

/*
 * Prototypes
 */

int main (int argc, char *argv[]);

static double
  Max (double a[], int n),
  Min (double a[], int n);

static FILE
  *NewFile (char *buffer, char *prefix, char *default_prefix,
	    int min, int max, char *extension);

static Bool
  BooleanValue (char *s),
  FileHasChanged (void),
  CanRead (int fd),
  GetNextEvent (Bool block, XEvent *e),
  InThumb (short x, short y, Bool *horizontal, Bool *thumbdrag),
  Open (char *path, char *slash, char *file, char *suffix, int *fd, Bool save),
  OptionValue (char *option, Bool previous),
  ParseBBox (char *bbox, double *x0, double *y0, double *x1, double *y1),
  PseudoRoot (Window window),
  PtInRect (short x, short y, XRectangle *r),
  Queue (QueueAction action,...),
  ReadEvent (XEvent *e);

static int
  ErrorHandler (Display *dpy, XErrorEvent *event),
  IOErrorHandler (Display *dpy),
  OpenFile (char *file, Bool save),
  OpenSocket (char *path),
  PageNumber(char* page);

static long
  AllocateColor (short r, short g, short b, long def);

static char*
  FindBBox(void);

static void
  AddTitle (XEvent *e),
  AdjustRatio (double s, double *r, int *m, double *p),
  DrawAxis (short x, short y, double angle),
  Beep (int n),
  BeginningOfPage (Bool erase),
  BuildNumber (int digit),
  ChangeWaitCursor (Bool timeout),
  ClearArgument (void),
  ComputePages (void),
  CreateContext (void),
  CreateCursors (void),
  CreateWindow (void),
  DeiconifyWindow (void),
  DiscardEvents (int type, XEvent *e),
  DisplayPage (Bool expose),
  DisplayPageNumber (int page),
  DragThumb (ThumbAction mode, short x1, short y1, Bool horizontal),
  Draw3DRectangle (Drawable d, XRectangle r),
  DrawCross (Bool reset, short x, short y),
  DrawHScrollBar (Drawable d, double scroll, Bool outline),
  DrawOutline (double scroll, Bool horizontal),
  DrawScrollBar (Drawable d, Bool outline, Bool horizontal,
		 XRectangle sb, XRectangle thumb),
  DrawScrollBars (Drawable d),
  DrawVScrollBar (Drawable d, double scroll, Bool outline),
  Duplicate (PageMode mode, double X0, double Y0, double X1, double Y1,
	     int GX0, int GY0, int GWidth, int GHeight, Bool sw),
  EndOfPage (void),
  EnterPrintCommand(Bool show, char *windowName),
  GetActualGeometry (void),
  GetDisplayInfo (void),
  GetParent (Window window, Window *par),
  GotoPage (Bool home, int n),
  GetWindowPosition (int *x, int *y),
  HandleSelection (Bool show, int mask, int set),
  Home (void),
  InstallHandlers (void),
  MoveResizeWindow (int x, int y, int w, int h),
  OpenDisplay (int argc, char **argv),
  ParseArguments (int argc, char **argv),
  ParseDefaults (void),
  ReadPrologue (char *prologue, Bool before),
  ReadStream (void),
  RestoreContext (int bbox),
  PrintFile (char *file),
  PrintPages (int page1, int page2),
  ReadFile (char *file),
  ResizeWindow (int w, int h),
  RestartContext (void),
  Rotate (double angle, double X, double Y, double x1, double y1,
	  double *x2, double *y2),
  RotateBox (double angle, double X, double Y,
	     double x0, double y0, double x1,
	     double y1, double *x, double *y),
  SaveContext (void),
  SaveFile (void),
  SavePixmap (char* name),
  SaveStream (Bool print),
  ScaleToFit (void),
  Scroll (double dx, double dy, Bool update, Bool interactive),
  SendCommand (char *format,...),
  SetBBox (double x0, double x1, double y0, double y1),
  SetCrossCursor (void),
  SetCursor (Cursor c, Bool rotate),
  SetFormat (char *format),
  SetIconName (void),
  SetPage (int page),
  SetViewClip (void),
  SetWindowName (char *title),
  SigHandler (int sig),
  SizeScrollBar (double thumb, double scrolls, int maxscrolls,
		 short *x, unsigned short *width),
  StartContext (int page),
  System (char *command),
  ToLower (char *to, char *from),
  UpdateArea (int x, int y, unsigned int w, unsigned int h),
  UpdatePage (Bool fade),
  WaitNextEvent (XEvent *e),
  ZoomWindow (PageMode mode);

/*
 * Error handlers
 */

void Exit(int i)
{
  int j, k;
  
  if (!Exiting) {

    Exiting = True;

    (*engine->DestroyContext)();

    if (StdInStatus != -1) {
      (void) fcntl(std_in, F_SETFL, StdInStatus);
    }
    if (pixmap != None) {
      XFreePixmap(dpy, pixmap);
    }
    if (SocketName != NULL) {
      (void) unlink(SocketName);
    }
    if (PSText != NULL) {
      Free(PSText);
    }
    if (Prologues > 0) {
      for (k = 0 ; k < Prologues ; k++) {
	Free(PSPrologue[k].name);
	Free(PSPrologue[k].string);
      }
    }
    if (updategc != (GC) 0) {
      XFreeGC(dpy, updategc);
    }
    if (selectgc != (GC) 0) {
      XFreeGC(dpy, selectgc);
    }
    if (crossgc != (GC) 0) {
      XFreeGC(dpy, crossgc);
    }
    if (invertgc != (GC) 0) {
      XFreeGC(dpy, invertgc);
    }
    if (darkgreygc != (GC) 0) {
      XFreeGC(dpy, darkgreygc);
    }
    if (lightgreygc != (GC) 0) {
      XFreeGC(dpy, lightgreygc);
    }
    if (whitegc != (GC) 0) {
      XFreeGC(dpy, whitegc);
    }
    if (blackgc != (GC) 0) {
      XFreeGC(dpy, blackgc);
    }
    XFreeCursor(dpy, URCursor);
    XFreeCursor(dpy, ULCursor);
    XFreeCursor(dpy, LRCursor);
    XFreeCursor(dpy, LLCursor);
    XFreeCursor(dpy, CrossCursor);
    XFreeCursor(dpy, WatchCursor);
    XFreeCursor(dpy, ArrowCursor);
    XFreeCursor(dpy, HScrollCursor);
    XFreeCursor(dpy, VScrollCursor);
    XFreeCursor(dpy, BigArrowCursor);
    for (j = 0 ; j < CURSORS ; j++) {
      XFreeCursor(dpy, WaitCursors[j]);
    }
    if (window != None) {
      XDestroySubwindows(dpy, window);
    }
    if (dpy != NULL) {
      XCloseDisplay(dpy);
    }
  }

  exit(i);
}

void Error(Bool cleanup, Bool usage, char *format,...)
{
  static Bool first = True;
  va_list args;
  
  if (first) {
    first = False;
  } else {
    exit(EXIT_FAILURE);
  }
  
  if (usage) {
    (void) fprintf(stderr, "*** PsView revision %s\n", VERSION);
    (void) fputs("*** Author: Francois Bourdoncle <bourdoncle@prl.dec.com>\n",
		 stderr);
  }
  
  if (format != NULL) {
    if (usage) {
      (void) fputs("*** Error: ", stderr);
    } else {
      (void) fputs("*** PsView: ", stderr);
    }
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputc('\n', stderr);
  }

  if (usage) {
    (void) fputs("*** Usage: psview [switches] [file[.ps|.PS]]\n\
*** Switches:\n\
  -H         On-line help\n\
  -d <dpy>   X display name\n\
  -g <geom>  Initial geometry\n\
  -t <title> Window's title\n\
  -p <num>   Initial page\n\
  -R <angle> Rotation angle\n\
  -s <scale> Fixed scale mode\n\
  -f         Full page mode\n\
  -w         Full width mode\n\
  -h         Full height mode\n\
  -b <bbox>  Absolute document's bounding box (<x0> <y0> <x1> <y1>)\n\
  -B <style> Document's bounding box style name (\"boundingBox\" resource)\n\
  -u         No automatic file update (\"autoUpdate\" resource toggle)\n\
  -D <delay> Automatic file update delay \n\
  -I <prlg>  PostScript prologue to be sent before the document's prologue\n\
  -J <prlg>  PostScript prologue to be sent after the document's prologue\n\
  -i         Do not display bitmap images (\"bitmapImages\" resource toggle)\n\
  -W         Disable the watch progress mode (\"watchProgress\" resource toggle)\n\
  -C         Clear page mode (\"clearPage\" resource toggle)\n\
  -G         Fixed geometry (\"fixedGeometry\" resource toggle)\n\
  -L         Disable smooth scrolling (\"smoothScrolling\" resource toggle)\n\
  -U         Unstructured mode\n\
  -Q         Quit when the parent process dies\n\
  -P[123]    Presentation mode (fade-out effect)\n\
  -v         Verbose mode\n\
  -e <eng>   PostScript engine (either \"dps\" or \"gs\")\n\
  -S <sock>  Socket to read PostScript from (stream mode)\n\
  -q         Quit at the end of the input stream (stream mode)\n\
  -# <id>    Create a child window of window <id> (child mode)\n\
  -l         Create the window below its siblings (child mode)\n\
  -o <file>  Save the pixmap to the specified ppm file\n",
	  stderr);
  }
  
  if (cleanup) {
    Exit(EXIT_FAILURE);
  } else {
    exit(EXIT_FAILURE);
  }
}

static int ErrorHandler(Display *dpy, XErrorEvent *event)
{
  if (Exiting) {
    exit(EXIT_FAILURE);
  }
  
  /*
   * If the parent window does not exit in child mode, then exit quietly
   */

  if (ChildMode &&
      (event->error_code == BadDrawable ||
       event->error_code == BadWindow)) {
    Exit(EXIT_SUCCESS);
  }

  if (CreatingPixmap && !PixmapError && !BatchMode && Smooth) {

    PixmapError = True;

  } else if (SavingPixmap && !BatchMode) {

    PixmapError = True;
    
  } else {

    (void) strcpy(Buffer, "X error: ");
    
    XGetErrorText(dpy, event->error_code,
		  Buffer + strlen(Buffer),
		  BUFFERLEN - strlen(Buffer));
    
    Error(False, False, Buffer);
  }

  return 0;
}

static Bool CanRead(int fd)
{
  int how;
  
  return (ioctl(fd, FIONREAD, &how) != -1 && how > 0);
}

static int IOErrorHandler(Display *dpy)
{
  if (Exiting) {
    exit(EXIT_FAILURE);
  }
  
  Error(False, False, "Fatal I/O error");

  return 0;
}

static void SigHandler(int sig)
{
  if (Exiting) {
    exit(EXIT_FAILURE);
  } else if (sig == SIGQUIT || sig == SIGTERM) {
    Exit(EXIT_SUCCESS);
  } else if (sig == SIGINT) {
    Error(False, False, "User interrupt");
  } else {
    Error(False, False, "Caught a signal (%d)", sig);
  }
}

static void InstallHandlers(void)
{
  int i;

  for (i = 1 ; i < NSIG ; i++) {
    if (i != SIGTSTP && i != SIGSTOP && i != SIGCONT && i != SIGCHLD &&
	i != SIGTTIN && i != SIGTTOU && i != SIGWINCH && i != SIGIO &&
	i != SIGURG) {

      (void) signal(i, SigHandler);
    }
  }

  /*
   * To avoid messing up with TTY inputs
   */
  
  (void) signal(SIGTTIN, SIG_IGN);
  
  XSetErrorHandler(ErrorHandler);
  XSetIOErrorHandler(IOErrorHandler);

}

void *Malloc(unsigned int n)
{
  void *ptr;

  if ((ptr = malloc(n)) == NULL) {
    Error(True, False, "Out of memory");
  } else {
    return ptr;
  }
}

void *Realloc(void *ptr, unsigned int n)
{
  if ((ptr = realloc(ptr, n)) == NULL) {
    Error(True, False, "Out of memory");
  } else {
    return ptr;
  }
}

void Free(void *ptr)
{
  free(ptr);
}

static void DisplayPageNumber(int page)
{
  int p;
  
  if (ChildMode) {
    p = PSPageNumber;
    PSPageNumber = page;
    SetWindowName(NULL);
    PSPageNumber = p;
  } else if (!StreamMode) {
    if (page < PSPageNumber) {
      (void) sprintf(Buffer, "Page [%d]", page);
      SetWindowName(Buffer);
    } else {
      SetWindowName(NULL);
    }
  }
}

static void UpdateArea(int x, int y, unsigned int w, unsigned int h)
{
  int dx, dy;
  
  dx = Smooth ? (int)(DXScroll * XScroll * PSWidth) : 0;
  dy = Smooth ? (int)(DYScroll * YScroll * PSHeight) : 0;

  if (x < PSWidth && y < PSHeight) {
    if (x + w > PSWidth) {
      w = PSWidth - x;
    }
    
    if (y + h > PSHeight) {
      h = PSHeight - y;
    }

    XCopyArea(dpy, pixmap, window, updategc, x + dx, y + dy, w, h, x, y);
    XFlush(dpy);
  }
}

static void UpdatePage(Bool fade)
{
  unsigned int
    i, j, w, carry, max_size, num,
    delta = 20, incr = 1, square2 = 45, square3 = 45, degree;
  int x, y;
  unsigned short *state;
  unsigned int *width;
  
  if (PresentationMode == 1 && fade) {
    
    for (i = 0 ; i < delta ; i += incr) {
      for (j = 0 ; j <= PSHeight/delta ; j++) {
	UpdateArea(0, delta*j + i, PSWidth, incr);
      }
    }

  } else if (PresentationMode == 2 && fade) {
    
    for (i = 0 ; i < PSHeight ; i += incr) {
      UpdateArea(0, i, PSWidth, incr);
      XSync(dpy, False);
    }
    
  } else if (PresentationMode == 3 && fade) {
    
    max_size = Maximum(PSHeight, PSWidth);
    
    degree = ceil(log((double) max_size / (double) square2) / log(2.0));

    if (degree <= 0) {

      UpdateArea(0, 0, PSWidth, PSHeight);

    } else {
      
      state = (unsigned short*) Malloc(degree * sizeof(short));
      width = (unsigned int*) Malloc(degree * sizeof(unsigned int));
      
      for (i = 0, w = square2; i < degree ; i++, w *= 2) {
	state[i] = 0;
	width[degree - i - 1] = w;
      }

      do {

	for (i = x = y = 0 ; i < degree ; i++) {
	  j = state[i] % 4;
	  x += width[i] * (j % 2);
	  y += width[i] * (j / 2);
	}
	
	UpdateArea(x, y, square2, square2);
	
	carry = 1;
	
	for (i = 0 ; i < degree && carry == 1; i++) {
	  carry = (state[i] + 1) / 4;
	  state[i] = (state[i] + 1) % 4;
	}
	
      } while (carry == 0);
      
      Free(width);
      Free(state);
    }
    
  } else {
    
    UpdateArea(0, 0, PSWidth, PSHeight);
  }
  
  XFlush(dpy);
}

static void BeginningOfPage(Bool erase)
{
  if (UnstructuredMode) {

    if (!StreamMode && PSTextPosition > 0) {
      (*engine->UnfreezeContext)();
    }

  } else if (erase) {

    SendCommand("erasepage");
    
    if (ClearPageMode && !WatchProgress) {
      XFillRectangle(dpy, window, whitegc,
		     0, 0, (int) PSWidth, (int) PSHeight);
    }
  }

  SetCursor(WaitCursor, True);
  Printing = True;

  if (!UnstructuredMode || StreamMode) {
    SetWindowName(NULL);
  }

  (*engine->SendData)(UnstructuredMode);
  
}

static void EndOfPage(void)
{
  XEvent e;

  if (!Done && (!UnstructuredMode || StreamMode)) {
    (*engine->UnfreezeContext)();
  }
  
  SetCrossCursor();
  
  Printing = False;
  
  if (BatchMode) {
    SavePixmap(PIXMAP);
    Exit(EXIT_SUCCESS);
  }
  
  if (!WatchProgress) {
    /* Update window */
    UpdatePage(True);
  }

  if (UnstructuredMode && !StreamMode) {
    SetWindowName(NULL);
  }

  (*engine->SendData)(False);
  
  /*
   * If there are queued events, then send a dummy
   * ClientMessage event to wake up PsView.
   */
  
  if (!Queue(Empty)) {
    
    e.type = ClientMessage;
    e.xclient.window = window;
    e.xclient.message_type = XA_INTEGER; /* Just to be clean... */
    e.xclient.format = 32;
    e.xclient.data.l[0] = 0L;
    
    XSendEvent(dpy, window, False, 0, &e);
  }
}

void TextProc(char *text, int len)
{
  if (Verbose) {
    fwrite(text, len, 1, stderr);
    fflush(stderr);
  }
}

void SynchronizeProc(void)
{
  int i, next;
  char *showpage;

  if (!Printing && !UnstructuredMode) {

    (*engine->UnfreezeContext)();
    
  } else if (StreamMode) {
    
    /*
     * Discard any PostScript that appears before a "showpage" in the
     * data sent so far.
     */

    if ((showpage = strstr(PSText, "showpage")) != NULL) {
      
      next = (showpage - PSText) + 8;
      
      if (next < PSTextPosition) {
	
	for (i = 0 ; i < PSTextLength - next ; i++) {
	  PSText[i] = PSText[next + i];
	}
	
	PSTextLength -= next;
	PSTextPosition -= next;
	PSText[PSTextLength] = null;
	PSPageLengthArray[0] = PSTextLength;
	PSPageBeginArray[0] = 0;
	
      }      
    }
    
    CurrentPage = PSPageNumber = 1;

    /*
     * If there's some more data, then send it
     */
    
    if (PSTextPosition < PSTextLength) {
      (*engine->UnfreezeContext)();
      (*engine->SendData)(True);
      SendData();
    } else {
      EndOfPage();
    }
    
  } else if (UnstructuredMode && ++CurrentPage < PSPageNumber) {
    
    DisplayPageNumber(CurrentPage);
    (*engine->UnfreezeContext)();
    
  } else {

    EndOfPage();
  }

}

void DoneProc(void)
{
  if (!StreamMode) {
    PSPageNumber = CurrentPage + 1;
  }
  Done = True;
  EndOfPage();
}

static void Beep(int n)
{
  while (n-- > 0) {
    XBell(dpy, 0);
  }
  XFlush(dpy);
}

static void GetWindowPosition(int *x, int *y)
{
  Window child;
  
  XTranslateCoordinates(dpy, window,
			RootWindow(dpy, screen),
			0, 0, x, y, &child);
  
  *y -= WMTitle;
  *x -= WMBorderWidth;

}

static void SaveContext(void)
{
  int i;
  
  if (Revert < REVERT - 1) {        /* Push the context on top of the stack */
    Revert++;
  } else {                          /* Shift the stack */
    for (i = 1 ; i < REVERT ; i++) {
      PSStack[i - 1] = PSStack[i];
    }
  }

  PSStack[Revert].X0 = BX0;
  PSStack[Revert].X1 = BX1;
  PSStack[Revert].Y0 = BY0;
  PSStack[Revert].Y1 = BY1;
  PSStack[Revert].Scale = PSScale;
  PSStack[Revert].Angle = PSAngle;
  PSStack[Revert].Mode = PSMode;
  PSStack[Revert].Width = Width;
  PSStack[Revert].Height = Height;
  PSStack[Revert].FixedGeometry = FixedGeometry;
  
  GetWindowPosition(& PSStack[Revert].GX0,
		    & PSStack[Revert].GY0);
}

static void RestoreContext(int bbox)
{
  if (Revert < 0) {
    Beep(1);
  } else {

    /*
     * Find a different bbox
     */
    
    if (bbox & ControlMask) {
      while (Revert > 0 &&
	     PSStack[Revert].X0 == BX0 &&
	     PSStack[Revert].Y0 == BY0 &&
	     PSStack[Revert].X1 == BX1 &&
	     PSStack[Revert].Y1 == BY1) {
	Revert--;
      }
    }
    
    BX0 = PSStack[Revert].X0;
    BX1 = PSStack[Revert].X1;
    BY0 = PSStack[Revert].Y0;
    BY1 = PSStack[Revert].Y1;
    
    PSScale = PSStack[Revert].Scale;
    PSMode = PSStack[Revert].Mode;
    PSAngle = PSStack[Revert].Angle;

    PSGX0 = PSStack[Revert].GX0;
    PSGY0 = PSStack[Revert].GY0;

    FixedGeometry = PSStack[Revert].FixedGeometry;
    
    if (!FixedGeometry && !ChildMode) {
      Width = PSStack[Revert].Width;
      Height = PSStack[Revert].Height;
    }

    Revert--;
    
    ForceAdjust = True;

    RestartContext();
  }
}

static void ZoomWindow(PageMode mode)
{
  PSMode = mode;

  if (!FixedGeometry && !ChildMode) {
    if (ConstrainedGeometry) {
      PSGX0 = ZoomX0;
      PSGY0 = ZoomY0;
      Width = ZoomWidth;
      Height = ZoomHeight;
    } else {
      PSGX0 = PSGY0 = 0;
      Width = MaxWidth;
      Height = MaxHeight;
    }
  
    ForceAdjust = True;
  }
}

static void SetBBox(double x0, double x1, double y0, double y1)
{
  BX0 = x0;
  BX1 = x1;
  BY0 = y0;
  BY1 = y1;

  ZoomWindow(FullPage);
  
  FixedBBox = True;
  
  RestartContext();
}

static void ChangeWaitCursor(Bool timeout)
{
  static clock_t
    last = (clock_t) 0,
    now = (clock_t) -1;
  
  if (timeout ||
      ((now = clock()) != (clock_t) -1 &&
       (now > last + TIMEOUT * (CLOCKS_PER_SEC / 1000000)))) {
    
    if (now != (clock_t) -1) {
      last = now;
    }
    
    if (CursorDirection > 0) {
      CurrentWaitCursor = (CurrentWaitCursor + 1) % CURSORS;
    } else {
      CurrentWaitCursor = (CurrentWaitCursor + CURSORS - 1) % CURSORS;
    }
    
    SetCursor(WaitCursor, True);
    
  }
}

static void SetCursor(Cursor c, Bool rotate)
{
  static Cursor C = None;
  
  if (c == None) {
    
    if (BigArrowMode) {
      XDefineCursor(dpy, window, BigArrowCursor);
      XSync(dpy, False);
    } else {
      XDefineCursor(dpy, window, C);
      XSync(dpy, False);
    }
    
  } else {
    
    RotatingCursor = rotate;
    
    if (c != C) {
      
      C = c;
      
      if (!BigArrowMode) {
	XDefineCursor(dpy, window, c);
	XSync(dpy, False);
      }
    }
  }
}

static void SetCrossCursor(void)
{
  Window w;
  unsigned int m;
  int i, x, y;

  if (!XQueryPointer(dpy, window, &w, &w, &i, &i, &x, &y, &m)) {
    x = y = 0;
  }
  
  if (HScroll && PtInRect(x, y, &HThumbRect)) {
    SetCursor(HScrollCursor, False);
  } else if (VScroll && PtInRect(x, y, &VThumbRect)) {
    SetCursor(VScrollCursor, False);
  } else if ((HScroll && PtInRect(x, y, &HScrollRect)) ||
	     (VScroll && PtInRect(x, y, &VScrollRect))) {
    SetCursor(ArrowCursor, False);
  } else {
    SetCursor(CrossCursor, False);
  }
}

static double Min(double a[], int n)
{
  double min;
  
  min = a[--n];
  while (--n >= 0) {
    if (a[n] < min) {
      min = a[n];
    }
  }
  return min;
}

static double Max(double a[], int n)
{
  double max;
  
  max = a[--n];
  while (--n >= 0) {
    if (a[n] > max) {
      max = a[n];
    }
  }
  
  return max;
}

static void DrawAxis(short x, short y, double angle)
{
  double C, S, x0, y0, x1, y1;
  int X0, Y0, X1, Y1;

  if ((!HScroll || !PtInRect(x, y, &HScrollRect)) &&
      (!VScroll || !PtInRect(x, y, &VScrollRect))) {
    
    C = COS((double) angle);
    S = SIN((double) angle);
    
    if (fabs(C) < 0.0001) {
      x0 = x1 = (double) x;
      y0 = 0.0;
      y1 = (double) DisplayHeight(dpy, screen);
    } else {
      x0 = 0.0;
      x1 = (double) DisplayWidth(dpy, screen);
      y0 = y + S * (x0 - x)/C;
      y1 = y + S * (x1 - x)/C;
    }
    
    XDrawLine(dpy, window, crossgc, (int) x0, (int) y0, (int) x1, (int) y1);
  }
}

static void DrawCross(Bool reset, short x, short y)
{
  static Bool first = True;
  static short x0, y0;
  double X0, Y0;

  if (reset) {
    if (!first) {
      first = True;
      DrawAxis(x0, y0, -PSAngle);
      DrawAxis(x0, y0, -PSAngle + 90.0);
      SetWindowName(NULL);
    }
  } else {
    if (!first) {
      DrawAxis(x0, y0, -PSAngle);
      DrawAxis(x0, y0, -PSAngle + 90.0);
    }

    x0 = x;
    y0 = y;
    first = False;

    DrawAxis(x0, y0, -PSAngle);
    DrawAxis(x0, y0, -PSAngle + 90.0);
    
    X0 = (BBoxRect.x + x0 + DXScroll * XScroll * PSWidth) /
         (XScale * PSScale);
    Y0 = (BBoxRect.y + BBoxRect.height - y0 - DYScroll * YScroll * PSHeight) /
         (YScale * PSScale);
      
    Rotate(-PSAngle, 0.0, 0.0, X0, Y0, &X0, &Y0);

    (void) sprintf(Buffer, "(%.1f, %.1f)", X0, Y0);
    SetWindowName(Buffer);
  }
}

static void HandleSelection(Bool show, int mask, Bool set)
{
  double xscale, yscale, C, S, x[4], y[4], dx, dy;
  int i, X0, Y0, X1, Y1;

  C = COS(PSAngle);
  S = SIN(PSAngle);
  
  x[0] = RX0; y[0] = RY0;
  x[2] = RX1; y[2] = RY1;
  
  Rotate(-PSAngle, 0.0, 0.0,
	 C*x[0]-S*y[0], S*x[2]+C*y[2], x+1, y+1);
  
  Rotate(-PSAngle, 0.0, 0.0,
	 C*x[2]-S*y[2], S*x[0]+C*y[0], x+3, y+3);

  for (i = 0 ; i < 4 ; i++) {
    XDrawLine(dpy, window, selectgc,
	      (int) x[i], (int) y[i],
	      (int) x[(i+1) % 4], (int) y[(i+1) % 4]);
  }
  
  X0 = (int) Min(x, 4);
  Y0 = (int) Min(y, 4);
  X1 = (int) Max(x, 4);
  Y1 = (int) Max(y, 4);
  
  XFlush(dpy);
  
  dx = DXScroll * XScroll * PSWidth;
  dy = DYScroll * YScroll * PSHeight;

  xscale = XScale * PSScale;
  yscale = YScale * PSScale;
  
  for (i = 0 ; i < 4 ; i++) {
    x[i] = (BBoxRect.x + x[i] + dx) / xscale;
    y[i] = (BBoxRect.y + BBoxRect.height - y[i] - dy) / yscale;
    Rotate(-PSAngle, 0.0, 0.0, x[i], y[i], x + i, y + i);
  }
  
  CX0 = Min(x, 4);
  CX1 = Max(x, 4);
  CY0 = Min(y, 4);
  CY1 = Max(y, 4);
  
  if (show) {
    if (RX0 == RX1 && RY0 == RY1) {
      SetCrossCursor();
    } else if (RX0 <= RX1 && RY0 <= RY1) {
      SetCursor(LRCursor, False);
    } else if (RX0 <= RX1 && RY0 > RY1) {
      SetCursor(URCursor, False);
    } else if (RX0 > RX1 && RY0 < RY1) {
      SetCursor(LLCursor, False);
    } else {
      SetCursor(ULCursor, False);
    }
    
    (void) sprintf(Buffer, "(%.1f, %.1f, %.1f, %.1f)", CX0, CY0, CX1, CY1);
    SetWindowName(Buffer);
  }
  
  if (set) {
    if (mask & ControlMask) {
      if (StandardInputMode || StreamMode) {
	Beep(1);
      } else {
	SetWindowName(NULL);
	Duplicate(FullPage, CX0, CY0, CX1, CY1,
		  X0, Y0, X1 - X0, Y1 - Y0,
		  False);
      }
    } else if ((mask & ShiftMask) && CX0 < CX1 - .5 && CY0 < CY1 - .5) {
      SaveContext();
      SetBBox(CX0, CX1, CY0, CY1);
    } else {
      Beep(1);
      SetWindowName(NULL);
    }
  }
}

static Bool Open(char *path, char *slash, char *file,
		 char *suffix, int *fd, Bool save)
{
  char dir[PATH_MAX];

  (void) sprintf(Buffer, "%s%s%s%s", path, slash, file, suffix);

  *fd = open(Buffer, O_RDONLY, 0);
  
  if (*fd >= 0) {
    if (save) {
      if (Buffer[0] != '/' && getcwd(dir, PATH_MAX + 2) != NULL) {
	(void) sprintf(PSFileName, "%s/%s", dir, Buffer);
      } else {
	(void) strcpy(PSFileName, Buffer);
      }
    }
    return True;
  } else {
    return False;
  }
}

static int OpenFile(char *file, Bool save)
{
  int i, fd;
  char *path, dir[PATH_MAX];
  
  if (Open("", "", file, "", &fd, save)    ||
      Open("", "", file, ".ps", &fd, save) ||
      Open("", "", file, ".PS", &fd, save)) {
    return fd;
  }
  
  if ((path = PSPath) != NULL) {
    
    while (*path != null) {
      
      /*
       * Remove leading spaces
       */
      
      for (; *path == ' ' || *path == '\t' || *path == '\n' ; path++) {
      }

      for (i = 0 ;
	   *path != null && *path != ' ' &&
	   *path != '\t' && *path != ':' ; ) {
	dir[i++] = *path++;
      }

      dir[i] = null;
      
      /*
       * Find next directory
       */

      for (; *path != null && *path++ != ':'; ) {
      }

      if (Open(dir, "/", file, "", &fd, save) ||
	  Open(dir, "/", file, ".ps", &fd, save) ||
	  Open(dir, "/", file, ".PS", &fd, save)) {
	return fd;
      }
    }
  }
  
  Error(True, False, "Cannot open \"%s\"", file);
}

static char* FindBBox(void)
{
  char* bbox;
  char* page;

  /*
   * Get the bounding box (only if before the first %%Page comment)
   */
  
  if ((bbox = strstr(PSText, "\n%%BoundingBox:")) != NULL &&
      ((page = strstr(PSText, "\n%%Page:")) == NULL || bbox < page)) {
    
    /*
     * Check if the bbox is "(atend)".
     */
    
    bbox += 15;
    while (*bbox == ' ') {
      bbox++;
    }
    
    if (strstr(bbox, "(atend)") == bbox) {
      if (PSTrailerLength > 0 &&
	  (bbox = strstr(PSText + PSTextLength + 1,
			 "\n%%BoundingBox:")) != NULL) {
	bbox += 15;
      } else {
	bbox = NULL;
      }
    }

    return bbox;
  } else {
    return NULL;
  }
}

static Bool ParseBBox (char *bbox,
		       double *x0, double *y0, double *x1, double *y1)
{
  return ((bbox != NULL) &&
	  sscanf(bbox, "%lf %lf %lf %lf", x0, y0, x1, y1) == 4 &&
	  *x0 + 0.1 < *x1 &&
	  *y0 + 0.1 < *y1);
}

static Bool FileHasChanged(void)
{
  struct stat st;
  long delay;
  static Bool changing = False;
  static time_t last, now;
  
  if (StandardInputMode || stat(PSFileName, &st) != 0) {
    return False; /* Cannot access file */
  }
  
  now = time(NULL);
  
  if (st.st_mtime != FileModificationTime) {
    changing = True;
    last = now;
    FileModificationTime = st.st_mtime;
  }

  /*
   * If the file size is a mutiple of the block size, chances are that the
   * file is being written. Therefore, we wait a little longuer.
   */
  
  delay = (st.st_size % st.st_blksize == 0 ? 5 : 1) * UpdateDelay;

  if (changing && st.st_size > 0 && difftime(now, last) >= delay) {
    changing = False;
    return True;
  } else {
    return False;
  }
}

static void ReadFile(char *file)
{
  int fd;
  struct stat st;
  char *trailer = NULL, *tmp1, *tmp2, *bbox, *start;
  double x0, y0, x1, y1;
  
  ClearArgument();

  if (PSText != NULL) {
    Free(PSText);
  }

  FileOutOfDate = False;

  if (file[0] == '-' && file[1] == null) {
    int i;
    int n;

    /* Read file from stdin */
    strcpy(PSFileName, "Standard Input");
    StandardInputMode = True;
    PSMaxLength = BUFFERLEN;
    PSText = (char*) Malloc(PSMaxLength);
    PSText[0] = null;
    PSTextLength = 0;
    PSTrailerLength = 0;
    
    while ((n = read(0, Buffer, BUFFERLEN)) > 0) {

      /* End-of-file */
      if (Buffer[n-1] == EOF) {
	n--;
      }

      /* There is something to add. */
      if (n > 0) {

	if (PSTextLength + 1 + n > PSMaxLength) {
	  PSMaxLength += BUFFERLEN;
	  PSText = (char*) Realloc(PSText, PSMaxLength);
	}
      
	for (i = 0; i < n ; i++) {
	  PSText[PSTextLength + i] = Buffer[i];
	}
      
	PSTextLength += n;
	PSText[PSTextLength] = null;
      }
    }

    /* File mode */
    FileMode = S_IRUSR | S_IWUSR;
    
    /* Leave room for newline and null character. */
    if (PSMaxLength <= PSTextLength + 2) {
      PSMaxLength += 2;
      PSText = (char*) Realloc(PSText, PSMaxLength);
    }
    
  } else {
    /* Read a real file */
    fd = OpenFile(file, True);

    if (fstat(fd, &st) != 0) {
      Error(True, False, "Cannot access file \"%s\"", file);
    } else {
      FileModificationTime = st.st_mtime;
      PSTextLength = (int) st.st_size;
      FileMode = st.st_mode;
    }

    PSMaxLength = PSTextLength + 2;
    PSText = (char*) Malloc(PSMaxLength);
    PSTextLength = read(fd, PSText, PSTextLength);
  }
  
  if (PSTextLength == -1) {
    
    Error(True, False, "Cannot read file \"%s\"", file);
  
  } else {
    
    /*
     * Add a terminal newline
     */
    
    if (PSText[PSTextLength-1] != '\n') {
      PSText[PSTextLength++] = '\n';
    }
    
    /*
     * Make PSText a C string.
     */
    
    PSText[PSTextLength] = null;
    start = NULL;
    
    if (strstr(PSText, "%!") != PSText &&
	(start = strstr(PSText, "\n%!")) == NULL) {
      Error(True, False, "Non PostScript file (does not begin with %%!)");
    } else if (start != NULL) {
      int i, delta;
      char *from, *to;

      /*
       * Remove junk header
       */
      
      delta = (start - PSText) + 1;
      PSTextLength -= delta;
      PSMaxLength -= delta;
      
      for (i = 0, to = PSText, from = start + 1 ;
	   i <= PSTextLength ;
	   i++) {
	*to++ = *from++;
      }
    }
    
    if (strstr(PSText, "%!PS-Adobe-") != PSText || PSText[11] < '2') {
      UnstructuredMode = True;
    }
    
    /*
     * Find the last %%Trailer comment
     */

    for (tmp1 = PSText, trailer = NULL;
	 *tmp1 != null && (tmp2 = strstr(tmp1 + 1, "\n%%Trailer")) != NULL;
	  trailer = tmp1 = tmp2) {
    }
    
    if (trailer != NULL) {
      trailer++;
      *trailer = null;
      PSTrailerLength = PSTextLength - (trailer - PSText);
      PSTextLength -= PSTrailerLength;
      PSTrailerLength--;
    } else {
      PSTrailerLength = 0;
    }
    
    /*
     * Bounding box
     */
    
    if (!FixedBBox && (bbox = FindBBox()) != NULL &&
	ParseBBox(bbox, &x0, &y0, &x1, &y1)) {
      BX0 = x0;
      BY0 = y0;
      BX1 = x1;
      BY1 = y1;
    }
  }

  (void) close(fd);
  
  if (PSWindowName[0] == null) {
    (void) strcpy(PSWindowName, PSFileName);
  }
  
  ComputePages();
}

static void ReadPrologue(char *prologue, Bool before)
{
  struct stat st;
  int fd, length;
  char *string, *name;
  
  if (Prologues == PROLOG) {
    Error(True, False, "Too many prologues (%d)", Prologues);
  }
  
  fd = OpenFile(prologue, False);
  
  if (fstat(fd, &st) != 0) {
    Error(True, False, "Cannot stat prologue \"%s\"", prologue);
  }

  length = (int) st.st_size;
  string = (char *) Malloc(length + 2);
  name = (char *) Malloc(strlen(prologue) + 1);
  length = read(fd, string, length);

  strcpy(name, prologue);

  if (length == -1) {
    Error(True, False, "Cannot read prologue \"%s\"", prologue);
  } else {
    (void) close(fd);
  }

  string[length++] = '\n';
  string[length] = null;

  
  PSPrologue[Prologues].before = before;
  PSPrologue[Prologues].length = length;
  PSPrologue[Prologues].string = string;
  PSPrologue[Prologues].name = name;
  
  Prologues++;
}

static void ReadStream(void)
{
  int i, n;

 start:

  n = read(std_in, Buffer, BUFFERLEN);

  if (n == 0) {
    EndOfStream = True;
  } else if (n > 0 ) {

    if (Buffer[n-1] == EOF) {
      n--;
    }

    if (n > 0) {

      if (PSTextLength + 1 + n > PSMaxLength) {
	PSMaxLength += BUFFERLEN;
	PSText = (char*) Realloc(PSText, PSMaxLength);
      }
      
      for (i = 0; i < n ; i++) {
	PSText[PSTextLength + i] = Buffer[i];
      }
      
      PSTextLength += n;
      PSText[PSTextLength] = null;

      if (CanRead(std_in)) {
	goto start;
      }
    }
  }
  
}

static void EnterPrintCommand(Bool execute, char *windowName)
{
  PrintMode = True;
  ExecutePrintCommand = execute;
  (void) strcpy(windowName, PSWindowName);
  PSWindowName[0] = null;
  SetWindowName("Enter print command...");
}

/*
 * We need this in order to avoid SIGCHLD signals to be caught by
 * the ghostscript signal handler
 */

static void System(char *command)
{
  void (*handler)(int);
  
  handler = signal(SIGCHLD, SIG_DFL);
  system(command);
  (void) signal(SIGCHLD, handler);
  
}

static void PrintFile(char *file)
{
  char *command;

  command = (char*) Malloc(strlen(PrintCommand) + 4);

  (void) sprintf(command, "%s %%s", PrintCommand);
  (void) sprintf(Buffer, command, file, "");

  System(Buffer);

  Free(command);
}

static void SavePixmap(char* name)
{
  Drawable d;
  unsigned width;
  unsigned height;
  Visual *visual;
  FILE *file;
  XImage *image;
  unsigned long pixel;
  Colormap cm;
  XColor *colors;
  unsigned int nb_colors, shift;
  int i, x, y;
  unsigned long blue, blue_bit, green, green_bit, red, red_bit;
  char buffer[BUFFERLEN];

  if (pixmap == None) {
    d = window;
    width = (unsigned) PSWidth;
    height = (unsigned) PSHeight;
  } else {
    d = pixmap;
    width = (unsigned) PixmapRect.width;
    height = (unsigned) PixmapRect.height;
  }
  
  SetCursor(WatchCursor, False);

  visual = DefaultVisual(dpy, screen);
  shift = DefaultDepth(dpy, screen);
  nb_colors = 1 << shift;

  if (name != NULL) {
    if ((file = fopen(name, "w")) == NULL) {
      (void) sprintf(Buffer, "Cannot create file \"%s\"", name);
      Error(True, False, Buffer);
    }
  } else {
    if ((file = NewFile(Buffer, PIXMAP, "Pixmap", 0, 1000, ".ppm")) == NULL) {
      goto error;
    } else {
      (void) sprintf(buffer, "Saving file \"%s\"...", Buffer);
      SetWindowName(buffer);
    }
  }

  SavingPixmap = True;
  PixmapError = False;
  
  image = XGetImage(dpy, d, (unsigned) 0, (unsigned) 0,
		    width, height, AllPlanes, ZPixmap);
  
  XSync(dpy, False);

  SavingPixmap = False;
  
  if (PixmapError) {
    SetWindowName("*** Error: part of the window is invisible");
    PixmapError = False;
    goto error;
  }
  
  if (image == NULL) {
    if (BatchMode) {
      Error(True, False, "Cannot get X image");
    } else {
      SetWindowName("*** Error: cannot get X image");
    }
    goto error;
  }

  cm = DefaultColormap(dpy, screen);
  
  colors = (XColor*) Malloc(nb_colors * sizeof(XColor));
  
  if (visual->class != DirectColor) {
    for (i = 0; i < nb_colors; i++) {
      colors[i].pixel = i;
      colors[i].pad = 0;
    }
  } else {
    red = 0;
    green = 0;
    blue = 0;
    
    red_bit = visual->red_mask & (~(visual->red_mask) + 1);
    green_bit = visual->green_mask & (~(visual->green_mask) + 1);
    blue_bit = visual->blue_mask & (~(visual->blue_mask) + 1);
    
    for (i = 0; i < nb_colors; i++) {
      
      colors[i].pixel = red | green | blue;
      colors[i].pad = 0;
      red += red_bit;
      
      if (red > visual->red_mask) {
	red = 0;
      }
      
      green += green_bit;
      
      if (green > visual->green_mask) {
	green = 0;
      }
      
      blue += blue_bit;
      
      if (blue > visual->blue_mask) {
	blue = 0;
      }
    }
  }
  
  XQueryColors(dpy, cm, colors, nb_colors);

  (void) fprintf(file , "P6 %d %d\n%d\n", width, height, 255);
  
  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {

      pixel = XGetPixel(image, x, y);

      putc((unsigned char) (colors[pixel].red >> shift), file);
      putc((unsigned char) (colors[pixel].green >> shift), file);
      putc((unsigned char) (colors[pixel].blue >> shift), file);
      
    }
  }

  (void) fclose(file);

  XDestroyImage(image);
  
  SetWindowName(NULL);
  SetCrossCursor();

  (void) sleep(1);
  
  return;

 error:

  if (file != NULL) {
    (void) fclose(file);
  }
  Beep(3);
  (void) sleep(3);
  SetWindowName(NULL);
  SetCrossCursor();
}

static FILE *NewFile(char *buffer, char *prefix, char *default_prefix,
		     int min, int max, char *extension)
{
  int counter;
  FILE *file;

  if (prefix == NULL) {
    prefix = default_prefix;
  }
  
  for (counter = min ; counter < max ; counter++) {

    (void) sprintf(buffer, "%s%03d%s", prefix, counter, extension);

    if ((file = fopen(buffer, "r")) == NULL) {      /* File does not exist */
      if ((file = fopen(buffer, "w")) == NULL) {    /* Cannot create file */
	return NULL;
      } else {
	return file;
      }
    } else {                                        /* File exists */
      (void) fclose(file);
    }
  }
  
  return NULL;                                      /* Could not create file */
}

static void SaveStream(Bool print)
{
  FILE *file = NULL;
  char *tmpFile = NULL;
  time_t tm;
  char buffer[BUFFERLEN];
  int i;
  
  SetCursor(WatchCursor, False);

  if (PSText == NULL) {
    goto error;
  }

  if (print) {

    tmpFile = tmpnam(NULL);
    
    if ((file = fopen(tmpFile, "w")) == NULL) {
      goto error;
    } else {
      (void) strcpy(Buffer, "Printing current page...");
    }
    
  } else if ((file = NewFile(Buffer, POSTSCRIPT,
			     "PostScript", 0, 1000, ".ps")) == NULL) {
    goto error;
  } else {
    (void) sprintf(buffer, "Saving file \"%s\"...", Buffer);
    SetWindowName(buffer);
  }
  
  (void) fputs("%!PS-Adobe-2.0\n", file);
  (void) fprintf(file, "%%%%Creator: PsView revision %s (%s)\n",
		 VERSION, PSWindowName);
  
  if (time(&tm) != (time_t) -1) {
    (void) fprintf(file, "%%%%CreationDate: %s", ctime(&tm));
  }
  
  (void) fputs("%%Pages: 1\n", file);
  
  (void) fprintf(file, "%%%%BoundingBox: %d %d %d %d\n",
		 (int) floor(BX0),
		 (int) floor(BY0),
		 (int) ceil(BX1),
		 (int) ceil(BY1));
  
  if (Prologues > 0) {
    for (i = 0 ; i < Prologues ; i++) {
      (void) fwrite(PSPrologue[i].string,
		    (size_t) PSPrologue[i].length,
		    (size_t) 1,
		    file);
    }
  }

  (void) fputs("%%Page: 1 1\n\n/PsViewState save def\n", file);

  (void) fwrite(PSText, (size_t) PSTextLength, (size_t) 1, file);
  
  (void) fputs("\nPsViewState restore\nshowpage\n%%Trailer\n", file);
  
  (void) fclose(file);

  if (print) {
    PrintFile(tmpFile);
    (void) unlink(tmpFile);
  }

  (void) sleep(1);
  
  SetWindowName(NULL);
  SetCrossCursor();

  return;

 error:

  if (file != NULL) {
    (void) fclose(file);
  }
  Beep(3);
  SetWindowName("*** Error ***");
  (void) sleep(2);
  SetWindowName(NULL);
  SetCrossCursor();
}

static void SaveFile(void)
{
  char *bbox;
  int fd = -1, before, length;
  struct stat st;
  
  SetWindowName("Saving file...");
  SetCursor(WatchCursor, False);
  
  bbox = strstr(PSText, "\n%%BoundingBox:");

  if (bbox == NULL) {
    bbox = strchr(PSText, '\n');
    if (bbox == NULL) {
      goto error;
    }
    before = (bbox - PSText) + 1; /* End of first line */
    length = 0;
  } else {
    bbox++;
    
    before = bbox - PSText;
    length = strchr(bbox, '\n') - bbox;
  }

  if (!StandardInputMode && DoBackup) {
    (void) sprintf(Buffer, "%s~", PSFileName);
    
    if (rename(PSFileName, Buffer) == -1) {
      goto error;
    }
  }

  if (StandardInputMode) {
    SetWindowName("Saving file in file \"StandardInput.ps\"...");
    strcpy(Buffer, "StandardInput.ps");
  } else {
    strcpy(Buffer, PSFileName);
  }
  
  if ((fd = open(Buffer, O_WRONLY | O_CREAT | O_TRUNC, FileMode)) == -1) {
    goto error;
  }
  
  if (write(fd, PSText, before) == -1) {
    goto error;
  }
  
  (void) sprintf(Buffer, "%%%%BoundingBox: %d %d %d %d%s",
		 (int) floor(BX0),
		 (int) floor(BY0),
		 (int) ceil(BX1),
		 (int) ceil(BY1),
		 PSText[before + length] == '\n' ? "" : "\n");
  
  if (write(fd, Buffer,  strlen(Buffer)) == -1) {
    goto error;
  }
  
  if (write(fd,
	    PSText + before + length,
	    PSTextLength - before - length) == -1) {
    goto error;
  }
  
  if (PSTrailerLength > 0) {
    char c;

    c = '%';
    (void) write(fd, &c, 1);
    (void) write(fd, PSText + PSTextLength + 1, PSTrailerLength);
  }
  
  (void) close(fd);
  if (StandardInputMode) {
    /* Longer user feedback */
    (void) sleep(3);
  } else {
    (void) sleep(1);
  }

  SetWindowName(NULL);
  SetCrossCursor();

  /*
   * Discard the next "auto update"
   */
  
  if (!StandardInputMode && stat(PSFileName, &st) == 0) {
    FileModificationTime = st.st_mtime;
  }
  
  return;
  
 error:
  if (fd > -1) {
    (void) close(fd);
  }
  Beep(3);
  SetWindowName("*** Error ***");
  (void) sleep(2);
  SetWindowName(NULL);
  SetCrossCursor();
}

static void PrintPages(int page1, int page2)
{
  char c, *tmpFile = NULL;
  int fd = -1, i;
  
  if (UnstructuredMode) {
    Beep(2);
    SetWindowName("*** Error: unstructured file");
    (void) sleep(2);
    SetWindowName(NULL);
    return;
  }
  
  if (page1 < 1) {
    page1 = 1;
  }

  if (page1 > PSPageMaxNumber) {
    page1 = PSPageMaxNumber;
  }
  
  if (page2 < page1 || page2 > PSPageMaxNumber) {
    page2 = PSPageMaxNumber;
  }
  
  if (page2 > page1) {
    (void) sprintf(Buffer, "Printing pages %d-%d...", page1, page2);
  } else {
    (void) sprintf(Buffer, "Printing page %d...", page1);
  }

  SetWindowName(Buffer);
  SetCursor(WatchCursor, False);
  
  tmpFile = tmpnam(NULL);
  
  if ((fd = open(tmpFile, O_WRONLY | O_CREAT | O_TRUNC, 0666)) == -1) {
    goto error;
  }

  if (PSPageBeginArray[0] > 0) {
    long size;
    long before;
    long length;
    char* bbox;
    char* start;
    
    /* Size of the prologue */
    size = PSPageBeginArray[0] - 1;
    while (size > 0 && PSText[size-1] != '\n') {
      size--;
    }

    /* Bounding box comment */
    (void) sprintf(Buffer, "\n%%%%BoundingBox: %d %d %d %d",
		   (int) floor(BX0),
		   (int) floor(BY0),
		   (int) ceil(BX1),
		   (int) ceil(BY1));

    bbox = strstr(PSText, "\n%%BoundingBox:");

    if (bbox != NULL) {
      char* p;
      
      before = bbox - PSText;
      if (0 < before && before < size) {
	if (write(fd, PSText, before) == -1) {
	  goto error;
	}
	if (write(fd, Buffer, strlen(Buffer)) == -1) {
	  goto error;
	}
	/* Skip the end of the bbox comment */
	for (p = bbox + 1 ; *p != null && *p != '\n' ; p++) {
	}
	length = size - (p - PSText);
	if (length > 0 && write(fd, p, length) == -1) {
	  goto error;
	} else {
	  char c= '\n';
	  if (write(fd, &c, 1) == -1) {
	    goto error;
	  }
	}
	goto done;
      }
    }
    bbox = strchr(PSText, '\n');
    if (bbox != NULL) {
      size -= bbox - PSText;
      if (size > 0 && write(fd, PSText, bbox - PSText) == -1 ||
	  write(fd, Buffer, strlen(Buffer)) == -1 ||
	  write(fd, bbox, size) == -1) {
	goto error;
      }
    } else {
      goto error;
    }
  }
  
  done:
  
  for (i = 1, page1-- ; page1 < page2 ; page1++, i++) {
    (void) sprintf(Buffer, "%%%%Page: %d %d\n", page1 + 1, i);
    (void) write(fd, Buffer, strlen(Buffer));
    (void) write(fd,
		 PSText + PSPageBeginArray[page1],
		 PSPageLengthArray[page1]);
  }

  if (PSTrailerLength > 0) {
    c = '%';
    if (write(fd, &c, 1) == -1 ||
	write(fd, PSText + PSTextLength + 1, PSTrailerLength) == -1) {
      goto error;
    }
  }
  
  (void) close(fd);

  PrintFile(tmpFile);

  (void) unlink(tmpFile);
  
  (void) sleep(1);
  SetWindowName(NULL);
  SetCrossCursor();
  
  return;
  
  error:
  
  if (fd > -1) {
    (void) close(fd);
  }
  if (tmpFile != NULL) {
    (void) unlink(tmpFile);
  }
  
  Beep(3);
  SetWindowName("*** Error ***");
  (void) sleep(2);
  SetWindowName(NULL);
  SetCrossCursor();
}

static void SetIconName(void)
{
  XTextProperty prop;
  char name[PATH_MAX], *list;
  int i, j;
  
  if (UserTitle) {
    (void) strcpy(name, PSWindowName);
  } else {

    for (i = strlen(PSWindowName) - 1 ;
	 i >= 0 && PSWindowName[i] != '/' ; i--) {
    }

    if (i < 0) {
      i = 0;
    }

    if (PSWindowName[i] == '/') {
      i++;
    }

    for (j = 0 ; i < strlen(PSWindowName) ; ) {
      name[j++] = PSWindowName[i++];
    }

    name[j] = null;

  }
  
  list = name;

  if (!HelpMode &&
      !ChildMode &&
      XStringListToTextProperty(&list, 1, &prop) != 0) {
    XSetWMIconName(dpy, window, &prop);
    XFree((char*) prop.value);
    XFlush(dpy);
  }
  
}

static void SetWindowName(char *title)
{
  char *mode;
  XTextProperty prop;
  char *name;
  char buffer[2 * BUFFERLEN];
  
  if (HelpMode) {            /* Do not change the title */
    return;
  } else if (ChildMode) {
    
    (void) sprintf(buffer, "<f%s><P%d/%d><s%.3f><R%.2f><b%.1f %.1f %.1f %.1f>",
		   PSFileName,
		   PSPageNumber, UnstructuredMode ? 0 : PSPageMaxNumber,
		   PSScale, PSAngle,
		   BX0, BY0, BX1, BY1);
    switch (PSMode) {
    case FixedScale:
      (void) strcat(buffer, "<fs>");
      break;
    case FullPage:
      (void) strcat(buffer, "<fp>");
      break;
    case FullWidth:
      (void) strcat(buffer, "<fw>");
      break;
    case FullHeight:
      (void) strcat(buffer, "<fh>");
      break;
    default:
      break;
    }

    (void) strcat(buffer, AutoUpdate ? "<U+>" : "<U->");
    (void) strcat(buffer, WatchProgress ? "<W+>" : "<W->");
    (void) strcat(buffer, NoImage ? "<I->" : "<I+>");
    (void) strcat(buffer, UnstructuredMode && !StreamMode ? "<L->" : "<L+>");
    (void) strcat(buffer,
		  WatchProgress ||
		  (!HScroll && !VScroll) ||
		  (SmoothScrolling && !Smooth) ?
		  "<S0>" :
		  Smooth ?
		  "<S+>" :
		  "<S->");
    (void) strcat(buffer, Revert >= 0 ? "<r+>" : "<r->");
    (void) strcat(buffer, PSPageNumber > 1 ? "<p+>" : "<p->");
    (void) strcat(buffer,
		  UnstructuredMode || PSPageNumber < PSPageMaxNumber ?
		  "<n+>" : "<n->");
    
    (void) sprintf(buffer + strlen(buffer), "<m%s>",
		   ((PrintMode && ExecutePrintCommand ) ||
		    title == NULL) ? "" : title);
    
    name = buffer;
    
  } else if (TitleMode || PrintMode) {
    
    if (title == NULL) {
      name = PSWindowName;
    } else {
      name = title;
    }
  
  } else if (title == NULL) { /* Display the information banner */
    
    switch (PSMode) {
    case FixedScale:
      mode = "Fixed scale";
      break;
    case FullPage:
      mode = "Full page";
      break;
    case FullWidth:
      mode = "Full width";
      break;
    case FullHeight:
      mode = "Full height";
      break;
    default:
      mode = "";
      break;
    }

    if (StreamMode) {
      (void) sprintf(buffer, "%s ", PSWindowName);
    } else if (UnstructuredMode) {
      if (Done) {
	(void) sprintf(buffer, "%s [eof]", PSWindowName);
      } else {
	(void) sprintf(buffer, "%s [%d]",
		       PSWindowName,
		       PSPageNumber);
      }
    } else {
      (void) sprintf(buffer, "%s [%d/%d]",
		     PSWindowName,
		     PSPageNumber, PSPageMaxNumber);
    }
    
    (void) sprintf(buffer + strlen(buffer),
		   " [%s, %d%%", mode, (int)(100*PSScale));

    if (PSAngle != 0.0) {
      (void) sprintf(buffer + strlen(buffer), ", %d deg", 360 - (int) PSAngle);
    } else if (ChildMode) {
      (void) strcat(buffer, ", 0 deg");
    }
    
    if (!StandardInputMode && !AutoUpdate && !StreamMode) {
      if (FileOutOfDate) {
	(void) strcat(buffer, ", Out of date");
      } else {
	(void) strcat(buffer, ", Up to date");
      }
    }
    
    (void) strcat(buffer, "] [");
    (void) strcat(buffer, engine->Name);
    (void) strcat(buffer, "]");
    
    if (FixedGeometry) {
      (void) strcat(buffer, " *");
    }
    
    name = buffer;
    
  } else if (title[0] == null) {
    name = " ";
  } else {
    name = title;
  }
  
  if (XStringListToTextProperty(&name, 1, &prop)) {
    XSetWMName(dpy, window, &prop);
    XFree((char*) prop.value);
  }
  
  XFlush(dpy);
}

/*
 * This is admitedly a terrible hack... Well, that's life!
 */

static Bool PseudoRoot(Window window)
{
  Window aWindow, root, *childs;
  int i;
  unsigned int W, H, ui;
  
  if (XQueryTree(dpy, window, &root, &aWindow, &childs, &ui) == 0) {
    Error(True, False, "Cannot access window\n");
  }
  
  if (childs) {
    XFree((char*) childs);
  }
  
  if (window == root) {
    return True;
  }
  
  XGetGeometry(dpy, window, &aWindow, &i, &i, &W, &H, &ui, &ui);
  
  return (W >= MaxWidth && H >= MaxHeight); /* Pseudo root... ? */
}

static void GetParent(Window window, Window *par)
{
  Window aWindow, *childs;
  unsigned int i;
  
  if (XQueryTree(dpy, window, &aWindow, par, &childs, &i) == 0) {
    Error(True, False, "Cannot access window\n");
  } else if (childs) {
    XFree((char*) childs);
  }
}

static void GetActualGeometry(void)
{
  Window par, root, pseudoRoot, aWindow, tmpWin;
  int px, py;
  unsigned int pw, ph, bw, ui;
  XSizeHints sizeHints;

  if (ChildMode) {
    WMBorderWidth = 0;
    WMTitle = 0;
    Virtual = False;
  } else {
    root = RootWindow(dpy, screen);
    
    GetParent(window, &tmpWin);
    
    do {
      par = tmpWin;
      GetParent(par, &tmpWin);
    } while (!PseudoRoot(tmpWin));
    
    pseudoRoot = tmpWin;
    
    XGetGeometry(dpy, par, &aWindow, &px, &py, &pw, &ph, &bw, &ui);
    
    XTranslateCoordinates(dpy, pseudoRoot, window,
			  px, py, &DeltaX, &DeltaY, &aWindow);
    
    WMBorderWidth = DeltaX = - DeltaX;
    WMTitle = DeltaY = - DeltaY;

    DeltaX = - px;
    DeltaY = - py;
    
    /*
     * With a virtual window manager, add the appropriate offset
     */
    
    if (pseudoRoot != root) {
      int vx, vy;
      
      Virtual = True;
      
      XTranslateCoordinates(dpy, root, pseudoRoot, 0, 0, &vx, &vy, &aWindow);
      
      DeltaX += vx;
      DeltaY += vy;
    }
  }
  
  MaxWidth  -= 2.0 * WMBorderWidth;
  MaxHeight -= WMTitle + WMBorderWidth;
  
  if (ChildGeometry) {
    PSGY0 -= WMTitle;
    PSGX0 -= WMBorderWidth;
  }
  
  if (!FixedWidth) {
    Width = MaxWidth;
    if (PSGX0 > 0 && PSGX0 < Width - 10 && !ChildMode) {
      Width -= PSGX0;
    }
    ZoomWidth = Width;
  }
  
  if (!FixedHeight) {
    Height = MaxHeight;
    if (PSGY0 > 0 && PSGY0 < Height - 10 && !ChildMode) {
      Height -= PSGY0;
    }
    ZoomHeight = Height;
  }

  if (!ChildMode) {
    sizeHints.flags = PMaxSize;
    sizeHints.max_width = (int) MaxWidth;
    sizeHints.max_height = (int) MaxHeight;
  
    XSetWMNormalHints(dpy, window, &sizeHints);
  }
} 

static long AllocateColor(short r, short g, short b, long def)
{
  XColor color;
  
  color.red = r;
  color.green = g;
  color.blue = b;
  
  if (!XAllocColor(dpy, DefaultColormap(dpy, screen), &color)) {
    return def;
  } else {
    return color.pixel;
  }
}

static void CreateCursors(void)
{
  int i;
  Pixmap mask;
  Pixmap cursor;
  XColor fg, bg;
  unsigned int w, h;
  
  fg.red = 0xafff;
  fg.green = fg.blue = 0x0;
  bg.red = bg.green = bg.blue = 0xffff;

  /*
   * Font cursors
   */
  
  ArrowCursor = XCreateFontCursor(dpy, XC_top_left_arrow);
  CrossCursor = XCreateFontCursor(dpy, XC_crosshair);
  WatchCursor = XCreateFontCursor(dpy, XC_watch);
  LLCursor = XCreateFontCursor(dpy, XC_ll_angle);
  LRCursor = XCreateFontCursor(dpy, XC_lr_angle);
  ULCursor = XCreateFontCursor(dpy, XC_ul_angle);
  URCursor = XCreateFontCursor(dpy, XC_ur_angle);
  HScrollCursor = XCreateFontCursor(dpy, XC_sb_h_double_arrow);
  VScrollCursor = XCreateFontCursor(dpy, XC_sb_v_double_arrow);

  XRecolorCursor(dpy, WatchCursor, &fg, &bg);
  
  /*
   * Custom rotating cursor
   */
  
  if (! XQueryBestCursor(dpy, window,
			 cursor_width, cursor_height, &w, &h) ||
      w < cursor_width ||
      h < cursor_height) {

    BigArrowCursor = ArrowCursor;
    
    for (i = 0 ; i < CURSORS ; i++) {
      WaitCursors[i] = WatchCursor;
    }
  
  } else {
    
    mask = XCreateBitmapFromData(dpy, window,
				 mask_bits, cursor_width, cursor_height);
    
    for ( i = 0 ; i < CURSORS ; i++) {
      
      cursor = XCreateBitmapFromData(dpy, window,
				     cursor_bits[i],
				     cursor_width, cursor_height);
      
      WaitCursors[i] = XCreatePixmapCursor(dpy, cursor, mask, &fg, &bg,
					   cursor_height / 2,
					   cursor_width / 2);

      XFreePixmap(dpy, cursor);

      cursor = XCreateBitmapFromData(dpy, window,
				     big_arrow_bits,
				     big_arrow_width, big_arrow_height);
      
      BigArrowCursor = XCreatePixmapCursor(dpy, cursor, cursor, &fg, &bg,
					   big_arrow_x_hot,
					   big_arrow_y_hot);
      XFreePixmap(dpy, cursor);
      
    }
    
    XFreePixmap(dpy, mask);
  }

}

static void CreateWindow(void)
{
  XSetWindowAttributes attrs;
  XWMHints wmHints;
  XClassHint classHints;
  XSizeHints sizeHints;
  XGCValues gcvalues;
  Bool helpMode;
  
  /*
   * Colors
   */
  
  white = WhitePixel(dpy, screen);
  lightgrey = AllocateColor(50000, 50000, 50000, white);
  black = BlackPixel(dpy, screen);
  darkgrey = AllocateColor(33000, 33000, 33000, black);
  
  /*
   * Window's attributes
   */
  
  attrs.background_pixel = white;
  attrs.border_pixel = black;
  attrs.event_mask = 
      KeyPressMask | KeyReleaseMask | ExposureMask | LeaveWindowMask |
      StructureNotifyMask | ButtonPressMask | ButtonReleaseMask |
      Button1MotionMask | Button2MotionMask | Button3MotionMask |
      PointerMotionMask;
  attrs.cursor = CrossCursor;
  attrs.override_redirect = False;
  attrs.backing_store = (WatchProgress && DoBackingStore) ? Always : NotUseful;
  
  /*
   * The initial (iconified) window must not be too large
   */
  
  window = XCreateWindow(dpy,
			 ChildMode ? parent : RootWindow(dpy, screen),
			 0, 0,
			 ChildMode ? (int) Width : DummySize1,
			 ChildMode ? (int) Height : DummySize1,
			 0,
			 CopyFromParent, InputOutput, CopyFromParent,
			 CWEventMask | CWBorderPixel | CWBackPixel |
			 CWOverrideRedirect | CWCursor | CWBackingStore,
			 &attrs);
  
  /*
   * Class hints
   */
   
  classHints.res_name = "psview";
  classHints.res_class = "PsView";
  
  XSetClassHint(dpy, window, &classHints);
  
  /*
   * Window manager stuff
   */

  if (!ChildMode) {
    
    wmHints.flags = InputHint | StateHint;
    wmHints.input = True;
    wmHints.initial_state = ChildMode ? NormalState : IconicState;
    
    XSetWMHints(dpy, window, &wmHints);
  
    sizeHints.flags = PMinSize | PMaxSize;
    sizeHints.min_width = 0;
    sizeHints.min_height = 0;
    sizeHints.max_width = (int) MaxWidth;
    sizeHints.max_height = (int) MaxHeight;
  
    XSetWMNormalHints(dpy, window, &sizeHints);

    XA_WM_DELETE_WINDOW =  XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    XA_WM_PROTOCOLS  =  XInternAtom(dpy, "WM_PROTOCOLS", False);
    
    XSetWMProtocols(dpy, window, &XA_WM_DELETE_WINDOW, 1);
  }
  
  /*
   * Cursors
   */
  
  CreateCursors();
  
  /*
   * Graphic contexts
   */
  
  gcvalues.arc_mode = ArcChord;
  gcvalues.line_width = 0;
  gcvalues.background = white;
  gcvalues.foreground = black;
  gcvalues.subwindow_mode = IncludeInferiors;

  updategc = XCreateGC(dpy, window,
		       GCSubwindowMode | GCLineWidth |
		       GCBackground | GCForeground, &gcvalues);
  
  blackgc = XCreateGC(dpy, window,
		      GCSubwindowMode | GCLineWidth |
		      GCBackground | GCForeground | GCArcMode,
		      &gcvalues);
  
  gcvalues.foreground = lightgrey;
  gcvalues.background = lightgrey;
  
  lightgreygc = XCreateGC(dpy, window,
			  GCSubwindowMode | GCLineWidth |
			  GCBackground | GCForeground,
			  &gcvalues);
  
  gcvalues.foreground = darkgrey;
  gcvalues.background = darkgrey;
  
  darkgreygc = XCreateGC(dpy, window,
			 GCSubwindowMode | GCLineWidth |
			 GCBackground | GCForeground,
			 &gcvalues);
  
  gcvalues.background = black;
  gcvalues.foreground = white;
  
  whitegc = XCreateGC(dpy, window,
		      GCSubwindowMode | GCLineWidth |
		      GCBackground | GCForeground | GCArcMode,
		      &gcvalues);
  
  gcvalues.function = GXinvert;
  
  invertgc = XCreateGC(dpy, window,
		       GCSubwindowMode | GCLineWidth | GCFunction,
		       &gcvalues);
  
  crossgc = XCreateGC(dpy, window,
		      GCSubwindowMode | GCLineWidth | GCFunction,
		      &gcvalues);
  
  gcvalues.line_style = LineOnOffDash;
  
  selectgc = XCreateGC(dpy, window,
		       GCSubwindowMode | GCLineWidth |
		       GCFunction | GCLineStyle, &gcvalues);

  /*
   * WM_NAME and WM_ICON_NAME properties
   */
  
  helpMode = HelpMode;
  HelpMode = False;
  
  SetWindowName(ChildMode ? NULL : PSWindowName);
  SetIconName();

  HelpMode = helpMode;
  
  /*
   * We then map the window, and wait for a "ReparentNotify" event
   */

  if (ChildMode && LowerWindow) {
    XLowerWindow(dpy, window);
  }

  if (BatchMode) {
    StartContext(UnstructuredMode ?
		 PSFirstPage :
		 Minimum(PSFirstPage, PSPageMaxNumber));
    Initializing = False;
    DisplayPage(True);
  } else {
    XMapWindow(dpy, window);
  }

  XFlush(dpy);
}

static void CreateContext(void)
{
  Window old_pswindow;
  XSetWindowAttributes attrs;
 
  PSRect.x = PSRect.y = 0;
  PSRect.width = PSWidth;
  PSRect.height = PSHeight;

  if (pixmap != None) {
    XFreePixmap(dpy, pixmap);
    pixmap = None;
  }

  /*
   * Here, we create a _new_ subwindow of the main window every time the
   * contexte is restarted. This is to avoid receiving old synchronization
   * events from ghostscript (through the "ClientMessage" event) which
   * would reference a ghost (!) window.
   */
  
  old_pswindow = pswindow;
  
  attrs.background_pixel = white;
  attrs.event_mask = ExposureMask;
  attrs.backing_store = (WatchProgress && DoBackingStore) ? Always : NotUseful;
  
  pswindow = XCreateWindow(dpy, window, 0, 0, PSWidth, PSHeight,
			   0, CopyFromParent, CopyFromParent, CopyFromParent,
			   CWEventMask | CWBackPixel | CWBackingStore,
			   &attrs);
  
  if (old_pswindow != None) {
    XDestroyWindow(dpy, old_pswindow);
  }

  XMapWindow(dpy, pswindow);
  
  if (WatchProgress) {
    
    PixmapRect = PSRect;
    Smooth = False;
  
  } else {
    
    PixmapRect.x = PixmapRect.y = 0;
    PixmapRect.width = BBoxRect.width;
    PixmapRect.height = BBoxRect.height;
    
    PixmapError = False;
    
  error:
    
    if ((SmoothScrolling && !PixmapError)) {
      Smooth = True;
    } else {
      Smooth = False;
      PixmapRect.width =  PSRect.width;
      PixmapRect.height = PSRect.height; 
    }

    /*
     * Create a new pixmap
     */

    CreatingPixmap = True;
    
    pixmap = XCreatePixmap(dpy, pswindow,
			   PixmapRect.width, PixmapRect.height,
			   DefaultDepth(dpy, screen));

    /*
     * Synchronize for errors
     */
    
    XSync(dpy, False);

    /*
     * If the server was unable to allocate the pixmap, then turn the
     * "Smooth" mode off
     */
    
    if (Smooth && PixmapError) {
      goto error;
    }
      
    PixmapError = False;
    CreatingPixmap = False;

    /*
     * Clear it
     */
    
    XFillRectangles(dpy, pixmap, whitegc, &PixmapRect, 1);

  }

  /*
   * Set the PostScript context clip rectangle to the appropriate value
   */
  
  XSetClipRectangles(dpy, selectgc, 0, 0, &PSRect, 1, Unsorted);
  XSetClipRectangles(dpy, updategc, 0, 0, &PSRect, 1, Unsorted);
  XSetClipRectangles(dpy, crossgc, 0, 0, &PSRect, 1, Unsorted);
  
  HScrollRect.x = 0;
  HScrollRect.y = PSRect.height + 1;
  HScrollRect.width = PSRect.width + 1;
  HScrollRect.height = ScrollBarWidth - 1;
  
  VScrollRect.x = PSRect.width + 1;
  VScrollRect.y = 0;
  VScrollRect.width = ScrollBarWidth - 1;
  VScrollRect.height = PSRect.height + 1;

  (*engine->StartContext)(dpy, pswindow, pixmap, XScale, YScale,
			  &PixmapRect, &BBoxRect, white, black);

  (*engine->SendData)(False);
}

static void StartContext(int page)
{
  double xs, ys;
  int i;
  static char
    
    unstructured[] =
      
      "\n/showpage {clientsync erasepage} bind def\n",
      
    structured[] =
      
      "\n/showpage {clientsync} bind def \n",
      
    image[] =
      
      "\n/image { \
           /imagedict 30 dict def \
           imagedict begin \
             pop /m exch def /b exch def /h exch def /w exch def \
             /saveimage save def mark \
              /s w string def \
              /count 0 def /maxcount h w mul 8 div b mul def \
              {currentfile s readhexstring pop \
               length count add /count exch def count maxcount ge {exit} if \
              } loop \
             cleartomark saveimage restore \
             gsave  \
               0 setlinewidth \
               0 0 moveto 1 0 lineto 1 1 lineto 0 1 lineto closepath \
               0 0 moveto 1 1 lineto 0 1 moveto 1 0 lineto stroke \
             grestore \
           end \
       } def \n\
       /colorimage { \
           /imagedict 30 dict def \
           imagedict begin \
             /nbc exch def \
             /multi exch def \
             multi {nbc}{1}ifelse {pop} repeat \
             /m exch def /b exch def /h exch def /w exch def \
             /saveimage save def mark \
              /s w string def \
              /count 0 def /maxcount h w mul 8 div b mul def \
              /maxcount maxcount nbc mul def \
              {currentfile s readhexstring pop \
               length count add /count exch def count maxcount ge {exit} if \
              } loop \
             cleartomark saveimage restore \
             gsave  \
               0 setlinewidth \
               0 0 moveto 1 0 lineto 1 1 lineto 0 1 lineto closepath \
               0 0 moveto 1 1 lineto 0 1 moveto 1 0 lineto stroke \
             grestore \
           end \
       } def \n",

    stdops[] =

      "\n /letter {} bind def \
          /note {} bind def \
          /legal {} bind def \
          /a3 {} bind def \
          /a4 {} bind def \
          /a5 {} bind def \
          /a6 {} bind def \
          /b4 {} bind def \
          /flsa {} bind def \
          /flse {} bind def \
          /halfletter {} bind def \
          /11x17 {} bind def \
          /ledger {} bind def \
          /11x17tray {} bind def \
          /a3tray {} bind def \
          /a4tray {} bind def \
          /a5tray {} bind def \
          /a6tray {} bind def \
          /b4tray {} bind def \
          /flsatray {} bind def \
          /flsetray {} bind def \
          /halflettertray {} bind def \
          /legaltray {} bind def \
          /lettertray {} bind def \
          /setjobtimeout {pop} bind def \n";

  xs           = XScroll;
  ys           = YScroll;
  XScroll      = 0;
  YScroll      = 0;
  PSPageNumber = 0;
  FirstDisplay = True;
  Done         = False;
  
  ClearArgument();
  ScaleToFit();
  CreateContext();

  SendCommand("initclip");
  SendCommand("%f %f scale", PSScale, PSScale);
  SendCommand("%f rotate", PSAngle);
  SendCommand("erasepage");
  
  if (ViewClip) {
    SendCommand("%f %f %f %f rectviewclip",
		BX0, BY0, BX1 - BX0, BY1 - BY0);
  }

  /*
   * Send the prologue
   */

  if (UnstructuredMode) {
    SendCommand(unstructured);
  } else {
    SendCommand(structured);
  }

  /* No bitmap images */
  if (NoImage) {
    SendCommand(image);
  }

  /* Standard device-dependant operators */
  if (engine == &DPS_Engine) {
    SendCommand(stdops);
  }
  
  /*
   * External prologues
   */
  
  for (i = 0 ; i < Prologues ; i++) {
    if (PSPrologue[i].before) {
      (*engine->SendBuffer)(PSPrologue[i].string, PSPrologue[i].length);
    }
  }

  /*
   * File prologue
   */
  
  if (PSPageBeginArray[0] > 0) {
    (*engine->SendBuffer)(PSText, PSPageBeginArray[0]);
  }
  
  /*
   * External prologues
   */

  for (i = 0 ; i < Prologues ; i++) {
    if (! PSPrologue[i].before) {
      (*engine->SendBuffer)(PSPrologue[i].string, PSPrologue[i].length);
    }
  }

  /*
   * Scroll to previous position when revisiting the file
   */
  
  if (ScrollPrevious) {
    Scroll(xs, ys, False, False);
  }
  
  ScrollPrevious = False;
  
  SetPage(page);

  CursorDirection = 1;

  /*
   * Disable any previous bbox selection and wait for an expose event
   * to display the page.  This event will necessarily be generated since a
   * new "pswindow" has been created
   */

  ButtonPressed = 0;
  DrawCross(True, 0, 0);
}

static void RestartContext(void)
{
  if (UnstructuredMode) {
    DisplayPageNumber(1);
  }

  StartContext(PSPageNumber);
}

static void SendCommand(char *format,...)
{
  va_list args;
  int len;
  static char buffer[2000]; /* This should be enough */
  
  buffer[0] = ' ';

  va_start(args, format);
  vsprintf(buffer + 1, format, args);
  va_end(args);
  
  buffer[len = strlen(buffer)] = '\n';
  
  (*engine->SendBuffer)(buffer, len + 1);
}

void SendData(void)
{
  int len;

  if (UnstructuredMode) {

    len = PSTextLength - PSTextPosition;
    
    if (len > 0) {
      (*engine->SendBuffer)(PSText + PSTextPosition, len);
      PSTextPosition += len;
    }

    if (PSTextPosition >= PSTextLength) {
      
      if (StreamMode) {
	SendCommand("clientsync"); /* Synchronize */
      } else {
	SendCommand("quit");       /* Terminate */
      }
      
      (*engine->SendData)(False);  /* No more data to send */

    }
  }
}

static void Draw3DRectangle(Drawable d, XRectangle r)
{
  short x0, x1, y0, y1;

  r.x++;
  r.y++;
  r.width -= 2;
  r.height -= 2;
  
  XFillRectangles(dpy, d, lightgreygc, &r, 1);
  
  x0 = r.x - 1;
  y0 = r.y - 1;
  x1 = x0 + r.width + 2;
  y1 = y0 + r.height + 2;
  
  XDrawLine(dpy, d, whitegc, x0, y1, x0, y0);
  XDrawLine(dpy, d, whitegc, x0, y0, x1, y0);
  XDrawLine(dpy, d, blackgc, x1, y0, x1, y1);
  XDrawLine(dpy, d, blackgc, x1, y1, x0, y1);
  
  x0++;
  y0++;
  x1--;
  y1--;

  XDrawLine(dpy, d, whitegc, x0, y1, x0, y0);
  XDrawLine(dpy, d, whitegc, x0, y0, x1, y0);
  XDrawLine(dpy, d, blackgc, x1, y0, x1, y1);
  XDrawLine(dpy, d, blackgc, x1, y1, x0, y1);
}

static void DrawScrollBar(Drawable d, Bool outline, Bool horizontal,
			  XRectangle sb, XRectangle thumb)
{
  XRectangle r;
  
  if (outline) {
    XDrawRectangles(dpy, d, invertgc, &thumb, 1);
    thumb.x++;
    thumb.y++;
    thumb.width -= 2;
    thumb.height -= 2;
    XDrawRectangles(dpy, d, invertgc, &thumb, 1);
  } else {
    if (horizontal) {
      r = sb;
      r.width = thumb.x - sb.x;
      if (thumb.x > sb.x) {
	XFillRectangles(dpy, d, darkgreygc, &r, 1);
      }
      r = sb;
      r.x = thumb.x + thumb.width + 1;
      r.width = sb.x + sb.width - r.x - 1;
      if (sb.x + sb.width > r.x + 1) {
	XFillRectangles(dpy, d, darkgreygc, &r, 1);
      }
    } else {
      r = sb;
      r.height = thumb.y - sb.y;
      if (thumb.y > sb.y) {
	XFillRectangles(dpy, d, darkgreygc, &r, 1);
      }
      r = sb;
      r.y = thumb.y + thumb.height + 1;
      r.height = sb.y + sb.height - r.y - 1;
      if (sb.y + sb.height > r.y + 1) {
	XFillRectangles(dpy, d, darkgreygc, &r, 1);
      }
    }

    Draw3DRectangle(d, thumb);

    XFillArc(dpy, d, whitegc,
	     thumb.x + thumb.width/2 - 4,
	     thumb.y + thumb.height/2 - 4,
	     7, 7,
	     220*64, 170*64);

    XFillArc(dpy, d, blackgc,
	     thumb.x + thumb.width/2 - 4,
	     thumb.y + thumb.height/2 - 4,
	     7, 7,
	     30*64, 220*64);
  }
  
  XFlush(dpy);
}

static void SizeScrollBar(double thumb, double scrolls, int maxscrolls,
			  short *x, unsigned short *width)
{
  int scrollbar;
  
  scrollbar = *width;
  
  thumb *= scrollbar;
  
  *x = (int) (scrolls * (scrollbar - thumb) / (maxscrolls - 1));
  
  *width = (int) thumb - 1;
  
  if (*width < ScrollBarWidth - 2) {
    *width = ScrollBarWidth - 2;
  }
  
  if (*x + *width > scrollbar) {
    *x = scrollbar - *width - 2;
  }
}

static void DrawHScrollBar(Drawable d, double scroll, Bool outline)
{
  if (HScroll) {
    
    HThumbRect = HScrollRect;
    
    XDrawLine(dpy, d, blackgc,
	      HThumbRect.x - 1, HThumbRect.y - 1,
	      HThumbRect.x - 1 + HThumbRect.width, HThumbRect.y - 1);
    
    HThumbRect.height--;
    
    SizeScrollBar(HPercentage, scroll, MXScroll,
		  &HThumbRect.x, &HThumbRect.width);
    
    DrawScrollBar(d, outline, True, HScrollRect, HThumbRect);
    
    if (VScroll) {
      XDrawLine(dpy, d, blackgc,
		HScrollRect.x - 1 + HScrollRect.width,
		HScrollRect.y - 1,
		HScrollRect.x - 1 + HScrollRect.width,
		HScrollRect.y + HScrollRect.height);
    }
    
    XFlush(dpy);
  }
}

static void DrawVScrollBar(Drawable d, double scroll, Bool outline)
{
  if (VScroll) {
    
    VThumbRect = VScrollRect;
    
    XDrawLine(dpy, d, blackgc,
	      VThumbRect.x - 1, VThumbRect.y - 1,
	      VThumbRect.x - 1, VThumbRect.y - 1 + VThumbRect.height);
    
    VThumbRect.width--;
    
    SizeScrollBar(VPercentage, scroll, MYScroll,
		  &VThumbRect.y, &VThumbRect.height);
    
    DrawScrollBar(d, outline, False, VScrollRect, VThumbRect);
    
    if (HScroll) {
      XDrawLine(dpy, d, blackgc,
		VScrollRect.x - 1,
		VScrollRect.y - 1 + VScrollRect.height,
		VScrollRect.x + VScrollRect.width,
		VScrollRect.y - 1 + VScrollRect.height);
    }
    
    XFlush(dpy);
  }
}

static void DrawScrollBars(Drawable d)
{
  XRectangle r;
  
  DrawHScrollBar(d, XScroll, False);
  DrawVScrollBar(d, YScroll, False);

  if (HScroll && VScroll) {

    r.x = VScrollRect.x;
    r.y = HScrollRect.y;
    r.width = ScrollBarWidth - 2;
    r.height = ScrollBarWidth - 2;

    Draw3DRectangle(d, r);
  }

  XSync(dpy, False);
  
}

static void DrawOutline(double scroll, Bool horizontal)
{
  if (!Smooth) {
    if (horizontal) {
      DrawHScrollBar(window, scroll, True);
    } else {
      DrawVScrollBar(window, scroll, True);
    }
  }
}

static void DragThumb(ThumbAction mode, short x1, short y1, Bool horizontal)
{
  static short X0, Y0;
  static double scroll0, scroll1, Scroll0;
  
  if (horizontal) {
    scroll1 =
      Scroll0 + (1.0 * (MXScroll-1) * (x1-X0))
	/ (HScrollRect.width - HThumbRect.width);
    if (scroll1 > MXScroll - 1) {
      scroll1 = MXScroll - 1;
    }
    if (scroll1 < 0.0) {
      scroll1 = 0.0;
    }
  } else {
    scroll1 =
      Scroll0 + (1.0 * (MYScroll-1)* (y1-Y0))
	/ (VScrollRect.height - VThumbRect.height);
    if (scroll1 > MYScroll - 1) {
      scroll1 = MYScroll - 1;
    }
    if (scroll1 < 0.0) {
      scroll1 = 0.0;
    }
  }
  
  switch (mode) {
    
  case BeginDrag:
    
    SetCursor(horizontal ? HScrollCursor : VScrollCursor, False);
    X0 = x1;
    Y0 = y1;
    Scroll0 = scroll0 = (horizontal ? XScroll : YScroll);
    DrawOutline(scroll0, horizontal);
    break;
    
  case DoDrag:
    
    DrawOutline(scroll0, horizontal);

    if (Smooth) {
      if (horizontal) {
	Scroll(scroll1 - scroll0, 0.0, True, False);
      } else {
	Scroll(0.0, scroll1 - scroll0, True, False);
      }
    }

    scroll0 = scroll1;

    DrawOutline(scroll0, horizontal);

    break;
  case EndDrag:
    
    DrawOutline(scroll0, horizontal);
    
    if (horizontal) {
      Scroll(scroll1 - (Smooth ? scroll0 : Scroll0),
	     0.0, True, False);
    } else {
      Scroll(0.0, scroll1 - (Smooth ? scroll0 : Scroll0),
	     True, False);
    }
    break;
  }
}

static Bool PtInRect(short x, short y, XRectangle *r)
{
  return (x >= r->x &&
	  y >= r->y &&
	  x <= r->x + r->width &&
	  y <= r->y + r->height);
}

static Bool InThumb(short x, short y, Bool *horizontal, Bool *thumbdrag)
{
  if (HScroll) {
    if (PtInRect(x, y, &HThumbRect)) {
      *horizontal = True;
      *thumbdrag = True;
      return True;
    } else if (PtInRect(x, y, &HScrollRect)) {
      *horizontal = True;
      *thumbdrag = False;
      if (x < HThumbRect.x) {
	Scroll(-1.0, 0.0, True, False);
      } else {
	Scroll(1.0, 0.0, True, False);
      }
      return True;
    }
  }
  
  if (VScroll) {
    if (PtInRect(x, y, &VThumbRect)) {
      *horizontal = False;
      *thumbdrag = True;
      return True;
    } else if (PtInRect(x, y, &VScrollRect)) {
      *horizontal = False;
      *thumbdrag = False;
      if (y < VThumbRect.y) {
	Scroll(0.0, -1.0, True, False);
      } else {
	Scroll(0.0, 1.0, True, False);
      }
      return True;
    }
  }
  
  *thumbdrag = False;
  
  if (HScroll && VScroll &&
      x > HScrollRect.x + HScrollRect.width &&
      y > VScrollRect.y + VScrollRect.height) {
    Beep(1);
    return True;
  } else {
    return False;
  }
}

static void DisplayPage(Bool expose)
{
  XEvent e;

  /*
   * Disable any previous bbox selection
   */
  
  ButtonPressed = 0;
  DrawCross(True, 0, 0);
  
  if (Done ||
      (UnstructuredMode &&
       !FirstDisplay &&
       (!expose || WatchProgress) &&
       ((!Smooth && (StreamMode || PSPageNumber <= CurrentPage)) ||
	(Smooth && (StreamMode || PSPageNumber < CurrentPage))))) {
    
    ScrollPrevious = True;
    RestartContext();
    
  } else {
    
    /*
     * Get rid of expose events
     */

    DiscardEvents(Expose, &e);
    
    /*
     * Draw the scroll bars
     */

    DrawScrollBars(window);
    
    /*
     * Update using the pixmap whenever possible, i.e.:
     * 
     * 1) Unstructured mode when an "Expose" event has been received
     *
     * 2) Under Smooth, when the current page corresponds to the
     *    pixmap contents, that is stream mode (one page) or the last displayed
     *    page.
     *
     * Otherwise, redraw the page.
     */
    
    if (!FirstDisplay &&
	((expose && !WatchProgress) ||
	 (Smooth && (StreamMode || CurrentPage == PSPageNumber)))) {
      
      UpdatePage(False);
      SetWindowName(NULL);
      
    } else if (UnstructuredMode) {

      if (FirstDisplay) {
	
	/*
	 * Erase the page in stream mode, but rotate only
	 * during the the first display
	 */
	
	PSTextPosition = 0;
	BeginningOfPage(False);
	SendData();
	
      } else {
	
	if (!StreamMode && CurrentPage < PSPageNumber - 1) {
	  DisplayPageNumber(CurrentPage + 1);
	}
	
	BeginningOfPage(True);
	SendData();

      }
    
    } else {
      
      BeginningOfPage(True);
      
      (*engine->SendBuffer)(PSText + PSPageBegin, PSPageLength);

      SendCommand("clientsync");
      
      (*engine->FlushContext)();
      
    }
    
    FirstDisplay = False;
    
  }
}

static int PageNumber(char *page)
{
  int i;
  
  /* End of line */
  
  for (i = 0 ; page[i] != null && page[i] != '\n' ; i++) {
  }

  /* Skip spaces */
  
  for (i-- ; i > 0 && isspace(page[i]) ; i--) {
  }

  /* Skip last number */
  
  for (; i > 0 && isdigit(page[i]) ; i--) {
  }

  if (i > 0) {
    return atoi(page + i + 1);
  } else {
    return 0;
  }
}

static void ComputePages(void)
{
  int n, i, N, p, delta = 1;
  char *page;
  
  if (UnstructuredMode) {
    PSPageBeginArray[0] = 0;
    PSPageLengthArray[0] = PSTextLength;
    PSPageMaxNumber = 1;
  } else {
    for (n = 0, i = 0;
	 n < PSTextLength && i < MAXPAGES &&
	 (page = strstr(PSText + n, "\n%%Page:")) != NULL ; ) {
      
      n = (strchr(page + 1, '\n') - PSText) + 1;
      p = PageNumber(page + 8);

      /* First "%%Page:" or next page (ascending or descending order) */
      
      if (p > 0 && (i == 0 || p == N + delta)) {

	if (i == 0) {
	  N = p;
	  delta = (N == 1) ? 1 : -1;
	} else {
	  N = p;
	  PSPageLengthArray[i-1] = (page - PSText) - PSPageBeginArray[i-1] + 1;
	}
	
	PSPageBeginArray[i] = n;
	i++;
      }
    }
	
    if (i == 0) {

      /* Unstructured mode */
      
      UnstructuredMode = True;
      
      PSPageBeginArray[0] = 0;
      PSPageLengthArray[0] = PSTextLength;
      PSPageMaxNumber = 1;

    } else {
      
      PSPageLengthArray[i-1] = PSTextLength - PSPageBeginArray[i-1] + 1;
      PSPageMaxNumber = i;
      
    }
  }
}

static void SetPage(int page)
{
  if (StreamMode || page < 1) {
    page = 1;
  } else if (!UnstructuredMode && page > PSPageMaxNumber) {
    page = PSPageMaxNumber;
  } else if (UnstructuredMode && PSPageNumber && /* i.e. : not initializing */
	     Done && page > PSPageNumber) {
    page = PSPageNumber; /* Do not jump past the end of the file */
  }
  
  CurrentPage = PSPageNumber;
  PSPageNumber = page;
  
  if (!UnstructuredMode) {
    PSPageBegin = PSPageBeginArray[PSPageNumber - 1];
    PSPageLength = PSPageLengthArray[PSPageNumber - 1];
  }
}

static void ClearArgument(void)
{
  NoArgument = True;
  NumNegative = False;
  NumArg = 0.0;
  NumMult = True;
}

static void BuildNumber(int digit)
{
  NoArgument = False;
  
  if (digit < 0) {
    NumMult = False;
    NumBasis = 0.1;
    return;
  }
  
  if (NumMult) {
    NumArg = 10.0 * NumArg + digit;
  } else {
    NumArg += digit * NumBasis;
    NumBasis /= 10.0;
  }
}

static void SetViewClip(void)
{
  double ddx, ddy;
  
  if (!ViewClip) {
    return;
  }
  
  /*
   * Save the current transformation matrix
   */
  
  SendCommand("matrix currentmatrix matrix defaultmatrix setmatrix");

  SendCommand("%f %f scale %f rotate",
	      PSScale, PSScale, PSAngle);

  SendCommand("%f %f itransform translate",
	      - DXScroll * (Smooth ? 0.0 : XScroll) * PSWidth,
	      - DYScroll * (Smooth ? 0.0 : YScroll) * PSHeight);
  
  SendCommand("%f %f %f %f rectviewclip",
	      BX0, BY0, BX1 - BX0, BY1 - BY0);

  /*
   * Restore the current transformation matrix
   */
  
  SendCommand("setmatrix");
  
}

static void Scroll(double dx, double dy, Bool update, Bool interactive)
{
  double ddx, ddy;
  Bool redraw = False;

  if (dx == 0.0 && dy == 0.0) {
    return;
  }
  
  if (dy < 0.0 || (dy == 0.0 && dx < 0.0)) {
    CursorDirection = -1;
  } else {
    CursorDirection = 1;
  }
  
  if (XScroll == 0.0 && dx < 0.0) {
    if (interactive && PSPageNumber > 1) {
      SetPage(PSPageNumber - 1);
      redraw = True;
      dx = MXScroll - 1.0;
    } else {
      Beep(1);
      return;
    }
  } else if (XScroll + dx < 0.0) {
    dx = - XScroll;
  } else if (XScroll == MXScroll - 1.0 && dx > 0.0) {
    if (!interactive || StreamMode || Done ||
	(!UnstructuredMode && PSPageNumber == PSPageMaxNumber)) {
      Beep(1);
      return;
    } else {
      SetPage(PSPageNumber + 1);
      redraw = True;
      dx = 1.0 - MXScroll;
    }
  } else if (XScroll + dx > MXScroll - 1.0) {
    dx = MXScroll - 1.0 - XScroll;
  }
  
  if (YScroll == 0.0 && dy < 0.0) {
    if (interactive && PSPageNumber > 1) {
      SetPage(PSPageNumber - 1);
      redraw = True;
      dy = MYScroll - 1.0;
    } else {
      Beep(1);
      return;
    }
  } else if (YScroll + dy < 0.0) {
    dy = - YScroll;
  } else if (YScroll == MYScroll - 1.0 && dy > 0.0) {
    if (!interactive || StreamMode || Done ||
	(!UnstructuredMode && PSPageNumber == PSPageMaxNumber)) {
      Beep(1);
      return;
    } else {
      SetPage(PSPageNumber + 1);
      redraw = True;
      dy = 1.0 - MYScroll;
    }
  } else if (YScroll + dy > MYScroll - 1.0) {
    dy = MYScroll - 1.0 - YScroll;
  }
  
  XScroll += dx;
  YScroll += dy;

  if (!Done && (dx != 0.0 || dy != 0.0)) {
    
    SetViewClip();
    
    SendCommand("%f %f idtransform translate",
		- DXScroll * (Smooth ? 0.0 : dx) * PSWidth,
		- DYScroll * (Smooth ? 0.0 : dy) * PSHeight);

  }
  
  if (update) {
    if (Smooth && !redraw) {
      UpdatePage(False);
      DrawHScrollBar(window, XScroll, False);
      DrawVScrollBar(window, YScroll, False);
    } else {
      DisplayPage(False);
    }
  }
}

static void Home(void)
{
  Scroll(-XScroll, -YScroll, False, False);
}

static void GotoPage(Bool home, int n)
{
  int page;
  
  ClearArgument();

  if (StreamMode) {
    Beep(1);
  } else if (n < 1 ||
	   (UnstructuredMode && Done && n >= PSPageNumber) ||
	     (!UnstructuredMode && n > PSPageMaxNumber)) {
    Beep(1);
  } else {
    
    page = PSPageNumber;

    SetPage(n);
    
    if (home) {
      Home();
    }
    
    if (PSPageNumber >= page) {
      CursorDirection = 1;
    } else {
      CursorDirection = -1;
    }

    DisplayPage(False);
  }
}

static void AdjustRatio(double s, double *r, int *m, double *p)
{
  double n, rho;
  
  *p = 1.0/s;         /* Percentage of page on screen */
  
  n = ceil(s - 0.01); /* Maximum number of scrolls */
  
  rho = s - floor(s);
  
  *m = (int) n;
  
  if (*m == 1) {
    *r = 0.0;
  } else if (s == n) {
    *r = 1.0;
  } else {
    *r = (rho + n - 2.0) / (n - 1.0);
  }
}

static void Rotate(double angle,
		   double X, double Y,
		   double x1, double y1,
		   double *x2, double *y2)
{
  double C, S;
  
  C = COS(angle);
  S = SIN(angle);
  
  *x2 = X + C * (x1 - X) - S * (y1 - Y);
  *y2 = Y + S * (x1 - X) + C * (y1 - Y);
}

static void RotateBox(double angle,
		      double X, double Y,
		      double x0, double y0,
		      double x1, double y1,
		      double *x, double *y)
{
  Rotate(angle, X, Y, x0, y0, x, y);
  Rotate(angle, X, Y, x0, y1, x + 1, y + 1);
  Rotate(angle, X, Y, x1, y1, x + 2, y + 2);
  Rotate(angle, X, Y, x1, y0, x + 3, y + 3);
}

static void DeiconifyWindow(void)
{
  XMapRaised(dpy, window);
}

static void ResizeWindow(int w, int h)
{
  XWindowChanges changes;
  
  changes.width = w;
  changes.height = h;
  
  XReconfigureWMWindow(dpy, window, screen,
		       CWWidth | CWHeight, &changes);
  
  XFlush(dpy);
}

static void MoveResizeWindow(int x, int y, int w, int h)
{
  XWindowChanges changes;

  changes.x = DeltaX + x;
  changes.y = DeltaY + y;
  changes.width = w;
  changes.height = h;
  
  XReconfigureWMWindow(dpy, window, screen,
		       CWX | CWY | CWWidth | CWHeight,
		       &changes);
  
  XFlush(dpy);
}

static void ScaleToFit(void)
{
  double ws, hs, w, h, bx0, bx1, by0, by1, x[4], y[4];
  Bool adjust = False, iter;
  
  HScroll = VScroll = False;
  
  iter = 3;
  
  RotateBox(PSAngle, 0.0, 0.0, BX0, BY0, BX1, BY1, x, y);
  
  bx0 = Min(x, 4);
  by0 = Min(y, 4);
  bx1 = Max(x, 4);
  by1 = Max(y, 4);
  
 start:
  
  ws = PSWidth  / (XScale * (bx1 - bx0));
  hs = PSHeight / (YScale * (by1 - by0));
  
  switch (PSMode) {
  case FixedScale:
    break;
  case FullPage:
    PSScale = Minimum(ws, hs);
    break;
  case FullHeight:
    PSScale = hs;
    break;
  case FullWidth:
    PSScale = ws;
    break;
  }
  
  /*
   * Compute the size of the window
   */
  
  w = ceil((bx1 - bx0) * XScale * PSScale);
  h = ceil((by1 - by0) * YScale * PSScale);
  
  if (iter > 0 &&
      Width > 2.0 * ScrollBarWidth &&   /* No scroll bars on small windows */
      Height > 2.0 * ScrollBarWidth &&
      (h > Height + 1 || w > Width + 1)) {
    
    iter--;
    VScroll = (h + (HScroll ? ScrollBarWidth : 0) > Height);
    HScroll = (w + (VScroll ? ScrollBarWidth : 0) > Width);
    goto start;
    
  }
  
  if (HScroll) {
    h += ScrollBarWidth;
  }
  
  if (VScroll) {
    w += ScrollBarWidth;
  }
  
  if (w < Width && (Initializing || !FixedGeometry) && !ChildMode) {
    adjust = True;
    Width = w;
  }
  
  if (h < Height && (Initializing || !FixedGeometry) && !ChildMode) {
    adjust = True;
    Height = h;
  }
  
  ws = (w - (VScroll ? ScrollBarWidth : 0.0)) / PSWidth;
  hs = (h - (HScroll ? ScrollBarWidth : 0.0)) / PSHeight;

  AdjustRatio(ws, &DXScroll, &MXScroll, &HPercentage);
  AdjustRatio(hs, &DYScroll, &MYScroll, &VPercentage);

  HScroll = HScroll && (MXScroll > 1);
  VScroll = VScroll && (MYScroll > 1);

  if ((Initializing || !FixedGeometry) && !ChildMode) {
    if (Initializing) {
      MoveResizeWindow(PSGX0, PSGY0, (int) Width, (int) Height);
      if (!BatchMode) {
	DeiconifyWindow();
      }
      XFlush(dpy);
    } else if (ForceAdjust) {
      MoveResizeWindow(PSGX0, PSGY0, (int) Width, (int) Height);
    } else if (adjust) {
      ResizeWindow((int) Width, (int) Height);
    }
  }
  
  PSGX0 = PSGY0 = 0;
  ForceAdjust = False;

  BBoxRect.x = floor(Min(x, 4)) * PSScale * XScale;
  BBoxRect.y = floor(Min(y, 4)) * PSScale * YScale;
  BBoxRect.width = ceil(Max(x, 4) - Min(x, 4)) * PSScale * XScale;
  BBoxRect.height = ceil(Max(y, 4) - Min(y, 4)) * PSScale * YScale;
}

static void GetDisplayInfo(void)
{
  int mask, x0, y0, i;
  unsigned int w, h, ui;
  Window aWindow;
  char *vendor;
  
  screen = DefaultScreen(dpy);
  vendor = ServerVendor(dpy);
  
  if (ChildMode) {
    
    XGetGeometry(dpy, parent, &aWindow,
		 &i, &i, &w, &h, &ui, &ui);
    Width = MaxWidth = w;
    Height = MaxHeight = h;
  } else {
    Width = MaxWidth = (double) DisplayWidth(dpy, screen);
    Height = MaxHeight = (double) DisplayHeight(dpy, screen);
  
    if (PSGeometry != NULL) {
      
      x0 = PSGX0;
      y0 = PSGY0;
      
      mask = XParseGeometry(PSGeometry, &PSGX0, &PSGY0, &w, &h);
      
      if ((mask & WidthValue) && w < (int) MaxWidth) {
	FixedWidth = True;
	Width = (double) w;
      }
      
      if ((mask & HeightValue) && h < (int) MaxHeight) {
	FixedHeight = True;
	Height = (double) h;
      }
      
      if (mask & (XNegative | YNegative)) {
	(void) fprintf(stderr,
		       "*** PsView: %s: '-g %s' (ignored)\n",
		       "Negative coordinates not supported",
		       PSGeometry);
      }
      
      if (mask & XNegative) {
	PSGX0 = x0;
      }
      
      if (mask & YNegative) {
	PSGY0 = y0;
      }
    }
  }
  
  ZoomX0 = PSGX0;
  ZoomY0 = PSGY0;
  ZoomWidth = (int) Width;
  ZoomHeight = (int) Height;
  
  XScale = (DisplayWidth(dpy, screen) * 25.4) /
    (DisplayWidthMM(dpy, screen) * 72.0);
  YScale = (DisplayHeight(dpy, screen) * 25.4) /
    (DisplayHeightMM(dpy, screen) * 72.0);

  /*
   * Well, Ultrix 4.2 DPS tends to believe that YScale = XScale...
   */

  if (engine == &DPS_Engine && vendor != NULL &&
      strstr(vendor, "UWS4.2LA") != NULL) {
    YScale = XScale;
  }
}

static void ToLower(char *to, char *from)
{
  while (*from) {
    if (isupper(*from)) {
      *to++ = tolower(*from++);
    } else {
      *to++ = *from++;
    }
  }
  *to = null;
}

static Bool BooleanValue(char *s)
{
  ToLower(Buffer, s);
  
  return (streq(Buffer, "yes") ||
	  streq(Buffer, "on")  ||
	  streq(Buffer, "true"));
}

static void SetFormat(char *format)
{
  char *formats = Formats, *f;
  double x0 = 0.0, y0 = 0.0, x1 = 0.0, y1 = 0.0;
  int i;
  
  /* User-defined formats */
  
  if (formats != NULL) {

    while (*formats != null && ParseBBox(formats, &x0, &y0, &x1, &y1)) {
    
      for (; *formats != null && *formats != '(' ; formats++) {
      }
      
      if (*formats++ != '(') {
	goto error;
      }
      
      for (; *formats == ' ' || *formats == '\t'; formats++) {
      }
      
      for (f = format;
	   *f != null && *f == *formats;
	    f++, formats++) {
      }
      
      if (*f == null) {
	BX0 = x0;
	BY0 = y0;
	BX1 = x1;
	BY1 = y1;
	FixedBBox = True;
	return;
      }
      
      for (; *formats != null && *formats != ')' ; formats++) {
      }
      
      if (*formats++ != ')') {
	goto error;
      }
      
      for (; *formats == ' ' || *formats == '\t' ; formats++) {
      }
    }
  }

  /* Predefined formats */

  for (i = 0 ;
       i < sizeof(PredefinedFormats)/sizeof(*PredefinedFormats) ;
       i++) {
    if (strcmp(PredefinedFormats[i].name, format) == 0) {
      BX0 = 0.0;
      BY0 = 0.0;
      BX1 = PredefinedFormats[i].width;
      BY1 = PredefinedFormats[i].height;
      FixedBBox = True;
      return;
    }
  }
  
  (void) fprintf(stderr, "Unknown format \"%s\" (ignored)\n", format);

  return;
  
 error:
  
  (void) fputs("Incorrect bounding box specification in resource file\n",
	       stderr);
}

void SetEnv(char *var, char *val)
{
  char **env;
  char *equal;
  int i;
  int found = -1;
  int length = 0;
  
  if (environ != NULL) {
    for (env = environ ; *env != NULL ; env++) {
      length++;
    }
  }
  
  for (i = 0 ; i < length ; i++) {
    if (strstr(environ[i], var) == environ[i] &&
	environ[i][strlen(var)] == '=') {
      found = i;
      break;
    }
  }

  if ((equal = (char*) malloc(strlen(var) + strlen(val) + 2)) == NULL) {
    Error(True, False, "Out of memory");
  }

  sprintf(equal, "%s=%s", var, val);

  if (found < 0) {
    if ((env = (char**) malloc((length + 2) * sizeof(char*))) == NULL) {
      Error(True, False, "Out of memory");
    }
    env[0] = equal;
    for (i = 0 ; i < length ; i++) {
      env[i + 1] = environ[i];
    }
    env[length + 1] = NULL;
    environ = env;
  } else {
    environ[found] = equal;
  }
}

static void ParseDefaults(void)
{
  char *d;
  double scale;
  
  if ((d = XGetDefault(dpy, "PsView", "updateDelay")) != NULL) {
    UpdateDelay = atoi(d);
    if (UpdateDelay < 0) {
      UpdateDelay = 0;
    }
  }

  if ((d = XGetDefault(dpy, "PsView", "engine")) != NULL) {
    int i;

    for (i = 0 ; i < sizeof(Engines) / sizeof(*Engines) ; i++) {
      if (streq(d, Engines[i]->Name)) {
	EngineNumber = i;
      }
    }
  }

  if ((d = XGetDefault(dpy, "PsView", "constrainedGeometry")) != NULL) {
    ConstrainedGeometry = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "clip")) != NULL) {
    ViewClip = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "backup")) != NULL) {
    DoBackup = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "backingStore")) != NULL) {
    DoBackingStore = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "fixedGeometry")) != NULL) {
    FixedGeometry = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "printCommand")) != NULL) {
    PrintCommand = d;
  }
  
  if ((d = XGetDefault(dpy, "PsView", "includeFile")) != NULL) {
    IncludeFile = d;
  }
  
  if ((d = XGetDefault(dpy, "PsView", "watchProgress")) != NULL) {
    WatchProgress = BooleanValue(d);
  }

  if ((d = XGetDefault(dpy, "PsView", "smoothScrolling")) != NULL) {
    SmoothScrolling = BooleanValue(d);
  }
  
  if ((d = XGetDefault(dpy, "PsView", "helpFile")) != NULL) {
    HelpFile = d;
  }
  
  if ((d = XGetDefault(dpy, "PsView", "clearPage")) != NULL) {
    ClearPageMode = BooleanValue(d);
  }
  
  if ((d = XGetDefault(dpy, "PsView", "verbose")) != NULL) {
    Verbose = BooleanValue(d);
  }
  
  if ((d = XGetDefault(dpy, "PsView", "bitmapImages")) != NULL) {
    NoImage = ! BooleanValue(d);
  }
  
  if ((d = XGetDefault(dpy, "PsView", "scrollBarWidth")) != NULL) {
    ScrollBarWidth = atoi(d);
    if (ScrollBarWidth < 10) {
      ScrollBarWidth = 10;
    }
  }
  
  if ((d = XGetDefault(dpy, "PsView", "autoUpdate")) != NULL) {
    AutoUpdate = BooleanValue(d);
  }
  
  if ((d = XGetDefault(dpy, "PsView", "mode")) != NULL) {
    ToLower(Buffer, d);
    if (streq(Buffer, "fullpage")) {
      PSMode = FullPage;
    } else if (streq(Buffer, "fullwidth")) {
      PSMode = FullWidth;
    } else if (streq(Buffer, "fullheight")) {
      PSMode = FullHeight;
    } else {
      
      if (sscanf(Buffer, "fixedscale %lf", &scale) == 1 &&
	  scale > 0.0001) {
	PSMode = FixedScale;
	PSScale = scale;
      }
    }
  }
  
  if ((d = XGetDefault(dpy, "PsView", "path")) != NULL) {
    PSPath = d;
  }
  
  if ((d = XGetDefault(dpy, "PsView", "geometry")) != NULL) {
    PSGeometry = d;
  }
  
  if ((d = XGetDefault(dpy, "PsView", "boundingBox")) != NULL) {
    Formats = d;
  }

  if ((d = XGetDefault(dpy, "PsView", "pixmapFileName")) != NULL) {
    PIXMAP = d;
  }

  if ((d = XGetDefault(dpy, "PsView", "postScriptFileName")) != NULL) {
    POSTSCRIPT = d;
  }
}

static int OpenSocket(char *path)
{
  int sock, namelen;
  struct sockaddr_un address;
  int fd;
  
  /*
   * Socket creation
   */
  
  if ((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
    Error(True, False, "Cannot create socket");
  }

  /*
   * Unlink the socket if it already exists
   */

  (void) unlink(path);
  
  /*
   * Socket binding
   */
  
  address.sun_family = AF_UNIX;
  (void) strcpy (address.sun_path, path);
  namelen = strlen(path) + sizeof(address.sun_family);

  if ((bind(sock, (struct sockaddr *)&address, namelen)) == -1) {
    Error(True, False, "Cannot bind socket");
  }

  /*
   * Socket listen
   */
  
  if (listen(sock, 8) == -1) {
    Error(True, False, "Cannot listen to socket");
  }

  /*
   * Socket wait for connection
   */

  if ((fd = accept(sock, (struct sockaddr *) &address, &namelen)) == -1) {
    Error(True, False, "Cannot accept connections");
  } else {
    return fd;
  }
}

static void OpenDisplay(int argc, char **argv)
{

  /*
   * Get the *last* display specification
   */
  
  while (argc > 0) {
    if (argv[0][0] == '-' && argv[0][1] == 'd' && argc >= 2) {
      display = argv[1];
      argc -= 2;
      argv += 2;
    } else {
      argc--;
      argv++;
    }
  }
  
  if ((dpy = XOpenDisplay(display)) == NULL) {
    Error(True, False, "Cannot open display");
  }

  SetEnv("DISPLAY", DisplayString(dpy));  
  XSocket = ConnectionNumber(dpy);
}

static Bool OptionValue(char *option, Bool previous)
{
  return option[2] == '+' ? True : option[2] == '-' ? False : previous;
}

static void ParseArguments(int argc, char **argv)
{
  char *file_name = NULL;
  char *format = NULL;
  Bool presentation, prolog = False;
  double scale;
  char c;
  
  while (argc > 0) {
    if (argv[0][0] == '-' && argv[0][1] != null) {
      switch (argv[0][1]) {
      case 'B':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  format = argv[1];
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'v':
	Verbose = OptionValue(argv[0], !Verbose);
	argc--;
	argv++;
	break;
      case 'C':
	ClearPageMode = OptionValue(argv[0], !ClearPageMode);
	argc--;
	argv++;
	break;
      case 'l':
	LowerWindow = OptionValue(argv[0], !LowerWindow);
	argc--;
	argv++;
	break;
      case 'L':
	SmoothScrolling = OptionValue(argv[0], !SmoothScrolling);
	argc--;
	argv++;
	break;
      case 'G':
	FixedGeometry = OptionValue(argv[0], !FixedGeometry);
	argc--;
	argv++;
	break;
      case 'i':
	NoImage = !OptionValue(argv[0], NoImage);
	argc--;
	argv++;
	break;
      case 'W':
	WatchProgress = OptionValue(argv[0], !WatchProgress);
	argc--;
	argv++;
	break;
      case 'u':
	AutoUpdate = OptionValue(argv[0], !AutoUpdate);
	argc--;
	argv++;
	break;
      case 'P':
	presentation = OptionValue(argv[0], True);
	if (presentation) {
	  c = argv[0][strlen(argv[0]) - 1];
	  switch (c) {
	  case '4':
	    PresentationMode = 4;
	    break;
	  case '3':
	    PresentationMode = 3;
	    break;
	  case '2':
	    PresentationMode = 2;
	    break;
	  default:
	    PresentationMode = 1;
	    break;
	  }
	}
	argc--;
	argv++;
	break;
      case 'U':
	UnstructuredMode = OptionValue(argv[0], True);
	argc--;
	argv++;
	break;
      case 'q':
	AutoQuitMode = OptionValue(argv[0], True);
	argc--;
	argv++;
	break;
      case 'Q':
	AutoDieMode = OptionValue(argv[0], True);
	argc--;
	argv++;
	break;
      case 'f':
	PSMode = FullPage;
	argc--;
	argv++;
	break;
      case 'w':
	PSMode = FullWidth;
	argc--;
	argv++;
	break;
      case 'h':
	PSMode = FullHeight;
	argc--;
	argv++;
	break;
      case '!':
	ChildGeometry = True;
	argc--;
	argv++;
	break;
      case 'b':
	if (argc < 5) {
	  Error(True, True, NULL);
	} else {
	  format = NULL;
	  FixedBBox = True;
	  BX0 = atof(argv[1]);
	  BY0 = atof(argv[2]);
	  BX1 = atof(argv[3]);
	  BY1 = atof(argv[4]);
	  argc -= 5;
	  argv += 5;
	}
	break;
      case 'D':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  UpdateDelay = atoi(argv[1]);
	  if (UpdateDelay < 0) {
	    UpdateDelay = 0;
	  }
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'p':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  PSFirstPage = atoi(argv[1]);
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'e':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  int i;

	  for (i = 0 ; i < sizeof(Engines) / sizeof(*Engines) ; i++) {
	    if (streq(argv[1], Engines[i]->Name)) {
	      EngineNumber = i;
	      goto found;
	    }
	  }

	  Error(True, True, "Unknown engine: \"%s\"", argv[1]);

	 found:

	  argc -= 2;
	  argv += 2;
	}
	break;
	break;
      case 'g':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  PSGeometry = argv[1];
	  ConstrainedGeometry = False;
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 's':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  if ((scale = atof(argv[1])) > 0.0001) {
	    PSMode = FixedScale;
	    PSScale = scale;
	  }
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 't':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  (void) strcpy(PSWindowName, argv[1]);
	  UserTitle = True;
	  argc -= 2;
	  argv += 2;
	}
	break;	
      case 'd':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  display = argv[1];
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'R':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {
	  PSAngle = - atof(argv[1]);
	  PSAngle = Normalize(PSAngle);
	  argc -= 2;
	  argv += 2;
	}
	break;
      case '#':
	if (argc < 2) {
	  Error(True, True, NULL);
	} {
	  if (strstr(argv[1], "0x") == argv[1]) {
	    (void) sscanf(argv[1] + 2, "%lx", &parent);
	  } else {
	    (void) sscanf(argv[1], "%lu", &parent);
	  }
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'I':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {                /* Prolog name */
	  ReadPrologue(argv[1], True);
	  prolog = True;
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'J':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {                /* Prolog name */
	  ReadPrologue(argv[1], False);
	  prolog = True;
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'S':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {                /* Socket connection */
	  SocketName = argv[1];
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'o':
	if (argc < 2) {
	  Error(True, True, NULL);
	} else {                /* Output ppm file */
	  BatchMode = True;
	  PIXMAP = argv[1];
	  argc -= 2;
	  argv += 2;
	}
	break;
      case 'H':
	file_name = HelpFile;
	HelpMode = True;
	(void) strcpy(PSWindowName, "PsView Help");
	argc--;
	argv++;
	break;
      default:
	Error(True, True, argv[0]);
	break;
      }
    } else {
      if (!HelpMode) {
	file_name = argv[0];
      }
      argc--;
      argv++;
    }
  }

  if (PresentationMode != 0) {
    WatchProgress = False;  /* To allow the fade-out effect */
  }
  
  if (BatchMode) {
    if (ChildMode) {
      Error(True, False, "-# not allowed in batch mode");
    }
    if (LowerWindow) {
      Error(True, False, "-l not allowed in batch mode");
    }
    if (PresentationMode) {
      Error(True, False, "-P not allowed in batch mode");
    }
    if (StreamMode) {
      Error(True, False, "-o not allowed in stream mode");
    }
    WatchProgress = False;
    SmoothScrolling = True;
  }
  
  if (format != NULL && format[0] != null) {
    SetFormat(format);
  }
  
  if (file_name != NULL) {

    /* File */
    ReadFile(file_name);
  
  } else {
    /* Stream mode => stdin or socket input */
    
    StreamMode = True;

    if (SocketName == NULL) {
      std_in = 0;
      if (((StdInStatus = fcntl(std_in, F_GETFL, 0)) == -1) ||
	  (fcntl(std_in, F_SETFL, StdInStatus | O_NONBLOCK) == -1)) {
	Error(True, False, "Cannot fcntl on stdin");
      }
    } else {
      std_in = OpenSocket(SocketName); /* socket */
    }
    
    /*
     * Stream mode is unstructured
     */
    
    UnstructuredMode = True;

    PSMaxLength = 1;
    PSText = (char*) Malloc(PSMaxLength);
    PSText[0] = null;
    PSTextLength = 0;
    PSTrailerLength = 0;

    ComputePages();
    
    if (PSWindowName[0] == null) {
      (void) strcpy(PSWindowName,
		    (SocketName == NULL) ? "Stream Mode" : SocketName);
    }
    
  }

  /*
   * Include file
   */
  
  if (StreamMode && IncludeFile != NULL) {
    ReadPrologue(IncludeFile, True);
  }
  
  /*
   * Force the pixmap mode
   */
  
  if (UnstructuredMode && !StreamMode) {
    WatchProgress = False;
  }

}

static void Duplicate(PageMode mode,
		      double X0, double Y0, double X1, double Y1,
		      int GX0, int GY0,
		      int GWidth, int GHeight,
		      Bool sw)
{
  int i, j;
  char **argv, scale[20], page[20], *dot, disp[100], angle[20],
  x0[20], y0[20], x1[20], y1[20], geometry[100], delay[20];
  Display *d;
  int gx0, gy0;

  (void) strcpy(disp, XDisplayString(dpy));
  
  if (sw) {
    dot = strchr(disp, '.');
    if (dot != NULL) {
      if (dot[1] == null) {
	(void) strcat(disp, "1");
      } else {
	dot[1] = '0' + ('1' - dot[1]);
      }
    } else {
      (void) strcat(disp, ".1");
    }
    
    if ((d = XOpenDisplay(disp)) != NULL) {
      XCloseDisplay(d);
    } else {
      Beep(1);
      return;
    }
  }
  
  argv = (char**) Malloc((34 + 2 * Prologues) * sizeof(char*));
  
  i = 0;
  
  argv[i++] = PsView;
  argv[i++] = PSFileName;
  
  (void) sprintf(scale, "%.4f", PSScale);
  (void) sprintf(angle, "%.4f", -PSAngle);
  (void) sprintf(page, "%d", PSPageNumber);
  (void) sprintf(x0, "%.4f", X0);
  (void) sprintf(y0, "%.4f", Y0);
  (void) sprintf(x1, "%.4f", X1);
  (void) sprintf(y1, "%.4f", Y1);
  (void) sprintf(delay, "%d", UpdateDelay);
  
  GetWindowPosition(&gx0, &gy0);
  
  (void) sprintf(geometry, "%dx%d+%d+%d",
		 GWidth, GHeight, gx0 + GX0, gy0 + GY0);
  
  switch (mode) {
  case FixedScale:
    argv[i++] = "-s";
    argv[i++] = scale;
    break;
  case FullPage:
    argv[i++] = "-f";
    break;
  case FullHeight:
    argv[i++] = "-h";
    break;
  case FullWidth:
    argv[i++] = "-w";
    break;
  }
  
  if (ChildMode) {
    argv[i++] = "-!";
  }
  
  argv[i++] = "-e";
    argv[i++] = engine->Name;
  
  if (UserTitle) {
    argv[i++] = "-t";
    argv[i++] = PSWindowName;
  }
  
  argv[i++] = "-D";
  argv[i++] = delay;
  argv[i++] = "-R";
  argv[i++] = angle;
  argv[i++] = "-d";
  argv[i++] = disp;
  argv[i++] = "-p";
  argv[i++] = page;
  argv[i++] = "-b";
  argv[i++] = x0;
  argv[i++] = y0;
  argv[i++] = x1;
  argv[i++] = y1;
  argv[i++] = "-g";
  argv[i++] = geometry;
  
  argv[i++] = ClearPageMode ? "-C+" : "-C-";
  argv[i++] = SmoothScrolling ? "-L+" : "-L-";
  argv[i++] = FixedGeometry ? "-G+" : "-G-";
  argv[i++] = NoImage ? "-i-" : "-i+";
  argv[i++] = WatchProgress ? "-W+" : "-W-";
  argv[i++] = AutoUpdate ? "-u+" : "-u-";
  argv[i++] = PresentationMode != 0 ? "-P+" : "-P-";
  argv[i++] = UnstructuredMode ? "-U+" : "-U-";
  argv[i++] = Verbose ? "-v+" : "-v-";
  
  for (j = 0 ; j < Prologues ; j += 2) {
    argv[i + j] = PSPrologue[j].before ? "-I" : "-J";
    argv[i + j + 1] = PSPrologue[j].name;
  }
  
  i += 2 * Prologues;
  
  argv[i] = NULL; /* NULL terminated array */

  /*
   * Note that fork() would do the job, but vfork() is more efficient
   */
  
  if (vfork() == 0) {
    (void) execvp(argv[0], argv);
    (void) fprintf(stderr, "Cannot execvp\n");
    _exit(EXIT_FAILURE);
  }
  
  Free(argv);

  /* Destroy process */
  
  if (sw) {
    Exit(EXIT_SUCCESS);
  }
}

static void AddTitle(XEvent *e)
{
  int i, length;
  
  length = XLookupString((XKeyEvent*) e, Buffer, BUFFERLEN, NULL, NULL);
  
  if (length > 0) {
    for (i = 0; i < length; i++) {
      Buffer[i] = isprint(Buffer[i]) ? Buffer[i] : ' ';
    }
    Buffer[length] = null;
    (void) strcat(PSWindowName, Buffer);
    SetWindowName(PSWindowName);
  }
}

/*
 * We maintain two queues:
 *
 * - for key events    (low priority)
 * - for other events  (hight priority)
 *
 * Operations:
 *
 * - Queue(Empty)
 * - Queue(Add, XEvent *e)
 * - Queue(Remove, XEvent *e)
 * - Queue(PutBack, XEvent *e)
 * - Queue(Discard, XEvent *e)
 */

#define VAL(X, i) (X[queue][(first[queue] + i) % maxlen[queue]])
			      
static Bool Queue(QueueAction action,...)
{
  XEvent *e, *events2;
  va_list args;
  int i, j, queue, type;
  static maxlen[2] = {100, 100};
  static XEvent *events[2] = {NULL, NULL};
  static int first[2] = {0, 0};
  static int count[2] = {0, 0};

  /*
   * We say that the queue is empty if:
   * 1) There are no key events (i.e. user commands), or there are
   *    some, but the initialization process is not terminated
   * 2) There are not other events
   */
  
  if (action == Empty) {
    return ((Initializing || count[0] == 0) && count[1] == 0);
 }  else {
    va_start(args, action);
    e = va_arg(args, XEvent*);
    if (action == Discard) {
      type = va_arg(args, int);
    }
    va_end(args);
  }
  
  /*
   * Allocate the queue during the first call
   */
  
  if (events[0] == NULL) {
    events[0] = (XEvent*) Malloc(maxlen[0] * sizeof(XEvent));
  }
  
  if (events[1] == NULL) {
    events[1] = (XEvent*) Malloc(maxlen[1] * sizeof(XEvent));
  }
  
  /*
   * Determine which queue must be used, according to the event's priority
   */

  if (action == Discard) {
    /* Do not discard key events */
    queue = 1;
  } else if (action == Add || action == PutBack) {
    /* Add */
    queue = (e->type == KeyPress) ? 0 : 1;
  } else {
    /* Remove */
    queue = (count[1] != 0) ? 1 : 0;
  }
  
  /*
   * Allocate a larger queue if necessary
   */
  
  if ((action == Add || action == PutBack) && count[queue] == maxlen[queue]) {
    events2 = (XEvent*) Malloc(2 * maxlen[queue] * sizeof(XEvent));
    for (i = 0; i < maxlen[queue] ; i++) {
      events2[i] = VAL(events, i);
    }
    first[queue] = 0;
    Free(events[queue]);
    maxlen[queue] *= 2;
    events[queue] = events2;
  }
  
  switch (action) {
  case Add:
    VAL(events, count[queue]) = *e;
    count[queue]++;
    break;
  case PutBack:
    first[queue] = (maxlen[queue] + first[queue] - 1) % maxlen[queue];
    VAL(events, 0) = *e;
    count[queue]++;
    break;
  case Discard:
    for (i = 0 ; i < count[queue] ; ) {
      if (VAL(events, i).type == type) {
	*e = VAL(events, i);
	count[queue]--;
	for (j = i ; j < count[queue] ; j++) {
	  VAL(events, j) = VAL(events, j + 1);
	}
      } else {
	i++;
      }
    }
    break;
  case Remove:
    if (count[queue] == 0) {
      return False;
    } else {
      *e = VAL(events, 0);
      first[queue] = (first[queue] + 1) % maxlen[queue];
      count[queue]--;
    }
    break;
  default:
    break;
  }
  
  return True;
}

#undef VAL

/*
 * X Event Handling
 */

static void DiscardEvents(int type, XEvent *e)
{
  XEvent c;

  Queue(Discard, e, type);
  
  while (XCheckTypedEvent(dpy, type, &c)) {
    *e = c;
  }
}

static Bool ReadEvent(XEvent *e)
{

  XNextEvent(dpy, e);

  if ((e->type == KeyPress && !TitleMode && !PrintMode &&
       XLookupKeysym((XKeyEvent*) e, e->xkey.state) == XK_q) ||
      (e->type == ClientMessage &&
       e->xclient.message_type == XA_WM_PROTOCOLS &&
       e->xclient.data.l[0] == XA_WM_DELETE_WINDOW)) {
    
    /*
     * Low level exit with the "q" key or window manager "DELETE_WINDOW"
     */
    
    Exit(EXIT_SUCCESS);
    
  } else {

    return !(*engine->Dispatch)(e);
  }
    
}

/*
 * High level event handling
 */

static Bool GetNextEvent(Bool block, XEvent *e)
{
  fd_set rmask, wmask, emask;
  Bool eventRead = False;
  int nfds, rfd, wfd;
  struct timeval _timeout, *timeout;
  int code;
  
  /*
   * Here, we have to wait for both the X socket and our input stream (if any).
   * We have to be very careful not to block on XNextEvent when error events
   * or extension events are received. This would block the display process.
   * We therefore use the "XPending" function to test that events are
   * actually queued. Since "XPending" (and "XNextEvent" for that matter)
   * calls the PostScript extension callback procedures,
   * no deadlock should occur (let's pray!).
   * Finally, a timeout is added to the select() call in order to check the
   * status of the file every second or rotate the cursor.
   */
   
  (*engine->Synchronize)();
      
  if (block) {

    do {

      XFlush(dpy);     /* To avoid deadlocks */
      
      FD_ZERO(&rmask);
      FD_ZERO(&wmask);
      FD_ZERO(&emask);
      
      FD_SET(XSocket, &rmask);
      FD_SET(XSocket, &emask);
      nfds = XSocket;
      
      if (!Initializing && StreamMode && !EndOfStream) {
	
	FD_SET(std_in, &rmask);
	FD_SET(std_in, &emask);
	nfds = Maximum(std_in, XSocket);
	
      }

      if (!Initializing) {
	
	(*engine->Select)(&rfd, &wfd);

	if (rfd != -1) {
	  FD_SET(rfd, &rmask);
	  FD_SET(rfd, &emask);
	  nfds = Maximum(rfd, XSocket);
	}

	if (wfd != -1) {
	  FD_SET(wfd, &wmask);
	  FD_SET(wfd, &emask);
	  nfds = Maximum(wfd, XSocket);
	}
      }
      
      if (RotatingCursor) {

	_timeout.tv_sec = 0;
	_timeout.tv_usec = TIMEOUT;
	timeout = &_timeout;
	
      } else if (StreamMode) {

	timeout = (struct timeval *) NULL;
      
      } else {

	_timeout.tv_sec = 1;
	_timeout.tv_usec = 0;
	timeout = &_timeout;
	
      }

      if ((XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	   (FD_ZERO(&rmask), FD_ZERO(&wmask),
	    FD_ZERO(&emask), code = 1, True)) ||
	  (code = select(nfds + 1, &rmask, &wmask, &emask, timeout)) > 0) {
	
	/*
	 * Error on the X socket: either an error, or nothing to read
	 * This is a hack to avoid infinite loop when the server dies...
	 */

	if (FD_ISSET(XSocket, &emask) ||
	    (FD_ISSET(XSocket, &rmask) && !CanRead(XSocket))) {
	  
	  IOErrorHandler(dpy);
	}
	
	/*
	 * The XPending call is necessary since there might be
	 * something to read from the X server, but nothing available
	 * to the user through a "XNextEvent" call (e.g. error reporting,
	 * PostScript extension status reporting, etc).
	 */
	
	if (XPending(dpy) > 0) {
	  eventRead = ReadEvent(e);
	}

	/*
	 * Read/write from the appropriate file descriptors
	 */
	
	if (!Initializing) {

	  if (StreamMode && !EndOfStream) {
	    if (FD_ISSET(std_in, &rmask)) {
	      ReadStream();
	    } else if (FD_ISSET(std_in, &emask)) {
	      EndOfStream = True;
	    }
	  }

	  if (rfd != -1) {
	    if (FD_ISSET(rfd, &emask)) {
	      Error(True, False, "Read error");
	    } else if (FD_ISSET(rfd, &rmask)) {
	      (*engine->ReadProc)();
	    }
	  }
	    
	  if (wfd != -1) {
	    if (FD_ISSET(wfd, &emask)) {
	      Error(True, False, "Write error");
	    } else if (FD_ISSET(wfd, &wmask)) {
	      (*engine->WriteProc)();
	    }
	  }
	}
	
      } else if (code != 0 && errno != EINTR) {
	/* Error */
	IOErrorHandler(dpy);
      }

      /*
       * Check that the interpreter is alive
       */
      
      if (Printing && !(*engine->Alive)()) {
	Error(True, False, "Engine failure");
      }
      
      /*
       * In child mode, if the father die, then die.
       */
      
      if (AutoDieMode && getppid() == 1) {
	Exit(EXIT_FAILURE);
      }
      
      /*
       * Exit if all the data has been read and displayed
       */
      
      if (StreamMode && AutoQuitMode && EndOfStream && Done) {
	Exit(EXIT_SUCCESS);
      }
      
      /*
       * Rotate the cursor when waiting
       */

      if (RotatingCursor) {
	ChangeWaitCursor(code == 0);
      }
      
      /*
       * If the file has changed and is "stable" then update it.
       */
      
      if (!Initializing && !Printing &&
	  !StreamMode && ButtonPressed == 0 &&
	  (FileHasChanged() || FileOutOfDate)) {
	
	if (AutoUpdate) {
	  
	  CursorDirection = 1;
	  ScrollPrevious = True; /* To enforce scrolling */
	  SetCursor(WatchCursor, False);
	  ReadFile(PSFileName);
	  RestartContext();
	  
	} else if (!FileOutOfDate) {
	  
	  FileOutOfDate = True;
	  SetWindowName(NULL);
	  
	}
	
	XFlush(dpy);
	
      }
      
      /*
       * Send some more PostScript (if any and if not printing)
       */

      if (!Initializing && StreamMode &&
	  !Printing && PSTextPosition < PSTextLength) {
	
	Printing = True;
	SetCursor(WaitCursor, True);
	(*engine->SendData)(True);
	SendData();
	
      }

      (*engine->Synchronize)();
	
     } while (!eventRead ||
	     (Printing && (e->type == ButtonPress ||
			   e->type == MotionNotify ||
			   e->type == ButtonRelease)));
    return True;
    
  } else if (XPending(dpy) > 0) {
    
    return ReadEvent(e);

  } else {
    
    return False;
  }
}

static void WaitNextEvent(XEvent *e)
{
  /*
   * While printing, we have to wait for the context to terminate. We shall
   * then send a dummy "ClientMessage" event to PsView window
   * so that it unblocks the "XNextEvent" call.
   */
  
  while (Printing || Queue(Empty)) {
    (void) GetNextEvent(True, e);
    (void) Queue(Add, e);
  }
  
  (void) Queue(Remove, e);
}

main(int argc, char *argv[])
{
  XEvent e, c;
  int width, height;
  KeySym keysym;
  double hsize, vsize;
  int last_width = 0, last_height = 0;
  Bool horizontal, firstConfigure = True, reparented = False;
  char windowName[PATH_MAX];

  /*
   * Select a default engine
   */
  
#ifdef DPS
  EngineNumber = 0;
#else
  EngineNumber = 1;
#endif
  
  PsView = argv[0];
  PSFileName[0] = null;         /* No input file */
  PSWindowName[0] = null;       /* Window name */
  PrintCommand  = PRINTCOMMAND; /* Print command */

  InstallHandlers();
  OpenDisplay(argc - 1, argv + 1);
  ParseDefaults();
  ParseArguments(argc - 1, argv + 1);
  GetDisplayInfo();
  CreateWindow();
  
  for (;;) {

    /*
     * Wait for the end of printing and get the next X event
     */
    
    WaitNextEvent(&e);
    
    switch (e.type) {
      
    case ReparentNotify:
      
      /*
       * When the window is reparented, we move it "by hand" to see where it
       * really goes after a "MoveWindow" operation. We will then be able to
       * analyze its geometry and determine the dimensions of the decoration
       * added by the window manager. We use two different sizes (DummySize1
       * and DummySize2) to ensure that a "ConfigureNotify" event will be
       * generated.
       */
      
      if (Initializing && !BatchMode && !ChildMode) {
        reparented = True;
	MoveResizeWindow(0, 0, DummySize2, DummySize2);
      }
      
      break;
      
    case ConfigureNotify:

      width  = e.xconfigure.width;
      height = e.xconfigure.height;
	
      if (!BatchMode) {
	if (firstConfigure && width == DummySize2) {
	
	  firstConfigure = False;
	
	  /*
	   * We now get the actual geometry of the window after the move
	   * (cf. supra), and start the PostScript context
	   */
	
	  GetActualGeometry();
	
	  StartContext(UnstructuredMode ?
		       PSFirstPage :
		       Minimum(PSFirstPage, PSPageMaxNumber));
	
	  SetWindowName(NULL);

	  Initializing = False;

	} else if (!Initializing) {
	
	  DrawCross(True, 0, 0);
	
	/*
	 * Restart the context only if a new size is requested (i.e.
	 * the configure notify is not an aknowledgement from the window
	 * manager) and if this is not the second identical notification
	 * (to avoid looping with the window manager...).
	 */
	
	  if ((width != (int) Width || height != (int) Height) &&
	      (FixedGeometry || ChildMode ||
	       width != (int) last_width ||
	       height != (int) last_height)) {

	    if (!ChildMode) {
	      SaveContext();
	    }

	    Width  = (double) width;
	    Height = (double) height;

	    if (ChildMode) {
	      MaxWidth = Width;
	      MaxHeight = Height;
	    }
	  
	    RestartContext();
	  }
	}
      }
      
      last_width  = width;
      last_height  = height;
      
      break;

    case MapNotify:

      /*
       * In child mode, no "ReparentNotify" will be sent, and therefore, we
       * must start the interpreter when the "MapNotify" event is received.
       * Otherwise, wait until the interpreter is started.
       */

      if (Initializing && !BatchMode && ChildMode) {

	firstConfigure = False;

	GetActualGeometry();

	StartContext(UnstructuredMode ?
		     PSFirstPage :
		     Minimum(PSFirstPage, PSPageMaxNumber));
	
	SetWindowName(NULL);
	
	Initializing = False;
	reparented = True;
	
      }
      
      break;
      
    case Expose:

      if (!Initializing && !BatchMode && reparented) {
	if (e.xexpose.window == window) {
	  DrawScrollBars(window);
	} else if (e.xexpose.window == pswindow) {
	  CursorDirection = 1;
	  DisplayPage(True);
	}
      }
      break;
      
    case ButtonPress:
      
      if (!Initializing) {

	DrawCross(True, 0, 0);

	if (PresentationMode != 0 && e.xbutton.button == Button1) {
	  GotoPage(True, PSPageNumber - 1);
	} else if (PresentationMode != 0 && e.xbutton.button == Button3) {
	  GotoPage(True, PSPageNumber + 1);
	} else if (ButtonPressed == 0) {
	  if (InThumb(e.xbutton.x, e.xbutton.y, &horizontal, &ThumbDrag)) {
	    if (ThumbDrag) {
	      ButtonPressed++;
	      DragThumb(BeginDrag, e.xbutton.x, e.xbutton.y, horizontal);
	    }
	  } else {
	    ButtonPressed++;
	    RX0 = RX1 = e.xbutton.x;
	    RY0 = RY1 = e.xbutton.y;
	    HandleSelection(True,
			    e.xbutton.state & (ShiftMask|ControlMask),
			    False);
	  }
	} else {
	  ButtonPressed++;
	}
      }
      
      break;
      
    case MotionNotify:

      DiscardEvents(MotionNotify, &e);
      
      if (!Initializing && !Printing) {
	
	if (ButtonPressed > 0) {
	  if (ThumbDrag) {
	    DragThumb(DoDrag, e.xmotion.x, e.xmotion.y, horizontal);
	  } else {
	    if (e.xmotion.x != RX1 || e.xmotion.y != RY1) {
	      HandleSelection(False, 0, False);
	      RX1 = e.xmotion.x;
	      RY1 = e.xmotion.y;
	      HandleSelection(True, 0, False);
	    }
	  }
	} else {
	  
	  SetCrossCursor();
	  
	  if ((e.xmotion.state & (ControlMask | ShiftMask)) &&
	      !(e.xmotion.state & ~(ControlMask | ShiftMask))) {
	    DrawCross(False, e.xmotion.x, e.xmotion.y);
	  }
	}
      }
      
      break;
      
    case ButtonRelease:
      
      if (!Initializing && ButtonPressed == 1) {
	
	SetCrossCursor();
	
	if (ThumbDrag) {
	  
	  DragThumb(EndDrag, e.xbutton.x, e.xbutton.y, horizontal);
	  ThumbDrag = False;
	  
	} else {
	  
	  SetWindowName(NULL);

	  if (e.xbutton.state & (ControlMask | ShiftMask)) {
	    HandleSelection(False, e.xbutton.state, True);
	  } else {
	    HandleSelection(False, 0, False);
	  }
	}
      }
      
      if (!Initializing && ButtonPressed > 0) {
	ButtonPressed--;
      }
      
      break;
      
    case LeaveNotify:
    case KeyRelease:

      DrawCross(True, 0, 0);
      break;
      
    case KeyPress:
      
      DrawCross(True, 0, 0);

      keysym = XLookupKeysym((XKeyEvent*) &e, e.xkey.state & ShiftMask);
      
      if (!Initializing) {
	if (TitleMode || PrintMode) {

	  switch (keysym) {

	  case XK_BackSpace:
	  case XK_Delete:
	    if (strlen(PSWindowName) > 0) {
	      PSWindowName[strlen(PSWindowName) - 1] = null;
	      if (PSWindowName[0] == null) {
		SetWindowName(" ");
	      } else {
		SetWindowName(PSWindowName);
	      }
	    } else {
	      Beep(1);
	    }
	    break;

	  case XK_Return:
	  case XK_KP_Enter:
	  case XK_Escape:

	    if (TitleMode) {

	      TitleMode = False;

	      if (PSWindowName[0] == null) { /* Revert to former window name */
		(void) strcpy(PSWindowName, windowName);
	      } else {
		UserTitle = True;
	      }
	      
	      SetWindowName(NULL);
	      SetIconName();
	      
	    } else {

	      
	      if (PSWindowName[0] != null) {

		if (ExecutePrintCommand) {
		  int page1, page2;
		  char *command, *old_command;
		  
		  if (sscanf(PSWindowName, "%d-%d", &page1, &page2) == 2 &&
		      (command = strchr(PSWindowName, ':')) != NULL) {
		    
		    old_command = PrintCommand;
		    PrintCommand = command + 1;
		    
		    ExecutePrintCommand = False;
		    
		    PrintPages(page1, page2);

		    ClearArgument();
		    PrintCommand = old_command;

		  } else {

		    ExecutePrintCommand = False;
		  }
		  
		} else {
		  (void) strcpy(UserPrintCommand, PSWindowName);
		  PrintCommand = UserPrintCommand;
		}
	      }
	      
	      PrintMode = False;
	      
	      (void) strcpy(PSWindowName, windowName);
	      SetWindowName(NULL);

	    }
	    
	    break;

	  default:
	    AddTitle(&e);
	    break;
	  }

	} else {
	  switch (keysym) {

	  case XK_Q:
	    Exit(EXIT_SUCCESS);
	    break;
	    
	  case XK_question:
	  case XK_Help:
	    if (!HelpMode) {
	      (void) sprintf(Buffer, "%s -H -d %s &",
			     PsView, XDisplayString(dpy));
	      System(Buffer);
	    } else {
	      Beep(1);
	    }
	    break;
	    
	  case XK_v:
	    (void) sprintf(Buffer, "PsView %s", VERSION);  
	    SetWindowName(Buffer);
	    (void) sleep(2);
	    SetWindowName(NULL);
	    break;
	  
	  case XK_Escape:
	  case XK_t:
	    if (ChildMode || HelpMode) {
	      Beep(1);
	    } else {
	      TitleMode = True;
	      (void) strcpy(windowName, PSWindowName);
	      PSWindowName[0] = null;
	      SetWindowName("Enter title...");
	    }
	    break;
	  
	  case XK_u:
	    if (StandardInputMode || StreamMode) {
	      Beep(1);
	    } else if (AutoUpdate) {
	      AutoUpdate = False;
	      SetWindowName(NULL);
	    } else {
	      AutoUpdate = True;
	      SetWindowName(NULL);
	    }
	    break;
	  
	  case XK_L:
	    SmoothScrolling = !SmoothScrolling;
	    if (!WatchProgress) {
	      ScrollPrevious = True;
	      RestartContext();
	    }
	    break;
	  
	  case XK_minus:
	    NumNegative = !NumNegative;
	    break;
	  
	  case XK_z:
	    hsize = (BX1 - BX0) / 2.0;
	    vsize = (BY1 - BY0) / 2.0;
	    SaveContext();
	    SetBBox(BX0 - hsize, BX1 + hsize, BY0 - vsize, BY1 + vsize);
	    break;
	  
	  case XK_period:
	    BuildNumber(-1);
	    break;
	  
	  case XK_0:
	  case XK_KP_0:
	    BuildNumber(0);
	    break;
	  
	  case XK_1:
	  case XK_KP_1:
	    BuildNumber(1);
	    break;
	  
	  case XK_2:
	  case XK_KP_2:
	    BuildNumber(2);
	    break;
	  
	  case XK_3:
	  case XK_KP_3:
	    BuildNumber(3);
	    break;
	  
	  case XK_4:
	  case XK_KP_4:
	    BuildNumber(4);
	    break;
	  
	  case XK_5:
	  case XK_KP_5:
	    BuildNumber(5);
	    break;
	  
	  case XK_6:
	  case XK_KP_6:
	    BuildNumber(6);
	    break;
	  
	  case XK_7:
	  case XK_KP_7:
	    BuildNumber(7);
	    break;
	  
	  case XK_8:
	  case XK_KP_8:
	    BuildNumber(8);
	    break;
	  
	  case XK_9:
	  case XK_KP_9:
	    BuildNumber(9);
	    break;
	  
	  case XK_d:
	    if (StandardInputMode || StreamMode || ChildMode) {
	      Beep(1);
	    } else {
	      Duplicate(PSMode, BX0, BY0, BX1, BY1,
			0, 0, (int) Width, (int) Height, True);
	    }
	    break;
	  
	  case XK_D:
	    if (StandardInputMode || StreamMode || ChildMode) {
	      Beep(1);
	    } else {
	      Duplicate(PSMode, BX0, BY0, BX1, BY1,
			0, 0, (int) Width, (int) Height, False);
	    }
	    break;
	  
	  case XK_U:
	    CursorDirection = 1;
	    DisplayPage(False);
	    break;

	  case XK_c:
	    BigArrowMode = !BigArrowMode;
	    SetCursor(None, 0);
	    break;
	  
	  case XK_Next:
	  case XK_n:
	    if (NumArg == 0.0) {
	      GotoPage(True, PSPageNumber + 1);
	    } else {
	      GotoPage(True, PSPageNumber + (int) NumArg);
	    }
	    break;
	  
	  case XK_Prior:
	  case XK_p:
	  case XK_b:
	    if (keysym == XK_p &&
		(e.xkey.state & ControlMask)) { /* Control-p */
	      if (HelpMode) {
		Beep(1);
	      } else {
		EnterPrintCommand(False, windowName);
	      }
	    } else if (NumArg == 0.0) {
	      GotoPage(True, PSPageNumber - 1);
	    } else {
	      GotoPage(True, PSPageNumber - (int) NumArg);
	    }
	    break;
	  
	  case XK_P:
	    if (keysym == XK_P
		&& (e.xkey.state & ControlMask)) { /* Control-p */
	      if (HelpMode) {
		Beep(1);
	      } else {
		EnterPrintCommand(True, windowName);
	      }
	    } else if (StreamMode) {
	      SaveStream(True);
	    } else {
	      int pages = (int) NumArg;

	      pages = (pages == 0) ? 1 : pages;
	    
	      PrintPages(PSPageNumber, PSPageNumber + pages - 1);
	      ClearArgument();
	    }
	    break;
	  
	  case XK_G:
	    FixedGeometry = !FixedGeometry;
	    SetWindowName(NULL);
	    break;
	  
	  case XK_Home:
	    Home();
	    DisplayPage(False);
	    break;
	  
	  case XK_g:
	    if (NumArg == 0.0) {
	      if (UnstructuredMode) {
		GotoPage(True, MAXPAGES + 1);
	      } else {
		GotoPage(True, PSPageMaxNumber);
	      }
	    } else {
	      GotoPage(True, (int) NumArg);
	    }
	    break;
	  
	  case XK_i:
	    NoImage = !NoImage;
	    ScrollPrevious = True;
	    RestartContext();
	    break;
	  
	  case XK_r:
	    RestoreContext(e.xkey.state);
	    break;
	  
	  case XK_equal:
	    if (!UnstructuredMode || StreamMode) {
	      WatchProgress = !WatchProgress;
	      ScrollPrevious = False;
	      RestartContext();
	    } else {
	      Beep(1);
	    }
	    break;
	  
	  case XK_e:
#ifdef DPS
	    EngineNumber = (EngineNumber + 1) %
	                      (sizeof(Engines) / sizeof(*Engines));
	    ScrollPrevious = False;
	    RestartContext();
#else
	    Beep(1);
#endif
	    break;
	  
	  case XK_R:
	    if (NoArgument) {
	      Beep(1);
	    } else {
	      SaveContext();
	      if (NumNegative) {
		PSAngle = Normalize(NumArg);
	      } else {
		PSAngle = Normalize(-NumArg);
	      }
	      ClearArgument();
	      ZoomWindow(FullPage);
	      RestartContext();
	    }
	    break;
	  
	  case XK_Select:
	  case XK_s:
	    if (e.xkey.state & ControlMask) {
	      SavePixmap(NULL);
	    } else if (NumArg == 0.0) {
	      PSMode = FixedScale;
	      SetWindowName(NULL);
	    } else if (NumArg > 0.01) {
	      SaveContext();
	      PSScale = NumArg;
	      PSMode = FixedScale;
	      ClearArgument();
	      RestartContext();
	    } else {
	      ClearArgument();
	      Beep(1);
	    }
	    break;
	  
	  case XK_f:
	    SaveContext();
	    PSMode = FullPage;
	    if (FixedGeometry || ChildMode || HScroll || VScroll) {
	      RestartContext();
	    } else {
	      SetWindowName(NULL);
	    }
	    break;
	  
	  case XK_w:
	    SaveContext();
	    PSMode = FullWidth;
	    if (FixedGeometry || ChildMode || HScroll) {
	      RestartContext();
	    } else {
	      SetWindowName(NULL);
	    }
	    break;
	  
	  case XK_h:
	    SaveContext();
	    PSMode = FullHeight;
	    if (FixedGeometry || ChildMode || VScroll) {
	      RestartContext();
	    } else {
	      SetWindowName(NULL);
	    }
	    break;
	  
	  case XK_F:
	    SaveContext();
	    ZoomWindow(FullPage);
	    RestartContext();
	    break;
	  
	  case XK_W:
	    SaveContext();
	    ZoomWindow(FullWidth);
	    RestartContext();
	    break;
	  
	  case XK_H:
	    SaveContext();
	    ZoomWindow(FullHeight);
	    RestartContext();
	    break;
	  
	  case XK_S:
	    if (StreamMode) {
	      SaveStream(False);
	    } else {
	      SaveFile();
	    }
	    break;
	  
	  case XK_Left:
	    Scroll(-1.0, 0.0, True, True);
	    break;
	  
	  case XK_Right:
	    Scroll(1.0, 0.0, True, True);
	    break;
	  
	  case XK_Up:
	    Scroll(0.0, -1.0, True, True);
	    break;
	  
	  case XK_space:
	  case XK_Return:
	  case XK_KP_Enter:
	  case XK_Down:
	    Scroll(0.0, 1.0, True, True);
	    break;
	  
	  case XK_Control_L:
	  case XK_Control_R:
	  case XK_Shift_L:
	  case XK_Shift_R:
	  case XK_Multi_key:
	  case XK_Meta_L:
	  case XK_Meta_R:
	  case XK_Alt_L:
	  case XK_Alt_R:
	  case XK_Super_L:
	  case XK_Super_R:
	  case XK_Hyper_L:
	  case XK_Hyper_R:
	  case XK_Shift_Lock:
	  case XK_Caps_Lock:
	    break;
	  
	  default:
	    Beep(1);
	    break;
	  }
	}
      }
    default:
      break;
    }
  }
}



