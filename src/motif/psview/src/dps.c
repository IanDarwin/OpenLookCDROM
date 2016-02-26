#include "psview.h"
  
/*
 * Exported engine
 */

static void DPS_StartContext (Display*, Window, Pixmap, double, double,
			      XRectangle*, XRectangle*, long, long);
static void DPS_DestroyContext (void);
static void DPS_SendData (Bool);
static void DPS_FlushContext (void);
static Bool DPS_Dispatch (XEvent*);
static void DPS_Synchronize(void);
static void DPS_ReadProc (void);
static void DPS_WriteProc (void);
static void DPS_Select (int*, int*);
static void DPS_SendBuffer (char*, int);
static void DPS_UnfreezeContext (void);
static Bool DPS_Alive (void);

Engine DPS_Engine = {
  "dps",
  &DPS_StartContext,
  &DPS_DestroyContext,
  &DPS_SendData,
  &DPS_FlushContext,
  &DPS_Dispatch,
  &DPS_Synchronize,
  &DPS_ReadProc,
  &DPS_WriteProc,
  &DPS_Select,
  &DPS_SendBuffer,
  &DPS_UnfreezeContext,
  &DPS_Alive
};

/*
 * Implementation
 */

#ifdef DPS

#include <DPS/dpsXclient.h>
#include <DPS/dpsfriends.h>
#include <DPS/dpsops.h>

#define MAX_CODES 1000

/*
 * Local objects
 */

static DPSContext context = NULL;
static GC psgc = (GC) 0;
static Display *dpy;
static Bool send_data = False;
static int codes[MAX_CODES];
static int numCodes = 0;

static Bool
  DPS_Alive (void);

static void
  DPS_ErrorProc (DPSContext context, DPSErrorCode errorCode,
		 long unsigned int arg1, long unsigned int arg2),
  DPS_StatusProc (DPSContext context, int code),
  DPS_TextProc (DPSContext context, char *text, int length);

/*
 * Implementation
 */

static void
  DPS_StartContext (Display *_dpy, Window pswindow, Pixmap pixmap,
		    double XScale, double YScale, XRectangle *PixmapRect,
		    XRectangle *BBoxRect, long white, long black)
{
  XGCValues gcvalues;

  dpy = _dpy;
  
  gcvalues.arc_mode = ArcChord;
  gcvalues.line_width = 0;
  gcvalues.background = white;
  gcvalues.foreground = black;

  DPS_DestroyContext();

  psgc = XCreateGC(dpy, pswindow,
		   GCLineWidth | GCBackground | GCForeground, &gcvalues);
  
  XSetClipRectangles(dpy, psgc, 0, 0, PixmapRect, 1, Unsorted);
  
  context =
    XDPSCreateSimpleContext(dpy,
			    (pixmap == None) ? pswindow : pixmap,
			    psgc,
			    -BBoxRect->x,
			    BBoxRect->y + BBoxRect->height,
			    DPS_TextProc, DPS_ErrorProc, NULL);

  if (context == NULL) {
    Error(True, False, "This server does not support Display PostScript");
  } else {
    XDPSSetStatusMask(context,
		      PSFROZENMASK | PSNEEDSINPUTMASK | PSZOMBIEMASK,
		      0, 0);
    XDPSRegisterStatusProc(context, DPS_StatusProc);
#ifdef PASS_THROUGH
    XDPSSetEventDelivery(dpy, dps_event_pass_through);
#endif
  }
}

static void DPS_DestroyContext(void)
{
  if (context != NULL) {
    DPSDestroySpace(DPSSpaceFromContext(context));
  }

  if (psgc != (GC) 0) {
    XFreeGC(dpy, psgc);
  }

  context = NULL;
  psgc = (GC) 0;
}

static void DPS_UnfreezeContext(void)
{
  if (DPS_Alive()) {
    XDPSUnfreezeContext(context);
  }
}

static void DPS_FlushContext(void)
{
  if (DPS_Alive()) {
    DPSFlushContext(context);
  }
}

static void DPS_SendBuffer(char *data, int len)
{
  if (DPS_Alive()) {
    DPSWriteData(context, data, len);
    DPSFlushContext(context);
  }
}

static void DPS_SendData(Bool _send_data)
{
  send_data = _send_data;
  if (!send_data) {
    DPS_FlushContext();
  }
}

static void DPS_Select(int *rfd, int *wfd)
{
  *rfd = *wfd = -1;
}

static Bool DPS_Alive(void)
{
  return (context != NULL);
}

static void DPS_TextProc(DPSContext context, char *text, int length)
{
  TextProc(text, length);
}

static void DPS_ErrorProc(DPSContext context,
			  DPSErrorCode errorCode,
			  long unsigned int arg1,
			  long unsigned int arg2)
{
  (void) fprintf(stderr, "*** PsView error: ");
  DPSDefaultErrorProc(context, errorCode, arg1, arg2);
  Exit(EXIT_FAILURE);
}

static void DPS_StatusProc(DPSContext context, int code)
{
#ifdef PASS_THROUGH
  switch (code) {
    
  case PSFROZEN:
    SynchronizeProc();
    break;
    
  case PSNEEDSINPUT:
    if (send_data) {
      SendData();
    }
    break;
    
  case PSZOMBIE:
    DoneProc();
    break;
  }
#else
  if (numCodes < MAX_CODES) {
    codes[numCodes++] = code;
  }
#endif
}

static Bool DPS_Dispatch (XEvent *e)
{
#ifdef PASS_THROUGH
  return XDPSDispatchEvent(e);
#else
  return False;
#endif
}

static void DPS_Synchronize (void)
{
#ifndef PASS_THROUGH
  int i;

  for (i = 0 ; i < numCodes ; i++) {
    switch (codes[i]) {
      
    case PSFROZEN:
      SynchronizeProc();
      break;
      
    case PSNEEDSINPUT:
      if (send_data) {
	SendData();
      }
      break;
      
    case PSZOMBIE:
      DoneProc();
      break;
    }
  }

  numCodes = 0;
#endif
}


#else

static void
  DPS_StartContext (Display *dpy, Window pswindow, Pixmap pixmap, 
		    double XScale, double YScale, XRectangle *PixmapRect,
		    XRectangle *BBoxRect, long white, long black)
{
  Error(True, False, "Display PostScript not supported");
}

static void DPS_DestroyContext (void) {}
static void DPS_SendData (Bool send_data) {}
static void DPS_FlushContext (void) {}
static void DPS_Select (int *rfd, int *wfd) {}
static void DPS_SendBuffer (char *data, int len) {}
static void DPS_UnfreezeContext (void) {}
static Bool DPS_Alive (void) {}
static Bool DPS_Dispatch (XEvent *e) {}

#endif

static void DPS_ReadProc(void) {}
static void DPS_WriteProc(void) {}





