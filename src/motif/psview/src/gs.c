#include "psview.h"

#ifndef GS
#define GS "gs"
#endif

/*
 * Exported engine
 */

static void GS_StartContext (Display*, Window, Pixmap, double, double,
			     XRectangle*, XRectangle*, long, long);
static void GS_DestroyContext (void);
static void GS_SendData (Bool);
static void GS_FlushContext (void);
static Bool GS_Dispatch (XEvent*);
static void GS_Synchronize (void);
static void GS_ReadProc (void);
static void GS_WriteProc (void);
static void GS_Select (int*, int*);
static void GS_SendBuffer (char*, int);
static void GS_UnfreezeContext (void);
static Bool GS_Alive (void);

Engine GS_Engine = {
  "gs",
  &GS_StartContext,
  &GS_DestroyContext,
  &GS_SendData,
  &GS_FlushContext,
  &GS_Dispatch,
  &GS_Synchronize,
  &GS_ReadProc,
  &GS_WriteProc,
  &GS_Select,
  &GS_SendBuffer,
  &GS_UnfreezeContext,
  &GS_Alive
};

/*
 * Implementation
 */

#define BUFFERLEN 2048

typedef enum {GS_Empty, GS_Reset, GS_Write, GS_Send} GS_Action;

static int from_gs[2], to_gs[2];
static int pid = -1;
static Display *dpy;
static Window pswindow = None, gswindow = None;
static Atom XA_GHOSTVIEW, XA_NEXT, XA_PAGE, XA_DONE;
static Bool send_data = False;
static char Buffer[BUFFERLEN];
static char *interpreter;
static Bool error = False;
static char dps_ops[] =
  " /clientsync {1 false .outputpage} bind def\n\
 /rectviewclip {pop pop pop pop} bind def\n";

static void
  GS_ResetContext (void),
  GS_SignalHandler (int sig);

static Bool
  GS_Buffer (GS_Action action,...);

/*
 * Implementation
 */

static void GS_ResetContext(void)
{
  /*
   * We close the pipes just to make sure that there is no data left
   */
  
  pid = -1;
  gswindow = None;
  
  close(from_gs[0]);
  close(from_gs[1]);
  close(to_gs[0]);
  close(to_gs[1]);
}

static void GS_SignalHandler(int sig)
{
  int _pid;
  int status;

  /*
   * Reset the context when ghostscript really dies. Note that
   * we'll get a "Done" message when "ghostscript" dies after an error
   */
  
  if (sig == SIGCHLD && (_pid = waitpid(-1, &status, WNOHANG)) != 0 &&
      pid != -1 && pid == _pid && WIFEXITED(status)) {
    GS_ResetContext();
  }
  
}

static void
  GS_StartContext (Display *_dpy, Window _pswindow, Pixmap pixmap,
		   double XScale, double YScale, XRectangle *PixmapRect,
		   XRectangle *BBoxRect, long white, long black)
{
  static Bool first = True;
  char *argv[6], *gs;
  int in, out, status;
  Bool ok;
  
  /*
   * We keep a local copy of pswindow to check that synchronisation events
   * sent by ghostscript are valid
   */
  
  dpy = _dpy;
  pswindow = _pswindow;
  
  if (first) {
    first = False;
    
    XA_GHOSTVIEW = XInternAtom(dpy, "GHOSTVIEW", False);
    XA_DONE = XInternAtom(dpy, "DONE", False);
    XA_PAGE = XInternAtom(dpy, "PAGE", False);
    XA_NEXT = XInternAtom(dpy, "NEXT", False);
    
    if ((gs = XGetDefault(dpy, "PsView", "gs")) != NULL) {
      interpreter = gs;
    } else {
      interpreter = GS;
    }
	
    (void) signal(SIGCHLD, GS_SignalHandler);
  } else {
    GS_DestroyContext();
  }
  
  if (pipe(from_gs) != 0 ||
      pipe(to_gs) != 0 ||
      (status = fcntl(to_gs[1], F_GETFL, 0)) == -1 ||
      fcntl(to_gs[1], F_SETFL, status | O_NONBLOCK) == -1 ||
      (status = fcntl(from_gs[0], F_GETFL, 0)) == -1 ||
      fcntl(from_gs[0], F_SETFL, status | O_NONBLOCK) == -1) {
    Error(True, False, "Cannot create pipe");
  }
  
  pid = -1;
  in = out = -1;
  ok = True;

  argv[0] = interpreter;
  argv[1] = "-dQUIET";
  argv[2] = "-dNOPAUSE";
  argv[3] = "-dSAFER";
  argv[4] = "-";
  argv[5] = NULL;
  
  sprintf(Buffer, "0 0 %d %d %d %d %f %f 0 0 0 0 %d %d",
	  (int) floor(BBoxRect->x / XScale),
	  (int) floor(BBoxRect->y / YScale),
	  (int) ceil((BBoxRect->x + BBoxRect->width) / XScale),
	  (int) ceil((BBoxRect->y + BBoxRect->height) / YScale),
	  72.0 * XScale, 72.0 * YScale,
	  PixmapRect->width,
	  PixmapRect->height);

  XChangeProperty(dpy, pswindow, XA_GHOSTVIEW,
		  XA_STRING, 8, PropModeReplace,
		  (unsigned char*) Buffer, strlen(Buffer));

  XSync(dpy, False);

  /*
   * Windows & Pixmaps are unsigned longs
   */
  
  (void) sprintf(Buffer, "%lu %lu",
		 pswindow,
		 (pixmap == None) ? pswindow : pixmap);
  
  SetEnv("GHOSTVIEW", Buffer);

  if ((pid = vfork()) == 0) {
    
    if ((in = dup(0)) != -1 &&
	dup2(to_gs[0], 0) != -1 &&
	(out = dup(1)) != -1 &&
	dup2(from_gs[1], 1) != -1) {
      execvp(interpreter, argv);
    }
    
    /*
     * Cannot start gs. Transmit the result through "ok". Since we use
     * vfork, this will work.
     */
    
    ok = False;
    _exit(EXIT_FAILURE);
  }
  
  if (in != -1) {
    dup2(in, 0);
    close(in);
  }
  
  if (out != -1) {
    dup2(out, 1);
    close(out);
  }

  if (!ok) {
    Error(True, False, "Cannot execute \"%s\"", interpreter);
  } else if (pid == -1) {
    Error(True, False, "Cannot fork gs", interpreter);
  } else {
    (void) GS_Buffer(GS_Reset);
    GS_Buffer(GS_Write, dps_ops, strlen(dps_ops));
  }
}

static void GS_DestroyContext(void)
{
  void (*handler)(int);
  
  if (GS_Alive() && kill(pid, SIGTERM) != -1) {
    handler = signal(SIGCHLD, SIG_DFL);
    waitpid(pid, NULL, 0);
    (void) signal(SIGCHLD, handler);
    GS_ResetContext();
  }
}

static void GS_UnfreezeContext(void)
{
  XEvent event;

  if (GS_Alive()) {
    event.xclient.type = ClientMessage;
    event.xclient.display = dpy;
    event.xclient.window = gswindow;
    event.xclient.message_type = XA_NEXT;
    event.xclient.format = 32;
    
    XSendEvent(dpy, gswindow, False, 0, &event);
  }
  
}

static void GS_FlushContext(void)
{
}

/*
 * Ghostscript output buffer. Actions:
 *
 * - GS_Buffer(Empty)
 * - GS_Buffer(Reset)
 * - GS_Buffer(Write, data, len)
 */

static Bool GS_Buffer(GS_Action action,...)
{
  int i, count, len;
  va_list args;
  static char *data, *buffer = NULL;
  static int
    maxlen = PIPE_BUF, /* Queue length */
    first = 0,         /* First element in queue */
    buflen = 0;        /* Number of elements in queue */

  if (buffer == NULL) {
    buffer = (char*) Malloc(maxlen);
  }
  
  switch (action) {

  case GS_Empty:

    return (buflen == 0);

  case GS_Reset:

    first = buflen = 0;
    return;
    
  case GS_Write:
    
    /*
     * Allocate a larger queue
     */

    va_start(args, action);
    data = va_arg(args, char*);
    len = va_arg(args, int);
    va_end(args);
    
    if (buflen + len > maxlen) {
      int new_maxlen;
      char *new_nuffer;
      
      new_maxlen = len > maxlen ? maxlen + len : 2 * maxlen;
      new_nuffer = (char*) Malloc(new_maxlen);

      for (i = 0 ; i < buflen ; i++) {
	new_nuffer[i] = buffer[(first + i) % maxlen];
      }

      Free(buffer);

      first = 0;
      maxlen = new_maxlen;
      buffer = new_nuffer;
    }

    /*
     * Write to the buffer
     */
    
    for (i = 0 ; i < len ; i++) {
      buffer[(first + buflen + i) % maxlen] = data[i];
    }

    buflen += len;

    break;

  case GS_Send:

    while (buflen > 0) {
      count = write(to_gs[1],
		    buffer + first,
		    Minimum(buflen, maxlen - first));
      if (count > 0) {
	first = (first + count) % maxlen;
	buflen -= count;
      } else {
	return;
      }
    }
  }
}

static void GS_SendBuffer(char *data, int len)
{
  if (GS_Alive()) {
    (void) GS_Buffer(GS_Write, data, len);
  }
}

static void GS_SendData(Bool _send_data)
{
  send_data = _send_data;
}

static void GS_Select(int *rfd, int *wfd)
{
  *wfd = *rfd = -1;

  if (GS_Alive()) {
    *rfd = from_gs[0];
    if (GS_Buffer(GS_Empty)) {
      if (send_data) {
	SendData();
      }
    } else {
      *wfd = to_gs[1];
    }
  }
}

static void GS_ReadProc(void)
{
  int how;
  char *err;

  /*
   * Well, this is a hack to detect a ghostscript error
   */

  while ((how = read(from_gs[0], Buffer, BUFFERLEN - 1)) > 0) {
    if (error) {
      (void) fwrite(Buffer, how, 1, stderr);
    } else {
      Buffer[how] = '\0';
      err = strstr(Buffer, "Error:");

      if (err == NULL) {
	TextProc(Buffer, how);
      } else {
	error = True;
	TextProc(Buffer, err - Buffer);
	(void) fprintf(stderr, "*** PsView error: %s", err + 7);
      }
    }
  }
  
  (void) fflush(stderr);
}

static void GS_WriteProc(void)
{
  (void) GS_Buffer(GS_Send);
}

static Bool GS_Alive(void)
{
  return (pid != -1);
}

static Bool GS_Dispatch (XEvent *e)
{
  if (e->type != ClientMessage) {
    return False;
  }
  
  /*
   * The message is valid only if it is addressed to the _current_ pswindow
   */
  
  if (error && e->xclient.message_type == XA_DONE) {

    /*
     * Display any unread error message, then exit
     */
    
    GS_ReadProc();
    Exit(EXIT_FAILURE);
    
  } else if (GS_Alive() && e->xclient.window == pswindow) {

    if (e->xclient.message_type == XA_DONE) {
      
      gswindow = None;
      pid = -1;
      DoneProc();
      return True;
      
    } else if (e->xclient.message_type == XA_PAGE) {
      
      gswindow = e->xclient.data.l[0];
      SynchronizeProc();
      return True;
      
    }
  }
  
  return False;
}

static void GS_Synchronize (void)
{
}
