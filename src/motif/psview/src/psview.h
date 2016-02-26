/*
 * ANSI C
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

/*
 * Sockets
 */

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>

/*
 * X11
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

/*
 * Macros
 */

#define null '\0'
#define streq(a, b) (strcmp(a, b) == 0)
#define Maximum(a, b) ((a) > (b) ? a : b)
#define Minimum(a, b) ((a) < (b) ? a : b)

/*
 * Engine
 */

typedef struct {
  char *Name;
  void (*StartContext) (Display* dpy,
			Window pswindow,
			Pixmap pixmap,
			double xscale,
			double yscale,
			XRectangle* pixmap_rect,
			XRectangle* bbox_rect,
			long white_pixel,
			long black_pixel);
  void (*DestroyContext) (void);
  void (*SendData) (Bool send_data);
  void (*FlushContext) (void);
  Bool (*Dispatch) (XEvent *event);
  void (*Synchronize) (void);
  void (*ReadProc) (void);
  void (*WriteProc) (void);
  void (*Select) (int *read_fd, int *write_fd);
  void (*SendBuffer) (char *data, int length);
  void (*UnfreezeContext) (void);
  Bool (*Alive) (void);
} Engine;

 /*
  * Global symbols & functions
  */

extern Engine DPS_Engine, GS_Engine;
extern void
  Error (Bool cleanup, Bool usage, char *format,...),
  Exit (int err_code),
  Free(void *ptr),
  *Malloc(unsigned int length),
  TextProc (char *text, int length),
  SynchronizeProc (void),
  SendData (void), 
  SetEnv (char* var, char* val),
  DoneProc (void);
