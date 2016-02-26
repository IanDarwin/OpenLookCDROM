 
/*  @(#)xview.c 1.9 91/09/05
 *
 *  XView dependent graphics routines used by sidtool.
 *
 *  Screen design and original implementation
 *               Copyright (C) 1981, 1982, 1983 - Brad A. Myers
 *
 *  Current implementation
 *               Copyright (C) 1991 Rich Burridge
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors on inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <memory.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <xview/canvas.h>
#include <xview/cursor.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>

#include "sidtool_ui.h"

#include "sidtool.h"
#include "extern.h"

#define  NOTIFY_SET_ITIMER_FUNC  (void) notify_set_itimer_func
#define  XV_SET                  (void) xv_set

#define  BFONT   "-b&h-lucidatypewriter-bold-r-*-*-*-130-*-*-*-*-iso8859-1"
#define  NFONT   "-b&h-lucidatypewriter-medium-r-*-*-*-130-*-*-*-*-iso8859-1"

#define  DEFFONT "fixed"

sidtool_Wmain_objects   *Sidtool_Wmain ;
sidtool_Pprops_objects  *Sidtool_Pprops ;
sidtool_Pscores_objects *Sidtool_Pscores ;

Attr_attribute INSTANCE ;

#ifdef SUN_ARROW_KEYS
static int event_is_keypad P((Event *)) ;
static KeySym keypad_keysym P((Event *)) ;
static void key_init P(()) ;
#endif /*SUN_ARROW_KEYS*/

void Bnotify P((Panel_item, Event *)) ;
void Ccanvas_repaint P((Canvas, Xv_window,
                        Display *, Window, Xv_xrectlist *)) ;
void event_proc P((Xv_window, Event *, Notify_arg, Notify_event_type)) ;
void make_x_stuff P((Frame, Canvas)) ;

Notify_value Ccanvas_event P((Xv_window, Event *,
                              Notify_arg, Notify_event_type)) ;
Notify_value main_loop P((Notify_client, int)) ;
Panel_setting Snotify P((Panel_item, Event *)) ;

Canvas canvas ;
Canvas_paint_window pw ;
Drawable xid ;
Frame frame ;
Pixmap images[MAXIMAGES] ;        /* The sidtool icon images. */
Pixmap scratch ;
XFontStruct *bfont, *nfont ;
XGCValues gc_val ;
XrmDatabase sidtool_DB ;          /* Combined resources database. */
Xv_cursor cursor, offcursor ;

Display *dpy ;
GC gc ;
GC ropgc ;                       /* Graphics context for rasterops. */
GC svgc = NULL ;
GC textgc ;                      /* Graphics context for writing all text. */

#ifdef SUN_ARROW_KEYS
/* Data for holding information about the server's keyboard mapping. */
int kcmin ;                      /* Minimum keycode. */
int kcmax ;                      /* Maximum keycode. */
int keysyms_per_key ;            /* Keysyms per keycode. */
unsigned char *kparray ;         /* Array indicating if key is on keypad. */
#endif /*SUN_ARROW_KEYS*/

static char *cmdline = NULL ;    /* Saved command line options. */

int rops[5] ;                    /* Graphics dependent rasterop codes. */
int screen ;
long backgnd, foregnd ;
unsigned int depth ;
unsigned long gc_mask ;


int
main(argc, argv)
int argc ;
char **argv ;
{ 
  progname = argv[0] ;            /* Save pointer to this programs name. */
  init_options() ;                /* Initialise default option values. */
  
  xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0) ;
  init_rops() ;

  Sidtool_Wmain   = sidtool_Wmain_objects_initialize(NULL, NULL) ;
  Sidtool_Pprops  = sidtool_Pprops_objects_initialize(NULL,
                                                      Sidtool_Wmain->Wmain) ;
  Sidtool_Pscores = sidtool_Pscores_objects_initialize(NULL,
                                                      Sidtool_Wmain->Wmain) ;
  make_x_stuff(Sidtool_Wmain->Wmain, Sidtool_Wmain->Ccanvas) ;
  load_resources() ;              /* Get resources from various places. */
  read_resources() ;              /* Read resources from merged database. */
  get_options(argc, argv) ;       /* Get command line options. */

  XV_SET(canvas, CANVAS_RETAINED, retained, 0) ;

#ifdef SUN_ARROW_KEYS
  key_init() ;                    /* Determine function pad keys. */
#endif /*SUN_ARROW_KEYS*/

  do_startup() ;
  set_timer(TRUE) ;
  write_cmdline() ;               /* Save sidtool command line. */
  canpaint = TRUE ;
  xv_main_loop(Sidtool_Wmain->Wmain) ;
  exit(0) ;
/*NOTREACHED*/
}


void
activate_button(btype, state)
enum but_type btype ;
int state ;
{
  switch (btype)
    {
      case BUT_HELP   : XV_SET(Sidtool_Wmain->Bhelp,
                               PANEL_INACTIVE, !state,
                               0) ;
                        break ;
      case BUT_PROPS  : XV_SET(Sidtool_Wmain->Bprops,
                               PANEL_INACTIVE, !state,
                               0) ;
                        break ;
      case BUT_SCORES : XV_SET(Sidtool_Wmain->Bscores,
                               PANEL_INACTIVE, !state,
                               0) ;
                        break ;
      case BUT_NEW    : XV_SET(Sidtool_Wmain->Bnewgame,
                               PANEL_INACTIVE, !state,
                               0) ;
                        break ;
      case BUT_CONT   :
      case BUT_STOP   : XV_SET(Sidtool_Wmain->Bstop,
                               PANEL_INACTIVE, !state,
                               0) ;
    }
}


void
activate_choice(ctype, state)
enum choice_type ctype ;
int state ;
{
  switch ((int) ctype)
    {
      case C_PLAY : XV_SET(Sidtool_Wmain->SETgame,
                           PANEL_INACTIVE, !state,
                           0) ;
    }
}


void
activate_message(mtype, state)
enum mes_type mtype ;
int state ;
{
       if (mtype == M_PLAYER1)
    { 
      XV_SET(Sidtool_Wmain->Mplayer1, PANEL_INACTIVE, !state, 0) ;
      XV_SET(Sidtool_Wmain->Mscore1,  PANEL_INACTIVE, !state, 0) ;
    }
  else if (mtype == M_PLAYER2)
    { 
      XV_SET(Sidtool_Wmain->Mplayer2, PANEL_INACTIVE, !state, 0) ;
      XV_SET(Sidtool_Wmain->Mscore2,  PANEL_INACTIVE, !state, 0) ;                  }
  else if (mtype == M_PLAYER3) 
    {  
      XV_SET(Sidtool_Wmain->Mplayer3, PANEL_INACTIVE, !state, 0) ; 
      XV_SET(Sidtool_Wmain->Mscore3,  PANEL_INACTIVE, !state, 0) ;                  }
  else if (mtype == M_PLAYER4) 
    {  
      XV_SET(Sidtool_Wmain->Mplayer4, PANEL_INACTIVE, !state, 0) ; 
      XV_SET(Sidtool_Wmain->Mscore4,  PANEL_INACTIVE, !state, 0) ;                  }
}


/* Move an offscreen raster area to another offscreen raster area. */

void
blt_mem(dst, dx, dy, width, height, rop, src, sx, sy)
int dx, dy, width, height, sx, sy ;
enum rop_type rop ;
enum icon_type dst, src ;
{
  XSetFunction(dpy, svgc, rops[(int) rop]) ;
  XCopyArea(dpy, images[(int) src], images[(int) dst], svgc, sx, sy,
            (unsigned int) width, (unsigned int) height, dx, dy) ;
}


/* Move an offscreen raster area to the screen. */

void
blt_mem_to_scrn(srcx, srcy, width, height, rop, mem, memx, memy)
int srcx, srcy, width, height, memx, memy ;
enum rop_type rop ;
enum icon_type mem ;
{
  if (memx || memy)
    {
      XCopyArea(dpy, images[(int) mem], scratch, svgc,
                memx, memy, width, height, 0, 0) ;
      gc_val.stipple = scratch ;
    }
  else gc_val.stipple = images[(int) mem] ;
  gc_val.ts_x_origin  = srcx ;
  gc_val.ts_y_origin  = srcy ;
  gc_val.function     = rops[(int) rop] ;
  gc_mask = GCFunction | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin ;
  XChangeGC(dpy, ropgc, gc_mask, &gc_val) ;
  XFillRectangle(dpy, xid, ropgc, srcx, srcy, width, height) ;
}


/* Manipulate a portion of the screen with itself. */

void
blt_scrn(srcx, srcy, width, height, rop)
int srcx, srcy, width, height ;
enum rop_type rop ;
{
  XSetFunction(dpy, ropgc, rops[(int) rop]) ;
  XCopyArea(dpy, xid, xid, ropgc, srcx, srcy,
            (unsigned int) width, (unsigned int) height, srcx, srcy) ;
}


/*ARGSUSED*/
void
Bnotify(item, event)
Panel_item item ;
Event *event ;
{
  char *str ;

  str = (char *) xv_get(item, PANEL_LABEL_STRING) ;
  process_button(str) ;
}


Notify_value
Ccanvas_event(win, event, arg, type)
Xv_window win ;
Event *event ;
Notify_arg arg ;
Notify_event_type type ;
{
  event_proc(win, event, arg, type) ;
  return(notify_next_event_func(win, (Notify_event) event, arg, type)) ;
}


/*ARGSUSED*/
void
Ccanvas_repaint(canvas, paint_window, display, xid, rects)
Canvas canvas ;
Xv_window paint_window ;
Display *display ;
Window xid ;
Xv_xrectlist *rects ;
{
  static int ignore_calls = 0 ;
  XEvent ev ;

/* Get the remaining expose events. */

  while (XPending(dpy) && (XPeekEvent(dpy, &ev),
         (ev.type == Expose && ev.xany.window == xid)))
    XNextEvent(dpy, &ev) ;

  if (ignore_calls < 1)
    {
      ++ignore_calls ;
      return ;
    }

  restore_screen() ;
}


void
do_flush()
{
  XSync(dpy, 0) ;
}


void
draw_text(x, y, ftype, text)    /* Write text in font at x,y. */
int x, y ;
enum font_type ftype ;
char *text ;
{
  XSetFunction(dpy, textgc, rops[(int) sfunc]) ;
  if (ftype == BOLDFONT) XSetFont(dpy, textgc, bfont->fid) ;
  else                   XSetFont(dpy, textgc, nfont->fid) ;
  XDrawString(dpy, xid, textgc, x, y, text, strlen(text)) ;
}


#ifdef SUN_ARROW_KEYS
/*  Tell whether an event is a keyboard event from a keypad key. This function
 *  looks at the raw X event and uses information from Xlib to make this
 *  determination. This function is sometimes necessary because there are
 *  often several keysyms on a keypad key, and XView doesn't always give the
 *  "right" one.
 */

static int
event_is_keypad(event)
Event *event ;
{
  XEvent *xk = event_xevent(event) ;

  if (xk->type != KeyPress && xk->type != KeyRelease) return(0) ;
  return(kparray[xk->xkey.keycode - kcmin] > 0) ;
}
#endif /*SUN_ARROW_KEYS*/


/*ARGSUSED*/
void
event_proc(window, event, arg, type)
Xv_window window ;
Event *event ;
Notify_arg arg ;
Notify_event_type type ;
{
  static int oldx = 0 ;
  static int oldy = 0 ;
  int curx, cury ;
  int dx, dy, id, is_ascii, is_down ;

  id      = event_id(event) ;
  is_ascii = event_is_ascii(event) ;
  is_down  = event_is_down(event) ;
  curx    = event_x(event) ;
  cury    = event_y(event) ;

#ifdef SUN_ARROW_KEYS
/*  Determine if one of the arrow keys on the right function pad have been
 *  pressed, and map to the appropriate direction. Saves us having to mess
 *  around with remapping the keyboard.
 */

  if (event_is_keypad(event))
    {
      switch (keypad_keysym(event))
        {
          case XK_KP_2 : sc = 'd' ;
                         break ;
          case XK_KP_4 : sc = 'l' ;
                         break ;
          case XK_KP_6 : sc = 'r' ;
                         break ;
          case XK_KP_8 : sc = 'u' ;
        }
      return ;
    }
#endif /*SUN_ARROW_KEYS*/

/*  If we get a KBD_DONE event and we are playing a normal game, then return
 *  the mouse to the middle of the window.
 */

  if (id == KBD_DONE && !stopped && progstate == MAKEPLAY && !autoplay)
    warp_mouse() ;

  if (is_ascii || !autoplay && (id == MS_RIGHT || id == LOC_MOVE))
    {
      c = id ;
      if (!autoplay)
        {
          if (is_ascii)
            {
              if (is_down)
                {
                       if (c == 'h') sc = 'l' ;
                  else if (c == 'j') sc = 'd' ;
                  else if (c == 'k') sc = 'u' ;
                  else if (c == 'l') sc = 'r' ;
                }
              return ;
            }
          if (c == LOC_MOVE)
            {
              dx = abs(curx - oldx) ;
              dy = abs(cury - oldy) ;
              if (dx <= 4 && dy <= 4)        /* Move at least 4 pixels */
                {
                  oldx = curx ;
                  oldy = cury ;
                  return ;
                }
              if (dx > dy)
                if ((curx - oldx) > 0) c = 'r' ;
                else                   c = 'l' ;
              else
                if ((cury - oldy) > 0) c = 'd' ;
                else                   c = 'u' ;
              oldx = curx ;
              oldy = cury ;
            }
          sc = c ;
        }
    }
}


int
get_choice(ctype)
enum choice_type ctype ;
{
  switch ((int) ctype)
    {
      case C_PLAY : return((int) xv_get(Sidtool_Wmain->SETgame, PANEL_VALUE)) ;
    }
/*NOTREACHED*/
}


char *
get_resource(rtype)      /* Get sidtool resource from merged databases. */
enum res_type rtype ;
{
  char cstr[MAXLINE], nstr[MAXLINE], str[MAXLINE] ;
  char *str_type[20] ;
  XrmValue value ;

  STRCPY(str, resources[(int) rtype]) ;
  SPRINTF(nstr,  "sidtool.%s", str) ;
  if (islower(str[0])) str[0] = toupper(str[0]) ;
  SPRINTF(cstr, "Sidtool.%s", str) ;
  if (XrmGetResource(sidtool_DB, nstr, cstr, str_type, &value) == NULL)
    return((char *) NULL) ;
  else return(value.addr) ;
}


int
get_value(ptype)
enum prop_type ptype ;
{
       if (ptype == P_PLAYERS)
    return((int) xv_get(Sidtool_Pprops->SETplayer, PANEL_VALUE)) ;
  else if (ptype == P_SKILL)
    return((int) xv_get(Sidtool_Pprops->SLlevel,   PANEL_VALUE)) ;
/*NOTREACHED*/
}


void
init_rops()         /* Load the array of rasterop values. */
{
  rops[(int) RRPL] = GXcopy ;
  rops[(int) RXOR] = GXxor ;
  rops[(int) RCLR] = GXclear ;
  rops[(int) RSET] = GXset ;
  rops[(int) RINV] = GXinvert ;
}


#ifdef SUN_ARROW_KEYS
/*  Get information about the keyboard mappings. Determine which keys are
 *  keypad.
 */

static void
key_init()
{
  int i, j ;
  KeySym *tmp ;
  KeySym ks ;

  XDisplayKeycodes(dpy, &kcmin, &kcmax) ;
  tmp = XGetKeyboardMapping(dpy, kcmin, 1, &keysyms_per_key) ;
  XFree((char *) tmp) ;

  kparray = (unsigned char *) malloc((unsigned) (kcmax - kcmin + 1)) ;

/*  For each key, run through its list of keysyms.  If this keysym is a
 *  keypad keysym, we know this key is on the keypad.  Mark it as such in
 *  kparray[].
 */

  for (i = kcmin; i <= kcmax; ++i)
    {
      kparray[i - kcmin] = 0 ;
      for (j = 0; j < keysyms_per_key; ++j)
        {
          ks = XKeycodeToKeysym(dpy, i, j) ;
          if (IsKeypadKey(ks))
            {
              kparray[i - kcmin] = 1 ;
              break ;
            }
        }
    }
}


/* Given a keyboard event from the keypad, return the KP_whatever keysym
 * corresponding to the key in the event.  If no keypad keysym can be found,
 * returns NoSymbol.
 */

static KeySym
keypad_keysym(event)
Event *event ;
{
  int i ;
  int keycode = event_xevent(event)->xkey.keycode ;
  KeySym ks ;

  for (i = 0; i < keysyms_per_key; ++i)
    {
      ks = XKeycodeToKeysym(dpy, keycode, i) ;
      if (IsKeypadKey(ks)) return(ks) ;
    }
  return(NoSymbol) ;
}
#endif /*SUN_ARROW_KEYS*/


void
load_image(itype, cbuf)
enum icon_type itype ;
unsigned char *cbuf ;
{
  images[(int) itype] = XCreatePixmapFromBitmapData(dpy, xid, (char *) cbuf,
                                                    64, 64, 1, 0, 1) ;
}


/*  Get the resource databases. These are looked for in the following ways:
 *
 *  Classname file in the app-defaults directory. In this case, Classname
 *  is Reve.
 *
 *  Classname file in the directory specified by the XUSERFILESEARCHPATH
 *  or XAPPLRESDIR environment variable.
 *
 *  Property set using xrdb, accessible through the XResourceManagerString
 *  macro or, if that is empty, the ~/.Xdefaults file.
 *
 *  XENVIRONMENT environment variable or, if not set, .Xdefaults-hostname
 *  file.
 */

void
load_resources()
{
  XrmDatabase db ;
  char *home, name[MAXPATHLEN], *ptr ;
  int len ;

  home = getenv("HOME") ;
  XrmInitialize() ;
  STRCPY(name, "/usr/lib/X11/app-defaults/Sidtool") ;

/* Get applications defaults file, if any. */

  db = XrmGetFileDatabase(name) ;
  XrmMergeDatabases(db, &sidtool_DB) ;

/* Merge server defaults, created by xrdb. If nor defined, use ~/.Xdefaults. */

  if (XResourceManagerString(dpy) != NULL)
    db = XrmGetStringDatabase(XResourceManagerString(dpy)) ;
  else
    { 
      SPRINTF(name, "%s/.Xdefaults", home) ;
      db = XrmGetFileDatabase(name) ;
    }
  XrmMergeDatabases(db, &sidtool_DB) ;

/*  Open XENVIRONMENT file or, if not defined, the .Xdefaults, and merge
 *  into existing database.
 */

  if ((ptr = getenv("XENVIRONMENT")) == NULL)
    {
      SPRINTF(name, "%s/.Xdefaults-", home) ;
      len = strlen(name) ;
      GETHOSTNAME(name+len, MAXPATHLEN-len) ;
      db = XrmGetFileDatabase(name) ;
    }
  else db = XrmGetFileDatabase(ptr) ;
  XrmMergeDatabases(db, &sidtool_DB) ;
}


/*ARGSUSED*/
Notify_value
main_loop(client, itimer_type)
Notify_client client ;
int itimer_type ;
{
  next_state() ;               /* Next step through the automation. */
  return(NOTIFY_DONE) ;
}


void
make_x_stuff(f, c)
Frame f ;
Canvas c ;
{
  char nullcur_data[32] ;
  Drawable svxid ;
  Server_image nullsv ;

  canvas  = c ;
  frame   = f ;
  dpy     = (Display *) xv_get(f, XV_DISPLAY) ;
  pw      = canvas_paint_window(c) ;
  xid     = (Window) xv_get(pw, XV_XID) ;
  screen  = DefaultScreen(dpy) ;
  foregnd = BlackPixel(dpy, screen) ;
  backgnd = WhitePixel(dpy, screen) ;
  depth   = DefaultDepth(dpy, screen) ;

  gc_mask           = GCForeground | GCBackground | GCGraphicsExposures ;
  gc_val.foreground = foregnd ;
  gc_val.background = backgnd ;
  gc_val.function   = GXcopy ;
  gc_val.graphics_exposures = False ;
  gc = XCreateGC(dpy, xid, gc_mask, &gc_val) ;

  textgc = XCreateGC(dpy, xid, gc_mask, &gc_val) ;
  if (depth > 1) XSetForeground(dpy, textgc, foregnd ^ backgnd) ;

  ropgc = XCreateGC(dpy, xid, gc_mask, &gc_val) ;
  XSetFillStyle(dpy, ropgc, FillStippled) ;

  scratch = XCreatePixmap(dpy, xid, 64, 64, 1) ;

  if ((nfont = XLoadQueryFont(dpy, NFONT)) == NULL)
    if ((nfont = XLoadQueryFont(dpy, DEFFONT)) == NULL)
      {
        FPRINTF(stderr, "%s: couldn't get the default font.\n", progname) ;
        exit(1) ;
      }

  if ((bfont = XLoadQueryFont(dpy, BFONT)) == NULL)
    if ((bfont = XLoadQueryFont(dpy, DEFFONT)) == NULL)
      {
        FPRINTF(stderr, "%s: couldn't get the default font.\n", progname) ;
        exit(1) ;
      }
  XSetFont(dpy, gc, nfont->fid) ;

  MEMSET(nullcur_data, 0, 32) ;
  nullsv = (Server_image) xv_create(XV_NULL,           SERVER_IMAGE,
                                    XV_HEIGHT,         16,
                                    XV_WIDTH,          16, 
                                    SERVER_IMAGE_BITS, nullcur_data,
                                    0) ;
  offcursor = xv_create(XV_NULL,      CURSOR,
                        CURSOR_IMAGE, nullsv,
                        0) ;

  svxid             = (Drawable) xv_get(nullsv, XV_XID) ;
  gc_mask           = GCForeground | GCBackground ;
  gc_val.foreground = 1 ;
  gc_val.background = 0 ;
  svgc = XCreateGC(dpy, svxid, gc_mask, &gc_val) ;

  cursor = (Xv_cursor) xv_get(pw, WIN_CURSOR) ;
  XV_SET(pw, WIN_CONSUME_EVENTS, LOC_MOVE, 0, 0) ;
}


void
position_popup(wtype)
enum win_type wtype ;
{
  Frame child ;
  Rect crect, prect ;
  int width ;

       if (wtype == W_PROPS)  child = Sidtool_Pprops->Pprops ;
  else if (wtype == W_SCORES) child = Sidtool_Pscores->Pscores ;
  if ((int) xv_get(child, XV_SHOW)) return ;
  frame_get_rect(Sidtool_Wmain->Wmain, &prect) ;
  frame_get_rect(child,  &crect) ;
  width  = (int) xv_get(Sidtool_Wmain->Wmain, XV_WIDTH) ;

  switch ((int) wtype)
    {
      case W_PROPS  : crect.r_left = prect.r_left + width + BORDER_SIZE ;
                      crect.r_top  = prect.r_top ;
                      break ;
      case W_SCORES : crect.r_left = prect.r_left + width + BORDER_SIZE ;
                      crect.r_top  = prect.r_top + 150 ;
    }
  frame_set_rect(child, &crect) ;
}


void
save_cmdline(line)       /* Save user supplied command line options. */
char *line ;
{
  if (cmdline != NULL) FREE(cmdline) ;
  cmdline = (char *) malloc((unsigned int) strlen(line) + 1) ;
  STRCPY(cmdline, line) ;
  XV_SET(frame, WIN_CMD_LINE, cmdline, 0) ;
}


Panel_setting
Snotify(item, event)
Panel_item item ;
Event *event ;
{
  STRCPY(allhighscores[skilllevel].who, (char *) xv_get(item, PANEL_VALUE)) ;
  savescorefile() ;
  set_list(skilllevel) ;
  set_timer(TRUE) ;

  XV_SET(Sidtool_Pscores->MShighscore, XV_SHOW, TRUE, 0) ;
  XV_SET(Sidtool_Pscores->MSoldscore,  XV_SHOW, TRUE, 0) ;
  XV_SET(Sidtool_Pscores->MSplayer,    XV_SHOW, TRUE, 0) ;
  XV_SET(Sidtool_Pscores->MSname,      XV_SHOW, TRUE, 0) ;

  progstate = DOCREDIT ;
  return panel_text_notify(item, event) ;
}


void
set_choice(ctype, val)
enum choice_type ctype ;
int val ;
{
  switch ((int) ctype)
    {
      case C_PLAY : XV_SET(Sidtool_Wmain->SETgame, PANEL_VALUE, val, 0) ;
    }
}


void
set_cursor(state)
int state ;
{
  if (state) XV_SET(pw, WIN_CURSOR, cursor, 0) ;
  else       XV_SET(pw, WIN_CURSOR, offcursor, 0) ;
}


void
set_highscore(level)
int level ;
{
  char buffer[MAXLINE] ;

  SPRINTF(buffer,
          "Player %1d has beaten the high score for skill level %1d.",
          highplayer, level) ;
  XV_SET(Sidtool_Pscores->MShighscore, PANEL_LABEL_STRING, buffer, 0) ;
  XV_SET(Sidtool_Pscores->MShighscore, XV_SHOW, TRUE, 0) ;

  if (allhighscores[skilllevel].score)
    {
      SPRINTF(buffer, "The old record was %1d0 held by: %s.",
                          allhighscores[skilllevel].score,
                          allhighscores[skilllevel].who) ;
      XV_SET(Sidtool_Pscores->MSoldscore, PANEL_LABEL_STRING, buffer, 0) ;
      XV_SET(Sidtool_Pscores->MSoldscore, XV_SHOW, TRUE, 0) ;
    }

  SPRINTF(buffer, "Type player %1d's name or initials: ", highplayer) ;
  XV_SET(Sidtool_Pscores->MSplayer, PANEL_LABEL_STRING, buffer, 0) ; 
  XV_SET(Sidtool_Pscores->MSplayer, XV_SHOW, TRUE, 0) ;

  XV_SET(Sidtool_Pscores->MSname, XV_SHOW, TRUE, 0) ;
}


void
set_image(image, icon, rop)
enum image_type image ;
enum icon_type icon ;
enum rop_type rop ;
{
  Drawable ixid ;
  Panel_item p ;
  Server_image sv ;

  switch ((int) image)
    {
      case CNTR1    : p = Sidtool_Wmain->Mcounter1 ;
                      break ;
      case CNTR2    : p = Sidtool_Wmain->Mcounter2 ;
                      break ;
      case CNTR3    : p = Sidtool_Wmain->Mcounter3 ;
                      break ;
      case CNTR4    : p = Sidtool_Wmain->Mcounter4 ;
                      break ;
      case CURFRUIT : p = Sidtool_Wmain->Mfruitpic ;
    }
  sv   = (Server_image) xv_get(p,  PANEL_LABEL_IMAGE) ;
  ixid = (Drawable)     xv_get(sv, XV_XID) ;

  XSetFunction(dpy, svgc, rops[(int) rop]) ;
  XCopyArea(dpy, images[(int) icon], ixid, svgc, 0, 0, 50, 50, 0, 0) ;
  XV_SET(p, XV_SHOW, TRUE, 0) ;
}


void
set_label(btype)
enum but_type btype ;
{
  XV_SET(Sidtool_Wmain->Bstop, PANEL_LABEL_STRING, but_names[(int) btype], 0) ;
}


void
set_list(level)
int level ;
{
  char buffer[MAXLINE] ;

  SPRINTF(buffer, "%1d: %s    -    %1d0",
          level, allhighscores[level].who, allhighscores[level].score) ;
  XV_SET(Sidtool_Pscores->Lscores, PANEL_LIST_STRING, level-1, buffer, 0) ;
}


void
set_score(stype, score)    /* Set auto, player or high score in panel. */
int stype, score ;
{
  Panel_item p ;
  char s[MAXLINE] ;
  int show  = TRUE ;

  if (stype >= 1 && stype <= 4) show = on ;
  score *= 10 ;
       if (stype == AUTO_SCORE) SPRINTF(s, "%1d  (AUTO)", score) ;
  else if (stype == HIGH_SCORE) SPRINTF(s, "(%1d)  %1d", skilllevel, score) ;
  else                          SPRINTF(s, "%1d", score) ;

  switch (stype)
    {
      case AUTO_SCORE  :
      case 1           : p = Sidtool_Wmain->Mscore1 ;
                         break ;
      case 2           : p = Sidtool_Wmain->Mscore2 ;
                         break ;
      case 3           : p = Sidtool_Wmain->Mscore3 ;
                         break ;
      case 4           : p = Sidtool_Wmain->Mscore4 ;
                         break ;
      case FRUIT_SCORE : p = Sidtool_Wmain->Mfruitval ;
                         break ;
      case HIGH_SCORE  : p = Sidtool_Wmain->Mhscore ;
    }
  XV_SET(p,
         PANEL_LABEL_STRING, s,
         XV_SHOW,            show,
         0) ;
}


void
set_timer(state)
int state ;
{
  NOTIFY_SET_ITIMER_FUNC(Sidtool_Wmain->Wmain,
                         state ? (Notify_func) main_loop :
                                 NOTIFY_FUNC_NULL,
                         ITIMER_REAL, &NOTIFY_POLLING_ITIMER,
                         ((struct itimerval *) 0)) ;
}


void
set_value(ptype, value)
enum prop_type ptype ;
int value ;
{
       if (ptype == P_PLAYERS)
    XV_SET(Sidtool_Pprops->SETplayer, PANEL_VALUE, value-1, 0) ;
  else if (ptype == P_SKILL)
    XV_SET(Sidtool_Pprops->SLlevel,   PANEL_VALUE, value, 0) ;
}


void
show_window(wtype, state)      /* [Un]show property or score popup. */
enum win_type wtype ;
int state ;
{
  Frame f ;

  switch ((int) wtype)
    {
      case W_PROPS  : f = Sidtool_Pprops->Pprops ;
                      break ;
      case W_SCORES : f = Sidtool_Pscores->Pscores ;
    }
  if (!state && (int) xv_get(f, FRAME_PROPS_PUSHPIN_IN)) return ;
  XV_SET(f, XV_SHOW, state, 0) ;
}


void
warp_mouse()
{
  XV_SET(canvas,
         WIN_MOUSE_XY, (int) xv_get(canvas, XV_WIDTH)  / 2,
                       (int) xv_get(canvas, XV_HEIGHT) / 2, 0) ;
}
