 
/*  @(#)sunview.c 1.4 90/04/04
 *
 *  SunView dependent graphics routines used by scantool.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <sys/fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "scantool.h"
#include "images.h"
#include "scantool_extern.h"
#include <suntool/sunview.h>
#include <suntool/canvas.h>

#define  MENU_SET                       (void) menu_set
#define  NOTIFY_DISPATCH                (void) notify_dispatch
#define  NOTIFY_INTERPOSE_DESTROY_FUNC  (void) notify_interpose_destroy_func
#define  NOTIFY_INTERPOSE_EVENT_FUNC    (void) notify_interpose_event_func
#define  NOTIFY_SET_WAIT3_FUNC          (void) notify_set_wait3_func
#define  PR_DESTROY                     (void) pr_destroy
#define  PW_REPLROP                     (void) pw_replrop
#define  PW_ROP                         (void) pw_rop
#define  PW_TEXT                        (void) pw_text
#define  PW_TTEXT                       (void) pw_ttext
#define  PW_VECTOR                      (void) pw_vector
#define  PW_WRITEBACKGROUND             (void) pw_writebackground
#define  WINDOW_READ_EVENT              (void) window_read_event
#define  WINDOW_SET                     (void) window_set

/* Fonts used by scantool. */
#define  BOLD_FONT      "/usr/lib/fonts/fixedwidthfonts/screen.b.14"
#define  NORMAL_FONT    "/usr/lib/fonts/fixedwidthfonts/screen.r.14"

struct pixfont *open_font() ;
void repaint_show_canvas() ;

Canvas canvas, show_canvas ;
Cursor cursor[MAXCURSORS] ;
Event *cur_event ;
Frame base_frame, show_frame ;
Icon scantool_icon ;
Menu menus[MAXMENUS] ;
Notify_value destroy(), handle_resize() ;
Pixfont *font[MAXFONTS] ;
Pixrect *images[MAXIMAGES] ;
Pixrect *spr ;
Pixwin *spw, *wpw ;

int firsttime ;      /* Set if we haven't resized the window yet. */
int init_height ;    /* Initial height of the scantool window. */
int init_width ;     /* Initial width of the scantool window. */

struct rasterfile hdr ;

mpr_static(grey_pr,          16, 16, 1, grey_image) ;
mpr_static(icon_pr,          64, 64, 1, icon_image) ;
mpr_static(exclaim_pr,       64, 64, 1, exclaim_image) ;
mpr_static(button_normal_pr, 64, 64, 1, button_normal_image) ;
mpr_static(button_invert_pr, 64, 64, 1, button_invert_image) ;
mpr_static(switch_normal_pr, 16, 16, 1, switch_normal_image) ;
mpr_static(switch_invert_pr, 16, 16, 1, switch_invert_image) ;
mpr_static(main_cursor_pr,   16, 16, 1, main_cursor_array) ;
mpr_static(hour_cursor_pr,   16, 16, 1, hour_cursor_array) ;
mpr_static(help_cursor_pr,   16, 16, 1, help_cursor_array) ;
mpr_static(frame_cursor_pr,  16, 16, 1, frame_cursor_array) ;
 

/*ARGSUSED*/
void
canvas_proc(canvas, event)
Canvas canvas ;
Event *event ;
{
  cur_event = event ;
  handle_event() ;       /* Determine what kind of event it is. */
  process_event() ;      /* And process it. */
}

create_menu(mtype, title, values)    /* Create popup menus for cycle items. */
enum menu_type mtype ;
char *title, *values[] ;
{
  int i = 0 ;
  int more = 1 ;   /* Cleared when current menu is complete.*/

  menus[(int) mtype] = menu_create(MENU_TITLE_ITEM, title,
                                   MENU_FONT,       font[(int) NFONT],
                                   0) ;
  do
    {
      if (values[i] != NULL)
        MENU_SET(menus[(int) mtype], MENU_STRING_ITEM, values[i], i+1, 0) ;
      else more = 0 ;
      i++ ;
    }
  while (more) ;
}

/*ARGSUSED*/
Notify_value
destroy(client, status)
Notify_client client ;
Destroy_status status ;
{
  stop_scan() ;        /* Stop the current scan (if there is one). */
  exit(0) ;
}

display_menu(menuno)
enum menu_type menuno ;
{
  return((int) menu_show(menus[(int) menuno], canvas,
                         canvas_window_event(canvas, cur_event), 0)) ;
}

draw_area(x, y, width, height, op)
int x, y, width, height ;
enum op_type op ;
{
  PW_WRITEBACKGROUND(wpw, x, y, width, height, ops[(int) op]) ;
}

draw_image(x, y, width, height, image)
int x, y, width, height ;
enum image_type image ;
{
  PW_ROP(wpw, x, y, width, height, PIX_SRC, images[(int) image], 0, 0) ;
}

draw_line(x1, y1, x2, y2, op)
int x1, y1, x2, y2 ;
enum op_type op ;
{
  PW_VECTOR(wpw, x1, y1, x2, y2, ops[(int) op], 1) ;
}

draw_text(x, y, stencil, ftype, str)
int x, y ;
enum sten_type stencil ;
enum font_type ftype ;
char *str ;
{
  switch (stencil)
    {
      case STEN_OFF : PW_TEXT(wpw,  x, y, PIX_SRC, font[(int) ftype], str) ;
                      break ;
      case STEN_ON  : PW_TTEXT(wpw, x, y, PIX_SET, font[(int) ftype], str) ;
                      break ;
      case STEN_INV : PW_TTEXT(wpw, x, y, PIX_CLR, font[(int) ftype], str) ;
    }
}

get_event()       /* Get the next SunView event. */
{
  WINDOW_READ_EVENT(canvas, cur_event) ;
}

get_strwidth(ftype, str)    /* Get width in pixels of string value. */
enum font_type ftype ;
char *str ;
{
  struct pr_size size ;

  size = pf_textwidth(strlen(str), font[(int) ftype], str) ;
  return(size.x) ;
}

grey_area(x, y, width, height)
{
  PW_REPLROP(wpw, x, y, width, height, PIX_SRC, &grey_pr, 0, 0) ;
}

handle_event()        /* Work out what kind of event this is. */
{
  curx = event_x(cur_event) ;
  cury = event_y(cur_event) ;
  cur_ch = event_id(cur_event) ;

  if (event_is_button(cur_event) && event_is_down(cur_event))
    {
           if (cur_ch == MS_LEFT)     type = LEFT_DOWN ;
      else if (cur_ch == MS_MIDDLE)   type = MIDDLE_DOWN ;
      else if (cur_ch == MS_RIGHT)    type = RIGHT_DOWN ;
    }
  else if (event_is_button(cur_event) && event_is_up(cur_event))
    {
           if (cur_ch == MS_LEFT)     type = LEFT_UP ;
      else if (cur_ch == MS_MIDDLE)   type = MIDDLE_UP ;
      else if (cur_ch == MS_RIGHT)    type = RIGHT_UP ;
    }
  else if (event_is_ascii(cur_event)) type = KEYBOARD ;
  else if (cur_ch == LOC_MOVE)        type = MOUSE_MOVE ;
  else if (cur_ch == LOC_DRAG)        type = MOUSE_DRAG ;
  else if (cur_ch == WIN_REPAINT)     type = REPAINT ;
  else                                type = IGNORE ;
}

Notify_value
handle_resize(frame, event, arg, type)
Frame frame ;
Event *event ;
Notify_arg arg ;
Notify_event_type type ;
{
  Notify_value value ;
  Rect *r ;

  value = notify_next_event_func(frame, (Notify_event) event, arg, type) ;
  if (event_id(event) == WIN_RESIZE)
    if (firsttime)
      {
        init_width = (int) window_get(base_frame, WIN_WIDTH) ;
        init_height = (int) window_get(base_frame, WIN_HEIGHT) ;
        firsttime = 0 ;
      }
    else
      {
        r = (Rect *) LINT_CAST(window_get(frame, WIN_RECT)) ;
        r->r_width  = init_width ;
        r->r_height = init_height ;
        WINDOW_SET(frame, FRAME_OPEN_RECT, r, 0) ;
        do_repaint() ;
      }   
  return(value) ;
}

init_fonts()
{
  font[(int) NFONT] = open_font(NORMAL_FONT) ;
  font_width = font[(int) NFONT]->pf_defaultsize.x ;
  font[(int) BFONT] = open_font(BOLD_FONT) ;
}

init_ws_type()
{
  ops[(int) GSET] = PIX_SET ;
  ops[(int) GCLR] = PIX_CLR ;
  ops[(int) GXOR] = PIX_SRC ^ PIX_DST ;
  ops[(int) GSRC] = PIX_SRC ;
  ops[(int) GOR]  = PIX_SRC | PIX_DST ;
  ops[(int) GNOT] = PIX_NOT(PIX_DST) ;

  firsttime = 1 ;
}

make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  scantool_icon = icon_create(ICON_IMAGE, &icon_pr, 0) ;
  base_frame = window_create((Window) 0,           FRAME,
                             FRAME_ICON,           scantool_icon,
                             FRAME_LABEL,          " MICROTEK MSF-300A Image Scanner",
                             FRAME_EMBOLDEN_LABEL, TRUE,
                             FRAME_ARGS,           argc,argv,
                             0) ;

  show_frame = window_create(base_frame,       FRAME,
                             FRAME_SHOW_LABEL, TRUE,
                             WIN_WIDTH,        650,
                             WIN_HEIGHT,       900,
                             WIN_X,            0,
                             WIN_Y,            0,
                             WIN_SHOW,         FALSE,
                             0) ;
  NOTIFY_INTERPOSE_EVENT_FUNC(base_frame, handle_resize, NOTIFY_SAFE) ;
}

make_subframes()
{
  cursor[(int) FRAME_CUR] = cursor_create(CURSOR_IMAGE, &frame_cursor_pr,
                                          CURSOR_XHOT,  7,
                                          CURSOR_YHOT,  7,
                                          0) ;
  cursor[(int) HELP_CUR]  = cursor_create(CURSOR_IMAGE, &help_cursor_pr, 0) ;
  cursor[(int) HOUR_CUR]  = cursor_create(CURSOR_IMAGE, &hour_cursor_pr, 0) ;
  cursor[(int) MAIN_CUR]  = cursor_create(CURSOR_IMAGE, &main_cursor_pr, 0) ;
 
  canvas = window_create(base_frame, CANVAS,
                         WIN_EVENT_PROC,      canvas_proc,
                         WIN_CURSOR,          cursor[(int) MAIN_CUR],
                         WIN_WIDTH,           SCAN_WIDTH,
                         WIN_HEIGHT,          SCAN_HEIGHT,
                         CANVAS_RETAINED,     FALSE,
                         CANVAS_FIXED_IMAGE,  FALSE,
                         0) ;
  WINDOW_SET(canvas, WIN_CONSUME_KBD_EVENTS, WIN_ASCII_EVENTS,
                     WIN_UP_EVENTS, 0, 0) ;
  WINDOW_SET(canvas, WIN_CONSUME_PICK_EVENTS, LOC_DRAG, LOC_MOVE, 0, 0) ;
  wpw = canvas_pixwin(canvas) ;

  show_canvas = window_create(show_frame, CANVAS,
                              CANVAS_REPAINT_PROC, repaint_show_canvas,
                              0) ;
  spw = canvas_pixwin(show_canvas) ;
  spr = NULL ;

  images[(int) B_NORMAL]      = &button_normal_pr ;
  images[(int) B_INVERT]      = &button_invert_pr ;
  images[(int) S_NORMAL]      = &switch_normal_pr ;
  images[(int) S_INVERT]      = &switch_invert_pr ;
  images[(int) EXCLAIM_IMAGE] = &exclaim_pr ;
}

Pixfont *
open_font(fontname)
char *fontname ;
{
  Pixfont *f ;

  if ((f = pf_open(fontname)) == NULL)
    if ((f = pf_default()) == NULL)
      {
        perror("couldn't get the default font.") ;
        exit(1) ;
      }
  return(f) ;
}

/*ARGSUSED*/
void
repaint_show_canvas(canvas, pw, repaint_area)     /* Show current picture. */
Canvas canvas ;
Pixwin *pw ;
Rectlist *repaint_area ;
{
  PW_ROP(pw, 0, 0, hdr.ras_width, hdr.ras_height, PIX_SRC, spr, 0, 0) ;
}

set_cursor(ctype)
enum cur_type ctype ;
{
  WINDOW_SET(canvas, WIN_CURSOR, cursor[(int) ctype], 0) ;
}

show()      /* Display scanned image. */
{
  Rect *temprect ;
  char errmes[MAXLINE] ;
  int fd ;

  showing = 1 ;
  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+100, "Show", B_INVERT) ;
  if (spr) PR_DESTROY(spr) ;
  if ((fd = open(picname, O_RDONLY | O_NDELAY, 0644)) == -1)
    {
      SPRINTF(errmes, "Cannot open %s.", picname) ;
      make_display(errmes) ;
    }
  else
    { 
      READ(fd, (char *) &hdr, sizeof(struct rasterfile)) ;
      if (hdr.ras_maplength)
        {
          make_display("Color rasterfiles currently not supported.") ;
          return ;
        }
      spr = mem_create(hdr.ras_width, hdr.ras_height, hdr.ras_depth) ;
      READ(fd, (char *) (mpr_d(spr)->md_image),
           hdr.ras_width * hdr.ras_height) ;
      CLOSE(fd) ;
      temprect = (Rect *) LINT_CAST(window_get(show_frame, FRAME_OPEN_RECT)) ;
      temprect->r_left = temprect->r_top = 0 ;
      temprect->r_height = hdr.ras_height ;
      temprect->r_width = hdr.ras_width ;
      WINDOW_SET(show_frame, FRAME_OPEN_RECT, temprect, 0) ;
      WINDOW_SET(show_frame, FRAME_LABEL, picname, 0) ;
      WINDOW_SET(show_frame, WIN_SHOW, TRUE, 0) ;
    }
  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+100, "Show", B_NORMAL) ;
  showing = 0 ;
}

start_tool()
{
  window_fit(base_frame) ;
  NOTIFY_INTERPOSE_DESTROY_FUNC(base_frame, destroy) ;
  window_main_loop(base_frame) ;
}

/*ARGSUSED*/
Notify_value
wait_child(frame, pid, status, rusage)
Window frame ;
int pid ;
union wait *status ;
struct rusage *rusage ;
{
  char output[MAXLINE] ;
  int high, low ;

  if (WIFSTOPPED(*status)) return (NOTIFY_IGNORED) ;

  high = (status->w_status & 0xFF00) >> 8 ;
  low = status->w_status & 0xFF ;
  if (low) SPRINTF(output, "Scan: terminated with signal %1d", (low & 0x3F)) ;
  else
    {
      switch (high)
        {
          case 0  : SPRINTF(output, "Scan: successful. %s saved",
                                     picname) ;
                    break ;
          case 1  : SPRINTF(output, "Scan: cannot open tty port %c",
                                     switches[SERIAL_PORT] + 'A') ;
                    break ;
          case 2  : SPRINTF(output, "Scan: cannot open temporary image file") ;
                    break ;
          case 3  : SPRINTF(output, "Scan: cannot open raster header file") ;
                    break ;
          case 4  : SPRINTF(output, "Scan: scanner not responding; aborting") ;
                    break ;
          case 5  : SPRINTF(output, "Scan: invalid command line argument") ;
                    break ;
          default : if (high >= 100 && high < 200)
                      SPRINTF(output, "Scan: scanner error %1d; aborting",
                                       high - 100) ;
                    else
                      SPRINTF(output, "Scan: terminated with signal %1d",
                                       high - 200) ;
        }
    }
  make_display(output) ;
  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+20, "Scan",   B_NORMAL) ;
  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+60, "Cancel", B_NORMAL) ;
  scanning = 0 ;
  return (NOTIFY_DONE) ;
}

wait_on_child(pid)      /* Wait on child scan process. */
int pid ;
{
  NOTIFY_SET_WAIT3_FUNC(base_frame, wait_child, pid) ;
}
