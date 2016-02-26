
/*  sunview.c
 *
 *  These are the SunView dependent graphics routines used by calctool.
 *
 *  Copyright (c) Rich Burridge - May 1988.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Version 2.2.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include "calctool.h"
#include "color.h"
#include "extern.h"
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <suntool/selection_svc.h>
#include <suntool/selection_attributes.h>

#define  ICON_SET           (void) icon_set
#define  PANEL_SET          (void) panel_set
#define  PW_SETCMSNAME      (void) pw_setcmsname
#define  PW_PUTCOLORMAP     (void) pw_putcolormap
#define  SELN_QUERY         (void) seln_query
#define  WINDOW_DESTROY     (void) window_destroy
#define  WINDOW_DONE        (void) window_done
#define  WINDOW_READ_EVENT  (void) window_read_event
#define  WINDOW_SET         (void) window_set

void canvas_proc() ;
Panel_setting panel_proc() ;
Seln_result get_proc() ;

Canvas kcanvas, rcanvas ;
Cursor main_cursor ;
Event *cevent ;
Frame frame, rframe ;
Icon calctool_icon ;
Panel panel ;
Panel_item item, base_item, display_item, op_item, ttype_item ;
Pixfont *font, *sfont, *nfont, *bfont ;
Pixwin *pw, *cpw, *rcpw ;
Seln_holder holder ;
Seln_rank rank = SELN_PRIMARY ;

short help_cursor_array[16] = {
#include "help.cursor"
} ;
mpr_static(help_cursor_pr,16,16,1,help_cursor_array) ;
struct cursor help_cursor =
         { 0, 0, PIX_SRC | PIX_DST, &help_cursor_pr } ;

short icon_image[] = {
#include "calctool.icon"
} ;
mpr_static(icon_pixrect,64,64,1,icon_image) ;


/*ARGSUSED*/
static void
canvas_proc(win,event,arg)
Canvas  win ;
Event *event ;
caddr_t arg ;
 
{
  char context = 0 ;
  int i,n,type ;
 
  x = event_x(event) ;
  y = event_y(event) ;
  cevent = event ;
  type = event_id(event) ;
  n = row*BCOLS*2 + column*2 + portion ;
  if (event_is_button(event))
    {
      if (event_is_down(event)) handle_down_event(type) ;
      else if (event_is_up(event) && (event_id(event) == down))
        {
          if (pending_op != '?' && n <= (NOBUTTONS*2))
            inv_but(row,column,portion,NORMAL) ;
          down = 0 ;
          if (n >= 0 && n <= (NOBUTTONS*2)) process_item(n) ;
        }
    }
  else if (event_is_ascii(event))
    {
      for (n = 0; n < TITEMS; n++)
        if (event_id(event) == buttons[n].value) break ;
      if (n == TITEMS) return ;
      if (n >= 0 && n <= TITEMS) process_item(n) ;
    }
  else if (event_id(event) == KBD_DONE && down)
    {
      if (pending_op != '?') draw_button(row,column,portion,NORMAL) ;
      down = 0 ;
    }
  else if ((event_id(event) == KEY_LEFT(8)) && event_is_up(event))
    {
      holder = seln_inquire(rank) ;
      if (holder.state == SELN_NONE) return ;
      SELN_QUERY(&holder,get_proc,&context,SELN_REQ_CONTENTS_ASCII,0,0) ;
      if (issel)
        for (i = 0 ; i < strlen(selection); i++)
          for (n = 0; n < TITEMS; n++)
            if (selection[i] == buttons[n].value)
              {
                process_item(n) ;
                break ;
              }
    }
  else window_default_event_proc(win,event,arg) ;
}        
         

clear_canvas(window,color)
enum can_type window ;
int color ;
{
  int height,width ;
  Canvas ctype ;

       if (window == KEYCANVAS) ctype = kcanvas ;
  else if (window == REGCANVAS) ctype = rcanvas ;
  height = (int) window_get(ctype, WIN_HEIGHT) ;
  width = (int) window_get(ctype, WIN_WIDTH) ;
  fillbox(0,0,window,width,height,0,color) ;
}


close_frame()
{
  if ((int) window_get(rframe, WIN_SHOW) == TRUE)
    WINDOW_SET(rframe, WIN_SHOW, FALSE, 0) ;
  WINDOW_SET(frame,FRAME_CLOSED,TRUE,0) ;
}


destroy_frame()
{
  WINDOW_DONE(frame) ;
}


drawbox(x,y,width,height)
int x,y,width,height ;
{
  pw_vector(cpw,x,y,x+width,y,PIX_SET,0) ;
  pw_vector(cpw,x,y,x,y+height,PIX_SET,0) ;
  pw_vector(cpw,x,y+height,x+width,y+height,PIX_SET,0) ;
  pw_vector(cpw,x+width,y,x+width,y+height,PIX_SET,0) ;
}


draw_calc()
{
  make_canvas(0) ;
}


draw_regs()
{
  WINDOW_SET(rframe, WIN_SHOW, TRUE, 0) ;
}


fillbox(x,y,window,width,height,boundry,color)
enum can_type window ;
int x,y,width,height,boundry,color ;
{
       if (window == KEYCANVAS) pw = cpw ;
  else if (window == REGCANVAS) pw = rcpw ;
  if (boundry)
    { 
      pw_writebackground(pw,x,y,width,height,PIX_CLR) ;
      pw_writebackground(pw,x+1,y+1,width-2,height-2,
                         PIX_SRC | PIX_COLOR(color)) ;
    }
  else pw_writebackground(pw,x,y,width,height,PIX_SRC | PIX_COLOR(color)) ;
}


Seln_result
get_proc(buffer)
Seln_request *buffer ;
{
  issel = 0 ;
  if (*buffer->requester.context == 0)
    {
      if (buffer == (Seln_request *) NULL ||
          *((Seln_attribute *) buffer->data) != SELN_REQ_CONTENTS_ASCII)
        return ;
      selection = buffer->data + sizeof(Seln_attribute) ;
      *buffer->requester.context = 1 ;
    }
  else selection = buffer->data ;
  issel = 1 ;
}


getxy(x,y)
int *x,*y ;
{
  *x = event_x(cevent) ;
  *y = event_y(cevent) ;
}


init_fonts()
{
  sfont = pf_open(SMALLFONT) ;
  nfont = pf_open(NORMALFONT) ;
  bfont = pf_open(BIGFONT) ;
}


init_ws_type()
{
  return 0 ;
}


load_colors()      /* Create and load calctool color map. */
{
  char colorname[CMS_NAMESIZE] ;
  u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;

  iscolor = (cpw->pw_pixrect->pr_depth == 8) ? 1 : 0 ;
  SPRINTF(colorname,"%s%D",CALC_COLOR,getpid()) ;
  PW_SETCMSNAME(cpw,colorname) ;

  calc_colorsetup(red,green,blue) ;
  PW_PUTCOLORMAP(cpw,0,CALC_COLORSIZE,red,green,blue) ;
}


make_frames(argc,argv)
int argc ;
char *argv[] ;
{
  frame = window_create((Window) 0, FRAME,
                        FRAME_ICON, calctool_icon,
                        FRAME_SHOW_LABEL, FALSE,
                        FRAME_SUBWINDOWS_ADJUSTABLE, FALSE,
                        FRAME_NO_CONFIRM, TRUE,
                        WIN_TOP_MARGIN, DISPLAY,
                        WIN_ROW_HEIGHT, BHEIGHT,
                        WIN_COLUMN_WIDTH, BWIDTH,
                        WIN_ROWS, BROWS,
                        WIN_COLUMNS, BCOLS,
                        FRAME_ARGS, argc,argv,
                        0) ;
  rframe = window_create(frame, FRAME,
                         FRAME_SHOW_LABEL, FALSE,
                         FRAME_NO_CONFIRM, TRUE,
                         WIN_X, TWIDTH+15,
                         WIN_Y, 0,
                         WIN_SHOW, FALSE,
                         WIN_WIDTH, TWIDTH,
                         WIN_HEIGHT, 200,
                         WIN_FONT,nfont,
                         0) ;
 
}


make_icon()
{
  calctool_icon = icon_create(ICON_WIDTH,42, ICON_IMAGE, &icon_pixrect, 0) ;
}


make_items()
{
  display_item = panel_create_item(panel,PANEL_TEXT,
                                   PANEL_VALUE_Y,DISPLAY-27,
                                   PANEL_NOTIFY_LEVEL, PANEL_ALL,
                                   PANEL_ACCEPT_KEYSTROKE, 1,
                                   PANEL_NOTIFY_PROC, panel_proc,
                                   PANEL_VALUE_FONT,nfont,
                                   0) ;
  base_item = panel_create_item(panel,PANEL_MESSAGE,
                                PANEL_LABEL_X,BBORDER,
                                PANEL_LABEL_Y,DISPLAY-10,
                                PANEL_LABEL_FONT,sfont,
                                PANEL_LABEL_STRING,"",
                                0) ;
  ttype_item = panel_create_item(panel,PANEL_MESSAGE,
                                PANEL_LABEL_X,BBORDER+(BWIDTH+BGAP),
                                PANEL_LABEL_Y,DISPLAY-10,
                                PANEL_LABEL_FONT,sfont,
                                PANEL_LABEL_STRING,"",
                                0) ;
  op_item = panel_create_item(panel,PANEL_MESSAGE,
                                PANEL_LABEL_X,BBORDER+2*(BWIDTH+BGAP),
                                PANEL_LABEL_Y,DISPLAY-10,
                                PANEL_LABEL_FONT,sfont,
                                PANEL_LABEL_STRING,"",
                                0) ;
  main_cursor = window_get(kcanvas,WIN_CURSOR) ;
}


make_subframes()
{
  panel = window_create(frame, PANEL,
                        PANEL_BLINK_CARET, 0,
                        WIN_CONSUME_KBD_EVENTS, WIN_ASCII_EVENTS, 0,
                        WIN_HEIGHT, DISPLAY,
                        0) ;
  rcanvas = window_create(rframe,CANVAS,0) ;
  kcanvas = window_create(frame,CANVAS,
                          CANVAS_RETAINED, FALSE,
                          CANVAS_REPAINT_PROC, draw_calc,
                          WIN_BELOW,panel,
                          WIN_WIDTH,TWIDTH,
                          WIN_HEIGHT,THEIGHT,
                          WIN_FONT,nfont,
                          WIN_EVENT_PROC,canvas_proc,
                          0) ;

  WINDOW_SET(kcanvas,WIN_CONSUME_KBD_EVENT,WIN_ASCII_EVENTS,0) ;
  WINDOW_SET(kcanvas,WIN_CONSUME_KBD_EVENTS,WIN_LEFT_KEYS,
                     WIN_TOP_KEYS,WIN_RIGHT_KEYS,0,0) ;
  WINDOW_SET(kcanvas,WIN_CONSUME_KBD_EVENT,WIN_UP_EVENTS,0) ;
  WINDOW_SET(kcanvas,WIN_IGNORE_PICK_EVENT,LOC_MOVE,0) ;

  cpw = canvas_pixwin(kcanvas) ;
  rcpw = canvas_pixwin(rcanvas) ;
}


/*ARGSUSED*/
Panel_setting
panel_proc(item,event)
Panel_item item ;
Event *event ;
                      
{
  int chr,n ;
 
  chr = event_id(event) ;
  for (n = 0; n < TITEMS; n++)
    {
      if (chr == buttons[n].value)
        {
          PANEL_SET(item,PANEL_NOTIFY_LEVEL,PANEL_NONE,0) ;
          process_item(n) ;
          PANEL_SET(item,PANEL_NOTIFY_LEVEL,PANEL_ALL,0) ;
          return ;
        }
    }
}
 
 
set_cursor(type)
int type ;
{
  switch (type)
    {
      case HELPCURSOR : WINDOW_SET(kcanvas,WIN_CURSOR,&help_cursor,0) ;
                        break ;
      case MAINCURSOR : WINDOW_SET(kcanvas,WIN_CURSOR,main_cursor,0) ;
    }
}


set_item(itemno,str)
enum item_type itemno ;
char *str ;
{
  if (itemno == DISPLAYITEM)
    {
      PANEL_SET(display_item,PANEL_VALUE_X,5+(MAX_DIGITS - strlen(display))*9,
                PANEL_VALUE,display,0) ;
      return ;
    }
  else if (itemno == BASEITEM) item = base_item ;
  else if (itemno == OPITEM) item = op_item ;
  else if (itemno == TTYPEITEM) item = ttype_item ;
  PANEL_SET(item,PANEL_LABEL_STRING,str,0) ;
}
 
 
start_tool()
{
  make_registers() ;         /* Calculate memory register frame values. */
  make_canvas(0) ;           /* Draw the calculators buttons. */
  set_item(BASEITEM,base_str[(int) base]) ;    /* Initial arithmetic base. */
  set_item(TTYPEITEM,ttype_str[(int) ttype]) ; /* Initial trigonometric type. */
  window_fit(frame) ;
  window_main_loop(frame) ;
}


text(x,y,window,fontno,color,str)
enum font_type fontno ;
enum can_type window ;
int x,y,color ;
char *str ;
{
       if (fontno == SFONT) font = sfont ;
  else if (fontno == NFONT) font = nfont ;
  else if (fontno == BFONT) font = bfont ;
       if (window == KEYCANVAS) pw = cpw ;
  else if (window == REGCANVAS) pw = rcpw ;
  pw_ttext(pw,x,y,PIX_SRC | PIX_COLOR(color),font,str) ;
}


toggle_reg_canvas()

{
  rstate = !rstate ;
  if (rstate) WINDOW_SET(rframe, WIN_SHOW, TRUE, 0) ;
  else WINDOW_SET(rframe, WIN_SHOW, FALSE, 0) ;
}
