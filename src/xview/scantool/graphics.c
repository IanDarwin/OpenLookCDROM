 
/*  @(#)graphics.c 1.5 90/04/10
 *
 *  Independent graphics routines associated with the scantool program.
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
#include <strings.h>
#include <signal.h>
#include "scantool.h"
#include "scantool_extern.h"


do_repaint()         /* Draw titleline and page layout. */
{
  int i ;
 
  draw_area(0, 0, SCAN_WIDTH, SCAN_HEIGHT, GCLR) ;
  draw_text(0*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Brightness") ;
  draw_text(1*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Contrast") ;
  draw_text(2*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Grain") ;
  draw_text(3*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Help") ;
  draw_text(4*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Resolution") ;
  draw_text(5*MBAR_WIDTH+10, 15, STEN_OFF, BFONT, "Set") ;
  draw_area(0, 0, SCAN_WIDTH, MBAR_HEIGHT, GNOT) ;

  make_switch(SCAN_WIDTH-150, 0*SWITCH_HEIGHT+40,
              "Mode", "Line Art", "Halftone") ;
  make_switch(SCAN_WIDTH-150, 1*SWITCH_HEIGHT+40,
              "Data Transfer", "Uncompressed", "Compressed") ;
  make_switch(SCAN_WIDTH-150, 2*SWITCH_HEIGHT+40,
              "Serial Port", "A", "B") ;
  make_switch(SCAN_WIDTH-150, 3*SWITCH_HEIGHT+40,
              "Baud Rate", "9600", "19200") ;

  for (i = 0; i < 4; i++)
    set_switch(SCAN_WIDTH-150, i*SWITCH_HEIGHT+50+switches[i]*20, ON) ;

  if (scanning)
    make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+20, "Scan", B_INVERT) ;
  else make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+20, "Scan", B_NORMAL) ;

  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+60, "Cancel", B_NORMAL) ;

  if (showing)
    make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+100, "Show", B_INVERT) ;
  else make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+100, "Show", B_NORMAL) ;

  draw_scanning_frame(SCAN_FRAME_X, SCAN_FRAME_Y) ;

  switch ((int) drawstate)
    {
      case DO_NOTHING  : break ;
      case DO_PICNAME  : draw_picarea() ;
                         break ;
      case DO_MESSAGE  : make_message() ;
                         break ;
      case DO_HELP     : get_help() ;
                         break ;
      case DO_SETTINGS : draw_settings() ;
    }
}


draw_frame(x, y, width, height)
int x, y, width, height ;
{
  draw_area(x,   y,   width,    height,    GSET) ;
  draw_area(x+1, y+1, width-2,  height-2,  GCLR) ;
  draw_area(x+3, y+3, width-6,  height-6,  GSET) ;
  draw_area(x+5, y+5, width-10, height-10, GCLR) ;
}


draw_picarea()
{
  char dummy[MAXLINE] ;
  int nochars, x, y ;

  x = (SCAN_WIDTH - 500) / 2 ;
  y = (SCAN_HEIGHT - 50) / 2 ;
  draw_frame(x, y, 500, 50) ;
  draw_text(x + 10, y + 27, STEN_OFF, BFONT, "Name:") ;
  draw_rect(x + 70, y + 10, x + 265, y + 35, GSET) ;
  nochars = (strlen(picname) <= 20) ? strlen(picname) : 20 ;
  STRNCPY(dummy, &picname[strlen(picname) - nochars], nochars) ;
  dummy[nochars] = '\0' ;
  draw_text(x + 80, y + 27, STEN_OFF, BFONT, dummy) ;
  draw_line(x + 80 + get_strwidth(BFONT, dummy), y + 15,
            x + 80 + get_strwidth(BFONT, dummy), y + 30, GSET) ;
  make_button(x + 300, y + 10, "OK", B_NORMAL) ;
  make_button(x + 400, y + 10, "Cancel", B_NORMAL) ;
  drawstate = DO_PICNAME ;
  butx = x ;
  buty = y ;
}


draw_rect(x1, y1, x2, y2, op)
int x1, y1, x2, y2 ;
enum op_type op ;
{
  draw_line(x1, y1, x2, y1, op) ;
  draw_line(x1, y1, x1, y2, op) ;
  draw_line(x2, y1, x2, y2, op) ;
  draw_line(x1, y2, x2, y2, op) ;
}


draw_scanning_frame(x, y)     /* Draw the current scanning frame. */
int x, y ;
{
  char number[3] ;
  int i ;

  for (i = 0; i <= 35; i++)       /* Draw horizontal rule. */
    if (!(i % 4))
      {
        SPRINTF(number, "%1d", i / 4) ;
        draw_text(x+i*LINEGAP+2, y-15, STEN_ON, NFONT, number) ;
        draw_line(x+i*LINEGAP, y-10, x+i*LINEGAP, y-26, GSET) ;
      }
    else if (!(i % 2))
      draw_line(x+i*LINEGAP, y-10, x+i*LINEGAP, y-18, GSET) ;
    else draw_line(x+i*LINEGAP, y-10, x+i*LINEGAP, y-14, GSET) ;

  draw_line(x, y-10, x+36*LINEGAP, y-10, GSET) ;

  for (i = 0; i <= 45; i++)       /* Draw vertical rule. */
    if (!(i % 4))
      {
        SPRINTF(number, "%1d", i / 4) ;
        draw_text(x-26, y+i*LINEGAP+12, STEN_ON, NFONT, number) ;
        draw_line(x-10, y+i*LINEGAP, x-26, y+i*LINEGAP, GSET) ;
      }
    else if (!(i % 2))
      draw_line(x-10, y+i*LINEGAP, x-18, y+i*LINEGAP, GSET) ;
    else draw_line(x-10, y+i*LINEGAP, x-14, y+i*LINEGAP, GSET) ;

  draw_line(x-10, y, x-10, y+46*LINEGAP, GSET) ;

/* Initial page frame. */
  draw_rect(x,   y,   x+34*LINEGAP,   y+44*LINEGAP,   GSET) ;
  draw_rect(x+1, y+1, x+34*LINEGAP+1, y+44*LINEGAP+1, GSET) ;

/* Make gray surrondings. */
  draw_line(x, y+44*LINEGAP, x, SCAN_HEIGHT, GSET) ;
  draw_line(x+34*LINEGAP, y,x+36*LINEGAP, y, GSET) ;
  draw_line(x+36*LINEGAP, y-26,x+36*LINEGAP, SCAN_HEIGHT, GSET) ;
  grey_area(x+1, y+44*LINEGAP+2, 36*LINEGAP-1, 2*LINEGAP-2) ;
  grey_area(x+LINEGAP*34+2, y+1, 2*LINEGAP-2, SCAN_HEIGHT-65) ;

/* Draw initial scanning frame. */
  draw_rect(x+framevals[X1]*FRAMEGAP, y+framevals[Y1]*FRAMEGAP,
            x+framevals[X2]*FRAMEGAP, y+framevals[Y2]*FRAMEGAP, GSET) ;
}


get_picname()       /* Get new picture name. */
{
  char c, dummy[MAXLINE] ;
  int finished, nochars, state ;

  finished = 0 ;
  if (type == LEFT_DOWN)
    {
           if (curx > butx+300 && curx < butx+300+BUTTON_WIDTH &&
               cury > buty+10  && cury < buty+10+BUTTON_HEIGHT)
        {
          make_button(butx+300, buty+10, "OK", B_INVERT) ;
          state = OK ;
          finished = 1 ;
        }
      else if (curx > butx+400 && curx < butx+400+BUTTON_WIDTH &&
               cury > buty+10  && cury < buty+10+BUTTON_HEIGHT)
        {
          state = CANCEL ;
          make_button(butx+400, buty+10, "Cancel", B_INVERT) ;
          finished = 1 ;
        }
    }
  else if (type == KEYBOARD)
    {
      c = cur_ch ;
      if (c == BACKSPACE || c == DEL)
        picname[strlen(picname)-1] = '\0' ;
      else if (c == RETURN || c == LINEFEED)
        {
          state = OK ;
          finished = 1 ;
        }
      else if (c >= 32) STRNCAT(picname, &c, 1) ;
      draw_area(butx+71, buty+11, 193, 23, GCLR) ;
      nochars = (strlen(picname) <= 20) ? strlen(picname) : 20 ;
      STRNCPY(dummy, &picname[strlen(picname) - nochars], nochars) ;
      dummy[nochars] = '\0' ;
      draw_text(butx+80, buty+27, STEN_OFF, BFONT, dummy) ;
      draw_line(butx + 80 + get_strwidth(BFONT, dummy), buty + 15,
                butx + 80 + get_strwidth(BFONT, dummy), buty + 30, GSET) ;
    }

  if (finished)
    {
      if (state == CANCEL) STRCPY(picname, old_picname) ;
      drawstate = DO_NOTHING ;
      do_repaint() ;              /* Redraw original screen. */
    }
}


make_menus()
{
  create_menu(BRIGHTNESS_M, "BRIGHTNESS", br_strs) ;
  create_menu(CONTRAST_M,   "CONTRAST",   con_strs) ;
  create_menu(GRAIN_M,      "GRAIN",      gr_strs) ;
  create_menu(HELP_M,       "HELP",       help_strs) ;
  create_menu(RESOLUTION_M, "RESOLUTION", res_strs) ;
  create_menu(SET_M,        "SET",        set_strs) ;
}


process_event()
{
  enum menu_type column ;
  int row, value ;

       if (type == IGNORE)              return ;
  else if (type == REPAINT)             do_repaint() ;
  else if (drawstate == DO_MESSAGE   ||
           drawstate == DO_HELP      ||
           drawstate == DO_SETTINGS)    wait_for_ok() ;
  else if (drawstate == DO_PICNAME)     get_picname() ;
  else
    {
      column = (enum menu_type) ((curx - 10) / MBAR_WIDTH) ;
      row = cury / MBAR_HEIGHT ;
      if (type == RIGHT_DOWN && row == 0)
        {
          value = display_menu(column) ;
          if (value) process_menu(column, value) ;
        }
      else if (type == LEFT_DOWN)
        {
               if (curx > SCAN_WIDTH-150                    &&
                   curx < SCAN_WIDTH-90                     &&
                   cury > 4*SWITCH_HEIGHT+20                &&
                   cury < 4*SWITCH_HEIGHT+20+BUTTON_HEIGHT) make_scan() ;
          else if (curx > SCAN_WIDTH-150                    &&
                   curx < SCAN_WIDTH-90                     &&
                   cury > 4*SWITCH_HEIGHT+60                &&
                   cury < 4*SWITCH_HEIGHT+60+BUTTON_HEIGHT) stop_scan() ;
          else if (curx > SCAN_WIDTH-150                    &&
                   curx < SCAN_WIDTH-90                     &&
                   cury > 4*SWITCH_HEIGHT+100               &&
                   cury < 4*SWITCH_HEIGHT+100+BUTTON_HEIGHT) show() ;
          else test_switch(curx, cury) ;   /* Test for a box switch press. */
        }
    }
}


select_frame()    /* Construct a new scanning window. */
{
  int oldx = 0 ;
  int oldy = 0 ;
  int x1, y1, x2, y2 ;
  int drawold = 0 ;
  int found = 0 ;

  set_cursor(FRAME_CUR) ;

/* Remove current scan settings, if any. */
  draw_area(SCAN_FRAME_X+2, SCAN_FRAME_Y+2, 34*LINEGAP-4, 44*LINEGAP-4, GCLR) ;
  draw_area(SCAN_FRAME_X-9, SCAN_FRAME_Y, 9, 46*LINEGAP-1, GCLR) ;
  draw_area(SCAN_FRAME_X, SCAN_FRAME_Y-9, 34*LINEGAP-1, 9, GCLR) ;

  do
    {
      get_event() ;
      handle_event() ;
      if (curx > SCAN_FRAME_X && curx < SCAN_FRAME_X+34*LINEGAP &&
          cury > SCAN_FRAME_Y && cury < SCAN_FRAME_Y+44*LINEGAP)
        {
          curx = (curx / (FRAMEGAP) * (FRAMEGAP)) + 1 ;
          cury = (cury / (FRAMEGAP) * (FRAMEGAP)) + 2 ;
          if (type == LEFT_DOWN) found = 1 ;
          if (type != MOUSE_MOVE) continue ;
          if (drawold)
            {
              draw_line(oldx, SCAN_FRAME_Y-1, oldx, SCAN_FRAME_Y-9, GXOR) ;
              draw_line(SCAN_FRAME_X-1, oldy, SCAN_FRAME_X-9, oldy, GXOR) ;
            }
          draw_line(curx, SCAN_FRAME_Y-1, curx, SCAN_FRAME_Y-9, GXOR) ;
          draw_line(SCAN_FRAME_X-1, cury, SCAN_FRAME_X-9, cury, GXOR) ;
          oldx = curx ;
          oldy = cury ;
          drawold++ ;
        }
    }    
  while (!found) ;
  x1 = curx ;
  y1 = cury ;

  found = 0 ;
  drawold = 0 ;
  do
    {
      get_event() ;
      handle_event() ;
      if (curx > SCAN_FRAME_X && curx < SCAN_FRAME_X+34*LINEGAP &&
          cury > SCAN_FRAME_Y && cury < SCAN_FRAME_Y+44*LINEGAP)
        {
          curx = (curx / (FRAMEGAP) * (FRAMEGAP)) + 1 ;
          cury = (cury / (FRAMEGAP) * (FRAMEGAP)) + 2 ;
          if (type == LEFT_UP) found = 1 ;
          if (type != MOUSE_DRAG) continue ;
          if (drawold)
            {
              draw_line(oldx, SCAN_FRAME_Y-1, oldx, SCAN_FRAME_Y-9, GXOR) ;
              draw_line(SCAN_FRAME_X-1, oldy, SCAN_FRAME_X-9, oldy, GXOR) ;
              draw_rect(x1, y1, oldx, oldy, GXOR) ;
            }
          draw_line(curx, SCAN_FRAME_Y-1, curx, SCAN_FRAME_Y-9, GXOR) ;
          draw_line(SCAN_FRAME_X-1, cury, SCAN_FRAME_X-9, cury, GXOR) ;
          draw_rect(x1, y1, curx, cury, GXOR) ;
          oldx = curx ;
          oldy = cury ;
          drawold++ ;
        }
    }
  while (!found) ;

  x2 = curx ;
  y2 = cury ;
  if (x1 > x2)
    {
      oldx = x2 ;
      x2 = x1 ;
      x1 = oldx ;
    }
  if (y1 > y2)
    {
      oldy = y2 ;
      y2 = y1 ;
      y1 = oldy ;
    }
  framevals[X1] = (x1 - SCAN_FRAME_X) / (FRAMEGAP) ;
  framevals[Y1] = (y1 - SCAN_FRAME_Y) / (FRAMEGAP) ;
  framevals[X2] = (x2 - SCAN_FRAME_X) / (FRAMEGAP) ;
  framevals[Y2] = (y2 - SCAN_FRAME_Y) / (FRAMEGAP) ;
  set_cursor(MAIN_CUR) ;
}


set_switch(x, y, position)
int x, y, position ;
{
  if (position == ON) draw_image(x, y-2, 16, 16, S_INVERT) ;
  else                draw_image(x, y-2, 16, 16, S_NORMAL) ;
}


stop_scan()       /* Stop the current scan (if there is one). */
{
  if (!scanning) return ;
  make_button(SCAN_WIDTH-150, 4*SWITCH_HEIGHT+60, "Cancel", B_INVERT) ;
  KILL(pid, SIGHUP) ;
}
