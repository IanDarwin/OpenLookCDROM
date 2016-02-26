#ident "@(#)tnt.c	1.20 91/09/14"

/*
 *
 * Copyright (c) 1991 by Sun Microsystems, Inc.
 *
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Josh Siegel (siegel@sun.com)
 */
  
#include "tnt_cps.h"
#include "term.h"
#include <sys/time.h>
#ifdef OW_V2
#include <NeWS/wire.h>
#else
#include <wire/wire.h>
#endif
  
static double font_width = 7.0;
static double font_height = 13;
static double font_descent = 3.0;

#ifdef NODEF
#define char_width(xlen)     ((double)((font_width * (xlen))))
#define line_height(ylen)    ((double)((font_height + font_descent) * (ylen)))
#define y_text_pos(yval)     (line_height(yval) + font_descent + 2.0)
#define y_cell_pos(yval)     (line_height(yval) + 2.0)
#define x_text_pos(xval)     (char_width(xval)  + 2.0)
#else
#define char_width(xlen)     ((double)((font_width * (xlen))))
#define line_height(ylen)    ((double)((font_height) * (ylen)))
#define y_text_pos(yval)     (line_height(yval) + font_descent + 2.0)
#define y_cell_pos(yval)     (line_height(yval) + 2.0)
#define x_text_pos(xval)     (char_width(xval)  + 2.0)
#endif

void
define_package_c()
{
  define_package();
}

void enter_package_c(ret)
int *ret;
{
  enter_package(ret);
}

void
  ps_set_tag_c(a,b)
char *a;
int b;
{
  ps_set_tag(a,b);
}

void
  ps_init_ps_c(font_name, font_size, retained, scrollbar_side,  autoscale, cols, c,r)
char *font_name;
int font_size, retained, scrollbar_side;
int autoscale, cols;
int c,r;
{
  ps_init_ps(font_name, font_size, retained,  scrollbar_side,  autoscale, cols, c,r);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

void
  ps_show_at_c(x,y,str, len, ats)
register int x,y;
register char_type *str;
register int len;
register attribute_type *ats;
{
  char c;


  if (ats == (attribute_type *) 0) { /* FAST path */
    c = str[len];
    str[len] = '\0';
    ps_show_at(x_text_pos(x),y_text_pos(y), str);
    str[len] = c;  
  } else {
    register double tx1,ty1,tx2,ty2;
    attribute_type *aptr, cur;
    char_type *cptr;
    int cnt;


    ty1 = y_cell_pos((double)y);
    ty2 = line_height(1) - 1;

    while (len > 0) {
      aptr = &ats[x];
      cur = *aptr;
      tx1 = x_text_pos((double)x);

      while (*aptr == cur && len > 0) {
	aptr++;
	len--;
      }

      cnt = aptr - &ats[x];

      x += cnt;

      cptr = str;
      str += cnt;

      tx2 = char_width(cnt);
      c = *str;
      *str = '\0';

      /** before **/
      if (cur & 0x1) { /* inverse video */
	ps_text_color(
	    (float) flags.r / 256,
	    (float) flags.g / 256,
	    (float) flags.b / 256
	);
	ps_fill_region(tx1, ty1, tx2, ty2);
	ps_back_color();
      }

      if (cur & 02) { /* bold */
	  ps_show_at(tx1 + 1,y_text_pos(y), cptr);
      }

      /** during -- show the line **/
      if (cur & 04) { /* underline, I hope */
	  ps_ul_show_at(tx1,y_text_pos(y), cptr);
      } else {
	  ps_show_at(tx1,y_text_pos(y), cptr);
      }

      /** after **/
      if (cur & 0x1) {
	ps_text_color(
	    (float) flags.r / 256,
	    (float) flags.g / 256,
	    (float) flags.b / 256
	);
      }
      *str = c;
    }
  }

#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

void
  ps_reset_canvas_c()
{
  ps_reset_canvas();
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

void
  ps_clear_region_c(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
  register double tx1,ty1,tx2,ty2;
  
  tx1 = x_text_pos((double)x1);
  ty1 = y_cell_pos((double)y1);
  tx2 = char_width(x2-x1+1);
  ty2 = line_height(y2-y1+1);
  
  ps_clear_region(tx1,ty1, tx2, ty2);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

/* 
 *  y1 = start-line
 *  y2 = end line
 *  n  = how far should we scroll it?
 */
ps_scroll_region_c(y_1,y_2,n)
     int y_1,y_2,n;
{
  double x1,y1,x2,y2,x3,y3;
  int cnt;
  
  x1 = x_text_pos(0);
  x2 = char_width(x_size);
  
  if ((y_2 - y_1 < n) || (y_2 - y_1 < -n)) {
    y1 = y_cell_pos(y_1);
    y2 = line_height(y_2 - y_1 + 1);
    ps_clear_region(x1,y1,x2,y2);
  } else {
    x3 = 0;
    y3 = line_height(n);
    
    if (n>0) {
      y1 = y_cell_pos(y_1);
      if (scrolling_method == 0 && scrolling == FALSE) {
	y2 = line_height(y_2 - y_1 + 1) - 1.0;
	for (cnt = 0; cnt < y3; cnt++) {
	  ps_copyarea(x1,(double) y1+cnt,x2,(double) y2-cnt,x3, 1.0);
	  ps_clear_region(x1, y1+cnt, x2, 1.0);
	}
      } else {
	y2 = line_height(y_2 - y_1 + 1 - n);
	ps_copyarea(x1,y1,x2,y2,x3,y3);
	ps_clear_region(x1, y1, x2, y3 - 1.0);
      }
    } else {
      y1 = y_cell_pos(y_1 - n);
      if (scrolling_method == 0 && scrolling == FALSE) {
	y2 = line_height(y_2 - y_1);
	for (cnt = 0; cnt > y3; cnt--) {
	  ps_copyarea(x1,(double) y1+cnt,x2,(double) y2,x3, -1.0);
	  ps_clear_region(x1, y1+cnt+y2, x2, 1.0); 
	}
      } else {
	y2 = line_height(y_2 - y_1 + 1 + n);
	ps_copyarea(x1,y1,x2,y2,x3,y3);
	ps_clear_region(x1, y1 + y2 + y3, x2, - y3);
      }
    }
    ps_pause();
  }
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_clear_screen_c()
{
  ps_clear_screen();
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_invert_box_c(x1,y1,x2,y2)
     int x1,y1,x2,y2;
{
  register double tx1,ty1,tx2,ty2;
  
  tx1 = x_text_pos((double)x1);
  ty1 = y_cell_pos((double)y1);
  tx2 = char_width(x2-x1+1);
  ty2 = line_height(y2-y1+1);
  ps_invert_box(tx1,ty1,tx2,ty2);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

void
  ps_insert_char_c(x,y,n, len)
int x,y,n, len;
{
  double x1,y1,x2,y2,x3,y3;
  
  x1 = x_text_pos(x);
  y1 = y_cell_pos(y);
  x2 = char_width(len-x);
  y2 = line_height(1);
  
  x3 = x_text_pos(x+n) - x1;
  y3 = 0;
  
  ps_copyarea(x1,y1,x2,y2,x3,y3);
  ps_clear_region(x1, y1, x3-1.0, y2);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

void
  ps_delete_char_c(x, y, n, len)
int x, y, n, len;
{
  double x1,y1,x2,y2,x3,y3;
  
  x1 = x_text_pos((double)(x+n));
  y1 = y_cell_pos((double)y);
  x2 = char_width(len-x-n);
  y2 = line_height(1);
  
  x3 = x_text_pos(x) - x1;
  y3 = 0;
  
  ps_copyarea(x1,y1,x2,y2,x3,y3);
  
  x1 = x_text_pos((double)(len-n));
  x2 = char_width(n);
  
  ps_clear_region(x1, y1, x2, y2);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_bell_c()
{
  ps_bell();
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_set_selection_c(str)
     char *str;
{
  ps_set_selection(str);
  
#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

int
  to_char_x(x)
register double x;
{
  x =  (x - 2.0) / font_width;
  return x;
}

int
  to_char_y(y)
register double y;
{
  y = (y - 2.0) / (font_height);
  return y;
}

void
  text_size_handler()
{
  font_width = wire_ReadFloat();
  font_height = wire_ReadFloat();
  font_descent = wire_ReadFloat();
}

ps_set_size_c(x,y)
     int x,y;
{
  double dx, dy;
  
  if (x == x_size && y == y_size)
    return;
  
  dy = (y * (font_height)) + 3.0; 
  dx = (x * font_width) + 3.0;
  ps_set_size(dx,dy);
#ifdef DEBUG
  ps_flush_PostScript();
#endif
  x_size = x;
  y_size = y;
  config_size();
}

ps_flush_PostScript_c() 
{
  ps_flush_PostScript();
}

ps_scroll_bottom_c()
{
  ps_scroll_bottom();

#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_set_view_c(x,y)
int x,y;
{
  ps_set_view(x,y);

#ifdef DEBUG
  ps_flush_PostScript();
#endif
}

ps_set_defaults_c(font_name, font_size, retained, scrollmode, scrollbar_side, term_id, autoscale, cols, saved_lines)
char *font_name, *term_id;
int font_size, retained, scrollmode, scrollbar_side;
int autoscale, cols, saved_lines;
{
  ps_set_defaults(font_name, font_size, retained, scrollmode, scrollbar_side, term_id, autoscale, cols, saved_lines);
}

ps_apply_c()
{
  ps_apply();
}

ps_cancel_selection_c()
{
  ps_cancel_selection();
}

ps_set_pins_c(pin_x, pin_y, last_x, last_y)
int pin_x, pin_y, last_x, last_y;
{
  ps_set_pins(pin_x, pin_y, last_x, last_y);  
}

ps_set_label_c(str)
char *str;
{
  ps_set_label(str);
}

ps_set_icon_label_c(str)
char *str;
{
  ps_set_icon_label(str);
}

ps_draw_cursor_c(x,y)
int x,y;
{
  ps_draw_cursor(x,y);
}

ps_erase_cursor_c()
{
  ps_erase_cursor();
}

ps_put_char_c(c)
char c;
{
  ps_put_char(c);
}

ps_new_clip_c()
{
  ps_new_clip();
}

ps_set_clip_c()
{
  ps_set_clip();
}

ps_namedictbegin_c(str)
char *str;
{
  ps_namedictbegin(str);
}

ps_dictenddef_c()
{
  ps_dictenddef();
}

ps_namebooleandef_c(name, value)
     char *name;
     int value;
{
  ps_namebooleandef(name, value);
}

ps_nameintdef_c(name, value)
     char *name;
     int value;
{
  ps_nameintdef(name,  value);
}

ps_namerealdef_c(name, value)
     char *name;
     float value;
{
  ps_namerealdef(name, value);
}

ps_namestringdef_c(name, value)
     char *name, *value;
{
  ps_namestringdef(name, value);
}

ps_namenamedef_c(name, value)
     char *name;
     char *value;
{
  ps_namenamedef(name, value);
}

ps_ready_c()
{
  ps_ready();
}

tnt_reshape_win_c(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
  tnt_reshape_win_ps(x1,y1,x2,y2);
}

disable_cursor()
{
  ps_disable_cursor();
  repair_cursor();
}

enable_cursor()
{
  ps_enable_cursor();
}

ps_close_connection_c()
{
  ps_close_connection();
}

ps_damage_start_c(tag, fx1, fy1, fx2, fy2)
    int tag; float *fx1; float *fy1; float *fx2; float *fy2;
{
  ps_damage_start(tag, fx1, fy1, fx2, fy2);
}

ps_damage_end_c()
{
  ps_damage_end();
}
