#ident "@(#)select.c	1.19 91/09/14"

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
 */

#include "term.h"
#include <sys/time.h>
#ifdef OW_V2
#include <NeWS/wire.h>
#else
#include <wire/wire.h>
#endif
#include <malloc.h>
#include <ctype.h>

#define BELOW(x1,y1,x2,y2) (((y1) < (y2))||(((y1) == (y2)) && ((x1) > (x2))))

/* 
 * We do selections here!
 */

int selection = 0;
static int pin_x, pin_y, last_x, last_y;

static int cur_level = 1;

static void
invert_between(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
  if (y2 > y1) {
    while (y1 != y2) {
      ps_invert_box_c(0,y1, x1,y1);
      y1++;
      x1 = screen[y1]->len;
    }
  } else {
    while (y1 != y2) {
      ps_invert_box_c(x1,y1, screen[y1]->len,y1);
      y1--;
      x1 = 0;
    }
  }
  if (x2 > x1)
    ps_invert_box_c(x1,y1,x2,y2); 
  else
    ps_invert_box_c(x2,y1,x1,y2); 
}

void 
invert_on()
{
  if (selection) {
    invert_between(pin_x,pin_y,last_x,last_y);
  }
}

void
invert_off()
{
  if (selection != 0) {
    invert_on();
    selection = 0;
  }
}

static void
invert_to(x2,y2)
int x2,y2;
{
  int f1,f2,f3;

  if (x2 > screen[y2]->len)
    x2 = screen[y2]->len;

  if (x2 == last_x && y2 == last_y)
    return;

  f1 = BELOW(pin_x,pin_y, last_x, last_y); /* previously extending up */
  f2 = BELOW(x2,y2, pin_x, pin_y);         /* would be extending down */
  f3 = BELOW(x2,y2, last_x, last_y);         /* down below previous? */

  if (f1 == f2) {
    invert_off();
    invert_between(pin_x,pin_y,x2,y2);
  } else { /* Now assume f1 != f2 */
    if (f2) {
      if (f3) { /* Going down and continuing to go down */
	invert_between(last_x+1, last_y, x2, y2);
      } else {  /* Was going down but turned around */
	if (x2 == screen[y2]->len && (y2 != last_y))
	  invert_between(last_x, last_y, 0, y2-1);
	else
	  invert_between(last_x, last_y, x2+1, y2);
      }
    } else {
      if (f3) { /* Going up previously but turned around */
	if (x2 == 0) 
	  invert_between(last_x, last_y, screen[y2+1]->len, y2+1);
	else
	  invert_between(last_x, last_y, x2-1, y2);
      } else {  /* Going up and continuing to go up */
	invert_between(last_x-1, last_y, x2, y2);
      }
    }
  }

  last_x = x2;
  last_y = y2;
  selection = 1;
}

void
selection_start_handler()
{
  int x,y;

  if (selection) {
    printf("Call to NewSelection while a previous selection was still active!\n");
    invert_on(); /* Shhesh?! how did we get here? */
  }

  x = wire_ReadInt();
  y = wire_ReadInt();

  if (y > y_size -1)
    y = y_size - 1;
  if (y < 0)
    y = 0;

  if (x > screen[y]->len )
    x = screen[y]->len;
  if (x < 0)
    x = 0;

  selection = 1;

  ps_invert_box_c(x,y,x,y);

  last_x = pin_x = x;
  last_y = pin_y = y;
}

static char syntax_table[256];

void
init_word_syntax()
{
  int i;

  for (i=0; i<256; i++) 
    syntax_table[i] = (isalnum(i)?1:0) | (ispunct(i)?2:0);

  syntax_table['_'] = syntax_table['a'];
}

static void
level_up(cx, cy)
register int *cx, *cy;
{
  int cur;

  switch (cur_level) {
  case 1: /* Character */
    break;
  case 2: /* Word */
    cur = syntax_table[screen[*cy]->line[*cx]];
    while(*cx != 0 && syntax_table[screen[*cy]->line[(*cx)-1]] == cur)
      (*cx)--;
    break;
  case 3: /* Line */
    while (*cy < y_size - 1 && screen[(*cy) + 1]->wrapped == TRUE)
      (*cy)++;
    *cx = 0;
    break;
  case 4: /* Page */
    *cx = 0;
    *cy = y_size - 1;
    break;
  }
}

static void
level_down(cx, cy)
register int *cx, *cy;
{
  int cur;

  switch (cur_level) {
  case 1: /* Character */
    break;
  case 2: /* Word */
    cur = syntax_table[screen[*cy]->line[*cx]];
    while(syntax_table[screen[*cy]->line[(*cx) + 1]] == cur && screen[*cy]->len > *cx)
      (*cx)++;
    break;
  case 3: /* Line */
    *cx = screen[*cy]->len;
    while (*cy > 0 && screen[*cy]->wrapped == TRUE)
      (*cy)--;
    break;
  case 4: /* Page */
    *cx = screen[0]->len;
    *cy = 0;
    break;
  }
}


void
selection_motion_handler()
{
  int x1,y1, x2, y2, level;

  x1 = wire_ReadInt();
  y1 = wire_ReadInt();

  x2 = wire_ReadInt();
  y2 = wire_ReadInt();

  level = wire_ReadInt();
  if (level > 4)
    level = 4;

  if (x1 < 0) x1 = 0;
  if (x2 < 0) x2 = 0;
  if (y1 < 0) y1 = 0;
  if (y2 < 0) y2 = 0;

  if (y1 >= y_size) y1 = y_size - 1;
  if (y2 >= y_size) y2 = y_size - 1;

  if (x1 >= screen[y1]->len) x1 = screen[y1]->len;
  if (x2 >= screen[y2]->len) x2 = screen[y2]->len;

  cur_level = level;

  if BELOW(x1, y1, x2, y2) {
    level_up(&x2, &y2);
    level_down(&x1, &y1);
  } else {
    level_up(&x1, &y1);
    level_down(&x2, &y2);
  }


  if (x2 != pin_x || y2 != pin_y) {
    if (x2 == last_x && y2 == last_y) {
      int t;

      t = last_x; last_x = pin_x; pin_x = t;
      t = last_y; last_y = pin_y; pin_y = t;
    } else {
      invert_off();

      pin_x = x2;
      pin_y = y2;

      last_x = x1;
      last_y = y1;
      selection = 1;
      invert_on();
      return;
    }
  } else
    if (last_x == x1 && last_y == y1)/* havn't moved much */
      return;

  invert_to(x1,y1);
}

static char * extract_selection()
{
  char *p;
  int x,y;
  char *buff;

  buff = malloc(65536); /* This should be done better! */

  p = buff;
  x = pin_x;
  y = pin_y;

  if (x > screen[y]->len)
     x = screen[y]->len;
  
  while (y != last_y) {
    memmove(p, &screen[y]->line[x],  screen[y]->len - x);
    p += screen[y]->len - x;
    if (screen[y]->wrapped == FALSE)
      *p++ = '\n';
    y--;
    x = 0;
  }

  memmove(p, &screen[y]->line[x],  last_x - x + 1);
  p += last_x - x + 1;

  if (last_x == screen[y]->len && screen[y]->wrapped == FALSE)
    *p++ = '\n';

  *p++ = '\0';
  return(buff);
}

void
selection_stop_handler()
{
  int x,y;
  char *buff;

  if BELOW(pin_x, pin_y, last_x, last_y) {
    x = pin_x; pin_x = last_x; last_x = x;
    y = pin_y; pin_y = last_y; last_y = y;
  }

  ps_set_pins_c(pin_x, pin_y, last_x, last_y);

  buff = extract_selection();
  ps_set_selection_c(buff);
  free(buff);
}

static SCREENLINE **saved_screen;

void find_handler(tag, data)
int tag;
caddr_t data;
{
  int x,y;
  char *buff;
  SCREENLINE *foo;

  ps_bell_c();
  return;
#ifdef nodef
  if (selection == 0) {
    ps_bell_c();
    return;
  }

  if BELOW(pin_x, pin_y, last_x, last_y) {
    x = pin_x; pin_x = last_x; last_x = x;
    y = pin_y; pin_y = last_y; last_y = y;
  }

  buff = extract_selection();

  foo = screen[last_y];

  foo = foo->prev;
  if (foo == NULL)
    foo = tail;

  while (foo != screen[last_y]) {
    if (strstr(foo->line, buff)!= NULL) {
      SCREENLINE *foo2;
      int cnt;

      invert_off();

      make_visable(foo);
    }
    foo = foo->prev;
    if (foo == NULL)
      foo = tail;
  }

  ps_bell_c();
  free(buff);
#endif
}

#ifdef nodef
make_visable(line)
SCREENLINE *line;
{
      if (scrolling) 
	foo2 = saved_screen[0];
      else
	foo2 = screen[0];

      cnt = 0;

      while (foo2 != foo) {
	foo2 = foo2->next;
	cnt++;
      }
      printf("%s - %d\n", buff, cnt);
      return;
}
#endif

static int last_scroll;

static void
scroll_to(place)
     int place;
{
  SCREENLINE     *foo;
  int i, cnt, diff, t, b;

  diff = y_size;

  cnt = last_scroll - place;

  if (cnt == 0)
    return;

  if (cnt > y_size || cnt < -y_size) {
    ps_clear_screen_c();
    
    assert(place <= link_count - y_size);
    
    foo = saved_screen[0];
    for (i=0;i < place && foo->next; i++)  
      foo = foo->next;
    
    for (i = 0; i < y_size - 1; i++) {
      screen[i] = foo;
      ps_show_at_c(0, i, foo->line, foo->len, foo->ats);
      foo = foo->next;
    }
  } else {
    ps_scroll_region_c(0, y_size - 1, cnt);
    if (cnt < 0) { /* Scrolling down! */
      memmove(&screen[0], &screen[0 - cnt], 
	    sizeof(SCREENLINE *) * (diff + cnt));
      t = y_size - 1;
      b = t + cnt + 1;
      foo = screen[t];
      while (cnt++ < 0 && foo->next)
	foo = foo->next;
    } else {
      memmove(&screen[0 + cnt], &screen[0],
	    sizeof(SCREENLINE *) * (diff - cnt));
      b = 0;
      t = cnt - 1;
      foo = screen[0]->prev;
    }
    for (; t >= b; t--) {
      screen[t] = foo;
      ps_show_at_c(0, t, foo->line, foo->len, foo->ats);
      foo = foo->prev;
    }
  }
  last_scroll = place;
}

/* 
 * This actually is the scrolling handler.  This handles scrolling update requests
 * from the scroll bar 
 */

void
update_handler()
{
  int f;

  if (selection != 0) {
    invert_off();
    ps_cancel_selection_c();
  }

  if (scrolling == FALSE) 
    disable_cursor();
  
  f = wire_ReadInt();

  if (f == 0)
    finish_scrolling();
  else {
    if (!scrolling) {
      saved_screen = screen;
      screen = (SCREENLINE **) malloc(sizeof(SCREENLINE *) * y_size);
      memmove(screen, saved_screen,  sizeof(SCREENLINE *) * y_size);
      last_scroll = 0;
    }
    scrolling = TRUE;
    scroll_to(f);
    wire_RemoveFileHandler(master_fp);
  }
  if (scrolling == FALSE) {
    enable_cursor();
  }
}


void
quick_finish()
{
  scrolling = FALSE;
  wire_AddFileHandler(master_fp, input_handler, 0);
  
  free(screen);
  screen = saved_screen;
  saved_screen = 0;
}

finish_scrolling()
{
  if (scrolling) {
    scroll_to(0);
    quick_finish();
    enable_cursor();
    ps_cancel_selection_c();
    selection = FALSE;
  }
}

void
erase_cursor()
{
  ps_erase_cursor_c();
}

void
draw_cursor()
{
  if (x_loc >= x_size)
    ps_draw_cursor_c(x_size-1, y_loc);
  else
    ps_draw_cursor_c(x_loc, y_loc);
}

void
repair_cursor()
{
  int x;

  if (x_loc >= x_size) 
    x = x_size - 1;
  else
    x = x_loc;

  ps_clear_region_c(x,y_loc, x, y_loc);
  ps_show_at_c(x, y_loc, &screen[y_loc]->line[x], 1, screen[y_loc]->ats);
  ps_draw_cursor_c(x,y_loc);
}
