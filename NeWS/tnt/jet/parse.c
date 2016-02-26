#ident "@(#)parse.c 1.26 91/09/14"

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
 *   Josh Siegel (siegel@sun.com)
 */
  
#include <sys/ioctl.h>  
#include <sys/filio.h>  
#include <stdio.h>
#include <malloc.h>
#include "actions.h"
#include "term.h"
#include <sys/time.h>
#ifdef OW_V2
#include <NeWS/wire.h>
#else
#include <wire/wire.h>
#endif
#include "tnt_cps.h"

#ifdef DEBUG
static void super_check();
#endif

extern char states[][256];
extern char actions[][256];
  
struct term_flags flags = {
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE
    /* Liam: default text color */ ,0, 0, 0 /* /Liam */
};

int		bottom_margin, top_margin, diff_margin;

int		x_loc, y_loc;	 /* Cursor location */
int		x_size, y_size;	   /* Size of screen */
int             scrolling_method = 3;

static int	state = 0;    /* Parser state */
static int	scroll_cnt, x, y;

static char ident_str[128] = {"NeWS ANSI Terminal"};

SCREENLINE     *cur;

static unsigned char     attributes = 0;

/*
 * One of the speed-features is that a attempt is made to do screen-scrolls
 * in big jumps instead of one line at a time.	Also, raw string output is
 * buffered into hopefully a single write() so that it can all be sent across
 * in one packet.  (Useful for thin-wire)
 *
 * Compression gets turned on when the cursor gets to the far left hand side and
 * is turned off upon edits.
 */

static int	compressed;

/* Are we currently scrolling? */

int		scrolling = FALSE;

SCREENLINE     *tail;
SCREENLINE    **screen;
static char    *tabs;
static int	tabs_size;
int		link_count = 0;	   /* number of lines total saved */
static int	last_count = -1; /* What was it last time we checked? */

/* How many lines should we save? */
int saved_lines = 128;

/* This will be inlined by the current Sun compiler! */

void
set_saved_lines(val)
int val;
{
  SCREENLINE	   *foo;

  if (val < 128)
    val = 128;
  
  saved_lines = val;

  while (link_count > val ) {
    foo = tail;
    tail = tail->prev;

    if (foo->ats)
      free(foo->ats);
    free(foo->line);
    free(foo);
    link_count--;
  }

  tail->next = (SCREENLINE *) 0;
}

static void attribute_ats(line, x)
SCREENLINE *line;
int x;
{

  if (line->ats == 0){
    line->ats = (unsigned char *) malloc((x_size + 1) * sizeof (attribute_type));
    bzero((char *)line->ats, (x_size + 1) * sizeof (attribute_type));
  }
  line->ats[x] = attributes;
}

/*
 * Notice that "next" and "prev" are not set here since they are more
 * specific to the actual place allocate_node() is used.
 */

static SCREENLINE *
  allocate_node()
{
  register SCREENLINE *foo;
  
  if (link_count < saved_lines) {
    foo = (SCREENLINE *) malloc (sizeof (SCREENLINE));
    foo->line = (char_type *) malloc ((x_size + 1) * sizeof (char_type));
    foo->rsize = x_size + 1;
    link_count++;
  } else {
    foo = tail;
    tail = tail->prev;
    tail->next = 0;
    if (foo->ats)
      free(foo->ats);
    if (foo->rsize < x_size + 1) {
      free(foo->line);
      foo->line = (char_type *) malloc ((x_size + 1) * sizeof (char_type));
      foo->rsize = x_size + 1;
    }
  }

  foo->len = 0;
  foo->trimmed = FALSE;
  foo->wrapped = FALSE;
  foo->dirty = FALSE;

  foo->ats = (attribute_type *) 0;

  memset (foo->line, ' ', foo->rsize * sizeof (char_type));
  return (foo);
}

/*
 * Allocate new lines between two points on the screen.
 *
 * The argument is a pointer to the place above the top (when the top is the top
 * of the screen, you can't figure it out yourself).
 */
static void
  allocate_between(b, t, next)
int		    b, t;
SCREENLINE	   *next;
{
  int		    j;
  SCREENLINE	   *prev;
  
  if (b == 0)
    prev = (SCREENLINE *) 0;
  else
    prev = screen[b - 1];
  
  for (j = b; j <= t; j++)
    screen[j] = allocate_node();
  
  for (j = b + 1; j <= t - 1; j++) {
    screen[j]->prev = screen[j - 1];
    screen[j]->next = screen[j + 1];
  }
  
  screen[b]->prev = prev;
  screen[t]->next = next;
  
  if (b != t) {
    screen[b]->next = screen[b + 1];
    screen[t]->prev = screen[t - 1];
  }
  if (prev)
    prev->next = screen[b];
  if (next)
    next->prev = screen[t];
}

init_term(rows, cols)
int rows, cols;
{
  int		    j;
  
  x_loc = 0;
  y_loc = rows - 1;

  x_size = cols;
  y_size = rows;

  bottom_margin = 0;
  top_margin = y_size - 1;
  diff_margin = top_margin - bottom_margin + 1;
  
  tabs = (char *) malloc(x_size + 1);
  bzero((char *) tabs, x_size);
  
  for (j = 0; j < x_size; j += 8)
    tabs[j] = '\t';
  
  tabs_size = x_size;
  
  screen = (SCREENLINE **) malloc (sizeof (SCREENLINE *) * y_size);
  
  allocate_between(0, y_size - 1, (SCREENLINE *) 0);
  tail = screen[y_size - 1];
#ifdef DEBUG
  super_check();
#endif
}

#ifdef DEBUG
/*
 * Screen data structure fsck
 *
 * This is useful to spread about when you arn't quite sure that the state of
 * the screen is still correct.  Its used primarily during bug hunts.
 */
static void
  super_check()
{
  int		    i, cnt;
  SCREENLINE	   *foo;

  if (scrolling) /* These become invalid during scrolling... */
    return;

  assert(screen[0]->prev == 0);
  assert(screen[0]->next == screen[1]);
  assert(screen[y_size - 1]->prev == screen[y_size - 2]);
  assert(bottom_margin >= 0);
  assert(top_margin > bottom_margin);
  assert(top_margin < y_size);
  
  assert(tail->next == (SCREENLINE *) 0);
  
  if (link_count > y_size)
    assert(screen[y_size - 1]->next != (SCREENLINE *) 0);
  
  foo = screen[0]->next;
  cnt = 2;
  
  for (i = 1; i < y_size - 2; i++) {
    assert(foo == screen[i]);
    assert(screen[i]->next == screen[i + 1]);
    assert(screen[i]->prev == screen[i - 1]);
    assert(screen[i]->trimmed == FALSE);
    foo = foo->next;
    cnt++;
  }
  
  while (foo->next) {
    assert(foo->next->prev == foo);
    foo = foo->next;
    cnt++;
  }
  assert(foo == tail);
  assert(tail->next == (SCREENLINE *) 0);
  assert(cnt == link_count);

  assert (y_loc < y_size);
}
#endif

/*
 * We run down the saved lines shrinking them to be as small as possible.
 */

static void
  cleaner()
{
  SCREENLINE	   *foo;
  
  foo = screen[y_size - 1];
  foo = foo->next;
  
  while (foo && foo->trimmed == FALSE) {
    foo->trimmed = TRUE;
    foo->line = (char_type *) realloc(foo->line, (foo->len + 1) 
				      * sizeof (char_type));
    if (foo->ats) {
      foo->ats = (attribute_type *) realloc(foo->ats, (foo->len + 1) 
					   * sizeof (attribute_type));
    }
    foo->rsize = foo->len + 1;
    foo = foo->next;
  }
}

/*
 * Show one line
 */

static void
  small_show()
{
  assert(compressed == FALSE);
  
  if (cur->dirty == FALSE) {
    x_loc = x;
    return;
  }
  ps_clear_region_c(x_loc, y_loc, x - 1, y_loc);
  ps_show_at_c(x_loc, y_loc, &cur->line[x_loc], x - x_loc, cur->ats);
  
  x_loc = x;
  cur->dirty = FALSE;
}

/*
 * Update the actual screen to the current saved screen state
 */

static void
  do_show()
{
  int		    i, j, all_clear;

  if (compressed) {
    all_clear = FALSE;
    if (scroll_cnt > 0) {
      if (scroll_cnt < diff_margin - 1)
	ps_scroll_region_c(bottom_margin, top_margin, scroll_cnt);
      else {
	ps_clear_region_c(0, bottom_margin, x_size, top_margin);
	if (y >= bottom_margin && y <= top_margin &&
	    y_loc >= bottom_margin && y_loc <= top_margin)
	  all_clear = TRUE;
      }
      scroll_cnt = 0;
    } else if ((y == y_loc) && (screen[y]->dirty == FALSE)) {
      x_loc = x;
      return;
    }
    if (all_clear == FALSE) {
      j = -1;
      for (i = y; i <= y_loc; i++) {
	if (screen[i]->dirty == FALSE) {
	  if (j != -1) {
	    ps_clear_region_c(0, j, x_size, i - 1);
	    j = -1;
	  }
	} else {
	  if (j == -1)
	    j = i;
	}
      }
      
      if (j != -1)
	ps_clear_region_c(0, j, x_size, i - 1);
    }
    for (i = y_loc; i >= y; i--) 
      {
	if (screen[i]->dirty) {
	  ps_show_at_c(0, i, screen[i]->line, screen[i]->len, screen[i]->ats);
	  screen[i]->dirty = FALSE;
	}
      }
    
    x_loc = x;
    y_loc = y;
    if (x != 0 || scrolling_method <= 1)
      compressed = FALSE;
    else
      compressed = TRUE;
  } else
    small_show();
}


/*
 * This cuts the requested range out of the screen and rewires it into the
 * saved scrolling buffer off the top of the screen.  It is used most often
 * when a scrolling region is in effect.
 */

static void
  preserve_range(b, t)
int		    b, t;
{
  if (t == y_size - 1) {
    /*
     * Do what it takes to preverse the lines scrolling off the
     * absolute top
     */
    return;
  } else {
    /* Cut out scrolled range */
    if (b > 0)
      screen[b - 1]->next = screen[t]->next;
    
    screen[t + 1]->prev = screen[b]->prev;
    
    /* Wire scrolling range into new spot */
    
    if (screen[y_size - 1]->next)
      screen[y_size - 1]->next->prev = screen[t];
    
    screen[t]->next = screen[y_size - 1]->next;
    screen[b]->prev = screen[y_size - 1];
    screen[y_size - 1]->next = screen[b];
  }
}

/*
 * All this does is free all the allocated data... it does NOT rewire any
 * pointers.
 */

static void
  free_range(b, t)
int		    b, t;
{
  for (; b <= t; b++) {
    free(screen[b]->line);
    free(screen[b]);
    link_count--;
  }
}


/*
 * Internal scroller.
 * 
 * This scrolls the internal representation of the screen between "top" and
 * "bottom" for cnt (where a cnt<0 is a reverse scroll).
 */

static void
  int_scroll(bottom, top, cnt, move_cursor)
int		    top, bottom, cnt, move_cursor;
{
  int		    t, b;
  int		    diff;
  SCREENLINE	   *next;
  
  diff = top - bottom + 1;
  
  if (cnt >= diff || cnt <= -diff) {
    b = bottom;
    t = top;
    if (move_cursor) {
      if (cnt > 0)
	y_loc = top;
      else
	y_loc = bottom;
    }
    next = screen[t]->next;
    
    if (cnt > 0)
      preserve_range(b, t);
    else
      free_range(b, t);
    
    allocate_between(b, t, next);
  } else {
    if (cnt > 0) {
#ifdef DEBUG
  super_check();
#endif
      preserve_range(top - cnt + 1, top);
      memmove(&screen[bottom + cnt], &screen[bottom],
	    sizeof (SCREENLINE *) * (diff - cnt));
      b = bottom;
      t = bottom + cnt - 1;
      
      allocate_between(b, t, screen[bottom + cnt]);
#ifdef DEBUG
  super_check();
#endif
    } else {
      next = screen[top]->next;
      free_range(bottom, bottom - cnt - 1);
      memmove(&screen[bottom], &screen[bottom - cnt],
	    sizeof(SCREENLINE *) * (diff + cnt));
      
      if (bottom == 0)
	screen[0]->prev = 0;
      else {
	screen[bottom - 1]->next = screen[bottom];
	screen[bottom]->prev = screen[bottom - 1];
      }
      
      b = top + cnt + 1;
      t = top;
      
      allocate_between(b, t, next);
    }
    if (move_cursor) {
      y_loc += cnt;
      y += cnt;
      scroll_cnt += cnt;
    }
  }
}


static void
  do_cr()
{
  if (compressed == FALSE) {
    if (cur->dirty)
      small_show();
    x_loc = x = 0;
    
    if (scrolling_method > 1)
      compressed = TRUE;
  } else {
    x = 0;
  }
}

static void
  do_nl()
{
#ifdef DEBUG
  super_check();
#endif

  if (y_loc >= top_margin && (scrolling_method < 3 || compressed == FALSE))
    do_show();
  
  if (y < bottom_margin && y == 0)
    return;	       /* No scrolls when outside the scrolling
			* region */
  
  if (compressed == FALSE) {
    small_show();
    
    if (y == bottom_margin) {
      ps_scroll_region_c(bottom_margin, top_margin, 1);
      int_scroll(bottom_margin, top_margin, 1, TRUE);
      scroll_cnt = 0;
    }
    y = --y_loc;
  } else {
    if (y == bottom_margin)
      int_scroll(bottom_margin, top_margin, 1, TRUE);

    if (scrolling_method == 3) {
      if (y_loc > top_margin)
	y_loc = top_margin;
    }
    assert (y_loc < y_size);
    
    y--;
  }
  
  cur = screen[y];
  assert(scrolling || (cur->trimmed == FALSE));
#ifdef DEBUG
  super_check();
#endif
}


/*
 * This is the actual input handler!
 */

static char window_label[256];
static int label_cnt = 0;
static int label_setting = 0; /* 0 - window label, 1 - icon label */

void
  input_handler()
{
  int		    cnt, chars_read;
  static int	    saved_x = 1;
  static int	    saved_y = 1;
  static int	    args[9], c_arg;
  unsigned char	    buff[8192], *p2;
  char	   *action_table;
  char_type	   *optr;

  if (selection != 0) {
    invert_off();
    ps_cancel_selection_c();
  }

  if (state == 0 && flags.insert == 0 && x_loc < x_size - 1) {
    if(ioctl(master_fd, FIONREAD, &chars_read) == 0) {
      if (chars_read == 1) {
	chars_read = read(master_fd, buff, 1);
	
	if (actions[0][buff[0]] != action_put_char) {
	  erase_cursor();
	  
	  scroll_cnt = 0;
	  x = x_loc;
	  y = y_loc;
	  
	  if (x == 0 && scrolling_method > 1) 
	    compressed = TRUE;
	  else
	    compressed = FALSE;
	  goto past_read;
	}
	ps_put_char_c(buff[0]);
	screen[y_loc]->line[x_loc++] = buff[0];
        if (x_loc > screen[y_loc]->len)
            screen[y_loc]->len = x_loc;
	ps_flush_PostScript_c();
	return;
      }
    }
  }
  
  erase_cursor();

  scroll_cnt = 0;
  x = x_loc;
  y = y_loc;
  
  if (x == 0 && scrolling_method > 1) 
    compressed = TRUE;
  else
    compressed = FALSE;

go_again:
  if (scrolling_method > 1)
    chars_read = read(master_fd, buff, sizeof(buff) - 1);
  else
    chars_read = read(master_fd, buff, 256);
past_read:
  
  if (chars_read < 0) {
    quit_handler();
  }
  
#ifdef DEBUG
  buff[chars_read] = '\0';
  super_check();
#endif

  p2 = buff - 1;
  action_table = actions[state];
  
  cur = screen[y];
  optr = &cur->line[x];
  
  assert(scrolling || (cur->trimmed == FALSE));
  
  while (chars_read--) {
    switch (action_table[*++p2]) {
    case action_nop:
      break;
    case action_state_trans:
    state_trans:
      state = states[state][*p2];
      action_table = actions[state];
      break;
    case action_enq:
      write(master_fd, ident_str, strlen(ident_str));
      break;
    case action_put_char:
      assert(cur == screen[y]);
      assert(optr == &cur->line[x]);
      assert(scrolling || (cur->trimmed == FALSE));
      
      if (flags.insert) {
	if (compressed || cur->dirty)
	  do_show();
	assert(x == x_loc);
	assert(y == y_loc);
	
	ps_insert_char_c(x_loc, y_loc, 1, cur->len);
	
	if (cur->len >= cur->rsize - 1)
	  cur->len = cur->rsize - 1;
	else
	  cur->len++;
	memmove(&cur->line[x + 1], &cur->line[x], (cur->len - x) 
	      * sizeof(char_type));
	if (cur->ats)
	  memmove(&cur->ats[x + 1], &cur->ats[x],  (cur->len - x) 
		* sizeof(attribute_type));
	cur->line[x] = *p2;
	if (attributes || cur->ats)
	  attribute_ats(cur, x);
      } else {
	if (x >= x_size) {
	  if (flags.decawm) {
	    cur->wrapped = TRUE;
	    do_cr();
	    do_nl();
	    optr = &cur->line[x];
	    goto put_char_spot;    /* YES! A goto! */
	  } else {
	    *(optr - 1) = *p2;
	    if (attributes  || cur->ats)
	      attribute_ats(cur, (optr - 1) - cur->line);
	  }
	} else {
	put_char_spot:
	  assert(cur == screen[y]);
	  assert(optr == &cur->line[x]);
	  assert(scrolling || (cur->trimmed == FALSE));

	  if (attributes  || cur->ats)
	    attribute_ats(cur, optr - cur->line);
	  
	  *optr++ = *p2;
	  if (++x > cur->len)
	    cur->len = x;
	}
      }
      cur->dirty = TRUE;
      break;
    case action_flush_display:
      c_arg = 0;
      args[0] = 0;
      do_show();
      goto state_trans;
    case action_unknown_escape:
#ifdef DEBUG
      printf("Unknown escape! (state:%d %c)\n", state, *p2);
#endif
      goto state_trans;
    case action_newline:	/* Do a newline depending on current
				 * newline mode */
#ifdef DEBUG
  super_check();
#endif

      if (flags.lnm)
	do_cr();
      do_nl();
      optr = &cur->line[x];
#ifdef DEBUG
  super_check();
#endif
      break;
    case action_return:    /* Beginning of line! */
      do_cr();
      optr = &cur->line[x];
      break;
    case action_set_arg:	/* set argument */
      args[c_arg] = (args[c_arg] * 10) + (*p2 - '0');
      break;
    case action_new_arg:	/* New argument */
      c_arg++;
      args[c_arg] = 0;
      break;
    case action_bell:    /* Ring the bell */
      ps_bell_c();
      break;
    case action_tab:    /* Move to next tab stop */
      if (x < x_size - 1) {
	x++;
	while (x < x_size && tabs[x] != '\t')
	  x++;
	optr = &cur->line[x];
	if (x > cur->len)
	    cur->len = x;
      }	 /* Should we auto-wrap here? */
      break;
    case action_error:    /* ERROR */
      printf("GAG!\n");	 /* Not used very often.. :-) */
      exit(0);
      break;
    case action_il:/* insert line */
      cnt = args[0] == 0 ? 1 : args[0];
      
      ps_scroll_region_c(bottom_margin, y_loc, -cnt);
      int_scroll(bottom_margin, y_loc, -cnt, FALSE);
      
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_dl:/* delete line */
      cnt = (args[0] == 0) ? 1 : args[0];
      
      ps_scroll_region_c(bottom_margin, y_loc, cnt);
      int_scroll(bottom_margin, y_loc, cnt, FALSE);
      
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_ic:/* insert char */
      cnt = (args[0] == 0) ? 1 : args[0];
      
      ps_insert_char_c(x_loc, y_loc, cnt, cur->len);
      
      if (cnt + cur->len > cur->rsize - 1)
	cur->len = cur->rsize - 1 - cnt;
      memmove(&cur->line[x + cnt], &cur->line[x], (cur->len - x) 
	    * sizeof(char_type));
      memset(&cur->line[x], ' ', cnt * sizeof(char_type));
      if (cur->ats) {
	memmove(&cur->ats[x + cnt], &cur->ats[x],  (cur->len - x) 
	      * sizeof(attribute_type));
	bzero((char *) (&cur->ats[x]), cnt * sizeof(attribute_type));
      }
      cur->len += cnt;
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_dc:/* delete char */
      cnt = (args[0] == 0) ? 1 : args[0];
      
      ps_delete_char_c(x_loc, y_loc, cnt, cur->len);
      
      if (x + cnt > cur->len) {
	memset(&cur->line[x], ' ', (cur->len - x) * sizeof(char_type));
	cur->len = x;
      } else {
	memmove(&cur->line[x], &cur->line[x + cnt], 
	      (cur->len - (x + cnt)) * sizeof(char_type));
	memset(&cur->line[cur->len - cnt], ' ', cnt * sizeof(char_type));
	if (cur->ats) {
	  memmove(&cur->ats[x], &cur->ats[x + cnt],  
		(cur->len - (x + cnt)) * sizeof(attribute_type));
	  bzero((char *) (&cur->ats[cur->len - cnt]), cnt * sizeof(attribute_type));
	}

	cur->len -= cnt;
      }
      
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_bs:/* back space */
      if ((x != x_loc) || (y != y_loc)) {
	do_show();
	assert((x == x_loc) && (y == y_loc));
      } else
	assert((x == x_loc) && (y == y_loc));
      
      if (x > 0) {
	x -= 1;
	x_loc = x;
	optr = &cur->line[x];
	
	if (x == 0 && scrolling_method > 1)
	  compressed = TRUE;
      }
      goto state_trans;
    case action_cub:    /* cursor backward */
      assert((x == x_loc) && (y == y_loc));
      
      cnt = (args[0] == 0) ? 1 : args[0];
      x -= cnt;
      if (x < 0)
	x = 0;
      x_loc = x;
      optr = &cur->line[x];
      
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      
      goto state_trans;
    case action_cud:    /* cursor down */
      assert((x == x_loc) && (y == y_loc));
      
      cnt = (args[0] == 0) ? 1 : args[0];
      y -= cnt;
      if (y < bottom_margin)
	y = bottom_margin;
      y_loc = y;
      
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      
      goto state_trans;
    case action_cuf:    /* cursor forward */
      assert((x == x_loc) && (y == y_loc));
      cnt = (args[0] == 0) ? 1 : args[0];
      x += cnt;
      if (x > x_size - 1)
	x = x_size - 1;
      x_loc = x;
      
      optr = &cur->line[x];
      
      goto state_trans;
    case action_cup:    /* cursor position */
      if (c_arg == 0 && args[0] == 0) {
	x = 0;
	if (flags.decom)
	  y = top_margin;
	else
	  y = y_size - 1;
      } else {
	if (args[0] == 0)
	  args[0] = 1;
	if (args[1] == 0)
	  args[1] = 1;
	
	if (flags.decom)
	  y = top_margin + 1 - args[0];
	else
	  y = y_size - args[0];
	if (c_arg == 1) {
	  x = args[1] - 1;
	} else {
	  x = 0;
	}
      }

do_move:
      if (flags.decom) {
	if (y > top_margin)
	  y = top_margin;
	if (y < bottom_margin)
	  y = bottom_margin;
      } else {
	if (y > y_size - 1)
	  y = y_size - 1;
	if (y < 0)
	  y = 0;
      }
      
      if (x > x_size - 1)
	x = x_size - 1;
      if (x < 0)
	x = 0;
      x_loc = x;
      y_loc = y;
      assert(y_loc >= 0);
      x = x_loc;
      y = y_loc;
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      
      goto state_trans;
    case action_cuu:    /* cursor up */
      cnt = (args[0] == 0) ? 1 : args[0];
      
      y += cnt;
      if (y > top_margin)
	y = top_margin;
      y_loc = y;
      
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      
      goto state_trans;
    case action_da:/* device attributes */
      write(master_fd, "\033[?1;0c", 7);	  /* But we really are a
						   * ansi term ?! */
      break;
    case action_decaln:    /* screen alignment display */
      ps_clear_screen_c();
      x_loc = 0;
      y_loc = y_size - 1;
      x = x_size - 2;    /* There arn't any rules for where we
			  * should end! */
      y = 0;
      for (cnt = 0; cnt < y_size; cnt++) {
	memset(screen[cnt]->line, 'E', screen[cnt]->rsize * sizeof(char_type));
	screen[cnt]->len = screen[cnt]->rsize - 1;
	screen[cnt]->dirty = TRUE;
      }
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      do_show();
      goto state_trans;
    case action_decdhl_top:	   /* Double height line */
      goto state_trans;
    case action_decdhl_bottom:    /* Double height line */
      goto state_trans;
    case action_decdwl:    /* double width line */
      goto state_trans;
    case action_decid:    /* identify terminal */
      write(master_fd, "\033/Z", 3);
      goto state_trans;
    case action_deckpam:	/* keypad application mode */
      goto state_trans;
    case action_deckpnm:	/* keypad numeric mode */
      goto state_trans;
    case action_decll:    /* Load LEDS */
      goto state_trans;
    case action_decrc:    /* Restore Cursor */
      do_show();
      x = x_loc = saved_x;
      y = y_loc = saved_y;
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      if (x == 0 && scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_decreqtparm:    /* Request terminal paramters */
      switch (args[0]) {
      case 0:
#define STR_1 "\033[2;1;1;112;112;1;0x"
	write(master_fd, STR_1, sizeof(STR_1));
	break;
      case 1:
#define STR_2 "\033[3;1;1;112;112;1;0x"
	write(master_fd, STR_2, sizeof(STR_2));
	break;
      }
      goto state_trans;
    case action_decsc:    /* Save cursor */
      saved_x = x;
      saved_y = y;
      goto state_trans;
    case action_decstbm:	/* Set top and bottom margins */
      if (c_arg == 1) {
        if (args[1] - args[0] >= 1) {
	  top_margin = y_size - args[0];
	  bottom_margin = y_size - args[1];
	  if (bottom_margin < 0)
	    bottom_margin = 0;
	  if (top_margin >= y_size)
	    top_margin = y_size - 1;
	  x = x_loc = 0;
	  if (flags.decom)
	    y = y_loc = top_margin;
	  else
	    y = y_loc = y_size - 1;
	  cur = screen[y];
	  optr = &cur->line[x];
	  assert(scrolling || (cur->trimmed == FALSE));
	  if (scrolling_method > 1)
	    compressed = TRUE;
	}
      } else {
	top_margin = y_size - 1;
	bottom_margin = 0;
      }
      goto state_trans;
    case action_decswl:    /* single width line */
      goto state_trans;
    case action_home: /* Go home! */
      x = x_loc = 0;
      if (flags.decom)
	y = y_loc = top_margin;
      else
	y = y_loc = y_size - 1;
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      if (scrolling_method > 1)
	compressed = TRUE;
      goto state_trans;
    case action_dsr:    /* Device status report */
      switch (args[0]) {
      case 5:
	write(master_fd, "\033[0n", 4);
	break;
      case 6:
	{
	  char	    l_buff[64];
	  
	  sprintf(l_buff, "\033[%d;%dR", y_size - y_loc, x + 1);
	  write(master_fd, l_buff, strlen(l_buff));
	}
      default:
	break;
      }
      goto state_trans;
    case action_ed:/* Erase Display J */
      switch (args[0]) {
      case 0:/* Cursor to end of screen */
	{
	  int		    i;
	  
	  for (i = bottom_margin; i < y_loc; i++) {
	    screen[i]->len = 0;
	    screen[i]->dirty = FALSE;
	    memset(screen[i]->line, ' ', screen[i]->rsize * sizeof(char_type));
	    if (screen[i]->ats) {
	      free(screen[i]->ats);
	      screen[i]->ats = (attribute_type *) 0;
	    }
	  }
	  
	  if (screen[y_loc]->len > x_loc) {
	    screen[y_loc]->len = x_loc;
	    screen[y_loc]->dirty = FALSE;
	    memset(screen[y_loc]->line + x_loc, ' ', 
		   (screen[y_loc]->rsize - x_loc) * sizeof(char_type));
	    if(screen[y_loc]->ats) 
	      bzero((char *) (screen[y_loc]->ats + x_loc), 
		    (screen[y_loc]->rsize - x_loc) * sizeof(attribute_type));
	  }
	  if (y_loc > bottom_margin)
	    ps_clear_region_c(0, bottom_margin, x_size, y_loc - 1);
	  ps_clear_region_c(x_loc, y_loc, x_size, y_loc);
	}
	break;
      case 1:/* Cursor to beginning of screen */
	{
	  int		    i;
	  
	  for (i = top_margin; i > y_loc; i--) {
	    screen[i]->len = 0;
	    screen[i]->dirty = FALSE;
	    memset(screen[i]->line, ' ', screen[i]->rsize * sizeof(char_type));
	    if (screen[i]->ats) {
	      free(screen[i]->ats);
	      screen[i]->ats = (attribute_type *) 0;
	    }
	  }
	  
	  memset(screen[y_loc]->line, ' ', (x_loc + 1) * sizeof(char_type));
	  if (screen[y_loc]->ats) 
	    bzero((char *) (screen[y_loc]->ats), (x_loc + 1) * sizeof(attribute_type));
	  
	  if (y_loc < top_margin)
	    ps_clear_region_c(0, y_loc + 1, x_size, top_margin);
	  ps_clear_region_c(0, y_loc, x_loc, y_loc);
	}
	break;
      case 2:/* Entire screen */
	{
	  ps_scroll_region_c(bottom_margin, top_margin, top_margin - bottom_margin + 1);

	  int_scroll(bottom_margin, top_margin, top_margin - bottom_margin, FALSE);
	  scroll_cnt = 0;
	  cur = screen[y];
	  optr = &cur->line[x];
	}
	break;
      }
      assert(scrolling || (cur->trimmed == FALSE));
      goto state_trans;
    case action_el:/* Erase line  K */
      switch (args[0]) {
      case 0:/* End of line */
	if (cur->len > x_loc) {
	  ps_clear_region_c(x_loc, y_loc, cur->len, y_loc);
	  cur->len = x_loc;
	  memset(&cur->line[x_loc], ' ',
		 (cur->rsize - x_loc) * sizeof(char_type));
	  if (cur->ats)
	    bzero((char *) (&cur->ats[x_loc]), 
		  (cur->rsize - x_loc) * sizeof(attribute_type));
	}
	break;
      case 1:/* Beginning of line */
	ps_clear_region_c(0, y_loc, x_loc, y_loc);
	memset(cur->line, ' ', (x_loc + 1) * sizeof(char_type));
	if (cur->ats)
	  bzero((char *) cur->ats, (x_loc + 1) * sizeof (attribute_type));
	break;
      case 2:/* Entire line */
	ps_clear_region_c(0, y_loc, cur->len, y_loc);
	cur->len = 0;
	memset(cur->line, ' ', cur->rsize * sizeof(char_type));
	if (cur->ats) {
	  free(cur->ats);
	  cur->ats = (attribute_type *) 0;
	}
	break;
      }
      assert(scrolling || (cur->trimmed == FALSE));
      goto state_trans;
    case action_hts:    /* Horizontal tab set */
      tabs[x] = '\t';
      goto state_trans;
    case action_ind:    /* index (move down one line with
			 * scroll) */
      do_nl();
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      goto state_trans;
    case action_nel:    /* new line */
      do_cr();
      do_nl();
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      goto state_trans;
    case action_ri:/* Reverse index */
      assert((y == y_loc) && (x == x_loc));
      if (y_loc == top_margin) {
	ps_scroll_region_c(bottom_margin, top_margin, -1);
	int_scroll(bottom_margin, top_margin, -1, FALSE);
      } else {
	y_loc++;
	y = y_loc;
      }
      cur = screen[y];
      optr = &cur->line[x];
      assert(scrolling || (cur->trimmed == FALSE));
      goto state_trans;
    case action_ris:    /* Reset terminal */
      goto state_trans;
    case action_rm:/* Reset Mode */
      for (cnt = 0; cnt <= c_arg; cnt++) {
	switch (args[cnt]) {
	case DECCOLM:
	  if (flags.deccolm != FALSE) {
	    flags.deccolm = FALSE;
	    ps_set_size_c(80, 24);
	    optr = &cur->line[x];
	  }
	  break;
	case VT52:
	  vt52_mode();
	  break;
	case INSERT:
	  if (state != 3)
	    flags.insert = FALSE;
	  break;
	case DECSCNM:
	  flags.decscnm = FALSE;
	  break;
	case DECOM:
	  flags.decom = FALSE;
	  break;
	case DECAWM:
	  flags.decawm = FALSE;
	  break;
	case DECARM:
	  flags.decarm = FALSE;
	  break;
	case LNM:
	  flags.lnm = FALSE;
	  break;
	default:
	  break;
	}
      }
      goto state_trans;
    case action_scs:    /* Select Character Set */
      goto state_trans;
    case action_sgr:    /* Select Graphics Redition */
      switch (args[0]) {
      case 0: /* all attributes off! */
	attributes = 0;
	flags.r = 0;
	flags.g = 0;
	flags.b = 0;
	ps_text_color(
	    (float) flags.r / 256,
	    (float) flags.g / 256,
	    (float) flags.b / 256
	);
	break;
      case 1: /* bold on */
	attributes |= 0x2;
	break;
      case 4: /* underscore on */
	attributes |= 0x4;
	break;
      case 5: /* blink on */
	break;
      case 7: /* reverse video on */
	attributes |= 0x1;
	break;
      case 60: /* colours follow */
	flags.r = args[1];
	flags.g = args[2];
	flags.b = args[3];
	ps_text_color(
	    (float) flags.r / 256,
	    (float) flags.g / 256,
	    (float) flags.b / 256
	);
      }
      goto state_trans;
    case action_sm:/* Set Mode */
      for (cnt = 0; cnt <= c_arg; cnt++) {
	switch (args[cnt]) {
	case DECCOLM:
	  if (flags.deccolm != TRUE) {
	    flags.deccolm = TRUE;
	    ps_set_size_c(132, 24);
	    optr = &cur->line[x];
	    assert(scrolling || (cur->trimmed == FALSE));
	  }
	  break;
	case INSERT:
	  if (state != 3)
	    flags.insert = TRUE;
	  break;
	case DECSCNM:
	  flags.decscnm = TRUE;
	  break;
	case DECOM:
	  flags.decom = TRUE;
	  if (y_loc < bottom_margin)
	    y_loc = bottom_margin;
	  if (y_loc > top_margin)
	    y_loc = top_margin;
	  y = y_loc;
	  cur = screen[y];
	  optr = &cur->line[x];
	  assert(scrolling || (cur->trimmed == FALSE));
	  break;
	case DECAWM:
	  flags.decawm = TRUE;
	  break;
	case DECARM:
	  flags.decarm = TRUE;
	  break;
	case LNM:
	  flags.lnm = TRUE;
	  break;
	default:
	  break;
	}
      }
      goto state_trans;
    case action_tbc:    /* Clear tabulation */
      switch (args[0]) {
      case 0:
	tabs[x_loc] = '\0';
	break;
      case 3:
	bzero((char *) tabs, x_size);
	break;
      }
      goto state_trans;
    case action_dca_arg: /* DCA arguments */
      args[c_arg++] = *p2;
      if (c_arg >= 2) {
	x = args[1] - 037 - 1;
	y = args[0] - 037;
	if (flags.decom)
	  y = top_margin + 1 - y;
	else
	  y = y_size - y;
	state = 0; /* CHEATING! GAG! */
	goto do_move;
      }
      break;
    case action_dca:   /* Setup */
      c_arg = 0;
      goto state_trans;
    case action_ansi:
      ansi_mode();
      goto state_trans;
    case action_set_label:
      window_label[label_cnt] = '\0';
      if (label_setting == 0)
	ps_set_label_c(window_label);
      else
	ps_set_icon_label_c(window_label);
      label_cnt = 0;
      goto state_trans;
    case action_build_label:
      if (label_cnt < sizeof(window_label)) {
	window_label[label_cnt++] = *p2;
      }
      break;
    case action_setup_icon_label:
      label_setting = 1;
      goto state_trans;
    case action_setup_label:
      label_setting = 0;
      goto state_trans;
    }
  }

  if(scrolling_method > 2 && ioctl(master_fd, FIONREAD, &chars_read) >=0) {
      if (chars_read>0) 
	goto go_again;
    }

  do_show();
  
  ps_flush_PostScript_c();
  if (scrolling == FALSE) {
    if (link_count > 1500)
      cleaner(); 
    
    if ((link_count != last_count)) {
      last_count = link_count;
      ps_set_view_c(y_size, link_count);
    }
  }
  draw_cursor();
#ifdef DEBUG
  super_check();
#endif
}


/*
 * This changes the size of the internal data structures to a new
 * x_size/y_size values.
 */

void
  config_size()
{
  int		    i;
  SCREENLINE	   *foo;

  assert(x_size > 0);
  assert(y_size > 0);

  assert(scrolling == FALSE);

  if (x_size > tabs_size) {
    tabs = realloc(tabs, x_size + 1);
    bzero((char *) (tabs + tabs_size), x_size - tabs_size);
    
    for (i = tabs_size + (8 - tabs_size & 7); i < x_size; i += 8)
      tabs[i] = '\t';
    
    tabs_size = x_size;
  }
  bottom_margin = 0;
  top_margin = y_size - 1;
  diff_margin = top_margin - bottom_margin + 1;
  
  pty_set_size(master_fd, x_size, y_size);
  
  foo = screen[0];
  
  free(screen);
  screen = (SCREENLINE **) malloc(sizeof(SCREENLINE *) * y_size);
  
  for (i = 0; i < y_size; i++) {
    screen[i] = foo;
    if (foo->rsize < x_size) {
      foo->line = (char_type *) realloc(foo->line, (x_size + 1) * sizeof(char_type));
      memset(&foo->line[foo->rsize], ' ', (x_size - foo->rsize + 1) * sizeof(char_type));
      if (foo->ats) {
	foo->ats = (unsigned char *) realloc(foo->ats, (x_size + 1) 
					     * sizeof(attribute_type));
	bzero((char *) (&foo->ats[foo->rsize]), 
	      (x_size - foo->rsize + 1) * sizeof(attribute_type));
      }
      foo->rsize = x_size;
      foo->trimmed = FALSE;
    }
    if (foo->next == (SCREENLINE *) 0) {
      SCREENLINE	   *foo2;
      
      foo2 = allocate_node();
      foo2->next = 0;
      foo2->prev = foo;
      foo->next = foo2;
      tail = foo2;
    }
    foo = foo->next;
  }
  
  ps_reset_canvas_c();    /* Make sure our perception of the canvas is
			   * correct */
  cur = screen[y];
}

int  damagestart_tag = -1;

void
damage_handler()
{
  int		    x1, y1, x2, y2;
  int		    len, tmp, i;
  float fx1, fy1, fx2, fy2;

  ps_damage_start_c(damagestart_tag, &fx1, &fy1, &fx2, &fy2);

  y2 = to_char_y(fy2);
  x2 = to_char_x(fx2) + 1; /* just to make sure :-) */
  y1 = to_char_y(fy1);
  x1 = to_char_x(fx1);

  if (x2 >= x_size)
    x2 = x_size - 1;
  
  if (y2 >= y_size)
    y2 = y_size - 1;
  
  if (y1 >= y_size)
    y1 = y_size - 1;
  
  for (i = y1; i <= y2; i++) {
    tmp = screen[i]->len - x1;
    if (tmp > 0) {
      len = (tmp > x2 - x1) ?
	x2 - x1 : tmp;
      
      ps_show_at_c(x1, i, &screen[i]->line[x1], len, screen[i]->ats);
    }
  }

  invert_on();
  ps_damage_end_c();

  repair_cursor();
}
    
void
  reshape_handler()
{
  int		    lx, ly;
  double tx, ty;

  ty = wire_ReadFloat();
  tx = wire_ReadFloat();

  ly = to_char_y(ty);
  lx = to_char_x(tx);

  ps_reset_canvas_c();    /* Make sure our perception of the canvas 
                             is correct! */

  if ((lx == x_size) && (ly == y_size))
    return;

  if (scrolling) {
    quick_finish();
    ps_scroll_bottom_c();
    enable_cursor();
  }
  
  x_size = lx;
  y_size = ly;

  if (y_loc > y_size - 1)
    y_loc = y_size - 1;

  if (x_loc > x_size - 1)
    x_loc = x_size - 1;

  x = x_loc;
  y = y_loc;
  
  config_size();
  ps_set_view_c(y_size, link_count);
}

void
set_mode_handler() 
{
  int data;

  data = wire_ReadInt();

  switch(data) {
  case 0: /* Setting the scrolling properties */
    scrolling_method = wire_ReadInt();
    break;
  case 1: /* Setting the terminal ident string */
    (void) wire_ReadString(ident_str);
    break;
  case 2:
    set_saved_lines (wire_ReadInt());

    if (scrolling) {
      quick_finish();
      enable_cursor();
    }

    ps_set_view_c(y_size, link_count);
    break;
  }
}
#ifdef DEBUG
int how_many(fd)
int fd;
{
    int chars_read;

    printf("how_many: %d\n",ioctl(fd, FIONREAD, &chars_read));

    return(chars_read);
}
#endif
