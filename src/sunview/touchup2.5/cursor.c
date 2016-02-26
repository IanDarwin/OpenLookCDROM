
#ifdef CHANGE_CURSOR

/*
 * NOTE: if you do not want the changing cursors remove the compile
 * time option '-DCHANGE_CURSOR' from the Makefile
 */

/*
 * This file contains the cursor definitions and some small
 * support routine(s).
 * Can't possibly claim this is very original, so no copyright from me...
 * Pell 8 Jul 88
 */

/*
 * NOTE! The ffill_cur is currently not used. Could be needed if the user
 * interface for filling is changed, e.g. like M*cPaint.
 */

#include <suntool/sunview.h>
#include <suntool/canvas.h>

static short drawcur_data[] = {
#include "draw.cur"
};
static mpr_static(draw_cur_pr, 16, 16, 1, drawcur_data);

static short erasecur_data[] = {
#include "erase.cur"
};
static mpr_static(erase_cur_pr, 16, 16, 1, erasecur_data);


/*************************
* static short ffillcur_data[] = {
* #include "ffill.cur"
* };
* static mpr_static(ffill_cur_pr, 16, 16, 1, ffillcur_data);
***************************/

static short lasocur_data[] = {
#include "laso.cur"
};
static mpr_static(laso_cur_pr, 16, 16, 1, lasocur_data);

static short paintcur_data[] = {
#include "paint.cur"
};
static mpr_static(paint_cur_pr, 16, 16, 1, paintcur_data);

static short sel_pointcur_data[] = {
#include "sel_point.cur"
};
static mpr_static(sel_point_cur_pr, 16, 16, 1, sel_pointcur_data);

static short textcur_data[] = {
#include "text.cur"
};
static mpr_static(text_cur_pr, 16, 16, 1, textcur_data);

Cursor old_cur, draw_cur, erase_cur, ffill_cur, laso_cur, paint_cur,
       sel_point_cur, text_cur;

/*
 * Init all cursors from static data.
 */

init_cursors()
{
  /* old_cur initialized in interface.c */

  draw_cur = cursor_create(CURSOR_IMAGE, &draw_cur_pr,
			   CURSOR_OP, PIX_SRC^PIX_DST,
			   CURSOR_XHOT, 0,
			   CURSOR_YHOT, 15,
			   0);
  erase_cur = cursor_create(CURSOR_IMAGE, &erase_cur_pr,
			    CURSOR_OP, PIX_SRC^PIX_DST,
			    CURSOR_XHOT, 3,
			    CURSOR_YHOT, 12,
			    0);
/***************
  ffill_cur = cursor_create(CURSOR_IMAGE, &ffill_cur_pr,
			    CURSOR_OP, PIX_SRC^PIX_DST,
			    CURSOR_XHOT, 13,
			    CURSOR_YHOT, 13,
			    0);
*******************/
  laso_cur = cursor_create(CURSOR_IMAGE, &laso_cur_pr,
			   CURSOR_OP, PIX_SRC^PIX_DST,
			   CURSOR_XHOT, 4,
			   CURSOR_YHOT, 15,
			   0);
  paint_cur = cursor_create(CURSOR_IMAGE, &paint_cur_pr,
			    CURSOR_OP, PIX_SRC^PIX_DST,
			    CURSOR_XHOT, 2,
			    CURSOR_YHOT, 13,
			    0);
  sel_point_cur = cursor_create(CURSOR_IMAGE, &sel_point_cur_pr,
				CURSOR_OP, PIX_SRC^PIX_DST,
				CURSOR_XHOT, 7,
				CURSOR_YHOT, 7,
				0);
  text_cur = cursor_create(CURSOR_IMAGE, &text_cur_pr,
			   CURSOR_OP, PIX_SRC^PIX_DST,
			   CURSOR_XHOT, 5,
			   CURSOR_YHOT, 9,
			   0);
}

/* 
 * Change cursor. Canvas and cursor passed as parameters.
 */

change_cursor(can, cur)
     Canvas can;
     Cursor cur;
{
  window_set(can, WIN_CURSOR, cur, 0); /* OK, maybe overkill... */
}

#endif


