/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright OMRON Corporation. 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright ASTEC, Inc. 1987, 1988, 1989, 1990, 1991, 1992
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that all of the following conditions are satisfied:
 *
 * 1) The above copyright notices appear in all copies
 * 2) Both those copyright notices and this permission notice appear
 *    in supporting documentation
 * 3) The name of "Wnn" isn't changed unless substantial modifications
 *    are made, or
 * 3') Following words followed by the above copyright notices appear
 *    in all supporting documentation of software based on "Wnn":
 *
 *   "This software is based on the original version of Wnn developed by
 *    Kyoto University Research Institute for Mathematical Sciences (KURIMS),
 *    OMRON Corporation and ASTEC Inc."
 *
 * 4) The names KURIMS, OMRON and ASTEC not be used in advertising or
 *    publicity pertaining to distribution of the software without
 *    specific, written prior permission
 *
 * KURIMS, OMRON and ASTEC make no representations about the suitability
 * of this software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * Wnn consortium is one of distributors of the official Wnn source code
 * release.  Wnn consortium also makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * KURIMS, OMRON, ASTEC AND WNN CONSORTIUM DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL KURIMS, OMRON, ASTEC OR
 * WNN CONSORTIUM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */
/*	Version 4.0
 */

/* Copyright 1993 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

#ifndef lint
static char rcsid[] = "$Id: canna.c,v 2.4 1994/04/26 07:49:53 kon Exp $";
#endif

#include "commonhd.h"
#include "sdefine.h"
#include "sheader.h"
#include "config.h"

#include <errno.h>

#if __STDC__
#include <stdlib.h>
#define pro(x) x
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#define pro(x) ()
#endif

#ifndef _WCHAR_T
#define _WCHAR_T
#define _WCHAR_T_NOTDEFINED
#endif
#define wchar_t w_char
#include <canna/jrkanji.h>
#include <canna/RK.h>
#undef wchar_t
#ifdef _WCHAR_T_NOTDEFINED
#undef _WCHAR_T_NOTDEFINED
#undef _WCHAR_T
#endif

#include <fcntl.h>
#include <ctype.h>

#ifdef BSD42
#       include <sgtty.h>
#endif /* BSD42 */

#if defined(SYSVR2) && !defined(linux)
#       include <curses.h>
#       include <term.h>
#endif

#define MAXSIZE 1024
#define FULLREDRAW    2
#define PARTIALREDRAW 1
#define NOREDRAW      0

extern	int	errno;

static int maxmodelen;
static int maxwidth = 0;

static struct linebuf {
  w_char line[MAXSIZE];
  int    length;
  int    revPos;
  int    revLen;
  int    cursorPos;
  int    displayLeft, displayRight;
  int    unchangedLeft, unchangedRight;
  w_char mode_string[MAXSIZE];
  int	 mode_string_length;
  int    mode_string_width;
} lbuf[2];

#define PREV 0
#define CRNT 1

static int curlbuf = 0;
#define prevlbuf (curlbuf ? 0 : 1)

static w_char leftover[2] = {(w_char)'<', (w_char)0};
static w_char rightover[2] = {(w_char)'>', (w_char)0};
static w_char lrok[2] = {(w_char)' ', (w_char)0};

static unsigned char buf[MAXSIZE * 2];

char *terminalname;

#ifdef NODEBUG
#define debug(fmt, a, b, c)
#else
#define debug debugprint
#endif


/*

  The following three functions are defined in "w_string.c" in the
  original uum source file.

  Canna rewrote these functions.

  w_char *Strncpy();
  int eu_columlen();

 */

w_char *
Strncpy(ws1, ws2, cnt)
w_char *ws1, *ws2;
int cnt;
{
  w_char *ws;

  if  (ws2 == (w_char *)0)
    return((w_char *)0);
  if (ws2 < ws1 && ws1 < ws2 + cnt) {
    while (cnt--) {
      ws1[cnt] = ws2[cnt];
    }
  }
  else {
    int i = 0;
    ws = ws1;
    while (i++ < cnt && *ws2) {
      *ws++ = *ws2++;
    }
  }
  return ws1;
}

/*
  eu_columlen counts the string width by `column'.
  The following definition is very Japanese dependent.
 */

int
eu_columlen(c)
unsigned char *c;
{
  int len = 0;
  unsigned char ch;
  
  while (ch = *c) {
    if (ch & 0x80) {
      if (ch == 0x8e) {
	c++; len++; /* kana with half column width */
      }
      else if (ch == 0x8f) {
	c += 3;	len += 2; /* G3 kanji character */
      }
      else {
	c += 2; len += 2; /* G1 kanji character */
      }
    }
    else {
      c++; len++; /* ascii alphabet */
    }
  }
  return(len);
}

/*

  The following functions are added for Canna.

  In fact these functions are brought from Canna/lib/canna/util.c.

 */

int
WStrlen(ws)
w_char *ws;
{
  int res = 0;
  while (*ws++) {
    res++;
  }
  return res;
}

int
WStrcmp(w1, w2)
w_char *w1, *w2;
{
  for (; *w1 && *w1 == *w2; w1++, w2++);
  return(*w1 - *w2);
}

w_char *
WStrcpy(ws1, ws2)
w_char *ws1, *ws2;
{
  w_char *ws;
  int cnt, len;

  for (ws = ws2, cnt = 0 ; *ws ; ws++, cnt++) ;
  len = cnt;
  if (ws2 < ws1 && ws1 < ws2 + cnt) {
    while (cnt--) {
      ws1[cnt] = ws2[cnt];
    }
  }
  else {
    ws = ws1;
    while (*ws2) {
      *ws++ = *ws2++;
    }
  }
  ws1[len] = (w_char)0;
  return ws1;
}

static
colwidth(s, len)
w_char *s;
int     len;
{
  int ret = 0;
  w_char *es = s + len;

  for (; s < es ; s++) {
    switch (*s & 0x8080) {
    case 0:
    case 0x80:
      ret ++;
      break;
    case 0x8000:
    case 0x8080:
      ret += 2;
      break;
    }
  }
  return ret;
}

/*

  skipchar -- To skip characters until specified column width is
              exhaused.

  skipchar returns the number of skipped characters.  Sometimes this
  function over-run the column width.  The amount to be over-run
  returns in the argument ov.

  This function is very Japanese dependent.

 */

static
skipchars(s, wi, ov)
w_char *s;
int wi, *ov;
{
  int ret, swi;

  for (swi = 0, ret = 0 ; swi < wi && s[ret] ; ret++) {
    switch (s[ret] & 0x8080) {
    case 0:
    case 0x80:
      swi ++;
      break;
    case 0x8000:
    case 0x8080:
      swi += 2;
      break;
    }
  }
  *ov = swi - wi;
  return ret;
}


void
set_screen_vars_default() /* originally defined in basic_op.c */
{
  maxwidth =
    maxlength - maxmodelen - 2 + (conv_lines - 1) * (maxlength - 1) - 1;
}

/* canna routines */

int init_uum() /* originally defined in prologue.c */
{
  char **msg, *p;
  extern char *prog;
  extern void ring_bell();
  extern (*jrBeepFunc) pro((void));
  void registerkeys(), cannakeydef();

  for (p = prog ; *p ; p++) { /* use basename */
    if (*p == '/' && *(p + 1)) {
      prog = p + 1;
    }
  }

  if (def_servername[0]) {
    wcKanjiControl(0, KC_SETSERVERNAME, def_servername);
  }

  if (defined_by_option & OPT_WNNKEY) {
    wcKanjiControl(0, KC_SETINITFILENAME, uumkey_name_in_uumrc);
  }
  wcKanjiControl(0, KC_KEYCONVCALLBACK, (char *)cannakeydef);

  wcKanjiControl(0, KC_INITIALIZE, (char *)&msg);
  registerkeys();
  jrBeepFunc = (int (*)())ring_bell;
  if (msg) {
    for (; *msg; msg++) {
      puteustring(*msg, stdout);
      puteustring("\r\n", stdout);
    }
  }
#ifdef KC_SETAPPNAME
  wcKanjiControl(0, KC_SETAPPNAME, prog);
#endif
  maxmodelen = wcKanjiControl(0, KC_QUERYMAXMODESTR, 0);

  if (maxmodelen > MAXSIZE - 1) {
    maxmodelen = MAXSIZE - 1;
  }

  set_screen_vars_default(); /* will set maxwidth */

  wcKanjiControl(0, KC_SETWIDTH, (char *)(maxwidth + 1));
  /* plus 1 is for ``rightover'' character. */

  throw_cur_raw(0 ,crow + conv_lines);
  if (keypad_fun) set_keypad_on();
  scroll_up();
  set_scroll_region(0 , crow - 1);
  throw_cur_raw(0 ,crow  - 1);
  flush();

  return 0; /* succeeded */
}

/*

  The following function epilogue_no_close is copied from the original
  uum source file epilogue.c, and a little bit modified for canna.

 */

static struct RkRxDic *eseqdic; /* used at keyin1 */

void epilogue_no_close() /* originally defined in epilogue.c */
{
  wcKanjiControl(0, KC_FINALIZE, 0);
  RkCloseRoma(eseqdic);
  eseqdic = (struct RkRxDic *)0;

  throw_col(0);
  clr_line();
  if (keypad_fun) set_keypad_off();
  set_scroll_region(0 , crow + conv_lines - 1);
  kk_restore_cursor();
  flush();
#ifdef TERMINFO
  closeTermData();
#endif
}

void epilogue() /* originally defined in epilogue.c */
{
  epilogue_no_close();
}

extern int ptyfd, ttyfd;

static void
ptyout(s, n)
w_char *s;
int n;
{
  int ml;

  if ((ml = (*code_trans[(internal_code << 2) | pty_c_flag])
       (buf, s, n * sizeof(w_char))) > 0) {
    write(ptyfd, buf, ml);
  }
}

static void
ttyout(s, n)
w_char *s;
int n;
{
  int ml;

  if ((ml = (*code_trans[(internal_code << 2) | tty_c_flag])
       (buf, s, n * sizeof(w_char))) > 0) {
    write(ttyfd, buf, ml);
  }
}

static int cursor_saved = 0;

static void
cursor_restore_if_saved()
{
  if (cursor_saved) {
    restore_cursor_raw();
    flush();
    cursor_saved = 0;
  }
}

static int
cursor_save_if_not_saved()
{
  if (!cursor_saved) {
    save_cursor_raw();
    flush();
    cursor_saved = 1;
    return 1;
  }
  return 0;
}

#if defined(KC_DISCONNECTSERVER) && defined(KanjiThroughInfo)
#define MAXTHROUGHCOUNT 300
static int throughcount = 1;
#endif

#define MAXSEQUENCELEN 8
static char seqbuf[MAXSEQUENCELEN];
static int spooled; /* treated mainly in keyin1(); */

static void
normalize(dstat)
struct linebuf *dstat;
{
  switch (dstat->revLen) {
  case 0:
    dstat->cursorPos = dstat->revPos = dstat->length;
    break;
  case 1:
    dstat->cursorPos = dstat->revPos;
    dstat->revLen = 0;
    break;
  default:
    dstat->cursorPos = dstat->revPos;
    break;
  }
}

static int
diff(pr, cr)
struct linebuf *pr, *cr;
{
  w_char *pstr, *cstr;
  int maxUnchanged, i;
  int pRevPos = pr->revPos, cRevPos = cr->revPos;

  if (pr->revLen == 0) {
    pRevPos = pr->length;
  }
  if (cr->revLen == 0) {
    cRevPos = cr->length;
  }

  if (pRevPos == cRevPos) {
    if (pr->revLen == cr->revLen) {
      if (pr->length < cr->length) {
	maxUnchanged = pr->length;
      }
      else {
	maxUnchanged = cr->length;
      }
    }
    else if (pr->revLen < cr->revLen) {
      maxUnchanged = pRevPos + pr->revLen;
    }
    else {
      maxUnchanged = cRevPos + cr->revLen;
    }
  }
  else if (pRevPos < cRevPos) {
    maxUnchanged = pRevPos;
  }
  else {
    maxUnchanged = cRevPos;
  }

  pstr = pr->line;
  cstr = cr->line;

  for (i = 0 ; i < maxUnchanged ; i++) {
    if (*pstr++ != *cstr++) {
      break;
    }
  }
  cr->unchangedLeft = i;

  if (i == pr->length || i == cr->length) {
    cr->unchangedRight = 0;
    return pr->length != cr->length;
  }
  else if (pr->length - pRevPos - pr->revLen ==
	   cr->length - cRevPos - cr->revLen) {
    if (pr->length - pRevPos == cr->length - cRevPos) {
      if (pr->length < cr->length) {
	maxUnchanged = pr->length;
      }
      else {
	maxUnchanged = cr->length;
      }
    }
    else if (pr->length - pRevPos < cr->length - cRevPos) {
      maxUnchanged = pr->length - pRevPos;
    }
    else {
      maxUnchanged = cr->length - cRevPos;
    }
  }
  else if (pr->length - pRevPos - pr->revLen <
	   cr->length - cRevPos - cr->revLen) {
    maxUnchanged = pr->length - pRevPos - pr->revLen;
  }
  else {
    maxUnchanged = cr->length - cRevPos - cr->revLen;
  }
  if (maxUnchanged > cr->length - cr->unchangedLeft) {
    maxUnchanged = cr->length - cr->unchangedLeft;
  }
  if (maxUnchanged > pr->length - cr->unchangedLeft) {
    maxUnchanged = pr->length - cr->unchangedLeft;
  }

  pstr = pr->line + pr->length;
  cstr = cr->line + cr->length;

  for (i = 0 ; i < maxUnchanged ; i++) {
    if (*--pstr != *--cstr) {
      break;
    }
  }
  cr->unchangedRight = i;
  return 1;
}

/*
  check_redraw -- to check the difference from the previous display.

  check_redraw have a side effect, that is, it sets display data from
  ks to lbc.
 */

int
check_redraw(ks, lbc, lbp)
wcKanjiStatus *ks;
struct linebuf *lbp, *lbc;
{
  int	result = NOREDRAW;
  w_char *modstr = lbc->mode_string;

  if (ks->info & KanjiModeInfo) {
    int width, length;

    WStrcpy(modstr, ks->mode);
    length = WStrlen(modstr);
    width = colwidth(modstr, length);

    while (width < maxmodelen) {
      modstr[length++] = (w_char)' ';
      width++;
    }
    modstr[length] = (w_char)'\0';
    if (WStrcmp(modstr, lbp->mode_string)) {
      result = FULLREDRAW;
    }
    lbc->mode_string_length = length;
    lbc->mode_string_width = width;
  }
  else {
    WStrcpy(modstr, lbp->mode_string);
    lbc->mode_string_length = lbp->mode_string_length;
    lbc->mode_string_width = lbp->mode_string_width;
  }

  if ((ks->info & KanjiGLineInfo) && ks->gline.length > 0) {
    Strncpy(lbc->line, ks->gline.line, ks->gline.length);
    lbc->line[ks->gline.length] = (w_char)0;
    lbc->length = ks->gline.length;
    lbc->revLen = ks->gline.revLen;
    lbc->revPos = ks->gline.revPos;
  }
  else if (ks->length >= 0) {
    Strncpy(lbc->line, ks->echoStr, ks->length);
    lbc->line[ks->length] = (w_char)0;
    lbc->length = ks->length;
    lbc->revPos = ks->revPos;
    lbc->revLen = ks->revLen;
  }
  else {
    WStrcpy(lbc->line, lbp->line);
    lbc->length = lbp->length;
    lbc->revPos = lbp->revPos;
    lbc->revLen = lbp->revLen;
  }
  normalize(lbc);
  lbc->unchangedLeft = lbc->unchangedRight = 0;
  if (result == NOREDRAW) {
    if (diff(lbp, lbc)) {
      result = PARTIALREDRAW;
    }
  }
  return result;
}

static void
cursorWarp(lbc, to)
struct linebuf *lbc;
int to;
{
  int pos;
    
  pos = colwidth(lbc->line + lbc->displayLeft, to - lbc->displayLeft) +
    maxmodelen + 1;
  throw_cur_raw(pos, crow);
  flush();
}

static void
cursorMoveForward(lbc, from, to)
struct linebuf *lbc;
int from, to;
{
  int n;

  if ((n = to - from) > 0) {
    if (n < 8 &&
	(lbc->revLen == 0 ||
	 to <= lbc->revPos ||
	 lbc->revPos + lbc->revLen <= from) ) {
      /* confirm that this doesn't cross the reversed area */
      ttyout(lbc->line + from, n);
    }
    else {
      cursorWarp(lbc, to);
    }
  }
}

/*
  cursorMove -- to move cursor.

  Note: lbc->displayLeft should be fixed before this function is called.
 */

static void
cursorMove(lbc, from, to)
struct linebuf *lbc;
int from, to;
{
  if (to < from) {
    cursorWarp(lbc, to);
  }
  else {
    cursorMoveForward(lbc, from, to);
  }
}

static void
adjust_reverse(length, revPos, revLen, uLeft)
int length, *revPos, *revLen, uLeft;
{
  if (*revPos < uLeft) {
    *revLen -= uLeft - *revPos;
    if (*revLen < 0) *revLen = 0;
    *revPos = 0;
  }
  else {
    *revPos -= uLeft;
  }

  if (*revPos > length) {
    *revPos = length;
  }
  else if (*revPos + *revLen > length) {
    *revLen = length - *revPos;
  }
}

static void
redraw_it(gline, length, revPos, revLen)
w_char *gline;
int length, revPos, revLen;
{
  ttyout(gline, revPos);

  if (revLen) {
    h_r_on_raw();
    flush();
    ttyout(gline + revPos, revLen);
    h_r_off_raw();
    flush();
  }

  ttyout(gline + revPos + revLen, length - revPos - revLen);
}

/*
  redraw -- to redraw display

  redraw has a side effect, that is, it affects lbc->displayLeft and
  lbc->displayRight.
 */

static int
redraw(how, lbc, lbp)
int how;
struct linebuf *lbp, *lbc;
{
  int restwidth, skips, ov;
  w_char	*gline	= lbc->line;
  int		length	= lbc->length;
  int		revLen	= lbc->revLen;
  int		revPos	= lbc->revPos;
  int		csrPos	= lbc->cursorPos;
  int           uLeft = lbc->unchangedLeft;
  int           dLeft = lbp->displayLeft;

  if (uLeft < dLeft || csrPos < dLeft ||
      colwidth(gline + dLeft, revPos + revLen - dLeft) > maxwidth ||
      (colwidth(gline + dLeft, csrPos - dLeft) >= maxwidth - 1 &&
       revLen == 0 && length - revPos > 0)) {
    how = FULLREDRAW;
  }

  if (!cursor_invisible_fun && revLen > 0) {
    lbc->cursorPos = lbc->length;
  }

  switch (how) {
  case NOREDRAW:
    lbc->displayLeft = dLeft;
    lbc->displayRight = lbp->displayRight;
    cursorMove(lbc, lbp->cursorPos, lbc->cursorPos);
    break;
  case PARTIALREDRAW:
    if (length == 0) {
      lbc->displayLeft = lbc->displayRight = 0;
      cursorMove(lbc, lbp->cursorPos, 0);
      clr_end_screen();
      flush();
    }
    else {
      lbc->displayLeft = dLeft;
      cursorMove(lbc, lbp->cursorPos, uLeft);

      if (lbc->unchangedRight > lbp->length - lbp->displayRight &&
	  colwidth(gline + uLeft, length - uLeft - lbc->unchangedRight) ==
	  colwidth(lbp->line + uLeft,
		   lbp->length - uLeft - lbc->unchangedRight)) {
	/* The width of changed area is the same as the previous one */
	gline += uLeft;
	length -= uLeft + lbc->unchangedRight;
	adjust_reverse(length, &revPos, &revLen, uLeft);

	lbc->displayRight = lbp->displayRight + lbc->length - lbp->length;

	redraw_it(gline, length, revPos, revLen);
	if (lbc->cursorPos != lbc->length - lbc->unchangedRight) {
	  if (lbc->cursorPos > lbc->displayRight) {
	    cursorWarp(lbc, lbc->displayRight);
	    ttyout(rightover, 1);
	  }
	  else {
	    cursorWarp(lbc, lbc->cursorPos);
	  }
	}
      }
      else {
	restwidth = maxwidth;
	restwidth -= colwidth(gline + dLeft, uLeft - dLeft);
	gline += uLeft;
	skips = skipchars(gline, restwidth, &ov);
	if (ov > 0) {
	  skips -= ov;
	}
	length = skips;
	adjust_reverse(length, &revPos, &revLen, uLeft);
	lbc->displayRight = uLeft + length;

	redraw_it(gline, length, revPos, revLen);
	if (lbc->displayRight < lbc->length) {
	  ttyout(rightover, 1);
	}
	clr_end_screen();
	flush();
	if (lbc->cursorPos != lbc->length &&
	    lbc->cursorPos != lbc->displayRight) {
	  cursorWarp(lbc, lbc->cursorPos);
	}
      }
    }
    break;
  case FULLREDRAW:
    throw_cur_raw(0, crow);
    flush();

    ttyout(lbc->mode_string, lbc->mode_string_length);

    lbc->displayLeft = 0;
    lbc->displayRight = length;

    if (length > 0) {
      int	l, l1, l2;

      l = colwidth(gline, length);
      if (l > maxwidth) {

	/* In this condition, it is impossible to display the whole
	   pre-edit characters.  Some part of the pre-edit string will
	   be cut */

	l1 = colwidth(gline, revPos);
	l2 = revLen > 0 ? colwidth(gline + revPos, revLen) : 0;

	if (cursor_invisible_fun) {
	  if (revLen > 1) {
	    cursor_invisible_raw();
	  }
	  else {
	    cursor_normal_raw();
	  }
	}

	if (l2 > maxwidth) {
	  /* Align to the right border of reverse area. */
	  skips = skipchars(gline + revPos, l2 - maxwidth, &ov);
	  gline += revPos + skips;
	  revLen = length = revLen - skips;
	  lbc->displayLeft = revPos + skips;
	  revPos = 0;
#if 0
	  /* In case Aline to the left */
	  gline += revPos;
	  skips = skipchars(gline, maxwidth, &ov);
	  revLen = length = skips - ov;
	  lbc->displayLeft = revPos;
	  revPos = 0;
#endif
	}
	else if (l1 + l2 > maxwidth ||
		 (length - revPos - revLen > 0 && l1 >= maxwidth - 1)) {
	  /* Place reverse area to the middle of line. */
	  skips = skipchars(gline, l1 - (maxwidth - l2) / 2, &ov);
	  /* ov is not used */
	  gline += skips;
	  revPos -= skips;
	  length = skipchars(gline, maxwidth, &ov);
	  if (ov > 0) {
	    length -= ov;
	  }
	  lbc->displayLeft = skips;
	}
	else { /* length > maxwidth */
	  /* Just cut off the rest */
	  skips = skipchars(gline, maxwidth, &ov);
	  length = skips - ov;
	  lbc->displayLeft = 0;
	}
	lbc->displayRight = lbc->displayLeft + length;
      }

      if (lbc->displayLeft > 0) {
	ttyout(leftover, 1);
      }
      else {
	ttyout(lrok, 1);
      }

      redraw_it(gline, length, revPos, revLen);
      if (lbc->displayRight < lbc->length) {
	ttyout(rightover, 1);
      }
    }
    clr_end_screen();
    flush();

    if (lbc->length != lbc->cursorPos && lbc->displayRight != lbc->cursorPos) {
      cursorWarp(lbc, lbc->cursorPos);
    }
    break;
  }
  return 0;
}

/*

  The following function t_print_l_normal is originally defined in
  functions.c in uum source code.

 */

int
t_print_l_normal()
{
  cursor_restore_if_saved();
  save_cursor_raw();
  flush();
  redraw(FULLREDRAW, lbuf + curlbuf, lbuf + prevlbuf);
  restore_cursor_raw();
  flush();
  return 0;
}

char *
romkan_dispmode()
{
  return (char *)"\244\253\244\363\244\312";
              /* "かんな" in EUC */
}

char *
romkan_offmode()
{
  return romkan_dispmode();
}

/*

  The following two functions are defined originally in etc/msg.c.
  And these two functions are for messaging facility.  Here canna
  rewrote them as dummy functions.

  struct msg_cat *msg_open();
  char *get_msg();

 */

struct msg_cat *
msg_open(name, nlspath, lang) /* originally defined in etc/msg.c */
char *name;
char *nlspath;
char *lang;
/* ARGSUSED */
{
  return 0;
}

char *
msg_get(cad, n, mesg, lang) /* originally defined in etc/msg.c */
struct msg_cat *cad;
int n;
char *mesg;
register char *lang;
/* ARGSUSED */
{


  static char *msgtbl[] = {
    "\r\243\343\243\341\243\356\243\365\243\365\243\355(\244\253\244\312\264\301\273\372\312\321\264\271\245\325\245\355\245\363\245\310\245\250\245\363\245\311\245\327\245\355\245\273\245\303\245\265)\r\n",
/*  "\rｃａｎｕｕｍ(かな漢字変換フロントエンドプロセッサ)\r\n", */

    "Malloc\244\313\274\272\307\324\244\267\244\336\244\267\244\277\241\243",
/*  "Mallocに失敗しました。", */

    "\r\n\243\343\243\341\243\356\243\365\243\365\243\355\244\362\275\252\244\357\244\352\244\336\244\271\241\243\r\n",
/*  "\r\nｃａｎｕｕｍを終わります。\r\n", */

    "\243\365\243\365\243\355\244\253\244\351\243\365\243\365\243\355\244\317\265\257\244\263\244\273\244\336\244\273\244\363\241\243\n",
/*  "ｕｕｍからｕｕｍは起こせません。\n", */

    "",
    "",
    "",
    " (\307\241\262\277)",
/*  " (如何)", */

    "",
  };
  static int msgtblsize = sizeof(msgtbl) / sizeof(char *);

  if (n <= 0 || msgtblsize < n) {
    return "";
  }
  else {
    return msgtbl[n - 1];
  }
}

char *
get_kbd_env() /* originally defined in wnnrc_op.c */
{
  extern char *getenv();
  return getenv("TERM");
}

typedef struct {
  char *seq;
  int id;
} SeqToID;

#define INITIALSIZE 256

static struct RkRxDic *
RkCreateRoma(keywords, n)
SeqToID *keywords;
int n;
{
  struct RkRxDic *rdic;
  unsigned char *p;
  int i;

  rdic = (struct RkRxDic *)malloc(sizeof(struct RkRxDic));
  if (rdic) {
    rdic->dic = RX_KPDIC;
    rdic->nr_nkey = n;
    rdic->nr_strsz = INITIALSIZE;
    rdic->nr_string = (unsigned char *)malloc(INITIALSIZE);
    if (rdic->nr_string) {
      rdic->nr_brules = (unsigned char *)0;
      rdic->nr_bchars = rdic->nr_string;

      p = rdic->nr_string;
      *p = (unsigned char)0; p++;
      for (i = 0 ; i < n ; i++) {
	int len;

	len = strlen(keywords[i].seq);
	while (p + len + 4 > rdic->nr_string + rdic->nr_strsz) {
	  int offset = p - rdic->nr_string;
	  rdic->nr_string =
	    (unsigned char *)realloc(rdic->nr_string,
				     rdic->nr_strsz + INITIALSIZE);
	  if (!rdic->nr_string) {
	    goto exit_nr_string;
	  }
	  rdic->nr_strsz += INITIALSIZE;
	  p = rdic->nr_string + offset;
	}
	strcpy((char *)p, keywords[i].seq);
	p += len + 1;
	*p++ = (unsigned char)keywords[i].id;
	*p++ = (unsigned char)0;
	*p++ = (unsigned char)0; /* for temp and bang */
      }
      rdic->nr_strsz = p - rdic->nr_string;
      rdic->nr_string = (unsigned char *)realloc(rdic->nr_string,
						 rdic->nr_strsz);
      if (!rdic->nr_string) {
	goto exit_nr_string;
      }

      rdic->nr_keyaddr =
	(unsigned char **)calloc((unsigned)n + 1, sizeof(unsigned char *));
      /* n + 1 にしたのは alloc(0) のチェックが面倒なため */
      if (rdic->nr_keyaddr) {
	for (i = 0, p = rdic->nr_string + 1 ; i < n ; i++) {
	  rdic->nr_keyaddr[i] = p;
	  while ( *p++ ); /* roma */
	  while ( *p++ ); /* kana */
	  while ( *p++ ); /* temp */
	}
	return rdic;
      }
    exit_nr_string:
      free((char *)rdic->nr_string);
    }
    free((char *)rdic);
    rdic = (struct RkRxDic *)0;
  }
  
  return rdic;
}

#define INITIALKEYS 128

static SeqToID *sequences;
static int nsequences = 0, seqsize = 0;

static int
compar(p, q)
SeqToID *p, *q;
{	
  char *s = p->seq;
  char *t = q->seq;

  while ( *s == *t )
    if ( *s )
      s++, t++;
    else
      return 0;
  return ((int)*s) - ((int)*t);
}

void
registerkeys()
{
  qsort((char *)sequences, nsequences, sizeof(SeqToID), compar);
  eseqdic = RkCreateRoma(sequences, nsequences);
}

static
cannakeyentry(s, ident)
char *s;
int ident;
{
  if (!s || s[0] != '\033' || !s[1]) {
    return -1;
  }
  s++;
  while (!(nsequences < seqsize)) {
    sequences =
      (seqsize == 0) ?
	(SeqToID *)malloc(INITIALKEYS * sizeof(SeqToID)) :
	  (SeqToID *)realloc(sequences,
			     (seqsize + INITIALKEYS) * sizeof(SeqToID));
    if (sequences) {
      seqsize += INITIALKEYS;
    }
    else {
      seqsize = 0;
      return -1;
    }
  }

  sequences[nsequences].seq = malloc(strlen(s) + 1);
  if (sequences[nsequences].seq) {
    strcpy(sequences[nsequences].seq, s);
    sequences[nsequences].id = ident;
    nsequences++;
    return 0;
  }
  else {
    return -1;
  }
}

void
cannakeydef(xterm, term, seq, id)
int xterm;
char *term, *seq;
int id;
{
  if (xterm == CANNA_CTERMINAL) {
    if (terminalname && !strcmp(terminalname, term)) {
      cannakeyentry(seq, id);
    }
  }
}

/*

  convert_getterm is called from termio.c and termcap.c.

  Here provides a dummy convert_getterm().

 */

#define MAXSEQUENCE 20
#define AREASIZE 1024

int
convert_getterm(term, flag) /* originally defined in conv/cvt_read.c */
char *term;
int flag;
/* ARGSUSED */
{
#ifdef TERMCAP
  char xx[MAXSEQUENCE], *p = xx, *q, *tgetstr();

  char	tcaparea[AREASIZE];

  if (tgetent(tcaparea, term) > 0) {
    p = xx; if (q = tgetstr("k1", &p)) cannakeyentry(q, CANNA_KEY_F1);
    p = xx; if (q = tgetstr("k2", &p)) cannakeyentry(q, CANNA_KEY_F2);
    p = xx; if (q = tgetstr("k3", &p)) cannakeyentry(q, CANNA_KEY_F3);
    p = xx; if (q = tgetstr("k4", &p)) cannakeyentry(q, CANNA_KEY_F4);
    p = xx; if (q = tgetstr("k5", &p)) cannakeyentry(q, CANNA_KEY_F5);
    p = xx; if (q = tgetstr("k6", &p)) cannakeyentry(q, CANNA_KEY_F6);
    p = xx; if (q = tgetstr("k7", &p)) cannakeyentry(q, CANNA_KEY_F7);
    p = xx; if (q = tgetstr("k8", &p)) cannakeyentry(q, CANNA_KEY_F8);
    p = xx; if (q = tgetstr("k9", &p)) cannakeyentry(q, CANNA_KEY_F9);
    p = xx; if (q = tgetstr("ku", &p)) cannakeyentry(q, CANNA_KEY_Up);
    p = xx; if (q = tgetstr("kr", &p)) cannakeyentry(q, CANNA_KEY_Right);
    p = xx; if (q = tgetstr("kl", &p)) cannakeyentry(q, CANNA_KEY_Left);
    p = xx; if (q = tgetstr("kd", &p)) cannakeyentry(q, CANNA_KEY_Down);
    p = xx; if (q = tgetstr("kh", &p)) cannakeyentry(q, CANNA_KEY_Home);
  }
#endif

#ifdef TERMINFO
  int fd, res;

  fd = open("/dev/null", O_WRONLY, &res);
  setupterm(term, fd, (int *)0);

  cannakeyentry(key_f1,    CANNA_KEY_F1);
  cannakeyentry(key_f2,    CANNA_KEY_F2);
  cannakeyentry(key_f3,    CANNA_KEY_F3);
  cannakeyentry(key_f4,    CANNA_KEY_F4);
  cannakeyentry(key_f5,    CANNA_KEY_F5);
  cannakeyentry(key_f6,    CANNA_KEY_F6);
  cannakeyentry(key_f7,    CANNA_KEY_F7);
  cannakeyentry(key_f8,    CANNA_KEY_F8);
  cannakeyentry(key_f9,    CANNA_KEY_F9);
  cannakeyentry(key_f10,   CANNA_KEY_F10);
  cannakeyentry(key_up,    CANNA_KEY_Up);
  cannakeyentry(key_right, CANNA_KEY_Right);
  cannakeyentry(key_left,  CANNA_KEY_Left);
  cannakeyentry(key_down,  CANNA_KEY_Down);
  cannakeyentry(key_home,  CANNA_KEY_Home);
  cannakeyentry(key_sf,    CANNA_KEY_Rollup);
  cannakeyentry(key_sr,    CANNA_KEY_Rolldown);
  cannakeyentry(key_ic,    CANNA_KEY_Insert);

  resetterm();
#endif

  if (terminalname = malloc(strlen(term) + 1)) {
    strcpy(terminalname, term);
  }

  return 0;
}

int
keyin1(gch, yyy) /* originally defined in conv/cvt_read.c */
int (*gch) pro((void));
char *yyy; /* ARGSUSED */
{
  int ch, n, dummy1, dummy2, dummy3;
  char xxx[MAXSEQUENCELEN];

  if (spooled && seqbuf[spooled]) {
    return seqbuf[spooled++];
  }
  while ((ch = (*gch)()) < 0)
    ;
  if (ch == 0x1b && eseqdic) {
    int i = 1, res;

    seqbuf[0] = 0x1b;
    seqbuf[1] = 0;
    do {
      ch = (*gch)();
      if (ch < 0) {
	break;
      }
      seqbuf[i++] = ch;
      seqbuf[i] = '\0';
      res = RkMapPhonogram(eseqdic, xxx, MAXSEQUENCELEN, seqbuf + 1, i - 1,
			   0, 0, &n, &dummy1, &dummy2, &dummy3);
    } while (!n && i < MAXSEQUENCELEN - 1);
    if (!(ch < 0) && res) {
      spooled = 0;
      return (int)xxx[0] & 0xff;
    }
    else {
      seqbuf[0] = '\0';
      spooled = 1;
      return 0x1b;
    }
  }
  seqbuf[0] = '\0';
  return ch;
}

void
canna_mainloop()
{
  w_char workbuf[MAXSIZE];
  int wch, ml, howtoredraw;
  wcKanjiStatus ks;

  for (;;) {
#ifdef MAXTHROUGHCOUNT
    if (throughcount) {
      if (throughcount > MAXTHROUGHCOUNT) {
	wcKanjiControl(0, KC_DISCONNECTSERVER, 0);
	throughcount = 0;
      }
      else {
	throughcount++;
      }
    }
#endif

    /* keyin is wrong.  keyin can not treat G3 code correctly.
       keyin should be modified someday. */
    wch = keyin();
    if (wch != -1) {
      if (wch & 0x8000) { /* G1 or G3 kanji is entered. */
	w_char xx[2];

	cursor_restore_if_saved();
	xx[0] = (w_char)wch;
	xx[1] = (w_char)0;
	ptyout(xx, 1);
      }
      else {
	ml = wcKanjiString(0, wch, workbuf, MAXSIZE, &ks);

#ifdef MAXTHROUGHCOUNT
	if (!(ks.info & KanjiThroughInfo)) {
	  throughcount = 1;
	}
#endif

	curlbuf = prevlbuf; /* Note: prevlbuf is a macro */

	howtoredraw = check_redraw(&ks, lbuf + curlbuf, lbuf + prevlbuf);
	if (howtoredraw 
	    || lbuf[curlbuf].cursorPos != lbuf[prevlbuf].cursorPos) {
	  if (cursor_save_if_not_saved()) {
	    howtoredraw = FULLREDRAW;
	  }
	  redraw(howtoredraw, lbuf + curlbuf, lbuf + prevlbuf);
	  if (lbuf[curlbuf].length == 0) {
	    cursor_restore_if_saved();
	  }
	}
	if (ml > 0) {
	  cursor_restore_if_saved();
	  if ((ks.info & KanjiThroughInfo) && seqbuf[0]) {
	    write(ptyfd, seqbuf, strlen(seqbuf));
	  }
	  else {
	    ptyout(workbuf, ml);
	  }
	}
      }
    }
  }
}


/* dummy function definitions are below this line */

/*

  The following 3 functions are defined in Wnn system.

  Canna rewrote them.

  char *wnn_perror();
  char *get_server_env();
  char *get_kbd_env();

 */

char *
wnn_perror()
{
  return "??";
}

char *
get_server_env(lang) /* originally defined in etc/server_env.c */
char *lang;
/* ARGSUSED */
{
  return "CANNAHOST";
}

int
hani_settei_normal(c_b) /* originally defined in touroku.c */
/* struct buf *c_b; */
/* ARGSUSED */
{
  return 0;
}

int
initial_message_out()  /* originally defined in prologue.c */
{
  return 1; /* dummy function */
}

int
set_cur_env(s) /* originally defined in uif.c. */
char s;
/* ARGSUSED */
{
  return 0;
}

/*

  The following functions are originally defined in functions.c, and
  refered from header.c.

 */

int char_len_normal(x) w_char x; /* ARGSUSED */ { return 0; }

int c_top_normal() { return 0; }
int c_end_normal() { return 0; }
int call_t_print_l_normal(x, add) int x, add; /* ARGSUSED */ { return 0; }
int char_q_len_normal(x) w_char x; /* ARGSUSED */ { return 0; }
int call_jl_yomi_len() { return 0; }

int t_redraw_move_normal(x , start , end,clr_l)
  int x, start, end, clr_l; /* ARGSUSED */ { return 0; }
int call_t_redraw_move_normal(x, start, end, clt_l, add) 
  int x, start, end, clt_l, add; /* ARGSUSED */ { return 0; }
int call_t_redraw_move_1_normal(x, start, end, clt_l, add1, add2, mode)
  int x, start, end, clt_l, add1, add2, mode; /* ARGSUSED */ { return 0; }
int call_t_redraw_move_2_normal(x, start1, start2, end1, end2, clt_l, add)
  int x, start1, start2, end1, end2, clt_l, add; /* ARGSUSED */ { return 0; }

int call_redraw_line_normal(x, add) int x, add; /* ARGSUSED */ { return 0; }


debugprint(fmt, a, b, c)
char *fmt, *a, *b, *c;
{
  FILE *f, *fopen();

  f = fopen("/tmp/kon", "a");
  fprintf(f, fmt, a, b, c);
  fclose(f);
}
