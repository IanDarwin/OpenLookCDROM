/* Copyright 1992 NEC Corporation, Tokyo, Japan.
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

#if !defined(lint) && !defined(__CODECENTER__)
static char rcsid[] = "$Id: ebind.c,v 7.3 1994/03/01 13:46:57 kon Exp $";
#endif

#include "canna.h"

#define MAX_BYTE_PER_CHAR 4

extern int howToReturnModeInfo;

static wchar_t *inbuf = 0;
static int inbufsize = 0;

static
StoreWCtoEUC(wbuf, wbuflen, wks, ebuf, maxebuf, ks, ch, nbytes)
wchar_t *wbuf;
int wbuflen;
wcKanjiStatus *wks;
char *ebuf;
int maxebuf;
jrKanjiStatus *ks;
int ch;
int nbytes;
{
  int ret, rest, totallen = 0, len;
  char *p;

  /* info */

  ks->info = wks->info;
    
  /* 結果 */

  if (ks->info & KanjiThroughInfo) {
    if (nbytes) {
      ebuf[0] = ch;
    }
    ret = nbytes;
  }
  else {
    ret = (wbuflen > 0) ? WCstombs(ebuf, wbuf, maxebuf) : 0;
    if (ks->info & KanjiYomiInfo) {
      wchar_t *ep;
      len = WCstombs(ebuf + ret + 1, wbuf + wbuflen + 1,
		     maxebuf - ret - 1);
      ep = wbuf + wbuflen + 1;
      while (*ep) ep++;
      WCstombs(ebuf + ret + 1 + len + 1, ep + 1,
	       maxebuf - ret - 1 - len - 1);
    }
  }

  if (wks->length > 0) {
    totallen = wks->length;
  }
  if (wks->info & KanjiModeInfo) {
    totallen += WStrlen(wks->mode);
  }
  if (wks->info & KanjiGLineInfo) {
    totallen += wks->gline.length;
  }

  if (inbufsize < totallen) {
    inbufsize = totallen; /* inbufsize will be greater than 0 */
    if (inbuf) free(inbuf);
    inbuf = (wchar_t *)malloc(inbufsize * sizeof(wchar_t));
    if (!inbuf) {
      inbufsize = 0;
      jrKanjiError = "メモリが足りません";
      return -1;
    }
  }

  rest = inbufsize * sizeof(wchar_t);
  p = (char *)inbuf;

  if (wks->length < 0) {
    ks->length = -1;
  }
  else {
    /* エコー文字 */

    ks->length = ks->revLen = ks->revPos = 0;

    if (wks->length > 0) {
      ks->echoStr = (unsigned char *)p;
      if (wks->revPos > 0) {
	len = ks->revPos = CNvW2E(wks->echoStr, wks->revPos, p, rest);
	p += len;
	rest -= len;
      }
      if (wks->revLen > 0) {
	len = ks->revLen 
	  = CNvW2E(wks->echoStr + wks->revPos, wks->revLen, p, rest);
	p += len;
	rest -= len;
      }
      len = 0;
      if (wks->length - wks->revPos - wks->revLen > 0) {
	len = CNvW2E(wks->echoStr + wks->revPos + wks->revLen,
		     wks->length - wks->revPos - wks->revLen, p, rest);
	p += len;
	rest -= len;
      }
      ks->length = ks->revLen + ks->revPos + len;
      *p++ = '\0';
      rest--;
    }
  }

  /* モード表示 */

  if (wks->info & KanjiModeInfo) {
    len = WCstombs(p, wks->mode, rest);
    ks->mode = (unsigned char *)p;
    p[len] = '\0';
    p += len + 1;
    rest -= len + 1;
  }

  /* 一覧行表示 */

  if (wks->info & KanjiGLineInfo) {
    ks->gline.length = ks->gline.revLen = ks->gline.revPos = 0;

    if (wks->gline.length > 0) {
      ks->gline.line = (unsigned char *)p;
      if (wks->gline.revPos > 0) {
	len = ks->gline.revPos 
	  = CNvW2E(wks->gline.line, wks->gline.revPos, p, rest);
	p += len;
	rest -= len;
      }
      if (wks->gline.revLen > 0) {
	len = ks->gline.revLen
	  = CNvW2E(wks->gline.line + wks->gline.revPos, wks->gline.revLen,
		   p, rest);
	p += len;
	rest -= len;
      }
      len = 0;
      if (wks->gline.length - wks->gline.revPos - wks->gline.revLen > 0) {
	len = CNvW2E(wks->gline.line + wks->gline.revPos +
		     wks->gline.revLen,
		     wks->gline.length -
		     wks->gline.revPos - wks->gline.revLen,
		     p, rest);
	p += len;
	rest -= len;
      }
      ks->gline.length = ks->gline.revLen + ks->gline.revPos + len;
      *p++ = '\0';
      rest--;
    }
  }
  return ret;
}

XLookupKanji2(dpy, win, buffer_return, bytes_buffer, nbytes, functionalChar,
	      kanji_status_return)
unsigned int dpy, win;
int functionalChar, nbytes;
char *buffer_return;
int bytes_buffer;
jrKanjiStatus *kanji_status_return;
{
  int ret;
  wcKanjiStatus wks;
  int ch;
  int i;

  /* 内部バッファをアロケートする */
  if (inbufsize < bytes_buffer) {
    inbufsize = bytes_buffer; /* inbufsize will be greater than 0 */
    if (inbuf) free(inbuf);
    inbuf = (wchar_t *)malloc(inbufsize * sizeof(wchar_t));
    if (!inbuf) {
      inbufsize = 0;
      jrKanjiError = "メモリが足りません";
      return -1;
    }
  }

  inbuf[0] = (wchar_t)buffer_return[0];
  for (i = 1 ; i < nbytes ; i++) {
    inbuf[i] = (wchar_t)buffer_return[i];
  }
  ch = buffer_return[0];
  ret = XwcLookupKanji2(dpy, win, inbuf, inbufsize, nbytes, functionalChar,
			&wks);
  if (ret >= inbufsize)
    ret = inbufsize - 1;
  inbuf[ret] = (wchar_t)0;

  return StoreWCtoEUC(inbuf, ret, &wks,
		      (char *)buffer_return, bytes_buffer, kanji_status_return,
		      ch, nbytes);
}
		      

int
XKanjiControl2(display, window, request, arg)
unsigned int display, window, request;
BYTE *arg;
{
  int ret, len1, len2;
  wchar_t arg2[256];
  wchar_t wbuf[320], wbuf1[320], wbuf2[320];
  wcKanjiStatusWithValue wksv;
  wcKanjiStatus wks;
  int ch;

  wksv.buffer = wbuf;
  wksv.n_buffer = 320;
  wksv.ks = &wks;

  switch (request) {
  case KC_DO: /* val と buffer_return に入れるタイプ */
    wbuf[0] = ((jrKanjiStatusWithValue *)arg)->buffer[0];
    /* 下へ続く */
  case KC_CHANGEMODE: /* val を与えるタイプ */
    wksv.val = ((jrKanjiStatusWithValue *)arg)->val;
    goto withksv;
  case KC_STOREYOMI: /* echoStr と length と mode を与えるタイプ */
    /* まず mode をワイドにしてみよう */
    if (((jrKanjiStatusWithValue *)arg)->ks->mode) {
      len2 = MBstowcs(wbuf2, (char *)((jrKanjiStatusWithValue *)arg)->ks->mode,
		      320);
      wbuf2[len2] = (wchar_t)0;
      wks.mode = wbuf2;
    }
    else {
      wks.mode = (wchar_t *)0;
    }
    /* 下へ続く */
  case KC_DEFINEKANJI: /* echoStr と length を与えるタイプ */
    /* echoStr をワイドにして与えてみよう */
    len1 = MBstowcs(wbuf1,
		    (char *)((jrKanjiStatusWithValue *)arg)->ks->echoStr, 320);
    wbuf1[len1] = (wchar_t)0;
    wks.echoStr = wbuf1;
    wks.length = len1;
    /* 下へ続く */
  case KC_KAKUTEI: /* ただ単に与えて返って来るタイプ */
  case KC_KILL:
    goto withksv;
  case KC_CLOSEUICONTEXT:
    goto closecont;
  case KC_QUERYMODE: /* querymode */
    ret = XwcKanjiControl2(display, window, request, (BYTE *)arg2);
    if (!ret) {
      switch (howToReturnModeInfo) {
      case ModeInfoStyleIsString:
	WCstombs((char *)arg, arg2, 256);
	break;
      case ModeInfoStyleIsExtendedNumeric:
	arg[1] = (unsigned char)arg2[1];
      case ModeInfoStyleIsNumeric:
	arg[0] = (unsigned char)arg2[0];
	break;
      }
    }
    return(ret);
  case KC_SETLISTCALLBACK: /* どうしたら良いかわからないもの */
    return -1;
  default: /* ワイドでもEUCでも変わらないもの */
    return XwcKanjiControl2(display, window, request, arg);
  }
 withksv:
  ch = ((jrKanjiStatusWithValue *)arg)->buffer[0];
  ret = XwcKanjiControl2(display, window, request, (BYTE *)&wksv);
  if (ret < 0)
    return(ret);
  else {
    wksv.buffer[ret] = (wchar_t)0;
    ((jrKanjiStatusWithValue *)arg)->val =
      StoreWCtoEUC(wksv.buffer, wksv.val, wksv.ks,
		   (char *)((jrKanjiStatusWithValue *)arg)->buffer,
		   ((jrKanjiStatusWithValue *)arg)->bytes_buffer,
		   ((jrKanjiStatusWithValue *)arg)->ks,
		   ch, ((jrKanjiStatusWithValue *)arg)->val);
    return ((jrKanjiStatusWithValue *)arg)->val;
  }
 closecont:
  ch = ((jrKanjiStatusWithValue *)arg)->buffer[0];
  ret = XwcKanjiControl2(display, window, request, (BYTE *)&wksv);
  if (ret < 0)
    return(ret);
  else {
    wksv.val = 0;
    ((jrKanjiStatusWithValue *)arg)->val =
      StoreWCtoEUC(wksv.buffer, wksv.val, wksv.ks,
		   (char *)((jrKanjiStatusWithValue *)arg)->buffer,
		   ((jrKanjiStatusWithValue *)arg)->bytes_buffer,
		   ((jrKanjiStatusWithValue *)arg)->ks,
		   ch, ((jrKanjiStatusWithValue *)arg)->val);
    return ret;
  }
}

int
jrKanjiString(context_id, ch, buffer_return, nbuffer, kanji_status_return)
#if __STDC__
const int context_id, ch, nbuffer;
#else
int context_id, ch, nbuffer;
#endif
char  *buffer_return;
jrKanjiStatus  *kanji_status_return;
{
  *buffer_return = ch;

  return XLookupKanji2((unsigned int)0, (unsigned int)context_id,
		       buffer_return, nbuffer,
		       1/* byte */, 1/* functional char*/,
		       kanji_status_return);
}

/* jrKanjiControl -- カナ漢字変換の制御を行う */

int
jrKanjiControl(context, request, arg)
#if __STDC__
     const int context;
     const int request;
#else
     int context;
     int request;
#endif
     char *arg;
{
  return XKanjiControl2((unsigned int)0, (unsigned int)context,
			request, (BYTE *)arg);
}
