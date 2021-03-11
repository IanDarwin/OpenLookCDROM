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

char *uiKanjiError; /* 逐次のデバグ用 */

static char rcs_id[] = "@(#)$Id: sample.c,v 2.11 1994/03/10 07:57:55 kon Exp $";

#include <stddef.h>

#include <stdio.h>

#ifdef BIGPOINTER
#define POINTERINT long long
#else
#define POINTERINT long
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#if defined(SVR4) || defined(sun) || defined(sony)
#define HAVE_LOCALE
#endif

#ifdef HAVE_LOCALE
#include <locale.h>
#if !defined(SYSV386) && !defined(sony) && !defined(sun)
#include <wctype.h>
#endif
#endif

/* libw を持っているマシンやそうでないマシンがあって面倒なので
   自分で持ってしまう */

/*#ifndef HAVE_WCHAR_OPERATION*/
#define mbstowcs MBstowcs
/*#endif*/
static wchar_t *Wsncpy(), *Wscpy();
static int Wslen();

#include <string.h>
#include <sys/types.h>
#include <sys/times.h>
#include <canna/jrkanji.h>

#define ECHO
#define STRBUFSIZE    512	/* バイト */
  
static char sccs_id[] = "@(#)echowin.c	2.5	88/09/29 10:45:43";

#define ctl(x) ((x) & 0x1f) /* コントロールキーを表すマクロ ctl('a') 
			       のように使う */

static char *program = "sample";

typedef struct {
  int length;
  wchar_t str[STRBUFSIZE];
  int revPos, revLen, width, prevWidth;
} strRec;

typedef struct {
  Window win;
  strRec kakutei, mikakutei, mode, gline;
  int    cursor;
} winRec;

#define DRAW 1
#define NODRAW 0

winRec wb[2];

#ifdef DEBUG
static iroha_debug = 0;
#endif /* DEBUG */

main(argc, argv, envp)
int argc;
char *argv[], *envp[];
{
  int i;
  int beep();
  extern (*jrBeepFunc)();

  jrBeepFunc = beep;

#ifdef HAVE_LOCALE
  setlocale(LC_ALL,"");
#endif

  checkOptions(argc, argv);
  initialize (argc, argv, envp);
  mainloop ();
  exit_program ();
}

/* initialize -- 初期化処理
 *
 *
 * 初期化処理ですること
 *
 *  ・サーバのオープン
 *  ・フォントのロード
 *  ・フォント情報を得る (XQueryFont, ascent, descent, width)
 *  ・テキストカーソルの幅を決める
 *  ・ウィンドウの生成
 *  ・グラフィックコンテクストの生成
 *  ・ウィンドウのマップ
 *
 */

initialize(argc, argv, envp)
int argc;
char *argv[], *envp[];
{
  initializeX(argc, argv, envp);
  initializeCanna(argc, argv, envp);
}

/* グローバル変数の定義 */

int              i, nbytes;	       /* ワークか？ */
/* 候補一覧の再描画するためのカーソル位置などの情報を保存しておく変数 */
int              charwi;	/* 文字の幅 */
XEvent           event;		/* イベント */
Display          *dpy;
int              ww = 0;


/* mainloop -- メインループ
 *
 * メインルーチン
 *
 * イベントを拾ってディスパッチする。
 */

mainloop()
{
  do {
    XNextEvent (dpy, &event);
    erase_textcursor();
    switch (event.type)
      {
      case Expose:
	proc_expose();
	break;
      case KeyPress:
	proc_keypress(&event);
	break;
      case ButtonPress:
	proc_buttonpress(&event);
	break;
      default: /* なんだかよくわからないイベントが来た場合 */
	break;
      }
    if (wb[ww].cursor == DRAW) {
      draw_textcursor();
    }
  } while (1); /* (event.type != ButtonPress); */
}

#define InnerBorder   10
#define OuterBorder   5
#define BaseFontName  "-*-fixed-*-*-*--24-*-*-*-*-*"

static int  WinWidth = 80; /* [文字] */

Window      win, parwin;
XFontSet    fs;
GC          gc, rgc, mgc;
XFontStruct **fonts;
int         asc, des, cw;	/* 文字のアセント/ディセント/幅 */
int         CursorWidth;
static int glinePosition, abandon_kakutei = 0;

initializeX(argc, argv, envp)
int argc;
char *argv[], *envp[];
{
  XGCValues   gcv;

  /* サ−バとの接続をする */
  dpy = XOpenDisplay (0);

  if (dpy == NULL) {
    fprintf(stderr, "Ｘサーバに接続できませんでした\n");
    exit(1);
  }

  /* フォントをロ−ドする */
  {
    char **misstr, *defstr;
    int mc, n;
    XFontSetExtents *xfse;

    fs = XCreateFontSet(dpy, BaseFontName, &misstr, &mc, &defstr);
    if (fs) {
      xfse = XExtentsOfFontSet(fs);
      n = XFontsOfFontSet(fs, &fonts, &misstr);
      asc = des = 0;
      for (i = 0 ; i < n ; i++) {
	if (asc < fonts[i]->ascent) {
	  asc = fonts[i]->ascent;
	}
	if (des < fonts[i]->descent) {
	  des = fonts[i]->descent;
	}
      }
      if (n > 1) {
	/* フォントの横幅をとる。 */
	cw = fonts[0] ->max_bounds.width * 2 > fonts[1]->max_bounds.width
	  ? fonts[0]->max_bounds.width * 2 : fonts[1]->max_bounds.width;
      
	/* テキストカーソルの幅を決める。 */
	CursorWidth = XTextWidth(fonts[0], "a", 1);
      }
    }
    else {
      fprintf(stderr, "can not create font set.\n");
      exit(1);
    }
  }


  /* ウィンドウを生成する */
  parwin = XCreateSimpleWindow
    (dpy, DefaultRootWindow(dpy), 0, 0,
     cw * WinWidth / 2 + InnerBorder * 2 + OuterBorder * 2 + 2/*border*/,
#ifndef TEISEI
     ((asc + des + InnerBorder ) * 2/*行*/ + InnerBorder 
#else /* TEISEI */
     ((asc + des + InnerBorder ) * (2 + teiseiline + kanaline)/*行*/
      + InnerBorder 
#endif /* TEISEI */
      + OuterBorder + 2/*border*/) * 2 + OuterBorder,
     1,
     BlackPixel(dpy, DefaultScreen(dpy)),
     WhitePixel(dpy, DefaultScreen(dpy)));

  XStoreName(dpy, parwin, "canna sample");
  win = parwin;

  /* ＧＣを新しく生成する */
  gcv.foreground = BlackPixel(dpy, DefaultScreen(dpy));
  gcv.background = WhitePixel(dpy, DefaultScreen(dpy));
  
  gc = XCreateGC (dpy, win, GCForeground|GCBackground, &gcv);
  
  {
    XColor colordef;

    colordef.red   = 0xdf00;
    colordef.green = 0xef00;
    colordef.blue  = 0xff00;
    colordef.flags = DoRed | DoGreen | DoBlue;
    XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &colordef);

    gcv.background = colordef.pixel;
    gcv.foreground = BlackPixel(dpy, DefaultScreen(dpy));
    mgc = XCreateGC (dpy, win, GCForeground|GCBackground, &gcv);
  }

  gcv.foreground = gcv.background;
  gcv.background = BlackPixel(dpy, DefaultScreen(dpy));

  rgc = XCreateGC (dpy, win, GCForeground|GCBackground, &gcv);
  
  win = XCreateSimpleWindow
    (dpy, parwin, OuterBorder, OuterBorder, 
     cw * WinWidth / 2 + InnerBorder * 2,
#ifndef TEISEI
     (asc + des + InnerBorder) * 2 /* 行 */ + InnerBorder,
#else /* TEISEI */
     (asc + des + InnerBorder) * (2 + teiseiline + kanaline) /* 行 */
     + InnerBorder,
#endif /* TEISEI */
     1, 
     BlackPixel(dpy, DefaultScreen(dpy)),
     WhitePixel(dpy, DefaultScreen(dpy)));
  XSelectInput (dpy, win, ExposureMask | ButtonPressMask | KeyPressMask);
  wb[0].win = win;
  
  win = XCreateSimpleWindow
    (dpy, parwin, OuterBorder,
#ifndef TEISEI
     (asc + des + InnerBorder) * 2 /* 行 */ 
#else /* TEISEI */
     (asc + des + InnerBorder) * (2 + teiseiline + kanaline) /* 行 */ 
#endif /* TEISEI */
     + InnerBorder + OuterBorder * 2 + 2,
     cw * WinWidth / 2 + InnerBorder * 2,
#ifndef TEISEI
     (asc + des + InnerBorder) * 2 /* 行 */ + InnerBorder,
#else /* TEISEI */
     (asc + des + InnerBorder) * (2 + teiseiline + kanaline) /* 行 */
     + InnerBorder,
#endif /* TEISEI */
     1, 
     BlackPixel(dpy, DefaultScreen(dpy)),
     WhitePixel(dpy, DefaultScreen(dpy)));
  XSelectInput (dpy, win, ExposureMask | ButtonPressMask | KeyPressMask);
  wb[1].win = win;

  /* ウィンドウをマップする */
  XMapSubwindows (dpy, parwin);
  XMapWindow (dpy, parwin);
  return 0;
}

int double_final = 0;

/* exit_program -- 終了処理
 *
 * このプログラムを終了する時は必ずこの関数を呼ぼう
 */

exit_program()
{
  int res;
  char **warn;

  if (wcKanjiControl ( 0, KC_FINALIZE, (char *)&warn ) < 0) {
    extern char * jrKanjiError;

    fprintf(stderr, "KC_FILENALIZEでエラーが発生したよ\n");
    fprintf(stderr, "error: %s\n", jrKanjiError);
  }
  if (double_final) {
    int res;
    res = wcKanjiControl(0, KC_FINALIZE, (char *)&warn);
    fprintf(stderr, "kc_finalize = %d\n", res);
    if (res < 0) {
      extern char *jrKanjiError;
      fprintf(stderr, "%s\n", jrKanjiError);
      return;
    }
  }
  if (warn) {
    char **p;

    for (p = warn; *p ; p++) {
      fprintf(stderr, "%s\n", *p);
    }
  }
  XFreeGC(dpy, gc);
  XFreeGC(dpy, rgc);
  XFreeGC(dpy, mgc);
  XFreeFontSet (dpy, fs);
/*  XUnloadFont(dpy, fn); */
/*  XUnloadFont(dpy, fh); */
/*  XUnloadFont(dpy, fk); */
  XCloseDisplay (dpy);
  exit (0);
}

/* テキストカーソルの描画消去のルーチン。
 *
 * カーソルをどこに描くかという情報を変数 cursorcol から得る。
 */

draw_textcursor()
{
  /* テキストカーソルを描画する。 */
  
  XFillRectangle(dpy, win, gc,
		 InnerBorder + wb[ww].kakutei.width + wb[ww].mikakutei.width,
		 InnerBorder, CursorWidth, asc + des);
}

erase_textcursor()
{
  XClearArea(dpy, win, 
	     InnerBorder + wb[ww].kakutei.width + wb[ww].mikakutei.width,
	     InnerBorder, CursorWidth, asc + des, False);
}

/* proc_expose -- エキスポーズ処理
 *
 * col, rcol, rlen などの情報を使って文字列を描画する。
 */

proc_expose()
{
  int pos;

  redraw_kakutei();
  redraw_mikakutei();
  draw_both_mode();
  draw_gline();
#ifdef TEISEI
  if (teiseiline) {
    draw_teisei();
  }
  if (kanaline) {
    draw_kana();
  }
#endif /* TEISEI */
  XFlush(dpy);
}

static kanjiContext = 0;
int    msystime = 0, kstime = 0;
int    yomiLevel = 0;
int    given_nbytes, nbytes_given = 0;
extern tpo, euc;

/* proc_keypress -- キー入力の時の処理
 *
 */

static int checkIfFunctionalChar();

proc_keypress(event)
XEvent *event;
{
  char		   lookupbuf[STRBUFSIZE];
  wchar_t          wcbuf[STRBUFSIZE];
  wchar_t          wecho[STRBUFSIZE], wgline[STRBUFSIZE], wmode[STRBUFSIZE];
  KeySym           keysym;
  XComposeStatus   compose_status;
  wcKanjiStatus     kanji_status;
  jrKanjiStatus     euc_kanji_status;
  extern char *jrKanjiError;
  int functionalChar;

  nbytes = XLookupString ((XKeyEvent *)event, lookupbuf, STRBUFSIZE,
			  &keysym, &compose_status);

  if (nbytes == 0 && XK_overline <= keysym && keysym <= XK_semivoicedsound) {
    lookupbuf[0] = (unsigned long)keysym & 0xff;
    nbytes = 1;
  }

  /* 長さがゼロのものに関してはちょっと救済 */
  functionalChar = checkIfFunctionalChar(event, keysym, lookupbuf, STRBUFSIZE);
  /* shift+→のような文字だったら１文字と数える */
  if ( !nbytes && functionalChar ) {
    nbytes = 1;
  }

  /* 要らない場合は無視する */
  if (nbytes == 0) return 0;

  if (event->xkey.window == wb[0].win)
    kanjiContext = 0;
  else
    kanjiContext = 1;

  if (optionControl(event, lookupbuf[0],
		    wcbuf, &kanji_status)) {
    int i;
    struct tms tmbuf;

#ifdef TPO
    if (tpo) {
      keyresult((int)(unsigned char)lookupbuf[0]);
    }
#endif

    if (msystime) {
      kstime -= times(&tmbuf);
    }
    if (euc) {
      char xxxx[1024];

      nbytes = jrKanjiString (kanjiContext, lookupbuf[0],
			      lookupbuf,
			      nbytes_given ? given_nbytes : STRBUFSIZE,
			      &euc_kanji_status);

      if (euc_kanji_status.length >= 0) {
	strncpy(xxxx, (char *)euc_kanji_status.echoStr,
		euc_kanji_status.revPos);
	xxxx[euc_kanji_status.revPos] = (unsigned char)0;
	kanji_status.revPos = mbstowcs(wecho, xxxx, STRBUFSIZE);

	strncpy(xxxx, (char *)euc_kanji_status.echoStr
		+ euc_kanji_status.revPos, euc_kanji_status.revLen);
	xxxx[euc_kanji_status.revLen] = (unsigned char)0;
	kanji_status.revLen = mbstowcs(wecho + kanji_status.revPos,
				       xxxx, STRBUFSIZE);
	strncpy((char *)xxxx,
		(char *)euc_kanji_status.echoStr + euc_kanji_status.revPos +
		euc_kanji_status.revLen,
		euc_kanji_status.length - euc_kanji_status.revPos -
		euc_kanji_status.revLen);
	xxxx[euc_kanji_status.length - euc_kanji_status.revPos - 
	     euc_kanji_status.revLen] = (unsigned char)0;
	kanji_status.length = kanji_status.revPos + kanji_status.revLen +
	  mbstowcs(wecho + kanji_status.revPos + kanji_status.revLen,
		   xxxx, STRBUFSIZE);
      }

      if (euc_kanji_status.info & KanjiGLineInfo) {
	strncpy((char *)xxxx, (char *)euc_kanji_status.gline.line,
		euc_kanji_status.gline.revPos);
	xxxx[euc_kanji_status.gline.revPos] = (unsigned char)0;
	kanji_status.gline.revPos = mbstowcs(wgline, xxxx, STRBUFSIZE);

	strncpy((char *)xxxx, (char *)euc_kanji_status.gline.line +
		euc_kanji_status.gline.revPos,
		euc_kanji_status.gline.revLen);
	xxxx[euc_kanji_status.gline.revLen] = (unsigned char)0;
	kanji_status.gline.revLen = mbstowcs(wgline +
					     kanji_status.gline.revPos,
					     xxxx,
					     STRBUFSIZE);
	strncpy((char *)xxxx, (char *)euc_kanji_status.gline.line +
		euc_kanji_status.gline.revPos +
		euc_kanji_status.gline.revLen,
		euc_kanji_status.gline.length -
		euc_kanji_status.gline.revPos -
		euc_kanji_status.gline.revLen);
	xxxx[euc_kanji_status.gline.length - euc_kanji_status.gline.revPos - 
	     euc_kanji_status.gline.revLen] = (unsigned char)0;
	kanji_status.gline.length = kanji_status.gline.revPos +
	  kanji_status.gline.revLen +
	  mbstowcs(wgline + kanji_status.gline.revPos +
		   kanji_status.gline.revLen,
		   xxxx, STRBUFSIZE);
      }

      if (euc_kanji_status.info & KanjiModeInfo) {
	mbstowcs(wmode, euc_kanji_status.mode, 1024);
      }

      lookupbuf[nbytes] = (wchar_t)0;
      nbytes = mbstowcs(wcbuf, lookupbuf, STRBUFSIZE);
      if (euc_kanji_status.info & KanjiYomiInfo) {
	wchar_t *foo = wcbuf + nbytes + 1;
	char *bar = lookupbuf;
	int len;

	while (*bar) bar++;
	bar++;
	len = mbstowcs(foo, bar, wcbuf + STRBUFSIZE - foo);
	while (*bar) bar++;
	bar++;
	foo += len + 1;
	len = mbstowcs(foo, bar, wcbuf + STRBUFSIZE - foo);
      }

      kanji_status.echoStr = wecho;
      kanji_status.mode = wmode;
      kanji_status.gline.line = wgline;
      kanji_status.info = euc_kanji_status.info;
    }
    else {
      nbytes = wcKanjiString (kanjiContext, lookupbuf[0],
			      wcbuf, 
			      nbytes_given ? given_nbytes : STRBUFSIZE,
			      &kanji_status);
    }
    if (msystime) {
      kstime += times(&tmbuf);
    }
#ifdef TPO
    if (tpo) {
      jrKanjiStatus ks;
      unsigned char echo[1024], gline[1024], mode[256], ebuf[1024];
      wchar_t buf[1024];

      if (euc) {
	kanjiresult(lookupbuf, nbytes, &euc_kanji_status, 0);
      }
      else {
	if (kanji_status.length >= 0) {
	  Wsncpy(buf, kanji_status.echoStr, kanji_status.length);
	  buf[kanji_status.length] = (wchar_t)0;
	  wcstombs(echo, buf, 1024);
	}

	if (kanji_status.info & KanjiGLineInfo) {
	  Wsncpy(buf, kanji_status.gline.line, kanji_status.gline.length);
	  buf[kanji_status.gline.length] = (wchar_t)0;
	  wcstombs(gline, buf, 1024);
	}

	if (kanji_status.info & KanjiModeInfo) {
	  wcstombs(mode, kanji_status.mode, 1024);
	}

	wcbuf[nbytes] = (wchar_t)0;
	wcstombs(ebuf, wcbuf, 1024);

	ks.echoStr = echo;
	ks.mode = mode;
	ks.gline.line = gline;
	ks.length = kanji_status.length;
	ks.revPos = kanji_status.revPos;
	ks.revLen = kanji_status.revLen;
	ks.info = kanji_status.info;
	ks.gline.length = kanji_status.gline.length;
	ks.gline.revPos = kanji_status.gline.revPos;
	ks.gline.revLen = kanji_status.gline.revLen;
	kanjiresult(ebuf, nbytes, &ks, 1);
      }
    }
#endif
  }
  if (abandon_kakutei) {
    nbytes = 0;
  }

  /* 読みを表示してみる。 */
  if (nbytes) {
    if (yomiLevel && (kanji_status.info & KanjiYomiInfo)) {
      if (nbytes < STRBUFSIZE) {
	printf("読み \"%ws\"", wcbuf + nbytes + 1);
	if (yomiLevel > 1) {
	  int len = 0;
	  wchar_t *p = wcbuf + nbytes + 1;
	  while (*p) p++;
	  len = p - wcbuf - nbytes - 1;
	  if (nbytes + 1 + len < STRBUFSIZE) {
	    printf("、入力 \"%ws\"", p + 1);
	  }
	}
	printf(" に対応する部分が確定しました。\n");
      }
    }
  }

  {
    int i;
    
    win = event->xkey.window;
    for (i = 0 ; i < 2 ; i++) {
      if (wb[i].win == win) {
	ww = i;
      }
    }
    if (kanji_status.length >= 0) {
      wb[ww].cursor = (kanji_status.length != 0 && kanji_status.revLen) ? 
	NODRAW : DRAW;
    }
  }

  if (nbytes < 0) {
    fprintf(stderr, "sample: エラー: %s\n", jrKanjiError);
  }

#ifdef DEBUG
  if (iroha_debug) {
    print_debug(nbytes, kanjiContext, wcbuf,
		lookupbuf[0], &kanji_status);
  }
#endif

  if (nbytes > 0) { /* wcKanjiString の結果が０より大きいと言うことは
		       確定文字列が返っていると言うことなので、その確定文
		       字列を描画する。 */
    draw_determined(nbytes, wcbuf, &kanji_status);
    if (kanji_status.length >= 0) {
      /* 未確定情報を取り敢えずとっておく */
      wb[ww].mikakutei.length = kanji_status.length;
      Wsncpy(wb[ww].mikakutei.str, kanji_status.echoStr, 
	     wb[ww].mikakutei.length);
      wb[ww].mikakutei.revPos = kanji_status.revPos;
      wb[ww].mikakutei.revLen = kanji_status.revLen;
      wb[ww].mikakutei.prevWidth = wb[ww].mikakutei.width;
      wb[ww].mikakutei.width = 
	XwcTextEscapement(fs, wb[ww].mikakutei.str, wb[ww].mikakutei.length);
    }

    if (kanji_status.length > 0) {
      redraw_mikakutei();
    } 
  }
  else if (kanji_status.length >= 0) { /* kanji_status.length が０より大き 
					  いと言うことは未確定文字列が返さ 
					  れていると言うことなので、それを
					  描画する。 */
    draw_undetermined(nbytes, wcbuf, &kanji_status);
  }

  if (kanji_status.info & KanjiModeInfo) {
    /* 上記の判定文が真の時はカナ漢字変換モードに関する情報が返されて
       いると言うことなので、それを描画する。 */
    wb[ww].mode.prevWidth = wb[ww].mode.width;
    Wscpy(wb[ww].mode.str, kanji_status.mode);
    wb[ww].mode.str[Wslen(kanji_status.mode)] = (wchar_t)0;
    wb[ww].mode.length = Wslen(wb[ww].mode.str);
    wb[ww].mode.width = XwcTextEscapement(fs, wb[ww].mode.str, 
					  wb[ww].mode.length);
    XClearArea(dpy, win, InnerBorder, 
	       2 * InnerBorder + asc + des,
	       glinePosition,
	       asc + des, False);
    draw_mode();
  }

  if (kanji_status.info & KanjiGLineInfo) {
    /* 上記の判定文が真の時は候補一覧に関する情報が返されていると言う
       ことなので、それを描画する。 */
    checkGlineWidth(&kanji_status);
    wb[ww].gline.prevWidth = wb[ww].gline.width;
    wb[ww].gline.length = kanji_status.gline.length;
    Wsncpy(wb[ww].gline.str, 
	   kanji_status.gline.line, kanji_status.gline.length);
    wb[ww].gline.revPos = kanji_status.gline.revPos;
    wb[ww].gline.revLen = kanji_status.gline.revLen;
    wb[ww].gline.width = XwcTextEscapement(fs, wb[ww].gline.str,
					   wb[ww].gline.length);
    XClearArea(dpy, win, InnerBorder + glinePosition,
	       2 * InnerBorder + asc + des,
	       wb[ww].gline.prevWidth,
	       asc + des, False);
    draw_gline();
  }
#ifndef KanjiTeiseiInfo
#define KanjiTeiseiWasUndefined
#define KanjiTeiseiInfo 128
#endif /* KanjiTeiseiInfo */

#ifdef TEISEI
  if ((kanji_status.info & KanjiTeiseiInfo) && teiseiline) {
    wb[ww].teisei.prevWidth = wb[ww].teisei.width;
    wb[ww].teisei.length = kanji_status.teisei.length;
    strncpy(wb[ww].teisei.str, 
	    kanji_status.teisei.teiseiStr, kanji_status.teisei.length);
    wb[ww].teisei.revPos = kanji_status.teisei.revPos;
    wb[ww].teisei.revLen = kanji_status.teisei.revLen;
    wb[ww].teisei.width = XwcTextEscapement(fs, wb[ww].teisei.str, 
					    wb[ww].teisei.length);
    XClearArea(dpy, win, InnerBorder,
	       InnerBorder + 2 * (InnerBorder + asc + des),
	       wb[ww].teisei.prevWidth,
	       asc + des, False);
    draw_teisei();
  }
#endif /* TEISEI */
}

#ifdef DEBUG
static printchar(c)
int c;
{
  if (c < ' ') {
    printf("^%c", c + '@');
  }
  else if (c == 0x007f) {
    printf("^?");
  }
  else {
    printf("%wc", c);
  }
}

static printtext(s)
char *s;
{
  unsigned char *p;

  for (p = (unsigned char *)s ; *p ; p++) {
    printchar((int)*p);
  }
}

print_debug(n, cx, lub, key, ks)
     int n;
     int cx;
     wchar_t *lub;
     int key;
     wcKanjiStatus *ks;
{
  wchar_t buf[1024];
  static int count = 0;

  count++;
  printf("\n(sample %d)\n", count);
  printf("len=%d, ", n);
  printf("key=0x%x, ", key);
  printf("&kanjiStat=0x%x, ", ks);
  printf("echo_len=%d, ", ks->length);
  printf("revPos=%d, ", ks->revPos);
  printf("revLen=%d, ", ks->revLen);
  if (ks->length > 0) {
    Wsncpy(buf, ks->echoStr, ks->length);
    buf[ks->length] = '\0';
    printf("ks.echoStr=(%s)\n", buf);
  }
  { /* info の表示 */
    printf("\nインフォ: ");
    if (ks->info & KanjiModeInfo)
      printf("モード ");
    if (ks->info & KanjiGLineInfo)
      printf("一覧 ");
    if (ks->info & KanjiYomiInfo)
      printf("読み ");
    if (ks->info & KanjiThroughInfo)
      printf("スルー ");
    if (ks->info & KanjiEmptyInfo)
      printf("空 ");
    printf("(info=0x%x)\n", ks->info);
  }
  printf("glinelen=%d, ", ks->gline.length);
  if ((ks->info & KanjiGLineInfo) && ks->gline.length > 0) {
    Wsncpy(buf, ks->gline.line, ks->gline.length);
    buf[ks->gline.length] = '\0';
    printf("\nks.gline.line=("); printtext(buf); printf(")\n");
    printf("gline.revPos = %d gline.revLen = %d\n",
	   ks->gline.revPos, ks->gline.revLen);
  }
  if (ks->info & KanjiModeInfo) {
    printf("mode("); printtext(ks->mode); printf(")\n");
  }
  printf("buffer[0]=0x%x, ", lub[0]);
  lub[nbytes] = '\0';
  printf("buffer=\""); printtext(lub); printf("\"\n");
}
#endif



/* draw_determined -- 確定文字列の描画
 *
 * 確定文字列の描画では以下のことを行う
 *
 * ・コントロール文字だったらその処理ルーチンへ行く。
 * ・新しく確定した文字列を内部バッファにアペンドする。
 * ・新しく確定した部分を描画する。
 * ・新しく確定した文字列の幅を調べて、新しいカーソル位置を計算する。
 * ・buf のインデックスである col を更新する。
 * ・新しく確定した文字列の幅が、以前に未確定文字列として描画されて
 *   いた文字列の幅よりも小さいのであれば、XwcDrawImageString だ
 *   けでは消し切れないので、XClearArea を用いてはみだした部分を消去
 *   する。
 */

draw_determined(nbytes, wcbuf, kanji_status)
     int nbytes;
     wchar_t *wcbuf;
     wcKanjiStatus *kanji_status;
{
  int str_width;

  if (nbytes == 1 && ((unsigned int)wcbuf[0] < 0x20 ||
		      (unsigned int)wcbuf[0] == 0x7f) ) {
    wb[ww].kakutei.prevWidth = wb[ww].kakutei.width;
    if (wb[ww].mikakutei.width > 0) {
      XClearArea(dpy, win, InnerBorder + wb[ww].kakutei.width, InnerBorder,
		 wb[ww].mikakutei.width, asc + des, False);
    }
    /* 未確定文字列の部分を消去したので、 mikakutei.width をクリアして
       おく */
    wb[ww].mikakutei.width = 0;

    /* コントロールコードの入力（あまり良いコーディングではない） */
    wcbuf[2] = (wchar_t)'^';
    wcbuf[3] = (wchar_t)(wcbuf[0] + '@');
    wcbuf[4] = (wchar_t)0;
    if (!proc_ctl_keypress(wcbuf)) {
      draw_determined(2, wcbuf + 2, kanji_status);
    }
  }
  else { /* コントロールコードでない場合 */
    /* length というのは str に格納されている文字列の末尾を抑えるためのイ
       ンデックスである。新しく確定した文字列は length でインデックスされ
       る部分よりも後ろに格納する。 */
    Wsncpy(wb[ww].kakutei.str + wb[ww].kakutei.length, wcbuf, nbytes); 
    /* とりあえず、最後に NULL を入れておくが、不要かもしれない */
    wb[ww].kakutei.str[wb[ww].kakutei.length + nbytes] = '\0';

    /* 新しく確定した部分のみを描画する。*/
    XwcDrawImageString(dpy, win, fs, gc,
		       InnerBorder + wb[ww].kakutei.width,
		       InnerBorder + asc,
		       wb[ww].kakutei.str + wb[ww].kakutei.length,
		       nbytes);
    
    /* 新しく確定した文字列の幅を調べて、新しいカーソル位置を計算する */
    str_width = XwcTextEscapement(fs, 
				  wb[ww].kakutei.str + wb[ww].kakutei.length, 
				  nbytes);
    wb[ww].kakutei.prevWidth = wb[ww].kakutei.width;
    wb[ww].kakutei.width += str_width;

    /* str のインデックスである length を更新する。 */
    wb[ww].kakutei.length += nbytes;
    
    /* 新しく確定した文字列の幅が、以前に未確定文字列として描画されて
       いた文字列の幅よりも小さいのであれば、XwcDrawImageString だ
       けでは消し切れないので、XClearArea を用いてはみだした部分を消去
       しなくてはならない。 */
    if (str_width < wb[ww].mikakutei.width) {
      XClearArea(dpy, win, InnerBorder + wb[ww].kakutei.width, InnerBorder,
		 wb[ww].mikakutei.width - str_width, asc + des, False);
    }
    /* 未確定文字列の部分を消去したので、 mikakutei.width をクリアして
       おく */
    wb[ww].mikakutei.width = 0;
  }
}

/* draw_undetermined -- 未確定文字列の描画
 *
 */

draw_undetermined(nbytes, wcbuf, kanji_status)
     int nbytes;
     wchar_t *wcbuf;
     wcKanjiStatus *kanji_status;
{
  int rev_width;
  int pos;
  
  pos = wb[ww].kakutei.width;
  
  if (kanji_status->length < 0) /* kanji_status->length がマイナスの時
				   は前の未確定文字列と変化がないと言
				   うことなので何もしない。*/
    return;
  /* 未確定文字列が存在するのであれば、その処理をする。とりあえず、
    まずはバッファにコピーしておく */
  wb[ww].mikakutei.length = kanji_status->length;
  Wsncpy(wb[ww].mikakutei.str, kanji_status->echoStr, 
	 wb[ww].mikakutei.length);
  wb[ww].mikakutei.revPos = kanji_status->revPos;
  wb[ww].mikakutei.revLen = kanji_status->revLen;
  wb[ww].mikakutei.prevWidth = wb[ww].mikakutei.width;
  wb[ww].mikakutei.width = XwcTextEscapement(fs, 
					     wb[ww].mikakutei.str, 
					     wb[ww].mikakutei.length);
  redraw_mikakutei();
  
  /* 前に描画してあった文字列の長さが、新しく描画した文字列よりも長かっ
     た場合には、前に描画している部分で、はみ出している部分をクリアす
     る。 */
  if (wb[ww].mikakutei.prevWidth > wb[ww].mikakutei.width) {
    XClearArea(dpy, win, 
	       InnerBorder + wb[ww].kakutei.width + wb[ww].mikakutei.width, 
	       InnerBorder, 
	       wb[ww].mikakutei.prevWidth - wb[ww].mikakutei.width,
	       asc + des, False);
  }
}

/* draw_gline -- 候補一覧表示の部分を描画する。
 *
 */

int	    max_mode_strlen = 0;
char *malloc();

draw_gline()
{
  static int width, firsttime = 1;

  if (firsttime) {
    char *str;

    if (str = malloc(max_mode_strlen + 1)) {
      for (i = 0 ; i < max_mode_strlen ; i++) {
	str[i] = 'x';
      }
    }
    firsttime = 0;
    glinePosition = XTextWidth(fonts[0], str, max_mode_strlen);
    if (str) {
      free(str);
    }
  }
  
  redraw_general(dpy, win, gc, rgc, fonts,
		 InnerBorder + glinePosition,
		 2 * InnerBorder + 2 * asc + des,
		 &wb[ww].gline);
}

#ifdef TEISEI

/* draw_teisei -- 訂正文字列を書く
 *
 */

draw_teisei()
{
  redraw_general(dpy, win, gc, rgc, fonts,
		 InnerBorder,
		 3 * InnerBorder + 3 * asc + 2 * des,
		 &wb[ww].teisei);
}

draw_kana()
{
  redraw_general(dpy, win, gc, rgc, fonts,
		 InnerBorder,
		 3 * InnerBorder + 3 * asc + 2 * des,
		 &wb[ww].kana);
}

#endif /* TEISEI */

/* proc_ctl_keypress -- コントロールキーの処理ルーチン
 *
 */

int
proc_ctl_keypress(wcbuf)
     wchar_t *wcbuf;
{
  switch (wcbuf[0])
    {
    case ctl('M'):
      /* リターンキー */
#if defined(ECHO) && defined(HAVE_WCHAR_OPERATION)
      if (!tpo) {
	wb[ww].kakutei.str[wb[ww].kakutei.length] = '\0';
	printf("%ws\n", wb[ww].kakutei.str);
      }
#endif
      if (msystime) {
	printf("ここまでの wcKanjiString の時間 %d tics\n", kstime);
      }
      /* コラムおよびカーソルを戻し、ウィンドウをクリアする。 */
      wb[ww].kakutei.length = 0;
      wb[ww].kakutei.width = 0;
      XClearWindow(dpy, win);
      draw_mode();
      XFlush(dpy);
      return 1;
      break;
    case ctl('H'):
      /* バックスペースキー */
      if (wb[ww].kakutei.length) {
	if (wb[ww].kakutei.str[wb[ww].kakutei.length - 1] & 0x80) {
	  wb[ww].kakutei.length -= 1;
	  charwi =
	    XwcTextEscapement(fs, 
			      wb[ww].kakutei.str + wb[ww].kakutei.length, 1);
	}
	else {
	  wb[ww].kakutei.length--;
	  charwi =
	    XwcTextEscapement(fs, 
			      wb[ww].kakutei.str + wb[ww].kakutei.length, 1);
	}
	wb[ww].kakutei.width -= charwi;
	XClearArea(dpy, win, InnerBorder + wb[ww].kakutei.width, 
		   InnerBorder, charwi,
		   asc + des, False);
	XFlush(dpy);
      }
      return 1;
      break;
    case 0x7f:
      /* DEL が打たれた場合 */
      exit_program();
      /* NOTREACHED */
      break;
    case ctl('C'):
      /* かな漢字変換を再起動する */
      reset_iroha();
      /* モードを消去する */
      strcpy((char *)wb[0].mode.str, "");
      strcpy((char *)wb[1].mode.str, "");
      /* コラムおよびカーソルを戻し、ウィンドウをクリアする。 */
      wb[0].kakutei.length = 0;
      wb[0].kakutei.width = 0;
      wb[1].kakutei.length = 0;
      wb[1].kakutei.width = 0;
      XClearWindow(dpy, win);
      draw_both_mode();
      XFlush(dpy);
      kstime = 0; /* システム計測時間もリセット */
      return 1;
      break;
    default:
      return 0;
      break;
    }
}

reset_iroha()
{
  char **warn;

/*
  printf("いろはを落とします...");
  fflush(stdout);
*/
  wcKanjiControl(0, KC_FINALIZE, (char *)&warn);
  if (warn) {
    char **p;

    for (p = warn ; *p ; p++) {
      printf("%s\n", *p);
    }
  }
/*
  printf("いろはを再起動します...");
  fflush(stdout);
*/
  wcKanjiControl(0, KC_INITIALIZE, (char *)&warn);
  if (warn) {
    char **p;

    for (p = warn ; *p ; p++) {
      printf("%s\n", *p);
    }
  }
/*
  printf("再起動しました\n");
*/
}

/* draw_mode -- モード表示の部分を描画する。
 *
 */

draw_mode()
{
  int pos;

  /* モード表示の文字を描画する */
  pos = InnerBorder;
  XwcDrawImageString(dpy, wb[ww].win, fs, gc, pos, 
		     2 * InnerBorder + 2 * asc + des,
		     wb[ww].mode.str,
		     Wslen(wb[ww].mode.str));
}

draw_both_mode()
{
  int wwbk = ww;

  for (ww = 0 ; ww < 2 ; ww++) {
    draw_mode();
  }
  ww = wwbk;
}

redraw_kakutei()
{
  XwcDrawImageString(dpy, win, fs, gc, InnerBorder, InnerBorder + asc,
		     wb[ww].kakutei.str, wb[ww].kakutei.length);
}

redraw_mikakutei()
{
  redraw_general(dpy, win, mgc, rgc, fonts,
		 InnerBorder + wb[ww].kakutei.width, InnerBorder + asc,
		 &wb[ww].mikakutei);
}


/* redraw_general -- 一般的な再描画ルーチン

  strRec 構造体へのポインタを渡してやると、通常描画、反転描画、通常描画を
  してくれる。

  */

redraw_general(dpy, win, gc, rgc, fonts, x, y, s)
Display *dpy;
Window win;
GC gc, rgc;
XFontStruct **fonts;
int x, y;
strRec *s;
{
  int pos;

  if (s->length == 0) {
    return;
  }
  pos = x;
  XwcDrawImageString(dpy, win, fs, gc, pos, y, s->str, s->revPos);

  /* 反転表示の文字を描画する */
  pos += XwcTextEscapement(fs, s->str, s->revPos);
  XwcDrawImageString(dpy, win, fs, rgc, pos, y,
		     s->str + s->revPos, s->revLen);
  
  /* 後ろの通常表示の文字を描画する */
  pos += XwcTextEscapement(fs, s->str + s->revPos, s->revLen);
  XwcDrawImageString(dpy, win, fs, gc, pos, y,
		     s->str + s->revPos + s->revLen,
		     s->length - s->revPos - s->revLen);
}

proc_buttonpress(event)
XButtonEvent *event;
{
  switch (event->button) {
#ifdef DEBUG
  case 3:
    iroha_debug = iroha_debug ? 0 : 1;
    if (iroha_debug) {
      fprintf(stderr, "《《《デバグメッセージ》》》\n");
      wcKanjiControl(0, KC_DEBUGMODE, 1);
    }
    else {
      fprintf(stderr, "《《《 メッセージ終了 》》》\n");
      wcKanjiControl(0, KC_DEBUGMODE, 0);
    }
    break;
  case 2:
    {
      int kanjiContext;

      if (event->window == wb[0].win)
	kanjiContext = 0;
      else
	kanjiContext = 1;

      wcKanjiControl(kanjiContext, KC_DEBUGYOMI, 0);
    }
    break;
#endif /* DEBUG */
  default:
#ifdef DEBUG_ALLOC
    {
      extern fail_malloc;
      fail_malloc = !fail_malloc;
      fprintf(stderr, "☆%s malloc()\n",
	      fail_malloc ? "エラーする" : "普通の");
    }
#endif /* DEBUG_ALLOC */
#ifdef SHOW_ROME_STRUCT
    showRomeStruct(event->display, event->window);
#endif
    break;
  }
}

void
listcallback(client_data, func, items, nitems, cur_item)
char *client_data;
int  func;
wchar_t **items;
int nitems, *cur_item;
{
  int i;
  wchar_t **p;

  switch (func) {
  case CANNA_LIST_Start:
    printf("一覧開始\n");
    for (i = 0, p = items ; i < nitems ; i++, p++) {
      if (i == *cur_item) {
	printf("%d: (%ws)\n", i, *p);
      }
      else {
	printf("%d: %ws\n", i, *p);
      }
    }
    break;
  case CANNA_LIST_Select:
    printf("一覧選択\n");
    break;
  case CANNA_LIST_Quit:
    printf("一覧中止\n");
    break;
  case CANNA_LIST_Forward:
    printf("右\n");
    break;
  case CANNA_LIST_Backward:
    printf("左\n");
    break;
  case CANNA_LIST_Next:
    printf("下\n");
    break;
  case CANNA_LIST_Prev:
    printf("上\n");
    break;
  case CANNA_LIST_BeginningOfLine:
    printf("左端\n");
    break;
  case CANNA_LIST_EndOfLine:
    printf("右端\n");
    break;
  default:
    printf("なにかおかしいぞ\n");
    break;
  }
}

beep()
{
  XBell(dpy, 100);
}

int	    numControl = 0;
int	    jrik = 0;		/* 半角カタカナの禁止 */
int         showkeys = 0;	/* アルファモードで使っているキーを示す */
int	    esckill = 0, esckakutei = 0, escControl = 0, ckconn = 0;
int	    double_init = 0, final_initial = 0;
int	    given_width, width_given = 0, showMode = 0, kugiri = 0;
int         detailMode = 0, max_width;
int	    printenv = 0;
int         customtest = 0;
int         storeYomi = 0, storeThen = 0;
int         parse_data = 0;
char        parse_buf[512];
int         rengo_data = 0;
char        rengo_buf[512];
int         check_context = 0;
int	    no_mode = 0;
int	    show_max_mode = 0;
int         restrict = 0;
int         escclose = 0;
int         tpo = 0;
int         euc = 0;
#ifdef TEISEI
int	    teiseiline = 0;
int	    kanaline = 0;
#endif /* TEISEI */
#ifdef KC_SETSERVERNAME
int	    clist = 0;
#endif
int hexBy = 0, modeString = 0, undeffn = kc_normal;
#ifdef KC_DISCONNECTSERVER
int         escdisconnect = 0;
#endif /* KC_DISCONNECTSERVER */
int kakuteikana = 0;
int noname = 0;
int askphono = 0;

checkOptions(argc, argv)
int argc;
char *argv[];
{
  int i;

  for (i = 1 ; i < argc ; i++) {
    if ( !strcmp(argv[i], "-nc") ) {
      numControl = 1;
    }
#ifdef DEBUG
    else if ( !strcmp(argv[i], "-debug") ) {
      iroha_debug |= 1;
      wcKanjiControl(0, KC_DEBUGMODE, 1);
    }
#endif /* DEBUG */
    else if ( !strcmp(argv[i], "-nmode") ) {
      modeString = 1;
    }
    else if ( !strcmp(argv[i], "-imhex") ) {
      hexBy = 1;
    }
    else if ( !strcmp(argv[i], "-kakutei") ) {
      undeffn = kc_kakutei;
    }
    else if ( !strcmp(argv[i], "-through") ) {
      undeffn = kc_through;
    }
    else if ( !strcmp(argv[i], "-kill") ) {
      undeffn = kc_kill;
    }
    else if ( !strcmp(argv[i], "-bell") ) {
      extern (*jrBeepFunc)();
      jrBeepFunc = 0;
    }
    else if ( !strcmp(argv[i], "-ik") ) {
      jrik = 1;
    }
    else if ( !strcmp(argv[i], "-showkeys") ) {
      showkeys = 1;
    }
    else if ( !strcmp(argv[i], "-esckill") ) {
      esckill = 1;
      escControl = 1;
    }
    else if ( !strcmp(argv[i], "-esckakutei") ) {
      esckakutei = 1;
      escControl = 1;
    }
    else if ( !strcmp(argv[i], "-ckconn") ) {
      ckconn = 1;
    }
    else if ( !strcmp(argv[i], "-dinit") ) {
      double_init = 1;
}
    else if ( !strcmp(argv[i], "-dfinal") ) {
      double_final = 1;
    }
    else if ( !strcmp(argv[i], "-fin_init") ) {
      final_initial = 1;
    }
    else if ( !strcmp(argv[i], "-width") ) {
      width_given = 1;
      i++;
      given_width = atoi(argv[i]);
    }
    else if ( !strcmp(argv[i], "-escmode") ) {
      showMode = 1;
    }
    else if ( !strcmp(argv[i], "-escdetail1") ) {
      modeString = 1;
      detailMode = 1;
    }
    else if ( !strcmp(argv[i], "-escdetail2") ) {
      modeString = 2;
      detailMode = 2;
    }
    else if ( !strcmp(argv[i], "-kugiri") ) {
      kugiri = 1;
    }
    else if ( !strcmp(argv[i], "-printenv") ) {
      printenv = 1;
    }
    else if ( !strncmp(argv[i], "-bigmem", 7) ) {
      char *malloc();
      int megabytes = atoi(argv[i] + 7);

      if (argv[i][7] == '\0') {
	megabytes = 2;
      }
      fprintf(stderr, "%d MByte alloc しています...", megabytes);
      if (malloc(megabytes * 1024 * 1024) == (char *)0) {
	fprintf(stderr, "失敗\n");
      }
      else {
	fprintf(stderr, "成功\n");
      }
    }
    else if ( (!strcmp(argv[i], "-cs") ||
	       !strcmp(argv[i], "-cannaserver")) && i + 1 < argc ) {
      i++;
      wcKanjiControl(0, KC_SETSERVERNAME, argv[i]);
    }
    else if ( !strcmp(argv[i], "-yomi1") ) {
      yomiLevel = 1;
    }
    else if ( !strcmp(argv[i], "-yomi2") ) {
      yomiLevel = 2;
    }
    else if ( !strcmp(argv[i], "-customtest") ) {
      customtest = 1;
    }
    else if ( !strcmp(argv[i], "-storeyomi") ) {
      storeYomi = 1;
    }
    else if ( !strcmp(argv[i], "-storethen") ) {
      i++;
      storeThen = atoi(argv[i]);
    }
    else if ( !strcmp(argv[i], "-bufsize") ) {
      nbytes_given = 1;
      i++;
      given_nbytes = atoi(argv[i]);
    }
    else if ( !strcmp(argv[i], "-fn14") ) {
      /* 14 のフォントにする。*/
    }
    else if ( !strcmp(argv[i], "-fn16") ) {
      /* 16 のフォントにする。*/
    }
    else if ( !strcmp(argv[i], "-fn24") ) {
      /* 24 のフォントにする。*/
    }
    else if ( !strcmp(argv[i], "-fnhangul") ) {
      /* ハングルにする */
    }
#ifdef KC_SETINITFILENAME
    else if ( !strcmp(argv[i], "-f") ) {
      i++;
      wcKanjiControl(0, KC_SETINITFILENAME, argv[i]);
    }
#endif /* KC_SETINITFILENAME */
    else if ( !strcmp(argv[i], "-time") ) {
      msystime++;
    }
    else if ( !strcmp(argv[i], "-rengodic") ) {
      rengo_data = 1;
      i++;
      sprintf(rengo_buf, "rengodic \"%s\"\n", argv[i]);
    }
    else if ( !strcmp(argv[i], "-parse") ) {
      parse_data = 1;
      i++;
      sprintf(parse_buf, "%s", argv[i]);
    }
    else if ( !strcmp(argv[i], "-context") ) {
      check_context = 1;
    }
    else if ( !strcmp(argv[i], "-nomode") ) {
      no_mode = 1;
    }
    else if ( !strcmp(argv[i], "-maxmode") ) {
      show_max_mode = 1;
    }
    else if ( !strcmp(argv[i], "-restrict") ) {
      i++;
      if ( !strcmp(argv[i], "none") ) {
	restrict = CANNA_NOTHING_RESTRICTED + 1;
      }
      else if ( !strcmp(argv[i], "alphanum") ) {
	restrict = CANNA_ONLY_ALPHANUM + 1;
      }
      else if ( !strcmp(argv[i], "hex") ) {
	restrict = CANNA_ONLY_HEX + 1;
      }
      else if ( !strcmp(argv[i], "num") ) {
	restrict = CANNA_ONLY_NUMERIC + 1;
      }
      else if ( !strcmp(argv[i], "all") ) {
	restrict = CANNA_NOTHING_ALLOWED + 1;
      }
    }
    else if ( !strcmp(argv[i], "-escclose") ) {
      escclose = 1;
      escControl = 1;
    }
#ifdef TEISEI
    else if ( !strcmp(argv[i], "-teisei") ) {
      teiseiline = 1;
    }
    else if ( !strcmp(argv[i], "-kana") ) {
      kanaline = 1;
    }
#endif /* TEISEI */
#ifdef KC_SETLISTCALLBACK
    else if ( !strcmp(argv[i], "-clist") ) {
      clist = 1;
    }
#endif /* KC_SETLISTCALLBACK */
    else if ( !strcmp(argv[i], "-tpo") ) {
      tpo = 1;
    }
    else if ( !strcmp(argv[i], "-euc") ) {
      euc = 1;
    }
    else if ( !strcmp(argv[i], "-abandon") ) {
      abandon_kakutei = 1;
    }
    else if ( !strcmp(argv[i], "-wide") ) {
      WinWidth = 160;
    }
#ifdef KC_DISCONNECTSERVER
    else if ( !strcmp(argv[i], "-escdis") ) {
      escdisconnect = 1;
      escControl = 1;
    }
#endif /* KC_DISCONNECTSERVER */
    else if ( !strcmp(argv[i], "-kakuteikana") ) {
      kakuteikana = 1;
    }
    else if ( !strcmp(argv[i], "-noname") ) {
      noname = 1;
    }
    else if ( !strcmp(argv[i], "-askphono") ) {
      askphono = 1;
    }
    else {
      fprintf(stderr, "usage: %s [-option ...]\n", argv[0]);
      fprintf(stderr, "options:\n");
/* 次の２つは無効である。
      fprintf(stderr, "-[no]im   一太郎と同じカーソルの動きをする(しない)\n");
      fprintf(stderr, "-[no]roc  幅広く反転する(しない)\n");
*/
      fprintf(stderr, "-bell     未定義キーでベルを鳴らさない\n");
      fprintf(stderr, "-bufsize num buffer_returnのサイズを与える\n");
      fprintf(stderr, "-ckconn   サーバとの接続のチェック\n");
      fprintf(stderr, "-context  コンテクストを調べる\n");
      fprintf(stderr, "-customtest KC_PARSEのテスト\n");
      fprintf(stderr, "-debug    デバグモード\n");
      fprintf(stderr, "-dinit    KC_INITIALIZEを２回続けて行うテスト\n");
      fprintf(stderr, "-dfinal   KC_FINALIZEを２回続けて行うテスト\n");
      fprintf(stderr, "-esckakutei ESCキーで未確定文字列を確定する\n");
      fprintf(stderr, "-esckill  ESCキーで未確定文字列を抹消する\n");
      fprintf(stderr, "-escmode  ESCキーでモードを表示する\n");
      fprintf(stderr, "-escdetail1 ESCキーでメジャーモードを表示する\n");
      fprintf(stderr, "-escdetail2 ESCキーでマイナーモードも表示する\n");
      fprintf(stderr, "-escdis   ESCキーでサーバとの接続を切る\n");
      fprintf(stderr, "-f        イニシャライズファイルを指定する\n");
      fprintf(stderr, "-fin_init KC_FINALIZEの後KC_INITIALIZEを行うテスト\n");
      fprintf(stderr, "-fn14     14ドットフォントを使う\n");
      fprintf(stderr, "-fn16     16ドットフォントを使う\n");
      fprintf(stderr, "-fn24     24ドットフォントを使う\n");
      fprintf(stderr, "-ik       半角カタカナに字種変換しない\n");
      fprintf(stderr, "-imhex    １６進入力は４文字目で確定する\n");
      fprintf(stderr, "-kakutei  未定義キーで未確定文字列を確定する\n");
      fprintf(stderr, "-kill     未定義キーで未確定文字列を抹消する\n");
      fprintf(stderr, "-kugiri   文節の区切りに空白を入れる\n");
      fprintf(stderr, "-nc       数字キーでコマンドを実行する\n");
      fprintf(stderr, "-nmode    モード表示文字を数値を表す文字にする\n");
      fprintf(stderr, "-parse str カスタマイズ文字列を与える\n");
      fprintf(stderr, "-printenv 環境変数を表示する\n");
      fprintf(stderr, "-rengodic dic 連語辞書を指定する\n");
      fprintf(stderr, "-cs       コネクトするサーバを指定する\n");
      fprintf(stderr, "-showkeys 予約されているキーを表示する\n");
      fprintf(stderr, "-storeyomi読みをストアする\n");
      fprintf(stderr, "-storethen num 読みをストアして次に何をするか指定\n");
      fprintf(stderr, "-through  未定義キーをそのまま通す\n");
      fprintf(stderr, "-width n  幅を与える\n");
      fprintf(stderr, "-yomi1    確定した時読みを表示する\n");
      fprintf(stderr, "-yomi2    確定した時読みと入力キー列を表示する\n");
      fprintf(stderr, "-tpo      テスト用の表示を出力する\n");
      exit (1);
    }
  }
}

initializeCanna(argc, argv, envp)
int argc;
char *argv[], *envp[];
{
  {
    char **warn;

    if (final_initial) {
      wcKanjiControl(0, KC_INITIALIZE, (char *)0);
      wcKanjiControl(0, KC_FINALIZE, (char *)0);
      wcKanjiControl(0, KC_INITIALIZE, (char *)0);
      wcKanjiControl(0, KC_FINALIZE, (char *)0);
      wcKanjiControl(0, KC_INITIALIZE, (char *)0);
      wcKanjiControl(0, KC_FINALIZE, (char *)0);
    }
    wcKanjiControl(0, KC_INITIALIZE, (char *)&warn);
    if (double_init) {
      int ret;

      ret = wcKanjiControl(0, KC_INITIALIZE, (char *)&warn);
      fprintf(stderr, "２回目の initialize の値は %d\n", ret);
    }
    if (warn) {
      char **p;
      
      for (p = warn; *p ; p++) {
	fprintf(stderr, "%s\n", *p);
      }
    }

#ifdef KC_SETAPPNAME
    if (!noname) {
      wcKanjiControl(0, KC_SETAPPNAME, "sample");
    }
#endif /* KC_SETAPPNAME */

#ifdef KC_QUERYPHONO
    if (askphono) {
      char *foo;
      wcKanjiControl(0, KC_QUERYPHONO, (char *)&foo);
      if (foo) {
	printf("ローマ字かな変換テーブルは \"%s\" です。\n", foo);
      }
    }
#endif

    wcKanjiControl(0, KC_QUERYMODE, (char *)wb[0].mode.str);
    wcKanjiControl(1, KC_QUERYMODE, (char *)wb[1].mode.str);
    wb[0].mode.length = Wslen(wb[0].mode.str);
    wb[0].mode.revPos = wb[0].mode.revLen = 0;
    wb[1].mode.length = Wslen(wb[1].mode.str);
    wb[1].mode.revPos = wb[1].mode.revLen = 0;
  }
  if (ckconn) {
    if (wcKanjiControl(0, KC_QUERYCONNECTION, (char *)0)) {
      fprintf(stderr, "サーバとつながっているよ\n");
    }
    else {
      fprintf(stderr, "サーバとつながっていないよ\n");
    }
  }
  max_mode_strlen = wcKanjiControl(0, KC_QUERYMAXMODESTR, (char *)0) + 2;
  if (show_max_mode) {
    fprintf(stderr, "モード文字列の最大コラム幅は %d です。\n",
	    max_mode_strlen - 2);
  }
  if (width_given) {
    fprintf(stderr, "候補一覧表示のコラム幅は %d です。\n", given_width);
    max_width = given_width;
  }
  else {
    max_width = WinWidth - 2 - max_mode_strlen;
  }
  wcKanjiControl(0, KC_SETWIDTH, (char *)(POINTERINT)max_width);
  wcKanjiControl(1, KC_SETWIDTH, (char *)(POINTERINT)max_width);

#ifdef KC_SETLISTCALLBACK
  if (clist) {
    jrListCallbackStruct lcs;
    void listcallback();

    lcs.client_data = 0;
    lcs.callback_func = listcallback;
    wcKanjiControl(0, KC_SETLISTCALLBACK, (char *)&lcs);
    wcKanjiControl(1, KC_SETLISTCALLBACK, (char *)&lcs);
  }
#endif
  if (kugiri) {
    wcKanjiControl(0, KC_SETBUNSETSUKUGIRI, (char *)(POINTERINT)1);
  }
  if (rengo_data) {
    int i, n;
    char *p = rengo_buf, **pp;

    n = wcKanjiControl(0, KC_PARSE, (char *)&p);
    pp = (char **)p;
    for (i = 0 ; i < n ; i++) {
      fprintf(stderr, "%s\n", *pp++);
    }
  }
  if (parse_data) {
    int i, n;
    char *p = parse_buf, **pp;

    n = wcKanjiControl(0, KC_PARSE, (char *)&p);
    pp = (char **)p;
    for (i = 0 ; i < n ; i++) {
      fprintf(stderr, "%s\n", *pp++);
    }
  }
  wcKanjiControl(0, KC_SETMODEINFOSTYLE, (char *)(POINTERINT)modeString);
  if (hexBy) {
    wcKanjiControl(0, KC_SETHEXINPUTSTYLE, (char *)(POINTERINT)hexBy);
  }
  wcKanjiControl(0, KC_SETUNDEFKEYFUNCTION, (char *)(POINTERINT)undeffn);
  wcKanjiControl(0, KC_INHIBITHANKAKUKANA, (char *)(POINTERINT)jrik);
  wcKanjiControl(0, KC_YOMIINFO, (char *)(POINTERINT)yomiLevel);

  if (kakuteikana) {
    wcKanjiStatusWithValue ksv;
    wcKanjiStatus ks;
    wchar_t xxxx[512];

    ksv.buffer = xxxx;
    ksv.n_buffer = 512;
    ksv.ks = &ks;
    ksv.buffer[0] = '@';
    ksv.val = CANNA_FN_JapaneseMode;
    wcKanjiControl(0, KC_DO, (char *)&ksv);
    ksv.val = CANNA_FN_BaseKakutei;
    wcKanjiControl(0, KC_DO, (char *)&ksv);
    ksv.val = CANNA_FN_BaseKatakana;
    wcKanjiControl(0, KC_DO, (char *)&ksv);
  }

  if (customtest) {
    char *custom = "henkan C-l\nyomiganai {\n  kigouMode ^l\n}\n";

    printf("C-l に変換、読みが無い状態では記号モード、を割り当てます。\n");
    wcKanjiControl(0, KC_PARSE, (char *)&custom);
  }
  if (showkeys) {
    char keys[20];
    int n, i;

    n = wcKanjiControl(0, KC_MODEKEYS, keys);
    fprintf(stderr, "アルファベットモードで %d 個のキーを使っていて\n", n);
    fprintf(stderr, "それらは、");
    for (i = 0 ; i < n ; i++) {
      fprintf(stderr, "0x%02x、", keys[i]);
    }
    fprintf(stderr, "です。\n");
  }
  if (printenv) {
    char **p;
    for (p = envp ; *p ; p++) {
      fprintf(stderr, "envは、(%s)\n", *p);
    }
  }
  if (check_context) {
    fprintf(stderr, "context: %d, bushuContext: %d.\n",
	    wcKanjiControl(0, KC_GETCONTEXT, (char *)(POINTERINT)0),
	    wcKanjiControl(0, KC_GETCONTEXT, (char *)(POINTERINT)1));
  }
  if (no_mode) {
    fprintf(stderr, "今から下は制限せずに上はモード変更を制限します....\n");
    if (wcKanjiControl(1, KC_INHIBITCHANGEMODE, (char *)(POINTERINT)1) < 0) {
      fprintf(stderr, "...モードの制限をするのに失敗しました。\n");
    }
    else {
      fprintf(stderr, "...モードの変更を制限しました。\n");
    }
  }
  if (restrict) {
    if (wcKanjiControl(1,
		      KC_LETTERRESTRICTION, (char *)(POINTERINT)(restrict - 1)) < 0) {
      fprintf(stderr, "文字種の制限に失敗しました。\n");
    }
  }
}

/*
 *
 * optionControl のリターン値は option の処理で wcKanjiString がシャドウ
 * されるかされないかを表す。1 はシャドウされず、wcKanjiString を行うべき
 * であることを示し、0 はシャドウされることを示す。
 *
 */

optionControl(event, key, wcbuf, ks)
XEvent *event;
wchar_t *wcbuf;
int key;
wcKanjiStatus *ks;
{
  int notControl = 1;
  int defineKanji = 0;

  if (escControl && nbytes > 0 && key == 0x1b) {
    wcKanjiStatusWithValue ksv;
    ksv.buffer = (wchar_t *)wcbuf;
    ksv.n_buffer = STRBUFSIZE;
    ksv.ks = ks;
    notControl = 0; defineKanji = 0;
    if (esckill) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1, KC_KILL,
		     (char *)&ksv);
      nbytes = ksv.val;
    }
    else if (esckakutei) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		     KC_KAKUTEI, (char *)&ksv);
      nbytes = ksv.val;
    }
    else if (escclose) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		     KC_CLOSEUICONTEXT, (char *)&ksv);
      nbytes = ksv.val;
    }
#ifdef KC_DISCONNECTSERVER
    else if (escdisconnect) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		     KC_DISCONNECTSERVER, (char *)0);
      ksv.ks->length = 0;
      ksv.ks->info = 0;
      nbytes = 0;
    }
#endif /* KC_DISCONNECTSERVER */
  }
  else if (showMode && nbytes > 0 && key == 0x1b) {
    wchar_t foo[100];
    
    wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		   KC_QUERYMODE, (char *)foo);
    fprintf(stderr, "%ws\n", foo);
    return;
  }
  else if (detailMode && nbytes > 0 && key == 0x1b) {
    unsigned char detailfoo[10];
    
    if ( detailMode == 2 ) {
      detailfoo[2] = 0x99;
    }
    else {
      detailfoo[1] = 0x99;
    }
    jrKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		   KC_QUERYMODE, (char *)detailfoo);
    fprintf(stderr, "majorMode = %c\n", detailfoo[0]);
    if ( detailMode == 2 ) {
      fprintf(stderr, "minorMode = %c\n", detailfoo[1]);
      if(detailfoo[2] != 0x99) {
	fprintf(stderr, "error: over run!\n");
      }
      else {
	fprintf(stderr, "not over run\n");
      }
    }
    else {
      if(detailfoo[1] != 0x99) {
	fprintf(stderr, "error: over run!\n");
      }
      else {
	fprintf(stderr, "not over run\n");
      }
    }
    fprintf(stderr, "%s\n", "---------------");
    return;
  }
  else if (storeYomi && nbytes > 0 && key == 0x1b) {
    static int ntime = 0;
    wchar_t wcbuf[STRBUFSIZE], wcbuf2[STRBUFSIZE], wcbuf3[STRBUFSIZE];
    wcKanjiStatusWithValue ksv;

    ntime++;
    mbstowcs(wcbuf2, "あぷりけーしょんからすとあされたよみです。", STRBUFSIZE);
    if (ntime % 2) {
      mbstowcs(wcbuf3, "apurike-shonkarasutoasaretayomidesu.", STRBUFSIZE);
    }
    ksv.buffer = (wchar_t *)wcbuf;
    ksv.n_buffer = STRBUFSIZE;
    ksv.ks = ks;
    ks->echoStr = wcbuf2;
    ks->mode = (ntime % 2) ? wcbuf3 : (wchar_t *)0;
    notControl = 0; defineKanji = 0;

    wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1, KC_STOREYOMI,
		   (char *)&ksv);
#ifdef KC_DO
    if (storeThen) {
      ksv.buffer[0] = '@';
      ksv.val = storeThen;
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1, KC_DO, 
		     (char *)&ksv);
    }
#endif /* KC_DO */
		  
    nbytes = ksv.val;
  }

  if (numControl && nbytes > 0) {
    wchar_t wcbuf[STRBUFSIZE];
    wcKanjiStatusWithValue ksv;
    ksv.buffer = (wchar_t *)wcbuf;
    ksv.n_buffer = STRBUFSIZE;
    ksv.ks = ks;
    notControl = 0; defineKanji = 0;
    switch (key)
      {
      case '0':
	ksv.val = CANNA_MODE_AlphaMode;
	break;
      case '1':
	ksv.val = CANNA_MODE_HenkanMode;
	break;
      case '2':
	ksv.val = CANNA_MODE_HexMode;
	break;
      case '3':
	ksv.val = CANNA_MODE_BushuMode;
	break;
      case '4':
	ksv.val = CANNA_MODE_KigoMode;
	break;
      case '5':
	ksv.val = CANNA_MODE_ZenHiraKakuteiMode;
	break;
      case '6':
	ksv.val = CANNA_MODE_ZenKataKakuteiMode;
	break;
      case '7':
	ksv.val = CANNA_MODE_HanKataKakuteiMode;
	break;
      case '8':
	ksv.val = CANNA_MODE_ZenAlphaKakuteiMode;
	break;
      case '9':
	ksv.val = CANNA_MODE_HanAlphaKakuteiMode;
	break;
      case '-':
	{
	  wchar_t defines[64];
	  int len;

	  len = mbstowcs(defines, "登録魑魅魍魎", 64);
	  ksv.ks->echoStr = defines;
	  ksv.ks->length = len;
	  notControl = 1;
	  defineKanji = 1;
	}
	break;
      case '=':
	ksv.ks->echoStr = (wchar_t *)"";
	ksv.ks->length = 0;
	notControl = 1;
	defineKanji = 1;
	break;
      default:
	notControl = 1;
      }
    if (!notControl) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		     KC_CHANGEMODE, (char *)&ksv);
      nbytes = ksv.val;
    }
    else if (defineKanji) {
      wcKanjiControl((event->xany.window == wb[0].win) ? 0 : 1,
		     KC_DEFINEKANJI, (char *)&ksv);
      nbytes = ksv.val;
    }
  }
  return (notControl && !defineKanji);
}

checkGlineWidth(ks)
wcKanjiStatus *ks;
{
#ifdef HAVE_WCHAR_OPERATION
  int len = ks->gline.length;
  wchar_t *gline = ks->gline.line;
  int width = 0;
  static int counter = 0;

  for (i = 0 ; i < len ; i++) {
    if (isG0(gline[i])) {
      width ++;
    }
    else if (isG1(gline[i])) {
      width += 2;
    }
    else if (isG2(gline[i])) {
      width ++;
    }
    else {
      width += 2;
    }
  }
  if (width > max_width) {
    counter++;
    fprintf(stderr, "[%d] 幅が大きすぎます %d (設定値 %d)\n",
	    counter, width, max_width);
  }
#endif
  return 0;
}

/* checkIfFunctionalChar -- シフトキーのように無意味なキーかどうかの判別

  値:		１	意味がある
  		０	無意味(モディファイアキーなど)
*/

static int
checkIfFunctionalChar(event_struct, keysym, buffer_return, bytes_buffer)
XKeyEvent *event_struct;
KeySym keysym;
unsigned char *buffer_return;
int bytes_buffer;
{
  int functionalChar = 0;

  switch ((int)keysym)
    {
    case XK_KP_F1:
    case XK_KP_F2:
    case XK_KP_F3:
    case XK_KP_F4:
      *buffer_return = 	CANNA_KEY_PF1 + (int)keysym - (int)XK_KP_F1;
      functionalChar = 1;
      break;
    case XK_F1:
    case XK_F2:
    case XK_F3:
    case XK_F4:
    case XK_F5:
    case XK_F6:
    case XK_F7:
    case XK_F8:
    case XK_F9:
    case XK_F10:
    case XK_F11:
    case XK_F12:
    case XK_F13:
    case XK_F14:
    case XK_F15:
    case XK_F16:
      *buffer_return = CANNA_KEY_F1 + (int)keysym - (int)XK_F1;
      functionalChar = 1;
      break;
    case XK_Insert:
      *buffer_return = CANNA_KEY_Insert;
      functionalChar = 1;
      break;
    case XK_Prior:
      *buffer_return = CANNA_KEY_Rollup;
      functionalChar = 1;
      break;
    case XK_Next:
      *buffer_return = CANNA_KEY_Rolldown;
      functionalChar = 1;
      break;
    case XK_Muhenkan:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Nfer;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Nfer;
      else
	*buffer_return = CANNA_KEY_Nfer;
      functionalChar = 1;
      break;
    case XK_Kanji:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Xfer;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Xfer;
      else
	*buffer_return = CANNA_KEY_Xfer;
      functionalChar = 1;
      break;
    case XK_Up:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Up;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Up;
      else
	*buffer_return = CANNA_KEY_Up;
      functionalChar = 1;
      break;
    case XK_Down:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Down;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Down;
      else
	*buffer_return = CANNA_KEY_Down;
      functionalChar = 1;
      break;
    case XK_Right:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Right;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Right;
      else
	*buffer_return = CANNA_KEY_Right;
      functionalChar = 1;
      break;
    case XK_Left:
      if (event_struct->state & 4 /* control-shifted */)
	*buffer_return = CANNA_KEY_Cntrl_Left;
      else if (event_struct->state & 1 /* shifted */)
	*buffer_return = CANNA_KEY_Shift_Left;
      else
	*buffer_return = CANNA_KEY_Left;
      functionalChar = 1;
      break;
    case XK_Help:
      *buffer_return = CANNA_KEY_Help;
      functionalChar = 1;
      break;
    case XK_Home:
      *buffer_return = CANNA_KEY_Home;
      functionalChar = 1;
      break;
    }
  return functionalChar;
}

static wchar_t *
Wscpy(dst, src)
wchar_t *dst, *src;
{
  wchar_t *p = dst;

  while (*src) {
    *p++ = *src++;
  }
  return dst;
}

static wchar_t *
Wsncpy(dst, src, n)
wchar_t *dst, *src;
int n;
{
  wchar_t *p = dst;

  while (n-- > 0) {
    *p++ = *src++;
  }
  return dst;
}

static int
Wslen(src)
wchar_t *src;
{
  int i = 0;

  while (*src++)
    i++;
  
  return i;
}

static unsigned char wchar_type; /* ワイドキャラクタのタイプ(下を見よ) */

#define CANNA_WCTYPE_16 0  /* 16ビット表現 */
#define CANNA_WCTYPE_32 1  /* 32ビット表現 */
#define CANNA_WCTYPE_OT 99 /* その他の表現 */
/*
 WCinit() -- ワイドキャラクタとしてどれが使われているかを確認する

        この関数が呼び出されるまえに setlocale がなされていなければならない
 */

#define TYPE16A 0x0000a4a2
#define TYPE32A 0x30001222

static int
WCinit()
{
  unsigned char *a = (unsigned char *)"あ"; /* 0xa4a2 */
  wchar_t wc[24];

#ifdef HAVE_WCHAR_OPERATION
  if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
    /* 多分 setlocale がなされていない */
    setlocale(LC_CTYPE, "");
    if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
      setlocale(LC_CTYPE, JAPANESE_LOCALE);
      if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
	return -1;
      }
    }
  }
  switch (wc[0]) {
  case TYPE16A:
    wchar_type = CANNA_WCTYPE_16;
    break;
  case TYPE32A:
    wchar_type = CANNA_WCTYPE_32;
    break;
  default:
    wchar_type = CANNA_WCTYPE_OT;
    break;
  }
#else /* !HAVE_WCHAR_OPERATION */
# ifdef _WCHAR16

  wchar_type = CANNA_WCTYPE_16;

# else /* !_WCHAR16 */

  if (sizeof(wchar_t) == 2) {
    wchar_type = CANNA_WCTYPE_16;
  }
  else {
    wchar_type = CANNA_WCTYPE_32;
  }

# endif /* !_WCHAR16 */
#endif /* !HAVE_WCHAR_OPERATION */

  return 0;
}


int
MBstowcs(dest, src, destlen)
wchar_t *dest;
unsigned char *src;
int destlen;
{
  register int i, j;
  register unsigned char ec;
  static first_time = 1;

  if (first_time) {
    WCinit();
    first_time = 0;
  }

  if (wchar_type == CANNA_WCTYPE_16) {
    for (i = 0, j = 0 ; (ec = src[i]) && j < destlen ; i++) {
      if (ec & 0x80) {
	switch (ec) {
	case 0x8e: /* SS2 */
	  dest[j++] = (wchar_t)(0x80 | ((unsigned)src[++i] & 0x7f));
	  break;
	case 0x8f: /* SS3 */
	  dest[j++] = (wchar_t)(0x8000
				| (((unsigned)src[i + 1] & 0x7f) << 8)
				| ((unsigned)src[i + 2] & 0x7f));
	  i += 2;
	  break;
	default:
	  dest[j++] = (wchar_t)(0x8080 | (((unsigned)src[i] & 0x7f) << 8)
				| ((unsigned)src[i + 1] & 0x7f));
	  i++;
	  break;
	}
      }
      else {
	dest[j++] = (wchar_t)ec;
      }
    }
    if (j < destlen)
      dest[j] = (wchar_t)0;
    return j;
  }
  else if (wchar_type == CANNA_WCTYPE_32) {
    return 0; /* まだインプリしていない */
  }
  else {
    return 0;
  }
}

/* WWhatGPlain -- どのグラフィックプレーンの文字か？

   戻り値:
     0 : G0 ASCII
     1 : G1 漢字(JISX0208)
     2 : G2 半角カタカナ(JISX0201)
     3 : G3 外字(補助漢字 JISX0212)
 */

int
WWhatGPlain(wc)
wchar_t wc;
{
  static char plain[4] = {0, 2, 3, 1};

  switch (wchar_type) {
  case CANNA_WCTYPE_16:
    switch (((unsigned long)wc) & 0x8080) {
    case 0x0000:
      return 0;
    case 0x8080:
      return 1;
    case 0x0080:
      return 2;
    case 0x8000:
      return 3;
    }
    break;
  case CANNA_WCTYPE_32:
    return plain[(((unsigned long)wc) >> 28) & 3];
    break;
  default:
    return 0; /* どうしよう */
    break;
  }
  /* NOTREACHED */
}

int
isG0(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 0);
}

int
isG1(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 1);
}

int
isG2(wc)
wchar_t wc;
{
  return (WWhatGPlain(wc) == 2);
}

