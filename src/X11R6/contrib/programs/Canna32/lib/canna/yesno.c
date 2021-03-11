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
static	char	rcs_id[] = "@(#) 102.1 $Id: yesno.c,v 5.3 1994/03/01 14:21:08 kon Exp $";
#endif  /* lint */

#include	<errno.h>
#include	"canna.h"

extern int  errno;

/* cfunc yesNoContext
 *
 * yesNoContext
 *
 */
static coreContext
newYesNoContext()
{
  coreContext ccxt;

  if((ccxt = (coreContext)malloc(sizeof(coreContextRec))) == NULL) {
    jrKanjiError = "malloc (newcoreContext) できませんでした";
    return(0);
  }
  ccxt->id = CORE_CONTEXT;

  return ccxt;
}

static void
freeYesNoContext(qc)
coreContext qc;
{
  free(qc);
}

/*
 * 候補一覧行を作る
 */
getYesNoContext(d,
	  everyTimeCallback, exitCallback, quitCallback, auxCallback)
uiContext d;
int (*everyTimeCallback)(), (*exitCallback)();
int (*quitCallback)(), (*auxCallback)();
{
  extern KanjiModeRec tourokureibun_mode;
  coreContext qc;
  int retval = 0;

  if(pushCallback(d, d->modec,
	everyTimeCallback, exitCallback, quitCallback, auxCallback) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((qc = newYesNoContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  qc->majorMode = d->majorMode;
  qc->minorMode = CANNA_MODE_HenkanMode;
  qc->next = d->modec;
  d->modec = (mode_context)qc;

  qc->prevMode = d->current_mode;
  d->current_mode = &tourokureibun_mode;

  return(retval);
}

static void
popYesNoMode(d)
uiContext d;
{
  coreContext qc = (coreContext)d->modec;

  d->modec = qc->next;
  d->current_mode = qc->prevMode;
  freeYesNoContext(qc);
}

#if DOYESNONOP
/*
  Nop を作ろうとしたが、 getYesNoContext を呼び出しているところで、
  everyTimeCallback を設定していないので、下の処理がうまく動かない
 */

static
YesNoNop(d)
uiContext	d;
{
  /* currentModeInfo でモード情報が必ず返るようにダミーのモードを入れておく */
  d->majorMode = d->minorMode = CANNA_MODE_AlphaMode;
  currentModeInfo(d);
  return 0;
}
#endif /* DOYESNONOP */

/*
 * EveryTimeCallback ... y/n 以外の文字が入力された
 * ExitCallback ...      y が入力された
 * quitCallback ...      quit が入力された
 * auxCallback ...       n が入力された
 */
static
YesNo(d)
uiContext	d;
{
  if((d->ch == 'y') || (d->ch == 'Y')) {
    popYesNoMode(d);
    d->status = EXIT_CALLBACK;
  } else if((d->ch == 'n') || (d->ch == 'N')) {
    popYesNoMode(d);
    d->status = AUX_CALLBACK;
  } else {
    /* d->status = EVERYTIME_CALLBACK; */
    return(NothingChangedWithBeep(d));
  }

  return(0);
}

static
YesNoQuit(d)
uiContext	d;
{
  int retval = 0;

  popYesNoMode(d);
  d->status = QUIT_CALLBACK;
  
  return(retval);
}

#include	"t_reimap.c"
