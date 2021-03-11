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
static char rcs_id[] = "@(#) 102.1 $Id: uiutil.c,v 5.31 1994/04/12 12:55:44 kon Exp $";
#endif

#include "canna.h"
#include "patchlevel.h"

typedef struct {
  char *title;
  int func;
  int funcd;
} e_menuitem;

#define MENU_NEXT_MENU 0 /* エントリはメニューである */
#define MENU_FUNC_NUM  1 /* エントリは『かんな』の機能番号である */

#define MT_HELP   0
#define MT_SONOTA 1
#define MT_KIGO   2
#define MT_SERV   3
#define MT_TANGO  4
#define MT_HENKAN 5
#define MT_FILE   6

static e_menuitem e_helptable[] = { 
  {"記号入力",   MENU_NEXT_MENU, MT_KIGO},
  {"コード入力", MENU_FUNC_NUM,  CANNA_FN_HexMode},
  {"部首入力",   MENU_FUNC_NUM,  CANNA_FN_BushuMode},
  {"単語登録",   MENU_NEXT_MENU, MT_TANGO},
  {"環境設定",   MENU_NEXT_MENU, MT_SONOTA},
};

static e_menuitem e_uusonotatable[] = { 
  {"変換方式",       MENU_NEXT_MENU, MT_HENKAN},
  {"サーバ操作",     MENU_NEXT_MENU, MT_SERV},
  {"辞書マウント／アンマウント", MENU_FUNC_NUM, CANNA_FN_DicMountMode},
  {"学習状態表示",   MENU_FUNC_NUM,  CANNA_FN_ShowGakushu},
  {"バージョン表示", MENU_FUNC_NUM,  CANNA_FN_ShowVersion},
  {"ファイル表示",   MENU_NEXT_MENU, MT_FILE},
};

static e_menuitem e_uukigotable[] = {
  {"記号全般",     MENU_FUNC_NUM, CANNA_FN_KigouMode},
  {"ロシア文字",   MENU_FUNC_NUM, CANNA_FN_RussianMode},
  {"ギリシャ文字", MENU_FUNC_NUM, CANNA_FN_GreekMode},
  {"罫線",         MENU_FUNC_NUM, CANNA_FN_LineMode},
};

static e_menuitem e_uuservertable[] = {
  {"サーバの切り離し", MENU_FUNC_NUM, CANNA_FN_DisconnectServer},
  {"サーバの切り替え", MENU_FUNC_NUM, CANNA_FN_ChangeServerMode},
  {"サーバの表示",     MENU_FUNC_NUM, CANNA_FN_ShowServer},
};

static e_menuitem e_uutangotable[] = {
  {"単語登録", MENU_FUNC_NUM, CANNA_FN_DefineDicMode},
  {"単語削除", MENU_FUNC_NUM, CANNA_FN_DeleteDicMode},
  {"辞書マウント／アンマウント", MENU_FUNC_NUM, CANNA_FN_DicMountMode},
  };

static e_menuitem e_uuhenkantable[] = {
  {"連文節変換",   MENU_FUNC_NUM, CANNA_FN_EnterRenbunMode},
  {"逐次自動変換", MENU_FUNC_NUM, CANNA_FN_EnterChikujiMode},
};

static e_menuitem e_uufiletable[] = {
  {"ローマ字かな変換辞書", MENU_FUNC_NUM, CANNA_FN_ShowPhonogramFile},
  {"カスタマイズファイル", MENU_FUNC_NUM, CANNA_FN_ShowCannaFile},
};

#define numitems(x) ((sizeof(x)) / sizeof(e_menuitem))

static struct _e_menu {
  e_menuitem *mi;
  int ni;
} e_me[] = {                                    /* MT_ の順と合わせること */
  {e_helptable,     numitems(e_helptable)},     /* MT_HELP */
  {e_uusonotatable, numitems(e_uusonotatable)}, /* MT_SONOTA */
  {e_uukigotable,   numitems(e_uukigotable)},   /* MT_KIGO */
  {e_uuservertable, numitems(e_uuservertable)}, /* MT_SERV */
  {e_uutangotable,  numitems(e_uutangotable)},  /* MT_TANGO */
  {e_uuhenkantable, numitems(e_uuhenkantable)}, /* MT_HENKAN */
  {e_uufiletable,   numitems(e_uufiletable)},   /* MT_FILE */
};

#define N_BUILTIN_MENU (sizeof(e_me) / sizeof(struct _e_menu))

static menustruct *me[N_BUILTIN_MENU];

#define MBUFSIZE 512

void
freeMenu(m)
menustruct *m;
{
  free((char *)m->titles);
  free((char *)m->titledata);
  free((char *)m->body);
  free((char *)m);
}

menustruct *
allocMenu(n, nc)
int n, nc;
{
  wchar_t *wctab, **wcs;
  menuitem *menubody;
  menustruct *res;

  res = (menustruct *)malloc(sizeof(menustruct));
  if (res) {
    wctab = (wchar_t *)malloc(sizeof(wchar_t) * nc);
    if (wctab) {
      wcs = (wchar_t **)malloc(sizeof(wchar_t *) * n);
      if (wcs) {
	menubody = (menuitem *)malloc(sizeof(menuitem) * n);
	if (menubody) {
	  res->titles = wcs;
	  res->titledata = wctab;
	  res->body = menubody;
	  return res;
	}
	free((char *)wcs);
      }
      free((char *)wctab);
    }
    free((char *)res);
  }
  return (menustruct *)0;
}

static menustruct *
copystruct(eucmenu)
struct _e_menu *eucmenu;
{
  int i, nc, len, n = eucmenu->ni;
  e_menuitem *euctable = eucmenu->mi;
  menuitem *menubody;
  wchar_t *wp, **wpp;
  wchar_t buf[MBUFSIZE];
  menustruct *res;

  /* タイトルの文字数をカウント */
  for (i = 0, nc = 0 ; i < n ; i++) {
    len = MBstowcs(buf, euctable[i].title, MBUFSIZE);
    nc += len + 1;
  }

  res = allocMenu(n, nc);
  if (res) {
    menubody = res->body;
    /* タイトル文字をデータバッファにコピー */
    for (i = 0, wp = res->titledata, wpp = res->titles ; i < n ; i++) {
      len = MBstowcs(wp, euctable[i].title, MBUFSIZE);
      *wpp++ = wp;
      wp += len + 1;

      /* 構造体をコピーする */
      switch (euctable[i].func) {
      case MENU_NEXT_MENU:
	menubody[i].flag = MENU_MENU;
	menubody[i].u.fnum = euctable[i].funcd;
	break;
      case MENU_FUNC_NUM:
	menubody[i].flag = MENU_FUNC;
	menubody[i].u.fnum = euctable[i].funcd;
	break;
      }
    }
    res->nentries = n;
    res->modeid = CANNA_MODE_ExtendMode;
  }
  return res;
}

/*
 * menuitem型の全ての構造体を"unsigned char"から"wchar_t"に変換する
 */
int
initExtMenu()
{
  int i, j;

  for (i = 0 ; i < N_BUILTIN_MENU ; i++) {
    me[i] = copystruct(e_me + i);
    if (!me[i]) {
      for (j = 0 ; j < i ; j++) {
	freeMenu(me[j]);
      }
      return -1;
    }
  }
  for (i = 0 ; i < N_BUILTIN_MENU ; i++) {
    menustruct *m = me[i];
    for (j = 0 ; j < m->nentries ; j++) {
      if (m->body[j].flag == MENU_MENU) {
	m->body[j].u.menu_next = me[m->body[j].u.fnum];
      }
    }
  }

  return 0;
}

#undef numitems

void
finExtMenu()
{
  int i;
  for (i = 0 ; i < N_BUILTIN_MENU ; i++) {
    freeMenu(me[i]);
  }
}

static
makeUiUtilEchoStr(d)
uiContext d;
{
  ichiranContext ic = (ichiranContext)d->modec;

  d->kanji_status_return->echoStr = ic->allkouho[*(ic->curIkouho)];
  d->kanji_status_return->length = WStrlen(ic->allkouho[*(ic->curIkouho)]);
  d->kanji_status_return->revPos = 0;
  d->kanji_status_return->revLen = 0;

  return(0);
}

uiUtilIchiranTooSmall(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  makeUiUtilEchoStr(d);
}

static void
pushmenu(d, tab)
uiContext d;
menustruct *tab;
{
  tab->prev = d->prevMenu;
  d->prevMenu = tab;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * UIユーティリティの一覧表示(FirstLine)                                     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuflExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;
  menustruct *mtab, *ptab;
  menuitem *men;
  int cur;

  d->nbytes = 0;

  popCallback(d); /* 一覧を pop */

  fc = (forichiranContext)d->modec;
  cur = fc->curIkouho;
  if (fc->prevcurp) {
    *(fc->prevcurp) = cur;
  }
  men = fc->table->body + cur;
  ptab = fc->table;

  popForIchiranMode(d);
  popCallback(d);

  pushmenu(d, ptab);
  switch (men->flag) {
  case MENU_MENU:
    for (mtab = d->prevMenu ; mtab ; mtab = mtab->prev) {
      if (mtab == men->u.menu_next) {
	killmenu(d);
	jrKanjiError = "この項目のメニューは再帰的に選択されています";
	makeGLineMessageFromString(d, jrKanjiError);
	currentModeInfo(d);
	return 0;
      }
    }
    return showmenu(d, men->u.menu_next);
  case MENU_FUNC:
    if (men->u.fnum < 0) {
      jrKanjiError = "この項目は正しく定義されていません";
      killmenu(d);
      makeGLineMessageFromString(d, jrKanjiError);
      currentModeInfo(d);
      return 0;
    }
    else {
      d->more.todo = 1;
      d->more.fnum = men->u.fnum;
      /* 以下の２つは必要かどうか良く分からない */
      GlineClear(d);
      echostrClear(d);
      return 0;
    }
  }
  return NothingChangedWithBeep(d); /* ここには来ないはず */
}

prevMenuIfExist(d)
uiContext d;
{
  menustruct *m = d->prevMenu;

  if (m) {
    d->prevMenu = m->prev;
    d->kanji_status_return->info &= ~KanjiEmptyInfo;

    return showmenu(d, m);
  }
  else {
    return 0;
  }
}

static
uuflQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* 一覧を pop */

  popForIchiranMode(d);
  popCallback(d);
  currentModeInfo(d);

  return prevMenuIfExist(d);
}

/* cfuncdef

  UiUtilMode -- UIユーティリティモードになるときに呼ばれる。

 */
UiUtilMode(d)
uiContext d;
{
  return showmenu(d, me[MT_HELP]);
}

/*
 * newMenuInfo() -- 新しいメニュー情報の取得
 */

static menuinfo *
newMenuInfo(tab)
menustruct *tab;
{
  menuinfo *res;

  res = (menuinfo *)malloc(sizeof(menuinfo));
  if (res) {
    res->mstruct = tab;
    res->curnum = 0;
  }
  return res;
}

void
freeAllMenuInfo(p)
menuinfo *p;
{
  menuinfo *q;

  while (p) {
    q = p->next;
    free((char *)p);
    p = q;
  }
}

static menuinfo *
findMenuInfo(p, ms)
menuinfo *p;
menustruct *ms;
{
  while (p) {
    if (p->mstruct == ms) {
      return p;
    }
    p = p->next;
  }
  return (menuinfo *)0;
}

/*
 * showmenu -- メニューの表示
 *
 * 引数
 *   d         : uiContext
 *   table     : メニュー自身(menustruct へのポインタ)
 */

int
showmenu(d, table)
uiContext d;
menustruct *table;
{
  yomiContext yc = (yomiContext)d->modec;
  forichiranContext fc;
  ichiranContext ic;
  unsigned char inhibit = 0;
  int retval = 0;
  menuinfo *minfo;
  int *prevcurp = (int *)0;
  extern int makeGlineStatus(), HexkeySelect;


  if (yc->generalFlags & CANNA_YOMI_CHGMODE_INHIBITTED) {
    return NothingChangedWithBeep(d);
  }    

  minfo = findMenuInfo(d->minfo, table);
  if (!minfo) {
    minfo = newMenuInfo(table);
    if (minfo) {
      minfo->next = d->minfo;
      d->minfo = minfo;
    }
  }

  if (minfo) {
    prevcurp = &(minfo->curnum);
  }

  d->status = 0;

  if((retval = getForIchiranContext(d)) == NG)
     return(GLineNGReturn(d));
  fc = (forichiranContext)d->modec;

  fc->prevcurp = prevcurp;
  fc->table = table;

  /* selectOne を呼ぶための準備 */
  fc->allkouho = table->titles;
  fc->curIkouho = 0;
  if( !HexkeySelect )
    inhibit |= ((unsigned char)NUMBERING | (unsigned char)CHARINSERT);
  else
    inhibit |= (unsigned char)CHARINSERT;
  if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, table->nentries,
			 BANGOMAX, inhibit, 0, WITHOUT_LIST_CALLBACK,
			 0, uuflExitCatch,
			 uuflQuitCatch, uiUtilIchiranTooSmall)) == NG) {
    return(GLineNGReturnFI(d));
  }

  ic = (ichiranContext)d->modec;
  ic->majorMode = CANNA_MODE_ExtendMode;
  ic->minorMode = table->modeid;
  currentModeInfo(d);

  if (prevcurp) {
    *(ic->curIkouho) = *prevcurp;
  }
  else {
    *(ic->curIkouho) = 0;
  }

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return(retval);
  }

  makeGlineStatus(d);
  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}

