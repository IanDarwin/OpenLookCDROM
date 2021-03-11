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
static char rcs_id[] = "@(#) 102.1 $Id: keydef.c,v 5.10 1994/03/10 02:26:59 kon Exp $";
#endif /* lint */

#include "canna.h"
#include <canna/mfdef.h>
#include <canna/keydef.h>

extern KanjiModeRec alpha_mode, empty_mode, yomi_mode;
extern KanjiModeRec jishu_mode, ce_mode, cy_mode, cb_mode;
extern KanjiModeRec tankouho_mode, ichiran_mode, onoff_mode;
extern KanjiModeRec khal_mode, khkt_mode, kzal_mode, kzhr_mode, kzkt_mode;
extern KanjiModeRec kigo_mode;
extern KanjiModeRec tourokureibun_mode;
extern KanjiModeRec bunsetsu_mode;
extern KanjiModeRec cy_mode, cb_mode;

extern multiSequenceFunc();

static int undefineKeyfunc(), regist_key_hash(), copyMultiSequence();
static int freeMultiSequence();
static void clearAllFuncSequence(), clearAllKeySequence();

#define NONE 0
#define ACTHASHTABLESIZE 64
#define KEYHASHTABLESIZE 16

#define SINGLE 0
#define MULTI  1
#define OTHER  2

struct  seq_struct{
  unsigned char    *to_tbl;
  unsigned char    as_key;
  unsigned char    *kinou_seq;
  struct  seq_struct *next;
};

static struct seq_struct *seq_hash[ACTHASHTABLESIZE];

struct map{
  KanjiMode tbl;
  unsigned char key;
  KanjiMode  mode;
  struct map *next;
};

static struct map *otherMap[KEYHASHTABLESIZE];

static KanjiMode ModeTbl[CANNA_MODE_MAX_REAL_MODE] = {
  &alpha_mode,        /* AlphaMode          アルファベットモード         */
  &empty_mode,	      /* EmptyMode          読み入力がない状態           */
  &kigo_mode,	      /* KigoMode           候補一覧を表示している状態   */
  &yomi_mode,         /* YomiMode           読み入力している状態         */
  &jishu_mode,	      /* JishuMode          文字種変換している状態       */
  &tankouho_mode,     /* TankouhoMode       単一の候補を表示している状態 */
  &ichiran_mode,      /* IchiranMode        候補一覧を表示している状態   */
  &tourokureibun_mode, /* TourokuReibunMode 単語登録の例文表示状態       */
  &onoff_mode,        /* OnOffMode          On/Offの一覧の表示状態       */
  &bunsetsu_mode,     /* AdjustBunsetsuMode 文節伸縮モード               */
  &cy_mode,	      /* ChikujiYomiMode    逐次の時の読み部分           */
  &cb_mode,           /* ChikujiHenkanMode  逐次の時の変換の部分         */
};

unsigned char *actFromHash();
static void regist_act_hash();

static unsigned char *
duplicatekmap(kmap)
unsigned char *kmap;
{
  unsigned char *res;
  int i;

  res = (unsigned char *)calloc(256, sizeof(unsigned char));
  if (res) {
    for (i = 0 ; i < 256 ; i++) {
      res[i] = kmap[i];
    }
  }
  return res;
}

static unsigned char *defaultkeytables[CANNA_MODE_MAX_REAL_MODE];
static unsigned char defaultsharing[CANNA_MODE_MAX_REAL_MODE];
static unsigned char *defaultmap;
unsigned char *alphamap, *emptymap;

/* cfuncdef

  initKeyTables() -- キーテーブルを初期化する関数。

  デフォルトのキーテーブルを記録しておき、実使用のテーブルを デフォル
  トテーブルからコピーする処理を行う。

*/

initKeyTables()
{
  int i;
  unsigned char *tbl;
  extern unsigned char default_kmap[], alpha_kmap[], empty_kmap[];

  defaultmap = duplicatekmap(default_kmap);
  alphamap   = duplicatekmap(alpha_kmap);
  emptymap   = duplicatekmap(empty_kmap);

  for (i = 0 ; i < CANNA_MODE_MAX_REAL_MODE ; i++) {
    if (ModeTbl[i]) {
      defaultsharing[i] = ModeTbl[i]->flags;
      tbl = defaultkeytables[i] = ModeTbl[i]->keytbl;
      if (tbl == default_kmap) {
	ModeTbl[i]->keytbl = defaultmap;
      }
      else if (tbl == alpha_kmap) {
	ModeTbl[i]->keytbl = alphamap;
      }
      else if (tbl == empty_kmap) {
	ModeTbl[i]->keytbl = emptymap;
      }
    }
  }
}

void
restoreDefaultKeymaps()
{
  int i;

  for (i = 0 ; i < CANNA_MODE_MAX_REAL_MODE ; i++) {
    if (ModeTbl[i]) {
      if ( !(ModeTbl[i]->flags & CANNA_KANJIMODE_TABLE_SHARED) ) {
	free(ModeTbl[i]->keytbl);
      }
      ModeTbl[i]->keytbl = defaultkeytables[i];
      ModeTbl[i]->flags = defaultsharing[i];
    }
  }
  free(defaultmap);
  free(alphamap);
  free(emptymap);
  clearAllFuncSequence();
  clearAllKeySequence();
}


/* 
 * あるモードのキーに対して関数を割り当てる処理
 *
 */

/* 

  １６進の時は４文字目を入れた時のモードにも設定する。

 */

extern nothermodes;

changeKeyfunc(modenum, key, fnum, actbuff, keybuff)
int modenum;
int key;
unsigned char fnum;
unsigned char *actbuff, *keybuff;
{
  int i;
  unsigned char *p, *q;
  KanjiMode mode;
  newmode *nmode;

  /* ちょっと小細工 */
  if (modenum == CANNA_MODE_HenkanNyuryokuMode) {
    changeKeyfunc(CANNA_MODE_EmptyMode, key, fnum, actbuff, keybuff);
    modenum = CANNA_MODE_YomiMode;
  }

  if (modenum < 0) {
    return 0;
  }
  else if (modenum < CANNA_MODE_MAX_REAL_MODE) {
    mode = ModeTbl[modenum];
  }
  else if (modenum < CANNA_MODE_MAX_IMAGINARY_MODE) {
    return 0;
  }
  else if (modenum < CANNA_MODE_MAX_IMAGINARY_MODE + nothermodes) {
    nmode = findExtraKanjiMode(modenum);
    if (!nmode) {
      return 0;
    }
    else {
      mode = nmode->emode;
    }
  }
  else {
    return 0;
  }

  if (mode && mode->func(0/*dummy*/, mode, KEY_CHECK, 0/*dummy*/, fnum)) { 
    /* その機能がそのモードにおいて有効な機能であれば */
    if (mode->keytbl) { /* キーテーブルが存在すれば */
      /* これは絶対に存在するのでは？ */
      if (mode->flags & CANNA_KANJIMODE_TABLE_SHARED) {
	/* キーマップが他のモードと共有されているなら */
	p = (unsigned char *)calloc(256, sizeof(unsigned char));
	if(p)
	  bcopy(mode->keytbl, p, 256 * sizeof(unsigned char));
	for (i = 0; i < 256; i++) {
	  if (mode->keytbl[i] == CANNA_FN_FuncSequence) {
	    q = actFromHash(mode->keytbl,i);
	    regist_act_hash(p, i, q);
	  }
	  if (mode->keytbl[i] == CANNA_FN_UseOtherKeymap) {
	    debug_message("changeKeyfunc:テーブル移動:このときのキーは%d\n",i,0,0);
	    (void)copyMultiSequence(i, (KanjiMode)mode->keytbl, (KanjiMode)p);
	  }
	}
	mode->keytbl = p;
	mode->flags &= ~CANNA_KANJIMODE_TABLE_SHARED;
	if (modenum == CANNA_MODE_YomiMode &&
	    (ModeTbl[CANNA_MODE_ChikujiYomiMode]->flags
	     & CANNA_KANJIMODE_TABLE_SHARED)) {
	  ModeTbl[CANNA_MODE_ChikujiYomiMode]->keytbl = p;
	}
	else if (modenum == CANNA_MODE_TankouhoMode &&
		 (ModeTbl[CANNA_MODE_ChikujiTanMode]->flags
		  & CANNA_KANJIMODE_TABLE_SHARED)) {
	  ModeTbl[CANNA_MODE_ChikujiTanMode]->keytbl = p;
	}
      }
      if (key >= 0 && key < 255) {
	if (mode->keytbl[key] == CANNA_FN_UseOtherKeymap &&
	    fnum != CANNA_FN_UseOtherKeymap)
	  freeMultiSequence(key,(KanjiMode)mode->keytbl);
	mode->keytbl[key] = fnum;
	if (fnum == CANNA_FN_FuncSequence) {
	  regist_act_hash(mode->keytbl,key,actbuff);
	}
	if (fnum == CANNA_FN_UseOtherKeymap) {
	  regist_key_hash(mode->keytbl,keybuff,actbuff);
	}
      }
      else if (key == CANNA_KEY_Undefine) {
	undefineKeyfunc(mode->keytbl, fnum);
      }
    }
  }
  return 0;
}

static void
changeKeyOnSomeCondition(mode, key, fnum, actbuff, keybuff)
KanjiMode mode;
int key, fnum;
unsigned char *actbuff, *keybuff;
{
  if (mode && /* そのモードが存在するなら */
      mode->func(0/*dummy*/, mode, KEY_CHECK, 0/*dummy*/, fnum)) {
    /* 関数がそのモードで有効なら */
    if ( !(mode->flags & CANNA_KANJIMODE_TABLE_SHARED) ) {
      /* テーブルが共有されていないなら */
      if (mode->keytbl) { /* キーテーブルが存在すれば */
	if (mode->keytbl[key] == CANNA_FN_UseOtherKeymap &&
	    fnum != CANNA_FN_UseOtherKeymap)
	  freeMultiSequence(key,(KanjiMode)mode->keytbl);
	mode->keytbl[key] = fnum;
	if (fnum == CANNA_FN_FuncSequence) {
	  regist_act_hash(mode->keytbl,key,actbuff);
	}
	if (fnum == CANNA_FN_UseOtherKeymap) {
	  regist_key_hash(mode->keytbl,keybuff,actbuff);
	}
      }
    }
  }
}

/*
 * 全てのモードの、あるキーに対して関数を割り当てる処理
 *
 */

changeKeyfuncOfAll(key, fnum, actbuff, keybuff)
int key, fnum;
unsigned char *actbuff, *keybuff;
{
  extern extraFunc *extrafuncp;
  extraFunc *ep;
  KanjiMode mode;
  int i;

  if (key >= 0 && key < 255) {
    if (defaultmap[key] == CANNA_FN_UseOtherKeymap &&
	      fnum != CANNA_FN_UseOtherKeymap)
      freeMultiSequence(key,(KanjiMode)defaultmap);
    if (alphamap[key] == CANNA_FN_UseOtherKeymap &&
	      fnum != CANNA_FN_UseOtherKeymap)
      freeMultiSequence(key,(KanjiMode)alphamap);
    if (emptymap[key] == CANNA_FN_UseOtherKeymap &&
	      fnum != CANNA_FN_UseOtherKeymap)
      freeMultiSequence(key,(KanjiMode)emptymap);
    defaultmap[key] = fnum;
    alphamap[key]   = fnum;
    emptymap[key]   = fnum;
    if (fnum == CANNA_FN_FuncSequence) {
      regist_act_hash(defaultmap,key,actbuff);
      regist_act_hash(alphamap,key,actbuff);
      regist_act_hash(emptymap,key,actbuff);
    }
    if (fnum == CANNA_FN_UseOtherKeymap) {
      regist_key_hash(defaultmap,keybuff,actbuff);
      regist_key_hash(alphamap,keybuff,actbuff);
      regist_key_hash(emptymap,keybuff,actbuff);
    }
    for (i = 0 ; i < CANNA_MODE_MAX_REAL_MODE ; i++) {
      mode = ModeTbl[i];
      changeKeyOnSomeCondition(mode, key, fnum, actbuff, keybuff);
    }
    for (ep = extrafuncp ; ep ; ep = ep->next) {
      /* defmode での全てのモードに対してやる */
      if (ep->keyword == EXTRA_FUNC_DEFMODE) {
	changeKeyOnSomeCondition(ep->u.modeptr->emode, key, fnum,
				 actbuff, keybuff);
      }
    }
  }
  else if (key == CANNA_KEY_Undefine) {
    undefineKeyfunc(defaultmap, fnum);
    undefineKeyfunc(alphamap, fnum);
    undefineKeyfunc(emptymap, fnum);
    for (i = 0 ; i < CANNA_MODE_MAX_REAL_MODE ; i++) {
      mode = ModeTbl[i];
      if (mode && /* そのモードが存在するなら */
	  mode->func(0/*dummy*/, mode, KEY_CHECK, 0/*dummy*/, fnum)) {
	/* 関数がそのモードで有効なら */
	if ( !(mode->flags & CANNA_KANJIMODE_TABLE_SHARED) ) {
	  /* テーブルが共有されていないなら */
	  if (mode->keytbl) { /* キーテーブルが存在すれば */
	    undefineKeyfunc(mode->keytbl, fnum);
	  }
	}
      }
    }
  }
}

static
undefineKeyfunc(keytbl, fnum)
unsigned char *keytbl;
unsigned char fnum;
{
  int i;

  for (i = 0 ; i < ' ' ; i++) {
    if (keytbl[i] == fnum) {
      keytbl[i] = CANNA_FN_Undefined;
    }
  }
  for (i = ' ' ; i < 0x7f ; i++) {
    if (keytbl[i] == fnum) {
      keytbl[i] = CANNA_FN_FunctionalInsert;
    }
  }
  for (i = 0x7f ; i < 0xa0 ; i++) {
    if (keytbl[i] == fnum) {
      keytbl[i] = CANNA_FN_Undefined;
    }
  }
  for (i = 0xa0 ; i < 0xe0 ; i++) {
    if (keytbl[i] == fnum) {
      keytbl[i] = CANNA_FN_FunctionalInsert;
    }
  }
  for (i = 0xe0 ; i < 0x100 ; i++) {
    if (keytbl[i] == fnum) {
      keytbl[i] = CANNA_FN_Undefined;
    }
  }
}

static unsigned int
createHashKey(data1, data2, which_seq)
unsigned char *data1;
unsigned char data2;
int which_seq;
{
  unsigned int hashKey;

  hashKey = (int)(((POINTERINT)data1 + (POINTERINT)data2) % which_seq);
  return hashKey;
}

/* 機能シーケンスを割り出す */
unsigned char *
actFromHash(tbl_ptr, key)
unsigned char *tbl_ptr;
unsigned char key;
{
  unsigned int hashKey;
  struct seq_struct *p;

  hashKey = createHashKey(tbl_ptr, key, ACTHASHTABLESIZE);
  for (p = seq_hash[hashKey] ; p ; p = p->next) {
    if (p->to_tbl == tbl_ptr && p->as_key == key) {
      return p->kinou_seq;
    }
  }
  debug_message("actFromHash:キーシケンスをみつけられませんでした。\n",0,0,0);
  return NULL; /* 該当するキーシーケンスは存在しない */
}

/* ハッシュテーブルに登録 */
static void
regist_act_hash(tbl_ptr, key, buff)
unsigned char *tbl_ptr;
unsigned char key;
unsigned char *buff;
{
  unsigned int hashKey;
  struct seq_struct *p, **pp;

  hashKey = createHashKey(tbl_ptr, key, ACTHASHTABLESIZE);
  for (pp = &seq_hash[hashKey] ; (p = *pp) != (struct seq_struct *)0 ;
       pp = &(p->next)) {
    if (p->to_tbl == tbl_ptr && p->as_key == key) {
      if (p->kinou_seq)
	free(p->kinou_seq);
      p->kinou_seq = (unsigned char *)malloc(strlen((char *)buff)+1);
      if (p->kinou_seq)
	strcpy((char *)p->kinou_seq,(char *)buff);
      return;
    }
  }
  p = *pp = (struct seq_struct *)malloc(sizeof(struct seq_struct));
  if(p) {
    p->to_tbl = tbl_ptr;
    p->as_key = key;
    p->kinou_seq = (unsigned char *)malloc(strlen((char *)buff)+1);
    if(p->kinou_seq)
      strcpy((char *)p->kinou_seq,(char *)buff);
    p->next = NULL;
  }
}  

/* ハッシュテーブルから削除 */
static
remove_hash(tbl_ptr, key, which_seq)
unsigned char *tbl_ptr;
unsigned char key;
int which_seq;
{
  unsigned int hashKey;
  struct seq_struct *p, **pp;

  hashKey = createHashKey(tbl_ptr, key, which_seq);
  for (pp = &seq_hash[hashKey] ; (p = *pp) != (struct seq_struct *)0 ;
       pp = &(p->next)) {
    if (p->to_tbl == tbl_ptr && p->as_key == key) {
      *pp = p->next;
      free(p);
    }
  }
}

static void
freeChain(p)
struct seq_struct *p;
{
  struct seq_struct *nextp;

  while (p) {
    free(p->kinou_seq);
    nextp = p->next;
    free(p);
    p = nextp;
  }
}

static void
clearAllFuncSequence()
{
  int i;

  for (i = 0 ; i < ACTHASHTABLESIZE ; i++) {
    freeChain(seq_hash[i]);
    seq_hash[i] = 0;
  }
}

static
freeKeySeqMode(m)
KanjiMode m;
{
  if (m) {
    if (m->keytbl) {
      free(m->keytbl);
    }
    free(m);
  }
}

static
freeMap(m)
struct map *m;
{
  struct map *n;

  while (m) {
    freeKeySeqMode(m->mode);
    n = m->next;
    free(m);
    m = n;
  }
}

static void
clearAllKeySequence()
{
  int i;

  for (i = 0 ; i < KEYHASHTABLESIZE ; i++) {
    freeMap(otherMap[i]);
    otherMap[i] = 0;
  }
}

static
specialen(block)
unsigned char *block;
{
  int i;
  for (i = 0 ; block[i] != 255 ;) {
    i++;
  }
  debug_message("specialen:長さは%dどす。\n",i,0,0);
  return i;
}

static
to_write_act(depth,keysize,actsize,singleAct)
int depth;
int keysize;
int actsize;
unsigned singleAct;
{
  if (depth == (keysize -2)) {
    if (actsize > 1){
      debug_message("to_write_act:CANNA_FN_FuncSequenceです。\n",0,0,0);
      return CANNA_FN_FuncSequence;
    }
    if (actsize == 1) {
      debug_message("to_write_act:singleAct%dです。\n",singleAct,0,0);
      return (int)singleAct;
    }
    else { /* 有り得ない？ */
      return 0;
    }
  } else if (depth < (keysize -2)){
    debug_message("to_write_act:CANNA_FN_UseOtherKeymapです。\n",0,0,0);
    return CANNA_FN_UseOtherKeymap;
  }
  else { /* 有り得ない？ */
    return 0;
  }
}

static struct map *
regist_map(tbl, keybuff, actbuff, depth)
KanjiMode tbl;
unsigned char *keybuff;
unsigned char *actbuff;
int      depth;
{
  unsigned int hashKey;
  int sequencelen, keybuffsize, actbuffsize, offs;
  struct map *p,**pp;
  unsigned char *q, prevfunc;

  actbuffsize = strlen((char *)actbuff);
  keybuffsize = specialen(keybuff);
  hashKey = 
    createHashKey((unsigned char *)tbl, keybuff[depth], KEYHASHTABLESIZE);
  debug_message("regist_map:hashKey = %d です。\n",hashKey,0,0);
  for (pp = &otherMap[hashKey]; (p = *pp) != (struct map *)0 ;
       pp = &(p->next)) {
    if (p->key == keybuff[depth] && p->tbl == tbl) { 
      for (q = p->mode->keytbl; *q != 255; q += 2) {
	if (*q == keybuff[depth+1]) {  /* 既に同じキーが存在した。 */
	  ++q;
	  prevfunc = *q; /* そのキーの今までの機能を取っておく */
	  *q = to_write_act(depth,keybuffsize,actbuffsize,actbuff[0]);
	  if(prevfunc == CANNA_FN_UseOtherKeymap &&
	     *q != CANNA_FN_UseOtherKeymap) {
            freeMultiSequence(keybuff[depth + 1], p->mode);
          }
	  if (*q == CANNA_FN_FuncSequence) {
	    regist_act_hash((unsigned char *)p->mode, keybuff[depth+1],
			    actbuff);
	  }
	  debug_message("regist_map:既に同じキーが存在:q=%d\n",*q,0,0);
	  return p;
	}
      }
      /* そこまでの、キーの履歴はあったがこのキー:keybuff[depth +1]は初めて */
      sequencelen = specialen(p->mode->keytbl);
      offs = q - p->mode->keytbl;
      if (p->mode->keytbl) {
	p->mode->keytbl =
	  (unsigned char *)realloc(p->mode->keytbl,sequencelen +3);
	if (p->mode->keytbl) {
	  p->mode->keytbl[sequencelen] = keybuff[depth +1];
	  p->mode->keytbl[++sequencelen] =
	  to_write_act(depth,keybuffsize,actbuffsize,actbuff[0]);
	  p->mode->keytbl[++sequencelen] = (BYTE)-1;
	}
      }
      if (p->mode->keytbl[offs] == CANNA_FN_FuncSequence) {
	regist_act_hash((unsigned char *)p->mode, keybuff[depth+1], actbuff);
      }
      debug_message("regist_map:そこまでの、キーの履歴はあったがこのキー%uは初めて\n",
		    p->mode->keytbl[sequencelen-3],0,0);
      debug_message("regist_map:sequencelenは%dです。\n",sequencelen,0,0);
      return p;
    }
  }
  /* 過去の履歴は全てなしのはず、新規に作成 */
  p = *pp = (struct map *)malloc(sizeof(struct map));
  if (p) {
    p->tbl = tbl;
    p->key = keybuff[depth];
    p->mode = (KanjiMode)malloc(sizeof(KanjiModeRec));
    if (p->mode) {
      p->mode->func = multiSequenceFunc;
      p->mode->flags = 0;
      p->mode->keytbl = (unsigned char *)malloc(3);
      if (p->mode->keytbl) {
	p->mode->keytbl[0] = keybuff[depth +1];
	p->mode->keytbl[1] = to_write_act(depth,keybuffsize,actbuffsize,actbuff[0]);
	debug_message("regist_map:p->mode->keytbl[1]は%dです。\n",p->mode->keytbl[1],0,0);
	p->mode->keytbl[2] = (BYTE)-1;
      }
    }
    p->next = NULL;
    if (p->mode->keytbl[1] == CANNA_FN_FuncSequence) {
      regist_act_hash((unsigned char *)p->mode, keybuff[depth+1], actbuff);
    }
    return p;
  }
  return((struct map *) 0);
}

struct map *
mapFromHash(tbl, key, ppp)
KanjiMode tbl;
unsigned char key;
struct map ***ppp;
{
  unsigned int hashKey;
  struct map *p, **pp;

  hashKey = createHashKey((unsigned char *)tbl, key, KEYHASHTABLESIZE);
  debug_message("mapFromHash:hashKeyは%d\n",hashKey,0,0);
  for(pp = otherMap + hashKey ; (p = *pp) != (struct map *)0 ;
      pp = &(p->next)) {
    if (p->tbl == tbl && p->key == key) {
      debug_message("mapFromHash:mapがみつかりました。\n",0,0,0);
      if (ppp) {
	*ppp = pp;
      }
      return p;
    }
  }
  debug_message("mapFromHash:mapがみつかりません。\n",0,0,0);
  return NULL;
}

static
regist_key_hash(tbl_ptr,keybuff, actbuff)
unsigned char *tbl_ptr;
unsigned char *keybuff;
unsigned char *actbuff;
{
  struct map *map_ptr;
  int keybuffsize, i;

  keybuffsize = specialen(keybuff);
  map_ptr = regist_map((KanjiMode)tbl_ptr, keybuff, actbuff, 0);  
  for (i = 1; i <= (keybuffsize -2); i++) {
    map_ptr = regist_map(map_ptr->mode, keybuff, actbuff, i);
  }
  debug_message("regist_key_hash:keybuffsizeは%d　actbuffsizeは%d　iは%dです。\n",
		keybuffsize,strlen(actbuff),i);
}

static
int
copyMultiSequence(key, old_tbl, new_tbl)
     unsigned char	key;
     KanjiMode		old_tbl, new_tbl;
{
  unsigned char hashKey;
  unsigned char *old_sequence, *new_sequence;
  int i, sequencelen;
  struct map *p, **pp;
  struct map *old_map;

  old_map = mapFromHash(old_tbl, key, 0);
  old_sequence = old_map->mode->keytbl;
  sequencelen = specialen(old_sequence);
  hashKey = createHashKey((unsigned char *)new_tbl, key, KEYHASHTABLESIZE);
  for (pp = &otherMap[hashKey]; (p = *pp) != (struct map *)0 ;
       pp = &(p->next)) {
    if (p->key == key && p->tbl == new_tbl) { 
      return 0;
    }
  }
  p = *pp = (struct map *)malloc(sizeof(struct map));
  if (p) {
    p->tbl = new_tbl;
    p->key = key;
    p->mode = (KanjiMode)malloc(sizeof(KanjiModeRec));
    if (p->mode) {
      p->mode->func = multiSequenceFunc;
      p->mode->flags = 0;
      p->next = NULL;
      p->mode->keytbl = (unsigned char *)malloc(sequencelen+1);
      if (p->mode->keytbl) {
	for (i = 0, new_sequence = p->mode->keytbl; i <= sequencelen; i++) {
	  new_sequence[i] = old_sequence[i];
	  if (i%2 == 1) {
	    if (old_sequence[i] == CANNA_FN_UseOtherKeymap) {
	      if (copyMultiSequence(old_sequence[i-1],
				    old_map->mode, p->mode) < 0) {
		free((char *)p->mode->keytbl);
		free((char *)p->mode);
		free((char *)p);
		*pp = (struct map *)0;
		return(-1);
	      }		
	    } else if (old_sequence[i] == CANNA_FN_FuncSequence)
	      regist_act_hash((unsigned char *)p->mode, old_sequence[i-1],
			      actFromHash((unsigned char *)old_map->mode,
					  old_sequence[i-1]));
	  }
	}
	return 0;
      } else {
	free((char *)p->mode);
	free((char *)p);
	*pp = (struct map *)0;
	return(-1);
      }
    } else {
      free((char *)p);
      *pp = (struct map *)0;
      return(-1);
    }
  } else
    return(-1);
}

static
freeMultiSequence(key, tbl)
unsigned char key;
KanjiMode tbl;
{
  unsigned char *sequence;
  int i, sequencelen;
  struct map *map, **ptr;

  map = mapFromHash(tbl, key, &ptr);
  if (!map)
    return;
  *ptr = map->next;
  sequence = map->mode->keytbl;
  sequencelen = specialen(sequence);

  for (i = 0; i <= sequencelen; i++) {
    if (i%2 == 1) {
      if (sequence[i] == CANNA_FN_UseOtherKeymap)
	freeMultiSequence(sequence[i-1], map->mode);
      if (sequence[i] == CANNA_FN_FuncSequence)
	remove_hash((unsigned char *)map->mode, sequence[i-1],
		    ACTHASHTABLESIZE);
    }
  }
  debug_message("●key[%d]のmap以下をフリーしているぞ\n",key,0,0);
  if (map->mode && sequence)
    free(sequence);
  if (map->mode)
    free(map->mode);
  free(map);
}

askQuitKey(key)
unsigned key;
{
  if (defaultmap[key] == CANNA_FN_Quit) {
    return 1; /* 受け取ったkeyはquitだった。 */
  }
  return 0; /* 受け取ったkeyはquitでなかった。 */
}
