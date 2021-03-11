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
 *
 *  Author : Osamu Hata, NEC Corporation  (hata@d1.bs2.mt.nec.co.jp)
 *
 */

static	char	rcs_id[] = "@(#) 102.1 $Id: set.c,v 2.4 1994/01/19 02:33:52 hamada Exp $";

/*#include "mfdef.h"*/
#include <stdio.h>

#include "canna.h"

#ifndef NEWGEN
#include "iroha.h"
#endif /* NEWGEN */

#include "symbolname.h"
#include "set.h"

#define PREVK 3
#define NEXTK 2
#define YES 1
#define NO  0

#ifndef NEWGEN
extern is_icustom;
#endif /* NEWGEN */

char *
showChar(c)
int c;
{
  if (c < 0x20) {
    strcpy(returnKey, "C-");
    if (c == 0x00 || (c > 0x1a && c < 0x20 ))
      returnKey[2] = c + 0x40;
    else
      returnKey[2] = c + 0x60;
    returnKey[3] = '\0';
  }
  else if (c > ' ' && c <= '~' ) {
    returnKey[0] = c;
    returnKey[1] = '\0';
  }
/*  else if (c > 0xa0 && c < 0xdf) {
    returnKey[0] = 0x8e;
    returnKey[1] = c;
    returnKey[2] = '\0';
  }*/
#ifndef NEWGEN
  else if (is_icustom && c == 0x89)
    strcpy(returnKey, "Clear");
#endif /* NEWGEN */
  else if (c == 0x20)
    strcpy(returnKey, keyCharMap[0]);
  else if (c > 0x7e && c < 0x8c)
    strcpy(returnKey, keyCharMap[c -0x7f +1]);
  else if (c > 0x8f && c < 0x9c)
    strcpy(returnKey, keyCharMap[c -0x90 +14]);
  else if (c > 0xdf && c < 0xea)
    strcpy(returnKey, keyCharMap[c -0xe0 +26]);
  else if (c > 0xef && c < 0xf4)
    strcpy(returnKey, keyCharMap[c -0xf0 +36]);
  else if (c == 0xff)
    strcpy(returnKey, "undefine");
  else
    strcpy(returnKey, "undefine");
  return returnKey;
}

scc(key)
char *key;
{
  int length, Fkey, Ckey, Pkey, Skey;

  length = strlen(key);
  switch (length) {
  case 1:
    return (int)*key; /* 一文字 */
  case 2:
    if (*key == 'F') {
      Fkey = (int)*(key +1) + 175;
      return Fkey; /* F1 〜 F9 */
    }
    else {
      return 130; /* up */
    }
  case 3:
    if (*key == 'C') {
      Ckey = *(key +2);
      if (Ckey == 64) {
	Ckey = 0;
      }
      else if (Ckey < 96) {
	Ckey -=64; /* [ 〜 _ */
      }
      else {
	Ckey -=96; /* a 〜 z */
      }
      return Ckey; /* CTRL */
    }
    else if (*key == 'D') {
      return 127; /* DEL */
    }
    else if (*key == 'F') {
      return 233; /* F10 */
    }
    else if (*key == 'P') {
      Pkey = (int)*(key +2) + 191;
      return Pkey; /* PF1 〜 PF4 */
    }
  case 4:
    if (*key == 'C') {
      return 152; /* C-up */
    }
    else if (*key == 'D') {
      return 133; /* Down */
    }
    else if (*key == 'H') {
      if (*(key+1) == 'E') {
	return 138; /* HELP */
      }
      else if (*(key+1) == 'O') {
	return 137; /* Home */
      }
    }
    else if (*key == 'L') {
      return 131; /* Left */
    }
    else if (*key == 'N') {
      return 128; /* Nfer */
    }
    else if (*key == 'S') {
      return 146; /* S-up */
    }
    else if (*key == 'X') {
      return 129; /* Xfer */
    }
  case 5:
    if (*key == 'R') {
      return 132; /* Right */
    }
    else if (*key == 's') {
      return 32; /* space */
    }
    else if (*key == 'C') {
      return 137; /* Clear */
    }
  case 6:
    if (*key == 'I') {
      return 134; /* Insert */
    }
    else if (*key == 'R') {
      return 135; /* Rollup */
    }
    else if (*key == 'C') {
      if (*(key +2) == 'n') {
	Ckey = 150; /* nfer */
      }
      else if (*(key +2) == 'x') {
	Ckey = 151; /* xfer */
      }
      else if (*(key +2) == 'l') {
	Ckey = 153; /* left */
      }
      else if (*(key +2) == 'd') {
	Ckey = 155; /* right */
      }
      return Ckey; /* CTRL */
    }
    else if (*key == 'S') {
      if (*(key +2) == 'n') {
	Skey = 144; /* nfer */
      }
      else if (*(key +2) == 'x') {
	Skey = 145; /* xfer */
      }
      else if (*(key +2) == 'l') {
	Skey = 147; /* left */
      }
      else if (*(key +2) == 'd') {
	Skey = 149; /* right */
      }
      return Skey; /* SHIFT */
    }
  case 7:
    if (*key == 'C') {
      return 154; /* C-Right */
    }
    if (*key == 'S') {
      return 148; /* S-Right */
    }
  case 8:
    if (*key == 'R') {
      return 136; /* Rolldown */
    }
    else {
      return 255; /* undefine */
    }
  default:
    return 255;
  }
}

tilda(fileName)
char *fileName;
{
  char q[1024];

  if (fileName[0] == '~') {
    strcpy(q , fileName);
      if (fileName[1] == '/') {
	strcpy(fileName, (char *)getenv("HOME"));
	strcat(fileName, q + 1);
      }
  }
}

changeModeName(mode,string)
int   mode;
char *string;
{
  char *p;

  if (!string)
    null_mode[mode] = (char)1; /* モード表示文字列はNULLである。*/
  else {
    p = (char *)malloc(strlen(string) +1);
    strcpy(p, string);
    mode_mei[mode] =  p;
  }
}

initKeyFunc()
{
  allKey[0] = (char *)NULL;
  alphaKey[0] = (char *)NULL;
  yomiganaiKey[0] = (char *)NULL;
  yomiKey[0] = (char *)NULL;
  jishuKey[0] = (char *)NULL; 
  tankouhoKey[0] = (char *)NULL;
  ichiranKey[0] = (char *)NULL;
  zenHiraKey[0] = (char *)NULL;
  zenKataKey[0] = (char *)NULL;
  zenAlphaKey[0] = (char *)NULL;
  hanKataKey[0] = (char *)NULL;
  hanAlphaKey[0] = (char *)NULL;
  allFunc[0] = (char *)NULL;
  alphaFunc[0] = (char *)NULL;
  yomiganaiFunc[0] = (char *)NULL;
  yomiFunc[0] = (char *)NULL;
  jishuFunc[0] = (char *)NULL; 
  tankouhoFunc[0] = (char *)NULL;
  ichiranFunc[0] = (char *)NULL;
  zenHiraFunc[0] = (char *)NULL;
  zenKataFunc[0] = (char *)NULL;
  zenAlphaFunc[0] = (char *)NULL;
  hanKataFunc[0] = (char *)NULL;
  hanAlphaFunc[0] = (char *)NULL;
}

specialen(block)
unsigned char *block;
{
  int i;
  for (i = 0;block[i] != 255; i++);
  return i;
}

specpy(a, b)
unsigned char *a, *b;
{
  unsigned char *p;

  for(p = b; *b != 255; b++,a++)
    *a = *b;
  *a = 255;
  if (p == b)
    *++a = 255;
}

char *
copy_acts(acts)
unsigned char *acts;
{
  unsigned char *p;
  p = (unsigned char *)malloc(strlen((char *)acts) + 1);
  strcpy((char *)p, (char *)acts);
  return (char *)p;
}

char *
copy_keys(keys)
unsigned char *keys;
{
  unsigned char *p, *k1, *k2;

  p = (unsigned char *)malloc(specialen(keys) + 1);
  k2 = p;
  for (k1 = keys; *k1 != 255; k1++,k2++)
    *k2 = *k1;
  *k2 = 255;
  if (p == k2)
    *++k2 = 255;
  return (char *)p;
}

changeKeyfunc(mode, key, fnum, actbuff, keybuff)
int mode, key, fnum;
unsigned char *actbuff, *keybuff;
{
  switch(mode) {
  case 0 :
    alphaKey[NalphaKeyFunc] = copy_keys(keybuff);
    alphaFunc[NalphaKeyFunc++] = copy_acts(actbuff);
    alphaKey[NalphaKeyFunc] = (char *)NULL;
    alphaFunc[NalphaKeyFunc] = (char *)NULL;
    break;
  case 1 :
    yomiganaiKey[NyomiganaiKeyFunc] = copy_keys(keybuff);
    yomiganaiFunc[NyomiganaiKeyFunc++] = copy_acts(actbuff);
    yomiganaiKey[NyomiganaiKeyFunc] = (char *)NULL;
    yomiganaiFunc[NyomiganaiKeyFunc] = (char *)NULL;
    break;
  case 3 :
    zenHiraKey[NzenHiraKeyFunc] = copy_keys(keybuff);
    zenHiraFunc[NzenHiraKeyFunc++] = copy_acts(actbuff);
    zenHiraKey[NzenHiraKeyFunc] = (char *)NULL;
    zenHiraFunc[NzenHiraKeyFunc] = (char *)NULL;
    break;
  case 4 :
    zenKataKey[NzenKataKeyFunc] = copy_keys(keybuff);
    zenKataFunc[NzenKataKeyFunc++] = copy_acts(actbuff);
    zenKataKey[NzenKataKeyFunc] = (char *)NULL;
    zenKataFunc[NzenKataKeyFunc] = (char *)NULL;
    break;
  case 5 :
    hanKataKey[NhanKataKeyFunc] = copy_keys(keybuff);
    hanKataFunc[NhanKataKeyFunc++] = copy_acts(actbuff);
    hanKataKey[NhanKataKeyFunc] = (char *)NULL;
    hanKataFunc[NhanKataKeyFunc] = (char *)NULL;
    break;
  case 6 :
    zenAlphaKey[NzenAlphaKeyFunc] = copy_keys(keybuff);
    zenAlphaFunc[NzenAlphaKeyFunc++] = copy_acts(actbuff);
    zenAlphaKey[NzenAlphaKeyFunc] = (char *)NULL;
    zenAlphaFunc[NzenAlphaKeyFunc] = (char *)NULL;
    break;
  case 7 :
    hanAlphaKey[NhanAlphaKeyFunc] = copy_keys(keybuff);
    hanAlphaFunc[NhanAlphaKeyFunc++] = copy_acts(actbuff);
    hanAlphaKey[NhanAlphaKeyFunc] = (char *)NULL;
    hanAlphaFunc[NhanAlphaKeyFunc] = (char *)NULL;
    break;
  case 8 :
    yomiKey[NyomiKeyFunc] = copy_keys(keybuff);
    yomiFunc[NyomiKeyFunc++] = copy_acts(actbuff);
    yomiKey[NyomiKeyFunc] = (char *)NULL;
    yomiFunc[NyomiKeyFunc] = (char *)NULL;
    break;
  case 9 :
    jishuKey[NjishuKeyFunc] = copy_keys(keybuff);
    jishuFunc[NjishuKeyFunc++] = copy_acts(actbuff);
    jishuKey[NjishuKeyFunc] = (char *)NULL;
    jishuFunc[NjishuKeyFunc] = (char *)NULL;
    break;
  case 10 :
    tankouhoKey[NtankouhoKeyFunc] = copy_keys(keybuff);
    tankouhoFunc[NtankouhoKeyFunc++] = copy_acts(actbuff);
    tankouhoKey[NtankouhoKeyFunc] = (char *)NULL;
    tankouhoFunc[NtankouhoKeyFunc] = (char *)NULL;
    break;
  case 11 :
    ichiranKey[NichiranKeyFunc] = copy_keys(keybuff);
    ichiranFunc[NichiranKeyFunc++] = copy_acts(actbuff);
    ichiranKey[NichiranKeyFunc] = (char *)NULL;
    ichiranFunc[NichiranKeyFunc] = (char *)NULL;
    break;
  }
}

cchangeKeyfunc(mode, key, fnum, actbuff, keybuff)
int mode, key, fnum;
unsigned char *actbuff, *keybuff;
{
  switch(mode) {
  case 0 :
    alphaKey[NalphaKeyFunc] = copy_keys(keybuff);
    alphaFunc[NalphaKeyFunc++] = copy_acts(actbuff);
    alphaKey[NalphaKeyFunc] = (char *)NULL;
    alphaFunc[NalphaKeyFunc] = (char *)NULL;
    break;
  case 1 :
    yomiganaiKey[NyomiganaiKeyFunc] = copy_keys(keybuff);
    yomiganaiFunc[NyomiganaiKeyFunc++] = copy_acts(actbuff);
    yomiganaiKey[NyomiganaiKeyFunc] = (char *)NULL;
    yomiganaiFunc[NyomiganaiKeyFunc] = (char *)NULL;
    break;
  case 19 :
    zenHiraKey[NzenHiraKeyFunc] = copy_keys(keybuff);
    zenHiraFunc[NzenHiraKeyFunc++] = copy_acts(actbuff);
    zenHiraKey[NzenHiraKeyFunc] = (char *)NULL;
    zenHiraFunc[NzenHiraKeyFunc] = (char *)NULL;
    break;
  case 21 :
    zenKataKey[NzenKataKeyFunc] = copy_keys(keybuff);
    zenKataFunc[NzenKataKeyFunc++] = copy_acts(actbuff);
    zenKataKey[NzenKataKeyFunc] = (char *)NULL;
    zenKataFunc[NzenKataKeyFunc] = (char *)NULL;
    break;
  case 22 :
    hanKataKey[NhanKataKeyFunc] = copy_keys(keybuff);
    hanKataFunc[NhanKataKeyFunc++] = copy_acts(actbuff);
    hanKataKey[NhanKataKeyFunc] = (char *)NULL;
    hanKataFunc[NhanKataKeyFunc] = (char *)NULL;
    break;
  case 23 :
    zenAlphaKey[NzenAlphaKeyFunc] = copy_keys(keybuff);
    zenAlphaFunc[NzenAlphaKeyFunc++] = copy_acts(actbuff);
    zenAlphaKey[NzenAlphaKeyFunc] = (char *)NULL;
    zenAlphaFunc[NzenAlphaKeyFunc] = (char *)NULL;
    break;
  case 24 :
    hanAlphaKey[NhanAlphaKeyFunc] = copy_keys(keybuff);
    hanAlphaFunc[NhanAlphaKeyFunc++] = copy_acts(actbuff);
    hanAlphaKey[NhanAlphaKeyFunc] = (char *)NULL;
    hanAlphaFunc[NhanAlphaKeyFunc] = (char *)NULL;
    break;
  case 3 :
    yomiKey[NyomiKeyFunc] = copy_keys(keybuff);
    yomiFunc[NyomiKeyFunc++] = copy_acts(actbuff);
    yomiKey[NyomiKeyFunc] = (char *)NULL;
    yomiFunc[NyomiKeyFunc] = (char *)NULL;
    break;
  case 4 :
    jishuKey[NjishuKeyFunc] = copy_keys(keybuff);
    jishuFunc[NjishuKeyFunc++] = copy_acts(actbuff);
    jishuKey[NjishuKeyFunc] = (char *)NULL;
    jishuFunc[NjishuKeyFunc] = (char *)NULL;
    break;
  case 5 :
    tankouhoKey[NtankouhoKeyFunc] = copy_keys(keybuff);
    tankouhoFunc[NtankouhoKeyFunc++] = copy_acts(actbuff);
    tankouhoKey[NtankouhoKeyFunc] = (char *)NULL;
    tankouhoFunc[NtankouhoKeyFunc] = (char *)NULL;
    break;
  case 6 :
    ichiranKey[NichiranKeyFunc] = copy_keys(keybuff);
    ichiranFunc[NichiranKeyFunc++] = copy_acts(actbuff);
    ichiranKey[NichiranKeyFunc] = (char *)NULL;
    ichiranFunc[NichiranKeyFunc] = (char *)NULL;
    break;
  }
}


changeKeyfuncOfAll(key, fnum, actbuff, keybuff)
int key, fnum;
unsigned char *actbuff, *keybuff;
{
    allKey[NallKeyFunc] = copy_keys(keybuff);
    allFunc[NallKeyFunc++] = copy_acts(actbuff);
    allKey[NallKeyFunc] = (char *)NULL;
    allFunc[NallKeyFunc] = (char *)NULL;
}

char *string;

append_dic(loc, dic)
int loc;
char *dic;
{
  switch(loc) {
  case 0 : /* ローマ字かな変換テーブル */
    if (RomkanaTable)
      free(RomkanaTable);
    RomkanaTable = (char *)malloc(64);
    strncpy(RomkanaTable, dic, 63);
    return ;
  case 1 : /* システム辞書 */
    string = (char *)malloc(64);
    strncpy(string, dic, 63);
    kanjidicname[nkanjidics++] = string;
    kanjidicname[nkanjidics] = NULL;
    return;
  case 2 : /* 部首変換辞書 */
    string = (char *)malloc(64);
    strncpy(string, dic, 63);
    bushudicname[nbushudics++] = string;
    bushudicname[nbushudics] = NULL;
    return;
  case 3 : /* ユーザ辞書 */
    string = (char *)malloc(64);
    strncpy(string, dic, 63);
    userdicname[nuserdics++] = string;
    userdicname[nuserdics] = NULL;
    return ;
  case 4 : /* 連語変換 */
    if (RengoGakushu)
      free (RengoGakushu);
    RengoGakushu = (char *)malloc(64);
    strncpy(RengoGakushu, dic, 63);
    return;
  }
}

delete_dic(loc, num)
int loc, num;
{
  switch(loc) {
  case 0 : /* ローマ字かな変換テーブル */
    if (RomkanaTable)
      free(RomkanaTable);
    RomkanaTable = NULL;
    return ;
  case 1 : /* システム辞書 */
    for(;kanjidicname[num]; num++)
      kanjidicname[num] = kanjidicname[num + 1];
    if (nkanjidics)
      nkanjidics--;
    return;
  case 2 : /* 部首変換辞書 */
    for(;bushudicname[num]; num++)
      bushudicname[num] = bushudicname[num + 1];
    if (nbushudics)
      nbushudics--;
    return;
  case 3 : /* ユーザ辞書 */
    for(;userdicname[num]; num++)
      userdicname[num] = userdicname[num + 1];
    if (nuserdics)
      nuserdics--;
    return;
  case 4 : /* 連語変換 */
    RengoGakushu = NULL;
    return;
  }
}

etc_action(kinou,which)
int kinou, which;
{
  switch(kinou) {
  case 0 :  /* initialMode */
    break;
  case 1 :  /* cusorWrap */
    CursorWrap = which;
    break;
  case 2 :  /* numericalKeySelect */
    HexkeySelect = which;
    break;
  case 3 :  /* selectDirect */
    SelectDirect = which;
    break;
  case 4 :  /* bunsetsuKugiri */
    BunsetsuKugiri = which;
    break;
  case 5 :  /* characterBaseMove*/
    ChBasedMove = which;
    break;
  case 6 :  /* reverseWidely */
    ReverseWidely = which;
    break;
  case 7 :  /* quitIfEndOfIchiran */
    QuitIchiranIfEnd = which;
    break;
  case 8 :  /* breakIntoRoman */
    BreakIntoRoman = which;
    break;
  case 9 :  /* gakushu */
    Gakushu = which;
    break;
  case 10 : /* stayAfterValidate */
    stayAfterValidate = which;
    break;
  case 11 : /* kakuteiIfEndOfBunsetsu */
    kakuteiIfEndOfBunsetsu = which;
    break;
  case 12 : /* gramaticalQuestion */
    gramaticalQuestion = which;
    break;
  case 13 : /* nHenkanForIchiran */
    break;
  case 14 : /* kouhoCount */
    kCount = which;
    break;
  case 15 : /* kojin */
    kojin = which;
    break;
  case 16 : /* auto */
    chikuji = which;
    break;
  case 17 : /* nKouhoBunsetsu */
    break;
  case 18 : /* abndonIllegalPhono */
    abandonIllegalPhono = which;
    break;
  case 19 : /* hexDirect */
    hexCharacterDefiningStyle = which;
    break;
  case 20 : /* allowNextInput */
    allowNextInput = which;
    break;
  case 21 : /* index-hankaku */
    indexhankaku = which;
    break;
  case 22 : /* ignore-case */
    ignorecase = which;
    break;
  case 23 : /* romaji-yuusen */
    romajiyuusen = which;
    break;
  case 24 : /* auto-sync */
    autosync = which;
    break;
  case 25 : /* n-key-to-disconnect */
    break;
  case 26 : /* quickly-escape-from-kigo-input */
    quicklyescape = which;
    break;
  }
}
