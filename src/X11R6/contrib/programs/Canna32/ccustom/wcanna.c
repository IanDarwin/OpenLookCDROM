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

static	char	rcs_id[] = "@(#) 102.1 $Id: wcanna.c,v 2.4 1994/01/19 02:34:00 hamada Exp $";

#include <stdio.h>
#include "mfdef.h"
#include "keydef.h"
#include "symbolname.h"

#define ON   1
#define OFF -1
#define MID  2

extern char *kanjidicname[], *userdicname[],  *bushudicname[], *localdicname[]; 
extern char *cfuncList[];

extern int  nkanjidics, nuserdics, nbushudics, nlocaldics;
extern char *RomkanaTable, *RengoGakushu[], *KatakanaGakushu[];
extern int InitialMode, CursorWrap, SelectDirect, HexkeySelect, BunsetsuKugiri;
extern int ChBasedMove, ReverseWidely, Gakushu, QuitIchiranIfEnd;
extern int kakuteiIfEndOfBunsetsu, stayAfterValidate, BreakIntoRoman;
extern int kouho_threshold, gramaticalQuestion;
extern int kCount,kojin,chikuji,nKouhoBunsetsu;
extern int abandonIllegalPhono, hexCharacterDefiningStyle,allowNextInput;
extern int indexhankaku,ignorecase,romajiyuusen,autosync,nkeysuu,quicklyescape;
extern char *mode_mei[], null_mode[];
extern char *mode_ichiran2[], *mode_ichiran3[];
extern char *allKey[], *alphaKey[], *yomiganaiKey[];
extern char *yomiKey[], *jishuKey[], *tankouhoKey[];
extern char *ichiranKey[], *zenHiraKey[], *zenKataKey[];
extern char *zenAlphaKey[], *hanKataKey[], *hanAlphaKey[];
extern char *allFunc[], *alphaFunc[], *yomiganaiFunc[];
extern char *yomiFunc[], *jishuFunc[], *tankouhoFunc[];
extern char *ichiranFunc[], *zenHiraFunc[], *zenKataFunc[];
extern char *zenAlphaFunc[], *hanKataFunc[], *hanAlphaFunc[];
extern int NallKeyFunc, NalphaKeyFunc, NyomiganaiKeyFunc, NyomiKeyFunc;
extern int NjishuKeyFunc, NtankouhoKeyFunc,  NichiranKeyFunc;
extern int NzenHiraKeyFunc, NzenKataKeyFunc, NzenAlphaKeyFunc;
extern int NhanKataKeyFunc, NhanAlphaKeyFunc;

#ifdef NEWGEN
init_mode_mei()
{
  int i;
  for (i = 0; i < 40; i++) {
    null_mode[i] = 0;
    mode_mei[i] = (char *)NULL;
  }
}

free_mode_mei()
{
  int i;
  for (i = 0; i < 40; i++) {
    if (mode_mei[i]) {
      free(mode_mei[i]);
    }
  }
}
#endif /* NEWGEN */

char *
toTnil(suuji)
int suuji;
{
  if (suuji == ON)
    return "t";
  else if (suuji == OFF)
    return "nil";
  else
    return " ";
}

static void
printkey(f, key)
FILE *f;
unsigned char key;
{
  if ((unsigned)('A' - '@') <= (unsigned)key &&
      (unsigned)key <= (unsigned)('Z' - '@')) {
    fprintf(f, "\\C-%c", key + 'a' - 'A' + '@');
  }
  else if (key < ' ') {
    fprintf(f, "\\C-%c", key + '@');
  }
  else if (key <= '~') {
    fprintf(f, "%c", key);
  }
  else if (key == 0x7f) {
    fprintf(f, "\\Delete");
  }
  else {
    switch (key) {
    case CANNA_KEY_Nfer:
      fprintf(f, "\\Nfer");
      break;
    case CANNA_KEY_Xfer:
      fprintf(f, "\\Xfer");
      break;
    case CANNA_KEY_Up:
      fprintf(f, "\\Up");
      break;
    case CANNA_KEY_Left:
      fprintf(f, "\\Left");
      break;
    case CANNA_KEY_Right:
      fprintf(f, "\\Right");
      break;
    case CANNA_KEY_Down:
      fprintf(f, "\\Down");
      break;
    case CANNA_KEY_Insert:
      fprintf(f, "\\Insert");
      break;
    case CANNA_KEY_Rollup:
      fprintf(f, "\\Rollup");
      break;
    case CANNA_KEY_Rolldown:
      fprintf(f, "\\Rolldown");
      break;
    case CANNA_KEY_Home:
      fprintf(f, "\\Home");
      break;
    case CANNA_KEY_Help:
      fprintf(f, "\\Help");
      break;
    case CANNA_KEY_KP_Key:
      fprintf(f, "\\Key");
      break;
    case CANNA_KEY_Shift_Nfer:
      fprintf(f, "\\S-Nfer");
      break;
    case CANNA_KEY_Shift_Xfer:
      fprintf(f, "\\S-Xfer");
      break;
    case CANNA_KEY_Shift_Up:
      fprintf(f, "\\S-Up");
      break;
    case CANNA_KEY_Shift_Left:
      fprintf(f, "\\S-Left");
      break;
    case CANNA_KEY_Shift_Right:
      fprintf(f, "\\S-Right");
      break;
    case CANNA_KEY_Shift_Down:
      fprintf(f, "\\S-Down");
      break;
    case CANNA_KEY_Cntrl_Nfer:
      fprintf(f, "\\C-Nfer");
      break;
    case CANNA_KEY_Cntrl_Xfer:
      fprintf(f, "\\C-Xfer");
      break;
    case CANNA_KEY_Cntrl_Up:
      fprintf(f, "\\C-Up");
      break;
    case CANNA_KEY_Cntrl_Left:
      fprintf(f, "\\C-Left");
      break;
    case CANNA_KEY_Cntrl_Right:
      fprintf(f,"\\C-Right");
      break;
    case CANNA_KEY_Cntrl_Down:
      fprintf(f, "\\C-Down");
      break;
    case CANNA_KEY_F1:
      fprintf(f, "\\F1");
      break;
    case CANNA_KEY_F2:
      fprintf(f, "\\F2");
      break;
    case CANNA_KEY_F3:
      fprintf(f, "\\F3");
      break;
    case CANNA_KEY_F4:
      fprintf(f, "\\F4");
      break;
    case CANNA_KEY_F5:
      fprintf(f, "\\F5");
      break;
    case CANNA_KEY_F6:
      fprintf(f, "\\F6");
      break;
    case CANNA_KEY_F7:
      fprintf(f, "\\F7");
      break;
    case CANNA_KEY_F8:
      fprintf(f, "\\F8");
      break;
    case CANNA_KEY_F9:
      fprintf(f, "\\F9");
      break;
    case CANNA_KEY_F10:
      fprintf(f, "\\F10");
      break;
    case CANNA_KEY_PF1:
      fprintf(f, "\\Pf1");
      break;
    case CANNA_KEY_PF2:
      fprintf(f, "\\Pf2");
      break;
    case CANNA_KEY_PF3:
      fprintf(f, "\\Pf3");
      break;
    case CANNA_KEY_PF4:
      fprintf(f, "\\Pf4");
      break;
    case CANNA_KEY_PF5:
      fprintf(f, "\\Pf5");
      break;
    case CANNA_KEY_PF6:
      fprintf(f, "\\Pf6");
      break;
    case CANNA_KEY_PF7:
      fprintf(f, "\\Pf7");
      break;
    case CANNA_KEY_PF8:
      fprintf(f, "\\Pf8");
      break;
    case CANNA_KEY_PF9:
      fprintf(f, "\\Pf9");
      break;
    case CANNA_KEY_PF10:
      fprintf(f, "\\Pf10");
      break;
    case CANNA_KEY_Undefine:
      fprintf(f, "\\Undefine"); /* おかしい！ */
      break;
    }
  }
}

static void
chkeyfn(f, actbuff, keybuff)
FILE *f;
unsigned char *actbuff, *keybuff;
{
  unsigned char *p;

  if (actbuff[1]) {
    fprintf(f, " (sequence");
    for (p = actbuff ; *p ; p++) {
      fprintf(f, " \'");
      fprintf(f, "%s", cfuncList[*p]);
    }
    putc(')', f);
  }
  else if (keybuff[1] != 255) {
    fprintf(f, " \'");
    fprintf(f, "%s", cfuncList[actbuff[0]]);
  }
  else {
    fprintf(f, " \'");
    fprintf(f, "%s", cfuncList[actbuff[0]]);
  }
  fprintf(f, ")");
}

static void
chkeyrest(f, actbuff, keybuff)
FILE *f;
unsigned char *actbuff, *keybuff;
{
  unsigned char *p;

  if (keybuff[1] != 255) { /* キーシーケンスなら */
    for (p = keybuff ; *p != 255 && *p != CANNA_KEY_Undefine ; p++) {
      printkey(f, *p);
    }
  }
  else {
    printkey(f, keybuff[0]);
  }
  fprintf(f, "\"");

  chkeyfn(f, actbuff, keybuff);
}

char *
print_acbuff(f, acts, keys)
FILE *f;
unsigned char *acts, *keys;
{
  if (keys[0] == CANNA_KEY_Undefine) {
    fprintf(f, "(%s", S_GUnbindKey);
    chkeyfn(f, acts, keys);
  }
  else {
    fprintf(f, "(%s \"", S_GSetKey);
    chkeyrest(f, acts, keys);
  }
  return 0;
}

print_cbuff(f, acts, keys)
FILE *f;
unsigned char *acts, *keys;
{
  /* (set-key mode "keysequence" 'function)
     (set-key mode "keysequence" (sequence 'function1 'function2 ...)) */

  if (keys[0] == CANNA_KEY_Undefine) {
    fprintf(f, "  (%s mode", S_UnbindKey);
    chkeyfn(f, acts, keys);
  }
  else {
    fprintf(f, "  (%s mode \"", S_SetKey);
    chkeyrest(f, acts, keys);
  }
  return 0;
}

static void
mode_style(f, mode, display, isnull)
FILE *f;
char *mode, *display, *isnull;
{
  if (!isnull) {
    if (display) {
      fprintf(f, "(set-mode-display '%s \"%s\")\n", mode, display);
    }
  }
  else {
    fprintf(f, "(set-mode-display '%s nil)\n", mode);
  }
}

write_canna(f)
FILE *f;
{
  int i;
  char romkana[255];

  if (RomkanaTable) {
    fprintf(f, "(setq %s \"%s\")\n\n", S_VA_RomkanaTable, RomkanaTable);
    strcpy(romkana, RomkanaTable);
  }
  else {
    strcpy(romkana, "default.kp");
  }

  fprintf(f, ";;  互換性確保のためのモードを定義します\n");
  fprintf(f, "(defmode %s \"%s\" %s)\n", "zen-hira-kakutei-mode  \"<全あ>\"",
	 romkana, "'(hiragana zenkaku kakutei)");
  fprintf(f, "(defmode %s \"%s\" %s)\n", "zen-kata-kakutei-mode  \"<全ア>\"",
	 romkana, "'(katakana zenkaku kakutei)"); 
  fprintf(f, "(defmode %s \"%s\" %s)\n", "zen-alpha-kakutei-mode \"<全英>\"",
	 romkana, "'(romaji zenkaku kakutei)");
  fprintf(f, "(defmode %s \"%s\" %s)\n", "han-kata-kakutei-mode  \"<半ア>\"",
	 romkana, "'(katakana hankaku kakutei)");
  fprintf(f, "(defmode %s \"%s\" %s)\n\n", "han-alpha-kakutei-mode \"<半英>\"",
	 romkana, "'(romaji hankaku kakutei)");

  if (mode_ichiran2[InitialMode]) {
    fprintf(f, "(%s '(%s %s))\n\n", S_SetInitFunc, S_FN_JapaneseMode,
	    mode_ichiran2[InitialMode]);
  }

  fprintf(f, "(%s\n", S_FN_UseDictionary);
  for (i = 0;i < nkanjidics && *kanjidicname[i];i++)
    fprintf(f, "  \"%s\"\n", kanjidicname[i]);
  for (i = 0;i < nbushudics && *bushudicname[i];i++)
    fprintf(f, "  :bushu \"%s\"\n", bushudicname[i]);
  for (i = 0;i < nuserdics && *userdicname[i];i++)
    fprintf(f, "  :user  \"%s\"\n", userdicname[i]);
/*
  if (RengoGakushu) {
    fprintf(f, "  :rengo \"%s\"\n", RengoGakushu);
  }
*/
  fprintf(f, "  )\n");

  fprintf(f, "\n(setq %s %s)\n", S_VA_CursorWrap, toTnil(CursorWrap));
  fprintf(f, "(setq %s %s)\n", S_VA_SelectDirect, toTnil(SelectDirect));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_NumericalKeySelect, toTnil(HexkeySelect));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_BunsetsuKugiri, toTnil(BunsetsuKugiri));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_CharacterBasedMove, toTnil(ChBasedMove));
  fprintf(f, "(setq %s %s)\n", S_VA_ReverseWidely, toTnil(ReverseWidely));
  fprintf(f, "(setq %s %s)\n", S_VA_Gakushu, toTnil(Gakushu));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_QuitIfEOIchiran, toTnil(QuitIchiranIfEnd));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_KakuteiIfEOBunsetsu, toTnil(kakuteiIfEndOfBunsetsu));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_StayAfterValidate, toTnil(stayAfterValidate));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_BreakIntoRoman, toTnil(BreakIntoRoman));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_GramaticalQuestion, toTnil(gramaticalQuestion));
  fprintf(f, "(setq %s %d)\n", S_VA_NHenkanForIchiran, kouho_threshold);
  fprintf(f, "(setq %s %s)\n", S_VA_KouhoCount, toTnil(kCount));
  fprintf(f, "(setq %s %s)\n", S_VA_Kojin, toTnil(kojin));
  fprintf(f, "(setq %s %s)\n", S_VA_Auto, toTnil(chikuji));
  fprintf(f, "(setq %s %d)\n", S_VA_nKouhoBunsetsu, nKouhoBunsetsu);
  fprintf(f, "(setq %s %s)\n", S_VA_Abandon, toTnil(abandonIllegalPhono));
  fprintf(f, "(setq %s %s)\n",
	  S_VA_HexDirect, toTnil(hexCharacterDefiningStyle));
  fprintf(f, "(setq %s %s)\n", S_VA_AllowNextInput, toTnil(allowNextInput));
  fprintf(f, "(setq %s %s)\n", S_VA_IndexHankaku, toTnil(indexhankaku));
  fprintf(f, "(setq %s %s)\n", S_VA_ignoreCase, toTnil(ignorecase));
  fprintf(f, "(setq %s %s)\n", S_VA_RomajiYuusen, toTnil(romajiyuusen));
  fprintf(f, "(setq %s %s)\n", S_VA_AutoSync, toTnil(autosync));
  fprintf(f, "(setq %s %d)\n", S_VA_nDisconnectServer,nkeysuu );
  fprintf(f, "(setq %s %s)\n", S_VA_QuicklyEscape,toTnil(quicklyescape) );
  fprintf(f, "\n");

  /* set-mode-display */

  for (i = 0; mode_ichiran3[i]; i++) {
      mode_style(f, mode_ichiran3[i], mode_mei[i], null_mode[i]);
  }
  fprintf(f, "\n");

  for (i = 0; i < NallKeyFunc && allKey[i] && allFunc[i]; i++) {
    print_acbuff(f, allFunc[i], allKey[i]);
    fprintf(f, "\n");
  }
  fprintf(f, "\n");

  if (NalphaKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_AlphaMode);
    for (i = 0; i < NalphaKeyFunc && alphaKey[i] && alphaFunc[i]; i++) {
      print_cbuff(f, alphaFunc[i], alphaKey[i]);
      fprintf(f ,"\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NyomiganaiKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_YomiganaiMode);
    for (i = 0; i < NyomiganaiKeyFunc && yomiganaiKey[i]
	 && yomiganaiFunc[i]; i++) {
      print_cbuff(f, yomiganaiFunc[i], yomiganaiKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f,"  )\n\n");
  }

  if (NyomiKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_YomiMode);
    for (i = 0; i < NyomiKeyFunc && yomiKey[i] && yomiFunc[i]; i++) {
      print_cbuff(f, yomiFunc[i], yomiKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NjishuKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_MojishuMode);
    for (i = 0; i < NjishuKeyFunc && jishuKey[i] && jishuFunc[i]; i++) {
      print_cbuff(f, jishuFunc[i], jishuKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NtankouhoKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_TankouhoMode);
    for (i = 0;
	 i < NtankouhoKeyFunc && tankouhoKey[i] && tankouhoFunc[i]; i++) {
      print_cbuff(f, tankouhoFunc[i], tankouhoKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NichiranKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_IchiranMode);
    for (i = 0; i < NichiranKeyFunc && ichiranKey[i] && ichiranFunc[i]; i++) {
      print_cbuff(f, ichiranFunc[i],ichiranKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NzenHiraKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_ZenHiraKakuteiMode);
    for (i = 0; i < NzenHiraKeyFunc && zenHiraKey[i] && zenHiraFunc[i]; i++) {
      print_cbuff(f, zenHiraFunc[i], zenHiraKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NzenKataKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_ZenKataKakuteiMode);
    for (i = 0; i < NzenKataKeyFunc && zenKataKey[i] && zenKataFunc[i]; i++) {
      print_cbuff(f, zenKataFunc[i], zenKataKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NzenAlphaKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_ZenAlphaKakuteiMode);
    for (i = 0;
	 i < NzenAlphaKeyFunc && zenAlphaKey[i] && zenAlphaFunc[i]; i++) {
      print_cbuff(f, zenAlphaFunc[i], zenAlphaKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NhanKataKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_HanKataKakuteiMode);
    for (i = 0; i < NhanKataKeyFunc && hanKataKey[i] && hanKataFunc[i]; i++) {
      print_cbuff(f, hanKataFunc[i], hanKataKey[i]);
      fprintf(f, "\n");
    }
    fprintf(f, "  )\n\n");
  }

  if (NhanAlphaKeyFunc) {
    fprintf(f, "(let ((mode '%s))\n", S_HanAlphaKakuteiMode);
    for (i = 0;
	 i < NhanAlphaKeyFunc && hanAlphaKey[i] && hanAlphaFunc[i]; i++) {
      print_cbuff(f, hanAlphaFunc[i], hanAlphaKey[i]);
      fprintf(f ,"\n");
    }
    fprintf(f, "  )\n\n");
  }
}
