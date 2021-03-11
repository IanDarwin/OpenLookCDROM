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
 *  Author : Osamu Hata, NEC Corporation  (hata@bs5.bsd.mt.nec.co.jp)
 *
 */

static	char	rcs_id[] = "@(#) 102.1 $Id: wiroha.c,v 1.10 1992/07/28 12:20:24 kon Exp $";

#include <stdio.h>

#define ON   1
#define OFF -1
#define MID  2

extern char *kanjidicname[], *userdicname[],  *bushudicname[], *localdicname[]; 
extern char *funcList[];

extern int  nkanjidics, nuserdics, nbushudics, nlocaldics;
extern char *RomkanaTable, *RengoGakushu[], *KatakanaGakushu[];
extern int InitialMode, CursorWrap, SelectDirect, HexkeySelect, BunsetsuKugiri;
extern int ChBasedMove, ReverseWidely, Gakushu, QuitIchiranIfEnd;
extern int kakuteiIfEndOfBunsetsu, stayAfterValidate, BreakIntoRoman;
extern int kouho_threshold, gramaticalQuestion;
extern char *mode_mei[], null_mode[];
extern char *old_mode_ichiran2[], *old_mode_ichiran3[];
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

char *
toChar(suuji)
int suuji;
{
  if (suuji == ON)
    return "on";
  else if (suuji == OFF)
    return "off";
  else
    return " ";
}

char *
print_buff(acts, keys)
unsigned char *acts, *keys;
{
  unsigned char *p;
  char *_buff;
  char buff[512];

  strcpy(buff, "");
  for (p = acts; *p; p++) {
    strcat(buff, funcList[(int)*p]);
    strcat(buff, ",");
  }
  buff[strlen(buff)-1] = '\0';
  strcat(buff, "     ");
  p = keys;
  if (*p == 255)
    strcat(buff, "undefine ");
  for (; *p != 255; p++) {
    if (*p == ',' || *p == '#' || *p == '{' || *p == '}'
	|| *p == '(' || *p == ')') 
      strcat(buff, "\\");
    strcat(buff, showChar((int)*p));
    strcat(buff, ",");
  }
  buff[strlen(buff)-1] = '\0';
  _buff = (char *)malloc(strlen(buff)+1);
  strcpy(_buff, buff);
  return _buff;
}

write_iroha(f)
FILE *f;
{
  int i;

  fprintf(f, "initialMode     %s\n\n",old_mode_ichiran2[InitialMode]);

  if (RomkanaTable) {
    fprintf(f, "romkanatable   \"%s\"\n", RomkanaTable);
  }

  for (i = 0;i < nkanjidics && *kanjidicname[i];i++)
    fprintf(f, "dictionary     \"%s\"\n", kanjidicname[i]);

  for (i = 0;i < nuserdics && *userdicname[i];i++)
    fprintf(f, "userdic        \"%s\"\n", userdicname[i]);

  for (i = 0;i < nbushudics && *bushudicname[i];i++)
    fprintf(f, "bushudic       \"%s\"\n", bushudicname[i]);
/*
  if (RengoGakushu) {
    fprintf(f, "rengodic       \"%s\"\n", RengoGakushu);
  }
*/

  fprintf(f, "\ncursorWrap              %s\n", toChar(CursorWrap));
  fprintf(f, "selectDirect            %s\n", toChar(SelectDirect));
  fprintf(f, "numericalKeySelect      %s\n", toChar(HexkeySelect));
  fprintf(f, "bunsetsuKugiri          %s\n", toChar(BunsetsuKugiri));
  fprintf(f, "characterBasedMove      %s\n", toChar(ChBasedMove));
  fprintf(f, "reverseWidely           %s\n", toChar(ReverseWidely));
  fprintf(f, "gakushu                 %s\n", toChar(Gakushu));
  fprintf(f, "quitIfEndOfIchiran      %s\n", toChar(QuitIchiranIfEnd));
  fprintf(f, "kakuteiIfEndOfBunsetsu  %s\n", toChar(kakuteiIfEndOfBunsetsu));
  fprintf(f, "stayAfterValidate       %s\n", toChar(stayAfterValidate));
  fprintf(f, "breakIntoRoman          %s\n", toChar(BreakIntoRoman));
  fprintf(f, "gramaticalQuestion      %s\n", toChar(gramaticalQuestion));
  fprintf(f, "nHenkanForIchiran       %d\n\n", kouho_threshold);

  for (i = 0; old_mode_ichiran3[i]; i++) {
    if (mode_mei[i]) {
      fprintf(f, "modeString(%s)     \"%s\"\n",
	      old_mode_ichiran3[i], mode_mei[i]);
    }
    else if (null_mode[i]) {
      fprintf(f, "modeString(%s)     NULL\n", old_mode_ichiran3[i]);
    } 
  }
  fprintf(f, "\n");

  for (i = 0; i < NallKeyFunc && allKey[i] && allFunc[i]; i++)
    fprintf(f, "%s\n", print_buff(allFunc[i],allKey[i]));

  if (NalphaKeyFunc) {
    fprintf(f, "\nalpha {\n");
    for (i = 0; i < NalphaKeyFunc && alphaKey[i] && alphaFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(alphaFunc[i],alphaKey[i]));
    fprintf(f, "}\n");
  }

  if (NyomiganaiKeyFunc) {
    fprintf(f, "\nyomiganai {\n");
    for (i = 0; i < NyomiganaiKeyFunc && yomiganaiKey[i]
	 && yomiganaiFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(yomiganaiFunc[i],yomiganaiKey[i]));
    fprintf(f,"}\n");
  }

  if (NyomiKeyFunc) {
    fprintf(f, "\nyomi {\n");
    for (i = 0; i < NyomiKeyFunc && yomiKey[i] && yomiFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(yomiFunc[i],yomiKey[i]));
    fprintf(f, "}\n");
  }

  if (NjishuKeyFunc) {
    fprintf(f, "\nmojishu {\n");
    for (i = 0; i < NjishuKeyFunc && jishuKey[i] && jishuFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(jishuFunc[i],jishuKey[i]));
    fprintf(f, "}\n");
  }

  if (NtankouhoKeyFunc) {
    fprintf(f, "\ntankouho {\n");
    for (i = 0; i < NtankouhoKeyFunc && tankouhoKey[i] && tankouhoFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(tankouhoFunc[i],tankouhoKey[i]));
    fprintf(f, "}\n");
  }

  if (NichiranKeyFunc) {
    fprintf(f, "\nichiran {\n");
    for (i = 0; i < NichiranKeyFunc && ichiranKey[i] && ichiranFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(ichiranFunc[i],ichiranKey[i]));
    fprintf(f, "}\n");
  }

  if (NzenHiraKeyFunc) {
    fprintf(f, "\nzenHiraKakutei {\n");
    for (i = 0; i < NzenHiraKeyFunc && zenHiraKey[i] && zenHiraFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(zenHiraFunc[i],zenHiraKey[i]));
    fprintf(f, "}\n");
  }

  if (NzenKataKeyFunc) {
    fprintf(f, "\nzenKataKakutei {\n");
    for (i = 0; i < NzenKataKeyFunc && zenKataKey[i] && zenKataFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(zenKataFunc[i],zenKataKey[i]));
    fprintf(f, "}\n");
  }

  if (NzenAlphaKeyFunc) {
    fprintf(f, "\nzenAlphaKakutei {\n");
    for (i = 0; i < NzenAlphaKeyFunc && zenAlphaKey[i] && zenAlphaFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(zenAlphaFunc[i],zenAlphaKey[i]));
    fprintf(f, "}\n");
  }

  if (NhanKataKeyFunc) {
    fprintf(f, "\nhanKataKakutei {\n");
    for (i = 0; i < NhanKataKeyFunc && hanKataKey[i] && hanKataFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(hanKataFunc[i],hanKataKey[i]));
    fprintf(f, "}\n");
  }

  if (NhanAlphaKeyFunc) {
    fprintf(f, "\nhanAlphaKakutei {\n");
    for (i = 0; i < NhanAlphaKeyFunc && hanAlphaKey[i] && hanAlphaFunc[i]; i++)
      fprintf(f, "  %s\n", print_buff(hanAlphaFunc[i],hanAlphaKey[i]));
    fprintf(f, "}\n");
  }
}
