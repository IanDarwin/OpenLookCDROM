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

/*
static	char	rcs_id[] = "@(#) 102.1 $Id: ccustom.h,v 2.5 1994/02/03 07:31:07 hamada Exp $";
*/

#include "symbolname.h"

#define NHENKAN_MAX 9999

extern char *kanjidicname[], *userdicname[],  *bushudicname[], *localdicname[]; 
extern int  nkanjidics, nuserdics, nbushudics, nlocaldics;
extern char  *RomkanaTable, *RengoGakushu, *KatakanaGakushu;
extern int InitialMode, CursorWrap, SelectDirect, HexkeySelect, BunsetsuKugiri;
extern int ChBasedMove, ReverseWidely, Gakushu, QuitIchiranIfEnd;
extern int kakuteiIfEndOfBunsetsu, stayAfterValidate, BreakIntoRoman;
extern int kouho_threshold, gramaticalQuestion;
extern char *mode_mei[], null_mode[];
int forceKana, kCount, chikuji, iListCB ,nKouhoBunsetsu;
int keepCursorPosition, CannaVersion, abandonIllegalPhono;
int hexCharacterDefiningStyle, kojin, ReverseWord , allowNextInput;
int indexhankaku,ignorecase,romajiyuusen,autosync,nkeysuu,quicklyescape;

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
extern char returnKey[];
extern char *funcList[];
extern char *cfuncList[];

char **fList;

char etc_ctm[32];
char **etc_menu;
char **etc_manual;

char **current_acts, **current_keys;
char err_mess[255];
char *initFileSpecified =(char *)NULL;

int is_icustom;
int junban;

char *old_mode_ichiran[] = {
  "アルファベットモード",
  "[あ] 変換入力モード",
  "[記号] 記号一覧表示状態",
  "確[あ] 全角ひらがな確定入力モード",
  "確[ア] 全角カタカナ確定入力モード",
  "確[ｱ] 半角カタカナ確定入力モード",
  "確[ａ] 全角アルファベット確定入力モード",
  "確[a] 半角アルファベット確定入力モード",
  "[あ] 読み入力状態",
  "[字種] 文字種変換状態",
  "[漢字] 単一候補表示状態",
  "[一覧] 候補一覧表示状態",
  "[質問] 単語登録の例文表示状態",
  "[質問] 単語登録の例文表示状態",
  "[ア] 全角カタカナ変換入力モード",
  "[ｱ] 半角カタカナ変換入力モード",
  "[ａ] 全角アルファベット変換入力モード",
  "[a] 半角アルファベット変換入力モード",
  "[16進] １６進コード入力状態",
  "[部首] 部首の読み入力状態",
  "[拡張] 拡張機能選択状態",
  "[ロ] ロシア文字選択状態",
  "[ギ] ギリシャ文字選択状態",
  "[罫線] 罫線選択状態",
  "[変更] サーバ変更状態",
  "[変換] 変換方式選択",
  "[削除] 単語削除状態",
  "[登録] 単語登録モード",
  "[品詞] 単語登録モードの品詞選択状態",
  "[辞書] 単語登録モードの辞書選択状態",
  "[ｑ] 引用入力モード",
  "[編集] 編集モード",
  "[辞書] 辞書のマウント、アンマウント状態",
  NULL
};

char *old_mode_ichiran2[] = {
  "alphaMode",
  "henkanNyuryokuMode",
  "[記号] 記号一覧表示状態",
  "zenHiraKakuteiMode",
  "zenKataKakuteiMode",
  "hanKataKakuteiMode",
  "zenAlphaKakuteiMode",
  "hanAlphaKakuteiMode",
  "[あ] 読み入力状態",
  "[字種] 文字種変換状態",
  "[漢字] 単一候補表示状態",
  "[一覧] 候補一覧表示状態",
  "[質問] 単語登録の例文表示状態",
  "[質問] 単語登録の例文表示状態",
  "zenKataHenkanMode",
  "hanKataHenkanMode",
  "zenAlphaHenkanMode",
  "hanAlphaHenkanMode",
  "[16進] １６進コード入力状態",
  "[部首] 部首の読み入力状態",
  "[拡張] 拡張機能選択状態",
  "[ロ] ロシア文字選択状態",
  "[ギ] ギリシャ文字選択状態",
  "[罫線] 罫線選択状態",
  "[変更] サーバ変更状態",
  "[変換] 変換方式選択",
  "[削除] 単語削除状態",
  "[登録] 単語登録モード",
  "[品詞] 単語登録モードの品詞選択状態",
  "[辞書] 単語登録モードの辞書選択状態",
  "[ｑ] 引用入力モード",
  "[編集] 編集モード",
  "[辞書] 辞書のマウント、アンマウント状態",
  NULL
};

char *mode_ichiran3[] = {
  S_AlphaMode,
  S_YomiganaiMode,
  S_KigouMode,
  S_YomiMode,
  S_MojishuMode,
  S_TankouhoMode,
  S_IchiranMode,
  S_YesNoMode,
  S_OnOffMode,
  "S_AdjustBunsetsu",
  S_AutoYomiMode,
  S_AutoBunsetsuMode,
  S_HenkanNyuuryokuMode,
  S_ZenHiraHenkanMode,
  S_HanHiraHenkanMode,
  S_ZenKataHenkanMode,
  S_HanKataHenkanMode,
  S_ZenAlphaHenkanMode,
  S_HanAlphaHenkanMode,
  S_ZenHiraKakuteiMode,
  S_HanHiraKakuteiMode,
  S_ZenKataKakuteiMode,
  S_HanKataKakuteiMode,
  S_ZenAlphaKakuteiMode,
  S_HanAlphaKakuteiMode,
  S_HexMode,
  S_BushuMode,
  S_ExtendMode,
  S_RussianMode,
  S_GreekMode,
  S_LineMode,
  S_ChangingServerMode,
  S_HenkanMethodMode,
  S_DeleteDicMode,
  S_TourokuMode,
  S_TourokuHinshiMode,
  S_TourokuDicMode,
  S_QuotedInsertMode,
  S_BubunMuhenkanMode,
  S_MountDicMode,
  NULL
};

char *mode_ichiran[] = {
  "アルファベットモード",
  "[ あ ] 変換入力モード",
  "[記号] 記号一覧表示状態",
  "<全あ> 全角ひらがな確定入力モード",
  "<全ア> 全角カタカナ確定入力モード",
  "<半ア> 半角カタカナ確定入力モード",
  "<全英> 全角アルファベット確定入力モード",
  "<全英> 半角アルファベット確定入力モード",
  "[ あ ] 読み入力状態",
  "[字種] 文字種変換状態",
  "[漢字] 単一候補表示状態",
  "[一覧] 候補一覧表示状態",
  "[質問] 単語登録の例文表示状態",
  "[質問] 単語登録の例文表示状態",
  "[全ア] 全角カタカナ変換入力モード",
  "[半ア] 半角カタカナ変換入力モード",
  "[全英] 全角アルファベット変換入力モード",
  "[半英] 半角アルファベット変換入力モード",
  "[16進] １６進コード入力状態",
  "[部首] 部首の読み入力状態",
  "[拡張] 拡張機能選択状態",
  "[ ロ ] ロシア文字選択状態",
  "[ ギ ] ギリシャ文字選択状態",
  "[罫線] 罫線選択状態",
  "[変更] サーバ変更状態",
  "[変換] 変換方式選択",
  "[削除] 単語削除状態",
  "[登録] 単語登録モード",
  "[品詞] 単語登録モードの品詞選択状態",
  "[辞書] 単語登録モードの辞書選択状態",
  "[ ｑ ] 引用入力モード",
  "[編集] 編集モード",
  "[辞書] 辞書のマウント、アンマウント状態",
  NULL
};

char *mode_ichiran2[] = {
  NULL,
  "",
  "[記号] 記号一覧表示状態",
  S_ZenHiraKakuteiMode,
  S_ZenKataKakuteiMode,
  S_HanKataKakuteiMode,
  S_ZenAlphaKakuteiMode,
  S_HanAlphaKakuteiMode,
  "[ あ ] 読み入力状態",
  "[字種] 文字種変換状態",
  "[漢字] 単一候補表示状態",
  "[一覧] 候補一覧表示状態",
  "[質問] 単語登録の例文表示状態",
  "[質問] 単語登録の例文表示状態",
  S_ZenKataHenkanMode,
  S_HanKataHenkanMode,
  S_ZenAlphaHenkanMode,
  S_HanAlphaHenkanMode,
  "[16進] １６進コード入力状態",
  "[部首] 部首の読み入力状態",
  "[拡張] 拡張機能選択状態",
  "[ ロ ] ロシア文字選択状態",
  "[ ギ ] ギリシャ文字選択状態",
  "[罫線] 罫線選択状態",
  "[変更] サーバ変更状態",
  "[変換] 変換方式選択",
  "[削除] 単語削除状態",
  "[登録] 単語登録モード",
  "[品詞] 単語登録モードの品詞選択状態",
  "[辞書] 単語登録モードの辞書選択状態",
  "[ ｑ ] 引用入力モード",
  "[編集] 編集モード",
  "[辞書] 辞書のマウント、アンマウント状態",
  NULL
};

char *old_mode_ichiran3[] = {
  "Alpha",
  "HenkanNyuryoku",
  "Kigo",
  "ZenHiraKakutei",
  "ZenKataKakutei",
  "HanKataKakutei",
  "ZenAlphaKakutei",
  "HanAlphaKakutei",
  "Yomi",
  "Mojishu",
  "Tankouho",
  "Ichiran",
  "YesNo",
  "YesNo",
  "ZenKataHenkan",
  "HanKataHenkan",
  "ZenAlphaHenkan",
  "HanAlphaHenkan",
  "Hex",
  "Bushu",
  "Extend",
  "Russian",
  "Greek",
  "Line",
  "ChangingServer",
  "HenkanMethod",
  "DeleteDic",
  "Touroku",
  "TourokuHinshi",
  "TourokuDic",
  "QuotedInsert",
  "BubunMuhenkan",
  "MountDic",
  NULL
};

char mode_set[] = { 0, 1, 3, 4, 6, 5, 7, 1, 14, 16, 15, 17, -1};

char *base_menu[] = {
  "カスタマイズファイルの読み込み",
  "カスタマイズファイルへの保存",
  "使用する辞書の設定",
  "キーのカスタマイズ",
  "モードの表示文字列のカスタマイズ",
  "その他のカスタマイズ",
  "終了",
  NULL
};

char *d_mess[] = {
  "辞書の種類選択部へ移動…スペースキー,ＥＳＣキー",
  "登録内容表示部へ移動…スペースキー",
  "辞書登録…辞書の種類選択状態でリターンキー",
  "辞書削除…辞書の登録内容表示状態でＤキー",
  "使用する辞書の選択を終了…辞書の種類選択状態でＥＳＣキー",
  NULL
};

char *dic_menu[] = {
  "ローマ字かな変換テーブル",
  "システム辞書名",
  "部首変換辞書名",
  "ユーザ辞書名",
/*
  "連語変換辞書名",
*/
  NULL
};

char *key_menu[] = {
  "全てのモード",
  "アルファベットモード",
  "変換入力モード(非読み入力時)",
  "変換入力モード(読み入力時)",
  "文字種変換状態",
  "単一候補表示状態",
  "候補一覧表示状態",
  "全角ひらがな確定入力モード",
  "全角カタカナ確定入力モード",
  "全角アルファベット確定入力モード",
  "半角カタカナ確定入力モード",
  "半角アルファベット確定入力モード",
  NULL
};

char *kh_mess[] = {
  "新規カスタマイズ：キー設定は「新規キー」を選択後、リターンキーを押す",
  "　　　　　　　　　機能設定は「新規機能」を選択後、リターンキーを押す",
  "　",
  "既カスタマイズ部の編集：変更する部分を選択後、",
  "　　　　　　　　　　　　削除　　Ｄキーを押す",
  "　　　　　　　　　　　　行削除　Ｋキーを押す",
  "　　　　　　　　　　　　挿入　　Ｉキーを押す",
  "　　　　　　　　　　　　追加　　Ａキーを押す",
  "このモードを抜ける：ＥＳＣキー、Ｑキーを押す",
  NULL
};

struct dk_block {
  char *str;
  int len;
  int gyousu;
  int keysu;
  int actsu;
} dk_blocks[1024];

struct U_dk_block {
  char str[256];
  int len;
  int gyousu;
  int keysu;
  int actsu;
} U_dk_blocks;

struct wdisp {
  int top;
  int line;
  int kora;
  int ban;
  int current;
} WDisp, U_WDisp;

char *cc_keys[1], *cc_acts[1];
char ccc_keys[256], ccc_acts[256];
char undo_keys[256], undo_acts[256];
int  cc_keys_len = 0, cc_acts_len = 0;
int touroku_start = -1;
int undo_status = -1, undo_ts;

char *key_iroha[] = {
  "all",
  "alpha",
  "yomiganai",
  "yomi",
  "mojishu",
  "tankouho",
  "ichiran",
  "zenHiraKakutei",
  "zenKataKakutei",
  "zenAlphaKakutei",
  "hanKataKakutei",
  "hanAlphaKakutei",
  NULL
  };

char *keyLine[] = {
  "C-@  C-a  C-b  C-c  C-d  C-e  C-f  C-g  C-h  C-i  C-j  C-k",
  "C-l  C-m  C-n  C-o  C-p  C-q  C-r  C-s  C-t  C-u  C-v  C-w",
  "C-x  C-y  C-z  C-[  C-\\  C-]  C-^  C-_  space  !  \"  #  $  %",
  "&  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  9",
  ":  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  M",
  "N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \\  ]  ^  _  `  a",
  "b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u",
  "v  w  x  y  z  {  |  }  ~  DEL  Nfer  Xfer  Up  Left  Right",
  "Down  Insert  Rollup  Rolldown  HOME  HELP  S-nfer  S-xfer",
  "S-up  S-left  S-right  S-down  C-nfer  C-xfer  C-up  C-left",
  "C-right  C-down  F1  F2  F3  F4  F5  F6  F7  F8  F9  F10",
  "PF1  PF2  PF3  PF4  undefine",
  NULL
};

char **actLine;

char *caLine[] = {
  "self-insert:その文字を挿入する",                             /*  2 */
  "quoted-insert:次の一文字を無条件に入力する",
  "japanese-mode:日本語モードへの移行",
  "alpha-mode:アルファベットモードに移行",                      /*  5 */
  "henkan-nyuuryoku-mode:変換入力モードに移行",
  "forward:右移動(選択)",
  "backward:左移動(選択)",
  "next:次選択、文字種変換(読み入力のとき)",
  "previous:前選択、文字種変換(読み入力のとき)",                /* 10 */
  "beginning-of-line:先頭(左端)選択",
  "end-of-line:末尾(右端)選択",
  "delete-next:次一文字消去",
  "delete-previous:前一文字削除",
  "kill-to-end-of-line:カーソルより右側の文字を行末まで削除",   /* 15 */
  "henkan:変換",
  "kakutei:確定",
  "extend:領域伸ばし",
  "shrink:領域縮め",
  "shinshuku-mode:文節伸縮モードへ移行",                        /* 20 */
  "quit:取りやめ",
  "convert-as-hex:読みを16進コードとみなして変換",
  "convert-as-bushu:読みを部首名とみなして変換",
  "kouho-ichiran:候補一覧表示",
  "henshu:編集",                                                /* 25 */
  "zenkaku:全角変換",
  "hankaku:半角変換",
  "to-upper:大文字に変換",
  "capitalize:先頭文字のみを大文字にする",
  "to-lower:小文字に変換",                                      /* 30 */
  "hiragana:ひらがな変換",
  "katakana:カタカナ変換",
  "romaji:ローマ字変換",
  "base-hiragana:入力ベースをひらがなにする",
  "base-katakana:入力ベースをカタカナにする",                   /* 35 */
  "base-eisu:入力ベースを英数にする",
  "base-zenkaku:入力ベースを全角にする",
  "base-hankaku:入力ベースを半角にする",
  "base-kana:入力ベースをカナにする",
  "base-kakutei:入力モードを確定モードにする",                  /* 40 */
  "base-henkan:入力モードを変換モードにする",
  "base-hiragana-katakana-toggle:入力ベースをひらがな、カタカナでトグル", 
  "base-zenkaku-hankaku-toggle:入力ベースを全角と半角でトグル",
  "base-kana-eisu-toggle:入力ベースをカナと英数でトグル",
  "base-kakutei-henkan-toggle:入力モードを確定と変換でトグル",  /* 45 */
  "base-rotate-forward:入力モードを順番に切り替える(正順)",
  "base-rotate-backward:入力モードを順番に切り替える(逆順)",
  "extend-mode:ユーティリティメニューに移行",
  "hex-mode:コード入力モードに移行",
  "bushu-mode:部首入力モードに移行",                            /* 50 */
  "kigou-mode:記号入力モードに移行",
  "zen-hira-kakutei-mode:全角ひらがな確定モードに移行",
  "zen-kata-kakutei-mode:全角カタカナ確定モードに移行",
  "han-kata-kakutei-mode:半角カタカナ確定モードに移行",
  "zen-alpha-kakutei-mode:全角アルファベット確定モードに移行",  /* 55 */
  "han-alpha-kakutei-mode:半角アルファベット確定モードに移行",
  "henkan-or-self-insert:ベースがひらがな以外のときself-insertをおこなう",
  "henkan-or-do-noting:ベースがひらがな以外のときなにもしない",
  "switch-server:サーバの切り替えをおこなう",
  "disconnect-server:サーバとの接続を切る",
  "show-server-name:サーバ名を表示する",
  "show-gakushu:学習状態を表示する",
  "show-canna-version:バージョンを表示する",
  "show-romkana-table:ローマ字かな変換テーブル名を表示する",
  "show-canna-file:カスタマイズファイル名を表示する",
  "sync-dicsyionary:辞書に書き込む",
  NULL
};

char *aLine[] = {
  "SelfInsert:一文字挿入する",
  "QuotedInsert:次の一文字を無条件に入力する",
  "JapaneseMode:日本語モードへの移行",
  "AlphaMode:アルファベットモードに移行",
  "HenkanNyuryokuMode:変換入力モードに移行",
  "ZenHiraKakuteiMode:全角ひらがな確定モードに移行",
  "ZenKataKakuteiMode:全角カタカナ確定モードに移行",
  "HanKataKakuteiMode:半角カタカナ確定モードに移行",
  "ZenAlphaKakuteiMode:全角アルファベット確定モードに移行",
  "HanAlphaKakuteiMode:半角アルファベット確定モードに移行",
  "HexMode:１６進コード入力モードに移行",
  "BushuMode:部首入力モードに移行",
  "KigouMode:記号入力モードに移行",
  "Forward:右移動(選択)",
  "Backward:左移動(選択)",
  "Next:次選択、文字種変換(読み入力のとき)",
  "Previous:前選択、文字種変換(読み入力のとき)",
  "BeginningOfLine:先頭(左端)選択",
  "EndOfLine:末尾(右端)選択",
  "DeleteNext:カーソル部分の文字削除",
  "DeletePrevious:カーソルの左の文字削除",
  "KillToEndOfLine:カーソルより右側の文字を行末まで削除",
  "Henkan:変換",
  "Kakutei:確定",
  "Extend:領域伸ばし、入力文字を切り替える(読み入力のとき)",
  "Shrink:領域縮め、入力文字を切り替える(読み入力のとき)",
  "Quit:取りやめ",
  "Touroku:ユーティリティ(拡張)モードへ移行",
  "ConvertAsHex:読みを16進コードとみなして変換",
  "ConvertAsBushu:読みを部首名とみなして変換",
  "KouhoIchiran:候補一覧表示",
  "BubunMuhenkan:部分無変換",
  "Zenkaku:全角に変換",
  "Hankaku:半角に変換",
  "ToUpper:大文字に変換",
  "Capitalize:先頭文字のみを大文字にする",
  "ToLower:小文字に変換",
  "Hiragana:ひらがなに変換",
  "Katakana:カタカナに変換",
  "Romaji:ローマ字に変換",
  "NextKouho:次候補",
  "PrevKouho:前候補",
  NULL
};

char *old_mode_menu[] = {
  "         アルファベットモード",
  "  [あ]   変換入力モード",
  "確[あ]   全角ひらがな確定入力モード",
  "確[ア]   全角カタカナ確定入力モード",
  "確[ｱ]    半角カタカナ確定入力モード",
  "確[ａ]   全角アルファベット確定入力モード",
  "確[a]    半角アルファベット確定入力モード",
  "  [字種] 文字種変換状態",
  "  [漢字] 単一候補表示状態",
  "  [一覧] 候補一覧表示状態",
  "  [ア]   全角カタカナ変換入力モード",
  "  [ｱ]    半角カタカナ変換入力モード",
  "  [ａ]   全角アルファベット変換入力モード",
  "  [a]    半角アルファベット変換入力モード",
  "  [16進] １６進コード入力状態",
  "  [部首] 部首の読み入力状態",
  "  [記号] 記号一覧表示状態",
  "  [拡張] 拡張機能選択状態",
  "  [ロ]   ロシア文字選択状態",
  "  [ギ]   ギリシャ文字選択状態",
  "  [罫線] 罫線選択状態",
  "  [変更] サーバ変更状態",
  "  [削除] 単語削除状態",
  "  [登録] 単語登録モード",
  "  [品詞] 単語登録モードの品詞選択状態",
  "  [辞書] 単語登録モードの辞書選択状態",
  "  [ｑ]   引用入力モード",
  "  [編集] 編集モード",
  "  [辞書] 辞書のマウント、アンマウント状態",
  NULL
};

char *mode_menu[] = {
  "         アルファベットモード",
  "[ あ ]   変換入力モード",
  "<全あ>   全角ひらがな確定入力モード",
  "<全ア>   全角カタカナ確定入力モード",
  "<半ア>   半角カタカナ確定入力モード",
  "<全英>   全角アルファベット確定入力モード",
  "<半英>   半角アルファベット確定入力モード",
  "[字種]   文字種変換状態",
  "[漢字]   単一候補表示状態",
  "[一覧]   候補一覧表示状態",
  "[全ア]   全角カタカナ変換入力モード",
  "[半ア]   半角カタカナ変換入力モード",
  "[全英]   全角アルファベット変換入力モード",
  "[半英]   半角アルファベット変換入力モード",
  "[逐次]   逐次自動変換読み部分",
  "[逐次]   逐次自動変換文節部分",
  "[16進]   １６進コード入力状態",
  "[部首]   部首の読み入力状態",
  "[記号]   記号一覧表示状態",
  "[拡張]   拡張機能選択状態",
  "[ ロ ]   ロシア文字選択状態",
  "[ ギ ]   ギリシャ文字選択状態",
  "[罫線]   罫線選択状態",
  "[変更]   サーバ変更状態",
  "[削除]   単語削除状態",
  "[登録]   単語登録モード",
  "[品詞]   単語登録モードの品詞選択状態",
  "[辞書]   単語登録モードの辞書選択状態",
  "[ ｑ ]   引用入力モード",
  "[編集]   編集モード",
  "[辞書]   辞書のマウント、アンマウント状態",
  NULL
};

char mm_set[] = { 0, 1, 3, 4, 5, 6, 7, 9, 10, 11, 14, 15, 16, 17, 18,
		     19, 2, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32 };

char c_mm_set[] = { 0, 12, 19, 21, 22, 23, 24, 4, 5, 6, 15,
		      16, 17, 18, 10, 11,25, 26,2, 27, 28, 
		      29, 30, 31, 33, 34, 35, 36, 37, 38, 39 };

char *ce_menu[] = {
  "initialMode",
  "cursorWrap",
  "numericalKeySelect",
  "selectDirect",
  "bunsetsuKugiri",
  "characterBasedMove",
  "reverseWidely",
  "quitIfEndOfIchiran",
  "breakIntoRoman",
  "gakushu",
  "stayAfterValidate",
  "kakuteiIfEndOfBunsetsu",
  "gramaticalQuestion",
  "nHenkanForIchiran",
  "kouhoCount",
  "kojin",
  "auto",
  "nKouhoBunsetsu",
  "abandonIllegalPhono",
  "hexDirect",
  "allowNextInput",
  "indexHankaku",
  "ignoreCase",
  "romajiYuusen",
  "auto-sync",
  "nKeyToDisconnect",
  "quicklyEscape",
  NULL
};

char *e_menu[] = {
  "initialMode",
  "cursorWrap",
  "numericalKeySelect",
  "selectDirect",
  "bunsetsuKugiri",
  "characterBasedMove",
  "reverseWidely",
  "quitIfEndOfIchiran",
  "breakIntoRoman",
  "gakushu",
  "stayAfterValidate",
  "kakuteiIfEndOfBunsetsu",
  "gramaticalQuestion",
  "nHenkanForIchiran",
  NULL
};
  
#define ON   1
#define OFF -1
#define MID  2

char *ce_manual[] = {
  "  起動時のモードを矢印キー(←,→)で指定します。デフォルトはアルファベット\
モードです。",
  "  読みを入力している状態でカーソルを移動する時に右端から右\
へ移動する操作をした時や左端から左へ移動する操作をした時に\
反対側のカーソルが移動することを指定します。onで移動し、off\
で移動しません。デフォルトはonです。",
  "候補一覧で、数字キーを用いて候補を選択できるかどうかを指定\
します。デフォルトはonです。offを指定すると、これらのキーを\
押した場合選択されている候補が確定し、押下したキーは次の読みとして\
取り扱われます。",
  "numericalKeySelectがonであるときに、数字キーで候補を選択したとき\
に候補一覧表示のままかそうでないかを指定します。onの場合は候補一覧が\
終了します。デフォルトはoffです。",
  "候補を表示しているときに文節ごとに空白で区切るかどうかを指定\
します。onで区切ります。デフォルトはoffです。",
  "読みを入力しているときにカーソル移動を行う場合に、文字単位で移動を\
行うかどうかを指定します。デフォルトはonです。offにすると、ローマ字\
かな変換の確定の単位を基にしてカーソルを移動します。文字削除も同様に\
行われます。",
  "onを指定すると読みを入力している時の文字列の反転範囲が広くなります。\
デフォルトはoffです。",
  "候補一覧で最終候補を表示している状態で次候補操作を行うと候補一覧を\
終了し、読みそのものを候補として表示するようになります。２打目の\
スペースキーを候補一覧表示にカスタマイズしている時などはonにしておく\
と便利です。デフォルトはoffです。",
  "バックスペースキーを打ったときに直前のローマ字かな変換された文字列が\
ローマ字に戻るかどうかを指定します。デフォルトはoffです。",
  "かな漢字変換が学習を行うかどうかを指定します。デフォルトはonです。",
  "候補一覧状態で候補を選択して単一候補表示状態に戻ったときに、カレント\
文節を次の文節に移動させるかどうかを指定します。offでカレント文節に移動\
します。onではカレント文節は変わりません。デフォルトはonです。",
  "最右文節で次文節へ移動しようとしたときに、確定するか否かを指定します。\
offを指定すると最左文節がカレント文節になります。デフォルトはoffです。",
  "単語登録で品詞を指定した後、詳細な品詞分類のための質問を行うか否かを\
指定します。onで質問を行い、offでは質問をしません。デフォルトはonです。",
  "変換キーを何回か押すと候補一覧表示が行われるようにする事\
ができます。ここではその回数を矢印キー(←,→)で指定します。\
0回にすると何回変換キーを\
押しても一覧表示が行われなくなります。デフォルトは2回で、2回変換キーを\
押すと一覧表示になります。",
  "候補一覧やユーティリティ機能の表示画面の部分の候補数またはアイテム数\
を表示するかしないかを設定します。onで表示し、offで表示されなくなります。\
デフォルトはonです。",
  "個人別学習ファイルが存在する辞書に対して、個人別学習を行うかどうかを\
指定します。onで行い、offで行いません。デフォルトはonで個人別学習を行います。",
  "変換方式を逐次自動変換にするかしないかを設定します。onでは逐次自動変換\
になりoffでは連文節変換になります。デフォルトはoff(連文節変換)です。",
  "逐次自動変換において変換中に保持している候補文節の数を設定します。\
デフォルトは16文節です。設定可能な数は3〜32です。 ここではその回数を\
矢印キー(←,→)で指定します。",
  "ローマ字かな変換で不正なローマ字が入力として残るかどうかを指定します。\
onで不正なローマ字が捨てられます。offでは不正なローマ字は残ります。デフォルト\
はoffです。",
  "コード入力において4桁のコードを入力後、得られる文字を確定するかしないか\
を設定します。onで確定して、offで確定しません。デフォルトはoffで確定\
しません。",
  "候補一覧表示状態で数字以外のキーを押した時に、現在カーソルが位置して\
いる候補が選択され次の入力となるかどうかを指定します。",
  "候補一覧の番号を全角文字から半角文字に変更するかどうかを指定します。デフォルトはoffで全角文字です。", 
  "ローマ字かな変換テーブルは通常は大文字を入力するとアルファベットのまま入力され、かなには変換されません。onにすると大文字で入力してもローマ字かな変換では小文字として取り扱われます。デフォルトはoffです。",
  "onにすると入力された文字がローマ字かな変換で有効となるキーであれば、そのキーに割り当てられている機能が何であろうと self-insert がおこなわれます。デフォルトはoffです。",
  "単語登録および削除をしたとき自動的に辞書の同期が行われるかどうかを指定します。onにすると同期が行われます。デフォルトはonです。",
  "アルファベットを入力し続けた場合のサーバとの接続の切れるストローク数を指定します。ここでは回数を矢印キー(←,→)で指定します。１キーで100回変化します。デフォルトは500です。",
  "記号入力モードで記号を選択した後も記号選択モードに留まるかどうかを指定します。off にすると留まります。",
  NULL
};

char *e_manual[] = {
  "  起動時のモードを矢印キー(←,→)で指定します。デフォルトはアルファベット\
モードです。",
  "  読みを入力している状態でカーソルを移動する時に右端から右\
へ移動する操作をした時や左端から左へ移動する操作をした時に\
反対側のカーソルが移動することを指定します。onで移動し、off\
で移動しません。デフォルトはonです。",
  "候補一覧で、数字キーを用いて候補を選択できるかどうかを指定\
します。デフォルトはonです。offを指定すると、これらのキーを\
押した場合選択されている候補が確定し、押下したキーは次の読みとして\
取り扱われます。",
  "numericalKeySelectがonであるときに、数字キーで候補を選択したとき\
に候補一覧表示のままかそうでないかを指定します。onの場合は候補一覧が\
終了します。デフォルトはoffです。",
  "候補を表示しているときに文節ごとに空白で区切るかどうかを指定\
します。onで区切ります。デフォルトはoffです。",
  "読みを入力しているときにカーソル移動を行う場合に、文字単位で移動を\
行うかどうかを指定します。デフォルトはonです。offにすると、ローマ字\
かな変換の確定の単位を基にしてカーソルを移動します。文字削除も同様に\
行われます。",
  "onを指定すると読みを入力している時の文字列の反転範囲が広くなります。\
デフォルトはoffです。",
  "候補一覧で最終候補を表示している状態で次候補操作を行うと候補一覧を\
終了し、読みそのものを候補として表示するようになります。２打目の\
スペースキーを候補一覧表示にカスタマイズしている時などはonにしておく\
と便利です。デフォルトはoffです。",
  "バックスペースキーを打ったときに直前のローマ字かな変換された文字列が\
ローマ字に戻るかどうかを指定します。デフォルトはoffです。",
  "かな漢字変換が学習を行うかどうかを指定します。デフォルトはonです。",
  "候補一覧状態で候補を選択して単一候補表示状態に戻ったときに、カレント\
文節を次の文節に移動させるかどうかを指定します。offでカレント文節に移動\
します。onではカレント文節は変わりません。デフォルトはonです。",
  "最右文節で次文節へ移動しようとしたときに、確定するか否かを指定します。\
offを指定すると最左文節がカレント文節になります。デフォルトはoffです。",
  "単語登録で品詞を指定した後、詳細な品詞分類のための質問を行うか否かを\
指定します。onで質問を行い、offでは質問をしません。デフォルトはonです。",
  "変換キーを何回か押すと候補一覧表示が行われるようにする事\
ができます。ここではその回数を矢印キー(←,→)で指定します。\
0回にすると何回変換キーを\
押しても一覧表示が行われなくなります。デフォルトは2回で、2回変換キーを\
押すと一覧表示になります。",
  NULL
};
