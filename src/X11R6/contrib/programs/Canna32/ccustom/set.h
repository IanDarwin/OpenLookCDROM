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

/*
static	char	rcs_id[] = "@(#) 102.1 $Id: set.h,v 2.5 1994/02/03 07:31:50 hamada Exp $";
*/

/* #include "symbolname.h" */
/* #include "canna.h" */

#define MAX_DICS 16

char *kanjidicname[MAX_DICS];
int  nkanjidics = 0;

char *userdicname[MAX_DICS];
int  nuserdics = 0;

char *bushudicname[MAX_DICS];
int  nbushudics = 0;

char *localdicname[MAX_DICS];
int  nlocaldics = 0;

char *RomkanaTable;
int InitialMode =0;
int CursorWrap = 1;
int SelectDirect = -1;
int HexkeySelect = 1;
int BunsetsuKugiri = -1;

char *RengoGakushu;
char *KatakanaGakushu;
int ChBasedMove = 1;
int ReverseWidely = -1;
int Gakushu = 1;
int QuitIchiranIfEnd = -1;
int kakuteiIfEndOfBunsetsu = -1;
int stayAfterValidate = 1;
int BreakIntoRoman = -1;
int kouho_threshold = 2;
int gramaticalQuestion = 1;
int kCount = 1;
int kojin = 1;
int chikuji = -1;
int nKouhoBunsetsu = 16;
int abandonIllegalPhono = -1;
int hexCharacterDefiningStyle = -1;
int ReverseWord = -1;
int ckverbose, nothermodes;
int protocol_version = -1;
int server_version = -1;
int allowNextInput = 1;
int indexhankaku = -1;
int ignorecase = -1;
int romajiyuusen = -1;
int autosync = 1;
int nkeysuu = 500 ; 
int quicklyescape = -1;
int empty_mode, emptymap,nkeysup, keysup;
unsigned char *initfunc = NULL;
int forceKana, iListCB ;
int keepCursorPosition, CannaVersion;

newmode OtherModes[MAX_OTHER_MODES];

char *mode_mei[64];
char null_mode[64];

char *allKey[1024];
char *allFunc[1024];
char *alphaKey[256];
char *alphaFunc[256];
char *yomiganaiKey[512];
char *yomiganaiFunc[512];
char *yomiKey[1024];
char *yomiFunc[1024];
char *jishuKey[512];
char *jishuFunc[512];
char *tankouhoKey[512];
char *tankouhoFunc[512];
char *ichiranKey[512];
char *ichiranFunc[512];
char *zenHiraKey[256];
char *zenHiraFunc[256];
char *zenKataKey[256];
char *zenKataFunc[256];
char *zenAlphaKey[256];
char *zenAlphaFunc[256];
char *hanKataKey[256];
char *hanKataFunc[256];
char *hanAlphaKey[256];
char *hanAlphaFunc[256];

int NallKeyFunc = 0;
int NalphaKeyFunc = 0;
int NyomiganaiKeyFunc = 0;
int NyomiKeyFunc = 0;
int NjishuKeyFunc = 0;
int NtankouhoKeyFunc = 0;
int NichiranKeyFunc = 0;
int NzenHiraKeyFunc = 0;
int NzenKataKeyFunc = 0;
int NzenAlphaKeyFunc = 0;
int NhanKataKeyFunc = 0;
int NhanAlphaKeyFunc = 0;

char *keyCharMap[] = {               
  "Space",    "Delete",      "Nfer",     "Xfer",     "Up",
  "Left",     "Right",    "Down",     "Insert",   "Rollup",
  "Rolldown", "Home",    "Help",     "KeyPad",   "S-nfer",
  "S-xfer",   "S-up",     "S-left",   "S-right",  "S-down",
  "C-nfer",   "C-xfer",   "C-up",     "C-left",   "C-right",
  "C-down",   "F1",       "F2",       "F3",       "F4",
  "F5",       "F6",       "F7",       "F8",       "F9",
  "F10",      "PF1",      "PF2",      "PF3",      "PF4",
  "PF5",      "PF6",      "PF7",      "PF8",      "PF9",
  "PF10",
};

char returnKey[16];

char *cfuncList[] = {
  S_FN_Undefined,                /*  0 */
  S_FN_SelfInsert,               /*  1 */
  S_FN_FunctionalInsert,         /*  2 */
  S_FN_QuotedInsert,             /*  3 */
  S_FN_JapaneseMode,             /*  4 */
  S_FN_AlphaMode,                /*  5 */
  S_FN_HenkanNyuryokuMode,       /*  6 */
  S_FN_Forward,                  /*  7 */
  S_FN_Backward,                 /*  8 */
  S_FN_Next,                     /*  9 */
  S_FN_Prev,                     /* 10 */
  S_FN_BeginningOfLine,          /* 11 */
  S_FN_EndOfLine,                /* 12 */
  S_FN_DeleteNext,               /* 13 */
  S_FN_DeletePrevious,           /* 14 */
  S_FN_KillToEndOfLine,          /* 15 */
  S_FN_Henkan,                   /* 16 */
  S_FN_Kakutei,                  /* 17 */
  S_FN_Extend,                   /* 18 */
  S_FN_Shrink,                   /* 19 */
  S_FN_AdjustBunsetsu,           /* 20 */
  S_FN_Quit,                     /* 21 */
  S_FN_ConvertAsHex,             /* 22 */
  S_FN_ConvertAsBushu,           /* 23 */
  S_FN_KouhoIchiran,             /* 24 */
  S_FN_BubunMuhenkan,            /* 25 */
  S_FN_Zenkaku,                  /* 26 */
  S_FN_Hankaku,                  /* 27 */
  S_FN_ToUpper,                  /* 28 */
  S_FN_Capitalize,               /* 29 */
  S_FN_ToLower,                  /* 30 */
  S_FN_Hiragana,                 /* 31 */
  S_FN_Katakana,                 /* 32 */
  S_FN_Romaji,                   /* 33 */
  S_FN_BaseHiragana,             /* 34 */
  S_FN_BaseKatakana,             /* 35 */
  S_FN_BaseEisu,                 /* 36 */
  S_FN_BaseZenkaku,              /* 37 */
  S_FN_BaseHankaku,              /* 38 */
  S_FN_BaseKana,                 /* 39 */
  S_FN_BaseKakutei,              /* 40 */
  S_FN_BaseHenkan,               /* 41 */
  S_FN_BaseHiraKataToggle,       /* 42 */
  S_FN_BaseZenHanToggle,         /* 43 */
  S_FN_BaseKanaEisuToggle,       /* 44 */
  S_FN_BaseKakuteiHenkanToggle,  /* 45 */
  S_FN_BaseRotateForward,        /* 46 */
  S_FN_BaseRotateBackward,       /* 47 */
  S_FN_ExtendMode,               /* 48 */
  S_FN_HexMode,                  /* 49 */
  S_FN_BushuMode,                /* 50 */
  S_FN_KigouMode,                /* 51 */
  S_FN_ZenHiraKakuteiMode,       /* 52 */
  S_FN_ZenKataKakuteiMode,       /* 53 */
  S_FN_HanKataKakuteiMode,       /* 54 */
  S_FN_ZenAlphaKakuteiMode,      /* 55 */
  S_FN_HanAlphaKakuteiMode,      /* 56 */
  S_FN_HenkanOrSelfInsert,       /* 57 */
  S_FN_HenkanOrNothing,          /* 58 */
  S_FN_ChangeServerMode,         /* 59 */
  S_FN_DisconnectServer,         /* 60 */
  S_FN_ShowServer,               /* 61 */
  S_FN_ShowGakushu,              /* 62 */
  S_FN_ShowVersion,              /* 63 */
  S_FN_ShowPhonogramFile,        /* 64 */
  S_FN_ShowCannaFile,            /* 65 */
  S_FN_SyncDic,                  /* 66 */
  NULL
};

char *funcList[] = {
  "Undefine",
  "SelfInsert",
  "SelfInsert",
  "QuotedInsert",
  "JapaneseMode",
  "AlphaMode",
  "HenkanNyuryokuMode",
  "ZenHiraKakuteiMode",
  "ZenKataKakuteiMode",
  "HanKataKakuteiMode",
  "ZenAlphaKakuteiMode",
  "HanAlphaKakuteiMode",
  "HexMode",
  "BushuMode",
  "KigouMode",
  "Forward",
  "Backward",
  "Next",
  "Previous",
  "BeginningOfLine",
  "EndOfLine",
  "DeleteNext",
  "DeletePrevious",
  "KillToEndOfLine",
  "Henkan",
  "Kakutei",
  "Extend",
  "Shrink",
  "Quit",
  "Touroku",
  "ConvertAsHex",
  "ConvertAsBushu",
  "KouhoIchiran",
  "BubunMuhenkan",
  "Zenkaku",
  "Hankaku",
  "ToUpper",
  "Capitalize",
  "ToLower",
  "Hiragana",
  "Katakana",
  "Romaji",
  "FuncSequence",
  "UseOtherKeymap",
  "NextKouho",
  "PrevKouho",
  "NULL"
};
