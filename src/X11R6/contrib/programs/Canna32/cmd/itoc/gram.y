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

/* -*-Mode: TEXT-*- $Id: gram.y,v 1.12 1992/07/28 12:37:35 kon Exp $ */

%{
static char rcs_id[]= "@(#) 102.1 $Id: gram.y,v 1.12 1992/07/28 12:37:35 kon Exp $";

#include <stdio.h>
#include <canna/jrkanji.h>
#include "symbolname.h"

#define PREVK 3
#define NEXTK 2
#define YES 1
#define NO  0

#define IROHA_FN_ZenHiraKakuteiMode     201 /* なくなったモードの  */
#define IROHA_FN_ZenKataKakuteiMode     202 /* 名前を、ここでとり  */
#define IROHA_FN_HanKataKakuteiMode     203 /* あえず復活させてい  */
#define IROHA_FN_ZenAlphaKakuteiMode    204 /* る。CANNA_FNが200   */
#define IROHA_FN_HanAlphaKakuteiMode    205 /* を越えた時が危ない。*/ 

static char *ptr;
static unsigned char actbuff[256],keybuff[128];
static int special = NO;
static int mode, localmode = NO;
static int firstKey, i = 0, j = 0;
extern InitialMode, CursorWrap, SelectDirect, chikuji;
extern HexkeySelect, Gakushu, BreakIntoRoman, BunsetsuKugiri;
extern QuitIchiranIfEnd, stayAfterValidate, kakuteiIfEndOfBunsetsu;
extern kouho_threshold, grammaticalQuestion, forceKana, kCount;
extern iListCB;
extern char *RomkanaTable, *RengoGakushu, *KatakanaGakushu;
extern char IROHA_rcfilename[];
static int offset = 0;
static char *TNIL();
static void mode_style();
static void defZenHiraKakutei();
static void defZenKataKakutei(), defZenAlphaKakutei();
static void defHanKataKakutei(), defHanAlphaKakutei();
static void defZenKataHenkan(), defZenAlphaHenkan();
static void defHanKataHenkan(), defHanAlphaHenkan();

static isZenHiraKakutei = 0;
static isZenKataKakutei = 0;
static isZenAlphaKakutei = 0;
static isHanKataKakutei = 0;
static isHanAlphaKakutei = 0;
static isZenKataHenkan = 0;
static isZenAlphaHenkan = 0;
static isHanKataHenkan = 0;
static isHanAlphaHenkan = 0;

int inhibitCR;
char *memo;

extern int yylineno;

static changeKeyfunc(), changeKeyfuncOfAll();
%}

%union
{
    int num;
    char *ptr;
};

%token <num> LB RB LP RP KEY NEWLINE COMMA
%token <num> ROMKANATABLE INITIALMODE HENKANNYURYOKUMODE 
%token <num> ZENHIRAHENKANMODE 
%token <num> ZENKATAHENKANMODE ZENALPHAHENKANMODE 
%token <num> HANKATAHENKANMODE HANALPHAHENKANMODE
%token <num> ZENHIRAKAKUTEIMODE 
%token <num> ZENKATAKAKUTEIMODE ZENALPHAKAKUTEIMODE 
%token <num> HANKATAKAKUTEIMODE HANALPHAKAKUTEIMODE
%token <num> BUNSETSUKUGIRI
%token <num> CURSORWRAP ON_FLAG OFF_FLAG
%token <num> SELECTEDIRECT
%token <num> DICTIONARY USERDIC BUSHUDIC RENGODIC KANADIC LOCALDIC
%token <num> CHARACTERBASEDMOVE REVERSEWIDELY GAKUSHU BREAKINTOROMAN
%token <num> QUITIFENDOFICHIRAN
%token <num> ALPHAMODE
%token <num> QUOTEDINSERT
%token <num> KAKUTEI EXTEND SHRINK SHINSHUKU
%token <num> TOUROKU FORWARD BACKWARD PREVIOUS NEXT
%token <num> BEGINNINGOFLINE ENDOFLINE DELETENEXT DELETEPREVIOUS
%token <num> KILLTOENDOFLINE HENKAN QUIT SELFINSERT
%token <num> ALPHA JAPANESEMODE
%token <num> YOMIGANAI HEXMODE BUSHUMODE KIGOUMODE
%token <num> YOMI NEXTCHARACTERSET PREVCHARACTERSET CONVERTASHEX
%token <num> CONVERTASBUSHU
%token <num> MOJISHU ZENKAKU HANKAKU TOUPPER CAPITALIZE TOLOWER
%token <num> TANKOUHO MUHENKAN NEXTKOUHO PREVKOUHO KOUHOICHIRAN BUBUNMUHENKAN
%token <num> ICHIRAN PREVKOUHORETSU NEXTKOUHORETSU BEGINNINGOFKOUHO
%token <num> ENDOFKOUHO AUTO
%token <num> KAKUTEINYURYOKU EXTENDMODE
%token <num> HEX BUSHU HENKANNYURYOKU ZENHIRAKAKUTEI
%token <num> ZENKATAKAKUTEI ZENALPHAKAKUTEI HANKATAKAKUTEI HANALPHAKAKUTEI
%token <num> SELECTDIRECT HEXKEYSELECT
%token <num> MODESTRING STAYAFTERVALIDATE KAKUTEIIFENDOFBUNSETSU
%token <num> HIRAGANA KATAKANA ROMAJI NHENKANFORICHIRAN NUMBER
%token <num> KIGO YESNO RUSSIAN GREEK LINE CHANGINGSERVER HENKANMETHOD
%token <num> DELETEDIC TOUROKUHINSHI TOUROKUDIC ZENKATAHENKAN 
%token <num> ZENALPHAHENKAN HANKATAHENKAN HANALPHAHENKAN MOUNTDIC  
%token <num> GRAMMATICALQUESTION FORCEKANA KOUHOCOUNT
%token <num> INHIBITLISTCALLBACK
%token <ptr> STRING

%type <ptr> string initMode
%type <num> flag number

%start cookrc

%%
cookrc		: stmts newlines
		;

stmts		: /* Empty */
		| stmt
		| stmts newline stmt
		;

stmt		: error
		| ROMKANATABLE string	{
		  printf("(setq %s \"%s\")", S_VA_RomkanaTable, $2); }
		| INITIALMODE initMode	{
		  if ($2)
		    printf("(%s '(%s  %s))",
			   S_SetInitFunc, S_FN_JapaneseMode, $2);
		  else
		    printf("(%s '(%s))", S_SetInitFunc, S_AlphaMode); }
		| CURSORWRAP flag	{
		  printf("(setq %s %s)", S_VA_CursorWrap, TNIL($2)); }
		| SELECTDIRECT flag	{
		  printf("(setq %s %s)", S_VA_SelectDirect, TNIL($2)); }
		| HEXKEYSELECT flag	{
		  printf("(setq %s %s)", S_VA_NumericalKeySelect, 
			                   TNIL($2)); }
		| BUNSETSUKUGIRI flag	{
		  printf("(setq %s %s)", S_VA_BunsetsuKugiri, TNIL($2)); }
		| DICTIONARY string	{ 
		  printf("(%s \"%s\")", S_FN_UseDictionary, $2); }
		| USERDIC string	{
		  printf("(%s :user \"%s\")", S_FN_UseDictionary, $2); }
		| BUSHUDIC string	{
		  printf("(%s :bushu \"%s\")", S_FN_UseDictionary, $2); }
		| LOCALDIC string	{
		  printf("(%s :local \"%s\")", S_FN_UseDictionary, $2); }
		| RENGODIC string	{
		  printf("(%s :rengo \"%s\")", S_FN_UseDictionary, $2); }
		| KANADIC string	{
		  printf("(%s :kana \"%s\")", S_FN_UseDictionary, $2); }
		| CHARACTERBASEDMOVE flag {
		  printf("(setq %s %s)", S_VA_CharacterBasedMove, 
			                   TNIL($2)); }
		| REVERSEWIDELY flag	{
		  printf("(setq %s %s)", S_VA_ReverseWidely, TNIL($2)); }
		| GAKUSHU flag		{
		  printf("(setq %s %s)", S_VA_Gakushu, TNIL($2)); }
		| QUITIFENDOFICHIRAN flag {
		  printf("(setq %s %s)", S_VA_QuitIfEOIchiran, TNIL($2)); }
		| KAKUTEIIFENDOFBUNSETSU flag {
		  printf("(setq %s %s)", S_VA_KakuteiIfEOBunsetsu,
			 TNIL($2)); }
		| STAYAFTERVALIDATE flag {
		  printf("(setq %s %s)", S_VA_StayAfterValidate, 
			                   TNIL($2)); }
		| BREAKINTOROMAN flag	{
		  printf("(setq %s %s)", S_VA_BreakIntoRoman, TNIL($2)); }
		| NHENKANFORICHIRAN number {
		  printf("(setq %s %d)", S_VA_NHenkanForIchiran, $2); }
		| NHENKANFORICHIRAN KEY {
			if ('0' <= $2 && $2 <= '9') {
			  printf("(setq %s %d)", S_VA_NHenkanForIchiran,
				 $2 - '0');
			}
			else {
			  yyerror(IROHA_rcfilename);
			} } 
		| GRAMMATICALQUESTION flag {
		  printf("(setq %s %s)", S_VA_GrammaticalQuestion,
			                   TNIL($2)); }
		| FORCEKANA flag {
		  printf("(setq %s %s)", S_VA_ForceKana, TNIL($2)); }
		| KOUHOCOUNT flag {
		  printf("(setq %s %s)", S_VA_KouhoCount, TNIL($2)); }
                | AUTO flag {
		  printf("(setq %s %s)", S_VA_Auto, TNIL($2)); }
		| INHIBITLISTCALLBACK flag {
		  printf("(setq %s %s)", S_VA_InhibitListCallback,
			                   TNIL($2)); }
		| keymapping

		| ALPHA newlines	
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_AlphaMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }
		| YOMIGANAI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_YomiganaiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }
		| YOMI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_YomiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| MOJISHU newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_MojishuMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| TANKOUHO newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_TankouhoMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| ICHIRAN newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))", S_IchiranMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| HENKANNYURYOKU newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
				 S_YomiganaiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| ZENHIRAKAKUTEI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
			  S_ZenHiraKakuteiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| ZENKATAKAKUTEI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
				 S_ZenKataKakuteiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| ZENALPHAKAKUTEI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
			  S_ZenAlphaKakuteiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| HANKATAKAKUTEI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
			  S_HanKataKakuteiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| HANALPHAKAKUTEI newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
			  S_HanAlphaKakuteiMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

		| SHINSHUKU newlines
			{ offset += 2; localmode = YES;
			  printf("(let ((mode '%s))",
			  S_ShinshukuMode); }
		  keymappingList
			{ offset -= 2; localmode = NO;
			  printf("  )"); }

                | MODESTRING LP ALPHA RP string
			{ mode_style(S_AlphaMode, $5); }
                | MODESTRING LP YOMIGANAI RP string
			{ mode_style(S_YomiganaiMode, $5); }
                | MODESTRING LP HEX RP string
			{ mode_style(S_HexMode, $5); }
                | MODESTRING LP BUSHU RP string
			{ mode_style(S_BushuMode, $5);}
                | MODESTRING LP HENKANNYURYOKU RP string
			{ mode_style(S_HenkanNyuuryokuMode, $5);}
                | MODESTRING LP ZENHIRAKAKUTEI RP string
			{ mode_style(S_ZenHiraKakuteiMode, $5);}
                | MODESTRING LP ZENKATAKAKUTEI RP string
			{ mode_style(S_ZenKataKakuteiMode, $5);}
                | MODESTRING LP ZENALPHAKAKUTEI RP string
			{ mode_style(S_ZenAlphaKakuteiMode, $5);}
                | MODESTRING LP HANKATAKAKUTEI RP string
			{ mode_style(S_HanKataKakuteiMode, $5);}
                | MODESTRING LP HANALPHAKAKUTEI RP string
			{ mode_style(S_HanAlphaKakuteiMode, $5);}
                | MODESTRING LP SHINSHUKU RP string
			{ mode_style(S_ShinshukuMode, $5); }
                | MODESTRING LP KIGO RP string
			{ mode_style(S_KigouMode, $5); }
                | MODESTRING LP MOJISHU RP string
			{ mode_style(S_MojishuMode, $5); }
                | MODESTRING LP TANKOUHO RP string
			{ mode_style(S_TankouhoMode, $5); }
                | MODESTRING LP ICHIRAN RP string
			{ mode_style(S_IchiranMode, $5); }
                | MODESTRING LP YESNO RP string
			{ mode_style(S_YesNoMode, $5); }
                | MODESTRING LP EXTEND RP string
			{ mode_style(S_ExtendMode, $5); }
                | MODESTRING LP RUSSIAN RP string
			{ mode_style(S_RussianMode, $5); }
                | MODESTRING LP GREEK  RP string
			{ mode_style(S_GreekMode, $5); }
                | MODESTRING LP LINE RP string
			{ mode_style(S_LineMode, $5); }
                | MODESTRING LP CHANGINGSERVER RP string
			{ mode_style(S_ChangingServerMode, $5); }
                | MODESTRING LP HENKANMETHOD RP string
			{ mode_style(S_HenkanMethodMode, $5); }
                | MODESTRING LP DELETEDIC RP string
			{ mode_style(S_DeleteDicMode, $5); }
                | MODESTRING LP TOUROKU RP string
			{ mode_style(S_TourokuMode, $5); }
                | MODESTRING LP TOUROKUHINSHI RP string
			{ mode_style(S_TourokuHinshiMode, $5); }
                | MODESTRING LP TOUROKUDIC RP string
			{ mode_style(S_TourokuDicMode, $5); }
                | MODESTRING LP QUOTEDINSERT RP string
			{ mode_style(S_QuotedInsertMode, $5); }
                | MODESTRING LP BUBUNMUHENKAN RP string
			{ mode_style(S_BubunMuhenkanMode, $5); }
                | MODESTRING LP ZENKATAHENKAN RP string
			{ mode_style(S_ZenKataHenkanMode, $5); }
                | MODESTRING LP HANKATAHENKAN RP string
			{ mode_style(S_HanKataHenkanMode, $5); }
                | MODESTRING LP ZENALPHAHENKAN RP string
			{ mode_style(S_ZenAlphaHenkanMode, $5);}
                | MODESTRING LP HANALPHAHENKAN RP string
			{ mode_style(S_HanAlphaHenkanMode, $5);}
                | MODESTRING LP MOUNTDIC RP string
			{ mode_style(S_MountDicMode, $5); }
		;

initMode	: HENKANNYURYOKUMODE	{ $$ = ""; }
		| ZENHIRAHENKANMODE	{ $$ = ""; }
		| ZENKATAHENKANMODE	{ $$ = S_ZenKataHenkanMode; }
		| HANKATAHENKANMODE	{ $$ = S_HanKataHenkanMode; }
		| ZENALPHAHENKANMODE	{ $$ = S_ZenAlphaHenkanMode; }
		| HANALPHAHENKANMODE	{ $$ = S_HanAlphaHenkanMode; }
		| ZENHIRAKAKUTEIMODE	{ $$ = S_ZenHiraKakuteiMode; }
		| ZENKATAKAKUTEIMODE	{ $$ = S_ZenKataKakuteiMode; }
		| ZENALPHAKAKUTEIMODE	{ $$ = S_ZenAlphaKakuteiMode;}
		| HANKATAKAKUTEIMODE	{ $$ = S_HanKataKakuteiMode; }
		| HANALPHAKAKUTEIMODE	{ $$ = S_HanAlphaKakuteiMode;}
		| ALPHAMODE		{ $$ = NULL; }
		;

flag		: ON_FLAG
		| OFF_FLAG
		;

number		: NUMBER

string		: STRING	{ 
				  if ($1) {
				    ptr = (char*)malloc(strlen($1) +1);
				    strcpy(ptr, $1);
				    RemoveDblQuote(ptr);
				    $$ = ptr;
				  }
				  else {
				    $$ = (char *)0;
				  }
				}

newline		: NEWLINE  { if (memo) {
                               printf(";;%s\n", memo);
			       free(memo);
			       memo = NULL;
			     }
			     else printf("\n"); }
		| newline NEWLINE   { if (memo) {
		                        printf(";;%s\n", memo);
			                free(memo);
			                memo = NULL;
			              }
			              else printf("\n"); }
		;

newlines	: /* empty */
		| newline
		;

keymappingList	: LB keymappings newlines RB
		;

keymappings	: /* empty */
		| keymappings newline keymapping
		;

keymapping	: seq

seq		: acts keys	
			{ if (i == 1 && j== 1) {
			    if (special == NO) {
			      chgKeyfunc((int)keybuff[0],(int)actbuff[0]);
			    }
			  }	
			  else {
			    actbuff[i] = 0;
			    keybuff[j] = 255;
			    if (j > 1) {
			      chgKeyfunc((int)keybuff[0],
					 CANNA_FN_UseOtherKeymap);
			    }
			    else {
			      chgKeyfunc((int)keybuff[0],
					 CANNA_FN_FuncSequence);
			    }
                          }
			  special = NO;
			  i = 0;
			  j = 0;
			}

acts		: act
		| acts COMMA act
		;

keys		: KEY {
		       if (special == NEXTK && i == 1 && j == 0) {
			 chgKeyfunc($1, CANNA_FN_Next);
/*			 changeKeyfunc(CANNA_MODE_IchiranMode,
			 $1, CANNA_FN_Forward,NULL,NULL); */
		       }
		       if (special == PREVK && i == 1 && j == 0){
			 chgKeyfunc($1, CANNA_FN_Prev);
/*			 changeKeyfunc(CANNA_MODE_IchiranMode,
			      $1, CANNA_FN_Backward,NULL,NULL); */
		       }
                       keybuff[j++] = $1;
		      }
		| keys COMMA KEY { keybuff[j++] = $3; }
		;

act		: FORWARD	{ actbuff[i++] = CANNA_FN_Forward; }
		| BACKWARD	{ actbuff[i++] = CANNA_FN_Backward; }
		| NEXT		{ actbuff[i++] = CANNA_FN_Next; }
		| PREVIOUS	{ actbuff[i++] = CANNA_FN_Prev; }
		| BEGINNINGOFLINE
				{ actbuff[i++] = CANNA_FN_BeginningOfLine; }
		| ENDOFLINE	{ actbuff[i++] = CANNA_FN_EndOfLine; }
		| DELETENEXT	{ actbuff[i++] = CANNA_FN_DeleteNext; }
		| DELETEPREVIOUS
				{ actbuff[i++] = CANNA_FN_DeletePrevious; }
		| KILLTOENDOFLINE
				{ actbuff[i++] = CANNA_FN_KillToEndOfLine; }
		| HENKAN	{ actbuff[i++] = CANNA_FN_Henkan; }
		| KAKUTEI	{ actbuff[i++] = CANNA_FN_Kakutei; }
		| SHRINK	{ actbuff[i++] = CANNA_FN_Shrink; }
		| EXTEND	{ actbuff[i++] = CANNA_FN_Extend; }
		| SHINSHUKU	{ actbuff[i++] = CANNA_FN_AdjustBunsetsu; }
		| QUIT		{ actbuff[i++] = CANNA_FN_Quit; }
		| SELFINSERT	{ actbuff[i++] = CANNA_FN_FunctionalInsert; }
		| QUOTEDINSERT	{ actbuff[i++] = CANNA_FN_QuotedInsert; }
		| TOUROKU	{ actbuff[i++] = CANNA_FN_ExtendMode; }
		| EXTENDMODE	{ actbuff[i++] = CANNA_FN_ExtendMode; }
		| NEXTCHARACTERSET
				{ actbuff[i++] = CANNA_FN_Next; }
		| PREVCHARACTERSET
				{ actbuff[i++] = CANNA_FN_Prev; }
		| CONVERTASHEX	{ actbuff[i++] = CANNA_FN_ConvertAsHex; }
		| CONVERTASBUSHU
				{ actbuff[i++] = CANNA_FN_ConvertAsBushu; }

		| ZENKAKU	{ actbuff[i++] = CANNA_FN_Zenkaku; }

		| HANKAKU	{ actbuff[i++] = CANNA_FN_Hankaku; }

		| TOUPPER	{ actbuff[i++] = CANNA_FN_ToUpper; }

		| CAPITALIZE	{ actbuff[i++] = CANNA_FN_Capitalize; }

		| TOLOWER	{ actbuff[i++] = CANNA_FN_ToLower; }

		| MUHENKAN	{ actbuff[i++] = CANNA_FN_DeletePrevious; }

		| NEXTKOUHO
			{ 
			 if (localmode == YES) {
			    if (mode == CANNA_MODE_IchiranMode) {
			       actbuff[i++] = CANNA_FN_Forward;
			    }
			    else {
			       actbuff[i++] = CANNA_FN_Next;
			    }
		 	   }
			 else {
			    actbuff[i++] = CANNA_FN_Next;
			    special = NEXTK;
			 }
		        }

		| PREVKOUHO
			{ 
			 if (localmode == YES) {
			    if (mode == CANNA_MODE_IchiranMode) {
			       actbuff[i++] = CANNA_FN_Backward;
			    }
			    else {
			       actbuff[i++] = CANNA_FN_Prev;
			    }
			 }
			 else {
			    actbuff[i++] = CANNA_FN_Prev;
                            special = PREVK;
		         }
			}

		| KOUHOICHIRAN	{ actbuff[i++] = CANNA_FN_KouhoIchiran; }

		| BUBUNMUHENKAN	{ actbuff[i++] = CANNA_FN_BubunMuhenkan; }

		| NEXTKOUHORETSU
				{ actbuff[i++] = CANNA_FN_Next; }

		| PREVKOUHORETSU
				{ actbuff[i++] = CANNA_FN_Prev; }

		| BEGINNINGOFKOUHO
				{ actbuff[i++] = CANNA_FN_BeginningOfLine; }

		| ENDOFKOUHO	{ actbuff[i++] = CANNA_FN_EndOfLine; }

		| ALPHAMODE	{ actbuff[i++] = CANNA_FN_AlphaMode; }

		| JAPANESEMODE	{ actbuff[i++] = CANNA_FN_JapaneseMode; }

		| HEXMODE	{ actbuff[i++] = CANNA_FN_HexMode; }

		| BUSHUMODE	{ actbuff[i++] = CANNA_FN_BushuMode; }

		| KIGOUMODE	{ actbuff[i++] = CANNA_FN_KigouMode; }

		| HENKANNYURYOKUMODE
				{ actbuff[i++] = CANNA_FN_HenkanNyuryokuMode; }

		| ZENHIRAKAKUTEIMODE
				{ actbuff[i++] = IROHA_FN_ZenHiraKakuteiMode; }

		| ZENKATAKAKUTEIMODE
				{ actbuff[i++] = IROHA_FN_ZenKataKakuteiMode; }

		| HANKATAKAKUTEIMODE
				{ actbuff[i++] = IROHA_FN_HanKataKakuteiMode; }

		| ZENALPHAKAKUTEIMODE
			       { actbuff[i++] = IROHA_FN_ZenAlphaKakuteiMode; }

		| HANALPHAKAKUTEIMODE
			       { actbuff[i++] = IROHA_FN_HanAlphaKakuteiMode; }

		| HIRAGANA	{ actbuff[i++] = CANNA_FN_Hiragana; }
		| KATAKANA	{ actbuff[i++] = CANNA_FN_Katakana; }
		| ROMAJI	{ actbuff[i++] = CANNA_FN_Romaji; }
		;

%%
extern IROHA_ParseError;

static
yyerror(s)
char *s;
{
   char buf[256];
   void addWarningMesg();

   sprintf(buf, "%s: syntax error, line %d", IROHA_rcfilename, yylineno);
   addWarningMesg(buf);
   IROHA_ParseError = 1;
}

static
RemoveDblQuote(str)
char *str;
{
    strcpy(str, &str[1]);
    str[strlen(str)-1] = '\0';
}

static char *
TNIL(b)
int b;
{
  return b ? "t" : "nil";
}

static
chgKeyfunc(key, fnum)
int key, fnum;
{
    if (localmode)
      changeKeyfunc(mode, key, fnum, actbuff, keybuff);
    else
      changeKeyfuncOfAll(key, fnum, actbuff, keybuff);
}

static void chkeyrest(), chkeyfn();

static
changeKeyfunc(mode, key, fnum, actbuff, keybuff)
int mode;
int key;
unsigned char fnum;
unsigned char *actbuff, *keybuff;
{
  /* (set-key mode "keysequence" 'function)
     (set-key mode "keysequence" (sequence 'function1 'function2 ...)) */
  int i;

  for (i = 0 ; i < offset ; i++) { /* offset って２しかないのでは？ */
    putchar(' ');
  }
  if (key == CANNA_KEY_Undefine) {
    printf("(%s mode", S_UnbindKey);
    chkeyfn(fnum, actbuff);
  }
  else {
    printf("(%s mode \"", S_SetKey);
    chkeyrest(key, fnum, actbuff, keybuff);
  }
  return 0;
}

static
changeKeyfuncOfAll(key, fnum, actbuff, keybuff)
int key;
unsigned char fnum;
unsigned char *actbuff, *keybuff;
{
  if (key == CANNA_KEY_Undefine) {
    printf("(%s", S_GUnbindKey);
    chkeyfn(fnum, actbuff);
  }
  else {
    printf("(%s \"", S_GSetKey);
    chkeyrest(key, fnum, actbuff, keybuff);
  }
  return 0;
}

static void printkey(), printaction();

static void
chkeyrest(key, fnum, actbuff, keybuff)
int key;
unsigned char fnum;
unsigned char *actbuff, *keybuff;
{
  unsigned char *p;

  if (fnum == CANNA_FN_UseOtherKeymap) { /* キーシーケンスなら */
    for (p = keybuff ; *p != 255 && *p != CANNA_KEY_Undefine ; p++) {
      printkey(*p);
    }
  }
  else {
    printkey(key);
  }
  printf("\"");

  chkeyfn(fnum, actbuff);
}

static void
chkeyfn(fnum, actbuff)
unsigned char fnum;
unsigned char *actbuff;
{
  unsigned char *p;

  if (fnum == CANNA_FN_FuncSequence ||
      (fnum == CANNA_FN_UseOtherKeymap) && actbuff[1]) {
    printf(" (sequence");
    for (p = actbuff ; *p ; p++) {
      printf(" \'");
      printaction(*p);
    }
    putchar(')');
  }
  else if (fnum == CANNA_FN_UseOtherKeymap) {
    printf(" \'");
    printaction(actbuff[0]);
  }
  else {
    printf(" \'");
    printaction(fnum);
  }
  printf(")");
}

static void
printkey(key)
unsigned char key;
{
  if ((unsigned)('A' - '@') <= (unsigned)key &&
      (unsigned)key <= (unsigned)('Z' - '@')) {
    printf("\\C-%c", key + 'a' - 'A' + '@');
  }
  else if (key < ' ') {
    printf("\\C-%c", key + '@');
  }
  else if (key <= '~') {
    printf("%c", key);
  }
  else if (key == 0x7f) {
    printf("\\Delete");
  }
  else {
    switch (key) {
    case CANNA_KEY_Nfer:
      printf("\\Nfer");
      break;
    case CANNA_KEY_Xfer:
      printf("\\Xfer");
      break;
    case CANNA_KEY_Up:
      printf("\\Up");
      break;
    case CANNA_KEY_Left:
      printf("\\Left");
      break;
    case CANNA_KEY_Right:
      printf("\\Right");
      break;
    case CANNA_KEY_Down:
      printf("\\Down");
      break;
    case CANNA_KEY_Insert:
      printf("\\Insert");
      break;
    case CANNA_KEY_Rollup:
      printf("\\Rollup");
      break;
    case CANNA_KEY_Rolldown:
      printf("\\Rolldown");
      break;
    case CANNA_KEY_Home:
      printf("\\Home");
      break;
    case CANNA_KEY_Help:
      printf("\\Help");
      break;
    case CANNA_KEY_KP_Key:
      printf("\\Key");
      break;
    case CANNA_KEY_Shift_Nfer:
      printf("\\S-Nfer");
      break;
    case CANNA_KEY_Shift_Xfer:
      printf("\\S-Xfer");
      break;
    case CANNA_KEY_Shift_Up:
      printf("\\S-Up");
      break;
    case CANNA_KEY_Shift_Left:
      printf("\\S-Left");
      break;
    case CANNA_KEY_Shift_Right:
      printf("\\S-Right");
      break;
    case CANNA_KEY_Shift_Down:
      printf("\\S-Down");
      break;
    case CANNA_KEY_Cntrl_Nfer:
      printf("\\C-Nfer");
      break;
    case CANNA_KEY_Cntrl_Xfer:
      printf("\\C-Xfer");
      break;
    case CANNA_KEY_Cntrl_Up:
      printf("\\C-Up");
      break;
    case CANNA_KEY_Cntrl_Left:
      printf("\\C-Left");
      break;
    case CANNA_KEY_Cntrl_Right:
      printf("\\C-Right");
      break;
    case CANNA_KEY_Cntrl_Down:
      printf("\\C-Down");
      break;
    case CANNA_KEY_F1:
      printf("\\F1");
      break;
    case CANNA_KEY_F2:
      printf("\\F2");
      break;
    case CANNA_KEY_F3:
      printf("\\F3");
      break;
    case CANNA_KEY_F4:
      printf("\\F4");
      break;
    case CANNA_KEY_F5:
      printf("\\F5");
      break;
    case CANNA_KEY_F6:
      printf("\\F6");
      break;
    case CANNA_KEY_F7:
      printf("\\F7");
      break;
    case CANNA_KEY_F8:
      printf("\\F8");
      break;
    case CANNA_KEY_F9:
      printf("\\F9");
      break;
    case CANNA_KEY_F10:
      printf("\\F10");
      break;
    case CANNA_KEY_PF1:
      printf("\\Pf1");
      break;
    case CANNA_KEY_PF2:
      printf("\\Pf2");
      break;
    case CANNA_KEY_PF3:
      printf("\\Pf3");
      break;
    case CANNA_KEY_PF4:
      printf("\\Pf4");
      break;
    case CANNA_KEY_PF5:
      printf("\\Pf5");
      break;
    case CANNA_KEY_PF6:
      printf("\\Pf6");
      break;
    case CANNA_KEY_PF7:
      printf("\\Pf7");
      break;
    case CANNA_KEY_PF8:
      printf("\\Pf8");
      break;
    case CANNA_KEY_PF9:
      printf("\\Pf9");
      break;
    case CANNA_KEY_PF10:
      printf("\\Pf10");
      break;
    case CANNA_KEY_Undefine:
      printf("\\Undefine"); /* おかしい！ */
      break;
    }
  }
}

static void
printaction(action)
unsigned char action;
{
  switch (action) {
  case CANNA_FN_Undefined:
    printf("%s", S_FN_Undefined);
    break;
  case CANNA_FN_SelfInsert:
    printf("%s", S_FN_SelfInsert);
    break;
  case CANNA_FN_FunctionalInsert:
    printf("%s", S_FN_FunctionalInsert);
    break;
  case CANNA_FN_QuotedInsert:
    printf("%s", S_FN_QuotedInsert);
    break;
  case CANNA_FN_JapaneseMode:
    printf("%s", S_FN_JapaneseMode);
    break;
  case CANNA_FN_AlphaMode:
    printf("%s", S_FN_AlphaMode);
    break;
  case CANNA_FN_HenkanNyuryokuMode:
    printf("%s", S_FN_HenkanNyuryokuMode);
    break;
  case CANNA_FN_BaseHiragana:
    printf("%s", S_FN_BaseHiragana);
    break;
  case CANNA_FN_BaseKatakana:
    printf("%s", S_FN_BaseKatakana);
    break;
  case CANNA_FN_BaseEisu:
    printf("%s", S_FN_BaseEisu);
    break;
  case CANNA_FN_BaseZenkaku:
    printf("%s", S_FN_BaseZenkaku);
    break;
  case CANNA_FN_BaseHankaku:
    printf("%s", S_FN_BaseHankaku);
    break;
  case CANNA_FN_BaseKakutei:
    printf("%s", S_FN_BaseKakutei);
    break;
  case CANNA_FN_BaseHenkan:
    printf("%s", S_FN_BaseHenkan);
    break;
  case CANNA_FN_HexMode:
    printf("%s", S_FN_HexMode);
    break;
  case CANNA_FN_BushuMode:
    printf("%s", S_FN_BushuMode);
    break;
  case CANNA_FN_KigouMode:
    printf("%s", S_FN_KigouMode);
    break;
  case CANNA_FN_Forward:
    printf("%s", S_FN_Forward);
    break;
  case CANNA_FN_Backward:
    printf("%s", S_FN_Backward);
    break;
  case CANNA_FN_Next:
    printf("%s", S_FN_Next);
    break;
  case CANNA_FN_Prev:
    printf("%s", S_FN_Prev);
    break;
  case CANNA_FN_BeginningOfLine:
    printf("%s", S_FN_BeginningOfLine);
    break;
  case CANNA_FN_EndOfLine:
    printf("%s", S_FN_EndOfLine);
    break;
  case CANNA_FN_DeleteNext:
    printf("%s", S_FN_DeleteNext);
    break;
  case CANNA_FN_DeletePrevious:
    printf("%s", S_FN_DeletePrevious);
    break;
  case CANNA_FN_KillToEndOfLine:
    printf("%s", S_FN_KillToEndOfLine);
    break;
  case CANNA_FN_Henkan:
    printf("%s", S_FN_Henkan);
    break;
  case CANNA_FN_Kakutei:
    printf("%s", S_FN_Kakutei);
    break;
  case CANNA_FN_Extend:
    printf("%s", S_FN_Extend);
    break;
  case CANNA_FN_Shrink:
    printf("%s", S_FN_Shrink);
    break;
  case CANNA_FN_AdjustBunsetsu:
    printf("%s", S_FN_AdjustBunsetsu);
    break;
  case CANNA_FN_Quit:
    printf("%s", S_FN_Quit);
    break;
  case CANNA_FN_ExtendMode: /* same as CANNA_FN_TourokuMode */
    printf("%s", S_FN_ExtendMode);
    break;
  case CANNA_FN_ConvertAsHex:
    printf("%s", S_FN_ConvertAsHex);
    break;
  case CANNA_FN_ConvertAsBushu:
    printf("%s", S_FN_ConvertAsBushu);
    break;
  case CANNA_FN_KouhoIchiran:
    printf("%s", S_FN_KouhoIchiran);
    break;
  case CANNA_FN_BubunMuhenkan:
    printf("%s", S_FN_BubunMuhenkan);
    break;
  case CANNA_FN_Zenkaku:
    printf("%s", S_FN_Zenkaku);
    break;
  case CANNA_FN_Hankaku:
    printf("%s", S_FN_Hankaku);
    break;
  case CANNA_FN_ToUpper:
    printf("%s", S_FN_ToUpper);
    break;
  case CANNA_FN_Capitalize:
    printf("%s", S_FN_Capitalize);
    break;
  case CANNA_FN_ToLower:
    printf("%s", S_FN_ToLower);
    break;
  case CANNA_FN_Hiragana:
    printf("%s", S_FN_Hiragana);
    break;
  case CANNA_FN_Katakana:
    printf("%s", S_FN_Katakana);
    break;
  case CANNA_FN_Romaji:
    printf("%s", S_FN_Romaji);
    break;
  case CANNA_FN_FuncSequence:
    printf("%s", S_FN_FuncSequence);
    break;
  case CANNA_FN_UseOtherKeymap:
    printf("%s", S_FN_UseOtherKeymap);
    break;
  case IROHA_FN_ZenHiraKakuteiMode:
    printf("%s", S_FN_ZenHiraKakuteiMode);
    break;
  case IROHA_FN_ZenKataKakuteiMode:
    printf("%s", S_FN_ZenKataKakuteiMode);
    break;
  case IROHA_FN_HanKataKakuteiMode:
    printf("%s", S_FN_HanKataKakuteiMode);
    break;
  case IROHA_FN_ZenAlphaKakuteiMode:
    printf("%s", S_FN_ZenAlphaKakuteiMode);
    break;
  case IROHA_FN_HanAlphaKakuteiMode:
    printf("%s", S_FN_HanAlphaKakuteiMode);
    break;
  }
}

static void
mode_style(mode, display)
char *mode, *display;
{
  printf("(set-mode-display '%s ", mode);
  if (display) {
    printf("\"%s\")", display);
  }
  else {
    printf("nil)");
  }
}

void
print_defmode(romkana)
char *romkana;
{
  printf(";;  互換性確保のためのモードを定義します\n");
  printf("(defmode %s \"%s\" %s)\n", "zen-hira-kakutei-mode  \"<全あ>\"",
	 romkana, "'(hiragana zenkaku kakutei)");
  printf("(defmode %s \"%s\" %s)\n", "zen-kata-kakutei-mode  \"<全ア>\"",
	 romkana, "'(katakana zenkaku kakutei)"); 
  printf("(defmode %s \"%s\" %s)\n", "zen-alpha-kakutei-mode \"<全英>\"",
	 romkana, "'(romaji zenkaku kakutei)");
  printf("(defmode %s \"%s\" %s)\n", "han-kata-kakutei-mode  \"<半ア>\"",
	 romkana, "'(katakana hankaku kakutei)");
  printf("(defmode %s \"%s\" %s)\n", "han-alpha-kakutei-mode \"<半英>\"",
	 romkana, "'(romaji hankaku kakutei)");
  printf("(defmode %s \"%s\" %s)\n", "zen-kata-henkan-mode   \"[全ア]\"",
	 romkana, "'(katakana zenkaku)");
  printf("(defmode %s \"%s\" %s)\n", "zen-alpha-henkan-mode  \"[全英]\"",
	 romkana, "'(romaji zenkaku)");
  printf("(defmode %s \"%s\" %s)\n", "han-kata-henkan-mode   \"[半ア]\"",
	 romkana, "'(katakana hankaku)");
  printf("(defmode %s \"%s\" %s)\n\n", "han-alpha-henkan-mode  \"[半英]\"",
	 romkana, "'(romaji hankaku)");
}

/*
static void
defZenHiraKakutei()
{
  if (!isZenHiraKakutei) {
    print_defmode(S_DM_ZenHiraKakutei);
    isZenHiraKakutei = 1;
  }
}

static void
defZenKataKakutei()
{
  if (!isZenKataKakutei) {
    print_defmode(S_DM_ZenKataKakutei);
    isZenKataKakutei = 1;
  }
}

static void
defZenAlphaKakutei()
{
  if (!isZenAlphaKakutei) {
    print_defmode(S_DM_ZenAlphaKakutei);
    isZenAlphaKakutei = 1;
  }
}

static void
defHanKataKakutei()
{
  if (!isHanKataKakutei) {
    print_defmode(S_DM_HanKataKakutei);
    isHanKataKakutei = 1;
  }
}

static void
defHanAlphaKakutei()
{
  if (!isHanAlphaKakutei) {
    print_defmode(S_DM_HanAlphaKakutei);
    isHanAlphaKakutei = 1;
  }
}

static void
defZenKataHenkan()
{
  if (!isZenKataHenkan) {
    print_defmode(S_DM_ZenKataHenkan);
    isZenKataHenkan = 1;
  }
}

static void
defZenAlphaHenkan()
{
  if (!isZenAlphaHenkan) {
    print_defmode(S_DM_ZenAlphaHenkan);
    isZenAlphaHenkan = 1;
  }
}

static void
defHanKataHenkan()
{
  if (!isHanKataHenkan) {
    print_defmode(S_DM_HanKataHenkan);
    isHanKataHenkan = 1;
  }
}

static void
defHanAlphaHenkan()
{
  if (!isHanAlphaHenkan) {
    print_defmode(S_DM_HanAlphaHenkan);
    isHanAlphaHenkan = 1;
  }
}
*/
