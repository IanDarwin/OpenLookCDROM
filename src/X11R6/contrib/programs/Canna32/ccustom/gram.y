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

/* -*-Mode: TEXT-*- $Id: gram.y,v 1.13 1992/07/28 12:14:19 kon Exp $ */

%{
static char rcs_id[]=
  "@(#) 102.1 $Id: gram.y,v 1.13 1992/07/28 12:14:19 kon Exp $";

#include <stdio.h>
#include "iroha.h"
#include "irmfdef.h"

#define PREVK 3
#define NEXTK 2
#define YES 1
#define NO  0

static char *ptr;
static unsigned char actbuff[256],keybuff[128];
static int special = NO;
static int mode, localmode = NO;
static int i = 0, j = 0;
extern InitialMode, CursorWrap, SelectDirect;
extern HexkeySelect, Gakushu, BreakIntoRoman, BunsetsuKugiri;
extern QuitIchiranIfEnd, stayAfterValidate, kakuteiIfEndOfBunsetsu;
extern kouho_threshold, gramaticalQuestion;
extern ChBasedMove, ReverseWidely;
extern char *RomkanaTable, *RengoGakushu, *KatakanaGakushu;
extern char IROHA_rcfilename[];

extern int yylineno;
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
%token <num> KAKUTEI EXTEND SHRINK TOUROKU FORWARD BACKWARD PREVIOUS NEXT
%token <num> BEGINNINGOFLINE ENDOFLINE DELETENEXT DELETEPREVIOUS
%token <num> KILLTOENDOFLINE HENKAN QUIT SELFINSERT
%token <num> ALPHA JAPANESEMODE
%token <num> YOMIGANAI HEXMODE BUSHUMODE KIGOUMODE
%token <num> YOMI NEXTCHARACTERSET PREVCHARACTERSET CONVERTASHEX
%token <num> CONVERTASBUSHU
%token <num> MOJISHU ZENKAKU HANKAKU TOUPPER CAPITALIZE TOLOWER
%token <num> TANKOUHO MUHENKAN NEXTKOUHO PREVKOUHO KOUHOICHIRAN BUBUNMUHENKAN
%token <num> ICHIRAN PREVKOUHORETSU NEXTKOUHORETSU BEGINNINGOFKOUHO
%token <num> ENDOFKOUHO
%token <num> KAKUTEINYURYOKU EXTENDMODE
%token <num> HEX BUSHU HENKANNYURYOKU ZENHIRAKAKUTEI
%token <num> ZENKATAKAKUTEI ZENALPHAKAKUTEI HANKATAKAKUTEI HANALPHAKAKUTEI
%token <num> SELECTDIRECT HEXKEYSELECT
%token <num> MODESTRING STAYAFTERVALIDATE KAKUTEIIFENDOFBUNSETSU
%token <num> HIRAGANA KATAKANA ROMAJI NHENKANFORICHIRAN NUMBER
%token <num> KIGO YESNO RUSSIAN GREEK LINE CHANGINGSERVER HENKANMETHOD
%token <num> DELETEDIC TOUROKUHINSHI TOUROKUDIC ZENKATAHENKAN 
%token <num> ZENALPHAHENKAN HANKATAHENKAN HANALPHAHENKAN MOUNTDIC  
%token <num> GRAMATICALQUESTION
%token <ptr> STRING

%type <ptr> string
%type <num> initMode flag number

%start cookrc

%%
cookrc		: stmts newlines
		;

stmts		: /* Empty */
		| stmt
		| stmts newline stmt
		;

stmt		: error
		| ROMKANATABLE string	{ RomkanaTable = $2; }
		| INITIALMODE initMode	{ InitialMode = $2; }
		| CURSORWRAP flag	{ CursorWrap = $2; }
		| SELECTDIRECT flag	{ SelectDirect = $2; }
		| HEXKEYSELECT flag	{ HexkeySelect = $2; }
		| BUNSETSUKUGIRI flag	{ BunsetsuKugiri = $2; }
		| DICTIONARY string	{ 
			kanjidicname[nkanjidics++] = $2;
			kanjidicname[nkanjidics] = NULL; } 
		| USERDIC string	{
			userdicname[nuserdics++] = $2;
			userdicname[nuserdics] = NULL; }
		| BUSHUDIC string	{
			bushudicname[nbushudics++] = $2;
			bushudicname[nbushudics] = NULL; }
		| LOCALDIC string	{
			localdicname[nlocaldics++] = $2;
			localdicname[nlocaldics] = NULL; }
		| RENGODIC string	{ RengoGakushu = $2; }
		| KANADIC string	{ KatakanaGakushu = $2; }
		| CHARACTERBASEDMOVE flag { ChBasedMove = $2; }
		| REVERSEWIDELY flag	{ ReverseWidely = $2; }
		| GAKUSHU flag		{ Gakushu = $2; }
		| QUITIFENDOFICHIRAN flag { QuitIchiranIfEnd = $2; }
		| KAKUTEIIFENDOFBUNSETSU flag { kakuteiIfEndOfBunsetsu = $2; }
		| STAYAFTERVALIDATE flag { stayAfterValidate = $2; }
		| BREAKINTOROMAN flag	{ BreakIntoRoman = $2; }
		| NHENKANFORICHIRAN number { kouho_threshold = $2; }
		| NHENKANFORICHIRAN KEY {
			if ('0' <= $2 && $2 <= '9') {
			  kouho_threshold = $2 - '0';
			}
			else {
			  yyerror(IROHA_rcfilename);
			} }
		| GRAMATICALQUESTION flag { gramaticalQuestion = $2; }
		| keymapping

		| ALPHA newlines	
			{ localmode = YES; mode = IROHA_MODE_AlphaMode; }
		  keymappingList
			{ localmode = NO; }

		| YOMIGANAI newlines
			{ localmode = YES; mode = IROHA_MODE_EmptyMode; }
		  keymappingList
			{ localmode = NO; }

		| YOMI newlines
			{ localmode = YES; mode = IROHA_MODE_YomiMode; }
		  keymappingList
			{ localmode = NO; }

		| MOJISHU newlines
			{ localmode = YES; mode = IROHA_MODE_JishuMode; }
		  keymappingList
			{ localmode = NO; }

		| TANKOUHO newlines
			{ localmode = YES; mode = IROHA_MODE_TankouhoMode; }
		  keymappingList
			{ localmode = NO; }

		| ICHIRAN newlines
			{ localmode = YES; mode = IROHA_MODE_IchiranMode; }
		  keymappingList
			{ localmode = NO; }

		| HENKANNYURYOKU newlines
			{ localmode = YES; mode = IROHA_MODE_EmptyMode; }
		  keymappingList
			{ localmode = NO; }

		| ZENHIRAKAKUTEI newlines
			{ localmode = YES; 
			  mode = IROHA_MODE_ZenHiraKakuteiMode; }
		  keymappingList
			{ localmode = NO; }

		| ZENKATAKAKUTEI newlines
			{ localmode = YES; 
			  mode = IROHA_MODE_ZenKataKakuteiMode; }
		  keymappingList
			{ localmode = NO; }

		| ZENALPHAKAKUTEI newlines
			{ localmode = YES; 
			  mode = IROHA_MODE_ZenAlphaKakuteiMode; }
		  keymappingList
			{ localmode = NO; }

		| HANKATAKAKUTEI newlines
			{ localmode = YES; 
			  mode = IROHA_MODE_HanKataKakuteiMode; }
		  keymappingList
			{ localmode = NO; }

		| HANALPHAKAKUTEI newlines
			{ localmode = YES; 
			  mode = IROHA_MODE_HanAlphaKakuteiMode; }
		  keymappingList
			{ localmode = NO; }

                | MODESTRING LP ALPHA RP string
			{ changeModeName(IROHA_MODE_AlphaMode,$5); }
                | MODESTRING LP YOMIGANAI RP string
			{ changeModeName(IROHA_MODE_EmptyMode,$5); }
                | MODESTRING LP HEX RP string
			{ changeModeName(IROHA_MODE_HexMode,$5); }
                | MODESTRING LP BUSHU RP string
			{ changeModeName(IROHA_MODE_BushuMode,$5); }
                | MODESTRING LP HENKANNYURYOKU RP string
			{ changeModeName(IROHA_MODE_EmptyMode,$5); }
                | MODESTRING LP ZENHIRAKAKUTEI RP string
			{ changeModeName(IROHA_MODE_ZenHiraKakuteiMode,$5); }
                | MODESTRING LP ZENKATAKAKUTEI RP string
			{ changeModeName(IROHA_MODE_ZenKataKakuteiMode,$5); }
                | MODESTRING LP ZENALPHAKAKUTEI RP string
			{ changeModeName(IROHA_MODE_ZenAlphaKakuteiMode,$5); }
                | MODESTRING LP HANKATAKAKUTEI RP string
			{ changeModeName(IROHA_MODE_HanKataKakuteiMode,$5); }
                | MODESTRING LP HANALPHAKAKUTEI RP string
			{ changeModeName(IROHA_MODE_HanAlphaKakuteiMode,$5); }
                | MODESTRING LP KIGO RP string
			{ changeModeName(IROHA_MODE_KigoMode,$5); }
                | MODESTRING LP MOJISHU RP string
			{ changeModeName(IROHA_MODE_JishuMode,$5); }
                | MODESTRING LP TANKOUHO RP string
			{ changeModeName(IROHA_MODE_TankouhoMode,$5); }
                | MODESTRING LP ICHIRAN RP string
			{ changeModeName(IROHA_MODE_IchiranMode,$5); }
                | MODESTRING LP YESNO RP string
			{ changeModeName(IROHA_MODE_YesNoMode,$5); }
                | MODESTRING LP EXTEND RP string
			{ changeModeName(IROHA_MODE_ExtendMode,$5); }
                | MODESTRING LP RUSSIAN RP string
			{ changeModeName(IROHA_MODE_RussianMode,$5); }
                | MODESTRING LP GREEK  RP string
			{ changeModeName(IROHA_MODE_GreekMode,$5); }
                | MODESTRING LP LINE RP string
			{ changeModeName(IROHA_MODE_LineMode,$5); }
                | MODESTRING LP CHANGINGSERVER RP string
			{ changeModeName(IROHA_MODE_ChangingServerMode,$5); }
                | MODESTRING LP HENKANMETHOD RP string
			{ changeModeName(IROHA_MODE_HenkanMethodMode,$5); }
                | MODESTRING LP DELETEDIC RP string
			{ changeModeName(IROHA_MODE_DeleteDicMode,$5); }
                | MODESTRING LP TOUROKU RP string
			{ changeModeName(IROHA_MODE_TourokuMode,$5); }
                | MODESTRING LP TOUROKUHINSHI RP string
			{ changeModeName(IROHA_MODE_TourokuHinshiMode,$5); }
                | MODESTRING LP TOUROKUDIC RP string
			{ changeModeName(IROHA_MODE_TourokuDicMode,$5); }
                | MODESTRING LP QUOTEDINSERT RP string
			{ changeModeName(IROHA_MODE_QuotedInsertMode,$5); }
                | MODESTRING LP BUBUNMUHENKAN RP string
			{ changeModeName(IROHA_MODE_BubunMuhenkanMode,$5); }
                | MODESTRING LP ZENKATAHENKAN RP string
			{ changeModeName(IROHA_MODE_ZenKataHenkanMode,$5); }
                | MODESTRING LP HANKATAHENKAN RP string
			{ changeModeName(IROHA_MODE_HanKataHenkanMode,$5); }
                | MODESTRING LP ZENALPHAHENKAN RP string
			{ changeModeName(IROHA_MODE_ZenAlphaHenkanMode,$5); }
                | MODESTRING LP HANALPHAHENKAN RP string
			{ changeModeName(IROHA_MODE_HanAlphaHenkanMode,$5); }
                | MODESTRING LP MOUNTDIC RP string
			{ changeModeName(IROHA_MODE_MountDicMode,$5); }
		;

initMode	: HENKANNYURYOKUMODE	{ $$ = IROHA_MODE_HenkanMode; }
		| ZENHIRAHENKANMODE	{ $$ = IROHA_MODE_ZenHiraHenkanMode; }
		| ZENKATAHENKANMODE	{ $$ = IROHA_MODE_ZenKataHenkanMode; }
		| HANKATAHENKANMODE	{ $$ = IROHA_MODE_HanKataHenkanMode; }
		| ZENALPHAHENKANMODE	{ $$ = IROHA_MODE_ZenAlphaHenkanMode; }
		| HANALPHAHENKANMODE	{ $$ = IROHA_MODE_HanAlphaHenkanMode; }
		| ZENHIRAKAKUTEIMODE	{ $$ = IROHA_MODE_ZenHiraKakuteiMode; }
		| ZENKATAKAKUTEIMODE	{ $$ = IROHA_MODE_ZenKataKakuteiMode; }
		| ZENALPHAKAKUTEIMODE	{ $$ = IROHA_MODE_ZenAlphaKakuteiMode;}
		| HANKATAKAKUTEIMODE	{ $$ = IROHA_MODE_HanKataKakuteiMode; }
		| HANALPHAKAKUTEIMODE	{ $$ = IROHA_MODE_HanAlphaKakuteiMode;}
		| ALPHAMODE		{ $$ = IROHA_MODE_AlphaMode; }
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

newline		: NEWLINE
		| newline NEWLINE
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
			{ actbuff[i] = 0;
			  keybuff[j] = 255;
			  if (i == 1 && j== 1) {
			    if (special == NO) {
			      chgKeyfunc((int)keybuff[0],(int)actbuff[0]);
			    }
			  }	
			  else {
			    if (j > 1) {
			      chgKeyfunc((int)keybuff[0],IROHA_FN_UseOtherKeymap);
			    }
			    else {
			      chgKeyfunc((int)keybuff[0],IROHA_FN_FuncSequence);
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
			 chgKeyfunc($1, IROHA_FN_Next);
			 changeKeyfunc(IROHA_MODE_IchiranMode,
			 $1, IROHA_FN_Forward,NULL,NULL);
		       }
		       if (special == PREVK && i == 1 && j == 0){
			 chgKeyfunc($1, IROHA_FN_Prev);
			 changeKeyfunc(IROHA_MODE_IchiranMode,
			      $1, IROHA_FN_Backward,NULL,NULL);
		       }
                       keybuff[j++] = $1;
		      }
		| keys COMMA KEY { keybuff[j++] = $3; }
		;

act		: FORWARD	{ actbuff[i++] = IROHA_FN_Forward; }
		| BACKWARD	{ actbuff[i++] = IROHA_FN_Backward; }
		| NEXT		{ actbuff[i++] = IROHA_FN_Next; }
		| PREVIOUS	{ actbuff[i++] = IROHA_FN_Prev; }
		| BEGINNINGOFLINE
				{ actbuff[i++] = IROHA_FN_BeginningOfLine; }
		| ENDOFLINE	{ actbuff[i++] = IROHA_FN_EndOfLine; }
		| DELETENEXT	{ actbuff[i++] = IROHA_FN_DeleteNext; }
		| DELETEPREVIOUS
				{ actbuff[i++] = IROHA_FN_DeletePrevious; }
		| KILLTOENDOFLINE
				{ actbuff[i++] = IROHA_FN_KillToEndOfLine; }
		| HENKAN	{ actbuff[i++] = IROHA_FN_Henkan; }
		| KAKUTEI	{ actbuff[i++] = IROHA_FN_Kakutei; }
		| SHRINK	{ actbuff[i++] = IROHA_FN_Shrink; }
		| EXTEND	{ actbuff[i++] = IROHA_FN_Extend; }
		| QUIT		{ actbuff[i++] = IROHA_FN_Quit; }
		| SELFINSERT	{ actbuff[i++] = IROHA_FN_FunctionalInsert; }
		| QUOTEDINSERT	{ actbuff[i++] = IROHA_FN_QuotedInsert; }
		| TOUROKU	{ actbuff[i++] = IROHA_FN_Touroku; }
		| EXTENDMODE	{ actbuff[i++] = IROHA_FN_ExtendMode; }
		| NEXTCHARACTERSET
				{ actbuff[i++] = IROHA_FN_Next; }
		| PREVCHARACTERSET
				{ actbuff[i++] = IROHA_FN_Prev; }
		| CONVERTASHEX	{ actbuff[i++] = IROHA_FN_ConvertAsHex; }
		| CONVERTASBUSHU
				{ actbuff[i++] = IROHA_FN_ConvertAsBushu; }

		| ZENKAKU	{ actbuff[i++] = IROHA_FN_Zenkaku; }

		| HANKAKU	{ actbuff[i++] = IROHA_FN_Hankaku; }

		| TOUPPER	{ actbuff[i++] = IROHA_FN_ToUpper; }

		| CAPITALIZE	{ actbuff[i++] = IROHA_FN_Capitalize; }

		| TOLOWER	{ actbuff[i++] = IROHA_FN_ToLower; }

		| MUHENKAN	{ actbuff[i++] = IROHA_FN_DeletePrevious; }

		| NEXTKOUHO	{ actbuff[i++] = IROHA_FN_NextKouho; }

		| PREVKOUHO    	{ actbuff[i++] = IROHA_FN_PrevKouho; }


		| KOUHOICHIRAN	{ actbuff[i++] = IROHA_FN_KouhoIchiran; }

		| BUBUNMUHENKAN	{ actbuff[i++] = IROHA_FN_BubunMuhenkan; }

		| NEXTKOUHORETSU
				{ actbuff[i++] = IROHA_FN_Next; }

		| PREVKOUHORETSU
				{ actbuff[i++] = IROHA_FN_Prev; }

		| BEGINNINGOFKOUHO
				{ actbuff[i++] = IROHA_FN_BeginningOfLine; }

		| ENDOFKOUHO	{ actbuff[i++] = IROHA_FN_EndOfLine; }

		| ALPHAMODE	{ actbuff[i++] = IROHA_FN_AlphaMode; }

		| JAPANESEMODE	{ actbuff[i++] = IROHA_FN_JapaneseMode; }

		| HEXMODE	{ actbuff[i++] = IROHA_FN_HexMode; }

		| BUSHUMODE	{ actbuff[i++] = IROHA_FN_BushuMode; }

		| KIGOUMODE	{ actbuff[i++] = IROHA_FN_KigouMode; }

		| HENKANNYURYOKUMODE
				{ actbuff[i++] = IROHA_FN_HenkanNyuryokuMode; }

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
		| HIRAGANA	{ actbuff[i++] = IROHA_FN_Hiragana; }
		| KATAKANA	{ actbuff[i++] = IROHA_FN_Katakana; }
		| ROMAJI	{ actbuff[i++] = IROHA_FN_Romaji; }
		;

%%
extern IROHA_ParseError;

static
yyerror(s)
char *s;
{
   char buf[256];

   sprintf(buf, "%s: syntax error, line %d", IROHA_rcfilename, yylineno);
   IROHA_ParseError = 1;
}

static
RemoveDblQuote(str)
char *str;
{
    strcpy(str, &str[1]);
    str[strlen(str)-1] = '\0';
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
