%{ /* -*-mode: text-*- */

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

static char lex_rcs_id[]=
"@(#) 102.1 $Id: lex.l,v 1.12 1993/07/07 05:05:03 kon Exp $";

#include "gram.h"
#include <canna/keydef.h>
#ifndef Ctl
#define Ctl(x)	((x) & 0x1F)
#endif

extern int IROHA_ParseError;
extern char IROHA_rcfilename[];
extern int inhibitCR;
extern char *memo;
#ifdef FLEX_SCANNER			/* need this for flex, hir, 1993.1.2 */
int yylineno;
#endif

%}
%e 4000
%p 6700
%n 2000
%k 3600
%a 4400
qstring			\"[^"]*\"
index			[1-9]
index2			1[0-5]
f			[Ff]
pf			[Pp][Ff]
control			"^"
control2		[cC]-
code			[!"#$%&'()*+\-./0-9:;<=>?@A-Z\[\\\]\^_`a-z{|}~]
number			[0-9][0-9]*
%%
","				{ return (COMMA); }
"{"				{ return (LB); }
"}"				{ return (RB); }
"("				{ return (LP); }
")"				{ return (RP); }

{f}{index}			{ 
	(void)sscanf(&yytext[1], "%d", &yylval.num);
	yylval.num += CANNA_KEY_F1 - 1 ;		     return (KEY);
				}
{f}{index2}			{ 
	(void)sscanf(&yytext[1], "%d", &yylval.num);
	yylval.num += CANNA_KEY_F1 - 1 ;		     return (KEY);
				}
{pf}{index}			{
	(void)sscanf(&yytext[2], "%d",	&yylval.num);
	yylval.num += CANNA_KEY_PF1 - 1;                     return (KEY);
				}
{pf}{index2}			{
	(void)sscanf(&yytext[2], "%d",	&yylval.num);
	yylval.num += CANNA_KEY_PF1 - 1;                     return (KEY);
				}

[Ee][Ss][Cc]			{ yylval.num = 0x1b ;        return (KEY); }
[Ee][Ss][Cc][Aa][Pp][Ee]        { yylval.num = 0x1b ;        return (KEY); }
[Tt][Aa][Bb]			{ yylval.num = '\t';         return (KEY); }
[Nn][Ff][Ee][Rr]		{ yylval.num = CANNA_KEY_Nfer ; return (KEY); }
[Xx][Ff][Ee][Rr]		{ yylval.num = CANNA_KEY_Xfer ; return (KEY); }
[Bb][Aa][Cc][Kk][Ss][Pp][Aa][Cc][Ee]	{
	yylval.num = '\b' ;                                  return (KEY); }
[Ii][Nn][Ss][Ee][Rr][Tt]	{ yylval.num = CANNA_KEY_Insert;return (KEY); }
[Dd][Ee][Ll][Ee][Tt][Ee]	{ yylval.num = 0x7f ;        return (KEY); }
[Rr][Oo][Ll][Ll][Uu][Pp]	{ yylval.num = CANNA_KEY_Rollup;return (KEY); }
[Rr][Oo][Ll][Ll][Dd][Oo][Ww][Nn]	{
	yylval.num = CANNA_KEY_Rolldown ;                    return (KEY); }
[Uu][Pp]			{ yylval.num = CANNA_KEY_Up ;return (KEY); }
[Dd][Oo][Ww][Nn]		{ yylval.num = CANNA_KEY_Down ; return (KEY); }
[Ll][Ee][Ff][Tt]		{ yylval.num = CANNA_KEY_Left ; return (KEY); }
[Rr][Ii][Gg][Hh][Tt]		{ yylval.num = CANNA_KEY_Right; return (KEY); }
[Cc][Ll][Ee][Aa][Rr]		{ yylval.num = Ctl('K') ;    return (KEY); }
[Hh][Ee][Ll][Pp]		{ yylval.num = CANNA_KEY_Help ; return (KEY); }
[Ee][Nn][Tt][Ee][Rr]		{ yylval.num = '\r' ;        return (KEY); }
[Ss][Pp][Aa][Cc][Ee]		{ yylval.num = ' ' ;         return (KEY); }
[Rr][Ee][Tt][Uu][Rr][Nn]	{ yylval.num = '\r' ;        return (KEY); }

[Ss]-[Nn][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Shift_Nfer ; return (KEY); }
[Ss]-[Xx][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Shift_Xfer ; return (KEY); }
[Ss]-[Uu][Pp]		{ yylval.num = CANNA_KEY_Shift_Up ;return (KEY); }
[Ss]-[Dd][Oo][Ww][Nn]	{ yylval.num = CANNA_KEY_Shift_Down ; return (KEY); }
[Ss]-[Ll][Ee][Ff][Tt]	{ yylval.num = CANNA_KEY_Shift_Left ; return (KEY); }
[Ss]-[Rr][Ii][Gg][Hh][Tt] { yylval.num = CANNA_KEY_Shift_Right; return (KEY); }

[Cc]-[Nn][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Cntrl_Nfer ; return (KEY); }
[Cc]-[Xx][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Cntrl_Xfer ; return (KEY); }
[Cc]-[Uu][Pp]		{ yylval.num = CANNA_KEY_Cntrl_Up ;return (KEY); }
[Cc]-[Dd][Oo][Ww][Nn]	{ yylval.num = CANNA_KEY_Cntrl_Down ; return (KEY); }
[Cc]-[Ll][Ee][Ff][Tt]	{ yylval.num = CANNA_KEY_Cntrl_Left ; return (KEY); }
[Cc]-[Rr][Ii][Gg][Hh][Tt] { yylval.num = CANNA_KEY_Cntrl_Right; return (KEY); }

\^[Nn][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Cntrl_Nfer ; return (KEY); }
\^[Xx][Ff][Ee][Rr]	{ yylval.num = CANNA_KEY_Cntrl_Xfer ; return (KEY); }
\^[Uu][Pp]		{ yylval.num = CANNA_KEY_Cntrl_Up ;return (KEY); }
\^[Dd][Oo][Ww][Nn]	{ yylval.num = CANNA_KEY_Cntrl_Down ; return (KEY); }
\^[Ll][Ee][Ff][Tt]	{ yylval.num = CANNA_KEY_Cntrl_Left ; return (KEY); }
\^[Rr][Ii][Gg][Hh][Tt]	{ yylval.num = CANNA_KEY_Cntrl_Right; return (KEY); }

{control}{code}			{ 
	yylval.num = Ctl(yytext[1]) ;                        return (KEY); }

{control2}{code}			{ 
	yylval.num = Ctl(yytext[2]) ;                        return (KEY); }

{code}				{ yylval.num = yytext[0] ;   return (KEY); }
\\\,				{ yylval.num = ',' ;	     return (KEY); }
\\\#				{ yylval.num = '#' ;	     return (KEY); }
\\\{				{ yylval.num = '{' ;	     return (KEY); }
\\\}				{ yylval.num = '}' ;	     return (KEY); }
\\\(				{ yylval.num = '(' ;	     return (KEY); }
\\\)				{ yylval.num = ')' ;	     return (KEY); }

[Uu][Nn][Dd][Ee][Ff][Ii][Nn][Ee]	{ 
	yylval.num = CANNA_KEY_Undefine ;                    return (KEY); }


[Rr][Oo][Mm][Kk][Aa][Nn][Aa][Tt][Aa][Bb][Ll][Ee]	{ 
	return (ROMKANATABLE); }

[Ii][Nn][Ii][Tt][Ii][Aa][Ll][Mm][Oo][Dd][Ee]	{ return (INITIALMODE); }
[Hh][Ee][Nn][Kk][Aa][Nn][Nn][Yy][Uu][Rr][Yy][Oo][Kk][Uu][Mm][Oo][Dd][Ee] {
	return (HENKANNYURYOKUMODE); }
[Zz][Ee][Nn][Hh][Ii][Rr][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii][Mm][Oo][Dd][Ee] {
	return (ZENHIRAKAKUTEIMODE); }
[Zz][Ee][Nn][Hh][Ii][Rr][Aa][Hh][Ee][Nn][Kk][Aa][Nn][Mm][Oo][Dd][Ee] {
	return (ZENHIRAHENKANMODE); }
[Hh][Ii][Rr][Aa][Gg][Aa][Nn][Aa][Mm][Oo][Dd][Ee]		{
	return (ZENHIRAKAKUTEIMODE); }
[Zz][Ee][Nn][Kk][Aa][Kk][Uu][Hh][Ii][Rr][Aa][Gg][Aa][Nn][Aa][Mm][Oo][Dd][Ee] {
	return (ZENHIRAKAKUTEIMODE); }
[Zz][Ee][Nn][Kk][Aa][Tt][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii][Mm][Oo][Dd][Ee] { 
	return (ZENKATAKAKUTEIMODE); }
[Zz][Ee][Nn][Kk][Aa][Tt][Aa][Hh][Ee][Nn][Kk][Aa][Nn][Mm][Oo][Dd][Ee] { 
	return (ZENKATAHENKANMODE); }
[Zz][Ee][Nn][Kk][Aa][Kk][Uu][Kk][Aa][Tt][Aa][Kk][Aa][Nn][Aa][Mm][Oo][Dd][Ee] {
	return (ZENKATAKAKUTEIMODE); }
[Zz][Ee][Nn][Aa][Ll][Pp][Hh][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii][Mm][Oo][Dd][Ee] {
	return (ZENALPHAKAKUTEIMODE); }
[Zz][Ee][Nn][Aa][Ll][Pp][Hh][Aa][Hh][Ee][Nn][Kk][Aa][Nn][Mm][Oo][Dd][Ee] {
	return (ZENALPHAHENKANMODE); }
[Zz][Ee][Nn][Kk][Aa][Kk][Uu][Aa][Ll][Pp][Hh][Aa][Bb][Ee][Tt][Mm][Oo][Dd][Ee] {
	return (ZENALPHAKAKUTEIMODE); }
[Zz][Ee][Nn][Kk][Aa][Kk][Uu][Rr][Oo][Mm][Aa][Jj][Ii][Mm][Oo][Dd][Ee] {
	return (ZENALPHAKAKUTEIMODE); }
[Hh][Aa][Nn][Kk][Aa][Tt][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii][Mm][Oo][Dd][Ee] { 
	return (HANKATAKAKUTEIMODE); }
[Hh][Aa][Nn][Kk][Aa][Tt][Aa][Hh][Ee][Nn][Kk][Aa][Nn][Mm][Oo][Dd][Ee] { 
	return (HANKATAHENKANMODE); }
[Hh][Aa][Nn][Kk][Aa][Kk][Uu][Kk][Aa][Tt][Aa][Kk][Aa][Nn][Aa][Mm][Oo][Dd][Ee] {
	return (HANKATAKAKUTEIMODE); }
[Hh][Aa][Nn][Aa][Ll][Pp][Hh][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii][Mm][Oo][Dd][Ee] {
	return (HANALPHAKAKUTEIMODE); }
[Hh][Aa][Nn][Aa][Ll][Pp][Hh][Aa][Hh][Ee][Nn][Kk][Aa][Nn][Mm][Oo][Dd][Ee] {
	return (HANALPHAHENKANMODE); }
[Hh][Aa][Nn][Kk][Aa][Kk][Uu][Rr][Oo][Mm][Aa][Jj][Ii][Mm][Oo][Dd][Ee] {
	return (HANALPHAKAKUTEIMODE); }
[Hh][Aa][Nn][Kk][Aa][Kk][Uu][Aa][Ll][Pp][Hh][Aa][Bb][Ee][Tt][Mm][Oo][Dd][Ee] {
	return (HANALPHAKAKUTEIMODE); }

[Cc][Uu][Rr][Ss][Oo][Rr][Ww][Rr][Aa][Pp]	{ return (CURSORWRAP); }
[Bb][Uu][Nn][Ss][Ee][Tt][Ss][Uu][Kk][Uu][Gg][Ii][Rr][Ii] {
	return (BUNSETSUKUGIRI); }

[Oo][Nn]			{ yylval.num = 1; return (ON_FLAG); }
[Oo][Ff][Ff]			{ yylval.num = 0; return (OFF_FLAG); }

[Ss][Ee][Ll][Ee][Cc][Tt][Dd][Ii][Rr][Ee][Cc][Tt]	{ 
	return (SELECTDIRECT); }

[Hh][Ee][Xx][Kk][Ee][Yy][Ss][Ee][Ll][Ee][Cc][Tt]	{
	return (HEXKEYSELECT); }

[Nn][Uu][Mm][Ee][Rr][Ii][Cc][Aa][Ll][Kk][Ee][Yy][Ss][Ee][Ll][Ee][Cc][Tt] {
	return (HEXKEYSELECT); }

[Dd][Ii][Cc][Tt][Ii][Oo][Nn][Aa][Rr][Yy]	{ return (DICTIONARY); }
[Uu][Ss][Ee][Rr][Dd][Ii][Cc]	{ return (USERDIC); }
[Bb][Uu][Ss][Hh][Uu][Dd][Ii][Cc]	{ return (BUSHUDIC); }
[Ll][Oo][Cc][Aa][Ll][Dd][Ii][Cc]	{ return (LOCALDIC); }
[Rr][Ee][Nn][Gg][Oo][Dd][Ii][Cc]	{ return (RENGODIC); }
[Kk][Aa][Nn][Aa][Dd][Ii][Cc]	{ return (KANADIC); }

[Cc][Hh][Aa][Rr][Aa][Cc][Tt][Ee][Rr][Bb][Aa][Ss][Ee][Dd][Mm][Oo][Vv][Ee] {
				  return (CHARACTERBASEDMOVE); }

[Rr][Ee][Vv][Ee][Rr][Ss][Ee][Ww][Ii][Dd][Ee][Ll][Yy] {
				  return (REVERSEWIDELY); }

[Bb][Rr][Ee][Aa][Kk][Ii][Nn][Tt][Oo][Rr][Oo][Mm][Aa][Nn] {
				  return (BREAKINTOROMAN); }

[Gg][Aa][Kk][Uu][Ss][Hh][Uu]	{ return (GAKUSHU); }
[Qq][Uu][Ii][Tt][Ii][Ff][Ee][Nn][Dd][Oo][Ff][Ii][Cc][Hh][Ii][Rr][Aa][Nn] {
				  return (QUITIFENDOFICHIRAN); }
[Kk][Aa][Kk][Uu][Tt][Ee][Ii][Ii][Ff][Ee][Nn][Dd][Oo][Ff][Bb][Uu][Nn][Ss][Ee][Tt][Ss][Uu] {
        return (KAKUTEIIFENDOFBUNSETSU); }
[Ss][Tt][Aa][Yy][Aa][Ff][Tt][Ee][Rr][Vv][Aa][Ll][Ii][Dd][Aa][Tt][Ee] {
	return (STAYAFTERVALIDATE); }
[Gg][Rr][Aa][Mm][Aa][Tt][Ii][Cc][Aa][Ll][Qq][Uu][Ee][Ss][Tt][Ii][Oo][Nn] {
	return (GRAMMATICALQUESTION); }
[Ff][Oo][Rr][Cc][Ee][Kk][Aa][Nn][Aa] {
	return (FORCEKANA); }
[Kk][Oo][Uu][Hh][Oo][Cc][Oo][Uu][Nn][Tt] {
	return (KOUHOCOUNT); }
[Aa][Uu][Tt][Oo] { return (AUTO); }
[Ii][Nn][Hh][Ii][Bb][Ii][Tt][Ll][Ii][Ss][Tt][Cc][Aa][Ll][Ll][Bb][Aa][Cc][Kk] {
	return (INHIBITLISTCALLBACK); }

[Aa][Ll][Pp][Hh][Aa][Mm][Oo][Dd][Ee]	{ return (ALPHAMODE); }
[Qq][Uu][Oo][Tt][Ee][Dd][Ii][Nn][Ss][Ee][Rr][Tt]	{ 
	return (QUOTEDINSERT); }
[Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ return (KAKUTEI); }
[Ee][Xx][Tt][Ee][Nn][Dd]	{ return (EXTEND); }
[Pp][Rr][Ee][Vv][Bb][Aa][Ss][Ee]	{ return (EXTEND); }
[Ss][Hh][Rr][Ii][Nn][Kk]	{ return (SHRINK); }
[Nn][Ee][Xx][Tt][Bb][Aa][Ss][Ee]	{ return (SHRINK); }
[Ss][Hh][Ii][Nn][Ss][Hh][Uu][Kk][Uu]	{ return (SHINSHUKU); }
[Tt][Oo][Uu][Rr][Oo][Kk][Uu]	{ return (TOUROKU); }
[Ee][Xx][Tt][Ee][Nn][Dd][Mm][Oo][Dd][Ee]	{ return (EXTENDMODE); }
[Ff][Oo][Rr][Ww][Aa][Rr][Dd]	{ return (FORWARD); }
[Bb][Aa][Cc][Kk][Ww][Aa][Rr][Dd]	{ return (BACKWARD); }
[Pp][Rr][Ee][Vv][Ii][Oo][Uu][Ss]	{ return (PREVIOUS); }
[Nn][Ee][Xx][Tt]		{ return (NEXT); }
[Bb][Ee][Gg][Ii][Nn][Nn][Ii][Nn][Gg][Oo][Ff][Ll][Ii][Nn][Ee]	{ 
	return (BEGINNINGOFLINE); }
[Ee][Nn][Dd][Oo][Ff][Ll][Ii][Nn][Ee]	{ return (ENDOFLINE); }
[Dd][Ee][Ll][Ee][Tt][Ee][Nn][Ee][Xx][Tt]	{ return (DELETENEXT); }
[Dd][Ee][Ll][Ee][Tt][Ee][Pp][Rr][Ee][Vv][Ii][Oo][Uu][Ss]	{ 
	return (DELETEPREVIOUS); }
[Kk][Ii][Ll][Ll][Tt][Oo][Ee][Nn][Dd][Oo][Ff][Ll][Ii][Nn][Ee]	{ 
	return (KILLTOENDOFLINE); }
[Hh][Ee][Nn][Kk][Aa][Nn]	{ return (HENKAN); }
[Qq][Uu][Ii][Tt]		{ return (QUIT); }
[Ss][Ee][Ll][Ff][Ii][Nn][Ss][Ee][Rr][Tt] { return (SELFINSERT); }

[Aa][Ll][Pp][Hh][Aa]		{ return (ALPHA); }
[Jj][Aa][Pp][Aa][Nn][Ee][Ss][Ee][Mm][Oo][Dd][Ee]	{ 
	return (JAPANESEMODE); }

[Yy][Oo][Mm][Ii][Gg][Aa][Nn][Aa][Ii]	{ return (YOMIGANAI); }
[Hh][Ee][Xx][Mm][Oo][Dd][Ee]	{ return (HEXMODE); }
[Bb][Uu][Ss][Hh][Uu][Mm][Oo][Dd][Ee]	{ return (BUSHUMODE); }
[Kk][Ii][Gg][Oo][Uu][Mm][Oo][Dd][Ee]	{ return (KIGOUMODE); }
[Kk][Ii][Gg][Oo][Uu]			{ return (KIGO); }
[Kk][Ii][Gg][Oo]			{ return (KIGO); }

[Yy][Oo][Mm][Ii]		{ return (YOMI); }
[Nn][Ee][Xx][Tt][Cc][Hh][Aa][Rr][Aa][Cc][Tt][Ee][Rr][Ss][Ee][Tt]	{ 
	return (NEXTCHARACTERSET); }
[Pp][Rr][Ee][Vv][Cc][Hh][Aa][Rr][Aa][Cc][Tt][Ee][Rr][Ss][Ee][Tt]	{ 
	return (PREVCHARACTERSET); }
[Cc][Oo][Nn][Vv][Ee][Rr][Tt][Aa][Ss][Hh][Ee][Xx]	{ 
	return (CONVERTASHEX); }
[Cc][Oo][Nn][Vv][Ee][Rr][Tt][Aa][Ss][Bb][Uu][Ss][Hh][Uu]	{ 
	return (CONVERTASBUSHU); }

[Mm][Oo][Jj][Ii][Ss][Hh][Uu]	{ return (MOJISHU); }
[Zz][Ee][Nn][Kk][Aa][Kk][Uu]	{ return (ZENKAKU); }
[Hh][Aa][Nn][Kk][Aa][Kk][Uu]	{ return (HANKAKU); }
[Tt][Oo][Uu][Pp][Pp][Ee][Rr]	{ return (TOUPPER); }
[Cc][Aa][Pp][Ii][Tt][Aa][Ll][Ii][Zz][Ee] { return (CAPITALIZE); }
[Tt][Oo][Ll][Oo][Ww][Ee][Rr]	{ return (TOLOWER); }

[Tt][Aa][Nn][Kk][Oo][Uu][Hh][Oo]	{ return (TANKOUHO); }
[Mm][Uu][Hh][Ee][Nn][Kk][Aa][Nn]	{ return (MUHENKAN); }
[Nn][Ee][Xx][Tt][Kk][Oo][Uu][Hh][Oo]	{ return (NEXTKOUHO); }
[Pp][Rr][Ee][Vv][Kk][Oo][Uu][Hh][Oo]	{ return (PREVKOUHO); }
[Kk][Oo][Uu][Hh][Oo][Ii][Cc][Hh][Ii][Rr][Aa][Nn]	{ 
	return (KOUHOICHIRAN); }
[Bb][Uu][Bb][Uu][Nn][Mm][Uu][Hh][Ee][Nn][Kk][Aa][Nn]	{ 
	return (BUBUNMUHENKAN); }

[Ii][Cc][Hh][Ii][Rr][Aa][Nn]	{ return (ICHIRAN); }
[Pp][Rr][Ee][Vv][Kk][Oo][Uu][Hh][Oo][Rr][Ee][Tt][Ss][Uu]	{ 
	return (PREVKOUHORETSU); }
[Nn][Ee][Xx][Tt][Kk][Oo][Uu][Hh][Oo][Rr][Ee][Tt][Ss][Uu]	{ 
	return (NEXTKOUHORETSU); }
[Bb][Ee][Gg][Ii][Nn][Nn][Ii][Nn][Gg][Oo][Ff][Kk][Oo][Uu][Hh][Oo]	{ 
	return (BEGINNINGOFKOUHO); }
[Ee][Nn][Dd][Oo][Ff][Kk][Oo][Uu][Hh][Oo]	{ return (ENDOFKOUHO); }
[Hh][Ii][Rr][Aa][Gg][Aa][Nn][Aa]		{ return (HIRAGANA); }
[Kk][Aa][Tt][Aa][Kk][Aa][Nn][Aa]		{ return (KATAKANA); }
[Rr][Oo][Mm][Aa][Jj][Ii]			{ return (ROMAJI); }

[Kk][Aa][Kk][Uu][Tt][Ee][Ii][Nn][Yy][Uu][Rr][Yy][Oo][Kk][Uu]	{ 
	return (KAKUTEINYURYOKU); }
[Hh][Ee][Nn][Kk][Aa][Nn][Nn][Yy][Uu][Rr][Yy][Oo][Kk][Uu][Mm][Oo][Dd][Ee] { 
	return (HENKANNYURYOKUMODE); }

[Hh][Ee][Xx]			{ return (HEX); }
[Bb][Uu][Ss][Hh][Uu]		{ return (BUSHU); }
[Hh][Ee][Nn][Kk][Aa][Nn][Nn][Yy][Uu][Rr][Yy][Oo][Kk][Uu] {
	return (HENKANNYURYOKU); }
[Zz][Ee][Nn][Hh][Ii][Rr][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ 
	return (ZENHIRAKAKUTEI); }
[Zz][Ee][Nn][Kk][Aa][Tt][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ 
	return (ZENKATAKAKUTEI); }
[Zz][Ee][Nn][Aa][Ll][Pp][Hh][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ 
	return (ZENALPHAKAKUTEI); }
[Hh][Aa][Nn][Kk][Aa][Tt][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ 
	return (HANKATAKAKUTEI); }
[Hh][Aa][Nn][Aa][Ll][Pp][Hh][Aa][Kk][Aa][Kk][Uu][Tt][Ee][Ii]	{ 
	return (HANALPHAKAKUTEI); }
[Nn][Hh][Ee][Nn][Kk][Aa][Nn][Ff][Oo][Rr][Ii][Cc][Hh][Ii][Rr][Aa][Nn] {
	return (NHENKANFORICHIRAN); }

[Yy][Ee][Ss][Nn][Oo]		{ return (YESNO); }
[Rr][Uu][Ss][Ss][Ii][Aa][Nn]	{ return (RUSSIAN); }
[Gg][Rr][Ee][Ee][Kk]		{ return (GREEK); }
[Ll][Ii][Nn][Ee]		{ return (LINE); }
[Cc][Hh][Aa][Nn][Gg][Ii][Nn][Gg][Ss][Ee][Rr][Vv][Ee][Rr] {
				return (CHANGINGSERVER); }
[Hh][Ee][Nn][Kk][Aa][Nn][Mm][Ee][Tt][Hh][Oo][Dd] {
				return (HENKANMETHOD); }
[Dd][Ee][Ll][Ee][Tt][Ee][Dd][Ii][Cc] {
				return (DELETEDIC); }
[Tt][Oo][Uu][Rr][Oo][Kk][Uu][Hh][Ii][Nn][Ss][Hh][Ii] {
				return (TOUROKUHINSHI); }
[Tt][Oo][Uu][Rr][Oo][Kk][Uu][Dd][Ii][Cc] {
				return (TOUROKUDIC); }
[Zz][Ee][Nn][Kk][Aa][Tt][Aa][Hh][Ee][Nn][Kk][Aa][Nn] {
				return (ZENKATAHENKAN); }
[Zz][Ee][Nn][Aa][Ll][Pp][Hh][Aa][Hh][Ee][Nn][Kk][Aa][Nn] {
				return (ZENALPHAHENKAN); }
[Hh][Aa][Nn][Kk][Aa][Tt][Aa][Hh][Ee][Nn][Kk][Aa][Nn] {
				return (HANKATAHENKAN); }
[Hh][Aa][Nn][Aa][Ll][Pp][Hh][Aa][Hh][Ee][Nn][Kk][Aa][Nn] {
				return (HANALPHAHENKAN); }
[Ee][Mm][Pp][Tt][Yy]		{ return (YOMIGANAI); }
[Mm][Oo][Uu][Nn][Tt][Dd][Ii][Cc] {
				return (MOUNTDIC); }

[Mm][Oo][Dd][Ee][Ss][Tt][Rr][Ii][Nn][Gg]    { return (MODESTRING); }
[Nn][Uu][Ll][Ll]		{ yylval.ptr = (char *)0; return STRING; }

{number}			{
	(void)sscanf(yytext, "%d", &yylval.num); return (NUMBER); }
{qstring}			{ yylval.ptr = (char *)yytext; return STRING; }

\#[^\n]*\n			{ char *p;
				  for (p = (char *)yytext + 1; *p ; p++) {
				    if (*p == '\n') {
				      *p = '\0';
				      break;
				    }
				  }
				  memo = (char *)malloc(strlen(yytext+1) +1);
				  if (memo)
				    strcpy(memo, yytext +1);
				  return NEWLINE; }
\n				{ return NEWLINE; }
[\t ]				{;}
.				{
				  char buf[256];

				  sprintf(buf,
			    "%s: %d 行目の文字 `%s' が認識できません。",
				    IROHA_rcfilename,
				    yylineno, yytext);
				  addWarningMesg(buf);
				  IROHA_ParseError = 1;
				}
%%
#ifdef yywrap
#undef yywrap
#endif
yywrap() { return (1); }

#ifdef unput
#undef unput
#undef input
#undef output
#endif

#define unput(c)	IROHA_unput(c)
#define input()		IROHA_input()
#define output(c)	IROHA_output(c)
