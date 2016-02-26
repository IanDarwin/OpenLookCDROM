#include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	void yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

# line 3 "parse.l"
/*
 *      (c) Copyright 1991 Scott Oaks
 *      See LEGAL_NOTICE file for terms of the license.
 */

#ifdef IDENT
#ident  "@(#)parse.l	1.5 olvwm version 07 Jan 1994"
#endif
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 13 "parse.l"
{ yylval.ival = Warp; return WARP; }
break;
case 2:

# line 14 "parse.l"
{ yylval.ival = Open; return OPEN; }
break;
case 3:

# line 15 "parse.l"
{ yylval.ival = RaiseLower; return RAISELOWER; }
break;
case 4:

# line 16 "parse.l"
{ yylval.ival = Lower; return LOWER; }
break;
case 5:

# line 17 "parse.l"
{ yylval.ival = Raise; return RAISE; }
break;
case 6:

# line 18 "parse.l"
{ yylval.ival = Execute; return EXECUTE; }
break;
case 7:

# line 19 "parse.l"
{ yylval.ival = Goto; return GOTO; }
break;
case 8:

# line 20 "parse.l"
{ yylval.ival = Close; return CLOSE; }
break;
case 9:

# line 21 "parse.l"
{ yylval.ival = Quit; return QUIT; }
break;
case 10:

# line 22 "parse.l"
{ yylval.ival = Geometry; return GEOMETRY; }
break;
case 11:

# line 23 "parse.l"
{ yylval.ival = Stick; return STICK; }
break;
case 12:

# line 24 "parse.l"
{ yylval.ival = SetSize; return SETSIZE; }
break;
case 13:

# line 25 "parse.l"
{ yylval.ival = Focus; return FOCUS; }
break;
case 14:

# line 26 "parse.l"
{ yylval.ival = Rebind; return REBIND; }
break;
case 15:

# line 27 "parse.l"
 { yylval.ival = IfElse; return IFELSE; }
break;
case 16:

# line 29 "parse.l"
{ yylval.ival = XK_Shift_L; return MODIFIER; }
break;
case 17:

# line 30 "parse.l"
{ yylval.ival = XK_Shift_Lock; return MODIFIER; }
break;
case 18:

# line 31 "parse.l"
{ yylval.ival = XK_Caps_Lock; return MODIFIER; }
break;
case 19:

# line 32 "parse.l"
{ yylval.ival = XK_Control_L; return MODIFIER; }
break;
case 20:

# line 33 "parse.l"
{ yylval.ival = XK_Control_L; return MODIFIER; }
break;
case 21:

# line 34 "parse.l"
{ yylval.ival = XK_Control_L; return MODIFIER; }
break;
case 22:

# line 35 "parse.l"
{ yylval.ival = XK_Meta_L; return MODIFIER; }
break;
case 23:

# line 36 "parse.l"
{ yylval.ival = XK_Alt_L; return MODIFIER; }
break;
case 24:

# line 37 "parse.l"
{ yylval.ival = XK_Super_L; return MODIFIER; }
break;
case 25:

# line 38 "parse.l"
{ yylval.ival = XK_Hyper_L; return MODIFIER; }
break;
case 26:

# line 39 "parse.l"
{ yylval.ival = yylval.ival = -1; return MODIFIER; }
break;
case 27:

# line 40 "parse.l"
{ yylval.ival = SCREEN; return SCREEN; }
break;
case 28:

# line 41 "parse.l"
{ yylval.ival = WINMENU; return WINMENU; }
break;
case 29:

# line 42 "parse.l"
{ yylval.ival = PLUS; return PLUS; }
break;
case 30:

# line 43 "parse.l"
{ yylval.ival = EQUALS; return EQUALS; }
break;
case 31:

# line 45 "parse.l"
{ yylval.ival = atoi(yytext); return INT; }
break;
case 32:

# line 47 "parse.l"
{ yylval.sval = strdup(yytext); return WORD; }
break;
case 33:

# line 49 "parse.l"
{ return COLON; }
break;
case 34:

# line 50 "parse.l"
{ return COMMA; }
break;
case 35:

# line 52 "parse.l"
{ return OPENBRACE; }
break;
case 36:

# line 53 "parse.l"
{ return CLOSEBRACE; }
break;
case 37:

# line 55 "parse.l"
{
	    if (yytext[yyleng - 1] == '\\')
		yymore();
	    else {
		yylval.sval = strdup(yytext);
		return WORD;
	    }
	}
break;
case 38:

# line 64 "parse.l"
{
	    if (yytext[yyleng - 1] == '\\')
		yymore();
	    else {
		yylval.sval = strdup(yytext);
		return WORD;
	    }
	}
break;
case 39:

# line 73 "parse.l"
;
break;
case 40:

# line 74 "parse.l"
;
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

40,
0,

29,
0,

34,
0,

32,
0,

31,
32,
0,

33,
0,

30,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

35,
0,

36,
0,

39,
0,

37,
0,

38,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

23,
32,
0,

26,
32,
0,

32,
0,

32,
0,

32,
0,

21,
32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

20,
32,
0,

32,
0,

32,
0,

32,
0,

7,
32,
0,

32,
0,

32,
0,

32,
0,

22,
32,
0,

2,
32,
0,

9,
32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

1,
32,
0,

8,
32,
0,

32,
0,

32,
0,

13,
32,
0,

32,
0,

25,
32,
0,

32,
0,

4,
32,
0,

5,
32,
0,

32,
0,

32,
0,

32,
0,

16,
32,
0,

11,
32,
0,

24,
32,
0,

32,
0,

32,
0,

32,
0,

32,
0,

15,
32,
0,

32,
0,

14,
32,
0,

27,
32,
0,

32,
0,

32,
0,

19,
32,
0,

6,
32,
0,

32,
0,

32,
0,

12,
32,
0,

28,
32,
0,

10,
32,
0,

32,
0,

18,
0,

32,
0,

3,
32,
0,

17,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	1,3,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,3,	0,0,	1,4,	
2,28,	0,0,	0,0,	0,0,	
1,5,	0,0,	0,0,	0,0,	
1,6,	1,7,	1,8,	0,0,	
0,0,	1,9,	1,9,	1,9,	
1,9,	1,9,	1,9,	1,9,	
1,9,	1,9,	1,9,	1,10,	
0,0,	0,0,	1,11,	0,0,	
0,0,	0,0,	1,12,	1,8,	
1,13,	1,8,	1,14,	1,15,	
1,16,	1,17,	1,18,	1,8,	
1,8,	1,19,	1,20,	1,8,	
1,21,	1,8,	1,22,	1,23,	
1,24,	1,8,	1,8,	1,8,	
1,25,	1,8,	1,8,	1,8,	
42,68,	54,80,	82,105,	118,131,	
1,8,	0,0,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,26,	4,4,	1,27,	5,5,	
12,31,	14,37,	12,32,	15,38,	
17,41,	4,4,	4,4,	5,5,	
5,5,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	16,39,	
13,33,	18,42,	19,43,	20,44,	
21,45,	22,46,	31,56,	32,57,	
33,58,	16,40,	4,29,	13,34,	
5,5,	34,59,	13,35,	4,4,	
23,47,	5,30,	25,54,	13,36,	
23,48,	4,4,	35,60,	5,5,	
4,4,	8,8,	5,5,	37,63,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	36,61,	38,64,	
39,65,	40,66,	25,55,	41,67,	
36,62,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	43,69,	
44,70,	45,71,	46,72,	8,8,	
47,73,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	24,49,	
28,28,	24,50,	48,74,	49,75,	
24,51,	50,76,	51,77,	52,78,	
28,28,	28,0,	53,79,	55,81,	
58,82,	59,83,	60,84,	62,85,	
24,52,	24,53,	63,86,	64,87,	
65,88,	66,89,	67,90,	68,91,	
69,92,	70,93,	71,94,	72,95,	
73,96,	74,97,	75,98,	76,99,	
77,100,	28,28,	78,101,	79,102,	
80,103,	81,104,	28,28,	83,106,	
84,107,	86,108,	87,109,	88,110,	
28,28,	90,111,	91,112,	28,28,	
92,113,	96,114,	97,115,	98,116,	
99,117,	100,118,	101,119,	102,120,	
103,121,	105,122,	107,123,	108,124,	
110,125,	112,126,	114,127,	115,128,	
116,129,	117,130,	121,132,	122,133,	
123,134,	124,135,	125,136,	127,137,	
130,138,	131,139,	132,140,	133,141,	
136,142,	137,143,	139,144,	141,145,	
143,146,	144,147,	146,148,	147,149,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+1,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+-124,	0,		0,	
yycrank+-126,	0,		0,	
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+128,	0,		yyvstop+7,
yycrank+89,	yysvec+8,	yyvstop+9,
yycrank+0,	0,		yyvstop+12,
yycrank+0,	0,		yyvstop+14,
yycrank+20,	yysvec+8,	yyvstop+16,
yycrank+51,	yysvec+8,	yyvstop+18,
yycrank+9,	yysvec+8,	yyvstop+20,
yycrank+20,	yysvec+8,	yyvstop+22,
yycrank+46,	yysvec+8,	yyvstop+24,
yycrank+11,	yysvec+8,	yyvstop+26,
yycrank+47,	yysvec+8,	yyvstop+28,
yycrank+39,	yysvec+8,	yyvstop+30,
yycrank+50,	yysvec+8,	yyvstop+32,
yycrank+40,	yysvec+8,	yyvstop+34,
yycrank+36,	yysvec+8,	yyvstop+36,
yycrank+67,	yysvec+8,	yyvstop+38,
yycrank+152,	yysvec+8,	yyvstop+40,
yycrank+93,	yysvec+8,	yyvstop+42,
yycrank+0,	0,		yyvstop+44,
yycrank+0,	0,		yyvstop+46,
yycrank+-251,	0,		yyvstop+48,
yycrank+0,	0,		yyvstop+50,
yycrank+0,	0,		yyvstop+52,
yycrank+38,	yysvec+8,	yyvstop+54,
yycrank+34,	yysvec+8,	yyvstop+56,
yycrank+44,	yysvec+8,	yyvstop+58,
yycrank+50,	yysvec+8,	yyvstop+60,
yycrank+60,	yysvec+8,	yyvstop+62,
yycrank+78,	yysvec+8,	yyvstop+64,
yycrank+74,	yysvec+8,	yyvstop+66,
yycrank+88,	yysvec+8,	yyvstop+68,
yycrank+77,	yysvec+8,	yyvstop+70,
yycrank+73,	yysvec+8,	yyvstop+72,
yycrank+79,	yysvec+8,	yyvstop+74,
yycrank+23,	yysvec+8,	yyvstop+76,
yycrank+100,	yysvec+8,	yyvstop+78,
yycrank+104,	yysvec+8,	yyvstop+80,
yycrank+120,	yysvec+8,	yyvstop+82,
yycrank+117,	yysvec+8,	yyvstop+84,
yycrank+119,	yysvec+8,	yyvstop+86,
yycrank+156,	yysvec+8,	yyvstop+88,
yycrank+141,	yysvec+8,	yyvstop+90,
yycrank+141,	yysvec+8,	yyvstop+92,
yycrank+153,	yysvec+8,	yyvstop+94,
yycrank+154,	yysvec+8,	yyvstop+96,
yycrank+150,	yysvec+8,	yyvstop+98,
yycrank+15,	yysvec+8,	yyvstop+100,
yycrank+149,	yysvec+8,	yyvstop+102,
yycrank+0,	yysvec+8,	yyvstop+104,
yycrank+0,	yysvec+8,	yyvstop+107,
yycrank+149,	yysvec+8,	yyvstop+110,
yycrank+150,	yysvec+8,	yyvstop+112,
yycrank+150,	yysvec+8,	yyvstop+114,
yycrank+0,	yysvec+8,	yyvstop+116,
yycrank+159,	yysvec+8,	yyvstop+119,
yycrank+171,	yysvec+8,	yyvstop+121,
yycrank+154,	yysvec+8,	yyvstop+123,
yycrank+163,	yysvec+8,	yyvstop+125,
yycrank+162,	yysvec+8,	yyvstop+127,
yycrank+173,	yysvec+8,	yyvstop+129,
yycrank+167,	yysvec+8,	yyvstop+131,
yycrank+175,	yysvec+8,	yyvstop+133,
yycrank+180,	yysvec+8,	yyvstop+135,
yycrank+168,	yysvec+8,	yyvstop+137,
yycrank+163,	yysvec+8,	yyvstop+139,
yycrank+165,	yysvec+8,	yyvstop+141,
yycrank+176,	yysvec+8,	yyvstop+143,
yycrank+181,	yysvec+8,	yyvstop+145,
yycrank+200,	yysvec+8,	yyvstop+147,
yycrank+182,	yysvec+8,	yyvstop+149,
yycrank+187,	yysvec+8,	yyvstop+151,
yycrank+186,	yysvec+8,	yyvstop+153,
yycrank+211,	yysvec+8,	yyvstop+155,
yycrank+177,	yysvec+8,	yyvstop+157,
yycrank+62,	yysvec+8,	yyvstop+159,
yycrank+190,	yysvec+8,	yyvstop+161,
yycrank+178,	yysvec+8,	yyvstop+163,
yycrank+0,	yysvec+8,	yyvstop+165,
yycrank+176,	yysvec+8,	yyvstop+168,
yycrank+179,	yysvec+8,	yyvstop+170,
yycrank+194,	yysvec+8,	yyvstop+172,
yycrank+0,	yysvec+8,	yyvstop+174,
yycrank+183,	yysvec+8,	yyvstop+177,
yycrank+183,	yysvec+8,	yyvstop+179,
yycrank+186,	yysvec+8,	yyvstop+181,
yycrank+0,	yysvec+8,	yyvstop+183,
yycrank+0,	yysvec+8,	yyvstop+186,
yycrank+0,	yysvec+8,	yyvstop+189,
yycrank+200,	yysvec+8,	yyvstop+192,
yycrank+192,	yysvec+8,	yyvstop+194,
yycrank+202,	yysvec+8,	yyvstop+196,
yycrank+199,	yysvec+8,	yyvstop+198,
yycrank+189,	yysvec+8,	yyvstop+200,
yycrank+199,	yysvec+8,	yyvstop+202,
yycrank+193,	yysvec+8,	yyvstop+204,
yycrank+239,	yysvec+8,	yyvstop+206,
yycrank+0,	yysvec+8,	yyvstop+208,
yycrank+233,	0,		0,	
yycrank+0,	yysvec+8,	yyvstop+211,
yycrank+199,	yysvec+8,	yyvstop+214,
yycrank+195,	yysvec+8,	yyvstop+216,
yycrank+0,	yysvec+8,	yyvstop+218,
yycrank+196,	yysvec+8,	yyvstop+221,
yycrank+0,	yysvec+8,	yyvstop+223,
yycrank+212,	yysvec+8,	yyvstop+226,
yycrank+0,	yysvec+8,	yyvstop+228,
yycrank+238,	yysvec+8,	yyvstop+231,
yycrank+215,	yysvec+8,	yyvstop+234,
yycrank+206,	yysvec+8,	yyvstop+236,
yycrank+195,	yysvec+8,	yyvstop+238,
yycrank+63,	yysvec+8,	yyvstop+240,
yycrank+0,	yysvec+8,	yyvstop+243,
yycrank+0,	yysvec+8,	yyvstop+246,
yycrank+240,	yysvec+8,	yyvstop+249,
yycrank+208,	0,		0,	
yycrank+212,	yysvec+8,	yyvstop+251,
yycrank+220,	yysvec+8,	yyvstop+253,
yycrank+208,	yysvec+8,	yyvstop+255,
yycrank+0,	yysvec+8,	yyvstop+257,
yycrank+212,	yysvec+8,	yyvstop+260,
yycrank+0,	yysvec+8,	yyvstop+262,
yycrank+0,	yysvec+8,	yyvstop+265,
yycrank+223,	yysvec+8,	yyvstop+268,
yycrank+249,	0,		0,	
yycrank+241,	yysvec+8,	yyvstop+270,
yycrank+228,	0,		0,	
yycrank+0,	yysvec+8,	yyvstop+272,
yycrank+0,	yysvec+8,	yyvstop+275,
yycrank+207,	yysvec+8,	yyvstop+278,
yycrank+210,	yysvec+8,	yyvstop+280,
yycrank+0,	yysvec+8,	yyvstop+282,
yycrank+219,	0,		0,	
yycrank+0,	yysvec+8,	yyvstop+285,
yycrank+224,	0,		0,	
yycrank+0,	yysvec+8,	yyvstop+288,
yycrank+231,	yysvec+8,	yyvstop+291,
yycrank+234,	0,		0,	
yycrank+0,	0,		yyvstop+293,
yycrank+220,	yysvec+8,	yyvstop+295,
yycrank+228,	0,		0,	
yycrank+0,	yysvec+8,	yyvstop+297,
yycrank+0,	0,		yyvstop+300,
0,	0,	0};
struct yywork *yytop = yycrank+335;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  9,   1,  34,   1,   1,   1,   1,  39, 
  1,   1,   1,   1,   1,  45,   1,   1, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 48,  48,   1,   1,   1,   1,   1,   1, 
  1,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,   1,   1,   1,   1,  45, 
  1,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,  45,  45,  45,  45,  45, 
 45,  45,  45,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.7	93/06/07 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
