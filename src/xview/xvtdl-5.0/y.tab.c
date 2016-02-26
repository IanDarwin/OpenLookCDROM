#ifndef lint
static char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
#define YYPREFIX "yy"
#line 8 "todo.y"
 
/*
 * $Id: todo.y,v 3.0.1.1 1995/06/29 23:04:41 ian Exp $
 * **********************************************************************
 *
 * Todo.y ==> YACC description of the ASCII todo database format.  
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1992 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 *
 * $Log: todo.y,v $
 * Revision 3.0.1.1  1995/06/29  23:04:41  ian
 * Add default yywrap() for systems (linux) that lack it.
 *
 * Revision 3.0  1992/09/15  12:07:36  jipping
 * Release 4.0 beta.  Added deadline recognition and changed recurring
 * parsing to accomodate verion 4.0 recurrence stuff.
 *
 * Revision 2.1  1992/07/13  14:36:58  jipping
 * Cleaned up code to avoid compilation warnings.
 *
 * Revision 2.0  1992/07/06  13:34:47  jipping
 * Initial release.
 *
 *
 */

#include "globaldefs.h"

extern struct entry_list *entry_search();
extern struct category_rec *new_category();
extern struct category_rec *cat_search();

extern int merging;

int number_value;
char string_value[80], cat[80];
int inquotes;
int datecode, currdc;
struct day_entry *de, *tmpde;
struct entry_list *el;
struct recurrence_list *tmprl;
struct category_rec *cr, *tmpcr, *ttmpcr;
struct deadline_rec *dr;
int yylineno;
#line 67 "y.tab.c"
#define NUMBER 257
#define LSTRING 258
#define STRING 259
#define COMMENT 260
#define SLASH 261
#define COLON 262
#define BAR 263
#define BIGD 264
#define BIGN 265
#define BIGW 266
#define BIGM 267
#define BIGT 268
#define BIGO 269
#define BIGS 270
#define LITTLED 271
#define LITTLEW 272
#define LITTLEB 273
#define LITTLEM 274
#define LITTLEY 275
#define CATEGORY 276
#define DEADLINE 277
#define PARENT 278
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,    0,    6,    1,    9,   10,    4,    5,
   12,    5,   11,   13,   11,   14,   11,   15,   11,   16,
   11,   17,   11,   18,   11,   19,   11,   20,   11,   21,
   11,   22,   11,   23,   11,   24,   11,    7,    8,   25,
   25,   26,   28,   29,   30,   31,   32,   33,   34,   35,
   36,   37,    2,   39,   27,   40,   38,   38,   38,   38,
   38,   42,    3,   41,   41,
};
short yylen[] = {                                         2,
    0,    2,    2,    2,    0,    5,    0,    0,    7,    0,
    0,    3,    0,    0,    3,    0,    3,    0,    3,    0,
    3,    0,    3,    0,    3,    0,    3,    0,    4,    0,
    4,    0,    4,    0,    4,    0,    4,    2,    2,    1,
    1,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   31,    0,    3,    0,    5,    1,    1,    1,
    1,    0,    5,    0,    3,
};
short yydefred[] = {                                      0,
    7,    0,   42,    0,    0,    0,    0,    5,    0,    0,
    0,    2,    3,    4,    0,    0,   41,   40,   62,    0,
   11,    0,    8,    0,   54,   43,    0,    0,    0,    0,
    0,   63,    0,    0,    0,    0,    0,   24,   26,    0,
    0,   14,   16,   18,   20,   22,   12,   38,    0,    6,
    0,    0,    0,   58,   59,   60,   61,   55,    0,   28,
   30,   32,    0,    0,   34,   36,    0,    0,    0,    0,
    0,   39,    9,   65,   56,   44,    0,    0,    0,   25,
   27,    0,    0,   15,   17,   19,   21,   23,    0,    0,
   29,   31,   33,   35,   37,    0,    0,   57,   45,    0,
   46,    0,    0,   47,    0,    0,   48,    0,    0,   49,
    0,    0,   50,    0,    0,   51,    0,   52,    0,   53,
};
short yydgoto[] = {                                       4,
    5,    6,    7,    8,   22,   15,   29,   50,    9,   30,
   47,   27,   67,   68,   69,   70,   71,   63,   64,   77,
   78,   79,   82,   83,   19,   11,   26,   34,   90,  100,
  102,  105,  108,  111,  114,  117,  119,   58,   33,   89,
   32,   24,
};
short yysindex[] = {                                   -257,
    0, -250,    0,    0, -257, -257, -257,    0, -246, -254,
 -237,    0,    0,    0, -220, -213,    0,    0,    0, -212,
    0, -216,    0, -231,    0,    0, -236, -209, -211, -208,
 -210,    0, -248, -207, -203, -201, -200,    0,    0, -199,
 -198,    0,    0,    0,    0,    0,    0,    0, -254,    0,
 -197, -254, -196,    0,    0,    0,    0,    0, -195,    0,
    0,    0, -236, -236,    0,    0, -236, -236, -236, -236,
 -236,    0,    0,    0,    0,    0, -236, -236, -236,    0,
    0, -236, -236,    0,    0,    0,    0,    0, -194, -193,
    0,    0,    0,    0,    0, -192, -191,    0,    0, -189,
    0, -190, -187,    0, -188, -186,    0, -185, -184,    0,
 -183, -181,    0, -182, -176,    0, -175,    0, -179,    0,
};
short yyrindex[] = {                                     49,
    0,    0,    0,    0,   49,   49,   49,    0,    0,    0,
    0,    0,    0,    0, -250,    0,    0,    0,    0,    0,
    0,    0,    0,    1,    0,    0, -178,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -178, -178,    0,    0, -178, -178, -178, -178,
 -178,    0,    0,    0,    0,    0, -178, -178, -178,    0,
    0, -178, -178,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
};
short yygindex[] = {                                     35,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  -61,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  -38,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,
};
#define YYTABLESIZE 278
short yytable[] = {                                       1,
   64,   80,   81,   17,   18,   84,   85,   86,   87,   88,
   72,   10,   53,   74,   16,   91,   92,   93,    2,    3,
   94,   95,   54,   55,   20,   56,   57,   35,   36,   37,
   38,   39,   40,   41,   42,   43,   44,   45,   46,   12,
   13,   14,   21,   23,   25,   28,   31,   48,    1,    0,
   49,   52,   51,   60,   59,   61,   62,   65,   66,   73,
   75,   76,    0,    0,   98,   99,   96,  101,   97,  104,
  107,  103,    0,  106,  110,    0,  109,  113,  112,  115,
  116,  118,  120,   13,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   64,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   64,   64,
};
short yycheck[] = {                                     257,
    0,   63,   64,  258,  259,   67,   68,   69,   70,   71,
   49,  262,  261,   52,  261,   77,   78,   79,  276,  277,
   82,   83,  271,  272,  262,  274,  275,  264,  265,  266,
  267,  268,  269,  270,  271,  272,  273,  274,  275,    5,
    6,    7,  263,  257,  257,  262,  278,  257,    0,   -1,
  262,  262,  261,  257,  262,  257,  257,  257,  257,  257,
  257,  257,   -1,   -1,  257,  257,  261,  257,  262,  257,
  257,  262,   -1,  262,  259,   -1,  262,  259,  262,  262,
  257,  257,  262,  262,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  276,  277,
};
#define YYFINAL 4
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 278
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"NUMBER","LSTRING","STRING",
"COMMENT","SLASH","COLON","BAR","BIGD","BIGN","BIGW","BIGM","BIGT","BIGO",
"BIGS","LITTLED","LITTLEW","LITTLEB","LITTLEM","LITTLEY","CATEGORY","DEADLINE",
"PARENT",
};
char *yyrule[] = {
"$accept : todo_list",
"todo_list :",
"todo_list : todo_entry todo_list",
"todo_list : deadline todo_list",
"todo_list : category todo_list",
"$$1 :",
"todo_entry : date_part $$1 recurring_part priority_part text_part",
"$$2 :",
"$$3 :",
"date_part : NUMBER $$2 SLASH NUMBER $$3 SLASH NUMBER",
"recurring_part :",
"$$4 :",
"recurring_part : BAR $$4 recur_desc",
"recur_desc :",
"$$5 :",
"recur_desc : LITTLED $$5 recur_desc",
"$$6 :",
"recur_desc : LITTLEW $$6 recur_desc",
"$$7 :",
"recur_desc : LITTLEB $$7 recur_desc",
"$$8 :",
"recur_desc : LITTLEM $$8 recur_desc",
"$$9 :",
"recur_desc : LITTLEY $$9 recur_desc",
"$$10 :",
"recur_desc : BIGM $$10 recur_desc",
"$$11 :",
"recur_desc : BIGT $$11 recur_desc",
"$$12 :",
"recur_desc : BIGD NUMBER $$12 recur_desc",
"$$13 :",
"recur_desc : BIGN NUMBER $$13 recur_desc",
"$$14 :",
"recur_desc : BIGW NUMBER $$14 recur_desc",
"$$15 :",
"recur_desc : BIGO NUMBER $$15 recur_desc",
"$$16 :",
"recur_desc : BIGS NUMBER $$16 recur_desc",
"priority_part : COLON NUMBER",
"text_part : COLON estring",
"estring : STRING",
"estring : LSTRING",
"$$17 :",
"$$18 :",
"$$19 :",
"$$20 :",
"$$21 :",
"$$22 :",
"$$23 :",
"$$24 :",
"$$25 :",
"$$26 :",
"$$27 :",
"deadline : DEADLINE $$17 COLON deadline_date_part $$18 COLON NUMBER $$19 COLON NUMBER $$20 NUMBER $$21 COLON NUMBER $$22 COLON NUMBER $$23 COLON STRING $$24 COLON STRING $$25 COLON NUMBER $$26 NUMBER $$27 COLON",
"$$28 :",
"deadline_date_part : NUMBER $$28 deadline_date_rest",
"$$29 :",
"deadline_date_rest : SLASH NUMBER $$29 SLASH NUMBER",
"deadline_date_rest : LITTLED",
"deadline_date_rest : LITTLEW",
"deadline_date_rest : LITTLEM",
"deadline_date_rest : LITTLEY",
"$$30 :",
"category : CATEGORY COLON estring $$30 parent",
"parent :",
"parent : PARENT COLON estring",
};
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#line 278 "todo.y"
#include "lex.yy.c"

void yyerror(s)
char *s;
{
	fprintf(stderr,
			  "Error on line %d: %s at or around \"%s\"\n",
			  yylineno, s, yytext);
   yyerrok;
}

#ifdef	linux
int yywrap()
{
	return 1;
}
#endif
#line 356 "y.tab.c"
#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse()
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#ifdef lint
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 5:
#line 72 "todo.y"
{
	             el = entry_search(datecode, TRUE, cr);
					 de = el->last;
					 tmprl = NULL;
	          }
break;
case 6:
#line 79 "todo.y"
{
					 currdc = (curr_year-1990)*10000 + curr_month*100 + curr_day;
					 if (!merging ||
						  !entry_text_search(string_value,
													datecode<currdc?currdc:datecode, cr)) {
						 if (tmprl == NULL) {
							 tmpde = NEW(struct day_entry);
							 tmpde->recurring_entry = FALSE;
							 tmpde->deadline = NULL;
							 tmpde->next = NULL;
							 tmpde->starting_day_code = datecode;
							 strcpy(tmpde->text, string_value);
							 if (number_value != 0) {
								 tmpde->priority = number_value;
								 tmpde->checked = FALSE;
							 } else {
								 tmpde->priority = default_priority;
								 tmpde->checked = TRUE;
							 }
							 if (de == NULL) {
								 el->first = el->last = tmpde;
								 tmpde->prev = NULL;
							 } else {
								 de->next = el->last = tmpde;
								 tmpde->prev = de;
							 }
						 } else {
							 strcpy(rl_tail->text, string_value);
							 rl_tail->priority = number_value;
						 }
					 }
				 }
break;
case 7:
#line 113 "todo.y"
{datecode = number_value;}
break;
case 8:
#line 114 "todo.y"
{datecode = datecode * 100 + number_value;}
break;
case 9:
#line 115 "todo.y"
{datecode = (number_value - 90) * 10000 + datecode;}
break;
case 11:
#line 119 "todo.y"
{
						tmprl = NEW(struct recurrence_list);
						if (rl_head == NULL) {
							cr->rl_head = rl_head = cr->rl_tail = rl_tail = tmprl;
						} else {
							cr->rl_tail->next = rl_tail->next = tmprl;
							cr->rl_tail = rl_tail = tmprl;
						}
						tmprl->starting_day_code = datecode;
						tmprl->daily = tmprl->weekly = tmprl->biweekly = tmprl->monthly = tmprl->yearly = FALSE;
						tmprl->dow = -1;
						tmprl->dom = 0;
						tmprl->week_number =
							tmprl->number_of_weeks =
								tmprl->number_of_months = 0;
						tmprl->deadline = NULL;
						tmprl->next = NULL;
					}
break;
case 14:
#line 141 "todo.y"
{tmprl->daily = TRUE;}
break;
case 16:
#line 142 "todo.y"
{tmprl->weekly = TRUE;}
break;
case 18:
#line 143 "todo.y"
{tmprl->biweekly = TRUE;}
break;
case 20:
#line 144 "todo.y"
{tmprl->monthly = TRUE;}
break;
case 22:
#line 145 "todo.y"
{tmprl->yearly = TRUE;}
break;
case 24:
#line 146 "todo.y"
{tmprl->dow = 42;}
break;
case 26:
#line 147 "todo.y"
{tmprl->dow = 20;}
break;
case 28:
#line 148 "todo.y"
{tmprl->dow = number_value;}
break;
case 30:
#line 149 "todo.y"
{tmprl->week_number = number_value;}
break;
case 32:
#line 150 "todo.y"
{tmprl->number_of_weeks = number_value;}
break;
case 34:
#line 151 "todo.y"
{tmprl->number_of_months = number_value;}
break;
case 36:
#line 152 "todo.y"
{tmprl->dom = number_value;}
break;
case 40:
#line 159 "todo.y"
{
				string_value[0] = '\0';
				sscanf(yytext, "\"%[^\"]\"", string_value);
			}
break;
case 41:
#line 163 "todo.y"
{
				string_value[0] = '\0';
				strncpy(string_value, &yytext[2], strlen(yytext)-4);
				string_value[strlen(yytext)-4] = '\0';
			}
break;
case 42:
#line 170 "todo.y"
{
				 dr = NEW(struct deadline_rec);
	       }
break;
case 43:
#line 173 "todo.y"
{
				 dr->datecode = datecode;
			 }
break;
case 44:
#line 176 "todo.y"
{
				 dr->actions = number_value;
			 }
break;
case 45:
#line 179 "todo.y"
{
				 dr->delete_time = number_value;
			 }
break;
case 46:
#line 182 "todo.y"
{
				 dr->delete_units = number_value;
			 }
break;
case 47:
#line 185 "todo.y"
{
				 dr->priority_up_units = number_value;
			 }
break;
case 48:
#line 188 "todo.y"
{
				 dr->priority_down_units = number_value;
			 }
break;
case 49:
#line 191 "todo.y"
{
	          string_value[0] = '\0';
				 sscanf(yytext, "\"%[^\"]\"", string_value);
				 strcpy(dr->mail_on, string_value);
			 }
break;
case 50:
#line 196 "todo.y"
{
	          string_value[0] = '\0';
				 sscanf(yytext, "\"%[^\"]\"", string_value);
				 strcpy(dr->mail_after, string_value);
			 }
break;
case 51:
#line 201 "todo.y"
{
				 dr->move_time = number_value;
			 }
break;
case 52:
#line 204 "todo.y"
{
				 dr->move_units = number_value;
			 }
break;
case 53:
#line 207 "todo.y"
{
				 if (tmprl == NULL) {
					 tmpde->deadline = dr;
				 } else {
					 rl_tail->deadline = dr;
				 }
			 }
break;
case 54:
#line 216 "todo.y"
{datecode = number_value;}
break;
case 56:
#line 218 "todo.y"
{datecode = datecode * 100 + number_value;}
break;
case 57:
#line 219 "todo.y"
{
								datecode = (number_value - 90) * 10000 + datecode;
								dr->relative = FALSE;
							}
break;
case 58:
#line 223 "todo.y"
{datecode = datecode * 10; dr->relative = TRUE;}
break;
case 59:
#line 224 "todo.y"
{datecode = datecode * 10 + 1; dr->relative = TRUE;}
break;
case 60:
#line 225 "todo.y"
{datecode = datecode * 10 + 2; dr->relative = TRUE;}
break;
case 61:
#line 226 "todo.y"
{datecode = datecode * 10 + 3; dr->relative = TRUE;}
break;
case 62:
#line 229 "todo.y"
{
	          if (category_head == NULL) cr = NULL;
	          strcpy(cat, string_value);
			 }
break;
case 64:
#line 236 "todo.y"
{
	         if ( merging &&
					 (tmpcr = cat_search(category_head, string_value))) {
					cr = tmpcr;
					entry_head = cr->entry_head;
					entry_tail = cr->entry_tail;
					rl_head = cr->rl_head;
					rl_tail = cr->rl_tail;
				} else {
					if (category_head == NULL) {
						cr = NULL;
					} else {
						for (cr=category_head; cr->next!=NULL; cr=cr->next) ;
					}
					cr = new_category(string_value, cr, FALSE);
					entry_head = entry_tail = (struct entry_list *) NULL;
					rl_head = rl_tail = (struct recurrence_list *) NULL;
				}
         }
break;
case 65:
#line 255 "todo.y"
{
				tmpcr = cat_search(category_head, string_value);
	         if ( merging &&
					 (ttmpcr = cat_search(category_head, cat))) {
					cr = ttmpcr;
					entry_head = cr->entry_head;
					entry_tail = cr->entry_tail;
					rl_head = cr->rl_head;
					rl_tail = cr->rl_tail;
				} else {
					if (tmpcr->subcats == NULL) {
						cr = new_category(cat, tmpcr, TRUE);
					} else {
						cr = new_category(cat, cr, FALSE);
					}
					entry_head = entry_tail = (struct entry_list *) NULL;
					rl_head = rl_tail = (struct recurrence_list *) NULL;
				}
				cr->parent = tmpcr;
		   }
break;
#line 799 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
