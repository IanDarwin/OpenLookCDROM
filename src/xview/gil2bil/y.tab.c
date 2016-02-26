extern char *malloc(), *realloc();

# line 2 "gil.y"
/* Copyright */

#include <stdio.h>
#include <stdlib.h>
int tokval;
# define GIL_HDR 257
# define LEFT 258
# define RIGHT 259
# define QSTRING 260
# define OBJ_NAME 261
# define NUMBER 262
# define ACTION 263
# define ACTIONS 264
# define ANCHOR_OBJECT 265
# define ANCHOR_POINT 266
# define ARG_TYPE 267
# define BACKGROUND_COLOR 268
# define BUTTON_TYPE 269
# define CHOICE_COLORS 270
# define CHOICE_LABEL_TYPES 271
# define CHOICES 272
# define COLUMN_ALIGNMENT 273
# define COLUMNS 274
# define CONSTANT_WIDTH 275
# define DEFAULT_DROP_SITE 276
# define DONE_HANDLER 277
# define DROPPABLE 278
# define EVENT_HANDLER 279
# define FOREGROUND_COLOR 280
# define FROM 281
# define FUNCTION_TYPE 282
# define GROUP_TYPE 283
# define HEIGHT 284
# define HELP 285
# define HORIZONTAL_OFFSET 286
# define HORIZONTAL_SPACING 287
# define ICON_FILE 288
# define ICON_LABEL 289
# define ICON_MASK_FILE 290
# define INITIAL_LIST_GLYPHS 291
# define INITIAL_LIST_VALUES 292
# define INITIAL_SELECTIONS 293
# define INITIAL_STATE 294
# define INITIAL_VALUE 295
# define LABEL 296
# define LABEL_TYPE 297
# define LAYOUT_TYPE 298
# define MAX_VALUE 299
# define MEMBERS 300
# define MENU 301
# define MIN_VALUE 302
# define MULTIPLE_SELECTIONS 303
# define NAME 304
# define NOTIFY_HANDLER 305
# define OWNER 306
# define PINNED 307
# define READ_ONLY 308
# define REFERENCE_POINT 309
# define RESIZABLE 310
# define ROW_ALIGNMENT 311
# define ROWS 312
# define SELECTION_REQUIRED 313
# define SETTING_TYPE 314
# define SHOW_BORDER 315
# define SHOW_FOOTER 316
# define STORED_LENGTH 317
# define TEXT_TYPE 318
# define TITLE 319
# define TO 320
# define TYPE 321
# define USER_DATA 322
# define VALUE_LENGTH 323
# define VALUE_UNDERLINED 324
# define VALUE_X 325
# define VALUE_Y 326
# define VERTICAL_OFFSET 327
# define VERTICAL_SPACING 328
# define WHEN 329
# define WIDTH 330
# define X 331
# define Y 332
# define ACTIVE 333
# define ALPHANUMERIC 334
# define BASE_WINDOW 335
# define BOTTOM_EDGES 336
# define BUTTON 337
# define COLUMN 338
# define CONTROL_AREA 339
# define EXCLUSIVE 340
# define GROUP 341
# define HORIZONTAL 342
# define HORIZONTAL_CENTERS 343
# define INACTIVE 344
# define INVISIBLE 345
# define LABELS 346
# define LEFT_EDGES 347
# define NIL 348
# define NORMAL 349
# define NORTH_WEST 350
# define NUMERIC 351
# define OPEN 352
# define POPUP_WINDOW 353
# define ROW 354
# define SCROLLING_LIST 355
# define SETTING 356
# define STRING 357
# define TEXT_FIELD 358
# define TEXT_PANE 359
# define TOP_EDGES 360
# define TRUE 361
# define USER_DEFINED 362
# define VERTICAL_CENTERS 363
# define VISIBLE 364
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 84 "gil.y"


/* Print message with context, and abort. All yyerrors are fatal, OK? */
void
yyerror(char *msg)
{
	extern int yylineno;

	fprintf(stderr, "%s: line %d: %s\n", "gil2bil", yylineno, msg);
	exit(1);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 32
# define YYLAST 93
int yyact[]={

    30,    47,    44,    45,    18,    36,    34,    40,    39,    52,
    51,    50,    43,    42,    48,    28,    19,    41,    31,    57,
    17,    56,    33,    30,    25,    26,    27,    18,     8,    55,
    22,    54,    20,    21,     7,     5,     3,     2,    28,    19,
    14,    11,    15,    17,    35,    46,    24,    25,    26,    27,
    10,    32,    23,    22,     9,    20,    21,    13,    29,     6,
    38,     4,     1,    14,     0,    15,    16,     0,     0,    24,
    49,     0,     0,    53,     0,    23,     0,     0,     0,     0,
    13,    29,     0,     0,    12,     0,     0,     0,     0,    16,
     0,     0,    37 };
int yypact[]={

 -1000,  -220, -1000, -1000,  -223,  -224,  -231,  -264, -1000,  -241,
  -237, -1000, -1000,  -329,  -256,  -256,  -254,  -255,  -243,  -247,
  -248,  -355,  -349,  -347,  -347,  -249,  -250,  -251,  -256,  -227,
  -229, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -238,  -240, -1000, -1000 };
int yypgo[]={

     0,    62,    61,    59,    54,    50,    41,    44,    45 };
int yyr1[]={

     0,     1,     2,     1,     1,     3,     3,     4,     4,     6,
     6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
     6,     6,     6,     6,     6,     6,     6,     5,     7,     7,
     8,     8 };
int yyr2[]={

     0,     0,     1,    13,     5,     6,     6,     4,     2,     5,
     5,     4,     4,     4,     5,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     6,     6,     2,     2,     2,
     2,     2 };
int yychk[]={

 -1000,    -1,   257,   256,    -2,   258,    -3,   258,   259,    -4,
    -5,    -6,   348,   321,   304,   306,   330,   284,   268,   280,
   296,   297,   294,   316,   310,   288,   289,   290,   279,   322,
   264,   259,    -6,   259,   335,    -7,   261,   348,    -7,   262,
   262,   260,   260,   260,   357,   352,    -8,   348,   361,    -8,
   260,   260,   260,    -7,   258,   258,   259,   259 };
int yydef[]={

     1,    -2,     2,     4,     0,     0,     0,     0,     3,     0,
     0,     8,    27,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     5,     7,     6,     9,    10,    28,    29,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    30,    31,    20,
    21,    22,    23,    24,     0,     0,    25,    26 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"GIL_HDR",	257,
	"LEFT",	258,
	"RIGHT",	259,
	"QSTRING",	260,
	"OBJ_NAME",	261,
	"NUMBER",	262,
	"ACTION",	263,
	"ACTIONS",	264,
	"ANCHOR_OBJECT",	265,
	"ANCHOR_POINT",	266,
	"ARG_TYPE",	267,
	"BACKGROUND_COLOR",	268,
	"BUTTON_TYPE",	269,
	"CHOICE_COLORS",	270,
	"CHOICE_LABEL_TYPES",	271,
	"CHOICES",	272,
	"COLUMN_ALIGNMENT",	273,
	"COLUMNS",	274,
	"CONSTANT_WIDTH",	275,
	"DEFAULT_DROP_SITE",	276,
	"DONE_HANDLER",	277,
	"DROPPABLE",	278,
	"EVENT_HANDLER",	279,
	"FOREGROUND_COLOR",	280,
	"FROM",	281,
	"FUNCTION_TYPE",	282,
	"GROUP_TYPE",	283,
	"HEIGHT",	284,
	"HELP",	285,
	"HORIZONTAL_OFFSET",	286,
	"HORIZONTAL_SPACING",	287,
	"ICON_FILE",	288,
	"ICON_LABEL",	289,
	"ICON_MASK_FILE",	290,
	"INITIAL_LIST_GLYPHS",	291,
	"INITIAL_LIST_VALUES",	292,
	"INITIAL_SELECTIONS",	293,
	"INITIAL_STATE",	294,
	"INITIAL_VALUE",	295,
	"LABEL",	296,
	"LABEL_TYPE",	297,
	"LAYOUT_TYPE",	298,
	"MAX_VALUE",	299,
	"MEMBERS",	300,
	"MENU",	301,
	"MIN_VALUE",	302,
	"MULTIPLE_SELECTIONS",	303,
	"NAME",	304,
	"NOTIFY_HANDLER",	305,
	"OWNER",	306,
	"PINNED",	307,
	"READ_ONLY",	308,
	"REFERENCE_POINT",	309,
	"RESIZABLE",	310,
	"ROW_ALIGNMENT",	311,
	"ROWS",	312,
	"SELECTION_REQUIRED",	313,
	"SETTING_TYPE",	314,
	"SHOW_BORDER",	315,
	"SHOW_FOOTER",	316,
	"STORED_LENGTH",	317,
	"TEXT_TYPE",	318,
	"TITLE",	319,
	"TO",	320,
	"TYPE",	321,
	"USER_DATA",	322,
	"VALUE_LENGTH",	323,
	"VALUE_UNDERLINED",	324,
	"VALUE_X",	325,
	"VALUE_Y",	326,
	"VERTICAL_OFFSET",	327,
	"VERTICAL_SPACING",	328,
	"WHEN",	329,
	"WIDTH",	330,
	"X",	331,
	"Y",	332,
	"ACTIVE",	333,
	"ALPHANUMERIC",	334,
	"BASE_WINDOW",	335,
	"BOTTOM_EDGES",	336,
	"BUTTON",	337,
	"COLUMN",	338,
	"CONTROL_AREA",	339,
	"EXCLUSIVE",	340,
	"GROUP",	341,
	"HORIZONTAL",	342,
	"HORIZONTAL_CENTERS",	343,
	"INACTIVE",	344,
	"INVISIBLE",	345,
	"LABELS",	346,
	"LEFT_EDGES",	347,
	"NIL",	348,
	"NORMAL",	349,
	"NORTH_WEST",	350,
	"NUMERIC",	351,
	"OPEN",	352,
	"POPUP_WINDOW",	353,
	"ROW",	354,
	"SCROLLING_LIST",	355,
	"SETTING",	356,
	"STRING",	357,
	"TEXT_FIELD",	358,
	"TEXT_PANE",	359,
	"TOP_EDGES",	360,
	"TRUE",	361,
	"USER_DEFINED",	362,
	"VERTICAL_CENTERS",	363,
	"VISIBLE",	364,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"list : /* empty */",
	"list : list GIL_HDR",
	"list : list GIL_HDR LEFT statmnts RIGHT",
	"list : list error",
	"statmnts : LEFT baseframe RIGHT",
	"statmnts : LEFT window RIGHT",
	"baseframe : baseframe bf_element",
	"baseframe : bf_element",
	"bf_element : TYPE BASE_WINDOW",
	"bf_element : NAME object",
	"bf_element : OWNER object",
	"bf_element : WIDTH NUMBER",
	"bf_element : HEIGHT NUMBER",
	"bf_element : BACKGROUND_COLOR QSTRING",
	"bf_element : FOREGROUND_COLOR QSTRING",
	"bf_element : LABEL QSTRING",
	"bf_element : LABEL_TYPE STRING",
	"bf_element : INITIAL_STATE OPEN",
	"bf_element : SHOW_FOOTER boolean",
	"bf_element : RESIZABLE boolean",
	"bf_element : ICON_FILE QSTRING",
	"bf_element : ICON_LABEL QSTRING",
	"bf_element : ICON_MASK_FILE QSTRING",
	"bf_element : EVENT_HANDLER object",
	"bf_element : USER_DATA LEFT RIGHT",
	"bf_element : ACTIONS LEFT RIGHT",
	"window : NIL",
	"object : OBJ_NAME",
	"object : NIL",
	"boolean : NIL",
	"boolean : TRUE",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 2:
# line 37 "gil.y"
{ fprintf(stderr, "GIL header\n"); } break;
case 3:
# line 39 "gil.y"
{ fprintf(stderr, "Rest of GIL\n"); } break;
case 4:
# line 41 "gil.y"
{ fprintf(stderr, "Error action\n"); exit(1); } break;
case 9:
# line 53 "gil.y"
{ fprintf(stderr, "Got a base window\n"); } break;
case 10:
# line 55 "gil.y"
{ fprintf(stderr, "Got baseframe %s\n", (char*)tokval); } break;
case 14:
# line 60 "gil.y"
{ fprintf(stderr, "BG_COLOR %s\n", (char*)tokval); } break;
	}
	goto yystack;		/* reset registers in driver code */
}
