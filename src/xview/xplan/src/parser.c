
/*  A Bison parser, made from parser.y
 with Bison version GNU/Andrew Bison version A2.2
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	TASK	258
#define	NAME	259
#define	DESCRIPTION	260
#define	PARENT	261
#define	DEPENDENCIES	262
#define	RESOURCES	263
#define	STRING	264
#define	DURATION	265
#define	FLOAT	266
#define	PLANNEDSTART	267
#define	ACTUALSTART	268
#define	FORECASTSTART	269
#define	EARLIESTSTART	270
#define	LATESTSTART	271
#define	PLANNEDEND	272
#define	ACTUALEND	273
#define	FORECASTEND	274
#define	EARLIESTEND	275
#define	LATESTEND	276
#define	MILESTONE	277
#define	DELIVERABLE	278
#define	UNSIGNED	279
#define	TRUE	280
#define	FALSE	281
#define	BEG	282
#define	END	283
#define	TASKS	284

#line 41 "parser.y"

#include <stdio.h>
#include "db.h"
#include "semantic_stack.h"
#include "string_table.h"

char *name;        /* name of the task */
char *desc;        /* task description */
unsigned duration;            /* the duration, in days */
unsigned planned_start_date;  /* All of the following dates are */
                              /* stored in Julian format */
unsigned planned_end_date;
unsigned actual_start_date;
unsigned actual_end_date;
unsigned forecast_start_date;
unsigned forecast_end_date;
unsigned earliest_start_date;
unsigned earliest_end_date;
unsigned latest_start_date;
unsigned latest_end_date;

unsigned float_time;              /* the float time, in days */

enum boolean milestone;           /* does this completion of this task */
      			          /* mean we have hit a milstone? */
enum boolean deliverable;         /* does the completion of this task */
                                  /* yield a deliverable? */

struct resource_list *resources; /* list of resources that are to */
                                 /* be employed in completing this */
				 /* task */

struct resource_node *resnode;
struct resource_info *resinfo;

struct task_list *subtasks;     /* list of sub-tasks that are a */
				/* part of this task */

struct task_node *parent;       /* if this task is a subtask, then */
				/* this points to the task that */
				/* this is a subtask of --- */
				/* otherwise this is NULL */

struct task_list *dependencies; /* those tasks that need to be */
				/* complete before this task can be */
			        /* started */

struct task_list *dependents;   /* those tasks that depend on this */
				/* tasks being complete before they */
                                /* can be started */
 
unsigned number_of_dependents;  /* used in checking for dependency */
                                /* loops */



static struct task_info *new_task;
static struct task_node *node, *new_node;

static unsigned x_pert = 0;
static unsigned y_pert = 0;

static unsigned x_gantt = 0;
static unsigned y_gantt = 0;

static unsigned length = 0;

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		103
#define	YYFLAG		-32768
#define	YYNTBASE	36

#define YYTRANSLATE(x) ((unsigned)(x) <= 284 ? yytranslate[x] : 57)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    35,     2,    30,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,    34,     2,
    31,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    32,     2,    33,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     5,     8,    11,    15,    18,    21,    23,    29,
    33,    36,    38,    40,    42,    44,    46,    48,    52,    56,
    60,    63,    66,    70,    74,    76,    79,    82,    86,    90,
    92,    96,   100,   104,   108,   112,   116,   120,   124,   128,
   132,   136,   140,   144,   148,   150
};

static const short yyrhs[] = {    37,
     0,    38,    30,     0,    39,    40,     0,    27,    29,     0,
    41,    28,    29,     0,    28,    29,     0,    42,    41,     0,
    42,     0,     3,    31,    32,    43,    33,     0,    44,    34,
    43,     0,    44,    34,     0,    45,     0,    46,     0,    50,
     0,    54,     0,    55,     0,    38,     0,     4,    31,     9,
     0,     5,    31,     9,     0,     6,    31,     9,     0,    47,
    48,     0,    27,     7,     0,    49,    28,     7,     0,    49,
    35,    48,     0,     9,     0,    51,    52,     0,    27,     8,
     0,    53,    28,     8,     0,    53,    35,    52,     0,     9,
     0,    10,    31,    24,     0,    11,    31,    24,     0,    12,
    31,    24,     0,    13,    31,    24,     0,    14,    31,    24,
     0,    15,    31,    24,     0,    16,    31,    24,     0,    17,
    31,    24,     0,    18,    31,    24,     0,    19,    31,    24,
     0,    20,    31,    24,     0,    21,    31,    24,     0,    22,
    31,    56,     0,    23,    31,    56,     0,    25,     0,    26,
     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   138,   145,   149,   153,   157,   158,   163,   164,   168,   220,
   221,   225,   226,   227,   228,   229,   230,   237,   241,   246,
   255,   259,   263,   287,   309,   313,   317,   323,   330,   339,
   343,   347,   351,   355,   359,   363,   367,   371,   375,   379,
   383,   387,   394,   398,   405,   409
};
#endif


#if YYDEBUG != 0

static const char * const yytname[] = {   "$","error","$illegal.","TASK","NAME",
"DESCRIPTION","PARENT","DEPENDENCIES","RESOURCES","STRING","DURATION","FLOAT",
"PLANNEDSTART","ACTUALSTART","FORECASTSTART","EARLIESTSTART","LATESTSTART","PLANNEDEND",
"ACTUALEND","FORECASTEND","EARLIESTEND","LATESTEND","MILESTONE","DELIVERABLE",
"UNSIGNED","TRUE","FALSE","BEG","END","TASKS","'.'","'='","'{'","'}'","';'",
"','","project","tasklist","taskblock","taskblockhead","taskblocktail","tasksublist",
"atask","alist","assignment","stringassign","dependencylist","dependhead","dependencytail",
"dependency","resourcelist","resourcehead","resourcetail","resource","numberassign",
"booleanassign","boolean", NULL
};
#endif

static const short yyr1[] = {     0,
    36,    37,    38,    39,    40,    40,    41,    41,    42,    43,
    43,    44,    44,    44,    44,    44,    44,    45,    45,    45,
    46,    47,    48,    48,    49,    50,    51,    52,    52,    53,
    54,    54,    54,    54,    54,    54,    54,    54,    54,    54,
    54,    54,    55,    55,    56,    56
};

static const short yyr2[] = {     0,
     1,     2,     2,     2,     3,     2,     2,     1,     5,     3,
     2,     1,     1,     1,     1,     1,     1,     3,     3,     3,
     2,     2,     3,     3,     1,     2,     2,     3,     3,     1,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     1,     1
};

static const short yydefact[] = {     0,
     0,     1,     0,     0,     4,     2,     0,     0,     3,     0,
     8,     0,     6,     0,     7,     0,     5,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    17,     0,     0,    12,    13,
     0,    14,     0,    15,    16,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    22,    27,     9,    11,    25,    21,     0,    30,
    26,     0,    18,    19,    20,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    45,    46,    43,
    44,    10,     0,     0,     0,     0,    23,    24,    28,    29,
     0,     0,     0
};

static const short yydefgoto[] = {   101,
     2,    36,     4,     9,    10,    11,    37,    38,    39,    40,
    41,    68,    69,    42,    43,    71,    72,    44,    45,    90
};

static const short yypact[] = {     2,
   -21,-32768,     3,    -3,-32768,-32768,     1,    -2,-32768,     6,
    27,     4,-32768,     8,-32768,    -1,-32768,     7,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    -6,-32768,    25,     5,-32768,-32768,
    47,-32768,    48,-32768,-32768,    50,    51,    52,    38,    39,
    40,    41,    42,    43,    44,    45,    46,    49,    53,    54,
   -19,   -19,-32768,-32768,-32768,    -1,-32768,-32768,    -4,-32768,
-32768,     0,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    64,    47,    66,    48,-32768,-32768,-32768,-32768,
    72,    75,-32768
};

static const short yypgoto[] = {-32768,
-32768,    76,-32768,-32768,    68,-32768,    26,-32768,-32768,-32768,
-32768,   -14,-32768,-32768,-32768,   -15,-32768,-32768,-32768,    28
};


#define	YYLAST		92


static const short yytable[] = {     7,
    63,    64,    18,    19,    20,    88,    89,     5,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
    33,    34,     5,    93,     8,    35,    13,    95,     1,     7,
    94,    12,     6,    14,    96,    16,    17,    46,    66,    47,
    48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
    58,    59,    60,    61,    62,    67,    70,    65,    73,    74,
    75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
    97,   102,    85,    99,   103,     3,    86,    87,    15,    98,
   100,     0,     0,     0,     0,     0,     0,     0,     0,    91,
     0,    92
};

static const short yycheck[] = {     3,
     7,     8,     4,     5,     6,    25,    26,    29,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    29,    28,    28,    27,    29,    28,    27,     3,
    35,    31,    30,    28,    35,    32,    29,    31,    34,    31,
    31,    31,    31,    31,    31,    31,    31,    31,    31,    31,
    31,    31,    31,    31,    31,     9,     9,    33,     9,     9,
     9,    24,    24,    24,    24,    24,    24,    24,    24,    24,
     7,     0,    24,     8,     0,     0,    24,    24,    11,    94,
    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,
    -1,    66
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/packages/andrew/etc/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/packages/andrew/etc/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 139 "parser.y"
{ 
   YYACCEPT;
;
    break;}
case 4:
#line 153 "parser.y"
{ clear_temps(); ;
    break;}
case 9:
#line 169 "parser.y"
{
   /* search for node in task list */
   node = find_task(get_main_task_list(), name);

   /* if it doesn't exist, add it */
   if (!node) {
      new_task = create_task_info(name, desc,
				  duration,
				  planned_start_date, planned_end_date,
				  actual_start_date, actual_end_date,
				  forecast_start_date, forecast_end_date,
				  earliest_start_date, earliest_end_date,
				  latest_start_date, latest_end_date,
				  float_time, milestone, deliverable,
				  resources, subtasks, parent,
				  dependencies, dependents,
				  number_of_dependents, x_pert, y_pert,
				  x_gantt, y_gantt, length);
      
      new_node = create_task_node(new_task, NULL, NULL);
      add_task_to_end(get_main_task_list(), new_node);
   } else {
      /* make a copy of the data and disconnect it from the list */
      /* temporarily */
      new_node = create_task_node(node->data, NULL, NULL);
      remove_task_node(get_main_task_list(), node);

      /* change the information */
      change_task_info(new_node->data, name, desc,
		       duration,
		       planned_start_date, planned_end_date,
		       actual_start_date, actual_end_date,
		       forecast_start_date, forecast_end_date,
		       earliest_start_date, earliest_end_date,
		       latest_start_date, latest_end_date,
		       float_time, milestone, deliverable,
		       resources, subtasks, parent,
		       dependencies, dependents,
		       number_of_dependents, x_pert, y_pert,
		       x_gantt, y_gantt, length);

      /* add it back to the list, at the end */
      add_task_to_end(get_main_task_list(), new_node);
   }
   
   /* clear our temporaries */
   clear_temps();
;
    break;}
case 17:
#line 231 "parser.y"
{
   fprintf(stderr, "Nested subtasks not implemented!\n");
;
    break;}
case 18:
#line 238 "parser.y"
{
   name = yyvsp[0].str;
;
    break;}
case 19:
#line 242 "parser.y"
{
   desc = yyvsp[0].str;
;
    break;}
case 20:
#line 247 "parser.y"
{
   parent = NULL;
   subtasks = NULL;
;
    break;}
case 23:
#line 263 "parser.y"
{
	   /* create a dependency list for this task */
	   dependencies = create_task_list(NULL, NULL, NULL);
	   node = find_task(get_main_task_list(), yyvsp[-2].str);
	   /* if the dependency is not already in the task list, */
	   /* create it */
	   if (!node) {
	      new_task = create_task_info(yyvsp[-2].str, NULL, 0,
					  0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0,
					  NULL, NULL, NULL,
					  NULL, NULL,
					  0,
					  x_pert, y_pert, x_gantt,
					  y_gantt, length);

	      node = create_task_node(new_task, NULL, NULL);
	      add_task_to_beginning(get_main_task_list(), node);
	   }
	   new_node = create_task_node(node->data, NULL, NULL);
	   /* always add to the beginning since this is a */
	   /* right-recursive production */
	   add_task_to_beginning(dependencies, new_node);
	;
    break;}
case 24:
#line 287 "parser.y"
{
	   /* same as above, but don't create the list */
	   node = find_task(get_main_task_list(), yyvsp[-2].str);
	   if (!node) {
	      new_task = create_task_info(yyvsp[-2].str, NULL, 0,
					  0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0,
					  NULL, NULL, NULL,
					  NULL, NULL,
					  0,
					  x_pert, y_pert, x_gantt,
					  y_gantt, length);

	      node = create_task_node(new_task, NULL, NULL);
	      add_task_to_beginning(get_main_task_list(), node);
	   }
	   new_node = create_task_node(node->data, NULL, NULL);
	   add_task_to_beginning(dependencies, new_node);
	;
    break;}
case 27:
#line 317 "parser.y"
{
	   
	;
    break;}
case 28:
#line 323 "parser.y"
{
	   /* create a new resource list and add resources to it */
	   resources = create_resource_list(NULL, NULL, NULL);
	   resinfo = create_resource_info(yyvsp[-2].str);
	   resnode = create_resource_node(resinfo, NULL, NULL);
	   add_resource_to_beginning(resources, resnode);
	;
    break;}
case 29:
#line 330 "parser.y"
{
	   /* add resource to our list */
	   resinfo = create_resource_info(yyvsp[-2].str);
	   resnode = create_resource_node(resinfo, NULL, NULL);
	   add_resource_to_beginning(resources, resnode);
	;
    break;}
case 31:
#line 344 "parser.y"
{ 
   duration = yyvsp[0].uint;
;
    break;}
case 32:
#line 348 "parser.y"
{ 
   float_time = yyvsp[0].uint;
;
    break;}
case 33:
#line 352 "parser.y"
{
   planned_start_date = yyvsp[0].uint;
;
    break;}
case 34:
#line 356 "parser.y"
{
   actual_start_date = yyvsp[0].uint;
;
    break;}
case 35:
#line 360 "parser.y"
{
   forecast_start_date = yyvsp[0].uint;
;
    break;}
case 36:
#line 364 "parser.y"
{
   earliest_start_date = yyvsp[0].uint;
;
    break;}
case 37:
#line 368 "parser.y"
{
   latest_start_date = yyvsp[0].uint;
;
    break;}
case 38:
#line 372 "parser.y"
{
   planned_end_date = yyvsp[0].uint;
;
    break;}
case 39:
#line 376 "parser.y"
{
   actual_end_date = yyvsp[0].uint;
;
    break;}
case 40:
#line 380 "parser.y"
{
   forecast_end_date = yyvsp[0].uint;
;
    break;}
case 41:
#line 384 "parser.y"
{
   earliest_end_date = yyvsp[0].uint;
;
    break;}
case 42:
#line 388 "parser.y"
{
   latest_end_date = yyvsp[0].uint;
;
    break;}
case 43:
#line 395 "parser.y"
{
   milestone = yyvsp[0].uint;
;
    break;}
case 44:
#line 399 "parser.y"
{
   deliverable = yyvsp[0].uint;
;
    break;}
case 45:
#line 406 "parser.y"
{ 
   yyval.uint = true;
;
    break;}
case 46:
#line 410 "parser.y"
{ 
   yyval.uint = false;
;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/packages/andrew/etc/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 414 "parser.y"

int yyerror(char *s)
{
}

clear_temps()
{
   name = NULL;
   desc = NULL;
   duration = 0;
   float_time = 0;
   planned_start_date = 0;
   planned_end_date = 0;
   actual_start_date = 0;
   actual_end_date = 0;
   forecast_start_date = 0;
   forecast_end_date = 0;
   earliest_start_date = 0;
   earliest_end_date = 0;
   latest_start_date = 0;
   latest_end_date = 0;
   milestone = 0;
   deliverable = 0; 
   resources = NULL;
   subtasks = NULL;
   parent = NULL;
   dependencies = NULL;
   dependents = NULL;
   number_of_dependents = 0;
 }
