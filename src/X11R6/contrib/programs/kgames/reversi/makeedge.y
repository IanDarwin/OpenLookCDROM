%{
int	score;
extern int	position;
%}
%union {
	struct {
		int	width;
		int	position;
		int	base;
	} field;
	int	ival;
}
%type  <field>	line whites blacks empties oempties
%type  <field>	type1 type2 otype3e type3e type3 otype4 type4 type4.w type4.b
%token <field>	WHITE BLACK EMPTY IGNORE
%token <ival>	NL
%%
lines	:	lines line
			{ output ($2.base, line); }
	|	lines ignore
			{ output (0, "ignore"); }
	|
	;
ignore	:	IGNORE NL
	;
line	:	whites type1 NL
			{ $$.base = 20 * $1.width + $2.base; }
	|	blacks type2 NL
			{ $$.base = -20 * $1.width + $2.base; }
	|	EMPTY type3 NL
			{ $$.base = $2.base; }
	|	EMPTY empties otype4 NL
			{ $$.base = $3.base; }
	;
type1	:	blacks whites empties otype4
			{
				$$.base = $4.base;
				switch ($2.position) {
				case 7:
					$$.base -= ($2.width + $1.width+1) * 15;
					break;
				default:
					if ($3.width == 1)
						$$.base -=
						($1.width + $2.width+1) * 15;
					else
						$$.base +=
						($2.width - $1.width) * 20;
					break;
				}
				$$.position = $4.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	blacks whites type1
			{
				$$.base = $3.base;
				$$.base -= ($1.width - $2.width) * 20;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	blacks empties otype4
			{
				$$.base = ($1.width + 1) * 15 + $3.base;
				$$.width = $1.width + $2.width + $3.width;
				$$.position = $3.position;
			}
	|	blacks
			{
				$$ = $1;
				$$.base = - $1.width * 20;
			}
	|	empties otype4
			{
				$$.position = $2.position;
				$$.width = $1.width+$2.width;
				$$.base = $2.base;
			}
	|
			{ $$.position = position; $$.width = 0; $$.base = 0; }
	;
type2	:	whites blacks empties otype4
			{
				$$.base = $4.base;
				switch ($2.position) {
				case 7:
					$$.base += ($2.width + $1.width+1) * 15;
					break;
				default:
					if ($3.width == 1)
						$$.base +=
						($1.width + $2.width+1) * 15;
					else
						$$.base -=
						($2.width - $1.width) * 20;
					break;
				}
				$$.position = $4.position;
				$$.width = $1.width + $2.width + $3.width
					 + $4.width;
			}
	|	whites blacks type2
			{
				$$.base = $3.base;
				$$.base += ($1.width - $2.width) * 20;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	whites empties otype4
			{
				$$.base = - ($1.width + 1) * 15 + $3.base;
				$$.width = $1.width + $2.width + $3.width;
				$$.position = $3.position;
			}
	|	whites
			{
				$$ = $1;
				$$.base = $1.width * 20;
			}
	|	empties otype4
			{
				$$.position = $2.position;
				$$.width = $1.width+$2.width;
				$$.base = $2.base;
			}
	|
		{ $$.position = 0; $$.width = 0; $$.base = 0; }
	;
otype4	:	type4
			{ $$ = $1; }
	|
			{ $$.position = position; $$.width = 0; $$.base = 0; }
	;
whites	:	whites WHITE
			{
				$$.position = $2.position;
				$$.width = $1.width + $2.width;
				$$.base = $1.base + $2.base;
			}
	|	WHITE
			{ $$ = $1; }
	;
blacks	:	blacks BLACK
			{
				$$.position = $2.position;
				$$.width = $1.width + $2.width;
				$$.base = $1.base + $2.base;
			}
	|	BLACK
			{ $$ = $1; }
	;
empties	:	empties EMPTY
			{
				$$.position = $2.position;
				$$.width = $1.width + $2.width;
				$$.base = $1.base + $2.base;
			}
	|	EMPTY
			{ $$ = $1; }
	;
otype3e	:	type3e
			{ $$ = $1; }
	|
			{ $$.position = position; $$.width = 0; $$.base = 0; }
	;
type3	:	whites EMPTY whites oempties otype3e
			{
				$$.base = -($1.width + $3.width + 2) * 15 +
					$5.base;
				$$.width = $1.width + $2.width + $3.width 
					 + $4.width + $5.width;
				$$.position = $5.position;
			}
	|	blacks EMPTY blacks oempties otype3e
			{
				$$.base = ($1.width + $3.width + 2) * 15 +
					$5.base;
				$$.width = $1.width + $2.width + $3.width 
					 + $4.width + $5.width;
				$$.position = $5.position;
			}
	|	type3e
	;
type3e	:	whites blacks type2
			{
				$$.base = -15 * ($1.width + $2.width + 1);
				$$.width = $1.width + $2.width + $3.width;
				$$.position = $3.position;
			}
	|	blacks whites type1
			{
				$$.base = 15 * ($1.width + $2.width + 1);
				$$.width = $1.width + $2.width + $3.width;
				$$.position = $3.position;
			}
	|	whites empties otype3e
			{
				if ($1.position - $1.width == 1) {
					switch ($1.width) {
					case 1:
						$$.base = -30;
						break;
					case 6:
						$$.base = -20;
						break;
					case 2:
						$$.base = -15;
						break;
					case 3:
						$$.base = -10;
						break;
					case 4:
						$$.base = -5;
						break;
					case 5:
						$$.base = 10;
						break;
					default:
						yyerror ("weirdo");
						break;
					}
				} else {
					$$.base = $1.base;
				}
				$$.base += $3.base;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	blacks empties otype3e
			{
				if ($1.position - $1.width == 1) {
					switch ($1.width) {
					case 1:
						$$.base = 30;
						break;
					case 6:
						$$.base = 20;
					break;
					case 2:
						$$.base = 15;
						break;
					case 3:
						$$.base = 10;
						break;
					case 4:
						$$.base = 5;
						break;
					case 5:
						$$.base = -10;
						break;
					default:
						yyerror ("weirdo");
						break;
					}
				} else {
					$$.base = $1.base;
				}
				$$.base += $3.base;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	whites
			{
				$$.base = 20 * $1.width;
				$$.position = $1.position;
				$$.width = $1.width;
			}
	|	blacks
			{
				$$.base = -20 * $1.width;
				$$.position = $1.position;
				$$.width = $1.width;
			}
	;
type4	:	whites EMPTY whites oempties otype4
		{
			if ($4.position == 8) {
				$$.base = -($1.width + $3.width + 2) * 15;
			} else {
				$$.base = 0;
				if ($1.position - $1.width + 1 == 3)
					$$.base = $1.width * 15;
				else
					$$.base = $1.base;
				if ($3.position == 6)
					$$.base += $3.width * 15;
				else
					$$.base += $3.base;
				$$.base += $5.base;
			}
			$$.width = $1.width + $2.width + $3.width
				 + $4.width + $5.width;
			$$.position = $5.position;
		}
	|	whites empties otype4
			{
				if ($1.position - $1.width + 1 == 3)
					$$.base = $1.width * 15 + $3.base;
				else if ($1.position == 6)
					$$.base = $1.width * 15 + $3.base;
				else
					$$.base = $1.base + $3.base;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	blacks EMPTY blacks oempties otype4
		{
			if ($4.position == 8) {
				$$.base = ($1.width + $3.width + 2) * 15;
			} else {
				$$.base = 0;
				if ($1.position - $1.width + 1 == 3)
					$$.base = -$1.width * 15;
				else
					$$.base = $1.base;
				if ($3.position == 6)
					$$.base +=  -$3.width * 15;
				else
					$$.base += $3.base;
				$$.base += $5.base;
			}
			$$.width = $1.width + $2.width + $3.width
				 + $4.width + $5.width;
			$$.position = $5.position;
		}
	|	blacks empties otype4
			{
				if ($1.position - $1.width + 1 == 3)
					$$.base = -$1.width * 15 + $3.base;
				else if ($1.position == 6)
					$$.base = -$1.width * 15 + $3.base;
				else
					$$.base = $1.base + $3.base;
				$$.position = $3.position;
				$$.width = $1.width + $2.width + $3.width;
			}
	|	whites
			{
				$$.base = 20 * $1.width;
				$$.position = $1.position;
				$$.width = $1.width;
			}
	|	blacks
			{
				$$.base = -20 * $1.width;
				$$.position = $1.position;
				$$.width = $1.width;
			}
	|	type4.w
			{ $$ = $1; }
	|	type4.b
			{ $$ = $1; }
	;
type4.w	:	whites blacks oempties otype4
		{
			if ($2.position == 8)
				$$.base = - ($1.width + $2.width + 1) * 15;
			else if ($3.position == 8 && $3.width == 1)
				$$.base = ($1.width + $2.width + 1) * 10;
			else
				$$.base = $1.base + $2.base + $4.base;
			$$.position = $4.position;
			$$.width = $1.width + $2.width + $3.width + $4.width;
		}
	;
type4.b	:	blacks whites oempties otype4
		{
			if ($2.position == 8)
				$$.base = ($1.width + $2.width + 1) * 15;
			else if ($3.position == 8 && $3.width == 1)
				$$.base = - ($1.width + $2.width + 1) * 10;
			else
				$$.base = $1.base + $2.base + $4.base;
			$$.position = $4.position;
			$$.width = $1.width + $2.width + $3.width + $4.width;
		}
	;
oempties:	empties
			{ $$ = $1; }
	|
			{ $$.position = position; $$.width = 0; $$.base = 0; }
	;
%%

# include	<stdio.h>

int	verbose;

main (argc, argv)
	int	argc;
	char	**argv;
{
	int	ret;

	if (argc > 1 && argv[1][0] == 'v')
		verbose = 1;
	ret = yyparse ();
	flush_output ();
	return ret;
}

char	line[80];
char	*lp = line;

yyerror (s)
char *s;
{
	fprintf (stderr, "%s in %s\n", s, line);
}

yywrap ()
{
	return 1;
}

int position = 1;

int base[] = { 0, 20, -30, 15, -5, -5, 15, -30, 20, 0 };

yylex ()
{
	char *gets();

	if (*lp == '\0')
		if (fgets (line, 80, stdin) == 0)
			return -1;
		else
			lp = line;
	for (;;) {
		switch (*lp++) {
		case ' ':
		case '\t':
			break;
		case '\n':
			lp[-1] = '\0';
			position = 1;
			return NL;
		case 'O':
			yylval.field.base = -
				base[yylval.field.position = position++];
			yylval.field.width = 1;
			return BLACK;
		case '*':
			yylval.field.base =
				base[yylval.field.position = position++];
			yylval.field.width = 1;
			return WHITE;
		case '-':
			yylval.field.base = 0;
			yylval.field.position = position++;
			yylval.field.width = 1;
			return EMPTY;
		case '?':
			return IGNORE;
		}
	}
}

#define	MAX_COLUMNS	8

int	column;

output (score, comment)
int	score;
char	*comment;
{
	if (verbose)
		printf ("\t%5d,\t/*%s */\n", score, comment);
	else
	{
		printf ("%d,", score);
		if (++column == MAX_COLUMNS) {
			printf ("\n");
			column = 0;
		}
	}
}

flush_output ()
{
	if (!verbose && column)
		printf ("\n");
}
