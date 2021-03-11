%{
/*
 *	user interface
 */

# include	"reversi.h"
# include	<stdio.h>
# include	<signal.h>

extern boardT	board, saveBoard;
extern int	saved;
extern int	savePlayer;
extern int	atend;
extern int	atbegin;
extern int	level;
extern int	player;
extern int	maxlev, movex, movey;
extern int	x, y;
extern int	com;
extern int	gotsignal;
extern char	sbuf[80];
extern char	ebuf[80];
extern int	sdebug, mdebug;
extern int	record;
extern FILE	*rfile;
extern int	first;
extern int	defcom;
extern int	showScore;

extern struct move	saveGame[64];
extern struct move	*saveP;

%}
%token	MOVE LEVEL COMPUTER UNDO HINT PLAY
%token	RECORD REPLAY SAVE
%token	RESTART NEW GAME QUIT
%token	GRID NOGRID HELP NOHELP SCORE NOSCORE
%token	DEBUG EVAL
%token	FROM INTO TO FILEe NO
%token	NUMBER LETTER COMMA NL STRING SEMI EOG ERR
%token	WH BL HUMAN BOTH NEITHER NONE FIRST SECOND
%%
game	:	game commands NL prompt
	|	prompt
	;
prompt	:
		{
		    checkInput ();
		    readLine ();
		}
	;
commands:	commands SEMI command
	|	command
	|	error oerror
		{
			dispHelp ();
		}
	;
command	:	
	|	EOG
		{
			YYACCEPT;
		}
	|	omove LETTER ocomma NUMBER
		{
			domove ($4, $2 - 'a' + 1);
		}
	|	omove NUMBER ocomma LETTER
		{
			domove ($2, $4 - 'a' + 1);
		}
	|	DEBUG STRING
		{
			register char	*s;
			register int	v;

			v = 1;
			for (s = sbuf; *s; ++s)
				switch (*s) {
				case 'm':
					mdebug = v;
					break;
				case 's':
					sdebug = v;
					break;
				case '!':
					v = !v;
					break;
				}
		}
	|	GRID
		{
			dispGrid ();
		}
	|	NO GRID
		{
			dispNoGrid ();
		}
	|	NOGRID
		{
			dispNoGrid ();
		}
	|	SCORE
		{
			showScore = 1;
			dispScore (board);
		}
	|	NOSCORE
		{
			showScore = 0;
			dispNoScore ();
		}
	|	NO SCORE
		{
			showScore = 0;
			dispNoScore ();
		}
	|	LEVEL NUMBER
		{
			level = $2;
		}
	|	LEVEL oerror
		{
			sprintf (ebuf, "current level is %d", level);
			dispError (ebuf);
		}
	|	PLAY whichp
		{
			if ($2 == WHITE || $2 == BLACK) 
				defcom = $2;
			com = $2;
		}
	|	PLAY oerror
		{
			dispError ("play (white black both none)");
		}
	|	whichp FIRST
		{
			if ($1 == WHITE || $1 == BLACK)
				first = $1;
			if (atbegin)
				player = first;
		}
	|	FIRST oerror
		{
			dispError ("(white black you me) first");
		}
	|	whichp SECOND
		{
			if ($1 == WHITE || $1 == BLACK)
				first = - $1;
			if (atbegin)
				player = first;
		}
	|	SECOND oerror
		{
			dispError ("(white black you me) second");
		}
	|	HELP
		{
			dispHelp ();
		}
	|	NOHELP
		{
			dispNoHelp ();
		}
	|	NO HELP
		{
			dispNoHelp ();
		}
	|	QUIT
		{
			YYACCEPT;
		}
	|	UNDO
		{
		    undo ();
		}
	|	NEW ogame eoc
		{
			YYABORT;
		}
	|	RESTART eoc
		{
			YYABORT;
		}
	|	RECORD ointo ofile STRING
		{
		}
	|	RECORD oerror
		{
			dispError ("record \"file\"");
		}
	|	REPLAY ofrom ofile STRING
		{
			replay (sbuf);
		}
	|	REPLAY oerror
		{
			dispError ("replay \"file\"");
		}
	|	SAVE ointo ofile STRING
		{
		    save (sbuf);
		}
	|	SAVE oerror
		{
			dispError ("save \"file\"");
		}
	|	HINT
		{
		    doHint ();
		}
	;
eoc	:	SEMI
	|	NL
	;
omove	:	MOVE
	|
	;
ogame	:	GAME
	|
	;
ocomma	:	COMMA
	|
	;
oerror	:	oerror error
		{
			yyerrok;
		}
	|	oerror ERR
	|
	;
ointo	:	TO
	|	INTO
	|
	;
ofrom	:	FROM
	|
	;
ofile	:	FILEe
	|
	;
whichp	:	WH
		{ $$ = WHITE; }
	|	BL
		{ $$ = BLACK; }
	|	COMPUTER
		{ $$ = com==WHITE?WHITE:BLACK; }
	|	HUMAN
		{ $$ = com==WHITE?BLACK:WHITE; }
	|	BOTH
		{ $$ = 0; }
	|	none
		{ $$ = 2; }
	;
none	:	NONE
	|	NEITHER
	;
%%
yyerror (s)
char	*s;
{
	dispError (s);
}

yywrap ()
{
	return 1;
}

playGame ()
{
    return yyparse ();
}
