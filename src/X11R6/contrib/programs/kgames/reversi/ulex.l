%{
/*
 *	ulex.l 
 *
 *	lexer for the user interface
 */
# include	"y.tab.h"
extern int	yylval;
extern char	sbuf[];
#undef input
#define input()	lexgetc()
#undef unput
#define unput(c)	lexungetc(c)
%}
%%
[ \t]		;
\004		return EOG;
black		{ yylval = -1; return BL; }
both		return BOTH;
computer	return COMPUTER;
debug		return DEBUG;
file		return FILEe;
first		return FIRST;
from		return FROM;
game		return GAME;
grid		return GRID;
help		return HELP;
hint		return HINT;
human		return HUMAN;
into		return INTO;
level		return LEVEL;
me		return HUMAN;
move		return MOVE;
new		return NEW;
neither		return NEITHER;
no		return NO;
nogrid		return NOGRID;
nohelp		return NOHELP;
noscore		return NOSCORE;
none		return NONE;
play		return PLAY;
quit		return QUIT;
record		return RECORD;
replay		return REPLAY;
restart		return RESTART;
save		return SAVE;
score		return SCORE;
second		return SECOND;
to		return TO;
undo		return UNDO;
white		{ yylval =  1; return WH; }
you		return COMPUTER;
\n		return NL;
[0-9]+		{ yylval = atoi (yytext); return NUMBER; }
[a-h]		{ yylval = *yytext; return LETTER; }
[A-H]		{ yylval = *yytext + 'a' - 'A'; return LETTER; }
","		return COMMA;
";"		return SEMI;
\"[^"]*\"	{ strcpy (sbuf, yytext+1); sbuf[yyleng-2]='\0'; return STRING; }
.		return ERR;
