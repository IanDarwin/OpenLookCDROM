/* Top Level Code: Group 7 scanner.l */

%{

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <search.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include "semantic_stack.h"
#include "parser.tab.h"

#define NUM_KW 25

struct string_entry {
   char *string;
} keywords[NUM_KW] = {
   "actual_end", "actual_start", "begin", "deliverable", "dependencies",
   "description", "duration", "earliest_end", "earliest_start",
   "end", "false", "float", "forecast_end", "forecast_start", "latest_end",
   "latest_start", "milestone", "name", "parent", "planned_end", 
   "planned_start", "resources", "task", "tasks", "true"
};

/* tokens for our keywords */
int keytoks[NUM_KW] = {
   ACTUALEND, ACTUALSTART, BEG, DELIVERABLE, DEPENDENCIES,
   DESCRIPTION, DURATION, EARLIESTEND, EARLIESTSTART,
   END, FALSE, FLOAT, FORECASTEND, FORECASTSTART, LATESTEND,
   LATESTSTART, MILESTONE, NAME, PARENT, PLANNEDEND,
   PLANNEDSTART, RESOURCES, TASK, TASKS, TRUE
};

/* prototypes */
int iskeyword(char *);
int compare(struct string_entry *, struct string_entry *);

extern char *filename;

static tok;
static char checkbuf[20];
static char *tmp;
static indx;
static count;

/*strhashtable *scontbl_ptr;
strhashtable *idtbl_ptr;
realhashtable *rcontbl_ptr;
*/
int lineno=1;

%}

DIGIT    [0-9]
LETTER   [a-zA-Z]

%%

{DIGIT}+				{ /* decimal integer constant */
   /* call strtol to convert */
   yylval.token_code = UNSIGNED;
   yylval.val.uint = strtol( yytext, NULL, 10 );

   return UNSIGNED;
}

\"([^\"])*\"	{ /* string constant */
   yylval.token_code = STRING;
   yylval.val.str = (char *) malloc(yyleng-1);
   strncpy(yylval.val.str, yytext, yyleng-2);
   yylval.val.str[yyleng-1] = '\0';
   return STRING;
}

=					{ return '='; }
;					{ return ';'; }
,					{ return ','; }
\.					{ return '.'; }

{LETTER}({LETTER}|{DIGIT}|_)*		{ 
   if ( tok = iskeyword( yytext ) ) {
      yylval.token_code = tok;
      return tok;
   } else {
   }
}

[ \t]					{ /* whitespace */ }

\n					{ /* newline */ ++lineno; }

.					{ 
/* Any character that we do not expect */
	return yytext[0];
}

%%

/*
  Perform a binary search on the list of keywords to see if this
  word is present.  If it is, return the token value for it.  If not,
  just return 0, to tell the calling routine that this is an identifier.
  */
  iskeyword( char *lexeme )
{
   int indx;
   struct string_entry tmp;
   struct string_entry *retval;
   
   tmp.string = lexeme;
   
   retval = (struct string_entry *)
     bsearch((char *)&tmp, (char *)keywords, NUM_KW, 
	     sizeof(struct string_entry), compare);
   
   if ( retval == NULL ) return 0;
   
   indx =  ((int) retval- (int) keywords)/(sizeof(struct
						  string_entry));
   
   return keytoks[indx];
}

int compare(struct string_entry *s1, struct string_entry *s2)
{
   return strcmp(s1->string, s2->string);
}


