/* FILE scanner.l **************************************************
 *
 * xplan - project planning tool
 * Copyright (C) 1992 Brian Gaubert, Mark M. Lacey, Richard Malingkas,
 * and Mike Marlow.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License (distributed with this program in the file
 * COPYING) for more details.
 * 
 * If you did not received a copy of the GNU General Public License
 * along with this program, write to the Free Software Foundation,
 * Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Since this was a project for a one semester software engineering
 * course, the authors will not be offering support for the product
 * after its release.
 *
 * AUTHORS/TESTERS
 *
 * Authors...
 *   Mark M. Lacey, 20 Nov 1992 --- Code derived from compiler project
 *   Brian Gaubert, 25 Nov 1992 --- Modified further to suit our needs 
 *
 */
%{

#include <stdlib.h>
#include <search.h>
#include <string.h>
#include "semantic_stack.h"
#include "parser.h"

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
extern stack_t yylval;
static tok;
static char checkbuf[20];
static char *tmp;
static indx;
static count;
char temp[800];
int lineno=1;

%}

DIGIT    [0-9]
LETTER   [a-zA-Z]

%%

{DIGIT}+				{ /* decimal integer constant */
   /* call strtol to convert */
   yylval.uint = strtol( yytext, NULL, 10 );
   return UNSIGNED;
}

\"([^\"])*\"	{ /* string constant */
   yylval.str = (char *) malloc(yyleng - 2 + 1);
   strncpy(yylval.str, yytext+1, yyleng - 2);
   yylval.str[yyleng-2] = '\0';
   return STRING;
}

=					{ return '='; }
;					{ return ';'; }
,					{ return ','; }
\.					{ return '.'; }

{LETTER}({LETTER}|{DIGIT}|_)*		{ 
   if ( tok = iskeyword( yytext ) ) {
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


