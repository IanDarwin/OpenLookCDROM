/* Top Level Code: Group 7 parser.y */

%{
#include "db.h"
#include "semantic_stack.h"
#include <stdio.h>

static char name[MAXNAMELEN+1];
static char desc[MAXDESCLEN+1];
unsigned duration;
unsigned planned_start_date;
unsigned planned_end_date;
unsigned actual_start_date;
unsigned actual_end_date;
unsigned forecast_start_date;
unsigned forecast_end_date;
unsigned earliest_start_date;
unsigned earliest_end_date;
unsigned latest_start_date;
unsigned latest_end_date;
unsigned float_time;
enum boolean milestone;
enum boolean deliverable;
struct resource_list *resources;
struct task_node *parent;
struct task_list *dependencies;
%}
%token TASK
%token NAME
%token DESCRIPTION
%token PARENT
%token DEPENDENCIES
%token RESOURCES
%token STRING
%token DURATION
%token FLOAT
%token PLANNEDSTART
%token ACTUALSTART
%token FORECASTSTART
%token EARLIESTSTART
%token LATESTSTART
%token PLANNEDEND
%token ACTUALEND
%token FORECASTEND
%token EARLIESTEND
%token LATESTEND
%token MILESTONE
%token DELIVERABLE
%token UNSIGNED
%token TRUE
%token FALSE
%token BEG
%token END
%token TASKS
%token RESOURCES
%%
project 
	: tasklist				
		{ print_rule(1); 
		}
	|					
		{ print_rule(2); 
		}
	;

tasklist
	: taskblock '.'
		{ print_rule(3); 
		}

taskblock
        : taskblockhead taskblocktail
                { print_rule(4);
		}
        ;

taskblockhead
        : BEG TASKS
              {
		print_rule(5);
	      }

taskblocktail
        : tasksublist END TASKS	
		{ print_rule(5); 
		}
	;


tasksublist
	: atask tasksublist			
		{ print_rule(5); 
		}
	| atask					
		{ print_rule(6); 
		}
	;

atask 
	: TASK '=' '{' alist '}'		
		{ print_rule(7); 
		}
	;

alist
	: assignment ';' alist			
		{ print_rule(8); 
		}
	| assignment ';'			
		{ print_rule(9); 
		}
	;

assignment
	: stringassign				
		{ print_rule(10); 
		}
	| dependencylist			
		{ print_rule(11); 
		}
	| resourcelist				
		{ print_rule(12); 
		}
	| numberassign				
		{ print_rule(13); 
		}
	| booleanassign				
		{ print_rule(14); 
		}
	| taskblock				
		{ print_rule(15); 
		}
	;

stringassign 
	: NAME '=' STRING			
		{ print_rule(16);
			strncpy(name, $3.val.str, MAXNAMELEN);
			name[MAXNAMELEN] = '\0';
			free($3.val.str);
		}
	| DESCRIPTION '=' STRING		
		{ print_rule(17);
			strncpy(desc, $3.val.str, MAXDESCLEN);
			desc[MAXDESCLEN] = '\0';
			free($3.val.str);
		}
	| PARENT '=' STRING			
		{ print_rule(18);
/*			parent = find_task(task_list, $3.val.str);*/
			free($3.val.str);
		}
	;

dependencylist
	: dependhead dependencytail		
		{ print_rule(19); 
		}
	;

dependhead
	: BEG DEPENDENCIES			
		{ print_rule(20); 
		}
	;

dependencytail
	: dependency END DEPENDENCIES 		
		{ print_rule(21); 
		}
	| dependency ',' dependencytail		
		{ print_rule(22); 
		}
	;

dependency
	: STRING				
		{ print_rule(23); 
		}
	;

resourcelist
	: resourcehead resourcetail		
		{ print_rule(24); 
		}
	;

resourcehead
	: BEG RESOURCES				
		{ print_rule(25); 
		}
	;

resourcetail
	: resource END RESOURCES 		
		{ print_rule(26); 
		}
	| resource ',' resourcetail		
		{ print_rule(27); 
		}
	;

resource
	: STRING				
		{ print_rule(28); 
		}
	;

numberassign 
	: DURATION '=' UNSIGNED			
		{ print_rule(29);
			duration = $3.val.uint;
		}
	| FLOAT '=' UNSIGNED			
		{ print_rule(30);
			float_time = $3.val.uint;
		}
	| PLANNEDSTART '=' UNSIGNED		
		{ print_rule(31);
			planned_start_date = $3.val.uint;
		}
	| ACTUALSTART '=' UNSIGNED		
		{ print_rule(32);
			actual_start_date = $3.val.uint;
		}
	| FORECASTSTART '=' UNSIGNED		
		{ print_rule(33); 
			forecast_start_date = $3.val.uint;
		}
	| EARLIESTSTART '=' UNSIGNED		
		{ print_rule(34);
			earliest_start_date = $3.val.uint;
		}
	| LATESTSTART '=' UNSIGNED		
		{ print_rule(35);
			latest_start_date = $3.val.uint;
		}
	| PLANNEDEND '=' UNSIGNED		
		{ print_rule(36);
			planned_end_date = $3.val.uint;
		}
	| ACTUALEND '=' UNSIGNED		
		{ print_rule(37);
			actual_end_date = $3.val.uint;
		}
	| FORECASTEND '=' UNSIGNED		
		{ print_rule(38);
			forecast_end_date = $3.val.uint;
		}
	| EARLIESTEND '=' UNSIGNED		
		{ print_rule(39);
			earliest_end_date = $3.val.uint;
		}
	| LATESTEND '=' UNSIGNED        	
		{ print_rule(40);
			latest_end_date = $3.val.uint;
		}
	;

booleanassign 
	: MILESTONE '=' boolean			
		{ print_rule(41);
			milestone = $3.val.uint;
		}
	| DELIVERABLE '=' boolean		
		{ print_rule(42);
			deliverable = $3.val.uint;
		}
	;

boolean
	: TRUE                                  
		{ print_rule(43);
			$$.val.uint = true;
		}
	| FALSE					
		{ print_rule(44);
			$$.val.uint = false;
		}
	;
%%
int yyerror( char *s)
{
}
print_rule(int num)
{
   printf("reduction %d\n", num);
}

