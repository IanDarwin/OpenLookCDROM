%start definitions
%token C CDEF DEFINE FLOAT IDENT INT NUMBER RARROW STRING
%{
#include <stdio.h>
int line_no = 1;
void print_decs(), print_ids(), print_addrs(), print_defs(),
    print_args(), print_sets();
extern FILE *scm, *c, *yyout;
typedef struct ident_info NODE;

struct ident_info {
    int tid;
    char *pid;
    NODE *next;
};
NODE *create_node(), *add_node();

extern scheme;  /* 1 if scheme ; 0 if common lisp */
%}
%%
definitions:
    | definitions def
    ;

def: ps_define | c_define | proc ;

ps_define:
    "#" DEFINE IDENT
        {
            fprintf(yyout, "%s", (char *)$3);
        }
    body
    ;

c_define:
    C ":" "#" DEFINE IDENT
        { 
            fprintf(yyout, "%s", (char *)$5);
	    if (scheme)
	      fprintf(scm, "(define %s ", (char *)$5);
	    else
	      fprintf(scm, "(defvar %s ", (char *) $5);
        }
    value
        {
	    fprintf(scm, "%s)\n", (char *)$7);
	}
    ;

value:
    | NUMBER
        {
	    $$ = (YYSTYPE)$1;
	}
    | IDENT
        {
            fprintf(yyout, "%s", (char *)$1);
	    $$ = (YYSTYPE)$1;
        }
 ;

proc:
    CDEF IDENT
        {
	    fprintf(yyout, "h_");
	    fprintf(yyout, "%s", (char *)$2);
        }
    "(" empty_or_typed_ident_list ")"
    body
    result
        {
	    if ($8) {		/* $8 = result */
		fprintf(c, "int c_%s(index)\nint index;\n{", (char *)$2);
		print_decs((NODE *)$5, 0);
		fprintf(c, "int result;\n");
		fprintf(c, "result = h_%s(", (char *)$2);
		print_addrs((NODE *)$5);
		fprintf(c, ");\nlisp_call(index");
		if ((NODE *)$5) {
		    fprintf(c, ",");
		    print_actuals(c, (NODE *)$5, ", ");
		}
		fprintf(c, ");\nreturn result;\n}\n");
		print_defs((char *)$2, (NODE *)$5);
		fprintf(scm, "(ff:defforeign 'c_%s\n", (char *)$2);
		fprintf(scm, "  :arguments \'(lisp:fixnum)\n");
		fprintf(scm, "  :return-type :fixnum)\n");
		fprintf(scm, "(ff:defun-c-callable %s_scm (", (char *)$2);
		print_ids(scm, (NODE *)$5, " ");
		fprintf(scm, ")\n");
		print_sets((char *)$2, (NODE *)$5);
		fprintf(scm, ")\n");
		if (scheme)
		  fprintf(scm, "(define %s_scm_index\n", (char *)$2);
		else
		  fprintf(scm, "(defvar %s_scm_index\n", (char *)$2);
		fprintf(scm, "  (cadr (lisp:multiple-value-list\n    (ff:");
		fprintf(scm, "register-function \'%s_scm))))\n", (char *)$2);
		if (scheme)
		  fprintf(scm, "(define (%s)\n", (char *)$2);
		else
		  fprintf(scm, "(defun %s ()\n", (char *)$2);
		fprintf(scm, "  (not (= (c_%s %s_scm_index) 0)))\n",
			(char *)$2, (char *)$2);
	      } else {
		  fprintf(c, "void %s(", (char *)$2);
		  print_ids(c, (NODE *)$5, ", ");
		  fprintf(c, ")\n");
		  print_decs((NODE *)$5, 1);
		  fprintf(c, "{ h_%s(", (char *)$2);
		  print_ids(c, (NODE *)$5, ", ");
		  fprintf(c, "); }\n");
		  fprintf(scm, "(ff:defforeign \'");
		  fprintf(scm, "%s", (char *)$2);
		  fprintf(scm, "\n  :arguments \'(");
		  print_args((NODE *)$5);
		  fprintf(scm, ")\n  :return-type :void)\n");
	      }
	}
    ;

body:
    | body body_token
    ;

body_token:
    IDENT
        {
            fprintf(yyout, "%s", (char *)$1);
        }
    | INT | FLOAT | NUMBER | STRING | "," | ":"
    | "(" extended_body ")"
    ;

extended_body:
    | extended_body body_token
    | extended_body RARROW
    ;

result:
        {
	    $$ = (YYSTYPE)0;
	}
    | RARROW IDENT
        {
	    fprintf(yyout, "%s", (char *)$2);
	}
    "(" empty_or_ident_list ")"
        {
	    $$ = (YYSTYPE)1;
	}
    ;

empty_or_typed_ident_list:
        {
	    $$ = (YYSTYPE)((NODE *)0);
	}
    | typed_ident_list
    ;

typed_ident_list:
    typed_ident
    | typed_ident_list ',' typed_ident
        {
	    $$ = (YYSTYPE)add_node((NODE *)$1, (NODE *)$3);
	}
    ;

typed_ident:
    IDENT
        {
	    fprintf(yyout, "%s", (char *)$1);
	    $$ = (YYSTYPE)create_node(INT, (char *)$1);
	}
    | INT IDENT
        {
	    fprintf(yyout, "%s", (char *)$2);
	    $$ = (YYSTYPE)create_node(INT, (char *)$2);
	}
    | FLOAT IDENT
        {
	    fprintf(yyout, "%s", (char *)$2);
	    $$ = (YYSTYPE)create_node(FLOAT, (char *)$2);
	}
    | STRING IDENT
        {
	    fprintf(yyout, "%s", (char *)$2);
	    $$ = (YYSTYPE)create_node(STRING, (char *)$2);
	}
    ;

empty_or_ident_list:
        {
	    $$ = (YYSTYPE)(NODE *)0;
	}
    | ident_list
    ;

ident_list:
    IDENT
        {
	    fprintf(yyout, "%s", (char *)$1);
	    $$ = (YYSTYPE)create_node(0, (char *)$1);
	}
    | ident_list ',' IDENT
        {
	    fprintf(yyout, "%s", (char *)$3);
	    $$ = (YYSTYPE)add_node((NODE *)$1,
				   create_node(0, (char *)$3));
	}
    ;
%%
void yyerror(s)
char *s;
{
    printf("line %d: %s\n", line_no, s);
}

int *yyerrlval()
{
    static int i = 0;
    return(&i);
}

NODE *create_node(t, s)
int t;
char *s;
{
    NODE *n;
    n = (NODE *)malloc(sizeof(NODE));
    n->tid = t;
    n->pid = s;
    n->next = (NODE *)0;
    return (n);
}

NODE *add_node(n1, n2)
NODE *n1, *n2;
{
    NODE *n = n1;

    while (n->next) {
	n = n->next;
    }
    n->next = n2;
    return(n1);
}

void print_decs(n,argument)
NODE *n;
int argument;
{
    while (n) {
	switch (n->tid) {
	  case INT:
	    fprintf(c, "int %s;\n", n->pid);
	    break;
	  case FLOAT:
	    fprintf(c, "float %s;\n", n->pid);
	    break;
	  case STRING:
	    if (argument)
	      fprintf(c, "char * %s;\n", n->pid);
	    else
	      fprintf(c, "static char %s[1024];\n", n->pid);
	    break;
	}
	n = n->next;
    }
}

void print_actuals (f, n, sep)
FILE *f;
NODE *n;
char *sep;
{
    while (n) {
        if (n->tid == STRING)
	  fprintf(f,"(unsigned long)&%s[0]",n->pid);
	else
	  fprintf(f, "%s", n->pid);
	if (n->next) fprintf(f, "%s", sep);
	n = n->next;
    }
}

void print_ids(f, n, sep)
FILE *f;
NODE *n;
char *sep;
{
    while (n) {
        if (n->tid == STRING)
	  fprintf(f, "%s", n->pid);
	else
	  fprintf(f, "%s", n->pid);
	if (n->next) fprintf(f, "%s", sep);
	n = n->next;
    }
}

void print_addrs(n)
NODE *n;
{
    while (n) {
        if (n->tid == STRING)
	  fprintf(c, "&%s[0]", n->pid);
	else
	  fprintf(c, "&%s", n->pid);
	if (n->next) fprintf(c, ", ");
	n = n->next;
    }
}

void print_defs(proc, n)
char *proc;
NODE *n;
{
    while (n) {
        if (scheme)
	  fprintf(scm, "(define %s_%s)\n", proc, n->pid);
	else
	  fprintf(scm, "(defvar %s_%s nil)\n", proc, n->pid);	  
	n = n->next;
    }
}

void print_args(n)
NODE *n;
{
    while (n) {
	switch (n->tid) {
	  case INT:
	    fprintf(scm, "lisp:fixnum ");
	    break;
	  case FLOAT:
	    if (scheme)
	      fprintf(scm, "lisp:flonum ");
	    else
	      fprintf(scm, "lisp:single-float ");
	    break;
	  case STRING:
	    fprintf(scm, "lisp:string ");
	    break;
	}
	n = n->next;
    }
}

void print_sets(proc, n)
char *proc;
NODE *n;
{
    while (n) {
        if (n->tid == STRING)
	  if (scheme)
	    fprintf(scm, "  (set! %s_%s (string-get %s))\n",
		    proc, n->pid, n->pid);
	  else
	    fprintf(scm, " (setq %s_%s (string-get %s))\n",
		    proc, n->pid, n->pid);
	else
	  if (scheme)
	    fprintf(scm, "  (set! %s_%s %s)\n", proc, n->pid, n->pid);
	  else
	    fprintf(scm, "  (setq %s_%s %s)\n", proc, n->pid, n->pid);
	n = n->next;
    }
}
