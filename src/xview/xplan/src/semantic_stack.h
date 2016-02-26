/* FILE semantic_stack.h ****************************************************
 *
 *  Description of contents:
 *
 *   This file contains the stack used in the processing of the
 *   semantic stack used in yylex and yyparse.
 *
 * FILE semantic_stack.h */


struct nodes {
  char str_const[60];
  int size;
  struct nodes *next;
};

typedef struct {
  unsigned uint;
  char *str;
  struct task_node *node;
} stack_t;

#ifndef YYSTYPE
#define YYSTYPE stack_t
#endif

YYSTYPE yylval;

/*#endif*/


