/* mess.y  -  test bison error handling */


/* these are the warning messages generated in bison 

getargs.c: warns("extra argument ignored: %s", program_name);

lex.c:	warn ("unexpected `/' found and ignored");
lex.c:	warn ("unescaped newline in constant");
lex.c:	warns("more than three octal digits: `\\%o'", code);
lex.c:	warns("hexadecimal value above 255: `\\x%x'", code);
lex.c:	warns("unknown escape sequence `\\%c'", c);
lex.c:	warns("unknown escape sequence: `\\' followed by char code 0x%x", c);
lex.c:	warn ("use \"...\" for multicharacter literal tokens");
lex.c:	warn ("unterminated type name");
*/

/* in subroutine skip_to_char */
/*  reader.c:  warn ("   Skipping to next \\n");  */
/*  reader.c:  warns("   Skipping to next %c", target);  */

/* declarations section */

/* read_declarations */
/*  reader.c:  warns("unrecognized: %s", token_buffer);  */
/*  reader.c:  warns("unknown character: %c (\\%03o)", c);  */

/* copy definition %{ ... %} */
/*  reader.c:  warn ("unterminated string");  */

/* parse_token_declaration */
/*  reader.c:  warns("symbol %s redefined", symval->tag);  */
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
/*  reader.c:  warns("'%s' is invalid in %s",  */

/* parse_thong_declaration */
/*  reader.c:  warns("unrecognized item %s, expected an identifier",   */
/*  reader.c:  warns("expected string constant instead of %s",   */

/* parse_start_decl */
/*  reader.c:  warn ("multiple %%start declarations");  */
/*  reader.c:  warn ("invalid %%start declaration");  */

/* parse_type_decl */
/*  reader.c:  warn ("%%type declaration has no <typename>");  */
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
/*  reader.c:  warns("invalid %%type declaration due to item: %s", token_buffer);  */

/* parse_assoc_decl */
/*  reader.c:  warns("redefining precedence of %s", symval->tag);  */
/*  reader.c:  warns("symbol %s redefined", symval->tag);  */
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
/*  reader.c:  warns("invalid text (%s).  Number should be after identifier.",   */
/*  reader.c:  warns("unexpected item: %s", token_buffer);  */

/* parse_union_decl */
/*  reader.c:  warn ("multiple %%union declarations");  */
/*  reader.c:  warn ("unmatched close-brace (`}')");  */

/* parse_expect_decl */
/*  reader.c:  warn ("funny integer after %%expect");  */


/* rules section */

/* get_type_name */
/*  reader.c:  warn ("invalid $ value"); n < 0  */
/*  reader.c:  warn ("invalid $ value"); n > # items */

/* copy_guard */
/*  reader.c:  warn ("unmatched right brace ('}')");  */
/*  reader.c:  warn ("unterminated string");  */
/*  reader.c:  warns("$$ of '%s' has no declared type.", rule->sym->tag);  */
/*  reader.c:  warns("$%d of '%s' has no declared type.", n, rule->sym->tag);  */
/*  reader.c:  warns("$%c is invalid", c);  */
/*  reader.c:  warns("@%c is invalid", c);  */

/* copy_action */
/*  reader.c:  warn ("unterminated string");  */
/*  reader.c:  warns("$$ of '%s' has no declared type.", rule->sym->tag);  */
/*  reader.c:  warns("$%d of '%s' has no declared type.", n, rule->sym->tag);  */
/*  reader.c:  warns("$%c is invalid",c);  */
/*  reader.c:  warn ("invalid @-construct");  */

/* readgram */
/*  reader.c:  warn ("ill-formed rule: initial symbol not followed by colon");  */
/*  reader.c:  warn ("grammar starts with vertical bar");  */
/*  reader.c:  warns("rule given for %s, which is a token", lhs->tag);  */
/*  reader.c:  warn ("two actions at end of one rule");  */
/*  reader.c:  warns("type clash ('%s' '%s') on default action",  */
/*  reader.c:  warn ("empty rule for typed nonterminal, and no action");  */
/*  reader.c:  warns("invalid input: %s", token_buffer);  */
/*  reader.c:  warns("symbol %s is used, but is not defined as a token and has no rules",  */

/* get_type */
/*  reader.c:  warn ("ill-formed %%type declaration");  */
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */

/* packsymbols */
/*  reader.c:  warns("conflicting precedences for %s and %s",  */
/*  reader.c:  warns("conflicting assoc values for %s and %s",  */
/*  reader.c:  warns("tokens %s and %s both assigned number %d",  */



/*  lex.c:	warn ("unexpected `/' found and ignored");  */
/ 
/*  lex.c:	warn ("unescaped newline in constant");  */
%token a "...
/*  lex.c:	warns("octal value outside range 0...255: `\\%o'", code);  */
%token b "\3333"
/*  lex.c:	warns("hexadecimal value above 255: `\\x%x'", code);  */
%token c "\x1Fa"
/*  lex.c:	warns("unknown escape sequence `\\%c'", c);  */
%token d "\q"
/*  lex.c:	warns("unknown escape sequence: `\\' followed by char code 0x%x", c);  */
%token e "\"
/*  lex.c:	warn ("use \"...\" for multicharacter literal tokens");  */
%token f 'ab'
/*  lex.c:	warn ("unterminated type name");  */
%token <unterminated g
%token 

/* in subroutine skip_to_char */
/*  reader.c:  warn ("   Skipping to next \\n");  */
	/* never used */
/*  reader.c:  warns("   Skipping to next %c", target);  */
	/* appears many places below;  target is always '%' */


/* declarations section */

%union {int u1; long u2; double u3;}

/* read_declarations */
/*  reader.c:  warns("unrecognized: %s", token_buffer);  */
%garbage
/*  reader.c:  warns("unknown character: %c (\\%03o)", c);  */
%start foo $

/* copy definition %{ ... %} */
/*  reader.c:  warn ("unterminated string");  */
%{ "...
%}

/* parse_token_declaration */
%token <u1> bog sog tog
%token <u2> gog
/*  reader.c:  warns("symbol %s redefined", symval->tag);  */
%nterm <u1> bog rog
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
%token <u2> bog
/*  reader.c:  warns("'%s' is invalid in %s",  */
%token <u1> ;

/* parse_thong_declaration */
/*  reader.c:  warns("unrecognized item %s, expected an identifier",   */
%thong 3
/*  reader.c:  warns("expected string constant instead of %s",   */
%thong lesseq 4
%thong geq 5 ">="

/* parse_start_decl */
/*  reader.c:  warn ("multiple %%start declarations");  */
%start fog
/*  reader.c:  warn ("invalid %%start declaration");  */
%start 3

/* parse_type_decl */
/*  reader.c:  warn ("%%type declaration has no <typename>");  */
%type fog
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
%type <u1> gog
/*  reader.c:  warns("invalid %%type declaration due to item: %s", token_buffer);  */
%type <u1> fog :

/* parse_assoc_decl */
%left '-' 3
/*  reader.c:  warns("redefining precedence of %s", symval->tag);  */
%right '-'
/*  reader.c:  warns("symbol %s redefined", symval->tag);  */
%right rog
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */
%left <u1> gog 3
/*  reader.c:  warns("invalid text (%s).  Number should be after identifier.",   */
%left 8 '+'
/*  reader.c:  warns("unexpected item: %s", token_buffer);  */
%nonassoc '<' 1 {  '>' 

/* parse_union_decl */
/*  reader.c:  warn ("multiple %%union declarations");  */
/*  reader.c:  warn ("unmatched close-brace (`}')");  */
%union }{int u1; long u2;}


/* parse_expect_decl */
/*  reader.c:  warn ("funny integer after %%expect");  */
%expect 000000000000000023%thong confprec "<<"
%left confprec
%right "<<"
%thong confassoc "=="
%left confassoc
%nonassoc "=="

%semantic_parser
	

%%

/* rules section */

/* readgram */
/*  reader.c:  warn ("grammar starts with vertical bar");  */
	|  ;
/*  reader.c:  warn ("ill-formed rule: initial symbol not followed by colon");  */
rr 3
/*  reader.c:  warns("rule given for %s, which is a token", lhs->tag);  */
confprec : /* empty */ ;
/*  reader.c:  warn ("two actions at end of one rule");  */
	/* this message cannot occur */
twoact : '.' {$$=NULL;} {$$=NULL;} ;
/*  reader.c:  warns("type clash ('%s' '%s') on default action",  */
gog : fog;
/*  reader.c:  warn ("empty rule for typed nonterminal, and no action");  */
gog : /* empty */ ;
/*  reader.c:  warns("invalid input: %s", token_buffer);  */
{
/*  reader.c:  warns("symbol %s is used, but is not defined as a token and has no rules",  */
	/* fog */

/* get_type_name */
/*  reader.c:  warn ("invalid $ value"); n < 0  */
  r1 : rog {$-1 = NULL;}
/*  reader.c:  warn ("invalid $ value"); n > # items */
	tog {$3 = NULL;} ;

/* copy_guard */
r1 : '.'
/*  reader.c:  warn ("unmatched right brace ('}')");  */
	%guard } ;
/*  reader.c:  warn ("unterminated string");  */
r1 : ',' %guard { "...
	} ;
/*  reader.c:  warns("$$ of '%s' has no declared type.", rule->sym->tag);  */
r1 : ';' %guard { int i; i = $$; 	
/*  reader.c:  warns("$%d of '%s' has no declared type.", n, rule->sym->tag);  */
	i = $1; 
/*  reader.c:  warns("$%c is invalid", c);  */
	i = $2;
/*  reader.c:  warns("@%c is invalid", c);  */
	@x; } ;

/* copy_action {*/
/*  reader.c:  warn ("unterminated string");  */
r1 : 'a' { "...
	} ;
/*  reader.c:  warns("$$ of '%s' has no declared type.", rule->sym->tag);  */
r1 : 'b' {$$ = 3;};
/*  reader.c:  warns("$%d of '%s' has no declared type.", n, rule->sym->tag);  */
r1 : 'c' {$1 = 3;};
/*  reader.c:  warns("$%c is invalid",c);  */
r1 : 'd' {$x = 4;};
/*  reader.c:  warn ("invalid @-construct");  */
r1 : 'e' {@x;};

/* get_type */
/* this function is unused */
/*  reader.c:  warn ("ill-formed %%type declaration");  */
/*  reader.c:  warns("type redeclaration for %s", symval->tag);  */

/* packsymbols */
/*  reader.c:  warns("conflicting precedences for %s and %s",  */
	/* %thong confprec "<<"
	%left confprec
	%right "<<"  */
/*  reader.c:  warns("conflicting assoc values for %s and %s",  */
	/* %thong confassoc "=="
	%left confassoc
	%nonassoc "=="  */
/*  reader.c:  warns("tokens %s and %s both assigned number %d",  */
	/* gog and '-' are both 3 */

