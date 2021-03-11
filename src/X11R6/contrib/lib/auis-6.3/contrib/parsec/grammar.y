/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** 

$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

%token PC_ADDASSIGN
%token PC_ANDASSIGN
%token PC_AUTO
%token PC_BREAK
%token PC_CASE
%token PC_CHAR
%token PC_CHARACTER_CONSTANT
%token PC_CONST
%token PC_CONTINUE
%token PC_DECR
%token PC_DEFAULT
%token PC_DEREF
%token PC_DIVASSIGN
%token PC_DO
%token PC_DOUBLE
%token PC_ELLIPSIS
%token PC_ELSE
%token PC_ENUM
%token PC_ENUMERATION_CONSTANT
%token PC_EQUAL
%token PC_EXTERN
%token PC_FLOAT
%token PC_FLOATING_CONSTANT
%token PC_FOR
%token PC_GE
%token PC_GOTO
%token PC_IDENTIFIER
%token PC_IF
%token PC_INCR
%token PC_INT
%token PC_INTEGER_CONSTANT
%token PC_LE
%token PC_LEFT
%token PC_LEFTASSIGN
%token PC_LOGICAL_AND
%token PC_LOGICAL_OR
%token PC_LONG
%token PC_MODASSIGN
%token PC_MULASSIGN
%token PC_NOT_EQUAL
%token PC_ORASSIGN
%token PC_REGISTER
%token PC_RETURN
%token PC_RIGHT
%token PC_RIGHTASSIGN
%token PC_SHORT
%token PC_SIGNED
%token PC_SIZEOF
%token PC_STATIC
%token PC_STRING_CONSTANT
%token PC_STRUCT
%token PC_SUBASSIGN
%token PC_SWITCH
%token PC_TYPEDEF
%token PC_TYPEDEF_NAME
%token PC_UNION
%token PC_UNSIGNED
%token PC_VOID
%token PC_VOLATILE
%token PC_WHILE
%token PC_XORASSIGN

%start top

%{

#include <andrewos.h>
#include <p1.h>
#include <stdio.h>

#define yylex PC_NextToken

static PC_ParseNode_t *parseResult;
static int Fold;

static char **enumerators, **typedefs;
static int enumeratorsUsed, enumeratorsAllocated;
static int typedefsUsed, typedefsAllocated;

extern int PC_NextToken();

static int ContainsTypedef();
static int GrowTypedefsIfNecessary();
static int GrowEnumeratorsIfNecessary();

char *PC_ParseError;

%}

%union {
    PC_ParseNode_t *node;
    char buffer[PC_TOKENMAX];
}

%type <node> module external_declaration function_definition function_body
%type <node> declaration declaration_list declaration_specifiers
%type <node> storage_class_specifier type_specifier type_qualifier
%type <node> struct_or_union_specifier struct_or_union struct_declaration_list
%type <node> init_declarator_list init_declarator struct_declaration
%type <node> specifier_qualifier_list struct_declarator_list
%type <node> struct_declarator enum_specifier enumerator_list
%type <node> enumerator declarator direct_declarator pointer
%type <node> type_qualifier_list parameter_type_list parameter_list
%type <node> parameter_declaration identifier_list initializer
%type <node> initializer_list type_name abstract_declarator
%type <node> direct_abstract_declarator statement labeled_statement
%type <node> expression_statement compound_statement statement_list
%type <node> selection_statement iteration_statement jump_statement
%type <node> expression assignment_expression assignment_operator
%type <node> conditional_expression constant_expression logical_or_expression
%type <node> logical_and_expression inclusive_or_expression
%type <node> exclusive_or_expression and_expression equality_expression
%type <node> relational_expression shift_expression additive_expression
%type <node> multiplicative_expression cast_expression unary_expression
%type <node> unary_operator postfix_expression primary_expression
%type <node> argument_expression_list constant
%type <node> identifier
%type <node> struct_typedef_tag

%%

top:
	module
		{
			parseResult = $1;
		}
	;

module:
	external_declaration
		{
			$$ = Fold ? $1 : EmitNode(pnt_module, 1, 1, $1, 0, 0, 0);
		}
	| module external_declaration
		{
			$$ = EmitNode(pnt_module, 2, 2, $1, $2, 0, 0);
		}
	;

external_declaration:
	function_definition
		{
			$$ = Fold ? $1 : EmitNode(pnt_external_declaration, 1, 1, $1, 0, 0, 0);
		}
	| declaration
		{
			$$ = Fold ? $1 : EmitNode(pnt_external_declaration, 2, 1, $1, 0, 0, 0);
		}
	;

function_definition:
	declarator function_body
		{
			$$ = EmitNode(pnt_function_definition, 1, 2, $1, $2, 0, 0);
		}
	| declaration_specifiers declarator function_body
		{
			$$ = EmitNode(pnt_function_definition, 2, 3, $1, $2, $3, 0);
		}
	;

function_body:
	compound_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_function_body, 1, 1, $1, 0, 0, 0);
		}
	| declaration_list compound_statement
		{
			$$ = EmitNode(pnt_function_body, 2, 2, $1, $2, 0, 0);
		}
	;

declaration:
	declaration_specifiers ';'
		{
			$$ = EmitNode(pnt_declaration, 1, 1, $1, 0, 0, 0);
		}
	| declaration_specifiers init_declarator_list ';'
		{
			$$ = EmitNode(pnt_declaration, 2, 2, $1, $2, 0, 0);
			if (ContainsTypedef($1))
				RememberTypedefs($2);
		}
	;

declaration_list:
	declaration
		{
			$$ = Fold ? $1 : EmitNode(pnt_declaration_list, 1, 1, $1, 0, 0, 0);
		}
	| declaration_list declaration
		{
			$$ = EmitNode(pnt_declaration_list, 2, 2, $1, $2, 0, 0);
		}
	;

declaration_specifiers:
	storage_class_specifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_declaration_specifiers, 1, 1, $1, 0, 0, 0);
		}
	| storage_class_specifier declaration_specifiers
		{
			$$ = EmitNode(pnt_declaration_specifiers, 2, 2, $1, $2, 0, 0);
		}
	| type_specifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_declaration_specifiers, 3, 1, $1, 0, 0, 0);
		}
	| type_specifier declaration_specifiers
		{
			$$ = EmitNode(pnt_declaration_specifiers, 4, 2, $1, $2, 0, 0);
		}
	| type_qualifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_declaration_specifiers, 5, 1, $1, 0, 0, 0);
		}
	| type_qualifier declaration_specifiers
		{
			$$ = EmitNode(pnt_declaration_specifiers, 6, 2, $1, $2, 0, 0);
		}
	;

storage_class_specifier:
	PC_AUTO
		{
			$$ = EmitToken(pnt_storage_class_specifier, PC_AUTO);
		}
	| PC_REGISTER
		{
			$$ = EmitToken(pnt_storage_class_specifier, PC_REGISTER);
		}
	| PC_STATIC
		{
			$$ = EmitToken(pnt_storage_class_specifier, PC_STATIC);
		}
	| PC_EXTERN
		{
			$$ = EmitToken(pnt_storage_class_specifier, PC_EXTERN);
		}
	| PC_TYPEDEF
		{
			$$ = EmitToken(pnt_storage_class_specifier, PC_TYPEDEF);
		}
	;

type_specifier:
	PC_VOID
		{
			$$ = EmitToken(pnt_type_specifier, PC_VOID);
		}
	| PC_CHAR
		{
			$$ = EmitToken(pnt_type_specifier, PC_CHAR);
		}
	| PC_SHORT
		{
			$$ = EmitToken(pnt_type_specifier, PC_SHORT);
		}
	| PC_INT
		{
			$$ = EmitToken(pnt_type_specifier, PC_INT);
		}
	| PC_LONG
		{
			$$ = EmitToken(pnt_type_specifier, PC_LONG);
		}
	| PC_FLOAT
		{
			$$ = EmitToken(pnt_type_specifier, PC_FLOAT);
		}
	| PC_DOUBLE
		{
			$$ = EmitToken(pnt_type_specifier, PC_DOUBLE);
		}
	| PC_SIGNED
		{
			$$ = EmitToken(pnt_type_specifier, PC_SIGNED);
		}
	| PC_UNSIGNED
		{
			$$ = EmitToken(pnt_type_specifier, PC_UNSIGNED);
		}
	| struct_or_union_specifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_type_specifier, 10, 1, $1, 0, 0, 0);
		}
	| enum_specifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_type_specifier, 11, 1, $1, 0, 0, 0);
		}
	| PC_TYPEDEF_NAME
		{
			$$ = EmitString(pnt_type_specifier, PC_TYPEDEF_NAME, yylval.buffer);
		}
	;

type_qualifier:
	PC_CONST
		{
			$$ = EmitToken(pnt_type_qualifier, PC_CONST);
		}
	| PC_VOLATILE
		{
			$$ = EmitToken(pnt_type_qualifier, PC_VOLATILE);
		}
	;

struct_or_union_specifier:
	struct_or_union '{' struct_declaration_list '}'
		{
			$$ = EmitNode(pnt_struct_or_union_specifier, 1, 2, $1, $3, 0, 0);
		}
	| struct_or_union identifier '{' struct_declaration_list '}'
		{
			$$ = EmitNode(pnt_struct_or_union_specifier, 2, 3, $1, $2, $4, 0);
		}
	| struct_or_union struct_typedef_tag '{' struct_declaration_list '}' /* typedef names may be used for struct names, too. */
		{
			$$ = EmitNode(pnt_struct_or_union_specifier, 2, 3, $1, $2, $4, 0);
		}
	| struct_or_union identifier
		{
			$$ = EmitNode(pnt_struct_or_union_specifier, 3, 2, $1, $2, 0, 0);
		}
	| struct_or_union struct_typedef_tag    /* A typedef name is also a legal struct tag. */
		{
			$$ = EmitNode(pnt_struct_or_union_specifier, 3, 2, $1, $2, 0, 0);
		}
	;

struct_or_union:
	PC_STRUCT
		{
			$$ = EmitToken(pnt_struct_or_union, PC_STRUCT);
		}
	| PC_UNION
		{
			$$ = EmitToken(pnt_struct_or_union, PC_UNION);
		}
	;

struct_declaration_list:
	struct_declaration
		{
			$$ = Fold ? $1 : EmitNode(pnt_struct_declaration_list, 1, 1, $1, 0, 0, 0);
		}
	| struct_declaration_list struct_declaration
		{
			$$ = EmitNode(pnt_struct_declaration_list, 2, 2, $1, $2, 0, 0);
		}
	;

init_declarator_list:
	init_declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_init_declarator_list, 1, 1, $1, 0, 0, 0);
		}
	| init_declarator_list ',' init_declarator
		{
			$$ = EmitNode(pnt_init_declarator_list, 2, 2, $1, $3, 0, 0);
		}
	;

init_declarator:
	declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_init_declarator, 1, 1, $1, 0, 0, 0);
		}
	| declarator '=' initializer
		{
			$$ = EmitNode(pnt_init_declarator, 2, 2, $1, $3, 0, 0);
		}
	;

struct_declaration:
	specifier_qualifier_list struct_declarator_list ';'
		{
			$$ = EmitNode(pnt_struct_declaration, 2, 2, $1, $2, 0, 0);
		}
	;

specifier_qualifier_list:
	type_specifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_specifier_qualifier_list, 1, 1, $1, 0, 0, 0);
		}
	| type_specifier specifier_qualifier_list
		{
			$$ = EmitNode(pnt_specifier_qualifier_list, 2, 2, $1, $2, 0, 0);
		}
	| type_qualifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_specifier_qualifier_list, 3, 1, $1, 0, 0, 0);
		}
	| type_qualifier specifier_qualifier_list
		{
			$$ = EmitNode(pnt_specifier_qualifier_list, 4, 2, $1, $2, 0, 0);
		}
	;

struct_declarator_list:
	struct_declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_struct_declarator_list, 1, 1, $1, 0, 0, 0);
		}
	| struct_declarator_list ',' struct_declarator
		{
			$$ = EmitNode(pnt_struct_declarator_list, 2, 2, $1, $3, 0, 0);
		}
	;

struct_declarator:
	declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_struct_declarator, 1, 1, $1, 0, 0, 0);
		}
	| ':' constant_expression
		{
			$$ = EmitNode(pnt_struct_declarator, 2, 1, $2, 0, 0, 0);
		}
	| declarator ':' constant_expression
		{
			$$ = EmitNode(pnt_struct_declarator, 3, 2, $1, $3, 0, 0);
		}
	;

enum_specifier:
	PC_ENUM '{' enumerator_list '}'
		{
			$$ = EmitNode(pnt_enum_specifier, 1, 1, $3, 0, 0, 0);
		}
	| PC_ENUM identifier '{' enumerator_list '}'
		{
			$$ = EmitNode(pnt_enum_specifier, 2, 2, $2, $4, 0, 0);
		}
	| PC_ENUM identifier
		{
			$$ = EmitNode(pnt_enum_specifier, 3, 1, $2, 0, 0, 0);
		}
	;

enumerator_list:
	enumerator
		{
			$$ = Fold ? $1 : EmitNode(pnt_enumerator_list, 1, 1, $1, 0, 0, 0);
		}
	| enumerator_list ',' enumerator
		{
			$$ = EmitNode(pnt_enumerator_list, 2, 2, $1, $3, 0, 0);
		}
	;

enumerator:
	identifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_enumerator, 1, 1, $1, 0, 0, 0);
			RememberEnumerator(FindName($1));
		}
	| identifier '=' constant_expression
		{
			$$ = EmitNode(pnt_enumerator, 2, 2, $1, $3, 0, 0);
			RememberEnumerator(FindName($1));
		}
	;

declarator:
	direct_declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_declarator, 1, 1, $1, 0, 0, 0);
		}
	| pointer direct_declarator
		{
			$$ = EmitNode(pnt_declarator, 2, 2, $1, $2, 0, 0);
		}
	;

direct_declarator:
	identifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_direct_declarator, 1, 1, $1, 0, 0, 0);
		}
	| '(' declarator ')'
		{
			$$ = EmitNode(pnt_direct_declarator, 2, 1, $2, 0, 0, 0);
		}
	| direct_declarator '[' ']'
		{
			$$ = EmitNode(pnt_direct_declarator, 3, 1, $1, 0, 0, 0);
		}
	| direct_declarator '[' constant_expression ']'
		{
			$$ = EmitNode(pnt_direct_declarator, 4, 2, $1, $3, 0, 0);
		}
	| direct_declarator '(' parameter_type_list ')'
		{
			$$ = EmitNode(pnt_direct_declarator, 5, 2, $1, $3, 0, 0);
		}
	| direct_declarator '(' ')'
		{
			$$ = EmitNode(pnt_direct_declarator, 6, 1, $1, 0, 0, 0);
		}
	| direct_declarator '(' identifier_list ')'
		{
			$$ = EmitNode(pnt_direct_declarator, 7, 2, $1, $3, 0, 0);
		}
	;

pointer:
	'*'
		{
			$$ = EmitNode(pnt_pointer, 1, 0, 0, 0, 0, 0);
		}
	| '*' type_qualifier_list
		{
			$$ = EmitNode(pnt_pointer, 2, 1, $2, 0, 0, 0);
		}
	| '*' pointer
		{
			$$ = EmitNode(pnt_pointer, 3, 1, $2, 0, 0, 0);
		}
	| '*' type_qualifier_list pointer
		{
			$$ = EmitNode(pnt_pointer, 4, 2, $2, $3, 0, 0);
		}
	;

type_qualifier_list:
	type_qualifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_type_qualifier_list, 1, 1, $1, 0, 0, 0);
		}
	| type_qualifier_list type_qualifier
		{
			$$ = EmitNode(pnt_type_qualifier_list, 2, 2, $1, $2, 0, 0);
		}
	;

parameter_type_list:
	parameter_list
		{
			$$ = Fold ? $1 : EmitNode(pnt_parameter_type_list, 1, 1, $1, 0, 0, 0);
		}
	| parameter_list ',' PC_ELLIPSIS
		{
			$$ = EmitNode(pnt_parameter_type_list, 2, 1, $1, 0, 0, 0);
		}
	;

parameter_list:
	parameter_declaration
		{
			$$ = Fold ? $1 : EmitNode(pnt_parameter_list, 1, 1, $1, 0, 0, 0);
		}
	| parameter_list ',' parameter_declaration
		{
			$$ = EmitNode(pnt_parameter_list, 2, 2, $1, $3, 0, 0);
		}
	;

parameter_declaration:
	declaration_specifiers declarator
		{
			$$ = EmitNode(pnt_parameter_declaration, 1, 2, $1, $2, 0, 0);
		}
	| declaration_specifiers
		{
			$$ = Fold ? $1 : EmitNode(pnt_parameter_declaration, 2, 1, $1, 0, 0, 0);
		}
	| declaration_specifiers abstract_declarator
		{
			$$ = EmitNode(pnt_parameter_declaration, 3, 2, $1, $2, 0, 0);
		}
	;

identifier_list:
	identifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_identifier_list, 1, 1, $1, 0, 0, 0);
		}
	| identifier_list ',' identifier
		{
			$$ = EmitNode(pnt_identifier_list, 2, 2, $1, $3, 0, 0);
		}
	;

initializer:
	assignment_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_initializer, 1, 1, $1, 0, 0, 0);
		}
	| '{' initializer_list '}'
		{
			$$ = EmitNode(pnt_initializer, 2, 1, $2, 0, 0, 0);
		}
	| '{' initializer_list ',' '}'
		{
			$$ = EmitNode(pnt_initializer, 3, 1, $2, 0, 0, 0);
		}
	;

initializer_list:
	initializer
		{
			$$ = Fold ? $1 : EmitNode(pnt_initializer_list, 1, 1, $1, 0, 0, 0);
		}
	| initializer_list ',' initializer
		{
			$$ = EmitNode(pnt_initializer_list, 2, 2, $1, $3, 0, 0);
		}
	;

type_name:
	specifier_qualifier_list
		{
			$$ = Fold ? $1 : EmitNode(pnt_type_name, 1, 1, $1, 0, 0, 0);
		}
	| specifier_qualifier_list abstract_declarator
		{
			$$ = EmitNode(pnt_type_name, 2, 2, $1, $2, 0, 0);
		}
	;

abstract_declarator:
	pointer
		{
			$$ = Fold ? $1 : EmitNode(pnt_abstract_declarator, 1, 1, $1, 0, 0, 0);
		}
	| direct_abstract_declarator
		{
			$$ = Fold ? $1 : EmitNode(pnt_abstract_declarator, 2, 1, $1, 0, 0, 0);
		}
	| pointer direct_abstract_declarator
		{
			$$ = EmitNode(pnt_abstract_declarator, 2, 2, $1, $2, 0, 0);
		}
	;

direct_abstract_declarator:
	'(' abstract_declarator ')'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 1, 1, $2, 0, 0, 0);
		}
	| '[' ']'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 2, 0, 0, 0, 0, 0);
		}
	| '[' constant_expression ']'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 3, 1, $2, 0, 0, 0);
		}
	| direct_abstract_declarator '[' ']'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 4, 1, $1, 0, 0, 0);
		}
	| direct_abstract_declarator '[' constant_expression ']'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 5, 2, $1, $3, 0, 0);
		}
	| '(' ')'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 6, 0, 0, 0, 0, 0);
		}
	| '(' parameter_type_list ')'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 7, 1, $2, 0, 0, 0);
		}
	| direct_abstract_declarator '(' ')'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 8, 1, $1, 0, 0, 0);
		}
	| direct_abstract_declarator '(' parameter_type_list ')'
		{
			$$ = EmitNode(pnt_direct_abstract_declarator, 9, 2, $1, $3, 0, 0);
		}
	;

statement:
	labeled_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 1, 1, $1, 0, 0, 0);
		}
	| expression_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 2, 1, $1, 0, 0, 0);
		}
	| compound_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 3, 1, $1, 0, 0, 0);
		}
	| selection_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 4, 1, $1, 0, 0, 0);
		}
	| iteration_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 5, 1, $1, 0, 0, 0);
		}
	| jump_statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement, 6, 1, $1, 0, 0, 0);
		}
	;

labeled_statement:
	identifier ':' statement
		{
			$$ = EmitNode(pnt_labeled_statement, 1, 2, $1, $3, 0, 0);
		}
	| PC_CASE constant_expression ':' statement
		{
			$$ = EmitNode(pnt_labeled_statement, 2, 2, $2, $4, 0, 0);
		}
	| PC_DEFAULT ':' statement
		{
			$$ = EmitNode(pnt_labeled_statement, 3, 1, $3, 0, 0, 0);
		}
	;

expression_statement:
	';'
		{
			$$ = EmitNode(pnt_expression_statement, 1, 0, 0, 0, 0, 0);
		}
	| expression ';'
		{
			$$ = EmitNode(pnt_expression_statement, 2, 1, $1, 0, 0, 0);
		}
	;

compound_statement:
	'{' '}'
		{
			$$ = EmitNode(pnt_compound_statement, 1, 0, 0, 0, 0, 0);
		}
	| '{' declaration_list '}'
		{
			$$ = EmitNode(pnt_compound_statement, 2, 1, $2, 0, 0, 0);
		}
	| '{' statement_list '}'
		{
			$$ = EmitNode(pnt_compound_statement, 3, 1, $2, 0, 0, 0);
		}
	| '{' declaration_list statement_list '}'
		{
			$$ = EmitNode(pnt_compound_statement, 4, 2, $2, $3, 0, 0);
		}
	;

statement_list:
	statement
		{
			$$ = Fold ? $1 : EmitNode(pnt_statement_list, 1, 1, $1, 0, 0, 0);
		}
	| statement_list statement
		{
			$$ = EmitNode(pnt_statement_list, 2, 2, $1, $2, 0, 0);
		}
	;

selection_statement:
	PC_IF '(' expression ')' statement
		{
			$$ = EmitNode(pnt_selection_statement, 1, 2, $3, $5, 0, 0);
		}
	| PC_IF '(' expression ')' statement PC_ELSE statement
		{
			$$ = EmitNode(pnt_selection_statement, 2, 3, $3, $5, $7, 0);
		}
	| PC_SWITCH '(' expression ')' statement
		{
			$$ = EmitNode(pnt_selection_statement, 3, 2, $3, $5, 0, 0);
		}
	;

iteration_statement:
	PC_WHILE '(' expression ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 1, 2, $3, $5, 0, 0);
		}
	| PC_DO statement PC_WHILE '(' expression ')' ';'
		{
			$$ = EmitNode(pnt_iteration_statement, 2, 2, $2, $5, 0, 0);
		}
	| PC_FOR '(' ';' ';' ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 3, 1, $6, 0, 0, 0);
		}
	| PC_FOR '(' expression ';' ';' ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 4, 2, $3, $7, 0, 0);
		}
	| PC_FOR '(' ';' expression ';' ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 5, 2, $4, $7, 0, 0);
		}
	| PC_FOR '(' ';' ';' expression ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 6, 2, $5, $7, 0, 0);
		}
	| PC_FOR '(' ';' expression ';' expression ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 7, 3, $4, $6, $8, 0);
		}
	| PC_FOR '(' expression ';' ';' expression ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 8, 3, $3, $6, $8, 0);
		}
	| PC_FOR '(' expression ';' expression ';' ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 9, 3, $3, $5, $8, 0);
		}
	| PC_FOR '(' expression ';' expression ';' expression ')' statement
		{
			$$ = EmitNode(pnt_iteration_statement, 10, 4, $3, $5, $7, $9);
		}
	;

jump_statement:
	PC_GOTO identifier ';'
		{
			$$ = EmitNode(pnt_jump_statement, 1, 1, $2, 0, 0, 0);
		}
	| PC_CONTINUE ';'
		{
			$$ = EmitNode(pnt_jump_statement, 2, 0, 0, 0, 0, 0);
		}
	| PC_BREAK ';'
		{
			$$ = EmitNode(pnt_jump_statement, 3, 0, 0, 0, 0, 0);
		}
	| PC_RETURN ';'
		{
			$$ = EmitNode(pnt_jump_statement, 4, 0, 0, 0, 0, 0);
		}
	| PC_RETURN expression ';'
		{
			$$ = EmitNode(pnt_jump_statement, 5, 1, $2, 0, 0, 0);
		}
	;

expression:
	assignment_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_expression, 1, 1, $1, 0, 0, 0);
		}
	| expression ',' assignment_expression
		{
			$$ = EmitNode(pnt_expression, 2, 2, $1, $3, 0, 0);
		}
	;


assignment_expression:
	conditional_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_assignment_expression, 1, 1, $1, 0, 0, 0);
		}
	| unary_expression assignment_operator assignment_expression
		{
			$$ = EmitNode(pnt_assignment_expression, 2, 3, $1, $2, $3, 0);
		}
	;

assignment_operator:
	'='
		{
			$$ = EmitChar(pnt_assignment_operator, '=');
		}
	| PC_MULASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_MULASSIGN);
		}
	| PC_DIVASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_DIVASSIGN);
		}
	| PC_MODASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_MODASSIGN);
		}
	| PC_ADDASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_ADDASSIGN);
		}
	| PC_SUBASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_SUBASSIGN);
		}
	| PC_LEFTASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_LEFTASSIGN);
		}
	| PC_RIGHTASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_RIGHTASSIGN);
		}
	| PC_ANDASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_ANDASSIGN);
		}
	| PC_XORASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_XORASSIGN);
		}
	| PC_ORASSIGN
		{
			$$ = EmitToken(pnt_assignment_operator, PC_ORASSIGN);
		}
	;

conditional_expression:
	logical_or_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_conditional_expression, 1, 1, $1, 0, 0, 0);
		}
	| logical_or_expression '?' expression ':' conditional_expression
		{
			$$ = EmitNode(pnt_conditional_expression, 2, 3, $1, $3, $5, 0);
		}
	;

constant_expression:
	conditional_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_constant_expression, 1, 1, $1, 0, 0, 0);
		}
	;

logical_or_expression:
	logical_and_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_logical_or_expression, 1, 1, $1, 0, 0, 0);
		}
	| logical_or_expression PC_LOGICAL_OR logical_and_expression
		{
			$$ = EmitNode(pnt_logical_or_expression, 2, 2, $1, $3, 0, 0);
		}
	;

logical_and_expression:
	inclusive_or_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_logical_and_expression, 1, 1, $1, 0, 0, 0);
		}
	| logical_and_expression PC_LOGICAL_AND inclusive_or_expression
		{
			$$ = EmitNode(pnt_logical_and_expression, 2, 2, $1, $3, 0, 0);
		}
	;

inclusive_or_expression:
	exclusive_or_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_inclusive_or_expression, 1, 1, $1, 0, 0, 0);
		}
	| inclusive_or_expression '|' exclusive_or_expression
		{
			$$ = EmitNode(pnt_inclusive_or_expression, 2, 2, $1, $3, 0, 0);
		}
	;

exclusive_or_expression:
	and_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_exclusive_or_expression, 1, 1, $1, 0, 0, 0);
		}
	| exclusive_or_expression '^' and_expression
		{
			$$ = EmitNode(pnt_exclusive_or_expression, 2, 2, $1, $3, 0, 0);
		}
	;

and_expression:
	equality_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_and_expression, 1, 1, $1, 0, 0, 0);
		}
	| and_expression '&' equality_expression
		{
			$$ = EmitNode(pnt_and_expression, 2, 2, $1, $3, 0, 0);
		}
	;

equality_expression:
	relational_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_equality_expression, 1, 1, $1, 0, 0, 0);
		}
	| equality_expression PC_EQUAL relational_expression
		{
			$$ = EmitNode(pnt_equality_expression, 2, 2, $1, $3, 0, 0);
		}
	| equality_expression PC_NOT_EQUAL relational_expression
		{
			$$ = EmitNode(pnt_equality_expression, 3, 2, $1, $3, 0, 0);
		}
	;

relational_expression:
	shift_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_relational_expression, 1, 1, $1, 0, 0, 0);
		}
	| relational_expression '<' shift_expression
		{
			$$ = EmitNode(pnt_relational_expression, 2, 2, $1, $3, 0, 0);
		}
	| relational_expression '>' shift_expression
		{
			$$ = EmitNode(pnt_relational_expression, 3, 2, $1, $3, 0, 0);
		}
	| relational_expression PC_LE shift_expression
		{
			$$ = EmitNode(pnt_relational_expression, 4, 2, $1, $3, 0, 0);
		}
	| relational_expression PC_GE shift_expression
		{
			$$ = EmitNode(pnt_relational_expression, 5, 2, $1, $3, 0, 0);
		}
	;

shift_expression:
	additive_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_shift_expression, 1, 1, $1, 0, 0, 0);
		}
	| shift_expression PC_LEFT additive_expression
		{
			$$ = EmitNode(pnt_shift_expression, 2, 2, $1, $3, 0, 0);
		}
	| shift_expression PC_RIGHT additive_expression
		{
			$$ = EmitNode(pnt_shift_expression, 3, 2, $1, $3, 0, 0);
		}
	;

additive_expression:
	multiplicative_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_additive_expression, 1, 1, $1, 0, 0, 0);
		}
	| additive_expression '+' multiplicative_expression
		{
			$$ = EmitNode(pnt_additive_expression, 2, 2, $1, $3, 0, 0);
		}
	| additive_expression '-' multiplicative_expression
		{
			$$ = EmitNode(pnt_additive_expression, 3, 2, $1, $3, 0, 0);
		}
	;

multiplicative_expression:
	cast_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_multiplicative_expression, 1, 1, $1, 0, 0, 0);
		}
	| multiplicative_expression '*' cast_expression
		{
			$$ = EmitNode(pnt_multiplicative_expression, 2, 2, $1, $3, 0, 0);
		}
	| multiplicative_expression '/' cast_expression
		{
			$$ = EmitNode(pnt_multiplicative_expression, 3, 2, $1, $3, 0, 0);
		}
	| multiplicative_expression '%' cast_expression
		{
			$$ = EmitNode(pnt_multiplicative_expression, 4, 2, $1, $3, 0, 0);
		}
	;

cast_expression:
	unary_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_cast_expression, 1, 1, $1, 0, 0, 0);
		}
	| '(' type_name ')' cast_expression
		{
			$$ = EmitNode(pnt_cast_expression, 2, 2, $2, $4, 0, 0);
		}
	;

unary_expression:
	postfix_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_unary_expression, 1, 1, $1, 0, 0, 0);
		}
	| PC_INCR unary_expression
		{
			$$ = EmitNode(pnt_unary_expression, 2, 1, $2, 0, 0, 0);
		}
	| PC_DECR unary_expression
		{
			$$ = EmitNode(pnt_unary_expression, 3, 1, $2, 0, 0, 0);
		}
	| unary_operator cast_expression
		{
			$$ = EmitNode(pnt_unary_expression, 4, 2, $1, $2, 0, 0);
		}
	| PC_SIZEOF unary_expression
		{
			$$ = EmitNode(pnt_unary_expression, 5, 1, $2, 0, 0, 0);
		}
	| PC_SIZEOF '(' type_name ')'
		{
			$$ = EmitNode(pnt_unary_expression, 6, 1, $3, 0, 0, 0);
		}
	;

unary_operator:
	'&'
		{
			$$ = EmitChar(pnt_unary_operator, '&');
		}
	| '*'
		{
			$$ = EmitChar(pnt_unary_operator, '*');
		}
	| '+'
		{
			$$ = EmitChar(pnt_unary_operator, '+');
		}
	| '-'
		{
			$$ = EmitChar(pnt_unary_operator, '-');
		}
	| '~'
		{
			$$ = EmitChar(pnt_unary_operator, '~');
		}
	| '!'
		{
			$$ = EmitChar(pnt_unary_operator, '!');
		}
	;

postfix_expression:
	primary_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_postfix_expression, 1, 1, $1, 0, 0, 0);
		}
	| postfix_expression '[' expression ']'
		{
			$$ = EmitNode(pnt_postfix_expression, 2, 2, $1, $3, 0, 0);
		}
	| postfix_expression '(' ')'
		{
			$$ = EmitNode(pnt_postfix_expression, 3, 1, $1, 0, 0, 0);
		}
	| postfix_expression '(' argument_expression_list ')'
		{
			$$ = EmitNode(pnt_postfix_expression, 4, 2, $1, $3, 0, 0);
		}
	| postfix_expression '.' identifier
		{
			$$ = EmitNode(pnt_postfix_expression, 5, 2, $1, $3, 0, 0);
		}
	| postfix_expression PC_DEREF identifier
		{
			$$ = EmitNode(pnt_postfix_expression, 6, 2, $1, $3, 0, 0);
		}
	| postfix_expression PC_INCR
		{
			$$ = EmitNode(pnt_postfix_expression, 7, 1, $1, 0, 0, 0);
		}
	| postfix_expression PC_DECR
		{
			$$ = EmitNode(pnt_postfix_expression, 8, 1, $1, 0, 0, 0);
		}
	;

primary_expression:
	identifier
		{
			$$ = Fold ? $1 : EmitNode(pnt_primary_expression, 1, 1, $1, 0, 0, 0);
		}
	| constant
		{
			$$ = Fold ? $1 : EmitNode(pnt_primary_expression, 2, 1, $1, 0, 0, 0);
		}
	| PC_STRING_CONSTANT
		{
			$$ = EmitString(pnt_primary_expression, PC_STRING_CONSTANT, yylval.buffer);
		}
	| '(' expression ')'
		{
			$$ = EmitNode(pnt_primary_expression, 4, 1, $2, 0, 0, 0);
		}
	;

argument_expression_list:
	assignment_expression
		{
			$$ = Fold ? $1 : EmitNode(pnt_argument_expression_list, 1, 1, $1, 0, 0, 0);
		}
	| argument_expression_list ',' assignment_expression
		{
			$$ = EmitNode(pnt_argument_expression_list, 2, 2, $1, $3, 0, 0);
		}
	;

constant:
	PC_INTEGER_CONSTANT
		{
			$$ = EmitString(pnt_constant, PC_INTEGER_CONSTANT, yylval.buffer);
		}
	| PC_CHARACTER_CONSTANT
		{
			$$ = EmitString(pnt_constant, PC_CHARACTER_CONSTANT, yylval.buffer);
		}
	| PC_FLOATING_CONSTANT
		{
			$$ = EmitString(pnt_constant, PC_FLOATING_CONSTANT, yylval.buffer);
		}
	| PC_ENUMERATION_CONSTANT
		{
			$$ = EmitString(pnt_constant, PC_ENUMERATION_CONSTANT, yylval.buffer);
		}
	;

identifier:
	PC_IDENTIFIER
		{
			$$ = EmitString(pnt_identifier, PC_IDENTIFIER, yylval.buffer);
		}
	;

struct_typedef_tag:
	PC_TYPEDEF_NAME
		{
			$$ = EmitString(pnt_identifier, PC_IDENTIFIER, yylval.buffer);
		}
	;

%%
static char *grammar_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/parsec/RCS/grammar.y,v 1.5 1993/11/11 23:29:49 gk5g Exp $";


static PC_ParseNode_t *EmitNode(type, subtype, numchildren, child0, child1, child2, child3)
PC_ParseNodeType_t type;
int             subtype, numchildren;
PC_ParseNode_t *child0, *child1, *child2, *child3;
{
    PC_ParseNode_t *result = (PC_ParseNode_t *) malloc(sizeof(PC_ParseNode_t));

    if (result) {
        result->isTokenNode = 0;
        result->type = type;
        result->datum.production.subtype = subtype;
        result->datum.production.numChildren = numchildren;
        if (numchildren > 0) {
            result->datum.production.children[0] = child0;
            if (numchildren > 1) {
                result->datum.production.children[1] = child1;
                if (numchildren > 2) {
                    result->datum.production.children[2] = child2;
                    if (numchildren > 3) {
                        result->datum.production.children[3] = child3;
                    }
                }
            }
        }
    }
    return (result);
}

static PC_ParseNode_t *EmitToken(type, tok)
PC_ParseNodeType_t type;
int             tok;
{
    PC_ParseNode_t *result = (PC_ParseNode_t *) malloc(sizeof(PC_ParseNode_t));

    if (result) {
        result->isTokenNode = 1;
        result->type = type;
        result->datum.token.isChar = 0;
        result->datum.token.hasText = 0;
        result->datum.token.val = tok;
    }
    return (result);
}

static PC_ParseNode_t *EmitChar(type, c)
PC_ParseNodeType_t type;
int             c;
{
    PC_ParseNode_t *result = (PC_ParseNode_t *) malloc(sizeof(PC_ParseNode_t));

    if (result) {
        result->isTokenNode = 1;
        result->type = type;
        result->datum.token.isChar = 1;
        result->datum.token.hasText = 0;
        result->datum.token.val = c;
    }
    return (result);
}

static PC_ParseNode_t *EmitString(type, tok, str)
PC_ParseNodeType_t type;
int             tok;
char           *str;
{
    PC_ParseNode_t *result = (PC_ParseNode_t *) malloc(sizeof(PC_ParseNode_t));

    if (result) {
        result->isTokenNode = 1;
        result->type = type;
        result->datum.token.isChar = 0;
        result->datum.token.hasText = 1;
        result->datum.token.val = tok;
        if (!(result->datum.token.text = malloc(1 + strlen(str)))) {
            free(result);
            return (PC_PARSENODE_NULL);
        }
        strcpy(result->datum.token.text, str);
    }
    return (result);
}

static          yyerror(str)
char           *str;
{
    PC_ParseError = str;
#ifdef MYYACCDEBUG
    fprintf(stderr, "Yyerror called with string %s, aborting...\n", str);
    abort();
#endif /* MYYACCDEBUG */
}

PC_ParseNode_t *PC_Parse(fold)
int             fold;
{
    Fold = fold;
    typedefs = enumerators = (char **) 0;
    typedefsUsed = typedefsAllocated = enumeratorsUsed = enumeratorsAllocated = 0;
    PC_ParseError = (char *) 0;
    return (yyparse() ? PC_PARSENODE_NULL : parseResult);
}

static int      strcmp_frontend(str1, str2)
char          **str1, **str2;
{
    return (strcmp(*str1, *str2));
}

static          RememberTypedef(str)
char           *str;
{
    GrowTypedefsIfNecessary();
    typedefs[typedefsUsed++] = str;
    qsort(typedefs, typedefsUsed, (sizeof(char *)), strcmp_frontend);
}

static          RememberEnumerator(str)
char           *str;
{
    GrowEnumeratorsIfNecessary();
    enumerators[enumeratorsUsed++] = str;
    qsort(enumerators, enumeratorsUsed, (sizeof(char *)), strcmp_frontend);
}

static          GrowTypedefsIfNecessary()
{
    if (typedefsUsed == typedefsAllocated) {
        if (typedefsAllocated) {
            typedefs = (char **) realloc(typedefs, (typedefsAllocated + 16) * (sizeof(char *)));
        }
        else {
            typedefs = (char **) malloc(16 * (sizeof(char *)));
        }
        typedefsAllocated += 16;
    }
}

static          GrowEnumeratorsIfNecessary()
{
    if (enumeratorsUsed == enumeratorsAllocated) {
        if (enumeratorsAllocated) {
            enumerators = (char **) realloc(enumerators,
                             (enumeratorsAllocated + 16) * (sizeof(char *)));
        }
        else {
            enumerators = (char **) malloc(16 * (sizeof(char *)));
        }
        enumeratorsAllocated += 16;
    }
}

static int      ContainsTypedef(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                    return (ContainsTypedef(PC_Child(node, 0)));
                case 2:
                    return (ContainsTypedef(PC_Child(node, 0))
                            || ContainsTypedef(PC_Child(node, 1)));
                case 4:
                case 6:
                    return (ContainsTypedef(PC_Child(node, 1)));
                default:
                    return (0);
            }
        case pnt_storage_class_specifier:
            return ((!PC_IsTextToken(PC_NodeToken(node)))
                    && (!PC_IsCharToken(PC_NodeToken(node)))
                    && (PC_TokenVal(PC_NodeToken(node)) == PC_TYPEDEF));
        default:
            return (0);
    }
}

int             PC_IsEnumerator(str)
char           *str;
{
    int             low = 0, high = enumeratorsUsed - 1, mid, cmpResult;

    while (low <= high) {
        mid = ((low + high) >> 1);
        if (!(cmpResult = strcmp(str, enumerators[mid])))
            return (1);
        if (high == (low + 1)) {
            low = high;
        }
        else {
            if (cmpResult < 0)
                high = mid - 1;
            else
                low = mid + 1;
        }
    }
    return (0);
}

int             PC_IsTypedef(str)
char           *str;
{
    int             low = 0, high = typedefsUsed - 1, mid, cmpResult;

    while (low <= high) {
        mid = ((low + high) >> 1);
        if (!(cmpResult = strcmp(str, typedefs[mid])))
            return (1);
        if (high == (low + 1)) {
            low = high;
        }
        else {
            if (cmpResult < 0)
                high = mid - 1;
            else
                low = mid + 1;
        }
    }
    return (0);
}

static char    *FindName(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_identifier:
            return (PC_TokenText(PC_NodeToken(node)));
        case pnt_init_declarator_list:/* FindName chooses the last name in
                                        * the list */
            switch (PC_SubType(node)) {
                case 1:
                    return (FindName(PC_Child(node, 0)));
                case 2:
                    return (FindName(PC_Child(node, 1)));
            }
        case pnt_init_declarator:
            return (FindName(PC_Child(node, 0)));
        case pnt_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (FindName(PC_Child(node, 0)));
                case 2:
                    return (FindName(PC_Child(node, 1)));
            }
        case pnt_direct_declarator:
            return (FindName(PC_Child(node, 0)));
    }
}

static          RememberTypedefs(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_init_declarator_list:
            switch (PC_SubType(node)) {
                case 1:
                    RememberTypedef(FindName(PC_Child(node, 0)));
                    break;
                case 2:
                    RememberTypedefs(PC_Child(node, 0));
                    RememberTypedef(FindName(PC_Child(node, 1)));
                    break;
            }
            break;
        default:
            RememberTypedef(FindName(node));
            break;
    }
}

void            PC_FillTokenBuf(str)
char           *str;
{
    strcpy(yylval.buffer, str);
}
