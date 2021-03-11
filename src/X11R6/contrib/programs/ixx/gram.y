/*
 * Copyright (c) 1992-1993 Silicon Graphics, Inc.
 * Copyright (c) 1993 Fujitsu, Ltd.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Silicon Graphics and Fujitsu may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Silicon Graphics and Fujitsu.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL SILICON GRAPHICS OR FUJITSU BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * IDL grammar
 */

%{

#include "expr.h"
#include "scanner.h"

#if defined(YYDEBUG)
#include <stdio.h>
#endif

#ifdef sun
/* workaround for Sun bug */
#include <malloc.h>
#endif

/*
 * Global data needed for communication between a yacc parser and
 * the rest of the program.
 */

Scanner* yyparse_scanner;
ExprKit* yyparse_exprkit;
Expr* yyparse_root;

/*
 * It would be better if yylex and yyerror were inline functions,
 * but some versions of yacc will generate prototypes or other
 * potentially confusing information unless they are macros.
 *
 * On the other hand, IBM AIXV3 requires that they be functions ...
 */

#if defined(AIXV3)
extern "C" {
    int yylex() { return yyparse_scanner->get_token(); }
    void yyerror(char* msg) { yyparse_scanner->error(msg); }
}
#else
#define yylex() (yyparse_scanner->get_token())
#define yyerror(msg) (yyparse_scanner->error(msg))
#endif

inline Expr* root(ExprList* list) {
    return yyparse_exprkit->root(list);
}

static ExprList* definition_list(Expr* def) {
    ExprList* s = yyparse_exprkit->exprlist();
    s->append(def);
    return s;
}

static ExprList* append(ExprList* list, Expr* element) {
    ExprList* e = list;
    if (e == nil) {
	e = yyparse_exprkit->exprlist();
    }
    e->append(element);
    return e;
}

static ExprList* concat(ExprList* list1, ExprList* list2) {
    ExprList* e = list1;
    if (e == nil) {
	e = list2;
    } else if (list2 != nil) {
	for (ListItr(ExprList) i(*list2); i.more(); i.next()) {
	    e->append(i.cur());
	}
    }
    return e;
}

static CaseList* case_append(CaseList* list, CaseElement* element) {
    CaseList* e = list;
    if (e == nil) {
	e = yyparse_exprkit->caselist();
    }
    e->append(element);
    return e;
}

inline Identifier* ident(UniqueString* s) {
    return yyparse_exprkit->ident(s);
}

inline Expr* interface(
    UniqueString* s, ExprList* supertypes, ExprList* defs
) {
    return yyparse_exprkit->interface(ident(s), supertypes, defs);
}

inline Expr* forward_interface(UniqueString* s) {
    return yyparse_exprkit->forward_interface(ident(s));
}

inline Expr* module(UniqueString* s, ExprList* defs) {
    return yyparse_exprkit->module(ident(s), defs);
}

inline Expr* srcpos(SourcePosition* p) {
    return yyparse_exprkit->position(p);
}

inline Expr* global(UniqueString* s) {
    return yyparse_exprkit->scoped(nil, s);
}

inline Expr* scoped(Expr* scope, UniqueString* s) {
    return yyparse_exprkit->scoped(scope, s);
}

inline Expr* const_dcl(Identifier* ident, Expr* type, Expr* expr) {
    return yyparse_exprkit->constant(ident, type, expr);
}

inline Expr* boolcon(Boolean value) {
    return yyparse_exprkit->boolean_literal(value);
}

inline Expr* intcon(long value) {
    return yyparse_exprkit->integer_literal(value);
}

inline Expr* charcon(long value) {
    return yyparse_exprkit->char_literal(value);
}

inline Expr* floatcon(double value) {
    return yyparse_exprkit->float_literal(value);
}

inline Expr* stringcon(String* value) {
    return yyparse_exprkit->string_literal(value);
}

inline Expr* unary(Opcode op, Expr* expr) {
    return yyparse_exprkit->unary(op, expr);
}

inline Expr* binary(Opcode op, Expr* left, Expr* right) {
    return yyparse_exprkit->binary(op, left, right);
}

inline Expr* typedef_decl(Expr* type, ExprList* declarator_list) {
    return yyparse_exprkit->typename(type, declarator_list);
}

inline Expr* unsigned_type(Expr* type) {
    return yyparse_exprkit->unsigned_type(type);
}

inline Expr* long_type() {
    return yyparse_exprkit->ident(new String("long"));
}

inline Expr* ulong_type() {
    return yyparse_exprkit->unsigned_type(long_type());
}

inline Expr* long_long_type() {
    return yyparse_exprkit->ident(new String("longlong"));
}

inline Expr* ulong_long_type() {
    return yyparse_exprkit->unsigned_type(long_long_type());
}

static ExprList* declarator_list(Expr* declarator) {
    ExprList* s = yyparse_exprkit->exprlist();
    s->append(declarator);
    return s;
}

inline Expr* declarator(Identifier* ident, ExprList* opt_subscript_list) {
    return yyparse_exprkit->declarator(ident, opt_subscript_list);
}

inline Expr* struct_type(Identifier* ident, ExprList* member_list) {
    return yyparse_exprkit->struct_decl(ident, member_list);
}

inline Expr* struct_member(Expr* type, ExprList* declarator_list) {
    return yyparse_exprkit->struct_member(type, declarator_list);
}

inline Expr* union_type(
    Identifier* ident, Expr* type, CaseList* case_list
) {
    return yyparse_exprkit->union_decl(ident, type, case_list);
}

static CaseList* case_list(CaseElement* case_stmt) {
    CaseList* s = yyparse_exprkit->caselist();
    s->append(case_stmt);
    return s;
}

inline CaseElement* case_element(
    ExprList* case_label_list, UnionMember* element
) {
    return yyparse_exprkit->case_element(case_label_list, element);
}

static ExprList* case_label_list(Expr* case_label) {
    ExprList* s = yyparse_exprkit->exprlist();
    s->append(case_label);
    return s;
}

inline Expr* case_label(Expr* value) {
    return yyparse_exprkit->case_label(value);
}

inline Expr* default_label() {
    return yyparse_exprkit->default_label();
}

inline UnionMember* union_member(Expr* type, Expr* declarator) {
    return yyparse_exprkit->union_member(type, declarator);
}

inline Expr* enum_type(Identifier* ident, ExprList* name_list) {
    return yyparse_exprkit->enum_decl(ident, name_list);
}

inline Expr* enumerator(UniqueString* s) {
    return yyparse_exprkit->enumerator(ident(s));
}

inline Expr* sequence_type(Expr* type, Expr* opt_length) {
    return yyparse_exprkit->sequence_decl(type, opt_length);
}

inline Expr* string_type(Expr* opt_length) {
    return yyparse_exprkit->string_decl(opt_length);
}

inline ExprList* attr_dcl(
    Boolean opt_readonly, Expr* type, ExprList* declarator_list
) {
    return yyparse_exprkit->attr_decl(opt_readonly, type, declarator_list);
}

inline Expr* except_dcl(Identifier* ident, ExprList* member_list) {
    return yyparse_exprkit->except_decl(ident, member_list);
}

inline Expr* op(
    ExprList* attr, Expr* type, UniqueString* s, ExprList* params,
    ExprList* raises, ExprList* context
) {
    return yyparse_exprkit->operation(
	ident(s), type, params, raises, attr, context
    );
}

static ExprList* param_list(Expr* param) {
    ExprList* s = yyparse_exprkit->exprlist();
    s->append(param);
    return s;
}

inline Expr* param(
    ParamTag attribute, Expr* type, Identifier* ident, ExprList* subscripts
) {
    return yyparse_exprkit->parameter(attribute, type, ident, subscripts);
}

static ExprList* string_list(Expr* str) {
    ExprList* s = yyparse_exprkit->exprlist();
    s->append(str);
    return s;
}

%}

%token
    /* keywords in alphabetical order */
    ATTRIBUTE CASE CONST CONTEXT DEFAULT ENUM EXCEPTION FALSE
    IN INOUT INTERFACE LONG MODULE ONEWAY OPERATOR OUT
    RAISES READONLY SEQUENCE STRING_TOKEN STRUCT SWITCH TRUE TYPEDEF
    UNION UNSIGNED

    /* include other C++ keywords to avoid generating confusing code */
    ASM AUTO BREAK CLASS CONTINUE DELETE DO ELSE EXTERN
    FOR FRIEND GOTO IF INLINE NEW PRIVATE PROTECTED PUBLIC
    REGISTER RETURN SIGNED SIZEOF STATIC TEMPLATE THIS
    VIRTUAL VOLATILE WHILE

    /* other tokens */
    INTCON CHARCON FLOATCON IDENT STRING
    SCOPE LSHIFT RSHIFT
    SRCPOS

    /* other C++ tokens */
    ELLIPSES INCR DECR ARROW LE GE EQ NE AND OR

%union {
    Boolean boolean_;
    long long_;
    double double_;
    class String* string_;
    class UniqueString* ustring_;
    class Identifier* identifier_;
    class Expr* expr_;
    class ExprList* exprlist_;
    class CaseList* caselist_;
    class CaseElement* case_;
    class UnionMember* umember_;
    class SourcePosition* position_;
    /* ParamTag */ unsigned long param_;
    /* Opcode */ unsigned long opcode_;
};

%type <boolean_> READONLY opt_readonly
%type <long_> INTCON CHARCON
%type <double_> FLOATCON
%type <string_> STRING
%type <ustring_> IDENT
%type <expr_>
    definition name export const_dcl expr type_dcl
    type simple_type template_type constr_type declarator
    struct_type member union_type switch_type case_label
    enum_type sequence_type opt_sequence_length
    string_type opt_string_length
    except_dcl op_dcl param

%type <exprlist_>
    definition_list opt_inheritance name_list export_list attr_dcl
    declarator_list opt_subscript_list member_list case_label_list enum_list
    opt_op_attr opt_params params opt_raises opt_context string_list

%type <caselist_>
    case_list

%type <case_>
    case

%type <umember_>
    element

%type <position_>
    SRCPOS

%type <param_>
    param_attribute

%type <opcode_>
    LSHIFT RSHIFT
    '(' '[' '.' '+' '-' '*' '/' '%' '!' '~'
    ',' '=' '?' ':' '|' '^' '&' '<' '>'

/* operator precedences */
%left lowPrec
%left '{'
%right ')'
%left ','
%right '='
%right '?' ':'
%left '|'
%left '^'
%left '&'
%left LSHIFT RSHIFT
%left '+' '-'
%left '*' '/' '%'
%right UNARY SIZEOF CAST '~'
%left '(' '['
%left SCOPE
%left highPrec

%%

start:
    definition_list			{ yyparse_root = root($1); }
;

definition_list:
    definition				{ $$ = definition_list($1); }
|   definition_list definition		{ $$ = append($1, $2); }
;

definition:
    type_dcl ';'			{ $$ = $1; }
|   const_dcl ';'			{ $$ = $1; }
|   except_dcl ';'			{ $$ = $1; }
|   INTERFACE IDENT opt_inheritance
	'{' export_list '}' ';'		{ $$ = interface($2, $3, $5); }
|   INTERFACE IDENT ';'			{ $$ = forward_interface($2); }
|   MODULE IDENT
	'{' definition_list '}' ';'	{ $$ = module($2, $4); }
|   SRCPOS				{ $$ = srcpos($1); }
;

opt_inheritance:
    /* empty */				{ $$ = nil; }
|   ':' name_list			{ $$ = $2; }
;

name_list:
    name				{ $$ = append(nil, $1); }
|   name_list ',' name			{ $$ = append($1, $3); }
;

name:
    IDENT				{ $$ = ident($1); }
|   SCOPE IDENT				{ $$ = global($2); }
|   name SCOPE IDENT			{ $$ = scoped($1, $3); }
;

export_list:
    /* empty */				{ $$ = nil; }
|   export_list export			{ $$ = append($1, $2); }
|   export_list attr_dcl ';'		{ $$ = concat($1, $2); }
;

export:
    type_dcl ';'			{ $$ = $1; }
|   const_dcl ';'			{ $$ = $1; }
|   except_dcl ';'			{ $$ = $1; }
|   op_dcl ';'				{ $$ = $1; }
;

const_dcl:
    CONST simple_type IDENT '=' expr	{ $$ = const_dcl(ident($3), $2, $5); }
;

expr:
    name				{ $$ = $1; }
|   FALSE				{ $$ = boolcon(false); }
|   TRUE				{ $$ = boolcon(true); }
|   INTCON				{ $$ = intcon($1); }
|   CHARCON				{ $$ = charcon($1); }
|   FLOATCON				{ $$ = floatcon($1); }
|   STRING				{ $$ = stringcon($1); }
|   '+' expr %prec UNARY		{ $$ = unary($1, $2); }
|   '-' expr %prec UNARY		{ $$ = unary($1, $2); }
|   expr '+' expr			{ $$ = binary($2, $1, $3); }
|   expr '-' expr			{ $$ = binary($2, $1, $3); }
|   expr '*' expr			{ $$ = binary($2, $1, $3); }
|   expr '/' expr			{ $$ = binary($2, $1, $3); }
|   expr '%' expr			{ $$ = binary($2, $1, $3); }
|   expr LSHIFT expr			{ $$ = binary($2, $1, $3); }
|   expr RSHIFT expr			{ $$ = binary($2, $1, $3); }
|   expr '&' expr			{ $$ = binary($2, $1, $3); }
|   expr '|' expr			{ $$ = binary($2, $1, $3); }
|   expr '^' expr			{ $$ = binary($2, $1, $3); }
|   '~' expr %prec UNARY		{ $$ = unary($1, $2); }
|   '(' expr ')'			{ $$ = $2; }
;

type_dcl:
    TYPEDEF type declarator_list	{ $$ = typedef_decl($2, $3); }
|   struct_type				{ $$ = $1; }
|   union_type				{ $$ = $1; }
|   enum_type				{ $$ = $1; }
;

type:
    simple_type				{ $$ = $1; }
|   constr_type				{ $$ = $1; }
;

simple_type:
    name				{ $$ = $1; }
|   UNSIGNED name			{ $$ = unsigned_type($2); }
|   LONG				{ $$ = long_type(); }
|   UNSIGNED LONG			{ $$ = ulong_type(); }
|   LONG LONG				{ $$ = long_long_type(); }
|   UNSIGNED LONG LONG			{ $$ = ulong_long_type(); }
|   template_type			{ $$ = $1; }
;

template_type:
    sequence_type			{ $$ = $1; }
|   string_type				{ $$ = $1; }
;

constr_type:
    struct_type				{ $$ = $1; }
|   union_type				{ $$ = $1; }
|   enum_type				{ $$ = $1; }
;

declarator_list:
    declarator				{ $$ = declarator_list($1); }
|   declarator_list ',' declarator	{ $$ = append($1, $3); }
;

declarator:
    IDENT opt_subscript_list		{ $$ = declarator(ident($1), $2); }
;

opt_subscript_list:
    /* empty */				{ $$ = nil; }
|   opt_subscript_list '[' expr ']'	{ $$ = append($1, $3); }
;

struct_type:
    STRUCT IDENT '{' member_list '}'	{ $$ = struct_type(ident($2), $4); }
;

member_list:
    /* empty */				{ $$ = nil; }
|   member_list member			{ $$ = append($1, $2); }
;

member:
    type declarator_list ';'		{ $$ = struct_member($1, $2); }
;

union_type:
    UNION IDENT
	SWITCH '(' switch_type ')'
	'{' case_list '}'		{ $$ = union_type(ident($2), $5, $8); }
;

switch_type:
    name				{ $$ = $1; }
|   enum_type				{ $$ = $1; }
|   LONG				{ $$ = long_type(); }
|   UNSIGNED LONG			{ $$ = ulong_type(); }
|   UNSIGNED name			{ $$ = unsigned_type($2); }
|   LONG LONG				{ $$ = long_long_type(); }
|   UNSIGNED LONG LONG			{ $$ = ulong_long_type(); }
;

case_list:
    case				{ $$ = case_list($1); }
|   case_list case			{ $$ = case_append($1, $2); }
;

case:
    case_label_list element ';'		{ $$ = case_element($1, $2); }
;

case_label_list:
    case_label				{ $$ = case_label_list($1); }
|   case_label_list case_label		{ $$ = append($1, $2); }
;

case_label:
    CASE expr ':'			{ $$ = case_label($2); }
|   DEFAULT ':'				{ $$ = default_label(); }
;

element:
    type declarator			{ $$ = union_member($1, $2); }
;

enum_type:
    ENUM IDENT '{' enum_list '}'	{ $$ = enum_type(ident($2), $4); }
;

enum_list:
    IDENT				{ $$ = append(nil, enumerator($1)); }
|   enum_list ',' IDENT			{ $$ = append($1, enumerator($3)); }
;

sequence_type:
    SEQUENCE '<' simple_type
	opt_sequence_length '>'		{ $$ = sequence_type($3, $4); }
;

opt_sequence_length:
    /* empty */				{ $$ = nil; }
|   ',' expr				{ $$ = $2; }
;

string_type:
    STRING_TOKEN opt_string_length	{ $$ = string_type($2); }
;

opt_string_length:
    /* empty */				{ $$ = nil; }
|   '<' expr '>'			{ $$ = $2; }
;

attr_dcl:
    opt_readonly ATTRIBUTE
	simple_type declarator_list	{ $$ = attr_dcl($1, $3, $4); }
;

opt_readonly:
    /* empty */				{ $$ = false; }
|   READONLY				{ $$ = true; }
;

except_dcl:
    EXCEPTION IDENT '{' member_list '}'	{ $$ = except_dcl(ident($2), $4); }
;

op_dcl:
    opt_op_attr simple_type
	IDENT '(' opt_params ')'
	opt_raises opt_context		{ $$ = op($1, $2, $3, $5, $7, $8); }
;

opt_op_attr:
    /* empty */				{ $$ = nil; }
|   ONEWAY				{ $$ = append(nil, intcon(ONEWAY)); }
;

opt_params:
    /* empty */				{ $$ = nil; }
|   params				{ $$ = $1; }
;

params:
    param				{ $$ = param_list($1); }
|   params ',' param			{ $$ = append($1, $3); }
;

/*
 * The extra rule for parameters is to allow missing names
 * to be caught as a semantic rather than syntax error.
 */
param:
    param_attribute simple_type
	IDENT opt_subscript_list	{ $$ = param($1, $2, ident($3), $4); }
|   param_attribute simple_type		{ $$ = param($1, $2, nil, nil); }
;

param_attribute:
    /* empty */				{ $$ = ExprKit::err_param; }
|   IN					{ $$ = ExprKit::in_param; }
|   OUT					{ $$ = ExprKit::out_param; }
|   INOUT				{ $$ = ExprKit::inout_param; }
;

opt_raises:
    /* empty */				{ $$ = nil; }
|   RAISES '(' name_list ')'		{ $$ = $3; }
;

opt_context:
    /* empty */				{ $$ = nil; }
|   CONTEXT '(' string_list ')'		{ $$ = $3; }
;

string_list:
    STRING				{ $$ = string_list(stringcon($1)); }
|   string_list ',' STRING		{ $$ = append($1, stringcon($3)); }
;
