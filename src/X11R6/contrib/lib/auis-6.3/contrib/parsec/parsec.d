\begindata{text,538529656}
\textdsversion{12}
\template{default}
\define{global
}
\define{footnote

attr:[Flags OverBar Int Set]
attr:[FontSize PreviousFontSize Point -2]}
\center{\bold{\bigger{PARSEC

}A library for parsing C programs

}by Bob Glickstein

}
\heading{Introduction

}Parsec is a link-time object library which exports a number of functions 
useful in the parsing of C program source.  The main routine of parsec, 
PC_Parse, returns a tree (whose structure is described in the Appendix) 
representing the parsed version of the input.  This tree can then be used to 
perform analysis of or transformations upon the input.


\heading{Library Routines

}These are the functions exported by libparsec.a.  To use them, you must 
include the header file parsec.h.


\bold{PC_Child} \smaller{[macro]}

\indent{\typewriter{PC_ParseNode_t *\bold{PC_Child}(node, num)

PC_ParseNode_t *node;

int             num;

}\indent{Returns the \italic{num}\superscript{th} child of node \italic{node}. 
 \italic{Num} can be any number between 0 and 
\bold{PC_NumChildren}(\italic{node}) - 1, inclusive.  The operation is only 
meaningful when performed on nodes of which both 
\bold{PC_IsProductionNode}(\italic{node}) and 
(\bold{PC_NumChildren}(\italic{node}) > 0) are true.

}}
\bold{PC_CountTokens}

\indent{\typewriter{int             \bold{PC_CountTokens}(node)

PC_ParseNode_t *node;

}\indent{Returns the number of tokens required to recreate the original source 
code associated with \italic{node}.  Most useful when used in conjunction with 
malloc to create a properly-sized buffer for \bold{PC_DumpTokens}.

}}
\bold{PC_DumpTokens}

\indent{\typewriter{int             \bold{PC_DumpTokens}(node, tokvec)

PC_ParseNode_t *node;

PC_Token_t     *tokvec;

}\indent{\italic{Tokvec} is an array of tokens.  \bold{PC_DumpTokens} fills 
this array with the sequence of tokens representing the input corresponding to 
\italic{node}.  The result of this function should always equal 
\bold{PC_CountTokens}(\italic{node}).

}}
\bold{PC_IsCharToken} \smaller{[macro]}

\indent{\typewriter{int         \bold{PC_IsCharToken}(tok)

PC_Token_t *tok;

}\indent{Returns non-zero if \italic{tok} is a token corresponding to a single 
character, otherwise returns zero.

}}
\bold{PC_IsEnumerator}

\indent{\typewriter{int   \bold{PC_IsEnumerator}(str)

char *str;

}\indent{Returns non-zero if the null-terminated string \italic{str} is the 
name of an enumeration constant defined in the input, otherwise returns zero.

}}
\bold{PC_IsProductionNode} \smaller{[macro]}

\indent{\typewriter{int             \bold{PC_IsProductionNode}(node)

PC_ParseNode_t *node;

}\indent{Returns non-zero if \italic{node} is a production (non-token) node, 
otherwise returns zero.

}}
\bold{PC_IsTextToken} \smaller{[macro]}

\indent{\typewriter{int         \bold{PC_IsTextToken}(tok)

PC_Token_t *tok;

}\indent{Returns non-zero if \italic{tok} is a token containing a string of 
text, otherwise returns zero.

}}
\bold{PC_IsTokenNode} \smaller{[macro]}

\indent{\typewriter{int             \bold{PC_IsTokenNode}(node)

PC_ParseNode_t *node;

}\indent{Returns non-zero if \italic{node} is a token (non-production) node, 
otherwise returns zero.

}}
\bold{PC_IsTypedef}

\indent{\typewriter{int   PC_IsTypedef(str)

char *str;

}\indent{Returns non-zero if the null-terminated string \italic{str} is the 
name of a typedef defined in the input, otherwise returns zero.

}}
\bold{PC_NodeToken} \smaller{[macro]}

\indent{\typewriter{PC_Token_t     *\bold{PC_NodeToken}(node)

PC_ParseNode_t *node;

}\indent{Returns the token associated with \italic{node}.  This operation is 
only meaningful when performed on nodes of which 
\bold{PC_IsTokenNode}(\italic{node}) is true.

}}
\bold{PC_NodeType} \smaller{[macro]}

\indent{\typewriter{PC_ParseNodeType_t  \bold{PC_NodeType}(node)

PC_ParseNode_t     *node;

}\indent{Returns the production type of \italic{node} (meaningful both for 
production nodes and token nodes).  The possible values for ``production 
types'' are outlined in the Appendix.

}}
\bold{PC_NumChildren} \smaller{[macro]}

\indent{\typewriter{int             \bold{PC_NumChildren}(node)

PC_ParseNode_t *node;

}\indent{Returns the number of children of \italic{node}.  This operation is 
only meaningful when performed on nodes of which 
\bold{PC_IsProductionNode}(\italic{node}) is true.

}}
\bold{PC_Parse}

\indent{\typewriter{PC_ParseNode_t *\bold{PC_Parse}(fold)

int             fold;

}\indent{Parses a C program on the standard input and returns the parse tree. 
 Will return NULL if a parsing error occurred, in which case the variable 
\bold{PC_ParseError} (type: char *) \italic{might} point to a null-terminated 
string describing the error (or it might be NULL).  If \italic{fold} is 
non-zero, then the parse tree will be abbreviated as described in the 
Appendix.  Such abbreviation makes some parse information slightly more 
obscure, but saves a great deal of memory.

}}
\bold{PC_PostWalkTree}

\indent{\typewriter{int              \bold{PC_PostWalkTree}(node, func, 
combine, leafVal)

PC_ParseNode_t  *node;

int            (*func)(), combine, leafVal;

}\indent{Performs a post-order traversal of the tree rooted at \italic{node}. 
 After a node's children are recursively visited, \italic{func} is applied to 
the node.  Returns the value of \italic{func} applied to the tree root 
\italic{node}.  \italic{Func} is a pointer to an integer function of four 
arguments.  These arguments are: PC_ParseNode_t *\italic{Node}, int 
\italic{Depth}, int \italic{ChildrenVal} and int \italic{WhichChild}. 
 \italic{Node} is the current node to which \italic{func} is being applied; 
\italic{Depth} is \italic{Node}'s depth in the tree relative to \italic{node} 
(\italic{node} is at depth 0); \italic{ChildrenVal} is the combination of the 
results of \italic{func} applied to \italic{Node}'s children; and 
\italic{WhichChild} is a non-negative integer specifying which child of 
\italic{Node}'s parent \italic{Node} is (except that \italic{WhichChild} is -1 
for the tree root \italic{node}).  The \italic{combine} argument to 
\bold{PC_PostWalkTree} specifies how the values of \italic{func} applied to 
the children of a node should be combined and passed back to the parent as the 
\italic{ChildrenVal} argument.  The possible values for \italic{combine} are:

\indent{\description{PC_COMBINE_ADD, which adds 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n}) 
together;

PC_COMBINE_LOR, which does a logical or of \italic{func}(child\subscript{0}), 
..., \italic{func}(child\subscript{n});

PC_COMBINE_BITOR, which does a bitwise or of 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n});

PC_COMBINE_LXOR, which does a logical exclusive-or of 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n});

PC_COMBINE_BITXOR, which does a bitwise exclusive-or of 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n});

PC_COMBINE_LAND, which does a logical and of 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n});

PC_COMBINE_BITAND, which does a bitwise and of 
\italic{func}(child\subscript{0}), ..., \italic{func}(child\subscript{n});

PC_COMBINE_MULTIPLY, which multiplies \italic{func}(child\subscript{0}), ..., 
\italic{func}(child\subscript{n}) together.

}}The \italic{leafVal} argument specifies what to pass as \italic{ChildrenVal} 
when \italic{func} is applied to nodes with no children.

}}
\bold{PC_PreWalkTree}

\indent{\typewriter{int              \bold{PC_PreWalkTree}(node, func, 
rootVal)

PC_ParseNode_t  *node;

int            (*func)(), rootVal;

}\indent{Performs a pre-order traversal of the tree rooted at \italic{node}. 
 Applies \italic{func} to a node and then recursively descends its children. 
 Returns the value of \italic{func} applied to the tree root \italic{node}. 
 \italic{Func} is a pointer to an integer function of five arguments.  These 
arguments are: PC_ParseNode_t *\italic{Node}, int \italic{Depth}, int 
*\italic{Descend}, int \italic{ParentVal} and int \italic{WhichChild}. 
 \italic{Node} is the current node to which \italic{func} is being applied; 
\italic{Depth} is \italic{Node}'s depth in the tree relative to \italic{node} 
(\italic{node} is at depth 0); \italic{Descend} is a pointer to an integer 
(explained below); \italic{ParentVal} is the result of \italic{func} applied 
to \italic{Node}'s parent; and \italic{WhichChild} is a non-negative integer 
specifying which child of \italic{Node}'s parent \italic{Node} is (except that 
\italic{WhichChild} is -1 for the tree root \italic{node}).  The integer 
pointed to by \italic{Descend} is initially 1, but if it is set to zero, it 
indicates to \bold{PC_PreWalkTree} that \italic{Node}'s children are not to be 
recursively descended (this is useful in tree-pruning).  The \italic{rootVal} 
argument specifies what to pass as \italic{ParentVal} when \italic{func} is 
applied to the tree root \italic{node}.

}}
\bold{PC_SetCharToken}

\indent{\typewriter{void        \bold{PC_SetCharToken}(tok, c)

PC_Token_t *tok;

char        c;

}\indent{Makes \italic{tok} a character-token containing the character 
\italic{c}.

}}
\bold{PC_SetTextToken}

\indent{\typewriter{void        \bold{PC_SetTextToken}(tok, val, str)

PC_Token_t *tok;

int         val;

char       *str;

}\indent{Makes \italic{tok} a text-token with value \italic{val} and 
containing the string \italic{str}.  Possible values for \italic{val} are 
outlined in the Appendix.

}}
\bold{PC_SetToken}

\indent{\typewriter{void        \bold{PC_SetToken}(tok, val)

PC_Token_t *tok;

int         val;

}\indent{Makes \italic{tok} a non-character, non-text token (that is to say, 
an encoded token) whose value is \italic{val}.  Possible values for 
\italic{val} are outlined in the Appendix.

}}
\bold{PC_SubType} \smaller{[macro]}

\indent{\typewriter{int             \bold{PC_SubType}(node)

PC_ParseNode_t *node;

}\indent{Returns the production subtype of \italic{node}; consult the Appendix 
for information about interpreting production types and subtypes.  This 
operation is only meaningful when performed on nodes of which 
\bold{PC_IsProductionNode}(\italic{node}) is true.

}}
\bold{PC_TokenChar} \smaller{[macro]}

\indent{\typewriter{char        \bold{PC_TokenChar}(tok)

PC_Token_t *tok;

}\indent{Returns the character associated with the token \italic{tok}.  This 
operation is only meaningful when performed on tokens of which 
\bold{PC_IsCharToken}(\italic{tok}) is true.

}}
\bold{PC_TokenChars}

\indent{\typewriter{char       *\bold{PC_TokenChars}(tok)

PC_Token_t *tok;

}\indent{Returns a null-terminated string containing a representation of the 
token \italic{tok}; this representation corresponds to the input form of the 
token.  The result is returned in a static buffer which is overwritten with 
each call.

}}
\bold{PC_TokenText} \smaller{[macro]}

\indent{\typewriter{char       *\bold{PC_TokenText}(tok)

PC_Token_t *tok;

}\indent{Returns the text string associated with \italic{tok}.  This operation 
is only meaningful when performed on tokens of which 
\bold{PC_IsTextToken}(\italic{tok}) is true.

}}
\bold{PC_TokenVal} \smaller{[macro]}

\indent{\typewriter{int         \bold{PC_TokenVal}(tok)

PC_Token_t *tok;

}\indent{Returns the token code associated with \italic{tok}.  Possible values 
are outlined in the Appendix.  This operation is only meaningful when 
performed on tokens of which neither \bold{PC_IsCharToken}(\italic{tok}) nor 
\bold{PC_IsTextToken}(\italic{tok}) is true.

}}

\begindata{bp,538268296}
\enddata{bp,538268296}
\view{bpv,538268296,87,0,0}
\heading{Example

}Following is a trivial example of a parsec application.  This program tries 
to parse its input.  If the input is a syntactically valid C module, then the 
module is reproduced on the standard output, one token at a time; otherwise, 
an error is reported.


\smaller{\indent{\typewriter{#include <stdio.h>

#include <parsec.h>


\bold{main}()

\{

    PC_ParseNode_t *Tree;

    PC_Token_t     *tokenVector;

    int             numTokens, i;

    extern char    *malloc();


    Tree = PC_Parse(1);                /}\italic{* Folding is "on" 
*}\typewriter{/

    if (!Tree) \{

        fprintf(stderr,

                "Input is not a syntactically valid C program\\n");

        exit(1);

    \}

    numTokens = PC_CountTokens(Tree);

    tokenVector = (PC_Token_t *)

          malloc(numTokens *

                 (sizeof(PC_Token_t)));

    if (!tokenVector) \{

        fprintf(stderr, "Out of memory\\n");

        exit(1);

    \}

    PC_DumpTokens(Tree, tokenVector);  /}\italic{* Fill the token vector with 
tokens *}\typewriter{/

    for (i = 0; i < numTokens; ++i) \{

        if (!(i % 6))

            putchar('\\n');             /}\italic{* Every six tokens, print a 
newline *}\typewriter{/

        printf("%s ",

               PC_TokenChars(&(tokenVector[i])));

    \}

    putchar('\\n');                     /}\italic{* Finish up with another 
newline *}\typewriter{/

    exit(0);                           /}\italic{* Normal termination 
*}\typewriter{/

\}

}}}
\heading{Bugs

}PC_Parse currently can only read the standard input.

There is no facility for identifying syntax errors in the input, except that 
one did or did not occur.

The parser is pretty slow.

Pre-processor directives are not handled.


\heading{Author

}Bob Glickstein, Information Technology Center, Carnegie Mellon University

July 1989

\begindata{bp,538268488}
\enddata{bp,538268488}
\view{bpv,538268488,88,0,0}
\center{\bold{\bigger{Appendix

}}}
The grammar recognized by the PC_Parse function 
follows\footnote{\
\begindata{fnote,538599176}
\textdsversion{12}
\define{italic
menu:[Font~1,Italic~11]
attr:[FontFace Italic Int Set]}
This grammar corresponds almost exactly to the one given in Appendix B of 
\italic{The C Programming Language, Second Edition}, by B.W. Kernighan and 
D.M. Ritchie [Prentice-Hall, NJ].\
\enddata{fnote,538599176}
\view{fnotev,538599176,89,0,0}}.  In this grammar, lower_case strings are 
production names, UPPER_CASE strings are input tokens, and single 
typographical characters (like this exclamation point\bold{!}) are in boldface 
and are also input tokens.  Italicized numbers in parentheses enumerate the 
different rules for a given production.  When such a number is followed by an 
asterisk, it indicates that the node emitted is a token node rather than a 
production node.


\indent{module \italic{(1)} ::= external_declaration

module \italic{(2)} ::= module external_declaration


external_declaration \italic{(1)} ::= function_definition

external_declaration \italic{(2)} ::= declaration


function_definition \italic{(1)} ::= declarator function_body

function_definition \italic{(2)} ::= declaration_specifiers declarator 
function_body


function_body \italic{(1)} ::= compound_statement

function_body \italic{(2)} ::= declaration_list compound_statement


declaration \italic{(1)} ::= declaration_specifiers \bold{;}

declaration \italic{(2)} ::= declaration_specifiers init_declarator_list 
\bold{;}


declaration_list \italic{(1)} ::= declaration

declaration_list \italic{(2)} ::= declaration_list declaration


declaration_specifiers \italic{(1)} ::= storage_class_specifier

declaration_specifiers \italic{(2)} ::= storage_class_specifier 
declaration_specifiers

declaration_specifiers \italic{(3)} ::= type_specifier

declaration_specifiers \italic{(4)} ::= type_specifier declaration_specifiers

declaration_specifiers \italic{(5)} ::= type_qualifier

declaration_specifiers \italic{(6)} ::= type_qualifier declaration_specifiers


storage_class_specifier \italic{(1*)} ::= PC_AUTO

storage_class_specifier \italic{(2*)} ::= PC_REGISTER

storage_class_specifier \italic{(3*)} ::= PC_STATIC

storage_class_specifier \italic{(4*)} ::= PC_EXTERN

storage_class_specifier \italic{(5*)} ::= PC_TYPEDEF


type_specifier \italic{(1*)} ::= PC_VOID

type_specifier \italic{(2*)} ::= PC_CHAR

type_specifier \italic{(3*)} ::= PC_SHORT

type_specifier \italic{(4*)} ::= PC_INT

type_specifier \italic{(5*)} ::= PC_LONG

type_specifier \italic{(6*)} ::= PC_FLOAT

type_specifier \italic{(7*)} ::= PC_DOUBLE

type_specifier \italic{(8*)} ::= PC_SIGNED

type_specifier \italic{(9*)} ::= PC_UNSIGNED

type_specifier \italic{(10)} ::= struct_or_union_specifier

type_specifier \italic{(11)} ::= enum_specifier

type_specifier \italic{(12*)} ::= PC_TYPEDEF_NAME


type_qualifier \italic{(1*)} ::= PC_CONST

type_qualifier \italic{(2*)} ::= PC_VOLATILE


struct_or_union_specifier \italic{(1)} ::= struct_or_union \bold{\{} 
struct_declaration_list \bold{\}}

struct_or_union_specifier \italic{(2)} ::= struct_or_union identifier \bold{\{} 
struct_declaration_list \bold{\}}

struct_or_union_specifier \italic{(3)} ::= struct_or_union identifier


struct_or_union \italic{(1*)} ::= PC_STRUCT

struct_or_union \italic{(2*)} ::= PC_UNION


struct_declaration_list \italic{(1)} ::= struct_declaration

struct_declaration_list \italic{(2)} ::= struct_declaration_list 
struct_declaration


init_declarator_list \italic{(1)} ::= init_declarator

init_declarator_list \italic{(2)} ::= init_declarator_list \bold{,} 
init_declarator


init_declarator \italic{(1)} ::= declarator

init_declarator \italic{(2)} ::= declarator \bold{=} initializer


struct_declaration \italic{(1)} ::= specifier_qualifier_list 
struct_declarator_list \bold{;}


specifier_qualifier_list \italic{(1)} ::= type_specifier

specifier_qualifier_list \italic{(2)} ::= type_specifier 
specifier_qualifier_list

specifier_qualifier_list \italic{(3)} ::= type_qualifier

specifier_qualifier_list \italic{(4)} ::= type_qualifier 
specifier_qualifier_list


struct_declarator_list \italic{(1)} ::= struct_declarator

struct_declarator_list \italic{(2)} ::= struct_declarator_list \bold{,} 
struct_declarator


struct_declarator \italic{(1)} ::= declarator

struct_declarator \italic{(2)} ::= \bold{:} constant_expression

struct_declarator \italic{(3)} ::= declarator \bold{:} constant_expression


enum_specifier \italic{(1)} ::= PC_ENUM \bold{\{} enumerator_list \bold{\}}

enum_specifier \italic{(2)} ::= PC_ENUM identifier \bold{\{} enumerator_list 
\bold{\}}

enum_specifier \italic{(3)} ::= PC_ENUM identifier


enumerator_list \italic{(1)} ::= enumerator

enumerator_list \italic{(2)} ::= enumerator_list \bold{,} enumerator


enumerator \italic{(1)} ::= identifier

enumerator \italic{(2)} ::= identifier \bold{=} constant_expression


declarator \italic{(1)} ::= direct_declarator

declarator \italic{(2)} ::= pointer direct_declarator


direct_declarator \italic{(1)} ::= identifier

direct_declarator \italic{(2)} ::= \bold{(} declarator \bold{)}

direct_declarator \italic{(3)} ::= direct_declarator \bold{[} \bold{]}

direct_declarator \italic{(4)} ::= direct_declarator \bold{[} 
constant_expression \bold{]}

direct_declarator \italic{(5)} ::= direct_declarator \bold{(} 
parameter_type_list \bold{)}

direct_declarator \italic{(6)} ::= direct_declarator \bold{(} \bold{)}

direct_declarator \italic{(7)} ::= direct_declarator \bold{(} identifier_list 
\bold{)}


pointer \italic{(1)} ::= \bold{*}

pointer \italic{(2)} ::= \bold{*} type_qualifier_list

pointer \italic{(3)} ::= \bold{*} pointer

pointer \italic{(4)} ::= \bold{*} type_qualifier_list pointer


type_qualifier_list \italic{(1)} ::= type_qualifier

type_qualifier_list \italic{(2)} ::= type_qualifier_list type_qualifier


parameter_type_list \italic{(1)} ::= parameter_list

parameter_type_list \italic{(2)} ::= parameter_list \bold{,} PC_ELLIPSIS


parameter_list \italic{(1)} ::= parameter_declaration

parameter_list \italic{(2)} ::= parameter_list \bold{,} parameter_declaration


parameter_declaration \italic{(1)} ::= declaration_specifiers declarator

parameter_declaration \italic{(2)} ::= declaration_specifiers

parameter_declaration \italic{(3)} ::= declaration_specifiers 
abstract_declarator


identifier_list \italic{(1)} ::= identifier

identifier_list \italic{(2)} ::= identifier_list \bold{,} identifier


initializer \italic{(1)} ::= assignment_expression

initializer \italic{(2)} ::= \bold{\{} initializer_list \bold{\}}

initializer \italic{(3)} ::= \bold{\{} initializer_list \bold{,} \bold{\}}


initializer_list \italic{(1)} ::= initializer

initializer_list \italic{(2)} ::= initializer_list \bold{,} initializer


type_name \italic{(1)} ::= specifier_qualifier_list

type_name \italic{(2)} ::= specifier_qualifier_list abstract_declarator


abstract_declarator \italic{(1)} ::= pointer

abstract_declarator \italic{(2)} ::= direct_abstract_declarator

abstract_declarator \italic{(3)} ::= pointer direct_abstract_declarator


direct_abstract_declarator \italic{(1)} ::= \bold{(} abstract_declarator 
\bold{)}

direct_abstract_declarator \italic{(2)} ::= \bold{[} \bold{]}

direct_abstract_declarator \italic{(3)} ::= \bold{[} constant_expression 
\bold{]}

direct_abstract_declarator \italic{(4)} ::= direct_abstract_declarator \bold{[} 
\bold{]}

direct_abstract_declarator \italic{(5)} ::= direct_abstract_declarator \bold{[} 
constant_expression \bold{]}

direct_abstract_declarator \italic{(6)} ::= \bold{(} \bold{)}

direct_abstract_declarator \italic{(7)} ::= \bold{(} parameter_type_list 
\bold{)}

direct_abstract_declarator \italic{(8)} ::= direct_abstract_declarator \bold{(} 
\bold{)}

direct_abstract_declarator \italic{(9)} ::= direct_abstract_declarator \bold{(} 
parameter_type_list \bold{)}


statement \italic{(1)} ::= labeled_statement

statement \italic{(2)} ::= expression_statement

statement \italic{(3)} ::= compound_statement

statement \italic{(4)} ::= selection_statement

statement \italic{(5)} ::= iteration_statement

statement \italic{(6)} ::= jump_statement


labeled_statement \italic{(1)} ::= identifier \bold{:} statement

labeled_statement \italic{(2)} ::= PC_CASE constant_expression \bold{:} 
statement

labeled_statement \italic{(3)} ::= PC_DEFAULT \bold{:} statement


expression_statement \italic{(1)} ::= \bold{;}

expression_statement \italic{(2)} ::= expression \bold{;}


compound_statement \italic{(1)} ::= \bold{\{} \bold{\}}

compound_statement \italic{(2)} ::= \bold{\{} declaration_list \bold{\}}

compound_statement \italic{(3)} ::= \bold{\{} statement_list \bold{\}}

compound_statement \italic{(4)} ::= \bold{\{} declaration_list statement_list 
\bold{\}}


statement_list \italic{(1)} ::= statement

statement_list \italic{(2)} ::= statement_list statement


selection_statement \italic{(1)} ::= PC_IF \bold{(} expression \bold{)} 
statement

selection_statement \italic{(2)} ::= PC_IF \bold{(} expression \bold{)} 
statement PC_ELSE statement

selection_statement \italic{(3)} ::= PC_SWITCH \bold{(} expression \bold{)} 
statement


iteration_statement \italic{(1)} ::= PC_WHILE \bold{(} expression \bold{)} 
statement

iteration_statement \italic{(2)} ::= PC_DO statement PC_WHILE \bold{(} 
expression \bold{)} \bold{;}

iteration_statement \italic{(3)} ::= PC_FOR \bold{(} \bold{;} \bold{;} \bold{)} 
statement

iteration_statement \italic{(4)} ::= PC_FOR \bold{(} expression \bold{;} 
\bold{;} \bold{)} statement

iteration_statement \italic{(5)} ::= PC_FOR \bold{(} \bold{;} expression 
\bold{;} \bold{)} statement

iteration_statement \italic{(6)} ::= PC_FOR \bold{(} \bold{;} \bold{;} 
expression \bold{)} statement

iteration_statement \italic{(7)} ::= PC_FOR \bold{(} \bold{;} expression 
\bold{;} expression \bold{)} statement

iteration_statement \italic{(8)} ::= PC_FOR \bold{(} expression \bold{;} 
\bold{;} expression \bold{)} statement

iteration_statement \italic{(9)} ::= PC_FOR \bold{(} expression \bold{;} 
expression \bold{;} \bold{)} statement

iteration_statement \italic{(10)} ::= PC_FOR \bold{(} expression \bold{;} 
expression \bold{;} expression \bold{)} statement


jump_statement \italic{(1)} ::= PC_GOTO identifier \bold{;}

jump_statement \italic{(2)} ::= PC_CONTINUE \bold{;}

jump_statement \italic{(3)} ::= PC_BREAK \bold{;}

jump_statement \italic{(4)} ::= PC_RETURN \bold{;}

jump_statement \italic{(5)} ::= PC_RETURN expression \bold{;}


expression \italic{(1)} ::= assignment_expression

expression \italic{(2)} ::= expression \bold{,} assignment_expression



assignment_expression \italic{(1)} ::= conditional_expression

assignment_expression \italic{(2)} ::= unary_expression assignment_operator 
assignment_expression


assignment_operator \italic{(1*)} ::= \bold{=}

assignment_operator \italic{(2*)} ::= PC_MULASSIGN

assignment_operator \italic{(3*)} ::= PC_DIVASSIGN

assignment_operator \italic{(4*)} ::= PC_MODASSIGN

assignment_operator \italic{(5*)} ::= PC_ADDASSIGN

assignment_operator \italic{(6*)} ::= PC_SUBASSIGN

assignment_operator \italic{(7*)} ::= PC_LEFTASSIGN

assignment_operator \italic{(8*)} ::= PC_RIGHTASSIGN

assignment_operator \italic{(9*)} ::= PC_ANDASSIGN

assignment_operator \italic{(10*)} ::= PC_XORASSIGN

assignment_operator \italic{(11*)} ::= PC_ORASSIGN


conditional_expression \italic{(1)} ::= logical_or_expression

conditional_expression \italic{(2)} ::= logical_or_expression \bold{?} 
expression \bold{:} conditional_expression


constant_expression \italic{(1)} ::= conditional_expression


logical_or_expression \italic{(1)} ::= logical_and_expression

logical_or_expression \italic{(2)} ::= logical_or_expression PC_LOGICAL_OR 
logical_and_expression


logical_and_expression \italic{(1)} ::= inclusive_or_expression

logical_and_expression \italic{(2)} ::= logical_and_expression PC_LOGICAL_AND 
inclusive_or_expression


inclusive_or_expression \italic{(1)} ::= exclusive_or_expression

inclusive_or_expression \italic{(2)} ::= inclusive_or_expression \bold{|} 
exclusive_or_expression


exclusive_or_expression \italic{(1)} ::= and_expression

exclusive_or_expression \italic{(2)} ::= exclusive_or_expression \bold{^} 
and_expression


and_expression \italic{(1)} ::= equality_expression

and_expression \italic{(2)} ::= and_expression \bold{&} equality_expression


equality_expression \italic{(1)} ::= relational_expression

equality_expression \italic{(2)} ::= equality_expression PC_EQUAL 
relational_expression

equality_expression \italic{(3)} ::= equality_expression PC_NOT_EQUAL 
relational_expression


relational_expression \italic{(1)} ::= shift_expression

relational_expression \italic{(2)} ::= relational_expression \bold{<} 
shift_expression

relational_expression \italic{(3)} ::= relational_expression \bold{>} 
shift_expression

relational_expression \italic{(4)} ::= relational_expression PC_LE 
shift_expression

relational_expression \italic{(5)} ::= relational_expression PC_GE 
shift_expression


shift_expression \italic{(1)} ::= additive_expression

shift_expression \italic{(2)} ::= shift_expression PC_LEFT additive_expression

shift_expression \italic{(3)} ::= shift_expression PC_RIGHT 
additive_expression


additive_expression \italic{(1)} ::= multiplicative_expression

additive_expression \italic{(2)} ::= additive_expression \bold{+} 
multiplicative_expression

additive_expression \italic{(3)} ::= additive_expression \bold{-} 
multiplicative_expression


multiplicative_expression \italic{(1)} ::= cast_expression

multiplicative_expression \italic{(2)} ::= multiplicative_expression \bold{*} 
cast_expression

multiplicative_expression \italic{(3)} ::= multiplicative_expression \bold{/} 
cast_expression

multiplicative_expression \italic{(4)} ::= multiplicative_expression \bold{%} 
cast_expression


cast_expression \italic{(1)} ::= unary_expression

cast_expression \italic{(2)} ::= \bold{(} type_name \bold{)} cast_expression


unary_expression \italic{(1)} ::= postfix_expression

unary_expression \italic{(2)} ::= PC_INCR unary_expression

unary_expression \italic{(3)} ::= PC_DECR unary_expression

unary_expression \italic{(4)} ::= unary_operator cast_expression

unary_expression \italic{(5)} ::= PC_SIZEOF unary_expression

unary_expression \italic{(6)} ::= PC_SIZEOF \bold{(} type_name \bold{)}


unary_operator \italic{(1*)} ::= \bold{&}

unary_operator \italic{(2*)} ::= \bold{*}

unary_operator \italic{(3*)} ::= \bold{+}

unary_operator \italic{(4*)} ::= \bold{-}

unary_operator \italic{(5*)} ::= \bold{~}

unary_operator \italic{(6*)} ::= \bold{!}


postfix_expression \italic{(1)} ::= primary_expression

postfix_expression \italic{(2)} ::= postfix_expression \bold{[} expression 
\bold{]}

postfix_expression \italic{(3)} ::= postfix_expression \bold{(} \bold{)}

postfix_expression \italic{(4)} ::= postfix_expression \bold{(} 
argument_expression_list \bold{)}

postfix_expression \italic{(5)} ::= postfix_expression \bold{.} identifier

postfix_expression \italic{(6)} ::= postfix_expression PC_DEREF identifier

postfix_expression \italic{(7)} ::= postfix_expression PC_INCR

postfix_expression \italic{(8)} ::= postfix_expression PC_DECR


primary_expression \italic{(1)} ::= identifier

primary_expression \italic{(2)} ::= constant

primary_expression \italic{(3*)} ::= PC_STRING_CONSTANT

primary_expression \italic{(4)} ::= \bold{(} expression \bold{)}


argument_expression_list \italic{(1)} ::= assignment_expression

argument_expression_list \italic{(2)} ::= argument_expression_list \bold{,} 
assignment_expression


constant \italic{(1*)} ::= PC_INTEGER_CONSTANT

constant \italic{(2*)} ::= PC_CHARACTER_CONSTANT

constant \italic{(3*)} ::= PC_FLOATING_CONSTANT

constant \italic{(4*)} ::= PC_ENUMERATION_CONSTANT


identifier \italic{(1*)} ::= PC_IDENTIFIER

}
A node is emitted for every production in the above list.  The production type 
of the node can be retrieved with PC_NodeType.  The value of this function is 
a constant whose name is the string "pnt_" ("Parsec Node Type") followed by 
the name of the production, such as pnt_module or pnt_storage_class_specifier. 
 When a production node is emitted, it has a subtype in addition to a 
production type.  The subtype is the parenthesized number in the above 
grammar, and can be retrieved with PC_SubType.  When the subtype in the above 
grammar is followed by an asterisk, then a token node is emitted rather than a 
production node.  A token node is a node PC_ParseNode_t object, but it 
contains a token (PC_Token_t) rather than some number of children.  Token 
nodes do not have subtypes, but the contents of the token are enough to 
determine the exact rule which was recignized for that node.  Tokens come in 
three flavors: character tokens, text tokens and code tokens.  A character 
token is one of which PC_IsCharToken is true; it contains a single character 
which corresponds exactly to a character that was recognized in the input 
(accessible via PC_TokenChar).  A text token is one of which PC_IsTextToken is 
true and contains a value (accessible via PC_TokenVal) and a string 
(accessible via PC_TokenText).  The value indicates what kind of input was 
recognized and the string contains the actual input.  The following kinds of 
tokens have text associated with them: Identifiers (PC_IDENTIFIER), Constants 
(PC_STRING_CONSTANT, PC_INTEGER_CONSTANT, PC_CHARACTER_CONSTANT, 
PC_FLOATING_CONSTANT, PC_ENUMERATION_CONSTANT), and typedef names 
(PC_TYPEDEF_NAME).  A code token is one containing one of the constant values 
given above in the grammar, such as PC_INCR (which corresponds to the input 
string "++"), or PC_TYPEDEF (which corresponds to the input string "typedef").


When a production node has children, those children only correspond to the 
sub-productions recognized in a given rule; no intervening input tokens are 
saved.  So, for example, if you have a node whose type is pnt_enum_specifier 
and it has one child, the only way to tell whether it corresponds to


\indent{\bold{enum} \bold{\{} \italic{child-tokens} \bold{\}}

}
or to


\indent{\bold{enum} \italic{child-tokens}

}
is to examine the subtype (in this case, the subtype will be 1 or 3).


There are many rules in the grammar of the form


\indent{foo ::= bar

}
that is, where a production contains exactly one subrule and no intervening 
tokens.  If the "fold" argument to PC_Parse is non-zero, then the resulting 
tree will be abbreviated to eliminate nodes corresponding to productions like 
"foo" above.  For example, consider the following (very short!) C source file:


\indent{\typewriter{int num;

}}
Without folding, the parse tree for this file looks like this:


\begindata{zip,538598408}
%ViewWidth 438
%ViewHeight 268
*D;-1000,1400
N8.5X11
>-1000,1400
*A;34,1224
Fandysans8b
Tpnt_module (1)
MCM
*A;51,916
Fandysans8b
Tpnt_external_declaration (2)
MCM
*A;-942,179
Fandysans8b
Tpnt_declaration_specifiers (3)
MCM
*A;34,599
Fandysans8b
Tpnt_declaration (2)
MCM
*A;873,188
Fandysans8b
Tpnt_init_declarator_list (1)
MCM
*A;-950,-222
Fandysans8b
Tpnt_type_specifier: PC_INT
MCM
*A;839,-214
Fandysans8b
Tpnt_init_declarator (1)
MCM
*A;805,-573
Fandysans8b
Tpnt_declarator (1)
MCM
*A;805,-925
Fandysans8b
Tpnt_direct_declarator (1)
MCM
*A;779,-1233
Fandysans8b
Tpnt_identifier: num
MCM
*C;0,1147
>8,1002
*C;8,813
>8,693
*C;-17,505
>-916,274
*C;-916,111
>-916,-128
*C;8,505
>779,265
*C;796,137
>796,-111
*C;796,-265
>796,-496
*C;796,-625
>796,-830
*C;788,-985
>788,-1147

\enddata{zip,538598408}
\view{zipview,538598408,90,0,270}

\begindata{bp,538269640}
\enddata{bp,538269640}
\view{bpv,538269640,91,0,0}
With folding, the parse tree is abbreviated to this:


\begindata{zip,538598664}
%ViewWidth 526
%ViewHeight 180
%ObjectWidth 571
%ObjectHeight 306
*D;-1000,1400
N8.5X11
>-1000,1400
*A;34,253
Fandysans8b
Tpnt_declaration (2)
MCM
*A;-884,-386
Fandysans8b
Tpnt_type_specifier: PC_INT
MCM
*A;820,-386
Fandysans8b
Tpnt_identifier: num
MCM
*C;-25,176
>-995,-304
*C;-8,176
>801,-304

\enddata{zip,538598664}
\view{zipview,538598664,92,573,182}

The uses of Parsec are numerous, however its usefulness is limited by the fact 
that it can only recognize C code proper; all pre-processor macros must be 
resolved before the parser can recognize the input.  Therefore, it is 
customary to do the following to a C file before letting parsec process it:


\indent{cc -E -I\italic{include-directories} ... -D\italic{defines} ... 
source.c | your-parsec-application

}
Parsec has been used to implement yyhide, a Yacc/Lex postprocessor which makes 
selected identifiers static.  It can also be used to write source-code 
indenters, call-graph generators, etc.


\begindata{bp,537558784}
\enddata{bp,537558784}
\view{bpv,537558784,94,0,0}
Copyright 1992 Carnegie Mellon University and IBM.  All rights reserved.

\smaller{\smaller{$Disclaimer: 

Permission to use, copy, modify, and distribute this software and its 

documentation for any purpose is hereby granted without fee, 

provided that the above copyright notice appear in all copies and that 

both that copyright notice, this permission notice, and the following 

disclaimer appear in supporting documentation, and that the names of 

IBM, Carnegie Mellon University, and other copyright holders, not be 

used in advertising or publicity pertaining to distribution of the software 

without specific, written prior permission.



IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 

DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 

ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 

SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 

BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 

DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 

WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 

ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 

OF THIS SOFTWARE.

 $

}}\enddata{text,538529656}
