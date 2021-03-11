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
 * Expr - node in parse graph
 */

#ifndef expr_h
#define expr_h

#include "list.h"
#include "types.h"

#define IdentString String

class Declarator;
class ErrorHandler;
class Generator;
class IdentString;
class Resolver;
class SourcePosition;
class String;
class StringList;
class Symbol;
class SymbolTable;

typedef unsigned long Opcode;
typedef unsigned long ParamTag;

struct ConfigInfo {
    const char* filename;
    const char* stubfile;
    const char* serverfile;
    const char* inclpath;
    const char* inclext;
    StringList* includes;
    StringList* stub_includes;
    StringList* server_includes;
    const char* superclass;
    const char* metaclass;
    const char* envclass;
    Boolean envfirst;
    const char* stubclass;
    const char* request;
    const char* buffer;
    const char* exchange;
    const char* except;
    const char* user_except;
    const char* prefix;
    const char* direct;
    StringList* transcriptions;
    Boolean refobjs;
    Boolean cdecls;
    Boolean cstubs;
    SymbolTable* symbols;
};

class Expr {
protected:
    Expr();
public:
    virtual ~Expr();

    virtual Symbol* symbol() = 0;
    virtual Expr* parent() = 0;
    virtual Declarator* declarator() = 0;
    virtual void resolve(Resolver*) = 0;
    virtual Boolean set_source(Generator*) = 0;
    virtual Boolean generate(Generator*) = 0;
    virtual Boolean generate_name(Generator*) = 0;
    virtual Boolean generate_def(Generator*) = 0;
    virtual Boolean generate_impl(Generator*) = 0;
    virtual Boolean generate_extern_types(Generator*) = 0;
    virtual Boolean generate_method(Generator*) = 0;
    virtual Boolean generate_params(Generator*) = 0;
    virtual Boolean generate_request(Generator*) = 0;
    virtual Boolean generate_stub(Generator*) = 0;
    virtual Boolean generate_extern_stubs(Generator*) = 0;
    virtual Boolean generate_types(Generator*) = 0;
    virtual Boolean generate_marshal(Generator*) = 0;
    virtual Boolean generate_unmarshal(Generator*) = 0;
    virtual Boolean generate_receive(Generator*) = 0;
    virtual Boolean varying() = 0;
    virtual SourcePosition* position() = 0;
    virtual void set_source_position(ErrorHandler*) = 0;
};

class Identifier : public Expr {
protected:
    Identifier();
public:
    virtual ~Identifier();

    virtual IdentString* string() = 0;
};

class CaseElement;
class ExprKitImpl;
class UnionMember;

declarePtrList(ExprList,Expr)
declarePtrList(CaseList,CaseElement)

class ExprKit {
public:
    ExprKit(ErrorHandler*);
    virtual ~ExprKit();

    enum { err_param, in_param, out_param, inout_param };

    virtual Expr* root(ExprList* defs);
    virtual Expr* interface(
	Identifier* ident, ExprList* supertypes, ExprList* defs
    );
    virtual Expr* forward_interface(Identifier* ident);
    virtual Expr* module(Identifier* ident, ExprList* defs);
    virtual Expr* scoped(Expr* scope, IdentString* ident);
    virtual Expr* constant(Identifier* ident, Expr* type, Expr* value);
    virtual Expr* unary(Opcode op, Expr* expr);
    virtual Expr* binary(Opcode op, Expr* left, Expr* right);
    virtual Expr* typename(Expr* type, ExprList* declarator_list);
    virtual Expr* unsigned_type(Expr* type);
    virtual Expr* declarator(Identifier* ident, ExprList* subscripts);
    virtual Expr* struct_decl(Identifier* ident, ExprList* members);
    virtual Expr* struct_member(Expr* type, ExprList* declarator_list);
    virtual Expr* union_decl(
	Identifier* ident, Expr* type, CaseList* case_list
    );
    virtual CaseElement* case_element(
	ExprList* case_label_list, UnionMember* element
    );
    virtual Expr* case_label(Expr*);
    virtual Expr* default_label();
    virtual UnionMember* union_member(Expr* type, Expr* declarator);
    virtual Expr* enum_decl(Identifier* ident, ExprList* members);
    virtual Expr* enumerator(Identifier* ident);
    virtual Expr* sequence_decl(Expr* type, Expr* length);
    virtual Expr* string_decl(Expr* length);
    virtual ExprList* attr_decl(
	Boolean readonly, Expr* type, ExprList* declarator_list
    );
    virtual Expr* except_decl(Identifier* ident, ExprList* members);
    virtual Expr* operation(
	Identifier* ident, Expr* type, ExprList* params,
	ExprList* exceptions, ExprList* attributes, ExprList* context
    );
    virtual Expr* parameter(
	ParamTag attr, Expr* type, Identifier* ident, ExprList* subscripts
    );
    virtual Identifier* ident(IdentString*);
    virtual Expr* boolean_literal(Boolean);
    virtual Expr* integer_literal(long);
    virtual Expr* float_literal(double);
    virtual Expr* string_literal(String*);
    virtual Expr* char_literal(long);
    virtual ExprList* exprlist();
    virtual CaseList* caselist();
    virtual Expr* position(SourcePosition*);

    virtual SymbolTable* symbol_table();
    virtual Resolver* resolver(const ConfigInfo&);
    virtual Generator* generator(const ConfigInfo&);
private:
    ExprKitImpl* impl_;
};

#endif
